module check

import token
import ast
import ir_gen
import ir_gen_expr
import ir_gen_builtins

-- ============================================================
-- THE CHECKER
-- Finds the type of every expression of a program and reports
-- where names and types do not fit, before any code is
-- generated: check_program gives the errors of a source file,
-- and the type of each of its expressions.
--
-- A type is its name in source form: "int", "List[string]",
-- "Map[string, int]", "fn(int) -> bool". "" is a type the
-- checker does not know, and "?" stands for any type, alone or
-- as a part ("Option[?]" is what None is). A mismatch is
-- reported only when both sides are known, so what the checker
-- cannot type is left to the checks of code generation.
--
-- The checker is a value: a function takes it, changes its
-- copy and returns it, alone or in a Typed with the type found.
--
-- The type of each expression is recorded at the expression's
-- id (see EXPRESSION IDS in ast.dm), for code generation. Where
-- a value goes says what type it has when the value alone does
-- not: None is "Option[?]", and an Option[int] in
-- `let o: Option[int] = None` (ck_settle).
-- ============================================================

-- A function's or method's signature
struct FnSig {
    -- The types of the parameters, with `self` first for a method.
    -- Type parameters are still names here; a call substitutes them.
    params: List[string],
    -- "void" when the function returns nothing
    ret: string,
    -- The type parameters of the impl block, then the function's own
    generics: List[string],
    -- The traits each type parameter must implement, joined with "+"
    -- ("" for none): the bounds of fn f[T: Show + Named]
    bounds: List[string],
    has_self: bool,
    is_async: bool,
    -- Whether each parameter is `mut`: the function changes the
    -- variable it is given, which must then be `mut` itself
    changes: List[bool],
    -- An extern fn that takes more arguments than it has parameters
    variadic: bool,
    -- The effects the function declares it has
    effects: List[string]
}

struct StructSig {
    generics: List[string],
    field_names: List[string],
    field_types: List[string]
}

struct EnumSig {
    generics: List[string],
    variant_names: List[string],
    -- The types of each variant's values, joined with "|"
    variant_payloads: List[string]
}

struct Checker {
    -- The files of the program, at their numbers (see LINES IN FILES in token.dm)
    files: List[string],
    errors: List[string],
    -- Functions by name, and methods as "Type_method"; a trait's
    -- methods are "Trait_method"
    fns: Map[string, FnSig],
    structs: Map[string, StructSig],
    enums: Map[string, EnumSig],
    -- A trait's method names, as "|a|b|"
    traits: Map[string, string],
    -- The traits a type implements, as "|A|B|"
    type_traits: Map[string, string],
    -- Constants and their types
    consts: Map[string, string],
    -- The variables in scope, innermost last. A flag is 1 when the
    -- variable may be assigned to.
    var_names: List[string],
    var_types: List[string],
    var_flags: List[int],
    -- Inside a lambda, how many variables there were where it began:
    -- those are the variables of the function around it, of which the
    -- lambda has copies (see FUNCTION VALUES in ir_gen_expr.dm). 0
    -- outside a lambda.
    lambda_floor: int,
    -- While a lambda that does not say what it returns is checked: the
    -- type of the value its first `return` gives ("void" for a return
    -- without one), "" before there was one. "-" otherwise.
    lambda_ret: string,
    -- What the function or lambda being checked returns: "void" for
    -- nothing, "" when it is not known
    ret_type: string,
    -- The id of the call that an `await` is waiting for, while that
    -- call is checked; -3 once the call is found to be of an async
    -- function, and -2 outside an `await`
    awaited: int,
    -- The function being checked and the effects it declares. One
    -- that declares none is not held to any.
    fn_name: string,
    fn_effects: List[string],
    -- Names that stand for other types where the checker is: Self,
    -- and the type parameters in scope, which stand for "?"
    norm_names: List[string],
    norm_repls: List[string],
    -- The type of each expression, at the expression's id
    types: List[string],
    -- For a call of a generic function, at the call's id: what the
    -- function's type parameters stand for, joined with "|"
    type_args: List[string],
    -- For a method call that is a call of a function, at the call's
    -- id: "call:f" for x.f(a), which is f(x, a)
    targets: List[string],
    -- False in the body of a generic function or method, where the
    -- types depend on the type arguments and none is recorded
    record: bool
}

-- What check_program finds
struct Checked {
    errors: List[string],
    -- The type of each expression at its id; "" where it is not known
    types: List[string],
    -- The type arguments of each call of a generic function at the
    -- call's id, joined with "|"; "" where they are not all known
    type_args: List[string],
    -- What a method call calls, at the call's id, where it is not a
    -- method: "call:f" for x.f(a), which is f(x, a)
    targets: List[string],
    -- The checker as it was at the end, with the program's functions
    -- and types: code generation checks each instance of a generic
    -- function with it (check_instance)
    checker: Box[Checker]
}

-- A checker and the type of the expression it has just checked
struct Typed {
    c: Checker,
    t: string
}

struct TypedList {
    c: Checker,
    types: List[string]
}

fn ck_typed(c: Checker, t: string) -> Typed {
    return Typed { c: c, t: t }
}

fn ck_error(c: Checker, msg: string, line: int) -> Checker {
    let mut out = c
    out.errors.push(error_at(out.files, msg, line))
    return out
}

fn ck_index_of(names: List[string], name: string) -> int {
    let mut i = 0
    while i < names.len() {
        if names[i] == name { return i }
        i = i + 1
    }
    return -1
}

-- ============================================================
-- TYPE NAMES
-- ============================================================

fn ty_ident_byte(b: int) -> bool {
    if b >= 97 and b <= 122 { return true }
    if b >= 65 and b <= 90 { return true }
    if b >= 48 and b <= 57 { return true }
    -- 36 is the $ of Dyn$Trait
    return b == 95 or b == 36
}

-- One name for the types that have two
fn ty_alias(name: string) -> string {
    if name == "i64" { return "int" }
    if name == "f64" { return "float" }
    if name == "String" { return "string" }
    return name
}

-- The type with each of `names` in it replaced by the entry of
-- `repls` at the same place, and every other name by its alias
fn ty_rewrite(t: string, names: List[string], repls: List[string]) -> string {
    let n = len(t)
    let mut out = ""
    let mut i = 0
    while i < n {
        let start = i
        if ty_ident_byte(byte_at(t, i)) {
            while i < n and ty_ident_byte(byte_at(t, i)) {
                i = i + 1
            }
            let word = substr(t, start, i - start)
            let at = ck_index_of(names, word)
            if at >= 0 and at < repls.len() {
                out = out + repls[at]
            } else {
                out = out + ty_alias(word)
            }
        } else {
            while i < n and ty_ident_byte(byte_at(t, i)) == false {
                i = i + 1
            }
            out = out + substr(t, start, i - start)
        }
    }
    return out
}

fn ty_known(t: string) -> bool {
    return t != "" and t != "?"
}

-- Whether every part of the type is known: "Option[int]", and not
-- "Option[?]"
fn ty_whole(t: string) -> bool {
    return t != "" and string_contains(t, "?") == false
}

-- One type from two that describe the same value, taking from each
-- the parts the other does not know: "Result[int, string]" from
-- "Result[int, ?]" and "Result[?, string]". The first where they differ.
fn ty_merge(a: string, b: string) -> string {
    if ty_known(a) == false { return b }
    if ty_known(b) == false or a == b { return a }
    if ty_whole(a) { return a }
    let head = ty_head(a)
    if head == "" or head != ty_head(b) { return a }
    let a_args = ty_args(a)
    let b_args = ty_args(b)
    if a_args.len() == 0 or a_args.len() != b_args.len() { return a }
    let mut out = head + "["
    let mut i = 0
    while i < a_args.len() {
        if i > 0 { out = out + ", " }
        out = out + ty_merge(a_args[i], b_args[i])
        i = i + 1
    }
    return out + "]"
}

fn ty_is_int(t: string) -> bool {
    if t == "int" or t == "i8" or t == "i16" or t == "i32" { return true }
    return t == "u8" or t == "u16" or t == "u32" or t == "u64"
}

fn ty_is_float(t: string) -> bool {
    return t == "float" or t == "f32"
}

-- "string", "bool" or "number" for a type that is one, else ""
fn ty_kind(t: string) -> string {
    if t == "string" or t == "bool" { return t }
    if ty_is_int(t) or ty_is_float(t) { return "number" }
    return ""
}

-- The name of a type without its arguments: "Map" for
-- "Map[string, int]". "" for function and array types.
fn ty_head(t: string) -> string {
    if starts_with(t, "fn(") or starts_with(t, "[") { return "" }
    let at = string_find(t, "[")
    if at < 0 { return t }
    return substr(t, 0, at)
}

-- The arguments of a type: "string" and "int" for "Map[string, int]"
fn ty_args(t: string) -> List[string] {
    let none: List[string] = []
    if starts_with(t, "fn(") or starts_with(t, "[") { return none }
    let at = string_find(t, "[")
    if at < 0 or ends_with(t, "]") == false { return none }
    return split_type_args(substr(t, at + 1, len(t) - at - 2))
}

fn ty_arg(t: string, i: int) -> string {
    let found = ty_args(t)
    if i < found.len() { return found[i] }
    return ""
}

-- "T" for "Box[T]", and any other type as it is
fn ty_unbox(t: string) -> string {
    if ty_head(t) == "Box" { return ty_arg(t, 0) }
    return t
}

-- The element type of an array type: "int" for "[int; 4]"
fn ty_array_elem(t: string) -> string {
    let inner = substr(t, 1, len(t) - 2)
    let semi = string_find(inner, ";")
    if semi >= 0 { return string_trim(substr(inner, 0, semi)) }
    return string_trim(inner)
}

-- What a `for` over a value of the type gives each time round
fn ty_iterated(t: string) -> string {
    let head = ty_head(t)
    if head == "List" or head == "Map" { return ty_arg(t, 0) }
    if starts_with(t, "[") { return ty_array_elem(t) }
    return ""
}

-- Where the parameters of a function type end: the place of the
-- ")" in "fn(A, B) -> R", or -1
fn ty_fn_close(t: string) -> int {
    let n = len(t)
    let mut depth = 0
    let mut i = 2
    while i < n {
        let b = byte_at(t, i)
        if b == 40 { depth = depth + 1 }
        if b == 41 {
            depth = depth - 1
            if depth == 0 { return i }
        }
        i = i + 1
    }
    return -1
}

-- The parameter types of a function type: "A" and "B" for "fn(A, B) -> R"
fn ty_fn_params(t: string) -> List[string] {
    let close = ty_fn_close(t)
    if close < 3 {
        let none: List[string] = []
        return none
    }
    return split_type_args(substr(t, 3, close - 3))
}

-- What a function type returns: "R" for "fn(A, B) -> R", "void" for
-- "fn(A)", and "" for a type that is not a function's
fn ty_fn_ret(t: string) -> string {
    if starts_with(t, "fn(") == false { return "" }
    let n = len(t)
    let close = ty_fn_close(t)
    if close < 0 { return "" }
    let rest = substr(t, close + 1, n - close - 1)
    if starts_with(rest, " -> ") { return substr(rest, 4, len(rest) - 4) }
    return "void"
}

-- Whether two types are the same type, as far as both are known:
-- "List[?]" is the same as "List[int]", and "int" is not "i32"
fn ty_same(a: string, b: string) -> bool {
    if a == b { return true }
    if ty_known(a) == false or ty_known(b) == false { return true }
    if starts_with(a, "fn(") and starts_with(b, "fn(") { return ty_fn_agrees(a, b) }
    if starts_with(a, "fn(") or starts_with(b, "fn(") { return false }
    if starts_with(a, "[") or starts_with(b, "[") { return true }
    if ty_head(a) != ty_head(b) { return false }
    let a_args = ty_args(a)
    let b_args = ty_args(b)
    if a_args.len() != b_args.len() { return false }
    let mut i = 0
    while i < a_args.len() {
        if ty_same(a_args[i], b_args[i]) == false { return false }
        i = i + 1
    }
    return true
}

-- Whether two function types take the same parameters and return the
-- same type. A function is called with the types its own type names,
-- so a parameter of another width is another type here.
fn ty_fn_agrees(a: string, b: string) -> bool {
    let a_params = ty_fn_params(a)
    let b_params = ty_fn_params(b)
    if a_params.len() != b_params.len() { return false }
    let mut i = 0
    while i < a_params.len() {
        if ty_same(a_params[i], b_params[i]) == false { return false }
        i = i + 1
    }
    return ty_same(ty_fn_ret(a), ty_fn_ret(b))
}

-- Whether a value of type `got` can go where `want` is expected.
-- Unknown types fit anything. Integers of every width fit each other
-- (code generation checks the narrowing), and so do the float types.
-- Two function types must agree (ty_fn_agrees). Array types are not
-- compared.
fn ty_fits(want: string, got: string) -> bool {
    if want == got { return true }
    if ty_known(want) == false or ty_known(got) == false { return true }
    if ty_is_int(want) and ty_is_int(got) { return true }
    if ty_is_float(want) and ty_is_float(got) { return true }
    if starts_with(want, "fn(") and starts_with(got, "fn(") { return ty_fn_agrees(want, got) }
    if starts_with(want, "fn(") or starts_with(got, "fn(") { return false }
    if starts_with(want, "[") or starts_with(got, "[") { return true }
    if ty_head(want) != ty_head(got) { return false }
    let want_args = ty_args(want)
    let got_args = ty_args(got)
    if want_args.len() == 0 or got_args.len() == 0 { return true }
    if want_args.len() != got_args.len() { return false }
    let mut i = 0
    while i < want_args.len() {
        if ty_fits(want_args[i], got_args[i]) == false { return false }
        i = i + 1
    }
    return true
}

-- The types `generics` stand for, found by laying the parameter type
-- `pattern` over the argument type `actual`: T is int when List[T]
-- meets List[int]. `binds` holds what is known so far, "" for a type
-- parameter that is not.
fn ty_bind(pattern: string, actual: string, generics: List[string], binds: List[string]) -> List[string] {
    let mut out = binds
    if ty_known(actual) == false { return out }
    let at = ck_index_of(generics, pattern)
    if at >= 0 {
        if at < out.len() and out[at] == "" {
            out[at] = actual
        }
        return out
    }
    -- A function type binds through its parameters and its result:
    -- fn(T) -> U against fn(int) -> string
    if starts_with(pattern, "fn(") and starts_with(actual, "fn(") {
        let pattern_params = ty_fn_params(pattern)
        let actual_params = ty_fn_params(actual)
        if pattern_params.len() != actual_params.len() { return out }
        let mut pi = 0
        while pi < pattern_params.len() {
            out = ty_bind(pattern_params[pi], actual_params[pi], generics, out)
            pi = pi + 1
        }
        return ty_bind(ty_fn_ret(pattern), ty_fn_ret(actual), generics, out)
    }
    let head = ty_head(pattern)
    if head == "" or head == pattern or head != ty_head(actual) { return out }
    let pattern_args = ty_args(pattern)
    let actual_args = ty_args(actual)
    if pattern_args.len() != actual_args.len() { return out }
    let mut i = 0
    while i < pattern_args.len() {
        out = ty_bind(pattern_args[i], actual_args[i], generics, out)
        i = i + 1
    }
    return out
}

-- `count` type parameters that stand for nothing yet
fn ty_unbound(count: int) -> List[string] {
    let mut out: List[string] = []
    let mut i = 0
    while i < count {
        out.push("")
        i = i + 1
    }
    return out
}

-- The type with its type parameters replaced by what they stand
-- for, and by "?" where that is not known
fn ty_apply(t: string, generics: List[string], binds: List[string]) -> string {
    if generics.len() == 0 { return t }
    let mut repls: List[string] = []
    let mut i = 0
    while i < generics.len() {
        if i < binds.len() and binds[i] != "" {
            repls.push(binds[i])
        } else {
            repls.push("?")
        }
        i = i + 1
    }
    return ty_rewrite(t, generics, repls)
}

fn ty_op_text(op: int) -> string {
    if op == BINOP_ADD() { return "+" }
    if op == BINOP_SUB() { return "-" }
    if op == BINOP_MUL() { return "*" }
    if op == BINOP_DIV() { return "/" }
    if op == BINOP_MOD() { return "%" }
    if op == BINOP_EQ() { return "==" }
    if op == BINOP_NE() { return "!=" }
    if op == BINOP_LT() { return "<" }
    if op == BINOP_LE() { return "<=" }
    if op == BINOP_GT() { return ">" }
    if op == BINOP_GE() { return ">=" }
    if op == BINOP_AND() { return "and" }
    if op == BINOP_OR() { return "or" }
    if op == BINOP_BIT_AND() { return "&" }
    if op == BINOP_BIT_OR() { return "|" }
    if op == BINOP_BIT_XOR() { return "^" }
    if op == BINOP_SHL() { return "<<" }
    return ">>"
}

-- The method that overloads the operator for a struct, or ""
fn ty_op_method(op: int) -> string {
    if op == BINOP_ADD() { return "add" }
    if op == BINOP_SUB() { return "sub" }
    if op == BINOP_MUL() { return "mul" }
    if op == BINOP_DIV() { return "div" }
    if op == BINOP_EQ() { return "eq" }
    if op == BINOP_NE() { return "ne" }
    if op == BINOP_LT() { return "lt" }
    if op == BINOP_GT() { return "gt" }
    if op == BINOP_LE() { return "le" }
    if op == BINOP_GE() { return "ge" }
    return ""
}

fn ty_is_comparison(op: int) -> bool {
    return op == BINOP_EQ() or op == BINOP_NE() or op == BINOP_LT() or op == BINOP_LE() or op == BINOP_GT() or op == BINOP_GE()
}

-- ============================================================
-- SCOPES
-- ============================================================

fn ck_bind(c: Checker, name: string, t: string, flag: int) -> Checker {
    let mut out = c
    out.var_names.push(name)
    out.var_types.push(t)
    out.var_flags.push(flag)
    return out
}

-- Forget the variables declared since there were `mark` of them
fn ck_unbind(c: Checker, mark: int) -> Checker {
    let mut out = c
    while out.var_names.len() > mark {
        out.var_names.pop()
        out.var_types.pop()
        out.var_flags.pop()
    }
    return out
}

-- The place of the innermost variable of the name, or -1
fn ck_var(c: Checker, name: string) -> int {
    let mut i = c.var_names.len() - 1
    while i >= 0 {
        if c.var_names[i] == name { return i }
        i = i - 1
    }
    return -1
}

-- Whether a value of type `got` can go where `want` is expected:
-- ty_fits, and a value of a type that implements the trait can go
-- where a `dyn Trait` is expected (it is put in the dyn's variant
-- there, see ck_settle)
fn ck_fits(c: Checker, want: string, got: string) -> bool {
    if starts_with(want, DYN_PREFIX()) and ty_known(got) and got != want {
        -- A `dyn` of no trait, or of a trait that no type implements,
        -- is reported where it is written
        if c.enums.contains(want) == false { return true }
        if ty_head(got) != got { return false }
        return ck_implements(c, got, substr(want, len(DYN_PREFIX()), len(want) - len(DYN_PREFIX())))
    }
    return ty_fits(want, got)
}

-- Whether the value of the expression `e`, of type `got`, can go
-- where `want` is expected: ck_fits, and a value that is made there
-- can be made of the type of its place. Some(circle) is an
-- Option[Circle], which is no Option[dyn Shape]; but written where an
-- Option[dyn Shape] is expected it makes one, with the circle put in
-- the dyn (ck_settle goes the same way down the expression).
fn ck_fits_value(c: Checker, want: string, got: string, e0: Expr) -> bool {
    if ck_fits(c, want, got) { return true }
    if string_contains(want, DYN_PREFIX()) == false { return false }
    let mut e = e0
    while e.kind == EXPR_GROUPED() {
        e = *e.operand
    }
    let head = ty_head(want)
    if ty_head(got) != head { return false }
    let k = e.kind
    if k == EXPR_FUNCTION_CALL() and e.args.len() == 1 {
        let callee = *e.callee
        if callee.kind != EXPR_IDENTIFIER() { return false }
        if (callee.name == "Some" and head == "Option") or (callee.name == "Box_new" and head == "Box") {
            return ck_fits_value(c, ty_arg(want, 0), ty_arg(got, 0), e.args[0])
        }
        if callee.name == "Ok" and head == "Result" {
            return ck_fits(c, ty_arg(want, 1), ty_arg(got, 1)) and ck_fits_value(c, ty_arg(want, 0), ty_arg(got, 0), e.args[0])
        }
        if callee.name == "Err" and head == "Result" {
            return ck_fits(c, ty_arg(want, 0), ty_arg(got, 0)) and ck_fits_value(c, ty_arg(want, 1), ty_arg(got, 1), e.args[0])
        }
        return false
    }
    if k == EXPR_ARRAY_LITERAL() and head == "List" {
        let mut i = 0
        while i < e.elements.len() {
            if ck_fits_value(c, ty_arg(want, 0), ty_arg(got, 0), e.elements[i]) == false { return false }
            i = i + 1
        }
        return true
    }
    if k == EXPR_IF() and e.has_else {
        return ck_fits_value(c, want, got, *e.then_branch) and ck_fits_value(c, want, got, *e.else_branch)
    }
    if k == EXPR_BLOCK() {
        if e.has_result { return ck_fits_value(c, want, got, *e.result) }
        return ck_fits_last(c, want, got, e.stmts)
    }
    if k == EXPR_MATCH() {
        let mut i = 0
        while i < e.match_arms.len() {
            let arm = e.match_arms[i]
            if arm.is_expr_body {
                if ck_fits_value(c, want, got, *arm.body_expr) == false { return false }
            } else if ck_fits_last(c, want, got, arm.body) == false {
                return false
            }
            i = i + 1
        }
        return e.match_arms.len() > 0
    }
    return false
}

-- ck_fits_value for the value of a body: that of its last statement
fn ck_fits_last(c: Checker, want: string, got: string, stmts: List[Stmt]) -> bool {
    if stmts.len() == 0 { return false }
    let last = stmts[stmts.len() - 1]
    if stmt_gives_value(last) == false { return false }
    return ck_fits_value(c, want, got, stmt_value_expr(last))
}

-- A type as written in the source, in the checker's form
fn ck_norm(c: Checker, t: string) -> string {
    if t == "" { return "" }
    return ty_rewrite(t, c.norm_names, c.norm_repls)
}

fn ck_builtin_type_name(name: string) -> bool {
    if name == "int" or name == "float" or name == "bool" or name == "string" or name == "void" { return true }
    if name == "i8" or name == "i16" or name == "i32" or name == "i64" { return true }
    if name == "u8" or name == "u16" or name == "u32" or name == "u64" { return true }
    if name == "f32" or name == "f64" or name == "String" { return true }
    if name == "List" or name == "Map" or name == "Box" or name == "Option" or name == "Result" { return true }
    if name == "f32x4" or name == "f32x8" or name == "f64x2" or name == "f64x4" { return true }
    if name == "i32x4" or name == "i32x8" or name == "i64x2" or name == "i64x4" { return true }
    return name == "Self" or name == "fn" or name == "Future"
}

-- Report the first name in the type that is no type
fn ck_type_exists(c: Checker, t: string, line: int) -> Checker {
    let n = len(t)
    let mut i = 0
    let mut in_size = false
    while i < n {
        let b = byte_at(t, i)
        if b == 59 { in_size = true }
        if b == 93 { in_size = false }
        if ty_ident_byte(b) == false {
            i = i + 1
            continue
        }
        let start = i
        while i < n and ty_ident_byte(byte_at(t, i)) {
            i = i + 1
        }
        -- An array's size is a number or a constant
        if in_size or (b >= 48 and b <= 57) { continue }
        let word = substr(t, start, i - start)
        if ck_builtin_type_name(word) { continue }
        if c.structs.contains(word) or c.enums.contains(word) { continue }
        if ck_index_of(c.norm_names, word) >= 0 { continue }
        -- `dyn Trait` is the enum Dyn$Trait, which there is when a type
        -- implements the trait (see TRAITS in main_split.dm)
        if starts_with(word, DYN_PREFIX()) {
            let trait_name = substr(word, len(DYN_PREFIX()), len(word) - len(DYN_PREFIX()))
            if c.traits.contains(trait_name) {
                return ck_error(c, "no type implements the trait '" + trait_name + "', so there is no value a '" + word + "' could hold", line)
            }
            return ck_error(c, "undefined trait '" + trait_name + "'", line)
        }
        if c.traits.contains(word) {
            return ck_error(c, "'" + word + "' is a trait, not a type: a value of any type that implements it is a '" + DYN_PREFIX() + word + "'", line)
        }
        return ck_error(c, "undefined type '" + word + "'", line)
    }
    return ck_type_arity(c, t, t, line)
}

-- Report a generic struct or enum in the type `t`, a part of the type
-- `whole`, that has another number of type arguments than it takes
fn ck_type_arity(c0: Checker, t: string, whole: string, line: int) -> Checker {
    let mut c = c0
    let args = ty_args(t)
    if args.len() == 0 { return c }
    let head = ty_head(t)
    let mut takes = -1
    if c.structs.contains(head) {
        takes = c.structs.get(head).generics.len()
    } else if c.enums.contains(head) {
        takes = c.enums.get(head).generics.len()
    }
    if takes >= 0 and takes != args.len() {
        return ck_error(c, "'" + head + "' takes " + int_to_string(takes) + " type arguments, got " + int_to_string(args.len()) + " in '" + whole + "'", line)
    }
    let mut i = 0
    while i < args.len() {
        c = ck_type_arity(c, args[i], whole, line)
        i = i + 1
    }
    return c
}

-- ============================================================
-- DECLARATIONS
-- ============================================================

fn ck_sig(fd: FunctionDecl, self_type: string, outer_generics: List[string]) -> FnSig {
    let mut names: List[string] = []
    names.push("Self")
    let mut repls: List[string] = []
    repls.push(self_type)
    let mut generics: List[string] = []
    let mut bounds: List[string] = []
    let mut gi = 0
    while gi < outer_generics.len() {
        generics.push(outer_generics[gi])
        bounds.push("")
        gi = gi + 1
    }
    gi = 0
    while gi < fd.generic_params.len() {
        generics.push(fd.generic_params[gi])
        if gi < fd.generic_bounds.len() {
            bounds.push(fd.generic_bounds[gi])
        } else {
            bounds.push("")
        }
        gi = gi + 1
    }
    let mut params: List[string] = []
    let mut changes: List[bool] = []
    let mut i = 0
    while i < fd.params.len() {
        params.push(ty_rewrite(fd.params[i].type_name, names, repls))
        changes.push(fd.params[i].is_mut)
        i = i + 1
    }
    let mut has_self = false
    if fd.params.len() > 0 {
        has_self = fd.params[0].name == "self"
    }
    return FnSig {
        params: params,
        ret: ty_rewrite(fd.return_type, names, repls),
        generics: generics,
        bounds: bounds,
        has_self: has_self,
        is_async: fd.is_async,
        changes: changes,
        variadic: fd.is_variadic,
        effects: fd.effects
    }
}

fn ck_struct_sig(sd: StructDecl) -> StructSig {
    let none: List[string] = []
    let mut names: List[string] = []
    let mut types: List[string] = []
    let mut i = 0
    while i < sd.fields.len() {
        names.push(sd.fields[i].name)
        types.push(ty_rewrite(sd.fields[i].type_name, none, none))
        i = i + 1
    }
    return StructSig { generics: sd.generic_params, field_names: names, field_types: types }
}

fn ck_enum_sig(ed: EnumDecl) -> EnumSig {
    let none: List[string] = []
    let mut names: List[string] = []
    let mut payloads: List[string] = []
    let mut i = 0
    while i < ed.variants.len() {
        let v = ed.variants[i]
        names.push(v.name)
        let mut joined = ""
        let mut j = 0
        while j < v.payload_types.len() {
            if j > 0 { joined = joined + "|" }
            joined = joined + ty_rewrite(v.payload_types[j], none, none)
            j = j + 1
        }
        payloads.push(joined)
        i = i + 1
    }
    return EnumSig { generics: ed.generic_params, variant_names: names, variant_payloads: payloads }
}

-- "Name[A, B]" for a type with type parameters, else "Name"
fn ck_generic_type(name: string, generics: List[string]) -> string {
    if generics.len() == 0 { return name }
    let mut t = name + "["
    let mut i = 0
    while i < generics.len() {
        if i > 0 { t = t + ", " }
        t = t + generics[i]
        i = i + 1
    }
    return t + "]"
}

fn ck_collect_impl(c0: Checker, ib: ImplBlock) -> Checker {
    let mut c = c0
    let self_type = ck_generic_type(ib.target_type, ib.generic_params)
    let mut i = 0
    while i < ib.methods.len() {
        let fd = ib.methods[i]
        c.fns.insert(ib.target_type + "_" + fd.name, ck_sig(fd, self_type, ib.generic_params))
        i = i + 1
    }
    if ib.trait_name != "" {
        let mut known = "|"
        if c.type_traits.contains(ib.target_type) {
            known = c.type_traits.get(ib.target_type)
        }
        c.type_traits.insert(ib.target_type, known + ib.trait_name + "|")
    }
    return c
}

fn ck_collect_trait(c0: Checker, td: TraitDecl) -> Checker {
    let mut c = c0
    let mut names = "|"
    let mut i = 0
    while i < td.methods.len() {
        let fd = td.methods[i]
        names = names + fd.name + "|"
        c.fns.insert(td.name + "_" + fd.name, ck_sig(fd, "?", td.generic_params))
        i = i + 1
    }
    c.traits.insert(td.name, names)
    return c
}

-- Every declaration's signature, before any body is checked
fn ck_collect(c0: Checker, sf: SourceFile) -> Checker {
    let mut c = c0
    let no_generics: List[string] = []
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_STRUCT() {
            let sd = *decl.struct_decl
            c.structs.insert(sd.name, ck_struct_sig(sd))
        } else if decl.kind == DECL_ENUM() {
            let ed = *decl.enum_decl
            c.enums.insert(ed.name, ck_enum_sig(ed))
        } else if decl.kind == DECL_TRAIT() {
            c = ck_collect_trait(c, *decl.trait_decl)
        } else if decl.kind == DECL_IMPL() {
            c = ck_collect_impl(c, *decl.impl_block)
        } else if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            c.fns.insert(fd.name, ck_sig(fd, "", no_generics))
        } else if decl.kind == DECL_CONST() {
            let cd = *decl.const_decl
            let none: List[string] = []
            c.consts.insert(cd.name, ty_rewrite(cd.type_name, none, none))
        }
        i = i + 1
    }
    return c
}

-- ============================================================
-- EXPRESSIONS
-- ============================================================

-- Record the type found for the expression with the id
fn ck_note(c: Checker, id: int, t: string) -> Checker {
    if c.record == false or id < 0 or id >= c.types.len() { return c }
    let mut out = c
    out.types[id] = t
    return out
}

-- Whether the expression makes a new value whose type its place
-- decides: None, Some(x), Ok(x), Err(x), a new Box or Map, a list
-- literal, a struct literal with no type arguments (`Pair { .. }`)
fn ck_makes_value(e: Expr) -> bool {
    if e.kind == EXPR_ARRAY_LITERAL() { return true }
    if e.kind == EXPR_STRUCT_LITERAL() { return string_find(e.type_name, "[") < 0 }
    if e.kind == EXPR_IDENTIFIER() { return e.name == "None" }
    if e.kind == EXPR_FUNCTION_CALL() {
        let callee = *e.callee
        if callee.kind != EXPR_IDENTIFIER() { return false }
        let name = callee.name
        return name == "Some" or name == "Ok" or name == "Err" or name == "Box_new" or name == "Box_null" or name == "Map_new"
    }
    return false
}

-- A value of the expression goes where a `want` is expected. Where
-- the checker found a type with unknown parts for it, such as
-- "Option[?]" for None, the place says what they are: record `want`
-- for the expression, and for the expressions its value comes from.
fn ck_settle(c0: Checker, e: Expr, want: string) -> Checker {
    if c0.record == false or ty_whole(want) == false or want == "void" { return c0 }
    if e.id < 0 or e.id >= c0.types.len() { return c0 }
    let mut c = c0
    let variant_of = ck_generic_variant(c, e)
    if variant_of != "" and ty_head(want) == variant_of {
        return ck_settle_variant(c, e, variant_of, want)
    }
    let have = c.types[e.id]
    -- A value of a type that implements the trait, where a `dyn Trait`
    -- is expected: code generation puts it in the dyn's variant there
    if starts_with(want, DYN_PREFIX()) {
        if ty_known(have) and have != want and starts_with(have, DYN_PREFIX()) == false and ck_fits(c, want, have) {
            if string_contains(c.targets[e.id], "dyn:") == false {
                let mut marks = c.targets[e.id]
                if marks != "" { marks = marks + ";" }
                c.targets[e.id] = marks + "dyn:" + want + "|" + have
            }
        }
        return c
    }
    -- A type the checker does not know at all stays unknown, and so
    -- does a whole type that differs, such as an i32 that goes where an
    -- int is expected: the value is converted there. What makes a new
    -- value (ck_makes_value) makes it of the type of its place.
    if ty_known(have) and have != want and ty_fits(want, have) and (ty_whole(have) == false or ck_makes_value(e)) {
        c = ck_note(c, e.id, want)
    } else if ty_known(have) and have != want and ty_fits(want, have) == false and ck_fits_value(c, want, have, e) {
        -- A value made here with a `dyn Trait` in it (ck_fits_value)
        c = ck_note(c, e.id, want)
    } else if have != want and ty_is_int(have) and ty_is_int(want) and branches_give_int_literals(e) {
        -- An `if` or a `match` of integer constants is an integer of
        -- the type of its place, as a literal is: 0 or 1 for an i32
        c = ck_note(c, e.id, want)
    }
    let k = e.kind
    if k == EXPR_GROUPED() { return ck_settle(c, *e.operand, want) }
    if k == EXPR_IF() {
        c = ck_settle(c, *e.then_branch, want)
        if e.has_else { c = ck_settle(c, *e.else_branch, want) }
        return c
    }
    if k == EXPR_BLOCK() {
        if e.has_result { return ck_settle(c, *e.result, want) }
        return ck_settle_last(c, e.stmts, want)
    }
    if k == EXPR_MATCH() {
        let mut i = 0
        while i < e.match_arms.len() {
            let arm = e.match_arms[i]
            if arm.is_expr_body {
                c = ck_settle(c, *arm.body_expr, want)
            } else {
                c = ck_settle_last(c, arm.body, want)
            }
            i = i + 1
        }
        return c
    }
    if k == EXPR_ARRAY_LITERAL() {
        if ty_head(want) == "List" {
            let elem = ty_arg(want, 0)
            let mut i = 0
            while i < e.elements.len() {
                c = ck_settle(c, e.elements[i], elem)
                i = i + 1
            }
        }
        return c
    }
    -- The fields of `Pair { first: None, second: [] }`, whose types
    -- the type arguments of the place give
    if k == EXPR_STRUCT_LITERAL() {
        let head = ty_head(want)
        if e.type_name != head or c.structs.contains(head) == false { return c }
        if c.structs.get(head).generics.len() == 0 { return c }
        let mut i = 0
        while i < e.field_names.len() and i < e.field_values.len() {
            c = ck_settle(c, e.field_values[i], ck_field_type(c, want, e.field_names[i]))
            i = i + 1
        }
        return c
    }
    if k == EXPR_METHOD_CALL() {
        let static_key = ck_generic_static(c, e)
        if static_key != "" { return ck_settle_static(c, e, static_key, want) }
        return c
    }
    -- The value inside a Some, an Ok, an Err or a Box
    if k == EXPR_FUNCTION_CALL() and e.args.len() == 1 {
        let callee = *e.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let head = ty_head(want)
            if (callee.name == "Some" and head == "Option") or (callee.name == "Ok" and head == "Result") or (callee.name == "Box_new" and head == "Box") {
                return ck_settle(c, e.args[0], ty_arg(want, 0))
            }
            if callee.name == "Err" and head == "Result" {
                return ck_settle(c, e.args[0], ty_arg(want, 1))
            }
        }
    }
    return c
}

-- ck_settle for the expression that gives a block its value
fn ck_settle_last(c: Checker, stmts: List[Stmt], want: string) -> Checker {
    if stmts.len() == 0 { return c }
    let last = stmts[stmts.len() - 1]
    if stmt_gives_value(last) == false { return c }
    return ck_settle(c, stmt_value_expr(last), want)
}

-- The type of the expression, which is recorded at its id
fn ck_expr(c: Checker, e: Expr) -> Typed {
    let found = ck_expr_of_kind(c, e)
    return ck_typed(ck_note(found.c, e.id, found.t), found.t)
}

fn ck_expr_of_kind(c: Checker, e: Expr) -> Typed {
    let k = e.kind
    if k == EXPR_LITERAL_INT() { return ck_typed(c, "int") }
    if k == EXPR_LITERAL_FLOAT() { return ck_typed(c, "float") }
    if k == EXPR_LITERAL_STRING() { return ck_typed(c, "string") }
    if k == EXPR_LITERAL_BOOL() { return ck_typed(c, "bool") }
    if k == EXPR_IDENTIFIER() { return ck_identifier(c, e) }
    if k == EXPR_BINARY() { return ck_binary(c, e) }
    if k == EXPR_UNARY() { return ck_unary(c, e) }
    if k == EXPR_FIELD_ACCESS() { return ck_field(c, e) }
    if k == EXPR_INDEX_ACCESS() { return ck_index(c, e) }
    if k == EXPR_METHOD_CALL() { return ck_method_call(c, e) }
    if k == EXPR_FUNCTION_CALL() { return ck_call(c, e) }
    if k == EXPR_STRUCT_LITERAL() { return ck_struct_literal(c, e) }
    if k == EXPR_ENUM_LITERAL() { return ck_enum_literal(c, e) }
    if k == EXPR_ARRAY_LITERAL() { return ck_array_literal(c, e) }
    if k == EXPR_IF() { return ck_if_expr(c, e) }
    if k == EXPR_MATCH() { return ck_match(c, e, true) }
    if k == EXPR_BLOCK() { return ck_block(c, e) }
    if k == EXPR_LAMBDA() { return ck_lambda(c, e) }
    if k == EXPR_ERROR_PROPAGATE() { return ck_propagate(c, e) }
    if k == EXPR_GROUPED() or k == EXPR_COMPTIME() { return ck_expr(c, *e.operand) }
    if k == EXPR_CAST() { return ck_cast(c, e) }
    if k == EXPR_STRING_INTERP() {
        let parts = ck_exprs(c, e.interp_parts)
        let mut pc = parts.c
        let mut pi = 0
        while pi < parts.types.len() {
            let pt = parts.types[pi]
            if pt == "void" or (ty_known(pt) and ty_kind(pt) == "") {
                pc = ck_error(pc, "a value in an f-string must be a string, a number or a bool, not " + pt, e.interp_parts[pi].line)
            }
            pi = pi + 1
        }
        return ck_typed(pc, "string")
    }
    if k == EXPR_RANGE() {
        let low = ck_expr(c, *e.left)
        let high = ck_expr(low.c, *e.right)
        return ck_typed(high.c, "")
    }
    if k == EXPR_AWAIT() { return ck_await(c, e) }
    return ck_typed(c, "")
}

-- await f(args): the value of a call of an async function. The call
-- runs the function to its end: an async function is called under
-- `await` and nowhere else, so there is no call still running to wait
-- for, and no value that stands for one.
fn ck_await(c0: Checker, e: Expr) -> Typed {
    let mut c = c0
    let mut call = *e.operand
    while call.kind == EXPR_GROUPED() {
        call = *call.operand
    }
    let outer = c.awaited
    c.awaited = call.id
    let found = ck_expr(c, *e.operand)
    c = found.c
    let was_async = c.awaited == -3
    c.awaited = outer
    if was_async == false {
        c = ck_error(c, "'await' takes a call of an async function", e.line)
    }
    return ck_typed(c, found.t)
}

-- A call of the function with the signature, by the expression with
-- the id: an async function is called under `await`
fn ck_async_call(c: Checker, sig: FnSig, shown: string, id: int, line: int) -> Checker {
    if sig.is_async == false { return c }
    let mut out = c
    if id >= 0 and id == c.awaited {
        out.awaited = -3
        return out
    }
    return ck_error(out, "the call of the async function '" + shown + "' needs 'await'", line)
}

-- The types of the expressions, in order
fn ck_exprs(c0: Checker, exprs: List[Expr]) -> TypedList {
    let mut c = c0
    let mut types: List[string] = []
    let mut i = 0
    while i < exprs.len() {
        let r = ck_expr(c, exprs[i])
        c = ck_needs_value(r.c, exprs[i], false)
        types.push(r.t)
        i = i + 1
    }
    return TypedList { c: c, types: types }
}

-- Whether the name is a type's and no variable's
fn ck_names_type(c: Checker, name: string) -> bool {
    if ck_var(c, name) >= 0 or c.consts.contains(name) { return false }
    return c.structs.contains(name) or c.enums.contains(name) or c.traits.contains(name)
}

fn ck_identifier(c: Checker, e: Expr) -> Typed {
    let name = e.name
    if name == "true" or name == "false" { return ck_typed(c, "bool") }
    if name == "None" { return ck_typed(c, "Option[?]") }
    let at = ck_var(c, name)
    if at >= 0 { return ck_typed(c, c.var_types[at]) }
    if c.consts.contains(name) { return ck_typed(c, c.consts.get(name)) }
    -- A function's name is a value: its address. Naming a function
    -- that has effects is using them.
    if c.fns.contains(name) {
        let sig = c.fns.get(name)
        let named = ck_effects(c, name, sig.effects, " not in caller's effects", "uses", e.line)
        if is_builtin_fn(name) or sig.variadic { return ck_typed(named, "") }
        -- The value is called with what its type says, which has
        -- neither type parameters nor `mut` parameters
        if sig.generics.len() > 0 {
            return ck_typed(ck_error(named, "the generic function '" + name + "' cannot be used as a value: call it in a lambda", e.line), "")
        }
        let mut pi = 0
        while pi < sig.changes.len() {
            if sig.changes[pi] {
                return ck_typed(ck_error(named, "the function '" + name + "' has a 'mut' parameter, so it cannot be used as a value: call it in a lambda", e.line), "")
            }
            pi = pi + 1
        }
        return ck_typed(named, ty_fn_text(sig))
    }
    if is_builtin_fn(name) { return ck_typed(c, "") }
    if ck_names_type(c, name) { return ck_typed(c, "") }
    return ck_typed(ck_error(c, "undefined variable '" + name + "'", e.line), "")
}

fn ck_cast(c0: Checker, e: Expr) -> Typed {
    let operand = ck_expr(c0, *e.operand)
    let c = ck_type_exists(operand.c, e.cast_type, e.line)
    -- `as &T`: the checker has no reference types. IR generation
    -- decides whether the cast is one that is allowed.
    if e.cast_is_ref { return ck_typed(c, "") }
    let to = ck_norm(c, e.cast_type)
    if starts_with(to, DYN_PREFIX()) and ty_known(operand.t) {
        -- `x as dyn Trait` puts x in the dyn's variant
        if ck_fits(c, to, operand.t) == false {
            return ck_typed(ck_error(c, "cannot cast " + operand.t + " to " + to + ": it does not implement the trait", e.line), to)
        }
        return ck_typed(ck_settle(c, *e.operand, to), to)
    }
    return ck_typed(c, to)
}

-- The result of `left op right` for operands of the types, with an
-- error where the operator does not take them
fn ck_operator(c0: Checker, op: int, left: string, right: string, line: int) -> Typed {
    let mut c = c0
    let text = ty_op_text(op)
    if op == BINOP_AND() or op == BINOP_OR() {
        if ty_known(left) and left != "bool" {
            c = ck_error(c, "the left side of '" + text + "' is " + left + ", not bool", line)
        }
        if ty_known(right) and right != "bool" {
            c = ck_error(c, "the right side of '" + text + "' is " + right + ", not bool", line)
        }
        return ck_typed(c, "bool")
    }
    -- A struct or enum may have a method for the operator
    let method = ty_op_method(op)
    let left_head = ty_head(left)
    if method != "" and left_head != "" and c.fns.contains(left_head + "_" + method) {
        let sig = c.fns.get(left_head + "_" + method)
        -- The left operand's type arguments are the impl block's type
        -- parameters: Vec2[int] + Vec2[int] is the Vec2[T] that add
        -- returns, for T = int
        return ck_typed(c, ty_apply(sig.ret, sig.generics, ty_args(left)))
    }
    let lk = ty_kind(left)
    let rk = ty_kind(right)
    if ty_is_comparison(op) {
        if lk != "" and rk != "" and lk != rk {
            c = ck_error(c, "cannot compare " + left + " with " + right + " ('" + text + "')", line)
        } else if (lk == "" or rk == "") and ty_known(left) and ty_known(right) {
            -- Values that are not numbers, strings or bools: two of
            -- one type are equal or not (EQUALITY in ir_gen_expr.dm
            -- says how), and have no order unless the type has the
            -- operator's method
            if ty_fits(left, right) == false and ty_fits(right, left) == false {
                c = ck_error(c, "cannot compare " + left + " with " + right + " ('" + text + "')", line)
            } else if op != BINOP_EQ() and op != BINOP_NE() and ty_whole(left) and ty_whole(right) {
                c = ck_error(c, "operator '" + text + "' does not take " + left + " and " + right, line)
            }
        }
        return ck_typed(c, "bool")
    }
    let bits = op == BINOP_BIT_AND() or op == BINOP_BIT_OR() or op == BINOP_BIT_XOR()
    if bits or op == BINOP_SHL() or op == BINOP_SHR() {
        -- Integers, and for & | ^ two bools
        if bits and left == "bool" and right == "bool" { return ck_typed(c, "bool") }
        if bits and ((left == "bool" and ty_is_int(right)) or (ty_is_int(left) and right == "bool")) {
            c = ck_error(c, "operator '" + text + "' does not take " + left + " and " + right, line)
            return ck_typed(c, "")
        }
        let mut wrong = ""
        if ty_known(right) and ty_is_int(right) == false { wrong = right }
        if ty_known(left) and ty_is_int(left) == false { wrong = left }
        if wrong != "" {
            let mut takes = "integers"
            if bits { takes = "integers or bools" }
            c = ck_error(c, "operator '" + text + "' takes " + takes + ", not " + wrong, line)
            return ck_typed(c, "")
        }
    }
    if lk != "" and rk != "" {
        if lk == "bool" and rk == "bool" and bits { return ck_typed(c, "bool") }
        if lk != rk or lk == "bool" or (lk == "string" and op != BINOP_ADD()) {
            c = ck_error(c, "operator '" + text + "' does not take " + left + " and " + right, line)
            return ck_typed(c, "")
        }
        if lk == "string" { return ck_typed(c, "string") }
        if ty_is_float(left) { return ck_typed(c, left) }
        if ty_is_float(right) { return ck_typed(c, right) }
        -- A literal takes the other operand's width
        if left == "int" { return ck_typed(c, right) }
        return ck_typed(c, left)
    }
    if lk == "string" and op == BINOP_ADD() { return ck_typed(c, "string") }
    -- Two values that are not numbers or strings, of a type without
    -- the operator's method
    if lk == "" and rk == "" and ty_whole(left) and ty_whole(right) and (method != "" or op == BINOP_MOD()) {
        if starts_with(left, "fn(") == false and ck_names_simd(left) == false {
            c = ck_error(c, "operator '" + text + "' does not take " + left + " and " + right, line)
            return ck_typed(c, "")
        }
    }
    if ty_known(left) and left == right { return ck_typed(c, left) }
    return ck_typed(c, "")
}

fn ck_names_simd(t: string) -> bool {
    return t == "f32x4" or t == "f32x8" or t == "f64x2" or t == "f64x4" or t == "i32x4" or t == "i32x8" or t == "i64x2" or t == "i64x4"
}

-- a op b op c ... nests one level per operator on the left, so the
-- chain is walked in a loop: a long one would overflow the stack
fn ck_binary(c0: Checker, e: Expr) -> Typed {
    let mut c = c0
    let mut ops: List[int] = []
    let mut lines: List[int] = []
    let mut ids: List[int] = []
    let mut rights: List[Expr] = []
    let mut cur = e
    while cur.kind == EXPR_BINARY() {
        ops.push(cur.op)
        lines.push(cur.line)
        ids.push(cur.id)
        rights.push(*cur.right)
        cur = *cur.left
    }
    let first = ck_expr(c, cur)
    c = first.c
    let mut t = first.t
    let mut t_literal = is_int_literal_expr(cur)
    let mut t_float_literal = is_float_literal_expr(cur)
    let mut i = ops.len() - 1
    while i >= 0 {
        let mut right = ck_expr(c, rights[i])
        if (ops[i] == BINOP_EQ() or ops[i] == BINOP_NE()) and (ty_kind(t) == "" or ty_kind(right.t) == "") {
            -- What says its type only in part takes the rest from the
            -- other side: None in `o == None`, [] in `xs != []`
            right.c = ck_settle(right.c, rights[i], t)
            if i == ops.len() - 1 { right.c = ck_settle(right.c, cur, right.t) }
        }
        let result = ck_operator(right.c, ops[i], t, right.t, lines[i])
        let mut result_t = result.t
        -- Two integers give the type code generation computes with
        let right_literal = is_int_literal_expr(rights[i])
        if ty_is_int(result_t) and ty_is_int(t) and ty_is_int(right.t) {
            result_t = int_meet_type(t, t_literal, right.t, right_literal)
        }
        -- And two floats: a literal is an f32 beside an f32
        let right_float_literal = is_float_literal_expr(rights[i])
        if ty_is_float(result_t) and ty_is_float(t) and ty_is_float(right.t) {
            result_t = float_meet_type(ty_alias(t), t_float_literal, ty_alias(right.t), right_float_literal)
        }
        let arithmetic = ops[i] == BINOP_ADD() or ops[i] == BINOP_SUB() or ops[i] == BINOP_MUL() or ops[i] == BINOP_DIV()
        t_float_literal = t_float_literal and right_float_literal and arithmetic
        c = ck_note(result.c, ids[i], result_t)
        t = result_t
        -- is_int_literal_expr of the operator's expression
        t_literal = t_literal and right_literal and (ops[i] == BINOP_ADD() or ops[i] == BINOP_SUB() or ops[i] == BINOP_MUL())
        i = i - 1
    }
    return ck_typed(c, t)
}

fn ck_unary(c0: Checker, e: Expr) -> Typed {
    let operand = ck_expr(c0, *e.operand)
    if e.op == UNOP_NOT() {
        if ty_known(operand.t) and operand.t != "bool" {
            return ck_typed(ck_error(operand.c, "'not' takes a bool, not " + operand.t, e.line), "bool")
        }
        return ck_typed(operand.c, "bool")
    }
    if e.op == UNOP_NEG() {
        let kind = ty_kind(operand.t)
        if kind == "string" or kind == "bool" {
            return ck_typed(ck_error(operand.c, "'-' takes a number, not " + operand.t, e.line), "")
        }
        return operand
    }
    if e.op == UNOP_DEREF() {
        if ty_head(operand.t) == "Box" { return ck_typed(operand.c, ty_arg(operand.t, 0)) }
        if ty_known(operand.t) {
            return ck_typed(ck_error(operand.c, "'*' takes a Box, not " + operand.t, e.line), "")
        }
        return ck_typed(operand.c, "")
    }
    if e.op == UNOP_BIT_NOT() {
        if ty_known(operand.t) and ty_is_int(operand.t) == false {
            return ck_typed(ck_error(operand.c, "'~' takes an integer, not " + operand.t, e.line), "")
        }
        return operand
    }
    return operand
}

-- The type of field `field` of a struct of type `t`, or "?" when the
-- struct has no such field
fn ck_field_type(c: Checker, t: string, field: string) -> string {
    let sig = c.structs.get(ty_head(t))
    let at = ck_index_of(sig.field_names, field)
    if at < 0 { return "?" }
    return ty_apply(sig.field_types[at], sig.generics, ty_args(t))
}

fn ck_field(c0: Checker, e: Expr) -> Typed {
    -- Enum.variant, for a variant without values
    let named = *e.object
    if named.kind == EXPR_IDENTIFIER() {
        if c0.enums.contains(named.name) and ck_names_type(c0, named.name) {
            let variants = c0.enums.get(named.name)
            if ck_index_of(variants.variant_names, e.field) < 0 {
                return ck_typed(ck_error(c0, "enum '" + named.name + "' has no variant '" + e.field + "'", e.line), named.name)
            }
            return ck_typed(c0, named.name)
        }
    }
    let object = ck_expr(c0, named)
    let c = object.c
    let t = ty_unbox(object.t)
    let head = ty_head(t)
    let field = e.field
    if head == "Option" {
        if field == "has_value" { return ck_typed(c, "bool") }
        if field == "value" { return ck_typed(c, ty_arg(t, 0)) }
        return ck_typed(c, "")
    }
    if head == "Result" {
        if field == "is_ok" { return ck_typed(c, "bool") }
        if field == "ok" or field == "value" { return ck_typed(c, ty_arg(t, 0)) }
        if field == "err" or field == "error" { return ck_typed(c, ty_arg(t, 1)) }
        return ck_typed(c, "")
    }
    if head != "" and c.structs.contains(head) {
        let sig = c.structs.get(head)
        if ck_index_of(sig.field_names, field) < 0 {
            return ck_typed(ck_error(c, "struct '" + head + "' has no field '" + field + "'", e.line), "")
        }
        return ck_typed(c, ck_field_type(c, t, field))
    }
    return ck_typed(c, "")
}

fn ck_index(c0: Checker, e: Expr) -> Typed {
    let object = ck_expr(c0, *e.object)
    let index = ck_expr(object.c, *e.index)
    let mut c = index.c
    let t = ty_unbox(object.t)
    let head = ty_head(t)
    if head == "Map" { return ck_typed(c, ty_arg(t, 1)) }
    if head == "List" or t == "string" or starts_with(t, "[") {
        if ty_known(index.t) and ty_is_int(index.t) == false {
            c = ck_error(c, "an index is an integer, not " + index.t, e.line)
        }
        if t == "string" { return ck_typed(c, "string") }
        return ck_typed(c, ty_iterated(t))
    }
    return ck_typed(c, "")
}

-- Report a call of a method that passes another number of arguments
-- than the method takes after its parameter `first`. (The number of
-- arguments of a call by name is checked when its code is generated,
-- where the variadic externs are known.)
fn ck_arity(c: Checker, sig: FnSig, shown: string, first: int, count: int, line: int) -> Checker {
    if first + count == sig.params.len() { return c }
    if sig.variadic and first + count > sig.params.len() { return c }
    let mut expected = int_to_string(sig.params.len() - first)
    if sig.variadic { expected = "at least " + expected }
    return ck_error(c, "call to '" + shown + "' passes " + int_to_string(count) + " arguments, expected " + expected, line)
}

-- The type of the function as a value: "fn(int, string) -> bool".
-- A type parameter is "?".
fn ty_fn_text(sig: FnSig) -> string {
    let none: List[string] = []
    let mut text = "fn("
    let mut i = 0
    while i < sig.params.len() {
        if i > 0 { text = text + ", " }
        text = text + ty_apply(sig.params[i], sig.generics, none)
        i = i + 1
    }
    return text + ") -> " + ty_apply(sig.ret, sig.generics, none)
}

-- An error for each of the effects that the function being checked
-- does not declare. `shown` is what needs them, `how` what the
-- function does with it ("calls" or "uses"), and `tail` ends the
-- message. A function that declares no effects is not held to any.
fn ck_effects(c0: Checker, shown: string, effects: List[string], tail: string, how: string, line: int) -> Checker {
    let mut c = c0
    if c.fn_effects.len() == 0 { return c }
    let mut i = 0
    while i < effects.len() {
        if ck_index_of(c.fn_effects, effects[i]) < 0 {
            c = ck_error(c, "function '" + c.fn_name + "' " + how + " '" + shown + "' which requires effect '" + effects[i] + "'" + tail, line)
        }
        i = i + 1
    }
    return c
}

-- The type of a place, read from the types of its parts without
-- checking it: a variable's, a field's, an element's. "" for any other
-- expression.
fn ck_place_type(c: Checker, e: Expr) -> string {
    if e.kind == EXPR_IDENTIFIER() {
        let at = ck_var(c, e.name)
        if at >= 0 { return c.var_types[at] }
        return ""
    }
    if e.kind == EXPR_GROUPED() { return ck_place_type(c, *e.operand) }
    if e.kind == EXPR_FIELD_ACCESS() {
        return ck_field_type(c, ty_unbox(ck_place_type(c, *e.object)), e.field)
    }
    if e.kind == EXPR_INDEX_ACCESS() {
        let held = ty_unbox(ck_place_type(c, *e.object))
        if ty_head(held) == "Map" { return ty_arg(held, 1) }
        return ty_iterated(held)
    }
    return ""
}

-- The variable that must be `mut` for the place to be changed, when it
-- is not: p for p.x and for p.xs[i].y. "" when the place may be
-- changed: its variable is `mut`, it is no part of a variable (the
-- result of a call, say), or it is reached through a Box. A Box is a
-- pointer to a value of its own, which every copy of the Box shares:
-- `mut` on a variable that holds one is about the variable.
fn ck_fixed_root(c: Checker, e: Expr) -> string {
    let mut cur = e
    let mut walking = true
    while walking {
        if ty_head(ck_place_type(c, cur)) == "Box" { return "" }
        if cur.kind == EXPR_FIELD_ACCESS() or cur.kind == EXPR_INDEX_ACCESS() {
            cur = *cur.object
        } else if cur.kind == EXPR_GROUPED() {
            cur = *cur.operand
        } else if cur.kind == EXPR_UNARY() and cur.op == UNOP_DEREF() {
            return ""
        } else {
            walking = false
        }
    }
    if cur.kind != EXPR_IDENTIFIER() { return "" }
    let at = ck_var(c, cur.name)
    if at < 0 { return "" }
    if c.var_flags[at] != 0 and at >= c.lambda_floor { return "" }
    return cur.name
}

-- Why the variable cannot be changed: it is not `mut`, or it is a
-- variable of the function around the lambda being checked
fn ck_fixed_why(c: Checker, name: string) -> string {
    let at = ck_var(c, name)
    if at >= 0 and at < c.lambda_floor {
        return "'" + name + "', a variable of the function around this lambda: the lambda has its own copy of it"
    }
    return "'" + name + "', which is not declared 'mut'"
}

-- A value that a call changes must be one that may be changed. `what`
-- says what changes it.
fn ck_changes(c: Checker, e: Expr, what: string, line: int) -> Checker {
    let root = ck_fixed_root(c, e)
    if root == "" { return c }
    if e.kind == EXPR_IDENTIFIER() {
        return ck_error(c, what + " changes " + ck_fixed_why(c, root), line)
    }
    return ck_error(c, what + " changes a part of " + ck_fixed_why(c, root), line)
}

-- The arguments that go to `mut` parameters of the function are
-- changed by the call, from its parameter `first` on
fn ck_mut_args(c0: Checker, sig: FnSig, shown: string, first: int, args: List[Expr]) -> Checker {
    let mut c = c0
    let mut i = 0
    while i < args.len() and first + i < sig.changes.len() {
        if sig.changes[first + i] {
            c = ck_changes(c, args[i], "the call of '" + shown + "'", args[i].line)
        }
        i = i + 1
    }
    return c
}

-- ---- Builtin functions ----

-- The parameter types of a builtin function, joined with "|"; "" for
-- one that takes nothing. "*" is a parameter of any type, and
-- "printable" one that print() takes: a string, a number or a bool.
-- "skip" for a builtin whose arguments code generation checks.
fn ck_builtin_params(name: string) -> string {
    if name == "print" or name == "println" or name == "eprint" or name == "eprintln" { return "printable" }
    if name == "panic" or name == "parse_int" or name == "parse_float" or name == "string_to_int" { return "string" }
    if name == "string_trim" or name == "string_to_upper" or name == "string_to_lower" { return "string" }
    if name == "is_alpha" or name == "is_digit" or name == "is_whitespace" or name == "is_alnum" or name == "char_to_string" { return "string" }
    if name == "file_read" or name == "file_exists" or name == "system" { return "string" }
    if name == "path_dirname" or name == "path_basename" or name == "path_extension" or name == "path_stem" { return "string" }
    if name == "fs_readdir" or name == "fs_mkdir" or name == "fs_remove" or name == "os_getenv" or name == "env_get" { return "string" }
    if name == "tcp_listen" or name == "tcp_connect" { return "string" }
    if name == "exit" or name == "int_to_string" or name == "args_get" or name == "mem_sweep" or name == "chr" or name == "sleep_ms" { return "int" }
    if name == "tcp_accept" or name == "tcp_close" or name == "thread_join" { return "int" }
    if name == "mutex_lock" or name == "mutex_unlock" or name == "mutex_destroy" { return "int" }
    if name == "float_to_string" { return "float" }
    if name == "bool_to_string" or name == "assert" { return "bool" }
    if name == "char_at" or name == "byte_at" { return "string|int" }
    if name == "substr" { return "string|int|int" }
    if name == "string_contains" or name == "string_find" or name == "starts_with" or name == "ends_with" or name == "string_split" { return "string|string" }
    if name == "file_write" or name == "file_append" or name == "path_join" or name == "fs_rename" { return "string|string" }
    if name == "string_replace" { return "string|string|string" }
    if name == "tcp_read" { return "int|int" }
    if name == "tcp_write" { return "int|string" }
    if name == "read_line" or name == "args_len" or name == "mem_mark" or name == "flush" or name == "fs_getcwd" { return "" }
    if name == "read_key" or name == "read_key_nb" or name == "term_width" or name == "term_height" or name == "time_ms" or name == "mutex_new" { return "" }
    if name == "Box_null" or name == "Map_new" { return "" }
    if name == "Box_new" or name == "hash_key" or name == "len" { return "*" }
    if name == "assert_eq" { return "*|*" }
    return "skip"
}

-- Check the arguments of a call of a builtin function, and the
-- effect it needs
fn ck_builtin_call(c0: Checker, name: string, arg_types: List[string], args: List[Expr], line: int) -> Checker {
    let mut c = c0
    let effect = builtin_required_effect(name)
    if effect != "" {
        let mut needs: List[string] = []
        needs.push(effect)
        c = ck_effects(c, name, needs, "", "calls", line)
    }
    let joined = ck_builtin_params(name)
    if joined == "skip" { return c }
    let mut params: List[string] = []
    if joined != "" { params = string_split(joined, "|") }
    if params.len() != arg_types.len() {
        return ck_error(c, name + "() takes " + arguments_text(params.len()) + ", not " + int_to_string(arg_types.len()), line)
    }
    let mut i = 0
    while i < params.len() {
        let got = arg_types[i]
        if got == "void" {
            c = ck_error(c, "argument " + int_to_string(i + 1) + " of '" + name + "' is the result of a call that returns no value", args[i].line)
        } else if params[i] == "printable" {
            if ty_known(got) and ty_kind(got) == "" {
                c = ck_error(c, "type mismatch in argument 1 of '" + name + "': expected a string, a number or a bool, found " + got, args[i].line)
            }
        } else if params[i] != "*" {
            if ck_fits_value(c, params[i], got, args[i]) == false {
                c = ck_error(c, "type mismatch in argument " + int_to_string(i + 1) + " of '" + name + "': expected " + params[i] + ", found " + got, args[i].line)
            } else {
                c = ck_settle(c, args[i], params[i])
            }
        }
        i = i + 1
    }
    if name == "assert_eq" {
        if ck_fits(c, arg_types[0], arg_types[1]) == false and ck_fits(c, arg_types[1], arg_types[0]) == false {
            c = ck_error(c, "assert_eq() compares two values of one type, not " + arg_types[0] + " and " + arg_types[1], line)
        }
    }
    return c
}

-- What the signature's type parameters stand for in a call with
-- arguments of the types, from its parameter `first` on. `binds0` is
-- what is known before the arguments are looked at.
fn ck_binds(sig: FnSig, first: int, binds0: List[string], arg_types: List[string]) -> List[string] {
    let mut binds = binds0
    let mut i = 0
    while i < arg_types.len() and first + i < sig.params.len() {
        binds = ty_bind(sig.params[first + i], arg_types[i], sig.generics, binds)
        i = i + 1
    }
    return binds
}

-- Record the type arguments of the call with the id, when all are known
fn ck_note_type_args(c: Checker, id: int, binds: List[string]) -> Checker {
    if c.record == false or id < 0 or id >= c.type_args.len() or binds.len() == 0 { return c }
    let mut joined = ""
    let mut i = 0
    while i < binds.len() {
        if ty_whole(binds[i]) == false or binds[i] == "void" { return c }
        if i > 0 { joined = joined + "|" }
        joined = joined + binds[i]
        i = i + 1
    }
    let mut out = c
    out.type_args[id] = joined
    return out
}

-- Whether the type has an `impl Trait for` it
fn ck_implements(c: Checker, t: string, trait_name: string) -> bool {
    let head = ty_head(t)
    if head == "" or c.type_traits.contains(head) == false { return false }
    return string_contains(c.type_traits.get(head), "|" + trait_name + "|")
}

-- Check the arguments of a call against the signature, starting at
-- its parameter `first`, and give what the call returns. `binds` is
-- what the signature's type parameters are known to stand for.
fn ck_apply(c0: Checker, sig: FnSig, shown: string, first: int, binds0: List[string], arg_types: List[string], args: List[Expr], line: int) -> Typed {
    let mut c = c0
    let binds = ck_binds(sig, first, binds0, arg_types)
    -- What a type parameter stands for must implement the parameter's traits
    let mut bi = 0
    while bi < sig.bounds.len() and bi < binds.len() {
        if sig.bounds[bi] != "" and ty_whole(binds[bi]) {
            let wanted = string_split(sig.bounds[bi], "+")
            let mut wi = 0
            while wi < wanted.len() {
                if c.traits.contains(wanted[wi]) and ck_implements(c, binds[bi], wanted[wi]) == false {
                    c = ck_error(c, "'" + shown + "' takes for " + sig.generics[bi] + " a type that implements " + wanted[wi] + ", which " + binds[bi] + " does not", line)
                }
                wi = wi + 1
            }
        }
        bi = bi + 1
    }
    let mut i = 0
    while i < arg_types.len() and first + i < sig.params.len() {
        let want = ty_apply(sig.params[first + i], sig.generics, binds)
        if ck_fits_value(c, want, arg_types[i], args[i]) == false {
            c = ck_error(c, "type mismatch in argument " + int_to_string(i + 1) + " of '" + shown + "': expected " + want + ", found " + arg_types[i], args[i].line)
        } else {
            c = ck_settle(c, args[i], want)
        }
        i = i + 1
    }
    return ck_typed(c, ty_apply(sig.ret, sig.generics, binds))
}

-- A call of a value whose type is a function's, such as a variable
-- that holds a lambda
fn ck_call_value(c0: Checker, held: string, shown: string, arg_types: List[string], args: List[Expr], line: int) -> Typed {
    let mut c = c0
    if starts_with(held, "fn(") == false {
        if ty_known(held) == false { return ck_typed(c, "") }
        if shown == "a function value" {
            return ck_typed(ck_error(c, "a value of type " + held + " cannot be called", line), "")
        }
        return ck_typed(ck_error(c, "'" + shown + "' is a value of type " + held + ", which cannot be called", line), "")
    }
    let params = ty_fn_params(held)
    if params.len() != arg_types.len() {
        c = ck_error(c, "call to '" + shown + "' passes " + int_to_string(arg_types.len()) + " arguments, expected " + int_to_string(params.len()), line)
        return ck_typed(c, ty_fn_ret(held))
    }
    let mut i = 0
    while i < params.len() {
        if ck_fits_value(c, params[i], arg_types[i], args[i]) == false {
            c = ck_error(c, "type mismatch in argument " + int_to_string(i + 1) + " of '" + shown + "': expected " + params[i] + ", found " + arg_types[i], args[i].line)
        } else {
            c = ck_settle(c, args[i], params[i])
        }
        i = i + 1
    }
    return ck_typed(c, ty_fn_ret(held))
}

-- What a builtin function returns
fn ck_builtin_type(name: string) -> string {
    let ir_type = builtin_return_type(name)
    if ir_type == "void" { return "void" }
    if ir_type == "ptr" { return "" }
    return ty_alias(builtin_src_type(ir_type))
}

-- A call of `name` with arguments of the types. `generic_args` are
-- the type arguments written at the call, as in name[int](x).
-- `call_id` is the id of the call's expression, -1 when it has none
-- of its own (a pipeline).
fn ck_call_named(c0: Checker, name: string, generic_args: List[string], arg_types: List[string], args: List[Expr], line: int, call_id: int) -> Typed {
    let c = c0
    let mut first_type = "?"
    if arg_types.len() > 0 {
        if ty_known(arg_types[0]) { first_type = arg_types[0] }
    }
    if name == "Some" { return ck_typed(c, "Option[" + first_type + "]") }
    if name == "Ok" { return ck_typed(c, "Result[" + first_type + ", ?]") }
    if name == "Err" { return ck_typed(c, "Result[?, " + first_type + "]") }
    if name == "Box_new" { return ck_typed(c, "Box[" + first_type + "]") }
    if name == "Box_null" { return ck_typed(ck_builtin_call(c, name, arg_types, args, line), "Box[?]") }
    if name == "Map_new" { return ck_typed(ck_builtin_call(c, name, arg_types, args, line), "Map[?, ?]") }
    if name == "List_new" { return ck_typed(c, "List[?]") }
    -- A variable that holds a function
    let at = ck_var(c, name)
    if at >= 0 {
        let held = c.var_types[at]
        if starts_with(held, "fn(") or ty_known(held) == false or c.fns.contains(name) == false {
            return ck_call_value(c, held, name, arg_types, args, line)
        }
    }
    let builtin = is_builtin_fn(name)
    if c.fns.contains(name) {
        if builtin { return ck_typed(c, "") }
        let sig = c.fns.get(name)
        let mut binds = ty_unbound(sig.generics.len())
        let none: List[string] = []
        let mut gi = 0
        while gi < generic_args.len() and gi < binds.len() {
            binds[gi] = ty_rewrite(generic_args[gi], none, none)
            gi = gi + 1
        }
        let mut counted = ck_arity(c, sig, name, 0, arg_types.len(), line)
        counted = ck_async_call(counted, sig, name, call_id, line)
        counted = ck_effects(counted, name, sig.effects, " not in caller's effects", "calls", line)
        counted = ck_mut_args(counted, sig, name, 0, args)
        let applied = ck_apply(counted, sig, name, 0, binds, arg_types, args, line)
        if sig.generics.len() == 0 { return applied }
        return ck_typed(ck_note_type_args(applied.c, call_id, ck_binds(sig, 0, binds, arg_types)), applied.t)
    }
    if builtin { return ck_typed(ck_builtin_call(c, name, arg_types, args, line), ck_builtin_type(name)) }
    if is_simd_builtin(name) { return ck_simd_call(c, name, arg_types, args, line) }
    return ck_typed(ck_error(c, "undefined function '" + name + "'", line), "")
}

-- A call of a SIMD builtin. simd_splat_T(x) and simd_set_T(a, b, ...)
-- make a vector of the type T their name ends in, from floats for a
-- vector of floats and integers for one of integers. simd_add,
-- simd_sub, simd_mul and simd_div take two vectors of one type and
-- give a third. simd_extract(v, i) gives lane i as a float or an int.
fn ck_simd_call(c0: Checker, name: string, arg_types: List[string], args: List[Expr], line: int) -> Typed {
    let mut c = c0
    if starts_with(name, "simd_splat_") or starts_with(name, "simd_set_") {
        let vec = parse_simd_type_from_name(name)
        if is_simd_type(vec) == false {
            return ck_typed(ck_error(c, "undefined function '" + name + "'", line), "")
        }
        let mut wanted = 1
        if starts_with(name, "simd_set_") { wanted = simd_lane_count(vec) }
        if arg_types.len() != wanted {
            return ck_typed(ck_error(c, name + "() takes " + arguments_text(wanted) + ", not " + int_to_string(arg_types.len()), line), vec)
        }
        let mut lane = "int"
        if is_simd_float_type(vec) { lane = "float" }
        let mut i = 0
        while i < args.len() {
            if arg_types[i] == "void" {
                c = ck_error(c, "argument " + int_to_string(i + 1) + " of '" + name + "' is the result of a call that returns no value", args[i].line)
            } else if ck_fits(c, lane, arg_types[i]) == false {
                c = ck_error(c, "type mismatch in argument " + int_to_string(i + 1) + " of '" + name + "': expected " + lane + ", found " + arg_types[i], args[i].line)
            }
            i = i + 1
        }
        return ck_typed(c, vec)
    }
    if arg_types.len() != 2 {
        return ck_typed(ck_error(c, name + "() takes 2 arguments, not " + int_to_string(arg_types.len()), line), "")
    }
    let vec = arg_types[0]
    if name == "simd_extract" {
        if arg_types[1] == "void" or ck_fits(c, "int", arg_types[1]) == false {
            c = ck_error(c, "type mismatch in argument 2 of 'simd_extract': expected int, found " + arg_types[1], args[1].line)
        }
        if ty_known(vec) == false { return ck_typed(c, "") }
        if is_simd_type(vec) == false {
            return ck_typed(ck_error(c, "type mismatch in argument 1 of 'simd_extract': expected a SIMD vector, such as f32x4, found " + vec, args[0].line), "")
        }
        -- A lane that does not exist, where the index is written out
        let mut index = args[1]
        while index.kind == EXPR_GROUPED() {
            index = *index.operand
        }
        if index.kind == EXPR_LITERAL_INT() {
            if index.int_val >= simd_lane_count(vec) {
                c = ck_error(c, "simd_extract() of lane " + int_to_string(index.int_val) + " of a " + vec + ", which has lanes 0 to " + int_to_string(simd_lane_count(vec) - 1), args[1].line)
            }
        }
        if is_simd_float_type(vec) { return ck_typed(c, "float") }
        return ck_typed(c, "int")
    }
    let other = arg_types[1]
    if ty_known(vec) and is_simd_type(vec) == false {
        return ck_typed(ck_error(c, "type mismatch in argument 1 of '" + name + "': expected a SIMD vector, such as f32x4, found " + vec, args[0].line), "")
    }
    if ty_known(other) and is_simd_type(other) == false {
        return ck_typed(ck_error(c, "type mismatch in argument 2 of '" + name + "': expected a SIMD vector, such as f32x4, found " + other, args[1].line), "")
    }
    if ty_known(vec) and ty_known(other) and vec != other {
        return ck_typed(ck_error(c, name + "() takes two vectors of one type, not " + vec + " and " + other, line), vec)
    }
    if ty_known(vec) { return ck_typed(c, vec) }
    if ty_known(other) { return ck_typed(c, other) }
    return ck_typed(c, "")
}

fn ck_call(c0: Checker, e: Expr) -> Typed {
    let callee = *e.callee
    if callee.kind != EXPR_IDENTIFIER() {
        let called = ck_expr(c0, callee)
        let rest = ck_exprs(called.c, e.args)
        return ck_call_value(rest.c, called.t, "a function value", rest.types, e.args, e.line)
    }
    let found = ck_exprs(c0, e.args)
    return ck_call_named(found.c, callee.name, callee.generic_args, found.types, e.args, e.line, e.id)
}

-- Whether a trait the type implements has the method
fn ck_trait_method(c: Checker, type_name: string, method: string) -> string {
    if c.type_traits.contains(type_name) == false { return "" }
    let traits = string_split(c.type_traits.get(type_name), "|")
    let mut i = 0
    while i < traits.len() {
        if traits[i] != "" and c.fns.contains(traits[i] + "_" + method) { return traits[i] }
        i = i + 1
    }
    return ""
}

-- The types of the values a variant of an enum carries
fn ck_payloads(sig: EnumSig, at: int) -> List[string] {
    if sig.variant_payloads[at] == "" {
        let none: List[string] = []
        return none
    }
    return string_split(sig.variant_payloads[at], "|")
}

-- Check the values given to a variant of an enum against the number
-- and the types of the values the variant carries
fn ck_variant_values(c0: Checker, enum_name: string, variant: string, enum_args: List[string], types: List[string], values: List[Expr], line: int) -> Checker {
    let mut c = c0
    let sig = c.enums.get(enum_name)
    let at = ck_index_of(sig.variant_names, variant)
    if at < 0 { return c }
    let payloads = ck_payloads(sig, at)
    if payloads.len() != types.len() {
        return ck_error(c, "variant '" + enum_name + "." + variant + "' carries " + values_text(payloads.len()) + ", not " + int_to_string(types.len()), line)
    }
    let mut i = 0
    while i < payloads.len() and i < types.len() {
        let want = ty_apply(payloads[i], sig.generics, enum_args)
        if ck_fits_value(c, want, types[i], values[i]) == false {
            c = ck_error(c, "type mismatch in value " + int_to_string(i + 1) + " of '" + enum_name + "." + variant + "': expected " + want + ", found " + types[i], values[i].line)
        } else {
            c = ck_settle(c, values[i], want)
        }
        i = i + 1
    }
    return c
}

-- The type of the value a variant's constructor makes: the enum, and
-- for a generic enum what its type parameters stand for as the types
-- of the values say, "Opt[int]" for Opt.Has(3) and "Opt[?]" for
-- Opt.Empty, whose place says the rest (ck_settle)
fn ck_enum_value_type(c: Checker, enum_name: string, variant: string, types: List[string]) -> string {
    let sig = c.enums.get(enum_name)
    if sig.generics.len() == 0 { return enum_name }
    let mut binds = ty_unbound(sig.generics.len())
    let at = ck_index_of(sig.variant_names, variant)
    if at >= 0 {
        -- A wrong number of values, which is reported, says nothing
        let mut payloads = ck_payloads(sig, at)
        if payloads.len() != types.len() { payloads = ty_unbound(0) }
        let mut i = 0
        while i < payloads.len() and i < types.len() {
            binds = ty_bind(payloads[i], types[i], sig.generics, binds)
            i = i + 1
        }
    }
    let mut t = enum_name + "["
    let mut bi = 0
    while bi < binds.len() {
        if bi > 0 { t = t + ", " }
        if binds[bi] == "" { t = t + "?" } else { t = t + binds[bi] }
        bi = bi + 1
    }
    return t + "]"
}

-- The generic enum that the expression makes a value of, when it is a
-- variant's constructor (`Opt.Has(3)`, `Opt.Empty`); else ""
fn ck_generic_variant(c: Checker, e: Expr) -> string {
    let mut name = ""
    let mut variant = ""
    if e.kind == EXPR_ENUM_LITERAL() {
        name = e.enum_name
        variant = e.variant_name
    } else if e.kind == EXPR_METHOD_CALL() {
        let object = *e.object
        if object.kind != EXPR_IDENTIFIER() { return "" }
        name = object.name
        variant = e.method
    } else {
        return ""
    }
    if ck_var(c, name) >= 0 or c.enums.contains(name) == false { return "" }
    let sig = c.enums.get(name)
    if sig.generics.len() == 0 or ck_index_of(sig.variant_names, variant) < 0 { return "" }
    return name
}

-- ck_settle for the constructor of a variant of the generic enum
-- `name`, whose value goes to a place of the type `want`
fn ck_settle_variant(c0: Checker, e: Expr, name: string, want: string) -> Checker {
    let mut c = c0
    let have = c.types[e.id]
    if have != want and ty_fits(want, have) {
        c = ck_note(c, e.id, want)
    }
    let sig = c.enums.get(name)
    let mut variant = e.variant_name
    let mut values = e.payload
    if e.kind == EXPR_METHOD_CALL() {
        variant = e.method
        values = e.args
    }
    let payloads = ck_payloads(sig, ck_index_of(sig.variant_names, variant))
    let enum_args = ty_args(want)
    let mut i = 0
    while i < payloads.len() and i < values.len() {
        c = ck_settle(c, values[i], ty_apply(payloads[i], sig.generics, enum_args))
        i = i + 1
    }
    return c
}

-- Type.name(args): a constructor of Box, Map or List, a variant of
-- an enum, or a function of the type's impl block
fn ck_static_call(c0: Checker, type_name: string, e: Expr, arg_types: List[string]) -> Typed {
    let method = e.method
    -- The type arguments of `Stack[int].new()`
    let explicit = ck_static_type_args(e)
    let mut c = c0
    if explicit.len() > 0 {
        c = ck_type_exists(c, ck_generic_type(type_name, explicit), e.line)
    }
    if type_name == "Box" and method == "new" {
        if arg_types.len() > 0 {
            if ty_known(arg_types[0]) { return ck_typed(c, "Box[" + arg_types[0] + "]") }
        }
        return ck_typed(c, "Box[?]")
    }
    if type_name == "Box" and method == "null" { return ck_typed(c, "Box[?]") }
    if type_name == "Map" and method == "new" { return ck_typed(c, "Map[?, ?]") }
    if type_name == "List" and method == "new" { return ck_typed(c, "List[?]") }
    if c.enums.contains(type_name) {
        let sig = c.enums.get(type_name)
        if ck_index_of(sig.variant_names, method) >= 0 {
            let mut made = ck_enum_value_type(c, type_name, method, arg_types)
            let mut enum_args = ty_unbound(0)
            if explicit.len() == sig.generics.len() and explicit.len() > 0 {
                made = ck_norm(c, ck_generic_type(type_name, explicit))
                enum_args = ty_args(made)
            }
            return ck_typed(ck_variant_values(c, type_name, method, enum_args, arg_types, e.args, e.line), made)
        }
        if c.fns.contains(type_name + "_" + method) == false and ck_trait_method(c, type_name, method) == "" {
            return ck_typed(ck_error(c, "enum '" + type_name + "' has no variant or function '" + method + "'", e.line), type_name)
        }
    }
    let key = type_name + "_" + method
    if c.fns.contains(key) {
        let sig = c.fns.get(key)
        let mut counted = ck_arity(c, sig, type_name + "." + method, 0, arg_types.len(), e.line)
        counted = ck_async_call(counted, sig, type_name + "." + method, e.id, e.line)
        counted = ck_effects(counted, type_name + "." + method, sig.effects, " not in caller's effects", "calls", e.line)
        counted = ck_mut_args(counted, sig, type_name + "." + method, 0, e.args)
        let binds = ck_static_binds(counted, sig, explicit)
        let applied = ck_apply(counted, sig, type_name + "." + method, 0, binds, arg_types, e.args, e.line)
        if sig.generics.len() == 0 { return applied }
        -- What the type parameters stand for, where the arguments say
        -- it; else the place the value goes to does (ck_settle_static)
        return ck_typed(ck_note_type_args(applied.c, e.id, ck_binds(sig, 0, binds, arg_types)), applied.t)
    }
    return ck_typed(c, "")
}

-- The type arguments written in `Stack[int].new()`: those of the
-- name the function is called on
fn ck_static_type_args(e: Expr) -> List[string] {
    let object = *e.object
    if object.kind != EXPR_IDENTIFIER() {
        let none: List[string] = []
        return none
    }
    return object.generic_args
}

-- What the type parameters of the function of a type's impl block
-- stand for before its arguments are looked at: the type arguments
-- written at the call for the impl block's, nothing for the rest
fn ck_static_binds(c: Checker, sig: FnSig, explicit: List[string]) -> List[string] {
    let mut binds = ty_unbound(sig.generics.len())
    let mut i = 0
    while i < explicit.len() and i < binds.len() {
        binds[i] = ck_norm(c, explicit[i])
        i = i + 1
    }
    return binds
}

-- "Stack_new" for `Stack.new(...)` and `Stack[int].new(...)`, the call
-- of a function of the impl block of a generic type; else ""
fn ck_generic_static(c: Checker, e: Expr) -> string {
    let object = *e.object
    if object.kind != EXPR_IDENTIFIER() { return "" }
    if ck_names_type(c, object.name) == false { return "" }
    let key = object.name + "_" + e.method
    if c.fns.contains(key) == false { return "" }
    let sig = c.fns.get(key)
    if sig.generics.len() == 0 or sig.has_self { return "" }
    return key
}

-- ck_settle for `Stack.new()`, the call of the function `key` of a
-- generic type's impl block, whose value goes to a place of the type
-- `want`: the place says what the type parameters stand for where the
-- call's arguments do not, as in `let s: Stack[int] = Stack.new()`
fn ck_settle_static(c0: Checker, e: Expr, key: string, want: string) -> Checker {
    let mut c = c0
    let sig = c.fns.get(key)
    let mut binds = ck_static_binds(c, sig, ck_static_type_args(e))
    binds = ty_bind(sig.ret, want, sig.generics, binds)
    let mut arg_types: List[string] = []
    let mut i = 0
    while i < e.args.len() {
        let id = e.args[i].id
        if id >= 0 and id < c.types.len() {
            arg_types.push(c.types[id])
        } else {
            arg_types.push("")
        }
        i = i + 1
    }
    binds = ck_binds(sig, 0, binds, arg_types)
    c = ck_note_type_args(c, e.id, binds)
    let made = ty_apply(sig.ret, sig.generics, binds)
    if ty_whole(made) { c = ck_note(c, e.id, made) }
    i = 0
    while i < e.args.len() and i < sig.params.len() {
        c = ck_settle(c, e.args[i], ty_apply(sig.params[i], sig.generics, binds))
        i = i + 1
    }
    return c
}

-- The builtin function that a method of a string is: s.trim() is
-- string_trim(s), s.split(",") is string_split(s, ","). "" for a name
-- that is no method of a string.
fn string_method_builtin(method: string) -> string {
    if method == "trim" { return "string_trim" }
    if method == "to_upper" { return "string_to_upper" }
    if method == "to_lower" { return "string_to_lower" }
    if method == "contains" { return "string_contains" }
    if method == "find" { return "string_find" }
    if method == "replace" { return "string_replace" }
    if method == "split" { return "string_split" }
    if method == "starts_with" or method == "ends_with" or method == "substr" or method == "char_at" or method == "byte_at" { return method }
    return ""
}

fn ck_method_arity(c: Checker, recv: string, method: string, want: int, got: int, line: int) -> Checker {
    if want == got { return c }
    return ck_error(c, "'" + method + "' of a " + ty_head(recv) + " takes " + arguments_text(want) + ", not " + int_to_string(got), line)
}

-- A method call on a value whose type has no such method: x.f(a) is
-- f(x, a) when there is a function f, and else the error `message`
fn ck_no_method(c0: Checker, e: Expr, recv: string, arg_types: List[string], message: string) -> Typed {
    let mut c = c0
    let method = e.method
    if c.fns.contains(method) == false or is_builtin_fn(method) or ck_var(c, method) >= 0 {
        return ck_typed(ck_error(c, message, e.line), "")
    }
    let mut types: List[string] = []
    types.push(recv)
    let mut args: List[Expr] = []
    args.push(*e.object)
    let mut i = 0
    while i < e.args.len() {
        types.push(arg_types[i])
        args.push(e.args[i])
        i = i + 1
    }
    if c.record and e.id >= 0 and e.id < c.targets.len() {
        c.targets[e.id] = "call:" + method
    }
    let none: List[string] = []
    return ck_call_named(c, method, none, types, args, e.line, e.id)
}

fn ck_method_call(c0: Checker, e: Expr) -> Typed {
    let mut c = c0
    let object = *e.object
    let method = e.method
    -- A name that is no variable may be a type's
    let mut type_name = ""
    if object.kind == EXPR_IDENTIFIER() {
        let name = object.name
        if ck_names_type(c, name) {
            type_name = name
        } else if ck_var(c, name) < 0 and (name == "Box" or name == "Map" or name == "List") {
            type_name = name
        }
        if type_name == "" and object.generic_args.len() > 0 {
            let written = ck_generic_type(name, object.generic_args)
            c = ck_error(c, "'" + name + "' is not a type: '" + written + "." + method + "(...)' calls a function of the type " + written + "; write (" + written + ")." + method + "(...) to call a method of an element", e.line)
            return ck_typed(c, "")
        }
    }
    let mut recv = ""
    if type_name == "" {
        let found = ck_expr(c, object)
        c = found.c
        recv = ty_unbox(found.t)
    }
    let found_args = ck_exprs(c, e.args)
    c = found_args.c
    let arg_types = found_args.types
    if type_name != "" { return ck_static_call(c, type_name, e, arg_types) }

    let head = ty_head(recv)
    if ty_known(head) == false { return ck_typed(c, "") }
    if c.fns.contains(head + "_" + method) {
        let sig = c.fns.get(head + "_" + method)
        c = ck_effects(c, head + "." + method, sig.effects, " not in caller's effects", "calls", e.line)
        c = ck_async_call(c, sig, head + "." + method, e.id, e.line)
        if sig.has_self {
            if sig.changes[0] { c = ck_changes(c, object, "'" + head + "." + method + "'", e.line) }
        }
        -- The receiver's type arguments are the impl block's type parameters
        let mut binds = ty_unbound(sig.generics.len())
        let recv_args = ty_args(recv)
        let mut bi = 0
        while bi < recv_args.len() and bi < binds.len() {
            binds[bi] = recv_args[bi]
            bi = bi + 1
        }
        let mut first = 0
        if sig.has_self { first = 1 }
        c = ck_arity(c, sig, head + "." + method, first, arg_types.len(), e.line)
        c = ck_mut_args(c, sig, head + "." + method, first, e.args)
        return ck_apply(c, sig, head + "." + method, first, binds, arg_types, e.args, e.line)
    }
    if head == "List" {
        let elem = ty_arg(recv, 0)
        if method == "push" {
            c = ck_method_arity(c, recv, method, 1, arg_types.len(), e.line)
            c = ck_changes(c, object, "'push'", e.line)
            if arg_types.len() == 1 {
                if ck_fits_value(c, elem, arg_types[0], e.args[0]) == false {
                    c = ck_error(c, "type mismatch in the value pushed to a " + recv + ": expected " + elem + ", found " + arg_types[0], e.line)
                } else {
                    c = ck_settle(c, e.args[0], elem)
                }
            }
            return ck_typed(c, "void")
        }
        if method == "pop" {
            c = ck_method_arity(c, recv, method, 0, arg_types.len(), e.line)
            return ck_typed(ck_changes(c, object, "'pop'", e.line), elem)
        }
        if method == "len" { return ck_typed(ck_method_arity(c, recv, method, 0, arg_types.len(), e.line), "int") }
        if method == "contains" {
            c = ck_method_arity(c, recv, method, 1, arg_types.len(), e.line)
            if arg_types.len() == 1 {
                if ck_fits(c, elem, arg_types[0]) == false {
                    c = ck_error(c, "type mismatch in the value looked for in a " + recv + ": expected " + elem + ", found " + arg_types[0], e.line)
                } else {
                    c = ck_settle(c, e.args[0], elem)
                }
            }
            return ck_typed(c, "bool")
        }
        return ck_no_method(c, e, recv, arg_types, "a " + recv + " has no method '" + method + "': a list has push, pop, len and contains")
    }
    if recv == "string" {
        if method == "len" { return ck_typed(ck_method_arity(c, recv, method, 0, arg_types.len(), e.line), "int") }
        -- A program's own function of the name is called with the
        -- string first, as for any type; else the string's method is
        -- the builtin function that takes the string first
        let builtin = string_method_builtin(method)
        if builtin != "" and (c.fns.contains(method) == false or is_builtin_fn(method)) and ck_var(c, method) < 0 {
            let mut types: List[string] = []
            types.push(recv)
            let mut args: List[Expr] = []
            args.push(object)
            let mut ai = 0
            while ai < e.args.len() {
                types.push(arg_types[ai])
                args.push(e.args[ai])
                ai = ai + 1
            }
            if c.record and e.id >= 0 and e.id < c.targets.len() {
                c.targets[e.id] = "call:" + builtin
            }
            let none: List[string] = []
            return ck_call_named(c, builtin, none, types, args, e.line, e.id)
        }
        return ck_no_method(c, e, recv, arg_types, "a string has no method '" + method + "': it has len, trim, to_upper, to_lower, contains, find, starts_with, ends_with, replace, split, substr, char_at and byte_at")
    }
    if c.structs.contains(head) or c.enums.contains(head) {
        -- A field that holds a function is called like a method
        if c.structs.contains(head) {
            let sig = c.structs.get(head)
            let at = ck_index_of(sig.field_names, method)
            if at >= 0 {
                let held = ty_apply(sig.field_types[at], sig.generics, ty_args(recv))
                if starts_with(held, "fn(") == false {
                    return ck_typed(ck_error(c, "field '" + method + "' of '" + head + "' is a " + held + ", which cannot be called", e.line), "")
                }
                return ck_call_value(c, held, head + "." + method, arg_types, e.args, e.line)
            }
        }
        let from_trait = ck_trait_method(c, head, method)
        if from_trait != "" {
            let sig = c.fns.get(from_trait + "_" + method)
            c = ck_effects(c, from_trait + "." + method, sig.effects, " not in caller's effects", "calls", e.line)
            return ck_typed(c, ty_apply(sig.ret, sig.generics, ty_unbound(0)))
        }
        if starts_with(head, DYN_PREFIX()) {
            -- A method of the trait that its `dyn` does not have
            let trait_name = substr(head, len(DYN_PREFIX()), len(head) - len(DYN_PREFIX()))
            if c.fns.contains(trait_name + "_" + method) {
                return ck_no_method(c, e, recv, arg_types, "the method '" + method + "' of the trait '" + trait_name + "' cannot be called on a '" + head + "': a 'dyn' has the methods that take 'self', have no type parameters and do not name 'Self' elsewhere")
            }
        }
        return ck_no_method(c, e, recv, arg_types, "type '" + head + "' has no method '" + method + "'")
    }
    -- A `dyn` that is reported where it is written
    if starts_with(recv, DYN_PREFIX()) { return ck_typed(c, "") }
    return ck_no_method(c, e, recv, arg_types, "type '" + recv + "' has no method '" + method + "'")
}

fn ck_struct_literal(c0: Checker, e: Expr) -> Typed {
    let mut c = ck_type_exists(c0, e.type_name, e.line)
    let written = ck_norm(c, e.type_name)
    let head = ty_head(written)
    let values = ck_exprs(c, e.field_values)
    c = values.c
    if head == "" or c.structs.contains(head) == false { return ck_typed(c, "") }
    let sig = c.structs.get(head)
    let mut i = 0
    let mut t = written
    if sig.generics.len() > 0 and ty_args(written).len() == 0 {
        -- `Pair { first: 1, second: "one" }` is a Pair[int, string]: the
        -- values say what the type parameters stand for, and where they
        -- do not, the place the struct goes to does (ck_settle)
        let mut binds = ty_unbound(sig.generics.len())
        while i < e.field_names.len() and i < values.types.len() {
            let at = ck_index_of(sig.field_names, e.field_names[i])
            if at >= 0 { binds = ty_bind(sig.field_types[at], values.types[i], sig.generics, binds) }
            i = i + 1
        }
        let mut found: List[string] = []
        i = 0
        while i < binds.len() {
            if binds[i] == "" { found.push("?") } else { found.push(binds[i]) }
            i = i + 1
        }
        t = ck_generic_type(head, found)
        i = 0
    }
    while i < e.field_names.len() and i < values.types.len() {
        let field = e.field_names[i]
        if ck_index_of(sig.field_names, field) < 0 {
            c = ck_error(c, "struct '" + head + "' has no field '" + field + "'", e.field_values[i].line)
        } else {
            let want = ck_field_type(c, t, field)
            if ck_fits_value(c, want, values.types[i], e.field_values[i]) == false {
                c = ck_error(c, "type mismatch in field '" + field + "' of '" + head + "': expected " + want + ", found " + values.types[i], e.field_values[i].line)
            } else {
                c = ck_settle(c, e.field_values[i], want)
            }
        }
        i = i + 1
    }
    i = 0
    while i < sig.field_names.len() {
        if ck_index_of(e.field_names, sig.field_names[i]) < 0 {
            c = ck_error(c, "the struct literal of '" + head + "' leaves out the field '" + sig.field_names[i] + "'", e.line)
        }
        i = i + 1
    }
    return ck_typed(c, t)
}

fn ck_enum_literal(c0: Checker, e: Expr) -> Typed {
    let values = ck_exprs(c0, e.payload)
    let mut c = values.c
    let name = e.enum_name
    if ck_var(c, name) >= 0 { return ck_typed(c, "") }
    -- The type arguments of `Opt[int].Empty`
    if e.generic_args.len() > 0 {
        c = ck_type_exists(c, ck_generic_type(name, e.generic_args), e.line)
    }
    if c.enums.contains(name) {
        let sig = c.enums.get(name)
        if ck_index_of(sig.variant_names, e.variant_name) >= 0 {
            let mut made = ck_enum_value_type(c, name, e.variant_name, values.types)
            let mut enum_args = ty_unbound(0)
            if e.generic_args.len() == sig.generics.len() and e.generic_args.len() > 0 {
                made = ck_norm(c, ck_generic_type(name, e.generic_args))
                enum_args = ty_args(made)
            }
            return ck_typed(ck_variant_values(c, name, e.variant_name, enum_args, values.types, e.payload, e.line), made)
        }
        if c.fns.contains(name + "_" + e.variant_name) == false and ck_trait_method(c, name, e.variant_name) == "" {
            return ck_typed(ck_error(c, "enum '" + name + "' has no variant or function '" + e.variant_name + "'", e.line), name)
        }
    }
    -- Type.Name(args), a function of the type's impl block
    let key = name + "_" + e.variant_name
    if c.fns.contains(key) {
        let sig = c.fns.get(key)
        let counted = ck_arity(c, sig, name + "." + e.variant_name, 0, values.types.len(), e.line)
        return ck_apply(counted, sig, name + "." + e.variant_name, 0, ty_unbound(sig.generics.len()), values.types, e.payload, e.line)
    }
    return ck_typed(c, "")
}

fn ck_array_literal(c0: Checker, e: Expr) -> Typed {
    let values = ck_exprs(c0, e.elements)
    let mut c = values.c
    -- The first element's type, with what the later ones know of its
    -- parts. Numbers meet at one type as the branches of an `if` do:
    -- a constant takes the type of the others, a narrower one widens.
    let mut elem = "?"
    let mut elem_literal = false
    let mut i = 0
    while i < values.types.len() {
        let t = values.types[i]
        if ty_known(t) and t != "void" {
            let literal = is_number_literal_expr(branch_value_expr(e.elements[i]))
            if elem == "?" {
                elem = t
                elem_literal = literal
            } else if numbers_meet(ty_alias(elem), ty_alias(t)) {
                elem = int_meet_type(elem, elem_literal, t, literal)
                elem_literal = elem_literal and literal
            } else if ty_kind(elem) != "" and ty_kind(t) != "" and (ty_kind(elem) != ty_kind(t) or ty_is_float(elem) != ty_is_float(t)) {
                c = ck_error(c, "the elements of this list are " + elem + " and " + t, e.elements[i].line)
            } else if ty_fits(elem, t) {
                elem = ty_merge(elem, t)
            }
        }
        i = i + 1
    }
    i = 0
    while i < e.elements.len() {
        c = ck_settle(c, e.elements[i], elem)
        i = i + 1
    }
    return ck_typed(c, "List[" + elem + "]")
}

fn ck_condition(c: Checker, t: string, what: string, line: int) -> Checker {
    if ty_known(t) and t != "bool" {
        return ck_error(c, "the condition of " + what + " is " + t + ", not bool", line)
    }
    return c
}

-- The statements, and the value of the last when it is an expression
fn ck_body_value(c0: Checker, stmts: List[Stmt], used: bool) -> Typed {
    let n = stmts.len()
    if n == 0 { return ck_typed(c0, "void") }
    let mut c = c0
    let mut i = 0
    while i < n - 1 {
        c = ck_stmt(c, stmts[i])
        i = i + 1
    }
    let last = stmts[n - 1]
    if last.kind == STMT_EXPRESSION() { return ck_expr(c, *last.expr) }
    -- A block that is used as a value and ends in an `if` with an
    -- `else`, or in a `match`, has the value of that statement
    if used and stmt_gives_value(last) { return ck_expr(c, stmt_value_expr(last)) }
    return ck_typed(ck_stmt(c, last), "void")
}

fn ck_block(c0: Checker, e: Expr) -> Typed {
    let mark = c0.var_names.len()
    if e.has_result {
        let c = ck_stmts(c0, e.stmts)
        let result = ck_expr(c, *e.result)
        return ck_typed(ck_unbind(result.c, mark), result.t)
    }
    let value = ck_body_value(c0, e.stmts, true)
    return ck_typed(ck_unbind(value.c, mark), value.t)
}

fn ck_if_expr(c0: Checker, e: Expr) -> Typed {
    let cond = ck_expr(c0, *e.condition)
    let mut c = ck_condition(cond.c, cond.t, "'if'", e.line)
    let then_value = ck_expr(c, *e.then_branch)
    c = then_value.c
    let mut t = ""
    if ty_known(then_value.t) and then_value.t != "void" { t = then_value.t }
    if e.has_else {
        let else_value = ck_expr(c, *e.else_branch)
        c = else_value.c
        if t == "" and ty_known(else_value.t) and else_value.t != "void" { t = else_value.t }
        if t != "" and else_value.t != "void" and ck_fits(c, t, else_value.t) == false and ck_fits(c, else_value.t, t) == false {
            c = ck_error(c, "the branches of this 'if' give " + t + " and " + else_value.t, e.line)
        } else if numbers_meet(t, else_value.t) and numbers_meet(then_value.t, else_value.t) {
            -- Integers of two widths give one value, and so do an f32 and a float
            t = int_meet_type(t, is_number_literal_expr(branch_value_expr(*e.then_branch)), else_value.t, is_number_literal_expr(branch_value_expr(*e.else_branch)))
        } else if t != "" and else_value.t != "void" {
            -- Each branch may know what the other does not: Some(n) and None
            t = ty_merge(t, else_value.t)
        }
        c = ck_settle(c, *e.then_branch, t)
        c = ck_settle(c, *e.else_branch, t)
    }
    return ck_typed(c, t)
}

-- The types of a variant's values in a pattern on a value of type t
fn ck_variant_types(c: Checker, p: Pattern, t: string) -> List[string] {
    let mut types: List[string] = []
    let head = ty_head(t)
    if p.variant == "Some" {
        if head == "Option" { types.push(ty_arg(t, 0)) }
        return types
    }
    if p.variant == "Ok" {
        if head == "Result" { types.push(ty_arg(t, 0)) }
        return types
    }
    if p.variant == "Err" {
        if head == "Result" { types.push(ty_arg(t, 1)) }
        return types
    }
    let mut enum_name = p.enum_type
    if enum_name == "" { enum_name = head }
    if enum_name == "" or c.enums.contains(enum_name) == false { return types }
    let sig = c.enums.get(enum_name)
    let at = ck_index_of(sig.variant_names, p.variant)
    if at < 0 or sig.variant_payloads[at] == "" { return types }
    -- The enum's type parameters are what the matched value's type gives them
    let mut enum_args: List[string] = []
    if head == enum_name { enum_args = ty_args(t) }
    let payloads = string_split(sig.variant_payloads[at], "|")
    let mut i = 0
    while i < payloads.len() {
        types.push(ty_apply(payloads[i], sig.generics, enum_args))
        i = i + 1
    }
    return types
}

-- Declare the variables a pattern binds when it meets a value of type t
fn ck_bind_pattern(c0: Checker, p: Pattern, t: string) -> Checker {
    let mut c = c0
    if p.kind == PAT_IDENTIFIER() {
        return ck_bind(c, p.name, t, 1)
    }
    if p.kind == PAT_ENUM_VARIANT() {
        let types = ck_variant_types(c, p, t)
        let mut i = 0
        while i < p.bindings.len() {
            let mut bound = ""
            if i < types.len() { bound = types[i] }
            c = ck_bind(c, p.bindings[i], bound, 1)
            i = i + 1
        }
        return c
    }
    return c
}

-- "string", "bool" or "number" for a pattern that is a literal
fn ck_literal_kind(p: Pattern) -> string {
    if p.kind != PAT_LITERAL() { return "" }
    let lit = *p.literal_expr
    if lit.kind == EXPR_LITERAL_STRING() { return "string" }
    if lit.kind == EXPR_LITERAL_BOOL() { return "bool" }
    return "number"
}

-- Whether an arm of the match takes the variant, guards aside
fn ck_arm_for(e: Expr, variant: string) -> bool {
    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        if arm.has_guard == false and arm.pattern.kind == PAT_ENUM_VARIANT() and arm.pattern.variant == variant { return true }
        i = i + 1
    }
    return false
}

-- Whether an arm of the match takes the bool
fn ck_arm_for_bool(e: Expr, value: bool) -> bool {
    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        if arm.has_guard == false and arm.pattern.kind == PAT_LITERAL() {
            let lit = *arm.pattern.literal_expr
            if lit.kind == EXPR_LITERAL_BOOL() and lit.bool_val == value { return true }
        }
        i = i + 1
    }
    return false
}

-- What a match on a value of type t leaves out, "" when its arms
-- take every value. `used` says that the match gives a value, which
-- it must then give whatever is matched.
fn ck_match_missing(c: Checker, e: Expr, t: string, used: bool) -> string {
    let mut enum_name = ""
    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        let kind = arm.pattern.kind
        if arm.has_guard == false and (kind == PAT_WILDCARD() or kind == PAT_IDENTIFIER()) { return "" }
        if kind == PAT_ENUM_VARIANT() and enum_name == "" { enum_name = arm.pattern.enum_type }
        i = i + 1
    }
    let head = ty_head(t)
    if head == "Option" {
        if ck_arm_for(e, "Some") == false { return "Some" }
        if ck_arm_for(e, "None") == false { return "None" }
        return ""
    }
    if head == "Result" {
        if ck_arm_for(e, "Ok") == false { return "Ok" }
        if ck_arm_for(e, "Err") == false { return "Err" }
        return ""
    }
    if t == "bool" {
        if ck_arm_for_bool(e, true) == false { return "true" }
        if ck_arm_for_bool(e, false) == false { return "false" }
        return ""
    }
    if ty_known(head) and c.enums.contains(head) { enum_name = head }
    if enum_name != "" and c.enums.contains(enum_name) and (ty_known(head) == false or head == enum_name) {
        let sig = c.enums.get(enum_name)
        let mut v = 0
        while v < sig.variant_names.len() {
            if ck_arm_for(e, sig.variant_names[v]) == false { return enum_name + "." + sig.variant_names[v] }
            v = v + 1
        }
        return ""
    }
    if used and ty_kind(t) != "" { return "every other " + t }
    return ""
}

-- Report a pattern that names a variant the enum does not have, or
-- binds more values than the variant carries
fn ck_variant_pattern(c: Checker, p: Pattern, t: string, line: int) -> Checker {
    if p.kind != PAT_ENUM_VARIANT() { return c }
    let mut enum_name = p.enum_type
    if enum_name == "" { enum_name = ty_head(t) }
    if enum_name == "" or c.enums.contains(enum_name) == false { return c }
    let sig = c.enums.get(enum_name)
    let at = ck_index_of(sig.variant_names, p.variant)
    if at < 0 {
        return ck_error(c, "enum '" + enum_name + "' has no variant '" + p.variant + "'", line)
    }
    let payloads = ck_payloads(sig, at)
    if p.bindings.len() > payloads.len() {
        return ck_error(c, "variant '" + enum_name + "." + p.variant + "' carries " + values_text(payloads.len()) + ", but the pattern binds " + int_to_string(p.bindings.len()), line)
    }
    return c
}

-- Whether the checker knows what sort of type `t` is: a number, a
-- string, a bool, an Option, a Result, a List, a Map, a Box, a struct
-- or an enum. (Not a type parameter, a function or a `dyn`.)
fn ck_sort_known(c: Checker, t: string) -> bool {
    if ty_known(t) == false { return false }
    if ty_kind(t) != "" { return true }
    let head = ty_head(t)
    if head == "Option" or head == "Result" or head == "List" or head == "Map" or head == "Box" { return true }
    return c.enums.contains(head) or c.structs.contains(head)
}

-- A pattern that is a variant must be a variant of the matched
-- value's type: `Some` and `None` of an Option, `Ok` and `Err` of a
-- Result, `Color.Red` of a Color. (That the enum has the variant is
-- ck_variant_pattern's.)
fn ck_variant_of(c: Checker, p: Pattern, t: string, line: int) -> Checker {
    if p.kind != PAT_ENUM_VARIANT() or ck_sort_known(c, t) == false { return c }
    let head = ty_head(t)
    let mut of = ty_head(p.enum_type)
    if of == "" {
        -- A variant of the value's own enum
        if c.enums.contains(head) { return c }
        if p.variant == "Some" or p.variant == "None" {
            of = "Option"
        } else if p.variant == "Ok" or p.variant == "Err" {
            of = "Result"
        } else {
            return ck_error(c, "the pattern '" + p.variant + "' is a variant, but the matched value is " + t, line)
        }
    } else if c.enums.contains(of) == false {
        return c
    }
    if of == head { return c }
    return ck_error(c, "the pattern '" + p.variant + "' is a variant of " + of + ", but the matched value is " + t, line)
}

-- A match. `used` says that its value goes somewhere.
fn ck_match(c0: Checker, e: Expr, used: bool) -> Typed {
    let scrutinee = ck_expr(c0, *e.scrutinee)
    let mut c = scrutinee.c
    let mut t = ""
    -- Whether every arm so far gives an integer literal
    let mut t_literal = false
    let scrutinee_kind = ty_kind(scrutinee.t)
    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        let mark = c.var_names.len()
        let pattern_kind = ck_literal_kind(arm.pattern)
        if pattern_kind != "" and pattern_kind != scrutinee_kind and (scrutinee_kind != "" or ck_sort_known(c, scrutinee.t)) {
            let lit = *arm.pattern.literal_expr
            c = ck_error(c, "a pattern is a " + pattern_kind + ", but the matched value is " + scrutinee.t, lit.line)
        }
        c = ck_variant_of(c, arm.pattern, scrutinee.t, e.line)
        c = ck_variant_pattern(c, arm.pattern, scrutinee.t, e.line)
        c = ck_bind_pattern(c, arm.pattern, scrutinee.t)
        if arm.has_guard {
            let guard_expr = *arm.guard
            let guard = ck_expr(c, guard_expr)
            c = ck_condition(guard.c, guard.t, "a match guard", guard_expr.line)
        }
        let mut arm_type = ""
        let mut arm_literal = false
        if arm.is_expr_body {
            let value = ck_expr(c, *arm.body_expr)
            c = value.c
            arm_type = value.t
            arm_literal = is_number_literal_expr(branch_value_expr(*arm.body_expr))
        } else {
            let value = ck_body_value(c, arm.body, used)
            c = value.c
            arm_type = value.t
            arm_literal = stmts_value_is_number_literal(arm.body)
        }
        c = ck_unbind(c, mark)
        if t == "" and ty_known(arm_type) and arm_type != "void" {
            t = arm_type
            t_literal = arm_literal
        }
        if used and t != "" and arm_type != "void" and ck_fits(c, t, arm_type) == false and ck_fits(c, arm_type, t) == false {
            c = ck_error(c, "the arms of this match give " + t + " and " + arm_type, e.line)
        } else if numbers_meet(t, arm_type) {
            -- Integers of two widths give one value, and so do an f32 and a float
            t = int_meet_type(t, t_literal, arm_type, arm_literal)
            t_literal = t_literal and arm_literal
        } else if t != "" and ty_known(arm_type) and arm_type != "void" {
            t = ty_merge(t, arm_type)
        }
        i = i + 1
    }
    -- Each arm gives a value of the type the arms give together
    i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        if arm.is_expr_body {
            c = ck_settle(c, *arm.body_expr, t)
        } else {
            c = ck_settle_last(c, arm.body, t)
        }
        i = i + 1
    }
    let missing = ck_match_missing(c, e, scrutinee.t, used)
    if missing != "" {
        c = ck_error(c, "this match has no arm for " + missing, e.line)
    }
    return ck_typed(c, t)
}

fn ck_lambda(c0: Checker, e: Expr) -> Typed {
    let mut c = c0
    let mark = c.var_names.len()
    let outer_ret = c.ret_type
    let outer_floor = c.lambda_floor
    c.lambda_floor = mark
    let mut text = "fn("
    let mut i = 0
    while i < e.lambda_params.len() {
        let lp = e.lambda_params[i]
        let mut t = ""
        if lp.type_name != "" {
            c = ck_type_exists(c, lp.type_name, e.line)
            t = ck_norm(c, lp.type_name)
        }
        c = ck_bind(c, lp.name, t, 1)
        if i > 0 { text = text + ", " }
        if t == "" { text = text + "?" } else { text = text + t }
        i = i + 1
    }
    let mut ret = ""
    if e.lambda_ret_type != "" {
        c = ck_type_exists(c, e.lambda_ret_type, e.line)
        ret = ck_norm(c, e.lambda_ret_type)
    }
    c.ret_type = ret
    let outer_lambda_ret = c.lambda_ret
    c.lambda_ret = "-"
    if ret == "" { c.lambda_ret = "" }
    let body = *e.lambda_body
    let value = ck_expr(c, body)
    c = value.c
    let returned = c.lambda_ret
    c.lambda_ret = outer_lambda_ret
    if body.kind == EXPR_BLOCK() {
        -- A block's lambda returns what its first `return` gives, else
        -- the block's value
        if ret == "" {
            if returned != "" and returned != "-" {
                ret = returned
            } else if ty_known(value.t) {
                ret = value.t
            }
        }
    } else {
        if ret == "" {
            if ty_known(value.t) { ret = value.t }
        } else if value.t != "void" and ck_fits_value(c, ret, value.t, body) == false {
            c = ck_error(c, "type mismatch in the lambda's value: expected " + ret + ", found " + value.t, body.line)
        } else {
            c = ck_settle(c, body, ret)
        }
    }
    c.ret_type = outer_ret
    c.lambda_floor = outer_floor
    c = ck_unbind(c, mark)
    if ret == "" { ret = "?" }
    return ck_typed(c, text + ") -> " + ret)
}

-- value? is the value of an Ok or a Some
-- `value?`: the value of an Ok or a Some. An Err or a None leaves
-- the function, which must return a Result with that error type, or
-- an Option.
fn ck_propagate(c0: Checker, e: Expr) -> Typed {
    let operand = ck_expr(c0, *e.operand)
    let mut c = operand.c
    let head = ty_head(operand.t)
    if head != "Result" and head != "Option" {
        if ty_known(operand.t) {
            c = ck_error(c, "'?' needs a Result or an Option, found " + operand.t, e.line)
        }
        return ck_typed(c, "")
    }
    let returns = c.ret_type
    if ty_known(returns) {
        if ty_head(returns) != head {
            let mut leaves = "an Err"
            let mut needs = "a Result"
            if head == "Option" {
                leaves = "a None"
                needs = "an Option"
            }
            let mut found = "returns " + returns
            if returns == "void" { found = "returns no value" }
            c = ck_error(c, "'?' leaves the function with " + leaves + ", so the function must return " + needs + "; it " + found, e.line)
        } else if head == "Result" {
            let given = ty_arg(operand.t, 1)
            let wanted = ty_arg(returns, 1)
            if ty_known(given) and ty_known(wanted) and ty_same(given, wanted) == false {
                c = ck_error(c, "'?' leaves the function with an error of type " + given + ", but the function returns " + returns, e.line)
            }
        }
    }
    return ck_typed(c, ty_arg(operand.t, 0))
}

-- ============================================================
-- STATEMENTS
-- ============================================================

fn ck_stmts(c0: Checker, stmts: List[Stmt]) -> Checker {
    let mut c = c0
    let mut i = 0
    while i < stmts.len() {
        c = ck_stmt(c, stmts[i])
        i = i + 1
    }
    return c
}

-- The statements, in a scope of their own
fn ck_scope(c0: Checker, stmts: List[Stmt]) -> Checker {
    let mark = c0.var_names.len()
    return ck_unbind(ck_stmts(c0, stmts), mark)
}

fn ck_let(c0: Checker, s: Stmt) -> Checker {
    let value = *s.let_value
    let found = ck_expr(c0, value)
    let mut c = ck_needs_value(found.c, value, false)
    let mut t = found.t
    if t == "void" {
        c = ck_error(c, "a variable holds the result of a call that returns no value", value.line)
        t = ""
    }
    if s.let_type != "" {
        c = ck_type_exists(c, s.let_type, s.line)
        let want = ck_norm(c, s.let_type)
        if ck_fits_value(c, want, t, value) == false {
            c = ck_error(c, "type mismatch in the value of '" + s.let_name + "': expected " + want + ", found " + t, value.line)
        } else {
            c = ck_settle(c, value, want)
        }
        t = want
    }
    let mut flag = 0
    if s.let_is_mut { flag = 1 }
    return ck_bind(c, s.let_name, t, flag)
}

fn ck_return(c0: Checker, s: Stmt) -> Checker {
    let want = c0.ret_type
    if s.has_ret_value == false {
        if want == "" and c0.lambda_ret == "" {
            -- The first return of a lambda says what the lambda returns
            let mut first = c0
            first.lambda_ret = "void"
            first.ret_type = "void"
            return first
        }
        if ty_known(want) and want != "void" {
            return ck_error(c0, "this return has no value, but the function returns " + want, s.line)
        }
        return c0
    }
    let value = *s.ret_value
    let found = ck_expr(c0, value)
    let mut c = found.c
    if want != "void" { c = ck_needs_value(c, value, false) }
    if want == "void" {
        if ty_known(found.t) and found.t != "void" {
            return ck_error(c, "this return has a value, but the function returns none", s.line)
        }
        return c
    }
    if found.t == "void" {
        if ty_known(want) {
            return ck_error(c, "the return value is a call that returns no value", s.line)
        }
        return c
    }
    if want == "" and c.lambda_ret == "" and ty_known(found.t) {
        -- The first return of a lambda says what the lambda returns
        c.lambda_ret = found.t
        c.ret_type = found.t
        return c
    }
    if ck_fits_value(c, want, found.t, value) == false {
        return ck_error(c, "type mismatch in the return value: expected " + want + ", found " + found.t, value.line)
    }
    return ck_settle(c, value, want)
}

fn ck_assign_op(op: int) -> int {
    if op == ASSIGN_ADD() { return BINOP_ADD() }
    if op == ASSIGN_SUB() { return BINOP_SUB() }
    if op == ASSIGN_MUL() { return BINOP_MUL() }
    if op == ASSIGN_DIV() { return BINOP_DIV() }
    return BINOP_MOD()
}

fn ck_assignment(c0: Checker, s: Stmt) -> Checker {
    let mut c = c0
    let target = *s.assign_target
    let mut shown = ""
    let mut place = target
    while place.kind == EXPR_GROUPED() {
        place = *place.operand
    }
    let is_box_value = place.kind == EXPR_UNARY() and place.op == UNOP_DEREF()
    if place.kind != EXPR_IDENTIFIER() and place.kind != EXPR_FIELD_ACCESS() and place.kind != EXPR_INDEX_ACCESS() and is_box_value == false {
        c = ck_error(c, "cannot assign to this expression: it is not a variable, a field, an element or the value of a box", s.line)
    }
    if target.kind == EXPR_FIELD_ACCESS() or target.kind == EXPR_INDEX_ACCESS() {
        let root = ck_fixed_root(c, *target.object)
        if root != "" {
            c = ck_error(c, "cannot assign to a part of " + ck_fixed_why(c, root), s.line)
        }
    }
    if target.kind == EXPR_IDENTIFIER() {
        shown = " to '" + target.name + "'"
        let at = ck_var(c, target.name)
        if at >= 0 {
            if c.var_flags[at] == 0 or at < c.lambda_floor {
                c = ck_error(c, "cannot assign to " + ck_fixed_why(c, target.name), s.line)
            }
        } else if c.consts.contains(target.name) {
            c = ck_error(c, "cannot assign to the constant '" + target.name + "'", s.line)
        }
    }
    let to = ck_expr(c, target)
    let value = ck_expr(to.c, *s.assign_value)
    c = ck_needs_value(value.c, *s.assign_value, false)
    if value.t == "void" {
        return ck_error(c, "the assignment" + shown + " takes the result of a call that returns no value", s.line)
    }
    if s.assign_op == ASSIGN_EQ() {
        if ck_fits_value(c, to.t, value.t, *s.assign_value) == false {
            c = ck_error(c, "type mismatch in the assignment" + shown + ": expected " + to.t + ", found " + value.t, s.line)
        } else {
            c = ck_settle(c, *s.assign_value, to.t)
        }
        return c
    }
    let combined = ck_operator(c, ck_assign_op(s.assign_op), to.t, value.t, s.line)
    return combined.c
}

fn ck_for(c0: Checker, s: Stmt) -> Checker {
    let mut c = c0
    let iter = *s.for_iter
    let mut elem = ""
    if iter.kind == EXPR_RANGE() {
        let low = ck_expr(c, *iter.left)
        let high = ck_expr(low.c, *iter.right)
        c = high.c
        elem = "int"
    } else {
        let over = ck_expr(c, iter)
        c = over.c
        elem = ty_iterated(ty_unbox(over.t))
    }
    let mark = c.var_names.len()
    c = ck_bind(c, s.for_var, elem, 1)
    c = ck_stmts(c, s.for_body)
    return ck_unbind(c, mark)
}

fn ck_stmt(c0: Checker, s: Stmt) -> Checker {
    let k = s.kind
    if k == STMT_LET() { return ck_let(c0, s) }
    if k == STMT_RETURN() { return ck_return(c0, s) }
    if k == STMT_EXPRESSION() {
        let stated = *s.expr
        if stated.kind == EXPR_BLOCK() and stated.has_result == false {
            -- A block that is a statement of its own gives no value
            return ck_note(ck_scope(c0, stated.stmts), stated.id, "void")
        }
        let value = ck_expr(c0, stated)
        return value.c
    }
    if k == STMT_ASSIGNMENT() { return ck_assignment(c0, s) }
    if k == STMT_IF() {
        let cond = ck_expr(c0, *s.if_cond)
        let mut c = ck_condition(cond.c, cond.t, "'if'", s.line)
        c = ck_scope(c, s.if_then)
        if s.has_else_branch {
            c = ck_scope(c, s.if_else)
        }
        return c
    }
    if k == STMT_WHILE() {
        let cond = ck_expr(c0, *s.while_cond)
        let c = ck_condition(cond.c, cond.t, "'while'", s.line)
        return ck_scope(c, s.while_body)
    }
    if k == STMT_FOR() { return ck_for(c0, s) }
    if k == STMT_LOOP() { return ck_scope(c0, s.loop_body) }
    if k == STMT_REGION() { return ck_scope(c0, s.region_body) }
    if k == STMT_MATCH() {
        let matched = *s.match_expr
        if matched.kind == EXPR_MATCH() {
            let value = ck_match(c0, matched, false)
            return value.c
        }
        let value = ck_expr(c0, matched)
        return value.c
    }
    return c0
}

-- ============================================================
-- FUNCTIONS
-- ============================================================

-- Whether the expression never gives control to what follows it: a
-- call of panic or exit, or branches that all return
fn ck_diverges(e: Expr) -> bool {
    if e.kind == EXPR_FUNCTION_CALL() {
        let callee = *e.callee
        return callee.kind == EXPR_IDENTIFIER() and (callee.name == "panic" or callee.name == "exit")
    }
    if e.kind == EXPR_GROUPED() { return ck_diverges(*e.operand) }
    if e.kind == EXPR_BLOCK() { return ck_always_returns(e.stmts) }
    if e.kind == EXPR_IF() {
        if e.has_else == false { return false }
        return ck_diverges(*e.then_branch) and ck_diverges(*e.else_branch)
    }
    if e.kind == EXPR_MATCH() {
        let mut i = 0
        while i < e.match_arms.len() {
            let arm = e.match_arms[i]
            if arm.is_expr_body {
                if ck_diverges(*arm.body_expr) == false { return false }
            } else if ck_always_returns(arm.body) == false {
                return false
            }
            i = i + 1
        }
        return e.match_arms.len() > 0
    }
    return false
}

-- Whether the statements end by leaving the code around them: what
-- ck_always_returns finds, a break or a continue
fn ck_leaves(stmts: List[Stmt]) -> bool {
    if stmts.len() == 0 { return false }
    let s = stmts[stmts.len() - 1]
    if s.kind == STMT_BREAK() or s.kind == STMT_CONTINUE() { return true }
    if s.kind == STMT_IF() {
        return s.has_else_branch and ck_leaves(s.if_then) and ck_leaves(s.if_else)
    }
    return ck_always_returns(stmts)
}

-- The value of the expression is used, so each of its branches must
-- give one: report an `if` without an `else`, and a branch or an arm
-- that ends in a statement or in a call that returns nothing. A
-- branch that leaves (return, break, continue, panic) needs no value.
-- `inner` says that the expression is a branch of the one in use.
fn ck_needs_value(c: Checker, e: Expr, inner: bool) -> Checker {
    let k = e.kind
    if k == EXPR_GROUPED() { return ck_needs_value(c, *e.operand, inner) }
    if k == EXPR_IF() {
        if e.has_else == false {
            return ck_error(c, "this 'if' is used as a value, and has no 'else' to give one", e.line)
        }
        return ck_needs_value(ck_needs_value(c, *e.then_branch, true), *e.else_branch, true)
    }
    if k == EXPR_BLOCK() {
        if e.has_result { return ck_needs_value(c, *e.result, true) }
        return ck_block_needs_value(c, e.stmts, e.line)
    }
    if k == EXPR_MATCH() {
        let mut out = c
        let mut i = 0
        while i < e.match_arms.len() {
            let arm = e.match_arms[i]
            if arm.no_else {
                out = ck_error(out, "this 'if let' is used as a value, and has no 'else' to give one", e.line)
            } else if arm.is_expr_body {
                out = ck_needs_value(out, *arm.body_expr, true)
            } else {
                out = ck_block_needs_value(out, arm.body, e.line)
            }
            i = i + 1
        }
        return out
    }
    -- A call that returns nothing. (One that is the whole value is
    -- reported where the value goes.)
    if inner and c.record and e.id >= 0 and e.id < c.types.len() {
        if c.types[e.id] == "void" and ck_diverges(e) == false {
            return ck_error(c, "this is used as a value, and it is a call that returns none", e.line)
        }
    }
    return c
}

-- ck_needs_value for the statements of a block
fn ck_block_needs_value(c: Checker, stmts: List[Stmt], line: int) -> Checker {
    if stmts.len() == 0 {
        return ck_error(c, "this block is used as a value, and it is empty", line)
    }
    let last = stmts[stmts.len() - 1]
    if stmt_gives_value(last) { return ck_needs_value(c, stmt_value_expr(last), true) }
    if ck_leaves(stmts) { return c }
    if last.kind == STMT_IF() {
        return ck_error(c, "this block is used as a value, and it ends in an 'if' that has no 'else' to give one", last.line)
    }
    return ck_error(c, "this block is used as a value, and it ends in a statement that gives none", last.line)
}

-- Whether the statements cannot reach their end: the last one
-- returns, or never ends, on every path
fn ck_always_returns(stmts: List[Stmt]) -> bool {
    if stmts.len() == 0 { return false }
    let s = stmts[stmts.len() - 1]
    let k = s.kind
    if k == STMT_RETURN() or k == STMT_LOOP() { return true }
    if k == STMT_IF() {
        return s.has_else_branch and ck_always_returns(s.if_then) and ck_always_returns(s.if_else)
    }
    if k == STMT_WHILE() {
        let cond = *s.while_cond
        if cond.kind == EXPR_LITERAL_BOOL() { return cond.bool_val }
        return cond.kind == EXPR_IDENTIFIER() and cond.name == "true"
    }
    if k == STMT_REGION() { return ck_always_returns(s.region_body) }
    if k == STMT_MATCH() { return ck_diverges(*s.match_expr) }
    if k == STMT_EXPRESSION() { return ck_diverges(*s.expr) }
    return false
}

-- Check a function's body. self_type is the type of the impl block it
-- is in, "" for a function; outer_generics are that block's type
-- parameters.
-- What type parameter `i` stands for in an instance; "?", any type,
-- where the function is checked for all its instances at once
fn ck_bound(binds: List[string], i: int) -> string {
    if i < binds.len() and binds[i] != "" { return binds[i] }
    return "?"
}

-- `binds` are what the type parameters stand for, the outer ones and
-- then the function's own, when one instance is checked (`instance`)
fn ck_function(c0: Checker, fd: FunctionDecl, self_type: string, outer_generics: List[string], binds: List[string], instance: bool) -> Checker {
    let mut c = c0
    let mut names: List[string] = []
    names.push("Self")
    let mut repls: List[string] = []
    repls.push(self_type)
    let mut gi = 0
    while gi < outer_generics.len() {
        names.push(outer_generics[gi])
        repls.push(ck_bound(binds, gi))
        gi = gi + 1
    }
    gi = 0
    while gi < fd.generic_params.len() {
        names.push(fd.generic_params[gi])
        repls.push(ck_bound(binds, outer_generics.len() + gi))
        gi = gi + 1
    }
    c.norm_names = names
    c.norm_repls = repls
    -- A generic function is compiled once for each set of type
    -- arguments, and a trait's own method once for each type that
    -- takes it: its expressions have no one type, until one instance
    -- is checked
    c.record = instance or (outer_generics.len() == 0 and fd.generic_params.len() == 0 and self_type != "?")
    let no_names: List[string] = []
    let no_types: List[string] = []
    let no_flags: List[int] = []
    c.var_names = no_names
    c.var_types = no_types
    c.var_flags = no_flags

    let mut i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        c = ck_type_exists(c, p.type_name, fd.line)
        let mut flag = 0
        if p.is_mut { flag = 1 }
        c = ck_bind(c, p.name, ck_norm(c, p.type_name), flag)
        i = i + 1
    }
    c = ck_type_exists(c, fd.return_type, fd.line)
    if instance == false {
        let mut bi = 0
        while bi < fd.generic_bounds.len() {
            if fd.generic_bounds[bi] != "" {
                let wanted = string_split(fd.generic_bounds[bi], "+")
                let mut wi = 0
                while wi < wanted.len() {
                    if c.traits.contains(wanted[wi]) == false {
                        c = ck_error(c, "undefined trait '" + wanted[wi] + "'", fd.line)
                    }
                    wi = wi + 1
                }
            }
            bi = bi + 1
        }
    }
    if fd.is_extern { return c }

    let ret = ck_norm(c, fd.return_type)
    c.ret_type = ret
    c.fn_name = fd.name
    c.fn_effects = fd.effects
    c = ck_stmts(c, fd.body)
    if ty_known(ret) and ret != "void" and fd.name != "main" and ck_always_returns(fd.body) == false {
        c = ck_error(c, "function '" + fd.name + "' can reach its end without returning a value of type " + ret, fd.line)
    }
    return c
}

fn ck_struct_decl(c0: Checker, sd: StructDecl) -> Checker {
    let mut c = c0
    c.norm_names = sd.generic_params
    let mut i = 0
    while i < sd.fields.len() {
        c = ck_type_exists(c, sd.fields[i].type_name, sd.line)
        i = i + 1
    }
    return c
}

fn ck_enum_decl(c0: Checker, ed: EnumDecl) -> Checker {
    let mut c = c0
    c.norm_names = ed.generic_params
    let mut i = 0
    while i < ed.variants.len() {
        let v = ed.variants[i]
        let mut j = 0
        while j < v.payload_types.len() {
            c = ck_type_exists(c, v.payload_types[j], ed.line)
            j = j + 1
        }
        i = i + 1
    }
    return c
}

fn ck_const_decl(c0: Checker, cd: ConstDecl) -> Checker {
    let mut c = c0
    let no_generics: List[string] = []
    c.norm_names = no_generics
    let no_repls: List[string] = []
    c.norm_repls = no_repls
    let no_names: List[string] = []
    c.var_names = no_names
    let no_types: List[string] = []
    c.var_types = no_types
    let no_flags: List[int] = []
    c.var_flags = no_flags
    c.record = true
    let value = *cd.value
    let found = ck_expr(c, value)
    c = found.c
    if cd.type_name == "" {
        c.consts.insert(cd.name, found.t)
        return c
    }
    c = ck_type_exists(c, cd.type_name, cd.line)
    let want = c.consts.get(cd.name)
    if ck_fits_value(c, want, found.t, value) == false {
        c = ck_error(c, "type mismatch in the value of '" + cd.name + "': expected " + want + ", found " + found.t, value.line)
    } else {
        c = ck_settle(c, value, want)
    }
    return c
}

fn ck_impl(c0: Checker, ib: ImplBlock) -> Checker {
    let mut c = c0
    -- Inside the block its type parameters stand for any type
    let self_type = ty_apply(ck_generic_type(ib.target_type, ib.generic_params), ib.generic_params, ty_unbound(0))
    let no_binds: List[string] = []
    let errors_before = c.errors.len()
    let mut i = 0
    while i < ib.methods.len() {
        c = ck_function(c, ib.methods[i], self_type, ib.generic_params, no_binds, false)
        i = i + 1
    }
    -- The methods of a `dyn Trait` are written by the compiler (TRAITS
    -- in main_split.dm): what is wrong in one is wrong in an impl of
    -- the trait, and is reported there
    if starts_with(ib.target_type, DYN_PREFIX()) {
        while c.errors.len() > errors_before {
            c.errors.pop()
        }
        return c
    }
    if ib.trait_name != "" { c = ck_impl_of_trait(c, ib) }
    return c
}

-- An impl of a trait is of a trait there is, and has each of the
-- trait's methods, with the parameters and the result the trait gives
-- it (a method the trait has a body for is in the impl already, see
-- TRAITS in main_split.dm)
fn ck_impl_of_trait(c0: Checker, ib: ImplBlock) -> Checker {
    let mut c = c0
    if c.traits.contains(ib.trait_name) == false {
        return ck_error(c, "undefined trait '" + ib.trait_name + "'", ib.line)
    }
    let shown = "the impl of '" + ib.trait_name + "' for '" + ib.target_type + "'"
    let names = string_split(c.traits.get(ib.trait_name), "|")
    let mut i = 0
    while i < names.len() {
        let name = names[i]
        i = i + 1
        if name == "" { continue }
        let mut found = -1
        let mut mi = 0
        while mi < ib.methods.len() {
            if ib.methods[mi].name == name { found = mi }
            mi = mi + 1
        }
        if found < 0 {
            c = ck_error(c, shown + " has no method '" + name + "', which the trait declares without a body", ib.line)
            continue
        }
        let line = ib.methods[found].line
        let want = c.fns.get(ib.trait_name + "_" + name)
        let have = c.fns.get(ib.target_type + "_" + name)
        if want.params.len() != have.params.len() or want.has_self != have.has_self {
            c = ck_error(c, "the method '" + name + "' of " + shown + " does not take the parameters the trait gives it", line)
            continue
        }
        let mut pi = 0
        if want.has_self { pi = 1 }
        while pi < want.params.len() {
            if ty_same(want.params[pi], have.params[pi]) == false {
                c = ck_error(c, "parameter " + int_to_string(pi + 1) + " of the method '" + name + "' of " + shown + " has the type " + have.params[pi] + ", and the trait gives it the type " + want.params[pi], line)
            } else if want.changes[pi] != have.changes[pi] {
                c = ck_error(c, "parameter " + int_to_string(pi + 1) + " of the method '" + name + "' of " + shown + " is 'mut' in one of the trait and the impl and not in the other", line)
            }
            pi = pi + 1
        }
        if want.has_self and want.changes.len() > 0 and have.changes.len() > 0 {
            if want.changes[0] != have.changes[0] {
                c = ck_error(c, "the method '" + name + "' of " + shown + " takes 'mut self' in one of the trait and the impl and 'self' in the other", line)
            }
        }
        if ty_same(want.ret, have.ret) == false {
            c = ck_error(c, "the method '" + name + "' of " + shown + " returns " + have.ret + ", and the trait says it returns " + want.ret, line)
        }
    }
    return c
}

fn ck_trait(c0: Checker, td: TraitDecl) -> Checker {
    let mut c = c0
    let no_binds: List[string] = []
    let mut i = 0
    while i < td.methods.len() {
        -- A method without a body is only a signature
        if td.methods[i].body.len() > 0 {
            c = ck_function(c, td.methods[i], "?", td.generic_params, no_binds, false)
        }
        i = i + 1
    }
    return c
}

-- The errors of the names and types of a program, which is the file
-- being compiled with the files it imports and the runtime; and the
-- type of each of its expressions
fn check_program(sf: SourceFile) -> Checked {
    -- Two lists: a copy of a list shares its storage
    let mut types: List[string] = []
    let mut type_args: List[string] = []
    let mut targets: List[string] = []
    let count = expr_id_count()
    let mut ti = 0
    while ti < count {
        types.push("")
        type_args.push("")
        targets.push("")
        ti = ti + 1
    }
    let mut c = Checker {
        files: sf.files,
        errors: [],
        fns: Map_new(),
        structs: Map_new(),
        enums: Map_new(),
        traits: Map_new(),
        type_traits: Map_new(),
        consts: Map_new(),
        var_names: [],
        var_types: [],
        var_flags: [],
        lambda_floor: 0,
        lambda_ret: "-",
        ret_type: "",
        awaited: -2,
        fn_name: "",
        fn_effects: [],
        norm_names: [],
        norm_repls: [],
        types: types,
        type_args: type_args,
        targets: targets,
        record: true
    }
    c = ck_collect(c, sf)
    -- Constants first: a function may use one declared after it
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_CONST() {
            c = ck_const_decl(c, *decl.const_decl)
        }
        i = i + 1
    }
    let no_generics: List[string] = []
    i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            c = ck_function(c, *decl.func_decl, "", no_generics, no_generics, false)
        } else if decl.kind == DECL_IMPL() {
            c = ck_impl(c, *decl.impl_block)
        } else if decl.kind == DECL_TRAIT() {
            c = ck_trait(c, *decl.trait_decl)
        } else if decl.kind == DECL_STRUCT() {
            c = ck_struct_decl(c, *decl.struct_decl)
        } else if decl.kind == DECL_ENUM() {
            c = ck_enum_decl(c, *decl.enum_decl)
        }
        i = i + 1
    }
    return Checked { errors: c.errors, types: c.types, type_args: c.type_args, targets: c.targets, checker: Box_new(c) }
}

-- ============================================================
-- INSTANCES
-- The body of a generic function, and of a method of a generic struct
-- or enum, is checked above with its type parameters standing for any
-- type, and no types are recorded for its expressions. Code generation
-- makes one copy of the function for each set of type arguments, and
-- before it generates a copy it has the function checked again with
-- the type parameters standing for those arguments
-- (instance_types_begin). The types are recorded at the same
-- expression ids for every instance, so the ones that were there are
-- kept and put back when the instance is generated
-- (instance_types_end): an instance may be generated in the middle of
-- another of the same function.
-- ============================================================

-- Whether every name in the type is one the checker knows
fn ck_knows_type(c: Checker, t: string) -> bool {
    if t == "" or string_contains(t, "?") { return false }
    let n = len(t)
    let mut i = 0
    let mut in_size = false
    while i < n {
        let b = byte_at(t, i)
        if b == 59 { in_size = true }
        if b == 93 { in_size = false }
        if ty_ident_byte(b) == false {
            i = i + 1
            continue
        }
        let start = i
        while i < n and ty_ident_byte(byte_at(t, i)) {
            i = i + 1
        }
        if in_size or (b >= 48 and b <= 57) { continue }
        let word = substr(t, start, i - start)
        if ck_builtin_type_name(word) { continue }
        if c.structs.contains(word) or c.enums.contains(word) or c.traits.contains(word) { continue }
        return false
    }
    return true
}

-- Check one instance of the function: `binds` are what the type
-- parameters stand for (outer_generics, then the function's own), ""
-- for one that is not known. The lists of types grow to hold the
-- expressions made since the program was checked.
fn check_instance(c0: Checker, fd: FunctionDecl, self_type: string, outer_generics: List[string], binds: List[string]) -> Checker {
    let mut c = c0
    let count = expr_id_count()
    while c.types.len() < count {
        c.types.push("")
        c.type_args.push("")
        c.targets.push("")
    }
    let mut id = fd.expr_from
    while id < fd.expr_to and id < count {
        c.types[id] = ""
        c.type_args[id] = ""
        c.targets[id] = ""
        id = id + 1
    }
    return ck_function(c, fd, self_type, outer_generics, binds, true)
}

-- The checked types of a function's expressions as they were before
-- one of its instances was checked
struct InstanceTypes {
    gen: IRGenerator,
    active: bool,
    from: int,
    types: List[string],
    type_args: List[string],
    targets: List[string]
}

-- A type of code generation (canon_type: i64, Pair_i64_string) as the
-- checker writes it (int, Pair[int, string])
fn checker_type(gen: IRGenerator, t: string) -> string {
    if t == "i64" { return "int" }
    if t == "f64" { return "float" }
    if t == "String" { return "string" }
    let mut base = t
    let mut args: List[string] = []
    if gen.generic_instances.contains(t) {
        base = gen.generic_instances.get(t)
        args = string_split(gen.generic_instance_args.get(t), "|")
    } else {
        let bracket = string_find(t, "[")
        if bracket <= 0 or ends_with(t, "]") == false { return t }
        base = substr(t, 0, bracket)
        args = split_type_args(substr(t, bracket + 1, len(t) - bracket - 2))
    }
    let mut out = base + "["
    let mut i = 0
    while i < args.len() {
        if i > 0 { out = out + ", " }
        out = out + checker_type(gen, args[i])
        i = i + 1
    }
    return out + "]"
}

-- Check the instance of `fd` that is about to be generated. `binds`
-- are the type arguments as code generation has them, `self_type` the
-- type the method is of ("" for a function) and `what` names the
-- instance in an error. Give the result to instance_types_end when
-- the instance is generated.
fn instance_types_begin(gen: IRGenerator, fd: FunctionDecl, self_type: string, outer_generics: List[string], binds: List[string], what: string) -> InstanceTypes {
    let mut out = InstanceTypes { gen: gen, active: false, from: fd.expr_from, types: [], type_args: [], targets: [] }
    if gen.has_checker == false or fd.expr_to <= fd.expr_from { return out }
    let mut g = gen
    let mut c = *g.checker
    c.types = g.checked_types
    c.type_args = g.checked_type_args
    c.targets = g.checked_targets
    let no_errors: List[string] = []
    c.errors = no_errors

    let mut id = fd.expr_from
    while id < fd.expr_to {
        if id < g.checked_types.len() {
            out.types.push(g.checked_types[id])
            out.type_args.push(g.checked_type_args[id])
            out.targets.push(g.checked_targets[id])
        } else {
            out.types.push("")
            out.type_args.push("")
            out.targets.push("")
        }
        id = id + 1
    }

    -- A type argument code generation only guessed, or has in a form
    -- the checker does not know, stands for any type
    let mut known: List[string] = []
    let mut bi = 0
    while bi < binds.len() {
        let t = checker_type(g, binds[bi])
        if ck_knows_type(c, t) {
            known.push(t)
        } else {
            known.push("")
        }
        bi = bi + 1
    }
    let mut self_known = ""
    if self_type != "" {
        self_known = checker_type(g, self_type)
        if ck_knows_type(c, self_known) == false { self_known = "?" }
    }
    c = check_instance(c, fd, self_known, outer_generics, known)
    g.checked_types = c.types
    g.checked_type_args = c.type_args
    g.checked_targets = c.targets
    g = ensure_checked_instances(g, fd.expr_from, fd.expr_to)
    let mut ei = 0
    while ei < c.errors.len() {
        let text = c.errors[ei] + " (in " + what + ")"
        let mut have = false
        let mut hi = 0
        while hi < g.type_errors.len() {
            if g.type_errors[hi] == text { have = true }
            hi = hi + 1
        }
        if have == false { g.type_errors.push(text) }
        ei = ei + 1
    }
    out.gen = g
    out.active = true
    return out
}

-- Put back the types that were recorded before the instance was checked
fn instance_types_end(gen: IRGenerator, saved: InstanceTypes) -> IRGenerator {
    if saved.active == false { return gen }
    let mut g = gen
    let mut i = 0
    while i < saved.types.len() {
        let id = saved.from + i
        if id < g.checked_types.len() {
            g.checked_types[id] = saved.types[i]
            g.checked_type_args[id] = saved.type_args[i]
            g.checked_targets[id] = saved.targets[i]
        }
        i = i + 1
    }
    return g
}
