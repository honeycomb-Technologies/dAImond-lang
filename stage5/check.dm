module check

import token
import ast
import ir_gen
import ir_gen_expr

-- ============================================================
-- THE CHECKER
-- Finds the type of every expression of a program and reports
-- where names and types do not fit, before any code is
-- generated: check_program gives the errors of a source file.
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
    has_self: bool,
    is_async: bool
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
    -- What the function or lambda being checked returns: "void" for
    -- nothing, "" when it is not known
    ret_type: string,
    -- Names that stand for other types where the checker is: Self,
    -- and the type parameters in scope, which stand for "?"
    norm_names: List[string],
    norm_repls: List[string]
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
    return b == 95
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
-- "Map[string, int]". "" for function, array and dyn types.
fn ty_head(t: string) -> string {
    if starts_with(t, "fn(") or starts_with(t, "[") or starts_with(t, "dyn ") { return "" }
    let at = string_find(t, "[")
    if at < 0 { return t }
    return substr(t, 0, at)
}

-- The arguments of a type: "string" and "int" for "Map[string, int]"
fn ty_args(t: string) -> List[string] {
    let none: List[string] = []
    if starts_with(t, "fn(") or starts_with(t, "[") or starts_with(t, "dyn ") { return none }
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

-- Whether a value of type `got` can go where `want` is expected.
-- Unknown types fit anything. Integers of every width fit each other
-- (code generation checks the narrowing), and so do the float types.
-- Function, array and dyn types are not compared.
fn ty_fits(want: string, got: string) -> bool {
    if want == got { return true }
    if ty_known(want) == false or ty_known(got) == false { return true }
    if ty_is_int(want) and ty_is_int(got) { return true }
    if ty_is_float(want) and ty_is_float(got) { return true }
    if starts_with(want, "dyn ") or starts_with(got, "dyn ") { return true }
    if starts_with(want, "fn(") or starts_with(got, "fn(") { return true }
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
-- ty_fits, and a struct or an enum that goes where a `dyn Trait` is
-- expected must implement the trait
fn ck_fits(c: Checker, want: string, got: string) -> bool {
    if starts_with(want, "dyn ") and ty_known(got) {
        let head = ty_head(got)
        let mut trait_name = substr(want, 4, len(want) - 4)
        let bracket = string_find(trait_name, "[")
        if bracket >= 0 { trait_name = substr(trait_name, 0, bracket) }
        if head != "" and c.traits.contains(trait_name) and (c.structs.contains(head) or c.enums.contains(head)) {
            if c.type_traits.contains(head) == false { return false }
            return string_contains(c.type_traits.get(head), "|" + trait_name + "|")
        }
    }
    return ty_fits(want, got)
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
    return name == "Self" or name == "dyn" or name == "fn" or name == "Future"
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
        if c.structs.contains(word) or c.enums.contains(word) or c.traits.contains(word) { continue }
        if ck_index_of(c.norm_names, word) >= 0 { continue }
        return ck_error(c, "undefined type '" + word + "'", line)
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
    let mut gi = 0
    while gi < outer_generics.len() {
        generics.push(outer_generics[gi])
        gi = gi + 1
    }
    gi = 0
    while gi < fd.generic_params.len() {
        generics.push(fd.generic_params[gi])
        gi = gi + 1
    }
    let mut params: List[string] = []
    let mut i = 0
    while i < fd.params.len() {
        params.push(ty_rewrite(fd.params[i].type_name, names, repls))
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
        has_self: has_self,
        is_async: fd.is_async
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

fn ck_expr(c: Checker, e: Expr) -> Typed {
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
    if k == EXPR_PIPELINE() { return ck_pipeline(c, e) }
    if k == EXPR_ERROR_PROPAGATE() { return ck_propagate(c, e) }
    if k == EXPR_GROUPED() or k == EXPR_COMPTIME() { return ck_expr(c, *e.operand) }
    if k == EXPR_CAST() { return ck_cast(c, e) }
    if k == EXPR_STRING_INTERP() {
        let parts = ck_exprs(c, e.interp_parts)
        return ck_typed(parts.c, "string")
    }
    if k == EXPR_RANGE() {
        let low = ck_expr(c, *e.left)
        let high = ck_expr(low.c, *e.right)
        return ck_typed(high.c, "")
    }
    if k == EXPR_AWAIT() {
        let awaited = ck_expr(c, *e.operand)
        return ck_typed(awaited.c, "")
    }
    return ck_typed(c, "")
}

-- The types of the expressions, in order
fn ck_exprs(c0: Checker, exprs: List[Expr]) -> TypedList {
    let mut c = c0
    let mut types: List[string] = []
    let mut i = 0
    while i < exprs.len() {
        let r = ck_expr(c, exprs[i])
        c = r.c
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
    -- A function's name is a value: its address
    if c.fns.contains(name) or is_builtin_fn(name) { return ck_typed(c, "") }
    if ck_names_type(c, name) { return ck_typed(c, "") }
    return ck_typed(ck_error(c, "undefined variable '" + name + "'", e.line), "")
}

fn ck_cast(c0: Checker, e: Expr) -> Typed {
    let operand = ck_expr(c0, *e.operand)
    let c = ck_type_exists(operand.c, e.cast_type, e.line)
    return ck_typed(c, ck_norm(c, e.cast_type))
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
        return ck_typed(c, ty_apply(sig.ret, sig.generics, ty_unbound(0)))
    }
    let lk = ty_kind(left)
    let rk = ty_kind(right)
    if ty_is_comparison(op) {
        if lk != "" and rk != "" and lk != rk {
            c = ck_error(c, "cannot compare " + left + " with " + right + " ('" + text + "')", line)
        }
        return ck_typed(c, "bool")
    }
    if lk != "" and rk != "" {
        let bits = op == BINOP_BIT_AND() or op == BINOP_BIT_OR() or op == BINOP_BIT_XOR()
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
    if ty_known(left) and left == right { return ck_typed(c, left) }
    return ck_typed(c, "")
}

-- a op b op c ... nests one level per operator on the left, so the
-- chain is walked in a loop: a long one would overflow the stack
fn ck_binary(c0: Checker, e: Expr) -> Typed {
    let mut c = c0
    let mut ops: List[int] = []
    let mut lines: List[int] = []
    let mut rights: List[Expr] = []
    let mut cur = e
    while cur.kind == EXPR_BINARY() {
        ops.push(cur.op)
        lines.push(cur.line)
        rights.push(*cur.right)
        cur = *cur.left
    }
    let first = ck_expr(c, cur)
    c = first.c
    let mut t = first.t
    let mut i = ops.len() - 1
    while i >= 0 {
        let right = ck_expr(c, rights[i])
        let result = ck_operator(right.c, ops[i], t, right.t, lines[i])
        c = result.c
        t = result.t
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
        return ck_typed(operand.c, "")
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
    return ck_error(c, "call to '" + shown + "' passes " + int_to_string(count) + " arguments, expected " + int_to_string(sig.params.len() - first), line)
}

-- Check the arguments of a call against the signature, starting at
-- its parameter `first`, and give what the call returns. `binds` is
-- what the signature's type parameters are known to stand for.
fn ck_apply(c0: Checker, sig: FnSig, shown: string, first: int, binds0: List[string], arg_types: List[string], args: List[Expr]) -> Typed {
    let mut c = c0
    let mut binds = binds0
    let mut i = 0
    while i < arg_types.len() and first + i < sig.params.len() {
        binds = ty_bind(sig.params[first + i], arg_types[i], sig.generics, binds)
        i = i + 1
    }
    i = 0
    while i < arg_types.len() and first + i < sig.params.len() {
        let want = ty_apply(sig.params[first + i], sig.generics, binds)
        if ck_fits(c, want, arg_types[i]) == false {
            c = ck_error(c, "type mismatch in argument " + int_to_string(i + 1) + " of '" + shown + "': expected " + want + ", found " + arg_types[i], args[i].line)
        }
        i = i + 1
    }
    if sig.is_async { return ck_typed(c, "") }
    return ck_typed(c, ty_apply(sig.ret, sig.generics, binds))
}

-- A call of a value whose type is a function's, such as a variable
-- that holds a lambda
fn ck_call_value(c0: Checker, held: string, shown: string, arg_types: List[string], args: List[Expr], line: int) -> Typed {
    let mut c = c0
    if starts_with(held, "fn(") == false { return ck_typed(c, "") }
    let params = ty_fn_params(held)
    if params.len() != arg_types.len() {
        c = ck_error(c, "call to '" + shown + "' passes " + int_to_string(arg_types.len()) + " arguments, expected " + int_to_string(params.len()), line)
        return ck_typed(c, ty_fn_ret(held))
    }
    let mut i = 0
    while i < params.len() {
        if ck_fits(c, params[i], arg_types[i]) == false {
            c = ck_error(c, "type mismatch in argument " + int_to_string(i + 1) + " of '" + shown + "': expected " + params[i] + ", found " + arg_types[i], args[i].line)
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
fn ck_call_named(c0: Checker, name: string, generic_args: List[string], arg_types: List[string], args: List[Expr], line: int) -> Typed {
    let c = c0
    let mut first_type = "?"
    if arg_types.len() > 0 {
        if ty_known(arg_types[0]) { first_type = arg_types[0] }
    }
    if name == "Some" { return ck_typed(c, "Option[" + first_type + "]") }
    if name == "Ok" { return ck_typed(c, "Result[" + first_type + ", ?]") }
    if name == "Err" { return ck_typed(c, "Result[?, " + first_type + "]") }
    if name == "Box_new" { return ck_typed(c, "Box[" + first_type + "]") }
    if name == "Box_null" { return ck_typed(c, "Box[?]") }
    if name == "Map_new" { return ck_typed(c, "Map[?, ?]") }
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
        return ck_apply(c, sig, name, 0, binds, arg_types, args)
    }
    if builtin { return ck_typed(c, ck_builtin_type(name)) }
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
    return ck_call_named(found.c, callee.name, callee.generic_args, found.types, e.args, e.line)
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
fn ck_variant_values(c0: Checker, enum_name: string, variant: string, types: List[string], values: List[Expr], line: int) -> Checker {
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
        let want = ty_apply(payloads[i], sig.generics, ty_unbound(0))
        if ck_fits(c, want, types[i]) == false {
            c = ck_error(c, "type mismatch in value " + int_to_string(i + 1) + " of '" + enum_name + "." + variant + "': expected " + want + ", found " + types[i], values[i].line)
        }
        i = i + 1
    }
    return c
}

-- Type.name(args): a constructor of Box, Map or List, a variant of
-- an enum, or a function of the type's impl block
fn ck_static_call(c: Checker, type_name: string, e: Expr, arg_types: List[string]) -> Typed {
    let method = e.method
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
            return ck_typed(ck_variant_values(c, type_name, method, arg_types, e.args, e.line), type_name)
        }
        if c.fns.contains(type_name + "_" + method) == false and ck_trait_method(c, type_name, method) == "" {
            return ck_typed(ck_error(c, "enum '" + type_name + "' has no variant or function '" + method + "'", e.line), type_name)
        }
    }
    let key = type_name + "_" + method
    if c.fns.contains(key) {
        let sig = c.fns.get(key)
        let counted = ck_arity(c, sig, type_name + "." + method, 0, arg_types.len(), e.line)
        return ck_apply(counted, sig, type_name + "." + method, 0, ty_unbound(sig.generics.len()), arg_types, e.args)
    }
    return ck_typed(c, "")
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

    if starts_with(recv, "dyn ") {
        let key = substr(recv, 4, len(recv) - 4) + "_" + method
        if c.fns.contains(key) {
            let sig = c.fns.get(key)
            return ck_typed(c, ty_apply(sig.ret, sig.generics, ty_unbound(0)))
        }
        return ck_typed(c, "")
    }
    let head = ty_head(recv)
    if ty_known(head) == false { return ck_typed(c, "") }
    if c.fns.contains(head + "_" + method) {
        let sig = c.fns.get(head + "_" + method)
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
        return ck_apply(c, sig, head + "." + method, first, binds, arg_types, e.args)
    }
    if head == "List" {
        let elem = ty_arg(recv, 0)
        if method == "push" {
            if arg_types.len() == 1 {
                if ck_fits(c, elem, arg_types[0]) == false {
                    c = ck_error(c, "type mismatch in the value pushed to a " + recv + ": expected " + elem + ", found " + arg_types[0], e.line)
                }
            }
            return ck_typed(c, "void")
        }
        if method == "pop" or method == "get" { return ck_typed(c, elem) }
        if method == "len" { return ck_typed(c, "int") }
        if method == "contains" { return ck_typed(c, "bool") }
        return ck_typed(c, "")
    }
    if recv == "string" {
        if method == "len" { return ck_typed(c, "int") }
        return ck_typed(c, "")
    }
    if c.structs.contains(head) or c.enums.contains(head) {
        -- A field that holds a function is called like a method
        if c.structs.contains(head) {
            let sig = c.structs.get(head)
            let at = ck_index_of(sig.field_names, method)
            if at >= 0 { return ck_typed(c, ty_fn_ret(ty_apply(sig.field_types[at], sig.generics, ty_args(recv)))) }
        }
        let from_trait = ck_trait_method(c, head, method)
        if from_trait != "" {
            let sig = c.fns.get(from_trait + "_" + method)
            return ck_typed(c, ty_apply(sig.ret, sig.generics, ty_unbound(0)))
        }
        return ck_typed(ck_error(c, "type '" + head + "' has no method '" + method + "'", e.line), "")
    }
    return ck_typed(c, "")
}

fn ck_struct_literal(c0: Checker, e: Expr) -> Typed {
    let mut c = ck_type_exists(c0, e.type_name, e.line)
    let t = ck_norm(c, e.type_name)
    let head = ty_head(t)
    let values = ck_exprs(c, e.field_values)
    c = values.c
    if head == "" or c.structs.contains(head) == false { return ck_typed(c, "") }
    let sig = c.structs.get(head)
    let mut i = 0
    while i < e.field_names.len() and i < values.types.len() {
        let field = e.field_names[i]
        if ck_index_of(sig.field_names, field) < 0 {
            c = ck_error(c, "struct '" + head + "' has no field '" + field + "'", e.field_values[i].line)
        } else {
            let want = ck_field_type(c, t, field)
            if ck_fits(c, want, values.types[i]) == false {
                c = ck_error(c, "type mismatch in field '" + field + "' of '" + head + "': expected " + want + ", found " + values.types[i], e.field_values[i].line)
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
    let c = values.c
    let name = e.enum_name
    if ck_var(c, name) >= 0 { return ck_typed(c, "") }
    if c.enums.contains(name) {
        let sig = c.enums.get(name)
        if ck_index_of(sig.variant_names, e.variant_name) >= 0 {
            return ck_typed(ck_variant_values(c, name, e.variant_name, values.types, e.payload, e.line), name)
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
        return ck_apply(counted, sig, name + "." + e.variant_name, 0, ty_unbound(sig.generics.len()), values.types, e.payload)
    }
    return ck_typed(c, "")
}

fn ck_array_literal(c0: Checker, e: Expr) -> Typed {
    let values = ck_exprs(c0, e.elements)
    let mut elem = "?"
    let mut i = 0
    while i < values.types.len() and elem == "?" {
        if ty_known(values.types[i]) and values.types[i] != "void" { elem = values.types[i] }
        i = i + 1
    }
    return ck_typed(values.c, "List[" + elem + "]")
}

fn ck_condition(c: Checker, t: string, what: string, line: int) -> Checker {
    if ty_known(t) and t != "bool" {
        return ck_error(c, "the condition of " + what + " is " + t + ", not bool", line)
    }
    return c
}

-- The statements, and the value of the last when it is an expression
fn ck_body_value(c0: Checker, stmts: List[Stmt]) -> Typed {
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
    return ck_typed(ck_stmt(c, last), "void")
}

fn ck_block(c0: Checker, e: Expr) -> Typed {
    let mark = c0.var_names.len()
    if e.has_result {
        let c = ck_stmts(c0, e.stmts)
        let result = ck_expr(c, *e.result)
        return ck_typed(ck_unbind(result.c, mark), result.t)
    }
    let value = ck_body_value(c0, e.stmts)
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
        }
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
    if p.kind == PAT_STRUCT() {
        let mut i = 0
        while i < p.field_patterns.len() {
            let fp = p.field_patterns[i]
            let mut name = fp.binding
            if name == "" { name = fp.name }
            c = ck_bind(c, name, "", 1)
            i = i + 1
        }
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
        if arm.has_guard == false and (kind == PAT_WILDCARD() or kind == PAT_IDENTIFIER() or kind == PAT_STRUCT()) { return "" }
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

-- A match. `used` says that its value goes somewhere.
fn ck_match(c0: Checker, e: Expr, used: bool) -> Typed {
    let scrutinee = ck_expr(c0, *e.scrutinee)
    let mut c = scrutinee.c
    let mut t = ""
    let scrutinee_kind = ty_kind(scrutinee.t)
    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        let mark = c.var_names.len()
        let pattern_kind = ck_literal_kind(arm.pattern)
        if pattern_kind != "" and scrutinee_kind != "" and pattern_kind != scrutinee_kind {
            let lit = *arm.pattern.literal_expr
            c = ck_error(c, "a pattern is a " + pattern_kind + ", but the matched value is " + scrutinee.t, lit.line)
        }
        c = ck_variant_pattern(c, arm.pattern, scrutinee.t, e.line)
        c = ck_bind_pattern(c, arm.pattern, scrutinee.t)
        if arm.has_guard {
            let guard_expr = *arm.guard
            let guard = ck_expr(c, guard_expr)
            c = ck_condition(guard.c, guard.t, "a match guard", guard_expr.line)
            -- Code generation tests a variant's arm by the variant alone
            -- (generate_enum_match, generate_result_match and
            -- generate_option_match in ir_gen_expr.dm), so the guard
            -- would be left out without a word
            if arm.pattern.kind == PAT_ENUM_VARIANT() {
                c = ck_error(c, "a guard on the arm of the variant '" + arm.pattern.variant + "' is not supported: test the condition inside the arm", guard_expr.line)
            }
        }
        let mut arm_type = ""
        if arm.is_expr_body {
            let value = ck_expr(c, *arm.body_expr)
            c = value.c
            arm_type = value.t
        } else {
            let value = ck_body_value(c, arm.body)
            c = value.c
            arm_type = value.t
        }
        c = ck_unbind(c, mark)
        if t == "" and ty_known(arm_type) and arm_type != "void" { t = arm_type }
        if used and t != "" and arm_type != "void" and ck_fits(c, t, arm_type) == false and ck_fits(c, arm_type, t) == false {
            c = ck_error(c, "the arms of this match give " + t + " and " + arm_type, e.line)
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
    let body = *e.lambda_body
    let value = ck_expr(c, body)
    c = value.c
    if body.kind != EXPR_BLOCK() {
        if ret == "" {
            if ty_known(value.t) and value.t != "void" { ret = value.t }
        } else if value.t != "void" and ck_fits(c, ret, value.t) == false {
            c = ck_error(c, "type mismatch in the lambda's value: expected " + ret + ", found " + value.t, body.line)
        }
    }
    c.ret_type = outer_ret
    c = ck_unbind(c, mark)
    if ret == "" { ret = "?" }
    return ck_typed(c, text + ") -> " + ret)
}

-- left |> f(args) is f(left, args)
fn ck_pipeline(c0: Checker, e: Expr) -> Typed {
    let left = ck_expr(c0, *e.left)
    let left_expr = *e.left
    let right = *e.pipe_right
    let mut types: List[string] = []
    types.push(left.t)
    let mut args: List[Expr] = []
    args.push(left_expr)
    let no_generics: List[string] = []
    if right.kind == EXPR_IDENTIFIER() {
        return ck_call_named(left.c, right.name, no_generics, types, args, e.line)
    }
    if right.kind == EXPR_FUNCTION_CALL() {
        let callee = *right.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let rest = ck_exprs(left.c, right.args)
            let mut i = 0
            while i < right.args.len() {
                types.push(rest.types[i])
                args.push(right.args[i])
                i = i + 1
            }
            return ck_call_named(rest.c, callee.name, callee.generic_args, types, args, e.line)
        }
    }
    let other = ck_expr(left.c, right)
    return ck_typed(other.c, "")
}

-- value? is the value of an Ok or a Some
fn ck_propagate(c0: Checker, e: Expr) -> Typed {
    let operand = ck_expr(c0, *e.operand)
    let head = ty_head(operand.t)
    if head == "Result" or head == "Option" { return ck_typed(operand.c, ty_arg(operand.t, 0)) }
    return ck_typed(operand.c, "")
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
    let mut c = found.c
    let mut t = found.t
    if t == "void" {
        c = ck_error(c, "a variable holds the result of a call that returns no value", value.line)
        t = ""
    }
    if s.let_type != "" {
        c = ck_type_exists(c, s.let_type, s.line)
        let want = ck_norm(c, s.let_type)
        if ck_fits(c, want, t) == false {
            c = ck_error(c, "type mismatch in the value of '" + s.let_name + "': expected " + want + ", found " + t, value.line)
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
        if ty_known(want) and want != "void" {
            return ck_error(c0, "this return has no value, but the function returns " + want, s.line)
        }
        return c0
    }
    let value = *s.ret_value
    let found = ck_expr(c0, value)
    let c = found.c
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
    if ck_fits(c, want, found.t) == false {
        return ck_error(c, "type mismatch in the return value: expected " + want + ", found " + found.t, value.line)
    }
    return c
}

fn ck_assign_op(op: int) -> int {
    if op == ASSIGN_ADD() { return BINOP_ADD() }
    if op == ASSIGN_SUB() { return BINOP_SUB() }
    if op == ASSIGN_MUL() { return BINOP_MUL() }
    if op == ASSIGN_DIV() { return BINOP_DIV() }
    return BINOP_MOD()
}

-- The variable a place is a part of: p for p.x and for p.xs[i].y,
-- and "" for a place that is a part of none
fn ck_place_root(e: Expr) -> string {
    let mut cur = e
    while cur.kind == EXPR_FIELD_ACCESS() or cur.kind == EXPR_INDEX_ACCESS() {
        cur = *cur.object
    }
    if cur.kind == EXPR_IDENTIFIER() { return cur.name }
    return ""
}

fn ck_assignment(c0: Checker, s: Stmt) -> Checker {
    let mut c = c0
    let target = *s.assign_target
    let mut shown = ""
    if target.kind == EXPR_FIELD_ACCESS() or target.kind == EXPR_INDEX_ACCESS() {
        let root = ck_place_root(target)
        let root_at = ck_var(c, root)
        if root_at >= 0 {
            if c.var_flags[root_at] == 0 {
                c = ck_error(c, "cannot assign to a part of '" + root + "', which is not declared 'mut'", s.line)
            }
        }
    }
    if target.kind == EXPR_IDENTIFIER() {
        shown = " to '" + target.name + "'"
        let at = ck_var(c, target.name)
        if at >= 0 {
            if c.var_flags[at] == 0 {
                c = ck_error(c, "cannot assign to '" + target.name + "', which is not declared 'mut'", s.line)
            } else if starts_with(c.var_types[at], "dyn ") {
                -- Which type a dyn variable holds is settled where it is
                -- declared (see the dyn Trait annotation in ir_gen_stmt.dm):
                -- its methods would still be those of its first value
                c = ck_error(c, "cannot assign to '" + target.name + "': a '" + c.var_types[at] + "' variable keeps the value it is declared with", s.line)
            }
        } else if c.consts.contains(target.name) {
            c = ck_error(c, "cannot assign to the constant '" + target.name + "'", s.line)
        }
    }
    let to = ck_expr(c, target)
    let value = ck_expr(to.c, *s.assign_value)
    c = value.c
    if value.t == "void" {
        return ck_error(c, "the assignment" + shown + " takes the result of a call that returns no value", s.line)
    }
    if s.assign_op == ASSIGN_EQ() {
        if ck_fits(c, to.t, value.t) == false {
            c = ck_error(c, "type mismatch in the assignment" + shown + ": expected " + to.t + ", found " + value.t, s.line)
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
        let value = ck_expr(c0, *s.expr)
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
fn ck_function(c0: Checker, fd: FunctionDecl, self_type: string, outer_generics: List[string]) -> Checker {
    let mut c = c0
    let mut names: List[string] = []
    names.push("Self")
    let mut repls: List[string] = []
    repls.push(self_type)
    let mut gi = 0
    while gi < outer_generics.len() {
        names.push(outer_generics[gi])
        repls.push("?")
        gi = gi + 1
    }
    gi = 0
    while gi < fd.generic_params.len() {
        names.push(fd.generic_params[gi])
        repls.push("?")
        gi = gi + 1
    }
    c.norm_names = names
    c.norm_repls = repls
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
    if fd.is_extern { return c }

    let mut ret = ck_norm(c, fd.return_type)
    if fd.is_async { ret = "" }
    c.ret_type = ret
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
    let value = *cd.value
    let found = ck_expr(c, value)
    c = found.c
    if cd.type_name == "" {
        c.consts.insert(cd.name, found.t)
        return c
    }
    c = ck_type_exists(c, cd.type_name, cd.line)
    let want = c.consts.get(cd.name)
    if ck_fits(c, want, found.t) == false {
        c = ck_error(c, "type mismatch in the value of '" + cd.name + "': expected " + want + ", found " + found.t, value.line)
    }
    return c
}

fn ck_impl(c0: Checker, ib: ImplBlock) -> Checker {
    let mut c = c0
    -- Inside the block its type parameters stand for any type
    let self_type = ty_apply(ck_generic_type(ib.target_type, ib.generic_params), ib.generic_params, ty_unbound(0))
    let mut i = 0
    while i < ib.methods.len() {
        c = ck_function(c, ib.methods[i], self_type, ib.generic_params)
        i = i + 1
    }
    return c
}

fn ck_trait(c0: Checker, td: TraitDecl) -> Checker {
    let mut c = c0
    let mut i = 0
    while i < td.methods.len() {
        -- A method without a body is only a signature
        if td.methods[i].body.len() > 0 {
            c = ck_function(c, td.methods[i], "?", td.generic_params)
        }
        i = i + 1
    }
    return c
}

-- The errors of the names and types of a program: the file being
-- compiled with the files it imports and the runtime
fn check_program(sf: SourceFile) -> List[string] {
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
        ret_type: "",
        norm_names: [],
        norm_repls: []
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
            c = ck_function(c, *decl.func_decl, "", no_generics)
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
    return c.errors
}
