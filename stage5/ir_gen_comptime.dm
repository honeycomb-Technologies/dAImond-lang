module ir_gen_comptime

import ast
import ir
import ir_builder
import ir_gen

-- ============================================================
-- COMPILE-TIME EVALUATION
-- The value of a `comptime` expression and of a `const` is found
-- here, by walking the expression: eval_comptime_expr gives it as an
-- IRValue, a constant of the program.
--
-- A value is an integer, a float, a bool, a string or a list of
-- values (CtValue). Variables are kept in CtState, innermost last;
-- a function that is called sees its own variables only (`base`).
-- The evaluator takes numbers, strings, bools and lists; operators;
-- `if`, `match` on literals and names, blocks; `let`, assignment,
-- `while`, `loop`, `for` over a range or a list, `break`, `continue`,
-- `return`; calls of the program's functions and of the builtins in
-- ct_builtin; f-strings; casts between numbers. What it does not
-- take is a compile error that says what it was, as is what would be
-- a panic in the program: an overflow, a division by zero, an index
-- out of range. A computation that takes more than CT_MAX_STEPS() steps
-- or calls nested more than CT_MAX_DEPTH() deep is an error too.
-- Until 2026-11-22 what the evaluator did not take was the value 0, a
-- loop stopped after 10,000 turns without a word, and a function
-- that was called saw its caller's variables.
-- ============================================================

fn CT_MAX_STEPS() -> int { return 1000000 }
fn CT_MAX_DEPTH() -> int { return 200 }

fn CT_NONE() -> int { return 0 }
fn CT_INT() -> int { return 1 }
fn CT_FLOAT() -> int { return 2 }
fn CT_BOOL() -> int { return 3 }
fn CT_STRING() -> int { return 4 }
fn CT_LIST() -> int { return 5 }

-- A value at compile time. CT_NONE is no value: what a statement gives.
struct CtValue {
    kind: int,
    int_val: int,
    float_val: float,
    bool_val: bool,
    str_val: string,
    items: List[CtValue]
}

fn ct_none() -> CtValue {
    return CtValue { kind: CT_NONE(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", items: [] }
}

fn ct_int(v: int) -> CtValue {
    return CtValue { kind: CT_INT(), int_val: v, float_val: 0.0, bool_val: false, str_val: "", items: [] }
}

fn ct_float(v: float) -> CtValue {
    return CtValue { kind: CT_FLOAT(), int_val: 0, float_val: v, bool_val: false, str_val: "", items: [] }
}

fn ct_bool(v: bool) -> CtValue {
    return CtValue { kind: CT_BOOL(), int_val: 0, float_val: 0.0, bool_val: v, str_val: "", items: [] }
}

fn ct_string(v: string) -> CtValue {
    return CtValue { kind: CT_STRING(), int_val: 0, float_val: 0.0, bool_val: false, str_val: v, items: [] }
}

fn ct_list(items: List[CtValue]) -> CtValue {
    return CtValue { kind: CT_LIST(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", items: items }
}

-- "an integer", "a list": the kind of a value, for a message
fn ct_kind_text(v: CtValue) -> string {
    if v.kind == CT_INT() { return "an integer" }
    if v.kind == CT_FLOAT() { return "a float" }
    if v.kind == CT_BOOL() { return "a bool" }
    if v.kind == CT_STRING() { return "a string" }
    if v.kind == CT_LIST() { return "a list" }
    return "no value"
}

-- What the evaluator knows and how far it is
struct CtState {
    files: List[string],
    decls: List[Declaration],
    globals: List[IRGlobal],
    -- The variables, innermost last
    names: List[string],
    values: List[CtValue],
    -- Where the variables of the function being evaluated begin
    base: int,
    steps: int,
    depth: int
}

-- How a statement ends: CT_NEXT goes on to the next one
fn CT_NEXT() -> int { return 0 }
fn CT_RETURN() -> int { return 1 }
fn CT_BREAK() -> int { return 2 }
fn CT_CONTINUE() -> int { return 3 }

struct CtOut {
    st: CtState,
    v: CtValue,
    flow: int
}

fn ct_out(st: CtState, v: CtValue) -> CtOut {
    return CtOut { st: st, v: v, flow: CT_NEXT() }
}

-- Report what cannot be evaluated and stop the compilation
fn ct_error(st: CtState, msg: string, line: int) {
    eprintln(error_at(st.files, msg + " in a compile-time expression", line))
    exit(1)
}

-- One more step taken
fn ct_step(st: CtState, line: int) -> CtState {
    let mut s = st
    s.steps = s.steps + 1
    if s.steps > CT_MAX_STEPS() {
        ct_error(s, "more than " + int_to_string(CT_MAX_STEPS()) + " steps (does it end?)", line)
    }
    return s
}

-- ============================================================
-- VARIABLES
-- ============================================================

fn ct_bind(st: CtState, name: string, v: CtValue) -> CtState {
    let mut s = st
    s.names.push(name)
    s.values.push(v)
    return s
}

-- Forget the variables declared since there were `mark` of them
fn ct_leave(st: CtState, mark: int) -> CtState {
    let mut s = st
    while s.names.len() > mark {
        s.names.pop()
        s.values.pop()
    }
    return s
}

-- The place of the variable among the function's own, or -1
fn ct_find(st: CtState, name: string) -> int {
    let mut i = st.names.len() - 1
    while i >= st.base {
        if st.names[i] == name { return i }
        i = i - 1
    }
    return -1
}

fn ct_from_ir(val: IRValue) -> CtValue {
    if val.kind == VAL_CONST_INT() { return ct_int(val.int_val) }
    if val.kind == VAL_CONST_FLOAT() { return ct_float(val.float_val) }
    if val.kind == VAL_CONST_BOOL() { return ct_bool(val.bool_val) }
    if val.kind == VAL_CONST_STRING() { return ct_string(val.str_val) }
    return ct_none()
}

-- The value of a name: a variable, or a constant of the program,
-- also one that is declared further down
fn ct_name(st: CtState, name: string, line: int) -> CtOut {
    let at = ct_find(st, name)
    if at >= 0 {
        let v = st.values[at]
        if v.kind == CT_NONE() { ct_error(st, "'" + source_name(name) + "' has no value", line) }
        return ct_out(st, v)
    }
    let mut gi = st.globals.len() - 1
    while gi >= 0 {
        let gl = st.globals[gi]
        if gl.name == name and gl.has_init and gl.is_const {
            let v = ct_from_ir(gl.init_value)
            if v.kind != CT_NONE() { return ct_out(st, v) }
        }
        gi = gi - 1
    }
    let mut di = 0
    while di < st.decls.len() {
        let decl = st.decls[di]
        if decl.kind == DECL_CONST() {
            let cd = *decl.const_decl
            if cd.name == name {
                -- Its value is evaluated with no variables in sight
                let mut inner = st
                inner.base = inner.names.len()
                inner.depth = inner.depth + 1
                if inner.depth > CT_MAX_DEPTH() {
                    ct_error(st, "the constant '" + name + "' is defined by itself", line)
                }
                let found = ct_expr(inner, *cd.value)
                let mut back = found.st
                back.base = st.base
                back.depth = st.depth
                return ct_out(back, found.v)
            }
        }
        di = di + 1
    }
    ct_error(st, "'" + source_name(name) + "' is not known at compile time: it is not a constant, nor a variable of the expression", line)
    return ct_out(st, ct_none())
}

-- ============================================================
-- OPERATORS
-- ============================================================

fn CT_INT_MAX() -> int { return 9223372036854775807 }
fn CT_INT_MIN() -> int { return 0 - 9223372036854775807 - 1 }

fn ct_add_overflows(a: int, b: int) -> bool {
    if b > 0 { return a > CT_INT_MAX() - b }
    if b < 0 { return a < CT_INT_MIN() - b }
    return false
}

fn ct_sub_overflows(a: int, b: int) -> bool {
    if b > 0 { return a < CT_INT_MIN() + b }
    if b < 0 { return a > CT_INT_MAX() + b }
    return false
}

fn ct_mul_overflows(a: int, b: int) -> bool {
    if a == 0 or b == 0 { return false }
    if a == 0 - 1 { return b == CT_INT_MIN() }
    if b == 0 - 1 { return a == CT_INT_MIN() }
    if a > 0 {
        if b > 0 { return a > CT_INT_MAX() / b }
        return b < CT_INT_MIN() / a
    }
    if b > 0 { return a < CT_INT_MIN() / b }
    return a < CT_INT_MAX() / b
}

fn is_bit_binop(op: int) -> bool {
    return op == BINOP_BIT_AND() or op == BINOP_BIT_OR() or op == BINOP_BIT_XOR() or op == BINOP_SHL() or op == BINOP_SHR()
}

-- The 64 bits of an integer, the lowest first, each 0 or 1. The
-- compiler's own source is compiled by the seed's compiler, which may
-- have no bitwise operators, so the bits are found by arithmetic.
fn comptime_bits(v: int) -> List[int] {
    let mut bits: List[int] = []
    let mut x = v
    let mut i = 0
    while i < 64 {
        -- x halved and rounded down, also where x is negative
        let mut half = x / 2
        if x < 0 and x % 2 != 0 { half = (x - 1) / 2 }
        bits.push(x - half - half)
        x = half
        i = i + 1
    }
    return bits
}

-- The integer with the 64 bits, the lowest first
fn comptime_from_bits(bits: List[int]) -> int {
    let mut v = 0
    let mut weight = 1
    let mut i = 0
    while i < 63 {
        if bits[i] == 1 { v = v + weight }
        if i < 62 { weight = weight * 2 }
        i = i + 1
    }
    -- The highest bit is the sign: it weighs -2^63
    if bits[63] == 1 { v = v + (0 - 9223372036854775807 - 1) }
    return v
}

-- a & b, a | b, a ^ b, a << b or a >> b of two 64-bit integers, as the
-- program computes them. A shift by 64 or more, or by a negative
-- amount, is an error here as it is a panic there.
fn comptime_bit_op(files: List[string], op: int, a: int, b: int, line: int) -> int {
    let x = comptime_bits(a)
    let mut out: List[int] = []
    let mut i = 0
    if op == BINOP_SHL() or op == BINOP_SHR() {
        if b < 0 or b > 63 {
            eprintln(error_at(files, "shift amount " + int_to_string(b) + " is out of range in a compile-time expression", line))
            exit(1)
        }
        while i < 64 {
            let mut bit = 0
            if op == BINOP_SHL() {
                if i >= b { bit = x[i - b] }
            } else if i + b < 64 {
                bit = x[i + b]
            } else {
                -- The sign fills the top
                bit = x[63]
            }
            out.push(bit)
            i = i + 1
        }
        return comptime_from_bits(out)
    }
    let y = comptime_bits(b)
    while i < 64 {
        let mut bit = 0
        if op == BINOP_BIT_AND() {
            if x[i] == 1 and y[i] == 1 { bit = 1 }
        } else if op == BINOP_BIT_OR() {
            if x[i] == 1 or y[i] == 1 { bit = 1 }
        } else if x[i] != y[i] {
            bit = 1
        }
        out.push(bit)
        i = i + 1
    }
    return comptime_from_bits(out)
}


fn ct_int_op(st: CtState, op: int, a: int, b: int, line: int) -> CtValue {
    if op == BINOP_ADD() {
        if ct_add_overflows(a, b) { ct_error(st, "integer overflow", line) }
        return ct_int(a + b)
    }
    if op == BINOP_SUB() {
        if ct_sub_overflows(a, b) { ct_error(st, "integer overflow", line) }
        return ct_int(a - b)
    }
    if op == BINOP_MUL() {
        if ct_mul_overflows(a, b) { ct_error(st, "integer overflow", line) }
        return ct_int(a * b)
    }
    if op == BINOP_DIV() or op == BINOP_MOD() {
        -- An error here as it is a panic in the program
        if b == 0 { ct_error(st, "division by zero", line) }
        if b == 0 - 1 {
            if op == BINOP_MOD() { return ct_int(0) }
            if a == CT_INT_MIN() { ct_error(st, "integer overflow", line) }
            return ct_int(0 - a)
        }
        if op == BINOP_DIV() { return ct_int(a / b) }
        return ct_int(a % b)
    }
    if is_bit_binop(op) { return ct_int(comptime_bit_op(st.files, op, a, b, line)) }
    if op == BINOP_EQ() { return ct_bool(a == b) }
    if op == BINOP_NE() { return ct_bool(a != b) }
    if op == BINOP_LT() { return ct_bool(a < b) }
    if op == BINOP_LE() { return ct_bool(a <= b) }
    if op == BINOP_GT() { return ct_bool(a > b) }
    if op == BINOP_GE() { return ct_bool(a >= b) }
    return ct_none()
}

fn ct_float_op(op: int, a: float, b: float) -> CtValue {
    if op == BINOP_ADD() { return ct_float(a + b) }
    if op == BINOP_SUB() { return ct_float(a - b) }
    if op == BINOP_MUL() { return ct_float(a * b) }
    if op == BINOP_DIV() { return ct_float(a / b) }
    if op == BINOP_EQ() { return ct_bool(a == b) }
    if op == BINOP_NE() { return ct_bool(a != b) }
    if op == BINOP_LT() { return ct_bool(a < b) }
    if op == BINOP_LE() { return ct_bool(a <= b) }
    if op == BINOP_GT() { return ct_bool(a > b) }
    if op == BINOP_GE() { return ct_bool(a >= b) }
    return ct_none()
}

-- Whether two values are equal: of one kind, and lists element by element
fn ct_equal(a: CtValue, b: CtValue) -> bool {
    if a.kind != b.kind { return false }
    if a.kind == CT_INT() { return a.int_val == b.int_val }
    if a.kind == CT_FLOAT() { return a.float_val == b.float_val }
    if a.kind == CT_BOOL() { return a.bool_val == b.bool_val }
    if a.kind == CT_STRING() { return a.str_val == b.str_val }
    if a.kind == CT_LIST() {
        if a.items.len() != b.items.len() { return false }
        let mut i = 0
        while i < a.items.len() {
            if ct_equal(a.items[i], b.items[i]) == false { return false }
            i = i + 1
        }
        return true
    }
    return true
}

-- a op b for two values; not `and` and `or`, which ct_expr evaluates
fn ct_binary(st: CtState, op: int, a: CtValue, b: CtValue, line: int) -> CtValue {
    let mut out = ct_none()
    if a.kind == CT_INT() and b.kind == CT_INT() {
        out = ct_int_op(st, op, a.int_val, b.int_val, line)
    } else if a.kind == CT_FLOAT() and b.kind == CT_FLOAT() {
        out = ct_float_op(op, a.float_val, b.float_val)
    } else if a.kind == CT_BOOL() and b.kind == CT_BOOL() {
        if op == BINOP_BIT_AND() { out = ct_bool(a.bool_val and b.bool_val) }
        if op == BINOP_BIT_OR() { out = ct_bool(a.bool_val or b.bool_val) }
        if op == BINOP_BIT_XOR() { out = ct_bool(a.bool_val != b.bool_val) }
        if op == BINOP_EQ() { out = ct_bool(a.bool_val == b.bool_val) }
        if op == BINOP_NE() { out = ct_bool(a.bool_val != b.bool_val) }
    } else if a.kind == CT_STRING() and b.kind == CT_STRING() {
        if op == BINOP_ADD() { out = ct_string(a.str_val + b.str_val) }
        if op == BINOP_EQ() { out = ct_bool(a.str_val == b.str_val) }
        if op == BINOP_NE() { out = ct_bool(a.str_val != b.str_val) }
        if op == BINOP_LT() { out = ct_bool(a.str_val < b.str_val) }
        if op == BINOP_LE() { out = ct_bool(a.str_val <= b.str_val) }
        if op == BINOP_GT() { out = ct_bool(a.str_val > b.str_val) }
        if op == BINOP_GE() { out = ct_bool(a.str_val >= b.str_val) }
    } else if a.kind == CT_LIST() and b.kind == CT_LIST() {
        if op == BINOP_EQ() { out = ct_bool(ct_equal(a, b)) }
        if op == BINOP_NE() { out = ct_bool(ct_equal(a, b) == false) }
    }
    if out.kind == CT_NONE() {
        ct_error(st, "operator '" + ty_op_text(op) + "' does not take " + ct_kind_text(a) + " and " + ct_kind_text(b), line)
    }
    return out
}

-- ============================================================
-- EXPRESSIONS
-- ============================================================

-- The value as a bool, which a condition must be
fn ct_condition(st: CtState, v: CtValue, line: int) -> bool {
    if v.kind != CT_BOOL() { ct_error(st, "a condition is " + ct_kind_text(v) + ", not a bool", line) }
    return v.bool_val
}

-- The value as text, as an f-string prints it
fn ct_text(st: CtState, v: CtValue, line: int) -> string {
    if v.kind == CT_INT() { return int_to_string(v.int_val) }
    if v.kind == CT_FLOAT() { return float_to_string(v.float_val) }
    if v.kind == CT_BOOL() { return bool_to_string(v.bool_val) }
    if v.kind == CT_STRING() { return v.str_val }
    ct_error(st, ct_kind_text(v) + " cannot be made a string", line)
    return ""
}

-- Whether the integer fits the integer type
fn ct_int_fits(v: int, t: string) -> bool {
    if t == "i8" { return v >= 0 - 128 and v <= 127 }
    if t == "i16" { return v >= 0 - 32768 and v <= 32767 }
    if t == "i32" { return v >= 0 - 2147483648 and v <= 2147483647 }
    if t == "u8" { return v >= 0 and v <= 255 }
    if t == "u16" { return v >= 0 and v <= 65535 }
    if t == "u32" { return v >= 0 and v <= 4294967295 }
    if t == "u64" { return v >= 0 }
    return true
}

fn ct_cast(st: CtState, v: CtValue, t: string, line: int) -> CtValue {
    let to_float = t == "float" or t == "f64" or t == "f32"
    let to_int = t == "int" or t == "i64" or t == "i8" or t == "i16" or t == "i32" or t == "u8" or t == "u16" or t == "u32" or t == "u64"
    if v.kind == CT_INT() {
        if to_float { return ct_float(v.int_val as float) }
        if to_int {
            if ct_int_fits(v.int_val, t) == false {
                ct_error(st, int_to_string(v.int_val) + " does not fit " + t, line)
            }
            return v
        }
    }
    if v.kind == CT_FLOAT() {
        if to_float { return v }
        if to_int {
            if v.float_val >= 9223372036854775808.0 or v.float_val < 0.0 - 9223372036854775808.0 or v.float_val != v.float_val {
                ct_error(st, float_to_string(v.float_val) + " does not fit " + t, line)
            }
            let whole = v.float_val as int
            if ct_int_fits(whole, t) == false {
                ct_error(st, int_to_string(whole) + " does not fit " + t, line)
            }
            return ct_int(whole)
        }
    }
    if v.kind == CT_BOOL() and t == "bool" { return v }
    if v.kind == CT_STRING() and (t == "string" or t == "String") { return v }
    ct_error(st, ct_kind_text(v) + " cannot be cast to " + t, line)
    return v
}

-- What an expression the evaluator does not take is called, for the error
fn ct_expr_text(e: Expr) -> string {
    let k = e.kind
    if k == EXPR_FIELD_ACCESS() { return "a field ('." + e.field + "')" }
    if k == EXPR_METHOD_CALL() { return "the method '" + e.method + "'" }
    if k == EXPR_STRUCT_LITERAL() { return "a struct ('" + e.type_name + "')" }
    if k == EXPR_ENUM_LITERAL() { return "an enum value" }
    if k == EXPR_LAMBDA() { return "a lambda" }
    if k == EXPR_ERROR_PROPAGATE() { return "'?'" }
    if k == EXPR_RANGE() { return "a range outside a 'for'" }
    if k == EXPR_AWAIT() { return "'await'" }
    return "this expression"
}

fn ct_index(st: CtState, object: CtValue, index: CtValue, line: int) -> CtValue {
    if index.kind != CT_INT() { ct_error(st, "an index is " + ct_kind_text(index) + ", not an integer", line) }
    let i = index.int_val
    if object.kind == CT_LIST() {
        if i < 0 or i >= object.items.len() {
            ct_error(st, "index " + int_to_string(i) + " is out of range for a list of " + int_to_string(object.items.len()), line)
        }
        return object.items[i]
    }
    if object.kind == CT_STRING() {
        if i < 0 or i >= len(object.str_val) {
            ct_error(st, "index " + int_to_string(i) + " is out of range for a string of " + int_to_string(len(object.str_val)), line)
        }
        return ct_string(char_at(object.str_val, i))
    }
    ct_error(st, ct_kind_text(object) + " cannot be indexed", line)
    return ct_none()
}

fn ct_expr(st0: CtState, e: Expr) -> CtOut {
    let k = e.kind
    if k == EXPR_LITERAL_INT() { return ct_out(st0, ct_int(e.int_val)) }
    if k == EXPR_LITERAL_FLOAT() { return ct_out(st0, ct_float(e.float_val)) }
    if k == EXPR_LITERAL_BOOL() { return ct_out(st0, ct_bool(e.bool_val)) }
    if k == EXPR_LITERAL_STRING() { return ct_out(st0, ct_string(e.str_val)) }
    if k == EXPR_IDENTIFIER() { return ct_name(st0, e.name, e.line) }
    if k == EXPR_GROUPED() or k == EXPR_COMPTIME() { return ct_expr(st0, *e.operand) }
    let mut st = st0

    if k == EXPR_BINARY() {
        let left = ct_expr(st, *e.left)
        st = left.st
        if e.op == BINOP_AND() or e.op == BINOP_OR() {
            -- The right side is evaluated only where it decides
            let first = ct_condition(st, left.v, e.line)
            if e.op == BINOP_AND() and first == false { return ct_out(st, ct_bool(false)) }
            if e.op == BINOP_OR() and first { return ct_out(st, ct_bool(true)) }
            let right = ct_expr(st, *e.right)
            return ct_out(right.st, ct_bool(ct_condition(right.st, right.v, e.line)))
        }
        let right = ct_expr(st, *e.right)
        st = right.st
        let mut a = left.v
        let mut b = right.v
        -- An integer meets a float as a float
        if a.kind == CT_INT() and b.kind == CT_FLOAT() { a = ct_float(a.int_val as float) }
        if a.kind == CT_FLOAT() and b.kind == CT_INT() { b = ct_float(b.int_val as float) }
        return ct_out(st, ct_binary(st, e.op, a, b, e.line))
    }

    if k == EXPR_UNARY() {
        let operand = ct_expr(st, *e.operand)
        st = operand.st
        let v = operand.v
        if e.op == UNOP_NEG() and v.kind == CT_INT() {
            if v.int_val == CT_INT_MIN() { ct_error(st, "integer overflow", e.line) }
            return ct_out(st, ct_int(0 - v.int_val))
        }
        if e.op == UNOP_NEG() and v.kind == CT_FLOAT() { return ct_out(st, ct_float(0.0 - v.float_val)) }
        -- ~x is -x - 1
        if e.op == UNOP_BIT_NOT() and v.kind == CT_INT() { return ct_out(st, ct_int(0 - 1 - v.int_val)) }
        if e.op == UNOP_NOT() and v.kind == CT_BOOL() { return ct_out(st, ct_bool(v.bool_val == false)) }
        let mut text = "'not'"
        if e.op == UNOP_NEG() { text = "'-'" }
        if e.op == UNOP_BIT_NOT() { text = "'~'" }
        if e.op == UNOP_DEREF() { text = "'*'" }
        ct_error(st, text + " does not take " + ct_kind_text(v), e.line)
    }

    if k == EXPR_IF() {
        let cond = ct_expr(st, *e.condition)
        st = cond.st
        if ct_condition(st, cond.v, e.line) { return ct_expr(st, *e.then_branch) }
        if e.has_else { return ct_expr(st, *e.else_branch) }
        return ct_out(st, ct_none())
    }

    if k == EXPR_BLOCK() {
        let mark = st.names.len()
        let mut done = ct_stmts(st, e.stmts)
        if done.flow == CT_NEXT() and e.has_result {
            done = ct_expr(done.st, *e.result)
        }
        done.st = ct_leave(done.st, mark)
        return done
    }

    if k == EXPR_MATCH() { return ct_match(st, e) }
    if k == EXPR_FUNCTION_CALL() { return ct_call(st, e) }

    if k == EXPR_ARRAY_LITERAL() {
        let mut items: List[CtValue] = []
        let mut i = 0
        while i < e.elements.len() {
            let item = ct_expr(st, e.elements[i])
            st = item.st
            items.push(item.v)
            i = i + 1
        }
        return ct_out(st, ct_list(items))
    }

    if k == EXPR_INDEX_ACCESS() {
        let object = ct_expr(st, *e.object)
        let index = ct_expr(object.st, *e.index)
        return ct_out(index.st, ct_index(index.st, object.v, index.v, e.line))
    }

    if k == EXPR_METHOD_CALL() and e.method == "len" and e.args.len() == 0 {
        let object = ct_expr(st, *e.object)
        if object.v.kind == CT_LIST() { return ct_out(object.st, ct_int(object.v.items.len())) }
        if object.v.kind == CT_STRING() { return ct_out(object.st, ct_int(len(object.v.str_val))) }
        ct_error(st, ct_kind_text(object.v) + " has no length", e.line)
    }

    if k == EXPR_CAST() {
        let operand = ct_expr(st, *e.operand)
        return ct_out(operand.st, ct_cast(operand.st, operand.v, e.cast_type, e.line))
    }

    if k == EXPR_STRING_INTERP() {
        let mut text = ""
        let mut i = 0
        while i < e.interp_parts.len() {
            let part = ct_expr(st, e.interp_parts[i])
            st = part.st
            text = text + ct_text(st, part.v, e.line)
            i = i + 1
        }
        return ct_out(st, ct_string(text))
    }

    ct_error(st, ct_expr_text(e) + " cannot be evaluated", e.line)
    return ct_out(st, ct_none())
}

-- Whether the value matches the pattern of a match arm
fn ct_pattern_matches(st: CtState, v: CtValue, pat: Pattern, line: int) -> bool {
    if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() { return true }
    if pat.kind == PAT_LITERAL() {
        let literal = ct_expr(st, *pat.literal_expr)
        return ct_equal(v, literal.v)
    }
    ct_error(st, "the pattern of a variant ('" + pat.variant + "') cannot be matched", line)
    return false
}

fn ct_match(st0: CtState, e: Expr) -> CtOut {
    let scrutinee = ct_expr(st0, *e.scrutinee)
    let mut st = scrutinee.st
    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        if ct_pattern_matches(st, scrutinee.v, arm.pattern, e.line) {
            let mark = st.names.len()
            if arm.pattern.kind == PAT_IDENTIFIER() {
                st = ct_bind(st, arm.pattern.name, scrutinee.v)
            }
            let mut taken = true
            if arm.has_guard {
                let guard = ct_expr(st, *arm.guard)
                st = guard.st
                taken = ct_condition(st, guard.v, e.line)
            }
            if taken {
                let mut done = ct_out(st, ct_none())
                if arm.is_expr_body {
                    done = ct_expr(st, *arm.body_expr)
                } else {
                    done = ct_stmts(st, arm.body)
                }
                done.st = ct_leave(done.st, mark)
                return done
            }
            st = ct_leave(st, mark)
        }
        i = i + 1
    }
    ct_error(st, "no arm of the match takes the value", e.line)
    return ct_out(st, ct_none())
}

-- ============================================================
-- CALLS
-- ============================================================

fn ct_want(st: CtState, name: string, args: List[CtValue], i: int, kind: int, line: int) -> CtValue {
    if args[i].kind != kind {
        ct_error(st, "argument " + int_to_string(i + 1) + " of " + name + "() is " + ct_kind_text(args[i]), line)
    }
    return args[i]
}

-- The builtin functions that are evaluated at compile time; CT_NONE
-- for any other name
fn ct_builtin(st: CtState, name: string, args: List[CtValue], line: int) -> CtValue {
    let n = args.len()
    if name == "len" and n == 1 {
        if args[0].kind == CT_LIST() { return ct_int(args[0].items.len()) }
        return ct_int(len(ct_want(st, name, args, 0, CT_STRING(), line).str_val))
    }
    if name == "int_to_string" and n == 1 { return ct_string(int_to_string(ct_want(st, name, args, 0, CT_INT(), line).int_val)) }
    if name == "float_to_string" and n == 1 { return ct_string(float_to_string(ct_want(st, name, args, 0, CT_FLOAT(), line).float_val)) }
    if name == "bool_to_string" and n == 1 { return ct_string(bool_to_string(ct_want(st, name, args, 0, CT_BOOL(), line).bool_val)) }
    if name == "char_at" and n == 2 {
        return ct_index(st, ct_want(st, name, args, 0, CT_STRING(), line), ct_want(st, name, args, 1, CT_INT(), line), line)
    }
    if name == "substr" and n == 3 {
        let s = ct_want(st, name, args, 0, CT_STRING(), line).str_val
        return ct_string(substr(s, ct_want(st, name, args, 1, CT_INT(), line).int_val, ct_want(st, name, args, 2, CT_INT(), line).int_val))
    }
    if n == 2 and (name == "string_contains" or name == "string_find" or name == "starts_with" or name == "ends_with") {
        let s = ct_want(st, name, args, 0, CT_STRING(), line).str_val
        let t = ct_want(st, name, args, 1, CT_STRING(), line).str_val
        if name == "string_contains" { return ct_bool(string_contains(s, t)) }
        if name == "string_find" { return ct_int(string_find(s, t)) }
        if name == "starts_with" { return ct_bool(starts_with(s, t)) }
        return ct_bool(ends_with(s, t))
    }
    if n == 1 and (name == "string_trim" or name == "string_to_upper" or name == "string_to_lower") {
        let s = ct_want(st, name, args, 0, CT_STRING(), line).str_val
        if name == "string_trim" { return ct_string(string_trim(s)) }
        if name == "string_to_upper" { return ct_string(string_to_upper(s)) }
        return ct_string(string_to_lower(s))
    }
    if name == "string_replace" and n == 3 {
        let s = ct_want(st, name, args, 0, CT_STRING(), line).str_val
        return ct_string(string_replace(s, ct_want(st, name, args, 1, CT_STRING(), line).str_val, ct_want(st, name, args, 2, CT_STRING(), line).str_val))
    }
    return ct_none()
}

fn ct_call(st0: CtState, e: Expr) -> CtOut {
    let callee = *e.callee
    if callee.kind != EXPR_IDENTIFIER() {
        ct_error(st0, "a call of what is not a function's name cannot be evaluated", e.line)
    }
    let name = callee.name
    let mut st = ct_step(st0, e.line)
    let mut args: List[CtValue] = []
    let mut i = 0
    while i < e.args.len() {
        let arg = ct_expr(st, e.args[i])
        st = arg.st
        args.push(arg.v)
        i = i + 1
    }

    -- A builtin's name is the builtin, as it is in the program
    if is_builtin_fn(name) {
        let built = ct_builtin(st, name, args, e.line)
        if built.kind == CT_NONE() {
            ct_error(st, "'" + name + "' cannot be called", e.line)
        }
        return ct_out(st, built)
    }
    -- The program's function of the name. The methods of generic
    -- types are among the declarations too, under their own names:
    -- they take `self`.
    let mut found = -1
    i = 0
    while i < st.decls.len() and found < 0 {
        let decl = st.decls[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            let mut is_method = false
            if fd.params.len() > 0 { is_method = fd.params[0].name == "self" }
            if fd.name == name and fd.is_extern == false and is_method == false { found = i }
        }
        i = i + 1
    }
    if found < 0 {
        ct_error(st, "'" + name + "' cannot be called", e.line)
    }
    let decl = st.decls[found]
    let fd = *decl.func_decl
    if fd.params.len() != args.len() {
        ct_error(st, "'" + name + "' takes " + int_to_string(fd.params.len()) + " arguments, not " + int_to_string(args.len()) + ",", e.line)
    }
    if st.depth >= CT_MAX_DEPTH() {
        ct_error(st, "calls nested more than " + int_to_string(CT_MAX_DEPTH()) + " deep ('" + name + "')", e.line)
    }

    -- The function sees its parameters and nothing of its caller
    let mark = st.names.len()
    let caller_base = st.base
    st.base = mark
    st.depth = st.depth + 1
    i = 0
    while i < fd.params.len() {
        st = ct_bind(st, fd.params[i].name, args[i])
        i = i + 1
    }
    let done = ct_stmts(st, fd.body)
    st = ct_leave(done.st, mark)
    st.base = caller_base
    st.depth = st.depth - 1
    if done.flow == CT_BREAK() or done.flow == CT_CONTINUE() {
        ct_error(st, "'break' or 'continue' outside a loop in '" + name + "'", e.line)
    }
    if done.flow != CT_RETURN() and done.v.kind == CT_NONE() and fd.return_type != "" and fd.return_type != "void" {
        ct_error(st, "'" + name + "' ended without returning a value", e.line)
    }
    return ct_out(st, done.v)
}

-- ============================================================
-- STATEMENTS
-- ============================================================

-- Run the body of a loop once. The variables it declares are forgotten.
fn ct_loop_body(st: CtState, body: List[Stmt]) -> CtOut {
    let mark = st.names.len()
    let mut done = ct_stmts(st, body)
    done.st = ct_leave(done.st, mark)
    return done
}

-- name = v, or name op= v
fn ct_assign(st0: CtState, s: Stmt) -> CtState {
    let value = ct_expr(st0, *s.assign_value)
    let mut st = value.st
    let mut v = value.v
    let target = *s.assign_target
    let mut op = 0 - 1
    if s.assign_op == ASSIGN_ADD() { op = BINOP_ADD() }
    if s.assign_op == ASSIGN_SUB() { op = BINOP_SUB() }
    if s.assign_op == ASSIGN_MUL() { op = BINOP_MUL() }
    if s.assign_op == ASSIGN_DIV() { op = BINOP_DIV() }
    if s.assign_op == ASSIGN_MOD() { op = BINOP_MOD() }
    if target.kind == EXPR_IDENTIFIER() {
        let at = ct_find(st, target.name)
        if at < 0 {
            ct_error(st, "'" + source_name(target.name) + "' is not a variable of the expression", s.line)
        }
        if op >= 0 { v = ct_binary(st, op, st.values[at], v, s.line) }
        st.values[at] = v
        return st
    }
    if target.kind == EXPR_INDEX_ACCESS() {
        let object = *target.object
        if object.kind == EXPR_IDENTIFIER() {
            let at = ct_find(st, object.name)
            if at < 0 {
                ct_error(st, "'" + source_name(object.name) + "' is not a variable of the expression", s.line)
            }
            let index = ct_expr(st, *target.index)
            st = index.st
            let old = ct_index(st, st.values[at], index.v, s.line)
            if op >= 0 { v = ct_binary(st, op, old, v, s.line) }
            let mut list = st.values[at]
            if list.kind != CT_LIST() { ct_error(st, "an element of " + ct_kind_text(list) + " cannot be assigned", s.line) }
            -- A list of its own: another variable may hold the old one
            let mut items: List[CtValue] = []
            let mut i = 0
            while i < list.items.len() {
                if i == index.v.int_val {
                    items.push(v)
                } else {
                    items.push(list.items[i])
                }
                i = i + 1
            }
            list.items = items
            st.values[at] = list
            return st
        }
    }
    ct_error(st, "this assignment cannot be evaluated", s.line)
    return st
}

-- xs.push(v) on a variable: the one method call that is a statement
fn ct_push(st0: CtState, e: Expr) -> CtState {
    let object = *e.object
    let at = ct_find(st0, object.name)
    if at < 0 {
        ct_error(st0, "'" + source_name(object.name) + "' is not a variable of the expression", e.line)
    }
    let pushed = ct_expr(st0, e.args[0])
    let mut st = pushed.st
    let mut list = st.values[at]
    if list.kind != CT_LIST() { ct_error(st, "push() on " + ct_kind_text(list), e.line) }
    -- A list of its own: another variable may hold the old one
    let mut items: List[CtValue] = []
    let mut i = 0
    while i < list.items.len() {
        items.push(list.items[i])
        i = i + 1
    }
    items.push(pushed.v)
    list.items = items
    st.values[at] = list
    return st
}

-- Run the statements in turn. The value is that of the last one, if it
-- gives one (the value of a block that ends in an expression, an `if`
-- or a `match`), or what a `return` returns.
fn ct_stmts(st0: CtState, stmts: List[Stmt]) -> CtOut {
    let mut st = st0
    let mut last = ct_none()
    let mut i = 0
    while i < stmts.len() {
        let s = stmts[i]
        let k = s.kind
        last = ct_none()
        if k == STMT_RETURN() {
            let mut out = ct_out(st, ct_none())
            if s.has_ret_value { out = ct_expr(st, *s.ret_value) }
            out.flow = CT_RETURN()
            return out
        } else if k == STMT_BREAK() or k == STMT_CONTINUE() {
            let mut out = ct_out(st, ct_none())
            out.flow = CT_BREAK()
            if k == STMT_CONTINUE() { out.flow = CT_CONTINUE() }
            return out
        } else if k == STMT_LET() {
            let value_expr = *s.let_value
            if value_expr.kind == EXPR_LITERAL_NULL() {
                st = ct_bind(st, s.let_name, ct_none())
            } else {
                let value = ct_expr(st, value_expr)
                st = ct_bind(value.st, s.let_name, value.v)
            }
        } else if k == STMT_ASSIGNMENT() {
            st = ct_assign(st, s)
        } else if k == STMT_EXPRESSION() {
            let e = *s.expr
            let mut is_push = false
            if e.kind == EXPR_METHOD_CALL() and e.method == "push" and e.args.len() == 1 {
                let object = *e.object
                is_push = object.kind == EXPR_IDENTIFIER()
            }
            if is_push {
                st = ct_push(st, e)
            } else {
                let value = ct_expr(st, e)
                st = value.st
                last = value.v
                if value.flow != CT_NEXT() { return value }
            }
        } else if k == STMT_IF() {
            let cond = ct_expr(st, *s.if_cond)
            st = cond.st
            let mark = st.names.len()
            let mut done = ct_out(st, ct_none())
            if ct_condition(st, cond.v, s.line) {
                done = ct_stmts(st, s.if_then)
            } else if s.has_else_branch {
                done = ct_stmts(st, s.if_else)
            }
            done.st = ct_leave(done.st, mark)
            if done.flow != CT_NEXT() { return done }
            st = done.st
            last = done.v
        } else if k == STMT_MATCH() {
            let done = ct_match(st, *s.match_expr)
            if done.flow != CT_NEXT() { return done }
            st = done.st
            last = done.v
        } else if k == STMT_WHILE() or k == STMT_LOOP() {
            let mut looping = true
            while looping {
                st = ct_step(st, s.line)
                if k == STMT_WHILE() {
                    let cond = ct_expr(st, *s.while_cond)
                    st = cond.st
                    looping = ct_condition(st, cond.v, s.line)
                }
                if looping {
                    let mut body = s.while_body
                    if k == STMT_LOOP() { body = s.loop_body }
                    let done = ct_loop_body(st, body)
                    st = done.st
                    if done.flow == CT_RETURN() { return done }
                    if done.flow == CT_BREAK() { looping = false }
                }
            }
        } else if k == STMT_FOR() {
            let iter = *s.for_iter
            let mut items: List[CtValue] = []
            if iter.kind == EXPR_RANGE() {
                let from = ct_expr(st, *iter.left)
                let to = ct_expr(from.st, *iter.right)
                st = to.st
                if from.v.kind != CT_INT() or to.v.kind != CT_INT() {
                    ct_error(st, "a range runs from " + ct_kind_text(from.v) + " to " + ct_kind_text(to.v) + ", not between integers", s.line)
                }
                let mut n = from.v.int_val
                let end = to.v.int_val
                while n < end {
                    st = ct_step(st, s.line)
                    items.push(ct_int(n))
                    n = n + 1
                }
                -- a..=b ends with b
                if iter.op == BINOP_LE() and from.v.int_val <= end { items.push(ct_int(end)) }
            } else {
                let list = ct_expr(st, iter)
                st = list.st
                if list.v.kind != CT_LIST() {
                    ct_error(st, "'for' runs over " + ct_kind_text(list.v) + ", not a list or a range", s.line)
                }
                items = list.v.items
            }
            let mut fi = 0
            let mut looping = true
            while looping and fi < items.len() {
                st = ct_step(st, s.line)
                let mark = st.names.len()
                st = ct_bind(st, s.for_var, items[fi])
                let mut done = ct_stmts(st, s.for_body)
                done.st = ct_leave(done.st, mark)
                st = done.st
                if done.flow == CT_RETURN() { return done }
                if done.flow == CT_BREAK() { looping = false }
                fi = fi + 1
            }
        } else {
            ct_error(st, "this statement cannot be evaluated", s.line)
        }
        i = i + 1
    }
    return ct_out(st, last)
}

-- ============================================================
-- THE VALUE OF AN EXPRESSION
-- ============================================================

-- Evaluate the expression at compile time. The value is a number, a
-- bool or a string; anything else, and anything the evaluator cannot
-- do, is reported and ends the compilation.
fn eval_comptime_expr(gen: IRGenerator, e: Expr) -> IRValue {
    let st = CtState {
        files: gen.ir_mod.files,
        decls: gen.declarations,
        globals: gen.ir_mod.globals,
        names: [],
        values: [],
        base: 0,
        steps: 0,
        depth: 0
    }
    let out = ct_expr(st, e)
    let v = out.v
    if out.flow == CT_BREAK() or out.flow == CT_CONTINUE() {
        ct_error(st, "'break' or 'continue' outside a loop", e.line)
    }
    if v.kind == CT_INT() { return ir_val_int(v.int_val) }
    if v.kind == CT_FLOAT() { return ir_val_float(v.float_val) }
    if v.kind == CT_BOOL() { return ir_val_bool(v.bool_val) }
    if v.kind == CT_STRING() { return ir_val_string(v.str_val) }
    ct_error(st, "the value is " + ct_kind_text(v) + ", not a number, a bool or a string,", e.line)
    return ir_val_int(0)
}

-- The IR type of a value that was found at compile time
fn comptime_value_type(val: IRValue) -> string {
    if val.kind == VAL_CONST_INT() { return "i64" }
    if val.kind == VAL_CONST_FLOAT() { return "f64" }
    if val.kind == VAL_CONST_BOOL() { return "bool" }
    if val.kind == VAL_CONST_STRING() { return "string" }
    return "i64"
}
