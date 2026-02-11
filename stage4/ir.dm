module ir

-- ============================================================
-- dAImond Intermediate Representation
-- SSA-based typed IR between AST and LLVM IR
-- Port of stage3/src/ir.zig using integer kind tags
-- ============================================================

-- ============================================================
-- IR TYPE KINDS
-- ============================================================
fn IR_TYPE_I8() -> int { return 1 }
fn IR_TYPE_I16() -> int { return 2 }
fn IR_TYPE_I32() -> int { return 3 }
fn IR_TYPE_I64() -> int { return 4 }
fn IR_TYPE_U8() -> int { return 5 }
fn IR_TYPE_U16() -> int { return 6 }
fn IR_TYPE_U32() -> int { return 7 }
fn IR_TYPE_U64() -> int { return 8 }
fn IR_TYPE_F32() -> int { return 9 }
fn IR_TYPE_F64() -> int { return 10 }
fn IR_TYPE_BOOL() -> int { return 11 }
fn IR_TYPE_VOID() -> int { return 12 }
fn IR_TYPE_STRING() -> int { return 13 }
fn IR_TYPE_NEVER() -> int { return 14 }
fn IR_TYPE_PTR() -> int { return 20 }
fn IR_TYPE_ARRAY() -> int { return 21 }
fn IR_TYPE_STRUCT() -> int { return 30 }
fn IR_TYPE_TAGGED_UNION() -> int { return 31 }
fn IR_TYPE_FN() -> int { return 40 }
fn IR_TYPE_FN_PTR() -> int { return 41 }
fn IR_TYPE_OPTION() -> int { return 50 }
fn IR_TYPE_RESULT() -> int { return 51 }
fn IR_TYPE_FUTURE() -> int { return 52 }
fn IR_TYPE_SLICE() -> int { return 53 }
fn IR_TYPE_VECTOR() -> int { return 60 }

-- SIMD vector element kinds
fn VEC_ELEM_F32() -> int { return 0 }
fn VEC_ELEM_F64() -> int { return 1 }
fn VEC_ELEM_I32() -> int { return 2 }
fn VEC_ELEM_I64() -> int { return 3 }

-- IR Type: represented as a struct with kind tag
struct IRType {
    kind: int,
    name: string,
    -- Struct fields
    fields: List[IRField],
    -- Enum variants
    variants: List[IRVariant],
    -- Function params (type IDs as strings for lookup)
    param_types: List[string],
    ret_type: string,
    -- Inner type for ptr/array/option/future/slice
    inner_type: string,
    -- Error type for result
    err_type: string,
    -- Array size
    array_size: int,
    -- Vector type info
    vec_elem_kind: int,
    vec_lanes: int
}

fn ir_type_new(kind: int) -> IRType {
    return IRType {
        kind: kind, name: "",
        fields: [], variants: [],
        param_types: [], ret_type: "",
        inner_type: "", err_type: "",
        array_size: 0,
        vec_elem_kind: 0, vec_lanes: 0
    }
}

fn ir_type_simple(kind: int) -> IRType {
    return ir_type_new(kind)
}

fn ir_type_named(kind: int, name: string) -> IRType {
    let mut t = ir_type_new(kind)
    t.name = name
    return t
}

fn ir_type_ptr(inner: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_PTR())
    t.inner_type = inner
    return t
}

fn ir_type_array(inner: string, size: int) -> IRType {
    let mut t = ir_type_new(IR_TYPE_ARRAY())
    t.inner_type = inner
    t.array_size = size
    return t
}

fn ir_type_option(inner: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_OPTION())
    t.inner_type = inner
    return t
}

fn ir_type_result(ok: string, err: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_RESULT())
    t.inner_type = ok
    t.err_type = err
    return t
}

fn ir_type_fn(params: List[string], ret: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_FN())
    t.param_types = params
    t.ret_type = ret
    return t
}

fn ir_type_vector(elem_kind: int, lanes: int) -> IRType {
    let mut t = ir_type_new(IR_TYPE_VECTOR())
    t.vec_elem_kind = elem_kind
    t.vec_lanes = lanes
    return t
}

fn ir_type_name(ty: IRType) -> string {
    if ty.kind == IR_TYPE_I8() { return "i8" }
    if ty.kind == IR_TYPE_I16() { return "i16" }
    if ty.kind == IR_TYPE_I32() { return "i32" }
    if ty.kind == IR_TYPE_I64() { return "i64" }
    if ty.kind == IR_TYPE_U8() { return "u8" }
    if ty.kind == IR_TYPE_U16() { return "u16" }
    if ty.kind == IR_TYPE_U32() { return "u32" }
    if ty.kind == IR_TYPE_U64() { return "u64" }
    if ty.kind == IR_TYPE_F32() { return "f32" }
    if ty.kind == IR_TYPE_F64() { return "f64" }
    if ty.kind == IR_TYPE_BOOL() { return "bool" }
    if ty.kind == IR_TYPE_VOID() { return "void" }
    if ty.kind == IR_TYPE_STRING() { return "string" }
    if ty.kind == IR_TYPE_NEVER() { return "!" }
    if ty.kind == IR_TYPE_PTR() { return "ptr(" + ty.inner_type + ")" }
    if ty.kind == IR_TYPE_ARRAY() { return "[" + ty.inner_type + "; " + int_to_string(ty.array_size) + "]" }
    if ty.kind == IR_TYPE_STRUCT() { return "struct " + ty.name }
    if ty.kind == IR_TYPE_TAGGED_UNION() { return "enum " + ty.name }
    if ty.kind == IR_TYPE_FN() { return "fn(...) -> " + ty.ret_type }
    if ty.kind == IR_TYPE_FN_PTR() { return "fn_ptr(...) -> " + ty.ret_type }
    if ty.kind == IR_TYPE_OPTION() { return "Option[" + ty.inner_type + "]" }
    if ty.kind == IR_TYPE_RESULT() { return "Result[" + ty.inner_type + ", " + ty.err_type + "]" }
    if ty.kind == IR_TYPE_FUTURE() { return "Future[" + ty.inner_type + "]" }
    if ty.kind == IR_TYPE_SLICE() { return "Slice[" + ty.inner_type + "]" }
    if ty.kind == IR_TYPE_VECTOR() { return vec_type_name(ty.vec_elem_kind, ty.vec_lanes) }
    return "unknown"
}

fn vec_type_name(elem_kind: int, lanes: int) -> string {
    if elem_kind == VEC_ELEM_F32() and lanes == 4 { return "f32x4" }
    if elem_kind == VEC_ELEM_F32() and lanes == 8 { return "f32x8" }
    if elem_kind == VEC_ELEM_F64() and lanes == 2 { return "f64x2" }
    if elem_kind == VEC_ELEM_F64() and lanes == 4 { return "f64x4" }
    if elem_kind == VEC_ELEM_I32() and lanes == 4 { return "i32x4" }
    if elem_kind == VEC_ELEM_I32() and lanes == 8 { return "i32x8" }
    if elem_kind == VEC_ELEM_I64() and lanes == 2 { return "i64x2" }
    if elem_kind == VEC_ELEM_I64() and lanes == 4 { return "i64x4" }
    return "vector_unknown"
}

fn vec_elem_is_float(elem_kind: int) -> bool {
    return elem_kind == VEC_ELEM_F32() or elem_kind == VEC_ELEM_F64()
}

-- Struct field in IR
struct IRField {
    name: string,
    type_id: string
}

fn ir_field_new(name: string, type_id: string) -> IRField {
    return IRField { name: name, type_id: type_id }
}

-- Enum variant in IR
struct IRVariant {
    name: string,
    tag: int,
    payload_type: string,
    has_payload: bool
}

fn ir_variant_new(name: string, tag: int) -> IRVariant {
    return IRVariant { name: name, tag: tag, payload_type: "", has_payload: false }
}

-- ============================================================
-- IR VALUE KINDS
-- ============================================================
fn VAL_CONST_INT() -> int { return 1 }
fn VAL_CONST_FLOAT() -> int { return 2 }
fn VAL_CONST_BOOL() -> int { return 3 }
fn VAL_CONST_STRING() -> int { return 4 }
fn VAL_INST_REF() -> int { return 5 }
fn VAL_PARAM_REF() -> int { return 6 }
fn VAL_GLOBAL_REF() -> int { return 7 }
fn VAL_UNDEF() -> int { return 8 }

struct IRValue {
    kind: int,
    int_val: int,
    float_val: float,
    bool_val: bool,
    str_val: string,
    ref_id: int
}

fn ir_val_int(v: int) -> IRValue {
    return IRValue { kind: VAL_CONST_INT(), int_val: v, float_val: 0.0, bool_val: false, str_val: "", ref_id: 0 }
}

fn ir_val_float(v: float) -> IRValue {
    return IRValue { kind: VAL_CONST_FLOAT(), int_val: 0, float_val: v, bool_val: false, str_val: "", ref_id: 0 }
}

fn ir_val_bool(v: bool) -> IRValue {
    return IRValue { kind: VAL_CONST_BOOL(), int_val: 0, float_val: 0.0, bool_val: v, str_val: "", ref_id: 0 }
}

fn ir_val_string(v: string) -> IRValue {
    return IRValue { kind: VAL_CONST_STRING(), int_val: 0, float_val: 0.0, bool_val: false, str_val: v, ref_id: 0 }
}

fn ir_val_inst(id: int) -> IRValue {
    return IRValue { kind: VAL_INST_REF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: id }
}

fn ir_val_param(id: int) -> IRValue {
    return IRValue { kind: VAL_PARAM_REF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: id }
}

fn ir_val_global(name: string) -> IRValue {
    return IRValue { kind: VAL_GLOBAL_REF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: name, ref_id: 0 }
}

fn ir_val_undef() -> IRValue {
    return IRValue { kind: VAL_UNDEF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: 0 }
}

-- ============================================================
-- IR INSTRUCTION OP KINDS
-- ============================================================
fn OP_ADD() -> int { return 1 }
fn OP_SUB() -> int { return 2 }
fn OP_MUL() -> int { return 3 }
fn OP_DIV() -> int { return 4 }
fn OP_MOD() -> int { return 5 }
fn OP_NEG() -> int { return 6 }
fn OP_EQ() -> int { return 10 }
fn OP_NE() -> int { return 11 }
fn OP_LT() -> int { return 12 }
fn OP_LE() -> int { return 13 }
fn OP_GT() -> int { return 14 }
fn OP_GE() -> int { return 15 }
fn OP_LOGICAL_AND() -> int { return 20 }
fn OP_LOGICAL_OR() -> int { return 21 }
fn OP_LOGICAL_NOT() -> int { return 22 }
fn OP_BIT_AND() -> int { return 25 }
fn OP_BIT_OR() -> int { return 26 }
fn OP_BIT_XOR() -> int { return 27 }
fn OP_SHL() -> int { return 28 }
fn OP_SHR() -> int { return 29 }
fn OP_ALLOCA() -> int { return 30 }
fn OP_LOAD() -> int { return 31 }
fn OP_STORE() -> int { return 32 }
fn OP_GEP() -> int { return 33 }
fn OP_EXTRACT_FIELD() -> int { return 34 }
fn OP_INSERT_FIELD() -> int { return 35 }
fn OP_CALL() -> int { return 40 }
fn OP_CALL_PTR() -> int { return 41 }
fn OP_CAST() -> int { return 50 }
fn OP_PHI() -> int { return 51 }
fn OP_STRING_CONCAT() -> int { return 60 }
fn OP_STRING_EQ() -> int { return 61 }
fn OP_STRING_LEN() -> int { return 62 }
fn OP_LIST_NEW() -> int { return 70 }
fn OP_LIST_PUSH() -> int { return 71 }
fn OP_LIST_GET() -> int { return 72 }
fn OP_LIST_LEN() -> int { return 73 }
fn OP_LIST_POP() -> int { return 74 }
fn OP_MAP_NEW() -> int { return 80 }
fn OP_MAP_INSERT() -> int { return 81 }
fn OP_MAP_GET() -> int { return 82 }
fn OP_MAP_CONTAINS() -> int { return 83 }
fn OP_ARENA_CREATE() -> int { return 90 }
fn OP_ARENA_DESTROY() -> int { return 91 }
fn OP_ARENA_ALLOC() -> int { return 92 }
fn OP_SIMD_SPLAT() -> int { return 100 }
fn OP_SIMD_SET() -> int { return 101 }
fn OP_SIMD_ADD() -> int { return 102 }
fn OP_SIMD_SUB() -> int { return 103 }
fn OP_SIMD_MUL() -> int { return 104 }
fn OP_SIMD_DIV() -> int { return 105 }
fn OP_SIMD_EXTRACT() -> int { return 106 }
fn OP_PANIC() -> int { return 110 }

-- IR Instruction
struct IRInst {
    id: int,
    op: int,
    result_type: string,
    has_result: bool,
    -- Binary operands
    lhs: IRValue,
    rhs: IRValue,
    -- Unary operand
    operand: IRValue,
    -- Alloca
    alloc_type: string,
    -- Load
    load_ptr: IRValue,
    load_type: string,
    -- Store
    store_ptr: IRValue,
    store_val: IRValue,
    -- GEP
    gep_base: IRValue,
    gep_indices: List[IRValue],
    -- Extract/Insert field
    field_base: IRValue,
    field_index: int,
    field_value: IRValue,
    field_type: string,
    -- Call
    callee: string,
    call_args: List[IRValue],
    -- Call ptr
    callee_val: IRValue,
    -- Cast
    cast_val: IRValue,
    cast_from: string,
    cast_to: string,
    -- Phi
    phi_entries: List[PhiEntry],
    -- List/Map new
    elem_type: string,
    key_type: string,
    val_type: string,
    -- Map insert
    map_val: IRValue,
    map_key: IRValue,
    map_value: IRValue,
    -- SIMD
    simd_scalar: IRValue,
    simd_vec_type: string,
    simd_elements: List[IRValue],
    simd_vector: IRValue,
    simd_index: IRValue,
    -- Source location
    source_line: int,
    source_col: int
}

fn ir_inst_new(id: int, op: int) -> IRInst {
    let undef = ir_val_undef()
    return IRInst {
        id: id, op: op,
        result_type: "", has_result: false,
        lhs: undef, rhs: undef,
        operand: undef,
        alloc_type: "",
        load_ptr: undef, load_type: "",
        store_ptr: undef, store_val: undef,
        gep_base: undef, gep_indices: [],
        field_base: undef, field_index: 0, field_value: undef, field_type: "",
        callee: "", call_args: [],
        callee_val: undef,
        cast_val: undef, cast_from: "", cast_to: "",
        phi_entries: [],
        elem_type: "", key_type: "", val_type: "",
        map_val: undef, map_key: undef, map_value: undef,
        simd_scalar: undef, simd_vec_type: "", simd_elements: [],
        simd_vector: undef, simd_index: undef,
        source_line: 0, source_col: 0
    }
}

-- Phi entry
struct PhiEntry {
    value: IRValue,
    block_label: string
}

fn phi_entry_new(value: IRValue, label: string) -> PhiEntry {
    return PhiEntry { value: value, block_label: label }
}

-- ============================================================
-- TERMINATOR KINDS
-- ============================================================
fn TERM_BR() -> int { return 1 }
fn TERM_BR_COND() -> int { return 2 }
fn TERM_RET() -> int { return 3 }
fn TERM_RET_VOID() -> int { return 4 }
fn TERM_UNREACHABLE() -> int { return 5 }

struct IRTerminator {
    kind: int,
    -- Branch target
    target: string,
    -- Conditional branch
    cond: IRValue,
    true_label: string,
    false_label: string,
    -- Return value
    ret_val: IRValue
}

fn term_br(target: string) -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_BR(), target: target,
        cond: undef, true_label: "", false_label: "",
        ret_val: undef
    }
}

fn term_br_cond(cond: IRValue, true_label: string, false_label: string) -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_BR_COND(), target: "",
        cond: cond, true_label: true_label, false_label: false_label,
        ret_val: undef
    }
}

fn term_ret(val: IRValue) -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_RET(), target: "",
        cond: undef, true_label: "", false_label: "",
        ret_val: val
    }
}

fn term_ret_void() -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_RET_VOID(), target: "",
        cond: undef, true_label: "", false_label: "",
        ret_val: undef
    }
}

fn term_unreachable() -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_UNREACHABLE(), target: "",
        cond: undef, true_label: "", false_label: "",
        ret_val: undef
    }
}

-- ============================================================
-- BASIC BLOCK
-- ============================================================

struct IRBasicBlock {
    label: string,
    instructions: List[IRInst],
    terminator: Box[IRTerminator],
    has_terminator: bool
}

fn ir_block_new(label: string) -> IRBasicBlock {
    return IRBasicBlock {
        label: label,
        instructions: [],
        terminator: Box_null(),
        has_terminator: false
    }
}

-- ============================================================
-- FUNCTION
-- ============================================================

struct IRParam {
    name: string,
    type_id: string
}

fn ir_param_new(name: string, type_id: string) -> IRParam {
    return IRParam { name: name, type_id: type_id }
}

struct IRFunction {
    name: string,
    params: List[IRParam],
    return_type: string,
    blocks: List[IRBasicBlock],
    is_async: bool,
    is_extern: bool,
    is_comptime: bool
}

fn ir_function_new(name: string, ret_type: string) -> IRFunction {
    return IRFunction {
        name: name, params: [], return_type: ret_type,
        blocks: [],
        is_async: false, is_extern: false, is_comptime: false
    }
}

-- ============================================================
-- MODULE
-- ============================================================

struct IRGlobal {
    name: string,
    type_id: string,
    init_value: IRValue,
    has_init: bool,
    is_const: bool
}

fn ir_global_new(name: string, type_id: string) -> IRGlobal {
    return IRGlobal { name: name, type_id: type_id, init_value: ir_val_undef(), has_init: false, is_const: false }
}

struct IRExternDecl {
    name: string,
    type_id: string
}

fn ir_extern_decl_new(name: string, type_id: string) -> IRExternDecl {
    return IRExternDecl { name: name, type_id: type_id }
}

struct IRModule {
    functions: List[IRFunction],
    globals: List[IRGlobal],
    struct_defs: Map[string, IRType],
    extern_decls: List[IRExternDecl],
    user_extern_fns: Map[string, bool]
}

fn ir_module_new() -> IRModule {
    return IRModule {
        functions: [],
        globals: [],
        struct_defs: Map_new(),
        extern_decls: [],
        user_extern_fns: Map_new()
    }
}
