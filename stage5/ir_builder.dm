module ir_builder

import ir

-- ============================================================
-- IR Builder
-- Builds instructions within a current function and basic block
-- Port of stage3/src/ir.zig IRBuilder
-- ============================================================

struct IRBuilder {
    current_function: Box[IRFunction],
    current_block_idx: int,
    next_id: int,
    has_function: bool,
    -- Type registry: maps type name to IRType
    types: Map[string, IRType]
}

fn ir_builder_new() -> IRBuilder {
    return IRBuilder {
        current_function: Box_null(),
        current_block_idx: -1,
        next_id: 0,
        has_function: false,
        types: Map_new()
    }
}

-- Register a type by name
fn builder_register_type(b: IRBuilder, name: string, ty: IRType) -> IRBuilder {
    let mut builder = b
    builder.types.insert(name, ty)
    return builder
}

-- Get a type by name
fn builder_get_type(b: IRBuilder, name: string) -> IRType {
    if b.types.contains(name) {
        return b.types.get(name)
    }
    return ir_type_simple(IR_TYPE_VOID())
}

-- Set current function
fn builder_set_function(b: IRBuilder, func: IRFunction) -> IRBuilder {
    let mut builder = b
    builder.current_function = Box_new(func)
    builder.has_function = true
    builder.next_id = 0
    return builder
}

-- Get the current function back (consumes the box)
fn builder_get_function(b: IRBuilder) -> IRFunction {
    return *b.current_function
}

-- Add a new basic block to the current function
fn builder_add_block(b: IRBuilder, label: string) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks.push(ir_block_new(label))
    builder.current_function = Box_new(func)
    builder.current_block_idx = func.blocks.len() - 1
    return builder
}

-- Set the insert point to a specific block by index
fn builder_set_block(b: IRBuilder, idx: int) -> IRBuilder {
    let mut builder = b
    builder.current_block_idx = idx
    return builder
}

-- Set the insert point to a block by label
fn builder_set_block_by_label(b: IRBuilder, label: string) -> IRBuilder {
    let mut builder = b
    let func = *builder.current_function
    let mut i = 0
    while i < func.blocks.len() {
        if func.blocks[i].label == label {
            builder.current_block_idx = i
            return builder
        }
        i = i + 1
    }
    return builder
}

-- Get current block index
fn builder_current_block(b: IRBuilder) -> int {
    return b.current_block_idx
}

-- Internal: add instruction and return its reference value
fn builder_add_inst(b: IRBuilder, inst: IRInst) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].instructions.push(inst)
    builder.current_function = Box_new(func)
    return builder
}

-- Allocate the next instruction ID
fn builder_next_id(b: IRBuilder) -> int {
    return b.next_id
}

fn builder_advance_id(b: IRBuilder) -> IRBuilder {
    let mut builder = b
    builder.next_id = builder.next_id + 1
    return builder
}

-- ============================================================
-- ARITHMETIC BUILDERS
-- ============================================================

fn builder_build_add(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_ADD())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_sub(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_SUB())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_mul(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MUL())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_div(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_DIV())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_mod(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MOD())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_neg(b: IRBuilder, operand: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_NEG())
    inst.operand = operand
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- COMPARISON BUILDERS
-- ============================================================

fn builder_build_cmp(b: IRBuilder, op: int, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, op)
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = "bool"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_eq(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_EQ(), lhs, rhs)
}

fn builder_build_ne(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_NE(), lhs, rhs)
}

fn builder_build_lt(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_LT(), lhs, rhs)
}

fn builder_build_le(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_LE(), lhs, rhs)
}

fn builder_build_gt(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_GT(), lhs, rhs)
}

fn builder_build_ge(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_GE(), lhs, rhs)
}

-- ============================================================
-- MEMORY BUILDERS
-- ============================================================

fn builder_build_alloca(b: IRBuilder, alloc_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = alloc_type
    inst.result_type = "ptr(" + alloc_type + ")"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_load(b: IRBuilder, ptr: IRValue, load_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LOAD())
    inst.load_ptr = ptr
    inst.load_type = load_type
    inst.result_type = load_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_store(b: IRBuilder, ptr: IRValue, value: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_STORE())
    inst.store_ptr = ptr
    inst.store_val = value
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_extract_field(b: IRBuilder, base: IRValue, field_index: int, field_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
    inst.field_base = base
    inst.field_index = field_index
    inst.field_type = field_type
    inst.result_type = field_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_insert_field(b: IRBuilder, base: IRValue, value: IRValue, field_index: int, struct_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
    inst.field_base = base
    inst.field_value = value
    inst.field_index = field_index
    inst.result_type = struct_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- CALL BUILDERS
-- ============================================================

fn builder_build_call(b: IRBuilder, callee: string, args: List[IRValue], ret_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = callee
    inst.call_args = args
    inst.result_type = ret_type
    inst.has_result = ret_type != "void"
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_call_void(b: IRBuilder, callee: string, args: List[IRValue]) -> IRBuilder {
    return builder_build_call(b, callee, args, "void")
}

fn builder_build_call_ptr(b: IRBuilder, callee: IRValue, args: List[IRValue], ret_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL_PTR())
    inst.callee_val = callee
    inst.call_args = args
    inst.result_type = ret_type
    inst.has_result = ret_type != "void"
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- CAST BUILDER
-- ============================================================

fn builder_build_cast(b: IRBuilder, value: IRValue, from_type: string, to_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = value
    inst.cast_from = from_type
    inst.cast_to = to_type
    inst.result_type = to_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- STRING BUILDERS
-- ============================================================

fn builder_build_string_concat(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_STRING_CONCAT())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = "string"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_string_eq(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_STRING_EQ())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = "bool"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- LIST BUILDERS
-- ============================================================

fn builder_build_list_new(b: IRBuilder, elem_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_NEW())
    inst.elem_type = elem_type
    inst.result_type = "list"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_list_push(b: IRBuilder, list: IRValue, elem: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_PUSH())
    inst.lhs = list
    inst.rhs = elem
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_list_get(b: IRBuilder, list: IRValue, index: IRValue, elem_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_GET())
    inst.lhs = list
    inst.rhs = index
    inst.result_type = elem_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_list_len(b: IRBuilder, list: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_LEN())
    inst.operand = list
    inst.result_type = "i64"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- MAP BUILDERS
-- ============================================================

fn builder_build_map_new(b: IRBuilder, key_type: string, val_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MAP_NEW())
    inst.key_type = key_type
    inst.val_type = val_type
    inst.result_type = "map"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_map_insert(b: IRBuilder, map: IRValue, key: IRValue, value: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MAP_INSERT())
    inst.map_val = map
    inst.map_key = key
    inst.map_value = value
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- TERMINATOR BUILDERS
-- ============================================================

fn builder_build_br(b: IRBuilder, target: string) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_br(target))
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

fn builder_build_cond_br(b: IRBuilder, cond: IRValue, true_label: string, false_label: string) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_br_cond(cond, true_label, false_label))
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

fn builder_build_ret(b: IRBuilder, value: IRValue) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_ret(value))
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

fn builder_build_ret_void(b: IRBuilder) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_ret_void())
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

-- ============================================================
-- PANIC BUILDER
-- ============================================================

fn builder_build_panic(b: IRBuilder, msg: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_PANIC())
    inst.operand = msg
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- Get the last instruction ID (the one just added)
fn builder_last_id(b: IRBuilder) -> int {
    return b.next_id - 1
}
