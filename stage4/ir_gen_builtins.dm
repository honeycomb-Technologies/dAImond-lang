module ir_gen_builtins

import ast
import ir
import ir_builder
import ir_gen
import ir_gen_stmt

-- ============================================================
-- SIMD Builtin IR Generation
-- Handles simd_splat_*, simd_set_*, simd_add/sub/mul/div,
-- and simd_extract intrinsic function calls.
-- Port of stage3/src/ir_gen.zig SIMD handling
-- ============================================================

-- ============================================================
-- SIMD BUILTIN DETECTION
-- ============================================================

-- Returns true if the function name is a SIMD builtin intrinsic
fn is_simd_builtin(name: string) -> bool {
    if starts_with(name, "simd_splat_") { return true }
    if starts_with(name, "simd_set_") { return true }
    if name == "simd_add" { return true }
    if name == "simd_sub" { return true }
    if name == "simd_mul" { return true }
    if name == "simd_div" { return true }
    if name == "simd_extract" { return true }
    return false
}

-- ============================================================
-- SIMD TYPE PARSING
-- ============================================================

-- Extracts the SIMD vector type name from a simd_splat_* or simd_set_* name.
-- "simd_splat_f32x4" -> "f32x4"
-- "simd_set_i32x8"   -> "i32x8"
fn parse_simd_type_from_name(name: string) -> string {
    if starts_with(name, "simd_splat_") {
        let prefix_len = len("simd_splat_")
        return substr(name, prefix_len, len(name) - prefix_len)
    }
    if starts_with(name, "simd_set_") {
        let prefix_len = len("simd_set_")
        return substr(name, prefix_len, len(name) - prefix_len)
    }
    return ""
}

-- Returns the scalar element type for a SIMD vector type.
-- f32x4 -> "f32", f32x8 -> "f32"
-- f64x2 -> "f64", f64x4 -> "f64"
-- i32x4 -> "i32", i32x8 -> "i32"
-- i64x2 -> "i64", i64x4 -> "i64"
fn simd_extract_scalar_type(vec_type: string) -> string {
    if vec_type == "f32x4" or vec_type == "f32x8" { return "f32" }
    if vec_type == "f64x2" or vec_type == "f64x4" { return "f64" }
    if vec_type == "i32x4" or vec_type == "i32x8" { return "i32" }
    if vec_type == "i64x2" or vec_type == "i64x4" { return "i64" }
    return "i32"
}

-- Returns the result type for a SIMD operation given the function name
-- and the type of the first argument.
-- For splat/set: the vector type from the function name
-- For add/sub/mul/div: same as first argument type
-- For extract: the scalar type of the vector
fn simd_result_type(fn_name: string, arg_type: string) -> string {
    -- splat and set derive their type from the function name suffix
    if starts_with(fn_name, "simd_splat_") or starts_with(fn_name, "simd_set_") {
        return parse_simd_type_from_name(fn_name)
    }
    -- Binary SIMD ops produce the same type as their inputs
    if fn_name == "simd_add" or fn_name == "simd_sub" {
        return arg_type
    }
    if fn_name == "simd_mul" or fn_name == "simd_div" {
        return arg_type
    }
    -- Extract produces the scalar element type
    if fn_name == "simd_extract" {
        return simd_extract_scalar_type(arg_type)
    }
    return arg_type
}

-- ============================================================
-- SIMD BUILTIN CODE GENERATION
-- ============================================================

-- Main dispatcher for SIMD builtin function calls.
-- Generates the appropriate SIMD IR instruction based on the function name.
fn generate_simd_builtin(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    -- simd_splat_TYPE(scalar_val)
    if starts_with(fn_name, "simd_splat_") {
        return generate_simd_splat(gen, block, fn_name, args)
    }

    -- simd_set_TYPE(a, b, c, d, ...)
    if starts_with(fn_name, "simd_set_") {
        return generate_simd_set(gen, block, fn_name, args)
    }

    -- simd_add(a, b)
    if fn_name == "simd_add" {
        return generate_simd_binary(gen, block, OP_SIMD_ADD(), args)
    }

    -- simd_sub(a, b)
    if fn_name == "simd_sub" {
        return generate_simd_binary(gen, block, OP_SIMD_SUB(), args)
    }

    -- simd_mul(a, b)
    if fn_name == "simd_mul" {
        return generate_simd_binary(gen, block, OP_SIMD_MUL(), args)
    }

    -- simd_div(a, b)
    if fn_name == "simd_div" {
        return generate_simd_binary(gen, block, OP_SIMD_DIV(), args)
    }

    -- simd_extract(vec, idx)
    if fn_name == "simd_extract" {
        return generate_simd_extract(gen, block, args)
    }

    -- Fallback: return zero
    return expr_result(gen, block, 0)
}

-- ============================================================
-- SIMD SPLAT
-- simd_splat_TYPE(scalar) -> vector with all lanes set to scalar
-- ============================================================

fn generate_simd_splat(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let vec_type = parse_simd_type_from_name(fn_name)

    -- Generate the scalar argument
    if args.len() < 1 {
        return expr_result(g, b, 0)
    }
    let scalar_result = generate_expr(g, b, args[0])
    g = scalar_result.gen
    b = scalar_result.block

    -- Emit OP_SIMD_SPLAT instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_SIMD_SPLAT())
    inst.simd_scalar = ir_val_inst(scalar_result.val_id)
    inst.simd_vec_type = vec_type
    inst.result_type = vec_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD SET
-- simd_set_TYPE(a, b, c, d, ...) -> vector from individual elements
-- ============================================================

fn generate_simd_set(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let vec_type = parse_simd_type_from_name(fn_name)

    -- Generate each element argument
    let mut elements: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let elem_result = generate_expr(g, b, args[i])
        g = elem_result.gen
        b = elem_result.block
        elements.push(ir_val_inst(elem_result.val_id))
        i = i + 1
    }

    -- Emit OP_SIMD_SET instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_SIMD_SET())
    inst.simd_elements = elements
    inst.simd_vec_type = vec_type
    inst.result_type = vec_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD BINARY OPERATIONS (add, sub, mul, div)
-- simd_op(lhs_vec, rhs_vec) -> element-wise result vector
-- ============================================================

fn generate_simd_binary(gen: IRGenerator, block: IRBasicBlock, op: int, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate left operand
    let lhs_result = generate_expr(g, b, args[0])
    g = lhs_result.gen
    b = lhs_result.block

    -- Generate right operand
    let rhs_result = generate_expr(g, b, args[1])
    g = rhs_result.gen
    b = rhs_result.block

    -- Infer the vector type from the first argument
    -- If the first arg is an identifier with a known SIMD variable type, use it
    let vec_type = infer_simd_arg_type(g, args[0])

    -- Emit the SIMD binary instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, op)
    inst.lhs = ir_val_inst(lhs_result.val_id)
    inst.rhs = ir_val_inst(rhs_result.val_id)
    inst.result_type = vec_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD EXTRACT
-- simd_extract(vector, index) -> scalar element at index
-- ============================================================

fn generate_simd_extract(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate the vector argument
    let vec_result = generate_expr(g, b, args[0])
    g = vec_result.gen
    b = vec_result.block

    -- Generate the index argument
    let idx_result = generate_expr(g, b, args[1])
    g = idx_result.gen
    b = idx_result.block

    -- Infer the vector type from the first argument
    let vec_type = infer_simd_arg_type(g, args[0])
    let scalar_type = simd_extract_scalar_type(vec_type)

    -- Emit OP_SIMD_EXTRACT instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_SIMD_EXTRACT())
    inst.simd_vector = ir_val_inst(vec_result.val_id)
    inst.simd_index = ir_val_inst(idx_result.val_id)
    inst.simd_vec_type = vec_type
    inst.result_type = scalar_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD TYPE INFERENCE HELPERS
-- ============================================================

-- Infer the SIMD vector type of an expression argument.
-- If the argument is an identifier whose type is tracked in variable_types,
-- use that. Otherwise fall back to "f32x4" as a default.
fn infer_simd_arg_type(gen: IRGenerator, arg: Expr) -> string {
    if arg.kind == EXPR_IDENTIFIER() {
        if gen.variable_types.contains(arg.name) {
            let ty = gen.variable_types.get(arg.name)
            if is_simd_type(ty) {
                return ty
            }
        }
    }
    -- If it's a function call (e.g. simd_splat_f32x4(...)), try to extract type from name
    if arg.kind == EXPR_FUNCTION_CALL() {
        let callee = *arg.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let callee_name = callee.name
            if starts_with(callee_name, "simd_splat_") or starts_with(callee_name, "simd_set_") {
                return parse_simd_type_from_name(callee_name)
            }
        }
    }
    -- Default to f32x4 if type cannot be inferred
    return "f32x4"
}

-- ============================================================
-- SIMD LANE COUNT HELPERS
-- ============================================================

-- Returns the number of lanes for a given SIMD vector type.
fn simd_lane_count(vec_type: string) -> int {
    if vec_type == "f32x4" or vec_type == "i32x4" { return 4 }
    if vec_type == "f32x8" or vec_type == "i32x8" { return 8 }
    if vec_type == "f64x2" or vec_type == "i64x2" { return 2 }
    if vec_type == "f64x4" or vec_type == "i64x4" { return 4 }
    return 4
}

-- ============================================================
-- BUILTIN CALL GENERATION HELPERS
-- ============================================================

-- Generate a simple builtin function call (non-SIMD).
-- Maps the dAImond builtin name to the C runtime name and emits
-- an OP_CALL instruction with generated argument values.
fn generate_builtin_call(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let c_name = map_builtin_name(fn_name)
    let ret_type = builtin_return_type(fn_name)

    -- Generate all argument expressions
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    -- Emit the call instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = c_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = ret_type != "void"
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate an assert call. Emits a conditional panic.
-- assert(cond) -> if !cond { panic("assertion failed") }
fn generate_assert_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the condition
    let cond_result = generate_expr(g, b, args[0])
    g = cond_result.gen
    b = cond_result.block

    -- Emit a call to dm_assert (runtime handles the panic)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = "dm_assert"
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(cond_result.val_id))
    inst.call_args = call_args
    inst.result_type = "void"
    inst.has_result = false
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate an assert_eq call.
-- assert_eq(a, b) -> if a != b { panic("values not equal") }
fn generate_assert_eq_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate both argument expressions
    let lhs_result = generate_expr(g, b, args[0])
    g = lhs_result.gen
    b = lhs_result.block

    let rhs_result = generate_expr(g, b, args[1])
    g = rhs_result.gen
    b = rhs_result.block

    -- Emit a call to dm_assert_eq (runtime handles the comparison + panic)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = "dm_assert_eq"
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(lhs_result.val_id))
    call_args.push(ir_val_inst(rhs_result.val_id))
    inst.call_args = call_args
    inst.result_type = "void"
    inst.has_result = false
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate a panic call.
-- panic(msg) -> dm_panic(msg); unreachable
fn generate_panic_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        -- Panic with no message
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_PANIC())
        inst.operand = ir_val_string("panic")
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- Generate the message expression
    let msg_result = generate_expr(g, b, args[0])
    g = msg_result.gen
    b = msg_result.block

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_PANIC())
    inst.operand = ir_val_inst(msg_result.val_id)
    inst.has_result = false
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate a Box_new(value) call.
-- Allocates the value on the heap via malloc and stores it.
fn generate_box_new_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the value to box
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Allocate memory via malloc
    let size = 8  -- default pointer-sized allocation
    let malloc_id = g.builder.next_id
    let mut malloc_inst = ir_inst_new(malloc_id, OP_CALL())
    malloc_inst.callee = "malloc"
    let mut margs: List[IRValue] = []
    margs.push(ir_val_int(size))
    malloc_inst.call_args = margs
    malloc_inst.result_type = "ptr"
    malloc_inst.has_result = true
    b.instructions.push(malloc_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Store the value into the allocated memory
    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
    store_inst.store_ptr = ir_val_inst(malloc_id)
    store_inst.store_val = ir_val_inst(val_result.val_id)
    store_inst.has_result = false
    b.instructions.push(store_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, malloc_id)
}

-- Generate a Box_null() call.
-- Returns a null pointer constant.
fn generate_box_null_call(gen: IRGenerator, block: IRBasicBlock) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Emit an alloca with null value (pointer set to 0)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = "malloc"
    let mut margs: List[IRValue] = []
    margs.push(ir_val_int(0))
    inst.call_args = margs
    inst.result_type = "ptr"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- STRING SPLIT BUILTIN
-- string_split(s, delim) returns List[string]
-- Needs special handling: allocates a list, calls dm_string_split
-- ============================================================

fn generate_string_split_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate source string
    let str_result = generate_expr(g, b, args[0])
    g = str_result.gen
    b = str_result.block

    -- Generate delimiter string
    let delim_result = generate_expr(g, b, args[1])
    g = delim_result.gen
    b = delim_result.block

    -- Allocate a list to receive the results
    let list_alloca_id = g.builder.next_id
    let mut list_inst = ir_inst_new(list_alloca_id, OP_ALLOCA())
    list_inst.alloc_type = "dm_list_dm_string"
    list_inst.result_type = "ptr(dm_list_dm_string)"
    list_inst.has_result = true
    b.instructions.push(list_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Call dm_string_split(list_ptr, str, delim)
    let id = g.builder.next_id
    let mut call_inst = ir_inst_new(id, OP_CALL())
    call_inst.callee = "dm_string_split"
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(list_alloca_id))
    call_args.push(ir_val_inst(str_result.val_id))
    call_args.push(ir_val_inst(delim_result.val_id))
    call_inst.call_args = call_args
    call_inst.result_type = "void"
    call_inst.has_result = false
    b.instructions.push(call_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, list_alloca_id)
}
