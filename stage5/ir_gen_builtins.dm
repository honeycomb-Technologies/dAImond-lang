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

    let line = if args.len() > 0 { args[0].line } else { 0 }
    return expr_result(ir_type_error(gen, "unknown SIMD builtin '" + fn_name + "'", line), block, 0)
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

    -- The vector type is the first argument's
    let mut vec_type = infer_simd_arg_type(g, args[0])
    if vec_type == "" {
        g = ir_type_error(g, "the arguments of a SIMD operation must be SIMD vectors, such as f32x4", args[0].line)
        vec_type = "f32x4"
    }

    -- Emit the SIMD binary instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, op)
    inst.lhs = ir_val_inst(lhs_result.val_id)
    inst.rhs = ir_val_inst(rhs_result.val_id)
    inst.simd_vec_type = vec_type
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

    -- The vector type is the first argument's
    let mut vec_type = infer_simd_arg_type(g, args[0])
    if vec_type == "" {
        g = ir_type_error(g, "the first argument of simd_extract must be a SIMD vector, such as f32x4", args[0].line)
        vec_type = "f32x4"
    }
    -- Use dAImond-native types (f64 for float vectors, i64 for int vectors)
    let scalar_type = if is_simd_float_type(vec_type) { "f64" } else { "i64" }

    -- The lane must be one the vector has: LLVM gives no defined value
    -- for any other
    let in_range = check_index_bounds(g, b, idx_result.val_id, ir_val_int(simd_lane_count(vec_type)), "(" + vec_type + ")")
    g = in_range.gen
    b = in_range.block

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

-- The SIMD vector type of an argument: "i32x8" for a variable, a call,
-- an element or a field of that type. "" when it is no vector.
fn infer_simd_arg_type(gen: IRGenerator, arg: Expr) -> string {
    let found = infer_expr_type(gen, arg)
    if is_simd_type(found) {
        return found
    }
    return ""
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
-- An integer as text. The value widens to 64 bits by the sign of its
-- own type, so a u8 of 200 is "200"; a u64 goes to uint_to_string,
-- which prints the values above the highest int.
fn emit_int_to_string(gen: IRGenerator, block: IRBasicBlock, val_id: int, int_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let mut wide_id = val_id
    let mut callee = "dm_int_to_string"
    if int_type == "u64" {
        callee = "dm_uint_to_string"
    } else if is_integer_type(int_type) and type_bit_width(int_type) < 64 {
        let wide = emit_ir_cast(g, b, ir_val_inst(val_id), int_type, "i64")
        g = wide.gen
        b = wide.block
        wide_id = wide.val_id
    }
    let conv_id = g.builder.next_id
    let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
    conv_inst.callee = callee
    let mut conv_args: List[IRValue] = []
    conv_args.push(ir_val_inst(wide_id))
    conv_inst.call_args = conv_args
    conv_inst.result_type = "string"
    conv_inst.has_result = true
    b.instructions.push(conv_inst)
    g.builder.next_id = conv_id + 1
    return expr_result(g, b, conv_id)
}

-- Maps the dAImond builtin name to the C runtime name and emits
-- an OP_CALL instruction with generated argument values.
fn generate_builtin_call(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let c_name = map_builtin_name(fn_name)
    let ret_type = builtin_return_type(fn_name)

    -- int_to_string takes an integer of any type
    if fn_name == "int_to_string" and args.len() == 1 {
        let n = generate_expr(g, b, args[0])
        return emit_int_to_string(n.gen, n.block, n.val_id, infer_expr_type(n.gen, args[0]))
    }

    -- Auto-convert print/println/eprint/eprintln args to string
    let is_print_fn = fn_name == "println" or fn_name == "print" or fn_name == "eprintln" or fn_name == "eprint"
    -- What the builtin's parameters take (see ck_builtin_params)
    let param_kinds = string_split(ck_builtin_params(fn_name), "|")

    -- Generate all argument expressions
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block

        if is_print_fn {
            let arg_type = infer_expr_type(g, args[i])
            if is_integer_type(arg_type) or arg_type == "int" {
                let conv = emit_int_to_string(g, b, arg_result.val_id, arg_type)
                g = conv.gen
                b = conv.block
                call_args.push(ir_val_inst(conv.val_id))
            } else if arg_type == "f64" or arg_type == "f32" or arg_type == "float" {
                -- Wrap with float_to_string, which takes a float
                let wide = emit_float_widened(g, b, arg_result.val_id, arg_type)
                g = wide.gen
                b = wide.block
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_float_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(wide.val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                call_args.push(ir_val_inst(conv_id))
            } else if arg_type == "bool" {
                -- Wrap with bool_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_bool_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(arg_result.val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                call_args.push(ir_val_inst(conv_id))
            } else {
                call_args.push(ir_val_inst(arg_result.val_id))
            }
        } else if i < param_kinds.len() and param_kinds[i] == "int" {
            -- An integer narrower than the int the builtin takes
            -- widens by the sign of its own type
            let arg_type = infer_expr_type(g, args[i])
            if is_integer_type(arg_type) and type_bit_width(arg_type) < 64 {
                let wide = emit_ir_cast(g, b, ir_val_inst(arg_result.val_id), arg_type, "i64")
                g = wide.gen
                b = wide.block
                call_args.push(ir_val_inst(wide.val_id))
            } else {
                call_args.push(ir_val_inst(arg_result.val_id))
            }
        } else if i < param_kinds.len() and param_kinds[i] == "float" {
            -- An f32 widens to the float the builtin takes
            let wide = emit_float_widened(g, b, arg_result.val_id, infer_expr_type(g, args[i]))
            g = wide.gen
            b = wide.block
            call_args.push(ir_val_inst(wide.val_id))
        } else {
            call_args.push(ir_val_inst(arg_result.val_id))
        }
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

-- assert(cond): panic with "assertion failed" unless cond is true
fn generate_assert_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() != 1 {
        g = ir_type_error(g, "assert() takes 1 argument, not " + int_to_string(args.len()), 0)
        return expr_result(g, b, 0)
    }

    let cond_result = generate_expr(g, b, args[0])
    g = cond_result.gen
    b = cond_result.block

    -- The runtime's rt_assert panics when the condition is false
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

-- assert_eq(a, b): panic unless a == b. The comparison is the ==
-- operator's, so it works for every type == does; the runtime's
-- function panics with "assert_eq failed: strings not equal" or
-- "assert_eq failed: values not equal".
fn generate_assert_eq_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() != 2 {
        g = ir_type_error(g, "assert_eq() takes 2 arguments, not " + int_to_string(args.len()), 0)
        return expr_result(g, b, 0)
    }

    let compares_strings = static_type(g, args[0]) == "string" or static_type(g, args[1]) == "string"
    let equal = expr_binary(BINOP_EQ(), args[0], args[1], args[0].line, args[0].col)
    let eq_result = generate_expr(g, b, equal)
    g = eq_result.gen
    b = eq_result.block

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = "dm_assert_eq_values"
    if compares_strings {
        inst.callee = "dm_assert_eq_strings"
    }
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(eq_result.val_id))
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
        return end_block_after_panic(g, b, id)
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

    return end_block_after_panic(g, b, id)
}

-- panic never returns: end the block with `unreachable` and continue in a
-- new block that nothing branches to, so statements after the panic are
-- not emitted after a terminator.
fn end_block_after_panic(gen: IRGenerator, block: IRBasicBlock, id: int) -> ExprResult {
    let mut g = gen
    let mut b = block
    b.terminator = Box_new(term_unreachable())
    b.has_terminator = true
    g.completed_blocks.push(b)
    let cont_label = gen_next_label(g, "after_panic")
    g = gen_advance_label(g)
    return expr_result(g, ir_block_new(cont_label), id)
}

-- Generate a Box_new(value) call.
-- Allocates the value on the heap via malloc and stores it.
fn generate_box_new_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let args = expr.args

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the value to box. An integer becomes the integer type
    -- of the box the checker found for the place the box goes to.
    let mut val_result = generate_expr(g, b, args[0])
    let box_type = checked_type(g, expr)
    if starts_with(box_type, "Box[") {
        val_result = convert_int_to_place(val_result.gen, val_result.block, val_result.val_id, args[0], extract_inner_type(box_type, "Box["), "the value of 'Box_new'")
    }
    g = val_result.gen
    b = val_result.block

    -- Allocate as many bytes as the boxed value's LLVM type needs
    let malloc_id = g.builder.next_id
    let mut malloc_inst = ir_inst_new(malloc_id, OP_CALL())
    malloc_inst.callee = "dm_libc_malloc"
    let mut margs: List[IRValue] = []
    margs.push(ir_val_sizeof_value(val_result.val_id))
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
-- Returns a null pointer constant via inttoptr(0).
fn generate_box_null_call(gen: IRGenerator, block: IRBasicBlock) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Emit inttoptr(i64 0) to produce a true null pointer
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = ir_val_int(0)
    inst.cast_from = "i64"
    inst.cast_to = "ptr"
    inst.result_type = "ptr"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- RUN COMMAND BUILTINS
-- run_command(args) and run_command_io(args, input, output, errors)
-- both call dm_run_command(list, input, output, errors); run_command
-- gives "" for the three files, which leaves the streams as they are.
-- ============================================================

fn generate_run_command_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let args = expr.args
    let callee = *expr.callee
    let fn_name = callee.name
    let takes = if fn_name == "run_command" { "1 argument" } else { "4 arguments" }
    let wanted = if fn_name == "run_command" { 1 } else { 4 }
    if args.len() != wanted {
        g = ir_type_error(g, fn_name + "() takes " + takes + ", not " + int_to_string(args.len()), expr.line)
        return expr_result(g, b, 0)
    }
    if static_type(g, args[0]) != "List[string]" {
        g = ir_type_error(g, fn_name + "() takes the program and its arguments as a List[string]", expr.line)
        return expr_result(g, b, 0)
    }
    -- The list goes to the runtime as the address of its header
    let handle = generate_list_handle(g, b, args[0])
    g = handle.gen
    b = handle.block
    let mut call_args: List[IRValue] = []
    call_args.push(handle.ptr)
    let mut i = 1
    while i < 4 {
        if i < args.len() {
            if infer_expr_type(g, args[i]) != "string" {
                g = ir_type_error(g, "argument " + int_to_string(i + 1) + " of '" + fn_name + "' is a file's path, a string", expr.line)
                return expr_result(g, b, 0)
            }
            let file = generate_expr(g, b, args[i])
            g = file.gen
            b = file.block
            call_args.push(ir_val_inst(file.val_id))
        } else {
            call_args.push(ir_val_string(""))
        }
        i = i + 1
    }
    let id = g.builder.next_id
    let mut call_inst = ir_inst_new(id, OP_CALL())
    call_inst.callee = "dm_run_command"
    call_inst.call_args = call_args
    call_inst.result_type = "i64"
    call_inst.has_result = true
    b.instructions.push(call_inst)
    g.builder.next_id = id + 1
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
