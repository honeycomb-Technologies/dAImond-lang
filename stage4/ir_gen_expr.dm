module ir_gen_expr

import ast
import ir
import ir_builder
import ir_gen
import ir_gen_stmt

-- ============================================================
-- EXPRESSION IR GENERATION
-- Main dispatcher and all expression kind handlers
-- Port of stage3/src/ir_gen.zig generateExpr
-- ============================================================

fn generate_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    if expr.kind == EXPR_LITERAL_INT() {
        return generate_literal_int(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_FLOAT() {
        return generate_literal_float(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_STRING() {
        return generate_literal_string(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_BOOL() {
        return generate_literal_bool(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_NULL() {
        return generate_literal_null(gen, block)
    }
    if expr.kind == EXPR_IDENTIFIER() {
        return generate_identifier(gen, block, expr)
    }
    if expr.kind == EXPR_BINARY() {
        return generate_binary(gen, block, expr)
    }
    if expr.kind == EXPR_UNARY() {
        return generate_unary(gen, block, expr)
    }
    if expr.kind == EXPR_FUNCTION_CALL() {
        return generate_function_call(gen, block, expr)
    }
    if expr.kind == EXPR_METHOD_CALL() {
        return generate_method_call(gen, block, expr)
    }
    if expr.kind == EXPR_FIELD_ACCESS() {
        return generate_field_access(gen, block, expr)
    }
    if expr.kind == EXPR_INDEX_ACCESS() {
        return generate_index_access(gen, block, expr)
    }
    if expr.kind == EXPR_STRUCT_LITERAL() {
        return generate_struct_literal(gen, block, expr)
    }
    if expr.kind == EXPR_ENUM_LITERAL() {
        return generate_enum_literal(gen, block, expr)
    }
    if expr.kind == EXPR_ARRAY_LITERAL() {
        return generate_array_literal(gen, block, expr)
    }
    if expr.kind == EXPR_IF() {
        return generate_if_expr(gen, block, expr)
    }
    if expr.kind == EXPR_MATCH() {
        return generate_match_expr(gen, block, expr)
    }
    if expr.kind == EXPR_BLOCK() {
        return generate_block_expr(gen, block, expr)
    }
    if expr.kind == EXPR_LAMBDA() {
        return generate_lambda_expr(gen, block, expr)
    }
    if expr.kind == EXPR_PIPELINE() {
        return generate_pipeline(gen, block, expr)
    }
    if expr.kind == EXPR_ERROR_PROPAGATE() {
        return generate_error_propagate(gen, block, expr)
    }
    if expr.kind == EXPR_CAST() {
        return generate_cast_expr(gen, block, expr)
    }
    if expr.kind == EXPR_STRING_INTERP() {
        return generate_string_interp(gen, block, expr)
    }
    if expr.kind == EXPR_GROUPED() {
        return generate_expr(gen, block, *expr.operand)
    }
    if expr.kind == EXPR_COMPTIME() {
        return generate_comptime_expr(gen, block, expr)
    }
    if expr.kind == EXPR_AWAIT() {
        -- Phase A synchronous semantics: await simply evaluates the inner expression
        return generate_expr(gen, block, *expr.operand)
    }
    -- Default: return undef
    let id = gen.builder.next_id
    let mut g = gen
    let mut b = block
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- ============================================================
-- LITERALS
-- ============================================================

fn generate_literal_int(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_int(expr.int_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "i64"
    load.result_type = "i64"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_float(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "f64"
    inst.result_type = "ptr(f64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_float(expr.float_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "f64"
    load.result_type = "f64"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_string(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "string"
    inst.result_type = "ptr(string)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_string(expr.str_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "string"
    load.result_type = "string"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_bool(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "bool"
    inst.result_type = "ptr(bool)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_bool(expr.bool_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "bool"
    load.result_type = "bool"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_null(gen: IRGenerator, block: IRBasicBlock) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "ptr"
    inst.result_type = "ptr(ptr)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_int(0)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- IDENTIFIER
-- ============================================================

fn generate_identifier(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let name = expr.name

    -- Check for "true" / "false" literals
    if name == "true" {
        return generate_literal_bool(g, b, expr_bool(true, 0, 0))
    }
    if name == "false" {
        return generate_literal_bool(g, b, expr_bool(false, 0, 0))
    }

    -- Check for "None" (Option None)
    if name == "None" {
        -- Build Option { has_value: 0, value: undef }
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_ALLOCA())
        inst.alloc_type = "i64"
        inst.result_type = "ptr(i64)"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1

        let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
        store.store_ptr = ir_val_inst(id)
        store.store_val = ir_val_int(0)
        store.has_result = false
        b.instructions.push(store)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- Look up variable in scope
    if g.variable_map.contains(name) {
        let alloca_id = g.variable_map.get(name)
        let var_type = if g.variable_types.contains(name) {
            g.variable_types.get(name)
        } else {
            "i64"
        }

        let load_id = g.builder.next_id
        let mut load = ir_inst_new(load_id, OP_LOAD())
        load.load_ptr = ir_val_inst(alloca_id)
        load.load_type = var_type
        load.result_type = var_type
        load.has_result = true
        b.instructions.push(load)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, load_id)
    }

    -- Unknown identifier - return undef
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- ============================================================
-- EXPRESSION TYPE INFERENCE
-- Determines the IR type name for an expression without
-- generating instructions. Used to select the correct opcode
-- family (integer, float, string) for binary operations.
-- Port of stage3/src/ir_gen.zig inferExprType
-- ============================================================

fn infer_expr_type(gen: IRGenerator, expr: Expr) -> string {
    if expr.kind == EXPR_LITERAL_INT() {
        return "i64"
    }
    if expr.kind == EXPR_LITERAL_FLOAT() {
        return "f64"
    }
    if expr.kind == EXPR_LITERAL_STRING() {
        return "string"
    }
    if expr.kind == EXPR_LITERAL_BOOL() {
        return "bool"
    }
    if expr.kind == EXPR_IDENTIFIER() {
        let name = expr.name
        if name == "true" or name == "false" {
            return "bool"
        }
        if gen.variable_types.contains(name) {
            return gen.variable_types.get(name)
        }
        return "i64"
    }
    if expr.kind == EXPR_BINARY() {
        let op = expr.op
        -- Comparison operators always produce bool
        if op == BINOP_EQ() or op == BINOP_NE() {
            return "bool"
        }
        if op == BINOP_LT() or op == BINOP_LE() or op == BINOP_GT() or op == BINOP_GE() {
            return "bool"
        }
        -- Logical operators always produce bool
        if op == BINOP_AND() or op == BINOP_OR() {
            return "bool"
        }
        -- Arithmetic: infer from left operand
        let left_type = infer_expr_type(gen, *expr.left)
        if left_type == "string" and op == BINOP_ADD() {
            return "string"
        }
        return left_type
    }
    if expr.kind == EXPR_UNARY() {
        if expr.op == UNOP_NOT() {
            return "bool"
        }
        -- Negation preserves the operand type
        return infer_expr_type(gen, *expr.operand)
    }
    if expr.kind == EXPR_CAST() {
        return map_type_name(expr.cast_type)
    }
    if expr.kind == EXPR_FUNCTION_CALL() {
        -- Check if callee is an identifier with a known builtin return type
        let callee = *expr.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let fn_name = callee.name
            if is_builtin_fn(fn_name) {
                return builtin_return_type(fn_name)
            }
        }
        return "i64"
    }
    if expr.kind == EXPR_STRING_INTERP() {
        return "string"
    }
    if expr.kind == EXPR_GROUPED() {
        return infer_expr_type(gen, *expr.operand)
    }
    if expr.kind == EXPR_AWAIT() {
        -- Await returns the type of the inner expression
        return infer_expr_type(gen, *expr.operand)
    }
    -- Default fallback
    return "i64"
}

-- ============================================================
-- BINARY EXPRESSION IR GENERATION
-- Handles arithmetic, comparison, logical, and string operators
-- Port of stage3/src/ir_gen.zig generateBinary
-- ============================================================

fn generate_binary(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let op = expr.op

    -- --------------------------------------------------------
    -- SHORT-CIRCUIT LOGICAL AND
    -- lhs && rhs: if lhs is false, result is false; else result is rhs
    -- --------------------------------------------------------
    if op == BINOP_AND() {
        -- Allocate a result slot for the bool
        let result_alloca_id = g.builder.next_id
        let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
        result_alloca.alloc_type = "bool"
        result_alloca.result_type = "ptr(bool)"
        result_alloca.has_result = true
        b.instructions.push(result_alloca)
        g.builder.next_id = g.builder.next_id + 1

        -- Evaluate left operand
        let lhs_result = generate_expr(g, b, *expr.left)
        g = lhs_result.gen
        b = lhs_result.block
        let lhs_id = lhs_result.val_id

        -- Labels for the branches
        let rhs_label = gen_next_label(g, "and_rhs")
        g = gen_advance_label(g)
        let short_label = gen_next_label(g, "and_short")
        g = gen_advance_label(g)
        let join_label = gen_next_label(g, "and_join")
        g = gen_advance_label(g)

        -- Branch: if lhs is true, evaluate rhs; else short-circuit to false
        b.terminator = Box_new(term_br_cond(ir_val_inst(lhs_id), rhs_label, short_label))
        b.has_terminator = true

        -- RHS block: evaluate rhs, store result, branch to join
        let mut rhs_block = ir_block_new(rhs_label)
        let rhs_result = generate_expr(g, rhs_block, *expr.right)
        g = rhs_result.gen
        rhs_block = rhs_result.block
        let rhs_id = rhs_result.val_id

        let mut store_rhs = ir_inst_new(g.builder.next_id, OP_STORE())
        store_rhs.store_ptr = ir_val_inst(result_alloca_id)
        store_rhs.store_val = ir_val_inst(rhs_id)
        store_rhs.has_result = false
        rhs_block.instructions.push(store_rhs)
        g.builder.next_id = g.builder.next_id + 1

        rhs_block.terminator = Box_new(term_br(join_label))
        rhs_block.has_terminator = true

        -- Short-circuit block: store false, branch to join
        let mut short_block = ir_block_new(short_label)
        let mut store_false = ir_inst_new(g.builder.next_id, OP_STORE())
        store_false.store_ptr = ir_val_inst(result_alloca_id)
        store_false.store_val = ir_val_bool(false)
        store_false.has_result = false
        short_block.instructions.push(store_false)
        g.builder.next_id = g.builder.next_id + 1

        short_block.terminator = Box_new(term_br(join_label))
        short_block.has_terminator = true

        -- Join block: load the result
        let mut join_block = ir_block_new(join_label)
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(result_alloca_id)
        load_inst.load_type = "bool"
        load_inst.result_type = "bool"
        load_inst.has_result = true
        join_block.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, join_block, load_id)
    }

    -- --------------------------------------------------------
    -- SHORT-CIRCUIT LOGICAL OR
    -- lhs || rhs: if lhs is true, result is true; else result is rhs
    -- --------------------------------------------------------
    if op == BINOP_OR() {
        -- Allocate a result slot for the bool
        let result_alloca_id = g.builder.next_id
        let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
        result_alloca.alloc_type = "bool"
        result_alloca.result_type = "ptr(bool)"
        result_alloca.has_result = true
        b.instructions.push(result_alloca)
        g.builder.next_id = g.builder.next_id + 1

        -- Evaluate left operand
        let lhs_result = generate_expr(g, b, *expr.left)
        g = lhs_result.gen
        b = lhs_result.block
        let lhs_id = lhs_result.val_id

        -- Labels for the branches
        let short_label = gen_next_label(g, "or_short")
        g = gen_advance_label(g)
        let rhs_label = gen_next_label(g, "or_rhs")
        g = gen_advance_label(g)
        let join_label = gen_next_label(g, "or_join")
        g = gen_advance_label(g)

        -- Branch: if lhs is true, short-circuit to true; else evaluate rhs
        b.terminator = Box_new(term_br_cond(ir_val_inst(lhs_id), short_label, rhs_label))
        b.has_terminator = true

        -- Short-circuit block: store true, branch to join
        let mut short_block = ir_block_new(short_label)
        let mut store_true = ir_inst_new(g.builder.next_id, OP_STORE())
        store_true.store_ptr = ir_val_inst(result_alloca_id)
        store_true.store_val = ir_val_bool(true)
        store_true.has_result = false
        short_block.instructions.push(store_true)
        g.builder.next_id = g.builder.next_id + 1

        short_block.terminator = Box_new(term_br(join_label))
        short_block.has_terminator = true

        -- RHS block: evaluate rhs, store result, branch to join
        let mut rhs_block = ir_block_new(rhs_label)
        let rhs_result = generate_expr(g, rhs_block, *expr.right)
        g = rhs_result.gen
        rhs_block = rhs_result.block
        let rhs_id = rhs_result.val_id

        let mut store_rhs = ir_inst_new(g.builder.next_id, OP_STORE())
        store_rhs.store_ptr = ir_val_inst(result_alloca_id)
        store_rhs.store_val = ir_val_inst(rhs_id)
        store_rhs.has_result = false
        rhs_block.instructions.push(store_rhs)
        g.builder.next_id = g.builder.next_id + 1

        rhs_block.terminator = Box_new(term_br(join_label))
        rhs_block.has_terminator = true

        -- Join block: load the result
        let mut join_block = ir_block_new(join_label)
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(result_alloca_id)
        load_inst.load_type = "bool"
        load_inst.result_type = "bool"
        load_inst.has_result = true
        join_block.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, join_block, load_id)
    }

    -- --------------------------------------------------------
    -- EAGER BINARY OPERATORS (arithmetic, comparison, string)
    -- Evaluate both operands first, then emit the operation
    -- --------------------------------------------------------

    -- Evaluate left operand
    let lhs_result = generate_expr(g, b, *expr.left)
    g = lhs_result.gen
    b = lhs_result.block
    let lhs_id = lhs_result.val_id

    -- Evaluate right operand
    let rhs_result = generate_expr(g, b, *expr.right)
    g = rhs_result.gen
    b = rhs_result.block
    let rhs_id = rhs_result.val_id

    -- Infer the type of the left operand to select the correct op family
    let lhs_type = infer_expr_type(g, *expr.left)
    let rhs_type = infer_expr_type(g, *expr.right)

    -- Determine if either operand is float or string
    let is_string = lhs_type == "string" or rhs_type == "string"
    let is_float = is_float_type(lhs_type) or is_float_type(rhs_type)

    -- Determine the arithmetic result type
    let arith_type = if is_float { "f64" } else { "i64" }

    -- --------------------------------------------------------
    -- STRING OPERATIONS
    -- --------------------------------------------------------
    if is_string {
        -- String concatenation with +
        if op == BINOP_ADD() {
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_STRING_CONCAT())
            inst.lhs = ir_val_inst(lhs_id)
            inst.rhs = ir_val_inst(rhs_id)
            inst.result_type = "string"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }

        -- String equality ==
        if op == BINOP_EQ() {
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_STRING_EQ())
            inst.lhs = ir_val_inst(lhs_id)
            inst.rhs = ir_val_inst(rhs_id)
            inst.result_type = "bool"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }

        -- String inequality !=
        if op == BINOP_NE() {
            -- First compute string equality
            let eq_id = g.builder.next_id
            let mut eq_inst = ir_inst_new(eq_id, OP_STRING_EQ())
            eq_inst.lhs = ir_val_inst(lhs_id)
            eq_inst.rhs = ir_val_inst(rhs_id)
            eq_inst.result_type = "bool"
            eq_inst.has_result = true
            b.instructions.push(eq_inst)
            g.builder.next_id = eq_id + 1

            -- Then negate it
            let not_id = g.builder.next_id
            let mut not_inst = ir_inst_new(not_id, OP_LOGICAL_NOT())
            not_inst.operand = ir_val_inst(eq_id)
            not_inst.result_type = "bool"
            not_inst.has_result = true
            b.instructions.push(not_inst)
            g.builder.next_id = not_id + 1
            return expr_result(g, b, not_id)
        }

        -- String ordering comparisons (<, <=, >, >=) via dm_string_cmp
        if op == BINOP_LT() or op == BINOP_LE() or op == BINOP_GT() or op == BINOP_GE() {
            -- Call dm_string_cmp(lhs, rhs) -> i64
            let cmp_call_id = g.builder.next_id
            let mut cmp_call = ir_inst_new(cmp_call_id, OP_CALL())
            cmp_call.callee = "dm_string_cmp"
            let mut cmp_args: List[IRValue] = []
            cmp_args.push(ir_val_inst(lhs_id))
            cmp_args.push(ir_val_inst(rhs_id))
            cmp_call.call_args = cmp_args
            cmp_call.result_type = "i64"
            cmp_call.has_result = true
            b.instructions.push(cmp_call)
            g.builder.next_id = cmp_call_id + 1

            -- Compare the cmp result to 0
            let cmp_op = if op == BINOP_LT() { OP_LT() }
            else if op == BINOP_LE() { OP_LE() }
            else if op == BINOP_GT() { OP_GT() }
            else { OP_GE() }

            let result_id = g.builder.next_id
            let mut cmp_inst = ir_inst_new(result_id, cmp_op)
            cmp_inst.lhs = ir_val_inst(cmp_call_id)
            cmp_inst.rhs = ir_val_int(0)
            cmp_inst.result_type = "bool"
            cmp_inst.has_result = true
            b.instructions.push(cmp_inst)
            g.builder.next_id = result_id + 1
            return expr_result(g, b, result_id)
        }
    }

    -- --------------------------------------------------------
    -- ARITHMETIC OPERATORS (+, -, *, /, %)
    -- --------------------------------------------------------
    if op == BINOP_ADD() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_ADD())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_SUB() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_SUB())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_MUL() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_MUL())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_DIV() {
        -- Safe division: check for zero divisor before dividing
        -- Compare rhs == 0
        let zero_val = if is_float { ir_val_float(0.0) } else { ir_val_int(0) }
        let zero_check_id = g.builder.next_id
        let mut zero_check = ir_inst_new(zero_check_id, OP_EQ())
        zero_check.lhs = ir_val_inst(rhs_id)
        zero_check.rhs = zero_val
        zero_check.result_type = "bool"
        zero_check.has_result = true
        b.instructions.push(zero_check)
        g.builder.next_id = zero_check_id + 1

        let panic_label = gen_next_label(g, "div_panic")
        g = gen_advance_label(g)
        let ok_label = gen_next_label(g, "div_ok")
        g = gen_advance_label(g)

        -- Branch: if zero, panic; else continue
        b.terminator = Box_new(term_br_cond(ir_val_inst(zero_check_id), panic_label, ok_label))
        b.has_terminator = true
        g.completed_blocks.push(b)

        -- Panic block: call dm_panic, then unreachable
        let mut panic_block = ir_block_new(panic_label)
        let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        panic_inst.callee = "dm_panic"
        let mut panic_args: List[IRValue] = []
        panic_args.push(ir_val_string("Runtime error: division by zero"))
        panic_inst.call_args = panic_args
        panic_inst.result_type = "void"
        panic_inst.has_result = false
        panic_block.instructions.push(panic_inst)
        g.builder.next_id = g.builder.next_id + 1

        panic_block.terminator = Box_new(term_unreachable())
        panic_block.has_terminator = true
        g.completed_blocks.push(panic_block)

        -- OK block: perform the division
        let mut ok_block = ir_block_new(ok_label)
        let div_id = g.builder.next_id
        let mut div_inst = ir_inst_new(div_id, OP_DIV())
        div_inst.lhs = ir_val_inst(lhs_id)
        div_inst.rhs = ir_val_inst(rhs_id)
        div_inst.result_type = arith_type
        div_inst.has_result = true
        ok_block.instructions.push(div_inst)
        g.builder.next_id = div_id + 1

        return expr_result(g, ok_block, div_id)
    }

    if op == BINOP_MOD() {
        -- Safe modulo: check for zero divisor before computing
        let zero_val = if is_float { ir_val_float(0.0) } else { ir_val_int(0) }
        let zero_check_id = g.builder.next_id
        let mut zero_check = ir_inst_new(zero_check_id, OP_EQ())
        zero_check.lhs = ir_val_inst(rhs_id)
        zero_check.rhs = zero_val
        zero_check.result_type = "bool"
        zero_check.has_result = true
        b.instructions.push(zero_check)
        g.builder.next_id = zero_check_id + 1

        let panic_label = gen_next_label(g, "mod_panic")
        g = gen_advance_label(g)
        let ok_label = gen_next_label(g, "mod_ok")
        g = gen_advance_label(g)

        b.terminator = Box_new(term_br_cond(ir_val_inst(zero_check_id), panic_label, ok_label))
        b.has_terminator = true
        g.completed_blocks.push(b)

        -- Panic block
        let mut panic_block = ir_block_new(panic_label)
        let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        panic_inst.callee = "dm_panic"
        let mut panic_args: List[IRValue] = []
        panic_args.push(ir_val_string("Runtime error: modulo by zero"))
        panic_inst.call_args = panic_args
        panic_inst.result_type = "void"
        panic_inst.has_result = false
        panic_block.instructions.push(panic_inst)
        g.builder.next_id = g.builder.next_id + 1

        panic_block.terminator = Box_new(term_unreachable())
        panic_block.has_terminator = true
        g.completed_blocks.push(panic_block)

        -- OK block
        let mut ok_block = ir_block_new(ok_label)
        let mod_id = g.builder.next_id
        let mut mod_inst = ir_inst_new(mod_id, OP_MOD())
        mod_inst.lhs = ir_val_inst(lhs_id)
        mod_inst.rhs = ir_val_inst(rhs_id)
        mod_inst.result_type = arith_type
        mod_inst.has_result = true
        ok_block.instructions.push(mod_inst)
        g.builder.next_id = mod_id + 1

        return expr_result(g, ok_block, mod_id)
    }

    -- --------------------------------------------------------
    -- COMPARISON OPERATORS (==, !=, <, <=, >, >=)
    -- --------------------------------------------------------
    if op == BINOP_EQ() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_EQ())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_NE() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_NE())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_LT() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LT())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_LE() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LE())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_GT() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_GT())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_GE() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_GE())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- --------------------------------------------------------
    -- BITWISE OPERATORS (&, |, ^, <<, >>)
    -- --------------------------------------------------------
    if op == BINOP_BIT_AND() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_AND())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_BIT_OR() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_OR())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_BIT_XOR() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_XOR())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_SHL() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_SHL())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_SHR() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_SHR())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Fallback: unsupported binary operator, return lhs
    return expr_result(g, b, lhs_id)
}

-- ============================================================
-- UNARY EXPRESSION IR GENERATION
-- Handles negation (-) and logical not (!)
-- Port of stage3/src/ir_gen.zig generateUnary
-- ============================================================

fn generate_unary(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Evaluate the operand expression
    let operand_result = generate_expr(g, b, *expr.operand)
    g = operand_result.gen
    b = operand_result.block
    let operand_id = operand_result.val_id

    if expr.op == UNOP_NEG() {
        -- Negate: emit OP_NEG with operand's type
        let operand_type = infer_expr_type(g, *expr.operand)
        let result_type = if is_float_type(operand_type) { "f64" } else { "i64" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_NEG())
        inst.operand = ir_val_inst(operand_id)
        inst.result_type = result_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if expr.op == UNOP_NOT() {
        -- Logical NOT: emit OP_LOGICAL_NOT, result is always bool
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LOGICAL_NOT())
        inst.operand = ir_val_inst(operand_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if expr.op == UNOP_BIT_NOT() {
        -- Bitwise NOT: emit OP_BIT_XOR with all-ones mask (XOR with -1)
        -- ~x is equivalent to x ^ (-1) for two's complement integers
        let operand_type = infer_expr_type(g, *expr.operand)
        let result_type = if is_float_type(operand_type) { "f64" } else { "i64" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_XOR())
        inst.lhs = ir_val_inst(operand_id)
        inst.rhs = ir_val_int(-1)
        inst.result_type = result_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Unknown unary operator, return operand unchanged
    return expr_result(g, b, operand_id)
}

-- ============================================================
-- CAST EXPRESSION IR GENERATION
-- Handles explicit type casts via `expr as Type`
-- Port of stage3/src/ir_gen.zig generateCast
-- ============================================================

fn generate_cast_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Evaluate the operand expression
    let operand_result = generate_expr(g, b, *expr.operand)
    g = operand_result.gen
    b = operand_result.block
    let operand_id = operand_result.val_id

    -- Determine source type by inference
    let from_type = infer_expr_type(g, *expr.operand)

    -- Determine target type from the cast annotation, mapped to IR names
    let to_type = map_type_name(expr.cast_type)

    -- If the types are the same, no cast needed
    if from_type == to_type {
        return expr_result(g, b, operand_id)
    }

    -- Emit OP_CAST instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = ir_val_inst(operand_id)
    inst.cast_from = from_type
    inst.cast_to = to_type
    inst.result_type = to_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- FUNCTION CALL IR GENERATION
-- Dispatches function calls to builtins, special constructors,
-- generic monomorphization, indirect (lambda) calls, and
-- regular user-defined function calls.
-- Port of stage3/src/ir_gen.zig generateCall
-- ============================================================

fn generate_function_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get the callee name from the callee expression (should be an identifier)
    let callee_expr = *expr.callee
    let fn_name = callee_expr.name

    -- --------------------------------------------------------
    -- Special case dispatch (in priority order)
    -- --------------------------------------------------------

    -- SIMD builtins (simd_splat_*, simd_set_*, simd_add, etc.)
    if is_simd_builtin(fn_name) {
        return generate_simd_builtin(g, b, fn_name, expr.args)
    }

    -- assert(cond)
    if fn_name == "assert" {
        return generate_assert_call(g, b, expr.args)
    }

    -- assert_eq(a, b)
    if fn_name == "assert_eq" {
        return generate_assert_eq_call(g, b, expr.args)
    }

    -- panic(msg)
    if fn_name == "panic" {
        return generate_panic_call(g, b, expr.args)
    }

    -- Box_new(value)
    if fn_name == "Box_new" {
        return generate_box_new_call(g, b, expr.args)
    }

    -- Box_null()
    if fn_name == "Box_null" {
        return generate_box_null_call(g, b)
    }

    -- string_split(s, delim) -> List[string]
    if fn_name == "string_split" {
        return generate_string_split_call(g, b, expr.args)
    }

    -- Some(value) -> Option with has_value=1
    if fn_name == "Some" {
        return generate_some_call(g, b, expr.args)
    }

    -- Ok(value) -> Result with tag=0
    if fn_name == "Ok" {
        return generate_ok_call(g, b, expr.args)
    }

    -- Err(value) -> Result with tag=1
    if fn_name == "Err" {
        return generate_err_call(g, b, expr.args)
    }

    -- Map_new() is handled at the let-binding level; return a placeholder
    if fn_name == "Map_new" {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_ALLOCA())
        inst.alloc_type = "i64"
        inst.result_type = "ptr(i64)"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Explicit generic type arguments: fn_name[T](args)
    if expr.generic_args.len() > 0 {
        return generate_generic_call(g, b, expr)
    }

    -- Implicit generic: check if callee is registered as a generic function
    if g.generic_fn_decls.contains(fn_name) {
        return generate_generic_call(g, b, expr)
    }

    -- Indirect call via function pointer (lambda variable)
    if g.fn_ptr_vars.contains(fn_name) {
        return generate_lambda_indirect_call(g, b, fn_name, expr.args)
    }

    -- len() on a list variable: dispatch to typed list_len runtime function
    if fn_name == "len" and expr.args.len() > 0 {
        let first_arg = expr.args[0]
        if first_arg.kind == EXPR_IDENTIFIER() {
            let arg_name = first_arg.name
            if g.list_elem_kinds.contains(arg_name) {
                let ek = g.list_elem_kinds.get(arg_name)
                let list_ptr = g.variable_map.get(arg_name)

                let len_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_len" }
                else if ek == LIST_ELEM_FLOAT() { "dm_list_double_len" }
                else if ek == LIST_ELEM_STRING() { "dm_list_string_len" }
                else { "dm_list_generic_len" }

                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = len_fn
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(list_ptr))
                inst.call_args = call_args
                inst.result_type = "i64"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, id)
            }
        }
    }

    -- Builtin function (println, len, int_to_string, etc.)
    if is_builtin_fn(fn_name) {
        return generate_builtin_call(g, b, fn_name, expr.args)
    }

    -- --------------------------------------------------------
    -- Default: regular user-defined function call or extern call
    -- --------------------------------------------------------
    return generate_normal_call(g, b, fn_name, expr.args)
}

-- ============================================================
-- NORMAL (USER-DEFINED / EXTERN) FUNCTION CALL
-- Evaluates all argument expressions, then emits OP_CALL.
-- Handles mut param pass-by-reference and extern functions.
-- ============================================================

fn generate_normal_call(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Check if this function has mut params (pass-by-reference)
    let has_mut_params = g.mut_param_fns.contains(fn_name)
    let mut_flags_str = if has_mut_params {
        g.mut_param_fns.get(fn_name)
    } else {
        ""
    }

    -- Split mut flags into a list for per-argument checking
    let mut mut_flags_list: List[string] = []
    if has_mut_params {
        mut_flags_list = string_split(mut_flags_str, "|")
    }

    -- Generate all argument expressions
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        -- Check if this arg position is a mut parameter
        let is_mut_arg = if i < mut_flags_list.len() {
            mut_flags_list[i] == "1"
        } else {
            false
        }

        if is_mut_arg and args[i].kind == EXPR_IDENTIFIER() {
            -- Pass the alloca pointer directly (pass-by-reference) for mut params
            let arg_name = args[i].name
            if g.variable_map.contains(arg_name) {
                let ptr_id = g.variable_map.get(arg_name)
                call_args.push(ir_val_inst(ptr_id))
            } else {
                -- Fallback: evaluate normally
                let arg_result = generate_expr(g, b, args[i])
                g = arg_result.gen
                b = arg_result.block
                call_args.push(ir_val_inst(arg_result.val_id))
            }
        } else {
            let arg_result = generate_expr(g, b, args[i])
            g = arg_result.gen
            b = arg_result.block
            call_args.push(ir_val_inst(arg_result.val_id))
        }
        i = i + 1
    }

    -- Determine the callee name for the call instruction
    -- For extern functions, use the function name directly
    -- For user functions, also use the function name directly
    let callee_name = fn_name

    -- Determine return type by looking up registered functions in the module
    let ret_type = infer_call_return_type(g, callee_name)
    let has_result = ret_type != "void"

    -- Emit the OP_CALL instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = callee_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- INFER CALL RETURN TYPE
-- Looks up the return type of a function by name in the module.
-- Falls back to "i64" for unknown functions.
-- ============================================================

fn infer_call_return_type(gen: IRGenerator, fn_name: string) -> string {
    -- Check builtins first
    if is_builtin_fn(fn_name) {
        return builtin_return_type(fn_name)
    }

    -- Search module functions for the declared return type
    let mut i = 0
    while i < gen.ir_mod.functions.len() {
        let f = gen.ir_mod.functions[i]
        if f.name == fn_name {
            return f.return_type
        }
        i = i + 1
    }

    -- Unknown function: assume i64 return
    return "i64"
}

-- ============================================================
-- LAMBDA / FUNCTION POINTER INDIRECT CALL
-- Calls a lambda stored in fn_ptr_vars.
-- Passes captured variables as additional arguments.
-- ============================================================

fn generate_lambda_indirect_call(gen: IRGenerator, block: IRBasicBlock, var_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get the actual lambda function name
    let lambda_name = g.fn_ptr_vars.get(var_name)

    -- Generate explicit arguments
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    -- Add captured variable values as extra arguments
    if g.lambda_captures.contains(lambda_name) {
        let captures_str = g.lambda_captures.get(lambda_name)
        let capture_names = string_split(captures_str, "|")
        let mut ci = 0
        while ci < capture_names.len() {
            let cap_name = capture_names[ci]
            if len(cap_name) > 0 and g.variable_map.contains(cap_name) {
                let cap_alloca = g.variable_map.get(cap_name)
                let cap_type = if g.variable_types.contains(cap_name) {
                    g.variable_types.get(cap_name)
                } else {
                    "i64"
                }

                -- Load the captured variable's current value
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(cap_alloca)
                load_inst.load_type = cap_type
                load_inst.result_type = cap_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1

                call_args.push(ir_val_inst(load_id))
            } else {
                -- Captured variable not found: pass zero
                call_args.push(ir_val_int(0))
            }
            ci = ci + 1
        }
    }

    -- Look up the return type of the lambda function
    let ret_type = infer_call_return_type(g, lambda_name)
    let has_result = ret_type != "void"

    -- Emit a direct call to the lambda function
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = lambda_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SOME CALL: Option constructor
-- Some(value) -> Option { has_value: 1, value: payload }
-- ============================================================

fn generate_some_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Build the Option struct: { has_value: 1, value: payload }
    -- Start with an undef base value
    let base_id = g.builder.next_id
    let mut base_inst = ir_inst_new(base_id, OP_ALLOCA())
    base_inst.alloc_type = "option"
    base_inst.result_type = "ptr(option)"
    base_inst.has_result = true
    b.instructions.push(base_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Store has_value = 1 (field index 0)
    let insert1_id = g.builder.next_id
    let mut insert1 = ir_inst_new(insert1_id, OP_INSERT_FIELD())
    insert1.field_base = ir_val_inst(base_id)
    insert1.field_value = ir_val_int(1)
    insert1.field_index = 0
    insert1.result_type = "option"
    insert1.has_result = true
    b.instructions.push(insert1)
    g.builder.next_id = g.builder.next_id + 1

    -- Store value = payload (field index 1)
    let insert2_id = g.builder.next_id
    let mut insert2 = ir_inst_new(insert2_id, OP_INSERT_FIELD())
    insert2.field_base = ir_val_inst(insert1_id)
    insert2.field_value = ir_val_inst(val_result.val_id)
    insert2.field_index = 1
    insert2.result_type = "option"
    insert2.has_result = true
    b.instructions.push(insert2)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert2_id)
}

-- ============================================================
-- OK CALL: Result constructor (success)
-- Ok(value) -> Result { tag: 0, ok_val: payload, err_val: undef }
-- ============================================================

fn generate_ok_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Build the Result struct: { tag: 0, ok_val: payload, err_val: undef }
    let base_id = g.builder.next_id
    let mut base_inst = ir_inst_new(base_id, OP_ALLOCA())
    base_inst.alloc_type = "result"
    base_inst.result_type = "ptr(result)"
    base_inst.has_result = true
    b.instructions.push(base_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- tag = 0 (Ok) at field index 0
    let insert_tag_id = g.builder.next_id
    let mut insert_tag = ir_inst_new(insert_tag_id, OP_INSERT_FIELD())
    insert_tag.field_base = ir_val_inst(base_id)
    insert_tag.field_value = ir_val_int(0)
    insert_tag.field_index = 0
    insert_tag.result_type = "result"
    insert_tag.has_result = true
    b.instructions.push(insert_tag)
    g.builder.next_id = g.builder.next_id + 1

    -- ok_val = payload at field index 1
    let insert_ok_id = g.builder.next_id
    let mut insert_ok = ir_inst_new(insert_ok_id, OP_INSERT_FIELD())
    insert_ok.field_base = ir_val_inst(insert_tag_id)
    insert_ok.field_value = ir_val_inst(val_result.val_id)
    insert_ok.field_index = 1
    insert_ok.result_type = "result"
    insert_ok.has_result = true
    b.instructions.push(insert_ok)
    g.builder.next_id = g.builder.next_id + 1

    -- err_val = undef at field index 2
    let insert_err_id = g.builder.next_id
    let mut insert_err = ir_inst_new(insert_err_id, OP_INSERT_FIELD())
    insert_err.field_base = ir_val_inst(insert_ok_id)
    insert_err.field_value = ir_val_undef()
    insert_err.field_index = 2
    insert_err.result_type = "result"
    insert_err.has_result = true
    b.instructions.push(insert_err)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert_err_id)
}

-- ============================================================
-- ERR CALL: Result constructor (error)
-- Err(value) -> Result { tag: 1, ok_val: undef, err_val: payload }
-- ============================================================

fn generate_err_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the error payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Build the Result struct: { tag: 1, ok_val: undef, err_val: payload }
    let base_id = g.builder.next_id
    let mut base_inst = ir_inst_new(base_id, OP_ALLOCA())
    base_inst.alloc_type = "result"
    base_inst.result_type = "ptr(result)"
    base_inst.has_result = true
    b.instructions.push(base_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- tag = 1 (Err) at field index 0
    let insert_tag_id = g.builder.next_id
    let mut insert_tag = ir_inst_new(insert_tag_id, OP_INSERT_FIELD())
    insert_tag.field_base = ir_val_inst(base_id)
    insert_tag.field_value = ir_val_int(1)
    insert_tag.field_index = 0
    insert_tag.result_type = "result"
    insert_tag.has_result = true
    b.instructions.push(insert_tag)
    g.builder.next_id = g.builder.next_id + 1

    -- ok_val = undef at field index 1
    let insert_ok_id = g.builder.next_id
    let mut insert_ok = ir_inst_new(insert_ok_id, OP_INSERT_FIELD())
    insert_ok.field_base = ir_val_inst(insert_tag_id)
    insert_ok.field_value = ir_val_undef()
    insert_ok.field_index = 1
    insert_ok.result_type = "result"
    insert_ok.has_result = true
    b.instructions.push(insert_ok)
    g.builder.next_id = g.builder.next_id + 1

    -- err_val = payload at field index 2
    let insert_err_id = g.builder.next_id
    let mut insert_err = ir_inst_new(insert_err_id, OP_INSERT_FIELD())
    insert_err.field_base = ir_val_inst(insert_ok_id)
    insert_err.field_value = ir_val_inst(val_result.val_id)
    insert_err.field_index = 2
    insert_err.result_type = "result"
    insert_err.has_result = true
    b.instructions.push(insert_err)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert_err_id)
}

-- ============================================================
-- GENERIC FUNCTION CALL
-- Builds a monomorphized name, generates the specialized copy
-- if not already done, then calls it normally.
-- Port of stage3/src/ir_gen.zig generateGenericCall
-- ============================================================

fn generate_generic_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let callee_expr = *expr.callee
    let fn_name = callee_expr.name

    -- Build monomorphized name: fn_name + "_" + type_args joined by "_"
    let mut mangled_name = fn_name
    let mut type_args = expr.generic_args

    if type_args.len() == 0 {
        -- Implicit generic: infer type args from call arguments
        -- For now, use "i64" as default for each generic param
        -- (A more complete implementation would inspect arg expressions)
        if g.generic_fn_decls.contains(fn_name) {
            let decl_idx = g.generic_fn_decls.get(fn_name)
            if decl_idx >= 0 and decl_idx < g.declarations.len() {
                let decl = g.declarations[decl_idx]
                if decl.kind == DECL_FUNCTION() {
                    let fd = *decl.func_decl
                    let mut inferred: List[string] = []
                    let mut pi = 0
                    while pi < fd.generic_params.len() {
                        -- Try to infer from corresponding argument type
                        let inferred_type = if pi < expr.args.len() {
                            infer_arg_type(g, expr.args[pi])
                        } else {
                            "i64"
                        }
                        inferred.push(inferred_type)
                        pi = pi + 1
                    }
                    type_args = inferred
                }
            }
        }
    }

    -- Build the mangled name from type arguments
    let mut ti = 0
    while ti < type_args.len() {
        mangled_name = mangled_name + "_" + map_type_name(type_args[ti])
        ti = ti + 1
    }

    -- Generate the monomorphized function if not already done
    if g.mono_generated.contains(mangled_name) == false {
        g.mono_generated.insert(mangled_name, true)

        if g.generic_fn_decls.contains(fn_name) {
            let decl_idx = g.generic_fn_decls.get(fn_name)
            if decl_idx >= 0 and decl_idx < g.declarations.len() {
                let decl = g.declarations[decl_idx]
                if decl.kind == DECL_FUNCTION() {
                    let fd = *decl.func_decl

                    -- Save current generator state
                    let saved_variable_map = g.variable_map
                    let saved_variable_types = g.variable_types
                    let saved_list_elem_kinds = g.list_elem_kinds
                    let saved_list_elem_types = g.list_elem_types
                    let saved_return_type = g.current_return_type
                    let saved_type_subs = g.type_substitutions

                    -- Set up fresh maps for the monomorphized function
                    g.variable_map = Map_new()
                    g.variable_types = Map_new()

                    -- Set up type substitutions: map generic param names to concrete types
                    g.type_substitutions = Map_new()
                    let mut si = 0
                    while si < fd.generic_params.len() and si < type_args.len() {
                        g.type_substitutions.insert(fd.generic_params[si], type_args[si])
                        si = si + 1
                    }

                    -- Declare and generate the monomorphized function body
                    -- Create a modified FunctionDecl with the mangled name and no generic params
                    let mut mono_fd = fd
                    let empty_gp: List[string] = []
                    mono_fd.generic_params = empty_gp

                    -- Replace type parameter references in param types and return type
                    let mut mono_params: List[FuncParam] = []
                    let mut pi = 0
                    while pi < fd.params.len() {
                        let p = fd.params[pi]
                        let resolved_type = resolve_type_substitution(g, p.type_name)
                        let mut mp = func_param_new(p.name, resolved_type)
                        mp.is_mut = p.is_mut
                        mono_params.push(mp)
                        pi = pi + 1
                    }
                    mono_fd.params = mono_params

                    let resolved_ret = resolve_type_substitution(g, fd.return_type)
                    mono_fd.return_type = resolved_ret

                    g = declare_function(g, mono_fd, mangled_name)
                    g = generate_function_body(g, mono_fd, mangled_name)

                    -- Restore previous generator state
                    g.variable_map = saved_variable_map
                    g.variable_types = saved_variable_types
                    g.list_elem_kinds = saved_list_elem_kinds
                    g.list_elem_types = saved_list_elem_types
                    g.current_return_type = saved_return_type
                    g.type_substitutions = saved_type_subs
                }
            }
        }
    }

    -- Generate the call to the monomorphized function
    let mut call_args: List[IRValue] = []
    let mut ai = 0
    while ai < expr.args.len() {
        let arg_result = generate_expr(g, b, expr.args[ai])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        ai = ai + 1
    }

    let ret_type = infer_call_return_type(g, mangled_name)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = mangled_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- RESOLVE TYPE SUBSTITUTION
-- Replaces type parameter names with concrete types from
-- the current type_substitutions map.
-- ============================================================

fn resolve_type_substitution(gen: IRGenerator, type_name: string) -> string {
    -- Direct substitution
    if gen.type_substitutions.contains(type_name) {
        return gen.type_substitutions.get(type_name)
    }

    -- Handle generic container types: List[T], Option[T], Box[T], etc.
    if starts_with(type_name, "List[") {
        let inner = extract_inner_type(type_name, "List[")
        let resolved_inner = resolve_type_substitution(gen, inner)
        return "List[" + resolved_inner + "]"
    }
    if starts_with(type_name, "Option[") {
        let inner = extract_inner_type(type_name, "Option[")
        let resolved_inner = resolve_type_substitution(gen, inner)
        return "Option[" + resolved_inner + "]"
    }
    if starts_with(type_name, "Box[") {
        let inner = extract_inner_type(type_name, "Box[")
        let resolved_inner = resolve_type_substitution(gen, inner)
        return "Box[" + resolved_inner + "]"
    }
    if starts_with(type_name, "Result[") {
        let inner = extract_inner_type(type_name, "Result[")
        -- Result inner is "T, E" — split on ", " and resolve each
        let comma_idx = string_find(inner, ", ")
        if comma_idx >= 0 {
            let ok_type = substr(inner, 0, comma_idx)
            let err_type = substr(inner, comma_idx + 2, len(inner) - comma_idx - 2)
            let resolved_ok = resolve_type_substitution(gen, ok_type)
            let resolved_err = resolve_type_substitution(gen, err_type)
            return "Result[" + resolved_ok + ", " + resolved_err + "]"
        }
    }

    return type_name
}

-- ============================================================
-- INFER ARGUMENT TYPE
-- Attempts to infer the IR type of an expression for implicit
-- generic type argument deduction.
-- ============================================================

fn infer_arg_type(gen: IRGenerator, arg: Expr) -> string {
    if arg.kind == EXPR_LITERAL_INT() {
        return "int"
    }
    if arg.kind == EXPR_LITERAL_FLOAT() {
        return "float"
    }
    if arg.kind == EXPR_LITERAL_STRING() {
        return "string"
    }
    if arg.kind == EXPR_LITERAL_BOOL() {
        return "bool"
    }
    if arg.kind == EXPR_IDENTIFIER() {
        if gen.variable_types.contains(arg.name) {
            let ty = gen.variable_types.get(arg.name)
            -- Map IR type names back to dAImond names
            if ty == "i64" { return "int" }
            if ty == "f64" { return "float" }
            return ty
        }
    }
    return "int"
}

-- ============================================================
-- METHOD CALL IR GENERATION
-- Dispatches method calls on objects to the appropriate handler:
-- enum constructors, dynamic dispatch, map methods, list methods,
-- string methods, struct methods.
-- Port of stage3/src/ir_gen.zig generateMethodCall
-- ============================================================

fn generate_method_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let obj_expr = *expr.object
    let method_name = expr.method
    let args = expr.args

    -- --------------------------------------------------------
    -- Check if this is an enum variant constructor: EnumName.Variant(args)
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_IDENTIFIER() {
        let obj_name = obj_expr.name
        if g.enum_defs.contains(obj_name) {
            return generate_enum_method_construct(g, b, obj_name, method_name, args)
        }
    }

    -- --------------------------------------------------------
    -- Check if this is a dyn Trait method call
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_IDENTIFIER() {
        let obj_name = obj_expr.name
        if g.dyn_var_concrete.contains(obj_name) {
            return generate_dyn_dispatch_call(g, b, obj_name, method_name, args)
        }
    }

    -- --------------------------------------------------------
    -- Get object variable name for type dispatch
    -- --------------------------------------------------------
    let obj_var_name = if obj_expr.kind == EXPR_IDENTIFIER() {
        obj_expr.name
    } else {
        ""
    }

    -- --------------------------------------------------------
    -- Map method calls
    -- --------------------------------------------------------
    if len(obj_var_name) > 0 and g.map_var_kinds.contains(obj_var_name) {
        let mk = g.map_var_kinds.get(obj_var_name)
        let map_ptr = g.variable_map.get(obj_var_name)
        return generate_map_method_call(g, b, mk, map_ptr, method_name, args)
    }

    -- --------------------------------------------------------
    -- List method calls (on direct variable)
    -- --------------------------------------------------------
    if len(obj_var_name) > 0 and g.list_elem_kinds.contains(obj_var_name) {
        let ek = g.list_elem_kinds.get(obj_var_name)
        let list_ptr = g.variable_map.get(obj_var_name)
        return generate_list_method_call(g, b, ek, list_ptr, obj_var_name, method_name, args)
    }

    -- --------------------------------------------------------
    -- List method calls on struct field access (e.g., p.tokens.push(x))
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_FIELD_ACCESS() {
        let field_obj = *obj_expr.object
        if field_obj.kind == EXPR_IDENTIFIER() {
            let field_obj_name = field_obj.name
            let field_name = obj_expr.field
            let field_key = if g.var_struct_types.contains(field_obj_name) {
                g.var_struct_types.get(field_obj_name) + "." + field_name
            } else {
                ""
            }
            if len(field_key) > 0 and g.field_list_elem_kinds.contains(field_key) {
                let ek = g.field_list_elem_kinds.get(field_key)
                -- Generate the field access to get the list pointer
                let obj_result = generate_expr(g, b, obj_expr)
                g = obj_result.gen
                b = obj_result.block
                let list_ptr = obj_result.val_id
                return generate_list_method_call(g, b, ek, list_ptr, "", method_name, args)
            }
        }
    }

    -- --------------------------------------------------------
    -- String .len() method
    -- --------------------------------------------------------
    if method_name == "len" and args.len() == 0 {
        if len(obj_var_name) > 0 and g.variable_types.contains(obj_var_name) {
            let vtype = g.variable_types.get(obj_var_name)
            if vtype == "string" {
                let obj_result = generate_expr(g, b, obj_expr)
                g = obj_result.gen
                b = obj_result.block

                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = "dm_string_len"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(obj_result.val_id))
                inst.call_args = call_args
                inst.result_type = "i64"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, id)
            }
        }
    }

    -- --------------------------------------------------------
    -- Struct method calls (mangled name dispatch)
    -- --------------------------------------------------------
    let struct_type = if len(obj_var_name) > 0 and g.var_struct_types.contains(obj_var_name) {
        g.var_struct_types.get(obj_var_name)
    } else if g.method_struct_map.contains(method_name) {
        g.method_struct_map.get(method_name)
    } else {
        ""
    }

    if len(struct_type) > 0 {
        return generate_struct_method_call(g, b, struct_type, obj_expr, method_name, args)
    }

    -- --------------------------------------------------------
    -- Fallback: treat as a regular function call with self as first arg
    -- --------------------------------------------------------
    let obj_result = generate_expr(g, b, obj_expr)
    g = obj_result.gen
    b = obj_result.block

    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(obj_result.val_id))

    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    let ret_type = infer_call_return_type(g, method_name)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = method_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- ENUM VARIANT CONSTRUCTOR VIA METHOD SYNTAX
-- Handles EnumName.Variant(payload...) as a method call.
-- ============================================================

fn generate_enum_method_construct(gen: IRGenerator, block: IRBasicBlock, enum_name: string, variant_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let info = g.enum_defs.get(enum_name)

    -- Find the variant
    let mut tag = -1
    let mut has_payload = false
    let mut vi = 0
    while vi < info.variant_names.len() {
        if info.variant_names[vi] == variant_name {
            tag = info.variant_tags[vi]
            has_payload = info.variant_has_payload[vi]
        }
        vi = vi + 1
    }

    if tag < 0 {
        -- Variant not found; return undef
        return expr_result(g, b, 0)
    }

    -- Build the enum struct with tag and optional payload
    let base_id = g.builder.next_id
    let mut base_inst = ir_inst_new(base_id, OP_ALLOCA())
    base_inst.alloc_type = enum_name
    base_inst.result_type = "ptr(" + enum_name + ")"
    base_inst.has_result = true
    b.instructions.push(base_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Insert tag at field index 0
    let tag_insert_id = g.builder.next_id
    let mut tag_insert = ir_inst_new(tag_insert_id, OP_INSERT_FIELD())
    tag_insert.field_base = ir_val_inst(base_id)
    tag_insert.field_value = ir_val_int(tag)
    tag_insert.field_index = 0
    tag_insert.result_type = enum_name
    tag_insert.has_result = true
    b.instructions.push(tag_insert)
    g.builder.next_id = g.builder.next_id + 1

    let mut last_id = tag_insert_id

    -- If has payload, insert payload fields
    if has_payload and args.len() > 0 {
        -- Determine the field index for this variant's payload
        -- The payload field is named "VariantName_0" and its index is
        -- computed by counting prior payload fields + 1 (for the tag)
        let mut field_idx = 1
        let mut pi = 0
        while pi < info.variant_names.len() {
            if info.variant_names[pi] == variant_name {
                -- Found our variant; field_idx is now correct
                pi = info.variant_names.len()  -- break
            } else {
                if info.variant_has_payload[pi] {
                    field_idx = field_idx + 1
                }
                pi = pi + 1
            }
        }

        -- Insert each payload argument
        let mut ai = 0
        while ai < args.len() {
            let arg_result = generate_expr(g, b, args[ai])
            g = arg_result.gen
            b = arg_result.block

            let payload_insert_id = g.builder.next_id
            let mut payload_insert = ir_inst_new(payload_insert_id, OP_INSERT_FIELD())
            payload_insert.field_base = ir_val_inst(last_id)
            payload_insert.field_value = ir_val_inst(arg_result.val_id)
            payload_insert.field_index = field_idx + ai
            payload_insert.result_type = enum_name
            payload_insert.has_result = true
            b.instructions.push(payload_insert)
            g.builder.next_id = g.builder.next_id + 1

            last_id = payload_insert_id
            ai = ai + 1
        }
    }

    return expr_result(g, b, last_id)
}

-- ============================================================
-- DYNAMIC TRAIT DISPATCH
-- Looks up the concrete type for a dyn Trait variable and
-- calls ConcreteType_method(data_ptr, args...)
-- ============================================================

fn generate_dyn_dispatch_call(gen: IRGenerator, block: IRBasicBlock, obj_name: string, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let concrete_type = g.dyn_var_concrete.get(obj_name)
    let mangled = concrete_type + "_" + method_name

    -- Load the data pointer from the dyn variable's alloca
    let alloca_id = g.variable_map.get(obj_name)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(alloca_id)
    load_inst.load_type = "ptr"
    load_inst.result_type = "ptr"
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Build args: data_ptr (as self), then method arguments
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(load_id))

    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    let ret_type = infer_call_return_type(g, mangled)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = mangled
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- MAP METHOD CALL
-- Dispatches .insert, .get, .contains, .remove, .len,
-- .keys, .values, .set on Map[K,V] variables.
-- ============================================================

fn generate_map_method_call(gen: IRGenerator, block: IRBasicBlock, mk: int, map_ptr: int, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- ---- insert / set ----
    if method_name == "insert" or method_name == "set" {
        if args.len() >= 2 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            let val_result = generate_expr(g, b, args[1])
            g = val_result.gen
            b = val_result.block

            let insert_fn = if mk == MAP_KIND_STRING_INT() {
                "dm_map_string_int_insert"
            } else {
                "dm_map_int_string_insert"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = insert_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(map_ptr))
            call_args.push(ir_val_inst(key_result.val_id))
            call_args.push(ir_val_inst(val_result.val_id))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- ---- get ----
    if method_name == "get" {
        if args.len() >= 1 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            if mk == MAP_KIND_STRING_INT() {
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = "dm_map_string_int_get"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(map_ptr))
                call_args.push(ir_val_inst(key_result.val_id))
                inst.call_args = call_args
                inst.result_type = "i64"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, id)
            } else {
                -- MAP_KIND_INT_STRING: get returns string via out param
                let out_alloca_id = g.builder.next_id
                let mut out_inst = ir_inst_new(out_alloca_id, OP_ALLOCA())
                out_inst.alloc_type = "string"
                out_inst.result_type = "ptr(string)"
                out_inst.has_result = true
                b.instructions.push(out_inst)
                g.builder.next_id = g.builder.next_id + 1

                let call_id = g.builder.next_id
                let mut call_inst = ir_inst_new(call_id, OP_CALL())
                call_inst.callee = "dm_map_int_string_get"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(out_alloca_id))
                call_args.push(ir_val_inst(map_ptr))
                call_args.push(ir_val_inst(key_result.val_id))
                call_inst.call_args = call_args
                call_inst.result_type = "void"
                call_inst.has_result = false
                b.instructions.push(call_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Load the result from out_alloca
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(out_alloca_id)
                load_inst.load_type = "string"
                load_inst.result_type = "string"
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, load_id)
            }
        }
        return expr_result(g, b, 0)
    }

    -- ---- contains ----
    if method_name == "contains" {
        if args.len() >= 1 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            let contains_fn = if mk == MAP_KIND_STRING_INT() {
                "dm_map_string_int_contains"
            } else {
                "dm_map_int_string_contains"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = contains_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(map_ptr))
            call_args.push(ir_val_inst(key_result.val_id))
            inst.call_args = call_args
            inst.result_type = "bool"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- ---- remove ----
    if method_name == "remove" {
        if args.len() >= 1 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            let remove_fn = if mk == MAP_KIND_STRING_INT() {
                "dm_map_string_int_remove"
            } else {
                "dm_map_int_string_remove"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = remove_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(map_ptr))
            call_args.push(ir_val_inst(key_result.val_id))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- ---- len ----
    if method_name == "len" {
        let len_fn = if mk == MAP_KIND_STRING_INT() {
            "dm_map_string_int_len"
        } else {
            "dm_map_int_string_len"
        }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = len_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(map_ptr))
        inst.call_args = call_args
        inst.result_type = "i64"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- keys ----
    if method_name == "keys" {
        let list_type = if mk == MAP_KIND_STRING_INT() {
            "dm_list_dm_string"
        } else {
            "dm_list_int64"
        }
        let keys_fn = if mk == MAP_KIND_STRING_INT() {
            "dm_map_string_int_keys"
        } else {
            "dm_map_int_string_keys"
        }

        -- Allocate a list to receive the keys
        let list_alloca_id = g.builder.next_id
        let mut list_inst = ir_inst_new(list_alloca_id, OP_ALLOCA())
        list_inst.alloc_type = list_type
        list_inst.result_type = "ptr(" + list_type + ")"
        list_inst.has_result = true
        b.instructions.push(list_inst)
        g.builder.next_id = g.builder.next_id + 1

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = keys_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_alloca_id))
        call_args.push(ir_val_inst(map_ptr))
        inst.call_args = call_args
        inst.result_type = "void"
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, list_alloca_id)
    }

    -- ---- values ----
    if method_name == "values" {
        let list_type = if mk == MAP_KIND_STRING_INT() {
            "dm_list_int64"
        } else {
            "dm_list_dm_string"
        }
        let values_fn = if mk == MAP_KIND_STRING_INT() {
            "dm_map_string_int_values"
        } else {
            "dm_map_int_string_values"
        }

        let list_alloca_id = g.builder.next_id
        let mut list_inst = ir_inst_new(list_alloca_id, OP_ALLOCA())
        list_inst.alloc_type = list_type
        list_inst.result_type = "ptr(" + list_type + ")"
        list_inst.has_result = true
        b.instructions.push(list_inst)
        g.builder.next_id = g.builder.next_id + 1

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = values_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_alloca_id))
        call_args.push(ir_val_inst(map_ptr))
        inst.call_args = call_args
        inst.result_type = "void"
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, list_alloca_id)
    }

    -- Unknown map method; return undef
    return expr_result(g, b, 0)
}

-- ============================================================
-- LIST METHOD CALL
-- Dispatches .push, .pop, .len, .contains on List[T] variables.
-- Runtime function names are selected based on elem_kind.
-- ============================================================

fn generate_list_method_call(gen: IRGenerator, block: IRBasicBlock, ek: int, list_ptr: int, obj_var_name: string, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- ---- push ----
    if method_name == "push" {
        if args.len() < 1 {
            return expr_result(g, b, 0)
        }

        let val_result = generate_expr(g, b, args[0])
        g = val_result.gen
        b = val_result.block

        if ek == LIST_ELEM_OTHER() {
            -- Generic push: alloca temp, store struct value, pass pointer + elem_size
            let elem_type_name = if len(obj_var_name) > 0 and g.list_elem_types.contains(obj_var_name) {
                g.list_elem_types.get(obj_var_name)
            } else {
                "i64"
            }
            let elem_size = estimate_type_size(elem_type_name)

            -- Alloca temp for the value
            let temp_alloca_id = g.builder.next_id
            let mut temp_inst = ir_inst_new(temp_alloca_id, OP_ALLOCA())
            temp_inst.alloc_type = elem_type_name
            temp_inst.result_type = "ptr(" + elem_type_name + ")"
            temp_inst.has_result = true
            b.instructions.push(temp_inst)
            g.builder.next_id = g.builder.next_id + 1

            -- Store value into temp
            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(temp_alloca_id)
            store_inst.store_val = ir_val_inst(val_result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1

            -- Call dm_list_generic_push(list_ptr, temp_ptr, elem_size)
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = "dm_list_generic_push"
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(list_ptr))
            call_args.push(ir_val_inst(temp_alloca_id))
            call_args.push(ir_val_int(elem_size))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }

        let push_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_push" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_push" }
        else { "dm_list_dm_string_push" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = push_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_ptr))
        call_args.push(ir_val_inst(val_result.val_id))
        inst.call_args = call_args
        inst.result_type = "void"
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- pop ----
    if method_name == "pop" {
        let pop_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_pop" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_get" }
        else if ek == LIST_ELEM_STRING() { "dm_list_string_get" }
        else { "dm_list_int64_pop" }

        let ret_type = if ek == LIST_ELEM_FLOAT() { "f64" }
        else if ek == LIST_ELEM_STRING() { "string" }
        else { "i64" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = pop_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_ptr))
        inst.call_args = call_args
        inst.result_type = ret_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- len ----
    if method_name == "len" {
        let len_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_len" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_len" }
        else if ek == LIST_ELEM_STRING() { "dm_list_string_len" }
        else { "dm_list_generic_len" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = len_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_ptr))
        inst.call_args = call_args
        inst.result_type = "i64"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- contains ----
    if method_name == "contains" {
        if args.len() >= 1 {
            let search_result = generate_expr(g, b, args[0])
            g = search_result.gen
            b = search_result.block

            let contains_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_contains" }
            else if ek == LIST_ELEM_STRING() { "dm_list_string_contains" }
            else { "dm_list_int64_contains" }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = contains_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(list_ptr))
            call_args.push(ir_val_inst(search_result.val_id))
            inst.call_args = call_args
            inst.result_type = "bool"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- Unknown list method; return undef
    return expr_result(g, b, 0)
}

-- ============================================================
-- STRUCT METHOD CALL
-- Builds mangled name (StructType_method), passes object as
-- self (first arg), and emits OP_CALL.
-- Handles both self-by-value and mut self (pass-by-pointer).
-- ============================================================

fn generate_struct_method_call(gen: IRGenerator, block: IRBasicBlock, struct_type: string, obj_expr: Expr, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let mangled = struct_type + "_" + method_name

    -- Check if this method takes a mut self parameter (pass-by-pointer)
    let mut takes_mut_self = false
    if g.mut_param_fns.contains(mangled) {
        let flags = g.mut_param_fns.get(mangled)
        takes_mut_self = starts_with(flags, "1")
    }

    let mut call_args: List[IRValue] = []

    if takes_mut_self and obj_expr.kind == EXPR_IDENTIFIER() {
        -- Pass the alloca pointer directly for mut self
        let obj_name = obj_expr.name
        if g.variable_map.contains(obj_name) {
            let ptr_id = g.variable_map.get(obj_name)
            call_args.push(ir_val_inst(ptr_id))
        } else {
            let obj_result = generate_expr(g, b, obj_expr)
            g = obj_result.gen
            b = obj_result.block
            call_args.push(ir_val_inst(obj_result.val_id))
        }
    } else {
        -- Pass the object value (self by value)
        let obj_result = generate_expr(g, b, obj_expr)
        g = obj_result.gen
        b = obj_result.block
        call_args.push(ir_val_inst(obj_result.val_id))
    }

    -- Generate remaining arguments
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    let ret_type = infer_call_return_type(g, mangled)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = mangled
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- IR Generation: Field Access, Index Access, Struct Literals,
--                Enum Literals, Array Literals
-- ============================================================
-- Part 4 of expression IR generation for the Stage 4 compiler.
-- Each function follows the signature:
--   fn xxx(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult
-- and returns expr_result(g, b, result_id).

-- ============================================================
-- Field Access: expr.object.expr.field
-- ============================================================

fn generate_field_access(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let object = *expr.object
    let field_name = expr.field

    -- Generate the base object expression
    let obj_result = generate_expr(g, b, object)
    g = obj_result.gen
    b = obj_result.block
    let obj_id = obj_result.val_id

    -- Determine the struct type name by examining the object expression
    let mut struct_name = ""
    if object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name
        if g.var_struct_types.contains(var_name) {
            struct_name = g.var_struct_types.get(var_name)
        }
    }

    -- If we could not resolve from a direct identifier, try from the
    -- variable_types map (e.g., for chained field access results)
    if struct_name == "" {
        if object.kind == EXPR_IDENTIFIER() {
            let var_name = object.name
            if g.variable_types.contains(var_name) {
                let vtype = g.variable_types.get(var_name)
                -- If the type string names a known struct, use it
                if g.struct_defs.contains(vtype) {
                    struct_name = vtype
                }
            }
        }
    }

    -- Look up struct definition to find the field index and type
    if struct_name == "" {
        -- Cannot resolve struct type; emit an extract at index 0 as fallback
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
        inst.field_base = ir_val_inst(obj_id)
        inst.field_index = 0
        inst.field_type = "i64"
        inst.result_type = "i64"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    let info = g.struct_defs.get(struct_name)

    -- Find the field index by name
    let mut field_idx = 0
    let mut found = false
    let mut field_type_str = "i64"
    let num_fields = info.field_names.len()
    let mut fi = 0
    while fi < num_fields {
        if info.field_names[fi] == field_name {
            field_idx = fi
            field_type_str = info.field_types[fi]
            found = true
        }
        fi = fi + 1
    }

    -- Map the dAImond type name to an IR type string
    let ir_type = map_type_name(field_type_str)

    -- Emit OP_EXTRACT_FIELD instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
    inst.field_base = ir_val_inst(obj_id)
    inst.field_index = field_idx
    inst.field_type = ir_type
    inst.result_type = ir_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    -- Track list-typed fields for later index access resolution
    if starts_with(field_type_str, "List[") {
        let inner = substr(field_type_str, 5, len(field_type_str) - 6)
        let key = struct_name + "." + field_name
        if inner == "int" {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_INT())
        } else if inner == "float" {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_FLOAT())
        } else if inner == "string" {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_STRING())
        } else {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_OTHER())
            g.field_list_elem_types.insert(key, inner)
        }
    }

    -- If the field type is itself a struct, track the result variable mapping
    if g.struct_defs.contains(field_type_str) {
        -- Use a synthetic name based on instruction id for chained access tracking
        let result_var = "_field_" + int_to_string(id)
        g.var_struct_types.insert(result_var, field_type_str)
    }

    return expr_result(g, b, id)
}

-- ============================================================
-- Index Access: expr.object[expr.index]
-- ============================================================

fn generate_index_access(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let object = *expr.object
    let index = *expr.index

    -- Generate the object expression
    let obj_result = generate_expr(g, b, object)
    g = obj_result.gen
    b = obj_result.block
    let obj_id = obj_result.val_id

    -- Generate the index expression
    let idx_result = generate_expr(g, b, index)
    g = idx_result.gen
    b = idx_result.block
    let idx_id = idx_result.val_id

    -- Check if the object is a known list variable
    if object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name

        -- List variable index access
        if g.list_elem_kinds.contains(var_name) {
            let elem_kind = g.list_elem_kinds.get(var_name)

            -- Determine element type from kind
            let mut elem_type = "i64"
            if elem_kind == LIST_ELEM_INT() {
                elem_type = "i64"
            } else if elem_kind == LIST_ELEM_FLOAT() {
                elem_type = "f64"
            } else if elem_kind == LIST_ELEM_STRING() {
                elem_type = "string"
            } else if elem_kind == LIST_ELEM_OTHER() {
                if g.list_elem_types.contains(var_name) {
                    elem_type = g.list_elem_types.get(var_name)
                }
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_LIST_GET())
            inst.lhs = ir_val_inst(obj_id)
            inst.rhs = ir_val_inst(idx_id)
            inst.result_type = map_type_name(elem_type)
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }

        -- Map variable index access
        if g.map_var_kinds.contains(var_name) {
            let map_kind = g.map_var_kinds.get(var_name)
            let mut callee_name = "dm_map_string_int_get"
            let mut ret_type = "i64"

            if map_kind == MAP_KIND_STRING_INT() {
                callee_name = "dm_map_string_int_get"
                ret_type = "i64"
            } else if map_kind == MAP_KIND_INT_STRING() {
                callee_name = "dm_map_int_string_get"
                ret_type = "string"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = callee_name
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(obj_id))
            call_args.push(ir_val_inst(idx_id))
            inst.call_args = call_args
            inst.result_type = ret_type
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }
    }

    -- Check for field access list indexing: obj.field[i]
    if object.kind == EXPR_FIELD_ACCESS() {
        let field_obj = *object.object
        let field_name = object.field

        if field_obj.kind == EXPR_IDENTIFIER() {
            let parent_var = field_obj.name
            if g.var_struct_types.contains(parent_var) {
                let parent_struct = g.var_struct_types.get(parent_var)
                let key = parent_struct + "." + field_name
                if g.field_list_elem_kinds.contains(key) {
                    let elem_kind = g.field_list_elem_kinds.get(key)
                    let mut elem_type = "i64"
                    if elem_kind == LIST_ELEM_INT() {
                        elem_type = "i64"
                    } else if elem_kind == LIST_ELEM_FLOAT() {
                        elem_type = "f64"
                    } else if elem_kind == LIST_ELEM_STRING() {
                        elem_type = "string"
                    } else if elem_kind == LIST_ELEM_OTHER() {
                        if g.field_list_elem_types.contains(key) {
                            elem_type = g.field_list_elem_types.get(key)
                        }
                    }

                    let id = g.builder.next_id
                    let mut inst = ir_inst_new(id, OP_LIST_GET())
                    inst.lhs = ir_val_inst(obj_id)
                    inst.rhs = ir_val_inst(idx_id)
                    inst.result_type = map_type_name(elem_type)
                    inst.has_result = true
                    b.instructions.push(inst)
                    g.builder.next_id = id + 1
                    return expr_result(g, b, id)
                }
            }
        }
    }

    -- Default: treat as a list of i64
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_GET())
    inst.lhs = ir_val_inst(obj_id)
    inst.rhs = ir_val_inst(idx_id)
    inst.result_type = "i64"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- ============================================================
-- Struct Literal: TypeName { field1: val1, field2: val2, ... }
-- ============================================================

fn generate_struct_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let type_name = expr.type_name

    -- Look up the struct definition
    if g.struct_defs.contains(type_name) == false {
        -- Unknown struct type; return undef
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
        inst.field_base = ir_val_undef()
        inst.field_value = ir_val_int(0)
        inst.field_index = 0
        inst.result_type = type_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    let info = g.struct_defs.get(type_name)
    let num_fields = expr.field_names.len()

    -- Build the struct value by chaining INSERT_FIELD instructions.
    -- The first insert uses undef as the base; each subsequent insert
    -- takes the result of the previous one as its base.
    let mut current_id = -1
    let mut i = 0
    while i < num_fields {
        let fname = expr.field_names[i]
        let fval_expr = expr.field_values[i]

        -- Generate the field value expression
        let val_result = generate_expr(g, b, fval_expr)
        g = val_result.gen
        b = val_result.block
        let val_id = val_result.val_id

        -- Find the field index in the struct definition
        let mut field_idx = 0
        let def_count = info.field_names.len()
        let mut fi = 0
        while fi < def_count {
            if info.field_names[fi] == fname {
                field_idx = fi
            }
            fi = fi + 1
        }

        -- Emit OP_INSERT_FIELD
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())

        -- Base is undef for the first field, or the previous insert result
        if current_id == -1 {
            inst.field_base = ir_val_undef()
        } else {
            inst.field_base = ir_val_inst(current_id)
        }

        inst.field_value = ir_val_inst(val_id)
        inst.field_index = field_idx
        inst.result_type = type_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1

        current_id = id
        i = i + 1
    }

    -- Handle empty struct (no fields specified in literal)
    if current_id == -1 {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
        inst.field_base = ir_val_undef()
        inst.field_value = ir_val_int(0)
        inst.field_index = 0
        inst.result_type = type_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        current_id = id
    }

    return expr_result(g, b, current_id)
}

-- ============================================================
-- Enum Literal: EnumName.VariantName or EnumName.VariantName(payload...)
-- ============================================================

fn generate_enum_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let enum_name = expr.enum_name
    let variant_name = expr.variant_name

    -- Look up the enum definition
    if g.enum_defs.contains(enum_name) == false {
        -- Unknown enum; return a zero-tagged undef
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
        inst.field_base = ir_val_undef()
        inst.field_value = ir_val_int(0)
        inst.field_index = 0
        inst.result_type = enum_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    let info = g.enum_defs.get(enum_name)

    -- Find the variant index and its tag value
    let mut variant_tag = 0
    let mut variant_idx = -1
    let mut has_payload = false
    let num_variants = info.variant_names.len()
    let mut vi = 0
    while vi < num_variants {
        if info.variant_names[vi] == variant_name {
            variant_tag = info.variant_tags[vi]
            variant_idx = vi
            has_payload = info.variant_has_payload[vi]
        }
        vi = vi + 1
    }

    -- Step 1: Insert tag field at index 0
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_INSERT_FIELD())
    tag_inst.field_base = ir_val_undef()
    tag_inst.field_value = ir_val_int(variant_tag)
    tag_inst.field_index = 0
    tag_inst.result_type = enum_name
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = tag_id + 1

    let mut current_id = tag_id

    -- Step 2: If the variant has payload, generate payload expressions
    -- and insert them starting at field index 1.
    -- The field offset must skip past prior variant payload fields in the
    -- tagged union layout.
    if has_payload {
        let payload = expr.payload
        let num_payload = payload.len()

        -- Compute the field offset: skip tag (1) + sum of prior variant payloads
        let mut field_offset = 1
        let mut pvi = 0
        while pvi < num_variants {
            if info.variant_names[pvi] == variant_name {
                -- Found our variant; stop counting
                pvi = num_variants
            } else {
                if info.variant_has_payload[pvi] {
                    -- Each prior payload variant contributes its payload count
                    -- For simplicity, assume one payload field per variant
                    field_offset = field_offset + 1
                }
                pvi = pvi + 1
            }
        }

        let mut pi = 0
        while pi < num_payload {
            let payload_expr = payload[pi]

            -- Generate the payload value
            let pval_result = generate_expr(g, b, payload_expr)
            g = pval_result.gen
            b = pval_result.block
            let pval_id = pval_result.val_id

            -- Insert payload at the computed field index
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
            inst.field_base = ir_val_inst(current_id)
            inst.field_value = ir_val_inst(pval_id)
            inst.field_index = field_offset + pi
            inst.result_type = enum_name
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1

            current_id = id
            pi = pi + 1
        }
    }

    return expr_result(g, b, current_id)
}

-- ============================================================
-- Array Literal: [elem1, elem2, ...]
-- ============================================================

fn generate_array_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let elements = expr.elements
    let num_elements = elements.len()

    -- Empty array: create a new empty list
    if num_elements == 0 {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LIST_NEW())
        inst.result_type = "list"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Generate all element expressions first to determine types
    let mut elem_ids: List[int] = []
    let mut i = 0
    while i < num_elements {
        let elem_result = generate_expr(g, b, elements[i])
        g = elem_result.gen
        b = elem_result.block
        elem_ids.push(elem_result.val_id)
        i = i + 1
    }

    -- Create a new list via OP_LIST_NEW
    let list_id = g.builder.next_id
    let mut list_inst = ir_inst_new(list_id, OP_LIST_NEW())
    list_inst.result_type = "list"
    list_inst.has_result = true
    b.instructions.push(list_inst)
    g.builder.next_id = list_id + 1

    -- Push each element into the list via OP_LIST_PUSH
    let mut current_list_id = list_id
    let mut pi = 0
    while pi < num_elements {
        let push_id = g.builder.next_id
        let mut push_inst = ir_inst_new(push_id, OP_LIST_PUSH())
        push_inst.lhs = ir_val_inst(current_list_id)
        push_inst.rhs = ir_val_inst(elem_ids[pi])
        push_inst.result_type = "list"
        push_inst.has_result = true
        b.instructions.push(push_inst)
        g.builder.next_id = push_id + 1

        current_list_id = push_id
        pi = pi + 1
    }

    return expr_result(g, b, current_list_id)
}

-- ============================================================
-- EXPRESSION IR GENERATION PART 5
-- If expressions, match expressions, block expressions,
-- lambda expressions, pipeline operator, error propagation,
-- string interpolation, and comptime expressions.
-- Port of stage3/src/ir_gen.zig complex expression handlers.
-- ============================================================

-- ============================================================
-- IF EXPRESSION (returns a value)
-- ============================================================

fn generate_if_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate condition expression
    let cond_result = generate_expr(g, b, *expr.condition)
    g = cond_result.gen
    b = cond_result.block
    let cond_id = cond_result.val_id

    -- Create labels for then, else, and join blocks
    let then_label = gen_next_label(g, "if_then")
    g = gen_advance_label(g)
    let else_label = gen_next_label(g, "if_else")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "if_join")
    g = gen_advance_label(g)

    -- Allocate a result slot before branching so both branches can store into it
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Terminate current block with conditional branch
    if expr.has_else {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_id), then_label, else_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    } else {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_id), then_label, join_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    }

    -- Generate then block
    let mut then_block = ir_block_new(then_label)
    let then_result = generate_expr(g, then_block, *expr.then_branch)
    g = then_result.gen
    then_block = then_result.block

    -- Store then result into the result slot
    let mut then_store = ir_inst_new(g.builder.next_id, OP_STORE())
    then_store.store_ptr = ir_val_inst(result_alloca_id)
    then_store.store_val = ir_val_inst(then_result.val_id)
    then_store.has_result = false
    then_block.instructions.push(then_store)
    g.builder.next_id = g.builder.next_id + 1

    -- Terminate then block with branch to join
    if then_block.has_terminator == false {
        then_block.terminator = Box_new(term_br(join_label))
        then_block.has_terminator = true
    }
    g.completed_blocks.push(then_block)

    -- Generate else block
    let mut else_block = ir_block_new(else_label)
    if expr.has_else {
        let else_result = generate_expr(g, else_block, *expr.else_branch)
        g = else_result.gen
        else_block = else_result.block

        -- Store else result into the result slot
        let mut else_store = ir_inst_new(g.builder.next_id, OP_STORE())
        else_store.store_ptr = ir_val_inst(result_alloca_id)
        else_store.store_val = ir_val_inst(else_result.val_id)
        else_store.has_result = false
        else_block.instructions.push(else_store)
        g.builder.next_id = g.builder.next_id + 1

        if else_block.has_terminator == false {
            else_block.terminator = Box_new(term_br(join_label))
            else_block.has_terminator = true
        }
        g.completed_blocks.push(else_block)
    } else {
        -- No else branch: store a default value
        let mut default_store = ir_inst_new(g.builder.next_id, OP_STORE())
        default_store.store_ptr = ir_val_inst(result_alloca_id)
        default_store.store_val = ir_val_int(0)
        default_store.has_result = false
        else_block.instructions.push(default_store)
        g.builder.next_id = g.builder.next_id + 1

        else_block.terminator = Box_new(term_br(join_label))
        else_block.has_terminator = true
        g.completed_blocks.push(else_block)
    }

    -- Create join block and load the result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- MATCH EXPRESSION (returns a value)
-- ============================================================

fn generate_match_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate scrutinee
    let scrut_result = generate_expr(g, b, *expr.scrutinee)
    g = scrut_result.gen
    b = scrut_result.block
    let scrut_id = scrut_result.val_id

    -- Determine the scrutinee type to route to specialized match
    let scrut_expr = *expr.scrutinee

    -- Check if scrutinee is a known enum variable
    if scrut_expr.kind == EXPR_IDENTIFIER() {
        let scrut_name = scrut_expr.name
        if g.var_enum_types.contains(scrut_name) {
            let enum_type = g.var_enum_types.get(scrut_name)
            return generate_enum_match(g, b, expr, scrut_id, enum_type)
        }
    }

    -- Check match arms for enum variant patterns to detect enum matching
    if expr.match_arms.len() > 0 {
        let first_arm = expr.match_arms[0]
        let first_pat = first_arm.pattern

        -- Check for bare Ok/Err patterns (Result type)
        if first_pat.kind == PAT_ENUM_VARIANT() {
            let variant = first_pat.variant
            if variant == "Ok" or variant == "Err" {
                return generate_result_match(g, b, expr, scrut_id)
            }
            if variant == "Some" or variant == "None" {
                return generate_option_match(g, b, expr, scrut_id)
            }
            -- General enum variant pattern with known type
            if len(first_pat.enum_type) > 0 {
                return generate_enum_match(g, b, expr, scrut_id, first_pat.enum_type)
            }
        }
    }

    -- Default: literal/identifier matching with cascading if-else
    return generate_literal_match(g, b, expr, scrut_id)
}

-- ============================================================
-- LITERAL MATCH (int, string, bool, wildcard)
-- ============================================================

fn generate_literal_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    let join_label = gen_next_label(g, "match_end")
    g = gen_advance_label(g)

    -- Process each arm as cascading if-else
    let mut arm_idx = 0
    while arm_idx < expr.match_arms.len() {
        let arm = expr.match_arms[arm_idx]
        let pat = arm.pattern

        let arm_label = gen_next_label(g, "match_arm")
        g = gen_advance_label(g)
        let next_label = gen_next_label(g, "match_next")
        g = gen_advance_label(g)

        if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            -- Wildcard or identifier always matches
            -- Bind identifier if needed
            if pat.kind == PAT_IDENTIFIER() {
                -- Bind the scrutinee value to the pattern name
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = "i64"
                bind_alloca.result_type = "ptr(i64)"
                bind_alloca.has_result = true
                b.instructions.push(bind_alloca)
                g.builder.next_id = g.builder.next_id + 1

                let mut bind_store = ir_inst_new(g.builder.next_id, OP_STORE())
                bind_store.store_ptr = ir_val_inst(bind_alloca_id)
                bind_store.store_val = ir_val_inst(scrut_id)
                bind_store.has_result = false
                b.instructions.push(bind_store)
                g.builder.next_id = g.builder.next_id + 1

                g.variable_map.insert(pat.name, bind_alloca_id)
                g.variable_types.insert(pat.name, "i64")
            }

            -- Generate arm body directly in current block
            if arm.is_expr_body {
                let body_result = generate_expr(g, b, *arm.body_expr)
                g = body_result.gen
                b = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                b.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, b, arm.body)
                g = body_gen.gen
                b = body_gen.block
            }

            if b.has_terminator == false {
                b.terminator = Box_new(term_br(join_label))
                b.has_terminator = true
            }

            -- Wildcard/identifier is the catch-all, break out of loop
            arm_idx = expr.match_arms.len()
        } else if pat.kind == PAT_LITERAL() {
            -- Generate comparison of scrutinee with literal
            let lit_expr = *pat.literal_expr
            let lit_result = generate_expr(g, b, lit_expr)
            g = lit_result.gen
            b = lit_result.block

            -- Compare scrutinee with literal value
            let cmp_id = g.builder.next_id
            let mut cmp_inst = ir_inst_new(cmp_id, OP_EQ())
            cmp_inst.lhs = ir_val_inst(scrut_id)
            cmp_inst.rhs = ir_val_inst(lit_result.val_id)
            cmp_inst.result_type = "bool"
            cmp_inst.has_result = true
            b.instructions.push(cmp_inst)
            g.builder.next_id = g.builder.next_id + 1

            -- Check guard if present
            if arm.has_guard {
                let guard_result = generate_expr(g, b, *arm.guard)
                g = guard_result.gen
                b = guard_result.block

                -- AND the pattern match with the guard condition
                let and_id = g.builder.next_id
                let mut and_inst = ir_inst_new(and_id, OP_LOGICAL_AND())
                and_inst.lhs = ir_val_inst(cmp_id)
                and_inst.rhs = ir_val_inst(guard_result.val_id)
                and_inst.result_type = "bool"
                and_inst.has_result = true
                b.instructions.push(and_inst)
                g.builder.next_id = g.builder.next_id + 1

                b.terminator = Box_new(term_br_cond(ir_val_inst(and_id), arm_label, next_label))
                b.has_terminator = true
            } else {
                b.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), arm_label, next_label))
                b.has_terminator = true
            }

            -- Generate arm body in arm_block
            let mut arm_block = ir_block_new(arm_label)
            if arm.is_expr_body {
                let body_result = generate_expr(g, arm_block, *arm.body_expr)
                g = body_result.gen
                arm_block = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                arm_block.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, arm_block, arm.body)
                g = body_gen.gen
                arm_block = body_gen.block
            }

            if arm_block.has_terminator == false {
                arm_block.terminator = Box_new(term_br(join_label))
                arm_block.has_terminator = true
            }

            -- Continue with next_label as the new current block
            b = ir_block_new(next_label)
            arm_idx = arm_idx + 1
        } else {
            -- Unsupported pattern kind in literal match, skip
            arm_idx = arm_idx + 1
        }
    }

    -- If we fell through all arms without a wildcard, branch to join
    if b.has_terminator == false {
        b.terminator = Box_new(term_br(join_label))
        b.has_terminator = true
    }

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- ENUM MATCH
-- ============================================================

fn generate_enum_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, enum_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get enum definition
    if g.enum_defs.contains(enum_type) == false {
        return generate_literal_match(g, b, expr, scrut_id)
    }
    let enum_info = g.enum_defs.get(enum_type)

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Extract tag from scrutinee (field 0)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(scrut_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    let join_label = gen_next_label(g, "ematch_end")
    g = gen_advance_label(g)

    -- Process each arm
    let mut arm_idx = 0
    while arm_idx < expr.match_arms.len() {
        let arm = expr.match_arms[arm_idx]
        let pat = arm.pattern

        let arm_label = gen_next_label(g, "ematch_arm")
        g = gen_advance_label(g)
        let next_label = gen_next_label(g, "ematch_next")
        g = gen_advance_label(g)

        if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            -- Catch-all: bind if identifier
            if pat.kind == PAT_IDENTIFIER() {
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = "i64"
                bind_alloca.result_type = "ptr(i64)"
                bind_alloca.has_result = true
                b.instructions.push(bind_alloca)
                g.builder.next_id = g.builder.next_id + 1

                let mut bind_store = ir_inst_new(g.builder.next_id, OP_STORE())
                bind_store.store_ptr = ir_val_inst(bind_alloca_id)
                bind_store.store_val = ir_val_inst(scrut_id)
                bind_store.has_result = false
                b.instructions.push(bind_store)
                g.builder.next_id = g.builder.next_id + 1

                g.variable_map.insert(pat.name, bind_alloca_id)
                g.variable_types.insert(pat.name, "i64")
            }

            -- Generate arm body in current block
            if arm.is_expr_body {
                let body_result = generate_expr(g, b, *arm.body_expr)
                g = body_result.gen
                b = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                b.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, b, arm.body)
                g = body_gen.gen
                b = body_gen.block
            }

            if b.has_terminator == false {
                b.terminator = Box_new(term_br(join_label))
                b.has_terminator = true
            }
            arm_idx = expr.match_arms.len()
        } else if pat.kind == PAT_ENUM_VARIANT() {
            -- Find variant tag value from enum definition
            let variant_name = pat.variant
            let mut variant_tag = -1
            let mut vi = 0
            while vi < enum_info.variant_names.len() {
                if enum_info.variant_names[vi] == variant_name {
                    variant_tag = enum_info.variant_tags[vi]
                }
                vi = vi + 1
            }

            -- Compare tag with variant tag
            let cmp_id = g.builder.next_id
            let mut cmp_inst = ir_inst_new(cmp_id, OP_EQ())
            cmp_inst.lhs = ir_val_inst(tag_id)
            cmp_inst.rhs = ir_val_int(variant_tag)
            cmp_inst.result_type = "bool"
            cmp_inst.has_result = true
            b.instructions.push(cmp_inst)
            g.builder.next_id = g.builder.next_id + 1

            b.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), arm_label, next_label))
            b.has_terminator = true

            -- Generate arm body in arm_block
            let mut arm_block = ir_block_new(arm_label)

            -- Bind payload variables if present
            if pat.bindings.len() > 0 {
                -- Extract payload from scrutinee (field 1 for first payload)
                let payload_id = g.builder.next_id
                let mut payload_inst = ir_inst_new(payload_id, OP_EXTRACT_FIELD())
                payload_inst.field_base = ir_val_inst(scrut_id)
                payload_inst.field_index = 1
                payload_inst.field_type = "i64"
                payload_inst.result_type = "i64"
                payload_inst.has_result = true
                arm_block.instructions.push(payload_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Bind first payload to the first binding name
                let bind_name = pat.bindings[0]
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = "i64"
                bind_alloca.result_type = "ptr(i64)"
                bind_alloca.has_result = true
                arm_block.instructions.push(bind_alloca)
                g.builder.next_id = g.builder.next_id + 1

                let mut bind_store = ir_inst_new(g.builder.next_id, OP_STORE())
                bind_store.store_ptr = ir_val_inst(bind_alloca_id)
                bind_store.store_val = ir_val_inst(payload_id)
                bind_store.has_result = false
                arm_block.instructions.push(bind_store)
                g.builder.next_id = g.builder.next_id + 1

                g.variable_map.insert(bind_name, bind_alloca_id)
                g.variable_types.insert(bind_name, "i64")
            }

            if arm.is_expr_body {
                let body_result = generate_expr(g, arm_block, *arm.body_expr)
                g = body_result.gen
                arm_block = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                arm_block.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, arm_block, arm.body)
                g = body_gen.gen
                arm_block = body_gen.block
            }

            if arm_block.has_terminator == false {
                arm_block.terminator = Box_new(term_br(join_label))
                arm_block.has_terminator = true
            }

            -- Move to next arm's block
            b = ir_block_new(next_label)
            arm_idx = arm_idx + 1
        } else {
            arm_idx = arm_idx + 1
        }
    }

    -- If we fell through all arms, branch to join
    if b.has_terminator == false {
        b.terminator = Box_new(term_br(join_label))
        b.has_terminator = true
    }

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- RESULT MATCH (Ok/Err patterns)
-- ============================================================

fn generate_result_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Extract tag from scrutinee (field 0: 0=Ok, 1=Err)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(scrut_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Compare tag to 0 (Ok)
    let is_ok_id = g.builder.next_id
    let mut is_ok_cmp = ir_inst_new(is_ok_id, OP_EQ())
    is_ok_cmp.lhs = ir_val_inst(tag_id)
    is_ok_cmp.rhs = ir_val_int(0)
    is_ok_cmp.result_type = "bool"
    is_ok_cmp.has_result = true
    b.instructions.push(is_ok_cmp)
    g.builder.next_id = g.builder.next_id + 1

    let ok_label = gen_next_label(g, "result_ok")
    g = gen_advance_label(g)
    let err_label = gen_next_label(g, "result_err")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "result_end")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(is_ok_id), ok_label, err_label))
    b.has_terminator = true

    -- Find Ok and Err arms
    let mut ok_arm_idx = -1
    let mut err_arm_idx = -1
    let mut wildcard_arm_idx = -1
    let mut ai = 0
    while ai < expr.match_arms.len() {
        let arm = expr.match_arms[ai]
        let pat = arm.pattern
        if pat.kind == PAT_ENUM_VARIANT() {
            if pat.variant == "Ok" {
                ok_arm_idx = ai
            } else if pat.variant == "Err" {
                err_arm_idx = ai
            }
        } else if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            wildcard_arm_idx = ai
        }
        ai = ai + 1
    }

    -- Generate Ok arm
    let mut ok_block = ir_block_new(ok_label)
    if ok_arm_idx >= 0 {
        let ok_arm = expr.match_arms[ok_arm_idx]
        let ok_pat = ok_arm.pattern

        -- Extract Ok payload (field 1) and bind
        if ok_pat.bindings.len() > 0 {
            let ok_val_id = g.builder.next_id
            let mut ok_extract = ir_inst_new(ok_val_id, OP_EXTRACT_FIELD())
            ok_extract.field_base = ir_val_inst(scrut_id)
            ok_extract.field_index = 1
            ok_extract.field_type = "i64"
            ok_extract.result_type = "i64"
            ok_extract.has_result = true
            ok_block.instructions.push(ok_extract)
            g.builder.next_id = g.builder.next_id + 1

            let bind_name = ok_pat.bindings[0]
            let ok_bind_id = g.builder.next_id
            let mut ok_bind = ir_inst_new(ok_bind_id, OP_ALLOCA())
            ok_bind.alloc_type = "i64"
            ok_bind.result_type = "ptr(i64)"
            ok_bind.has_result = true
            ok_block.instructions.push(ok_bind)
            g.builder.next_id = g.builder.next_id + 1

            let mut ok_store = ir_inst_new(g.builder.next_id, OP_STORE())
            ok_store.store_ptr = ir_val_inst(ok_bind_id)
            ok_store.store_val = ir_val_inst(ok_val_id)
            ok_store.has_result = false
            ok_block.instructions.push(ok_store)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(bind_name, ok_bind_id)
            g.variable_types.insert(bind_name, "i64")
        }

        if ok_arm.is_expr_body {
            let body_result = generate_expr(g, ok_block, *ok_arm.body_expr)
            g = body_result.gen
            ok_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            ok_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, ok_block, ok_arm.body)
            g = body_gen.gen
            ok_block = body_gen.block
        }
    }
    if ok_block.has_terminator == false {
        ok_block.terminator = Box_new(term_br(join_label))
        ok_block.has_terminator = true
    }

    -- Generate Err arm
    let mut err_block = ir_block_new(err_label)
    if err_arm_idx >= 0 {
        let err_arm = expr.match_arms[err_arm_idx]
        let err_pat = err_arm.pattern

        -- Extract Err payload (field 2 for error value) and bind
        if err_pat.bindings.len() > 0 {
            let err_val_id = g.builder.next_id
            let mut err_extract = ir_inst_new(err_val_id, OP_EXTRACT_FIELD())
            err_extract.field_base = ir_val_inst(scrut_id)
            err_extract.field_index = 2
            err_extract.field_type = "i64"
            err_extract.result_type = "i64"
            err_extract.has_result = true
            err_block.instructions.push(err_extract)
            g.builder.next_id = g.builder.next_id + 1

            let bind_name = err_pat.bindings[0]
            let err_bind_id = g.builder.next_id
            let mut err_bind = ir_inst_new(err_bind_id, OP_ALLOCA())
            err_bind.alloc_type = "i64"
            err_bind.result_type = "ptr(i64)"
            err_bind.has_result = true
            err_block.instructions.push(err_bind)
            g.builder.next_id = g.builder.next_id + 1

            let mut err_store = ir_inst_new(g.builder.next_id, OP_STORE())
            err_store.store_ptr = ir_val_inst(err_bind_id)
            err_store.store_val = ir_val_inst(err_val_id)
            err_store.has_result = false
            err_block.instructions.push(err_store)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(bind_name, err_bind_id)
            g.variable_types.insert(bind_name, "i64")
        }

        if err_arm.is_expr_body {
            let body_result = generate_expr(g, err_block, *err_arm.body_expr)
            g = body_result.gen
            err_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            err_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, err_block, err_arm.body)
            g = body_gen.gen
            err_block = body_gen.block
        }
    } else if wildcard_arm_idx >= 0 {
        -- Use wildcard arm for Err path
        let wild_arm = expr.match_arms[wildcard_arm_idx]
        if wild_arm.is_expr_body {
            let body_result = generate_expr(g, err_block, *wild_arm.body_expr)
            g = body_result.gen
            err_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            err_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, err_block, wild_arm.body)
            g = body_gen.gen
            err_block = body_gen.block
        }
    }
    if err_block.has_terminator == false {
        err_block.terminator = Box_new(term_br(join_label))
        err_block.has_terminator = true
    }

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- OPTION MATCH (Some/None patterns)
-- ============================================================

fn generate_option_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Extract tag (field 0: 0=Some, 1=None)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(scrut_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Compare tag to 0 (Some)
    let is_some_id = g.builder.next_id
    let mut is_some_cmp = ir_inst_new(is_some_id, OP_EQ())
    is_some_cmp.lhs = ir_val_inst(tag_id)
    is_some_cmp.rhs = ir_val_int(0)
    is_some_cmp.result_type = "bool"
    is_some_cmp.has_result = true
    b.instructions.push(is_some_cmp)
    g.builder.next_id = g.builder.next_id + 1

    let some_label = gen_next_label(g, "option_some")
    g = gen_advance_label(g)
    let none_label = gen_next_label(g, "option_none")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "option_end")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(is_some_id), some_label, none_label))
    b.has_terminator = true

    -- Find Some and None arms
    let mut some_arm_idx = -1
    let mut none_arm_idx = -1
    let mut wildcard_arm_idx = -1
    let mut ai = 0
    while ai < expr.match_arms.len() {
        let arm = expr.match_arms[ai]
        let pat = arm.pattern
        if pat.kind == PAT_ENUM_VARIANT() {
            if pat.variant == "Some" {
                some_arm_idx = ai
            } else if pat.variant == "None" {
                none_arm_idx = ai
            }
        } else if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            wildcard_arm_idx = ai
        }
        ai = ai + 1
    }

    -- Generate Some arm
    let mut some_block = ir_block_new(some_label)
    if some_arm_idx >= 0 {
        let some_arm = expr.match_arms[some_arm_idx]
        let some_pat = some_arm.pattern

        -- Extract Some payload (field 1) and bind
        if some_pat.bindings.len() > 0 {
            let some_val_id = g.builder.next_id
            let mut some_extract = ir_inst_new(some_val_id, OP_EXTRACT_FIELD())
            some_extract.field_base = ir_val_inst(scrut_id)
            some_extract.field_index = 1
            some_extract.field_type = "i64"
            some_extract.result_type = "i64"
            some_extract.has_result = true
            some_block.instructions.push(some_extract)
            g.builder.next_id = g.builder.next_id + 1

            let bind_name = some_pat.bindings[0]
            let some_bind_id = g.builder.next_id
            let mut some_bind = ir_inst_new(some_bind_id, OP_ALLOCA())
            some_bind.alloc_type = "i64"
            some_bind.result_type = "ptr(i64)"
            some_bind.has_result = true
            some_block.instructions.push(some_bind)
            g.builder.next_id = g.builder.next_id + 1

            let mut some_store = ir_inst_new(g.builder.next_id, OP_STORE())
            some_store.store_ptr = ir_val_inst(some_bind_id)
            some_store.store_val = ir_val_inst(some_val_id)
            some_store.has_result = false
            some_block.instructions.push(some_store)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(bind_name, some_bind_id)
            g.variable_types.insert(bind_name, "i64")
        }

        if some_arm.is_expr_body {
            let body_result = generate_expr(g, some_block, *some_arm.body_expr)
            g = body_result.gen
            some_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            some_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, some_block, some_arm.body)
            g = body_gen.gen
            some_block = body_gen.block
        }
    }
    if some_block.has_terminator == false {
        some_block.terminator = Box_new(term_br(join_label))
        some_block.has_terminator = true
    }

    -- Generate None arm
    let mut none_block = ir_block_new(none_label)
    if none_arm_idx >= 0 {
        let none_arm = expr.match_arms[none_arm_idx]

        if none_arm.is_expr_body {
            let body_result = generate_expr(g, none_block, *none_arm.body_expr)
            g = body_result.gen
            none_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            none_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, none_block, none_arm.body)
            g = body_gen.gen
            none_block = body_gen.block
        }
    } else if wildcard_arm_idx >= 0 {
        let wild_arm = expr.match_arms[wildcard_arm_idx]
        if wild_arm.is_expr_body {
            let body_result = generate_expr(g, none_block, *wild_arm.body_expr)
            g = body_result.gen
            none_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            none_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, none_block, wild_arm.body)
            g = body_gen.gen
            none_block = body_gen.block
        }
    }
    if none_block.has_terminator == false {
        none_block.terminator = Box_new(term_br(join_label))
        none_block.has_terminator = true
    }

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- BLOCK EXPRESSION
-- ============================================================

fn generate_block_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate all statements in the block
    let stmts_result = generate_block_stmts(g, b, expr.stmts)
    g = stmts_result.gen
    b = stmts_result.block

    -- If the block has a result expression, generate it and return its value
    if expr.has_result {
        let result = generate_expr(g, b, *expr.result)
        return result
    }

    -- No result expression: return undef-like value (alloca of 0)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_int(0)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let load_id = g.builder.next_id
    let mut load = ir_inst_new(load_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "i64"
    load.result_type = "i64"
    load.has_result = true
    b.instructions.push(load)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, load_id)
}

-- ============================================================
-- LAMBDA EXPRESSION
-- Creates a new IR function for the lambda and returns a
-- reference to it. The lambda is lifted to a module-level
-- function with a unique name.
-- ============================================================

fn generate_lambda_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate unique lambda name
    let lambda_name = gen_next_lambda_name(g)
    g = gen_advance_lambda(g)

    -- Determine return type
    let ret_type = if len(expr.lambda_ret_type) > 0 {
        map_type_name(expr.lambda_ret_type)
    } else {
        "i64"
    }

    -- Create a new IR function for the lambda
    let mut lambda_func = ir_function_new(lambda_name, ret_type)

    -- Add lambda parameters
    let mut pi = 0
    while pi < expr.lambda_params.len() {
        let lp = expr.lambda_params[pi]
        let ptype = if len(lp.type_name) > 0 {
            map_type_name(lp.type_name)
        } else {
            "i64"
        }
        lambda_func.params.push(ir_param_new(lp.name, ptype))
        pi = pi + 1
    }

    -- Collect captured variables from the current scope
    -- Build a pipe-delimited list of captured variable names
    let mut captures = ""
    let mut capture_names: List[string] = []
    let mut capture_types: List[string] = []
    let mut keys = g.variable_map.keys()
    let mut ki = 0
    while ki < keys.len() {
        let var_name = keys[ki]
        -- Skip lambda parameters (they are not captures)
        let mut is_param = false
        let mut pj = 0
        while pj < expr.lambda_params.len() {
            if expr.lambda_params[pj].name == var_name {
                is_param = true
            }
            pj = pj + 1
        }
        if is_param == false {
            capture_names.push(var_name)
            let cap_type = if g.variable_types.contains(var_name) {
                g.variable_types.get(var_name)
            } else {
                "i64"
            }
            capture_types.push(cap_type)
            lambda_func.params.push(ir_param_new(var_name, cap_type))

            if len(captures) > 0 {
                captures = captures + "|"
            }
            captures = captures + var_name
        }
        ki = ki + 1
    }

    -- Save current generator state
    let saved_variable_map = g.variable_map
    let saved_variable_types = g.variable_types
    let saved_return_type = g.current_return_type
    let saved_next_id = g.builder.next_id

    -- Set up new variable scope for the lambda body
    g.variable_map = Map_new()
    g.variable_types = Map_new()
    g.current_return_type = ret_type
    g.builder.next_id = 0

    -- Create entry block for the lambda
    let mut entry_block = ir_block_new("entry")

    -- Create allocas and stores for all parameters (lambda params + captures)
    let total_params = expr.lambda_params.len() + capture_names.len()
    let mut param_idx = 0

    -- Lambda parameters first
    while param_idx < expr.lambda_params.len() {
        let lp = expr.lambda_params[param_idx]
        let ptype = if len(lp.type_name) > 0 {
            map_type_name(lp.type_name)
        } else {
            "i64"
        }

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = ptype
        alloca_inst.result_type = "ptr(" + ptype + ")"
        alloca_inst.has_result = true
        entry_block.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_param(param_idx)
        store_inst.has_result = false
        entry_block.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        g.variable_map.insert(lp.name, alloca_id)
        g.variable_types.insert(lp.name, ptype)
        param_idx = param_idx + 1
    }

    -- Captured variables next
    let mut cap_idx = 0
    while cap_idx < capture_names.len() {
        let cap_name = capture_names[cap_idx]
        let cap_type = capture_types[cap_idx]
        let full_param_idx = expr.lambda_params.len() + cap_idx

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = cap_type
        alloca_inst.result_type = "ptr(" + cap_type + ")"
        alloca_inst.has_result = true
        entry_block.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_param(full_param_idx)
        store_inst.has_result = false
        entry_block.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        g.variable_map.insert(cap_name, alloca_id)
        g.variable_types.insert(cap_name, cap_type)
        cap_idx = cap_idx + 1
    }

    -- Generate the lambda body expression
    let body_result = generate_expr(g, entry_block, *expr.lambda_body)
    g = body_result.gen
    entry_block = body_result.block

    -- Add return terminator
    if entry_block.has_terminator == false {
        if ret_type == "void" {
            entry_block.terminator = Box_new(term_ret_void())
        } else {
            entry_block.terminator = Box_new(term_ret(ir_val_inst(body_result.val_id)))
        }
        entry_block.has_terminator = true
    }

    -- Add entry block to the lambda function and push to module
    lambda_func.blocks.push(entry_block)
    g.ir_mod.functions.push(lambda_func)

    -- Restore generator state
    g.variable_map = saved_variable_map
    g.variable_types = saved_variable_types
    g.current_return_type = saved_return_type
    g.builder.next_id = saved_next_id

    -- Record the lambda in fn_ptr_vars and lambda_captures
    g.fn_ptr_vars.insert(lambda_name, lambda_name)
    if len(captures) > 0 {
        g.lambda_captures.insert(lambda_name, captures)
    }

    -- Return a global reference to the lambda function
    let ref_id = g.builder.next_id
    let mut ref_inst = ir_inst_new(ref_id, OP_ALLOCA())
    ref_inst.alloc_type = "ptr"
    ref_inst.result_type = "ptr(ptr)"
    ref_inst.has_result = true
    b.instructions.push(ref_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_ref = ir_inst_new(g.builder.next_id, OP_STORE())
    store_ref.store_ptr = ir_val_inst(ref_id)
    store_ref.store_val = ir_val_global(lambda_name)
    store_ref.has_result = false
    b.instructions.push(store_ref)
    g.builder.next_id = g.builder.next_id + 1

    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(ref_id)
    load_inst.load_type = "ptr"
    load_inst.result_type = "ptr"
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, load_id)
}

-- ============================================================
-- PIPELINE OPERATOR
-- x |> f desugars to f(x)
-- x |> f(y) desugars to f(x, y)
-- ============================================================

fn generate_pipeline(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate the left side (the argument to prepend)
    let left_result = generate_expr(g, b, *expr.left)
    g = left_result.gen
    b = left_result.block

    let pipe_right = *expr.pipe_right

    -- Case 1: Right side is a function call with args -> prepend left as first arg
    if pipe_right.kind == EXPR_FUNCTION_CALL() {
        let callee = *pipe_right.callee
        let fn_name = if callee.kind == EXPR_IDENTIFIER() {
            callee.name
        } else {
            ""
        }

        -- Build argument list: left value first, then original args
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(left_result.val_id))

        let mut i = 0
        while i < pipe_right.args.len() {
            let arg_result = generate_expr(g, b, pipe_right.args[i])
            g = arg_result.gen
            b = arg_result.block
            call_args.push(ir_val_inst(arg_result.val_id))
            i = i + 1
        }

        -- Determine return type
        let ret_type = if is_builtin_fn(fn_name) {
            builtin_return_type(fn_name)
        } else {
            "i64"
        }

        -- Map builtin name if needed
        let c_name = if is_builtin_fn(fn_name) {
            map_builtin_name(fn_name)
        } else {
            fn_name
        }

        let call_id = g.builder.next_id
        let mut call_inst = ir_inst_new(call_id, OP_CALL())
        call_inst.callee = c_name
        call_inst.call_args = call_args
        call_inst.result_type = ret_type
        call_inst.has_result = ret_type != "void"
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, call_id)
    }

    -- Case 2: Right side is an identifier -> call it with left as sole arg
    if pipe_right.kind == EXPR_IDENTIFIER() {
        let fn_name = pipe_right.name

        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(left_result.val_id))

        let ret_type = if is_builtin_fn(fn_name) {
            builtin_return_type(fn_name)
        } else {
            "i64"
        }

        let c_name = if is_builtin_fn(fn_name) {
            map_builtin_name(fn_name)
        } else {
            fn_name
        }

        let call_id = g.builder.next_id
        let mut call_inst = ir_inst_new(call_id, OP_CALL())
        call_inst.callee = c_name
        call_inst.call_args = call_args
        call_inst.result_type = ret_type
        call_inst.has_result = ret_type != "void"
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, call_id)
    }

    -- Case 3: Right side is a lambda variable -> call_ptr with left as arg
    if pipe_right.kind == EXPR_IDENTIFIER() {
        let fn_name = pipe_right.name
        if g.fn_ptr_vars.contains(fn_name) {
            let lambda_fn = g.fn_ptr_vars.get(fn_name)

            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(left_result.val_id))

            let call_id = g.builder.next_id
            let mut call_inst = ir_inst_new(call_id, OP_CALL())
            call_inst.callee = lambda_fn
            call_inst.call_args = call_args
            call_inst.result_type = "i64"
            call_inst.has_result = true
            b.instructions.push(call_inst)
            g.builder.next_id = g.builder.next_id + 1

            return expr_result(g, b, call_id)
        }
    }

    -- Fallback: just return the left value (pipeline target not recognized)
    return expr_result(g, b, left_result.val_id)
}

-- ============================================================
-- ERROR PROPAGATION (? operator)
-- expr? on Result[T, E] unwraps Ok or early-returns Err
-- expr? on Option[T] unwraps Some or early-returns None
-- ============================================================

fn generate_error_propagate(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate the operand (the Result/Option expression)
    let operand_result = generate_expr(g, b, *expr.operand)
    g = operand_result.gen
    b = operand_result.block
    let operand_id = operand_result.val_id

    -- Extract tag from the result/option (field 0)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(operand_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Compare tag: 0 = Ok/Some (success), nonzero = Err/None (failure)
    let is_ok_id = g.builder.next_id
    let mut is_ok_cmp = ir_inst_new(is_ok_id, OP_EQ())
    is_ok_cmp.lhs = ir_val_inst(tag_id)
    is_ok_cmp.rhs = ir_val_int(0)
    is_ok_cmp.result_type = "bool"
    is_ok_cmp.has_result = true
    b.instructions.push(is_ok_cmp)
    g.builder.next_id = g.builder.next_id + 1

    -- Create labels for ok, err, and continue paths
    let ok_label = gen_next_label(g, "prop_ok")
    g = gen_advance_label(g)
    let err_label = gen_next_label(g, "prop_err")
    g = gen_advance_label(g)
    let cont_label = gen_next_label(g, "prop_cont")
    g = gen_advance_label(g)

    -- Branch: if tag == 0 go to ok, else go to err
    b.terminator = Box_new(term_br_cond(ir_val_inst(is_ok_id), ok_label, err_label))
    b.has_terminator = true
    g.completed_blocks.push(b)

    -- Err path: extract error value and early-return it from the function
    let mut err_block = ir_block_new(err_label)

    -- Extract the error payload (field 2 for Result, or undef for Option)
    let err_val_id = g.builder.next_id
    let mut err_extract = ir_inst_new(err_val_id, OP_EXTRACT_FIELD())
    err_extract.field_base = ir_val_inst(operand_id)
    err_extract.field_index = 2
    err_extract.field_type = "i64"
    err_extract.result_type = "i64"
    err_extract.has_result = true
    err_block.instructions.push(err_extract)
    g.builder.next_id = g.builder.next_id + 1

    -- Build an error return value: construct a Result/Option with error tag
    -- For simplicity, we reconstruct a tagged struct {tag=1, payload=0, error=err_val}
    -- and return it. The LLVM backend will handle struct construction.
    -- Here we return the operand directly (it already has the Err tag).
    err_block.terminator = Box_new(term_ret(ir_val_inst(operand_id)))
    err_block.has_terminator = true
    g.completed_blocks.push(err_block)

    -- Ok path: extract the Ok/Some value (field 1)
    let mut ok_block = ir_block_new(ok_label)
    let ok_val_id = g.builder.next_id
    let mut ok_extract = ir_inst_new(ok_val_id, OP_EXTRACT_FIELD())
    ok_extract.field_base = ir_val_inst(operand_id)
    ok_extract.field_index = 1
    ok_extract.field_type = "i64"
    ok_extract.result_type = "i64"
    ok_extract.has_result = true
    ok_block.instructions.push(ok_extract)
    g.builder.next_id = g.builder.next_id + 1

    -- Store ok value so it can be used after the branch
    let ok_alloca_id = g.builder.next_id
    let mut ok_alloca = ir_inst_new(ok_alloca_id, OP_ALLOCA())
    ok_alloca.alloc_type = "i64"
    ok_alloca.result_type = "ptr(i64)"
    ok_alloca.has_result = true
    ok_block.instructions.push(ok_alloca)
    g.builder.next_id = g.builder.next_id + 1

    let mut ok_store = ir_inst_new(g.builder.next_id, OP_STORE())
    ok_store.store_ptr = ir_val_inst(ok_alloca_id)
    ok_store.store_val = ir_val_inst(ok_val_id)
    ok_store.has_result = false
    ok_block.instructions.push(ok_store)
    g.builder.next_id = g.builder.next_id + 1

    ok_block.terminator = Box_new(term_br(cont_label))
    ok_block.has_terminator = true
    g.completed_blocks.push(ok_block)

    -- Continue block: load the ok value
    let mut cont_block = ir_block_new(cont_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(ok_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    cont_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, cont_block, load_id)
}

-- ============================================================
-- STRING INTERPOLATION
-- f"Hello {name}, you are {age} years old"
-- Alternating literal and expression parts concatenated.
-- ============================================================

fn generate_string_interp(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Start with an empty string
    let empty_id = g.builder.next_id
    let mut empty_inst = ir_inst_new(empty_id, OP_ALLOCA())
    empty_inst.alloc_type = "string"
    empty_inst.result_type = "ptr(string)"
    empty_inst.has_result = true
    b.instructions.push(empty_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut empty_store = ir_inst_new(g.builder.next_id, OP_STORE())
    empty_store.store_ptr = ir_val_inst(empty_id)
    empty_store.store_val = ir_val_string("")
    empty_store.has_result = false
    b.instructions.push(empty_store)
    g.builder.next_id = g.builder.next_id + 1

    let mut empty_load = ir_inst_new(g.builder.next_id, OP_LOAD())
    empty_load.load_ptr = ir_val_inst(empty_id)
    empty_load.load_type = "string"
    empty_load.result_type = "string"
    empty_load.has_result = true
    b.instructions.push(empty_load)
    let mut running_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Process each part
    let mut i = 0
    while i < expr.interp_parts.len() {
        let part = expr.interp_parts[i]
        let is_literal = if i < expr.interp_is_literal.len() {
            expr.interp_is_literal[i]
        } else {
            false
        }

        if is_literal {
            -- Literal string part: generate directly
            let lit_result = generate_expr(g, b, part)
            g = lit_result.gen
            b = lit_result.block

            -- Concatenate with running string
            let concat_id = g.builder.next_id
            let mut concat_inst = ir_inst_new(concat_id, OP_STRING_CONCAT())
            concat_inst.lhs = ir_val_inst(running_id)
            concat_inst.rhs = ir_val_inst(lit_result.val_id)
            concat_inst.result_type = "string"
            concat_inst.has_result = true
            b.instructions.push(concat_inst)
            g.builder.next_id = g.builder.next_id + 1
            running_id = concat_id
        } else {
            -- Expression part: generate the expression
            let expr_result_val = generate_expr(g, b, part)
            g = expr_result_val.gen
            b = expr_result_val.block
            let part_val_id = expr_result_val.val_id

            -- Determine the expression type to decide if conversion is needed
            -- Check if the part is an identifier with a known type
            let part_type = infer_part_type(g, part)

            let mut str_val_id = part_val_id
            if part_type == "string" {
                -- Already a string, use directly
                str_val_id = part_val_id
            } else if part_type == "i64" or part_type == "int" {
                -- Convert int to string via int_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_int_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            } else if part_type == "f64" or part_type == "float" {
                -- Convert float to string via float_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_float_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            } else if part_type == "bool" {
                -- Convert bool to string via bool_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_bool_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            } else {
                -- Unknown type: try int_to_string as default
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_int_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            }

            -- Concatenate with running string
            let concat_id = g.builder.next_id
            let mut concat_inst = ir_inst_new(concat_id, OP_STRING_CONCAT())
            concat_inst.lhs = ir_val_inst(running_id)
            concat_inst.rhs = ir_val_inst(str_val_id)
            concat_inst.result_type = "string"
            concat_inst.has_result = true
            b.instructions.push(concat_inst)
            g.builder.next_id = g.builder.next_id + 1
            running_id = concat_id
        }

        i = i + 1
    }

    return expr_result(g, b, running_id)
}

-- Infer the type of a string interpolation part expression.
-- Checks identifiers against variable_types, and literals by kind.
fn infer_part_type(gen: IRGenerator, part: Expr) -> string {
    if part.kind == EXPR_LITERAL_STRING() {
        return "string"
    }
    if part.kind == EXPR_LITERAL_INT() {
        return "i64"
    }
    if part.kind == EXPR_LITERAL_FLOAT() {
        return "f64"
    }
    if part.kind == EXPR_LITERAL_BOOL() {
        return "bool"
    }
    if part.kind == EXPR_IDENTIFIER() {
        if gen.variable_types.contains(part.name) {
            return gen.variable_types.get(part.name)
        }
    }
    if part.kind == EXPR_FUNCTION_CALL() {
        let callee = *part.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let fn_name = callee.name
            if is_builtin_fn(fn_name) {
                return builtin_return_type(fn_name)
            }
        }
    }
    -- Default: assume int (most common)
    return "i64"
}

-- ============================================================
-- COMPTIME EXPRESSION
-- Evaluates the inner expression at compile time and emits
-- the result as a constant value.
-- ============================================================

fn generate_comptime_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Evaluate the inner expression at compile time
    let ct_val = eval_comptime_expr(g, *expr.operand)

    -- Determine the type of the compile-time value
    let ct_type = comptime_value_type(ct_val)

    -- Emit an alloca/store/load sequence for the compile-time constant
    let alloca_id = g.builder.next_id
    let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
    alloca_inst.alloc_type = ct_type
    alloca_inst.result_type = "ptr(" + ct_type + ")"
    alloca_inst.has_result = true
    b.instructions.push(alloca_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
    store_inst.store_ptr = ir_val_inst(alloca_id)
    store_inst.store_val = ct_val
    store_inst.has_result = false
    b.instructions.push(store_inst)
    g.builder.next_id = g.builder.next_id + 1

    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(alloca_id)
    load_inst.load_type = ct_type
    load_inst.result_type = ct_type
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, load_id)
}
