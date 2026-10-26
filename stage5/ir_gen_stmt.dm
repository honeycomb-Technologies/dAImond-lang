module ir_gen_stmt

import ast
import ir
import ir_builder
import ir_gen

-- ============================================================
-- GenResult: return both generator and current block
-- ============================================================

struct GenResult {
    gen: IRGenerator,
    block: IRBasicBlock
}

fn gen_result(g: IRGenerator, b: IRBasicBlock) -> GenResult {
    return GenResult { gen: g, block: b }
}

-- ============================================================
-- ExprResult: return generator, block, and result value ID
-- ============================================================

struct ExprResult {
    gen: IRGenerator,
    block: IRBasicBlock,
    val_id: int
}

fn expr_result(g: IRGenerator, b: IRBasicBlock, v: int) -> ExprResult {
    return ExprResult { gen: g, block: b, val_id: v }
}

-- ============================================================
-- STATEMENT GENERATION
-- ============================================================
fn resolve_field_original_type(gen: IRGenerator, expr: Expr) -> string {
    let obj = *expr.object
    let field = expr.field
    let mut sname = ""
    if obj.kind == EXPR_IDENTIFIER() {
        if gen.var_struct_types.contains(obj.name) {
            sname = gen.var_struct_types.get(obj.name)
        }
    }
    if sname == "" {
        let obj_type = infer_expr_type(gen, obj)
        if gen.struct_defs.contains(obj_type) {
            sname = obj_type
        }
    }
    if sname != "" and gen.struct_defs.contains(sname) {
        let sinfo = gen.struct_defs.get(sname)
        let mut fi = 0
        while fi < sinfo.field_names.len() {
            if sinfo.field_names[fi] == field {
                return sinfo.field_types[fi]
            }
            fi = fi + 1
        }
    }
    return ""
}

-- ============================================================
-- STATEMENT GENERATION
-- ============================================================

fn generate_statement(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    if stmt.kind == STMT_LET() {
        return generate_let_binding(gen, block, stmt)
    }
    if stmt.kind == STMT_RETURN() {
        return generate_return(gen, block, stmt)
    }
    if stmt.kind == STMT_IF() {
        return generate_if_statement(gen, block, stmt)
    }
    if stmt.kind == STMT_WHILE() {
        return generate_while_loop(gen, block, stmt)
    }
    if stmt.kind == STMT_FOR() {
        return generate_for_loop(gen, block, stmt)
    }
    if stmt.kind == STMT_LOOP() {
        return generate_loop_stmt(gen, block, stmt)
    }
    if stmt.kind == STMT_BREAK() {
        return generate_break(gen, block)
    }
    if stmt.kind == STMT_CONTINUE() {
        return generate_continue(gen, block)
    }
    if stmt.kind == STMT_ASSIGNMENT() {
        return generate_assignment(gen, block, stmt)
    }
    if stmt.kind == STMT_EXPRESSION() {
        let result = generate_expr(gen, block, *stmt.expr)
        return gen_result(result.gen, result.block)
    }
    if stmt.kind == STMT_MATCH() {
        let result = generate_match_expr(gen, block, *stmt.match_expr)
        return gen_result(result.gen, result.block)
    }
    if stmt.kind == STMT_REGION() {
        -- Region blocks: just generate the body (runtime handles arena)
        return generate_block_stmts(gen, block, stmt.region_body)
    }
    return gen_result(gen, block)
}

-- ============================================================
-- BLOCK STATEMENT GENERATION
-- ============================================================

fn generate_block_stmts(gen: IRGenerator, block: IRBasicBlock, stmts: List[Stmt]) -> GenResult {
    let mut g = gen
    let mut b = block
    let mut i = 0
    while i < stmts.len() {
        let result = generate_statement(g, b, stmts[i])
        g = result.gen
        b = result.block
        i = i + 1
    }
    return gen_result(g, b)
}

-- ============================================================
-- LET BINDING
-- ============================================================

fn generate_let_binding(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.let_name
    let mut type_str = stmt.let_type

    -- Normalize fixed-size array type [T; N] to List[T]
    if starts_with(type_str, "[") {
        let semi_pos = string_find(type_str, ";")
        if semi_pos > 0 {
            let inner_type = string_trim(substr(type_str, 1, semi_pos - 1))
            type_str = "List[" + inner_type + "]"
            g.array_var_names.insert(var_name, true)
        }
    }

    -- Detect List[T] type annotation
    if starts_with(type_str, "List[") {
        let inner = extract_inner_type(type_str, "List[")
        let elem_kind = classify_list_elem(inner)
        g.list_elem_kinds.insert(var_name, elem_kind)
        if elem_kind == LIST_ELEM_OTHER() {
            g.list_elem_types.insert(var_name, inner)
        }

        -- Determine list struct type name
        let list_type = if elem_kind == LIST_ELEM_INT() {
            "dm_list_int64"
        } else if elem_kind == LIST_ELEM_FLOAT() {
            "dm_list_double"
        } else if elem_kind == LIST_ELEM_STRING() {
            "dm_list_dm_string"
        } else {
            "dm_list_generic"
        }

        -- Alloca for list struct
        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = list_type
        alloca_inst.result_type = "ptr(" + list_type + ")"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Call new function to initialize
        let new_fn = if elem_kind == LIST_ELEM_INT() { "dm_list_int64_new" }
        else if elem_kind == LIST_ELEM_FLOAT() { "dm_list_double_new" }
        else if elem_kind == LIST_ELEM_STRING() { "dm_list_string_new" }
        else { "dm_list_generic_new" }
        let mut call_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        call_inst.callee = new_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(alloca_id))
        call_inst.call_args = call_args
        call_inst.result_type = "void"
        call_inst.has_result = false
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- If the initializer is a non-empty array literal, push elements into the list
        let init_expr = *stmt.let_value
        if init_expr.kind == EXPR_ARRAY_LITERAL() and init_expr.elements.len() > 0 {
            let push_fn_name = if elem_kind == LIST_ELEM_INT() { "dm_list_int64_push" }
            else if elem_kind == LIST_ELEM_FLOAT() { "dm_list_double_push" }
            else if elem_kind == LIST_ELEM_STRING() { "llvm_dm_list_string_push" }
            else { "dm_list_generic_push" }
            let mut ei = 0
            while ei < init_expr.elements.len() {
                let elem_result = generate_expr(g, b, init_expr.elements[ei])
                g = elem_result.gen
                b = elem_result.block
                let mut push_call_inst = ir_inst_new(g.builder.next_id, OP_CALL())
                push_call_inst.callee = push_fn_name
                let mut push_call_args: List[IRValue] = []
                push_call_args.push(ir_val_inst(alloca_id))
                push_call_args.push(ir_val_inst(elem_result.val_id))
                push_call_inst.call_args = push_call_args
                push_call_inst.result_type = "void"
                push_call_inst.has_result = false
                b.instructions.push(push_call_inst)
                g.builder.next_id = g.builder.next_id + 1
                ei = ei + 1
            }
        }

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, list_type)
        return gen_result(g, b)
    }

    -- Detect Map[K,V] type annotation
    if starts_with(type_str, "Map[") {
        let inner = extract_inner_type(type_str, "Map[")
        -- Determine map kind from inner type string
        let map_kind = if starts_with(inner, "string") {
            MAP_KIND_STRING_INT()
        } else {
            MAP_KIND_INT_STRING()
        }
        g.map_var_kinds.insert(var_name, map_kind)

        let map_type = if map_kind == MAP_KIND_STRING_INT() {
            "dm_map_string_int"
        } else {
            "dm_map_int_string"
        }

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = map_type
        alloca_inst.result_type = "ptr(" + map_type + ")"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Initialize map
        let new_fn = map_type + "_new"
        let mut call_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        call_inst.callee = new_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(alloca_id))
        call_inst.call_args = call_args
        call_inst.result_type = "void"
        call_inst.has_result = false
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, map_type)
        return gen_result(g, b)
    }

    -- Detect Box[T] type annotation
    if starts_with(type_str, "Box[") {
        let inner = extract_inner_type(type_str, "Box[")
        g.box_inner_types.insert(var_name, inner)

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = "ptr"
        alloca_inst.result_type = "ptr(ptr)"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Generate and store the value if present
        let val_expr = *stmt.let_value
        if val_expr.kind != EXPR_LITERAL_NULL() {
            let result = generate_expr(g, b, val_expr)
            g = result.gen
            b = result.block
            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(alloca_id)
            store_inst.store_val = ir_val_inst(result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1
        }

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, "ptr")
        return gen_result(g, b)
    }

    -- Detect dyn Trait type annotation
    if starts_with(type_str, "dyn ") {
        let trait_name = substr(type_str, 4, len(type_str) - 4)

        let val_expr = *stmt.let_value
        -- Infer concrete type from value expression
        let mut concrete = ""
        if val_expr.kind == EXPR_IDENTIFIER() {
            if g.var_struct_types.contains(val_expr.name) {
                concrete = g.var_struct_types.get(val_expr.name)
            }
        } else if val_expr.kind == EXPR_STRUCT_LITERAL() {
            concrete = val_expr.type_name
        }

        -- Use concrete struct type for alloca so the struct is stored directly
        let alloc_type = if len(concrete) > 0 { concrete } else { "ptr" }
        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = alloc_type
        alloca_inst.result_type = "ptr(" + alloc_type + ")"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        if val_expr.kind != EXPR_LITERAL_NULL() {
            let result = generate_expr(g, b, val_expr)
            g = result.gen
            b = result.block

            if len(concrete) > 0 {
                g.dyn_var_concrete.insert(var_name, concrete)
                g.dyn_var_traits.insert(var_name, trait_name)
            }

            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(alloca_id)
            store_inst.store_val = ir_val_inst(result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1
        }

        g.variable_map.insert(var_name, alloca_id)
        if len(concrete) > 0 {
            g.variable_types.insert(var_name, concrete)
            g.var_struct_types.insert(var_name, concrete)
        } else {
            g.variable_types.insert(var_name, "ptr")
        }
        return gen_result(g, b)
    }

    -- Regular let binding: infer type from annotation or value expression
    let infer_val = *stmt.let_value

    -- Early handling: method calls that return lists (.keys(), .values())
    -- These produce list allocas directly; bypass normal alloca+store
    if len(type_str) == 0 and infer_val.kind == EXPR_METHOD_CALL() {
        let early_method = infer_val.method
        if early_method == "keys" or early_method == "values" {
            let early_obj = *infer_val.object
            if early_obj.kind == EXPR_IDENTIFIER() {
                let early_obj_name = early_obj.name
                if g.map_var_kinds.contains(early_obj_name) {
                    let early_mk = g.map_var_kinds.get(early_obj_name)
                    let result = generate_expr(g, b, infer_val)
                    g = result.gen
                    b = result.block
                    if early_method == "keys" {
                        if early_mk == MAP_KIND_STRING_INT() {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_STRING())
                        } else {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_INT())
                        }
                    } else {
                        if early_mk == MAP_KIND_STRING_INT() {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_INT())
                        } else {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_STRING())
                        }
                    }
                    g.variable_map.insert(var_name, result.val_id)
                    g.variable_types.insert(var_name, "list")
                    return gen_result(g, b)
                }
            }
        }
    }

    -- Early handling: string_split() calls return list allocas directly
    if len(type_str) == 0 and infer_val.kind == EXPR_FUNCTION_CALL() {
        let early_callee = *infer_val.callee
        if early_callee.kind == EXPR_IDENTIFIER() and early_callee.name == "string_split" {
            let result = generate_expr(g, b, infer_val)
            g = result.gen
            b = result.block
            g.list_elem_kinds.insert(var_name, LIST_ELEM_STRING())
            g.variable_map.insert(var_name, result.val_id)
            g.variable_types.insert(var_name, "list")
            return gen_result(g, b)
        }
    }

    -- Early handling: array literal [elem1, elem2, ...] without type annotation
    -- Treat as List[T] where T is inferred from the first element
    if infer_val.kind == EXPR_ARRAY_LITERAL() and infer_val.elements.len() > 0 {
        let arr_elem_type = infer_expr_type(g, infer_val.elements[0])
        let arr_elem_kind = if arr_elem_type == "i64" or arr_elem_type == "int" { LIST_ELEM_INT() }
            else if arr_elem_type == "f64" or arr_elem_type == "float" { LIST_ELEM_FLOAT() }
            else if arr_elem_type == "string" { LIST_ELEM_STRING() }
            else { LIST_ELEM_OTHER() }
        let result = generate_expr(g, b, infer_val)
        g = result.gen
        b = result.block
        g.list_elem_kinds.insert(var_name, arr_elem_kind)
        g.variable_map.insert(var_name, result.val_id)
        g.variable_types.insert(var_name, "list")
        return gen_result(g, b)
    }

    let mut ir_type = "i64"
    if len(type_str) > 0 {
        ir_type = map_type_name(type_str)
        -- If the type annotation is Result[T, E] or Option[T], preserve the full type string
        if starts_with(type_str, "Result[") or starts_with(type_str, "Option[") {
            ir_type = type_str
        }
    } else if infer_val.kind != EXPR_LITERAL_NULL() {
        ir_type = infer_expr_type(g, infer_val)
    }

    -- Additional check: if value is a function call and type is still i64,
    -- try infer_call_return_type to detect Result/Option return types
    if ir_type == "i64" and infer_val.kind == EXPR_FUNCTION_CALL() {
        let call_callee = *infer_val.callee
        if call_callee.kind == EXPR_IDENTIFIER() {
            let call_ret = infer_call_return_type(g, call_callee.name)
            if starts_with(call_ret, "Result[") or starts_with(call_ret, "result_") or call_ret == "result" {
                ir_type = call_ret
            }
            if starts_with(call_ret, "Option[") or starts_with(call_ret, "option_") or call_ret == "option" {
                ir_type = call_ret
            }
        }
    }

    let alloca_id = g.builder.next_id
    let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
    alloca_inst.alloc_type = ir_type
    alloca_inst.result_type = "ptr(" + ir_type + ")"
    alloca_inst.has_result = true
    b.instructions.push(alloca_inst)
    g.builder.next_id = g.builder.next_id + 1

    let val_expr = *stmt.let_value
    if val_expr.kind != EXPR_LITERAL_NULL() {
        let result = generate_expr(g, b, val_expr)
        g = result.gen
        b = result.block

        -- If the alloca type differs from the expression type, insert a cast
        -- This handles cases like `let x: i32 = 42` where the literal is i64
        let expr_type = infer_expr_type(g, val_expr)
        let mut store_val_id = result.val_id
        if expr_type != ir_type and (is_integer_type(ir_type) or is_float_type(ir_type)) {
            let cast_id = g.builder.next_id
            let mut cast_inst = ir_inst_new(cast_id, OP_CAST())
            cast_inst.cast_val = ir_val_inst(result.val_id)
            cast_inst.cast_from = expr_type
            cast_inst.cast_to = ir_type
            cast_inst.result_type = ir_type
            cast_inst.has_result = true
            b.instructions.push(cast_inst)
            g.builder.next_id = g.builder.next_id + 1
            store_val_id = cast_id
        }

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_inst(store_val_id)
        store_inst.has_result = false
        b.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Track struct type
        if val_expr.kind == EXPR_STRUCT_LITERAL() {
            g.var_struct_types.insert(var_name, val_expr.type_name)
        }
        -- Track struct type from index access on a list of structs
        if val_expr.kind == EXPR_INDEX_ACCESS() {
            let idx_obj = *val_expr.object
            if idx_obj.kind == EXPR_IDENTIFIER() {
                let idx_var = idx_obj.name
                if g.list_elem_kinds.contains(idx_var) {
                    let ek = g.list_elem_kinds.get(idx_var)
                    if ek == LIST_ELEM_OTHER() and g.list_elem_types.contains(idx_var) {
                        let elem_t = g.list_elem_types.get(idx_var)
                        if g.struct_defs.contains(elem_t) {
                            g.var_struct_types.insert(var_name, elem_t)
                        }
                    }
                }
            }
        }
        if val_expr.kind == EXPR_IDENTIFIER() {
            if g.var_struct_types.contains(val_expr.name) {
                g.var_struct_types.insert(var_name, g.var_struct_types.get(val_expr.name))
            }
            if g.list_elem_kinds.contains(val_expr.name) {
                g.list_elem_kinds.insert(var_name, g.list_elem_kinds.get(val_expr.name))
            }
            if g.var_enum_types.contains(val_expr.name) {
                g.var_enum_types.insert(var_name, g.var_enum_types.get(val_expr.name))
            }
        }
        if val_expr.kind == EXPR_LAMBDA() {
            -- The lambda was just generated by generate_expr, which advanced lambda_counter.
            -- So the lambda name is __lambda_(counter-1).
            let actual_lambda_name = "__lambda_" + int_to_string(g.lambda_counter - 1)
            g.fn_ptr_vars.insert(var_name, actual_lambda_name)
        }
        -- If the value is a function name (identifier referencing a known function),
        -- register it in fn_ptr_vars so indirect calls work correctly
        if val_expr.kind == EXPR_IDENTIFIER() and starts_with(type_str, "fn(") {
            let fn_ref_name = val_expr.name
            -- Verify it's a known function
            let mut fn_found = false
            let mut fn_check = 0
            while fn_check < g.ir_mod.functions.len() {
                if g.ir_mod.functions[fn_check].name == fn_ref_name {
                    fn_found = true
                }
                fn_check = fn_check + 1
            }
            if fn_found {
                g.fn_ptr_vars.insert(var_name, fn_ref_name)
            }
        }
    }

    -- Track struct/enum type from type annotation
    if len(type_str) > 0 {
        if g.struct_defs.contains(type_str) {
            g.var_struct_types.insert(var_name, type_str)
        }
        if g.enum_defs.contains(type_str) {
            g.var_enum_types.insert(var_name, type_str)
        }
    }

    g.variable_map.insert(var_name, alloca_id)
    -- For function pointer type annotations, preserve the full fn(...) -> T string
    -- so that indirect call dispatch can detect it
    if starts_with(type_str, "fn(") {
        g.variable_types.insert(var_name, type_str)
    } else {
        g.variable_types.insert(var_name, ir_type)
    }
    return gen_result(g, b)
}

-- ============================================================
-- RETURN STATEMENT
-- ============================================================

fn generate_return(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    if stmt.has_ret_value {
        let result = generate_expr(g, b, *stmt.ret_value)
        g = result.gen
        b = result.block
        b.terminator = Box_new(term_ret(ir_val_inst(result.val_id)))
        b.has_terminator = true
    } else {
        b.terminator = Box_new(term_ret_void())
        b.has_terminator = true
    }

    return gen_result(g, b)
}

-- ============================================================
-- IF STATEMENT
-- ============================================================

fn generate_if_statement(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    -- Generate condition
    let cond_result = generate_expr(g, b, *stmt.if_cond)
    g = cond_result.gen
    b = cond_result.block

    let then_label = gen_next_label(g, "then")
    g = gen_advance_label(g)
    let else_label = gen_next_label(g, "else")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "if_end")
    g = gen_advance_label(g)

    -- Conditional branch
    if stmt.has_else_branch {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_result.val_id), then_label, else_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    } else {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_result.val_id), then_label, join_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    }

    -- Then block
    let mut then_block = ir_block_new(then_label)
    let then_result = generate_block_stmts(g, then_block, stmt.if_then)
    g = then_result.gen
    then_block = then_result.block
    if then_block.has_terminator == false {
        then_block.terminator = Box_new(term_br(join_label))
        then_block.has_terminator = true
    }
    g.completed_blocks.push(then_block)
    -- Else block (if present)
    let mut else_block = ir_block_new(else_label)
    if stmt.has_else_branch {
        let else_result = generate_block_stmts(g, else_block, stmt.if_else)
        g = else_result.gen
        else_block = else_result.block
        if else_block.has_terminator == false {
            else_block.terminator = Box_new(term_br(join_label))
            else_block.has_terminator = true
        }
        g.completed_blocks.push(else_block)
    }

    -- Join block continues
    let join_block = ir_block_new(join_label)

    return gen_result(g, join_block)
}

-- ============================================================
-- WHILE LOOP
-- ============================================================

fn generate_while_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    let header_label = gen_next_label(g, "while_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "while_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "while_exit")
    g = gen_advance_label(g)

    -- Save and set break/continue targets
    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    -- Branch to header
    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true
    g.completed_blocks.push(b)
    -- Header block: evaluate condition
    let mut header_block = ir_block_new(header_label)
    let cond_result = generate_expr(g, header_block, *stmt.while_cond)
    g = cond_result.gen
    header_block = cond_result.block

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cond_result.val_id), body_label, exit_label))
    header_block.has_terminator = true
    g.completed_blocks.push(header_block)
    -- Body block
    let mut body_block = ir_block_new(body_label)
    let body_result = generate_block_stmts(g, body_block, stmt.while_body)
    g = body_result.gen
    body_block = body_result.block
    if body_block.has_terminator == false {
        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }
    -- Push completed body block (b and header_block already pushed above)
    g.completed_blocks.push(body_block)
    -- Restore break/continue targets
    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- FOR LOOP
-- ============================================================

fn generate_for_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    let var_name = stmt.for_var

    -- Get the iterator expression
    let iter_expr = *stmt.for_iter

    -- Check for range-based for loop (iter is a Range expression)
    if iter_expr.kind == EXPR_RANGE() {
        return generate_range_for_loop(g, b, stmt, iter_expr)
    }

    -- Check for string literal iteration (for c in "abc")
    if iter_expr.kind == EXPR_LITERAL_STRING() {
        return generate_string_for_loop(g, b, stmt, "")
    }

    -- Check for string iteration (if iter is identifier and tracked as string type)
    if iter_expr.kind == EXPR_IDENTIFIER() {
        let iter_name = iter_expr.name

        -- Check for map iteration
        if g.map_var_kinds.contains(iter_name) {
            let mk = g.map_var_kinds.get(iter_name)
            return generate_map_for_in(g, b, stmt, iter_name, mk)
        }

        -- Check for string variable iteration
        if g.variable_types.contains(iter_name) {
            let vtype = g.variable_types.get(iter_name)
            if vtype == "string" {
                return generate_string_for_loop(g, b, stmt, iter_name)
            }
        }
    }

    -- Default: List iteration
    -- Generate the iterator expression to get the list value
    let iter_result = generate_expr(g, b, iter_expr)
    g = iter_result.gen
    b = iter_result.block
    let list_ptr = iter_result.val_id

    -- Determine list kind from iterator name or default
    let list_kind = if iter_expr.kind == EXPR_IDENTIFIER() and g.list_elem_kinds.contains(iter_expr.name) {
        g.list_elem_kinds.get(iter_expr.name)
    } else {
        LIST_ELEM_INT()
    }

    -- Get list length
    let len_fn = if list_kind == LIST_ELEM_INT() { "dm_list_int64_len" }
    else if list_kind == LIST_ELEM_FLOAT() { "dm_list_double_len" }
    else if list_kind == LIST_ELEM_STRING() { "dm_list_string_len" }
    else { "dm_list_generic_len" }

    let mut len_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    len_inst.callee = len_fn
    let mut len_args: List[IRValue] = []
    len_args.push(ir_val_inst(list_ptr))
    len_inst.call_args = len_args
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    let len_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Create index variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_zero = ir_inst_new(g.builder.next_id, OP_STORE())
    store_zero.store_ptr = ir_val_inst(idx_alloca_id)
    store_zero.store_val = ir_val_int(0)
    store_zero.has_result = false
    b.instructions.push(store_zero)
    g.builder.next_id = g.builder.next_id + 1

    -- Determine element type
    let iter_name = if iter_expr.kind == EXPR_IDENTIFIER() { iter_expr.name } else { "" }
    let elem_type = if list_kind == LIST_ELEM_INT() { "i64" }
    else if list_kind == LIST_ELEM_FLOAT() { "f64" }
    else if list_kind == LIST_ELEM_STRING() { "string" }
    else if list_kind == LIST_ELEM_OTHER() and len(iter_name) > 0 and g.list_elem_types.contains(iter_name) {
        g.list_elem_types.get(iter_name)
    }
    else { "i64" }

    -- Create element variable
    let elem_alloca_id = g.builder.next_id
    let mut elem_inst = ir_inst_new(elem_alloca_id, OP_ALLOCA())
    elem_inst.alloc_type = elem_type
    elem_inst.result_type = "ptr(" + elem_type + ")"
    elem_inst.has_result = true
    b.instructions.push(elem_inst)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, elem_alloca_id)
    g.variable_types.insert(var_name, elem_type)

    -- Labels
    let header_label = gen_next_label(g, "for_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "for_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "for_exit")
    g = gen_advance_label(g)

    -- Save break/continue targets
    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    -- Branch to header
    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header: check idx < len
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut cmp_inst = ir_inst_new(g.builder.next_id, OP_LT())
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(len_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body: get element, run body, increment
    let mut body_block = ir_block_new(body_label)

    let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx2.load_type = "i64"
    load_idx2.result_type = "i64"
    load_idx2.has_result = true
    body_block.instructions.push(load_idx2)
    let idx_val2_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Get element from list
    if list_kind == LIST_ELEM_OTHER() {
        -- Generic list: dm_list_generic_get(out_ptr, list_ptr, idx, elem_size)
        let elem_size = estimate_type_size_gen(g, elem_type)
        let mut get_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        get_inst.callee = "dm_list_generic_get"
        let mut get_args: List[IRValue] = []
        get_args.push(ir_val_inst(elem_alloca_id))
        get_args.push(ir_val_inst(list_ptr))
        get_args.push(ir_val_inst(idx_val2_id))
        get_args.push(ir_val_int(elem_size))
        get_inst.call_args = get_args
        get_inst.result_type = "void"
        get_inst.has_result = false
        body_block.instructions.push(get_inst)
        g.builder.next_id = g.builder.next_id + 1
        -- elem_alloca is already populated by dm_list_generic_get, no store needed
    } else {
        let get_fn = if list_kind == LIST_ELEM_INT() { "dm_list_int64_get" }
        else if list_kind == LIST_ELEM_FLOAT() { "dm_list_double_get" }
        else { "dm_list_string_get" }

        let mut get_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        get_inst.callee = get_fn
        let mut get_args: List[IRValue] = []
        get_args.push(ir_val_inst(list_ptr))
        get_args.push(ir_val_inst(idx_val2_id))
        get_inst.call_args = get_args
        get_inst.result_type = elem_type
        get_inst.has_result = true
        body_block.instructions.push(get_inst)
        let elem_val_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_elem = ir_inst_new(g.builder.next_id, OP_STORE())
        store_elem.store_ptr = ir_val_inst(elem_alloca_id)
        store_elem.store_val = ir_val_inst(elem_val_id)
        store_elem.has_result = false
        body_block.instructions.push(store_elem)
        g.builder.next_id = g.builder.next_id + 1
    }

    -- Generate body
    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    -- Increment index
    if body_block.has_terminator == false {
        let mut load_idx3 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx3.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx3.load_type = "i64"
        load_idx3.result_type = "i64"
        load_idx3.has_result = true
        body_block.instructions.push(load_idx3)
        let idx_val3_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val3_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_idx_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_idx_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    -- Restore break/continue
    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- RANGE FOR LOOP
-- ============================================================

fn generate_range_for_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt, range_expr: Expr) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.for_var

    -- Generate start and end values from the range expression
    let start_result = generate_expr(g, b, *range_expr.left)
    g = start_result.gen
    b = start_result.block

    let end_result = generate_expr(g, b, *range_expr.right)
    g = end_result.gen
    b = end_result.block

    -- Create loop variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_start = ir_inst_new(g.builder.next_id, OP_STORE())
    store_start.store_ptr = ir_val_inst(idx_alloca_id)
    store_start.store_val = ir_val_inst(start_result.val_id)
    store_start.has_result = false
    b.instructions.push(store_start)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, idx_alloca_id)
    g.variable_types.insert(var_name, "i64")

    -- Labels
    let header_label = gen_next_label(g, "range_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "range_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "range_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    -- Branch to header
    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header: check i < end (or i <= end for inclusive)
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Use LT for exclusive range (..), LE for inclusive range (..=)
    -- Range expr op == BINOP_LE means inclusive
    let cmp_op = if range_expr.op == BINOP_LE() { OP_LE() } else { OP_LT() }
    let mut cmp_inst = ir_inst_new(g.builder.next_id, cmp_op)
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(end_result.val_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body
    let mut body_block = ir_block_new(body_label)
    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    -- Increment
    if body_block.has_terminator == false {
        let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx2.load_type = "i64"
        load_idx2.result_type = "i64"
        load_idx2.has_result = true
        body_block.instructions.push(load_idx2)
        let idx_val2_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val2_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- STRING FOR LOOP
-- ============================================================

fn generate_string_for_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt, iter_name: string) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.for_var

    -- Generate string value from the iterator expression
    let str_result = generate_expr(g, b, *stmt.for_iter)
    g = str_result.gen
    b = str_result.block

    -- Get string length
    let mut len_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    len_inst.callee = "dm_string_len"
    let mut len_args: List[IRValue] = []
    len_args.push(ir_val_inst(str_result.val_id))
    len_inst.call_args = len_args
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    let len_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Create index variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_zero = ir_inst_new(g.builder.next_id, OP_STORE())
    store_zero.store_ptr = ir_val_inst(idx_alloca_id)
    store_zero.store_val = ir_val_int(0)
    store_zero.has_result = false
    b.instructions.push(store_zero)
    g.builder.next_id = g.builder.next_id + 1

    -- Create element variable (string)
    let elem_alloca_id = g.builder.next_id
    let mut elem_inst = ir_inst_new(elem_alloca_id, OP_ALLOCA())
    elem_inst.alloc_type = "string"
    elem_inst.result_type = "ptr(string)"
    elem_inst.has_result = true
    b.instructions.push(elem_inst)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, elem_alloca_id)
    g.variable_types.insert(var_name, "string")

    let header_label = gen_next_label(g, "strfor_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "strfor_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "strfor_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut cmp_inst = ir_inst_new(g.builder.next_id, OP_LT())
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(len_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body
    let mut body_block = ir_block_new(body_label)
    let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx2.load_type = "i64"
    load_idx2.result_type = "i64"
    load_idx2.has_result = true
    body_block.instructions.push(load_idx2)
    let idx_val2_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut char_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    char_inst.callee = "dm_char_at"
    let mut char_args: List[IRValue] = []
    char_args.push(ir_val_inst(str_result.val_id))
    char_args.push(ir_val_inst(idx_val2_id))
    char_inst.call_args = char_args
    char_inst.result_type = "string"
    char_inst.has_result = true
    body_block.instructions.push(char_inst)
    let char_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut store_elem = ir_inst_new(g.builder.next_id, OP_STORE())
    store_elem.store_ptr = ir_val_inst(elem_alloca_id)
    store_elem.store_val = ir_val_inst(char_id)
    store_elem.has_result = false
    body_block.instructions.push(store_elem)
    g.builder.next_id = g.builder.next_id + 1

    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    if body_block.has_terminator == false {
        let mut load_idx3 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx3.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx3.load_type = "i64"
        load_idx3.result_type = "i64"
        load_idx3.has_result = true
        body_block.instructions.push(load_idx3)
        let idx_val3_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val3_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- MAP FOR-IN LOOP
-- ============================================================

fn generate_map_for_in(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt, iter_name: string, mk: int) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.for_var

    let map_ptr = if g.variable_map.contains(iter_name) {
        g.variable_map.get(iter_name)
    } else {
        0
    }

    -- Build keys list
    let key_elem_kind = if mk == MAP_KIND_STRING_INT() { LIST_ELEM_STRING() } else { LIST_ELEM_INT() }
    let key_type = if key_elem_kind == LIST_ELEM_STRING() { "string" } else { "i64" }
    let list_type = if key_elem_kind == LIST_ELEM_STRING() { "dm_list_dm_string" } else { "dm_list_int64" }

    -- Alloca for keys list
    let keys_alloca_id = g.builder.next_id
    let mut keys_inst = ir_inst_new(keys_alloca_id, OP_ALLOCA())
    keys_inst.alloc_type = list_type
    keys_inst.result_type = "ptr(" + list_type + ")"
    keys_inst.has_result = true
    b.instructions.push(keys_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Call keys function
    let keys_fn = if mk == MAP_KIND_STRING_INT() { "dm_map_string_int_keys" } else { "dm_map_int_string_keys" }
    let mut keys_call = ir_inst_new(g.builder.next_id, OP_CALL())
    keys_call.callee = keys_fn
    let mut kargs: List[IRValue] = []
    kargs.push(ir_val_inst(keys_alloca_id))
    kargs.push(ir_val_inst(map_ptr))
    keys_call.call_args = kargs
    keys_call.result_type = "void"
    keys_call.has_result = false
    b.instructions.push(keys_call)
    g.builder.next_id = g.builder.next_id + 1

    -- Get keys list length
    let len_fn = if key_elem_kind == LIST_ELEM_STRING() { "dm_list_string_len" } else { "dm_list_int64_len" }
    let mut len_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    len_inst.callee = len_fn
    let mut largs: List[IRValue] = []
    largs.push(ir_val_inst(keys_alloca_id))
    len_inst.call_args = largs
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    let len_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Index variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_zero = ir_inst_new(g.builder.next_id, OP_STORE())
    store_zero.store_ptr = ir_val_inst(idx_alloca_id)
    store_zero.store_val = ir_val_int(0)
    store_zero.has_result = false
    b.instructions.push(store_zero)
    g.builder.next_id = g.builder.next_id + 1

    -- Element variable
    let elem_alloca_id = g.builder.next_id
    let mut elem_inst = ir_inst_new(elem_alloca_id, OP_ALLOCA())
    elem_inst.alloc_type = key_type
    elem_inst.result_type = "ptr(" + key_type + ")"
    elem_inst.has_result = true
    b.instructions.push(elem_inst)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, elem_alloca_id)
    g.variable_types.insert(var_name, key_type)

    let header_label = gen_next_label(g, "mapfor_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "mapfor_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "mapfor_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut cmp_inst = ir_inst_new(g.builder.next_id, OP_LT())
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(len_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body
    let mut body_block = ir_block_new(body_label)
    let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx2.load_type = "i64"
    load_idx2.result_type = "i64"
    load_idx2.has_result = true
    body_block.instructions.push(load_idx2)
    let idx_val2_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let get_fn = if key_elem_kind == LIST_ELEM_STRING() { "dm_list_string_get" } else { "dm_list_int64_get" }
    let mut get_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    get_inst.callee = get_fn
    let mut gargs: List[IRValue] = []
    gargs.push(ir_val_inst(keys_alloca_id))
    gargs.push(ir_val_inst(idx_val2_id))
    get_inst.call_args = gargs
    get_inst.result_type = key_type
    get_inst.has_result = true
    body_block.instructions.push(get_inst)
    let elem_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut store_elem = ir_inst_new(g.builder.next_id, OP_STORE())
    store_elem.store_ptr = ir_val_inst(elem_alloca_id)
    store_elem.store_val = ir_val_inst(elem_val_id)
    store_elem.has_result = false
    body_block.instructions.push(store_elem)
    g.builder.next_id = g.builder.next_id + 1

    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    if body_block.has_terminator == false {
        let mut load_idx3 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx3.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx3.load_type = "i64"
        load_idx3.result_type = "i64"
        load_idx3.has_result = true
        body_block.instructions.push(load_idx3)
        let idx_val3_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val3_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- LOOP STATEMENT (infinite loop)
-- ============================================================

fn generate_loop_stmt(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    let body_label = gen_next_label(g, "loop_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "loop_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = body_label
    g.has_break_target = true
    g.has_continue_target = true

    b.terminator = Box_new(term_br(body_label))
    b.has_terminator = true

    let mut body_block = ir_block_new(body_label)
    let body_result = generate_block_stmts(g, body_block, stmt.loop_body)
    g = body_result.gen
    body_block = body_result.block
    if body_block.has_terminator == false {
        body_block.terminator = Box_new(term_br(body_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- BREAK / CONTINUE
-- ============================================================

fn generate_break(gen: IRGenerator, block: IRBasicBlock) -> GenResult {
    let mut b = block
    if gen.has_break_target {
        b.terminator = Box_new(term_br(gen.break_target))
        b.has_terminator = true
    }
    return gen_result(gen, b)
}

fn generate_continue(gen: IRGenerator, block: IRBasicBlock) -> GenResult {
    let mut b = block
    if gen.has_continue_target {
        b.terminator = Box_new(term_br(gen.continue_target))
        b.has_terminator = true
    }
    return gen_result(gen, b)
}

-- ============================================================
-- ASSIGNMENT
-- ============================================================

fn generate_assignment(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    -- Generate the value
    let val_result = generate_expr(g, b, *stmt.assign_value)
    g = val_result.gen
    b = val_result.block

    -- Get the target expression
    let target = *stmt.assign_target
    let assign_op = stmt.assign_op

    -- Simple identifier assignment
    if target.kind == EXPR_IDENTIFIER() {
        let target_name = target.name

        if assign_op == ASSIGN_EQ() {
            -- Simple assignment
            if g.variable_map.contains(target_name) {
                let ptr = g.variable_map.get(target_name)
                let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                -- For mut params, store through the param pointer (no alloca exists)
                if g.param_ptr_map.contains(target_name) {
                    let pidx = g.param_ptr_map.get(target_name)
                    store_inst.store_ptr = ir_val_param(pidx)
                } else {
                    store_inst.store_ptr = ir_val_inst(ptr)
                }
                store_inst.store_val = ir_val_inst(val_result.val_id)
                store_inst.has_result = false
                b.instructions.push(store_inst)
                g.builder.next_id = g.builder.next_id + 1
            }
        } else {
            -- Compound assignment (+=, -=, *=, /=)
            if g.variable_map.contains(target_name) {
                let ptr = g.variable_map.get(target_name)
                let ty = if g.variable_types.contains(target_name) {
                    g.variable_types.get(target_name)
                } else {
                    "i64"
                }

                -- Determine the pointer value for load/store
                -- For mut params, use the param pointer directly (no alloca exists)
                let mut ptr_val = ir_val_inst(ptr)
                if g.param_ptr_map.contains(target_name) {
                    let pidx = g.param_ptr_map.get(target_name)
                    ptr_val = ir_val_param(pidx)
                }

                -- Load current value
                let mut load_inst = ir_inst_new(g.builder.next_id, OP_LOAD())
                load_inst.load_ptr = ptr_val
                load_inst.load_type = ty
                load_inst.result_type = ty
                load_inst.has_result = true
                b.instructions.push(load_inst)
                let old_val_id = g.builder.next_id
                g.builder.next_id = g.builder.next_id + 1

                -- Compute new value
                let op = if assign_op == ASSIGN_ADD() { OP_ADD() }
                else if assign_op == ASSIGN_SUB() { OP_SUB() }
                else if assign_op == ASSIGN_MUL() { OP_MUL() }
                else if assign_op == ASSIGN_DIV() { OP_DIV() }
                else { OP_MOD() }

                let mut arith_inst = ir_inst_new(g.builder.next_id, op)
                arith_inst.lhs = ir_val_inst(old_val_id)
                arith_inst.rhs = ir_val_inst(val_result.val_id)
                arith_inst.result_type = ty
                arith_inst.has_result = true
                b.instructions.push(arith_inst)
                let new_val_id = g.builder.next_id
                g.builder.next_id = g.builder.next_id + 1

                -- Store new value
                let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                store_inst.store_ptr = ptr_val
                store_inst.store_val = ir_val_inst(new_val_id)
                store_inst.has_result = false
                b.instructions.push(store_inst)
                g.builder.next_id = g.builder.next_id + 1
            }
        }
    } else if target.kind == EXPR_FIELD_ACCESS() {
        -- Field assignment: obj.field = value
        -- Generate the target field access as an lvalue (get struct ptr + field index)
        -- For now, generate full struct load, insert_field, store back
        let obj = *target.object
        if obj.kind == EXPR_IDENTIFIER() and g.variable_map.contains(obj.name) {
            let ptr = g.variable_map.get(obj.name)
            let struct_name = if g.var_struct_types.contains(obj.name) {
                g.var_struct_types.get(obj.name)
            } else {
                ""
            }
            if len(struct_name) > 0 and g.struct_defs.contains(struct_name) {
                let info = g.struct_defs.get(struct_name)
                let field_name = target.field
                let mut field_idx = -1
                let mut fi = 0
                while fi < info.field_names.len() {
                    if info.field_names[fi] == field_name {
                        field_idx = fi
                    }
                    fi = fi + 1
                }
                if field_idx >= 0 {
                    -- Determine the struct pointer source:
                    -- For mut pointer params (mut self): use param ref directly
                    -- For regular vars: use the alloca instruction ref
                    let mut load_ptr_val = ir_val_inst(ptr)
                    if g.param_ptr_map.contains(obj.name) {
                        let pidx = g.param_ptr_map.get(obj.name)
                        load_ptr_val = ir_val_param(pidx)
                    }

                    -- Load current struct value
                    let mut load_inst = ir_inst_new(g.builder.next_id, OP_LOAD())
                    load_inst.load_ptr = load_ptr_val
                    load_inst.load_type = struct_name
                    load_inst.result_type = struct_name
                    load_inst.has_result = true
                    b.instructions.push(load_inst)
                    let struct_val_id = g.builder.next_id
                    g.builder.next_id = g.builder.next_id + 1

                    -- Insert new field value
                    let mut insert_inst = ir_inst_new(g.builder.next_id, OP_INSERT_FIELD())
                    insert_inst.field_base = ir_val_inst(struct_val_id)
                    insert_inst.field_value = ir_val_inst(val_result.val_id)
                    insert_inst.field_index = field_idx
                    insert_inst.result_type = struct_name
                    insert_inst.has_result = true
                    b.instructions.push(insert_inst)
                    let new_struct_id = g.builder.next_id
                    g.builder.next_id = g.builder.next_id + 1

                    -- Store back through the pointer
                    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                    store_inst.store_ptr = load_ptr_val
                    store_inst.store_val = ir_val_inst(new_struct_id)
                    store_inst.has_result = false
                    b.instructions.push(store_inst)
                    g.builder.next_id = g.builder.next_id + 1
                }
            }
        }
    } else if target.kind == EXPR_INDEX_ACCESS() {
        -- Index assignment: arr[i] = value or map[key] = value
        -- Delegate to list_push or map_insert depending on type
        let obj = *target.object
        if obj.kind == EXPR_IDENTIFIER() {
            let obj_name = obj.name
            -- Map index assignment
            if g.map_var_kinds.contains(obj_name) {
                let mk = g.map_var_kinds.get(obj_name)
                let map_ptr = g.variable_map.get(obj_name)
                let insert_fn = if mk == MAP_KIND_STRING_INT() { "dm_map_string_int_insert" } else { "dm_map_int_string_insert" }

                -- Generate the key expression
                let key_result = generate_expr(g, b, *target.index)
                g = key_result.gen
                b = key_result.block

                let mut insert_inst = ir_inst_new(g.builder.next_id, OP_CALL())
                insert_inst.callee = insert_fn
                let mut iargs: List[IRValue] = []
                iargs.push(ir_val_inst(map_ptr))
                iargs.push(ir_val_inst(key_result.val_id))
                iargs.push(ir_val_inst(val_result.val_id))
                insert_inst.call_args = iargs
                insert_inst.result_type = "void"
                insert_inst.has_result = false
                b.instructions.push(insert_inst)
                g.builder.next_id = g.builder.next_id + 1
            }

            -- List/array index assignment: bounds check, then typed set
            if g.list_elem_kinds.contains(obj_name) {
                let ek = g.list_elem_kinds.get(obj_name)
                let list_ptr = g.variable_map.get(obj_name)

                -- Generate the index expression
                let idx_result = generate_expr(g, b, *target.index)
                g = idx_result.gen
                b = idx_result.block

                -- Length for the bounds check
                let set_len_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_len" }
                else if ek == LIST_ELEM_FLOAT() { "dm_list_double_len" }
                else if ek == LIST_ELEM_STRING() { "dm_list_string_len" }
                else { "dm_list_generic_len" }
                let set_len_id = g.builder.next_id
                let mut set_len_inst = ir_inst_new(set_len_id, OP_CALL())
                set_len_inst.callee = set_len_fn
                let mut set_len_args: List[IRValue] = []
                set_len_args.push(ir_val_inst(list_ptr))
                set_len_inst.call_args = set_len_args
                set_len_inst.result_type = "i64"
                set_len_inst.has_result = true
                b.instructions.push(set_len_inst)
                g.builder.next_id = set_len_id + 1

                let set_ctx = if g.array_var_names.contains(obj_name) { "(array)" } else { "(list)" }
                let set_chk = check_index_bounds(g, b, idx_result.val_id, ir_val_inst(set_len_id), set_ctx)
                g = set_chk.gen
                b = set_chk.block

                if ek == LIST_ELEM_OTHER() {
                    -- Generic set: alloca temp, store value, pass pointer + elem_size
                    let set_elem_type = if g.list_elem_types.contains(obj_name) {
                        g.list_elem_types.get(obj_name)
                    } else {
                        "i64"
                    }
                    let set_elem_size = estimate_type_size_gen(g, set_elem_type)
                    let set_temp_id = g.builder.next_id
                    let mut set_temp_inst = ir_inst_new(set_temp_id, OP_ALLOCA())
                    set_temp_inst.alloc_type = set_elem_type
                    set_temp_inst.result_type = "ptr(" + set_elem_type + ")"
                    set_temp_inst.has_result = true
                    b.instructions.push(set_temp_inst)
                    g.builder.next_id = g.builder.next_id + 1

                    let mut set_store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                    set_store_inst.store_ptr = ir_val_inst(set_temp_id)
                    set_store_inst.store_val = ir_val_inst(val_result.val_id)
                    set_store_inst.has_result = false
                    b.instructions.push(set_store_inst)
                    g.builder.next_id = g.builder.next_id + 1

                    let mut set_inst = ir_inst_new(g.builder.next_id, OP_CALL())
                    set_inst.callee = "dm_list_generic_set"
                    let mut set_args: List[IRValue] = []
                    set_args.push(ir_val_inst(list_ptr))
                    set_args.push(ir_val_inst(set_temp_id))
                    set_args.push(ir_val_inst(idx_result.val_id))
                    set_args.push(ir_val_int(set_elem_size))
                    set_inst.call_args = set_args
                    set_inst.result_type = "void"
                    set_inst.has_result = false
                    b.instructions.push(set_inst)
                    g.builder.next_id = g.builder.next_id + 1
                } else {
                    let set_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_set" }
                    else if ek == LIST_ELEM_FLOAT() { "dm_list_double_set" }
                    else { "dm_list_string_set" }
                    let mut set_inst = ir_inst_new(g.builder.next_id, OP_CALL())
                    set_inst.callee = set_fn
                    let mut set_args: List[IRValue] = []
                    set_args.push(ir_val_inst(list_ptr))
                    set_args.push(ir_val_inst(idx_result.val_id))
                    set_args.push(ir_val_inst(val_result.val_id))
                    set_inst.call_args = set_args
                    set_inst.result_type = "void"
                    set_inst.has_result = false
                    b.instructions.push(set_inst)
                    g.builder.next_id = g.builder.next_id + 1
                }
            }
        }
    }

    return gen_result(g, b)
}


-- ============================================================
-- EXPRESSION IR GENERATION
-- Main dispatcher and all expression kind handlers
-- Port of stage3/src/ir_gen.zig generateExpr
-- ============================================================
