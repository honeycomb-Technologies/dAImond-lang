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
    let type_str = stmt.let_type

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
        let new_fn = list_type + "_new"
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

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = "ptr"
        alloca_inst.result_type = "ptr(ptr)"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        let val_expr = *stmt.let_value
        if val_expr.kind != EXPR_LITERAL_NULL() {
            let result = generate_expr(g, b, val_expr)
            g = result.gen
            b = result.block

            -- Infer concrete type from value
            if val_expr.kind == EXPR_IDENTIFIER() {
                if g.var_struct_types.contains(val_expr.name) {
                    let concrete = g.var_struct_types.get(val_expr.name)
                    g.dyn_var_concrete.insert(var_name, concrete)
                    g.dyn_var_traits.insert(var_name, trait_name)
                }
            }

            -- Heap allocate and store
            let size = estimate_type_size("ptr")
            let mut malloc_inst = ir_inst_new(g.builder.next_id, OP_CALL())
            malloc_inst.callee = "malloc"
            let mut margs: List[IRValue] = []
            margs.push(ir_val_int(size))
            malloc_inst.call_args = margs
            malloc_inst.result_type = "ptr"
            malloc_inst.has_result = true
            b.instructions.push(malloc_inst)
            let data_ptr_id = g.builder.next_id
            g.builder.next_id = g.builder.next_id + 1

            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(data_ptr_id)
            store_inst.store_val = ir_val_inst(result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1

            let mut store2 = ir_inst_new(g.builder.next_id, OP_STORE())
            store2.store_ptr = ir_val_inst(alloca_id)
            store2.store_val = ir_val_inst(data_ptr_id)
            store2.has_result = false
            b.instructions.push(store2)
            g.builder.next_id = g.builder.next_id + 1
        }

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, "ptr")
        return gen_result(g, b)
    }

    -- Regular let binding
    let ir_type = if len(type_str) > 0 {
        map_type_name(type_str)
    } else {
        "i64"
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

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_inst(result.val_id)
        store_inst.has_result = false
        b.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Track struct type
        if val_expr.kind == EXPR_STRUCT_LITERAL() {
            g.var_struct_types.insert(var_name, val_expr.type_name)
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
            g.fn_ptr_vars.insert(var_name, val_expr.str_val)
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
    g.variable_types.insert(var_name, ir_type)
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
    let elem_type = if list_kind == LIST_ELEM_INT() { "i64" }
    else if list_kind == LIST_ELEM_FLOAT() { "f64" }
    else if list_kind == LIST_ELEM_STRING() { "string" }
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
    let get_fn = if list_kind == LIST_ELEM_INT() { "dm_list_int64_get" }
    else if list_kind == LIST_ELEM_FLOAT() { "dm_list_double_get" }
    else if list_kind == LIST_ELEM_STRING() { "dm_list_string_get" }
    else { "dm_list_generic_get" }

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
    g.completed_blocks.push(b)

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
    g.completed_blocks.push(header_block)

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
    g.completed_blocks.push(body_block)

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

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
    g.completed_blocks.push(body_block)

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

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
    g.completed_blocks.push(body_block)

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

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
    g.completed_blocks.push(body_block)
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

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
                store_inst.store_ptr = ir_val_inst(ptr)
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

                -- Load current value
                let mut load_inst = ir_inst_new(g.builder.next_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(ptr)
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
                store_inst.store_ptr = ir_val_inst(ptr)
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
                    -- Load current struct value
                    let mut load_inst = ir_inst_new(g.builder.next_id, OP_LOAD())
                    load_inst.load_ptr = ir_val_inst(ptr)
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

                    -- Store back
                    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                    store_inst.store_ptr = ir_val_inst(ptr)
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
        }
    }

    return gen_result(g, b)
}
