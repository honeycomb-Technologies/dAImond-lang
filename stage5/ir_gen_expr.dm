module ir_gen_expr

import ast
import ir
import ir_builder
import ir_gen
import ir_gen_stmt

-- ============================================================
-- EXPRESSION IR GENERATION
-- Main dispatcher and all expression kind handlers
-- ============================================================

-- Parse the error type from a "Result[T, E]" type annotation string
-- Returns the mapped IR type of E (e.g., "string" or "i64")
fn parse_result_err_type(type_ann: string) -> string {
    -- Find first "[" and last "]"
    let open_bracket = string_find(type_ann, "[")
    if open_bracket < 0 { return "i64" }
    let close_bracket = len(type_ann) - 1
    if char_at(type_ann, close_bracket) != "]" { return "i64" }
    -- Extract inner: "int, string"
    let inner = substr(type_ann, open_bracket + 1, close_bracket - open_bracket - 1)
    -- Find the comma that separates T and E (handle nested brackets)
    let mut depth = 0
    let mut ci = 0
    while ci < len(inner) {
        let ch = char_at(inner, ci)
        if ch == "[" { depth = depth + 1 }
        if ch == "]" { depth = depth - 1 }
        if ch == "," and depth == 0 {
            let err_type = string_trim(substr(inner, ci + 1, len(inner) - ci - 1))
            return map_type_name(err_type)
        }
        ci = ci + 1
    }
    return "i64"
}

-- Parse the ok type from a "Result[T, E]" type annotation string
fn parse_result_ok_type(type_ann: string) -> string {
    let open_bracket = string_find(type_ann, "[")
    if open_bracket < 0 { return "i64" }
    let close_bracket = len(type_ann) - 1
    if char_at(type_ann, close_bracket) != "]" { return "i64" }
    let inner = substr(type_ann, open_bracket + 1, close_bracket - open_bracket - 1)
    -- Find the comma that separates T and E
    let mut depth = 0
    let mut ci = 0
    while ci < len(inner) {
        let ch = char_at(inner, ci)
        if ch == "[" { depth = depth + 1 }
        if ch == "]" { depth = depth - 1 }
        if ch == "," and depth == 0 {
            let ok_type = string_trim(substr(inner, 0, ci))
            return map_type_name(ok_type)
        }
        ci = ci + 1
    }
    return "i64"
}

-- Parse the value type from an "Option[T]" type annotation string
fn parse_option_value_type(type_ann: string) -> string {
    let open_bracket = string_find(type_ann, "[")
    if open_bracket < 0 { return "i64" }
    let close_bracket = len(type_ann) - 1
    if char_at(type_ann, close_bracket) != "]" { return "i64" }
    let inner = substr(type_ann, open_bracket + 1, close_bracket - open_bracket - 1)
    return map_type_name(string_trim(inner))
}

-- Look up the Result type annotation for a scrutinee expression
fn get_scrutinee_type_annotation(gen: IRGenerator, scrut_expr: Expr) -> string {
    if scrut_expr.kind == EXPR_IDENTIFIER() {
        let name = scrut_expr.name
        if gen.var_type_annotations.contains(name) {
            return gen.var_type_annotations.get(name)
        }
    }
    return ""
}

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
        let static_target = static_call_target(gen, expr)
        if static_target != "" {
            return generate_function_call(gen, block, static_call_expr(expr, static_target))
        }
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

    -- A name bound to a lambda that captures variables is only called
    -- (see generate_lambda_expr); here it is used as a value
    if g.fn_ptr_vars.contains(name) {
        let bound_lambda = g.fn_ptr_vars.get(name)
        if g.lambda_captures.contains(bound_lambda) {
            let captured = string_split(g.lambda_captures.get(bound_lambda), "|")
            eprintln(error_at(g.ir_mod.files, captured_lambda_error(captured[0], "the lambda '" + name + "'"), expr.line))
            exit(1)
        }
    }

    -- Check for "None" (Option None)
    if name == "None" {
        -- Build Option { has_value: 0, value: undef } as an aggregate value.
        -- Its type is that of the place it goes to, which the checker
        -- found; without it, the function's return type
        let mut opt_type = checked_ir_type(g, expr)
        if starts_with(opt_type, "Option[") == false {
            opt_type = g.current_return_type
        }
        if starts_with(opt_type, "Option[") == false {
            opt_type = "Option[i64]"
        }
        let insert_id = g.builder.next_id
        let mut insert_tag = ir_inst_new(insert_id, OP_INSERT_FIELD())
        insert_tag.field_base = ir_val_undef()
        insert_tag.field_value = ir_val_int(0)
        insert_tag.field_index = 0
        insert_tag.result_type = opt_type
        insert_tag.has_result = true
        b.instructions.push(insert_tag)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, insert_id)
    }

    -- Look up variable in scope
    if g.variable_map.contains(name) {
        let alloca_id = g.variable_map.get(name)
        let var_type = if g.variable_types.contains(name) {
            g.variable_types.get(name)
        } else {
            "i64"
        }

        -- For mut pointer vars (mut self): load struct through the param pointer.
        -- The param IS a pointer to the struct (no alloca was created).
        if g.param_ptr_map.contains(name) {
            let param_idx = g.param_ptr_map.get(name)
            let load_id = g.builder.next_id
            let mut load_struct = ir_inst_new(load_id, OP_LOAD())
            load_struct.load_ptr = ir_val_param(param_idx)
            load_struct.load_type = var_type
            load_struct.result_type = var_type
            load_struct.has_result = true
            b.instructions.push(load_struct)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, load_id)
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

    -- Check for global constants (const SIZE = 4096, etc.)
    let mut gi = 0
    while gi < g.ir_mod.globals.len() {
        let glob = g.ir_mod.globals[gi]
        if glob.name == name and glob.has_init {
            let init = glob.init_value
            if init.kind == VAL_CONST_INT() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "i64"
                ai.result_type = "ptr(i64)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_int(init.int_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "i64"
                li.result_type = "i64"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
            if init.kind == VAL_CONST_FLOAT() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "f64"
                ai.result_type = "ptr(f64)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_float(init.float_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "f64"
                li.result_type = "f64"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
            if init.kind == VAL_CONST_BOOL() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "bool"
                ai.result_type = "ptr(bool)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_bool(init.bool_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "bool"
                li.result_type = "bool"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
            if init.kind == VAL_CONST_STRING() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "string"
                ai.result_type = "ptr(string)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_string(init.str_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "string"
                li.result_type = "string"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
        }
        gi = gi + 1
    }

    -- Check if the identifier is a known function name (function pointer reference).
    -- A function declared further down is not in the module yet, but
    -- its signature is registered.
    let mut is_function = g.fn_src_param_types.contains(name) and g.generic_fn_decls.contains(name) == false
    let mut fi = 0
    while fi < g.ir_mod.functions.len() and is_function == false {
        if g.ir_mod.functions[fi].name == name {
            is_function = true
        }
        fi = fi + 1
    }
    if is_function {
        -- Return a global reference to the function
        let ref_id = g.builder.next_id
        let mut ref_inst = ir_inst_new(ref_id, OP_ALLOCA())
        ref_inst.alloc_type = "ptr"
        ref_inst.result_type = "ptr(ptr)"
        ref_inst.has_result = true
        b.instructions.push(ref_inst)
        g.builder.next_id = g.builder.next_id + 1

        let mut store_ref = ir_inst_new(g.builder.next_id, OP_STORE())
        store_ref.store_ptr = ir_val_inst(ref_id)
        store_ref.store_val = ir_val_global(name)
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

    -- Unknown identifier: report it, then emit a placeholder to continue
    g = ir_type_error(g, "undefined variable '" + expr.name + "'", expr.line)
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
-- STATIC SOURCE TYPES
-- The dAImond source type of an expression, as written in the
-- program (e.g. "Parser", "List[Stmt]", "Map_string_i64",
-- "Box[Expr]"), or "" when it cannot be determined. Used to
-- dispatch field access and method calls on any receiver, not
-- just on variables.
-- ============================================================

fn strip_box_type(t: string) -> string {
    if starts_with(t, "Box[") { return extract_inner_type(t, "Box[") }
    return t
}

fn struct_field_src_type(gen: IRGenerator, struct_name: string, field: string) -> string {
    if gen.struct_defs.contains(struct_name) == false { return "" }
    let info = gen.struct_defs.get(struct_name)
    let mut i = 0
    while i < info.field_names.len() {
        if info.field_names[i] == field and i < info.field_src_types.len() {
            return info.field_src_types[i]
        }
        i = i + 1
    }
    return ""
}

-- Source type for a builtin's IR return type name
fn builtin_src_type(ir_type: string) -> string {
    -- string_split is the only builtin that returns a list
    if ir_type == "list" { return "List[string]" }
    if ir_type == "i64" { return "int" }
    if ir_type == "f64" { return "float" }
    if ir_type == "void" { return "" }
    return ir_type
}

-- ============================================================
-- MAPS
-- Map[K, V] is the generic struct in runtime/map.dm; a map type
-- is one of its instances, such as Map_string_i64.
-- ============================================================

fn is_map_type(gen: IRGenerator, t: string) -> bool {
    if gen.generic_instances.contains(t) == false { return false }
    return gen.generic_instances.get(t) == "Map"
}

-- Source type argument `which` (0 = K, 1 = V) of a map instance
fn map_instance_arg(gen: IRGenerator, t: string, which: int) -> string {
    let args = string_split(gen.generic_instance_args.get(t), "|")
    if which < args.len() { return args[which] }
    return ""
}

-- Which of the basic kinds a source type is: "string", "bool" or
-- "number" (an integer or a float type). "" for every other type.
fn basic_type_kind(t: string) -> string {
    if t == "string" or t == "bool" { return t }
    if t == "int" or t == "float" or t == "f32" or t == "f64" { return "number" }
    if t == "i8" or t == "i16" or t == "i32" or t == "i64" { return "number" }
    if t == "u8" or t == "u16" or t == "u32" or t == "u64" { return "number" }
    return ""
}

-- Report a string, a bool or a number given where another of the
-- three is expected. `what` names the place, such as "argument 1 of
-- 'f'". Values of other types, and values whose type is not known
-- here, are left to the checks of code generation.
fn check_basic_type(gen: IRGenerator, expected: string, value: Expr, what: string) -> IRGenerator {
    let want = basic_type_kind(expected)
    if want == "" { return gen }
    let found = static_type(gen, value)
    let got = basic_type_kind(found)
    if got == "" or got == want { return gen }
    return ir_type_error(gen, "type mismatch in " + what + ": expected " + expected + ", found " + found, value.line)
}

-- Record the source type of a variable that a pattern binds, as a
-- parameter's is recorded, so that its fields, elements and methods
-- resolve. What an earlier variable of the name left is replaced.
fn track_bound_variable(gen: IRGenerator, name: string, src_type: string) -> IRGenerator {
    let mut g = gen
    let t = canon_type(g, src_type)
    g.variable_types.insert(name, map_type_name(t))
    g.var_src_types.insert(name, t)
    if g.struct_defs.contains(t) {
        g.var_struct_types.insert(name, t)
    } else if g.var_struct_types.contains(name) {
        g.var_struct_types.remove(name)
    }
    if g.enum_defs.contains(t) {
        g.var_enum_types.insert(name, t)
    } else if g.var_enum_types.contains(name) {
        g.var_enum_types.remove(name)
    }
    if starts_with(t, "List[") {
        let inner = extract_inner_type(t, "List[")
        let elem_kind = classify_list_elem(inner)
        g.list_elem_kinds.insert(name, elem_kind)
        if elem_kind == LIST_ELEM_OTHER() {
            g.list_elem_types.insert(name, inner)
        }
    } else if g.list_elem_kinds.contains(name) {
        g.list_elem_kinds.remove(name)
    }
    if starts_with(t, "Box[") {
        g.box_inner_types.insert(name, extract_inner_type(t, "Box["))
    } else if g.box_inner_types.contains(name) {
        g.box_inner_types.remove(name)
    }
    return g
}

-- The function that a call written Type.name(args) calls, or "" when
-- the call is a method call on a value. Box.new and Box.null are the
-- builtins Box_new and Box_null; a function of an impl block that
-- takes no self is called on its struct or enum. A variable in scope
-- with the type's name is a value, so a call on it is a method call.
fn static_call_target(gen: IRGenerator, expr: Expr) -> string {
    let obj = *expr.object
    if obj.kind != EXPR_IDENTIFIER() { return "" }
    if gen.variable_map.contains(obj.name) { return "" }
    if obj.name == "Box" and (expr.method == "new" or expr.method == "null") {
        return "Box_" + expr.method
    }
    let target = obj.name + "_" + expr.method
    if gen.static_fns.contains(target) { return target }
    return ""
}

-- The call Type.name(args) as the call of the function `target`
fn static_call_expr(expr: Expr, target: string) -> Expr {
    return expr_call(expr_ident(target, expr.line, expr.col), expr.args, expr.line, expr.col)
}

-- Map_new() or Map.new()
fn is_map_ctor(e: Expr) -> bool {
    if e.kind == EXPR_FUNCTION_CALL() and e.args.len() == 0 {
        let c = *e.callee
        return c.kind == EXPR_IDENTIFIER() and c.name == "Map_new"
    }
    if e.kind == EXPR_METHOD_CALL() and e.method == "new" and e.args.len() == 0 {
        let o = *e.object
        return o.kind == EXPR_IDENTIFIER() and o.name == "Map"
    }
    return false
}

-- Map_new() becomes an empty map of the type its destination
-- needs, written as a struct literal; anything else is unchanged
fn map_ctor_as(gen: IRGenerator, e: Expr, dest: string) -> Expr {
    if is_map_ctor(e) == false or is_map_type(gen, dest) == false { return e }
    let info = gen.struct_defs.get(dest)
    let mut lit = expr_new(EXPR_STRUCT_LITERAL(), e.line, e.col)
    lit.type_name = dest
    let mut names: List[string] = []
    let mut vals: List[Expr] = []
    let mut i = 0
    while i < info.field_names.len() {
        names.push(info.field_names[i])
        if starts_with(info.field_src_types[i], "List[") {
            vals.push(expr_new(EXPR_ARRAY_LITERAL(), e.line, e.col))
        } else {
            vals.push(expr_int(0, e.line, e.col))
        }
        i = i + 1
    }
    lit.field_names = names
    lit.field_values = vals
    return lit
}

-- byte_at(s, i): the byte at index i of s as an int, 0 to 255
fn generate_byte_at(gen: IRGenerator, block: IRBasicBlock, s_expr: Expr, i_expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let s_res = generate_expr(g, b, s_expr)
    g = s_res.gen
    b = s_res.block
    let i_res = generate_expr(g, b, i_expr)
    g = i_res.gen
    b = i_res.block
    -- A string is { data, len, capacity }, like a list of u8
    let slot = g.builder.next_id
    let mut slot_inst = ir_inst_new(slot, OP_ALLOCA())
    slot_inst.alloc_type = "string"
    slot_inst.result_type = "ptr(string)"
    slot_inst.has_result = true
    b.instructions.push(slot_inst)
    g.builder.next_id = slot + 1
    let mut st = ir_inst_new(g.builder.next_id, OP_STORE())
    st.store_ptr = ir_val_inst(slot)
    st.store_val = ir_val_inst(s_res.val_id)
    b.instructions.push(st)
    g.builder.next_id = g.builder.next_id + 1
    let len_id = g.builder.next_id
    let mut len_inst = ir_inst_new(len_id, OP_CALL())
    len_inst.callee = "dm_string_len"
    let mut len_args: List[IRValue] = []
    len_args.push(ir_val_inst(s_res.val_id))
    len_inst.call_args = len_args
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    g.builder.next_id = len_id + 1
    let chk = check_index_bounds(g, b, i_res.val_id, ir_val_inst(len_id), "(string)")
    g = chk.gen
    b = chk.block
    let addr = g.builder.next_id
    let mut gep = ir_inst_new(addr, OP_GEP())
    gep.gep_base = ir_val_inst(slot)
    let mut idx: List[IRValue] = []
    idx.push(ir_val_inst(i_res.val_id))
    gep.gep_indices = idx
    gep.alloc_type = "u8"
    gep.field_type = "u8"
    gep.result_type = "ptr(u8)"
    gep.has_result = true
    b.instructions.push(gep)
    g.builder.next_id = addr + 1
    let ld = g.builder.next_id
    let mut ld_inst = ir_inst_new(ld, OP_LOAD())
    ld_inst.load_ptr = ir_val_inst(addr)
    ld_inst.load_type = "u8"
    ld_inst.result_type = "u8"
    ld_inst.has_result = true
    b.instructions.push(ld_inst)
    g.builder.next_id = ld + 1
    let cast = g.builder.next_id
    let mut cast_inst = ir_inst_new(cast, OP_CAST())
    cast_inst.cast_val = ir_val_inst(ld)
    cast_inst.cast_from = "u8"
    cast_inst.cast_to = "i64"
    cast_inst.result_type = "i64"
    cast_inst.has_result = true
    b.instructions.push(cast_inst)
    g.builder.next_id = cast + 1
    return expr_result(g, b, cast)
}

-- Emit a cast of `val` from type `from` to type `to`
fn emit_ir_cast(gen: IRGenerator, block: IRBasicBlock, val: IRValue, from: string, to: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = val
    inst.cast_from = from
    inst.cast_to = to
    inst.result_type = to
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- Emit a store of `val` through `ptr`
fn emit_ir_store(gen: IRGenerator, block: IRBasicBlock, ptr: IRValue, val: IRValue) -> GenResult {
    let mut g = gen
    let mut b = block
    let mut st = ir_inst_new(g.builder.next_id, OP_STORE())
    st.store_ptr = ptr
    st.store_val = val
    st.has_result = false
    b.instructions.push(st)
    g.builder.next_id = g.builder.next_id + 1
    return gen_result(g, b)
}

-- Emit the address of field `index` (of IR type `field_t`) of the
-- struct of IR type `struct_t` at `base`
fn emit_ir_field_addr(gen: IRGenerator, block: IRBasicBlock, base: IRValue, struct_t: string, index: int, field_t: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut gep = ir_inst_new(id, OP_GEP())
    gep.gep_base = base
    gep.alloc_type = struct_t
    gep.field_index = index
    gep.field_type = field_t
    gep.result_type = "ptr(" + field_t + ")"
    gep.has_result = true
    b.instructions.push(gep)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- Generate an integer argument of a raw memory primitive
fn generate_raw_int_arg(gen: IRGenerator, block: IRBasicBlock, fn_name: string, what: string, arg: Expr) -> ExprResult {
    let mut g = gen
    let arg_t = infer_expr_type(g, arg)
    if is_integer_type(arg_t) == false {
        g = ir_type_error(g, fn_name + "() takes an integer " + what + ", not '" + builtin_src_type(arg_t) + "'", arg.line)
    }
    let r = generate_expr(g, block, arg)
    if arg_t == "i64" or is_integer_type(arg_t) == false {
        return r
    }
    return emit_ir_cast(r.gen, r.block, ir_val_inst(r.val_id), arg_t, "i64")
}

-- An address argument as a pointer
fn generate_raw_ptr_arg(gen: IRGenerator, block: IRBasicBlock, fn_name: string, arg: Expr) -> ExprResult {
    let r = generate_raw_int_arg(gen, block, fn_name, "address", arg)
    return emit_ir_cast(r.gen, r.block, ir_val_inst(r.val_id), "i64", "ptr")
}

-- Whether a source type names a type whose size is known
fn sized_type_known(gen: IRGenerator, t: string) -> bool {
    let ir_t = map_type_name(t)
    if is_integer_type(ir_t) or is_float_type(ir_t) or t == "string" or t == "bool" { return true }
    if starts_with(t, "List[") or starts_with(t, "Box[") { return true }
    return gen.struct_defs.contains(t) or gen.enum_defs.contains(t)
}

fn generate_raw_builtin(gen: IRGenerator, block: IRBasicBlock, fn_name: string, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let args = expr.args
    if g.unsafe_depth == 0 {
        g = ir_type_error(g, fn_name + "() works on raw memory and requires an unsafe block", expr.line)
        return expr_result(g, b, 0)
    }
    let want = if fn_name == "string_from_raw" { 3 } else if starts_with(fn_name, "store_") or fn_name == "static_addr" { 2 } else if fn_name == "size_of" { 0 } else { 1 }
    if args.len() != want {
        g = ir_type_error(g, fn_name + "() takes " + arguments_text(want) + ", not " + int_to_string(args.len()), expr.line)
        return expr_result(g, b, 0)
    }
    let mem_t = raw_mem_type(fn_name)

    -- size_of[T]()
    if fn_name == "size_of" {
        if expr.generic_args.len() != 1 {
            g = ir_type_error(g, "size_of takes one type: size_of[T]()", expr.line)
            return expr_result(g, b, 0)
        }
        let mut t = expr.generic_args[0]
        g = ensure_type_instances(g, t, expr.line)
        t = canon_type(g, t)
        if sized_type_known(g, t) == false {
            g = ir_type_error(g, "size_of: unknown type '" + t + "'", expr.line)
            return expr_result(g, b, 0)
        }
        return emit_ir_cast(g, b, ir_val_sizeof(map_type_name(t)), "i64", "i64")
    }

    -- symbol_addr("name"): lowered where the call is, see generate_call
    if fn_name == "symbol_addr" {
        if args[0].kind != EXPR_LITERAL_STRING() or args[0].str_val == "" {
            g = ir_type_error(g, "symbol_addr() takes the name of a symbol as a string literal", expr.line)
            return expr_result(g, b, 0)
        }
        let sym = g.builder.next_id
        let mut sym_inst = ir_inst_new(sym, OP_CALL())
        sym_inst.callee = "dm_symbol_addr"
        let mut sym_args: List[IRValue] = []
        sym_args.push(ir_val_string(args[0].str_val))
        sym_inst.call_args = sym_args
        sym_inst.result_type = "i64"
        sym_inst.has_result = true
        b.instructions.push(sym_inst)
        g.builder.next_id = sym + 1
        return expr_result(g, b, sym)
    }

    -- static_addr("name", size): lowered where the call is, see generate_call
    if fn_name == "static_addr" {
        if args[0].kind != EXPR_LITERAL_STRING() or args[0].str_val == "" {
            g = ir_type_error(g, "static_addr() takes the name of the storage as a string literal", expr.line)
            return expr_result(g, b, 0)
        }
        if args[1].kind != EXPR_LITERAL_INT() or args[1].int_val <= 0 {
            g = ir_type_error(g, "static_addr() takes the size in bytes as an integer literal above 0", expr.line)
            return expr_result(g, b, 0)
        }
        let st = g.builder.next_id
        let mut st_inst = ir_inst_new(st, OP_CALL())
        st_inst.callee = "dm_static_addr"
        let mut st_args: List[IRValue] = []
        st_args.push(ir_val_string(args[0].str_val))
        st_args.push(ir_val_int(args[1].int_val))
        st_inst.call_args = st_args
        st_inst.result_type = "i64"
        st_inst.has_result = true
        b.instructions.push(st_inst)
        g.builder.next_id = st + 1
        return expr_result(g, b, st)
    }

    -- load_T(addr)
    if starts_with(fn_name, "load_") {
        let p = generate_raw_ptr_arg(g, b, fn_name, args[0])
        g = p.gen
        b = p.block
        let ld = g.builder.next_id
        let mut ld_inst = ir_inst_new(ld, OP_LOAD())
        ld_inst.load_ptr = ir_val_inst(p.val_id)
        ld_inst.load_type = mem_t
        ld_inst.result_type = mem_t
        ld_inst.has_result = true
        b.instructions.push(ld_inst)
        g.builder.next_id = ld + 1
        let wide_t = if is_float_type(mem_t) { "f64" } else { "i64" }
        return emit_ir_cast(g, b, ir_val_inst(ld), mem_t, wide_t)
    }

    -- store_T(addr, v)
    if starts_with(fn_name, "store_") {
        let p = generate_raw_ptr_arg(g, b, fn_name, args[0])
        g = p.gen
        b = p.block
        let val_t = infer_expr_type(g, args[1])
        let fits = if is_float_type(mem_t) { is_float_type(val_t) or is_integer_type(val_t) } else { is_integer_type(val_t) }
        if fits == false {
            g = ir_type_error(g, fn_name + "() cannot store a value of type '" + builtin_src_type(val_t) + "'", expr.line)
            return expr_result(g, b, 0)
        }
        let v = generate_expr(g, b, args[1])
        let narrow = emit_ir_cast(v.gen, v.block, ir_val_inst(v.val_id), val_t, mem_t)
        let st = emit_ir_store(narrow.gen, narrow.block, ir_val_inst(p.val_id), ir_val_inst(narrow.val_id))
        return expr_result(st.gen, st.block, 0)
    }

    -- addr_of(place)
    if fn_name == "addr_of" {
        let a = generate_addr(g, b, args[0])
        g = a.gen
        b = a.block
        if a.ok == false {
            g = ir_type_error(g, "addr_of() takes a variable, a field or a list element", expr.line)
            return expr_result(g, b, 0)
        }
        return emit_ir_cast(g, b, a.ptr, "ptr", "i64")
    }

    -- string_data(s)
    if fn_name == "string_data" {
        let s_t = infer_expr_type(g, args[0])
        if s_t != "string" {
            g = ir_type_error(g, "string_data() takes a string, not '" + builtin_src_type(s_t) + "'", expr.line)
            return expr_result(g, b, 0)
        }
        let s = generate_expr(g, b, args[0])
        g = s.gen
        b = s.block
        let slot = g.builder.next_id
        let mut slot_inst = ir_inst_new(slot, OP_ALLOCA())
        slot_inst.alloc_type = "string"
        slot_inst.result_type = "ptr(string)"
        slot_inst.has_result = true
        b.instructions.push(slot_inst)
        g.builder.next_id = slot + 1
        let st = emit_ir_store(g, b, ir_val_inst(slot), ir_val_inst(s.val_id))
        let data_addr = emit_ir_field_addr(st.gen, st.block, ir_val_inst(slot), "string", 0, "ptr")
        g = data_addr.gen
        b = data_addr.block
        let ld = g.builder.next_id
        let mut ld_inst = ir_inst_new(ld, OP_LOAD())
        ld_inst.load_ptr = ir_val_inst(data_addr.val_id)
        ld_inst.load_type = "ptr"
        ld_inst.result_type = "ptr"
        ld_inst.has_result = true
        b.instructions.push(ld_inst)
        g.builder.next_id = ld + 1
        return emit_ir_cast(g, b, ir_val_inst(ld), "ptr", "i64")
    }

    -- string_from_raw(ptr, len, cap)
    let p = generate_raw_ptr_arg(g, b, fn_name, args[0])
    let n = generate_raw_int_arg(p.gen, p.block, fn_name, "length", args[1])
    let c = generate_raw_int_arg(n.gen, n.block, fn_name, "capacity", args[2])
    g = c.gen
    b = c.block
    let slot = g.builder.next_id
    let mut slot_inst = ir_inst_new(slot, OP_ALLOCA())
    slot_inst.alloc_type = "string"
    slot_inst.result_type = "ptr(string)"
    slot_inst.has_result = true
    b.instructions.push(slot_inst)
    g.builder.next_id = slot + 1
    let mut fi = 0
    while fi < 3 {
        let field_t = if fi == 0 { "ptr" } else { "i64" }
        let field_val = if fi == 0 { p.val_id } else if fi == 1 { n.val_id } else { c.val_id }
        let fa = emit_ir_field_addr(g, b, ir_val_inst(slot), "string", fi, field_t)
        let st = emit_ir_store(fa.gen, fa.block, ir_val_inst(fa.val_id), ir_val_inst(field_val))
        g = st.gen
        b = st.block
        fi = fi + 1
    }
    let ld = g.builder.next_id
    let mut ld_inst = ir_inst_new(ld, OP_LOAD())
    ld_inst.load_ptr = ir_val_inst(slot)
    ld_inst.load_type = "string"
    ld_inst.result_type = "string"
    ld_inst.has_result = true
    b.instructions.push(ld_inst)
    g.builder.next_id = ld + 1
    return expr_result(g, b, ld)
}

fn static_type(gen: IRGenerator, expr: Expr) -> string {
    let found = static_type_from_parts(gen, expr)
    if found != "" { return found }
    -- What cannot be told from the expression's parts, the checker
    -- found from the whole program (see CHECKED TYPES in ir_gen.dm)
    return checked_type(gen, expr)
}

fn static_type_from_parts(gen: IRGenerator, expr: Expr) -> string {
    let k = expr.kind
    if k == EXPR_LITERAL_INT() { return "int" }
    if k == EXPR_LITERAL_FLOAT() { return "float" }
    if k == EXPR_LITERAL_STRING() or k == EXPR_STRING_INTERP() { return "string" }
    if k == EXPR_LITERAL_BOOL() { return "bool" }
    if k == EXPR_STRUCT_LITERAL() { return canon_type(gen, expr.type_name) }
    if k == EXPR_ENUM_LITERAL() {
        if is_generic_enum(gen, expr.enum_name) {
            return canon_type(gen, generic_enum_type(gen, expr, expr.enum_name, expr.variant_name, expr.payload))
        }
        return expr.enum_name
    }
    if k == EXPR_ARRAY_LITERAL() {
        -- A list of its first element's type
        if expr.elements.len() == 0 { return "" }
        let elem_t = static_type(gen, expr.elements[0])
        if elem_t == "" { return "" }
        return "List[" + elem_t + "]"
    }
    if k == EXPR_GROUPED() { return static_type(gen, *expr.operand) }
    if k == EXPR_BINARY() {
        -- Two strings joined are a string; other operators may be
        -- overloaded, so their type is not known here
        if expr.op == BINOP_ADD() and static_type(gen, *expr.left) == "string" and static_type(gen, *expr.right) == "string" {
            return "string"
        }
        return ""
    }
    if k == EXPR_IDENTIFIER() {
        if gen.var_src_types.contains(expr.name) { return gen.var_src_types.get(expr.name) }
        if gen.var_struct_types.contains(expr.name) { return gen.var_struct_types.get(expr.name) }
        return ""
    }
    if k == EXPR_FIELD_ACCESS() {
        let obj_t = strip_box_type(static_type(gen, *expr.object))
        return struct_field_src_type(gen, obj_t, expr.field)
    }
    if k == EXPR_INDEX_ACCESS() {
        -- What is in a box is indexed as itself (unboxed_container)
        let mut obj_t = static_type(gen, *expr.object)
        if starts_with(obj_t, "Box[") { obj_t = extract_inner_type(obj_t, "Box[") }
        if is_map_type(gen, obj_t) { return map_instance_arg(gen, obj_t, 1) }
        if starts_with(obj_t, "List[") { return extract_inner_type(obj_t, "List[") }
        if obj_t == "string" { return "string" }
        return ""
    }
    if k == EXPR_UNARY() {
        if expr.op == UNOP_DEREF() {
            let inner_t = static_type(gen, *expr.operand)
            if starts_with(inner_t, "Box[") { return extract_inner_type(inner_t, "Box[") }
            return ""
        }
        if expr.op == UNOP_NOT() { return "bool" }
        return static_type(gen, *expr.operand)
    }
    if k == EXPR_FUNCTION_CALL() {
        let callee = *expr.callee
        if callee.kind != EXPR_IDENTIFIER() { return "" }
        let fname = callee.name
        if fname == "Box_new" and expr.args.len() == 1 {
            let inner_t = static_type(gen, expr.args[0])
            if inner_t == "" { return "" }
            return "Box[" + inner_t + "]"
        }
        if gen.generic_fn_decls.contains(fname) { return "" }
        if gen.fn_src_return_types.contains(fname) { return gen.fn_src_return_types.get(fname) }
        if is_builtin_fn(fname) { return builtin_src_type(builtin_return_type(fname)) }
        return ""
    }
    if k == EXPR_METHOD_CALL() {
        if starts_with(checked_target(gen, expr), "call:") {
            return static_type(gen, method_as_call(gen, expr))
        }
        let static_target = static_call_target(gen, expr)
        if static_target != "" {
            return static_type(gen, static_call_expr(expr, static_target))
        }
        -- Enum.Variant(values) is a value of the enum
        let call_object = *expr.object
        if call_object.kind == EXPR_IDENTIFIER() and gen.enum_defs.contains(call_object.name) and gen.variable_map.contains(call_object.name) == false {
            if enum_variant_index(gen.enum_defs.get(call_object.name), expr.method) >= 0 {
                return call_object.name
            }
        }
        if call_object.kind == EXPR_IDENTIFIER() and gen.variable_map.contains(call_object.name) == false and is_generic_enum(gen, call_object.name) {
            if generic_enum_has_variant(gen, call_object.name, expr.method) {
                return canon_type(gen, generic_enum_type(gen, expr, call_object.name, expr.method, expr.args))
            }
        }
        let recv_t = strip_box_type(static_type(gen, *expr.object))
        let m = expr.method
        if gen.generic_instances.contains(recv_t) {
            let base_key = gen.generic_instances.get(recv_t) + "_" + m
            if gen.fn_src_return_types.contains(base_key) {
                return instance_type(gen, recv_t, gen.fn_src_return_types.get(base_key))
            }
            return ""
        }
        if gen.struct_defs.contains(recv_t) {
            let key = recv_t + "_" + m
            if gen.fn_src_return_types.contains(key) { return gen.fn_src_return_types.get(key) }
            return ""
        }
        if recv_t == "string" and m == "len" { return "int" }
        if starts_with(recv_t, "List[") {
            if m == "len" { return "int" }
            if m == "pop" or m == "get" { return extract_inner_type(recv_t, "List[") }
            if m == "contains" { return "bool" }
            return ""
        }
        if recv_t == "string" and m == "len" { return "int" }
        return ""
    }
    return ""
}

-- ============================================================
-- ADDRESSES
-- Storage an expression names, so that assignments and list
-- operations change the value in place rather than a copy.
-- ============================================================

struct AddrResult {
    gen: IRGenerator,
    block: IRBasicBlock,
    -- Pointer to the storage
    ptr: IRValue,
    -- Source type of the value stored there
    src_type: string,
    -- false when the expression names no storage
    ok: bool
}

fn addr_result(g: IRGenerator, b: IRBasicBlock, ptr: IRValue, src_type: string) -> AddrResult {
    return AddrResult { gen: g, block: b, ptr: ptr, src_type: src_type, ok: true }
}

fn addr_fail(g: IRGenerator, b: IRBasicBlock) -> AddrResult {
    return AddrResult { gen: g, block: b, ptr: ir_val_undef(), src_type: "", ok: false }
}

fn struct_field_index(gen: IRGenerator, struct_name: string, field: string) -> int {
    if gen.struct_defs.contains(struct_name) == false { return -1 }
    let info = gen.struct_defs.get(struct_name)
    let mut i = 0
    while i < info.field_names.len() {
        if info.field_names[i] == field { return i }
        i = i + 1
    }
    return -1
}

-- Address of a variable, a field of an addressable or boxed struct, or `*box`.
fn generate_addr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> AddrResult {
    let mut g = gen
    let mut b = block
    if expr.kind == EXPR_GROUPED() {
        return generate_addr(g, b, *expr.operand)
    }
    if expr.kind == EXPR_IDENTIFIER() {
        let var_t = static_type(g, expr)
        -- mut parameters are pointers to the caller's storage
        if g.param_ptr_map.contains(expr.name) {
            return addr_result(g, b, ir_val_param(g.param_ptr_map.get(expr.name)), var_t)
        }
        if g.variable_map.contains(expr.name) {
            let slot = g.variable_map.get(expr.name)
            if slot >= 0 {
                return addr_result(g, b, ir_val_inst(slot), var_t)
            }
        }
        return addr_fail(g, b)
    }
    if expr.kind == EXPR_UNARY() and expr.op == UNOP_DEREF() {
        let box_t = static_type(g, *expr.operand)
        if starts_with(box_t, "Box[") == false {
            return addr_fail(g, b)
        }
        let box_result = generate_expr(g, b, *expr.operand)
        g = box_result.gen
        b = box_result.block
        let chk = check_box_not_null(g, b, box_result.val_id)
        g = chk.gen
        b = chk.block
        return addr_result(g, b, ir_val_inst(box_result.val_id), extract_inner_type(box_t, "Box["))
    }
    if expr.kind == EXPR_INDEX_ACCESS() {
        let indexed = unboxed_container(g, *expr.object)
        let list_t = static_type(g, indexed)
        if starts_with(list_t, "List[") == false {
            return addr_fail(g, b)
        }
        let h = generate_list_handle(g, b, indexed)
        g = h.gen
        b = h.block
        if h.ok == false {
            return addr_fail(g, b)
        }
        let elem_src = extract_inner_type(list_t, "List[")
        let elem_ir = list_elem_ir_type(elem_src)
        let ek = classify_list_elem(elem_src)
        let idx_result = generate_expr(g, b, *expr.index)
        g = idx_result.gen
        b = idx_result.block
        let len_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_len" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_len" }
        else if ek == LIST_ELEM_STRING() { "dm_list_string_len" }
        else { "dm_list_generic_len" }
        let len_id = g.builder.next_id
        let mut len_inst = ir_inst_new(len_id, OP_CALL())
        len_inst.callee = len_fn
        let mut len_args: List[IRValue] = []
        len_args.push(h.ptr)
        len_inst.call_args = len_args
        len_inst.result_type = "i64"
        len_inst.has_result = true
        b.instructions.push(len_inst)
        g.builder.next_id = len_id + 1
        let chk = check_index_bounds(g, b, idx_result.val_id, ir_val_inst(len_id), "(list)")
        g = chk.gen
        b = chk.block
        let elem_addr = emit_list_elem_addr(g, b, h.ptr, idx_result.val_id, elem_ir)
        return addr_result(elem_addr.gen, elem_addr.block, ir_val_inst(elem_addr.val_id), elem_src)
    }
    if expr.kind == EXPR_FIELD_ACCESS() {
        let obj = *expr.object
        let obj_t = static_type(g, obj)
        let mut struct_name = obj_t
        let mut base = ir_val_undef()
        if starts_with(obj_t, "Box[") {
            -- A boxed struct lives on the heap: the box value is its address
            struct_name = extract_inner_type(obj_t, "Box[")
            let box_result = generate_expr(g, b, obj)
            g = box_result.gen
            b = box_result.block
            let chk = check_box_not_null(g, b, box_result.val_id)
            g = chk.gen
            b = chk.block
            base = ir_val_inst(box_result.val_id)
        } else {
            let obj_addr = generate_addr(g, b, obj)
            g = obj_addr.gen
            b = obj_addr.block
            if obj_addr.ok == false {
                return addr_fail(g, b)
            }
            base = obj_addr.ptr
        }
        let fidx = struct_field_index(g, struct_name, expr.field)
        if fidx < 0 {
            return addr_fail(g, b)
        }
        let field_src = struct_field_src_type(g, struct_name, expr.field)
        let field_ir = map_type_name(field_src)
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_GEP())
        inst.gep_base = base
        inst.alloc_type = struct_name
        inst.field_index = fidx
        inst.field_type = field_ir
        inst.result_type = "ptr(" + field_ir + ")"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return addr_result(g, b, ir_val_inst(id), field_src)
    }
    return addr_fail(g, b)
}

-- Pointer to a list for the runtime's list functions. A list variable
-- or field is used in place. Any other list value (a call result, a
-- list element) is copied to a temporary and `ok` is false, because
-- changes to that copy would be lost.
fn generate_list_handle(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> AddrResult {
    let mut g = gen
    let mut b = block
    let list_t = static_type(g, expr)
    if expr.kind == EXPR_IDENTIFIER() {
        -- A list variable's value is already a pointer to its storage
        let var_result = generate_expr(g, b, expr)
        return addr_result(var_result.gen, var_result.block, ir_val_inst(var_result.val_id), list_t)
    }
    if expr.kind == EXPR_FIELD_ACCESS() or expr.kind == EXPR_INDEX_ACCESS() or expr.kind == EXPR_UNARY() or expr.kind == EXPR_GROUPED() {
        let a = generate_addr(g, b, expr)
        g = a.gen
        b = a.block
        if a.ok {
            return a
        }
    }
    let val_result = generate_expr(g, b, expr)
    g = val_result.gen
    b = val_result.block
    let tmp_id = g.builder.next_id
    let mut tmp_inst = ir_inst_new(tmp_id, OP_ALLOCA())
    tmp_inst.alloc_type = list_t
    tmp_inst.result_type = "ptr(" + list_t + ")"
    tmp_inst.has_result = true
    b.instructions.push(tmp_inst)
    g.builder.next_id = tmp_id + 1
    let mut st_inst = ir_inst_new(g.builder.next_id, OP_STORE())
    st_inst.store_ptr = ir_val_inst(tmp_id)
    st_inst.store_val = ir_val_inst(val_result.val_id)
    b.instructions.push(st_inst)
    g.builder.next_id = g.builder.next_id + 1
    let mut copy = addr_result(g, b, ir_val_inst(tmp_id), list_t)
    copy.ok = false
    return copy
}

-- The address of element idx_id of the list whose header is at
-- list_ptr and whose elements have IR type elem_ir. The caller has
-- checked the index, and nothing may run between that check and the
-- use of the address.
fn emit_list_elem_addr(gen: IRGenerator, block: IRBasicBlock, list_ptr: IRValue, idx_id: int, elem_ir: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_GEP())
    inst.gep_base = list_ptr
    let mut gep_idx: List[IRValue] = []
    gep_idx.push(ir_val_inst(idx_id))
    inst.gep_indices = gep_idx
    inst.alloc_type = elem_ir
    inst.field_type = elem_ir
    inst.result_type = "ptr(" + elem_ir + ")"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- IR type of the elements a list of this kind stores
fn list_slot_ir_type(elem_kind: int, elem_type: string) -> string {
    if elem_kind == LIST_ELEM_INT() { return "i64" }
    if elem_kind == LIST_ELEM_FLOAT() { return "f64" }
    if elem_kind == LIST_ELEM_STRING() { return "string" }
    return elem_type
}

-- Whether loading a value of IR type t gives the address it was
-- loaded from instead of a copy (strings and lists)
fn ir_load_gives_address(t: string) -> bool {
    return t == "string" or t == "list" or starts_with(t, "List[") or starts_with(t, "dm_list_")
}

-- IR element type of a list whose elements have source type elem_src
fn list_elem_ir_type(elem_src: string) -> string {
    let kind = classify_list_elem(elem_src)
    if kind == LIST_ELEM_INT() { return "i64" }
    if kind == LIST_ELEM_FLOAT() { return "f64" }
    if kind == LIST_ELEM_STRING() { return "string" }
    return elem_src
}

-- ============================================================
-- EXPRESSION TYPE INFERENCE
-- Determines the IR type name for an expression without
-- generating instructions. Used to select the correct opcode
-- family (integer, float, string) for binary operations.
-- ============================================================

-- Whether the checker's type for the expression is used before any
-- found here (see CHECKED TYPES in ir_gen.dm): for an `if`, a `match`
-- or a block, whose type here is that of their first branch; for
-- None, Some(x), Ok(x) and Err(x), whose type depends on where the
-- value goes; and for a call of a generic function
fn checked_type_first(gen: IRGenerator, expr: Expr) -> bool {
    let k = expr.kind
    if k == EXPR_IF() or k == EXPR_MATCH() or k == EXPR_BLOCK() { return true }
    if k == EXPR_IDENTIFIER() { return expr.name == "None" and gen.variable_map.contains("None") == false }
    if k == EXPR_FUNCTION_CALL() {
        let callee = *expr.callee
        if callee.kind != EXPR_IDENTIFIER() { return false }
        let name = callee.name
        return name == "Some" or name == "Ok" or name == "Err" or gen.generic_fn_decls.contains(name)
    }
    return false
}

-- The function that `recv.method(...)` calls when the receiver is a
-- struct or an enum value, or a box of one, whose type has a method of
-- the name: "Counter_get". "" for any other receiver, and for a method
-- of a generic struct, which is found through its instance.
fn own_method_of(gen: IRGenerator, recv: Expr, method: string) -> string {
    let recv_type = strip_box_type(static_type(gen, recv))
    if recv_type == "" { return "" }
    if gen.struct_defs.contains(recv_type) == false and gen.enum_defs.contains(recv_type) == false { return "" }
    if gen.generic_instances.contains(recv_type) { return "" }
    let mangled = recv_type + "_" + method
    if gen.generic_fn_decls.contains(mangled) { return "" }
    if gen.fn_return_types.contains(mangled) { return mangled }
    return ""
}

fn infer_expr_type(gen: IRGenerator, expr: Expr) -> string {
    if checked_type_first(gen, expr) {
        let checked = checked_ir_type(gen, expr)
        if checked != "" { return checked }
        let k = expr.kind
        if k == EXPR_IF() or k == EXPR_MATCH() or k == EXPR_BLOCK() {
            return checked_slot_type(gen, expr, infer_expr_type_from_parts(gen, expr))
        }
    }
    return infer_expr_type_from_parts(gen, expr)
}

-- The type of the expression as its parts and the variables declared
-- so far give it
fn infer_expr_type_from_parts(gen: IRGenerator, expr: Expr) -> string {
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
        -- Check global constants for their type
        let mut gi_ty = 0
        while gi_ty < gen.ir_mod.globals.len() {
            let gl_ty = gen.ir_mod.globals[gi_ty]
            if gl_ty.name == name and gl_ty.has_init {
                if gl_ty.init_value.kind == VAL_CONST_STRING() {
                    return "string"
                }
                if gl_ty.init_value.kind == VAL_CONST_FLOAT() {
                    return "f64"
                }
                if gl_ty.init_value.kind == VAL_CONST_BOOL() {
                    return "bool"
                }
                return "i64"
            }
            gi_ty = gi_ty + 1
        }
        return "i64"
    }
    -- A lambda is the address of its function
    if expr.kind == EXPR_LAMBDA() {
        return "ptr"
    }
    if expr.kind == EXPR_BINARY() {
        -- Comparison and logical operators always produce bool
        if is_bool_binop(expr.op) {
            return "bool"
        }
        -- Arithmetic. A chain a + b + c + ... nests to the left, one
        -- level for each operator, so its type is worked out in a loop
        -- from the innermost operator outwards; by recursion, a chain
        -- of a few hundred overflowed the stack. Each operand's type is
        -- inferred once: inferring the left one twice doubles the work
        -- at every level.
        let mut levels: List[Expr] = []
        let mut node = expr
        while node.kind == EXPR_BINARY() and is_bool_binop(node.op) == false {
            levels.push(node)
            node = *node.left
        }
        let mut chain_type = infer_expr_type(gen, node)
        let mut level = levels.len() - 1
        while level >= 0 {
            -- Integers of different types meet at one type; otherwise
            -- the operator has its left operand's type
            let common_t = common_int_type(levels[level], chain_type, infer_expr_type(gen, *levels[level].right))
            if common_t != "" {
                chain_type = common_t
            }
            level = level - 1
        }
        return chain_type
    }
    if expr.kind == EXPR_UNARY() {
        if expr.op == UNOP_NOT() {
            return "bool"
        }
        if expr.op == UNOP_DEREF() {
            let deref_src = static_type(gen, expr)
            if deref_src != "" { return map_type_name(deref_src) }
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
            -- SIMD builtins: infer return type from function name or argument types
            if is_simd_builtin(fn_name) {
                if starts_with(fn_name, "simd_splat_") or starts_with(fn_name, "simd_set_") {
                    return parse_simd_type_from_name(fn_name)
                }
                if fn_name == "simd_add" or fn_name == "simd_sub" or fn_name == "simd_mul" or fn_name == "simd_div" {
                    -- Return type is the same as the first argument's vector type
                    if expr.args.len() > 0 {
                        return infer_expr_type(gen, expr.args[0])
                    }
                    return "f32x4"
                }
                if fn_name == "simd_extract" {
                    -- Return dAImond-native type (f64 for float vectors, i64 for int vectors)
                    if expr.args.len() > 0 {
                        let vec_ty = infer_expr_type(gen, expr.args[0])
                        if is_simd_float_type(vec_ty) {
                            return "f64"
                        }
                        return "i64"
                    }
                    return "f64"
                }
            }
            -- Look up user-defined function return types
            let mut fi = 0
            while fi < gen.ir_mod.functions.len() {
                let func = gen.ir_mod.functions[fi]
                if func.name == fn_name {
                    return func.return_type
                }
                fi = fi + 1
            }
            -- Check for generic function declarations
            if gen.generic_fn_decls.contains(fn_name) {
                let decl_idx: int = gen.generic_fn_decls.get(fn_name)
                if decl_idx >= 0 and decl_idx < gen.declarations.len() {
                    let decl = gen.declarations[decl_idx]
                    if decl.kind == DECL_FUNCTION() {
                        let fd = *decl.func_decl
                        let ret_type = fd.return_type
                        -- The return type with the type arguments the
                        -- checker found for this call
                        let call_type_args = checked_type_args(gen, expr)
                        if expr.generic_args.len() == 0 and call_type_args.len() == fd.generic_params.len() and call_type_args.len() > 0 {
                            let mut with_args = gen
                            with_args.type_substitutions = Map_new()
                            let mut ta = 0
                            while ta < call_type_args.len() {
                                with_args.type_substitutions.insert(fd.generic_params[ta], call_type_args[ta])
                                ta = ta + 1
                            }
                            return map_type_name(canon_type(with_args, ret_type))
                        }
                        -- If return type is a generic param, resolve from args
                        if fd.generic_params.len() > 0 {
                            let mut pi = 0
                            while pi < fd.generic_params.len() {
                                if fd.generic_params[pi] == ret_type {
                                    -- Infer from corresponding argument
                                    if pi < expr.args.len() {
                                        return infer_expr_type(gen, expr.args[pi])
                                    }
                                }
                                pi = pi + 1
                            }
                            -- Also check explicit type args
                            if expr.generic_args.len() > 0 {
                                let mut gi = 0
                                while gi < fd.generic_params.len() {
                                    if fd.generic_params[gi] == ret_type and gi < expr.generic_args.len() {
                                        return map_type_name(expr.generic_args[gi])
                                    }
                                    gi = gi + 1
                                }
                            }
                        }
                        return map_type_name(ret_type)
                    }
                }
            }
            -- Declared later in the source: use the signature registered up front
            if gen.fn_return_types.contains(fn_name) {
                return gen.fn_return_types.get(fn_name)
            }
            -- Check if callee is a lambda/fn-ptr variable: look up actual function
            if gen.fn_ptr_vars.contains(fn_name) {
                let lambda_fn_name = gen.fn_ptr_vars.get(fn_name)
                let mut lfi = 0
                while lfi < gen.ir_mod.functions.len() {
                    if gen.ir_mod.functions[lfi].name == lambda_fn_name {
                        return gen.ir_mod.functions[lfi].return_type
                    }
                    lfi = lfi + 1
                }
            }
            -- Check if callee is a function-pointer-typed variable: parse return type
            if gen.variable_types.contains(fn_name) {
                let vtype = gen.variable_types.get(fn_name)
                if starts_with(vtype, "fn(") {
                    let arrow_pos = string_find(vtype, " -> ")
                    if arrow_pos >= 0 {
                        let ret_str = substr(vtype, arrow_pos + 4, len(vtype) - arrow_pos - 4)
                        return map_type_name(ret_str)
                    }
                }
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
    if expr.kind == EXPR_PIPELINE() {
        -- Pipeline: a |> f — return type of function f
        let rhs = *expr.pipe_right
        if rhs.kind == EXPR_FUNCTION_CALL() {
            let callee = *rhs.callee
            if callee.kind == EXPR_IDENTIFIER() {
                let fn_name = callee.name
                if is_builtin_fn(fn_name) {
                    return builtin_return_type(fn_name)
                }
                let mut fi = 0
                while fi < gen.ir_mod.functions.len() {
                    let func = gen.ir_mod.functions[fi]
                    if func.name == fn_name {
                        return func.return_type
                    }
                    fi = fi + 1
                }
                if gen.fn_return_types.contains(fn_name) and gen.generic_fn_decls.contains(fn_name) == false {
                    return gen.fn_return_types.get(fn_name)
                }
            }
        } else if rhs.kind == EXPR_IDENTIFIER() {
            let fn_name = rhs.name
            if is_builtin_fn(fn_name) {
                return builtin_return_type(fn_name)
            }
            let mut fi = 0
            while fi < gen.ir_mod.functions.len() {
                let func = gen.ir_mod.functions[fi]
                if func.name == fn_name {
                    return func.return_type
                }
                fi = fi + 1
            }
        }
        return "i64"
    }
    if expr.kind == EXPR_STRUCT_LITERAL() {
        return canon_type(gen, expr.type_name)
    }
    if expr.kind == EXPR_ENUM_LITERAL() {
        if is_generic_enum(gen, expr.enum_name) {
            return canon_type(gen, generic_enum_type(gen, expr, expr.enum_name, expr.variant_name, expr.payload))
        }
        return expr.enum_name
    }
    if expr.kind == EXPR_IF() {
        -- Type of the then branch's trailing expression, else the else branch's
        -- (a branch may end in `return` instead)
        let then_t = infer_expr_type(gen, *expr.then_branch)
        if expr.has_else == false { return then_t }
        let else_t = infer_expr_type(gen, *expr.else_branch)
        if then_t == "void" { return else_t }
        -- Integers of different widths meet at one type
        if is_integer_type(then_t) and is_integer_type(else_t) {
            return int_meet_type(then_t, is_int_literal_expr(branch_value_expr(*expr.then_branch)), else_t, is_int_literal_expr(branch_value_expr(*expr.else_branch)))
        }
        return then_t
    }
    if expr.kind == EXPR_BLOCK() {
        -- The type of the expression that ends the block, which an
        -- `if` with an `else` and a `match` are too
        if expr.stmts.len() > 0 {
            let last = expr.stmts[expr.stmts.len() - 1]
            if stmt_gives_value(last) {
                return infer_expr_type(gen, stmt_value_expr(last))
            }
        }
        return "void"
    }
    if expr.kind == EXPR_MATCH() {
        -- The type of the first arm, and for integers the type the
        -- arms meet at
        if expr.match_arms.len() > 0 {
            let arm = expr.match_arms[0]
            if arm.is_expr_body {
                return arms_meet_type(gen, expr.match_arms, infer_expr_type(gen, *arm.body_expr))
            }
        }
        return "void"
    }
    if expr.kind == EXPR_FIELD_ACCESS() {
        -- Look up struct field type
        let obj = *expr.object
        let obj_type = infer_expr_type(gen, obj)
        if gen.struct_defs.contains(obj_type) {
            let si = gen.struct_defs.get(obj_type)
            let mut fi = 0
            while fi < si.field_names.len() {
                if si.field_names[fi] == expr.field {
                    return si.field_types[fi]
                }
                fi = fi + 1
            }
        }
        -- Check variable's struct type
        if obj.kind == EXPR_IDENTIFIER() and gen.var_struct_types.contains(obj.name) {
            let sname = gen.var_struct_types.get(obj.name)
            if gen.struct_defs.contains(sname) {
                let si = gen.struct_defs.get(sname)
                let mut fi = 0
                while fi < si.field_names.len() {
                    if si.field_names[fi] == expr.field {
                        return si.field_types[fi]
                    }
                    fi = fi + 1
                }
            }
        }
        -- Check Option[T] / Result[T, E] types for field access
        if obj.kind == EXPR_IDENTIFIER() and gen.variable_types.contains(obj.name) {
            let fa_vtype = gen.variable_types.get(obj.name)
            if starts_with(fa_vtype, "Option[") {
                let fa_inner = substr(fa_vtype, 7, len(fa_vtype) - 8)
                if expr.field == "value" {
                    return map_type_name(fa_inner)
                }
                if expr.field == "has_value" {
                    return "i32"
                }
            }
            if starts_with(fa_vtype, "Result[") {
                let fa_inner_str = substr(fa_vtype, 7, len(fa_vtype) - 8)
                let mut fa_comma = 0
                let mut fa_ci = 0
                while fa_ci < len(fa_inner_str) {
                    if char_at(fa_inner_str, fa_ci) == "," {
                        fa_comma = fa_ci
                    }
                    fa_ci = fa_ci + 1
                }
                let fa_ok_type = string_trim(substr(fa_inner_str, 0, fa_comma))
                let fa_err_type = string_trim(substr(fa_inner_str, fa_comma + 1, len(fa_inner_str) - fa_comma - 1))
                if expr.field == "ok" or expr.field == "value" {
                    return map_type_name(fa_ok_type)
                }
                if expr.field == "err" or expr.field == "error" {
                    return map_type_name(fa_err_type)
                }
                if expr.field == "is_ok" {
                    return "bool"
                }
            }
        }
        let field_src = static_type(gen, expr)
        if field_src != "" { return map_type_name(field_src) }
        return "i64"
    }
    if expr.kind == EXPR_INDEX_ACCESS() {
        let obj = *expr.object
        if obj.kind == EXPR_IDENTIFIER() {
            let name = obj.name
            -- String indexing returns a string (char_at)
            if gen.variable_types.contains(name) {
                let idx_vtype = gen.variable_types.get(name)
                if idx_vtype == "string" {
                    return "string"
                }
            }
            if gen.list_elem_kinds.contains(name) {
                let ek = gen.list_elem_kinds.get(name)
                if ek == LIST_ELEM_INT() { return "i64" }
                if ek == LIST_ELEM_FLOAT() { return "f64" }
                if ek == LIST_ELEM_STRING() { return "string" }
                if ek == LIST_ELEM_OTHER() and gen.list_elem_types.contains(name) {
                    return gen.list_elem_types.get(name)
                }
            }
        }
        let idx_src = static_type(gen, expr)
        if idx_src != "" {
            return list_elem_ir_type(idx_src)
        }
        return "i64"
    }
    if expr.kind == EXPR_METHOD_CALL() {
        if starts_with(checked_target(gen, expr), "call:") {
            return infer_expr_type(gen, method_as_call(gen, expr))
        }
        let static_target = static_call_target(gen, expr)
        if static_target != "" {
            return infer_expr_type(gen, static_call_expr(expr, static_target))
        }
        let method = expr.method
        -- Methods of generic struct instances have exact signatures
        if gen.generic_instances.contains(strip_box_type(static_type(gen, *expr.object))) {
            let mc_src = static_type(gen, expr)
            if mc_src == "" { return "void" }
            return map_type_name(mc_src)
        }
        -- A struct's or an enum's own method, whatever its name: the
        -- type it declares, not that of the list or map method of the
        -- name, nor of another struct's method of the name
        let own_method = own_method_of(gen, *expr.object, method)
        if own_method != "" { return infer_call_return_type(gen, own_method) }
        if method == "len" { return "i64" }
        if method == "pop" {
            let pop_src = static_type(gen, expr)
            if pop_src != "" { return list_elem_ir_type(pop_src) }
            return "i64"
        }
        if method == "get" {
            return "i64"
        }
        if method == "contains" { return "bool" }
        -- Check for struct method calls (regular and generic impl)
        if gen.method_struct_map.contains(method) {
            let struct_type = gen.method_struct_map.get(method)
            let mangled = struct_type + "_" + method
            -- Check generic impl method
            if gen.generic_fn_decls.contains(mangled) {
                let decl_idx: int = gen.generic_fn_decls.get(mangled)
                if decl_idx >= 0 and decl_idx < gen.declarations.len() {
                    let decl = gen.declarations[decl_idx]
                    if decl.kind == DECL_FUNCTION() {
                        let fd = *decl.func_decl
                        -- Check if method takes self
                        let mut has_self = false
                        if fd.params.len() > 0 {
                            has_self = fd.params[0].name == "self"
                        }
                        -- Build effective args
                        let mut effective_args: List[Expr] = []
                        if has_self {
                            effective_args.push(*expr.object)
                        }
                        let mut ai = 0
                        while ai < expr.args.len() {
                            effective_args.push(expr.args[ai])
                            ai = ai + 1
                        }
                        -- Infer type args
                        let mut temp_subs: Map[string, string] = Map_new()
                        let mut pi = 0
                        while pi < fd.generic_params.len() {
                            let gp = fd.generic_params[pi]
                            let mut inferred = "i64"
                            let mut mi = 0
                            while mi < fd.params.len() and mi < effective_args.len() {
                                if fd.params[mi].type_name == gp {
                                    inferred = infer_arg_type(gen, effective_args[mi])
                                }
                                mi = mi + 1
                            }
                            temp_subs.insert(gp, inferred)
                            pi = pi + 1
                        }
                        -- Resolve return type
                        let ret = fd.return_type
                        if temp_subs.contains(ret) {
                            return map_type_name(temp_subs.get(ret))
                        }
                        -- Handle complex return types like Pair[A, B]
                        let bracket_idx = string_find(ret, "[")
                        if bracket_idx > 0 {
                            let base = substr(ret, 0, bracket_idx)
                            let inner = substr(ret, bracket_idx + 1, len(ret) - bracket_idx - 2)
                            let comma_idx = string_find(inner, ", ")
                            if comma_idx >= 0 {
                                let first_t = substr(inner, 0, comma_idx)
                                let second_t = substr(inner, comma_idx + 2, len(inner) - comma_idx - 2)
                                let r1 = if temp_subs.contains(first_t) { map_type_name(temp_subs.get(first_t)) } else { map_type_name(first_t) }
                                let r2 = if temp_subs.contains(second_t) { map_type_name(temp_subs.get(second_t)) } else { map_type_name(second_t) }
                                return base + "_" + r1 + "_" + r2
                            } else {
                                let r1 = if temp_subs.contains(inner) { map_type_name(temp_subs.get(inner)) } else { map_type_name(inner) }
                                return base + "_" + r1
                            }
                        }
                        return map_type_name(ret)
                    }
                }
            }
            -- Non-generic struct method: look up return type from IR functions
            let ret_type = infer_call_return_type(gen, mangled)
            return ret_type
        }
        return "i64"
    }
    if expr.kind == EXPR_COMPTIME() {
        return infer_expr_type(gen, *expr.operand)
    }
    if expr.kind == EXPR_ARRAY_LITERAL() {
        return "list"
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
-- ============================================================

fn generate_binary(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    -- A chain such as a + b + c + ... nests to the left, one level
    -- for each operator. Its levels are generated in a loop, from the
    -- innermost outwards: generated by recursion, each level takes
    -- more than a megabyte of stack, and a chain of 40 overflowed it.
    let mut spine: List[Expr] = []
    let mut node = expr
    while node.op != BINOP_AND() and node.op != BINOP_OR() and binary_left_is_binary(node) {
        spine.push(node)
        node = *node.left
    }
    let mut result = generate_binary_level(gen, block, node, -1)
    let mut i = spine.len() - 1
    while i >= 0 {
        result = generate_binary_level(result.gen, result.block, spine[i], result.val_id)
        i = i - 1
    }
    return result
}

fn binary_left_is_binary(expr: Expr) -> bool {
    let left = *expr.left
    return left.kind == EXPR_BINARY()
}

-- Whether the struct has a method of this name, declared or generic
fn struct_has_method(g: IRGenerator, struct_name: string, method: string) -> bool {
    let mangled = struct_name + "_" + method
    let mut fci = 0
    while fci < g.ir_mod.functions.len() {
        if g.ir_mod.functions[fci].name == mangled { return true }
        fci = fci + 1
    }
    if g.generic_fn_decls.contains(mangled) { return true }
    -- A method that is declared further down the file
    let mut dci = 0
    while dci < g.declarations.len() {
        let decl = g.declarations[dci]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.name == mangled { return true }
        }
        dci = dci + 1
    }
    return false
}

-- The method a struct defines an arithmetic operator with
fn arith_overload_method(op: int) -> string {
    if op == BINOP_ADD() { return "add" }
    if op == BINOP_SUB() { return "sub" }
    if op == BINOP_MUL() { return "mul" }
    if op == BINOP_DIV() { return "div" }
    return ""
}

-- A variable no source can name (its name begins with %), which holds
-- a value that is generated already, so that the value can be given
-- where an expression is expected. ty is its IR type and src its
-- source type. unbind_hidden_value forgets it.
fn bind_hidden_value(gen: IRGenerator, block: IRBasicBlock, name: string, val_id: int, ty: string, src: string) -> GenResult {
    let mut g = gen
    let mut b = block
    let slot_id = g.builder.next_id
    let mut slot = ir_inst_new(slot_id, OP_ALLOCA())
    slot.alloc_type = ty
    slot.result_type = "ptr(" + ty + ")"
    slot.has_result = true
    b.instructions.push(slot)
    g.builder.next_id = slot_id + 1
    let stored = emit_ir_store(g, b, ir_val_inst(slot_id), ir_val_inst(val_id))
    g = stored.gen
    g.variable_map.insert(name, slot_id)
    g.variable_types.insert(name, ty)
    if src != "" { g.var_src_types.insert(name, src) }
    if g.struct_defs.contains(ty) { g.var_struct_types.insert(name, ty) }
    return gen_result(g, stored.block)
}

fn unbind_hidden_value(gen: IRGenerator, name: string) -> IRGenerator {
    let mut g = gen
    if g.variable_map.contains(name) { g.variable_map.remove(name) }
    if g.variable_types.contains(name) { g.variable_types.remove(name) }
    if g.var_src_types.contains(name) { g.var_src_types.remove(name) }
    if g.var_struct_types.contains(name) { g.var_struct_types.remove(name) }
    return g
}

-- One binary operator. lhs_done is the value of its left operand when
-- that is generated already, and -1 when it is not. It is only given
-- for an operator that is not `and` or `or` and whose left operand is
-- itself a binary operator.
fn generate_binary_level(gen: IRGenerator, block: IRBasicBlock, expr: Expr, lhs_done: int) -> ExprResult {
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
        g.completed_blocks.push(b)

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
        g.completed_blocks.push(rhs_block)

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
        g.completed_blocks.push(short_block)

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
        g.completed_blocks.push(b)

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
        g.completed_blocks.push(short_block)

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
        g.completed_blocks.push(rhs_block)

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
    -- OPERATOR OVERLOADING: check if left operand is a struct
    -- with an impl method for this operator
    -- --------------------------------------------------------
    let mut overload_method = ""
    if op == BINOP_ADD() { overload_method = "add" }
    if op == BINOP_SUB() { overload_method = "sub" }
    if op == BINOP_MUL() { overload_method = "mul" }
    if op == BINOP_DIV() { overload_method = "div" }
    if op == BINOP_EQ() { overload_method = "eq" }
    if op == BINOP_NE() { overload_method = "ne" }
    if op == BINOP_LT() { overload_method = "lt" }
    if op == BINOP_GT() { overload_method = "gt" }
    if op == BINOP_LE() { overload_method = "le" }
    if op == BINOP_GE() { overload_method = "ge" }

    if len(overload_method) > 0 {
        let left_expr = *expr.left
        let mut left_struct_name = ""
        if left_expr.kind == EXPR_IDENTIFIER() {
            if g.var_struct_types.contains(left_expr.name) {
                left_struct_name = g.var_struct_types.get(left_expr.name)
            }
        }
        if left_struct_name == "" {
            -- Any other left operand that is a struct: a field, a call,
            -- another operator's result
            let ltype = infer_expr_type(g, left_expr)
            if g.struct_defs.contains(ltype) {
                left_struct_name = ltype
            }
        }

        if len(left_struct_name) > 0 {
            if struct_has_method(g, left_struct_name, overload_method) {
                -- Call the overloaded operator method: struct_method(left, right)
                let mut op_args: List[Expr] = []
                op_args.push(*expr.right)
                if lhs_done >= 0 {
                    -- The left operand is generated already (a + b + c):
                    -- the method takes that value
                    let held = bind_hidden_value(g, b, "%operand.left", lhs_done, left_struct_name, static_type(g, left_expr))
                    let called = generate_struct_method_call(held.gen, held.block, left_struct_name, expr_ident("%operand.left", expr.line, expr.col), overload_method, op_args)
                    return expr_result(unbind_hidden_value(called.gen, "%operand.left"), called.block, called.val_id)
                }
                return generate_struct_method_call(g, b, left_struct_name, left_expr, overload_method, op_args)
            }
        }
    }

    -- --------------------------------------------------------
    -- EAGER BINARY OPERATORS (arithmetic, comparison, string)
    -- Evaluate both operands first, then emit the operation
    -- --------------------------------------------------------

    -- Evaluate left operand
    let mut lhs_id = lhs_done
    if lhs_done < 0 {
        let lhs_result = generate_expr(g, b, *expr.left)
        g = lhs_result.gen
        b = lhs_result.block
        lhs_id = lhs_result.val_id
    }

    -- Evaluate right operand
    let rhs_result = generate_expr(g, b, *expr.right)
    g = rhs_result.gen
    b = rhs_result.block
    let mut rhs_id = rhs_result.val_id

    -- The operands' types select the operation
    return emit_binary_op(g, b, expr, lhs_id, infer_expr_type(g, *expr.left), rhs_id, infer_expr_type(g, *expr.right))
}

-- The operation of the binary operator `expr` on two values that are
-- generated already, with their types. Not `and` and `or`, which do
-- not evaluate both operands, and not an overloaded operator. A
-- compound assignment (generate_compound_value) gives the value its
-- place holds as the left operand.
fn emit_binary_op(gen: IRGenerator, block: IRBasicBlock, expr: Expr, lhs_val_id: int, lhs_in_type: string, rhs_val_id: int, rhs_in_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let op = expr.op
    let mut lhs_id = lhs_val_id
    let mut rhs_id = rhs_val_id
    let mut lhs_type = lhs_in_type
    let mut rhs_type = rhs_in_type

    -- Integers of different types meet at one type (see binary_int_type)
    let common_t = common_int_type(expr, lhs_type, rhs_type)
    if common_t != "" {
        if lhs_type != common_t {
            g = check_int_literal_fits(g, *expr.left, common_t)
            let lc = emit_ir_cast(g, b, ir_val_inst(lhs_id), lhs_type, common_t)
            g = lc.gen
            b = lc.block
            lhs_id = lc.val_id
            lhs_type = common_t
        }
        if rhs_type != common_t {
            g = check_int_literal_fits(g, *expr.right, common_t)
            let rc = emit_ir_cast(g, b, ir_val_inst(rhs_id), rhs_type, common_t)
            g = rc.gen
            b = rc.block
            rhs_id = rc.val_id
            rhs_type = common_t
        }
    }
    let int_operand_t = if is_integer_type(lhs_type) and lhs_type == rhs_type { lhs_type } else { "" }

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
        if is_signed_int_type(lhs_type) and is_signed_int_type(rhs_type) {
            let mut fit_type = ""
            if lhs_type == rhs_type and type_bit_width(lhs_type) < 64 { fit_type = lhs_type }
            return emit_checked_int_arith(g, b, "dm_checked_add_i64", ir_val_inst(lhs_id), lhs_type, ir_val_inst(rhs_id), rhs_type, fit_type)
        }
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
        if is_signed_int_type(lhs_type) and is_signed_int_type(rhs_type) {
            let mut fit_type = ""
            if lhs_type == rhs_type and type_bit_width(lhs_type) < 64 { fit_type = lhs_type }
            return emit_checked_int_arith(g, b, "dm_checked_sub_i64", ir_val_inst(lhs_id), lhs_type, ir_val_inst(rhs_id), rhs_type, fit_type)
        }
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
        if is_signed_int_type(lhs_type) and is_signed_int_type(rhs_type) {
            let mut fit_type = ""
            if lhs_type == rhs_type and type_bit_width(lhs_type) < 64 { fit_type = lhs_type }
            return emit_checked_int_arith(g, b, "dm_checked_mul_i64", ir_val_inst(lhs_id), lhs_type, ir_val_inst(rhs_id), rhs_type, fit_type)
        }
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
        panic_args.push(ir_val_string("division by zero"))
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
        let div_chk = check_div_overflow(g, ok_block, lhs_id, rhs_id, lhs_type)
        g = div_chk.gen
        ok_block = div_chk.block
        let div_id = g.builder.next_id
        let mut div_inst = ir_inst_new(div_id, OP_DIV())
        div_inst.lhs = ir_val_inst(lhs_id)
        div_inst.rhs = ir_val_inst(rhs_id)
        div_inst.result_type = arith_type
        div_inst.operand_type = int_operand_t
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
        panic_args.push(ir_val_string("modulo by zero"))
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
        let mod_chk = check_div_overflow(g, ok_block, lhs_id, rhs_id, lhs_type)
        g = mod_chk.gen
        ok_block = mod_chk.block
        let mod_id = g.builder.next_id
        let mut mod_inst = ir_inst_new(mod_id, OP_MOD())
        mod_inst.lhs = ir_val_inst(lhs_id)
        mod_inst.rhs = ir_val_inst(rhs_id)
        mod_inst.result_type = arith_type
        mod_inst.operand_type = int_operand_t
        mod_inst.has_result = true
        ok_block.instructions.push(mod_inst)
        g.builder.next_id = mod_id + 1

        return expr_result(g, ok_block, mod_id)
    }

    -- --------------------------------------------------------
    -- COMPARISON OPERATORS (==, !=, <, <=, >, >=)
    -- --------------------------------------------------------

    -- Check if this is an enum comparison (need to extract tags first)
    let mut is_enum_cmp = false
    let left_expr = *expr.left
    if left_expr.kind == EXPR_IDENTIFIER() {
        if g.var_enum_types.contains(left_expr.name) {
            is_enum_cmp = true
        }
    }
    if left_expr.kind == EXPR_ENUM_LITERAL() {
        is_enum_cmp = true
    }
    -- Also check right side for enum literal like Color.Red
    let right_expr = *expr.right
    if right_expr.kind == EXPR_ENUM_LITERAL() {
        is_enum_cmp = true
    }
    if right_expr.kind == EXPR_IDENTIFIER() {
        if g.var_enum_types.contains(right_expr.name) {
            is_enum_cmp = true
        }
    }

    if is_enum_cmp and (op == BINOP_EQ() or op == BINOP_NE()) {
        -- Extract tag (field 0) from both enum values
        let lhs_tag_id = g.builder.next_id
        let mut lhs_tag = ir_inst_new(lhs_tag_id, OP_EXTRACT_FIELD())
        lhs_tag.field_base = ir_val_inst(lhs_id)
        lhs_tag.field_index = 0
        lhs_tag.field_type = "i32"
        lhs_tag.result_type = "i32"
        lhs_tag.has_result = true
        b.instructions.push(lhs_tag)
        g.builder.next_id = g.builder.next_id + 1

        let rhs_tag_id = g.builder.next_id
        let mut rhs_tag = ir_inst_new(rhs_tag_id, OP_EXTRACT_FIELD())
        rhs_tag.field_base = ir_val_inst(rhs_id)
        rhs_tag.field_index = 0
        rhs_tag.field_type = "i32"
        rhs_tag.result_type = "i32"
        rhs_tag.has_result = true
        b.instructions.push(rhs_tag)
        g.builder.next_id = g.builder.next_id + 1

        let cmp_op = if op == BINOP_EQ() { OP_EQ() } else { OP_NE() }
        let cmp_id = g.builder.next_id
        let mut cmp_inst = ir_inst_new(cmp_id, cmp_op)
        cmp_inst.lhs = ir_val_inst(lhs_tag_id)
        cmp_inst.rhs = ir_val_inst(rhs_tag_id)
        cmp_inst.result_type = "bool"
        cmp_inst.has_result = true
        b.instructions.push(cmp_inst)
        g.builder.next_id = cmp_id + 1
        return expr_result(g, b, cmp_id)
    }

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
        inst.operand_type = int_operand_t
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
        inst.operand_type = int_operand_t
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
        inst.operand_type = int_operand_t
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
        inst.operand_type = int_operand_t
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
        -- An unsigned integer shifts in zeros, a signed one its sign
        inst.operand_type = int_operand_t
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    g = ir_type_error(g, "unsupported binary operator", expr.line)
    return expr_result(g, b, lhs_id)
}

-- ============================================================
-- UNARY EXPRESSION IR GENERATION
-- Handles negation (-) and logical not (!)
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
        -- Checked negation for signed ints: -INT_MIN overflows
        if is_signed_int_type(operand_type) {
            let mut fit_type = ""
            if type_bit_width(operand_type) < 64 { fit_type = operand_type }
            return emit_checked_int_arith(g, b, "dm_checked_sub_i64", ir_val_int(0), "i64", ir_val_inst(operand_id), operand_type, fit_type)
        }
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

    if expr.op == UNOP_DEREF() {
        -- Unsafe fence: references of extern/cast provenance (including
        -- &T parameters) need an unsafe block to dereference. Box derefs
        -- stay safe (runtime null check below).
        if g.unsafe_depth == 0 and expr_is_tainted_ref(g, *expr.operand) {
            eprintln(error_at(gen.ir_mod.files, "dereference of a reference from an extern call or cast requires unsafe block", expr.line))
            exit(1)
        }
        -- Dereference Box[T]: load the struct value from the pointer
        -- Try to determine the Box inner type from tracked info
        let mut inner_type = "i64"

        -- Check if operand is a variable with known Box inner type
        let operand_expr = *expr.operand
        if operand_expr.kind == EXPR_IDENTIFIER() {
            if g.box_inner_types.contains(operand_expr.name) {
                inner_type = g.box_inner_types.get(operand_expr.name)
            }
        }

        -- Check if operand was a field access that produced a Box value
        let inst_key = "_inst_" + int_to_string(operand_id)
        if g.box_inner_types.contains(inst_key) {
            inner_type = g.box_inner_types.get(inst_key)
        }

        -- Otherwise use the operand's source type (a field, a call, ...)
        if inner_type == "i64" {
            let operand_src = static_type(g, operand_expr)
            if starts_with(operand_src, "Box[") {
                inner_type = extract_inner_type(operand_src, "Box[")
            } else {
                g = ir_type_error(g, "cannot determine the type of the box being dereferenced", expr.line)
            }
        }

        -- Map the inner type to IR type
        let load_type = map_type_name(inner_type)

        -- Null check before dereferencing the pointer
        let chk = check_box_not_null(g, b, operand_id)
        g = chk.gen
        b = chk.block

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LOAD())
        inst.load_ptr = ir_val_inst(operand_id)
        inst.load_type = load_type
        inst.result_type = load_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1

        -- If inner type is a struct, track the result for subsequent field access
        if g.struct_defs.contains(inner_type) {
            let result_var = "_deref_" + int_to_string(id)
            g.var_struct_types.insert(result_var, inner_type)
        }

        return expr_result(g, b, id)
    }

    -- Unknown unary operator, return operand unchanged
    return expr_result(g, b, operand_id)
}

-- ============================================================
-- CAST EXPRESSION IR GENERATION
-- Handles explicit type casts via `expr as Type`
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

    -- Compile-time cast legality (Stage 0 Phase 2 cast matrix)
    check_cast_legal(g, *expr.operand, from_type, expr.cast_type, expr.cast_is_ref, expr.line)

    -- If the types are the same, no cast needed
    if from_type == to_type {
        return expr_result(g, b, operand_id)
    }

    -- Narrowing integer casts get a runtime range check (Stage 0 matrix);
    -- float<->int and widening casts keep plain conversion semantics.
    if is_integer_type(from_type) and is_integer_type(to_type) and int_cast_fits(from_type, to_type) == false {
        return emit_checked_cast(g, b, operand_id, from_type, to_type)
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
-- ============================================================

fn generate_function_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get the callee name from the callee expression (should be an identifier)
    let callee_expr = *expr.callee
    let fn_name = callee_expr.name

    -- Unsafe fence: calling an `extern fn` requires an unsafe block
    check_extern_fence(g, fn_name, expr.line)

    -- Raw memory primitives (load_T, store_T, addr_of, size_of, ...)
    if is_raw_builtin(fn_name) {
        return generate_raw_builtin(g, b, fn_name, expr)
    }

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
        return generate_box_new_call(g, b, expr)
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
        return generate_some_call(g, b, expr)
    }

    -- Ok(value) -> Result with tag=0
    if fn_name == "Ok" {
        return generate_ok_call(g, b, expr)
    }

    -- Err(value) -> Result with tag=1
    if fn_name == "Err" {
        return generate_err_call(g, b, expr)
    }

    -- Map_new() takes its type from where the map goes (a typed let,
    -- an assignment, a struct field, a return); see map_ctor_as
    if fn_name == "Map_new" {
        g = ir_type_error(g, "cannot tell which Map type Map_new() creates here; give it one, e.g. let m: Map[string, int] = Map_new()", expr.line)
        return expr_result(g, b, 0)
    }

    -- byte_at(s, i): the byte at index i of s, 0 to 255
    if fn_name == "byte_at" and expr.args.len() == 2 {
        return generate_byte_at(g, b, expr.args[0], expr.args[1])
    }

    -- hash_key(k): the hash Map[K, V] uses for a string or integer key
    if fn_name == "hash_key" and expr.args.len() == 1 {
        let key_type = infer_expr_type(g, expr.args[0])
        let hash_fn = if key_type == "string" { "rt_map_hash_string" } else { "rt_map_hash_int" }
        if key_type != "string" and is_integer_type(key_type) == false {
            g = ir_type_error(g, "hash_key() takes a string or integer key, not '" + key_type + "'", expr.line)
        }
        let mut hash_args: List[Expr] = []
        hash_args.push(expr.args[0])
        return generate_expr(g, b, expr_call(expr_ident(hash_fn, expr.line, expr.col), hash_args, expr.line, expr.col))
    }

    -- len() of a number or a bool is an error
    if fn_name == "len" and expr.args.len() == 1 {
        let len_arg_type = static_type(g, expr.args[0])
        let len_arg_kind = basic_type_kind(len_arg_type)
        if len_arg_kind == "number" or len_arg_kind == "bool" {
            g = ir_type_error(g, "len() takes a string, a list, a map or an array, not " + len_arg_type, expr.line)
            return expr_result(g, b, 0)
        }
    }

    -- len(m) on a map is m.len()
    if fn_name == "len" and expr.args.len() == 1 {
        if is_map_type(g, static_type(g, expr.args[0])) {
            let no_args: List[Expr] = []
            return generate_expr(g, b, expr_method_call(expr.args[0], "len", no_args, expr.line, expr.col))
        }
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

    -- Indirect call via function-pointer-typed parameter: f: fn(int) -> int
    if g.variable_types.contains(fn_name) {
        let vtype = g.variable_types.get(fn_name)
        if starts_with(vtype, "fn(") {
            return generate_fn_ptr_param_call(g, b, fn_name, vtype, expr.args)
        }
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
        -- A thread runs a function that is named and takes nothing: the
        -- runtime gets the function's address and nothing to call it with
        if fn_name == "thread_spawn" {
            if expr.args.len() != 1 {
                g = ir_type_error(g, "thread_spawn() takes 1 argument, not " + int_to_string(expr.args.len()), expr.line)
                return expr_result(g, b, 0)
            }
            if is_thread_function(g, expr.args[0]) == false {
                g = ir_type_error(g, "thread_spawn() takes a function that takes no arguments, by its name", expr.line)
                return expr_result(g, b, 0)
            }
        }
        if fn_name == "run_command" or fn_name == "run_command_io" {
            return generate_run_command_call(g, b, expr)
        }
        return generate_builtin_call(g, b, fn_name, expr.args)
    }

    -- Check if function is defined
    if g.known_functions.contains(fn_name) == false and is_builtin_fn(fn_name) == false and g.generic_fn_decls.contains(fn_name) == false and g.user_extern_fns.contains(fn_name) == false and g.method_struct_map.contains(fn_name) == false and g.static_fns.contains(fn_name) == false {
        eprintln(error_at(gen.ir_mod.files, "undefined function '" + fn_name + "'", expr.line))
        exit(1)
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

    -- The parameters' source types, for the type-mismatch check
    let mut param_types: List[string] = []
    if g.fn_src_param_types.contains(fn_name) {
        param_types = string_split(g.fn_src_param_types.get(fn_name), "|")
    }

    -- Generate all argument expressions
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        if i < param_types.len() {
            g = check_basic_type(g, param_types[i], args[i], "argument " + int_to_string(i + 1) + " of '" + fn_name + "'")
        }
        -- Check if this arg position is a mut parameter
        let is_mut_arg = if i < mut_flags_list.len() {
            mut_flags_list[i] == "1"
        } else {
            false
        }

        let mut arg_addr = addr_fail(g, b)
        if is_mut_arg {
            -- A mut parameter takes the argument's address: a variable,
            -- a mut parameter, a field, a list element or a boxed value
            arg_addr = generate_addr(g, b, args[i])
            g = arg_addr.gen
            b = arg_addr.block
        }
        if arg_addr.ok {
            call_args.push(arg_addr.ptr)
        } else if is_mut_arg {
            -- A mut parameter takes a pointer: spill a non-variable argument
            -- (e.g. a struct literal) to a temporary and pass its address
            let arg_type = infer_expr_type(g, args[i])
            let arg_result = generate_expr(g, b, args[i])
            g = arg_result.gen
            b = arg_result.block
            let tmp_id = g.builder.next_id
            let mut tmp_inst = ir_inst_new(tmp_id, OP_ALLOCA())
            tmp_inst.alloc_type = arg_type
            tmp_inst.result_type = "ptr(" + arg_type + ")"
            tmp_inst.has_result = true
            b.instructions.push(tmp_inst)
            g.builder.next_id = tmp_id + 1
            let mut st_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            st_inst.store_ptr = ir_val_inst(tmp_id)
            st_inst.store_val = ir_val_inst(arg_result.val_id)
            b.instructions.push(st_inst)
            g.builder.next_id = g.builder.next_id + 1
            call_args.push(ir_val_inst(tmp_id))
        } else {
            let mut arg_result = generate_expr(g, b, args[i])
            if i < param_types.len() {
                -- An integer becomes the parameter's integer type
                arg_result = convert_int_to_place(arg_result.gen, arg_result.block, arg_result.val_id, args[i], param_types[i], "argument " + int_to_string(i + 1) + " of '" + fn_name + "'")
            }
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

    -- Declared later in the source: use the signature registered up front
    if gen.fn_return_types.contains(fn_name) {
        return gen.fn_return_types.get(fn_name)
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

    -- Generate explicit arguments; an integer becomes the parameter's
    -- integer type
    let mut param_types: List[string] = []
    if g.fn_src_param_types.contains(lambda_name) {
        param_types = string_split(g.fn_src_param_types.get(lambda_name), "|")
    }
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let mut arg_result = generate_expr(g, b, args[i])
        if i < param_types.len() {
            arg_result = convert_int_to_place(arg_result.gen, arg_result.block, arg_result.val_id, args[i], param_types[i], "argument " + int_to_string(i + 1) + " of '" + var_name + "'")
        }
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
-- FUNCTION POINTER PARAMETER INDIRECT CALL
-- Handles calling through function-pointer-typed parameters:
--   fn apply(f: fn(int) -> int, x: int) -> int { return f(x) }
-- ============================================================

fn generate_some_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let args = expr.args

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- The Option[T] the checker found for the place the value goes to;
    -- without it, the function's return type, or Option of the argument's type
    let mut opt_type = checked_ir_type(g, expr)
    if starts_with(opt_type, "Option[") == false {
        opt_type = g.current_return_type
    }
    if starts_with(opt_type, "Option[") == false {
        -- current_return_type isn't an Option (e.g., we're in main/void function).
        -- Infer Option type from the argument expression type.
        let arg_type = infer_expr_type(g, args[0])
        opt_type = "Option[" + arg_type + "]"
    }
    -- An integer becomes the Option's integer type
    let some_val = convert_int_to_place(g, b, val_result.val_id, args[0], extract_inner_type(opt_type, "Option["), "the value of 'Some'")
    g = some_val.gen
    b = some_val.block

    -- Build the Option struct: { has_value: 1, value: payload }
    -- Use undef as the base (not alloca), since insertvalue works on values not pointers

    -- Store has_value = 1 (field index 0)
    let insert1_id = g.builder.next_id
    let mut insert1 = ir_inst_new(insert1_id, OP_INSERT_FIELD())
    insert1.field_base = ir_val_undef()
    insert1.field_value = ir_val_int(1)
    insert1.field_index = 0
    insert1.result_type = opt_type
    insert1.has_result = true
    b.instructions.push(insert1)
    g.builder.next_id = g.builder.next_id + 1

    -- Store value = payload (field index 1)
    let insert2_id = g.builder.next_id
    let mut insert2 = ir_inst_new(insert2_id, OP_INSERT_FIELD())
    insert2.field_base = ir_val_inst(insert1_id)
    insert2.field_value = ir_val_inst(some_val.val_id)
    insert2.field_index = 1
    insert2.result_type = opt_type
    insert2.has_result = true
    b.instructions.push(insert2)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert2_id)
}

-- ============================================================
-- OK CALL: Result constructor (success)
-- Ok(value) -> Result { tag: 0, ok_val: payload, err_val: undef }
-- ============================================================

fn generate_ok_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let args = expr.args

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- The Result[T, E] the checker found for the place the value goes
    -- to; without it, the function's return type
    let mut res_type = checked_ir_type(g, expr)
    if starts_with(res_type, "Result[") == false {
        res_type = g.current_return_type
    }
    -- An integer becomes the Result's integer type
    let mut ok_val = val_result
    if starts_with(res_type, "Result[") {
        let res_args = split_type_args(extract_inner_type(res_type, "Result["))
        if res_args.len() > 0 {
            ok_val = convert_int_to_place(g, b, val_result.val_id, args[0], res_args[0], "the value of 'Ok'")
            g = ok_val.gen
            b = ok_val.block
        }
    }

    -- Build the Result struct: { tag: 0, ok_val: payload, err_val: undef }
    -- Use undef as the base (not alloca), since insertvalue works on values not pointers

    -- tag = 0 (Ok) at field index 0
    let insert_tag_id = g.builder.next_id
    let mut insert_tag = ir_inst_new(insert_tag_id, OP_INSERT_FIELD())
    insert_tag.field_base = ir_val_undef()
    insert_tag.field_value = ir_val_int(0)
    insert_tag.field_index = 0
    insert_tag.result_type = res_type
    insert_tag.has_result = true
    b.instructions.push(insert_tag)
    g.builder.next_id = g.builder.next_id + 1

    -- ok_val = payload at field index 1
    let insert_ok_id = g.builder.next_id
    let mut insert_ok = ir_inst_new(insert_ok_id, OP_INSERT_FIELD())
    insert_ok.field_base = ir_val_inst(insert_tag_id)
    insert_ok.field_value = ir_val_inst(ok_val.val_id)
    insert_ok.field_index = 1
    insert_ok.result_type = res_type
    insert_ok.has_result = true
    b.instructions.push(insert_ok)
    g.builder.next_id = g.builder.next_id + 1

    -- err_val = undef at field index 2
    let insert_err_id = g.builder.next_id
    let mut insert_err = ir_inst_new(insert_err_id, OP_INSERT_FIELD())
    insert_err.field_base = ir_val_inst(insert_ok_id)
    insert_err.field_value = ir_val_undef()
    insert_err.field_index = 2
    insert_err.result_type = res_type
    insert_err.has_result = true
    b.instructions.push(insert_err)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert_err_id)
}

-- ============================================================
-- ERR CALL: Result constructor (error)
-- Err(value) -> Result { tag: 1, ok_val: undef, err_val: payload }
-- ============================================================

fn generate_err_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let args = expr.args

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the error payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- The Result[T, E] the checker found for the place the value goes
    -- to; without it, the function's return type
    let mut res_type = checked_ir_type(g, expr)
    if starts_with(res_type, "Result[") == false {
        res_type = g.current_return_type
    }

    -- An integer becomes the Result's integer type
    let mut err_val = val_result
    if starts_with(res_type, "Result[") {
        let res_args = split_type_args(extract_inner_type(res_type, "Result["))
        if res_args.len() > 1 {
            err_val = convert_int_to_place(g, b, val_result.val_id, args[0], res_args[1], "the value of 'Err'")
            g = err_val.gen
            b = err_val.block
        }
    }

    -- Build the Result struct: { tag: 1, ok_val: undef, err_val: payload }
    -- Use undef as the base (not alloca), since insertvalue works on values not pointers

    -- tag = 1 (Err) at field index 0
    let insert_tag_id = g.builder.next_id
    let mut insert_tag = ir_inst_new(insert_tag_id, OP_INSERT_FIELD())
    insert_tag.field_base = ir_val_undef()
    insert_tag.field_value = ir_val_int(1)
    insert_tag.field_index = 0
    insert_tag.result_type = res_type
    insert_tag.has_result = true
    b.instructions.push(insert_tag)
    g.builder.next_id = g.builder.next_id + 1

    -- ok_val = undef at field index 1
    let insert_ok_id = g.builder.next_id
    let mut insert_ok = ir_inst_new(insert_ok_id, OP_INSERT_FIELD())
    insert_ok.field_base = ir_val_inst(insert_tag_id)
    insert_ok.field_value = ir_val_undef()
    insert_ok.field_index = 1
    insert_ok.result_type = res_type
    insert_ok.has_result = true
    b.instructions.push(insert_ok)
    g.builder.next_id = g.builder.next_id + 1

    -- err_val = payload at field index 2
    let insert_err_id = g.builder.next_id
    let mut insert_err = ir_inst_new(insert_err_id, OP_INSERT_FIELD())
    insert_err.field_base = ir_val_inst(insert_ok_id)
    insert_err.field_value = ir_val_inst(err_val.val_id)
    insert_err.field_index = 2
    insert_err.result_type = res_type
    insert_err.has_result = true
    b.instructions.push(insert_err)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert_err_id)
}

-- ============================================================
-- GENERIC FUNCTION CALL
-- Builds a monomorphized name, generates the specialized copy
-- if not already done, then calls it normally.
-- ============================================================

fn generate_generic_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let callee_expr = *expr.callee
    let fn_name = callee_expr.name

    -- Build monomorphized name: fn_name + "_" + type_args joined by "_"
    let mut mangled_name = fn_name
    let mut type_args = expr.generic_args

    -- Type arguments that are not written out are those the checker
    -- found from the arguments
    if type_args.len() == 0 {
        type_args = checked_type_args(g, expr)
    }

    if type_args.len() == 0 {
        -- The checker found none (the call is in the body of a generic
        -- function, or made during code generation): type parameter i
        -- takes the type of argument i
        if g.generic_fn_decls.contains(fn_name) {
            let decl_idx: int = g.generic_fn_decls.get(fn_name)
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
        mangled_name = mangled_name + "_" + mangle_type_arg(canon_type(g, type_args[ti]))
        ti = ti + 1
    }

    -- Generate the monomorphized function if not already done
    if g.mono_generated.contains(mangled_name) == false {
        g.mono_generated.insert(mangled_name, true)

        if g.generic_fn_decls.contains(fn_name) {
            let decl_idx: int = g.generic_fn_decls.get(fn_name)
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
                    let saved_completed_blocks = g.completed_blocks
                    let saved_next_id = g.builder.next_id
                    let saved_unsafe_depth = g.unsafe_depth
                    let saved_region_depth = g.region_depth
                    let saved_tainted_vars = g.tainted_vars
                    let saved_ref_vars = g.ref_vars
                    let saved_region_born_vars = g.region_born_vars
                    let saved_declared_vars = g.declared_vars
                    let saved_region_marks = g.region_marks

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
                        mp.is_ref = p.is_ref
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
                    g.completed_blocks = saved_completed_blocks
                    g.builder.next_id = saved_next_id
                    g.unsafe_depth = saved_unsafe_depth
                    g.region_depth = saved_region_depth
                    g.tainted_vars = saved_tainted_vars
                    g.ref_vars = saved_ref_vars
                    g.region_born_vars = saved_region_born_vars
                    g.declared_vars = saved_declared_vars
                    g.region_marks = saved_region_marks
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

-- Helper: join type names with "_" for mangled names

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

    -- Handle arbitrary user-defined generic types: Pair[A, B] etc.
    -- Look for Name[...] pattern where Name is not a known builtin container
    let bracket_idx = string_find(type_name, "[")
    if bracket_idx > 0 {
        let base = substr(type_name, 0, bracket_idx)
        let inner = substr(type_name, bracket_idx + 1, len(type_name) - bracket_idx - 2)
        -- Resolve each comma-separated type arg
        let comma_idx = string_find(inner, ", ")
        if comma_idx >= 0 {
            let first_type = substr(inner, 0, comma_idx)
            let second_type = substr(inner, comma_idx + 2, len(inner) - comma_idx - 2)
            let resolved_first = resolve_type_substitution(gen, first_type)
            let resolved_second = resolve_type_substitution(gen, second_type)
            return base + "_" + map_type_name(resolved_first) + "_" + map_type_name(resolved_second)
        } else {
            let resolved_inner = resolve_type_substitution(gen, inner)
            return base + "_" + map_type_name(resolved_inner)
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
-- ============================================================

-- x.f(a) as the call f(x, a), which it is where the checker found that
-- the type of x has no method f (checked_target). The call keeps the
-- id, at which the checker recorded its type and its type arguments.
fn method_as_call(gen: IRGenerator, expr: Expr) -> Expr {
    let target = checked_target(gen, expr)
    let mut call_args: List[Expr] = []
    call_args.push(*expr.object)
    let mut i = 0
    while i < expr.args.len() {
        call_args.push(expr.args[i])
        i = i + 1
    }
    let mut as_call = expr_call(expr_ident(substr(target, 5, len(target) - 5), expr.line, expr.col), call_args, expr.line, expr.col)
    as_call.id = expr.id
    return as_call
}

fn generate_method_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let obj_expr = *expr.object
    let method_name = expr.method
    let args = expr.args

    if is_map_ctor(expr) {
        g = ir_type_error(g, "cannot tell which Map type Map.new() creates here; give it one, e.g. let m: Map[string, int] = Map.new()", expr.line)
        return expr_result(g, b, 0)
    }

    -- x.f(a) where the type of x has no method f is the call f(x, a),
    -- as the checker found. The call keeps the id, at which the checker
    -- recorded its type and its type arguments.
    if starts_with(checked_target(g, expr), "call:") {
        return generate_function_call(g, b, method_as_call(g, expr))
    }

    -- Region escape: push/insert/set store their argument into the
    -- container, so a region-born argument must not land in a
    -- container declared outside the region
    if g.region_depth > 0 and (method_name == "push" or method_name == "insert" or method_name == "set") {
        let mut any_born = false
        let mut bai = 0
        while bai < args.len() {
            if expr_is_region_born(g, args[bai]) { any_born = true }
            bai = bai + 1
        }
        if any_born {
            let born_root = root_identifier_name(obj_expr)
            if born_root != "" and var_declared_outside_innermost_region(g, born_root) {
                eprintln(error_at(gen.ir_mod.files, "store of region-born value into container declared outside the region (region escape)", expr.line))
                exit(1)
            }
        }
    }

    -- --------------------------------------------------------
    -- Check if this is an enum variant constructor: EnumName.Variant(args)
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_IDENTIFIER() {
        let obj_name = obj_expr.name
        if g.enum_defs.contains(obj_name) and g.variable_map.contains(obj_name) == false {
            return generate_enum_method_construct(g, b, obj_name, method_name, args, expr.line)
        }
        if g.variable_map.contains(obj_name) == false and is_generic_enum(g, obj_name) {
            if generic_enum_has_variant(g, obj_name, method_name) {
                let instance = generic_enum_instance(g, expr, obj_name, method_name, args)
                if instance.gen.enum_defs.contains(instance.name) == false {
                    return expr_result(instance.gen, b, 0)
                }
                return generate_enum_method_construct(instance.gen, b, instance.name, method_name, args, expr.line)
            }
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
    -- List method calls (on direct variable)
    -- --------------------------------------------------------
    if len(obj_var_name) > 0 and g.list_elem_kinds.contains(obj_var_name) {
        let ek = g.list_elem_kinds.get(obj_var_name)
        let raw_list_ptr = g.variable_map.get(obj_var_name)
        -- For mut params (negative sentinel), generate expr to get the actual pointer
        if raw_list_ptr < 0 and g.param_ptr_map.contains(obj_var_name) {
            let obj_result = generate_expr(g, b, obj_expr)
            g = obj_result.gen
            b = obj_result.block
            return generate_list_method_call(g, b, ek, obj_result.val_id, obj_var_name, method_name, args)
        }
        return generate_list_method_call(g, b, ek, raw_list_ptr, obj_var_name, method_name, args)
    }

    -- --------------------------------------------------------
    -- List methods on any other list-typed receiver: a field
    -- (p.tokens.push(x)), a boxed value, a call result, ...
    -- --------------------------------------------------------
    let mut list_recv = obj_expr
    let mut recv_list_type = if g.list_elem_kinds.contains(obj_var_name) { "" } else { static_type(g, obj_expr) }
    if starts_with(recv_list_type, "Box[List[") {
        -- A list in a box: the method is the list's, as b.len() is
        -- (*b).len(), and it changes the list in the box
        list_recv = unboxed_container(g, obj_expr)
        recv_list_type = extract_inner_type(recv_list_type, "Box[")
    }
    if starts_with(recv_list_type, "List[") {
        let recv_elem = extract_inner_type(recv_list_type, "List[")
        let h = generate_list_handle(g, b, list_recv)
        g = h.gen
        b = h.block
        if h.ok == false and (method_name == "push" or method_name == "pop") {
            g = ir_type_error(g, "'." + method_name + "()' would change a temporary copy of this list; store the list in a variable or field first", expr.line)
        }
        -- Generic list methods look up the element type by name
        let recv_key = "_listrecv_" + int_to_string(h.ptr.ref_id)
        g.list_elem_types.insert(recv_key, recv_elem)
        return generate_list_method_call(g, b, classify_list_elem(recv_elem), h.ptr.ref_id, recv_key, method_name, args)
    }

    -- --------------------------------------------------------
    -- String .len() method
    -- --------------------------------------------------------
    -- s.len() on any string-typed expression is len(s), and on a boxed
    -- string it is the length of the string in the box
    let len_recv_type = if method_name == "len" and args.len() == 0 { static_type(g, obj_expr) } else { "" }
    if len_recv_type == "string" or len_recv_type == "Box[string]" {
        let mut len_args: List[Expr] = []
        if len_recv_type == "string" {
            len_args.push(obj_expr)
        } else {
            len_args.push(expr_unary(UNOP_DEREF(), obj_expr, expr.line, expr.col))
        }
        return generate_function_call(g, b, expr_call(expr_ident("len", expr.line, expr.col), len_args, expr.line, expr.col))
    }
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
    -- Methods of a generic struct instance
    -- --------------------------------------------------------
    -- A method called on a box is called on what is in the box:
    -- b.inc() is (*b).inc(), and a `mut self` method changes the value
    -- in the box
    let recv_src = static_type(g, obj_expr)
    let unboxed_recv = if starts_with(recv_src, "Box[") { expr_unary(UNOP_DEREF(), obj_expr, expr.line, expr.col) } else { obj_expr }
    let instance_recv = strip_box_type(recv_src)
    if g.generic_instances.contains(instance_recv) {
        return generate_struct_method_call(g, b, instance_recv, unboxed_recv, method_name, args)
    }
    -- A method of the receiver's own type, before the search by the
    -- method's name alone, which finds the last struct that has one
    if own_method_of(g, obj_expr, method_name) != "" {
        return generate_struct_method_call(g, b, instance_recv, unboxed_recv, method_name, args)
    }
    if starts_with(recv_src, "Box[") and g.struct_defs.contains(instance_recv) and g.generic_fn_decls.contains(instance_recv + "_" + method_name) {
        return generate_struct_method_call(g, b, instance_recv, unboxed_recv, method_name, args)
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
    -- Any other receiver: dispatch on its static source type
    -- --------------------------------------------------------
    let recv_type = static_type(g, obj_expr)
    if g.struct_defs.contains(recv_type) {
        return generate_struct_method_call(g, b, recv_type, obj_expr, method_name, args)
    }

    -- --------------------------------------------------------
    -- Fallback: a free function called with method syntax, self first.
    -- Any other method means the receiver's type is unknown.
    -- --------------------------------------------------------
    if g.known_functions.contains(method_name) == false {
        g = ir_type_error(g, "cannot determine the type of the value '." + method_name + "()' is called on", expr.line)
    }
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

fn generate_enum_method_construct(gen: IRGenerator, block: IRBasicBlock, enum_name: string, variant_name: string, args: List[Expr], line: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    let info = g.enum_defs.get(enum_name)

    -- Find the variant
    let variant_index = enum_variant_index(info, variant_name)
    if variant_index < 0 {
        g = ir_type_error(g, "enum '" + enum_name + "' has no variant or function '" + variant_name + "'", line)
        return expr_result(g, b, 0)
    }
    let tag = info.variant_tags[variant_index]
    let payload_count = enum_payload_count(info, variant_index)
    if args.len() != payload_count {
        g = ir_type_error(g, "variant '" + enum_name + "." + variant_name + "' carries " + values_text(payload_count) + ", not " + int_to_string(args.len()), line)
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

    -- The values the variant carries go in its fields
    if payload_count > 0 {
        let field_idx = enum_payload_field(info, variant_index)

        -- Insert each payload argument
        let mut ai = 0
        while ai < args.len() {
            g = check_basic_type(g, enum_payload_type(info, variant_index, ai), args[ai], "value " + int_to_string(ai + 1) + " of '" + enum_name + "." + variant_name + "'")
            let mut arg_result = generate_expr(g, b, args[ai])
            arg_result = convert_int_to_place(arg_result.gen, arg_result.block, arg_result.val_id, args[ai], enum_payload_type(info, variant_index, ai), "value " + int_to_string(ai + 1) + " of '" + enum_name + "." + variant_name + "'")
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

    let alloca_id = g.variable_map.get(obj_name)

    -- Determine if this is a direct struct param or an indirect let-binding
    -- For function params, the alloca holds the struct directly (load as struct type)
    -- For let-bindings, the alloca holds a ptr to heap data (load as ptr)
    let is_direct_struct = g.var_struct_types.contains(obj_name) and g.variable_types.contains(obj_name) and g.variable_types.get(obj_name) != "ptr"

    let mut self_val_id = 0
    if is_direct_struct {
        -- Load the struct value directly from the alloca
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(alloca_id)
        load_inst.load_type = concrete_type
        load_inst.result_type = concrete_type
        load_inst.has_result = true
        b.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1
        self_val_id = load_id
    } else {
        -- Load the data pointer from the dyn variable's alloca (indirect)
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(alloca_id)
        load_inst.load_type = "ptr"
        load_inst.result_type = "ptr"
        load_inst.has_result = true
        b.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1
        self_val_id = load_id
    }

    -- Build args: self value, then method arguments
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(self_val_id))

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

        let mut val_result = generate_expr(g, b, args[0])
        -- An integer becomes the elements' integer type
        let pushed_elem_type = if len(obj_var_name) > 0 and g.list_elem_types.contains(obj_var_name) { g.list_elem_types.get(obj_var_name) } else { "i64" }
        val_result = convert_list_elem(val_result.gen, val_result.block, val_result.val_id, args[0], ek, pushed_elem_type, "the pushed value")
        g = val_result.gen
        b = val_result.block

        if ek == LIST_ELEM_OTHER() {
            -- Generic push: alloca temp, store struct value, pass pointer + elem_size
            let elem_type_name = pushed_elem_type
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
            call_args.push(ir_val_sizeof(elem_type_name))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }

        let push_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_push" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_push" }
        else { "dm_list_string_push" }

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
        if ek == LIST_ELEM_OTHER() {
            -- Generic pop: dm_list_generic_pop(out_ptr, list_ptr, elem_size)
            -- copies the element to a temporary, which is then loaded
            let elem_type_name = if len(obj_var_name) > 0 and g.list_elem_types.contains(obj_var_name) {
                g.list_elem_types.get(obj_var_name)
            } else {
                "i64"
            }
            let temp_alloca_id = g.builder.next_id
            let mut temp_inst = ir_inst_new(temp_alloca_id, OP_ALLOCA())
            temp_inst.alloc_type = elem_type_name
            temp_inst.result_type = "ptr(" + elem_type_name + ")"
            temp_inst.has_result = true
            b.instructions.push(temp_inst)
            g.builder.next_id = g.builder.next_id + 1

            let pop_id = g.builder.next_id
            let mut pop_inst = ir_inst_new(pop_id, OP_CALL())
            pop_inst.callee = "dm_list_generic_pop"
            let mut pop_args: List[IRValue] = []
            pop_args.push(ir_val_inst(temp_alloca_id))
            pop_args.push(ir_val_inst(list_ptr))
            pop_args.push(ir_val_sizeof(elem_type_name))
            pop_inst.call_args = pop_args
            pop_inst.result_type = "void"
            pop_inst.has_result = false
            b.instructions.push(pop_inst)
            g.builder.next_id = pop_id + 1

            let load_id = g.builder.next_id
            let mut load_inst = ir_inst_new(load_id, OP_LOAD())
            load_inst.load_ptr = ir_val_inst(temp_alloca_id)
            load_inst.load_type = elem_type_name
            load_inst.result_type = elem_type_name
            load_inst.has_result = true
            b.instructions.push(load_inst)
            g.builder.next_id = load_id + 1
            if g.struct_defs.contains(elem_type_name) {
                g.var_struct_types.insert("_listget_" + int_to_string(load_id), elem_type_name)
            }
            return expr_result(g, b, load_id)
        }

        let pop_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_pop" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_pop" }
        else { "dm_list_string_pop" }

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
            else if ek == LIST_ELEM_FLOAT() { "dm_list_double_contains" }
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

-- Generate Instance_method for a generic struct instance from its
-- generic impl, with the instance's type arguments substituted.
-- Only signature-wide state survives; the caller's function state
-- is restored.
fn ensure_instance_method(gen: IRGenerator, instance: string, method_name: string) -> IRGenerator {
    let mangled = instance + "_" + method_name
    if gen.mono_generated.contains(mangled) { return gen }
    let base_mangled = gen.generic_instances.get(instance) + "_" + method_name
    if gen.generic_fn_decls.contains(base_mangled) == false { return gen }
    let decl_idx: int = gen.generic_fn_decls.get(base_mangled)
    let decl = gen.declarations[decl_idx]
    if decl.kind != DECL_FUNCTION() { return gen }
    let fd = *decl.func_decl

    let caller = gen
    let mut g = gen
    g.mono_generated.insert(mangled, true)
    g.type_substitutions = instance_subs(g, instance)
    g.current_impl_type = instance
    g.has_impl_type = true

    let mut mono_fd = fd
    let empty_gp: List[string] = []
    mono_fd.generic_params = empty_gp
    let mut mono_params: List[FuncParam] = []
    let mut pi = 0
    while pi < fd.params.len() {
        let p = fd.params[pi]
        g = ensure_type_instances(g, p.type_name, fd.line)
        let mut mp = func_param_new(p.name, canon_type(g, p.type_name))
        mp.is_mut = p.is_mut
        mp.is_ref = p.is_ref
        mono_params.push(mp)
        pi = pi + 1
    }
    mono_fd.params = mono_params
    g = ensure_type_instances(g, fd.return_type, fd.line)
    let ret_src = canon_type(g, fd.return_type)
    mono_fd.return_type = ret_src
    g.fn_src_return_types.insert(mangled, ret_src)
    g.fn_src_param_types.insert(mangled, param_src_types(mono_fd))
    if len(ret_src) > 0 {
        g.fn_return_types.insert(mangled, parse_type_to_ir(ret_src))
    } else {
        g.fn_return_types.insert(mangled, "void")
    }
    g.known_functions.insert(mangled, true)

    g.var_src_types = Map_new()
    g.var_struct_types = Map_new()
    g.list_elem_kinds = Map_new()
    g.list_elem_types = Map_new()
    g.var_enum_types = Map_new()
    g.box_inner_types = Map_new()
    g.fn_ptr_vars = Map_new()
    g.array_var_names = Map_new()
    g = declare_function(g, mono_fd, mangled)
    g = generate_function_body(g, mono_fd, mangled)

    -- Back to the caller's function, keeping what the new function added
    let mut out = caller
    out.ir_mod = g.ir_mod
    out.struct_defs = g.struct_defs
    out.mono_generated = g.mono_generated
    out.lambda_captures = g.lambda_captures
    out.mut_param_fns = g.mut_param_fns
    out.user_extern_fns = g.user_extern_fns
    out.fn_return_types = g.fn_return_types
    out.fn_src_return_types = g.fn_src_return_types
    out.fn_src_param_types = g.fn_src_param_types
    out.field_list_elem_kinds = g.field_list_elem_kinds
    out.field_list_elem_types = g.field_list_elem_types
    out.label_counter = g.label_counter
    out.lambda_counter = g.lambda_counter
    out.known_functions = g.known_functions
    out.type_errors = g.type_errors
    out.generic_instances = g.generic_instances
    out.generic_instance_args = g.generic_instance_args
    return out
}

fn generate_struct_method_call(gen: IRGenerator, block: IRBasicBlock, struct_type: string, obj_expr: Expr, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let mangled = struct_type + "_" + method_name

    -- --------------------------------------------------------
    -- GENERIC IMPL METHOD: monomorphize on demand
    -- --------------------------------------------------------
    if g.generic_fn_decls.contains(mangled) {
        let decl_idx: int = g.generic_fn_decls.get(mangled)
        if decl_idx >= 0 and decl_idx < g.declarations.len() {
            let decl = g.declarations[decl_idx]
            if decl.kind == DECL_FUNCTION() {
                let fd = *decl.func_decl

                -- Check if this method takes self (first param named "self")
                let mut has_self = false
                if fd.params.len() > 0 {
                    has_self = fd.params[0].name == "self"
                }

                -- Build the effective argument list for type inference
                -- If method takes self, include obj_expr; otherwise, just args
                let mut effective_args: List[Expr] = []
                if has_self {
                    effective_args.push(obj_expr)
                }
                let mut ai = 0
                while ai < args.len() {
                    effective_args.push(args[ai])
                    ai = ai + 1
                }

                -- Infer type arguments from effective args
                let mut type_args: List[string] = []
                let mut pi = 0
                while pi < fd.generic_params.len() {
                    let gp = fd.generic_params[pi]
                    -- Try to infer this generic param from matching param types
                    let mut inferred = "i64"
                    let mut mi = 0
                    while mi < fd.params.len() and mi < effective_args.len() {
                        if fd.params[mi].type_name == gp {
                            inferred = infer_arg_type(g, effective_args[mi])
                        }
                        mi = mi + 1
                    }
                    type_args.push(inferred)
                    pi = pi + 1
                }

                -- Build monomorphized name
                let mut mono_name = mangled
                let mut ti = 0
                while ti < type_args.len() {
                    mono_name = mono_name + "_" + map_type_name(type_args[ti])
                    ti = ti + 1
                }

                -- Monomorphize if not already done
                if g.mono_generated.contains(mono_name) == false {
                    g.mono_generated.insert(mono_name, true)

                    -- Save current state
                    let saved_variable_map = g.variable_map
                    let saved_variable_types = g.variable_types
                    let saved_list_elem_kinds = g.list_elem_kinds
                    let saved_list_elem_types = g.list_elem_types
                    let saved_return_type = g.current_return_type
                    let saved_type_subs = g.type_substitutions
                    let saved_completed_blocks = g.completed_blocks
                    let saved_next_id = g.builder.next_id
                    let saved_impl_type = g.current_impl_type
                    let saved_has_impl = g.has_impl_type
                    let saved_unsafe_depth = g.unsafe_depth
                    let saved_region_depth = g.region_depth
                    let saved_tainted_vars = g.tainted_vars
                    let saved_ref_vars = g.ref_vars
                    let saved_region_born_vars = g.region_born_vars
                    let saved_declared_vars = g.declared_vars
                    let saved_region_marks = g.region_marks

                    g.variable_map = Map_new()
                    g.variable_types = Map_new()

                    -- Set up type substitutions
                    g.type_substitutions = Map_new()
                    let mut si = 0
                    while si < fd.generic_params.len() and si < type_args.len() {
                        g.type_substitutions.insert(fd.generic_params[si], type_args[si])
                        si = si + 1
                    }

                    -- Set impl type context for Self resolution
                    g.current_impl_type = struct_type
                    g.has_impl_type = true

                    -- Create monomorphized FunctionDecl
                    let mut mono_fd = fd
                    let empty_gp: List[string] = []
                    mono_fd.generic_params = empty_gp

                    -- Resolve param types
                    let mut mono_params: List[FuncParam] = []
                    let mut mpi = 0
                    while mpi < fd.params.len() {
                        let p = fd.params[mpi]
                        let resolved = resolve_type_substitution(g, p.type_name)
                        let mut mp = func_param_new(p.name, resolved)
                        mp.is_mut = p.is_mut
                        mp.is_ref = p.is_ref
                        mono_params.push(mp)
                        mpi = mpi + 1
                    }
                    mono_fd.params = mono_params

                    -- Resolve return type
                    let resolved_ret = resolve_type_substitution(g, fd.return_type)
                    mono_fd.return_type = resolved_ret

                    -- Also register the monomorphized struct type if needed
                    -- e.g., Pair[A,B] with A=int, B=string → register Pair_i64_string struct
                    let mono_struct = struct_type + "_" + join_types(type_args)
                    if g.struct_defs.contains(struct_type) and g.struct_defs.contains(mono_struct) == false {
                        let base_info = g.struct_defs.get(struct_type)
                        let mut mono_info = struct_info_new(mono_struct)
                        let mut mono_fields: List[IRField] = []
                        let mut fi = 0
                        while fi < base_info.field_names.len() {
                            let fname = base_info.field_names[fi]
                            let ftype = resolve_type_substitution(g, base_info.field_types[fi])
                            mono_info.field_names.push(fname)
                            mono_info.field_types.push(map_type_name(ftype))
                            mono_info.field_src_types.push(ftype)
                            mono_fields.push(ir_field_new(fname, map_type_name(ftype)))
                            fi = fi + 1
                        }
                        g.struct_defs.insert(mono_struct, mono_info)
                        let mut ir_ty = ir_type_named(IR_TYPE_STRUCT(), mono_struct)
                        ir_ty.fields = mono_fields
                        g.ir_mod.struct_defs.insert(mono_struct, ir_ty)
                    }

                    -- Map the base struct name to monomorphized name for struct literals
                    g.type_substitutions.insert(struct_type, mono_struct)

                    g = declare_function(g, mono_fd, mono_name)
                    g = generate_function_body(g, mono_fd, mono_name)

                    -- Restore state
                    g.variable_map = saved_variable_map
                    g.variable_types = saved_variable_types
                    g.list_elem_kinds = saved_list_elem_kinds
                    g.list_elem_types = saved_list_elem_types
                    g.current_return_type = saved_return_type
                    g.type_substitutions = saved_type_subs
                    g.completed_blocks = saved_completed_blocks
                    g.builder.next_id = saved_next_id
                    g.current_impl_type = saved_impl_type
                    g.has_impl_type = saved_has_impl
                    g.unsafe_depth = saved_unsafe_depth
                    g.region_depth = saved_region_depth
                    g.tainted_vars = saved_tainted_vars
                    g.ref_vars = saved_ref_vars
                    g.region_born_vars = saved_region_born_vars
                    g.declared_vars = saved_declared_vars
                    g.region_marks = saved_region_marks
                }

                -- Generate the call to monomorphized function
                let mut call_args: List[IRValue] = []
                let mut ci = 0
                while ci < effective_args.len() {
                    let arg_result = generate_expr(g, b, effective_args[ci])
                    g = arg_result.gen
                    b = arg_result.block
                    call_args.push(ir_val_inst(arg_result.val_id))
                    ci = ci + 1
                }

                let ret_type = infer_call_return_type(g, mono_name)
                let has_result = ret_type != "void"

                let id = g.builder.next_id
                let mut call_inst = ir_inst_new(id, OP_CALL())
                call_inst.callee = mono_name
                call_inst.call_args = call_args
                call_inst.result_type = ret_type
                call_inst.has_result = has_result
                b.instructions.push(call_inst)
                g.builder.next_id = g.builder.next_id + 1

                return expr_result(g, b, id)
            }
        }
    }

    -- --------------------------------------------------------
    -- NON-GENERIC IMPL METHOD: direct call
    -- --------------------------------------------------------

    -- A method of a generic struct instance is generated on first use
    if g.generic_instances.contains(struct_type) {
        g = ensure_instance_method(g, struct_type, method_name)
    }

    -- Check if this method takes a mut self parameter (pass-by-pointer)
    let mut takes_mut_self = false
    if g.mut_param_fns.contains(mangled) {
        let flags = g.mut_param_fns.get(mangled)
        takes_mut_self = starts_with(flags, "1")
    }

    let mut call_args: List[IRValue] = []

    if takes_mut_self {
        -- mut self takes the receiver's address: a variable, a mut
        -- parameter, a field, a list element or a boxed value
        let recv_addr = generate_addr(g, b, obj_expr)
        g = recv_addr.gen
        b = recv_addr.block
        if recv_addr.ok {
            call_args.push(recv_addr.ptr)
        } else {
            -- A temporary receiver: its changes are lost, as with a copy
            let recv_type = infer_expr_type(g, obj_expr)
            let obj_result = generate_expr(g, b, obj_expr)
            g = obj_result.gen
            b = obj_result.block
            let tmp_id = g.builder.next_id
            let mut tmp_inst = ir_inst_new(tmp_id, OP_ALLOCA())
            tmp_inst.alloc_type = recv_type
            tmp_inst.result_type = "ptr(" + recv_type + ")"
            tmp_inst.has_result = true
            b.instructions.push(tmp_inst)
            g.builder.next_id = tmp_id + 1
            let mut st_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            st_inst.store_ptr = ir_val_inst(tmp_id)
            st_inst.store_val = ir_val_inst(obj_result.val_id)
            b.instructions.push(st_inst)
            g.builder.next_id = g.builder.next_id + 1
            call_args.push(ir_val_inst(tmp_id))
        }
    } else {
        -- Pass the object value (self by value)
        let obj_result = generate_expr(g, b, obj_expr)
        g = obj_result.gen
        b = obj_result.block
        call_args.push(ir_val_inst(obj_result.val_id))
    }

    -- The parameters' source types, self first
    let mut method_param_types: List[string] = []
    if g.fn_src_param_types.contains(mangled) {
        method_param_types = string_split(g.fn_src_param_types.get(mangled), "|")
    }

    -- Generate remaining arguments
    let mut i = 0
    while i < args.len() {
        let mut arg_result = generate_expr(g, b, args[i])
        if i + 1 < method_param_types.len() {
            -- An integer becomes the parameter's integer type
            arg_result = convert_int_to_place(arg_result.gen, arg_result.block, arg_result.val_id, args[i], method_param_types[i + 1], "argument " + int_to_string(i + 1) + " of '" + method_name + "'")
        }
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
-- RUNTIME SAFETY CHECKS (Phase 4 M2a)
-- Conditional-panic block split following the div-by-zero pattern in
-- generate_binary: terminate the current block with a conditional branch
-- to a panic block (dm_panic + unreachable) and an OK block, push both
-- finished blocks to completed_blocks, and continue in the OK block.
-- Panic messages mirror Stage 0 codegen shapes. dm_panic prints
-- "PANIC: " before the message.
-- ============================================================

-- Panic if a Box pointer value is null. Branches directly on the pointer
-- value; llvm_gen coerces a pointer condition to (ptr != null), so the
-- true edge is the non-null (OK) edge.
fn check_box_not_null(gen: IRGenerator, block: IRBasicBlock, ptr_id: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    let panic_label = gen_next_label(g, "box_null_panic")
    g = gen_advance_label(g)
    let ok_label = gen_next_label(g, "box_ok")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(ptr_id), ok_label, panic_label))
    b.has_terminator = true
    g.completed_blocks.push(b)

    let mut panic_block = ir_block_new(panic_label)
    let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    panic_inst.callee = "dm_panic"
    let mut panic_args: List[IRValue] = []
    panic_args.push(ir_val_string("null Box dereference"))
    panic_inst.call_args = panic_args
    panic_inst.result_type = "void"
    panic_inst.has_result = false
    panic_block.instructions.push(panic_inst)
    g.builder.next_id = g.builder.next_id + 1
    panic_block.terminator = Box_new(term_unreachable())
    panic_block.has_terminator = true
    g.completed_blocks.push(panic_block)

    let ok_block = ir_block_new(ok_label)
    return expr_result(g, ok_block, ptr_id)
}

-- Panic unless 0 <= idx < len. ctx is "(array)", "(string)", or "(list)".
-- The panic message is built in the panic block (idx and len dominate it):
--   PANIC: index out of bounds: index <idx>, length <len> <ctx>
fn check_index_bounds(gen: IRGenerator, block: IRBasicBlock, idx_id: int, len_val: IRValue, ctx: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- bad_neg = idx < 0
    let neg_id = g.builder.next_id
    let mut neg_inst = ir_inst_new(neg_id, OP_LT())
    neg_inst.lhs = ir_val_inst(idx_id)
    neg_inst.rhs = ir_val_int(0)
    neg_inst.result_type = "bool"
    neg_inst.has_result = true
    b.instructions.push(neg_inst)
    g.builder.next_id = neg_id + 1

    let panic_label = gen_next_label(g, "oob_panic")
    g = gen_advance_label(g)
    let mid_label = gen_next_label(g, "oob_check")
    g = gen_advance_label(g)
    let ok_label = gen_next_label(g, "oob_ok")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(neg_id), panic_label, mid_label))
    b.has_terminator = true
    g.completed_blocks.push(b)

    -- Mid block: bad_ge = idx >= len
    let mut mid_block = ir_block_new(mid_label)
    let ge_id = g.builder.next_id
    let mut ge_inst = ir_inst_new(ge_id, OP_GE())
    ge_inst.lhs = ir_val_inst(idx_id)
    ge_inst.rhs = len_val
    ge_inst.result_type = "bool"
    ge_inst.has_result = true
    mid_block.instructions.push(ge_inst)
    g.builder.next_id = ge_id + 1
    mid_block.terminator = Box_new(term_br_cond(ir_val_inst(ge_id), panic_label, ok_label))
    mid_block.has_terminator = true
    g.completed_blocks.push(mid_block)

    -- Panic block: build the message, then OP_PANIC + unreachable
    let mut panic_block = ir_block_new(panic_label)

    let idx_str_id = g.builder.next_id
    let mut idx_str_inst = ir_inst_new(idx_str_id, OP_CALL())
    idx_str_inst.callee = "dm_int_to_string"
    let mut idx_args: List[IRValue] = []
    idx_args.push(ir_val_inst(idx_id))
    idx_str_inst.call_args = idx_args
    idx_str_inst.result_type = "string"
    idx_str_inst.has_result = true
    panic_block.instructions.push(idx_str_inst)
    g.builder.next_id = idx_str_id + 1

    let len_str_id = g.builder.next_id
    let mut len_str_inst = ir_inst_new(len_str_id, OP_CALL())
    len_str_inst.callee = "dm_int_to_string"
    let mut len_args: List[IRValue] = []
    len_args.push(len_val)
    len_str_inst.call_args = len_args
    len_str_inst.result_type = "string"
    len_str_inst.has_result = true
    panic_block.instructions.push(len_str_inst)
    g.builder.next_id = len_str_id + 1

    let c1_id = g.builder.next_id
    let mut c1 = ir_inst_new(c1_id, OP_STRING_CONCAT())
    c1.lhs = ir_val_string("index out of bounds: index ")
    c1.rhs = ir_val_inst(idx_str_id)
    c1.result_type = "string"
    c1.has_result = true
    panic_block.instructions.push(c1)
    g.builder.next_id = c1_id + 1

    let c2_id = g.builder.next_id
    let mut c2 = ir_inst_new(c2_id, OP_STRING_CONCAT())
    c2.lhs = ir_val_inst(c1_id)
    c2.rhs = ir_val_string(", length ")
    c2.result_type = "string"
    c2.has_result = true
    panic_block.instructions.push(c2)
    g.builder.next_id = c2_id + 1

    let c3_id = g.builder.next_id
    let mut c3 = ir_inst_new(c3_id, OP_STRING_CONCAT())
    c3.lhs = ir_val_inst(c2_id)
    c3.rhs = ir_val_inst(len_str_id)
    c3.result_type = "string"
    c3.has_result = true
    panic_block.instructions.push(c3)
    g.builder.next_id = c3_id + 1

    let c4_id = g.builder.next_id
    let mut c4 = ir_inst_new(c4_id, OP_STRING_CONCAT())
    c4.lhs = ir_val_inst(c3_id)
    c4.rhs = ir_val_string(" " + ctx)
    c4.result_type = "string"
    c4.has_result = true
    panic_block.instructions.push(c4)
    g.builder.next_id = c4_id + 1

    -- OP_CALL (not OP_PANIC): OP_PANIC lowering emits its own unreachable,
    -- which would duplicate this block's terminator.
    let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    panic_inst.callee = "dm_panic"
    let mut oob_panic_args: List[IRValue] = []
    oob_panic_args.push(ir_val_inst(c4_id))
    panic_inst.call_args = oob_panic_args
    panic_inst.result_type = "void"
    panic_inst.has_result = false
    panic_block.instructions.push(panic_inst)
    g.builder.next_id = g.builder.next_id + 1
    panic_block.terminator = Box_new(term_unreachable())
    panic_block.has_terminator = true
    g.completed_blocks.push(panic_block)

    let ok_block = ir_block_new(ok_label)
    return expr_result(g, ok_block, idx_id)
}

-- Panic on INT_MIN / -1 (signed division overflow) before dividing.
-- No-op for non-signed-int operands. Returns the block to divide in.
fn check_div_overflow(gen: IRGenerator, block: IRBasicBlock, lhs_id: int, rhs_id: int, lhs_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    if is_signed_int_type(lhs_type) == false {
        return expr_result(g, b, lhs_id)
    }

    let min_id = g.builder.next_id
    let mut min_inst = ir_inst_new(min_id, OP_EQ())
    min_inst.lhs = ir_val_inst(lhs_id)
    min_inst.rhs = ir_val_int(int_fit_min(lhs_type))
    min_inst.result_type = "bool"
    min_inst.has_result = true
    b.instructions.push(min_inst)
    g.builder.next_id = min_id + 1

    let neg1_id = g.builder.next_id
    let mut neg1_inst = ir_inst_new(neg1_id, OP_EQ())
    neg1_inst.lhs = ir_val_inst(rhs_id)
    neg1_inst.rhs = ir_val_int(-1)
    neg1_inst.result_type = "bool"
    neg1_inst.has_result = true
    b.instructions.push(neg1_inst)
    g.builder.next_id = neg1_id + 1

    let both_id = g.builder.next_id
    let mut both_inst = ir_inst_new(both_id, OP_LOGICAL_AND())
    both_inst.lhs = ir_val_inst(min_id)
    both_inst.rhs = ir_val_inst(neg1_id)
    both_inst.result_type = "bool"
    both_inst.has_result = true
    b.instructions.push(both_inst)
    g.builder.next_id = both_id + 1

    let panic_label = gen_next_label(g, "div_ovf_panic")
    g = gen_advance_label(g)
    let do_label = gen_next_label(g, "div_do")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(both_id), panic_label, do_label))
    b.has_terminator = true
    g.completed_blocks.push(b)

    let mut panic_block = ir_block_new(panic_label)
    let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    panic_inst.callee = "dm_panic"
    let mut panic_args: List[IRValue] = []
    panic_args.push(ir_val_string("integer overflow"))
    panic_inst.call_args = panic_args
    panic_inst.result_type = "void"
    panic_inst.has_result = false
    panic_block.instructions.push(panic_inst)
    g.builder.next_id = g.builder.next_id + 1
    panic_block.terminator = Box_new(term_unreachable())
    panic_block.has_terminator = true
    g.completed_blocks.push(panic_block)

    let do_block = ir_block_new(do_label)
    return expr_result(g, do_block, lhs_id)
}

-- True for signed integer IR types (i8/i16/i32/i64)
fn is_signed_int_type(name: string) -> bool {
    return is_integer_type(name) and is_unsigned_type(name) == false
}

-- Range bounds for the narrow signed widths (fit targets)
fn int_fit_min(name: string) -> int {
    if name == "i8" { return -128 }
    if name == "i16" { return -32768 }
    if name == "i32" { return -2147483648 }
    return 0 - 9223372036854775807 - 1
}

-- An integer constant: a literal, or literals combined with unary minus,
-- parentheses, +, - and *
fn is_int_literal_expr(e: Expr) -> bool {
    if e.kind == EXPR_LITERAL_INT() { return true }
    if e.kind == EXPR_GROUPED() { return is_int_literal_expr(*e.operand) }
    if e.kind == EXPR_UNARY() and e.op == UNOP_NEG() { return is_int_literal_expr(*e.operand) }
    if e.kind == EXPR_BINARY() and (e.op == BINOP_ADD() or e.op == BINOP_SUB() or e.op == BINOP_MUL()) {
        return is_int_literal_expr(*e.left) and is_int_literal_expr(*e.right)
    }
    return false
}

-- Whether the statements of a branch give an integer constant as
-- their value
fn stmts_value_is_int_literal(stmts: List[Stmt]) -> bool {
    if stmts.len() == 0 { return false }
    let last = stmts[stmts.len() - 1]
    if last.kind != STMT_EXPRESSION() { return false }
    return is_int_literal_expr(branch_value_expr(*last.expr))
}

fn arm_value_is_int_literal(arm: MatchArm) -> bool {
    if arm.is_expr_body { return is_int_literal_expr(branch_value_expr(*arm.body_expr)) }
    return stmts_value_is_int_literal(arm.body)
}

-- The type the arms of a match give together, as far as it can be
-- told before the arms' own variables are declared, where the first
-- arm gives `first`: integers of different widths meet at one type
-- (int_meet_type); for any other type it is the first arm's
fn arms_meet_type(gen: IRGenerator, arms: List[MatchArm], first: string) -> string {
    if is_integer_type(first) == false or arms.len() == 0 { return first }
    let mut t = first
    let mut t_literal = arm_value_is_int_literal(arms[0])
    let mut i = 1
    while i < arms.len() {
        let arm = arms[i]
        let mut arm_type = ""
        if arm.is_expr_body {
            arm_type = infer_expr_type(gen, *arm.body_expr)
        } else if arm.body.len() > 0 {
            let last = arm.body[arm.body.len() - 1]
            if last.kind == STMT_EXPRESSION() { arm_type = infer_expr_type(gen, *last.expr) }
        }
        if is_integer_type(arm_type) {
            let arm_literal = arm_value_is_int_literal(arm)
            t = int_meet_type(t, t_literal, arm_type, arm_literal)
            t_literal = t_literal and arm_literal
        }
        i = i + 1
    }
    return t
}

-- What a branch of an `if` or an arm of a `match` gives: 1 for an
-- integer constant, itself or through an `if` or a `match` whose
-- branches all give one; 0 for no value, as it returns or panics; -1
-- for any other value
fn branch_int_literal_state(e: Expr) -> int {
    let v = branch_value_expr(e)
    if is_int_literal_expr(v) { return 1 }
    -- A block that ends in a statement
    if v.kind == EXPR_BLOCK() { return 0 }
    if v.kind == EXPR_FUNCTION_CALL() {
        let callee = *v.callee
        if callee.kind == EXPR_IDENTIFIER() and (callee.name == "panic" or callee.name == "exit") { return 0 }
    }
    if branches_give_int_literals(v) { return 1 }
    return -1
}

-- Whether the expression is an `if` or a `match` whose every branch
-- that gives a value gives an integer constant: its type is that of
-- the place it goes to, as a literal's is
fn branches_give_int_literals(e: Expr) -> bool {
    let v = branch_value_expr(e)
    let mut states: List[int] = []
    if v.kind == EXPR_IF() {
        if v.has_else == false { return false }
        states.push(branch_int_literal_state(*v.then_branch))
        states.push(branch_int_literal_state(*v.else_branch))
    } else if v.kind == EXPR_MATCH() {
        let mut ai = 0
        while ai < v.match_arms.len() {
            let arm = v.match_arms[ai]
            if arm.is_expr_body {
                states.push(branch_int_literal_state(*arm.body_expr))
            } else if arm.body.len() == 0 {
                states.push(0)
            } else {
                let last = arm.body[arm.body.len() - 1]
                if last.kind == STMT_EXPRESSION() {
                    states.push(branch_int_literal_state(*last.expr))
                } else {
                    states.push(0)
                }
            }
            ai = ai + 1
        }
    } else {
        return false
    }
    let mut any = false
    let mut si = 0
    while si < states.len() {
        if states[si] < 0 { return false }
        if states[si] == 1 { any = true }
        si = si + 1
    }
    return any
}

fn int_literal_value(e: Expr) -> int {
    if e.kind == EXPR_GROUPED() { return int_literal_value(*e.operand) }
    if e.kind == EXPR_UNARY() { return 0 - int_literal_value(*e.operand) }
    if e.kind == EXPR_BINARY() {
        let l = int_literal_value(*e.left)
        let r = int_literal_value(*e.right)
        if e.op == BINOP_ADD() { return l + r }
        if e.op == BINOP_SUB() { return l - r }
        return l * r
    }
    return e.int_val
}

-- Convert an integer value of type `from` to type `to` where it is
-- stored, passed or returned as `to`: constants that fit and widening
-- convert; narrowing anything else is an error that asks for `as`.
-- `from` is the value's type as it is found here, which is "i64" where
-- nothing is known, as for the value of a `?`. So a narrowing is an
-- error only where the checker, when it has a type for the value, also
-- finds one that does not fit. An `if` or a `match` whose branches all
-- give constants is no narrowing: each constant is checked where its
-- branch stores it.
fn convert_int_value(gen: IRGenerator, block: IRBasicBlock, val_id: int, e: Expr, from: string, to: string, what: string) -> ExprResult {
    let mut g = gen
    if is_integer_type(from) == false or is_integer_type(to) == false or from == to {
        return expr_result(g, block, val_id)
    }
    let mut value = branch_value_expr(e)
    -- `comptime 3` is the constant 3
    if value.kind == EXPR_COMPTIME() { value = branch_value_expr(*value.operand) }
    if is_int_literal_expr(value) {
        g = check_int_literal_fits(g, value, to)
    } else if type_bit_width(from) > type_bit_width(to) and branches_give_int_literals(value) == false {
        let checked = map_type_name(checked_type(g, e))
        if is_integer_type(checked) == false or type_bit_width(checked) > type_bit_width(to) {
            g = ir_type_error(g, what + " has type " + builtin_src_type(from) + ", which does not fit in " + to + "; convert it with `as " + to + "`", e.line)
        }
    }
    return emit_ir_cast(g, block, ir_val_inst(val_id), from, to)
}

-- convert_int_value for a value that goes to a place of the source
-- type `to`: a parameter, a field, a variant's value. Any place that
-- is not an integer's leaves the value as it is.
fn convert_int_to_place(gen: IRGenerator, block: IRBasicBlock, val_id: int, e: Expr, to: string, what: string) -> ExprResult {
    let to_ir = map_type_name(canon_type(gen, to))
    if is_integer_type(to_ir) == false { return expr_result(gen, block, val_id) }
    return convert_int_value(gen, block, val_id, e, infer_expr_type(gen, e), to_ir, what)
}

-- convert_int_value for an element that a list of integers stores:
-- a List[int] holds 64-bit elements, and a list of another integer
-- type holds elements of that type
fn convert_list_elem(gen: IRGenerator, block: IRBasicBlock, val_id: int, e: Expr, elem_kind: int, elem_type: string, what: string) -> ExprResult {
    if elem_kind == LIST_ELEM_INT() { return convert_int_to_place(gen, block, val_id, e, "i64", what) }
    if elem_kind == LIST_ELEM_OTHER() { return convert_int_to_place(gen, block, val_id, e, elem_type, what) }
    return expr_result(gen, block, val_id)
}

-- The integer type both operands of a binary operator take when they
-- are integers of different types: an integer literal takes the other
-- operand's type, as in `fd < 0` with fd: i32; otherwise the narrower
-- operand widens. "" when the operands need no conversion.
fn binary_int_type(gen: IRGenerator, expr: Expr) -> string {
    return common_int_type(expr, infer_expr_type(gen, *expr.left), infer_expr_type(gen, *expr.right))
}

-- Whether a binary operator gives a bool whatever its operands are
fn is_bool_binop(op: int) -> bool {
    if op == BINOP_EQ() or op == BINOP_NE() { return true }
    if op == BINOP_LT() or op == BINOP_LE() or op == BINOP_GT() or op == BINOP_GE() { return true }
    return op == BINOP_AND() or op == BINOP_OR()
}

-- binary_int_type for operands whose types, lt and rt, are known
fn common_int_type(expr: Expr, lt: string, rt: string) -> string {
    if is_integer_type(lt) == false or is_integer_type(rt) == false or lt == rt { return "" }
    return int_meet_type(lt, is_int_literal_expr(*expr.left), rt, is_int_literal_expr(*expr.right))
}

-- Report an integer literal that does not fit the type it converts to
fn check_int_literal_fits(gen: IRGenerator, e: Expr, t: string) -> IRGenerator {
    if is_int_literal_expr(e) == false or type_bit_width(t) >= 64 { return gen }
    let v = int_literal_value(e)
    let mut lo = 0
    let mut hi = 0
    if is_unsigned_type(t) {
        hi = if t == "u8" { 255 } else if t == "u16" { 65535 } else { 4294967295 }
    } else {
        lo = int_fit_min(t)
        hi = int_fit_max(t)
    }
    if v < lo or v > hi {
        return ir_type_error(gen, "integer literal " + int_to_string(v) + " does not fit in " + t, e.line)
    }
    return gen
}

fn int_fit_max(name: string) -> int {
    if name == "i8" { return 127 }
    if name == "i16" { return 32767 }
    if name == "i32" { return 2147483647 }
    return 9223372036854775807
}

-- Checked signed arithmetic, mirroring Stage 0: widen both operands to
-- i64, call the dm_checked_*_i64 runtime helper (panics on overflow),
-- and range-fit the result when fit_type names a narrower signed width.
fn emit_checked_int_arith(gen: IRGenerator, block: IRBasicBlock, callee: string, lhs_val: IRValue, lhs_type: string, rhs_val: IRValue, rhs_type: string, fit_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    let mut lhs64 = lhs_val
    if is_integer_type(lhs_type) and type_bit_width(lhs_type) < 64 {
        let widen_id = g.builder.next_id
        let mut widen_inst = ir_inst_new(widen_id, OP_CAST())
        widen_inst.cast_val = lhs_val
        widen_inst.cast_from = lhs_type
        widen_inst.cast_to = "i64"
        widen_inst.result_type = "i64"
        widen_inst.has_result = true
        b.instructions.push(widen_inst)
        g.builder.next_id = widen_id + 1
        lhs64 = ir_val_inst(widen_id)
    }

    let mut rhs64 = rhs_val
    if is_integer_type(rhs_type) and type_bit_width(rhs_type) < 64 {
        let widen_id = g.builder.next_id
        let mut widen_inst = ir_inst_new(widen_id, OP_CAST())
        widen_inst.cast_val = rhs_val
        widen_inst.cast_from = rhs_type
        widen_inst.cast_to = "i64"
        widen_inst.result_type = "i64"
        widen_inst.has_result = true
        b.instructions.push(widen_inst)
        g.builder.next_id = widen_id + 1
        rhs64 = ir_val_inst(widen_id)
    }

    let call_id = g.builder.next_id
    let mut call_inst = ir_inst_new(call_id, OP_CALL())
    call_inst.callee = callee
    let mut call_args: List[IRValue] = []
    call_args.push(lhs64)
    call_args.push(rhs64)
    call_inst.call_args = call_args
    call_inst.result_type = "i64"
    call_inst.has_result = true
    b.instructions.push(call_inst)
    g.builder.next_id = call_id + 1

    if len(fit_type) == 0 {
        return expr_result(g, b, call_id)
    }

    -- Range-fit to the narrower signed width, then truncate back
    let range_id = g.builder.next_id
    let mut range_inst = ir_inst_new(range_id, OP_CALL())
    range_inst.callee = "dm_checked_range_i64"
    let mut range_args: List[IRValue] = []
    range_args.push(ir_val_inst(call_id))
    range_args.push(ir_val_int(int_fit_min(fit_type)))
    range_args.push(ir_val_int(int_fit_max(fit_type)))
    range_inst.call_args = range_args
    range_inst.result_type = "i64"
    range_inst.has_result = true
    b.instructions.push(range_inst)
    g.builder.next_id = range_id + 1

    let trunc_id = g.builder.next_id
    let mut trunc_inst = ir_inst_new(trunc_id, OP_CAST())
    trunc_inst.cast_val = ir_val_inst(range_id)
    trunc_inst.cast_from = "i64"
    trunc_inst.cast_to = fit_type
    trunc_inst.result_type = fit_type
    trunc_inst.has_result = true
    b.instructions.push(trunc_inst)
    g.builder.next_id = trunc_id + 1
    return expr_result(g, b, trunc_id)
}

-- Stage 0 cast-fit rule: same signedness needs dst width >= src width;
-- unsigned -> signed needs dst width > src width; signed -> unsigned
-- never fits (negative values).
fn int_cast_fits(from_type: string, to_type: string) -> bool {
    let from_signed = is_unsigned_type(from_type) == false
    let to_signed = is_unsigned_type(to_type) == false
    if from_signed == to_signed { return type_bit_width(to_type) >= type_bit_width(from_type) }
    if from_signed == false and to_signed { return type_bit_width(to_type) > type_bit_width(from_type) }
    return false
}

-- Max value of the unsigned widths (u64-as-target uses INT64_MAX, as in
-- Stage 0's signed -> unsigned rule)
fn uint_fit_max(name: string) -> int {
    if name == "u8" { return 255 }
    if name == "u16" { return 65535 }
    if name == "u32" { return 4294967295 }
    return 9223372036854775807
}

-- Narrowing integer cast with a runtime range check, mirroring Stage 0:
-- widen to 64 bits, dm_cast_check_* (panics out of range), then convert.
fn emit_checked_cast(gen: IRGenerator, block: IRBasicBlock, operand_id: int, from_type: string, to_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let from_signed = is_unsigned_type(from_type) == false
    let to_signed = is_unsigned_type(to_type) == false

    let mut wide_id = operand_id
    if type_bit_width(from_type) < 64 {
        let wide_to = if from_signed { "i64" } else { "u64" }
        let wid = g.builder.next_id
        let mut winst = ir_inst_new(wid, OP_CAST())
        winst.cast_val = ir_val_inst(operand_id)
        winst.cast_from = from_type
        winst.cast_to = wide_to
        winst.result_type = wide_to
        winst.has_result = true
        b.instructions.push(winst)
        g.builder.next_id = wid + 1
        wide_id = wid
    }

    let chk_id = g.builder.next_id
    let mut chk_inst = ir_inst_new(chk_id, OP_CALL())
    let mut chk_args: List[IRValue] = []
    chk_args.push(ir_val_inst(wide_id))
    if from_signed and to_signed {
        chk_inst.callee = "dm_cast_check_i64"
        chk_args.push(ir_val_int(int_fit_min(to_type)))
        chk_args.push(ir_val_int(int_fit_max(to_type)))
        chk_inst.result_type = "i64"
    } else if from_signed {
        chk_inst.callee = "dm_cast_check_i64"
        chk_args.push(ir_val_int(0))
        chk_args.push(ir_val_int(uint_fit_max(to_type)))
        chk_inst.result_type = "i64"
    } else if to_signed {
        chk_inst.callee = "dm_cast_check_u64"
        chk_args.push(ir_val_int(int_fit_max(to_type)))
        chk_inst.result_type = "u64"
    } else {
        chk_inst.callee = "dm_cast_check_u64"
        chk_args.push(ir_val_int(uint_fit_max(to_type)))
        chk_inst.result_type = "u64"
    }
    chk_inst.call_args = chk_args
    chk_inst.has_result = true
    b.instructions.push(chk_inst)
    g.builder.next_id = chk_id + 1
    let chk_ret_type = chk_inst.result_type

    let cast_id = g.builder.next_id
    let mut cast_inst = ir_inst_new(cast_id, OP_CAST())
    cast_inst.cast_val = ir_val_inst(chk_id)
    cast_inst.cast_from = chk_ret_type
    cast_inst.cast_to = to_type
    cast_inst.result_type = to_type
    cast_inst.has_result = true
    b.instructions.push(cast_inst)
    g.builder.next_id = cast_id + 1
    return expr_result(g, b, cast_id)
}

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

    -- Check for list index result struct type (e.g., points[0].x)
    if struct_name == "" {
        let listget_key = "_listget_" + int_to_string(obj_id)
        if g.var_struct_types.contains(listget_key) {
            struct_name = g.var_struct_types.get(listget_key)
        }
    }

    -- Check for chained field access struct type (e.g., a.b.x)
    if struct_name == "" {
        let field_key = "_field_" + int_to_string(obj_id)
        if g.var_struct_types.contains(field_key) {
            struct_name = g.var_struct_types.get(field_key)
        }
    }

    -- Check for Box[T] field access: dereference pointer then access struct field
    if struct_name == "" and object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name
        if g.box_inner_types.contains(var_name) {
            let inner_type = g.box_inner_types.get(var_name)
            if g.struct_defs.contains(inner_type) {
                let info = g.struct_defs.get(inner_type)
                -- Null check before dereferencing the box pointer
                let chk = check_box_not_null(g, b, obj_id)
                g = chk.gen
                b = chk.block
                -- Load the struct from the box pointer
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(obj_id)
                load_inst.load_type = inner_type
                load_inst.result_type = inner_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Find the field index by name
                let mut fidx = 0
                let mut field_type_s = "i64"
                let mut ffi = 0
                while ffi < info.field_names.len() {
                    if info.field_names[ffi] == field_name {
                        fidx = ffi
                        field_type_s = info.field_types[ffi]
                    }
                    ffi = ffi + 1
                }
                let ir_type = map_type_name(field_type_s)

                -- Extract field from loaded struct
                let extract_id = g.builder.next_id
                let mut extract_inst = ir_inst_new(extract_id, OP_EXTRACT_FIELD())
                extract_inst.field_base = ir_val_inst(load_id)
                extract_inst.field_index = fidx
                extract_inst.field_type = ir_type
                extract_inst.result_type = ir_type
                extract_inst.has_result = true
                b.instructions.push(extract_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Track struct result for chained access
                if g.struct_defs.contains(field_type_s) {
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.var_struct_types.insert(result_var, field_type_s)
                }
                -- Track Box[T] result for chained box access
                if starts_with(field_type_s, "Box[") {
                    let box_inner = extract_inner_type(field_type_s, "Box[")
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.box_inner_types.insert(result_var, box_inner)
                }

                return expr_result(g, b, extract_id)
            }
        }
    }

    -- Check for Box field access from chained field access (non-identifier objects)
    -- e.g., o.inner.value where inner: Box[Inner] — obj_id is the result of extracting inner
    if struct_name == "" {
        let box_key = "_field_" + int_to_string(obj_id)
        if g.box_inner_types.contains(box_key) {
            let inner_type = g.box_inner_types.get(box_key)
            if g.struct_defs.contains(inner_type) {
                let info = g.struct_defs.get(inner_type)
                -- Null check before dereferencing the box pointer
                let chk = check_box_not_null(g, b, obj_id)
                g = chk.gen
                b = chk.block
                -- Load the struct from the box pointer
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(obj_id)
                load_inst.load_type = inner_type
                load_inst.result_type = inner_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1

                let mut fidx = 0
                let mut field_type_s = "i64"
                let mut ffi = 0
                while ffi < info.field_names.len() {
                    if info.field_names[ffi] == field_name {
                        fidx = ffi
                        field_type_s = info.field_types[ffi]
                    }
                    ffi = ffi + 1
                }
                let ir_type = map_type_name(field_type_s)

                let extract_id = g.builder.next_id
                let mut extract_inst = ir_inst_new(extract_id, OP_EXTRACT_FIELD())
                extract_inst.field_base = ir_val_inst(load_id)
                extract_inst.field_index = fidx
                extract_inst.field_type = ir_type
                extract_inst.result_type = ir_type
                extract_inst.has_result = true
                b.instructions.push(extract_inst)
                g.builder.next_id = g.builder.next_id + 1

                if g.struct_defs.contains(field_type_s) {
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.var_struct_types.insert(result_var, field_type_s)
                }
                if starts_with(field_type_s, "Box[") {
                    let box_inner = extract_inner_type(field_type_s, "Box[")
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.box_inner_types.insert(result_var, box_inner)
                }

                return expr_result(g, b, extract_id)
            }
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

    -- Check for Option/Result types before falling back
    if struct_name == "" and object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name
        if g.variable_types.contains(var_name) {
            let vtype = g.variable_types.get(var_name)
            if starts_with(vtype, "Option[") {
                -- Option[T] = { i32 has_value, T value }
                let inner = substr(vtype, 7, len(vtype) - 8)
                let inner_ir = map_type_name(inner)
                let mut fidx = 0
                let mut ftype = "i32"
                if field_name == "value" {
                    fidx = 1
                    ftype = inner_ir
                }
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
                inst.field_base = ir_val_inst(obj_id)
                inst.field_index = fidx
                inst.field_type = ftype
                inst.result_type = ftype
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = id + 1
                return expr_result(g, b, id)
            }
            if starts_with(vtype, "Result[") {
                -- Result[T, E] = { i32 tag, T ok_val, E err_val }
                let inner_str = substr(vtype, 7, len(vtype) - 8)
                -- Parse "T, E" from inner_str
                let mut comma_pos = 0
                let mut ci = 0
                while ci < len(inner_str) {
                    if char_at(inner_str, ci) == "," {
                        comma_pos = ci
                    }
                    ci = ci + 1
                }
                let ok_type = string_trim(substr(inner_str, 0, comma_pos))
                let err_type = string_trim(substr(inner_str, comma_pos + 1, len(inner_str) - comma_pos - 1))
                let ok_ir = map_type_name(ok_type)
                let err_ir = map_type_name(err_type)
                if field_name == "is_ok" {
                    -- Extract tag (field 0) and compare with 0 to get boolean
                    -- is_ok should be true when tag == 0 (Ok), false when tag == 1 (Err)
                    let tag_id = g.builder.next_id
                    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
                    tag_inst.field_base = ir_val_inst(obj_id)
                    tag_inst.field_index = 0
                    tag_inst.field_type = "i32"
                    tag_inst.result_type = "i32"
                    tag_inst.has_result = true
                    b.instructions.push(tag_inst)
                    g.builder.next_id = g.builder.next_id + 1

                    let cmp_id = g.builder.next_id
                    let mut cmp_inst = ir_inst_new(cmp_id, OP_EQ())
                    cmp_inst.lhs = ir_val_inst(tag_id)
                    cmp_inst.rhs = ir_val_int(0)
                    cmp_inst.result_type = "bool"
                    cmp_inst.has_result = true
                    b.instructions.push(cmp_inst)
                    g.builder.next_id = g.builder.next_id + 1

                    return expr_result(g, b, cmp_id)
                }
                let mut fidx = 0
                let mut ftype = "i32"
                if field_name == "ok" or field_name == "value" {
                    fidx = 1
                    ftype = ok_ir
                }
                if field_name == "err" or field_name == "error" {
                    fidx = 2
                    ftype = err_ir
                }
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
                inst.field_base = ir_val_inst(obj_id)
                inst.field_index = fidx
                inst.field_type = ftype
                inst.result_type = ftype
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = id + 1
                return expr_result(g, b, id)
            }
        }
    }

    if struct_name == "" {
        let obj_static = static_type(g, object)
        if g.struct_defs.contains(obj_static) {
            struct_name = obj_static
        }
    }

    -- Look up struct definition to find the field index and type
    if struct_name == "" {
        -- Cannot resolve struct type: report it, then extract index 0 to continue
        g = ir_type_error(g, "cannot determine the struct type for field access '." + expr.field + "'", expr.line)
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

    -- If the field type is Box[T], track for chained box access
    if starts_with(field_type_str, "Box[") {
        let box_inner = extract_inner_type(field_type_str, "Box[")
        let result_var = "_field_" + int_to_string(id)
        g.box_inner_types.insert(result_var, box_inner)
    }

    return expr_result(g, b, id)
}

-- ============================================================
-- Index Access: expr.object[expr.index]
-- ============================================================

-- Read element idx_id of the list at list_id (a pointer to the list),
-- with a bounds check that panics with bounds_ctx in its message.
fn generate_list_index_get(gen: IRGenerator, block: IRBasicBlock, elem_kind: int, elem_type: string, obj_id: int, idx_id: int, bounds_ctx: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    -- Bounds check: panic unless 0 <= idx < len
    let bounds_len_fn = if elem_kind == LIST_ELEM_INT() { "dm_list_int64_len" }
    else if elem_kind == LIST_ELEM_FLOAT() { "dm_list_double_len" }
    else if elem_kind == LIST_ELEM_STRING() { "dm_list_string_len" }
    else { "dm_list_generic_len" }
    let bounds_len_id = g.builder.next_id
    let mut bounds_len_inst = ir_inst_new(bounds_len_id, OP_CALL())
    bounds_len_inst.callee = bounds_len_fn
    let mut bounds_len_args: List[IRValue] = []
    bounds_len_args.push(ir_val_inst(obj_id))
    bounds_len_inst.call_args = bounds_len_args
    bounds_len_inst.result_type = "i64"
    bounds_len_inst.has_result = true
    b.instructions.push(bounds_len_inst)
    g.builder.next_id = bounds_len_id + 1
    let bounds_chk = check_index_bounds(g, b, idx_id, ir_val_inst(bounds_len_id), bounds_ctx)
    g = bounds_chk.gen
    b = bounds_chk.block

    -- The index is in range: read the element where it lies
    let slot_type = list_slot_ir_type(elem_kind, elem_type)
    let elem_addr = emit_list_elem_addr(g, b, ir_val_inst(obj_id), idx_id, slot_type)
    g = elem_addr.gen
    b = elem_addr.block
    let mut load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(elem_addr.val_id)
    load_inst.load_type = slot_type
    load_inst.result_type = slot_type
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = load_id + 1

    if ir_load_gives_address(slot_type) {
        -- A string or list element: copy it out, so the value read
        -- does not change when the element is assigned later
        let copy_id = g.builder.next_id
        let mut copy_inst = ir_inst_new(copy_id, OP_ALLOCA())
        copy_inst.alloc_type = slot_type
        copy_inst.result_type = "ptr(" + slot_type + ")"
        copy_inst.has_result = true
        b.instructions.push(copy_inst)
        g.builder.next_id = copy_id + 1
        let copied = emit_ir_store(g, b, ir_val_inst(copy_id), ir_val_inst(load_id))
        g = copied.gen
        b = copied.block
        load_id = g.builder.next_id
        let mut reload_inst = ir_inst_new(load_id, OP_LOAD())
        reload_inst.load_ptr = ir_val_inst(copy_id)
        reload_inst.load_type = slot_type
        reload_inst.result_type = slot_type
        reload_inst.has_result = true
        b.instructions.push(reload_inst)
        g.builder.next_id = load_id + 1
    }

    -- Track struct type for subsequent field access
    if elem_kind == LIST_ELEM_OTHER() and g.struct_defs.contains(elem_type) {
        g.var_struct_types.insert("_listget_" + int_to_string(load_id), elem_type)
    }
    return expr_result(g, b, load_id)
}

fn generate_index_access(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- A list, a map or a string in a box is indexed as itself
    let object = unboxed_container(g, *expr.object)
    let index = *expr.index

    -- m[k] is m.get(k)
    if is_map_type(g, static_type(g, object)) {
        let mut get_args: List[Expr] = []
        get_args.push(index)
        return generate_expr(g, b, expr_method_call(object, "get", get_args, expr.line, expr.col))
    }

    -- A list reached through a field, a box, a call, ...: index its storage
    let obj_static = if object.kind == EXPR_IDENTIFIER() { "" } else { static_type(g, object) }
    if starts_with(obj_static, "List[") {
        let h = generate_list_handle(g, b, object)
        g = h.gen
        b = h.block
        let list_idx = generate_expr(g, b, index)
        g = list_idx.gen
        b = list_idx.block
        let list_elem_src = extract_inner_type(obj_static, "List[")
        let list_get = generate_list_index_get(g, b, classify_list_elem(list_elem_src), list_elem_ir_type(list_elem_src), h.ptr.ref_id, list_idx.val_id, "(list)")
        -- Track struct elements for subsequent field access
        if g.struct_defs.contains(list_elem_src) {
            let mut lg = list_get.gen
            lg.var_struct_types.insert("_listget_" + int_to_string(list_get.val_id), list_elem_src)
            return expr_result(lg, list_get.block, list_get.val_id)
        }
        return list_get
    }

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

    -- Check if the object is a string (s[i] -> char_at)
    let mut obj_is_string = obj_static == "string"
    if object.kind == EXPR_IDENTIFIER() and g.variable_types.contains(object.name) {
        obj_is_string = g.variable_types.get(object.name) == "string"
    }
    if obj_is_string {
        -- Bounds check: panic unless 0 <= idx < strlen
        let str_len_id = g.builder.next_id
        let mut str_len_inst = ir_inst_new(str_len_id, OP_CALL())
        str_len_inst.callee = "dm_string_len"
        let mut str_len_args: List[IRValue] = []
        str_len_args.push(ir_val_inst(obj_id))
        str_len_inst.call_args = str_len_args
        str_len_inst.result_type = "i64"
        str_len_inst.has_result = true
        b.instructions.push(str_len_inst)
        g.builder.next_id = str_len_id + 1
        let str_chk = check_index_bounds(g, b, idx_id, ir_val_inst(str_len_id), "(string)")
        g = str_chk.gen
        b = str_chk.block

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = "dm_char_at"
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(obj_id))
        call_args.push(ir_val_inst(idx_id))
        inst.call_args = call_args
        inst.result_type = "string"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

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

            let bounds_ctx = if g.array_var_names.contains(var_name) { "(array)" } else { "(list)" }
            return generate_list_index_get(g, b, elem_kind, elem_type, obj_id, idx_id, bounds_ctx)
        }
    }

    -- Unknown object type: report it, then emit an i64 list read to continue
    g = ir_type_error(g, "cannot determine the type of the indexed value", expr.line)
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

    -- Resolve type parameters and generic struct instances
    g = ensure_type_instances(g, expr.type_name, expr.line)
    let type_name = canon_type(g, expr.type_name)
    if g.generic_struct_params.contains(type_name) {
        g = ir_type_error(g, "generic struct '" + type_name + "' needs type arguments, e.g. " + type_name + "[int] { ... }", expr.line)
    }

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
        let fval_expr = map_ctor_as(g, expr.field_values[i], struct_field_src_type(g, type_name, fname))

        -- Generate the field value expression; an integer becomes the
        -- field's integer type
        let mut val_result = generate_expr(g, b, fval_expr)
        val_result = convert_int_to_place(val_result.gen, val_result.block, val_result.val_id, fval_expr, struct_field_src_type(g, type_name, fname), "field '" + fname + "' of '" + type_name + "'")
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

-- ============================================================
-- GENERIC ENUMS
-- `enum Opt[T] { Has(T), Empty }` is one enum for each set of type
-- arguments it is used with, as a generic struct is: Opt[int] is the
-- enum Opt_i64 (ensure_type_instances). A constructor names no type
-- arguments, `Opt.Has(3)`, `Opt.Empty`: they are what the checker
-- found, from the values or from the place the value goes to.
-- ============================================================

-- Whether the generic enum has the variant
fn generic_enum_has_variant(gen: IRGenerator, enum_name: string, variant: string) -> bool {
    let decl = gen.declarations[gen.generic_struct_decls.get(enum_name)]
    let ed = *decl.enum_decl
    let mut i = 0
    while i < ed.variants.len() {
        if ed.variants[i].name == variant { return true }
        i = i + 1
    }
    return false
}

-- The type of the value that `expr`, a constructor of a variant of the
-- generic enum, makes: "Opt[int]". "" when nothing says.
fn generic_enum_type(gen: IRGenerator, expr: Expr, enum_name: string, variant: string, args: List[Expr]) -> string {
    if expr.id >= 0 and expr.id < gen.checked_types.len() {
        let checked = gen.checked_types[expr.id]
        if starts_with(checked, enum_name + "[") and string_contains(checked, "?") == false { return checked }
    }
    -- In the body of a generic function the checker records nothing:
    -- the types of the values, then the type the function returns
    let decl = gen.declarations[gen.generic_struct_decls.get(enum_name)]
    let ed = *decl.enum_decl
    let mut binds = ty_unbound(ed.generic_params.len())
    let mut vi = 0
    while vi < ed.variants.len() {
        let v = ed.variants[vi]
        if v.name == variant {
            let mut ai = 0
            while ai < args.len() and ai < v.payload_types.len() {
                binds = ty_bind(v.payload_types[ai], static_type(gen, args[ai]), ed.generic_params, binds)
                ai = ai + 1
            }
        }
        vi = vi + 1
    }
    let mut t = enum_name + "["
    let mut whole = true
    let mut bi = 0
    while bi < binds.len() {
        if binds[bi] == "" { whole = false }
        if bi > 0 { t = t + ", " }
        t = t + binds[bi]
        bi = bi + 1
    }
    if whole { return t + "]" }
    if starts_with(gen.current_return_type, enum_name + "[") { return gen.current_return_type }
    -- An instance of the function has the instance it returns by name
    let returned = canon_type(gen, gen.current_return_type)
    if gen.generic_instances.contains(returned) {
        if gen.generic_instances.get(returned) == enum_name { return returned }
    }
    return ""
}

struct EnumInstance {
    gen: IRGenerator,
    -- The instance's name; the generic enum's own when nothing says
    -- which instance it is, which is then reported
    name: string
}

-- The instance of the generic enum that `expr` makes a value of (see
-- generic_enum_type), registered
fn generic_enum_instance(gen: IRGenerator, expr: Expr, enum_name: string, variant: string, args: List[Expr]) -> EnumInstance {
    let mut g = gen
    let t = generic_enum_type(g, expr, enum_name, variant, args)
    if t == "" {
        g = ir_type_error(g, "cannot tell the type arguments of '" + enum_name + "' from '" + enum_name + "." + variant + "': give the place the value goes to a type, such as `let v: " + enum_name + "[int] = ...`", expr.line)
        return EnumInstance { gen: g, name: enum_name }
    }
    g = ensure_type_instances(g, t, expr.line)
    return EnumInstance { gen: g, name: canon_type(g, t) }
}

fn generate_enum_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let mut enum_name = expr.enum_name
    let variant_name = expr.variant_name
    if is_generic_enum(g, enum_name) {
        let instance = generic_enum_instance(g, expr, enum_name, variant_name, expr.payload)
        g = instance.gen
        enum_name = instance.name
    }

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
    if has_payload and variant_idx >= 0 {
        let payload = expr.payload
        let mut num_payload = payload.len()
        let payload_count = enum_payload_count(info, variant_idx)
        if num_payload != payload_count {
            g = ir_type_error(g, "variant '" + enum_name + "." + variant_name + "' carries " + values_text(payload_count) + ", not " + int_to_string(num_payload), expr.line)
            num_payload = 0
        }

        -- The variant's values go in its fields
        let field_offset = enum_payload_field(info, variant_idx)

        let mut pi = 0
        while pi < num_payload {
            let payload_expr = payload[pi]
            g = check_basic_type(g, enum_payload_type(info, variant_idx, pi), payload_expr, "value " + int_to_string(pi + 1) + " of '" + enum_name + "." + variant_name + "'")

            -- Generate the payload value
            let mut pval_result = generate_expr(g, b, payload_expr)
            pval_result = convert_int_to_place(pval_result.gen, pval_result.block, pval_result.val_id, payload_expr, enum_payload_type(info, variant_idx, pi), "value " + int_to_string(pi + 1) + " of '" + enum_name + "." + variant_name + "'")
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

-- Push a value that a list stores whole, such as a struct or an enum
-- value, onto the list at `list_ptr`: the runtime copies the bytes of
-- `elem_type` from a temporary that holds the value
fn push_whole_value(gen: IRGenerator, block: IRBasicBlock, list_ptr: int, val_id: int, elem_type: string) -> GenResult {
    let mut g = gen
    let mut b = block
    let temp_id = g.builder.next_id
    let mut temp_inst = ir_inst_new(temp_id, OP_ALLOCA())
    temp_inst.alloc_type = elem_type
    temp_inst.result_type = "ptr(" + elem_type + ")"
    temp_inst.has_result = true
    b.instructions.push(temp_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
    store_inst.store_ptr = ir_val_inst(temp_id)
    store_inst.store_val = ir_val_inst(val_id)
    store_inst.has_result = false
    b.instructions.push(store_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut push_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    push_inst.callee = "dm_list_generic_push"
    let mut push_args: List[IRValue] = []
    push_args.push(ir_val_inst(list_ptr))
    push_args.push(ir_val_inst(temp_id))
    push_args.push(ir_val_sizeof(elem_type))
    push_inst.call_args = push_args
    push_inst.result_type = "void"
    push_inst.has_result = false
    b.instructions.push(push_inst)
    g.builder.next_id = g.builder.next_id + 1
    return gen_result(g, b)
}

-- The type of the elements of a list literal whose elements are
-- lists, as [[1, 2], [3]] and [xs, ys] are: "List[int]". "" for any
-- other literal. Such a list stores each element's header whole.
fn nested_list_elem_type(gen: IRGenerator, lit: Expr) -> string {
    let t = static_type(gen, lit)
    if starts_with(t, "List[") == false { return "" }
    let inner = extract_inner_type(t, "List[")
    if starts_with(inner, "List[") { return inner }
    return ""
}

-- Whether the expression is a box of a list, a map or a string
fn is_boxed_container(gen: IRGenerator, e: Expr) -> bool {
    let t = static_type(gen, e)
    if starts_with(t, "Box[") == false { return false }
    let inner = extract_inner_type(t, "Box[")
    return starts_with(inner, "List[") or inner == "string" or is_map_type(gen, inner)
}

-- What is indexed or has a list's method called on it: a list, a map
-- or a string in a box stands for what is in the box, so b[i] is
-- (*b)[i] and b.len() is (*b).len(). Any other expression is itself.
fn unboxed_container(gen: IRGenerator, e: Expr) -> Expr {
    if is_boxed_container(gen, e) { return expr_unary(UNOP_DEREF(), e, e.line, e.col) }
    return e
}

-- List_new() and List.new(): the empty list
fn is_list_ctor(e: Expr) -> bool {
    if e.args.len() != 0 { return false }
    if e.kind == EXPR_FUNCTION_CALL() {
        let c = *e.callee
        return c.kind == EXPR_IDENTIFIER() and c.name == "List_new"
    }
    if e.kind == EXPR_METHOD_CALL() and e.method == "new" {
        let o = *e.object
        return o.kind == EXPR_IDENTIFIER() and o.name == "List"
    }
    return false
}

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

    -- Infer element type from the first element
    let mut elem_ir_type = infer_expr_type(g, elements[0])
    let nested = nested_list_elem_type(g, expr)
    if nested != "" { elem_ir_type = nested }

    -- Determine which list new/push functions to use
    let mut new_fn = "dm_list_int64_new"
    let mut push_fn = "dm_list_int64_push"
    if elem_ir_type == "f64" or elem_ir_type == "float" {
        new_fn = "dm_list_double_new"
        push_fn = "dm_list_double_push"
    } else if elem_ir_type == "string" {
        new_fn = "dm_list_string_new"
        push_fn = "dm_list_string_push"
    } else if g.struct_defs.contains(elem_ir_type) or g.enum_defs.contains(elem_ir_type) or nested != "" {
        -- Structs, enum values and lists are stored whole
        new_fn = "dm_list_generic_new"
        push_fn = "dm_list_generic_push"
    }

    -- Create list alloca and call new function (sret pattern)
    let alloca_id = g.builder.next_id
    let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
    alloca_inst.alloc_type = "list"
    alloca_inst.result_type = "ptr(list)"
    alloca_inst.has_result = true
    b.instructions.push(alloca_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut call_new = ir_inst_new(g.builder.next_id, OP_CALL())
    call_new.callee = new_fn
    let mut new_args: List[IRValue] = []
    new_args.push(ir_val_inst(alloca_id))
    call_new.call_args = new_args
    call_new.result_type = "void"
    call_new.has_result = false
    b.instructions.push(call_new)
    g.builder.next_id = g.builder.next_id + 1

    -- Push each element into the list
    let mut pi = 0
    if push_fn == "dm_list_generic_push" {
        while pi < num_elements {
            let pushed = push_whole_value(g, b, alloca_id, elem_ids[pi], elem_ir_type)
            g = pushed.gen
            b = pushed.block
            pi = pi + 1
        }
    }
    while pi < num_elements {
        let mut push_call = ir_inst_new(g.builder.next_id, OP_CALL())
        push_call.callee = push_fn
        let mut push_args: List[IRValue] = []
        push_args.push(ir_val_inst(alloca_id))
        push_args.push(ir_val_inst(elem_ids[pi]))
        push_call.call_args = push_args
        push_call.result_type = "void"
        push_call.has_result = false
        b.instructions.push(push_call)
        g.builder.next_id = g.builder.next_id + 1
        pi = pi + 1
    }

    return expr_result(g, b, alloca_id)
}

-- ============================================================
-- EXPRESSION IR GENERATION PART 5
-- If expressions, match expressions, block expressions,
-- lambda expressions, pipeline operator, error propagation,
-- string interpolation, and comptime expressions.
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

    -- Infer result type from the branches' trailing expressions
    let mut if_result_type = infer_expr_type(g, expr)
    if if_result_type == "void" {
        if_result_type = "i64"
    }

    -- Allocate a result slot before branching so both branches can store into it
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = if_result_type
    result_alloca.result_type = "ptr(" + if_result_type + ")"
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
    let stored_then = store_branch_value(g, then_block, result_alloca_id, if_result_type, then_result.val_id, *expr.then_branch)
    g = stored_then.gen
    then_block = stored_then.block

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
        let stored_else = store_branch_value(g, else_block, result_alloca_id, if_result_type, else_result.val_id, *expr.else_branch)
        g = stored_else.gen
        else_block = stored_else.block

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
    load_inst.load_type = if_result_type
    load_inst.result_type = if_result_type
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

    -- Check match arms for enum variant patterns to detect enum matching:
    -- the first arm that names a variant says which kind of value it is
    let mut variant_arm = 0
    while variant_arm < expr.match_arms.len() and expr.match_arms[variant_arm].pattern.kind != PAT_ENUM_VARIANT() {
        variant_arm = variant_arm + 1
    }
    if variant_arm < expr.match_arms.len() {
        let first_arm = expr.match_arms[variant_arm]
        let first_pat = first_arm.pattern

        -- Check for bare Ok/Err patterns (Result type)
        if first_pat.kind == PAT_ENUM_VARIANT() {
            let variant = first_pat.variant
            if variant == "Ok" or variant == "Err" {
                -- Determine the Result type for correct field types
                let mut result_type_name = ""
                if scrut_expr.kind == EXPR_IDENTIFIER() {
                    if g.variable_types.contains(scrut_expr.name) {
                        result_type_name = g.variable_types.get(scrut_expr.name)
                    }
                }
                if starts_with(result_type_name, "Result[") == false {
                    -- A call, a field or a variable with no annotation
                    let scrut_src = canon_type(g, static_type(g, scrut_expr))
                    if starts_with(scrut_src, "Result[") {
                        result_type_name = scrut_src
                    }
                }
                return generate_result_match(g, b, expr, scrut_id, result_type_name)
            }
            if variant == "Some" or variant == "None" {
                let mut option_type_name = ""
                if scrut_expr.kind == EXPR_IDENTIFIER() {
                    if g.variable_types.contains(scrut_expr.name) {
                        option_type_name = g.variable_types.get(scrut_expr.name)
                    }
                }
                if starts_with(option_type_name, "Option[") == false {
                    let scrut_src = canon_type(g, static_type(g, scrut_expr))
                    if starts_with(scrut_src, "Option[") {
                        option_type_name = scrut_src
                    }
                }
                return generate_option_match(g, b, expr, scrut_id, option_type_name)
            }
            -- General enum variant pattern with known type
            if len(first_pat.enum_type) > 0 {
                let mut matched_enum = first_pat.enum_type
                if is_generic_enum(g, matched_enum) {
                    -- Which instance it is, the matched value's type says
                    matched_enum = canon_type(g, static_type(g, scrut_expr))
                    if g.enum_defs.contains(matched_enum) == false {
                        g = ir_type_error(g, "cannot tell which '" + first_pat.enum_type + "' this match is on: give the matched value a type", expr.line)
                        return expr_result(g, b, scrut_id)
                    }
                }
                return generate_enum_match(g, b, expr, scrut_id, matched_enum)
            }
        }
    }

    -- Default: literal/identifier matching with cascading if-else
    return generate_literal_match(g, b, expr, scrut_id)
}

-- ============================================================
-- MATCH RESULT TYPE INFERENCE
-- ============================================================

fn generate_literal_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- The type of the matched value, for an arm that binds it to a name
    let scrut_bind_type = static_type(g, *expr.scrutinee)

    -- Infer the match result type from arm bodies
    let match_type = match_slot_type(g, expr, infer_match_result_type(g, expr.match_arms))

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = slot_alloc_type(match_type)
    result_alloca.result_type = "ptr(" + slot_alloc_type(match_type) + ")"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    let join_label = gen_next_label(g, "match_end")
    g = gen_advance_label(g)

    -- Process each arm as cascading if-else
    let mut final_block_pushed = false
    let mut arm_idx = 0
    while arm_idx < expr.match_arms.len() {
        let arm = expr.match_arms[arm_idx]
        let pat = arm.pattern

        let arm_label = gen_next_label(g, "match_arm")
        g = gen_advance_label(g)
        let next_label = gen_next_label(g, "match_next")
        g = gen_advance_label(g)

        if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            -- Bind identifier if needed
            if pat.kind == PAT_IDENTIFIER() {
                -- Bind the scrutinee value to the pattern name
                -- The name is a variable of the matched value's type
                let mut bind_type = "i64"
                if scrut_bind_type != "" {
                    bind_type = map_type_name(canon_type(g, scrut_bind_type))
                }
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = bind_type
                bind_alloca.result_type = "ptr(" + bind_type + ")"
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
                g.variable_types.insert(pat.name, bind_type)
                if scrut_bind_type != "" {
                    g = track_bound_variable(g, pat.name, scrut_bind_type)
                }
            }

            -- If there is a guard condition, evaluate it and branch
            if arm.has_guard {
                let guard_result = generate_expr(g, b, *arm.guard)
                g = guard_result.gen
                b = guard_result.block

                -- Branch: if guard is true, execute arm body; else go to next arm
                b.terminator = Box_new(term_br_cond(ir_val_inst(guard_result.val_id), arm_label, next_label))
                b.has_terminator = true
                g.completed_blocks.push(b)

                -- Generate arm body in arm_block
                let mut arm_block = ir_block_new(arm_label)
                if arm.is_expr_body {
                    let body_result = generate_expr(g, arm_block, *arm.body_expr)
                    g = body_result.gen
                    arm_block = body_result.block

                    let stored_body = store_branch_value(g, arm_block, result_alloca_id, match_type, body_result.val_id, *arm.body_expr)
                    g = stored_body.gen
                    arm_block = stored_body.block
                } else {
                    let body_gen = generate_arm_block(g, arm_block, arm.body, result_alloca_id, match_type)
                    g = body_gen.gen
                    arm_block = body_gen.block
                }

                if arm_block.has_terminator == false {
                    arm_block.terminator = Box_new(term_br(join_label))
                    arm_block.has_terminator = true
                }
                g.completed_blocks.push(arm_block)

                -- Continue to next arm (set up next block)
                b = ir_block_new(next_label)
                arm_idx = arm_idx + 1
            } else {
                -- No guard: always matches (catch-all)
                -- Generate arm body directly in current block
                if arm.is_expr_body {
                    let body_result = generate_expr(g, b, *arm.body_expr)
                    g = body_result.gen
                    b = body_result.block

                    let stored_body = store_branch_value(g, b, result_alloca_id, match_type, body_result.val_id, *arm.body_expr)
                    g = stored_body.gen
                    b = stored_body.block
                } else {
                    let body_gen = generate_arm_block(g, b, arm.body, result_alloca_id, match_type)
                    g = body_gen.gen
                    b = body_gen.block
                }

                if b.has_terminator == false {
                    b.terminator = Box_new(term_br(join_label))
                    b.has_terminator = true
                }
                g.completed_blocks.push(b)
                final_block_pushed = true

                -- Wildcard/identifier without guard is the catch-all, break out of loop
                arm_idx = expr.match_arms.len()
            }
        } else if pat.kind == PAT_LITERAL() {
            -- Generate comparison of scrutinee with literal
            let lit_expr = *pat.literal_expr
            let lit_result = generate_expr(g, b, lit_expr)
            g = lit_result.gen
            b = lit_result.block

            -- Compare scrutinee with literal value: strings by their text
            let cmp_id = g.builder.next_id
            let mut cmp_op = OP_EQ()
            if lit_expr.kind == EXPR_LITERAL_STRING() {
                cmp_op = OP_STRING_EQ()
            }
            let mut cmp_inst = ir_inst_new(cmp_id, cmp_op)
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
            g.completed_blocks.push(b)

            -- Generate arm body in arm_block
            let mut arm_block = ir_block_new(arm_label)
            if arm.is_expr_body {
                let body_result = generate_expr(g, arm_block, *arm.body_expr)
                g = body_result.gen
                arm_block = body_result.block

                let stored_body = store_branch_value(g, arm_block, result_alloca_id, match_type, body_result.val_id, *arm.body_expr)
                g = stored_body.gen
                arm_block = stored_body.block
            } else {
                let body_gen = generate_arm_block(g, arm_block, arm.body, result_alloca_id, match_type)
                g = body_gen.gen
                arm_block = body_gen.block
            }

            if arm_block.has_terminator == false {
                arm_block.terminator = Box_new(term_br(join_label))
                arm_block.has_terminator = true
            }
            g.completed_blocks.push(arm_block)

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
    if final_block_pushed == false { g.completed_blocks.push(b) }

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = slot_alloc_type(match_type)
    load_inst.result_type = slot_alloc_type(match_type)
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- ENUM MATCH
-- ============================================================

fn generate_enum_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, enum_type: string) -> ExprResult {
    if gen.enum_defs.contains(enum_type) == false {
        return generate_literal_match(gen, block, expr, scrut_id)
    }
    return generate_variant_match(gen, block, expr, scrut_id, gen.enum_defs.get(enum_type), enum_type)
}

-- A Result is matched as the enum it is laid out as: the tag, 0 for
-- Ok, then Ok's value and Err's value
fn generate_result_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, result_type_name: string) -> ExprResult {
    let mut ok_type = "int"
    let mut err_type = "int"
    if starts_with(result_type_name, "Result[") {
        let res_args = split_type_args(extract_inner_type(result_type_name, "Result["))
        if res_args.len() == 2 {
            ok_type = res_args[0]
            err_type = res_args[1]
        }
    }
    let mut info = enum_info_new("Result")
    info.variant_names.push("Ok")
    info.variant_tags.push(0)
    info.variant_payload_types.push(ok_type)
    info.variant_has_payload.push(true)
    info.variant_payloads.push(ok_type)
    info.variant_names.push("Err")
    info.variant_tags.push(1)
    info.variant_payload_types.push(err_type)
    info.variant_has_payload.push(true)
    info.variant_payloads.push(err_type)
    return generate_variant_match(gen, block, expr, scrut_id, info, result_type_name)
}

-- An Option is matched as the enum it is laid out as: the tag, 1 for
-- Some, then Some's value
fn generate_option_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, option_type_name: string) -> ExprResult {
    let mut some_type = "int"
    if starts_with(option_type_name, "Option[") {
        some_type = extract_inner_type(option_type_name, "Option[")
    }
    let mut info = enum_info_new("Option")
    info.variant_names.push("Some")
    info.variant_tags.push(1)
    info.variant_payload_types.push(some_type)
    info.variant_has_payload.push(true)
    info.variant_payloads.push(some_type)
    info.variant_names.push("None")
    info.variant_tags.push(0)
    info.variant_payload_types.push("")
    info.variant_has_payload.push(false)
    info.variant_payloads.push("")
    return generate_variant_match(gen, block, expr, scrut_id, info, option_type_name)
}

-- A match on the variants of an enum, an Option or a Result. The arms
-- are tried in the order they are written: an arm of a variant is
-- taken when the value is that variant and the arm's guard, if it has
-- one, holds; a name or `_` takes any value. `scrut_type` is the type
-- of the matched value, "" when it is not known.
fn generate_variant_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, enum_info: EnumInfo, scrut_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block
    let enum_type = enum_info.name
    -- The type of the matched value, for an arm that binds it to a name
    let scrut_bind_type = scrut_type

    -- Infer match result type from arm bodies
    let ematch_type = match_slot_type(g, expr, infer_match_result_type(g, expr.match_arms))

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = slot_alloc_type(ematch_type)
    result_alloca.result_type = "ptr(" + slot_alloc_type(ematch_type) + ")"
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
    let mut final_block_pushed = false
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
                -- The name is a variable of the matched value's type
                let mut bind_type = "i64"
                if scrut_bind_type != "" {
                    bind_type = map_type_name(canon_type(g, scrut_bind_type))
                }
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = bind_type
                bind_alloca.result_type = "ptr(" + bind_type + ")"
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
                g.variable_types.insert(pat.name, bind_type)
                if scrut_bind_type != "" {
                    g = track_bound_variable(g, pat.name, scrut_bind_type)
                }
            }

            -- With a guard, the arm is taken when the guard holds
            if arm.has_guard {
                let guard = generate_expr(g, b, *arm.guard)
                g = guard.gen
                b = guard.block
                b.terminator = Box_new(term_br_cond(ir_val_inst(guard.val_id), arm_label, next_label))
                b.has_terminator = true
                g.completed_blocks.push(b)
                b = ir_block_new(arm_label)
            }

            -- Generate arm body in current block
            if arm.is_expr_body {
                let body_result = generate_expr(g, b, *arm.body_expr)
                g = body_result.gen
                b = body_result.block

                let stored_body = store_branch_value(g, b, result_alloca_id, ematch_type, body_result.val_id, *arm.body_expr)
                g = stored_body.gen
                b = stored_body.block
            } else {
                let body_gen = generate_arm_block(g, b, arm.body, result_alloca_id, ematch_type)
                g = body_gen.gen
                b = body_gen.block
            }

            if b.has_terminator == false {
                b.terminator = Box_new(term_br(join_label))
                b.has_terminator = true
            }
            g.completed_blocks.push(b)
            if arm.has_guard {
                b = ir_block_new(next_label)
                arm_idx = arm_idx + 1
            } else {
                -- No arm after this one is reached
                final_block_pushed = true
                arm_idx = expr.match_arms.len()
            }
        } else if pat.kind == PAT_ENUM_VARIANT() {
            -- Find variant tag value from enum definition
            let variant_name = pat.variant
            let variant_index = enum_variant_index(enum_info, variant_name)
            let mut variant_tag = -1
            let mut payload_count = 0
            if variant_index >= 0 {
                variant_tag = enum_info.variant_tags[variant_index]
                payload_count = enum_payload_count(enum_info, variant_index)
            } else {
                g = ir_type_error(g, "enum '" + enum_type + "' has no variant '" + variant_name + "'", expr.line)
            }
            if pat.bindings.len() > payload_count and variant_index >= 0 {
                g = ir_type_error(g, "variant '" + enum_type + "." + variant_name + "' carries " + values_text(payload_count) + ", but the pattern binds " + int_to_string(pat.bindings.len()), expr.line)
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
            g.completed_blocks.push(b)

            -- Generate arm body in arm_block
            let mut arm_block = ir_block_new(arm_label)

            -- Bind the variant's values: each is a variable of its type
            let mut bi = 0
            while bi < pat.bindings.len() and bi < payload_count {
                let bind_name = pat.bindings[bi]
                let payload_src = enum_payload_type(enum_info, variant_index, bi)
                let payload_type = map_type_name(payload_src)

                let payload_id = g.builder.next_id
                let mut payload_inst = ir_inst_new(payload_id, OP_EXTRACT_FIELD())
                payload_inst.field_base = ir_val_inst(scrut_id)
                payload_inst.field_index = enum_payload_field(enum_info, variant_index) + bi
                payload_inst.field_type = payload_type
                payload_inst.result_type = payload_type
                payload_inst.has_result = true
                arm_block.instructions.push(payload_inst)
                g.builder.next_id = g.builder.next_id + 1

                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = payload_type
                bind_alloca.result_type = "ptr(" + payload_type + ")"
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
                g = track_bound_variable(g, bind_name, payload_src)
                bi = bi + 1
            }

            -- With a guard, which may read the values just bound, the
            -- arm is taken when the guard holds; the arms after it are
            -- tried when it does not
            if arm.has_guard {
                let guard = generate_expr(g, arm_block, *arm.guard)
                g = guard.gen
                arm_block = guard.block
                let body_label = gen_next_label(g, "ematch_body")
                g = gen_advance_label(g)
                arm_block.terminator = Box_new(term_br_cond(ir_val_inst(guard.val_id), body_label, next_label))
                arm_block.has_terminator = true
                g.completed_blocks.push(arm_block)
                arm_block = ir_block_new(body_label)
            }

            if arm.is_expr_body {
                let body_result = generate_expr(g, arm_block, *arm.body_expr)
                g = body_result.gen
                arm_block = body_result.block

                let stored_body = store_branch_value(g, arm_block, result_alloca_id, ematch_type, body_result.val_id, *arm.body_expr)
                g = stored_body.gen
                arm_block = stored_body.block
            } else {
                let body_gen = generate_arm_block(g, arm_block, arm.body, result_alloca_id, ematch_type)
                g = body_gen.gen
                arm_block = body_gen.block
            }

            if arm_block.has_terminator == false {
                arm_block.terminator = Box_new(term_br(join_label))
                arm_block.has_terminator = true
            }
            g.completed_blocks.push(arm_block)

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
    if final_block_pushed == false { g.completed_blocks.push(b) }

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = slot_alloc_type(ematch_type)
    load_inst.result_type = slot_alloc_type(ematch_type)
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- BLOCK EXPRESSION
-- ============================================================

-- Whether the block, which ends in an `if` or a `match` statement, has
-- that statement's value: as the checker found, and where it recorded
-- nothing (the body of a generic function), as the statement's
-- branches say
fn block_ends_in_value(gen: IRGenerator, expr: Expr) -> bool {
    if expr.id >= 0 and expr.id < gen.checked_types.len() {
        let checked = gen.checked_types[expr.id]
        if checked == "void" { return false }
        if checked != "" { return true }
    }
    let last = expr.stmts[expr.stmts.len() - 1]
    return infer_expr_type(gen, stmt_value_expr(last)) != "void"
}

fn generate_block_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- A block whose last statement is a non-void expression, such as an
    -- `if` branch `{ a }`, has that expression's value
    let n = expr.stmts.len()
    if expr.has_result == false and n > 0 {
        let last = expr.stmts[n - 1]
        if last.kind == STMT_EXPRESSION() and infer_expr_type(g, *last.expr) != "void" {
            let mut lead: List[Stmt] = []
            let mut li = 0
            while li < n - 1 {
                lead.push(expr.stmts[li])
                li = li + 1
            }
            let lead_result = generate_block_stmts(g, b, lead)
            g = lead_result.gen
            b = lead_result.block
            if b.has_terminator {
                return expr_result(g, b, g.builder.next_id - 1)
            }
            return generate_expr(g, b, *last.expr)
        }
        -- A block that ends in an `if` with an `else` or in a `match`
        -- has that statement's value, unless the checker found that
        -- it gives none
        if last.kind != STMT_EXPRESSION() and stmt_gives_value(last) and block_ends_in_value(g, expr) {
            let mut lead: List[Stmt] = []
            let mut li = 0
            while li < n - 1 {
                lead.push(expr.stmts[li])
                li = li + 1
            }
            let lead_result = generate_block_stmts(g, b, lead)
            g = lead_result.gen
            b = lead_result.block
            if b.has_terminator {
                return expr_result(g, b, g.builder.next_id - 1)
            }
            return generate_expr(g, b, stmt_value_expr(last))
        }
    }

    -- Generate all statements in the block
    let stmts_result = generate_block_stmts(g, b, expr.stmts)
    g = stmts_result.gen
    b = stmts_result.block

    -- If the block already has a terminator (e.g., from a return statement), just return
    if b.has_terminator {
        return expr_result(g, b, g.builder.next_id - 1)
    }

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

-- The error for a lambda that captures `variable` and is used as a
-- value (passed, returned or stored) instead of being called by name
fn captured_lambda_error(variable: string, what: string) -> string {
    return what + " uses '" + variable + "', a variable of the function around it, so it can only be bound to a name with 'let' and called by that name; pass it '" + variable + "' as a parameter to use it as a value"
}

fn generate_lambda_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let bound_by_let = g.lambda_bound_by_let
    g.lambda_bound_by_let = false

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
    lambda_func.source_line = expr.line

    -- Add lambda parameters
    let mut lambda_param_src = ""
    let mut pi = 0
    while pi < expr.lambda_params.len() {
        let lp = expr.lambda_params[pi]
        let ptype = if len(lp.type_name) > 0 {
            map_type_name(lp.type_name)
        } else {
            "i64"
        }
        lambda_func.params.push(ir_param_new(lp.name, ptype))
        if pi > 0 { lambda_param_src = lambda_param_src + "|" }
        if len(lp.type_name) > 0 {
            lambda_param_src = lambda_param_src + lp.type_name
        } else {
            lambda_param_src = lambda_param_src + "int"
        }
        pi = pi + 1
    }
    -- What a call of the lambda by its name converts its arguments to
    g.fn_src_param_types.insert(lambda_name, lambda_param_src)

    -- Collect captured variables: the variables of the current scope
    -- that the lambda's body mentions. A lambda that captures nothing
    -- is a plain function, which can be passed where a fn(...) is
    -- expected.
    -- Build a pipe-delimited list of captured variable names
    let mut captures = ""
    let mut capture_names: List[string] = []
    let mut capture_types: List[string] = []
    let body_names = expr_names(*expr.lambda_body)
    let mut mentioned: Map[string, bool] = Map_new()
    let mut ni = 0
    while ni < body_names.len() {
        mentioned.insert(body_names[ni], true)
        ni = ni + 1
    }
    let mut keys = g.variable_map.keys()
    let mut ki = 0
    while ki < keys.len() {
        let var_name = keys[ki]
        -- Skip lambda parameters (they are not captures)
        let mut is_param = mentioned.contains(var_name) == false
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

    -- A captured variable is an extra parameter of the lambda's function,
    -- which only a call of the lambda by its name passes. Anywhere else
    -- the lambda would be called without it.
    if capture_names.len() > 0 and bound_by_let == false {
        eprintln(error_at(g.ir_mod.files, captured_lambda_error(capture_names[0], "this lambda"), expr.line))
        exit(1)
    }

    -- Save current generator state
    let saved_variable_map = g.variable_map
    let saved_variable_types = g.variable_types
    let saved_return_type = g.current_return_type
    let saved_next_id = g.builder.next_id
    let saved_region_depth = g.region_depth
    let saved_tainted_vars = g.tainted_vars
    let saved_ref_vars = g.ref_vars
    let saved_region_born_vars = g.region_born_vars
    let saved_declared_vars = g.declared_vars
    let saved_region_marks = g.region_marks
    let saved_completed_blocks = g.completed_blocks

    -- The lambda's blocks are its own: a body with a branch in it (an
    -- `if`, `and`, a division's zero check) finishes blocks as it goes
    let empty_lambda_blocks: List[IRBasicBlock] = []
    g.completed_blocks = empty_lambda_blocks

    -- Set up new variable scope for the lambda body. Safety state is
    -- fresh per function; unsafe_depth is lexical and carries in.
    g.variable_map = Map_new()
    g.region_depth = 0
    g.tainted_vars = Map_new()
    g.ref_vars = Map_new()
    g.region_born_vars = Map_new()
    let empty_lambda_declared: List[string] = []
    g.declared_vars = empty_lambda_declared
    let empty_lambda_marks: List[int] = []
    g.region_marks = empty_lambda_marks
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
        g.declared_vars.push(lp.name)
        if g.var_src_types.contains(lp.name) {
            g.var_src_types.remove(lp.name)
        }
        let param_debug = debug_var(g, entry_block, lp.name, expr.line, param_idx + 1)
        g = param_debug.gen
        entry_block = param_debug.block
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
        g.declared_vars.push(cap_name)
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

    -- The lambda's blocks in order: the ones the body finished, then
    -- the block it ended in (the entry block itself for a simple body)
    let mut lbi = 0
    while lbi < g.completed_blocks.len() {
        lambda_func.blocks.push(g.completed_blocks[lbi])
        lbi = lbi + 1
    }
    lambda_func.blocks.push(entry_block)
    g.ir_mod.functions.push(lambda_func)

    -- Restore generator state
    g.variable_map = saved_variable_map
    g.variable_types = saved_variable_types
    g.current_return_type = saved_return_type
    g.builder.next_id = saved_next_id
    g.region_depth = saved_region_depth
    g.tainted_vars = saved_tainted_vars
    g.ref_vars = saved_ref_vars
    g.region_born_vars = saved_region_born_vars
    g.declared_vars = saved_declared_vars
    g.region_marks = saved_region_marks
    g.completed_blocks = saved_completed_blocks

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

        check_extern_fence(g, fn_name, pipe_right.line)

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

        check_extern_fence(g, fn_name, pipe_right.line)

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

    g = ir_type_error(g, "unsupported pipeline target", expr.line)
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

    -- Infer operand type to get correct Ok/Err field types
    let operand_type = infer_expr_type(g, *expr.operand)
    let mut ok_field_type = "i64"
    let mut err_field_type = "i64"
    if starts_with(operand_type, "Result[") {
        let inner = substr(operand_type, 7, len(operand_type) - 8)
        let mut comma_pos = 0 - 1
        let mut depth = 0
        let mut ci = 0
        while ci < len(inner) {
            let ch = char_at(inner, ci)
            if ch == "[" { depth = depth + 1 }
            if ch == "]" { depth = depth - 1 }
            if ch == "," and depth == 0 {
                comma_pos = ci
                ci = len(inner)
            }
            ci = ci + 1
        }
        if comma_pos >= 0 {
            ok_field_type = map_type_name(substr(inner, 0, comma_pos))
            let mut err_start = comma_pos + 1
            if err_start < len(inner) and char_at(inner, err_start) == " " {
                err_start = err_start + 1
            }
            err_field_type = map_type_name(substr(inner, err_start, len(inner) - err_start))
        }
    } else if starts_with(operand_type, "Option[") {
        let inner = substr(operand_type, 7, len(operand_type) - 8)
        ok_field_type = map_type_name(inner)
    }

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

    -- Pre-allocate storage for the Ok value (must be in current block, before the branch)
    let ok_alloca_id = g.builder.next_id
    let mut ok_alloca = ir_inst_new(ok_alloca_id, OP_ALLOCA())
    ok_alloca.alloc_type = ok_field_type
    ok_alloca.result_type = "ptr(" + ok_field_type + ")"
    ok_alloca.has_result = true
    b.instructions.push(ok_alloca)
    g.builder.next_id = g.builder.next_id + 1

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
    err_extract.field_type = err_field_type
    err_extract.result_type = err_field_type
    err_extract.has_result = true
    err_block.instructions.push(err_extract)
    g.builder.next_id = g.builder.next_id + 1

    -- Return the operand directly (it already has the Err tag).
    err_block.terminator = Box_new(term_ret(ir_val_inst(operand_id)))
    err_block.has_terminator = true
    g.completed_blocks.push(err_block)
    -- Ok path: extract the Ok/Some value (field 1)
    let mut ok_block = ir_block_new(ok_label)
    let ok_val_id = g.builder.next_id
    let mut ok_extract = ir_inst_new(ok_val_id, OP_EXTRACT_FIELD())
    ok_extract.field_base = ir_val_inst(operand_id)
    ok_extract.field_index = 1
    ok_extract.field_type = ok_field_type
    ok_extract.result_type = ok_field_type
    ok_extract.has_result = true
    ok_block.instructions.push(ok_extract)
    g.builder.next_id = g.builder.next_id + 1

    -- Store ok value into the pre-allocated alloca
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
    load_inst.load_type = ok_field_type
    load_inst.result_type = ok_field_type
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
            } else if is_integer_type(part_type) or part_type == "int" {
                let conv = emit_int_to_string(g, b, part_val_id, part_type)
                g = conv.gen
                b = conv.block
                str_val_id = conv.val_id
            } else if part_type == "f64" or part_type == "f32" or part_type == "float" {
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
    return infer_expr_type(gen, part)
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


-- ============================================================
-- SIMD Builtin IR Generation
-- Handles simd_splat_*, simd_set_*, simd_add/sub/mul/div,
-- and simd_extract intrinsic function calls.
-- ============================================================

-- ============================================================
-- SIMD BUILTIN DETECTION
-- ============================================================

-- Returns true if the function name is a SIMD builtin intrinsic

fn generate_fn_ptr_param_call(gen: IRGenerator, block: IRBasicBlock, var_name: string, var_type: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Load the function pointer value from the parameter's alloca
    let alloca_id = g.variable_map.get(var_name)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(alloca_id)
    load_inst.load_type = "ptr"
    load_inst.result_type = "ptr"
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Generate arguments; an integer becomes the integer type the
    -- function type names for it
    let param_types = ty_fn_params(var_type)
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let mut arg_result = generate_expr(g, b, args[i])
        if i < param_types.len() {
            arg_result = convert_int_to_place(arg_result.gen, arg_result.block, arg_result.val_id, args[i], param_types[i], "argument " + int_to_string(i + 1) + " of '" + var_name + "'")
        }
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    -- Parse return type from "fn(int, int) -> int"
    let arrow_pos = string_find(var_type, " -> ")
    let ret_type_str = if arrow_pos >= 0 {
        substr(var_type, arrow_pos + 4, len(var_type) - arrow_pos - 4)
    } else {
        "void"
    }
    let ret_type = map_type_name(ret_type_str)
    let has_result = ret_type != "void"

    -- Emit OP_CALL_PTR instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL_PTR())
    inst.callee_val = ir_val_inst(load_id)
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

-- Store the value that a branch of an `if` or an arm of a `match`
-- gives into the slot of the whole. The branches may give numbers of
-- different widths, which meet at the slot's type (checked_slot_type
-- in ir_gen.dm): a narrower integer widens to it, and a literal takes
-- it. `value` is the branch's expression, generated as `val_id`.
fn store_branch_value(gen: IRGenerator, block: IRBasicBlock, slot_id: int, slot_type: string, val_id: int, value: Expr) -> GenResult {
    let mut g = gen
    let mut b = block
    -- A `match` that is a statement keeps no value
    if slot_type == "void" { return gen_result(g, b) }
    let mut store_id = val_id
    -- The type is asked for here, where the branch's variables are known
    let from = infer_expr_type(g, value)
    if b.has_terminator == false and from != slot_type {
        if is_integer_type(from) and is_integer_type(slot_type) {
            let conv = convert_int_value(g, b, val_id, branch_value_expr(value), from, slot_type, "the value of this branch")
            g = conv.gen
            b = conv.block
            store_id = conv.val_id
        } else if is_float_type(from) and is_float_type(slot_type) {
            let conv = emit_ir_cast(g, b, ir_val_inst(val_id), from, slot_type)
            g = conv.gen
            b = conv.block
            store_id = conv.val_id
        }
    }
    return emit_ir_store(g, b, ir_val_inst(slot_id), ir_val_inst(store_id))
}

-- The type of the slot that holds the value of the `match`: the
-- checker's, else `inferred` (checked_slot_type); and "void" for a
-- `match` that is a statement, whose arms' values are not stored
fn match_slot_type(gen: IRGenerator, expr: Expr, inferred: string) -> string {
    if gen.unused_match == expr.id and expr.id >= 0 { return "void" }
    return checked_slot_type(gen, expr, inferred)
}

-- The type a slot of the type is allocated with: a slot that is not
-- used still has a size
fn slot_alloc_type(slot_type: string) -> string {
    if slot_type == "void" { return "i64" }
    return slot_type
}

-- The block of a match arm. When it ends in an expression of the type
-- of the match's slot, or in a number where the slot holds a number
-- of another width, that value is the arm's and goes into the slot.
-- An `if` with an `else` or a `match` that ends the block is such an
-- expression (stmt_value_expr).
fn generate_arm_block(gen: IRGenerator, block: IRBasicBlock, stmts: List[Stmt], slot_id: int, slot_type: string) -> GenResult {
    let n = stmts.len()
    if n == 0 { return gen_result(gen, block) }
    let last = stmts[n - 1]
    if slot_type == "void" or stmt_gives_value(last) == false { return generate_block_stmts(gen, block, stmts) }
    let mut lead: List[Stmt] = []
    let mut li = 0
    while li < n - 1 {
        lead.push(stmts[li])
        li = li + 1
    }
    let lead_result = generate_block_stmts(gen, block, lead)
    let mut g = lead_result.gen
    let mut b = lead_result.block
    if b.has_terminator { return gen_result(g, b) }
    -- The type is asked for here, where the block's variables are known
    let value = stmt_value_expr(last)
    let value_type = infer_expr_type(g, value)
    let both_integers = is_integer_type(value_type) and is_integer_type(slot_type)
    let both_floats = is_float_type(value_type) and is_float_type(slot_type)
    if value_type != slot_type and both_integers == false and both_floats == false { return generate_statement(g, b, last) }
    let value_result = generate_expr(g, b, value)
    g = value_result.gen
    b = value_result.block
    if b.has_terminator { return gen_result(g, b) }
    return store_branch_value(g, b, slot_id, slot_type, value_result.val_id, value)
}

fn infer_match_result_type(gen: IRGenerator, arms: List[MatchArm]) -> string {
    -- The first arm's type, and for integers the type the arms meet at
    if arms.len() == 0 {
        return "i64"
    }
    let arm = arms[0]
    if arm.is_expr_body {
        -- Arms that give no value, such as calls of println, leave the
        -- result slot unused; it still needs a type that has a size
        let arm_type = infer_expr_type(gen, *arm.body_expr)
        if arm_type == "void" {
            return "i64"
        }
        return arms_meet_type(gen, arms, arm_type)
    }
    return "i64"
}

-- ============================================================
-- LITERAL MATCH (int, string, bool, wildcard)
-- ============================================================

fn join_types(types: List[string]) -> string {
    let mut result = ""
    let mut i = 0
    while i < types.len() {
        if i > 0 {
            result = result + "_"
        }
        result = result + map_type_name(types[i])
        i = i + 1
    }
    return result
}
