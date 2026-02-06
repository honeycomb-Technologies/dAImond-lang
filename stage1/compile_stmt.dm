module compile_stmt

import compiler

-- ============================================================
-- STATEMENT COMPILER
-- ============================================================

fn compile_stmt(c: Compiler) -> Compiler {
    let mut cc = c_skip_nl(c)
    let k = c_peek(cc)
    let ind = indent_str(cc.indent)

    if k == TK_LET() {
        return compile_let_stmt(cc)
    }
    if k == TK_RETURN() {
        cc = c_advance(cc)
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_NEWLINE() or c_peek(cc) == TK_RBRACE() or c_peek(cc) == TK_EOF() {
            cc.output = cc.output + ind + "return;\n"
        } else {
            -- Set expected type for Some/None/Ok/Err from fn return type
            let saved_expected = cc.expected_type
            if cc.current_fn_ret_type != "" {
                cc.expected_type = cc.current_fn_ret_type
            }
            let val = compile_expr(cc)
            cc = val.c
            cc.expected_type = saved_expected
            -- If in async function, wrap return value with _ready()
            if cc.current_fn_is_async and cc.current_fn_async_inner_type != "" {
                let future_t = "dm_future_" + cc.current_fn_async_inner_type
                cc.output = cc.output + ind + "return " + future_t + "_ready(" + val.code + ");\n"
            } else {
                cc.output = cc.output + ind + "return " + val.code + ";\n"
            }
        }
        return cc
    }
    if k == TK_IF() {
        return compile_if_stmt(cc)
    }
    if k == TK_WHILE() {
        return compile_while_stmt(cc)
    }
    if k == TK_FOR() {
        return compile_for_stmt(cc)
    }
    if k == TK_BREAK() {
        cc = c_advance(cc)
        cc.output = cc.output + ind + "break;\n"
        return cc
    }
    if k == TK_CONTINUE() {
        cc = c_advance(cc)
        cc.output = cc.output + ind + "continue;\n"
        return cc
    }
    if k == TK_LOOP() {
        return compile_loop_stmt(cc)
    }
    if k == TK_MATCH() {
        return compile_match_stmt(cc)
    }
    if k == TK_REGION() {
        return compile_region_stmt(cc)
    }
    if k == TK_CONST() {
        return compile_const_stmt(cc)
    }
    -- Expression statement (possibly assignment)
    let lhs = compile_expr(cc)
    cc = lhs.c
    -- Check for assignment
    if c_peek(cc) == TK_EQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        -- Check for map index assignment: dm_map_K_V_get(&dm_var, key) = val -> dm_map_K_V_insert(&dm_var, key, val)
        if starts_with(lhs.code, "dm_map_") and string_contains(lhs.code, "_get(") {
            let get_pos = string_find(lhs.code, "_get(")
            let map_type_name = substr(lhs.code, 0, get_pos)
            let args_part = substr(lhs.code, get_pos + 5, len(lhs.code) - get_pos - 6)
            cc.output = cc.output + ind + map_type_name + "_insert(" + args_part + ", " + rhs.code + ");\n"
        } else {
            cc.output = cc.output + ind + lhs.code + " = " + rhs.code + ";\n"
        }
    } else if c_peek(cc) == TK_PLUSEQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = (" + lhs.code + " + " + rhs.code + ");\n"
    } else if c_peek(cc) == TK_MINUSEQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = (" + lhs.code + " - " + rhs.code + ");\n"
    } else if c_peek(cc) == TK_STAREQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = (" + lhs.code + " * " + rhs.code + ");\n"
    } else if c_peek(cc) == TK_SLASHEQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = (" + lhs.code + " / " + rhs.code + ");\n"
    } else {
        cc.output = cc.output + ind + lhs.code + ";\n"
    }
    return cc
}

fn lookup_fn_ret_type(c: Compiler, name: string) -> string {
    let mut i = 0
    while i < c.fn_names.len() {
        -- Use "" + to force string concat context (Stage 0 workaround)
        let cur = "" + c.fn_names[i]
        if cur == name {
            let ret = "" + c.fn_ret_types[i]
            return ret
        }
        i = i + 1
    }
    return ""
}

fn is_struct_name(c: Compiler, name: string) -> bool {
    let mut i = 0
    while i < c.struct_names.len() {
        let cur = "" + c.struct_names[i]
        if cur == name {
            return true
        }
        i = i + 1
    }
    return false
}

fn lookup_list_elem_type(c: Compiler, list_var: string) -> string {
    let marker = "|" + list_var + "="
    let pos = string_find(c.list_elem_types, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.list_elem_types, start, len(c.list_elem_types) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

fn infer_list_get_type(code: string, c: Compiler) -> string {
    if starts_with(code, "DM_LIST_GET(") == false { return "" }
    let paren = string_find(code, "(")
    let comma = string_find(code, ",")
    if comma < 0 { return "" }
    let list_var = substr(code, paren + 1, comma - paren - 1)
    -- Try direct lookup first
    let direct = lookup_list_elem_type(c, list_var)
    if direct != "" { return direct }
    -- Try struct field access: dm_var.field -> look up field type
    let dot_pos = string_find(list_var, ".")
    if dot_pos < 0 { return "" }
    let var_part = substr(list_var, 0, dot_pos)
    let field_part = substr(list_var, dot_pos + 1, len(list_var) - dot_pos - 1)
    let var_type = lookup_var_type(c, var_part)
    if var_type == "" { return "" }
    let field_type = lookup_struct_field_type(c, var_type, field_part)
    if field_type == "" { return "" }
    -- field_type should be like "dm_list_dm_Token" -> extract element type after "dm_list_"
    if starts_with(field_type, "dm_list_") == false { return "" }
    return substr(field_type, 8, len(field_type) - 8)
}

fn track_var_type(c: Compiler, mangled_name: string, c_type: string) -> Compiler {
    let mut cc = c
    cc.var_types = cc.var_types + "|" + mangled_name + "=" + c_type + "|"
    return cc
}

fn lookup_var_type(c: Compiler, mangled_name: string) -> string {
    let marker = "|" + mangled_name + "="
    let pos = string_find(c.var_types, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.var_types, start, len(c.var_types) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

fn infer_builtin_ret_type(fn_code: string) -> string {
    -- Future await: dm_future_T_await(...) -> T
    if starts_with(fn_code, "dm_future_") and string_contains(fn_code, "_await(") {
        let await_pos = string_find(fn_code, "_await(")
        let inner_type = substr(fn_code, 10, await_pos - 10)
        return inner_type
    }
    -- Builtins that return specific types
    if starts_with(fn_code, "dm_parse_int(") { return "int64_t" }
    if starts_with(fn_code, "dm_parse_float(") { return "double" }
    if starts_with(fn_code, "dm_len(") { return "int64_t" }
    if starts_with(fn_code, "dm_string_find(") { return "int64_t" }
    if starts_with(fn_code, "dm_string_contains(") { return "bool" }
    if starts_with(fn_code, "dm_string_starts_with(") { return "bool" }
    if starts_with(fn_code, "dm_string_ends_with(") { return "bool" }
    if starts_with(fn_code, "dm_args_len(") { return "int64_t" }
    if starts_with(fn_code, "dm_system(") { return "int64_t" }
    -- String-returning builtins
    if starts_with(fn_code, "dm_string_trim(") { return "dm_string" }
    if starts_with(fn_code, "dm_string_replace(") { return "dm_string" }
    if starts_with(fn_code, "dm_string_to_upper(") { return "dm_string" }
    if starts_with(fn_code, "dm_string_to_lower(") { return "dm_string" }
    if starts_with(fn_code, "dm_char_at(") { return "dm_string" }
    if starts_with(fn_code, "dm_substr(") { return "dm_string" }
    if starts_with(fn_code, "dm_args_get(") { return "dm_string" }
    if starts_with(fn_code, "dm_file_read(") { return "dm_string" }
    if starts_with(fn_code, "dm_int_to_string(") { return "dm_string" }
    if starts_with(fn_code, "dm_float_to_string(") { return "dm_string" }
    if starts_with(fn_code, "dm_bool_to_string(") { return "dm_string" }
    if starts_with(fn_code, "dm_string_concat(") { return "dm_string" }
    if starts_with(fn_code, "dm_string_concat_arena(") { return "dm_string" }
    if starts_with(fn_code, "dm_string_split(") { return "dm_list_dm_string" }
    -- SIMD splat builtins
    if starts_with(fn_code, "dm_simd_splat_f32x4(") { return "dm_f32x4" }
    if starts_with(fn_code, "dm_simd_splat_f32x8(") { return "dm_f32x8" }
    if starts_with(fn_code, "dm_simd_splat_f64x2(") { return "dm_f64x2" }
    if starts_with(fn_code, "dm_simd_splat_f64x4(") { return "dm_f64x4" }
    if starts_with(fn_code, "dm_simd_splat_i32x4(") { return "dm_i32x4" }
    if starts_with(fn_code, "dm_simd_splat_i32x8(") { return "dm_i32x8" }
    if starts_with(fn_code, "dm_simd_splat_i64x2(") { return "dm_i64x2" }
    if starts_with(fn_code, "dm_simd_splat_i64x4(") { return "dm_i64x4" }
    -- SIMD set builtins
    if starts_with(fn_code, "dm_simd_set_f32x4(") { return "dm_f32x4" }
    if starts_with(fn_code, "dm_simd_set_f32x8(") { return "dm_f32x8" }
    if starts_with(fn_code, "dm_simd_set_f64x2(") { return "dm_f64x2" }
    if starts_with(fn_code, "dm_simd_set_f64x4(") { return "dm_f64x4" }
    if starts_with(fn_code, "dm_simd_set_i32x4(") { return "dm_i32x4" }
    if starts_with(fn_code, "dm_simd_set_i32x8(") { return "dm_i32x8" }
    if starts_with(fn_code, "dm_simd_set_i64x2(") { return "dm_i64x2" }
    if starts_with(fn_code, "dm_simd_set_i64x4(") { return "dm_i64x4" }
    return ""
}

fn infer_fn_call_type(code: string, c: Compiler) -> string {
    if starts_with(code, "dm_") == false { return "" }
    if string_contains(code, "(") == false { return "" }
    -- Check builtin return types first
    let builtin_ret = infer_builtin_ret_type(code)
    if builtin_ret != "" { return builtin_ret }
    let paren_pos = string_find(code, "(")
    let fn_name = substr(code, 3, paren_pos - 3)
    let ret = lookup_fn_ret_type(c, fn_name)
    if ret == "" { return "" }
    if ret == "void" { return "" }
    return ret
}

fn infer_struct_lit_type(code: string) -> string {
    if starts_with(code, "(dm_") == false { return "" }
    if string_contains(code, "){") == false { return "" }
    let close = string_find(code, ")")
    let type_name = substr(code, 1, close - 1)
    if string_contains(type_name, " ") { return "" }
    return type_name
}

fn infer_enum_ctor_type(code: string, c: Compiler) -> string {
    -- Detect enum constructor: dm_EnumName_Variant(...) or dm_EnumName_Variant()
    if starts_with(code, "dm_") == false { return "" }
    let paren_pos = string_find(code, "(")
    if paren_pos < 0 { return "" }
    let ctor_name = substr(code, 3, paren_pos - 3)
    -- Check each enum name to see if ctor_name starts with it + "_"
    let mut i = 0
    while i < c.enum_names.len() {
        let ename = "" + c.enum_names[i]
        let prefix = ename + "_"
        if starts_with(ctor_name, prefix) {
            return "dm_" + ename
        }
        i = i + 1
    }
    -- Check for Option/Result constructors (flattened to avoid nested if issue)
    if starts_with(code, "dm_option_") and string_contains(code, "_Some(") {
        let pos = string_find(code, "_Some(")
        return substr(code, 0, pos)
    }
    if starts_with(code, "dm_option_") and string_contains(code, "_None(") {
        let pos = string_find(code, "_None(")
        return substr(code, 0, pos)
    }
    if starts_with(code, "dm_result_") and string_contains(code, "_Ok(") {
        let pos = string_find(code, "_Ok(")
        return substr(code, 0, pos)
    }
    if starts_with(code, "dm_result_") and string_contains(code, "_Err(") {
        let pos = string_find(code, "_Err(")
        return substr(code, 0, pos)
    }
    return ""
}

fn infer_map_method_type(code: string, c: Compiler) -> string {
    -- Match dm_map_*_keys(, dm_map_*_values(, dm_map_*_get(, dm_map_*_contains(, etc.
    if starts_with(code, "dm_map_") == false { return "" }
    if string_contains(code, "(") == false { return "" }
    let paren_pos = string_find(code, "(")
    let call_name = substr(code, 0, paren_pos)
    -- Check for _keys suffix
    if ends_with(call_name, "_keys") {
        let map_type = substr(call_name, 0, len(call_name) - 5)
        let key_t = lookup_map_key_type(c, map_type)
        if key_t != "" { return "dm_list_" + key_t }
    }
    -- Check for _values suffix
    if ends_with(call_name, "_values") {
        let map_type = substr(call_name, 0, len(call_name) - 7)
        let val_t = lookup_map_val_type(c, map_type)
        if val_t != "" { return "dm_list_" + val_t }
    }
    -- Check for _get suffix
    if ends_with(call_name, "_get") {
        let map_type = substr(call_name, 0, len(call_name) - 4)
        let val_t = lookup_map_val_type(c, map_type)
        if val_t != "" { return val_t }
    }
    -- Check for _contains suffix
    if ends_with(call_name, "_contains") {
        return "bool"
    }
    -- Check for _remove suffix
    if ends_with(call_name, "_remove") {
        return "bool"
    }
    -- Check for _new suffix
    if ends_with(call_name, "_new") {
        let map_type = substr(call_name, 0, len(call_name) - 4)
        if starts_with(map_type, "dm_map_") {
            return map_type
        }
    }
    return ""
}

fn is_simd_type(t: string) -> bool {
    if t == "dm_f32x4" { return true }
    if t == "dm_f32x8" { return true }
    if t == "dm_f64x2" { return true }
    if t == "dm_f64x4" { return true }
    if t == "dm_i32x4" { return true }
    if t == "dm_i32x8" { return true }
    if t == "dm_i64x2" { return true }
    if t == "dm_i64x4" { return true }
    return false
}

fn simd_scalar_type(t: string) -> string {
    if t == "dm_f32x4" { return "float" }
    if t == "dm_f32x8" { return "float" }
    if t == "dm_f64x2" { return "double" }
    if t == "dm_f64x4" { return "double" }
    if t == "dm_i32x4" { return "int32_t" }
    if t == "dm_i32x8" { return "int32_t" }
    if t == "dm_i64x2" { return "int64_t" }
    if t == "dm_i64x4" { return "int64_t" }
    return ""
}

-- Infer SIMD vector type from binary expression like (dm_a + dm_b)
fn infer_simd_binop_type(code: string, c: Compiler) -> string {
    let code_len = len(code)
    if code_len < 5 { return "" }
    if char_at(code, 0) != "(" { return "" }
    if char_at(code, code_len - 1) != ")" { return "" }
    -- Extract inner content between outer parens
    let inner = substr(code, 1, code_len - 2)
    -- Find operator: look for " + " or " - " or " * " or " / "
    let mut op_pos = string_find(inner, " + ")
    if op_pos < 0 { op_pos = string_find(inner, " - ") }
    if op_pos < 0 { op_pos = string_find(inner, " * ") }
    if op_pos < 0 { op_pos = string_find(inner, " / ") }
    if op_pos < 0 { return "" }
    let lhs = substr(inner, 0, op_pos)
    let lhs_type = lookup_var_type(c, lhs)
    if is_simd_type(lhs_type) { return lhs_type }
    return ""
}

-- Infer scalar type from SIMD extract: dm_var[idx]
fn infer_simd_extract_type(code: string, c: Compiler) -> string {
    let bracket_pos = string_find(code, "[")
    if bracket_pos < 1 { return "" }
    if ends_with(code, "]") == false { return "" }
    let var_name = substr(code, 0, bracket_pos)
    let var_type = lookup_var_type(c, var_name)
    if is_simd_type(var_type) {
        return simd_scalar_type(var_type)
    }
    return ""
}

fn infer_type_from_code(code: string, c: Compiler) -> string {
    if code_is_string(code, c) { return "dm_string" }
    if code == "true" { return "bool" }
    if code == "false" { return "bool" }
    -- Struct literal: (dm_TypeName){ ... }
    let maybe_struct_type = infer_struct_lit_type(code)
    if maybe_struct_type != "" { return maybe_struct_type }
    -- Enum constructor: dm_EnumName_Variant(...)
    let maybe_enum_type = infer_enum_ctor_type(code, c)
    if maybe_enum_type != "" { return maybe_enum_type }
    -- List element access: DM_LIST_GET(dm_var, idx)
    let list_elem = infer_list_get_type(code, c)
    if list_elem != "" { return list_elem }
    -- Map method calls: dm_map_K_V_keys/values/get/contains/new
    let map_ret = infer_map_method_type(code, c)
    if map_ret != "" { return map_ret }
    -- Function call: dm_foo(...) - look up return type
    let fn_ret = infer_fn_call_type(code, c)
    if fn_ret != "" { return fn_ret }
    -- Variable reference: look up known variable types
    let var_type = lookup_var_type(c, code)
    if var_type != "" { return var_type }
    -- SIMD binary operation: (dm_a + dm_b) etc.
    let simd_binop = infer_simd_binop_type(code, c)
    if simd_binop != "" { return simd_binop }
    -- SIMD extract: dm_var[idx]
    let simd_extract = infer_simd_extract_type(code, c)
    if simd_extract != "" { return simd_extract }
    -- Ternary expression: infer from first branch
    let ternary_type = infer_ternary_type(code, c)
    if ternary_type != "" { return ternary_type }
    -- Default fallback (uncomment for debugging):
    -- eprintln("warning: defaulting to int64_t for: " + code)
    return "int64_t"
}

fn infer_ternary_type(code: string, c: Compiler) -> string {
    if starts_with(code, "(") == false { return "" }
    if string_contains(code, " ? ") == false { return "" }
    let q_pos = string_find(code, " ? ")
    let after_q = substr(code, q_pos + 3, len(code) - q_pos - 3)
    let colon_pos = string_find(after_q, " : ")
    if colon_pos < 0 { return "" }
    let branch = substr(after_q, 0, colon_pos)
    let branch_type = infer_type_from_code(branch, c)
    if branch_type != "int64_t" { return branch_type }
    return ""
}

fn track_str_var(c: Compiler, mangled_name: string, is_str: bool) -> Compiler {
    let mut cc = c
    if is_str == false { return cc }
    cc.str_vars.push(mangled_name)
    return cc
}

fn track_list_elem_type(c: Compiler, type_str: string, var_name: string) -> Compiler {
    let mut cc = c
    if starts_with(type_str, "dm_list_") == false { return cc }
    let elem_t = substr(type_str, 8, len(type_str) - 8)
    let mname = dm_mangle(var_name)
    cc.list_elem_types = cc.list_elem_types + "|" + mname + "=" + elem_t + "|"
    return cc
}

fn compile_let_list_init(c: Compiler, ind: string, type_str: string, var_name: string) -> Compiler {
    let mut cc = c_advance(c)  -- skip '['
    cc = track_list_elem_type(cc, type_str, var_name)
    cc = track_var_type(cc, dm_mangle(var_name), type_str)
    let mname = dm_mangle(var_name)
    if c_peek(cc) == TK_RBRACKET() {
        -- Empty list: []
        cc = c_advance(cc)
        cc.output = cc.output + ind + type_str + " " + mname + " = " + type_str + "_new();\n"
        return cc
    }
    -- Non-empty list literal: [a, b, c]
    cc.output = cc.output + ind + type_str + " " + mname + " = " + type_str + "_new();\n"
    let mut first_el = true
    while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
        if first_el == false {
            cc = c_expect(cc, TK_COMMA())
        }
        first_el = false
        let elem = compile_expr(cc)
        cc = elem.c
        cc.output = cc.output + ind + type_str + "_push(&" + mname + ", " + elem.code + ");\n"
    }
    cc = c_expect(cc, TK_RBRACKET())
    return cc
}

fn compile_let_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'let'
    let ind = indent_str(cc.indent)
    let mut is_mut = false
    if c_peek(cc) == TK_MUT() {
        is_mut = true
        cc = c_advance(cc)
    }
    let name_tok = c_cur(cc)
    let var_name = name_tok.value
    cc = c_advance(cc)
    -- Optional type annotation
    let mut type_str = ""
    let mut has_type_ann = false
    if c_peek(cc) == TK_COLON() {
        cc = c_advance(cc)
        let tr = parse_type_for_c(cc)
        cc = tr.c
        type_str = tr.code
        has_type_ann = true
    }
    -- = value
    cc = c_expect(cc, TK_EQ())
    -- Handle list literals: [] or [a, b, c] (but not array types [T; N])
    if c_peek(cc) == TK_LBRACKET() and starts_with(type_str, "dm_array_") == false {
        return compile_let_list_init(cc, ind, type_str, var_name)
    }
    -- Set expected type for Some/None/Ok/Err inference
    let saved_expected = cc.expected_type
    if has_type_ann {
        cc.expected_type = type_str
    }
    let val = compile_expr(cc)
    cc = val.c
    cc.expected_type = saved_expected
    -- Infer type from value if no annotation
    if has_type_ann == false {
        type_str = infer_type_from_code(val.code, cc)
    }
    -- Track string variables (use a flag to avoid ternary-in-if issue)
    let is_string_type = type_str == "dm_string"
    cc = track_str_var(cc, dm_mangle(var_name), is_string_type)
    -- Track list variable element types
    cc = track_list_elem_type(cc, type_str, var_name)
    -- Track variable type for inference
    cc = track_var_type(cc, dm_mangle(var_name), type_str)
    -- Function pointer types need special declaration syntax: ret (*name)(params)
    let is_fptr = string_contains(type_str, "(*)")
    if is_fptr {
        let decl = emit_fptr_decl(type_str, dm_mangle(var_name))
        cc.output = cc.output + ind + decl + " = " + val.code + ";\n"
    } else {
        cc.output = cc.output + ind + type_str + " " + dm_mangle(var_name) + " = " + val.code + ";\n"
    }
    return cc
}

struct TypeCResult {
    c: Compiler,
    code: string
}

fn register_list_type(c: Compiler, list_type: string, elem_type: string) -> Compiler {
    let mut cc = c
    -- Check if already registered (search in the defs string)
    if string_contains(cc.list_type_defs, "} " + list_type + ";") { return cc }
    cc.list_type_defs = cc.list_type_defs + emit_list_type_def(list_type, elem_type)
    return cc
}

fn register_map_type(c: Compiler, map_type: string, key_type: string, val_type: string) -> Compiler {
    let mut cc = c
    -- Check if already registered
    if string_contains(cc.map_type_defs, "} " + map_type + ";") { return cc }
    -- Also register List types for keys() and values()
    let key_list_t = "dm_list_" + key_type
    cc = register_list_type(cc, key_list_t, key_type)
    let val_list_t = "dm_list_" + val_type
    cc = register_list_type(cc, val_list_t, val_type)
    cc.map_type_defs = cc.map_type_defs + emit_map_type_def(map_type, key_type, val_type)
    -- Track map key/value types for iteration and method dispatch
    cc.map_kv_types = cc.map_kv_types + "|" + map_type + "=" + key_type + ":" + val_type + "|"
    return cc
}

fn lookup_map_key_type(c: Compiler, map_type: string) -> string {
    let marker = "|" + map_type + "="
    let pos = string_find(c.map_kv_types, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.map_kv_types, start, len(c.map_kv_types) - start)
    let colon_pos = string_find(rest, ":")
    if colon_pos < 0 { return "" }
    return substr(rest, 0, colon_pos)
}

fn lookup_map_val_type(c: Compiler, map_type: string) -> string {
    let marker = "|" + map_type + "="
    let pos = string_find(c.map_kv_types, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.map_kv_types, start, len(c.map_kv_types) - start)
    let colon_pos = string_find(rest, ":")
    if colon_pos < 0 { return "" }
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, colon_pos + 1, end_pos - colon_pos - 1)
}

fn emit_map_type_def(map_type: string, key_type: string, val_type: string) -> string {
    let is_string_key = key_type == "dm_string"
    let key_list_t = "dm_list_" + key_type
    let val_list_t = "dm_list_" + val_type
    -- Entry struct
    let mut d = "typedef struct " + map_type + "_entry {\n"
    d = d + "    " + key_type + " key;\n"
    d = d + "    " + val_type + " value;\n"
    d = d + "    int state;  /* 0=empty, 1=occupied, 2=tombstone */\n"
    d = d + "} " + map_type + "_entry;\n\n"
    -- Map struct
    d = d + "typedef struct " + map_type + " {\n"
    d = d + "    " + map_type + "_entry* entries;\n"
    d = d + "    size_t len;\n"
    d = d + "    size_t capacity;\n"
    d = d + "} " + map_type + ";\n\n"
    -- Hash function
    d = d + "static inline size_t " + map_type + "_hash(" + key_type + " key) {\n"
    if is_string_key {
        d = d + "    size_t h = 14695981039346656037ULL;\n"
        d = d + "    for (size_t i = 0; i < key.len; i++) {\n"
        d = d + "        h ^= (unsigned char)key.data[i];\n"
        d = d + "        h *= 1099511628211ULL;\n"
        d = d + "    }\n"
        d = d + "    return h;\n"
    } else {
        d = d + "    uint64_t x = (uint64_t)key;\n"
        d = d + "    x ^= x >> 30;\n"
        d = d + "    x *= 0xbf58476d1ce4e5b9ULL;\n"
        d = d + "    x ^= x >> 27;\n"
        d = d + "    x *= 0x94d049bb133111ebULL;\n"
        d = d + "    x ^= x >> 31;\n"
        d = d + "    return (size_t)x;\n"
    }
    d = d + "}\n\n"
    -- Key equality
    d = d + "static inline bool " + map_type + "_key_eq(" + key_type + " a, " + key_type + " b) {\n"
    if is_string_key {
        d = d + "    return dm_string_eq(a, b);\n"
    } else {
        d = d + "    return a == b;\n"
    }
    d = d + "}\n\n"
    -- _new
    d = d + "static inline " + map_type + " " + map_type + "_new(void) {\n"
    d = d + "    return (" + map_type + "){ .entries = NULL, .len = 0, .capacity = 0 };\n"
    d = d + "}\n\n"
    -- _find_slot
    d = d + "static inline size_t " + map_type + "_find_slot(" + map_type + "* map, " + key_type + " key) {\n"
    d = d + "    size_t idx = " + map_type + "_hash(key) & (map->capacity - 1);\n"
    d = d + "    size_t first_tombstone = (size_t)-1;\n"
    d = d + "    for (size_t i = 0; i < map->capacity; i++) {\n"
    d = d + "        if (map->entries[idx].state == 0) {\n"
    d = d + "            return (first_tombstone != (size_t)-1) ? first_tombstone : idx;\n"
    d = d + "        }\n"
    d = d + "        if (map->entries[idx].state == 1 && " + map_type + "_key_eq(map->entries[idx].key, key)) {\n"
    d = d + "            return idx;\n"
    d = d + "        }\n"
    d = d + "        if (map->entries[idx].state == 2 && first_tombstone == (size_t)-1) {\n"
    d = d + "            first_tombstone = idx;\n"
    d = d + "        }\n"
    d = d + "        idx = (idx + 1) & (map->capacity - 1);\n"
    d = d + "    }\n"
    d = d + "    return (first_tombstone != (size_t)-1) ? first_tombstone : idx;\n"
    d = d + "}\n\n"
    -- _resize
    d = d + "static inline void " + map_type + "_resize(" + map_type + "* map) {\n"
    d = d + "    size_t new_cap = map->capacity == 0 ? 16 : map->capacity * 2;\n"
    d = d + "    " + map_type + "_entry* new_entries = (" + map_type + "_entry*)calloc(new_cap, sizeof(" + map_type + "_entry));\n"
    d = d + "    if (!new_entries) dm_panic_cstr(\"map resize: out of memory\");\n"
    d = d + "    " + map_type + " new_map = { .entries = new_entries, .len = 0, .capacity = new_cap };\n"
    d = d + "    for (size_t i = 0; i < map->capacity; i++) {\n"
    d = d + "        if (map->entries[i].state == 1) {\n"
    d = d + "            size_t slot = " + map_type + "_find_slot(&new_map, map->entries[i].key);\n"
    d = d + "            new_map.entries[slot].key = map->entries[i].key;\n"
    d = d + "            new_map.entries[slot].value = map->entries[i].value;\n"
    d = d + "            new_map.entries[slot].state = 1;\n"
    d = d + "            new_map.len++;\n"
    d = d + "        }\n"
    d = d + "    }\n"
    d = d + "    free(map->entries);\n"
    d = d + "    map->entries = new_entries;\n"
    d = d + "    map->capacity = new_cap;\n"
    d = d + "}\n\n"
    -- _insert
    d = d + "static inline void " + map_type + "_insert(" + map_type + "* map, " + key_type + " key, " + val_type + " value) {\n"
    d = d + "    if (map->capacity == 0 || (map->len + 1) * 4 > map->capacity * 3) {\n"
    d = d + "        " + map_type + "_resize(map);\n"
    d = d + "    }\n"
    d = d + "    size_t slot = " + map_type + "_find_slot(map, key);\n"
    d = d + "    if (map->entries[slot].state != 1) {\n"
    d = d + "        map->len++;\n"
    d = d + "    }\n"
    d = d + "    map->entries[slot].key = key;\n"
    d = d + "    map->entries[slot].value = value;\n"
    d = d + "    map->entries[slot].state = 1;\n"
    d = d + "}\n\n"
    -- _get
    d = d + "static inline " + val_type + " " + map_type + "_get(" + map_type + "* map, " + key_type + " key) {\n"
    d = d + "    if (map->capacity == 0) dm_panic_cstr(\"map get: key not found\");\n"
    d = d + "    size_t slot = " + map_type + "_find_slot(map, key);\n"
    d = d + "    if (map->entries[slot].state != 1) dm_panic_cstr(\"map get: key not found\");\n"
    d = d + "    return map->entries[slot].value;\n"
    d = d + "}\n\n"
    -- _contains
    d = d + "static inline bool " + map_type + "_contains(" + map_type + "* map, " + key_type + " key) {\n"
    d = d + "    if (map->capacity == 0) return false;\n"
    d = d + "    size_t slot = " + map_type + "_find_slot(map, key);\n"
    d = d + "    return map->entries[slot].state == 1;\n"
    d = d + "}\n\n"
    -- _remove
    d = d + "static inline bool " + map_type + "_remove(" + map_type + "* map, " + key_type + " key) {\n"
    d = d + "    if (map->capacity == 0) return false;\n"
    d = d + "    size_t slot = " + map_type + "_find_slot(map, key);\n"
    d = d + "    if (map->entries[slot].state != 1) return false;\n"
    d = d + "    map->entries[slot].state = 2;\n"
    d = d + "    map->len--;\n"
    d = d + "    return true;\n"
    d = d + "}\n\n"
    -- _keys
    d = d + "static inline " + key_list_t + " " + map_type + "_keys(" + map_type + "* map) {\n"
    d = d + "    " + key_list_t + " result = " + key_list_t + "_new();\n"
    d = d + "    for (size_t i = 0; i < map->capacity; i++) {\n"
    d = d + "        if (map->entries[i].state == 1) DM_LIST_PUSH(result, map->entries[i].key);\n"
    d = d + "    }\n"
    d = d + "    return result;\n"
    d = d + "}\n\n"
    -- _values
    d = d + "static inline " + val_list_t + " " + map_type + "_values(" + map_type + "* map) {\n"
    d = d + "    " + val_list_t + " result = " + val_list_t + "_new();\n"
    d = d + "    for (size_t i = 0; i < map->capacity; i++) {\n"
    d = d + "        if (map->entries[i].state == 1) DM_LIST_PUSH(result, map->entries[i].value);\n"
    d = d + "    }\n"
    d = d + "    return result;\n"
    d = d + "}\n\n"
    -- _set (alias for _insert)
    d = d + "static inline void " + map_type + "_set(" + map_type + "* map, " + key_type + " key, " + val_type + " value) {\n"
    d = d + "    " + map_type + "_insert(map, key, value);\n"
    d = d + "}\n\n"
    return d
}

fn emit_option_type_def(opt_type: string, val_type: string) -> string {
    let mut d = "typedef enum " + opt_type + "_tag {\n"
    d = d + "    " + opt_type + "_tag_None,\n"
    d = d + "    " + opt_type + "_tag_Some\n"
    d = d + "} " + opt_type + "_tag;\n\n"
    d = d + "typedef struct " + opt_type + " {\n"
    d = d + "    " + opt_type + "_tag tag;\n"
    d = d + "    union { struct { " + val_type + " _0; } Some; } data;\n"
    d = d + "} " + opt_type + ";\n\n"
    d = d + "static inline " + opt_type + " " + opt_type + "_None(void) {\n"
    d = d + "    " + opt_type + " _r; _r.tag = " + opt_type + "_tag_None; return _r;\n"
    d = d + "}\n\n"
    d = d + "static inline " + opt_type + " " + opt_type + "_Some(" + val_type + " _0) {\n"
    d = d + "    " + opt_type + " _r; _r.tag = " + opt_type + "_tag_Some; _r.data.Some._0 = _0; return _r;\n"
    d = d + "}\n\n"
    return d
}

fn emit_result_type_def(res_type: string, ok_type: string, err_type: string) -> string {
    let mut d = "typedef enum " + res_type + "_tag {\n"
    d = d + "    " + res_type + "_tag_Ok,\n"
    d = d + "    " + res_type + "_tag_Err\n"
    d = d + "} " + res_type + "_tag;\n\n"
    d = d + "typedef struct " + res_type + " {\n"
    d = d + "    " + res_type + "_tag tag;\n"
    d = d + "    union {\n"
    d = d + "        struct { " + ok_type + " _0; } Ok;\n"
    d = d + "        struct { " + err_type + " _0; } Err;\n"
    d = d + "    } data;\n"
    d = d + "} " + res_type + ";\n\n"
    d = d + "static inline " + res_type + " " + res_type + "_Ok(" + ok_type + " _0) {\n"
    d = d + "    " + res_type + " _r; _r.tag = " + res_type + "_tag_Ok; _r.data.Ok._0 = _0; return _r;\n"
    d = d + "}\n\n"
    d = d + "static inline " + res_type + " " + res_type + "_Err(" + err_type + " _0) {\n"
    d = d + "    " + res_type + " _r; _r.tag = " + res_type + "_tag_Err; _r.data.Err._0 = _0; return _r;\n"
    d = d + "}\n\n"
    return d
}

fn emit_future_type_def(future_type: string, val_type: string) -> string {
    let mut d = "typedef struct " + future_type + " {\n"
    d = d + "    " + val_type + " value;\n"
    d = d + "    bool _ready;\n"
    d = d + "} " + future_type + ";\n\n"
    d = d + "static inline " + future_type + " " + future_type + "_ready(" + val_type + " val) {\n"
    d = d + "    return (" + future_type + "){ .value = val, ._ready = true };\n"
    d = d + "}\n\n"
    d = d + "static inline " + val_type + " " + future_type + "_await(" + future_type + " f) {\n"
    d = d + "    return f.value;\n"
    d = d + "}\n\n"
    return d
}

fn register_future_type(c: Compiler, future_type: string, val_type: string) -> Compiler {
    let mut cc = c
    if string_contains(cc.future_type_defs, "} " + future_type + ";") { return cc }
    cc.future_type_defs = cc.future_type_defs + emit_future_type_def(future_type, val_type)
    return cc
}

fn register_option_type(c: Compiler, opt_type: string, val_type: string) -> Compiler {
    let mut cc = c
    if string_contains(cc.option_type_defs, "} " + opt_type + ";") { return cc }
    cc.option_type_defs = cc.option_type_defs + emit_option_type_def(opt_type, val_type)
    -- Register enum variants for match/constructor use
    cc.enum_variants = cc.enum_variants + "|" + opt_type + ".None=unit|"
    cc.enum_variants = cc.enum_variants + "|" + opt_type + ".Some=tuple:" + val_type + "|"
    return cc
}

fn register_result_type(c: Compiler, res_type: string, ok_type: string, err_type: string) -> Compiler {
    let mut cc = c
    if string_contains(cc.option_type_defs, "} " + res_type + ";") { return cc }
    cc.option_type_defs = cc.option_type_defs + emit_result_type_def(res_type, ok_type, err_type)
    -- Register enum variants for match/constructor use
    cc.enum_variants = cc.enum_variants + "|" + res_type + ".Ok=tuple:" + ok_type + "|"
    cc.enum_variants = cc.enum_variants + "|" + res_type + ".Err=tuple:" + err_type + "|"
    return cc
}

-- Generate vtable struct, fat pointer struct, and dispatch functions for a dyn trait
fn generate_dyn_trait_defs(c: Compiler, trait_name: string) -> Compiler {
    let mut cc = c
    let marker = "|" + trait_name + "|"
    if string_contains(cc.dyn_traits_generated, marker) { return cc }
    cc.dyn_traits_generated = cc.dyn_traits_generated + marker
    let mangled = "dm_" + trait_name
    let dyn_type = "dm_dyn_" + trait_name
    -- Collect all methods for this trait from trait_methods
    -- Format: "|TraitName.method=ret_type:param_types|"
    -- We need to iterate over all methods for this trait
    let mut vtable_fields = ""
    let mut dispatch_fns = ""
    let search_prefix = "|" + trait_name + "."
    let mut spos = string_find(cc.trait_methods, search_prefix)
    while spos >= 0 {
        let after_prefix = spos + len(search_prefix)
        let rest = substr(cc.trait_methods, after_prefix, len(cc.trait_methods) - after_prefix)
        let eq_pos = string_find(rest, "=")
        if eq_pos >= 0 {
            let method_name = substr(rest, 0, eq_pos)
            let after_eq = substr(rest, eq_pos + 1, len(rest) - eq_pos - 1)
            let pipe_pos = string_find(after_eq, "|")
            let mut sig_str = after_eq
            if pipe_pos >= 0 {
                sig_str = substr(after_eq, 0, pipe_pos)
            }
            -- sig_str is "ret_type:param_types" (param_types may be empty)
            let colon_pos = string_find(sig_str, ":")
            let mut ret_type = "void"
            let mut param_types_str = ""
            if colon_pos >= 0 {
                ret_type = substr(sig_str, 0, colon_pos)
                param_types_str = substr(sig_str, colon_pos + 1, len(sig_str) - colon_pos - 1)
            }
            -- Build vtable function pointer field: ret_type (*method_name)(void* self, extra_params...)
            let mut fptr_params = "void*"
            if param_types_str != "" {
                fptr_params = fptr_params + ", " + param_types_str
            }
            vtable_fields = vtable_fields + "    " + ret_type + " (*" + method_name + ")(" + fptr_params + ");\n"
            -- Build dispatch function
            let mut dispatch_params = dyn_type + " self"
            let mut call_args = "self.data"
            if param_types_str != "" {
                -- Parse extra param types and add named params
                let mut pt_rest = param_types_str
                let mut pi = 0
                let mut pt_done = false
                while pt_done == false {
                    let comma = string_find(pt_rest, ",")
                    let mut ptype = pt_rest
                    if comma >= 0 {
                        ptype = substr(pt_rest, 0, comma)
                        pt_rest = substr(pt_rest, comma + 1, len(pt_rest) - comma - 1)
                    } else {
                        pt_done = true
                    }
                    dispatch_params = dispatch_params + ", " + ptype + " _p" + int_to_string(pi)
                    call_args = call_args + ", _p" + int_to_string(pi)
                    pi = pi + 1
                }
            }
            dispatch_fns = dispatch_fns + "static " + ret_type + " " + dyn_type + "_" + method_name + "(" + dispatch_params + ") {\n"
            if ret_type == "void" {
                dispatch_fns = dispatch_fns + "    self.vtable->" + method_name + "(" + call_args + ");\n"
            } else {
                dispatch_fns = dispatch_fns + "    return self.vtable->" + method_name + "(" + call_args + ");\n"
            }
            dispatch_fns = dispatch_fns + "}\n\n"
        }
        -- Search for next method
        let next_search_start = spos + len(search_prefix)
        let next_rest = substr(cc.trait_methods, next_search_start, len(cc.trait_methods) - next_search_start)
        let next_found = string_find(next_rest, search_prefix)
        if next_found < 0 {
            spos = 0 - 1
        } else {
            spos = next_search_start + next_found
        }
    }
    -- Build the vtable struct
    let mut defs = "// dyn " + trait_name + " support\n"
    defs = defs + "typedef struct " + mangled + "_vtable {\n"
    defs = defs + vtable_fields
    defs = defs + "} " + mangled + "_vtable;\n\n"
    -- Build the fat pointer struct
    defs = defs + "typedef struct " + dyn_type + " {\n"
    defs = defs + "    void* data;\n"
    defs = defs + "    " + mangled + "_vtable* vtable;\n"
    defs = defs + "} " + dyn_type + ";\n\n"
    -- Add dispatch functions
    defs = defs + dispatch_fns
    cc.dyn_trait_defs = cc.dyn_trait_defs + defs
    return cc
}

-- Look up all impl-for mappings for a given trait name, returning the concrete types
-- (searches impl_for_map for "|TypeName=TraitName|" patterns)
fn lookup_impl_type_for_trait(c: Compiler, type_name: string, trait_name: string) -> bool {
    let marker = "|" + type_name + "=" + trait_name + "|"
    return string_contains(c.impl_for_map, marker)
}

fn register_array_type(c: Compiler, elem_type: string, size_str: string) -> Compiler {
    let mut cc = c
    let array_type = "dm_array_" + elem_type + "_" + size_str
    let marker = "} " + array_type + ";"
    if string_contains(cc.array_type_defs, marker) { return cc }
    cc.array_type_defs = cc.array_type_defs + "typedef " + elem_type + " " + array_type + "[" + size_str + "];\n"
    return cc
}

fn parse_type_for_c(c: Compiler) -> TypeCResult {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)
    -- Handle array type: [T; N]
    if tok.kind == TK_LBRACKET() {
        cc = c_advance(cc)
        let inner = parse_type_for_c(cc)
        cc = inner.c
        cc = c_expect(cc, TK_SEMICOLON())
        let size_tok = c_cur(cc)
        let size_str = size_tok.value
        cc = c_advance(cc)
        cc = c_expect(cc, TK_RBRACKET())
        let array_type = "dm_array_" + inner.code + "_" + size_str
        cc = register_array_type(cc, inner.code, size_str)
        return TypeCResult { c: cc, code: array_type }
    }
    if tok.kind != TK_IDENT() {
        return TypeCResult { c: cc, code: "int64_t" }
    }
    let name = tok.value
    cc = c_advance(cc)
    -- Handle dyn TraitName
    if name == "dyn" {
        let trait_tok = c_cur(cc)
        if trait_tok.kind == TK_IDENT() {
            let trait_name = trait_tok.value
            cc = c_advance(cc)
            cc = generate_dyn_trait_defs(cc, trait_name)
            return TypeCResult { c: cc, code: "dm_dyn_" + trait_name }
        }
        return TypeCResult { c: cc, code: "int64_t" }
    }
    -- Check for generic [T] or [T, E]
    if c_peek(cc) == TK_LBRACKET() {
        cc = c_advance(cc)
        let inner = parse_type_for_c(cc)
        cc = inner.c
        -- Check for second type arg (Result[T, E])
        let mut second_type = ""
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
            let inner2 = parse_type_for_c(cc)
            cc = inner2.c
            second_type = inner2.code
        }
        cc = c_expect(cc, TK_RBRACKET())
        if name == "List" {
            let list_t = "dm_list_" + inner.code
            cc = register_list_type(cc, list_t, inner.code)
            return TypeCResult { c: cc, code: list_t }
        }
        if name == "Box" {
            return TypeCResult { c: cc, code: inner.code + "*" }
        }
        if name == "Option" {
            let opt_t = "dm_option_" + inner.code
            cc = register_option_type(cc, opt_t, inner.code)
            return TypeCResult { c: cc, code: opt_t }
        }
        if name == "Result" and second_type != "" {
            let res_t = "dm_result_" + inner.code + "_" + second_type
            cc = register_result_type(cc, res_t, inner.code, second_type)
            return TypeCResult { c: cc, code: res_t }
        }
        if name == "Map" and second_type != "" {
            let map_t = "dm_map_" + inner.code + "_" + second_type
            cc = register_map_type(cc, map_t, inner.code, second_type)
            return TypeCResult { c: cc, code: map_t }
        }
        if name == "Future" {
            let future_t = "dm_future_" + inner.code
            cc = register_future_type(cc, future_t, inner.code)
            return TypeCResult { c: cc, code: future_t }
        }
        return TypeCResult { c: cc, code: "dm_" + name + "_" + inner.code }
    }
    let mapped = map_dm_type(name)
    return TypeCResult { c: cc, code: mapped }
}

fn compile_if_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'if'
    let ind = indent_str(cc.indent)
    let cond = compile_expr(cc)
    cc = cond.c
    cc.output = cc.output + ind + "if (" + cond.code + ") {\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}"
    -- Check for else
    let mut cc2 = c_skip_nl(cc)
    if c_peek(cc2) != TK_ELSE() {
        cc.output = cc.output + "\n"
        return cc
    }
    cc2 = c_advance(cc2)
    cc2 = c_skip_nl(cc2)
    if c_peek(cc2) == TK_IF() {
        cc2.output = cc2.output + " else "
        return compile_if_stmt(cc2)
    }
    cc2.output = cc2.output + " else {\n"
    cc2 = c_expect(cc2, TK_LBRACE())
    cc2.indent = cc2.indent + 1
    cc2 = compile_block_body(cc2)
    cc2 = c_expect(cc2, TK_RBRACE())
    cc2.indent = cc2.indent - 1
    cc2.output = cc2.output + ind + "}\n"
    return cc2
}

fn compile_while_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'while'
    let ind = indent_str(cc.indent)
    let cond = compile_expr(cc)
    cc = cond.c
    cc.output = cc.output + ind + "while (" + cond.code + ") {\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}\n"
    return cc
}

fn compile_for_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'for'
    let ind = indent_str(cc.indent)
    let var_tok = c_cur(cc)
    let var_name = var_tok.value
    cc = c_advance(cc)
    cc = c_expect(cc, TK_IN())
    let start_expr = compile_expr(cc)
    cc = start_expr.c
    -- Check for range: start..end or start..=end
    if c_peek(cc) == TK_DOTDOT() {
        cc = c_advance(cc)
        let end_expr = compile_expr(cc)
        cc = end_expr.c
        cc.output = cc.output + ind + "for (int64_t " + dm_mangle(var_name) + " = " + start_expr.code + "; " + dm_mangle(var_name) + " < " + end_expr.code + "; " + dm_mangle(var_name) + "++) {\n"
        cc = c_expect(cc, TK_LBRACE())
        cc.indent = cc.indent + 1
        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())
        cc.indent = cc.indent - 1
        cc.output = cc.output + ind + "}\n"
        return cc
    }
    if c_peek(cc) == TK_DOTDOTEQ() {
        cc = c_advance(cc)
        let end_expr = compile_expr(cc)
        cc = end_expr.c
        cc.output = cc.output + ind + "for (int64_t " + dm_mangle(var_name) + " = " + start_expr.code + "; " + dm_mangle(var_name) + " <= " + end_expr.code + "; " + dm_mangle(var_name) + "++) {\n"
        cc = c_expect(cc, TK_LBRACE())
        cc.indent = cc.indent + 1
        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())
        cc.indent = cc.indent - 1
        cc.output = cc.output + ind + "}\n"
        return cc
    }
    -- Use unique iterator variable to support nested loops
    let fi_name = "_fi" + int_to_string(cc.for_counter)
    cc.for_counter = cc.for_counter + 1
    -- Infer collection type
    let coll_type = infer_type_from_code(start_expr.code, cc)
    -- Map iteration: for key in map { ... } (iterates over keys)
    if starts_with(coll_type, "dm_map_") {
        let key_type = lookup_map_key_type(cc, coll_type)
        let mut ktype = key_type
        if ktype == "" { ktype = "int64_t" }
        cc.output = cc.output + ind + "for (size_t " + fi_name + " = 0; " + fi_name + " < " + start_expr.code + ".capacity; " + fi_name + "++) {\n"
        cc.output = cc.output + ind + "    if (" + start_expr.code + ".entries[" + fi_name + "].state == 1) {\n"
        cc.output = cc.output + ind + "    " + ktype + " " + dm_mangle(var_name) + " = " + start_expr.code + ".entries[" + fi_name + "].key;\n"
        -- Track the loop variable type
        cc = track_var_type(cc, dm_mangle(var_name), ktype)
        cc = track_str_var(cc, dm_mangle(var_name), ktype == "dm_string")
        cc = c_expect(cc, TK_LBRACE())
        cc.indent = cc.indent + 2
        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())
        cc.indent = cc.indent - 2
        cc.output = cc.output + ind + "    }\n"
        cc.output = cc.output + ind + "}\n"
        return cc
    }
    -- List iteration: for x in list { ... }
    let mut elem_type = "int64_t"
    if starts_with(coll_type, "dm_list_") {
        elem_type = substr(coll_type, 8, len(coll_type) - 8)
    }
    cc.output = cc.output + ind + "for (size_t " + fi_name + " = 0; " + fi_name + " < " + start_expr.code + ".len; " + fi_name + "++) {\n"
    cc.output = cc.output + ind + "    " + elem_type + " " + dm_mangle(var_name) + " = " + start_expr.code + ".data[" + fi_name + "];\n"
    -- Track the loop variable type
    cc = track_var_type(cc, dm_mangle(var_name), elem_type)
    cc = track_str_var(cc, dm_mangle(var_name), elem_type == "dm_string")
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}\n"
    return cc
}

fn compile_loop_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'loop'
    let ind = indent_str(cc.indent)
    cc.output = cc.output + ind + "while (1) {\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}\n"
    return cc
}

fn compile_region_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'region'
    let ind = indent_str(cc.indent)
    -- Parse region name (optional, just skip it)
    if c_peek(cc) == TK_IDENT() {
        cc = c_advance(cc)
    }
    -- Generate unique arena variable name
    let arena_name = "_region_arena" + int_to_string(cc.region_counter)
    cc.region_counter = cc.region_counter + 1
    -- Emit arena create
    cc.output = cc.output + ind + "dm_arena* " + arena_name + " = dm_arena_create(4096);\n"
    -- Set current region arena for allocation redirection
    let saved_arena = cc.current_region_arena
    cc.current_region_arena = arena_name
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    -- Restore previous region arena (supports nesting)
    cc.current_region_arena = saved_arena
    -- Emit arena destroy
    cc.output = cc.output + ind + "dm_arena_destroy(" + arena_name + ");\n"
    return cc
}

fn compile_const_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'const'
    let ind = indent_str(cc.indent)
    let name_tok = c_cur(cc)
    let var_name = name_tok.value
    cc = c_advance(cc)

    -- Optional type annotation
    let mut type_str = ""
    if c_peek(cc) == TK_COLON() {
        cc = c_advance(cc)
        let tr = parse_type_for_c(cc)
        cc = tr.c
        type_str = tr.code
    }

    cc = c_expect(cc, TK_EQ())

    if c_peek(cc) == TK_COMPTIME() {
        cc = c_advance(cc)  -- skip 'comptime'
        let val = eval_comptime_expr(cc)
        cc = val.c
        -- Infer type from value if no annotation
        if type_str == "" {
            if val.code == "true" or val.code == "false" {
                type_str = "bool"
            } else if string_contains(val.code, ".") {
                type_str = "double"
            } else {
                type_str = "int64_t"
            }
        }
        cc.output = cc.output + ind + "const " + type_str + " " + dm_mangle(var_name) + " = " + val.code + ";\n"
        cc = track_var_type(cc, dm_mangle(var_name), type_str)
    } else {
        -- Regular const (non-comptime): compile expression normally
        let val = compile_expr(cc)
        cc = val.c
        if type_str == "" {
            type_str = infer_type_from_code(val.code, cc)
        }
        cc.output = cc.output + ind + "const " + type_str + " " + dm_mangle(var_name) + " = " + val.code + ";\n"
        cc = track_var_type(cc, dm_mangle(var_name), type_str)
    }

    return cc
}

fn compile_block_body(c: Compiler) -> Compiler {
    let mut cc = c
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = compile_stmt(cc)
        cc = c_skip_nl(cc)
    }
    return cc
}
