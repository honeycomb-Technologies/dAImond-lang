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
            cc.output = cc.output + ind + "return " + val.code + ";\n"
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
    -- Expression statement (possibly assignment)
    let lhs = compile_expr(cc)
    cc = lhs.c
    -- Check for assignment
    if c_peek(cc) == TK_EQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = " + rhs.code + ";\n"
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
    -- Function call: dm_foo(...) - look up return type
    let fn_ret = infer_fn_call_type(code, c)
    if fn_ret != "" { return fn_ret }
    -- Variable reference: look up known variable types
    let var_type = lookup_var_type(c, code)
    if var_type != "" { return var_type }
    return "int64_t"
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
    -- Handle list literals: [] or [a, b, c]
    if c_peek(cc) == TK_LBRACKET() {
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

fn parse_type_for_c(c: Compiler) -> TypeCResult {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)
    if tok.kind != TK_IDENT() {
        return TypeCResult { c: cc, code: "int64_t" }
    }
    let name = tok.value
    cc = c_advance(cc)
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
    -- List iteration: for x in list { ... }
    -- Use unique iterator variable to support nested loops
    let fi_name = "_fi" + int_to_string(cc.for_counter)
    cc.for_counter = cc.for_counter + 1
    -- Infer element type from list type
    let list_type = infer_type_from_code(start_expr.code, cc)
    let mut elem_type = "int64_t"
    if starts_with(list_type, "dm_list_") {
        elem_type = substr(list_type, 8, len(list_type) - 8)
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

fn compile_block_body(c: Compiler) -> Compiler {
    let mut cc = c
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = compile_stmt(cc)
        cc = c_skip_nl(cc)
    }
    return cc
}
