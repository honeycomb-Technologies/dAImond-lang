module compile_match

import compiler

-- ============================================================
-- MATCH EXPRESSION/STATEMENT COMPILER
-- ============================================================

fn infer_field_type_from_code(code: string, c: Compiler) -> string {
    let dot_pos = string_find(code, ".")
    if dot_pos < 0 { return "" }
    let var_part = substr(code, 0, dot_pos)
    let field_part = substr(code, dot_pos + 1, len(code) - dot_pos - 1)
    let vtype = lookup_var_type(c, var_part)
    if vtype == "" { return "" }
    return lookup_struct_field_type(c, vtype, field_part)
}

-- Determine the enum type from a match subject C code string
fn infer_match_subject_type(code: string, c: Compiler) -> string {
    -- Look up variable type from var_types
    let vt = lookup_var_type(c, code)
    if vt != "" { return vt }
    -- Try struct field access (delegated to helper to avoid nested-if codegen bug)
    let field_result = infer_field_type_from_code(code, c)
    if field_result != "" { return field_result }
    -- Try function call inference
    let fn_ret = infer_fn_call_type(code, c)
    if fn_ret != "" { return fn_ret }
    return ""
}

-- Compile a match arm pattern and return the condition code and any bindings
-- Pattern kinds:
--   EnumName.Variant(x, y) -> tag check + bindings
--   EnumName.Variant -> tag check (unit)
--   _ -> wildcard (always matches)
--   literal int/string/bool -> equality check
--   ident -> catch-all binding
struct MatchArm {
    c: Compiler,
    condition: string,
    bindings: string
}

fn compile_match_pattern(c: Compiler, subject_code: string, subject_type: string) -> MatchArm {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)

    -- Wildcard: _
    if tok.kind == TK_UNDERSCORE() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "", bindings: "" }
    }

    -- Literal integer
    if tok.kind == TK_INTEGER() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "(" + subject_code + " == " + tok.value + ")", bindings: "" }
    }

    -- Literal string
    if tok.kind == TK_STRING() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "dm_string_eq(" + subject_code + ", dm_string_from_cstr(\"" + tok.value + "\"))", bindings: "" }
    }

    -- true/false
    if tok.kind == TK_TRUE() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "(" + subject_code + " == true)", bindings: "" }
    }
    if tok.kind == TK_FALSE() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "(" + subject_code + " == false)", bindings: "" }
    }

    -- Identifier: could be EnumName.Variant or catch-all binding
    if tok.kind == TK_IDENT() {
        let name = tok.value
        cc = c_advance(cc)

        -- Check for EnumName.Variant pattern
        if c_peek(cc) == TK_DOT() {
            cc = c_advance(cc)
            let vtok = c_cur(cc)
            let variant = vtok.value
            cc = c_advance(cc)

            -- Determine the C type name for the enum
            let mangled_enum = dm_mangle(name)

            -- Check if it's a tagged union (has variant info)
            let vinfo = lookup_enum_variant(cc, name, variant)
            -- Also check for Option/Result with dm_ prefix
            let vinfo2 = lookup_enum_variant(cc, subject_type, variant)

            let mut use_type = mangled_enum
            let mut use_vinfo = vinfo
            if vinfo == "" and vinfo2 != "" {
                use_type = subject_type
                use_vinfo = vinfo2
            }

            let condition = "(" + subject_code + ".tag == " + use_type + "_tag_" + variant + ")"

            -- Check for payload bindings: (x, y)
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)  -- skip '('
                let mut bindings = ""
                let ind = indent_str(cc.indent + 1)
                let mut field_idx = 0

                -- Parse the payload types from vinfo to get C types
                let mut payload_types = ""
                if starts_with(use_vinfo, "tuple:") {
                    payload_types = substr(use_vinfo, 6, len(use_vinfo) - 6)
                }

                while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
                    if field_idx > 0 {
                        cc = c_expect(cc, TK_COMMA())
                    }
                    let bind_tok = c_cur(cc)
                    cc = c_advance(cc)

                    -- Get the type for this field from payload types
                    let mut field_type = "int64_t"
                    if payload_types != "" {
                        let comma_pos = string_find(payload_types, ",")
                        if comma_pos < 0 {
                            field_type = payload_types
                        } else {
                            field_type = substr(payload_types, 0, comma_pos)
                            payload_types = substr(payload_types, comma_pos + 1, len(payload_types) - comma_pos - 1)
                        }
                    }

                    if bind_tok.value != "_" {
                        bindings = bindings + ind + field_type + " " + dm_mangle(bind_tok.value) + " = " + subject_code + ".data." + variant + "._" + int_to_string(field_idx) + ";\n"
                        -- Track the binding variable type
                        cc = track_var_type(cc, dm_mangle(bind_tok.value), field_type)
                        let is_str = field_type == "dm_string"
                        cc = track_str_var(cc, dm_mangle(bind_tok.value), is_str)
                    }
                    field_idx = field_idx + 1
                }
                cc = c_expect(cc, TK_RPAREN())
                return MatchArm { c: cc, condition: condition, bindings: bindings }
            }
            return MatchArm { c: cc, condition: condition, bindings: "" }
        }

        -- Check for bare Option/Result constructors: Some(x), None, Ok(x), Err(x)
        let is_some = name == "Some" and starts_with(subject_type, "dm_option_")
        let is_none = name == "None" and starts_with(subject_type, "dm_option_")
        let is_ok = name == "Ok" and starts_with(subject_type, "dm_result_")
        let is_err = name == "Err" and starts_with(subject_type, "dm_result_")

        if is_some or is_ok or is_err {
            -- Constructor with payload: Some(x), Ok(x), Err(e)
            let condition = "(" + subject_code + ".tag == " + subject_type + "_tag_" + name + ")"
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let mut bindings = ""
                let ind = indent_str(cc.indent + 1)
                let mut field_idx = 0
                -- Get payload type from variant info
                let vinfo = lookup_enum_variant(cc, subject_type, name)
                let mut payload_types = ""
                if starts_with(vinfo, "tuple:") {
                    payload_types = substr(vinfo, 6, len(vinfo) - 6)
                }
                while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
                    if field_idx > 0 {
                        cc = c_expect(cc, TK_COMMA())
                    }
                    let bind_tok = c_cur(cc)
                    cc = c_advance(cc)
                    let mut field_type = "int64_t"
                    if payload_types != "" {
                        let comma_pos = string_find(payload_types, ",")
                        if comma_pos < 0 {
                            field_type = payload_types
                        } else {
                            field_type = substr(payload_types, 0, comma_pos)
                            payload_types = substr(payload_types, comma_pos + 1, len(payload_types) - comma_pos - 1)
                        }
                    }
                    if bind_tok.value != "_" {
                        bindings = bindings + ind + field_type + " " + dm_mangle(bind_tok.value) + " = " + subject_code + ".data." + name + "._" + int_to_string(field_idx) + ";\n"
                        cc = track_var_type(cc, dm_mangle(bind_tok.value), field_type)
                        cc = track_str_var(cc, dm_mangle(bind_tok.value), field_type == "dm_string")
                    }
                    field_idx = field_idx + 1
                }
                cc = c_expect(cc, TK_RPAREN())
                return MatchArm { c: cc, condition: condition, bindings: bindings }
            }
            return MatchArm { c: cc, condition: condition, bindings: "" }
        }
        if is_none {
            let condition = "(" + subject_code + ".tag == " + subject_type + "_tag_None)"
            return MatchArm { c: cc, condition: condition, bindings: "" }
        }

        -- Simple identifier - catch-all binding
        let ind = indent_str(cc.indent + 1)
        let inferred = infer_match_subject_type(subject_code, cc)
        let mut bind_type = "int64_t"
        if inferred != "" {
            bind_type = inferred
        }
        let bindings = ind + bind_type + " " + dm_mangle(name) + " = " + subject_code + ";\n"
        cc = track_var_type(cc, dm_mangle(name), bind_type)
        return MatchArm { c: cc, condition: "", bindings: bindings }
    }

    -- Fallback
    return MatchArm { c: cc, condition: "", bindings: "" }
}

fn compile_match_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'match'
    let ind = indent_str(cc.indent)
    let subject = compile_expr(cc)
    cc = subject.c
    let subject_code = subject.code
    let subject_type = infer_type_from_code(subject_code, cc)
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)

    let mut first_arm = true
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_RBRACE() { break }

        let arm = compile_match_pattern(cc, subject_code, subject_type)
        cc = arm.c

        cc = c_skip_nl(cc)
        cc = c_expect(cc, TK_FAT_ARROW())
        cc = c_skip_nl(cc)

        if arm.condition == "" {
            -- Wildcard or catch-all
            if first_arm {
                cc.output = cc.output + ind + "{\n"
            } else {
                cc.output = cc.output + ind + "} else {\n"
            }
        } else {
            if first_arm {
                cc.output = cc.output + ind + "if " + arm.condition + " {\n"
            } else {
                cc.output = cc.output + ind + "} else if " + arm.condition + " {\n"
            }
        }
        first_arm = false

        -- Emit bindings
        cc.output = cc.output + arm.bindings

        -- Compile arm body: either a block { ... } or a single expression
        if c_peek(cc) == TK_LBRACE() {
            cc = c_expect(cc, TK_LBRACE())
            cc.indent = cc.indent + 1
            cc = compile_block_body(cc)
            cc = c_expect(cc, TK_RBRACE())
            cc.indent = cc.indent - 1
        } else {
            -- Single expression as statement
            cc.indent = cc.indent + 1
            let body_ind = indent_str(cc.indent)
            let body_expr = compile_expr(cc)
            cc = body_expr.c
            cc.output = cc.output + body_ind + body_expr.code + ";\n"
            cc.indent = cc.indent - 1
        }

        -- Skip optional comma
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    if first_arm == false {
        cc.output = cc.output + ind + "}\n"
    }
    return cc
}

fn compile_match_expr(c: Compiler) -> ExprOut {
    let mut cc = c_advance(c)  -- skip 'match'
    let ind = indent_str(cc.indent)
    let subject = compile_expr(cc)
    cc = subject.c
    let subject_code = subject.code
    let subject_type = infer_type_from_code(subject_code, cc)
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)

    -- Create a temp var for the result
    let match_id = cc.match_counter
    cc.match_counter = cc.match_counter + 1
    let temp_var = "_match_" + int_to_string(match_id)

    -- We need to determine the result type from the first arm
    -- For now, emit the match as a statement block and use the temp var
    -- The type will be inferred after we see the first arm's result
    let mut first_arm = true
    let mut result_type = "int64_t"
    let mut match_output = ""

    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_RBRACE() { break }

        let arm = compile_match_pattern(cc, subject_code, subject_type)
        cc = arm.c

        cc = c_skip_nl(cc)
        cc = c_expect(cc, TK_FAT_ARROW())
        cc = c_skip_nl(cc)

        if arm.condition == "" {
            if first_arm {
                match_output = match_output + ind + "{\n"
            } else {
                match_output = match_output + ind + "} else {\n"
            }
        } else {
            if first_arm {
                match_output = match_output + ind + "if " + arm.condition + " {\n"
            } else {
                match_output = match_output + ind + "} else if " + arm.condition + " {\n"
            }
        }

        -- Emit bindings
        match_output = match_output + arm.bindings

        -- Compile arm expression
        let arm_ind = indent_str(cc.indent + 1)
        if c_peek(cc) == TK_LBRACE() {
            -- Block body - the last expression should be the value
            -- For simplicity, compile as a block and expect an assignment
            cc = c_expect(cc, TK_LBRACE())
            let saved_out = cc.output
            cc.output = ""
            cc.indent = cc.indent + 1
            cc = compile_block_body(cc)
            cc = c_expect(cc, TK_RBRACE())
            cc.indent = cc.indent - 1
            match_output = match_output + cc.output
            cc.output = saved_out
        } else {
            let arm_expr = compile_expr(cc)
            cc = arm_expr.c
            if first_arm {
                result_type = infer_type_from_code(arm_expr.code, cc)
            }
            match_output = match_output + arm_ind + temp_var + " = " + arm_expr.code + ";\n"
        }
        first_arm = false

        -- Skip optional comma
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    if first_arm == false {
        match_output = match_output + ind + "}\n"
    }

    -- Emit the temp var declaration and match block into the output
    cc.output = cc.output + ind + result_type + " " + temp_var + ";\n"
    cc.output = cc.output + match_output
    -- Track the temp var type so let inference works
    cc = track_var_type(cc, temp_var, result_type)
    let is_str = result_type == "dm_string"
    cc = track_str_var(cc, temp_var, is_str)
    return ExprOut { c: cc, code: temp_var }
}
