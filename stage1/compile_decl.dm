module compile_decl

import compiler

-- ============================================================
-- DECLARATION COMPILER
-- ============================================================

fn compile_fn_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'fn'
    let name_tok = c_cur(cc)
    let fn_name = name_tok.value
    cc = c_advance(cc)

    -- Check for generic function: fn name[T](...) — skip it (compiled on demand)
    if c_peek(cc) == TK_LBRACKET() {
        -- Skip entire generic function body
        let mut depth = 0
        let mut found_end = false
        while found_end == false and c_peek(cc) != TK_EOF() {
            if c_peek(cc) == TK_LBRACE() {
                depth = depth + 1
            }
            if c_peek(cc) == TK_RBRACE() {
                depth = depth - 1
                if depth == 0 {
                    cc = c_advance(cc)
                    found_end = true
                }
            }
            if found_end == false {
                cc = c_advance(cc)
            }
        }
        return cc
    }

    -- Parameters
    cc = c_expect(cc, TK_LPAREN())
    let mut params_c = ""
    let mut first = true
    while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
        if first == false {
            cc = c_expect(cc, TK_COMMA())
            params_c = params_c + ", "
        }
        first = false
        let pname_tok = c_cur(cc)
        let pname = pname_tok.value
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let pt = parse_type_for_c(cc)
        cc = pt.c
        params_c = params_c + pt.code + " " + dm_mangle(pname)
        -- Track string parameters
        cc = track_str_var(cc, dm_mangle(pname), pt.code == "dm_string")
        -- Track list element type for list parameters
        cc = track_list_elem_type(cc, pt.code, pname)
        -- Track parameter type for variable inference
        cc = track_var_type(cc, dm_mangle(pname), pt.code)
    }
    cc = c_expect(cc, TK_RPAREN())

    -- Return type
    let mut ret_type = "void"
    if c_peek(cc) == TK_ARROW() {
        cc = c_advance(cc)
        let rt = parse_type_for_c(cc)
        cc = rt.c
        ret_type = rt.code
    }

    -- Generate function signature
    let mut final_sig = ret_type + " " + dm_mangle(fn_name) + "(" + params_c + ")"
    if params_c == "" {
        final_sig = ret_type + " " + dm_mangle(fn_name) + "(void)"
    }
    cc.fn_sigs.push(final_sig + ";")
    cc.fn_names.push(fn_name)
    cc.fn_ret_types.push(ret_type)

    -- Function body
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    let mut body_out = final_sig + " {\n"

    -- Save and reset output for body
    let saved_output = cc.output
    let saved_fn_ret = cc.current_fn_ret_type
    cc.output = ""
    cc.indent = 1
    cc.current_fn_ret_type = ret_type
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    body_out = body_out + cc.output + "}\n\n"
    cc.output = saved_output
    cc.indent = 0
    cc.current_fn_ret_type = saved_fn_ret
    cc.fn_defs.push(body_out)

    return cc
}

fn compile_struct_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'struct'
    let name_tok = c_cur(cc)
    let struct_name = name_tok.value
    cc = c_advance(cc)
    cc.struct_names.push(struct_name)

    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())

    let mut fields = "typedef struct " + dm_mangle(struct_name) + " {\n"
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        let fname_tok = c_cur(cc)
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let ft = parse_type_for_c(cc)
        cc = ft.c
        fields = fields + "    " + ft.code + " " + fname_tok.value + ";\n"
        -- Track struct field type
        cc.struct_fields = cc.struct_fields + "|" + dm_mangle(struct_name) + "." + fname_tok.value + "=" + ft.code + "|"
        -- Skip optional comma
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    fields = fields + "} " + dm_mangle(struct_name) + ";\n\n"
    cc.struct_defs.push(fields)

    return cc
}

fn compile_enum_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'enum'
    let name_tok = c_cur(cc)
    let enum_name = name_tok.value
    cc = c_advance(cc)
    cc.enum_names.push(enum_name)

    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())

    -- First pass: collect variant names and their payload types
    let mut variant_names: List[string] = []
    let mut variant_payloads: List[string] = []
    let mut has_any_payload = false

    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        let vname_tok = c_cur(cc)
        let vname = vname_tok.value
        cc = c_advance(cc)
        variant_names.push(vname)

        -- Check for tuple payload: Variant(Type1, Type2, ...)
        if c_peek(cc) == TK_LPAREN() {
            cc = c_advance(cc)  -- skip '('
            let mut payload_types = ""
            let mut first = true
            while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
                if first == false {
                    cc = c_expect(cc, TK_COMMA())
                    payload_types = payload_types + ","
                }
                first = false
                let pt = parse_type_for_c(cc)
                cc = pt.c
                payload_types = payload_types + pt.code
            }
            cc = c_expect(cc, TK_RPAREN())
            variant_payloads.push("tuple:" + payload_types)
            has_any_payload = true
            cc.enum_variants = cc.enum_variants + "|" + enum_name + "." + vname + "=tuple:" + payload_types + "|"
        } else {
            variant_payloads.push("unit")
            cc.enum_variants = cc.enum_variants + "|" + enum_name + "." + vname + "=unit|"
        }

        -- Skip optional comma
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())

    let mut def = ""
    if has_any_payload == false {
        -- Simple enum: no payloads, generate plain C enum
        def = "typedef enum " + dm_mangle(enum_name) + " {\n"
        let mut i = 0
        while i < variant_names.len() {
            def = def + "    " + dm_mangle(enum_name) + "_" + variant_names[i]
            if i + 1 < variant_names.len() {
                def = def + ","
            }
            def = def + "\n"
            i = i + 1
        }
        def = def + "} " + dm_mangle(enum_name) + ";\n\n"
    } else {
        -- Tagged union enum: generate tag enum + union struct + constructors
        let mangled = dm_mangle(enum_name)
        -- Tag enum
        def = def + "typedef enum " + mangled + "_tag {\n"
        let mut i = 0
        while i < variant_names.len() {
            def = def + "    " + mangled + "_tag_" + variant_names[i]
            if i + 1 < variant_names.len() {
                def = def + ","
            }
            def = def + "\n"
            i = i + 1
        }
        def = def + "} " + mangled + "_tag;\n\n"

        -- Struct with tag + union
        def = def + "typedef struct " + mangled + " {\n"
        def = def + "    " + mangled + "_tag tag;\n"
        def = def + "    union {\n"
        i = 0
        while i < variant_names.len() {
            let payload = "" + variant_payloads[i]
            if starts_with(payload, "tuple:") {
                let types_str = substr(payload, 6, len(payload) - 6)
                def = def + "        struct {"
                -- Parse comma-separated types
                let mut field_idx = 0
                let mut tpos = 0
                let mut tstr = types_str
                let mut parse_done = false
                while parse_done == false {
                    let comma_pos = string_find(tstr, ",")
                    if comma_pos < 0 {
                        def = def + " " + tstr + " _" + int_to_string(field_idx) + ";"
                        parse_done = true
                    } else {
                        let t = substr(tstr, 0, comma_pos)
                        def = def + " " + t + " _" + int_to_string(field_idx) + ";"
                        tstr = substr(tstr, comma_pos + 1, len(tstr) - comma_pos - 1)
                        field_idx = field_idx + 1
                    }
                }
                def = def + " } " + variant_names[i] + ";\n"
            }
            i = i + 1
        }
        def = def + "    } data;\n"
        def = def + "} " + mangled + ";\n\n"

        -- Constructor functions
        i = 0
        while i < variant_names.len() {
            let payload = "" + variant_payloads[i]
            let vname = "" + variant_names[i]
            if payload == "unit" {
                def = def + "static inline " + mangled + " " + mangled + "_" + vname + "(void) {\n"
                def = def + "    " + mangled + " _r; _r.tag = " + mangled + "_tag_" + vname + "; return _r;\n"
                def = def + "}\n\n"
            } else if starts_with(payload, "tuple:") {
                let types_str = substr(payload, 6, len(payload) - 6)
                -- Build parameter list
                let mut params = ""
                let mut body = ""
                let mut field_idx = 0
                let mut tstr = types_str
                let mut parse_done = false
                while parse_done == false {
                    let comma_pos = string_find(tstr, ",")
                    if field_idx > 0 {
                        params = params + ", "
                    }
                    if comma_pos < 0 {
                        params = params + tstr + " _" + int_to_string(field_idx)
                        body = body + "    _r.data." + vname + "._" + int_to_string(field_idx) + " = _" + int_to_string(field_idx) + ";\n"
                        parse_done = true
                    } else {
                        let t = substr(tstr, 0, comma_pos)
                        params = params + t + " _" + int_to_string(field_idx)
                        body = body + "    _r.data." + vname + "._" + int_to_string(field_idx) + " = _" + int_to_string(field_idx) + ";\n"
                        tstr = substr(tstr, comma_pos + 1, len(tstr) - comma_pos - 1)
                        field_idx = field_idx + 1
                    }
                }
                def = def + "static inline " + mangled + " " + mangled + "_" + vname + "(" + params + ") {\n"
                def = def + "    " + mangled + " _r; _r.tag = " + mangled + "_tag_" + vname + ";\n"
                def = def + body
                def = def + "    return _r;\n"
                def = def + "}\n\n"
            }
            i = i + 1
        }
    }
    cc.struct_defs.push(def)

    return cc
}

fn compile_impl_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'impl'
    let type_tok = c_cur(cc)
    cc = c_advance(cc)
    -- Skip generic params if any
    if c_peek(cc) == TK_LBRACKET() {
        let mut depth = 1
        cc = c_advance(cc)
        while depth > 0 and c_peek(cc) != TK_EOF() {
            if c_peek(cc) == TK_LBRACKET() {
                depth = depth + 1
            }
            if c_peek(cc) == TK_RBRACKET() {
                depth = depth - 1
            }
            cc = c_advance(cc)
        }
    }
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)
    -- Parse methods inside impl block
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        if c_peek(cc) == TK_FN() {
            cc = compile_fn_decl(cc)
        } else {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    return cc
}

fn compile_import_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'import'
    -- Skip the rest of the import line
    while c_peek(cc) != TK_NEWLINE() and c_peek(cc) != TK_EOF() {
        cc = c_advance(cc)
    }
    return cc
}

-- ============================================================
-- TOP-LEVEL COMPILER
-- ============================================================

fn prescan_declarations(c: Compiler) -> Compiler {
    let mut cc = c
    let mut i = 0
    while i < cc.tokens.len() {
        let tok = cc.tokens[i]
        if tok.kind == TK_FN() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                let fn_name = name_tok.value

                -- Check for generic function: fn name[T](...)
                let mut is_generic = false
                let mut type_params = ""
                if i + 2 < cc.tokens.len() {
                    let maybe_bracket = cc.tokens[i + 2]
                    if maybe_bracket.kind == TK_LBRACKET() {
                        is_generic = true
                        -- Collect type parameter names
                        let mut gj = i + 3
                        while gj < cc.tokens.len() and cc.tokens[gj].kind != TK_RBRACKET() {
                            if cc.tokens[gj].kind == TK_IDENT() {
                                if type_params != "" {
                                    type_params = type_params + ","
                                }
                                type_params = type_params + cc.tokens[gj].value
                            }
                            gj = gj + 1
                        }
                        -- Find end of function body (matching closing brace)
                        let mut body_j = gj
                        let mut brace_depth = 0
                        let mut found_body_end = false
                        while body_j < cc.tokens.len() and found_body_end == false {
                            if cc.tokens[body_j].kind == TK_LBRACE() {
                                brace_depth = brace_depth + 1
                            }
                            if cc.tokens[body_j].kind == TK_RBRACE() {
                                brace_depth = brace_depth - 1
                                if brace_depth == 0 {
                                    found_body_end = true
                                }
                            }
                            body_j = body_j + 1
                        }
                        -- Store: "|fn_name=type_params:start:end|"
                        cc.generic_fn_tokens = cc.generic_fn_tokens + "|" + fn_name + "=" + type_params + ":" + int_to_string(i) + ":" + int_to_string(body_j) + "|"
                    }
                }

                if is_generic {
                    -- Don't add generic fns to fn_names/fn_ret_types (they're templates)
                    i = i + 1
                    continue
                }

                -- Find return type by scanning for -> after )
                let mut j = i + 2
                let mut depth = 0
                -- Skip past parameter list
                while j < cc.tokens.len() {
                    let t = cc.tokens[j]
                    if t.kind == TK_LPAREN() { depth = depth + 1 }
                    if t.kind == TK_RPAREN() {
                        depth = depth - 1
                        if depth == 0 {
                            j = j + 1
                            break
                        }
                    }
                    j = j + 1
                }
                -- Check for -> RetType
                let mut ret_type = "void"
                if j < cc.tokens.len() {
                    let arrow_tok = cc.tokens[j]
                    if arrow_tok.kind == TK_ARROW() {
                        j = j + 1
                        if j < cc.tokens.len() {
                            let ret_tok = cc.tokens[j]
                            if ret_tok.kind == TK_IDENT() {
                                let ret_name = ret_tok.value
                                -- Check for generic: List[T], Box[T], Option[T], Result[T, E]
                                if j + 2 < cc.tokens.len() {
                                    let maybe_bracket = cc.tokens[j + 1]
                                    if maybe_bracket.kind == TK_LBRACKET() {
                                        let inner_tok = cc.tokens[j + 2]
                                        if inner_tok.kind == TK_IDENT() {
                                            let inner_c = map_dm_type(inner_tok.value)
                                            if ret_name == "List" {
                                                ret_type = "dm_list_" + inner_c
                                            } else if ret_name == "Box" {
                                                ret_type = inner_c + "*"
                                            } else if ret_name == "Option" {
                                                ret_type = "dm_option_" + inner_c
                                            } else if ret_name == "Result" {
                                                -- Result[T, E] - look for comma and second type
                                                let mut second_c = "dm_string"
                                                if j + 4 < cc.tokens.len() {
                                                    let maybe_comma = cc.tokens[j + 3]
                                                    if maybe_comma.kind == TK_COMMA() {
                                                        let second_tok = cc.tokens[j + 4]
                                                        if second_tok.kind == TK_IDENT() {
                                                            second_c = map_dm_type(second_tok.value)
                                                        }
                                                    }
                                                }
                                                ret_type = "dm_result_" + inner_c + "_" + second_c
                                            } else {
                                                ret_type = "dm_" + ret_name + "_" + inner_c
                                            }
                                        } else {
                                            ret_type = map_dm_type(ret_name)
                                        }
                                    } else {
                                        ret_type = map_dm_type(ret_name)
                                    }
                                } else {
                                    ret_type = map_dm_type(ret_name)
                                }
                            }
                        }
                    }
                }
                cc.fn_names.push(fn_name)
                cc.fn_ret_types.push(ret_type)
            }
        }
        if tok.kind == TK_STRUCT() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                cc.struct_names.push(name_tok.value)
            }
        }
        if tok.kind == TK_ENUM() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                cc.enum_names.push(name_tok.value)
            }
        }
        i = i + 1
    }
    return cc
}

fn compile_source(c: Compiler) -> Compiler {
    let mut cc = c
    -- Pre-scan all declarations for forward references
    cc = prescan_declarations(cc)
    cc = c_skip_nl(cc)

    -- Parse module declaration
    let mut module_name = "unknown"
    if c_peek(cc) == TK_MODULE() {
        cc = c_advance(cc)
        let mod_tok = c_cur(cc)
        module_name = mod_tok.value
        cc = c_advance(cc)
        -- Skip dotted module path
        while c_peek(cc) == TK_DOT() {
            cc = c_advance(cc)
            cc = c_advance(cc)
        }
    }

    cc = c_skip_nl(cc)

    -- Parse declarations
    while c_peek(cc) != TK_EOF() {
        let k = c_peek(cc)
        if k == TK_FN() {
            cc = compile_fn_decl(cc)
        } else if k == TK_STRUCT() {
            cc = compile_struct_decl(cc)
        } else if k == TK_ENUM() {
            cc = compile_enum_decl(cc)
        } else if k == TK_IMPL() {
            cc = compile_impl_decl(cc)
        } else if k == TK_IMPORT() {
            cc = compile_import_decl(cc)
        } else if k == TK_NEWLINE() {
            cc = c_advance(cc)
        } else {
            cc = c_error(cc, "unexpected token at top level: " + token_kind_name(k))
            cc = c_advance(cc)
        }
    }

    return cc
}
