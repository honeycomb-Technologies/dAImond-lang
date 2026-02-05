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

    -- Skip optional effect annotation: with [IO, Console, ...]
    if c_peek(cc) == TK_WITH() {
        cc = c_advance(cc)
        cc = c_expect(cc, TK_LBRACKET())
        while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
            cc = c_advance(cc)
        }
        cc = c_expect(cc, TK_RBRACKET())
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

fn compile_trait_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'trait'
    let name_tok = c_cur(cc)
    let trait_name = name_tok.value
    cc = c_advance(cc)
    cc.trait_names.push(trait_name)

    -- Skip optional trait inheritance: trait Ord: Eq { ... }
    if c_peek(cc) == TK_COLON() {
        cc = c_advance(cc)
        -- Skip parent trait name(s)
        while c_peek(cc) == TK_IDENT() or c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
    }

    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)

    -- Parse method signatures inside trait block
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        if c_peek(cc) == TK_FN() {
            cc = c_advance(cc)  -- skip 'fn'
            let method_tok = c_cur(cc)
            let method_name = method_tok.value
            cc = c_advance(cc)

            -- Parse parameters (skip the method signature, just collect types)
            cc = c_expect(cc, TK_LPAREN())
            let mut param_types = ""
            let mut first = true
            while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
                if first == false {
                    cc = c_expect(cc, TK_COMMA())
                }
                first = false
                let ptok = c_cur(cc)
                -- Handle 'self' and 'mut self' parameter
                if ptok.kind == TK_SELF() {
                    cc = c_advance(cc)
                    -- self parameter: skip for param_types (receiver is implicit)
                } else if ptok.kind == TK_MUT() {
                    cc = c_advance(cc)
                    if c_peek(cc) == TK_SELF() {
                        cc = c_advance(cc)
                        -- mut self: skip
                    } else {
                        -- regular mut param: name: type
                        cc = c_advance(cc)
                        cc = c_expect(cc, TK_COLON())
                        let pt = parse_type_for_c(cc)
                        cc = pt.c
                        if param_types != "" {
                            param_types = param_types + ","
                        }
                        param_types = param_types + pt.code
                    }
                } else {
                    -- Regular param: name: type
                    cc = c_advance(cc)
                    cc = c_expect(cc, TK_COLON())
                    -- Handle 'Self' type as placeholder
                    let pt = parse_type_for_c(cc)
                    cc = pt.c
                    if param_types != "" {
                        param_types = param_types + ","
                    }
                    param_types = param_types + pt.code
                }
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

            -- Skip optional effect annotation: with [IO, Console, ...]
            if c_peek(cc) == TK_WITH() {
                cc = c_advance(cc)
                cc = c_expect(cc, TK_LBRACKET())
                while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
                    cc = c_advance(cc)
                }
                cc = c_expect(cc, TK_RBRACKET())
            }

            -- Store trait method signature: "|TraitName.method=ret_type:param_types|"
            cc.trait_methods = cc.trait_methods + "|" + trait_name + "." + method_name + "=" + ret_type + ":" + param_types + "|"

            -- Skip optional method body (default impl) or just continue
            cc = c_skip_nl(cc)
            if c_peek(cc) == TK_LBRACE() {
                -- Skip default implementation body
                let mut depth = 1
                cc = c_advance(cc)
                while depth > 0 and c_peek(cc) != TK_EOF() {
                    if c_peek(cc) == TK_LBRACE() {
                        depth = depth + 1
                    }
                    if c_peek(cc) == TK_RBRACE() {
                        depth = depth - 1
                    }
                    if depth > 0 {
                        cc = c_advance(cc)
                    }
                }
                cc = c_advance(cc)  -- skip final '}'
            }
        } else {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    return cc
}

fn compile_impl_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'impl'
    let first_tok = c_cur(cc)
    let first_name = first_tok.value
    cc = c_advance(cc)

    -- Determine if this is 'impl Type { ... }' or 'impl Trait for Type { ... }'
    let mut impl_type_name = first_name
    let mut trait_name = ""

    -- Skip generic params on first name if any
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

    -- Check for 'for' keyword -> impl Trait for Type
    cc = c_skip_nl(cc)
    if c_peek(cc) == TK_FOR() {
        cc = c_advance(cc)  -- skip 'for'
        trait_name = first_name
        let type_tok = c_cur(cc)
        impl_type_name = type_tok.value
        cc = c_advance(cc)
        -- Skip generic params on type if any
        if c_peek(cc) == TK_LBRACKET() {
            let mut depth2 = 1
            cc = c_advance(cc)
            while depth2 > 0 and c_peek(cc) != TK_EOF() {
                if c_peek(cc) == TK_LBRACKET() {
                    depth2 = depth2 + 1
                }
                if c_peek(cc) == TK_RBRACKET() {
                    depth2 = depth2 - 1
                }
                cc = c_advance(cc)
            }
        }
        -- Record the impl-for mapping
        cc.impl_for_map = cc.impl_for_map + "|" + impl_type_name + "=" + trait_name + "|"
    }

    -- Set the current impl type for method name mangling
    let saved_impl_type = cc.current_impl_type
    cc.current_impl_type = impl_type_name

    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)
    -- Parse methods inside impl block
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        if c_peek(cc) == TK_FN() {
            cc = compile_impl_method(cc, impl_type_name)
        } else {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())

    -- Restore impl type
    cc.current_impl_type = saved_impl_type
    return cc
}

-- Compile a method inside an impl block with proper name mangling
fn compile_impl_method(c: Compiler, type_name: string) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'fn'
    let name_tok = c_cur(cc)
    let method_name = name_tok.value
    cc = c_advance(cc)

    -- Mangled name: TypeName_method
    let mangled_name = type_name + "_" + method_name

    -- Check for generic function: fn name[T](...) — skip it
    if c_peek(cc) == TK_LBRACKET() {
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

        -- Handle 'self' and 'mut self' parameters
        if pname_tok.kind == TK_SELF() {
            cc = c_advance(cc)
            let self_type = dm_mangle(type_name)
            params_c = params_c + self_type + "* dm_self"
            cc = track_str_var(cc, "dm_self", false)
            cc = track_var_type(cc, "dm_self", self_type + "*")
        } else if pname_tok.kind == TK_MUT() {
            cc = c_advance(cc)
            if c_peek(cc) == TK_SELF() {
                cc = c_advance(cc)
                let self_type = dm_mangle(type_name)
                params_c = params_c + self_type + "* dm_self"
                cc = track_str_var(cc, "dm_self", false)
                cc = track_var_type(cc, "dm_self", self_type + "*")
            } else {
                -- Regular mut param
                let mpname_tok = c_cur(cc)
                let mpname = mpname_tok.value
                cc = c_advance(cc)
                cc = c_expect(cc, TK_COLON())
                let pt = parse_type_for_c(cc)
                cc = pt.c
                params_c = params_c + pt.code + " " + dm_mangle(mpname)
                cc = track_str_var(cc, dm_mangle(mpname), pt.code == "dm_string")
                cc = track_list_elem_type(cc, pt.code, mpname)
                cc = track_var_type(cc, dm_mangle(mpname), pt.code)
            }
        } else {
            cc = c_advance(cc)
            cc = c_expect(cc, TK_COLON())
            let pt = parse_type_for_c(cc)
            cc = pt.c
            params_c = params_c + pt.code + " " + dm_mangle(pname)
            cc = track_str_var(cc, dm_mangle(pname), pt.code == "dm_string")
            cc = track_list_elem_type(cc, pt.code, pname)
            cc = track_var_type(cc, dm_mangle(pname), pt.code)
        }
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

    -- Skip optional effect annotation: with [IO, Console, ...]
    if c_peek(cc) == TK_WITH() {
        cc = c_advance(cc)
        cc = c_expect(cc, TK_LBRACKET())
        while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
            cc = c_advance(cc)
        }
        cc = c_expect(cc, TK_RBRACKET())
    }

    -- Generate function signature with mangled name
    let mut final_sig = ret_type + " " + dm_mangle(mangled_name) + "(" + params_c + ")"
    if params_c == "" {
        final_sig = ret_type + " " + dm_mangle(mangled_name) + "(void)"
    }
    cc.fn_sigs.push(final_sig + ";")
    cc.fn_names.push(mangled_name)
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
                -- Skip optional effect annotation: with [IO, Console, ...]
                if j < cc.tokens.len() and cc.tokens[j].kind == TK_WITH() {
                    j = j + 1
                    if j < cc.tokens.len() and cc.tokens[j].kind == TK_LBRACKET() {
                        j = j + 1
                        while j < cc.tokens.len() and cc.tokens[j].kind != TK_RBRACKET() {
                            j = j + 1
                        }
                        if j < cc.tokens.len() { j = j + 1 }
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
        if tok.kind == TK_TRAIT() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                cc.trait_names.push(name_tok.value)
            }
        }
        -- Prescan impl blocks to register mangled method names
        if tok.kind == TK_IMPL() and i + 1 < cc.tokens.len() {
            let impl_first_tok = cc.tokens[i + 1]
            if impl_first_tok.kind == TK_IDENT() {
                let first_name = impl_first_tok.value
                -- Scan forward to check for 'for' keyword
                let mut scan_j = i + 2
                -- Skip generic params if any
                if scan_j < cc.tokens.len() and cc.tokens[scan_j].kind == TK_LBRACKET() {
                    let mut gdepth = 1
                    scan_j = scan_j + 1
                    while gdepth > 0 and scan_j < cc.tokens.len() {
                        if cc.tokens[scan_j].kind == TK_LBRACKET() {
                            gdepth = gdepth + 1
                        }
                        if cc.tokens[scan_j].kind == TK_RBRACKET() {
                            gdepth = gdepth - 1
                        }
                        scan_j = scan_j + 1
                    }
                }
                -- Skip newlines
                while scan_j < cc.tokens.len() and cc.tokens[scan_j].kind == TK_NEWLINE() {
                    scan_j = scan_j + 1
                }
                let mut impl_type = first_name
                if scan_j < cc.tokens.len() and cc.tokens[scan_j].kind == TK_FOR() {
                    -- impl Trait for Type: the type is after 'for'
                    scan_j = scan_j + 1
                    if scan_j < cc.tokens.len() and cc.tokens[scan_j].kind == TK_IDENT() {
                        let impl_type_tok = cc.tokens[scan_j]
                        impl_type = impl_type_tok.value
                    }
                }
                -- Now scan methods inside { } and register with mangled names
                -- Find the opening brace
                while scan_j < cc.tokens.len() and cc.tokens[scan_j].kind != TK_LBRACE() {
                    scan_j = scan_j + 1
                }
                if scan_j < cc.tokens.len() {
                    scan_j = scan_j + 1  -- skip '{'
                    let mut impl_brace_depth = 1
                    while impl_brace_depth > 0 and scan_j < cc.tokens.len() {
                        if cc.tokens[scan_j].kind == TK_LBRACE() {
                            impl_brace_depth = impl_brace_depth + 1
                        }
                        if cc.tokens[scan_j].kind == TK_RBRACE() {
                            impl_brace_depth = impl_brace_depth - 1
                            if impl_brace_depth == 0 { break }
                        }
                        if cc.tokens[scan_j].kind == TK_FN() and impl_brace_depth == 1 {
                            -- Found a method at the impl level
                            if scan_j + 1 < cc.tokens.len() and cc.tokens[scan_j + 1].kind == TK_IDENT() {
                                let method_tok = cc.tokens[scan_j + 1]
                                let method_name = method_tok.value
                                let mangled = impl_type + "_" + method_name
                                -- Find return type of this method
                                let mut mj = scan_j + 2
                                -- Skip generic params
                                if mj < cc.tokens.len() and cc.tokens[mj].kind == TK_LBRACKET() {
                                    while mj < cc.tokens.len() and cc.tokens[mj].kind != TK_RBRACKET() {
                                        mj = mj + 1
                                    }
                                    mj = mj + 1
                                }
                                -- Skip past parameter list
                                let mut pdepth = 0
                                while mj < cc.tokens.len() {
                                    let pt = cc.tokens[mj]
                                    if pt.kind == TK_LPAREN() { pdepth = pdepth + 1 }
                                    if pt.kind == TK_RPAREN() {
                                        pdepth = pdepth - 1
                                        if pdepth == 0 {
                                            mj = mj + 1
                                            break
                                        }
                                    }
                                    mj = mj + 1
                                }
                                -- Check for -> RetType
                                let mut mret_type = "void"
                                if mj < cc.tokens.len() and cc.tokens[mj].kind == TK_ARROW() {
                                    mj = mj + 1
                                    if mj < cc.tokens.len() and cc.tokens[mj].kind == TK_IDENT() {
                                        let ret_tok = cc.tokens[mj]
                                        mret_type = map_dm_type(ret_tok.value)
                                    }
                                }
                                -- Skip optional effect annotation: with [...]
                                if mj < cc.tokens.len() and cc.tokens[mj].kind == TK_WITH() {
                                    mj = mj + 1
                                    if mj < cc.tokens.len() and cc.tokens[mj].kind == TK_LBRACKET() {
                                        mj = mj + 1
                                        while mj < cc.tokens.len() and cc.tokens[mj].kind != TK_RBRACKET() {
                                            mj = mj + 1
                                        }
                                        if mj < cc.tokens.len() { mj = mj + 1 }
                                    }
                                }
                                cc.fn_names.push(mangled)
                                cc.fn_ret_types.push(mret_type)
                            }
                        }
                        scan_j = scan_j + 1
                    }
                }
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
        } else if k == TK_TRAIT() {
            cc = compile_trait_decl(cc)
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
