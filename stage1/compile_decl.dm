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

    -- Parse optional effect annotation: with [IO, Console, ...]
    let mut fn_effects = ""
    let mut fn_has_effects = false
    if c_peek(cc) == TK_WITH() {
        cc = c_advance(cc)
        cc = c_expect(cc, TK_LBRACKET())
        while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
            let effect_tok = c_cur(cc)
            if effect_tok.kind == TK_IDENT() {
                fn_effects = fn_effects + "|" + effect_tok.value + "|"
            }
            cc = c_advance(cc)
        }
        cc = c_expect(cc, TK_RBRACKET())
        fn_has_effects = true
    }

    -- If this is an async fn, extract inner type from dm_future_T return type
    if cc.current_fn_is_async and starts_with(ret_type, "dm_future_") {
        cc.current_fn_async_inner_type = substr(ret_type, 10, len(ret_type) - 10)
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
    let saved_fn_effects = cc.current_fn_effects
    let saved_fn_has_effects = cc.current_fn_has_effects
    cc.output = ""
    cc.indent = 1
    cc.current_fn_ret_type = ret_type
    cc.current_fn_effects = fn_effects
    cc.current_fn_has_effects = fn_has_effects
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    body_out = body_out + cc.output + "}\n\n"
    cc.output = saved_output
    cc.indent = 0
    cc.current_fn_ret_type = saved_fn_ret
    cc.current_fn_effects = saved_fn_effects
    cc.current_fn_has_effects = saved_fn_has_effects
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

    -- Parse optional effect annotation: with [IO, Console, ...]
    let mut method_effects = ""
    let mut method_has_effects = false
    if c_peek(cc) == TK_WITH() {
        cc = c_advance(cc)
        cc = c_expect(cc, TK_LBRACKET())
        while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
            let effect_tok = c_cur(cc)
            if effect_tok.kind == TK_IDENT() {
                method_effects = method_effects + "|" + effect_tok.value + "|"
            }
            cc = c_advance(cc)
        }
        cc = c_expect(cc, TK_RBRACKET())
        method_has_effects = true
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
    let saved_method_effects = cc.current_fn_effects
    let saved_method_has_effects = cc.current_fn_has_effects
    cc.output = ""
    cc.indent = 1
    cc.current_fn_ret_type = ret_type
    cc.current_fn_effects = method_effects
    cc.current_fn_has_effects = method_has_effects
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    body_out = body_out + cc.output + "}\n\n"
    cc.output = saved_output
    cc.indent = 0
    cc.current_fn_ret_type = saved_fn_ret
    cc.current_fn_effects = saved_method_effects
    cc.current_fn_has_effects = saved_method_has_effects
    cc.fn_defs.push(body_out)

    return cc
}

fn compile_extern_fn_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'extern'
    cc = c_advance(cc)  -- skip 'fn'
    let name_tok = c_cur(cc)
    let fn_name = name_tok.value
    cc = c_advance(cc)

    -- Parse parameters
    cc = c_expect(cc, TK_LPAREN())
    let mut param_names: List[string] = []
    let mut param_types: List[string] = []
    let mut first = true
    while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
        if first == false {
            cc = c_expect(cc, TK_COMMA())
        }
        first = false
        let pname_tok = c_cur(cc)
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let pt = parse_type_for_c(cc)
        cc = pt.c
        param_names.push(pname_tok.value)
        param_types.push(pt.code)
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

    -- Check if any params or return type involve dm_string
    let mut needs_string_wrapper = false
    if ret_type == "dm_string" { needs_string_wrapper = true }
    let mut pi = 0
    while pi < param_types.len() {
        let pt = "" + param_types[pi]
        if pt == "dm_string" { needs_string_wrapper = true }
        pi = pi + 1
    }

    if needs_string_wrapper {
        -- String-typed extern: emit C extern with native types and wrapper with dm_string conversion
        let mut c_params = ""
        let mut wrapper_call_args = ""
        let mut wrapper_params = ""
        let mut fi = 0
        while fi < param_types.len() {
            if fi > 0 {
                c_params = c_params + ", "
                wrapper_call_args = wrapper_call_args + ", "
                wrapper_params = wrapper_params + ", "
            }
            let pt = "" + param_types[fi]
            let pn = "" + param_names[fi]
            if pt == "dm_string" {
                c_params = c_params + "const char*"
                wrapper_params = wrapper_params + "dm_string " + dm_mangle(pn)
                wrapper_call_args = wrapper_call_args + dm_mangle(pn) + ".data"
            } else {
                c_params = c_params + pt
                wrapper_params = wrapper_params + pt + " " + dm_mangle(pn)
                wrapper_call_args = wrapper_call_args + dm_mangle(pn)
            }
            fi = fi + 1
        }

        let mut c_ret_native = ret_type
        if c_ret_native == "dm_string" { c_ret_native = "const char*" }

        -- Build wrapper function
        let mut wrapper = "static " + ret_type + " " + dm_mangle(fn_name) + "(" + wrapper_params + ") {\n"
        if ret_type == "dm_string" {
            wrapper = wrapper + "    const char* __ret = " + fn_name + "(" + wrapper_call_args + ");\n"
            wrapper = wrapper + "    return __ret ? dm_string_from_cstr(__ret) : dm_string_from_cstr(\"\");\n"
        } else if ret_type == "void" {
            wrapper = wrapper + "    " + fn_name + "(" + wrapper_call_args + ");\n"
        } else {
            wrapper = wrapper + "    return " + fn_name + "(" + wrapper_call_args + ");\n"
        }
        wrapper = wrapper + "}\n\n"

        -- We do NOT emit an extern declaration for string-typed externs because the
        -- real C function may already be declared in system headers (e.g., getenv in <stdlib.h>)
        -- and re-declaring with different types would cause C compilation errors.
        -- The function is expected to be available at link time.
        cc.fn_defs.push(wrapper)
        let _x1 = 0
    } else {
        -- Non-string extern: generate a simple dm_ wrapper that forwards to the real C function
        -- We do NOT emit an extern declaration because the function may already be declared
        -- in system headers (e.g., abs in <stdlib.h>) with different parameter types
        -- (C uses int, dAImond maps int to int64_t). The function will be resolved at link time.
        let mut wrapper_params = ""
        let mut call_args = ""
        let mut fi = 0
        while fi < param_types.len() {
            if fi > 0 {
                wrapper_params = wrapper_params + ", "
                call_args = call_args + ", "
            }
            let pt = "" + param_types[fi]
            let pn = "" + param_names[fi]
            wrapper_params = wrapper_params + pt + " " + dm_mangle(pn)
            call_args = call_args + dm_mangle(pn)
            fi = fi + 1
        }

        -- Generate dm_ wrapper that forwards to the real C function
        let mut wp = wrapper_params
        if wp == "" { wp = "void" }
        let mut wrapper = "static " + ret_type + " " + dm_mangle(fn_name) + "(" + wp + ") {\n"
        if ret_type == "void" {
            wrapper = wrapper + "    " + fn_name + "(" + call_args + ");\n"
        } else {
            wrapper = wrapper + "    return " + fn_name + "(" + call_args + ");\n"
        }
        wrapper = wrapper + "}\n\n"
        cc.fn_defs.push(wrapper)
        let _x2 = 0
    }

    -- Register function name and return type
    cc.fn_names.push(fn_name)
    cc.fn_ret_types.push(ret_type)

    return cc
}

fn compile_async_fn_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'async'
    -- The next token should be 'fn'
    if c_peek(cc) != TK_FN() {
        cc = c_error(cc, "expected 'fn' after 'async'")
        return cc
    }
    -- Set async flag before compiling function
    let saved_is_async = cc.current_fn_is_async
    let saved_async_inner = cc.current_fn_async_inner_type
    cc.current_fn_is_async = true
    cc.current_fn_async_inner_type = ""
    cc = compile_fn_decl(cc)
    -- Restore
    cc.current_fn_is_async = saved_is_async
    cc.current_fn_async_inner_type = saved_async_inner
    return cc
}

fn compile_const_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'const'
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
        if type_str == "" {
            if val.code == "true" or val.code == "false" {
                type_str = "bool"
            } else if string_contains(val.code, ".") {
                type_str = "double"
            } else {
                type_str = "int64_t"
            }
        }
        cc.fn_defs.push("static const " + type_str + " " + dm_mangle(var_name) + " = " + val.code + ";\n")
        cc = track_var_type(cc, dm_mangle(var_name), type_str)
    } else {
        -- Regular const (non-comptime): compile expression normally
        let val = compile_expr(cc)
        cc = val.c
        if type_str == "" {
            type_str = infer_type_from_code(val.code, cc)
        }
        cc.fn_defs.push("static const " + type_str + " " + dm_mangle(var_name) + " = " + val.code + ";\n")
        cc = track_var_type(cc, dm_mangle(var_name), type_str)
    }

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
        -- Prescan extern fn declarations
        if tok.kind == TK_EXTERN() and i + 2 < cc.tokens.len() {
            let next_tok = cc.tokens[i + 1]
            if next_tok.kind == TK_FN() {
                let ename_tok = cc.tokens[i + 2]
                if ename_tok.kind == TK_IDENT() {
                    let efn_name = ename_tok.value
                    -- Find return type by scanning for -> after )
                    let mut ej = i + 3
                    let mut edepth = 0
                    while ej < cc.tokens.len() {
                        let et = cc.tokens[ej]
                        if et.kind == TK_LPAREN() { edepth = edepth + 1 }
                        if et.kind == TK_RPAREN() {
                            edepth = edepth - 1
                            if edepth == 0 {
                                ej = ej + 1
                                break
                            }
                        }
                        ej = ej + 1
                    }
                    let mut eret_type = "void"
                    if ej < cc.tokens.len() {
                        let earrow_tok = cc.tokens[ej]
                        if earrow_tok.kind == TK_ARROW() {
                            ej = ej + 1
                            if ej < cc.tokens.len() {
                                let eret_tok = cc.tokens[ej]
                                if eret_tok.kind == TK_IDENT() {
                                    let eret_name = eret_tok.value
                                    if ej + 2 < cc.tokens.len() {
                                        let emaybe_bracket = cc.tokens[ej + 1]
                                        if emaybe_bracket.kind == TK_LBRACKET() {
                                            let einner_tok = cc.tokens[ej + 2]
                                            if einner_tok.kind == TK_IDENT() {
                                                let einner_c = map_dm_type(einner_tok.value)
                                                if eret_name == "List" {
                                                    eret_type = "dm_list_" + einner_c
                                                } else if eret_name == "Box" {
                                                    eret_type = einner_c + "*"
                                                } else if eret_name == "Option" {
                                                    eret_type = "dm_option_" + einner_c
                                                } else if eret_name == "Result" {
                                                    let mut esecond_c = "dm_string"
                                                    if ej + 4 < cc.tokens.len() {
                                                        let emaybe_comma = cc.tokens[ej + 3]
                                                        if emaybe_comma.kind == TK_COMMA() {
                                                            let esecond_tok = cc.tokens[ej + 4]
                                                            if esecond_tok.kind == TK_IDENT() {
                                                                esecond_c = map_dm_type(esecond_tok.value)
                                                            }
                                                        }
                                                    }
                                                    eret_type = "dm_result_" + einner_c + "_" + esecond_c
                                                } else {
                                                    eret_type = "dm_" + eret_name + "_" + einner_c
                                                }
                                            } else {
                                                eret_type = map_dm_type(eret_name)
                                            }
                                        } else {
                                            eret_type = map_dm_type(eret_name)
                                        }
                                    } else {
                                        eret_type = map_dm_type(eret_name)
                                    }
                                }
                            }
                        }
                    }
                    cc.fn_names.push(efn_name)
                    cc.fn_ret_types.push(eret_type)
                }
            }
        }
        -- Prescan async fn declarations: treat 'async fn' like 'fn' with offset+1
        if tok.kind == TK_ASYNC() and i + 2 < cc.tokens.len() {
            let next_tok = cc.tokens[i + 1]
            if next_tok.kind == TK_FN() {
                let aname_tok = cc.tokens[i + 2]
                if aname_tok.kind == TK_IDENT() {
                    let afn_name = aname_tok.value
                    -- Find return type by scanning for -> after )
                    let mut aj = i + 3
                    let mut adepth = 0
                    while aj < cc.tokens.len() {
                        let at = cc.tokens[aj]
                        if at.kind == TK_LPAREN() { adepth = adepth + 1 }
                        if at.kind == TK_RPAREN() {
                            adepth = adepth - 1
                            if adepth == 0 {
                                aj = aj + 1
                                break
                            }
                        }
                        aj = aj + 1
                    }
                    let mut aret_type = "void"
                    if aj < cc.tokens.len() {
                        let aarrow_tok = cc.tokens[aj]
                        if aarrow_tok.kind == TK_ARROW() {
                            aj = aj + 1
                            if aj < cc.tokens.len() {
                                let aret_tok = cc.tokens[aj]
                                if aret_tok.kind == TK_IDENT() {
                                    let aret_name = aret_tok.value
                                    if aj + 2 < cc.tokens.len() {
                                        let amaybe_bracket = cc.tokens[aj + 1]
                                        if amaybe_bracket.kind == TK_LBRACKET() {
                                            let ainner_tok = cc.tokens[aj + 2]
                                            if ainner_tok.kind == TK_IDENT() {
                                                let ainner_c = map_dm_type(ainner_tok.value)
                                                if aret_name == "Future" {
                                                    aret_type = "dm_future_" + ainner_c
                                                } else if aret_name == "List" {
                                                    aret_type = "dm_list_" + ainner_c
                                                } else if aret_name == "Box" {
                                                    aret_type = ainner_c + "*"
                                                } else if aret_name == "Option" {
                                                    aret_type = "dm_option_" + ainner_c
                                                } else {
                                                    aret_type = "dm_" + aret_name + "_" + ainner_c
                                                }
                                            } else {
                                                aret_type = map_dm_type(aret_name)
                                            }
                                        } else {
                                            aret_type = map_dm_type(aret_name)
                                        }
                                    } else {
                                        aret_type = map_dm_type(aret_name)
                                    }
                                }
                            }
                        }
                    }
                    cc.fn_names.push(afn_name)
                    cc.fn_ret_types.push(aret_type)
                }
            }
        }
        if tok.kind == TK_FN() and i + 1 < cc.tokens.len() {
            -- Skip if preceded by 'async' (already handled above)
            let mut skip_fn = false
            if i > 0 {
                let prev_tok = cc.tokens[i - 1]
                if prev_tok.kind == TK_ASYNC() {
                    skip_fn = true
                }
            }
            if skip_fn == false {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                let fn_name = name_tok.value

                -- Check for generic function: fn name[T](...) or fn name[T: Trait](...)
                let mut is_generic = false
                let mut type_params = ""
                if i + 2 < cc.tokens.len() {
                    let maybe_bracket = cc.tokens[i + 2]
                    if maybe_bracket.kind == TK_LBRACKET() {
                        is_generic = true
                        -- Collect type parameter names (skip trait bounds after colon)
                        let mut gj = i + 3
                        while gj < cc.tokens.len() and cc.tokens[gj].kind != TK_RBRACKET() {
                            if cc.tokens[gj].kind == TK_IDENT() {
                                let tp_tok = cc.tokens[gj]
                                let tp_name = tp_tok.value
                                if type_params != "" {
                                    type_params = type_params + ","
                                }
                                type_params = type_params + tp_name
                                gj = gj + 1
                                -- Check for trait bound: T: TraitName
                                if gj < cc.tokens.len() and cc.tokens[gj].kind == TK_COLON() {
                                    gj = gj + 1  -- skip ':'
                                    if gj < cc.tokens.len() and cc.tokens[gj].kind == TK_IDENT() {
                                        let bound_tok = cc.tokens[gj]
                                        let bound_name = bound_tok.value
                                        cc.generic_bounds = cc.generic_bounds + "|" + fn_name + ":" + tp_name + "=" + bound_name + "|"
                                        gj = gj + 1  -- skip trait name
                                    }
                                }
                            } else {
                                gj = gj + 1
                            }
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
                                            } else if ret_name == "Future" {
                                                ret_type = "dm_future_" + inner_c
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
        } else if k == TK_EXTERN() {
            cc = compile_extern_fn_decl(cc)
        } else if k == TK_CONST() {
            cc = compile_const_decl(cc)
        } else if k == TK_ASYNC() {
            cc = compile_async_fn_decl(cc)
        } else if k == TK_NEWLINE() {
            cc = c_advance(cc)
        } else {
            cc = c_error(cc, "unexpected token at top level: " + token_kind_name(k))
            cc = c_advance(cc)
        }
    }

    return cc
}
