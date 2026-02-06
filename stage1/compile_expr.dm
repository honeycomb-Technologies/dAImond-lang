module compile_expr

import compiler

-- ============================================================
-- EXPRESSION COMPILER (returns C expression string)
-- ============================================================

struct ExprOut {
    c: Compiler,
    code: string
}

fn compile_expr(c: Compiler) -> ExprOut {
    return compile_pipe_expr(c)
}

-- Returns the required effect for a builtin function name, or "" if none needed
fn get_builtin_effect(name: string) -> string {
    -- Console effect: stdout/stderr output
    if name == "println" { return "Console" }
    if name == "print" { return "Console" }
    if name == "eprintln" { return "Console" }
    if name == "eprint" { return "Console" }
    -- FileSystem effect: file I/O
    if name == "file_read" { return "FileSystem" }
    if name == "file_write" { return "FileSystem" }
    if name == "file_append" { return "FileSystem" }
    if name == "file_exists" { return "FileSystem" }
    -- IO effect: stdin input
    if name == "read_line" { return "IO" }
    -- Process effect: system calls and exit
    if name == "system" { return "Process" }
    if name == "exit" { return "Process" }
    -- No effect needed for all other builtins
    return ""
}

-- Check if the current function's effect set allows the given effect
fn check_effect_allowed(c: Compiler, builtin_name: string) -> Compiler {
    let mut cc = c
    if cc.current_fn_has_effects == false {
        -- No with [...] clause on this function: no enforcement (opt-in)
        return cc
    }
    let required = get_builtin_effect(builtin_name)
    if required == "" {
        -- This builtin has no effect requirement
        return cc
    }
    -- Check if the required effect is in the current_fn_effects pipe-delimited string
    let marker = "|" + required + "|"
    if string_contains(cc.current_fn_effects, marker) {
        return cc
    }
    -- Effect not allowed: report error and abort
    -- Extract the allowed effects from the pipe-delimited string for error message
    let effects_str = string_replace(cc.current_fn_effects, "|", ", ")
    let trimmed = string_trim(effects_str)
    let mut display = trimmed
    if starts_with(display, ", ") {
        display = substr(display, 2, len(display) - 2)
    }
    if ends_with(display, ", ") {
        display = substr(display, 0, len(display) - 2)
    }
    if display == "" {
        display = "(none)"
    }
    eprintln("Effect error: '" + builtin_name + "' requires '" + required + "' effect but function only allows [" + display + "]")
    panic("")
    return cc
}

-- Find the position of the outermost function call's opening paren.
-- Returns -1 if the expression is not a function call.
-- Skips past nested parens in the function name position (e.g. casts).
fn find_call_paren(code: string) -> int {
    let code_len = len(code)
    -- Must end with ')' to be a function call
    if code_len == 0 { return -1 }
    if char_at(code, code_len - 1) != ")" { return -1 }
    -- Walk backwards from the closing ')' to find the matching '('
    let mut depth = 0
    let mut i = code_len - 1
    while i >= 0 {
        let ch = char_at(code, i)
        if ch == ")" {
            depth = depth + 1
        } else if ch == "(" {
            depth = depth - 1
            if depth == 0 {
                -- This is the matching open paren
                if i > 0 { return i }
                return -1
            }
        }
        i = i - 1
    }
    return -1
}

fn compile_pipe_expr(c: Compiler) -> ExprOut {
    let mut result = compile_or_expr(c)
    while c_peek(result.c) == TK_PIPEGT() {
        result.c = c_advance(result.c)
        -- Parse the RHS: could be a bare identifier or a function call
        let rhs = compile_or_expr(result.c)
        result.c = rhs.c
        -- If the RHS is a function call f(args...), insert lhs as first arg
        let paren_pos = find_call_paren(rhs.code)
        if paren_pos >= 0 {
            let fn_part = substr(rhs.code, 0, paren_pos + 1)
            let args_part = substr(rhs.code, paren_pos + 1, len(rhs.code) - paren_pos - 1)
            -- args_part is "arg1, arg2)" or ")"
            if starts_with(args_part, ")") {
                -- No args: f() -> f(lhs)
                result.code = fn_part + result.code + ")"
            } else {
                -- Has args: f(args...) -> f(lhs, args...)
                result.code = fn_part + result.code + ", " + args_part
            }
        } else {
            -- Bare identifier: f -> f(lhs)
            result.code = rhs.code + "(" + result.code + ")"
        }
    }
    return result
}

fn compile_or_expr(c: Compiler) -> ExprOut {
    let mut result = compile_and_expr(c)
    while c_peek(result.c) == TK_OR() {
        result.c = c_advance(result.c)
        let rhs = compile_and_expr(result.c)
        result.c = rhs.c
        result.code = "(" + result.code + " || " + rhs.code + ")"
    }
    return result
}

fn compile_and_expr(c: Compiler) -> ExprOut {
    let mut result = compile_eq_expr(c)
    while c_peek(result.c) == TK_AND() {
        result.c = c_advance(result.c)
        let rhs = compile_eq_expr(result.c)
        result.c = rhs.c
        result.code = "(" + result.code + " && " + rhs.code + ")"
    }
    return result
}

fn compile_eq_expr(c: Compiler) -> ExprOut {
    let mut result = compile_cmp_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_EQEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_cmp_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_eq(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " == " + rhs.code + ")"
            }
        } else if k == TK_BANGEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_cmp_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "(!dm_string_eq(" + result.code + ", " + rhs.code + "))"
            } else {
                result.code = "(" + result.code + " != " + rhs.code + ")"
            }
        } else {
            cont = false
        }
    }
    return result
}

fn compile_cmp_expr(c: Compiler) -> ExprOut {
    let mut result = compile_add_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_LT() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_lt(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " < " + rhs.code + ")"
            }
        } else if k == TK_GT() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_gt(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " > " + rhs.code + ")"
            }
        } else if k == TK_LTEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_lteq(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " <= " + rhs.code + ")"
            }
        } else if k == TK_GTEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_gteq(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " >= " + rhs.code + ")"
            }
        } else {
            cont = false
        }
    }
    return result
}

fn compile_add_expr(c: Compiler) -> ExprOut {
    let mut result = compile_mul_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_PLUS() {
            result.c = c_advance(result.c)
            let rhs = compile_mul_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                if result.c.current_region_arena != "" {
                    result.code = "dm_string_concat_arena(" + result.c.current_region_arena + ", " + result.code + ", " + rhs.code + ")"
                } else {
                    result.code = "dm_string_concat(" + result.code + ", " + rhs.code + ")"
                }
            } else {
                result.code = "(" + result.code + " + " + rhs.code + ")"
            }
        } else if k == TK_MINUS() {
            result.c = c_advance(result.c)
            let rhs = compile_mul_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " - " + rhs.code + ")"
        } else {
            cont = false
        }
    }
    return result
}

fn compile_mul_expr(c: Compiler) -> ExprOut {
    let mut result = compile_unary_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_STAR() {
            result.c = c_advance(result.c)
            let rhs = compile_unary_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " * " + rhs.code + ")"
        } else if k == TK_SLASH() {
            result.c = c_advance(result.c)
            let rhs = compile_unary_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " / " + rhs.code + ")"
        } else if k == TK_PERCENT() {
            result.c = c_advance(result.c)
            let rhs = compile_unary_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " % " + rhs.code + ")"
        } else {
            cont = false
        }
    }
    return result
}

fn compile_unary_expr(c: Compiler) -> ExprOut {
    let cc = c_skip_nl(c)
    let k = c_peek(cc)
    if k == TK_MINUS() {
        let cc2 = c_advance(cc)
        let operand = compile_unary_expr(cc2)
        return ExprOut { c: operand.c, code: "(-" + operand.code + ")" }
    }
    if k == TK_NOT() {
        let cc2 = c_advance(cc)
        let operand = compile_unary_expr(cc2)
        return ExprOut { c: operand.c, code: "(!" + operand.code + ")" }
    }
    return compile_postfix_expr(cc)
}

fn compile_postfix_expr(c: Compiler) -> ExprOut {
    let mut result = compile_primary_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_DOT() {
            result.c = c_advance(result.c)
            let tok = c_cur(result.c)
            let field = tok.value
            result.c = c_advance(result.c)
            -- Check if this is an enum constructor: EnumName.Variant
            -- result.code will be "dm_EnumName" if the base was an enum ident
            let mut is_enum_ctor = false
            let mut enum_base_name = ""
            if starts_with(result.code, "dm_") {
                let base_name = substr(result.code, 3, len(result.code) - 3)
                if is_enum_name(result.c, base_name) {
                    let vinfo = lookup_enum_variant(result.c, base_name, field)
                    if vinfo != "" {
                        is_enum_ctor = true
                        enum_base_name = base_name
                    }
                }
            }
            if is_enum_ctor {
                -- Enum constructor
                let vinfo = lookup_enum_variant(result.c, enum_base_name, field)
                if c_peek(result.c) == TK_LPAREN() {
                    -- Constructor with args: EnumName.Variant(args)
                    result.c = c_advance(result.c)
                    let mut args_code = ""
                    let mut first = true
                    while c_peek(result.c) != TK_RPAREN() and c_peek(result.c) != TK_EOF() {
                        if first == false {
                            result.c = c_expect(result.c, TK_COMMA())
                            args_code = args_code + ", "
                        }
                        first = false
                        let arg = compile_expr(result.c)
                        result.c = arg.c
                        args_code = args_code + arg.code
                    }
                    result.c = c_expect(result.c, TK_RPAREN())
                    result.code = dm_mangle(enum_base_name) + "_" + field + "(" + args_code + ")"
                } else {
                    -- Unit constructor: EnumName.Variant
                    if vinfo == "unit" {
                        result.code = dm_mangle(enum_base_name) + "_" + field + "()"
                    } else {
                        result.code = dm_mangle(enum_base_name) + "_" + field
                    }
                }
            } else if c_peek(result.c) == TK_LPAREN() {
                -- Method call
                result.c = c_advance(result.c)
                let mut args_code = ""
                let mut first = true
                while c_peek(result.c) != TK_RPAREN() and c_peek(result.c) != TK_EOF() {
                    if first == false {
                        result.c = c_expect(result.c, TK_COMMA())
                        args_code = args_code + ", "
                    }
                    first = false
                    let arg = compile_expr(result.c)
                    result.c = arg.c
                    args_code = args_code + arg.code
                }
                result.c = c_expect(result.c, TK_RPAREN())
                -- Determine receiver type for map vs list dispatch
                let recv_method_type = infer_type_from_code(result.code, result.c)
                let is_map_recv = starts_with(recv_method_type, "dm_map_")
                -- Generate method call
                if field == "insert" and is_map_recv {
                    result.code = recv_method_type + "_insert(&" + result.code + ", " + args_code + ")"
                } else if field == "get" and is_map_recv {
                    result.code = recv_method_type + "_get(&" + result.code + ", " + args_code + ")"
                } else if field == "contains" and is_map_recv {
                    result.code = recv_method_type + "_contains(&" + result.code + ", " + args_code + ")"
                } else if field == "remove" and is_map_recv {
                    result.code = recv_method_type + "_remove(&" + result.code + ", " + args_code + ")"
                } else if field == "len" and is_map_recv {
                    result.code = "(int64_t)(" + result.code + ").len"
                } else if field == "keys" and is_map_recv {
                    result.code = recv_method_type + "_keys(&" + result.code + ")"
                } else if field == "values" and is_map_recv {
                    result.code = recv_method_type + "_values(&" + result.code + ")"
                } else if field == "set" and is_map_recv {
                    result.code = recv_method_type + "_insert(&" + result.code + ", " + args_code + ")"
                } else if field == "push" {
                    result.code = "DM_LIST_PUSH(" + result.code + ", (" + args_code + "))"
                } else if field == "len" {
                    result.code = "DM_LIST_LEN(" + result.code + ")"
                } else if field == "pop" {
                    result.code = "DM_LIST_POP(" + result.code + ")"
                } else if field == "contains" {
                    result.code = "DM_LIST_CONTAINS(" + result.code + ", " + args_code + ")"
                } else {
                    -- Check for dyn trait dispatch: receiver type starts with dm_dyn_
                    let receiver_type = infer_type_from_code(result.code, result.c)
                    let mut found_impl = false
                    if starts_with(receiver_type, "dm_dyn_") {
                        found_impl = true
                        -- Dispatch through vtable: dm_dyn_TraitName_method(receiver, args)
                        if args_code != "" {
                            result.code = receiver_type + "_" + field + "(" + result.code + ", " + args_code + ")"
                        } else {
                            result.code = receiver_type + "_" + field + "(" + result.code + ")"
                        }
                    }
                    -- Check for impl method: look up the receiver's type and try TypeName_method
                    if found_impl == false and starts_with(receiver_type, "dm_") {
                        let type_name = substr(receiver_type, 3, len(receiver_type) - 3)
                        -- Strip trailing * for pointer types
                        let mut clean_type = type_name
                        if ends_with(clean_type, "*") {
                            clean_type = substr(clean_type, 0, len(clean_type) - 1)
                        }
                        let impl_method = lookup_impl_method(result.c, clean_type, field)
                        if impl_method != "" {
                            found_impl = true
                            -- Pass receiver as pointer (self is always a pointer in impl methods)
                            if args_code != "" {
                                result.code = impl_method + "(&" + result.code + ", " + args_code + ")"
                            } else {
                                result.code = impl_method + "(&" + result.code + ")"
                            }
                        }
                    }
                    if found_impl == false {
                        if args_code != "" {
                            result.code = dm_mangle(field) + "(" + result.code + ", " + args_code + ")"
                        } else {
                            result.code = dm_mangle(field) + "(" + result.code + ")"
                        }
                    }
                }
            } else {
                -- Field access: check if receiver is a pointer (self in impl methods)
                let recv_type = infer_type_from_code(result.code, result.c)
                if ends_with(recv_type, "*") {
                    result.code = result.code + "->" + field
                } else {
                    result.code = result.code + "." + field
                }
            }
        } else if k == TK_QUESTION() {
            -- Error propagation: expr? unwraps Ok/Some or early-returns Err/None
            result.c = c_advance(result.c)
            let try_id = result.c.try_counter
            result.c.try_counter = result.c.try_counter + 1
            let tmp = "_try_" + int_to_string(try_id)
            let ind = indent_str(result.c.indent)
            let expr_type = infer_type_from_code(result.code, result.c)
            if starts_with(expr_type, "dm_result_") {
                -- Result[T, E]: check for Err, early-return it
                result.c.output = result.c.output + ind + expr_type + " " + tmp + " = " + result.code + ";\n"
                result.c.output = result.c.output + ind + "if (" + tmp + ".tag == " + expr_type + "_tag_Err) { return " + tmp + "; }\n"
                result.code = tmp + ".data.Ok._0"
                result.c = track_var_type(result.c, tmp, expr_type)
            } else if starts_with(expr_type, "dm_option_") {
                -- Option[T]: check for None, early-return it
                result.c.output = result.c.output + ind + expr_type + " " + tmp + " = " + result.code + ";\n"
                result.c.output = result.c.output + ind + "if (" + tmp + ".tag == " + expr_type + "_tag_None) { return " + tmp + "; }\n"
                result.code = tmp + ".data.Some._0"
                result.c = track_var_type(result.c, tmp, expr_type)
            } else {
                -- Unknown type: just pass through (C compiler will catch errors)
                result.c.output = result.c.output + ind + "// try operator on unknown type\n"
            }
        } else if k == TK_LBRACKET() {
            result.c = c_advance(result.c)
            let idx = compile_expr(result.c)
            result.c = idx.c
            result.c = c_expect(result.c, TK_RBRACKET())
            -- Check if receiver is a map type for index access
            let idx_recv_type = infer_type_from_code(result.code, result.c)
            if starts_with(idx_recv_type, "dm_map_") {
                result.code = idx_recv_type + "_get(&" + result.code + ", " + idx.code + ")"
            } else {
                result.code = "DM_LIST_GET(" + result.code + ", " + idx.code + ")"
            }
        } else if k == TK_LPAREN() and result.code != "" {
            -- Function call (only if we have a callee)
            result.c = c_advance(result.c)
            let mut args_code = ""
            let mut arg_types: List[string] = []
            let mut first = true
            while c_peek(result.c) != TK_RPAREN() and c_peek(result.c) != TK_EOF() {
                if first == false {
                    result.c = c_expect(result.c, TK_COMMA())
                    args_code = args_code + ", "
                }
                first = false
                result.c = c_skip_nl(result.c)
                let arg = compile_expr(result.c)
                result.c = arg.c
                args_code = args_code + arg.code
                arg_types.push(infer_type_from_code(arg.code, result.c))
            }
            result.c = c_expect(result.c, TK_RPAREN())
            -- Check for implicit generic function call: dm_name where name is generic
            let mut callee_code = result.code
            if starts_with(callee_code, "dm_") {
                let base_name = substr(callee_code, 3, len(callee_code) - 3)
                let ginfo = lookup_generic_fn(result.c, base_name)
                if ginfo != "" and arg_types.len() > 0 {
                    -- Infer type from first argument
                    let concrete = "" + arg_types[0]
                    result.c = monomorphize_generic_fn(result.c, base_name, ginfo, concrete)
                    callee_code = dm_mangle(base_name) + "_" + concrete
                }
            }
            result.code = callee_code + "(" + args_code + ")"
        } else if k == TK_AS() {
            -- Cast expression: expr as Type or expr as dyn TraitName
            result.c = c_advance(result.c)
            -- Check for "dyn" keyword for trait object cast
            let as_tok = c_cur(result.c)
            if as_tok.kind == TK_IDENT() and as_tok.value == "dyn" {
                result.c = c_advance(result.c)  -- skip 'dyn'
                let dyn_trait_tok = c_cur(result.c)
                let dyn_trait_name = dyn_trait_tok.value
                result.c = c_advance(result.c)  -- skip trait name
                -- Generate dyn trait infrastructure if not already done
                result.c = generate_dyn_trait_defs(result.c, dyn_trait_name)
                -- Infer concrete type of expression
                let concrete_type = infer_type_from_code(result.code, result.c)
                -- Strip dm_ prefix and trailing * to get the clean type name
                let mut clean_type = concrete_type
                if starts_with(clean_type, "dm_") {
                    clean_type = substr(clean_type, 3, len(clean_type) - 3)
                }
                if ends_with(clean_type, "*") {
                    clean_type = substr(clean_type, 0, len(clean_type) - 1)
                }
                -- Generate the vtable instance and fat pointer inline
                let dyn_type = "dm_dyn_" + dyn_trait_name
                let vtable_type = "dm_" + dyn_trait_name + "_vtable"
                let vtable_var = "_vtable_" + clean_type + "_" + dyn_trait_name
                -- Emit the static vtable variable (once) into lambda_defs
                let vtable_marker = "|" + vtable_var + "|"
                if string_contains(result.c.dyn_traits_generated, vtable_marker) == false {
                    result.c.dyn_traits_generated = result.c.dyn_traits_generated + vtable_marker
                    let mut vtable_init = ""
                    let search_prefix = "|" + dyn_trait_name + "."
                    let mut sp2 = string_find(result.c.trait_methods, search_prefix)
                    while sp2 >= 0 {
                        let after_p2 = sp2 + len(search_prefix)
                        let rest3 = substr(result.c.trait_methods, after_p2, len(result.c.trait_methods) - after_p2)
                        let eq_p2 = string_find(rest3, "=")
                        if eq_p2 >= 0 {
                            let mname2 = substr(rest3, 0, eq_p2)
                            let after_eq2 = substr(rest3, eq_p2 + 1, len(rest3) - eq_p2 - 1)
                            let pipe_p2 = string_find(after_eq2, "|")
                            let mut sig3 = after_eq2
                            if pipe_p2 >= 0 {
                                sig3 = substr(after_eq2, 0, pipe_p2)
                            }
                            let colon_p2 = string_find(sig3, ":")
                            let mut mret2 = "void"
                            let mut mparams2 = ""
                            if colon_p2 >= 0 {
                                mret2 = substr(sig3, 0, colon_p2)
                                mparams2 = substr(sig3, colon_p2 + 1, len(sig3) - colon_p2 - 1)
                            }
                            let mut cast_fptr_params = "void*"
                            if mparams2 != "" {
                                cast_fptr_params = cast_fptr_params + ", " + mparams2
                            }
                            if vtable_init != "" {
                                vtable_init = vtable_init + ", "
                            }
                            vtable_init = vtable_init + "." + mname2 + " = (" + mret2 + " (*)(" + cast_fptr_params + "))dm_" + clean_type + "_" + mname2
                        }
                        let next_s2 = sp2 + len(search_prefix)
                        let next_rest2 = substr(result.c.trait_methods, next_s2, len(result.c.trait_methods) - next_s2)
                        let nf2 = string_find(next_rest2, search_prefix)
                        if nf2 < 0 {
                            sp2 = 0 - 1
                        } else {
                            sp2 = next_s2 + nf2
                        }
                    }
                    let vtable_def = "static " + vtable_type + " " + vtable_var + " = { " + vtable_init + " };\n"
                    result.c.lambda_defs = result.c.lambda_defs + vtable_def
                }
                result.code = "(" + dyn_type + "){ .data = (void*)&" + result.code + ", .vtable = &" + vtable_var + " }"
            } else {
                let cast_type = parse_type_for_c(result.c)
                result.c = cast_type.c
                result.code = "((" + cast_type.code + ")(" + result.code + "))"
            }
        } else {
            cont = false
        }
    }
    return result
}

-- Try to compile struct literal. Returns ExprOut with code="" if not a struct literal.
fn try_compile_struct_lit(c: Compiler, name: string) -> ExprOut {
    if c_peek(c) != TK_LBRACE() {
        return ExprOut { c: c, code: "" }
    }
    if is_struct_name(c, name) == false {
        return ExprOut { c: c, code: "" }
    }
    return compile_struct_lit(c, name)
}

fn compile_struct_lit(c: Compiler, type_name: string) -> ExprOut {
    let mut cc = c_advance(c)  -- skip '{'
    let mut code = "(dm_" + type_name + "){ "
    let mut first = true
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        if first == false {
            cc = c_expect(cc, TK_COMMA())
            code = code + ", "
        }
        first = false
        cc = c_skip_nl(cc)
        let field_tok = c_cur(cc)
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let val = compile_expr(cc)
        cc = val.c
        code = code + "." + field_tok.value + " = " + val.code
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    code = code + " }"
    return ExprOut { c: cc, code: code }
}

-- Compile lambda expression: |params| expr or |params| { block }
-- Lifts the lambda to a static function at file scope
fn compile_lambda_expr(c: Compiler) -> ExprOut {
    let mut cc = c_advance(c)  -- skip opening '|'

    -- Parse parameters: |x: int, y: string| or || for no params
    let mut param_names: List[string] = []
    let mut param_types: List[string] = []
    let mut params_c = ""
    let mut first = true

    while c_peek(cc) != TK_PIPE() and c_peek(cc) != TK_EOF() {
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
        param_names.push(pname)
        param_types.push(pt.code)
        params_c = params_c + pt.code + " " + dm_mangle(pname)
    }
    cc = c_advance(cc)  -- skip closing '|'

    -- Determine lambda name
    let lambda_id = cc.lambda_counter
    cc.lambda_counter = lambda_id + 1
    let lambda_name = "_lambda_" + int_to_string(lambda_id)

    -- Check for block body { ... } vs expression body
    let mut is_block = c_peek(cc) == TK_LBRACE()

    if is_block {
        -- Block body: |params| { stmts }
        cc = c_expect(cc, TK_LBRACE())
        -- Save compiler state for body compilation
        let saved_output = cc.output
        let saved_indent = cc.indent
        let saved_fn_ret = cc.current_fn_ret_type
        cc.output = ""
        cc.indent = 1

        -- Track lambda parameter types
        let mut pi = 0
        while pi < param_names.len() {
            let pn = "" + param_names[pi]
            let pt = "" + param_types[pi]
            cc = track_var_type(cc, dm_mangle(pn), pt)
            cc = track_str_var(cc, dm_mangle(pn), pt == "dm_string")
            pi = pi + 1
        }

        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())

        let body_code = cc.output
        cc.output = saved_output
        cc.indent = saved_indent
        cc.current_fn_ret_type = saved_fn_ret

        -- For block lambdas, try to infer return type from the body
        -- Look for "return <expr>;" pattern in body to determine type
        -- Default to void if no return found
        let mut block_ret_type = "void"
        let ret_pos = string_find(body_code, "return ")
        let has_expected = cc.expected_type != ""
        if ret_pos >= 0 and has_expected {
            block_ret_type = cc.expected_type
        } else if ret_pos >= 0 {
            block_ret_type = "int64_t"
        }

        -- Generate lambda definition
        let mut lambda_def = "static " + block_ret_type + " " + lambda_name + "("
        if params_c == "" {
            lambda_def = lambda_def + "void"
        } else {
            lambda_def = lambda_def + params_c
        }
        lambda_def = lambda_def + ") {\n" + body_code + "}\n\n"
        cc.lambda_defs = cc.lambda_defs + lambda_def

        -- Build and track function pointer type
        let mut fptr_type = build_fptr_type(block_ret_type, param_types)
        cc = track_var_type(cc, lambda_name, fptr_type)

        return ExprOut { c: cc, code: lambda_name }
    }

    -- Expression body: |params| expr
    -- Track lambda parameter types
    let mut pi2 = 0
    while pi2 < param_names.len() {
        let pn = "" + param_names[pi2]
        let pt = "" + param_types[pi2]
        cc = track_var_type(cc, dm_mangle(pn), pt)
        cc = track_str_var(cc, dm_mangle(pn), pt == "dm_string")
        pi2 = pi2 + 1
    }

    let body_expr = compile_expr(cc)
    cc = body_expr.c

    -- Infer return type from expression
    let ret_type = infer_type_from_code(body_expr.code, cc)

    -- Generate the lifted function
    let mut lambda_def = "static " + ret_type + " " + lambda_name + "("
    if params_c == "" {
        lambda_def = lambda_def + "void"
    } else {
        lambda_def = lambda_def + params_c
    }
    lambda_def = lambda_def + ") {\n    return " + body_expr.code + ";\n}\n\n"
    cc.lambda_defs = cc.lambda_defs + lambda_def

    -- Build and track function pointer type
    let mut fptr_type = build_fptr_type(ret_type, param_types)
    cc = track_var_type(cc, lambda_name, fptr_type)

    return ExprOut { c: cc, code: lambda_name }
}

-- Emit a function pointer variable declaration: "ret_type (*name)(params)"
-- from a type string like "ret_type (*)(params)" and a variable name
fn emit_fptr_decl(fptr_type: string, var_name: string) -> string {
    -- fptr_type is like "int64_t (*)(int64_t, dm_string)"
    -- We need to insert var_name between (* and )
    -- Find "(*)" and replace with "(*var_name)"
    return string_replace(fptr_type, "(*)", "(*" + var_name + ")")
}

-- Build a C function pointer type string: "ret_type (*)(param_types...)"
fn build_fptr_type(ret_type: string, param_types: List[string]) -> string {
    let mut fptr = ret_type + " (*)("
    if param_types.len() == 0 {
        fptr = fptr + "void"
    } else {
        let mut ti = 0
        while ti < param_types.len() {
            if ti > 0 {
                fptr = fptr + ", "
            }
            fptr = fptr + param_types[ti]
            ti = ti + 1
        }
    }
    fptr = fptr + ")"
    return fptr
}

-- Look up generic function info: returns "type_params:start:end" or "" if not generic
fn lookup_generic_fn(c: Compiler, name: string) -> string {
    let marker = "|" + name + "="
    let pos = string_find(c.generic_fn_tokens, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.generic_fn_tokens, start, len(c.generic_fn_tokens) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

-- Check if a function name is a generic function
fn is_generic_fn(c: Compiler, name: string) -> bool {
    let info = lookup_generic_fn(c, name)
    return info != ""
}

-- Monomorphize a generic function for concrete type arguments.
-- generic_info is "T:start_idx:end_idx" (from lookup_generic_fn)
-- concrete_types is "int64_t" or "dm_string" etc (comma-separated for multiple type params)
-- Returns updated Compiler with the monomorphized function added to fn_defs/fn_sigs
fn monomorphize_generic_fn(c: Compiler, fn_name: string, generic_info: string, concrete_types: string) -> Compiler {
    let mut cc = c

    -- Parse generic_info: "T:start:end" or "T,U:start:end"
    let first_colon = string_find(generic_info, ":")
    if first_colon < 0 { return cc }
    let type_params_str = substr(generic_info, 0, first_colon)
    let rest_after_params = substr(generic_info, first_colon + 1, len(generic_info) - first_colon - 1)
    let second_colon = string_find(rest_after_params, ":")
    if second_colon < 0 { return cc }
    let start_str = substr(rest_after_params, 0, second_colon)
    let end_str = substr(rest_after_params, second_colon + 1, len(rest_after_params) - second_colon - 1)
    let tok_start = parse_int(start_str)
    let tok_end = parse_int(end_str)

    -- Build mangled name: dm_fn_name_concreteType
    let mangled = dm_mangle(fn_name) + "_" + concrete_types
    let mono_marker = "|" + mangled + "|"

    -- Check if already monomorphized
    if string_contains(cc.monomorphized_fns, mono_marker) { return cc }
    cc.monomorphized_fns = cc.monomorphized_fns + mono_marker

    -- Extract tokens for this generic function and create substituted copy
    -- The token range [tok_start..tok_end) covers "fn name[T](...) -> Ret { body }"
    -- We need to:
    -- 1. Copy these tokens with T replaced by concrete type
    -- 2. Replace the function name with the mangled version
    -- 3. Skip the [T] part
    -- 4. Compile the result as a regular function

    let mut new_tokens: List[Token] = []
    let mut ti = tok_start
    let mut skip_bracket = false

    while ti < tok_end {
        let t = cc.tokens[ti]

        -- Skip the [T] generic parameter declaration
        if t.kind == TK_LBRACKET() and ti == tok_start + 2 {
            -- Skip until ]
            ti = ti + 1
            while ti < tok_end and cc.tokens[ti].kind != TK_RBRACKET() {
                ti = ti + 1
            }
            ti = ti + 1  -- skip ]
            continue
        }

        -- Replace function name with monomorphized name (without dm_ prefix — compile_fn_decl will add it)
        if t.kind == TK_IDENT() and t.value == fn_name and ti == tok_start + 1 {
            let mono_name = fn_name + "_" + concrete_types
            new_tokens.push(token_new(TK_IDENT(), mono_name, t.line, t.col))
            ti = ti + 1
            continue
        }

        -- Replace type parameter with concrete type
        if t.kind == TK_IDENT() and t.value == type_params_str {
            -- Map concrete type back to dAImond type name for the parser
            let mut dm_type_name = concrete_types
            if concrete_types == "int64_t" {
                dm_type_name = "int"
            } else if concrete_types == "dm_string" {
                dm_type_name = "string"
            } else if concrete_types == "bool" {
                dm_type_name = "bool"
            } else if concrete_types == "double" {
                dm_type_name = "float"
            }
            new_tokens.push(token_new(TK_IDENT(), dm_type_name, t.line, t.col))
            ti = ti + 1
            continue
        }

        new_tokens.push(t)
        ti = ti + 1
    }

    -- Add EOF token
    new_tokens.push(token_new(TK_EOF(), "", 0, 0))

    -- Compile the monomorphized function using a temporary compiler
    let mut temp_cc = compiler_new(new_tokens)
    -- Copy over important state from the real compiler
    temp_cc.struct_names = cc.struct_names
    temp_cc.enum_names = cc.enum_names
    temp_cc.fn_names = cc.fn_names
    temp_cc.fn_ret_types = cc.fn_ret_types
    temp_cc.struct_fields = cc.struct_fields
    temp_cc.enum_variants = cc.enum_variants
    temp_cc.var_types = cc.var_types
    temp_cc.str_vars = cc.str_vars
    temp_cc.list_type_defs = cc.list_type_defs
    temp_cc.map_type_defs = cc.map_type_defs
    temp_cc.map_kv_types = cc.map_kv_types
    temp_cc.option_type_defs = cc.option_type_defs
    temp_cc.future_type_defs = cc.future_type_defs
    temp_cc.list_elem_types = cc.list_elem_types
    temp_cc.generic_fn_tokens = cc.generic_fn_tokens
    temp_cc.monomorphized_fns = cc.monomorphized_fns
    temp_cc.trait_methods = cc.trait_methods
    temp_cc.trait_names = cc.trait_names
    temp_cc.impl_for_map = cc.impl_for_map
    temp_cc.dyn_trait_defs = cc.dyn_trait_defs
    temp_cc.dyn_traits_generated = cc.dyn_traits_generated
    temp_cc.array_type_defs = cc.array_type_defs
    -- Propagate counters to avoid name collisions with the caller context
    temp_cc.lambda_counter = cc.lambda_counter
    temp_cc.match_counter = cc.match_counter
    temp_cc.for_counter = cc.for_counter
    temp_cc.try_counter = cc.try_counter

    -- Skip 'fn' keyword to enter compile_fn_decl properly
    temp_cc = compile_fn_decl(temp_cc)

    -- Copy generated function back to real compiler
    let mut fi = 0
    while fi < temp_cc.fn_sigs.len() {
        cc.fn_sigs.push(temp_cc.fn_sigs[fi])
        fi = fi + 1
    }
    fi = 0
    while fi < temp_cc.fn_defs.len() {
        cc.fn_defs.push(temp_cc.fn_defs[fi])
        fi = fi + 1
    }
    -- Copy monomorphized tracking
    cc.monomorphized_fns = temp_cc.monomorphized_fns
    -- Copy any new list/option/map type defs
    cc.list_type_defs = temp_cc.list_type_defs
    cc.map_type_defs = temp_cc.map_type_defs
    cc.map_kv_types = temp_cc.map_kv_types
    cc.option_type_defs = temp_cc.option_type_defs
    cc.future_type_defs = temp_cc.future_type_defs
    cc.dyn_trait_defs = temp_cc.dyn_trait_defs
    cc.dyn_traits_generated = temp_cc.dyn_traits_generated
    cc.array_type_defs = temp_cc.array_type_defs
    -- Copy fn_names/ret_types (the monomorphized fn was added)
    cc.fn_names = temp_cc.fn_names
    cc.fn_ret_types = temp_cc.fn_ret_types
    -- Copy counters back so subsequent code doesn't collide
    cc.lambda_counter = temp_cc.lambda_counter
    cc.match_counter = temp_cc.match_counter
    cc.for_counter = temp_cc.for_counter
    cc.try_counter = temp_cc.try_counter
    -- Copy lambda definitions generated inside the generic function
    cc.lambda_defs = cc.lambda_defs + temp_cc.lambda_defs

    return cc
}

-- Compile f-string interpolation: f"Hello {name}, x = {x + 1}"
-- Parses the raw content between quotes, splits on {expr} placeholders,
-- and generates dm_string_concat chains with auto-conversion for non-string types
fn compile_fstring(c: Compiler, content: string) -> ExprOut {
    let mut cc = c
    let content_len = len(content)
    let mut result_code = ""
    let mut i = 0
    let mut seg_start = 0

    while i < content_len {
        let ch = char_at(content, i)
        if ch == "{" {
            -- Emit the literal segment before this placeholder
            if i > seg_start {
                let literal = substr(content, seg_start, i - seg_start)
                let lit_code = "dm_string_from_cstr(\"" + literal + "\")"
                if result_code == "" {
                    result_code = lit_code
                } else {
                    if cc.current_region_arena != "" {
                        result_code = "dm_string_concat_arena(" + cc.current_region_arena + ", " + result_code + ", " + lit_code + ")"
                    } else {
                        result_code = "dm_string_concat(" + result_code + ", " + lit_code + ")"
                    }
                }
            }
            -- Find matching }
            i = i + 1
            let expr_start = i
            let mut depth = 1
            while i < content_len and depth > 0 {
                let c2 = char_at(content, i)
                if c2 == "{" { depth = depth + 1 }
                if c2 == "}" { depth = depth - 1 }
                if depth > 0 { i = i + 1 }
            }
            -- Extract the expression text
            let expr_text = substr(content, expr_start, i - expr_start)
            i = i + 1  -- skip closing }
            seg_start = i

            -- Tokenize and compile the sub-expression
            let expr_tokens = tokenize(expr_text)
            let mut temp_cc = compiler_new(expr_tokens)
            -- Copy important state from the outer compiler
            temp_cc.str_vars = cc.str_vars
            temp_cc.var_types = cc.var_types
            temp_cc.struct_fields = cc.struct_fields
            temp_cc.struct_names = cc.struct_names
            temp_cc.enum_names = cc.enum_names
            temp_cc.fn_names = cc.fn_names
            temp_cc.fn_ret_types = cc.fn_ret_types
            temp_cc.list_elem_types = cc.list_elem_types
            temp_cc.enum_variants = cc.enum_variants
            temp_cc.trait_methods = cc.trait_methods
            temp_cc.trait_names = cc.trait_names
            temp_cc.impl_for_map = cc.impl_for_map
            temp_cc.generic_fn_tokens = cc.generic_fn_tokens
            temp_cc.monomorphized_fns = cc.monomorphized_fns
            temp_cc.current_region_arena = cc.current_region_arena
            let expr_result = compile_expr(temp_cc)
            let expr_code = expr_result.code

            -- Auto-convert to string if needed
            let mut str_expr = expr_code
            if code_is_string(expr_code, cc) == false {
                let etype = infer_type_from_code(expr_code, cc)
                if etype == "int64_t" {
                    str_expr = "dm_int_to_string(" + expr_code + ")"
                } else if etype == "double" {
                    str_expr = "dm_float_to_string(" + expr_code + ")"
                } else if etype == "bool" {
                    str_expr = "dm_bool_to_string(" + expr_code + ")"
                } else {
                    -- Fallback: try int_to_string
                    str_expr = "dm_int_to_string(" + expr_code + ")"
                }
            }

            if result_code == "" {
                result_code = str_expr
            } else {
                if cc.current_region_arena != "" {
                    result_code = "dm_string_concat_arena(" + cc.current_region_arena + ", " + result_code + ", " + str_expr + ")"
                } else {
                    result_code = "dm_string_concat(" + result_code + ", " + str_expr + ")"
                }
            }
        } else {
            i = i + 1
        }
    }

    -- Emit any remaining literal tail
    if seg_start < content_len {
        let literal = substr(content, seg_start, content_len - seg_start)
        let lit_code = "dm_string_from_cstr(\"" + literal + "\")"
        if result_code == "" {
            result_code = lit_code
        } else {
            if cc.current_region_arena != "" {
                result_code = "dm_string_concat_arena(" + cc.current_region_arena + ", " + result_code + ", " + lit_code + ")"
            } else {
                result_code = "dm_string_concat(" + result_code + ", " + lit_code + ")"
            }
        }
    }

    if result_code == "" {
        result_code = "dm_string_from_cstr(\"\")"
    }

    return ExprOut { c: cc, code: result_code }
}

-- ============================================================
-- COMPTIME EXPRESSION EVALUATOR
-- Evaluates constant expressions at compile time
-- ============================================================

fn eval_comptime_expr(c: Compiler) -> ExprOut {
    return eval_comptime_or(c)
}

fn eval_comptime_or(c: Compiler) -> ExprOut {
    let mut result = eval_comptime_and(c)
    while c_peek(result.c) == TK_OR() {
        result.c = c_advance(result.c)
        let rhs = eval_comptime_and(result.c)
        result.c = rhs.c
        let l = result.code == "true" or result.code == "1"
        let r = rhs.code == "true" or rhs.code == "1"
        if l or r {
            result.code = "true"
        } else {
            result.code = "false"
        }
    }
    return result
}

fn eval_comptime_and(c: Compiler) -> ExprOut {
    let mut result = eval_comptime_add(c)
    while c_peek(result.c) == TK_AND() {
        result.c = c_advance(result.c)
        let rhs = eval_comptime_add(result.c)
        result.c = rhs.c
        let l = result.code == "true" or result.code == "1"
        let r = rhs.code == "true" or rhs.code == "1"
        if l and r {
            result.code = "true"
        } else {
            result.code = "false"
        }
    }
    return result
}

fn eval_comptime_add(c: Compiler) -> ExprOut {
    let mut result = eval_comptime_mul(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_PLUS() {
            result.c = c_advance(result.c)
            let rhs = eval_comptime_mul(result.c)
            result.c = rhs.c
            let lv = parse_int(result.code)
            let rv = parse_int(rhs.code)
            result.code = int_to_string(lv + rv)
        } else if k == TK_MINUS() {
            result.c = c_advance(result.c)
            let rhs = eval_comptime_mul(result.c)
            result.c = rhs.c
            let lv = parse_int(result.code)
            let rv = parse_int(rhs.code)
            result.code = int_to_string(lv - rv)
        } else {
            cont = false
        }
    }
    return result
}

fn eval_comptime_mul(c: Compiler) -> ExprOut {
    let mut result = eval_comptime_primary(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_STAR() {
            result.c = c_advance(result.c)
            let rhs = eval_comptime_primary(result.c)
            result.c = rhs.c
            let lv = parse_int(result.code)
            let rv = parse_int(rhs.code)
            result.code = int_to_string(lv * rv)
        } else if k == TK_SLASH() {
            result.c = c_advance(result.c)
            let rhs = eval_comptime_primary(result.c)
            result.c = rhs.c
            let lv = parse_int(result.code)
            let rv = parse_int(rhs.code)
            if rv != 0 {
                result.code = int_to_string(lv / rv)
            } else {
                result.code = "0"
            }
        } else if k == TK_PERCENT() {
            result.c = c_advance(result.c)
            let rhs = eval_comptime_primary(result.c)
            result.c = rhs.c
            let lv = parse_int(result.code)
            let rv = parse_int(rhs.code)
            if rv != 0 {
                result.code = int_to_string(lv % rv)
            } else {
                result.code = "0"
            }
        } else {
            cont = false
        }
    }
    return result
}

fn eval_comptime_primary(c: Compiler) -> ExprOut {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)
    let k = tok.kind
    if k == TK_INTEGER() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: tok.value }
    }
    if k == TK_FLOAT() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: tok.value }
    }
    if k == TK_TRUE() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "true" }
    }
    if k == TK_FALSE() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "false" }
    }
    if k == TK_NOT() {
        cc = c_advance(cc)
        let operand = eval_comptime_primary(cc)
        cc = operand.c
        if operand.code == "true" or operand.code == "1" {
            return ExprOut { c: cc, code: "false" }
        } else {
            return ExprOut { c: cc, code: "true" }
        }
    }
    if k == TK_MINUS() {
        cc = c_advance(cc)
        let operand = eval_comptime_primary(cc)
        cc = operand.c
        let v = parse_int(operand.code)
        return ExprOut { c: cc, code: int_to_string(0 - v) }
    }
    if k == TK_LPAREN() {
        cc = c_advance(cc)
        let inner = eval_comptime_expr(cc)
        cc = inner.c
        cc = c_expect(cc, TK_RPAREN())
        return ExprOut { c: cc, code: inner.code }
    }
    -- Unknown token - just return 0
    cc = c_advance(cc)
    return ExprOut { c: cc, code: "0" }
}

fn compile_primary_expr(c: Compiler) -> ExprOut {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)
    let k = tok.kind

    if k == TK_INTEGER() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: tok.value }
    }
    if k == TK_FLOAT() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: tok.value }
    }
    if k == TK_STRING() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "dm_string_from_cstr(\"" + tok.value + "\")" }
    }
    if k == TK_FSTRING() {
        cc = c_advance(cc)
        return compile_fstring(cc, tok.value)
    }
    if k == TK_TRUE() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "true" }
    }
    if k == TK_FALSE() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "false" }
    }
    if k == TK_IDENT() {
        let name = tok.value
        cc = c_advance(cc)
        -- Check for builtin function mapping
        if name == "println" {
            cc = check_effect_allowed(cc, "println")
            return ExprOut { c: cc, code: "dm_println_str" }
        }
        if name == "print" {
            cc = check_effect_allowed(cc, "print")
            return ExprOut { c: cc, code: "dm_print_str" }
        }
        if name == "eprintln" {
            cc = check_effect_allowed(cc, "eprintln")
            return ExprOut { c: cc, code: "dm_eprintln_str" }
        }
        if name == "eprint" {
            cc = check_effect_allowed(cc, "eprint")
            return ExprOut { c: cc, code: "dm_eprint_str" }
        }
        if name == "int_to_string" {
            return ExprOut { c: cc, code: "dm_int_to_string" }
        }
        if name == "bool_to_string" {
            return ExprOut { c: cc, code: "dm_bool_to_string" }
        }
        if name == "float_to_string" {
            return ExprOut { c: cc, code: "dm_float_to_string" }
        }
        if name == "char_at" {
            return ExprOut { c: cc, code: "dm_char_at" }
        }
        if name == "len" {
            return ExprOut { c: cc, code: "dm_len" }
        }
        if name == "substr" {
            return ExprOut { c: cc, code: "dm_string_substr" }
        }
        if name == "parse_int" {
            return ExprOut { c: cc, code: "dm_parse_int" }
        }
        if name == "parse_float" {
            return ExprOut { c: cc, code: "dm_parse_float" }
        }
        if name == "string_contains" {
            return ExprOut { c: cc, code: "dm_string_contains" }
        }
        if name == "string_find" {
            return ExprOut { c: cc, code: "dm_string_find" }
        }
        if name == "starts_with" {
            return ExprOut { c: cc, code: "dm_string_starts_with" }
        }
        if name == "ends_with" {
            return ExprOut { c: cc, code: "dm_string_ends_with" }
        }
        if name == "string_replace" {
            return ExprOut { c: cc, code: "dm_string_replace" }
        }
        if name == "string_trim" {
            return ExprOut { c: cc, code: "dm_string_trim" }
        }
        if name == "string_to_upper" {
            return ExprOut { c: cc, code: "dm_string_to_upper" }
        }
        if name == "string_to_lower" {
            return ExprOut { c: cc, code: "dm_string_to_lower" }
        }
        if name == "file_read" {
            cc = check_effect_allowed(cc, "file_read")
            return ExprOut { c: cc, code: "dm_file_read" }
        }
        if name == "file_write" {
            cc = check_effect_allowed(cc, "file_write")
            return ExprOut { c: cc, code: "dm_file_write" }
        }
        if name == "file_append" {
            cc = check_effect_allowed(cc, "file_append")
            return ExprOut { c: cc, code: "dm_append_file" }
        }
        if name == "file_exists" {
            cc = check_effect_allowed(cc, "file_exists")
            return ExprOut { c: cc, code: "dm_file_exists" }
        }
        if name == "read_line" {
            cc = check_effect_allowed(cc, "read_line")
            return ExprOut { c: cc, code: "dm_read_line" }
        }
        if name == "exit" {
            cc = check_effect_allowed(cc, "exit")
            return ExprOut { c: cc, code: "dm_exit" }
        }
        if name == "system" {
            cc = check_effect_allowed(cc, "system")
            return ExprOut { c: cc, code: "dm_system" }
        }
        if name == "args_get" {
            return ExprOut { c: cc, code: "dm_args_get" }
        }
        if name == "args_len" {
            return ExprOut { c: cc, code: "dm_args_len" }
        }
        if name == "panic" {
            return ExprOut { c: cc, code: "dm_panic" }
        }
        -- assert(cond) builtin: inline expansion to check + panic
        if name == "assert" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let assert_cond = compile_expr(cc)
                cc = assert_cond.c
                cc = c_expect(cc, TK_RPAREN())
                let assert_code = "do { if (!(" + assert_cond.code + ")) { fprintf(stderr, \"PANIC: assertion failed\\n\"); exit(1); } } while(0)"
                return ExprOut { c: cc, code: assert_code }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        -- assert_eq(a, b) builtin: inline expansion with type-aware comparison
        if name == "assert_eq" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let aeq_a = compile_expr(cc)
                cc = aeq_a.c
                cc = c_expect(cc, TK_COMMA())
                let aeq_b = compile_expr(cc)
                cc = aeq_b.c
                cc = c_expect(cc, TK_RPAREN())
                if code_is_string(aeq_a.code, cc) or code_is_string(aeq_b.code, cc) {
                    let aeq_code = "do { if (!dm_string_eq(" + aeq_a.code + ", " + aeq_b.code + ")) { fprintf(stderr, \"PANIC: assert_eq failed\\n\"); exit(1); } } while(0)"
                    return ExprOut { c: cc, code: aeq_code }
                } else {
                    let aeq_code = "do { if ((" + aeq_a.code + ") != (" + aeq_b.code + ")) { fprintf(stderr, \"PANIC: assert_eq failed\\n\"); exit(1); } } while(0)"
                    return ExprOut { c: cc, code: aeq_code }
                }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        -- string_split(s, delim) builtin: register list type and emit helper
        if name == "string_split" {
            cc = register_list_type(cc, "dm_list_dm_string", "dm_string")
            if string_contains(cc.lambda_defs, "dm_string_split") == false {
                let mut split_fn = "static dm_list_dm_string dm_string_split(dm_string s, dm_string delim) {\n"
                split_fn = split_fn + "    dm_list_dm_string result = dm_list_dm_string_new();\n"
                split_fn = split_fn + "    if (delim.len == 0) {\n"
                split_fn = split_fn + "        DM_LIST_PUSH(result, s);\n"
                split_fn = split_fn + "        return result;\n"
                split_fn = split_fn + "    }\n"
                split_fn = split_fn + "    size_t start = 0;\n"
                split_fn = split_fn + "    for (size_t i = 0; i + delim.len <= s.len; i++) {\n"
                split_fn = split_fn + "        if (memcmp(s.data + i, delim.data, delim.len) == 0) {\n"
                split_fn = split_fn + "            size_t seg_len = i - start;\n"
                split_fn = split_fn + "            char* buf = (char*)malloc(seg_len + 1);\n"
                split_fn = split_fn + "            memcpy(buf, s.data + start, seg_len);\n"
                split_fn = split_fn + "            buf[seg_len] = '\\0';\n"
                split_fn = split_fn + "            DM_LIST_PUSH(result, ((dm_string){ .data = buf, .len = seg_len, .capacity = seg_len }));\n"
                split_fn = split_fn + "            i += delim.len - 1;\n"
                split_fn = split_fn + "            start = i + 1;\n"
                split_fn = split_fn + "        }\n"
                split_fn = split_fn + "    }\n"
                split_fn = split_fn + "    size_t last_len = s.len - start;\n"
                split_fn = split_fn + "    char* last_buf = (char*)malloc(last_len + 1);\n"
                split_fn = split_fn + "    memcpy(last_buf, s.data + start, last_len);\n"
                split_fn = split_fn + "    last_buf[last_len] = '\\0';\n"
                split_fn = split_fn + "    DM_LIST_PUSH(result, ((dm_string){ .data = last_buf, .len = last_len, .capacity = last_len }));\n"
                split_fn = split_fn + "    return result;\n"
                split_fn = split_fn + "}\n\n"
                cc.lambda_defs = cc.lambda_defs + split_fn
            }
            return ExprOut { c: cc, code: "dm_string_split" }
        }
        -- Box_new(val) and Box_null() builtins
        if name == "Box_new" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let arg = compile_expr(cc)
                cc = arg.c
                cc = c_expect(cc, TK_RPAREN())
                let val_type = infer_type_from_code(arg.code, cc)
                let box_code = "({ " + val_type + "* _bp = (" + val_type + "*)malloc(sizeof(" + val_type + ")); *_bp = " + arg.code + "; _bp; })"
                return ExprOut { c: cc, code: box_code }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "Box_null" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                cc = c_expect(cc, TK_RPAREN())
                return ExprOut { c: cc, code: "NULL" }
            }
            return ExprOut { c: cc, code: "NULL" }
        }
        -- SIMD splat builtins: simd_splat_TYPE(val)
        if starts_with(name, "simd_splat_") {
            let simd_type = substr(name, 11, len(name) - 11)
            let c_fn = "dm_simd_splat_" + simd_type
            return ExprOut { c: cc, code: c_fn }
        }
        -- SIMD set builtins: simd_set_TYPE(a, b, ...)
        if starts_with(name, "simd_set_") {
            let simd_type = substr(name, 9, len(name) - 9)
            let c_fn = "dm_simd_set_" + simd_type
            return ExprOut { c: cc, code: c_fn }
        }
        -- SIMD arithmetic builtins: simd_add, simd_sub, simd_mul, simd_div
        if name == "simd_add" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let arg_a = compile_expr(cc)
                cc = arg_a.c
                cc = c_expect(cc, TK_COMMA())
                let arg_b = compile_expr(cc)
                cc = arg_b.c
                cc = c_expect(cc, TK_RPAREN())
                return ExprOut { c: cc, code: "(" + arg_a.code + " + " + arg_b.code + ")" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "simd_sub" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let arg_a = compile_expr(cc)
                cc = arg_a.c
                cc = c_expect(cc, TK_COMMA())
                let arg_b = compile_expr(cc)
                cc = arg_b.c
                cc = c_expect(cc, TK_RPAREN())
                return ExprOut { c: cc, code: "(" + arg_a.code + " - " + arg_b.code + ")" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "simd_mul" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let arg_a = compile_expr(cc)
                cc = arg_a.c
                cc = c_expect(cc, TK_COMMA())
                let arg_b = compile_expr(cc)
                cc = arg_b.c
                cc = c_expect(cc, TK_RPAREN())
                return ExprOut { c: cc, code: "(" + arg_a.code + " * " + arg_b.code + ")" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "simd_div" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let arg_a = compile_expr(cc)
                cc = arg_a.c
                cc = c_expect(cc, TK_COMMA())
                let arg_b = compile_expr(cc)
                cc = arg_b.c
                cc = c_expect(cc, TK_RPAREN())
                return ExprOut { c: cc, code: "(" + arg_a.code + " / " + arg_b.code + ")" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        -- SIMD extract: simd_extract(vec, idx) -> vec[idx]
        if name == "simd_extract" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                let arg_vec = compile_expr(cc)
                cc = arg_vec.c
                cc = c_expect(cc, TK_COMMA())
                let arg_idx = compile_expr(cc)
                cc = arg_idx.c
                cc = c_expect(cc, TK_RPAREN())
                return ExprOut { c: cc, code: arg_vec.code + "[" + arg_idx.code + "]" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "Map_new" {
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)
                cc = c_expect(cc, TK_RPAREN())
                -- Use expected_type to determine the map type
                let map_type = cc.expected_type
                if map_type != "" and starts_with(map_type, "dm_map_") {
                    return ExprOut { c: cc, code: map_type + "_new()" }
                }
                return ExprOut { c: cc, code: "/* Map_new unknown type */" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        -- Option/Result constructors: Some, None, Ok, Err
        -- Use expected_type or current_fn_ret_type for type context
        if name == "Some" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_option_") {
                -- Some(value) -> dm_option_T_Some(value)
                -- The value will be parsed by the subsequent LPAREN in postfix
                return ExprOut { c: cc, code: etype + "_Some" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "None" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_option_") {
                return ExprOut { c: cc, code: etype + "_None()" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "Ok" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_result_") {
                return ExprOut { c: cc, code: etype + "_Ok" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "Err" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_result_") {
                return ExprOut { c: cc, code: etype + "_Err" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        -- Check for struct literal: Name { field: value, ... }
        -- Only parse as struct literal if name is a known struct type
        let struct_try = try_compile_struct_lit(cc, name)
        if struct_try.code != "" {
            return struct_try
        }
        -- Check for generic function with explicit type params: name[Type](...)
        let is_gen = is_generic_fn(cc, name)
        if is_gen and c_peek(cc) == TK_LBRACKET() {
            cc = c_advance(cc)  -- skip '['
            -- Parse the concrete type argument
            let ct = parse_type_for_c(cc)
            cc = ct.c
            cc = c_expect(cc, TK_RBRACKET())
            -- Monomorphize the function
            let ginfo = lookup_generic_fn(cc, name)
            cc = monomorphize_generic_fn(cc, name, ginfo, ct.code)
            -- Return the mangled name so the call compiles normally
            return ExprOut { c: cc, code: dm_mangle(name) + "_" + ct.code }
        }
        -- Regular identifier -> dm_ prefix for user functions
        return ExprOut { c: cc, code: dm_mangle(name) }
    }
    if k == TK_LPAREN() {
        cc = c_advance(cc)
        let inner = compile_expr(cc)
        cc = inner.c
        cc = c_expect(cc, TK_RPAREN())
        return ExprOut { c: cc, code: "(" + inner.code + ")" }
    }
    if k == TK_LBRACKET() {
        -- Array repeat literal: [value; N]
        cc = c_advance(cc)  -- skip '['
        let arr_val = compile_expr(cc)
        cc = arr_val.c
        if c_peek(cc) == TK_SEMICOLON() {
            cc = c_advance(cc)  -- skip ';'
            let arr_size_tok = c_cur(cc)
            let arr_size = arr_size_tok.value
            cc = c_advance(cc)  -- skip N
            cc = c_expect(cc, TK_RBRACKET())
            -- Infer element type from value
            let arr_elem_type = infer_type_from_code(arr_val.code, cc)
            let arr_type = "dm_array_" + arr_elem_type + "_" + arr_size
            cc = register_array_type(cc, arr_elem_type, arr_size)
            -- Build compound literal: (dm_array_T_N){val, val, ..., val}
            let arr_n = parse_int(arr_size)
            let mut arr_init = ""
            let mut arr_i = 0
            while arr_i < arr_n {
                if arr_i > 0 {
                    arr_init = arr_init + ", "
                }
                arr_init = arr_init + arr_val.code
                arr_i = arr_i + 1
            }
            let arr_code = "((" + arr_type + "){" + arr_init + "})"
            return ExprOut { c: cc, code: arr_code }
        }
        -- Otherwise unexpected (list literals handled in compile_let_stmt)
        cc = c_expect(cc, TK_RBRACKET())
        return ExprOut { c: cc, code: "0 /* unexpected [ expr */" }
    }
    if k == TK_SELF() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "dm_self" }
    }
    if k == TK_MATCH() {
        return compile_match_expr(cc)
    }
    if k == TK_PIPE() {
        return compile_lambda_expr(cc)
    }
    if k == TK_AWAIT() {
        cc = c_advance(cc)  -- skip 'await'
        let await_expr = compile_postfix_expr(cc)
        cc = await_expr.c
        -- Infer the future type from the expression
        let expr_type = infer_type_from_code(await_expr.code, cc)
        if starts_with(expr_type, "dm_future_") {
            return ExprOut { c: cc, code: expr_type + "_await(" + await_expr.code + ")" }
        }
        -- Fallback: if type inference can't determine the future type, try from the expression code
        -- For simple variable references, look up the variable type
        return ExprOut { c: cc, code: await_expr.code }
    }
    -- Unknown primary
    cc = c_error(cc, "unexpected token in expression: " + token_kind_name(k))
    cc = c_advance(cc)
    return ExprOut { c: cc, code: "0 /* error */" }
}
