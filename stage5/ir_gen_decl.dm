module ir_gen_decl

import ast
import ir
import ir_builder
import ir_gen

-- ============================================================
-- STRUCT REGISTRATION
-- Registers struct types in the IR module and struct_defs map
-- ============================================================

fn register_struct(gen: IRGenerator, sd: StructDecl) -> IRGenerator {
    let mut g = gen
    let mut info = struct_info_new(sd.name)

    let mut ir_fields: List[IRField] = []
    let mut i = 0
    while i < sd.fields.len() {
        let f = sd.fields[i]
        let fname = f.name
        g = ensure_type_instances(g, f.type_name, sd.line)
        let ftype = canon_type(g, f.type_name)
        let ir_type_name = map_type_name(ftype)

        info.field_names.push(fname)
        info.field_types.push(ir_type_name)
        info.field_src_types.push(ftype)

        let field = ir_field_new(fname, ir_type_name)
        ir_fields.push(field)

        -- Detect List[T] fields for field access dispatch
        if starts_with(ftype, "List[") {
            let inner = extract_inner_type(ftype, "List[")
            let elem_kind = classify_list_elem(inner)
            let field_key = sd.name + "." + fname
            g.field_list_elem_kinds.insert(field_key, elem_kind)
            if elem_kind == LIST_ELEM_OTHER() {
                g.field_list_elem_types.insert(field_key, inner)
            }
        }

        i = i + 1
    }

    -- Create IR struct type
    let mut ir_ty = ir_type_named(IR_TYPE_STRUCT(), sd.name)
    ir_ty.fields = ir_fields

    g.struct_defs.insert(sd.name, info)
    g.ir_mod.struct_defs.insert(sd.name, ir_ty)

    return g
}

-- Register every generic struct instance a source type names,
-- including instances nested in its type arguments and fields.
-- line locates errors in the type.
fn ensure_type_instances(gen: IRGenerator, t: string, line: int) -> IRGenerator {
    let mut g = gen
    if g.type_substitutions.contains(t) {
        return g
    }
    let bracket = string_find(t, "[")
    if bracket <= 0 or ends_with(t, "]") == false {
        return g
    }
    let base = substr(t, 0, bracket)
    let raw_args = split_type_args(substr(t, bracket + 1, len(t) - bracket - 2))
    let mut args: List[string] = []
    let mut i = 0
    while i < raw_args.len() {
        g = ensure_type_instances(g, raw_args[i], line)
        args.push(canon_type(g, raw_args[i]))
        i = i + 1
    }
    if g.generic_struct_decls.contains(base) == false {
        return g
    }
    let instance = generic_instance_name(base, args)
    if g.struct_defs.contains(instance) or g.enum_defs.contains(instance) {
        return g
    }
    let sd_idx: int = g.generic_struct_decls.get(base)
    let decl = g.declarations[sd_idx]
    if decl.kind == DECL_ENUM() {
        return register_enum_instance(g, *decl.enum_decl, instance, args, t, line)
    }
    let generic_sd = *decl.struct_decl
    if args.len() != generic_sd.generic_params.len() {
        g = ir_type_error(g, "'" + base + "' takes " + int_to_string(generic_sd.generic_params.len()) + " type arguments, got " + int_to_string(args.len()) + " in '" + t + "'", line)
        return g
    }
    let mut arg_list = ""
    i = 0
    while i < args.len() {
        if i > 0 { arg_list = arg_list + "|" }
        arg_list = arg_list + args[i]
        i = i + 1
    }
    g.generic_instances.insert(instance, base)
    g.generic_instance_args.insert(instance, arg_list)

    -- The instance is the generic struct with its arguments substituted
    let mut sd = generic_sd
    sd.name = instance
    let empty_params: List[string] = []
    sd.generic_params = empty_params
    let mut fields: List[StructField] = []
    i = 0
    while i < generic_sd.fields.len() {
        let mut f = generic_sd.fields[i]
        f.type_name = instance_type(g, instance, f.type_name)
        fields.push(f)
        i = i + 1
    }
    sd.fields = fields
    g = register_struct(g, sd)

    -- Field types can name further instances
    let saved_subs = g.type_substitutions
    g.type_substitutions = instance_subs(g, instance)
    i = 0
    while i < generic_sd.fields.len() {
        g = ensure_type_instances(g, generic_sd.fields[i].type_name, generic_sd.line)
        i = i + 1
    }
    g.type_substitutions = saved_subs
    return g
}

-- ============================================================
-- ENUM REGISTRATION
-- ============================================================

-- Register `instance`, the generic enum with `args` for its type
-- parameters: Opt_i64 for Opt[int]. `t` is the type as it was written.
fn register_enum_instance(gen: IRGenerator, generic_ed: EnumDecl, instance: string, args: List[string], t: string, line: int) -> IRGenerator {
    let mut g = gen
    let base = generic_ed.name
    if args.len() != generic_ed.generic_params.len() {
        return ir_type_error(g, "'" + base + "' takes " + int_to_string(generic_ed.generic_params.len()) + " type arguments, got " + int_to_string(args.len()) + " in '" + t + "'", line)
    }
    let mut arg_list = ""
    let mut i = 0
    while i < args.len() {
        if i > 0 { arg_list = arg_list + "|" }
        arg_list = arg_list + args[i]
        i = i + 1
    }
    g.generic_instances.insert(instance, base)
    g.generic_instance_args.insert(instance, arg_list)

    -- The instance is the generic enum with its arguments substituted
    let mut ed = generic_ed
    ed.name = instance
    let empty_params: List[string] = []
    ed.generic_params = empty_params
    let mut variants: List[EnumVariant] = []
    i = 0
    while i < generic_ed.variants.len() {
        let mut v = generic_ed.variants[i]
        let mut payload_types: List[string] = []
        let mut pi = 0
        while pi < v.payload_types.len() {
            payload_types.push(instance_type(g, instance, v.payload_types[pi]))
            pi = pi + 1
        }
        v.payload_types = payload_types
        variants.push(v)
        i = i + 1
    }
    ed.variants = variants
    g = register_enum(g, ed)

    -- The types of the values can name further instances
    let saved_subs = g.type_substitutions
    g.type_substitutions = instance_subs(g, instance)
    i = 0
    while i < generic_ed.variants.len() {
        let v = generic_ed.variants[i]
        let mut pi = 0
        while pi < v.payload_types.len() {
            g = ensure_type_instances(g, v.payload_types[pi], generic_ed.line)
            pi = pi + 1
        }
        i = i + 1
    }
    g.type_substitutions = saved_subs
    return g
}

fn register_enum(gen: IRGenerator, ed: EnumDecl) -> IRGenerator {
    let mut g = gen
    let mut info = enum_info_new(ed.name)

    let mut ir_fields: List[IRField] = []
    -- First field: tag (i32)
    ir_fields.push(ir_field_new("tag", "i32"))

    let mut i = 0
    while i < ed.variants.len() {
        let v = ed.variants[i]
        info.variant_names.push(v.name)
        info.variant_tags.push(i)

        -- Check for payload types
        if v.has_payload and v.payload_types.len() > 0 {
            -- A field of the struct for each value the variant carries
            let mut joined = ""
            let mut pi = 0
            while pi < v.payload_types.len() {
                g = ensure_type_instances(g, v.payload_types[pi], ed.line)
                let pt = canon_type(g, v.payload_types[pi])
                if pi == 0 {
                    info.variant_payload_types.push(pt)
                } else {
                    joined = joined + "|"
                }
                joined = joined + pt
                ir_fields.push(ir_field_new(v.name + "_" + int_to_string(pi), map_type_name(pt)))
                pi = pi + 1
            }
            info.variant_has_payload.push(true)
            info.variant_payloads.push(joined)
        } else {
            info.variant_payload_types.push("")
            info.variant_has_payload.push(false)
            info.variant_payloads.push("")
        }

        i = i + 1
    }

    -- If no payload fields, add dummy payload
    if ir_fields.len() == 1 {
        ir_fields.push(ir_field_new("payload", "i64"))
    }

    let mut ir_ty = ir_type_named(IR_TYPE_STRUCT(), ed.name)
    ir_ty.fields = ir_fields

    g.enum_defs.insert(ed.name, info)
    g.ir_mod.struct_defs.insert(ed.name, ir_ty)

    return g
}

-- ============================================================
-- IMPL BLOCK REGISTRATION
-- Registers method-to-struct mappings and trait impl methods
-- ============================================================

fn register_impl_methods(gen: IRGenerator, ib: ImplBlock) -> IRGenerator {
    let mut g = gen
    let struct_name = ib.target_type

    -- If this is a trait impl, register trait methods
    if len(ib.trait_name) > 0 {
        let key = struct_name + ":" + ib.trait_name
        let mut method_list = ""
        let mut i = 0
        while i < ib.methods.len() {
            if i > 0 {
                method_list = method_list + "|"
            }
            method_list = method_list + ib.methods[i].name
            i = i + 1
        }
        g.trait_impl_methods.insert(key, method_list)
    }

    -- Register method->struct mappings
    let mut i = 0
    while i < ib.methods.len() {
        -- A generic impl's methods are found through the receiver's
        -- instance type, never by method name alone
        if ib.generic_params.len() == 0 {
            g.method_struct_map.insert(ib.methods[i].name, struct_name)
            let method = ib.methods[i]
            let mut takes_self = false
            if method.params.len() > 0 {
                takes_self = method.params[0].name == "self"
            }
            if takes_self == false {
                g.static_fns.insert(struct_name + "_" + method.name, true)
            }
        }

        -- For generic impl blocks, register each method as a generic fn decl
        -- so it can be monomorphized when called
        if ib.generic_params.len() > 0 {
            let mangled = struct_name + "_" + ib.methods[i].name
            -- Create a modified FunctionDecl with the impl's generic params
            let mut method_fd = ib.methods[i]
            method_fd.generic_params = ib.generic_params
            -- Push it as a synthetic declaration and record its index
            let decl_idx = g.declarations.len()
            g.declarations.push(decl_function(method_fd))
            g.generic_fn_decls.insert(mangled, decl_idx)
        }

        i = i + 1
    }

    return g
}

-- ============================================================
-- RUNTIME FUNCTION DECLARATIONS
-- Declares all extern builtin functions used by the compiler
-- ============================================================

fn declare_runtime_functions(gen: IRGenerator) -> IRGenerator {
    let mut g = gen

    -- I/O
    g = declare_extern(g, "dm_println", "string", "void")
    g = declare_extern(g, "dm_print", "string", "void")
    g = declare_extern(g, "dm_eprintln", "string", "void")
    g = declare_extern(g, "dm_eprint", "string", "void")

    -- Conversion
    g = declare_extern(g, "dm_int_to_string", "i64", "string")
    g = declare_extern(g, "dm_uint_to_string", "u64", "string")
    g = declare_extern(g, "dm_float_to_string", "f64", "string")
    g = declare_extern(g, "dm_bool_to_string", "bool", "string")
    g = declare_extern(g, "dm_parse_int", "string", "i64")
    g = declare_extern(g, "dm_parse_float", "string", "f64")

    -- String operations
    g = declare_extern2(g, "dm_string_concat", "string", "string", "string")
    g = declare_extern2(g, "dm_string_eq", "string", "string", "bool")
    g = declare_extern(g, "dm_string_len", "string", "i64")
    g = declare_extern2(g, "dm_string_contains", "string", "string", "bool")
    g = declare_extern2(g, "dm_string_cmp", "string", "string", "i64")
    g = declare_extern2(g, "dm_string_find", "string", "string", "i64")
    g = declare_extern2(g, "dm_char_at", "string", "i64", "string")
    g = declare_extern3(g, "dm_substr", "string", "i64", "i64", "string")
    g = declare_extern2(g, "dm_starts_with", "string", "string", "bool")
    g = declare_extern2(g, "dm_ends_with", "string", "string", "bool")
    g = declare_extern(g, "dm_string_trim", "string", "string")
    g = declare_extern3(g, "dm_string_replace", "string", "string", "string", "string")
    g = declare_extern(g, "dm_string_to_upper", "string", "string")
    g = declare_extern(g, "dm_string_to_lower", "string", "string")

    -- Character classification and conversion
    g = declare_extern(g, "dm_is_alpha", "string", "bool")
    g = declare_extern(g, "dm_is_digit", "string", "bool")
    g = declare_extern(g, "dm_is_whitespace", "string", "bool")
    g = declare_extern(g, "dm_is_alnum", "string", "bool")
    g = declare_extern(g, "dm_char_to_string", "string", "string")

    -- List contains
    g = declare_extern2(g, "dm_list_int64_contains", "ptr", "i64", "bool")
    g = declare_extern2(g, "dm_list_string_contains", "ptr", "string", "bool")
    g = declare_extern2(g, "dm_list_double_contains", "ptr", "f64", "bool")

    -- Process
    g = declare_extern(g, "dm_panic", "string", "void")
    g = declare_extern(g, "dm_assert", "bool", "void")
    g = declare_extern(g, "dm_assert_eq_values", "bool", "void")
    g = declare_extern(g, "dm_assert_eq_strings", "bool", "void")
    g = declare_extern2(g, "dm_checked_add_i64", "i64", "i64", "i64")
    g = declare_extern2(g, "dm_checked_sub_i64", "i64", "i64", "i64")
    g = declare_extern2(g, "dm_checked_mul_i64", "i64", "i64", "i64")
    g = declare_extern3(g, "dm_checked_range_i64", "i64", "i64", "i64", "i64")
    g = declare_extern3(g, "dm_cast_check_i64", "i64", "i64", "i64", "i64")
    g = declare_extern2(g, "dm_cast_check_u64", "u64", "u64", "u64")
    g = declare_libc(g, "exit", "i64", "void")

    -- Stdin
    g = declare_extern0(g, "dm_read_line", "string")

    -- System
    g = declare_extern(g, "dm_system", "string", "i64")
    g = declare_extern4(g, "dm_run_command", "ptr", "string", "string", "string", "i64")
    g = declare_extern0(g, "dm_args_len", "i64")
    g = declare_extern(g, "dm_args_get", "i64", "string")
    g = declare_extern0(g, "dm_start", "void")
    g = declare_extern0(g, "dm_mem_mark", "i64")
    g = declare_extern(g, "dm_mem_sweep", "i64", "void")

    -- File I/O
    g = declare_extern(g, "dm_file_read", "string", "string")
    g = declare_extern2(g, "dm_file_write", "string", "string", "void")
    g = declare_extern2(g, "dm_file_append", "string", "string", "void")
    g = declare_extern(g, "dm_file_exists", "string", "bool")

    -- Path utilities
    g = declare_extern(g, "dm_path_dirname", "string", "string")
    g = declare_extern(g, "dm_path_basename", "string", "string")
    g = declare_extern(g, "dm_path_extension", "string", "string")
    g = declare_extern(g, "dm_path_stem", "string", "string")
    g = declare_extern2(g, "dm_path_join", "string", "string", "string")

    -- Memory allocation
    g = declare_libc(g, "malloc", "i64", "ptr")

    -- List operations (int64)
    g = declare_extern(g, "dm_list_int64_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_int64_push", "ptr", "i64", "void")
    g = declare_extern2(g, "dm_list_int64_get", "ptr", "i64", "i64")
    g = declare_extern(g, "dm_list_int64_len", "ptr", "i64")
    g = declare_extern(g, "dm_list_int64_pop", "ptr", "i64")

    -- List operations (string)
    g = declare_extern(g, "dm_list_string_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_string_push", "ptr", "string", "void")
    g = declare_extern2(g, "dm_list_string_get", "ptr", "i64", "string")
    g = declare_extern(g, "dm_list_string_len", "ptr", "i64")
    g = declare_extern(g, "dm_list_string_pop", "ptr", "string")

    -- List operations (double)
    g = declare_extern(g, "dm_list_double_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_double_push", "ptr", "f64", "void")
    g = declare_extern2(g, "dm_list_double_get", "ptr", "i64", "f64")
    g = declare_extern(g, "dm_list_double_len", "ptr", "i64")
    g = declare_extern(g, "dm_list_double_pop", "ptr", "f64")

    -- List operations (generic)
    g = declare_extern(g, "dm_list_generic_new", "ptr", "void")
    g = declare_extern3(g, "dm_list_generic_push", "ptr", "ptr", "i64", "void")
    g = declare_extern4(g, "dm_list_generic_get", "ptr", "ptr", "i64", "i64", "void")
    g = declare_extern(g, "dm_list_generic_len", "ptr", "i64")
    g = declare_extern3(g, "dm_list_generic_pop", "ptr", "ptr", "i64", "void")

    -- Filesystem
    g = declare_extern(g, "dm_fs_mkdir", "string", "i64")
    g = declare_extern(g, "dm_fs_readdir", "string", "string")
    g = declare_extern(g, "dm_fs_remove", "string", "i64")
    g = declare_extern2(g, "dm_fs_rename", "string", "string", "i64")
    g = declare_extern0(g, "dm_fs_getcwd", "string")

    -- OS
    g = declare_extern(g, "dm_os_getenv", "string", "string")

    -- Terminal and time
    g = declare_extern(g, "dm_chr", "i64", "string")
    g = declare_extern0(g, "dm_flush", "void")
    g = declare_extern0(g, "dm_read_key", "i64")
    g = declare_extern0(g, "dm_read_key_nb", "i64")
    g = declare_extern0(g, "dm_term_width", "i64")
    g = declare_extern0(g, "dm_term_height", "i64")
    g = declare_extern0(g, "dm_time_ms", "i64")
    g = declare_extern(g, "dm_sleep_ms", "i64", "void")

    -- Network
    g = declare_extern(g, "dm_tcp_listen", "string", "i64")
    g = declare_extern(g, "dm_tcp_accept", "i64", "i64")
    g = declare_extern(g, "dm_tcp_connect", "string", "i64")
    g = declare_extern2(g, "dm_tcp_read", "i64", "i64", "string")
    g = declare_extern2(g, "dm_tcp_write", "i64", "string", "i64")
    g = declare_extern(g, "dm_tcp_close", "i64", "void")

    -- Threads. A thread's function reaches the runtime as its address.
    g = declare_extern(g, "dm_thread_spawn", "ptr", "i64")
    g = declare_extern(g, "dm_thread_join", "i64", "void")
    g = declare_extern0(g, "dm_mutex_new", "i64")
    g = declare_extern(g, "dm_mutex_lock", "i64", "void")
    g = declare_extern(g, "dm_mutex_unlock", "i64", "void")
    g = declare_extern(g, "dm_mutex_destroy", "i64", "void")

    -- String split
    g = declare_extern3(g, "dm_string_split", "ptr", "string", "string", "void")

    return g
}

-- Helper: declare extern with 0 params
fn declare_extern0(gen: IRGenerator, name: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 1 param
fn declare_extern(gen: IRGenerator, name: string, p1: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    g.ir_mod.functions.push(func)
    return g
}

-- Declare libc function `symbol`, which the compiler itself calls, as
-- dm_libc_<symbol>. A program's own function or extern named `symbol`
-- is then a different function, with its own signature.
fn declare_libc(gen: IRGenerator, symbol: string, p1: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new("dm_libc_" + symbol, ret)
    func.is_extern = true
    func.link_name = symbol
    func.params.push(ir_param_new("p0", p1))
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 2 params
fn declare_extern2(gen: IRGenerator, name: string, p1: string, p2: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    func.params.push(ir_param_new("p1", p2))
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 3 params
fn declare_extern3(gen: IRGenerator, name: string, p1: string, p2: string, p3: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    func.params.push(ir_param_new("p1", p2))
    func.params.push(ir_param_new("p2", p3))
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 4 params
fn declare_extern4(gen: IRGenerator, name: string, p1: string, p2: string, p3: string, p4: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    func.params.push(ir_param_new("p1", p2))
    func.params.push(ir_param_new("p2", p3))
    func.params.push(ir_param_new("p3", p4))
    g.ir_mod.functions.push(func)
    return g
}

-- ============================================================
-- FUNCTION GENERATION
-- ============================================================

fn generate_function(gen: IRGenerator, fd: FunctionDecl) -> IRGenerator {
    let mut g = gen

    -- Skip generic functions (monomorphized at call site)
    if fd.generic_params.len() > 0 {
        return g
    }

    let name = fd.name

    -- Declare function if not already present
    g = declare_function(g, fd, name)

    -- Skip extern functions without body
    if fd.is_extern {
        return g
    }

    -- The precompiled runtime object holds this function's body
    if g.linked_fns.contains(name) {
        return g
    }

    -- Generate function body (variable source types are per function)
    let saved_src_types = g.var_src_types
    g.var_src_types = Map_new()
    g = generate_function_body(g, fd, name)
    g.var_src_types = saved_src_types

    return g
}

fn declare_function(gen: IRGenerator, fd: FunctionDecl, name: string) -> IRGenerator {
    let mut g = gen

    let ret_src = canon_type(g, fd.return_type)
    let ret_type = if len(ret_src) > 0 {
        map_type_name(ret_src)
    } else {
        "void"
    }

    let mut func = ir_function_new(name, ret_type)
    func.source_line = fd.line
    func.is_extern = fd.is_extern
    func.is_variadic = fd.is_variadic
    func.link_name = fd.link_name
    if fd.is_variadic and ret_type == "string" {
        g = ir_type_error(g, "variadic extern fn '" + name + "' cannot return string; declare it as returning int, the address of the C string", fd.line)
    }

    -- Build parameter list
    let mut mut_flags = ""
    let mut i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        let pname = p.name
        -- Resolve "Self" to current impl type
        let mut ptype_str = if p.type_name == "Self" and g.has_impl_type {
            g.current_impl_type
        } else {
            p.type_name
        }
        ptype_str = canon_type(g, ptype_str)
        let ptype = map_type_name(ptype_str)

        let actual_type = if p.is_mut {
            "ptr"
        } else {
            ptype
        }

        func.params.push(ir_param_new(pname, actual_type))

        -- Build mut flags string
        if i > 0 {
            mut_flags = mut_flags + "|"
        }
        if p.is_mut {
            mut_flags = mut_flags + "1"
        } else {
            mut_flags = mut_flags + "0"
        }

        i = i + 1
    }

    -- An extern fn declared in source is a C function: its string
    -- parameters and return are char*, whatever its name (Stage 0 does
    -- the same). The compiler's own runtime declarations don't come here.
    if fd.is_extern {
        g.user_extern_fns.insert(name, true)
        g.ir_mod.user_extern_fns.insert(name, true)
    }

    -- Track mut params
    if string_contains(mut_flags, "1") {
        g.mut_param_fns.insert(name, mut_flags)
    }

    -- A source extern fn replaces the compiler's own declaration of the
    -- same runtime function; calls convert pointers and ints between the
    -- two signatures
    if fd.is_extern {
        let mut fi = 0
        while fi < g.ir_mod.functions.len() {
            if g.ir_mod.functions[fi].name == name and g.ir_mod.functions[fi].is_extern {
                g.ir_mod.functions[fi] = func
                return g
            }
            fi = fi + 1
        }
    }

    g.ir_mod.functions.push(func)

    return g
}

fn generate_function_body(gen: IRGenerator, fd: FunctionDecl, name: string) -> IRGenerator {
    let mut g = gen

    -- Find the function in the module
    let mut func_idx = -1
    let mut i = 0
    while i < g.ir_mod.functions.len() {
        let f = g.ir_mod.functions[i]
        if f.name == name {
            func_idx = i
        }
        i = i + 1
    }

    if func_idx < 0 {
        return g
    }

    -- Set current return type
    let ret_src = canon_type(g, fd.return_type)
    let ret_type = if len(ret_src) > 0 {
        map_type_name(ret_src)
    } else {
        "void"
    }
    g.current_return_type = ret_type

    -- Reset variable maps for this function
    g.variable_map = Map_new()
    g.variable_types = Map_new()
    g.mut_ptr_vars = Map_new()
    g.param_ptr_map = Map_new()
    -- What is known about each variable is keyed by its name, so it must
    -- not outlive the function: a `c: Color` in one function says nothing
    -- about a `c` in the next. The caller's are put back at the end, for
    -- a function generated in the middle of another (a generic instance).
    let outer_var_src_types = g.var_src_types
    let outer_var_struct_types = g.var_struct_types
    let outer_var_enum_types = g.var_enum_types
    let outer_box_inner_types = g.box_inner_types
    let outer_list_elem_kinds = g.list_elem_kinds
    let outer_list_elem_types = g.list_elem_types
    let outer_fn_ptr_vars = g.fn_ptr_vars
    let outer_array_var_names = g.array_var_names
    g.var_src_types = Map_new()
    g.var_struct_types = Map_new()
    g.var_enum_types = Map_new()
    g.box_inner_types = Map_new()
    g.list_elem_kinds = Map_new()
    g.list_elem_types = Map_new()
    g.fn_ptr_vars = Map_new()
    g.array_var_names = Map_new()
    -- Safety-check state is per-function (Stage 0 checker resets likewise)
    g.unsafe_depth = 0
    g.region_depth = 0
    g.tainted_vars = Map_new()
    g.ref_vars = Map_new()
    g.region_born_vars = Map_new()
    let empty_declared: List[string] = []
    g.declared_vars = empty_declared
    let empty_marks: List[int] = []
    g.region_marks = empty_marks
    let empty_blocks: List[IRBasicBlock] = []
    g.completed_blocks = empty_blocks

    -- Create entry block
    let entry_label = "entry"
    let mut blk = ir_block_new(entry_label)

    -- A program begins by letting the runtime prepare the process
    if name == "main" {
        let mut start_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        start_inst.callee = "dm_start"
        let no_args: List[IRValue] = []
        start_inst.call_args = no_args
        start_inst.result_type = "void"
        start_inst.has_result = false
        blk.instructions.push(start_inst)
        g.builder.next_id = g.builder.next_id + 1
    }

    -- Set up parameters
    i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        let pname = p.name
        -- Resolve "Self" to current impl type
        let mut ptype_str = if p.type_name == "Self" and g.has_impl_type {
            g.current_impl_type
        } else {
            p.type_name
        }
        ptype_str = canon_type(g, ptype_str)
        let ptype = map_type_name(ptype_str)

        if p.is_mut {
            -- Mut param: the caller passes a POINTER to the struct.
            -- Do NOT create an alloca — use the param ref directly as a pointer.
            -- Loads/stores go through the caller's memory, so mutations are visible.
            g.variable_map.insert(pname, -(i + 1))
            g.variable_types.insert(pname, ptype)
            g.mut_ptr_vars.insert(pname, true)
            g.param_ptr_map.insert(pname, i)
        } else {
            -- Regular param: alloca + store
            let alloca_id = g.builder.next_id
            let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
            alloca_inst.alloc_type = ptype
            alloca_inst.result_type = "ptr(" + ptype + ")"
            alloca_inst.has_result = true
            blk.instructions.push(alloca_inst)
            g.builder.next_id = g.builder.next_id + 1

            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(alloca_id)
            store_inst.store_val = ir_val_param(i)
            store_inst.has_result = false
            blk.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(pname, alloca_id)
            g.variable_types.insert(pname, ptype)
        }


        -- Track function-pointer-typed parameters for indirect calls
        if starts_with(ptype_str, "fn(") {
            g.variable_types.insert(pname, ptype_str)
        }

        g.var_src_types.insert(pname, ptype_str)
        let param_debug = debug_var(g, blk, pname, fd.line, i + 1)
        g = param_debug.gen
        blk = param_debug.block

        -- Track struct/enum/list/box typed parameters
        if g.struct_defs.contains(ptype_str) {
            g.var_struct_types.insert(pname, ptype_str)
        }
        if g.enum_defs.contains(ptype_str) {
            g.var_enum_types.insert(pname, ptype_str)
        }
        if starts_with(ptype_str, "List[") {
            let inner = extract_inner_type(ptype_str, "List[")
            let elem_kind = classify_list_elem(inner)
            g.list_elem_kinds.insert(pname, elem_kind)
            if elem_kind == LIST_ELEM_OTHER() {
                g.list_elem_types.insert(pname, inner)
            }
        }
        if starts_with(ptype_str, "Box[") {
            let inner = extract_inner_type(ptype_str, "Box[")
            g.box_inner_types.insert(pname, inner)
        }

        -- Safety checks: parameters are declared in the outermost scope;
        -- reference-typed parameters are tainted by default (their
        -- provenance is the unchecked caller), mirroring Stage 0
        g.declared_vars.push(pname)
        if p.is_ref {
            g.tainted_vars.insert(pname, true)
            g.ref_vars.insert(pname, true)
        }

        i = i + 1
    }

    -- Generate body statements
    i = 0
    while i < fd.body.len() {
        let s = fd.body[i]
        let result = generate_statement(g, blk, s)
        g = result.gen
        blk = result.block
        i = i + 1
    }

    -- Add implicit return if block has no terminator
    if blk.has_terminator == false {
        if ret_type == "void" {
            blk.terminator = Box_new(term_ret_void())
            blk.has_terminator = true
        } else if name == "main" {
            blk.terminator = Box_new(term_ret(ir_val_int(0)))
            blk.has_terminator = true
        } else {
            blk.terminator = Box_new(term_ret(default_value_for_type(ret_type)))
            blk.has_terminator = true
        }
    }

    -- Add blocks to function in correct order (entry block must be first)
    let mut func = g.ir_mod.functions[func_idx]
    -- Find and push the entry block first
    i = 0
    while i < g.completed_blocks.len() {
        if g.completed_blocks[i].label == "entry" {
            func.blocks.push(g.completed_blocks[i])
        }
        i = i + 1
    }
    -- Push remaining completed blocks (non-entry)
    i = 0
    while i < g.completed_blocks.len() {
        if g.completed_blocks[i].label != "entry" {
            func.blocks.push(g.completed_blocks[i])
        }
        i = i + 1
    }
    -- Then push the final block (if it's the entry and no blocks, push it; otherwise push as last)
    func.blocks.push(blk)
    g.ir_mod.functions[func_idx] = func

    g.var_src_types = outer_var_src_types
    g.var_struct_types = outer_var_struct_types
    g.var_enum_types = outer_var_enum_types
    g.box_inner_types = outer_box_inner_types
    g.list_elem_kinds = outer_list_elem_kinds
    g.list_elem_types = outer_list_elem_types
    g.fn_ptr_vars = outer_fn_ptr_vars
    g.array_var_names = outer_array_var_names

    return g
}

-- ============================================================
-- IMPL BLOCK GENERATION
-- ============================================================

fn generate_impl_block(gen: IRGenerator, ib: ImplBlock) -> IRGenerator {
    let mut g = gen
    let struct_name = ib.target_type

    -- Skip generic impl blocks — their methods are monomorphized on demand
    if ib.generic_params.len() > 0 {
        return g
    }

    g.current_impl_type = struct_name
    g.has_impl_type = true

    let mut i = 0
    while i < ib.methods.len() {
        let fd = ib.methods[i]
        let mangled = struct_name + "_" + fd.name
        g = declare_function(g, fd, mangled)
        if fd.is_extern == false {
            g.method_struct_map.insert(fd.name, struct_name)
            let saved_src_types = g.var_src_types
            g.var_src_types = Map_new()
            g = generate_function_body(g, fd, mangled)
            g.var_src_types = saved_src_types
        }
        i = i + 1
    }

    g.has_impl_type = false
    g.current_impl_type = ""
    return g
}

-- ============================================================
-- CONSTANT GENERATION
-- ============================================================

fn generate_constant(gen: IRGenerator, cd: ConstDecl) -> IRGenerator {
    let mut g = gen

    -- A constant's value is found at compile time, with `comptime` or
    -- without it: every operator, and calls of functions
    let val = eval_comptime_expr(g, *cd.value)

    let ty = if len(cd.type_name) > 0 {
        map_type_name(cd.type_name)
    } else if val.kind == VAL_CONST_INT() {
        "i64"
    } else if val.kind == VAL_CONST_FLOAT() {
        "f64"
    } else if val.kind == VAL_CONST_BOOL() {
        "bool"
    } else if val.kind == VAL_CONST_STRING() {
        "string"
    } else {
        "i64"
    }

    let mut global = ir_global_new(cd.name, ty)
    global.init_value = val
    global.has_init = true
    global.is_const = true
    g.ir_mod.globals.push(global)

    return g
}

-- ============================================================
-- GenResult: return both generator and current block
-- ============================================================
