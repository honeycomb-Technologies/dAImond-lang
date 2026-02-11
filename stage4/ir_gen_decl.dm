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
        let ftype = f.type_name
        let ir_type_name = map_type_name(ftype)

        info.field_names.push(fname)
        info.field_types.push(ir_type_name)

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

-- ============================================================
-- ENUM REGISTRATION
-- ============================================================

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
            let pt = v.payload_types[0]
            info.variant_payload_types.push(pt)
            info.variant_has_payload.push(true)
            -- Add payload field to struct
            let field_name = v.name + "_0"
            let field_type = map_type_name(pt)
            ir_fields.push(ir_field_new(field_name, field_type))
        } else {
            info.variant_payload_types.push("")
            info.variant_has_payload.push(false)
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
        g.method_struct_map.insert(ib.methods[i].name, struct_name)
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
    g = declare_extern(g, "dm_float_to_string", "f64", "string")
    g = declare_extern(g, "dm_bool_to_string", "bool", "string")
    g = declare_extern(g, "dm_parse_int", "string", "i64")
    g = declare_extern(g, "dm_parse_float", "string", "f64")

    -- String operations
    g = declare_extern(g, "dm_string_new", "ptr", "string")
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

    -- Character classification
    g = declare_extern(g, "dm_is_alpha", "string", "bool")
    g = declare_extern(g, "dm_is_digit", "string", "bool")
    g = declare_extern(g, "dm_is_whitespace", "string", "bool")
    g = declare_extern(g, "dm_is_alnum", "string", "bool")

    -- List contains
    g = declare_extern2(g, "dm_list_int64_contains", "ptr", "i64", "bool")
    g = declare_extern2(g, "dm_list_string_contains", "ptr", "string", "bool")

    -- Process
    g = declare_extern(g, "dm_panic", "string", "void")
    g = declare_extern(g, "exit", "i64", "void")

    -- Stdin
    g = declare_extern0(g, "dm_read_line", "string")

    -- System
    g = declare_extern(g, "dm_system", "string", "i64")
    g = declare_extern0(g, "dm_args_len", "i64")
    g = declare_extern(g, "dm_args_get", "i64", "string")

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
    g = declare_extern(g, "malloc", "i64", "ptr")
    g = declare_extern(g, "free", "ptr", "void")

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

    -- List operations (double)
    g = declare_extern(g, "dm_list_double_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_double_push", "ptr", "f64", "void")
    g = declare_extern2(g, "dm_list_double_get", "ptr", "i64", "f64")
    g = declare_extern(g, "dm_list_double_len", "ptr", "i64")

    -- List operations (generic)
    g = declare_extern(g, "dm_list_generic_new", "ptr", "void")
    g = declare_extern3(g, "dm_list_generic_push", "ptr", "ptr", "i64", "void")
    g = declare_extern4(g, "dm_list_generic_get", "ptr", "ptr", "i64", "i64", "void")
    g = declare_extern(g, "dm_list_generic_len", "ptr", "i64")

    -- Filesystem
    g = declare_extern(g, "dm_fs_mkdir", "string", "i64")
    g = declare_extern(g, "dm_fs_readdir", "string", "string")
    g = declare_extern(g, "dm_fs_remove", "string", "i64")
    g = declare_extern2(g, "dm_fs_rename", "string", "string", "i64")
    g = declare_extern0(g, "dm_fs_getcwd", "string")

    -- OS
    g = declare_extern(g, "dm_os_getenv", "string", "string")

    -- String split
    g = declare_extern3(g, "dm_string_split", "ptr", "string", "string", "void")

    -- Map[string, int]
    g = declare_extern(g, "dm_map_string_int_new", "ptr", "void")
    g = declare_extern3(g, "dm_map_string_int_insert", "ptr", "string", "i64", "void")
    g = declare_extern2(g, "dm_map_string_int_get", "ptr", "string", "i64")
    g = declare_extern2(g, "dm_map_string_int_contains", "ptr", "string", "bool")
    g = declare_extern2(g, "dm_map_string_int_remove", "ptr", "string", "void")
    g = declare_extern(g, "dm_map_string_int_len", "ptr", "i64")
    g = declare_extern2(g, "dm_map_string_int_keys", "ptr", "ptr", "void")
    g = declare_extern2(g, "dm_map_string_int_values", "ptr", "ptr", "void")

    -- Map[int, string]
    g = declare_extern(g, "dm_map_int_string_new", "ptr", "void")
    g = declare_extern3(g, "dm_map_int_string_insert", "ptr", "i64", "string", "void")
    g = declare_extern3(g, "dm_map_int_string_get", "ptr", "ptr", "i64", "void")
    g = declare_extern2(g, "dm_map_int_string_contains", "ptr", "i64", "bool")
    g = declare_extern2(g, "dm_map_int_string_remove", "ptr", "i64", "void")
    g = declare_extern(g, "dm_map_int_string_len", "ptr", "i64")

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

    -- Generate function body
    g = generate_function_body(g, fd, name)

    return g
}

fn declare_function(gen: IRGenerator, fd: FunctionDecl, name: string) -> IRGenerator {
    let mut g = gen

    let ret_type = if len(fd.return_type) > 0 {
        map_type_name(fd.return_type)
    } else {
        "void"
    }

    let mut func = ir_function_new(name, ret_type)
    func.is_extern = fd.is_extern

    -- Build parameter list
    let mut mut_flags = ""
    let mut i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        let pname = p.name
        let ptype_str = p.type_name
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

    -- Track user extern functions
    if fd.is_extern {
        let is_runtime = starts_with(name, "dm_") or name == "malloc" or name == "free" or name == "exit"
        if is_runtime == false {
            g.user_extern_fns.insert(name, true)
            g.ir_mod.user_extern_fns.insert(name, true)
        }
    }

    -- Track mut params
    if string_contains(mut_flags, "1") {
        g.mut_param_fns.insert(name, mut_flags)
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
    let ret_type = if len(fd.return_type) > 0 {
        map_type_name(fd.return_type)
    } else {
        "void"
    }
    g.current_return_type = ret_type

    -- Reset variable maps and completed blocks for this function
    g.variable_map = Map_new()
    g.variable_types = Map_new()
    let empty_blocks: List[IRBasicBlock] = []
    g.completed_blocks = empty_blocks

    -- Create entry block
    let entry_label = "entry"
    let mut blk = ir_block_new(entry_label)

    -- Set up parameters
    i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        let pname = p.name
        let ptype_str = p.type_name
        let ptype = map_type_name(ptype_str)

        if p.is_mut {
            -- Mut param: alloca + store from param ref (pointer semantics)
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

    -- Add blocks to function
    let mut func = g.ir_mod.functions[func_idx]
    -- First push all completed blocks (terminated blocks from control flow)
    i = 0
    while i < g.completed_blocks.len() {
        func.blocks.push(g.completed_blocks[i])
        i = i + 1
    }
    -- Then push the final block
    func.blocks.push(blk)
    g.ir_mod.functions[func_idx] = func

    return g
}

-- ============================================================
-- IMPL BLOCK GENERATION
-- ============================================================

fn generate_impl_block(gen: IRGenerator, ib: ImplBlock) -> IRGenerator {
    let mut g = gen
    let struct_name = ib.target_type
    g.current_impl_type = struct_name
    g.has_impl_type = true

    let mut i = 0
    while i < ib.methods.len() {
        let fd = ib.methods[i]
        let mangled = struct_name + "_" + fd.name
        g = declare_function(g, fd, mangled)
        if fd.is_extern == false {
            g.method_struct_map.insert(fd.name, struct_name)
            g = generate_function_body(g, fd, mangled)
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

    -- Try to evaluate the value at compile time
    let val = eval_const_value(g, cd.value)

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

-- Simple constant value evaluation
fn eval_const_value(gen: IRGenerator, expr_box: Box[Expr]) -> IRValue {
    let e = *expr_box
    if e.kind == EXPR_LITERAL_INT() {
        return ir_val_int(e.int_val)
    }
    if e.kind == EXPR_LITERAL_FLOAT() {
        return ir_val_float(e.float_val)
    }
    if e.kind == EXPR_LITERAL_BOOL() {
        return ir_val_bool(e.bool_val)
    }
    if e.kind == EXPR_LITERAL_STRING() {
        return ir_val_string(e.str_val)
    }
    if e.kind == EXPR_BINARY() {
        let lhs = eval_const_value(gen, e.left)
        let rhs = eval_const_value(gen, e.right)
        -- Integer arithmetic
        if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_INT() {
            if e.op == BINOP_ADD() { return ir_val_int(lhs.int_val + rhs.int_val) }
            if e.op == BINOP_SUB() { return ir_val_int(lhs.int_val - rhs.int_val) }
            if e.op == BINOP_MUL() { return ir_val_int(lhs.int_val * rhs.int_val) }
            if e.op == BINOP_DIV() and rhs.int_val != 0 { return ir_val_int(lhs.int_val / rhs.int_val) }
        }
        -- Float arithmetic
        if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_FLOAT() {
            if e.op == BINOP_ADD() { return ir_val_float(lhs.float_val + rhs.float_val) }
            if e.op == BINOP_SUB() { return ir_val_float(lhs.float_val - rhs.float_val) }
            if e.op == BINOP_MUL() { return ir_val_float(lhs.float_val * rhs.float_val) }
        }
        -- String concatenation
        if lhs.kind == VAL_CONST_STRING() and rhs.kind == VAL_CONST_STRING() {
            if e.op == BINOP_ADD() { return ir_val_string(lhs.str_val + rhs.str_val) }
        }
    }
    if e.kind == EXPR_UNARY() {
        if e.op == UNOP_NEG() {
            let operand_val = eval_const_value(gen, e.operand)
            if operand_val.kind == VAL_CONST_INT() {
                return ir_val_int(0 - operand_val.int_val)
            }
            if operand_val.kind == VAL_CONST_FLOAT() {
                return ir_val_float(0.0 - operand_val.float_val)
            }
        }
    }
    if e.kind == EXPR_IDENTIFIER() {
        -- Look up globals
        let mut i = 0
        while i < gen.ir_mod.globals.len() {
            let gl = gen.ir_mod.globals[i]
            if gl.name == e.name {
                return gl.init_value
            }
            i = i + 1
        }
    }
    return ir_val_int(0)
}

-- ============================================================
-- ESTIMATE TYPE SIZE (for malloc/generic list operations)
-- ============================================================

fn estimate_type_size(type_name: string) -> int {
    if type_name == "i8" or type_name == "u8" or type_name == "bool" { return 1 }
    if type_name == "i16" or type_name == "u16" { return 2 }
    if type_name == "i32" or type_name == "u32" or type_name == "f32" { return 4 }
    if type_name == "i64" or type_name == "u64" or type_name == "f64" or type_name == "ptr" { return 8 }
    if type_name == "string" { return 24 }
    -- Struct size: rough estimate of 8 bytes per field
    return 64
}
