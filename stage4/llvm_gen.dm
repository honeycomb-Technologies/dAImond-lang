module llvm_gen

import ir
import llvm

-- ============================================================
-- LLVM IR Generator
-- Translates dAImond IR (from ir.dm) into LLVM IR using
-- the LLVM-C bridge functions declared in llvm.dm.
--
-- Key design: All LLVM refs (context, module, builder, types,
-- values, basic blocks) are represented as int (int64_t on C side).
--
-- String ABI: dm_string is { ptr, i64, i64 } (24 bytes) which
-- exceeds the x86-64 SysV ABI 16-byte register threshold.
-- Runtime functions using dm_string are called through thin
-- C wrapper functions (llvm_dm_*) with pointer-based ABI:
-- - Returns dm_string -> wrapper takes dm_string* as first param (sret)
-- - Takes dm_string -> wrapper takes const dm_string* (pointer)
-- String values in LLVM IR are pointers to stack-allocated dm_string structs.
-- ============================================================

-- Bridge functions declared in llvm_bridge.c but not yet in llvm.dm
extern fn dm_llvm_struct_set_body(struct_ty: int, fields: int, field_count: int, is_packed: int)extern fn dm_llvm_add_incoming(phi: int, values: int, blocks: int, count: int)extern fn dm_llvm_const_struct(ctx: int, values: int, count: int, is_packed: int) -> int
extern fn dm_llvm_const_array(elem_ty: int, values: int, count: int) -> int

-- ============================================================
-- LLVM GENERATOR STRUCT
-- ============================================================

struct LLVMGenerator {
    context: int,
    llvm_module: int,
    builder: int,
    -- Maps
    value_map: Map[string, int],
    function_map: Map[string, int],
    fn_type_map: Map[string, int],
    block_map: Map[string, int],
    struct_type_map: Map[string, int],
    global_var_map: Map[string, int],
    -- String handling
    dm_string_type: int,
    has_dm_string_type: bool,
    string_counter: int,
    sret_counter: int,
    -- Track which instruction IDs are string pointers
    string_ptrs: Map[string, bool],
    -- Track wrapper functions for string ABI
    wrapper_returns_string: Map[string, bool],
    -- Track which functions have wrappers
    has_wrapper: Map[string, bool],
    -- Track which functions use sret return convention (result/option types)
    returns_sret: Map[string, bool],
    -- Reference to IR module for type lookups
    ir_module: IRModule,
    -- Current function being generated (for param lookups)
    current_func_name: string
}

-- ============================================================
-- CONSTRUCTOR / DESTRUCTOR
-- ============================================================

fn llvm_gen_new(ir_mod: IRModule, module_name: string) -> LLVMGenerator {
    let ctx = dm_llvm_context_create()
    let mod = dm_llvm_module_create(ctx, module_name)
    let bld = dm_llvm_create_builder(ctx)
    return LLVMGenerator {
        context: ctx,
        llvm_module: mod,
        builder: bld,
        value_map: Map_new(),
        function_map: Map_new(),
        fn_type_map: Map_new(),
        block_map: Map_new(),
        struct_type_map: Map_new(),
        global_var_map: Map_new(),
        dm_string_type: 0,
        has_dm_string_type: false,
        string_counter: 0,
        sret_counter: 0,
        string_ptrs: Map_new(),
        wrapper_returns_string: Map_new(),
        has_wrapper: Map_new(),
        returns_sret: Map_new(),
        ir_module: ir_mod,
        current_func_name: ""
    }
}

fn llvm_gen_dispose(gen: LLVMGenerator) {
    dm_llvm_dispose_builder(gen.builder)
    dm_llvm_module_dispose(gen.llvm_module)
    dm_llvm_context_dispose(gen.context)
}

-- ============================================================
-- DM_STRING TYPE
-- ============================================================

fn get_dm_string_type(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    if g.has_dm_string_type {
        return g
    }
    let ctx = g.context
    let str_ty = dm_llvm_struct_create_named(ctx, "dm_string")
    -- Body: { ptr (data), i64 (len), i64 (capacity) }
    dm_llvm_buf_clear()
    dm_llvm_buf_push(dm_llvm_pointer_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_struct_set_body_buf(str_ty, 0)
    g.dm_string_type = str_ty
    g.has_dm_string_type = true
    return g
}

-- ============================================================
-- TYPE MAPPING
-- ============================================================

-- Check if a type name represents a float type
fn lg_is_float_type(type_name: string) -> bool {
    return type_name == "f32" or type_name == "f64"
}

-- Check if a type name represents an integer type
fn is_int_type(type_name: string) -> bool {
    if type_name == "i8" { return true }
    if type_name == "i16" { return true }
    if type_name == "i32" { return true }
    if type_name == "i64" { return true }
    if type_name == "u8" { return true }
    if type_name == "u16" { return true }
    if type_name == "u32" { return true }
    if type_name == "u64" { return true }
    return false
}

-- Check if a type name represents an unsigned integer type
fn lg_is_unsigned_type(type_name: string) -> bool {
    if type_name == "u8" { return true }
    if type_name == "u16" { return true }
    if type_name == "u32" { return true }
    if type_name == "u64" { return true }
    return false
}

-- Check if a type is a SIMD vector type
fn lg_is_simd_type(type_name: string) -> bool {
    if type_name == "f32x4" { return true }
    if type_name == "f32x8" { return true }
    if type_name == "f64x2" { return true }
    if type_name == "f64x4" { return true }
    if type_name == "i32x4" { return true }
    if type_name == "i32x8" { return true }
    if type_name == "i64x2" { return true }
    if type_name == "i64x4" { return true }
    return false
}

-- Map IR type name string to LLVM type ref
fn map_ir_type_to_llvm(gen: LLVMGenerator, type_name: string) -> int {
    let ctx = gen.context
    if type_name == "i8" { return dm_llvm_int8_type(ctx) }
    if type_name == "i16" { return dm_llvm_int16_type(ctx) }
    if type_name == "i32" { return dm_llvm_int32_type(ctx) }
    if type_name == "i64" { return dm_llvm_int64_type(ctx) }
    -- Unsigned types map to same LLVM int types (signedness is in instructions, not types)
    if type_name == "u8" { return dm_llvm_int8_type(ctx) }
    if type_name == "u16" { return dm_llvm_int16_type(ctx) }
    if type_name == "u32" { return dm_llvm_int32_type(ctx) }
    if type_name == "u64" { return dm_llvm_int64_type(ctx) }
    if type_name == "f32" { return dm_llvm_float_type(ctx) }
    if type_name == "f64" { return dm_llvm_double_type(ctx) }
    if type_name == "bool" { return dm_llvm_int1_type(ctx) }
    if type_name == "void" { return dm_llvm_void_type(ctx) }
    if type_name == "string" { return gen.dm_string_type }
    if type_name == "ptr" { return dm_llvm_pointer_type(ctx) }
    -- Pointer types: ptr(...)
    if starts_with(type_name, "ptr(") { return dm_llvm_pointer_type(ctx) }
    -- Box types are heap pointers
    if starts_with(type_name, "Box[") { return dm_llvm_pointer_type(ctx) }
    -- List types: use dm_string type {ptr, i64, i64} since lists share the same layout
    if starts_with(type_name, "dm_list_") or starts_with(type_name, "list") or type_name == "list" or starts_with(type_name, "List[") {
        if gen.has_dm_string_type { return gen.dm_string_type }
        return dm_llvm_pointer_type(ctx)
    }
    -- Map types: use [64 x i8] to ensure enough space for runtime map struct
    if starts_with(type_name, "dm_map_") or starts_with(type_name, "map") or type_name == "map" or starts_with(type_name, "Map[") {
        return dm_llvm_array_type(dm_llvm_int8_type(ctx), 64)
    }
    -- Result type: { i32 tag, ok_type, err_type }
    -- Parse inner types from Result[T, E] for correct LLVM struct layout
    if type_name == "result" or starts_with(type_name, "Result[") or starts_with(type_name, "result_") {
        let i32_ty = dm_llvm_int32_type(ctx)
        let mut ok_ty = dm_llvm_int64_type(ctx)
        let mut err_ty = dm_llvm_int64_type(ctx)
        if starts_with(type_name, "Result[") {
            -- Parse Result[T, E] -> extract T and E
            let inner = substr(type_name, 7, len(type_name) - 8)
            -- Find comma separating T and E, respecting nested brackets
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
                let ok_str = substr(inner, 0, comma_pos)
                let mut err_start = comma_pos + 1
                if err_start < len(inner) and char_at(inner, err_start) == " " {
                    err_start = err_start + 1
                }
                let err_str = substr(inner, err_start, len(inner) - err_start)
                ok_ty = map_ir_type_to_llvm(gen, ok_str)
                err_ty = map_ir_type_to_llvm(gen, err_str)
            }
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(i32_ty)
        dm_llvm_buf_push(ok_ty)
        dm_llvm_buf_push(err_ty)
        return dm_llvm_struct_type_buf(ctx, 3, 0)
    }
    -- Option type: { i32 has_value, inner_type }
    -- Parse inner type from Option[T] for correct LLVM struct layout
    if type_name == "option" or starts_with(type_name, "Option[") or starts_with(type_name, "option_") {
        let i32_ty = dm_llvm_int32_type(ctx)
        let mut inner_ty = dm_llvm_int64_type(ctx)
        if starts_with(type_name, "Option[") {
            let inner = substr(type_name, 7, len(type_name) - 8)
            inner_ty = map_ir_type_to_llvm(gen, inner)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(i32_ty)
        dm_llvm_buf_push(inner_ty)
        return dm_llvm_struct_type_buf(ctx, 2, 0)
    }
    -- SIMD vector types
    if lg_is_simd_type(type_name) {
        return get_simd_llvm_type(gen, type_name)
    }
    -- Struct types: look up in struct_type_map
    if gen.struct_type_map.contains(type_name) {
        return gen.struct_type_map.get(type_name)
    }
    -- Struct names prefixed with "struct "
    if starts_with(type_name, "struct ") {
        let sname = substr(type_name, 7, len(type_name) - 7)
        if gen.struct_type_map.contains(sname) {
            return gen.struct_type_map.get(sname)
        }
    }
    -- Fallback: i64 (default integer type)
    return dm_llvm_int64_type(ctx)
}

-- ============================================================
-- STRUCT TYPE DECLARATION
-- ============================================================

fn register_builtin_struct_types(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context

    -- Register Result type: { i64 tag, i64 ok_val, i64 err_val }
    -- All fields are i64 for uniform insertvalue/extractvalue (avoids i32/i64 mismatch)
    if g.struct_type_map.contains("result") == false {
        let result_struct = dm_llvm_struct_create_named(ctx, "result")
        dm_llvm_buf_clear()
        dm_llvm_buf_push(dm_llvm_int64_type(ctx))
        dm_llvm_buf_push(dm_llvm_int64_type(ctx))
        dm_llvm_buf_push(dm_llvm_int64_type(ctx))
        dm_llvm_struct_set_body_buf(result_struct, 0)
        g.struct_type_map.insert("result", result_struct)
        g.struct_type_map.insert("Result", result_struct)
    }

    -- Register Option type: { i64 has_value, i64 value }
    if g.struct_type_map.contains("option") == false {
        let option_struct = dm_llvm_struct_create_named(ctx, "option")
        dm_llvm_buf_clear()
        dm_llvm_buf_push(dm_llvm_int64_type(ctx))
        dm_llvm_buf_push(dm_llvm_int64_type(ctx))
        dm_llvm_struct_set_body_buf(option_struct, 0)
        g.struct_type_map.insert("option", option_struct)
        g.struct_type_map.insert("Option", option_struct)
    }

    -- Register list and map runtime struct types: { ptr, i64, i64 }
    -- All list types share one LLVM named struct, same for all map types.
    -- This avoids type mismatches when list values move between contexts.
    let list_struct = dm_llvm_struct_create_named(ctx, "dm_list")
    dm_llvm_buf_clear()
    dm_llvm_buf_push(dm_llvm_pointer_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_struct_set_body_buf(list_struct, 0)
    g.struct_type_map.insert("dm_list", list_struct)
    g.struct_type_map.insert("dm_list_int64", list_struct)
    g.struct_type_map.insert("dm_list_double", list_struct)
    g.struct_type_map.insert("dm_list_dm_string", list_struct)
    g.struct_type_map.insert("dm_list_generic", list_struct)
    g.struct_type_map.insert("list", list_struct)

    let map_struct = dm_llvm_struct_create_named(ctx, "dm_map")
    dm_llvm_buf_clear()
    dm_llvm_buf_push(dm_llvm_pointer_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_struct_set_body_buf(map_struct, 0)
    g.struct_type_map.insert("dm_map", map_struct)
    g.struct_type_map.insert("dm_map_string_int", map_struct)
    g.struct_type_map.insert("dm_map_int_string", map_struct)
    g.struct_type_map.insert("map", map_struct)

    return g
}

fn declare_struct_types(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    -- Phase 1: Create opaque named struct types (forward declarations)
    let struct_keys = g.ir_module.struct_defs.keys()
    let mut i = 0
    while i < struct_keys.len() {
        let name = struct_keys[i]
        let ir_ty = g.ir_module.struct_defs.get(name)
        if ir_ty.kind == IR_TYPE_STRUCT() {
            let sname = ir_ty.name
            if sname != "" {
                let llvm_struct = dm_llvm_struct_create_named(ctx, sname)
                g.struct_type_map.insert(sname, llvm_struct)
            }
        }
        i = i + 1
    }
    -- Phase 2: Set struct bodies (all types now registered)
    i = 0
    while i < struct_keys.len() {
        let name = struct_keys[i]
        let ir_ty = g.ir_module.struct_defs.get(name)
        if ir_ty.kind == IR_TYPE_STRUCT() {
            let sname = ir_ty.name
            if sname != "" and g.struct_type_map.contains(sname) {
                let llvm_struct = g.struct_type_map.get(sname)
                -- Collect field types into list first (avoid buffer clobbering)
                let mut field_types: List[int] = []
                let mut fi = 0
                while fi < ir_ty.fields.len() {
                    let field = ir_ty.fields[fi]
                    field_types.push(map_ir_type_to_llvm(g, field.type_id))
                    fi = fi + 1
                }
                if field_types.len() > 0 {
                    dm_llvm_buf_clear()
                    fi = 0
                    while fi < field_types.len() {
                        dm_llvm_buf_push(field_types[fi])
                        fi = fi + 1
                    }
                    dm_llvm_struct_set_body_buf(llvm_struct, 0)
                }
            }
        }
        i = i + 1
    }
    return g
}

-- ============================================================
-- GLOBAL VARIABLES
-- ============================================================

fn generate_globals(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    let mut i = 0
    while i < g.ir_module.globals.len() {
        let global = g.ir_module.globals[i]
        -- Skip comptime bookkeeping globals (array elements, markers)
        if starts_with(global.name, "__ct_arr_") {
            i = i + 1
            continue
        }
        -- Skip string-type globals (they are resolved inline by generate_identifier)
        if global.has_init and global.init_value.kind == VAL_CONST_STRING() {
            i = i + 1
            continue
        }
        let llvm_ty = map_ir_type_to_llvm(g, global.type_id)
        let gvar = dm_llvm_module_add_global(g.llvm_module, llvm_ty, global.name)
        -- Set initializer
        if global.has_init {
            let init = global.init_value
            if init.kind == VAL_CONST_INT() {
                dm_llvm_set_initializer(gvar, dm_llvm_const_int(llvm_ty, init.int_val, 1))
            } else if init.kind == VAL_CONST_FLOAT() {
                dm_llvm_set_initializer(gvar, dm_llvm_const_real(llvm_ty, init.float_val))
            } else if init.kind == VAL_CONST_BOOL() {
                let mut bval = 0
                if init.bool_val { bval = 1 }
                dm_llvm_set_initializer(gvar, dm_llvm_const_int(llvm_ty, bval, 0))
            } else {
                dm_llvm_set_initializer(gvar, dm_llvm_const_null(llvm_ty))
            }
        } else {
            dm_llvm_set_initializer(gvar, dm_llvm_const_null(llvm_ty))
        }
        if global.is_const {
            dm_llvm_set_global_constant(gvar, 1)
        }
        g.global_var_map.insert(global.name, gvar)
        i = i + 1
    }
    return g
}

-- ============================================================
-- FUNCTION DECLARATION
-- ============================================================

-- Check if a return type needs sret (struct return) convention.
-- Result ({i32, i64, i64} = 20 bytes) and Option ({i32, i64} = 12 bytes)
-- are too large for register return on x86_64 and cause LLVM errors.
fn func_needs_wrapper(func: IRFunction) -> bool {
    if func.is_extern == false { return false }
    if func.return_type == "string" { return true }
    let mut i = 0
    while i < func.params.len() {
        if func.params[i].type_id == "string" { return true }
        i = i + 1
    }
    return false
}

fn declare_wrapper(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    let returns_string = func.return_type == "string"
    let ptr_ty = dm_llvm_pointer_type(ctx)
    let void_ty = dm_llvm_void_type(ctx)
    -- Compute return type BEFORE using buffer (map_ir_type_to_llvm may use buffer internally)
    let mut ret_ty = 0
    if returns_string {
        ret_ty = void_ty
    } else {
        ret_ty = map_ir_type_to_llvm(g, func.return_type)
    }
    -- Collect param types into a list first (avoid buffer clobbering)
    let mut param_types: List[int] = []
    if returns_string {
        param_types.push(ptr_ty)
    }
    let mut i = 0
    while i < func.params.len() {
        let p = func.params[i]
        if p.type_id == "string" {
            param_types.push(ptr_ty)
        } else {
            param_types.push(map_ir_type_to_llvm(g, p.type_id))
        }
        i = i + 1
    }
    -- Push all to shared buffer at once and create function type
    dm_llvm_buf_clear()
    i = 0
    while i < param_types.len() {
        dm_llvm_buf_push(param_types[i])
        i = i + 1
    }
    let fn_ty = dm_llvm_function_type_buf(ret_ty, 0)
    -- Wrapper name: "llvm_" + original name
    let wrapper_name = "llvm_" + func.name
    let llvm_fn = dm_llvm_module_add_function(g.llvm_module, wrapper_name, fn_ty)
    -- Store under ORIGINAL name for call resolution
    g.function_map.insert(func.name, llvm_fn)
    g.fn_type_map.insert(func.name, fn_ty)
    g.has_wrapper.insert(func.name, true)
    g.wrapper_returns_string.insert(func.name, returns_string)
    return g
}

-- Declare and generate a wrapper for a user-declared extern function that uses dm_string.
-- For `extern fn getenv(name: string) -> string`:
-- 1. Declare the real C function `getenv` as `ptr getenv(ptr)`
-- 2. Generate a wrapper `llvm_getenv` that converts dm_string <-> char*

fn lg_declare_function(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    -- Check if wrapper is needed for extern functions with string params/return
    if func_needs_wrapper(func) {
        -- Check if this is a user extern function
        if g.ir_module.user_extern_fns.contains(func.name) {
            g = declare_user_extern_wrapper(g, func)
            return g
        }
        g = declare_wrapper(g, func)
        return g
    }
    -- Regular function declaration
    -- Check if function returns result/option (needs sret convention)
    let needs_sret = is_sret_return_type(func.return_type) and func.name != "main"
    let is_main = func.name == "main"
    -- IMPORTANT: Compute return type and collect param types BEFORE using the shared
    -- buffer, because map_ir_type_to_llvm may use dm_llvm_buf internally for struct types
    let mut ret_ty = 0
    if is_main {
        ret_ty = dm_llvm_int32_type(ctx)
    } else if needs_sret {
        ret_ty = dm_llvm_void_type(ctx)
    } else {
        ret_ty = map_ir_type_to_llvm(g, func.return_type)
    }
    -- Collect all param types into a list first (avoid buffer clobbering)
    let mut param_types: List[int] = []
    if needs_sret {
        param_types.push(dm_llvm_pointer_type(ctx))
    }
    let mut i = 0
    while i < func.params.len() {
        param_types.push(map_ir_type_to_llvm(g, func.params[i].type_id))
        i = i + 1
    }
    if is_main {
        param_types.push(dm_llvm_int32_type(ctx))
        param_types.push(dm_llvm_pointer_type(ctx))
    }
    -- Now push all to shared buffer at once and create function type
    dm_llvm_buf_clear()
    i = 0
    while i < param_types.len() {
        dm_llvm_buf_push(param_types[i])
        i = i + 1
    }
    let fn_ty = dm_llvm_function_type_buf(ret_ty, 0)
    let llvm_fn = dm_llvm_module_add_function(g.llvm_module, func.name, fn_ty)
    g.function_map.insert(func.name, llvm_fn)
    g.fn_type_map.insert(func.name, fn_ty)
    -- Track sret functions
    if needs_sret {
        g.returns_sret.insert(func.name, true)
    }
    -- Name parameters (offset by 1 for sret functions since param 0 is the sret ptr)
    i = 0
    if needs_sret {
        let sret_param = dm_llvm_get_param(llvm_fn, 0)
        dm_llvm_set_value_name(sret_param, "sret.out")
        while i < func.params.len() {
            let param_val = dm_llvm_get_param(llvm_fn, i + 1)
            dm_llvm_set_value_name(param_val, func.params[i].name)
            i = i + 1
        }
    } else {
        while i < func.params.len() {
            let param_val = dm_llvm_get_param(llvm_fn, i)
            dm_llvm_set_value_name(param_val, func.params[i].name)
            i = i + 1
        }
    }
    return g
}

-- ============================================================
-- FUNCTION BODY GENERATION
-- ============================================================

fn lg_generate_function(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    if g.function_map.contains(func.name) == false { return g }
    let llvm_fn = g.function_map.get(func.name)
    g.current_func_name = func.name
    -- Create basic blocks
    -- Clear block_map for this function
    g.block_map = Map_new()
    let mut bi = 0
    while bi < func.blocks.len() {
        let block = func.blocks[bi]
        let bb = dm_llvm_append_basic_block(ctx, llvm_fn, block.label)
        g.block_map.insert(block.label, bb)
        bi = bi + 1
    }
    -- Clear value_map and string_ptrs for this function
    g.value_map = Map_new()
    g.string_ptrs = Map_new()
    -- ALLOCA HOISTING: Generate all allocas in the entry block first.
    -- This ensures all allocas are available regardless of block processing order.
    -- Standard LLVM practice: all allocas belong in the entry block.
    if func.blocks.len() > 0 {
        let entry_label = func.blocks[0].label
        if g.block_map.contains(entry_label) {
            let entry_bb = g.block_map.get(entry_label)
            dm_llvm_position_at_end(g.builder, entry_bb)
            -- Pre-generate argc/argv for main
            if func.name == "main" {
                let i32_ty = dm_llvm_int32_type(ctx)
                let ptr_ty = dm_llvm_pointer_type(ctx)
                let argc_global = dm_llvm_module_add_global(g.llvm_module, i32_ty, "dm_argc")
                dm_llvm_set_linkage(argc_global, dm_llvm_external_linkage())
                let argv_global = dm_llvm_module_add_global(g.llvm_module, ptr_ty, "dm_argv")
                dm_llvm_set_linkage(argv_global, dm_llvm_external_linkage())
                let argc_param = dm_llvm_get_param(llvm_fn, func.params.len())
                let argv_param = dm_llvm_get_param(llvm_fn, func.params.len() + 1)
                dm_llvm_build_store(g.builder, argc_param, argc_global)
                dm_llvm_build_store(g.builder, argv_param, argv_global)
            }
            -- Scan ALL blocks for OP_ALLOCA and generate them in entry block
            bi = 0
            while bi < func.blocks.len() {
                let scan_block = func.blocks[bi]
                let mut si = 0
                while si < scan_block.instructions.len() {
                    let scan_inst = scan_block.instructions[si]
                    if scan_inst.op == OP_ALLOCA() {
                        g = generate_instruction(g, scan_inst)
                    }
                    si = si + 1
                }
                bi = bi + 1
            }
        }
    }
    -- Build BFS-ordered block list for correct processing order.
    -- Blocks must be processed in dominator order so that value_map is populated
    -- before any instruction references a value from an earlier block.
    let mut bfs_order: List[string] = []
    let mut bfs_visited: Map[string, bool] = Map_new()
    if func.blocks.len() > 0 {
        -- Start BFS from entry block
        let mut bfs_queue: List[string] = []
        bfs_queue.push(func.blocks[0].label)
        bfs_visited.insert(func.blocks[0].label, true)
        while bfs_queue.len() > 0 {
            let cur_label = bfs_queue[0]
            -- Shift: manual removal of first element by rebuilding
            let mut new_queue: List[string] = []
            let mut qi = 1
            while qi < bfs_queue.len() {
                new_queue.push(bfs_queue[qi])
                qi = qi + 1
            }
            bfs_queue = new_queue
            bfs_order.push(cur_label)
            -- Find this block and get its terminator targets
            let mut fi = 0
            while fi < func.blocks.len() {
                if func.blocks[fi].label == cur_label {
                    let cur_block = func.blocks[fi]
                    if cur_block.has_terminator {
                        let t = *cur_block.terminator
                        if t.kind == TERM_BR() {
                            if bfs_visited.contains(t.target) == false {
                                bfs_visited.insert(t.target, true)
                                bfs_queue.push(t.target)
                            }
                        }
                        if t.kind == TERM_BR_COND() {
                            if bfs_visited.contains(t.true_label) == false {
                                bfs_visited.insert(t.true_label, true)
                                bfs_queue.push(t.true_label)
                            }
                            if bfs_visited.contains(t.false_label) == false {
                                bfs_visited.insert(t.false_label, true)
                                bfs_queue.push(t.false_label)
                            }
                        }
                    }
                }
                fi = fi + 1
            }
        }
        -- Add any blocks not reachable from entry (safety: shouldn't happen)
        bi = 0
        while bi < func.blocks.len() {
            if bfs_visited.contains(func.blocks[bi].label) == false {
                bfs_order.push(func.blocks[bi].label)
            }
            bi = bi + 1
        }
    }
    -- Generate instructions and terminators for each block in BFS order (skip allocas)
    let mut bfs_i = 0
    while bfs_i < bfs_order.len() {
        let bfs_label = bfs_order[bfs_i]
        -- Find the block with this label
        let mut found_bi = 0
        while found_bi < func.blocks.len() {
            if func.blocks[found_bi].label == bfs_label {
                let block = func.blocks[found_bi]
                if g.block_map.contains(block.label) {
                    let bb = g.block_map.get(block.label)
                    dm_llvm_position_at_end(g.builder, bb)
                    -- Generate non-alloca instructions
                    let mut ii = 0
                    while ii < block.instructions.len() {
                        if block.instructions[ii].op != OP_ALLOCA() {
                            g = generate_instruction(g, block.instructions[ii])
                        }
                        ii = ii + 1
                    }
                    -- Generate terminator
                    if block.has_terminator {
                        let term = *block.terminator
                        g = generate_terminator(g, term, func)
                    }
                }
            }
            found_bi = found_bi + 1
        }
        bfs_i = bfs_i + 1
    }
    return g
}

-- ============================================================
-- VALUE RESOLUTION
-- ============================================================

fn resolve_value(gen: LLVMGenerator, val: IRValue) -> int {
    let ctx = gen.context
    if val.kind == VAL_CONST_INT() {
        return dm_llvm_const_int(dm_llvm_int64_type(ctx), val.int_val, 1)
    }
    if val.kind == VAL_CONST_FLOAT() {
        return dm_llvm_const_real(dm_llvm_double_type(ctx), val.float_val)
    }
    if val.kind == VAL_CONST_BOOL() {
        let mut bval = 0
        if val.bool_val { bval = 1 }
        return dm_llvm_const_int(dm_llvm_int1_type(ctx), bval, 0)
    }
    if val.kind == VAL_CONST_STRING() {
        -- Build a global string pointer for the raw C string data
        let name = ".str." + int_to_string(gen.string_counter)
        return dm_llvm_build_global_string_ptr(gen.builder, val.str_val, name)
    }
    if val.kind == VAL_INST_REF() {
        let key = int_to_string(val.ref_id)
        if gen.value_map.contains(key) {
            return gen.value_map.get(key)
        }
        -- Fallback: return null
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    if val.kind == VAL_PARAM_REF() {
        if gen.function_map.contains(gen.current_func_name) {
            let llvm_fn = gen.function_map.get(gen.current_func_name)
            -- For sret functions, LLVM param 0 is the sret pointer, so user params start at index 1
            let mut param_offset = 0
            if gen.returns_sret.contains(gen.current_func_name) {
                param_offset = 1
            }
            return dm_llvm_get_param(llvm_fn, val.ref_id + param_offset)
        }
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    if val.kind == VAL_GLOBAL_REF() {
        -- Check functions first, then global variables
        if gen.function_map.contains(val.str_val) {
            return gen.function_map.get(val.str_val)
        }
        if gen.global_var_map.contains(val.str_val) {
            let gvar = gen.global_var_map.get(val.str_val)
            -- Load the value from the global variable
            return dm_llvm_build_load2(gen.builder, dm_llvm_int64_type(ctx), gvar, "gload")
        }
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    if val.kind == VAL_UNDEF() {
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    return dm_llvm_const_null(dm_llvm_int64_type(ctx))
}

-- Resolve value for const_string, incrementing string counter
fn resolve_value_with_counter(gen: LLVMGenerator, val: IRValue) -> LLVMGenerator {
    let mut g = gen
    if val.kind == VAL_CONST_STRING() {
        g.string_counter = g.string_counter + 1
    }
    return g
}

-- Check if an IR value is a string pointer (alloca to dm_string)
fn is_string_ptr(gen: LLVMGenerator, val: IRValue) -> bool {
    if val.kind == VAL_INST_REF() {
        let key = int_to_string(val.ref_id)
        if gen.string_ptrs.contains(key) {
            return gen.string_ptrs.get(key)
        }
    }
    return false
}

-- Expected IR type of field `idx` of aggregate type `type_name`, or "".
fn expected_field_ir_type(gen: LLVMGenerator, type_name: string, idx: int) -> string {
    if gen.ir_module.struct_defs.contains(type_name) {
        let ir_struct = gen.ir_module.struct_defs.get(type_name)
        if idx < ir_struct.fields.len() {
            return ir_struct.fields[idx].type_id
        }
        return ""
    }
    if idx == 0 and (type_name == "result" or type_name == "option" or starts_with(type_name, "Result[") or starts_with(type_name, "Option[") or starts_with(type_name, "result_") or starts_with(type_name, "option_")) {
        return "i32"
    }
    if starts_with(type_name, "Result[") {
        let inner = substr(type_name, 7, len(type_name) - 8)
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
            if idx == 1 {
                return substr(inner, 0, comma_pos)
            }
            if idx == 2 {
                let mut err_start = comma_pos + 1
                if err_start < len(inner) and char_at(inner, err_start) == " " {
                    err_start = err_start + 1
                }
                return substr(inner, err_start, len(inner) - err_start)
            }
        }
        return "i64"
    }
    if starts_with(type_name, "Option[") {
        if idx == 1 {
            return substr(type_name, 7, len(type_name) - 8)
        }
        return "i64"
    }
    if type_name == "result" or type_name == "option" {
        return "i64"
    }
    return ""
}

-- ============================================================
-- INSTRUCTION GENERATION
-- ============================================================

fn generate_instruction(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let op = inst.op
    let id_key = int_to_string(inst.id)
    -- Arithmetic
    if op == OP_ADD() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fadd(b, lhs, rhs, "fadd")
        } else {
            result = dm_llvm_build_add(b, lhs, rhs, "add")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_SUB() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fsub(b, lhs, rhs, "fsub")
        } else {
            result = dm_llvm_build_sub(b, lhs, rhs, "sub")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_MUL() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fmul(b, lhs, rhs, "fmul")
        } else {
            result = dm_llvm_build_mul(b, lhs, rhs, "mul")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_DIV() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fdiv(b, lhs, rhs, "fdiv")
        } else {
            result = dm_llvm_build_sdiv(b, lhs, rhs, "div")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_MOD() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_frem(b, lhs, rhs, "frem")
        } else {
            result = dm_llvm_build_srem(b, lhs, rhs, "mod")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_NEG() {
        let operand = resolve_value(g, inst.operand)
        g = resolve_value_with_counter(g, inst.operand)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fneg(b, operand, "fneg")
        } else {
            result = dm_llvm_build_neg(b, operand, "neg")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Comparison
    if op == OP_EQ() {
        let mut lhs = resolve_value(g, inst.lhs)
        let mut rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        -- Fix i32 vs i64 type mismatch for icmp (e.g., enum/option/result tag comparisons)
        let lhs_ty = dm_llvm_type_of(lhs)
        let rhs_ty = dm_llvm_type_of(rhs)
        if lhs_ty != rhs_ty {
            let lhs_kind = dm_llvm_get_type_kind(lhs_ty)
            let rhs_kind = dm_llvm_get_type_kind(rhs_ty)
            -- type kind 8 = LLVMIntegerTypeKind
            if lhs_kind == 8 and rhs_kind == 8 {
                let lw = dm_llvm_get_int_type_width(lhs_ty)
                let rw = dm_llvm_get_int_type_width(rhs_ty)
                if lw < rw {
                    rhs = dm_llvm_build_trunc(b, rhs, lhs_ty, "trunc")
                }
                if rw < lw {
                    lhs = dm_llvm_build_trunc(b, lhs, rhs_ty, "trunc")
                }
            }
        }
        let lhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(lhs))
        let rhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(rhs))
        let is_float_cmp = lhs_kind_f == 2 or lhs_kind_f == 3 or rhs_kind_f == 2 or rhs_kind_f == 3
        let mut fl = lhs
        let mut fr = rhs
        if is_float_cmp and lhs_kind_f == 8 { fl = dm_llvm_build_si_to_fp(b, lhs, dm_llvm_type_of(rhs), "itof") }
        if is_float_cmp and rhs_kind_f == 8 { fr = dm_llvm_build_si_to_fp(b, rhs, dm_llvm_type_of(lhs), "itof") }
        let mut result = 0
        if is_float_cmp {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_oeq(), fl, fr, "feq")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_eq(), lhs, rhs, "eq")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_NE() {
        let mut lhs = resolve_value(g, inst.lhs)
        let mut rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        -- Fix i32 vs i64 type mismatch for icmp
        let lhs_ty = dm_llvm_type_of(lhs)
        let rhs_ty = dm_llvm_type_of(rhs)
        if lhs_ty != rhs_ty {
            let lhs_kind = dm_llvm_get_type_kind(lhs_ty)
            let rhs_kind = dm_llvm_get_type_kind(rhs_ty)
            if lhs_kind == 8 and rhs_kind == 8 {
                let lw = dm_llvm_get_int_type_width(lhs_ty)
                let rw = dm_llvm_get_int_type_width(rhs_ty)
                if lw < rw {
                    rhs = dm_llvm_build_trunc(b, rhs, lhs_ty, "trunc")
                }
                if rw < lw {
                    lhs = dm_llvm_build_trunc(b, lhs, rhs_ty, "trunc")
                }
            }
        }
        let lhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(lhs))
        let rhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(rhs))
        let is_float_cmp = lhs_kind_f == 2 or lhs_kind_f == 3 or rhs_kind_f == 2 or rhs_kind_f == 3
        let mut fl = lhs
        let mut fr = rhs
        if is_float_cmp and lhs_kind_f == 8 { fl = dm_llvm_build_si_to_fp(b, lhs, dm_llvm_type_of(rhs), "itof") }
        if is_float_cmp and rhs_kind_f == 8 { fr = dm_llvm_build_si_to_fp(b, rhs, dm_llvm_type_of(lhs), "itof") }
        let mut result = 0
        if is_float_cmp {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_one(), fl, fr, "fne")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_ne(), lhs, rhs, "ne")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LT() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let lhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(lhs))
        let rhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(rhs))
        let is_float_cmp = lhs_kind_f == 2 or lhs_kind_f == 3 or rhs_kind_f == 2 or rhs_kind_f == 3
        let mut fl = lhs
        let mut fr = rhs
        if is_float_cmp and lhs_kind_f == 8 { fl = dm_llvm_build_si_to_fp(b, lhs, dm_llvm_type_of(rhs), "itof") }
        if is_float_cmp and rhs_kind_f == 8 { fr = dm_llvm_build_si_to_fp(b, rhs, dm_llvm_type_of(lhs), "itof") }
        let mut result = 0
        if is_float_cmp {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_olt(), fl, fr, "flt")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_slt(), lhs, rhs, "lt")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LE() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let lhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(lhs))
        let rhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(rhs))
        let is_float_cmp = lhs_kind_f == 2 or lhs_kind_f == 3 or rhs_kind_f == 2 or rhs_kind_f == 3
        let mut fl = lhs
        let mut fr = rhs
        if is_float_cmp and lhs_kind_f == 8 { fl = dm_llvm_build_si_to_fp(b, lhs, dm_llvm_type_of(rhs), "itof") }
        if is_float_cmp and rhs_kind_f == 8 { fr = dm_llvm_build_si_to_fp(b, rhs, dm_llvm_type_of(lhs), "itof") }
        let mut result = 0
        if is_float_cmp {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_ole(), fl, fr, "fle")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_sle(), lhs, rhs, "le")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_GT() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let lhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(lhs))
        let rhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(rhs))
        let is_float_cmp = lhs_kind_f == 2 or lhs_kind_f == 3 or rhs_kind_f == 2 or rhs_kind_f == 3
        let mut fl = lhs
        let mut fr = rhs
        if is_float_cmp and lhs_kind_f == 8 { fl = dm_llvm_build_si_to_fp(b, lhs, dm_llvm_type_of(rhs), "itof") }
        if is_float_cmp and rhs_kind_f == 8 { fr = dm_llvm_build_si_to_fp(b, rhs, dm_llvm_type_of(lhs), "itof") }
        let mut result = 0
        if is_float_cmp {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_ogt(), fl, fr, "fgt")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_sgt(), lhs, rhs, "gt")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_GE() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let lhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(lhs))
        let rhs_kind_f = dm_llvm_get_type_kind(dm_llvm_type_of(rhs))
        let is_float_cmp = lhs_kind_f == 2 or lhs_kind_f == 3 or rhs_kind_f == 2 or rhs_kind_f == 3
        let mut fl = lhs
        let mut fr = rhs
        if is_float_cmp and lhs_kind_f == 8 { fl = dm_llvm_build_si_to_fp(b, lhs, dm_llvm_type_of(rhs), "itof") }
        if is_float_cmp and rhs_kind_f == 8 { fr = dm_llvm_build_si_to_fp(b, rhs, dm_llvm_type_of(lhs), "itof") }
        let mut result = 0
        if is_float_cmp {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_oge(), fl, fr, "fge")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_sge(), lhs, rhs, "ge")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Logical operators
    if op == OP_LOGICAL_AND() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let result = dm_llvm_build_and(b, lhs, rhs, "and")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LOGICAL_OR() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let result = dm_llvm_build_or(b, lhs, rhs, "or")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LOGICAL_NOT() {
        let operand = resolve_value(g, inst.operand)
        g = resolve_value_with_counter(g, inst.operand)
        let result = dm_llvm_build_not(b, operand, "not")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Memory: alloca
    if op == OP_ALLOCA() {
        let alloc_ty = map_ir_type_to_llvm(g, inst.alloc_type)
        let result = dm_llvm_build_alloca(b, alloc_ty, "alloca")
        if inst.alloc_type == "string" {
            g.string_ptrs.insert(id_key, true)
        }
        -- List and map types are also aggregate pointers (pass by pointer, not value)
        if starts_with(inst.alloc_type, "dm_list_") or starts_with(inst.alloc_type, "dm_map_") or starts_with(inst.alloc_type, "List[") or starts_with(inst.alloc_type, "Map[") {
            g.string_ptrs.insert(id_key, true)
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Memory: load
    if op == OP_LOAD() {
        if inst.load_type == "string" {
            -- String loads return the pointer itself (dm_string is always by pointer)
            let ptr_val = resolve_value(g, inst.load_ptr)
            g = resolve_value_with_counter(g, inst.load_ptr)
            g.string_ptrs.insert(id_key, true)
            if inst.has_result { g.value_map.insert(id_key, ptr_val) }
            return g
        }
        -- List/map loads return the pointer itself (aggregate types, always by pointer)
        if starts_with(inst.load_type, "dm_list_") or starts_with(inst.load_type, "dm_map_") or inst.load_type == "list" or inst.load_type == "map" or starts_with(inst.load_type, "List[") or starts_with(inst.load_type, "Map[") {
            let ptr_val = resolve_value(g, inst.load_ptr)
            g = resolve_value_with_counter(g, inst.load_ptr)
            g.string_ptrs.insert(id_key, true)
            if inst.has_result { g.value_map.insert(id_key, ptr_val) }
            return g
        }
        let load_ty = map_ir_type_to_llvm(g, inst.load_type)
        let ptr_val = resolve_value(g, inst.load_ptr)
        g = resolve_value_with_counter(g, inst.load_ptr)
        let result = dm_llvm_build_load2(b, load_ty, ptr_val, "load")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Memory: store
    if op == OP_STORE() {
        let dst = resolve_value(g, inst.store_ptr)
        g = resolve_value_with_counter(g, inst.store_ptr)
        -- Special case: storing const_string into a string alloca
        -- Must call dm_string_new to properly init {data, length, capacity}
        if inst.store_val.kind == VAL_CONST_STRING() {
            let raw_ptr = resolve_value(g, inst.store_val)
            g = resolve_value_with_counter(g, inst.store_val)
            if g.function_map.contains("dm_string_new") {
                let func = g.function_map.get("dm_string_new")
                let fn_ty = g.fn_type_map.get("dm_string_new")
                dm_llvm_buf_clear()
                dm_llvm_buf_push(dst)
                dm_llvm_buf_push(raw_ptr)
                dm_llvm_build_call2_buf(b, fn_ty, func, "")
            } else {
                dm_llvm_build_store(b, raw_ptr, dst)
            }
            return g
        }
        let src = resolve_value(g, inst.store_val)
        g = resolve_value_with_counter(g, inst.store_val)
        if is_string_ptr(g, inst.store_val) {
            -- String copy: load struct from source alloca, store to dest alloca
            let str_ty = g.dm_string_type
            let loaded = dm_llvm_build_load2(b, str_ty, src, "strcpy")
            dm_llvm_build_store(b, loaded, dst)
        } else {
            dm_llvm_build_store(b, src, dst)
        }
        return g
    }
    -- Struct: extract field
    if op == OP_EXTRACT_FIELD() {
        let base = resolve_value(g, inst.field_base)
        g = resolve_value_with_counter(g, inst.field_base)
        let result = dm_llvm_build_extract_value(b, base, inst.field_index, "field")
        -- If extracted field is dm_string, store to alloca and track
        if inst.field_type == "string" {
            let str_alloca = dm_llvm_build_alloca(b, g.dm_string_type, "str.extract")
            dm_llvm_build_store(b, result, str_alloca)
            g.string_ptrs.insert(id_key, true)
            if inst.has_result { g.value_map.insert(id_key, str_alloca) }
            return g
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Struct: insert field
    if op == OP_INSERT_FIELD() {
        let mut base_val = 0
        if inst.field_base.kind == VAL_UNDEF() {
            -- Create undef of the result type
            let result_ty = map_ir_type_to_llvm(g, inst.result_type)
            base_val = dm_llvm_get_undef(result_ty)
        } else {
            base_val = resolve_value(g, inst.field_base)
            g = resolve_value_with_counter(g, inst.field_base)
        }
        let mut val = resolve_value(g, inst.field_value)
        g = resolve_value_with_counter(g, inst.field_value)
        -- Coerce the inserted value to the aggregate field's LLVM type.
        -- IR emits i64 constants as placeholders for fields of other types
        -- (i32 tags, the unused Result payload, Box ptr fields, ...).
        let exp_ir_ty = expected_field_ir_type(g, inst.result_type, inst.field_index)
        if exp_ir_ty != "" {
            let exp_ty = map_ir_type_to_llvm(g, exp_ir_ty)
            let val_ty = dm_llvm_type_of(val)
            if val_ty != exp_ty {
                let ek = dm_llvm_get_type_kind(exp_ty)
                let vk = dm_llvm_get_type_kind(val_ty)
                if inst.field_value.kind == VAL_UNDEF() {
                    val = dm_llvm_get_undef(exp_ty)
                } else if inst.field_value.kind == VAL_CONST_INT() {
                    if ek == 8 {
                        val = dm_llvm_const_int(exp_ty, inst.field_value.int_val, 1)
                    } else if ek == 2 or ek == 3 {
                        val = dm_llvm_build_si_to_fp(b, val, exp_ty, "itof")
                    } else {
                        val = dm_llvm_const_null(exp_ty)
                    }
                } else if ek == 8 and vk == 8 {
                    if dm_llvm_get_int_type_width(val_ty) < dm_llvm_get_int_type_width(exp_ty) {
                        val = dm_llvm_build_sext(b, val, exp_ty, "sext")
                    } else {
                        val = dm_llvm_build_trunc(b, val, exp_ty, "trunc")
                    }
                } else if (ek == 2 or ek == 3) and vk == 8 {
                    val = dm_llvm_build_si_to_fp(b, val, exp_ty, "itof")
                } else if ek == 8 and (vk == 2 or vk == 3) {
                    val = dm_llvm_build_fp_to_si(b, val, exp_ty, "ftoi")
                } else if ek == 12 and vk == 8 {
                    val = dm_llvm_build_int_to_ptr(b, val, exp_ty, "itop")
                } else if ek == 8 and vk == 12 {
                    val = dm_llvm_build_ptr_to_int(b, val, exp_ty, "ptoi")
                }
            }
        }
        -- If inserting a string value (pointer to dm_string), load the actual struct
        if is_string_ptr(g, inst.field_value) {
            val = dm_llvm_build_load2(b, g.dm_string_type, val, "str.load")
        }
        let result = dm_llvm_build_insert_value(b, base_val, val, inst.field_index, "insert")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Call: delegate to llvm_gen_call module
    if op == OP_CALL() {
        g = generate_call(g, inst)
        return g
    }
    if op == OP_CALL_PTR() {
        g = generate_call_ptr(g, inst)
        return g
    }
    -- Cast
    if op == OP_CAST() {
        let val = resolve_value(g, inst.cast_val)
        g = resolve_value_with_counter(g, inst.cast_val)
        let result = generate_cast(g, val, inst.cast_from, inst.cast_to)
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Phi
    if op == OP_PHI() {
        let phi_ty = map_ir_type_to_llvm(g, inst.result_type)
        let phi_val = dm_llvm_build_phi(b, phi_ty, "phi")
        -- Add incoming values/blocks
        let mut pi = 0
        while pi < inst.phi_entries.len() {
            let entry = inst.phi_entries[pi]
            let incoming_val = resolve_value(g, entry.value)
            g = resolve_value_with_counter(g, entry.value)
            if g.block_map.contains(entry.block_label) {
                let incoming_block = g.block_map.get(entry.block_label)
                dm_llvm_add_incoming_one(phi_val, incoming_val, incoming_block)
            }
            pi = pi + 1
        }
        if inst.has_result { g.value_map.insert(id_key, phi_val) }
        return g
    }
    -- String concatenation
    if op == OP_STRING_CONCAT() {
        g = generate_string_concat(g, inst)
        return g
    }
    -- String equality
    if op == OP_STRING_EQ() {
        g = generate_string_eq(g, inst)
        return g
    }
    -- List operations
    if op == OP_LIST_NEW() {
        g = generate_list_new(g, inst)
        return g
    }
    if op == OP_LIST_PUSH() {
        g = generate_list_push(g, inst)
        return g
    }
    if op == OP_LIST_GET() {
        g = generate_list_get(g, inst)
        return g
    }
    if op == OP_LIST_LEN() {
        g = generate_list_len(g, inst)
        return g
    }
    if op == OP_LIST_POP() {
        g = generate_list_pop(g, inst)
        return g
    }
    -- Map operations
    if op == OP_MAP_NEW() {
        g = generate_map_new(g, inst)
        return g
    }
    if op == OP_MAP_INSERT() {
        g = generate_map_insert(g, inst)
        return g
    }
    if op == OP_MAP_GET() {
        g = generate_map_get(g, inst)
        return g
    }
    if op == OP_MAP_CONTAINS() {
        g = generate_map_contains(g, inst)
        return g
    }
    -- Panic
    if op == OP_PANIC() {
        g = generate_panic(g, inst)
        return g
    }
    -- SIMD: delegate to llvm_gen_simd module
    if op == OP_SIMD_SPLAT() or op == OP_SIMD_SET() or op == OP_SIMD_ADD() or op == OP_SIMD_SUB() or op == OP_SIMD_MUL() or op == OP_SIMD_DIV() or op == OP_SIMD_EXTRACT() {
        g = generate_simd_instruction(g, inst)
        return g
    }
    -- Unhandled op: skip
    return g
}

-- ============================================================
-- CAST GENERATION
-- ============================================================

fn generate_cast(gen: LLVMGenerator, val: int, from: string, to: string) -> int {
    let b = gen.builder
    let from_is_int = is_int_type(from)
    let to_is_int = is_int_type(to)
    let from_is_float = lg_is_float_type(from)
    let to_is_float = lg_is_float_type(to)
    -- Int to int: truncate or sign/zero-extend
    if from_is_int and to_is_int {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        let mut is_signed = 1
        if lg_is_unsigned_type(from) { is_signed = 0 }
        return dm_llvm_build_int_cast2(b, val, dest_ty, is_signed, "cast")
    }
    -- Int to float
    if from_is_int and to_is_float {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        if lg_is_unsigned_type(from) {
            return dm_llvm_build_ui_to_fp(b, val, dest_ty, "cast")
        }
        return dm_llvm_build_si_to_fp(b, val, dest_ty, "cast")
    }
    -- Float to int
    if from_is_float and to_is_int {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        return dm_llvm_build_fp_to_si(b, val, dest_ty, "cast")
    }
    -- Float to float
    if from_is_float and to_is_float {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        return dm_llvm_build_fp_cast(b, val, dest_ty, "cast")
    }
    -- Int to pointer (inttoptr)
    if from_is_int and to == "ptr" {
        let dest_ty = dm_llvm_pointer_type(gen.context)
        return dm_llvm_build_int_to_ptr(b, val, dest_ty, "cast")
    }
    -- Fallback: return value unchanged
    return val
}

-- ============================================================
-- STRING OPERATIONS
-- ============================================================

fn generate_string_concat(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    -- Resolve LHS and RHS (both should be string pointers or const strings)
    let lhs = resolve_value(g, inst.lhs)
    let rhs = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Call dm_string_concat wrapper (takes two dm_string* pointers, writes result via sret)
    if g.function_map.contains("dm_string_concat") {
        let func = g.function_map.get("dm_string_concat")
        let fn_ty = g.fn_type_map.get("dm_string_concat")
        -- Allocate sret for result
        let sret_name = "sret." + int_to_string(g.sret_counter)
        g.sret_counter = g.sret_counter + 1
        let sret = dm_llvm_build_alloca(b, g.dm_string_type, sret_name)
        -- Create dm_string allocas for const string args if needed
        let mut lhs_ptr = lhs
        if inst.lhs.kind == VAL_CONST_STRING() {
            lhs_ptr = create_dm_string(g, lhs)
        }
        let mut rhs_ptr = rhs
        if inst.rhs.kind == VAL_CONST_STRING() {
            rhs_ptr = create_dm_string(g, rhs)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(sret)
        dm_llvm_buf_push(lhs_ptr)
        dm_llvm_buf_push(rhs_ptr)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
        g.string_ptrs.insert(id_key, true)
        if inst.has_result { g.value_map.insert(id_key, sret) }
    }
    return g
}

fn generate_string_eq(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let lhs = resolve_value(g, inst.lhs)
    let rhs = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Call dm_string_eq wrapper (takes two dm_string*, returns i1)
    if g.function_map.contains("dm_string_eq") {
        let func = g.function_map.get("dm_string_eq")
        let fn_ty = g.fn_type_map.get("dm_string_eq")
        let mut lhs_ptr = lhs
        if inst.lhs.kind == VAL_CONST_STRING() {
            lhs_ptr = create_dm_string(g, lhs)
        }
        let mut rhs_ptr = rhs
        if inst.rhs.kind == VAL_CONST_STRING() {
            rhs_ptr = create_dm_string(g, rhs)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(lhs_ptr)
        dm_llvm_buf_push(rhs_ptr)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "streq")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

-- Create a dm_string on the stack from a raw C string pointer
fn create_dm_string(gen: LLVMGenerator, raw_str: int) -> int {
    let b = gen.builder
    -- Allocate dm_string on stack
    let sret_name = "sret." + int_to_string(gen.sret_counter)
    let sret = dm_llvm_build_alloca(b, gen.dm_string_type, sret_name)
    -- Call dm_string_new wrapper
    if gen.function_map.contains("dm_string_new") {
        let func = gen.function_map.get("dm_string_new")
        let fn_ty = gen.fn_type_map.get("dm_string_new")
        dm_llvm_buf_clear()
        dm_llvm_buf_push(sret)
        dm_llvm_buf_push(raw_str)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return sret
}

-- ============================================================
-- LIST OPERATIONS
-- ============================================================

fn generate_list_new(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let id_key = int_to_string(inst.id)
    -- Call dm_list_new() runtime function
    if g.function_map.contains("dm_list_new") {
        let func = g.function_map.get("dm_list_new")
        let fn_ty = g.fn_type_map.get("dm_list_new")
        dm_llvm_buf_clear()
        let result = dm_llvm_build_call2_buf(g.builder, fn_ty, func, "list")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_list_push(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let list_val = resolve_value(g, inst.lhs)
    let elem_val = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Pick the correct runtime function based on element type
    let mut fn_name = "dm_list_int64_push"
    if inst.callee == "dm_list_double_push" or inst.result_type == "f64" {
        fn_name = "dm_list_double_push"
    } else if inst.callee == "dm_list_string_push" or inst.callee == "llvm_dm_list_string_push" or inst.result_type == "string" {
        fn_name = "dm_list_string_push"
    }
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(list_val)
        dm_llvm_buf_push(elem_val)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return g
}

fn generate_list_get(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let list_val = resolve_value(g, inst.lhs)
    let idx_val = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Pick the correct runtime function based on element type
    let mut fn_name = "dm_list_int64_get"
    if inst.result_type == "f64" {
        fn_name = "dm_list_double_get"
    } else if inst.result_type == "string" {
        fn_name = "dm_list_string_get"
    }
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        if fn_name == "dm_list_string_get" {
            -- String list get uses sret pattern
            g = get_dm_string_type(g)
            let sret = dm_llvm_build_alloca(b, g.dm_string_type, "listget_sret")
            dm_llvm_buf_clear()
            dm_llvm_buf_push(sret)
            dm_llvm_buf_push(list_val)
            dm_llvm_buf_push(idx_val)
            dm_llvm_build_call2_buf(b, fn_ty, func, "")
            if inst.has_result { g.value_map.insert(id_key, sret) }
        } else {
            dm_llvm_buf_clear()
            dm_llvm_buf_push(list_val)
            dm_llvm_buf_push(idx_val)
            let result = dm_llvm_build_call2_buf(b, fn_ty, func, "listget")
            if inst.has_result { g.value_map.insert(id_key, result) }
        }
    }
    return g
}

fn generate_list_len(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let list_val = resolve_value(g, inst.operand)
    g = resolve_value_with_counter(g, inst.operand)
    -- Use type-specific len function (all return i64)
    let mut fn_name = "dm_list_int64_len"
    if inst.result_type == "f64" or inst.callee == "dm_list_double_len" {
        fn_name = "dm_list_double_len"
    } else if inst.result_type == "string" or inst.callee == "dm_list_string_len" {
        fn_name = "dm_list_string_len"
    } else if inst.callee == "dm_list_generic_len" {
        fn_name = "dm_list_generic_len"
    }
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(list_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "listlen")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_list_pop(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let list_val = resolve_value(g, inst.operand)
    g = resolve_value_with_counter(g, inst.operand)
    let fn_name = "dm_list_int64_pop"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(list_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "listpop")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

-- ============================================================
-- MAP OPERATIONS
-- ============================================================

fn generate_map_new(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let id_key = int_to_string(inst.id)
    let fn_name = "dm_map_new"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        let result = dm_llvm_build_call2_buf(g.builder, fn_ty, func, "map")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_map_insert(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let map_val = resolve_value(g, inst.map_val)
    let key_val = resolve_value(g, inst.map_key)
    let value_val = resolve_value(g, inst.map_value)
    g = resolve_value_with_counter(g, inst.map_val)
    g = resolve_value_with_counter(g, inst.map_key)
    g = resolve_value_with_counter(g, inst.map_value)
    let fn_name = "dm_map_insert"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(map_val)
        dm_llvm_buf_push(key_val)
        dm_llvm_buf_push(value_val)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return g
}

fn generate_map_get(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let map_val = resolve_value(g, inst.map_val)
    let key_val = resolve_value(g, inst.map_key)
    g = resolve_value_with_counter(g, inst.map_val)
    g = resolve_value_with_counter(g, inst.map_key)
    let fn_name = "dm_map_get"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(map_val)
        dm_llvm_buf_push(key_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "mapget")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_map_contains(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let map_val = resolve_value(g, inst.map_val)
    let key_val = resolve_value(g, inst.map_key)
    g = resolve_value_with_counter(g, inst.map_val)
    g = resolve_value_with_counter(g, inst.map_key)
    let fn_name = "dm_map_contains"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(map_val)
        dm_llvm_buf_push(key_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "maphas")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

-- ============================================================
-- PANIC
-- ============================================================

fn generate_panic(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let msg = resolve_value(g, inst.operand)
    g = resolve_value_with_counter(g, inst.operand)
    let fn_name = "dm_panic"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        -- If operand is const_string, wrap in dm_string
        let mut msg_val = msg
        if inst.operand.kind == VAL_CONST_STRING() {
            msg_val = create_dm_string(g, msg)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(msg_val)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    dm_llvm_build_unreachable(b)
    return g
}

-- ============================================================
-- TERMINATOR GENERATION
-- ============================================================

fn generate_terminator(gen: LLVMGenerator, term: IRTerminator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    if term.kind == TERM_BR() {
        if g.block_map.contains(term.target) {
            let bb = g.block_map.get(term.target)
            dm_llvm_build_br(b, bb)
        }
        return g
    }
    if term.kind == TERM_BR_COND() {
        let mut cond = resolve_value(g, term.cond)
        g = resolve_value_with_counter(g, term.cond)
        -- Coerce non-i1 conditions (e.g. Option/Result/Enum i32 tags) to i1
        let cond_ty = dm_llvm_type_of(cond)
        let cond_kind = dm_llvm_get_type_kind(cond_ty)
        if cond_kind == 8 and dm_llvm_get_int_type_width(cond_ty) != 1 {
            cond = dm_llvm_build_icmp(b, dm_llvm_int_ne(), cond, dm_llvm_const_int(cond_ty, 0, 0), "tobool")
        }
        if cond_kind == 2 or cond_kind == 3 {
            cond = dm_llvm_build_fcmp(b, dm_llvm_real_one(), cond, dm_llvm_const_real(cond_ty, 0.0), "tobool")
        }
        if cond_kind == 12 {
            cond = dm_llvm_build_icmp(b, dm_llvm_int_ne(), cond, dm_llvm_const_null(cond_ty), "tobool")
        }
        if g.block_map.contains(term.true_label) and g.block_map.contains(term.false_label) {
            let true_bb = g.block_map.get(term.true_label)
            let false_bb = g.block_map.get(term.false_label)
            dm_llvm_build_cond_br(b, cond, true_bb, false_bb)
        }
        return g
    }
    if term.kind == TERM_RET() {
        let ctx = g.context
        let is_main = func.name == "main"
        if is_main {
            -- main() always returns i32 0
            let ret_val = dm_llvm_const_int(dm_llvm_int32_type(ctx), 0, 0)
            dm_llvm_build_ret(b, ret_val)
        } else if g.returns_sret.contains(func.name) {
            -- sret convention: store return value to the sret output pointer (param 0), then ret void
            let mut ret_val = resolve_value(g, term.ret_val)
            g = resolve_value_with_counter(g, term.ret_val)
            -- If returning a string pointer, load the struct before storing to sret
            if func.return_type == "string" and is_string_ptr(g, term.ret_val) {
                ret_val = dm_llvm_build_load2(b, g.dm_string_type, ret_val, "sret.str")
            }
            let llvm_fn = g.function_map.get(func.name)
            let sret_ptr = dm_llvm_get_param(llvm_fn, 0)
            dm_llvm_build_store(b, ret_val, sret_ptr)
            dm_llvm_build_ret_void(b)
        } else {
            -- If returning string pointer, load the struct first
            if func.return_type == "string" and is_string_ptr(g, term.ret_val) {
                let ptr_val = resolve_value(g, term.ret_val)
                g = resolve_value_with_counter(g, term.ret_val)
                let loaded = dm_llvm_build_load2(b, g.dm_string_type, ptr_val, "ret.str")
                dm_llvm_build_ret(b, loaded)
            } else {
                let ret_val = resolve_value(g, term.ret_val)
                g = resolve_value_with_counter(g, term.ret_val)
                dm_llvm_build_ret(b, ret_val)
            }
        }
        return g
    }
    if term.kind == TERM_RET_VOID() {
        let is_main = func.name == "main"
        if is_main {
            let ret_val = dm_llvm_const_int(dm_llvm_int32_type(g.context), 0, 0)
            dm_llvm_build_ret(b, ret_val)
        } else {
            dm_llvm_build_ret_void(b)
        }
        return g
    }
    if term.kind == TERM_UNREACHABLE() {
        dm_llvm_build_unreachable(b)
        return g
    }
    return g
}

-- ============================================================
-- TOP-LEVEL MODULE GENERATION
-- ============================================================

fn lg_generate_module(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    -- Phase 0: Create dm_string struct type
    g = get_dm_string_type(g)
    -- Phase 1: Declare struct types
    g = declare_struct_types(g)
    -- Phase 2: Generate global variables
    g = generate_globals(g)
    -- Phase 3: Declare all functions
    let mut i = 0
    while i < g.ir_module.functions.len() {
        g = lg_declare_function(g, g.ir_module.functions[i])
        i = i + 1
    }
    -- Phase 4: Generate function bodies (non-extern only)
    i = 0
    while i < g.ir_module.functions.len() {
        let func = g.ir_module.functions[i]
        if func.is_extern == false and func.blocks.len() > 0 {
            g = lg_generate_function(g, func)
        }
        i = i + 1
    }
    return g
}

-- ============================================================
-- EMIT: Object file, LLVM IR, verification
-- ============================================================

fn emit_to_object(gen: LLVMGenerator, filename: string, opt_level: int) -> int {
    dm_llvm_initialize_all_targets()
    let triple_ptr = dm_llvm_get_default_target_triple()
    -- Convert C string to dAImond string
    let triple = dm_llvm_c_str_to_string(triple_ptr)
    dm_llvm_free_c_string(triple_ptr)
    -- Create target machine
    let tm = dm_llvm_create_target_machine(triple, opt_level)
    if tm == 0 {
        eprintln("Error: Failed to create target machine")
        return 1
    }
    -- Set module target triple and data layout
    dm_llvm_module_set_target(gen.llvm_module, triple)
    let layout_ptr = dm_llvm_get_target_data_layout(tm)
    let layout = dm_llvm_c_str_to_string(layout_ptr)
    dm_llvm_free_c_string(layout_ptr)
    dm_llvm_module_set_data_layout(gen.llvm_module, layout)
    -- Run optimization passes if opt_level > 0
    if opt_level > 0 {
        let mut passes = "default<O1>"
        if opt_level == 2 { let passes = "default<O2>" }
        if opt_level >= 3 { let passes = "default<O3>" }
        dm_llvm_run_passes(gen.llvm_module, passes, tm)
    }
    -- Emit object file
    let result = dm_llvm_target_machine_emit_to_file(tm, gen.llvm_module, filename, dm_llvm_object_file())
    dm_llvm_dispose_target_machine(tm)
    return result
}

fn emit_to_ir_file(gen: LLVMGenerator, filename: string) -> int {
    return dm_llvm_module_print_to_file(gen.llvm_module, filename)
}

fn verify_module(gen: LLVMGenerator) -> int {
    return dm_llvm_module_verify(gen.llvm_module)
}

fn declare_user_extern_wrapper(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    let returns_string = func.return_type == "string"
    let ptr_ty = dm_llvm_pointer_type(ctx)
    let void_ty = dm_llvm_void_type(ctx)

    -- Step 1: Declare the real C function with C types
    let mut c_param_count = 0
    dm_llvm_buf_clear()
    let mut ci = 0
    while ci < func.params.len() {
        let p = func.params[ci]
        if p.type_id == "string" {
            dm_llvm_buf_push(ptr_ty)
        } else {
            dm_llvm_buf_push(map_ir_type_to_llvm(g, p.type_id))
        }
        c_param_count = c_param_count + 1
        ci = ci + 1
    }
    let c_ret_ty = if returns_string { ptr_ty } else { map_ir_type_to_llvm(g, func.return_type) }
    let c_fn_ty = dm_llvm_function_type_buf(c_ret_ty, 0)
    let real_fn = dm_llvm_module_add_function(g.llvm_module, func.name, c_fn_ty)

    -- Step 2: Declare the wrapper function with dm_string-based API
    let mut w_param_count = 0
    dm_llvm_buf_clear()
    if returns_string {
        dm_llvm_buf_push(ptr_ty)
        w_param_count = w_param_count + 1
    }
    ci = 0
    while ci < func.params.len() {
        dm_llvm_buf_push(ptr_ty)
        w_param_count = w_param_count + 1
        ci = ci + 1
    }
    let w_ret_ty = if returns_string { void_ty } else { map_ir_type_to_llvm(g, func.return_type) }
    let w_fn_ty = dm_llvm_function_type_buf(w_ret_ty, 0)
    let wrapper_name = "llvm_" + func.name
    let wrapper_fn = dm_llvm_module_add_function(g.llvm_module, wrapper_name, w_fn_ty)

    -- Step 3: Generate the wrapper body
    let saved_bb = dm_llvm_get_insert_block(g.builder)
    let entry_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "entry")
    dm_llvm_position_at_end(g.builder, entry_bb)

    -- Convert string params: extract data field (field 0) from dm_string*
    g = get_dm_string_type(g)
    let mut w_param_idx = 0
    if returns_string { w_param_idx = 1 }

    dm_llvm_buf_clear()
    ci = 0
    while ci < func.params.len() {
        let p = func.params[ci]
        let wrapper_param = dm_llvm_get_param(wrapper_fn, w_param_idx)
        if p.type_id == "string" {
            let data_ptr = dm_llvm_build_struct_gep2(g.builder, g.dm_string_type, wrapper_param, 0, "data_ptr")
            let data = dm_llvm_build_load2(g.builder, ptr_ty, data_ptr, "data")
            dm_llvm_buf_push(data)
        } else {
            dm_llvm_buf_push(wrapper_param)
        }
        w_param_idx = w_param_idx + 1
        ci = ci + 1
    }
    let call_name = if returns_string { "c_result" } else { "" }
    let c_result = dm_llvm_build_call2_buf(g.builder, c_fn_ty, real_fn, call_name)

    if returns_string {
        let sret_ptr = dm_llvm_get_param(wrapper_fn, 0)
        -- Handle NULL return: call dm_string_new with empty string if NULL
        let null_val = dm_llvm_const_null(ptr_ty)
        let is_null = dm_llvm_build_icmp(g.builder, dm_llvm_int_eq(), c_result, null_val, "is_null")
        let then_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "then_null")
        let else_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "else_nonnull")
        let merge_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "merge")
        dm_llvm_build_cond_br(g.builder, is_null, then_bb, else_bb)

        -- Null case: create empty string
        dm_llvm_position_at_end(g.builder, then_bb)
        let empty_str = dm_llvm_build_global_string_ptr(g.builder, "", "empty_str")
        if g.function_map.contains("dm_string_new") {
            let sn_fn = g.function_map.get("dm_string_new")
            let sn_ty = g.fn_type_map.get("dm_string_new")
            dm_llvm_buf_clear()
            dm_llvm_buf_push(sret_ptr)
            dm_llvm_buf_push(empty_str)
            dm_llvm_build_call2_buf(g.builder, sn_ty, sn_fn, "")
        }
        dm_llvm_build_br(g.builder, merge_bb)

        -- Non-null case: create string from C result
        dm_llvm_position_at_end(g.builder, else_bb)
        if g.function_map.contains("dm_string_new") {
            let sn_fn = g.function_map.get("dm_string_new")
            let sn_ty = g.fn_type_map.get("dm_string_new")
            dm_llvm_buf_clear()
            dm_llvm_buf_push(sret_ptr)
            dm_llvm_buf_push(c_result)
            dm_llvm_build_call2_buf(g.builder, sn_ty, sn_fn, "")
        }
        dm_llvm_build_br(g.builder, merge_bb)

        dm_llvm_position_at_end(g.builder, merge_bb)
        dm_llvm_build_ret_void(g.builder)
    } else {
        dm_llvm_build_ret(g.builder, c_result)
    }

    -- Restore builder position
    if saved_bb != 0 {
        dm_llvm_position_at_end(g.builder, saved_bb)
    }

    -- Store under ORIGINAL name for call resolution
    g.function_map.insert(func.name, wrapper_fn)
    g.fn_type_map.insert(func.name, w_fn_ty)
    g.has_wrapper.insert(func.name, true)
    g.wrapper_returns_string.insert(func.name, returns_string)
    return g
}

fn is_sret_return_type(ret_type: string) -> bool {
    if ret_type == "string" {
        return true
    }
    if ret_type == "result" or starts_with(ret_type, "Result[") or starts_with(ret_type, "result_") {
        return true
    }
    if ret_type == "option" or starts_with(ret_type, "Option[") or starts_with(ret_type, "option_") {
        return true
    }
    return false
}

-- Check if an IR function involves dm_string in params or return type
