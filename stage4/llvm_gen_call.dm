module llvm_gen_call

import ir
import llvm
import llvm_gen

-- ============================================================
-- Call Instruction Handling for LLVM IR Generation
--
-- Handles OP_CALL and OP_CALL_PTR instructions, including:
-- - Direct function calls
-- - Wrapper-based string ABI calls (sret pattern for dm_string returns)
-- - Indirect calls through function pointers
-- ============================================================

-- ============================================================
-- DIRECT CALL (OP_CALL)
-- ============================================================

fn generate_call(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let callee_name = inst.callee
    -- Look up function in function_map
    if g.function_map.contains(callee_name) == false {
        -- Unknown function: skip
        return g
    }
    let callee = g.function_map.get(callee_name)
    let fn_ty = g.fn_type_map.get(callee_name)
    -- Check if this function has a wrapper (string ABI)
    if g.has_wrapper.contains(callee_name) {
        g = generate_wrapper_call(g, inst, callee, fn_ty, callee_name)
        return g
    }
    -- Regular (non-wrapper) call
    -- Resolve arguments, handling string pointer loads
    let mut args: List[int] = []
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let mut val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        -- If arg is a string pointer and target function expects dm_string by value,
        -- load the struct from the pointer
        if is_string_ptr(g, arg) {
            let expects_string = callee_param_is_string(g, callee_name, ai)
            if expects_string {
                val = dm_llvm_build_load2(b, g.dm_string_type, val, "str.arg")
            }
        }
        args.push(val)
        ai = ai + 1
    }
    -- Determine call result name (empty string for void returns)
    let mut call_name = ""
    if inst.has_result {
        call_name = "call"
    }
    -- Build the call
    dm_llvm_buf_clear()
    let mut argi = 0
    while argi < args.len() {
        dm_llvm_buf_push(args[argi])
        argi = argi + 1
    }
    let mut result = dm_llvm_build_call2_buf(b, fn_ty, callee, call_name)
    -- Check if callee returns dm_string: store to alloca and track as string ptr
    if callee_returns_string(g, callee_name) and result != 0 {
        let str_alloca = dm_llvm_build_alloca(b, g.dm_string_type, "str.ret")
        dm_llvm_build_store(b, result, str_alloca)
        g.string_ptrs.insert(id_key, true)
        if inst.has_result { g.value_map.insert(id_key, str_alloca) }
        return g
    }
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- WRAPPER CALL (string ABI via sret pattern)
-- ============================================================

fn generate_wrapper_call(gen: LLVMGenerator, inst: IRInst, callee: int, fn_ty: int, callee_name: string) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let returns_string = wrapper_returns_string_for(g, callee_name)
    if returns_string {
        -- Allocate stack space for the string result (sret pattern)
        let sret_name = "sret." + int_to_string(g.sret_counter)
        g.sret_counter = g.sret_counter + 1
        let sret = dm_llvm_build_alloca(b, g.dm_string_type, sret_name)
        -- Build args: sret pointer first, then resolved args
        let mut args: List[int] = []
        args.push(sret)
        let mut ai = 0
        while ai < inst.call_args.len() {
            let arg = inst.call_args[ai]
            let mut val = resolve_value(g, arg)
            g = resolve_value_with_counter(g, arg)
            -- If arg is a const_string and param expects dm_string, wrap it
            if arg.kind == VAL_CONST_STRING() {
                let param_is_str = orig_param_is_string(g, callee_name, ai)
                if param_is_str {
                    val = create_dm_string_for_call(g, val)
                }
            }
            args.push(val)
            ai = ai + 1
        }
        -- Call wrapper (returns void, result written to sret)
        dm_llvm_buf_clear()
        let mut argi2 = 0
        while argi2 < args.len() {
            dm_llvm_buf_push(args[argi2])
            argi2 = argi2 + 1
        }
        dm_llvm_build_call2_buf(b, fn_ty, callee, "")
        -- The sret alloca IS the result -- a pointer to dm_string
        g.string_ptrs.insert(id_key, true)
        if inst.has_result { g.value_map.insert(id_key, sret) }
        return g
    }
    -- Non-string return, but params may need wrapping
    let mut args: List[int] = []
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let mut val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        -- Wrap const_string args if parameter is string type
        if arg.kind == VAL_CONST_STRING() {
            let param_is_str = orig_param_is_string(g, callee_name, ai)
            if param_is_str {
                val = create_dm_string_for_call(g, val)
            }
        }
        args.push(val)
        ai = ai + 1
    }
    let mut call_name = ""
    if inst.has_result { call_name = "call" }
    dm_llvm_buf_clear()
    let mut argi3 = 0
    while argi3 < args.len() {
        dm_llvm_buf_push(args[argi3])
        argi3 = argi3 + 1
    }
    let result = dm_llvm_build_call2_buf(b, fn_ty, callee, call_name)
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- INDIRECT CALL (OP_CALL_PTR)
-- ============================================================

fn generate_call_ptr(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    -- Resolve the function pointer value
    let fn_ptr = resolve_value(g, inst.callee_val)
    g = resolve_value_with_counter(g, inst.callee_val)
    -- Build the LLVM function type for the indirect call
    -- Map the return type
    let ret_ty = map_ir_type_to_llvm(g, inst.result_type)
    -- Build param types and args simultaneously
    dm_llvm_buf_clear()
    let mut args: List[int] = []
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        args.push(val)
        -- Get the LLVM type of the resolved value
        let val_ty = dm_llvm_type_of(val)
        dm_llvm_buf_push(val_ty)
        ai = ai + 1
    }
    -- Create function type
    let fn_ty = dm_llvm_function_type_buf(ret_ty, 0)
    -- Build the indirect call
    let mut call_name = ""
    if inst.has_result { call_name = "callptr" }
    dm_llvm_buf_clear()
    let mut argi4 = 0
    while argi4 < args.len() {
        dm_llvm_buf_push(args[argi4])
        argi4 = argi4 + 1
    }
    let result = dm_llvm_build_call2_buf(b, fn_ty, fn_ptr, call_name)
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- HELPER FUNCTIONS
-- ============================================================

-- Check if a function requires a string wrapper
fn needs_string_wrapper(gen: LLVMGenerator, fn_name: string) -> bool {
    return gen.has_wrapper.contains(fn_name)
}

-- Get the wrapper function name
fn get_wrapper_name(fn_name: string) -> string {
    return "llvm_" + fn_name
}

-- Check if wrapper returns string (sret pattern)
fn wrapper_returns_string_for(gen: LLVMGenerator, fn_name: string) -> bool {
    if gen.wrapper_returns_string.contains(fn_name) {
        return gen.wrapper_returns_string.get(fn_name)
    }
    return false
}

-- Check if a specific parameter of a callee function is string type
-- Looks up the IR function definition
fn callee_param_is_string(gen: LLVMGenerator, fn_name: string, param_idx: int) -> bool {
    -- Search through IR module functions
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        let func = gen.ir_module.functions[i]
        if func.name == fn_name {
            if param_idx < func.params.len() {
                return func.params[param_idx].type_id == "string"
            }
            return false
        }
        i = i + 1
    }
    return false
}

-- Check if a callee function returns string type
fn callee_returns_string(gen: LLVMGenerator, fn_name: string) -> bool {
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        let func = gen.ir_module.functions[i]
        if func.name == fn_name {
            return func.return_type == "string"
        }
        i = i + 1
    }
    return false
}

-- Check if the original (pre-wrapper) function parameter is string type
-- For wrapper functions, look up using the original name
fn orig_param_is_string(gen: LLVMGenerator, fn_name: string, param_idx: int) -> bool {
    return callee_param_is_string(gen, fn_name, param_idx)
}

-- Create a dm_string on the stack from a raw C string pointer (for call args)
fn create_dm_string_for_call(gen: LLVMGenerator, raw_str: int) -> int {
    let b = gen.builder
    let sret_name = "sret.arg." + int_to_string(gen.sret_counter)
    let sret = dm_llvm_build_alloca(b, gen.dm_string_type, sret_name)
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

-- Find an IR function by name for type info lookups
fn find_ir_function(gen: LLVMGenerator, name: string) -> IRFunction {
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        if gen.ir_module.functions[i].name == name {
            return gen.ir_module.functions[i]
        }
        i = i + 1
    }
    -- Return a dummy function (should not happen in practice)
    return ir_function_new("__not_found__", "void")
}

-- Check if a function parameter at given index takes string type
-- Used for deciding whether to load dm_string from pointer for non-wrapper calls
fn func_param_type_at(gen: LLVMGenerator, fn_name: string, idx: int) -> string {
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        let func = gen.ir_module.functions[i]
        if func.name == fn_name {
            if idx < func.params.len() {
                return func.params[idx].type_id
            }
            return ""
        }
        i = i + 1
    }
    return ""
}

-- Check if a function name refers to a runtime function that needs dm_string wrapper
fn is_runtime_string_fn(fn_name: string) -> bool {
    if starts_with(fn_name, "dm_string_") { return true }
    if starts_with(fn_name, "dm_print") { return true }
    if starts_with(fn_name, "dm_println") { return true }
    if fn_name == "dm_panic" { return true }
    if fn_name == "dm_read_line" { return true }
    if starts_with(fn_name, "dm_file_") { return true }
    if fn_name == "dm_int_to_string" { return true }
    if fn_name == "dm_float_to_string" { return true }
    if fn_name == "dm_bool_to_string" { return true }
    if fn_name == "dm_parse_int" { return true }
    if fn_name == "dm_parse_float" { return true }
    if fn_name == "dm_string_new" { return true }
    return false
}
