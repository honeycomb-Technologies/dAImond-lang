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

-- The type of parameter `idx` of function type `fn_ty`, or 0 past its
-- parameters. Calls take parameter types from their own function type:
-- the LLVM function of a C symbol may have been declared with another.
fn call_param_type(fn_ty: int, idx: int) -> int {
    unsafe {
        if idx >= dm_llvm_count_param_types(fn_ty) { return 0 }
        return dm_llvm_param_type_at(fn_ty, idx)
    }
}

-- Coerce an integer call argument to the callee's parameter width
-- (sext when narrower, trunc when wider). Non-integer pairs and
-- unknown callees/params pass through unchanged.
fn coerce_int_call_arg(gen: LLVMGenerator, fn_ty: int, fn_name: string, user_idx: int, llvm_idx: int, val: int) -> int {
    unsafe {
        if func_param_type_at(gen, fn_name, user_idx) == "" { return val }
        let param_ty = call_param_type(fn_ty, llvm_idx)
        if param_ty == 0 { return val }
        let val_ty = dm_llvm_type_of(val)
        if dm_llvm_get_type_kind(param_ty) != 8 or dm_llvm_get_type_kind(val_ty) != 8 { return val }
        let param_w = dm_llvm_get_int_type_width(param_ty)
        let val_w = dm_llvm_get_int_type_width(val_ty)
        if val_w < param_w { return dm_llvm_build_sext(gen.builder, val, param_ty, "arg.sext") }
        if val_w > param_w { return dm_llvm_build_trunc(gen.builder, val, param_ty, "arg.trunc") }
        return val
    }
}

-- Convert between a pointer and an integer where a call's argument or
-- result is one and the other side wants the other: a source extern fn
-- may declare a C pointer as int, while the compiler passes ptr.
fn coerce_ptr_int(gen: LLVMGenerator, val: int, want_ty: int) -> int {
    unsafe {
        let have_kind = dm_llvm_get_type_kind(dm_llvm_type_of(val))
        let want_kind = dm_llvm_get_type_kind(want_ty)
        if have_kind == 8 and want_kind == 12 { return dm_llvm_build_int_to_ptr(gen.builder, val, want_ty, "int.ptr") }
        if have_kind == 12 and want_kind == 8 { return dm_llvm_build_ptr_to_int(gen.builder, val, want_ty, "ptr.int") }
        return val
    }
}

-- A string argument of a variadic C function as char*: the address of
-- its bytes. A string literal already is one.
fn string_arg_as_cstr(gen: LLVMGenerator, arg: IRValue, val: int) -> int {
    unsafe {
        if arg.kind == VAL_CONST_STRING() { return val }
        let b = gen.builder
        let ptr_ty = dm_llvm_pointer_type(gen.context)
        if is_string_ptr(gen, arg) and dm_llvm_get_type_kind(dm_llvm_type_of(val)) == 12 {
            let data_slot = dm_llvm_build_struct_gep2(b, gen.dm_string_type, val, 0, "cstr.addr")
            return dm_llvm_build_load2(b, ptr_ty, data_slot, "cstr")
        }
        if dm_llvm_type_of(val) == gen.dm_string_type {
            return dm_llvm_build_extract_value(b, val, 0, "cstr")
        }
        return val
    }
}

-- C's calling convention has the caller widen an argument narrower
-- than int to 32 bits, and a C function may read all 32. Mark such
-- arguments of `call`, a call of the C function declared as fn_name,
-- so that LLVM widens them: signed types by sign, the others by zeros.
fn mark_narrow_c_args(gen: LLVMGenerator, call: int, fn_name: string, nparams: int) {
    unsafe {
        let mut i = 0
        while i < nparams {
            let t = func_param_type_at(gen, fn_name, i)
            if t == "i8" or t == "i16" { dm_llvm_add_call_param_attr(call, i, "signext") }
            if t == "u8" or t == "u16" or t == "bool" { dm_llvm_add_call_param_attr(call, i, "zeroext") }
            i = i + 1
        }
    }
}

-- An argument past a variadic C function's parameters, with C's default
-- promotions: float to double, and bool, i8 and i16 to int
fn promote_vararg(gen: LLVMGenerator, val: int) -> int {
    unsafe {
        let ty = dm_llvm_type_of(val)
        let kind = dm_llvm_get_type_kind(ty)
        if kind == 2 { return dm_llvm_build_fp_cast(gen.builder, val, dm_llvm_double_type(gen.context), "vararg.f64") }
        if kind == 8 and dm_llvm_get_int_type_width(ty) < 32 {
            let is_signed = if dm_llvm_get_int_type_width(ty) == 1 { 0 } else { 1 }
            return dm_llvm_build_int_cast2(gen.builder, val, dm_llvm_int32_type(gen.context), is_signed, "vararg.i32")
        }
        return val
    }
}

-- ============================================================
-- INLINE RUNTIME CHECKS
-- Signed integer arithmetic and narrowing casts are checked: the IR
-- calls dm_checked_* and dm_cast_check_*, which are lowered here to
-- a test and a branch to a block that reports the failure and exits.
-- ============================================================

-- The LLVM intrinsic that performs checked operation `name` and
-- reports whether it overflowed; "" when `name` is not one
fn overflow_intrinsic_of(name: string) -> string {
    if name == "dm_checked_add_i64" { return "llvm.sadd.with.overflow.i64" }
    if name == "dm_checked_sub_i64" { return "llvm.ssub.with.overflow.i64" }
    if name == "dm_checked_mul_i64" { return "llvm.smul.with.overflow.i64" }
    return ""
}

fn is_range_check_fn(name: string) -> bool {
    return name == "dm_checked_range_i64" or name == "dm_cast_check_i64" or name == "dm_cast_check_u64"
}

-- Operand `idx` of a check as a 64-bit integer
fn check_operand(gen: LLVMGenerator, inst: IRInst, idx: int) -> int {
    unsafe {
        let i64_ty = dm_llvm_int64_type(gen.context)
        let val = resolve_value(gen, inst.call_args[idx])
        if dm_llvm_type_of(val) == i64_ty { return val }
        return dm_llvm_build_int_cast2(gen.builder, val, i64_ty, 1, "check.arg")
    }
}

-- Make gen.panic_blocks hold the block of the current function that
-- prints `message` and exits. A function has one such block per
-- message, however many checks branch to it.
fn need_panic_block(gen: LLVMGenerator, message: string) -> LLVMGenerator {
    unsafe {
        let mut g = gen
        if g.panic_blocks.contains(message) { return g }
        let resume_at = dm_llvm_get_insert_block(g.builder)
        let bb = dm_llvm_append_basic_block(g.context, g.function_map.get(g.current_func_name), "check_failed")
        dm_llvm_position_at_end(g.builder, bb)
        let mut panic_call = ir_inst_new(0, OP_CALL())
        panic_call.callee = "dm_panic"
        let mut panic_args: List[IRValue] = []
        panic_args.push(ir_val_string(message))
        panic_call.call_args = panic_args
        panic_call.result_type = "void"
        panic_call.has_result = false
        g = generate_call(g, panic_call)
        dm_llvm_build_unreachable(g.builder)
        dm_llvm_position_at_end(g.builder, resume_at)
        g.panic_blocks.insert(message, bb)
        return g
    }
}

-- Branch to the panic block for `message` when `failed` is true, and
-- carry on in a new block otherwise
fn branch_on_failed_check(gen: LLVMGenerator, failed: int, message: string) -> LLVMGenerator {
    unsafe {
        let mut g = need_panic_block(gen, message)
        let ok_bb = dm_llvm_append_basic_block(g.context, g.function_map.get(g.current_func_name), "check_ok")
        dm_llvm_build_cond_br(g.builder, failed, g.panic_blocks.get(message), ok_bb)
        dm_llvm_position_at_end(g.builder, ok_bb)
        return g
    }
}

-- dm_checked_add_i64(a, b) and its sub and mul counterparts: the
-- result, or a panic when it does not fit in 64 bits
fn generate_checked_arith(gen: LLVMGenerator, inst: IRInst, intrinsic: string) -> LLVMGenerator {
    unsafe {
        let mut g = gen
        let b = g.builder
        let i64_ty = dm_llvm_int64_type(g.context)
        if g.function_map.contains(intrinsic) == false {
            -- { i64 result, i1 overflowed } (i64, i64)
            dm_llvm_buf_clear()
            dm_llvm_buf_push(i64_ty)
            dm_llvm_buf_push(dm_llvm_int1_type(g.context))
            let pair_ty = dm_llvm_struct_type_buf(g.context, 2, 0)
            dm_llvm_buf_clear()
            dm_llvm_buf_push(i64_ty)
            dm_llvm_buf_push(i64_ty)
            let intrinsic_ty = dm_llvm_function_type_buf(pair_ty, 0)
            g.function_map.insert(intrinsic, dm_llvm_module_add_function(g.llvm_module, intrinsic, intrinsic_ty))
            g.fn_type_map.insert(intrinsic, intrinsic_ty)
        }
        let lhs = check_operand(g, inst, 0)
        let rhs = check_operand(g, inst, 1)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(lhs)
        dm_llvm_buf_push(rhs)
        let pair = dm_llvm_build_call2_buf(b, g.fn_type_map.get(intrinsic), g.function_map.get(intrinsic), "checked")
        let result = dm_llvm_build_extract_value(b, pair, 0, "arith")
        let overflowed = dm_llvm_build_extract_value(b, pair, 1, "overflowed")
        g = branch_on_failed_check(g, overflowed, "integer overflow")
        if inst.has_result { g.value_map.insert(int_to_string(inst.id), result) }
        return g
    }
}

-- dm_checked_range_i64(v, lo, hi) and dm_cast_check_i64(v, lo, hi)
-- give v when lo <= v <= hi; dm_cast_check_u64(v, hi) gives v when
-- v <= hi, unsigned. Otherwise they panic.
fn generate_range_check(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    unsafe {
        let mut g = gen
        let b = g.builder
        let v = check_operand(g, inst, 0)
        let mut failed = 0
        if inst.callee == "dm_cast_check_u64" {
            -- 34 is LLVM's unsigned greater-than
            failed = dm_llvm_build_icmp(b, 34, v, check_operand(g, inst, 1), "above")
        } else {
            let below = dm_llvm_build_icmp(b, dm_llvm_int_slt(), v, check_operand(g, inst, 1), "below")
            let above = dm_llvm_build_icmp(b, dm_llvm_int_sgt(), v, check_operand(g, inst, 2), "above")
            failed = dm_llvm_build_or(b, below, above, "out_of_range")
        }
        let mut message = "integer cast out of range"
        if inst.callee == "dm_checked_range_i64" { message = "integer overflow" }
        g = branch_on_failed_check(g, failed, message)
        if inst.has_result { g.value_map.insert(int_to_string(inst.id), v) }
        return g
    }
}

fn generate_call(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    unsafe {
        let mut g = gen
        let b = g.builder
        let id_key = int_to_string(inst.id)
        let callee_name = inst.callee
        let intrinsic = overflow_intrinsic_of(callee_name)
        if intrinsic != "" and inst.call_args.len() == 2 {
            return generate_checked_arith(g, inst, intrinsic)
        }
        if is_range_check_fn(callee_name) and inst.call_args.len() >= 2 {
            return generate_range_check(g, inst)
        }
        -- symbol_addr("name"): the address of a data symbol
        if callee_name == "dm_symbol_addr" and inst.call_args.len() == 1 {
            let symbol = inst.call_args[0].str_val
            if g.function_map.contains(symbol) or g.c_symbols.contains(symbol) {
                g.type_errors.push(lg_error(g, "symbol_addr(\"" + symbol + "\") names a function, not a data symbol"))
            }
            g = declare_data_symbol(g, symbol, dm_llvm_int8_type(g.context))
            let symbol_addr = dm_llvm_build_ptr_to_int(b, g.data_symbols.get(symbol), dm_llvm_int64_type(g.context), "symbol.addr")
            if inst.has_result { g.value_map.insert(id_key, symbol_addr) }
            return g
        }
        -- static_addr("name", size): the address of the program's own
        -- storage of that name
        if callee_name == "dm_static_addr" and inst.call_args.len() == 2 {
            let storage = inst.call_args[0].str_val
            let size = inst.call_args[1].int_val
            g = define_static_storage(g, storage, size)
            let storage_addr = dm_llvm_build_ptr_to_int(b, g.data_symbols.get(static_symbol(storage)), dm_llvm_int64_type(g.context), "static.addr")
            if inst.has_result { g.value_map.insert(id_key, storage_addr) }
            return g
        }
        -- len(s) reads the string's length field
        if callee_name == "dm_string_len" and inst.call_args.len() == 1 {
            let s_arg = inst.call_args[0]
            let s_res = resolve_value(g, s_arg)
            g = resolve_value_with_counter(g, s_arg)
            let mut s_len = 0
            if s_arg.kind == VAL_CONST_STRING() {
                s_len = literal_len_const(g, s_arg)
            } else if dm_llvm_get_type_kind(dm_llvm_type_of(s_res)) == 12 {
                let len_slot = dm_llvm_build_struct_gep2(b, g.dm_string_type, s_res, 1, "str.len.addr")
                s_len = dm_llvm_build_load2(b, dm_llvm_int64_type(g.context), len_slot, "str.len")
            } else {
                s_len = dm_llvm_build_extract_value(b, s_res, 1, "str.len")
            }
            if inst.has_result { g.value_map.insert(id_key, s_len) }
            return g
        }
        -- xs.len() reads the list's length field
        if is_list_len_fn(callee_name) and inst.call_args.len() == 1 {
            let list_res = resolve_value(g, inst.call_args[0])
            let mut list_len = 0
            if dm_llvm_get_type_kind(dm_llvm_type_of(list_res)) == 12 {
                let len_slot = dm_llvm_build_struct_gep2(b, g.dm_string_type, list_res, 1, "list.len.addr")
                list_len = dm_llvm_build_load2(b, dm_llvm_int64_type(g.context), len_slot, "list.len")
            } else {
                list_len = dm_llvm_build_extract_value(b, list_res, 1, "list.len")
            }
            if inst.has_result { g.value_map.insert(id_key, list_len) }
            return g
        }
        -- An empty list is all zeros
        if is_list_new_fn(callee_name) and inst.call_args.len() == 1 {
            dm_llvm_build_store(b, dm_llvm_const_null(g.dm_string_type), resolve_value(g, inst.call_args[0]))
            return g
        }
        -- Look up function in function_map
        if g.function_map.contains(callee_name) == false {
            -- A call the IR generator emitted of a function it never
            -- declared. Dropping it would make the program do less than
            -- its source says.
            g.type_errors.push(lg_error(g, "internal: call of '" + callee_name + "', which is not declared"))
            return g
        }
        let callee = g.function_map.get(callee_name)
        let fn_ty = g.fn_type_map.get(callee_name)
        -- Check if this function has a wrapper (string ABI)
        if g.has_wrapper.contains(callee_name) {
            g = generate_wrapper_call(g, inst, callee, fn_ty, callee_name)
            return g
        }
        -- Check if callee uses sret return convention (result/option types)
        if g.returns_sret.contains(callee_name) {
            g = generate_sret_call(g, inst, callee, fn_ty, callee_name)
            return g
        }
        -- Regular (non-wrapper) call
        -- Resolve arguments, handling string pointer loads
        let nparams = dm_llvm_count_param_types(fn_ty)
        let is_source_extern = g.ir_module.user_extern_fns.contains(callee_name)
        let is_variadic = is_source_extern and dm_llvm_is_function_var_arg(fn_ty) != 0
        let mut args: List[int] = []
        let mut ai = 0
        while ai < inst.call_args.len() {
            let arg = inst.call_args[ai]
            let mut val = resolve_value(g, arg)
            g = resolve_value_with_counter(g, arg)
            -- Strings go to a variadic C function as char*, and arguments
            -- past its parameters get C's default promotions
            let is_string_arg = arg.kind == VAL_CONST_STRING() or is_string_ptr(g, arg) or dm_llvm_type_of(val) == g.dm_string_type
            if is_variadic and is_string_arg {
                val = string_arg_as_cstr(g, arg, val)
            } else if is_variadic and ai >= nparams {
                val = promote_vararg(g, val)
            }
            if ai < nparams and (is_variadic and is_string_arg) == false {
                if arg.kind == VAL_CONST_STRING() and call_param_type(fn_ty, ai) == g.dm_string_type {
                    val = string_operand(g, arg, val, true)
                }
                -- A string or list pointer passed by value: pass the value
                val = load_handle_as(g, arg, val, call_param_type(fn_ty, ai))
                val = coerce_int_call_arg(g, fn_ty, callee_name, ai, ai, val)
                -- A source extern and a ported runtime function may take an
                -- int where the compiler has a pointer, and the other way round
                if is_source_extern or g.rt_ports.contains(callee_name) {
                    val = coerce_ptr_int(g, val, dm_llvm_param_type_at(fn_ty, ai))
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
        g = check_call_args(g, fn_ty, callee)
        let mut result = dm_llvm_build_call2_buf(b, fn_ty, callee, call_name)
        if is_source_extern { mark_narrow_c_args(g, result, callee_name, nparams) }
        if is_source_extern and inst.has_result and (inst.result_type == "ptr" or is_int_type(inst.result_type)) {
            result = coerce_ptr_int(g, result, map_ir_type_to_llvm(g, inst.result_type))
        }
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
}

-- ============================================================
-- WRAPPER CALL (string ABI via sret pattern)
-- ============================================================

fn generate_wrapper_call(gen: LLVMGenerator, inst: IRInst, callee: int, fn_ty: int, callee_name: string) -> LLVMGenerator {
    unsafe {
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
                        val = create_dm_string(g, arg, val)
                    }
                }
                val = coerce_int_call_arg(g, fn_ty, callee_name, ai, ai + 1, val)
                let wparam_ty = call_param_type(fn_ty, ai + 1)
                if wparam_ty != 0 { val = load_handle_as(g, arg, val, wparam_ty) }
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
            g = check_call_args(g, fn_ty, callee)
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
                    val = create_dm_string(g, arg, val)
                }
            }
            val = coerce_int_call_arg(g, fn_ty, callee_name, ai, ai, val)
            let wparam_ty = call_param_type(fn_ty, ai)
            if wparam_ty != 0 { val = load_handle_as(g, arg, val, wparam_ty) }
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
        g = check_call_args(g, fn_ty, callee)
        let result = dm_llvm_build_call2_buf(b, fn_ty, callee, call_name)
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
}

-- ============================================================
-- SRET CALL (result/option return via output pointer)
-- ============================================================

fn generate_call_ptr(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    unsafe {
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
        g = check_call_args(g, fn_ty, fn_ptr)
        let result = dm_llvm_build_call2_buf(b, fn_ty, fn_ptr, call_name)
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
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
    if fn_name == "dm_uint_to_string" { return true }
    if fn_name == "dm_float_to_string" { return true }
    if fn_name == "dm_bool_to_string" { return true }
    if fn_name == "dm_parse_int" { return true }
    if fn_name == "dm_parse_float" { return true }
    if fn_name == "dm_string_new" { return true }
    return false
}


-- ============================================================
-- SIMD Instruction Handling for LLVM IR Generation
--
-- Translates OP_SIMD_* IR instructions to LLVM vector operations.
-- SIMD types map to LLVM fixed-width vector types:
--   f32x4 -> <4 x float>, f32x8 -> <8 x float>
--   f64x2 -> <2 x double>, f64x4 -> <4 x double>
--   i32x4 -> <4 x i32>,    i32x8 -> <8 x i32>
--   i64x2 -> <2 x i64>,    i64x4 -> <4 x i64>
-- ============================================================

-- ============================================================
-- SIMD TYPE HELPERS
-- ============================================================

-- Get the LLVM vector type for a SIMD type name

fn generate_sret_call(gen: LLVMGenerator, inst: IRInst, callee: int, fn_ty: int, callee_name: string) -> LLVMGenerator {
    unsafe {
        let mut g = gen
        let b = g.builder
        let id_key = int_to_string(inst.id)
        -- Allocate stack space for the result struct (sret pattern)
        let sret_name = "sret." + int_to_string(g.sret_counter)
        g.sret_counter = g.sret_counter + 1
        -- Look up the IR function to get its return type for correct alloca sizing
        let ir_func = find_ir_function(g, callee_name)
        let ret_llvm_ty = map_ir_type_to_llvm(g, ir_func.return_type)
        let sret = dm_llvm_build_alloca(b, ret_llvm_ty, sret_name)
        -- Build args: sret pointer first, then resolved args
        let mut args: List[int] = []
        args.push(sret)
        let mut ai = 0
        while ai < inst.call_args.len() {
            let arg = inst.call_args[ai]
            let mut val = resolve_value(g, arg)
            g = resolve_value_with_counter(g, arg)
            -- A string or list pointer passed by value: pass the value
            let sparam_ty = call_param_type(fn_ty, ai + 1)
            if arg.kind == VAL_CONST_STRING() and sparam_ty == g.dm_string_type {
                val = string_operand(g, arg, val, true)
            }
            if sparam_ty != 0 { val = load_handle_as(g, arg, val, sparam_ty) }
            val = coerce_int_call_arg(g, fn_ty, callee_name, ai, ai + 1, val)
            if sparam_ty != 0 and g.rt_ports.contains(callee_name) {
                val = coerce_ptr_int(g, val, sparam_ty)
            }
            args.push(val)
            ai = ai + 1
        }
        -- Call function (returns void, result written to sret pointer)
        dm_llvm_buf_clear()
        let mut argi = 0
        while argi < args.len() {
            dm_llvm_buf_push(args[argi])
            argi = argi + 1
        }
        g = check_call_args(g, fn_ty, callee)
        dm_llvm_build_call2_buf(b, fn_ty, callee, "")
        -- Load the result from the sret alloca
        if inst.has_result {
            -- If the function returns string, keep the sret alloca as a pointer (string_ptrs convention)
            if ir_func.return_type == "string" {
                g.string_ptrs.insert(id_key, true)
                g.value_map.insert(id_key, sret)
            } else {
                let loaded = dm_llvm_build_load2(b, ret_llvm_ty, sret, "sret.load")
                g.value_map.insert(id_key, loaded)
            }
        }
        return g
    }
}

-- ============================================================
-- INDIRECT CALL (OP_CALL_PTR)
-- ============================================================
