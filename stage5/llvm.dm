module llvm

-- ============================================================
-- LLVM-C Bridge Extern Declarations
-- All LLVM refs are represented as int (int64_t on C side)
-- The dm_llvm_* functions are defined in llvm_bridge.dm
-- ============================================================

-- ============================================================
-- Context
-- ============================================================
extern fn dm_llvm_context_create() -> int
extern fn dm_llvm_context_dispose(ctx: int)
-- ============================================================
-- Module
-- ============================================================
extern fn dm_llvm_module_create(ctx: int, name: string) -> int
extern fn dm_llvm_module_dispose(mod: int)
extern fn dm_llvm_module_set_target(mod: int, triple: string)
extern fn dm_llvm_module_set_data_layout(mod: int, layout: string)
extern fn dm_llvm_module_add_function(mod: int, name: string, fn_type: int) -> int
extern fn dm_llvm_module_add_global(mod: int, ty: int, name: string) -> int
extern fn dm_llvm_module_verify(mod: int) -> int
extern fn dm_llvm_module_print_to_file(mod: int, filename: string) -> int
extern fn dm_llvm_run_passes(mod: int, passes: string, tm: int) -> int
extern fn dm_llvm_module_write_bitcode(mod: int, filename: string) -> int

-- ============================================================
-- Types
-- ============================================================
extern fn dm_llvm_int1_type(ctx: int) -> int
extern fn dm_llvm_int8_type(ctx: int) -> int
extern fn dm_llvm_int16_type(ctx: int) -> int
extern fn dm_llvm_int32_type(ctx: int) -> int
extern fn dm_llvm_int64_type(ctx: int) -> int
extern fn dm_llvm_float_type(ctx: int) -> int
extern fn dm_llvm_double_type(ctx: int) -> int
extern fn dm_llvm_void_type(ctx: int) -> int
extern fn dm_llvm_pointer_type(ctx: int) -> int
extern fn dm_llvm_array_type(elem: int, count: int) -> int
extern fn dm_llvm_vector_type(elem: int, count: int) -> int
extern fn dm_llvm_struct_create_named(ctx: int, name: string) -> int
extern fn dm_llvm_get_type_kind(ty: int) -> int
extern fn dm_llvm_get_int_type_width(ty: int) -> int

-- ============================================================
-- Values / Constants
-- ============================================================
extern fn dm_llvm_const_int(ty: int, val: int, sign_extend: int) -> int
extern fn dm_llvm_const_real(ty: int, val: float) -> int
extern fn dm_llvm_const_null(ty: int) -> int
extern fn dm_llvm_set_initializer(global: int, init: int)
extern fn dm_llvm_set_global_constant(global: int, is_const: int)
extern fn dm_llvm_set_linkage(global: int, linkage: int)
extern fn dm_llvm_get_param(func: int, index: int) -> int
extern fn dm_llvm_set_value_name(val: int, name: string)
extern fn dm_llvm_type_of(val: int) -> int
extern fn dm_llvm_get_undef(ty: int) -> int

-- ============================================================
-- Basic Blocks
-- ============================================================
extern fn dm_llvm_append_basic_block(ctx: int, func: int, name: string) -> int

-- ============================================================
-- Builder
-- ============================================================
extern fn dm_llvm_create_builder(ctx: int) -> int
extern fn dm_llvm_dispose_builder(builder: int)
extern fn dm_llvm_position_at_end(builder: int, block: int)
extern fn dm_llvm_get_insert_block(builder: int) -> int

-- Arithmetic
extern fn dm_llvm_build_add(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_sub(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_mul(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_sdiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_srem(b: int, lhs: int, rhs: int, name: string) -> int
-- libc: the bytes of a string up to its first NUL, which is what a
-- string passed to LLVM as a C string holds
extern fn strlen(s: string) -> u64
extern fn dm_llvm_build_udiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_urem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_neg(b: int, val: int, name: string) -> int
extern fn dm_llvm_build_fadd(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fsub(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fmul(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fdiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_frem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fneg(b: int, val: int, name: string) -> int

-- Comparison
extern fn dm_llvm_build_icmp(b: int, pred: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fcmp(b: int, pred: int, lhs: int, rhs: int, name: string) -> int

-- Memory
extern fn dm_llvm_build_alloca(b: int, ty: int, name: string) -> int
extern fn dm_llvm_build_load2(b: int, ty: int, ptr: int, name: string) -> int
extern fn dm_llvm_build_store(b: int, val: int, ptr: int) -> int
extern fn dm_llvm_build_struct_gep2(b: int, ty: int, ptr: int, idx: int, name: string) -> int
extern fn dm_llvm_build_gep2_one(b: int, ty: int, ptr: int, idx: int, name: string) -> int
extern fn dm_llvm_build_extract_value(b: int, agg: int, idx: int, name: string) -> int
extern fn dm_llvm_build_insert_value(b: int, agg: int, val: int, idx: int, name: string) -> int

-- Control flow
extern fn dm_llvm_build_br(b: int, dest: int) -> int
extern fn dm_llvm_build_cond_br(b: int, cond: int, then_bb: int, else_bb: int) -> int
extern fn dm_llvm_build_ret(b: int, val: int) -> int
extern fn dm_llvm_build_ret_void(b: int) -> int
extern fn dm_llvm_build_unreachable(b: int) -> int

-- Calls

-- Phi
extern fn dm_llvm_build_phi(b: int, ty: int, name: string) -> int

-- Casts
extern fn dm_llvm_build_int_cast2(b: int, val: int, dest_ty: int, is_signed: int, name: string) -> int
extern fn dm_llvm_build_fp_cast(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_si_to_fp(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_ui_to_fp(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_fp_to_si(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_trunc(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_sext(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_ptr_to_int(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_int_to_ptr(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_sizeof_type(ty: int) -> int

-- Logical
extern fn dm_llvm_build_and(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_or(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_not(b: int, val: int, name: string) -> int

-- SIMD / Vector
extern fn dm_llvm_build_insert_element(b: int, vec: int, elem: int, idx: int, name: string) -> int
extern fn dm_llvm_build_extract_element(b: int, vec: int, idx: int, name: string) -> int

-- Global strings
extern fn dm_llvm_build_global_string_ptr(b: int, str: string, name: string) -> int

-- ============================================================
-- Target
-- ============================================================
extern fn dm_llvm_initialize_all_targets()
extern fn dm_llvm_get_default_target_triple() -> int
extern fn dm_llvm_create_target_machine(triple: string, opt_level: int) -> int
extern fn dm_llvm_add_function_attr(func: int, name: string)
extern fn dm_llvm_dispose_target_machine(tm: int)
extern fn dm_llvm_target_machine_emit_to_file(tm: int, mod: int, filename: string, file_type: int) -> int
extern fn dm_llvm_get_target_data_layout(tm: int) -> int

-- ============================================================
-- Enum Constants
-- ============================================================
extern fn dm_llvm_int_eq() -> int
extern fn dm_llvm_int_ne() -> int
extern fn dm_llvm_int_slt() -> int
extern fn dm_llvm_int_sle() -> int
extern fn dm_llvm_int_sgt() -> int
extern fn dm_llvm_int_sge() -> int
extern fn dm_llvm_real_oeq() -> int
extern fn dm_llvm_real_one() -> int
extern fn dm_llvm_real_olt() -> int
extern fn dm_llvm_real_ole() -> int
extern fn dm_llvm_real_ogt() -> int
extern fn dm_llvm_real_oge() -> int
extern fn dm_llvm_external_linkage() -> int
extern fn dm_llvm_weak_linkage() -> int
extern fn dm_llvm_object_file() -> int
extern fn dm_llvm_assembly_file() -> int

-- ============================================================
-- String helpers
-- ============================================================
extern fn dm_llvm_c_str_to_string(c_str_ptr: int) -> string
extern fn dm_llvm_free_c_string(c_str_ptr: int)
-- Buffer-based API (for passing arrays to LLVM without raw pointers)
extern fn dm_llvm_buf_clear()
extern fn dm_llvm_buf_push(ref: int)
extern fn dm_llvm_buf_len() -> int
extern fn dm_llvm_buf_get(index: int) -> int
extern fn dm_llvm_count_param_types(fn_ty: int) -> int
extern fn dm_llvm_param_type_at(fn_ty: int, index: int) -> int
extern fn dm_llvm_is_function_var_arg(fn_ty: int) -> int
extern fn dm_llvm_print_type_to_string(ty: int) -> int
extern fn dm_llvm_get_value_name(val: int) -> string
extern fn dm_llvm_get_struct_name(ty: int) -> string
extern fn dm_llvm_struct_set_body_buf(struct_ty: int, is_packed: int)
extern fn dm_llvm_function_type_buf(ret: int, is_var_arg: int) -> int
extern fn dm_llvm_build_call2_buf(b: int, fn_ty: int, func: int, name: string) -> int
extern fn dm_llvm_add_incoming_one(phi: int, val: int, block: int)

-- Debug info (DWARF). A file, compile unit, function or place is a
-- reference to LLVM metadata.
extern fn dm_llvm_di_builder_create(mod: int) -> int
extern fn dm_llvm_di_builder_finalize(dib: int)
extern fn dm_llvm_di_file(dib: int, name: string, directory: string) -> int
extern fn dm_llvm_di_compile_unit(dib: int, file: int, producer: string, optimized: int) -> int
extern fn dm_llvm_di_function(dib: int, file: int, name: string, symbol: string, line: int, optimized: int) -> int
extern fn dm_llvm_set_subprogram(func: int, subprogram: int)
extern fn dm_llvm_set_debug_location(b: int, ctx: int, line: int, col: int, scope: int)
extern fn dm_llvm_di_basic_type(dib: int, name: string, bits: int, encoding: int) -> int
extern fn dm_llvm_di_pointer_type(dib: int, pointee: int, name: string) -> int
extern fn dm_llvm_di_member(dib: int, scope: int, name: string, bits: int, offset_bits: int, ty: int) -> int
extern fn dm_llvm_di_struct_begin(dib: int, name: string, bits: int) -> int
extern fn dm_llvm_di_struct_end_buf(dib: int, begun: int, name: string, bits: int) -> int
extern fn dm_llvm_di_variable(dib: int, scope: int, name: string, file: int, line: int, ty: int, arg: int) -> int
extern fn dm_llvm_di_declare(dib: int, b: int, storage: int, variable: int, ctx: int, line: int, scope: int, deref: int)
extern fn dm_llvm_is_alloca(val: int) -> int
extern fn dm_llvm_abi_size_of(mod: int, ty: int) -> int
extern fn dm_llvm_offset_of_element(mod: int, struct_ty: int, index: int) -> int
extern fn dm_llvm_struct_field_count(ty: int) -> int

-- ============================================================
-- Convenience wrapper functions (dAImond-friendly API)
-- ============================================================

fn llvm_create_module(ctx: int, name: string) -> int {
    unsafe {
        return dm_llvm_module_create(ctx, name)
    }
}

fn llvm_init_targets() {
    unsafe {
        dm_llvm_initialize_all_targets()
    }
}

fn llvm_get_triple() -> int {
    unsafe {
        return dm_llvm_get_default_target_triple()
    }
}

fn llvm_create_tm(triple: string, opt: int) -> int {
    unsafe {
        return dm_llvm_create_target_machine(triple, opt)
    }
}

fn llvm_emit_object(tm: int, mod: int, filename: string) -> int {
    unsafe {
        return dm_llvm_target_machine_emit_to_file(tm, mod, filename, dm_llvm_object_file())
    }
}

fn llvm_emit_asm(tm: int, mod: int, filename: string) -> int {
    unsafe {
        return dm_llvm_target_machine_emit_to_file(tm, mod, filename, dm_llvm_assembly_file())
    }
}

extern fn dm_llvm_struct_type_buf(ctx: int, count: int, is_packed: int) -> int
