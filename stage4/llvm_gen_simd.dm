module llvm_gen_simd

import ir
import llvm
import llvm_gen

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
fn get_simd_llvm_type(gen: LLVMGenerator, vec_type: string) -> int {
    let ctx = gen.context
    let elem_ty = get_simd_elem_type(gen, vec_type)
    let lanes = get_simd_lane_count(vec_type)
    return dm_llvm_vector_type(elem_ty, lanes)
}

-- Get the scalar element LLVM type for a vector type name
fn get_simd_elem_type(gen: LLVMGenerator, vec_type: string) -> int {
    let ctx = gen.context
    if vec_type == "f32x4" or vec_type == "f32x8" {
        return dm_llvm_float_type(ctx)
    }
    if vec_type == "f64x2" or vec_type == "f64x4" {
        return dm_llvm_double_type(ctx)
    }
    if vec_type == "i32x4" or vec_type == "i32x8" {
        return dm_llvm_int32_type(ctx)
    }
    if vec_type == "i64x2" or vec_type == "i64x4" {
        return dm_llvm_int64_type(ctx)
    }
    -- Fallback
    return dm_llvm_float_type(ctx)
}

-- Get the lane count for a SIMD type name
fn get_simd_lane_count(vec_type: string) -> int {
    if vec_type == "f32x4" { return 4 }
    if vec_type == "f32x8" { return 8 }
    if vec_type == "f64x2" { return 2 }
    if vec_type == "f64x4" { return 4 }
    if vec_type == "i32x4" { return 4 }
    if vec_type == "i32x8" { return 8 }
    if vec_type == "i64x2" { return 2 }
    if vec_type == "i64x4" { return 4 }
    return 4
}

-- Check if a SIMD type uses floating-point elements
fn is_simd_float_type(vec_type: string) -> bool {
    if vec_type == "f32x4" { return true }
    if vec_type == "f32x8" { return true }
    if vec_type == "f64x2" { return true }
    if vec_type == "f64x4" { return true }
    return false
}

-- ============================================================
-- SCALAR COERCION
-- ============================================================

-- Coerce a scalar value to match the expected SIMD element type.
-- Handles int-to-float, float-to-int, float width, int width mismatches.
fn coerce_simd_scalar(gen: LLVMGenerator, val: int, vec_type: string) -> int {
    let b = gen.builder
    let ctx = gen.context
    let val_ty_kind = dm_llvm_get_type_kind(dm_llvm_type_of(val))
    -- Type kind constants from LLVM:
    -- 10 = LLVMIntegerTypeKind, 2 = LLVMFloatTypeKind, 3 = LLVMDoubleTypeKind
    let is_int_val = val_ty_kind == 10
    let is_float_val = val_ty_kind == 2
    let is_double_val = val_ty_kind == 3
    if starts_with(vec_type, "f32") {
        -- Target: float
        if is_double_val {
            return dm_llvm_build_fp_cast(b, val, dm_llvm_float_type(ctx), "f64tof32")
        }
        if is_int_val {
            return dm_llvm_build_si_to_fp(b, val, dm_llvm_float_type(ctx), "itof32")
        }
        return val
    }
    if starts_with(vec_type, "f64") {
        -- Target: double
        if is_float_val {
            return dm_llvm_build_fp_cast(b, val, dm_llvm_double_type(ctx), "f32tof64")
        }
        if is_int_val {
            return dm_llvm_build_si_to_fp(b, val, dm_llvm_double_type(ctx), "itof64")
        }
        return val
    }
    if starts_with(vec_type, "i32") {
        -- Target: i32
        if is_int_val {
            let width = dm_llvm_get_int_type_width(dm_llvm_type_of(val))
            if width != 32 {
                return dm_llvm_build_int_cast2(b, val, dm_llvm_int32_type(ctx), 1, "toi32")
            }
        }
        if is_float_val or is_double_val {
            return dm_llvm_build_fp_to_si(b, val, dm_llvm_int32_type(ctx), "ftoi32")
        }
        return val
    }
    if starts_with(vec_type, "i64") {
        -- Target: i64
        if is_int_val {
            let width = dm_llvm_get_int_type_width(dm_llvm_type_of(val))
            if width != 64 {
                return dm_llvm_build_int_cast2(b, val, dm_llvm_int64_type(ctx), 1, "toi64")
            }
        }
        if is_float_val or is_double_val {
            return dm_llvm_build_fp_to_si(b, val, dm_llvm_int64_type(ctx), "ftoi64")
        }
        return val
    }
    return val
}

-- ============================================================
-- SIMD INSTRUCTION DISPATCHER
-- ============================================================

fn generate_simd_instruction(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let op = inst.op
    if op == OP_SIMD_SPLAT() {
        return lg_generate_simd_splat(gen, inst)
    }
    if op == OP_SIMD_SET() {
        return lg_generate_simd_set(gen, inst)
    }
    if op == OP_SIMD_ADD() {
        return generate_simd_binop(gen, inst, "add")
    }
    if op == OP_SIMD_SUB() {
        return generate_simd_binop(gen, inst, "sub")
    }
    if op == OP_SIMD_MUL() {
        return generate_simd_binop(gen, inst, "mul")
    }
    if op == OP_SIMD_DIV() {
        return generate_simd_binop(gen, inst, "div")
    }
    if op == OP_SIMD_EXTRACT() {
        return lg_generate_simd_extract(gen, inst)
    }
    return gen
}

-- ============================================================
-- SIMD SPLAT: create vector with all lanes set to scalar
-- ============================================================

fn lg_generate_simd_splat(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let ctx = g.context
    let id_key = int_to_string(inst.id)
    let vec_type = inst.simd_vec_type
    let llvm_vec_type = get_simd_llvm_type(g, vec_type)
    let lanes = get_simd_lane_count(vec_type)
    -- Resolve scalar value and coerce to element type
    let mut scalar = resolve_value(g, inst.simd_scalar)
    g = resolve_value_with_counter(g, inst.simd_scalar)
    scalar = coerce_simd_scalar(g, scalar, vec_type)
    -- Build vector by inserting scalar into each lane
    let mut vec = dm_llvm_get_undef(llvm_vec_type)
    let i32_ty = dm_llvm_int32_type(ctx)
    let mut i = 0
    while i < lanes {
        let idx = dm_llvm_const_int(i32_ty, i, 0)
        vec = dm_llvm_build_insert_element(b, vec, scalar, idx, "splat")
        i = i + 1
    }
    if inst.has_result { g.value_map.insert(id_key, vec) }
    return g
}

-- ============================================================
-- SIMD SET: create vector from individual element values
-- ============================================================

fn lg_generate_simd_set(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let ctx = g.context
    let id_key = int_to_string(inst.id)
    let vec_type = inst.simd_vec_type
    let llvm_vec_type = get_simd_llvm_type(g, vec_type)
    -- Start with undef vector
    let mut vec = dm_llvm_get_undef(llvm_vec_type)
    let i32_ty = dm_llvm_int32_type(ctx)
    let mut i = 0
    while i < inst.simd_elements.len() {
        let mut val = resolve_value(g, inst.simd_elements[i])
        g = resolve_value_with_counter(g, inst.simd_elements[i])
        val = coerce_simd_scalar(g, val, vec_type)
        let idx = dm_llvm_const_int(i32_ty, i, 0)
        vec = dm_llvm_build_insert_element(b, vec, val, idx, "set")
        i = i + 1
    }
    if inst.has_result { g.value_map.insert(id_key, vec) }
    return g
}

-- ============================================================
-- SIMD BINARY OP: element-wise add/sub/mul/div
-- ============================================================

fn generate_simd_binop(gen: LLVMGenerator, inst: IRInst, op_kind: string) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let lhs = resolve_value(g, inst.lhs)
    let rhs = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    let is_float = is_simd_float_type(inst.simd_vec_type)
    let mut result = 0
    if op_kind == "add" {
        if is_float {
            result = dm_llvm_build_fadd(b, lhs, rhs, "vadd")
        } else {
            result = dm_llvm_build_add(b, lhs, rhs, "vadd")
        }
    } else if op_kind == "sub" {
        if is_float {
            result = dm_llvm_build_fsub(b, lhs, rhs, "vsub")
        } else {
            result = dm_llvm_build_sub(b, lhs, rhs, "vsub")
        }
    } else if op_kind == "mul" {
        if is_float {
            result = dm_llvm_build_fmul(b, lhs, rhs, "vmul")
        } else {
            result = dm_llvm_build_mul(b, lhs, rhs, "vmul")
        }
    } else if op_kind == "div" {
        if is_float {
            result = dm_llvm_build_fdiv(b, lhs, rhs, "vdiv")
        } else {
            result = dm_llvm_build_sdiv(b, lhs, rhs, "vdiv")
        }
    }
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- SIMD EXTRACT: extract scalar element from vector
-- ============================================================

fn lg_generate_simd_extract(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let ctx = g.context
    let id_key = int_to_string(inst.id)
    let vec = resolve_value(g, inst.simd_vector)
    let mut idx = resolve_value(g, inst.simd_index)
    g = resolve_value_with_counter(g, inst.simd_vector)
    g = resolve_value_with_counter(g, inst.simd_index)
    -- LLVM extractelement needs an i32 index
    let idx_ty_kind = dm_llvm_get_type_kind(dm_llvm_type_of(idx))
    if idx_ty_kind == 10 {
        -- Integer type: check width
        let idx_width = dm_llvm_get_int_type_width(dm_llvm_type_of(idx))
        if idx_width != 32 {
            idx = dm_llvm_build_int_cast2(b, idx, dm_llvm_int32_type(ctx), 1, "idx.cast")
        }
    }
    let result = dm_llvm_build_extract_element(b, vec, idx, "extract")
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}
