module error_simd_operands

fn main() {
    let a = simd_add(1, 2)
    let b = simd_extract(3.5, 0)
    let v = simd_splat_f32x4(1.0)
    let w = simd_splat_i32x4(1)
    let c = simd_mul(v, w)
    let d = simd_set_f32x4(1.0, 2.0)
    let e = simd_splat_i32x4(1.5)
    let f = simd_splat_f64x2("two")
    let g = simd_extract(v, 4)
    let h = simd_extract(w, 1.0)
    let i = simd_sub(v)
    let j: int = simd_extract(v, 0)
    let k: f32x4 = simd_add(w, w)
    println(float_to_string(b))
}
