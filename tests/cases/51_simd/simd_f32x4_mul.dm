module test

fn main() {
    let a: f32x4 = simd_set_f32x4(1.0, 2.0, 3.0, 4.0)
    let b: f32x4 = simd_set_f32x4(2.0, 3.0, 4.0, 5.0)
    let c: f32x4 = simd_mul(a, b)
    let v0 = simd_extract(c, 0)
    let v1 = simd_extract(c, 1)
    let v2 = simd_extract(c, 2)
    let v3 = simd_extract(c, 3)
    println(float_to_string(v0))
    println(float_to_string(v1))
    println(float_to_string(v2))
    println(float_to_string(v3))
}