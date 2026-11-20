module test

fn main() {
    let a: f64x2 = simd_set_f64x2(3.14, 2.71)
    let b: f64x2 = simd_splat_f64x2(2.0)
    let c: f64x2 = simd_div(a, b)
    println(float_to_string(simd_extract(c, 0)))
    println(float_to_string(simd_extract(c, 1)))
}