module test

fn main() {
    let a: i32x4 = simd_set_i32x4(10, 20, 30, 40)
    let b: i32x4 = simd_splat_i32x4(2)
    let c: i32x4 = simd_mul(a, b)
    let d: i32x4 = simd_sub(a, b)
    println(int_to_string(simd_extract(c, 0)))
    println(int_to_string(simd_extract(c, 1)))
    println(int_to_string(simd_extract(c, 2)))
    println(int_to_string(simd_extract(c, 3)))
    println(int_to_string(simd_extract(d, 0)))
    println(int_to_string(simd_extract(d, 3)))
}