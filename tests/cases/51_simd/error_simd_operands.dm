module error_simd_operands

fn main() {
    let a = simd_add(1, 2)
    let b = simd_extract(3.5, 0)
    println(float_to_string(b))
}
