module vector_add

-- SIMD vector addition benchmark
-- Adds two large arrays element-wise using SIMD i32x4

fn main() {
    let iterations = 1000
    let size = 250000

    let mut checksum = 0
    let mut i = 0
    while i < iterations {
        let mut sum: i32x4 = simd_splat_i32x4(0)
        let mut j = 0
        while j < size {
            let a: i32x4 = simd_set_i32x4(j, j + 1, j + 2, j + 3)
            let b: i32x4 = simd_splat_i32x4(1)
            let c: i32x4 = simd_add(a, b)
            sum = simd_add(sum, c)
            j = j + 4
        }
        checksum = checksum + simd_extract(sum, 0) + simd_extract(sum, 3)
        i = i + 1
    }

    println(int_to_string(checksum))
}
