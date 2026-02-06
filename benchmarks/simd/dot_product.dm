module dot_product

-- SIMD dot product benchmark
-- Computes dot product of two large vectors using SIMD f32x4

fn main() {
    let size = 1000000
    let iterations = 100

    -- Accumulate dot product using SIMD
    let mut total = 0.0
    let mut i = 0
    while i < iterations {
        let mut sum: f32x4 = simd_splat_f32x4(0.0)
        let mut j = 0
        while j < size {
            -- Simulate two vectors with predictable values
            let a: f32x4 = simd_set_f32x4(1.0, 2.0, 3.0, 4.0)
            let b: f32x4 = simd_set_f32x4(4.0, 3.0, 2.0, 1.0)
            let prod: f32x4 = simd_mul(a, b)
            sum = simd_add(sum, prod)
            j = j + 4
        }
        -- Horizontal sum
        let s0 = simd_extract(sum, 0)
        let s1 = simd_extract(sum, 1)
        let s2 = simd_extract(sum, 2)
        let s3 = simd_extract(sum, 3)
        total = total + s0 + s1 + s2 + s3
        i = i + 1
    }

    println(float_to_string(total))
}
