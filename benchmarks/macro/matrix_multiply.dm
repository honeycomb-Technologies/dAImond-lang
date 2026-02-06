-- Macro Benchmark: Matrix Multiplication
-- Multiplies two 500x500 matrices.
-- Tests nested loops and floating-point arithmetic.

module matrix_multiply_bench

fn main() {
    let n = 500

    -- Allocate matrices as flat lists
    let total = n * n
    let mut a: List[float] = []
    let mut b: List[float] = []
    let mut c: List[float] = []

    -- Initialize A and B
    let mut i = 0
    while i < total {
        a.push((i % n) as float * 0.01)
        b.push((i / n) as float * 0.01)
        c.push(0.0)
        i = i + 1
    }

    -- Matrix multiply: C = A * B
    i = 0
    while i < n {
        let mut j = 0
        while j < n {
            let mut sum = 0.0
            let mut k = 0
            while k < n {
                sum = sum + a[i * n + k] * b[k * n + j]
                k = k + 1
            }
            c[i * n + j] = sum
            j = j + 1
        }
        i = i + 1
    }

    -- Print a checksum value
    let mut checksum = 0.0
    i = 0
    while i < total {
        checksum = checksum + c[i]
        i = i + 1
    }
    println("checksum = " + float_to_string(checksum))
}
