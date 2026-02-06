-- Micro Benchmark: Recursive Fibonacci
-- Computes fib(40) using naive recursive approach.
-- Tests function call overhead and recursion performance.

module fibonacci_bench

fn fib(n: int) -> int {
    if n <= 1 {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

fn main() {
    let result = fib(40)
    println("fib(40) = " + int_to_string(result))
}
