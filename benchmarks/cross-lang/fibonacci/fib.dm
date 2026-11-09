module fib_bench

-- Naive recursive fibonacci
-- Computes fib(40) to test function call overhead

fn fib(n: int) -> int {
    if n <= 1 { return n }
    return fib(n - 1) + fib(n - 2)
}

fn main() {
    let result = fib(40)
    println(int_to_string(result))
}
