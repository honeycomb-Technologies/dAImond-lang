module test

fn fib(n: int) -> int {
    if n <= 1 { return n }
    return fib(n - 1) + fib(n - 2)
}

const FIB10 = comptime fib(10)

fn main() {
    println(int_to_string(FIB10))
}