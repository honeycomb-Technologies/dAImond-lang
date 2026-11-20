module test

fn factorial(n: int) -> int {
    if n <= 1 { return 1 }
    return n * factorial(n - 1)
}

const F10 = comptime factorial(10)

fn main() {
    println(int_to_string(F10))
}