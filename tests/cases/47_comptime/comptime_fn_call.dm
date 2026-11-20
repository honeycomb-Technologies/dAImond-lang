module test

fn double(x: int) -> int {
    return x * 2
}

const X = comptime double(21)

fn main() {
    println(int_to_string(X))
}