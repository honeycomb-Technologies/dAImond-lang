module test

fn double(x: int) -> int {
    return x * 2
}

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() {
    println(int_to_string(apply(double, 21)))
}