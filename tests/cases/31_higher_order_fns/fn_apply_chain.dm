module test

fn add_one(x: int) -> int {
    return x + 1
}

fn mul_two(x: int) -> int {
    return x * 2
}

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() {
    let a = apply(add_one, 5)
    let b = apply(mul_two, a)
    println(int_to_string(b))
}