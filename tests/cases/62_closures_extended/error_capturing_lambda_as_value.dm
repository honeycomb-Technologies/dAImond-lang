module test

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() -> int {
    let scale = 10
    let scaled = |x: int| -> int x * scale
    println(int_to_string(scaled(4)))
    println(int_to_string(apply(scaled, 4)))
    return 0
}
