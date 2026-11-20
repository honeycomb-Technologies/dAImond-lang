module test

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() -> int {
    let scale = 10
    println(int_to_string(apply(|x: int| -> int x * scale, 4)))
    return 0
}
