module test

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() {
    let result = apply(|x: int| -> int x * 2, 21)
    println(int_to_string(result))
}