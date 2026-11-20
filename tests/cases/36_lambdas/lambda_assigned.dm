module test

fn main() {
    let f: fn(int) -> int = |x: int| -> int x + 10
    println(int_to_string(f(32)))
}