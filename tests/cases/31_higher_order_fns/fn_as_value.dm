module test

fn triple(x: int) -> int {
    return x * 3
}

fn main() {
    let f: fn(int) -> int = triple
    println(int_to_string(f(10)))
}