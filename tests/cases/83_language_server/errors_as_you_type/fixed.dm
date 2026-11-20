module main

fn double(n: int) -> int {
    return n * 2
}

fn main() -> int {
    let total: int = 10
    println(int_to_string(double(total)))
    return 0
}
