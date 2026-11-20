module test

fn is_positive(n: int) -> bool {
    return n > 0
}

fn main() {
    println(is_positive(5))
    println(is_positive(-1))
}