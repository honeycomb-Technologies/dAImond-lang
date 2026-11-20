module test

fn max(a: int, b: int) -> int {
    if a > b {
        return a
    }
    return b
}

fn main() {
    println(max(3, 7))
    println(max(10, 2))
}