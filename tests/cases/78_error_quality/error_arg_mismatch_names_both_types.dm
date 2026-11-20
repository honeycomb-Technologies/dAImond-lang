module test

fn take_int(n: int) -> int { return n }

fn main() {
    let y = take_int("hello")
    println(y)
}