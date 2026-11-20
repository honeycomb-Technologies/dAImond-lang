module test

fn main() {
    println(a())
}

fn a() -> int {
    return b() + 1
}

fn b() -> int {
    return 10
}