module test

fn amount() -> int {
    return 64
}

fn main() {
    let x = 1
    println(x << 63 >> 63)
    println(x << amount())
    println("not reached")
}
