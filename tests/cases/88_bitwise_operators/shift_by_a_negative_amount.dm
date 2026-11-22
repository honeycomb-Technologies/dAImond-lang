module test

fn amount() -> int {
    return -1
}

fn main() {
    let x: u8 = 255
    println(x >> 7)
    println(x >> amount())
    println("not reached")
}
