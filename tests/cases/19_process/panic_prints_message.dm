module test

fn check(n: int) {
    if n > 2 {
        panic("value too big: " + int_to_string(n))
    }
    println("ok " + int_to_string(n))
}

fn main() {
    check(1)
    check(7)
    println("not reached")
}