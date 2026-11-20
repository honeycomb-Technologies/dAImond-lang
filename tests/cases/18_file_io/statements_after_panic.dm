module test

fn check(x: int) -> int {
    if x > 5 {
        panic("too big")
        return 0
    }
    return x * 2
}

fn main() {
    println(int_to_string(check(3)))
}