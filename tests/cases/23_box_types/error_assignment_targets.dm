module test

fn one() -> int {
    return 1
}

fn main() {
    let b = Box_new(1)
    *b = "x"
    let n = 2
    *n = 3
    one() = 4
}
