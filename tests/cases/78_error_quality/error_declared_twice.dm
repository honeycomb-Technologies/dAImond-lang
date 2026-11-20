module test

fn area(width: int, height: int) -> int {
    return width * height
}

struct Size {
    width: int
}

fn area(side: int) -> int {
    return side * side
}

enum Size {
    Small,
    Large
}

const LIMIT = 1
const LIMIT = 2

extern fn getpid() -> i32
extern fn getpid() -> i32

fn main() -> int {
    return area(2)
}
