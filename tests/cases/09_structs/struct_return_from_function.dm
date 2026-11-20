module test

struct Point {
    x: int,
    y: int
}

fn make_point(x: int, y: int) -> Point {
    return Point { x: x, y: y }
}

fn main() {
    let p = make_point(42, 99)
    println(p.x)
    println(p.y)
}