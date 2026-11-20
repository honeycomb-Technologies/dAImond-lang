module test

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn new(x: int, y: int) -> Point {
        return Point { x: x, y: y }
    }
}

fn main() {
    let p = Point.new(1)
    println(p.x)
}