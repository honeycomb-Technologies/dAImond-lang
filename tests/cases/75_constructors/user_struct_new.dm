module test

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn new(x: int, y: int) -> Point {
        return Point { x: x, y: y }
    }

    fn sum(self: &Self) -> int {
        return self.x + self.y
    }
}

fn main() {
    let p = Point.new(10, 20)
    println(p.sum())
    let q: Point = Point.new(1, 2)
    println(q.x + q.y)
}