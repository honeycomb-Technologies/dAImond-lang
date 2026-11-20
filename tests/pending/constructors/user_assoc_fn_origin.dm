module test

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn origin() -> Point {
        return Point { x: 0, y: 0 }
    }
}

fn main() {
    let p = Point.origin()
    println(p.x + p.y)
}