module test

struct Point {
    x: int,
    y: int,
}

fn main() {
    let b: Box[Point] = Box_new(Point { x: 10, y: 20 })
    println(b.x)
    println(b.y)
}