module test

struct Point {
    x: int,
    y: int
}

fn sum_point(p: Point) -> int {
    return p.x + p.y
}

fn main() {
    let p = Point { x: 3, y: 7 }
    println(sum_point(p))
}