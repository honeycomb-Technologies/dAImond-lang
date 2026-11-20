module test

struct Point {
    x: int,
    y: int,
}

fn move_right(mut p: Point, dx: int) {
    p.x = p.x + dx
}

fn main() {
    let mut pt = Point { x: 10, y: 20 }
    move_right(pt, 5)
    println(pt.x)
    println(pt.y)
}