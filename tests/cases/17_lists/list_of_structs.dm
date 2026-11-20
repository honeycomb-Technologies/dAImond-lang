module test

struct Point {
    x: int,
    y: int
}

fn main() {
    let mut points: List[Point] = List_new()
    points.push(Point { x: 1, y: 2 })
    points.push(Point { x: 3, y: 4 })
    let p = points[0]
    println(p.x)
    println(p.y)
    let q = points[1]
    println(q.x)
    println(q.y)
}