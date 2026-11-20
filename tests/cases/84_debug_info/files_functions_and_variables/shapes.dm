module shapes

struct Point {
    x: int,
    y: int
}

-- The area of the rectangle from the origin to p

fn area(p: Point) -> int {
    let width = p.x
    let height = p.y
    return width * height
}
