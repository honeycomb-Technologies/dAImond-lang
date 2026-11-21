module test

struct Point { x: int, y: int }
struct Line { from: Point, to: Point }

const LIMIT: int = 10

fn bump(n: int) -> int {
    n = n + 1
    return n
}

fn main() -> int {
    let n = 3
    n = 4
    LIMIT = 11
    let p = Point { x: 1, y: 2 }
    p.x = 5
    let line = Line { from: p, to: p }
    line.to.y = 6
    let xs = [1, 2, 3]
    xs[0] = 7
    let mut q = Point { x: 1, y: 2 }
    q.x = 5
    let mut ys = [1, 2, 3]
    ys[0] = 7
    return bump(n) + q.x + ys[0]
}
