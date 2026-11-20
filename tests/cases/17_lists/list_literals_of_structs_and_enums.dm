module main

-- A list literal of structs or of enum values, held by a variable or
-- passed straight to a function

struct Point {
    x: int,
    y: int
}

enum Shape {
    Circle(int),
    Rect(int, int),
    Empty
}

fn area(s: Shape) -> int {
    match s {
        Shape.Circle(r) => { return 3 * r * r }
        Shape.Rect(w, h) => { return w * h }
        Shape.Empty => { return 0 }
    }
    return 0
}

fn total(shapes: List[Shape]) -> int {
    let mut sum = 0
    for s in shapes {
        sum = sum + area(s)
    }
    return sum
}

fn find(xs: List[int], wanted: int) -> Option[int] {
    let mut i = 0
    while i < xs.len() {
        if xs[i] == wanted {
            return Some(i)
        }
        i = i + 1
    }
    return None
}

fn width(pts: List[Point]) -> int {
    return pts[pts.len() - 1].x - pts[0].x
}

fn main() -> int {
    let shapes = [Shape.Circle(1), Shape.Rect(2, 3), Shape.Empty]
    for s in shapes {
        println(int_to_string(area(s)))
    }
    println(int_to_string(total(shapes)))
    println(int_to_string(total([Shape.Rect(4, 5), Shape.Circle(2)])))
    let pts = [Point { x: 1, y: 2 }, Point { x: 7, y: 4 }]
    println(int_to_string(pts[1].x + pts[0].y))
    for p in pts {
        println(int_to_string(p.x))
    }
    println(int_to_string(width(pts)))
    println(int_to_string(width([Point { x: 2, y: 0 }, Point { x: 12, y: 0 }])))
    match find([4, 5, 6], 6) {
        Some(at) => println("at " + int_to_string(at)),
        None => println("missing")
    }
    match find([4, 5, 6], 9) {
        Some(at) => println("at " + int_to_string(at)),
        None => println("missing")
    }
    let names = ["b", "a"]
    println(names[1] + int_to_string(names.len()))
    let typed: List[Point] = [Point { x: 5, y: 6 }, Point { x: 8, y: 9 }]
    println(int_to_string(typed[1].y + typed.len()))
    return 0
}
