module main

-- A variant carries any number of values of any types, each in its own
-- field: the second value of one variant is not the first of the next

struct Point {
    x: int,
    y: int
}

enum Shape {
    Circle(float),
    Rect(float, float),
    Label(string, Point, int),
    Line(Point, Point),
    Empty
}

fn area(s: Shape) -> float {
    match s {
        Shape.Circle(r) => { return 3.0 * r * r }
        Shape.Rect(w, h) => { return w * h }
        Shape.Empty => { return 0.0 }
        _ => { return 1.0 }
    }
    return 0.0
}

fn name(s: Shape) -> string {
    match s {
        Shape.Label(text, at, size) => { return text + "@" + int_to_string(at.x + at.y) + "#" + int_to_string(size) }
        Shape.Line(from, to) => { return "line of " + int_to_string(to.x - from.x) }
        -- A pattern may bind fewer values than the variant carries
        Shape.Rect(w) => { return "rect " + float_to_string(w) }
        other => { return "shape " + float_to_string(area(other)) }
    }
    return "?"
}

fn main() -> int {
    println(float_to_string(area(Shape.Circle(1.0))))
    println(float_to_string(area(Shape.Rect(2.0, 3.5))))
    println(name(Shape.Label("here", Point { x: 1, y: 2 }, 12)))
    println(name(Shape.Line(Point { x: 1, y: 0 }, Point { x: 9, y: 0 })))
    println(name(Shape.Rect(1.5, 2.0)))
    println(name(Shape.Circle(2.0)))
    println(name(Shape.Empty))
    let both = Shape.Rect(4.0, 0.25)
    match both {
        Shape.Rect(w, h) => println(float_to_string(w) + " by " + float_to_string(h)),
        _ => println("not a rect")
    }
    return 0
}
