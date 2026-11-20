module test

enum Shape {
    Circle(int),
    Square(int),
    Empty
}

fn area(s: Shape) -> int {
    match s {
        Shape::Circle(r) => { return r * r }
        Shape::Square(s) => { return s * s }
        _ => { return 0 }
    }
    return 0
}

fn main() {
    let c = Shape.Circle(5)
    let sq = Shape.Square(4)
    println(int_to_string(area(c)))
    println(int_to_string(area(sq)))
}