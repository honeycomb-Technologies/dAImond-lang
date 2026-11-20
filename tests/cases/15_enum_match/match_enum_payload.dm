module test

enum Shape {
    Circle(int),
    Square(int),
    Empty
}

fn area(s: Shape) -> int {
    match s {
        Shape::Circle(r) => { return r * r * 3 }
        Shape::Square(side) => { return side * side }
        _ => { return 0 }
    }
    return 0
}

fn main() {
    let c = Shape::Circle(5)
    let sq = Shape::Square(4)
    let e = Shape::Empty
    println(area(c))
    println(area(sq))
    println(area(e))
}