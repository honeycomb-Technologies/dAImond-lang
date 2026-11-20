module test

enum Shape {
    Circle(int),
    Square(int),
}

impl Shape {
    fn new(r: int) -> Shape {
        return Shape.Circle(r)
    }
}

fn main() {
    let s = Shape.new(5)
    match s {
        Shape::Circle(r) => println(r),
        _ => println("other"),
    }
}