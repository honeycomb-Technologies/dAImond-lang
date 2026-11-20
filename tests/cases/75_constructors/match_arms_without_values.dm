module test

enum Shape {
    Circle(int),
    Square(int),
}

fn main() {
    -- Arms that are calls with no value
    let n = 2
    match n {
        1 => println("one"),
        _ => println("other"),
    }
    let s = Shape.Square(4)
    match s {
        Shape::Circle(r) => println(r),
        Shape::Square(side) => println(side * side),
    }
    -- A variable named like a type is a value: its method is called
    let Shape = "text"
    println(Shape.len())
}
