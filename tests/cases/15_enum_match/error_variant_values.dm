module main
enum Shape {
    Circle(int),
    Rect(int, int),
    Empty
}
fn main() -> int {
    let a = Shape.Rect(1)
    let b = Shape.Circle("wide")
    let c = Shape.Empty
    match c {
        Shape.Circle(r, extra) => println("c"),
        Shape.Square(w) => println("s"),
        _ => println("other")
    }
    return 0
}
