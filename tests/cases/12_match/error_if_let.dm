module test
enum Shape { Circle(int), Empty }
fn f(o: Option[int], s: Shape) -> int {
    let v = if let Some(x) = o { x }
    if let Some(y) = 5 { println("five") }
    if let Shape.Square(w) = s { println("square") }
    if let Some(z) = o { println(q) }
    if let Ok(z) = o { println("ok") }
    return v
}
fn main() {
    println(int_to_string(f(None, Shape.Empty)))
}
