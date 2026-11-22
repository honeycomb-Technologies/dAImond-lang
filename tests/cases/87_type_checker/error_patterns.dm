module test
enum Shape { Circle(int), Empty }
enum Color { Red, Green }
fn f(o: Option[int], s: Shape, r: Result[int, string]) -> int {
    match 5 { Some(y) => { println("five") }, _ => {} }
    match o { Ok(z) => { println("ok") }, _ => {} }
    match r { Some(z) => { println("ok") }, _ => {} }
    match s { Color.Red => { println("red") }, _ => {} }
    match s { Some(z) => { println("red") }, _ => {} }
    match o { Shape.Empty => { println("red") }, _ => {} }
    match "text" { 1 => { println("one") }, _ => {} }
    match 4 { "four" => { println("one") }, _ => {} }
    match o { 1 => { println("one") }, _ => {} }
    match s { true => { println("one") }, _ => {} }
    return 0
}
fn main() {
    println(int_to_string(f(None, Shape.Empty, Ok(1))))
}
