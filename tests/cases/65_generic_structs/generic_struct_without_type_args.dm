module test
struct Box2[T] { v: T }
fn main() {
    let b = Box2 { v: 1 }
    println(int_to_string(b.v))
}