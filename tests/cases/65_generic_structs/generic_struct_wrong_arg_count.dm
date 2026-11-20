module test
struct Pair[A, B] { a: A, b: B }
fn main() {
    let p = Pair[int] { a: 1, b: 2 }
    println(int_to_string(p.a))
}