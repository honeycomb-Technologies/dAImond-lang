module test
-- What the values and the written type arguments of a generic struct
-- say must agree with each other and with the place the struct goes to
struct Pair[A, B] { first: A, second: B }
impl Pair[A, B] {
    fn new(first: A, second: B) -> Pair[A, B] { return Pair { first, second } }
}
enum Opt[T] { Has(T), Empty }
fn main() {
    let q = Pair[int].new(1, 2)
    let r: Pair[int, string] = Pair.new(1, 2)
    let t: Pair[string, string] = Pair { first: 1, second: "b" }
    let u = Pair[int, int].new("a", 2)
    let a = Opt[int].Has("x")
    let b = Opt[int, int].Empty
    let Rows: List[string] = ["a"]
    let Idx = 0
    println(int_to_string(Rows[Idx].len()))
}
