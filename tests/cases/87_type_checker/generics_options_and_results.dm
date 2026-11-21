module test

struct Pair[A, B] { first: A, second: B }
impl Pair[A, B] {
    fn swap(self) -> Pair[B, A] { return Pair[B, A] { first: self.second, second: self.first } }
    fn first_of(self) -> A { return self.first }
}
struct Stack[T] { items: List[T] }
impl Stack[T] {
    fn push(mut self, v: T) -> Stack[T] { self.items.push(v) return self }
    fn top(self) -> T { return self.items[self.items.len() - 1] }
    fn size(self) -> int { return self.items.len() }
}
fn id[T](x: T) -> T { return x }
fn first[T](xs: List[T]) -> T { return xs[0] }
fn wrap[T](x: T) -> Option[T] { return Some(x) }
fn safe_div(a: int, b: int) -> Result[int, string] {
    if b == 0 { return Err("div by zero") }
    return Ok(a / b)
}
fn twice(a: int, b: int) -> Result[int, string] {
    let q = safe_div(a, b)?
    return Ok(q * 2)
}
fn find(xs: List[string], s: string) -> Option[int] {
    let mut i = 0
    while i < xs.len() {
        if xs[i] == s { return Some(i) }
        i = i + 1
    }
    return None
}
fn main() -> int {
    let p = Pair[int, string] { first: 1, second: "one" }
    let q = p.swap()
    println(q.first + int_to_string(q.second + p.first_of()))
    let mut s = Stack[string] { items: [] }
    s = s.push("a")
    s = s.push("b")
    println(s.top() + int_to_string(s.size()))
    let n: int = id(41) + 1
    let str: string = id("x") + "y"
    println(str + int_to_string(n))
    let names = ["a", "b", "c"]
    println(first(names) + int_to_string(first([7, 8])))
    match wrap(5) {
        Some(v) => println(v + 1),
        None => println("none")
    }
    match twice(9, 3) {
        Ok(v) => println(v + 1),
        Err(e) => println("err " + e)
    }
    match twice(9, 0) {
        Ok(v) => println(v),
        Err(e) => println("err " + e)
    }
    match find(names, "c") {
        Some(i) => println(i * 10),
        None => println("no")
    }
    let o = find(names, "zz")
    let r = twice(4, 2)
    if o.has_value == false and r.is_ok { println("fields") }
    return 0
}
