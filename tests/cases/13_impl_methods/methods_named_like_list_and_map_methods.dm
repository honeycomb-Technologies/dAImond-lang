module methods_named_like_list_and_map_methods

-- A struct's own method is called and typed by the struct it is
-- called on, whatever its name: one named like a method of a list or
-- a map (get, len, contains, pop, push, insert, keys), and one that
-- another struct has under the same name
struct A { n: int }
impl A {
    fn val(self) -> int { return self.n }
    fn get(self, k: int) -> string { return "a" + int_to_string(k) }
    fn len(self) -> string { return "long" }
    fn contains(self, k: int) -> float { return 1.5 }
    fn pop(self) -> string { return "popped" }
    fn push(mut self, k: int) { self.n = self.n + k }
    fn insert(mut self, k: int, v: int) -> string { return "ins" }
    fn keys(self) -> int { return 7 }
}
struct B { s: string }
impl B {
    fn val(self) -> string { return self.s }
    fn get(self) -> float { return 2.5 }
}
struct H { a: A, b: B }
fn mk() -> A { return A { n: 9 } }
fn main() -> int {
    let mut a = A { n: 1 }
    let b = B { s: "bee" }
    let h = H { a: A { n: 4 }, b: B { s: "hb" } }
    println(a.val())
    println(b.val())
    println(h.a.val())
    println(h.b.val())
    println(mk().val())
    println(a.get(3))
    println(b.get())
    println(a.len())
    println(a.contains(1))
    println(a.pop())
    a.push(5)
    println(a.n)
    println(a.insert(1, 2))
    println(a.keys())
    let x = a.get(1)
    println(x + "!")
    let y = b.get()
    println(y + 1.0)
    let bb = Box_new(B { s: "boxed" })
    println(bb.val())
    println(bb.get())
    return 0
}
