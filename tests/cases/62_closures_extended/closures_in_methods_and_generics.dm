module closures_in_methods_and_generics

-- Closures made by a method (which have `self`), by a generic function
-- and around a box; results that are structs, Options and Results;
-- narrow parameters; `==`; a lambda with no types written; a thread.
struct P { x: int, name: string }
struct Acc { total: int }
impl Acc {
    fn adder(self) -> fn(int) -> int {
        return |n: int| -> int self.total + n
    }
    fn bump(mut self, by: int) -> fn() -> int {
        self.total = self.total + by
        return || -> int self.total * 2
    }
}
fn wrap[T](v: T) -> fn() -> T {
    return || -> T v
}
fn each[T](xs: List[T], f: fn(T)) {
    for x in xs { f(x) }
}
fn mk_p(n: int) -> fn(string) -> P {
    return |s: string| -> P P { x: n, name: s }
}
fn opt(limit: int) -> fn(int) -> Option[string] {
    return |n: int| -> Option[string] {
        if n > limit { return Some("big " + int_to_string(n)) }
        return None
    }
}
fn res(d: int) -> fn(int) -> Result[int, string] {
    return |n: int| -> Result[int, string] {
        if d == 0 { return Err("zero") }
        return Ok(n / d)
    }
}
fn counter(start: int) -> fn() -> int {
    let cell = Box_new(start)
    return || -> int {
        *cell = *cell + 1
        return *cell
    }
}
fn fl(k: float) -> fn(float, f32, i32, bool) -> float {
    return |a: float, b: f32, c: i32, d: bool| -> float if d { a * k + (b as float) + (c as float) } else { 0.0 }
}
fn main() {
    let mut a = Acc { total: 10 }
    println(int_to_string(a.adder()(5)))
    let twice = a.bump(1)
    println(int_to_string(twice()) + " " + int_to_string(a.total))
    println(wrap("str")() + " " + int_to_string(wrap(41)() + 1))
    let tag = ">"
    each([1, 2], |n: int| { println(tag + int_to_string(n)) })
    each(["a"], |s: string| { println(tag + s) })
    let p = mk_p(7)("seven")
    println(p.name + int_to_string(p.x))
    match opt(3)(5) { Some(s) => println(s), None => println("none") }
    match opt(3)(1) { Some(s) => println(s), None => println("none") }
    match res(2)(10) { Ok(v) => println(int_to_string(v)), Err(e) => println(e) }
    match res(0)(10) { Ok(v) => println(int_to_string(v)), Err(e) => println(e) }
    let next = counter(10)
    next()
    println(int_to_string(next()) + " " + int_to_string(next()))
    println(float_to_string(fl(2.0)(1.5, 0.5 as f32, 3 as i32, true)))
    let same = next
    println(bool_to_string(same == next) + " " + bool_to_string(counter(1) == next))
    let untyped = |x| x + 1
    println(int_to_string(untyped(1)))
    let t = thread_spawn(|| { println("thread " + tag + int_to_string(next())) })
    thread_join(t)
}
