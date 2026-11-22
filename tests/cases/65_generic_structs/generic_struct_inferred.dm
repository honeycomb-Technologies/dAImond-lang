module test
struct Pair[A, B] { first: A, second: B }
struct Stack[T] { items: List[T] }
impl Pair[A, B] {
    fn new(first: A, second: B) -> Pair[A, B] {
        return Pair { first, second }
    }
    fn swap(self) -> Pair[B, A] {
        return Pair { first: self.second, second: self.first }
    }
}
impl Stack[T] {
    fn new() -> Stack[T] { return Stack { items: [] } }
    fn of(x: T) -> Stack[T] {
        let mut s: Stack[T] = Stack.new()
        s.push(x)
        return s
    }
    fn push(mut self, x: T) { self.items.push(x) }
    fn top(self) -> T { return self.items[self.items.len() - 1] }
    fn size(self) -> int { return self.items.len() }
}
enum Opt[T] { Has(T), Empty }
impl Opt[T] {
    fn none() -> Opt[T] { return Opt.Empty }
    fn get_or(self, d: T) -> T {
        match self { Opt.Has(v) => { return v }, Opt.Empty => { return d } }
    }
}
struct Holder { names: Stack[string], pair: Pair[int, Option[string]] }
fn make() -> Stack[float] { return Stack.new() }
fn total(s: Stack[int]) -> int {
    let mut t = 0
    for v in s.items { t = t + v }
    return t
}
fn show(p: Pair[string, Option[int]]) -> string {
    match p.second { Some(v) => { return p.first + int_to_string(v) }, None => { return p.first + "-" } }
}
fn sizes(a: Stack[int], b: Stack[string]) -> int { return a.size() + b.size() }
fn pick(c: bool) -> Stack[float] {
    if c { return Stack.new() }
    let s = Stack { items: [1.5] }
    return s
}
-- In a generic function the type parameters are those of the instance
fn wrap[T](x: T) -> Stack[T] { return Stack.of(x) }
fn wrap_written[T](x: T) -> Stack[T] {
    let s = Stack[T].of(x)
    return s
}
fn tops[T](xs: List[T]) -> List[T] {
    let mut out: List[T] = []
    for x in xs {
        let s = Stack { items: [x] }
        out.push(s.top())
    }
    return out
}

fn main() {
    -- From the arguments of the type's function
    let pair = Pair.new(1, "one")
    let swapped = pair.swap()
    println(swapped.first + " " + int_to_string(swapped.second))
    let t = Stack.of("solo")
    println(t.top() + " " + int_to_string(t.size()))
    println(int_to_string(total(Stack.of(9))))
    let nested = Stack.of(Stack.of(7))
    println(int_to_string(nested.top().top()))

    -- From the fields of the literal
    let p = Pair { first: 2.5, second: [1, 2, 3] }
    println(float_to_string(p.first) + " " + int_to_string(p.second.len()))
    let ps = [Pair { first: 1, second: "x" }, Pair { first: 2, second: "y" }]
    println(ps[1].second)
    let x = if ps.len() > 1 { Pair { first: "big", second: 1 } } else { Pair { first: "small", second: 0 } }
    println(x.first)

    -- From the place the value goes to
    let q: Pair[i32, Option[string]] = Pair { first: 7, second: None }
    let n: i32 = q.first
    println(int_to_string(n as int))
    let mut s: Stack[string] = Stack.new()
    s.push("a")
    println(s.top())
    let mut f = make()
    f.push(1.5)
    println(float_to_string(f.top()))
    let h = Holder { names: Stack.new(), pair: Pair { first: 1, second: Some("x") } }
    println(int_to_string(h.names.size()) + " " + int_to_string(h.pair.first))
    println(show(Pair { first: "a", second: None }))
    println(show(Pair { first: "b", second: Some(3) }))
    println(int_to_string(sizes(Stack.new(), Stack { items: ["x"] })))
    println(int_to_string(pick(true).size()) + int_to_string(pick(false).size()))
    let mut r: Stack[int] = Stack { items: [] }
    r = Stack.new()
    r = Stack { items: [1, 2] }
    println(int_to_string(r.size()))
    let o: Option[Stack[string]] = Some(Stack.new())
    match o { Some(st) => println(int_to_string(st.size())), None => println("none") }
    let res: Result[Pair[int, string], string] = Ok(Pair { first: 1, second: "r" })
    match res { Ok(pp) => println(pp.second), Err(m) => println(m) }
    let mut stacks: List[Stack[int]] = []
    stacks.push(Stack.new())
    stacks.push(Stack.of(8))
    println(int_to_string(stacks.len()) + " " + int_to_string(stacks[1].top()))

    -- Written: Type[args].function(...) and Enum[args].Variant
    let mut u = Stack[int].new()
    u.push(4)
    u.push(5)
    println(int_to_string(total(u)))
    let e: Opt[int] = Opt.none()
    println(int_to_string(e.get_or(42)))
    let e2 = Opt[string].none()
    println(e2.get_or("dflt"))
    let e3 = Opt[int].Has(3)
    println(int_to_string(e3.get_or(0)))
    let e4 = Opt[string].Empty
    println(e4.get_or("four"))

    -- An index is no type argument: a name written as a variable's is one
    let grid: List[string] = ["ab", "c"]
    let i = 1
    println(int_to_string(grid[i].len()))
    let Rows: List[string] = ["abc", "de"]
    println(int_to_string(Rows[i].len()))

    println(wrap("w").top())
    println(int_to_string(wrap_written(5).top()))
    println(float_to_string(tops([1.5, 2.5])[1]))
}
