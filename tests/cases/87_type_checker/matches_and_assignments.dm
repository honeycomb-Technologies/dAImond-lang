module test

enum Color { Red, Green, Blue(int) }
enum Opt { Has(int), Nope }
struct Counter { n: int, items: List[int] }
impl Counter {
    fn bump(mut self) { self.n = self.n + 1 }
    fn having(self, n: int) -> Counter {
        let mut out = self
        out.n = n
        return out
    }
}
fn name(c: Color) -> string {
    return match c {
        Color.Red => "red",
        Color.Green => "green",
        Color.Blue(n) => if n > 5 { "deep blue" } else { "blue" }
    }
}
fn code(c: Color) -> int {
    match c {
        Color.Red => { return 1 }
        other => { return 0 }
    }
}
fn sign(n: int) -> string {
    return match n {
        0 => "zero",
        x if x < 0 => "neg",
        _ => "pos"
    }
}
fn yes(b: bool) -> string {
    return match b { true => "y", false => "n" }
}
fn get(o: Opt) -> int {
    match o {
        Opt.Has(v) => { return v }
        Opt.Nope => { return -1 }
    }
}
fn boxed(b: Box[Color]) -> string {
    return name(*b)
}
fn word(s: string) -> int {
    let mut n = 0
    match s {
        "one" => { n = 1 }
        "two" => { n = 2 }
    }
    return n
}
fn opt(n: int) -> Option[int] {
    if n > 0 { return Some(n) }
    return None
}
fn main() -> int {
    println(name(Color.Blue(9)) + name(Color.Blue(1)) + name(Color.Red))
    println(code(Color.Green) + code(Color.Red))
    println(sign(-3) + sign(0) + sign(4) + yes(true) + yes(false))
    println(get(Opt.Has(4)) + get(Opt.Nope))
    println(boxed(Box_new(Color.Green)))
    println(word("two") + word("three"))
    match opt(3) {
        Some(v) => println(v),
        None => println("none")
    }
    let mut c = Counter { n: 0, items: [] }
    c.bump()
    c.n = c.n + 10
    c.items.push(4)
    let mut xs = [1, 2, 3]
    xs[0] = 7
    let d = c.having(5)
    println(c.n + xs[0] + d.n + c.items[0])
    let mut i = 0
    for k in 0..3 { i += k }
    let neg = -i
    let f = -1.5
    if not (neg > 0) and not false { println(neg) }
    println(f)
    return 0
}
