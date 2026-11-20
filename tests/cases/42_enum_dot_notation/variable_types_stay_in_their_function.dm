module test
enum Color {
    Red,
    Green
}
struct P {
    v: int
}
fn f(c: Color, s: P, xs: List[string]) -> int {
    if c == Color::Red {
        return 1 + s.v + xs.len()
    }
    return 2
}
fn g(c: int, s: string, xs: int) -> bool {
    return c == 0 and len(s) == 1 and xs == 2
}
fn h() -> bool {
    let c = 5
    let s = 7
    let xs = "x"
    return c != 0 and s == 7 and len(xs) == 1
}
fn k() -> bool {
    let c: int = 5
    let s: int = 7
    return c != 0 and s == 7
}
fn main() {
    let mut names: List[string] = []
    names.push("a")
    println(int_to_string(f(Color::Red, P { v: 3 }, names)))
    println(bool_to_string(g(0, "s", 2)) + " " + bool_to_string(h()) + " " + bool_to_string(k()))
}