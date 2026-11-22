module named_functions_with_wide_values

-- Functions passed by name whose parameters and results are lists,
-- structs and strings, which are passed in memory.
struct P { x: int, y: int, name: string }
fn total(xs: List[int]) -> int {
    let mut t = 0
    for x in xs { t = t + x }
    return t
}
fn show(p: P) -> string { return p.name + int_to_string(p.x + p.y) }
fn mk(n: int) -> P { return P { x: n, y: n, name: "p" } }
fn names(n: int) -> List[string] { return ["a", int_to_string(n)] }
fn run(f: fn(List[int]) -> int) -> int {
    let xs = [1, 2, 3]
    return f(xs)
}
fn run2(f: fn(P) -> string, g: fn(int) -> P) -> string { return f(g(4)) }
fn run3(f: fn(int) -> List[string]) -> int { return f(3).len() }
fn run4(f: fn(string, float, bool) -> string) -> string { return f("s", 1.5, true) }
fn main() {
    println(int_to_string(run(total)))
    println(run2(show, mk))
    println(int_to_string(run3(names)))
    println(run4(|s: string, x: float, b: bool| -> string s + float_to_string(x) + bool_to_string(b)))
}
