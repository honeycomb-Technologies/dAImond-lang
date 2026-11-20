module test
struct P {
    a: string,
    b: int
}
fn label(n: int) -> string {
    return "n" + int_to_string(n)
}
fn pick(t: string, n: int) -> string {
    let s = if len(t) > 0 { t } else { label(n) }
    return s
}
fn grade(n: int) -> string {
    return if n > 10 { "big" } else if n > 5 { label(n) } else { "small" }
}
fn main() {
    println(pick("", 3))
    println(pick("given", 3))
    println(grade(20) + " " + grade(7) + " " + grade(1))
    let p = if pick("", 2) == "n2" { P { a: "x", b: 2 } } else { P { a: "y", b: 3 } }
    println(p.a + int_to_string(p.b))
    let k = if pick("", 1) == "n1" { 40 + 2 } else { 0 }
    println(int_to_string(k))
}