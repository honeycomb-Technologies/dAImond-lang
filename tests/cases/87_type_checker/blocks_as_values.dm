module test

enum Color { Red, Green, Blue }
fn label(c: Color, loud: bool) -> string {
    let s = match c {
        Color.Red => { (if loud { "RED" } else { "red" }) }
        Color.Green => {
            let base = "green"
            base + "!"
        }
        _ => "blue"
    }
    return s
}
fn grade(a: bool, b: bool) -> int {
    let v = if a { (if b { 1 } else { 2 }) } else { 3 }
    return v
}
fn main() -> int {
    println(label(Color.Red, true) + label(Color.Red, false) + label(Color.Green, true) + label(Color.Blue, false))
    println(grade(true, true) * 100 + grade(true, false) * 10 + grade(false, false))
    return 0
}
