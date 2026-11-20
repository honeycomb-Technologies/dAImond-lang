module test

fn main() {
    let s = "hello"
    let a: int = len(s)
    let b: int = s.len()
    println(a)
    println(b)
    println(len("abcd"))
    println(("ab" + "cde").len())
    println(len(s) + s.len())
}