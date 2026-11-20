module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("hello", 42)
    println(m.contains("hello"))
    println(m.contains("world"))
}