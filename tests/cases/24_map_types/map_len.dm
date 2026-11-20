module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    println(m.len())
    m.insert("a", 1)
    m.insert("b", 2)
    m.insert("c", 3)
    println(m.len())
    m.remove("b")
    println(m.len())
}