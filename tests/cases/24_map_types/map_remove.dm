module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    println(m.contains("a"))
    m.remove("a")
    println(m.contains("a"))
}