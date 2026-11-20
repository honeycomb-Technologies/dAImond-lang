module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("x", 10)
    m.insert("y", 20)
    println(m.get("x"))
    println(m.get("y"))
}