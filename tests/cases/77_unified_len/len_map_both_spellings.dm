module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    m.insert("b", 2)
    println(len(m))
    println(m.len())
    println(m.keys().len())
    println(len(m.values()))
}