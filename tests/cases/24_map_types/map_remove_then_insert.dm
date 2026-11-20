module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("key", 1)
    m.remove("key")
    m.insert("key", 2)
    println(m.get("key"))
    println(m.len())
}