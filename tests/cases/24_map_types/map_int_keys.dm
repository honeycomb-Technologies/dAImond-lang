module test

fn main() {
    let mut m: Map[int, string] = Map_new()
    m.insert(1, "one")
    m.insert(2, "two")
    println(m.get(1))
    println(m.get(2))
}