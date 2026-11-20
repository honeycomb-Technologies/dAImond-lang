module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m["a"] = 1
    m.insert("b", 2)
    println(m["a"])
    println(m.get("b"))
    m["a"] = 10
    println(m["a"])
    let v: int = m["b"]
    println(v)
}