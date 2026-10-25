module e4

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    let n = m
    m.insert("a", 222)
    m.remove("a")
    println(int_to_string(n.get("a")))
    println(int_to_string(len(n)))
    println(int_to_string(len(m)))
}
