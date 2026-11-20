module probe

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("target", 777)
    let n = m
    let mut i = 0
    while i < 10000 {
        m.insert("k" + int_to_string(i), i)
        i = i + 1
    }
    println(int_to_string(n.get("target")))
}
