module test
fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("z", 1)
    m.insert("a", 2)
    m.insert("m", 3)
    m.remove("a")
    m.insert("a", 4)
    m["a"] += 10
    let mut out = ""
    for k in m {
        out = out + k + int_to_string(m[k])
    }
    println(out)
}