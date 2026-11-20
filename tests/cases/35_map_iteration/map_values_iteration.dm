module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 100)
    let vals = m.values()
    println(int_to_string(vals.len()))
}