module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("hello", 1)
    let keys = m.keys()
    println(int_to_string(keys.len()))
}