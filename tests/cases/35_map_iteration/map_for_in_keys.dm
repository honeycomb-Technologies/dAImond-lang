module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    m.insert("b", 2)
    let mut count = 0
    for k in m {
        count = count + 1
    }
    println(int_to_string(count))
}