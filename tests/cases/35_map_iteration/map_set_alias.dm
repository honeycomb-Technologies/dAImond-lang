module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.set("x", 10)
    m.set("y", 20)
    println(int_to_string(m.get("x")))
    println(int_to_string(m.get("y")))
}