module test
fn main() {
    let mut a: Map[string, int] = Map_new()
    a.insert("x", 1)
    let mut b = a
    b.insert("y", 2)
    b.insert("z", 3)
    println(int_to_string(a.len()) + " " + bool_to_string(a.contains("y")) + " " + int_to_string(a.get("x")))
    println(int_to_string(b.len()) + " " + int_to_string(b.get("y")) + " " + int_to_string(b.get("x")))
}