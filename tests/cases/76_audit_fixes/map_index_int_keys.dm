module test

fn main() {
    let mut m: Map[int, string] = Map_new()
    m[1] = "one"
    m[2] = "two"
    println(m[1])
    println(m[2])
}