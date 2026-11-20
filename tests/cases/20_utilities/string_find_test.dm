module test

fn main() {
    let pos = string_find("hello world", "world")
    println(pos)
    let notfound = string_find("hello", "xyz")
    println(notfound)
}