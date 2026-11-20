module test

fn main() {
    let mut words: List[string] = List_new()
    words.push("hello")
    words.push("world")
    println(bool_to_string(words.contains("hello")))
    println(bool_to_string(words.contains("missing")))
}