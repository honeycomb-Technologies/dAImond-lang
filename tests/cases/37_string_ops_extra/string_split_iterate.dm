module test

fn main() {
    let words = string_split("hello world", " ")
    for w in words {
        println(w)
    }
}