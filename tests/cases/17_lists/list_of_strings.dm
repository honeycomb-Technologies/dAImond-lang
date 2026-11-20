module test

fn main() {
    let mut words: List[String] = List_new()
    words.push("hello")
    words.push(" ")
    words.push("world")
    print(words[0])
    print(words[1])
    println(words[2])
}