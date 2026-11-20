module test

fn main() {
    println(is_alpha("a"))
    println(is_alpha("5"))
    println(is_digit("5"))
    println(is_digit("a"))
    println(is_alnum("Z"))
    println(is_alnum("9"))
    println(is_whitespace(" "))
    println(is_whitespace("x"))
    let s = "a1"
    println(is_alpha(char_at(s, 0)))
    println(is_digit(char_at(s, 1)))
    println(is_alpha(""))
}