module test

fn main() {
    let s = "a5 Z"
    println(is_alpha(s[0]))
    println(is_digit(s[1]))
    println(is_whitespace(s[2]))
    println(is_alnum(s[3]))
}