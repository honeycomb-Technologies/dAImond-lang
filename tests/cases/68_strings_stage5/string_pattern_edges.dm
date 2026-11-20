module test

fn show(parts: List[string]) -> string {
    let mut out = int_to_string(parts.len()) + ":"
    for p in parts {
        out = out + "[" + p + "]"
    }
    return out
}

fn main() {
    -- A pattern longer than the string is not found
    println(show(string_split("ab", "abc")))
    println("[" + string_replace("", "a", "b") + "][" + string_replace("ab", "abc", "x") + "]")
    -- An empty delimiter splits a string into its bytes
    println(show(string_split("abc", "")))
    if is_alpha("_") {
        println("underscore is a letter")
    }
}