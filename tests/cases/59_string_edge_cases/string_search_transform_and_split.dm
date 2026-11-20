module test

fn show(parts: List[string]) -> string {
    let mut out = int_to_string(parts.len()) + ":"
    for p in parts {
        out = out + "[" + p + "]"
    }
    return out
}

fn b(v: bool) -> string {
    if v { return "T" }
    return "F"
}

fn main() {
    -- contains
    println(b(string_contains("hello world", "lo w")) + b(string_contains("hello", "")) + b(string_contains("", "")) + b(string_contains("", "a")) + b(string_contains("abc", "abcd")) + b(string_contains("abc", "abc")) + b(string_contains("aab", "ab")) + b(string_contains("abc", "bd")) + b(string_contains("abc", "c")))
    -- find
    println(int_to_string(string_find("hello world", "o")) + " " + int_to_string(string_find("hello", "")) + " " + int_to_string(string_find("", "")) + " " + int_to_string(string_find("", "a")) + " " + int_to_string(string_find("abc", "abcd")) + " " + int_to_string(string_find("aaab", "ab")) + " " + int_to_string(string_find("abc", "x")) + " " + int_to_string(string_find("abc", "c")) + " " + int_to_string(string_find("abcabc", "ca")))
    -- starts_with / ends_with
    println(b(starts_with("hello", "he")) + b(starts_with("hello", "")) + b(starts_with("", "")) + b(starts_with("he", "hello")) + b(starts_with("hello", "hello")) + b(starts_with("hello", "el")) + b(starts_with("", "a")))
    println(b(ends_with("hello", "lo")) + b(ends_with("hello", "")) + b(ends_with("", "")) + b(ends_with("lo", "hello")) + b(ends_with("hello", "hello")) + b(ends_with("hello", "ll")) + b(ends_with("", "a")))
    -- trim
    println("[" + string_trim("  hi there \t\n") + "][" + string_trim("") + "][" + string_trim("   ") + "][" + string_trim("x") + "][" + string_trim("\r\n a b \r\n") + "][" + string_trim("no") + "]")
    -- replace
    println("[" + string_replace("a-b-c", "-", "+") + "][" + string_replace("aaa", "aa", "b") + "][" + string_replace("abc", "", "x") + "][" + string_replace("abc", "x", "y") + "][" + "][" + string_replace("abab", "ab", "") + "][" + string_replace("xx", "x", "long") + "][" + string_replace("abc", "abc", "") + "][" + string_replace("a.b", ".", "..") + "][" + string_replace("abcab", "ab", "ab") + "]")
    -- case
    println(string_to_upper("Hello, World 123 az{`") + "|" + string_to_lower("Hello, World 123 AZ[@") + "|" + string_to_upper("") + "|" + string_to_lower(""))
    -- split
    println(show(string_split("a,b,c", ",")))
    println(show(string_split("", ",")))
    println(show(string_split(",a,,b,", ",")))
    println(show(string_split("abc", ",")))
    println(show(string_split("a::b::c", "::")))
    println(show(string_split("a:::b", "::")))
    println(show(string_split("::", "::")))
    println(show(string_split("aaa", "aa")))
    println(show(string_split("one two  three", " ")))
    -- character classes
    println(b(is_alpha("a")) + b(is_alpha("Z")) + b(is_alpha("5")) + b(is_alpha("")) + b(is_alpha("ab1")) + " " + b(is_digit("0")) + b(is_digit("9")) + b(is_digit("a")) + b(is_digit("")) + " " + b(is_whitespace(" ")) + b(is_whitespace("\t")) + b(is_whitespace("\n")) + b(is_whitespace("x")) + b(is_whitespace("")) + " " + b(is_alnum("a")) + b(is_alnum("7")) + b(is_alnum("-")) + b(is_alnum("")))
    -- results are independent strings
    let src = "  Mixed Case  "
    let t = string_trim(src)
    let u = string_to_upper(t)
    let parts = string_split("k=v", "=")
    let mut ps = parts
    ps.push("extra")
    println(src + "|" + t + "|" + u + "|" + show(parts) + "|" + parts[0] + parts[1] + "|" + int_to_string(len(t)))
    -- many parts, past the first allocation
    let mut joined = ""
    let mut i = 0
    while i < 100 {
        joined = joined + int_to_string(i) + ";"
        i = i + 1
    }
    let many = string_split(joined, ";")
    println(int_to_string(many.len()) + " " + many[0] + " " + many[57] + " " + many[99] + "[" + many[100] + "]")
}