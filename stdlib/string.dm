module string

-- Standard String Module
-- Provides string manipulation utilities beyond the built-in functions.

-- Join a list of strings with a separator
fn string_join(parts: List[string], sep: string) -> string {
    if parts.len() == 0 { return "" }
    let mut result = parts[0]
    let mut i = 1
    while i < parts.len() {
        result = result + sep + parts[i]
        i = i + 1
    }
    return result
}

-- Repeat a string n times
fn string_repeat(s: string, n: int) -> string {
    let mut result = ""
    let mut i = 0
    while i < n {
        result = result + s
        i = i + 1
    }
    return result
}

-- Pad a string on the left to a given width
fn pad_left(s: string, width: int, pad_char: string) -> string {
    let slen = len(s)
    if slen >= width { return s }
    let padding = string_repeat(pad_char, width - slen)
    return padding + s
}

-- Pad a string on the right to a given width
fn pad_right(s: string, width: int, pad_char: string) -> string {
    let slen = len(s)
    if slen >= width { return s }
    let padding = string_repeat(pad_char, width - slen)
    return s + padding
}

-- Get individual characters as a list of single-char strings
fn chars(s: string) -> List[string] {
    let mut result: List[string] = List_new()
    let mut i = 0
    while i < len(s) {
        result.push(char_at(s, i))
        i = i + 1
    }
    return result
}

-- Reverse a string
fn string_reverse(s: string) -> string {
    let mut result = ""
    let mut i = len(s) - 1
    while i >= 0 {
        result = result + char_at(s, i)
        i = i - 1
    }
    return result
}
