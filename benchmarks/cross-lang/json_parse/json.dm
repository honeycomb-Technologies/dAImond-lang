module json_bench

fn skip_whitespace(input: string, mut pos: int) -> int {
    while pos < len(input) and (char_at(input, pos) == " " or char_at(input, pos) == "\n" or char_at(input, pos) == "\t" or char_at(input, pos) == "\r") {
        pos = pos + 1
    }
    return pos
}

fn parse_string(input: string, mut pos: int) -> int {
    -- skip opening quote
    pos = pos + 1
    while pos < len(input) and char_at(input, pos) != "\"" {
        if char_at(input, pos) == "\\" {
            pos = pos + 2
        } else {
            pos = pos + 1
        }
    }
    -- skip closing quote
    pos = pos + 1
    return pos
}

fn parse_number(input: string, mut pos: int) -> int {
    if pos < len(input) and char_at(input, pos) == "-" {
        pos = pos + 1
    }
    while pos < len(input) and char_at(input, pos) >= "0" and char_at(input, pos) <= "9" {
        pos = pos + 1
    }
    if pos < len(input) and char_at(input, pos) == "." {
        pos = pos + 1
        while pos < len(input) and char_at(input, pos) >= "0" and char_at(input, pos) <= "9" {
            pos = pos + 1
        }
    }
    return pos
}

fn parse_value(input: string, mut pos: int) -> int {
    pos = skip_whitespace(input, pos)
    if pos >= len(input) { return pos }
    let ch = char_at(input, pos)
    if ch == "\"" { return parse_string(input, pos) }
    if ch == "{" { return parse_object(input, pos) }
    if ch == "[" { return parse_array(input, pos) }
    if ch == "t" { return pos + 4 }
    if ch == "f" { return pos + 5 }
    if ch == "n" { return pos + 4 }
    return parse_number(input, pos)
}

fn parse_array(input: string, mut pos: int) -> int {
    -- skip opening bracket
    pos = pos + 1
    pos = skip_whitespace(input, pos)
    if pos < len(input) and char_at(input, pos) == "]" {
        return pos + 1
    }
    pos = parse_value(input, pos)
    pos = skip_whitespace(input, pos)
    while pos < len(input) and char_at(input, pos) == "," {
        pos = pos + 1
        pos = parse_value(input, pos)
        pos = skip_whitespace(input, pos)
    }
    -- skip closing bracket
    pos = pos + 1
    return pos
}

fn parse_object(input: string, mut pos: int) -> int {
    -- skip opening brace
    pos = pos + 1
    pos = skip_whitespace(input, pos)
    if pos < len(input) and char_at(input, pos) == "}" {
        return pos + 1
    }
    -- parse first key:value
    pos = parse_string(input, pos)
    pos = skip_whitespace(input, pos)
    -- skip colon
    pos = pos + 1
    pos = parse_value(input, pos)
    pos = skip_whitespace(input, pos)
    while pos < len(input) and char_at(input, pos) == "," {
        pos = pos + 1
        pos = skip_whitespace(input, pos)
        pos = parse_string(input, pos)
        pos = skip_whitespace(input, pos)
        -- skip colon
        pos = pos + 1
        pos = parse_value(input, pos)
        pos = skip_whitespace(input, pos)
    }
    -- skip closing brace
    pos = pos + 1
    return pos
}

fn main() {
    let mut json = "["
    let mut i = 0
    while i < 1000 {
        if i > 0 {
            json = json + ","
        }
        json = json + "{\"id\":" + int_to_string(i) + ",\"name\":\"item_" + int_to_string(i) + "\",\"value\":" + float_to_string(i as float * 1.5) + ",\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}"
        i = i + 1
    }
    json = json + "]"

    println("json length: " + int_to_string(len(json)))

    let mut count = 0
    while count < 100 {
        let mut start_pos = 0
        let result = parse_value(json, start_pos)
        count = count + 1
    }

    println("parsed: " + int_to_string(count))
}
