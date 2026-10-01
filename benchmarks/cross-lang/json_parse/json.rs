fn skip_whitespace(input: &[u8], mut pos: usize) -> usize {
    while pos < input.len() && (input[pos] == b' ' || input[pos] == b'\n' || input[pos] == b'\t' || input[pos] == b'\r') {
        pos += 1;
    }
    pos
}

fn parse_string(input: &[u8], mut pos: usize) -> usize {
    pos += 1; // skip opening quote
    while pos < input.len() && input[pos] != b'"' {
        if input[pos] == b'\\' {
            pos += 2;
        } else {
            pos += 1;
        }
    }
    pos += 1; // skip closing quote
    pos
}

fn parse_number(input: &[u8], mut pos: usize) -> usize {
    if pos < input.len() && input[pos] == b'-' {
        pos += 1;
    }
    while pos < input.len() && input[pos] >= b'0' && input[pos] <= b'9' {
        pos += 1;
    }
    if pos < input.len() && input[pos] == b'.' {
        pos += 1;
        while pos < input.len() && input[pos] >= b'0' && input[pos] <= b'9' {
            pos += 1;
        }
    }
    pos
}

fn parse_value(input: &[u8], mut pos: usize) -> usize {
    pos = skip_whitespace(input, pos);
    if pos >= input.len() { return pos; }
    let ch = input[pos];
    if ch == b'"' { return parse_string(input, pos); }
    if ch == b'{' { return parse_object(input, pos); }
    if ch == b'[' { return parse_array(input, pos); }
    if ch == b't' { return pos + 4; }
    if ch == b'f' { return pos + 5; }
    if ch == b'n' { return pos + 4; }
    parse_number(input, pos)
}

fn parse_array(input: &[u8], mut pos: usize) -> usize {
    pos += 1; // skip [
    pos = skip_whitespace(input, pos);
    if pos < input.len() && input[pos] == b']' { return pos + 1; }
    pos = parse_value(input, pos);
    pos = skip_whitespace(input, pos);
    while pos < input.len() && input[pos] == b',' {
        pos += 1;
        pos = parse_value(input, pos);
        pos = skip_whitespace(input, pos);
    }
    pos += 1; // skip ]
    pos
}

fn parse_object(input: &[u8], mut pos: usize) -> usize {
    pos += 1; // skip {
    pos = skip_whitespace(input, pos);
    if pos < input.len() && input[pos] == b'}' { return pos + 1; }
    pos = parse_string(input, pos);
    pos = skip_whitespace(input, pos);
    pos += 1; // skip :
    pos = parse_value(input, pos);
    pos = skip_whitespace(input, pos);
    while pos < input.len() && input[pos] == b',' {
        pos += 1;
        pos = skip_whitespace(input, pos);
        pos = parse_string(input, pos);
        pos = skip_whitespace(input, pos);
        pos += 1; // skip :
        pos = parse_value(input, pos);
        pos = skip_whitespace(input, pos);
    }
    pos += 1; // skip }
    pos
}

fn main() {
    let mut json = String::with_capacity(2 * 1024 * 1024);
    json.push('[');
    for i in 0..10000 {
        if i > 0 { json.push(','); }
        json.push_str(&format!(
            "{{\"id\":{},\"name\":\"item_{}\",\"value\":{},\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}}",
            i, i, i as f64 * 1.5
        ));
    }
    json.push(']');

    println!("json length: {}", json.len());

    let input = json.as_bytes();
    let mut count = 0;
    while count < 100 {
        parse_value(input, 0);
        count += 1;
    }

    println!("parsed: {}", count);
}
