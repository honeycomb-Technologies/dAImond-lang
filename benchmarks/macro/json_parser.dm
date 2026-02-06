-- Macro Benchmark: JSON Parser
-- A simple recursive descent JSON parser.
-- Tests string processing, recursion, and pattern matching.

module json_parser_bench

-- JSON value types: 0=null, 1=bool, 2=number, 3=string, 4=array, 5=object
struct JsonValue {
    kind: int,
    str_val: string,
    num_val: float,
    bool_val: bool,
}

struct ParseCtx {
    input: string,
    pos: int,
}

fn json_is_ws(ch: string) -> bool {
    return ch == " " or ch == "\t" or ch == "\n" or ch == "\r"
}

fn json_is_digit(ch: string) -> bool {
    return ch >= "0" and ch <= "9"
}

fn skip_whitespace(mut ctx: ParseCtx) -> ParseCtx {
    while ctx.pos < len(ctx.input) {
        let ch = char_at(ctx.input, ctx.pos)
        if json_is_ws(ch) {
            ctx.pos = ctx.pos + 1
        } else {
            break
        }
    }
    return ctx
}

fn parse_string(mut ctx: ParseCtx) -> ParseCtx {
    -- Skip opening quote
    ctx.pos = ctx.pos + 1
    let start = ctx.pos
    while ctx.pos < len(ctx.input) {
        let ch = char_at(ctx.input, ctx.pos)
        if ch == "\"" {
            ctx.pos = ctx.pos + 1
            return ctx
        }
        if ch == "\\" {
            ctx.pos = ctx.pos + 2
        } else {
            ctx.pos = ctx.pos + 1
        }
    }
    return ctx
}

fn parse_number(mut ctx: ParseCtx) -> ParseCtx {
    if ctx.pos < len(ctx.input) {
        if char_at(ctx.input, ctx.pos) == "-" {
            ctx.pos = ctx.pos + 1
        }
    }
    let mut cont = true
    while cont {
        if ctx.pos >= len(ctx.input) {
            cont = false
        } else {
            let ch = char_at(ctx.input, ctx.pos)
            if json_is_digit(ch) {
                ctx.pos = ctx.pos + 1
            } else {
                cont = false
            }
        }
    }
    if ctx.pos < len(ctx.input) {
        if char_at(ctx.input, ctx.pos) == "." {
            ctx.pos = ctx.pos + 1
            let mut cont2 = true
            while cont2 {
                if ctx.pos >= len(ctx.input) {
                    cont2 = false
                } else {
                    let ch = char_at(ctx.input, ctx.pos)
                    if json_is_digit(ch) {
                        ctx.pos = ctx.pos + 1
                    } else {
                        cont2 = false
                    }
                }
            }
        }
    }
    return ctx
}

fn parse_value(mut ctx: ParseCtx) -> ParseCtx {
    ctx = skip_whitespace(ctx)
    if ctx.pos >= len(ctx.input) {
        return ctx
    }
    let ch = char_at(ctx.input, ctx.pos)

    if ch == "\"" {
        ctx = parse_string(ctx)
    } else {
        if ch == "{" {
            ctx = parse_object(ctx)
        } else {
            if ch == "[" {
                ctx = parse_array(ctx)
            } else {
                if ch == "t" {
                    ctx.pos = ctx.pos + 4
                } else {
                    if ch == "f" {
                        ctx.pos = ctx.pos + 5
                    } else {
                        if ch == "n" {
                            ctx.pos = ctx.pos + 4
                        } else {
                            ctx = parse_number(ctx)
                        }
                    }
                }
            }
        }
    }

    return ctx
}

fn parse_array(mut ctx: ParseCtx) -> ParseCtx {
    ctx.pos = ctx.pos + 1
    ctx = skip_whitespace(ctx)
    if ctx.pos < len(ctx.input) and char_at(ctx.input, ctx.pos) == "]" {
        ctx.pos = ctx.pos + 1
        return ctx
    }
    ctx = parse_value(ctx)
    ctx = skip_whitespace(ctx)
    while ctx.pos < len(ctx.input) and char_at(ctx.input, ctx.pos) == "," {
        ctx.pos = ctx.pos + 1
        ctx = parse_value(ctx)
        ctx = skip_whitespace(ctx)
    }
    if ctx.pos < len(ctx.input) {
        ctx.pos = ctx.pos + 1
    }
    return ctx
}

fn parse_object(mut ctx: ParseCtx) -> ParseCtx {
    ctx.pos = ctx.pos + 1
    ctx = skip_whitespace(ctx)
    if ctx.pos < len(ctx.input) and char_at(ctx.input, ctx.pos) == "}" {
        ctx.pos = ctx.pos + 1
        return ctx
    }
    -- Parse key
    ctx = parse_string(ctx)
    ctx = skip_whitespace(ctx)
    -- Skip colon
    if ctx.pos < len(ctx.input) {
        ctx.pos = ctx.pos + 1
    }
    -- Parse value
    ctx = parse_value(ctx)
    ctx = skip_whitespace(ctx)

    while ctx.pos < len(ctx.input) and char_at(ctx.input, ctx.pos) == "," {
        ctx.pos = ctx.pos + 1
        ctx = skip_whitespace(ctx)
        ctx = parse_string(ctx)
        ctx = skip_whitespace(ctx)
        if ctx.pos < len(ctx.input) {
            ctx.pos = ctx.pos + 1
        }
        ctx = parse_value(ctx)
        ctx = skip_whitespace(ctx)
    }
    if ctx.pos < len(ctx.input) {
        ctx.pos = ctx.pos + 1
    }
    return ctx
}

fn build_test_json() -> string {
    let mut json = "["
    let mut i = 0
    while i < 1000 {
        if i > 0 {
            json = json + ","
        }
        json = json + "{\"id\":" + int_to_string(i) + ",\"name\":\"item_" + int_to_string(i) + "\",\"value\":" + float_to_string(1.5 * i as float) + ",\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}"
        i = i + 1
    }
    json = json + "]"
    return json
}

fn main() {
    let json = build_test_json()
    println("JSON length: " + int_to_string(len(json)))

    -- Parse 10 times
    let mut i = 0
    while i < 10 {
        let mut ctx = ParseCtx { input: json, pos: 0 }
        ctx = parse_value(ctx)
        i = i + 1
    }
    println("Parsed 10 iterations")
    println("Done")
}
