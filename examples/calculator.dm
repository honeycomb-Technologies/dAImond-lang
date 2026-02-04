-- dAImond Calculator
-- Demonstrates basic parsing and evaluation of arithmetic expressions.
-- Supports: +, -, *, /, parentheses, decimal numbers

module calculator

-- Check if a character is a digit (0-9)
fn is_digit_char(ch: string) -> bool {
    return ch >= "0" and ch <= "9"
}

-- Check if a character is whitespace
fn is_space_char(ch: string) -> bool {
    return ch == " " or ch == "\t"
}

-- Token types as integer constants
-- 0 = Number, 1 = Plus, 2 = Minus, 3 = Star, 4 = Slash,
-- 5 = LParen, 6 = RParen, 7 = Eof, 8 = Error

struct Token {
    kind: int,
    value: string,
}

struct Tokenizer {
    input: string,
    pos: int,
}

fn tokenizer_new(input: string) -> Tokenizer {
    return Tokenizer { input: input, pos: 0 }
}

fn tokenizer_skip_whitespace(mut tok: Tokenizer) -> Tokenizer {
    while tok.pos < len(tok.input) {
        let ch = char_at(tok.input, tok.pos)
        if is_space_char(ch) {
            tok.pos = tok.pos + 1
        } else {
            break
        }
    }
    return tok
}

fn tokenizer_read_number(mut tok: Tokenizer) -> Tokenizer {
    let mut has_dot = false
    while tok.pos < len(tok.input) {
        let ch = char_at(tok.input, tok.pos)
        if is_digit_char(ch) {
            tok.pos = tok.pos + 1
        } else {
            if ch == "." and not has_dot {
                has_dot = true
                tok.pos = tok.pos + 1
            } else {
                break
            }
        }
    }
    return tok
}

fn tokenizer_next(mut tok: Tokenizer, start_pos: int) -> Token {
    tok = tokenizer_skip_whitespace(tok)
    if tok.pos >= len(tok.input) {
        return Token { kind: 7, value: "" }
    }
    let ch = char_at(tok.input, tok.pos)
    if is_digit_char(ch) or ch == "." {
        return Token { kind: 0, value: "num" }
    }
    tok.pos = tok.pos + 1
    if ch == "+" { return Token { kind: 1, value: "+" } }
    if ch == "-" { return Token { kind: 2, value: "-" } }
    if ch == "*" { return Token { kind: 3, value: "*" } }
    if ch == "/" { return Token { kind: 4, value: "/" } }
    if ch == "(" { return Token { kind: 5, value: "(" } }
    if ch == ")" { return Token { kind: 6, value: ")" } }
    return Token { kind: 8, value: ch }
}

fn main() {
    println("dAImond Calculator")
    println("==================")

    let expr1 = "2 + 3"
    println(expr1 + " = " + float_to_string(eval_expr_string(expr1)))

    let expr2 = "10 - 4 * 2"
    println(expr2 + " = " + float_to_string(eval_expr_string(expr2)))

    let expr3 = "(1 + 2) * (3 + 4)"
    println(expr3 + " = " + float_to_string(eval_expr_string(expr3)))

    let expr4 = "100 / 4 / 5"
    println(expr4 + " = " + float_to_string(eval_expr_string(expr4)))

    let expr5 = "-3 + 5"
    println(expr5 + " = " + float_to_string(eval_expr_string(expr5)))

    let expr6 = "3.14 * 2"
    println(expr6 + " = " + float_to_string(eval_expr_string(expr6)))

    println("==================")
    println("Done")
}

-- Global parser state
-- We use global mutable state to avoid the double-pointer codegen bug
-- that occurs when methods call other methods on mut self.

struct ParseState {
    input: string,
    pos: int,
    cur_kind: int,
    cur_value: string,
}

fn ps_new(input: string) -> ParseState {
    return ParseState { input: input, pos: 0, cur_kind: 7, cur_value: "" }
}

fn ps_skip_ws(mut ps: ParseState) -> ParseState {
    while ps.pos < len(ps.input) {
        let ch = char_at(ps.input, ps.pos)
        if is_space_char(ch) {
            ps.pos = ps.pos + 1
        } else {
            break
        }
    }
    return ps
}

fn ps_read_number(mut ps: ParseState) -> ParseState {
    let start = ps.pos
    let mut has_dot = false
    while ps.pos < len(ps.input) {
        let ch = char_at(ps.input, ps.pos)
        if is_digit_char(ch) {
            ps.pos = ps.pos + 1
        } else {
            if ch == "." and not has_dot {
                has_dot = true
                ps.pos = ps.pos + 1
            } else {
                break
            }
        }
    }
    ps.cur_kind = 0
    ps.cur_value = substr(ps.input, start, ps.pos - start)
    return ps
}

fn ps_advance(mut ps: ParseState) -> ParseState {
    ps = ps_skip_ws(ps)
    if ps.pos >= len(ps.input) {
        ps.cur_kind = 7
        ps.cur_value = ""
        return ps
    }
    let ch = char_at(ps.input, ps.pos)
    if is_digit_char(ch) or ch == "." {
        ps = ps_read_number(ps)
        return ps
    }
    ps.pos = ps.pos + 1
    if ch == "+" {
        ps.cur_kind = 1
        ps.cur_value = "+"
    } else {
        if ch == "-" {
            ps.cur_kind = 2
            ps.cur_value = "-"
        } else {
            if ch == "*" {
                ps.cur_kind = 3
                ps.cur_value = "*"
            } else {
                if ch == "/" {
                    ps.cur_kind = 4
                    ps.cur_value = "/"
                } else {
                    if ch == "(" {
                        ps.cur_kind = 5
                        ps.cur_value = "("
                    } else {
                        if ch == ")" {
                            ps.cur_kind = 6
                            ps.cur_value = ")"
                        } else {
                            ps.cur_kind = 8
                            ps.cur_value = ch
                        }
                    }
                }
            }
        }
    }
    return ps
}

-- Recursive descent parser returning (result, updated_state) via struct

struct ParseResult {
    value: float,
    ps: ParseState,
}

fn parse_factor(mut ps: ParseState) -> ParseResult {
    -- Unary minus
    if ps.cur_kind == 2 {
        ps = ps_advance(ps)
        let inner = parse_factor(ps)
        return ParseResult { value: 0.0 - inner.value, ps: inner.ps }
    }
    -- Parenthesized expression
    if ps.cur_kind == 5 {
        ps = ps_advance(ps)
        let inner = parse_expr(ps)
        ps = inner.ps
        if ps.cur_kind != 6 {
            panic("Expected closing parenthesis")
            return ParseResult { value: 0.0, ps: ps }
        }
        ps = ps_advance(ps)
        return ParseResult { value: inner.value, ps: ps }
    }
    -- Number
    if ps.cur_kind == 0 {
        let val = parse_float(ps.cur_value)
        ps = ps_advance(ps)
        return ParseResult { value: val, ps: ps }
    }
    panic("Unexpected token: " + ps.cur_value)
    return ParseResult { value: 0.0, ps: ps }
}

fn parse_term(mut ps: ParseState) -> ParseResult {
    let mut result = parse_factor(ps)
    ps = result.ps
    let mut left = result.value
    while ps.cur_kind == 3 or ps.cur_kind == 4 {
        let op = ps.cur_kind
        ps = ps_advance(ps)
        result = parse_factor(ps)
        ps = result.ps
        if op == 3 {
            left = left * result.value
        } else {
            if result.value == 0.0 {
                panic("Division by zero")
            }
            left = left / result.value
        }
    }
    return ParseResult { value: left, ps: ps }
}

fn parse_expr(mut ps: ParseState) -> ParseResult {
    let mut result = parse_term(ps)
    ps = result.ps
    let mut left = result.value
    while ps.cur_kind == 1 or ps.cur_kind == 2 {
        let op = ps.cur_kind
        ps = ps_advance(ps)
        result = parse_term(ps)
        ps = result.ps
        if op == 1 {
            left = left + result.value
        } else {
            left = left - result.value
        }
    }
    return ParseResult { value: left, ps: ps }
}

fn eval_expr_string(input: string) -> float {
    let mut ps = ps_new(input)
    ps = ps_advance(ps)
    let result = parse_expr(ps)
    return result.value
}
