module lexer

import token

-- ============================================================
-- LEXER
-- ============================================================

fn is_alpha_char(ch: string) -> bool {
    if ch >= "a" and ch <= "z" { return true }
    if ch >= "A" and ch <= "Z" { return true }
    if ch == "_" { return true }
    return false
}

-- Where the digits of a number's exponent begin, when an exponent
-- begins at pos: after "e" or "E" and a sign, if there is one. Gives
-- pos when no exponent begins there.
fn exponent_digits_at(source: string, pos: int) -> int {
    let n = len(source)
    if pos >= n {
        return pos
    }
    let e = char_at(source, pos)
    if e != "e" and e != "E" {
        return pos
    }
    let mut digits = pos + 1
    if digits < n and (char_at(source, digits) == "+" or char_at(source, digits) == "-") {
        digits = digits + 1
    }
    if digits < n and is_digit_char(char_at(source, digits)) {
        return digits
    }
    return pos
}

fn is_digit_char(ch: string) -> bool {
    if ch >= "0" and ch <= "9" { return true }
    return false
}

-- ============================================================
-- NUMBERS
-- An integer is written in base 10, or after 0x, 0b or 0o in base
-- 16, 2 or 8. One `_` may stand between two digits: 1_000_000,
-- 0xff_ff. A float is digits, a point and digits, with an exponent
-- or without (see exponent_digits_at).
--
-- The token of an integer holds its value in base 10. An integer in
-- base 10 must fit in an int. One in another base gives the 64 bits
-- it writes: 0xffffffffffffffff is -1, and is the highest u64 where
-- a u64 is expected.
-- ============================================================

-- The base of the number that begins at pos
fn number_base_at(source: string, pos: int) -> int {
    if pos + 1 >= len(source) or char_at(source, pos) != "0" { return 10 }
    let mark = char_at(source, pos + 1)
    if mark == "x" { return 16 }
    if mark == "b" { return 2 }
    if mark == "o" { return 8 }
    return 10
}

-- The value of the digit at pos, or -1 when what is there is no digit
-- of the base
fn digit_at(source: string, pos: int, base: int) -> int {
    if pos >= len(source) { return -1 }
    let c = byte_at(source, pos)
    let mut d = -1
    if c >= 48 and c <= 57 {
        d = c - 48
    } else if c >= 97 and c <= 102 {
        d = c - 87
    } else if c >= 65 and c <= 70 {
        d = c - 55
    }
    if d >= base { return -1 }
    return d
}

-- Where the digits that begin at pos end. A `_` belongs to them when
-- it stands between two digits.
fn digits_end_at(source: string, pos: int, base: int) -> int {
    let mut i = pos
    let mut going = true
    while going {
        if digit_at(source, i, base) >= 0 {
            i = i + 1
        } else if i > pos and i < len(source) and char_at(source, i) == "_" and digit_at(source, i + 1, base) >= 0 {
            i = i + 2
        } else {
            going = false
        }
    }
    return i
}

-- The digits source[from..to] of the base as an integer in base 10:
-- the int with the 64 bits they write. "" when they need more bits.
fn based_int_text(source: string, from: int, to: int, base: int) -> string {
    -- The value is kept as two halves of 32 bits, since an int's
    -- arithmetic stops the program when it leaves an int's range
    let half = 4294967296
    let mut high = 0
    let mut low = 0
    let mut i = from
    while i < to {
        let d = digit_at(source, i, base)
        if d >= 0 {
            low = low * base + d
            high = high * base + low / half
            low = low % half
            if high >= half { return "" }
        }
        i = i + 1
    }
    if high >= 2147483648 {
        return int_to_string((high - half) * half + low)
    }
    return int_to_string(high * half + low)
}

-- The digits of a number in base 10 as they go into its token:
-- without `_`, and an integer without the zeros it begins with
fn plain_digits(text: string, is_float: bool) -> string {
    let mut parts: List[string] = []
    let mut leading = is_float == false
    let mut i = 0
    while i < len(text) {
        let ch = char_at(text, i)
        if ch != "_" {
            if leading and ch == "0" and i + 1 < len(text) {
                -- a zero that more digits follow
            } else {
                leading = false
                parts.push(ch)
            }
        }
        i = i + 1
    }
    return join_strings(parts)
}

-- One more than the highest int: the digits of the lowest int, which
-- is written with a minus before them (see int_token_value in
-- parser.dm)
fn LOWEST_INT_DIGITS() -> string { return "9223372036854775808" }

-- Whether an integer in base 10, as plain_digits gives it, is at most
-- LOWEST_INT_DIGITS
fn decimal_fits(digits: string) -> bool {
    if len(digits) != 19 { return len(digits) < 19 }
    return digits <= LOWEST_INT_DIGITS()
}

-- A number that cannot be read ends the compilation, as a string
-- with no end does
fn number_error(prefix: string, message: string, line: int) {
    eprintln(prefix + "Error: " + message + " at line " + int_to_string(line_in_file(line)))
    exit(1)
}

fn is_alnum_char(ch: string) -> bool {
    return is_alpha_char(ch) or is_digit_char(ch)
}

struct Lexer {
    source: string,
    pos: int,
    line: int,
    col: int,
    src_len: int
}

fn lexer_new(source: string) -> Lexer {
    return Lexer { source: source, pos: 0, line: 1, col: 1, src_len: len(source) }
}

fn lexer_skip_ws(lex: Lexer) -> Lexer {
    let mut l = lex
    while l.pos < l.src_len {
        let ch = char_at(l.source, l.pos)
        if ch == " " or ch == "\t" or ch == "\r" {
            l.pos = l.pos + 1
            l.col = l.col + 1
        } else if ch == "-" and l.pos + 1 < l.src_len and char_at(l.source, l.pos + 1) == "-" {
            l.pos = l.pos + 2
            l.col = l.col + 2
            while l.pos < l.src_len and char_at(l.source, l.pos) != "\n" {
                l.pos = l.pos + 1
                l.col = l.col + 1
            }
        } else {
            return l
        }
    }
    return l
}

fn tokenize(source: string) -> List[Token] {
    return tokenize_file(source, "", 0)
}

-- tokenize, for file number `file` of a program (see LINES IN FILES in
-- token.dm), which is named `name` in errors
fn tokenize_file(source: string, name: string, file: int) -> List[Token] {
    let mut prefix = ""
    if file > 0 {
        prefix = name + ": "
    }
    let mut lex = lexer_new(source)
    lex.line = file * FILE_LINE_SPAN() + 1
    let mut tokens: List[Token] = []
    let mut done = false

    while done == false {
        lex = lexer_skip_ws(lex)
        if lex.pos >= lex.src_len {
            tokens.push(token_new(TK_EOF(), "", lex.line, lex.col))
            done = true
        } else {
            let ch = char_at(lex.source, lex.pos)
            let sl = lex.line
            let sc = lex.col
            let sp = lex.pos

            if ch == "\n" {
                tokens.push(token_new(TK_NEWLINE(), "", sl, sc))
                lex.pos = lex.pos + 1
                lex.line = lex.line + 1
                lex.col = 1
            } else if is_alpha_char(ch) {
                while lex.pos < lex.src_len and is_alnum_char(char_at(lex.source, lex.pos)) {
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
                let text = substr(lex.source, sp, lex.pos - sp)
                -- Check for f-string: identifier "f" immediately followed by "
                let mut is_fstr = false
                if text == "f" and lex.pos < lex.src_len and char_at(lex.source, lex.pos) == "\"" {
                    is_fstr = true
                    lex.pos = lex.pos + 1  -- skip opening "
                    lex.col = lex.col + 1
                    let fstart = lex.pos
                    while lex.pos < lex.src_len and char_at(lex.source, lex.pos) != "\"" {
                        if char_at(lex.source, lex.pos) == "\\" {
                            lex.pos = lex.pos + 2
                            lex.col = lex.col + 2
                        } else {
                            lex.pos = lex.pos + 1
                            lex.col = lex.col + 1
                        }
                    }
                    let ftext = substr(lex.source, fstart, lex.pos - fstart)
                    if lex.pos < lex.src_len {
                        lex.pos = lex.pos + 1  -- skip closing "
                        lex.col = lex.col + 1
                    }
                    tokens.push(token_new(TK_FSTRING(), ftext, sl, sc))
                }
                if is_fstr == false {
                    tokens.push(token_new(keyword_lookup(text), text, sl, sc))
                }
            } else if is_digit_char(ch) {
                -- See NUMBERS
                let base = number_base_at(lex.source, lex.pos)
                let mut is_float = false
                let mut text = ""
                if base != 10 {
                    let digits_end = digits_end_at(lex.source, sp + 2, base)
                    if digits_end == sp + 2 {
                        number_error(prefix, "'" + substr(lex.source, sp, 2) + "' begins an integer in base " + int_to_string(base) + ", and no digit of that base follows it", sl)
                    }
                    lex.pos = digits_end
                    text = based_int_text(lex.source, sp + 2, digits_end, base)
                    if text == "" {
                        number_error(prefix, "the integer " + substr(lex.source, sp, lex.pos - sp) + " does not fit in 64 bits", sl)
                    }
                } else {
                    lex.pos = digits_end_at(lex.source, lex.pos, 10)
                    if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos) == "." and is_digit_char(char_at(lex.source, lex.pos + 1)) {
                        is_float = true
                        lex.pos = digits_end_at(lex.source, lex.pos + 1, 10)
                    }
                    -- An exponent makes a float: 1.5e10, 2e-3, 6E+2. The
                    -- "e" belongs to the number only when digits follow it.
                    let exp_digits = exponent_digits_at(lex.source, lex.pos)
                    if exp_digits > lex.pos {
                        is_float = true
                        lex.pos = exp_digits
                        while lex.pos < lex.src_len and is_digit_char(char_at(lex.source, lex.pos)) {
                            lex.pos = lex.pos + 1
                        }
                    }
                    text = plain_digits(substr(lex.source, sp, lex.pos - sp), is_float)
                    if is_float == false and decimal_fits(text) == false {
                        number_error(prefix, "the integer " + text + " does not fit in an int: the highest is 9223372036854775807", sl)
                    }
                }
                lex.col = lex.col + lex.pos - sp
                -- `12abc` was the number 12 and then the name abc
                if lex.pos < lex.src_len and is_alnum_char(char_at(lex.source, lex.pos)) {
                    number_error(prefix, "the number " + substr(lex.source, sp, lex.pos - sp) + " runs into '" + char_at(lex.source, lex.pos) + "': a number has no suffix, and a name does not begin with a digit", sl)
                }
                if is_float {
                    tokens.push(token_new(TK_FLOAT(), text, sl, sc))
                } else {
                    tokens.push(token_new(TK_INTEGER(), text, sl, sc))
                }
            } else if ch == "\"" {
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
                while lex.pos < lex.src_len and char_at(lex.source, lex.pos) != "\"" {
                    if char_at(lex.source, lex.pos) == "\\" {
                        lex.pos = lex.pos + 2
                        lex.col = lex.col + 2
                    } else {
                        lex.pos = lex.pos + 1
                        lex.col = lex.col + 1
                    }
                }
                if lex.pos >= lex.src_len {
                    eprintln(prefix + "Error: unterminated string at line " + int_to_string(line_in_file(sl)))
                    exit(1)
                }
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
                let raw_text = substr(lex.source, sp + 1, lex.pos - sp - 2)
                let text = unescape_string(raw_text)
                tokens.push(token_new(TK_STRING(), text, sl, sc))
            } else if ch == "(" {
                tokens.push(token_new(TK_LPAREN(), "(", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == ")" {
                tokens.push(token_new(TK_RPAREN(), ")", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "{" {
                tokens.push(token_new(TK_LBRACE(), "{", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "}" {
                tokens.push(token_new(TK_RBRACE(), "}", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "[" {
                tokens.push(token_new(TK_LBRACKET(), "[", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "]" {
                tokens.push(token_new(TK_RBRACKET(), "]", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "+" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_PLUSEQ(), "+=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_PLUS(), "+", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "-" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_ARROW(), "->", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_MINUSEQ(), "-=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_MINUS(), "-", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "*" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_STAREQ(), "*=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_STAR(), "*", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "/" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "/" {
                    -- C-style // comment: skip to end of line
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                    while lex.pos < lex.src_len and char_at(lex.source, lex.pos) != "\n" {
                        lex.pos = lex.pos + 1
                        lex.col = lex.col + 1
                    }
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_SLASHEQ(), "/=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_SLASH(), "/", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "%" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_PERCENTEQ(), "%=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_PERCENT(), "%", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "=" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_EQEQ(), "==", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_FAT_ARROW(), "=>", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_EQ(), "=", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "!" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_BANGEQ(), "!=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_BANG(), "!", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "<" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_LTEQ(), "<=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "<" {
                    tokens.push(token_new(TK_SHL(), "<<", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_LT(), "<", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == ">" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_GTEQ(), ">=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_SHR(), ">>", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_GT(), ">", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "|" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_PIPEGT(), "|>", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_PIPE(), "|", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "&" {
                tokens.push(token_new(TK_AMP(), "&", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "^" {
                tokens.push(token_new(TK_CARET(), "^", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "~" {
                tokens.push(token_new(TK_TILDE(), "~", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "?" {
                tokens.push(token_new(TK_QUESTION(), "?", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "." {
                if lex.pos + 2 < lex.src_len and char_at(lex.source, lex.pos + 1) == "." and char_at(lex.source, lex.pos + 2) == "=" {
                    tokens.push(token_new(TK_DOTDOTEQ(), "..=", sl, sc))
                    lex.pos = lex.pos + 3
                    lex.col = lex.col + 3
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "." {
                    tokens.push(token_new(TK_DOTDOT(), "..", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_DOT(), ".", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == ":" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ":" {
                    tokens.push(token_new(TK_COLONCOLON(), "::", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_COLON(), ":", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "," {
                tokens.push(token_new(TK_COMMA(), ",", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == ";" {
                tokens.push(token_new(TK_SEMICOLON(), ";", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "@" {
                tokens.push(token_new(TK_AT(), "@", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else {
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            }
        }
    }

    return tokens
}

-- ============================================================
-- EXPRESSION KINDS
-- ============================================================

-- The strings joined into one. They are joined in pairs, then the
-- pairs in pairs, so the text is copied once for each level of
-- pairing. Adding them one after another to a growing string copies
-- the whole string so far each time, and no string is ever freed: the
-- 60 KB string that holds the runtime's source took 1.6 GB that way.
fn join_strings(pieces: List[string]) -> string {
    let mut level = pieces
    while level.len() > 1 {
        let mut next: List[string] = []
        let mut i = 0
        while i + 1 < level.len() {
            next.push(level[i] + level[i + 1])
            i = i + 2
        }
        if i < level.len() {
            next.push(level[i])
        }
        level = next
    }
    if level.len() == 0 {
        return ""
    }
    return level[0]
}

-- The text of a string literal with its escapes (\n, \t, \r, \\,
-- \" and \') replaced by what they stand for. A backslash before
-- any other character stays as it is.
fn unescape_string(s: string) -> string {
    -- Quick check: if no backslashes, return as-is
    if string_find(s, "\\") < 0 {
        return s
    }
    -- The stretches between escapes, and what the escapes stand for
    let mut pieces: List[string] = []
    let slen = len(s)
    let mut start = 0
    let mut i = 0
    while i < slen {
        -- 92 is a backslash
        if byte_at(s, i) == 92 and i + 1 < slen {
            let next = byte_at(s, i + 1)
            let mut stands_for = ""
            if next == 110 { stands_for = "\n" }
            if next == 116 { stands_for = "\t" }
            if next == 114 { stands_for = "\r" }
            if next == 92 { stands_for = "\\" }
            if next == 34 { stands_for = "\"" }
            if next == 39 { stands_for = "'" }
            if stands_for != "" {
                if i > start { pieces.push(substr(s, start, i - start)) }
                pieces.push(stands_for)
                i = i + 2
                start = i
            } else {
                i = i + 1
            }
        } else {
            i = i + 1
        }
    }
    if slen > start { pieces.push(substr(s, start, slen - start)) }
    return join_strings(pieces)
}
