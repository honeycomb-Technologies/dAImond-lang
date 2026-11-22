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
                while lex.pos < lex.src_len and is_digit_char(char_at(lex.source, lex.pos)) {
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
                let mut is_float = false
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos) == "." and is_digit_char(char_at(lex.source, lex.pos + 1)) {
                    is_float = true
                }
                if is_float {
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                    while lex.pos < lex.src_len and is_digit_char(char_at(lex.source, lex.pos)) {
                        lex.pos = lex.pos + 1
                        lex.col = lex.col + 1
                    }
                }
                -- An exponent makes a float: 1.5e10, 2e-3, 6E+2. The
                -- "e" belongs to the number only when digits follow it.
                let exp_digits = exponent_digits_at(lex.source, lex.pos)
                if exp_digits > lex.pos {
                    is_float = true
                    lex.col = lex.col + exp_digits - lex.pos
                    lex.pos = exp_digits
                    while lex.pos < lex.src_len and is_digit_char(char_at(lex.source, lex.pos)) {
                        lex.pos = lex.pos + 1
                        lex.col = lex.col + 1
                    }
                }
                let text = substr(lex.source, sp, lex.pos - sp)
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
