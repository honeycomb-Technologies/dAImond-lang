module lexer

-- Lexer state
struct Lexer {
    source: string,
    pos: int,
    line: int,
    col: int,
    src_len: int
}

fn lexer_new(source: string) -> Lexer {
    return Lexer {
        source: source,
        pos: 0,
        line: 1,
        col: 1,
        src_len: len(source)
    }
}

-- Character classification helpers (work on single-char strings)
fn is_alpha_char(ch: string) -> bool {
    if ch >= "a" and ch <= "z" { return true }
    if ch >= "A" and ch <= "Z" { return true }
    if ch == "_" { return true }
    return false
}

fn is_digit_char(ch: string) -> bool {
    if ch >= "0" and ch <= "9" { return true }
    return false
}

fn is_alnum_char(ch: string) -> bool {
    return is_alpha_char(ch) or is_digit_char(ch)
}

fn is_whitespace_char(ch: string) -> bool {
    if ch == " " or ch == "\t" or ch == "\r" { return true }
    return false
}

-- Peek at current character without advancing
fn lexer_peek(lex: Lexer) -> string {
    if lex.pos >= lex.src_len {
        return ""
    }
    return char_at(lex.source, lex.pos)
}

-- Peek at character at offset from current position
fn lexer_peek_at(lex: Lexer, offset: int) -> string {
    let idx = lex.pos + offset
    if idx >= lex.src_len {
        return ""
    }
    return char_at(lex.source, idx)
}

-- Skip whitespace (not newlines)
fn lexer_skip_whitespace(lex: Lexer) -> Lexer {
    let mut l = lex
    while l.pos < l.src_len {
        let ch = char_at(l.source, l.pos)
        if ch == " " or ch == "\t" or ch == "\r" {
            l.pos = l.pos + 1
            l.col = l.col + 1
        } else if ch == "-" and l.pos + 1 < l.src_len and char_at(l.source, l.pos + 1) == "-" {
            -- Skip line comment
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

-- Read an identifier or keyword
fn lexer_read_identifier(lex: Lexer) -> Lexer {
    let mut l = lex
    let start = l.pos
    while l.pos < l.src_len and is_alnum_char(char_at(l.source, l.pos)) {
        l.pos = l.pos + 1
        l.col = l.col + 1
    }
    return l
}

-- Read a number (integer or float)
fn lexer_read_number(lex: Lexer) -> Lexer {
    let mut l = lex
    while l.pos < l.src_len and is_digit_char(char_at(l.source, l.pos)) {
        l.pos = l.pos + 1
        l.col = l.col + 1
    }
    -- Check for decimal point followed by digit (float)
    let mut is_float = false
    if l.pos + 1 < l.src_len and char_at(l.source, l.pos) == "." and is_digit_char(char_at(l.source, l.pos + 1)) {
        is_float = true
    }
    if is_float {
        l.pos = l.pos + 1
        l.col = l.col + 1
        while l.pos < l.src_len and is_digit_char(char_at(l.source, l.pos)) {
            l.pos = l.pos + 1
            l.col = l.col + 1
        }
    }
    return l
}

-- Read a string literal (assumes pos is on the opening quote)
fn lexer_read_string(lex: Lexer) -> Lexer {
    let mut l = lex
    l.pos = l.pos + 1
    l.col = l.col + 1
    while l.pos < l.src_len and char_at(l.source, l.pos) != "\"" {
        if char_at(l.source, l.pos) == "\\" {
            -- Skip escape sequence
            l.pos = l.pos + 2
            l.col = l.col + 2
        } else {
            l.pos = l.pos + 1
            l.col = l.col + 1
        }
    }
    -- Skip closing quote
    if l.pos < l.src_len {
        l.pos = l.pos + 1
        l.col = l.col + 1
    }
    return l
}

-- Main tokenizer: produce all tokens from source
fn tokenize(source: string) -> List[Token] {
    let mut lex = lexer_new(source)
    let mut tokens: List[Token] = []

    let mut done = false
    while done == false {
        -- Skip whitespace and comments
        lex = lexer_skip_whitespace(lex)

        if lex.pos >= lex.src_len {
            tokens.push(token_new(TK_EOF(), "", lex.line, lex.col))
            done = true
        } else {
            let ch = char_at(lex.source, lex.pos)
            let start_line = lex.line
            let start_col = lex.col
            let start_pos = lex.pos

            if ch == "\n" {
                tokens.push(token_new(TK_NEWLINE(), "\n", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.line = lex.line + 1
                lex.col = 1
            } else if is_alpha_char(ch) {
                -- Identifier or keyword
                lex = lexer_read_identifier(lex)
                let text = substr(lex.source, start_pos, lex.pos - start_pos)
                let kind = keyword_lookup(text)
                tokens.push(token_new(kind, text, start_line, start_col))
            } else if is_digit_char(ch) {
                -- Number
                let before = lex
                lex = lexer_read_number(lex)
                let text = substr(lex.source, start_pos, lex.pos - start_pos)
                if string_contains(text, ".") {
                    tokens.push(token_new(TK_FLOAT(), text, start_line, start_col))
                } else {
                    tokens.push(token_new(TK_INTEGER(), text, start_line, start_col))
                }
            } else if ch == "\"" {
                -- String literal
                lex = lexer_read_string(lex)
                -- Extract string content (without quotes)
                let text = substr(lex.source, start_pos + 1, lex.pos - start_pos - 2)
                tokens.push(token_new(TK_STRING(), text, start_line, start_col))
            } else if ch == "(" {
                tokens.push(token_new(TK_LPAREN(), "(", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == ")" {
                tokens.push(token_new(TK_RPAREN(), ")", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "{" {
                tokens.push(token_new(TK_LBRACE(), "{", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "}" {
                tokens.push(token_new(TK_RBRACE(), "}", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "[" {
                tokens.push(token_new(TK_LBRACKET(), "[", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "]" {
                tokens.push(token_new(TK_RBRACKET(), "]", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "+" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_PLUSEQ(), "+=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_PLUS(), "+", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "-" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_ARROW(), "->", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_MINUSEQ(), "-=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_MINUS(), "-", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "*" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_STAREQ(), "*=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_STAR(), "*", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "/" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_SLASHEQ(), "/=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_SLASH(), "/", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "%" {
                tokens.push(token_new(TK_PERCENT(), "%", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "=" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_EQEQ(), "==", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_FAT_ARROW(), "=>", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_EQ(), "=", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "!" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_BANGEQ(), "!=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_BANG(), "!", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "<" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_LTEQ(), "<=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_LT(), "<", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == ">" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_GTEQ(), ">=", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_GT(), ">", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "|" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ">" {
                    tokens.push(token_new(TK_PIPEGT(), "|>", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_PIPE(), "|", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "&" {
                tokens.push(token_new(TK_AMP(), "&", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "?" {
                tokens.push(token_new(TK_QUESTION(), "?", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "." {
                if lex.pos + 2 < lex.src_len and char_at(lex.source, lex.pos + 1) == "." and char_at(lex.source, lex.pos + 2) == "=" {
                    tokens.push(token_new(TK_DOTDOTEQ(), "..=", start_line, start_col))
                    lex.pos = lex.pos + 3
                    lex.col = lex.col + 3
                } else if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "." {
                    tokens.push(token_new(TK_DOTDOT(), "..", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_DOT(), ".", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == ":" {
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == ":" {
                    tokens.push(token_new(TK_COLONCOLON(), "::", start_line, start_col))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_COLON(), ":", start_line, start_col))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "," {
                tokens.push(token_new(TK_COMMA(), ",", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == ";" {
                tokens.push(token_new(TK_SEMICOLON(), ";", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "@" {
                tokens.push(token_new(TK_AT(), "@", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "#" {
                tokens.push(token_new(TK_HASH(), "#", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else {
                -- Unknown character, skip it
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            }
        }
    }

    return tokens
}
