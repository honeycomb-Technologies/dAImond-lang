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
    let mut lex = lexer_new(source)
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
                tokens.push(token_new(keyword_lookup(text), text, sl, sc))
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
                if lex.pos < lex.src_len {
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
                let text = substr(lex.source, sp + 1, lex.pos - sp - 2)
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
                if lex.pos + 1 < lex.src_len and char_at(lex.source, lex.pos + 1) == "=" {
                    tokens.push(token_new(TK_SLASHEQ(), "/=", sl, sc))
                    lex.pos = lex.pos + 2
                    lex.col = lex.col + 2
                } else {
                    tokens.push(token_new(TK_SLASH(), "/", sl, sc))
                    lex.pos = lex.pos + 1
                    lex.col = lex.col + 1
                }
            } else if ch == "%" {
                tokens.push(token_new(TK_PERCENT(), "%", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
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
