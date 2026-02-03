module test_lexer

-- Token kind constants (duplicated from token.dm since no multi-file imports yet)
fn TK_EOF() -> int { return 0 }
fn TK_INTEGER() -> int { return 1 }
fn TK_FLOAT() -> int { return 2 }
fn TK_STRING() -> int { return 3 }
fn TK_IDENT() -> int { return 4 }
fn TK_NEWLINE() -> int { return 5 }
fn TK_FN() -> int { return 10 }
fn TK_LET() -> int { return 11 }
fn TK_MUT() -> int { return 12 }
fn TK_IF() -> int { return 13 }
fn TK_ELSE() -> int { return 14 }
fn TK_MATCH() -> int { return 15 }
fn TK_FOR() -> int { return 16 }
fn TK_WHILE() -> int { return 17 }
fn TK_LOOP() -> int { return 18 }
fn TK_BREAK() -> int { return 19 }
fn TK_CONTINUE() -> int { return 20 }
fn TK_RETURN() -> int { return 21 }
fn TK_STRUCT() -> int { return 22 }
fn TK_ENUM() -> int { return 23 }
fn TK_TRAIT() -> int { return 24 }
fn TK_IMPL() -> int { return 25 }
fn TK_IMPORT() -> int { return 26 }
fn TK_MODULE() -> int { return 27 }
fn TK_TRUE() -> int { return 28 }
fn TK_FALSE() -> int { return 29 }
fn TK_AND() -> int { return 30 }
fn TK_OR() -> int { return 31 }
fn TK_NOT() -> int { return 32 }
fn TK_IN() -> int { return 33 }
fn TK_CONST() -> int { return 34 }
fn TK_WITH() -> int { return 35 }
fn TK_SELF() -> int { return 36 }
fn TK_PLUS() -> int { return 50 }
fn TK_MINUS() -> int { return 51 }
fn TK_STAR() -> int { return 52 }
fn TK_SLASH() -> int { return 53 }
fn TK_PERCENT() -> int { return 54 }
fn TK_EQ() -> int { return 55 }
fn TK_EQEQ() -> int { return 56 }
fn TK_BANGEQ() -> int { return 57 }
fn TK_LT() -> int { return 58 }
fn TK_GT() -> int { return 59 }
fn TK_LTEQ() -> int { return 60 }
fn TK_GTEQ() -> int { return 61 }
fn TK_ARROW() -> int { return 62 }
fn TK_FAT_ARROW() -> int { return 63 }
fn TK_QUESTION() -> int { return 64 }
fn TK_PIPE() -> int { return 65 }
fn TK_PIPEGT() -> int { return 66 }
fn TK_AMP() -> int { return 67 }
fn TK_BANG() -> int { return 68 }
fn TK_PLUSEQ() -> int { return 69 }
fn TK_MINUSEQ() -> int { return 70 }
fn TK_STAREQ() -> int { return 71 }
fn TK_SLASHEQ() -> int { return 72 }
fn TK_LPAREN() -> int { return 80 }
fn TK_RPAREN() -> int { return 81 }
fn TK_LBRACE() -> int { return 82 }
fn TK_RBRACE() -> int { return 83 }
fn TK_LBRACKET() -> int { return 84 }
fn TK_RBRACKET() -> int { return 85 }
fn TK_DOT() -> int { return 90 }
fn TK_DOTDOT() -> int { return 91 }
fn TK_DOTDOTEQ() -> int { return 92 }
fn TK_COLON() -> int { return 93 }
fn TK_COLONCOLON() -> int { return 94 }
fn TK_COMMA() -> int { return 95 }
fn TK_UNDERSCORE() -> int { return 96 }
fn TK_SEMICOLON() -> int { return 97 }
fn TK_AT() -> int { return 98 }
fn TK_HASH() -> int { return 99 }

struct Token {
    kind: int,
    value: string,
    line: int,
    col: int
}

fn token_new(kind: int, value: string, line: int, col: int) -> Token {
    return Token { kind: kind, value: value, line: line, col: col }
}

fn keyword_lookup(name: string) -> int {
    if name == "fn" { return TK_FN() }
    if name == "let" { return TK_LET() }
    if name == "mut" { return TK_MUT() }
    if name == "if" { return TK_IF() }
    if name == "else" { return TK_ELSE() }
    if name == "match" { return TK_MATCH() }
    if name == "for" { return TK_FOR() }
    if name == "while" { return TK_WHILE() }
    if name == "loop" { return TK_LOOP() }
    if name == "break" { return TK_BREAK() }
    if name == "continue" { return TK_CONTINUE() }
    if name == "return" { return TK_RETURN() }
    if name == "struct" { return TK_STRUCT() }
    if name == "enum" { return TK_ENUM() }
    if name == "trait" { return TK_TRAIT() }
    if name == "impl" { return TK_IMPL() }
    if name == "import" { return TK_IMPORT() }
    if name == "module" { return TK_MODULE() }
    if name == "true" { return TK_TRUE() }
    if name == "false" { return TK_FALSE() }
    if name == "and" { return TK_AND() }
    if name == "or" { return TK_OR() }
    if name == "not" { return TK_NOT() }
    if name == "in" { return TK_IN() }
    if name == "const" { return TK_CONST() }
    if name == "with" { return TK_WITH() }
    if name == "self" { return TK_SELF() }
    return TK_IDENT()
}

fn token_kind_name(kind: int) -> string {
    if kind == TK_EOF() { return "EOF" }
    if kind == TK_INTEGER() { return "INT" }
    if kind == TK_FLOAT() { return "FLOAT" }
    if kind == TK_STRING() { return "STR" }
    if kind == TK_IDENT() { return "IDENT" }
    if kind == TK_NEWLINE() { return "NL" }
    if kind == TK_FN() { return "fn" }
    if kind == TK_LET() { return "let" }
    if kind == TK_MUT() { return "mut" }
    if kind == TK_IF() { return "if" }
    if kind == TK_ELSE() { return "else" }
    if kind == TK_RETURN() { return "return" }
    if kind == TK_STRUCT() { return "struct" }
    if kind == TK_ENUM() { return "enum" }
    if kind == TK_MODULE() { return "module" }
    if kind == TK_IMPORT() { return "import" }
    if kind == TK_TRUE() { return "true" }
    if kind == TK_FALSE() { return "false" }
    if kind == TK_AND() { return "and" }
    if kind == TK_OR() { return "or" }
    if kind == TK_NOT() { return "not" }
    if kind == TK_IN() { return "in" }
    if kind == TK_FOR() { return "for" }
    if kind == TK_WHILE() { return "while" }
    if kind == TK_MATCH() { return "match" }
    if kind == TK_IMPL() { return "impl" }
    if kind == TK_TRAIT() { return "trait" }
    if kind == TK_PLUS() { return "+" }
    if kind == TK_MINUS() { return "-" }
    if kind == TK_STAR() { return "*" }
    if kind == TK_SLASH() { return "/" }
    if kind == TK_EQ() { return "=" }
    if kind == TK_EQEQ() { return "==" }
    if kind == TK_BANGEQ() { return "!=" }
    if kind == TK_LT() { return "<" }
    if kind == TK_GT() { return ">" }
    if kind == TK_LTEQ() { return "<=" }
    if kind == TK_GTEQ() { return ">=" }
    if kind == TK_ARROW() { return "->" }
    if kind == TK_FAT_ARROW() { return "=>" }
    if kind == TK_LPAREN() { return "(" }
    if kind == TK_RPAREN() { return ")" }
    if kind == TK_LBRACE() { return "{" }
    if kind == TK_RBRACE() { return "}" }
    if kind == TK_LBRACKET() { return "[" }
    if kind == TK_RBRACKET() { return "]" }
    if kind == TK_DOT() { return "." }
    if kind == TK_COLON() { return ":" }
    if kind == TK_COMMA() { return "," }
    if kind == TK_PIPEGT() { return "|>" }
    return "?" + int_to_string(kind)
}

-- Character helpers
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

fn lexer_skip_whitespace(lex: Lexer) -> Lexer {
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

fn lexer_read_identifier(lex: Lexer) -> Lexer {
    let mut l = lex
    while l.pos < l.src_len and is_alnum_char(char_at(l.source, l.pos)) {
        l.pos = l.pos + 1
        l.col = l.col + 1
    }
    return l
}

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

fn lexer_read_string(lex: Lexer) -> Lexer {
    let mut l = lex
    l.pos = l.pos + 1
    l.col = l.col + 1
    while l.pos < l.src_len and char_at(l.source, l.pos) != "\"" {
        if char_at(l.source, l.pos) == "\\" {
            l.pos = l.pos + 2
            l.col = l.col + 2
        } else {
            l.pos = l.pos + 1
            l.col = l.col + 1
        }
    }
    if l.pos < l.src_len {
        l.pos = l.pos + 1
        l.col = l.col + 1
    }
    return l
}

fn tokenize(source: string) -> List[Token] {
    let mut lex = lexer_new(source)
    let mut tokens: List[Token] = []
    let mut done = false

    while done == false {
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
                tokens.push(token_new(TK_NEWLINE(), "", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.line = lex.line + 1
                lex.col = 1
            } else if is_alpha_char(ch) {
                lex = lexer_read_identifier(lex)
                let text = substr(lex.source, start_pos, lex.pos - start_pos)
                let kind = keyword_lookup(text)
                tokens.push(token_new(kind, text, start_line, start_col))
            } else if is_digit_char(ch) {
                lex = lexer_read_number(lex)
                let text = substr(lex.source, start_pos, lex.pos - start_pos)
                if string_contains(text, ".") {
                    tokens.push(token_new(TK_FLOAT(), text, start_line, start_col))
                } else {
                    tokens.push(token_new(TK_INTEGER(), text, start_line, start_col))
                }
            } else if ch == "\"" {
                lex = lexer_read_string(lex)
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
                tokens.push(token_new(TK_STAR(), "*", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "/" {
                tokens.push(token_new(TK_SLASH(), "/", start_line, start_col))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
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
            } else {
                -- Skip unknown
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            }
        }
    }

    return tokens
}

fn main() {
    let source = "module hello\n\nfn main() {\n    let x = 42\n    println(\"Hello, world!\")\n}\n"
    let tokens = tokenize(source)

    println("=== Lexer Test ===")
    println("Source:")
    println(source)
    println("Tokens (" + int_to_string(tokens.len()) + "):")

    let mut i = 0
    for tok in tokens {
        if tok.kind != TK_NEWLINE() {
            let line_str = int_to_string(tok.line)
            let col_str = int_to_string(tok.col)
            let name = token_kind_name(tok.kind)
            if tok.kind == TK_INTEGER() or tok.kind == TK_FLOAT() or tok.kind == TK_STRING() or tok.kind == TK_IDENT() {
                println("  " + line_str + ":" + col_str + " " + name + "(" + tok.value + ")")
            } else {
                println("  " + line_str + ":" + col_str + " " + name)
            }
        }
        i = i + 1
    }
    println("=== PASS ===")
}
