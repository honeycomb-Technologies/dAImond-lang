module main

-- ============================================================
-- TOKEN KINDS
-- ============================================================
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
fn TK_REGION() -> int { return 37 }
fn TK_AS() -> int { return 38 }
fn TK_EXTERN() -> int { return 39 }
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
fn TK_FSTRING() -> int { return 100 }
fn TK_COMPTIME() -> int { return 101 }
fn TK_ASYNC() -> int { return 102 }
fn TK_AWAIT() -> int { return 103 }

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
    if name == "region" { return TK_REGION() }
    if name == "as" { return TK_AS() }
    if name == "extern" { return TK_EXTERN() }
    if name == "comptime" { return TK_COMPTIME() }
    if name == "async" { return TK_ASYNC() }
    if name == "await" { return TK_AWAIT() }
    return TK_IDENT()
}

fn token_kind_name(kind: int) -> string {
    if kind == TK_EOF() { return "EOF" }
    if kind == TK_INTEGER() { return "integer" }
    if kind == TK_FLOAT() { return "float" }
    if kind == TK_STRING() { return "string" }
    if kind == TK_IDENT() { return "identifier" }
    if kind == TK_NEWLINE() { return "newline" }
    if kind == TK_FN() { return "'fn'" }
    if kind == TK_LET() { return "'let'" }
    if kind == TK_MUT() { return "'mut'" }
    if kind == TK_IF() { return "'if'" }
    if kind == TK_ELSE() { return "'else'" }
    if kind == TK_RETURN() { return "'return'" }
    if kind == TK_STRUCT() { return "'struct'" }
    if kind == TK_ENUM() { return "'enum'" }
    if kind == TK_MODULE() { return "'module'" }
    if kind == TK_IMPORT() { return "'import'" }
    if kind == TK_FOR() { return "'for'" }
    if kind == TK_WHILE() { return "'while'" }
    if kind == TK_MATCH() { return "'match'" }
    if kind == TK_IMPL() { return "'impl'" }
    if kind == TK_TRAIT() { return "'trait'" }
    if kind == TK_TRUE() { return "'true'" }
    if kind == TK_FALSE() { return "'false'" }
    if kind == TK_PLUS() { return "'+'" }
    if kind == TK_MINUS() { return "'-'" }
    if kind == TK_STAR() { return "'*'" }
    if kind == TK_SLASH() { return "'/'" }
    if kind == TK_EQ() { return "'='" }
    if kind == TK_EQEQ() { return "'=='" }
    if kind == TK_BANGEQ() { return "'!='" }
    if kind == TK_LT() { return "'<'" }
    if kind == TK_GT() { return "'>'" }
    if kind == TK_LTEQ() { return "'<='" }
    if kind == TK_GTEQ() { return "'>='" }
    if kind == TK_ARROW() { return "'->'" }
    if kind == TK_FAT_ARROW() { return "'=>'" }
    if kind == TK_LPAREN() { return "'('" }
    if kind == TK_RPAREN() { return "')'" }
    if kind == TK_LBRACE() { return "'{'" }
    if kind == TK_RBRACE() { return "'}'" }
    if kind == TK_LBRACKET() { return "'['" }
    if kind == TK_RBRACKET() { return "']'" }
    if kind == TK_DOT() { return "'.'" }
    if kind == TK_COLON() { return "':'" }
    if kind == TK_COMMA() { return "','" }
    if kind == TK_REGION() { return "'region'" }
    if kind == TK_AS() { return "'as'" }
    if kind == TK_EXTERN() { return "'extern'" }
    if kind == TK_FSTRING() { return "f-string" }
    if kind == TK_COMPTIME() { return "'comptime'" }
    if kind == TK_ASYNC() { return "'async'" }
    if kind == TK_AWAIT() { return "'await'" }
    return "token(" + int_to_string(kind) + ")"
}


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

fn unescape_string(s: string) -> string {
    -- Quick check: if no backslashes, return as-is
    if string_find(s, "\\") < 0 {
        return s
    }
    let mut result = ""
    let mut i = 0
    let slen = len(s)
    while i < slen {
        let ch = char_at(s, i)
        if ch == "\\" and i + 1 < slen {
            let next = char_at(s, i + 1)
            if next == "n" {
                result = result + "\n"
                i = i + 2
            } else if next == "t" {
                result = result + "\t"
                i = i + 2
            } else if next == "r" {
                result = result + "\r"
                i = i + 2
            } else if next == "\\" {
                result = result + "\\"
                i = i + 2
            } else if next == "\"" {
                result = result + "\""
                i = i + 2
            } else if next == "'" {
                result = result + "'"
                i = i + 2
            } else {
                result = result + ch
                i = i + 1
            }
        } else {
            result = result + ch
            i = i + 1
        }
    }
    return result
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
                    eprintln("Error: unterminated string at line " + int_to_string(sl))
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

-- ============================================================
-- AST Node Definitions for dAImond Stage 4
-- Uses integer kind tags (not tagged unions) following Stage 1 pattern
-- ============================================================

-- ============================================================
-- EXPRESSION KINDS
-- ============================================================
fn EXPR_LITERAL_INT() -> int { return 1 }
fn EXPR_LITERAL_FLOAT() -> int { return 2 }
fn EXPR_LITERAL_STRING() -> int { return 3 }
fn EXPR_LITERAL_BOOL() -> int { return 4 }
fn EXPR_LITERAL_NULL() -> int { return 5 }
fn EXPR_IDENTIFIER() -> int { return 10 }
fn EXPR_BINARY() -> int { return 20 }
fn EXPR_UNARY() -> int { return 21 }
fn EXPR_FIELD_ACCESS() -> int { return 30 }
fn EXPR_INDEX_ACCESS() -> int { return 31 }
fn EXPR_METHOD_CALL() -> int { return 32 }
fn EXPR_FUNCTION_CALL() -> int { return 40 }
fn EXPR_STRUCT_LITERAL() -> int { return 41 }
fn EXPR_ENUM_LITERAL() -> int { return 42 }
fn EXPR_ARRAY_LITERAL() -> int { return 43 }
fn EXPR_IF() -> int { return 50 }
fn EXPR_MATCH() -> int { return 51 }
fn EXPR_BLOCK() -> int { return 52 }
fn EXPR_LAMBDA() -> int { return 60 }
fn EXPR_PIPELINE() -> int { return 70 }
fn EXPR_ERROR_PROPAGATE() -> int { return 71 }
fn EXPR_RANGE() -> int { return 72 }
fn EXPR_CAST() -> int { return 80 }
fn EXPR_STRING_INTERP() -> int { return 90 }
fn EXPR_GROUPED() -> int { return 91 }
fn EXPR_COMPTIME() -> int { return 92 }
fn EXPR_AWAIT() -> int { return 93 }

-- ============================================================
-- STATEMENT KINDS
-- ============================================================
fn STMT_LET() -> int { return 1 }
fn STMT_RETURN() -> int { return 2 }
fn STMT_IF() -> int { return 3 }
fn STMT_MATCH() -> int { return 4 }
fn STMT_FOR() -> int { return 5 }
fn STMT_WHILE() -> int { return 6 }
fn STMT_LOOP() -> int { return 7 }
fn STMT_BREAK() -> int { return 8 }
fn STMT_CONTINUE() -> int { return 9 }
fn STMT_REGION() -> int { return 10 }
fn STMT_EXPRESSION() -> int { return 11 }
fn STMT_ASSIGNMENT() -> int { return 12 }

-- ============================================================
-- DECLARATION KINDS
-- ============================================================
fn DECL_FUNCTION() -> int { return 1 }
fn DECL_STRUCT() -> int { return 2 }
fn DECL_ENUM() -> int { return 3 }
fn DECL_TRAIT() -> int { return 4 }
fn DECL_IMPL() -> int { return 5 }
fn DECL_CONST() -> int { return 6 }

-- ============================================================
-- BINARY OPERATOR KINDS
-- ============================================================
fn BINOP_ADD() -> int { return 1 }
fn BINOP_SUB() -> int { return 2 }
fn BINOP_MUL() -> int { return 3 }
fn BINOP_DIV() -> int { return 4 }
fn BINOP_MOD() -> int { return 5 }
fn BINOP_EQ() -> int { return 10 }
fn BINOP_NE() -> int { return 11 }
fn BINOP_LT() -> int { return 12 }
fn BINOP_LE() -> int { return 13 }
fn BINOP_GT() -> int { return 14 }
fn BINOP_GE() -> int { return 15 }
fn BINOP_AND() -> int { return 20 }
fn BINOP_OR() -> int { return 21 }
fn BINOP_BIT_AND() -> int { return 30 }
fn BINOP_BIT_OR() -> int { return 31 }
fn BINOP_BIT_XOR() -> int { return 32 }
fn BINOP_SHL() -> int { return 33 }
fn BINOP_SHR() -> int { return 34 }

-- ============================================================
-- UNARY OPERATOR KINDS
-- ============================================================
fn UNOP_NEG() -> int { return 1 }
fn UNOP_NOT() -> int { return 2 }
fn UNOP_BIT_NOT() -> int { return 3 }

-- ============================================================
-- ASSIGNMENT OPERATOR KINDS
-- ============================================================
fn ASSIGN_EQ() -> int { return 0 }
fn ASSIGN_ADD() -> int { return 1 }
fn ASSIGN_SUB() -> int { return 2 }
fn ASSIGN_MUL() -> int { return 3 }
fn ASSIGN_DIV() -> int { return 4 }
fn ASSIGN_MOD() -> int { return 5 }

-- ============================================================
-- TYPE EXPRESSION KINDS
-- ============================================================
fn TYPE_NAMED() -> int { return 1 }
fn TYPE_FUNCTION() -> int { return 2 }
fn TYPE_ARRAY() -> int { return 3 }
fn TYPE_POINTER() -> int { return 4 }
fn TYPE_OPTION() -> int { return 5 }
fn TYPE_RESULT() -> int { return 6 }
fn TYPE_TRAIT_OBJECT() -> int { return 7 }
fn TYPE_INFER() -> int { return 8 }

-- ============================================================
-- PATTERN KINDS
-- ============================================================
fn PAT_LITERAL() -> int { return 1 }
fn PAT_IDENTIFIER() -> int { return 2 }
fn PAT_WILDCARD() -> int { return 3 }
fn PAT_ENUM_VARIANT() -> int { return 4 }
fn PAT_STRUCT() -> int { return 5 }

-- ============================================================
-- AST NODE STRUCTS
-- ============================================================

-- Expression node
struct Expr {
    kind: int,
    line: int,
    col: int,
    -- Literal values
    int_val: int,
    float_val: float,
    str_val: string,
    bool_val: bool,
    -- Identifier name
    name: string,
    -- Binary/Unary operator
    op: int,
    -- Children (Box for tree structure)
    left: Box[Expr],
    right: Box[Expr],
    operand: Box[Expr],
    -- Function call / method call
    callee: Box[Expr],
    args: List[Expr],
    -- Field access / index
    object: Box[Expr],
    field: string,
    index: Box[Expr],
    -- Method name
    method: string,
    -- Generic type arguments (as strings)
    generic_args: List[string],
    -- Struct literal
    type_name: string,
    field_names: List[string],
    field_values: List[Expr],
    -- Enum literal
    enum_name: string,
    variant_name: string,
    payload: List[Expr],
    -- Array literal
    elements: List[Expr],
    -- If expression
    condition: Box[Expr],
    then_branch: Box[Expr],
    else_branch: Box[Expr],
    has_else: bool,
    -- Match expression
    scrutinee: Box[Expr],
    match_arms: List[MatchArm],
    -- Block expression
    stmts: List[Stmt],
    result: Box[Expr],
    has_result: bool,
    -- Lambda
    lambda_params: List[LambdaParam],
    lambda_ret_type: string,
    lambda_body: Box[Expr],
    -- Cast target type
    cast_type: string,
    -- String interpolation parts (alternating literal and expr)
    interp_parts: List[Expr],
    interp_is_literal: List[bool],
    -- Pipeline right side
    pipe_right: Box[Expr]
}

fn expr_new(kind: int, line: int, col: int) -> Expr {
    return Expr {
        kind: kind, line: line, col: col,
        int_val: 0, float_val: 0.0, str_val: "", bool_val: false,
        name: "", op: 0,
        left: Box_null(), right: Box_null(), operand: Box_null(),
        callee: Box_null(), args: [],
        object: Box_null(), field: "", index: Box_null(),
        method: "", generic_args: [],
        type_name: "", field_names: [], field_values: [],
        enum_name: "", variant_name: "", payload: [],
        elements: [],
        condition: Box_null(), then_branch: Box_null(), else_branch: Box_null(), has_else: false,
        scrutinee: Box_null(), match_arms: [],
        stmts: [], result: Box_null(), has_result: false,
        lambda_params: [], lambda_ret_type: "", lambda_body: Box_null(),
        cast_type: "",
        interp_parts: [], interp_is_literal: [],
        pipe_right: Box_null()
    }
}

fn expr_int(val: int, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_INT(), line, col)
    e.int_val = val
    return e
}

fn expr_float(val: float, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_FLOAT(), line, col)
    e.float_val = val
    return e
}

fn expr_string(val: string, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_STRING(), line, col)
    e.str_val = val
    return e
}

fn expr_bool(val: bool, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_BOOL(), line, col)
    e.bool_val = val
    return e
}

fn expr_ident(name: string, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_IDENTIFIER(), line, col)
    e.name = name
    return e
}

fn expr_binary(op: int, left: Expr, right: Expr, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_BINARY(), line, col)
    e.op = op
    e.left = Box_new(left)
    e.right = Box_new(right)
    return e
}

fn expr_unary(op: int, operand: Expr, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_UNARY(), line, col)
    e.op = op
    e.operand = Box_new(operand)
    return e
}

fn expr_call(callee: Expr, args: List[Expr], line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_FUNCTION_CALL(), line, col)
    e.callee = Box_new(callee)
    e.args = args
    return e
}

fn expr_method_call(object: Expr, method: string, args: List[Expr], line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_METHOD_CALL(), line, col)
    e.object = Box_new(object)
    e.method = method
    e.args = args
    return e
}

fn expr_field_access(object: Expr, field: string, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_FIELD_ACCESS(), line, col)
    e.object = Box_new(object)
    e.field = field
    return e
}

fn expr_index_access(object: Expr, index: Expr, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_INDEX_ACCESS(), line, col)
    e.object = Box_new(object)
    e.index = Box_new(index)
    return e
}

-- Statement node
struct Stmt {
    kind: int,
    line: int,
    col: int,
    -- Let binding
    let_name: string,
    let_type: string,
    let_value: Box[Expr],
    let_is_mut: bool,
    -- Return
    ret_value: Box[Expr],
    has_ret_value: bool,
    -- If statement
    if_cond: Box[Expr],
    if_then: List[Stmt],
    if_else: List[Stmt],
    has_else_branch: bool,
    -- For loop
    for_var: string,
    for_iter: Box[Expr],
    for_body: List[Stmt],
    -- While loop
    while_cond: Box[Expr],
    while_body: List[Stmt],
    -- Loop
    loop_body: List[Stmt],
    -- Match statement
    match_expr: Box[Expr],
    match_stmt_arms: List[MatchArm],
    -- Region
    region_name: string,
    region_body: List[Stmt],
    -- Expression statement
    expr: Box[Expr],
    -- Assignment
    assign_target: Box[Expr],
    assign_op: int,
    assign_value: Box[Expr]
}

fn stmt_new(kind: int, line: int, col: int) -> Stmt {
    return Stmt {
        kind: kind, line: line, col: col,
        let_name: "", let_type: "", let_value: Box_null(), let_is_mut: false,
        ret_value: Box_null(), has_ret_value: false,
        if_cond: Box_null(), if_then: [], if_else: [], has_else_branch: false,
        for_var: "", for_iter: Box_null(), for_body: [],
        while_cond: Box_null(), while_body: [],
        loop_body: [],
        match_expr: Box_null(), match_stmt_arms: [],
        region_name: "", region_body: [],
        expr: Box_null(),
        assign_target: Box_null(), assign_op: 0, assign_value: Box_null()
    }
}

-- Match arm
struct MatchArm {
    pattern: Pattern,
    body: List[Stmt],
    body_expr: Box[Expr],
    is_expr_body: bool,
    guard: Box[Expr],
    has_guard: bool
}

fn match_arm_new(pattern: Pattern) -> MatchArm {
    return MatchArm {
        pattern: pattern,
        body: [],
        body_expr: Box_null(),
        is_expr_body: false,
        guard: Box_null(),
        has_guard: false
    }
}

-- Lambda parameter
struct LambdaParam {
    name: string,
    type_name: string
}

fn lambda_param_new(name: string, type_name: string) -> LambdaParam {
    return LambdaParam { name: name, type_name: type_name }
}

-- Pattern node
struct Pattern {
    kind: int,
    -- Literal pattern
    literal_expr: Box[Expr],
    -- Identifier pattern
    name: string,
    is_mut: bool,
    -- Enum variant pattern
    enum_type: string,
    variant: string,
    bindings: List[string],
    -- Struct pattern
    struct_type: string,
    field_patterns: List[FieldPattern]
}

fn pattern_new(kind: int) -> Pattern {
    return Pattern {
        kind: kind,
        literal_expr: Box_null(),
        name: "", is_mut: false,
        enum_type: "", variant: "", bindings: [],
        struct_type: "", field_patterns: []
    }
}

fn pattern_wildcard() -> Pattern {
    return pattern_new(PAT_WILDCARD())
}

fn pattern_ident(name: string) -> Pattern {
    let mut p = pattern_new(PAT_IDENTIFIER())
    p.name = name
    return p
}

fn pattern_enum_variant(enum_type: string, variant: string, bindings: List[string]) -> Pattern {
    let mut p = pattern_new(PAT_ENUM_VARIANT())
    p.enum_type = enum_type
    p.variant = variant
    p.bindings = bindings
    return p
}

-- Field pattern for struct destructuring
struct FieldPattern {
    name: string,
    binding: string
}

fn field_pattern_new(name: string, binding: string) -> FieldPattern {
    return FieldPattern { name: name, binding: binding }
}

-- ============================================================
-- DECLARATION STRUCTS
-- ============================================================

-- Function declaration
struct FunctionDecl {
    name: string,
    params: List[FuncParam],
    return_type: string,
    generic_params: List[string],
    body: List[Stmt],
    effects: List[string],
    is_extern: bool,
    is_async: bool,
    is_comptime: bool,
    line: int,
    col: int
}

fn func_decl_new(name: string, line: int, col: int) -> FunctionDecl {
    return FunctionDecl {
        name: name,
        params: [],
        return_type: "void",
        generic_params: [],
        body: [],
        effects: [],
        is_extern: false,
        is_async: false,
        is_comptime: false,
        line: line,
        col: col
    }
}

-- Function parameter
struct FuncParam {
    name: string,
    type_name: string,
    is_mut: bool
}

fn func_param_new(name: string, type_name: string) -> FuncParam {
    return FuncParam { name: name, type_name: type_name, is_mut: false }
}

-- Struct declaration
struct StructDecl {
    name: string,
    fields: List[StructField],
    generic_params: List[string],
    line: int,
    col: int
}

fn struct_decl_new(name: string, line: int, col: int) -> StructDecl {
    return StructDecl { name: name, fields: [], generic_params: [], line: line, col: col }
}

-- Struct field
struct StructField {
    name: string,
    type_name: string
}

fn struct_field_new(name: string, type_name: string) -> StructField {
    return StructField { name: name, type_name: type_name }
}

-- Enum declaration
struct EnumDecl {
    name: string,
    variants: List[EnumVariant],
    generic_params: List[string],
    line: int,
    col: int
}

fn enum_decl_new(name: string, line: int, col: int) -> EnumDecl {
    return EnumDecl { name: name, variants: [], generic_params: [], line: line, col: col }
}

-- Enum variant
struct EnumVariant {
    name: string,
    payload_types: List[string],
    has_payload: bool
}

fn enum_variant_new(name: string) -> EnumVariant {
    return EnumVariant { name: name, payload_types: [], has_payload: false }
}

-- Trait declaration
struct TraitDecl {
    name: string,
    methods: List[FunctionDecl],
    generic_params: List[string],
    line: int,
    col: int
}

fn trait_decl_new(name: string, line: int, col: int) -> TraitDecl {
    return TraitDecl { name: name, methods: [], generic_params: [], line: line, col: col }
}

-- Impl block
struct ImplBlock {
    trait_name: string,
    target_type: string,
    methods: List[FunctionDecl],
    generic_params: List[string],
    line: int,
    col: int
}

fn impl_block_new(target: string, line: int, col: int) -> ImplBlock {
    return ImplBlock {
        trait_name: "", target_type: target, methods: [],
        generic_params: [], line: line, col: col
    }
}

-- Const declaration
struct ConstDecl {
    name: string,
    type_name: string,
    value: Box[Expr],
    is_comptime: bool,
    line: int,
    col: int
}

fn const_decl_new(name: string, line: int, col: int) -> ConstDecl {
    return ConstDecl {
        name: name, type_name: "", value: Box_null(),
        is_comptime: false, line: line, col: col
    }
}

-- Top-level declaration (wraps any of the above)
struct Declaration {
    kind: int,
    func_decl: Box[FunctionDecl],
    struct_decl: Box[StructDecl],
    enum_decl: Box[EnumDecl],
    trait_decl: Box[TraitDecl],
    impl_block: Box[ImplBlock],
    const_decl: Box[ConstDecl]
}

fn decl_function(fd: FunctionDecl) -> Declaration {
    return Declaration {
        kind: DECL_FUNCTION(),
        func_decl: Box_new(fd),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_struct(sd: StructDecl) -> Declaration {
    return Declaration {
        kind: DECL_STRUCT(),
        func_decl: Box_null(),
        struct_decl: Box_new(sd),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_enum(ed: EnumDecl) -> Declaration {
    return Declaration {
        kind: DECL_ENUM(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_new(ed),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_trait(td: TraitDecl) -> Declaration {
    return Declaration {
        kind: DECL_TRAIT(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_new(td),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_impl(ib: ImplBlock) -> Declaration {
    return Declaration {
        kind: DECL_IMPL(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_new(ib),
        const_decl: Box_null()
    }
}

fn decl_const(cd: ConstDecl) -> Declaration {
    return Declaration {
        kind: DECL_CONST(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_new(cd)
    }
}

-- Import declaration
struct ImportDecl {
    path: string,
    line: int,
    col: int
}

fn import_decl_new(path: string, line: int, col: int) -> ImportDecl {
    return ImportDecl { path: path, line: line, col: col }
}

-- Source file (root AST node)
struct SourceFile {
    module_name: string,
    imports: List[ImportDecl],
    declarations: List[Declaration]
}

fn source_file_new(name: string) -> SourceFile {
    return SourceFile { module_name: name, imports: [], declarations: [] }
}

-- ============================================================
-- dAImond Intermediate Representation
-- SSA-based typed IR between AST and LLVM IR
-- Port of stage3/src/ir.zig using integer kind tags
-- ============================================================

-- ============================================================
-- IR TYPE KINDS
-- ============================================================
fn IR_TYPE_I8() -> int { return 1 }
fn IR_TYPE_I16() -> int { return 2 }
fn IR_TYPE_I32() -> int { return 3 }
fn IR_TYPE_I64() -> int { return 4 }
fn IR_TYPE_U8() -> int { return 5 }
fn IR_TYPE_U16() -> int { return 6 }
fn IR_TYPE_U32() -> int { return 7 }
fn IR_TYPE_U64() -> int { return 8 }
fn IR_TYPE_F32() -> int { return 9 }
fn IR_TYPE_F64() -> int { return 10 }
fn IR_TYPE_BOOL() -> int { return 11 }
fn IR_TYPE_VOID() -> int { return 12 }
fn IR_TYPE_STRING() -> int { return 13 }
fn IR_TYPE_NEVER() -> int { return 14 }
fn IR_TYPE_PTR() -> int { return 20 }
fn IR_TYPE_ARRAY() -> int { return 21 }
fn IR_TYPE_STRUCT() -> int { return 30 }
fn IR_TYPE_TAGGED_UNION() -> int { return 31 }
fn IR_TYPE_FN() -> int { return 40 }
fn IR_TYPE_FN_PTR() -> int { return 41 }
fn IR_TYPE_OPTION() -> int { return 50 }
fn IR_TYPE_RESULT() -> int { return 51 }
fn IR_TYPE_FUTURE() -> int { return 52 }
fn IR_TYPE_SLICE() -> int { return 53 }
fn IR_TYPE_VECTOR() -> int { return 60 }

-- SIMD vector element kinds
fn VEC_ELEM_F32() -> int { return 0 }
fn VEC_ELEM_F64() -> int { return 1 }
fn VEC_ELEM_I32() -> int { return 2 }
fn VEC_ELEM_I64() -> int { return 3 }

-- IR Type: represented as a struct with kind tag
struct IRType {
    kind: int,
    name: string,
    -- Struct fields
    fields: List[IRField],
    -- Enum variants
    variants: List[IRVariant],
    -- Function params (type IDs as strings for lookup)
    param_types: List[string],
    ret_type: string,
    -- Inner type for ptr/array/option/future/slice
    inner_type: string,
    -- Error type for result
    err_type: string,
    -- Array size
    array_size: int,
    -- Vector type info
    vec_elem_kind: int,
    vec_lanes: int
}

fn ir_type_new(kind: int) -> IRType {
    return IRType {
        kind: kind, name: "",
        fields: [], variants: [],
        param_types: [], ret_type: "",
        inner_type: "", err_type: "",
        array_size: 0,
        vec_elem_kind: 0, vec_lanes: 0
    }
}

fn ir_type_simple(kind: int) -> IRType {
    return ir_type_new(kind)
}

fn ir_type_named(kind: int, name: string) -> IRType {
    let mut t = ir_type_new(kind)
    t.name = name
    return t
}

fn ir_type_ptr(inner: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_PTR())
    t.inner_type = inner
    return t
}

fn ir_type_array(inner: string, size: int) -> IRType {
    let mut t = ir_type_new(IR_TYPE_ARRAY())
    t.inner_type = inner
    t.array_size = size
    return t
}

fn ir_type_option(inner: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_OPTION())
    t.inner_type = inner
    return t
}

fn ir_type_result(ok: string, err: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_RESULT())
    t.inner_type = ok
    t.err_type = err
    return t
}

fn ir_type_fn(params: List[string], ret: string) -> IRType {
    let mut t = ir_type_new(IR_TYPE_FN())
    t.param_types = params
    t.ret_type = ret
    return t
}

fn ir_type_vector(elem_kind: int, lanes: int) -> IRType {
    let mut t = ir_type_new(IR_TYPE_VECTOR())
    t.vec_elem_kind = elem_kind
    t.vec_lanes = lanes
    return t
}

fn ir_type_name(ty: IRType) -> string {
    if ty.kind == IR_TYPE_I8() { return "i8" }
    if ty.kind == IR_TYPE_I16() { return "i16" }
    if ty.kind == IR_TYPE_I32() { return "i32" }
    if ty.kind == IR_TYPE_I64() { return "i64" }
    if ty.kind == IR_TYPE_U8() { return "u8" }
    if ty.kind == IR_TYPE_U16() { return "u16" }
    if ty.kind == IR_TYPE_U32() { return "u32" }
    if ty.kind == IR_TYPE_U64() { return "u64" }
    if ty.kind == IR_TYPE_F32() { return "f32" }
    if ty.kind == IR_TYPE_F64() { return "f64" }
    if ty.kind == IR_TYPE_BOOL() { return "bool" }
    if ty.kind == IR_TYPE_VOID() { return "void" }
    if ty.kind == IR_TYPE_STRING() { return "string" }
    if ty.kind == IR_TYPE_NEVER() { return "!" }
    if ty.kind == IR_TYPE_PTR() { return "ptr(" + ty.inner_type + ")" }
    if ty.kind == IR_TYPE_ARRAY() { return "[" + ty.inner_type + "; " + int_to_string(ty.array_size) + "]" }
    if ty.kind == IR_TYPE_STRUCT() { return "struct " + ty.name }
    if ty.kind == IR_TYPE_TAGGED_UNION() { return "enum " + ty.name }
    if ty.kind == IR_TYPE_FN() { return "fn(...) -> " + ty.ret_type }
    if ty.kind == IR_TYPE_FN_PTR() { return "fn_ptr(...) -> " + ty.ret_type }
    if ty.kind == IR_TYPE_OPTION() { return "Option[" + ty.inner_type + "]" }
    if ty.kind == IR_TYPE_RESULT() { return "Result[" + ty.inner_type + ", " + ty.err_type + "]" }
    if ty.kind == IR_TYPE_FUTURE() { return "Future[" + ty.inner_type + "]" }
    if ty.kind == IR_TYPE_SLICE() { return "Slice[" + ty.inner_type + "]" }
    if ty.kind == IR_TYPE_VECTOR() { return vec_type_name(ty.vec_elem_kind, ty.vec_lanes) }
    return "unknown"
}

fn vec_type_name(elem_kind: int, lanes: int) -> string {
    if elem_kind == VEC_ELEM_F32() and lanes == 4 { return "f32x4" }
    if elem_kind == VEC_ELEM_F32() and lanes == 8 { return "f32x8" }
    if elem_kind == VEC_ELEM_F64() and lanes == 2 { return "f64x2" }
    if elem_kind == VEC_ELEM_F64() and lanes == 4 { return "f64x4" }
    if elem_kind == VEC_ELEM_I32() and lanes == 4 { return "i32x4" }
    if elem_kind == VEC_ELEM_I32() and lanes == 8 { return "i32x8" }
    if elem_kind == VEC_ELEM_I64() and lanes == 2 { return "i64x2" }
    if elem_kind == VEC_ELEM_I64() and lanes == 4 { return "i64x4" }
    return "vector_unknown"
}

fn vec_elem_is_float(elem_kind: int) -> bool {
    return elem_kind == VEC_ELEM_F32() or elem_kind == VEC_ELEM_F64()
}

-- Struct field in IR
struct IRField {
    name: string,
    type_id: string
}

fn ir_field_new(name: string, type_id: string) -> IRField {
    return IRField { name: name, type_id: type_id }
}

-- Enum variant in IR
struct IRVariant {
    name: string,
    tag: int,
    payload_type: string,
    has_payload: bool
}

fn ir_variant_new(name: string, tag: int) -> IRVariant {
    return IRVariant { name: name, tag: tag, payload_type: "", has_payload: false }
}

-- ============================================================
-- IR VALUE KINDS
-- ============================================================
fn VAL_CONST_INT() -> int { return 1 }
fn VAL_CONST_FLOAT() -> int { return 2 }
fn VAL_CONST_BOOL() -> int { return 3 }
fn VAL_CONST_STRING() -> int { return 4 }
fn VAL_INST_REF() -> int { return 5 }
fn VAL_PARAM_REF() -> int { return 6 }
fn VAL_GLOBAL_REF() -> int { return 7 }
fn VAL_UNDEF() -> int { return 8 }
fn VAL_BREAK_SENTINEL() -> int { return 9 }

struct IRValue {
    kind: int,
    int_val: int,
    float_val: float,
    bool_val: bool,
    str_val: string,
    ref_id: int
}

fn ir_val_int(v: int) -> IRValue {
    return IRValue { kind: VAL_CONST_INT(), int_val: v, float_val: 0.0, bool_val: false, str_val: "", ref_id: 0 }
}

fn ir_val_float(v: float) -> IRValue {
    return IRValue { kind: VAL_CONST_FLOAT(), int_val: 0, float_val: v, bool_val: false, str_val: "", ref_id: 0 }
}

fn ir_val_bool(v: bool) -> IRValue {
    return IRValue { kind: VAL_CONST_BOOL(), int_val: 0, float_val: 0.0, bool_val: v, str_val: "", ref_id: 0 }
}

fn ir_val_string(v: string) -> IRValue {
    return IRValue { kind: VAL_CONST_STRING(), int_val: 0, float_val: 0.0, bool_val: false, str_val: v, ref_id: 0 }
}

fn ir_val_inst(id: int) -> IRValue {
    return IRValue { kind: VAL_INST_REF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: id }
}

fn ir_val_param(id: int) -> IRValue {
    return IRValue { kind: VAL_PARAM_REF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: id }
}

fn ir_val_global(name: string) -> IRValue {
    return IRValue { kind: VAL_GLOBAL_REF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: name, ref_id: 0 }
}

fn ir_val_undef() -> IRValue {
    return IRValue { kind: VAL_UNDEF(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: 0 }
}

fn ir_val_break() -> IRValue {
    return IRValue { kind: VAL_BREAK_SENTINEL(), int_val: 0, float_val: 0.0, bool_val: false, str_val: "", ref_id: 0 }
}

-- ============================================================
-- IR INSTRUCTION OP KINDS
-- ============================================================
fn OP_ADD() -> int { return 1 }
fn OP_SUB() -> int { return 2 }
fn OP_MUL() -> int { return 3 }
fn OP_DIV() -> int { return 4 }
fn OP_MOD() -> int { return 5 }
fn OP_NEG() -> int { return 6 }
fn OP_EQ() -> int { return 10 }
fn OP_NE() -> int { return 11 }
fn OP_LT() -> int { return 12 }
fn OP_LE() -> int { return 13 }
fn OP_GT() -> int { return 14 }
fn OP_GE() -> int { return 15 }
fn OP_LOGICAL_AND() -> int { return 20 }
fn OP_LOGICAL_OR() -> int { return 21 }
fn OP_LOGICAL_NOT() -> int { return 22 }
fn OP_BIT_AND() -> int { return 25 }
fn OP_BIT_OR() -> int { return 26 }
fn OP_BIT_XOR() -> int { return 27 }
fn OP_SHL() -> int { return 28 }
fn OP_SHR() -> int { return 29 }
fn OP_ALLOCA() -> int { return 30 }
fn OP_LOAD() -> int { return 31 }
fn OP_STORE() -> int { return 32 }
fn OP_GEP() -> int { return 33 }
fn OP_EXTRACT_FIELD() -> int { return 34 }
fn OP_INSERT_FIELD() -> int { return 35 }
fn OP_CALL() -> int { return 40 }
fn OP_CALL_PTR() -> int { return 41 }
fn OP_CAST() -> int { return 50 }
fn OP_PHI() -> int { return 51 }
fn OP_STRING_CONCAT() -> int { return 60 }
fn OP_STRING_EQ() -> int { return 61 }
fn OP_STRING_LEN() -> int { return 62 }
fn OP_LIST_NEW() -> int { return 70 }
fn OP_LIST_PUSH() -> int { return 71 }
fn OP_LIST_GET() -> int { return 72 }
fn OP_LIST_LEN() -> int { return 73 }
fn OP_LIST_POP() -> int { return 74 }
fn OP_MAP_NEW() -> int { return 80 }
fn OP_MAP_INSERT() -> int { return 81 }
fn OP_MAP_GET() -> int { return 82 }
fn OP_MAP_CONTAINS() -> int { return 83 }
fn OP_ARENA_CREATE() -> int { return 90 }
fn OP_ARENA_DESTROY() -> int { return 91 }
fn OP_ARENA_ALLOC() -> int { return 92 }
fn OP_SIMD_SPLAT() -> int { return 100 }
fn OP_SIMD_SET() -> int { return 101 }
fn OP_SIMD_ADD() -> int { return 102 }
fn OP_SIMD_SUB() -> int { return 103 }
fn OP_SIMD_MUL() -> int { return 104 }
fn OP_SIMD_DIV() -> int { return 105 }
fn OP_SIMD_EXTRACT() -> int { return 106 }
fn OP_PANIC() -> int { return 110 }

-- IR Instruction
struct IRInst {
    id: int,
    op: int,
    result_type: string,
    has_result: bool,
    -- Binary operands
    lhs: IRValue,
    rhs: IRValue,
    -- Unary operand
    operand: IRValue,
    -- Alloca
    alloc_type: string,
    -- Load
    load_ptr: IRValue,
    load_type: string,
    -- Store
    store_ptr: IRValue,
    store_val: IRValue,
    -- GEP
    gep_base: IRValue,
    gep_indices: List[IRValue],
    -- Extract/Insert field
    field_base: IRValue,
    field_index: int,
    field_value: IRValue,
    field_type: string,
    -- Call
    callee: string,
    call_args: List[IRValue],
    -- Call ptr
    callee_val: IRValue,
    -- Cast
    cast_val: IRValue,
    cast_from: string,
    cast_to: string,
    -- Phi
    phi_entries: List[PhiEntry],
    -- List/Map new
    elem_type: string,
    key_type: string,
    val_type: string,
    -- Map insert
    map_val: IRValue,
    map_key: IRValue,
    map_value: IRValue,
    -- SIMD
    simd_scalar: IRValue,
    simd_vec_type: string,
    simd_elements: List[IRValue],
    simd_vector: IRValue,
    simd_index: IRValue,
    -- Source location
    source_line: int,
    source_col: int
}

fn ir_inst_new(id: int, op: int) -> IRInst {
    let undef = ir_val_undef()
    return IRInst {
        id: id, op: op,
        result_type: "", has_result: false,
        lhs: undef, rhs: undef,
        operand: undef,
        alloc_type: "",
        load_ptr: undef, load_type: "",
        store_ptr: undef, store_val: undef,
        gep_base: undef, gep_indices: [],
        field_base: undef, field_index: 0, field_value: undef, field_type: "",
        callee: "", call_args: [],
        callee_val: undef,
        cast_val: undef, cast_from: "", cast_to: "",
        phi_entries: [],
        elem_type: "", key_type: "", val_type: "",
        map_val: undef, map_key: undef, map_value: undef,
        simd_scalar: undef, simd_vec_type: "", simd_elements: [],
        simd_vector: undef, simd_index: undef,
        source_line: 0, source_col: 0
    }
}

-- Phi entry
struct PhiEntry {
    value: IRValue,
    block_label: string
}

fn phi_entry_new(value: IRValue, label: string) -> PhiEntry {
    return PhiEntry { value: value, block_label: label }
}

-- ============================================================
-- TERMINATOR KINDS
-- ============================================================
fn TERM_BR() -> int { return 1 }
fn TERM_BR_COND() -> int { return 2 }
fn TERM_RET() -> int { return 3 }
fn TERM_RET_VOID() -> int { return 4 }
fn TERM_UNREACHABLE() -> int { return 5 }

struct IRTerminator {
    kind: int,
    -- Branch target
    target: string,
    -- Conditional branch
    cond: IRValue,
    true_label: string,
    false_label: string,
    -- Return value
    ret_val: IRValue
}

fn term_br(target: string) -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_BR(), target: target,
        cond: undef, true_label: "", false_label: "",
        ret_val: undef
    }
}

fn term_br_cond(cond: IRValue, true_label: string, false_label: string) -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_BR_COND(), target: "",
        cond: cond, true_label: true_label, false_label: false_label,
        ret_val: undef
    }
}

fn term_ret(val: IRValue) -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_RET(), target: "",
        cond: undef, true_label: "", false_label: "",
        ret_val: val
    }
}

fn term_ret_void() -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_RET_VOID(), target: "",
        cond: undef, true_label: "", false_label: "",
        ret_val: undef
    }
}

fn term_unreachable() -> IRTerminator {
    let undef = ir_val_undef()
    return IRTerminator {
        kind: TERM_UNREACHABLE(), target: "",
        cond: undef, true_label: "", false_label: "",
        ret_val: undef
    }
}

-- ============================================================
-- BASIC BLOCK
-- ============================================================

struct IRBasicBlock {
    label: string,
    instructions: List[IRInst],
    terminator: Box[IRTerminator],
    has_terminator: bool
}

fn ir_block_new(label: string) -> IRBasicBlock {
    return IRBasicBlock {
        label: label,
        instructions: [],
        terminator: Box_null(),
        has_terminator: false
    }
}

-- ============================================================
-- FUNCTION
-- ============================================================

struct IRParam {
    name: string,
    type_id: string
}

fn ir_param_new(name: string, type_id: string) -> IRParam {
    return IRParam { name: name, type_id: type_id }
}

struct IRFunction {
    name: string,
    params: List[IRParam],
    return_type: string,
    blocks: List[IRBasicBlock],
    is_async: bool,
    is_extern: bool,
    is_comptime: bool
}

fn ir_function_new(name: string, ret_type: string) -> IRFunction {
    return IRFunction {
        name: name, params: [], return_type: ret_type,
        blocks: [],
        is_async: false, is_extern: false, is_comptime: false
    }
}

-- ============================================================
-- MODULE
-- ============================================================

struct IRGlobal {
    name: string,
    type_id: string,
    init_value: IRValue,
    has_init: bool,
    is_const: bool
}

fn ir_global_new(name: string, type_id: string) -> IRGlobal {
    return IRGlobal { name: name, type_id: type_id, init_value: ir_val_undef(), has_init: false, is_const: false }
}

struct IRExternDecl {
    name: string,
    type_id: string
}

fn ir_extern_decl_new(name: string, type_id: string) -> IRExternDecl {
    return IRExternDecl { name: name, type_id: type_id }
}

struct IRModule {
    functions: List[IRFunction],
    globals: List[IRGlobal],
    struct_defs: Map[string, IRType],
    extern_decls: List[IRExternDecl],
    user_extern_fns: Map[string, bool]
}

fn ir_module_new() -> IRModule {
    return IRModule {
        functions: [],
        globals: [],
        struct_defs: Map_new(),
        extern_decls: [],
        user_extern_fns: Map_new()
    }
}


-- ============================================================
-- IR Builder
-- Builds instructions within a current function and basic block
-- Port of stage3/src/ir.zig IRBuilder
-- ============================================================

struct IRBuilder {
    current_function: Box[IRFunction],
    current_block_idx: int,
    next_id: int,
    has_function: bool,
    -- Type registry: maps type name to IRType
    types: Map[string, IRType]
}

fn ir_builder_new() -> IRBuilder {
    return IRBuilder {
        current_function: Box_null(),
        current_block_idx: -1,
        next_id: 0,
        has_function: false,
        types: Map_new()
    }
}

-- Register a type by name
fn builder_register_type(b: IRBuilder, name: string, ty: IRType) -> IRBuilder {
    let mut builder = b
    builder.types.insert(name, ty)
    return builder
}

-- Get a type by name
fn builder_get_type(b: IRBuilder, name: string) -> IRType {
    if b.types.contains(name) {
        return b.types.get(name)
    }
    return ir_type_simple(IR_TYPE_VOID())
}

-- Set current function
fn builder_set_function(b: IRBuilder, func: IRFunction) -> IRBuilder {
    let mut builder = b
    builder.current_function = Box_new(func)
    builder.has_function = true
    builder.next_id = 0
    return builder
}

-- Get the current function back (consumes the box)
fn builder_get_function(b: IRBuilder) -> IRFunction {
    return *b.current_function
}

-- Add a new basic block to the current function
fn builder_add_block(b: IRBuilder, label: string) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks.push(ir_block_new(label))
    builder.current_function = Box_new(func)
    builder.current_block_idx = func.blocks.len() - 1
    return builder
}

-- Set the insert point to a specific block by index
fn builder_set_block(b: IRBuilder, idx: int) -> IRBuilder {
    let mut builder = b
    builder.current_block_idx = idx
    return builder
}

-- Set the insert point to a block by label
fn builder_set_block_by_label(b: IRBuilder, label: string) -> IRBuilder {
    let mut builder = b
    let func = *builder.current_function
    let mut i = 0
    while i < func.blocks.len() {
        if func.blocks[i].label == label {
            builder.current_block_idx = i
            return builder
        }
        i = i + 1
    }
    return builder
}

-- Get current block index
fn builder_current_block(b: IRBuilder) -> int {
    return b.current_block_idx
}

-- Internal: add instruction and return its reference value
fn builder_add_inst(b: IRBuilder, inst: IRInst) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].instructions.push(inst)
    builder.current_function = Box_new(func)
    return builder
}

-- Allocate the next instruction ID
fn builder_next_id(b: IRBuilder) -> int {
    return b.next_id
}

fn builder_advance_id(b: IRBuilder) -> IRBuilder {
    let mut builder = b
    builder.next_id = builder.next_id + 1
    return builder
}

-- ============================================================
-- ARITHMETIC BUILDERS
-- ============================================================

fn builder_build_add(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_ADD())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_sub(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_SUB())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_mul(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MUL())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_div(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_DIV())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_mod(b: IRBuilder, lhs: IRValue, rhs: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MOD())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_neg(b: IRBuilder, operand: IRValue, result_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_NEG())
    inst.operand = operand
    inst.result_type = result_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- COMPARISON BUILDERS
-- ============================================================

fn builder_build_cmp(b: IRBuilder, op: int, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, op)
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = "bool"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_eq(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_EQ(), lhs, rhs)
}

fn builder_build_ne(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_NE(), lhs, rhs)
}

fn builder_build_lt(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_LT(), lhs, rhs)
}

fn builder_build_le(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_LE(), lhs, rhs)
}

fn builder_build_gt(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_GT(), lhs, rhs)
}

fn builder_build_ge(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    return builder_build_cmp(b, OP_GE(), lhs, rhs)
}

-- ============================================================
-- MEMORY BUILDERS
-- ============================================================

fn builder_build_alloca(b: IRBuilder, alloc_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = alloc_type
    inst.result_type = "ptr(" + alloc_type + ")"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_load(b: IRBuilder, ptr: IRValue, load_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LOAD())
    inst.load_ptr = ptr
    inst.load_type = load_type
    inst.result_type = load_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_store(b: IRBuilder, ptr: IRValue, value: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_STORE())
    inst.store_ptr = ptr
    inst.store_val = value
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_extract_field(b: IRBuilder, base: IRValue, field_index: int, field_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
    inst.field_base = base
    inst.field_index = field_index
    inst.field_type = field_type
    inst.result_type = field_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_insert_field(b: IRBuilder, base: IRValue, value: IRValue, field_index: int, struct_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
    inst.field_base = base
    inst.field_value = value
    inst.field_index = field_index
    inst.result_type = struct_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- CALL BUILDERS
-- ============================================================

fn builder_build_call(b: IRBuilder, callee: string, args: List[IRValue], ret_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = callee
    inst.call_args = args
    inst.result_type = ret_type
    inst.has_result = ret_type != "void"
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_call_void(b: IRBuilder, callee: string, args: List[IRValue]) -> IRBuilder {
    return builder_build_call(b, callee, args, "void")
}

fn builder_build_call_ptr(b: IRBuilder, callee: IRValue, args: List[IRValue], ret_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL_PTR())
    inst.callee_val = callee
    inst.call_args = args
    inst.result_type = ret_type
    inst.has_result = ret_type != "void"
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- CAST BUILDER
-- ============================================================

fn builder_build_cast(b: IRBuilder, value: IRValue, from_type: string, to_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = value
    inst.cast_from = from_type
    inst.cast_to = to_type
    inst.result_type = to_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- STRING BUILDERS
-- ============================================================

fn builder_build_string_concat(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_STRING_CONCAT())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = "string"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_string_eq(b: IRBuilder, lhs: IRValue, rhs: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_STRING_EQ())
    inst.lhs = lhs
    inst.rhs = rhs
    inst.result_type = "bool"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- LIST BUILDERS
-- ============================================================

fn builder_build_list_new(b: IRBuilder, elem_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_NEW())
    inst.elem_type = elem_type
    inst.result_type = "list"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_list_push(b: IRBuilder, list: IRValue, elem: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_PUSH())
    inst.lhs = list
    inst.rhs = elem
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_list_get(b: IRBuilder, list: IRValue, index: IRValue, elem_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_GET())
    inst.lhs = list
    inst.rhs = index
    inst.result_type = elem_type
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_list_len(b: IRBuilder, list: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_LEN())
    inst.operand = list
    inst.result_type = "i64"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- MAP BUILDERS
-- ============================================================

fn builder_build_map_new(b: IRBuilder, key_type: string, val_type: string) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MAP_NEW())
    inst.key_type = key_type
    inst.val_type = val_type
    inst.result_type = "map"
    inst.has_result = true
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

fn builder_build_map_insert(b: IRBuilder, map: IRValue, key: IRValue, value: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_MAP_INSERT())
    inst.map_val = map
    inst.map_key = key
    inst.map_value = value
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- ============================================================
-- TERMINATOR BUILDERS
-- ============================================================

fn builder_build_br(b: IRBuilder, target: string) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_br(target))
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

fn builder_build_cond_br(b: IRBuilder, cond: IRValue, true_label: string, false_label: string) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_br_cond(cond, true_label, false_label))
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

fn builder_build_ret(b: IRBuilder, value: IRValue) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_ret(value))
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

fn builder_build_ret_void(b: IRBuilder) -> IRBuilder {
    let mut builder = b
    let mut func = *builder.current_function
    func.blocks[builder.current_block_idx].terminator = Box_new(term_ret_void())
    func.blocks[builder.current_block_idx].has_terminator = true
    builder.current_function = Box_new(func)
    return builder
}

-- ============================================================
-- PANIC BUILDER
-- ============================================================

fn builder_build_panic(b: IRBuilder, msg: IRValue) -> IRBuilder {
    let mut builder = b
    let id = builder.next_id
    let mut inst = ir_inst_new(id, OP_PANIC())
    inst.operand = msg
    inst.has_result = false
    builder = builder_add_inst(builder, inst)
    builder.next_id = id + 1
    return builder
}

-- Get the last instruction ID (the one just added)
fn builder_last_id(b: IRBuilder) -> int {
    return b.next_id - 1
}

-- ============================================================
-- LLVM-C Bridge Extern Declarations
-- All LLVM refs are represented as int (int64_t on C side)
-- Maps to dm_llvm_* functions in llvm_bridge.c
-- ============================================================

-- ============================================================
-- Context
-- ============================================================
extern fn dm_llvm_context_create() -> int
extern fn dm_llvm_context_dispose(ctx: int)
-- ============================================================
-- Module
-- ============================================================
extern fn dm_llvm_module_create(ctx: int, name: string) -> int
extern fn dm_llvm_module_dispose(mod: int)extern fn dm_llvm_module_set_target(mod: int, triple: string)extern fn dm_llvm_module_set_data_layout(mod: int, layout: string)extern fn dm_llvm_module_add_function(mod: int, name: string, fn_type: int) -> int
extern fn dm_llvm_module_get_named_function(mod: int, name: string) -> int
extern fn dm_llvm_module_add_global(mod: int, ty: int, name: string) -> int
extern fn dm_llvm_module_verify(mod: int) -> int
extern fn dm_llvm_module_print_to_string(mod: int) -> int
extern fn dm_llvm_module_print_to_file(mod: int, filename: string) -> int
extern fn dm_llvm_module_write_bitcode(mod: int, filename: string) -> int
extern fn dm_llvm_dispose_message(msg: int)extern fn dm_llvm_run_passes(mod: int, passes: string, tm: int) -> int

-- ============================================================
-- Types
-- ============================================================
extern fn dm_llvm_int1_type(ctx: int) -> int
extern fn dm_llvm_int8_type(ctx: int) -> int
extern fn dm_llvm_int16_type(ctx: int) -> int
extern fn dm_llvm_int32_type(ctx: int) -> int
extern fn dm_llvm_int64_type(ctx: int) -> int
extern fn dm_llvm_float_type(ctx: int) -> int
extern fn dm_llvm_double_type(ctx: int) -> int
extern fn dm_llvm_void_type(ctx: int) -> int
extern fn dm_llvm_pointer_type(ctx: int) -> int
extern fn dm_llvm_array_type(elem: int, count: int) -> int
extern fn dm_llvm_vector_type(elem: int, count: int) -> int
extern fn dm_llvm_struct_create_named(ctx: int, name: string) -> int
extern fn dm_llvm_struct_get_type_at_index(struct_ty: int, index: int) -> int
extern fn dm_llvm_get_type_kind(ty: int) -> int
extern fn dm_llvm_get_int_type_width(ty: int) -> int
extern fn dm_llvm_function_type(ret: int, params: int, param_count: int, is_var_arg: int) -> int

-- ============================================================
-- Values / Constants
-- ============================================================
extern fn dm_llvm_const_int(ty: int, val: int, sign_extend: int) -> int
extern fn dm_llvm_const_real(ty: int, val: float) -> int
extern fn dm_llvm_const_null(ty: int) -> int
extern fn dm_llvm_const_string(ctx: int, str: string, len: int, null_terminate: int) -> int
extern fn dm_llvm_set_initializer(global: int, init: int)extern fn dm_llvm_set_global_constant(global: int, is_const: int)extern fn dm_llvm_set_linkage(global: int, linkage: int)extern fn dm_llvm_get_param(func: int, index: int) -> int
extern fn dm_llvm_set_value_name(val: int, name: string)extern fn dm_llvm_type_of(val: int) -> int
extern fn dm_llvm_get_undef(ty: int) -> int

-- ============================================================
-- Basic Blocks
-- ============================================================
extern fn dm_llvm_append_basic_block(ctx: int, func: int, name: string) -> int

-- ============================================================
-- Builder
-- ============================================================
extern fn dm_llvm_create_builder(ctx: int) -> int
extern fn dm_llvm_dispose_builder(builder: int)extern fn dm_llvm_position_at_end(builder: int, block: int)extern fn dm_llvm_get_insert_block(builder: int) -> int

-- Arithmetic
extern fn dm_llvm_build_add(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_sub(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_mul(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_sdiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_srem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_neg(b: int, val: int, name: string) -> int
extern fn dm_llvm_build_fadd(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fsub(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fmul(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fdiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_frem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fneg(b: int, val: int, name: string) -> int

-- Comparison
extern fn dm_llvm_build_icmp(b: int, pred: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_fcmp(b: int, pred: int, lhs: int, rhs: int, name: string) -> int

-- Memory
extern fn dm_llvm_build_alloca(b: int, ty: int, name: string) -> int
extern fn dm_llvm_build_load2(b: int, ty: int, ptr: int, name: string) -> int
extern fn dm_llvm_build_store(b: int, val: int, ptr: int) -> int
extern fn dm_llvm_build_struct_gep2(b: int, ty: int, ptr: int, idx: int, name: string) -> int
extern fn dm_llvm_build_extract_value(b: int, agg: int, idx: int, name: string) -> int
extern fn dm_llvm_build_insert_value(b: int, agg: int, val: int, idx: int, name: string) -> int

-- Control flow
extern fn dm_llvm_build_br(b: int, dest: int) -> int
extern fn dm_llvm_build_cond_br(b: int, cond: int, then_bb: int, else_bb: int) -> int
extern fn dm_llvm_build_ret(b: int, val: int) -> int
extern fn dm_llvm_build_ret_void(b: int) -> int
extern fn dm_llvm_build_unreachable(b: int) -> int

-- Calls
extern fn dm_llvm_build_call2(b: int, fn_ty: int, func: int, args: int, arg_count: int, name: string) -> int

-- Phi
extern fn dm_llvm_build_phi(b: int, ty: int, name: string) -> int

-- Casts
extern fn dm_llvm_build_int_cast2(b: int, val: int, dest_ty: int, is_signed: int, name: string) -> int
extern fn dm_llvm_build_fp_cast(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_si_to_fp(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_ui_to_fp(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_fp_to_si(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_trunc(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_sext(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_bit_cast(b: int, val: int, dest_ty: int, name: string) -> int
extern fn dm_llvm_build_int_to_ptr(b: int, val: int, dest_ty: int, name: string) -> int

-- Logical
extern fn dm_llvm_build_and(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_or(b: int, lhs: int, rhs: int, name: string) -> int
extern fn dm_llvm_build_not(b: int, val: int, name: string) -> int

-- SIMD / Vector
extern fn dm_llvm_build_insert_element(b: int, vec: int, elem: int, idx: int, name: string) -> int
extern fn dm_llvm_build_extract_element(b: int, vec: int, idx: int, name: string) -> int
extern fn dm_llvm_build_shuffle_vector(b: int, v1: int, v2: int, mask: int, name: string) -> int

-- Global strings
extern fn dm_llvm_build_global_string_ptr(b: int, str: string, name: string) -> int

-- ============================================================
-- Target
-- ============================================================
extern fn dm_llvm_initialize_all_targets()extern fn dm_llvm_initialize_x86()extern fn dm_llvm_initialize_aarch64()extern fn dm_llvm_get_default_target_triple() -> int
extern fn dm_llvm_create_target_machine(triple: string, opt_level: int) -> int
extern fn dm_llvm_dispose_target_machine(tm: int)extern fn dm_llvm_target_machine_emit_to_file(tm: int, mod: int, filename: string, file_type: int) -> int
extern fn dm_llvm_get_target_data_layout(tm: int) -> int

-- ============================================================
-- Enum Constants
-- ============================================================
extern fn dm_llvm_int_eq() -> int
extern fn dm_llvm_int_ne() -> int
extern fn dm_llvm_int_slt() -> int
extern fn dm_llvm_int_sle() -> int
extern fn dm_llvm_int_sgt() -> int
extern fn dm_llvm_int_sge() -> int
extern fn dm_llvm_real_oeq() -> int
extern fn dm_llvm_real_one() -> int
extern fn dm_llvm_real_olt() -> int
extern fn dm_llvm_real_ole() -> int
extern fn dm_llvm_real_ogt() -> int
extern fn dm_llvm_real_oge() -> int
extern fn dm_llvm_external_linkage() -> int
extern fn dm_llvm_internal_linkage() -> int
extern fn dm_llvm_private_linkage() -> int
extern fn dm_llvm_object_file() -> int
extern fn dm_llvm_assembly_file() -> int

-- ============================================================
-- String helpers
-- ============================================================
extern fn dm_llvm_c_str_to_string(c_str_ptr: int) -> string
extern fn dm_llvm_free_c_string(c_str_ptr: int)
-- Buffer-based API (for passing arrays to LLVM without raw pointers)
extern fn dm_llvm_buf_clear()
extern fn dm_llvm_buf_push(ref: int)
extern fn dm_llvm_struct_type_buf(ctx: int, count: int, is_packed: int) -> int
extern fn dm_llvm_struct_set_body_buf(struct_ty: int, is_packed: int)
extern fn dm_llvm_function_type_buf(ret: int, is_var_arg: int) -> int
extern fn dm_llvm_build_call2_buf(b: int, fn_ty: int, func: int, name: string) -> int
extern fn dm_llvm_const_struct_buf(ctx: int, is_packed: int) -> int
extern fn dm_llvm_const_array_buf(elem_ty: int) -> int
extern fn dm_llvm_add_incoming_one(phi: int, val: int, block: int)

-- ============================================================
-- Convenience wrapper functions (dAImond-friendly API)
-- ============================================================

fn llvm_create_module(ctx: int, name: string) -> int {
    return dm_llvm_module_create(ctx, name)
}

fn llvm_init_targets() {
    dm_llvm_initialize_all_targets()
}

fn llvm_get_triple() -> int {
    return dm_llvm_get_default_target_triple()
}

fn llvm_create_tm(triple: string, opt: int) -> int {
    return dm_llvm_create_target_machine(triple, opt)
}

fn llvm_emit_object(tm: int, mod: int, filename: string) -> int {
    return dm_llvm_target_machine_emit_to_file(tm, mod, filename, dm_llvm_object_file())
}

fn llvm_emit_asm(tm: int, mod: int, filename: string) -> int {
    return dm_llvm_target_machine_emit_to_file(tm, mod, filename, dm_llvm_assembly_file())
}


-- ============================================================
-- Parser: Recursive descent + Pratt parser
-- Builds AST nodes from token stream
-- Adapted from Stage 1 parsing patterns
-- ============================================================

struct Parser {
    tokens: List[Token],
    pos: int,
    errors: List[string]
}

fn parser_new(tokens: List[Token]) -> Parser {
    let errs: List[string] = []
    return Parser { tokens: tokens, pos: 0, errors: errs }
}

-- ============================================================
-- PARSER HELPERS
-- ============================================================

fn p_cur(p: Parser) -> Token {
    if p.pos < p.tokens.len() {
        return p.tokens[p.pos]
    }
    return token_new(TK_EOF(), "", 0, 0)
}

fn p_peek(p: Parser) -> int {
    return p_cur(p).kind
}

fn p_advance(p: Parser) -> Parser {
    let mut pp = p
    pp.pos = pp.pos + 1
    return pp
}

fn p_skip_newlines(p: Parser) -> Parser {
    let mut pp = p
    while p_peek(pp) == TK_NEWLINE() {
        pp = p_advance(pp)
    }
    return pp
}

fn p_expect(p: Parser, kind: int) -> Parser {
    let mut pp = p_skip_newlines(p)
    if p_peek(pp) == kind {
        return p_advance(pp)
    }
    let tok = p_cur(pp)
    let msg = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": expected " + token_kind_name(kind) + " but got " + token_kind_name(tok.kind)
    pp.errors.push(msg)
    return pp
}

fn p_error(p: Parser, msg: string) -> Parser {
    let mut pp = p
    let tok = p_cur(pp)
    let full = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": " + msg
    pp.errors.push(full)
    return pp
}

fn p_at(p: Parser, kind: int) -> bool {
    return p_peek(p_skip_newlines(p)) == kind
}

fn p_match(p: Parser, kind: int) -> bool {
    return p_peek(p_skip_newlines(p)) == kind
}

-- ============================================================
-- PARSE RESULT TYPES
-- ============================================================

struct ParseExprResult {
    p: Parser,
    expr: Expr
}

struct ParseStmtResult {
    p: Parser,
    stmt: Stmt
}

struct ParseTypeResult {
    p: Parser,
    type_name: string
}

struct ParseDeclResult {
    p: Parser,
    decl: Declaration
}

-- ============================================================
-- PARSE TYPE EXPRESSION
-- Returns type as a string (e.g., "int", "List[string]", "Option[int]")
-- ============================================================

fn parse_type(p: Parser) -> ParseTypeResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    -- Check for dyn keyword
    if tok.kind == TK_IDENT() and tok.value == "dyn" {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        let inner = parse_type(pp)
        return ParseTypeResult { p: inner.p, type_name: "dyn " + inner.type_name }
    }

    -- Check for fn type
    if tok.kind == TK_FN() {
        pp = p_advance(pp)
        pp = p_expect(pp, TK_LPAREN())
        let mut type_str = "fn("
        let mut first = true
        while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if first == false {
                pp = p_expect(pp, TK_COMMA())
                type_str = type_str + ", "
            }
            first = false
            let param_type = parse_type(pp)
            pp = param_type.p
            type_str = type_str + param_type.type_name
        }
        pp = p_expect(pp, TK_RPAREN())
        type_str = type_str + ")"
        if p_peek(p_skip_newlines(pp)) == TK_ARROW() {
            pp = p_skip_newlines(pp)
            pp = p_advance(pp)
            let ret_type = parse_type(pp)
            pp = ret_type.p
            type_str = type_str + " -> " + ret_type.type_name
        }
        return ParseTypeResult { p: pp, type_name: type_str }
    }

    -- Reference type: &T or &Self
    if tok.kind == TK_AMP() {
        pp = p_advance(pp)
        -- Skip 'mut' if present (&mut T)
        if p_peek(pp) == TK_MUT() {
            pp = p_advance(pp)
        }
        let inner = parse_type(pp)
        -- For the LLVM backend, &T is treated the same as T (pass by pointer)
        return ParseTypeResult { p: inner.p, type_name: inner.type_name }
    }

    -- Fixed-size array type: [T; N]
    if tok.kind == TK_LBRACKET() {
        pp = p_advance(pp)
        let inner = parse_type(pp)
        pp = inner.p
        pp = p_skip_newlines(pp)
        -- Expect semicolon separator
        if p_peek(pp) == TK_SEMICOLON() {
            pp = p_advance(pp)
            pp = p_skip_newlines(pp)
            let size_tok = p_cur(pp)
            pp = p_advance(pp)
            pp = p_skip_newlines(pp)
            pp = p_expect(pp, TK_RBRACKET())
            let type_str = "[" + inner.type_name + "; " + size_tok.value + "]"
            return ParseTypeResult { p: pp, type_name: type_str }
        }
        -- Not a fixed array, might be something else - return what we can
        pp = p_expect(pp, TK_RBRACKET())
        return ParseTypeResult { p: pp, type_name: "[" + inner.type_name + "]" }
    }

    if tok.kind != TK_IDENT() and tok.kind != TK_SELF() {
        return ParseTypeResult { p: pp, type_name: "" }
    }

    let name = tok.value
    pp = p_advance(pp)

    -- Check for generic args [T, U]
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut type_str = name + "["
        let mut first = true
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if first == false {
                pp = p_expect(pp, TK_COMMA())
                type_str = type_str + ", "
            }
            first = false
            let inner = parse_type(pp)
            pp = inner.p
            type_str = type_str + inner.type_name
        }
        pp = p_expect(pp, TK_RBRACKET())
        type_str = type_str + "]"
        return ParseTypeResult { p: pp, type_name: type_str }
    }

    return ParseTypeResult { p: pp, type_name: name }
}

-- ============================================================
-- PARSE EXPRESSIONS (Pratt Parser)
-- ============================================================

fn parse_expr(p: Parser) -> ParseExprResult {
    return parse_expr_pratt(p, 0)
}

fn parse_expr_pratt(p: Parser, min_prec: int) -> ParseExprResult {
    let mut pp = p_skip_newlines(p)
    let mut result = parse_prefix(pp)
    pp = result.p
    let mut left = result.expr

    -- Postfix: ?, field access, index, method call
    let mut cont = true
    while cont {
        pp = p_skip_newlines(pp)
        let kind = p_peek(pp)
        if kind == TK_QUESTION() {
            -- Error propagation: expr?
            let line = p_cur(pp).line
            let col = p_cur(pp).col
            pp = p_advance(pp)
            let mut e = expr_new(EXPR_ERROR_PROPAGATE(), line, col)
            e.operand = Box_new(left)
            left = e
        } else if kind == TK_DOT() {
            pp = p_advance(pp)
            pp = p_skip_newlines(pp)
            let field_tok = p_cur(pp)
            if field_tok.kind == TK_IDENT() {
                pp = p_advance(pp)
                -- Check for method call: obj.method(args)
                if p_peek(pp) == TK_LPAREN() {
                    pp = p_advance(pp)
                    let mut args: List[Expr] = []
                    if p_peek(p_skip_newlines(pp)) != TK_RPAREN() {
                        -- Skip 'mut' keyword before argument
                        if p_peek(p_skip_newlines(pp)) == TK_MUT() {
                            pp = p_skip_newlines(pp)
                            pp = p_advance(pp)
                        }
                        let arg = parse_expr(pp)
                        pp = arg.p
                        args.push(arg.expr)
                        while p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                            pp = p_expect(pp, TK_COMMA())
                            -- Skip 'mut' keyword before argument
                            if p_peek(p_skip_newlines(pp)) == TK_MUT() {
                                pp = p_skip_newlines(pp)
                                pp = p_advance(pp)
                            }
                            let next_arg = parse_expr(pp)
                            pp = next_arg.p
                            args.push(next_arg.expr)
                        }
                    }
                    pp = p_expect(pp, TK_RPAREN())
                    left = expr_method_call(left, field_tok.value, args, field_tok.line, field_tok.col)
                } else {
                    -- Field access: obj.field
                    left = expr_field_access(left, field_tok.value, field_tok.line, field_tok.col)
                }
            } else {
                cont = false
            }
        } else if kind == TK_LBRACKET() {
            -- Index access: obj[idx]
            let line = p_cur(pp).line
            let col = p_cur(pp).col
            pp = p_advance(pp)
            let idx = parse_expr(pp)
            pp = idx.p
            pp = p_expect(pp, TK_RBRACKET())
            left = expr_index_access(left, idx.expr, line, col)
        } else if kind == TK_AS() {
            -- Cast: expr as Type
            let line = p_cur(pp).line
            let col = p_cur(pp).col
            pp = p_advance(pp)
            let ty_result = parse_type(pp)
            pp = ty_result.p
            let mut e = expr_new(EXPR_CAST(), line, col)
            e.operand = Box_new(left)
            e.cast_type = ty_result.type_name
            left = e
        } else {
            cont = false
        }
    }

    -- Infix: binary operators with precedence
    let mut keep_going = true
    while keep_going {
        pp = p_skip_newlines(pp)
        let prec = infix_prec(p_peek(pp))
        if prec < min_prec {
            keep_going = false
        } else {
            let op_tok = p_cur(pp)
            let op_kind = p_peek(pp)

            -- Range operator (.. and ..=)
            if op_kind == TK_DOTDOT() or op_kind == TK_DOTDOTEQ() {
                let range_op = if op_kind == TK_DOTDOTEQ() { BINOP_LE() } else { BINOP_LT() }
                pp = p_advance(pp)
                let right_result = parse_expr_pratt(pp, prec + 1)
                pp = right_result.p
                let mut e = expr_new(EXPR_RANGE(), op_tok.line, op_tok.col)
                e.left = Box_new(left)
                e.right = Box_new(right_result.expr)
                e.op = range_op
                left = e
            -- Pipeline operator
            } else if op_kind == TK_PIPEGT() {
                pp = p_advance(pp)
                let right_result = parse_expr_pratt(pp, prec + 1)
                pp = right_result.p
                let mut e = expr_new(EXPR_PIPELINE(), op_tok.line, op_tok.col)
                e.left = Box_new(left)
                e.pipe_right = Box_new(right_result.expr)
                left = e
            } else {
                let binop = token_to_binop(op_kind)
                if binop >= 0 {
                    pp = p_advance(pp)
                    let right_result = parse_expr_pratt(pp, prec + 1)
                    pp = right_result.p
                    left = expr_binary(binop, left, right_result.expr, op_tok.line, op_tok.col)
                } else {
                    keep_going = false
                }
            }
        }
    }

    return ParseExprResult { p: pp, expr: left }
}

fn infix_prec(kind: int) -> int {
    if kind == TK_DOTDOT() or kind == TK_DOTDOTEQ() { return 0 }
    if kind == TK_OR() { return 1 }
    if kind == TK_AND() { return 2 }
    if kind == TK_EQEQ() or kind == TK_BANGEQ() { return 3 }
    if kind == TK_LT() or kind == TK_GT() or kind == TK_LTEQ() or kind == TK_GTEQ() { return 4 }
    if kind == TK_PIPEGT() { return 5 }
    if kind == TK_PLUS() or kind == TK_MINUS() { return 6 }
    if kind == TK_STAR() or kind == TK_SLASH() or kind == TK_PERCENT() { return 7 }
    return -1
}

fn token_to_binop(kind: int) -> int {
    if kind == TK_PLUS() { return BINOP_ADD() }
    if kind == TK_MINUS() { return BINOP_SUB() }
    if kind == TK_STAR() { return BINOP_MUL() }
    if kind == TK_SLASH() { return BINOP_DIV() }
    if kind == TK_PERCENT() { return BINOP_MOD() }
    if kind == TK_EQEQ() { return BINOP_EQ() }
    if kind == TK_BANGEQ() { return BINOP_NE() }
    if kind == TK_LT() { return BINOP_LT() }
    if kind == TK_LTEQ() { return BINOP_LE() }
    if kind == TK_GT() { return BINOP_GT() }
    if kind == TK_GTEQ() { return BINOP_GE() }
    if kind == TK_AND() { return BINOP_AND() }
    if kind == TK_OR() { return BINOP_OR() }
    return -1
}

-- ============================================================
-- PARSE PREFIX EXPRESSIONS
-- ============================================================

fn parse_prefix(p: Parser) -> ParseExprResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    -- Integer literal
    if tok.kind == TK_INTEGER() {
        pp = p_advance(pp)
        return ParseExprResult { p: pp, expr: expr_int(parse_int(tok.value), tok.line, tok.col) }
    }

    -- Float literal
    if tok.kind == TK_FLOAT() {
        pp = p_advance(pp)
        return ParseExprResult { p: pp, expr: expr_float(parse_float(tok.value), tok.line, tok.col) }
    }

    -- String literal
    if tok.kind == TK_STRING() {
        pp = p_advance(pp)
        return ParseExprResult { p: pp, expr: expr_string(tok.value, tok.line, tok.col) }
    }

    -- Boolean literals
    if tok.kind == TK_TRUE() {
        pp = p_advance(pp)
        return ParseExprResult { p: pp, expr: expr_bool(true, tok.line, tok.col) }
    }
    if tok.kind == TK_FALSE() {
        pp = p_advance(pp)
        return ParseExprResult { p: pp, expr: expr_bool(false, tok.line, tok.col) }
    }

    -- F-string
    if tok.kind == TK_FSTRING() {
        return parse_fstring(pp)
    }

    -- Unary minus
    if tok.kind == TK_MINUS() {
        pp = p_advance(pp)
        let operand = parse_expr_pratt(pp, 8)
        return ParseExprResult { p: operand.p, expr: expr_unary(UNOP_NEG(), operand.expr, tok.line, tok.col) }
    }

    -- Unary not
    if tok.kind == TK_NOT() {
        pp = p_advance(pp)
        let operand = parse_expr_pratt(pp, 8)
        return ParseExprResult { p: operand.p, expr: expr_unary(UNOP_NOT(), operand.expr, tok.line, tok.col) }
    }

    -- Parenthesized expression
    if tok.kind == TK_LPAREN() {
        pp = p_advance(pp)
        let inner = parse_expr(pp)
        pp = inner.p
        pp = p_expect(pp, TK_RPAREN())
        let mut e = expr_new(EXPR_GROUPED(), tok.line, tok.col)
        e.operand = Box_new(inner.expr)
        return ParseExprResult { p: pp, expr: e }
    }

    -- Array literal
    if tok.kind == TK_LBRACKET() {
        return parse_array_literal(pp)
    }

    -- Lambda: |params| body
    if tok.kind == TK_PIPE() {
        return parse_lambda(pp)
    }

    -- If expression
    if tok.kind == TK_IF() {
        return parse_if_expr(pp)
    }

    -- Match expression
    if tok.kind == TK_MATCH() {
        return parse_match_expr(pp)
    }

    -- Comptime expression
    if tok.kind == TK_COMPTIME() {
        pp = p_advance(pp)
        let inner = parse_expr(pp)
        let mut e = expr_new(EXPR_COMPTIME(), tok.line, tok.col)
        e.operand = Box_new(inner.expr)
        return ParseExprResult { p: inner.p, expr: e }
    }

    -- Await expression
    if tok.kind == TK_AWAIT() {
        pp = p_advance(pp)
        let inner = parse_expr(pp)
        let mut e = expr_new(EXPR_AWAIT(), tok.line, tok.col)
        e.operand = Box_new(inner.expr)
        return ParseExprResult { p: inner.p, expr: e }
    }

    -- Block expression: { stmts... }
    if tok.kind == TK_LBRACE() {
        pp = p_advance(pp)
        let body = parse_block_body(pp)
        pp = body.p
        pp = p_expect(pp, TK_RBRACE())
        let mut e = expr_new(EXPR_BLOCK(), tok.line, tok.col)
        e.stmts = body.stmts
        return ParseExprResult { p: pp, expr: e }
    }

    -- Identifier / function call / struct literal / enum literal
    if tok.kind == TK_IDENT() or tok.kind == TK_SELF() {
        return parse_ident_expr(pp)
    }

    -- Unknown token - return a dummy expression
    pp = p_error(pp, "unexpected token: " + token_kind_name(tok.kind))
    pp = p_advance(pp)
    return ParseExprResult { p: pp, expr: expr_int(0, tok.line, tok.col) }
}

-- ============================================================
-- PARSE IDENTIFIER / CALL / STRUCT LITERAL / ENUM VARIANT
-- ============================================================

fn parse_ident_expr(p: Parser) -> ParseExprResult {
    let mut pp = p
    let tok = p_cur(pp)
    let name = tok.value
    pp = p_advance(pp)

    -- Check for generic args: name[T](...)
    let mut generics: List[string] = []
    if p_peek(pp) == TK_LBRACKET() {
        -- Peek ahead to see if this is a generic call or an index
        let saved = pp
        pp = p_advance(pp)
        -- Try to parse as type
        let tr = parse_type(pp)
        if p_peek(tr.p) == TK_RBRACKET() or p_peek(tr.p) == TK_COMMA() {
            -- This is a generic arg list
            generics.push(tr.type_name)
            pp = tr.p
            while p_peek(pp) == TK_COMMA() {
                pp = p_advance(pp)
                let tr2 = parse_type(pp)
                pp = tr2.p
                generics.push(tr2.type_name)
            }
            pp = p_expect(pp, TK_RBRACKET())
        } else {
            -- Not a generic arg, restore and let index access handle it
            pp = saved
        }
    }

    -- Check for function call: name(args)
    if p_peek(pp) == TK_LPAREN() {
        pp = p_advance(pp)
        let mut args: List[Expr] = []
        if p_peek(p_skip_newlines(pp)) != TK_RPAREN() {
            -- Skip 'mut' keyword before argument
            if p_peek(p_skip_newlines(pp)) == TK_MUT() {
                pp = p_skip_newlines(pp)
                pp = p_advance(pp)
            }
            let arg = parse_expr(pp)
            pp = arg.p
            args.push(arg.expr)
            while p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                pp = p_expect(pp, TK_COMMA())
                -- Skip 'mut' keyword before argument
                if p_peek(p_skip_newlines(pp)) == TK_MUT() {
                    pp = p_skip_newlines(pp)
                    pp = p_advance(pp)
                }
                let next_arg = parse_expr(pp)
                pp = next_arg.p
                args.push(next_arg.expr)
            }
        }
        pp = p_expect(pp, TK_RPAREN())
        let mut e = expr_call(expr_ident(name, tok.line, tok.col), args, tok.line, tok.col)
        e.generic_args = generics
        return ParseExprResult { p: pp, expr: e }
    }

    -- Check for struct literal: Name { field: val, ... } or Name { field, ... }
    if p_peek(pp) == TK_LBRACE() {
        -- Only treat as struct literal if first char is uppercase
        if len(name) > 0 and char_at(name, 0) >= "A" and char_at(name, 0) <= "Z" {
            pp = p_advance(pp)
            let mut field_names: List[string] = []
            let mut field_values: List[Expr] = []
            pp = p_skip_newlines(pp)
            while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
                pp = p_skip_newlines(pp)
                let fname = p_cur(pp).value
                pp = p_advance(pp)
                -- Support both explicit (field: expr) and shorthand (field) syntax
                if p_peek(pp) == TK_COLON() {
                    pp = p_advance(pp)
                    let fval = parse_expr(pp)
                    pp = fval.p
                    field_names.push(fname)
                    field_values.push(fval.expr)
                } else {
                    -- Shorthand: field name = variable name (Pair { first, second })
                    field_names.push(fname)
                    field_values.push(expr_ident(fname, tok.line, tok.col))
                }
                pp = p_skip_newlines(pp)
                if p_peek(pp) == TK_COMMA() {
                    pp = p_advance(pp)
                }
            }
            pp = p_expect(pp, TK_RBRACE())
            let mut e = expr_new(EXPR_STRUCT_LITERAL(), tok.line, tok.col)
            e.type_name = name
            e.field_names = field_names
            e.field_values = field_values
            return ParseExprResult { p: pp, expr: e }
        }
    }

    -- Check for enum variant: Name.Variant or Name::Variant
    if p_peek(pp) == TK_DOT() or p_peek(pp) == TK_COLONCOLON() {
        let saved = pp
        pp = p_advance(pp)
        let variant_tok = p_cur(pp)
        if variant_tok.kind == TK_IDENT() and len(variant_tok.value) > 0 and char_at(variant_tok.value, 0) >= "A" and char_at(variant_tok.value, 0) <= "Z" {
            pp = p_advance(pp)
            let mut payload_args: List[Expr] = []
            if p_peek(pp) == TK_LPAREN() {
                pp = p_advance(pp)
                if p_peek(p_skip_newlines(pp)) != TK_RPAREN() {
                    let arg = parse_expr(pp)
                    pp = arg.p
                    payload_args.push(arg.expr)
                    while p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                        pp = p_expect(pp, TK_COMMA())
                        let next_arg = parse_expr(pp)
                        pp = next_arg.p
                        payload_args.push(next_arg.expr)
                    }
                }
                pp = p_expect(pp, TK_RPAREN())
            }
            let mut e = expr_new(EXPR_ENUM_LITERAL(), tok.line, tok.col)
            e.enum_name = name
            e.variant_name = variant_tok.value
            e.payload = payload_args
            return ParseExprResult { p: pp, expr: e }
        } else {
            -- Not an enum variant, restore
            pp = saved
        }
    }

    -- Plain identifier
    let mut ident_expr = expr_ident(name, tok.line, tok.col)
    ident_expr.generic_args = generics
    return ParseExprResult { p: pp, expr: ident_expr }
}

-- ============================================================
-- PARSE SPECIAL EXPRESSIONS
-- ============================================================

fn parse_array_literal(p: Parser) -> ParseExprResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip [
    let mut elems: List[Expr] = []
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RBRACKET() and p_peek(pp) != TK_EOF() {
        let elem = parse_expr(pp)
        pp = elem.p
        elems.push(elem.expr)
        pp = p_skip_newlines(pp)
        -- Check for array repeat syntax: [value; count]
        if p_peek(pp) == TK_SEMICOLON() and elems.len() == 1 {
            pp = p_advance(pp) -- skip ;
            pp = p_skip_newlines(pp)
            let count_result = parse_expr(pp)
            pp = count_result.p
            pp = p_skip_newlines(pp)
            pp = p_expect(pp, TK_RBRACKET())
            -- Expand: repeat the element 'count' times
            let repeat_val = elems[0]
            let mut repeat_count = 0
            if count_result.expr.kind == EXPR_LITERAL_INT() {
                repeat_count = count_result.expr.int_val
            }
            let mut expanded: List[Expr] = []
            let mut ri = 0
            while ri < repeat_count {
                expanded.push(repeat_val)
                ri = ri + 1
            }
            let mut e = expr_new(EXPR_ARRAY_LITERAL(), tok.line, tok.col)
            e.elements = expanded
            return ParseExprResult { p: pp, expr: e }
        }
        if p_peek(pp) == TK_COMMA() {
            pp = p_advance(pp)
        }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RBRACKET())
    let mut e = expr_new(EXPR_ARRAY_LITERAL(), tok.line, tok.col)
    e.elements = elems
    return ParseExprResult { p: pp, expr: e }
}

fn parse_lambda(p: Parser) -> ParseExprResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip |
    let mut params: List[LambdaParam] = []
    while p_peek(pp) != TK_PIPE() and p_peek(pp) != TK_EOF() {
        let name = p_cur(pp).value
        pp = p_advance(pp)
        let mut type_name = ""
        if p_peek(pp) == TK_COLON() {
            pp = p_advance(pp)
            let ty = parse_type(pp)
            pp = ty.p
            type_name = ty.type_name
        }
        params.push(lambda_param_new(name, type_name))
        if p_peek(pp) == TK_COMMA() {
            pp = p_advance(pp)
        }
    }
    pp = p_expect(pp, TK_PIPE())
    -- Optional return type
    let mut ret_type = ""
    if p_peek(p_skip_newlines(pp)) == TK_ARROW() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        let ty = parse_type(pp)
        pp = ty.p
        ret_type = ty.type_name
    }
    -- Body: either block { ... } or single expression
    let body = parse_expr(pp)
    pp = body.p
    let mut e = expr_new(EXPR_LAMBDA(), tok.line, tok.col)
    e.lambda_params = params
    e.lambda_ret_type = ret_type
    e.lambda_body = Box_new(body.expr)
    return ParseExprResult { p: pp, expr: e }
}

fn parse_if_expr(p: Parser) -> ParseExprResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'if'
    let cond = parse_expr(pp)
    pp = cond.p
    pp = p_expect(pp, TK_LBRACE())
    let then_block = parse_block_body(pp)
    pp = then_block.p
    pp = p_expect(pp, TK_RBRACE())

    let mut e = expr_new(EXPR_IF(), tok.line, tok.col)
    e.condition = Box_new(cond.expr)
    let mut then_expr = expr_new(EXPR_BLOCK(), tok.line, tok.col)
    then_expr.stmts = then_block.stmts
    e.then_branch = Box_new(then_expr)
    e.has_else = false

    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_ELSE() {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_IF() {
            -- else if
            let else_if = parse_if_expr(pp)
            pp = else_if.p
            e.else_branch = Box_new(else_if.expr)
            e.has_else = true
        } else {
            pp = p_expect(pp, TK_LBRACE())
            let else_block = parse_block_body(pp)
            pp = else_block.p
            pp = p_expect(pp, TK_RBRACE())
            let mut else_expr = expr_new(EXPR_BLOCK(), tok.line, tok.col)
            else_expr.stmts = else_block.stmts
            e.else_branch = Box_new(else_expr)
            e.has_else = true
        }
    }

    return ParseExprResult { p: pp, expr: e }
}

fn parse_match_expr(p: Parser) -> ParseExprResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'match'
    let scrutinee = parse_expr(pp)
    pp = scrutinee.p
    pp = p_expect(pp, TK_LBRACE())
    let mut arms: List[MatchArm] = []
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        let arm_result = parse_match_arm(pp)
        pp = arm_result.p
        arms.push(arm_result.arm)
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RBRACE())
    let mut e = expr_new(EXPR_MATCH(), tok.line, tok.col)
    e.scrutinee = Box_new(scrutinee.expr)
    e.match_arms = arms
    return ParseExprResult { p: pp, expr: e }
}

struct ParseArmResult {
    p: Parser,
    arm: MatchArm
}

fn parse_match_arm(p: Parser) -> ParseArmResult {
    let mut pp = p_skip_newlines(p)
    let pat = parse_pattern(pp)
    pp = pat.p

    let mut arm = match_arm_new(pat.pattern)

    -- Optional guard: if condition
    if p_peek(p_skip_newlines(pp)) == TK_IF() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        let guard = parse_expr(pp)
        pp = guard.p
        arm.guard = Box_new(guard.expr)
        arm.has_guard = true
    }

    pp = p_expect(pp, TK_FAT_ARROW())
    pp = p_skip_newlines(pp)

    -- Body: block or expression
    if p_peek(pp) == TK_LBRACE() {
        pp = p_advance(pp)
        let block = parse_block_body(pp)
        pp = block.p
        pp = p_expect(pp, TK_RBRACE())
        arm.body = block.stmts
        arm.is_expr_body = false
    } else {
        let body_expr = parse_expr(pp)
        pp = body_expr.p
        arm.body_expr = Box_new(body_expr.expr)
        arm.is_expr_body = true
    }

    -- Skip optional comma/newline separator
    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_COMMA() {
        pp = p_advance(pp)
    }

    return ParseArmResult { p: pp, arm: arm }
}

fn parse_fstring(p: Parser) -> ParseExprResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip FSTRING token
    -- Parse the f-string template into alternating literal/expr parts
    let template = tok.value
    let mut parts: List[Expr] = []
    let mut is_literal: List[bool] = []
    let mut i = 0
    let tlen = len(template)
    let mut current_lit = ""

    while i < tlen {
        let ch = char_at(template, i)
        if ch == "{" {
            -- Emit accumulated literal
            if len(current_lit) > 0 {
                parts.push(expr_string(current_lit, tok.line, tok.col))
                is_literal.push(true)
                current_lit = ""
            }
            -- Find matching }
            i = i + 1
            let start = i
            let mut depth = 1
            while i < tlen and depth > 0 {
                let c2 = char_at(template, i)
                if c2 == "{" { depth = depth + 1 }
                if c2 == "}" { depth = depth - 1 }
                if depth > 0 { i = i + 1 }
            }
            let expr_text = substr(template, start, i - start)
            -- Tokenize and parse the expression
            let expr_tokens = tokenize(expr_text)
            let expr_parser = parser_new(expr_tokens)
            let expr_result = parse_expr(expr_parser)
            parts.push(expr_result.expr)
            is_literal.push(false)
            if i < tlen {
                i = i + 1 -- skip }
            }
        } else {
            current_lit = current_lit + ch
            i = i + 1
        }
    }
    -- Emit trailing literal
    if len(current_lit) > 0 {
        parts.push(expr_string(current_lit, tok.line, tok.col))
        is_literal.push(true)
    }

    let mut e = expr_new(EXPR_STRING_INTERP(), tok.line, tok.col)
    e.interp_parts = parts
    e.interp_is_literal = is_literal
    return ParseExprResult { p: pp, expr: e }
}

-- ============================================================
-- PARSE PATTERNS
-- ============================================================

struct ParsePatternResult {
    p: Parser,
    pattern: Pattern
}

fn parse_pattern(p: Parser) -> ParsePatternResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    -- Wildcard: _
    if tok.kind == TK_UNDERSCORE() or (tok.kind == TK_IDENT() and tok.value == "_") {
        pp = p_advance(pp)
        return ParsePatternResult { p: pp, pattern: pattern_wildcard() }
    }

    -- Integer literal pattern
    if tok.kind == TK_INTEGER() {
        pp = p_advance(pp)
        let mut pat = pattern_new(PAT_LITERAL())
        pat.literal_expr = Box_new(expr_int(parse_int(tok.value), tok.line, tok.col))
        return ParsePatternResult { p: pp, pattern: pat }
    }

    -- String literal pattern
    if tok.kind == TK_STRING() {
        pp = p_advance(pp)
        let mut pat = pattern_new(PAT_LITERAL())
        pat.literal_expr = Box_new(expr_string(tok.value, tok.line, tok.col))
        return ParsePatternResult { p: pp, pattern: pat }
    }

    -- Boolean literal
    if tok.kind == TK_TRUE() {
        pp = p_advance(pp)
        let mut pat = pattern_new(PAT_LITERAL())
        pat.literal_expr = Box_new(expr_bool(true, tok.line, tok.col))
        return ParsePatternResult { p: pp, pattern: pat }
    }
    if tok.kind == TK_FALSE() {
        pp = p_advance(pp)
        let mut pat = pattern_new(PAT_LITERAL())
        pat.literal_expr = Box_new(expr_bool(false, tok.line, tok.col))
        return ParsePatternResult { p: pp, pattern: pat }
    }

    -- Identifier: could be variable binding, enum variant, or bare variant
    if tok.kind == TK_IDENT() {
        let name = tok.value
        pp = p_advance(pp)

        -- Check for enum variant: Name(bindings) or Name.Variant(bindings)
        if p_peek(pp) == TK_LPAREN() {
            -- Enum variant with payload: Some(x), Ok(v), Err(e)
            pp = p_advance(pp)
            let mut bindings: List[string] = []
            while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                pp = p_skip_newlines(pp)
                let bind_tok = p_cur(pp)
                pp = p_advance(pp)
                bindings.push(bind_tok.value)
                if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                    pp = p_advance(pp)
                }
            }
            pp = p_expect(pp, TK_RPAREN())
            return ParsePatternResult { p: pp, pattern: pattern_enum_variant("", name, bindings) }
        }

        if p_peek(pp) == TK_DOT() or p_peek(pp) == TK_COLONCOLON() {
            -- Qualified enum: EnumName.Variant or EnumName::Variant
            pp = p_advance(pp)
            let variant_tok = p_cur(pp)
            pp = p_advance(pp)
            let mut bindings: List[string] = []
            if p_peek(pp) == TK_LPAREN() {
                pp = p_advance(pp)
                while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                    pp = p_skip_newlines(pp)
                    let bind_tok = p_cur(pp)
                    pp = p_advance(pp)
                    bindings.push(bind_tok.value)
                    if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                        pp = p_advance(pp)
                    }
                }
                pp = p_expect(pp, TK_RPAREN())
            }
            return ParsePatternResult { p: pp, pattern: pattern_enum_variant(name, variant_tok.value, bindings) }
        }

        -- Check if uppercase first letter -> bare variant (None, Point, etc.)
        if len(name) > 0 and char_at(name, 0) >= "A" and char_at(name, 0) <= "Z" {
            -- Bare variant without payload (e.g., None in match)
            let empty_bindings: List[string] = []
            return ParsePatternResult { p: pp, pattern: pattern_enum_variant("", name, empty_bindings) }
        }

        -- Variable binding
        return ParsePatternResult { p: pp, pattern: pattern_ident(name) }
    }

    -- Unknown pattern
    pp = p_error(pp, "unexpected pattern token: " + token_kind_name(tok.kind))
    return ParsePatternResult { p: pp, pattern: pattern_wildcard() }
}

-- ============================================================
-- PARSE STATEMENTS
-- ============================================================

struct ParseBlockResult {
    p: Parser,
    stmts: List[Stmt]
}

fn parse_block_body(p: Parser) -> ParseBlockResult {
    let mut pp = p_skip_newlines(p)
    let mut stmts: List[Stmt] = []
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        let result = parse_stmt(pp)
        pp = result.p
        stmts.push(result.stmt)
        pp = p_skip_newlines(pp)
    }
    return ParseBlockResult { p: pp, stmts: stmts }
}

fn parse_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    -- Let binding
    if tok.kind == TK_LET() {
        return parse_let_stmt(pp)
    }

    -- Const binding
    if tok.kind == TK_CONST() {
        return parse_const_stmt(pp)
    }

    -- Return
    if tok.kind == TK_RETURN() {
        return parse_return_stmt(pp)
    }

    -- If
    if tok.kind == TK_IF() {
        return parse_if_stmt(pp)
    }

    -- While
    if tok.kind == TK_WHILE() {
        return parse_while_stmt(pp)
    }

    -- For
    if tok.kind == TK_FOR() {
        return parse_for_stmt(pp)
    }

    -- Loop
    if tok.kind == TK_LOOP() {
        return parse_loop_stmt(pp)
    }

    -- Break
    if tok.kind == TK_BREAK() {
        pp = p_advance(pp)
        let mut s = stmt_new(STMT_BREAK(), tok.line, tok.col)
        return ParseStmtResult { p: pp, stmt: s }
    }

    -- Continue
    if tok.kind == TK_CONTINUE() {
        pp = p_advance(pp)
        let mut s = stmt_new(STMT_CONTINUE(), tok.line, tok.col)
        return ParseStmtResult { p: pp, stmt: s }
    }

    -- Match statement
    if tok.kind == TK_MATCH() {
        let match_result = parse_match_expr(pp)
        let mut s = stmt_new(STMT_MATCH(), tok.line, tok.col)
        s.match_expr = Box_new(match_result.expr)
        return ParseStmtResult { p: match_result.p, stmt: s }
    }

    -- Region
    if tok.kind == TK_REGION() {
        return parse_region_stmt(pp)
    }

    -- Expression statement (may also be assignment)
    let expr_result = parse_expr(pp)
    pp = expr_result.p

    -- Check for assignment: expr = value, expr += value, etc.
    pp = p_skip_newlines(pp)
    let assign_kind = p_peek(pp)
    if assign_kind == TK_EQ() or assign_kind == TK_PLUSEQ() or assign_kind == TK_MINUSEQ() or assign_kind == TK_STAREQ() or assign_kind == TK_SLASHEQ() {
        let assign_op = token_to_assign_op(assign_kind)
        pp = p_advance(pp)
        let val = parse_expr(pp)
        pp = val.p
        let mut s = stmt_new(STMT_ASSIGNMENT(), tok.line, tok.col)
        s.assign_target = Box_new(expr_result.expr)
        s.assign_op = assign_op
        s.assign_value = Box_new(val.expr)
        return ParseStmtResult { p: pp, stmt: s }
    }

    -- Plain expression statement
    let mut s = stmt_new(STMT_EXPRESSION(), tok.line, tok.col)
    s.expr = Box_new(expr_result.expr)
    return ParseStmtResult { p: pp, stmt: s }
}

fn token_to_assign_op(kind: int) -> int {
    if kind == TK_EQ() { return ASSIGN_EQ() }
    if kind == TK_PLUSEQ() { return ASSIGN_ADD() }
    if kind == TK_MINUSEQ() { return ASSIGN_SUB() }
    if kind == TK_STAREQ() { return ASSIGN_MUL() }
    if kind == TK_SLASHEQ() { return ASSIGN_DIV() }
    return ASSIGN_EQ()
}

fn parse_let_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'let'

    let mut is_mut = false
    if p_peek(p_skip_newlines(pp)) == TK_MUT() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        is_mut = true
    }

    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    -- Optional type annotation
    let mut type_name = ""
    if p_peek(pp) == TK_COLON() {
        pp = p_advance(pp)
        let ty = parse_type(pp)
        pp = ty.p
        type_name = ty.type_name
    }

    -- Optional initializer
    let mut s = stmt_new(STMT_LET(), tok.line, tok.col)
    s.let_name = name_tok.value
    s.let_type = type_name
    s.let_is_mut = is_mut

    if p_peek(pp) == TK_EQ() {
        pp = p_advance(pp)
        let val = parse_expr(pp)
        pp = val.p
        s.let_value = Box_new(val.expr)
    }

    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_const_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'const'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)
    pp = p_expect(pp, TK_EQ())
    let val = parse_expr(pp)
    pp = val.p
    let mut s = stmt_new(STMT_LET(), tok.line, tok.col)
    s.let_name = name_tok.value
    s.let_is_mut = false
    s.let_value = Box_new(val.expr)
    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_return_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp)
    let mut s = stmt_new(STMT_RETURN(), tok.line, tok.col)
    -- Check if there's a return value (not followed by newline/rbrace)
    let next = p_peek(pp)
    if next != TK_NEWLINE() and next != TK_RBRACE() and next != TK_EOF() {
        let val = parse_expr(pp)
        pp = val.p
        s.ret_value = Box_new(val.expr)
        s.has_ret_value = true
    }
    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_if_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'if'
    let cond = parse_expr(pp)
    pp = cond.p
    pp = p_expect(pp, TK_LBRACE())
    let then_block = parse_block_body(pp)
    pp = then_block.p
    pp = p_expect(pp, TK_RBRACE())

    let mut s = stmt_new(STMT_IF(), tok.line, tok.col)
    s.if_cond = Box_new(cond.expr)
    s.if_then = then_block.stmts

    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_ELSE() {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        s.has_else_branch = true
        if p_peek(pp) == TK_IF() {
            -- else if: wrap as a single-statement else block
            let elif = parse_if_stmt(pp)
            pp = elif.p
            s.if_else.push(elif.stmt)
        } else {
            pp = p_expect(pp, TK_LBRACE())
            let else_block = parse_block_body(pp)
            pp = else_block.p
            pp = p_expect(pp, TK_RBRACE())
            s.if_else = else_block.stmts
        }
    }

    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_while_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'while'
    let cond = parse_expr(pp)
    pp = cond.p
    pp = p_expect(pp, TK_LBRACE())
    let body = parse_block_body(pp)
    pp = body.p
    pp = p_expect(pp, TK_RBRACE())
    let mut s = stmt_new(STMT_WHILE(), tok.line, tok.col)
    s.while_cond = Box_new(cond.expr)
    s.while_body = body.stmts
    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_for_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'for'
    pp = p_skip_newlines(pp)
    let var_tok = p_cur(pp)
    pp = p_advance(pp)
    pp = p_expect(pp, TK_IN())
    let iter = parse_expr(pp)
    pp = iter.p
    pp = p_expect(pp, TK_LBRACE())
    let body = parse_block_body(pp)
    pp = body.p
    pp = p_expect(pp, TK_RBRACE())
    let mut s = stmt_new(STMT_FOR(), tok.line, tok.col)
    s.for_var = var_tok.value
    s.for_iter = Box_new(iter.expr)
    s.for_body = body.stmts
    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_loop_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'loop'
    pp = p_expect(pp, TK_LBRACE())
    let body = parse_block_body(pp)
    pp = body.p
    pp = p_expect(pp, TK_RBRACE())
    let mut s = stmt_new(STMT_LOOP(), tok.line, tok.col)
    s.loop_body = body.stmts
    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_region_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'region'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)
    pp = p_expect(pp, TK_LBRACE())
    let body = parse_block_body(pp)
    pp = body.p
    pp = p_expect(pp, TK_RBRACE())
    let mut s = stmt_new(STMT_REGION(), tok.line, tok.col)
    s.region_name = name_tok.value
    s.region_body = body.stmts
    return ParseStmtResult { p: pp, stmt: s }
}

-- ============================================================
-- PARSE DECLARATIONS
-- ============================================================

fn parse_source_file(p: Parser) -> SourceFile {
    let mut pp = p_skip_newlines(p)
    let mut sf = source_file_new("")

    -- Optional module declaration
    if p_peek(pp) == TK_MODULE() {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        let name_tok = p_cur(pp)
        sf.module_name = name_tok.value
        pp = p_advance(pp)
    }
    pp = p_skip_newlines(pp)

    -- Imports
    while p_peek(pp) == TK_IMPORT() {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        let mut path = p_cur(pp).value
        pp = p_advance(pp)
        -- Handle dotted paths: import std.io and import foo::bar
        while p_peek(pp) == TK_DOT() or p_peek(pp) == TK_COLONCOLON() {
            pp = p_advance(pp)
            path = path + "." + p_cur(pp).value
            pp = p_advance(pp)
        }
        sf.imports.push(import_decl_new(path, 0, 0))
        pp = p_skip_newlines(pp)
    }

    -- Declarations
    while p_peek(pp) != TK_EOF() {
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_EOF() { break }
        let decl_result = parse_declaration(pp)
        pp = decl_result.p
        sf.declarations.push(decl_result.decl)
    }

    return sf
}

fn parse_declaration(p: Parser) -> ParseDeclResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    if tok.kind == TK_FN() or tok.kind == TK_ASYNC() {
        return parse_fn_decl(pp)
    }
    if tok.kind == TK_EXTERN() {
        return parse_extern_decl(pp)
    }
    if tok.kind == TK_STRUCT() {
        return parse_struct_decl(pp)
    }
    if tok.kind == TK_ENUM() {
        return parse_enum_decl(pp)
    }
    if tok.kind == TK_TRAIT() {
        return parse_trait_decl(pp)
    }
    if tok.kind == TK_IMPL() {
        return parse_impl_decl(pp)
    }
    if tok.kind == TK_CONST() {
        return parse_const_decl(pp)
    }

    -- Unknown declaration - skip
    pp = p_error(pp, "unexpected declaration: " + token_kind_name(tok.kind))
    pp = p_advance(pp)
    -- Return a dummy
    let fd = func_decl_new("__error__", tok.line, tok.col)
    return ParseDeclResult { p: pp, decl: decl_function(fd) }
}

fn parse_fn_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)
    let mut is_async = false

    if tok.kind == TK_ASYNC() {
        is_async = true
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
    }
    pp = p_advance(pp) -- skip 'fn'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut fd = func_decl_new(name_tok.value, tok.line, tok.col)
    fd.is_async = is_async

    -- Generic params [T, U]
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            pp = p_skip_newlines(pp)
            let gp = p_cur(pp)
            pp = p_advance(pp)
            fd.generic_params.push(gp.value)
            if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                pp = p_advance(pp)
            }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    -- Parameters
    pp = p_expect(pp, TK_LPAREN())
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RPAREN() and p_peek(pp) != TK_EOF() {
        let mut is_mut = false
        pp = p_skip_newlines(pp)
        -- Handle bare 'self' or 'mut self'
        if p_peek(pp) == TK_MUT() {
            let next_pp = p_advance(pp)
            if p_peek(p_skip_newlines(next_pp)) == TK_SELF() {
                is_mut = true
                pp = p_skip_newlines(next_pp)
                let mut self_param = func_param_new("self", "Self")
                self_param.is_mut = true
                fd.params.push(self_param)
                pp = p_advance(pp)
                if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                    pp = p_advance(pp)
                }
                pp = p_skip_newlines(pp)
                continue
            }
            is_mut = true
            pp = next_pp
        }
        if p_peek(pp) == TK_SELF() {
            pp = p_advance(pp)
            -- Check if self has explicit type annotation: self: Type
            let mut self_type = "Self"
            if p_peek(pp) == TK_COLON() {
                pp = p_expect(pp, TK_COLON())
                let st = parse_type(pp)
                pp = st.p
                self_type = st.type_name
            }
            let mut self_param = func_param_new("self", self_type)
            self_param.is_mut = is_mut
            fd.params.push(self_param)
            if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                pp = p_advance(pp)
            }
            pp = p_skip_newlines(pp)
            continue
        }

        let pname = p_cur(pp).value
        pp = p_advance(pp)
        pp = p_expect(pp, TK_COLON())
        let pty = parse_type(pp)
        pp = pty.p
        let mut param = func_param_new(pname, pty.type_name)
        param.is_mut = is_mut
        fd.params.push(param)
        if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
            pp = p_advance(pp)
        }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RPAREN())

    -- Return type
    if p_peek(p_skip_newlines(pp)) == TK_ARROW() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        let rty = parse_type(pp)
        pp = rty.p
        fd.return_type = rty.type_name
    }

    -- Effects: with [IO, Console]
    if p_peek(p_skip_newlines(pp)) == TK_WITH() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        pp = p_expect(pp, TK_LBRACKET())
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            pp = p_skip_newlines(pp)
            let eff = p_cur(pp).value
            pp = p_advance(pp)
            fd.effects.push(eff)
            if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                pp = p_advance(pp)
            }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    -- Return type may also come AFTER effects: fn foo() with [...] -> T
    if p_peek(p_skip_newlines(pp)) == TK_ARROW() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        let rty2 = parse_type(pp)
        pp = rty2.p
        fd.return_type = rty2.type_name
    }

    -- Body
    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_LBRACE() {
        pp = p_advance(pp)
        let body = parse_block_body(pp)
        pp = body.p
        pp = p_expect(pp, TK_RBRACE())
        fd.body = body.stmts
    } else if p_peek(pp) == TK_EQ() {
        -- Expression body: fn foo() -> int = 42
        pp = p_advance(pp)
        let expr = parse_expr(pp)
        pp = expr.p
        let mut ret_stmt = stmt_new(STMT_RETURN(), tok.line, tok.col)
        ret_stmt.ret_value = Box_new(expr.expr)
        ret_stmt.has_ret_value = true
        fd.body.push(ret_stmt)
    }

    return ParseDeclResult { p: pp, decl: decl_function(fd) }
}

fn parse_extern_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'extern'
    pp = p_skip_newlines(pp)
    pp = p_advance(pp) -- skip 'fn'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut fd = func_decl_new(name_tok.value, tok.line, tok.col)
    fd.is_extern = true

    -- Parameters
    pp = p_expect(pp, TK_LPAREN())
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RPAREN() and p_peek(pp) != TK_EOF() {
        pp = p_skip_newlines(pp)
        let pname = p_cur(pp).value
        pp = p_advance(pp)
        pp = p_expect(pp, TK_COLON())
        let pty = parse_type(pp)
        pp = pty.p
        fd.params.push(func_param_new(pname, pty.type_name))
        if p_peek(p_skip_newlines(pp)) == TK_COMMA() {
            pp = p_advance(pp)
        }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RPAREN())

    if p_peek(p_skip_newlines(pp)) == TK_ARROW() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        let rty = parse_type(pp)
        pp = rty.p
        fd.return_type = rty.type_name
    }

    return ParseDeclResult { p: pp, decl: decl_function(fd) }
}

fn parse_struct_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'struct'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut sd = struct_decl_new(name_tok.value, tok.line, tok.col)

    -- Generic params
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() {
            pp = p_skip_newlines(pp)
            sd.generic_params.push(p_cur(pp).value)
            pp = p_advance(pp)
            if p_peek(p_skip_newlines(pp)) == TK_COMMA() { pp = p_advance(pp) }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        pp = p_skip_newlines(pp)
        let fname = p_cur(pp).value
        pp = p_advance(pp)
        pp = p_expect(pp, TK_COLON())
        let fty = parse_type(pp)
        pp = fty.p
        sd.fields.push(struct_field_new(fname, fty.type_name))
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_COMMA() { pp = p_advance(pp) }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RBRACE())

    return ParseDeclResult { p: pp, decl: decl_struct(sd) }
}

fn parse_enum_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'enum'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut ed = enum_decl_new(name_tok.value, tok.line, tok.col)

    -- Generic params
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() {
            pp = p_skip_newlines(pp)
            ed.generic_params.push(p_cur(pp).value)
            pp = p_advance(pp)
            if p_peek(p_skip_newlines(pp)) == TK_COMMA() { pp = p_advance(pp) }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        pp = p_skip_newlines(pp)
        let vname = p_cur(pp).value
        pp = p_advance(pp)
        let mut variant = enum_variant_new(vname)
        -- Optional payload: Variant(Type1, Type2)
        if p_peek(pp) == TK_LPAREN() {
            pp = p_advance(pp)
            variant.has_payload = true
            while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                pp = p_skip_newlines(pp)
                let pty = parse_type(pp)
                pp = pty.p
                variant.payload_types.push(pty.type_name)
                if p_peek(p_skip_newlines(pp)) == TK_COMMA() { pp = p_advance(pp) }
            }
            pp = p_expect(pp, TK_RPAREN())
        }
        ed.variants.push(variant)
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_COMMA() { pp = p_advance(pp) }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RBRACE())

    return ParseDeclResult { p: pp, decl: decl_enum(ed) }
}

fn parse_trait_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'trait'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut td = trait_decl_new(name_tok.value, tok.line, tok.col)

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_FN() {
            let method_result = parse_fn_decl(pp)
            pp = method_result.p
            td.methods.push(*method_result.decl.func_decl)
        } else {
            pp = p_advance(pp)
        }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RBRACE())

    return ParseDeclResult { p: pp, decl: decl_trait(td) }
}

fn parse_impl_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'impl'
    pp = p_skip_newlines(pp)

    -- Parse: impl Type { ... } or impl Type[A, B] { ... }
    --    or impl Trait for Type { ... }
    let first_name = p_cur(pp).value
    pp = p_advance(pp)

    let mut ib = impl_block_new(first_name, tok.line, tok.col)

    -- Parse optional generic params [A, B] after the type name
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            pp = p_skip_newlines(pp)
            ib.generic_params.push(p_cur(pp).value)
            pp = p_advance(pp)
            if p_peek(p_skip_newlines(pp)) == TK_COMMA() { pp = p_advance(pp) }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_FOR() {
        -- impl Trait for Type
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        let target_name = p_cur(pp).value
        pp = p_advance(pp)
        ib.trait_name = first_name
        ib.target_type = target_name
        -- Parse optional generic params on the target type too
        if p_peek(pp) == TK_LBRACKET() {
            pp = p_advance(pp)
            while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                pp = p_skip_newlines(pp)
                ib.generic_params.push(p_cur(pp).value)
                pp = p_advance(pp)
                if p_peek(p_skip_newlines(pp)) == TK_COMMA() { pp = p_advance(pp) }
            }
            pp = p_expect(pp, TK_RBRACKET())
        }
    }

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_FN() or p_peek(pp) == TK_ASYNC() {
            let method_result = parse_fn_decl(pp)
            pp = method_result.p
            ib.methods.push(*method_result.decl.func_decl)
        } else {
            pp = p_advance(pp)
        }
        pp = p_skip_newlines(pp)
    }
    pp = p_expect(pp, TK_RBRACE())

    return ParseDeclResult { p: pp, decl: decl_impl(ib) }
}

fn parse_const_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'const'
    pp = p_skip_newlines(pp)
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut cd = const_decl_new(name_tok.value, tok.line, tok.col)

    -- Check for comptime: const FOO = comptime expr
    pp = p_expect(pp, TK_EQ())
    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_COMPTIME() {
        cd.is_comptime = true
        pp = p_advance(pp)
    }
    let val = parse_expr(pp)
    pp = val.p
    cd.value = Box_new(val.expr)

    return ParseDeclResult { p: pp, decl: decl_const(cd) }
}

-- ============================================================
-- TOP-LEVEL PARSE FUNCTION
-- ============================================================

fn parse(source: string) -> SourceFile {
    let tokens = tokenize(source)
    let p = parser_new(tokens)
    return parse_source_file(p)
}


-- ============================================================
-- IR Generator Core
-- Central struct and initialization, type mapping
-- Port of stage3/src/ir_gen.zig IRGenerator struct
-- ============================================================

-- Struct info for tracking registered structs
struct StructInfo {
    name: string,
    field_names: List[string],
    field_types: List[string]
}

fn struct_info_new(name: string) -> StructInfo {
    return StructInfo { name: name, field_names: [], field_types: [] }
}

-- Enum info for tracking registered enums
struct EnumInfo {
    name: string,
    variant_names: List[string],
    variant_tags: List[int],
    variant_payload_types: List[string],
    variant_has_payload: List[bool]
}

fn enum_info_new(name: string) -> EnumInfo {
    return EnumInfo {
        name: name, variant_names: [], variant_tags: [],
        variant_payload_types: [], variant_has_payload: []
    }
}

-- List element kind for typed list operations
fn LIST_ELEM_INT() -> int { return 0 }
fn LIST_ELEM_FLOAT() -> int { return 1 }
fn LIST_ELEM_STRING() -> int { return 2 }
fn LIST_ELEM_OTHER() -> int { return 3 }

-- Map key/value type combinations
fn MAP_KIND_STRING_INT() -> int { return 0 }
fn MAP_KIND_INT_STRING() -> int { return 1 }

-- ============================================================
-- IR GENERATOR STRUCT
-- ============================================================

struct IRGenerator {
    -- IR module being built
    ir_mod: IRModule,
    -- IR builder for current function
    builder: IRBuilder,
    -- Variable name -> alloca instruction ID
    variable_map: Map[string, int],
    -- Variable name -> type name
    variable_types: Map[string, string],
    -- Struct name -> StructInfo
    struct_defs: Map[string, StructInfo],
    -- Variable name -> list element kind
    list_elem_kinds: Map[string, int],
    -- Variable name -> list element type name (for struct-typed lists)
    list_elem_types: Map[string, string],
    -- Generic function name -> declaration index in source
    generic_fn_decls: Map[string, int],
    -- Mangled monomorphization name -> generated flag
    mono_generated: Map[string, bool],
    -- Type param name -> concrete type name
    type_substitutions: Map[string, string],
    -- Variable name -> lambda function name
    fn_ptr_vars: Map[string, string],
    -- Lambda name -> pipe-delimited capture list
    lambda_captures: Map[string, string],
    -- Method name -> struct type name
    method_struct_map: Map[string, string],
    -- Variable name -> struct type name
    var_struct_types: Map[string, string],
    -- Function name -> pipe-delimited mut param flags
    mut_param_fns: Map[string, string],
    -- Enum name -> EnumInfo
    enum_defs: Map[string, EnumInfo],
    -- Variable name -> enum type name
    var_enum_types: Map[string, string],
    -- Variable name -> map kind
    map_var_kinds: Map[string, int],
    -- Variable name -> box inner type name
    box_inner_types: Map[string, string],
    -- Variable name -> concrete struct type for dyn Trait
    dyn_var_concrete: Map[string, string],
    -- Variable name -> trait name for dyn Trait
    dyn_var_traits: Map[string, string],
    -- "StructName:TraitName" -> pipe-delimited method list
    trait_impl_methods: Map[string, string],
    -- User extern function names
    user_extern_fns: Map[string, bool],
    -- "StructName.field" -> list elem kind
    field_list_elem_kinds: Map[string, int],
    -- "StructName.field" -> list elem type name
    field_list_elem_types: Map[string, string],
    -- Variable name -> true if it's a mut pointer (e.g. mut self)
    mut_ptr_vars: Map[string, bool],
    -- Variable name -> param index for mut pointer params (no alloca created)
    param_ptr_map: Map[string, int],
    -- Label counter for unique block names
    label_counter: int,
    -- Lambda counter
    lambda_counter: int,
    -- Break/continue targets
    break_target: string,
    continue_target: string,
    has_break_target: bool,
    has_continue_target: bool,
    -- Current return type name
    current_return_type: string,
    -- Current impl type name
    current_impl_type: string,
    has_impl_type: bool,
    -- Source declarations for comptime resolution
    declarations: List[Declaration],
    -- Completed blocks that need to be added to the function
    completed_blocks: List[IRBasicBlock],
    -- Known user-defined function names (for undefined function detection)
    known_functions: Map[string, bool]
}

fn ir_generator_new() -> IRGenerator {
    return IRGenerator {
        ir_mod: ir_module_new(),
        builder: ir_builder_new(),
        variable_map: Map_new(),
        variable_types: Map_new(),
        struct_defs: Map_new(),
        list_elem_kinds: Map_new(),
        list_elem_types: Map_new(),
        generic_fn_decls: Map_new(),
        mono_generated: Map_new(),
        type_substitutions: Map_new(),
        fn_ptr_vars: Map_new(),
        lambda_captures: Map_new(),
        method_struct_map: Map_new(),
        var_struct_types: Map_new(),
        mut_param_fns: Map_new(),
        enum_defs: Map_new(),
        var_enum_types: Map_new(),
        map_var_kinds: Map_new(),
        box_inner_types: Map_new(),
        dyn_var_concrete: Map_new(),
        dyn_var_traits: Map_new(),
        trait_impl_methods: Map_new(),
        user_extern_fns: Map_new(),
        field_list_elem_kinds: Map_new(),
        field_list_elem_types: Map_new(),
        mut_ptr_vars: Map_new(),
        param_ptr_map: Map_new(),
        label_counter: 0,
        lambda_counter: 0,
        break_target: "",
        continue_target: "",
        has_break_target: false,
        has_continue_target: false,
        current_return_type: "void",
        current_impl_type: "",
        has_impl_type: false,
        declarations: [],
        completed_blocks: [],
        known_functions: Map_new()
    }
}

-- ============================================================
-- LABEL GENERATION
-- ============================================================

fn gen_next_label(gen: IRGenerator, prefix: string) -> string {
    return prefix + "_" + int_to_string(gen.label_counter)
}

fn gen_advance_label(gen: IRGenerator) -> IRGenerator {
    let mut g = gen
    g.label_counter = g.label_counter + 1
    return g
}

fn gen_next_lambda_name(gen: IRGenerator) -> string {
    return "__lambda_" + int_to_string(gen.lambda_counter)
}

fn gen_advance_lambda(gen: IRGenerator) -> IRGenerator {
    let mut g = gen
    g.lambda_counter = g.lambda_counter + 1
    return g
}

-- ============================================================
-- TYPE MAPPING: AST type names -> IR type names
-- ============================================================

fn map_type_name(name: string) -> string {
    -- Primitive types
    if name == "int" { return "i64" }
    if name == "i8" { return "i8" }
    if name == "i16" { return "i16" }
    if name == "i32" { return "i32" }
    if name == "i64" { return "i64" }
    if name == "u8" { return "u8" }
    if name == "u16" { return "u16" }
    if name == "u32" { return "u32" }
    if name == "u64" { return "u64" }
    if name == "float" { return "f64" }
    if name == "f32" { return "f32" }
    if name == "f64" { return "f64" }
    if name == "bool" { return "bool" }
    if name == "string" { return "string" }
    if name == "void" { return "void" }
    -- SIMD types
    if name == "f32x4" { return "f32x4" }
    if name == "f32x8" { return "f32x8" }
    if name == "f64x2" { return "f64x2" }
    if name == "f64x4" { return "f64x4" }
    if name == "i32x4" { return "i32x4" }
    if name == "i32x8" { return "i32x8" }
    if name == "i64x2" { return "i64x2" }
    if name == "i64x4" { return "i64x4" }
    -- Function pointer type: fn(int, int) -> int becomes ptr
    if starts_with(name, "fn(") {
        return "ptr"
    }
    -- Default: return as-is (struct name, enum name, etc.)
    return name
}

fn is_integer_type(name: string) -> bool {
    if name == "i8" or name == "i16" or name == "i32" or name == "i64" { return true }
    if name == "u8" or name == "u16" or name == "u32" or name == "u64" { return true }
    return false
}

fn is_float_type(name: string) -> bool {
    return name == "f32" or name == "f64"
}

fn is_signed_type(name: string) -> bool {
    return name == "i8" or name == "i16" or name == "i32" or name == "i64"
}

fn is_unsigned_type(name: string) -> bool {
    return name == "u8" or name == "u16" or name == "u32" or name == "u64"
}

fn type_bit_width(name: string) -> int {
    if name == "i8" or name == "u8" { return 8 }
    if name == "i16" or name == "u16" { return 16 }
    if name == "i32" or name == "u32" { return 32 }
    if name == "i64" or name == "u64" { return 64 }
    if name == "f32" { return 32 }
    if name == "f64" { return 64 }
    return 64
}

fn is_simd_type(name: string) -> bool {
    if name == "f32x4" or name == "f32x8" { return true }
    if name == "f64x2" or name == "f64x4" { return true }
    if name == "i32x4" or name == "i32x8" { return true }
    if name == "i64x2" or name == "i64x4" { return true }
    return false
}

-- ============================================================
-- DEFAULT VALUE FOR TYPE
-- ============================================================

fn default_value_for_type(type_name: string) -> IRValue {
    if type_name == "i64" or type_name == "i32" or type_name == "i16" or type_name == "i8" {
        return ir_val_int(0)
    }
    if type_name == "u64" or type_name == "u32" or type_name == "u16" or type_name == "u8" {
        return ir_val_int(0)
    }
    if type_name == "f64" or type_name == "f32" {
        return ir_val_float(0.0)
    }
    if type_name == "bool" {
        return ir_val_bool(false)
    }
    if type_name == "string" {
        return ir_val_string("")
    }
    return ir_val_int(0)
}

-- ============================================================
-- PARSE TYPE STRING
-- Handles composite types like List[int], Option[string], etc.
-- Returns the IR type name
-- ============================================================

fn parse_type_to_ir(type_str: string) -> string {
    -- Check for generic types
    if starts_with(type_str, "List[") {
        return "list"
    }
    if starts_with(type_str, "Map[") {
        return "map"
    }
    if starts_with(type_str, "Box[") {
        return "ptr"
    }
    if starts_with(type_str, "Option[") {
        return "option"
    }
    if starts_with(type_str, "Result[") {
        return "result"
    }
    if starts_with(type_str, "Future[") {
        return "future"
    }
    if starts_with(type_str, "dyn ") {
        return "dyn"
    }
    return map_type_name(type_str)
}

-- Extract inner type from "List[T]", "Option[T]", "Box[T]", etc.
fn extract_inner_type(type_str: string, prefix: string) -> string {
    let prefix_len = len(prefix)
    let total_len = len(type_str)
    if total_len <= prefix_len + 1 {
        return "i64"
    }
    -- Extract between prefix and closing ]
    return substr(type_str, prefix_len, total_len - prefix_len - 1)
}

-- Determine list element kind from type name
fn classify_list_elem(elem_type: string) -> int {
    if elem_type == "int" or elem_type == "i64" { return LIST_ELEM_INT() }
    if elem_type == "float" or elem_type == "f64" { return LIST_ELEM_FLOAT() }
    if elem_type == "string" or elem_type == "String" { return LIST_ELEM_STRING() }
    return LIST_ELEM_OTHER()
}

-- ============================================================
-- BUILTIN NAME MAPPING
-- Maps dAImond builtin names to C runtime function names
-- ============================================================

fn map_builtin_name(name: string) -> string {
    if name == "print" { return "dm_print" }
    if name == "println" { return "dm_println" }
    if name == "eprint" { return "dm_eprint" }
    if name == "eprintln" { return "dm_eprintln" }
    if name == "panic" { return "dm_panic" }
    if name == "exit" { return "exit" }
    if name == "len" { return "dm_string_len" }
    if name == "char_at" { return "dm_char_at" }
    if name == "substr" { return "dm_substr" }
    if name == "int_to_string" { return "dm_int_to_string" }
    if name == "float_to_string" { return "dm_float_to_string" }
    if name == "bool_to_string" { return "dm_bool_to_string" }
    if name == "parse_int" { return "dm_parse_int" }
    if name == "parse_float" { return "dm_parse_float" }
    if name == "string_contains" { return "dm_string_contains" }
    if name == "string_find" { return "dm_string_find" }
    if name == "starts_with" { return "dm_starts_with" }
    if name == "ends_with" { return "dm_ends_with" }
    if name == "string_trim" { return "dm_string_trim" }
    if name == "string_replace" { return "dm_string_replace" }
    if name == "string_to_upper" { return "dm_string_to_upper" }
    if name == "string_to_lower" { return "dm_string_to_lower" }
    if name == "string_split" { return "dm_string_split" }
    if name == "file_read" { return "dm_file_read" }
    if name == "file_write" { return "dm_file_write" }
    if name == "file_append" { return "dm_file_append" }
    if name == "file_exists" { return "dm_file_exists" }
    if name == "read_line" { return "dm_read_line" }
    if name == "args_len" { return "dm_args_len" }
    if name == "args_get" { return "dm_args_get" }
    if name == "system" { return "dm_system" }
    if name == "assert" { return "dm_assert" }
    if name == "assert_eq" { return "dm_assert_eq" }
    -- Character classification
    if name == "is_alpha" { return "dm_is_alpha" }
    if name == "is_digit" { return "dm_is_digit" }
    if name == "is_whitespace" { return "dm_is_whitespace" }
    if name == "is_alnum" { return "dm_is_alnum" }
    -- Character/string conversion
    if name == "char_to_string" { return "dm_char_to_string" }
    if name == "string_to_int" { return "dm_parse_int" }
    -- Path utilities
    if name == "path_dirname" { return "dm_path_dirname" }
    if name == "path_basename" { return "dm_path_basename" }
    if name == "path_extension" { return "dm_path_extension" }
    if name == "path_stem" { return "dm_path_stem" }
    if name == "path_join" { return "dm_path_join" }
    -- Filesystem builtins
    if name == "fs_getcwd" { return "dm_fs_getcwd" }
    if name == "fs_readdir" { return "dm_fs_readdir" }
    if name == "fs_rename" { return "dm_fs_rename" }
    if name == "fs_mkdir" { return "dm_fs_mkdir" }
    if name == "fs_remove" { return "dm_fs_remove" }
    -- OS builtins
    if name == "os_getenv" { return "dm_os_getenv" }
    return name
}

fn is_builtin_fn(name: string) -> bool {
    if name == "print" or name == "println" { return true }
    if name == "eprint" or name == "eprintln" { return true }
    if name == "panic" or name == "exit" { return true }
    if name == "len" or name == "char_at" or name == "substr" { return true }
    if name == "int_to_string" or name == "float_to_string" or name == "bool_to_string" { return true }
    if name == "parse_int" or name == "parse_float" { return true }
    if name == "string_contains" or name == "string_find" { return true }
    if name == "starts_with" or name == "ends_with" { return true }
    if name == "string_trim" or name == "string_replace" { return true }
    if name == "string_to_upper" or name == "string_to_lower" { return true }
    if name == "string_split" { return true }
    if name == "file_read" or name == "file_write" or name == "file_append" or name == "file_exists" { return true }
    if name == "read_line" { return true }
    if name == "args_len" or name == "args_get" { return true }
    if name == "system" { return true }
    if name == "assert" or name == "assert_eq" { return true }
    if name == "Box_new" or name == "Box_null" { return true }
    if name == "Map_new" { return true }
    if name == "is_alpha" or name == "is_digit" or name == "is_whitespace" or name == "is_alnum" { return true }
    if name == "char_to_string" { return true }
    if name == "string_to_int" { return true }
    if name == "path_dirname" or name == "path_basename" or name == "path_extension" or name == "path_stem" { return true }
    if name == "path_join" { return true }
    if name == "fs_getcwd" or name == "fs_readdir" or name == "fs_rename" { return true }
    if name == "fs_mkdir" or name == "fs_remove" { return true }
    if name == "os_getenv" { return true }
    return false
}

-- Get the return type of a builtin function
fn builtin_return_type(name: string) -> string {
    if name == "print" or name == "println" { return "void" }
    if name == "eprint" or name == "eprintln" { return "void" }
    if name == "panic" { return "void" }
    if name == "exit" { return "void" }
    if name == "len" { return "i64" }
    if name == "char_at" { return "string" }
    if name == "substr" { return "string" }
    if name == "int_to_string" { return "string" }
    if name == "float_to_string" { return "string" }
    if name == "bool_to_string" { return "string" }
    if name == "parse_int" { return "i64" }
    if name == "parse_float" { return "f64" }
    if name == "string_contains" { return "bool" }
    if name == "string_find" { return "i64" }
    if name == "starts_with" or name == "ends_with" { return "bool" }
    if name == "string_trim" { return "string" }
    if name == "string_replace" { return "string" }
    if name == "string_to_upper" or name == "string_to_lower" { return "string" }
    if name == "string_split" { return "list" }
    if name == "file_read" { return "string" }
    if name == "file_write" or name == "file_append" { return "void" }
    if name == "file_exists" { return "bool" }
    if name == "read_line" { return "string" }
    if name == "args_len" { return "i64" }
    if name == "args_get" { return "string" }
    if name == "system" { return "i64" }
    if name == "assert" or name == "assert_eq" { return "void" }
    -- Character classification
    if name == "is_alpha" or name == "is_digit" or name == "is_whitespace" or name == "is_alnum" { return "bool" }
    -- Character/string conversion
    if name == "char_to_string" { return "string" }
    if name == "string_to_int" { return "i64" }
    -- Path utilities
    if name == "path_dirname" or name == "path_basename" or name == "path_extension" or name == "path_stem" { return "string" }
    if name == "path_join" { return "string" }
    -- Filesystem builtins
    if name == "fs_getcwd" { return "string" }
    if name == "fs_readdir" { return "string" }
    if name == "fs_rename" { return "i64" }
    if name == "fs_mkdir" { return "i64" }
    if name == "fs_remove" { return "i64" }
    -- OS builtins
    if name == "os_getenv" { return "string" }
    return "void"
}

-- ============================================================
-- GENERATE MODULE: Entry point for IR generation from AST
-- ============================================================

fn generate_module(gen: IRGenerator, source: SourceFile) -> IRGenerator {
    let mut g = gen
    g.declarations = source.declarations

    -- Phase 1: Register all struct and enum definitions
    let mut i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_STRUCT() {
            let sd = *decl.struct_decl
            g = register_struct(g, sd)
        } else if decl.kind == DECL_ENUM() {
            let ed = *decl.enum_decl
            g = register_enum(g, ed)
        }
        i = i + 1
    }

    -- Phase 2: Register all function signatures (forward declarations)
    i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            g.known_functions.insert(fd.name, true)
            if fd.generic_params.len() > 0 {
                -- Store generic function for later monomorphization
                g.generic_fn_decls.insert(fd.name, i)
            }
        } else if decl.kind == DECL_IMPL() {
            let ib = *decl.impl_block
            g = register_impl_methods(g, ib)
        }
        i = i + 1
    }

    -- Phase 3: Declare runtime builtin functions
    g = declare_runtime_functions(g)

    -- Phase 4: Generate IR for all declarations
    i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.generic_params.len() == 0 {
                g = generate_function(g, fd)
            }
        } else if decl.kind == DECL_IMPL() {
            let ib = *decl.impl_block
            g = generate_impl_block(g, ib)
        } else if decl.kind == DECL_CONST() {
            let cd = *decl.const_decl
            g = generate_constant(g, cd)
        }
        i = i + 1
    }

    return g
}

-- ============================================================
-- FORWARD DECLARATIONS (stubs - implemented in ir_gen_decl.dm)
-- ============================================================

-- These will be defined in the split module files
-- Using the import concatenation system, they are visible

-- register_struct: from ir_gen_decl.dm
-- register_enum: from ir_gen_decl.dm
-- register_impl_methods: from ir_gen_decl.dm
-- declare_runtime_functions: from ir_gen_decl.dm
-- generate_function: from ir_gen_decl.dm
-- generate_impl_block: from ir_gen_decl.dm
-- generate_constant: from ir_gen_decl.dm


-- ============================================================
-- STRUCT REGISTRATION
-- Registers struct types in the IR module and struct_defs map
-- ============================================================

fn register_struct(gen: IRGenerator, sd: StructDecl) -> IRGenerator {
    let mut g = gen
    let mut info = struct_info_new(sd.name)

    let mut ir_fields: List[IRField] = []
    let mut i = 0
    while i < sd.fields.len() {
        let f = sd.fields[i]
        let fname = f.name
        let ftype = f.type_name
        let ir_type_name = map_type_name(ftype)

        info.field_names.push(fname)
        info.field_types.push(ir_type_name)

        let field = ir_field_new(fname, ir_type_name)
        ir_fields.push(field)

        -- Detect List[T] fields for field access dispatch
        if starts_with(ftype, "List[") {
            let inner = extract_inner_type(ftype, "List[")
            let elem_kind = classify_list_elem(inner)
            let field_key = sd.name + "." + fname
            g.field_list_elem_kinds.insert(field_key, elem_kind)
            if elem_kind == LIST_ELEM_OTHER() {
                g.field_list_elem_types.insert(field_key, inner)
            }
        }

        i = i + 1
    }

    -- Create IR struct type
    let mut ir_ty = ir_type_named(IR_TYPE_STRUCT(), sd.name)
    ir_ty.fields = ir_fields

    g.struct_defs.insert(sd.name, info)
    g.ir_mod.struct_defs.insert(sd.name, ir_ty)

    return g
}

-- ============================================================
-- ENUM REGISTRATION
-- ============================================================

fn register_enum(gen: IRGenerator, ed: EnumDecl) -> IRGenerator {
    let mut g = gen
    let mut info = enum_info_new(ed.name)

    let mut ir_fields: List[IRField] = []
    -- First field: tag (i32)
    ir_fields.push(ir_field_new("tag", "i32"))

    let mut i = 0
    while i < ed.variants.len() {
        let v = ed.variants[i]
        info.variant_names.push(v.name)
        info.variant_tags.push(i)

        -- Check for payload types
        if v.has_payload and v.payload_types.len() > 0 {
            let pt = v.payload_types[0]
            info.variant_payload_types.push(pt)
            info.variant_has_payload.push(true)
            -- Add payload field to struct
            let field_name = v.name + "_0"
            let field_type = map_type_name(pt)
            ir_fields.push(ir_field_new(field_name, field_type))
        } else {
            info.variant_payload_types.push("")
            info.variant_has_payload.push(false)
        }

        i = i + 1
    }

    -- If no payload fields, add dummy payload
    if ir_fields.len() == 1 {
        ir_fields.push(ir_field_new("payload", "i64"))
    }

    let mut ir_ty = ir_type_named(IR_TYPE_STRUCT(), ed.name)
    ir_ty.fields = ir_fields

    g.enum_defs.insert(ed.name, info)
    g.ir_mod.struct_defs.insert(ed.name, ir_ty)

    return g
}

-- ============================================================
-- IMPL BLOCK REGISTRATION
-- Registers method-to-struct mappings and trait impl methods
-- ============================================================

fn register_impl_methods(gen: IRGenerator, ib: ImplBlock) -> IRGenerator {
    let mut g = gen
    let struct_name = ib.target_type

    -- If this is a trait impl, register trait methods
    if len(ib.trait_name) > 0 {
        let key = struct_name + ":" + ib.trait_name
        let mut method_list = ""
        let mut i = 0
        while i < ib.methods.len() {
            if i > 0 {
                method_list = method_list + "|"
            }
            method_list = method_list + ib.methods[i].name
            i = i + 1
        }
        g.trait_impl_methods.insert(key, method_list)
    }

    -- Register method->struct mappings
    let mut i = 0
    while i < ib.methods.len() {
        g.method_struct_map.insert(ib.methods[i].name, struct_name)

        -- For generic impl blocks, register each method as a generic fn decl
        -- so it can be monomorphized when called
        if ib.generic_params.len() > 0 {
            let mangled = struct_name + "_" + ib.methods[i].name
            -- Create a modified FunctionDecl with the impl's generic params
            let mut method_fd = ib.methods[i]
            method_fd.generic_params = ib.generic_params
            -- Push it as a synthetic declaration and record its index
            let decl_idx = g.declarations.len()
            g.declarations.push(decl_function(method_fd))
            g.generic_fn_decls.insert(mangled, decl_idx)
        }

        i = i + 1
    }

    return g
}

-- ============================================================
-- RUNTIME FUNCTION DECLARATIONS
-- Declares all extern builtin functions used by the compiler
-- ============================================================

fn declare_runtime_functions(gen: IRGenerator) -> IRGenerator {
    let mut g = gen

    -- I/O
    g = declare_extern(g, "dm_println", "string", "void")
    g = declare_extern(g, "dm_print", "string", "void")
    g = declare_extern(g, "dm_eprintln", "string", "void")
    g = declare_extern(g, "dm_eprint", "string", "void")

    -- Conversion
    g = declare_extern(g, "dm_int_to_string", "i64", "string")
    g = declare_extern(g, "dm_float_to_string", "f64", "string")
    g = declare_extern(g, "dm_bool_to_string", "bool", "string")
    g = declare_extern(g, "dm_parse_int", "string", "i64")
    g = declare_extern(g, "dm_parse_float", "string", "f64")

    -- String operations
    g = declare_extern(g, "dm_string_new", "ptr", "string")
    g = declare_extern2(g, "dm_string_concat", "string", "string", "string")
    g = declare_extern2(g, "dm_string_eq", "string", "string", "bool")
    g = declare_extern(g, "dm_string_len", "string", "i64")
    g = declare_extern2(g, "dm_string_contains", "string", "string", "bool")
    g = declare_extern2(g, "dm_string_cmp", "string", "string", "i64")
    g = declare_extern2(g, "dm_string_find", "string", "string", "i64")
    g = declare_extern2(g, "dm_char_at", "string", "i64", "string")
    g = declare_extern3(g, "dm_substr", "string", "i64", "i64", "string")
    g = declare_extern2(g, "dm_starts_with", "string", "string", "bool")
    g = declare_extern2(g, "dm_ends_with", "string", "string", "bool")
    g = declare_extern(g, "dm_string_trim", "string", "string")
    g = declare_extern3(g, "dm_string_replace", "string", "string", "string", "string")
    g = declare_extern(g, "dm_string_to_upper", "string", "string")
    g = declare_extern(g, "dm_string_to_lower", "string", "string")

    -- Character classification and conversion
    g = declare_extern(g, "dm_is_alpha", "string", "bool")
    g = declare_extern(g, "dm_is_digit", "string", "bool")
    g = declare_extern(g, "dm_is_whitespace", "string", "bool")
    g = declare_extern(g, "dm_is_alnum", "string", "bool")
    g = declare_extern(g, "dm_char_to_string", "string", "string")

    -- List contains
    g = declare_extern2(g, "dm_list_int64_contains", "ptr", "i64", "bool")
    g = declare_extern2(g, "dm_list_string_contains", "ptr", "string", "bool")

    -- Process
    g = declare_extern(g, "dm_panic", "string", "void")
    g = declare_extern(g, "exit", "i64", "void")

    -- Stdin
    g = declare_extern0(g, "dm_read_line", "string")

    -- System
    g = declare_extern(g, "dm_system", "string", "i64")
    g = declare_extern0(g, "dm_args_len", "i64")
    g = declare_extern(g, "dm_args_get", "i64", "string")

    -- File I/O
    g = declare_extern(g, "dm_file_read", "string", "string")
    g = declare_extern2(g, "dm_file_write", "string", "string", "void")
    g = declare_extern2(g, "dm_file_append", "string", "string", "void")
    g = declare_extern(g, "dm_file_exists", "string", "bool")

    -- Path utilities
    g = declare_extern(g, "dm_path_dirname", "string", "string")
    g = declare_extern(g, "dm_path_basename", "string", "string")
    g = declare_extern(g, "dm_path_extension", "string", "string")
    g = declare_extern(g, "dm_path_stem", "string", "string")
    g = declare_extern2(g, "dm_path_join", "string", "string", "string")

    -- Memory allocation
    g = declare_extern(g, "malloc", "i64", "ptr")
    g = declare_extern(g, "free", "ptr", "void")

    -- List operations (int64)
    g = declare_extern(g, "dm_list_int64_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_int64_push", "ptr", "i64", "void")
    g = declare_extern2(g, "dm_list_int64_get", "ptr", "i64", "i64")
    g = declare_extern(g, "dm_list_int64_len", "ptr", "i64")
    g = declare_extern(g, "dm_list_int64_pop", "ptr", "i64")

    -- List operations (string)
    g = declare_extern(g, "dm_list_string_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_string_push", "ptr", "string", "void")
    g = declare_extern2(g, "dm_list_string_get", "ptr", "i64", "string")
    g = declare_extern(g, "dm_list_string_len", "ptr", "i64")

    -- List operations (double)
    g = declare_extern(g, "dm_list_double_new", "ptr", "void")
    g = declare_extern2(g, "dm_list_double_push", "ptr", "f64", "void")
    g = declare_extern2(g, "dm_list_double_get", "ptr", "i64", "f64")
    g = declare_extern(g, "dm_list_double_len", "ptr", "i64")

    -- List operations (generic)
    g = declare_extern(g, "dm_list_generic_new", "ptr", "void")
    g = declare_extern3(g, "dm_list_generic_push", "ptr", "ptr", "i64", "void")
    g = declare_extern4(g, "dm_list_generic_get", "ptr", "ptr", "i64", "i64", "void")
    g = declare_extern(g, "dm_list_generic_len", "ptr", "i64")

    -- Filesystem
    g = declare_extern(g, "dm_fs_mkdir", "string", "i64")
    g = declare_extern(g, "dm_fs_readdir", "string", "string")
    g = declare_extern(g, "dm_fs_remove", "string", "i64")
    g = declare_extern2(g, "dm_fs_rename", "string", "string", "i64")
    g = declare_extern0(g, "dm_fs_getcwd", "string")

    -- OS
    g = declare_extern(g, "dm_os_getenv", "string", "string")

    -- String split
    g = declare_extern3(g, "dm_string_split", "ptr", "string", "string", "void")

    -- Map[string, int]
    g = declare_extern(g, "dm_map_string_int_new", "ptr", "void")
    g = declare_extern3(g, "dm_map_string_int_insert", "ptr", "string", "i64", "void")
    g = declare_extern2(g, "dm_map_string_int_get", "ptr", "string", "i64")
    g = declare_extern2(g, "dm_map_string_int_contains", "ptr", "string", "bool")
    g = declare_extern2(g, "dm_map_string_int_remove", "ptr", "string", "void")
    g = declare_extern(g, "dm_map_string_int_len", "ptr", "i64")
    g = declare_extern2(g, "dm_map_string_int_keys", "ptr", "ptr", "void")
    g = declare_extern2(g, "dm_map_string_int_values", "ptr", "ptr", "void")

    -- Map[int, string]
    g = declare_extern(g, "dm_map_int_string_new", "ptr", "void")
    g = declare_extern3(g, "dm_map_int_string_insert", "ptr", "i64", "string", "void")
    g = declare_extern3(g, "dm_map_int_string_get", "ptr", "ptr", "i64", "void")
    g = declare_extern2(g, "dm_map_int_string_contains", "ptr", "i64", "bool")
    g = declare_extern2(g, "dm_map_int_string_remove", "ptr", "i64", "void")
    g = declare_extern(g, "dm_map_int_string_len", "ptr", "i64")

    return g
}

-- Helper: declare extern with 0 params
fn declare_extern0(gen: IRGenerator, name: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 1 param
fn declare_extern(gen: IRGenerator, name: string, p1: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 2 params
fn declare_extern2(gen: IRGenerator, name: string, p1: string, p2: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    func.params.push(ir_param_new("p1", p2))
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 3 params
fn declare_extern3(gen: IRGenerator, name: string, p1: string, p2: string, p3: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    func.params.push(ir_param_new("p1", p2))
    func.params.push(ir_param_new("p2", p3))
    g.ir_mod.functions.push(func)
    return g
}

-- Helper: declare extern with 4 params
fn declare_extern4(gen: IRGenerator, name: string, p1: string, p2: string, p3: string, p4: string, ret: string) -> IRGenerator {
    let mut g = gen
    let mut func = ir_function_new(name, ret)
    func.is_extern = true
    func.params.push(ir_param_new("p0", p1))
    func.params.push(ir_param_new("p1", p2))
    func.params.push(ir_param_new("p2", p3))
    func.params.push(ir_param_new("p3", p4))
    g.ir_mod.functions.push(func)
    return g
}

-- ============================================================
-- FUNCTION GENERATION
-- ============================================================

fn generate_function(gen: IRGenerator, fd: FunctionDecl) -> IRGenerator {
    let mut g = gen

    -- Skip generic functions (monomorphized at call site)
    if fd.generic_params.len() > 0 {
        return g
    }

    let name = fd.name

    -- Declare function if not already present
    g = declare_function(g, fd, name)

    -- Skip extern functions without body
    if fd.is_extern {
        return g
    }

    -- Generate function body
    g = generate_function_body(g, fd, name)

    return g
}

fn declare_function(gen: IRGenerator, fd: FunctionDecl, name: string) -> IRGenerator {
    let mut g = gen

    let ret_type = if len(fd.return_type) > 0 {
        map_type_name(fd.return_type)
    } else {
        "void"
    }

    let mut func = ir_function_new(name, ret_type)
    func.is_extern = fd.is_extern

    -- Build parameter list
    let mut mut_flags = ""
    let mut i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        let pname = p.name
        -- Resolve "Self" to current impl type
        let mut ptype_str = if p.type_name == "Self" and g.has_impl_type {
            g.current_impl_type
        } else {
            p.type_name
        }
        -- Resolve dyn Trait to concrete implementor type
        if starts_with(ptype_str, "dyn ") {
            let dyn_trait = substr(ptype_str, 4, len(ptype_str) - 4)
            let impl_keys = g.trait_impl_methods.keys()
            let mut ki = 0
            while ki < impl_keys.len() {
                let impl_key = impl_keys[ki]
                if ends_with(impl_key, ":" + dyn_trait) {
                    let colon_pos = string_find(impl_key, ":")
                    if colon_pos > 0 {
                        ptype_str = substr(impl_key, 0, colon_pos)
                    }
                }
                ki = ki + 1
            }
        }
        let ptype = map_type_name(ptype_str)

        let actual_type = if p.is_mut {
            "ptr"
        } else {
            ptype
        }

        func.params.push(ir_param_new(pname, actual_type))

        -- Build mut flags string
        if i > 0 {
            mut_flags = mut_flags + "|"
        }
        if p.is_mut {
            mut_flags = mut_flags + "1"
        } else {
            mut_flags = mut_flags + "0"
        }

        i = i + 1
    }

    -- Track user extern functions
    if fd.is_extern {
        let is_runtime = starts_with(name, "dm_") or name == "malloc" or name == "free" or name == "exit"
        if is_runtime == false {
            g.user_extern_fns.insert(name, true)
            g.ir_mod.user_extern_fns.insert(name, true)
        }
    }

    -- Track mut params
    if string_contains(mut_flags, "1") {
        g.mut_param_fns.insert(name, mut_flags)
    }

    g.ir_mod.functions.push(func)

    return g
}

fn generate_function_body(gen: IRGenerator, fd: FunctionDecl, name: string) -> IRGenerator {
    let mut g = gen

    -- Find the function in the module
    let mut func_idx = -1
    let mut i = 0
    while i < g.ir_mod.functions.len() {
        let f = g.ir_mod.functions[i]
        if f.name == name {
            func_idx = i
        }
        i = i + 1
    }

    if func_idx < 0 {
        return g
    }

    -- Set current return type
    let ret_type = if len(fd.return_type) > 0 {
        map_type_name(fd.return_type)
    } else {
        "void"
    }
    g.current_return_type = ret_type

    -- Reset variable maps for this function
    g.variable_map = Map_new()
    g.variable_types = Map_new()
    g.mut_ptr_vars = Map_new()
    g.param_ptr_map = Map_new()
    let empty_blocks: List[IRBasicBlock] = []
    g.completed_blocks = empty_blocks

    -- Create entry block
    let entry_label = "entry"
    let mut blk = ir_block_new(entry_label)

    -- Set up parameters
    i = 0
    while i < fd.params.len() {
        let p = fd.params[i]
        let pname = p.name
        -- Resolve "Self" to current impl type
        let mut ptype_str = if p.type_name == "Self" and g.has_impl_type {
            g.current_impl_type
        } else {
            p.type_name
        }
        -- Resolve dyn Trait to concrete implementor type
        if starts_with(ptype_str, "dyn ") {
            let dyn_trait = substr(ptype_str, 4, len(ptype_str) - 4)
            let impl_keys = g.trait_impl_methods.keys()
            let mut ki = 0
            while ki < impl_keys.len() {
                let impl_key = impl_keys[ki]
                if ends_with(impl_key, ":" + dyn_trait) {
                    let colon_pos = string_find(impl_key, ":")
                    if colon_pos > 0 {
                        ptype_str = substr(impl_key, 0, colon_pos)
                    }
                }
                ki = ki + 1
            }
        }
        let ptype = map_type_name(ptype_str)

        if p.is_mut {
            -- Mut param: the caller passes a POINTER to the struct.
            -- Do NOT create an alloca — use the param ref directly as a pointer.
            -- Loads/stores go through the caller's memory, so mutations are visible.
            g.variable_map.insert(pname, -(i + 1))
            g.variable_types.insert(pname, ptype)
            g.mut_ptr_vars.insert(pname, true)
            g.param_ptr_map.insert(pname, i)
        } else {
            -- Regular param: alloca + store
            let alloca_id = g.builder.next_id
            let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
            alloca_inst.alloc_type = ptype
            alloca_inst.result_type = "ptr(" + ptype + ")"
            alloca_inst.has_result = true
            blk.instructions.push(alloca_inst)
            g.builder.next_id = g.builder.next_id + 1

            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(alloca_id)
            store_inst.store_val = ir_val_param(i)
            store_inst.has_result = false
            blk.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(pname, alloca_id)
            g.variable_types.insert(pname, ptype)
        }

        -- Track dyn Trait parameters: resolve to concrete implementor
        if starts_with(ptype_str, "dyn ") {
            let dyn_trait_name = substr(ptype_str, 4, len(ptype_str) - 4)
            -- Find a concrete type that implements this trait
            let mut concrete_found = ""
            let impl_keys = g.trait_impl_methods.keys()
            let mut ki = 0
            while ki < impl_keys.len() {
                let impl_key = impl_keys[ki]
                if ends_with(impl_key, ":" + dyn_trait_name) {
                    let colon_pos = string_find(impl_key, ":")
                    if colon_pos > 0 {
                        concrete_found = substr(impl_key, 0, colon_pos)
                    }
                }
                ki = ki + 1
            }
            if len(concrete_found) > 0 {
                g.dyn_var_concrete.insert(pname, concrete_found)
                g.dyn_var_traits.insert(pname, dyn_trait_name)
                g.var_struct_types.insert(pname, concrete_found)
            }
        }

        -- Track function-pointer-typed parameters for indirect calls
        if starts_with(ptype_str, "fn(") {
            g.variable_types.insert(pname, ptype_str)
        }

        -- Track struct/enum/list/box typed parameters
        if g.struct_defs.contains(ptype_str) {
            g.var_struct_types.insert(pname, ptype_str)
        }
        if g.enum_defs.contains(ptype_str) {
            g.var_enum_types.insert(pname, ptype_str)
        }
        if starts_with(ptype_str, "List[") {
            let inner = extract_inner_type(ptype_str, "List[")
            let elem_kind = classify_list_elem(inner)
            g.list_elem_kinds.insert(pname, elem_kind)
            if elem_kind == LIST_ELEM_OTHER() {
                g.list_elem_types.insert(pname, inner)
            }
        }
        if starts_with(ptype_str, "Box[") {
            let inner = extract_inner_type(ptype_str, "Box[")
            g.box_inner_types.insert(pname, inner)
        }

        i = i + 1
    }

    -- Generate body statements
    i = 0
    while i < fd.body.len() {
        let s = fd.body[i]
        let result = generate_statement(g, blk, s)
        g = result.gen
        blk = result.block
        i = i + 1
    }

    -- Add implicit return if block has no terminator
    if blk.has_terminator == false {
        if ret_type == "void" {
            blk.terminator = Box_new(term_ret_void())
            blk.has_terminator = true
        } else if name == "main" {
            blk.terminator = Box_new(term_ret(ir_val_int(0)))
            blk.has_terminator = true
        } else {
            blk.terminator = Box_new(term_ret(default_value_for_type(ret_type)))
            blk.has_terminator = true
        }
    }

    -- Add blocks to function in correct order (entry block must be first)
    let mut func = g.ir_mod.functions[func_idx]
    -- Find and push the entry block first
    i = 0
    while i < g.completed_blocks.len() {
        if g.completed_blocks[i].label == "entry" {
            func.blocks.push(g.completed_blocks[i])
        }
        i = i + 1
    }
    -- Push remaining completed blocks (non-entry)
    i = 0
    while i < g.completed_blocks.len() {
        if g.completed_blocks[i].label != "entry" {
            func.blocks.push(g.completed_blocks[i])
        }
        i = i + 1
    }
    -- Then push the final block (if it's the entry and no blocks, push it; otherwise push as last)
    func.blocks.push(blk)
    g.ir_mod.functions[func_idx] = func

    return g
}

-- ============================================================
-- IMPL BLOCK GENERATION
-- ============================================================

fn generate_impl_block(gen: IRGenerator, ib: ImplBlock) -> IRGenerator {
    let mut g = gen
    let struct_name = ib.target_type

    -- Skip generic impl blocks — their methods are monomorphized on demand
    if ib.generic_params.len() > 0 {
        return g
    }

    g.current_impl_type = struct_name
    g.has_impl_type = true

    let mut i = 0
    while i < ib.methods.len() {
        let fd = ib.methods[i]
        let mangled = struct_name + "_" + fd.name
        g = declare_function(g, fd, mangled)
        if fd.is_extern == false {
            g.method_struct_map.insert(fd.name, struct_name)
            g = generate_function_body(g, fd, mangled)
        }
        i = i + 1
    }

    g.has_impl_type = false
    g.current_impl_type = ""
    return g
}

-- ============================================================
-- CONSTANT GENERATION
-- ============================================================

fn generate_constant(gen: IRGenerator, cd: ConstDecl) -> IRGenerator {
    let mut g = gen

    -- Pre-grow globals list to avoid realloc during comptime evaluation.
    -- Comptime recursion copies the generator by value, sharing the list buffer.
    -- A realloc in a callee would free the buffer the caller is still using.
    -- By pre-growing, we ensure enough capacity for comptime variable pushes.
    if cd.is_comptime {
        let mut prealloc_i = 0
        while prealloc_i < 1024 {
            let mut dummy = ir_global_new("__comptime_prealloc__", "i64")
            dummy.has_init = false
            dummy.is_const = false
            g.ir_mod.globals.push(dummy)
            prealloc_i = prealloc_i + 1
        }
        let mut pop_i = 0
        while pop_i < 1024 {
            g.ir_mod.globals.pop()
            pop_i = pop_i + 1
        }
    }

    -- Try to evaluate the value at compile time
    -- Use the full comptime evaluator for comptime expressions (blocks, fn calls, etc.)
    let val = if cd.is_comptime {
        eval_comptime_expr(g, *cd.value)
    } else {
        eval_const_value(g, cd.value)
    }

    let ty = if len(cd.type_name) > 0 {
        map_type_name(cd.type_name)
    } else if val.kind == VAL_CONST_INT() {
        "i64"
    } else if val.kind == VAL_CONST_FLOAT() {
        "f64"
    } else if val.kind == VAL_CONST_BOOL() {
        "bool"
    } else if val.kind == VAL_CONST_STRING() {
        "string"
    } else {
        "i64"
    }

    let mut global = ir_global_new(cd.name, ty)
    global.init_value = val
    global.has_init = true
    global.is_const = true
    g.ir_mod.globals.push(global)

    return g
}

-- Simple constant value evaluation
fn eval_const_value(gen: IRGenerator, expr_box: Box[Expr]) -> IRValue {
    let e = *expr_box
    if e.kind == EXPR_LITERAL_INT() {
        return ir_val_int(e.int_val)
    }
    if e.kind == EXPR_LITERAL_FLOAT() {
        return ir_val_float(e.float_val)
    }
    if e.kind == EXPR_LITERAL_BOOL() {
        return ir_val_bool(e.bool_val)
    }
    if e.kind == EXPR_LITERAL_STRING() {
        return ir_val_string(e.str_val)
    }
    if e.kind == EXPR_BINARY() {
        let lhs = eval_const_value(gen, e.left)
        let rhs = eval_const_value(gen, e.right)
        -- Integer arithmetic
        if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_INT() {
            if e.op == BINOP_ADD() { return ir_val_int(lhs.int_val + rhs.int_val) }
            if e.op == BINOP_SUB() { return ir_val_int(lhs.int_val - rhs.int_val) }
            if e.op == BINOP_MUL() { return ir_val_int(lhs.int_val * rhs.int_val) }
            if e.op == BINOP_DIV() and rhs.int_val != 0 { return ir_val_int(lhs.int_val / rhs.int_val) }
        }
        -- Float arithmetic
        if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_FLOAT() {
            if e.op == BINOP_ADD() { return ir_val_float(lhs.float_val + rhs.float_val) }
            if e.op == BINOP_SUB() { return ir_val_float(lhs.float_val - rhs.float_val) }
            if e.op == BINOP_MUL() { return ir_val_float(lhs.float_val * rhs.float_val) }
        }
        -- String concatenation
        if lhs.kind == VAL_CONST_STRING() and rhs.kind == VAL_CONST_STRING() {
            if e.op == BINOP_ADD() { return ir_val_string(lhs.str_val + rhs.str_val) }
        }
        -- Boolean AND/OR
        if lhs.kind == VAL_CONST_BOOL() and rhs.kind == VAL_CONST_BOOL() {
            if e.op == BINOP_AND() { return ir_val_bool(lhs.bool_val and rhs.bool_val) }
            if e.op == BINOP_OR() { return ir_val_bool(lhs.bool_val or rhs.bool_val) }
        }
    }
    if e.kind == EXPR_UNARY() {
        if e.op == UNOP_NOT() {
            let operand_val = eval_const_value(gen, e.operand)
            if operand_val.kind == VAL_CONST_BOOL() {
                return ir_val_bool(operand_val.bool_val == false)
            }
        }
        if e.op == UNOP_NEG() {
            let operand_val = eval_const_value(gen, e.operand)
            if operand_val.kind == VAL_CONST_INT() {
                return ir_val_int(0 - operand_val.int_val)
            }
            if operand_val.kind == VAL_CONST_FLOAT() {
                return ir_val_float(0.0 - operand_val.float_val)
            }
        }
    }
    if e.kind == EXPR_IDENTIFIER() {
        -- Look up globals
        let mut i = 0
        while i < gen.ir_mod.globals.len() {
            let gl = gen.ir_mod.globals[i]
            if gl.name == e.name {
                return gl.init_value
            }
            i = i + 1
        }
    }
    return ir_val_int(0)
}

-- ============================================================
-- ESTIMATE TYPE SIZE (for malloc/generic list operations)
-- ============================================================

fn estimate_type_size(type_name: string) -> int {
    if type_name == "i8" or type_name == "u8" or type_name == "bool" { return 1 }
    if type_name == "i16" or type_name == "u16" { return 2 }
    if type_name == "i32" or type_name == "u32" or type_name == "f32" { return 4 }
    if type_name == "i64" or type_name == "u64" or type_name == "f64" or type_name == "ptr" { return 8 }
    if type_name == "string" { return 24 }
    -- Struct size: rough estimate of 8 bytes per field
    return 64
}

fn estimate_type_size_gen(gen: IRGenerator, type_name: string) -> int {
    if type_name == "i8" or type_name == "u8" or type_name == "bool" { return 1 }
    if type_name == "i16" or type_name == "u16" { return 2 }
    if type_name == "i32" or type_name == "u32" or type_name == "f32" { return 4 }
    if type_name == "i64" or type_name == "u64" or type_name == "f64" or type_name == "ptr" { return 8 }
    if type_name == "string" { return 24 }
    -- Look up struct definition
    if gen.struct_defs.contains(type_name) {
        let info = gen.struct_defs.get(type_name)
        let mut size = 0
        let mut fi = 0
        while fi < info.field_names.len() {
            let field_type = info.field_types[fi]
            let fs = estimate_type_size(field_type)
            let aligned = if fs < 8 { 8 } else { fs }
            size = size + aligned
            fi = fi + 1
        }
        return size
    }
    return 64
}


-- ============================================================
-- GenResult: return both generator and current block
-- ============================================================

struct GenResult {
    gen: IRGenerator,
    block: IRBasicBlock
}

fn gen_result(g: IRGenerator, b: IRBasicBlock) -> GenResult {
    return GenResult { gen: g, block: b }
}

-- ============================================================
-- ExprResult: return generator, block, and result value ID
-- ============================================================

struct ExprResult {
    gen: IRGenerator,
    block: IRBasicBlock,
    val_id: int
}

fn expr_result(g: IRGenerator, b: IRBasicBlock, v: int) -> ExprResult {
    return ExprResult { gen: g, block: b, val_id: v }
}

-- ============================================================
-- STATEMENT GENERATION
-- ============================================================

fn generate_statement(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    if stmt.kind == STMT_LET() {
        return generate_let_binding(gen, block, stmt)
    }
    if stmt.kind == STMT_RETURN() {
        return generate_return(gen, block, stmt)
    }
    if stmt.kind == STMT_IF() {
        return generate_if_statement(gen, block, stmt)
    }
    if stmt.kind == STMT_WHILE() {
        return generate_while_loop(gen, block, stmt)
    }
    if stmt.kind == STMT_FOR() {
        return generate_for_loop(gen, block, stmt)
    }
    if stmt.kind == STMT_LOOP() {
        return generate_loop_stmt(gen, block, stmt)
    }
    if stmt.kind == STMT_BREAK() {
        return generate_break(gen, block)
    }
    if stmt.kind == STMT_CONTINUE() {
        return generate_continue(gen, block)
    }
    if stmt.kind == STMT_ASSIGNMENT() {
        return generate_assignment(gen, block, stmt)
    }
    if stmt.kind == STMT_EXPRESSION() {
        let result = generate_expr(gen, block, *stmt.expr)
        return gen_result(result.gen, result.block)
    }
    if stmt.kind == STMT_MATCH() {
        let result = generate_match_expr(gen, block, *stmt.match_expr)
        return gen_result(result.gen, result.block)
    }
    if stmt.kind == STMT_REGION() {
        -- Region blocks: just generate the body (runtime handles arena)
        return generate_block_stmts(gen, block, stmt.region_body)
    }
    return gen_result(gen, block)
}

-- ============================================================
-- BLOCK STATEMENT GENERATION
-- ============================================================

fn generate_block_stmts(gen: IRGenerator, block: IRBasicBlock, stmts: List[Stmt]) -> GenResult {
    let mut g = gen
    let mut b = block
    let mut i = 0
    while i < stmts.len() {
        let result = generate_statement(g, b, stmts[i])
        g = result.gen
        b = result.block
        i = i + 1
    }
    return gen_result(g, b)
}

-- ============================================================
-- LET BINDING
-- ============================================================

fn generate_let_binding(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.let_name
    let mut type_str = stmt.let_type

    -- Normalize fixed-size array type [T; N] to List[T]
    if starts_with(type_str, "[") {
        let semi_pos = string_find(type_str, ";")
        if semi_pos > 0 {
            let inner_type = string_trim(substr(type_str, 1, semi_pos - 1))
            type_str = "List[" + inner_type + "]"
        }
    }

    -- Detect List[T] type annotation
    if starts_with(type_str, "List[") {
        let inner = extract_inner_type(type_str, "List[")
        let elem_kind = classify_list_elem(inner)
        g.list_elem_kinds.insert(var_name, elem_kind)
        if elem_kind == LIST_ELEM_OTHER() {
            g.list_elem_types.insert(var_name, inner)
        }

        -- Determine list struct type name
        let list_type = if elem_kind == LIST_ELEM_INT() {
            "dm_list_int64"
        } else if elem_kind == LIST_ELEM_FLOAT() {
            "dm_list_double"
        } else if elem_kind == LIST_ELEM_STRING() {
            "dm_list_dm_string"
        } else {
            "dm_list_generic"
        }

        -- Alloca for list struct
        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = list_type
        alloca_inst.result_type = "ptr(" + list_type + ")"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Call new function to initialize
        let new_fn = if elem_kind == LIST_ELEM_INT() { "dm_list_int64_new" }
        else if elem_kind == LIST_ELEM_FLOAT() { "dm_list_double_new" }
        else if elem_kind == LIST_ELEM_STRING() { "dm_list_string_new" }
        else { "dm_list_generic_new" }
        let mut call_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        call_inst.callee = new_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(alloca_id))
        call_inst.call_args = call_args
        call_inst.result_type = "void"
        call_inst.has_result = false
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- If the initializer is a non-empty array literal, push elements into the list
        let init_expr = *stmt.let_value
        if init_expr.kind == EXPR_ARRAY_LITERAL() and init_expr.elements.len() > 0 {
            let push_fn_name = if elem_kind == LIST_ELEM_INT() { "dm_list_int64_push" }
            else if elem_kind == LIST_ELEM_FLOAT() { "dm_list_double_push" }
            else if elem_kind == LIST_ELEM_STRING() { "llvm_dm_list_string_push" }
            else { "dm_list_generic_push" }
            let mut ei = 0
            while ei < init_expr.elements.len() {
                let elem_result = generate_expr(g, b, init_expr.elements[ei])
                g = elem_result.gen
                b = elem_result.block
                let mut push_call_inst = ir_inst_new(g.builder.next_id, OP_CALL())
                push_call_inst.callee = push_fn_name
                let mut push_call_args: List[IRValue] = []
                push_call_args.push(ir_val_inst(alloca_id))
                push_call_args.push(ir_val_inst(elem_result.val_id))
                push_call_inst.call_args = push_call_args
                push_call_inst.result_type = "void"
                push_call_inst.has_result = false
                b.instructions.push(push_call_inst)
                g.builder.next_id = g.builder.next_id + 1
                ei = ei + 1
            }
        }

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, list_type)
        return gen_result(g, b)
    }

    -- Detect Map[K,V] type annotation
    if starts_with(type_str, "Map[") {
        let inner = extract_inner_type(type_str, "Map[")
        -- Determine map kind from inner type string
        let map_kind = if starts_with(inner, "string") {
            MAP_KIND_STRING_INT()
        } else {
            MAP_KIND_INT_STRING()
        }
        g.map_var_kinds.insert(var_name, map_kind)

        let map_type = if map_kind == MAP_KIND_STRING_INT() {
            "dm_map_string_int"
        } else {
            "dm_map_int_string"
        }

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = map_type
        alloca_inst.result_type = "ptr(" + map_type + ")"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Initialize map
        let new_fn = map_type + "_new"
        let mut call_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        call_inst.callee = new_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(alloca_id))
        call_inst.call_args = call_args
        call_inst.result_type = "void"
        call_inst.has_result = false
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, map_type)
        return gen_result(g, b)
    }

    -- Detect Box[T] type annotation
    if starts_with(type_str, "Box[") {
        let inner = extract_inner_type(type_str, "Box[")
        g.box_inner_types.insert(var_name, inner)

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = "ptr"
        alloca_inst.result_type = "ptr(ptr)"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Generate and store the value if present
        let val_expr = *stmt.let_value
        if val_expr.kind != EXPR_LITERAL_NULL() {
            let result = generate_expr(g, b, val_expr)
            g = result.gen
            b = result.block
            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(alloca_id)
            store_inst.store_val = ir_val_inst(result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1
        }

        g.variable_map.insert(var_name, alloca_id)
        g.variable_types.insert(var_name, "ptr")
        return gen_result(g, b)
    }

    -- Detect dyn Trait type annotation
    if starts_with(type_str, "dyn ") {
        let trait_name = substr(type_str, 4, len(type_str) - 4)

        let val_expr = *stmt.let_value
        -- Infer concrete type from value expression
        let mut concrete = ""
        if val_expr.kind == EXPR_IDENTIFIER() {
            if g.var_struct_types.contains(val_expr.name) {
                concrete = g.var_struct_types.get(val_expr.name)
            }
        } else if val_expr.kind == EXPR_STRUCT_LITERAL() {
            concrete = val_expr.type_name
        }

        -- Use concrete struct type for alloca so the struct is stored directly
        let alloc_type = if len(concrete) > 0 { concrete } else { "ptr" }
        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = alloc_type
        alloca_inst.result_type = "ptr(" + alloc_type + ")"
        alloca_inst.has_result = true
        b.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        if val_expr.kind != EXPR_LITERAL_NULL() {
            let result = generate_expr(g, b, val_expr)
            g = result.gen
            b = result.block

            if len(concrete) > 0 {
                g.dyn_var_concrete.insert(var_name, concrete)
                g.dyn_var_traits.insert(var_name, trait_name)
            }

            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(alloca_id)
            store_inst.store_val = ir_val_inst(result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1
        }

        g.variable_map.insert(var_name, alloca_id)
        if len(concrete) > 0 {
            g.variable_types.insert(var_name, concrete)
            g.var_struct_types.insert(var_name, concrete)
        } else {
            g.variable_types.insert(var_name, "ptr")
        }
        return gen_result(g, b)
    }

    -- Regular let binding: infer type from annotation or value expression
    let infer_val = *stmt.let_value

    -- Early handling: method calls that return lists (.keys(), .values())
    -- These produce list allocas directly; bypass normal alloca+store
    if len(type_str) == 0 and infer_val.kind == EXPR_METHOD_CALL() {
        let early_method = infer_val.method
        if early_method == "keys" or early_method == "values" {
            let early_obj = *infer_val.object
            if early_obj.kind == EXPR_IDENTIFIER() {
                let early_obj_name = early_obj.name
                if g.map_var_kinds.contains(early_obj_name) {
                    let early_mk = g.map_var_kinds.get(early_obj_name)
                    let result = generate_expr(g, b, infer_val)
                    g = result.gen
                    b = result.block
                    if early_method == "keys" {
                        if early_mk == MAP_KIND_STRING_INT() {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_STRING())
                        } else {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_INT())
                        }
                    } else {
                        if early_mk == MAP_KIND_STRING_INT() {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_INT())
                        } else {
                            g.list_elem_kinds.insert(var_name, LIST_ELEM_STRING())
                        }
                    }
                    g.variable_map.insert(var_name, result.val_id)
                    g.variable_types.insert(var_name, "list")
                    return gen_result(g, b)
                }
            }
        }
    }

    -- Early handling: string_split() calls return list allocas directly
    if len(type_str) == 0 and infer_val.kind == EXPR_FUNCTION_CALL() {
        let early_callee = *infer_val.callee
        if early_callee.kind == EXPR_IDENTIFIER() and early_callee.name == "string_split" {
            let result = generate_expr(g, b, infer_val)
            g = result.gen
            b = result.block
            g.list_elem_kinds.insert(var_name, LIST_ELEM_STRING())
            g.variable_map.insert(var_name, result.val_id)
            g.variable_types.insert(var_name, "list")
            return gen_result(g, b)
        }
    }

    -- Early handling: array literal [elem1, elem2, ...] without type annotation
    -- Treat as List[T] where T is inferred from the first element
    if infer_val.kind == EXPR_ARRAY_LITERAL() and infer_val.elements.len() > 0 {
        let arr_elem_type = infer_expr_type(g, infer_val.elements[0])
        let arr_elem_kind = if arr_elem_type == "i64" or arr_elem_type == "int" { LIST_ELEM_INT() }
            else if arr_elem_type == "f64" or arr_elem_type == "float" { LIST_ELEM_FLOAT() }
            else if arr_elem_type == "string" { LIST_ELEM_STRING() }
            else { LIST_ELEM_OTHER() }
        let result = generate_expr(g, b, infer_val)
        g = result.gen
        b = result.block
        g.list_elem_kinds.insert(var_name, arr_elem_kind)
        g.variable_map.insert(var_name, result.val_id)
        g.variable_types.insert(var_name, "list")
        return gen_result(g, b)
    }

    let mut ir_type = "i64"
    if len(type_str) > 0 {
        ir_type = map_type_name(type_str)
        -- If the type annotation is Result[T, E] or Option[T], preserve the full type string
        if starts_with(type_str, "Result[") or starts_with(type_str, "Option[") {
            ir_type = type_str
        }
    } else if infer_val.kind != EXPR_LITERAL_NULL() {
        ir_type = infer_expr_type(g, infer_val)
    }

    -- Additional check: if value is a function call and type is still i64,
    -- try infer_call_return_type to detect Result/Option return types
    if ir_type == "i64" and infer_val.kind == EXPR_FUNCTION_CALL() {
        let call_callee = *infer_val.callee
        if call_callee.kind == EXPR_IDENTIFIER() {
            let call_ret = infer_call_return_type(g, call_callee.name)
            if starts_with(call_ret, "Result[") or starts_with(call_ret, "result_") or call_ret == "result" {
                ir_type = call_ret
            }
            if starts_with(call_ret, "Option[") or starts_with(call_ret, "option_") or call_ret == "option" {
                ir_type = call_ret
            }
        }
    }

    let alloca_id = g.builder.next_id
    let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
    alloca_inst.alloc_type = ir_type
    alloca_inst.result_type = "ptr(" + ir_type + ")"
    alloca_inst.has_result = true
    b.instructions.push(alloca_inst)
    g.builder.next_id = g.builder.next_id + 1

    let val_expr = *stmt.let_value
    if val_expr.kind != EXPR_LITERAL_NULL() {
        let result = generate_expr(g, b, val_expr)
        g = result.gen
        b = result.block

        -- If the alloca type differs from the expression type, insert a cast
        -- This handles cases like `let x: i32 = 42` where the literal is i64
        let expr_type = infer_expr_type(g, val_expr)
        let mut store_val_id = result.val_id
        if expr_type != ir_type and (is_integer_type(ir_type) or is_float_type(ir_type)) {
            let cast_id = g.builder.next_id
            let mut cast_inst = ir_inst_new(cast_id, OP_CAST())
            cast_inst.cast_val = ir_val_inst(result.val_id)
            cast_inst.cast_from = expr_type
            cast_inst.cast_to = ir_type
            cast_inst.result_type = ir_type
            cast_inst.has_result = true
            b.instructions.push(cast_inst)
            g.builder.next_id = g.builder.next_id + 1
            store_val_id = cast_id
        }

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_inst(store_val_id)
        store_inst.has_result = false
        b.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        -- Track struct type
        if val_expr.kind == EXPR_STRUCT_LITERAL() {
            g.var_struct_types.insert(var_name, val_expr.type_name)
        }
        -- Track struct type from index access on a list of structs
        if val_expr.kind == EXPR_INDEX_ACCESS() {
            let idx_obj = *val_expr.object
            if idx_obj.kind == EXPR_IDENTIFIER() {
                let idx_var = idx_obj.name
                if g.list_elem_kinds.contains(idx_var) {
                    let ek = g.list_elem_kinds.get(idx_var)
                    if ek == LIST_ELEM_OTHER() and g.list_elem_types.contains(idx_var) {
                        let elem_t = g.list_elem_types.get(idx_var)
                        if g.struct_defs.contains(elem_t) {
                            g.var_struct_types.insert(var_name, elem_t)
                        }
                    }
                }
            }
        }
        if val_expr.kind == EXPR_IDENTIFIER() {
            if g.var_struct_types.contains(val_expr.name) {
                g.var_struct_types.insert(var_name, g.var_struct_types.get(val_expr.name))
            }
            if g.list_elem_kinds.contains(val_expr.name) {
                g.list_elem_kinds.insert(var_name, g.list_elem_kinds.get(val_expr.name))
            }
            if g.var_enum_types.contains(val_expr.name) {
                g.var_enum_types.insert(var_name, g.var_enum_types.get(val_expr.name))
            }
        }
        if val_expr.kind == EXPR_LAMBDA() {
            -- The lambda was just generated by generate_expr, which advanced lambda_counter.
            -- So the lambda name is __lambda_(counter-1).
            let actual_lambda_name = "__lambda_" + int_to_string(g.lambda_counter - 1)
            g.fn_ptr_vars.insert(var_name, actual_lambda_name)
        }
        -- If the value is a function name (identifier referencing a known function),
        -- register it in fn_ptr_vars so indirect calls work correctly
        if val_expr.kind == EXPR_IDENTIFIER() and starts_with(type_str, "fn(") {
            let fn_ref_name = val_expr.name
            -- Verify it's a known function
            let mut fn_found = false
            let mut fn_check = 0
            while fn_check < g.ir_mod.functions.len() {
                if g.ir_mod.functions[fn_check].name == fn_ref_name {
                    fn_found = true
                }
                fn_check = fn_check + 1
            }
            if fn_found {
                g.fn_ptr_vars.insert(var_name, fn_ref_name)
            }
        }
    }

    -- Track struct/enum type from type annotation
    if len(type_str) > 0 {
        if g.struct_defs.contains(type_str) {
            g.var_struct_types.insert(var_name, type_str)
        }
        if g.enum_defs.contains(type_str) {
            g.var_enum_types.insert(var_name, type_str)
        }
    }

    g.variable_map.insert(var_name, alloca_id)
    -- For function pointer type annotations, preserve the full fn(...) -> T string
    -- so that indirect call dispatch can detect it
    if starts_with(type_str, "fn(") {
        g.variable_types.insert(var_name, type_str)
    } else {
        g.variable_types.insert(var_name, ir_type)
    }
    return gen_result(g, b)
}

-- ============================================================
-- RETURN STATEMENT
-- ============================================================

fn generate_return(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    if stmt.has_ret_value {
        let result = generate_expr(g, b, *stmt.ret_value)
        g = result.gen
        b = result.block
        b.terminator = Box_new(term_ret(ir_val_inst(result.val_id)))
        b.has_terminator = true
    } else {
        b.terminator = Box_new(term_ret_void())
        b.has_terminator = true
    }

    return gen_result(g, b)
}

-- ============================================================
-- IF STATEMENT
-- ============================================================

fn generate_if_statement(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    -- Generate condition
    let cond_result = generate_expr(g, b, *stmt.if_cond)
    g = cond_result.gen
    b = cond_result.block

    let then_label = gen_next_label(g, "then")
    g = gen_advance_label(g)
    let else_label = gen_next_label(g, "else")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "if_end")
    g = gen_advance_label(g)

    -- Conditional branch
    if stmt.has_else_branch {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_result.val_id), then_label, else_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    } else {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_result.val_id), then_label, join_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    }

    -- Then block
    let mut then_block = ir_block_new(then_label)
    let then_result = generate_block_stmts(g, then_block, stmt.if_then)
    g = then_result.gen
    then_block = then_result.block
    if then_block.has_terminator == false {
        then_block.terminator = Box_new(term_br(join_label))
        then_block.has_terminator = true
    }
    g.completed_blocks.push(then_block)
    -- Else block (if present)
    let mut else_block = ir_block_new(else_label)
    if stmt.has_else_branch {
        let else_result = generate_block_stmts(g, else_block, stmt.if_else)
        g = else_result.gen
        else_block = else_result.block
        if else_block.has_terminator == false {
            else_block.terminator = Box_new(term_br(join_label))
            else_block.has_terminator = true
        }
        g.completed_blocks.push(else_block)
    }

    -- Join block continues
    let join_block = ir_block_new(join_label)

    return gen_result(g, join_block)
}

-- ============================================================
-- WHILE LOOP
-- ============================================================

fn generate_while_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    let header_label = gen_next_label(g, "while_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "while_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "while_exit")
    g = gen_advance_label(g)

    -- Save and set break/continue targets
    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    -- Branch to header
    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true
    g.completed_blocks.push(b)
    -- Header block: evaluate condition
    let mut header_block = ir_block_new(header_label)
    let cond_result = generate_expr(g, header_block, *stmt.while_cond)
    g = cond_result.gen
    header_block = cond_result.block

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cond_result.val_id), body_label, exit_label))
    header_block.has_terminator = true
    g.completed_blocks.push(header_block)
    -- Body block
    let mut body_block = ir_block_new(body_label)
    let body_result = generate_block_stmts(g, body_block, stmt.while_body)
    g = body_result.gen
    body_block = body_result.block
    if body_block.has_terminator == false {
        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }
    -- Push completed body block (b and header_block already pushed above)
    g.completed_blocks.push(body_block)
    -- Restore break/continue targets
    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- FOR LOOP
-- ============================================================

fn generate_for_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    let var_name = stmt.for_var

    -- Get the iterator expression
    let iter_expr = *stmt.for_iter

    -- Check for range-based for loop (iter is a Range expression)
    if iter_expr.kind == EXPR_RANGE() {
        return generate_range_for_loop(g, b, stmt, iter_expr)
    }

    -- Check for string literal iteration (for c in "abc")
    if iter_expr.kind == EXPR_LITERAL_STRING() {
        return generate_string_for_loop(g, b, stmt, "")
    }

    -- Check for string iteration (if iter is identifier and tracked as string type)
    if iter_expr.kind == EXPR_IDENTIFIER() {
        let iter_name = iter_expr.name

        -- Check for map iteration
        if g.map_var_kinds.contains(iter_name) {
            let mk = g.map_var_kinds.get(iter_name)
            return generate_map_for_in(g, b, stmt, iter_name, mk)
        }

        -- Check for string variable iteration
        if g.variable_types.contains(iter_name) {
            let vtype = g.variable_types.get(iter_name)
            if vtype == "string" {
                return generate_string_for_loop(g, b, stmt, iter_name)
            }
        }
    }

    -- Default: List iteration
    -- Generate the iterator expression to get the list value
    let iter_result = generate_expr(g, b, iter_expr)
    g = iter_result.gen
    b = iter_result.block
    let list_ptr = iter_result.val_id

    -- Determine list kind from iterator name or default
    let list_kind = if iter_expr.kind == EXPR_IDENTIFIER() and g.list_elem_kinds.contains(iter_expr.name) {
        g.list_elem_kinds.get(iter_expr.name)
    } else {
        LIST_ELEM_INT()
    }

    -- Get list length
    let len_fn = if list_kind == LIST_ELEM_INT() { "dm_list_int64_len" }
    else if list_kind == LIST_ELEM_FLOAT() { "dm_list_double_len" }
    else if list_kind == LIST_ELEM_STRING() { "dm_list_string_len" }
    else { "dm_list_generic_len" }

    let mut len_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    len_inst.callee = len_fn
    let mut len_args: List[IRValue] = []
    len_args.push(ir_val_inst(list_ptr))
    len_inst.call_args = len_args
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    let len_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Create index variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_zero = ir_inst_new(g.builder.next_id, OP_STORE())
    store_zero.store_ptr = ir_val_inst(idx_alloca_id)
    store_zero.store_val = ir_val_int(0)
    store_zero.has_result = false
    b.instructions.push(store_zero)
    g.builder.next_id = g.builder.next_id + 1

    -- Determine element type
    let iter_name = if iter_expr.kind == EXPR_IDENTIFIER() { iter_expr.name } else { "" }
    let elem_type = if list_kind == LIST_ELEM_INT() { "i64" }
    else if list_kind == LIST_ELEM_FLOAT() { "f64" }
    else if list_kind == LIST_ELEM_STRING() { "string" }
    else if list_kind == LIST_ELEM_OTHER() and len(iter_name) > 0 and g.list_elem_types.contains(iter_name) {
        g.list_elem_types.get(iter_name)
    }
    else { "i64" }

    -- Create element variable
    let elem_alloca_id = g.builder.next_id
    let mut elem_inst = ir_inst_new(elem_alloca_id, OP_ALLOCA())
    elem_inst.alloc_type = elem_type
    elem_inst.result_type = "ptr(" + elem_type + ")"
    elem_inst.has_result = true
    b.instructions.push(elem_inst)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, elem_alloca_id)
    g.variable_types.insert(var_name, elem_type)

    -- Labels
    let header_label = gen_next_label(g, "for_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "for_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "for_exit")
    g = gen_advance_label(g)

    -- Save break/continue targets
    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    -- Branch to header
    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header: check idx < len
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut cmp_inst = ir_inst_new(g.builder.next_id, OP_LT())
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(len_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body: get element, run body, increment
    let mut body_block = ir_block_new(body_label)

    let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx2.load_type = "i64"
    load_idx2.result_type = "i64"
    load_idx2.has_result = true
    body_block.instructions.push(load_idx2)
    let idx_val2_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Get element from list
    if list_kind == LIST_ELEM_OTHER() {
        -- Generic list: dm_list_generic_get(out_ptr, list_ptr, idx, elem_size)
        let elem_size = estimate_type_size_gen(g, elem_type)
        let mut get_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        get_inst.callee = "dm_list_generic_get"
        let mut get_args: List[IRValue] = []
        get_args.push(ir_val_inst(elem_alloca_id))
        get_args.push(ir_val_inst(list_ptr))
        get_args.push(ir_val_inst(idx_val2_id))
        get_args.push(ir_val_int(elem_size))
        get_inst.call_args = get_args
        get_inst.result_type = "void"
        get_inst.has_result = false
        body_block.instructions.push(get_inst)
        g.builder.next_id = g.builder.next_id + 1
        -- elem_alloca is already populated by dm_list_generic_get, no store needed
    } else {
        let get_fn = if list_kind == LIST_ELEM_INT() { "dm_list_int64_get" }
        else if list_kind == LIST_ELEM_FLOAT() { "dm_list_double_get" }
        else { "dm_list_string_get" }

        let mut get_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        get_inst.callee = get_fn
        let mut get_args: List[IRValue] = []
        get_args.push(ir_val_inst(list_ptr))
        get_args.push(ir_val_inst(idx_val2_id))
        get_inst.call_args = get_args
        get_inst.result_type = elem_type
        get_inst.has_result = true
        body_block.instructions.push(get_inst)
        let elem_val_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_elem = ir_inst_new(g.builder.next_id, OP_STORE())
        store_elem.store_ptr = ir_val_inst(elem_alloca_id)
        store_elem.store_val = ir_val_inst(elem_val_id)
        store_elem.has_result = false
        body_block.instructions.push(store_elem)
        g.builder.next_id = g.builder.next_id + 1
    }

    -- Generate body
    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    -- Increment index
    if body_block.has_terminator == false {
        let mut load_idx3 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx3.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx3.load_type = "i64"
        load_idx3.result_type = "i64"
        load_idx3.has_result = true
        body_block.instructions.push(load_idx3)
        let idx_val3_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val3_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_idx_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_idx_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    -- Restore break/continue
    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- RANGE FOR LOOP
-- ============================================================

fn generate_range_for_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt, range_expr: Expr) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.for_var

    -- Generate start and end values from the range expression
    let start_result = generate_expr(g, b, *range_expr.left)
    g = start_result.gen
    b = start_result.block

    let end_result = generate_expr(g, b, *range_expr.right)
    g = end_result.gen
    b = end_result.block

    -- Create loop variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_start = ir_inst_new(g.builder.next_id, OP_STORE())
    store_start.store_ptr = ir_val_inst(idx_alloca_id)
    store_start.store_val = ir_val_inst(start_result.val_id)
    store_start.has_result = false
    b.instructions.push(store_start)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, idx_alloca_id)
    g.variable_types.insert(var_name, "i64")

    -- Labels
    let header_label = gen_next_label(g, "range_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "range_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "range_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    -- Branch to header
    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header: check i < end (or i <= end for inclusive)
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Use LT for exclusive range (..), LE for inclusive range (..=)
    -- Range expr op == BINOP_LE means inclusive
    let cmp_op = if range_expr.op == BINOP_LE() { OP_LE() } else { OP_LT() }
    let mut cmp_inst = ir_inst_new(g.builder.next_id, cmp_op)
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(end_result.val_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body
    let mut body_block = ir_block_new(body_label)
    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    -- Increment
    if body_block.has_terminator == false {
        let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx2.load_type = "i64"
        load_idx2.result_type = "i64"
        load_idx2.has_result = true
        body_block.instructions.push(load_idx2)
        let idx_val2_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val2_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- STRING FOR LOOP
-- ============================================================

fn generate_string_for_loop(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt, iter_name: string) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.for_var

    -- Generate string value from the iterator expression
    let str_result = generate_expr(g, b, *stmt.for_iter)
    g = str_result.gen
    b = str_result.block

    -- Get string length
    let mut len_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    len_inst.callee = "dm_string_len"
    let mut len_args: List[IRValue] = []
    len_args.push(ir_val_inst(str_result.val_id))
    len_inst.call_args = len_args
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    let len_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Create index variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_zero = ir_inst_new(g.builder.next_id, OP_STORE())
    store_zero.store_ptr = ir_val_inst(idx_alloca_id)
    store_zero.store_val = ir_val_int(0)
    store_zero.has_result = false
    b.instructions.push(store_zero)
    g.builder.next_id = g.builder.next_id + 1

    -- Create element variable (string)
    let elem_alloca_id = g.builder.next_id
    let mut elem_inst = ir_inst_new(elem_alloca_id, OP_ALLOCA())
    elem_inst.alloc_type = "string"
    elem_inst.result_type = "ptr(string)"
    elem_inst.has_result = true
    b.instructions.push(elem_inst)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, elem_alloca_id)
    g.variable_types.insert(var_name, "string")

    let header_label = gen_next_label(g, "strfor_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "strfor_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "strfor_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut cmp_inst = ir_inst_new(g.builder.next_id, OP_LT())
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(len_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body
    let mut body_block = ir_block_new(body_label)
    let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx2.load_type = "i64"
    load_idx2.result_type = "i64"
    load_idx2.has_result = true
    body_block.instructions.push(load_idx2)
    let idx_val2_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut char_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    char_inst.callee = "dm_char_at"
    let mut char_args: List[IRValue] = []
    char_args.push(ir_val_inst(str_result.val_id))
    char_args.push(ir_val_inst(idx_val2_id))
    char_inst.call_args = char_args
    char_inst.result_type = "string"
    char_inst.has_result = true
    body_block.instructions.push(char_inst)
    let char_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut store_elem = ir_inst_new(g.builder.next_id, OP_STORE())
    store_elem.store_ptr = ir_val_inst(elem_alloca_id)
    store_elem.store_val = ir_val_inst(char_id)
    store_elem.has_result = false
    body_block.instructions.push(store_elem)
    g.builder.next_id = g.builder.next_id + 1

    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    if body_block.has_terminator == false {
        let mut load_idx3 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx3.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx3.load_type = "i64"
        load_idx3.result_type = "i64"
        load_idx3.has_result = true
        body_block.instructions.push(load_idx3)
        let idx_val3_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val3_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- MAP FOR-IN LOOP
-- ============================================================

fn generate_map_for_in(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt, iter_name: string, mk: int) -> GenResult {
    let mut g = gen
    let mut b = block
    let var_name = stmt.for_var

    let map_ptr = if g.variable_map.contains(iter_name) {
        g.variable_map.get(iter_name)
    } else {
        0
    }

    -- Build keys list
    let key_elem_kind = if mk == MAP_KIND_STRING_INT() { LIST_ELEM_STRING() } else { LIST_ELEM_INT() }
    let key_type = if key_elem_kind == LIST_ELEM_STRING() { "string" } else { "i64" }
    let list_type = if key_elem_kind == LIST_ELEM_STRING() { "dm_list_dm_string" } else { "dm_list_int64" }

    -- Alloca for keys list
    let keys_alloca_id = g.builder.next_id
    let mut keys_inst = ir_inst_new(keys_alloca_id, OP_ALLOCA())
    keys_inst.alloc_type = list_type
    keys_inst.result_type = "ptr(" + list_type + ")"
    keys_inst.has_result = true
    b.instructions.push(keys_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Call keys function
    let keys_fn = if mk == MAP_KIND_STRING_INT() { "dm_map_string_int_keys" } else { "dm_map_int_string_keys" }
    let mut keys_call = ir_inst_new(g.builder.next_id, OP_CALL())
    keys_call.callee = keys_fn
    let mut kargs: List[IRValue] = []
    kargs.push(ir_val_inst(keys_alloca_id))
    kargs.push(ir_val_inst(map_ptr))
    keys_call.call_args = kargs
    keys_call.result_type = "void"
    keys_call.has_result = false
    b.instructions.push(keys_call)
    g.builder.next_id = g.builder.next_id + 1

    -- Get keys list length
    let len_fn = if key_elem_kind == LIST_ELEM_STRING() { "dm_list_string_len" } else { "dm_list_int64_len" }
    let mut len_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    len_inst.callee = len_fn
    let mut largs: List[IRValue] = []
    largs.push(ir_val_inst(keys_alloca_id))
    len_inst.call_args = largs
    len_inst.result_type = "i64"
    len_inst.has_result = true
    b.instructions.push(len_inst)
    let len_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Index variable
    let idx_alloca_id = g.builder.next_id
    let mut idx_inst = ir_inst_new(idx_alloca_id, OP_ALLOCA())
    idx_inst.alloc_type = "i64"
    idx_inst.result_type = "ptr(i64)"
    idx_inst.has_result = true
    b.instructions.push(idx_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_zero = ir_inst_new(g.builder.next_id, OP_STORE())
    store_zero.store_ptr = ir_val_inst(idx_alloca_id)
    store_zero.store_val = ir_val_int(0)
    store_zero.has_result = false
    b.instructions.push(store_zero)
    g.builder.next_id = g.builder.next_id + 1

    -- Element variable
    let elem_alloca_id = g.builder.next_id
    let mut elem_inst = ir_inst_new(elem_alloca_id, OP_ALLOCA())
    elem_inst.alloc_type = key_type
    elem_inst.result_type = "ptr(" + key_type + ")"
    elem_inst.has_result = true
    b.instructions.push(elem_inst)
    g.builder.next_id = g.builder.next_id + 1

    g.variable_map.insert(var_name, elem_alloca_id)
    g.variable_types.insert(var_name, key_type)

    let header_label = gen_next_label(g, "mapfor_header")
    g = gen_advance_label(g)
    let body_label = gen_next_label(g, "mapfor_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "mapfor_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = header_label
    g.has_break_target = true
    g.has_continue_target = true

    b.terminator = Box_new(term_br(header_label))
    b.has_terminator = true

    -- Header
    let mut header_block = ir_block_new(header_label)
    let mut load_idx = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx.load_type = "i64"
    load_idx.result_type = "i64"
    load_idx.has_result = true
    header_block.instructions.push(load_idx)
    let idx_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut cmp_inst = ir_inst_new(g.builder.next_id, OP_LT())
    cmp_inst.lhs = ir_val_inst(idx_val_id)
    cmp_inst.rhs = ir_val_inst(len_id)
    cmp_inst.result_type = "bool"
    cmp_inst.has_result = true
    header_block.instructions.push(cmp_inst)
    let cmp_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    header_block.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), body_label, exit_label))
    header_block.has_terminator = true

    -- Body
    let mut body_block = ir_block_new(body_label)
    let mut load_idx2 = ir_inst_new(g.builder.next_id, OP_LOAD())
    load_idx2.load_ptr = ir_val_inst(idx_alloca_id)
    load_idx2.load_type = "i64"
    load_idx2.result_type = "i64"
    load_idx2.has_result = true
    body_block.instructions.push(load_idx2)
    let idx_val2_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let get_fn = if key_elem_kind == LIST_ELEM_STRING() { "dm_list_string_get" } else { "dm_list_int64_get" }
    let mut get_inst = ir_inst_new(g.builder.next_id, OP_CALL())
    get_inst.callee = get_fn
    let mut gargs: List[IRValue] = []
    gargs.push(ir_val_inst(keys_alloca_id))
    gargs.push(ir_val_inst(idx_val2_id))
    get_inst.call_args = gargs
    get_inst.result_type = key_type
    get_inst.has_result = true
    body_block.instructions.push(get_inst)
    let elem_val_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    let mut store_elem = ir_inst_new(g.builder.next_id, OP_STORE())
    store_elem.store_ptr = ir_val_inst(elem_alloca_id)
    store_elem.store_val = ir_val_inst(elem_val_id)
    store_elem.has_result = false
    body_block.instructions.push(store_elem)
    g.builder.next_id = g.builder.next_id + 1

    let body_result = generate_block_stmts(g, body_block, stmt.for_body)
    g = body_result.gen
    body_block = body_result.block

    if body_block.has_terminator == false {
        let mut load_idx3 = ir_inst_new(g.builder.next_id, OP_LOAD())
        load_idx3.load_ptr = ir_val_inst(idx_alloca_id)
        load_idx3.load_type = "i64"
        load_idx3.result_type = "i64"
        load_idx3.has_result = true
        body_block.instructions.push(load_idx3)
        let idx_val3_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut add_inst = ir_inst_new(g.builder.next_id, OP_ADD())
        add_inst.lhs = ir_val_inst(idx_val3_id)
        add_inst.rhs = ir_val_int(1)
        add_inst.result_type = "i64"
        add_inst.has_result = true
        body_block.instructions.push(add_inst)
        let next_id = g.builder.next_id
        g.builder.next_id = g.builder.next_id + 1

        let mut store_idx = ir_inst_new(g.builder.next_id, OP_STORE())
        store_idx.store_ptr = ir_val_inst(idx_alloca_id)
        store_idx.store_val = ir_val_inst(next_id)
        store_idx.has_result = false
        body_block.instructions.push(store_idx)
        g.builder.next_id = g.builder.next_id + 1

        body_block.terminator = Box_new(term_br(header_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(header_block)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- LOOP STATEMENT (infinite loop)
-- ============================================================

fn generate_loop_stmt(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    let body_label = gen_next_label(g, "loop_body")
    g = gen_advance_label(g)
    let exit_label = gen_next_label(g, "loop_exit")
    g = gen_advance_label(g)

    let prev_break = g.break_target
    let prev_continue = g.continue_target
    let prev_has_break = g.has_break_target
    let prev_has_continue = g.has_continue_target
    g.break_target = exit_label
    g.continue_target = body_label
    g.has_break_target = true
    g.has_continue_target = true

    b.terminator = Box_new(term_br(body_label))
    b.has_terminator = true

    let mut body_block = ir_block_new(body_label)
    let body_result = generate_block_stmts(g, body_block, stmt.loop_body)
    g = body_result.gen
    body_block = body_result.block
    if body_block.has_terminator == false {
        body_block.terminator = Box_new(term_br(body_label))
        body_block.has_terminator = true
    }

    g.break_target = prev_break
    g.continue_target = prev_continue
    g.has_break_target = prev_has_break
    g.has_continue_target = prev_has_continue

    -- Push completed blocks
    g.completed_blocks.push(b)
    g.completed_blocks.push(body_block)

    let exit_block = ir_block_new(exit_label)
    return gen_result(g, exit_block)
}

-- ============================================================
-- BREAK / CONTINUE
-- ============================================================

fn generate_break(gen: IRGenerator, block: IRBasicBlock) -> GenResult {
    let mut b = block
    if gen.has_break_target {
        b.terminator = Box_new(term_br(gen.break_target))
        b.has_terminator = true
    }
    return gen_result(gen, b)
}

fn generate_continue(gen: IRGenerator, block: IRBasicBlock) -> GenResult {
    let mut b = block
    if gen.has_continue_target {
        b.terminator = Box_new(term_br(gen.continue_target))
        b.has_terminator = true
    }
    return gen_result(gen, b)
}

-- ============================================================
-- ASSIGNMENT
-- ============================================================

fn generate_assignment(gen: IRGenerator, block: IRBasicBlock, stmt: Stmt) -> GenResult {
    let mut g = gen
    let mut b = block

    -- Generate the value
    let val_result = generate_expr(g, b, *stmt.assign_value)
    g = val_result.gen
    b = val_result.block

    -- Get the target expression
    let target = *stmt.assign_target
    let assign_op = stmt.assign_op

    -- Simple identifier assignment
    if target.kind == EXPR_IDENTIFIER() {
        let target_name = target.name

        if assign_op == ASSIGN_EQ() {
            -- Simple assignment
            if g.variable_map.contains(target_name) {
                let ptr = g.variable_map.get(target_name)
                let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                -- For mut params, store through the param pointer (no alloca exists)
                if g.param_ptr_map.contains(target_name) {
                    let pidx = g.param_ptr_map.get(target_name)
                    store_inst.store_ptr = ir_val_param(pidx)
                } else {
                    store_inst.store_ptr = ir_val_inst(ptr)
                }
                store_inst.store_val = ir_val_inst(val_result.val_id)
                store_inst.has_result = false
                b.instructions.push(store_inst)
                g.builder.next_id = g.builder.next_id + 1
            }
        } else {
            -- Compound assignment (+=, -=, *=, /=)
            if g.variable_map.contains(target_name) {
                let ptr = g.variable_map.get(target_name)
                let ty = if g.variable_types.contains(target_name) {
                    g.variable_types.get(target_name)
                } else {
                    "i64"
                }

                -- Determine the pointer value for load/store
                -- For mut params, use the param pointer directly (no alloca exists)
                let mut ptr_val = ir_val_inst(ptr)
                if g.param_ptr_map.contains(target_name) {
                    let pidx = g.param_ptr_map.get(target_name)
                    ptr_val = ir_val_param(pidx)
                }

                -- Load current value
                let mut load_inst = ir_inst_new(g.builder.next_id, OP_LOAD())
                load_inst.load_ptr = ptr_val
                load_inst.load_type = ty
                load_inst.result_type = ty
                load_inst.has_result = true
                b.instructions.push(load_inst)
                let old_val_id = g.builder.next_id
                g.builder.next_id = g.builder.next_id + 1

                -- Compute new value
                let op = if assign_op == ASSIGN_ADD() { OP_ADD() }
                else if assign_op == ASSIGN_SUB() { OP_SUB() }
                else if assign_op == ASSIGN_MUL() { OP_MUL() }
                else if assign_op == ASSIGN_DIV() { OP_DIV() }
                else { OP_MOD() }

                let mut arith_inst = ir_inst_new(g.builder.next_id, op)
                arith_inst.lhs = ir_val_inst(old_val_id)
                arith_inst.rhs = ir_val_inst(val_result.val_id)
                arith_inst.result_type = ty
                arith_inst.has_result = true
                b.instructions.push(arith_inst)
                let new_val_id = g.builder.next_id
                g.builder.next_id = g.builder.next_id + 1

                -- Store new value
                let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                store_inst.store_ptr = ptr_val
                store_inst.store_val = ir_val_inst(new_val_id)
                store_inst.has_result = false
                b.instructions.push(store_inst)
                g.builder.next_id = g.builder.next_id + 1
            }
        }
    } else if target.kind == EXPR_FIELD_ACCESS() {
        -- Field assignment: obj.field = value
        -- Generate the target field access as an lvalue (get struct ptr + field index)
        -- For now, generate full struct load, insert_field, store back
        let obj = *target.object
        if obj.kind == EXPR_IDENTIFIER() and g.variable_map.contains(obj.name) {
            let ptr = g.variable_map.get(obj.name)
            let struct_name = if g.var_struct_types.contains(obj.name) {
                g.var_struct_types.get(obj.name)
            } else {
                ""
            }
            if len(struct_name) > 0 and g.struct_defs.contains(struct_name) {
                let info = g.struct_defs.get(struct_name)
                let field_name = target.field
                let mut field_idx = -1
                let mut fi = 0
                while fi < info.field_names.len() {
                    if info.field_names[fi] == field_name {
                        field_idx = fi
                    }
                    fi = fi + 1
                }
                if field_idx >= 0 {
                    -- Determine the struct pointer source:
                    -- For mut pointer params (mut self): use param ref directly
                    -- For regular vars: use the alloca instruction ref
                    let mut load_ptr_val = ir_val_inst(ptr)
                    if g.param_ptr_map.contains(obj.name) {
                        let pidx = g.param_ptr_map.get(obj.name)
                        load_ptr_val = ir_val_param(pidx)
                    }

                    -- Load current struct value
                    let mut load_inst = ir_inst_new(g.builder.next_id, OP_LOAD())
                    load_inst.load_ptr = load_ptr_val
                    load_inst.load_type = struct_name
                    load_inst.result_type = struct_name
                    load_inst.has_result = true
                    b.instructions.push(load_inst)
                    let struct_val_id = g.builder.next_id
                    g.builder.next_id = g.builder.next_id + 1

                    -- Insert new field value
                    let mut insert_inst = ir_inst_new(g.builder.next_id, OP_INSERT_FIELD())
                    insert_inst.field_base = ir_val_inst(struct_val_id)
                    insert_inst.field_value = ir_val_inst(val_result.val_id)
                    insert_inst.field_index = field_idx
                    insert_inst.result_type = struct_name
                    insert_inst.has_result = true
                    b.instructions.push(insert_inst)
                    let new_struct_id = g.builder.next_id
                    g.builder.next_id = g.builder.next_id + 1

                    -- Store back through the pointer
                    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
                    store_inst.store_ptr = load_ptr_val
                    store_inst.store_val = ir_val_inst(new_struct_id)
                    store_inst.has_result = false
                    b.instructions.push(store_inst)
                    g.builder.next_id = g.builder.next_id + 1
                }
            }
        }
    } else if target.kind == EXPR_INDEX_ACCESS() {
        -- Index assignment: arr[i] = value or map[key] = value
        -- Delegate to list_push or map_insert depending on type
        let obj = *target.object
        if obj.kind == EXPR_IDENTIFIER() {
            let obj_name = obj.name
            -- Map index assignment
            if g.map_var_kinds.contains(obj_name) {
                let mk = g.map_var_kinds.get(obj_name)
                let map_ptr = g.variable_map.get(obj_name)
                let insert_fn = if mk == MAP_KIND_STRING_INT() { "dm_map_string_int_insert" } else { "dm_map_int_string_insert" }

                -- Generate the key expression
                let key_result = generate_expr(g, b, *target.index)
                g = key_result.gen
                b = key_result.block

                let mut insert_inst = ir_inst_new(g.builder.next_id, OP_CALL())
                insert_inst.callee = insert_fn
                let mut iargs: List[IRValue] = []
                iargs.push(ir_val_inst(map_ptr))
                iargs.push(ir_val_inst(key_result.val_id))
                iargs.push(ir_val_inst(val_result.val_id))
                insert_inst.call_args = iargs
                insert_inst.result_type = "void"
                insert_inst.has_result = false
                b.instructions.push(insert_inst)
                g.builder.next_id = g.builder.next_id + 1
            }
        }
    }

    return gen_result(g, b)
}


-- ============================================================
-- EXPRESSION IR GENERATION
-- Main dispatcher and all expression kind handlers
-- Port of stage3/src/ir_gen.zig generateExpr
-- ============================================================

fn generate_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    if expr.kind == EXPR_LITERAL_INT() {
        return generate_literal_int(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_FLOAT() {
        return generate_literal_float(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_STRING() {
        return generate_literal_string(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_BOOL() {
        return generate_literal_bool(gen, block, expr)
    }
    if expr.kind == EXPR_LITERAL_NULL() {
        return generate_literal_null(gen, block)
    }
    if expr.kind == EXPR_IDENTIFIER() {
        return generate_identifier(gen, block, expr)
    }
    if expr.kind == EXPR_BINARY() {
        return generate_binary(gen, block, expr)
    }
    if expr.kind == EXPR_UNARY() {
        return generate_unary(gen, block, expr)
    }
    if expr.kind == EXPR_FUNCTION_CALL() {
        return generate_function_call(gen, block, expr)
    }
    if expr.kind == EXPR_METHOD_CALL() {
        return generate_method_call(gen, block, expr)
    }
    if expr.kind == EXPR_FIELD_ACCESS() {
        return generate_field_access(gen, block, expr)
    }
    if expr.kind == EXPR_INDEX_ACCESS() {
        return generate_index_access(gen, block, expr)
    }
    if expr.kind == EXPR_STRUCT_LITERAL() {
        return generate_struct_literal(gen, block, expr)
    }
    if expr.kind == EXPR_ENUM_LITERAL() {
        return generate_enum_literal(gen, block, expr)
    }
    if expr.kind == EXPR_ARRAY_LITERAL() {
        return generate_array_literal(gen, block, expr)
    }
    if expr.kind == EXPR_IF() {
        return generate_if_expr(gen, block, expr)
    }
    if expr.kind == EXPR_MATCH() {
        return generate_match_expr(gen, block, expr)
    }
    if expr.kind == EXPR_BLOCK() {
        return generate_block_expr(gen, block, expr)
    }
    if expr.kind == EXPR_LAMBDA() {
        return generate_lambda_expr(gen, block, expr)
    }
    if expr.kind == EXPR_PIPELINE() {
        return generate_pipeline(gen, block, expr)
    }
    if expr.kind == EXPR_ERROR_PROPAGATE() {
        return generate_error_propagate(gen, block, expr)
    }
    if expr.kind == EXPR_CAST() {
        return generate_cast_expr(gen, block, expr)
    }
    if expr.kind == EXPR_STRING_INTERP() {
        return generate_string_interp(gen, block, expr)
    }
    if expr.kind == EXPR_GROUPED() {
        return generate_expr(gen, block, *expr.operand)
    }
    if expr.kind == EXPR_COMPTIME() {
        return generate_comptime_expr(gen, block, expr)
    }
    if expr.kind == EXPR_AWAIT() {
        -- Phase A synchronous semantics: await simply evaluates the inner expression
        return generate_expr(gen, block, *expr.operand)
    }
    -- Default: return undef
    let id = gen.builder.next_id
    let mut g = gen
    let mut b = block
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- ============================================================
-- LITERALS
-- ============================================================

fn generate_literal_int(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_int(expr.int_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "i64"
    load.result_type = "i64"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_float(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "f64"
    inst.result_type = "ptr(f64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_float(expr.float_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "f64"
    load.result_type = "f64"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_string(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "string"
    inst.result_type = "ptr(string)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_string(expr.str_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "string"
    load.result_type = "string"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_bool(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "bool"
    inst.result_type = "ptr(bool)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_bool(expr.bool_val)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let mut load = ir_inst_new(g.builder.next_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "bool"
    load.result_type = "bool"
    load.has_result = true
    b.instructions.push(load)
    let load_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1
    return expr_result(g, b, load_id)
}

fn generate_literal_null(gen: IRGenerator, block: IRBasicBlock) -> ExprResult {
    let mut g = gen
    let mut b = block
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "ptr"
    inst.result_type = "ptr(ptr)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_int(0)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- IDENTIFIER
-- ============================================================

fn generate_identifier(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let name = expr.name

    -- Check for "true" / "false" literals
    if name == "true" {
        return generate_literal_bool(g, b, expr_bool(true, 0, 0))
    }
    if name == "false" {
        return generate_literal_bool(g, b, expr_bool(false, 0, 0))
    }

    -- Check for "None" (Option None)
    if name == "None" {
        -- Build Option { has_value: 0, value: undef }
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_ALLOCA())
        inst.alloc_type = "i64"
        inst.result_type = "ptr(i64)"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1

        let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
        store.store_ptr = ir_val_inst(id)
        store.store_val = ir_val_int(0)
        store.has_result = false
        b.instructions.push(store)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- Look up variable in scope
    if g.variable_map.contains(name) {
        let alloca_id = g.variable_map.get(name)
        let var_type = if g.variable_types.contains(name) {
            g.variable_types.get(name)
        } else {
            "i64"
        }

        -- For mut pointer vars (mut self): load struct through the param pointer.
        -- The param IS a pointer to the struct (no alloca was created).
        if g.param_ptr_map.contains(name) {
            let param_idx = g.param_ptr_map.get(name)
            let load_id = g.builder.next_id
            let mut load_struct = ir_inst_new(load_id, OP_LOAD())
            load_struct.load_ptr = ir_val_param(param_idx)
            load_struct.load_type = var_type
            load_struct.result_type = var_type
            load_struct.has_result = true
            b.instructions.push(load_struct)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, load_id)
        }

        let load_id = g.builder.next_id
        let mut load = ir_inst_new(load_id, OP_LOAD())
        load.load_ptr = ir_val_inst(alloca_id)
        load.load_type = var_type
        load.result_type = var_type
        load.has_result = true
        b.instructions.push(load)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, load_id)
    }

    -- Check for global constants (const SIZE = 4096, etc.)
    let mut gi = 0
    while gi < g.ir_mod.globals.len() {
        let glob = g.ir_mod.globals[gi]
        if glob.name == name and glob.has_init {
            let init = glob.init_value
            if init.kind == VAL_CONST_INT() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "i64"
                ai.result_type = "ptr(i64)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_int(init.int_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "i64"
                li.result_type = "i64"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
            if init.kind == VAL_CONST_FLOAT() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "f64"
                ai.result_type = "ptr(f64)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_float(init.float_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "f64"
                li.result_type = "f64"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
            if init.kind == VAL_CONST_BOOL() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "bool"
                ai.result_type = "ptr(bool)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_bool(init.bool_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "bool"
                li.result_type = "bool"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
            if init.kind == VAL_CONST_STRING() {
                let id = g.builder.next_id
                let mut ai = ir_inst_new(id, OP_ALLOCA())
                ai.alloc_type = "string"
                ai.result_type = "ptr(string)"
                ai.has_result = true
                b.instructions.push(ai)
                let sid = id + 1
                let mut si = ir_inst_new(sid, OP_STORE())
                si.store_ptr = ir_val_inst(id)
                si.store_val = ir_val_string(init.str_val)
                si.has_result = false
                b.instructions.push(si)
                g.builder.next_id = sid + 1
                let lid = g.builder.next_id
                let mut li = ir_inst_new(lid, OP_LOAD())
                li.load_ptr = ir_val_inst(id)
                li.load_type = "string"
                li.result_type = "string"
                li.has_result = true
                b.instructions.push(li)
                g.builder.next_id = lid + 1
                return expr_result(g, b, lid)
            }
        }
        gi = gi + 1
    }

    -- Check if the identifier is a known function name (function pointer reference)
    let mut fi = 0
    while fi < g.ir_mod.functions.len() {
        let func = g.ir_mod.functions[fi]
        if func.name == name {
            -- Return a global reference to the function
            let ref_id = g.builder.next_id
            let mut ref_inst = ir_inst_new(ref_id, OP_ALLOCA())
            ref_inst.alloc_type = "ptr"
            ref_inst.result_type = "ptr(ptr)"
            ref_inst.has_result = true
            b.instructions.push(ref_inst)
            g.builder.next_id = g.builder.next_id + 1

            let mut store_ref = ir_inst_new(g.builder.next_id, OP_STORE())
            store_ref.store_ptr = ir_val_inst(ref_id)
            store_ref.store_val = ir_val_global(name)
            store_ref.has_result = false
            b.instructions.push(store_ref)
            g.builder.next_id = g.builder.next_id + 1

            let load_id = g.builder.next_id
            let mut load_inst = ir_inst_new(load_id, OP_LOAD())
            load_inst.load_ptr = ir_val_inst(ref_id)
            load_inst.load_type = "ptr"
            load_inst.result_type = "ptr"
            load_inst.has_result = true
            b.instructions.push(load_inst)
            g.builder.next_id = g.builder.next_id + 1

            return expr_result(g, b, load_id)
        }
        fi = fi + 1
    }

    -- Unknown identifier - return undef
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- ============================================================
-- EXPRESSION TYPE INFERENCE
-- Determines the IR type name for an expression without
-- generating instructions. Used to select the correct opcode
-- family (integer, float, string) for binary operations.
-- Port of stage3/src/ir_gen.zig inferExprType
-- ============================================================

fn infer_expr_type(gen: IRGenerator, expr: Expr) -> string {
    if expr.kind == EXPR_LITERAL_INT() {
        return "i64"
    }
    if expr.kind == EXPR_LITERAL_FLOAT() {
        return "f64"
    }
    if expr.kind == EXPR_LITERAL_STRING() {
        return "string"
    }
    if expr.kind == EXPR_LITERAL_BOOL() {
        return "bool"
    }
    if expr.kind == EXPR_IDENTIFIER() {
        let name = expr.name
        if name == "true" or name == "false" {
            return "bool"
        }
        if gen.variable_types.contains(name) {
            return gen.variable_types.get(name)
        }
        -- Check global constants for their type
        let mut gi_ty = 0
        while gi_ty < gen.ir_mod.globals.len() {
            let gl_ty = gen.ir_mod.globals[gi_ty]
            if gl_ty.name == name and gl_ty.has_init {
                if gl_ty.init_value.kind == VAL_CONST_STRING() {
                    return "string"
                }
                if gl_ty.init_value.kind == VAL_CONST_FLOAT() {
                    return "f64"
                }
                if gl_ty.init_value.kind == VAL_CONST_BOOL() {
                    return "bool"
                }
                return "i64"
            }
            gi_ty = gi_ty + 1
        }
        return "i64"
    }
    if expr.kind == EXPR_BINARY() {
        let op = expr.op
        -- Comparison operators always produce bool
        if op == BINOP_EQ() or op == BINOP_NE() {
            return "bool"
        }
        if op == BINOP_LT() or op == BINOP_LE() or op == BINOP_GT() or op == BINOP_GE() {
            return "bool"
        }
        -- Logical operators always produce bool
        if op == BINOP_AND() or op == BINOP_OR() {
            return "bool"
        }
        -- Arithmetic: infer from left operand
        let left_type = infer_expr_type(gen, *expr.left)
        if left_type == "string" and op == BINOP_ADD() {
            return "string"
        }
        return left_type
    }
    if expr.kind == EXPR_UNARY() {
        if expr.op == UNOP_NOT() {
            return "bool"
        }
        -- Negation preserves the operand type
        return infer_expr_type(gen, *expr.operand)
    }
    if expr.kind == EXPR_CAST() {
        return map_type_name(expr.cast_type)
    }
    if expr.kind == EXPR_FUNCTION_CALL() {
        -- Check if callee is an identifier with a known builtin return type
        let callee = *expr.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let fn_name = callee.name
            if is_builtin_fn(fn_name) {
                return builtin_return_type(fn_name)
            }
            -- SIMD builtins: infer return type from function name or argument types
            if is_simd_builtin(fn_name) {
                if starts_with(fn_name, "simd_splat_") or starts_with(fn_name, "simd_set_") {
                    return parse_simd_type_from_name(fn_name)
                }
                if fn_name == "simd_add" or fn_name == "simd_sub" or fn_name == "simd_mul" or fn_name == "simd_div" {
                    -- Return type is the same as the first argument's vector type
                    if expr.args.len() > 0 {
                        return infer_expr_type(gen, expr.args[0])
                    }
                    return "f32x4"
                }
                if fn_name == "simd_extract" {
                    -- Return dAImond-native type (f64 for float vectors, i64 for int vectors)
                    if expr.args.len() > 0 {
                        let vec_ty = infer_expr_type(gen, expr.args[0])
                        if is_simd_float_type(vec_ty) {
                            return "f64"
                        }
                        return "i64"
                    }
                    return "f64"
                }
            }
            -- Look up user-defined function return types
            let mut fi = 0
            while fi < gen.ir_mod.functions.len() {
                let func = gen.ir_mod.functions[fi]
                if func.name == fn_name {
                    return func.return_type
                }
                fi = fi + 1
            }
            -- Check for generic function declarations
            if gen.generic_fn_decls.contains(fn_name) {
                let decl_idx = gen.generic_fn_decls.get(fn_name)
                if decl_idx >= 0 and decl_idx < gen.declarations.len() {
                    let decl = gen.declarations[decl_idx]
                    if decl.kind == DECL_FUNCTION() {
                        let fd = *decl.func_decl
                        let ret_type = fd.return_type
                        -- If return type is a generic param, resolve from args
                        if fd.generic_params.len() > 0 {
                            let mut pi = 0
                            while pi < fd.generic_params.len() {
                                if fd.generic_params[pi] == ret_type {
                                    -- Infer from corresponding argument
                                    if pi < expr.args.len() {
                                        return infer_expr_type(gen, expr.args[pi])
                                    }
                                }
                                pi = pi + 1
                            }
                            -- Also check explicit type args
                            if expr.generic_args.len() > 0 {
                                let mut gi = 0
                                while gi < fd.generic_params.len() {
                                    if fd.generic_params[gi] == ret_type and gi < expr.generic_args.len() {
                                        return map_type_name(expr.generic_args[gi])
                                    }
                                    gi = gi + 1
                                }
                            }
                        }
                        return map_type_name(ret_type)
                    }
                }
            }
            -- Check if callee is a lambda/fn-ptr variable: look up actual function
            if gen.fn_ptr_vars.contains(fn_name) {
                let lambda_fn_name = gen.fn_ptr_vars.get(fn_name)
                let mut lfi = 0
                while lfi < gen.ir_mod.functions.len() {
                    if gen.ir_mod.functions[lfi].name == lambda_fn_name {
                        return gen.ir_mod.functions[lfi].return_type
                    }
                    lfi = lfi + 1
                }
            }
            -- Check if callee is a function-pointer-typed variable: parse return type
            if gen.variable_types.contains(fn_name) {
                let vtype = gen.variable_types.get(fn_name)
                if starts_with(vtype, "fn(") {
                    let arrow_pos = string_find(vtype, " -> ")
                    if arrow_pos >= 0 {
                        let ret_str = substr(vtype, arrow_pos + 4, len(vtype) - arrow_pos - 4)
                        return map_type_name(ret_str)
                    }
                }
            }
        }
        return "i64"
    }
    if expr.kind == EXPR_STRING_INTERP() {
        return "string"
    }
    if expr.kind == EXPR_GROUPED() {
        return infer_expr_type(gen, *expr.operand)
    }
    if expr.kind == EXPR_PIPELINE() {
        -- Pipeline: a |> f — return type of function f
        let rhs = *expr.pipe_right
        if rhs.kind == EXPR_FUNCTION_CALL() {
            let callee = *rhs.callee
            if callee.kind == EXPR_IDENTIFIER() {
                let fn_name = callee.name
                if is_builtin_fn(fn_name) {
                    return builtin_return_type(fn_name)
                }
                let mut fi = 0
                while fi < gen.ir_mod.functions.len() {
                    let func = gen.ir_mod.functions[fi]
                    if func.name == fn_name {
                        return func.return_type
                    }
                    fi = fi + 1
                }
            }
        } else if rhs.kind == EXPR_IDENTIFIER() {
            let fn_name = rhs.name
            if is_builtin_fn(fn_name) {
                return builtin_return_type(fn_name)
            }
            let mut fi = 0
            while fi < gen.ir_mod.functions.len() {
                let func = gen.ir_mod.functions[fi]
                if func.name == fn_name {
                    return func.return_type
                }
                fi = fi + 1
            }
        }
        return "i64"
    }
    if expr.kind == EXPR_STRUCT_LITERAL() {
        return expr.type_name
    }
    if expr.kind == EXPR_ENUM_LITERAL() {
        return expr.enum_name
    }
    if expr.kind == EXPR_IF() {
        -- Return type of then branch (then_branch is Box[Expr] to EXPR_BLOCK)
        let tb = *expr.then_branch
        if tb.kind == EXPR_BLOCK() and tb.stmts.len() > 0 {
            let last = tb.stmts[tb.stmts.len() - 1]
            if last.kind == STMT_EXPRESSION() {
                return infer_expr_type(gen, *last.expr)
            }
        }
        return "void"
    }
    if expr.kind == EXPR_MATCH() {
        -- Return type of first arm
        if expr.match_arms.len() > 0 {
            let arm = expr.match_arms[0]
            if arm.is_expr_body {
                return infer_expr_type(gen, *arm.body_expr)
            }
        }
        return "void"
    }
    if expr.kind == EXPR_FIELD_ACCESS() {
        -- Look up struct field type
        let obj = *expr.object
        let obj_type = infer_expr_type(gen, obj)
        if gen.struct_defs.contains(obj_type) {
            let si = gen.struct_defs.get(obj_type)
            let mut fi = 0
            while fi < si.field_names.len() {
                if si.field_names[fi] == expr.field {
                    return si.field_types[fi]
                }
                fi = fi + 1
            }
        }
        -- Check variable's struct type
        if obj.kind == EXPR_IDENTIFIER() and gen.var_struct_types.contains(obj.name) {
            let sname = gen.var_struct_types.get(obj.name)
            if gen.struct_defs.contains(sname) {
                let si = gen.struct_defs.get(sname)
                let mut fi = 0
                while fi < si.field_names.len() {
                    if si.field_names[fi] == expr.field {
                        return si.field_types[fi]
                    }
                    fi = fi + 1
                }
            }
        }
        -- Check Option[T] / Result[T, E] types for field access
        if obj.kind == EXPR_IDENTIFIER() and gen.variable_types.contains(obj.name) {
            let fa_vtype = gen.variable_types.get(obj.name)
            if starts_with(fa_vtype, "Option[") {
                let fa_inner = substr(fa_vtype, 7, len(fa_vtype) - 8)
                if expr.field == "value" {
                    return map_type_name(fa_inner)
                }
                if expr.field == "has_value" {
                    return "i32"
                }
            }
            if starts_with(fa_vtype, "Result[") {
                let fa_inner_str = substr(fa_vtype, 7, len(fa_vtype) - 8)
                let mut fa_comma = 0
                let mut fa_ci = 0
                while fa_ci < len(fa_inner_str) {
                    if char_at(fa_inner_str, fa_ci) == "," {
                        fa_comma = fa_ci
                    }
                    fa_ci = fa_ci + 1
                }
                let fa_ok_type = string_trim(substr(fa_inner_str, 0, fa_comma))
                let fa_err_type = string_trim(substr(fa_inner_str, fa_comma + 1, len(fa_inner_str) - fa_comma - 1))
                if expr.field == "ok" or expr.field == "value" {
                    return map_type_name(fa_ok_type)
                }
                if expr.field == "err" or expr.field == "error" {
                    return map_type_name(fa_err_type)
                }
                if expr.field == "is_ok" {
                    return "bool"
                }
            }
        }
        return "i64"
    }
    if expr.kind == EXPR_INDEX_ACCESS() {
        let obj = *expr.object
        if obj.kind == EXPR_IDENTIFIER() {
            let name = obj.name
            -- String indexing returns a string (char_at)
            if gen.variable_types.contains(name) {
                let idx_vtype = gen.variable_types.get(name)
                if idx_vtype == "string" {
                    return "string"
                }
            }
            if gen.list_elem_kinds.contains(name) {
                let ek = gen.list_elem_kinds.get(name)
                if ek == LIST_ELEM_INT() { return "i64" }
                if ek == LIST_ELEM_FLOAT() { return "f64" }
                if ek == LIST_ELEM_STRING() { return "string" }
                if ek == LIST_ELEM_OTHER() and gen.list_elem_types.contains(name) {
                    return gen.list_elem_types.get(name)
                }
            }
            -- Map index access
            if gen.map_var_kinds.contains(name) {
                let idx_mk = gen.map_var_kinds.get(name)
                if idx_mk == MAP_KIND_STRING_INT() { return "i64" }
                if idx_mk == MAP_KIND_INT_STRING() { return "string" }
            }
        }
        return "i64"
    }
    if expr.kind == EXPR_METHOD_CALL() {
        let method = expr.method
        if method == "len" { return "i64" }
        if method == "pop" { return "i64" }
        if method == "get" {
            -- Check if object is a map variable to determine get return type
            let get_obj = *expr.object
            if get_obj.kind == EXPR_IDENTIFIER() {
                let get_var = get_obj.name
                if gen.map_var_kinds.contains(get_var) {
                    let mk = gen.map_var_kinds.get(get_var)
                    if mk == MAP_KIND_INT_STRING() {
                        return "string"
                    }
                }
            }
            return "i64"
        }
        if method == "contains" { return "bool" }
        -- Check for struct method calls (regular and generic impl)
        if gen.method_struct_map.contains(method) {
            let struct_type = gen.method_struct_map.get(method)
            let mangled = struct_type + "_" + method
            -- Check generic impl method
            if gen.generic_fn_decls.contains(mangled) {
                let decl_idx = gen.generic_fn_decls.get(mangled)
                if decl_idx >= 0 and decl_idx < gen.declarations.len() {
                    let decl = gen.declarations[decl_idx]
                    if decl.kind == DECL_FUNCTION() {
                        let fd = *decl.func_decl
                        -- Check if method takes self
                        let mut has_self = false
                        if fd.params.len() > 0 {
                            has_self = fd.params[0].name == "self"
                        }
                        -- Build effective args
                        let mut effective_args: List[Expr] = []
                        if has_self {
                            effective_args.push(*expr.object)
                        }
                        let mut ai = 0
                        while ai < expr.args.len() {
                            effective_args.push(expr.args[ai])
                            ai = ai + 1
                        }
                        -- Infer type args
                        let mut temp_subs: Map[string, string] = Map_new()
                        let mut pi = 0
                        while pi < fd.generic_params.len() {
                            let gp = fd.generic_params[pi]
                            let mut inferred = "i64"
                            let mut mi = 0
                            while mi < fd.params.len() and mi < effective_args.len() {
                                if fd.params[mi].type_name == gp {
                                    inferred = infer_arg_type(gen, effective_args[mi])
                                }
                                mi = mi + 1
                            }
                            temp_subs.insert(gp, inferred)
                            pi = pi + 1
                        }
                        -- Resolve return type
                        let ret = fd.return_type
                        if temp_subs.contains(ret) {
                            return map_type_name(temp_subs.get(ret))
                        }
                        -- Handle complex return types like Pair[A, B]
                        let bracket_idx = string_find(ret, "[")
                        if bracket_idx > 0 {
                            let base = substr(ret, 0, bracket_idx)
                            let inner = substr(ret, bracket_idx + 1, len(ret) - bracket_idx - 2)
                            let comma_idx = string_find(inner, ", ")
                            if comma_idx >= 0 {
                                let first_t = substr(inner, 0, comma_idx)
                                let second_t = substr(inner, comma_idx + 2, len(inner) - comma_idx - 2)
                                let r1 = if temp_subs.contains(first_t) { map_type_name(temp_subs.get(first_t)) } else { map_type_name(first_t) }
                                let r2 = if temp_subs.contains(second_t) { map_type_name(temp_subs.get(second_t)) } else { map_type_name(second_t) }
                                return base + "_" + r1 + "_" + r2
                            } else {
                                let r1 = if temp_subs.contains(inner) { map_type_name(temp_subs.get(inner)) } else { map_type_name(inner) }
                                return base + "_" + r1
                            }
                        }
                        return map_type_name(ret)
                    }
                }
            }
            -- Non-generic struct method: look up return type from IR functions
            let ret_type = infer_call_return_type(gen, mangled)
            return ret_type
        }
        return "i64"
    }
    if expr.kind == EXPR_COMPTIME() {
        return infer_expr_type(gen, *expr.operand)
    }
    if expr.kind == EXPR_ARRAY_LITERAL() {
        return "i64"
    }
    if expr.kind == EXPR_AWAIT() {
        -- Await returns the type of the inner expression
        return infer_expr_type(gen, *expr.operand)
    }
    -- Default fallback
    return "i64"
}

-- ============================================================
-- BINARY EXPRESSION IR GENERATION
-- Handles arithmetic, comparison, logical, and string operators
-- Port of stage3/src/ir_gen.zig generateBinary
-- ============================================================

fn generate_binary(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block
    let op = expr.op

    -- --------------------------------------------------------
    -- SHORT-CIRCUIT LOGICAL AND
    -- lhs && rhs: if lhs is false, result is false; else result is rhs
    -- --------------------------------------------------------
    if op == BINOP_AND() {
        -- Allocate a result slot for the bool
        let result_alloca_id = g.builder.next_id
        let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
        result_alloca.alloc_type = "bool"
        result_alloca.result_type = "ptr(bool)"
        result_alloca.has_result = true
        b.instructions.push(result_alloca)
        g.builder.next_id = g.builder.next_id + 1

        -- Evaluate left operand
        let lhs_result = generate_expr(g, b, *expr.left)
        g = lhs_result.gen
        b = lhs_result.block
        let lhs_id = lhs_result.val_id

        -- Labels for the branches
        let rhs_label = gen_next_label(g, "and_rhs")
        g = gen_advance_label(g)
        let short_label = gen_next_label(g, "and_short")
        g = gen_advance_label(g)
        let join_label = gen_next_label(g, "and_join")
        g = gen_advance_label(g)

        -- Branch: if lhs is true, evaluate rhs; else short-circuit to false
        b.terminator = Box_new(term_br_cond(ir_val_inst(lhs_id), rhs_label, short_label))
        b.has_terminator = true
        g.completed_blocks.push(b)

        -- RHS block: evaluate rhs, store result, branch to join
        let mut rhs_block = ir_block_new(rhs_label)
        let rhs_result = generate_expr(g, rhs_block, *expr.right)
        g = rhs_result.gen
        rhs_block = rhs_result.block
        let rhs_id = rhs_result.val_id

        let mut store_rhs = ir_inst_new(g.builder.next_id, OP_STORE())
        store_rhs.store_ptr = ir_val_inst(result_alloca_id)
        store_rhs.store_val = ir_val_inst(rhs_id)
        store_rhs.has_result = false
        rhs_block.instructions.push(store_rhs)
        g.builder.next_id = g.builder.next_id + 1

        rhs_block.terminator = Box_new(term_br(join_label))
        rhs_block.has_terminator = true
        g.completed_blocks.push(rhs_block)

        -- Short-circuit block: store false, branch to join
        let mut short_block = ir_block_new(short_label)
        let mut store_false = ir_inst_new(g.builder.next_id, OP_STORE())
        store_false.store_ptr = ir_val_inst(result_alloca_id)
        store_false.store_val = ir_val_bool(false)
        store_false.has_result = false
        short_block.instructions.push(store_false)
        g.builder.next_id = g.builder.next_id + 1

        short_block.terminator = Box_new(term_br(join_label))
        short_block.has_terminator = true
        g.completed_blocks.push(short_block)

        -- Join block: load the result
        let mut join_block = ir_block_new(join_label)
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(result_alloca_id)
        load_inst.load_type = "bool"
        load_inst.result_type = "bool"
        load_inst.has_result = true
        join_block.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, join_block, load_id)
    }

    -- --------------------------------------------------------
    -- SHORT-CIRCUIT LOGICAL OR
    -- lhs || rhs: if lhs is true, result is true; else result is rhs
    -- --------------------------------------------------------
    if op == BINOP_OR() {
        -- Allocate a result slot for the bool
        let result_alloca_id = g.builder.next_id
        let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
        result_alloca.alloc_type = "bool"
        result_alloca.result_type = "ptr(bool)"
        result_alloca.has_result = true
        b.instructions.push(result_alloca)
        g.builder.next_id = g.builder.next_id + 1

        -- Evaluate left operand
        let lhs_result = generate_expr(g, b, *expr.left)
        g = lhs_result.gen
        b = lhs_result.block
        let lhs_id = lhs_result.val_id

        -- Labels for the branches
        let short_label = gen_next_label(g, "or_short")
        g = gen_advance_label(g)
        let rhs_label = gen_next_label(g, "or_rhs")
        g = gen_advance_label(g)
        let join_label = gen_next_label(g, "or_join")
        g = gen_advance_label(g)

        -- Branch: if lhs is true, short-circuit to true; else evaluate rhs
        b.terminator = Box_new(term_br_cond(ir_val_inst(lhs_id), short_label, rhs_label))
        b.has_terminator = true
        g.completed_blocks.push(b)

        -- Short-circuit block: store true, branch to join
        let mut short_block = ir_block_new(short_label)
        let mut store_true = ir_inst_new(g.builder.next_id, OP_STORE())
        store_true.store_ptr = ir_val_inst(result_alloca_id)
        store_true.store_val = ir_val_bool(true)
        store_true.has_result = false
        short_block.instructions.push(store_true)
        g.builder.next_id = g.builder.next_id + 1

        short_block.terminator = Box_new(term_br(join_label))
        short_block.has_terminator = true
        g.completed_blocks.push(short_block)

        -- RHS block: evaluate rhs, store result, branch to join
        let mut rhs_block = ir_block_new(rhs_label)
        let rhs_result = generate_expr(g, rhs_block, *expr.right)
        g = rhs_result.gen
        rhs_block = rhs_result.block
        let rhs_id = rhs_result.val_id

        let mut store_rhs = ir_inst_new(g.builder.next_id, OP_STORE())
        store_rhs.store_ptr = ir_val_inst(result_alloca_id)
        store_rhs.store_val = ir_val_inst(rhs_id)
        store_rhs.has_result = false
        rhs_block.instructions.push(store_rhs)
        g.builder.next_id = g.builder.next_id + 1

        rhs_block.terminator = Box_new(term_br(join_label))
        rhs_block.has_terminator = true
        g.completed_blocks.push(rhs_block)

        -- Join block: load the result
        let mut join_block = ir_block_new(join_label)
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(result_alloca_id)
        load_inst.load_type = "bool"
        load_inst.result_type = "bool"
        load_inst.has_result = true
        join_block.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, join_block, load_id)
    }

    -- --------------------------------------------------------
    -- OPERATOR OVERLOADING: check if left operand is a struct
    -- with an impl method for this operator
    -- --------------------------------------------------------
    let mut overload_method = ""
    if op == BINOP_ADD() { overload_method = "add" }
    if op == BINOP_SUB() { overload_method = "sub" }
    if op == BINOP_MUL() { overload_method = "mul" }
    if op == BINOP_DIV() { overload_method = "div" }
    if op == BINOP_EQ() { overload_method = "eq" }
    if op == BINOP_NE() { overload_method = "ne" }
    if op == BINOP_LT() { overload_method = "lt" }
    if op == BINOP_GT() { overload_method = "gt" }
    if op == BINOP_LE() { overload_method = "le" }
    if op == BINOP_GE() { overload_method = "ge" }

    if len(overload_method) > 0 {
        let left_expr = *expr.left
        let mut left_struct_name = ""
        if left_expr.kind == EXPR_IDENTIFIER() {
            if g.var_struct_types.contains(left_expr.name) {
                left_struct_name = g.var_struct_types.get(left_expr.name)
            }
        }
        if left_struct_name == "" and left_expr.kind == EXPR_IDENTIFIER() {
            let ltype = infer_expr_type(g, left_expr)
            if g.struct_defs.contains(ltype) {
                left_struct_name = ltype
            }
        }

        if len(left_struct_name) > 0 {
            let mangled = left_struct_name + "_" + overload_method
            -- Check if method exists (declared or generic)
            let mut method_exists = false
            let mut fci = 0
            while fci < g.ir_mod.functions.len() {
                if g.ir_mod.functions[fci].name == mangled {
                    method_exists = true
                }
                fci = fci + 1
            }
            if method_exists == false and g.generic_fn_decls.contains(mangled) {
                method_exists = true
            }
            -- Also check pending declarations
            if method_exists == false {
                let mut dci = 0
                while dci < g.declarations.len() {
                    let decl = g.declarations[dci]
                    if decl.kind == DECL_FUNCTION() {
                        let fd = *decl.func_decl
                        if fd.name == mangled {
                            method_exists = true
                        }
                    }
                    dci = dci + 1
                }
            }

            if method_exists {
                -- Call the overloaded operator method: struct_method(left, right)
                let mut op_args: List[Expr] = []
                op_args.push(*expr.right)
                return generate_struct_method_call(g, b, left_struct_name, left_expr, overload_method, op_args)
            }
        }
    }

    -- --------------------------------------------------------
    -- EAGER BINARY OPERATORS (arithmetic, comparison, string)
    -- Evaluate both operands first, then emit the operation
    -- --------------------------------------------------------

    -- Evaluate left operand
    let lhs_result = generate_expr(g, b, *expr.left)
    g = lhs_result.gen
    b = lhs_result.block
    let lhs_id = lhs_result.val_id

    -- Evaluate right operand
    let rhs_result = generate_expr(g, b, *expr.right)
    g = rhs_result.gen
    b = rhs_result.block
    let rhs_id = rhs_result.val_id

    -- Infer the type of the left operand to select the correct op family
    let lhs_type = infer_expr_type(g, *expr.left)
    let rhs_type = infer_expr_type(g, *expr.right)

    -- Determine if either operand is float or string
    let is_string = lhs_type == "string" or rhs_type == "string"
    let is_float = is_float_type(lhs_type) or is_float_type(rhs_type)

    -- Determine the arithmetic result type
    let arith_type = if is_float { "f64" } else { "i64" }

    -- --------------------------------------------------------
    -- STRING OPERATIONS
    -- --------------------------------------------------------
    if is_string {
        -- String concatenation with +
        if op == BINOP_ADD() {
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_STRING_CONCAT())
            inst.lhs = ir_val_inst(lhs_id)
            inst.rhs = ir_val_inst(rhs_id)
            inst.result_type = "string"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }

        -- String equality ==
        if op == BINOP_EQ() {
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_STRING_EQ())
            inst.lhs = ir_val_inst(lhs_id)
            inst.rhs = ir_val_inst(rhs_id)
            inst.result_type = "bool"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }

        -- String inequality !=
        if op == BINOP_NE() {
            -- First compute string equality
            let eq_id = g.builder.next_id
            let mut eq_inst = ir_inst_new(eq_id, OP_STRING_EQ())
            eq_inst.lhs = ir_val_inst(lhs_id)
            eq_inst.rhs = ir_val_inst(rhs_id)
            eq_inst.result_type = "bool"
            eq_inst.has_result = true
            b.instructions.push(eq_inst)
            g.builder.next_id = eq_id + 1

            -- Then negate it
            let not_id = g.builder.next_id
            let mut not_inst = ir_inst_new(not_id, OP_LOGICAL_NOT())
            not_inst.operand = ir_val_inst(eq_id)
            not_inst.result_type = "bool"
            not_inst.has_result = true
            b.instructions.push(not_inst)
            g.builder.next_id = not_id + 1
            return expr_result(g, b, not_id)
        }

        -- String ordering comparisons (<, <=, >, >=) via dm_string_cmp
        if op == BINOP_LT() or op == BINOP_LE() or op == BINOP_GT() or op == BINOP_GE() {
            -- Call dm_string_cmp(lhs, rhs) -> i64
            let cmp_call_id = g.builder.next_id
            let mut cmp_call = ir_inst_new(cmp_call_id, OP_CALL())
            cmp_call.callee = "dm_string_cmp"
            let mut cmp_args: List[IRValue] = []
            cmp_args.push(ir_val_inst(lhs_id))
            cmp_args.push(ir_val_inst(rhs_id))
            cmp_call.call_args = cmp_args
            cmp_call.result_type = "i64"
            cmp_call.has_result = true
            b.instructions.push(cmp_call)
            g.builder.next_id = cmp_call_id + 1

            -- Compare the cmp result to 0
            let cmp_op = if op == BINOP_LT() { OP_LT() }
            else if op == BINOP_LE() { OP_LE() }
            else if op == BINOP_GT() { OP_GT() }
            else { OP_GE() }

            let result_id = g.builder.next_id
            let mut cmp_inst = ir_inst_new(result_id, cmp_op)
            cmp_inst.lhs = ir_val_inst(cmp_call_id)
            cmp_inst.rhs = ir_val_int(0)
            cmp_inst.result_type = "bool"
            cmp_inst.has_result = true
            b.instructions.push(cmp_inst)
            g.builder.next_id = result_id + 1
            return expr_result(g, b, result_id)
        }
    }

    -- --------------------------------------------------------
    -- ARITHMETIC OPERATORS (+, -, *, /, %)
    -- --------------------------------------------------------
    if op == BINOP_ADD() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_ADD())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_SUB() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_SUB())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_MUL() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_MUL())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_DIV() {
        -- Safe division: check for zero divisor before dividing
        -- Compare rhs == 0
        let zero_val = if is_float { ir_val_float(0.0) } else { ir_val_int(0) }
        let zero_check_id = g.builder.next_id
        let mut zero_check = ir_inst_new(zero_check_id, OP_EQ())
        zero_check.lhs = ir_val_inst(rhs_id)
        zero_check.rhs = zero_val
        zero_check.result_type = "bool"
        zero_check.has_result = true
        b.instructions.push(zero_check)
        g.builder.next_id = zero_check_id + 1

        let panic_label = gen_next_label(g, "div_panic")
        g = gen_advance_label(g)
        let ok_label = gen_next_label(g, "div_ok")
        g = gen_advance_label(g)

        -- Branch: if zero, panic; else continue
        b.terminator = Box_new(term_br_cond(ir_val_inst(zero_check_id), panic_label, ok_label))
        b.has_terminator = true
        g.completed_blocks.push(b)

        -- Panic block: call dm_panic, then unreachable
        let mut panic_block = ir_block_new(panic_label)
        let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        panic_inst.callee = "dm_panic"
        let mut panic_args: List[IRValue] = []
        panic_args.push(ir_val_string("Runtime error: division by zero"))
        panic_inst.call_args = panic_args
        panic_inst.result_type = "void"
        panic_inst.has_result = false
        panic_block.instructions.push(panic_inst)
        g.builder.next_id = g.builder.next_id + 1

        panic_block.terminator = Box_new(term_unreachable())
        panic_block.has_terminator = true
        g.completed_blocks.push(panic_block)

        -- OK block: perform the division
        let mut ok_block = ir_block_new(ok_label)
        let div_id = g.builder.next_id
        let mut div_inst = ir_inst_new(div_id, OP_DIV())
        div_inst.lhs = ir_val_inst(lhs_id)
        div_inst.rhs = ir_val_inst(rhs_id)
        div_inst.result_type = arith_type
        div_inst.has_result = true
        ok_block.instructions.push(div_inst)
        g.builder.next_id = div_id + 1

        return expr_result(g, ok_block, div_id)
    }

    if op == BINOP_MOD() {
        -- Safe modulo: check for zero divisor before computing
        let zero_val = if is_float { ir_val_float(0.0) } else { ir_val_int(0) }
        let zero_check_id = g.builder.next_id
        let mut zero_check = ir_inst_new(zero_check_id, OP_EQ())
        zero_check.lhs = ir_val_inst(rhs_id)
        zero_check.rhs = zero_val
        zero_check.result_type = "bool"
        zero_check.has_result = true
        b.instructions.push(zero_check)
        g.builder.next_id = zero_check_id + 1

        let panic_label = gen_next_label(g, "mod_panic")
        g = gen_advance_label(g)
        let ok_label = gen_next_label(g, "mod_ok")
        g = gen_advance_label(g)

        b.terminator = Box_new(term_br_cond(ir_val_inst(zero_check_id), panic_label, ok_label))
        b.has_terminator = true
        g.completed_blocks.push(b)

        -- Panic block
        let mut panic_block = ir_block_new(panic_label)
        let mut panic_inst = ir_inst_new(g.builder.next_id, OP_CALL())
        panic_inst.callee = "dm_panic"
        let mut panic_args: List[IRValue] = []
        panic_args.push(ir_val_string("Runtime error: modulo by zero"))
        panic_inst.call_args = panic_args
        panic_inst.result_type = "void"
        panic_inst.has_result = false
        panic_block.instructions.push(panic_inst)
        g.builder.next_id = g.builder.next_id + 1

        panic_block.terminator = Box_new(term_unreachable())
        panic_block.has_terminator = true
        g.completed_blocks.push(panic_block)

        -- OK block
        let mut ok_block = ir_block_new(ok_label)
        let mod_id = g.builder.next_id
        let mut mod_inst = ir_inst_new(mod_id, OP_MOD())
        mod_inst.lhs = ir_val_inst(lhs_id)
        mod_inst.rhs = ir_val_inst(rhs_id)
        mod_inst.result_type = arith_type
        mod_inst.has_result = true
        ok_block.instructions.push(mod_inst)
        g.builder.next_id = mod_id + 1

        return expr_result(g, ok_block, mod_id)
    }

    -- --------------------------------------------------------
    -- COMPARISON OPERATORS (==, !=, <, <=, >, >=)
    -- --------------------------------------------------------

    -- Check if this is an enum comparison (need to extract tags first)
    let mut is_enum_cmp = false
    let left_expr = *expr.left
    if left_expr.kind == EXPR_IDENTIFIER() {
        if g.var_enum_types.contains(left_expr.name) {
            is_enum_cmp = true
        }
    }
    if left_expr.kind == EXPR_ENUM_LITERAL() {
        is_enum_cmp = true
    }
    -- Also check right side for enum literal like Color.Red
    let right_expr = *expr.right
    if right_expr.kind == EXPR_ENUM_LITERAL() {
        is_enum_cmp = true
    }
    if right_expr.kind == EXPR_IDENTIFIER() {
        if g.var_enum_types.contains(right_expr.name) {
            is_enum_cmp = true
        }
    }

    if is_enum_cmp and (op == BINOP_EQ() or op == BINOP_NE()) {
        -- Extract tag (field 0) from both enum values
        let lhs_tag_id = g.builder.next_id
        let mut lhs_tag = ir_inst_new(lhs_tag_id, OP_EXTRACT_FIELD())
        lhs_tag.field_base = ir_val_inst(lhs_id)
        lhs_tag.field_index = 0
        lhs_tag.field_type = "i32"
        lhs_tag.result_type = "i32"
        lhs_tag.has_result = true
        b.instructions.push(lhs_tag)
        g.builder.next_id = g.builder.next_id + 1

        let rhs_tag_id = g.builder.next_id
        let mut rhs_tag = ir_inst_new(rhs_tag_id, OP_EXTRACT_FIELD())
        rhs_tag.field_base = ir_val_inst(rhs_id)
        rhs_tag.field_index = 0
        rhs_tag.field_type = "i32"
        rhs_tag.result_type = "i32"
        rhs_tag.has_result = true
        b.instructions.push(rhs_tag)
        g.builder.next_id = g.builder.next_id + 1

        let cmp_op = if op == BINOP_EQ() { OP_EQ() } else { OP_NE() }
        let cmp_id = g.builder.next_id
        let mut cmp_inst = ir_inst_new(cmp_id, cmp_op)
        cmp_inst.lhs = ir_val_inst(lhs_tag_id)
        cmp_inst.rhs = ir_val_inst(rhs_tag_id)
        cmp_inst.result_type = "bool"
        cmp_inst.has_result = true
        b.instructions.push(cmp_inst)
        g.builder.next_id = cmp_id + 1
        return expr_result(g, b, cmp_id)
    }

    if op == BINOP_EQ() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_EQ())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_NE() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_NE())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_LT() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LT())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_LE() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LE())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_GT() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_GT())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_GE() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_GE())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- --------------------------------------------------------
    -- BITWISE OPERATORS (&, |, ^, <<, >>)
    -- --------------------------------------------------------
    if op == BINOP_BIT_AND() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_AND())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_BIT_OR() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_OR())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_BIT_XOR() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_XOR())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_SHL() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_SHL())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if op == BINOP_SHR() {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_SHR())
        inst.lhs = ir_val_inst(lhs_id)
        inst.rhs = ir_val_inst(rhs_id)
        inst.result_type = arith_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Fallback: unsupported binary operator, return lhs
    return expr_result(g, b, lhs_id)
}

-- ============================================================
-- UNARY EXPRESSION IR GENERATION
-- Handles negation (-) and logical not (!)
-- Port of stage3/src/ir_gen.zig generateUnary
-- ============================================================

fn generate_unary(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Evaluate the operand expression
    let operand_result = generate_expr(g, b, *expr.operand)
    g = operand_result.gen
    b = operand_result.block
    let operand_id = operand_result.val_id

    if expr.op == UNOP_NEG() {
        -- Negate: emit OP_NEG with operand's type
        let operand_type = infer_expr_type(g, *expr.operand)
        let result_type = if is_float_type(operand_type) { "f64" } else { "i64" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_NEG())
        inst.operand = ir_val_inst(operand_id)
        inst.result_type = result_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if expr.op == UNOP_NOT() {
        -- Logical NOT: emit OP_LOGICAL_NOT, result is always bool
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LOGICAL_NOT())
        inst.operand = ir_val_inst(operand_id)
        inst.result_type = "bool"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    if expr.op == UNOP_BIT_NOT() {
        -- Bitwise NOT: emit OP_BIT_XOR with all-ones mask (XOR with -1)
        -- ~x is equivalent to x ^ (-1) for two's complement integers
        let operand_type = infer_expr_type(g, *expr.operand)
        let result_type = if is_float_type(operand_type) { "f64" } else { "i64" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_BIT_XOR())
        inst.lhs = ir_val_inst(operand_id)
        inst.rhs = ir_val_int(-1)
        inst.result_type = result_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Unknown unary operator, return operand unchanged
    return expr_result(g, b, operand_id)
}

-- ============================================================
-- CAST EXPRESSION IR GENERATION
-- Handles explicit type casts via `expr as Type`
-- Port of stage3/src/ir_gen.zig generateCast
-- ============================================================

fn generate_cast_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Evaluate the operand expression
    let operand_result = generate_expr(g, b, *expr.operand)
    g = operand_result.gen
    b = operand_result.block
    let operand_id = operand_result.val_id

    -- Determine source type by inference
    let from_type = infer_expr_type(g, *expr.operand)

    -- Determine target type from the cast annotation, mapped to IR names
    let to_type = map_type_name(expr.cast_type)

    -- If the types are the same, no cast needed
    if from_type == to_type {
        return expr_result(g, b, operand_id)
    }

    -- Emit OP_CAST instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = ir_val_inst(operand_id)
    inst.cast_from = from_type
    inst.cast_to = to_type
    inst.result_type = to_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- FUNCTION CALL IR GENERATION
-- Dispatches function calls to builtins, special constructors,
-- generic monomorphization, indirect (lambda) calls, and
-- regular user-defined function calls.
-- Port of stage3/src/ir_gen.zig generateCall
-- ============================================================

fn generate_function_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get the callee name from the callee expression (should be an identifier)
    let callee_expr = *expr.callee
    let fn_name = callee_expr.name

    -- --------------------------------------------------------
    -- Special case dispatch (in priority order)
    -- --------------------------------------------------------

    -- SIMD builtins (simd_splat_*, simd_set_*, simd_add, etc.)
    if is_simd_builtin(fn_name) {
        return generate_simd_builtin(g, b, fn_name, expr.args)
    }

    -- assert(cond)
    if fn_name == "assert" {
        return generate_assert_call(g, b, expr.args)
    }

    -- assert_eq(a, b)
    if fn_name == "assert_eq" {
        return generate_assert_eq_call(g, b, expr.args)
    }

    -- panic(msg)
    if fn_name == "panic" {
        return generate_panic_call(g, b, expr.args)
    }

    -- Box_new(value)
    if fn_name == "Box_new" {
        return generate_box_new_call(g, b, expr.args)
    }

    -- Box_null()
    if fn_name == "Box_null" {
        return generate_box_null_call(g, b)
    }

    -- string_split(s, delim) -> List[string]
    if fn_name == "string_split" {
        return generate_string_split_call(g, b, expr.args)
    }

    -- Some(value) -> Option with has_value=1
    if fn_name == "Some" {
        return generate_some_call(g, b, expr.args)
    }

    -- Ok(value) -> Result with tag=0
    if fn_name == "Ok" {
        return generate_ok_call(g, b, expr.args)
    }

    -- Err(value) -> Result with tag=1
    if fn_name == "Err" {
        return generate_err_call(g, b, expr.args)
    }

    -- Map_new() is handled at the let-binding level; return a placeholder
    if fn_name == "Map_new" {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_ALLOCA())
        inst.alloc_type = "i64"
        inst.result_type = "ptr(i64)"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Explicit generic type arguments: fn_name[T](args)
    if expr.generic_args.len() > 0 {
        return generate_generic_call(g, b, expr)
    }

    -- Implicit generic: check if callee is registered as a generic function
    if g.generic_fn_decls.contains(fn_name) {
        return generate_generic_call(g, b, expr)
    }

    -- Indirect call via function pointer (lambda variable)
    if g.fn_ptr_vars.contains(fn_name) {
        return generate_lambda_indirect_call(g, b, fn_name, expr.args)
    }

    -- Indirect call via function-pointer-typed parameter: f: fn(int) -> int
    if g.variable_types.contains(fn_name) {
        let vtype = g.variable_types.get(fn_name)
        if starts_with(vtype, "fn(") {
            return generate_fn_ptr_param_call(g, b, fn_name, vtype, expr.args)
        }
    }

    -- len() on a list variable: dispatch to typed list_len runtime function
    if fn_name == "len" and expr.args.len() > 0 {
        let first_arg = expr.args[0]
        if first_arg.kind == EXPR_IDENTIFIER() {
            let arg_name = first_arg.name
            if g.list_elem_kinds.contains(arg_name) {
                let ek = g.list_elem_kinds.get(arg_name)
                let list_ptr = g.variable_map.get(arg_name)

                let len_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_len" }
                else if ek == LIST_ELEM_FLOAT() { "dm_list_double_len" }
                else if ek == LIST_ELEM_STRING() { "dm_list_string_len" }
                else { "dm_list_generic_len" }

                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = len_fn
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(list_ptr))
                inst.call_args = call_args
                inst.result_type = "i64"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, id)
            }
        }
    }

    -- Builtin function (println, len, int_to_string, etc.)
    if is_builtin_fn(fn_name) {
        return generate_builtin_call(g, b, fn_name, expr.args)
    }

    -- Check if function is defined
    if g.known_functions.contains(fn_name) == false and is_builtin_fn(fn_name) == false and g.generic_fn_decls.contains(fn_name) == false and g.user_extern_fns.contains(fn_name) == false and g.method_struct_map.contains(fn_name) == false {
        eprintln("Error: undefined function '" + fn_name + "'")
        exit(1)
    }

    -- --------------------------------------------------------
    -- Default: regular user-defined function call or extern call
    -- --------------------------------------------------------
    return generate_normal_call(g, b, fn_name, expr.args)
}

-- ============================================================
-- NORMAL (USER-DEFINED / EXTERN) FUNCTION CALL
-- Evaluates all argument expressions, then emits OP_CALL.
-- Handles mut param pass-by-reference and extern functions.
-- ============================================================

fn generate_normal_call(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Check if this function has mut params (pass-by-reference)
    let has_mut_params = g.mut_param_fns.contains(fn_name)
    let mut_flags_str = if has_mut_params {
        g.mut_param_fns.get(fn_name)
    } else {
        ""
    }

    -- Split mut flags into a list for per-argument checking
    let mut mut_flags_list: List[string] = []
    if has_mut_params {
        mut_flags_list = string_split(mut_flags_str, "|")
    }

    -- Generate all argument expressions
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        -- Check if this arg position is a mut parameter
        let is_mut_arg = if i < mut_flags_list.len() {
            mut_flags_list[i] == "1"
        } else {
            false
        }

        if is_mut_arg and args[i].kind == EXPR_IDENTIFIER() {
            -- Pass the alloca pointer directly (pass-by-reference) for mut params
            let arg_name = args[i].name
            if g.variable_map.contains(arg_name) {
                -- If the arg is itself a mut param, forward the param pointer
                if g.param_ptr_map.contains(arg_name) {
                    let pidx = g.param_ptr_map.get(arg_name)
                    call_args.push(ir_val_param(pidx))
                } else {
                    let ptr_id = g.variable_map.get(arg_name)
                    call_args.push(ir_val_inst(ptr_id))
                }
            } else {
                -- Fallback: evaluate normally
                let arg_result = generate_expr(g, b, args[i])
                g = arg_result.gen
                b = arg_result.block
                call_args.push(ir_val_inst(arg_result.val_id))
            }
        } else {
            let arg_result = generate_expr(g, b, args[i])
            g = arg_result.gen
            b = arg_result.block
            call_args.push(ir_val_inst(arg_result.val_id))
        }
        i = i + 1
    }

    -- Determine the callee name for the call instruction
    -- For extern functions, use the function name directly
    -- For user functions, also use the function name directly
    let callee_name = fn_name

    -- Determine return type by looking up registered functions in the module
    let ret_type = infer_call_return_type(g, callee_name)
    let has_result = ret_type != "void"

    -- Emit the OP_CALL instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = callee_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- INFER CALL RETURN TYPE
-- Looks up the return type of a function by name in the module.
-- Falls back to "i64" for unknown functions.
-- ============================================================

fn infer_call_return_type(gen: IRGenerator, fn_name: string) -> string {
    -- Check builtins first
    if is_builtin_fn(fn_name) {
        return builtin_return_type(fn_name)
    }

    -- Search module functions for the declared return type
    let mut i = 0
    while i < gen.ir_mod.functions.len() {
        let f = gen.ir_mod.functions[i]
        if f.name == fn_name {
            return f.return_type
        }
        i = i + 1
    }

    -- Unknown function: assume i64 return
    return "i64"
}

-- ============================================================
-- LAMBDA / FUNCTION POINTER INDIRECT CALL
-- Calls a lambda stored in fn_ptr_vars.
-- Passes captured variables as additional arguments.
-- ============================================================

fn generate_lambda_indirect_call(gen: IRGenerator, block: IRBasicBlock, var_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get the actual lambda function name
    let lambda_name = g.fn_ptr_vars.get(var_name)

    -- Generate explicit arguments
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    -- Add captured variable values as extra arguments
    if g.lambda_captures.contains(lambda_name) {
        let captures_str = g.lambda_captures.get(lambda_name)
        let capture_names = string_split(captures_str, "|")
        let mut ci = 0
        while ci < capture_names.len() {
            let cap_name = capture_names[ci]
            if len(cap_name) > 0 and g.variable_map.contains(cap_name) {
                let cap_alloca = g.variable_map.get(cap_name)
                let cap_type = if g.variable_types.contains(cap_name) {
                    g.variable_types.get(cap_name)
                } else {
                    "i64"
                }

                -- Load the captured variable's current value
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(cap_alloca)
                load_inst.load_type = cap_type
                load_inst.result_type = cap_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1

                call_args.push(ir_val_inst(load_id))
            } else {
                -- Captured variable not found: pass zero
                call_args.push(ir_val_int(0))
            }
            ci = ci + 1
        }
    }

    -- Look up the return type of the lambda function
    let ret_type = infer_call_return_type(g, lambda_name)
    let has_result = ret_type != "void"

    -- Emit a direct call to the lambda function
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = lambda_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- FUNCTION POINTER PARAMETER INDIRECT CALL
-- Handles calling through function-pointer-typed parameters:
--   fn apply(f: fn(int) -> int, x: int) -> int { return f(x) }
-- ============================================================

fn generate_fn_ptr_param_call(gen: IRGenerator, block: IRBasicBlock, var_name: string, var_type: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Load the function pointer value from the parameter's alloca
    let alloca_id = g.variable_map.get(var_name)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(alloca_id)
    load_inst.load_type = "ptr"
    load_inst.result_type = "ptr"
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Generate arguments
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    -- Parse return type from "fn(int, int) -> int"
    let arrow_pos = string_find(var_type, " -> ")
    let ret_type_str = if arrow_pos >= 0 {
        substr(var_type, arrow_pos + 4, len(var_type) - arrow_pos - 4)
    } else {
        "void"
    }
    let ret_type = map_type_name(ret_type_str)
    let has_result = ret_type != "void"

    -- Emit OP_CALL_PTR instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL_PTR())
    inst.callee_val = ir_val_inst(load_id)
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SOME CALL: Option constructor
-- Some(value) -> Option { has_value: 1, value: payload }
-- ============================================================

fn generate_some_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Use full Option[T] type from current function return type, or infer from arg type
    let mut opt_type = g.current_return_type
    if starts_with(opt_type, "Option[") == false {
        -- current_return_type isn't an Option (e.g., we're in main/void function).
        -- Infer Option type from the argument expression type.
        let arg_type = infer_expr_type(g, args[0])
        opt_type = "Option[" + arg_type + "]"
    }

    -- Build the Option struct: { has_value: 1, value: payload }
    -- Use undef as the base (not alloca), since insertvalue works on values not pointers

    -- Store has_value = 1 (field index 0)
    let insert1_id = g.builder.next_id
    let mut insert1 = ir_inst_new(insert1_id, OP_INSERT_FIELD())
    insert1.field_base = ir_val_undef()
    insert1.field_value = ir_val_int(1)
    insert1.field_index = 0
    insert1.result_type = opt_type
    insert1.has_result = true
    b.instructions.push(insert1)
    g.builder.next_id = g.builder.next_id + 1

    -- Store value = payload (field index 1)
    let insert2_id = g.builder.next_id
    let mut insert2 = ir_inst_new(insert2_id, OP_INSERT_FIELD())
    insert2.field_base = ir_val_inst(insert1_id)
    insert2.field_value = ir_val_inst(val_result.val_id)
    insert2.field_index = 1
    insert2.result_type = opt_type
    insert2.has_result = true
    b.instructions.push(insert2)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert2_id)
}

-- ============================================================
-- OK CALL: Result constructor (success)
-- Ok(value) -> Result { tag: 0, ok_val: payload, err_val: undef }
-- ============================================================

fn generate_ok_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Use full Result[T, E] type from current function return type for correct LLVM struct layout
    let res_type = g.current_return_type

    -- Build the Result struct: { tag: 0, ok_val: payload, err_val: undef }
    -- Use undef as the base (not alloca), since insertvalue works on values not pointers

    -- tag = 0 (Ok) at field index 0
    let insert_tag_id = g.builder.next_id
    let mut insert_tag = ir_inst_new(insert_tag_id, OP_INSERT_FIELD())
    insert_tag.field_base = ir_val_undef()
    insert_tag.field_value = ir_val_int(0)
    insert_tag.field_index = 0
    insert_tag.result_type = res_type
    insert_tag.has_result = true
    b.instructions.push(insert_tag)
    g.builder.next_id = g.builder.next_id + 1

    -- ok_val = payload at field index 1
    let insert_ok_id = g.builder.next_id
    let mut insert_ok = ir_inst_new(insert_ok_id, OP_INSERT_FIELD())
    insert_ok.field_base = ir_val_inst(insert_tag_id)
    insert_ok.field_value = ir_val_inst(val_result.val_id)
    insert_ok.field_index = 1
    insert_ok.result_type = res_type
    insert_ok.has_result = true
    b.instructions.push(insert_ok)
    g.builder.next_id = g.builder.next_id + 1

    -- err_val = undef at field index 2
    let insert_err_id = g.builder.next_id
    let mut insert_err = ir_inst_new(insert_err_id, OP_INSERT_FIELD())
    insert_err.field_base = ir_val_inst(insert_ok_id)
    insert_err.field_value = ir_val_undef()
    insert_err.field_index = 2
    insert_err.result_type = res_type
    insert_err.has_result = true
    b.instructions.push(insert_err)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert_err_id)
}

-- ============================================================
-- ERR CALL: Result constructor (error)
-- Err(value) -> Result { tag: 1, ok_val: undef, err_val: payload }
-- ============================================================

fn generate_err_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the error payload value
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Use full Result[T, E] type from current function return type for correct LLVM struct layout
    let res_type = g.current_return_type

    -- Build the Result struct: { tag: 1, ok_val: undef, err_val: payload }
    -- Use undef as the base (not alloca), since insertvalue works on values not pointers

    -- tag = 1 (Err) at field index 0
    let insert_tag_id = g.builder.next_id
    let mut insert_tag = ir_inst_new(insert_tag_id, OP_INSERT_FIELD())
    insert_tag.field_base = ir_val_undef()
    insert_tag.field_value = ir_val_int(1)
    insert_tag.field_index = 0
    insert_tag.result_type = res_type
    insert_tag.has_result = true
    b.instructions.push(insert_tag)
    g.builder.next_id = g.builder.next_id + 1

    -- ok_val = undef at field index 1
    let insert_ok_id = g.builder.next_id
    let mut insert_ok = ir_inst_new(insert_ok_id, OP_INSERT_FIELD())
    insert_ok.field_base = ir_val_inst(insert_tag_id)
    insert_ok.field_value = ir_val_undef()
    insert_ok.field_index = 1
    insert_ok.result_type = res_type
    insert_ok.has_result = true
    b.instructions.push(insert_ok)
    g.builder.next_id = g.builder.next_id + 1

    -- err_val = payload at field index 2
    let insert_err_id = g.builder.next_id
    let mut insert_err = ir_inst_new(insert_err_id, OP_INSERT_FIELD())
    insert_err.field_base = ir_val_inst(insert_ok_id)
    insert_err.field_value = ir_val_inst(val_result.val_id)
    insert_err.field_index = 2
    insert_err.result_type = res_type
    insert_err.has_result = true
    b.instructions.push(insert_err)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, insert_err_id)
}

-- ============================================================
-- GENERIC FUNCTION CALL
-- Builds a monomorphized name, generates the specialized copy
-- if not already done, then calls it normally.
-- Port of stage3/src/ir_gen.zig generateGenericCall
-- ============================================================

fn generate_generic_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let callee_expr = *expr.callee
    let fn_name = callee_expr.name

    -- Build monomorphized name: fn_name + "_" + type_args joined by "_"
    let mut mangled_name = fn_name
    let mut type_args = expr.generic_args

    if type_args.len() == 0 {
        -- Implicit generic: infer type args from call arguments
        -- For now, use "i64" as default for each generic param
        -- (A more complete implementation would inspect arg expressions)
        if g.generic_fn_decls.contains(fn_name) {
            let decl_idx = g.generic_fn_decls.get(fn_name)
            if decl_idx >= 0 and decl_idx < g.declarations.len() {
                let decl = g.declarations[decl_idx]
                if decl.kind == DECL_FUNCTION() {
                    let fd = *decl.func_decl
                    let mut inferred: List[string] = []
                    let mut pi = 0
                    while pi < fd.generic_params.len() {
                        -- Try to infer from corresponding argument type
                        let inferred_type = if pi < expr.args.len() {
                            infer_arg_type(g, expr.args[pi])
                        } else {
                            "i64"
                        }
                        inferred.push(inferred_type)
                        pi = pi + 1
                    }
                    type_args = inferred
                }
            }
        }
    }

    -- Build the mangled name from type arguments
    let mut ti = 0
    while ti < type_args.len() {
        mangled_name = mangled_name + "_" + map_type_name(type_args[ti])
        ti = ti + 1
    }

    -- Generate the monomorphized function if not already done
    if g.mono_generated.contains(mangled_name) == false {
        g.mono_generated.insert(mangled_name, true)

        if g.generic_fn_decls.contains(fn_name) {
            let decl_idx = g.generic_fn_decls.get(fn_name)
            if decl_idx >= 0 and decl_idx < g.declarations.len() {
                let decl = g.declarations[decl_idx]
                if decl.kind == DECL_FUNCTION() {
                    let fd = *decl.func_decl

                    -- Save current generator state
                    let saved_variable_map = g.variable_map
                    let saved_variable_types = g.variable_types
                    let saved_list_elem_kinds = g.list_elem_kinds
                    let saved_list_elem_types = g.list_elem_types
                    let saved_return_type = g.current_return_type
                    let saved_type_subs = g.type_substitutions
                    let saved_completed_blocks = g.completed_blocks
                    let saved_next_id = g.builder.next_id

                    -- Set up fresh maps for the monomorphized function
                    g.variable_map = Map_new()
                    g.variable_types = Map_new()

                    -- Set up type substitutions: map generic param names to concrete types
                    g.type_substitutions = Map_new()
                    let mut si = 0
                    while si < fd.generic_params.len() and si < type_args.len() {
                        g.type_substitutions.insert(fd.generic_params[si], type_args[si])
                        si = si + 1
                    }

                    -- Declare and generate the monomorphized function body
                    -- Create a modified FunctionDecl with the mangled name and no generic params
                    let mut mono_fd = fd
                    let empty_gp: List[string] = []
                    mono_fd.generic_params = empty_gp

                    -- Replace type parameter references in param types and return type
                    let mut mono_params: List[FuncParam] = []
                    let mut pi = 0
                    while pi < fd.params.len() {
                        let p = fd.params[pi]
                        let resolved_type = resolve_type_substitution(g, p.type_name)
                        let mut mp = func_param_new(p.name, resolved_type)
                        mp.is_mut = p.is_mut
                        mono_params.push(mp)
                        pi = pi + 1
                    }
                    mono_fd.params = mono_params

                    let resolved_ret = resolve_type_substitution(g, fd.return_type)
                    mono_fd.return_type = resolved_ret

                    g = declare_function(g, mono_fd, mangled_name)
                    g = generate_function_body(g, mono_fd, mangled_name)

                    -- Restore previous generator state
                    g.variable_map = saved_variable_map
                    g.variable_types = saved_variable_types
                    g.list_elem_kinds = saved_list_elem_kinds
                    g.list_elem_types = saved_list_elem_types
                    g.current_return_type = saved_return_type
                    g.type_substitutions = saved_type_subs
                    g.completed_blocks = saved_completed_blocks
                    g.builder.next_id = saved_next_id
                }
            }
        }
    }

    -- Generate the call to the monomorphized function
    let mut call_args: List[IRValue] = []
    let mut ai = 0
    while ai < expr.args.len() {
        let arg_result = generate_expr(g, b, expr.args[ai])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        ai = ai + 1
    }

    let ret_type = infer_call_return_type(g, mangled_name)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = mangled_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- RESOLVE TYPE SUBSTITUTION
-- Replaces type parameter names with concrete types from
-- the current type_substitutions map.
-- ============================================================

-- Helper: join type names with "_" for mangled names
fn join_types(types: List[string]) -> string {
    let mut result = ""
    let mut i = 0
    while i < types.len() {
        if i > 0 {
            result = result + "_"
        }
        result = result + map_type_name(types[i])
        i = i + 1
    }
    return result
}

fn resolve_type_substitution(gen: IRGenerator, type_name: string) -> string {
    -- Direct substitution
    if gen.type_substitutions.contains(type_name) {
        return gen.type_substitutions.get(type_name)
    }

    -- Handle generic container types: List[T], Option[T], Box[T], etc.
    if starts_with(type_name, "List[") {
        let inner = extract_inner_type(type_name, "List[")
        let resolved_inner = resolve_type_substitution(gen, inner)
        return "List[" + resolved_inner + "]"
    }
    if starts_with(type_name, "Option[") {
        let inner = extract_inner_type(type_name, "Option[")
        let resolved_inner = resolve_type_substitution(gen, inner)
        return "Option[" + resolved_inner + "]"
    }
    if starts_with(type_name, "Box[") {
        let inner = extract_inner_type(type_name, "Box[")
        let resolved_inner = resolve_type_substitution(gen, inner)
        return "Box[" + resolved_inner + "]"
    }
    if starts_with(type_name, "Result[") {
        let inner = extract_inner_type(type_name, "Result[")
        -- Result inner is "T, E" — split on ", " and resolve each
        let comma_idx = string_find(inner, ", ")
        if comma_idx >= 0 {
            let ok_type = substr(inner, 0, comma_idx)
            let err_type = substr(inner, comma_idx + 2, len(inner) - comma_idx - 2)
            let resolved_ok = resolve_type_substitution(gen, ok_type)
            let resolved_err = resolve_type_substitution(gen, err_type)
            return "Result[" + resolved_ok + ", " + resolved_err + "]"
        }
    }

    -- Handle arbitrary user-defined generic types: Pair[A, B] etc.
    -- Look for Name[...] pattern where Name is not a known builtin container
    let bracket_idx = string_find(type_name, "[")
    if bracket_idx > 0 {
        let base = substr(type_name, 0, bracket_idx)
        let inner = substr(type_name, bracket_idx + 1, len(type_name) - bracket_idx - 2)
        -- Resolve each comma-separated type arg
        let comma_idx = string_find(inner, ", ")
        if comma_idx >= 0 {
            let first_type = substr(inner, 0, comma_idx)
            let second_type = substr(inner, comma_idx + 2, len(inner) - comma_idx - 2)
            let resolved_first = resolve_type_substitution(gen, first_type)
            let resolved_second = resolve_type_substitution(gen, second_type)
            return base + "_" + map_type_name(resolved_first) + "_" + map_type_name(resolved_second)
        } else {
            let resolved_inner = resolve_type_substitution(gen, inner)
            return base + "_" + map_type_name(resolved_inner)
        }
    }

    return type_name
}

-- ============================================================
-- INFER ARGUMENT TYPE
-- Attempts to infer the IR type of an expression for implicit
-- generic type argument deduction.
-- ============================================================

fn infer_arg_type(gen: IRGenerator, arg: Expr) -> string {
    if arg.kind == EXPR_LITERAL_INT() {
        return "int"
    }
    if arg.kind == EXPR_LITERAL_FLOAT() {
        return "float"
    }
    if arg.kind == EXPR_LITERAL_STRING() {
        return "string"
    }
    if arg.kind == EXPR_LITERAL_BOOL() {
        return "bool"
    }
    if arg.kind == EXPR_IDENTIFIER() {
        if gen.variable_types.contains(arg.name) {
            let ty = gen.variable_types.get(arg.name)
            -- Map IR type names back to dAImond names
            if ty == "i64" { return "int" }
            if ty == "f64" { return "float" }
            return ty
        }
    }
    return "int"
}

-- ============================================================
-- METHOD CALL IR GENERATION
-- Dispatches method calls on objects to the appropriate handler:
-- enum constructors, dynamic dispatch, map methods, list methods,
-- string methods, struct methods.
-- Port of stage3/src/ir_gen.zig generateMethodCall
-- ============================================================

fn generate_method_call(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let obj_expr = *expr.object
    let method_name = expr.method
    let args = expr.args

    -- --------------------------------------------------------
    -- Check if this is an enum variant constructor: EnumName.Variant(args)
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_IDENTIFIER() {
        let obj_name = obj_expr.name
        if g.enum_defs.contains(obj_name) {
            return generate_enum_method_construct(g, b, obj_name, method_name, args)
        }
    }

    -- --------------------------------------------------------
    -- Check if this is a dyn Trait method call
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_IDENTIFIER() {
        let obj_name = obj_expr.name
        if g.dyn_var_concrete.contains(obj_name) {
            return generate_dyn_dispatch_call(g, b, obj_name, method_name, args)
        }
    }

    -- --------------------------------------------------------
    -- Get object variable name for type dispatch
    -- --------------------------------------------------------
    let obj_var_name = if obj_expr.kind == EXPR_IDENTIFIER() {
        obj_expr.name
    } else {
        ""
    }

    -- --------------------------------------------------------
    -- Map method calls
    -- --------------------------------------------------------
    if len(obj_var_name) > 0 and g.map_var_kinds.contains(obj_var_name) {
        let mk = g.map_var_kinds.get(obj_var_name)
        let map_ptr = g.variable_map.get(obj_var_name)
        return generate_map_method_call(g, b, mk, map_ptr, method_name, args)
    }

    -- --------------------------------------------------------
    -- List method calls (on direct variable)
    -- --------------------------------------------------------
    if len(obj_var_name) > 0 and g.list_elem_kinds.contains(obj_var_name) {
        let ek = g.list_elem_kinds.get(obj_var_name)
        let raw_list_ptr = g.variable_map.get(obj_var_name)
        -- For mut params (negative sentinel), generate expr to get the actual pointer
        if raw_list_ptr < 0 and g.param_ptr_map.contains(obj_var_name) {
            let obj_result = generate_expr(g, b, obj_expr)
            g = obj_result.gen
            b = obj_result.block
            return generate_list_method_call(g, b, ek, obj_result.val_id, obj_var_name, method_name, args)
        }
        return generate_list_method_call(g, b, ek, raw_list_ptr, obj_var_name, method_name, args)
    }

    -- --------------------------------------------------------
    -- List method calls on struct field access (e.g., p.tokens.push(x))
    -- --------------------------------------------------------
    if obj_expr.kind == EXPR_FIELD_ACCESS() {
        let field_obj = *obj_expr.object
        if field_obj.kind == EXPR_IDENTIFIER() {
            let field_obj_name = field_obj.name
            let field_name = obj_expr.field
            let field_key = if g.var_struct_types.contains(field_obj_name) {
                g.var_struct_types.get(field_obj_name) + "." + field_name
            } else {
                ""
            }
            if len(field_key) > 0 and g.field_list_elem_kinds.contains(field_key) {
                let ek = g.field_list_elem_kinds.get(field_key)
                -- Generate the field access to get the list pointer
                let obj_result = generate_expr(g, b, obj_expr)
                g = obj_result.gen
                b = obj_result.block
                let list_ptr = obj_result.val_id
                return generate_list_method_call(g, b, ek, list_ptr, "", method_name, args)
            }
        }
    }

    -- --------------------------------------------------------
    -- String .len() method
    -- --------------------------------------------------------
    if method_name == "len" and args.len() == 0 {
        if len(obj_var_name) > 0 and g.variable_types.contains(obj_var_name) {
            let vtype = g.variable_types.get(obj_var_name)
            if vtype == "string" {
                let obj_result = generate_expr(g, b, obj_expr)
                g = obj_result.gen
                b = obj_result.block

                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = "dm_string_len"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(obj_result.val_id))
                inst.call_args = call_args
                inst.result_type = "i64"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, id)
            }
        }
    }

    -- --------------------------------------------------------
    -- Struct method calls (mangled name dispatch)
    -- --------------------------------------------------------
    let struct_type = if len(obj_var_name) > 0 and g.var_struct_types.contains(obj_var_name) {
        g.var_struct_types.get(obj_var_name)
    } else if g.method_struct_map.contains(method_name) {
        g.method_struct_map.get(method_name)
    } else {
        ""
    }

    if len(struct_type) > 0 {
        return generate_struct_method_call(g, b, struct_type, obj_expr, method_name, args)
    }

    -- --------------------------------------------------------
    -- Fallback: treat as a regular function call with self as first arg
    -- --------------------------------------------------------
    let obj_result = generate_expr(g, b, obj_expr)
    g = obj_result.gen
    b = obj_result.block

    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(obj_result.val_id))

    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    let ret_type = infer_call_return_type(g, method_name)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = method_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- ENUM VARIANT CONSTRUCTOR VIA METHOD SYNTAX
-- Handles EnumName.Variant(payload...) as a method call.
-- ============================================================

fn generate_enum_method_construct(gen: IRGenerator, block: IRBasicBlock, enum_name: string, variant_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let info = g.enum_defs.get(enum_name)

    -- Find the variant
    let mut tag = -1
    let mut has_payload = false
    let mut vi = 0
    while vi < info.variant_names.len() {
        if info.variant_names[vi] == variant_name {
            tag = info.variant_tags[vi]
            has_payload = info.variant_has_payload[vi]
        }
        vi = vi + 1
    }

    if tag < 0 {
        -- Variant not found; return undef
        return expr_result(g, b, 0)
    }

    -- Build the enum struct with tag and optional payload
    let base_id = g.builder.next_id
    let mut base_inst = ir_inst_new(base_id, OP_ALLOCA())
    base_inst.alloc_type = enum_name
    base_inst.result_type = "ptr(" + enum_name + ")"
    base_inst.has_result = true
    b.instructions.push(base_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Insert tag at field index 0
    let tag_insert_id = g.builder.next_id
    let mut tag_insert = ir_inst_new(tag_insert_id, OP_INSERT_FIELD())
    tag_insert.field_base = ir_val_inst(base_id)
    tag_insert.field_value = ir_val_int(tag)
    tag_insert.field_index = 0
    tag_insert.result_type = enum_name
    tag_insert.has_result = true
    b.instructions.push(tag_insert)
    g.builder.next_id = g.builder.next_id + 1

    let mut last_id = tag_insert_id

    -- If has payload, insert payload fields
    if has_payload and args.len() > 0 {
        -- Determine the field index for this variant's payload
        -- The payload field is named "VariantName_0" and its index is
        -- computed by counting prior payload fields + 1 (for the tag)
        let mut field_idx = 1
        let mut pi = 0
        while pi < info.variant_names.len() {
            if info.variant_names[pi] == variant_name {
                -- Found our variant; field_idx is now correct
                pi = info.variant_names.len()  -- break
            } else {
                if info.variant_has_payload[pi] {
                    field_idx = field_idx + 1
                }
                pi = pi + 1
            }
        }

        -- Insert each payload argument
        let mut ai = 0
        while ai < args.len() {
            let arg_result = generate_expr(g, b, args[ai])
            g = arg_result.gen
            b = arg_result.block

            let payload_insert_id = g.builder.next_id
            let mut payload_insert = ir_inst_new(payload_insert_id, OP_INSERT_FIELD())
            payload_insert.field_base = ir_val_inst(last_id)
            payload_insert.field_value = ir_val_inst(arg_result.val_id)
            payload_insert.field_index = field_idx + ai
            payload_insert.result_type = enum_name
            payload_insert.has_result = true
            b.instructions.push(payload_insert)
            g.builder.next_id = g.builder.next_id + 1

            last_id = payload_insert_id
            ai = ai + 1
        }
    }

    return expr_result(g, b, last_id)
}

-- ============================================================
-- DYNAMIC TRAIT DISPATCH
-- Looks up the concrete type for a dyn Trait variable and
-- calls ConcreteType_method(data_ptr, args...)
-- ============================================================

fn generate_dyn_dispatch_call(gen: IRGenerator, block: IRBasicBlock, obj_name: string, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let concrete_type = g.dyn_var_concrete.get(obj_name)
    let mangled = concrete_type + "_" + method_name

    let alloca_id = g.variable_map.get(obj_name)

    -- Determine if this is a direct struct param or an indirect let-binding
    -- For function params, the alloca holds the struct directly (load as struct type)
    -- For let-bindings, the alloca holds a ptr to heap data (load as ptr)
    let is_direct_struct = g.var_struct_types.contains(obj_name) and g.variable_types.contains(obj_name) and g.variable_types.get(obj_name) != "ptr"

    let mut self_val_id = 0
    if is_direct_struct {
        -- Load the struct value directly from the alloca
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(alloca_id)
        load_inst.load_type = concrete_type
        load_inst.result_type = concrete_type
        load_inst.has_result = true
        b.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1
        self_val_id = load_id
    } else {
        -- Load the data pointer from the dyn variable's alloca (indirect)
        let load_id = g.builder.next_id
        let mut load_inst = ir_inst_new(load_id, OP_LOAD())
        load_inst.load_ptr = ir_val_inst(alloca_id)
        load_inst.load_type = "ptr"
        load_inst.result_type = "ptr"
        load_inst.has_result = true
        b.instructions.push(load_inst)
        g.builder.next_id = g.builder.next_id + 1
        self_val_id = load_id
    }

    -- Build args: self value, then method arguments
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(self_val_id))

    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    let ret_type = infer_call_return_type(g, mangled)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = mangled
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- MAP METHOD CALL
-- Dispatches .insert, .get, .contains, .remove, .len,
-- .keys, .values, .set on Map[K,V] variables.
-- ============================================================

fn generate_map_method_call(gen: IRGenerator, block: IRBasicBlock, mk: int, map_ptr: int, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- ---- insert / set ----
    if method_name == "insert" or method_name == "set" {
        if args.len() >= 2 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            let val_result = generate_expr(g, b, args[1])
            g = val_result.gen
            b = val_result.block

            let insert_fn = if mk == MAP_KIND_STRING_INT() {
                "dm_map_string_int_insert"
            } else {
                "dm_map_int_string_insert"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = insert_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(map_ptr))
            call_args.push(ir_val_inst(key_result.val_id))
            call_args.push(ir_val_inst(val_result.val_id))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- ---- get ----
    if method_name == "get" {
        if args.len() >= 1 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            if mk == MAP_KIND_STRING_INT() {
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = "dm_map_string_int_get"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(map_ptr))
                call_args.push(ir_val_inst(key_result.val_id))
                inst.call_args = call_args
                inst.result_type = "i64"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, id)
            } else {
                -- MAP_KIND_INT_STRING: get returns string via out param
                let out_alloca_id = g.builder.next_id
                let mut out_inst = ir_inst_new(out_alloca_id, OP_ALLOCA())
                out_inst.alloc_type = "string"
                out_inst.result_type = "ptr(string)"
                out_inst.has_result = true
                b.instructions.push(out_inst)
                g.builder.next_id = g.builder.next_id + 1

                let call_id = g.builder.next_id
                let mut call_inst = ir_inst_new(call_id, OP_CALL())
                call_inst.callee = "dm_map_int_string_get"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(out_alloca_id))
                call_args.push(ir_val_inst(map_ptr))
                call_args.push(ir_val_inst(key_result.val_id))
                call_inst.call_args = call_args
                call_inst.result_type = "void"
                call_inst.has_result = false
                b.instructions.push(call_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Load the result from out_alloca
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(out_alloca_id)
                load_inst.load_type = "string"
                load_inst.result_type = "string"
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1
                return expr_result(g, b, load_id)
            }
        }
        return expr_result(g, b, 0)
    }

    -- ---- contains ----
    if method_name == "contains" {
        if args.len() >= 1 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            let contains_fn = if mk == MAP_KIND_STRING_INT() {
                "dm_map_string_int_contains"
            } else {
                "dm_map_int_string_contains"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = contains_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(map_ptr))
            call_args.push(ir_val_inst(key_result.val_id))
            inst.call_args = call_args
            inst.result_type = "bool"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- ---- remove ----
    if method_name == "remove" {
        if args.len() >= 1 {
            let key_result = generate_expr(g, b, args[0])
            g = key_result.gen
            b = key_result.block

            let remove_fn = if mk == MAP_KIND_STRING_INT() {
                "dm_map_string_int_remove"
            } else {
                "dm_map_int_string_remove"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = remove_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(map_ptr))
            call_args.push(ir_val_inst(key_result.val_id))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- ---- len ----
    if method_name == "len" {
        let len_fn = if mk == MAP_KIND_STRING_INT() {
            "dm_map_string_int_len"
        } else {
            "dm_map_int_string_len"
        }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = len_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(map_ptr))
        inst.call_args = call_args
        inst.result_type = "i64"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- keys ----
    if method_name == "keys" {
        let list_type = if mk == MAP_KIND_STRING_INT() {
            "dm_list_dm_string"
        } else {
            "dm_list_int64"
        }
        let keys_fn = if mk == MAP_KIND_STRING_INT() {
            "dm_map_string_int_keys"
        } else {
            "dm_map_int_string_keys"
        }

        -- Allocate a list to receive the keys
        let list_alloca_id = g.builder.next_id
        let mut list_inst = ir_inst_new(list_alloca_id, OP_ALLOCA())
        list_inst.alloc_type = list_type
        list_inst.result_type = "ptr(" + list_type + ")"
        list_inst.has_result = true
        b.instructions.push(list_inst)
        g.builder.next_id = g.builder.next_id + 1

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = keys_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_alloca_id))
        call_args.push(ir_val_inst(map_ptr))
        inst.call_args = call_args
        inst.result_type = "void"
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, list_alloca_id)
    }

    -- ---- values ----
    if method_name == "values" {
        let list_type = if mk == MAP_KIND_STRING_INT() {
            "dm_list_int64"
        } else {
            "dm_list_dm_string"
        }
        let values_fn = if mk == MAP_KIND_STRING_INT() {
            "dm_map_string_int_values"
        } else {
            "dm_map_int_string_values"
        }

        let list_alloca_id = g.builder.next_id
        let mut list_inst = ir_inst_new(list_alloca_id, OP_ALLOCA())
        list_inst.alloc_type = list_type
        list_inst.result_type = "ptr(" + list_type + ")"
        list_inst.has_result = true
        b.instructions.push(list_inst)
        g.builder.next_id = g.builder.next_id + 1

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = values_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_alloca_id))
        call_args.push(ir_val_inst(map_ptr))
        inst.call_args = call_args
        inst.result_type = "void"
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, list_alloca_id)
    }

    -- Unknown map method; return undef
    return expr_result(g, b, 0)
}

-- ============================================================
-- LIST METHOD CALL
-- Dispatches .push, .pop, .len, .contains on List[T] variables.
-- Runtime function names are selected based on elem_kind.
-- ============================================================

fn generate_list_method_call(gen: IRGenerator, block: IRBasicBlock, ek: int, list_ptr: int, obj_var_name: string, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- ---- push ----
    if method_name == "push" {
        if args.len() < 1 {
            return expr_result(g, b, 0)
        }

        let val_result = generate_expr(g, b, args[0])
        g = val_result.gen
        b = val_result.block

        if ek == LIST_ELEM_OTHER() {
            -- Generic push: alloca temp, store struct value, pass pointer + elem_size
            let elem_type_name = if len(obj_var_name) > 0 and g.list_elem_types.contains(obj_var_name) {
                g.list_elem_types.get(obj_var_name)
            } else {
                "i64"
            }
            let elem_size = estimate_type_size_gen(g, elem_type_name)

            -- Alloca temp for the value
            let temp_alloca_id = g.builder.next_id
            let mut temp_inst = ir_inst_new(temp_alloca_id, OP_ALLOCA())
            temp_inst.alloc_type = elem_type_name
            temp_inst.result_type = "ptr(" + elem_type_name + ")"
            temp_inst.has_result = true
            b.instructions.push(temp_inst)
            g.builder.next_id = g.builder.next_id + 1

            -- Store value into temp
            let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
            store_inst.store_ptr = ir_val_inst(temp_alloca_id)
            store_inst.store_val = ir_val_inst(val_result.val_id)
            store_inst.has_result = false
            b.instructions.push(store_inst)
            g.builder.next_id = g.builder.next_id + 1

            -- Call dm_list_generic_push(list_ptr, temp_ptr, elem_size)
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = "dm_list_generic_push"
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(list_ptr))
            call_args.push(ir_val_inst(temp_alloca_id))
            call_args.push(ir_val_int(elem_size))
            inst.call_args = call_args
            inst.result_type = "void"
            inst.has_result = false
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }

        let push_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_push" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_push" }
        else { "dm_list_string_push" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = push_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_ptr))
        call_args.push(ir_val_inst(val_result.val_id))
        inst.call_args = call_args
        inst.result_type = "void"
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- pop ----
    if method_name == "pop" {
        let pop_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_pop" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_get" }
        else if ek == LIST_ELEM_STRING() { "dm_list_string_get" }
        else { "dm_list_int64_pop" }

        let ret_type = if ek == LIST_ELEM_FLOAT() { "f64" }
        else if ek == LIST_ELEM_STRING() { "string" }
        else { "i64" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = pop_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_ptr))
        inst.call_args = call_args
        inst.result_type = ret_type
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- len ----
    if method_name == "len" {
        let len_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_len" }
        else if ek == LIST_ELEM_FLOAT() { "dm_list_double_len" }
        else if ek == LIST_ELEM_STRING() { "dm_list_string_len" }
        else { "dm_list_generic_len" }

        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_CALL())
        inst.callee = len_fn
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(list_ptr))
        inst.call_args = call_args
        inst.result_type = "i64"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- ---- contains ----
    if method_name == "contains" {
        if args.len() >= 1 {
            let search_result = generate_expr(g, b, args[0])
            g = search_result.gen
            b = search_result.block

            let contains_fn = if ek == LIST_ELEM_INT() { "dm_list_int64_contains" }
            else if ek == LIST_ELEM_STRING() { "dm_list_string_contains" }
            else { "dm_list_int64_contains" }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = contains_fn
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(list_ptr))
            call_args.push(ir_val_inst(search_result.val_id))
            inst.call_args = call_args
            inst.result_type = "bool"
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = g.builder.next_id + 1
            return expr_result(g, b, id)
        }
        return expr_result(g, b, 0)
    }

    -- Unknown list method; return undef
    return expr_result(g, b, 0)
}

-- ============================================================
-- STRUCT METHOD CALL
-- Builds mangled name (StructType_method), passes object as
-- self (first arg), and emits OP_CALL.
-- Handles both self-by-value and mut self (pass-by-pointer).
-- ============================================================

fn generate_struct_method_call(gen: IRGenerator, block: IRBasicBlock, struct_type: string, obj_expr: Expr, method_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    let mangled = struct_type + "_" + method_name

    -- --------------------------------------------------------
    -- GENERIC IMPL METHOD: monomorphize on demand
    -- --------------------------------------------------------
    if g.generic_fn_decls.contains(mangled) {
        let decl_idx = g.generic_fn_decls.get(mangled)
        if decl_idx >= 0 and decl_idx < g.declarations.len() {
            let decl = g.declarations[decl_idx]
            if decl.kind == DECL_FUNCTION() {
                let fd = *decl.func_decl

                -- Check if this method takes self (first param named "self")
                let mut has_self = false
                if fd.params.len() > 0 {
                    has_self = fd.params[0].name == "self"
                }

                -- Build the effective argument list for type inference
                -- If method takes self, include obj_expr; otherwise, just args
                let mut effective_args: List[Expr] = []
                if has_self {
                    effective_args.push(obj_expr)
                }
                let mut ai = 0
                while ai < args.len() {
                    effective_args.push(args[ai])
                    ai = ai + 1
                }

                -- Infer type arguments from effective args
                let mut type_args: List[string] = []
                let mut pi = 0
                while pi < fd.generic_params.len() {
                    let gp = fd.generic_params[pi]
                    -- Try to infer this generic param from matching param types
                    let mut inferred = "i64"
                    let mut mi = 0
                    while mi < fd.params.len() and mi < effective_args.len() {
                        if fd.params[mi].type_name == gp {
                            inferred = infer_arg_type(g, effective_args[mi])
                        }
                        mi = mi + 1
                    }
                    type_args.push(inferred)
                    pi = pi + 1
                }

                -- Build monomorphized name
                let mut mono_name = mangled
                let mut ti = 0
                while ti < type_args.len() {
                    mono_name = mono_name + "_" + map_type_name(type_args[ti])
                    ti = ti + 1
                }

                -- Monomorphize if not already done
                if g.mono_generated.contains(mono_name) == false {
                    g.mono_generated.insert(mono_name, true)

                    -- Save current state
                    let saved_variable_map = g.variable_map
                    let saved_variable_types = g.variable_types
                    let saved_list_elem_kinds = g.list_elem_kinds
                    let saved_list_elem_types = g.list_elem_types
                    let saved_return_type = g.current_return_type
                    let saved_type_subs = g.type_substitutions
                    let saved_completed_blocks = g.completed_blocks
                    let saved_next_id = g.builder.next_id
                    let saved_impl_type = g.current_impl_type
                    let saved_has_impl = g.has_impl_type

                    g.variable_map = Map_new()
                    g.variable_types = Map_new()

                    -- Set up type substitutions
                    g.type_substitutions = Map_new()
                    let mut si = 0
                    while si < fd.generic_params.len() and si < type_args.len() {
                        g.type_substitutions.insert(fd.generic_params[si], type_args[si])
                        si = si + 1
                    }

                    -- Set impl type context for Self resolution
                    g.current_impl_type = struct_type
                    g.has_impl_type = true

                    -- Create monomorphized FunctionDecl
                    let mut mono_fd = fd
                    let empty_gp: List[string] = []
                    mono_fd.generic_params = empty_gp

                    -- Resolve param types
                    let mut mono_params: List[FuncParam] = []
                    let mut mpi = 0
                    while mpi < fd.params.len() {
                        let p = fd.params[mpi]
                        let resolved = resolve_type_substitution(g, p.type_name)
                        let mut mp = func_param_new(p.name, resolved)
                        mp.is_mut = p.is_mut
                        mono_params.push(mp)
                        mpi = mpi + 1
                    }
                    mono_fd.params = mono_params

                    -- Resolve return type
                    let resolved_ret = resolve_type_substitution(g, fd.return_type)
                    mono_fd.return_type = resolved_ret

                    -- Also register the monomorphized struct type if needed
                    -- e.g., Pair[A,B] with A=int, B=string → register Pair_i64_string struct
                    let mono_struct = struct_type + "_" + join_types(type_args)
                    if g.struct_defs.contains(struct_type) and g.struct_defs.contains(mono_struct) == false {
                        let base_info = g.struct_defs.get(struct_type)
                        let mut mono_info = struct_info_new(mono_struct)
                        let mut mono_fields: List[IRField] = []
                        let mut fi = 0
                        while fi < base_info.field_names.len() {
                            let fname = base_info.field_names[fi]
                            let ftype = resolve_type_substitution(g, base_info.field_types[fi])
                            mono_info.field_names.push(fname)
                            mono_info.field_types.push(map_type_name(ftype))
                            mono_fields.push(ir_field_new(fname, map_type_name(ftype)))
                            fi = fi + 1
                        }
                        g.struct_defs.insert(mono_struct, mono_info)
                        let mut ir_ty = ir_type_named(IR_TYPE_STRUCT(), mono_struct)
                        ir_ty.fields = mono_fields
                        g.ir_mod.struct_defs.insert(mono_struct, ir_ty)
                    }

                    -- Map the base struct name to monomorphized name for struct literals
                    g.type_substitutions.insert(struct_type, mono_struct)

                    g = declare_function(g, mono_fd, mono_name)
                    g = generate_function_body(g, mono_fd, mono_name)

                    -- Restore state
                    g.variable_map = saved_variable_map
                    g.variable_types = saved_variable_types
                    g.list_elem_kinds = saved_list_elem_kinds
                    g.list_elem_types = saved_list_elem_types
                    g.current_return_type = saved_return_type
                    g.type_substitutions = saved_type_subs
                    g.completed_blocks = saved_completed_blocks
                    g.builder.next_id = saved_next_id
                    g.current_impl_type = saved_impl_type
                    g.has_impl_type = saved_has_impl
                }

                -- Generate the call to monomorphized function
                let mut call_args: List[IRValue] = []
                let mut ci = 0
                while ci < effective_args.len() {
                    let arg_result = generate_expr(g, b, effective_args[ci])
                    g = arg_result.gen
                    b = arg_result.block
                    call_args.push(ir_val_inst(arg_result.val_id))
                    ci = ci + 1
                }

                let ret_type = infer_call_return_type(g, mono_name)
                let has_result = ret_type != "void"

                let id = g.builder.next_id
                let mut call_inst = ir_inst_new(id, OP_CALL())
                call_inst.callee = mono_name
                call_inst.call_args = call_args
                call_inst.result_type = ret_type
                call_inst.has_result = has_result
                b.instructions.push(call_inst)
                g.builder.next_id = g.builder.next_id + 1

                return expr_result(g, b, id)
            }
        }
    }

    -- --------------------------------------------------------
    -- NON-GENERIC IMPL METHOD: direct call
    -- --------------------------------------------------------

    -- Check if this method takes a mut self parameter (pass-by-pointer)
    let mut takes_mut_self = false
    if g.mut_param_fns.contains(mangled) {
        let flags = g.mut_param_fns.get(mangled)
        takes_mut_self = starts_with(flags, "1")
    }

    let mut call_args: List[IRValue] = []

    if takes_mut_self and obj_expr.kind == EXPR_IDENTIFIER() {
        -- Pass the alloca pointer directly for mut self
        let obj_name = obj_expr.name
        if g.variable_map.contains(obj_name) {
            -- If the object is itself a mut param, forward the param pointer
            if g.param_ptr_map.contains(obj_name) {
                let pidx = g.param_ptr_map.get(obj_name)
                call_args.push(ir_val_param(pidx))
            } else {
                let ptr_id = g.variable_map.get(obj_name)
                call_args.push(ir_val_inst(ptr_id))
            }
        } else {
            let obj_result = generate_expr(g, b, obj_expr)
            g = obj_result.gen
            b = obj_result.block
            call_args.push(ir_val_inst(obj_result.val_id))
        }
    } else {
        -- Pass the object value (self by value)
        let obj_result = generate_expr(g, b, obj_expr)
        g = obj_result.gen
        b = obj_result.block
        call_args.push(ir_val_inst(obj_result.val_id))
    }

    -- Generate remaining arguments
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block
        call_args.push(ir_val_inst(arg_result.val_id))
        i = i + 1
    }

    let ret_type = infer_call_return_type(g, mangled)
    let has_result = ret_type != "void"

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = mangled
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = has_result
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- IR Generation: Field Access, Index Access, Struct Literals,
--                Enum Literals, Array Literals
-- ============================================================
-- Part 4 of expression IR generation for the Stage 4 compiler.
-- Each function follows the signature:
--   fn xxx(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult
-- and returns expr_result(g, b, result_id).

-- ============================================================
-- Field Access: expr.object.expr.field
-- ============================================================

fn generate_field_access(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let object = *expr.object
    let field_name = expr.field

    -- Generate the base object expression
    let obj_result = generate_expr(g, b, object)
    g = obj_result.gen
    b = obj_result.block
    let obj_id = obj_result.val_id

    -- Determine the struct type name by examining the object expression
    let mut struct_name = ""
    if object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name
        if g.var_struct_types.contains(var_name) {
            struct_name = g.var_struct_types.get(var_name)
        }
    }

    -- Check for list index result struct type (e.g., points[0].x)
    if struct_name == "" {
        let listget_key = "_listget_" + int_to_string(obj_id)
        if g.var_struct_types.contains(listget_key) {
            struct_name = g.var_struct_types.get(listget_key)
        }
    }

    -- Check for chained field access struct type (e.g., a.b.x)
    if struct_name == "" {
        let field_key = "_field_" + int_to_string(obj_id)
        if g.var_struct_types.contains(field_key) {
            struct_name = g.var_struct_types.get(field_key)
        }
    }

    -- Check for Box[T] field access: dereference pointer then access struct field
    if struct_name == "" and object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name
        if g.box_inner_types.contains(var_name) {
            let inner_type = g.box_inner_types.get(var_name)
            if g.struct_defs.contains(inner_type) {
                let info = g.struct_defs.get(inner_type)
                -- Load the struct from the box pointer
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(obj_id)
                load_inst.load_type = inner_type
                load_inst.result_type = inner_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Find the field index by name
                let mut fidx = 0
                let mut field_type_s = "i64"
                let mut ffi = 0
                while ffi < info.field_names.len() {
                    if info.field_names[ffi] == field_name {
                        fidx = ffi
                        field_type_s = info.field_types[ffi]
                    }
                    ffi = ffi + 1
                }
                let ir_type = map_type_name(field_type_s)

                -- Extract field from loaded struct
                let extract_id = g.builder.next_id
                let mut extract_inst = ir_inst_new(extract_id, OP_EXTRACT_FIELD())
                extract_inst.field_base = ir_val_inst(load_id)
                extract_inst.field_index = fidx
                extract_inst.field_type = ir_type
                extract_inst.result_type = ir_type
                extract_inst.has_result = true
                b.instructions.push(extract_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Track struct result for chained access
                if g.struct_defs.contains(field_type_s) {
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.var_struct_types.insert(result_var, field_type_s)
                }
                -- Track Box[T] result for chained box access
                if starts_with(field_type_s, "Box[") {
                    let box_inner = extract_inner_type(field_type_s, "Box[")
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.box_inner_types.insert(result_var, box_inner)
                }

                return expr_result(g, b, extract_id)
            }
        }
    }

    -- Check for Box field access from chained field access (non-identifier objects)
    -- e.g., o.inner.value where inner: Box[Inner] — obj_id is the result of extracting inner
    if struct_name == "" {
        let box_key = "_field_" + int_to_string(obj_id)
        if g.box_inner_types.contains(box_key) {
            let inner_type = g.box_inner_types.get(box_key)
            if g.struct_defs.contains(inner_type) {
                let info = g.struct_defs.get(inner_type)
                -- Load the struct from the box pointer
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(obj_id)
                load_inst.load_type = inner_type
                load_inst.result_type = inner_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = g.builder.next_id + 1

                let mut fidx = 0
                let mut field_type_s = "i64"
                let mut ffi = 0
                while ffi < info.field_names.len() {
                    if info.field_names[ffi] == field_name {
                        fidx = ffi
                        field_type_s = info.field_types[ffi]
                    }
                    ffi = ffi + 1
                }
                let ir_type = map_type_name(field_type_s)

                let extract_id = g.builder.next_id
                let mut extract_inst = ir_inst_new(extract_id, OP_EXTRACT_FIELD())
                extract_inst.field_base = ir_val_inst(load_id)
                extract_inst.field_index = fidx
                extract_inst.field_type = ir_type
                extract_inst.result_type = ir_type
                extract_inst.has_result = true
                b.instructions.push(extract_inst)
                g.builder.next_id = g.builder.next_id + 1

                if g.struct_defs.contains(field_type_s) {
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.var_struct_types.insert(result_var, field_type_s)
                }
                if starts_with(field_type_s, "Box[") {
                    let box_inner = extract_inner_type(field_type_s, "Box[")
                    let result_var = "_field_" + int_to_string(extract_id)
                    g.box_inner_types.insert(result_var, box_inner)
                }

                return expr_result(g, b, extract_id)
            }
        }
    }

    -- If we could not resolve from a direct identifier, try from the
    -- variable_types map (e.g., for chained field access results)
    if struct_name == "" {
        if object.kind == EXPR_IDENTIFIER() {
            let var_name = object.name
            if g.variable_types.contains(var_name) {
                let vtype = g.variable_types.get(var_name)
                -- If the type string names a known struct, use it
                if g.struct_defs.contains(vtype) {
                    struct_name = vtype
                }
            }
        }
    }

    -- Check for Option/Result types before falling back
    if struct_name == "" and object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name
        if g.variable_types.contains(var_name) {
            let vtype = g.variable_types.get(var_name)
            if starts_with(vtype, "Option[") {
                -- Option[T] = { i32 has_value, T value }
                let inner = substr(vtype, 7, len(vtype) - 8)
                let inner_ir = map_type_name(inner)
                let mut fidx = 0
                let mut ftype = "i32"
                if field_name == "value" {
                    fidx = 1
                    ftype = inner_ir
                }
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
                inst.field_base = ir_val_inst(obj_id)
                inst.field_index = fidx
                inst.field_type = ftype
                inst.result_type = ftype
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = id + 1
                return expr_result(g, b, id)
            }
            if starts_with(vtype, "Result[") {
                -- Result[T, E] = { i32 tag, T ok_val, E err_val }
                let inner_str = substr(vtype, 7, len(vtype) - 8)
                -- Parse "T, E" from inner_str
                let mut comma_pos = 0
                let mut ci = 0
                while ci < len(inner_str) {
                    if char_at(inner_str, ci) == "," {
                        comma_pos = ci
                    }
                    ci = ci + 1
                }
                let ok_type = string_trim(substr(inner_str, 0, comma_pos))
                let err_type = string_trim(substr(inner_str, comma_pos + 1, len(inner_str) - comma_pos - 1))
                let ok_ir = map_type_name(ok_type)
                let err_ir = map_type_name(err_type)
                if field_name == "is_ok" {
                    -- Extract tag (field 0) and compare with 0 to get boolean
                    -- is_ok should be true when tag == 0 (Ok), false when tag == 1 (Err)
                    let tag_id = g.builder.next_id
                    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
                    tag_inst.field_base = ir_val_inst(obj_id)
                    tag_inst.field_index = 0
                    tag_inst.field_type = "i32"
                    tag_inst.result_type = "i32"
                    tag_inst.has_result = true
                    b.instructions.push(tag_inst)
                    g.builder.next_id = g.builder.next_id + 1

                    let cmp_id = g.builder.next_id
                    let mut cmp_inst = ir_inst_new(cmp_id, OP_EQ())
                    cmp_inst.lhs = ir_val_inst(tag_id)
                    cmp_inst.rhs = ir_val_int(0)
                    cmp_inst.result_type = "bool"
                    cmp_inst.has_result = true
                    b.instructions.push(cmp_inst)
                    g.builder.next_id = g.builder.next_id + 1

                    return expr_result(g, b, cmp_id)
                }
                let mut fidx = 0
                let mut ftype = "i32"
                if field_name == "ok" or field_name == "value" {
                    fidx = 1
                    ftype = ok_ir
                }
                if field_name == "err" or field_name == "error" {
                    fidx = 2
                    ftype = err_ir
                }
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
                inst.field_base = ir_val_inst(obj_id)
                inst.field_index = fidx
                inst.field_type = ftype
                inst.result_type = ftype
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = id + 1
                return expr_result(g, b, id)
            }
        }
    }

    -- Look up struct definition to find the field index and type
    if struct_name == "" {
        -- Cannot resolve struct type; emit an extract at index 0 as fallback
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
        inst.field_base = ir_val_inst(obj_id)
        inst.field_index = 0
        inst.field_type = "i64"
        inst.result_type = "i64"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    let info = g.struct_defs.get(struct_name)

    -- Find the field index by name
    let mut field_idx = 0
    let mut found = false
    let mut field_type_str = "i64"
    let num_fields = info.field_names.len()
    let mut fi = 0
    while fi < num_fields {
        if info.field_names[fi] == field_name {
            field_idx = fi
            field_type_str = info.field_types[fi]
            found = true
        }
        fi = fi + 1
    }

    -- Map the dAImond type name to an IR type string
    let ir_type = map_type_name(field_type_str)

    -- Emit OP_EXTRACT_FIELD instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_EXTRACT_FIELD())
    inst.field_base = ir_val_inst(obj_id)
    inst.field_index = field_idx
    inst.field_type = ir_type
    inst.result_type = ir_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    -- Track list-typed fields for later index access resolution
    if starts_with(field_type_str, "List[") {
        let inner = substr(field_type_str, 5, len(field_type_str) - 6)
        let key = struct_name + "." + field_name
        if inner == "int" {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_INT())
        } else if inner == "float" {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_FLOAT())
        } else if inner == "string" {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_STRING())
        } else {
            g.field_list_elem_kinds.insert(key, LIST_ELEM_OTHER())
            g.field_list_elem_types.insert(key, inner)
        }
    }

    -- If the field type is itself a struct, track the result variable mapping
    if g.struct_defs.contains(field_type_str) {
        -- Use a synthetic name based on instruction id for chained access tracking
        let result_var = "_field_" + int_to_string(id)
        g.var_struct_types.insert(result_var, field_type_str)
    }

    -- If the field type is Box[T], track for chained box access
    if starts_with(field_type_str, "Box[") {
        let box_inner = extract_inner_type(field_type_str, "Box[")
        let result_var = "_field_" + int_to_string(id)
        g.box_inner_types.insert(result_var, box_inner)
    }

    return expr_result(g, b, id)
}

-- ============================================================
-- Index Access: expr.object[expr.index]
-- ============================================================

fn generate_index_access(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let object = *expr.object
    let index = *expr.index

    -- Generate the object expression
    let obj_result = generate_expr(g, b, object)
    g = obj_result.gen
    b = obj_result.block
    let obj_id = obj_result.val_id

    -- Generate the index expression
    let idx_result = generate_expr(g, b, index)
    g = idx_result.gen
    b = idx_result.block
    let idx_id = idx_result.val_id

    -- Check if the object is a string variable (s[i] -> char_at)
    if object.kind == EXPR_IDENTIFIER() {
        let str_var_name = object.name
        if g.variable_types.contains(str_var_name) {
            let vtype = g.variable_types.get(str_var_name)
            if vtype == "string" {
                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = "dm_char_at"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(obj_id))
                call_args.push(ir_val_inst(idx_id))
                inst.call_args = call_args
                inst.result_type = "string"
                inst.has_result = true
                b.instructions.push(inst)
                g.builder.next_id = id + 1
                return expr_result(g, b, id)
            }
        }
    }

    -- Check if the object is a known list variable
    if object.kind == EXPR_IDENTIFIER() {
        let var_name = object.name

        -- List variable index access
        if g.list_elem_kinds.contains(var_name) {
            let elem_kind = g.list_elem_kinds.get(var_name)

            -- Determine element type from kind
            let mut elem_type = "i64"
            if elem_kind == LIST_ELEM_INT() {
                elem_type = "i64"
            } else if elem_kind == LIST_ELEM_FLOAT() {
                elem_type = "f64"
            } else if elem_kind == LIST_ELEM_STRING() {
                elem_type = "string"
            } else if elem_kind == LIST_ELEM_OTHER() {
                if g.list_elem_types.contains(var_name) {
                    elem_type = g.list_elem_types.get(var_name)
                }
            }

            if elem_kind == LIST_ELEM_OTHER() {
                -- Generic list get: alloca temp, call dm_list_generic_get(out_ptr, list_ptr, idx, elem_size)
                let elem_size = estimate_type_size_gen(g, elem_type)
                let temp_alloca_id = g.builder.next_id
                let mut temp_inst = ir_inst_new(temp_alloca_id, OP_ALLOCA())
                temp_inst.alloc_type = elem_type
                temp_inst.result_type = "ptr(" + elem_type + ")"
                temp_inst.has_result = true
                b.instructions.push(temp_inst)
                g.builder.next_id = g.builder.next_id + 1

                let id = g.builder.next_id
                let mut inst = ir_inst_new(id, OP_CALL())
                inst.callee = "dm_list_generic_get"
                let mut call_args: List[IRValue] = []
                call_args.push(ir_val_inst(temp_alloca_id))
                call_args.push(ir_val_inst(obj_id))
                call_args.push(ir_val_inst(idx_id))
                call_args.push(ir_val_int(elem_size))
                inst.call_args = call_args
                inst.result_type = "void"
                inst.has_result = false
                b.instructions.push(inst)
                g.builder.next_id = id + 1

                -- Load the struct from temp alloca
                let load_id = g.builder.next_id
                let mut load_inst = ir_inst_new(load_id, OP_LOAD())
                load_inst.load_ptr = ir_val_inst(temp_alloca_id)
                load_inst.load_type = elem_type
                load_inst.result_type = elem_type
                load_inst.has_result = true
                b.instructions.push(load_inst)
                g.builder.next_id = load_id + 1

                -- Track struct type for subsequent field access
                if g.struct_defs.contains(elem_type) {
                    g.var_struct_types.insert("_listget_" + int_to_string(load_id), elem_type)
                }

                return expr_result(g, b, load_id)
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_LIST_GET())
            inst.lhs = ir_val_inst(obj_id)
            inst.rhs = ir_val_inst(idx_id)
            inst.result_type = map_type_name(elem_type)
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }

        -- Map variable index access
        if g.map_var_kinds.contains(var_name) {
            let map_kind = g.map_var_kinds.get(var_name)
            let mut callee_name = "dm_map_string_int_get"
            let mut ret_type = "i64"

            if map_kind == MAP_KIND_STRING_INT() {
                callee_name = "dm_map_string_int_get"
                ret_type = "i64"
            } else if map_kind == MAP_KIND_INT_STRING() {
                callee_name = "dm_map_int_string_get"
                ret_type = "string"
            }

            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_CALL())
            inst.callee = callee_name
            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(obj_id))
            call_args.push(ir_val_inst(idx_id))
            inst.call_args = call_args
            inst.result_type = ret_type
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1
            return expr_result(g, b, id)
        }
    }

    -- Check for field access list indexing: obj.field[i]
    if object.kind == EXPR_FIELD_ACCESS() {
        let field_obj = *object.object
        let field_name = object.field

        if field_obj.kind == EXPR_IDENTIFIER() {
            let parent_var = field_obj.name
            if g.var_struct_types.contains(parent_var) {
                let parent_struct = g.var_struct_types.get(parent_var)
                let key = parent_struct + "." + field_name
                if g.field_list_elem_kinds.contains(key) {
                    let elem_kind = g.field_list_elem_kinds.get(key)
                    let mut elem_type = "i64"
                    if elem_kind == LIST_ELEM_INT() {
                        elem_type = "i64"
                    } else if elem_kind == LIST_ELEM_FLOAT() {
                        elem_type = "f64"
                    } else if elem_kind == LIST_ELEM_STRING() {
                        elem_type = "string"
                    } else if elem_kind == LIST_ELEM_OTHER() {
                        if g.field_list_elem_types.contains(key) {
                            elem_type = g.field_list_elem_types.get(key)
                        }
                    }

                    let id = g.builder.next_id
                    let mut inst = ir_inst_new(id, OP_LIST_GET())
                    inst.lhs = ir_val_inst(obj_id)
                    inst.rhs = ir_val_inst(idx_id)
                    inst.result_type = map_type_name(elem_type)
                    inst.has_result = true
                    b.instructions.push(inst)
                    g.builder.next_id = id + 1
                    return expr_result(g, b, id)
                }
            }
        }
    }

    -- Default: treat as a list of i64
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_LIST_GET())
    inst.lhs = ir_val_inst(obj_id)
    inst.rhs = ir_val_inst(idx_id)
    inst.result_type = "i64"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1
    return expr_result(g, b, id)
}

-- ============================================================
-- Struct Literal: TypeName { field1: val1, field2: val2, ... }
-- ============================================================

fn generate_struct_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Resolve the struct type through type_substitutions for monomorphization
    let type_name = resolve_type_substitution(g, expr.type_name)

    -- Look up the struct definition
    if g.struct_defs.contains(type_name) == false {
        -- Unknown struct type; return undef
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
        inst.field_base = ir_val_undef()
        inst.field_value = ir_val_int(0)
        inst.field_index = 0
        inst.result_type = type_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    let info = g.struct_defs.get(type_name)
    let num_fields = expr.field_names.len()

    -- Build the struct value by chaining INSERT_FIELD instructions.
    -- The first insert uses undef as the base; each subsequent insert
    -- takes the result of the previous one as its base.
    let mut current_id = -1
    let mut i = 0
    while i < num_fields {
        let fname = expr.field_names[i]
        let fval_expr = expr.field_values[i]

        -- Generate the field value expression
        let val_result = generate_expr(g, b, fval_expr)
        g = val_result.gen
        b = val_result.block
        let val_id = val_result.val_id

        -- Find the field index in the struct definition
        let mut field_idx = 0
        let def_count = info.field_names.len()
        let mut fi = 0
        while fi < def_count {
            if info.field_names[fi] == fname {
                field_idx = fi
            }
            fi = fi + 1
        }

        -- Emit OP_INSERT_FIELD
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())

        -- Base is undef for the first field, or the previous insert result
        if current_id == -1 {
            inst.field_base = ir_val_undef()
        } else {
            inst.field_base = ir_val_inst(current_id)
        }

        inst.field_value = ir_val_inst(val_id)
        inst.field_index = field_idx
        inst.result_type = type_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1

        current_id = id
        i = i + 1
    }

    -- Handle empty struct (no fields specified in literal)
    if current_id == -1 {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
        inst.field_base = ir_val_undef()
        inst.field_value = ir_val_int(0)
        inst.field_index = 0
        inst.result_type = type_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        current_id = id
    }

    return expr_result(g, b, current_id)
}

-- ============================================================
-- Enum Literal: EnumName.VariantName or EnumName.VariantName(payload...)
-- ============================================================

fn generate_enum_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let enum_name = expr.enum_name
    let variant_name = expr.variant_name

    -- Look up the enum definition
    if g.enum_defs.contains(enum_name) == false {
        -- Unknown enum; return a zero-tagged undef
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
        inst.field_base = ir_val_undef()
        inst.field_value = ir_val_int(0)
        inst.field_index = 0
        inst.result_type = enum_name
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    let info = g.enum_defs.get(enum_name)

    -- Find the variant index and its tag value
    let mut variant_tag = 0
    let mut variant_idx = -1
    let mut has_payload = false
    let num_variants = info.variant_names.len()
    let mut vi = 0
    while vi < num_variants {
        if info.variant_names[vi] == variant_name {
            variant_tag = info.variant_tags[vi]
            variant_idx = vi
            has_payload = info.variant_has_payload[vi]
        }
        vi = vi + 1
    }

    -- Step 1: Insert tag field at index 0
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_INSERT_FIELD())
    tag_inst.field_base = ir_val_undef()
    tag_inst.field_value = ir_val_int(variant_tag)
    tag_inst.field_index = 0
    tag_inst.result_type = enum_name
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = tag_id + 1

    let mut current_id = tag_id

    -- Step 2: If the variant has payload, generate payload expressions
    -- and insert them starting at field index 1.
    -- The field offset must skip past prior variant payload fields in the
    -- tagged union layout.
    if has_payload {
        let payload = expr.payload
        let num_payload = payload.len()

        -- Compute the field offset: skip tag (1) + sum of prior variant payloads
        let mut field_offset = 1
        let mut pvi = 0
        while pvi < num_variants {
            if info.variant_names[pvi] == variant_name {
                -- Found our variant; stop counting
                pvi = num_variants
            } else {
                if info.variant_has_payload[pvi] {
                    -- Each prior payload variant contributes its payload count
                    -- For simplicity, assume one payload field per variant
                    field_offset = field_offset + 1
                }
                pvi = pvi + 1
            }
        }

        let mut pi = 0
        while pi < num_payload {
            let payload_expr = payload[pi]

            -- Generate the payload value
            let pval_result = generate_expr(g, b, payload_expr)
            g = pval_result.gen
            b = pval_result.block
            let pval_id = pval_result.val_id

            -- Insert payload at the computed field index
            let id = g.builder.next_id
            let mut inst = ir_inst_new(id, OP_INSERT_FIELD())
            inst.field_base = ir_val_inst(current_id)
            inst.field_value = ir_val_inst(pval_id)
            inst.field_index = field_offset + pi
            inst.result_type = enum_name
            inst.has_result = true
            b.instructions.push(inst)
            g.builder.next_id = id + 1

            current_id = id
            pi = pi + 1
        }
    }

    return expr_result(g, b, current_id)
}

-- ============================================================
-- Array Literal: [elem1, elem2, ...]
-- ============================================================

fn generate_array_literal(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    let elements = expr.elements
    let num_elements = elements.len()

    -- Empty array: create a new empty list
    if num_elements == 0 {
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_LIST_NEW())
        inst.result_type = "list"
        inst.has_result = true
        b.instructions.push(inst)
        g.builder.next_id = id + 1
        return expr_result(g, b, id)
    }

    -- Generate all element expressions first to determine types
    let mut elem_ids: List[int] = []
    let mut i = 0
    while i < num_elements {
        let elem_result = generate_expr(g, b, elements[i])
        g = elem_result.gen
        b = elem_result.block
        elem_ids.push(elem_result.val_id)
        i = i + 1
    }

    -- Infer element type from the first element
    let elem_ir_type = infer_expr_type(g, elements[0])

    -- Determine which list new/push functions to use
    let mut new_fn = "dm_list_int64_new"
    let mut push_fn = "dm_list_int64_push"
    if elem_ir_type == "f64" or elem_ir_type == "float" {
        new_fn = "dm_list_double_new"
        push_fn = "dm_list_double_push"
    } else if elem_ir_type == "string" {
        new_fn = "dm_list_string_new"
        push_fn = "llvm_dm_list_string_push"
    }

    -- Create list alloca and call new function (sret pattern)
    let alloca_id = g.builder.next_id
    let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
    alloca_inst.alloc_type = "list"
    alloca_inst.result_type = "ptr(list)"
    alloca_inst.has_result = true
    b.instructions.push(alloca_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut call_new = ir_inst_new(g.builder.next_id, OP_CALL())
    call_new.callee = new_fn
    let mut new_args: List[IRValue] = []
    new_args.push(ir_val_inst(alloca_id))
    call_new.call_args = new_args
    call_new.result_type = "void"
    call_new.has_result = false
    b.instructions.push(call_new)
    g.builder.next_id = g.builder.next_id + 1

    -- Push each element into the list
    let mut pi = 0
    while pi < num_elements {
        let mut push_call = ir_inst_new(g.builder.next_id, OP_CALL())
        push_call.callee = push_fn
        let mut push_args: List[IRValue] = []
        push_args.push(ir_val_inst(alloca_id))
        push_args.push(ir_val_inst(elem_ids[pi]))
        push_call.call_args = push_args
        push_call.result_type = "void"
        push_call.has_result = false
        b.instructions.push(push_call)
        g.builder.next_id = g.builder.next_id + 1
        pi = pi + 1
    }

    return expr_result(g, b, alloca_id)
}

-- ============================================================
-- EXPRESSION IR GENERATION PART 5
-- If expressions, match expressions, block expressions,
-- lambda expressions, pipeline operator, error propagation,
-- string interpolation, and comptime expressions.
-- Port of stage3/src/ir_gen.zig complex expression handlers.
-- ============================================================

-- ============================================================
-- IF EXPRESSION (returns a value)
-- ============================================================

fn generate_if_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate condition expression
    let cond_result = generate_expr(g, b, *expr.condition)
    g = cond_result.gen
    b = cond_result.block
    let cond_id = cond_result.val_id

    -- Create labels for then, else, and join blocks
    let then_label = gen_next_label(g, "if_then")
    g = gen_advance_label(g)
    let else_label = gen_next_label(g, "if_else")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "if_join")
    g = gen_advance_label(g)

    -- Infer result type from then-branch expression
    let if_result_type = infer_expr_type(g, *expr.then_branch)

    -- Allocate a result slot before branching so both branches can store into it
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = if_result_type
    result_alloca.result_type = "ptr(" + if_result_type + ")"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Terminate current block with conditional branch
    if expr.has_else {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_id), then_label, else_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    } else {
        b.terminator = Box_new(term_br_cond(ir_val_inst(cond_id), then_label, join_label))
        b.has_terminator = true
        g.completed_blocks.push(b)
    }

    -- Generate then block
    let mut then_block = ir_block_new(then_label)
    let then_result = generate_expr(g, then_block, *expr.then_branch)
    g = then_result.gen
    then_block = then_result.block

    -- Store then result into the result slot
    let mut then_store = ir_inst_new(g.builder.next_id, OP_STORE())
    then_store.store_ptr = ir_val_inst(result_alloca_id)
    then_store.store_val = ir_val_inst(then_result.val_id)
    then_store.has_result = false
    then_block.instructions.push(then_store)
    g.builder.next_id = g.builder.next_id + 1

    -- Terminate then block with branch to join
    if then_block.has_terminator == false {
        then_block.terminator = Box_new(term_br(join_label))
        then_block.has_terminator = true
    }
    g.completed_blocks.push(then_block)
    -- Generate else block
    let mut else_block = ir_block_new(else_label)
    if expr.has_else {
        let else_result = generate_expr(g, else_block, *expr.else_branch)
        g = else_result.gen
        else_block = else_result.block

        -- Store else result into the result slot
        let mut else_store = ir_inst_new(g.builder.next_id, OP_STORE())
        else_store.store_ptr = ir_val_inst(result_alloca_id)
        else_store.store_val = ir_val_inst(else_result.val_id)
        else_store.has_result = false
        else_block.instructions.push(else_store)
        g.builder.next_id = g.builder.next_id + 1

        if else_block.has_terminator == false {
            else_block.terminator = Box_new(term_br(join_label))
            else_block.has_terminator = true
        }
        g.completed_blocks.push(else_block)
    } else {
        -- No else branch: store a default value
        let mut default_store = ir_inst_new(g.builder.next_id, OP_STORE())
        default_store.store_ptr = ir_val_inst(result_alloca_id)
        default_store.store_val = ir_val_int(0)
        default_store.has_result = false
        else_block.instructions.push(default_store)
        g.builder.next_id = g.builder.next_id + 1

        else_block.terminator = Box_new(term_br(join_label))
        else_block.has_terminator = true
        g.completed_blocks.push(else_block)
    }

    -- Create join block and load the result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = if_result_type
    load_inst.result_type = if_result_type
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- MATCH EXPRESSION (returns a value)
-- ============================================================

fn generate_match_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate scrutinee
    let scrut_result = generate_expr(g, b, *expr.scrutinee)
    g = scrut_result.gen
    b = scrut_result.block
    let scrut_id = scrut_result.val_id

    -- Determine the scrutinee type to route to specialized match
    let scrut_expr = *expr.scrutinee

    -- Check if scrutinee is a known enum variable
    if scrut_expr.kind == EXPR_IDENTIFIER() {
        let scrut_name = scrut_expr.name
        if g.var_enum_types.contains(scrut_name) {
            let enum_type = g.var_enum_types.get(scrut_name)
            return generate_enum_match(g, b, expr, scrut_id, enum_type)
        }
    }

    -- Check match arms for enum variant patterns to detect enum matching
    if expr.match_arms.len() > 0 {
        let first_arm = expr.match_arms[0]
        let first_pat = first_arm.pattern

        -- Check for bare Ok/Err patterns (Result type)
        if first_pat.kind == PAT_ENUM_VARIANT() {
            let variant = first_pat.variant
            if variant == "Ok" or variant == "Err" {
                -- Determine the Result type for correct field types
                let mut result_type_name = ""
                if scrut_expr.kind == EXPR_IDENTIFIER() {
                    if g.variable_types.contains(scrut_expr.name) {
                        result_type_name = g.variable_types.get(scrut_expr.name)
                    }
                }
                return generate_result_match(g, b, expr, scrut_id, result_type_name)
            }
            if variant == "Some" or variant == "None" {
                let mut option_type_name = ""
                if scrut_expr.kind == EXPR_IDENTIFIER() {
                    if g.variable_types.contains(scrut_expr.name) {
                        option_type_name = g.variable_types.get(scrut_expr.name)
                    }
                }
                return generate_option_match(g, b, expr, scrut_id, option_type_name)
            }
            -- General enum variant pattern with known type
            if len(first_pat.enum_type) > 0 {
                return generate_enum_match(g, b, expr, scrut_id, first_pat.enum_type)
            }
        }
    }

    -- Default: literal/identifier matching with cascading if-else
    return generate_literal_match(g, b, expr, scrut_id)
}

-- ============================================================
-- MATCH RESULT TYPE INFERENCE
-- ============================================================

fn infer_match_result_type(gen: IRGenerator, arms: List[MatchArm]) -> string {
    -- Check the first arm's body to infer result type
    if arms.len() == 0 {
        return "i64"
    }
    let arm = arms[0]
    if arm.is_expr_body {
        return infer_expr_type(gen, *arm.body_expr)
    }
    return "i64"
}

-- ============================================================
-- LITERAL MATCH (int, string, bool, wildcard)
-- ============================================================

fn generate_literal_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Infer the match result type from arm bodies
    let match_type = infer_match_result_type(g, expr.match_arms)

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = match_type
    result_alloca.result_type = "ptr(" + match_type + ")"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    let join_label = gen_next_label(g, "match_end")
    g = gen_advance_label(g)

    -- Process each arm as cascading if-else
    let mut arm_idx = 0
    while arm_idx < expr.match_arms.len() {
        let arm = expr.match_arms[arm_idx]
        let pat = arm.pattern

        let arm_label = gen_next_label(g, "match_arm")
        g = gen_advance_label(g)
        let next_label = gen_next_label(g, "match_next")
        g = gen_advance_label(g)

        if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            -- Bind identifier if needed
            if pat.kind == PAT_IDENTIFIER() {
                -- Bind the scrutinee value to the pattern name
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = "i64"
                bind_alloca.result_type = "ptr(i64)"
                bind_alloca.has_result = true
                b.instructions.push(bind_alloca)
                g.builder.next_id = g.builder.next_id + 1

                let mut bind_store = ir_inst_new(g.builder.next_id, OP_STORE())
                bind_store.store_ptr = ir_val_inst(bind_alloca_id)
                bind_store.store_val = ir_val_inst(scrut_id)
                bind_store.has_result = false
                b.instructions.push(bind_store)
                g.builder.next_id = g.builder.next_id + 1

                g.variable_map.insert(pat.name, bind_alloca_id)
                g.variable_types.insert(pat.name, "i64")
            }

            -- If there is a guard condition, evaluate it and branch
            if arm.has_guard {
                let guard_result = generate_expr(g, b, *arm.guard)
                g = guard_result.gen
                b = guard_result.block

                -- Branch: if guard is true, execute arm body; else go to next arm
                b.terminator = Box_new(term_br_cond(ir_val_inst(guard_result.val_id), arm_label, next_label))
                b.has_terminator = true
                g.completed_blocks.push(b)

                -- Generate arm body in arm_block
                let mut arm_block = ir_block_new(arm_label)
                if arm.is_expr_body {
                    let body_result = generate_expr(g, arm_block, *arm.body_expr)
                    g = body_result.gen
                    arm_block = body_result.block

                    let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                    store_result.store_ptr = ir_val_inst(result_alloca_id)
                    store_result.store_val = ir_val_inst(body_result.val_id)
                    store_result.has_result = false
                    arm_block.instructions.push(store_result)
                    g.builder.next_id = g.builder.next_id + 1
                } else {
                    let body_gen = generate_block_stmts(g, arm_block, arm.body)
                    g = body_gen.gen
                    arm_block = body_gen.block
                }

                if arm_block.has_terminator == false {
                    arm_block.terminator = Box_new(term_br(join_label))
                    arm_block.has_terminator = true
                }
                g.completed_blocks.push(arm_block)

                -- Continue to next arm (set up next block)
                b = ir_block_new(next_label)
                arm_idx = arm_idx + 1
            } else {
                -- No guard: always matches (catch-all)
                -- Generate arm body directly in current block
                if arm.is_expr_body {
                    let body_result = generate_expr(g, b, *arm.body_expr)
                    g = body_result.gen
                    b = body_result.block

                    let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                    store_result.store_ptr = ir_val_inst(result_alloca_id)
                    store_result.store_val = ir_val_inst(body_result.val_id)
                    store_result.has_result = false
                    b.instructions.push(store_result)
                    g.builder.next_id = g.builder.next_id + 1
                } else {
                    let body_gen = generate_block_stmts(g, b, arm.body)
                    g = body_gen.gen
                    b = body_gen.block
                }

                if b.has_terminator == false {
                    b.terminator = Box_new(term_br(join_label))
                    b.has_terminator = true
                }
                g.completed_blocks.push(b)

                -- Wildcard/identifier without guard is the catch-all, break out of loop
                arm_idx = expr.match_arms.len()
            }
        } else if pat.kind == PAT_LITERAL() {
            -- Generate comparison of scrutinee with literal
            let lit_expr = *pat.literal_expr
            let lit_result = generate_expr(g, b, lit_expr)
            g = lit_result.gen
            b = lit_result.block

            -- Compare scrutinee with literal value
            let cmp_id = g.builder.next_id
            let mut cmp_inst = ir_inst_new(cmp_id, OP_EQ())
            cmp_inst.lhs = ir_val_inst(scrut_id)
            cmp_inst.rhs = ir_val_inst(lit_result.val_id)
            cmp_inst.result_type = "bool"
            cmp_inst.has_result = true
            b.instructions.push(cmp_inst)
            g.builder.next_id = g.builder.next_id + 1

            -- Check guard if present
            if arm.has_guard {
                let guard_result = generate_expr(g, b, *arm.guard)
                g = guard_result.gen
                b = guard_result.block

                -- AND the pattern match with the guard condition
                let and_id = g.builder.next_id
                let mut and_inst = ir_inst_new(and_id, OP_LOGICAL_AND())
                and_inst.lhs = ir_val_inst(cmp_id)
                and_inst.rhs = ir_val_inst(guard_result.val_id)
                and_inst.result_type = "bool"
                and_inst.has_result = true
                b.instructions.push(and_inst)
                g.builder.next_id = g.builder.next_id + 1

                b.terminator = Box_new(term_br_cond(ir_val_inst(and_id), arm_label, next_label))
                b.has_terminator = true
            } else {
                b.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), arm_label, next_label))
                b.has_terminator = true
            }
            g.completed_blocks.push(b)

            -- Generate arm body in arm_block
            let mut arm_block = ir_block_new(arm_label)
            if arm.is_expr_body {
                let body_result = generate_expr(g, arm_block, *arm.body_expr)
                g = body_result.gen
                arm_block = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                arm_block.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, arm_block, arm.body)
                g = body_gen.gen
                arm_block = body_gen.block
            }

            if arm_block.has_terminator == false {
                arm_block.terminator = Box_new(term_br(join_label))
                arm_block.has_terminator = true
            }
            g.completed_blocks.push(arm_block)

            -- Continue with next_label as the new current block
            b = ir_block_new(next_label)
            arm_idx = arm_idx + 1
        } else {
            -- Unsupported pattern kind in literal match, skip
            arm_idx = arm_idx + 1
        }
    }

    -- If we fell through all arms without a wildcard, branch to join
    if b.has_terminator == false {
        b.terminator = Box_new(term_br(join_label))
        b.has_terminator = true
    }
    g.completed_blocks.push(b)

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = match_type
    load_inst.result_type = match_type
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- ENUM MATCH
-- ============================================================

fn generate_enum_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, enum_type: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Get enum definition
    if g.enum_defs.contains(enum_type) == false {
        return generate_literal_match(g, b, expr, scrut_id)
    }
    let enum_info = g.enum_defs.get(enum_type)

    -- Infer match result type from arm bodies
    let ematch_type = infer_match_result_type(g, expr.match_arms)

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = ematch_type
    result_alloca.result_type = "ptr(" + ematch_type + ")"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Extract tag from scrutinee (field 0)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(scrut_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    let join_label = gen_next_label(g, "ematch_end")
    g = gen_advance_label(g)

    -- Process each arm
    let mut arm_idx = 0
    while arm_idx < expr.match_arms.len() {
        let arm = expr.match_arms[arm_idx]
        let pat = arm.pattern

        let arm_label = gen_next_label(g, "ematch_arm")
        g = gen_advance_label(g)
        let next_label = gen_next_label(g, "ematch_next")
        g = gen_advance_label(g)

        if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            -- Catch-all: bind if identifier
            if pat.kind == PAT_IDENTIFIER() {
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = "i64"
                bind_alloca.result_type = "ptr(i64)"
                bind_alloca.has_result = true
                b.instructions.push(bind_alloca)
                g.builder.next_id = g.builder.next_id + 1

                let mut bind_store = ir_inst_new(g.builder.next_id, OP_STORE())
                bind_store.store_ptr = ir_val_inst(bind_alloca_id)
                bind_store.store_val = ir_val_inst(scrut_id)
                bind_store.has_result = false
                b.instructions.push(bind_store)
                g.builder.next_id = g.builder.next_id + 1

                g.variable_map.insert(pat.name, bind_alloca_id)
                g.variable_types.insert(pat.name, "i64")
            }

            -- Generate arm body in current block
            if arm.is_expr_body {
                let body_result = generate_expr(g, b, *arm.body_expr)
                g = body_result.gen
                b = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                b.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, b, arm.body)
                g = body_gen.gen
                b = body_gen.block
            }

            if b.has_terminator == false {
                b.terminator = Box_new(term_br(join_label))
                b.has_terminator = true
            }
            g.completed_blocks.push(b)
            arm_idx = expr.match_arms.len()
        } else if pat.kind == PAT_ENUM_VARIANT() {
            -- Find variant tag value from enum definition
            let variant_name = pat.variant
            let mut variant_tag = -1
            let mut vi = 0
            while vi < enum_info.variant_names.len() {
                if enum_info.variant_names[vi] == variant_name {
                    variant_tag = enum_info.variant_tags[vi]
                }
                vi = vi + 1
            }

            -- Compare tag with variant tag
            let cmp_id = g.builder.next_id
            let mut cmp_inst = ir_inst_new(cmp_id, OP_EQ())
            cmp_inst.lhs = ir_val_inst(tag_id)
            cmp_inst.rhs = ir_val_int(variant_tag)
            cmp_inst.result_type = "bool"
            cmp_inst.has_result = true
            b.instructions.push(cmp_inst)
            g.builder.next_id = g.builder.next_id + 1

            b.terminator = Box_new(term_br_cond(ir_val_inst(cmp_id), arm_label, next_label))
            b.has_terminator = true
            g.completed_blocks.push(b)

            -- Generate arm body in arm_block
            let mut arm_block = ir_block_new(arm_label)

            -- Bind payload variables if present
            if pat.bindings.len() > 0 {
                -- Compute field offset: skip tag (1) + count prior payload variants
                let mut payload_field_offset = 1
                let mut pvi2 = 0
                while pvi2 < enum_info.variant_names.len() {
                    if enum_info.variant_names[pvi2] == variant_name {
                        pvi2 = enum_info.variant_names.len()
                    } else {
                        if enum_info.variant_has_payload[pvi2] {
                            payload_field_offset = payload_field_offset + 1
                        }
                        pvi2 = pvi2 + 1
                    }
                }

                -- Extract payload from scrutinee at computed field offset
                let payload_id = g.builder.next_id
                let mut payload_inst = ir_inst_new(payload_id, OP_EXTRACT_FIELD())
                payload_inst.field_base = ir_val_inst(scrut_id)
                payload_inst.field_index = payload_field_offset
                payload_inst.field_type = "i64"
                payload_inst.result_type = "i64"
                payload_inst.has_result = true
                arm_block.instructions.push(payload_inst)
                g.builder.next_id = g.builder.next_id + 1

                -- Bind first payload to the first binding name
                let bind_name = pat.bindings[0]
                let bind_alloca_id = g.builder.next_id
                let mut bind_alloca = ir_inst_new(bind_alloca_id, OP_ALLOCA())
                bind_alloca.alloc_type = "i64"
                bind_alloca.result_type = "ptr(i64)"
                bind_alloca.has_result = true
                arm_block.instructions.push(bind_alloca)
                g.builder.next_id = g.builder.next_id + 1

                let mut bind_store = ir_inst_new(g.builder.next_id, OP_STORE())
                bind_store.store_ptr = ir_val_inst(bind_alloca_id)
                bind_store.store_val = ir_val_inst(payload_id)
                bind_store.has_result = false
                arm_block.instructions.push(bind_store)
                g.builder.next_id = g.builder.next_id + 1

                g.variable_map.insert(bind_name, bind_alloca_id)
                g.variable_types.insert(bind_name, "i64")
            }

            if arm.is_expr_body {
                let body_result = generate_expr(g, arm_block, *arm.body_expr)
                g = body_result.gen
                arm_block = body_result.block

                let mut store_result = ir_inst_new(g.builder.next_id, OP_STORE())
                store_result.store_ptr = ir_val_inst(result_alloca_id)
                store_result.store_val = ir_val_inst(body_result.val_id)
                store_result.has_result = false
                arm_block.instructions.push(store_result)
                g.builder.next_id = g.builder.next_id + 1
            } else {
                let body_gen = generate_block_stmts(g, arm_block, arm.body)
                g = body_gen.gen
                arm_block = body_gen.block
            }

            if arm_block.has_terminator == false {
                arm_block.terminator = Box_new(term_br(join_label))
                arm_block.has_terminator = true
            }
            g.completed_blocks.push(arm_block)

            -- Move to next arm's block
            b = ir_block_new(next_label)
            arm_idx = arm_idx + 1
        } else {
            arm_idx = arm_idx + 1
        }
    }

    -- If we fell through all arms, branch to join
    if b.has_terminator == false {
        b.terminator = Box_new(term_br(join_label))
        b.has_terminator = true
    }
    g.completed_blocks.push(b)

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = ematch_type
    load_inst.result_type = ematch_type
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- RESULT MATCH (Ok/Err patterns)
-- ============================================================

fn generate_result_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, result_type_name: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Parse Result[T, E] to get ok_type and err_type
    let mut ok_field_type = "i64"
    let mut err_field_type = "i64"
    if starts_with(result_type_name, "Result[") {
        let inner = substr(result_type_name, 7, len(result_type_name) - 8)
        let mut comma_pos = 0 - 1
        let mut depth = 0
        let mut ci = 0
        while ci < len(inner) {
            let ch = char_at(inner, ci)
            if ch == "[" { depth = depth + 1 }
            if ch == "]" { depth = depth - 1 }
            if ch == "," and depth == 0 {
                comma_pos = ci
                ci = len(inner)
            }
            ci = ci + 1
        }
        if comma_pos >= 0 {
            ok_field_type = map_type_name(substr(inner, 0, comma_pos))
            let mut err_start = comma_pos + 1
            if err_start < len(inner) and char_at(inner, err_start) == " " {
                err_start = err_start + 1
            }
            err_field_type = map_type_name(substr(inner, err_start, len(inner) - err_start))
        }
    }

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Extract tag from scrutinee (field 0: 0=Ok, 1=Err)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(scrut_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Compare tag to 0 (Ok)
    let is_ok_id = g.builder.next_id
    let mut is_ok_cmp = ir_inst_new(is_ok_id, OP_EQ())
    is_ok_cmp.lhs = ir_val_inst(tag_id)
    is_ok_cmp.rhs = ir_val_int(0)
    is_ok_cmp.result_type = "bool"
    is_ok_cmp.has_result = true
    b.instructions.push(is_ok_cmp)
    g.builder.next_id = g.builder.next_id + 1

    let ok_label = gen_next_label(g, "result_ok")
    g = gen_advance_label(g)
    let err_label = gen_next_label(g, "result_err")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "result_end")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(is_ok_id), ok_label, err_label))
    b.has_terminator = true
    g.completed_blocks.push(b)

    -- Find Ok and Err arms
    let mut ok_arm_idx = -1
    let mut err_arm_idx = -1
    let mut wildcard_arm_idx = -1
    let mut ai = 0
    while ai < expr.match_arms.len() {
        let arm = expr.match_arms[ai]
        let pat = arm.pattern
        if pat.kind == PAT_ENUM_VARIANT() {
            if pat.variant == "Ok" {
                ok_arm_idx = ai
            } else if pat.variant == "Err" {
                err_arm_idx = ai
            }
        } else if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            wildcard_arm_idx = ai
        }
        ai = ai + 1
    }

    -- Generate Ok arm
    let mut ok_block = ir_block_new(ok_label)
    if ok_arm_idx >= 0 {
        let ok_arm = expr.match_arms[ok_arm_idx]
        let ok_pat = ok_arm.pattern

        -- Extract Ok payload (field 1) and bind
        if ok_pat.bindings.len() > 0 {
            let ok_val_id = g.builder.next_id
            let mut ok_extract = ir_inst_new(ok_val_id, OP_EXTRACT_FIELD())
            ok_extract.field_base = ir_val_inst(scrut_id)
            ok_extract.field_index = 1
            ok_extract.field_type = ok_field_type
            ok_extract.result_type = ok_field_type
            ok_extract.has_result = true
            ok_block.instructions.push(ok_extract)
            g.builder.next_id = g.builder.next_id + 1

            let bind_name = ok_pat.bindings[0]
            let ok_bind_id = g.builder.next_id
            let mut ok_bind = ir_inst_new(ok_bind_id, OP_ALLOCA())
            ok_bind.alloc_type = ok_field_type
            ok_bind.result_type = "ptr(" + ok_field_type + ")"
            ok_bind.has_result = true
            ok_block.instructions.push(ok_bind)
            g.builder.next_id = g.builder.next_id + 1

            let mut ok_store = ir_inst_new(g.builder.next_id, OP_STORE())
            ok_store.store_ptr = ir_val_inst(ok_bind_id)
            ok_store.store_val = ir_val_inst(ok_val_id)
            ok_store.has_result = false
            ok_block.instructions.push(ok_store)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(bind_name, ok_bind_id)
            g.variable_types.insert(bind_name, ok_field_type)
        }

        if ok_arm.is_expr_body {
            let body_result = generate_expr(g, ok_block, *ok_arm.body_expr)
            g = body_result.gen
            ok_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            ok_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, ok_block, ok_arm.body)
            g = body_gen.gen
            ok_block = body_gen.block
        }
    }
    if ok_block.has_terminator == false {
        ok_block.terminator = Box_new(term_br(join_label))
        ok_block.has_terminator = true
    }
    g.completed_blocks.push(ok_block)

    -- Generate Err arm
    let mut err_block = ir_block_new(err_label)
    if err_arm_idx >= 0 {
        let err_arm = expr.match_arms[err_arm_idx]
        let err_pat = err_arm.pattern

        -- Extract Err payload (field 2 for error value) and bind
        if err_pat.bindings.len() > 0 {
            let err_val_id = g.builder.next_id
            let mut err_extract = ir_inst_new(err_val_id, OP_EXTRACT_FIELD())
            err_extract.field_base = ir_val_inst(scrut_id)
            err_extract.field_index = 2
            err_extract.field_type = err_field_type
            err_extract.result_type = err_field_type
            err_extract.has_result = true
            err_block.instructions.push(err_extract)
            g.builder.next_id = g.builder.next_id + 1

            let bind_name = err_pat.bindings[0]
            let err_bind_id = g.builder.next_id
            let mut err_bind = ir_inst_new(err_bind_id, OP_ALLOCA())
            err_bind.alloc_type = err_field_type
            err_bind.result_type = "ptr(" + err_field_type + ")"
            err_bind.has_result = true
            err_block.instructions.push(err_bind)
            g.builder.next_id = g.builder.next_id + 1

            let mut err_store = ir_inst_new(g.builder.next_id, OP_STORE())
            err_store.store_ptr = ir_val_inst(err_bind_id)
            err_store.store_val = ir_val_inst(err_val_id)
            err_store.has_result = false
            err_block.instructions.push(err_store)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(bind_name, err_bind_id)
            g.variable_types.insert(bind_name, err_field_type)
        }

        if err_arm.is_expr_body {
            let body_result = generate_expr(g, err_block, *err_arm.body_expr)
            g = body_result.gen
            err_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            err_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, err_block, err_arm.body)
            g = body_gen.gen
            err_block = body_gen.block
        }
    } else if wildcard_arm_idx >= 0 {
        -- Use wildcard arm for Err path
        let wild_arm = expr.match_arms[wildcard_arm_idx]
        if wild_arm.is_expr_body {
            let body_result = generate_expr(g, err_block, *wild_arm.body_expr)
            g = body_result.gen
            err_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            err_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, err_block, wild_arm.body)
            g = body_gen.gen
            err_block = body_gen.block
        }
    }
    if err_block.has_terminator == false {
        err_block.terminator = Box_new(term_br(join_label))
        err_block.has_terminator = true
    }
    g.completed_blocks.push(err_block)

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- OPTION MATCH (Some/None patterns)
-- ============================================================

fn generate_option_match(gen: IRGenerator, block: IRBasicBlock, expr: Expr, scrut_id: int, option_type_name: string) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Parse Option[T] to get the inner type
    let mut some_field_type = "i64"
    if starts_with(option_type_name, "Option[") {
        let inner = substr(option_type_name, 7, len(option_type_name) - 8)
        some_field_type = map_type_name(inner)
    }

    -- Allocate result slot
    let result_alloca_id = g.builder.next_id
    let mut result_alloca = ir_inst_new(result_alloca_id, OP_ALLOCA())
    result_alloca.alloc_type = "i64"
    result_alloca.result_type = "ptr(i64)"
    result_alloca.has_result = true
    b.instructions.push(result_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Extract tag (field 0: 1=Some/has_value, 0=None)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(scrut_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Compare tag to 1 (Some = has_value = 1)
    let is_some_id = g.builder.next_id
    let mut is_some_cmp = ir_inst_new(is_some_id, OP_EQ())
    is_some_cmp.lhs = ir_val_inst(tag_id)
    is_some_cmp.rhs = ir_val_int(1)
    is_some_cmp.result_type = "bool"
    is_some_cmp.has_result = true
    b.instructions.push(is_some_cmp)
    g.builder.next_id = g.builder.next_id + 1

    let some_label = gen_next_label(g, "option_some")
    g = gen_advance_label(g)
    let none_label = gen_next_label(g, "option_none")
    g = gen_advance_label(g)
    let join_label = gen_next_label(g, "option_end")
    g = gen_advance_label(g)

    b.terminator = Box_new(term_br_cond(ir_val_inst(is_some_id), some_label, none_label))
    b.has_terminator = true
    g.completed_blocks.push(b)

    -- Find Some and None arms
    let mut some_arm_idx = -1
    let mut none_arm_idx = -1
    let mut wildcard_arm_idx = -1
    let mut ai = 0
    while ai < expr.match_arms.len() {
        let arm = expr.match_arms[ai]
        let pat = arm.pattern
        if pat.kind == PAT_ENUM_VARIANT() {
            if pat.variant == "Some" {
                some_arm_idx = ai
            } else if pat.variant == "None" {
                none_arm_idx = ai
            }
        } else if pat.kind == PAT_WILDCARD() or pat.kind == PAT_IDENTIFIER() {
            wildcard_arm_idx = ai
        }
        ai = ai + 1
    }

    -- Generate Some arm
    let mut some_block = ir_block_new(some_label)
    if some_arm_idx >= 0 {
        let some_arm = expr.match_arms[some_arm_idx]
        let some_pat = some_arm.pattern

        -- Extract Some payload (field 1) and bind
        if some_pat.bindings.len() > 0 {
            let some_val_id = g.builder.next_id
            let mut some_extract = ir_inst_new(some_val_id, OP_EXTRACT_FIELD())
            some_extract.field_base = ir_val_inst(scrut_id)
            some_extract.field_index = 1
            some_extract.field_type = some_field_type
            some_extract.result_type = some_field_type
            some_extract.has_result = true
            some_block.instructions.push(some_extract)
            g.builder.next_id = g.builder.next_id + 1

            let bind_name = some_pat.bindings[0]
            let some_bind_id = g.builder.next_id
            let mut some_bind = ir_inst_new(some_bind_id, OP_ALLOCA())
            some_bind.alloc_type = some_field_type
            some_bind.result_type = "ptr(" + some_field_type + ")"
            some_bind.has_result = true
            some_block.instructions.push(some_bind)
            g.builder.next_id = g.builder.next_id + 1

            let mut some_store = ir_inst_new(g.builder.next_id, OP_STORE())
            some_store.store_ptr = ir_val_inst(some_bind_id)
            some_store.store_val = ir_val_inst(some_val_id)
            some_store.has_result = false
            some_block.instructions.push(some_store)
            g.builder.next_id = g.builder.next_id + 1

            g.variable_map.insert(bind_name, some_bind_id)
            g.variable_types.insert(bind_name, some_field_type)
        }

        if some_arm.is_expr_body {
            let body_result = generate_expr(g, some_block, *some_arm.body_expr)
            g = body_result.gen
            some_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            some_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, some_block, some_arm.body)
            g = body_gen.gen
            some_block = body_gen.block
        }
    }
    if some_block.has_terminator == false {
        some_block.terminator = Box_new(term_br(join_label))
        some_block.has_terminator = true
    }
    g.completed_blocks.push(some_block)

    -- Generate None arm
    let mut none_block = ir_block_new(none_label)
    if none_arm_idx >= 0 {
        let none_arm = expr.match_arms[none_arm_idx]

        if none_arm.is_expr_body {
            let body_result = generate_expr(g, none_block, *none_arm.body_expr)
            g = body_result.gen
            none_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            none_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, none_block, none_arm.body)
            g = body_gen.gen
            none_block = body_gen.block
        }
    } else if wildcard_arm_idx >= 0 {
        let wild_arm = expr.match_arms[wildcard_arm_idx]
        if wild_arm.is_expr_body {
            let body_result = generate_expr(g, none_block, *wild_arm.body_expr)
            g = body_result.gen
            none_block = body_result.block

            let mut store_r = ir_inst_new(g.builder.next_id, OP_STORE())
            store_r.store_ptr = ir_val_inst(result_alloca_id)
            store_r.store_val = ir_val_inst(body_result.val_id)
            store_r.has_result = false
            none_block.instructions.push(store_r)
            g.builder.next_id = g.builder.next_id + 1
        } else {
            let body_gen = generate_block_stmts(g, none_block, wild_arm.body)
            g = body_gen.gen
            none_block = body_gen.block
        }
    }
    if none_block.has_terminator == false {
        none_block.terminator = Box_new(term_br(join_label))
        none_block.has_terminator = true
    }
    g.completed_blocks.push(none_block)

    -- Create join block and load result
    let mut join_block = ir_block_new(join_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(result_alloca_id)
    load_inst.load_type = "i64"
    load_inst.result_type = "i64"
    load_inst.has_result = true
    join_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, join_block, load_id)
}

-- ============================================================
-- BLOCK EXPRESSION
-- ============================================================

fn generate_block_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate all statements in the block
    let stmts_result = generate_block_stmts(g, b, expr.stmts)
    g = stmts_result.gen
    b = stmts_result.block

    -- If the block already has a terminator (e.g., from a return statement), just return
    if b.has_terminator {
        return expr_result(g, b, g.builder.next_id - 1)
    }

    -- If the block has a result expression, generate it and return its value
    if expr.has_result {
        let result = generate_expr(g, b, *expr.result)
        return result
    }

    -- No result expression: return undef-like value (alloca of 0)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_ALLOCA())
    inst.alloc_type = "i64"
    inst.result_type = "ptr(i64)"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = id + 1

    let mut store = ir_inst_new(g.builder.next_id, OP_STORE())
    store.store_ptr = ir_val_inst(id)
    store.store_val = ir_val_int(0)
    store.has_result = false
    b.instructions.push(store)
    g.builder.next_id = g.builder.next_id + 1

    let load_id = g.builder.next_id
    let mut load = ir_inst_new(load_id, OP_LOAD())
    load.load_ptr = ir_val_inst(id)
    load.load_type = "i64"
    load.result_type = "i64"
    load.has_result = true
    b.instructions.push(load)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, load_id)
}

-- ============================================================
-- LAMBDA EXPRESSION
-- Creates a new IR function for the lambda and returns a
-- reference to it. The lambda is lifted to a module-level
-- function with a unique name.
-- ============================================================

fn generate_lambda_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate unique lambda name
    let lambda_name = gen_next_lambda_name(g)
    g = gen_advance_lambda(g)

    -- Determine return type
    let ret_type = if len(expr.lambda_ret_type) > 0 {
        map_type_name(expr.lambda_ret_type)
    } else {
        "i64"
    }

    -- Create a new IR function for the lambda
    let mut lambda_func = ir_function_new(lambda_name, ret_type)

    -- Add lambda parameters
    let mut pi = 0
    while pi < expr.lambda_params.len() {
        let lp = expr.lambda_params[pi]
        let ptype = if len(lp.type_name) > 0 {
            map_type_name(lp.type_name)
        } else {
            "i64"
        }
        lambda_func.params.push(ir_param_new(lp.name, ptype))
        pi = pi + 1
    }

    -- Collect captured variables from the current scope
    -- Build a pipe-delimited list of captured variable names
    let mut captures = ""
    let mut capture_names: List[string] = []
    let mut capture_types: List[string] = []
    let mut keys = g.variable_map.keys()
    let mut ki = 0
    while ki < keys.len() {
        let var_name = keys[ki]
        -- Skip lambda parameters (they are not captures)
        let mut is_param = false
        let mut pj = 0
        while pj < expr.lambda_params.len() {
            if expr.lambda_params[pj].name == var_name {
                is_param = true
            }
            pj = pj + 1
        }
        if is_param == false {
            capture_names.push(var_name)
            let cap_type = if g.variable_types.contains(var_name) {
                g.variable_types.get(var_name)
            } else {
                "i64"
            }
            capture_types.push(cap_type)
            lambda_func.params.push(ir_param_new(var_name, cap_type))

            if len(captures) > 0 {
                captures = captures + "|"
            }
            captures = captures + var_name
        }
        ki = ki + 1
    }

    -- Save current generator state
    let saved_variable_map = g.variable_map
    let saved_variable_types = g.variable_types
    let saved_return_type = g.current_return_type
    let saved_next_id = g.builder.next_id

    -- Set up new variable scope for the lambda body
    g.variable_map = Map_new()
    g.variable_types = Map_new()
    g.current_return_type = ret_type
    g.builder.next_id = 0

    -- Create entry block for the lambda
    let mut entry_block = ir_block_new("entry")

    -- Create allocas and stores for all parameters (lambda params + captures)
    let total_params = expr.lambda_params.len() + capture_names.len()
    let mut param_idx = 0

    -- Lambda parameters first
    while param_idx < expr.lambda_params.len() {
        let lp = expr.lambda_params[param_idx]
        let ptype = if len(lp.type_name) > 0 {
            map_type_name(lp.type_name)
        } else {
            "i64"
        }

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = ptype
        alloca_inst.result_type = "ptr(" + ptype + ")"
        alloca_inst.has_result = true
        entry_block.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_param(param_idx)
        store_inst.has_result = false
        entry_block.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        g.variable_map.insert(lp.name, alloca_id)
        g.variable_types.insert(lp.name, ptype)
        param_idx = param_idx + 1
    }

    -- Captured variables next
    let mut cap_idx = 0
    while cap_idx < capture_names.len() {
        let cap_name = capture_names[cap_idx]
        let cap_type = capture_types[cap_idx]
        let full_param_idx = expr.lambda_params.len() + cap_idx

        let alloca_id = g.builder.next_id
        let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
        alloca_inst.alloc_type = cap_type
        alloca_inst.result_type = "ptr(" + cap_type + ")"
        alloca_inst.has_result = true
        entry_block.instructions.push(alloca_inst)
        g.builder.next_id = g.builder.next_id + 1

        let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
        store_inst.store_ptr = ir_val_inst(alloca_id)
        store_inst.store_val = ir_val_param(full_param_idx)
        store_inst.has_result = false
        entry_block.instructions.push(store_inst)
        g.builder.next_id = g.builder.next_id + 1

        g.variable_map.insert(cap_name, alloca_id)
        g.variable_types.insert(cap_name, cap_type)
        cap_idx = cap_idx + 1
    }

    -- Generate the lambda body expression
    let body_result = generate_expr(g, entry_block, *expr.lambda_body)
    g = body_result.gen
    entry_block = body_result.block

    -- Add return terminator
    if entry_block.has_terminator == false {
        if ret_type == "void" {
            entry_block.terminator = Box_new(term_ret_void())
        } else {
            entry_block.terminator = Box_new(term_ret(ir_val_inst(body_result.val_id)))
        }
        entry_block.has_terminator = true
    }

    -- Add entry block to the lambda function and push to module
    lambda_func.blocks.push(entry_block)
    g.ir_mod.functions.push(lambda_func)

    -- Restore generator state
    g.variable_map = saved_variable_map
    g.variable_types = saved_variable_types
    g.current_return_type = saved_return_type
    g.builder.next_id = saved_next_id

    -- Record the lambda in fn_ptr_vars and lambda_captures
    g.fn_ptr_vars.insert(lambda_name, lambda_name)
    if len(captures) > 0 {
        g.lambda_captures.insert(lambda_name, captures)
    }

    -- Return a global reference to the lambda function
    let ref_id = g.builder.next_id
    let mut ref_inst = ir_inst_new(ref_id, OP_ALLOCA())
    ref_inst.alloc_type = "ptr"
    ref_inst.result_type = "ptr(ptr)"
    ref_inst.has_result = true
    b.instructions.push(ref_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_ref = ir_inst_new(g.builder.next_id, OP_STORE())
    store_ref.store_ptr = ir_val_inst(ref_id)
    store_ref.store_val = ir_val_global(lambda_name)
    store_ref.has_result = false
    b.instructions.push(store_ref)
    g.builder.next_id = g.builder.next_id + 1

    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(ref_id)
    load_inst.load_type = "ptr"
    load_inst.result_type = "ptr"
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, load_id)
}

-- ============================================================
-- PIPELINE OPERATOR
-- x |> f desugars to f(x)
-- x |> f(y) desugars to f(x, y)
-- ============================================================

fn generate_pipeline(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Generate the left side (the argument to prepend)
    let left_result = generate_expr(g, b, *expr.left)
    g = left_result.gen
    b = left_result.block

    let pipe_right = *expr.pipe_right

    -- Case 1: Right side is a function call with args -> prepend left as first arg
    if pipe_right.kind == EXPR_FUNCTION_CALL() {
        let callee = *pipe_right.callee
        let fn_name = if callee.kind == EXPR_IDENTIFIER() {
            callee.name
        } else {
            ""
        }

        -- Build argument list: left value first, then original args
        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(left_result.val_id))

        let mut i = 0
        while i < pipe_right.args.len() {
            let arg_result = generate_expr(g, b, pipe_right.args[i])
            g = arg_result.gen
            b = arg_result.block
            call_args.push(ir_val_inst(arg_result.val_id))
            i = i + 1
        }

        -- Determine return type
        let ret_type = if is_builtin_fn(fn_name) {
            builtin_return_type(fn_name)
        } else {
            "i64"
        }

        -- Map builtin name if needed
        let c_name = if is_builtin_fn(fn_name) {
            map_builtin_name(fn_name)
        } else {
            fn_name
        }

        let call_id = g.builder.next_id
        let mut call_inst = ir_inst_new(call_id, OP_CALL())
        call_inst.callee = c_name
        call_inst.call_args = call_args
        call_inst.result_type = ret_type
        call_inst.has_result = ret_type != "void"
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, call_id)
    }

    -- Case 2: Right side is an identifier -> call it with left as sole arg
    if pipe_right.kind == EXPR_IDENTIFIER() {
        let fn_name = pipe_right.name

        let mut call_args: List[IRValue] = []
        call_args.push(ir_val_inst(left_result.val_id))

        let ret_type = if is_builtin_fn(fn_name) {
            builtin_return_type(fn_name)
        } else {
            "i64"
        }

        let c_name = if is_builtin_fn(fn_name) {
            map_builtin_name(fn_name)
        } else {
            fn_name
        }

        let call_id = g.builder.next_id
        let mut call_inst = ir_inst_new(call_id, OP_CALL())
        call_inst.callee = c_name
        call_inst.call_args = call_args
        call_inst.result_type = ret_type
        call_inst.has_result = ret_type != "void"
        b.instructions.push(call_inst)
        g.builder.next_id = g.builder.next_id + 1

        return expr_result(g, b, call_id)
    }

    -- Case 3: Right side is a lambda variable -> call_ptr with left as arg
    if pipe_right.kind == EXPR_IDENTIFIER() {
        let fn_name = pipe_right.name
        if g.fn_ptr_vars.contains(fn_name) {
            let lambda_fn = g.fn_ptr_vars.get(fn_name)

            let mut call_args: List[IRValue] = []
            call_args.push(ir_val_inst(left_result.val_id))

            let call_id = g.builder.next_id
            let mut call_inst = ir_inst_new(call_id, OP_CALL())
            call_inst.callee = lambda_fn
            call_inst.call_args = call_args
            call_inst.result_type = "i64"
            call_inst.has_result = true
            b.instructions.push(call_inst)
            g.builder.next_id = g.builder.next_id + 1

            return expr_result(g, b, call_id)
        }
    }

    -- Fallback: just return the left value (pipeline target not recognized)
    return expr_result(g, b, left_result.val_id)
}

-- ============================================================
-- ERROR PROPAGATION (? operator)
-- expr? on Result[T, E] unwraps Ok or early-returns Err
-- expr? on Option[T] unwraps Some or early-returns None
-- ============================================================

fn generate_error_propagate(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Infer operand type to get correct Ok/Err field types
    let operand_type = infer_expr_type(g, *expr.operand)
    let mut ok_field_type = "i64"
    let mut err_field_type = "i64"
    if starts_with(operand_type, "Result[") {
        let inner = substr(operand_type, 7, len(operand_type) - 8)
        let mut comma_pos = 0 - 1
        let mut depth = 0
        let mut ci = 0
        while ci < len(inner) {
            let ch = char_at(inner, ci)
            if ch == "[" { depth = depth + 1 }
            if ch == "]" { depth = depth - 1 }
            if ch == "," and depth == 0 {
                comma_pos = ci
                ci = len(inner)
            }
            ci = ci + 1
        }
        if comma_pos >= 0 {
            ok_field_type = map_type_name(substr(inner, 0, comma_pos))
            let mut err_start = comma_pos + 1
            if err_start < len(inner) and char_at(inner, err_start) == " " {
                err_start = err_start + 1
            }
            err_field_type = map_type_name(substr(inner, err_start, len(inner) - err_start))
        }
    } else if starts_with(operand_type, "Option[") {
        let inner = substr(operand_type, 7, len(operand_type) - 8)
        ok_field_type = map_type_name(inner)
    }

    -- Generate the operand (the Result/Option expression)
    let operand_result = generate_expr(g, b, *expr.operand)
    g = operand_result.gen
    b = operand_result.block
    let operand_id = operand_result.val_id

    -- Extract tag from the result/option (field 0)
    let tag_id = g.builder.next_id
    let mut tag_inst = ir_inst_new(tag_id, OP_EXTRACT_FIELD())
    tag_inst.field_base = ir_val_inst(operand_id)
    tag_inst.field_index = 0
    tag_inst.field_type = "i32"
    tag_inst.result_type = "i32"
    tag_inst.has_result = true
    b.instructions.push(tag_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Compare tag: 0 = Ok/Some (success), nonzero = Err/None (failure)
    let is_ok_id = g.builder.next_id
    let mut is_ok_cmp = ir_inst_new(is_ok_id, OP_EQ())
    is_ok_cmp.lhs = ir_val_inst(tag_id)
    is_ok_cmp.rhs = ir_val_int(0)
    is_ok_cmp.result_type = "bool"
    is_ok_cmp.has_result = true
    b.instructions.push(is_ok_cmp)
    g.builder.next_id = g.builder.next_id + 1

    -- Create labels for ok, err, and continue paths
    let ok_label = gen_next_label(g, "prop_ok")
    g = gen_advance_label(g)
    let err_label = gen_next_label(g, "prop_err")
    g = gen_advance_label(g)
    let cont_label = gen_next_label(g, "prop_cont")
    g = gen_advance_label(g)

    -- Pre-allocate storage for the Ok value (must be in current block, before the branch)
    let ok_alloca_id = g.builder.next_id
    let mut ok_alloca = ir_inst_new(ok_alloca_id, OP_ALLOCA())
    ok_alloca.alloc_type = ok_field_type
    ok_alloca.result_type = "ptr(" + ok_field_type + ")"
    ok_alloca.has_result = true
    b.instructions.push(ok_alloca)
    g.builder.next_id = g.builder.next_id + 1

    -- Branch: if tag == 0 go to ok, else go to err
    b.terminator = Box_new(term_br_cond(ir_val_inst(is_ok_id), ok_label, err_label))
    b.has_terminator = true
    g.completed_blocks.push(b)
    -- Err path: extract error value and early-return it from the function
    let mut err_block = ir_block_new(err_label)

    -- Extract the error payload (field 2 for Result, or undef for Option)
    let err_val_id = g.builder.next_id
    let mut err_extract = ir_inst_new(err_val_id, OP_EXTRACT_FIELD())
    err_extract.field_base = ir_val_inst(operand_id)
    err_extract.field_index = 2
    err_extract.field_type = err_field_type
    err_extract.result_type = err_field_type
    err_extract.has_result = true
    err_block.instructions.push(err_extract)
    g.builder.next_id = g.builder.next_id + 1

    -- Return the operand directly (it already has the Err tag).
    err_block.terminator = Box_new(term_ret(ir_val_inst(operand_id)))
    err_block.has_terminator = true
    g.completed_blocks.push(err_block)
    -- Ok path: extract the Ok/Some value (field 1)
    let mut ok_block = ir_block_new(ok_label)
    let ok_val_id = g.builder.next_id
    let mut ok_extract = ir_inst_new(ok_val_id, OP_EXTRACT_FIELD())
    ok_extract.field_base = ir_val_inst(operand_id)
    ok_extract.field_index = 1
    ok_extract.field_type = ok_field_type
    ok_extract.result_type = ok_field_type
    ok_extract.has_result = true
    ok_block.instructions.push(ok_extract)
    g.builder.next_id = g.builder.next_id + 1

    -- Store ok value into the pre-allocated alloca
    let mut ok_store = ir_inst_new(g.builder.next_id, OP_STORE())
    ok_store.store_ptr = ir_val_inst(ok_alloca_id)
    ok_store.store_val = ir_val_inst(ok_val_id)
    ok_store.has_result = false
    ok_block.instructions.push(ok_store)
    g.builder.next_id = g.builder.next_id + 1

    ok_block.terminator = Box_new(term_br(cont_label))
    ok_block.has_terminator = true
    g.completed_blocks.push(ok_block)
    -- Continue block: load the ok value
    let mut cont_block = ir_block_new(cont_label)
    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(ok_alloca_id)
    load_inst.load_type = ok_field_type
    load_inst.result_type = ok_field_type
    load_inst.has_result = true
    cont_block.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, cont_block, load_id)
}

-- ============================================================
-- STRING INTERPOLATION
-- f"Hello {name}, you are {age} years old"
-- Alternating literal and expression parts concatenated.
-- ============================================================

fn generate_string_interp(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Start with an empty string
    let empty_id = g.builder.next_id
    let mut empty_inst = ir_inst_new(empty_id, OP_ALLOCA())
    empty_inst.alloc_type = "string"
    empty_inst.result_type = "ptr(string)"
    empty_inst.has_result = true
    b.instructions.push(empty_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut empty_store = ir_inst_new(g.builder.next_id, OP_STORE())
    empty_store.store_ptr = ir_val_inst(empty_id)
    empty_store.store_val = ir_val_string("")
    empty_store.has_result = false
    b.instructions.push(empty_store)
    g.builder.next_id = g.builder.next_id + 1

    let mut empty_load = ir_inst_new(g.builder.next_id, OP_LOAD())
    empty_load.load_ptr = ir_val_inst(empty_id)
    empty_load.load_type = "string"
    empty_load.result_type = "string"
    empty_load.has_result = true
    b.instructions.push(empty_load)
    let mut running_id = g.builder.next_id
    g.builder.next_id = g.builder.next_id + 1

    -- Process each part
    let mut i = 0
    while i < expr.interp_parts.len() {
        let part = expr.interp_parts[i]
        let is_literal = if i < expr.interp_is_literal.len() {
            expr.interp_is_literal[i]
        } else {
            false
        }

        if is_literal {
            -- Literal string part: generate directly
            let lit_result = generate_expr(g, b, part)
            g = lit_result.gen
            b = lit_result.block

            -- Concatenate with running string
            let concat_id = g.builder.next_id
            let mut concat_inst = ir_inst_new(concat_id, OP_STRING_CONCAT())
            concat_inst.lhs = ir_val_inst(running_id)
            concat_inst.rhs = ir_val_inst(lit_result.val_id)
            concat_inst.result_type = "string"
            concat_inst.has_result = true
            b.instructions.push(concat_inst)
            g.builder.next_id = g.builder.next_id + 1
            running_id = concat_id
        } else {
            -- Expression part: generate the expression
            let expr_result_val = generate_expr(g, b, part)
            g = expr_result_val.gen
            b = expr_result_val.block
            let part_val_id = expr_result_val.val_id

            -- Determine the expression type to decide if conversion is needed
            -- Check if the part is an identifier with a known type
            let part_type = infer_part_type(g, part)

            let mut str_val_id = part_val_id
            if part_type == "string" {
                -- Already a string, use directly
                str_val_id = part_val_id
            } else if part_type == "i64" or part_type == "int" {
                -- Convert int to string via int_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_int_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            } else if part_type == "f64" or part_type == "float" {
                -- Convert float to string via float_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_float_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            } else if part_type == "bool" {
                -- Convert bool to string via bool_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_bool_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            } else {
                -- Unknown type: try int_to_string as default
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_int_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(part_val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                str_val_id = conv_id
            }

            -- Concatenate with running string
            let concat_id = g.builder.next_id
            let mut concat_inst = ir_inst_new(concat_id, OP_STRING_CONCAT())
            concat_inst.lhs = ir_val_inst(running_id)
            concat_inst.rhs = ir_val_inst(str_val_id)
            concat_inst.result_type = "string"
            concat_inst.has_result = true
            b.instructions.push(concat_inst)
            g.builder.next_id = g.builder.next_id + 1
            running_id = concat_id
        }

        i = i + 1
    }

    return expr_result(g, b, running_id)
}

-- Infer the type of a string interpolation part expression.
-- Checks identifiers against variable_types, and literals by kind.
fn infer_part_type(gen: IRGenerator, part: Expr) -> string {
    if part.kind == EXPR_LITERAL_STRING() {
        return "string"
    }
    if part.kind == EXPR_LITERAL_INT() {
        return "i64"
    }
    if part.kind == EXPR_LITERAL_FLOAT() {
        return "f64"
    }
    if part.kind == EXPR_LITERAL_BOOL() {
        return "bool"
    }
    if part.kind == EXPR_IDENTIFIER() {
        if gen.variable_types.contains(part.name) {
            return gen.variable_types.get(part.name)
        }
    }
    if part.kind == EXPR_FUNCTION_CALL() {
        let callee = *part.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let fn_name = callee.name
            if is_builtin_fn(fn_name) {
                return builtin_return_type(fn_name)
            }
        }
    }
    -- Default: assume int (most common)
    return "i64"
}

-- ============================================================
-- COMPTIME EXPRESSION
-- Evaluates the inner expression at compile time and emits
-- the result as a constant value.
-- ============================================================

fn generate_comptime_expr(gen: IRGenerator, block: IRBasicBlock, expr: Expr) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Evaluate the inner expression at compile time
    let ct_val = eval_comptime_expr(g, *expr.operand)

    -- Determine the type of the compile-time value
    let ct_type = comptime_value_type(ct_val)

    -- Emit an alloca/store/load sequence for the compile-time constant
    let alloca_id = g.builder.next_id
    let mut alloca_inst = ir_inst_new(alloca_id, OP_ALLOCA())
    alloca_inst.alloc_type = ct_type
    alloca_inst.result_type = "ptr(" + ct_type + ")"
    alloca_inst.has_result = true
    b.instructions.push(alloca_inst)
    g.builder.next_id = g.builder.next_id + 1

    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
    store_inst.store_ptr = ir_val_inst(alloca_id)
    store_inst.store_val = ct_val
    store_inst.has_result = false
    b.instructions.push(store_inst)
    g.builder.next_id = g.builder.next_id + 1

    let load_id = g.builder.next_id
    let mut load_inst = ir_inst_new(load_id, OP_LOAD())
    load_inst.load_ptr = ir_val_inst(alloca_id)
    load_inst.load_type = ct_type
    load_inst.result_type = ct_type
    load_inst.has_result = true
    b.instructions.push(load_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, load_id)
}


-- ============================================================
-- SIMD Builtin IR Generation
-- Handles simd_splat_*, simd_set_*, simd_add/sub/mul/div,
-- and simd_extract intrinsic function calls.
-- Port of stage3/src/ir_gen.zig SIMD handling
-- ============================================================

-- ============================================================
-- SIMD BUILTIN DETECTION
-- ============================================================

-- Returns true if the function name is a SIMD builtin intrinsic
fn is_simd_builtin(name: string) -> bool {
    if starts_with(name, "simd_splat_") { return true }
    if starts_with(name, "simd_set_") { return true }
    if name == "simd_add" { return true }
    if name == "simd_sub" { return true }
    if name == "simd_mul" { return true }
    if name == "simd_div" { return true }
    if name == "simd_extract" { return true }
    return false
}

-- ============================================================
-- SIMD TYPE PARSING
-- ============================================================

-- Extracts the SIMD vector type name from a simd_splat_* or simd_set_* name.
-- "simd_splat_f32x4" -> "f32x4"
-- "simd_set_i32x8"   -> "i32x8"
fn parse_simd_type_from_name(name: string) -> string {
    if starts_with(name, "simd_splat_") {
        let prefix_len = len("simd_splat_")
        return substr(name, prefix_len, len(name) - prefix_len)
    }
    if starts_with(name, "simd_set_") {
        let prefix_len = len("simd_set_")
        return substr(name, prefix_len, len(name) - prefix_len)
    }
    return ""
}

-- Returns the scalar element type for a SIMD vector type.
-- f32x4 -> "f32", f32x8 -> "f32"
-- f64x2 -> "f64", f64x4 -> "f64"
-- i32x4 -> "i32", i32x8 -> "i32"
-- i64x2 -> "i64", i64x4 -> "i64"
fn simd_extract_scalar_type(vec_type: string) -> string {
    if vec_type == "f32x4" or vec_type == "f32x8" { return "f32" }
    if vec_type == "f64x2" or vec_type == "f64x4" { return "f64" }
    if vec_type == "i32x4" or vec_type == "i32x8" { return "i32" }
    if vec_type == "i64x2" or vec_type == "i64x4" { return "i64" }
    return "i32"
}

-- Returns the result type for a SIMD operation given the function name
-- and the type of the first argument.
-- For splat/set: the vector type from the function name
-- For add/sub/mul/div: same as first argument type
-- For extract: the scalar type of the vector
fn simd_result_type(fn_name: string, arg_type: string) -> string {
    -- splat and set derive their type from the function name suffix
    if starts_with(fn_name, "simd_splat_") or starts_with(fn_name, "simd_set_") {
        return parse_simd_type_from_name(fn_name)
    }
    -- Binary SIMD ops produce the same type as their inputs
    if fn_name == "simd_add" or fn_name == "simd_sub" {
        return arg_type
    }
    if fn_name == "simd_mul" or fn_name == "simd_div" {
        return arg_type
    }
    -- Extract produces the scalar element type
    if fn_name == "simd_extract" {
        return simd_extract_scalar_type(arg_type)
    }
    return arg_type
}

-- ============================================================
-- SIMD BUILTIN CODE GENERATION
-- ============================================================

-- Main dispatcher for SIMD builtin function calls.
-- Generates the appropriate SIMD IR instruction based on the function name.
fn generate_simd_builtin(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    -- simd_splat_TYPE(scalar_val)
    if starts_with(fn_name, "simd_splat_") {
        return generate_simd_splat(gen, block, fn_name, args)
    }

    -- simd_set_TYPE(a, b, c, d, ...)
    if starts_with(fn_name, "simd_set_") {
        return generate_simd_set(gen, block, fn_name, args)
    }

    -- simd_add(a, b)
    if fn_name == "simd_add" {
        return generate_simd_binary(gen, block, OP_SIMD_ADD(), args)
    }

    -- simd_sub(a, b)
    if fn_name == "simd_sub" {
        return generate_simd_binary(gen, block, OP_SIMD_SUB(), args)
    }

    -- simd_mul(a, b)
    if fn_name == "simd_mul" {
        return generate_simd_binary(gen, block, OP_SIMD_MUL(), args)
    }

    -- simd_div(a, b)
    if fn_name == "simd_div" {
        return generate_simd_binary(gen, block, OP_SIMD_DIV(), args)
    }

    -- simd_extract(vec, idx)
    if fn_name == "simd_extract" {
        return generate_simd_extract(gen, block, args)
    }

    -- Fallback: return zero
    return expr_result(gen, block, 0)
}

-- ============================================================
-- SIMD SPLAT
-- simd_splat_TYPE(scalar) -> vector with all lanes set to scalar
-- ============================================================

fn generate_simd_splat(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let vec_type = parse_simd_type_from_name(fn_name)

    -- Generate the scalar argument
    if args.len() < 1 {
        return expr_result(g, b, 0)
    }
    let scalar_result = generate_expr(g, b, args[0])
    g = scalar_result.gen
    b = scalar_result.block

    -- Emit OP_SIMD_SPLAT instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_SIMD_SPLAT())
    inst.simd_scalar = ir_val_inst(scalar_result.val_id)
    inst.simd_vec_type = vec_type
    inst.result_type = vec_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD SET
-- simd_set_TYPE(a, b, c, d, ...) -> vector from individual elements
-- ============================================================

fn generate_simd_set(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let vec_type = parse_simd_type_from_name(fn_name)

    -- Generate each element argument
    let mut elements: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let elem_result = generate_expr(g, b, args[i])
        g = elem_result.gen
        b = elem_result.block
        elements.push(ir_val_inst(elem_result.val_id))
        i = i + 1
    }

    -- Emit OP_SIMD_SET instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_SIMD_SET())
    inst.simd_elements = elements
    inst.simd_vec_type = vec_type
    inst.result_type = vec_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD BINARY OPERATIONS (add, sub, mul, div)
-- simd_op(lhs_vec, rhs_vec) -> element-wise result vector
-- ============================================================

fn generate_simd_binary(gen: IRGenerator, block: IRBasicBlock, op: int, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate left operand
    let lhs_result = generate_expr(g, b, args[0])
    g = lhs_result.gen
    b = lhs_result.block

    -- Generate right operand
    let rhs_result = generate_expr(g, b, args[1])
    g = rhs_result.gen
    b = rhs_result.block

    -- Infer the vector type from the first argument
    -- If the first arg is an identifier with a known SIMD variable type, use it
    let vec_type = infer_simd_arg_type(g, args[0])

    -- Emit the SIMD binary instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, op)
    inst.lhs = ir_val_inst(lhs_result.val_id)
    inst.rhs = ir_val_inst(rhs_result.val_id)
    inst.simd_vec_type = vec_type
    inst.result_type = vec_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD EXTRACT
-- simd_extract(vector, index) -> scalar element at index
-- ============================================================

fn generate_simd_extract(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate the vector argument
    let vec_result = generate_expr(g, b, args[0])
    g = vec_result.gen
    b = vec_result.block

    -- Generate the index argument
    let idx_result = generate_expr(g, b, args[1])
    g = idx_result.gen
    b = idx_result.block

    -- Infer the vector type from the first argument
    let vec_type = infer_simd_arg_type(g, args[0])
    -- Use dAImond-native types (f64 for float vectors, i64 for int vectors)
    let scalar_type = if is_simd_float_type(vec_type) { "f64" } else { "i64" }

    -- Emit OP_SIMD_EXTRACT instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_SIMD_EXTRACT())
    inst.simd_vector = ir_val_inst(vec_result.val_id)
    inst.simd_index = ir_val_inst(idx_result.val_id)
    inst.simd_vec_type = vec_type
    inst.result_type = scalar_type
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- SIMD TYPE INFERENCE HELPERS
-- ============================================================

-- Infer the SIMD vector type of an expression argument.
-- If the argument is an identifier whose type is tracked in variable_types,
-- use that. Otherwise fall back to "f32x4" as a default.
fn infer_simd_arg_type(gen: IRGenerator, arg: Expr) -> string {
    if arg.kind == EXPR_IDENTIFIER() {
        if gen.variable_types.contains(arg.name) {
            let ty = gen.variable_types.get(arg.name)
            if is_simd_type(ty) {
                return ty
            }
        }
    }
    -- If it's a function call (e.g. simd_splat_f32x4(...)), try to extract type from name
    if arg.kind == EXPR_FUNCTION_CALL() {
        let callee = *arg.callee
        if callee.kind == EXPR_IDENTIFIER() {
            let callee_name = callee.name
            if starts_with(callee_name, "simd_splat_") or starts_with(callee_name, "simd_set_") {
                return parse_simd_type_from_name(callee_name)
            }
        }
    }
    -- Default to f32x4 if type cannot be inferred
    return "f32x4"
}

-- ============================================================
-- SIMD LANE COUNT HELPERS
-- ============================================================

-- Returns the number of lanes for a given SIMD vector type.
fn simd_lane_count(vec_type: string) -> int {
    if vec_type == "f32x4" or vec_type == "i32x4" { return 4 }
    if vec_type == "f32x8" or vec_type == "i32x8" { return 8 }
    if vec_type == "f64x2" or vec_type == "i64x2" { return 2 }
    if vec_type == "f64x4" or vec_type == "i64x4" { return 4 }
    return 4
}

-- ============================================================
-- BUILTIN CALL GENERATION HELPERS
-- ============================================================

-- Generate a simple builtin function call (non-SIMD).
-- Maps the dAImond builtin name to the C runtime name and emits
-- an OP_CALL instruction with generated argument values.
fn generate_builtin_call(gen: IRGenerator, block: IRBasicBlock, fn_name: string, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block
    let c_name = map_builtin_name(fn_name)
    let ret_type = builtin_return_type(fn_name)

    -- Auto-convert print/println/eprint/eprintln args to string
    let is_print_fn = fn_name == "println" or fn_name == "print" or fn_name == "eprintln" or fn_name == "eprint"

    -- Generate all argument expressions
    let mut call_args: List[IRValue] = []
    let mut i = 0
    while i < args.len() {
        let arg_result = generate_expr(g, b, args[i])
        g = arg_result.gen
        b = arg_result.block

        if is_print_fn {
            let arg_type = infer_expr_type(g, args[i])
            if arg_type == "i64" or arg_type == "i32" or arg_type == "i16" or arg_type == "i8" or arg_type == "u64" or arg_type == "u32" or arg_type == "u16" or arg_type == "u8" or arg_type == "int" {
                -- Wrap with int_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_int_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(arg_result.val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                call_args.push(ir_val_inst(conv_id))
            } else if arg_type == "f64" or arg_type == "f32" or arg_type == "float" {
                -- Wrap with float_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_float_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(arg_result.val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                call_args.push(ir_val_inst(conv_id))
            } else if arg_type == "bool" {
                -- Wrap with bool_to_string
                let conv_id = g.builder.next_id
                let mut conv_inst = ir_inst_new(conv_id, OP_CALL())
                conv_inst.callee = "dm_bool_to_string"
                let mut conv_args: List[IRValue] = []
                conv_args.push(ir_val_inst(arg_result.val_id))
                conv_inst.call_args = conv_args
                conv_inst.result_type = "string"
                conv_inst.has_result = true
                b.instructions.push(conv_inst)
                g.builder.next_id = g.builder.next_id + 1
                call_args.push(ir_val_inst(conv_id))
            } else {
                call_args.push(ir_val_inst(arg_result.val_id))
            }
        } else {
            call_args.push(ir_val_inst(arg_result.val_id))
        }
        i = i + 1
    }

    -- Emit the call instruction
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = c_name
    inst.call_args = call_args
    inst.result_type = ret_type
    inst.has_result = ret_type != "void"
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate an assert call. Emits a conditional panic.
-- assert(cond) -> if !cond { panic("assertion failed") }
fn generate_assert_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the condition
    let cond_result = generate_expr(g, b, args[0])
    g = cond_result.gen
    b = cond_result.block

    -- Emit a call to dm_assert (runtime handles the panic)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = "dm_assert"
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(cond_result.val_id))
    inst.call_args = call_args
    inst.result_type = "void"
    inst.has_result = false
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate an assert_eq call.
-- assert_eq(a, b) -> if a != b { panic("values not equal") }
fn generate_assert_eq_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate both argument expressions
    let lhs_result = generate_expr(g, b, args[0])
    g = lhs_result.gen
    b = lhs_result.block

    let rhs_result = generate_expr(g, b, args[1])
    g = rhs_result.gen
    b = rhs_result.block

    -- Emit a call to dm_assert_eq (runtime handles the comparison + panic)
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CALL())
    inst.callee = "dm_assert_eq"
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(lhs_result.val_id))
    call_args.push(ir_val_inst(rhs_result.val_id))
    inst.call_args = call_args
    inst.result_type = "void"
    inst.has_result = false
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate a panic call.
-- panic(msg) -> dm_panic(msg); unreachable
fn generate_panic_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        -- Panic with no message
        let id = g.builder.next_id
        let mut inst = ir_inst_new(id, OP_PANIC())
        inst.operand = ir_val_string("panic")
        inst.has_result = false
        b.instructions.push(inst)
        g.builder.next_id = g.builder.next_id + 1
        return expr_result(g, b, id)
    }

    -- Generate the message expression
    let msg_result = generate_expr(g, b, args[0])
    g = msg_result.gen
    b = msg_result.block

    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_PANIC())
    inst.operand = ir_val_inst(msg_result.val_id)
    inst.has_result = false
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- Generate a Box_new(value) call.
-- Allocates the value on the heap via malloc and stores it.
fn generate_box_new_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 1 {
        return expr_result(g, b, 0)
    }

    -- Generate the value to box
    let val_result = generate_expr(g, b, args[0])
    g = val_result.gen
    b = val_result.block

    -- Compute allocation size based on the value being boxed
    let mut size = 8  -- default pointer-sized allocation
    let arg0 = args[0]
    if arg0.kind == EXPR_STRUCT_LITERAL() {
        let sname = arg0.type_name
        if g.struct_defs.contains(sname) {
            let sinfo = g.struct_defs.get(sname)
            -- Each field is 8 bytes (i64/f64/ptr)
            size = sinfo.field_names.len() * 8
            if size < 8 {
                size = 8
            }
        }
    }

    -- Allocate memory via malloc
    let malloc_id = g.builder.next_id
    let mut malloc_inst = ir_inst_new(malloc_id, OP_CALL())
    malloc_inst.callee = "malloc"
    let mut margs: List[IRValue] = []
    margs.push(ir_val_int(size))
    malloc_inst.call_args = margs
    malloc_inst.result_type = "ptr"
    malloc_inst.has_result = true
    b.instructions.push(malloc_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Store the value into the allocated memory
    let mut store_inst = ir_inst_new(g.builder.next_id, OP_STORE())
    store_inst.store_ptr = ir_val_inst(malloc_id)
    store_inst.store_val = ir_val_inst(val_result.val_id)
    store_inst.has_result = false
    b.instructions.push(store_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, malloc_id)
}

-- Generate a Box_null() call.
-- Returns a null pointer constant via inttoptr(0).
fn generate_box_null_call(gen: IRGenerator, block: IRBasicBlock) -> ExprResult {
    let mut g = gen
    let mut b = block

    -- Emit inttoptr(i64 0) to produce a true null pointer
    let id = g.builder.next_id
    let mut inst = ir_inst_new(id, OP_CAST())
    inst.cast_val = ir_val_int(0)
    inst.cast_from = "i64"
    inst.cast_to = "ptr"
    inst.result_type = "ptr"
    inst.has_result = true
    b.instructions.push(inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, id)
}

-- ============================================================
-- STRING SPLIT BUILTIN
-- string_split(s, delim) returns List[string]
-- Needs special handling: allocates a list, calls dm_string_split
-- ============================================================

fn generate_string_split_call(gen: IRGenerator, block: IRBasicBlock, args: List[Expr]) -> ExprResult {
    let mut g = gen
    let mut b = block

    if args.len() < 2 {
        return expr_result(g, b, 0)
    }

    -- Generate source string
    let str_result = generate_expr(g, b, args[0])
    g = str_result.gen
    b = str_result.block

    -- Generate delimiter string
    let delim_result = generate_expr(g, b, args[1])
    g = delim_result.gen
    b = delim_result.block

    -- Allocate a list to receive the results
    let list_alloca_id = g.builder.next_id
    let mut list_inst = ir_inst_new(list_alloca_id, OP_ALLOCA())
    list_inst.alloc_type = "dm_list_dm_string"
    list_inst.result_type = "ptr(dm_list_dm_string)"
    list_inst.has_result = true
    b.instructions.push(list_inst)
    g.builder.next_id = g.builder.next_id + 1

    -- Call dm_string_split(list_ptr, str, delim)
    let id = g.builder.next_id
    let mut call_inst = ir_inst_new(id, OP_CALL())
    call_inst.callee = "dm_string_split"
    let mut call_args: List[IRValue] = []
    call_args.push(ir_val_inst(list_alloca_id))
    call_args.push(ir_val_inst(str_result.val_id))
    call_args.push(ir_val_inst(delim_result.val_id))
    call_inst.call_args = call_args
    call_inst.result_type = "void"
    call_inst.has_result = false
    b.instructions.push(call_inst)
    g.builder.next_id = g.builder.next_id + 1

    return expr_result(g, b, list_alloca_id)
}


-- ============================================================
-- Compile-Time Expression Evaluation
-- Evaluates expressions at compile time, producing IRValues
-- directly without emitting IR instructions.
-- Port of stage3/src/ir_gen.zig comptime evaluation logic.
-- ============================================================

-- Maximum recursion depth for comptime function calls
fn COMPTIME_MAX_DEPTH() -> int { return 64 }

-- ============================================================
-- MAIN COMPTIME DISPATCHER
-- ============================================================

-- Evaluate an expression at compile time, returning an IRValue.
-- Returns ir_val_int(0) for unsupported expression kinds.
fn eval_comptime_expr(gen: IRGenerator, e: Expr) -> IRValue {
    return eval_comptime_expr_d(gen, e, 0)
}

fn eval_comptime_expr_d(gen: IRGenerator, e: Expr, depth: int) -> IRValue {
    -- Integer literal
    if e.kind == EXPR_LITERAL_INT() {
        return ir_val_int(e.int_val)
    }

    -- Float literal
    if e.kind == EXPR_LITERAL_FLOAT() {
        return ir_val_float(e.float_val)
    }

    -- Bool literal
    if e.kind == EXPR_LITERAL_BOOL() {
        return ir_val_bool(e.bool_val)
    }

    -- String literal
    if e.kind == EXPR_LITERAL_STRING() {
        return ir_val_string(e.str_val)
    }

    -- Binary expression
    if e.kind == EXPR_BINARY() {
        let lv = eval_comptime_expr_d(gen, *e.left, depth)
        let rv = eval_comptime_expr_d(gen, *e.right, depth)
        return eval_binary_comptime_vals(gen, e.op, lv, rv)
    }

    -- Unary expression
    if e.kind == EXPR_UNARY() {
        return eval_unary_comptime(gen, e)
    }

    -- Identifier: look up in module globals (reverse search for correct scoping)
    if e.kind == EXPR_IDENTIFIER() {
        return eval_identifier_comptime(gen, e.name)
    }

    -- If expression
    if e.kind == EXPR_IF() {
        let cond = eval_comptime_expr_d(gen, *e.condition, depth)
        if cond.kind == VAL_CONST_BOOL() and cond.bool_val {
            return eval_comptime_expr_d(gen, *e.then_branch, depth)
        }
        if e.has_else {
            return eval_comptime_expr_d(gen, *e.else_branch, depth)
        }
        return ir_val_int(0)
    }

    -- Block expression
    if e.kind == EXPR_BLOCK() {
        return eval_block_comptime_d(gen, e, depth)
    }

    -- Function call
    if e.kind == EXPR_FUNCTION_CALL() {
        return eval_call_comptime(gen, e, depth)
    }

    -- Match expression
    if e.kind == EXPR_MATCH() {
        return eval_match_comptime(gen, e)
    }

    -- Grouped expression (parenthesized)
    if e.kind == EXPR_GROUPED() {
        let inner = *e.operand
        return eval_comptime_expr_d(gen, inner, depth)
    }

    -- Comptime expression wrapper (e.g., `comptime expr`)
    if e.kind == EXPR_COMPTIME() {
        let inner = *e.operand
        return eval_comptime_expr_d(gen, inner, depth)
    }

    -- Array literal: store elements as indexed globals (arr__0, arr__1, ..., arr__len)
    if e.kind == EXPR_ARRAY_LITERAL() {
        let arr_name = "__ct_arr_" + int_to_string(gen.builder.next_id)
        let mut g = gen
        let mut ei = 0
        while ei < e.elements.len() {
            let elem_val = eval_comptime_expr_d(g, e.elements[ei], depth)
            let elem_name = arr_name + "__" + int_to_string(ei)
            let mut gl = ir_global_new(elem_name, "i64")
            gl.init_value = elem_val
            gl.has_init = true
            gl.is_const = true
            g.ir_mod.globals.push(gl)
            ei = ei + 1
        }
        -- Store array length
        let len_name = arr_name + "__len"
        let mut len_gl = ir_global_new(len_name, "i64")
        len_gl.init_value = ir_val_int(e.elements.len())
        len_gl.has_init = true
        len_gl.is_const = true
        g.ir_mod.globals.push(len_gl)
        -- Store the array name marker so identifier lookup can find it
        let mut marker = ir_global_new(arr_name, "i64")
        marker.init_value = ir_val_string(arr_name)
        marker.has_init = true
        marker.is_const = true
        g.ir_mod.globals.push(marker)
        return ir_val_string(arr_name)
    }

    -- Index access: load from a comptime array at constant index
    if e.kind == EXPR_INDEX_ACCESS() {
        let obj_val = eval_comptime_expr_d(gen, *e.object, depth)
        let idx_val = eval_comptime_expr_d(gen, *e.index, depth)
        if obj_val.kind == VAL_CONST_STRING() and idx_val.kind == VAL_CONST_INT() {
            -- obj_val.str_val is the array name, look up element
            let elem_name = obj_val.str_val + "__" + int_to_string(idx_val.int_val)
            return eval_identifier_comptime(gen, elem_name)
        }
        return ir_val_int(0)
    }

    -- Unsupported expression kind
    return ir_val_int(0)
}

-- ============================================================
-- IDENTIFIER LOOKUP
-- ============================================================

-- Look up an identifier in the module's global constants.
fn eval_identifier_comptime(gen: IRGenerator, name: string) -> IRValue {
    -- Search in reverse order so innermost bindings (e.g. recursive call params) shadow outer ones
    let mut i = gen.ir_mod.globals.len() - 1
    while i >= 0 {
        let gl = gen.ir_mod.globals[i]
        if gl.name == name and gl.has_init {
            return gl.init_value
        }
        i = i - 1
    }
    return ir_val_int(0)
}

-- ============================================================
-- BINARY EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a binary expression at compile time.
-- Supports integer arithmetic, float arithmetic, boolean logic,
-- comparison operators, string concatenation, and string comparison.
fn eval_binary_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let lhs = eval_comptime_expr(gen, *e.left)
    let rhs = eval_comptime_expr(gen, *e.right)
    let op = e.op

    -- Integer arithmetic
    if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_INT() {
        let a = lhs.int_val
        let b = rhs.int_val

        if op == BINOP_ADD() { return ir_val_int(a + b) }
        if op == BINOP_SUB() { return ir_val_int(a - b) }
        if op == BINOP_MUL() { return ir_val_int(a * b) }
        if op == BINOP_DIV() {
            if b != 0 {
                return ir_val_int(a / b)
            }
            return ir_val_int(0)
        }
        if op == BINOP_MOD() {
            if b != 0 {
                return ir_val_int(a % b)
            }
            return ir_val_int(0)
        }

        -- Integer comparisons
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
        if op == BINOP_LT() { return ir_val_bool(a < b) }
        if op == BINOP_LE() { return ir_val_bool(a <= b) }
        if op == BINOP_GT() { return ir_val_bool(a > b) }
        if op == BINOP_GE() { return ir_val_bool(a >= b) }
    }

    -- Float arithmetic
    if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_FLOAT() {
        let a = lhs.float_val
        let b = rhs.float_val

        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
        if op == BINOP_DIV() {
            return ir_val_float(a / b)
        }

        -- Float comparisons
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
        if op == BINOP_LT() { return ir_val_bool(a < b) }
        if op == BINOP_LE() { return ir_val_bool(a <= b) }
        if op == BINOP_GT() { return ir_val_bool(a > b) }
        if op == BINOP_GE() { return ir_val_bool(a >= b) }
    }

    -- Mixed int/float: promote int to float
    if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_FLOAT() {
        let a = lhs.int_val
        let b = rhs.float_val
        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
    }

    if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_INT() {
        let a = lhs.float_val
        let b = rhs.int_val
        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
    }

    -- Boolean logic
    if lhs.kind == VAL_CONST_BOOL() and rhs.kind == VAL_CONST_BOOL() {
        let a = lhs.bool_val
        let b = rhs.bool_val

        if op == BINOP_AND() { return ir_val_bool(a and b) }
        if op == BINOP_OR() { return ir_val_bool(a or b) }
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
    }

    -- String concatenation
    if lhs.kind == VAL_CONST_STRING() and rhs.kind == VAL_CONST_STRING() {
        let a = lhs.str_val
        let b = rhs.str_val

        if op == BINOP_ADD() { return ir_val_string(a + b) }
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
    }

    return ir_val_int(0)
}

-- Binary comptime with pre-evaluated values
fn eval_binary_comptime_vals(gen: IRGenerator, op: int, lhs: IRValue, rhs: IRValue) -> IRValue {
    if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_INT() {
        let a = lhs.int_val
        let b = rhs.int_val
        if op == BINOP_ADD() { return ir_val_int(a + b) }
        if op == BINOP_SUB() { return ir_val_int(a - b) }
        if op == BINOP_MUL() { return ir_val_int(a * b) }
        if op == BINOP_DIV() { if b != 0 { return ir_val_int(a / b) } return ir_val_int(0) }
        if op == BINOP_MOD() { if b != 0 { return ir_val_int(a % b) } return ir_val_int(0) }
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
        if op == BINOP_LT() { return ir_val_bool(a < b) }
        if op == BINOP_LE() { return ir_val_bool(a <= b) }
        if op == BINOP_GT() { return ir_val_bool(a > b) }
        if op == BINOP_GE() { return ir_val_bool(a >= b) }
    }
    if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_FLOAT() {
        let a = lhs.float_val
        let b = rhs.float_val
        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
        if op == BINOP_DIV() { return ir_val_float(a / b) }
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
        if op == BINOP_LT() { return ir_val_bool(a < b) }
        if op == BINOP_LE() { return ir_val_bool(a <= b) }
        if op == BINOP_GT() { return ir_val_bool(a > b) }
        if op == BINOP_GE() { return ir_val_bool(a >= b) }
    }
    if lhs.kind == VAL_CONST_BOOL() and rhs.kind == VAL_CONST_BOOL() {
        if op == BINOP_AND() { return ir_val_bool(lhs.bool_val and rhs.bool_val) }
        if op == BINOP_OR() { return ir_val_bool(lhs.bool_val or rhs.bool_val) }
        if op == BINOP_EQ() { return ir_val_bool(lhs.bool_val == rhs.bool_val) }
        if op == BINOP_NE() { return ir_val_bool(lhs.bool_val != rhs.bool_val) }
    }
    if lhs.kind == VAL_CONST_STRING() and rhs.kind == VAL_CONST_STRING() {
        if op == BINOP_ADD() { return ir_val_string(lhs.str_val + rhs.str_val) }
        if op == BINOP_EQ() { return ir_val_bool(lhs.str_val == rhs.str_val) }
        if op == BINOP_NE() { return ir_val_bool(lhs.str_val != rhs.str_val) }
    }
    return ir_val_int(0)
}

-- ============================================================
-- UNARY EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a unary expression at compile time.
-- Supports negation of int/float and logical not of bool.
fn eval_unary_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let operand_val = eval_comptime_expr(gen, *e.operand)

    if e.op == UNOP_NEG() {
        if operand_val.kind == VAL_CONST_INT() {
            return ir_val_int(0 - operand_val.int_val)
        }
        if operand_val.kind == VAL_CONST_FLOAT() {
            return ir_val_float(0.0 - operand_val.float_val)
        }
    }

    if e.op == UNOP_NOT() {
        if operand_val.kind == VAL_CONST_BOOL() {
            if operand_val.bool_val {
                return ir_val_bool(false)
            } else {
                return ir_val_bool(true)
            }
        }
    }

    return ir_val_int(0)
}

-- ============================================================
-- IF EXPRESSION EVALUATION
-- ============================================================

-- Evaluate an if expression at compile time.
-- The condition must evaluate to a boolean. The appropriate branch
-- is then evaluated and its value returned.
fn eval_if_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let cond = eval_comptime_expr(gen, *e.condition)

    -- Condition must be a boolean
    if cond.kind == VAL_CONST_BOOL() {
        if cond.bool_val {
            return eval_comptime_expr(gen, *e.then_branch)
        } else if e.has_else {
            return eval_comptime_expr(gen, *e.else_branch)
        }
    }

    -- If condition is a truthy int (non-zero)
    if cond.kind == VAL_CONST_INT() {
        if cond.int_val != 0 {
            return eval_comptime_expr(gen, *e.then_branch)
        } else if e.has_else {
            return eval_comptime_expr(gen, *e.else_branch)
        }
    }

    return ir_val_int(0)
}

-- ============================================================
-- BLOCK EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a block expression at compile time.
-- Processes statements in order (let bindings, returns, expressions).
-- The last expression-statement's value is the block result.
fn eval_block_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    return eval_block_comptime_d(gen, e, 0)
}

fn eval_block_comptime_d(gen: IRGenerator, e: Expr, depth: int) -> IRValue {
    -- Process statements in the block first
    let mut g = gen
    let mut last_val = ir_val_int(0)
    let mut i = 0
    while i < e.stmts.len() {
        let stmt = e.stmts[i]

        -- Handle return statements - return immediately
        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            return eval_comptime_expr_d(g, *stmt.ret_value, depth)
        }

        -- Handle let bindings - store in globals for subsequent lookups
        if stmt.kind == STMT_LET() {
            let val_expr = *stmt.let_value
            if val_expr.kind != EXPR_LITERAL_NULL() {
                -- Array literal: inline element global creation on g directly
                -- to avoid losing them in a by-value function call copy.
                if val_expr.kind == EXPR_ARRAY_LITERAL() {
                    let arr_name = "__ct_arr_" + int_to_string(g.builder.next_id)
                    let mut ei = 0
                    while ei < val_expr.elements.len() {
                        let elem_val = eval_comptime_expr_d(g, val_expr.elements[ei], depth)
                        let elem_name = arr_name + "__" + int_to_string(ei)
                        let mut elem_gl = ir_global_new(elem_name, "i64")
                        elem_gl.init_value = elem_val
                        elem_gl.has_init = true
                        elem_gl.is_const = true
                        g.ir_mod.globals.push(elem_gl)
                        ei = ei + 1
                    }
                    let len_name = arr_name + "__len"
                    let mut len_gl = ir_global_new(len_name, "i64")
                    len_gl.init_value = ir_val_int(val_expr.elements.len())
                    len_gl.has_init = true
                    len_gl.is_const = true
                    g.ir_mod.globals.push(len_gl)
                    let mut marker = ir_global_new(arr_name, "i64")
                    marker.init_value = ir_val_string(arr_name)
                    marker.has_init = true
                    marker.is_const = true
                    g.ir_mod.globals.push(marker)
                    let mut global = ir_global_new(stmt.let_name, "i64")
                    global.init_value = ir_val_string(arr_name)
                    global.has_init = true
                    global.is_const = true
                    g.ir_mod.globals.push(global)
                } else {
                    let val = eval_comptime_expr_d(g, val_expr, depth)
                    let mut global = ir_global_new(stmt.let_name, "i64")
                    global.init_value = val
                    global.has_init = true
                    global.is_const = true
                    g.ir_mod.globals.push(global)
                }
            }
        }

        -- Handle expression statements - last one is the block result
        if stmt.kind == STMT_EXPRESSION() {
            last_val = eval_comptime_expr_d(g, *stmt.expr, depth)
        }

        -- Handle assignment statements
        if stmt.kind == STMT_ASSIGNMENT() {
            let val = eval_comptime_expr_d(g, *stmt.assign_value, depth)
            let target_expr = *stmt.assign_target
            if target_expr.kind == EXPR_IDENTIFIER() {
                let target_name = target_expr.name
                let mut gi = g.ir_mod.globals.len() - 1
                while gi >= 0 {
                    if g.ir_mod.globals[gi].name == target_name {
                        let mut glob = g.ir_mod.globals[gi]
                        glob.init_value = val
                        g.ir_mod.globals[gi] = glob
                        gi = -1
                    } else {
                        gi = gi - 1
                    }
                }
            }
        }

        -- Handle while loops
        if stmt.kind == STMT_WHILE() {
            let mut loop_count = 0
            let mut looping = true
            while looping and loop_count < 10000 {
                let cond = eval_comptime_expr_d(g, *stmt.while_cond, depth)
                let mut cond_true = false
                if cond.kind == VAL_CONST_BOOL() and cond.bool_val {
                    cond_true = true
                }
                if cond.kind == VAL_CONST_INT() and cond.int_val != 0 {
                    cond_true = true
                }
                if cond_true == false {
                    looping = false
                } else {
                    let body_val = eval_comptime_stmts(g, stmt.while_body, depth + 1)
                    if body_val.kind == VAL_BREAK_SENTINEL() {
                        looping = false
                    } else if body_val.kind != VAL_UNDEF() {
                        return body_val
                    }
                    loop_count = loop_count + 1
                }
            }
        }

        -- Handle for loops (range-based and array-based)
        if stmt.kind == STMT_FOR() {
            let iter_expr = *stmt.for_iter
            if iter_expr.kind == EXPR_RANGE() {
                -- Range for loop: for v in start..end
                let start_val = eval_comptime_expr_d(g, *iter_expr.left, depth)
                let end_val = eval_comptime_expr_d(g, *iter_expr.right, depth)
                if start_val.kind == VAL_CONST_INT() and end_val.kind == VAL_CONST_INT() {
                    let mut fi = start_val.int_val
                    while fi < end_val.int_val {
                        let mut loop_var = ir_global_new(stmt.for_var, "i64")
                        loop_var.init_value = ir_val_int(fi)
                        loop_var.has_init = true
                        loop_var.is_const = true
                        g.ir_mod.globals.push(loop_var)
                        let body_val = eval_comptime_stmts(g, stmt.for_body, depth + 1)
                        if body_val.kind == VAL_BREAK_SENTINEL() {
                            fi = end_val.int_val
                        } else if body_val.kind != VAL_UNDEF() {
                            return body_val
                        }
                        fi = fi + 1
                    }
                }
            } else {
                -- Array for loop: for v in arr
                let arr_val = eval_comptime_expr_d(g, iter_expr, depth)
                if arr_val.kind == VAL_CONST_STRING() {
                    let arr_name = arr_val.str_val
                    let len_val = eval_identifier_comptime(g, arr_name + "__len")
                    if len_val.kind == VAL_CONST_INT() {
                        let mut fi = 0
                        while fi < len_val.int_val {
                            let elem_val = eval_identifier_comptime(g, arr_name + "__" + int_to_string(fi))
                            let mut loop_var = ir_global_new(stmt.for_var, "i64")
                            loop_var.init_value = elem_val
                            loop_var.has_init = true
                            loop_var.is_const = true
                            g.ir_mod.globals.push(loop_var)
                            let body_val = eval_comptime_stmts(g, stmt.for_body, depth + 1)
                            if body_val.kind == VAL_BREAK_SENTINEL() {
                                fi = len_val.int_val
                            } else if body_val.kind != VAL_UNDEF() {
                                return body_val
                            }
                            fi = fi + 1
                        }
                    }
                }
            }
        }

        -- Handle match statements
        if stmt.kind == STMT_MATCH() {
            -- The parser stores the full EXPR_MATCH in stmt.match_expr
            -- (match_stmt_arms is always empty). Evaluate the match expression
            -- directly, which delegates to eval_match_comptime for correct
            -- scrutinee/arm resolution.
            let match_val = eval_comptime_expr_d(g, *stmt.match_expr, depth)
            last_val = match_val
        }

        -- Handle if statements with potential returns
        if stmt.kind == STMT_IF() {
            let cond = eval_comptime_expr_d(g, *stmt.if_cond, depth)
            if cond.kind == VAL_CONST_BOOL() {
                if cond.bool_val {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
            if cond.kind == VAL_CONST_INT() {
                if cond.int_val != 0 {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
        }

        i = i + 1
    }

    -- If the block has a result expression, evaluate it after processing statements
    if e.has_result {
        return eval_comptime_expr_d(g, *e.result, depth)
    }

    return last_val
}

-- ============================================================
-- FUNCTION CALL EVALUATION
-- ============================================================

-- Evaluate a function call at compile time.
-- Looks up the function in the generator's declaration list,
-- substitutes parameters, and evaluates the body.
-- Supports recursion up to COMPTIME_MAX_DEPTH.
fn eval_call_comptime(gen: IRGenerator, e: Expr, depth: int) -> IRValue {
    -- Prevent infinite recursion
    if depth >= COMPTIME_MAX_DEPTH() {
        return ir_val_int(0)
    }

    -- Get the function name from the callee expression
    let callee = *e.callee
    if callee.kind != EXPR_IDENTIFIER() {
        return ir_val_int(0)
    }
    let fn_name = callee.name

    -- Evaluate all argument expressions
    let mut arg_values: List[IRValue] = []
    let mut i = 0
    while i < e.args.len() {
        let val = eval_comptime_expr_d(gen, e.args[i], depth)
        arg_values.push(val)
        i = i + 1
    }

    -- Handle builtin functions
    if fn_name == "len" and arg_values.len() == 1 {
        let arg = arg_values[0]
        if arg.kind == VAL_CONST_STRING() {
            return ir_val_int(len(arg.str_val))
        }
        return ir_val_int(0)
    }

    -- Look up the function declaration in source
    let mut found_idx = -1
    i = 0
    while i < gen.declarations.len() {
        let decl = gen.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.name == fn_name {
                found_idx = i
            }
        }
        i = i + 1
    }

    if found_idx < 0 {
        return ir_val_int(0)
    }

    let decl = gen.declarations[found_idx]
    let fd = *decl.func_decl

    -- Simple case: function with a single return statement
    if fd.body.len() == 1 {
        let stmt = fd.body[0]
        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            -- Create a modified generator with parameter values as globals
            let mut g = gen
            i = 0
            while i < fd.params.len() and i < arg_values.len() {
                let param_name = fd.params[i].name
                let mut global = ir_global_new(param_name, "i64")
                global.init_value = arg_values[i]
                global.has_init = true
                global.is_const = true
                g.ir_mod.globals.push(global)
                i = i + 1
            }
            return eval_comptime_expr_d(g, *stmt.ret_value, depth + 1)
        }
    }

    -- Multi-statement function: evaluate sequentially
    let mut g = gen

    -- Bind parameters as globals
    i = 0
    while i < fd.params.len() and i < arg_values.len() {
        let param_name = fd.params[i].name
        let mut global = ir_global_new(param_name, "i64")
        global.init_value = arg_values[i]
        global.has_init = true
        global.is_const = true
        g.ir_mod.globals.push(global)
        i = i + 1
    }

    -- Evaluate body statements looking for return
    i = 0
    while i < fd.body.len() {
        let stmt = fd.body[i]

        -- Handle return statements
        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            return eval_comptime_expr_d(g, *stmt.ret_value, depth + 1)
        }

        -- Handle let bindings (add to globals for subsequent expressions)
        if stmt.kind == STMT_LET() {
            let val_expr = *stmt.let_value
            if val_expr.kind != EXPR_LITERAL_NULL() {
                let val = eval_comptime_expr_d(g, val_expr, depth + 1)
                let mut global = ir_global_new(stmt.let_name, "i64")
                global.init_value = val
                global.has_init = true
                global.is_const = true
                g.ir_mod.globals.push(global)
            }
        }

        -- Handle if statements with returns
        if stmt.kind == STMT_IF() {
            let cond = eval_comptime_expr_d(g, *stmt.if_cond, depth + 1)
            if cond.kind == VAL_CONST_BOOL() {
                if cond.bool_val {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
            if cond.kind == VAL_CONST_INT() {
                if cond.int_val != 0 {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
        }

        -- Handle assignment statements
        if stmt.kind == STMT_ASSIGNMENT() {
            let val = eval_comptime_expr_d(g, *stmt.assign_value, depth + 1)
            let target_expr = *stmt.assign_target
            if target_expr.kind == EXPR_IDENTIFIER() {
                let target_name = target_expr.name
                let mut gi = g.ir_mod.globals.len() - 1
                while gi >= 0 {
                    if g.ir_mod.globals[gi].name == target_name {
                        let mut glob = g.ir_mod.globals[gi]
                        glob.init_value = val
                        g.ir_mod.globals[gi] = glob
                        gi = -1
                    } else {
                        gi = gi - 1
                    }
                }
            }
        }

        -- Handle while loops
        if stmt.kind == STMT_WHILE() {
            let mut loop_count = 0
            let mut looping = true
            while looping and loop_count < 10000 {
                let cond = eval_comptime_expr_d(g, *stmt.while_cond, depth + 1)
                let mut cond_true = false
                if cond.kind == VAL_CONST_BOOL() and cond.bool_val {
                    cond_true = true
                }
                if cond.kind == VAL_CONST_INT() and cond.int_val != 0 {
                    cond_true = true
                }
                if cond_true == false {
                    looping = false
                } else {
                    let body_val = eval_comptime_stmts(g, stmt.while_body, depth + 1)
                    if body_val.kind == VAL_BREAK_SENTINEL() {
                        looping = false
                    } else if body_val.kind != VAL_UNDEF() {
                        return body_val
                    }
                    loop_count = loop_count + 1
                }
            }
        }

        i = i + 1
    }

    return ir_val_int(0)
}

-- ============================================================
-- STATEMENT LIST EVALUATION (for comptime if branches)
-- ============================================================

-- Evaluate a list of statements at compile time, looking for a return.
-- Returns ir_val_undef() if no return is found.
fn eval_comptime_stmts(gen: IRGenerator, stmts: List[Stmt], depth: int) -> IRValue {
    let mut g = gen
    let mut i = 0
    let mut last_val = ir_val_undef()
    while i < stmts.len() {
        let stmt = stmts[i]

        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            return eval_comptime_expr_d(g, *stmt.ret_value, depth)
        }

        -- Handle break statements - return break sentinel
        if stmt.kind == STMT_BREAK() {
            return ir_val_break()
        }

        if stmt.kind == STMT_LET() {
            let val_expr = *stmt.let_value
            if val_expr.kind != EXPR_LITERAL_NULL() {
                let val = eval_comptime_expr_d(g, val_expr, depth)
                let mut global = ir_global_new(stmt.let_name, "i64")
                global.init_value = val
                global.has_init = true
                global.is_const = true
                g.ir_mod.globals.push(global)
            }
        }

        if stmt.kind == STMT_EXPRESSION() {
            last_val = eval_comptime_expr_d(g, *stmt.expr, depth)
        }

        -- Handle assignment (for loop variables etc)
        if stmt.kind == STMT_ASSIGNMENT() {
            let val = eval_comptime_expr_d(g, *stmt.assign_value, depth)
            -- Update the global with the new value
            let target_expr = *stmt.assign_target
            if target_expr.kind == EXPR_IDENTIFIER() {
                let target_name = target_expr.name
                let mut gi = g.ir_mod.globals.len() - 1
                while gi >= 0 {
                    if g.ir_mod.globals[gi].name == target_name {
                        let mut glob = g.ir_mod.globals[gi]
                        glob.init_value = val
                        g.ir_mod.globals[gi] = glob
                        gi = -1
                    } else {
                        gi = gi - 1
                    }
                }
            }
        }

        -- Handle if statements
        if stmt.kind == STMT_IF() {
            let cond = eval_comptime_expr_d(g, *stmt.if_cond, depth)
            if cond.kind == VAL_CONST_BOOL() {
                if cond.bool_val {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
            if cond.kind == VAL_CONST_INT() {
                if cond.int_val != 0 {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth + 1)
                    if branch_val.kind == VAL_BREAK_SENTINEL() {
                        return branch_val
                    }
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
        }

        -- Handle while loops
        if stmt.kind == STMT_WHILE() {
            let mut loop_count = 0
            let mut looping = true
            while looping and loop_count < 10000 {
                let cond = eval_comptime_expr_d(g, *stmt.while_cond, depth)
                let mut cond_true = false
                if cond.kind == VAL_CONST_BOOL() and cond.bool_val {
                    cond_true = true
                }
                if cond.kind == VAL_CONST_INT() and cond.int_val != 0 {
                    cond_true = true
                }
                if cond_true == false {
                    looping = false
                } else {
                    let body_val = eval_comptime_stmts(g, stmt.while_body, depth + 1)
                    if body_val.kind == VAL_BREAK_SENTINEL() {
                        looping = false
                    } else if body_val.kind != VAL_UNDEF() {
                        return body_val
                    }
                    loop_count = loop_count + 1
                }
            }
        }

        -- Handle for loops (range-based and array-based)
        if stmt.kind == STMT_FOR() {
            let iter_expr = *stmt.for_iter
            if iter_expr.kind == EXPR_RANGE() {
                let start_val = eval_comptime_expr_d(g, *iter_expr.left, depth)
                let end_val = eval_comptime_expr_d(g, *iter_expr.right, depth)
                if start_val.kind == VAL_CONST_INT() and end_val.kind == VAL_CONST_INT() {
                    let mut fi = start_val.int_val
                    while fi < end_val.int_val {
                        let mut loop_var = ir_global_new(stmt.for_var, "i64")
                        loop_var.init_value = ir_val_int(fi)
                        loop_var.has_init = true
                        loop_var.is_const = true
                        g.ir_mod.globals.push(loop_var)
                        let body_val = eval_comptime_stmts(g, stmt.for_body, depth + 1)
                        if body_val.kind == VAL_BREAK_SENTINEL() {
                            fi = end_val.int_val
                        } else if body_val.kind != VAL_UNDEF() {
                            return body_val
                        }
                        fi = fi + 1
                    }
                }
            } else {
                let arr_val = eval_comptime_expr_d(g, iter_expr, depth)
                if arr_val.kind == VAL_CONST_STRING() {
                    let arr_name = arr_val.str_val
                    let len_val = eval_identifier_comptime(g, arr_name + "__len")
                    if len_val.kind == VAL_CONST_INT() {
                        let mut fi = 0
                        while fi < len_val.int_val {
                            let elem_val = eval_identifier_comptime(g, arr_name + "__" + int_to_string(fi))
                            let mut loop_var = ir_global_new(stmt.for_var, "i64")
                            loop_var.init_value = elem_val
                            loop_var.has_init = true
                            loop_var.is_const = true
                            g.ir_mod.globals.push(loop_var)
                            let body_val = eval_comptime_stmts(g, stmt.for_body, depth + 1)
                            if body_val.kind == VAL_BREAK_SENTINEL() {
                                fi = len_val.int_val
                            } else if body_val.kind != VAL_UNDEF() {
                                return body_val
                            }
                            fi = fi + 1
                        }
                    }
                }
            }
        }

        -- Handle match statements
        if stmt.kind == STMT_MATCH() {
            let scrutinee_val = eval_comptime_expr_d(g, *stmt.match_expr, depth)
            let mut mi = 0
            let mut matched = false
            while mi < stmt.match_stmt_arms.len() and matched == false {
                let arm = stmt.match_stmt_arms[mi]
                let pat = arm.pattern
                let matches = comptime_pattern_matches(scrutinee_val, pat)
                if matches {
                    matched = true
                    if arm.is_expr_body {
                        last_val = eval_comptime_expr_d(g, *arm.body_expr, depth)
                    } else {
                        let body_val = eval_comptime_stmts(g, arm.body, depth + 1)
                        if body_val.kind != VAL_UNDEF() {
                            return body_val
                        }
                    }
                }
                mi = mi + 1
            }
        }

        i = i + 1
    }
    return last_val
}

-- ============================================================
-- MATCH EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a match expression at compile time.
-- Evaluates the scrutinee, then checks each arm's pattern.
-- For literal patterns, compares values. For wildcard/identifier,
-- always matches. Returns the body expression of the first match.
fn eval_match_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let scrutinee_val = eval_comptime_expr(gen, *e.scrutinee)

    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        let pat = arm.pattern

        -- Check if pattern matches
        let matches = comptime_pattern_matches(scrutinee_val, pat)

        if matches {
            -- If guard present, evaluate it
            if arm.has_guard {
                let guard_val = eval_comptime_expr(gen, *arm.guard)
                if guard_val.kind == VAL_CONST_BOOL() and guard_val.bool_val == false {
                    -- Guard failed, try next arm
                    i = i + 1
                    continue
                }
            }

            -- Evaluate the arm body
            if arm.is_expr_body {
                return eval_comptime_expr(gen, *arm.body_expr)
            }

            -- Statement body: look for return
            let body_val = eval_comptime_stmts(gen, arm.body, 0)
            if body_val.kind != VAL_UNDEF() {
                return body_val
            }
            return ir_val_int(0)
        }

        i = i + 1
    }

    return ir_val_int(0)
}

-- ============================================================
-- PATTERN MATCHING FOR COMPTIME
-- ============================================================

-- Check if a comptime value matches a pattern.
fn comptime_pattern_matches(val: IRValue, pat: Pattern) -> bool {
    -- Wildcard always matches
    if pat.kind == PAT_WILDCARD() {
        return true
    }

    -- Identifier pattern always matches (acts as binding)
    if pat.kind == PAT_IDENTIFIER() {
        return true
    }

    -- Literal pattern: compare values
    if pat.kind == PAT_LITERAL() {
        let lit_expr = *pat.literal_expr
        let lit_val = comptime_literal_to_value(lit_expr)
        return comptime_values_equal(val, lit_val)
    }

    -- Other patterns (enum, struct) not supported at comptime
    return false
}

-- Convert a literal expression to an IRValue for pattern comparison.
fn comptime_literal_to_value(e: Expr) -> IRValue {
    if e.kind == EXPR_LITERAL_INT() {
        return ir_val_int(e.int_val)
    }
    if e.kind == EXPR_LITERAL_FLOAT() {
        return ir_val_float(e.float_val)
    }
    if e.kind == EXPR_LITERAL_BOOL() {
        return ir_val_bool(e.bool_val)
    }
    if e.kind == EXPR_LITERAL_STRING() {
        return ir_val_string(e.str_val)
    }
    return ir_val_int(0)
}

-- ============================================================
-- VALUE COMPARISON
-- ============================================================

-- Compare two IRValues for equality at compile time.
-- Returns true if both values are of the same kind and equal.
fn comptime_values_equal(a: IRValue, b: IRValue) -> bool {
    -- Different kinds are never equal
    if a.kind != b.kind {
        return false
    }

    if a.kind == VAL_CONST_INT() {
        return a.int_val == b.int_val
    }

    if a.kind == VAL_CONST_FLOAT() {
        return a.float_val == b.float_val
    }

    if a.kind == VAL_CONST_BOOL() {
        return a.bool_val == b.bool_val
    }

    if a.kind == VAL_CONST_STRING() {
        return a.str_val == b.str_val
    }

    -- Instruction refs: compare by ID
    if a.kind == VAL_INST_REF() {
        return a.ref_id == b.ref_id
    }

    return false
}

-- ============================================================
-- COMPTIME RESULT TYPE INFERENCE
-- ============================================================

-- Infer the IR type string for a comptime-evaluated value.
fn comptime_value_type(val: IRValue) -> string {
    if val.kind == VAL_CONST_INT() { return "i64" }
    if val.kind == VAL_CONST_FLOAT() { return "f64" }
    if val.kind == VAL_CONST_BOOL() { return "bool" }
    if val.kind == VAL_CONST_STRING() { return "string" }
    return "i64"
}


-- ============================================================
-- LLVM IR Generator
-- Translates dAImond IR (from ir.dm) into LLVM IR using
-- the LLVM-C bridge functions declared in llvm.dm.
--
-- Key design: All LLVM refs (context, module, builder, types,
-- values, basic blocks) are represented as int (int64_t on C side).
--
-- String ABI: dm_string is { ptr, i64, i64 } (24 bytes) which
-- exceeds the x86-64 SysV ABI 16-byte register threshold.
-- Runtime functions using dm_string are called through thin
-- C wrapper functions (llvm_dm_*) with pointer-based ABI:
-- - Returns dm_string -> wrapper takes dm_string* as first param (sret)
-- - Takes dm_string -> wrapper takes const dm_string* (pointer)
-- String values in LLVM IR are pointers to stack-allocated dm_string structs.
-- ============================================================

-- Bridge functions declared in llvm_bridge.c but not yet in llvm.dm
extern fn dm_llvm_struct_set_body(struct_ty: int, fields: int, field_count: int, is_packed: int)extern fn dm_llvm_add_incoming(phi: int, values: int, blocks: int, count: int)extern fn dm_llvm_const_struct(ctx: int, values: int, count: int, is_packed: int) -> int
extern fn dm_llvm_const_array(elem_ty: int, values: int, count: int) -> int

-- ============================================================
-- LLVM GENERATOR STRUCT
-- ============================================================

struct LLVMGenerator {
    context: int,
    llvm_module: int,
    builder: int,
    -- Maps
    value_map: Map[string, int],
    function_map: Map[string, int],
    fn_type_map: Map[string, int],
    block_map: Map[string, int],
    struct_type_map: Map[string, int],
    global_var_map: Map[string, int],
    -- String handling
    dm_string_type: int,
    has_dm_string_type: bool,
    string_counter: int,
    sret_counter: int,
    -- Track which instruction IDs are string pointers
    string_ptrs: Map[string, bool],
    -- Track wrapper functions for string ABI
    wrapper_returns_string: Map[string, bool],
    -- Track which functions have wrappers
    has_wrapper: Map[string, bool],
    -- Track which functions use sret return convention (result/option types)
    returns_sret: Map[string, bool],
    -- Reference to IR module for type lookups
    ir_module: IRModule,
    -- Current function being generated (for param lookups)
    current_func_name: string
}

-- ============================================================
-- CONSTRUCTOR / DESTRUCTOR
-- ============================================================

fn llvm_gen_new(ir_mod: IRModule, module_name: string) -> LLVMGenerator {
    let ctx = dm_llvm_context_create()
    let mod = dm_llvm_module_create(ctx, module_name)
    let bld = dm_llvm_create_builder(ctx)
    return LLVMGenerator {
        context: ctx,
        llvm_module: mod,
        builder: bld,
        value_map: Map_new(),
        function_map: Map_new(),
        fn_type_map: Map_new(),
        block_map: Map_new(),
        struct_type_map: Map_new(),
        global_var_map: Map_new(),
        dm_string_type: 0,
        has_dm_string_type: false,
        string_counter: 0,
        sret_counter: 0,
        string_ptrs: Map_new(),
        wrapper_returns_string: Map_new(),
        has_wrapper: Map_new(),
        returns_sret: Map_new(),
        ir_module: ir_mod,
        current_func_name: ""
    }
}

fn llvm_gen_dispose(gen: LLVMGenerator) {
    dm_llvm_dispose_builder(gen.builder)
    dm_llvm_module_dispose(gen.llvm_module)
    dm_llvm_context_dispose(gen.context)
}

-- ============================================================
-- DM_STRING TYPE
-- ============================================================

fn get_dm_string_type(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    if g.has_dm_string_type {
        return g
    }
    let ctx = g.context
    let str_ty = dm_llvm_struct_create_named(ctx, "dm_string")
    -- Body: { ptr (data), i64 (len), i64 (capacity) }
    dm_llvm_buf_clear()
    dm_llvm_buf_push(dm_llvm_pointer_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_buf_push(dm_llvm_int64_type(ctx))
    dm_llvm_struct_set_body_buf(str_ty, 0)
    g.dm_string_type = str_ty
    g.has_dm_string_type = true
    return g
}

-- ============================================================
-- TYPE MAPPING
-- ============================================================

-- Check if a type name represents a float type
fn lg_is_float_type(type_name: string) -> bool {
    return type_name == "f32" or type_name == "f64"
}

-- Check if a type name represents an integer type
fn is_int_type(type_name: string) -> bool {
    if type_name == "i8" { return true }
    if type_name == "i16" { return true }
    if type_name == "i32" { return true }
    if type_name == "i64" { return true }
    if type_name == "u8" { return true }
    if type_name == "u16" { return true }
    if type_name == "u32" { return true }
    if type_name == "u64" { return true }
    return false
}

-- Check if a type name represents an unsigned integer type
fn lg_is_unsigned_type(type_name: string) -> bool {
    if type_name == "u8" { return true }
    if type_name == "u16" { return true }
    if type_name == "u32" { return true }
    if type_name == "u64" { return true }
    return false
}

-- Check if a type is a SIMD vector type
fn lg_is_simd_type(type_name: string) -> bool {
    if type_name == "f32x4" { return true }
    if type_name == "f32x8" { return true }
    if type_name == "f64x2" { return true }
    if type_name == "f64x4" { return true }
    if type_name == "i32x4" { return true }
    if type_name == "i32x8" { return true }
    if type_name == "i64x2" { return true }
    if type_name == "i64x4" { return true }
    return false
}

-- Map IR type name string to LLVM type ref
fn map_ir_type_to_llvm(gen: LLVMGenerator, type_name: string) -> int {
    let ctx = gen.context
    if type_name == "i8" { return dm_llvm_int8_type(ctx) }
    if type_name == "i16" { return dm_llvm_int16_type(ctx) }
    if type_name == "i32" { return dm_llvm_int32_type(ctx) }
    if type_name == "i64" { return dm_llvm_int64_type(ctx) }
    -- Unsigned types map to same LLVM int types (signedness is in instructions, not types)
    if type_name == "u8" { return dm_llvm_int8_type(ctx) }
    if type_name == "u16" { return dm_llvm_int16_type(ctx) }
    if type_name == "u32" { return dm_llvm_int32_type(ctx) }
    if type_name == "u64" { return dm_llvm_int64_type(ctx) }
    if type_name == "f32" { return dm_llvm_float_type(ctx) }
    if type_name == "f64" { return dm_llvm_double_type(ctx) }
    if type_name == "bool" { return dm_llvm_int1_type(ctx) }
    if type_name == "void" { return dm_llvm_void_type(ctx) }
    if type_name == "string" { return gen.dm_string_type }
    if type_name == "ptr" { return dm_llvm_pointer_type(ctx) }
    -- Pointer types: ptr(...)
    if starts_with(type_name, "ptr(") { return dm_llvm_pointer_type(ctx) }
    -- List types: use dm_string type {ptr, i64, i64} since lists share the same layout
    if starts_with(type_name, "dm_list_") or starts_with(type_name, "list") or type_name == "list" or starts_with(type_name, "List[") {
        if gen.has_dm_string_type { return gen.dm_string_type }
        return dm_llvm_pointer_type(ctx)
    }
    -- Map types: use [64 x i8] to ensure enough space for runtime map struct
    if starts_with(type_name, "dm_map_") or starts_with(type_name, "map") or type_name == "map" or starts_with(type_name, "Map[") {
        return dm_llvm_array_type(dm_llvm_int8_type(ctx), 64)
    }
    -- Result type: { i32 tag, ok_type, err_type }
    -- Parse inner types from Result[T, E] for correct LLVM struct layout
    if type_name == "result" or starts_with(type_name, "Result[") or starts_with(type_name, "result_") {
        let i32_ty = dm_llvm_int32_type(ctx)
        let mut ok_ty = dm_llvm_int64_type(ctx)
        let mut err_ty = dm_llvm_int64_type(ctx)
        if starts_with(type_name, "Result[") {
            -- Parse Result[T, E] -> extract T and E
            let inner = substr(type_name, 7, len(type_name) - 8)
            -- Find comma separating T and E, respecting nested brackets
            let mut comma_pos = 0 - 1
            let mut depth = 0
            let mut ci = 0
            while ci < len(inner) {
                let ch = char_at(inner, ci)
                if ch == "[" { depth = depth + 1 }
                if ch == "]" { depth = depth - 1 }
                if ch == "," and depth == 0 {
                    comma_pos = ci
                    ci = len(inner)
                }
                ci = ci + 1
            }
            if comma_pos >= 0 {
                let ok_str = substr(inner, 0, comma_pos)
                let mut err_start = comma_pos + 1
                if err_start < len(inner) and char_at(inner, err_start) == " " {
                    err_start = err_start + 1
                }
                let err_str = substr(inner, err_start, len(inner) - err_start)
                ok_ty = map_ir_type_to_llvm(gen, ok_str)
                err_ty = map_ir_type_to_llvm(gen, err_str)
            }
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(i32_ty)
        dm_llvm_buf_push(ok_ty)
        dm_llvm_buf_push(err_ty)
        return dm_llvm_struct_type_buf(ctx, 3, 0)
    }
    -- Option type: { i32 has_value, inner_type }
    -- Parse inner type from Option[T] for correct LLVM struct layout
    if type_name == "option" or starts_with(type_name, "Option[") or starts_with(type_name, "option_") {
        let i32_ty = dm_llvm_int32_type(ctx)
        let mut inner_ty = dm_llvm_int64_type(ctx)
        if starts_with(type_name, "Option[") {
            let inner = substr(type_name, 7, len(type_name) - 8)
            inner_ty = map_ir_type_to_llvm(gen, inner)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(i32_ty)
        dm_llvm_buf_push(inner_ty)
        return dm_llvm_struct_type_buf(ctx, 2, 0)
    }
    -- SIMD vector types
    if lg_is_simd_type(type_name) {
        return get_simd_llvm_type(gen, type_name)
    }
    -- Struct types: look up in struct_type_map
    if gen.struct_type_map.contains(type_name) {
        return gen.struct_type_map.get(type_name)
    }
    -- Struct names prefixed with "struct "
    if starts_with(type_name, "struct ") {
        let sname = substr(type_name, 7, len(type_name) - 7)
        if gen.struct_type_map.contains(sname) {
            return gen.struct_type_map.get(sname)
        }
    }
    -- Fallback: i64 (default integer type)
    return dm_llvm_int64_type(ctx)
}

-- ============================================================
-- STRUCT TYPE DECLARATION
-- ============================================================

fn declare_struct_types(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    -- Phase 1: Create opaque named struct types (forward declarations)
    let struct_keys = g.ir_module.struct_defs.keys()
    let mut i = 0
    while i < struct_keys.len() {
        let name = struct_keys[i]
        let ir_ty = g.ir_module.struct_defs.get(name)
        if ir_ty.kind == IR_TYPE_STRUCT() {
            let sname = ir_ty.name
            if sname != "" {
                let llvm_struct = dm_llvm_struct_create_named(ctx, sname)
                g.struct_type_map.insert(sname, llvm_struct)
            }
        }
        i = i + 1
    }
    -- Phase 2: Set struct bodies (all types now registered)
    i = 0
    while i < struct_keys.len() {
        let name = struct_keys[i]
        let ir_ty = g.ir_module.struct_defs.get(name)
        if ir_ty.kind == IR_TYPE_STRUCT() {
            let sname = ir_ty.name
            if sname != "" and g.struct_type_map.contains(sname) {
                let llvm_struct = g.struct_type_map.get(sname)
                -- Collect field types into list first (avoid buffer clobbering)
                let mut field_types: List[int] = []
                let mut fi = 0
                while fi < ir_ty.fields.len() {
                    let field = ir_ty.fields[fi]
                    field_types.push(map_ir_type_to_llvm(g, field.type_id))
                    fi = fi + 1
                }
                if field_types.len() > 0 {
                    dm_llvm_buf_clear()
                    fi = 0
                    while fi < field_types.len() {
                        dm_llvm_buf_push(field_types[fi])
                        fi = fi + 1
                    }
                    dm_llvm_struct_set_body_buf(llvm_struct, 0)
                }
            }
        }
        i = i + 1
    }
    return g
}

-- ============================================================
-- GLOBAL VARIABLES
-- ============================================================

fn generate_globals(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    let mut i = 0
    while i < g.ir_module.globals.len() {
        let global = g.ir_module.globals[i]
        -- Skip comptime bookkeeping globals (array elements, markers)
        if starts_with(global.name, "__ct_arr_") {
            i = i + 1
            continue
        }
        -- Skip string-type globals (they are resolved inline by generate_identifier)
        if global.has_init and global.init_value.kind == VAL_CONST_STRING() {
            i = i + 1
            continue
        }
        let llvm_ty = map_ir_type_to_llvm(g, global.type_id)
        let gvar = dm_llvm_module_add_global(g.llvm_module, llvm_ty, global.name)
        -- Set initializer
        if global.has_init {
            let init = global.init_value
            if init.kind == VAL_CONST_INT() {
                dm_llvm_set_initializer(gvar, dm_llvm_const_int(llvm_ty, init.int_val, 1))
            } else if init.kind == VAL_CONST_FLOAT() {
                dm_llvm_set_initializer(gvar, dm_llvm_const_real(llvm_ty, init.float_val))
            } else if init.kind == VAL_CONST_BOOL() {
                let mut bval = 0
                if init.bool_val { bval = 1 }
                dm_llvm_set_initializer(gvar, dm_llvm_const_int(llvm_ty, bval, 0))
            } else {
                dm_llvm_set_initializer(gvar, dm_llvm_const_null(llvm_ty))
            }
        } else {
            dm_llvm_set_initializer(gvar, dm_llvm_const_null(llvm_ty))
        }
        if global.is_const {
            dm_llvm_set_global_constant(gvar, 1)
        }
        g.global_var_map.insert(global.name, gvar)
        i = i + 1
    }
    return g
}

-- ============================================================
-- FUNCTION DECLARATION
-- ============================================================

-- Check if a return type needs sret (struct return) convention.
-- Result ({i32, i64, i64} = 20 bytes) and Option ({i32, i64} = 12 bytes)
-- are too large for register return on x86_64 and cause LLVM errors.
fn is_sret_return_type(ret_type: string) -> bool {
    if ret_type == "string" {
        return true
    }
    if ret_type == "result" or starts_with(ret_type, "Result[") or starts_with(ret_type, "result_") {
        return true
    }
    if ret_type == "option" or starts_with(ret_type, "Option[") or starts_with(ret_type, "option_") {
        return true
    }
    return false
}

-- Check if an IR function involves dm_string in params or return type
fn func_needs_wrapper(func: IRFunction) -> bool {
    if func.is_extern == false { return false }
    if func.return_type == "string" { return true }
    let mut i = 0
    while i < func.params.len() {
        if func.params[i].type_id == "string" { return true }
        i = i + 1
    }
    return false
}

fn declare_wrapper(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    let returns_string = func.return_type == "string"
    let ptr_ty = dm_llvm_pointer_type(ctx)
    let void_ty = dm_llvm_void_type(ctx)
    -- Compute return type BEFORE using buffer (map_ir_type_to_llvm may use buffer internally)
    let mut ret_ty = 0
    if returns_string {
        ret_ty = void_ty
    } else {
        ret_ty = map_ir_type_to_llvm(g, func.return_type)
    }
    -- Collect param types into a list first (avoid buffer clobbering)
    let mut param_types: List[int] = []
    if returns_string {
        param_types.push(ptr_ty)
    }
    let mut i = 0
    while i < func.params.len() {
        let p = func.params[i]
        if p.type_id == "string" {
            param_types.push(ptr_ty)
        } else {
            param_types.push(map_ir_type_to_llvm(g, p.type_id))
        }
        i = i + 1
    }
    -- Push all to shared buffer at once and create function type
    dm_llvm_buf_clear()
    i = 0
    while i < param_types.len() {
        dm_llvm_buf_push(param_types[i])
        i = i + 1
    }
    let fn_ty = dm_llvm_function_type_buf(ret_ty, 0)
    -- Wrapper name: "llvm_" + original name
    let wrapper_name = "llvm_" + func.name
    let llvm_fn = dm_llvm_module_add_function(g.llvm_module, wrapper_name, fn_ty)
    -- Store under ORIGINAL name for call resolution
    g.function_map.insert(func.name, llvm_fn)
    g.fn_type_map.insert(func.name, fn_ty)
    g.has_wrapper.insert(func.name, true)
    g.wrapper_returns_string.insert(func.name, returns_string)
    return g
}

-- Declare and generate a wrapper for a user-declared extern function that uses dm_string.
-- For `extern fn getenv(name: string) -> string`:
-- 1. Declare the real C function `getenv` as `ptr getenv(ptr)`
-- 2. Generate a wrapper `llvm_getenv` that converts dm_string <-> char*
fn declare_user_extern_wrapper(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    let returns_string = func.return_type == "string"
    let ptr_ty = dm_llvm_pointer_type(ctx)
    let void_ty = dm_llvm_void_type(ctx)

    -- Step 1: Declare the real C function with C types
    let mut c_param_count = 0
    dm_llvm_buf_clear()
    let mut ci = 0
    while ci < func.params.len() {
        let p = func.params[ci]
        if p.type_id == "string" {
            dm_llvm_buf_push(ptr_ty)
        } else {
            dm_llvm_buf_push(map_ir_type_to_llvm(g, p.type_id))
        }
        c_param_count = c_param_count + 1
        ci = ci + 1
    }
    let c_ret_ty = if returns_string { ptr_ty } else { map_ir_type_to_llvm(g, func.return_type) }
    let c_fn_ty = dm_llvm_function_type_buf(c_ret_ty, 0)
    let real_fn = dm_llvm_module_add_function(g.llvm_module, func.name, c_fn_ty)

    -- Step 2: Declare the wrapper function with dm_string-based API
    let mut w_param_count = 0
    dm_llvm_buf_clear()
    if returns_string {
        dm_llvm_buf_push(ptr_ty)
        w_param_count = w_param_count + 1
    }
    ci = 0
    while ci < func.params.len() {
        dm_llvm_buf_push(ptr_ty)
        w_param_count = w_param_count + 1
        ci = ci + 1
    }
    let w_ret_ty = if returns_string { void_ty } else { map_ir_type_to_llvm(g, func.return_type) }
    let w_fn_ty = dm_llvm_function_type_buf(w_ret_ty, 0)
    let wrapper_name = "llvm_" + func.name
    let wrapper_fn = dm_llvm_module_add_function(g.llvm_module, wrapper_name, w_fn_ty)

    -- Step 3: Generate the wrapper body
    let saved_bb = dm_llvm_get_insert_block(g.builder)
    let entry_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "entry")
    dm_llvm_position_at_end(g.builder, entry_bb)

    -- Convert string params: extract data field (field 0) from dm_string*
    g = get_dm_string_type(g)
    let mut w_param_idx = 0
    if returns_string { w_param_idx = 1 }

    dm_llvm_buf_clear()
    ci = 0
    while ci < func.params.len() {
        let p = func.params[ci]
        let wrapper_param = dm_llvm_get_param(wrapper_fn, w_param_idx)
        if p.type_id == "string" {
            let data_ptr = dm_llvm_build_struct_gep2(g.builder, g.dm_string_type, wrapper_param, 0, "data_ptr")
            let data = dm_llvm_build_load2(g.builder, ptr_ty, data_ptr, "data")
            dm_llvm_buf_push(data)
        } else {
            dm_llvm_buf_push(wrapper_param)
        }
        w_param_idx = w_param_idx + 1
        ci = ci + 1
    }
    let call_name = if returns_string { "c_result" } else { "" }
    let c_result = dm_llvm_build_call2_buf(g.builder, c_fn_ty, real_fn, call_name)

    if returns_string {
        let sret_ptr = dm_llvm_get_param(wrapper_fn, 0)
        -- Handle NULL return: call dm_string_new with empty string if NULL
        let null_val = dm_llvm_const_null(ptr_ty)
        let is_null = dm_llvm_build_icmp(g.builder, dm_llvm_int_eq(), c_result, null_val, "is_null")
        let then_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "then_null")
        let else_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "else_nonnull")
        let merge_bb = dm_llvm_append_basic_block(ctx, wrapper_fn, "merge")
        dm_llvm_build_cond_br(g.builder, is_null, then_bb, else_bb)

        -- Null case: create empty string
        dm_llvm_position_at_end(g.builder, then_bb)
        let empty_str = dm_llvm_build_global_string_ptr(g.builder, "", "empty_str")
        if g.function_map.contains("dm_string_new") {
            let sn_fn = g.function_map.get("dm_string_new")
            let sn_ty = g.fn_type_map.get("dm_string_new")
            dm_llvm_buf_clear()
            dm_llvm_buf_push(sret_ptr)
            dm_llvm_buf_push(empty_str)
            dm_llvm_build_call2_buf(g.builder, sn_ty, sn_fn, "")
        }
        dm_llvm_build_br(g.builder, merge_bb)

        -- Non-null case: create string from C result
        dm_llvm_position_at_end(g.builder, else_bb)
        if g.function_map.contains("dm_string_new") {
            let sn_fn = g.function_map.get("dm_string_new")
            let sn_ty = g.fn_type_map.get("dm_string_new")
            dm_llvm_buf_clear()
            dm_llvm_buf_push(sret_ptr)
            dm_llvm_buf_push(c_result)
            dm_llvm_build_call2_buf(g.builder, sn_ty, sn_fn, "")
        }
        dm_llvm_build_br(g.builder, merge_bb)

        dm_llvm_position_at_end(g.builder, merge_bb)
        dm_llvm_build_ret_void(g.builder)
    } else {
        dm_llvm_build_ret(g.builder, c_result)
    }

    -- Restore builder position
    if saved_bb != 0 {
        dm_llvm_position_at_end(g.builder, saved_bb)
    }

    -- Store under ORIGINAL name for call resolution
    g.function_map.insert(func.name, wrapper_fn)
    g.fn_type_map.insert(func.name, w_fn_ty)
    g.has_wrapper.insert(func.name, true)
    g.wrapper_returns_string.insert(func.name, returns_string)
    return g
}

fn lg_declare_function(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    -- Check if wrapper is needed for extern functions with string params/return
    if func_needs_wrapper(func) {
        -- Check if this is a user extern function
        if g.ir_module.user_extern_fns.contains(func.name) {
            g = declare_user_extern_wrapper(g, func)
            return g
        }
        g = declare_wrapper(g, func)
        return g
    }
    -- Regular function declaration
    -- Check if function returns result/option (needs sret convention)
    let needs_sret = is_sret_return_type(func.return_type) and func.name != "main"
    let is_main = func.name == "main"
    -- IMPORTANT: Compute return type and collect param types BEFORE using the shared
    -- buffer, because map_ir_type_to_llvm may use dm_llvm_buf internally for struct types
    let mut ret_ty = 0
    if is_main {
        ret_ty = dm_llvm_int32_type(ctx)
    } else if needs_sret {
        ret_ty = dm_llvm_void_type(ctx)
    } else {
        ret_ty = map_ir_type_to_llvm(g, func.return_type)
    }
    -- Collect all param types into a list first (avoid buffer clobbering)
    let mut param_types: List[int] = []
    if needs_sret {
        param_types.push(dm_llvm_pointer_type(ctx))
    }
    let mut i = 0
    while i < func.params.len() {
        param_types.push(map_ir_type_to_llvm(g, func.params[i].type_id))
        i = i + 1
    }
    if is_main {
        param_types.push(dm_llvm_int32_type(ctx))
        param_types.push(dm_llvm_pointer_type(ctx))
    }
    -- Now push all to shared buffer at once and create function type
    dm_llvm_buf_clear()
    i = 0
    while i < param_types.len() {
        dm_llvm_buf_push(param_types[i])
        i = i + 1
    }
    let fn_ty = dm_llvm_function_type_buf(ret_ty, 0)
    let llvm_fn = dm_llvm_module_add_function(g.llvm_module, func.name, fn_ty)
    g.function_map.insert(func.name, llvm_fn)
    g.fn_type_map.insert(func.name, fn_ty)
    -- Track sret functions
    if needs_sret {
        g.returns_sret.insert(func.name, true)
    }
    -- Name parameters (offset by 1 for sret functions since param 0 is the sret ptr)
    i = 0
    if needs_sret {
        let sret_param = dm_llvm_get_param(llvm_fn, 0)
        dm_llvm_set_value_name(sret_param, "sret.out")
        while i < func.params.len() {
            let param_val = dm_llvm_get_param(llvm_fn, i + 1)
            dm_llvm_set_value_name(param_val, func.params[i].name)
            i = i + 1
        }
    } else {
        while i < func.params.len() {
            let param_val = dm_llvm_get_param(llvm_fn, i)
            dm_llvm_set_value_name(param_val, func.params[i].name)
            i = i + 1
        }
    }
    return g
}

-- ============================================================
-- FUNCTION BODY GENERATION
-- ============================================================

fn lg_generate_function(gen: LLVMGenerator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let ctx = g.context
    if g.function_map.contains(func.name) == false { return g }
    let llvm_fn = g.function_map.get(func.name)
    g.current_func_name = func.name
    -- Create basic blocks
    -- Clear block_map for this function
    g.block_map = Map_new()
    let mut bi = 0
    while bi < func.blocks.len() {
        let block = func.blocks[bi]
        let bb = dm_llvm_append_basic_block(ctx, llvm_fn, block.label)
        g.block_map.insert(block.label, bb)
        bi = bi + 1
    }
    -- Clear value_map and string_ptrs for this function
    g.value_map = Map_new()
    g.string_ptrs = Map_new()
    -- ALLOCA HOISTING: Generate all allocas in the entry block first.
    -- This ensures all allocas are available regardless of block processing order.
    -- Standard LLVM practice: all allocas belong in the entry block.
    if func.blocks.len() > 0 {
        let entry_label = func.blocks[0].label
        if g.block_map.contains(entry_label) {
            let entry_bb = g.block_map.get(entry_label)
            dm_llvm_position_at_end(g.builder, entry_bb)
            -- Pre-generate argc/argv for main
            if func.name == "main" {
                let i32_ty = dm_llvm_int32_type(ctx)
                let ptr_ty = dm_llvm_pointer_type(ctx)
                let argc_global = dm_llvm_module_add_global(g.llvm_module, i32_ty, "dm_argc")
                dm_llvm_set_linkage(argc_global, dm_llvm_external_linkage())
                let argv_global = dm_llvm_module_add_global(g.llvm_module, ptr_ty, "dm_argv")
                dm_llvm_set_linkage(argv_global, dm_llvm_external_linkage())
                let argc_param = dm_llvm_get_param(llvm_fn, func.params.len())
                let argv_param = dm_llvm_get_param(llvm_fn, func.params.len() + 1)
                dm_llvm_build_store(g.builder, argc_param, argc_global)
                dm_llvm_build_store(g.builder, argv_param, argv_global)
            }
            -- Scan ALL blocks for OP_ALLOCA and generate them in entry block
            bi = 0
            while bi < func.blocks.len() {
                let scan_block = func.blocks[bi]
                let mut si = 0
                while si < scan_block.instructions.len() {
                    let scan_inst = scan_block.instructions[si]
                    if scan_inst.op == OP_ALLOCA() {
                        g = generate_instruction(g, scan_inst)
                    }
                    si = si + 1
                }
                bi = bi + 1
            }
        }
    }
    -- Build BFS-ordered block list for correct processing order.
    -- Blocks must be processed in dominator order so that value_map is populated
    -- before any instruction references a value from an earlier block.
    let mut bfs_order: List[string] = []
    let mut bfs_visited: Map[string, bool] = Map_new()
    if func.blocks.len() > 0 {
        -- Start BFS from entry block
        let mut bfs_queue: List[string] = []
        bfs_queue.push(func.blocks[0].label)
        bfs_visited.insert(func.blocks[0].label, true)
        while bfs_queue.len() > 0 {
            let cur_label = bfs_queue[0]
            -- Shift: manual removal of first element by rebuilding
            let mut new_queue: List[string] = []
            let mut qi = 1
            while qi < bfs_queue.len() {
                new_queue.push(bfs_queue[qi])
                qi = qi + 1
            }
            bfs_queue = new_queue
            bfs_order.push(cur_label)
            -- Find this block and get its terminator targets
            let mut fi = 0
            while fi < func.blocks.len() {
                if func.blocks[fi].label == cur_label {
                    let cur_block = func.blocks[fi]
                    if cur_block.has_terminator {
                        let t = *cur_block.terminator
                        if t.kind == TERM_BR() {
                            if bfs_visited.contains(t.target) == false {
                                bfs_visited.insert(t.target, true)
                                bfs_queue.push(t.target)
                            }
                        }
                        if t.kind == TERM_BR_COND() {
                            if bfs_visited.contains(t.true_label) == false {
                                bfs_visited.insert(t.true_label, true)
                                bfs_queue.push(t.true_label)
                            }
                            if bfs_visited.contains(t.false_label) == false {
                                bfs_visited.insert(t.false_label, true)
                                bfs_queue.push(t.false_label)
                            }
                        }
                    }
                }
                fi = fi + 1
            }
        }
        -- Add any blocks not reachable from entry (safety: shouldn't happen)
        bi = 0
        while bi < func.blocks.len() {
            if bfs_visited.contains(func.blocks[bi].label) == false {
                bfs_order.push(func.blocks[bi].label)
            }
            bi = bi + 1
        }
    }
    -- Generate instructions and terminators for each block in BFS order (skip allocas)
    let mut bfs_i = 0
    while bfs_i < bfs_order.len() {
        let bfs_label = bfs_order[bfs_i]
        -- Find the block with this label
        let mut found_bi = 0
        while found_bi < func.blocks.len() {
            if func.blocks[found_bi].label == bfs_label {
                let block = func.blocks[found_bi]
                if g.block_map.contains(block.label) {
                    let bb = g.block_map.get(block.label)
                    dm_llvm_position_at_end(g.builder, bb)
                    -- Generate non-alloca instructions
                    let mut ii = 0
                    while ii < block.instructions.len() {
                        if block.instructions[ii].op != OP_ALLOCA() {
                            g = generate_instruction(g, block.instructions[ii])
                        }
                        ii = ii + 1
                    }
                    -- Generate terminator
                    if block.has_terminator {
                        let term = *block.terminator
                        g = generate_terminator(g, term, func)
                    }
                }
            }
            found_bi = found_bi + 1
        }
        bfs_i = bfs_i + 1
    }
    return g
}

-- ============================================================
-- VALUE RESOLUTION
-- ============================================================

fn resolve_value(gen: LLVMGenerator, val: IRValue) -> int {
    let ctx = gen.context
    if val.kind == VAL_CONST_INT() {
        return dm_llvm_const_int(dm_llvm_int64_type(ctx), val.int_val, 1)
    }
    if val.kind == VAL_CONST_FLOAT() {
        return dm_llvm_const_real(dm_llvm_double_type(ctx), val.float_val)
    }
    if val.kind == VAL_CONST_BOOL() {
        let mut bval = 0
        if val.bool_val { bval = 1 }
        return dm_llvm_const_int(dm_llvm_int1_type(ctx), bval, 0)
    }
    if val.kind == VAL_CONST_STRING() {
        -- Build a global string pointer for the raw C string data
        let name = ".str." + int_to_string(gen.string_counter)
        return dm_llvm_build_global_string_ptr(gen.builder, val.str_val, name)
    }
    if val.kind == VAL_INST_REF() {
        let key = int_to_string(val.ref_id)
        if gen.value_map.contains(key) {
            return gen.value_map.get(key)
        }
        -- Fallback: return null
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    if val.kind == VAL_PARAM_REF() {
        if gen.function_map.contains(gen.current_func_name) {
            let llvm_fn = gen.function_map.get(gen.current_func_name)
            -- For sret functions, LLVM param 0 is the sret pointer, so user params start at index 1
            let mut param_offset = 0
            if gen.returns_sret.contains(gen.current_func_name) {
                param_offset = 1
            }
            return dm_llvm_get_param(llvm_fn, val.ref_id + param_offset)
        }
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    if val.kind == VAL_GLOBAL_REF() {
        -- Check functions first, then global variables
        if gen.function_map.contains(val.str_val) {
            return gen.function_map.get(val.str_val)
        }
        if gen.global_var_map.contains(val.str_val) {
            let gvar = gen.global_var_map.get(val.str_val)
            -- Load the value from the global variable
            return dm_llvm_build_load2(gen.builder, dm_llvm_int64_type(ctx), gvar, "gload")
        }
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    if val.kind == VAL_UNDEF() {
        return dm_llvm_const_null(dm_llvm_int64_type(ctx))
    }
    return dm_llvm_const_null(dm_llvm_int64_type(ctx))
}

-- Resolve value for const_string, incrementing string counter
fn resolve_value_with_counter(gen: LLVMGenerator, val: IRValue) -> LLVMGenerator {
    let mut g = gen
    if val.kind == VAL_CONST_STRING() {
        g.string_counter = g.string_counter + 1
    }
    return g
}

-- Check if an IR value is a string pointer (alloca to dm_string)
fn is_string_ptr(gen: LLVMGenerator, val: IRValue) -> bool {
    if val.kind == VAL_INST_REF() {
        let key = int_to_string(val.ref_id)
        if gen.string_ptrs.contains(key) {
            return gen.string_ptrs.get(key)
        }
    }
    return false
}

-- ============================================================
-- INSTRUCTION GENERATION
-- ============================================================

fn generate_instruction(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let op = inst.op
    let id_key = int_to_string(inst.id)
    -- Arithmetic
    if op == OP_ADD() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fadd(b, lhs, rhs, "fadd")
        } else {
            result = dm_llvm_build_add(b, lhs, rhs, "add")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_SUB() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fsub(b, lhs, rhs, "fsub")
        } else {
            result = dm_llvm_build_sub(b, lhs, rhs, "sub")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_MUL() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fmul(b, lhs, rhs, "fmul")
        } else {
            result = dm_llvm_build_mul(b, lhs, rhs, "mul")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_DIV() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fdiv(b, lhs, rhs, "fdiv")
        } else {
            result = dm_llvm_build_sdiv(b, lhs, rhs, "div")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_MOD() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_frem(b, lhs, rhs, "frem")
        } else {
            result = dm_llvm_build_srem(b, lhs, rhs, "mod")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_NEG() {
        let operand = resolve_value(g, inst.operand)
        g = resolve_value_with_counter(g, inst.operand)
        let mut result = 0
        if lg_is_float_type(inst.result_type) {
            result = dm_llvm_build_fneg(b, operand, "fneg")
        } else {
            result = dm_llvm_build_neg(b, operand, "neg")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Comparison
    if op == OP_EQ() {
        let mut lhs = resolve_value(g, inst.lhs)
        let mut rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        -- Fix i32 vs i64 type mismatch for icmp (e.g., enum/option/result tag comparisons)
        let lhs_ty = dm_llvm_type_of(lhs)
        let rhs_ty = dm_llvm_type_of(rhs)
        if lhs_ty != rhs_ty {
            let lhs_kind = dm_llvm_get_type_kind(lhs_ty)
            let rhs_kind = dm_llvm_get_type_kind(rhs_ty)
            -- type kind 8 = LLVMIntegerTypeKind
            if lhs_kind == 8 and rhs_kind == 8 {
                let lw = dm_llvm_get_int_type_width(lhs_ty)
                let rw = dm_llvm_get_int_type_width(rhs_ty)
                if lw < rw {
                    rhs = dm_llvm_build_trunc(b, rhs, lhs_ty, "trunc")
                }
                if rw < lw {
                    lhs = dm_llvm_build_trunc(b, lhs, rhs_ty, "trunc")
                }
            }
        }
        let mut result = 0
        if lg_is_float_type(inst.result_type) or lg_is_float_type(inst.lhs.str_val) {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_oeq(), lhs, rhs, "feq")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_eq(), lhs, rhs, "eq")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_NE() {
        let mut lhs = resolve_value(g, inst.lhs)
        let mut rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        -- Fix i32 vs i64 type mismatch for icmp
        let lhs_ty = dm_llvm_type_of(lhs)
        let rhs_ty = dm_llvm_type_of(rhs)
        if lhs_ty != rhs_ty {
            let lhs_kind = dm_llvm_get_type_kind(lhs_ty)
            let rhs_kind = dm_llvm_get_type_kind(rhs_ty)
            if lhs_kind == 8 and rhs_kind == 8 {
                let lw = dm_llvm_get_int_type_width(lhs_ty)
                let rw = dm_llvm_get_int_type_width(rhs_ty)
                if lw < rw {
                    rhs = dm_llvm_build_trunc(b, rhs, lhs_ty, "trunc")
                }
                if rw < lw {
                    lhs = dm_llvm_build_trunc(b, lhs, rhs_ty, "trunc")
                }
            }
        }
        let mut result = 0
        if lg_is_float_type(inst.result_type) or lg_is_float_type(inst.lhs.str_val) {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_one(), lhs, rhs, "fne")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_ne(), lhs, rhs, "ne")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LT() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) or lg_is_float_type(inst.lhs.str_val) {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_olt(), lhs, rhs, "flt")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_slt(), lhs, rhs, "lt")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LE() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) or lg_is_float_type(inst.lhs.str_val) {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_ole(), lhs, rhs, "fle")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_sle(), lhs, rhs, "le")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_GT() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) or lg_is_float_type(inst.lhs.str_val) {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_ogt(), lhs, rhs, "fgt")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_sgt(), lhs, rhs, "gt")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_GE() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let mut result = 0
        if lg_is_float_type(inst.result_type) or lg_is_float_type(inst.lhs.str_val) {
            result = dm_llvm_build_fcmp(b, dm_llvm_real_oge(), lhs, rhs, "fge")
        } else {
            result = dm_llvm_build_icmp(b, dm_llvm_int_sge(), lhs, rhs, "ge")
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Logical operators
    if op == OP_LOGICAL_AND() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let result = dm_llvm_build_and(b, lhs, rhs, "and")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LOGICAL_OR() {
        let lhs = resolve_value(g, inst.lhs)
        let rhs = resolve_value(g, inst.rhs)
        g = resolve_value_with_counter(g, inst.lhs)
        g = resolve_value_with_counter(g, inst.rhs)
        let result = dm_llvm_build_or(b, lhs, rhs, "or")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    if op == OP_LOGICAL_NOT() {
        let operand = resolve_value(g, inst.operand)
        g = resolve_value_with_counter(g, inst.operand)
        let result = dm_llvm_build_not(b, operand, "not")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Memory: alloca
    if op == OP_ALLOCA() {
        let alloc_ty = map_ir_type_to_llvm(g, inst.alloc_type)
        let result = dm_llvm_build_alloca(b, alloc_ty, "alloca")
        if inst.alloc_type == "string" {
            g.string_ptrs.insert(id_key, true)
        }
        -- List and map types are also aggregate pointers (pass by pointer, not value)
        if starts_with(inst.alloc_type, "dm_list_") or starts_with(inst.alloc_type, "dm_map_") or starts_with(inst.alloc_type, "List[") or starts_with(inst.alloc_type, "Map[") {
            g.string_ptrs.insert(id_key, true)
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Memory: load
    if op == OP_LOAD() {
        if inst.load_type == "string" {
            -- String loads return the pointer itself (dm_string is always by pointer)
            let ptr_val = resolve_value(g, inst.load_ptr)
            g = resolve_value_with_counter(g, inst.load_ptr)
            g.string_ptrs.insert(id_key, true)
            if inst.has_result { g.value_map.insert(id_key, ptr_val) }
            return g
        }
        -- List/map loads return the pointer itself (aggregate types, always by pointer)
        if starts_with(inst.load_type, "dm_list_") or starts_with(inst.load_type, "dm_map_") or inst.load_type == "list" or inst.load_type == "map" or starts_with(inst.load_type, "List[") or starts_with(inst.load_type, "Map[") {
            let ptr_val = resolve_value(g, inst.load_ptr)
            g = resolve_value_with_counter(g, inst.load_ptr)
            g.string_ptrs.insert(id_key, true)
            if inst.has_result { g.value_map.insert(id_key, ptr_val) }
            return g
        }
        let load_ty = map_ir_type_to_llvm(g, inst.load_type)
        let ptr_val = resolve_value(g, inst.load_ptr)
        g = resolve_value_with_counter(g, inst.load_ptr)
        let result = dm_llvm_build_load2(b, load_ty, ptr_val, "load")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Memory: store
    if op == OP_STORE() {
        let dst = resolve_value(g, inst.store_ptr)
        g = resolve_value_with_counter(g, inst.store_ptr)
        -- Special case: storing const_string into a string alloca
        -- Must call dm_string_new to properly init {data, length, capacity}
        if inst.store_val.kind == VAL_CONST_STRING() {
            let raw_ptr = resolve_value(g, inst.store_val)
            g = resolve_value_with_counter(g, inst.store_val)
            if g.function_map.contains("dm_string_new") {
                let func = g.function_map.get("dm_string_new")
                let fn_ty = g.fn_type_map.get("dm_string_new")
                dm_llvm_buf_clear()
                dm_llvm_buf_push(dst)
                dm_llvm_buf_push(raw_ptr)
                dm_llvm_build_call2_buf(b, fn_ty, func, "")
            } else {
                dm_llvm_build_store(b, raw_ptr, dst)
            }
            return g
        }
        let src = resolve_value(g, inst.store_val)
        g = resolve_value_with_counter(g, inst.store_val)
        if is_string_ptr(g, inst.store_val) {
            -- String copy: load struct from source alloca, store to dest alloca
            let str_ty = g.dm_string_type
            let loaded = dm_llvm_build_load2(b, str_ty, src, "strcpy")
            dm_llvm_build_store(b, loaded, dst)
        } else {
            dm_llvm_build_store(b, src, dst)
        }
        return g
    }
    -- Struct: extract field
    if op == OP_EXTRACT_FIELD() {
        let base = resolve_value(g, inst.field_base)
        g = resolve_value_with_counter(g, inst.field_base)
        let result = dm_llvm_build_extract_value(b, base, inst.field_index, "field")
        -- If extracted field is dm_string, store to alloca and track
        if inst.field_type == "string" {
            let str_alloca = dm_llvm_build_alloca(b, g.dm_string_type, "str.extract")
            dm_llvm_build_store(b, result, str_alloca)
            g.string_ptrs.insert(id_key, true)
            if inst.has_result { g.value_map.insert(id_key, str_alloca) }
            return g
        }
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Struct: insert field
    if op == OP_INSERT_FIELD() {
        let mut base_val = 0
        if inst.field_base.kind == VAL_UNDEF() {
            -- Create undef of the result type
            let result_ty = map_ir_type_to_llvm(g, inst.result_type)
            base_val = dm_llvm_get_undef(result_ty)
        } else {
            base_val = resolve_value(g, inst.field_base)
            g = resolve_value_with_counter(g, inst.field_base)
        }
        let mut val = resolve_value(g, inst.field_value)
        g = resolve_value_with_counter(g, inst.field_value)
        -- Fix type mismatch: ir_val_int creates i64 constants, but some struct fields
        -- (e.g., Result/Option/Enum tag at index 0) are i32. Look up the actual field
        -- type from the IR struct definition and re-create the constant if needed.
        if inst.field_value.kind == VAL_CONST_INT() {
            let mut field_needs_i32 = false
            -- Check result/option types (inline struct, not in struct_defs)
            if is_sret_return_type(inst.result_type) and inst.field_index == 0 {
                field_needs_i32 = true
            }
            -- Check named struct types (enums have i32 tag at field 0)
            if g.ir_module.struct_defs.contains(inst.result_type) {
                let ir_struct = g.ir_module.struct_defs.get(inst.result_type)
                if inst.field_index < ir_struct.fields.len() {
                    let ft = ir_struct.fields[inst.field_index].type_id
                    if ft == "i32" or ft == "u32" {
                        field_needs_i32 = true
                    }
                }
            }
            if field_needs_i32 {
                val = dm_llvm_const_int(dm_llvm_int32_type(g.context), inst.field_value.int_val, 1)
            }
        }
        -- If inserting a string value (pointer to dm_string), load the actual struct
        if is_string_ptr(g, inst.field_value) {
            val = dm_llvm_build_load2(b, g.dm_string_type, val, "str.load")
        }
        let result = dm_llvm_build_insert_value(b, base_val, val, inst.field_index, "insert")
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Call: delegate to llvm_gen_call module
    if op == OP_CALL() {
        g = generate_call(g, inst)
        return g
    }
    if op == OP_CALL_PTR() {
        g = generate_call_ptr(g, inst)
        return g
    }
    -- Cast
    if op == OP_CAST() {
        let val = resolve_value(g, inst.cast_val)
        g = resolve_value_with_counter(g, inst.cast_val)
        let result = generate_cast(g, val, inst.cast_from, inst.cast_to)
        if inst.has_result { g.value_map.insert(id_key, result) }
        return g
    }
    -- Phi
    if op == OP_PHI() {
        let phi_ty = map_ir_type_to_llvm(g, inst.result_type)
        let phi_val = dm_llvm_build_phi(b, phi_ty, "phi")
        -- Add incoming values/blocks
        let mut pi = 0
        while pi < inst.phi_entries.len() {
            let entry = inst.phi_entries[pi]
            let incoming_val = resolve_value(g, entry.value)
            g = resolve_value_with_counter(g, entry.value)
            if g.block_map.contains(entry.block_label) {
                let incoming_block = g.block_map.get(entry.block_label)
                dm_llvm_add_incoming_one(phi_val, incoming_val, incoming_block)
            }
            pi = pi + 1
        }
        if inst.has_result { g.value_map.insert(id_key, phi_val) }
        return g
    }
    -- String concatenation
    if op == OP_STRING_CONCAT() {
        g = generate_string_concat(g, inst)
        return g
    }
    -- String equality
    if op == OP_STRING_EQ() {
        g = generate_string_eq(g, inst)
        return g
    }
    -- List operations
    if op == OP_LIST_NEW() {
        g = generate_list_new(g, inst)
        return g
    }
    if op == OP_LIST_PUSH() {
        g = generate_list_push(g, inst)
        return g
    }
    if op == OP_LIST_GET() {
        g = generate_list_get(g, inst)
        return g
    }
    if op == OP_LIST_LEN() {
        g = generate_list_len(g, inst)
        return g
    }
    if op == OP_LIST_POP() {
        g = generate_list_pop(g, inst)
        return g
    }
    -- Map operations
    if op == OP_MAP_NEW() {
        g = generate_map_new(g, inst)
        return g
    }
    if op == OP_MAP_INSERT() {
        g = generate_map_insert(g, inst)
        return g
    }
    if op == OP_MAP_GET() {
        g = generate_map_get(g, inst)
        return g
    }
    if op == OP_MAP_CONTAINS() {
        g = generate_map_contains(g, inst)
        return g
    }
    -- Panic
    if op == OP_PANIC() {
        g = generate_panic(g, inst)
        return g
    }
    -- SIMD: delegate to llvm_gen_simd module
    if op == OP_SIMD_SPLAT() or op == OP_SIMD_SET() or op == OP_SIMD_ADD() or op == OP_SIMD_SUB() or op == OP_SIMD_MUL() or op == OP_SIMD_DIV() or op == OP_SIMD_EXTRACT() {
        g = generate_simd_instruction(g, inst)
        return g
    }
    -- Unhandled op: skip
    return g
}

-- ============================================================
-- CAST GENERATION
-- ============================================================

fn generate_cast(gen: LLVMGenerator, val: int, from: string, to: string) -> int {
    let b = gen.builder
    let from_is_int = is_int_type(from)
    let to_is_int = is_int_type(to)
    let from_is_float = lg_is_float_type(from)
    let to_is_float = lg_is_float_type(to)
    -- Int to int: truncate or sign/zero-extend
    if from_is_int and to_is_int {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        let mut is_signed = 1
        if lg_is_unsigned_type(from) { is_signed = 0 }
        return dm_llvm_build_int_cast2(b, val, dest_ty, is_signed, "cast")
    }
    -- Int to float
    if from_is_int and to_is_float {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        if lg_is_unsigned_type(from) {
            return dm_llvm_build_ui_to_fp(b, val, dest_ty, "cast")
        }
        return dm_llvm_build_si_to_fp(b, val, dest_ty, "cast")
    }
    -- Float to int
    if from_is_float and to_is_int {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        return dm_llvm_build_fp_to_si(b, val, dest_ty, "cast")
    }
    -- Float to float
    if from_is_float and to_is_float {
        let dest_ty = map_ir_type_to_llvm(gen, to)
        return dm_llvm_build_fp_cast(b, val, dest_ty, "cast")
    }
    -- Int to pointer (inttoptr)
    if from_is_int and to == "ptr" {
        let dest_ty = dm_llvm_pointer_type(gen.context)
        return dm_llvm_build_int_to_ptr(b, val, dest_ty, "cast")
    }
    -- Fallback: return value unchanged
    return val
}

-- ============================================================
-- STRING OPERATIONS
-- ============================================================

fn generate_string_concat(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    -- Resolve LHS and RHS (both should be string pointers or const strings)
    let lhs = resolve_value(g, inst.lhs)
    let rhs = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Call dm_string_concat wrapper (takes two dm_string* pointers, writes result via sret)
    if g.function_map.contains("dm_string_concat") {
        let func = g.function_map.get("dm_string_concat")
        let fn_ty = g.fn_type_map.get("dm_string_concat")
        -- Allocate sret for result
        let sret_name = "sret." + int_to_string(g.sret_counter)
        g.sret_counter = g.sret_counter + 1
        let sret = dm_llvm_build_alloca(b, g.dm_string_type, sret_name)
        -- Create dm_string allocas for const string args if needed
        let mut lhs_ptr = lhs
        if inst.lhs.kind == VAL_CONST_STRING() {
            lhs_ptr = create_dm_string(g, lhs)
        }
        let mut rhs_ptr = rhs
        if inst.rhs.kind == VAL_CONST_STRING() {
            rhs_ptr = create_dm_string(g, rhs)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(sret)
        dm_llvm_buf_push(lhs_ptr)
        dm_llvm_buf_push(rhs_ptr)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
        g.string_ptrs.insert(id_key, true)
        if inst.has_result { g.value_map.insert(id_key, sret) }
    }
    return g
}

fn generate_string_eq(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let lhs = resolve_value(g, inst.lhs)
    let rhs = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Call dm_string_eq wrapper (takes two dm_string*, returns i1)
    if g.function_map.contains("dm_string_eq") {
        let func = g.function_map.get("dm_string_eq")
        let fn_ty = g.fn_type_map.get("dm_string_eq")
        let mut lhs_ptr = lhs
        if inst.lhs.kind == VAL_CONST_STRING() {
            lhs_ptr = create_dm_string(g, lhs)
        }
        let mut rhs_ptr = rhs
        if inst.rhs.kind == VAL_CONST_STRING() {
            rhs_ptr = create_dm_string(g, rhs)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(lhs_ptr)
        dm_llvm_buf_push(rhs_ptr)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "streq")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

-- Create a dm_string on the stack from a raw C string pointer
fn create_dm_string(gen: LLVMGenerator, raw_str: int) -> int {
    let b = gen.builder
    -- Allocate dm_string on stack
    let sret_name = "sret." + int_to_string(gen.sret_counter)
    let sret = dm_llvm_build_alloca(b, gen.dm_string_type, sret_name)
    -- Call dm_string_new wrapper
    if gen.function_map.contains("dm_string_new") {
        let func = gen.function_map.get("dm_string_new")
        let fn_ty = gen.fn_type_map.get("dm_string_new")
        dm_llvm_buf_clear()
        dm_llvm_buf_push(sret)
        dm_llvm_buf_push(raw_str)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return sret
}

-- ============================================================
-- LIST OPERATIONS
-- ============================================================

fn generate_list_new(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let id_key = int_to_string(inst.id)
    -- Call dm_list_new() runtime function
    if g.function_map.contains("dm_list_new") {
        let func = g.function_map.get("dm_list_new")
        let fn_ty = g.fn_type_map.get("dm_list_new")
        dm_llvm_buf_clear()
        let result = dm_llvm_build_call2_buf(g.builder, fn_ty, func, "list")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_list_push(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let list_val = resolve_value(g, inst.lhs)
    let elem_val = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Pick the correct runtime function based on element type
    let mut fn_name = "dm_list_int64_push"
    if inst.callee == "dm_list_double_push" or inst.result_type == "f64" {
        fn_name = "dm_list_double_push"
    } else if inst.callee == "dm_list_string_push" or inst.callee == "llvm_dm_list_string_push" or inst.result_type == "string" {
        fn_name = "dm_list_string_push"
    }
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(list_val)
        dm_llvm_buf_push(elem_val)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return g
}

fn generate_list_get(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let list_val = resolve_value(g, inst.lhs)
    let idx_val = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    -- Pick the correct runtime function based on element type
    let mut fn_name = "dm_list_int64_get"
    if inst.result_type == "f64" {
        fn_name = "dm_list_double_get"
    } else if inst.result_type == "string" {
        fn_name = "dm_list_string_get"
    }
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        if fn_name == "dm_list_string_get" {
            -- String list get uses sret pattern
            g = get_dm_string_type(g)
            let sret = dm_llvm_build_alloca(b, g.dm_string_type, "listget_sret")
            dm_llvm_buf_clear()
            dm_llvm_buf_push(sret)
            dm_llvm_buf_push(list_val)
            dm_llvm_buf_push(idx_val)
            dm_llvm_build_call2_buf(b, fn_ty, func, "")
            if inst.has_result { g.value_map.insert(id_key, sret) }
        } else {
            dm_llvm_buf_clear()
            dm_llvm_buf_push(list_val)
            dm_llvm_buf_push(idx_val)
            let result = dm_llvm_build_call2_buf(b, fn_ty, func, "listget")
            if inst.has_result { g.value_map.insert(id_key, result) }
        }
    }
    return g
}

fn generate_list_len(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let list_val = resolve_value(g, inst.operand)
    g = resolve_value_with_counter(g, inst.operand)
    -- Use type-specific len function (all return i64)
    let mut fn_name = "dm_list_int64_len"
    if inst.result_type == "f64" or inst.callee == "dm_list_double_len" {
        fn_name = "dm_list_double_len"
    } else if inst.result_type == "string" or inst.callee == "dm_list_string_len" {
        fn_name = "dm_list_string_len"
    } else if inst.callee == "dm_list_generic_len" {
        fn_name = "dm_list_generic_len"
    }
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(list_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "listlen")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_list_pop(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let list_val = resolve_value(g, inst.operand)
    g = resolve_value_with_counter(g, inst.operand)
    let fn_name = "dm_list_int64_pop"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(list_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "listpop")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

-- ============================================================
-- MAP OPERATIONS
-- ============================================================

fn generate_map_new(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let id_key = int_to_string(inst.id)
    let fn_name = "dm_map_new"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        let result = dm_llvm_build_call2_buf(g.builder, fn_ty, func, "map")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_map_insert(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let map_val = resolve_value(g, inst.map_val)
    let key_val = resolve_value(g, inst.map_key)
    let value_val = resolve_value(g, inst.map_value)
    g = resolve_value_with_counter(g, inst.map_val)
    g = resolve_value_with_counter(g, inst.map_key)
    g = resolve_value_with_counter(g, inst.map_value)
    let fn_name = "dm_map_insert"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(map_val)
        dm_llvm_buf_push(key_val)
        dm_llvm_buf_push(value_val)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return g
}

fn generate_map_get(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let map_val = resolve_value(g, inst.map_val)
    let key_val = resolve_value(g, inst.map_key)
    g = resolve_value_with_counter(g, inst.map_val)
    g = resolve_value_with_counter(g, inst.map_key)
    let fn_name = "dm_map_get"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(map_val)
        dm_llvm_buf_push(key_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "mapget")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

fn generate_map_contains(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let map_val = resolve_value(g, inst.map_val)
    let key_val = resolve_value(g, inst.map_key)
    g = resolve_value_with_counter(g, inst.map_val)
    g = resolve_value_with_counter(g, inst.map_key)
    let fn_name = "dm_map_contains"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        dm_llvm_buf_clear()
        dm_llvm_buf_push(map_val)
        dm_llvm_buf_push(key_val)
        let result = dm_llvm_build_call2_buf(b, fn_ty, func, "maphas")
        if inst.has_result { g.value_map.insert(id_key, result) }
    }
    return g
}

-- ============================================================
-- PANIC
-- ============================================================

fn generate_panic(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let msg = resolve_value(g, inst.operand)
    g = resolve_value_with_counter(g, inst.operand)
    let fn_name = "dm_panic"
    if g.function_map.contains(fn_name) {
        let func = g.function_map.get(fn_name)
        let fn_ty = g.fn_type_map.get(fn_name)
        -- If operand is const_string, wrap in dm_string
        let mut msg_val = msg
        if inst.operand.kind == VAL_CONST_STRING() {
            msg_val = create_dm_string(g, msg)
        }
        dm_llvm_buf_clear()
        dm_llvm_buf_push(msg_val)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    dm_llvm_build_unreachable(b)
    return g
}

-- ============================================================
-- TERMINATOR GENERATION
-- ============================================================

fn generate_terminator(gen: LLVMGenerator, term: IRTerminator, func: IRFunction) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    if term.kind == TERM_BR() {
        if g.block_map.contains(term.target) {
            let bb = g.block_map.get(term.target)
            dm_llvm_build_br(b, bb)
        }
        return g
    }
    if term.kind == TERM_BR_COND() {
        let cond = resolve_value(g, term.cond)
        g = resolve_value_with_counter(g, term.cond)
        if g.block_map.contains(term.true_label) and g.block_map.contains(term.false_label) {
            let true_bb = g.block_map.get(term.true_label)
            let false_bb = g.block_map.get(term.false_label)
            dm_llvm_build_cond_br(b, cond, true_bb, false_bb)
        }
        return g
    }
    if term.kind == TERM_RET() {
        let ctx = g.context
        let is_main = func.name == "main"
        if is_main {
            -- main() always returns i32 0
            let ret_val = dm_llvm_const_int(dm_llvm_int32_type(ctx), 0, 0)
            dm_llvm_build_ret(b, ret_val)
        } else if g.returns_sret.contains(func.name) {
            -- sret convention: store return value to the sret output pointer (param 0), then ret void
            let mut ret_val = resolve_value(g, term.ret_val)
            g = resolve_value_with_counter(g, term.ret_val)
            -- If returning a string pointer, load the struct before storing to sret
            if func.return_type == "string" and is_string_ptr(g, term.ret_val) {
                ret_val = dm_llvm_build_load2(b, g.dm_string_type, ret_val, "sret.str")
            }
            let llvm_fn = g.function_map.get(func.name)
            let sret_ptr = dm_llvm_get_param(llvm_fn, 0)
            dm_llvm_build_store(b, ret_val, sret_ptr)
            dm_llvm_build_ret_void(b)
        } else {
            -- If returning string pointer, load the struct first
            if func.return_type == "string" and is_string_ptr(g, term.ret_val) {
                let ptr_val = resolve_value(g, term.ret_val)
                g = resolve_value_with_counter(g, term.ret_val)
                let loaded = dm_llvm_build_load2(b, g.dm_string_type, ptr_val, "ret.str")
                dm_llvm_build_ret(b, loaded)
            } else {
                let ret_val = resolve_value(g, term.ret_val)
                g = resolve_value_with_counter(g, term.ret_val)
                dm_llvm_build_ret(b, ret_val)
            }
        }
        return g
    }
    if term.kind == TERM_RET_VOID() {
        let is_main = func.name == "main"
        if is_main {
            let ret_val = dm_llvm_const_int(dm_llvm_int32_type(g.context), 0, 0)
            dm_llvm_build_ret(b, ret_val)
        } else {
            dm_llvm_build_ret_void(b)
        }
        return g
    }
    if term.kind == TERM_UNREACHABLE() {
        dm_llvm_build_unreachable(b)
        return g
    }
    return g
}

-- ============================================================
-- TOP-LEVEL MODULE GENERATION
-- ============================================================

fn lg_generate_module(gen: LLVMGenerator) -> LLVMGenerator {
    let mut g = gen
    -- Phase 0: Create dm_string struct type
    g = get_dm_string_type(g)
    -- Phase 1: Declare struct types
    g = declare_struct_types(g)
    -- Phase 2: Generate global variables
    g = generate_globals(g)
    -- Phase 3: Declare all functions
    let mut i = 0
    while i < g.ir_module.functions.len() {
        g = lg_declare_function(g, g.ir_module.functions[i])
        i = i + 1
    }
    -- Phase 4: Generate function bodies (non-extern only)
    i = 0
    while i < g.ir_module.functions.len() {
        let func = g.ir_module.functions[i]
        if func.is_extern == false and func.blocks.len() > 0 {
            g = lg_generate_function(g, func)
        }
        i = i + 1
    }
    return g
}

-- ============================================================
-- EMIT: Object file, LLVM IR, verification
-- ============================================================

fn emit_to_object(gen: LLVMGenerator, filename: string, opt_level: int) -> int {
    dm_llvm_initialize_all_targets()
    let triple_ptr = dm_llvm_get_default_target_triple()
    -- Convert C string to dAImond string
    let triple = dm_llvm_c_str_to_string(triple_ptr)
    dm_llvm_free_c_string(triple_ptr)
    -- Create target machine
    let tm = dm_llvm_create_target_machine(triple, opt_level)
    if tm == 0 {
        eprintln("Error: Failed to create target machine")
        return 1
    }
    -- Set module target triple and data layout
    dm_llvm_module_set_target(gen.llvm_module, triple)
    let layout_ptr = dm_llvm_get_target_data_layout(tm)
    let layout = dm_llvm_c_str_to_string(layout_ptr)
    dm_llvm_free_c_string(layout_ptr)
    dm_llvm_module_set_data_layout(gen.llvm_module, layout)
    -- Run optimization passes if opt_level > 0
    if opt_level > 0 {
        let mut passes = "default<O1>"
        if opt_level == 2 { let passes = "default<O2>" }
        if opt_level >= 3 { let passes = "default<O3>" }
        dm_llvm_run_passes(gen.llvm_module, passes, tm)
    }
    -- Emit object file
    let result = dm_llvm_target_machine_emit_to_file(tm, gen.llvm_module, filename, dm_llvm_object_file())
    dm_llvm_dispose_target_machine(tm)
    return result
}

fn emit_to_ir_file(gen: LLVMGenerator, filename: string) -> int {
    return dm_llvm_module_print_to_file(gen.llvm_module, filename)
}

fn verify_module(gen: LLVMGenerator) -> int {
    return dm_llvm_module_verify(gen.llvm_module)
}


-- ============================================================
-- Call Instruction Handling for LLVM IR Generation
--
-- Handles OP_CALL and OP_CALL_PTR instructions, including:
-- - Direct function calls
-- - Wrapper-based string ABI calls (sret pattern for dm_string returns)
-- - Indirect calls through function pointers
-- ============================================================

-- ============================================================
-- DIRECT CALL (OP_CALL)
-- ============================================================

fn generate_call(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let callee_name = inst.callee
    -- Look up function in function_map
    if g.function_map.contains(callee_name) == false {
        -- Unknown function: skip
        return g
    }
    let callee = g.function_map.get(callee_name)
    let fn_ty = g.fn_type_map.get(callee_name)
    -- Check if this function has a wrapper (string ABI)
    if g.has_wrapper.contains(callee_name) {
        g = generate_wrapper_call(g, inst, callee, fn_ty, callee_name)
        return g
    }
    -- Check if callee uses sret return convention (result/option types)
    if g.returns_sret.contains(callee_name) {
        g = generate_sret_call(g, inst, callee, fn_ty, callee_name)
        return g
    }
    -- Regular (non-wrapper) call
    -- Resolve arguments, handling string pointer loads
    let mut args: List[int] = []
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let mut val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        -- If arg is a string pointer and target function expects dm_string by value,
        -- load the struct from the pointer
        if is_string_ptr(g, arg) {
            let expects_string = callee_param_is_string(g, callee_name, ai)
            if expects_string {
                val = dm_llvm_build_load2(b, g.dm_string_type, val, "str.arg")
            }
        }
        args.push(val)
        ai = ai + 1
    }
    -- Determine call result name (empty string for void returns)
    let mut call_name = ""
    if inst.has_result {
        call_name = "call"
    }
    -- Build the call
    dm_llvm_buf_clear()
    let mut argi = 0
    while argi < args.len() {
        dm_llvm_buf_push(args[argi])
        argi = argi + 1
    }
    let mut result = dm_llvm_build_call2_buf(b, fn_ty, callee, call_name)
    -- Check if callee returns dm_string: store to alloca and track as string ptr
    if callee_returns_string(g, callee_name) and result != 0 {
        let str_alloca = dm_llvm_build_alloca(b, g.dm_string_type, "str.ret")
        dm_llvm_build_store(b, result, str_alloca)
        g.string_ptrs.insert(id_key, true)
        if inst.has_result { g.value_map.insert(id_key, str_alloca) }
        return g
    }
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- WRAPPER CALL (string ABI via sret pattern)
-- ============================================================

fn generate_wrapper_call(gen: LLVMGenerator, inst: IRInst, callee: int, fn_ty: int, callee_name: string) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let returns_string = wrapper_returns_string_for(g, callee_name)
    if returns_string {
        -- Allocate stack space for the string result (sret pattern)
        let sret_name = "sret." + int_to_string(g.sret_counter)
        g.sret_counter = g.sret_counter + 1
        let sret = dm_llvm_build_alloca(b, g.dm_string_type, sret_name)
        -- Build args: sret pointer first, then resolved args
        let mut args: List[int] = []
        args.push(sret)
        let mut ai = 0
        while ai < inst.call_args.len() {
            let arg = inst.call_args[ai]
            let mut val = resolve_value(g, arg)
            g = resolve_value_with_counter(g, arg)
            -- If arg is a const_string and param expects dm_string, wrap it
            if arg.kind == VAL_CONST_STRING() {
                let param_is_str = orig_param_is_string(g, callee_name, ai)
                if param_is_str {
                    val = create_dm_string_for_call(g, val)
                }
            }
            args.push(val)
            ai = ai + 1
        }
        -- Call wrapper (returns void, result written to sret)
        dm_llvm_buf_clear()
        let mut argi2 = 0
        while argi2 < args.len() {
            dm_llvm_buf_push(args[argi2])
            argi2 = argi2 + 1
        }
        dm_llvm_build_call2_buf(b, fn_ty, callee, "")
        -- The sret alloca IS the result -- a pointer to dm_string
        g.string_ptrs.insert(id_key, true)
        if inst.has_result { g.value_map.insert(id_key, sret) }
        return g
    }
    -- Non-string return, but params may need wrapping
    let mut args: List[int] = []
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let mut val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        -- Wrap const_string args if parameter is string type
        if arg.kind == VAL_CONST_STRING() {
            let param_is_str = orig_param_is_string(g, callee_name, ai)
            if param_is_str {
                val = create_dm_string_for_call(g, val)
            }
        }
        args.push(val)
        ai = ai + 1
    }
    let mut call_name = ""
    if inst.has_result { call_name = "call" }
    dm_llvm_buf_clear()
    let mut argi3 = 0
    while argi3 < args.len() {
        dm_llvm_buf_push(args[argi3])
        argi3 = argi3 + 1
    }
    let result = dm_llvm_build_call2_buf(b, fn_ty, callee, call_name)
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- SRET CALL (result/option return via output pointer)
-- ============================================================

fn generate_sret_call(gen: LLVMGenerator, inst: IRInst, callee: int, fn_ty: int, callee_name: string) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    -- Allocate stack space for the result struct (sret pattern)
    let sret_name = "sret." + int_to_string(g.sret_counter)
    g.sret_counter = g.sret_counter + 1
    -- Look up the IR function to get its return type for correct alloca sizing
    let ir_func = find_ir_function(g, callee_name)
    let ret_llvm_ty = map_ir_type_to_llvm(g, ir_func.return_type)
    let sret = dm_llvm_build_alloca(b, ret_llvm_ty, sret_name)
    -- Build args: sret pointer first, then resolved args
    let mut args: List[int] = []
    args.push(sret)
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let mut val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        -- If arg is a string pointer and target function expects dm_string by value, load it
        if is_string_ptr(g, arg) {
            let expects_string = callee_param_is_string(g, callee_name, ai)
            if expects_string {
                val = dm_llvm_build_load2(b, g.dm_string_type, val, "str.arg")
            }
        }
        args.push(val)
        ai = ai + 1
    }
    -- Call function (returns void, result written to sret pointer)
    dm_llvm_buf_clear()
    let mut argi = 0
    while argi < args.len() {
        dm_llvm_buf_push(args[argi])
        argi = argi + 1
    }
    dm_llvm_build_call2_buf(b, fn_ty, callee, "")
    -- Load the result from the sret alloca
    if inst.has_result {
        -- If the function returns string, keep the sret alloca as a pointer (string_ptrs convention)
        if ir_func.return_type == "string" {
            g.string_ptrs.insert(id_key, true)
            g.value_map.insert(id_key, sret)
        } else {
            let loaded = dm_llvm_build_load2(b, ret_llvm_ty, sret, "sret.load")
            g.value_map.insert(id_key, loaded)
        }
    }
    return g
}

-- ============================================================
-- INDIRECT CALL (OP_CALL_PTR)
-- ============================================================

fn generate_call_ptr(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    -- Resolve the function pointer value
    let fn_ptr = resolve_value(g, inst.callee_val)
    g = resolve_value_with_counter(g, inst.callee_val)
    -- Build the LLVM function type for the indirect call
    -- Map the return type
    let ret_ty = map_ir_type_to_llvm(g, inst.result_type)
    -- Build param types and args simultaneously
    dm_llvm_buf_clear()
    let mut args: List[int] = []
    let mut ai = 0
    while ai < inst.call_args.len() {
        let arg = inst.call_args[ai]
        let val = resolve_value(g, arg)
        g = resolve_value_with_counter(g, arg)
        args.push(val)
        -- Get the LLVM type of the resolved value
        let val_ty = dm_llvm_type_of(val)
        dm_llvm_buf_push(val_ty)
        ai = ai + 1
    }
    -- Create function type
    let fn_ty = dm_llvm_function_type_buf(ret_ty, 0)
    -- Build the indirect call
    let mut call_name = ""
    if inst.has_result { call_name = "callptr" }
    dm_llvm_buf_clear()
    let mut argi4 = 0
    while argi4 < args.len() {
        dm_llvm_buf_push(args[argi4])
        argi4 = argi4 + 1
    }
    let result = dm_llvm_build_call2_buf(b, fn_ty, fn_ptr, call_name)
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- HELPER FUNCTIONS
-- ============================================================

-- Check if a function requires a string wrapper
fn needs_string_wrapper(gen: LLVMGenerator, fn_name: string) -> bool {
    return gen.has_wrapper.contains(fn_name)
}

-- Get the wrapper function name
fn get_wrapper_name(fn_name: string) -> string {
    return "llvm_" + fn_name
}

-- Check if wrapper returns string (sret pattern)
fn wrapper_returns_string_for(gen: LLVMGenerator, fn_name: string) -> bool {
    if gen.wrapper_returns_string.contains(fn_name) {
        return gen.wrapper_returns_string.get(fn_name)
    }
    return false
}

-- Check if a specific parameter of a callee function is string type
-- Looks up the IR function definition
fn callee_param_is_string(gen: LLVMGenerator, fn_name: string, param_idx: int) -> bool {
    -- Search through IR module functions
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        let func = gen.ir_module.functions[i]
        if func.name == fn_name {
            if param_idx < func.params.len() {
                return func.params[param_idx].type_id == "string"
            }
            return false
        }
        i = i + 1
    }
    return false
}

-- Check if a callee function returns string type
fn callee_returns_string(gen: LLVMGenerator, fn_name: string) -> bool {
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        let func = gen.ir_module.functions[i]
        if func.name == fn_name {
            return func.return_type == "string"
        }
        i = i + 1
    }
    return false
}

-- Check if the original (pre-wrapper) function parameter is string type
-- For wrapper functions, look up using the original name
fn orig_param_is_string(gen: LLVMGenerator, fn_name: string, param_idx: int) -> bool {
    return callee_param_is_string(gen, fn_name, param_idx)
}

-- Create a dm_string on the stack from a raw C string pointer (for call args)
fn create_dm_string_for_call(gen: LLVMGenerator, raw_str: int) -> int {
    let b = gen.builder
    let sret_name = "sret.arg." + int_to_string(gen.sret_counter)
    let sret = dm_llvm_build_alloca(b, gen.dm_string_type, sret_name)
    if gen.function_map.contains("dm_string_new") {
        let func = gen.function_map.get("dm_string_new")
        let fn_ty = gen.fn_type_map.get("dm_string_new")
        dm_llvm_buf_clear()
        dm_llvm_buf_push(sret)
        dm_llvm_buf_push(raw_str)
        dm_llvm_build_call2_buf(b, fn_ty, func, "")
    }
    return sret
}

-- Find an IR function by name for type info lookups
fn find_ir_function(gen: LLVMGenerator, name: string) -> IRFunction {
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        if gen.ir_module.functions[i].name == name {
            return gen.ir_module.functions[i]
        }
        i = i + 1
    }
    -- Return a dummy function (should not happen in practice)
    return ir_function_new("__not_found__", "void")
}

-- Check if a function parameter at given index takes string type
-- Used for deciding whether to load dm_string from pointer for non-wrapper calls
fn func_param_type_at(gen: LLVMGenerator, fn_name: string, idx: int) -> string {
    let mut i = 0
    while i < gen.ir_module.functions.len() {
        let func = gen.ir_module.functions[i]
        if func.name == fn_name {
            if idx < func.params.len() {
                return func.params[idx].type_id
            }
            return ""
        }
        i = i + 1
    }
    return ""
}

-- Check if a function name refers to a runtime function that needs dm_string wrapper
fn is_runtime_string_fn(fn_name: string) -> bool {
    if starts_with(fn_name, "dm_string_") { return true }
    if starts_with(fn_name, "dm_print") { return true }
    if starts_with(fn_name, "dm_println") { return true }
    if fn_name == "dm_panic" { return true }
    if fn_name == "dm_read_line" { return true }
    if starts_with(fn_name, "dm_file_") { return true }
    if fn_name == "dm_int_to_string" { return true }
    if fn_name == "dm_float_to_string" { return true }
    if fn_name == "dm_bool_to_string" { return true }
    if fn_name == "dm_parse_int" { return true }
    if fn_name == "dm_parse_float" { return true }
    if fn_name == "dm_string_new" { return true }
    return false
}


-- ============================================================
-- SIMD Instruction Handling for LLVM IR Generation
--
-- Translates OP_SIMD_* IR instructions to LLVM vector operations.
-- SIMD types map to LLVM fixed-width vector types:
--   f32x4 -> <4 x float>, f32x8 -> <8 x float>
--   f64x2 -> <2 x double>, f64x4 -> <4 x double>
--   i32x4 -> <4 x i32>,    i32x8 -> <8 x i32>
--   i64x2 -> <2 x i64>,    i64x4 -> <4 x i64>
-- ============================================================

-- ============================================================
-- SIMD TYPE HELPERS
-- ============================================================

-- Get the LLVM vector type for a SIMD type name
fn get_simd_llvm_type(gen: LLVMGenerator, vec_type: string) -> int {
    let ctx = gen.context
    let elem_ty = get_simd_elem_type(gen, vec_type)
    let lanes = get_simd_lane_count(vec_type)
    return dm_llvm_vector_type(elem_ty, lanes)
}

-- Get the scalar element LLVM type for a vector type name
fn get_simd_elem_type(gen: LLVMGenerator, vec_type: string) -> int {
    let ctx = gen.context
    if vec_type == "f32x4" or vec_type == "f32x8" {
        return dm_llvm_float_type(ctx)
    }
    if vec_type == "f64x2" or vec_type == "f64x4" {
        return dm_llvm_double_type(ctx)
    }
    if vec_type == "i32x4" or vec_type == "i32x8" {
        return dm_llvm_int32_type(ctx)
    }
    if vec_type == "i64x2" or vec_type == "i64x4" {
        return dm_llvm_int64_type(ctx)
    }
    -- Fallback
    return dm_llvm_float_type(ctx)
}

-- Get the lane count for a SIMD type name
fn get_simd_lane_count(vec_type: string) -> int {
    if vec_type == "f32x4" { return 4 }
    if vec_type == "f32x8" { return 8 }
    if vec_type == "f64x2" { return 2 }
    if vec_type == "f64x4" { return 4 }
    if vec_type == "i32x4" { return 4 }
    if vec_type == "i32x8" { return 8 }
    if vec_type == "i64x2" { return 2 }
    if vec_type == "i64x4" { return 4 }
    return 4
}

-- Check if a SIMD type uses floating-point elements
fn is_simd_float_type(vec_type: string) -> bool {
    if vec_type == "f32x4" { return true }
    if vec_type == "f32x8" { return true }
    if vec_type == "f64x2" { return true }
    if vec_type == "f64x4" { return true }
    return false
}

-- ============================================================
-- SCALAR COERCION
-- ============================================================

-- Coerce a scalar value to match the expected SIMD element type.
-- Handles int-to-float, float-to-int, float width, int width mismatches.
fn coerce_simd_scalar(gen: LLVMGenerator, val: int, vec_type: string) -> int {
    let b = gen.builder
    let ctx = gen.context
    let val_ty_kind = dm_llvm_get_type_kind(dm_llvm_type_of(val))
    -- Type kind constants from LLVM-C Core.h:
    -- 8 = LLVMIntegerTypeKind, 2 = LLVMFloatTypeKind, 3 = LLVMDoubleTypeKind
    let is_int_val = val_ty_kind == 8
    let is_float_val = val_ty_kind == 2
    let is_double_val = val_ty_kind == 3
    if starts_with(vec_type, "f32") {
        -- Target: float
        if is_double_val {
            return dm_llvm_build_fp_cast(b, val, dm_llvm_float_type(ctx), "f64tof32")
        }
        if is_int_val {
            return dm_llvm_build_si_to_fp(b, val, dm_llvm_float_type(ctx), "itof32")
        }
        return val
    }
    if starts_with(vec_type, "f64") {
        -- Target: double
        if is_float_val {
            return dm_llvm_build_fp_cast(b, val, dm_llvm_double_type(ctx), "f32tof64")
        }
        if is_int_val {
            return dm_llvm_build_si_to_fp(b, val, dm_llvm_double_type(ctx), "itof64")
        }
        return val
    }
    if starts_with(vec_type, "i32") {
        -- Target: i32
        if is_int_val {
            let width = dm_llvm_get_int_type_width(dm_llvm_type_of(val))
            if width != 32 {
                return dm_llvm_build_int_cast2(b, val, dm_llvm_int32_type(ctx), 1, "toi32")
            }
        }
        if is_float_val or is_double_val {
            return dm_llvm_build_fp_to_si(b, val, dm_llvm_int32_type(ctx), "ftoi32")
        }
        return val
    }
    if starts_with(vec_type, "i64") {
        -- Target: i64
        if is_int_val {
            let width = dm_llvm_get_int_type_width(dm_llvm_type_of(val))
            if width != 64 {
                return dm_llvm_build_int_cast2(b, val, dm_llvm_int64_type(ctx), 1, "toi64")
            }
        }
        if is_float_val or is_double_val {
            return dm_llvm_build_fp_to_si(b, val, dm_llvm_int64_type(ctx), "ftoi64")
        }
        return val
    }
    return val
}

-- ============================================================
-- SIMD INSTRUCTION DISPATCHER
-- ============================================================

fn generate_simd_instruction(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let op = inst.op
    if op == OP_SIMD_SPLAT() {
        return lg_generate_simd_splat(gen, inst)
    }
    if op == OP_SIMD_SET() {
        return lg_generate_simd_set(gen, inst)
    }
    if op == OP_SIMD_ADD() {
        return generate_simd_binop(gen, inst, "add")
    }
    if op == OP_SIMD_SUB() {
        return generate_simd_binop(gen, inst, "sub")
    }
    if op == OP_SIMD_MUL() {
        return generate_simd_binop(gen, inst, "mul")
    }
    if op == OP_SIMD_DIV() {
        return generate_simd_binop(gen, inst, "div")
    }
    if op == OP_SIMD_EXTRACT() {
        return lg_generate_simd_extract(gen, inst)
    }
    return gen
}

-- ============================================================
-- SIMD SPLAT: create vector with all lanes set to scalar
-- ============================================================

fn lg_generate_simd_splat(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let ctx = g.context
    let id_key = int_to_string(inst.id)
    let vec_type = inst.simd_vec_type
    let llvm_vec_type = get_simd_llvm_type(g, vec_type)
    let lanes = get_simd_lane_count(vec_type)
    -- Resolve scalar value and coerce to element type
    let mut scalar = resolve_value(g, inst.simd_scalar)
    g = resolve_value_with_counter(g, inst.simd_scalar)
    scalar = coerce_simd_scalar(g, scalar, vec_type)
    -- Build vector by inserting scalar into each lane
    let mut vec = dm_llvm_get_undef(llvm_vec_type)
    let i32_ty = dm_llvm_int32_type(ctx)
    let mut i = 0
    while i < lanes {
        let idx = dm_llvm_const_int(i32_ty, i, 0)
        vec = dm_llvm_build_insert_element(b, vec, scalar, idx, "splat")
        i = i + 1
    }
    if inst.has_result { g.value_map.insert(id_key, vec) }
    return g
}

-- ============================================================
-- SIMD SET: create vector from individual element values
-- ============================================================

fn lg_generate_simd_set(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let ctx = g.context
    let id_key = int_to_string(inst.id)
    let vec_type = inst.simd_vec_type
    let llvm_vec_type = get_simd_llvm_type(g, vec_type)
    -- Start with undef vector
    let mut vec = dm_llvm_get_undef(llvm_vec_type)
    let i32_ty = dm_llvm_int32_type(ctx)
    let mut i = 0
    while i < inst.simd_elements.len() {
        let mut val = resolve_value(g, inst.simd_elements[i])
        g = resolve_value_with_counter(g, inst.simd_elements[i])
        val = coerce_simd_scalar(g, val, vec_type)
        let idx = dm_llvm_const_int(i32_ty, i, 0)
        vec = dm_llvm_build_insert_element(b, vec, val, idx, "set")
        i = i + 1
    }
    if inst.has_result { g.value_map.insert(id_key, vec) }
    return g
}

-- ============================================================
-- SIMD BINARY OP: element-wise add/sub/mul/div
-- ============================================================

fn generate_simd_binop(gen: LLVMGenerator, inst: IRInst, op_kind: string) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let id_key = int_to_string(inst.id)
    let lhs = resolve_value(g, inst.lhs)
    let rhs = resolve_value(g, inst.rhs)
    g = resolve_value_with_counter(g, inst.lhs)
    g = resolve_value_with_counter(g, inst.rhs)
    let is_float = is_simd_float_type(inst.simd_vec_type)
    let mut result = 0
    if op_kind == "add" {
        if is_float {
            result = dm_llvm_build_fadd(b, lhs, rhs, "vadd")
        } else {
            result = dm_llvm_build_add(b, lhs, rhs, "vadd")
        }
    } else if op_kind == "sub" {
        if is_float {
            result = dm_llvm_build_fsub(b, lhs, rhs, "vsub")
        } else {
            result = dm_llvm_build_sub(b, lhs, rhs, "vsub")
        }
    } else if op_kind == "mul" {
        if is_float {
            result = dm_llvm_build_fmul(b, lhs, rhs, "vmul")
        } else {
            result = dm_llvm_build_mul(b, lhs, rhs, "vmul")
        }
    } else if op_kind == "div" {
        if is_float {
            result = dm_llvm_build_fdiv(b, lhs, rhs, "vdiv")
        } else {
            result = dm_llvm_build_sdiv(b, lhs, rhs, "vdiv")
        }
    }
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}

-- ============================================================
-- SIMD EXTRACT: extract scalar element from vector
-- ============================================================

fn lg_generate_simd_extract(gen: LLVMGenerator, inst: IRInst) -> LLVMGenerator {
    let mut g = gen
    let b = g.builder
    let ctx = g.context
    let id_key = int_to_string(inst.id)
    let vec = resolve_value(g, inst.simd_vector)
    let mut idx = resolve_value(g, inst.simd_index)
    g = resolve_value_with_counter(g, inst.simd_vector)
    g = resolve_value_with_counter(g, inst.simd_index)
    -- LLVM extractelement needs an i32 index
    let idx_ty_kind = dm_llvm_get_type_kind(dm_llvm_type_of(idx))
    if idx_ty_kind == 8 {
        -- Integer type (LLVMIntegerTypeKind = 8): check width
        let idx_width = dm_llvm_get_int_type_width(dm_llvm_type_of(idx))
        if idx_width != 32 {
            idx = dm_llvm_build_int_cast2(b, idx, dm_llvm_int32_type(ctx), 1, "idx.cast")
        }
    }
    let mut result = dm_llvm_build_extract_element(b, vec, idx, "extract")
    -- Coerce extracted value to dAImond-native type:
    -- f32 -> f64 (fpext), i32 -> i64 (sext) so downstream code sees native types
    let vec_type = inst.simd_vec_type
    if vec_type == "f32x4" or vec_type == "f32x8" {
        -- float -> double
        result = dm_llvm_build_fp_cast(b, result, dm_llvm_double_type(ctx), "ext.f64")
    }
    if vec_type == "i32x4" or vec_type == "i32x8" {
        -- i32 -> i64
        result = dm_llvm_build_int_cast2(b, result, dm_llvm_int64_type(ctx), 1, "ext.i64")
    }
    if inst.has_result { g.value_map.insert(id_key, result) }
    return g
}


-- ============================================================
-- dAImond Stage 4 Compiler - LLVM Backend (Self-Hosted)
--
-- Compiles dAImond source to native binaries via LLVM.
-- Uses its own frontend (lexer.dm, parser.dm) instead of
-- reusing Stage 0's Zig frontend.
--
-- Pipeline:
--   Source (.dm) -> Lexer -> Tokens -> Parser -> AST
--   -> IR Gen -> dAImond IR -> LLVM Gen -> LLVM IR
--   -> Object File -> Linker -> Native Binary
-- ============================================================

fn VERSION() -> string { return "0.1.0" }

-- ============================================================
-- USAGE / HELP
-- ============================================================

fn print_usage() {
    println("dAImond Stage 4 Compiler (LLVM Backend)")
    println("")
    println("Usage: daimond-llvm <file.dm> [options]")
    println("")
    println("Options:")
    println("  -o <file>     Output binary name (default: input without .dm)")
    println("  -O0           No optimization (default)")
    println("  -O1           Basic optimization")
    println("  -O2           Full optimization")
    println("  -O3           Aggressive optimization")
    println("  --emit-ir     Print dAImond IR and exit")
    println("  --emit-llvm   Emit LLVM IR file (.ll) and exit")
    println("  -v, --verbose Verbose output")
    println("  -h, --help    Show this help")
    println("  --version     Show version")
}

-- ============================================================
-- CLI ARGUMENT PARSING
-- ============================================================

struct CLIArgs {
    input_file: string,
    output_file: string,
    opt_level: int,
    emit_ir: bool,
    emit_llvm: bool,
    verbose: bool,
    show_help: bool,
    show_version: bool,
    has_error: bool,
    error_msg: string
}

fn parse_cli_args() -> CLIArgs {
    let mut args = CLIArgs {
        input_file: "",
        output_file: "",
        opt_level: 0,
        emit_ir: false,
        emit_llvm: false,
        verbose: false,
        show_help: false,
        show_version: false,
        has_error: false,
        error_msg: ""
    }

    let argc = args_len()
    if argc < 2 {
        args.show_help = true
        return args
    }

    -- First non-flag argument is the input file
    let mut i = 1
    while i < argc {
        let arg = args_get(i)

        if arg == "-h" or arg == "--help" {
            args.show_help = true
            return args
        } else if arg == "--version" {
            args.show_version = true
            return args
        } else if arg == "-o" {
            if i + 1 < argc {
                i = i + 1
                args.output_file = args_get(i)
            } else {
                args.has_error = true
                args.error_msg = "Error: -o requires an argument"
                return args
            }
        } else if arg == "-O0" {
            args.opt_level = 0
        } else if arg == "-O1" {
            args.opt_level = 1
        } else if arg == "-O2" {
            args.opt_level = 2
        } else if arg == "-O3" {
            args.opt_level = 3
        } else if arg == "--emit-ir" {
            args.emit_ir = true
        } else if arg == "--emit-llvm" {
            args.emit_llvm = true
        } else if arg == "-v" or arg == "--verbose" {
            args.verbose = true
        } else if starts_with(arg, "-") {
            args.has_error = true
            args.error_msg = "Error: unknown option: " + arg
            return args
        } else {
            -- Input file
            if args.input_file == "" {
                args.input_file = arg
            } else {
                args.has_error = true
                args.error_msg = "Error: multiple input files specified"
                return args
            }
        }

        i = i + 1
    }

    if args.input_file == "" {
        args.has_error = true
        args.error_msg = "Error: no input file specified"
        return args
    }

    -- Default output file: strip .dm extension from input
    if args.output_file == "" {
        if ends_with(args.input_file, ".dm") {
            let input_len = len(args.input_file)
            args.output_file = substr(args.input_file, 0, input_len - 3)
        } else {
            args.output_file = args.input_file + ".out"
        }
    }

    return args
}

-- ============================================================
-- PATH UTILITIES
-- ============================================================

-- Derive the directory containing a file path.
-- "foo/bar/baz.dm" -> "foo/bar"
-- "baz.dm" -> "."
fn dirname(path: string) -> string {
    let path_len = len(path)
    let mut last_slash = -1
    let mut i = 0
    while i < path_len {
        if char_at(path, i) == "/" {
            last_slash = i
        }
        i = i + 1
    }
    if last_slash < 0 {
        return "."
    }
    return substr(path, 0, last_slash)
}

-- Find the project root by looking for stage0/runtime/daimond_runtime.c
-- relative to the executable's working directory. Tries several strategies:
--   1. Relative to input file directory: ../stage0/runtime/
--   2. CWD-relative: ../stage0/runtime/ (if running from stage4/)
--   3. CWD-relative: stage0/runtime/ (if running from project root)
fn find_runtime_dir(input_dir: string) -> string {
    -- Strategy 1: Go up from input file dir
    let try1 = input_dir + "/../stage0/runtime"
    if file_exists(try1 + "/daimond_runtime.c") {
        return try1
    }

    -- Strategy 2: Relative (assume running from stage4/)
    if file_exists("../stage0/runtime/daimond_runtime.c") {
        return "../stage0/runtime"
    }

    -- Strategy 3: From project root
    if file_exists("stage0/runtime/daimond_runtime.c") {
        return "stage0/runtime"
    }

    -- Strategy 4: Absolute fallback (common development path)
    return "../stage0/runtime"
}

-- Find the Stage 3 runtime (llvm_wrappers.c) directory
fn find_wrappers_dir(input_dir: string) -> string {
    -- Strategy 1: Go up from input file dir
    let try1 = input_dir + "/../stage3/runtime"
    if file_exists(try1 + "/llvm_wrappers.c") {
        return try1
    }

    -- Also check stage4/runtime which may have its own copy
    let try1b = input_dir + "/../stage4/runtime"
    if file_exists(try1b + "/llvm_wrappers.c") {
        return try1b
    }

    -- Strategy 2: Relative (assume running from stage4/)
    if file_exists("runtime/llvm_wrappers.c") {
        return "runtime"
    }
    if file_exists("../stage3/runtime/llvm_wrappers.c") {
        return "../stage3/runtime"
    }

    -- Strategy 3: From project root
    if file_exists("stage4/runtime/llvm_wrappers.c") {
        return "stage4/runtime"
    }
    if file_exists("stage3/runtime/llvm_wrappers.c") {
        return "stage3/runtime"
    }

    -- Fallback
    return "runtime"
}

-- ============================================================
-- IMPORT RESOLUTION
-- ============================================================

-- Resolve imports: read each imported file, tokenize, parse,
-- and merge declarations into the main source file.
-- Handles transitive and diamond imports via deduplication.
fn resolve_imports(sf: SourceFile, input_dir: string, verbose: bool) -> SourceFile {
    let mut merged = source_file_new(sf.module_name)
    let mut loaded: Map[string, bool] = Map_new()
    let mut decls: List[Declaration] = []

    -- Process each import
    let mut i = 0
    while i < sf.imports.len() {
        let imp = sf.imports[i]
        decls = process_import(imp, input_dir, loaded, decls, verbose)
        i = i + 1
    }

    -- Add the original file's declarations after imported ones
    let mut j = 0
    while j < sf.declarations.len() {
        decls.push(sf.declarations[j])
        j = j + 1
    }

    merged.declarations = decls
    return merged
}

-- Resolve an import path to a filesystem path.
-- "foo" -> "<dir>/foo.dm"
-- "std.io" -> "<dir>/std/io.dm"
fn resolve_import_path(import_path: string, base_dir: string) -> string {
    -- Replace dots with /
    let mut result = base_dir + "/"
    let path_len = len(import_path)
    let mut i = 0
    while i < path_len {
        let ch = char_at(import_path, i)
        if ch == "." {
            result = result + "/"
        } else {
            result = result + ch
        }
        i = i + 1
    }
    return result + ".dm"
}

-- Process a single import, recursively loading transitive imports
fn process_import(imp: ImportDecl, base_dir: string, loaded: Map[string, bool], decls: List[Declaration], verbose: bool) -> List[Declaration] {
    let mut result_decls = decls

    let file_path = resolve_import_path(imp.path, base_dir)

    -- Skip already-loaded imports (diamond import deduplication)
    if loaded.contains(file_path) {
        return result_decls
    }
    loaded.insert(file_path, true)

    if verbose {
        println("  Loading import: " + file_path)
    }

    -- Check if file exists
    if file_exists(file_path) == false {
        eprintln("Error: import file not found: " + file_path)
        return result_decls
    }

    -- Read, tokenize, and parse the imported file
    let source = file_read(file_path)
    let tokens = tokenize(source)
    let p = parser_new(tokens)
    let imp_sf = parse_source_file(p)

    -- Recursively process imports from the imported file
    let import_dir = dirname(file_path)
    let mut k = 0
    while k < imp_sf.imports.len() {
        result_decls = process_import(imp_sf.imports[k], import_dir, loaded, result_decls, verbose)
        k = k + 1
    }

    -- Add declarations from the imported file
    let mut m = 0
    while m < imp_sf.declarations.len() {
        result_decls.push(imp_sf.declarations[m])
        m = m + 1
    }

    return result_decls
}

-- ============================================================
-- POST-PARSE VALIDATION
-- ============================================================

fn validate_source_file(sf: SourceFile) -> List[string] {
    let mut errors: List[string] = []
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            -- Check for params with empty type (sign of a parse error)
            let mut pi = 0
            while pi < fd.params.len() {
                let param = fd.params[pi]
                if param.name != "self" and param.name != "mut self" and len(param.type_name) == 0 {
                    errors.push("Error: parameter '" + param.name + "' in function '" + fd.name + "' has no type")
                }
                pi = pi + 1
            }
        }
        i = i + 1
    }
    return errors
}

-- ============================================================
-- EFFECT SYSTEM ENFORCEMENT
-- ============================================================

fn builtin_required_effect(name: string) -> string {
    if name == "file_read" or name == "file_write" or name == "file_append" or name == "file_exists" { return "FileSystem" }
    if name == "println" or name == "print" or name == "eprintln" or name == "eprint" or name == "read_line" { return "Console" }
    if name == "args_get" or name == "args_len" { return "IO" }
    if name == "system" or name == "exit" { return "Process" }
    return ""
}

fn collect_call_names_from_expr(e: Expr) -> List[string] {
    let mut names: List[string] = []
    if e.kind == EXPR_FUNCTION_CALL() {
        let callee_expr = *e.callee
        if len(callee_expr.name) > 0 {
            names.push(callee_expr.name)
        }
    }
    return names
}

fn collect_call_names_from_stmts(stmts: List[Stmt]) -> List[string] {
    let mut names: List[string] = []
    let mut i = 0
    while i < stmts.len() {
        let s = stmts[i]
        if s.kind == STMT_EXPRESSION() {
            let e = *s.expr
            let sub = collect_call_names_from_expr(e)
            let mut j = 0
            while j < sub.len() {
                names.push(sub[j])
                j = j + 1
            }
        } else if s.kind == STMT_RETURN() {
            if s.has_ret_value {
                let e = *s.ret_value
                let sub = collect_call_names_from_expr(e)
                let mut j = 0
                while j < sub.len() {
                    names.push(sub[j])
                    j = j + 1
                }
            }
        } else if s.kind == STMT_LET() {
            let e = *s.let_value
            let sub = collect_call_names_from_expr(e)
            let mut j = 0
            while j < sub.len() {
                names.push(sub[j])
                j = j + 1
            }
        }
        i = i + 1
    }
    return names
}

fn list_contains_str(lst: List[string], val: string) -> bool {
    let mut i = 0
    while i < lst.len() {
        if lst[i] == val { return true }
        i = i + 1
    }
    return false
}

fn validate_effects(sf: SourceFile) -> List[string] {
    let mut errors: List[string] = []
    -- Build map: function name -> declared effects
    let mut fn_effects: Map[string, List[string]] = Map_new()
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.effects.len() > 0 {
                fn_effects.insert(fd.name, fd.effects)
            }
        }
        i = i + 1
    }

    -- For each function with effects, check its calls
    i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.effects.len() > 0 {
                let caller_effects = fd.effects
                let call_names = collect_call_names_from_stmts(fd.body)
                let mut ci = 0
                while ci < call_names.len() {
                    let callee = call_names[ci]
                    -- Check builtin required effects
                    let req = builtin_required_effect(callee)
                    if len(req) > 0 {
                        if list_contains_str(caller_effects, req) == false {
                            errors.push("Error: function '" + fd.name + "' calls '" + callee + "' which requires effect '" + req + "'")
                        }
                    }
                    -- Check callee's declared effects are subset of caller's
                    if fn_effects.contains(callee) {
                        let callee_effects = fn_effects.get(callee)
                        let mut ei = 0
                        while ei < callee_effects.len() {
                            let eff = callee_effects[ei]
                            if list_contains_str(caller_effects, eff) == false {
                                errors.push("Error: function '" + fd.name + "' calls '" + callee + "' which requires effect '" + eff + "' not in caller's effects")
                            }
                            ei = ei + 1
                        }
                    }
                    ci = ci + 1
                }
            }
        }
        i = i + 1
    }
    return errors
}

-- ============================================================
-- COMPILATION PIPELINE
-- ============================================================

fn compile(cli: CLIArgs) -> int {
    let input_file = cli.input_file
    let output_file = cli.output_file
    let opt_level = cli.opt_level
    let verbose = cli.verbose

    if verbose {
        println("dAImond Stage 4 Compiler (LLVM Backend)")
        println("Input:  " + input_file)
        println("Output: " + output_file)
    }

    -- Phase 1: Read source file
    if verbose { println("  [1/6] Reading source...") }

    if file_exists(input_file) == false {
        eprintln("Error: file not found: " + input_file)
        return 1
    }

    let source = file_read(input_file)

    -- Phase 2: Tokenize
    if verbose { println("  [2/6] Lexing...") }

    let tokens = tokenize(source)

    -- Phase 3: Parse
    if verbose { println("  [3/6] Parsing...") }

    let p = parser_new(tokens)
    let mut sf = parse_source_file(p)

    -- Check for parse errors
    if p.errors.len() > 0 {
        let mut ei = 0
        while ei < p.errors.len() {
            eprintln(p.errors[ei])
            ei = ei + 1
        }
        exit(1)
    }

    -- Post-parse validation
    let validation_errors = validate_source_file(sf)
    if validation_errors.len() > 0 {
        let mut vi = 0
        while vi < validation_errors.len() {
            eprintln(validation_errors[vi])
            vi = vi + 1
        }
        exit(1)
    }

    -- Phase 3.5: Resolve imports
    if sf.imports.len() > 0 {
        if verbose { println("  [3.5/6] Resolving imports...") }
        let input_dir = dirname(input_file)
        sf = resolve_imports(sf, input_dir, verbose)
    }

    -- Effect system enforcement
    let effect_errors = validate_effects(sf)
    if effect_errors.len() > 0 {
        let mut ei2 = 0
        while ei2 < effect_errors.len() {
            eprintln(effect_errors[ei2])
            ei2 = ei2 + 1
        }
        exit(1)
    }

    -- Phase 4: Generate IR
    if verbose { println("  [4/6] Generating IR...") }

    let mut ir_gen = ir_generator_new()
    ir_gen = generate_module(ir_gen, sf)

    -- If --emit-ir, print the IR module and exit
    if cli.emit_ir {
        -- Print a summary of the IR module (functions and their basic blocks)
        println("dAImond IR Module:")
        println("  Functions: " + int_to_string(ir_gen.ir_mod.functions.len()))
        println("  Globals:   " + int_to_string(ir_gen.ir_mod.globals.len()))
        let mut fi = 0
        while fi < ir_gen.ir_mod.functions.len() {
            let func = ir_gen.ir_mod.functions[fi]
            let mut sig = "  fn " + func.name + "("
            let mut pi = 0
            while pi < func.params.len() {
                if pi > 0 { sig = sig + ", " }
                sig = sig + func.params[pi].name + ": " + func.params[pi].type_id
                pi = pi + 1
            }
            sig = sig + ") -> " + func.return_type
            if func.is_extern { sig = sig + " [extern]" }
            println(sig)
            let mut bi = 0
            while bi < func.blocks.len() {
                let block = func.blocks[bi]
                println("    " + block.label + ": (" + int_to_string(block.instructions.len()) + " instructions)")
                bi = bi + 1
            }
            fi = fi + 1
        }
        return 0
    }

    -- Phase 5: Generate LLVM IR
    if verbose { println("  [5/6] Generating LLVM IR...") }

    let mut lg = llvm_gen_new(ir_gen.ir_mod, "daimond_module")
    lg = lg_generate_module(lg)

    -- If --emit-llvm, emit LLVM IR file BEFORE verification (for debugging)
    if cli.emit_llvm {
        let ll_file = output_file + ".ll"
        let ir_result = emit_to_ir_file(lg, ll_file)
        if ir_result != 0 {
            eprintln("Error: failed to emit LLVM IR to " + ll_file)
            llvm_gen_dispose(lg)
            return 1
        }
        if verbose {
            println("  Emitted LLVM IR: " + ll_file)
        } else {
            println(ll_file)
        }
        llvm_gen_dispose(lg)
        return 0
    }

    -- Phase 6: Emit object file and link
    if verbose { println("  [6/6] Emitting binary...") }

    let obj_file = output_file + ".o"
    let emit_result = emit_to_object(lg, obj_file, opt_level)
    if emit_result != 0 {
        eprintln("Error: failed to emit object file")
        llvm_gen_dispose(lg)
        return 1
    }

    -- Dispose LLVM resources before linking
    llvm_gen_dispose(lg)

    -- Find runtime and wrapper source paths
    let input_dir = dirname(input_file)
    let runtime_dir = find_runtime_dir(input_dir)
    let wrappers_dir = find_wrappers_dir(input_dir)

    let runtime_c = runtime_dir + "/daimond_runtime.c"
    let runtime_h_dir = runtime_dir
    let wrappers_c = wrappers_dir + "/llvm_wrappers.c"

    -- Build the link command
    -- cc -o <output> <obj> <runtime.c> <wrappers.c> -I<runtime_dir> -lm -lpthread
    let mut link_cmd = "cc"
    link_cmd = link_cmd + " -o " + output_file
    link_cmd = link_cmd + " " + obj_file

    -- Add runtime source (compile inline)
    if file_exists(runtime_c) {
        link_cmd = link_cmd + " " + runtime_c
    } else {
        eprintln("Warning: runtime not found at " + runtime_c)
    }

    -- Add ABI wrappers source
    if file_exists(wrappers_c) {
        link_cmd = link_cmd + " " + wrappers_c
    } else {
        eprintln("Warning: LLVM wrappers not found at " + wrappers_c)
    }

    -- Include path for runtime headers
    link_cmd = link_cmd + " -I" + runtime_h_dir

    -- Link system libraries
    link_cmd = link_cmd + " -lm -lpthread"

    if verbose {
        println("  Link: " + link_cmd)
    }

    let link_result = system(link_cmd)
    if link_result != 0 {
        eprintln("Error: linking failed (exit code " + int_to_string(link_result) + ")")
        -- Clean up object file on failure
        system("rm -f " + obj_file)
        return 1
    }

    -- Clean up temporary object file
    system("rm -f " + obj_file)

    if verbose {
        println("Successfully compiled: " + output_file)
    }

    return 0
}

-- ============================================================
-- ENTRY POINT
-- ============================================================

fn main() -> int {
    let cli = parse_cli_args()

    if cli.show_help {
        print_usage()
        return 0
    }

    if cli.show_version {
        println("daimond-llvm " + VERSION())
        return 0
    }

    if cli.has_error {
        eprintln(cli.error_msg)
        return 1
    }

    return compile(cli)
}
