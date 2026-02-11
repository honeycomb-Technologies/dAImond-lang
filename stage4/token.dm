module token

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
