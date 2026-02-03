module main

-- ============================================================
-- dAImond Stage 1 Compiler
-- Self-hosting compiler written in dAImond, compiled by Stage 0
-- Compiles a subset of dAImond to C11
-- ============================================================

-- [module: token]

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
    return "token(" + int_to_string(kind) + ")"
}

-- [module: lexer]


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
                tokens.push(token_new(TK_STAR(), "*", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
            } else if ch == "/" {
                tokens.push(token_new(TK_SLASH(), "/", sl, sc))
                lex.pos = lex.pos + 1
                lex.col = lex.col + 1
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

-- [module: compiler]


-- ============================================================
-- PARSER
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
    let mut pp = p
    pp = p_skip_newlines(pp)
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

-- ============================================================
-- PARSE TYPE EXPRESSIONS
-- ============================================================

struct TypeExpr {
    kind: int,
    name: string,
    generic_args: List[TypeExpr],
    is_mutable: bool
}

fn empty_type() -> TypeExpr {
    let ga: List[TypeExpr] = []
    return TypeExpr { kind: 0, name: "", generic_args: ga, is_mutable: false }
}

struct TypeResult {
    p: Parser,
    ty: TypeExpr
}

fn parse_type(p: Parser) -> TypeResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)
    if tok.kind != TK_IDENT() and tok.kind != TK_SELF() {
        return TypeResult { p: pp, ty: empty_type() }
    }
    let name = tok.value
    pp = p_advance(pp)
    -- Check for generic args [T, U]
    let ga: List[TypeExpr] = []
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut first = true
        let mut type_done = false
        while type_done == false {
            if p_peek(pp) == TK_RBRACKET() {
                type_done = true
            } else {
                if first == false {
                    pp = p_expect(pp, TK_COMMA())
                }
                first = false
                let inner = parse_type(pp)
                pp = inner.p
                ga.push(inner.ty)
            }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }
    let ty = TypeExpr { kind: 70, name: name, generic_args: ga, is_mutable: false }
    return TypeResult { p: pp, ty: ty }
}

-- ============================================================
-- PARSE EXPRESSIONS (Pratt parser)
-- ============================================================

struct ExprResult {
    p: Parser,
    kind: int,
    int_val: int,
    float_val: float,
    bool_val: bool,
    str_val: string,
    name: string,
    op: string,
    left_idx: int,
    right_idx: int,
    args_start: int,
    args_count: int,
    line: int,
    col: int
}

-- We store expressions in a flat pool since Box[Expr] doesn't work well
-- Instead, we'll generate C code directly from token stream
-- This avoids the need for a complex AST

-- For Stage 1 v0.1, we use a simpler approach:
-- Parse directly to C code string output (one-pass compilation)

-- ============================================================
-- SIMPLE ONE-PASS COMPILER
-- Parse and generate C code simultaneously
-- ============================================================

struct Compiler {
    tokens: List[Token],
    pos: int,
    output: string,
    indent: int,
    errors: List[string],
    -- Track struct names for type generation
    struct_names: List[string],
    fn_names: List[string],
    fn_ret_types: List[string],
    fn_sigs: List[string],
    struct_defs: List[string],
    fn_defs: List[string],
    -- Track which variables are strings for operator dispatch
    str_vars: List[string],
    -- Accumulated list type definitions (built during parsing)
    list_type_defs: string,
    -- Track list variable element types: "|dm_var=elem_type|..."
    list_elem_types: string,
    -- Track all variable types: "|dm_var=c_type|..."
    var_types: string,
    -- Track struct field types: "|dm_StructName.fieldName=c_type|..."
    struct_fields: string,
    -- Track enum variant info: "|EnumName.Variant=unit|" or "|EnumName.Variant=tuple:type1,type2|"
    enum_variants: string,
    -- Track enum names (separate from struct_names)
    enum_names: List[string],
    -- Track generated Option/Result type defs to avoid duplicates
    option_type_defs: string,
    -- Counter for match expression temporaries
    match_counter: int,
    -- Expected type context for Some/None/Ok/Err inference
    expected_type: string,
    -- Current function return type for return statement context
    current_fn_ret_type: string,
    -- Counter for lambda lifted functions
    lambda_counter: int,
    -- Accumulated lambda function definitions (lifted to file scope)
    lambda_defs: string,
    -- Track generic function token ranges: "|fn_name=start:end|"
    generic_fn_tokens: string,
    -- Track already-monomorphized generic functions: "|mangled_name|"
    monomorphized_fns: string
}

fn compiler_new(tokens: List[Token]) -> Compiler {
    let errs: List[string] = []
    let sn: List[string] = []
    let fnames: List[string] = []
    let frets: List[string] = []
    let fsigs: List[string] = []
    let sdefs: List[string] = []
    let fdefs: List[string] = []
    let sv: List[string] = []
    let en: List[string] = []
    return Compiler {
        tokens: tokens, pos: 0, output: "", indent: 0,
        errors: errs, struct_names: sn, fn_names: fnames,
        fn_ret_types: frets,
        fn_sigs: fsigs, struct_defs: sdefs, fn_defs: fdefs,
        str_vars: sv, list_type_defs: "",
        list_elem_types: "",
        var_types: "",
        struct_fields: "",
        enum_variants: "",
        enum_names: en,
        option_type_defs: "",
        match_counter: 0,
        expected_type: "",
        current_fn_ret_type: "",
        lambda_counter: 0,
        lambda_defs: "",
        generic_fn_tokens: "",
        monomorphized_fns: ""
    }
}

fn lookup_struct_field_type(c: Compiler, struct_type: string, field_name: string) -> string {
    let marker = "|" + struct_type + "." + field_name + "="
    let pos = string_find(c.struct_fields, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.struct_fields, start, len(c.struct_fields) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

fn is_enum_name(c: Compiler, name: string) -> bool {
    let mut i = 0
    while i < c.enum_names.len() {
        let cur = "" + c.enum_names[i]
        if cur == name { return true }
        i = i + 1
    }
    return false
}

fn lookup_enum_variant(c: Compiler, enum_name: string, variant_name: string) -> string {
    let marker = "|" + enum_name + "." + variant_name + "="
    let pos = string_find(c.enum_variants, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.enum_variants, start, len(c.enum_variants) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

-- Check if a field access like "dm_var.field" is a string
fn field_access_is_string(code: string, c: Compiler) -> bool {
    let dot_pos = string_find(code, ".")
    if dot_pos < 0 { return false }
    let var_name = substr(code, 0, dot_pos)
    let field_name = substr(code, dot_pos + 1, len(code) - dot_pos - 1)
    -- Look up the variable's type
    let var_type = lookup_var_type(c, var_name)
    if var_type == "" { return false }
    -- Look up the field's type in that struct
    let field_type = lookup_struct_field_type(c, var_type, field_name)
    if field_type == "dm_string" { return true }
    return false
}

-- Check if a generated C code expression is known to produce a string
fn str_list_contains(list: List[string], val: string) -> bool {
    let mut i = 0
    while i < list.len() {
        let cur = "" + list[i]
        if cur == val { return true }
        i = i + 1
    }
    return false
}

fn code_is_string(code: string, c: Compiler) -> bool {
    if starts_with(code, "dm_string_from_cstr(") { return true }
    if starts_with(code, "dm_string_concat(") { return true }
    if starts_with(code, "dm_int_to_string(") { return true }
    if starts_with(code, "dm_bool_to_string(") { return true }
    if starts_with(code, "dm_float_to_string(") { return true }
    if starts_with(code, "dm_string_substr(") { return true }
    if starts_with(code, "dm_char_at(") { return true }
    if starts_with(code, "dm_file_read(") { return true }
    if starts_with(code, "dm_args_get(") { return true }
    if starts_with(code, "dm_string_replace(") { return true }
    if starts_with(code, "dm_string_trim(") { return true }
    -- Check if it's a known string variable
    if str_list_contains(c.str_vars, code) { return true }
    -- Check var_types for dm_string
    let vt = lookup_var_type(c, code)
    if vt == "dm_string" { return true }
    -- Check struct field access: dm_var.field
    if field_access_is_string(code, c) { return true }
    return false
}

fn c_cur(c: Compiler) -> Token {
    if c.pos < c.tokens.len() {
        return c.tokens[c.pos]
    }
    return token_new(TK_EOF(), "", 0, 0)
}

fn c_peek(c: Compiler) -> int {
    return c_cur(c).kind
}

fn c_advance(c: Compiler) -> Compiler {
    let mut cc = c
    cc.pos = cc.pos + 1
    return cc
}

fn c_skip_nl(c: Compiler) -> Compiler {
    let mut cc = c
    while c_peek(cc) == TK_NEWLINE() {
        cc = c_advance(cc)
    }
    return cc
}

fn c_expect(c: Compiler, kind: int) -> Compiler {
    let mut cc = c_skip_nl(c)
    if c_peek(cc) == kind {
        return c_advance(cc)
    }
    let tok = c_cur(cc)
    let msg = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": expected " + token_kind_name(kind) + " but got " + token_kind_name(tok.kind)
    cc.errors.push(msg)
    return cc
}

fn c_error(c: Compiler, msg: string) -> Compiler {
    let mut cc = c
    let tok = c_cur(cc)
    let full = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": " + msg
    cc.errors.push(full)
    return cc
}

fn indent_str(level: int) -> string {
    let mut s = ""
    let mut i = 0
    while i < level {
        s = s + "    "
        i = i + 1
    }
    return s
}

fn dm_mangle(name: string) -> string {
    return "dm_" + name
}

fn map_dm_type(name: string) -> string {
    if name == "int" { return "int64_t" }
    if name == "float" { return "double" }
    if name == "bool" { return "bool" }
    if name == "string" { return "dm_string" }
    if name == "void" { return "void" }
    return "dm_" + name
}

-- [module: compile_expr]


-- ============================================================
-- EXPRESSION COMPILER (returns C expression string)
-- ============================================================

struct ExprOut {
    c: Compiler,
    code: string
}

fn compile_expr(c: Compiler) -> ExprOut {
    return compile_or_expr(c)
}

fn compile_or_expr(c: Compiler) -> ExprOut {
    let mut result = compile_and_expr(c)
    while c_peek(result.c) == TK_OR() {
        result.c = c_advance(result.c)
        let rhs = compile_and_expr(result.c)
        result.c = rhs.c
        result.code = "(" + result.code + " || " + rhs.code + ")"
    }
    return result
}

fn compile_and_expr(c: Compiler) -> ExprOut {
    let mut result = compile_eq_expr(c)
    while c_peek(result.c) == TK_AND() {
        result.c = c_advance(result.c)
        let rhs = compile_eq_expr(result.c)
        result.c = rhs.c
        result.code = "(" + result.code + " && " + rhs.code + ")"
    }
    return result
}

fn compile_eq_expr(c: Compiler) -> ExprOut {
    let mut result = compile_cmp_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_EQEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_cmp_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_eq(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " == " + rhs.code + ")"
            }
        } else if k == TK_BANGEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_cmp_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "(!dm_string_eq(" + result.code + ", " + rhs.code + "))"
            } else {
                result.code = "(" + result.code + " != " + rhs.code + ")"
            }
        } else {
            cont = false
        }
    }
    return result
}

fn compile_cmp_expr(c: Compiler) -> ExprOut {
    let mut result = compile_add_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_LT() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_lt(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " < " + rhs.code + ")"
            }
        } else if k == TK_GT() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_gt(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " > " + rhs.code + ")"
            }
        } else if k == TK_LTEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_lteq(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " <= " + rhs.code + ")"
            }
        } else if k == TK_GTEQ() {
            result.c = c_advance(result.c)
            let rhs = compile_add_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_gteq(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " >= " + rhs.code + ")"
            }
        } else {
            cont = false
        }
    }
    return result
}

fn compile_add_expr(c: Compiler) -> ExprOut {
    let mut result = compile_mul_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_PLUS() {
            result.c = c_advance(result.c)
            let rhs = compile_mul_expr(result.c)
            result.c = rhs.c
            if code_is_string(result.code, result.c) or code_is_string(rhs.code, result.c) {
                result.code = "dm_string_concat(" + result.code + ", " + rhs.code + ")"
            } else {
                result.code = "(" + result.code + " + " + rhs.code + ")"
            }
        } else if k == TK_MINUS() {
            result.c = c_advance(result.c)
            let rhs = compile_mul_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " - " + rhs.code + ")"
        } else {
            cont = false
        }
    }
    return result
}

fn compile_mul_expr(c: Compiler) -> ExprOut {
    let mut result = compile_unary_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_STAR() {
            result.c = c_advance(result.c)
            let rhs = compile_unary_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " * " + rhs.code + ")"
        } else if k == TK_SLASH() {
            result.c = c_advance(result.c)
            let rhs = compile_unary_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " / " + rhs.code + ")"
        } else if k == TK_PERCENT() {
            result.c = c_advance(result.c)
            let rhs = compile_unary_expr(result.c)
            result.c = rhs.c
            result.code = "(" + result.code + " % " + rhs.code + ")"
        } else {
            cont = false
        }
    }
    return result
}

fn compile_unary_expr(c: Compiler) -> ExprOut {
    let cc = c_skip_nl(c)
    let k = c_peek(cc)
    if k == TK_MINUS() {
        let cc2 = c_advance(cc)
        let operand = compile_unary_expr(cc2)
        return ExprOut { c: operand.c, code: "(-" + operand.code + ")" }
    }
    if k == TK_NOT() {
        let cc2 = c_advance(cc)
        let operand = compile_unary_expr(cc2)
        return ExprOut { c: operand.c, code: "(!" + operand.code + ")" }
    }
    return compile_postfix_expr(cc)
}

fn compile_postfix_expr(c: Compiler) -> ExprOut {
    let mut result = compile_primary_expr(c)
    let mut cont = true
    while cont {
        let k = c_peek(result.c)
        if k == TK_DOT() {
            result.c = c_advance(result.c)
            let tok = c_cur(result.c)
            let field = tok.value
            result.c = c_advance(result.c)
            -- Check if this is an enum constructor: EnumName.Variant
            -- result.code will be "dm_EnumName" if the base was an enum ident
            let mut is_enum_ctor = false
            let mut enum_base_name = ""
            if starts_with(result.code, "dm_") {
                let base_name = substr(result.code, 3, len(result.code) - 3)
                if is_enum_name(result.c, base_name) {
                    let vinfo = lookup_enum_variant(result.c, base_name, field)
                    if vinfo != "" {
                        is_enum_ctor = true
                        enum_base_name = base_name
                    }
                }
            }
            if is_enum_ctor {
                -- Enum constructor
                let vinfo = lookup_enum_variant(result.c, enum_base_name, field)
                if c_peek(result.c) == TK_LPAREN() {
                    -- Constructor with args: EnumName.Variant(args)
                    result.c = c_advance(result.c)
                    let mut args_code = ""
                    let mut first = true
                    while c_peek(result.c) != TK_RPAREN() and c_peek(result.c) != TK_EOF() {
                        if first == false {
                            result.c = c_expect(result.c, TK_COMMA())
                            args_code = args_code + ", "
                        }
                        first = false
                        let arg = compile_expr(result.c)
                        result.c = arg.c
                        args_code = args_code + arg.code
                    }
                    result.c = c_expect(result.c, TK_RPAREN())
                    result.code = dm_mangle(enum_base_name) + "_" + field + "(" + args_code + ")"
                } else {
                    -- Unit constructor: EnumName.Variant
                    if vinfo == "unit" {
                        result.code = dm_mangle(enum_base_name) + "_" + field + "()"
                    } else {
                        result.code = dm_mangle(enum_base_name) + "_" + field
                    }
                }
            } else if c_peek(result.c) == TK_LPAREN() {
                -- Method call
                result.c = c_advance(result.c)
                let mut args_code = ""
                let mut first = true
                while c_peek(result.c) != TK_RPAREN() and c_peek(result.c) != TK_EOF() {
                    if first == false {
                        result.c = c_expect(result.c, TK_COMMA())
                        args_code = args_code + ", "
                    }
                    first = false
                    let arg = compile_expr(result.c)
                    result.c = arg.c
                    args_code = args_code + arg.code
                }
                result.c = c_expect(result.c, TK_RPAREN())
                -- Generate method call
                if field == "push" {
                    result.code = "DM_LIST_PUSH(" + result.code + ", (" + args_code + "))"
                } else if field == "len" {
                    result.code = "DM_LIST_LEN(" + result.code + ")"
                } else if field == "pop" {
                    result.code = "DM_LIST_POP(" + result.code + ")"
                } else if field == "contains" {
                    result.code = "DM_LIST_CONTAINS(" + result.code + ", " + args_code + ")"
                } else {
                    result.code = dm_mangle(field) + "(" + result.code + ", " + args_code + ")"
                }
            } else {
                result.code = result.code + "." + field
            }
        } else if k == TK_LBRACKET() {
            result.c = c_advance(result.c)
            let idx = compile_expr(result.c)
            result.c = idx.c
            result.c = c_expect(result.c, TK_RBRACKET())
            result.code = "DM_LIST_GET(" + result.code + ", " + idx.code + ")"
        } else if k == TK_LPAREN() and result.code != "" {
            -- Function call (only if we have a callee)
            result.c = c_advance(result.c)
            let mut args_code = ""
            let mut arg_types: List[string] = []
            let mut first = true
            while c_peek(result.c) != TK_RPAREN() and c_peek(result.c) != TK_EOF() {
                if first == false {
                    result.c = c_expect(result.c, TK_COMMA())
                    args_code = args_code + ", "
                }
                first = false
                result.c = c_skip_nl(result.c)
                let arg = compile_expr(result.c)
                result.c = arg.c
                args_code = args_code + arg.code
                arg_types.push(infer_type_from_code(arg.code, result.c))
            }
            result.c = c_expect(result.c, TK_RPAREN())
            -- Check for implicit generic function call: dm_name where name is generic
            let mut callee_code = result.code
            if starts_with(callee_code, "dm_") {
                let base_name = substr(callee_code, 3, len(callee_code) - 3)
                let ginfo = lookup_generic_fn(result.c, base_name)
                if ginfo != "" and arg_types.len() > 0 {
                    -- Infer type from first argument
                    let concrete = "" + arg_types[0]
                    result.c = monomorphize_generic_fn(result.c, base_name, ginfo, concrete)
                    callee_code = dm_mangle(base_name) + "_" + concrete
                }
            }
            result.code = callee_code + "(" + args_code + ")"
        } else {
            cont = false
        }
    }
    return result
}

-- Try to compile struct literal. Returns ExprOut with code="" if not a struct literal.
fn try_compile_struct_lit(c: Compiler, name: string) -> ExprOut {
    if c_peek(c) != TK_LBRACE() {
        return ExprOut { c: c, code: "" }
    }
    if is_struct_name(c, name) == false {
        return ExprOut { c: c, code: "" }
    }
    return compile_struct_lit(c, name)
}

fn compile_struct_lit(c: Compiler, type_name: string) -> ExprOut {
    let mut cc = c_advance(c)  -- skip '{'
    let mut code = "(dm_" + type_name + "){ "
    let mut first = true
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        if first == false {
            cc = c_expect(cc, TK_COMMA())
            code = code + ", "
        }
        first = false
        cc = c_skip_nl(cc)
        let field_tok = c_cur(cc)
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let val = compile_expr(cc)
        cc = val.c
        code = code + "." + field_tok.value + " = " + val.code
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    code = code + " }"
    return ExprOut { c: cc, code: code }
}

-- Compile lambda expression: |params| expr or |params| { block }
-- Lifts the lambda to a static function at file scope
fn compile_lambda_expr(c: Compiler) -> ExprOut {
    let mut cc = c_advance(c)  -- skip opening '|'

    -- Parse parameters: |x: int, y: string| or || for no params
    let mut param_names: List[string] = []
    let mut param_types: List[string] = []
    let mut params_c = ""
    let mut first = true

    while c_peek(cc) != TK_PIPE() and c_peek(cc) != TK_EOF() {
        if first == false {
            cc = c_expect(cc, TK_COMMA())
            params_c = params_c + ", "
        }
        first = false
        let pname_tok = c_cur(cc)
        let pname = pname_tok.value
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let pt = parse_type_for_c(cc)
        cc = pt.c
        param_names.push(pname)
        param_types.push(pt.code)
        params_c = params_c + pt.code + " " + dm_mangle(pname)
    }
    cc = c_advance(cc)  -- skip closing '|'

    -- Determine lambda name
    let lambda_id = cc.lambda_counter
    cc.lambda_counter = lambda_id + 1
    let lambda_name = "_lambda_" + int_to_string(lambda_id)

    -- Check for block body { ... } vs expression body
    let mut is_block = c_peek(cc) == TK_LBRACE()

    if is_block {
        -- Block body: |params| { stmts }
        cc = c_expect(cc, TK_LBRACE())
        -- Save compiler state for body compilation
        let saved_output = cc.output
        let saved_indent = cc.indent
        let saved_fn_ret = cc.current_fn_ret_type
        cc.output = ""
        cc.indent = 1

        -- Track lambda parameter types
        let mut pi = 0
        while pi < param_names.len() {
            let pn = "" + param_names[pi]
            let pt = "" + param_types[pi]
            cc = track_var_type(cc, dm_mangle(pn), pt)
            cc = track_str_var(cc, dm_mangle(pn), pt == "dm_string")
            pi = pi + 1
        }

        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())

        let body_code = cc.output
        cc.output = saved_output
        cc.indent = saved_indent
        cc.current_fn_ret_type = saved_fn_ret

        -- For block lambdas, try to infer return type from the body
        -- Look for "return <expr>;" pattern in body to determine type
        -- Default to void if no return found
        let mut block_ret_type = "void"
        let ret_pos = string_find(body_code, "return ")
        let has_expected = cc.expected_type != ""
        if ret_pos >= 0 and has_expected {
            block_ret_type = cc.expected_type
        } else if ret_pos >= 0 {
            block_ret_type = "int64_t"
        }

        -- Generate lambda definition
        let mut lambda_def = "static " + block_ret_type + " " + lambda_name + "("
        if params_c == "" {
            lambda_def = lambda_def + "void"
        } else {
            lambda_def = lambda_def + params_c
        }
        lambda_def = lambda_def + ") {\n" + body_code + "}\n\n"
        cc.lambda_defs = cc.lambda_defs + lambda_def

        -- Build and track function pointer type
        let mut fptr_type = build_fptr_type(block_ret_type, param_types)
        cc = track_var_type(cc, lambda_name, fptr_type)

        return ExprOut { c: cc, code: lambda_name }
    }

    -- Expression body: |params| expr
    -- Track lambda parameter types
    let mut pi2 = 0
    while pi2 < param_names.len() {
        let pn = "" + param_names[pi2]
        let pt = "" + param_types[pi2]
        cc = track_var_type(cc, dm_mangle(pn), pt)
        cc = track_str_var(cc, dm_mangle(pn), pt == "dm_string")
        pi2 = pi2 + 1
    }

    let body_expr = compile_expr(cc)
    cc = body_expr.c

    -- Infer return type from expression
    let ret_type = infer_type_from_code(body_expr.code, cc)

    -- Generate the lifted function
    let mut lambda_def = "static " + ret_type + " " + lambda_name + "("
    if params_c == "" {
        lambda_def = lambda_def + "void"
    } else {
        lambda_def = lambda_def + params_c
    }
    lambda_def = lambda_def + ") {\n    return " + body_expr.code + ";\n}\n\n"
    cc.lambda_defs = cc.lambda_defs + lambda_def

    -- Build and track function pointer type
    let mut fptr_type = build_fptr_type(ret_type, param_types)
    cc = track_var_type(cc, lambda_name, fptr_type)

    return ExprOut { c: cc, code: lambda_name }
}

-- Emit a function pointer variable declaration: "ret_type (*name)(params)"
-- from a type string like "ret_type (*)(params)" and a variable name
fn emit_fptr_decl(fptr_type: string, var_name: string) -> string {
    -- fptr_type is like "int64_t (*)(int64_t, dm_string)"
    -- We need to insert var_name between (* and )
    -- Find "(*)" and replace with "(*var_name)"
    return string_replace(fptr_type, "(*)", "(*" + var_name + ")")
}

-- Build a C function pointer type string: "ret_type (*)(param_types...)"
fn build_fptr_type(ret_type: string, param_types: List[string]) -> string {
    let mut fptr = ret_type + " (*)("
    if param_types.len() == 0 {
        fptr = fptr + "void"
    } else {
        let mut ti = 0
        while ti < param_types.len() {
            if ti > 0 {
                fptr = fptr + ", "
            }
            fptr = fptr + param_types[ti]
            ti = ti + 1
        }
    }
    fptr = fptr + ")"
    return fptr
}

-- Look up generic function info: returns "type_params:start:end" or "" if not generic
fn lookup_generic_fn(c: Compiler, name: string) -> string {
    let marker = "|" + name + "="
    let pos = string_find(c.generic_fn_tokens, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.generic_fn_tokens, start, len(c.generic_fn_tokens) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

-- Check if a function name is a generic function
fn is_generic_fn(c: Compiler, name: string) -> bool {
    let info = lookup_generic_fn(c, name)
    return info != ""
}

-- Monomorphize a generic function for concrete type arguments.
-- generic_info is "T:start_idx:end_idx" (from lookup_generic_fn)
-- concrete_types is "int64_t" or "dm_string" etc (comma-separated for multiple type params)
-- Returns updated Compiler with the monomorphized function added to fn_defs/fn_sigs
fn monomorphize_generic_fn(c: Compiler, fn_name: string, generic_info: string, concrete_types: string) -> Compiler {
    let mut cc = c

    -- Parse generic_info: "T:start:end" or "T,U:start:end"
    let first_colon = string_find(generic_info, ":")
    if first_colon < 0 { return cc }
    let type_params_str = substr(generic_info, 0, first_colon)
    let rest_after_params = substr(generic_info, first_colon + 1, len(generic_info) - first_colon - 1)
    let second_colon = string_find(rest_after_params, ":")
    if second_colon < 0 { return cc }
    let start_str = substr(rest_after_params, 0, second_colon)
    let end_str = substr(rest_after_params, second_colon + 1, len(rest_after_params) - second_colon - 1)
    let tok_start = parse_int(start_str)
    let tok_end = parse_int(end_str)

    -- Build mangled name: dm_fn_name_concreteType
    let mangled = dm_mangle(fn_name) + "_" + concrete_types
    let mono_marker = "|" + mangled + "|"

    -- Check if already monomorphized
    if string_contains(cc.monomorphized_fns, mono_marker) { return cc }
    cc.monomorphized_fns = cc.monomorphized_fns + mono_marker

    -- Extract tokens for this generic function and create substituted copy
    -- The token range [tok_start..tok_end) covers "fn name[T](...) -> Ret { body }"
    -- We need to:
    -- 1. Copy these tokens with T replaced by concrete type
    -- 2. Replace the function name with the mangled version
    -- 3. Skip the [T] part
    -- 4. Compile the result as a regular function

    let mut new_tokens: List[Token] = []
    let mut ti = tok_start
    let mut skip_bracket = false

    while ti < tok_end {
        let t = cc.tokens[ti]

        -- Skip the [T] generic parameter declaration
        if t.kind == TK_LBRACKET() and ti == tok_start + 2 {
            -- Skip until ]
            ti = ti + 1
            while ti < tok_end and cc.tokens[ti].kind != TK_RBRACKET() {
                ti = ti + 1
            }
            ti = ti + 1  -- skip ]
            continue
        }

        -- Replace function name with monomorphized name (without dm_ prefix — compile_fn_decl will add it)
        if t.kind == TK_IDENT() and t.value == fn_name and ti == tok_start + 1 {
            let mono_name = fn_name + "_" + concrete_types
            new_tokens.push(token_new(TK_IDENT(), mono_name, t.line, t.col))
            ti = ti + 1
            continue
        }

        -- Replace type parameter with concrete type
        if t.kind == TK_IDENT() and t.value == type_params_str {
            -- Map concrete type back to dAImond type name for the parser
            let mut dm_type_name = concrete_types
            if concrete_types == "int64_t" {
                dm_type_name = "int"
            } else if concrete_types == "dm_string" {
                dm_type_name = "string"
            } else if concrete_types == "bool" {
                dm_type_name = "bool"
            } else if concrete_types == "double" {
                dm_type_name = "float"
            }
            new_tokens.push(token_new(TK_IDENT(), dm_type_name, t.line, t.col))
            ti = ti + 1
            continue
        }

        new_tokens.push(t)
        ti = ti + 1
    }

    -- Add EOF token
    new_tokens.push(token_new(TK_EOF(), "", 0, 0))

    -- Compile the monomorphized function using a temporary compiler
    let mut temp_cc = compiler_new(new_tokens)
    -- Copy over important state from the real compiler
    temp_cc.struct_names = cc.struct_names
    temp_cc.enum_names = cc.enum_names
    temp_cc.fn_names = cc.fn_names
    temp_cc.fn_ret_types = cc.fn_ret_types
    temp_cc.struct_fields = cc.struct_fields
    temp_cc.enum_variants = cc.enum_variants
    temp_cc.var_types = cc.var_types
    temp_cc.str_vars = cc.str_vars
    temp_cc.list_type_defs = cc.list_type_defs
    temp_cc.option_type_defs = cc.option_type_defs
    temp_cc.list_elem_types = cc.list_elem_types
    temp_cc.generic_fn_tokens = cc.generic_fn_tokens
    temp_cc.monomorphized_fns = cc.monomorphized_fns

    -- Skip 'fn' keyword to enter compile_fn_decl properly
    temp_cc = compile_fn_decl(temp_cc)

    -- Copy generated function back to real compiler
    let mut fi = 0
    while fi < temp_cc.fn_sigs.len() {
        cc.fn_sigs.push(temp_cc.fn_sigs[fi])
        fi = fi + 1
    }
    fi = 0
    while fi < temp_cc.fn_defs.len() {
        cc.fn_defs.push(temp_cc.fn_defs[fi])
        fi = fi + 1
    }
    -- Copy monomorphized tracking
    cc.monomorphized_fns = temp_cc.monomorphized_fns
    -- Copy any new list/option type defs
    cc.list_type_defs = temp_cc.list_type_defs
    cc.option_type_defs = temp_cc.option_type_defs
    -- Copy fn_names/ret_types (the monomorphized fn was added)
    cc.fn_names = temp_cc.fn_names
    cc.fn_ret_types = temp_cc.fn_ret_types

    return cc
}

fn compile_primary_expr(c: Compiler) -> ExprOut {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)
    let k = tok.kind

    if k == TK_INTEGER() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: tok.value }
    }
    if k == TK_FLOAT() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: tok.value }
    }
    if k == TK_STRING() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "dm_string_from_cstr(\"" + tok.value + "\")" }
    }
    if k == TK_TRUE() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "true" }
    }
    if k == TK_FALSE() {
        cc = c_advance(cc)
        return ExprOut { c: cc, code: "false" }
    }
    if k == TK_IDENT() {
        let name = tok.value
        cc = c_advance(cc)
        -- Check for builtin function mapping
        if name == "println" {
            return ExprOut { c: cc, code: "dm_println_str" }
        }
        if name == "print" {
            return ExprOut { c: cc, code: "dm_print_str" }
        }
        if name == "eprintln" {
            return ExprOut { c: cc, code: "dm_eprintln_str" }
        }
        if name == "int_to_string" {
            return ExprOut { c: cc, code: "dm_int_to_string" }
        }
        if name == "bool_to_string" {
            return ExprOut { c: cc, code: "dm_bool_to_string" }
        }
        if name == "float_to_string" {
            return ExprOut { c: cc, code: "dm_float_to_string" }
        }
        if name == "char_at" {
            return ExprOut { c: cc, code: "dm_char_at" }
        }
        if name == "len" {
            return ExprOut { c: cc, code: "dm_len" }
        }
        if name == "substr" {
            return ExprOut { c: cc, code: "dm_string_substr" }
        }
        if name == "parse_int" {
            return ExprOut { c: cc, code: "dm_parse_int" }
        }
        if name == "string_contains" {
            return ExprOut { c: cc, code: "dm_string_contains" }
        }
        if name == "string_find" {
            return ExprOut { c: cc, code: "dm_string_find" }
        }
        if name == "starts_with" {
            return ExprOut { c: cc, code: "dm_string_starts_with" }
        }
        if name == "ends_with" {
            return ExprOut { c: cc, code: "dm_string_ends_with" }
        }
        if name == "string_replace" {
            return ExprOut { c: cc, code: "dm_string_replace" }
        }
        if name == "string_trim" {
            return ExprOut { c: cc, code: "dm_string_trim" }
        }
        if name == "file_read" {
            return ExprOut { c: cc, code: "dm_file_read" }
        }
        if name == "file_write" {
            return ExprOut { c: cc, code: "dm_file_write" }
        }
        if name == "exit" {
            return ExprOut { c: cc, code: "dm_exit" }
        }
        if name == "system" {
            return ExprOut { c: cc, code: "dm_system" }
        }
        if name == "args_get" {
            return ExprOut { c: cc, code: "dm_args_get" }
        }
        if name == "args_len" {
            return ExprOut { c: cc, code: "dm_args_len" }
        }
        if name == "panic" {
            return ExprOut { c: cc, code: "dm_panic" }
        }
        -- Option/Result constructors: Some, None, Ok, Err
        -- Use expected_type or current_fn_ret_type for type context
        if name == "Some" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_option_") {
                -- Some(value) -> dm_option_T_Some(value)
                -- The value will be parsed by the subsequent LPAREN in postfix
                return ExprOut { c: cc, code: etype + "_Some" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "None" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_option_") {
                return ExprOut { c: cc, code: etype + "_None()" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "Ok" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_result_") {
                return ExprOut { c: cc, code: etype + "_Ok" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        if name == "Err" {
            let etype = cc.expected_type
            if etype != "" and starts_with(etype, "dm_result_") {
                return ExprOut { c: cc, code: etype + "_Err" }
            }
            return ExprOut { c: cc, code: dm_mangle(name) }
        }
        -- Check for struct literal: Name { field: value, ... }
        -- Only parse as struct literal if name is a known struct type
        let struct_try = try_compile_struct_lit(cc, name)
        if struct_try.code != "" {
            return struct_try
        }
        -- Check for generic function with explicit type params: name[Type](...)
        let is_gen = is_generic_fn(cc, name)
        if is_gen and c_peek(cc) == TK_LBRACKET() {
            cc = c_advance(cc)  -- skip '['
            -- Parse the concrete type argument
            let ct = parse_type_for_c(cc)
            cc = ct.c
            cc = c_expect(cc, TK_RBRACKET())
            -- Monomorphize the function
            let ginfo = lookup_generic_fn(cc, name)
            cc = monomorphize_generic_fn(cc, name, ginfo, ct.code)
            -- Return the mangled name so the call compiles normally
            return ExprOut { c: cc, code: dm_mangle(name) + "_" + ct.code }
        }
        -- Regular identifier -> dm_ prefix for user functions
        return ExprOut { c: cc, code: dm_mangle(name) }
    }
    if k == TK_LPAREN() {
        cc = c_advance(cc)
        let inner = compile_expr(cc)
        cc = inner.c
        cc = c_expect(cc, TK_RPAREN())
        return ExprOut { c: cc, code: "(" + inner.code + ")" }
    }
    if k == TK_MATCH() {
        return compile_match_expr(cc)
    }
    if k == TK_PIPE() {
        return compile_lambda_expr(cc)
    }
    -- Unknown primary
    cc = c_error(cc, "unexpected token in expression: " + token_kind_name(k))
    cc = c_advance(cc)
    return ExprOut { c: cc, code: "0 /* error */" }
}

-- [module: compile_stmt]


-- ============================================================
-- STATEMENT COMPILER
-- ============================================================

fn compile_stmt(c: Compiler) -> Compiler {
    let mut cc = c_skip_nl(c)
    let k = c_peek(cc)
    let ind = indent_str(cc.indent)

    if k == TK_LET() {
        return compile_let_stmt(cc)
    }
    if k == TK_RETURN() {
        cc = c_advance(cc)
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_NEWLINE() or c_peek(cc) == TK_RBRACE() or c_peek(cc) == TK_EOF() {
            cc.output = cc.output + ind + "return;\n"
        } else {
            -- Set expected type for Some/None/Ok/Err from fn return type
            let saved_expected = cc.expected_type
            if cc.current_fn_ret_type != "" {
                cc.expected_type = cc.current_fn_ret_type
            }
            let val = compile_expr(cc)
            cc = val.c
            cc.expected_type = saved_expected
            cc.output = cc.output + ind + "return " + val.code + ";\n"
        }
        return cc
    }
    if k == TK_IF() {
        return compile_if_stmt(cc)
    }
    if k == TK_WHILE() {
        return compile_while_stmt(cc)
    }
    if k == TK_FOR() {
        return compile_for_stmt(cc)
    }
    if k == TK_BREAK() {
        cc = c_advance(cc)
        cc.output = cc.output + ind + "break;\n"
        return cc
    }
    if k == TK_CONTINUE() {
        cc = c_advance(cc)
        cc.output = cc.output + ind + "continue;\n"
        return cc
    }
    if k == TK_LOOP() {
        return compile_loop_stmt(cc)
    }
    if k == TK_MATCH() {
        return compile_match_stmt(cc)
    }
    -- Expression statement (possibly assignment)
    let lhs = compile_expr(cc)
    cc = lhs.c
    -- Check for assignment
    if c_peek(cc) == TK_EQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = " + rhs.code + ";\n"
    } else if c_peek(cc) == TK_PLUSEQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = (" + lhs.code + " + " + rhs.code + ");\n"
    } else if c_peek(cc) == TK_MINUSEQ() {
        cc = c_advance(cc)
        let rhs = compile_expr(cc)
        cc = rhs.c
        cc.output = cc.output + ind + lhs.code + " = (" + lhs.code + " - " + rhs.code + ");\n"
    } else {
        cc.output = cc.output + ind + lhs.code + ";\n"
    }
    return cc
}

fn lookup_fn_ret_type(c: Compiler, name: string) -> string {
    let mut i = 0
    while i < c.fn_names.len() {
        -- Use "" + to force string concat context (Stage 0 workaround)
        let cur = "" + c.fn_names[i]
        if cur == name {
            let ret = "" + c.fn_ret_types[i]
            return ret
        }
        i = i + 1
    }
    return ""
}

fn is_struct_name(c: Compiler, name: string) -> bool {
    let mut i = 0
    while i < c.struct_names.len() {
        let cur = "" + c.struct_names[i]
        if cur == name {
            return true
        }
        i = i + 1
    }
    return false
}

fn lookup_list_elem_type(c: Compiler, list_var: string) -> string {
    let marker = "|" + list_var + "="
    let pos = string_find(c.list_elem_types, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.list_elem_types, start, len(c.list_elem_types) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

fn infer_list_get_type(code: string, c: Compiler) -> string {
    if starts_with(code, "DM_LIST_GET(") == false { return "" }
    let paren = string_find(code, "(")
    let comma = string_find(code, ",")
    if comma < 0 { return "" }
    let list_var = substr(code, paren + 1, comma - paren - 1)
    -- Try direct lookup first
    let direct = lookup_list_elem_type(c, list_var)
    if direct != "" { return direct }
    -- Try struct field access: dm_var.field -> look up field type
    let dot_pos = string_find(list_var, ".")
    if dot_pos < 0 { return "" }
    let var_part = substr(list_var, 0, dot_pos)
    let field_part = substr(list_var, dot_pos + 1, len(list_var) - dot_pos - 1)
    let var_type = lookup_var_type(c, var_part)
    if var_type == "" { return "" }
    let field_type = lookup_struct_field_type(c, var_type, field_part)
    if field_type == "" { return "" }
    -- field_type should be like "dm_list_dm_Token" -> extract element type after "dm_list_"
    if starts_with(field_type, "dm_list_") == false { return "" }
    return substr(field_type, 8, len(field_type) - 8)
}

fn track_var_type(c: Compiler, mangled_name: string, c_type: string) -> Compiler {
    let mut cc = c
    cc.var_types = cc.var_types + "|" + mangled_name + "=" + c_type + "|"
    return cc
}

fn lookup_var_type(c: Compiler, mangled_name: string) -> string {
    let marker = "|" + mangled_name + "="
    let pos = string_find(c.var_types, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.var_types, start, len(c.var_types) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

fn infer_fn_call_type(code: string, c: Compiler) -> string {
    if starts_with(code, "dm_") == false { return "" }
    if string_contains(code, "(") == false { return "" }
    let paren_pos = string_find(code, "(")
    let fn_name = substr(code, 3, paren_pos - 3)
    let ret = lookup_fn_ret_type(c, fn_name)
    if ret == "" { return "" }
    if ret == "void" { return "" }
    return ret
}

fn infer_struct_lit_type(code: string) -> string {
    if starts_with(code, "(dm_") == false { return "" }
    if string_contains(code, "){") == false { return "" }
    let close = string_find(code, ")")
    let type_name = substr(code, 1, close - 1)
    if string_contains(type_name, " ") { return "" }
    return type_name
}

fn infer_enum_ctor_type(code: string, c: Compiler) -> string {
    -- Detect enum constructor: dm_EnumName_Variant(...) or dm_EnumName_Variant()
    if starts_with(code, "dm_") == false { return "" }
    let paren_pos = string_find(code, "(")
    if paren_pos < 0 { return "" }
    let ctor_name = substr(code, 3, paren_pos - 3)
    -- Check each enum name to see if ctor_name starts with it + "_"
    let mut i = 0
    while i < c.enum_names.len() {
        let ename = "" + c.enum_names[i]
        let prefix = ename + "_"
        if starts_with(ctor_name, prefix) {
            return "dm_" + ename
        }
        i = i + 1
    }
    -- Check for Option/Result constructors (flattened to avoid nested if issue)
    if starts_with(code, "dm_option_") and string_contains(code, "_Some(") {
        let pos = string_find(code, "_Some(")
        return substr(code, 0, pos)
    }
    if starts_with(code, "dm_option_") and string_contains(code, "_None(") {
        let pos = string_find(code, "_None(")
        return substr(code, 0, pos)
    }
    if starts_with(code, "dm_result_") and string_contains(code, "_Ok(") {
        let pos = string_find(code, "_Ok(")
        return substr(code, 0, pos)
    }
    if starts_with(code, "dm_result_") and string_contains(code, "_Err(") {
        let pos = string_find(code, "_Err(")
        return substr(code, 0, pos)
    }
    return ""
}

fn infer_type_from_code(code: string, c: Compiler) -> string {
    if code_is_string(code, c) { return "dm_string" }
    if code == "true" { return "bool" }
    if code == "false" { return "bool" }
    -- Struct literal: (dm_TypeName){ ... }
    let maybe_struct_type = infer_struct_lit_type(code)
    if maybe_struct_type != "" { return maybe_struct_type }
    -- Enum constructor: dm_EnumName_Variant(...)
    let maybe_enum_type = infer_enum_ctor_type(code, c)
    if maybe_enum_type != "" { return maybe_enum_type }
    -- List element access: DM_LIST_GET(dm_var, idx)
    let list_elem = infer_list_get_type(code, c)
    if list_elem != "" { return list_elem }
    -- Function call: dm_foo(...) - look up return type
    let fn_ret = infer_fn_call_type(code, c)
    if fn_ret != "" { return fn_ret }
    -- Variable reference: look up known variable types
    let var_type = lookup_var_type(c, code)
    if var_type != "" { return var_type }
    return "int64_t"
}

fn track_str_var(c: Compiler, mangled_name: string, is_str: bool) -> Compiler {
    let mut cc = c
    if is_str == false { return cc }
    cc.str_vars.push(mangled_name)
    return cc
}

fn track_list_elem_type(c: Compiler, type_str: string, var_name: string) -> Compiler {
    let mut cc = c
    if starts_with(type_str, "dm_list_") == false { return cc }
    let elem_t = substr(type_str, 8, len(type_str) - 8)
    let mname = dm_mangle(var_name)
    cc.list_elem_types = cc.list_elem_types + "|" + mname + "=" + elem_t + "|"
    return cc
}

fn compile_let_list_init(c: Compiler, ind: string, type_str: string, var_name: string) -> Compiler {
    let mut cc = c_advance(c)  -- skip '['
    cc = track_list_elem_type(cc, type_str, var_name)
    cc = track_var_type(cc, dm_mangle(var_name), type_str)
    let mname = dm_mangle(var_name)
    if c_peek(cc) == TK_RBRACKET() {
        -- Empty list: []
        cc = c_advance(cc)
        cc.output = cc.output + ind + type_str + " " + mname + " = " + type_str + "_new();\n"
        return cc
    }
    -- Non-empty list literal: [a, b, c]
    cc.output = cc.output + ind + type_str + " " + mname + " = " + type_str + "_new();\n"
    let mut first_el = true
    while c_peek(cc) != TK_RBRACKET() and c_peek(cc) != TK_EOF() {
        if first_el == false {
            cc = c_expect(cc, TK_COMMA())
        }
        first_el = false
        let elem = compile_expr(cc)
        cc = elem.c
        cc.output = cc.output + ind + type_str + "_push(&" + mname + ", " + elem.code + ");\n"
    }
    cc = c_expect(cc, TK_RBRACKET())
    return cc
}

fn compile_let_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'let'
    let ind = indent_str(cc.indent)
    let mut is_mut = false
    if c_peek(cc) == TK_MUT() {
        is_mut = true
        cc = c_advance(cc)
    }
    let name_tok = c_cur(cc)
    let var_name = name_tok.value
    cc = c_advance(cc)
    -- Optional type annotation
    let mut type_str = ""
    let mut has_type_ann = false
    if c_peek(cc) == TK_COLON() {
        cc = c_advance(cc)
        let tr = parse_type_for_c(cc)
        cc = tr.c
        type_str = tr.code
        has_type_ann = true
    }
    -- = value
    cc = c_expect(cc, TK_EQ())
    -- Handle list literals: [] or [a, b, c]
    if c_peek(cc) == TK_LBRACKET() {
        return compile_let_list_init(cc, ind, type_str, var_name)
    }
    -- Set expected type for Some/None/Ok/Err inference
    let saved_expected = cc.expected_type
    if has_type_ann {
        cc.expected_type = type_str
    }
    let val = compile_expr(cc)
    cc = val.c
    cc.expected_type = saved_expected
    -- Infer type from value if no annotation
    if has_type_ann == false {
        type_str = infer_type_from_code(val.code, cc)
    }
    -- Track string variables (use a flag to avoid ternary-in-if issue)
    let is_string_type = type_str == "dm_string"
    cc = track_str_var(cc, dm_mangle(var_name), is_string_type)
    -- Track list variable element types
    cc = track_list_elem_type(cc, type_str, var_name)
    -- Track variable type for inference
    cc = track_var_type(cc, dm_mangle(var_name), type_str)
    -- Function pointer types need special declaration syntax: ret (*name)(params)
    let is_fptr = string_contains(type_str, "(*)")
    if is_fptr {
        let decl = emit_fptr_decl(type_str, dm_mangle(var_name))
        cc.output = cc.output + ind + decl + " = " + val.code + ";\n"
    } else {
        cc.output = cc.output + ind + type_str + " " + dm_mangle(var_name) + " = " + val.code + ";\n"
    }
    return cc
}

struct TypeCResult {
    c: Compiler,
    code: string
}

fn register_list_type(c: Compiler, list_type: string, elem_type: string) -> Compiler {
    let mut cc = c
    -- Check if already registered (search in the defs string)
    if string_contains(cc.list_type_defs, "} " + list_type + ";") { return cc }
    cc.list_type_defs = cc.list_type_defs + emit_list_type_def(list_type, elem_type)
    return cc
}

fn emit_option_type_def(opt_type: string, val_type: string) -> string {
    let mut d = "typedef enum " + opt_type + "_tag {\n"
    d = d + "    " + opt_type + "_tag_None,\n"
    d = d + "    " + opt_type + "_tag_Some\n"
    d = d + "} " + opt_type + "_tag;\n\n"
    d = d + "typedef struct " + opt_type + " {\n"
    d = d + "    " + opt_type + "_tag tag;\n"
    d = d + "    union { struct { " + val_type + " _0; } Some; } data;\n"
    d = d + "} " + opt_type + ";\n\n"
    d = d + "static inline " + opt_type + " " + opt_type + "_None(void) {\n"
    d = d + "    " + opt_type + " _r; _r.tag = " + opt_type + "_tag_None; return _r;\n"
    d = d + "}\n\n"
    d = d + "static inline " + opt_type + " " + opt_type + "_Some(" + val_type + " _0) {\n"
    d = d + "    " + opt_type + " _r; _r.tag = " + opt_type + "_tag_Some; _r.data.Some._0 = _0; return _r;\n"
    d = d + "}\n\n"
    return d
}

fn emit_result_type_def(res_type: string, ok_type: string, err_type: string) -> string {
    let mut d = "typedef enum " + res_type + "_tag {\n"
    d = d + "    " + res_type + "_tag_Ok,\n"
    d = d + "    " + res_type + "_tag_Err\n"
    d = d + "} " + res_type + "_tag;\n\n"
    d = d + "typedef struct " + res_type + " {\n"
    d = d + "    " + res_type + "_tag tag;\n"
    d = d + "    union {\n"
    d = d + "        struct { " + ok_type + " _0; } Ok;\n"
    d = d + "        struct { " + err_type + " _0; } Err;\n"
    d = d + "    } data;\n"
    d = d + "} " + res_type + ";\n\n"
    d = d + "static inline " + res_type + " " + res_type + "_Ok(" + ok_type + " _0) {\n"
    d = d + "    " + res_type + " _r; _r.tag = " + res_type + "_tag_Ok; _r.data.Ok._0 = _0; return _r;\n"
    d = d + "}\n\n"
    d = d + "static inline " + res_type + " " + res_type + "_Err(" + err_type + " _0) {\n"
    d = d + "    " + res_type + " _r; _r.tag = " + res_type + "_tag_Err; _r.data.Err._0 = _0; return _r;\n"
    d = d + "}\n\n"
    return d
}

fn register_option_type(c: Compiler, opt_type: string, val_type: string) -> Compiler {
    let mut cc = c
    if string_contains(cc.option_type_defs, "} " + opt_type + ";") { return cc }
    cc.option_type_defs = cc.option_type_defs + emit_option_type_def(opt_type, val_type)
    -- Register enum variants for match/constructor use
    cc.enum_variants = cc.enum_variants + "|" + opt_type + ".None=unit|"
    cc.enum_variants = cc.enum_variants + "|" + opt_type + ".Some=tuple:" + val_type + "|"
    return cc
}

fn register_result_type(c: Compiler, res_type: string, ok_type: string, err_type: string) -> Compiler {
    let mut cc = c
    if string_contains(cc.option_type_defs, "} " + res_type + ";") { return cc }
    cc.option_type_defs = cc.option_type_defs + emit_result_type_def(res_type, ok_type, err_type)
    -- Register enum variants for match/constructor use
    cc.enum_variants = cc.enum_variants + "|" + res_type + ".Ok=tuple:" + ok_type + "|"
    cc.enum_variants = cc.enum_variants + "|" + res_type + ".Err=tuple:" + err_type + "|"
    return cc
}

fn parse_type_for_c(c: Compiler) -> TypeCResult {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)
    if tok.kind != TK_IDENT() {
        return TypeCResult { c: cc, code: "int64_t" }
    }
    let name = tok.value
    cc = c_advance(cc)
    -- Check for generic [T] or [T, E]
    if c_peek(cc) == TK_LBRACKET() {
        cc = c_advance(cc)
        let inner = parse_type_for_c(cc)
        cc = inner.c
        -- Check for second type arg (Result[T, E])
        let mut second_type = ""
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
            let inner2 = parse_type_for_c(cc)
            cc = inner2.c
            second_type = inner2.code
        }
        cc = c_expect(cc, TK_RBRACKET())
        if name == "List" {
            let list_t = "dm_list_" + inner.code
            cc = register_list_type(cc, list_t, inner.code)
            return TypeCResult { c: cc, code: list_t }
        }
        if name == "Box" {
            return TypeCResult { c: cc, code: inner.code + "*" }
        }
        if name == "Option" {
            let opt_t = "dm_option_" + inner.code
            cc = register_option_type(cc, opt_t, inner.code)
            return TypeCResult { c: cc, code: opt_t }
        }
        if name == "Result" and second_type != "" {
            let res_t = "dm_result_" + inner.code + "_" + second_type
            cc = register_result_type(cc, res_t, inner.code, second_type)
            return TypeCResult { c: cc, code: res_t }
        }
        return TypeCResult { c: cc, code: "dm_" + name + "_" + inner.code }
    }
    let mapped = map_dm_type(name)
    return TypeCResult { c: cc, code: mapped }
}

fn compile_if_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'if'
    let ind = indent_str(cc.indent)
    let cond = compile_expr(cc)
    cc = cond.c
    cc.output = cc.output + ind + "if (" + cond.code + ") {\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}"
    -- Check for else
    let mut cc2 = c_skip_nl(cc)
    if c_peek(cc2) != TK_ELSE() {
        cc.output = cc.output + "\n"
        return cc
    }
    cc2 = c_advance(cc2)
    cc2 = c_skip_nl(cc2)
    if c_peek(cc2) == TK_IF() {
        cc2.output = cc2.output + " else "
        return compile_if_stmt(cc2)
    }
    cc2.output = cc2.output + " else {\n"
    cc2 = c_expect(cc2, TK_LBRACE())
    cc2.indent = cc2.indent + 1
    cc2 = compile_block_body(cc2)
    cc2 = c_expect(cc2, TK_RBRACE())
    cc2.indent = cc2.indent - 1
    cc2.output = cc2.output + ind + "}\n"
    return cc2
}

fn compile_while_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'while'
    let ind = indent_str(cc.indent)
    let cond = compile_expr(cc)
    cc = cond.c
    cc.output = cc.output + ind + "while (" + cond.code + ") {\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}\n"
    return cc
}

fn compile_for_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'for'
    let ind = indent_str(cc.indent)
    let var_tok = c_cur(cc)
    let var_name = var_tok.value
    cc = c_advance(cc)
    cc = c_expect(cc, TK_IN())
    let start_expr = compile_expr(cc)
    cc = start_expr.c
    -- Check for range: start..end or start..=end
    if c_peek(cc) == TK_DOTDOT() {
        cc = c_advance(cc)
        let end_expr = compile_expr(cc)
        cc = end_expr.c
        cc.output = cc.output + ind + "for (int64_t " + dm_mangle(var_name) + " = " + start_expr.code + "; " + dm_mangle(var_name) + " < " + end_expr.code + "; " + dm_mangle(var_name) + "++) {\n"
        cc = c_expect(cc, TK_LBRACE())
        cc.indent = cc.indent + 1
        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())
        cc.indent = cc.indent - 1
        cc.output = cc.output + ind + "}\n"
        return cc
    }
    if c_peek(cc) == TK_DOTDOTEQ() {
        cc = c_advance(cc)
        let end_expr = compile_expr(cc)
        cc = end_expr.c
        cc.output = cc.output + ind + "for (int64_t " + dm_mangle(var_name) + " = " + start_expr.code + "; " + dm_mangle(var_name) + " <= " + end_expr.code + "; " + dm_mangle(var_name) + "++) {\n"
        cc = c_expect(cc, TK_LBRACE())
        cc.indent = cc.indent + 1
        cc = compile_block_body(cc)
        cc = c_expect(cc, TK_RBRACE())
        cc.indent = cc.indent - 1
        cc.output = cc.output + ind + "}\n"
        return cc
    }
    -- List iteration: for x in list { ... }
    cc.output = cc.output + ind + "for (size_t _fi = 0; _fi < " + start_expr.code + ".len; _fi++) {\n"
    cc.output = cc.output + ind + "    int64_t " + dm_mangle(var_name) + " = " + start_expr.code + ".data[_fi];\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}\n"
    return cc
}

fn compile_loop_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'loop'
    let ind = indent_str(cc.indent)
    cc.output = cc.output + ind + "while (1) {\n"
    cc = c_expect(cc, TK_LBRACE())
    cc.indent = cc.indent + 1
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    cc.indent = cc.indent - 1
    cc.output = cc.output + ind + "}\n"
    return cc
}

fn compile_block_body(c: Compiler) -> Compiler {
    let mut cc = c
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = compile_stmt(cc)
        cc = c_skip_nl(cc)
    }
    return cc
}

-- [module: compile_match]


-- ============================================================
-- MATCH EXPRESSION/STATEMENT COMPILER
-- ============================================================

fn infer_field_type_from_code(code: string, c: Compiler) -> string {
    let dot_pos = string_find(code, ".")
    if dot_pos < 0 { return "" }
    let var_part = substr(code, 0, dot_pos)
    let field_part = substr(code, dot_pos + 1, len(code) - dot_pos - 1)
    let vtype = lookup_var_type(c, var_part)
    if vtype == "" { return "" }
    return lookup_struct_field_type(c, vtype, field_part)
}

-- Determine the enum type from a match subject C code string
fn infer_match_subject_type(code: string, c: Compiler) -> string {
    -- Look up variable type from var_types
    let vt = lookup_var_type(c, code)
    if vt != "" { return vt }
    -- Try struct field access (delegated to helper to avoid nested-if codegen bug)
    let field_result = infer_field_type_from_code(code, c)
    if field_result != "" { return field_result }
    -- Try function call inference
    let fn_ret = infer_fn_call_type(code, c)
    if fn_ret != "" { return fn_ret }
    return ""
}

-- Compile a match arm pattern and return the condition code and any bindings
-- Pattern kinds:
--   EnumName.Variant(x, y) -> tag check + bindings
--   EnumName.Variant -> tag check (unit)
--   _ -> wildcard (always matches)
--   literal int/string/bool -> equality check
--   ident -> catch-all binding
struct MatchArm {
    c: Compiler,
    condition: string,
    bindings: string
}

fn compile_match_pattern(c: Compiler, subject_code: string, subject_type: string) -> MatchArm {
    let mut cc = c_skip_nl(c)
    let tok = c_cur(cc)

    -- Wildcard: _
    if tok.kind == TK_UNDERSCORE() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "", bindings: "" }
    }

    -- Literal integer
    if tok.kind == TK_INTEGER() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "(" + subject_code + " == " + tok.value + ")", bindings: "" }
    }

    -- Literal string
    if tok.kind == TK_STRING() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "dm_string_eq(" + subject_code + ", dm_string_from_cstr(\"" + tok.value + "\"))", bindings: "" }
    }

    -- true/false
    if tok.kind == TK_TRUE() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "(" + subject_code + " == true)", bindings: "" }
    }
    if tok.kind == TK_FALSE() {
        cc = c_advance(cc)
        return MatchArm { c: cc, condition: "(" + subject_code + " == false)", bindings: "" }
    }

    -- Identifier: could be EnumName.Variant or catch-all binding
    if tok.kind == TK_IDENT() {
        let name = tok.value
        cc = c_advance(cc)

        -- Check for EnumName.Variant pattern
        if c_peek(cc) == TK_DOT() {
            cc = c_advance(cc)
            let vtok = c_cur(cc)
            let variant = vtok.value
            cc = c_advance(cc)

            -- Determine the C type name for the enum
            let mangled_enum = dm_mangle(name)

            -- Check if it's a tagged union (has variant info)
            let vinfo = lookup_enum_variant(cc, name, variant)
            -- Also check for Option/Result with dm_ prefix
            let vinfo2 = lookup_enum_variant(cc, subject_type, variant)

            let mut use_type = mangled_enum
            let mut use_vinfo = vinfo
            if vinfo == "" and vinfo2 != "" {
                use_type = subject_type
                use_vinfo = vinfo2
            }

            let condition = "(" + subject_code + ".tag == " + use_type + "_tag_" + variant + ")"

            -- Check for payload bindings: (x, y)
            if c_peek(cc) == TK_LPAREN() {
                cc = c_advance(cc)  -- skip '('
                let mut bindings = ""
                let ind = indent_str(cc.indent + 1)
                let mut field_idx = 0

                -- Parse the payload types from vinfo to get C types
                let mut payload_types = ""
                if starts_with(use_vinfo, "tuple:") {
                    payload_types = substr(use_vinfo, 6, len(use_vinfo) - 6)
                }

                while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
                    if field_idx > 0 {
                        cc = c_expect(cc, TK_COMMA())
                    }
                    let bind_tok = c_cur(cc)
                    cc = c_advance(cc)

                    -- Get the type for this field from payload types
                    let mut field_type = "int64_t"
                    if payload_types != "" {
                        let comma_pos = string_find(payload_types, ",")
                        if comma_pos < 0 {
                            field_type = payload_types
                        } else {
                            field_type = substr(payload_types, 0, comma_pos)
                            payload_types = substr(payload_types, comma_pos + 1, len(payload_types) - comma_pos - 1)
                        }
                    }

                    if bind_tok.value != "_" {
                        bindings = bindings + ind + field_type + " " + dm_mangle(bind_tok.value) + " = " + subject_code + ".data." + variant + "._" + int_to_string(field_idx) + ";\n"
                        -- Track the binding variable type
                        cc = track_var_type(cc, dm_mangle(bind_tok.value), field_type)
                        let is_str = field_type == "dm_string"
                        cc = track_str_var(cc, dm_mangle(bind_tok.value), is_str)
                    }
                    field_idx = field_idx + 1
                }
                cc = c_expect(cc, TK_RPAREN())
                return MatchArm { c: cc, condition: condition, bindings: bindings }
            }
            return MatchArm { c: cc, condition: condition, bindings: "" }
        }

        -- Simple identifier - catch-all binding
        let ind = indent_str(cc.indent + 1)
        let inferred = infer_match_subject_type(subject_code, cc)
        let mut bind_type = "int64_t"
        if inferred != "" {
            bind_type = inferred
        }
        let bindings = ind + bind_type + " " + dm_mangle(name) + " = " + subject_code + ";\n"
        cc = track_var_type(cc, dm_mangle(name), bind_type)
        return MatchArm { c: cc, condition: "", bindings: bindings }
    }

    -- Fallback
    return MatchArm { c: cc, condition: "", bindings: "" }
}

fn compile_match_stmt(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'match'
    let ind = indent_str(cc.indent)
    let subject = compile_expr(cc)
    cc = subject.c
    let subject_code = subject.code
    let subject_type = infer_type_from_code(subject_code, cc)
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)

    let mut first_arm = true
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_RBRACE() { break }

        let arm = compile_match_pattern(cc, subject_code, subject_type)
        cc = arm.c

        cc = c_skip_nl(cc)
        cc = c_expect(cc, TK_FAT_ARROW())
        cc = c_skip_nl(cc)

        if arm.condition == "" {
            -- Wildcard or catch-all
            if first_arm {
                cc.output = cc.output + ind + "{\n"
            } else {
                cc.output = cc.output + ind + "} else {\n"
            }
        } else {
            if first_arm {
                cc.output = cc.output + ind + "if " + arm.condition + " {\n"
            } else {
                cc.output = cc.output + ind + "} else if " + arm.condition + " {\n"
            }
        }
        first_arm = false

        -- Emit bindings
        cc.output = cc.output + arm.bindings

        -- Compile arm body: either a block { ... } or a single expression
        if c_peek(cc) == TK_LBRACE() {
            cc = c_expect(cc, TK_LBRACE())
            cc.indent = cc.indent + 1
            cc = compile_block_body(cc)
            cc = c_expect(cc, TK_RBRACE())
            cc.indent = cc.indent - 1
        } else {
            -- Single expression as statement
            cc.indent = cc.indent + 1
            let body_ind = indent_str(cc.indent)
            let body_expr = compile_expr(cc)
            cc = body_expr.c
            cc.output = cc.output + body_ind + body_expr.code + ";\n"
            cc.indent = cc.indent - 1
        }

        -- Skip optional comma
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    if first_arm == false {
        cc.output = cc.output + ind + "}\n"
    }
    return cc
}

fn compile_match_expr(c: Compiler) -> ExprOut {
    let mut cc = c_advance(c)  -- skip 'match'
    let ind = indent_str(cc.indent)
    let subject = compile_expr(cc)
    cc = subject.c
    let subject_code = subject.code
    let subject_type = infer_type_from_code(subject_code, cc)
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)

    -- Create a temp var for the result
    let match_id = cc.match_counter
    cc.match_counter = cc.match_counter + 1
    let temp_var = "_match_" + int_to_string(match_id)

    -- We need to determine the result type from the first arm
    -- For now, emit the match as a statement block and use the temp var
    -- The type will be inferred after we see the first arm's result
    let mut first_arm = true
    let mut result_type = "int64_t"
    let mut match_output = ""

    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_RBRACE() { break }

        let arm = compile_match_pattern(cc, subject_code, subject_type)
        cc = arm.c

        cc = c_skip_nl(cc)
        cc = c_expect(cc, TK_FAT_ARROW())
        cc = c_skip_nl(cc)

        if arm.condition == "" {
            if first_arm {
                match_output = match_output + ind + "{\n"
            } else {
                match_output = match_output + ind + "} else {\n"
            }
        } else {
            if first_arm {
                match_output = match_output + ind + "if " + arm.condition + " {\n"
            } else {
                match_output = match_output + ind + "} else if " + arm.condition + " {\n"
            }
        }

        -- Emit bindings
        match_output = match_output + arm.bindings

        -- Compile arm expression
        let arm_ind = indent_str(cc.indent + 1)
        if c_peek(cc) == TK_LBRACE() {
            -- Block body - the last expression should be the value
            -- For simplicity, compile as a block and expect an assignment
            cc = c_expect(cc, TK_LBRACE())
            let saved_out = cc.output
            cc.output = ""
            cc.indent = cc.indent + 1
            cc = compile_block_body(cc)
            cc = c_expect(cc, TK_RBRACE())
            cc.indent = cc.indent - 1
            match_output = match_output + cc.output
            cc.output = saved_out
        } else {
            let arm_expr = compile_expr(cc)
            cc = arm_expr.c
            if first_arm {
                result_type = infer_type_from_code(arm_expr.code, cc)
            }
            match_output = match_output + arm_ind + temp_var + " = " + arm_expr.code + ";\n"
        }
        first_arm = false

        -- Skip optional comma
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    if first_arm == false {
        match_output = match_output + ind + "}\n"
    }

    -- Emit the temp var declaration and match block into the output
    cc.output = cc.output + ind + result_type + " " + temp_var + ";\n"
    cc.output = cc.output + match_output
    -- Track the temp var type so let inference works
    cc = track_var_type(cc, temp_var, result_type)
    let is_str = result_type == "dm_string"
    cc = track_str_var(cc, temp_var, is_str)
    return ExprOut { c: cc, code: temp_var }
}

-- [module: compile_decl]


-- ============================================================
-- DECLARATION COMPILER
-- ============================================================

fn compile_fn_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'fn'
    let name_tok = c_cur(cc)
    let fn_name = name_tok.value
    cc = c_advance(cc)

    -- Check for generic function: fn name[T](...) — skip it (compiled on demand)
    if c_peek(cc) == TK_LBRACKET() {
        -- Skip entire generic function body
        let mut depth = 0
        let mut found_end = false
        while found_end == false and c_peek(cc) != TK_EOF() {
            if c_peek(cc) == TK_LBRACE() {
                depth = depth + 1
            }
            if c_peek(cc) == TK_RBRACE() {
                depth = depth - 1
                if depth == 0 {
                    cc = c_advance(cc)
                    found_end = true
                }
            }
            if found_end == false {
                cc = c_advance(cc)
            }
        }
        return cc
    }

    -- Parameters
    cc = c_expect(cc, TK_LPAREN())
    let mut params_c = ""
    let mut first = true
    while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
        if first == false {
            cc = c_expect(cc, TK_COMMA())
            params_c = params_c + ", "
        }
        first = false
        let pname_tok = c_cur(cc)
        let pname = pname_tok.value
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let pt = parse_type_for_c(cc)
        cc = pt.c
        params_c = params_c + pt.code + " " + dm_mangle(pname)
        -- Track string parameters
        cc = track_str_var(cc, dm_mangle(pname), pt.code == "dm_string")
        -- Track list element type for list parameters
        cc = track_list_elem_type(cc, pt.code, pname)
        -- Track parameter type for variable inference
        cc = track_var_type(cc, dm_mangle(pname), pt.code)
    }
    cc = c_expect(cc, TK_RPAREN())

    -- Return type
    let mut ret_type = "void"
    if c_peek(cc) == TK_ARROW() {
        cc = c_advance(cc)
        let rt = parse_type_for_c(cc)
        cc = rt.c
        ret_type = rt.code
    }

    -- Generate function signature
    let mut final_sig = ret_type + " " + dm_mangle(fn_name) + "(" + params_c + ")"
    if params_c == "" {
        final_sig = ret_type + " " + dm_mangle(fn_name) + "(void)"
    }
    cc.fn_sigs.push(final_sig + ";")
    cc.fn_names.push(fn_name)
    cc.fn_ret_types.push(ret_type)

    -- Function body
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    let mut body_out = final_sig + " {\n"

    -- Save and reset output for body
    let saved_output = cc.output
    let saved_fn_ret = cc.current_fn_ret_type
    cc.output = ""
    cc.indent = 1
    cc.current_fn_ret_type = ret_type
    cc = compile_block_body(cc)
    cc = c_expect(cc, TK_RBRACE())
    body_out = body_out + cc.output + "}\n\n"
    cc.output = saved_output
    cc.indent = 0
    cc.current_fn_ret_type = saved_fn_ret
    cc.fn_defs.push(body_out)

    return cc
}

fn compile_struct_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'struct'
    let name_tok = c_cur(cc)
    let struct_name = name_tok.value
    cc = c_advance(cc)
    cc.struct_names.push(struct_name)

    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())

    let mut fields = "typedef struct " + dm_mangle(struct_name) + " {\n"
    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        let fname_tok = c_cur(cc)
        cc = c_advance(cc)
        cc = c_expect(cc, TK_COLON())
        let ft = parse_type_for_c(cc)
        cc = ft.c
        fields = fields + "    " + ft.code + " " + fname_tok.value + ";\n"
        -- Track struct field type
        cc.struct_fields = cc.struct_fields + "|" + dm_mangle(struct_name) + "." + fname_tok.value + "=" + ft.code + "|"
        -- Skip optional comma
        cc = c_skip_nl(cc)
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    fields = fields + "} " + dm_mangle(struct_name) + ";\n\n"
    cc.struct_defs.push(fields)

    return cc
}

fn compile_enum_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'enum'
    let name_tok = c_cur(cc)
    let enum_name = name_tok.value
    cc = c_advance(cc)
    cc.enum_names.push(enum_name)

    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())

    -- First pass: collect variant names and their payload types
    let mut variant_names: List[string] = []
    let mut variant_payloads: List[string] = []
    let mut has_any_payload = false

    cc = c_skip_nl(cc)
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        let vname_tok = c_cur(cc)
        let vname = vname_tok.value
        cc = c_advance(cc)
        variant_names.push(vname)

        -- Check for tuple payload: Variant(Type1, Type2, ...)
        if c_peek(cc) == TK_LPAREN() {
            cc = c_advance(cc)  -- skip '('
            let mut payload_types = ""
            let mut first = true
            while c_peek(cc) != TK_RPAREN() and c_peek(cc) != TK_EOF() {
                if first == false {
                    cc = c_expect(cc, TK_COMMA())
                    payload_types = payload_types + ","
                }
                first = false
                let pt = parse_type_for_c(cc)
                cc = pt.c
                payload_types = payload_types + pt.code
            }
            cc = c_expect(cc, TK_RPAREN())
            variant_payloads.push("tuple:" + payload_types)
            has_any_payload = true
            cc.enum_variants = cc.enum_variants + "|" + enum_name + "." + vname + "=tuple:" + payload_types + "|"
        } else {
            variant_payloads.push("unit")
            cc.enum_variants = cc.enum_variants + "|" + enum_name + "." + vname + "=unit|"
        }

        -- Skip optional comma
        if c_peek(cc) == TK_COMMA() {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())

    let mut def = ""
    if has_any_payload == false {
        -- Simple enum: no payloads, generate plain C enum
        def = "typedef enum " + dm_mangle(enum_name) + " {\n"
        let mut i = 0
        while i < variant_names.len() {
            def = def + "    " + dm_mangle(enum_name) + "_" + variant_names[i]
            if i + 1 < variant_names.len() {
                def = def + ","
            }
            def = def + "\n"
            i = i + 1
        }
        def = def + "} " + dm_mangle(enum_name) + ";\n\n"
    } else {
        -- Tagged union enum: generate tag enum + union struct + constructors
        let mangled = dm_mangle(enum_name)
        -- Tag enum
        def = def + "typedef enum " + mangled + "_tag {\n"
        let mut i = 0
        while i < variant_names.len() {
            def = def + "    " + mangled + "_tag_" + variant_names[i]
            if i + 1 < variant_names.len() {
                def = def + ","
            }
            def = def + "\n"
            i = i + 1
        }
        def = def + "} " + mangled + "_tag;\n\n"

        -- Struct with tag + union
        def = def + "typedef struct " + mangled + " {\n"
        def = def + "    " + mangled + "_tag tag;\n"
        def = def + "    union {\n"
        i = 0
        while i < variant_names.len() {
            let payload = "" + variant_payloads[i]
            if starts_with(payload, "tuple:") {
                let types_str = substr(payload, 6, len(payload) - 6)
                def = def + "        struct {"
                -- Parse comma-separated types
                let mut field_idx = 0
                let mut tpos = 0
                let mut tstr = types_str
                let mut parse_done = false
                while parse_done == false {
                    let comma_pos = string_find(tstr, ",")
                    if comma_pos < 0 {
                        def = def + " " + tstr + " _" + int_to_string(field_idx) + ";"
                        parse_done = true
                    } else {
                        let t = substr(tstr, 0, comma_pos)
                        def = def + " " + t + " _" + int_to_string(field_idx) + ";"
                        tstr = substr(tstr, comma_pos + 1, len(tstr) - comma_pos - 1)
                        field_idx = field_idx + 1
                    }
                }
                def = def + " } " + variant_names[i] + ";\n"
            }
            i = i + 1
        }
        def = def + "    } data;\n"
        def = def + "} " + mangled + ";\n\n"

        -- Constructor functions
        i = 0
        while i < variant_names.len() {
            let payload = "" + variant_payloads[i]
            let vname = "" + variant_names[i]
            if payload == "unit" {
                def = def + "static inline " + mangled + " " + mangled + "_" + vname + "(void) {\n"
                def = def + "    " + mangled + " _r; _r.tag = " + mangled + "_tag_" + vname + "; return _r;\n"
                def = def + "}\n\n"
            } else if starts_with(payload, "tuple:") {
                let types_str = substr(payload, 6, len(payload) - 6)
                -- Build parameter list
                let mut params = ""
                let mut body = ""
                let mut field_idx = 0
                let mut tstr = types_str
                let mut parse_done = false
                while parse_done == false {
                    let comma_pos = string_find(tstr, ",")
                    if field_idx > 0 {
                        params = params + ", "
                    }
                    if comma_pos < 0 {
                        params = params + tstr + " _" + int_to_string(field_idx)
                        body = body + "    _r.data." + vname + "._" + int_to_string(field_idx) + " = _" + int_to_string(field_idx) + ";\n"
                        parse_done = true
                    } else {
                        let t = substr(tstr, 0, comma_pos)
                        params = params + t + " _" + int_to_string(field_idx)
                        body = body + "    _r.data." + vname + "._" + int_to_string(field_idx) + " = _" + int_to_string(field_idx) + ";\n"
                        tstr = substr(tstr, comma_pos + 1, len(tstr) - comma_pos - 1)
                        field_idx = field_idx + 1
                    }
                }
                def = def + "static inline " + mangled + " " + mangled + "_" + vname + "(" + params + ") {\n"
                def = def + "    " + mangled + " _r; _r.tag = " + mangled + "_tag_" + vname + ";\n"
                def = def + body
                def = def + "    return _r;\n"
                def = def + "}\n\n"
            }
            i = i + 1
        }
    }
    cc.struct_defs.push(def)

    return cc
}

fn compile_impl_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'impl'
    let type_tok = c_cur(cc)
    cc = c_advance(cc)
    -- Skip generic params if any
    if c_peek(cc) == TK_LBRACKET() {
        let mut depth = 1
        cc = c_advance(cc)
        while depth > 0 and c_peek(cc) != TK_EOF() {
            if c_peek(cc) == TK_LBRACKET() {
                depth = depth + 1
            }
            if c_peek(cc) == TK_RBRACKET() {
                depth = depth - 1
            }
            cc = c_advance(cc)
        }
    }
    cc = c_skip_nl(cc)
    cc = c_expect(cc, TK_LBRACE())
    cc = c_skip_nl(cc)
    -- Parse methods inside impl block
    while c_peek(cc) != TK_RBRACE() and c_peek(cc) != TK_EOF() {
        if c_peek(cc) == TK_FN() {
            cc = compile_fn_decl(cc)
        } else {
            cc = c_advance(cc)
        }
        cc = c_skip_nl(cc)
    }
    cc = c_expect(cc, TK_RBRACE())
    return cc
}

fn compile_import_decl(c: Compiler) -> Compiler {
    let mut cc = c_advance(c)  -- skip 'import'
    -- Skip the rest of the import line
    while c_peek(cc) != TK_NEWLINE() and c_peek(cc) != TK_EOF() {
        cc = c_advance(cc)
    }
    return cc
}

-- ============================================================
-- TOP-LEVEL COMPILER
-- ============================================================

fn prescan_declarations(c: Compiler) -> Compiler {
    let mut cc = c
    let mut i = 0
    while i < cc.tokens.len() {
        let tok = cc.tokens[i]
        if tok.kind == TK_FN() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                let fn_name = name_tok.value

                -- Check for generic function: fn name[T](...)
                let mut is_generic = false
                let mut type_params = ""
                if i + 2 < cc.tokens.len() {
                    let maybe_bracket = cc.tokens[i + 2]
                    if maybe_bracket.kind == TK_LBRACKET() {
                        is_generic = true
                        -- Collect type parameter names
                        let mut gj = i + 3
                        while gj < cc.tokens.len() and cc.tokens[gj].kind != TK_RBRACKET() {
                            if cc.tokens[gj].kind == TK_IDENT() {
                                if type_params != "" {
                                    type_params = type_params + ","
                                }
                                type_params = type_params + cc.tokens[gj].value
                            }
                            gj = gj + 1
                        }
                        -- Find end of function body (matching closing brace)
                        let mut body_j = gj
                        let mut brace_depth = 0
                        let mut found_body_end = false
                        while body_j < cc.tokens.len() and found_body_end == false {
                            if cc.tokens[body_j].kind == TK_LBRACE() {
                                brace_depth = brace_depth + 1
                            }
                            if cc.tokens[body_j].kind == TK_RBRACE() {
                                brace_depth = brace_depth - 1
                                if brace_depth == 0 {
                                    found_body_end = true
                                }
                            }
                            body_j = body_j + 1
                        }
                        -- Store: "|fn_name=type_params:start:end|"
                        cc.generic_fn_tokens = cc.generic_fn_tokens + "|" + fn_name + "=" + type_params + ":" + int_to_string(i) + ":" + int_to_string(body_j) + "|"
                    }
                }

                if is_generic {
                    -- Don't add generic fns to fn_names/fn_ret_types (they're templates)
                    i = i + 1
                    continue
                }

                -- Find return type by scanning for -> after )
                let mut j = i + 2
                let mut depth = 0
                -- Skip past parameter list
                while j < cc.tokens.len() {
                    let t = cc.tokens[j]
                    if t.kind == TK_LPAREN() { depth = depth + 1 }
                    if t.kind == TK_RPAREN() {
                        depth = depth - 1
                        if depth == 0 {
                            j = j + 1
                            break
                        }
                    }
                    j = j + 1
                }
                -- Check for -> RetType
                let mut ret_type = "void"
                if j < cc.tokens.len() {
                    let arrow_tok = cc.tokens[j]
                    if arrow_tok.kind == TK_ARROW() {
                        j = j + 1
                        if j < cc.tokens.len() {
                            let ret_tok = cc.tokens[j]
                            if ret_tok.kind == TK_IDENT() {
                                let ret_name = ret_tok.value
                                -- Check for generic: List[T], Box[T], Option[T], Result[T, E]
                                if j + 2 < cc.tokens.len() {
                                    let maybe_bracket = cc.tokens[j + 1]
                                    if maybe_bracket.kind == TK_LBRACKET() {
                                        let inner_tok = cc.tokens[j + 2]
                                        if inner_tok.kind == TK_IDENT() {
                                            let inner_c = map_dm_type(inner_tok.value)
                                            if ret_name == "List" {
                                                ret_type = "dm_list_" + inner_c
                                            } else if ret_name == "Box" {
                                                ret_type = inner_c + "*"
                                            } else if ret_name == "Option" {
                                                ret_type = "dm_option_" + inner_c
                                            } else if ret_name == "Result" {
                                                -- Result[T, E] - look for comma and second type
                                                let mut second_c = "dm_string"
                                                if j + 4 < cc.tokens.len() {
                                                    let maybe_comma = cc.tokens[j + 3]
                                                    if maybe_comma.kind == TK_COMMA() {
                                                        let second_tok = cc.tokens[j + 4]
                                                        if second_tok.kind == TK_IDENT() {
                                                            second_c = map_dm_type(second_tok.value)
                                                        }
                                                    }
                                                }
                                                ret_type = "dm_result_" + inner_c + "_" + second_c
                                            } else {
                                                ret_type = "dm_" + ret_name + "_" + inner_c
                                            }
                                        } else {
                                            ret_type = map_dm_type(ret_name)
                                        }
                                    } else {
                                        ret_type = map_dm_type(ret_name)
                                    }
                                } else {
                                    ret_type = map_dm_type(ret_name)
                                }
                            }
                        }
                    }
                }
                cc.fn_names.push(fn_name)
                cc.fn_ret_types.push(ret_type)
            }
        }
        if tok.kind == TK_STRUCT() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                cc.struct_names.push(name_tok.value)
            }
        }
        if tok.kind == TK_ENUM() and i + 1 < cc.tokens.len() {
            let name_tok = cc.tokens[i + 1]
            if name_tok.kind == TK_IDENT() {
                cc.enum_names.push(name_tok.value)
            }
        }
        i = i + 1
    }
    return cc
}

fn compile_source(c: Compiler) -> Compiler {
    let mut cc = c
    -- Pre-scan all declarations for forward references
    cc = prescan_declarations(cc)
    cc = c_skip_nl(cc)

    -- Parse module declaration
    let mut module_name = "unknown"
    if c_peek(cc) == TK_MODULE() {
        cc = c_advance(cc)
        let mod_tok = c_cur(cc)
        module_name = mod_tok.value
        cc = c_advance(cc)
        -- Skip dotted module path
        while c_peek(cc) == TK_DOT() {
            cc = c_advance(cc)
            cc = c_advance(cc)
        }
    }

    cc = c_skip_nl(cc)

    -- Parse declarations
    while c_peek(cc) != TK_EOF() {
        let k = c_peek(cc)
        if k == TK_FN() {
            cc = compile_fn_decl(cc)
        } else if k == TK_STRUCT() {
            cc = compile_struct_decl(cc)
        } else if k == TK_ENUM() {
            cc = compile_enum_decl(cc)
        } else if k == TK_IMPL() {
            cc = compile_impl_decl(cc)
        } else if k == TK_IMPORT() {
            cc = compile_import_decl(cc)
        } else if k == TK_NEWLINE() {
            cc = c_advance(cc)
        } else {
            cc = c_error(cc, "unexpected token at top level: " + token_kind_name(k))
            cc = c_advance(cc)
        }
    }

    return cc
}

-- [module: runtime]


fn emit_runtime() -> string {
    let mut r = "// dAImond Generated C Code\n"
    r = r + "// Generated by dAImond Stage 1 Compiler\n\n"
    r = r + "#include <stdint.h>\n"
    r = r + "#include <stdbool.h>\n"
    r = r + "#include <stddef.h>\n"
    r = r + "#include <stdlib.h>\n"
    r = r + "#include <string.h>\n"
    r = r + "#include <stdio.h>\n\n"
    r = r + "// Runtime Types\n\n"
    r = r + "typedef struct dm_string {\n"
    r = r + "    const char* data;\n"
    r = r + "    size_t len;\n"
    r = r + "    size_t capacity;\n"
    r = r + "} dm_string;\n\n"
    r = r + "// Runtime Functions\n\n"
    r = r + "static inline dm_string dm_string_from_cstr(const char* s) {\n"
    r = r + "    size_t len = strlen(s);\n"
    r = r + "    return (dm_string){ .data = s, .len = len, .capacity = len };\n"
    r = r + "}\n\n"
    r = r + "static inline bool dm_string_eq(dm_string a, dm_string b) {\n"
    r = r + "    if (a.len != b.len) return false;\n"
    r = r + "    return memcmp(a.data, b.data, a.len) == 0;\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_string_concat(dm_string a, dm_string b) {\n"
    r = r + "    size_t new_len = a.len + b.len;\n"
    r = r + "    char* buf = (char*)malloc(new_len + 1);\n"
    r = r + "    memcpy(buf, a.data, a.len);\n"
    r = r + "    memcpy(buf + a.len, b.data, b.len);\n"
    r = r + "    buf[new_len] = '\\0';\n"
    r = r + "    return (dm_string){ .data = buf, .len = new_len, .capacity = new_len };\n"
    r = r + "}\n\n"
    r = r + "static inline void dm_print_str(dm_string s) { fwrite(s.data, 1, s.len, stdout); }\n"
    r = r + "static inline void dm_println_str(dm_string s) { fwrite(s.data, 1, s.len, stdout); putchar('\\n'); }\n"
    r = r + "static inline void dm_eprintln_str(dm_string s) { fwrite(s.data, 1, s.len, stderr); fputc('\\n', stderr); }\n\n"
    r = r + "static inline dm_string dm_int_to_string(int64_t n) {\n"
    r = r + "    char buf[32];\n"
    r = r + "    int len = snprintf(buf, sizeof(buf), \"%lld\", (long long)n);\n"
    r = r + "    char* result = (char*)malloc(len + 1);\n"
    r = r + "    memcpy(result, buf, len + 1);\n"
    r = r + "    return (dm_string){ .data = result, .len = (size_t)len, .capacity = (size_t)len };\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_bool_to_string(bool b) {\n"
    r = r + "    return b ? dm_string_from_cstr(\"true\") : dm_string_from_cstr(\"false\");\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_float_to_string(double f) {\n"
    r = r + "    char buf[64];\n"
    r = r + "    int len = snprintf(buf, sizeof(buf), \"%g\", f);\n"
    r = r + "    char* result = (char*)malloc(len + 1);\n"
    r = r + "    memcpy(result, buf, len + 1);\n"
    r = r + "    return (dm_string){ .data = result, .len = (size_t)len, .capacity = (size_t)len };\n"
    r = r + "}\n\n"
    r = r + "static inline void dm_panic(const char* msg) { fprintf(stderr, \"PANIC: %s\\n\", msg); exit(1); }\n"
    r = r + "static inline void dm_exit(int64_t code) { exit((int)code); }\n\n"
    -- Generic list macros (type-agnostic, works with any list struct)
    r = r + "#define DM_LIST_PUSH(list, val) do { \\\n"
    r = r + "    if ((list).len >= (list).capacity) { \\\n"
    r = r + "        size_t _nc = (list).capacity == 0 ? 8 : (list).capacity * 2; \\\n"
    r = r + "        void* _nd = realloc((list).data, _nc * sizeof(*(list).data)); \\\n"
    r = r + "        if (!_nd) dm_panic(\"list push: out of memory\"); \\\n"
    r = r + "        (list).data = _nd; \\\n"
    r = r + "        (list).capacity = _nc; \\\n"
    r = r + "    } \\\n"
    r = r + "    (list).data[(list).len] = (val); \\\n"
    r = r + "    (list).len++; \\\n"
    r = r + "} while(0)\n\n"
    r = r + "#define DM_LIST_LEN(list) ((int64_t)(list).len)\n"
    r = r + "#define DM_LIST_GET(list, idx) ((list).data[idx])\n"
    r = r + "#define DM_LIST_POP(list) ((list).data[--(list).len])\n"
    r = r + "static inline bool dm_list_generic_contains(void* list_ptr, void* val_ptr, size_t elem_size) {\n"
    r = r + "    size_t len = *(size_t*)((char*)list_ptr + sizeof(void*));\n"
    r = r + "    char* data = *(char**)list_ptr;\n"
    r = r + "    for (size_t i = 0; i < len; i++) {\n"
    r = r + "        if (memcmp(data + i * elem_size, val_ptr, elem_size) == 0) return true;\n"
    r = r + "    }\n"
    r = r + "    return false;\n"
    r = r + "}\n"
    r = r + "#define DM_LIST_CONTAINS(list, val) dm_list_generic_contains(&(list), &(val), sizeof(val))\n\n"
    r = r + "static int dm_argc = 0;\n"
    r = r + "static char** dm_argv = NULL;\n"
    r = r + "static inline dm_string dm_args_get(int64_t i) {\n"
    r = r + "    if (i < 0 || i >= (int64_t)dm_argc) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };\n"
    r = r + "    return dm_string_from_cstr(dm_argv[i]);\n"
    r = r + "}\n"
    r = r + "static inline int64_t dm_args_len(void) { return (int64_t)dm_argc; }\n\n"
    r = r + "static inline dm_string dm_file_read(dm_string path) {\n"
    r = r + "    char* cpath = (char*)malloc(path.len + 1);\n"
    r = r + "    memcpy(cpath, path.data, path.len);\n"
    r = r + "    cpath[path.len] = '\\0';\n"
    r = r + "    FILE* f = fopen(cpath, \"rb\");\n"
    r = r + "    free(cpath);\n"
    r = r + "    if (!f) dm_panic(\"file_read: cannot open file\");\n"
    r = r + "    fseek(f, 0, SEEK_END);\n"
    r = r + "    long sz = ftell(f);\n"
    r = r + "    fseek(f, 0, SEEK_SET);\n"
    r = r + "    char* buf = (char*)malloc((size_t)sz + 1);\n"
    r = r + "    size_t rd = fread(buf, 1, (size_t)sz, f);\n"
    r = r + "    fclose(f);\n"
    r = r + "    buf[rd] = '\\0';\n"
    r = r + "    return (dm_string){ .data = buf, .len = rd, .capacity = (size_t)sz };\n"
    r = r + "}\n\n"
    r = r + "static inline void dm_file_write(dm_string path, dm_string content) {\n"
    r = r + "    char* cpath = (char*)malloc(path.len + 1);\n"
    r = r + "    memcpy(cpath, path.data, path.len);\n"
    r = r + "    cpath[path.len] = '\\0';\n"
    r = r + "    FILE* f = fopen(cpath, \"wb\");\n"
    r = r + "    free(cpath);\n"
    r = r + "    if (!f) dm_panic(\"file_write: cannot open file\");\n"
    r = r + "    fwrite(content.data, 1, content.len, f);\n"
    r = r + "    fclose(f);\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_string_substr(dm_string s, int64_t start, int64_t length) {\n"
    r = r + "    if (start < 0 || (size_t)start >= s.len) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };\n"
    r = r + "    size_t actual_len = (size_t)length;\n"
    r = r + "    if ((size_t)start + actual_len > s.len) actual_len = s.len - (size_t)start;\n"
    r = r + "    char* buf = (char*)malloc(actual_len + 1);\n"
    r = r + "    memcpy(buf, s.data + start, actual_len);\n"
    r = r + "    buf[actual_len] = '\\0';\n"
    r = r + "    return (dm_string){ .data = buf, .len = actual_len, .capacity = actual_len };\n"
    r = r + "}\n\n"
    r = r + "static inline int64_t dm_parse_int(dm_string s) {\n"
    r = r + "    char buf[32];\n"
    r = r + "    size_t cl = s.len < 31 ? s.len : 31;\n"
    r = r + "    memcpy(buf, s.data, cl);\n"
    r = r + "    buf[cl] = '\\0';\n"
    r = r + "    return (int64_t)atoll(buf);\n"
    r = r + "}\n\n"
    r = r + "static inline bool dm_string_contains(dm_string h, dm_string n) {\n"
    r = r + "    if (n.len == 0) return true;\n"
    r = r + "    if (n.len > h.len) return false;\n"
    r = r + "    for (size_t i = 0; i <= h.len - n.len; i++) {\n"
    r = r + "        if (memcmp(h.data + i, n.data, n.len) == 0) return true;\n"
    r = r + "    }\n"
    r = r + "    return false;\n"
    r = r + "}\n\n"
    r = r + "static inline int64_t dm_string_find(dm_string h, dm_string n) {\n"
    r = r + "    if (n.len == 0) return 0;\n"
    r = r + "    if (n.len > h.len) return -1;\n"
    r = r + "    for (size_t i = 0; i <= h.len - n.len; i++) {\n"
    r = r + "        if (memcmp(h.data + i, n.data, n.len) == 0) return (int64_t)i;\n"
    r = r + "    }\n"
    r = r + "    return -1;\n"
    r = r + "}\n\n"
    r = r + "static inline bool dm_string_starts_with(dm_string s, dm_string p) {\n"
    r = r + "    if (p.len > s.len) return false;\n"
    r = r + "    return memcmp(s.data, p.data, p.len) == 0;\n"
    r = r + "}\n\n"
    r = r + "static inline bool dm_string_ends_with(dm_string s, dm_string x) {\n"
    r = r + "    if (x.len > s.len) return false;\n"
    r = r + "    return memcmp(s.data + s.len - x.len, x.data, x.len) == 0;\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_string_replace(dm_string s, dm_string old_s, dm_string new_s) {\n"
    r = r + "    if (old_s.len == 0) return s;\n"
    r = r + "    size_t count = 0;\n"
    r = r + "    for (size_t i = 0; i + old_s.len <= s.len; i++) {\n"
    r = r + "        if (memcmp(s.data + i, old_s.data, old_s.len) == 0) { count++; i += old_s.len - 1; }\n"
    r = r + "    }\n"
    r = r + "    if (count == 0) { char* b = (char*)malloc(s.len+1); memcpy(b,s.data,s.len); b[s.len]='\\0'; return (dm_string){.data=b,.len=s.len,.capacity=s.len}; }\n"
    r = r + "    size_t nl = s.len + count * (new_s.len - old_s.len);\n"
    r = r + "    char* buf = (char*)malloc(nl + 1);\n"
    r = r + "    size_t pos = 0;\n"
    r = r + "    for (size_t i = 0; i < s.len; ) {\n"
    r = r + "        if (i + old_s.len <= s.len && memcmp(s.data+i, old_s.data, old_s.len)==0) {\n"
    r = r + "            memcpy(buf+pos, new_s.data, new_s.len); pos += new_s.len; i += old_s.len;\n"
    r = r + "        } else { buf[pos++] = s.data[i++]; }\n"
    r = r + "    }\n"
    r = r + "    buf[nl] = '\\0';\n"
    r = r + "    return (dm_string){ .data = buf, .len = nl, .capacity = nl };\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_string_trim(dm_string s) {\n"
    r = r + "    size_t start = 0;\n"
    r = r + "    while (start < s.len && (s.data[start]==' '||s.data[start]=='\\t'||s.data[start]=='\\n'||s.data[start]=='\\r')) start++;\n"
    r = r + "    size_t end = s.len;\n"
    r = r + "    while (end > start && (s.data[end-1]==' '||s.data[end-1]=='\\t'||s.data[end-1]=='\\n'||s.data[end-1]=='\\r')) end--;\n"
    r = r + "    size_t nl = end - start;\n"
    r = r + "    if (nl == 0) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };\n"
    r = r + "    char* buf = (char*)malloc(nl + 1); memcpy(buf, s.data + start, nl); buf[nl] = '\\0';\n"
    r = r + "    return (dm_string){ .data = buf, .len = nl, .capacity = nl };\n"
    r = r + "}\n\n"
    r = r + "static inline int64_t dm_system(dm_string cmd) {\n"
    r = r + "    char* c = (char*)malloc(cmd.len + 1); memcpy(c, cmd.data, cmd.len); c[cmd.len] = '\\0';\n"
    r = r + "    int r = system(c); free(c); return (int64_t)r;\n"
    r = r + "}\n\n"
    r = r + "static inline dm_string dm_char_at(dm_string s, int64_t i) {\n"
    r = r + "    if (i < 0 || (size_t)i >= s.len) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };\n"
    r = r + "    char* buf = (char*)malloc(2); buf[0] = s.data[i]; buf[1] = '\\0';\n"
    r = r + "    return (dm_string){ .data = buf, .len = 1, .capacity = 1 };\n"
    r = r + "}\n\n"
    r = r + "static inline int64_t dm_len(dm_string s) { return (int64_t)s.len; }\n\n"
    r = r + "static inline int dm_string_cmp(dm_string a, dm_string b) {\n"
    r = r + "    size_t min_len = a.len < b.len ? a.len : b.len;\n"
    r = r + "    int c = memcmp(a.data, b.data, min_len);\n"
    r = r + "    if (c != 0) return c;\n"
    r = r + "    if (a.len < b.len) return -1;\n"
    r = r + "    if (a.len > b.len) return 1;\n"
    r = r + "    return 0;\n"
    r = r + "}\n"
    r = r + "static inline bool dm_string_lt(dm_string a, dm_string b) { return dm_string_cmp(a, b) < 0; }\n"
    r = r + "static inline bool dm_string_gt(dm_string a, dm_string b) { return dm_string_cmp(a, b) > 0; }\n"
    r = r + "static inline bool dm_string_lteq(dm_string a, dm_string b) { return dm_string_cmp(a, b) <= 0; }\n"
    r = r + "static inline bool dm_string_gteq(dm_string a, dm_string b) { return dm_string_cmp(a, b) >= 0; }\n\n"
    r = r + "// End of Runtime\n\n"
    return r
}

fn emit_list_type_def(list_type: string, elem_type: string) -> string {
    let mut d = "typedef struct " + list_type + " {\n"
    d = d + "    " + elem_type + "* data;\n"
    d = d + "    size_t len;\n"
    d = d + "    size_t capacity;\n"
    d = d + "} " + list_type + ";\n\n"
    d = d + "static inline " + list_type + " " + list_type + "_new(void) {\n"
    d = d + "    return (" + list_type + "){ .data = NULL, .len = 0, .capacity = 0 };\n"
    d = d + "}\n\n"
    return d
}

fn assemble_output(c: Compiler) -> string {
    let mut out = emit_runtime()

    -- Forward declare all struct types (so list types can use pointers)
    let mut si = 0
    while si < c.struct_names.len() {
        let sname = "dm_" + c.struct_names[si]
        let fwd = "typedef struct " + sname + " " + sname + ";\n"
        out = out + fwd
        si = si + 1
    }
    out = out + "\n"

    -- List type definitions (after forward declarations, before full struct defs)
    out = out + c.list_type_defs

    -- Option/Result type definitions
    out = out + c.option_type_defs

    -- Struct/enum definitions
    let mut i = 0
    while i < c.struct_defs.len() {
        out = out + c.struct_defs[i]
        i = i + 1
    }

    -- Forward declarations
    let mut j = 0
    while j < c.fn_sigs.len() {
        out = out + c.fn_sigs[j] + "\n"
        j = j + 1
    }
    out = out + "\n"

    -- Lambda definitions (lifted anonymous functions)
    out = out + c.lambda_defs

    -- Function definitions
    let mut k = 0
    while k < c.fn_defs.len() {
        out = out + c.fn_defs[k]
        k = k + 1
    }

    -- main() wrapper
    out = out + "int main(int argc, char** argv) {\n"
    out = out + "    dm_argc = argc;\n"
    out = out + "    dm_argv = argv;\n"
    out = out + "    dm_main();\n"
    out = out + "    return 0;\n"
    out = out + "}\n"

    return out
}

-- [module: imports]

-- ============================================================
-- MULTI-FILE IMPORT RESOLVER
-- ============================================================

-- Extract directory from a file path (everything up to and including last '/')
fn dir_of(path: string) -> string {
    let mut last_slash = 0 - 1
    let mut i = 0
    let path_len = len(path)
    while i < path_len {
        let ch = char_at(path, i)
        if ch == "/" {
            last_slash = i
        }
        i = i + 1
    }
    if last_slash < 0 {
        return ""
    }
    return substr(path, 0, last_slash + 1)
}

-- Scan source text for import lines, return list of module names
fn find_imports(source: string) -> List[string] {
    let mut imports: List[string] = []
    let src_len = len(source)
    let mut pos = 0

    while pos < src_len {
        -- Skip to start of line
        -- Check if line starts with "import " (possibly after whitespace)
        let mut lp = pos
        while lp < src_len and (char_at(source, lp) == " " or char_at(source, lp) == "\t") {
            lp = lp + 1
        }

        -- Check for "import " at current position
        let mut is_import = false
        if lp + 7 <= src_len {
            let word = substr(source, lp, 7)
            if word == "import " {
                is_import = true
            }
        }

        if is_import {
            -- Extract module name: everything after "import " until newline
            let mut name_start = lp + 7
            -- Skip whitespace after "import "
            while name_start < src_len and char_at(source, name_start) == " " {
                name_start = name_start + 1
            }
            let mut name_end = name_start
            while name_end < src_len and char_at(source, name_end) != "\n" and char_at(source, name_end) != "\r" {
                name_end = name_end + 1
            }
            -- Trim trailing whitespace from name
            let mut trim_end = name_end
            while trim_end > name_start and (char_at(source, trim_end - 1) == " " or char_at(source, trim_end - 1) == "\t") {
                trim_end = trim_end - 1
            }
            if trim_end > name_start {
                let module_name = substr(source, name_start, trim_end - name_start)
                imports.push(module_name)
            }
        }

        -- Advance to next line
        while pos < src_len and char_at(source, pos) != "\n" {
            pos = pos + 1
        }
        if pos < src_len {
            pos = pos + 1
        }
    }

    return imports
}

-- Remove "module X" line from source (first occurrence only)
fn strip_module_decl(source: string) -> string {
    let src_len = len(source)
    let mut pos = 0

    while pos < src_len {
        -- Skip whitespace at start of line
        let mut lp = pos
        while lp < src_len and (char_at(source, lp) == " " or char_at(source, lp) == "\t") {
            lp = lp + 1
        }

        -- Check for "module " at current position
        let mut is_module = false
        if lp + 7 <= src_len {
            let word = substr(source, lp, 7)
            if word == "module " {
                is_module = true
            }
        }

        if is_module {
            -- Find end of line
            let mut line_end = lp
            while line_end < src_len and char_at(source, line_end) != "\n" {
                line_end = line_end + 1
            }
            if line_end < src_len {
                line_end = line_end + 1
            }
            -- Remove this line: before + after
            let before = substr(source, 0, pos)
            let after = substr(source, line_end, src_len - line_end)
            return before + after
        }

        -- Advance to next line
        while pos < src_len and char_at(source, pos) != "\n" {
            pos = pos + 1
        }
        if pos < src_len {
            pos = pos + 1
        }
    }

    return source
}

-- Remove "import X" lines from source
fn strip_import_decls(source: string) -> string {
    let src_len = len(source)
    let mut result = ""
    let mut pos = 0

    while pos < src_len {
        let mut line_start = pos
        -- Find end of line
        let mut line_end = pos
        while line_end < src_len and char_at(source, line_end) != "\n" {
            line_end = line_end + 1
        }
        if line_end < src_len {
            line_end = line_end + 1
        }

        -- Check if this line is an import
        let mut lp = pos
        while lp < src_len and (char_at(source, lp) == " " or char_at(source, lp) == "\t") {
            lp = lp + 1
        }
        let mut is_import = false
        if lp + 7 <= src_len {
            let word = substr(source, lp, 7)
            if word == "import " {
                is_import = true
            }
        }

        if is_import == false {
            let line = substr(source, line_start, line_end - line_start)
            result = result + line
        }

        pos = line_end
    }

    return result
}

-- Resolve module name to file path using dots as directory separators
fn resolve_module_path(base_dir: string, module_name: string) -> string {
    -- Replace dots with '/' for nested modules: "std.io" -> "std/io"
    let path_part = string_replace(module_name, ".", "/")
    return base_dir + path_part + ".dm"
}

-- Recursively resolve imports and concatenate sources
-- imported_modules is a pipe-delimited string tracking already-imported modules
struct ResolveResult {
    source: string,
    imported: string
}

fn collect_imports_recursive(base_dir: string, source: string, imported: string) -> ResolveResult {
    let imports = find_imports(source)
    let mut combined_imports = ""
    let mut current_imported = imported

    let mut i = 0
    while i < imports.len() {
        let module_name = "" + imports[i]
        let marker = "|" + module_name + "|"

        -- Skip if already imported
        if string_contains(current_imported, marker) == false {
            current_imported = current_imported + marker

            let file_path = resolve_module_path(base_dir, module_name)
            let mod_source = file_read(file_path)

            -- Determine the base directory for the imported file
            let mod_dir = dir_of(file_path)

            -- Recursively collect imports from this module first
            let inner = collect_imports_recursive(mod_dir, mod_source, current_imported)
            current_imported = inner.imported
            combined_imports = combined_imports + inner.source

            -- Strip module declaration and import lines from imported source
            let cleaned = strip_import_decls(strip_module_decl(mod_source))

            combined_imports = combined_imports + "\n-- [imported: " + module_name + "]\n" + cleaned + "\n"
        }
        i = i + 1
    }

    return ResolveResult { source: combined_imports, imported: current_imported }
}

fn resolve_imports(source: string, filename: string) -> string {
    let imports = find_imports(source)
    -- If no imports, return source unchanged
    if imports.len() == 0 {
        return source
    }

    let base_dir = dir_of(filename)

    -- Collect all imported code
    let result = collect_imports_recursive(base_dir, source, "")

    -- Build final source: module decl from main, then imports, then rest of main
    -- The main source keeps its module declaration, but imports are inserted after it
    let main_no_imports = strip_import_decls(source)

    -- Find the end of the module declaration line to insert imports after it
    let src_len = len(main_no_imports)
    let mut mod_end = 0
    let mut pos = 0
    let mut found_module = false

    while pos < src_len and found_module == false {
        let mut lp = pos
        while lp < src_len and (char_at(main_no_imports, lp) == " " or char_at(main_no_imports, lp) == "\t") {
            lp = lp + 1
        }
        let mut is_module = false
        if lp + 7 <= src_len {
            if substr(main_no_imports, lp, 7) == "module " {
                is_module = true
            }
        }
        -- Find end of this line
        let mut line_end = pos
        while line_end < src_len and char_at(main_no_imports, line_end) != "\n" {
            line_end = line_end + 1
        }
        if line_end < src_len {
            line_end = line_end + 1
        }
        if is_module {
            mod_end = line_end
            found_module = true
        }
        pos = line_end
    }

    if found_module {
        let before = substr(main_no_imports, 0, mod_end)
        let after = substr(main_no_imports, mod_end, src_len - mod_end)
        return before + "\n" + result.source + "\n" + after
    }
    -- No module decl found: just prepend imports
    return result.source + "\n" + main_no_imports
}

-- [module: main]


-- ============================================================
-- MAIN ENTRY POINT
-- ============================================================

fn main() {
    let argc = args_len()
    if argc < 2 {
        println("dAImond Stage 1 Compiler")
        println("Usage: daimond1 <file.dm>")
        println("       daimond1 run <file.dm>")
        exit(1)
    }

    let mut filename = args_get(1)
    let mut do_run = false
    if filename == "run" and argc > 2 {
        do_run = true
        filename = args_get(2)
    }

    -- Read source
    let raw_source = file_read(filename)

    -- Resolve imports (concatenate imported files before tokenizing)
    let source = resolve_imports(raw_source, filename)

    -- Tokenize
    let tokens = tokenize(source)

    -- Compile
    let mut comp = compiler_new(tokens)
    comp = compile_source(comp)

    -- Check for errors
    if comp.errors.len() > 0 {
        eprintln("Compilation errors:")
        let mut i = 0
        while i < comp.errors.len() {
            eprintln(comp.errors[i])
            i = i + 1
        }
        exit(1)
    }

    -- Assemble C output
    let c_code = assemble_output(comp)

    -- Write C file
    let c_file = string_replace(filename, ".dm", ".c")
    file_write(c_file, c_code)
    println("Generated: " + c_file)

    -- Compile C to binary
    let bin_file = string_replace(filename, ".dm", "")
    let compile_cmd = "cc -o " + bin_file + " " + c_file + " -lm"
    let exit_code = system(compile_cmd)
    if exit_code != 0 {
        eprintln("C compilation failed")
        exit(1)
    }
    println("Compiled: " + bin_file)

    -- Optionally run
    if do_run {
        println("Running:")
        println("============================================================")
        let run_code = system("./" + bin_file)
        println("============================================================")
    }
}
