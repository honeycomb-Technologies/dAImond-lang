module parser

import token
import lexer
import ast

-- ============================================================
-- Parser: Recursive descent + Pratt parser
-- Builds AST nodes from token stream
-- ============================================================

struct Parser {
    tokens: List[Token],
    pos: int,
    errors: List[string],
    -- How deep the parser is in expressions, in statements and in
    -- types; and how many `else if`s came before the one being
    -- parsed (see NESTING)
    depth: int,
    block_depth: int,
    type_depth: int,
    else_ifs: int
}

fn parser_new(tokens: List[Token]) -> Parser {
    let errs: List[string] = []
    return Parser { tokens: tokens, pos: 0, errors: errs, depth: 0, block_depth: 0, type_depth: 0, else_ifs: 0 }
}

-- ============================================================
-- NESTING
-- The parser, the checker and the code generators call themselves
-- once for each level an expression, a statement or a type is
-- nested, and once for each link of a chain: a.b().c(),
-- a + b + c, `else if` after `else if`. Each call takes stack: at
-- most a third of a megabyte, of 64 (measured for each kind of
-- nesting on its own). What is nested or chained further than the
-- limits here is an error that says where. They are well under what
-- one kind of nesting takes on its own, and do not rule out every
-- program that is too deep: chains inside nesting inside chains add
-- up. For those, the compiler says that its stack is full (see
-- main), with no line.
-- ============================================================

-- The deepest nesting of expressions in expressions, of statements in
-- statements and of types in types
fn MAX_NESTING() -> int { return 100 }

-- The most calls, fields and indexes that follow one another in one
-- expression
fn MAX_CHAIN() -> int { return 100 }

-- The most operators that follow one another in one expression
fn MAX_OPERATORS() -> int { return 2000 }

-- The most `else if`s that follow one `if`
fn MAX_ELSE_IFS() -> int { return 200 }

-- Report what is past a limit, and move to the end of the file: the
-- rest would be parsed out of step
fn p_past_limit(p: Parser, what: string) -> Parser {
    let mut pp = p_error(p, what)
    pp.pos = pp.tokens.len()
    return pp
}

-- What is said of an expression, a statement or a type that is
-- nested too deeply
fn nesting_error(what: string) -> string {
    return what + " nested more than " + int_to_string(MAX_NESTING()) + " levels deep"
}

-- ============================================================
-- PARSER HELPERS
-- ============================================================

-- The value of an integer token; `negative` when a minus stands
-- before it. The lexer lets through one integer that is no int, the
-- digits of the lowest int, which are an int only after the minus:
-- without it the value is 0, and p_check_int reports it.
fn int_token_value(tok: Token, negative: bool) -> int {
    if tok.value == LOWEST_INT_DIGITS() {
        if negative { return 0 - 9223372036854775807 - 1 }
        return 0
    }
    if negative { return 0 - parse_int(tok.value) }
    return parse_int(tok.value)
}

-- Report the integer at the parser's place when it is the one that
-- is no int without a minus before it
fn p_check_int(p: Parser) -> Parser {
    if p_cur(p).kind == TK_INTEGER() and p_cur(p).value == LOWEST_INT_DIGITS() {
        return p_error(p, "the integer " + LOWEST_INT_DIGITS() + " does not fit in an int: the highest is 9223372036854775807")
    }
    return p
}

fn p_cur(p: Parser) -> Token {
    if p.pos < p.tokens.len() {
        return p.tokens[p.pos]
    }
    return token_new(TK_EOF(), "", 0, 0)
}

-- For a loop that parses one item a turn until a closing token: when
-- a turn consumed nothing, the loop would never end. Report the token
-- it is stuck at and skip it.
fn p_skip_stuck(p: Parser) -> Parser {
    let reported = p_error(p, "unexpected " + token_kind_name(p_peek(p)))
    return p_advance(reported)
}

-- After an item of a list that ends at the token `closer`: take the
-- comma after it. Anything but a comma or the closer there is an error.
fn p_comma_or(p: Parser, closer: int) -> Parser {
    let pp = p_skip_newlines(p)
    if p_peek(pp) == TK_COMMA() {
        return p_advance(pp)
    }
    if p_peek(pp) == closer or p_peek(pp) == TK_EOF() {
        return p
    }
    return p_error(pp, "expected ',' or " + token_kind_name(closer) + " but got " + token_kind_name(p_peek(pp)))
}

-- Report an error unless the current token can be a name. `what` says
-- what is named, such as "a function name".
fn p_need_name(p: Parser, what: string) -> Parser {
    let tok = p_cur(p)
    if tok.kind == TK_IDENT() or tok.kind == TK_SELF() {
        return p
    }
    return p_error(p, "expected " + what + " but got " + token_kind_name(tok.kind))
}

fn p_peek(p: Parser) -> int {
    return p_cur(p).kind
}

-- The parser at the next token. It stays at the last token, the end of
-- the file, so that an error there has the file's last line.
fn p_advance(p: Parser) -> Parser {
    let mut pp = p
    if pp.pos + 1 < pp.tokens.len() {
        pp.pos = pp.pos + 1
    }
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
    let msg = "Error at " + int_to_string(line_in_file(tok.line)) + ":" + int_to_string(tok.col) + ": expected " + token_kind_name(kind) + " but got " + token_kind_name(tok.kind)
    pp.errors.push(msg)
    return pp
}

fn p_error(p: Parser, msg: string) -> Parser {
    let mut pp = p
    let tok = p_cur(pp)
    let full = "Error at " + int_to_string(line_in_file(tok.line)) + ":" + int_to_string(tok.col) + ": " + msg
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
    let mut down = p
    down.type_depth = down.type_depth + 1
    if down.type_depth == MAX_NESTING() + 1 {
        down = p_past_limit(down, nesting_error("a type is"))
    }
    let result = parse_type_at(down)
    let mut up = result.p
    up.type_depth = up.type_depth - 1
    return ParseTypeResult { p: up, type_name: result.type_name }
}

fn parse_type_at(p: Parser) -> ParseTypeResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    -- Check for dyn keyword
    if tok.kind == TK_IDENT() and tok.value == "dyn" {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        let inner = parse_type(pp)
        return ParseTypeResult { p: inner.p, type_name: DYN_PREFIX() + inner.type_name }
    }

    -- Check for fn type
    if tok.kind == TK_FN() {
        pp = p_advance(pp)
        pp = p_expect(pp, TK_LPAREN())
        let mut type_str = "fn("
        let mut first = true
        let mut turn1 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn1 {
                pp = p_skip_stuck(pp)
                turn1 = -1
                continue
            }
            turn1 = pp.pos
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
            if size_tok.kind != TK_INTEGER() and size_tok.kind != TK_IDENT() {
                pp = p_error(pp, "expected the size of the array but got " + token_kind_name(size_tok.kind))
            }
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
        pp = p_error(pp, "expected a type but got " + token_kind_name(tok.kind))
        return ParseTypeResult { p: pp, type_name: "" }
    }

    let name = tok.value
    pp = p_advance(pp)

    -- Check for generic args [T, U]
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut type_str = name + "["
        let mut first = true
        let mut turn2 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn2 {
                pp = p_skip_stuck(pp)
                turn2 = -1
                continue
            }
            turn2 = pp.pos
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
    let mut links = 0
    while cont {
        -- The last turn finds nothing more and ends the loop
        links = links + 1
        if links == MAX_CHAIN() + 2 {
            pp = p_past_limit(pp, "more than " + int_to_string(MAX_CHAIN()) + " calls, fields and indexes follow one another in this expression")
        }
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
                pp = p_error(pp, "expected a field or method name after '.' but got " + token_kind_name(field_tok.kind))
                cont = false
            }
        } else if kind == TK_LPAREN() and p_starts_line(pp) == false {
            -- A call of the value before it: make_adder(1)(2), fs[0](x).
            -- (A call of a name is parsed with the name.) At the start
            -- of a line a parenthesis begins a statement of its own.
            let line = p_cur(pp).line
            let col = p_cur(pp).col
            pp = p_advance(pp)
            let mut args: List[Expr] = []
            if p_peek(p_skip_newlines(pp)) != TK_RPAREN() {
                let arg = parse_expr(pp)
                pp = arg.p
                args.push(arg.expr)
                while p_peek(p_skip_newlines(pp)) == TK_COMMA() {
                    pp = p_expect(pp, TK_COMMA())
                    let next_arg = parse_expr(pp)
                    pp = next_arg.p
                    args.push(next_arg.expr)
                }
            }
            pp = p_skip_newlines(pp)
            pp = p_expect(pp, TK_RPAREN())
            left = expr_call(left, args, line, col)
        } else if kind == TK_LBRACKET() and p_starts_line(pp) == false {
            -- Index access: obj[idx]
            let line = p_cur(pp).line
            let col = p_cur(pp).col
            pp = p_advance(pp)
            let idx = parse_expr(pp)
            pp = idx.p
            pp = p_expect(pp, TK_RBRACKET())
            left = expr_index_access(left, idx.expr, line, col)
        } else {
            cont = false
        }
    }

    -- Infix: binary operators with precedence
    let mut keep_going = true
    links = 0
    while keep_going {
        links = links + 1
        if links == MAX_OPERATORS() + 2 {
            pp = p_past_limit(pp, "more than " + int_to_string(MAX_OPERATORS()) + " operators follow one another in this expression")
        }
        pp = p_skip_newlines(pp)
        let prec = infix_prec(p_peek(pp))
        if prec < min_prec or (p_starts_line(pp) and starts_expression(p_peek(pp))) {
            keep_going = false
        } else {
            let op_tok = p_cur(pp)
            let op_kind = p_peek(pp)

            if op_kind == TK_AS() {
                -- Cast: expr as Type
                pp = p_advance(pp)
                let cast_ref = p_peek(p_skip_newlines(pp)) == TK_AMP()
                let ty_result = parse_type(pp)
                pp = ty_result.p
                let mut e = expr_new(EXPR_CAST(), op_tok.line, op_tok.col)
                e.operand = Box_new(left)
                e.cast_type = ty_result.type_name
                e.cast_is_ref = cast_ref
                left = e
            -- Range operator (.. and ..=)
            } else if op_kind == TK_DOTDOT() or op_kind == TK_DOTDOTEQ() {
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
                -- left |> f(args) is the call f(left, args), and
                -- left |> f is f(left)
                let piped_to = right_result.expr
                let mut piped_args: List[Expr] = []
                piped_args.push(left)
                if piped_to.kind == EXPR_FUNCTION_CALL() {
                    let mut pai = 0
                    while pai < piped_to.args.len() {
                        piped_args.push(piped_to.args[pai])
                        pai = pai + 1
                    }
                    left = expr_call(*piped_to.callee, piped_args, op_tok.line, op_tok.col)
                } else if piped_to.kind == EXPR_METHOD_CALL() {
                    -- left |> x.f(args) is x.f(left, args)
                    let mut pai = 0
                    while pai < piped_to.args.len() {
                        piped_args.push(piped_to.args[pai])
                        pai = pai + 1
                    }
                    left = expr_method_call(*piped_to.object, piped_to.method, piped_args, op_tok.line, op_tok.col)
                } else {
                    left = expr_call(piped_to, piped_args, op_tok.line, op_tok.col)
                }
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

-- How tightly a binary operator binds; -1 for a token that is none.
-- The bitwise operators bind tighter than the comparisons, so
-- `a & mask == 0` compares `a & mask`; among them `|` binds loosest,
-- then `^`, then `&`, then the shifts.
fn infix_prec(kind: int) -> int {
    if kind == TK_DOTDOT() or kind == TK_DOTDOTEQ() { return 0 }
    if kind == TK_OR() { return 1 }
    if kind == TK_AND() { return 2 }
    if kind == TK_EQEQ() or kind == TK_BANGEQ() { return 3 }
    if kind == TK_LT() or kind == TK_GT() or kind == TK_LTEQ() or kind == TK_GTEQ() { return 4 }
    if kind == TK_PIPEGT() { return 5 }
    if kind == TK_PIPE() { return 6 }
    if kind == TK_CARET() { return 7 }
    if kind == TK_AMP() { return 8 }
    if kind == TK_SHL() or kind == TK_SHR() { return 9 }
    if kind == TK_PLUS() or kind == TK_MINUS() { return 10 }
    if kind == TK_STAR() or kind == TK_SLASH() or kind == TK_PERCENT() { return 11 }
    -- `x as T` binds tighter than any binary operator and looser than
    -- a prefix operator: `a * i as float` casts i, `-x as u8` casts -x
    -- and `*b as float` casts what is in the box
    if kind == TK_AS() { return 12 }
    return -1
}

-- What a unary operator's operand binds with: tighter than any binary
-- operator
fn PREFIX_PREC() -> int { return 13 }

-- Whether the token the parser is at is the first of its line
fn p_starts_line(p: Parser) -> bool {
    if p.pos < 1 or p.pos >= p.tokens.len() { return false }
    return p.tokens[p.pos - 1].kind == TK_NEWLINE()
}

-- Whether the token can begin an expression as well as continue one.
-- At the start of a line it begins one: `*b = 5`, `-x`, `[1, 2]`, a
-- lambda and a reference are statements of their own, not the rest of
-- the line before.
fn starts_expression(kind: int) -> bool {
    return kind == TK_MINUS() or kind == TK_STAR() or kind == TK_AMP() or kind == TK_PIPE() or kind == TK_LBRACKET()
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
    if kind == TK_AMP() { return BINOP_BIT_AND() }
    if kind == TK_PIPE() { return BINOP_BIT_OR() }
    if kind == TK_CARET() { return BINOP_BIT_XOR() }
    if kind == TK_SHL() { return BINOP_SHL() }
    if kind == TK_SHR() { return BINOP_SHR() }
    return -1
}

-- ============================================================
-- PARSE PREFIX EXPRESSIONS
-- ============================================================

fn parse_prefix(p: Parser) -> ParseExprResult {
    let mut down = p
    down.depth = down.depth + 1
    if down.depth == MAX_NESTING() + 1 {
        down = p_past_limit(down, nesting_error("an expression is"))
    }
    let result = parse_prefix_at(down)
    let mut up = result.p
    up.depth = up.depth - 1
    return ParseExprResult { p: up, expr: result.expr }
}

fn parse_prefix_at(p: Parser) -> ParseExprResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)

    -- Integer literal
    if tok.kind == TK_INTEGER() {
        pp = p_check_int(pp)
        pp = p_advance(pp)
        return ParseExprResult { p: pp, expr: expr_int(int_token_value(tok, false), tok.line, tok.col) }
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
        if p_cur(pp).kind == TK_INTEGER() and p_cur(pp).value == LOWEST_INT_DIGITS() {
            -- The lowest int: its digits alone are no int to negate
            let lowest = p_cur(pp)
            pp = p_advance(pp)
            return ParseExprResult { p: pp, expr: expr_int(int_token_value(lowest, true), tok.line, tok.col) }
        }
        let operand = parse_expr_pratt(pp, PREFIX_PREC())
        return ParseExprResult { p: operand.p, expr: expr_unary(UNOP_NEG(), operand.expr, tok.line, tok.col) }
    }

    -- Bitwise not
    if tok.kind == TK_TILDE() {
        pp = p_advance(pp)
        let operand = parse_expr_pratt(pp, PREFIX_PREC())
        return ParseExprResult { p: operand.p, expr: expr_unary(UNOP_BIT_NOT(), operand.expr, tok.line, tok.col) }
    }

    -- Unary not
    if tok.kind == TK_NOT() {
        pp = p_advance(pp)
        let operand = parse_expr_pratt(pp, PREFIX_PREC())
        return ParseExprResult { p: operand.p, expr: expr_unary(UNOP_NOT(), operand.expr, tok.line, tok.col) }
    }

    -- Dereference *expr (Box[T] -> T)
    if tok.kind == TK_STAR() {
        pp = p_advance(pp)
        let operand = parse_expr_pratt(pp, PREFIX_PREC())
        return ParseExprResult { p: operand.p, expr: expr_unary(UNOP_DEREF(), operand.expr, tok.line, tok.col) }
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
    pp = p_error(pp, "expected an expression but got " + token_kind_name(tok.kind))
    -- A closing token stays: it ends what is around this expression
    if tok.kind != TK_RBRACE() and tok.kind != TK_RPAREN() and tok.kind != TK_RBRACKET() {
        pp = p_advance(pp)
    }
    return ParseExprResult { p: pp, expr: expr_int(0, tok.line, tok.col) }
}

-- ============================================================
-- PARSE IDENTIFIER / CALL / STRUCT LITERAL / ENUM VARIANT
-- ============================================================

-- Whether the name is written as a type's is: a capital first, and a
-- small letter after it unless it is one letter long. `Stack` and `T`
-- are; `xs`, `TABLE` and `MAX_SIZE` are not.
fn p_type_like_name(name: string) -> bool {
    let n = len(name)
    if n == 0 { return false }
    let first = byte_at(name, 0)
    if first < 65 or first > 90 { return false }
    if n == 1 { return true }
    let mut i = 1
    while i < n {
        let b = byte_at(name, i)
        if b >= 97 and b <= 122 { return true }
        i = i + 1
    }
    return false
}

-- Whether the text of a type, as parse_type gives it, is written as a
-- type is and not as an index: it begins with the name of a builtin
-- type, or with a name written as a type's (p_type_like_name), or it
-- is a function's or an array's type. `int`, `List[T]` and `Shape`
-- are; `i`, `count` and `MAX` are not.
fn p_type_like(t: string) -> bool {
    if starts_with(t, "fn(") or starts_with(t, "[") { return true }
    let n = len(t)
    let mut end = 0
    while end < n and byte_at(t, end) != 91 {
        end = end + 1
    }
    let word = substr(t, 0, end)
    if word == "int" or word == "float" or word == "bool" or word == "string" or word == "void" { return true }
    if word == "i8" or word == "i16" or word == "i32" or word == "i64" { return true }
    if word == "u8" or word == "u16" or word == "u32" or word == "u64" { return true }
    if word == "f32" or word == "f64" { return true }
    if word == "f32x4" or word == "f32x8" or word == "f64x2" or word == "f64x4" { return true }
    if word == "i32x4" or word == "i32x8" or word == "i64x2" or word == "i64x4" { return true }
    return p_type_like_name(word)
}

fn p_types_like(types: List[string]) -> bool {
    let mut i = 0
    while i < types.len() {
        if p_type_like(types[i]) == false { return false }
        i = i + 1
    }
    return true
}

-- Whether the parser is at `.name(` or at `.Name`: the call of a
-- function, or a variant of an enum
fn p_call_follows_dot(p: Parser) -> bool {
    if p_peek(p) != TK_DOT() { return false }
    let at_name = p_advance(p)
    if p_peek(at_name) != TK_IDENT() { return false }
    let first = byte_at(p_cur(at_name).value, 0)
    if first >= 65 and first <= 90 { return true }
    return p_peek(p_advance(at_name)) == TK_LPAREN()
}

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
            -- A bare identifier also parses as a type name, so `a[i]` looks
            -- like a generic arg list here. Generic args in expression
            -- position only occur in calls `name[T](...)` and struct
            -- literals `Name[T] { ... }`; anything else is index access.
            -- A struct literal needs a capitalized name, as below, so that
            -- `if a == xs[i] { ... }` indexes `xs`.
            let after_args = p_peek(p_skip_newlines(pp))
            let mut is_generic = after_args == TK_LPAREN()
            if after_args == TK_LBRACE() and len(name) > 0 and char_at(name, 0) >= "A" and char_at(name, 0) <= "Z" {
                is_generic = true
            }
            -- `Stack[int].new()` calls a function of the type Stack[int],
            -- and `Opt[int].Empty` is a variant of Opt[int], where
            -- `Grid[i].len()` is a method call on an element: the name
            -- and what is in the brackets are a type's when they are
            -- written as one (p_type_like_name, p_type_like)
            if p_peek(pp) == TK_DOT() and p_type_like_name(name) and p_types_like(generics) and p_call_follows_dot(pp) {
                is_generic = true
            }
            if not is_generic {
                generics = []
                pp = saved
            }
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
            let mut turn3 = -1
            while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
                if pp.pos == turn3 {
                    pp = p_skip_stuck(pp)
                    turn3 = -1
                    continue
                }
                turn3 = pp.pos
                pp = p_skip_newlines(pp)
                pp = p_need_name(pp, "a field name")
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
                pp = p_comma_or(pp, TK_RBRACE())
            }
            pp = p_expect(pp, TK_RBRACE())
            let mut e = expr_new(EXPR_STRUCT_LITERAL(), tok.line, tok.col)
            -- A generic struct literal keeps its type arguments: Table[string, int]
            let mut lit_type = name
            if generics.len() > 0 {
                lit_type = name + "["
                let mut gi = 0
                while gi < generics.len() {
                    if gi > 0 { lit_type = lit_type + ", " }
                    lit_type = lit_type + generics[gi]
                    gi = gi + 1
                }
                lit_type = lit_type + "]"
            }
            e.type_name = lit_type
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
            e.generic_args = generics
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
    let mut turn4 = -1
    while p_peek(pp) != TK_RBRACKET() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn4 {
            pp = p_skip_stuck(pp)
            turn4 = -1
            continue
        }
        turn4 = pp.pos
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
        pp = p_comma_or(pp, TK_RBRACKET())
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
    let mut turn5 = -1
    while p_peek(pp) != TK_PIPE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn5 {
            pp = p_skip_stuck(pp)
            turn5 = -1
            continue
        }
        turn5 = pp.pos
        pp = p_need_name(pp, "a parameter name")
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
        pp = p_comma_or(pp, TK_PIPE())
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
    -- The `else if`s before this one; what is inside the branches
    -- begins its own count
    let before = pp.else_ifs
    pp.else_ifs = 0
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
            pp.else_ifs = before + 1
            if pp.else_ifs == MAX_ELSE_IFS() + 1 {
                pp = p_past_limit(pp, "more than " + int_to_string(MAX_ELSE_IFS()) + " `else if`s follow this `if` (a `match`, or `if`s that return, take any number)")
            }
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

    pp.else_ifs = before
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
    let mut turn6 = -1
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn6 {
            pp = p_skip_stuck(pp)
            turn6 = -1
            continue
        }
        turn6 = pp.pos
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
    -- The literals among a variant pattern's values are tests that come
    -- before the guard (see parse_variant_value)
    let tests = arm.pattern.tests
    if tests.len() > 0 {
        let mut cond = tests[0]
        let mut ti = 1
        while ti < tests.len() {
            cond = expr_binary(BINOP_AND(), cond, tests[ti], cond.line, cond.col)
            ti = ti + 1
        }
        if arm.has_guard {
            cond = expr_binary(BINOP_AND(), cond, *arm.guard, cond.line, cond.col)
        }
        arm.guard = Box_new(cond)
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
        if ch == "\\" and i + 1 < tlen {
            -- An escape stays as it is written until the text is whole
            current_lit = current_lit + ch + char_at(template, i + 1)
            i = i + 2
        } else if ch == "{" and i + 1 < tlen and char_at(template, i + 1) == "{" {
            -- {{ is a brace of the text, and so is }}
            current_lit = current_lit + "{"
            i = i + 2
        } else if ch == "}" and i + 1 < tlen and char_at(template, i + 1) == "}" {
            current_lit = current_lit + "}"
            i = i + 2
        } else if ch == "{" {
            -- Emit accumulated literal
            if len(current_lit) > 0 {
                parts.push(expr_string(unescape_string(current_lit), tok.line, tok.col))
                is_literal.push(true)
                current_lit = ""
            }
            -- Find the matching }. A string in the expression is
            -- written with \" (a plain " would end the f-string), and
            -- a brace in such a string is not the expression's.
            i = i + 1
            let start = i
            let mut depth = 1
            let mut in_string = false
            while i < tlen and depth > 0 {
                let c2 = char_at(template, i)
                if c2 == "\\" and i + 1 < tlen {
                    if char_at(template, i + 1) == "\"" { in_string = in_string == false }
                    i = i + 2
                } else {
                    if in_string == false {
                        if c2 == "{" { depth = depth + 1 }
                        if c2 == "}" { depth = depth - 1 }
                    }
                    if depth > 0 { i = i + 1 }
                }
            }
            let expr_text = unescape_string(substr(template, start, i - start))
            if depth > 0 or in_string or string_trim(expr_text) == "" {
                let mut what = "an f-string has a '{' without a '}' (write {{ for a brace)"
                if in_string { what = "a string in an f-string's expression has no end" }
                if depth == 0 and in_string == false { what = "an f-string has an empty {}" }
                -- The error is at the f-string
                let mut at_fstring = pp
                at_fstring.pos = pp.pos - 1
                at_fstring = p_error(at_fstring, what)
                pp.errors = at_fstring.errors
                parts.push(expr_string("", tok.line, tok.col))
                is_literal.push(true)
                i = tlen
                continue
            }
            -- Tokenize and parse the expression. Its tokens are at the
            -- f-string's place, which is where its errors are reported.
            let lexed = tokenize(expr_text)
            let mut expr_tokens: List[Token] = []
            let mut ti = 0
            while ti < lexed.len() {
                expr_tokens.push(token_new(lexed[ti].kind, lexed[ti].value, tok.line, tok.col))
                ti = ti + 1
            }
            let expr_parser = parser_new(expr_tokens)
            let expr_result = parse_expr(expr_parser)
            if expr_result.p.errors.len() > 0 {
                pp.errors.push(expr_result.p.errors[0])
            }
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
        parts.push(expr_string(unescape_string(current_lit), tok.line, tok.col))
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

-- One value of a variant's pattern, as in Color.Rgb(r, _, 0): a name
-- binds the value, `_` leaves it, and a literal must equal it. The
-- value a literal stands at is bound to a name of its own, and
-- `name == literal` is added to the pattern's tests.
fn parse_variant_value(p: Parser, into: Pattern) -> ParsePatternResult {
    let mut pp = p
    let mut pat = into
    let tok = p_cur(pp)
    if tok.kind == TK_IDENT() or tok.kind == TK_UNDERSCORE() {
        pp = p_advance(pp)
        if tok.kind == TK_UNDERSCORE() {
            pat.bindings.push("_")
        } else {
            pat.bindings.push(tok.value)
        }
        return ParsePatternResult { p: pp, pattern: pat }
    }
    let mut literal = expr_int(0, tok.line, tok.col)
    if tok.kind == TK_INTEGER() {
        pp = p_check_int(pp)
        literal = expr_int(int_token_value(tok, false), tok.line, tok.col)
    } else if tok.kind == TK_MINUS() and p_cur(p_advance(pp)).kind == TK_INTEGER() {
        pp = p_advance(pp)
        literal = expr_int(int_token_value(p_cur(pp), true), tok.line, tok.col)
    } else if tok.kind == TK_STRING() {
        literal = expr_string(tok.value, tok.line, tok.col)
    } else if tok.kind == TK_TRUE() or tok.kind == TK_FALSE() {
        literal = expr_bool(tok.kind == TK_TRUE(), tok.line, tok.col)
    } else {
        pp = p_error(pp, "expected a name, '_', an integer, a string or a bool for a value of the variant but got " + token_kind_name(tok.kind))
        pp = p_advance(pp)
        pat.bindings.push("_")
        return ParsePatternResult { p: pp, pattern: pat }
    }
    pp = p_advance(pp)
    -- No name in a program has a `$`
    let name = "$lit_" + int_to_string(tok.line) + "_" + int_to_string(tok.col)
    pat.bindings.push(name)
    pat.tests.push(expr_binary(BINOP_EQ(), expr_ident(name, tok.line, tok.col), literal, tok.line, tok.col))
    return ParsePatternResult { p: pp, pattern: pat }
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
        pp = p_check_int(pp)
        pp = p_advance(pp)
        let mut pat = pattern_new(PAT_LITERAL())
        pat.literal_expr = Box_new(expr_int(int_token_value(tok, false), tok.line, tok.col))
        return ParsePatternResult { p: pp, pattern: pat }
    }
    if tok.kind == TK_MINUS() and p_cur(p_advance(pp)).kind == TK_INTEGER() {
        pp = p_advance(pp)
        let mut pat = pattern_new(PAT_LITERAL())
        pat.literal_expr = Box_new(expr_int(int_token_value(p_cur(pp), true), tok.line, tok.col))
        pp = p_advance(pp)
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
            let no_bindings: List[string] = []
            let mut variant_pat = pattern_enum_variant("", name, no_bindings)
            let mut turn7 = -1
            while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                if pp.pos == turn7 {
                    pp = p_skip_stuck(pp)
                    turn7 = -1
                    continue
                }
                turn7 = pp.pos
                pp = p_skip_newlines(pp)
                let value = parse_variant_value(pp, variant_pat)
                pp = value.p
                variant_pat = value.pattern
                pp = p_comma_or(pp, TK_RPAREN())
            }
            pp = p_expect(pp, TK_RPAREN())
            return ParsePatternResult { p: pp, pattern: variant_pat }
        }

        if p_peek(pp) == TK_DOT() or p_peek(pp) == TK_COLONCOLON() {
            -- Qualified enum: EnumName.Variant or EnumName::Variant
            pp = p_advance(pp)
            pp = p_need_name(pp, "a variant name")
            let variant_tok = p_cur(pp)
            pp = p_advance(pp)
            let no_bindings: List[string] = []
            let mut variant_pat = pattern_enum_variant(name, variant_tok.value, no_bindings)
            if p_peek(pp) == TK_LPAREN() {
                pp = p_advance(pp)
                let mut turn8 = -1
                while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                    if pp.pos == turn8 {
                        pp = p_skip_stuck(pp)
                        turn8 = -1
                        continue
                    }
                    turn8 = pp.pos
                    pp = p_skip_newlines(pp)
                    let value = parse_variant_value(pp, variant_pat)
                    pp = value.p
                    variant_pat = value.pattern
                    pp = p_comma_or(pp, TK_RPAREN())
                }
                pp = p_expect(pp, TK_RPAREN())
            }
            return ParsePatternResult { p: pp, pattern: variant_pat }
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

-- Skip what separates the statements of a block: line ends, and the
-- `;` between statements written on one line
fn p_skip_separators(p: Parser) -> Parser {
    let mut pp = p
    while p_peek(pp) == TK_NEWLINE() or p_peek(pp) == TK_SEMICOLON() {
        pp = p_advance(pp)
    }
    return pp
}

fn parse_block_body(p: Parser) -> ParseBlockResult {
    let mut pp = p_skip_separators(p)
    let mut stmts: List[Stmt] = []
    let mut turn9 = -1
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn9 {
            pp = p_skip_stuck(pp)
            turn9 = -1
            continue
        }
        turn9 = pp.pos
        let result = parse_stmt(pp)
        pp = result.p
        stmts.push(result.stmt)
        pp = p_skip_separators(pp)
    }
    return ParseBlockResult { p: pp, stmts: stmts }
}

fn parse_stmt(p: Parser) -> ParseStmtResult {
    let mut down = p
    down.block_depth = down.block_depth + 1
    if down.block_depth == MAX_NESTING() + 1 {
        down = p_past_limit(down, nesting_error("a statement is"))
    }
    let result = parse_stmt_at(down)
    let mut up = result.p
    up.block_depth = up.block_depth - 1
    return ParseStmtResult { p: up, stmt: result.stmt }
}

fn parse_stmt_at(p: Parser) -> ParseStmtResult {
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

    -- Unsafe block (IR generation enforces the fence)
    if tok.kind == TK_UNSAFE() {
        return parse_unsafe_stmt(pp)
    }

    -- Expression statement (may also be assignment)
    let expr_result = parse_expr(pp)
    pp = expr_result.p

    -- Check for assignment: expr = value, expr += value, etc.
    pp = p_skip_newlines(pp)
    let assign_kind = p_peek(pp)
    if assign_kind == TK_EQ() or assign_kind == TK_PLUSEQ() or assign_kind == TK_MINUSEQ() or assign_kind == TK_STAREQ() or assign_kind == TK_SLASHEQ() or assign_kind == TK_PERCENTEQ() {
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
    if kind == TK_PERCENTEQ() { return ASSIGN_MOD() }
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
    pp = p_need_name(pp, "a variable name")
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
    } else {
        -- A variable without a value would be read before it is written
        pp = p_error(pp, "expected '=' and a value for '" + name_tok.value + "' but got " + token_kind_name(p_peek(pp)))
        s.let_value = Box_new(expr_new(EXPR_LITERAL_NULL(), tok.line, tok.col))
    }

    return ParseStmtResult { p: pp, stmt: s }
}

fn parse_const_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'const'
    pp = p_skip_newlines(pp)
    pp = p_need_name(pp, "a constant name")
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

-- An `if` statement is parsed as the expression, which it is where
-- the block it ends is used as a value (if_stmt_of in ast.dm)
fn parse_if_stmt(p: Parser) -> ParseStmtResult {
    let parsed = parse_if_expr(p)
    return ParseStmtResult { p: parsed.p, stmt: if_stmt_of(parsed.expr) }
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
    pp = p_need_name(pp, "a loop variable")
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
    pp = p_need_name(pp, "a region name")
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

fn parse_unsafe_stmt(p: Parser) -> ParseStmtResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'unsafe'
    pp = p_expect(pp, TK_LBRACE())
    let body = parse_block_body(pp)
    pp = body.p
    pp = p_expect(pp, TK_RBRACE())
    let mut s = stmt_new(STMT_REGION(), tok.line, tok.col)
    s.region_name = "unsafe"
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
        pp = p_need_name(pp, "a module name")
        let name_tok = p_cur(pp)
        sf.module_name = name_tok.value
        pp = p_advance(pp)
        -- A dotted name: module tests.arithmetic
        while p_peek(pp) == TK_DOT() {
            pp = p_advance(pp)
            pp = p_need_name(pp, "a module name")
            sf.module_name = sf.module_name + "." + p_cur(pp).value
            pp = p_advance(pp)
        }
    }
    pp = p_skip_newlines(pp)

    -- Imports
    while p_peek(pp) == TK_IMPORT() {
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        pp = p_need_name(pp, "a module name")
        let path_tok = p_cur(pp)
        let mut path = path_tok.value
        pp = p_advance(pp)
        -- Handle dotted paths: import std.io and import foo::bar
        while p_peek(pp) == TK_DOT() or p_peek(pp) == TK_COLONCOLON() {
            pp = p_advance(pp)
            pp = p_need_name(pp, "a module name")
            path = path + "." + p_cur(pp).value
            pp = p_advance(pp)
        }
        sf.imports.push(import_decl_new(path, path_tok.line, path_tok.col))
        -- import m { a, b } names what the file uses of m. Everything
        -- m declares is visible either way.
        if p_peek(pp) == TK_LBRACE() {
            pp = p_advance(pp)
            let mut turn10 = -1
            while p_peek(p_skip_newlines(pp)) != TK_RBRACE() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                if pp.pos == turn10 {
                    pp = p_skip_stuck(pp)
                    turn10 = -1
                    continue
                }
                turn10 = pp.pos
                pp = p_skip_newlines(pp)
                pp = p_need_name(pp, "the name of a declaration")
                pp = p_advance(pp)
                pp = p_comma_or(pp, TK_RBRACE())
            }
            pp = p_skip_newlines(pp)
            pp = p_expect(pp, TK_RBRACE())
        }
        pp = p_skip_newlines(pp)
    }

    -- The first error of the module line and the imports. The parser
    -- is passed by value, so its errors are in the last copy, not in
    -- the caller's: they leave in the SourceFile.
    if pp.errors.len() > 0 {
        sf.errors.push(pp.errors[0])
    }

    -- Declarations
    let mut turn11 = -1
    while p_peek(pp) != TK_EOF() {
        if pp.pos == turn11 {
            pp = p_skip_stuck(pp)
            turn11 = -1
            continue
        }
        turn11 = pp.pos
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_EOF() { break }
        let errors_before = pp.errors.len()
        let decl_result = parse_declaration(pp)
        pp = decl_result.p
        -- After its first error a declaration is parsed out of step,
        -- and what follows that error is noise
        if pp.errors.len() > errors_before {
            sf.errors.push(pp.errors[errors_before])
        }
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

    -- Not a declaration: report it, and skip to the next token that
    -- starts one
    pp = p_error(pp, "expected a declaration but got " + token_kind_name(tok.kind))
    pp = p_advance(pp)
    while p_peek(pp) != TK_EOF() and p_peek(pp) != TK_FN() and p_peek(pp) != TK_ASYNC() and p_peek(pp) != TK_EXTERN() and p_peek(pp) != TK_STRUCT() and p_peek(pp) != TK_ENUM() and p_peek(pp) != TK_TRAIT() and p_peek(pp) != TK_IMPL() and p_peek(pp) != TK_CONST() {
        pp = p_advance(pp)
    }
    -- Return a dummy
    let fd = func_decl_new("__error__", tok.line, tok.col)
    return ParseDeclResult { p: pp, decl: decl_function(fd) }
}

fn parse_fn_decl(p: Parser) -> ParseDeclResult {
    let from = expr_id_count()
    let mut parsed = parse_fn_decl_parts(p)
    if parsed.decl.kind == DECL_FUNCTION() {
        let mut fd = *parsed.decl.func_decl
        fd.expr_from = from
        fd.expr_to = expr_id_count()
        parsed.decl.func_decl = Box_new(fd)
    }
    return parsed
}

fn parse_fn_decl_parts(p: Parser) -> ParseDeclResult {
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
    pp = p_need_name(pp, "a function name")
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut fd = func_decl_new(name_tok.value, tok.line, tok.col)
    fd.is_async = is_async

    -- Generic params [T, U]
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut turn12 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn12 {
                pp = p_skip_stuck(pp)
                turn12 = -1
                continue
            }
            turn12 = pp.pos
            pp = p_skip_newlines(pp)
            pp = p_need_name(pp, "a type parameter")
            let gp = p_cur(pp)
            pp = p_advance(pp)
            -- T: Show + Named
            let mut bound = ""
            if p_peek(pp) == TK_COLON() {
                pp = p_advance(pp)
                let mut more = true
                while more {
                    pp = p_need_name(pp, "a trait name")
                    if p_peek(pp) == TK_IDENT() {
                        if bound != "" { bound = bound + "+" }
                        bound = bound + p_cur(pp).value
                        pp = p_advance(pp)
                    }
                    more = p_peek(pp) == TK_PLUS()
                    if more { pp = p_advance(pp) }
                }
            }
            fd.generic_params.push(gp.value)
            fd.generic_bounds.push(bound)
            pp = p_comma_or(pp, TK_RBRACKET())
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    -- Parameters
    pp = p_expect(pp, TK_LPAREN())
    pp = p_skip_newlines(pp)
    let mut turn13 = -1
    while p_peek(pp) != TK_RPAREN() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn13 {
            pp = p_skip_stuck(pp)
            turn13 = -1
            continue
        }
        turn13 = pp.pos
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
                pp = p_comma_or(pp, TK_RPAREN())
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
            pp = p_comma_or(pp, TK_RPAREN())
            pp = p_skip_newlines(pp)
            continue
        }

        pp = p_need_name(pp, "a parameter name")
        let pname = p_cur(pp).value
        pp = p_advance(pp)
        pp = p_expect(pp, TK_COLON())
        let pty_ref = p_peek(p_skip_newlines(pp)) == TK_AMP()
        let pty = parse_type(pp)
        pp = pty.p
        let mut param = func_param_new(pname, pty.type_name)
        param.is_mut = is_mut
        param.is_ref = pty_ref
        fd.params.push(param)
        pp = p_comma_or(pp, TK_RPAREN())
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
        let mut turn14 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn14 {
                pp = p_skip_stuck(pp)
                turn14 = -1
                continue
            }
            turn14 = pp.pos
            pp = p_skip_newlines(pp)
            pp = p_need_name(pp, "an effect name")
            let eff = p_cur(pp).value
            pp = p_advance(pp)
            fd.effects.push(eff)
            pp = p_comma_or(pp, TK_RBRACKET())
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
    pp = p_need_name(pp, "a function name")
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut fd = func_decl_new(name_tok.value, tok.line, tok.col)
    fd.is_extern = true

    -- Parameters
    pp = p_expect(pp, TK_LPAREN())
    pp = p_skip_newlines(pp)
    let mut turn15 = -1
    while p_peek(pp) != TK_RPAREN() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn15 {
            pp = p_skip_stuck(pp)
            turn15 = -1
            continue
        }
        turn15 = pp.pos
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_DOTDOT() {
            -- `...` (lexed as `..` then `.`): further arguments are C varargs
            pp = p_advance(pp)
            pp = p_expect(pp, TK_DOT())
            fd.is_variadic = true
            pp = p_skip_newlines(pp)
            if p_peek(pp) != TK_RPAREN() {
                eprintln("Error: `...` must be the last parameter of extern fn '" + fd.name + "' at line " + int_to_string(line_in_file(tok.line)))
                exit(1)
            }
        } else {
            pp = p_need_name(pp, "a parameter name")
            let pname = p_cur(pp).value
            pp = p_advance(pp)
            pp = p_expect(pp, TK_COLON())
            let pty_ref = p_peek(p_skip_newlines(pp)) == TK_AMP()
            let pty = parse_type(pp)
            pp = pty.p
            let mut eparam = func_param_new(pname, pty.type_name)
            eparam.is_ref = pty_ref
            fd.params.push(eparam)
            pp = p_comma_or(pp, TK_RPAREN())
            pp = p_skip_newlines(pp)
        }
    }
    pp = p_expect(pp, TK_RPAREN())

    if p_peek(p_skip_newlines(pp)) == TK_ARROW() {
        pp = p_skip_newlines(pp)
        pp = p_advance(pp)
        let rty = parse_type(pp)
        pp = rty.p
        fd.return_type = rty.type_name
    }

    -- `= "symbol"` binds the function to a C symbol of another name, so
    -- that two declarations can name the same C function
    if p_peek(pp) == TK_EQ() {
        pp = p_advance(pp)
        if p_peek(pp) != TK_STRING() or p_cur(pp).value == "" {
            eprintln("Error: expected a C symbol name in quotes after `=` in extern fn '" + fd.name + "' at line " + int_to_string(line_in_file(tok.line)))
            exit(1)
        }
        fd.link_name = p_cur(pp).value
        pp = p_advance(pp)
    }

    return ParseDeclResult { p: pp, decl: decl_function(fd) }
}

fn parse_struct_decl(p: Parser) -> ParseDeclResult {
    let mut pp = p
    let tok = p_cur(pp)
    pp = p_advance(pp) -- skip 'struct'
    pp = p_skip_newlines(pp)
    pp = p_need_name(pp, "a struct name")
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut sd = struct_decl_new(name_tok.value, tok.line, tok.col)

    -- Generic params
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut turn16 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn16 {
                pp = p_skip_stuck(pp)
                turn16 = -1
                continue
            }
            turn16 = pp.pos
            pp = p_skip_newlines(pp)
            pp = p_need_name(pp, "a type parameter")
            sd.generic_params.push(p_cur(pp).value)
            pp = p_advance(pp)
            pp = p_comma_or(pp, TK_RBRACKET())
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    let mut turn17 = -1
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn17 {
            pp = p_skip_stuck(pp)
            turn17 = -1
            continue
        }
        turn17 = pp.pos
        pp = p_skip_newlines(pp)
        pp = p_need_name(pp, "a field name")
        let fname = p_cur(pp).value
        pp = p_advance(pp)
        pp = p_expect(pp, TK_COLON())
        let fty = parse_type(pp)
        pp = fty.p
        sd.fields.push(struct_field_new(fname, fty.type_name))
        pp = p_skip_newlines(pp)
        pp = p_comma_or(pp, TK_RBRACE())
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
    pp = p_need_name(pp, "an enum name")
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut ed = enum_decl_new(name_tok.value, tok.line, tok.col)

    -- Generic params
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut turn18 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn18 {
                pp = p_skip_stuck(pp)
                turn18 = -1
                continue
            }
            turn18 = pp.pos
            pp = p_skip_newlines(pp)
            pp = p_need_name(pp, "a type parameter")
            ed.generic_params.push(p_cur(pp).value)
            pp = p_advance(pp)
            pp = p_comma_or(pp, TK_RBRACKET())
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    let mut turn19 = -1
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn19 {
            pp = p_skip_stuck(pp)
            turn19 = -1
            continue
        }
        turn19 = pp.pos
        pp = p_skip_newlines(pp)
        pp = p_need_name(pp, "a variant name")
        let vname = p_cur(pp).value
        pp = p_advance(pp)
        let mut variant = enum_variant_new(vname)
        -- Optional payload: Variant(Type1, Type2)
        if p_peek(pp) == TK_LPAREN() {
            pp = p_advance(pp)
            variant.has_payload = true
            let mut turn20 = -1
            while p_peek(p_skip_newlines(pp)) != TK_RPAREN() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                if pp.pos == turn20 {
                    pp = p_skip_stuck(pp)
                    turn20 = -1
                    continue
                }
                turn20 = pp.pos
                pp = p_skip_newlines(pp)
                let pty = parse_type(pp)
                pp = pty.p
                variant.payload_types.push(pty.type_name)
                pp = p_comma_or(pp, TK_RPAREN())
            }
            pp = p_expect(pp, TK_RPAREN())
        }
        ed.variants.push(variant)
        pp = p_skip_newlines(pp)
        pp = p_comma_or(pp, TK_RBRACE())
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
    pp = p_need_name(pp, "a trait name")
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut td = trait_decl_new(name_tok.value, tok.line, tok.col)

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    let mut turn21 = -1
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn21 {
            pp = p_skip_stuck(pp)
            turn21 = -1
            continue
        }
        turn21 = pp.pos
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_FN() {
            let method_result = parse_fn_decl(pp)
            pp = method_result.p
            td.methods.push(*method_result.decl.func_decl)
        } else {
            pp = p_error(pp, "expected 'fn' in a trait but got " + token_kind_name(p_peek(pp)))
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
    pp = p_need_name(pp, "a type name")
    let first_name = p_cur(pp).value
    pp = p_advance(pp)

    let mut ib = impl_block_new(first_name, tok.line, tok.col)

    -- Parse optional generic params [A, B] after the type name
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut turn22 = -1
        while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
            if pp.pos == turn22 {
                pp = p_skip_stuck(pp)
                turn22 = -1
                continue
            }
            turn22 = pp.pos
            pp = p_skip_newlines(pp)
            pp = p_need_name(pp, "a type parameter")
            ib.generic_params.push(p_cur(pp).value)
            pp = p_advance(pp)
            pp = p_comma_or(pp, TK_RBRACKET())
        }
        pp = p_expect(pp, TK_RBRACKET())
    }

    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_FOR() {
        -- impl Trait for Type
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        pp = p_need_name(pp, "a type name")
        let target_name = p_cur(pp).value
        pp = p_advance(pp)
        ib.trait_name = first_name
        ib.target_type = target_name
        -- Parse optional generic params on the target type too
        if p_peek(pp) == TK_LBRACKET() {
            pp = p_advance(pp)
            let mut turn23 = -1
            while p_peek(p_skip_newlines(pp)) != TK_RBRACKET() and p_peek(p_skip_newlines(pp)) != TK_EOF() {
                if pp.pos == turn23 {
                    pp = p_skip_stuck(pp)
                    turn23 = -1
                    continue
                }
                turn23 = pp.pos
                pp = p_skip_newlines(pp)
                pp = p_need_name(pp, "a type parameter")
                ib.generic_params.push(p_cur(pp).value)
                pp = p_advance(pp)
                pp = p_comma_or(pp, TK_RBRACKET())
            }
            pp = p_expect(pp, TK_RBRACKET())
        }
    }

    pp = p_expect(pp, TK_LBRACE())
    pp = p_skip_newlines(pp)
    let mut turn24 = -1
    while p_peek(pp) != TK_RBRACE() and p_peek(pp) != TK_EOF() {
        if pp.pos == turn24 {
            pp = p_skip_stuck(pp)
            turn24 = -1
            continue
        }
        turn24 = pp.pos
        pp = p_skip_newlines(pp)
        if p_peek(pp) == TK_FN() or p_peek(pp) == TK_ASYNC() {
            let method_result = parse_fn_decl(pp)
            pp = method_result.p
            ib.methods.push(*method_result.decl.func_decl)
        } else {
            pp = p_error(pp, "expected 'fn' in an impl but got " + token_kind_name(p_peek(pp)))
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
    pp = p_need_name(pp, "a constant name")
    let name_tok = p_cur(pp)
    pp = p_advance(pp)

    let mut cd = const_decl_new(name_tok.value, tok.line, tok.col)

    -- Optional type: const FOO: int = expr
    if p_peek(pp) == TK_COLON() {
        pp = p_advance(pp)
        let cty = parse_type(pp)
        pp = cty.p
        cd.type_name = cty.type_name
    }

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
