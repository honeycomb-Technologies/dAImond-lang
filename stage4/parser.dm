module parser

import token
import lexer
import ast

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

            -- Pipeline operator
            if op_kind == TK_PIPEGT() {
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
        pp = p_expect(pp, TK_RPAREN())
        let mut e = expr_call(expr_ident(name, tok.line, tok.col), args, tok.line, tok.col)
        e.generic_args = generics
        return ParseExprResult { p: pp, expr: e }
    }

    -- Check for struct literal: Name { field: val, ... }
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
                pp = p_expect(pp, TK_COLON())
                let fval = parse_expr(pp)
                pp = fval.p
                field_names.push(fname)
                field_values.push(fval.expr)
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

    -- Check for enum variant: Name.Variant or Name.Variant(payload)
    if p_peek(pp) == TK_DOT() {
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

        if p_peek(pp) == TK_DOT() {
            -- Qualified enum: EnumName.Variant or EnumName.Variant(bindings)
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
        -- Handle dotted paths: import std.io
        while p_peek(pp) == TK_DOT() {
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
                fd.params.push(func_param_new("self", "Self"))
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
            fd.params.push(func_param_new("self", "Self"))
            pp = p_advance(pp)
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

    -- Parse: impl Type { ... } or impl Trait for Type { ... }
    let first_name = p_cur(pp).value
    pp = p_advance(pp)

    let mut ib = impl_block_new(first_name, tok.line, tok.col)

    pp = p_skip_newlines(pp)
    if p_peek(pp) == TK_FOR() {
        -- impl Trait for Type
        pp = p_advance(pp)
        pp = p_skip_newlines(pp)
        let target_name = p_cur(pp).value
        pp = p_advance(pp)
        ib.trait_name = first_name
        ib.target_type = target_name
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
