-- dAImond Scientific Calculator
-- This is the first real program for the dAImond language.
-- It demonstrates lexing, parsing, evaluation, and error handling.

module calculator

import std.io { print, read_line }
import std.math { sin, cos, tan, sqrt, log, ln, exp, pow, PI, E }
import std.map { Map }

-- Token types for the calculator's own lexer
enum Token {
    Number(float),
    Plus, Minus, Star, Slash, Caret, Percent,
    LParen, RParen,
    Identifier(str),
    Let, Equals,
    Eof,
}

-- Expression AST
enum Expr {
    Number(float),
    Variable(str),
    Unary { op: UnaryOp, operand: Box[Expr] },
    Binary { op: BinaryOp, left: Box[Expr], right: Box[Expr] },
    Call { name: str, arg: Box[Expr] },
}

enum UnaryOp { Neg }
enum BinaryOp { Add, Sub, Mul, Div, Pow, Mod }

-- Lexer for calculator expressions
struct Lexer {
    input: str,
    pos: uint,
}

impl Lexer {
    fn new(input: str) -> Lexer {
        return Lexer { input, pos: 0 }
    }

    fn next_token(mut self) -> Result[Token, LexError] {
        self.skip_whitespace()

        if self.pos >= self.input.len() {
            return Ok(Token.Eof)
        }

        let c = self.input[self.pos]

        match c {
            '+' => { self.pos += 1; return Ok(Token.Plus) },
            '-' => { self.pos += 1; return Ok(Token.Minus) },
            '*' => { self.pos += 1; return Ok(Token.Star) },
            '/' => { self.pos += 1; return Ok(Token.Slash) },
            '^' => { self.pos += 1; return Ok(Token.Caret) },
            '%' => { self.pos += 1; return Ok(Token.Percent) },
            '(' => { self.pos += 1; return Ok(Token.LParen) },
            ')' => { self.pos += 1; return Ok(Token.RParen) },
            '=' => { self.pos += 1; return Ok(Token.Equals) },
            _ => {}
        }

        if c.is_digit() or c == '.' {
            return self.read_number()
        }

        if c.is_alpha() {
            return self.read_identifier()
        }

        return Err(LexError.UnexpectedChar(c))
    }

    private fn skip_whitespace(mut self) {
        while self.pos < self.input.len() and self.input[self.pos].is_whitespace() {
            self.pos += 1
        }
    }

    private fn read_number(mut self) -> Result[Token, LexError] {
        let start = self.pos
        while self.pos < self.input.len() and (self.input[self.pos].is_digit() or self.input[self.pos] == '.') {
            self.pos += 1
        }
        -- Handle scientific notation
        if self.pos < self.input.len() and (self.input[self.pos] == 'e' or self.input[self.pos] == 'E') {
            self.pos += 1
            if self.pos < self.input.len() and (self.input[self.pos] == '+' or self.input[self.pos] == '-') {
                self.pos += 1
            }
            while self.pos < self.input.len() and self.input[self.pos].is_digit() {
                self.pos += 1
            }
        }
        let text = self.input.slice(start, self.pos)
        match text.parse_float() {
            Ok(n) => Ok(Token.Number(n)),
            Err(_) => Err(LexError.InvalidNumber(text)),
        }
    }

    private fn read_identifier(mut self) -> Result[Token, LexError] {
        let start = self.pos
        while self.pos < self.input.len() and self.input[self.pos].is_alphanumeric() {
            self.pos += 1
        }
        let text = self.input.slice(start, self.pos)
        match text {
            "let" => Ok(Token.Let),
            _ => Ok(Token.Identifier(text)),
        }
    }
}

-- Parser using recursive descent
struct Parser {
    lexer: Lexer,
    current: Token,
}

impl Parser {
    fn new(input: str) -> Result[Parser, ParseError] {
        let mut lexer = Lexer.new(input)
        let current = lexer.next_token()?
        return Ok(Parser { lexer, current })
    }

    fn parse(mut self) -> Result[Expr, ParseError] {
        return self.expression()
    }

    private fn advance(mut self) -> Result[(), ParseError] {
        self.current = self.lexer.next_token()?
        return Ok(())
    }

    private fn expression(mut self) -> Result[Expr, ParseError] {
        return self.addition()
    }

    private fn addition(mut self) -> Result[Expr, ParseError] {
        let mut left = self.multiplication()?

        while self.current == Token.Plus or self.current == Token.Minus {
            let op = if self.current == Token.Plus { BinaryOp.Add } else { BinaryOp.Sub }
            self.advance()?
            let right = self.multiplication()?
            left = Expr.Binary { op, left: Box.new(left), right: Box.new(right) }
        }

        return Ok(left)
    }

    private fn multiplication(mut self) -> Result[Expr, ParseError] {
        let mut left = self.power()?

        while self.current == Token.Star or self.current == Token.Slash or self.current == Token.Percent {
            let op = match self.current {
                Token.Star => BinaryOp.Mul,
                Token.Slash => BinaryOp.Div,
                Token.Percent => BinaryOp.Mod,
                _ => unreachable(),
            }
            self.advance()?
            let right = self.power()?
            left = Expr.Binary { op, left: Box.new(left), right: Box.new(right) }
        }

        return Ok(left)
    }

    private fn power(mut self) -> Result[Expr, ParseError] {
        let base = self.unary()?

        if self.current == Token.Caret {
            self.advance()?
            let exp = self.power()?  -- right associative
            return Ok(Expr.Binary { op: BinaryOp.Pow, left: Box.new(base), right: Box.new(exp) })
        }

        return Ok(base)
    }

    private fn unary(mut self) -> Result[Expr, ParseError] {
        if self.current == Token.Minus {
            self.advance()?
            let operand = self.unary()?
            return Ok(Expr.Unary { op: UnaryOp.Neg, operand: Box.new(operand) })
        }
        return self.call()
    }

    private fn call(mut self) -> Result[Expr, ParseError] {
        if let Token.Identifier(name) = self.current {
            self.advance()?
            if self.current == Token.LParen {
                self.advance()?
                let arg = self.expression()?
                if self.current != Token.RParen {
                    return Err(ParseError.ExpectedRParen)
                }
                self.advance()?
                return Ok(Expr.Call { name, arg: Box.new(arg) })
            }
            return Ok(Expr.Variable(name))
        }
        return self.primary()
    }

    private fn primary(mut self) -> Result[Expr, ParseError] {
        match self.current {
            Token.Number(n) => {
                self.advance()?
                return Ok(Expr.Number(n))
            },
            Token.LParen => {
                self.advance()?
                let expr = self.expression()?
                if self.current != Token.RParen {
                    return Err(ParseError.ExpectedRParen)
                }
                self.advance()?
                return Ok(expr)
            },
            _ => return Err(ParseError.UnexpectedToken(self.current)),
        }
    }
}

-- Evaluator
struct Evaluator {
    variables: Map[str, float],
    ans: float,
}

impl Evaluator {
    fn new() -> Evaluator {
        let mut vars = Map.new()
        vars.insert("pi", PI)
        vars.insert("e", E)
        return Evaluator { variables: vars, ans: 0.0 }
    }

    fn eval(mut self, expr: Expr) -> Result[float, EvalError] {
        let result = self.eval_expr(expr)?
        self.ans = result
        self.variables.insert("ans", result)
        return Ok(result)
    }

    fn set_variable(mut self, name: str, value: float) {
        self.variables.insert(name, value)
    }

    private fn eval_expr(self, expr: Expr) -> Result[float, EvalError] {
        match expr {
            Expr.Number(n) => Ok(n),

            Expr.Variable(name) => {
                match self.variables.get(name) {
                    Some(v) => Ok(v),
                    None => Err(EvalError.UnknownVariable(name)),
                }
            },

            Expr.Unary { op: UnaryOp.Neg, operand } => {
                let v = self.eval_expr(*operand)?
                Ok(-v)
            },

            Expr.Binary { op, left, right } => {
                let l = self.eval_expr(*left)?
                let r = self.eval_expr(*right)?
                match op {
                    BinaryOp.Add => Ok(l + r),
                    BinaryOp.Sub => Ok(l - r),
                    BinaryOp.Mul => Ok(l * r),
                    BinaryOp.Div => {
                        if r == 0.0 {
                            return Err(EvalError.DivisionByZero)
                        }
                        Ok(l / r)
                    },
                    BinaryOp.Pow => Ok(pow(l, r)),
                    BinaryOp.Mod => {
                        if r == 0.0 {
                            return Err(EvalError.DivisionByZero)
                        }
                        Ok(l % r)
                    },
                }
            },

            Expr.Call { name, arg } => {
                let v = self.eval_expr(*arg)?
                match name {
                    "sin" => Ok(sin(v)),
                    "cos" => Ok(cos(v)),
                    "tan" => Ok(tan(v)),
                    "sqrt" => {
                        if v < 0.0 {
                            return Err(EvalError.NegativeSqrt)
                        }
                        Ok(sqrt(v))
                    },
                    "log" => {
                        if v <= 0.0 {
                            return Err(EvalError.LogOfNonPositive)
                        }
                        Ok(log(v))
                    },
                    "ln" => {
                        if v <= 0.0 {
                            return Err(EvalError.LogOfNonPositive)
                        }
                        Ok(ln(v))
                    },
                    "exp" => Ok(exp(v)),
                    "abs" => Ok(v.abs()),
                    "floor" => Ok(v.floor()),
                    "ceil" => Ok(v.ceil()),
                    _ => Err(EvalError.UnknownFunction(name)),
                }
            },
        }
    }
}

-- Error types
enum LexError {
    UnexpectedChar(char),
    InvalidNumber(str),
}

enum ParseError {
    UnexpectedToken(Token),
    ExpectedRParen,
    UnexpectedEof,
}

enum EvalError {
    DivisionByZero,
    NegativeSqrt,
    LogOfNonPositive,
    UnknownVariable(str),
    UnknownFunction(str),
}

-- REPL entry point
fn main() with [Console, IO] {
    print("dAImond Scientific Calculator")
    print("Type 'exit' to quit")
    print("")

    let mut eval = Evaluator.new()

    loop {
        print("> ")
        let line = read_line()

        if line == "exit" {
            break
        }

        if line.starts_with("let ") {
            -- Handle variable assignment: "let x = 5"
            let rest = line.slice(4, line.len()).trim()
            match rest.split_once("=") {
                Some((name, expr_str)) => {
                    let name = name.trim()
                    let expr_str = expr_str.trim()
                    match Parser.new(expr_str) {
                        Ok(mut parser) => {
                            match parser.parse() {
                                Ok(expr) => {
                                    match eval.eval(expr) {
                                        Ok(value) => {
                                            eval.set_variable(name, value)
                                            print("{name} = {value}")
                                        },
                                        Err(e) => print("Error: {e}"),
                                    }
                                },
                                Err(e) => print("Parse error: {e}"),
                            }
                        },
                        Err(e) => print("Lex error: {e}"),
                    }
                },
                None => print("Invalid let syntax. Use: let x = <expression>"),
            }
        } else {
            -- Evaluate expression
            match Parser.new(line) {
                Ok(mut parser) => {
                    match parser.parse() {
                        Ok(expr) => {
                            match eval.eval(expr) {
                                Ok(value) => print("{value}"),
                                Err(e) => print("Error: {e}"),
                            }
                        },
                        Err(e) => print("Parse error: {e}"),
                    }
                },
                Err(e) => print("Lex error: {e}"),
            }
        }
    }
}
