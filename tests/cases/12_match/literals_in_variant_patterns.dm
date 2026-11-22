module test

enum Token {
    Number(int),
    Word(string),
    Pair(int, string),
    Flag(bool),
    End
}

fn describe(t: Token) -> string {
    -- A literal among a variant's values must equal the value
    return match t {
        Token.Number(0) => "zero",
        Token.Number(-1) => "minus one",
        Token.Number(n) if n > 100 => "big",
        Token.Number(n) => "number " + int_to_string(n),
        Token.Word("if") => "keyword",
        Token.Word(w) => "word " + w,
        Token.Pair(1, "x") => "one x",
        Token.Pair(1, s) => "one " + s,
        Token.Pair(_, "y") => "any y",
        Token.Pair(n, s) if n == 7 => "seven",
        Token.Pair(n, s) => "pair",
        Token.Flag(true) => "on",
        Token.Flag(_) => "off",
        Token.End => "end"
    }
}

fn level(o: Option[int]) -> string {
    match o {
        Some(0) => { return "none" }
        Some(1) => { return "one" }
        Some(n) if n < 0 => { return "negative" }
        Some(_) => { return "many" }
        None => { return "nothing" }
    }
}

fn code(r: Result[int, string]) -> int {
    return match r {
        Ok(0) => 0,
        Ok(n) => n * 2,
        Err("soft") => -1,
        Err(_) => -2
    }
}

fn sign(n: int) -> string {
    return match n {
        -1 => "minus one",
        0 => "zero",
        _ => "other"
    }
}

fn main() {
    println(describe(Token.Number(0)))
    println(describe(Token.Number(-1)))
    println(describe(Token.Number(500)))
    println(describe(Token.Number(5)))
    println(describe(Token.Word("if")))
    println(describe(Token.Word("iff")))
    println(describe(Token.Pair(1, "x")))
    println(describe(Token.Pair(1, "z")))
    println(describe(Token.Pair(2, "y")))
    println(describe(Token.Pair(7, "q")))
    println(describe(Token.Pair(2, "x")))
    println(describe(Token.Flag(true)))
    println(describe(Token.Flag(false)))
    println(describe(Token.End))

    println(level(Some(0)))
    println(level(Some(1)))
    println(level(Some(-4)))
    println(level(Some(9)))
    println(level(None))

    println(code(Ok(0)))
    println(code(Ok(21)))
    println(code(Err("soft")))
    println(code(Err("hard")))

    println(sign(-1))
    println(sign(0))
    println(sign(1))
}
