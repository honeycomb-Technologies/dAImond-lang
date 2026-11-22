module test

enum Color {
    Red,
    Green,
    Blue(int)
}

enum Token {
    Number(int),
    Word(string),
    Pair(int, string),
    End
}

fn shade(c: Color) -> string {
    match c {
        Color.Blue(n) if n > 5 => { return "deep blue" }
        Color.Blue(n) if n < 0 => { return "no blue" }
        Color.Blue(n) => { return "blue" }
        _ => { return "other" }
    }
}

fn describe(t: Token, limit: int) -> string {
    -- The arms are tried in the order they are written. A guard reads
    -- what its pattern binds, and what is in scope around the match.
    return match t {
        Token.Number(n) if n > limit => "big " + int_to_string(n),
        Token.Number(n) if n == limit => "the limit",
        Token.Word(w) if len(w) == 0 => "empty word",
        Token.Pair(n, w) if n > 0 and w == "x" => "pair x",
        Token.Number(n) => "small " + int_to_string(n),
        Token.Word(w) => "word " + w,
        other if limit > 100 => "anything",
        Token.Pair(n, w) => "pair " + w,
        Token.End => "end"
    }
}

fn status(r: Result[int, string]) -> int {
    match r {
        Ok(v) if v > 0 => { return 1 }
        Ok(v) => { return 0 }
        Err(e) if e == "soft" => { return 2 }
        Err(e) => { return 3 }
    }
}

fn sign(o: Option[int]) -> string {
    return match o {
        Some(v) if v > 0 => "positive",
        Some(v) if v < 0 => "negative",
        Some(v) => "zero",
        None => "nothing"
    }
}

fn first_or(o: Option[string], fallback: string) -> string {
    -- A catch-all arm takes whatever the arms before it did not
    return match o {
        Some(s) if len(s) > 0 => s,
        _ => fallback
    }
}

fn present(o: Option[int]) -> int {
    return match o {
        None => 0,
        _ => 1
    }
}

fn main() {
    println(shade(Color.Blue(9)))
    println(shade(Color.Blue(-1)))
    println(shade(Color.Blue(3)))
    println(shade(Color.Red))

    println(describe(Token.Number(9), 5))
    println(describe(Token.Number(5), 5))
    println(describe(Token.Number(2), 5))
    println(describe(Token.Word(""), 5))
    println(describe(Token.Word("hi"), 5))
    println(describe(Token.Pair(1, "x"), 5))
    println(describe(Token.Pair(0, "x"), 5))
    println(describe(Token.Pair(0, "y"), 500))
    println(describe(Token.End, 5))
    println(describe(Token.End, 500))

    println(status(Ok(4)))
    println(status(Ok(-4)))
    println(status(Err("soft")))
    println(status(Err("hard")))

    println(sign(Some(3)))
    println(sign(Some(-3)))
    println(sign(Some(0)))
    println(sign(None))

    println(first_or(Some("a"), "b"))
    println(first_or(Some(""), "b"))
    println(first_or(None, "c"))
    println(present(Some(7)))
    println(present(None))
}
