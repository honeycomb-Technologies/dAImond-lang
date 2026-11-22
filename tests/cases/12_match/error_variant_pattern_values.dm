module test

enum Token {
    Number(int),
    Word(string),
    Flag(bool)
}

fn describe(t: Token) -> string {
    return match t {
        Token.Number("one") => "one",
        Token.Number(n) => "number",
        Token.Word(w) => "word",
        Token.Flag(true) => "on",
        Token.Flag(false) => "off"
    }
}

fn main() {
    println(describe(Token.Number(1)))
}
