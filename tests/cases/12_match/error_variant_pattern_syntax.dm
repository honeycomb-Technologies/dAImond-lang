module test

enum Token {
    Number(int),
    Scale(float)
}

fn describe(t: Token) -> string {
    return match t {
        Token.Scale(1.5) => "half more",
        Token.Scale(f) => "scale",
        Token.Number(n) => "number"
    }
}

fn main() {
    println(describe(Token.Number(1)))
}
