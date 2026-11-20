module test

struct Token {
    kind: int,
    value: string,
    line: int,
}

fn main() {
    let t = Token { kind: 1, value: "hello", line: 5 }
    println(t.kind)
    println(t.value)
    println(t.line)
}