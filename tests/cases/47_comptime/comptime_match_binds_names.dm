module test

fn kind(n: int) -> int {
    -- An arm that is a name binds the matched value, at compile time
    -- as in the program
    return match n {
        0 => 100,
        x if x > 10 => 200,
        x if x < 0 => 300,
        other => 400 + other
    }
}

const A = comptime { kind(0) }
const B = comptime { kind(50) }
const C = comptime { kind(-5) }
const D = comptime { kind(5) }

fn main() {
    println(f"{A} {B} {C} {D}")
    println(f"{kind(0)} {kind(50)} {kind(-5)} {kind(5)}")
}
