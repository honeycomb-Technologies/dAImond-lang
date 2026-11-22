module test

-- A constant's value is found at compile time, with `comptime` or
-- without it
const A: int = 17 % 5
const B = 7 - 2 * 3
const C = (1 + 2) * 3
const D: bool = 3 < 4
const E = "a" + "b"
const F = A + C
const G = 10 / 4
const H = sq(6)
const I = -A

fn sq(x: int) -> int {
    return x * x
}

fn main() {
    println(A)
    println(B)
    println(C)
    println(D)
    println(E)
    println(F)
    println(G)
    println(H)
    println(I)
}
