module test

const FLAGS = comptime { (1 << 4) | 3 }
const LOW: int = 1000 & 255
const TOP = comptime { -8 >> 1 }
const INVERTED = comptime { ~5 }
const MIXED = comptime { 6 ^ 3 }

fn ones(n: int) -> int {
    let mut count = 0
    let mut x = n
    while x != 0 {
        count = count + (x & 1)
        x = x >> 1
    }
    return count
}

fn main() {
    let a = 12
    let b = 10
    println(a & b)
    println(a | b)
    println(a ^ b)
    println(a << 2)
    println(a >> 2)
    println(~a)
    println(-16 >> 2)

    -- The shifts bind tighter than &, & tighter than ^, ^ tighter than
    -- |, and all of them tighter than a comparison and looser than +
    println(1 | 2 ^ 3 & 4)
    println(1 + 1 << 2)
    println(1 << 2 + 1)
    println(a & 4 == 4)
    println(a & b == 8 and a | b == 14)

    -- An unsigned integer shifts in zeros, a signed one its sign
    let u: u8 = 200
    println(u >> 3)
    println(u & 15)
    println(~u)
    let s: i8 = -128
    println(s >> 7)
    let w: u32 = 4294967295
    println(w >> 31)
    println(w << 4)
    let n: i32 = 1
    println(n << 31)

    -- Two bools
    let t = true
    let f = false
    println(t & f)
    println(t | f)
    println(t ^ t)

    let mut h = 0
    let mut i = 0
    while i < 4 {
        h = (h << 5) ^ (h >> 2) ^ i
        i = i + 1
    }
    println(h)
    println(ones(255))
    println(ones(h))

    println(FLAGS)
    println(LOW)
    println(TOP)
    println(INVERTED)
    println(MIXED)
}
