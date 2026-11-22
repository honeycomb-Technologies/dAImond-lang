module main

struct P { name: string, n: i32, f: float }

fn main() {
    let mut s = "a"
    s += "b"
    println(s)
    let mut p = P { name: "x", n: 1 as i32, f: 1.5 }
    p.name += "y"
    p.n += 2
    p.f *= 2.0
    println(p.name + " " + int_to_string(p.n as int) + " " + float_to_string(p.f))
    let mut u: u8 = 250 as u8
    u += 5
    println(int_to_string(u as int))
    let mut k: i8 = 100 as i8
    k -= 3
    println(int_to_string(k as int))
    let mut q = 17
    q %= 5
    println(int_to_string(q))
    q /= 2
    println(int_to_string(q))
    let small: i32 = 7 as i32
    let mut w = 100
    w += small
    println(int_to_string(w))
}
