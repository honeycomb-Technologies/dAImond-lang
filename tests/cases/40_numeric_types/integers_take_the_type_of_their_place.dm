module integers_take_the_type_of_their_place

-- An integer that goes to a place of another integer type becomes
-- that type: a constant that fits and a narrower value convert, in a
-- typed let, an argument, a field, a variant's value, Some, Ok and a
-- list's element. A wider value needs `as`
-- (error_wider_integer_needs_as).

struct S { n: i8, m: i32 }
enum E { A(i8), B }

fn take(v: i8) -> int { return v as int }
fn code(ok: bool) -> i32 {
    let c: i32 = if ok { 0 } else { 1 }
    return c
}
fn res(a: i32) -> Result[i32, string] { return Ok(a) }
fn through(a: i32) -> Result[i32, string] {
    let r: i32 = res(a)?
    return Ok(r)
}
fn pick[T](t: T, ok: bool) -> int {
    let c: i8 = if ok { 3 } else { 4 }
    let d: i16 = match ok { true => 10, false => 20 }
    return (c as int) + (d as int)
}

fn main() -> int {
    let x: i32 = -7
    let small: i8 = 5
    let u: u32 = 4000000000

    -- constants take the type of their place
    let a: i8 = 100
    let b: i8 = -128
    let c: u8 = 255
    let d: i16 = 100 * 300
    let e: u32 = 4294967295
    println((a as int) + (b as int) + (c as int) + (d as int))
    println(e as int)

    -- an `if` or a `match` of constants does too
    let f: i8 = if x < 0 { 1 } else { 2 }
    let g: i16 = match small { 5 => 20, _ => 30 }
    let h: i32 = if x > 0 { 1 } else { (if x < -5 { 2 } else { 3 }) }
    let k: i8 = match res(x) { Ok(v) => 1, Err(m) => panic(m) }
    println((f as int) + (g as int) + (h as int) + (k as int))
    println((code(true) as int) + (code(false) as int))
    println(pick("t", true))

    -- narrower values widen
    let w1: int = x
    let w2: i32 = small
    let w3: i64 = small
    let w4: u64 = u
    let w5: int = u
    println(w1 + (w2 as int) + w3 + (w4 as int) + w5)

    -- `as` narrows
    let n1: i8 = (x + 10) as i8
    let n2: i32 = (w1 * 2) as i32
    println((n1 as int) + (n2 as int))

    -- arguments, fields, variant values, Some and Ok
    println(take(5) + take(small) + take(-3))
    let s = S { n: 7, m: small }
    let s2 = S { n: small, m: x }
    println((s.n as int) + (s.m as int) + (s2.n as int) + (s2.m as int))
    let en = E.A(9)
    match en {
        E.A(v) => println(v as int),
        E.B => println("b")
    }
    let o: Option[i8] = Some(11)
    match o {
        Some(v) => println(v as int),
        None => println("none")
    }
    match through(x) {
        Ok(v) => println(v as int),
        Err(m) => println(m)
    }

    -- lists of narrow integers
    let xs: List[i32] = [1, 2, x]
    let mut ys: List[i8] = []
    ys.push(7)
    ys.push(small)
    ys.push(-1)
    println((xs[0] as int) + (xs[2] as int) + xs.len())
    println((ys[0] as int) + (ys[1] as int) + (ys[2] as int))
    let mut zs: List[int] = [x, small]
    zs.push(u)
    zs.push(small)
    println(zs[0] + zs[1] + zs[2] + zs[3])
    let mut m: Map[string, i32] = Map_new()
    m.insert("a", 5)
    m.insert("b", small)
    println((m.get("a") as int) + (m.get("b") as int))

    -- a constant known at compile time
    let ct: i8 = comptime 3
    println(ct as int)
    return 0
}
