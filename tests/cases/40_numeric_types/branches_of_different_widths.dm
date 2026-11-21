module branches_of_different_widths

-- The branches of an `if` and the arms of a `match` may give integers
-- of different widths. They meet at one type, as the operands of an
-- operator do: a literal takes the type of the others, otherwise the
-- narrower ones widen. Each value is converted where it is stored.

enum E { A(i32), B }

fn opt(n: i32) -> Option[i32] {
    if n != 0 { return Some(n) }
    return None
}
fn res(n: i32) -> Result[i32, string] {
    if n != 0 { return Ok(n) }
    return Err("zero")
}
fn show(v: i32) { println(v as int) }
fn wide(v: int) { println(v) }
-- leaves values on the stack that are not zero
fn dirty() -> int {
    let a = 9223372036854775807
    let b = a - 1
    let d = b - 1
    return d / 2 + a / 3
}

-- In the body of a generic function the checker records no types
fn pick[T](t: T, x: i32, big: int, c: bool) -> int {
    let a = if c { x } else { 0 }
    let b = match opt(x) { Some(v) => v, None => 0 }
    let d = if c { big } else { x }
    return a + b + d
}

fn main() -> int {
    let x: i32 = -7
    let zero: i32 = 0
    let big: int = 5000000000
    let yes = big > 0
    let no = big < 0
    let w = dirty()

    -- Option and Result: the literal takes the other arm's type
    let a = match opt(x) { Some(v) => v, None => 0 }
    println(a)
    show(a)
    let a2 = match opt(zero) { Some(v) => v, None => 5 }
    show(a2)
    let a3 = match opt(x) { None => 0, Some(v) => v }
    show(a3)
    let r = match res(x) { Ok(v) => v, Err(e) => 0 }
    show(r)
    let r2 = match res(zero) { Ok(v) => v, Err(e) => -1 }
    show(r2)

    -- a wider arm widens the others
    let m = match opt(x) { Some(v) => v, None => big }
    wide(m)
    let m2 = match opt(zero) { Some(v) => v, None => big }
    wide(m2)
    let m3 = match opt(x) { Some(v) => big, None => 0 }
    wide(m3)

    -- if
    let i1 = if yes { x } else { 0 }
    show(i1)
    let i2 = if no { 0 } else { x }
    show(i2)
    let i3 = if yes { x } else { big }
    wide(i3)
    let i4 = if no { x } else { big }
    wide(i4)
    let i5 = if no { big } else { x }
    wide(i5)

    -- match on a number and on an enum
    let l1 = match big { 0 => 3, _ => x }
    show(l1)
    let l2 = match big { 0 => x, _ => big }
    wide(l2)
    let l3 = match zero { 0 => x, n => big }
    wide(l3)
    let e = E.A(x)
    let e1 = match e { E.A(v) => v, E.B => 0 }
    show(e1)
    let e2 = match e { E.B => big, E.A(v) => v }
    wide(e2)

    -- an arm that is a block
    let b1 = match opt(x) {
        Some(v) => {
            let q = v
            q
        },
        None => 0
    }
    show(b1)
    let b2 = match opt(x) {
        Some(v) => {
            let q = v
            q
        },
        None => { big }
    }
    wide(b2)

    -- arithmetic in the arms, and the value used at once
    let c1 = match opt(x) { Some(v) => v + 1, None => -1 }
    show(c1)
    show(match opt(x) { Some(v) => v, None => 0 })
    wide(match opt(x) { Some(v) => v, None => big })
    wide((if yes { x } else { 0 }) + big)

    -- unsigned
    let u: u8 = 200
    let ou: Option[u8] = Some(u)
    let u1 = match ou { Some(v) => v, None => 0 }
    println(u1 as int)
    let u2 = if yes { u } else { big }
    wide(u2)

    -- a typed destination
    let t1: i32 = match opt(x) { Some(v) => v, None => 0 }
    show(t1)
    let t2: int = match opt(x) { Some(v) => v, None => 0 }
    wide(t2)
    let t3: int = if yes { x } else { 0 }
    wide(t3)

    -- nested
    let n1 = if yes { x } else { (if no { 1 } else { 2 }) }
    wide(n1)
    let n2 = if no { x } else { (match opt(x) { Some(v) => v, None => 0 }) }
    show(n2)

    -- a generic function
    wide(pick(1, x, big, yes))
    wide(pick(1, x, big, no))

    -- a match whose value is not used, and assignments
    match x { 0 => x, _ => big }
    let mut t = 0
    t = if yes { x } else { 0 }
    t += if yes { x } else { 0 }
    wide(t)
    let mut q: i32 = 0
    q = if yes { x } else { 0 }
    show(q)

    -- floats
    let f: f32 = 2.5 as f32
    let f1 = if yes { f } else { 0.5 }
    println(f1 as float)
    let f2 = if no { 1.5 } else { f }
    println(f2)
    let of: Option[f32] = Some(f)
    let f3 = match of { Some(v) => v, None => 0.0 }
    println(f3 as float)
    println(w > 0)
    return 0
}
