module try_other_value_type

-- `value?` on a Result leaves the function with the Err it found. The
-- function may return a Result with another type of value: only the
-- type of the error is the same.

struct Inner {
    v: int,
    name: string
}

struct Outer {
    id: int,
    inner: Inner,
    items: List[int]
}

enum Problem {
    Negative(int),
    TooBig
}

fn outer(v: int) -> Result[Outer, string] {
    if v < 0 { return Err("negative: " + int_to_string(v)) }
    return Ok(Outer { id: v, inner: Inner { v: v * 10, name: "in" }, items: [v, v] })
}

fn inner(v: int) -> Result[Inner, string] {
    let o = outer(v)?
    return Ok(Inner { v: o.inner.v + 1, name: o.inner.name + int_to_string(o.items.len()) })
}

fn text(v: int) -> Result[string, string] {
    let i = inner(v)?
    return Ok(i.name + "=" + int_to_string(i.v))
}

fn count(v: int) -> Result[int, string] {
    return Ok(len(text(v)?) + inner(v + 1)?.v)
}

fn nothing(v: int) -> Result[bool, string] {
    let o = outer(v)?
    let p = outer(o.id - 5)?
    return Ok(p.id == 0)
}

fn sized(v: int) -> Result[int, Problem] {
    if v < 0 { return Err(Problem.Negative(v)) }
    if v > 100 { return Err(Problem.TooBig) }
    return Ok(v)
}

fn listed(v: int) -> Result[List[int], Problem] {
    let n = sized(v)?
    let m = sized(n * 20)?
    return Ok([n, m])
}

fn half(v: float) -> Result[float, int] {
    if v < 0.0 { return Err(7) }
    return Ok(v / 2.0)
}

fn rounded(v: float) -> Result[string, int] {
    let h = half(v)?
    return Ok(float_to_string(h))
}

fn main() {
    match inner(2) {
        Ok(i) => println(i.name + " " + int_to_string(i.v)),
        Err(e) => println(e)
    }
    match inner(-2) {
        Ok(i) => println(i.name),
        Err(e) => println(e)
    }
    match text(3) {
        Ok(s) => println(s),
        Err(e) => println(e)
    }
    match text(-3) {
        Ok(s) => println(s),
        Err(e) => println(e)
    }
    match count(4) {
        Ok(n) => println(int_to_string(n)),
        Err(e) => println(e)
    }
    match count(-1) {
        Ok(n) => println(int_to_string(n)),
        Err(e) => println(e)
    }
    match nothing(5) {
        Ok(b) => println(bool_to_string(b)),
        Err(e) => println(e)
    }
    match nothing(4) {
        Ok(b) => println(bool_to_string(b)),
        Err(e) => println(e)
    }
    match listed(3) {
        Ok(xs) => println(int_to_string(xs[0]) + " " + int_to_string(xs[1])),
        Err(p) => println("problem")
    }
    match listed(9) {
        Ok(xs) => println(int_to_string(xs[0])),
        Err(p) => {
            match p {
                Problem.Negative(n) => println("negative " + int_to_string(n)),
                Problem.TooBig => println("too big")
            }
        }
    }
    match listed(-9) {
        Ok(xs) => println(int_to_string(xs[0])),
        Err(p) => {
            match p {
                Problem.Negative(n) => println("negative " + int_to_string(n)),
                Problem.TooBig => println("too big")
            }
        }
    }
    match rounded(5.0) {
        Ok(s) => println(s),
        Err(code) => println(int_to_string(code))
    }
    match rounded(-5.0) {
        Ok(s) => println(s),
        Err(code) => println(int_to_string(code))
    }
}
