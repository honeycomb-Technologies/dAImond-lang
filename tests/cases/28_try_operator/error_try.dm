module error_try

fn parsed(s: string) -> Result[int, string] {
    if s == "" { return Err("empty") }
    return Ok(len(s))
}

fn found(n: int) -> Option[int] {
    if n > 0 { return Some(n) }
    return None
}

fn coded(n: int) -> Result[int, int] {
    return Ok(n)
}

fn plain(n: int) -> int {
    let a = parsed("x")?
    return a + n
}

fn no_value() {
    let b = found(1)?
    println(int_to_string(b))
}

fn mixed(n: int) -> Option[int] {
    let c = parsed("x")?
    return Some(c + n)
}

fn other_error(n: int) -> Result[int, string] {
    let d = coded(n)?
    return Ok(d)
}

fn not_a_result(n: int) -> Result[int, string] {
    let e = n?
    return Ok(e)
}

fn main() {
    println(int_to_string(plain(1)))
    no_value()
}
