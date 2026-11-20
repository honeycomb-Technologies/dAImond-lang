module test

fn might_fail(x: int) -> Result[int, string] {
    if x < 0 {
        return Err("negative")
    }
    return Ok(x * 2)
}

fn compute(x: int) -> Result[int, string] {
    let val: int = might_fail(x)?
    return Ok(val + 1)
}

fn main() {
    let r = compute(-3)
    if r.is_ok {
        println("should not print")
    } else {
        println(r.err)
    }
}