module test

fn safe_div(a: int, b: int) -> Result[int, string] {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

fn main() {
    let r = safe_div(10, 0)
    match r {
        Err(e) => println(e)
        _ => println("ok")
    }
}