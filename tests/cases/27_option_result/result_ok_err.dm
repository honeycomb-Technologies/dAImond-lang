module test

fn safe_div(a: int, b: int) -> Result[int, string] {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

fn main() {
    let r1 = safe_div(10, 2)
    match r1 {
        Ok(v) => println(int_to_string(v))
        _ => println("error")
    }
    let r2 = safe_div(10, 0)
    match r2 {
        Err(e) => println(e)
        _ => println("should not print")
    }
}