module test

fn safe_div(a: int, b: int) -> Result[int, string] {
    if b == 0 {
        return Err("division by zero")
    }
    return Ok(a / b)
}

fn main() {
    let r = safe_div(10, 2)
    match r {
        Ok(v) => println(int_to_string(v))
        _ => println("error")
    }
}