module test

fn parse_positive(x: int) -> Result[int, string] {
    if x <= 0 {
        return Err("not positive")
    }
    return Ok(x * 10)
}

fn main() {
    let r = parse_positive(5)
    if r.is_ok {
        println(int_to_string(r.ok))
    }
}