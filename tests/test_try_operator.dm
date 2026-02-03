module test_try_operator

-- Test error propagation operator ? (Phase 2.2)

fn parse_number(s: string) -> Result[int, string] {
    if s == "42" {
        return Ok(42)
    }
    return Err("not 42")
}

fn try_add(a: string, b: string) -> Result[int, string] {
    let x: Result[int, string] = parse_number(a)
    let ax = x?
    let y: Result[int, string] = parse_number(b)
    let by = y?
    return Ok(ax + by)
}

fn main() {
    let r1: Result[int, string] = try_add("42", "42")
    match r1 {
        Ok(v) => println("try_ok=" + int_to_string(v)),
        Err(e) => println("try_err=" + e)
    }

    let r2: Result[int, string] = try_add("42", "bad")
    match r2 {
        Ok(v) => println("try_ok2=" + int_to_string(v)),
        Err(e) => println("try_err2=" + e)
    }
}
