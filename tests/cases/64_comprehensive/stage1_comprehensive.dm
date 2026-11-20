module test

fn double(x: int) -> int {
    return x * 2
}

fn safe_div(a: int, b: int) -> Result[int, string] {
    if b == 0 { return Err("division by zero") }
    return Ok(a / b)
}

fn compute(a: int, b: int) -> Result[int, string] {
    let result: int = safe_div(a, b)?
    return Ok(result * 2)
}

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() {
    -- Match on Result
    let r = compute(10, 2)
    match r {
        Ok(v) => println(int_to_string(v))
        _ => println("error")
    }

    -- Match on Option
    let opt: Option[int] = Some(42)
    match opt {
        Some(v) => println(int_to_string(v))
        _ => println("none")
    }

    -- List method calls
    let mut items: List[int] = []
    items.push(10)
    items.push(20)
    println(int_to_string(items.len()))

    -- Higher-order with named function
    println(int_to_string(apply(double, 21)))

    -- String utils
    let n = parse_int("42")
    println(int_to_string(n))

    let trimmed = string_trim("  hello  ")
    println(trimmed)
}