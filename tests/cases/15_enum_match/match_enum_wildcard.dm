module test

enum Result {
    Ok(int),
    Err(int)
}

fn is_ok(r: Result) -> int {
    match r {
        Result::Ok(v) => { return v }
        _ => { return -1 }
    }
    return -2
}

fn main() {
    let ok = Result::Ok(42)
    let err = Result::Err(99)
    println(is_ok(ok))
    println(is_ok(err))
}