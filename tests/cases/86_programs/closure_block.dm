module closure_block

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() {
    let add_one = |x: int| -> int { return x + 1 }
    let result = apply(add_one, 41)
    println(int_to_string(result))
}
