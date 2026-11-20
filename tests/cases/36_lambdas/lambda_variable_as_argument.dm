module main

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() -> int {
    let add_one = |x: int| -> int { return x + 1 }
    println(int_to_string(apply(add_one, 41)))
    let double = |x: int| -> int x * 2
    println(int_to_string(apply(double, 21)))
    println(int_to_string(add_one(double(4))))
    return 0
}
