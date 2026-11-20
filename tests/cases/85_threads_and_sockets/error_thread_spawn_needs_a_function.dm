module main

fn takes_one(n: int) {
    println(int_to_string(n))
}

fn nothing() {
}

fn main() -> int {
    let a = thread_spawn(takes_one)
    let b = thread_spawn(nothing, 2)
    let number = 7
    let c = thread_spawn(number)
    let d = thread_spawn(puts)
    return 0
}

extern fn puts(s: string) -> i32
