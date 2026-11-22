module main

fn exit(code: int) -> int {
    return code + 1
}

fn len(n: int) -> int {
    return n * 2
}

extern fn system(command: string) -> i32

fn Some(n: int) -> int {
    return n
}

fn main() {
    println(int_to_string(len(21)))
}
