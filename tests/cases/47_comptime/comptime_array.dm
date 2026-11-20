module test

const SECOND = comptime { let arr = [10, 20, 30]; arr[1] }

fn main() {
    println(int_to_string(SECOND))
}