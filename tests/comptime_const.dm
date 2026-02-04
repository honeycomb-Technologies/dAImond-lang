module comptime_const

const SIZE = comptime 4 * 1024
const FLAG = comptime true and false
const RATIO = comptime 3.14 * 2.0

fn main() {
    println(int_to_string(SIZE))
    println(bool_to_string(FLAG))
    println(float_to_string(RATIO))
}
