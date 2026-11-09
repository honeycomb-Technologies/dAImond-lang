module o8

fn main() {
    let n = comptime 0 - 9223372036854775807 - 2
    println(int_to_string(n))
}
