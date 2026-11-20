module probe

fn main() {
    let n = comptime 9223372036854775807 + 1
    println(int_to_string(n))
}
