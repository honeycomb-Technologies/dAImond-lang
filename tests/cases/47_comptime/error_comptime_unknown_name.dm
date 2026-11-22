module error_comptime_unknown_name

fn main() {
    let x = 5
    let y = comptime x + 1
    println(int_to_string(y))
}
