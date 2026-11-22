module error_comptime_value

const XS = comptime [1, 2]

fn main() {
    println("unreachable")
}
