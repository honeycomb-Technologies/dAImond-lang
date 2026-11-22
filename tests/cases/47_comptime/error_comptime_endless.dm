module error_comptime_endless

fn spin() -> int {
    let mut n = 0
    while true {
        n = 1 - n
    }
    return n
}

const N = comptime spin()

fn main() {
    println(int_to_string(N))
}
