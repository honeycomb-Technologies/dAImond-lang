module error_comptime_recursion

fn deeper(n: int) -> int {
    return deeper(n + 1)
}

const N = comptime deeper(0)

fn main() {
    println(int_to_string(N))
}
