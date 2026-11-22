module error_comptime_index

const X = comptime {
    let xs = [1, 2, 3]
    xs[3]
}

fn main() {
    println(int_to_string(X))
}
