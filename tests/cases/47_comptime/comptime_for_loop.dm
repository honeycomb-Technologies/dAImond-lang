module test

const X = comptime {
    let arr = [1, 2, 3, 4, 5]
    let mut sum = 0
    for v in arr {
        sum = sum + v
    }
    sum
}

fn main() {
    println(int_to_string(X))
}