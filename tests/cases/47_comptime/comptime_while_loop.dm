module test

const X = comptime {
    let mut i = 0
    let mut sum = 0
    while i < 10 {
        sum = sum + i
        i = i + 1
    }
    sum
}

fn main() {
    println(int_to_string(X))
}