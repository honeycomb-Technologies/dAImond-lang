module test

const X = comptime {
    let mut i = 0
    while true {
        if i == 5 { break }
        i = i + 1
    }
    i
}

fn main() {
    println(int_to_string(X))
}