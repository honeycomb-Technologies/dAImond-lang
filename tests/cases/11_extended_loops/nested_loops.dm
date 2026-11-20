module test

fn main() {
    let mut total = 0
    for i in 0..3 {
        for j in 0..3 {
            total = total + 1
        }
    }
    println(total)
}