module test

fn main() {
    let mut count = 0
    loop {
        if count >= 5 {
            break
        }
        count = count + 1
    }
    println(count)
}