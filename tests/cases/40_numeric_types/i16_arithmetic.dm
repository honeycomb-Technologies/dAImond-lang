module test

fn main() {
    let a: i16 = 100
    let b: i16 = 200
    let sum: i16 = (a + b) as i16
    let result: i64 = sum as i64
    println(int_to_string(result))
}