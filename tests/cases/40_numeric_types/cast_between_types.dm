module test

fn main() {
    let x: int = 1000
    let narrow: i16 = x as i16
    let wide: i64 = narrow as i64
    println(int_to_string(wide))
}