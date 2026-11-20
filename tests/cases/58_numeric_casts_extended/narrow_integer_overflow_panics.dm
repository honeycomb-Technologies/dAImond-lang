module test
fn main() {
    let small: i32 = 2000000000 as i32
    println(int_to_string((small + 5) as int))
    let wide = 250
    let b = wide as u8
    println(int_to_string(b as int))
    println(int_to_string((small + small) as int))
    println("not reached")
}