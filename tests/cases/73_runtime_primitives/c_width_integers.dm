module test
fn half(x: u32) -> u32 {
    return x / 2
}
fn code(ok: bool) -> i32 {
    if ok {
        return 0
    }
    return 0 - 1
}
fn main() {
    let big: u32 = 4000000000 as u32
    let small: u32 = 5 as u32
    println(bool_to_string(big > small) + " " + bool_to_string(big < 3000000000) + " " + bool_to_string(small >= 5))
    println(int_to_string(half(big) as int) + " " + int_to_string((big % 7) as int))
    let c = code(false)
    println(int_to_string(c as int) + " " + bool_to_string(c < 0) + " " + bool_to_string(code(true) == 0))
    let b: u8 = 250 as u8
    println(bool_to_string(b > 200) + " " + int_to_string((b + 5) as int))
    let w = c + 10
    println(int_to_string(w as int))
    let mix = c * 3 + big as int
    println(int_to_string(mix))
}