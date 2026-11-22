module main

extern fn abs(x: i32) -> i32
extern fn toupper(c: i32) -> i32
extern fn c_abs8(x: i8) -> i32 = "abs"
extern fn c_abs16(x: i16) -> i32 = "abs"
extern fn c_absu8(x: u8) -> i32 = "abs"

fn main() {
    let a: i8 = -5 as i8
    let b: i16 = -300 as i16
    let c: u8 = 200 as u8
    unsafe {
        println(int_to_string(c_abs8(a) as int))
        println(int_to_string(c_abs16(b) as int))
        println(int_to_string(c_absu8(c) as int))
    }
}
