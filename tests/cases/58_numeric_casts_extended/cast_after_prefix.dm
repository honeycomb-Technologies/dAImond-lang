module test
-- `as` binds tighter than any binary operator and looser than a prefix
-- operator: `-n as i32` casts -n, `*b as float` casts what is in the
-- box, and `a * i as float` casts i
fn main() {
    let n = -3
    println(int_to_string(-n as i32 as int))
    let b = Box_new(7)
    println(float_to_string(*b as float + 0.5))
    let i = 4
    println(float_to_string(1.5 * i as float))
    let x = 200
    println(int_to_string(-x as i16 as int))
    println(int_to_string(2 + 3 as int * 4))
    println(bool_to_string(-1 as int < 0))
    let small: u8 = 255
    println(int_to_string(small as int + 1))
    let f = 2.75
    println(int_to_string(-f as int))
    println(bool_to_string(not true as bool))
}
