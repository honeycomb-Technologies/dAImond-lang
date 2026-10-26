module o1

fn main() {
    let mut a = 9223372036854775807 - 1
    a = a + 1
    println(int_to_string(a))
    let mut x: i32 = 2000000000
    let y: i32 = 100000000
    println(int_to_string(x + y))
    let seven: i32 = 7
    x += seven
    println(int_to_string(x))
    let mut u: uint = 9223372036854775807
    let one: uint = 1
    u = u + u + one
    u = u + one
    println(int_to_string(u as int))
}
