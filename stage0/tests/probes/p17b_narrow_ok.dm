module probe

fn main() {
    let a: int = 42
    let b = a as i32
    println(int_to_string(b as int))
    let f = a as float
    println(int_to_string(f as int))
    let g: float = 3.7
    let h = g as int
    println(int_to_string(h))
}
