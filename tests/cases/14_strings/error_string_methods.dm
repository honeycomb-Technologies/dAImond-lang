module test
fn main() {
    let s = "abc"
    println(s.upper())
    println(s.trim(1))
    println(int_to_string(s.find(3)))
    let n: int = s.to_upper()
}
