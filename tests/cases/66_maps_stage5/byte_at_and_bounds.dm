module test
fn main() {
    let s = "Az~"
    println(int_to_string(byte_at(s, 0)) + " " + int_to_string(byte_at(s, 1)) + " " + int_to_string(byte_at(s, 2)))
    println(int_to_string(byte_at(s, 3)))
}