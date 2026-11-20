module test

fn main() {
    let parts = string_split("a,b,c", ",")
    println(int_to_string(parts.len()))
    println(parts[0])
    println(parts[1])
    println(parts[2])
}