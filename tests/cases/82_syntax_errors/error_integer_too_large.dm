module error_integer_too_large

-- An integer that is no int was read as the highest int

fn main() {
    let x = 9223372036854775808
    println(int_to_string(x))
}
