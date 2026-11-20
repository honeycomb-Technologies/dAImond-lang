module test_compound_assign

-- Test compound assignment operators (Phase 1.3)

fn main() {
    let mut x = 10
    x += 5
    println("plus_eq=" + int_to_string(x))

    x -= 3
    println("minus_eq=" + int_to_string(x))

    x *= 4
    println("star_eq=" + int_to_string(x))

    x /= 6
    println("slash_eq=" + int_to_string(x))
}
