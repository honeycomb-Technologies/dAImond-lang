module test

fn main() -> int {
    let mut n = 1; n = n + 1; println(int_to_string(n))
    if n == 2 { n = n * 10; println(int_to_string(n)) }
    return 0
}
