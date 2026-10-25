module probe

fn main() {
    let mut a = 9223372036854775807 - 1
    a = a + 1
    println(int_to_string(a + 1))
}
