module test
fn first() -> int {
    unsafe {
        return static_addr("shared", 8)
    }
}
fn main() {
    unsafe {
        println(int_to_string(first() - static_addr("shared", 16)))
    }
}