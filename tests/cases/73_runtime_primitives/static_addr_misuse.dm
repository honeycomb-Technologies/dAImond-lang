module test
fn size() -> int { return 8 }
fn main() {
    let a = static_addr("a", 8)
    unsafe {
        let b = static_addr(int_to_string(1), 8)
        let c = static_addr("c", size())
        let d = static_addr("d", 0)
        let e = static_addr("e")
    }
}