module test
fn main() {
    let x = 5
    let a = addr_of(x)
    unsafe {
        let b = addr_of(x + 1)
        let c = load_i32("str")
        store_i64(b, "no")
        let d = size_of[Nope]()
        let e = load_u8(1, 2)
        let f = string_data(3)
    }
}