module test
extern fn puts(s: string) -> i32
fn helper() -> int { return 1 }
fn main() {
    unsafe {
        let e = symbol_addr("puts")
        let f = symbol_addr("helper")
    }
}