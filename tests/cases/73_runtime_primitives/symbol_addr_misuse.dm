module test
fn name() -> string { return "stdout" }
fn main() {
    let a = symbol_addr("stdout")
    unsafe {
        let b = symbol_addr(name())
        let c = symbol_addr("")
        let d = symbol_addr("stdout", "stderr")
    }
}