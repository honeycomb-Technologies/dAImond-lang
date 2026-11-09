module probe

extern fn puts(s: string) -> int

fn main() {
    unsafe {
        let r = puts("x")
    }
}
