module test
extern fn fputs(s: string, stream: int) -> i32
extern fn fflush(stream: int) -> i32

fn main() {
    unsafe {
        -- stdout is a variable of the C library that holds a FILE*
        let out = load_i64(symbol_addr("stdout"))
        fputs("through stdout\n", out)
        fflush(out)
        if symbol_addr("stdout") == symbol_addr("stdout") and symbol_addr("stdout") != symbol_addr("stderr") {
            println("one address per symbol")
        }
        -- The compiler's own symbol for main's argument count
        println(int_to_string(load_i32(symbol_addr("dm_argc"))))
    }
}