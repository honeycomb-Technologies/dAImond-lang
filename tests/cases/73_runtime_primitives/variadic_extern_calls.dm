module test
extern fn malloc(n: int) -> int
extern fn free(p: int)
extern fn snprintf(buf: int, n: int, fmt: string, ...) -> i32
extern fn open(path: string, flags: i32, ...) -> i32
extern fn close(fd: i32) -> i32
fn main() {
    unsafe {
        let buf = malloc(128)
        let word = "wor" + "ld"
        let small: i16 = 0 - 3 as i16
        let f: f32 = 0.5 as f32
        let n = snprintf(buf, 128, "%d %s %.3f %ld %d %d %.2f", 42 as i32, word, 3.14159, 9000000000, small, true, f)
        println(string_from_raw(buf, n as int, 0) + " (" + int_to_string(n as int) + ")")
        let fd = open("/dev/null", 1, 420)
        println(bool_to_string(fd >= 0) + " " + int_to_string(close(fd) as int))
        free(buf)
    }
}