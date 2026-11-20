module test
extern fn malloc(n: int) -> int
extern fn free(p: int)
struct Pt {
    x: int,
    y: i32,
    name: string
}
fn main() {
    unsafe {
        let p = malloc(64)
        store_u8(p, 300)
        store_i8(p + 1, 0 - 2)
        store_u16(p + 2, 65535)
        store_i32(p + 4, 0 - 5)
        store_i64(p + 8, 1234567890123)
        store_f64(p + 16, 2.5)
        store_f32(p + 24, 0.25)
        println(int_to_string(load_u8(p)) + " " + int_to_string(load_i8(p + 1)) + " " + int_to_string(load_u8(p + 1)))
        println(int_to_string(load_u16(p + 2)) + " " + int_to_string(load_i16(p + 2)))
        println(int_to_string(load_i32(p + 4)) + " " + int_to_string(load_u32(p + 4)) + " " + int_to_string(load_i64(p + 8)))
        println(float_to_string(load_f64(p + 16)) + " " + float_to_string(load_f32(p + 24)))
        let mut pt = Pt { x: 7, y: 9, name: "pt" }
        store_i64(addr_of(pt.x), 70)
        let ay = addr_of(pt.y)
        store_i32(ay, 90)
        println(int_to_string(pt.x) + " " + int_to_string(pt.y as int) + " " + int_to_string(ay - addr_of(pt)))
        let mut n = 5
        store_i64(addr_of(n), 6)
        println(int_to_string(n))
        println(int_to_string(size_of[int]()) + " " + int_to_string(size_of[u8]()) + " " + int_to_string(size_of[string]()) + " " + int_to_string(size_of[Pt]()))
        let s = "hello world"
        let d = string_data(s)
        println(int_to_string(load_u8(d + 4)) + " " + string_from_raw(d + 6, 5, 0))
        free(p)
    }
}