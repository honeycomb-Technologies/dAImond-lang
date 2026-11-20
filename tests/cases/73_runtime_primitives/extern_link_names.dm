module test
-- A program's own declarations of C functions the runtime also uses,
-- with other signatures
extern fn memcmp(a: string, b: string, n: int) -> i32
extern fn malloc(n: int) -> int
extern fn my_len(s: string) -> u64 = "strlen"
extern fn other_len(p: int) -> int = "strlen"
fn main() {
    unsafe {
        let p = malloc(16)
        store_u8(p, 104)
        store_u8(p + 1, 105)
        store_u8(p + 2, 0)
        println(int_to_string(memcmp("abc", "abd", 3) as int) + " " + int_to_string(memcmp("same", "same", 4) as int))
        println(int_to_string(my_len("four") as int) + " " + int_to_string(other_len(p)) + " " + string_from_raw(p, 2, 0) + "!" + " " + bool_to_string("x" + "y" == "xy"))
    }
}