module test
extern fn getenv(name: string) -> string
extern fn strlen(s: string) -> int
fn main() {
    let b = "hello, world"
    println("[" + substr(b, 3, 0 - 2) + "|" + substr(b, 0 - 1, 2) + "|" + char_at(b, 99) + "|" + char_at(b, 0 - 1) + "|" + b[1] + "]")
    unsafe {
        println(bool_to_string(len(getenv("DM_NOT_SET_XYZ")) == 0) + " " + int_to_string(strlen(b + "!")) + " " + int_to_string(strlen(substr(b, 0, 5))) + " " + int_to_string(strlen(char_at(b, 4))))
    }
}