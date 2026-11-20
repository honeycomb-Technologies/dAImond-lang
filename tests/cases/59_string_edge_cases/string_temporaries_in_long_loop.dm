module test
fn main() {
    let mut total = 0
    let mut i = 0
    while i < 400000 {
        let s = "key_" + int_to_string(i) + "_suffix"
        let t = s + "!" + int_to_string(i % 7)
        if substr(t, 0, 4) == "key_" and char_at(s, len(s) - 1) == "x" {
            total = total + len(s)
        }
        i = i + 1
    }
    println(int_to_string(total))
}