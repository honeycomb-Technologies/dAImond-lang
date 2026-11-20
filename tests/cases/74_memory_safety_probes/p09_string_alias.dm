module probe

fn main() {
    let mut s = ""
    let mut i = 0
    while i < 20 {
        s = s + "abcdefghij"
        i = i + 1
    }
    let t = s
    let mut j = 0
    while j < 20 {
        s = s + "abcdefghij"
        j = j + 1
    }
    println(int_to_string(len(t)))
    println(int_to_string(len(s)))
    println(substr(t, 0, 10))
}
