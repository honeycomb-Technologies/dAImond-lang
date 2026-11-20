module test
fn count_words(text: string) -> Map[string, int] {
    let mut m: Map[string, int] = Map_new()
    for w in string_split(text, " ") {
        if m.contains(w) {
            m[w] = m[w] + 1
        } else {
            m[w] = 1
        }
    }
    return m
}
fn main() {
    let c = count_words("a b a c a b")
    let mut total = 0
    for k in c {
        total = total + c[k]
    }
    let mut vsum = 0
    for v in c.values() {
        vsum = vsum + v
    }
    println(int_to_string(c["a"]) + " " + int_to_string(c["b"]) + " " + int_to_string(c.len()))
    println(int_to_string(total) + " " + int_to_string(vsum) + " " + int_to_string(c.keys().len()))
}