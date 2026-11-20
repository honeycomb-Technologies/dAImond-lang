module test
fn main() {
    let mut m: Map[int, string] = Map_new()
    let mut i = 0
    while i < 2000 {
        m.insert(i * 7919, int_to_string(i))
        i = i + 1
    }
    i = 0
    while i < 2000 {
        if i % 3 == 0 {
            m.remove(i * 7919)
        }
        i = i + 1
    }
    m.insert(3 * 7919, "back")
    m.remove(123456789)
    let mut s: Map[string, int] = Map_new()
    i = 0
    while i < 500 {
        s.insert("k" + int_to_string(i), i)
        i = i + 1
    }
    println(int_to_string(m.len()))
    println(m.get(1997 * 7919) + " " + m.get(3 * 7919) + " " + bool_to_string(m.contains(1998 * 7919)))
    println(int_to_string(s.get("k499") + s.get("k0") + s.get("k250")))
}