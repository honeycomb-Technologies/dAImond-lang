module map_copies

-- A copy of a map has the entries of the original. A key added to
-- one is not in the other, and a map whose copy has added a key
-- shares nothing with it once it changes.

fn with_extra(m: Map[string, int]) -> int {
    let mut mine = m
    mine.insert("extra", 0)
    return mine.len()
}

fn keys_of(m: Map[string, int]) -> string {
    let mut text = ""
    for k in m.keys() {
        text = text + k + "=" + int_to_string(m.get(k)) + " "
    }
    return text
}

fn main() {
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    let mut n = m

    -- A key added to the copy
    n.insert("b", 3)
    println(int_to_string(m.len()) + " " + int_to_string(n.len()) + " " + bool_to_string(m.contains("b")))

    -- And one added to the original
    m.insert("c", 4)
    println(keys_of(m) + "| " + keys_of(n))

    -- They share nothing now
    m.insert("a", 10)
    n.remove("a")
    println(keys_of(m) + "| " + keys_of(n))
    println(int_to_string(m.len()) + " " + int_to_string(n.len()))

    -- A function that adds to its parameter's copy
    println(int_to_string(with_extra(m)) + " " + int_to_string(m.len()) + " " + bool_to_string(m.contains("extra")))
    m.insert("z", 26)
    println(keys_of(m))

    -- Two maps that begin as one, with enough keys to grow
    let mut base: Map[int, int] = Map_new()
    let mut i = 0
    while i < 20 {
        base.insert(i, i * i)
        i = i + 1
    }
    let mut evens = base
    let mut odds = base
    i = 20
    while i < 60 {
        if i % 2 == 0 {
            evens.insert(i, 2)
        } else {
            odds.insert(i, 1)
        }
        i = i + 1
    }
    println(int_to_string(base.len()) + " " + int_to_string(evens.len()) + " " + int_to_string(odds.len()))
    println(bool_to_string(evens.contains(21)) + " " + bool_to_string(odds.contains(21)) + " " + bool_to_string(odds.contains(22)) + " " + bool_to_string(base.contains(22)))
    println(int_to_string(evens.get(19)) + " " + int_to_string(odds.get(7)) + " " + int_to_string(evens.get(58)) + " " + int_to_string(odds.get(59)))

    -- A removal from a map whose copy has added a key
    let mut left: Map[string, int] = Map_new()
    left.insert("k", 1)
    left.insert("l", 2)
    let mut right = left
    right.insert("r", 3)
    left.remove("k")
    println(keys_of(left) + "| " + keys_of(right))
}
