-- Micro Benchmark: Map Operations
-- Inserts and looks up 100,000 key-value pairs.
-- Tests hash map performance.

module map_ops_bench

fn main() {
    let mut m: Map[string, int] = Map_new()

    -- Insert 100K entries
    let mut i = 0
    while i < 100000 {
        let key = "key_" + int_to_string(i)
        m.insert(key, i)
        i = i + 1
    }
    println("inserted " + int_to_string(m.len()) + " entries")

    -- Lookup all entries
    let mut found = 0
    i = 0
    while i < 100000 {
        let key = "key_" + int_to_string(i)
        if m.contains(key) {
            found = found + 1
        }
        i = i + 1
    }
    println("found " + int_to_string(found) + " entries")

    -- Remove half
    i = 0
    while i < 50000 {
        let key = "key_" + int_to_string(i)
        m.remove(key)
        i = i + 1
    }
    println("remaining " + int_to_string(m.len()) + " entries")
}
