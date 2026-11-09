module hashmap_bench

fn main() {
    let mut m: Map[string, int] = Map_new()

    -- Insert 1,000,000 entries
    let mut i = 0
    while i < 1000000 {
        m.insert("key_" + int_to_string(i), i)
        i = i + 1
    }
    println("inserted: 1000000")

    -- Lookup all entries, sum values
    let mut sum = 0
    i = 0
    while i < 1000000 {
        sum = sum + m.get("key_" + int_to_string(i))
        i = i + 1
    }
    println("lookup sum: " + int_to_string(sum))

    -- Delete entries where i % 3 == 0
    i = 0
    while i < 1000000 {
        if i % 3 == 0 {
            m.remove("key_" + int_to_string(i))
        }
        i = i + 1
    }
    println("final: " + int_to_string(m.len()))
}
