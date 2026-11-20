module test

fn main() {
    assert(1 + 1 == 2)
    assert_eq(2 + 2, 4)
    assert_eq("ab", "ab")
    file_write("/tmp/dm_audit_misc.txt", "x")
    file_append("/tmp/dm_audit_misc.txt", "y")
    println(file_read("/tmp/dm_audit_misc.txt"))
    println(file_exists("/tmp/dm_audit_misc.txt"))
    println(len(args_get(0)) > 0)
    let mut m: Map[string, int] = Map_new()
    m.set("k", 3)
    println(m.contains("k"))
    m.remove("k")
    println(m.contains("k"))
    m.insert("a", 1)
    println(len(m.keys()))
    println(len(m.values()))
    println(float_to_string(1.5))
    println(bool_to_string(true))
}