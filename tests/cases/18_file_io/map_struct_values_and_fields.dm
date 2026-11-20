module test
struct Info { name: string, n: int }
struct Reg { by_name: Map[string, Info], flags: Map[string, bool], ids: Map[int, string] }
fn main() {
    let mut r = Reg { by_name: Map_new(), flags: Map_new(), ids: Map_new() }
    r.by_name.insert("a", Info { name: "alpha", n: 1 })
    r.by_name.insert("b", Info { name: "beta", n: 2 })
    r.flags.insert("a", true)
    r.ids[5] = "five"
    r.ids[5] = "FIVE"
    println(r.by_name.get("b").name + " " + int_to_string(r.by_name.get("a").n))
    println(bool_to_string(r.flags.get("a")) + " " + bool_to_string(r.flags.contains("b")))
    println(r.ids[5] + " " + int_to_string(r.ids.len()))
}