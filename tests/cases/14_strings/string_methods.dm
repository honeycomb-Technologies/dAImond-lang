module test
-- A string has methods: each is the builtin function that takes the
-- string first
struct Doc { title: string, lines: List[string] }
fn shout(s: string) -> string { return s.trim().to_upper() + "!" }
fn words(s: string) -> List[string] {
    let mut out: List[string] = []
    for w in s.split(" ") { if w.len() > 0 { out.push(w.to_lower()) } }
    return out
}
fn initials[T](xs: List[T], name: fn(T) -> string) -> string {
    let mut out = ""
    for x in xs { out = out + name(x).char_at(0).to_upper() }
    return out
}
fn main() {
    let s = "  Hello, World  "
    println(shout(s))
    println(int_to_string(s.trim().len()) + " " + int_to_string(s.find("World")) + " " + int_to_string("abc".find("z")))
    println(bool_to_string(s.contains("lo, W")) + bool_to_string(s.trim().starts_with("Hello")) + bool_to_string(s.ends_with("x")))
    println(s.trim().replace("l", "L").substr(0, 5) + "|" + "a,b,,c".split(",")[3] + "|" + int_to_string("a,b,,c".split(",").len()))
    println(int_to_string("A".byte_at(0)) + "abc".char_at(1))
    let d = Doc { title: " notes ", lines: ["One two", "THREE"] }
    println(d.title.trim() + ":" + int_to_string(words(d.lines[0]).len()) + words(d.lines[1])[0])
    println(initials(d.lines, |l: string| -> string l.to_lower()))
    println(f"{s.trim().to_lower()} has {s.trim().split(\" \").len()} words")
    let o: Option[string] = Some(" x ")
    match o { Some(v) => println("[" + v.trim() + "]"), None => {} }
    let m: Map[string, string] = Map_new()
    println(bool_to_string(m.contains("k")) + bool_to_string(["a", "b"].contains("b")))
}
