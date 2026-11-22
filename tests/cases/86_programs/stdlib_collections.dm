module stdlib_collections
import std.collections
import std.math
import std.string

fn ck(label: string, got: string, want: string) {
    if got != want { println("FAIL " + label + ": got [" + got + "], want [" + want + "]") }
}
fn cki(label: string, got: int, want: int) { ck(label, int_to_string(got), int_to_string(want)) }
fn ckb(label: string, got: bool, want: bool) { ck(label, bool_to_string(got), bool_to_string(want)) }

struct Job { id: int, name: string }

fn drain(mut q: Queue[Job]) -> string {
    let mut out = ""
    while q.is_empty() == false {
        match q.pop() { Some(j) => { out = out + j.name + int_to_string(j.id) }, None => {} }
    }
    return out
}

fn main() {
    let mut seen: Set[string] = Set.new()
    seen.add("a")
    seen.add("b")
    seen.add("a")
    cki("set len", seen.len(), 2)
    ckb("set contains", seen.contains("a"), true)
    ckb("set contains no", seen.contains("z"), false)
    ckb("set remove", seen.remove("a"), true)
    ckb("set remove again", seen.remove("a"), false)
    ck("set list", string_join(seen.to_list(), ","), "b")
    let nums = Set.from([3, 1, 3, 2, 1])
    cki("nums len", nums.len(), 3)
    let more = Set.from([2, 9])
    let both = nums.union(more)
    let common = nums.intersection(more)
    cki("union", both.len(), 4)
    cki("intersection", common.len(), 1)
    ckb("common has 2", common.contains(2), true)
    let ids = Set[i32].new()
    ckb("empty", ids.is_empty(), true)

    let mut q = Queue[int].new()
    let mut i = 0
    while i < 100 {
        q.push(i)
        i += 1
    }
    let mut total = 0
    let mut taken = 0
    while taken < 60 {
        match q.pop() { Some(v) => { total += v }, None => println("FAIL queue ran dry") }
        taken += 1
    }
    cki("queue total", total, 1770)
    cki("queue len", q.len(), 40)
    match q.peek() { Some(v) => cki("queue peek", v, 60), None => println("FAIL peek") }
    q.push(1000)
    cki("queue len 2", q.len(), 41)
    let mut last = -1
    while q.is_empty() == false {
        match q.pop() { Some(v) => { last = v }, None => {} }
    }
    cki("queue last", last, 1000)
    match q.pop() { Some(v) => println("FAIL pop of empty"), None => {} }
    let mut jobs: Queue[Job] = Queue.new()
    jobs.push(Job { id: 1, name: "a" })
    jobs.push(Job { id: 2, name: "b" })
    ck("drain", drain(jobs), "a1b2")

    let mut undo = Stack.of("first")
    undo.push("second")
    cki("stack len", undo.len(), 2)
    match undo.peek() { Some(s) => ck("stack peek", s, "second"), None => println("FAIL stack peek") }
    match undo.pop() { Some(s) => ck("stack pop", s, "second"), None => println("FAIL stack pop") }
    match undo.pop() { Some(s) => ck("stack pop 2", s, "first"), None => println("FAIL stack pop 2") }
    match undo.pop() { Some(s) => println("FAIL pop of empty stack"), None => {} }
    ckb("stack empty", undo.is_empty(), true)
    let mut fs: Stack[float] = Stack.new()
    fs.push(1.5)
    fs.push(sqrt(16.0))
    match fs.pop() { Some(v) => ck("sqrt", float_to_string(v), "4"), None => {} }

    ck("pad", pad_left("7", 3, "0") + pad_right("x", 3, "."), "007x..")
    ck("repeat", string_repeat("ab", 3), "ababab")
    ck("reverse", string_reverse("abc"), "cba")
    ck("math", float_to_string(math_clamp(pow(2.0, 10.0), 0.0, 1000.0)) + " " + float_to_string(floor(math_pi())), "1000 3")
    cki("old api", max_int(3, min_int(9, 4)), 4)
    println("done")
}
