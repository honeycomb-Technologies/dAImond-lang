module logic_chains

-- `and` and `or` in chains: each runs its right side only when its left
-- side leaves the result open, `and` binds tighter than `or`, and a
-- chain is evaluated from the left.

struct Log { calls: List[string] }

fn t(mut log: Log, name: string) -> bool {
    log.calls.push(name)
    return true
}

fn f(mut log: Log, name: string) -> bool {
    log.calls.push(name)
    return false
}

fn show(mut log: Log, label: string, v: bool) {
    let mut line = label + " = " + bool_to_string(v) + " :"
    for c in log.calls { line = line + " " + c }
    println(line)
    let none: List[string] = []
    log.calls = none
}

fn main() {
    let mut log = Log { calls: [] }
    show(log, "t and t and t", t(log, "a") and t(log, "b") and t(log, "c"))
    show(log, "t and f and t", t(log, "a") and f(log, "b") and t(log, "c"))
    show(log, "f and t and t", f(log, "a") and t(log, "b") and t(log, "c"))
    show(log, "f or f or t", f(log, "a") or f(log, "b") or t(log, "c"))
    show(log, "t or f or t", t(log, "a") or f(log, "b") or t(log, "c"))
    show(log, "f or t and f", f(log, "a") or t(log, "b") and f(log, "c"))
    show(log, "t or t and f", t(log, "a") or t(log, "b") and f(log, "c"))
    show(log, "f and t or t", f(log, "a") and t(log, "b") or t(log, "c"))
    show(log, "t and f or f or t", t(log, "a") and f(log, "b") or f(log, "c") or t(log, "d"))
    show(log, "(f or t) and (f or f)", (f(log, "a") or t(log, "b")) and (f(log, "c") or f(log, "d")))
    show(log, "not f and t", not f(log, "a") and t(log, "b"))
    show(log, "t and not t or t", t(log, "a") and not t(log, "b") or t(log, "c"))
    let x = 5
    show(log, "x > 3 and x < 9 and t", x > 3 and x < 9 and t(log, "a"))
    show(log, "x > 7 and t or x == 5 and t", x > 7 and t(log, "a") or x == 5 and t(log, "b"))
    show(log, "1 + 2 == 3 and 2 * 2 == 4 and t", 1 + 2 == 3 and 2 * 2 == 4 and t(log, "a"))
    let mut i = 0
    let mut n = 0
    while i < 10 and (i % 2 == 0 or i < 5) and n < 100 {
        n = n + i
        i = i + 1
    }
    println(int_to_string(i) + " " + int_to_string(n))
    let s = "abc"
    if len(s) == 3 and s == "abc" and starts_with(s, "a") and not ends_with(s, "x") or f(log, "never") {
        println("strings")
    }
    show(log, "after if", true)
    let v = if t(log, "a") and f(log, "b") or t(log, "c") { 1 } else { 2 }
    show(log, "if value " + int_to_string(v), v == 1)
}
