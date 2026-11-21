module test

fn main() -> int {
    let n = 3
    let s = "a"
    let b = true
    let xs = [1, 2]
    println(s + 1)
    println(n - s)
    println(s * s)
    println(b + b)
    if s == 3 { println("equal") }
    if n < b { println("less") }
    if n and b { println("both") }
    if b or s { println("either") }
    if n { println("n") }
    while s { println("s") }
    let negative = -s
    let inverse = not n
    println(xs["first"])
    println(s[b])
    match n {
        x if x => println("guard"),
        _ => println("other")
    }
    return 0
}
