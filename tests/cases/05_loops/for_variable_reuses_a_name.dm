module main

-- A loop variable has the type the loop gives it, not that of an
-- earlier variable of the same name

fn takes_int(n: int) -> int {
    return n + 1
}

fn main() -> int {
    let i = "text"
    println(i)
    for i in 0..3 {
        let n: int = i
        println(int_to_string(takes_int(n)))
    }
    let k = 5
    let mut m: Map[string, int] = Map_new()
    m.insert("a", k)
    for k in m {
        let s: string = k
        println(s)
    }
    let x = 1.5
    println(float_to_string(x))
    let xs = [1, 2]
    for x in xs {
        let y: int = x
        println(int_to_string(y))
    }
    return 0
}
