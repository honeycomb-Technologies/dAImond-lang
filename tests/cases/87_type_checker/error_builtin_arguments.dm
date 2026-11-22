module test

struct Point { x: int, y: int }

fn nothing() {
}

fn main() -> int {
    let p = Point { x: 1, y: 2 }
    println(p)
    println("a", "b")
    let a = substr("abc", 1)
    let b = int_to_string("seven")
    let c = int_to_string(1.5)
    let d = float_to_string(2)
    let e = parse_int(5)
    let f = starts_with("abc", 5)
    let g = chr("a")
    let h = time_ms(4)
    sleep_ms("soon")
    file_write("only_a_path")
    let i = string_split("a,b")
    exit("no")
    assert(1)
    assert_eq(1, "one")
    println(nothing())
    let j = len("a", "b")
    let k = bool_to_string(0)
    let n: i32 = 7
    println(int_to_string(n))
    println(n)
    println(1.5)
    println(true)
    return 0
}
