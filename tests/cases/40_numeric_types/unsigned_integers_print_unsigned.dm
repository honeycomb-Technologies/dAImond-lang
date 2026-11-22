module test

fn show(x: u32) {
    println(x)
}

fn main() {
    -- An unsigned integer is printed, and widens to int, as the number
    -- it is: its top bit is no sign
    let a: u8 = 200
    println(a)
    println(a + 50)
    println(int_to_string(a))
    println(f"{a} and {a - 1}")
    let b: u16 = 65535
    println(b)
    let c: u32 = 4294967295
    println(c)
    println(c - 1)
    show(c - 1)
    println("n=" + int_to_string(c))

    -- A u64 has values above the highest int
    let d: u64 = 0
    println(d + 10)
    println(~d)
    println(~d / 3)
    println(int_to_string(~d))
    println(f"{~d}")

    -- A signed integer keeps its sign
    let e: i8 = -5
    println(e)
    println(int_to_string(e))
    println(f"{e}")

    -- A builtin that takes an int takes the number too
    let mut s = ""
    while len(s) < 200 {
        s = s + "a"
    }
    s = s + "z"
    println(char_at(s, a))
    eprintln(a)
}
