module number_literals

-- Integers in base 16, 2 and 8, `_` between digits, and the ends of
-- an int's range

fn show(label: string, v: int) {
    println(label + " = " + int_to_string(v))
}

fn main() {
    show("0xff", 0xff)
    show("0xFF", 0xFF)
    show("0x1F_e0", 0x1F_e0)
    show("0b1010", 0b1010)
    show("0b1111_0000", 0b1111_0000)
    show("0o17", 0o17)
    show("0o777", 0o777)
    show("0x0", 0x0)
    show("1_000_000", 1_000_000)
    show("0007", 0007)
    show("12_34", 12_34)
    show("0x7fffffffffffffff", 0x7fffffffffffffff)
    show("0x8000000000000000", 0x8000000000000000)
    show("0xffffffffffffffff", 0xffffffffffffffff)
    show("0b1 << 63", 0b1 << 63)
    show("9223372036854775807", 9223372036854775807)
    show("-9223372036854775808", -9223372036854775808)
    show("-9223372036854775808 + 1", -9223372036854775808 + 1)
    show("0xff + 0b1 + 0o10 + 1_0", 0xff + 0b1 + 0o10 + 1_0)
    show("0xdead_beef & 0xffff", 0xdead_beef & 0xffff)

    -- A literal takes the type of its place
    let byte: u8 = 0xff
    let word: u32 = 0xffff_ffff
    let all: u64 = 0xffffffffffffffff
    println(int_to_string(byte as int) + " " + int_to_string(word as int) + " " + bool_to_string(all > 0x7fffffffffffffff))
    println(float_to_string(1_000.5) + " " + float_to_string(1_0e2) + " " + float_to_string(2.5e-1))

    -- In patterns, ranges and array sizes
    let n = 0x10
    match n {
        0x10 => println("sixteen"),
        _ => println("other")
    }
    match -9223372036854775808 + n - n {
        -9223372036854775808 => println("lowest"),
        _ => println("other")
    }
    let mut total = 0
    for i in 0..0b100 {
        total += i
    }
    show("total", total)
    -- 1.len and 1..2 are a number and then something else
    let r = [10, 20, 30]
    for i in 1..3 {
        total += r[i]
    }
    show("total", total)
}
