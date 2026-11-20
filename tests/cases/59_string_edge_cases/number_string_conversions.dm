module test
fn main() {
    println(int_to_string(0) + " " + int_to_string(7) + " " + int_to_string(0 - 7) + " " + int_to_string(10) + " " + int_to_string(99) + " " + int_to_string(100) + " " + int_to_string(0 - 1000))
    let big = 9223372036854775807
    let low = 0 - big - 1
    println(int_to_string(big) + " " + int_to_string(low) + " " + int_to_string(1000000000000000000) + " " + int_to_string(999999999999999999) + " " + int_to_string(0 - 1000000000000000000))
    println(int_to_string(len(int_to_string(low))) + " " + int_to_string(len(int_to_string(big))) + " " + int_to_string(len(int_to_string(0))))
    println(float_to_string(3.14159) + " " + float_to_string(0.0) + " " + float_to_string(0.0 - 2.5) + " " + float_to_string(100000000000000000000.0) + " " + float_to_string(0.000001234) + " " + float_to_string(1234567.0) + " " + float_to_string(1.0 / 3.0))
    println(bool_to_string(true) + " " + bool_to_string(false) + " " + bool_to_string(1 > 2) + int_to_string(len(bool_to_string(false))))
    println(int_to_string(parse_int("42")) + " " + int_to_string(parse_int("-17")) + " " + int_to_string(parse_int("+8")) + " " + int_to_string(parse_int("  \t12abc")) + " " + int_to_string(parse_int("abc")) + " " + int_to_string(parse_int("")) + " " + int_to_string(parse_int("-")) + " " + int_to_string(parse_int("007")) + " " + int_to_string(parse_int("- 5")) + " " + int_to_string(parse_int("3.9")))
    println(int_to_string(parse_int("9223372036854775807")) + " " + int_to_string(parse_int("-9223372036854775808")) + " " + int_to_string(parse_int("9223372036854775808")) + " " + int_to_string(parse_int("-9223372036854775809")) + " " + int_to_string(parse_int("99999999999999999999")) + " " + int_to_string(parse_int("-99999999999999999999")) + " " + int_to_string(parse_int("9223372036854775806")))
    println(float_to_string(parse_float("3.5")) + " " + float_to_string(parse_float("-1e3")) + " " + float_to_string(parse_float("  2.25xyz")) + " " + float_to_string(parse_float("abc")) + " " + float_to_string(parse_float("")) + " " + float_to_string(parse_float(".5")) + " " + float_to_string(parse_float(substr("x12.75y", 1, 5))))
    let mut sum = 0
    let mut i = 0 - 5000
    while i < 5000 {
        sum = sum + parse_int(int_to_string(i * 7919)) - i * 7919 + len(int_to_string(i))
        i = i + 1
    }
    println(int_to_string(sum))
}