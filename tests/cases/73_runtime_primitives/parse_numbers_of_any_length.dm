module test
fn main() {
    println(int_to_string(parse_int("000000000000000000000000000000000042")) + " " + int_to_string(parse_int("                                        5")) + " " + float_to_string(parse_float("0000000000000000000000000000000000000000000000000000000000000000002.5")))
}