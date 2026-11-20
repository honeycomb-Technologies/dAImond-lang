module main

-- Compiled from another directory, with DAIMOND_STDLIB naming the library
import std.math

fn main() -> int {
    if math_abs(0.0 - 7.5) == 7.5 { println("std.math found") }
    return 0
}
