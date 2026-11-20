module main

import left
import right
import base

fn main() -> int {
    println(int_to_string(left_value() + right_value() + base_value() + core_value()))
    return 0
}
