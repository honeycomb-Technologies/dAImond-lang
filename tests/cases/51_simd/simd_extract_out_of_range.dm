module simd_extract_out_of_range

-- A lane the vector does not have is a panic, as an index a list
-- does not have is.

fn lane(v: i32x4, at: int) -> int {
    return simd_extract(v, at)
}

fn main() {
    let v = simd_set_i32x4(10, 20, 30, 40)
    println(int_to_string(lane(v, 0) + lane(v, 3)))
    println(int_to_string(lane(v, 4)))
    println("not reached")
}
