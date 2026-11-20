module test_box

-- Test Box[T] support (Phase 2.3)

struct Wrapper {
    value: int
}

fn main() {
    -- Box_new: heap allocate a struct
    let b: Box[Wrapper] = Box_new(Wrapper { value: 42 })
    println("box_created=true")

    -- Box_null
    let n: Box[Wrapper] = Box_null()
    println("box_null=true")
}
