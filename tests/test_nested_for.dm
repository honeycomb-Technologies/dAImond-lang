module test_nested_for

-- Test nested for-in loops (Phase 1.1: unique iterator variables)

fn main() {
    let mut outer: List[int] = []
    outer.push(1)
    outer.push(2)
    outer.push(3)

    let mut inner: List[int] = []
    inner.push(10)
    inner.push(20)

    let mut sum = 0
    for a in outer {
        for b in inner {
            sum = sum + a * b
        }
    }
    -- Expected: (1*10 + 1*20) + (2*10 + 2*20) + (3*10 + 3*20) = 30 + 60 + 90 = 180
    println("nested_for_sum=" + int_to_string(sum))

    -- Triple nesting
    let mut tiny: List[int] = []
    tiny.push(1)
    tiny.push(2)

    let mut count = 0
    for x in tiny {
        for y in tiny {
            for z in tiny {
                count = count + 1
            }
        }
    }
    -- Expected: 2*2*2 = 8
    println("triple_nested_count=" + int_to_string(count))
}
