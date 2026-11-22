module error_in_one_instance

-- An error that depends on the type argument is reported for the
-- instance that has it

struct Point {
    x: int,
    y: int
}

fn total[T](xs: List[T], zero: T) -> T {
    let mut sum = zero
    for x in xs {
        sum = sum + x
    }
    return sum
}

fn describe[T](x: T) -> string {
    let text: string = x
    return text
}

fn main() {
    println(int_to_string(total([1, 2, 3], 0)))
    println(total(["a", "b"], ""))
    let ps = [Point { x: 1, y: 2 }]
    let p = total(ps, Point { x: 0, y: 0 })
    println(describe("fine"))
    println(describe(5))
}
