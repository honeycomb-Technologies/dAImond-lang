module test_mono_stress

-- Generic function with a for-in loop inside
fn count_up_to[T](n: int, label: T) -> int {
    let mut items: List[int] = []
    let mut i = 0
    while i < n {
        items.push(i)
        i = i + 1
    }
    let mut total = 0
    for item in items {
        total = total + item
    }
    return total
}

-- Generic identity to test basic monomorphization with multiple types
fn identity[T](x: T) -> T {
    return x
}

-- Generic with string ops
fn wrap[T](val: T, prefix: string) -> string {
    let result = prefix + ": done"
    return result
}

fn main() {
    -- Call count_up_to with int and string to test for_counter propagation
    let sum1 = count_up_to[int](4, 0)
    println("sum1=" + int_to_string(sum1))

    let sum2 = count_up_to[string](3, "x")
    println("sum2=" + int_to_string(sum2))

    -- Call identity with multiple types
    let a = identity[int](42)
    println("a=" + int_to_string(a))

    let b = identity[string]("hello")
    println("b=" + b)

    let c = identity[bool](true)
    println("c=" + bool_to_string(c))

    -- Call wrap with multiple types
    let w1 = wrap[int](10, "num")
    println(w1)

    let w2 = wrap[string]("hi", "str")
    println(w2)
}
