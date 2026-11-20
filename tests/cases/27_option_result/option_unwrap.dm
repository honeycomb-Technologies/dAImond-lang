module test

fn find_positive(x: int) -> Option[int] {
    if x > 0 {
        return Some(x)
    }
    return None
}

fn main() {
    let a = find_positive(10)
    if a.has_value {
        println(int_to_string(a.value))
    }
    let b = find_positive(-5)
    if b.has_value {
        println("should not print")
    } else {
        println("not found")
    }
}