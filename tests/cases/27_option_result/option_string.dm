module test

fn maybe_greet(greet: bool) -> Option[string] {
    if greet {
        return Some("hello")
    }
    return None
}

fn main() {
    let a = maybe_greet(true)
    if a.has_value {
        println(a.value)
    }
    let b = maybe_greet(false)
    if b.has_value {
        println(a.value)
    } else {
        println("no greeting")
    }
}