module test

fn main() {
    let x: Option[int] = Some(42)
    if x.has_value {
        println(int_to_string(x.value))
    }
    let y: Option[int] = None
    if y.has_value {
        println("should not print")
    } else {
        println("none")
    }
}