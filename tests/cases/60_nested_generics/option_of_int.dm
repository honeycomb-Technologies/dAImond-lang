fn main() {
    let x: Option[int] = Some(42)
    match x {
        Some(v) => println(int_to_string(v)),
        None => println("none")
    }
}