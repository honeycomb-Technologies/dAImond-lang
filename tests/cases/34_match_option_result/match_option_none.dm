module test

fn make_none() -> Option[int] {
    return None
}

fn main() {
    let opt: Option[int] = make_none()
    match opt {
        Some(v) => println(int_to_string(v))
        _ => println("none")
    }
}