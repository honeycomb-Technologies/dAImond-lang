module test

fn main() {
    let opt: Option[int] = Some(42)
    match opt {
        Some(v) => println(int_to_string(v))
        _ => println("none")
    }
}