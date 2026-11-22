module test
fn main() {
    let o: Option[int] = Some(1)
    if let Some(x) o {
        println("x")
    }
}
