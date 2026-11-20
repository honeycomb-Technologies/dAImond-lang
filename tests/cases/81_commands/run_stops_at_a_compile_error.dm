module run_stops_at_a_compile_error

fn main() -> int {
    println("must not run")
    let x: int = "text"
    return 0
}
