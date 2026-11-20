module check_reports_errors

fn double(n: int) -> int {
    return n * 2
}

fn main() -> int {
    println(int_to_string(double("two")))
    return 0
}
