module test_reports_each_test

fn add(a: int, b: int) -> int {
    return a + b
}

fn test_add() {
    assert_eq(add(2, 3), 5)
}

fn test_wrong_sum() {
    assert_eq(add(2, 2), 5)
}

fn test_strings() {
    assert_eq("x" + "y", "xy")
}

fn test_out_of_bounds() {
    let xs: List[int] = []
    println(int_to_string(xs[3]))
}

fn test_last_still_runs() {
    assert(add(1, 1) == 2)
}

-- Not a test: it takes a parameter
fn test_helper(n: int) -> int {
    return n
}

fn main() -> int {
    println("main must not run")
    return 0
}
