module assert_eq_strings_differ

fn main() -> int {
    let name = "dAI" + "mond"
    assert_eq(name, "dAImond")
    println("before")
    assert_eq(name, "diamond")
    println("after")
    return 0
}
