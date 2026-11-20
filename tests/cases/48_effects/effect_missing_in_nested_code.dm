module test

-- The calls that need an effect are inside an if, a loop, an operand
-- and an argument, not statements of their own
fn report(path: string, times: int) -> int with [Console] {
    let mut i = 0
    while i < times {
        if len(path) > 0 {
            let text = "read: " + file_read(path)
            println(text)
        }
        i = i + 1
    }
    println(int_to_string(system("true")))
    return i
}

fn main() -> int {
    return report("", 0)
}
