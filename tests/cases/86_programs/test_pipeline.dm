module test_pipeline

-- Test pipeline operator |> (Phase 2.1)

fn double(n: int) -> int {
    return n * 2
}

fn add(a: int, b: int) -> int {
    return a + b
}

fn to_upper(s: string) -> string {
    return string_to_upper(s)
}

fn main() {
    -- Simple pipe: 5 |> double => double(5) = 10
    let a = 5 |> double
    println("simple_pipe=" + int_to_string(a))

    -- Pipe with extra args: 5 |> add(3) => add(5, 3) = 8
    let b = 5 |> add(3)
    println("pipe_with_arg=" + int_to_string(b))

    -- Chained pipes: 5 |> double |> double => double(double(5)) = 20
    let c = 5 |> double |> double
    println("chained_pipe=" + int_to_string(c))

    -- String pipes via wrapper function
    let d = "hello" |> to_upper
    println("string_pipe=" + d)
}
