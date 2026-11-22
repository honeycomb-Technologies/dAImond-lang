module main

fn main() {
    let echo: List[string] = ["/bin/echo", "a b", "$HOME", "*"]
    println("start")
    let code = run_command(echo)
    println(int_to_string(code))
    println(int_to_string(run_command(["false"])))
    println(int_to_string(run_command(["/no/such/program"])))
    println(int_to_string(run_command(["sh", "-c", "exit 7"])))
    println(int_to_string(run_command(["sh", "-c", "kill -9 $$"])))
    let empty: List[string] = []
    println(int_to_string(run_command(empty)))

    let input = "/tmp/daimond_test_run_command.in"
    let output = "/tmp/daimond_test_run_command.out"
    let errors = "/tmp/daimond_test_run_command.err"
    file_write(input, "hello\n")
    println(int_to_string(run_command_io(["tr", "a-z", "A-Z"], input, output, "")))
    print(file_read(output))
    println(int_to_string(run_command_io(["sh", "-c", "echo oops >&2; echo fine; exit 3"], "", output, errors)))
    print(file_read(output))
    print(file_read(errors))
    fs_remove(input)
    fs_remove(output)
    fs_remove(errors)
}
