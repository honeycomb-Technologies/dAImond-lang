module run_passes_arguments_and_exit_code

fn main() -> int {
    let mut i = 1
    while i < args_len() {
        println(int_to_string(i) + ": [" + args_get(i) + "]")
        i = i + 1
    }
    return 7
}
