module error_number_runs_into_name

-- `12abc` was read as the number 12 and then the name abc

fn main() {
    let abc = 1
    let x = 12abc
    println(int_to_string(x))
}
