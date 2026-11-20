module probe

fn main() {
    let big = parse_int(args_get(1))
    let n = big as i32
    println(int_to_string(n as int))
}
