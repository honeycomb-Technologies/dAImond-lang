module probe

fn main() {
    let a: [int; 3] = [1, 2, 3]
    let idx = parse_int(args_get(1))
    println(int_to_string(a[idx]))
}
