module probe

fn main() {
    let mut a: [int; 3] = [1, 2, 3]
    let idx = parse_int(args_get(1))
    a[idx] = 999
    println(int_to_string(a[0]))
}
