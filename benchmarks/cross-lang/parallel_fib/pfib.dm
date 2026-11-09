module pfib_bench

fn fib(n: int) -> int {
    if n <= 1 { return n }
    return fib(n - 1) + fib(n - 2)
}

fn worker35() {
    let r = fib(35)
    file_write("/tmp/pfib_35.txt", int_to_string(r))
}

fn worker36() {
    let r = fib(36)
    file_write("/tmp/pfib_36.txt", int_to_string(r))
}

fn worker37() {
    let r = fib(37)
    file_write("/tmp/pfib_37.txt", int_to_string(r))
}

fn worker38() {
    let r = fib(38)
    file_write("/tmp/pfib_38.txt", int_to_string(r))
}

fn main() {
    let t1 = thread_spawn(worker35)
    let t2 = thread_spawn(worker36)
    let t3 = thread_spawn(worker37)
    let t4 = thread_spawn(worker38)
    thread_join(t1)
    thread_join(t2)
    thread_join(t3)
    thread_join(t4)
    let r1 = parse_int(file_read("/tmp/pfib_35.txt"))
    let r2 = parse_int(file_read("/tmp/pfib_36.txt"))
    let r3 = parse_int(file_read("/tmp/pfib_37.txt"))
    let r4 = parse_int(file_read("/tmp/pfib_38.txt"))
    println(int_to_string(r1 + r2 + r3 + r4))
}
