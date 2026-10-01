use std::thread;

fn fib(n: i64) -> i64 {
    if n <= 1 {
        return n;
    }
    fib(n - 1) + fib(n - 2)
}

fn main() {
    let t1 = thread::spawn(|| fib(35));
    let t2 = thread::spawn(|| fib(36));
    let t3 = thread::spawn(|| fib(37));
    let t4 = thread::spawn(|| fib(38));

    let r1 = t1.join().unwrap();
    let r2 = t2.join().unwrap();
    let r3 = t3.join().unwrap();
    let r4 = t4.join().unwrap();

    let sum = r1 + r2 + r3 + r4;
    println!("{}", sum);
}
