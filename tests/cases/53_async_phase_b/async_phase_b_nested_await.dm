module test

async fn inner() -> int { return 100 }

async fn middle() -> int {
    let y = await inner()
    return y + 10
}

async fn outer() -> int {
    let x = await middle()
    return x + 1
}

fn main() {
    let r = await outer()
    println(int_to_string(r))
}