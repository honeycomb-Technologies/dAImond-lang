module test

async fn transform(n: int) -> int {
    return n * 2
}

async fn sum_transformed(n: int) -> int {
    let mut sum = 0
    let mut i = 1
    while i <= n {
        sum = sum + await transform(i)
        i = i + 1
    }
    return sum
}

fn main() {
    let r = await sum_transformed(5)
    println(int_to_string(r))
}