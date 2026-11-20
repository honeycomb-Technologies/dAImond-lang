module test

async fn get_val(n: int) -> int {
    return n
}

async fn sum_async(n: int) -> int {
    let mut total = 0
    let mut i = 0
    while i < n {
        total = total + await get_val(i)
        i = i + 1
    }
    return total
}

fn main() {
    let r = await sum_async(5)
    println(int_to_string(r))
}