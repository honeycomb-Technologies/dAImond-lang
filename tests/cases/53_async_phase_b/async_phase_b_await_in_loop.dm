module test

async fn get_value() -> int {
    return 1
}

async fn loop_sum() -> int {
    let mut sum = 0
    while sum < 10 {
        sum = sum + await get_value()
    }
    return sum
}

fn main() {
    let result = await loop_sum()
    println(int_to_string(result))
}