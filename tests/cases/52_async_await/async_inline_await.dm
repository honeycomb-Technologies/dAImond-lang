module test

async fn add(a: int, b: int) -> int {
    return a + b
}

fn main() {
    let sum = await add(3, 4)
    println(int_to_string(sum))
}