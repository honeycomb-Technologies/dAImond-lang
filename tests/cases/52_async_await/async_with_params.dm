module test

async fn add(a: int, b: int) -> int {
    return a + b
}

fn main() {
    let result = await add(10, 20)
    println(int_to_string(result))
}