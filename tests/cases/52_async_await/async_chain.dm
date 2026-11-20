module test

async fn add(a: int, b: int) -> int {
    return a + b
}

async fn chain(x: int) -> int {
    let a = await add(x, 5)
    let b = await add(a, 6)
    return b
}

fn main() {
    let result = await chain(0)
    println(int_to_string(result))
}