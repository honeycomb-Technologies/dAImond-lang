module test

async fn get_value() -> int {
    return 10
}

async fn add_five(x: int) -> int {
    return x + 5
}

async fn compute() -> int {
    let a = await get_value()
    let b = await add_five(a)
    return a + b
}

fn main() {
    let result = await compute()
    println(int_to_string(result))
}