module test

async fn compute() -> int {
    return 42
}

fn main() {
    let result = await compute()
    println(int_to_string(result))
}