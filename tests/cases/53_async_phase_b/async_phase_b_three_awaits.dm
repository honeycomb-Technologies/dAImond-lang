module test

async fn step1() -> int {
    return 3
}

async fn step2(x: int) -> int {
    return x * 2
}

async fn step3(x: int) -> int {
    return x + 10
}

async fn pipeline() -> int {
    let a = await step1()
    let b = await step2(a)
    let c = await step3(b)
    return c
}

fn main() {
    let result = await pipeline()
    println(int_to_string(result))
}