module test

async fn get_val(n: int) -> int {
    return n * 10
}

async fn conditional(x: int) -> int {
    if x > 0 {
        let a = await get_val(x)
        return a + 1
    } else {
        let b = await get_val(0)
        return b
    }
}

fn main() {
    let r = await conditional(5)
    println(int_to_string(r))
}