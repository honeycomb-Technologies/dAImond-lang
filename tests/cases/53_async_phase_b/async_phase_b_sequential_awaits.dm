module test

async fn get_one() -> int { return 1 }
async fn get_two() -> int { return 2 }
async fn get_three() -> int { return 3 }

async fn multi() -> int {
    let a = await get_one()
    let b = await get_two()
    let c = await get_three()
    return a + b + c
}

fn main() {
    let r = await multi()
    println(int_to_string(r))
}