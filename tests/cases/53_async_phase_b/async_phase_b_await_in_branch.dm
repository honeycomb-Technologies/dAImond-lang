module test

async fn get_a() -> int {
    return 1
}

async fn get_b() -> int {
    return 2
}

async fn branch_await(flag: bool) -> int {
    if flag {
        return await get_a()
    } else {
        return await get_b()
    }
}

fn main() {
    let result = await branch_await(true)
    println(int_to_string(result))
}