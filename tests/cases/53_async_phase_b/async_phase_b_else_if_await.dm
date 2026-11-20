module test

async fn fetch(n: int) -> int { return n * 100 }

async fn classify(x: int) -> int {
    if x > 10 {
        return await fetch(3)
    } else if x > 5 {
        return await fetch(2)
    } else {
        return await fetch(1)
    }
}

fn main() {
    let r = await classify(7)
    println(int_to_string(r))
}