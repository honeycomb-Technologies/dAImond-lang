module test

async fn greet() -> string {
    return "Hello, world!"
}

fn main() {
    let msg = await greet()
    println(msg)
}