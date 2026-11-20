module test

async fn do_work() {
    println("working")
}

fn main() {
    await do_work()
}