module test

fn main() {
    let mut items: List[int] = List_new()
    items.push(10)
    items.push(20)
    items.push(30)
    println(items[0])
    println(items[1])
    println(items[2])
}