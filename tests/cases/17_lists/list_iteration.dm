module test

fn main() {
    let mut items: List[int] = List_new()
    items.push(1)
    items.push(2)
    items.push(3)
    let mut sum: int = 0
    for x in items {
        sum = sum + x
    }
    println(sum)
}