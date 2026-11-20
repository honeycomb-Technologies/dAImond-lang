module test

fn add_items(mut items: List[int]) {
    items.push(10)
    items.push(20)
}

fn main() {
    let mut list: List[int] = List_new()
    add_items(list)
    println(list[0])
    println(list[1])
}