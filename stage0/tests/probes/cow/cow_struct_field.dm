module e6

struct Bag {
    items: List[int]
}

fn main() {
    let mut a: List[int] = []
    a.push(5)
    let mut bag = Bag { items: a }
    a.push(6)
    a[0] = 50
    println(int_to_string(len(bag.items)))
    println(int_to_string(bag.items[0]))
    bag.items.push(7)
    println(int_to_string(len(a)))
    println(int_to_string(len(bag.items)))
}
