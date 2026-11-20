module test
struct Big {
    a: string,
    b: string,
    c: string,
    d: int,
    e: int
}
struct Node {
    big: Big,
    next: Box[Node],
    has_next: bool
}
fn make(i: int) -> Big {
    return Big { a: "a" + int_to_string(i), b: "b", c: "c", d: i, e: i * 2 }
}
fn main() {
    let mut head = Node { big: make(0), next: Box_null(), has_next: false }
    let mut i = 1
    while i < 50 {
        let prev = head
        head = Node { big: make(i), next: Box_new(prev), has_next: true }
        i = i + 1
    }
    let mut total = 0
    let mut cur = head
    let mut more = true
    while more {
        total = total + cur.big.d + cur.big.e + len(cur.big.a)
        more = cur.has_next
        if more {
            cur = *cur.next
        }
    }
    println(int_to_string(total))
}