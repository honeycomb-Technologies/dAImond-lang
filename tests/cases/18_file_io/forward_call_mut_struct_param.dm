module test

struct State { pos: int }
struct Res { value: int, st: State }

fn first(mut s: State) -> Res {
    let inner = second(s)
    return Res { value: inner.value + 1, st: inner.st }
}

fn second(mut s: State) -> Res {
    s.pos = s.pos + 10
    return Res { value: s.pos, st: s }
}

fn main() {
    let r = first(State { pos: 1 })
    println(int_to_string(r.value))
    println(int_to_string(r.st.pos))
}