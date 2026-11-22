module test

enum Opt[T] {
    Has(T),
    Empty
}

fn show(o: Opt[int]) -> int {
    return match o {
        Opt.Has(v) => v
    }
}

fn main() {
    let a: Opt[int] = Opt.Has("s")
    let b: Opt[string] = Opt.Has(1, 2)
    println(show(Opt.Has(2.5)))
    let c: Opt[int] = Opt.Missing
    let two: Opt[int, int] = Opt.Empty
    let inner: List[Opt[int, string]] = []
}
