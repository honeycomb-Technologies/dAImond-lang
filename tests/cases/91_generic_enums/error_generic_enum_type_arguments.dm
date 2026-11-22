module test

enum Opt[T] {
    Has(T),
    Empty
}

fn main() {
    -- Nothing says what T is
    let e = Opt.Empty
    match e {
        Opt.Has(v) => println("has"),
        Opt.Empty => println("empty")
    }
}
