module test

enum Option {
    Some(int),
    None
}

fn double_option(opt: Option) -> Option {
    match opt {
        Option::Some(v) => { return Option::Some(v * 2) }
        _ => { return Option::None }
    }
    return Option::None
}

fn get_value(opt: Option) -> int {
    match opt {
        Option::Some(v) => { return v }
        _ => { return 0 }
    }
    return 0
}

fn main() {
    let a = Option::Some(21)
    let b = double_option(a)
    println(get_value(b))
    let c = Option::None
    let d = double_option(c)
    println(get_value(d))
}