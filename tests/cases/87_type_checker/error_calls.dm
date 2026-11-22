module test

struct Button { label: string, on_click: fn(int) -> int }

fn twice(x: int) -> int {
    return x * 2
}

fn join(a: string, b: string) -> string {
    return a + b
}

fn apply(f: fn(int) -> int, v: int) -> int {
    return f(v)
}

extern fn printf(format: string, ...) -> i32

fn main() -> int {
    let n = missing(1)
    let xs: List[int] = [1, 2]
    let a = xs.first()
    let b = xs.len(1)
    let c = xs.contains("one")
    let s = "abc"
    let d = s.upper()
    let e = 5
    let f = e(1)
    let button = Button { label: "ok", on_click: twice }
    let g = button.on_click("left")
    let h = button.on_click(1, 2)
    let i = button.label(1)
    let j = button.pressed()
    let k = apply(join, 1)
    let l = apply(twice, 1)
    let m: fn(string) -> string = twice
    let o = Button { label: "no", on_click: join }
    unsafe {
        printf()
        printf("%d\n", 1)
    }
    let q = 1 |> gone
    return 0
}
