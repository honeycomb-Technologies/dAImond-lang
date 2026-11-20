module test

struct Rect {
    w: int,
    h: int,
}

impl Rect {
    fn area(self: &Self) -> int {
        return self.w * self.h
    }

    fn width(self: &Self) -> int {
        return self.w
    }
}

fn main() {
    let r = Rect { w: 5, h: 8 }
    println(r.area())
    println(r.width())
}