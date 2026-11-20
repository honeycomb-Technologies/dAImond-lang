module geometry

import shapes.square

fn area(w: int, h: int) -> int {
    let label: string = w
    return w * h + square(w)
}
