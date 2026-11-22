module enums

-- Two enum values are equal when they are the same variant with
-- equal values

enum Shape {
    Circle(float),
    Rect(int, int),
    Label(string),
    Nothing
}

enum Color {
    Red,
    Green
}

struct Pixel {
    color: Color,
    shape: Shape
}

enum Tree {
    Leaf(int),
    Pair(Pixel, Pixel)
}

fn show(what: string, b: bool) {
    println(what + ": " + bool_to_string(b))
}

fn rect(w: int, h: int) -> Shape {
    return Shape.Rect(w, h)
}

fn main() {
    show("same values", Shape.Rect(1, 2) == Shape.Rect(1, 2))
    show("another second value", Shape.Rect(1, 2) == Shape.Rect(1, 3))
    show("another first value", Shape.Rect(0, 2) == Shape.Rect(1, 2))
    show("another variant", Shape.Rect(1, 2) == Shape.Circle(1.0))
    show("floats", Shape.Circle(1.5) == Shape.Circle(1.5))
    show("strings", Shape.Label("a" + "b") == Shape.Label("ab"))
    show("other strings", Shape.Label("ab") == Shape.Label("abc"))
    show("no values", Shape.Nothing == Shape.Nothing)
    show("no values and some", Shape.Nothing == Shape.Label(""))
    show("!=", Shape.Rect(1, 2) != Shape.Rect(1, 3))
    show("calls", rect(2, 3) == rect(2, 3))
    let s = rect(4, 5)
    let t = rect(4, 6)
    show("variables", s == t)
    show("a variable and itself", s == s)

    -- An enum whose variants carry nothing, in fields
    let p = Pixel { color: Color.Red, shape: Shape.Nothing }
    let q = Pixel { color: Color.Green, shape: Shape.Nothing }
    show("colors", p.color == q.color)
    show("a color and a variant", p.color == Color.Red)
    show("colors differ", p.color != q.color)
    show("pixels", p == q)
    show("a pixel and one like it", p == Pixel { color: Color.Red, shape: Shape.Nothing })

    -- Values within values
    show("trees", Tree.Pair(p, q) == Tree.Pair(p, q))
    show("other trees", Tree.Pair(p, q) == Tree.Pair(q, p))
    show("leaves", Tree.Leaf(3) != Tree.Leaf(3))
    assert_eq(rect(1, 1), Shape.Rect(1, 1))
    println("done")
}
