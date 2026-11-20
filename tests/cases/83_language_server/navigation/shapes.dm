module shapes

-- ============================================================
-- Shapes
-- ============================================================

-- A rectangle with its sides along the axes.
-- Its sides are whole numbers.
struct Rect {
    width: int,
    height: int
}

enum Shade {
    Dark,
    Tint(int)
}

impl Rect {
    -- The square with this side
    fn square(side: int) -> Rect {
        return Rect { width: side, height: side }
    }

    fn area(self) -> int {
        return self.width * self.height
    }
}

-- How many units of paint a rectangle needs
fn paint_needed(rect: Rect, coats: int) -> int {
    return rect.area() * coats
}
