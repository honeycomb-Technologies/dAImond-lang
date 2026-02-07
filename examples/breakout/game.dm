module game

-- ============================================================
--  Game constants, brick logic, and collision detection
-- ============================================================

-- Field dimensions
fn FIELD_W() -> int = 50
fn FIELD_H() -> int = 22

-- Paddle
fn PADDLE_W() -> int = 10
fn PADDLE_SPEED() -> int = 2

-- Brick grid
fn BRICK_ROWS() -> int = 5
fn BRICK_COLS() -> int = 9
fn BRICK_W() -> int = 4
fn BRICK_GAP() -> int = 1
fn BRICK_Y0() -> int = 3
fn BRICK_X0() -> int = 3
fn BRICK_STRIDE() -> int = 5

fn total_bricks() -> int = BRICK_ROWS() * BRICK_COLS()

-- Utilities
fn make_str(ch: string, n: int) -> string {
    let mut s = ""
    let mut i = 0
    while i < n {
        s = s + ch
        i = i + 1
    }
    return s
}

fn new_int_list() -> List[int] {
    let mut v: List[int] = []
    return v
}

fn list_has(list: List[int], val: int) -> bool {
    let mut i = 0
    while i < list.len() {
        if list[i] == val { return true }
        i = i + 1
    }
    return false
}

-- Brick color (bright backgrounds)
fn brick_bg(row: int) -> string {
    if row == 0 { return bg(101) }
    if row == 1 { return bg(105) }
    if row == 2 { return bg(103) }
    if row == 3 { return bg(102) }
    return bg(106)
}

-- Score per brick row (top = most valuable)
fn brick_score(row: int) -> int {
    if row == 0 { return 50 }
    if row == 1 { return 40 }
    if row == 2 { return 30 }
    if row == 3 { return 20 }
    return 10
}

-- Returns brick index if ball hits a live brick, -1 otherwise
fn hit_test(ball_x: int, ball_y: int, dead: List[int]) -> int {
    let mut r = 0
    while r < BRICK_ROWS() {
        let mut c = 0
        while c < BRICK_COLS() {
            let idx = r * BRICK_COLS() + c
            if list_has(dead, idx) == false {
                let x0 = BRICK_X0() + c * BRICK_STRIDE()
                let y0 = BRICK_Y0() + r
                if ball_y == y0 and ball_x >= x0 and ball_x < x0 + BRICK_W() {
                    return idx
                }
            }
            c = c + 1
        }
        r = r + 1
    }
    return 0 - 1
}
