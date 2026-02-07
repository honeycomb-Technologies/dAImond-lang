module render

-- ============================================================
--  Rendering: prints each frame directly to stdout
--  No string accumulation — avoids O(n²) memory blowup
--  Uses synchronized output for flicker-free display
-- ============================================================

-- ● = U+25CF, UTF-8: 0xE2 0x97 0x8F (3 bytes, 1 terminal column)
fn ball_char() -> string = chr(226) + chr(151) + chr(143)

fn render_frame(
    paddle_x: int,
    ball_x: int, ball_y: int, ball_on: bool,
    dead: List[int],
    score: int, lives: int,
    state: int, won: bool,
    top_border: string, bot_border: string
) {
    print(sync_begin())

    -- Top border
    print(at(1, 1) + top_border)

    -- Build each row in a single pass (no flicker)
    let mut gy = 0
    while gy < FIELD_H() {
        print(at(gy + 2, 1) + fg(90) + "|" + rst())

        let mut gx = 0
        while gx < FIELD_W() {
            let mut is_special = false

            -- Ball (drawn first so we know to skip other checks)
            let is_ball = state == 1 and ball_on and gx == ball_x and gy == ball_y
            let is_ball_rest = state == 1 and ball_on == false and gx == paddle_x + PADDLE_W() / 2 and gy == FIELD_H() - 2

            if is_ball or is_ball_rest {
                print(bold() + fg(97) + ball_char() + rst())
                is_special = true
            }

            -- Paddle
            if is_special == false and state == 1 and gy == FIELD_H() - 1 and gx >= paddle_x and gx < paddle_x + PADDLE_W() {
                print(bg(107) + " " + rst())
                is_special = true
            }

            -- Bricks
            if is_special == false and gy >= BRICK_Y0() and gy < BRICK_Y0() + BRICK_ROWS() {
                let br = gy - BRICK_Y0()
                let mut found = false
                let mut bc = 0
                while bc < BRICK_COLS() and found == false {
                    let x0 = BRICK_X0() + bc * BRICK_STRIDE()
                    if gx >= x0 and gx < x0 + BRICK_W() {
                        let idx = br * BRICK_COLS() + bc
                        if list_has(dead, idx) == false {
                            print(brick_bg(br) + " " + rst())
                            is_special = true
                            found = true
                        }
                    }
                    bc = bc + 1
                }
            }

            -- Empty cell
            if is_special == false {
                print(" ")
            }

            gx = gx + 1
        }

        print(fg(90) + "|" + rst())
        gy = gy + 1
    }

    -- Bottom border
    print(bot_border)

    -- HUD
    print(at(FIELD_H() + 3, 2))
    print(fg(96) + "SCORE " + rst() + bold() + fg(97) + int_to_string(score) + rst())
    print("   " + fg(91) + "LIVES: ")
    let mut li = 0
    while li < lives {
        print("<3 ")
        li = li + 1
    }
    print(rst() + "      ")

    -- Title screen overlay
    if state == 0 {
        let cy = FIELD_H() / 2 + 1
        let cx = FIELD_W() / 2 - 7
        print(at(cy - 1, cx) + bold() + fg(96) + "B R E A K O U T" + rst())
        print(at(cy + 1, cx) + fg(97) + "Press SPACE to play" + rst())
        print(at(cy + 3, cx + 1) + fg(90) + "A/D or arrows   " + rst())
        print(at(cy + 4, cx + 3) + fg(90) + "Q = quit    " + rst())
    }

    -- Game over overlay
    if state == 2 {
        let cy = FIELD_H() / 2 + 1
        let cx = FIELD_W() / 2 - 5
        if won {
            print(at(cy - 1, cx) + bold() + fg(92) + "* YOU WIN! *" + rst())
        } else {
            print(at(cy - 1, cx) + bold() + fg(91) + " GAME  OVER " + rst())
        }
        print(at(cy + 1, cx - 1) + fg(97) + "Score: " + bold() + fg(93) + int_to_string(score) + rst())
        print(at(cy + 3, cx - 2) + fg(90) + "SPACE=play  Q=quit" + rst())
    }

    print(sync_end())
}
