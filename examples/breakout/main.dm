module breakout

import terminal
import game
import render

-- ============================================================
--  Main: game loop, state management, physics
-- ============================================================

fn main() {
    let un1 = enter_raw_mode()
    -- Set fast key repeat (35ms delay, 60/s rate) so held keys feel instant
    let un3 = system("xset r rate 35 60 2>/dev/null")
    print(hide_cursor() + cls())

    -- Precompute static strings
    let dashes = make_str("-", FIELD_W())
    let top_border = fg(90) + "+" + dashes + "+" + rst()
    let bot_border = at(FIELD_H() + 2, 1) + fg(90) + "+" + dashes + "+" + rst()

    -- State
    let mut paddle_x = FIELD_W() / 2 - PADDLE_W() / 2
    let mut ball_x = FIELD_W() / 2
    let mut ball_y = FIELD_H() - 2
    let mut ball_dx = 1
    let mut ball_dy = 0 - 1
    let mut score = 0
    let mut lives = 3
    let mut state = 0
    let mut ball_on = false
    let mut won = false
    let mut running = true
    let mut dead = new_int_list()
    let spf = 1000 / 60
    let mut tick = 0
    let mut paddle_dir = 0
    let mut since_key = 999
    let mut key_count = 0

    -- Mark long-lived strings (borders etc) before game loop
    let mem0 = mem_mark()

    while running {
        let t0 = time_ms()
        let inp = read_all_input()

        -- Quit
        if inp.quit { running = false }

        -- State: title
        if state == 0 {
            if inp.space {
                state = 1
                ball_on = false
            }
        -- State: game over
        } else if state == 2 {
            if inp.space {
                state = 1
                ball_on = false
                score = 0
                lives = 3
                won = false
                paddle_x = FIELD_W() / 2 - PADDLE_W() / 2
                dead = new_int_list()
            }
        -- State: playing
        } else {
            -- Paddle movement
            let net = inp.right - inp.left
            if net != 0 {
                if net < 0 { paddle_dir = 0 - 1 }
                if net > 0 { paddle_dir = 1 }
                key_count = key_count + 1
                since_key = 0
            } else {
                since_key = since_key + 1
                -- Once repeat was active (3+ events), stop fast on release
                if key_count >= 3 and since_key > 4 {
                    paddle_dir = 0
                    key_count = 0
                }
                -- Before repeat kicks in, bridge the OS delay
                if key_count < 3 and since_key > 40 {
                    paddle_dir = 0
                    key_count = 0
                }
            }
            paddle_x = paddle_x + paddle_dir * PADDLE_SPEED()
            if paddle_x < 0 { paddle_x = 0 }
            if paddle_x + PADDLE_W() > FIELD_W() { paddle_x = FIELD_W() - PADDLE_W() }

            -- Launch ball
            if ball_on == false and inp.space {
                ball_on = true
                ball_x = paddle_x + PADDLE_W() / 2
                ball_y = FIELD_H() - 2
                ball_dx = 1
                ball_dy = 0 - 1
            }
        }

        -- Physics at 30Hz (every 2nd frame at 60fps)
        let do_physics = tick % 2 == 0
        tick = tick + 1

        if state == 1 and ball_on and do_physics {
            ball_x = ball_x + ball_dx
            ball_y = ball_y + ball_dy

            -- Wall bounces
            if ball_x <= 0 { ball_x = 1; ball_dx = 0 - ball_dx }
            if ball_x >= FIELD_W() - 1 { ball_x = FIELD_W() - 2; ball_dx = 0 - ball_dx }
            if ball_y <= 1 { ball_y = 2; ball_dy = 0 - ball_dy }

            -- Paddle bounce
            if ball_dy > 0 and ball_y == FIELD_H() - 1 {
                if ball_x >= paddle_x and ball_x < paddle_x + PADDLE_W() {
                    ball_dy = 0 - ball_dy
                    -- Angle control: left third = left, right third = right
                    let rel = ball_x - paddle_x
                    if rel < PADDLE_W() / 3 { ball_dx = 0 - 1 }
                    if rel >= PADDLE_W() * 2 / 3 { ball_dx = 1 }
                }
            }

            -- Ball lost
            if ball_y >= FIELD_H() {
                lives = lives - 1
                ball_on = false
                if lives <= 0 { state = 2 }
            }

            -- Brick collision
            let hit = hit_test(ball_x, ball_y, dead)
            if hit >= 0 {
                dead.push(hit)
                ball_dy = 0 - ball_dy
                score = score + brick_score(hit / BRICK_COLS())
                if dead.len() == total_bricks() {
                    won = true
                    state = 2
                }
            }
        } else if state == 1 and ball_on == false {
            -- Ball tracks paddle before launch
            ball_x = paddle_x + PADDLE_W() / 2
            ball_y = FIELD_H() - 2
        }

        -- Render (prints directly, no string accumulation)
        render_frame(paddle_x, ball_x, ball_y, ball_on, dead, score, lives, state, won, top_border, bot_border)
        flush()

        -- Free all temporary strings from this frame
        mem_sweep(mem0)

        -- Frame cap
        let dt = time_ms() - t0
        if dt < spf { sleep_ms(spf - dt) }
    }

    -- Cleanup
    print(show_cursor() + cls())
    let un4 = system("xset r rate 2>/dev/null")
    let un2 = exit_raw_mode()
    println("Thanks for playing! Score: " + int_to_string(score))
}
