module snake

-- ============================================================
--  Snake game in dAImond
--  Terminal-based with ANSI rendering, 60fps frame cap
--  Controls: WASD or arrow keys, Q to quit, SPACE to start
-- ============================================================

-- ============================================================
--  Constants
-- ============================================================
fn FIELD_W() -> int = 40
fn FIELD_H() -> int = 20
fn INITIAL_LEN() -> int = 4
fn TICKS_PER_MOVE() -> int = 6

-- Directions
fn DIR_UP() -> int = 0
fn DIR_DOWN() -> int = 1
fn DIR_LEFT() -> int = 2
fn DIR_RIGHT() -> int = 3

-- ============================================================
--  Terminal helpers
-- ============================================================
fn esc() -> string = chr(27)
fn csi() -> string = esc() + "["
fn at(r: int, c: int) -> string =
    csi() + int_to_string(r) + ";" + int_to_string(c) + "H"
fn cls() -> string = csi() + "2J" + at(1, 1)
fn hide_cursor() -> string = csi() + "?25l"
fn show_cursor() -> string = csi() + "?25h"
fn sync_begin() -> string = ""
fn sync_end() -> string = ""
fn rst() -> string = csi() + "0m"
fn bold() -> string = csi() + "1m"
fn fg(code: int) -> string = csi() + int_to_string(code) + "m"
fn bg(code: int) -> string = csi() + int_to_string(code) + "m"

-- ============================================================
--  Utility
-- ============================================================
fn make_str(ch: string, n: int) -> string {
    let mut s = ""
    let mut i = 0
    while i < n {
        s = s + ch
        i = i + 1
    }
    return s
}

fn list_contains(lst: List[int], val: int) -> bool {
    let mut i = 0
    while i < lst.len() {
        if lst[i] == val { return true }
        i = i + 1
    }
    return false
}

-- Encode (x, y) as a single int for fast lookup
fn encode_pos(x: int, y: int) -> int = y * FIELD_W() + x

-- Simple RNG (linear congruential)
fn next_rand(seed: int) -> int {
    let s = seed * 1103515245 + 12345
    -- Keep it positive
    if s < 0 { return 0 - s }
    return s
}

-- ============================================================
--  Input handling
-- ============================================================
struct Input {
    dir: int,
    has_dir: bool,
    space: bool,
    quit: bool
}

fn read_all_input() -> Input {
    let mut dir = 0
    let mut has = false
    let mut sp = false
    let mut q = false
    let mut k = read_key_nb()
    while k != 0 - 1 {
        if k == 113 or k == 81 { q = true }
        if k == 32 { sp = true }
        -- WASD
        if k == 119 or k == 87 { dir = DIR_UP(); has = true }
        if k == 115 or k == 83 { dir = DIR_DOWN(); has = true }
        if k == 97 or k == 65 { dir = DIR_LEFT(); has = true }
        if k == 100 or k == 68 { dir = DIR_RIGHT(); has = true }
        -- Arrow keys
        if k == 27 {
            let k2 = read_key_nb()
            if k2 == 91 {
                let k3 = read_key_nb()
                if k3 == 65 { dir = DIR_UP(); has = true }
                if k3 == 66 { dir = DIR_DOWN(); has = true }
                if k3 == 68 { dir = DIR_LEFT(); has = true }
                if k3 == 67 { dir = DIR_RIGHT(); has = true }
            }
        }
        k = read_key_nb()
    }
    return Input { dir: dir, has_dir: has, space: sp, quit: q }
}

-- ============================================================
--  Rendering
-- ============================================================

-- Snake body: block char
fn snake_head_char() -> string = chr(226) + chr(150) + chr(160)
fn snake_body_char() -> string = chr(226) + chr(150) + chr(161)
-- Food: star
fn food_char() -> string = chr(226) + chr(152) + chr(133)

fn render_frame(
    snake_x: List[int], snake_y: List[int],
    food_x: int, food_y: int,
    score: int, state: int, high_score: int
) {
    print(sync_begin())

    -- Top border
    print(at(1, 1) + fg(90) + "+" + make_str("-", FIELD_W()) + "+" + rst())

    -- Game field
    let mut gy = 0
    while gy < FIELD_H() {
        print(at(gy + 2, 1) + fg(90) + "|" + rst())

        let mut gx = 0
        while gx < FIELD_W() {
            let mut drawn = false

            -- Snake head
            if drawn == false and gx == snake_x[0] and gy == snake_y[0] {
                print(bold() + fg(92) + snake_head_char() + rst())
                drawn = true
            }

            -- Snake body
            if drawn == false {
                let mut si = 1
                while si < snake_x.len() and drawn == false {
                    if gx == snake_x[si] and gy == snake_y[si] {
                        print(fg(32) + snake_body_char() + rst())
                        drawn = true
                    }
                    si = si + 1
                }
            }

            -- Food
            if drawn == false and gx == food_x and gy == food_y {
                print(bold() + fg(91) + food_char() + rst())
                drawn = true
            }

            -- Empty
            if drawn == false {
                print(" ")
            }

            gx = gx + 1
        }

        print(fg(90) + "|" + rst())
        gy = gy + 1
    }

    -- Bottom border
    print(at(FIELD_H() + 2, 1) + fg(90) + "+" + make_str("-", FIELD_W()) + "+" + rst())

    -- HUD
    print(at(FIELD_H() + 3, 2))
    print(fg(96) + "SCORE " + rst() + bold() + fg(97) + int_to_string(score) + rst())
    print("   " + fg(93) + "BEST " + rst() + fg(97) + int_to_string(high_score) + rst() + "      ")

    -- Title screen
    if state == 0 {
        let cy = FIELD_H() / 2 + 1
        let cx = FIELD_W() / 2 - 3
        print(at(cy - 1, cx) + bold() + fg(92) + "S N A K E" + rst())
        print(at(cy + 1, cx - 3) + fg(97) + "Press SPACE to play" + rst())
        print(at(cy + 3, cx - 2) + fg(90) + "WASD or arrows   " + rst())
        print(at(cy + 4, cx) + fg(90) + "Q = quit    " + rst())
    }

    -- Game over
    if state == 2 {
        let cy = FIELD_H() / 2 + 1
        let cx = FIELD_W() / 2 - 3
        print(at(cy - 1, cx) + bold() + fg(91) + "GAME  OVER" + rst())
        print(at(cy + 1, cx - 2) + fg(97) + "Score: " + bold() + fg(93) + int_to_string(score) + rst())
        print(at(cy + 3, cx - 3) + fg(90) + "SPACE=play  Q=quit" + rst())
    }

    print(sync_end())
}

-- ============================================================
--  Game logic
-- ============================================================

fn spawn_food(snake_x: List[int], snake_y: List[int], seed: int) -> List[int] {
    let mut s = seed
    let mut fx = 0
    let mut fy = 0
    let mut ok = false
    while ok == false {
        s = next_rand(s)
        fx = s % FIELD_W()
        s = next_rand(s)
        fy = s % FIELD_H()
        -- Make sure food doesn't land on snake
        let mut collision = false
        let mut i = 0
        while i < snake_x.len() {
            if snake_x[i] == fx and snake_y[i] == fy {
                collision = true
            }
            i = i + 1
        }
        if collision == false { ok = true }
    }
    -- Return [fx, fy, s] packed in a list
    let mut result: List[int] = []
    result.push(fx)
    result.push(fy)
    result.push(s)
    return result
}

-- ============================================================
--  Main
-- ============================================================

fn main() {
    let un1 = system("stty raw -echo")
    print(hide_cursor() + cls())

    -- State: 0=title, 1=playing, 2=game_over
    let mut state = 0
    let mut running = true
    let mut score = 0
    let mut high_score = 0
    let mut dir = DIR_RIGHT()
    let mut tick = 0
    let mut rng_seed = 42

    -- Snake body (head is index 0)
    let mut snake_x: List[int] = []
    let mut snake_y: List[int] = []

    -- Initial snake
    let start_x = FIELD_W() / 2
    let start_y = FIELD_H() / 2
    let mut si = 0
    while si < INITIAL_LEN() {
        snake_x.push(start_x - si)
        snake_y.push(start_y)
        si = si + 1
    }

    -- Initial food
    rng_seed = next_rand(time_ms() % 100000 + 7)
    let food_info = spawn_food(snake_x, snake_y, rng_seed)
    let mut food_x = food_info[0]
    let mut food_y = food_info[1]
    rng_seed = food_info[2]

    let spf = 1000 / 60
    let mem0 = mem_mark()

    while running {
        let t0 = time_ms()
        let inp = read_all_input()

        if inp.quit { running = false }

        if state == 0 {
            if inp.space {
                state = 1
                score = 0
                dir = DIR_RIGHT()
                tick = 0
                -- Reset snake
                snake_x = []
                snake_y = []
                si = 0
                while si < INITIAL_LEN() {
                    snake_x.push(start_x - si)
                    snake_y.push(start_y)
                    si = si + 1
                }
                -- New food
                rng_seed = next_rand(time_ms() % 100000 + 13)
                let fi = spawn_food(snake_x, snake_y, rng_seed)
                food_x = fi[0]
                food_y = fi[1]
                rng_seed = fi[2]
            }
        } else if state == 2 {
            if inp.space {
                state = 1
                score = 0
                dir = DIR_RIGHT()
                tick = 0
                snake_x = []
                snake_y = []
                si = 0
                while si < INITIAL_LEN() {
                    snake_x.push(start_x - si)
                    snake_y.push(start_y)
                    si = si + 1
                }
                rng_seed = next_rand(time_ms() % 100000 + 17)
                let fi2 = spawn_food(snake_x, snake_y, rng_seed)
                food_x = fi2[0]
                food_y = fi2[1]
                rng_seed = fi2[2]
            }
        } else {
            -- Playing: handle direction input
            if inp.has_dir {
                -- Prevent 180-degree turns
                let new_dir = inp.dir
                let ok = true
                if new_dir == DIR_UP() and dir == DIR_DOWN() { let ok = false }
                if new_dir == DIR_DOWN() and dir == DIR_UP() { let ok = false }
                if new_dir == DIR_LEFT() and dir == DIR_RIGHT() { let ok = false }
                if new_dir == DIR_RIGHT() and dir == DIR_LEFT() { let ok = false }
                if ok { dir = new_dir }
            }

            -- Move snake at fixed rate
            tick = tick + 1
            if tick >= TICKS_PER_MOVE() {
                tick = 0

                -- Compute new head position
                let head_x = snake_x[0]
                let head_y = snake_y[0]
                let mut nx = head_x
                let mut ny = head_y
                if dir == DIR_UP() { ny = ny - 1 }
                if dir == DIR_DOWN() { ny = ny + 1 }
                if dir == DIR_LEFT() { nx = nx - 1 }
                if dir == DIR_RIGHT() { nx = nx + 1 }

                -- Check wall collision
                if nx < 0 or nx >= FIELD_W() or ny < 0 or ny >= FIELD_H() {
                    state = 2
                    if score > high_score { high_score = score }
                }

                -- Check self collision
                if state == 1 {
                    let mut ci = 0
                    while ci < snake_x.len() {
                        if snake_x[ci] == nx and snake_y[ci] == ny {
                            state = 2
                            if score > high_score { high_score = score }
                        }
                        ci = ci + 1
                    }
                }

                -- Move if still alive
                if state == 1 {
                    -- Check food
                    let ate = nx == food_x and ny == food_y

                    -- Shift body: insert new head, remove tail (unless ate food)
                    -- Build new lists (head first)
                    let mut new_sx: List[int] = []
                    let mut new_sy: List[int] = []
                    new_sx.push(nx)
                    new_sy.push(ny)
                    let mut tail_limit = snake_x.len()
                    if ate == false { tail_limit = tail_limit - 1 }
                    let mut mi = 0
                    while mi < tail_limit {
                        new_sx.push(snake_x[mi])
                        new_sy.push(snake_y[mi])
                        mi = mi + 1
                    }
                    snake_x = new_sx
                    snake_y = new_sy

                    if ate {
                        score = score + 10
                        let fi3 = spawn_food(snake_x, snake_y, rng_seed)
                        food_x = fi3[0]
                        food_y = fi3[1]
                        rng_seed = fi3[2]
                    }
                }
            }
        }

        -- Render
        render_frame(snake_x, snake_y, food_x, food_y, score, state, high_score)
        flush()

        -- Free temporary strings
        mem_sweep(mem0)

        -- Frame cap ~60fps
        let dt = time_ms() - t0
        if dt < spf { sleep_ms(spf - dt) }
    }

    -- Cleanup
    print(show_cursor() + cls())
    let un2 = system("stty cooked echo")
    println("Thanks for playing! Final score: " + int_to_string(score))
}
