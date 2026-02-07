module terminal

-- ============================================================
--  Terminal control: ANSI escape codes + raw input
-- ============================================================

fn esc() -> string = chr(27)
fn csi() -> string = esc() + "["

-- Cursor and screen
fn at(r: int, c: int) -> string =
    csi() + int_to_string(r) + ";" + int_to_string(c) + "H"
fn cls() -> string = csi() + "2J" + at(1, 1)
fn hide_cursor() -> string = csi() + "?25l"
fn show_cursor() -> string = csi() + "?25h"

-- Synchronized output (batch frame updates, no flicker)
fn sync_begin() -> string = csi() + "?2026h"
fn sync_end() -> string = csi() + "?2026l"

-- Text style
fn rst() -> string = csi() + "0m"
fn bold() -> string = csi() + "1m"
fn fg(code: int) -> string = csi() + int_to_string(code) + "m"
fn bg(code: int) -> string = csi() + int_to_string(code) + "m"

-- Input
struct Input {
    left: int,
    right: int,
    space: bool,
    quit: bool
}

fn read_all_input() -> Input {
    let mut l = 0
    let mut r = 0
    let mut sp = false
    let mut q = false
    let mut k = read_key_nb()
    while k != 0 - 1 {
        if k == 113 or k == 81 { q = true }
        if k == 32 { sp = true }
        if k == 97 { l = l + 1 }
        if k == 100 { r = r + 1 }
        if k == 27 {
            let k2 = read_key_nb()
            if k2 == 91 {
                let k3 = read_key_nb()
                if k3 == 68 { l = l + 1 }
                if k3 == 67 { r = r + 1 }
            }
        }
        k = read_key_nb()
    }
    return Input { left: l, right: r, space: sp, quit: q }
}

fn enter_raw_mode() -> int = system("stty raw -echo")
fn exit_raw_mode() -> int = system("stty cooked echo")
