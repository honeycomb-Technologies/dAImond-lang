module lookup

import buffer
import json
import text
import sources
import transport

-- ============================================================
-- Finding names in a document: the variables of a function, and the
-- declarations a name can stand for
--
-- The compiler tells the server the declarations of a file (`daimond
-- symbols`). What is inside a function, the server reads from the text
-- itself, by its shape: `let name`, `for name in`, a parameter, a name
-- bound by a match arm.
-- ============================================================

-- ---- Scanning a text for names ----

-- Start a scan at offset `at`, which must be in code: not in a string
-- or a comment
fn scan_begin(server: Server, at: int) {
    set_cell(server.state + STATE_SCAN_AT(), at)
    set_cell(server.state + STATE_SCAN_MODE(), 0)
}

-- The offset of the next name in code before `limit`, or -1. Names in
-- strings and comments are passed over. word_end gives where it ends.
fn scan_word(server: Server, text: string, limit: int) -> int {
    let mut at = cell(server.state + STATE_SCAN_AT())
    let mut mode = cell(server.state + STATE_SCAN_MODE())
    let mut end = limit
    if end > len(text) {
        end = len(text)
    }
    while at < end {
        let c = byte_at(text, at)
        if mode == 3 {
            -- A comment ends with its line
            if c == 10 {
                mode = 0
            }
            at = at + 1
        } else if mode != 0 {
            mode = scan_state(mode, c, 0)
            at = at + 1
        } else if is_word_byte(c) {
            let stop = word_end(text, at)
            set_cell(server.state + STATE_SCAN_AT(), stop)
            set_cell(server.state + STATE_SCAN_MODE(), 0)
            return at
        } else {
            mode = scan_state(0, c, byte_or_zero(text, at + 1))
            at = at + 1
        }
    }
    set_cell(server.state + STATE_SCAN_AT(), at)
    set_cell(server.state + STATE_SCAN_MODE(), mode)
    return -1
}

-- The offset of the next `name` in code before `limit`, or -1
fn scan_next(server: Server, text: string, name: string, limit: int) -> int {
    let mut at = scan_word(server, text, limit)
    while at >= 0 {
        let count = word_end(text, at) - at
        if count == len(name) and view(text, at, count) == name {
            return at
        }
        at = scan_word(server, text, limit)
    }
    return -1
}

-- The offset of the first byte `a` or `b` in code from `from` to
-- `limit`, or -1. `from` must be in code.
fn find_in_code(text: string, from: int, limit: int, a: int, b: int) -> int {
    let mut mode = 0
    let mut at = from
    while at < limit and at < len(text) {
        let c = byte_at(text, at)
        if mode == 3 {
            if c == 10 {
                mode = 0
            }
        } else {
            mode = scan_state(mode, c, byte_or_zero(text, at + 1))
            if mode == 0 and (c == a or c == b) {
                return at
            }
        }
        at = at + 1
    }
    return -1
}

-- The offset of the last byte before `at` that is not a space or a
-- tab, nor a line end when `across_lines`; -1 when there is none
fn before_blanks(text: string, at: int, across_lines: bool) -> int {
    let mut i = at - 1
    while i >= 0 {
        let c = byte_at(text, i)
        if c == 32 or c == 9 {
            i = i - 1
        } else if across_lines and (c == 10 or c == 13) {
            i = i - 1
        } else {
            return i
        }
    }
    return -1
}

-- The name whose last byte is at offset `last`; "" when there is none
fn word_ending_at(text: string, last: int) -> string {
    if last < 0 or last >= len(text) or is_word_byte(byte_at(text, last)) == false {
        return ""
    }
    let mut start = last
    while start > 0 and is_word_byte(byte_at(text, start - 1)) {
        start = start - 1
    }
    return view(text, start, last + 1 - start)
}

-- Whether the name at `start` is written after a `.` or `::`, as a
-- field, method or variant is
fn is_member(text: string, start: int) -> bool {
    if start > 0 and byte_at(text, start - 1) == 46 {
        return true
    }
    return start > 1 and byte_at(text, start - 1) == 58 and byte_at(text, start - 2) == 58
}

-- The name before the `.` or `::` that the name at `start` follows;
-- "" when there is none
fn member_owner(text: string, start: int) -> string {
    if start > 0 and byte_at(text, start - 1) == 46 {
        return word_ending_at(text, start - 2)
    }
    if start > 1 and byte_at(text, start - 1) == 58 and byte_at(text, start - 2) == 58 {
        return word_ending_at(text, start - 3)
    }
    return ""
}

-- ---- The variables of a function ----

-- Whether the name of `count` bytes at offset `at` declares a variable
-- there: 0 when it does not, 1 for a `let`, the variable of a `for` or
-- a name bound by a match arm, 2 for a parameter. `body_start` is
-- where the body of the function around it starts.
fn declaration_kind(text: string, at: int, count: int, body_start: int) -> int {
    let prev = before_blanks(text, at, false)
    let before = word_ending_at(text, prev)
    let after = skip_blanks(text, at + count)
    let next = byte_or_zero(text, after)
    -- What is before a `mut`
    let mut_prev = before_blanks(text, prev - 2, true)

    if before == "let" {
        return 1
    }
    if before == "mut" and word_ending_at(text, mut_prev) == "let" {
        return 1
    }
    if before == "for" and next == 105 and byte_or_zero(text, after + 1) == 110 and is_word_byte(byte_or_zero(text, after + 2)) == false {
        return 1
    }
    if at < body_start {
        -- A parameter: after `(` or `,`, and before `:`
        let mut opener = byte_or_zero(text, before_blanks(text, at, true))
        if before == "mut" {
            opener = byte_or_zero(text, mut_prev)
        }
        if next == 58 and (opener == 40 or opener == 44) {
            return 2
        }
        return 0
    }
    -- A name bound by a match arm is before the arm's `=>`
    let end = line_end(text, at)
    let arrow = string_find(view(text, at + count, end - at - count), "=>")
    if arrow < 0 {
        return 0
    }
    let opener = byte_or_zero(text, prev)
    -- Variant(name) => ... and Variant(a, name) => ...
    if (opener == 40 or opener == 44) and (next == 41 or next == 44) {
        return 1
    }
    -- name => ..., where a name that starts with a capital is a variant
    let first = byte_at(text, at)
    if (prev < 0 or opener == 10) and at + count + arrow == after and first >= 97 and first <= 122 {
        return 1
    }
    return 0
}

-- The function around a place in a document
struct Scope {
    found: bool,
    -- The offsets where the function starts, where its body starts,
    -- and of the line after its last
    start: int,
    body_start: int,
    end: int
}

-- The function around offset `at` of the text of the source
fn scope_at(server: Server, src: int, text: string, at: int) -> Scope {
    let symbols = buf_view(source_symbols(server, src))
    let line_at = enclosing_function_at(symbols, line_of_offset(text, at) + 1)
    if line_at < 0 {
        return Scope { found: false, start: 0, body_start: 0, end: 0 }
    }
    let first = json_int(symbols, tab_field_at(symbols, line_at, 2), 1)
    let last = json_int(symbols, tab_field_at(symbols, line_at, 4), first)
    let start = line_start(text, first - 1)
    let end = line_start(text, last)
    -- The body starts at the first `{`, or at the `=` of
    -- fn name() -> T = expr
    let mut body_start = find_in_code(text, start, end, 123, 61)
    if body_start < 0 {
        body_start = end
    }
    return Scope { found: true, start: start, body_start: body_start, end: end }
}

-- A variable of a function
struct Local {
    found: bool,
    -- The offset of its name where it is declared
    at: int,
    is_param: bool
}

-- The variable that `name` stands for at offset `cursor`, in the
-- function `scope`: the nearest declaration of the name before the end
-- of the cursor's line
fn find_local(server: Server, text: string, scope: Scope, name: string, cursor: int) -> Local {
    let mut best = Local { found: false, at: 0, is_param: false }
    if scope.found == false or len(name) == 0 {
        return best
    }
    let mut limit = next_line(text, cursor)
    if limit > scope.end {
        limit = scope.end
    }
    scan_begin(server, scope.start)
    let mut at = scan_next(server, text, name, limit)
    while at >= 0 {
        let kind = declaration_kind(text, at, len(name), scope.body_start)
        if kind != 0 {
            best = Local { found: true, at: at, is_param: kind == 2 }
        }
        at = scan_next(server, text, name, limit)
    }
    return best
}

-- ---- Declarations ----

-- Make Server.found the declarations that the name at offset `start`
-- of the source's text can stand for, and give how many there are.
-- After a `.`, they are the fields, methods and variants of that name,
-- of the type before the `.` when it names one; elsewhere, what is
-- declared at the top of the file or of a file it imports.
fn collect_matches(server: Server, src: int, text: string, name: string, start: int) -> int {
    let found = server.found
    buf_reset(found)
    collect_sources(server, src)
    let mut class = 1
    if is_member(text, start) {
        class = 2
        let owner = member_owner(text, start)
        let mut m = next_match(server, name, 2, no_match())
        while m.found {
            let symbols = buf_view(cell(m.src + SRC_SYMBOLS()))
            if len(owner) > 0 and tab_field(symbols, m.line_at, 5) == owner {
                buf_i64(found, m.src)
                buf_i64(found, m.line_at)
            }
            m = next_match(server, name, 2, m)
        }
        if buf_len(found) > 0 {
            return buf_len(found) / 16
        }
    }
    -- Two turns: the class the name is written as, then the other
    let mut turn = 0
    while turn < 2 and buf_len(found) == 0 {
        let mut m = next_match(server, name, class, no_match())
        while m.found {
            buf_i64(found, m.src)
            buf_i64(found, m.line_at)
            m = next_match(server, name, class, m)
        }
        class = 3 - class
        turn = turn + 1
    }
    return buf_len(found) / 16
}

-- ---- Writing places ----

-- Add the Location of the `count` bytes at offset `at` of the text of
-- the source
fn add_location(out: int, src: int, text: string, at: int, count: int) {
    buf_add(out, "{\"uri\":")
    buf_json(out, buf_view(cell(src + SRC_URI())))
    buf_add(out, ",\"range\":")
    add_range_of(out, text, line_of_offset(text, at), at, at + count)
    buf_add(out, "}")
}

-- Add the Location of the declaration whose line starts at `line_at`
-- in the symbols of the source: its name, or the file for an import
fn add_symbol_location(server: Server, out: int, src: int, line_at: int) {
    let symbols = buf_view(cell(src + SRC_SYMBOLS()))
    if tab_field(symbols, line_at, 0) == "import" {
        let file = source_of_path(server, tab_field(symbols, line_at, 6))
        buf_add(out, "{\"uri\":")
        buf_json(out, buf_view(cell(file + SRC_URI())))
        buf_add(out, ",\"range\":")
        add_range(out, 0, 0, 0, 0)
        buf_add(out, "}")
        return
    }
    let line = json_int(symbols, tab_field_at(symbols, line_at, 2), 1) - 1
    let column = json_int(symbols, tab_field_at(symbols, line_at, 3), 1) - 1
    let text = source_text(server, src)
    add_location(out, src, text, line_start(text, line) + column, len(tab_field(symbols, line_at, 1)))
}

-- Add the comment lines right above line `line` (from 0) of the text,
-- without their `--`, as the description of what that line declares.
-- Lines that only rule off a section are left out.
fn add_comment_above(note: int, text: string, line: int) {
    let mut first = line
    while first > 0 {
        let at = skip_blanks(text, line_start(text, first - 1))
        if byte_or_zero(text, at) != 45 or byte_or_zero(text, at + 1) != 45 {
            break
        }
        first = first - 1
    }
    let mut wrote = false
    let mut i = first
    while i < line {
        let mut at = skip_blanks(text, line_start(text, i)) + 2
        let end = line_end(text, at)
        if at < end and byte_at(text, at) == 32 {
            at = at + 1
        }
        -- A rule is a line of = or -
        let mut is_rule = end - at > 3
        let mut k = at
        while k < end and is_rule {
            if byte_at(text, k) != 61 and byte_at(text, k) != 45 {
                is_rule = false
            }
            k = k + 1
        }
        if is_rule == false {
            if wrote == false {
                buf_add(note, "\n\n")
            } else {
                buf_add(note, "\n")
            }
            buf_add_part(note, text, at, end - at)
            wrote = true
        }
        i = i + 1
    }
}
