module text

import buffer
import json

-- ============================================================
-- Positions in a document
-- The protocol names a place by its line and character, both from 0,
-- where a character is a UTF-16 code unit. The server works with byte
-- offsets into the document's UTF-8 text. The compiler reports lines
-- and columns from 1, a column being a byte.
-- ============================================================

-- The offset where line `line` (from 0) starts; the end of the text
-- when it has fewer lines
fn line_start(text: string, line: int) -> int {
    let mut at = 0
    let mut current = 0
    while current < line and at < len(text) {
        if byte_at(text, at) == 10 {
            current = current + 1
        }
        at = at + 1
    }
    if current < line {
        return len(text)
    }
    return at
}

-- The offset where the line that contains offset `at` ends: its line
-- feed, or the carriage return before that, or the end of the text
fn line_end(text: string, at: int) -> int {
    let mut end = at
    while end < len(text) and byte_at(text, end) != 10 {
        end = end + 1
    }
    if end > at and byte_at(text, end - 1) == 13 {
        end = end - 1
    }
    return end
}

-- How many bytes the UTF-8 character that starts with byte `c` has
fn utf8_length(c: int) -> int {
    if c < 192 {
        return 1
    }
    if c < 224 {
        return 2
    }
    if c < 240 {
        return 3
    }
    return 4
}

-- The offset of character `character` of the line that starts at
-- `start`; the end of the line when it is shorter
fn offset_of_character(text: string, start: int, character: int) -> int {
    let end = line_end(text, start)
    let mut at = start
    let mut units = 0
    while at < end and units < character {
        let size = utf8_length(byte_at(text, at))
        -- A character of four bytes is two UTF-16 code units
        if size == 4 {
            units = units + 2
        } else {
            units = units + 1
        }
        at = at + size
    }
    if at > end {
        return end
    }
    return at
}

-- The character number of offset `at` in the line that starts at
-- `start`
fn character_of_offset(text: string, start: int, at: int) -> int {
    let mut i = start
    let mut units = 0
    while i < at and i < len(text) {
        let size = utf8_length(byte_at(text, i))
        if size == 4 {
            units = units + 2
        } else {
            units = units + 1
        }
        i = i + size
    }
    return units
}

-- The offset of the place the protocol calls (line, character)
fn offset_of_position(text: string, line: int, character: int) -> int {
    return offset_of_character(text, line_start(text, line), character)
}

-- Whether the byte can be part of a name
fn is_word_byte(c: int) -> bool {
    if c >= 97 and c <= 122 {
        return true
    }
    if c >= 65 and c <= 90 {
        return true
    }
    if c >= 48 and c <= 57 {
        return true
    }
    return c == 95
}

-- Where the name at offset `at` starts: a name the offset is in, or
-- one that ends right before it. -1 when there is no name there.
fn word_start(text: string, at: int) -> int {
    let mut start = at
    if start > len(text) {
        start = len(text)
    }
    if start >= len(text) or is_word_byte(byte_at(text, start)) == false {
        if start == 0 or is_word_byte(byte_at(text, start - 1)) == false {
            return -1
        }
    }
    while start > 0 and is_word_byte(byte_at(text, start - 1)) {
        start = start - 1
    }
    return start
}

-- Where the name that starts at `start` ends
fn word_end(text: string, start: int) -> int {
    let mut end = start
    while end < len(text) and is_word_byte(byte_at(text, end)) {
        end = end + 1
    }
    return end
}

-- The name at offset `at`, as a view of the text; "" when there is none
fn word_at(text: string, at: int) -> string {
    let start = word_start(text, at)
    if start < 0 {
        return ""
    }
    return view(text, start, word_end(text, start) - start)
}

-- For a scan of a line from its start: `state` is 0 in code, 1 in a
-- string and 2 right after a backslash in a string. Gives the state
-- after byte `c`, or 3 when `c` and the byte after it, `next`, start a
-- comment, which runs to the end of the line.
fn scan_state(state: int, c: int, next: int) -> int {
    if state == 2 {
        return 1
    }
    if state == 1 {
        if c == 92 {
            return 2
        }
        if c == 34 {
            return 0
        }
        return 1
    }
    if c == 34 {
        return 1
    }
    if c == 45 and next == 45 {
        return 3
    }
    return 0
}

-- The byte at `at`, or 0 past the end of the text
fn byte_or_zero(text: string, at: int) -> int {
    if at < 0 or at >= len(text) {
        return 0
    }
    return byte_at(text, at)
}

-- The offset of the first byte at or after `at` that is not a space
-- or a tab
fn skip_blanks(text: string, at: int) -> int {
    let mut i = at
    while i < len(text) and (byte_at(text, i) == 32 or byte_at(text, i) == 9) {
        i = i + 1
    }
    return i
}

-- The offset after the line feed that ends the line containing `at`;
-- the end of the text when no line feed follows
fn next_line(text: string, at: int) -> int {
    let mut i = at
    while i < len(text) {
        if byte_at(text, i) == 10 {
            return i + 1
        }
        i = i + 1
    }
    return i
}

-- The offset of the last `sub` in `s`, or -1
fn find_last(s: string, sub: string) -> int {
    let mut last = -1
    let mut from = 0
    while from <= len(s) {
        let found = string_find(view(s, from, len(s) - from), sub)
        if found < 0 {
            return last
        }
        last = from + found
        from = last + 1
    }
    return last
}

-- Whether the string is one or more digits
fn all_digits(s: string) -> bool {
    let mut i = 0
    while i < len(s) {
        if byte_at(s, i) < 48 or byte_at(s, i) > 57 {
            return false
        }
        i = i + 1
    }
    return len(s) > 0
}

-- ---- Lines of fields separated by tabs ----
-- (what the compiler's `symbols` command prints)

-- The offset of field `n`, from 0, of the line that starts at `line_at`;
-- the end of the line when it has fewer fields
fn tab_field_at(text: string, line_at: int, n: int) -> int {
    let mut at = line_at
    let mut field = 0
    while at < len(text) and field < n {
        let c = byte_at(text, at)
        if c == 10 {
            return at
        }
        if c == 9 {
            field = field + 1
        }
        at = at + 1
    }
    return at
}

-- Field `n` of the line that starts at `line_at`, as a view of the text
fn tab_field(text: string, line_at: int, n: int) -> string {
    let start = tab_field_at(text, line_at, n)
    let mut end = start
    while end < len(text) and byte_at(text, end) != 9 and byte_at(text, end) != 10 {
        end = end + 1
    }
    return view(text, start, end - start)
}

-- ============================================================
-- URIs
-- ============================================================

-- Add the path of a file: URI to the buffer, its %XX escapes replaced
-- by their bytes. Adds nothing, and gives false, for another kind of
-- URI.
fn uri_path_into(b: int, uri: string) -> bool {
    if starts_with(uri, "file://") == false {
        return false
    }
    -- file:///path, or file://host/path: the path starts at the next /
    let mut at = 7
    while at < len(uri) and byte_at(uri, at) != 47 {
        at = at + 1
    }
    while at < len(uri) {
        let c = byte_at(uri, at)
        let mut value = -1
        if c == 37 and at + 2 < len(uri) {
            let high = json_hex_value(byte_at(uri, at + 1))
            let low = json_hex_value(byte_at(uri, at + 2))
            if high >= 0 and low >= 0 {
                value = high * 16 + low
            }
        }
        if value >= 0 {
            buf_byte(b, value)
            at = at + 3
        } else {
            buf_byte(b, c)
            at = at + 1
        }
    }
    return true
}

-- Add the file: URI of an absolute path to the buffer
fn path_uri_into(b: int, path: string) {
    buf_add(b, "file://")
    let mut i = 0
    while i < len(path) {
        let c = byte_at(path, i)
        -- Letters, digits and / - . _ ~ stand for themselves
        if is_word_byte(c) or c == 47 or c == 45 or c == 46 or c == 126 {
            buf_byte(b, c)
        } else {
            buf_byte(b, 37)
            buf_byte(b, upper_hex_digit(c / 16))
            buf_byte(b, upper_hex_digit(c % 16))
        }
        i = i + 1
    }
}

fn upper_hex_digit(n: int) -> int {
    if n < 10 {
        return 48 + n
    }
    return 55 + n
}
