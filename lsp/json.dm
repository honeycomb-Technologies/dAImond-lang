module json

import buffer

-- ============================================================
-- Reading JSON
-- A value is read where it lies in the text: these functions take the
-- text and the position of a value in it, and give positions of the
-- values inside. Nothing is copied but the strings that are asked for,
-- which go into a buffer (see buffer.dm).
--
-- The text is trusted to be JSON only as far as each function reads
-- it: none of them reads past the end of the text, and a position of
-- -1, which means "no such value", may be given to all of them.
-- ============================================================

-- The position of the first byte at or after `i` that is not white
-- space
fn json_skip_space(s: string, i: int) -> int {
    let mut at = i
    while at < len(s) {
        let c = byte_at(s, at)
        if c != 32 and c != 10 and c != 13 and c != 9 {
            return at
        }
        at = at + 1
    }
    return at
}

-- The position after the string that starts with the quote at `i`
fn json_string_end(s: string, i: int) -> int {
    let mut at = i + 1
    while at < len(s) {
        let c = byte_at(s, at)
        if c == 92 {
            at = at + 1
        } else if c == 34 {
            return at + 1
        }
        at = at + 1
    }
    return len(s)
}

-- The position after the value at `i`
fn json_end(s: string, i: int) -> int {
    if i < 0 or i >= len(s) {
        return len(s)
    }
    let first = byte_at(s, i)
    if first == 34 {
        return json_string_end(s, i)
    }
    if first == 123 or first == 91 {
        -- An object or an array: to the bracket that closes it
        let mut depth = 0
        let mut at = i
        while at < len(s) {
            let c = byte_at(s, at)
            if c == 34 {
                at = json_string_end(s, at)
            } else {
                if c == 123 or c == 91 {
                    depth = depth + 1
                } else if c == 125 or c == 93 {
                    depth = depth - 1
                    if depth == 0 {
                        return at + 1
                    }
                }
                at = at + 1
            }
        }
        return len(s)
    }
    -- A number, true, false or null: to the next , } ] or white space
    let mut at = i
    while at < len(s) {
        let c = byte_at(s, at)
        if c == 44 or c == 125 or c == 93 or c == 32 or c == 10 or c == 13 or c == 9 {
            return at
        }
        at = at + 1
    }
    return at
}

-- Whether the string at `i` is exactly `text`. A string with escapes
-- in it is compared as written, so `text` must need none.
fn json_string_is(s: string, i: int, text: string) -> bool {
    if i < 0 or i >= len(s) or byte_at(s, i) != 34 {
        return false
    }
    let end = json_string_end(s, i)
    if end - i - 2 != len(text) {
        return false
    }
    return view(s, i + 1, len(text)) == text
}

-- The position of the value of member `key` of the object at `obj`;
-- -1 when `obj` is not an object or has no such member
fn json_member(s: string, obj: int, key: string) -> int {
    if obj < 0 or obj >= len(s) or byte_at(s, obj) != 123 {
        return -1
    }
    let mut at = json_skip_space(s, obj + 1)
    while at < len(s) and byte_at(s, at) == 34 {
        let key_at = at
        at = json_skip_space(s, json_string_end(s, at))
        if at >= len(s) or byte_at(s, at) != 58 {
            return -1
        }
        at = json_skip_space(s, at + 1)
        if json_string_is(s, key_at, key) {
            return at
        }
        at = json_skip_space(s, json_end(s, at))
        if at < len(s) and byte_at(s, at) == 44 {
            at = json_skip_space(s, at + 1)
        }
    }
    return -1
}

-- The position of item `n`, counted from 0, of the array at `arr`; -1
-- when `arr` is not an array or has fewer items
fn json_item(s: string, arr: int, n: int) -> int {
    if arr < 0 or arr >= len(s) or byte_at(s, arr) != 91 {
        return -1
    }
    let mut at = json_skip_space(s, arr + 1)
    let mut index = 0
    while at < len(s) and byte_at(s, at) != 93 {
        if index == n {
            return at
        }
        at = json_skip_space(s, json_end(s, at))
        if at < len(s) and byte_at(s, at) == 44 {
            at = json_skip_space(s, at + 1)
        }
        index = index + 1
    }
    return -1
}

-- How many items the array at `arr` has; 0 when it is not an array
fn json_count(s: string, arr: int) -> int {
    if arr < 0 or arr >= len(s) or byte_at(s, arr) != 91 {
        return 0
    }
    let mut at = json_skip_space(s, arr + 1)
    let mut count = 0
    while at < len(s) and byte_at(s, at) != 93 {
        count = count + 1
        at = json_skip_space(s, json_end(s, at))
        if at < len(s) and byte_at(s, at) == 44 {
            at = json_skip_space(s, at + 1)
        }
    }
    return count
}

-- The whole number at `i`: its digits up to a fraction or exponent.
-- `fallback` when there is no number there.
fn json_int(s: string, i: int, fallback: int) -> int {
    if i < 0 or i >= len(s) {
        return fallback
    }
    let mut at = i
    let negative = byte_at(s, at) == 45
    if negative {
        at = at + 1
    }
    if at >= len(s) or byte_at(s, at) < 48 or byte_at(s, at) > 57 {
        return fallback
    }
    let mut n = 0
    while at < len(s) and byte_at(s, at) >= 48 and byte_at(s, at) <= 57 {
        -- Stop before the number would overflow
        if n > 100000000000000000 {
            return fallback
        }
        n = n * 10 + (byte_at(s, at) - 48)
        at = at + 1
    }
    if negative {
        return 0 - n
    }
    return n
}

-- The value of the hexadecimal digit `c`, or -1
fn json_hex_value(c: int) -> int {
    if c >= 48 and c <= 57 {
        return c - 48
    }
    if c >= 97 and c <= 102 {
        return c - 87
    }
    if c >= 65 and c <= 70 {
        return c - 55
    }
    return -1
}

-- The number written as four hexadecimal digits at `i`, or -1
fn json_hex4(s: string, i: int) -> int {
    if i + 4 > len(s) {
        return -1
    }
    let mut n = 0
    let mut k = 0
    while k < 4 {
        let digit = json_hex_value(byte_at(s, i + k))
        if digit < 0 {
            return -1
        }
        n = n * 16 + digit
        k = k + 1
    }
    return n
}

-- Add the character with code `code` to the buffer, as UTF-8
fn buf_utf8(b: int, code: int) {
    if code < 128 {
        buf_byte(b, code)
    } else if code < 2048 {
        buf_byte(b, 192 + code / 64)
        buf_byte(b, 128 + code % 64)
    } else if code < 65536 {
        buf_byte(b, 224 + code / 4096)
        buf_byte(b, 128 + (code / 64) % 64)
        buf_byte(b, 128 + code % 64)
    } else {
        buf_byte(b, 240 + code / 262144)
        buf_byte(b, 128 + (code / 4096) % 64)
        buf_byte(b, 128 + (code / 64) % 64)
        buf_byte(b, 128 + code % 64)
    }
}

-- Add the text of the string at `i` to the buffer `b`, with its
-- escapes replaced by what they stand for. Adds nothing when there is
-- no string at `i`; gives whether there was one.
fn json_string_into(b: int, s: string, i: int) -> bool {
    if i < 0 or i >= len(s) or byte_at(s, i) != 34 {
        return false
    }
    let end = json_string_end(s, i) - 1
    buf_reserve(b, end - i)
    let mut at = i + 1
    -- Bytes that need no change are added a run at a time
    let mut run = at
    while at < end {
        if byte_at(s, at) != 92 or at + 1 >= end {
            at = at + 1
        } else {
            buf_add_part(b, s, run, at - run)
            let c = byte_at(s, at + 1)
            at = at + 2
            if c == 110 {
                buf_byte(b, 10)
            } else if c == 116 {
                buf_byte(b, 9)
            } else if c == 114 {
                buf_byte(b, 13)
            } else if c == 98 {
                buf_byte(b, 8)
            } else if c == 102 {
                buf_byte(b, 12)
            } else if c == 117 {
                let mut code = json_hex4(s, at)
                if code < 0 {
                    -- Not four digits: U+FFFD, the replacement character
                    code = 65533
                } else {
                    at = at + 4
                    -- Two \u escapes, a high and a low surrogate, are
                    -- one character above U+FFFF
                    if code >= 55296 and code < 56320 and at + 1 < end and byte_at(s, at) == 92 and byte_at(s, at + 1) == 117 {
                        let low = json_hex4(s, at + 2)
                        if low >= 56320 and low < 57344 {
                            code = 65536 + (code - 55296) * 1024 + (low - 56320)
                            at = at + 6
                        }
                    }
                    if code >= 55296 and code < 57344 {
                        -- Half of a pair on its own is no character
                        code = 65533
                    }
                }
                buf_utf8(b, code)
            } else {
                -- \" \\ \/ stand for the character itself
                buf_byte(b, c)
            }
            run = at
        }
    }
    buf_add_part(b, s, run, end - run)
    return true
}
