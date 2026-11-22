module string

-- ============================================================
-- Strings
-- The string functions behind every program Stage 5 compiles.
-- gen_bootstrap.dm embeds this file in the compiler (prelude.dm),
-- which adds it to each program's declarations.
--
-- A string is { data, len, cap }: the address of its bytes, their
-- count, and the size of the allocation that holds them. cap is 0
-- when the string does not own its bytes, as a literal doesn't.
-- Every string built here is followed by a NUL byte, so its data
-- can go to C as a char*.
--
-- The compiler calls rt_X wherever its IR names the runtime
-- function dm_X (see RUNTIME FUNCTIONS in llvm_gen.dm), so each
-- rt_X keeps the signature that dm_X is declared with in
-- ir_gen_decl.dm. len(s) and string literals are inline and call
-- nothing. Code here must not use the string operators it
-- implements: no +, == or < on strings, and no char_at or substr.
-- ============================================================

-- libc, under names that a program's own extern declarations of
-- the same C functions cannot collide with
extern fn libc_malloc(n: int) -> int = "malloc"
extern fn libc_memcpy(dst: int, src: int, n: int) -> int = "memcpy"
extern fn libc_memcmp(a: int, b: int, n: int) -> i32 = "memcmp"
extern fn libc_memmem(hay: int, hay_len: int, needle: int, needle_len: int) -> int = "memmem"

-- Room for a string of n bytes: the address of n + 1 bytes, the last
-- of them the NUL
fn rt_string_buffer(n: int) -> int {
    unsafe {
        let buf = libc_malloc(n + 1)
        if buf == 0 {
            panic("out of memory")
        }
        store_u8(buf + n, 0)
        return buf
    }
}

-- A new string holding the n bytes at address src
fn rt_string_copy(src: int, n: int) -> string {
    if n <= 0 {
        return ""
    }
    unsafe {
        let buf = rt_string_buffer(n)
        libc_memcpy(buf, src, n)
        return string_from_raw(buf, n, n + 1)
    }
}

-- a + b
fn rt_string_concat(a: string, b: string) -> string {
    let na = len(a)
    let nb = len(b)
    let n = na + nb
    if n == 0 {
        return ""
    }
    unsafe {
        let buf = rt_string_buffer(n)
        if na > 0 {
            libc_memcpy(buf, string_data(a), na)
        }
        if nb > 0 {
            libc_memcpy(buf + na, string_data(b), nb)
        }
        return string_from_raw(buf, n, n + 1)
    }
}

-- a == b
fn rt_string_eq(a: string, b: string) -> bool {
    let n = len(a)
    if n != len(b) {
        return false
    }
    if n == 0 {
        return true
    }
    unsafe {
        return libc_memcmp(string_data(a), string_data(b), n) == 0
    }
}

-- Byte order of a and b: negative when a sorts first, 0 when they
-- are equal, positive when b sorts first. A prefix sorts before
-- the longer string.
fn rt_string_cmp(a: string, b: string) -> int {
    let na = len(a)
    let nb = len(b)
    let mut n = na
    if nb < na {
        n = nb
    }
    if n > 0 {
        unsafe {
            let c = libc_memcmp(string_data(a), string_data(b), n)
            if c != 0 {
                return c as int
            }
        }
    }
    if na < nb {
        return -1
    }
    if na > nb {
        return 1
    }
    return 0
}

-- char_at(s, i): the byte at index i as a one-byte string, or ""
-- when i is outside s
fn rt_char_at(s: string, i: int) -> string {
    if i < 0 or i >= len(s) {
        return ""
    }
    unsafe {
        return rt_string_copy(string_data(s) + i, 1)
    }
}

-- substr(s, start, length): up to length bytes of s from index
-- start. The range is clamped to s: a start outside s or a length
-- below 1 gives "", and a length past the end stops at the end.
fn rt_substr(s: string, start: int, length: int) -> string {
    let n = len(s)
    if start < 0 or start >= n {
        return ""
    }
    let mut count = length
    if count > n - start {
        count = n - start
    }
    unsafe {
        return rt_string_copy(string_data(s) + start, count)
    }
}

-- ---- Search ----

-- The index of the first needle in s at or after index from, or -1
-- when there is none. An empty needle is found at from.
fn rt_string_index(s: string, needle: string, from: int) -> int {
    let n = len(s)
    let m = len(needle)
    if m == 0 {
        return from
    }
    if from + m > n {
        return -1
    }
    unsafe {
        let base = string_data(s)
        let hit = libc_memmem(base + from, n - from, string_data(needle), m)
        if hit == 0 {
            return -1
        }
        return hit - base
    }
}

-- string_contains(s, sub)
fn rt_string_contains(s: string, sub: string) -> bool {
    return rt_string_index(s, sub, 0) >= 0
}

-- string_find(s, sub): the index of the first sub in s, or -1
fn rt_string_find(s: string, sub: string) -> int {
    return rt_string_index(s, sub, 0)
}

fn rt_starts_with(s: string, prefix: string) -> bool {
    let m = len(prefix)
    if m > len(s) {
        return false
    }
    if m == 0 {
        return true
    }
    unsafe {
        return libc_memcmp(string_data(s), string_data(prefix), m) == 0
    }
}

fn rt_ends_with(s: string, suffix: string) -> bool {
    let n = len(s)
    let m = len(suffix)
    if m > n {
        return false
    }
    if m == 0 {
        return true
    }
    unsafe {
        return libc_memcmp(string_data(s) + n - m, string_data(suffix), m) == 0
    }
}

-- ---- Transformation ----

-- Whether byte c is a space, tab, newline or carriage return
fn rt_is_space_byte(c: int) -> bool {
    return c == 32 or c == 9 or c == 10 or c == 13
}

-- string_trim(s): s without the whitespace at its ends
fn rt_string_trim(s: string) -> string {
    let n = len(s)
    unsafe {
        let data = string_data(s)
        let mut start = 0
        while start < n and rt_is_space_byte(load_u8(data + start)) {
            start = start + 1
        }
        let mut end = n
        while end > start and rt_is_space_byte(load_u8(data + end - 1)) {
            end = end - 1
        }
        if start == 0 and end == n {
            return s
        }
        return rt_string_copy(data + start, end - start)
    }
}

-- string_replace(s, pattern, replacement): s with each pattern
-- replaced, found from the left without overlap. An empty pattern
-- replaces nothing.
fn rt_string_replace(s: string, pattern: string, replacement: string) -> string {
    let n = len(s)
    let m = len(pattern)
    let r = len(replacement)
    if m == 0 {
        return s
    }
    let mut count = 0
    let mut at = rt_string_index(s, pattern, 0)
    while at >= 0 {
        count = count + 1
        at = rt_string_index(s, pattern, at + m)
    }
    if count == 0 {
        return s
    }
    let out_len = n + count * (r - m)
    if out_len == 0 {
        return ""
    }
    unsafe {
        let src = string_data(s)
        let buf = rt_string_buffer(out_len)
        let mut from = 0
        let mut pos = 0
        at = rt_string_index(s, pattern, 0)
        while at >= 0 {
            libc_memcpy(buf + pos, src + from, at - from)
            pos = pos + at - from
            libc_memcpy(buf + pos, string_data(replacement), r)
            pos = pos + r
            from = at + m
            at = rt_string_index(s, pattern, from)
        }
        libc_memcpy(buf + pos, src + from, n - from)
        return string_from_raw(buf, out_len, out_len + 1)
    }
}

-- A copy of s in which each byte from first to last is moved by shift
fn rt_string_shift_bytes(s: string, first: int, last: int, shift: int) -> string {
    let n = len(s)
    if n == 0 {
        return ""
    }
    unsafe {
        let src = string_data(s)
        let buf = rt_string_buffer(n)
        let mut i = 0
        while i < n {
            let mut c = load_u8(src + i)
            if c >= first and c <= last {
                c = c + shift
            }
            store_u8(buf + i, c)
            i = i + 1
        }
        return string_from_raw(buf, n, n + 1)
    }
}

-- string_to_upper(s): a to z become A to Z; other bytes stay
fn rt_string_to_upper(s: string) -> string {
    return rt_string_shift_bytes(s, 97, 122, -32)
}

-- string_to_lower(s): A to Z become a to z; other bytes stay
fn rt_string_to_lower(s: string) -> string {
    return rt_string_shift_bytes(s, 65, 90, 32)
}

-- string_split(s, delim): the parts of s between delimiters, written
-- to the List[string] whose header is at address out. An empty s has
-- no parts, and an empty delimiter splits s into its bytes.
fn rt_string_split(out: int, s: string, delim: string) {
    let n = len(s)
    let m = len(delim)
    unsafe {
        store_i64(out, 0)
        store_i64(out + 8, 0)
        store_i64(out + 16, 0)
        if n == 0 {
            return
        }
        let src = string_data(s)
        if m == 0 {
            let mut i = 0
            while i < n {
                rt_list_string_push(out, rt_string_copy(src + i, 1))
                i = i + 1
            }
            return
        }
        let mut start = 0
        let mut at = rt_string_index(s, delim, 0)
        while at >= 0 {
            rt_list_string_push(out, rt_string_copy(src + start, at - start))
            start = at + m
            at = rt_string_index(s, delim, start)
        }
        rt_list_string_push(out, rt_string_copy(src + start, n - start))
    }
}

-- ---- Character classes: each looks at the first byte of s ----

-- The first byte of s, or -1 when s is empty
fn rt_first_byte(s: string) -> int {
    if len(s) == 0 {
        return -1
    }
    unsafe {
        return load_u8(string_data(s))
    }
}

fn rt_is_alpha(s: string) -> bool {
    let c = rt_first_byte(s)
    return (c >= 97 and c <= 122) or (c >= 65 and c <= 90)
}

fn rt_is_digit(s: string) -> bool {
    let c = rt_first_byte(s)
    return c >= 48 and c <= 57
}

fn rt_is_whitespace(s: string) -> bool {
    return rt_is_space_byte(rt_first_byte(s))
}

fn rt_is_alnum(s: string) -> bool {
    return rt_is_alpha(s) or rt_is_digit(s)
}

-- char_to_string(c): a character is already a one-byte string
fn rt_char_to_string(s: string) -> string {
    return s
}

-- ---- Memory ----
-- Strings are never freed: a string lives until the program ends.
-- mem_mark and mem_sweep exist for programs written when a sweep
-- freed the strings made since the mark, which also freed strings
-- still in use.

-- mem_mark(): always 0
fn rt_mem_mark() -> int {
    return 0
}

-- mem_sweep(mark): does nothing
fn rt_mem_sweep(mark: int) {
}
