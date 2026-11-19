module string

-- ============================================================
-- Strings
-- The string functions behind every program Stage 5 compiles.
-- gen_bootstrap.sh embeds this file in the compiler (prelude.dm),
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

-- A new string holding the n bytes at address src
fn rt_string_copy(src: int, n: int) -> string {
    if n <= 0 {
        return ""
    }
    unsafe {
        let buf = libc_malloc(n + 1)
        if buf == 0 {
            panic("out of memory")
        }
        libc_memcpy(buf, src, n)
        store_u8(buf + n, 0)
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
        let buf = libc_malloc(n + 1)
        if buf == 0 {
            panic("out of memory")
        }
        if na > 0 {
            libc_memcpy(buf, string_data(a), na)
        }
        if nb > 0 {
            libc_memcpy(buf + na, string_data(b), nb)
        }
        store_u8(buf + n, 0)
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
