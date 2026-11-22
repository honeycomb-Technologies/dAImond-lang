module convert

-- ============================================================
-- Conversions between numbers, booleans and strings
-- int_to_string, float_to_string, bool_to_string, chr, parse_int and
-- parse_float. See runtime/string.dm for how rt_X functions
-- replace the C runtime and for the layout of a string.
--
-- Integers are formatted and parsed here. Floats go through
-- libc (snprintf "%g" and strtod), which round correctly.
-- ============================================================

extern fn libc_free(p: int) = "free"
extern fn libc_memmove(dst: int, src: int, n: int) -> int = "memmove"
extern fn libc_snprintf(buf: int, n: int, fmt: string, ...) -> i32 = "snprintf"
extern fn libc_strtod(s: int, end: int) -> float = "strtod"

-- int_to_string(n): n in decimal, with a leading "-" when negative
fn rt_int_to_string(n: int) -> string {
    -- The two digits of every number below 100, side by side
    let pairs = "00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899"
    -- Work on -|n|: every int has a negative counterpart, but the
    -- lowest int has no positive one
    let mut v = n
    if n > 0 {
        v = 0 - n
    }
    unsafe {
        let table = string_data(pairs)
        -- The longest result, "-9223372036854775808", is 20 bytes
        let size = 21
        let buf = libc_malloc(size)
        if buf == 0 {
            panic("out of memory")
        }
        -- Digits come out lowest first, two per division, so they are
        -- written from the end of the buffer backwards
        let end = buf + size - 1
        store_u8(end, 0)
        let mut at = end
        while v <= 0 - 100 {
            let rest = v / 100
            let pair = table + (rest * 100 - v) * 2
            at = at - 2
            store_u8(at, load_u8(pair))
            store_u8(at + 1, load_u8(pair + 1))
            v = rest
        }
        if v <= 0 - 10 {
            let pair = table - v * 2
            at = at - 2
            store_u8(at, load_u8(pair))
            store_u8(at + 1, load_u8(pair + 1))
        } else {
            at = at - 1
            store_u8(at, 48 - v)
        }
        if n < 0 {
            at = at - 1
            store_u8(at, 45)
        }
        -- Move the text, with its NUL, to the start of the buffer
        let length = end - at
        if at != buf {
            libc_memmove(buf, at, length + 1)
        }
        return string_from_raw(buf, length, size)
    }
}

-- An unsigned 64-bit integer in decimal: what print and int_to_string
-- make of a u64, whose values above the highest int have no int
fn rt_uint_to_string(n: u64) -> string {
    unsafe {
        -- The longest result, "18446744073709551615", is 20 bytes
        let size = 21
        let buf = libc_malloc(size)
        if buf == 0 {
            panic("out of memory")
        }
        let end = buf + size - 1
        store_u8(end, 0)
        let mut at = end
        let mut v = n
        while v >= 10 {
            let rest = v / 10
            at = at - 1
            store_u8(at, 48 + ((v - rest * 10) as int))
            v = rest
        }
        at = at - 1
        store_u8(at, 48 + (v as int))
        let length = end - at
        if at != buf {
            libc_memmove(buf, at, length + 1)
        }
        return string_from_raw(buf, length, size)
    }
}

-- float_to_string(f): f as C's "%g" prints it, with up to six
-- significant digits ("3.14159", "1e+20", "inf", "nan")
fn rt_float_to_string(f: float) -> string {
    unsafe {
        -- The longest "%g" output is 13 bytes, such as "-1.79769e+308"
        let size = 32
        let buf = libc_malloc(size)
        if buf == 0 {
            panic("out of memory")
        }
        let n = libc_snprintf(buf, size, "%g", f) as int
        if n < 0 or n >= size {
            libc_free(buf)
            return ""
        }
        return string_from_raw(buf, n, size)
    }
}

-- bool_to_string(b): "true" or "false"
fn rt_bool_to_string(b: bool) -> string {
    if b {
        return "true"
    }
    return "false"
}

-- chr(n): the one-byte string whose byte is n. Only the low 8 bits of
-- n count.
fn rt_chr(n: int) -> string {
    unsafe {
        let buf = rt_string_buffer(1)
        store_u8(buf, n)
        return string_from_raw(buf, 1, 2)
    }
}

-- Whether byte c is whitespace to C's isspace: space, or tab
-- through carriage return
fn rt_is_space(c: int) -> bool {
    return c == 32 or (c >= 9 and c <= 13)
}

-- parse_int(s): the decimal integer at the start of s, as C's
-- atoll reads it. Leading whitespace is skipped, a "+" or "-" may
-- come first, and the number ends at the first byte that is not a
-- digit. Without a digit the result is 0. A number outside the
-- range of int gives the nearest int.
fn rt_parse_int(s: string) -> int {
    let n = len(s)
    let int_max = 9223372036854775807
    -- The lowest int is lowest_tenth * 10 - 8
    let lowest_tenth = 0 - 922337203685477580
    unsafe {
        let p = string_data(s)
        let mut i = 0
        while i < n and rt_is_space(load_u8(p + i)) {
            i = i + 1
        }
        let mut negative = false
        if i < n {
            let c = load_u8(p + i)
            if c == 45 {
                negative = true
                i = i + 1
            } else if c == 43 {
                i = i + 1
            }
        }
        -- Accumulate -|value|, which can hold the lowest int
        let mut v = 0
        let mut in_range = true
        while i < n {
            let d = load_u8(p + i) - 48
            if d < 0 or d > 9 {
                i = n
            } else {
                if v < lowest_tenth or (v == lowest_tenth and d > 8) {
                    in_range = false
                    i = n
                } else {
                    v = v * 10 - d
                    i = i + 1
                }
            }
        }
        if negative {
            if in_range {
                return v
            }
            return 0 - int_max - 1
        }
        if in_range and v >= 0 - int_max {
            return 0 - v
        }
        return int_max
    }
}

-- parse_float(s): the number at the start of s, as C's strtod
-- reads it ("3.5", "-1e3", "inf"); 0.0 when s does not start with
-- a number. Leading whitespace is skipped.
fn rt_parse_float(s: string) -> float {
    let n = len(s)
    if n == 0 {
        return 0.0
    }
    unsafe {
        -- strtod reads up to a NUL byte, which s need not end with
        let buf = libc_malloc(n + 1)
        if buf == 0 {
            panic("out of memory")
        }
        libc_memcpy(buf, string_data(s), n)
        store_u8(buf + n, 0)
        let value = libc_strtod(buf, 0)
        libc_free(buf)
        return value
    }
}
