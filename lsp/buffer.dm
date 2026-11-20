module buffer

-- ============================================================
-- Byte buffers
-- A buffer is a run of bytes that grows as bytes are added, for
-- building a long string without copying it at every step. A string
-- is never freed, so a program that runs for long, as the language
-- server does, builds its texts in buffers, which it frees or reuses.
--
-- A buffer is the address of its header, an int, so adding to it
-- needs no `let mut` and no returned copy:
--   +0   the address of the bytes
--   +8   how many bytes it holds
--   +16  how many bytes it has room for
-- ============================================================

extern fn buf_c_malloc(n: int) -> int = "malloc"
extern fn buf_c_realloc(p: int, n: int) -> int = "realloc"
extern fn buf_c_free(p: int) = "free"
extern fn buf_c_calloc(n: int, size: int) -> int = "calloc"
extern fn buf_c_memcpy(to: int, from: int, n: int) -> int = "memcpy"
extern fn buf_c_fopen(path: string, mode: string) -> int = "fopen"
extern fn buf_c_fread(to: int, size: int, count: int, file: int) -> int = "fread"
extern fn buf_c_fwrite(from: int, size: int, count: int, file: int) -> int = "fwrite"
extern fn buf_c_fclose(file: int) -> i32 = "fclose"
extern fn buf_c_memmove(to: int, from: int, n: int) -> int = "memmove"

-- ---- Cells ----
-- A cell is 8 bytes of memory that hold an int, named by its address.
-- Records that must be changed through any copy of a struct, such as
-- the language server's documents, are runs of cells.

-- `n` cells, each 0
fn cells_new(n: int) -> int {
    unsafe {
        let at = buf_c_calloc(n, 8)
        if at == 0 {
            panic("out of memory")
        }
        return at
    }
}

fn cell(at: int) -> int {
    unsafe {
        return load_i64(at)
    }
}

fn set_cell(at: int, value: int) {
    unsafe {
        store_i64(at, value)
    }
}

-- ---- Buffers ----

-- A new, empty buffer with room for `room` bytes
fn buf_new(room: int) -> int {
    let mut cap = room + 1
    if cap < 64 {
        cap = 64
    }
    unsafe {
        let b = buf_c_malloc(24)
        let data = buf_c_malloc(cap)
        if b == 0 or data == 0 {
            panic("out of memory")
        }
        store_i64(b, data)
        store_i64(b + 8, 0)
        store_i64(b + 16, cap)
        return b
    }
}

-- How many bytes the buffer holds
fn buf_len(b: int) -> int {
    unsafe {
        return load_i64(b + 8)
    }
}

-- Make room for `extra` more bytes and the NUL that ends the string
fn buf_reserve(b: int, extra: int) {
    unsafe {
        let need = load_i64(b + 8) + extra + 1
        let mut cap = load_i64(b + 16)
        if need > cap {
            while cap < need {
                cap = cap * 2
            }
            let data = buf_c_realloc(load_i64(b), cap)
            if data == 0 {
                panic("out of memory")
            }
            store_i64(b, data)
            store_i64(b + 16, cap)
        }
    }
}

-- Add one byte
fn buf_byte(b: int, byte: int) {
    buf_reserve(b, 1)
    unsafe {
        let n = load_i64(b + 8)
        store_u8(load_i64(b) + n, byte)
        store_i64(b + 8, n + 1)
    }
}

-- Add an int as 8 bytes, for a buffer used as a list of ints
fn buf_i64(b: int, value: int) {
    buf_reserve(b, 8)
    unsafe {
        let n = load_i64(b + 8)
        store_i64(load_i64(b) + n, value)
        store_i64(b + 8, n + 8)
    }
}

-- Int number `index`, from 0, of a buffer used as a list of ints
fn buf_get_i64(b: int, index: int) -> int {
    unsafe {
        return load_i64(load_i64(b) + index * 8)
    }
}

-- Set int number `index` of a buffer used as a list of ints
fn buf_put_i64(b: int, index: int, value: int) {
    unsafe {
        store_i64(load_i64(b) + index * 8, value)
    }
}

-- The buffer's last byte; 0 when it is empty
fn buf_last_byte(b: int) -> int {
    unsafe {
        let n = load_i64(b + 8)
        if n == 0 {
            return 0
        }
        return load_u8(load_i64(b) + n - 1)
    }
}

-- Add the bytes of a string
fn buf_add(b: int, s: string) {
    let count = len(s)
    if count == 0 {
        return
    }
    buf_reserve(b, count)
    unsafe {
        let n = load_i64(b + 8)
        buf_c_memcpy(load_i64(b) + n, string_data(s), count)
        store_i64(b + 8, n + count)
    }
}

-- Add `count` bytes of a string, starting at byte `start`
fn buf_add_part(b: int, s: string, start: int, count: int) {
    if count <= 0 or start < 0 or start + count > len(s) {
        return
    }
    buf_reserve(b, count)
    unsafe {
        let n = load_i64(b + 8)
        buf_c_memcpy(load_i64(b) + n, string_data(s) + start, count)
        store_i64(b + 8, n + count)
    }
}

-- Add a number in decimal
fn buf_int(b: int, n: int) {
    if n < 0 {
        buf_byte(b, 45)
        -- The digits of a negative number, from its remainders, so that
        -- the smallest int needs no negation
        buf_digits(b, 0 - (n / 10), 0 - (n % 10))
        return
    }
    buf_digits(b, n / 10, n % 10)
}

-- Add the digits of `rest`, when it is not 0, and then the digit `last`
fn buf_digits(b: int, rest: int, last: int) {
    if rest > 0 {
        buf_digits(b, rest / 10, rest % 10)
    }
    buf_byte(b, 48 + last)
}

-- The address of the buffer's bytes. Adding to the buffer may move them.
fn buf_data(b: int) -> int {
    unsafe {
        return load_i64(b)
    }
}

-- Empty the buffer, keeping its room
fn buf_reset(b: int) {
    unsafe {
        store_i64(b + 8, 0)
    }
}

-- Say that the buffer holds `n` bytes, after writing them at buf_data
-- into room that buf_reserve made
fn buf_set_len(b: int, n: int) {
    unsafe {
        store_i64(b + 8, n)
    }
}

-- Free the buffer and its bytes
fn buf_free(b: int) {
    if b == 0 {
        return
    }
    unsafe {
        buf_c_free(load_i64(b))
        buf_c_free(b)
    }
}

-- The buffer's bytes as a string that does not own them: it is valid
-- until the buffer is added to, reset or freed. A NUL byte follows the
-- bytes, so its data can go to C.
fn buf_view(b: int) -> string {
    unsafe {
        let data = load_i64(b)
        let n = load_i64(b + 8)
        store_u8(data + n, 0)
        return string_from_raw(data, n, 0)
    }
}

-- `count` bytes of a string from byte `start`, as a string that shares
-- the bytes. Nothing is copied; the part is valid as long as `s` is.
fn view(s: string, start: int, count: int) -> string {
    let mut from = start
    if from < 0 {
        from = 0
    }
    if from > len(s) {
        from = len(s)
    }
    let mut n = count
    if n > len(s) - from {
        n = len(s) - from
    }
    if n <= 0 {
        return ""
    }
    unsafe {
        return string_from_raw(string_data(s) + from, n, 0)
    }
}

-- The buffer's bytes as a string. The buffer is gone afterwards: the
-- string owns its bytes.
fn buf_take(b: int) -> string {
    unsafe {
        let data = load_i64(b)
        let n = load_i64(b + 8)
        let cap = load_i64(b + 16)
        store_u8(data + n, 0)
        buf_c_free(b)
        return string_from_raw(data, n, cap)
    }
}

-- Add up to `count` bytes read from the open stdio file `file`. Gives
-- how many were added: fewer than `count` at the end of the file.
fn buf_read(b: int, file: int, count: int) -> int {
    if count <= 0 {
        return 0
    }
    buf_reserve(b, count)
    unsafe {
        let n = load_i64(b + 8)
        let got = buf_c_fread(load_i64(b) + n, 1, count, file)
        store_i64(b + 8, n + got)
        return got
    }
}

-- Add the contents of the file at `path`, which must be a string with
-- a NUL after it (a buf_view is; a view of part of a string is not).
-- Adds nothing, and gives false, when the file cannot be opened.
fn buf_append_file(b: int, path: string) -> bool {
    let mut file = 0
    unsafe {
        file = buf_c_fopen(path, "rb")
    }
    if file == 0 {
        return false
    }
    let mut got = buf_read(b, file, 65536)
    while got > 0 {
        got = buf_read(b, file, 65536)
    }
    unsafe {
        buf_c_fclose(file)
    }
    return true
}

-- Make the buffer hold the contents of the file at `path`. It is left
-- empty, and the result is false, when the file cannot be opened.
fn buf_read_file(b: int, path: string) -> bool {
    buf_reset(b)
    return buf_append_file(b, path)
}

-- Make the buffer's bytes the whole of the file at `path`. False when
-- the file cannot be written.
fn buf_write_file(b: int, path: string) -> bool {
    unsafe {
        let file = buf_c_fopen(path, "wb")
        if file == 0 {
            return false
        }
        let n = load_i64(b + 8)
        let mut wrote = n
        if n > 0 {
            wrote = buf_c_fwrite(load_i64(b), 1, n, file)
        }
        let closed = buf_c_fclose(file) as int
        return wrote == n and closed == 0
    }
}

-- Remove the first `n` bytes of the buffer
fn buf_drop_front(b: int, n: int) {
    let have = buf_len(b)
    if n <= 0 {
        return
    }
    if n >= have {
        buf_reset(b)
        return
    }
    unsafe {
        buf_c_memmove(load_i64(b), load_i64(b) + n, have - n)
        store_i64(b + 8, have - n)
    }
}

-- Add a string as one word of a shell command
fn buf_shell_quote(b: int, s: string) {
    buf_byte(b, 39)
    let mut run = 0
    let mut i = 0
    while i < len(s) {
        if byte_at(s, i) == 39 {
            buf_add_part(b, s, run, i - run)
            buf_add(b, "'\\''")
            run = i + 1
        }
        i = i + 1
    }
    buf_add_part(b, s, run, len(s) - run)
    buf_byte(b, 39)
}

-- A copy of a string that owns its bytes and has a NUL after them, as
-- a path or a map's key must
fn owned(s: string) -> string {
    let b = buf_new(len(s))
    buf_add(b, s)
    return buf_take(b)
}

-- Add a string as a JSON string: in quotes, with what JSON does not
-- allow in a string escaped
fn buf_json(b: int, s: string) {
    let count = len(s)
    buf_reserve(b, count + 2)
    buf_byte(b, 34)
    let mut run = 0
    let mut i = 0
    while i < count {
        let c = byte_at(s, i)
        if c == 34 or c == 92 or c < 32 {
            buf_add_part(b, s, run, i - run)
            run = i + 1
            buf_byte(b, 92)
            if c == 34 or c == 92 {
                buf_byte(b, c)
            } else if c == 10 {
                buf_byte(b, 110)
            } else if c == 13 {
                buf_byte(b, 114)
            } else if c == 9 {
                buf_byte(b, 116)
            } else {
                -- \u00XX
                buf_add(b, "u00")
                buf_byte(b, hex_digit(c / 16))
                buf_byte(b, hex_digit(c % 16))
            }
        }
        i = i + 1
    }
    buf_add_part(b, s, run, count - run)
    buf_byte(b, 34)
}

-- The character of a hexadecimal digit's value
fn hex_digit(n: int) -> int {
    if n < 10 {
        return 48 + n
    }
    return 87 + n
}

-- A string as JSON text
fn json_quote(s: string) -> string {
    let b = buf_new(len(s) + 2)
    buf_json(b, s)
    return buf_take(b)
}
