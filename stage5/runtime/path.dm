module path

-- ============================================================
-- Paths
-- path_dirname, path_basename, path_extension, path_stem and
-- path_join. They work on the text of a path and never look at the
-- file system. See runtime/string.dm for how rt_X functions replace
-- the C runtime.
-- ============================================================

-- The index after the last "/" in the n bytes at data, or 0 when
-- there is none
fn rt_path_name_start(data: int, n: int) -> int {
    let mut i = n
    unsafe {
        -- 47 is "/"
        while i > 0 and load_u8(data + i - 1) != 47 {
            i = i - 1
        }
    }
    return i
}

-- path_dirname(path): the part before the last "/". A path with no
-- "/" is in ".", and the directory of a path in "/" is "/".
fn rt_path_dirname(path: string) -> string {
    unsafe {
        let data = string_data(path)
        let name_start = rt_path_name_start(data, len(path))
        if name_start == 0 {
            return "."
        }
        if name_start == 1 {
            return "/"
        }
        return rt_string_copy(data, name_start - 1)
    }
}

-- path_basename(path): the part after the last "/"
fn rt_path_basename(path: string) -> string {
    let n = len(path)
    unsafe {
        let data = string_data(path)
        let name_start = rt_path_name_start(data, n)
        if name_start == 0 {
            return path
        }
        return rt_string_copy(data + name_start, n - name_start)
    }
}

-- path_extension(path): the name's last "." and what follows it, or
-- "" when the name has no "."
fn rt_path_extension(path: string) -> string {
    let n = len(path)
    unsafe {
        let data = string_data(path)
        let mut i = n
        -- 46 is "." and 47 is "/"
        while i > 0 and load_u8(data + i - 1) != 46 and load_u8(data + i - 1) != 47 {
            i = i - 1
        }
        if i == 0 or load_u8(data + i - 1) == 47 {
            return ""
        }
        return rt_string_copy(data + i - 1, n - i + 1)
    }
}

-- path_stem(path): the name without its extension. A name whose only
-- "." is its first byte, such as ".bashrc", is its own stem.
fn rt_path_stem(path: string) -> string {
    let base = rt_path_basename(path)
    unsafe {
        let data = string_data(base)
        let mut i = len(base)
        while i > 0 and load_u8(data + i - 1) != 46 {
            i = i - 1
        }
        if i <= 1 {
            return base
        }
        return rt_string_copy(data, i - 1)
    }
}

-- path_join(a, b): a and b with one "/" between them, unless a ends
-- in one. An empty part gives the other part.
fn rt_path_join(a: string, b: string) -> string {
    let na = len(a)
    let nb = len(b)
    if na == 0 {
        return b
    }
    if nb == 0 {
        return a
    }
    unsafe {
        let mut sep = 1
        if load_u8(string_data(a) + na - 1) == 47 {
            sep = 0
        }
        let n = na + sep + nb
        let buf = rt_string_buffer(n)
        libc_memcpy(buf, string_data(a), na)
        store_u8(buf + na, 47)
        libc_memcpy(buf + na + sep, string_data(b), nb)
        return string_from_raw(buf, n, n + 1)
    }
}
