module list

-- ============================================================
-- Lists
-- The functions behind List[T]. See runtime/string.dm for how
-- rt_X functions replace the C runtime.
--
-- A list is { data, len, capacity }: the address of its elements,
-- their count, and the number of elements its allocation has room
-- for. The empty list is all zeros. The compiler passes a list as
-- the address of this header.
--
-- Elements lie side by side in the allocation. An int or a float
-- takes 8 bytes and a string 24. Lists of any other type go through
-- the rt_list_generic functions, which are told the element size
-- and copy elements to and from addresses the caller provides.
--
-- len(xs) and the creation of an empty list are inline and call
-- nothing.
--
-- A list that outgrows its allocation moves to a new one and leaves
-- the old one as it is. A copy of a list is a copy of its header, so
-- the copy goes on reading the allocation it was made with.
-- ============================================================

-- Make room for one more element of elem_size bytes
fn rt_list_grow(list: int, elem_size: int) {
    unsafe {
        let count = load_i64(list + 8)
        let capacity = load_i64(list + 16)
        if count < capacity {
            return
        }
        let mut new_capacity = 8
        if capacity > 0 {
            new_capacity = capacity * 2
        }
        let new_data = libc_malloc(new_capacity * elem_size)
        if new_data == 0 {
            panic("list push: out of memory")
        }
        let data = load_i64(list)
        if count > 0 {
            libc_memcpy(new_data, data, count * elem_size)
        }
        store_i64(list, new_data)
        store_i64(list + 16, new_capacity)
    }
}

-- The address of a new last element of elem_size bytes, which the
-- caller fills
fn rt_list_append(list: int, elem_size: int) -> int {
    rt_list_grow(list, elem_size)
    unsafe {
        let count = load_i64(list + 8)
        store_i64(list + 8, count + 1)
        return load_i64(list) + count * elem_size
    }
}

-- The address of element index, whose elements are elem_size bytes.
-- Panics when index is outside the list.
fn rt_list_slot(list: int, index: int, elem_size: int) -> int {
    unsafe {
        let count = load_i64(list + 8)
        if index < 0 or index >= count {
            panic("list index out of bounds: " + int_to_string(index) + " (len=" + int_to_string(count) + ")")
        }
        return load_i64(list) + index * elem_size
    }
}

-- Remove the last element, of elem_size bytes, and give its address.
-- The bytes stay where they are until the next push. Panics on an
-- empty list.
fn rt_list_remove_last(list: int, elem_size: int) -> int {
    unsafe {
        let count = load_i64(list + 8)
        if count <= 0 {
            panic("list pop: empty list")
        }
        store_i64(list + 8, count - 1)
        return load_i64(list) + (count - 1) * elem_size
    }
}

-- The string stored at address p
fn rt_string_at(p: int) -> string {
    unsafe {
        return string_from_raw(load_i64(p), load_i64(p + 8), load_i64(p + 16))
    }
}

-- ---- List[int] and the other integer lists ----

fn rt_list_int64_push(list: int, value: int) {
    unsafe {
        store_i64(rt_list_append(list, 8), value)
    }
}

fn rt_list_int64_get(list: int, index: int) -> int {
    unsafe {
        return load_i64(rt_list_slot(list, index, 8))
    }
}

fn rt_list_int64_set(list: int, index: int, value: int) {
    unsafe {
        store_i64(rt_list_slot(list, index, 8), value)
    }
}

fn rt_list_int64_pop(list: int) -> int {
    unsafe {
        return load_i64(rt_list_remove_last(list, 8))
    }
}

fn rt_list_int64_contains(list: int, value: int) -> bool {
    unsafe {
        let data = load_i64(list)
        let count = load_i64(list + 8)
        let mut i = 0
        while i < count {
            if load_i64(data + i * 8) == value {
                return true
            }
            i = i + 1
        }
        return false
    }
}

-- ---- List[float] ----

fn rt_list_double_push(list: int, value: float) {
    unsafe {
        store_f64(rt_list_append(list, 8), value)
    }
}

fn rt_list_double_get(list: int, index: int) -> float {
    unsafe {
        return load_f64(rt_list_slot(list, index, 8))
    }
}

fn rt_list_double_set(list: int, index: int, value: float) {
    unsafe {
        store_f64(rt_list_slot(list, index, 8), value)
    }
}

fn rt_list_double_pop(list: int) -> float {
    unsafe {
        return load_f64(rt_list_remove_last(list, 8))
    }
}

fn rt_list_double_contains(list: int, value: float) -> bool {
    unsafe {
        let data = load_i64(list)
        let count = load_i64(list + 8)
        let mut i = 0
        while i < count {
            if load_f64(data + i * 8) == value {
                return true
            }
            i = i + 1
        }
        return false
    }
}

-- ---- List[string] ----

fn rt_list_string_push(list: int, value: string) {
    unsafe {
        libc_memcpy(rt_list_append(list, 24), addr_of(value), 24)
    }
}

fn rt_list_string_get(list: int, index: int) -> string {
    return rt_string_at(rt_list_slot(list, index, 24))
}

fn rt_list_string_set(list: int, index: int, value: string) {
    unsafe {
        libc_memcpy(rt_list_slot(list, index, 24), addr_of(value), 24)
    }
}

fn rt_list_string_pop(list: int) -> string {
    return rt_string_at(rt_list_remove_last(list, 24))
}

fn rt_list_string_contains(list: int, value: string) -> bool {
    unsafe {
        let data = load_i64(list)
        let count = load_i64(list + 8)
        let mut i = 0
        while i < count {
            if rt_string_at(data + i * 24) == value {
                return true
            }
            i = i + 1
        }
        return false
    }
}

-- ---- Lists of any other type ----

-- Append the elem_size bytes at address value
fn rt_list_generic_push(list: int, value: int, elem_size: int) {
    unsafe {
        libc_memcpy(rt_list_append(list, elem_size), value, elem_size)
    }
}

-- Copy element index to address out
fn rt_list_generic_get(out: int, list: int, index: int, elem_size: int) {
    unsafe {
        libc_memcpy(out, rt_list_slot(list, index, elem_size), elem_size)
    }
}

-- Replace element index with the elem_size bytes at address value
fn rt_list_generic_set(list: int, value: int, index: int, elem_size: int) {
    unsafe {
        libc_memcpy(rt_list_slot(list, index, elem_size), value, elem_size)
    }
}

-- Remove the last element and copy it to address out
fn rt_list_generic_pop(out: int, list: int, elem_size: int) {
    unsafe {
        libc_memcpy(out, rt_list_remove_last(list, elem_size), elem_size)
    }
}
