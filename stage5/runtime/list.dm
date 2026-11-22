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
-- len(xs), xs[i], xs[i] = v and the creation of an empty list are
-- inline and call nothing: the compiler checks the index and reads or
-- writes the element itself. The get functions here serve for-in,
-- which reads each element with a checked call.
--
-- The word before the elements, which data does not point at, is the
-- allocation's count: the number of elements the allocation
-- holds, counted over every list that uses it. A copy of a list is a
-- copy of its header, so the copy and the original use one allocation
-- and see each other's `xs[i] = v`. A push writes into the allocation
-- only when the list is as long as that count, which says that no
-- other list has an element where the new one goes. Otherwise the
-- list moves to an allocation of its own first, as a list that has
-- outgrown its allocation does, and leaves the old one as it is.
-- ============================================================

extern fn libc_aligned_alloc(alignment: int, size: int) -> int = "aligned_alloc"

-- The bytes of an allocation that come before its elements, of
-- elem_size bytes each. The last 8 of them are the count. malloc gives
-- memory at a multiple of 16; a type that must lie at a multiple of 16
-- or 32 has a size that is one, so the elements of such a size begin
-- at a multiple of it.
fn rt_list_lead(elem_size: int) -> int {
    if elem_size % 32 == 0 {
        return 32
    }
    if elem_size % 16 == 0 {
        return 16
    }
    return 8
}

-- The address of a new last element of elem_size bytes, which the
-- caller fills
fn rt_list_append(list: int, elem_size: int) -> int {
    unsafe {
        let data = load_i64(list)
        let count = load_i64(list + 8)
        let capacity = load_i64(list + 16)
        if count < capacity {
            if load_i64(data - 8) == count {
                store_i64(data - 8, count + 1)
                store_i64(list + 8, count + 1)
                return data + count * elem_size
            }
        }
        -- Full, or another list has an element in the next place
        let mut new_capacity = capacity
        if count >= capacity {
            new_capacity = 8
            if capacity > 0 {
                new_capacity = capacity * 2
            }
        }
        -- The elements begin where the strictest of them may: a SIMD
        -- vector of 16 or 32 bytes lies at a multiple of its size
        let lead = rt_list_lead(elem_size)
        let mut block = 0
        if lead == 32 {
            block = libc_aligned_alloc(32, 32 + new_capacity * elem_size)
        } else {
            block = libc_malloc(lead + new_capacity * elem_size)
        }
        if block == 0 {
            panic("list push: out of memory")
        }
        let new_data = block + lead
        if count > 0 {
            libc_memcpy(new_data, data, count * elem_size)
        }
        store_i64(new_data - 8, count + 1)
        store_i64(list, new_data)
        store_i64(list + 8, count + 1)
        store_i64(list + 16, new_capacity)
        return new_data + count * elem_size
    }
}

-- Whether the list's allocation holds another number of elements than
-- the list has: a copy of the list has pushed or popped since the two
-- were the same
fn rt_list_apart(list: int) -> bool {
    unsafe {
        let data = load_i64(list)
        if data == 0 {
            return false
        }
        return load_i64(data - 8) != load_i64(list + 8)
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
-- The bytes stay where they are until the next push. The allocation
-- holds one element fewer when this list was as long as any that uses
-- it, so that a push after a pop writes in place. Panics on an empty
-- list.
fn rt_list_remove_last(list: int, elem_size: int) -> int {
    unsafe {
        let count = load_i64(list + 8)
        if count <= 0 {
            panic("list pop: empty list")
        }
        let data = load_i64(list)
        if load_i64(data - 8) == count {
            store_i64(data - 8, count - 1)
        }
        store_i64(list + 8, count - 1)
        return data + (count - 1) * elem_size
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

-- Remove the last element and copy it to address out
fn rt_list_generic_pop(out: int, list: int, elem_size: int) {
    unsafe {
        libc_memcpy(out, rt_list_remove_last(list, elem_size), elem_size)
    }
}
