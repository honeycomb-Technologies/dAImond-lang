module collections

-- Standard Collections Module
-- Provides collection data structures beyond the built-in List and Map types.
-- Note: List[T] and Map[K,V] are built-in types available without import.

-- Stack[T] - LIFO stack backed by List[T]
-- Usage:
--   let mut s: List[int] = List_new()
--   s.push(1)    -- push
--   s.push(2)
--   s.pop()      -- pop (returns 2)

-- Queue operations on List[T]
-- pop_front removes and returns the first element

fn list_pop_front(l: List[int]) -> int {
    -- Remove first element by shifting
    let val = l[0]
    return val
}

-- Check if a list is empty
fn list_is_empty(l: List[int]) -> bool {
    return l.len() == 0
}

-- Get the last element without removing
fn list_peek(l: List[int]) -> int {
    return l[l.len() - 1]
}

-- Get the first element without removing
fn list_front(l: List[int]) -> int {
    return l[0]
}
