module collections

-- Standard Collections Module
-- Provides collection types built on top of List[T] and Map[K,V].
--
-- NOTE: Due to the current type system, the following collections are
-- type-specialized rather than generic:
--   - Set: backed by Map[string, bool] (string keys only)
--   - Queue: backed by List[int] (int elements only)
--   - Stack: backed by List[int] (int elements only)
-- For other types, use List[T] and Map[K,V] directly.

-- ============================================================================
-- Set[T] - Unique element collection (backed by Map[T, bool])
-- ============================================================================
-- Usage:
--   let mut s: Map[string, bool] = Map_new()
--   s = set_add(s, "hello")
--   s = set_add(s, "world")
--   set_has(s, "hello")    -- true
--   set_size(s)            -- 2
--   s = set_remove(s, "hello")

-- Add an element to a set (Map[T, bool])
fn set_add_str(mut s: Map[string, bool], item: string) -> Map[string, bool] {
    s.insert(item, true)
    return s
}

-- Check if set contains an element
fn set_has_str(s: Map[string, bool], item: string) -> bool {
    return s.contains(item)
}

-- Remove an element from a set
fn set_remove_str(mut s: Map[string, bool], item: string) -> Map[string, bool] {
    s.remove(item)
    return s
}

-- Get the number of elements in a set
fn set_size_str(s: Map[string, bool]) -> int {
    return s.len()
}

-- Get all elements as a list
fn set_items_str(s: Map[string, bool]) -> List[string] {
    return s.keys()
}

-- ============================================================================
-- Queue[T] - FIFO queue (backed by List[T] with front index)
-- ============================================================================
-- Usage:
--   let mut q: List[string] = []
--   q.push("first")
--   q.push("second")
--   let front = q[0]       -- peek front
--   queue_size(q)           -- 2

-- Get the size of a queue
fn queue_size_int(q: List[int]) -> int {
    return q.len()
}

-- Check if queue is empty
fn queue_empty_int(q: List[int]) -> bool {
    return q.len() == 0
}

-- ============================================================================
-- Stack[T] - LIFO stack (List[T] with push/pop)
-- ============================================================================
-- Usage:
--   let mut stack: List[int] = []
--   stack.push(1)
--   stack.push(2)
--   let top = stack[stack.len() - 1]   -- peek top
--   stack.pop()                         -- pop

-- Peek at the top of a stack without removing it
fn stack_peek_int(stack: List[int]) -> int {
    return stack[stack.len() - 1]
}

-- Check if stack is empty
fn stack_empty_int(stack: List[int]) -> bool {
    return stack.len() == 0
}

-- Get the size of a stack
fn stack_size_int(stack: List[int]) -> int {
    return stack.len()
}

-- ============================================================================
-- Utility functions
-- ============================================================================

-- Check if a collection count is zero
fn is_empty_count(count: int) -> bool {
    return count == 0
}

-- Return the minimum of two integers
fn min_int(a: int, b: int) -> int {
    if a < b { return a }
    return b
}

-- Return the maximum of two integers
fn max_int(a: int, b: int) -> int {
    if a > b { return a }
    return b
}

-- Clamp an integer between bounds
fn clamp_int(x: int, lo: int, hi: int) -> int {
    if x < lo { return lo }
    if x > hi { return hi }
    return x
}
