module collections

-- Standard Collections Module
-- Set[T], Queue[T] and Stack[T], generic structs over Map[K, V] and
-- List[T]:
--
--   import std.collections
--
--   let mut seen: Set[string] = Set.new()
--   seen.add("a")
--   if seen.contains("a") { ... }
--
--   let mut jobs = Queue[int].new()
--   jobs.push(1)
--   match jobs.pop() { Some(job) => ..., None => ... }
--
--   let mut undo = Stack.of("first")
--   undo.push("second")
--
-- A copy of one shares its storage with the original, as a copy of a
-- list or a map does. Below them are functions over plain lists and
-- maps, from before the language had generic structs.

-- ============================================================================
-- Set[T]: values without repeats, in the order they were first added.
-- T is a string or an integer type, as a Map's keys are.
-- ============================================================================

struct Set[T] {
    items: Map[T, bool]
}

impl Set[T] {
    fn new() -> Set[T] {
        return Set { items: Map_new() }
    }

    -- The set of a list's values
    fn from(values: List[T]) -> Set[T] {
        let mut s: Set[T] = Set.new()
        for v in values { s.add(v) }
        return s
    }

    fn add(mut self, item: T) {
        self.items.insert(item, true)
    }

    fn contains(self, item: T) -> bool {
        return self.items.contains(item)
    }

    -- Take the item out; whether it was in the set
    fn remove(mut self, item: T) -> bool {
        if self.items.contains(item) == false { return false }
        self.items.remove(item)
        return true
    }

    fn len(self) -> int {
        return self.items.len()
    }

    fn is_empty(self) -> bool {
        return self.items.len() == 0
    }

    -- The items, in the order they were added
    fn to_list(self) -> List[T] {
        return self.items.keys()
    }

    -- The items of this set and of the other
    fn union(self, other: Set[T]) -> Set[T] {
        let mut out: Set[T] = Set.new()
        for a in self.items.keys() { out.add(a) }
        for b in other.items.keys() { out.add(b) }
        return out
    }

    -- The items that are in both sets
    fn intersection(self, other: Set[T]) -> Set[T] {
        let mut out: Set[T] = Set.new()
        for a in self.items.keys() {
            if other.contains(a) { out.add(a) }
        }
        return out
    }
}

-- ============================================================================
-- Queue[T]: first in, first out
-- ============================================================================

struct Queue[T] {
    items: List[T],
    -- The index of the front item; the items before it have left
    head: int
}

impl Queue[T] {
    fn new() -> Queue[T] {
        return Queue { items: [], head: 0 }
    }

    -- Add an item at the back
    fn push(mut self, item: T) {
        self.items.push(item)
    }

    -- Take the front item; None when the queue is empty
    fn pop(mut self) -> Option[T] {
        if self.head >= self.items.len() { return None }
        let item = self.items[self.head]
        self.head = self.head + 1
        -- Drop the items that have left once they are half of the list
        if self.head * 2 >= self.items.len() and self.head >= 16 {
            let mut rest: List[T] = []
            let mut i = self.head
            while i < self.items.len() {
                rest.push(self.items[i])
                i = i + 1
            }
            self.items = rest
            self.head = 0
        }
        return Some(item)
    }

    -- The front item without taking it
    fn peek(self) -> Option[T] {
        if self.head >= self.items.len() { return None }
        return Some(self.items[self.head])
    }

    fn len(self) -> int {
        return self.items.len() - self.head
    }

    fn is_empty(self) -> bool {
        return self.head >= self.items.len()
    }
}

-- ============================================================================
-- Stack[T]: last in, first out
-- ============================================================================

struct Stack[T] {
    items: List[T]
}

impl Stack[T] {
    fn new() -> Stack[T] {
        return Stack { items: [] }
    }

    -- A stack that holds the one item
    fn of(item: T) -> Stack[T] {
        let mut s: Stack[T] = Stack.new()
        s.push(item)
        return s
    }

    fn push(mut self, item: T) {
        self.items.push(item)
    }

    -- Take the top item; None when the stack is empty
    fn pop(mut self) -> Option[T] {
        if self.items.len() == 0 { return None }
        return Some(self.items.pop())
    }

    -- The top item without taking it
    fn peek(self) -> Option[T] {
        if self.items.len() == 0 { return None }
        return Some(self.items[self.items.len() - 1])
    }

    fn len(self) -> int {
        return self.items.len()
    }

    fn is_empty(self) -> bool {
        return self.items.len() == 0
    }
}

-- ============================================================================
-- Functions over plain lists and maps
-- ============================================================================

-- A set of strings as a Map[string, bool]

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

-- A queue of ints as a List[int]

-- Get the size of a queue
fn queue_size_int(q: List[int]) -> int {
    return q.len()
}

-- Check if queue is empty
fn queue_empty_int(q: List[int]) -> bool {
    return q.len() == 0
}

-- A stack of ints as a List[int]

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
