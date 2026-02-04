module collections

-- Standard Collections Module
-- Provides guidance on using built-in collection types.
--
-- List[T] - Dynamic array (built-in)
--   let mut items: List[int] = []
--   items.push(1)      -- append element
--   items.push(2)
--   items.pop()        -- remove and return last element (returns 2)
--   items.len()        -- get length
--   items[0]           -- index access
--   for item in items { ... }  -- iteration
--
-- Stack (LIFO) - Use List[T] directly:
--   let mut stack: List[int] = []
--   stack.push(1)      -- push
--   stack.pop()        -- pop
--
-- Queue (FIFO) - Use List[T] with index access:
--   let mut queue: List[int] = []
--   queue.push(1)      -- enqueue (push to back)
--   let front = queue[0]  -- peek front
--
-- Map[K,V] - Hash map (built-in)
--   let mut m: Map[string, int] = Map_new()
--   m.insert("key", 42)
--   m.get("key")       -- returns value (panics if missing)
--   m.contains("key")  -- check existence
--   m.remove("key")    -- remove entry
--   m.len()            -- number of entries
--   m.keys()           -- List[K] of keys
--   m.values()         -- List[V] of values
--   m["key"]           -- index access (alias for get)
--   m["key"] = 42      -- index assignment (alias for insert)

-- Helper: check if a collection count is zero
fn is_empty_count(count: int) -> bool {
    return count == 0
}
