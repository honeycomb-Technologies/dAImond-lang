module map

-- ============================================================
-- Map[K, V]
-- The hash map behind Map[K, V] in every program Stage 5
-- compiles. gen_bootstrap.sh embeds this file in the compiler
-- (prelude.dm), which adds it to each program's declarations.
-- Keys are strings or integers; hash_key picks the hash.
--
-- Entries are kept in insertion order in entry_keys,
-- entry_vals and entry_hashes, so keys() and values() return
-- them in that order. slots is an open-addressing table of
-- indexes into those lists: -1 marks an empty slot and -2 a
-- removed entry. A removed entry's hash is -1 until rebuild()
-- drops it.
--
-- Copying a map copies its list headers, so a copy shares the
-- slot table with the original until either one rebuilds. A slot
-- that names an entry past this map's own entries was written by
-- another copy; lookups skip it and inserts may take it.
-- ============================================================

struct Map[K, V] {
    entry_keys: List[K],
    entry_vals: List[V],
    entry_hashes: List[int],
    slots: List[int],
    count: int
}

impl Map[K, V] {
    fn insert(mut self, k: K, v: V) {
        let h = hash_key(k)
        let s = self.find_slot(k, h)
        if s >= 0 {
            self.entry_vals[self.slots[s]] = v
            return
        }
        -- Keep at least a quarter of the slots empty so probes end
        if (self.entry_keys.len() + 1) * 4 > self.slots.len() * 3 {
            self.rebuild()
        }
        let cap = self.slots.len()
        let n = self.entry_keys.len()
        let mut i = h % cap
        while self.slots[i] != -1 and self.slots[i] < n {
            i = i + 1
            if i == cap { i = 0 }
        }
        self.slots[i] = n
        self.entry_keys.push(k)
        self.entry_vals.push(v)
        self.entry_hashes.push(h)
        self.count = self.count + 1
    }

    fn set(mut self, k: K, v: V) {
        self.insert(k, v)
    }

    fn get(self, k: K) -> V {
        let s = self.find_slot(k, hash_key(k))
        if s < 0 {
            panic("map get: key not found")
        }
        return self.entry_vals[self.slots[s]]
    }

    fn contains(self, k: K) -> bool {
        return self.find_slot(k, hash_key(k)) >= 0
    }

    fn remove(mut self, k: K) {
        let s = self.find_slot(k, hash_key(k))
        if s < 0 {
            return
        }
        self.entry_hashes[self.slots[s]] = -1
        self.slots[s] = -2
        self.count = self.count - 1
    }

    fn len(self) -> int {
        return self.count
    }

    fn keys(self) -> List[K] {
        let mut out: List[K] = []
        let mut e = 0
        while e < self.entry_keys.len() {
            if self.entry_hashes[e] >= 0 {
                out.push(self.entry_keys[e])
            }
            e = e + 1
        }
        return out
    }

    fn values(self) -> List[V] {
        let mut out: List[V] = []
        let mut e = 0
        while e < self.entry_vals.len() {
            if self.entry_hashes[e] >= 0 {
                out.push(self.entry_vals[e])
            }
            e = e + 1
        }
        return out
    }

    -- Slot holding k, whose hash is h, or -1
    fn find_slot(self, k: K, h: int) -> int {
        let cap = self.slots.len()
        if cap == 0 {
            return -1
        }
        let n = self.entry_keys.len()
        let mut i = h % cap
        while self.slots[i] != -1 {
            let e = self.slots[i]
            if e >= 0 and e < n {
                if self.entry_hashes[e] == h {
                    if self.entry_keys[e] == k {
                        return i
                    }
                }
            }
            i = i + 1
            if i == cap { i = 0 }
        }
        return -1
    }

    -- Drop removed entries and size the table for twice the live ones
    fn rebuild(mut self) {
        let mut cap = 8
        while cap < (self.count + 1) * 2 {
            cap = cap * 2
        }
        let mut new_keys: List[K] = []
        let mut new_vals: List[V] = []
        let mut new_hashes: List[int] = []
        let mut e = 0
        while e < self.entry_keys.len() {
            if self.entry_hashes[e] >= 0 {
                new_keys.push(self.entry_keys[e])
                new_vals.push(self.entry_vals[e])
                new_hashes.push(self.entry_hashes[e])
            }
            e = e + 1
        }
        let mut new_slots: List[int] = []
        while new_slots.len() < cap {
            new_slots.push(-1)
        }
        e = 0
        while e < new_hashes.len() {
            let mut i = new_hashes[e] % cap
            while new_slots[i] != -1 {
                i = i + 1
                if i == cap { i = 0 }
            }
            new_slots[i] = e
            e = e + 1
        }
        self.entry_keys = new_keys
        self.entry_vals = new_vals
        self.entry_hashes = new_hashes
        self.slots = new_slots
    }
}

-- Hashes are in [0, 2^31 - 1), and every step stays far below
-- 2^63, so the checked arithmetic never traps.
fn rt_map_hash_string(s: string) -> int {
    let mut h = 0
    let n = len(s)
    let mut i = 0
    while i < n {
        h = (h * 31 + byte_at(s, i) + 1) % 2147483647
        i = i + 1
    }
    return h
}

fn rt_map_hash_int(n: int) -> int {
    let h = n % 2147483647
    if h < 0 {
        return h + 2147483647
    }
    return h
}
