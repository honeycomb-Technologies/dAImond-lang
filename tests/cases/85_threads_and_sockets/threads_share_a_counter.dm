module main

-- Four threads add to one counter. Each holds the mutex while it
-- reads and writes the counter, so no addition is lost.

-- The counter (8 bytes) and the mutex (8 bytes), shared by the threads
fn shared() -> int {
    unsafe {
        return static_addr("shared", 16)
    }
}

fn add_many() {
    unsafe {
        let mutex = load_i64(shared() + 8)
        let mut i = 0
        while i < 100000 {
            mutex_lock(mutex)
            store_i64(shared(), load_i64(shared()) + 1)
            mutex_unlock(mutex)
            i = i + 1
        }
    }
}

fn say_hello() {
    println("hello from a thread")
}

fn main() -> int {
    let first = thread_spawn(say_hello)
    thread_join(first)
    println("joined")

    let mutex = mutex_new()
    unsafe {
        store_i64(shared() + 8, mutex)
    }
    let mut threads: List[int] = []
    let mut i = 0
    while i < 4 {
        threads.push(thread_spawn(add_many))
        i = i + 1
    }
    for thread in threads {
        thread_join(thread)
    }
    mutex_destroy(mutex)
    unsafe {
        println(int_to_string(load_i64(shared())))
    }
    return 0
}
