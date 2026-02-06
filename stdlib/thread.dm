module thread

-- Standard Threading Module
-- Provides threading primitives backed by POSIX pthreads.
-- Thread handles and mutex handles are represented as integers.
--
-- Example:
--   import std.thread
--
--   fn worker() {
--       println("Hello from thread!")
--   }
--
--   fn main() {
--       let t = thread_spawn(worker)
--       thread_join(t)
--
--       let m = mutex_new()
--       mutex_lock(m)
--       -- critical section
--       mutex_unlock(m)
--       mutex_destroy(m)
--   }
--
-- Note: thread_spawn takes a function reference. The function must take
-- no arguments and return void. It is spawned in a new OS thread.

-- Create a new mutex
-- Returns a mutex handle (integer)
fn thread_mutex_new() -> int {
    return mutex_new()
}

-- Lock a mutex (blocks until acquired)
fn thread_mutex_lock(m: int) {
    mutex_lock(m)
}

-- Unlock a mutex
fn thread_mutex_unlock(m: int) {
    mutex_unlock(m)
}

-- Destroy a mutex and free its resources
fn thread_mutex_destroy(m: int) {
    mutex_destroy(m)
}

-- Spawn a new thread running a function
-- Returns a thread handle (integer)
fn thread_run(func: fn() -> void) -> int {
    return thread_spawn(func)
}

-- Wait for a thread to finish
fn thread_wait(t: int) {
    thread_join(t)
}
