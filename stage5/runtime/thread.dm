module thread

-- ============================================================
-- Threads and mutexes
-- thread_spawn, thread_join, mutex_new, mutex_lock, mutex_unlock and
-- mutex_destroy, over libc's pthreads. See runtime/string.dm for how
-- rt_X functions replace the C runtime.
--
-- A thread is its pthread_t and a mutex is the address of its
-- pthread_mutex_t, both as an int.
-- ============================================================

extern fn libc_pthread_create(thread: int, attributes: int, start: int, argument: int) -> i32 = "pthread_create"
extern fn libc_pthread_join(thread: int, result: int) -> i32 = "pthread_join"
extern fn libc_pthread_mutex_init(mutex: int, attributes: int) -> i32 = "pthread_mutex_init"
extern fn libc_pthread_mutex_lock(mutex: int) -> i32 = "pthread_mutex_lock"
extern fn libc_pthread_mutex_unlock(mutex: int) -> i32 = "pthread_mutex_unlock"
extern fn libc_pthread_mutex_destroy(mutex: int) -> i32 = "pthread_mutex_destroy"

-- thread_spawn(f): run the function f, which takes nothing and gives
-- nothing, on a new thread, and give the thread. Panics when the
-- thread cannot be started.
--
-- f itself is the thread's start routine. pthreads calls it with one
-- argument and reads a result, and on x86-64 a function that takes
-- nothing ignores the argument's register and leaves the result's
-- register as it is. thread_join asks for no result, so that value is
-- never read.
fn rt_thread_spawn(f: int) -> int {
    unsafe {
        let mut thread = 0
        if libc_pthread_create(addr_of(thread), 0, f, 0) != 0 {
            panic("thread_spawn: cannot start a thread")
        }
        return thread
    }
}

-- thread_join(thread): wait for the thread to end
fn rt_thread_join(thread: int) {
    unsafe {
        libc_pthread_join(thread, 0)
    }
}

-- mutex_new(): a new, unlocked mutex
fn rt_mutex_new() -> int {
    unsafe {
        -- A pthread_mutex_t is 40 bytes on x86-64 Linux
        let mutex = libc_malloc(40)
        if mutex == 0 {
            panic("out of memory")
        }
        libc_pthread_mutex_init(mutex, 0)
        return mutex
    }
}

-- mutex_lock(mutex): wait until the mutex is unlocked, and lock it
fn rt_mutex_lock(mutex: int) {
    unsafe {
        libc_pthread_mutex_lock(mutex)
    }
}

-- mutex_unlock(mutex): unlock the mutex
fn rt_mutex_unlock(mutex: int) {
    unsafe {
        libc_pthread_mutex_unlock(mutex)
    }
}

-- mutex_destroy(mutex): free the mutex, which must be unlocked
fn rt_mutex_destroy(mutex: int) {
    unsafe {
        libc_pthread_mutex_destroy(mutex)
        libc_free(mutex)
    }
}
