module thread

-- Standard Threading Module
-- Provides threading primitives backed by POSIX pthreads.
-- Note: Thread functions spawned must be top-level functions (not closures)
-- because the spawn API uses C function pointers.

-- Thread spawn: run a function in a new thread
-- The function must take no arguments and return void.
-- extern fn dm_thread_spawn(func: fn() -> void) -> Thread
-- extern fn dm_thread_join(thread: Thread) -> void

-- Mutex operations
-- extern fn dm_mutex_new() -> Mutex
-- extern fn dm_mutex_lock(mutex: &Mutex) -> void
-- extern fn dm_mutex_unlock(mutex: &Mutex) -> void
-- extern fn dm_mutex_destroy(mutex: &Mutex) -> void

-- Note: Direct use of the runtime threading API requires extern fn declarations.
-- The threading primitives (dm_thread_spawn, dm_thread_join, dm_mutex_*) are
-- available in the C runtime and can be called via extern fn from dAImond code.
