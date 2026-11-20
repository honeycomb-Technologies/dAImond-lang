module main

-- A thread's function may be declared after its use, be named like a
-- C function, or come through a parameter.

fn run_and_wait(f: fn() -> void) {
    thread_join(thread_spawn(f))
}

fn main() -> int {
    thread_join(thread_spawn(later))
    run_and_wait(remove)
    run_and_wait(later)
    return 0
}

fn later() {
    println("later ran")
}

fn remove() {
    println("remove ran")
}
