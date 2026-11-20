module test

extern fn getrlimit(resource: i32, limit: int) -> i32

struct Limit {
    current: int,
    highest: int
}

-- Neither a loop nor a tail call: each level keeps its n for after
-- the call
fn depth(n: int) -> int {
    if n == 0 {
        return 1
    }
    return (depth(n - 1) * 31 + n) % 1000003
}

fn main() {
    -- The limit on the stack's size is at least 64 MB, or there is
    -- none, or it is as high as it may be raised
    unsafe {
        let mut limit = Limit { current: 0, highest: 0 }
        let result = getrlimit(3, addr_of(limit))
        let mb = 1024 * 1024
        println(int_to_string(result as int) + " " + bool_to_string(limit.current < 0 or limit.current >= 64 * mb or limit.current == limit.highest))
    }
    -- 100,000 levels need more than the 8 MB a program usually starts
    -- with
    println(int_to_string(depth(100000)))
}