// Parallel Fibonacci Benchmark
//
// NOTE: JavaScript/Bun does not have true shared-memory threads in the same
// way as C/Rust/Go/Zig. Worker threads exist but have significant overhead
// due to message passing serialization. For a fair comparison we compute
// sequentially here. The benchmark measures raw computation speed, not
// parallelism, for the JS implementation.

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const sum = fib(35) + fib(36) + fib(37) + fib(38);
console.log(sum);
