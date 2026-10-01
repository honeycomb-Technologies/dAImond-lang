# dAImond Cross-Language Benchmark Suite

A collection of benchmarks comparing dAImond's performance against C, Rust, Go, Zig, and JavaScript (Bun) on identical algorithms.

## Prerequisites

Required:
- **hyperfine** -- benchmark runner (`cargo install hyperfine`)
- **dAImond** -- the dAImond compiler (build with `cd stage0 && zig build`)

Optional (benchmarks are skipped for missing compilers):
- **gcc** -- C compiler
- **rustc** -- Rust compiler
- **go** -- Go compiler
- **zig** -- Zig compiler (0.13.0)
- **bun** -- JavaScript runtime

## Usage

```bash
# Run all benchmarks (compile + benchmark with hyperfine)
cd benchmarks/cross-lang
bash run.sh

# Run a single benchmark
bash run.sh parallel_fib

# Compile only (no benchmarking)
bash build.sh

# Compile a single benchmark
bash build.sh parallel_fib

# Run compile-speed benchmark separately
bash compile_speed/measure.sh
```

## Benchmarks

### Runtime Benchmarks

Each benchmark directory contains implementations in up to 6 languages (`.dm`, `.c`, `.rs`, `.go`, `.zig`, `.js`), all implementing the same algorithm.

| Benchmark | Description |
|-----------|-------------|
| `fibonacci` | Recursive Fibonacci (fib(40)), testing function call overhead |
| `mandelbrot` | Mandelbrot set computation, testing floating-point and loop performance |
| `nbody` | N-body gravitational simulation, testing struct and floating-point ops |
| `binary_trees` | Allocate and traverse binary trees, testing allocation patterns |
| `list_stress` | Push/pop/iterate over large lists, testing dynamic array performance |
| `json_parse` | Parse a JSON-like string, testing string processing |
| `string_concat` | Repeated string concatenation, testing string allocation |
| `hashmap` | Hash map insert/lookup/delete, testing associative container performance |
| `sort` | Sort a large array of integers, testing comparison and swap performance |
| `parallel_fib` | Spawn 4 threads computing fibonacci, testing thread creation and parallel speedup |

### Compile Speed Benchmark

The `compile_speed/` directory measures how fast each compiler processes a ~500 line source file containing struct definitions, arithmetic functions, recursive algorithms, and loop-based computations.

```bash
bash compile_speed/measure.sh
```

This uses `-O0` / Debug optimization level to measure pure compiler throughput rather than optimizer performance.

## Methodology

- All implementations use the **same algorithm** with the same parameters
- All compiled languages use **-O3 / ReleaseFast** optimization for runtime benchmarks
- **hyperfine** runs each benchmark with:
  - 3 warmup runs (excluded from results)
  - Minimum 10 timed runs
  - Statistical analysis (mean, stddev, min, max)
- Results are exported as Markdown tables to `results/`
- The benchmark runner automatically detects available compilers and skips missing ones

## Interpreting Results

hyperfine outputs a table like:

```
| Command   | Mean [ms]  | Min [ms] | Max [ms] | Relative |
|:----------|:----------:|:--------:|:--------:|:--------:|
| C         | 120.3      | 118.1    | 125.4    | 1.00     |
| dAImond   | 125.7      | 122.0    | 131.2    | 1.04     |
| Rust      | 121.1      | 119.0    | 126.8    | 1.01     |
```

The **Relative** column shows how many times slower each command is compared to the fastest. A value of 1.00 is the fastest.

## Fairness Notes

These benchmarks aim to compare performance honestly, but there are inherent differences between languages that affect the comparison:

- **String model**: dAImond and C use immutable reference-counted strings. Rust uses owned `String` / `&str`. Go uses immutable strings with GC. These differences affect string-heavy benchmarks.
- **Memory management**: dAImond uses region-based allocation with arenas. C uses manual allocation. Rust uses ownership/borrowing. Go uses garbage collection. Zig uses explicit allocators. This affects allocation-heavy benchmarks.
- **JavaScript concurrency**: Bun/Node.js does not have true shared-memory threads. The `parallel_fib` JS implementation runs sequentially and measures single-threaded computation only.
- **Compiler maturity**: GCC, LLVM (Rust/Zig), and Go have decades of optimization work. dAImond's C backend inherits GCC/Clang optimizations for the generated C code.
- **Integer sizes**: dAImond uses 64-bit integers by default. Some implementations in other languages may use different default sizes, though all benchmarks aim for 64-bit where possible.

## Directory Structure

```
cross-lang/
  run.sh              -- Main runner: compile + benchmark all
  build.sh            -- Compile only, no benchmarking
  README.md           -- This file
  build/              -- Compiled binaries (gitignored)
  results/            -- Benchmark results as Markdown (gitignored)
  fibonacci/          -- Recursive fibonacci benchmark
  mandelbrot/         -- Mandelbrot set benchmark
  nbody/              -- N-body simulation benchmark
  binary_trees/       -- Binary tree allocation benchmark
  list_stress/        -- Dynamic list stress test
  json_parse/         -- JSON-like string parsing
  string_concat/      -- String concatenation benchmark
  hashmap/            -- Hash map operations benchmark
  sort/               -- Integer sorting benchmark
  parallel_fib/       -- Parallel fibonacci (threading)
  compile_speed/      -- Compiler throughput benchmark
```

## Adding a New Benchmark

1. Create a new directory under `cross-lang/` (e.g., `my_bench/`)
2. Add source files: `my_bench.dm`, `my_bench.c`, `my_bench.rs`, `my_bench.go`, `my_bench.zig`, `my_bench.js`
3. Each implementation must produce **identical output** for verification
4. Add the directory name to the `BENCHMARKS` array in `run.sh` and `build.sh`
5. All implementations should use the same algorithm and data sizes
