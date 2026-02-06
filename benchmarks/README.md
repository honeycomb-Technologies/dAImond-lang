# dAImond Benchmark Suite

Performance benchmarks for the dAImond programming language, comparing compiled dAImond programs against equivalent C implementations.

## Structure

```
benchmarks/
├── micro/           # Micro-benchmarks (single operation focus)
│   ├── fibonacci.dm       # Recursive fib(40) — function call overhead
│   ├── string_concat.dm   # 100K string concatenations
│   ├── list_ops.dm        # 1M list push/pop operations
│   ├── struct_create.dm   # 1M struct creation + field access
│   ├── pattern_match.dm   # 1M enum variant pattern matches
│   └── map_ops.dm         # 100K map insert/lookup/remove
├── macro/           # Macro-benchmarks (real-world programs)
│   ├── json_parser.dm     # Recursive descent JSON parser
│   ├── matrix_multiply.dm # 500x500 matrix multiplication
│   └── word_count.dm      # Word frequency counting
├── compare/         # Equivalent C implementations
│   ├── json_parser.c
│   ├── matrix_multiply.c
│   └── word_count.c
├── run.sh           # Benchmark runner script
└── README.md        # This file
```

## Running

```bash
# Build the compiler first
cd stage0 && zig build

# Run all benchmarks
cd benchmarks && bash run.sh
```

The runner script:
1. Compiles all dAImond benchmarks at `-O3`
2. Compiles C comparison benchmarks at `-O3`
3. Runs each benchmark and captures wall-clock time
4. Outputs a comparison table with dAImond/C ratios

## Micro Benchmarks

| Benchmark | What it tests |
|-----------|---------------|
| `fibonacci` | Recursive function calls (fib(40), ~1B calls) |
| `string_concat` | String allocation and copying (100K concatenations) |
| `list_ops` | Dynamic array resize (1M push + 1M pop) |
| `struct_create` | Struct instantiation and field access (1M iterations) |
| `pattern_match` | Enum variant dispatch (1M match operations) |
| `map_ops` | Hash map insert/lookup/remove (100K entries) |

## Macro Benchmarks

| Benchmark | What it tests |
|-----------|---------------|
| `json_parser` | String processing, recursion, control flow |
| `matrix_multiply` | Nested loops, floating-point arithmetic (500x500) |
| `word_count` | String splitting, map operations, iteration |

## Environment

Set `CC` to choose the C compiler for comparisons:

```bash
CC=gcc bash run.sh    # Compare against GCC
CC=clang bash run.sh  # Compare against Clang
```
