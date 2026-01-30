# dAImond Programming Language

**The end-all language that AI builds with.**

dAImond is a systems programming language designed to be:
- **The fastest compiled language** (beating C and Rust)
- **The most AI-generatable language** (unambiguous, verifiable, composable)
- **Self-compiling** (compiler written in dAImond compiles itself)
- **Memory-safe without garbage collection** (region-based with full inference)
- **Fully Turing-complete at compile time**

## Quick Start

### Prerequisites

- [Zig](https://ziglang.org/) 0.13.0 or later

### Building

```bash
cd stage0
zig build
```

### Running

```bash
# Compile a dAImond file
./zig-out/bin/daimond examples/hello.dm

# Compile and run in one step
./zig-out/bin/daimond run examples/hello.dm

# View tokens (for debugging)
./zig-out/bin/daimond lex examples/hello.dm

# View AST (for debugging)
./zig-out/bin/daimond parse examples/hello.dm

# Type check only
./zig-out/bin/daimond check examples/hello.dm
```

### Running Tests

```bash
cd stage0

# Unit tests (all compiler modules)
zig build test

# Integration tests (compiles and runs .dm test programs)
zig build test-integration
```

## Project Structure

```
dAImond-lang/
├── stage0/                    # Stage 0 compiler (written in Zig)
│   ├── build.zig              # Zig build configuration
│   ├── .mise.toml             # Tool version pinning (Zig 0.13.0)
│   ├── src/
│   │   ├── main.zig           # CLI entry point and pipeline orchestration
│   │   ├── lexer.zig          # Lexical analyzer
│   │   ├── parser.zig         # Recursive descent + Pratt parser
│   │   ├── ast.zig            # AST node type definitions
│   │   ├── types.zig          # Type system, inference, effects
│   │   ├── checker.zig        # Type checker, symbol tables, unification
│   │   ├── codegen.zig        # C11 code generator
│   │   └── errors.zig         # Diagnostic reporting, colored output
│   ├── runtime/               # C runtime library
│   │   ├── daimond_runtime.h  # Runtime API (strings, arenas, option/result, I/O)
│   │   ├── daimond_runtime.c  # Runtime implementation
│   │   └── test_runtime.c     # Runtime unit tests
│   └── tests/
│       └── runner.zig         # Integration test harness
├── examples/                  # Example programs
│   ├── hello.dm               # Hello World
│   ├── arithmetic.dm          # Basic math operations
│   ├── fibonacci.dm           # Recursive functions
│   └── calculator.dm          # Scientific calculator
└── tests/                     # Integration test programs
    ├── arithmetic.dm          # Arithmetic validation
    ├── structs.dm             # Struct/enum/pattern matching
    └── generics.dm            # Generics and traits
```

## Language Overview

### Hello World

```dm
module hello

import std.io { print }

fn main() with [Console] {
    print("Hello, dAImond!")
}
```

### Variables

```dm
-- Immutable by default
let x = 42
let y: int = 42          -- explicit type

-- Mutable requires keyword
let mut counter = 0
counter = counter + 1

-- Constants (must be comptime-known)
const MAX_SIZE = 1024
```

### Functions

```dm
fn add(a: int, b: int) -> int {
    return a + b
}

-- Single expression (implicit return)
fn double(x: int) -> int = x * 2

-- Generic function
fn first[T](list: List[T]) -> Option[T] {
    if list.len() == 0 {
        return None
    }
    return Some(list[0])
}
```

### Structs and Enums

```dm
struct Point {
    x: float,
    y: float,
}

enum Option[T] {
    Some(T),
    None,
}

enum Result[T, E] {
    Ok(T),
    Err(E),
}
```

### Pattern Matching

```dm
match value {
    Some(x) => use(x),
    None => handle_missing(),
}
```

### Error Handling

```dm
fn parse_int(s: str) -> Result[int, ParseError] {
    ...
}

-- Propagation with ?
fn process(input: str) -> Result[Output, Error] {
    let num = parse_int(input)?
    return Ok(compute(num))
}
```

### Regions (Memory Management)

```dm
region scratch {
    let big_buffer = alloc[byte](1_000_000)
    process_in_buffer(big_buffer)
}   -- big_buffer freed here, all at once, O(1)
```

### Effects

```dm
fn read_file(path: str) -> str with [IO] {
    ...
}

fn main() with [IO, Console] {
    let data = read_file("input.txt")
    print("Done!")
}
```

## Bootstrap Path

```
Stage 0 (Zig) → Stage 1 (dAImond) → Stage 2 (Self-compiled) → Stage 3 (LLVM)
```

1. **Stage 0**: Hand-written compiler in Zig, compiles dAImond → C
2. **Stage 1**: Compiler rewritten in dAImond, compiled by Stage 0
3. **Stage 2**: Stage 1 compiles itself (verified identical output)
4. **Stage 3**: LLVM backend for optimized native code

## Current Status

### Stage 0 Compiler (Complete)
- [x] Project structure
- [x] Lexer with comprehensive token support
- [x] Recursive descent + Pratt parser
- [x] AST definitions for all language constructs
- [x] Type system with Hindley-Milner inference and unification
- [x] Type checker with symbol tables and scope management
- [x] C11 code generator
- [x] Error diagnostics with colored output
- [x] C runtime library (strings, arenas, option/result, I/O)
- [x] CLI with multiple commands (compile, run, lex, parse, check)
- [x] Unit tests for all compiler modules
- [x] Integration test harness

### Upcoming
- [ ] Code formatter (`fmt` command)
- [ ] Standard library (`stdlib/`)
- [ ] Stage 1 compiler (dAImond self-hosting)
- [ ] LLVM backend
- [ ] Package management
- [ ] Module system refinements

## Design Principles

1. **One obvious way to do anything**
2. **Reading code reveals its cost**
3. **Syntax should be greppable**
4. **No magic, no surprises**
5. **Familiar to C/Rust/Zig programmers**

## Contributing

This project is in early development. See `CLAUDE.md` for detailed development guidance.

## License

MIT License - see [LICENSE](LICENSE) for details.
