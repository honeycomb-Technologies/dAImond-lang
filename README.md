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
├── stage0/                    # Stage 0 bootstrap compiler (written in Zig)
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
├── stage1/                    # Stage 1 self-hosting compiler (written in dAImond)
│   ├── main.dm                # Monolithic bootstrap file (generated)
│   ├── main_split.dm          # Entry point with imports (for self-compilation)
│   ├── token.dm               # Token kinds, Token struct, keyword lookup
│   ├── lexer.dm               # Lexer struct, tokenize, character helpers
│   ├── compiler.dm            # Compiler struct, parser helpers, type mapping
│   ├── compile_expr.dm        # Expression compiler, lambdas, generics
│   ├── compile_stmt.dm        # Statement compiler, type tracking/inference
│   ├── compile_match.dm       # Match expressions and statements
│   ├── compile_decl.dm        # Function/struct/enum/impl declarations
│   ├── runtime.dm             # C runtime emission, output assembly
│   ├── imports.dm             # Multi-file import resolver
│   └── gen_bootstrap.sh       # Concatenates modules into main.dm
├── examples/                  # Example programs
│   ├── hello.dm               # Hello World
│   ├── arithmetic.dm          # Basic math operations
│   ├── fibonacci.dm           # Recursive functions
│   └── calculator.dm          # Scientific calculator
└── tests/                     # Integration test programs
    ├── arithmetic.dm          # Arithmetic validation
    ├── structs.dm             # Struct/enum/pattern matching
    ├── generics.dm            # Generics and traits
    ├── test_nested_for.dm     # Nested for-in loops
    ├── test_for_types.dm      # For-in with typed lists
    ├── test_compound_assign.dm # Compound assignment operators
    ├── test_pipeline.dm       # Pipeline operator |>
    ├── test_try_operator.dm   # Error propagation ?
    ├── test_box.dm            # Box[T] heap allocation
    └── test_builtins.dm       # All builtin functions
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
Stage 0 (Zig) ✅ → Stage 1 (dAImond) ✅ → Stage 2 (Self-compiled) ✅ → Stage 3 (LLVM)
```

1. **Stage 0** (Complete): Hand-written compiler in Zig, compiles dAImond → C
2. **Stage 1** (Complete): Compiler rewritten in dAImond, compiled by Stage 0
3. **Stage 2** (Complete): Stage 1 compiles itself — fixed-point bootstrap verified (Stage 1 output = Stage 2 output)
4. **Stage 3**: LLVM backend for optimized native code

## Current Status

### Stage 0 Compiler — Complete
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

### Stage 1 Self-Hosting Compiler — Complete
- [x] Compiler rewritten in dAImond (~10 modules)
- [x] Full feature parity with Stage 0 subset
- [x] Verified fixed-point bootstrap (Stage 1 compiles itself with identical output)
- [x] Multi-file import system with transitive dependency resolution
- [x] Enum payloads, Option[T], Result[T, E], match expressions
- [x] Lambda expressions (lifted to static functions)
- [x] Generic function monomorphization (explicit and implicit)
- [x] Pipeline operator `|>` and error propagation `?`
- [x] Box[T] heap allocation support
- [x] Compound assignment operators (`+=`, `-=`, `*=`, `/=`) and modulo `%`
- [x] All builtins (I/O, string ops, file I/O, CLI args, system)
- [x] CLI flag parity (`-o`, `-c`, `--emit-c`, `-v`, `--version`, `-h`)

### Upcoming
- [ ] Code formatter (`fmt` command)
- [ ] Standard library (`stdlib/`)
- [ ] Traits and effects (deferred to Stage 2+)
- [ ] Region-based memory management (deferred to Stage 2+)
- [ ] LLVM backend (Stage 3)
- [ ] Package management
- [ ] Multi-file imports for user programs in Stage 0

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
