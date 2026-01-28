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

# View tokens (for debugging)
./zig-out/bin/daimond lex examples/hello.dm
```

### Running Tests

```bash
cd stage0
zig build test
```

## Project Structure

```
dAImond-lang/
├── stage0/              # Stage 0 compiler (written in Zig)
│   ├── src/
│   │   ├── main.zig     # Entry point
│   │   ├── lexer.zig    # Lexical analyzer
│   │   ├── parser.zig   # Parser (TODO)
│   │   ├── checker.zig  # Type checker (TODO)
│   │   └── codegen.zig  # C code generator (TODO)
│   ├── tests/
│   └── build.zig
├── stage1/              # Stage 1 compiler (written in dAImond, TODO)
├── stdlib/              # Standard library (TODO)
├── examples/            # Example programs
│   ├── hello.dm         # Hello World
│   └── calculator.dm    # Scientific calculator
├── tests/               # Test programs
│   ├── arithmetic.dm
│   ├── structs.dm
│   └── generics.dm
└── docs/                # Documentation
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

- [x] Project structure
- [x] Lexer implementation
- [x] Lexer tests
- [ ] Parser
- [ ] Type checker
- [ ] C code generator
- [ ] Standard library
- [ ] Self-hosting

## Design Principles

1. **One obvious way to do anything**
2. **Reading code reveals its cost**
3. **Syntax should be greppable**
4. **No magic, no surprises**
5. **Familiar to C/Rust/Zig programmers**

## Contributing

This project is in early development. See the implementation roadmap in `docs/spec.md`.

## License

MIT License - see [LICENSE](LICENSE) for details.
