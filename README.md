# dAImond Programming Language

**The end-all language that AI builds with.**

dAImond is a systems programming language designed to be:
- **The fastest compiled language** (beating C and Rust)
- **The most AI-generatable language** (unambiguous, verifiable, composable)
- **Self-compiling** (compiler written in dAImond compiles itself)
- **Memory-safe without garbage collection** (region-based with full inference)
- **Fully Turing-complete at compile time**

## Quick Start

The compiler is written in dAImond and builds itself. It needs x86-64 Linux with glibc, LLVM 19 or newer with `lld`, and the C library's development files (`libc6-dev` or `glibc-devel`). It needs no C compiler and no other language's toolchain.

```bash
sh bootstrap.sh          # builds build/daimond (about six minutes, 1.4 GB of memory)
build/daimond examples/hello.dm -o hello && ./hello
```

`bootstrap.sh` turns the seed, `seed/daimond.bc` (the compiler as LLVM bitcode), into a compiler with LLVM's `llc` and `ld.lld`, compiles the compiler's source with it, and repeats until the compiler reproduces itself. It has been run with LLVM 19 and LLVM 23.

### Using the compiler

```bash
build/daimond file.dm -o program         # compile (-O1 to -O3 optimize, -g adds debug info for gdb and lldb)
build/daimond run file.dm -- arg1 arg2   # compile and run; the exit code is the program's
build/daimond check file.dm              # report errors without building
build/daimond fmt file.dm                # indent the file by its braces
build/daimond test file.dm               # run each fn test_*() and report PASS or FAIL
build/daimond --help
```

The language server is a dAImond program too. It runs the `daimond` next to it:

```bash
build/daimond lsp/main.dm -o build/daimond-lsp
```

### Running the tests

```bash
build/daimond tests/harness.dm -o build/harness
build/harness build/daimond              # every case in tests/cases (572 cases, about a minute)
build/harness build/daimond tests/cases 17_lists/
```

## Project Structure

```
dAImond-lang/
├── bootstrap.sh               # Builds the compiler from the seed with LLVM's llc and ld.lld
├── seed/
│   ├── daimond.bc             # The compiler as LLVM bitcode, written by bootstrap.sh --update-seed
│   └── BUILD_ID               # The build ID of the source the seed was made from
├── stage5/                    # The compiler, written in dAImond
│   ├── main_split.dm          # Entry point: the command line, imports, the commands, linking
│   ├── token.dm, lexer.dm     # Tokens and the lexer
│   ├── parser.dm, ast.dm      # Recursive descent + Pratt parser, and the AST
│   ├── ir.dm, ir_builder.dm   # dAImond IR (SSA) and its builder
│   ├── ir_gen*.dm             # IR generation: declarations, statements, expressions, builtins, comptime
│   ├── llvm_gen*.dm, llvm.dm  # LLVM IR generation from dAImond IR
│   ├── llvm_bridge.dm         # The compiler's calls of LLVM's C API
│   ├── runtime/               # The runtime of every program, written in dAImond over libc:
│   │   └── string.dm, convert.dm, list.dm, map.dm, os.dm, path.dm, io.dm, net.dm, thread.dm
│   ├── prelude.dm             # Generated: the runtime's source as a string, and the build ID
│   ├── main.dm                # Generated: the modules as one file, which bootstrap.sh compiles
│   └── gen_bootstrap.dm       # A dAImond program that writes prelude.dm and main.dm
├── lsp/                       # The language server, written in dAImond
├── stdlib/                    # Standard library modules (io, math, collections, string, os, fs, net, thread, test)
├── tests/
│   ├── harness.dm             # The test harness, written in dAImond
│   └── cases/                 # The suite: one directory per suite, one program and its expected output per case
├── examples/                  # hello, arithmetic, fibonacci, a calculator, snake and breakout
├── benchmarks/                # Benchmarks, some with versions in other languages to compare with
├── proposals/                 # Language features that are proposed and not implemented
├── tools/                     # Editor and debugger helpers
└── docs/                      # The IR specification and the project's history
```

## Language Overview

Every example below is part of `tests/cases/86_programs/readme_overview.dm`, which the test suite compiles and runs.

### Hello World

```dm
module hello

fn main() {
    println("Hello, dAImond!")
}
```

### Variables

```dm
-- Variables: immutable by default
let x = 42
let y: int = 42          -- explicit type
let mut counter = 0      -- mutable requires the keyword
counter = counter + 1
```

### Functions

```dm
fn add(a: int, b: int) -> int {
    return a + b
}

-- Single expression (implicit return)
fn double(x: int) -> int = x * 2

-- Generic function
fn max_of[T](a: T, b: T) -> T {
    if a > b { return a }
    return b
}
```

### String Interpolation

```dm
let name = "world"
let age = 42
println(f"Hello {name}, you are {age} years old")
```

### Structs and Enums

```dm
struct Point {
    x: int,
    y: int,
}

impl Point {
    fn manhattan(self) -> int {             -- bare self (type inferred from impl block)
        return self.x + self.y
    }
    fn translate(self: Point, dx: int, dy: int) -> Point {  -- explicit self type also works
        return Point { x: self.x + dx, y: self.y + dy }
    }
}

enum Shape {
    Circle(float),
    Rect(float, float),
    Dot,
}
```

### Traits

```dm
trait Display {
    fn show(self: &Self) -> string
}

impl Display for Point {
    fn show(self: &Self) -> string {
        return f"({self.x}, {self.y})"
    }
}

let p = Point { x: 3, y: 4 }
println(p.show())
let d: dyn Display = p   -- dynamic dispatch
println(d.show())
```

### Pattern Matching

```dm
fn describe(shape: Shape) -> string {
    match shape {
        Shape.Circle(r) => { return f"circle with radius {r}" }
        Shape.Rect(w, h) => { return f"rectangle {w} by {h}" }
        Shape.Dot => { return "just a dot" }
    }
    return ""
}

fn first_even(numbers: List[int]) -> Option[int] {
    for n in numbers {
        if n % 2 == 0 { return Some(n) }
    }
    return None
}

match first_even(numbers) {
    Some(n) => println(f"first even: {n}"),
    None => println("no even number"),
}
```

### Error Handling

```dm
fn read_number(text: string) -> Result[int, string] {
    if len(text) == 0 {
        return Err("nothing to read")
    }
    return Ok(parse_int(text))
}

fn parse_config(text: string) -> Result[Config, string] {
    let val = read_number(text)?   -- propagate the error with ?
    return Ok(Config { value: val })
}

match parse_config("") {
    Ok(config) => println(f"config value {config.value}"),
    Err(message) => println("error: " + message),
}
```

### Closures

```dm
let base = 10
let add_base = |n: int| -> int base + n   -- captures base
println(int_to_string(add_base(5)))
```

### Operator Overloading

```dm
struct Vec2 { x: int, y: int }

impl Vec2 {
    fn add(self, other: Vec2) -> Vec2 {
        return Vec2 { x: self.x + other.x, y: self.y + other.y }
    }
}

let v1 = Vec2 { x: 1, y: 2 }
let v2 = Vec2 { x: 10, y: 20 }
let v = v1 + v2          -- calls Vec2.add
println(f"{v.x}, {v.y}")
```

### Regions

```dm
region scratch {
    let label = "built in a region: " + int_to_string(x * 2)
    println(label)
}                        -- label cannot be used after the region
```

A value made inside a region may not leave it: storing it in a variable from outside the region, or returning it, is a compile error. The runtime does not release a region's memory yet.

### Async/Await

```dm
async fn compute(x: int, y: int) -> int {
    return x + y
}

let sum = await compute(10, 20)
println(int_to_string(sum))
```

### Effects

```dm
fn read_file(path: string) -> string with [FileSystem] {
    return file_read(path)
}

fn pure_add(a: int, b: int) -> int {
    -- no effects needed for pure computation
    return a + b
}
```

A function that declares effects may only call what its `with [...]` list allows: `file_read` needs `FileSystem`, `println` needs `Console`.

### Imports

```dm
module myapp

import helpers          -- helpers.dm, from the same directory
import geometry.shapes  -- geometry/shapes.dm
import std.math         -- stdlib/math.dm, the standard library
```

An import names a file next to the importing file. `std.<module>` with no such file is a module of the standard library: the directory `$DAIMOND_STDLIB`, else `stdlib/` beside the compiler's directory (as in a checkout, where the compiler is `build/daimond`), else `stdlib/` in the current directory.

### FFI (Foreign Function Interface)

```dm
extern fn abs(n: i32) -> i32
extern fn puts(s: string) -> i32

unsafe {
    println(int_to_string(abs(-7) as int))
    puts("printed by C's puts")
```

C's `int` is `i32`, a string argument goes to C as a `char*`, and a variadic C function is declared with `...`.

### Testing

```dm
module my_tests

fn test_addition() {
    assert_eq(2 + 3, 5)
}

fn test_strings() {
    let s = "hello" + " world"
    assert_eq(s, "hello world")
}

fn main() {
    println("Run with: daimond test my_tests.dm")
}
```

`daimond test my_tests.dm` runs each `test_*` function in its own process and reports `PASS` or `FAIL` for each.

## How the Compiler Builds Itself

The compiler has no other implementation. A new machine gets its first compiler from the seed:

```
llc + ld.lld turn seed/daimond.bc into   -> the seed compiler
the seed compiler compiles stage5/       -> generation 1
generation 1 compiles stage5/            -> generation 2
generation 2 compiles stage5/            -> generation 3, which must be the same file as generation 2
```

When the seed was made from the current source, generations 1 and 2 are already the same file. The seed does not have to match the source; it only has to be able to compile it. `bootstrap.sh --update-seed` writes the seed again, which is needed before the compiler's own source may use a language feature that the seed's compiler lacks.

What a build depends on: the dAImond source in this repository, LLVM (`llc`, `ld.lld` and `libLLVM.so`), and the C library, which the runtime calls and whose start-up objects every program links. No C is compiled. `bootstrap.sh` is a POSIX shell script, because nothing written in dAImond can run before the seed is linked; after that the compiler starts the linker and the programs it runs itself, with no shell and without `llvm-config`, and the files it generates (`main.dm`, `prelude.dm`) are written by a dAImond program.

## Status

The compiler passes its suite of 572 cases, builds itself to a fixed point, and builds the examples in `examples/`.

### What works (each has cases in `tests/cases/`)

- Structs, enums with payloads, `impl` methods with bare `self`, operator overloading through `add`, `sub`, `mul`, `div`, `eq`, `lt` and the other comparison methods; compound assignment with `+=`, `-=`, `*=`, `/=` and `%=`
- Pattern matching on enums, `Option`, `Result`, strings and numbers, with guards and with literals among a variant's values (`Token.Number(0) =>`); `?` error propagation
- `==` and `!=` on structs, enums, `Option`, `Result` and lists compare the values part by part; a type's own `eq` method is used where it has one
- Generic functions (monomorphized, with type arguments inferred at the call, and trait bounds: `fn f[T: Show + Named]`) generic structs (`struct Table[K, V]`, with type arguments written out) and generic enums (`enum Opt[T]`, with type arguments found from the values or the place)
- `List[T]`, `Map[K, V]` and `Box[T]`, `for`-in over lists, maps and ranges
- Traits: methods with bodies that every implementing type gets, impls checked against the trait, and `dyn Trait` values that hold any implementing type and dispatch at run time
- Lambdas and function values that are closures: a lambda keeps the values of the variables it uses and can be passed, returned and stored. The pipeline operator `|>`, bitwise operators (`&`, `|`, `^`, `<<`, `>>`, `~`)
- String interpolation (`f"..."`), all numeric types (`i8` to `u64`, `f32`, `f64`) with `as` casts
- Compile-time evaluation (`comptime`, and every `const`): numbers, strings, lists, loops and calls of the program's functions; what cannot be evaluated is a compile error. Compile-time array sizes
- Regions, `async`/`await`, SIMD vectors
- Effects (`with [Console, FileSystem]`), also user-defined ones
- Multi-file imports, and the standard library in `stdlib/` (`import std.math`)
- `extern fn` with C-width integer types, variadic C functions and link names; raw memory inside `unsafe`
- Files, directories, paths, the environment, processes, time, the terminal, TCP sockets, threads and mutexes
- Runtime checks: integer overflow, division by zero, shift amounts, list bounds, narrowing casts
- A type checker: mismatched types, undefined names and functions, wrong arguments, changes to what is not `mut`, missing returns, matches that leave a variant out and effects that are not declared are compile errors
- Tooling: `run`, `check`, `fmt`, `test`, debug info (`-g`), the language server (errors as you type, hover, go to definition, references, outline, completion, signature help, formatting)

### Known gaps

- The type checker reports a mismatch only where it knows both types. A generic function is checked for each set of type arguments the program uses it with, so an error that depends on them is reported for that use, and not for a generic function that is never called.
- A lambda has copies of the variables it uses: it cannot change the originals, and does not see later changes to them.
- `async fn` and `await` are synchronous: `await f()` runs `f` to its end.
- Strings are never freed, and neither is what a region allocates.
- Only x86-64 Linux with glibc is supported.
- An expression nested several hundred levels deep overflows the compiler's stack.
- Debug info has one scope per function, and does not describe `match` bindings or captured variables.
- There is no package manager.

## History

dAImond was first implemented by a compiler written in Zig that emitted C ("Stage 0"), then by a C-emitting compiler written in dAImond ("Stage 1"), an LLVM backend in Zig ("Stage 3") and one in dAImond ("Stage 4"). The compiler in `stage5/` grew out of Stage 4. Once it could build itself from a seed, pass the suite and replace the older compilers' tools, the others were deleted, along with the C runtime and the C bridge to LLVM. They are in the repository's history up to commit `2ddc25f`; `docs/stage-history.md` says more.

## Design Principles

1. **One obvious way to do anything**
2. **Reading code reveals its cost**
3. **Syntax should be greppable**
4. **No magic, no surprises**
5. **Familiar to C/Rust/Zig programmers**

## Contributing

See `CLAUDE.md` for detailed development guidance.

## License

MIT License - see [LICENSE](LICENSE) for details.
