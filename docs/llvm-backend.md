# dAImond LLVM Backend Architecture

## Overview

Stage 3 replaces the C code generation backend with direct LLVM IR emission. This eliminates the dependency on a system C compiler and enables:

- Native code generation without intermediate C files
- Direct access to LLVM optimization passes (-O0 through -O3)
- Debug information generation (DWARF) for source-level debugging
- Cross-compilation via LLVM target triples
- Link-time optimization (LTO)

## Pipeline

```
Source (.dm) -> Lexer -> Parser -> AST -> Checker -> dAImond IR -> LLVM IR -> Object Code -> Linker -> Binary
                                                     ^^^^^^^^^    ^^^^^^^^
                                                     New stages   New stages
```

### Stage 3 Compiler Architecture

```
stage3/
├── src/
│   ├── main.zig          -- CLI entry point (reuse Stage 0 frontend)
│   ├── ir.zig            -- dAImond IR definitions and builders
│   ├── ir_gen.zig        -- AST -> dAImond IR lowering
│   ├── ir_opt.zig        -- dAImond IR optimization passes
│   ├── llvm_gen.zig      -- dAImond IR -> LLVM IR translation
│   ├── llvm_bindings.zig -- Zig bindings to LLVM-C API
│   ├── debug_info.zig    -- DWARF debug info generation
│   └── target.zig        -- Target triple selection and configuration
├── runtime/
│   ├── rt.zig            -- Runtime library (rewritten in Zig for static linking)
│   └── gc.zig            -- Optional reference counting for escaped allocations
└── build.zig             -- Build configuration (links LLVM)
```

## Component Details

### 1. AST -> dAImond IR Lowering (`ir_gen.zig`)

The IR generator walks the typed AST and produces dAImond IR (defined in `ir.zig`). Key lowering decisions:

| AST Construct | IR Lowering |
|--------------|-------------|
| `let x = expr` | `%x = <expr>` (SSA value) |
| `let mut x = expr` | `%x_ptr = alloca T; store %expr, %x_ptr` (mutable via pointer) |
| `if/else` | `br.cond` + basic blocks + phi nodes |
| `while` | Loop header block with back-edge |
| `for item in list` | Index loop: `%i = phi [0, entry], [%i_next, body]; %item = list.get %list, %i` |
| `match expr { ... }` | Tag extraction + conditional branch chain |
| `fn f(x: T) -> R` | `define R @f(T %x) { ... }` |
| `struct S { ... }` | Named struct type |
| `enum E { V1(T), V2 }` | Tagged union type |
| `trait T { ... }` | Vtable struct type |
| `dyn Trait` | Fat pointer: `{ ptr, vtable_ptr }` |
| `impl T for S` | Vtable global constant |
| `region name { ... }` | Arena create/destroy with alloc redirection |
| `async fn` / `await` | State machine frame/poll lowering |
| Closures `\|x\| expr` | Environment struct + lifted function |
| `Box_new(v)` | `call @malloc(sizeof(T)); store %v` |
| `x as T` | Appropriate cast instruction |

### 2. dAImond IR Optimizations (`ir_opt.zig`)

Pre-LLVM optimization passes that leverage dAImond-specific knowledge:

1. **Comptime evaluation**: Fold `comptime` expressions to constants
2. **Region inference**: Identify allocations whose lifetime fits within a region scope, redirect to arena
3. **Effect checking**: Verify effect annotations (already done in checker, but IR can validate)
4. **Escape analysis**: Stack-promote `Box_new` calls when value doesn't escape
5. **String optimization**: Fold constant string concatenations at compile time
6. **Dead code elimination**: Remove unreachable blocks
7. **Constant propagation**: Replace uses of constants

### 3. dAImond IR -> LLVM IR Translation (`llvm_gen.zig`)

Maps dAImond IR instructions to LLVM IR:

| dAImond IR | LLVM IR |
|-----------|---------|
| `%v = add %a, %b` (i64) | `%v = add i64 %a, %b` |
| `%v = add %a, %b` (f64) | `%v = fadd double %a, %b` |
| `%v = div %a, %b` | Call to `@dm_safe_div` (checked) |
| `%v = cast %a to f64` | `%v = sitofp i64 %a to double` |
| `%v = call @fn(...)` | `%v = call T @fn(...)` |
| `%v = gep %p, %i` | `%v = getelementptr T, ptr %p, i64 %i` |
| `%v = phi [...]` | `%v = phi T [...]` |
| `br bb` | `br label %bb` |
| `br.cond %c, bb1, bb2` | `br i1 %c, label %bb1, label %bb2` |
| `%v = string.concat` | `call @dm_string_concat(...)` |
| `%v = list.get` | `call @dm_list_get_checked(...)` |
| `%v = alloca T` | `%v = alloca %T` |
| `store %v, %p` | `store T %v, ptr %p` |
| `%v = load %p` | `%v = load T, ptr %p` |

### 4. LLVM-C API Bindings (`llvm_bindings.zig`)

Zig bindings to the LLVM-C API (`llvm-c/Core.h`, `llvm-c/Analysis.h`, `llvm-c/Target.h`, etc.):

```zig
const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/BitWriter.h");
});

// Key LLVM types used:
// LLVMModuleRef, LLVMBuilderRef, LLVMValueRef, LLVMTypeRef,
// LLVMBasicBlockRef, LLVMTargetMachineRef
```

Required LLVM version: 17+ (for opaque pointers).

### 5. Debug Information (`debug_info.zig`)

Generate DWARF debug info for source-level debugging with gdb/lldb:

- **Compilation unit**: Maps to each `.dm` source file
- **Functions**: `DISubprogram` for each function with parameter info
- **Variables**: `DILocalVariable` for let/let mut bindings
- **Types**: `DIType` for struct, enum, and primitive types
- **Line info**: Source locations via `DILocation` on each instruction
- **Scope info**: Lexical blocks for if/while/region/match bodies

### 6. Target Configuration (`target.zig`)

Support multiple targets via LLVM target triples:

| Platform | Triple |
|---------|--------|
| Linux x86_64 | `x86_64-unknown-linux-gnu` |
| Linux aarch64 | `aarch64-unknown-linux-gnu` |
| macOS x86_64 | `x86_64-apple-darwin` |
| macOS arm64 | `aarch64-apple-darwin` |
| Windows x86_64 | `x86_64-pc-windows-msvc` |

Default: host triple (auto-detected).

## Runtime Library

The Stage 3 runtime is rewritten in Zig (instead of C) for better integration:

```zig
// runtime/rt.zig
pub const dm_string = extern struct {
    data: [*]const u8,
    len: i64,
    cap: i64,
};

pub export fn dm_string_concat(a: dm_string, b: dm_string) dm_string { ... }
pub export fn dm_string_from_cstr(s: [*:0]const u8) dm_string { ... }
pub export fn dm_println_str(s: dm_string) void { ... }
pub export fn dm_arena_create(size: i64) *dm_arena { ... }
// ... etc
```

The runtime is compiled to a static library (`libdaimond_rt.a`) and linked into every compiled program.

## Build Requirements

- Zig 0.13.0+
- LLVM 17+ development headers and libraries
- System linker (ld/lld)

## Migration Path

1. **Phase 1**: Implement IR data structures and AST -> IR lowering for a minimal subset (functions, arithmetic, print)
2. **Phase 2**: Implement LLVM IR generation for the minimal subset, verify output matches C backend
3. **Phase 3**: Extend to all types (structs, enums, generics, closures)
4. **Phase 4**: Add debug info generation
5. **Phase 5**: Add optimization passes
6. **Phase 6**: Bootstrap: Stage 3 compiles Stage 1, verify identical behavior
7. **Phase 7**: Stage 3 compiles itself (full self-hosting with LLVM backend)

## Performance Goals

- Compile time: Within 2x of Stage 0 (C backend) for typical programs
- Runtime performance: At least equal to `-O2` C backend output
- Binary size: Comparable to C backend (no runtime bloat)
- Debug build: Full source-level debugging with DWARF info
