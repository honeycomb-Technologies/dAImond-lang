# dAImond Intermediate Representation (IR) Specification

## Overview

The dAImond IR is an SSA-based, typed intermediate representation that sits between the AST and LLVM IR in the Stage 3 compiler pipeline:

```
AST -> dAImond IR -> LLVM IR -> Machine Code
```

The IR is designed to be:
- **Explicit**: All types are annotated, no implicit conversions
- **SSA**: Every value is assigned exactly once (phi nodes at join points)
- **Lowered**: High-level constructs (match, for-in, closures) are desugared
- **Optimizable**: Amenable to standard SSA optimizations before LLVM lowering

## Type System

### Primitive Types

```
i8, i16, i32, i64     -- Signed integers
u8, u16, u32, u64     -- Unsigned integers
f32, f64              -- IEEE 754 floating point
bool                  -- 1-bit boolean
void                  -- Unit type (no value)
ptr                   -- Raw pointer (untyped, for GEP/load/store)
```

### Aggregate Types

```
struct { field0: T0, field1: T1, ... }  -- Named struct (lowered from dAImond structs)
array(T, N)                              -- Fixed-size array [T; N]
```

### Reference Types

```
ptr(T)        -- Typed pointer to T (Box[T], &T)
slice(T)      -- Fat pointer: { ptr: ptr(T), len: i64 }  (List[T] data)
string        -- dm_string struct: { data: ptr(u8), len: i64, cap: i64 }
```

### Function Types

```
fn(T0, T1, ...) -> R            -- Direct function
fn_ptr(T0, T1, ...) -> R        -- Function pointer (closures, dyn dispatch)
```

### Enum/Variant Types

```
tagged_union { tag: i32, payload: union { V0: T0, V1: T1, ... } }
option(T) = tagged_union { tag: i32, payload: union { some: T } }
result(T, E) = tagged_union { tag: i32, payload: union { ok: T, err: E } }
```

## Instructions

### Value Instructions (produce a result)

```
%v = const.i64 42                        -- Integer constant
%v = const.f64 3.14                      -- Float constant
%v = const.bool true                     -- Boolean constant
%v = const.string "hello"                -- String literal (global)

%v = add %a, %b                          -- Arithmetic (i8-i64, u8-u64, f32, f64)
%v = sub %a, %b
%v = mul %a, %b
%v = div %a, %b                          -- Safe division (checked, panics on zero)
%v = mod %a, %b                          -- Safe modulo (checked)

%v = eq %a, %b                           -- Comparison (any comparable type)
%v = ne %a, %b
%v = lt %a, %b
%v = le %a, %b
%v = gt %a, %b
%v = ge %a, %b

%v = and %a, %b                          -- Logical (bool)
%v = or %a, %b
%v = not %a

%v = bitand %a, %b                       -- Bitwise (integers)
%v = bitor %a, %b
%v = bitxor %a, %b
%v = shl %a, %b
%v = shr %a, %b

%v = cast %a to T                        -- Numeric cast (as)
%v = trunc %a to T                       -- Narrowing integer cast
%v = zext %a to T                        -- Zero-extend
%v = sext %a to T                        -- Sign-extend
%v = fpext %a to T                       -- Float extend (f32 -> f64)
%v = fptrunc %a to T                     -- Float truncate (f64 -> f32)
%v = fptoi %a to T                       -- Float to int
%v = itofp %a to T                       -- Int to float

%v = alloca T                            -- Stack allocation
%v = load %ptr                           -- Load from pointer
%v = gep %ptr, %idx                      -- Get element pointer (struct field, array index)
%v = extractfield %struct, N             -- Extract field N from struct value

%v = call @fn(%a, %b, ...)              -- Direct function call
%v = callptr %fptr(%a, %b, ...)         -- Indirect call (closures, dyn dispatch)

%v = phi [%a, bb0], [%b, bb1]           -- SSA phi node

%v = string.concat %a, %b               -- String concatenation
%v = string.len %s                       -- String length
%v = string.eq %a, %b                   -- String equality

%v = list.new T                          -- Create empty list
%v = list.push %list, %val              -- Append to list (returns new list)
%v = list.get %list, %idx               -- Bounds-checked index
%v = list.len %list                     -- List length
%v = list.pop %list                     -- Remove last (returns value)

%v = arena.alloc %arena, %size          -- Allocate from arena
```

### Void Instructions (no result)

```
store %val, %ptr                         -- Store to pointer
br bb_target                             -- Unconditional branch
br.cond %cond, bb_true, bb_false        -- Conditional branch
ret %val                                 -- Return value
ret.void                                 -- Return void
panic %msg                               -- Runtime panic (unreachable after)
```

## Control Flow

The IR uses basic blocks with explicit branches (no fall-through):

```
fn @factorial(%n: i64) -> i64 {
entry:
    %cond = le %n, const.i64 1
    br.cond %cond, bb_base, bb_rec

bb_base:
    ret const.i64 1

bb_rec:
    %n1 = sub %n, const.i64 1
    %r = call @factorial(%n1)
    %result = mul %n, %r
    ret %result
}
```

## Lowering from AST

### Match Expressions

Match arms lower to a chain of conditional branches:

```daimond
match shape {
    Circle(r) => 3.14 * r * r,
    Rect(w, h) => w * h,
    _ => 0.0
}
```

Lowers to:
```
%tag = extractfield %shape, 0
%is_circle = eq %tag, const.i32 0
br.cond %is_circle, bb_circle, bb_check_rect

bb_circle:
    %r = extractfield %shape.payload.circle, 0
    %r2 = mul %r, %r
    %area = mul const.f64 3.14, %r2
    br bb_join

bb_check_rect:
    %is_rect = eq %tag, const.i32 1
    br.cond %is_rect, bb_rect, bb_wildcard

bb_rect:
    %w = extractfield %shape.payload.rect, 0
    %h = extractfield %shape.payload.rect, 1
    %area2 = mul %w, %h
    br bb_join

bb_wildcard:
    br bb_join

bb_join:
    %result = phi [%area, bb_circle], [%area2, bb_rect], [const.f64 0.0, bb_wildcard]
```

### Closures

Closures lower to a struct (captured environment) + function pointer:

```daimond
let offset = 10
let f = |x: int| -> int { return x + offset }
```

Lowers to:
```
%env = alloca { i64 }                   -- Closure environment struct
%env_field0 = gep %env, 0
store %offset, %env_field0              -- Capture offset

-- Lifted closure function:
fn @closure_0(%env_ptr: ptr, %x: i64) -> i64 {
    %env = load %env_ptr as ptr({ i64 })
    %offset = extractfield %env, 0
    %result = add %x, %offset
    ret %result
}
```

### Regions

Region blocks lower to arena create/destroy with allocation redirection:

```daimond
region mem {
    let s = "hello" + " " + "world"
}
```

Lowers to:
```
%arena = call @dm_arena_create(const.i64 4096)
%s1 = string.concat.arena %arena, const.string "hello", const.string " "
%s2 = string.concat.arena %arena, %s1, const.string "world"
call @dm_arena_destroy(%arena)
```

### Async/Await

Phase B async functions lower to state machine structs:

```
-- Frame struct for each async function
struct @async_frame_foo {
    state: i32,          -- Current state (0 = initial, 1 = after await, ...)
    result: T,           -- Final result
    ready: bool,         -- Whether result is ready
    -- Captured locals that live across await points:
    saved_x: i64,
    saved_y: string,
}

fn @foo_poll(%frame: ptr(@async_frame_foo)) -> bool {
    %state = load (gep %frame, 0)
    switch %state {
        0 -> bb_state0,
        1 -> bb_state1,
    }
    ...
}
```

## Optimization Passes (Pre-LLVM)

1. **Dead code elimination**: Remove unreachable blocks and unused values
2. **Constant folding**: Evaluate compile-time expressions (comptime)
3. **Inline small functions**: Functions under 5 instructions
4. **Escape analysis**: Determine if allocations can be stack-promoted
5. **Region promotion**: Move heap allocations into regions when lifetime is bounded

These are performed on the dAImond IR before lowering to LLVM IR, since they benefit from dAImond-specific knowledge (regions, effects, ownership).

## Serialization

The IR can be serialized to a text format (for debugging) or a binary format (for caching):

```
; Text format
define i64 @add(i64 %a, i64 %b) {
entry:
    %result = add %a, %b
    ret %result
}
```

The binary format uses a simple bytecode encoding for compact representation.
