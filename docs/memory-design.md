# Memory in dAImond: what is true today, and the design it is going to

This document records how memory behaves in programs the compiler builds
today, where that falls short of the language's goal ("memory-safe without
garbage collection, region-based with full inference"), and the design
that closes the gap. The first part is a description of the code. The
second part is a plan: nothing in it is implemented.

## Today

### Nothing is freed

A program takes memory from libc's `malloc` and never gives it back. That
covers the bytes of every string a program builds, the elements of every
list, what a `Box` points at, a closure, and the storage of a `dyn` value.
`region name { ... }` frees nothing when it ends, and `mem_mark` and
`mem_sweep` do nothing.

A program's memory therefore grows until it ends. A short-running program
(the compiler itself: about 2 GB for a self-build) is not harmed. A
program that runs for a long time has to work in storage it reuses, as the
language server does (`lsp/buffer.dm`).

What the compiler does enforce about regions: a value made inside a region
may not be returned from it, assigned to a variable declared outside it,
or stored in a container declared outside it. These are compile errors
("region escape"), so that a region's memory can be released at its end
once the runtime does that.

### Values and copies

| Type | A copy is | Shared with the original |
|------|-----------|--------------------------|
| numbers, `bool` | the value | nothing |
| struct, enum, `Option`, `Result` | every field | what its fields share |
| `string` | `{ data, len, capacity }` | the bytes, which never change |
| `List[T]` | `{ data, len, capacity }` | the elements (see below) |
| `Map[K, V]` | its four list headers and its count | the entries (see below) |
| `Box[T]` | the address | the boxed value |
| `fn(...)` value | the closure's address | the closure, which never changes |

A parameter is a copy, unless it is declared `mut`, which passes the
caller's variable itself.

### Lists

A list's elements are in one allocation. The word before them is a count of
the elements the allocation holds. A copy of a list is a copy of its header: both lists
then use that allocation.

- `xs[i] = v` writes in place. A copy that still uses the allocation sees
  the new value.
- `xs.push(v)` writes in place only when `xs` is as long as the
  allocation's count, which says that no list has an element where the new
  one goes. Otherwise `xs` first moves to an allocation of its own, with a
  copy of its elements. **A push never changes an element of another
  list**, and after it the two lists that pushed share nothing.
- `xs.pop()` lowers the allocation's count when `xs` is as long as it, so
  a push after a pop writes in place and a list used as a stack never
  copies. A copy made before the pop still has its element there, and the
  push after the pop writes over it.

So two copies see each other's changes to the elements they both have,
until one of them pushes where the other already has an element. To get a
list that shares nothing, push the elements onto a new list.

Before 2026-11-22 a push always wrote in place when the allocation had
room: `let mut b = a` followed by `b.push(1)` and `a.push(2)` left `b[n]`
as 2.

### Maps

A `Map[K, V]` is a struct of lists (`stage5/runtime/map.dm`): the keys,
values and hashes in insertion order, and a table of slots. A copy shares
all four.

- A key added to one copy is not in the other: the other's lists are
  shorter, and lookups skip a slot that names an entry past them.
- A map whose copy has added a key is rebuilt, into lists of its own,
  before it inserts or removes anything. From then on the two share
  nothing.
- Until that happens, an insert for a key both have, and a remove, change
  the shared entry: both maps see the new value or lose the key, and the
  `len()` of the one that did not remove is one too high until it is
  rebuilt.

Before 2026-11-22 a map behind its copy inserted into the shared table,
and the copy lost keys.

### What is checked

Integer overflow, division by zero, list indexes, narrowing casts and
null `Box` dereferences panic. `unsafe { }` fences the builtins that read
and write raw memory. There is no check on a `Box` whose value another
`Box` copy has changed (boxes are shared by design), and none is needed
for use after free, because nothing is freed.

## The design

The goal is that a program's values behave as values (a copy never changes
when the original does), that memory is released when its owner goes, and
that neither costs a collector or annotations. The three parts depend on
one analysis.

### 1. Ownership inference

Every allocation has one owner: a variable, a field of an owned value, or
an element of an owned list. The compiler decides, for each place a value
of a type that owns memory (string, list, map, box, closure, and structs
that hold them) is used, which of three things the use is:

- **a borrow**: the value is read, or changed in place, and the owner
  keeps it. A parameter that the function does not store, return or
  change is a borrow, and so is the subject of `xs[i]`, `xs.len()`,
  `for x in xs`.
- **a move**: the last use of the owner. `g = f(g)`, `return xs` and
  `let ys = xs` where `xs` is not used again move the value: no copy is
  made and nothing is shared afterwards.
- **a copy**: a use that is neither. The value is copied in full
  (`let mut ys = xs` with `xs` used later), so the two never see each
  other.

This is a liveness analysis over the checked program, per function, with
a summary for each function of what it does with each parameter (borrows,
keeps, returns). It runs after the type checker, on the same tree.
Monomorphized generic functions are analysed per instance, as they are
checked today.

The compiler's own source relies on the move case throughout: a generator
struct is passed, changed and returned, and its lists hold hundreds of
thousands of entries. With inference those stay O(1). Without it, value
semantics would copy every list on every call, which is why the runtime
today shares instead.

### 2. Releasing memory

With one owner per allocation, the compiler emits the release at the end
of the owner's life: the end of the scope of a variable that still owns
its value, the overwrite of a field or an element, the end of a region for
everything made in it. A moved value is not released by the place it left.
A release of a struct releases what its fields own; a release of a list
releases its elements, then its allocation.

Regions then are what the language says: `region r { ... }` allocates
from an arena that is released whole at the closing brace, which the
escape rule already makes safe.

Strings need one decision first: a literal's bytes are in the program's
image and are not released; `string_from_raw` with capacity 0 already
marks a string that does not own its bytes, and the same mark serves here.

### 3. Shared values, where they are meant

`Box[T]` stays the type whose copies share (it is how the compiler's tree
nodes are linked). Under ownership a box has one owner too; a second
handle to the same box is a borrow, and a box that must outlive its owner
is moved.

### Order of work

1. The analysis, reporting only: for each function, which uses are
   borrows, moves and copies. Run it over the compiler and the suite, and
   read the copies it finds; each one is either a real copy the program
   wants or a pattern the analysis should see as a move.
2. Copies: make the copy case copy lists and maps in full. The list
   allocation's count and the map's rebuild-when-behind then have nothing
   left to do and are removed.
3. Releases: strings and lists first, behind a flag, with the suite run
   under a checking allocator; then boxes, closures and regions.
4. The runtime: `mem_mark` and `mem_sweep` are deleted, and the language
   server's reused buffers become ordinary values.

Until step 2, the rules under Today are the language's behaviour, and the
suite pins them (`tests/cases/95_copies`).
