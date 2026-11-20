# Pending cases

Cases that the Stage 0 compiler passes and the Stage 5 compiler does not yet. They come from the Stage 0 suites that Stage 5's suite never ran. Each stays here until Stage 5 passes it, and then moves to the suite of the same name in `tests/cases/`.

```bash
build/harness build/daimond tests/pending
```

What they need from Stage 5:

- `constructors/`: `Box.new(x)` and `Box.null()` spelled with a dot, and functions of an `impl` called on the type (`Point.new(1, 2)`). `user_enum_new` crashes the compiler.
- `unified_len/`: `s.len()` on a string, and a compile error when the result of `len` is used as a string.
- `error_quality/`: a compile error for a value of the wrong type (`let x: int = "text"`, a string passed for an `int`, `len(42)`). The `.errors` files of the two type-mismatch cases hold Stage 0's wording (`Error[E0100]`, `expected int`, `found string`); change them to Stage 5's wording when it reports these errors.
