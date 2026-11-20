# The seed

`daimond.bc` is the dAImond compiler (Stage 5) as LLVM bitcode for x86-64 Linux. It is what the compiler is built from when no dAImond compiler exists yet: `bash bootstrap.sh`, in the repository's root, turns it into a compiler with LLVM's `llc` and `ld.lld`, and that compiler then compiles the source in `stage5/`.

- The seed is a build product, not source. Do not edit it.
- It was written by LLVM 19. A newer LLVM reads it; an older one does not.
- It does not have to match the source. An older seed is fine as long as its compiler can still compile `stage5/main.dm`; `bootstrap.sh` checks that the result reproduces itself.
- `BUILD_ID` is the build ID of the source the seed was made from (`runtime_build_id` in `stage5/prelude.dm`).

To write the seed again after changing the compiler:

```bash
cd stage5 && bash gen_bootstrap.sh && cd ..
LLVM_BIN=/usr/lib/llvm-19/bin bash bootstrap.sh --update-seed
```

Write it with LLVM 19, the oldest LLVM the compiler is built with: a seed is readable by the LLVM that wrote it and by newer ones, so a seed written by LLVM 23 could not be read where only LLVM 19 is installed. `bootstrap.sh` takes the newest LLVM it finds unless `LLVM_BIN` names another.

Write it again when the compiler's source starts to use a language feature that the seed's compiler does not have, and before that feature is used: first teach the compiler the feature and update the seed, then use the feature in the compiler.
