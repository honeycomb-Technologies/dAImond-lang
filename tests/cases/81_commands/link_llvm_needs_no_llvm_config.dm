module main

-- Built with --link-llvm, as the compiler itself is: the LLVM bridge
-- is added to the program, which then needs the LLVM library. The
-- compiler finds the library itself; the `llvm-config` on PATH here
-- fails, and nothing may ask it.
extern fn dm_llvm_context_create() -> int
extern fn dm_llvm_context_dispose(context: int)

fn main() {
    let context = dm_llvm_context_create()
    if context != 0 {
        println("made an LLVM context")
    }
    dm_llvm_context_dispose(context)
}
