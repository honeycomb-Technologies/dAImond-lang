// The C that is left of the Stage 5 runtime. The rest of it is the
// dAImond code in this directory (string.dm, convert.dm, list.dm,
// map.dm, os.dm, path.dm, io.dm).

#include <sys/resource.h>

// Increase default stack size to 64MB for LLVM-compiled binaries.
// LLVM passes large structs by value through recursive calls, consuming
// more stack than equivalent C code (e.g., the Stage 1 compiler's ~922-byte
// Compiler struct through recursive descent parsing).
__attribute__((constructor))
static void dm_increase_stack_size(void) {
    struct rlimit rl;
    if (getrlimit(RLIMIT_STACK, &rl) == 0 && rl.rlim_cur < 64 * 1024 * 1024) {
        rl.rlim_cur = 64 * 1024 * 1024;
        setrlimit(RLIMIT_STACK, &rl);
    }
}
