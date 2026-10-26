#!/bin/bash
# Concatenate Stage 4 modules into a single main.dm for bootstrap compilation
# Usage: cd stage4 && bash gen_bootstrap.sh

set -e

OUTPUT="main.dm"

# Order matters: dependencies before dependents
cat \
    token.dm \
    lexer.dm \
    ast.dm \
    ir.dm \
    ir_builder.dm \
    llvm.dm \
    parser.dm \
    ir_gen.dm \
    ir_gen_decl.dm \
    ir_gen_stmt.dm \
    ir_gen_expr.dm \
    ir_gen_builtins.dm \
    ir_gen_comptime.dm \
    llvm_gen.dm \
    llvm_gen_call.dm \
    llvm_gen_simd.dm \
    main_split.dm \
| sed '/^module /d' | sed '/^import /d' > "$OUTPUT"

# Add module declaration at the top
sed -i '1i module main' "$OUTPUT"

echo "Generated $OUTPUT"
wc -l "$OUTPUT"
