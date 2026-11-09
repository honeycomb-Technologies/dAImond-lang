#!/bin/bash
# Build Stage 0, build the Stage 5 compiler binary, build the Stage 5
# parity harness, and run the shared integration suite.
# Usage: bash stage5/run_parity.sh   (from anywhere)
# Env:   ZIG=/path/to/zig  DAIMOND_STAGE5_BIN=/path/to/stage5_bin

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Zig 0.13.0 (not on PATH on the dev machine; mise direct binary path)
ZIG="${ZIG:-$HOME/.local/share/mise/installs/zig/0.13.0/zig}"
if [ ! -x "$ZIG" ]; then
    ZIG="$(command -v zig || true)"
fi
if [ -z "$ZIG" ] || [ ! -x "$ZIG" ]; then
    echo "error: zig not found (set ZIG=/path/to/zig)" >&2
    exit 1
fi

STAGE5_BIN="${DAIMOND_STAGE5_BIN:-/tmp/stage5_bin}"
export DAIMOND_STAGE5_BIN="$STAGE5_BIN"

echo "==> Building Stage 0"
(cd "$ROOT/stage0" && "$ZIG" build)

echo "==> Building Stage 5 compiler -> $STAGE5_BIN"
cd "$ROOT/stage0"
./zig-out/bin/daimond ../stage5/main.dm -c -o /tmp/stage5.c
gcc -c /tmp/stage5.c -o /tmp/stage5.o -I runtime -Wno-incompatible-pointer-types -Wno-int-conversion
gcc -c ../stage5/llvm_bridge.c -o /tmp/llvm_bridge.o $(llvm-config --cflags)
gcc -c ../stage5/runtime/llvm_wrappers.c -o /tmp/llvm_wrappers.o -I runtime
gcc -c runtime/daimond_runtime.c -o /tmp/daimond_runtime.o
gcc /tmp/stage5.o /tmp/llvm_bridge.o /tmp/llvm_wrappers.o /tmp/daimond_runtime.o -o "$STAGE5_BIN" -lm -lpthread $(llvm-config --libs --ldflags)

echo "==> Building Stage 5 parity harness"
(cd "$ROOT/stage5" && "$ZIG" build)

echo "==> Running Stage 5 integration tests (compiler: $STAGE5_BIN)"
# Must run from stage5/ so the compiler finds the C runtime/wrappers.
cd "$ROOT/stage5"
set +e
./zig-out/bin/test-integration 2>&1 | tee /tmp/stage5_parity.log
STATUS=${PIPESTATUS[0]}
set -e
echo "==> Harness exit code: $STATUS (full log: /tmp/stage5_parity.log)"
exit "$STATUS"
