#!/bin/bash
# Build Stage 0, build the Stage 4 compiler binary, build the Stage 4
# parity harness, and run the shared 254-case integration suite.
# Usage: bash stage4/run_parity.sh   (from anywhere)
# Env:   ZIG=/path/to/zig  DAIMOND_STAGE4_BIN=/path/to/stage4_bin

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

STAGE4_BIN="${DAIMOND_STAGE4_BIN:-/tmp/stage4_bin}"
export DAIMOND_STAGE4_BIN="$STAGE4_BIN"

echo "==> Building Stage 0"
(cd "$ROOT/stage0" && "$ZIG" build)

echo "==> Building Stage 4 compiler -> $STAGE4_BIN"
cd "$ROOT/stage0"
./zig-out/bin/daimond ../stage4/main.dm --skip-type-check -c -o /tmp/stage4.c
gcc -c /tmp/stage4.c -o /tmp/stage4.o -I runtime -Wno-incompatible-pointer-types -Wno-int-conversion
gcc -c ../stage4/llvm_bridge.c -o /tmp/llvm_bridge.o $(llvm-config --cflags)
gcc -c ../stage4/runtime/llvm_wrappers.c -o /tmp/llvm_wrappers.o -I runtime
gcc -c runtime/daimond_runtime.c -o /tmp/daimond_runtime.o
gcc /tmp/stage4.o /tmp/llvm_bridge.o /tmp/llvm_wrappers.o /tmp/daimond_runtime.o -o "$STAGE4_BIN" -lm -lpthread $(llvm-config --libs --ldflags)

echo "==> Building Stage 4 parity harness"
(cd "$ROOT/stage4" && "$ZIG" build)

echo "==> Running Stage 4 integration tests (compiler: $STAGE4_BIN)"
# Must run from stage4/ so the compiler finds the C runtime/wrappers.
cd "$ROOT/stage4"
set +e
./zig-out/bin/test-integration 2>&1 | tee /tmp/stage4_parity.log
STATUS=${PIPESTATUS[0]}
set -e
echo "==> Harness exit code: $STATUS (full log: /tmp/stage4_parity.log)"
exit "$STATUS"
