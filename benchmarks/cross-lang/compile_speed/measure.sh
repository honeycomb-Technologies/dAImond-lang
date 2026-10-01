#!/bin/bash
# Compile Speed Benchmark
# Measures how fast each language's compiler can compile a ~500 line source file.
# Uses -O0 / Debug to measure compiler throughput, not optimization time.

set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(dirname "$(dirname "$DIR")")"
BUILD_DIR="$DIR/build"
mkdir -p "$BUILD_DIR"

echo "=== Compile Speed Benchmark ==="
echo ""

# Check for hyperfine
if ! command -v hyperfine &>/dev/null; then
    echo "ERROR: hyperfine is required. Install with: cargo install hyperfine"
    exit 1
fi

# Check for dAImond compiler
DAIMOND="$ROOT/stage0/zig-out/bin/daimond"
if [ ! -f "$DAIMOND" ]; then
    echo "Building dAImond compiler..."
    (cd "$ROOT/stage0" && zig build)
fi

# Build command list dynamically based on available tools
CMDS=()
NAMES=()

if [ -f "$DAIMOND" ]; then
    CMDS+=("$DAIMOND $DIR/sample.dm -o $BUILD_DIR/sample_dm -O0")
    NAMES+=("dAImond")
fi

if command -v gcc &>/dev/null; then
    CMDS+=("gcc -O0 -o $BUILD_DIR/sample_c $DIR/sample.c -lm")
    NAMES+=("C (gcc)")
fi

if command -v rustc &>/dev/null; then
    CMDS+=("rustc -o $BUILD_DIR/sample_rs $DIR/sample.rs 2>/dev/null")
    NAMES+=("Rust")
fi

if command -v go &>/dev/null; then
    CMDS+=("go build -o $BUILD_DIR/sample_go $DIR/sample.go")
    NAMES+=("Go")
fi

if command -v zig &>/dev/null; then
    # Zig build-exe outputs to current directory; use --name and move
    CMDS+=("cd $BUILD_DIR && zig build-exe -O Debug $DIR/sample.zig --name sample_zig 2>/dev/null")
    NAMES+=("Zig")
fi

if [ ${#CMDS[@]} -lt 2 ]; then
    echo "ERROR: Need at least 2 compilers available to benchmark."
    echo "Available: ${NAMES[*]}"
    exit 1
fi

echo "Compilers found: ${NAMES[*]}"
echo "Measuring compilation time (no optimization)..."
echo ""

# Build hyperfine arguments
ARGS=("--warmup" "2" "--min-runs" "10")
for i in "${!CMDS[@]}"; do
    ARGS+=("-n" "${NAMES[$i]}" "${CMDS[$i]}")
done
ARGS+=("--export-markdown" "$DIR/results.md")

hyperfine "${ARGS[@]}"

echo ""
echo "Results saved to $DIR/results.md"
