#!/bin/bash
# Cross-Language Benchmark Suite - Compile Only
# Compiles all benchmark implementations without running them.
# Usage: cd benchmarks/cross-lang && bash build.sh [benchmark_name]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
COMPILER="$ROOT_DIR/stage0/zig-out/bin/daimond"
BUILD_DIR="$SCRIPT_DIR/build"

# Colors
BOLD='\033[1m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BOLD}=====================================================================${NC}"
echo -e "${BOLD}        dAImond Cross-Language Benchmark Suite - Build Only           ${NC}"
echo -e "${BOLD}=====================================================================${NC}"
echo ""

# Check prerequisites
HAS_DAIMOND=0; HAS_GCC=0; HAS_RUSTC=0; HAS_GO=0; HAS_ZIG=0; HAS_BUN=0

echo -e "${CYAN}Checking tools...${NC}"
[ -f "$COMPILER" ] && HAS_DAIMOND=1 && echo -e "  ${GREEN}[ok]${NC} daimond found" || echo -e "  ${YELLOW}[--]${NC} daimond not found (run: cd stage0 && zig build)"
command -v gcc &>/dev/null && HAS_GCC=1 && echo -e "  ${GREEN}[ok]${NC} gcc found" || echo -e "  ${YELLOW}[--]${NC} gcc not found"
command -v rustc &>/dev/null && HAS_RUSTC=1 && echo -e "  ${GREEN}[ok]${NC} rustc found" || echo -e "  ${YELLOW}[--]${NC} rustc not found"
command -v go &>/dev/null && HAS_GO=1 && echo -e "  ${GREEN}[ok]${NC} go found" || echo -e "  ${YELLOW}[--]${NC} go not found"
command -v zig &>/dev/null && HAS_ZIG=1 && echo -e "  ${GREEN}[ok]${NC} zig found" || echo -e "  ${YELLOW}[--]${NC} zig not found"
command -v bun &>/dev/null && HAS_BUN=1 && echo -e "  ${GREEN}[ok]${NC} bun found" || echo -e "  ${YELLOW}[--]${NC} bun not found"

mkdir -p "$BUILD_DIR"
echo ""

# Benchmark definitions
BENCHMARKS=(fibonacci mandelbrot nbody binary_trees list_stress json_parse string_concat hashmap sort parallel_fib)

# If specific benchmark requested
if [ -n "$1" ]; then
    BENCHMARKS=("$1")
fi

SUCCESS=0
FAIL=0
SKIP=0

# Compile function
compile_bench() {
    local name="$1"
    local dir="$SCRIPT_DIR/$name"
    echo -e "${CYAN}Compiling $name...${NC}"

    # Auto-detect source files
    local dm_src="" c_src="" rs_src="" go_src="" zig_src="" js_src=""
    for f in "$dir"/*.dm; do [ -f "$f" ] && dm_src="$f" && break; done
    for f in "$dir"/*.c; do [ -f "$f" ] && c_src="$f" && break; done
    for f in "$dir"/*.rs; do [ -f "$f" ] && rs_src="$f" && break; done
    for f in "$dir"/*.go; do [ -f "$f" ] && go_src="$f" && break; done
    for f in "$dir"/*.zig; do [ -f "$f" ] && zig_src="$f" && break; done
    for f in "$dir"/*.js; do [ -f "$f" ] && js_src="$f" && break; done

    # Compile each language
    if [ "$HAS_DAIMOND" -eq 1 ] && [ -n "$dm_src" ]; then
        if "$COMPILER" "$dm_src" -o "$BUILD_DIR/${name}_dm" -O3 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} dAImond"
            SUCCESS=$((SUCCESS + 1))
        else
            echo -e "  ${RED}[!!]${NC} dAImond compile failed"
            FAIL=$((FAIL + 1))
        fi
    fi
    if [ "$HAS_GCC" -eq 1 ] && [ -n "$c_src" ]; then
        if gcc -O3 -o "$BUILD_DIR/${name}_c" "$c_src" -lm -lpthread 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} C"
            SUCCESS=$((SUCCESS + 1))
        else
            echo -e "  ${RED}[!!]${NC} C compile failed"
            FAIL=$((FAIL + 1))
        fi
    fi
    if [ "$HAS_RUSTC" -eq 1 ] && [ -n "$rs_src" ]; then
        if rustc -C opt-level=3 -o "$BUILD_DIR/${name}_rs" "$rs_src" 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} Rust"
            SUCCESS=$((SUCCESS + 1))
        else
            echo -e "  ${RED}[!!]${NC} Rust compile failed"
            FAIL=$((FAIL + 1))
        fi
    fi
    if [ "$HAS_GO" -eq 1 ] && [ -n "$go_src" ]; then
        if go build -o "$BUILD_DIR/${name}_go" "$go_src" 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} Go"
            SUCCESS=$((SUCCESS + 1))
        else
            echo -e "  ${RED}[!!]${NC} Go compile failed"
            FAIL=$((FAIL + 1))
        fi
    fi
    if [ "$HAS_ZIG" -eq 1 ] && [ -n "$zig_src" ]; then
        if (cd "$BUILD_DIR" && zig build-exe -O ReleaseFast "$zig_src" --name "${name}_zig" 2>/dev/null); then
            echo -e "  ${GREEN}[ok]${NC} Zig"
            SUCCESS=$((SUCCESS + 1))
        else
            echo -e "  ${RED}[!!]${NC} Zig compile failed"
            FAIL=$((FAIL + 1))
        fi
    fi
    if [ "$HAS_BUN" -eq 1 ] && [ -n "$js_src" ]; then
        echo -e "  ${GREEN}[ok]${NC} Bun (interpreted, no compilation needed)"
        SUCCESS=$((SUCCESS + 1))
    fi
}

# Main loop
for bench in "${BENCHMARKS[@]}"; do
    if [ ! -d "$SCRIPT_DIR/$bench" ]; then
        echo -e "${YELLOW}Skipping $bench (directory not found)${NC}"
        SKIP=$((SKIP + 1))
        continue
    fi
    compile_bench "$bench"
done

echo ""
echo -e "${BOLD}=== Build Summary ===================================================${NC}"
echo -e "  ${GREEN}Success: $SUCCESS${NC}"
echo -e "  ${RED}Failed:  $FAIL${NC}"
echo -e "  ${YELLOW}Skipped: $SKIP${NC}"
echo -e "  Build output: ${CYAN}$BUILD_DIR/${NC}"
echo -e "${BOLD}Done.${NC}"
