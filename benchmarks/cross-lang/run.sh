#!/bin/bash
# Cross-Language Benchmark Suite for dAImond
# Usage: cd benchmarks/cross-lang && bash run.sh [benchmark_name]
# Requires: hyperfine, gcc, rustc, go, zig, bun, daimond compiler

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
COMPILER="$ROOT_DIR/stage0/zig-out/bin/daimond"
BUILD_DIR="$SCRIPT_DIR/build"
RESULTS_DIR="$SCRIPT_DIR/results"

# Colors
BOLD='\033[1m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BOLD}=====================================================================${NC}"
echo -e "${BOLD}        dAImond Cross-Language Benchmark Suite                        ${NC}"
echo -e "${BOLD}=====================================================================${NC}"
echo ""

# Check prerequisites
HAS_DAIMOND=0; HAS_GCC=0; HAS_RUSTC=0; HAS_GO=0; HAS_ZIG=0; HAS_BUN=0; HAS_HYPERFINE=0

echo -e "${CYAN}Checking tools...${NC}"
[ -f "$COMPILER" ] && HAS_DAIMOND=1 && echo -e "  ${GREEN}[ok]${NC} daimond found" || echo -e "  ${YELLOW}[--]${NC} daimond not found (run: cd stage0 && zig build)"
command -v gcc &>/dev/null && HAS_GCC=1 && echo -e "  ${GREEN}[ok]${NC} gcc found" || echo -e "  ${YELLOW}[--]${NC} gcc not found"
command -v rustc &>/dev/null && HAS_RUSTC=1 && echo -e "  ${GREEN}[ok]${NC} rustc found" || echo -e "  ${YELLOW}[--]${NC} rustc not found"
command -v go &>/dev/null && HAS_GO=1 && echo -e "  ${GREEN}[ok]${NC} go found" || echo -e "  ${YELLOW}[--]${NC} go not found"
command -v zig &>/dev/null && HAS_ZIG=1 && echo -e "  ${GREEN}[ok]${NC} zig found" || echo -e "  ${YELLOW}[--]${NC} zig not found"
command -v bun &>/dev/null && HAS_BUN=1 && echo -e "  ${GREEN}[ok]${NC} bun found" || echo -e "  ${YELLOW}[--]${NC} bun not found"
command -v hyperfine &>/dev/null && HAS_HYPERFINE=1 && echo -e "  ${GREEN}[ok]${NC} hyperfine found" || echo -e "  ${RED}[!!]${NC} hyperfine not found (required! install: cargo install hyperfine)"

if [ "$HAS_HYPERFINE" -eq 0 ]; then
    echo -e "\n${RED}ERROR: hyperfine is required. Install with: cargo install hyperfine${NC}"
    exit 1
fi

mkdir -p "$BUILD_DIR" "$RESULTS_DIR"
echo ""

# Benchmark definitions
BENCHMARKS=(fibonacci mandelbrot nbody binary_trees list_stress json_parse string_concat hashmap sort parallel_fib)

# If specific benchmark requested
if [ -n "$1" ]; then
    BENCHMARKS=("$1")
fi

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
        else
            echo -e "  ${RED}[!!]${NC} dAImond compile failed"
        fi
    fi
    if [ "$HAS_GCC" -eq 1 ] && [ -n "$c_src" ]; then
        if gcc -O3 -o "$BUILD_DIR/${name}_c" "$c_src" -lm -lpthread 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} C"
        else
            echo -e "  ${RED}[!!]${NC} C compile failed"
        fi
    fi
    if [ "$HAS_RUSTC" -eq 1 ] && [ -n "$rs_src" ]; then
        if rustc -C opt-level=3 -o "$BUILD_DIR/${name}_rs" "$rs_src" 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} Rust"
        else
            echo -e "  ${RED}[!!]${NC} Rust compile failed"
        fi
    fi
    if [ "$HAS_GO" -eq 1 ] && [ -n "$go_src" ]; then
        if go build -o "$BUILD_DIR/${name}_go" "$go_src" 2>/dev/null; then
            echo -e "  ${GREEN}[ok]${NC} Go"
        else
            echo -e "  ${RED}[!!]${NC} Go compile failed"
        fi
    fi
    if [ "$HAS_ZIG" -eq 1 ] && [ -n "$zig_src" ]; then
        if (cd "$BUILD_DIR" && zig build-exe -O ReleaseFast "$zig_src" --name "${name}_zig" 2>/dev/null); then
            echo -e "  ${GREEN}[ok]${NC} Zig"
        else
            echo -e "  ${RED}[!!]${NC} Zig compile failed"
        fi
    fi
    # JS doesn't need compilation
    if [ "$HAS_BUN" -eq 1 ] && [ -n "$js_src" ]; then
        echo -e "  ${GREEN}[ok]${NC} Bun (interpreted)"
    fi
}

# Run benchmark with hyperfine
run_bench() {
    local name="$1"
    local dir="$SCRIPT_DIR/$name"
    local cmds=()
    local names=()

    echo -e "\n${BOLD}=== $name ===================================================${NC}"

    if [ -f "$BUILD_DIR/${name}_dm" ]; then
        cmds+=("$BUILD_DIR/${name}_dm")
        names+=("dAImond")
    fi
    if [ -f "$BUILD_DIR/${name}_c" ]; then
        cmds+=("$BUILD_DIR/${name}_c")
        names+=("C")
    fi
    if [ -f "$BUILD_DIR/${name}_rs" ]; then
        cmds+=("$BUILD_DIR/${name}_rs")
        names+=("Rust")
    fi
    if [ -f "$BUILD_DIR/${name}_go" ]; then
        cmds+=("$BUILD_DIR/${name}_go")
        names+=("Go")
    fi
    if [ -f "$BUILD_DIR/${name}_zig" ]; then
        cmds+=("$BUILD_DIR/${name}_zig")
        names+=("Zig")
    fi

    # Find JS source
    local js_src=""
    for f in "$dir"/*.js; do [ -f "$f" ] && js_src="$f" && break; done
    if [ "$HAS_BUN" -eq 1 ] && [ -n "$js_src" ]; then
        cmds+=("bun $js_src")
        names+=("Bun")
    fi

    if [ ${#cmds[@]} -lt 2 ]; then
        echo -e "  ${YELLOW}Skipping (need at least 2 implementations)${NC}"
        return
    fi

    # Build hyperfine args
    local args=("--warmup" "3" "--min-runs" "10")
    for i in "${!cmds[@]}"; do
        args+=("-n" "${names[$i]}" "${cmds[$i]}")
    done
    args+=("--export-markdown" "$RESULTS_DIR/${name}.md")

    hyperfine "${args[@]}"
}

# Main loop
for bench in "${BENCHMARKS[@]}"; do
    if [ ! -d "$SCRIPT_DIR/$bench" ]; then
        echo -e "${YELLOW}Skipping $bench (directory not found)${NC}"
        continue
    fi
    compile_bench "$bench"
    run_bench "$bench"
done

echo ""
echo -e "${BOLD}=== Results Summary =================================================${NC}"
echo -e "Individual results saved in: ${CYAN}$RESULTS_DIR/${NC}"
echo ""
for bench in "${BENCHMARKS[@]}"; do
    if [ -f "$RESULTS_DIR/${bench}.md" ]; then
        echo -e "${BOLD}--- $bench ---${NC}"
        cat "$RESULTS_DIR/${bench}.md"
        echo ""
    fi
done
echo -e "${BOLD}Done.${NC}"
