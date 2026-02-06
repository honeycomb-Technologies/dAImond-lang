#!/bin/bash
# dAImond Benchmark Suite Runner
# Builds all benchmarks at -O3 and runs them with timing.
# Usage: cd benchmarks && bash run.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
COMPILER="$ROOT_DIR/stage0/zig-out/bin/daimond"
BUILD_DIR="$SCRIPT_DIR/build"
CC="${CC:-cc}"
CFLAGS="-O3 -lm"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Check compiler exists
if [ ! -f "$COMPILER" ]; then
    echo -e "${YELLOW}Building dAImond compiler...${NC}"
    (cd "$ROOT_DIR/stage0" && zig build)
fi

mkdir -p "$BUILD_DIR"

echo -e "${BOLD}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}║           dAImond Benchmark Suite                       ║${NC}"
echo -e "${BOLD}╚══════════════════════════════════════════════════════════╝${NC}"
echo ""

# Function to run a benchmark and capture time
run_bench() {
    local name="$1"
    local binary="$2"
    local start end elapsed

    start=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')
    output=$("$binary" 2>&1) || true
    end=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')

    elapsed=$(( (end - start) / 1000000 ))
    echo "$elapsed"
}

# Results storage
declare -A MICRO_TIMES
declare -A MACRO_DM_TIMES
declare -A MACRO_C_TIMES

# ============================================================
# Micro Benchmarks
# ============================================================
echo -e "${CYAN}── Micro Benchmarks ──────────────────────────────────────${NC}"
echo ""

MICRO_BENCHES=(fibonacci string_concat list_ops struct_create pattern_match map_ops)

for bench in "${MICRO_BENCHES[@]}"; do
    src="$SCRIPT_DIR/micro/${bench}.dm"
    bin="$BUILD_DIR/${bench}"

    if [ ! -f "$src" ]; then
        echo -e "  ${RED}SKIP${NC} $bench (source not found)"
        continue
    fi

    printf "  %-25s" "$bench"

    # Compile
    if ! "$COMPILER" "$src" -o "$bin" -O3 2>/dev/null; then
        echo -e "${RED}COMPILE ERROR${NC}"
        continue
    fi

    # Run with timing
    ms=$(run_bench "$bench" "$bin")
    MICRO_TIMES[$bench]=$ms

    if [ "$ms" -lt 1000 ]; then
        echo -e "${GREEN}${ms}ms${NC}"
    elif [ "$ms" -lt 10000 ]; then
        echo -e "${YELLOW}$(echo "scale=2; $ms/1000" | bc)s${NC}"
    else
        echo -e "${RED}$(echo "scale=2; $ms/1000" | bc)s${NC}"
    fi
done

echo ""

# ============================================================
# Macro Benchmarks (dAImond)
# ============================================================
echo -e "${CYAN}── Macro Benchmarks (dAImond) ────────────────────────────${NC}"
echo ""

MACRO_BENCHES=(json_parser matrix_multiply word_count)

for bench in "${MACRO_BENCHES[@]}"; do
    src="$SCRIPT_DIR/macro/${bench}.dm"
    bin="$BUILD_DIR/${bench}_dm"

    if [ ! -f "$src" ]; then
        echo -e "  ${RED}SKIP${NC} $bench (source not found)"
        continue
    fi

    printf "  %-25s"  "$bench"

    # Compile
    if ! "$COMPILER" "$src" -o "$bin" -O3 2>/dev/null; then
        echo -e "${RED}COMPILE ERROR${NC}"
        continue
    fi

    # Run with timing
    ms=$(run_bench "$bench" "$bin")
    MACRO_DM_TIMES[$bench]=$ms

    if [ "$ms" -lt 1000 ]; then
        echo -e "${GREEN}${ms}ms${NC}"
    elif [ "$ms" -lt 10000 ]; then
        echo -e "${YELLOW}$(echo "scale=2; $ms/1000" | bc)s${NC}"
    else
        echo -e "${RED}$(echo "scale=2; $ms/1000" | bc)s${NC}"
    fi
done

echo ""

# ============================================================
# Macro Benchmarks (C comparison)
# ============================================================
echo -e "${CYAN}── Macro Benchmarks (C comparison) ───────────────────────${NC}"
echo ""

for bench in "${MACRO_BENCHES[@]}"; do
    src="$SCRIPT_DIR/compare/${bench}.c"
    bin="$BUILD_DIR/${bench}_c"

    if [ ! -f "$src" ]; then
        echo -e "  ${RED}SKIP${NC} $bench (source not found)"
        continue
    fi

    printf "  %-25s" "$bench"

    # Compile
    if ! $CC $CFLAGS "$src" -o "$bin" 2>/dev/null; then
        echo -e "${RED}COMPILE ERROR${NC}"
        continue
    fi

    # Run with timing
    ms=$(run_bench "$bench" "$bin")
    MACRO_C_TIMES[$bench]=$ms

    if [ "$ms" -lt 1000 ]; then
        echo -e "${GREEN}${ms}ms${NC}"
    elif [ "$ms" -lt 10000 ]; then
        echo -e "${YELLOW}$(echo "scale=2; $ms/1000" | bc)s${NC}"
    else
        echo -e "${RED}$(echo "scale=2; $ms/1000" | bc)s${NC}"
    fi
done

echo ""

# ============================================================
# Comparison Table
# ============================================================
echo -e "${CYAN}── Comparison Table ──────────────────────────────────────${NC}"
echo ""
printf "  ${BOLD}%-20s %12s %12s %10s${NC}\n" "Benchmark" "dAImond" "C" "Ratio"
printf "  %-20s %12s %12s %10s\n" "────────────────────" "────────────" "────────────" "──────────"

for bench in "${MACRO_BENCHES[@]}"; do
    dm_ms="${MACRO_DM_TIMES[$bench]:-N/A}"
    c_ms="${MACRO_C_TIMES[$bench]:-N/A}"

    if [ "$dm_ms" != "N/A" ] && [ "$c_ms" != "N/A" ] && [ "$c_ms" -gt 0 ]; then
        ratio=$(echo "scale=2; $dm_ms / $c_ms" | bc)
        printf "  %-20s %10sms %10sms %9sx\n" "$bench" "$dm_ms" "$c_ms" "$ratio"
    else
        printf "  %-20s %12s %12s %10s\n" "$bench" "${dm_ms}ms" "${c_ms}ms" "N/A"
    fi
done

echo ""
echo -e "${BOLD}Done.${NC}"

# Cleanup
# rm -rf "$BUILD_DIR"  # Uncomment to auto-clean
