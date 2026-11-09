#!/bin/bash
# Defined-overflow edge cases (Stage 0 codegen + comptime evaluator).
#
# Supplements run_probes.sh p13/p14 with: near-boundary OK arithmetic
# incl. i32 and documented unsigned wrapping; i32 overflow, compound
# += overflow, INT64_MIN / -1, negation overflow as runtime PANICs;
# comptime mul/sub and const-decl folding overflow as clean compile
# errors (exit nonzero, not a signal, message contains "overflow").
#
# Usage: bash stage0/tests/probes/overflow/run_overflow_edges.sh
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

ZIG="${ZIG:-$HOME/.local/share/mise/installs/zig/0.13.0/zig}"
if [ ! -x "$ZIG" ]; then ZIG="$(command -v zig || true)"; fi
DAIMOND="$ROOT/stage0/zig-out/bin/daimond"

# name|KIND|expected stdout (\n escapes, OK only)
CASES=(
    'overflow_ok|OK|9223372036854775807\n2100000000\n2000000007\n0\n'
    'overflow_i32_overflow|PANIC|'
    'overflow_compound|PANIC|'
    'overflow_divmin|PANIC|'
    'overflow_negmin|PANIC|'
    'overflow_comptime_mul|COMPILE_ERROR|'
    'overflow_const_decl|COMPILE_ERROR|'
    'overflow_comptime_sub|COMPILE_ERROR|'
)

echo "==> Building Stage 0"
if ! (cd "$ROOT/stage0" && timeout 900 "$ZIG" build); then
    echo "error: stage0 build failed" >&2
    exit 2
fi

WORK="$(mktemp -d /tmp/daimond_overflow_edges.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

passed=0
failed=0
for row in "${CASES[@]}"; do
    IFS='|' read -r name kind want <<<"$row"
    dir="$WORK/$name"
    mkdir -p "$dir"
    cp "$SCRIPT_DIR/$name.dm" "$dir/$name.dm"
    (cd "$dir" && timeout 120 "$DAIMOND" "$name.dm" -o bin --no-cache --no-color >compile.log 2>&1)
    code=$?
    fail=""
    case "$kind" in
        COMPILE_ERROR)
            if [ $code -eq 0 ]; then
                fail="expected compile error, compilation succeeded"
            elif [ $code -ge 128 ]; then
                fail="compiler died by signal (exit $code)"
            elif ! grep -qF "overflow" "$dir/compile.log"; then
                fail="compile error missing 'overflow': $(tail -n 3 "$dir/compile.log" | tr '\n' ' ')"
            fi
            ;;
        PANIC)
            if [ $code -ne 0 ]; then
                fail="expected successful compile, got exit $code: $(tail -n 2 "$dir/compile.log" | tr '\n' ' ')"
            else
                timeout 10 "$dir/bin" >"$dir/run.out" 2>"$dir/run.err"
                rc=$?
                if [ $rc -eq 0 ] || [ $rc -ge 128 ] || ! grep -qF "PANIC: integer overflow" "$dir/run.err"; then
                    fail="expected PANIC integer overflow, run exit $rc err [$(head -c 120 "$dir/run.err" | tr '\n' '~')]"
                fi
            fi
            ;;
        OK)
            if [ $code -ne 0 ]; then
                fail="expected successful compile, got exit $code: $(tail -n 2 "$dir/compile.log" | tr '\n' ' ')"
            else
                timeout 10 "$dir/bin" >"$dir/run.out" 2>"$dir/run.err"
                printf '%b' "$want" >"$dir/want.out"
                cmp -s "$dir/want.out" "$dir/run.out" || fail="stdout mismatch: want [$(tr '\n' '~' <"$dir/want.out")] got [$(tr '\n' '~' <"$dir/run.out")]"
            fi
            ;;
    esac
    if [ -z "$fail" ]; then
        echo "PASS $name"
        passed=$((passed + 1))
    else
        echo "FAIL $name: $fail"
        failed=$((failed + 1))
    fi
done

echo "Overflow edge summary: $passed passed, $failed failed"
[ "$failed" -eq 0 ]
