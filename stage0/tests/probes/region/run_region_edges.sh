#!/bin/bash
# Region escape edge cases for the Stage 0 checker rule.
#
# Supplements run_probes.sh p11 (basic escape) with: legal uses staying
# legal (reads of outer vars, plain assignments to outer vars, use of
# region-born values inside the region) and each escape path erroring
# (let-propagated taint, return, push/field stores into outer
# containers, nested-region escape, string +=, interpolation).
#
# Usage: bash stage0/tests/probes/region/run_region_edges.sh
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

ZIG="${ZIG:-$HOME/.local/share/mise/installs/zig/0.13.0/zig}"
if [ ! -x "$ZIG" ]; then ZIG="$(command -v zig || true)"; fi
DAIMOND="$ROOT/stage0/zig-out/bin/daimond"

# name|KIND|expected stdout (\n escapes, OK only)
CASES=(
    'region_legal|OK|hello\nouter\n42\np\n'
    'region_let_propagate|COMPILE_ERROR|'
    'region_return|COMPILE_ERROR|'
    'region_push_store|COMPILE_ERROR|'
    'region_field_store|COMPILE_ERROR|'
    'region_nested|COMPILE_ERROR|'
    'region_add_assign|COMPILE_ERROR|'
    'region_interp|COMPILE_ERROR|'
)

echo "==> Building Stage 0"
if ! (cd "$ROOT/stage0" && timeout 900 "$ZIG" build); then
    echo "error: stage0 build failed" >&2
    exit 2
fi

WORK="$(mktemp -d /tmp/daimond_region_edges.XXXXXX)"
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
    if [ "$kind" = "COMPILE_ERROR" ]; then
        if [ $code -eq 0 ]; then
            fail="expected compile error, compilation succeeded"
        elif [ $code -ge 128 ]; then
            fail="compiler died by signal (exit $code)"
        elif ! grep -qF "region escape" "$dir/compile.log"; then
            fail="compile error missing 'region escape': $(tail -n 3 "$dir/compile.log" | tr '\n' ' ')"
        fi
    else
        if [ $code -ne 0 ]; then
            fail="expected successful compile, got exit $code: $(tail -n 2 "$dir/compile.log" | tr '\n' ' ')"
        else
            timeout 10 "$dir/bin" >"$dir/run.out" 2>"$dir/run.err"
            printf '%b' "$want" >"$dir/want.out"
            cmp -s "$dir/want.out" "$dir/run.out" || fail="stdout mismatch: want [$(tr '\n' '~' <"$dir/want.out")] got [$(tr '\n' '~' <"$dir/run.out")]"
        fi
    fi
    if [ -z "$fail" ]; then
        echo "PASS $name"
        passed=$((passed + 1))
    else
        echo "FAIL $name: $fail"
        failed=$((failed + 1))
    fi
done

echo "Region edge summary: $passed passed, $failed failed"
[ "$failed" -eq 0 ]
