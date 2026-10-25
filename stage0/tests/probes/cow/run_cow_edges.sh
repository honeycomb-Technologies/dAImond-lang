#!/bin/bash
# Copy-on-write aliasing edge cases for List/Map (Stage 0 codegen).
#
# Supplements run_probes.sh p08/p10 (alias UAF under ASan) with the
# observable snapshot semantics at each tracked copy site: let alias +
# index write, by-value parameter, nested List[List[int]], Map
# insert/remove, return value, struct field store.
#
# Usage: bash stage0/tests/probes/cow/run_cow_edges.sh
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

ZIG="${ZIG:-$HOME/.local/share/mise/installs/zig/0.13.0/zig}"
if [ ! -x "$ZIG" ]; then ZIG="$(command -v zig || true)"; fi
DAIMOND="$ROOT/stage0/zig-out/bin/daimond"

# name|expected stdout (\n escapes)
CASES=(
    'cow_list_idxwrite|1\n99\n2\n'
    'cow_param|3\n2\n10\n'
    'cow_nested|1\n42\n10001\n'
    'cow_map|1\n1\n0\n'
    'cow_return|1\n7\n2\n70\n'
    'cow_struct_field|1\n5\n2\n2\n'
)

echo "==> Building Stage 0"
if ! (cd "$ROOT/stage0" && timeout 900 "$ZIG" build); then
    echo "error: stage0 build failed" >&2
    exit 2
fi

WORK="$(mktemp -d /tmp/daimond_cow_edges.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

passed=0
failed=0
for row in "${CASES[@]}"; do
    IFS='|' read -r name want <<<"$row"
    dir="$WORK/$name"
    mkdir -p "$dir"
    cp "$SCRIPT_DIR/$name.dm" "$dir/$name.dm"
    (cd "$dir" && timeout 120 "$DAIMOND" "$name.dm" -o bin --no-cache --no-color >compile.log 2>&1)
    code=$?
    if [ $code -ne 0 ]; then
        echo "FAIL $name: compile exit $code: $(tail -n 2 "$dir/compile.log" | tr '\n' ' ')"
        failed=$((failed + 1))
        continue
    fi
    timeout 10 "$dir/bin" >"$dir/run.out" 2>"$dir/run.err"
    code=$?
    printf '%b' "$want" >"$dir/want.out"
    if [ $code -ne 0 ] || ! cmp -s "$dir/want.out" "$dir/run.out"; then
        echo "FAIL $name: run exit $code, want [$(tr '\n' '~' <"$dir/want.out")] got [$(tr '\n' '~' <"$dir/run.out")] err [$(head -c 120 "$dir/run.err" | tr '\n' '~')]"
        failed=$((failed + 1))
        continue
    fi
    echo "PASS $name"
    passed=$((passed + 1))
done

echo "COW edge summary: $passed passed, $failed failed"
[ "$failed" -eq 0 ]
