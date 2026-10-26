#!/bin/bash
# ============================================================================
# dAImond memory-safety probe regression suite (Stage 5 compiler)
# ============================================================================
#
# Same 19 probes and Phase 2 expectation table as run_probes.sh, but every
# probe is compiled with the Stage 5 compiler binary (LLVM backend) instead
# of Stage 0. This is the Phase 4 M2a runner; run_probes.sh is unchanged.
#
#   name|KIND|data|args
#
#   COMPILE_ERROR <substrings> : the stage5 compiler must exit nonzero
#                                (not a timeout, not a signal) and its
#                                combined output must contain every
#                                substring in `data` (";;"-separated).
#   PANIC <substrings>         : compilation must succeed; the binary must
#                                exit nonzero (not a signal) with "PANIC"
#                                and every substring in `data` in its
#                                combined stdout+stderr.
#   OK <exact stdout>          : compilation must succeed; the binary must
#                                exit 0 and stdout must equal `data`
#                                exactly ("\n" escapes allowed).
#
# Any signal exit (compiler or binary), and any timeout, is a FAIL.
# p11b is compiled by stage5 like the rest (its Stage 0 row uses the
# Stage 1 compiler; here it is a plain COMPILE_ERROR row).
#
# ASan: stage5 has NO sanitizer route. The Stage 0 runner rebuilds the
# generated C with -fsanitize=address,undefined; stage5 emits an LLVM
# object file and links it with a fixed `cc ... -lm -lpthread` command in
# stage5/main_split.dm (no -fsanitize flag, no environment override, no
# retained C), so an ASan rebuild is not possible without modifying the
# compiler. Substitution used here: signal-exit detection (any crash is a
# FAIL) plus exact stdout comparison for OK probes.
#
# The stage5 binary is built through the normal chain (Stage 0 build,
# `daimond ../stage5/main.dm -c` with NO --skip-type-check, then the
# stage4/run_parity.sh gcc recipe with stage5/ paths) and cached in /tmp;
# it is rebuilt only when the md5 of stage5/main.dm (plus the C inputs to
# the link) changes. Set STAGE5_PROBE_FORCE_BUILD=1 to force a rebuild.
# Probes are compiled from stage5/ as CWD, mirroring parity invocation:
# `<stage5_bin> <src> -o <out>`.
#
# Every invocation runs under `timeout` (900s stage0 build, 300s stage5
# build, 120s probe compile, 10s probe run). The stage5 build passes
# --no-cache (the stage5 compiler itself has no cache flag).
#
# Usage:
#   bash stage0/tests/probes/run_probes_stage5.sh [probe-id ...]
# With no arguments all probes run. With arguments only probes whose name
# starts with one of the arguments run (e.g. `run_probes_stage5.sh p01 p17`).
# Exit status is nonzero if any probe FAILs.
# Env: ZIG=/path/to/zig  STAGE5_PROBE_BIN=/path/to/bin
# ============================================================================

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

ZIG="${ZIG:-$HOME/.local/share/mise/installs/zig/0.13.0/zig}"
if [ ! -x "$ZIG" ]; then
    ZIG="$(command -v zig || true)"
fi
if [ -z "$ZIG" ] || [ ! -x "$ZIG" ]; then
    echo "error: zig not found (set ZIG=/path/to/zig)" >&2
    exit 2
fi

DAIMOND="$ROOT/stage0/zig-out/bin/daimond"
STAGE5_BIN="${STAGE5_PROBE_BIN:-/tmp/stage5_probe_bin}"
STAGE5_HASH_FILE="/tmp/stage5_probe_bin.md5"
STAGE5_C="/tmp/stage5_probe.c"

# Expectations: name|KIND|data|args   (args: single token, "" for none)
# Phase 2 outcomes, identical to run_probes.sh except p11b (see header).
PROBES=(
    'p01_box_null_deref|PANIC|null|'
    'p02_forged_ptr_ref|COMPILE_ERROR|cast|'
    'p03_forged_ptr_box|COMPILE_ERROR|cast|'
    'p04_cast_confusion|COMPILE_ERROR|int;;string|'
    'p05_array_oob_write|PANIC|index out of bounds;;(array)|100'
    'p06_array_oob_read|PANIC|index out of bounds;;(array)|100'
    'p07_string_oob_read|PANIC|index out of bounds;;(string)|'
    'p08_list_alias_uaf|OK|123\n|'
    'p09_string_alias|OK|200\n400\nabcdefghij\n|'
    'p10_map_alias|OK|777\n|'
    'p11_region_escape|COMPILE_ERROR|escape|'
    'p11b_region_escape_s1|COMPILE_ERROR|region;;escape|'
    'p12_memsweep_uaf|OK|hello world\n|'
    'p13_overflow_runtime|PANIC|overflow|'
    'p14_overflow_comptime|COMPILE_ERROR|overflow|'
    'p15_extern_unfenced|COMPILE_ERROR|unsafe|'
    'p16_extern_fenced|OK|x\n|'
    'p17_narrowing_cast|PANIC|cast|3000000000'
    'p17b_narrow_ok|OK|42\n42\n3\n|'
)

FILTERS=("$@")

want_probe() {
    if [ "${#FILTERS[@]}" -eq 0 ]; then return 0; fi
    local f
    for f in "${FILTERS[@]}"; do
        case "$1" in "$f"*) return 0 ;; esac
    done
    return 1
}

# check_substrings FILE DATA  -- DATA is ";;"-separated, all must appear
check_substrings() {
    local file="$1" data="$2" part
    local oldifs="$IFS"
    IFS=';'
    # shellcheck disable=SC2206
    local parts=($data)
    IFS="$oldifs"
    for part in "${parts[@]}"; do
        [ -z "$part" ] && continue
        if ! grep -qF -- "$part" "$file"; then
            echo "missing substring: $part"
            return 1
        fi
    done
    return 0
}

echo "==> Building Stage 0 ($ZIG build)"
if ! (cd "$ROOT/stage0" && timeout 900 "$ZIG" build); then
    echo "error: stage0 build failed" >&2
    exit 2
fi

# ---- build the stage5 compiler binary (md5-cached in /tmp) ----
new_hash="$(cat "$ROOT/stage5/main.dm" "$ROOT/stage5/llvm_bridge.c" \
    "$ROOT/stage5/runtime/llvm_wrappers.c" \
    "$ROOT/stage0/runtime/daimond_runtime.c" | md5sum | cut -d' ' -f1)"
old_hash=""
[ -f "$STAGE5_HASH_FILE" ] && old_hash="$(cat "$STAGE5_HASH_FILE")"
if [ "${STAGE5_PROBE_FORCE_BUILD:-0}" = "1" ] || [ ! -x "$STAGE5_BIN" ] || [ "$new_hash" != "$old_hash" ]; then
    echo "==> Building Stage 5 compiler -> $STAGE5_BIN (no --skip-type-check)"
    if ! (cd "$ROOT/stage0" && timeout 300 ./zig-out/bin/daimond ../stage5/main.dm -c -o "$STAGE5_C" --no-cache --no-color); then
        echo "error: stage0 failed to compile stage5/main.dm" >&2
        exit 2
    fi
    if ! (cd "$ROOT/stage0" && timeout 300 gcc -c "$STAGE5_C" -o /tmp/stage5_probe.o -I runtime -Wno-incompatible-pointer-types -Wno-int-conversion); then
        echo "error: gcc failed on stage5 generated C" >&2
        exit 2
    fi
    if ! (cd "$ROOT/stage0" && timeout 300 gcc -c ../stage5/llvm_bridge.c -o /tmp/llvm_bridge_probe.o $(llvm-config --cflags)); then
        echo "error: gcc failed on stage5/llvm_bridge.c" >&2
        exit 2
    fi
    if ! (cd "$ROOT/stage0" && timeout 300 gcc -c ../stage5/runtime/llvm_wrappers.c -o /tmp/llvm_wrappers_probe.o -I runtime); then
        echo "error: gcc failed on stage5/runtime/llvm_wrappers.c" >&2
        exit 2
    fi
    if ! (cd "$ROOT/stage0" && timeout 300 gcc -c runtime/daimond_runtime.c -o /tmp/daimond_runtime_probe.o); then
        echo "error: gcc failed on stage0 runtime" >&2
        exit 2
    fi
    if ! timeout 300 gcc /tmp/stage5_probe.o /tmp/llvm_bridge_probe.o /tmp/llvm_wrappers_probe.o /tmp/daimond_runtime_probe.o -o "$STAGE5_BIN" -lm -lpthread $(llvm-config --libs --ldflags); then
        echo "error: linking the stage5 compiler failed" >&2
        exit 2
    fi
    echo "$new_hash" > "$STAGE5_HASH_FILE"
else
    echo "==> Stage 5 compiler up to date: $STAGE5_BIN"
fi

WORK="$(mktemp -d /tmp/daimond_probes_stage5.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
echo "==> Probe work dir: $WORK (compiler: $STAGE5_BIN, CWD: stage5/)"

passed=0
failed=0
total=0

for row in "${PROBES[@]}"; do
    IFS='|' read -r name kind data args <<<"$row"
    if ! want_probe "$name"; then continue; fi
    total=$((total + 1))

    src="$SCRIPT_DIR/$name.dm"
    dir="$WORK/$name"
    mkdir -p "$dir"

    # ---- compile with the stage5 compiler, from stage5/ as CWD ----
    (cd "$ROOT/stage5" && timeout 120 "$STAGE5_BIN" "$src" -o "$dir/bin" >"$dir/compile.log" 2>&1)
    compile_code=$?

    fail_reason=""

    if [ "$kind" = "COMPILE_ERROR" ]; then
        if [ "$compile_code" -eq 0 ]; then
            fail_reason="expected compile error, compilation succeeded"
        elif [ "$compile_code" -eq 124 ]; then
            fail_reason="expected compile error, compiler timed out"
        elif [ "$compile_code" -ge 128 ]; then
            fail_reason="expected clean compile error, compiler died by signal (exit $compile_code)"
        else
            msg="$(check_substrings "$dir/compile.log" "$data")" || fail_reason="compile error $msg"
        fi
    else
        if [ "$compile_code" -ne 0 ]; then
            fail_reason="expected successful compile, got exit $compile_code: $(grep -vE 'warning:|note:|^\s|In file included' "$dir/compile.log" | tail -n 2 | tr '\n' ' ' | head -c 200)"
        elif [ ! -x "$dir/bin" ]; then
            fail_reason="compiler exited 0 but no binary produced"
        else
            (cd "$dir" && timeout 10 ./bin $args >run.out 2>run.err)
            run_code=$?
            cat "$dir/run.out" "$dir/run.err" > "$dir/run.all"
            if [ "$run_code" -eq 124 ]; then
                fail_reason="run timed out"
            elif [ "$kind" = "PANIC" ]; then
                if [ "$run_code" -eq 0 ]; then
                    fail_reason="expected panic, exit 0 (stdout: $(head -c 80 "$dir/run.out" | tr '\n' '~'))"
                elif [ "$run_code" -ge 128 ]; then
                    fail_reason="expected panic, died by signal (exit $run_code)"
                elif ! grep -qF "PANIC" "$dir/run.all"; then
                    fail_reason="expected PANIC in output (exit $run_code)"
                else
                    msg="$(check_substrings "$dir/run.all" "$data")" || fail_reason="$msg"
                fi
            elif [ "$kind" = "OK" ]; then
                if [ "$run_code" -ne 0 ]; then
                    fail_reason="expected exit 0, got $run_code"
                else
                    printf '%b' "$data" >"$dir/run.expected"
                    if ! cmp -s "$dir/run.expected" "$dir/run.out"; then
                        fail_reason="stdout mismatch (want/got): $(tr '\n' '~' <"$dir/run.expected" | head -c 80) / $(tr '\n' '~' <"$dir/run.out" | head -c 80)"
                    fi
                fi
            fi
        fi
    fi

    if [ -z "$fail_reason" ]; then
        echo "PASS $name"
        passed=$((passed + 1))
    else
        echo "FAIL $name: $fail_reason"
        failed=$((failed + 1))
    fi
done

echo "Stage5 probe summary: $passed passed, $failed failed (of $total)"

if [ "$failed" -gt 0 ]; then
    exit 1
fi
exit 0
