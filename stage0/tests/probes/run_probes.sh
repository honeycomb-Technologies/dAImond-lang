#!/bin/bash
# ============================================================================
# dAImond memory-safety probe regression suite (Stage 0)
# ============================================================================
#
# Each probe in this directory encodes one audit finding. Expectations are
# data in the table below, one row per probe:
#
#   name|KIND|data|args
#
#   COMPILE_ERROR <substrings> : compilation must fail cleanly (exit nonzero,
#                                not a signal, not a timeout) and the combined
#                                compiler output must contain every substring
#                                in `data` (separated by ";;").
#   PANIC <substrings>         : compilation must succeed; the binary must
#                                exit nonzero (not a signal) with "PANIC" and
#                                every substring in `data` on stderr.
#   OK <exact stdout>          : compilation must succeed; the binary must
#                                exit 0 and stdout must equal `data` exactly
#                                ("\\n" escapes allowed).
#
# For every probe that produces a binary, the generated C is ALSO rebuilt
# with `gcc -O0 -g -fsanitize=address,undefined` and re-run; sanitizer
# output (AddressSanitizer / UBSan "runtime error:") or a different outcome
# is a FAIL. ASAN_OPTIONS=detect_leaks=0 is set because leak-freedom is not
# part of these probes' contract (arena/pool allocations live until exit by
# design); heap/stack/global memory errors and UB are still caught.
#
# Every compiler/binary invocation runs under `timeout` (120s compile,
# 10s run, 30s sanitizer run, 900s stage0 build). Compiles pass --no-cache:
# the source-hash C cache otherwise skips type checking entirely on a hit.
#
# Usage:
#   bash stage0/tests/probes/run_probes.sh [probe-id ...]
# With no arguments all probes run. With arguments only probes whose name
# starts with one of the arguments run (e.g. `run_probes.sh p02 p17`).
# Exit status is nonzero if any probe FAILs.
#
# Unsafe fencing rules (Stage 0 checker; see checker.zig):
#   - `unsafe { ... }` is a statement block (lexer kw_unsafe, parser
#     statement, codegen-transparent braced block).
#   - Calling a function declared `extern fn` outside an unsafe block is a
#     type error: "extern function call '<name>' requires unsafe block".
#   - Dereferencing (*expr) a reference (&T) of untrusted provenance
#     outside an unsafe block is a type error. Provenance is tracked
#     simply, by variable name per function: a variable initialized from
#     an extern call result or a cast to a reference type is tainted
#     (taint propagates through plain copies/assignments), and every
#     reference-typed function parameter is tainted by default.
#   - Dereferencing Box[T] stays safe: codegen emits a runtime null check
#     (PANIC: null Box dereference) on Box field access and Box deref.
#   - The fence is checker-level: programs compiled with
#     --skip-type-check bypass it (documented remainder; Stage 4's own
#     bootstrap uses that flag for its dm_llvm_* extern calls).
#
# Per-probe intent:
#   p01_box_null_deref    field read through Box_null()      -> PANIC (null)
#   p02_forged_ptr_ref    1 as &int, deref                   -> COMPILE_ERROR
#   p03_forged_ptr_box    12345 as Box[Node], field read     -> COMPILE_ERROR
#   p04_cast_confusion    42 as string                       -> COMPILE_ERROR naming int+string
#   p05_array_oob_write   runtime idx 100 into [int; 3]      -> PANIC ((array))
#   p06_array_oob_read    runtime idx 100 from [int; 3]      -> PANIC ((array))
#   p07_string_oob_read   "hi"[1000]                         -> PANIC ((string))
#   p08_list_alias_uaf    aliased List, grow original        -> OK "123" (Session B)
#   p09_string_alias      aliased string, grow original      -> OK (Session B)
#   p10_map_alias         aliased Map, grow original         -> OK (Session B)
#   p11_region_escape     region string escapes to outer var -> COMPILE_ERROR (Session B)
#   p12_memsweep_uaf      concat after mem_mark, then sweep  -> OK (Session B)
#   p13_overflow_runtime  INT64_MAX + 1 at runtime           -> PANIC (Session B)
#   p14_overflow_comptime comptime INT64_MAX + 1             -> COMPILE_ERROR (Session B)
#   p15_extern_unfenced   extern call without unsafe         -> COMPILE_ERROR (unsafe)
#   p16_extern_fenced     extern call inside unsafe { }      -> OK prints x
#   p17_narrowing_cast    3000000000 as i32 (runtime value)  -> PANIC (cast)
#   p17b_narrow_ok        in-range narrow/widen/float casts  -> OK
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
RUNTIME_DIR="$ROOT/stage0/runtime"

# Expectations: name|KIND|data|args   (args: single token, "" for none)
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
    'p12_memsweep_uaf|OK|hello world\n|'
    'p13_overflow_runtime|PANIC|overflow|'
    'p14_overflow_comptime|COMPILE_ERROR|overflow|'
    'p15_extern_unfenced|COMPILE_ERROR|unsafe|'
    'p16_extern_fenced|OK|x\n|'
    'p17_narrowing_cast|PANIC|cast|3000000000'
    'p17b_narrow_ok|OK|42\n42\n3\n|'
)

want_probe() {
    # $1 = probe name; remaining script args are in FILTERS
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

# check_run KIND DATA CODE OUTFILE ERRFILE -- echoes reason on failure
check_run() {
    local kind="$1" data="$2" code="$3" out="$4" err="$5"
    if [ "$code" -eq 124 ]; then echo "run timed out"; return 1; fi
    case "$kind" in
        PANIC)
            if [ "$code" -eq 0 ]; then echo "expected panic, exit 0"; return 1; fi
            if [ "$code" -ge 128 ]; then echo "expected panic, died by signal (exit $code)"; return 1; fi
            if ! grep -qF "PANIC" "$err"; then echo "expected PANIC on stderr"; return 1; fi
            check_substrings "$err" "$data" || return 1
            ;;
        OK)
            if [ "$code" -ne 0 ]; then echo "expected exit 0, got $code"; return 1; fi
            printf '%b' "$data" >"$out.expected"
            if ! cmp -s "$out.expected" "$out"; then
                echo "stdout mismatch (want/got): $(cat "$out.expected" | head -c 80 | tr '\n' '~') / $(cat "$out" | head -c 80 | tr '\n' '~')"
                return 1
            fi
            ;;
        *)
            echo "internal: check_run for kind $kind"
            return 1
            ;;
    esac
    return 0
}

check_sanitizer_clean() {
    local err="$1"
    if grep -qE "AddressSanitizer|UndefinedBehaviorSanitizer|runtime error:" "$err"; then
        echo "sanitizer report: $(grep -m1 -E 'AddressSanitizer|runtime error:' "$err" | head -c 160)"
        return 1
    fi
    return 0
}

FILTERS=("$@")

echo "==> Building Stage 0 ($ZIG build)"
if ! (cd "$ROOT/stage0" && timeout 900 "$ZIG" build); then
    echo "error: stage0 build failed" >&2
    exit 2
fi

WORK="$(mktemp -d /tmp/daimond_probes.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
echo "==> Probe work dir: $WORK"

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
    cp "$src" "$dir/$name.dm"

    # ---- compile with stage0 daimond (C kept via --emit-c, lands in CWD) ----
    (cd "$dir" && timeout 120 "$DAIMOND" "$name.dm" -o bin --emit-c --no-cache --no-color >compile.log 2>&1)
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
            fail_reason="expected successful compile, got exit $compile_code: $(tail -n 2 "$dir/compile.log" | tr '\n' ' ' | head -c 200)"
        fi
    fi

    # ---- any produced binary gets run + ASan/UBSan rebuild + run ----
    if [ -x "$dir/bin" ]; then
        # shellcheck disable=SC2086
        (cd "$dir" && timeout 10 ./bin $args >run.out 2>run.err)
        run_code=$?
        if [ -z "$fail_reason" ]; then
            msg="$(check_run "$kind" "$data" "$run_code" "$dir/run.out" "$dir/run.err")" || fail_reason="$msg"
        fi

        if [ -f "$dir/$name.c" ]; then
            if timeout 120 gcc -O0 -g -fsanitize=address,undefined -std=gnu11 \
                -I "$RUNTIME_DIR" "$dir/$name.c" "$RUNTIME_DIR/daimond_runtime.c" \
                -o "$dir/bin_asan" -lm -lpthread >"$dir/asan_build.log" 2>&1; then
                # shellcheck disable=SC2086
                (cd "$dir" && ASAN_OPTIONS=detect_leaks=0 timeout 30 ./bin_asan $args >asan.out 2>asan.err)
                asan_code=$?
                msg="$(check_sanitizer_clean "$dir/asan.err")" || {
                    [ -n "$fail_reason" ] || fail_reason="asan: $msg"
                }
                if [ -z "$fail_reason" ]; then
                    msg="$(check_run "$kind" "$data" "$asan_code" "$dir/asan.out" "$dir/asan.err")" || fail_reason="asan run: $msg"
                fi
            else
                [ -n "$fail_reason" ] || fail_reason="asan rebuild failed: $(tail -n 2 "$dir/asan_build.log" | tr '\n' ' ' | head -c 200)"
            fi
        else
            [ -n "$fail_reason" ] || fail_reason="binary produced but generated C not found ($name.c)"
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

echo "Probe summary: $passed passed, $failed failed (of $total)"

if [ "$failed" -gt 0 ]; then
    exit 1
fi
exit 0
