#!/bin/bash
# Build the dAImond compiler from its seed, with LLVM's tools only.
#
# The seed (seed/daimond.bc) is the compiler as LLVM bitcode. LLVM's llc
# and ld.lld turn it into a compiler. That compiler compiles the
# compiler's source (stage5/main.dm), the result compiles it again, and
# so on until two generations in a row are the same file. No Zig and no
# C compiler takes part.
#
# Usage: bash bootstrap.sh [-o <compiler>] [--no-verify] [--update-seed]
#   -o <compiler>   where to put the compiler (default: build/daimond)
#   --no-verify     stop after the second generation without checking
#                   that the compiler reproduces itself
#   --update-seed   write seed/daimond.bc again from the new compiler
#
# Environment:
#   LLVM_BIN          the directory of llc, ld.lld and llvm-config
#                     (default: PATH, else the newest /usr/lib/llvm-*/bin)
#   DAIMOND_LIBC_DIR  the directory of the C library's Scrt1.o, crti.o,
#                     crtn.o and libc.so
#
# Needs x86-64 Linux with glibc, the C library's development files, and
# LLVM with lld. The seed was written by LLVM 19: an older LLVM cannot
# read it. LLVM 19 and LLVM 23 have been run.

set -e

ROOT="$(cd "$(dirname "$0")" && pwd)"
OUT="$ROOT/build/daimond"
VERIFY=1
UPDATE_SEED=0

while [ $# -gt 0 ]; do
    case "$1" in
        -o)
            [ $# -ge 2 ] || { echo "error: -o needs a path" >&2; exit 2; }
            OUT="$2"; shift ;;
        --no-verify) VERIFY=0 ;;
        --update-seed) UPDATE_SEED=1 ;;
        -h|--help) sed -n '2,25p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "error: unknown option: $1" >&2; exit 2 ;;
    esac
    shift
done

if [ "$UPDATE_SEED" = 1 ] && [ "$VERIFY" = 0 ]; then
    echo "error: --update-seed writes the seed from a compiler that reproduces itself, so it cannot go with --no-verify" >&2
    exit 2
fi

# An output path the user gave is relative to where they are, not to stage5/
case "$OUT" in
    /*) ;;
    *) OUT="$PWD/$OUT" ;;
esac

SEED="$ROOT/seed/daimond.bc"
[ -f "$SEED" ] || { echo "error: the seed $SEED is missing" >&2; exit 1; }

# ---- LLVM's tools ----

have_tools() {
    [ -x "$1/llc" ] && [ -x "$1/ld.lld" ] && [ -x "$1/llvm-config" ]
}

if [ -n "$LLVM_BIN" ]; then
    have_tools "$LLVM_BIN" || { echo "error: LLVM_BIN=$LLVM_BIN does not hold llc, ld.lld and llvm-config" >&2; exit 1; }
elif ! { command -v llc && command -v ld.lld && command -v llvm-config; } >/dev/null 2>&1; then
    for dir in $(ls -d /usr/lib/llvm-*/bin 2>/dev/null | sort -V -r); do
        if have_tools "$dir"; then LLVM_BIN="$dir"; break; fi
    done
    [ -n "$LLVM_BIN" ] || { echo "error: llc, ld.lld and llvm-config were not found on PATH or in /usr/lib/llvm-*/bin; install LLVM and lld, or set LLVM_BIN to their directory" >&2; exit 1; }
fi
# The compiler looks on PATH for ld.lld and llvm-config too
[ -z "$LLVM_BIN" ] || PATH="$LLVM_BIN:$PATH"

# ---- The C library's start-up objects ----

LIBC_DIR="$DAIMOND_LIBC_DIR"
if [ -z "$LIBC_DIR" ]; then
    for dir in /usr/lib/x86_64-linux-gnu /usr/lib64 /usr/lib; do
        if [ -f "$dir/Scrt1.o" ]; then LIBC_DIR="$dir"; break; fi
    done
fi
[ -n "$LIBC_DIR" ] && [ -f "$LIBC_DIR/Scrt1.o" ] || { echo "error: the C library's start-up object Scrt1.o was not found; install the C library's development files (libc6-dev or glibc-devel), or set DAIMOND_LIBC_DIR to their directory" >&2; exit 1; }

WORK="$ROOT/build/bootstrap"
rm -rf "$WORK"
mkdir -p "$WORK" "$(dirname "$OUT")"

SEED_ID="$(cat "$ROOT/seed/BUILD_ID" 2>/dev/null || true)"
SOURCE_ID="$(grep -A1 '^fn runtime_build_id' "$ROOT/stage5/prelude.dm" | grep -o '"[0-9a-f]*"' | tr -d '"')"

echo "==> LLVM $(llvm-config --version), from $(dirname "$(command -v llc)")"
if [ "$SEED_ID" = "$SOURCE_ID" ]; then
    echo "==> The seed was made from this source (build ID $SOURCE_ID)"
else
    echo "==> The seed was made from other source (its build ID is ${SEED_ID:-unknown}, this source's is $SOURCE_ID)"
fi

# ---- The seed, as a compiler ----

echo "==> Compiling the seed with llc (a minute or two)"
llc -O0 -filetype=obj -relocation-model=pic "$SEED" -o "$WORK/seed.o"
# shellcheck disable=SC2046
ld.lld -pie --eh-frame-hdr -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o "$WORK/seed" \
    "$LIBC_DIR/Scrt1.o" "$LIBC_DIR/crti.o" "$WORK/seed.o" -L"$LIBC_DIR" \
    $(llvm-config --libs --ldflags --system-libs) -lm -lpthread -lc "$LIBC_DIR/crtn.o"
rm -f "$WORK/seed.o"

# ---- Generations ----

# generation <compiler> <name>: compile the compiler's source with
# <compiler> into $WORK/<name>. Each compiler keeps its runtime object
# in a cache of its own, so that no generation links what another built.
generation() {
    echo "==> Building $2 with $(basename "$1")"
    (cd "$ROOT/stage5" && DAIMOND_CACHE_DIR="$WORK/cache-$(basename "$1")" "$1" main.dm --link-llvm -o "$WORK/$2")
    [ -x "$WORK/$2" ] || { echo "error: $(basename "$1") did not build $2" >&2; exit 1; }
}

generation "$WORK/seed" gen1
generation "$WORK/gen1" gen2
FINAL="$WORK/gen2"

if [ "$VERIFY" = 1 ]; then
    if cmp -s "$WORK/gen1" "$WORK/gen2"; then
        echo "==> Fixed point: generations 1 and 2 are the same file"
    else
        generation "$WORK/gen2" gen3
        if cmp -s "$WORK/gen2" "$WORK/gen3"; then
            echo "==> Fixed point: generations 2 and 3 are the same file"
        else
            echo "error: the compiler does not reproduce itself: generations 2 and 3 differ (both are in $WORK)" >&2
            exit 1
        fi
    fi
fi

cp "$FINAL" "$OUT"
echo "==> The compiler is $OUT"

# ---- A new seed ----

if [ "$UPDATE_SEED" = 1 ]; then
    (cd "$ROOT/stage5" && "$FINAL" main.dm --link-llvm --emit-bitcode -o "$WORK/daimond" >/dev/null)
    if cmp -s "$WORK/daimond.bc" "$SEED"; then
        echo "==> The seed is unchanged: this source gives the same bitcode"
    else
        cp "$WORK/daimond.bc" "$SEED"
        echo "==> Wrote $SEED ($(wc -c < "$SEED") bytes)"
    fi
    echo "$SOURCE_ID" > "$ROOT/seed/BUILD_ID"
fi

rm -rf "$WORK"
