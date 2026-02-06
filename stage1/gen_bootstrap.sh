#!/bin/bash
# Concatenate split modules into monolithic main.dm for Stage 0 bootstrap
# Usage: cd stage1 && bash gen_bootstrap.sh
set -e
cd "$(dirname "$0")"
{
  echo "module main"
  echo ""
  echo "-- ============================================================"
  echo "-- dAImond Stage 1 Compiler"
  echo "-- Self-hosting compiler written in dAImond, compiled by Stage 0"
  echo "-- Compiles a subset of dAImond to C11"
  echo "-- ============================================================"
  echo ""
  for mod in token lexer compiler compile_expr compile_stmt compile_match compile_decl runtime imports package; do
    echo "-- [module: $mod]"
    # Strip module and import lines, output the rest
    sed '/^module /d; /^import /d' "$mod.dm"
    echo ""
  done
  # Append main() from main_split.dm (strip module/import lines)
  echo "-- [module: main]"
  sed '/^module /d; /^import /d' main_split.dm
} > main.dm
echo "Generated main.dm from split modules"
