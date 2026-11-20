#!/bin/sh
# Run one language server session, for a case of this suite.
# usage: session.sh <compiler> <the case's main.dm> <an empty directory>
#
# The case's directory is copied to <directory>/project. Its file
# `messages` is the script (see lsp/test_client.dm), played to the
# server with the copy as $DIR. What the server sends is printed, one
# message a line.
set -e
compiler=$1
case_dir=$(dirname "$2")
work=$3
lsp=$case_dir/../../../../lsp

"$compiler" "$lsp/main.dm" -o "$work/daimond-lsp" > /dev/null
"$compiler" "$lsp/test_client.dm" -o "$work/client" > /dev/null
cp -r "$case_dir" "$work/project"
"$work/client" send "$work/project/messages" "$work/project" \
    | "$work/daimond-lsp" --compiler "$compiler" --debounce 0 \
    | "$work/client" receive "$work/project"
