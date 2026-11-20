#!/bin/sh
# Build a case of this suite with -g and print what its debug info says.
# usage: dwarf.sh <compiler> <the case's main.dm> <an empty directory> [option ...]
#
# The program is built in the case's directory with -g and the options
# given, and run. Then its debug info (DWARF), read with LLVM's
# llvm-dwarfdump, is printed in short: each function with its file and
# line, its parameters and variables with their types, each type, and
# the lines of each file that have code.
set -e
compiler=$1
source=$2
work=$3
shift 3

dump=${LLVM_BIN:+$LLVM_BIN/llvm-dwarfdump}
if [ -z "$dump" ] || [ ! -x "$dump" ]; then
    dump=$(command -v llvm-dwarfdump || true)
fi
if [ -z "$dump" ] && command -v llvm-config > /dev/null; then
    dump=$(llvm-config --bindir)/llvm-dwarfdump
fi
if [ -z "$dump" ] || [ ! -x "$dump" ]; then
    dump=$(ls -d /usr/lib/llvm-*/bin/llvm-dwarfdump 2> /dev/null | sort -V | tail -1)
fi
if [ -z "$dump" ]; then
    echo "llvm-dwarfdump was not found; set LLVM_BIN to LLVM's bin directory" >&2
    exit 1
fi

cd "$(dirname "$source")"
"$compiler" "$(basename "$source")" -g "$@" -o "$work/program" > /dev/null
"$work/program"

"$dump" --debug-info "$work/program" | awk '
function quoted(s) { sub(/^[^"]*"/, "", s); sub(/"[^"]*$/, "", s); return s }
function number(s,    i, n, d) {
    sub(/^[^(]*\(/, "", s); sub(/\).*$/, "", s)
    if (s !~ /^0x/) return s + 0
    n = 0
    for (i = 3; i <= length(s); i++) {
        d = index("0123456789abcdef", tolower(substr(s, i, 1))) - 1
        n = n * 16 + d
    }
    return n
}
function base(s) { sub(/^.*\//, "", s); return s }
function flush() {
    if (tag == "subprogram") { locals(); print "fn " name " at " file ":" line }
    else if (tag == "formal_parameter") print "  parameter " name ": " type
    else if (tag == "variable") {
        variables[line] = variables[line] "  variable " name ": " type ", line " line "\n"
        if (line > last_line) last_line = line
    }
    else if (tag == "structure_type") { finish(); struct = name; size = bytes; fields = "" }
    else if (tag == "member") fields = fields (fields == "" ? "" : ", ") name ": " type " at " offset
    else if (tag == "base_type") types[++ntypes] = "type " name ", " bytes " bytes"
    tag = ""
}
# The variables of the function before, in the order of their lines
function locals(    l) {
    for (l = 1; l <= last_line; l++) if (l in variables) { printf "%s", variables[l]; delete variables[l] }
    last_line = 0
}
function finish() {
    if (struct != "") types[++ntypes] = "struct " struct " { " fields " }, " size " bytes"
    struct = ""
}
/^0x[0-9a-f]+: +DW_TAG_/ { flush(); tag = $2; sub(/^DW_TAG_/, "", tag); name = ""; type = ""; next }
/^0x[0-9a-f]+: +NULL/ { flush(); next }
/DW_AT_name/ { name = quoted($0) }
/DW_AT_decl_file/ { file = base(quoted($0)) }
/DW_AT_decl_line/ { line = number($0) }
/DW_AT_type/ { type = quoted($0) }
/DW_AT_byte_size/ { bytes = number($0) }
/DW_AT_data_member_location/ { offset = number($0) }
END {
    flush(); locals(); finish()
    for (i = 1; i <= ntypes; i++) print types[i] | "sort"
}'

"$dump" --debug-line "$work/program" | awk '
/^file_names\[/ { index_of = $0; gsub(/[^0-9]/, "", index_of) }
/^ +name: "/ { n = $0; sub(/^[^"]*"/, "", n); sub(/"$/, "", n); sub(/^\.\//, "", n); names[index_of + 0] = n }
/^0x[0-9a-f]+ / && /is_stmt/ && $2 > 0 { seen[$4 + 0, $2 + 0] = 1; if ($2 + 0 > last[$4 + 0]) last[$4 + 0] = $2 + 0 }
END {
    for (f = 0; f < 100; f++) {
        if (!(f in last)) continue
        text = "lines of " names[f] ":"
        for (l = 1; l <= last[f]; l++) if ((f, l) in seen) text = text " " l
        print text
    }
}'
