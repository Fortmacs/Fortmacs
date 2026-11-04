#!/bin/sh
# status.sh -- the status files of the modules (docs/status/MODULE.md):
# check them, print their index, and make them from rows of the status
# table PORTING.md had before 2026-11-04.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs, under the GNU GPL version 3 or later.

# Usage: tools/status.sh --check | --index | --from-row [-o DIR] [FILE...]
#                        | --move-rows OLD NEW
#   --check      check every docs/status/*.md against the format below
#                (tools/check.sh runs it); exit status 1 on a finding.
#   --index      print the index of the modules as a Markdown table
#                (module, upstream, state), one line per status file.
#   --from-row   turn each row "| Module | Upstream | Ported | Deferred |
#                Tests | State |" of the FILEs (or standard input) into
#                DIR/NAME.md (DIR defaults to docs/status), printing each
#                path.  A cell that holds a bare `|' must have it escaped
#                as `\|' (as GitHub's tables need anyway).
#   --move-rows  for a branch written before the status files: carry the
#                changes its commit NEW made to the status table, against
#                the commit OLD it started from (its merge base with main),
#                to docs/status/*.md, by a three-way merge of each changed
#                row (git merge-file; a conflict is left marked in the file
#                and the exit status is 1).  A row only NEW has becomes a
#                new file; a row NEW removed is reported, not deleted.
#
# The format (PORTING.md, "Status"): a file per table row, named for the
# first module of its Module cell:
#
#   # NAME
#
#   - Module: the Module cell (NAME, then any submodules and notes)
#   - Upstream: the upstream files
#   - State: done or partial
#
#   ## Ported
#
#   the list, one top-level item per line (a line break after a comma
#   or semicolon outside parentheses and code), joined with spaces
#
#   ## Deferred
#
#   the same; "—" for nothing
#
#   ## Tests
#
#   the same
#
# A field is read line-wise: a bullet's text, or the lines between a
# section's heading and the next heading joined with single spaces.

set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
dir=$root/docs/status

# The awk program that turns rows into files: one line per row on input,
# the files written under the variable dir.
from_row_awk='
# Split a row at the bars that are not escaped; the cells are trimmed.
function split_row(line, cell,    n, i, c, current) {
    n = 0
    current = ""
    sub(/^\|/, "", line)
    sub(/\|[ \t]*$/, "", line)
    for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)
        if (c == "\\" && substr(line, i + 1, 1) == "|") {
            current = current "\\|"
            i++
        } else if (c == "|") {
            cell[++n] = trim(current)
            current = ""
        } else {
            current = current c
        }
    }
    cell[++n] = trim(current)
    return n
}

function trim(s) {
    sub(/^[ \t]+/, "", s)
    sub(/[ \t]+$/, "", s)
    return s
}

# Whether a line starting with s would start a Markdown block (a list,
# a heading, a ">" line, a fence, a table, HTML) instead of continuing the
# paragraph above it.
function starts_block(s) {
    return s ~ /^([-+*>#<=|[]|[0-9]+[.)]|```|~~~)/
}

# The text with a line break after each comma or semicolon outside
# parentheses, brackets and code spans that is followed by one space.
function break_lines(s,    out, i, n, c, depth, code, rest) {
    out = ""
    depth = 0
    code = 0
    n = length(s)
    for (i = 1; i <= n; i++) {
        c = substr(s, i, 1)
        if (c == "`") {
            code = !code
        } else if (!code && (c == "(" || c == "[")) {
            depth++
        } else if (!code && (c == ")" || c == "]") && depth > 0) {
            depth--
        }
        if (!code && depth == 0 && (c == "," || c == ";") && substr(s, i + 1, 1) == " ") {
            rest = substr(s, i + 2)
            if (rest != "" && substr(rest, 1, 1) != " " && !starts_block(rest)) {
                out = out c "\n"
                i++
                continue
            }
        }
        out = out c
    }
    return out
}

/^\|/ {
    n = split_row($0, cell)
    if (cell[1] == "Module" || cell[1] ~ /^-+$/)
        next
    if (n != 6) {
        printf "status.sh: %d cells, not 6 (escape a bare | as \\|): %s\n", n, substr($0, 1, 60) > "/dev/stderr"
        bad = 1
        next
    }
    name = cell[1]
    sub(/[^a-z0-9_].*/, "", name)
    if (name == "") {
        printf "status.sh: no module name: %s\n", substr($0, 1, 60) > "/dev/stderr"
        bad = 1
        next
    }
    f = dir "/" name ".md"
    printf "# %s\n\n", name > f
    printf "- Module: %s\n", cell[1] > f
    printf "- Upstream: %s\n", cell[2] > f
    printf "- State: %s\n", cell[6] > f
    printf "\n## Ported\n\n" > f
    if (cell[3] != "") print break_lines(cell[3]) > f
    printf "\n## Deferred\n\n" > f
    if (cell[4] != "") print break_lines(cell[4]) > f
    printf "\n## Tests\n\n" > f
    if (cell[5] != "") print break_lines(cell[5]) > f
    close(f)
    print f
}

END { exit bad }
'

# The fields of status files, one line per file: NAME, then the fields in
# the order Module, Upstream, Ported, Deferred, Tests, State, separated by
# tabs (tabs inside fields become spaces).  With check=1, report every
# departure from the format instead, as FILE:LINE: message.
fields_awk='
function flush() {
    if (file == "")
        return
    if (check) {
        if (!("Module" in v) || !("Upstream" in v) || !("State" in v))
            report(1, "the bullets Module, Upstream and State are needed")
        if (nsections != 3)
            report(FNR, "the sections Ported, Deferred and Tests are needed, in that order")
    } else {
        printf "%s", stem
        for (k = 1; k <= 6; k++) {
            x = v[order[k]]
            gsub(/\t/, " ", x)
            printf "\t%s", x
        }
        printf "\n"
    }
    delete v
}

function report(line, msg) {
    printf "%s:%d: %s\n", file, line, msg
    bad = 1
}

BEGIN {
    split("Module Upstream Ported Deferred Tests State", order, " ")
    expect[1] = "Ported"; expect[2] = "Deferred"; expect[3] = "Tests"
}

FNR == 1 {
    flush()
    file = FILENAME
    stem = FILENAME
    sub(/.*\//, "", stem)
    sub(/\.md$/, "", stem)
    section = ""
    nsections = 0
    if (check && $0 != "# " stem)
        report(1, "the first line must be \"# " stem "\"")
    next
}

/^- (Module|Upstream|State): / && section == "" {
    key = $2
    sub(/:$/, "", key)
    text = $0
    sub(/^- [A-Za-z]+: /, "", text)
    if (check && key in v)
        report(FNR, key " given twice")
    v[key] = text
    if (check && key == "Module" && text !~ "^" stem "([^a-z0-9_]|$)")
        report(FNR, "the Module cell must start with " stem)
    if (check && key == "State" && text !~ /^(done|partial|complete)$/)
        report(FNR, "the state must be done, partial or complete")
    next
}

/^## / {
    section = substr($0, 4)
    nsections++
    if (check && section != expect[nsections])
        report(FNR, "expected the section \"" expect[nsections] "\"")
    v[section] = ""
    next
}

/^$/ { next }

{
    if (section == "") {
        if (check)
            report(FNR, "unexpected line before the sections")
        next
    }
    if (check && $0 ~ /^#/)
        report(FNR, "a heading other than the three sections")
    v[section] = v[section] == "" ? $0 : v[section] " " $0
}

END {
    flush()
    exit bad
}
'

case "${1:-}" in
--check)
    if ! ls "$dir"/*.md >/dev/null 2>&1; then
        echo "status.sh: no status files in docs/status" >&2
        exit 1
    fi
    cd "$root"
    if ! awk -v check=1 "$fields_awk" docs/status/*.md; then
        echo "status files out of format: see tools/status.sh" >&2
        exit 1
    fi
    ;;
--rows)
    cd "$root"
    awk -v check=0 "$fields_awk" docs/status/*.md |
        awk -F '\t' '{ printf "| %s | %s | %s | %s | %s | %s |\n", $2, $3, $4, $5, $6, $7 }'
    ;;
--index)
    cd "$root"
    echo "| Module | Upstream | State |"
    echo "|---|---|---|"
    awk -v check=0 "$fields_awk" docs/status/*.md |
        awk -F '\t' '{ printf "| [%s](docs/status/%s.md) | %s | %s |\n", $1, $1, $3, $7 }'
    ;;
--from-row)
    shift
    if [ "${1:-}" = -o ]; then
        dir=$2
        shift 2
    fi
    mkdir -p "$dir"
    awk -v dir="$dir" "$from_row_awk" "$@"
    ;;
--move-rows)
    if [ $# -ne 3 ]; then
        echo "usage: tools/status.sh --move-rows OLD NEW" >&2
        exit 2
    fi
    old=$2
    new=$3
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    cd "$root"
    # The rows of the table in each commit's PORTING.md.
    rows() {
        git show "$1:PORTING.md" | awk '/^## Status/ { s = 1; next } s && /^#/ { exit } s && /^\|/'
    }
    rows "$old" >"$tmp/old.rows"
    rows "$new" >"$tmp/new.rows"
    mkdir "$tmp/old" "$tmp/new"
    # A row the converter refuses (a bare `|' in a cell) is reported, and
    # checked for below.
    "$0" --from-row -o "$tmp/old" "$tmp/old.rows" >/dev/null 2>&1 || true
    "$0" --from-row -o "$tmp/new" "$tmp/new.rows" >/dev/null 2>&1 || true
    status=0
    # The modules whose rows differ, by their raw text.
    keys() {
        awk '{ k = $0; sub(/^\| */, "", k); sub(/[^a-z0-9_].*/, "", k); print k "\t" $0 }' "$1" | sort
    }
    keys "$tmp/old.rows" >"$tmp/old.keys"
    keys "$tmp/new.rows" >"$tmp/new.keys"
    for m in $(comm -13 "$tmp/old.keys" "$tmp/new.keys" | cut -f1 | sort -u); do
        if [ ! -f "$tmp/new/$m.md" ]; then
            echo "docs/status/$m.md: the branch changed this row, which tools/status.sh cannot read (escape a bare | as \\|); move it by hand" >&2
            status=1
        fi
    done
    for f in "$tmp"/new/*.md; do
        m=$(basename "$f")
        if [ -f "$tmp/old/$m" ] && cmp -s "$tmp/old/$m" "$f"; then
            continue
        fi
        if [ ! -f "$tmp/old/$m" ] && [ ! -f "docs/status/$m" ]; then
            cp "$f" "docs/status/$m"
            echo "docs/status/$m: new"
        elif [ ! -f "$tmp/old/$m" ] || [ ! -f "docs/status/$m" ]; then
            cp "$f" "docs/status/$m.branch"
            echo "docs/status/$m: new on both sides, or gone from main; merge docs/status/$m.branch by hand" >&2
            status=1
        elif git merge-file -L main -L base -L branch "docs/status/$m" "$tmp/old/$m" "$f"; then
            echo "docs/status/$m: merged"
        else
            echo "docs/status/$m: conflict; resolve the marked lines" >&2
            status=1
        fi
    done
    for f in "$tmp"/old/*.md; do
        m=$(basename "$f")
        if [ ! -f "$tmp/new/$m" ] && grep -q "^${m%.md}	" "$tmp/new.keys"; then
            continue
        fi
        if [ ! -f "$tmp/new/$m" ]; then
            echo "docs/status/$m: the branch removed this row; remove the file if that still holds" >&2
        fi
    done
    exit $status
    ;;
*)
    echo "usage: tools/status.sh --check | --index | --from-row [-o DIR] [FILE...] | --move-rows OLD NEW" >&2
    exit 2
    ;;
esac
