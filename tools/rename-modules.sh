#!/bin/sh
# rename-modules.sh -- give modules their spelled-out names
# (tools/module-renames) in files and file names.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs.
#
# Fortmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fortmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

# Usage: tools/rename-modules.sh [MAP]
#
# Applies MAP (default tools/module-renames: lines "OLD NEW", module
# stems without the _mod suffix, # starts a comment) to the files git
# tracks in this repository, except the session logs (docs/log, dated
# records) and docs/progress (generated on main):
#
#   - a file named OLD_mod.EXT or test_OLD_mod.EXT (src/OLD_mod.f90,
#     test/test_OLD_mod.f90, docs/status/OLD_mod.md, a test fixture) is
#     moved with `git mv' to NEW_mod.EXT or test_NEW_mod.EXT in the same
#     directory;
#   - the identifiers OLD_mod, test_OLD_mod and collect_OLD_mod (the
#     module, its test module and test suite, and the suite's collect
#     procedure) become NEW_mod, test_NEW_mod and collect_NEW_mod
#     wherever they occur as a whole identifier (a run of letters, digits
#     and underscores), in code, comments, strings and paths alike.
#
# Upstream citations (xdisp.c:redisplay, lisp/vc/ediff-util.el) never
# carry the _mod suffix, so they stay as they are; so do other names
# that only contain OLD (init_OLD, OLD_hook).  Running it again changes
# nothing.  An open branch runs it after merging main, so its own new
# uses and files follow; then tools/check.sh --fix (lines may need
# rewrapping past 132 columns, reported as FILE:LINE) and commit.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
map=${1:-$here/module-renames}
case $map in
    /*) ;;
    *) map=$(pwd)/$map ;;
esac
cd "$here/.."

if [ ! -f "$map" ]; then
    echo "rename-modules.sh: no such map: $map" >&2
    exit 1
fi

# The renames as "OLD NEW" identifier pairs, refusing a chain (a NEW that
# is also an OLD), which would make a second run change more.
pairs=${TMPDIR:-/tmp}/rename-modules.$$
trap 'rm -f "$pairs" "$pairs.new"' EXIT INT TERM
awk '
    { sub(/#.*/, "") }
    NF == 0 { next }
    NF != 2 { print "rename-modules.sh: bad map line " FNR ": " $0 > "/dev/stderr"; bad = 1; next }
    {
        old[$1] = $2
        n++
        olds[n] = $1
    }
    END {
        if (bad) exit 1
        for (i = 1; i <= n; i++) {
            o = olds[i]
            if (old[o] in old) {
                print "rename-modules.sh: " old[o] " is both a new and an old name" > "/dev/stderr"
                exit 1
            }
            print o "_mod", old[o] "_mod"
            print "test_" o "_mod", "test_" old[o] "_mod"
            print "collect_" o "_mod", "collect_" old[o] "_mod"
        }
    }
' "$map" >"$pairs"

# The files to look at: tracked, not dated records nor generated pages.
files() {
    git ls-files | grep -v -e '^docs/log/' -e '^docs/progress/'
}

# 1. File names.
moved=0
for f in $(files | awk -v pairs="$pairs" '
    BEGIN { while ((getline line < pairs) > 0) { split(line, p, " "); to[p[1]] = p[2] } }
    {
        base = $0
        sub(/.*\//, "", base)
        stem = base
        ext = ""
        if (match(stem, /\./)) {
            ext = substr(stem, RSTART)
            stem = substr(stem, 1, RSTART - 1)
        }
        if (stem in to) {
            dir = $0
            sub(/[^\/]*$/, "", dir)
            print $0 ":" dir to[stem] ext
        }
    }'); do
    from=${f%%:*}
    to=${f#*:}
    if [ -e "$to" ]; then
        echo "rename-modules.sh: $to already exists; $from not moved" >&2
        exit 1
    fi
    git mv "$from" "$to"
    moved=$((moved + 1))
done

# 2. Identifiers, in the text files that hold one.
edited=0
pattern=$(awk '{ printf "%s%s", (NR > 1 ? "|" : ""), $1 }' "$pairs")
for f in $(files | xargs git grep -lIwE "($pattern)" -- 2>/dev/null || true); do
    awk -v pairs="$pairs" '
        BEGIN { while ((getline line < pairs) > 0) { split(line, p, " "); to[p[1]] = p[2] } }
        {
            out = ""
            rest = $0
            while (match(rest, /[A-Za-z0-9_]+/)) {
                word = substr(rest, RSTART, RLENGTH)
                out = out substr(rest, 1, RSTART - 1) (word in to ? to[word] : word)
                rest = substr(rest, RSTART + RLENGTH)
            }
            print out rest
        }
    ' "$f" >"$pairs.new"
    if ! cmp -s "$f" "$pairs.new"; then
        # cat keeps the file's mode (executable scripts).
        cat "$pairs.new" >"$f"
        edited=$((edited + 1))
        awk -v f="$f" 'length($0) > 132 && f ~ /\.f90$/ { print f ":" FNR ": longer than 132 columns" }' "$f"
    fi
done

echo "rename-modules.sh: $moved files moved, $edited files edited"
