#!/bin/sh
# progress.sh -- rough measure of how much of GNU Emacs 31.1 is ported.
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

# Units of work:
#   - C: function definitions in upstream src/*.[ch] (GNU style: name at
#     column 0 followed by " (", or a DEFUN), excluding files for other
#     platforms.  A function counts as done when a "file.c:name" citation
#     for it appears in our sources ("Port of buffer.c:Fset_buffer").
#   - Lisp: top-level definitions (defun, defmacro, cl-defun,
#     define-minor-mode, define-derived-mode) in upstream lisp/, counted
#     done when cited as "path.el:name" with the path under lisp/
#     ("simple.el:newline", "term/xterm.el:terminal-init-xterm").
# It's an estimate: functions differ wildly in size, and macros and
# variables are not counted.
#
# Usage: tools/progress.sh   (needs upstream/emacs; see fetch-upstream.sh)

set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
up="$root/upstream/emacs"
[ -d "$up/src" ] || { echo "run tools/fetch-upstream.sh first" >&2; exit 1; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Every upstream C function, as file:name.
for f in "$up"/src/*.c "$up"/src/*.h; do
    b=$(basename "$f")
    case "$b" in
        w32*|ns*|mac*|msdos*|dosfns*|android*|haiku*) continue ;;
    esac
    sed -n -e "s/^DEFUN (\"[^\"]*\", *\([A-Za-z_][A-Za-z0-9_]*\),.*/$b:\1/p" \
           -e "s/^\([A-Za-z_][A-Za-z0-9_]*\) (.*/$b:\1/p" "$f"
done | grep -v ':DEFUN$' | sort -u > "$tmp/all"

# Functions our sources cite.
grep -ohE '\b[a-z0-9_-]+\.[ch]:[A-Za-z_][A-Za-z0-9_]*' \
    "$root"/src/*.f90 "$root"/src/sys/*.f90 | sort -u > "$tmp/cited"
comm -12 "$tmp/all" "$tmp/cited" > "$tmp/done"

c_total=$(wc -l < "$tmp/all")
c_done=$(wc -l < "$tmp/done")
# Every upstream Lisp definition, as path-under-lisp/:name.
(cd "$up/lisp" && find . -name '*.el' | sed 's|^\./||' | sort | while read -r f; do
    sed -nE "s/^\((defun|cl-defun|defmacro|define-minor-mode|define-derived-mode) ([^ ()]+).*/$(printf '%s' "$f" | sed 's|[/&]|\\&|g'):\2/p" "$f"
done) | sort -u > "$tmp/lall"

# Lisp definitions our sources cite ("simple.el:newline",
# "term/xterm.el:terminal-init-xterm").
grep -ohE '\b[a-z0-9/_-]+\.el:[A-Za-z0-9*+/<>=!?-]*[A-Za-z0-9*+<>=!?]' \
    "$root"/src/*.f90 "$root"/src/sys/*.f90 | sort -u > "$tmp/lcited"
comm -12 "$tmp/lall" "$tmp/lcited" > "$tmp/ldone"

l_total=$(wc -l < "$tmp/lall")
l_done=$(wc -l < "$tmp/ldone")

total=$((c_total + l_total))
done=$((c_done + l_done))
pct=$(awk "BEGIN { printf \"%.2f\", 100 * $done / $total }")

echo "C functions:      $c_done of $c_total"
echo "Lisp definitions: $l_done of $l_total"
echo "Overall:          Done $done of $total (~$pct%)"
