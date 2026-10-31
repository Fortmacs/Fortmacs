#!/bin/sh
# fortran-names.sh -- give Lisp-derived terms their Fortran names
# (tools/fortran-names) in files and file names.
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

# Usage: tools/fortran-names.sh [--table FILE] [PATH...]
#
# Applies the table to each PATH (a file, or every file under a
# directory), relative to the current directory; with no PATH, to the
# repository's src, test, app and tools, docs/init.f90, fpm.toml,
# PORTING.md and README.md.  A file of an old name is renamed (with
# `git mv' when git tracks it), then its contents are.  Citations and
# other upstream names are kept (see the table's header).  Running it
# again changes nothing.  Afterwards run the generators (tools/gen-*.sh)
# and tools/check.sh --fix: generated files and line lengths follow.
#
# Use it on this tree when the table gains an entry, on a branch written
# before an entry (after merging main), and on code freshly ported from a
# new upstream version, before review.
#
# Not touched by default: the table and its tools and test, the session
# logs (docs/log, dated records), docs/syntax.html (it shows Elisp), and
# images.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
table=$here/fortran-names
engine=$here/fortran-names.awk
lib=$here/fortran-names-lib.awk
if [ "${1:-}" = --table ]; then
    table=$2
    shift 2
fi

if [ $# -eq 0 ]; then
    cd "$here/.."
    set -- src test app tools docs/init.f90 fpm.toml PORTING.md README.md
fi

# The files under the PATHs, one per line, without the table's own.
list_files() {
    for p in "$@"; do
        if [ -d "$p" ]; then
            find "$p" -type f ! -path '*/.git/*' | sort
        elif [ -f "$p" ]; then
            printf '%s\n' "$p"
        else
            echo "fortran-names.sh: no such file: $p" >&2
            exit 1
        fi
    done | grep -v -e '/fortran-names$' -e '/fortran-names\.awk$' -e '/fortran-names-lib\.awk$' -e '/fortran-names\.sh$' \
        -e '/test-fortran-names\.sh$' -e '/fortran-names-test/' -e '\.\(svg\|png\|jpg\|ico\)$' || true
}

tmp=${TMPDIR:-/tmp}/fortran-names.$$
trap 'rm -f "$tmp" "$tmp.paths"' EXIT
list_files "$@" >"$tmp.paths"
nrenamed=0
nchanged=0

while IFS= read -r f; do
    new=$(printf '%s\n' "$f" | awk -v mode=path -v table="$table" -f "$lib" -f "$engine" -)
    if [ "$new" != "$f" ]; then
        if [ -e "$new" ]; then
            echo "fortran-names.sh: $f: $new exists; not renamed" >&2
            exit 1
        fi
        if git ls-files --error-unmatch "$f" >/dev/null 2>&1; then
            git mv "$f" "$new"
        else
            mv "$f" "$new"
        fi
        echo "renamed $f -> $new"
        nrenamed=$((nrenamed + 1))
        f=$new
    fi
    # Text files only.
    if grep -qI '' "$f" 2>/dev/null || [ ! -s "$f" ]; then
        awk -v mode=apply -v table="$table" -f "$lib" -f "$engine" "$f" >"$tmp"
        if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { exit 1 } { m = FNR } END { exit m != n }' \
            "$f" "$tmp"; then
            cat "$tmp" >"$f"
            nchanged=$((nchanged + 1))
        fi
    fi
done <"$tmp.paths"

echo "fortran-names.sh: $nrenamed file(s) renamed, $nchanged changed"
