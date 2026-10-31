#!/bin/sh
# test-fortran-names.sh -- check that tools/fortran-names.sh renames the
# fixture in tools/fortran-names-test/input (old names, citations, a
# docstring, Lisp source, a file of an old name) into the files of
# tools/fortran-names-test/expected, that a second run changes nothing,
# and that tools/fortran-names.awk flags the input but not the result.
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

# Usage: tools/test-fortran-names.sh  (from anywhere; run by tools/check.sh)

set -eu

cd "$(dirname "$0")/.."
root=$(pwd)
fixture=tools/fortran-names-test
work=${TMPDIR:-/tmp}/test-fortran-names.$$
trap 'rm -rf "$work"' EXIT
rm -rf "$work"
mkdir -p "$work"
cp "$fixture"/input/* "$work"/

check() {
    awk -v mode=check -v table="$root/tools/fortran-names" -f "$root/tools/fortran-names-lib.awk" \
        -f "$root/tools/fortran-names.awk" "$@"
}

# Same files and contents, as `diff -r' would say (awk: the distro
# containers lack diffutils).
same() {
    if [ "$(cd "$1" && ls)" != "$(cd "$2" && ls)" ]; then
        echo "test-fortran-names.sh: files differ: $(cd "$1" && ls) vs $(cd "$2" && ls)" >&2
        return 1
    fi
    for f in $(cd "$1" && ls); do
        if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { print FILENAME ":" FNR ": " $0; exit 1 }
                  { m = FNR } END { if (m != n) { print FILENAME ": length differs"; exit 1 } }' \
            "$1/$f" "$2/$f" >&2; then
            return 1
        fi
    done
}

if check "$work"/lisp_cmds_fm.f90 >/dev/null; then
    echo "test-fortran-names.sh: the check finds nothing in the old names" >&2
    exit 1
fi
# The first run reports the line it pushes past 132 columns and the file
# it leaves alone for a clash (clash_fm.f90), with status 4.
status=0
report=$( (cd "$work" && "$root/tools/fortran-names.sh" ./*) 2>&1 >/dev/null) || status=$?
if [ "$status" -ne 4 ] || ! printf '%s\n' "$report" | grep -q 'expression_motion_fm.f90:28: longer than 132 columns' ||
    ! printf '%s\n' "$report" | grep -q 'clash_fm.f90:4: forward_sexp -> forward_expression clashes'; then
    echo "test-fortran-names.sh: tools/fortran-names.sh did not report the long line and the clash (status $status)" >&2
    printf '%s\n' "$report" >&2
    exit 1
fi
if ! same "$work" "$fixture/expected"; then
    echo "test-fortran-names.sh: tools/fortran-names.sh differs from $fixture/expected" >&2
    exit 1
fi
(cd "$work" && "$root/tools/fortran-names.sh" ./*) >/dev/null 2>&1 || [ $? -eq 4 ]
if ! same "$work" "$fixture/expected"; then
    echo "test-fortran-names.sh: a second run of tools/fortran-names.sh changed the result" >&2
    exit 1
fi
if ! check $(ls "$work"/*.f90 | grep -v '/clash_fm\.f90$'); then
    echo "test-fortran-names.sh: the check flags the renamed fixture" >&2
    exit 1
fi
