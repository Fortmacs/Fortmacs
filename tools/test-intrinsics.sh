#!/bin/sh
# test-intrinsics.sh -- check tools/intrinsics.awk on the fixture in
# tools/intrinsics-test: the thin wrappers and Lisp helper names it must
# report, the ones it must not (two statements, a module variable, nested
# intrinsics, an interface body), a finding its baseline excuses and a
# baseline entry that no longer occurs.
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

# Usage: tools/test-intrinsics.sh  (from anywhere; run by tools/check.sh)

set -eu

cd "$(dirname "$0")/intrinsics-test"
out=${TMPDIR:-/tmp}/test-intrinsics.$$
trap 'rm -f "$out"' EXIT

if awk -v baseline=baseline -f ../intrinsics.awk sample.f90 >"$out"; then
    echo "test-intrinsics.sh: tools/intrinsics.awk passes the fixture" >&2
    exit 1
fi
# (awk, not cmp or diff: the distro containers lack diffutils)
if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { exit 1 } { m = FNR } END { exit m != n }' \
    expected "$out"; then
    echo "test-intrinsics.sh: tools/intrinsics.awk differs from tools/intrinsics-test/expected:" >&2
    cat "$out" >&2
    exit 1
fi
