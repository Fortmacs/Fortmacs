#!/bin/sh
# test-hazards.sh -- check tools/hazards.awk on the fixture in
# tools/hazards-test: the components of command_type and the strings in
# array constructors it must report and the ones it must not; and a
# statement of more than 255 continuation lines, made here.
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

# Usage: tools/test-hazards.sh  (from anywhere; run by tools/check.sh)

set -eu

cd "$(dirname "$0")/hazards-test"
out=${TMPDIR:-/tmp}/test-hazards.$$
trap 'rm -f "$out" "$out.f90"' EXIT

if awk -f ../hazards.awk sample.f90 >"$out"; then
    echo "test-hazards.sh: tools/hazards.awk passes the fixture" >&2
    exit 1
fi
# (awk, not cmp or diff: the distro containers lack diffutils)
if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { exit 1 } { m = FNR } END { exit m != n }' \
    expected "$out"; then
    echo "test-hazards.sh: tools/hazards.awk differs from tools/hazards-test/expected:" >&2
    cat "$out" >&2
    exit 1
fi

# 255 continuation lines are allowed, 256 are reported.
awk 'BEGIN {
    for (n = 254; n <= 255; n++) {
        printf "integer, parameter :: a%d(%d) = [ &\n", n, n + 1
        for (i = 1; i <= n; i++) printf "    %d, &\n", i
        print "    0]"
    }
}' >"$out.f90"
if [ "$(awk -f ../hazards.awk "$out.f90" | cut -d: -f2-3)" != "257: continuation-limit" ]; then
    echo "test-hazards.sh: tools/hazards.awk does not report exactly the statement of 256 continuation lines" >&2
    exit 1
fi
