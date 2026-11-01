#!/bin/sh
# test-hazards.sh -- check tools/hazards.awk on the fixture in
# tools/hazards-test: the components of command_type it must report and
# the ones it must not.
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
trap 'rm -f "$out"' EXIT

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
