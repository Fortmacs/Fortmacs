#!/bin/sh
# test-fortran-prose.sh -- check that tools/fortran-prose.awk reports
# each rejected term of tools/fortran-prose-test/rejected.f90 (as listed
# in tools/fortran-prose-test/expected) and nothing in
# tools/fortran-prose-test/accepted.f90.
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

# Usage: tools/test-fortran-prose.sh  (from anywhere; run by tools/check.sh)

set -eu

cd "$(dirname "$0")/fortran-prose-test"

check() {
    awk -v table=../fortran-prose -f ../fortran-prose.awk "$@"
}

if found=$(check rejected.f90); then
    echo "test-fortran-prose.sh: the check finds nothing in rejected.f90" >&2
    exit 1
fi
got=$(printf '%s\n' "$found" | sed 's/ (use .*//')
if [ "$got" != "$(cat expected)" ]; then
    echo "test-fortran-prose.sh: findings differ from expected:" >&2
    printf '%s\n' "$got" >&2
    exit 1
fi
if ! check accepted.f90; then
    echo "test-fortran-prose.sh: the check rejects accepted.f90" >&2
    exit 1
fi
