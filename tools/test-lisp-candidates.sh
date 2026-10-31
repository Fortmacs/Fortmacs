#!/bin/sh
# test-lisp-candidates.sh -- check tools/lisp-candidates.awk on the diff in
# tools/lisp-candidates-test: words reported once per line, citations and
# kept files skipped.
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

# Usage: tools/test-lisp-candidates.sh  (from anywhere; run by tools/check.sh)

set -eu

cd "$(dirname "$0")/.."
fixture=tools/lisp-candidates-test

got=$(awk -v words=tools/lisp-candidates -v keep=tools/fortran-names -f tools/lisp-candidates.awk \
    "$fixture/input.diff")

if [ "$got" != "$(cat "$fixture/expected")" ]; then
    echo "test-lisp-candidates: output differs from $fixture/expected:" >&2
    printf '%s\n' "$got" >&2
    exit 1
fi
