#!/bin/sh
# lisp-candidates.sh -- list Lisp vocabulary in the lines this branch adds
# (PORTING.md, "Fortran-first decision guide").
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

# Usage: tools/lisp-candidates.sh [BASE]   (run by tools/check.sh)
#
# Compares the working tree with BASE (default: $LISP_CANDIDATES_BASE,
# which CI sets to the merge commit's first parent, else where the branch
# left origin/main) and prints each Lisp candidate word in an added line.  It
# only warns and always exits 0.  Without a base (a shallow checkout with
# no origin/main) it says so and does nothing.

set -u

cd "$(dirname "$0")/.."

base=${1:-${LISP_CANDIDATES_BASE:-}}

if [ -z "$base" ]; then
    base=$(git merge-base HEAD origin/main 2>/dev/null) || base=
fi

if [ -z "$base" ]; then
    echo "lisp-candidates: no origin/main to compare with; skipped"
    exit 0
fi

git diff -U0 "$base" -- '*.f90' '*.md' '*.sh' '*.awk' \
    ':(exclude)docs/log' ':(exclude)tools/lisp-candidates' ':(exclude)tools/lisp-candidates.*' \
    ':(exclude)tools/lisp-candidates-test' ':(exclude)src/docstrings_mod.f90' |
    awk -v words=tools/lisp-candidates -v keep=tools/fortran-names -f tools/lisp-candidates.awk

exit 0
