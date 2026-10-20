#!/bin/sh
# check.sh -- the single gate run locally and in CI: format, build, test.
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

# Usage: tools/check.sh [--fix] [--valgrind]
#   --fix       reformat sources in place instead of failing on format drift.
#   --valgrind  also run the tests under valgrind and fail on any definite
#               leak or memory error (see tools/valgrind.supp).

set -eu

FFLAGS="-std=f2018 -Wall -Wextra -fcheck=all -g"
DIRS="src app test"
fix=false
memcheck=false
for arg in "$@"; do
    case "$arg" in
        --fix) fix=true ;;
        --valgrind) memcheck=true ;;
        *) echo "usage: tools/check.sh [--fix] [--valgrind]" >&2; exit 2 ;;
    esac
done

cd "$(dirname "$0")/.."

if $fix; then
    fprettify --config-file .fprettify.rc --recursive $DIRS
else
    drift=$(fprettify --config-file .fprettify.rc --recursive --diff $DIRS)
    if [ -n "$drift" ]; then
        printf '%s\n' "$drift"
        echo "format drift: run tools/check.sh --fix" >&2
        exit 1
    fi
fi

fpm build --profile debug --flag "$FFLAGS"
fpm test --profile debug --flag "$FFLAGS"

if $memcheck; then
    fpm test --profile debug --flag "$FFLAGS" --runner \
        "valgrind -q --leak-check=full --errors-for-leak-kinds=definite --error-exitcode=9 --suppressions=tools/valgrind.supp"
fi
