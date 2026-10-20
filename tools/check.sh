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

# Usage: tools/check.sh [--fix]
#   --fix  reformat sources in place instead of failing on format drift.

set -eu

FFLAGS="-std=f2018 -Wall -Wextra -fcheck=all -g"
DIRS="src app test"

cd "$(dirname "$0")/.."

if [ "${1:-}" = "--fix" ]; then
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
