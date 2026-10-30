#!/bin/sh
# check.sh -- the single gate run locally and in CI: format, lint, build,
# test, and --recompile against an installed copy.
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

# Usage: tools/check.sh [--fix] [--valgrind] [--no-style]
#   --fix       reformat sources and apply safe lint fixes in place instead
#               of failing on format drift.
#   --valgrind  also run the tests under valgrind and fail on any definite
#               leak or memory error (see tools/valgrind.supp).
#   --no-style  skip the format and lint checks (for distro jobs that only
#               need the build and tests; style is checked elsewhere).

set -eu

FFLAGS="-std=f2018 -Wall -Wextra -fcheck=all -g"
# The linter version CI installs (tools/check.sh only reports it).
FORTITUDE_VERSION=0.9.2
DIRS="src app test docs/init.f90"
fix=false
memcheck=false
style=true
for arg in "$@"; do
    case "$arg" in
        --fix) fix=true ;;
        --valgrind) memcheck=true ;;
        --no-style) style=false ;;
        *) echo "usage: tools/check.sh [--fix] [--valgrind] [--no-style]" >&2; exit 2 ;;
    esac
done

cd "$(dirname "$0")/.."

if $style; then
    # Lint: Fortitude's default rules, configured in fpm.toml.  Fixes go
    # first so the formatter tidies what they change.
    if ! command -v fortitude >/dev/null 2>&1; then
        echo "fortitude $FORTITUDE_VERSION not found" >&2
        exit 1
    fi
    # src/commands_fm.f90 is generated from the `commands%NAME%run =>`
    # assignments, src/modes_fm.f90 from the `major_modes%NAME%define(`
    # and `minor_modes%NAME%define(` calls, src/face_table_fm.f90 from the
    # face declarations, src/docstrings_fm.f90 from the docstrings (help).
    if $fix; then
        tools/gen-commands.sh
        tools/gen-modes.sh
        tools/gen-faces.sh
        tools/gen-docstrings.sh
    else
        for pair in commands:commands_fm modes:modes_fm faces:face_table_fm; do
            gen=${pair%%:*}
            file=${pair#*:}
            fresh=${TMPDIR:-/tmp}/${file}.$$.f90
            tools/gen-$gen.sh "$fresh"
            if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { exit 1 } { m = FNR } END { exit m != n }' \
                src/${file}.f90 "$fresh"; then
                rm -f "$fresh"
                echo "src/${file}.f90 is out of date: run tools/gen-$gen.sh" >&2
                exit 1
            fi
            rm -f "$fresh"
        done
        fresh=${TMPDIR:-/tmp}/docstrings_fm.$$.f90
        tools/gen-docstrings.sh "$fresh"
        # (awk, not cmp or diff: the distro containers lack diffutils)
        if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { exit 1 } { m = FNR } END { exit m != n }' \
            src/docstrings_fm.f90 "$fresh"; then
            rm -f "$fresh"
            echo "src/docstrings_fm.f90 is out of date: run tools/gen-docstrings.sh" >&2
            exit 1
        fi
        rm -f "$fresh"
    fi

    if $fix; then
        fortitude check --fix $DIRS >/dev/null 2>&1 || true
    fi

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

    # Blank lines (CLAUDE.md, Fortran conventions): tools/blank-lines.awk.
    blank_drift=false
    fresh=${TMPDIR:-/tmp}/blank-lines.$$.f90
    for f in $(find $DIRS -name '*.f90' | sort); do
        awk -f tools/blank-lines.awk "$f" >"$fresh"
        if ! awk 'NR == FNR { a[NR] = $0; n = NR; next } a[FNR] != $0 { exit 1 } { m = FNR } END { exit m != n }' \
            "$f" "$fresh"; then
            if $fix; then
                cp "$fresh" "$f"
            else
                echo "$f: blank lines differ from tools/blank-lines.awk" >&2
                blank_drift=true
            fi
        fi
    done
    rm -f "$fresh"
    if $blank_drift; then
        echo "blank line drift: run tools/check.sh --fix" >&2
        exit 1
    fi

    # Hazards (PORTING.md, "Hazards"): tools/hazards.awk.
    if ! awk -f tools/hazards.awk $(find $DIRS -name '*.f90' | sort); then
        echo "hazards found: fix them, or mark a justified exception with ! hazard-ok(NAME): why" >&2
        exit 1
    fi

    fortitude check $DIRS
fi

fpm build --profile debug --flag "$FFLAGS"
fpm test --profile debug --flag "$FFLAGS"

# Each suite alone too, so none depends on state an earlier one left.
for suite in $(sed -n 's/.*new_testsuite("\([a-z_]*\)".*/\1/p' test/main.f90); do
    if ! fpm test --profile debug --flag "$FFLAGS" -- "$suite" >/dev/null 2>&1; then
        echo "test suite $suite fails when run alone: fpm test -- $suite" >&2
        exit 1
    fi
done

if $memcheck; then
    fpm test --profile debug --flag "$FFLAGS" --runner \
        "valgrind -q --leak-check=full --errors-for-leak-kinds=definite --error-exitcode=9 --suppressions=tools/valgrind.supp"
fi

# fortmacs --recompile against an installed copy.
tools/test-recompile.sh
