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
#               leak or memory error (see tools/valgrind.supp): each suite
#               in its own process, as many at a time as there are CPUs.
#   --valgrind-combined
#               like --valgrind, then also all the suites under valgrind in
#               one process, for a leak that only one suite running after
#               another causes (CI does this on main after each merge).
#   --no-style  skip the format and lint checks (for distro jobs that only
#               need the build and tests; style is checked elsewhere).

set -eu

FFLAGS="-std=f2018 -Wall -Wextra -fcheck=all -g"
# The linter version CI installs (tools/check.sh only reports it).
FORTITUDE_VERSION=0.9.2
DIRS="src app test docs/init.f90"
fix=false
memcheck=false
memcheck_combined=false
style=true
for arg in "$@"; do
    case "$arg" in
        --fix) fix=true ;;
        --valgrind) memcheck=true ;;
        --valgrind-combined) memcheck=true; memcheck_combined=true ;;
        --no-style) style=false ;;
        *) echo "usage: tools/check.sh [--fix] [--valgrind | --valgrind-combined] [--no-style]" >&2; exit 2 ;;
    esac
done

cd "$(dirname "$0")/.."

# The modules generated from src/ (git does not track them):
# src/commands_mod.f90 from the `commands%NAME%execute =>` assignments,
# src/modes_mod.f90 from the `major_modes%NAME%define(` and
# `minor_modes%NAME%define(` calls, src/face_table_mod.f90 from the face
# declarations, src/docstrings_mod.f90 from the docstrings (help).  They
# are made before the style checks, which read them too; --fix makes them
# even when tools/build.sh finds nothing changed.
if $fix; then
    tools/build.sh --generate --force
else
    tools/build.sh --generate
fi

if $style; then
    # Lint: Fortitude's default rules, configured in fpm.toml.  Fixes go
    # first so the formatter tidies what they change.
    if ! command -v fortitude >/dev/null 2>&1; then
        echo "fortitude $FORTITUDE_VERSION not found" >&2
        exit 1
    fi
    # tools/unported-commands (the Deferred list for commands, CLAUDE.md) is
    # not checked here: the Progress workflow regenerates it on main after
    # each merge, so branches never touch it.

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

    # Audit (PORTING.md, "Audit"): the header, name, suffix, doc and port
    # rules, and the suffix rule's own test.
    if ! awk -v baseline=tools/audit-baseline -f tools/audit.awk $(find $DIRS -name '*.f90' | sort); then
        echo "audit findings: fix them (PORTING.md, \"Audit\")" >&2
        exit 1
    fi
    tools/test-audit.sh

    # Hazards (PORTING.md, "Hazards"): tools/hazards.awk, and its own test.
    if ! awk -f tools/hazards.awk $(find $DIRS -name '*.f90' | sort); then
        echo "hazards found: fix them, or mark a justified exception with ! hazard-ok(NAME): why" >&2
        exit 1
    fi
    tools/test-hazards.sh

    # Fortran names for Lisp terms (PORTING.md, "Fortran names for Lisp
    # terms"): tools/fortran-names.awk, and its rename script's own test.
    if ! awk -v mode=check -v table=tools/fortran-names.d -f tools/fortran-names-lib.awk -f tools/fortran-names.awk \
        $(find $DIRS -name '*.f90' | sort); then
        echo "old names for Lisp terms: run tools/fortran-names.sh (PORTING.md, \"Fortran names for Lisp terms\")" >&2
        exit 1
    fi
    tools/test-fortran-names.sh
    # Two branches can add the same entry, each in its own file of
    # tools/fortran-names.d: an exact duplicate line in any two files (or one)
    # is an error.
    dups=$(find tools/fortran-names.d -type f | LC_ALL=C sort |
        xargs awk '($1 == "name" || $1 == "keep") { if ($0 in seen) print FILENAME ":" FNR ": " $0 " (also " seen[$0] ")"; seen[$0] = FILENAME ":" FNR }')
    if [ -n "$dups" ]; then
        echo "tools/fortran-names.d: duplicate entries:" >&2
        echo "$dups" >&2
        exit 1
    fi

    # The wording of docstrings and messages (PORTING.md, "Fortran names
    # for Lisp terms", "Wording"): tools/fortran-prose.awk, and its test.
    if ! awk -v table=tools/fortran-prose -f tools/fortran-prose.awk $(find $DIRS -name '*.f90' | sort); then
        echo "Lisp wording in docstrings or messages: reword it (PORTING.md, \"Fortran names for Lisp terms\")" >&2
        exit 1
    fi
    tools/test-fortran-prose.sh

    # The Lisp-data ratchet (CLAUDE.md: compile-time things are never reached
    # by a name string; PORTING.md, "Known debt"): tools/lisp-data.awk against
    # its shrinking baseline, and its test.
    if ! tools/lisp-data.sh; then
        echo "Lisp data access rose: reach the thing through its typed object (CLAUDE.md), or shrink tools/lisp-data-baseline with tools/lisp-data.sh --update" >&2
        exit 1
    fi
    tools/test-lisp-data.sh

    # Intrinsics first (PORTING.md, "Intrinsics first"): tools/intrinsics.awk
    # with its shrinking baseline, and its own test.
    if ! awk -v baseline=tools/intrinsics-baseline -f tools/intrinsics.awk $(find $DIRS -name '*.f90' | sort); then
        echo "thin wrappers of intrinsics or Lisp helpers: use the intrinsic (PORTING.md, \"Intrinsics first\")" >&2
        exit 1
    fi
    tools/test-intrinsics.sh

    # American spelling (CLAUDE.md): tools/spelling.awk.  The session logs
    # (docs/log) are dated records and are not checked.
    if ! awk -f tools/spelling.awk tools/american-spelling README.md CLAUDE.md PORTING.md docs/status/*.md \
        $(find $DIRS -name '*.f90' ! -name docstrings_mod.f90 | sort) tools/*.sh tools/*.awk; then
        echo "British spellings: use the American ones (or keep the text in tools/american-spelling)" >&2
        exit 1
    fi

    # The status files of the modules (PORTING.md, "Status"): their format.
    tools/status.sh --check

    # Lisp candidates (PORTING.md, "Fortran-first decision guide"): Lisp
    # vocabulary in the lines this branch adds.  It only warns.
    tools/lisp-candidates.sh
    tools/test-lisp-candidates.sh

    fortitude check $DIRS
fi

fpm build --profile debug --flag "$FFLAGS"
fpm test --profile debug --flag "$FFLAGS"

# The suites run one at a time below are run as many at a time as there
# are CPUs: each writes only to its own build/test-* directory and to
# temporary files of unique names, so two different suites never share a
# file.  A suite's output is kept in build/check-logs/SUITE and shown if
# it fails.
tester=$(fpm test --profile debug --flag "$FFLAGS" --runner echo 2>/dev/null | tail -n 1)
suites=$(sed -n 's/.*new_testsuite("\([a-z_]*\)".*/\1/p' test/main.f90)
jobs=$(nproc 2>/dev/null || echo 1)
logs=build/check-logs
VALGRIND="valgrind -q --leak-check=full --errors-for-leak-kinds=definite --error-exitcode=9 --suppressions=tools/valgrind.supp"

# Run every suite alone, each by PREFIX TESTER SUITE ($1 is PREFIX, empty
# for none; $2 names the pass), and fail on the first that fails.
run_suites() {
    rm -rf "$logs"
    mkdir -p "$logs"
    printf '%s\n' $suites | xargs -P "$jobs" -I SUITE sh -c \
        "$1 \"$tester\" SUITE >\"$logs/SUITE\" 2>&1 || echo SUITE >>\"$logs/failed\""
    if [ -s "$logs/failed" ]; then
        for suite in $(sort "$logs/failed"); do
            echo "test suite $suite fails $2: fpm test -- $suite" >&2
            cat "$logs/$suite" >&2
        done
        exit 1
    fi
}

# Each suite alone too, so none depends on state an earlier one left.
run_suites "" "when run alone"

if $memcheck; then
    run_suites "$VALGRIND" "under valgrind"
fi

if $memcheck_combined; then
    fpm test --profile debug --flag "$FFLAGS" --runner "$VALGRIND"
fi

# fortmacs --recompile against an installed copy.
tools/test-recompile.sh
