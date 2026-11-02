#!/bin/sh
# lisp-data.sh -- the Lisp-data ratchet (tools/lisp-data.awk): check the
# counts against tools/lisp-data-baseline, or --update it when no count
# rose.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs, under the GNU GPL version 3 or later.
# Usage: tools/lisp-data.sh [--update]   (run by tools/check.sh)
set -eu
cd "$(dirname "$0")/.."
files=$(find src app -name '*.f90' | sort)
case ${1:-} in
--update)
    # Refuse when a count rose: the baseline only shrinks.
    if awk -v mode=check -v baseline=tools/lisp-data-baseline -f tools/lisp-data.awk $files 2>/dev/null | grep -q 'new'; then
        awk -v mode=check -v baseline=tools/lisp-data-baseline -f tools/lisp-data.awk $files | grep 'new' >&2
        echo "lisp-data.sh: counts rose; write Fortran instead (CLAUDE.md: never reached by a name string)" >&2
        exit 1
    fi
    { sed -n '1,/^$/p' tools/lisp-data-baseline; awk -v mode=write -f tools/lisp-data.awk $files; } >tools/lisp-data-baseline.new
    mv tools/lisp-data-baseline.new tools/lisp-data-baseline
    ;;
"")
    awk -v mode=check -v baseline=tools/lisp-data-baseline -f tools/lisp-data.awk $files
    ;;
*)
    echo "usage: tools/lisp-data.sh [--update]" >&2
    exit 2
    ;;
esac
