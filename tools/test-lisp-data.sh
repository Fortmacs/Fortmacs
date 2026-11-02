#!/bin/sh
# test-lisp-data.sh -- test tools/lisp-data.awk on the fixture in
# tools/lisp-data-test: the counts match the baseline, a new use fails,
# a dropped use asks for an update.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs, under the GNU GPL version 3 or later.
set -eu
cd "$(dirname "$0")/.."
f=tools/lisp-data-test/probe_mod.f90
b=tools/lisp-data-test/baseline
work=${TMPDIR:-/tmp}/test-lisp-data.$$
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/tools/lisp-data-test"
cp "$f" "$work/tools/lisp-data-test/"
if ! (cd "$work" && awk -v mode=check -v baseline="$OLDPWD/$b" -f "$OLDPWD/tools/lisp-data.awk" "$f") >/dev/null; then
    echo "test-lisp-data.sh: the fixture does not match its baseline" >&2
    exit 1
fi
sed 's/w = value_type(name="x")/w = value_type(name="x"); u = value_type(name="y")/' "$f" >"$work/$f"
if (cd "$work" && awk -v mode=check -v baseline="$OLDPWD/$b" -f "$OLDPWD/tools/lisp-data.awk" "$f") >/dev/null; then
    echo "test-lisp-data.sh: a new name-value use was not reported" >&2
    exit 1
fi
sed '/w = value_type(name="x")/d' "$f" >"$work/$f"
if (cd "$work" && awk -v mode=check -v baseline="$OLDPWD/$b" -f "$OLDPWD/tools/lisp-data.awk" "$f") >/dev/null; then
    echo "test-lisp-data.sh: a dropped use did not ask for an update" >&2
    exit 1
fi
echo "test-lisp-data: ok"
