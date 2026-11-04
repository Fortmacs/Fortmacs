#!/bin/sh
# split-table-additions.sh -- for a branch written before tools/fortran-names
# and tools/user-options became directories: move the lines it added to
# their 0-base files into files of its own, and drop its edits of
# tools/unported-commands (generated on main).
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs, under the GNU GPL version 3 or later.

# Usage: tools/split-table-additions.sh [NAME [BASE]]
#
# Run it after merging main (git carries the branch's additions over to
# tools/fortran-names.d/0-base and tools/user-options.d/0-base; keep both
# sides of a conflict there first).  NAME is the branch's file name,
# DATE-BRANCH like its session log (default: the one docs/log/*-BRANCH.md
# of the current branch); BASE is what the branch is compared with
# (default: origin/main).  For each table, the lines the branch added to
# 0-base are appended to tools/TABLE.d/NAME, in their order, and 0-base is
# restored to BASE's.  A branch that also removed or changed lines of a
# 0-base is left as it was, and the lines are shown: move those by hand.
# tools/unported-commands is restored to BASE's.  Then review and commit.

set -eu

cd "$(dirname "$0")/.."

branch=$(git rev-parse --abbrev-ref HEAD)
name=${1:-}
base=${2:-origin/main}
if [ -z "$name" ]; then
    set -- docs/log/*-"$branch".md
    if [ $# -ne 1 ] || [ ! -f "$1" ]; then
        echo "split-table-additions.sh: no single docs/log/*-$branch.md; give NAME (DATE-BRANCH)" >&2
        exit 2
    fi
    name=$(basename "$1" .md)
fi

# Lines the branch removed from a 0-base: nothing is moved then.
changed=false
for table in fortran-names user-options; do
    removed=$(git diff -U0 "$base" -- "tools/$table.d/0-base" | awk '/^-/ && !/^--- /')
    if [ -n "$removed" ]; then
        echo "tools/$table.d/0-base: lines removed or changed; move them by hand:" >&2
        printf '%s\n' "$removed" >&2
        changed=true
    fi
done
$changed && exit 1

for table in fortran-names user-options; do
    file=tools/$table.d/0-base
    git diff -U0 "$base" -- "$file" | awk '/^\+/ && !/^\+\+\+ / { print substr($0, 2) }' >"$file.added"
    if [ -s "$file.added" ]; then
        cat "$file.added" >>"tools/$table.d/$name"
        git checkout -q "$base" -- "$file"
        git add "tools/$table.d/$name"
        echo "tools/$table.d/$name: $(wc -l <"$file.added") line(s) moved from 0-base"
    fi
    rm -f "$file.added"
done

if ! git diff --quiet "$base" -- tools/unported-commands; then
    git checkout -q "$base" -- tools/unported-commands
    echo "tools/unported-commands: restored (the Progress workflow regenerates it on main)"
fi
