#!/bin/sh
# build.sh -- generate the modules made from src/, then run fpm.
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

# Usage: tools/build.sh [--generate] [--force] [FPM-ARGUMENTS...]
#   Writes src/commands_mod.f90, src/modes_mod.f90, src/face_table_mod.f90
#   and src/docstrings_mod.f90 (tools/gen-commands.sh, gen-modes.sh,
#   gen-faces.sh and gen-docstrings.sh), which git does not track, then
#   runs fpm with FPM-ARGUMENTS in the current directory: `build' when
#   there are none, so `tools/build.sh test --target test_x',
#   `tools/build.sh run -- FILE' and `tools/build.sh install --profile
#   release' work as fpm's own commands do.
#   The generators run only when one of the four files is missing or one
#   of their inputs (src/, the generators, tools/fortran-names,
#   tools/fortran-names-lib.awk, tools/user-options, tools/blank-lines.awk,
#   .fprettify.rc) changed since the last run, which build/generated.stamp
#   records.  A file deleted or renamed in src/ changes its directory, so it
#   counts as a change too.
#   --generate  only generate; do not run fpm.
#   --force     generate even when nothing changed.

set -eu

generate_only=false
force=false
while [ $# -gt 0 ]; do
    case "$1" in
        --generate) generate_only=true ;;
        --force) force=true ;;
        *) break ;;
    esac
    shift
done

root=$(cd "$(dirname "$0")/.." && pwd)
stamp=$root/build/generated.stamp
outputs="src/commands_mod.f90 src/modes_mod.f90 src/face_table_mod.f90 src/docstrings_mod.f90"

# Whether the four files must be generated (again).
is_stale() {
    $force && return 0
    [ -f "$stamp" ] || return 0
    for f in $outputs; do
        [ -f "$root/$f" ] || return 0
    done
    newer=$(cd "$root" && find src tools/gen-commands.sh tools/gen-modes.sh tools/gen-faces.sh tools/gen-docstrings.sh \
        tools/fortran-names tools/fortran-names-lib.awk tools/user-options tools/blank-lines.awk .fprettify.rc \
        -newer "$stamp" | sed -n 1p)
    [ -n "$newer" ]
}

if is_stale; then
    echo "build.sh: generating $outputs"
    # In this order: each generator reads all of src/, the files made
    # before it included.
    "$root/tools/gen-commands.sh"
    "$root/tools/gen-modes.sh"
    "$root/tools/gen-faces.sh"
    "$root/tools/gen-docstrings.sh"
    mkdir -p "$root/build"
    touch "$stamp"
fi

$generate_only && exit 0
[ $# -eq 0 ] && set -- build
exec fpm "$@"
