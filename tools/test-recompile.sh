#!/bin/sh
# test-recompile.sh -- install Fortmacs into a scratch prefix and check that
# `fortmacs --recompile` builds an init.f90 into a binary that the
# installed fortmacs then runs.
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

# Usage: tools/test-recompile.sh  (from anywhere; run by tools/check.sh)

set -eu

cd "$(dirname "$0")/.."
root=$(pwd)/build/test-recompile
rm -rf "$root"
mkdir -p "$root"
fpm install --profile release --prefix "$root/prefix" >"$root/install.log" 2>&1 ||
    { cat "$root/install.log" >&2; exit 1; }

XDG_CONFIG_HOME=$root/config
XDG_STATE_HOME=$root/state
XDG_CACHE_HOME=$root/cache
export XDG_CONFIG_HOME XDG_STATE_HOME XDG_CACHE_HOME
fm=$root/prefix/bin/fortmacs
init=$XDG_CONFIG_HOME/fortmacs/init.f90
out=$root/out

fail() {
    echo "test-recompile: $*" >&2
    [ -f "$out" ] && cat "$out" >&2
    exit 1
}

# Without an init file there is nothing to build.
if "$fm" --recompile >"$out" 2>&1; then fail "--recompile without an init file succeeded"; fi
grep -q "Opening init file: No such file or directory, $init" "$out" || fail "wrong error without an init file"

# The example builds, and the installed fortmacs runs the build.
mkdir -p "$XDG_CONFIG_HOME/fortmacs"
cp docs/init.f90 "$init"
"$fm" --recompile >"$out" 2>&1 || fail "--recompile of docs/init.f90 failed"
[ -x "$XDG_STATE_HOME/fortmacs/fortmacs" ] || fail "no build in $XDG_STATE_HOME/fortmacs"
"$fm" --version >"$out" 2>&1 || fail "--version failed"
grep -q "With the configuration in $init" "$out" || fail "the installed fortmacs did not run the build"

# A broken init file: the compiler's errors are shown and the last good
# build stays.
printf 'module user_config\n' >"$init"
if "$fm" --recompile >"$out" 2>&1; then fail "--recompile of a broken init file succeeded"; fi
grep -q "Compiling $init failed" "$out" || fail "no failure message"
grep -q "Error:" "$out" || fail "no compiler errors shown"
"$fm" --version >"$out" 2>&1 || fail "--version failed"
grep -q "With the configuration in $init" "$out" || fail "the last good build was lost"

echo "test-recompile: ok"
