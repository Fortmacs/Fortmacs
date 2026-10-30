#!/bin/sh
# gen-unispecial.sh -- generate src/uni_special_casing_fm.f90, the title
# case and special casing tables.
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

# Upstream's unidata-gen.el writes the `titlecase' property table
# (uni-titlecase.el) from field 14 of UnicodeData.txt, and the
# `special-uppercase', `special-lowercase' and `special-titlecase' tables
# (uni-special-*.el) from the unconditional entries of SpecialCasing.txt
# whose mapping is not a single character (unidata-gen-table-special-casing).
# casefiddle.c uses them.  This script writes the title case mappings that
# differ from the simple upper case mapping, and the special casings, each
# as sorted arrays.
#
# Usage: tools/gen-unispecial.sh [UnicodeData.txt [SpecialCasing.txt]]
#   The defaults are in upstream/emacs/admin/unidata (see
#   tools/fetch-upstream.sh).  The output replaces
#   src/uni_special_casing_fm.f90.

set -eu

cd "$(dirname "$0")/.."
data=${1:-upstream/emacs/admin/unidata/UnicodeData.txt}
special=${2:-upstream/emacs/admin/unidata/SpecialCasing.txt}
out=src/uni_special_casing_fm.f90

awk -F ';' '
function hex(s,    i, v) {
    s = toupper(s)
    gsub(/ /, "", s)
    v = 0
    for (i = 1; i <= length(s); i++) v = v * 16 + index("0123456789ABCDEF", substr(s, i, 1)) - 1
    return v
}
function sortnum(a, n,    i, j, t, gap) {
    for (gap = int(n / 2); gap > 0; gap = int(gap / 2))
        for (i = gap + 1; i <= n; i++) {
            t = a[i]
            for (j = i; j > gap && a[j - gap] > t; j -= gap) a[j] = a[j - gap]
            a[j] = t
        }
}
function array(name, count, a, n,    i, line) {
    printf "    integer(int32), parameter :: %s(%s) = [ &\n", name, count
    line = ""
    for (i = 1; i <= n; i++) {
        line = line sprintf("%d", a[i])
        if (i < n) line = line ", "
        if (i % 10 == 0 || i == n) {
            sub(/ $/, "", line)
            printf "        %s &\n", line
            line = ""
        }
    }
    printf "        ]\n\n"
}
# Split the code points of mapping S into M (as numbers); return how many.
function codes(s, m,    parts, n, i, k) {
    n = split(s, parts, " ")
    k = 0
    for (i = 1; i <= n; i++) if (parts[i] != "") m[++k] = hex(parts[i])
    return k
}
FNR == NR {
    # UnicodeData.txt: title case (field 14) where it differs from upper
    # case (field 12).
    if ($15 != "") {
        c = hex($1)
        t = hex($15)
        u = ($13 != "") ? hex($13) : c
        if (t != u) { nt++; tkeys[nt] = c; title[c] = t }
    }
    next
}
/^#/ || /^[ \t]*$/ { next }
# SpecialCasing.txt: code; lower; title; upper; # comment -- entries with
# a condition have one more field.
NF == 5 {
    c = hex($1)
    for (p = 1; p <= 3; p++) {
        k = codes($(p + 1), m)
        if (k != 1) {
            ns[p]++
            skeys[p, ns[p]] = c
            slen[p, c] = k
            for (i = 1; i <= 3; i++) sval[p, c, i] = (i <= k) ? m[i] : 0
        }
    }
}
END {
    print "! uni_special_casing_fm.f90 -- title case and special casing.  GENERATED"
    print "! by tools/gen-unispecial.sh from admin/unidata/UnicodeData.txt and"
    print "! admin/unidata/SpecialCasing.txt of GNU Emacs 31.1, the sources of"
    print "! lisp/international/uni-titlecase.el and uni-special-*.el.  Do not edit."
    print "!"
    print "! Copyright (C) 1991-2026 Unicode, Inc. (the data; see"
    print "! admin/unidata/copyright.html upstream)"
    print "! Copyright (C) 2026 The Fortmacs authors"
    print "!"
    print "! This file is part of Fortmacs."
    print "!"
    print "! Fortmacs is free software: you can redistribute it and/or modify"
    print "! it under the terms of the GNU General Public License as published by"
    print "! the Free Software Foundation, either version 3 of the License, or"
    print "! (at your option) any later version."
    print "!"
    print "! Fortmacs is distributed in the hope that it will be useful,"
    print "! but WITHOUT ANY WARRANTY; without even the implied warranty of"
    print "! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    print "! GNU General Public License for more details."
    print "!"
    print "! You should have received a copy of the GNU General Public License"
    print "! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>."
    print ""
    print "!> The title case mappings that differ from the simple upper case"
    print "!> mapping (`title_keys` to `title_values`), and the unconditional"
    print "!> special casings that are not a single character: for each of lower"
    print "!> (1), title (2) and upper (3) case, the characters (`special_keys_N`),"
    print "!> the length of their mapping (`special_lengths_N`) and up to three"
    print "!> characters of it (`special_values_N`, three per key, 0 for unused)."
    print "!> Used by casefiddle_fm."
    print "module uni_special_casing_fm"
    print "    use, intrinsic :: iso_fortran_env, only: int32"
    print "    implicit none(type, external)"
    print "    private"
    print ""
    print "    public :: title_count, title_keys, title_values"
    print "    public :: special_count_1, special_keys_1, special_lengths_1, special_values_1"
    print "    public :: special_count_2, special_keys_2, special_lengths_2, special_values_2"
    print "    public :: special_count_3, special_keys_3, special_lengths_3, special_values_3"
    print ""
    sortnum(tkeys, nt)
    for (i = 1; i <= nt; i++) tvals[i] = title[tkeys[i]]
    printf "    integer, parameter :: title_count = %d\n\n", nt
    array("title_keys", "title_count", tkeys, nt)
    array("title_values", "title_count", tvals, nt)
    for (p = 1; p <= 3; p++) {
        n = 0
        for (i = 1; i <= ns[p]; i++) keys[++n] = skeys[p, i]
        sortnum(keys, n)
        nv = 0
        for (i = 1; i <= n; i++) {
            lens[i] = slen[p, keys[i]]
            for (j = 1; j <= 3; j++) vals[++nv] = sval[p, keys[i], j]
        }
        printf "    integer, parameter :: special_count_%d = %d\n\n", p, n
        array("special_keys_" p, "special_count_" p, keys, n)
        array("special_lengths_" p, "special_count_" p, lens, n)
        array("special_values_" p, "3*special_count_" p, vals, nv)
    }
    print "end module uni_special_casing_fm"
}' "$data" "$special" > "$out.tmp"
# The last blank line before "end module" goes.
awk 'NR > 1 { if (!(prev == "" && $0 ~ /^end module/)) print prev } { prev = $0 } END { print prev }' "$out.tmp" > "$out"
rm -f "$out.tmp"
if command -v fprettify >/dev/null 2>&1; then
    fprettify --config-file .fprettify.rc "$out" 2>/dev/null
fi
# The blank lines of CLAUDE.md.
awk -f tools/blank-lines.awk "$out" >"$out.bl" && mv "$out.bl" "$out"
echo "wrote $out"
