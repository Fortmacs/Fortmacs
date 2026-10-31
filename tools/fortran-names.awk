# fortran-names.awk -- apply or check the Fortran names for Lisp terms
# (tools/fortran-names).
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

# Usage: awk -v mode=MODE -v table=tools/fortran-names \
#            -f tools/fortran-names-lib.awk -f tools/fortran-names.awk FILE...
#
#   mode=apply  print FILE (one file) with the table applied; run by
#               tools/fortran-names.sh.
#   mode=path   print each line of FILE (a path) with its base name
#               renamed.
#   mode=check  print FILE:LINE: fortran-names: OLD -> NEW for each name
#               the table would still rename in Fortran code or in a
#               docstring (`!>'; other comments are not checked), and
#               FILE: fortran-names: file name: ... for a file of an old
#               name; exit with status 1 if there is any.  Run by
#               tools/check.sh.
#
# The format of the table and what is protected (citations, hyphenated
# and C names, upstream names, kept files, names and text, marked lines)
# are described at its top.

BEGIN {
    if (mode != "apply" && mode != "path" && mode != "check") fail("mode must be apply, path or check")
    if (table == "") fail("no table (-v table=tools/fortran-names)")
    load_names(table)
}

FNR == 1 {
    whole = whole_file_kept()
    off = 0
    if (mode == "check") {
        new = check_file_name(FILENAME)
        base = FILENAME
        sub(/.*\//, "", base)
        if (new != base) {
            printf "%s: fortran-names: file name: %s -> %s\n", FILENAME, base, new
            nfindings++
        }
    }
}

{
    line = $0
    keep = 0
    if (line ~ /fortran-names: off/) off = 1
    if (line ~ /fortran-names: (keep|off|on)/ || off || whole) keep = 1
    if (line ~ /fortran-names: on/) off = 0
}

mode == "apply" {
    print (keep ? line : apply_line(line))
    next
}

mode == "path" {
    dir = line
    if (!sub(/[^\/]*$/, "", dir)) dir = ""
    print dir check_file_name(line)
    next
}

mode == "check" && !keep && tolower(line) ~ fn_cand {
    c = comment_start(line)
    code = c ? substr(line, 1, c - 1) : line
    doc = (c && substr(line, c, 2) == "!>") ? substr(line, c) : ""
    apply_line(code)
    if (fn_nrenamed == 0 && doc != "") apply_line(doc)
    if (fn_nrenamed > 0) {
        printf "%s:%d: fortran-names: %s\n", FILENAME, FNR, fn_what
        nfindings++
    }
}

END {
    if (fn_names_failed) exit 2
    if (mode == "check" && nfindings) exit 1
}
