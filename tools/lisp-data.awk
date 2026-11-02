# lisp-data.awk -- count the places that reach a compile-time thing through
# Lisp data (a name string), per file, against a baseline that only shrinks
# (CLAUDE.md: compile-time things are never reached by a name string;
# PORTING.md, "Known debt").
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
# Usage: awk -v mode=check -v baseline=tools/lisp-data-baseline -f tools/lisp-data.awk FILE.f90...
#        awk -v mode=write -f tools/lisp-data.awk FILE.f90...   (prints a baseline)
#
# The patterns (the rows of the baseline, FILE<TAB>PATTERN<TAB>COUNT):
#   name-value    value_type(name="...")  a compile-time name as a value
#   set-by-name   set(value_type(name=    a variable written through the name table
#   get-by-name   symbol_value(value_type(name=  a variable read through it
#   intern        intern("...")           a literal name interned
#   name-dispatch select case (name)      a command or mode chosen by its name string
#
# In check mode, exits 1 when a file's count exceeds its baseline (new Lisp
# data access: write Fortran instead), or when it is below it (the
# baseline must shrink: run tools/lisp-data.sh --update), or when a file
# has a count and no row.  Generated files (docstrings_mod) are not counted.

BEGIN {
    np = 0
    add("name-value", "value_type\\(name=\"")
    add("set-by-name", "set\\(value_type\\(name=")
    add("get-by-name", "symbol_value\\(value_type\\(name=")
    add("intern", "intern\\(\"")
    add("name-dispatch", "select case \\((trim\\()?name\\)?\\)")
    if (mode == "check") {
        if (baseline == "") { print "lisp-data.awk: no baseline (-v baseline=tools/lisp-data-baseline)" > "/dev/stderr"; exit 2 }
        while ((getline l < baseline) > 0) {
            if (l ~ /^#/ || l == "") continue
            split(l, f, "\t")
            base[f[1] "\t" f[2]] = f[3] + 0
        }
        close(baseline)
    }
}

function add(label, re) { np++; plabel[np] = label; pre[np] = re }

FNR == 1 {
    path = FILENAME
    sub(/^\.\//, "", path)
    skip = path ~ /(^|\/)docstrings_mod\.f90$/
}

!skip {
    line = $0
    sub(/!.*/, "", line)
    for (i = 1; i <= np; i++) {
        rest = line
        while (match(rest, pre[i])) {
            count[path "\t" plabel[i]]++
            rest = substr(rest, RSTART + RLENGTH)
        }
    }
}

END {
    if (mode == "write") {
        n = 0
        for (k in count) keys[++n] = k
        sort(keys, n)
        for (i = 1; i <= n; i++) printf "%s\t%d\n", keys[i], count[keys[i]]
        exit 0
    }
    bad = 0
    for (k in count) {
        if (!(k in base)) { printf "%s: lisp-data: %d new (no baseline row)\n", k, count[k]; bad = 1 }
        else if (count[k] > base[k]) { printf "%s: lisp-data: %d, baseline %d: new Lisp data access\n", k, count[k], base[k]; bad = 1 }
        else if (count[k] < base[k]) { printf "%s: lisp-data: %d, baseline %d: run tools/lisp-data.sh --update\n", k, count[k], base[k]; bad = 1 }
    }
    for (k in base) if (!(k in count)) { printf "%s: lisp-data: gone, baseline %d: run tools/lisp-data.sh --update\n", k, base[k]; bad = 1 }
    exit bad
}

# Insertion sort of A[1..N] (gawk's asort is not in mawk).
function sort(a, n, i, j, t) {
    for (i = 2; i <= n; i++) {
        t = a[i]
        for (j = i - 1; j >= 1 && a[j] > t; j--) a[j + 1] = a[j]
        a[j + 1] = t
    }
}
