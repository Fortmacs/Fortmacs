# lisp-candidates.awk -- report Lisp vocabulary (tools/lisp-candidates) in
# the lines a change adds, read from `git diff -U0` output.
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

# Usage: git diff -U0 BASE -- FILES | awk -v words=tools/lisp-candidates \
#            -v keep=tools/fortran-names -f tools/lisp-candidates.awk
#
# Prints FILE:LINE: lisp candidate: WORD for each word of the list in an
# added line, once per word and line.  Skips the files that
# tools/fortran-names keeps (Lisp modes, their tests) and citations of
# upstream names.  Always exits 0: these are candidates to decide on
# (PORTING.md, "Fortran-first decision guide"), not errors.

BEGIN {
    while ((getline line < words) > 0) {
        if (line ~ /^#/ || line ~ /^[ \t]*$/) continue
        word[tolower(line)] = 1
    }
    close(words)
    while ((getline line < keep) > 0) {
        n = split(line, f, "\t")
        if (f[1] == "keep" && n == 2 && f[2] ~ /^(src|test|tools|app|docs)\//) keepfile[f[2]] = 1
    }
    close(keep)
}

/^\+\+\+ / {
    file = substr($0, 5)
    sub(/^b\//, "", file)
    next
}

/^@@ / {
    # @@ -a,b +c,d @@: the added lines start at line c.
    split($3, p, ",")
    lnum = substr(p[1], 2) + 0
    next
}

/^\+/ {
    text = substr($0, 2)
    if (!(file in keepfile)) scan(text)
    lnum++
}

function scan(text,    t, n, i, w, seen) {
    t = text
    # Citations keep upstream names: "Port of FILE.el:name", "lisp.h:x".
    gsub(/[A-Za-z0-9_.\/-]+\.(el|c|h):[A-Za-z0-9_*+-]+/, " ", t)
    n = split(t, toks, /[^A-Za-z0-9]+/)
    for (i = 1; i <= n; i++) {
        w = tolower(toks[i])
        if ((w in word) && !(w in seen)) {
            seen[w] = 1
            printf "%s:%d: lisp candidate: %s\n", file, lnum, w
            found++
        }
    }
}

END {
    if (found) {
        printf "%d Lisp candidate(s) in new lines: decide each one (PORTING.md, \"Fortran-first decision guide\") and list them in the PR description\n", found
    }
    exit 0
}
