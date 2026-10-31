# fortran-prose.awk -- check prose for the Lisp wording of
# tools/fortran-prose.
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

# Usage: awk -v table=tools/fortran-prose -f tools/fortran-prose.awk FILE...
#
# Prints FILE:LINE: fortran-prose: LABEL (use USE) for each rejected
# term in a `!>' docstring or a message string, and exits with status 1
# if there is any.  The table's header says what is checked.

function fail(msg) {
    print "fortran-prose.awk: " msg > "/dev/stderr"
    failed = 1
    exit 2
}

BEGIN {
    if (table == "") fail("no table (-v table=tools/fortran-prose)")
    FS = "\t"
    while ((getline entry < table) > 0) {
        if (entry ~ /^#/ || entry == "") continue
        n = split(entry, f, "\t")
        if (f[1] == "reject" && n == 4) {
            nrej++
            label[nrej] = f[2]
            re[nrej] = f[3]
            use[nrej] = f[4]
        } else if (f[1] == "keep" && n == 2 && f[2] ~ /^".*"$/) {
            nkeep++
            keeptext[nkeep] = tolower(substr(f[2], 2, length(f[2]) - 2))
        } else if (f[1] == "keep" && n == 2) {
            keepfile[f[2]] = 1
        } else {
            fail(table ": bad entry: " entry)
        }
    }
    close(table)
    FS = " "
}

FNR == 1 {
    path = FILENAME
    sub(/^\.\//, "", path)
    skipfile = (path in keepfile) || path ~ /(^|\/)docstrings_fm\.f90$/ || path ~ /^test\//
    off = 0
}

skipfile { next }

/fortran-names: off/ { off = 1 }
/fortran-names: on/ { off = 0; next }
off { next }
/fortran-prose: keep|fortran-names: keep/ { next }

{
    text = ""
    if ($0 ~ /^[ \t]*!>/) {
        text = $0
        sub(/^[ \t]*!>/, "", text)
    } else if ($0 !~ /^[ \t]*!/ && $0 ~ /set_error\(|call message\(|user_error\(|format_message\(|minibuffer_message\(/) {
        text = strings($0)
    }
    if (text == "") next
    # Kept text first (matched ignoring case).
    for (k = 1; k <= nkeep; k++) {
        while ((p = index(tolower(text), keeptext[k])) > 0)
            text = substr(text, 1, p - 1) " " substr(text, p + length(keeptext[k]))
    }
    # Identifiers are names, not prose (tools/fortran-names checks them):
    # a quoted name (`plist'), a name with an underscore, and an
    # argument in capitals (PLIST).
    gsub(/`[A-Za-z0-9_]+'/, " ", text)
    gsub(/[A-Za-z0-9]*_[A-Za-z0-9_]*/, " ", text)
    gsub(/(^|[^A-Za-z0-9-])[A-Z][A-Z0-9]+([^A-Za-z0-9-]|$)/, " ", text)
    check(tolower(text))
}

# The string literals of a code line, joined by spaces.
function strings(s,    out, q, i, c, inq) {
    out = ""
    inq = ""
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (inq == "") {
            if (c == "!") break
            if (c == "\"" || c == "'") inq = c
        } else if (c == inq) {
            if (substr(s, i + 1, 1) == inq) { out = out c; i++ }
            else { inq = ""; out = out " " }
        } else {
            out = out c
        }
    }
    return out
}

function check(t,    k) {
    # Citations and Info node names.
    gsub(/[a-z0-9_.\/+-]+\.(el|c|h):[^ \t,;)]+/, " ", t)
    gsub(/upstream('s)? `?[a-z0-9]+(-[a-z0-9]+)+'?/, " ", t)
    gsub(/\(elisp\)[a-z0-9 -]*/, " ", t)
    for (k = 1; k <= nrej; k++) {
        if (t ~ re[k]) {
            printf "%s:%d: fortran-prose: %s (use %s)\n", FILENAME, FNR, label[k], use[k]
            nfindings++
        }
    }
}

END {
    if (failed) exit 2
    if (nfindings) exit 1
}
