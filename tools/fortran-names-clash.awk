# fortran-names-clash.awk -- for tools/fortran-names.sh: report the lines
# of a Fortran file where a rename produces an identifier the file already
# has, which would define or import it twice.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs, under the GNU GPL version 3 or later.

# Usage: awk -v f=NAME -f tools/fortran-names-clash.awk OLD NEW
#
# OLD is the file, NEW the same file renamed (the same lines, identifiers
# replaced by identifiers).  Prints NAME:LINE: OLD -> NEW clashes with the
# existing NEW, for each such rename in code (comments and strings aside).

# LINE without its string literals and trailing comment, lower case.
function code_of(s, out, i, c, q) {
    out = ""
    q = ""
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (q != "") {
            if (c == q) q = ""
            continue
        }
        if (c == "\"" || c == "'") {
            q = c
            out = out " "
            continue
        }
        if (c == "!") break
        out = out c
    }
    return tolower(out)
}

# Splits the identifiers of S into T; returns their count.
function idents(s, t, n) {
    n = 0
    while (match(s, /[a-z_][a-z0-9_]*/)) {
        t[++n] = substr(s, RSTART, RLENGTH)
        s = substr(s, RSTART + RLENGTH)
    }
    return n
}

NR == FNR {
    old[FNR] = code_of($0)
    n = idents(old[FNR], t)
    for (i = 1; i <= n; i++) have[t[i]] = 1
    next
}

{
    if (!(FNR in old)) next
    a = old[FNR]
    b = code_of($0)
    if (a == b) next
    na = idents(a, ta)
    nb = idents(b, tb)
    if (na != nb) next
    for (i = 1; i <= na; i++) {
        if (ta[i] != tb[i] && (tb[i] in have)) printf "%s:%d: %s -> %s clashes with the existing %s\n", f, FNR, ta[i], tb[i], tb[i]
    }
}
