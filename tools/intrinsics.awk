# intrinsics.awk -- report procedures that only wrap an intrinsic, and the
# Lisp helper names (CLAUDE.md: intrinsics and language constructs come
# first), in Fortran source files.
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

# Usage: awk -v baseline=tools/intrinsics-baseline -f tools/intrinsics.awk FILE.f90...
#
# Prints FILE:LINE: intrinsics: RULE: NAME ... for each finding and exits
# with status 1 if there is any.  The rules (PORTING.md, "Intrinsics
# first"):
#
#   wrapper      a function or subroutine whose executable part is one
#                statement that assigns one intrinsic (or `.not.' of one)
#                applied to its dummy arguments: `is_null = .not.
#                allocated(v%a)', `n = size(x)'.  Write the intrinsic.
#   lisp-helper  a procedure named after a Lisp helper (nilp, consp, car,
#                cdr, car_safe, cdr_safe, nth, nthcdr, memq, member,
#                length, fixnump, make_fixnum, xfixnum, nil, t_value).
#
# The baseline file lists the findings of today's code as FILE:NAME:RULE
# lines (they go in a later PR); one listed is not reported, and a listed
# one that no longer occurs is a finding too, so the baseline only
# shrinks.  Without -v baseline, nothing is excused.

function finding(line, rule, name, msg, key) {
    key = file ":" name ":" rule
    seen[key] = 1
    if (key in base) return
    printf "%s:%d: intrinsics: %s: %s%s\n", file, line, rule, name, msg
    nfindings++
}

function trim(s) {
    sub(/^[ \t]+/, "", s)
    sub(/[ \t]+$/, "", s)
    return s
}

# S without its string literals and trailing comment.
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
            out = out "S"
            continue
        }
        if (c == "!") break
        out = out c
    }
    return out
}

# Starts procedure NAME (dummies in ARGS, result RES) at line LINE.
function start_proc(name, args, res, line, n, i, a) {
    depth++
    pname[depth] = name
    pline[depth] = line
    pres[depth] = res
    pstmts[depth] = 0
    pstmt[depth] = ""
    pexec[depth] = 0
    pdone[depth] = 0
    for (i in dummy) if (index(i, depth SUBSEP) == 1) delete dummy[i]
    n = split(args, a, ",")
    for (i = 1; i <= n; i++) dummy[depth, tolower(trim(a[i]))] = 1
    if (tolower(name) in lisp) finding(line, "lisp-helper", name, "")
}

# Judges the body of the innermost procedure, once.
function judge(stmt, lhs, rest, intr, args, ok, tok, t) {
    if (pdone[depth]) return
    pdone[depth] = 1
    if (pstmts[depth] != 1) return
    stmt = tolower(pstmt[depth])
    if (!match(stmt, /^[a-z_][a-z0-9_]*[ \t]*=[ \t]*/)) return
    lhs = trim(substr(stmt, 1, RLENGTH))
    sub(/[ \t]*=$/, "", lhs)
    rest = substr(stmt, RLENGTH + 1)
    if (lhs != tolower(pres[depth]) && !((depth, lhs) in dummy)) return
    sub(/^\.not\.[ \t]*/, "", rest)
    if (!match(rest, /^[a-z_][a-z0-9_]*[ \t]*\(/)) return
    intr = rest
    sub(/[ \t]*\(.*/, "", intr)
    if (!(intr in intrinsic)) return
    args = substr(rest, RLENGTH + 1)
    if (args !~ /\)[ \t]*$/) return
    sub(/\)[ \t]*$/, "", args)
    # Only dummies (with components and elements), keywords, kinds and
    # literals: no other call.
    gsub(/\.[a-z]+\./, " ", args)
    gsub(/%[a-z_][a-z0-9_]*/, "", args)
    gsub(/[a-z_][a-z0-9_]*[ \t]*=/, " ", args)
    t = args
    while (match(t, /[a-z_][a-z0-9_]*/)) {
        tok = substr(t, RSTART, RLENGTH)
        t = substr(t, RSTART + RLENGTH)
        if ((depth, tok) in dummy || tok in kinds || tok == "s") continue
        return
    }
    finding(pline[depth], "wrapper", pname[depth], " (only `" intr "')")
}

BEGIN {
    n = split("allocated associated present size len len_trim any all count findloc pack merge " \
              "index scan verify trim adjustl adjustr maxval minval sum product abs max min mod " \
              "modulo iachar achar char ichar lbound ubound shape int real nint ceiling floor " \
              "repeat ior iand ieor btest ibset ibclr ishft popcnt maxloc minloc reshape spread " \
              "is_iostat_end is_iostat_eor huge tiny kind bit_size new_line lge lgt lle llt", w, " ")
    for (i = 1; i <= n; i++) intrinsic[w[i]] = 1
    n = split("nilp consp car cdr car_safe cdr_safe nth nthcdr memq member length fixnump " \
              "make_fixnum xfixnum nil t_value", w, " ")
    for (i = 1; i <= n; i++) lisp[w[i]] = 1
    n = split("int8 int16 int32 int64 real32 real64 real128 c_int c_long c_char", w, " ")
    for (i = 1; i <= n; i++) kinds[w[i]] = 1
    if (baseline != "") {
        while ((getline l <baseline) > 0) {
            if (l ~ /^#/ || l ~ /^[ \t]*$/) continue
            base[l] = 1
        }
        close(baseline)
    }
}

FNR == 1 {
    file = FILENAME
    depth = 0
    ininterface = 0
    cont = ""
}

{
    raw = $0
    c = code_of(raw)
    # Join continuation lines.
    if (cont != "") {
        t = c
        sub(/^[ \t]*&/, "", t)
        c = cont t
        startline = startline
    } else {
        startline = FNR
    }
    if (c ~ /&[ \t]*$/) {
        sub(/&[ \t]*$/, "", c)
        cont = c
        next
    }
    cont = ""
    l = tolower(trim(c))
    if (l == "") next
}

l ~ /^(abstract[ \t]+)?interface([ \t]|$)/ { ininterface++; next }
l ~ /^end[ \t]*interface/ { ininterface--; next }
ininterface { next }

# The start of a procedure.
l ~ /^((pure|elemental|recursive|impure|module|non_recursive)[ \t]+|(logical|integer|real|complex|character|type|class)(\([^)]*\))?[ \t]+)*(function|subroutine)[ \t]+[a-z_][a-z0-9_]*/ {
    s = l
    sub(/^.*(function|subroutine)[ \t]+/, "", s)
    name = s
    sub(/[^a-z0-9_].*/, "", name)
    args = ""
    if (match(s, /\([^)]*\)/)) args = substr(s, RSTART + 1, RLENGTH - 2)
    res = name
    if (match(s, /result[ \t]*\([ \t]*[a-z_][a-z0-9_]*/)) {
        res = substr(s, RSTART, RLENGTH)
        sub(/.*\([ \t]*/, "", res)
    }
    start_proc(name, args, res, startline)
    next
}

# A separate module procedure (its dummies are its interface's).
l ~ /^module[ \t]+procedure[ \t]+[a-z_][a-z0-9_]*[ \t]*$/ {
    name = l
    sub(/^module[ \t]+procedure[ \t]+/, "", name)
    start_proc(trim(name), "", "", startline)
    pdone[depth] = 1
    next
}

l ~ /^end[ \t]*(function|subroutine|procedure)/ {
    if (depth > 0) {
        judge()
        depth--
    }
    next
}

depth > 0 && l ~ /^contains$/ { judge(); next }

# Declarations.
depth > 0 && !pexec[depth] && l ~ /^(implicit|use[ \t,]|import|integer|logical|real|complex|character|type[ \t]*\(|class[ \t]*\(|procedure[ \t]*\(|external|intrinsic|dimension|parameter|save|data[ \t]|optional|intent|allocatable|pointer|target|value|contiguous|volatile|asynchronous|enum|enumerator|end[ \t]*enum)/ { next }

depth > 0 && !pdone[depth] {
    pexec[depth] = 1
    pstmts[depth]++
    pstmt[depth] = l
}

END {
    for (k in base) {
        if (!(k in seen)) {
            printf "%s: intrinsics: baseline entry no longer occurs: %s (remove it)\n", baseline, k
            nfindings++
        }
    }
    exit nfindings > 0
}
