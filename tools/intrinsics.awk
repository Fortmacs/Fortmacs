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
#                allocated(v%a)', `n = size(x)'; or that only tests a
#                value's tag or reads its component: `is_integer =
#                v%a(1)%tag == V_INTEGER', `integer_of = v%a(1)%i'.  Write
#                the intrinsic, or the test or read itself.
#   lisp-helper  a procedure named after a Lisp helper (nil, nilp,
#                t_value, sym, consp, cons, car, cdr, car_safe, cdr_safe,
#                nth, nthcdr, memq, member, length, fixnump, make_fixnum,
#                xfixnum, xstring, xsymbol, xhandle, is_integer,
#                integer_of, funcall, funcall_noargs, apply, put, get,
#                plist_get, plist_put, plist_member, intern, intern_soft,
#                setq_local, boundp, fboundp, makunbound, keywordp,
#                defvar_nil, prin1_to_string, prin1_string, cons_repr,
#                cons_arg).
#   predicate    a procedure named like a Lisp predicate: ending in `_p'
#                (region_active_p), or in `p' after a type word
#                (charsetp, processp, bobp).  Name it is_* or has_*.
#   constant     a named constant ending in _NIL, _T, _TRUE or _FALSE.
#                Name the states for their meaning (_NO, _YES, _ALWAYS,
#                ...); two states make a logical.
#   vector       a name (variable, component, named constant, type or
#                procedure) ending in `_vector'.  Use a plural.
#   interactive  a component `interactive' of command_type (it is
#                `is_command').
#
# Logical user options named *_p upstream keep their names: they are
# variables, which the predicate rule does not look at.
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
    check_predicate(line, name)
    if (tolower(name) ~ /_vector$/) finding(line, "vector", name, "")
}

# Whether NAME reads as a Lisp predicate (region_active_p, charsetp).
function check_predicate(line, name, n, stem) {
    n = tolower(name)
    if (n ~ /_p$/) {
        finding(line, "predicate", name, "")
        return
    }
    if (n !~ /p$/) return
    stem = substr(n, 1, length(n) - 1)
    sub(/^.*_/, "", stem)
    if (stem in typeword) finding(line, "predicate", name, "")
}

# The names a declaration statement L (lower case, without strings)
# declares: what follows `::', at parenthesis and bracket depth 0, up to
# each `=' or `=>'.
function decl_names(l, names, i, c, d, out, n, a, k, nm) {
    sub(/^[^:]*::/, "", l)
    out = ""
    d = 0
    for (i = 1; i <= length(l); i++) {
        c = substr(l, i, 1)
        if (c == "(" || c == "[") d++
        else if (c == ")" || c == "]") d--
        else if (d == 0) out = out c
    }
    n = split(out, a, ",")
    k = 0
    for (i = 1; i <= n; i++) {
        nm = trim(a[i])
        sub(/[ \t]*=.*/, "", nm)
        if (nm ~ /^[a-z_][a-z0-9_]*$/) names[++k] = nm
    }
    return k
}

# Checks the declaration L at line LINE.
function check_decl(line, l, names, n, i, nm) {
    if (l !~ /::/) return
    n = decl_names(l, names)
    for (i = 1; i <= n; i++) {
        nm = names[i]
        if (l ~ /(^|,)[ \t]*parameter([ \t]*(,|::))/ && nm ~ /_(nil|t|true|false)$/) {
            finding(line, "constant", toupper(nm), "")
        }
        if (nm ~ /_vector$/) finding(line, "vector", nm, "")
        if (intype == "command_type" && nm == "interactive") finding(line, "interactive", "command_type%interactive", "")
    }
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
    # A tag test or a component read of a value: `stringp = vtag(v) ==
    # V_STRING', `xfixnum = v%a(1)%i'.
    if (rest ~ /^(vtag[ \t]*\([a-z_][a-z0-9_]*\)|[a-z_][a-z0-9_]*%a\(1\)%tag)[ \t]*(==|\/=)[ \t]*v_[a-z0-9_]+[ \t]*$/ ||
        rest ~ /^[a-z_][a-z0-9_]*%a\(1\)%[a-z_][a-z0-9_]*[ \t]*$/) {
        finding(pline[depth], "wrapper", pname[depth], " (only a value's tag or component)")
        return
    }
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
    n = split("nil nilp t_value sym consp cons car cdr car_safe cdr_safe nth nthcdr memq member length " \
              "fixnump make_fixnum xfixnum xstring xsymbol xhandle is_integer integer_of funcall " \
              "funcall_noargs apply put get plist_get plist_put plist_member intern intern_soft setq_local " \
              "boundp fboundp makunbound keywordp defvar_nil prin1_to_string prin1_string cons_repr cons_arg", w, " ")
    for (i = 1; i <= n; i++) lisp[w[i]] = 1
    # Words that, with `p' added, name a Lisp type or position predicate.
    n = split("buffer marker string window frame process timer charset symbol keymap list cons fixnum " \
              "number integer natnum float overlay category char keyword command minibuffer function " \
              "sequence vector array atom boolean bound fbound bob eob bol eol less greater blank " \
              "graphic alphabetic alphanumeric uppercase lowercase printable syntax_table case_table " \
              "hash_table record subr", w, " ")
    for (i = 1; i <= n; i++) typeword[w[i]] = 1
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
    intype = ""
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

# Derived-type definitions (for the component rule) and declarations
# anywhere (for the constant and vector rules).
l ~ /^type([ \t]*,[^:]*)?[ \t]*::[ \t]*[a-z_][a-z0-9_]*[ \t]*$/ || l ~ /^type[ \t]+[a-z_][a-z0-9_]*[ \t]*$/ {
    intype = l
    sub(/^.*[: \t]/, "", intype)
    if (intype ~ /_vector$/) finding(startline, "vector", intype, "")
    next
}
l ~ /^end[ \t]*type/ { intype = ""; next }
l ~ /::/ && l !~ /^(use|procedure[ \t]*\(|generic)/ { check_decl(startline, l) }

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
depth > 0 && !pexec[depth] && (l ~ /^(implicit|use|import|integer|logical|real|complex|character|external|intrinsic|dimension|parameter|save|data|optional|intent|allocatable|pointer|target|value|contiguous|volatile|asynchronous|enum|enumerator)([^a-z0-9_]|$)/ || \
                                  l ~ /^(type|class|procedure)[ \t]*\(/ || l ~ /^end[ \t]*enum/) { next }

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
