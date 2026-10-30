# hazards.awk -- report the code patterns CLAUDE.md and PORTING.md name as
# hazards (gfortran bugs and Fortran pitfalls) in Fortran source files.
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

# Usage: awk -f tools/hazards.awk FILE.f90...
#
# Prints FILE:LINE: HAZARD: explanation for each hit and exits with
# status 1 if there is any.  A statement that is a justified exception
# carries `! hazard-ok(HAZARD): why' on one of its
# lines or on the comment line just above it.
#
# The hazards (PORTING.md, "Hazards", lists them with the ones only a
# reviewer can find):
#   short-circuit       `.and.'/`.or.' whose right side uses what the left
#                       side guards (allocated, associated, present, len,
#                       size, or an index compared with a bound): both
#                       sides are evaluated.
#   implied-save        an initialized declaration of a local variable
#                       (it implies `save').
#   deferred-result-pointer
#                       a procedure pointer to a function whose result is
#                       allocatable or of deferred length (gfortran frees
#                       the procedure's address).
#   allocatable-constructor
#                       a function result or structure constructor of a
#                       type with allocatable components inside an array
#                       constructor (gfortran leaks it).
#   recursive-allocatable-type
#                       a type with an allocatable component of its own
#                       type (gfortran copies it shallowly).
#   implicit-none       `implicit none' without `(type, external)'.
#   stop                `stop' outside a main program.
#   obsolete            `common', `equivalence' or `goto'.

# CODE: LINE without its comment and with the text of its strings
# removed (quotes kept); `cont': whether it continues; `quote' carries a
# string continued across lines.
function scan(line,    i, c, n, out) {
    out = ""
    if (quote != "") sub(/^[ \t]*&/, "", line)
    n = length(line)
    i = 1
    while (i <= n) {
        c = substr(line, i, 1)
        if (quote != "") {
            if (c == quote) {
                if (substr(line, i + 1, 1) == quote) {
                    i += 2
                    continue
                }
                quote = ""
                out = out c
            }
            i++
            continue
        }
        if (c == "!") break
        if (c == "\"" || c == "'") quote = c
        out = out c
        i++
    }
    sub(/[ \t]+$/, "", out)
    cont = (out ~ /&$/)
    sub(/[ \t]*&$/, "", out)
    sub(/^[ \t]*&/, "", out)
    return tolower(out)
}

function trim(s) {
    sub(/^[ \t]+/, "", s)
    sub(/[ \t]+$/, "", s)
    return s
}

function report(s, hazard, text,    k) {
    for (k = sline[s] - 1; k <= eline[s]; k++)
        if (index(tolower(raw[sfile[s], k]), "hazard-ok(" hazard ")") > 0) return
    printf "%s:%d: %s: %s\n", fname[sfile[s]], sline[s], hazard, text
    hits++
}

# The procedure name a function header declares ("" for none), and in
# `resvar' its result variable.
function function_header(code,    s, name) {
    s = code
    while (s ~ /^(pure|elemental|recursive|impure|non_recursive|module)[ \t]+/)
        sub(/^[a-z_]+[ \t]+/, "", s)
    restype = ""
    if (s ~ /^(type|class)[ \t]*\([ \t]*[a-z_][a-z0-9_]*[ \t]*\)[ \t]+/) {
        restype = s
        sub(/^(type|class)[ \t]*\([ \t]*/, "", restype)
        sub(/[ \t]*\).*/, "", restype)
        sub(/^(type|class)[ \t]*\([^)]*\)[ \t]+/, "", s)
    } else if (s ~ /^(integer|real|logical|complex|character)[ \t]*(\([^)]*\))?[ \t]+/) {
        sub(/^[a-z]+[ \t]*(\([^)]*\))?[ \t]+/, "", s)
    }
    while (s ~ /^(pure|elemental|recursive|impure|non_recursive|module)[ \t]+/)
        sub(/^[a-z_]+[ \t]+/, "", s)
    if (s !~ /^function[ \t]+[a-z_]/) return ""
    sub(/^function[ \t]+/, "", s)
    name = s
    sub(/[^a-z0-9_].*/, "", name)
    resvar = name
    if (s ~ /result[ \t]*\([ \t]*[a-z_]/) {
        resvar = s
        sub(/.*result[ \t]*\([ \t]*/, "", resvar)
        sub(/[^a-z0-9_].*/, "", resvar)
    }
    return name
}

# Whether the declaration DECL declares the entity NAME.
function declares(decl, name,    list) {
    if (decl !~ /::/) return 0
    list = decl
    sub(/^[^:]*::/, "", list)
    gsub(/\([^()]*\)/, "", list)
    gsub(/=[^,]*/, "", list)
    return ("," list ",") ~ ("[, \t]" name "[ \t]*,")
}

function is_declaration(code) {
    if (code ~ /^[a-z_][a-z0-9_]*[ \t]*(\([^=]*\))?[ \t]*(%[a-z0-9_%()]*)?[ \t]*=/ && code !~ /::/) return 0
    return code ~ /^(use|implicit|import|integer|real|double|complex|logical|character|external|intrinsic|parameter|dimension|save|data|namelist|public|private|optional|intent|pointer|target|value|volatile|contiguous|protected|generic)([^a-z0-9_]|$)/ ||
        code ~ /^(type|class|procedure)[ \t]*(\(|,|::)/
}

# The text from position P of S (just after an opening parenthesis) up
# to the matching closing parenthesis.
function balanced(s, p,    depth, c, out) {
    depth = 1
    out = ""
    while (p <= length(s)) {
        c = substr(s, p, 1)
        if (c == "(") depth++
        if (c == ")") {
            depth--
            if (depth == 0) return out
        }
        out = out c
        p++
    }
    return out
}

# Report short-circuit hazards in the statement S whose code is CODE:
# the condition of an `if' or `do while' and the statement a one-line
# `if' controls are checked apart.
function check_short_circuit(s, code,    p, cond, action) {
    if (match(code, /^(else[ \t]*)?if[ \t]*\(/) || match(code, /^do[ \t]+while[ \t]*\(/) || \
        match(code, /^[a-z_][a-z0-9_]*[ \t]*:[ \t]*do[ \t]+while[ \t]*\(/)) {
        p = RSTART + RLENGTH
        cond = balanced(code, p)
        action = substr(code, p + length(cond) + 1)
        if (check_expression(s, cond)) return
        sub(/^[ \t]*then[ \t]*$/, "", action)
        if (action ~ /\.(and|or)\./) check_short_circuit(s, trim(action))
        return
    }
    check_expression(s, code)
}

# Report a short-circuit hazard in the logical expression EXPR; returns
# whether it found one.
function check_expression(s, expr,    rest, left, right, g, name, idx, kind, p, before, arg, after) {
    rest = expr
    left = ""
    while (match(rest, /\.(and|or)\./)) {
        left = left substr(rest, 1, RSTART - 1)
        right = substr(rest, RSTART + RLENGTH)
        rest = right
        # Guards on the left: allocated(X), associated(X), present(X),
        # len(X), len_trim(X), size(X).
        g = left
        while (match(g, /(allocated|associated|present|len_trim|len|size)[ \t]*\(/)) {
            kind = substr(g, RSTART, RLENGTH)
            sub(/[ \t]*\($/, "", kind)
            p = RSTART + RLENGTH
            before = substr(g, 1, RSTART - 1)
            arg = balanced(g, p)
            after = substr(g, p + length(arg) + 1)
            name = trim(balanced(g, p))
            sub(/[ \t]*,.*/, "", name)
            g = substr(g, p)
            if (name == "") continue
            if (kind !~ /^(allocated|associated|present)$/ && after !~ /^[ \t]*(==|\/=|<=|>=|<|>)/ && \
                before !~ /(==|\/=|<=|>=|<|>)[ \t]*$/) continue
            if (kind == "allocated" || kind == "associated" || kind == "present") {
                if (uses(right, name, 0)) {
                    report(s, "short-circuit", kind "(" name ") does not guard the use of " name " after .and./.or.")
                    return 1
                }
            } else if (uses(right, name, 1)) {
                report(s, "short-circuit", kind "(" name ") does not guard " name "(...) after .and./.or.")
                return 1
            }
        }
        # An index compared with a bound on the left, used as a
        # subscript on the right.
        g = left
        while (match(g, /[a-z_][a-z0-9_]*[ \t]*(<=|>=|<|>)/)) {
            idx = substr(g, RSTART, RLENGTH)
            g = substr(g, RSTART + RLENGTH)
            sub(/[ \t]*(<=|>=|<|>)$/, "", idx)
            if (right ~ ("(^|[^a-z0-9_%])[a-z_][a-z0-9_%]*[ \t]*\\([ \t]*" idx "[ \t]*[-+:,)]")) {
                if (subscripted(right, idx)) {
                    report(s, "short-circuit", "the bound check of " idx " does not guard its use as a subscript after .and./.or.")
                    return 1
                }
            }
        }
        left = left " .op. "
    }
    return 0
}

# Whether RIGHT uses IDX as the first subscript of something that is not
# an intrinsic function.
function subscripted(right, idx,    t, m, name) {
    t = right
    while (match(t, ("[a-z_][a-z0-9_%]*[ \t]*\\([ \t]*" idx "[ \t]*[-+:,)]"))) {
        m = substr(t, RSTART, RLENGTH)
        t = substr(t, RSTART + RLENGTH)
        name = m
        sub(/[ \t]*\(.*/, "", name)
        sub(/.*%/, "", name)
        if (name ~ /^(achar|char|iachar|ichar|abs|mod|modulo|min|max|int|nint|real|dble|present|allocated|associated|len|len_trim|size|trim|adjustl|adjustr|repeat|index|scan|verify|btest|iand|ior|ieor|ishft|merge|huge|kind|sign|any|all|count|sum|product|maxval|minval|lbound|ubound|shape|transfer|itoa|make_fixnum)$/) continue
        return 1
    }
    return 0
}

# Whether TEXT uses NAME (followed by `(' if SUBSCRIPT), outside the
# guards allocated(), associated(), present(), len() and size().
function uses(text, name, subscript,    re, q, t) {
    q = name
    gsub(/[][\\.*+?^${}()|%]/, "\\\\&", q)
    t = text
    gsub(/(allocated|associated|present|len_trim|len|size)[ \t]*\([^()]*(\([^()]*\)[^()]*)*\)/, " ", t)
    if (subscript) re = "(^|[^a-z0-9_%])" q "[ \t]*\\("
    else re = "(^|[^a-z0-9_%])" q "([^a-z0-9_]|$)"
    return t ~ re
}

# Add the entities the declaration CODE declares to VARS (as keys).
function add_declared(code, vars,    list, n, k, items) {
    if (code !~ /::/) return
    list = code
    sub(/^[^:]*::/, "", list)
    while (list ~ /\([^()]*\)/) gsub(/\([^()]*\)/, "", list)
    gsub(/=>?[^,]*/, "", list)
    n = split(list, items, ",")
    for (k = 1; k <= n; k++) vars[trim(items[k])] = 1
}

# Add the dummy arguments of the procedure header CODE to VARS.
function add_dummies(code, vars,    list, n, k, items) {
    if (!match(code, /\([^()]*\)/)) return
    list = substr(code, RSTART + 1, RLENGTH - 2)
    n = split(list, items, ",")
    for (k = 1; k <= n; k++) vars[trim(items[k])] = 1
    if (match(code, /result[ \t]*\([ \t]*[a-z_][a-z0-9_]*/)) {
        list = substr(code, RSTART, RLENGTH)
        sub(/.*\([ \t]*/, "", list)
        vars[list] = 1
    }
}

# Report array constructors holding results or constructors of types
# with allocatable components.
function check_constructor(s, code,    rest, inner, depth, i, c, start, call, name) {
    rest = code
    while ((start = index(rest, "[")) > 0) {
        depth = 0
        inner = ""
        for (i = start; i <= length(rest); i++) {
            c = substr(rest, i, 1)
            if (c == "[") depth++
            if (c == "]") depth--
            if (depth == 0) break
            inner = inner c
        }
        rest = substr(rest, i + 1)
        inner = substr(inner, 2)
        if (inner ~ /^[ \t]*(character|integer|real|logical|complex)[ \t(:]/) continue
        sub(/^[^:\[]*::/, "", inner)
        while (match(inner, /[a-z_][a-z0-9_]*[ \t]*\(/)) {
            call = substr(inner, RSTART, RLENGTH)
            inner = substr(inner, RSTART + RLENGTH)
            name = call
            sub(/[ \t]*\($/, "", name)
            if (name in localvar || name in modvar) continue
            if ((name in fres && hasalloc[fres[name]]) || hasalloc[name]) {
                report(s, "allocatable-constructor", name "(...) inside an array constructor: its type has allocatable components")
                return
            }
        }
    }
}

{
    if (FNR == 1) {
        nf++
        fname[nf] = FILENAME
        ismain[nf] = 0
    }
    raw[nf, FNR] = $0
    nlines[nf] = FNR
}

END {
    hits = 0
    # Statements: code with its continuation lines joined.
    ns = 0
    for (f = 1; f <= nf; f++) {
        quote = ""
        i = 1
        while (i <= nlines[f]) {
            line = raw[f, i]
            if (line ~ /^[ \t]*$/ || line ~ /^[ \t]*!/) {
                i++
                continue
            }
            ns++
            sfile[ns] = f
            sline[ns] = i
            code = scan(line)
            while ((cont || quote != "") && i < nlines[f]) {
                i++
                if (raw[f, i] ~ /^[ \t]*!/ || raw[f, i] ~ /^[ \t]*$/) continue
                code = code " " scan(raw[f, i])
            }
            quote = ""
            eline[ns] = i
            scode[ns] = trim(code)
            if (scode[ns] ~ /^program[ \t]/) ismain[f] = 1
            i++
        }
    }

    # Pass 1: types with allocatable components, and function result
    # types and kinds.
    intype = ""
    for (s = 1; s <= ns; s++) {
        code = scode[s]
        if (intype != "") {
            if (code ~ /^end[ \t]*type/) {
                intype = ""
                continue
            }
            if (code == "contains") {
                intypeprocs = 1
                continue
            }
            if (intypeprocs) continue
            if (code ~ /allocatable/) hasalloc[intype] = 1
            if (code ~ /^(type|class)[ \t]*\(/ && code !~ /pointer/) {
                u = code
                sub(/^(type|class)[ \t]*\([ \t]*/, "", u)
                sub(/[ \t]*\).*/, "", u)
                dep[intype] = dep[intype] " " u
                if (u == intype && code ~ /allocatable/) report(s, "recursive-allocatable-type", "type " intype " has an allocatable component of its own type")
            }
            continue
        }
        if (code ~ /^type[ \t]*(,[^:]*)?::[ \t]*[a-z_]/ || (code ~ /^type[ \t]+[a-z_]/ && code !~ /^type[ \t]+is[ \t]*\(/)) {
            intype = code
            sub(/.*::[ \t]*/, "", intype)
            sub(/^type[ \t]+/, "", intype)
            sub(/[^a-z0-9_].*/, "", intype)
            intypeprocs = 0
            continue
        }
        name = function_header(code)
        if (name != "") {
            curfn = name
            currv = resvar
            if (restype != "") fres[name] = restype
            continue
        }
        if (curfn != "" && is_declaration(code)) {
            if (declares(code, currv)) {
                if (code ~ /^(type|class)[ \t]*\(/) {
                    u = code
                    sub(/^(type|class)[ \t]*\([ \t]*/, "", u)
                    sub(/[ \t]*\).*/, "", u)
                    fres[curfn] = u
                }
                if (code ~ /allocatable/ || code ~ /len[ \t]*=[ \t]*:/) fdeferred[curfn] = 1
            }
        } else if (curfn != "" && code !~ /^(abstract[ \t]+)?interface/) {
            curfn = ""
        }
    }
    # Types holding such types have allocatable components too.
    changed = 1
    while (changed) {
        changed = 0
        for (t in dep) {
            if (hasalloc[t]) continue
            n = split(dep[t], us, " ")
            for (k = 1; k <= n; k++) {
                if (hasalloc[us[k]]) {
                    hasalloc[t] = 1
                    changed = 1
                    break
                }
            }
        }
    }

    # Pass 2: the checks.  `state': "spec" outside procedure bodies,
    # "decl" in a procedure's declarations, "exec" in its body.
    for (s = 1; s <= ns; s++) {
        code = scode[s]
        f = sfile[s]
        if (s == 1 || sfile[s - 1] != f) {
            state = "spec"
            delete modvar
            delete localvar
            ininterface = 0
            intype = 0
        }
        if (code ~ /^(abstract[ \t]+)?interface([^a-z0-9_]|$)/) ininterface++
        if (code ~ /^end[ \t]*interface/) ininterface--
        if (code ~ /^type[ \t]*(,[^:]*)?::/ || (code ~ /^type[ \t]+[a-z_]/ && code !~ /^type[ \t]+is[ \t]*\(/)) intype = 1
        if (code ~ /^end[ \t]*type/) intype = 0

        if (code ~ /^implicit[ \t]+none/ && code !~ /^implicit[ \t]+none[ \t]*\([ \t]*type[ \t]*,[ \t]*external[ \t]*\)/)
            report(s, "implicit-none", "use implicit none (type, external)")
        if (code ~ /^(error[ \t]+)?stop([^a-z0-9_]|$)/ && code !~ /^stop[ \t]*(=|%|\()/ && !ismain[f])
            report(s, "stop", "stop outside a main program: return a status instead")
        if (code ~ /^(common|equivalence)([^a-z0-9_]|$)/ || code ~ /^go[ \t]*to([^a-z0-9_]|$)/ || code ~ /[^a-z0-9_]go[ \t]*to[ \t]+[0-9]/)
            report(s, "obsolete", "common, equivalence and goto are not used")
        if (code ~ /procedure[ \t]*\([ \t]*[a-z_][a-z0-9_]*[ \t]*\)/ && code ~ /pointer/) {
            name = code
            sub(/.*procedure[ \t]*\([ \t]*/, "", name)
            sub(/[^a-z0-9_].*/, "", name)
            if (fdeferred[name])
                report(s, "deferred-result-pointer", "procedure pointer to " name ", a function with an allocatable or deferred-length result: make it a subroutine")
        }

        if (!ininterface && !intype) {
            if (function_header(code) != "" || code ~ /^(pure[ \t]+|elemental[ \t]+|recursive[ \t]+|impure[ \t]+|module[ \t]+)*subroutine[ \t]+[a-z_]/ || code ~ /^module[ \t]+procedure[ \t]+[a-z_][a-z0-9_]*$/ || code ~ /^program[ \t]/) {
                state = "decl"
                delete localvar
                add_dummies(code, localvar)
                continue
            }
            if (code ~ /^end[ \t]*(subroutine|function|procedure|program)([^a-z0-9_]|$)/ || code == "contains") {
                state = "spec"
                continue
            }
            if (state == "decl" && !is_declaration(code)) state = "exec"
            if (state == "decl") add_declared(code, localvar)
            if (state == "spec" && is_declaration(code)) add_declared(code, modvar)
            if (state == "decl" && code ~ /::/ && code !~ /parameter/) {
                d = code
                sub(/^[^:]*::/, "", d)
                gsub(/\([^()]*\)/, "", d)
                if (d ~ /[^=<>\/]=[^=]/ || d ~ /=>/)
                    report(s, "implied-save", "an initialized local declaration implies save: assign it in the body")
            }
        }

        if (code ~ /\.(and|or)\./) check_short_circuit(s, code)
        if (index(code, "[") > 0) check_constructor(s, code)
    }
    exit hits > 0
}
