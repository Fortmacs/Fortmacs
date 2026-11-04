#!/bin/sh
# gen-docstrings.sh -- generate src/docstrings_mod.f90, the documentation
# that the help commands (C-h f, C-h k, C-h v) show.
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

# Upstream collects docstrings into etc/DOC at build time
# (lib-src/make-docfile.c) and Lisp keeps the rest with each definition.
# Here a docstring is the !> comment on a procedure or variable, up to
# its "Port of" citation (the rest of the comment is notes on the port).
# This script collects:
#
#   - commands: every `commands%NAME%execute => PROC` in src/.  The
#     docstring is PROC's, or else that of the procedure whose comment
#     cites NAME ("Port of simple.el:NAME" or "Port of cmds.c:FNAME");
#   - functions: the procedures src/fortmacs.f90 exports (the config
#     API), named by their citation or else by their Fortran name with
#     hyphens, and the private procedures behind an `interface
#     operator(OP)', with the operator form as their call line;
#   - variables: the variables src/fortmacs.f90 exports, named by their
#     citation ("Port of files.el make-backup-files") or likewise, and
#     every other variable with such a citation.
#
# Usage: tools/gen-docstrings.sh [OUTPUT]
#   The default output is src/docstrings_mod.f90.  It is formatted with
#   fprettify, as tools/check.sh requires.

set -eu

cd "$(dirname "$0")/.."
out=${1:-src/docstrings_mod.f90}
tmp=${TMPDIR:-/tmp}/gen-docstrings.$$
trap 'rm -f "$tmp".*' EXIT

# Pass 1: one record per entry, fields separated by \037 and docstring
# lines by \036:
#   F  help-name  file  fortran-call  command(0/1)  declarations  doc
# (declarations: the dummy arguments' and the result's declaration lines,
# separated by \036, for the declaration block C-h f shows)
#   V  help-name  file  fortran-name  type  array(0/1)  init  target-file  doc
# (init: the initial value; for a procedure pointer the target of its
# `=>`, and target-file the file whose module exports that procedure, ""
# when it is private.)
# (The program is in a file, to be run with tools/fortran-names-lib.awk,
# which gives the cited upstream names their Fortran names.)
cat >"$tmp.pass1" <<'EOF'
BEGIN { load_names("tools/fortran-names") }
function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
function help_name(s) { return s }
# A declaration line with deferred lengths (len=:) written as len=*, so
# the "::" that separates the names is the first colon.
function declline(s) { gsub(/len=:/, "len=*", s); return s }

# The docstring of comment block b (lines joined by \036): the text
# before "Port of" or "Not a port".
function docstring(b,    n, i, lines, out, k, line) {
    n = split(b, lines, "\036")
    out = ""
    for (i = 1; i <= n; i++) {
        line = lines[i]
        k = index(line, "Port of")
        if (k == 0) k = index(line, "Not a port")
        # "Port" at the end of a line, "of" starting the next.
        if (k == 0 && i < n && match(line, /(^|[ \t])Port$/) && lines[i + 1] ~ /^of /)
            k = length(line) - 3
        if (k > 0) {
            line = substr(line, 1, k - 1)
            sub(/[ \t]+$/, "", line)
            if (line != "") out = out (out == "" ? "" : "\036") line
            break
        }
        out = out (i == 1 ? "" : "\036") line
    }
    # Drop trailing empty lines.
    while (out ~ /\036$/) sub(/\036$/, "", out)
    return out
}

# The upstream name cited in block b: "Port of FILE:NAME" for a function
# ("" for an internal C function), "Port of FILE NAME" for a variable.
function citation(b, var,    flat, k, rest, file, name) {
    flat = b
    gsub(/\036/, " ", flat)
    gsub(/  +/, " ", flat)
    k = index(flat, "Port of ")
    if (k == 0) return ""
    rest = substr(flat, k + 8)
    if (!match(rest, /^[A-Za-z0-9_\/.-]+\.(c|el|h)/)) return ""
    file = substr(rest, 1, RLENGTH)
    rest = substr(rest, RLENGTH + 1)
    if (var) {
        if (!match(rest, /^ [a-z][A-Za-z0-9*+-]*/)) return ""
        # (lower case: dired-ls-F-marks-symlinks is dired_ls_f_marks_symlinks)
        name = tolower(substr(rest, 2, RLENGTH - 1))
        # A C variable (point_before_last_command_or_undo), not a DEFVAR.
        if (substr(rest, RLENGTH + 1, 1) == "_") return ""
        sub(/-$/, "", name)
        gsub(/-/, "_", name)
        name = rename(name)
        return name
    }
    if (!match(rest, /^:[A-Za-z0-9_*+<>=\/-]+/)) return ""
    name = substr(rest, 2, RLENGTH - 1)
    sub(/-$/, "", name)
    if (file ~ /\.(c|h)$/) {
        if (name !~ /^F[a-z]/) return ""
        name = substr(name, 2)
    }
    # Fortmacs names use underscores where Emacs uses hyphens, and Fortran
    # words for Lisp terms (tools/fortran-names).
    gsub(/-/, "_", name)
    return rename(name)
}

FNR == 1 { block = ""; sig = ""; insig = 0; indecl = 0; base = FILENAME; sub(/.*\//, "", base) }
# Submodules (their variables are not accessible) and the names in
# `public ::` statements.
/^submodule/ { submod[base] = 1 }
# The startup value of an array component of a setting, assigned in its
# init_ routine while still unallocated (`if (.not. allocated(x%commands))`
# then `x%commands = [...]`): help shows it in the original value, in
# the form the setting's other_value gives (`%commands = ["a", "b"]`).
startvar != "" {
    l = $0
    if (match(l, "^[ \t]*" startvar "[ \t]*=[ \t]*\\[")) {
        while (l ~ /&[ \t]*$/ && (getline nextl) > 0) {
            sub(/[ \t]*&[ \t]*$/, " ", l)
            sub(/^[ \t]*&?[ \t]*/, "", nextl)
            l = l nextl
        }
        sub(/^[^[]*\[/, "", l)
        sub(/^[^:"]*::/, "", l)
        items = ""
        while (match(l, /"[^"]*"/)) {
            items = items (items == "" ? "" : ", ") substr(l, RSTART, RLENGTH)
            l = substr(l, RSTART + RLENGTH)
        }
        split(startvar, sv, "%")
        startdef[sv[1] SUBSEP base] = "%" sv[2] " = [" items "]"
    }
    startvar = ""
}
/^[ \t]*if[ \t]*\([ \t]*\.not\.[ \t]*allocated[ \t]*\([ \t]*[a-z0-9_]+%[a-z0-9_]+[ \t]*\)[ \t]*\)[ \t]*then/ {
    startvar = $0
    sub(/.*allocated[ \t]*\([ \t]*/, "", startvar)
    sub(/[ \t]*\).*/, "", startvar)
}
/^[ \t]*public[ \t]*::/ { inpub = 1; l = $0; sub(/^[^:]*::/, "", l) }
inpub {
    if (!(/^[ \t]*public[ \t]*::/)) l = $0
    cont = (l ~ /&[ \t]*$/); sub(/&[ \t]*$/, "", l)
    np = split(l, pn, ",")
    for (ip = 1; ip <= np; ip++) { nm = trim(pn[ip]); if (nm != "") pubname[nm SUBSEP base] = 1 }
    if (!cont) inpub = 0
}

# Continuation of a procedure header: collect the argument list.
insig {
    line = $0
    sub(/^[ \t]+/, "", line)
    sig = sig line
    if (sig ~ /&[ \t]*$/) { sub(/&[ \t]*$/, "", sig); next }
    insig = 0
    finish_sig()
    next
}

# The declarations after a procedure header: those of its dummy arguments
# and result go into the declaration block of C-h f (B6), in argument
# order; the first executable statement (or contains, or end) ends them.
indecl { collect_decl() }

/^[ \t]*!>/ {
    line = $0
    sub(/^[ \t]*!>[ ]?/, "", line)
    sub(/[ \t]+$/, "", line)
    block = block (block == "" ? "" : "\036") line
    next
}

/^[ \t]*abstract interface/ { abstract = 1; block = ""; next }
/^[ \t]*end interface/ { abstract = 0; ingeneric = ""; inop = ""; block = ""; next }

# An operator interface (`interface operator(<=)'): its specific
# procedures are private, the operator their only spelling, so C-h f
# shows each under its own help name with the operator as its call line
# (`v1 <= v2'; see `operator_call').
/^[ \t]*interface[ \t]+operator[ \t]*\(/ {
    inop = $0
    sub(/^[^(]*\([ \t]*/, "", inop)
    sub(/[ \t]*\).*$/, "", inop)
    block = ""
    next
}
inop != "" && /^[ \t]*module procedure/ {
    line = tolower($0)
    sub(/^[ \t]*module procedure[ \t]*/, "", line)
    gsub(/[ \t]/, "", line)
    n = split(line, opnames, ",")
    for (i = 1; i <= n; i++) if (opnames[i] != "") opof[opnames[i] SUBSEP base] = inop
    next
}

# A named generic interface: documented like a procedure, called like
# its first specific procedure.
/^[ \t]*interface [a-zA-Z0-9_]+[ \t]*$/ {
    gname = tolower($2)
    if (block != "") {
        key = gname SUBSEP base
        pdoc[key] = docstring(block)
        c = citation(block, 0)
        pcite[key] = c
        if (c != "" && !(c in cited)) cited[c] = key
        generic[key] = 1
        ingeneric = key
    }
    block = ""
    next
}
ingeneric != "" && /^[ \t]*module procedure/ {
    line = tolower($0)
    sub(/^[ \t]*module procedure[ \t]*/, "", line)
    sub(/[ \t,].*/, "", line)
    if (!(ingeneric in gfirst)) gfirst[ingeneric] = line
    next
}

# A separate module procedure in a submodule (`module procedure NAME`):
# its comment documents the procedure its parent declares.
ingeneric == "" && /^[ \t]*module procedure[ \t]+[a-zA-Z0-9_]+[ \t]*$/ {
    if (block != "") {
        pname = tolower($3)
        key = pname SUBSEP base
        if (!(key in pdoc)) {
            pdoc[key] = docstring(block)
            c = citation(block, 0)
            pcite[key] = c
            if (c != "" && !(c in cited)) cited[c] = key
        }
        if (!(pname in anydoc) || anydoc[pname] == "") anydoc[pname] = pdoc[key]
    }
    block = ""
    next
}

# Built-in commands (not in comments): `commands%NAME%execute => PROC`, and
# `commands%NAME%is_command = .false.` for a function that is not a
# command.
/^[ \t]*commands%[a-z0-9_]+%execute[ \t]*=>/ {
    line = $0
    sub(/^[ \t]*commands%/, "", line)
    name = line; sub(/%.*/, "", name); name = help_name(name)
    proc = line; sub(/^[^>]*>[ \t]*/, "", proc); sub(/[^a-z0-9_].*/, "", proc)
    if (!(name in cmd)) { cmd[name] = proc; cmdfile[name] = base }
}
/^[ \t]*commands%[a-z0-9_]+%is_command[ \t]*=[ \t]*\.false\./ {
    line = $0
    sub(/^[ \t]*commands%/, "", line)
    name = line; sub(/%.*/, "", name)
    noncommand[help_name(name)] = 1
}

{
    line = tolower($0)
    if (!abstract && match(line, /^[ \t]*((pure|elemental|impure|recursive|module|non_recursive)[ \t]+|(integer|logical|real|character|type)(\([^)]*\))?[ \t]+)*(subroutine|function)[ \t]+[a-z0-9_]+/)) {
        head = substr(line, 1, RLENGTH)
        kind = (head ~ /subroutine[ \t]+[a-z0-9_]+$/) ? "subroutine" : "function"
        # A function's result type written in its header.
        ptype = head
        sub(/[ \t]*(subroutine|function)[ \t]+[a-z0-9_]+$/, "", ptype)
        gsub(/(pure|elemental|impure|recursive|module|non_recursive)[ \t]+/, "", ptype)
        ptype = trim(ptype)
        pname = head; sub(/.*[ \t]/, "", pname)
        pblock = block
        sig = trim(substr($0, RLENGTH + 1))
        sig = pname sig
        if (sig ~ /&[ \t]*$/) { sub(/&[ \t]*$/, "", sig); insig = 1; block = ""; next }
        finish_sig()
    } else if (block != "" && line !~ /parameter/ && match(declline(line), /^[ \t]*(integer|logical|character|real|type[ \t]*\(|procedure[ \t]*\()[^:!]*::[ \t]*[a-z0-9_]+/)) {
        decl = substr(declline(line), 1, RLENGTH)
        vname = decl; sub(/.*[ \t:]/, "", vname)
        type = decl; sub(/[ \t]*::.*/, "", type); sub(/^[ \t]+/, "", type)
        rest = substr($0, RLENGTH + 1)
        # A declaration continued over several lines is joined into one.
        while (rest ~ /&[ \t]*$/ && (getline nextl) > 0) {
            sub(/[ \t]*&[ \t]*$/, " ", rest)
            sub(/^[ \t]*&?[ \t]*/, "", nextl)
            rest = rest nextl
        }
        is_array = (rest ~ /^\(/) ? 1 : 0
        init = ""
        if (!is_array && match(rest, /^[ \t]*=[ \t]*/)) {
            init = substr(rest, RLENGTH + 1)
            sub(/[ \t]*(,|&|!).*$/, "", init)
            if (init ~ /^"/ || init ~ "^\047") {
                init = substr(rest, RLENGTH + 1)
                sub(/[ \t]*$/, "", init)
            }
        }
        # A procedure pointer: its initial value is the target of its `=>`
        # (null(), or the name of its default procedure).
        if (type ~ /^procedure/) {
            init = ""
            if (match(rest, /^[ \t]*=>[ \t]*/)) {
                init = substr(rest, RLENGTH + 1)
                sub(/[ \t]*(,|&|!).*$/, "", init)
                sub(/[ \t]+$/, "", init)
            }
        } else if (type ~ /_setting_type\)/) {
            # A setting object (data_mod): its initial value is the value=
            # argument of its constructor.
            init = ""
            if (match(rest, /value[ \t]*=[ \t]*[^,)]+/)) {
                init = substr(rest, RSTART, RLENGTH)
                sub(/value[ \t]*=[ \t]*/, "", init)
            }
            # A choice setting: its value= argument (.false. when none), or
            # its choice= argument; a setting with a component of another
            # kind set in its constructor (`lines=1000') shows that.
            if (type ~ /choice_setting_type\)/ && init == "") init = ".false."
            if (type ~ /choice_setting_type\)/ && match(rest, /choice[ \t]*=[ \t]*[^,)]+/)) {
                init = substr(rest, RSTART, RLENGTH)
                sub(/choice[ \t]*=[ \t]*/, "", init)
                sub(/[ \t]+$/, "", init)
            }
            if (match(rest, /(^|[(, ])(lines|seconds|level|char|characters|columns|modifications)[ \t]*=[ \t]*[^,)]+/)) {
                init = substr(rest, RSTART, RLENGTH)
                sub(/^[(, ]/, "", init)
                sub(/[ \t]*=[ \t]*/, " = ", init)
                init = "%" init
            }
        } else if (type ~ /^type/) {
            # Other derived types have no printed value to compare.
            init = ""
        }
        key = vname SUBSEP base
        vdoc[key] = docstring(block)
        vcite[key] = citation(block, 1)
        vtype[key] = type
        vpub[key] = (type ~ /public/ || (key in pubname)) && !(base in submod)
        varr[key] = is_array
        vinit[key] = init
    }
    block = ""
}

# The dummy argument names of signature S, and the result name RES, as
# the declarations to collect for KEY.
function start_decl(key, s, res,    a, n, i, nm) {
    for (i in dpos) delete dpos[i]
    for (i in dline) delete dline[i]
    dn = 0
    a = s
    if (!sub(/^[^(]*\(/, "", a)) a = ""
    sub(/\).*$/, "", a)
    n = split(a, parts, ",")
    for (i = 1; i <= n; i++) {
        nm = tolower(trim(parts[i]))
        if (nm != "") dpos[nm] = ++dn
    }
    if (res != "") dpos[tolower(res)] = ++dn
    if (res != "" && ptype != "") dline[dn] = ptype " :: " res
    dkey = key
    indecl = 1
}

# One line inside the declaration part (see `indecl').
function collect_decl(    l, attrs, names, out, i, c, d, cur, n, nm, base0, k, j, joined) {
    l = $0
    sub(/[ \t]*![^"']*$/, "", l)
    while (l ~ /&[ \t]*$/ && (getline nextl) > 0) {
        sub(/[ \t]*&[ \t]*$/, " ", l)
        sub(/^[ \t]*&?[ \t]*/, "", nextl)
        sub(/[ \t]*![^"']*$/, "", nextl)
        l = l nextl
    }
    if (l ~ /^[ \t]*$/ || l ~ /^[ \t]*!/ || tolower(l) ~ /^[ \t]*(use|implicit|import)([ \t,]|$)/) return
    if (l !~ /::/ || tolower(l) !~ /^[ \t]*(integer|logical|real|complex|character|type[ \t]*\(|class[ \t]*\(|procedure[ \t]*\()/) {
        joined = ""
        for (k = 1; k <= dn; k++) if (k in dline) joined = joined (joined == "" ? "" : "\036") dline[k]
        if (!(dkey in pdecl)) pdecl[dkey] = joined
        split(dkey, kp, SUBSEP)
        if (!(kp[1] in anydecl)) anydecl[kp[1]] = joined
        indecl = 0
        return
    }
    attrs = l
    sub(/[ \t]*::.*/, "", attrs)
    sub(/^[ \t]+/, "", attrs)
    gsub(/[ \t]*,[ \t]*/, ", ", attrs)
    names = l
    sub(/^[^:]*::[ \t]*/, "", names)
    # The names at parenthesis depth 0, each with its array spec.
    cur = ""
    d = 0
    n = 0
    for (i = 1; i <= length(names); i++) {
        c = substr(names, i, 1)
        if (c == "(" || c == "[") d++
        if (c == ")" || c == "]") d--
        if (c == "," && d == 0) { item[++n] = cur; cur = ""; continue }
        cur = cur c
    }
    item[++n] = cur
    for (j = 1; j <= n; j++) {
        nm = trim(item[j])
        sub(/[ \t]*=.*$/, "", nm)
        base0 = tolower(nm)
        sub(/[ \t]*\(.*$/, "", base0)
        if (base0 in dpos) dline[dpos[base0]] = attrs " :: " nm
    }
}

function finish_sig(    s, key, c, res) {
    s = sig
    res = ""
    if (kind == "function") {
        res = pname
        if (match(tolower(s), /result[ \t]*\([ \t]*[a-z0-9_]+/)) {
            res = substr(s, RSTART, RLENGTH)
            sub(/.*\([ \t]*/, "", res)
        }
    }
    sub(/[ \t]*result[ \t]*\(.*$/, "", s)
    sub(/[ \t]*bind[ \t]*\(.*$/, "", s)
    sub(/[ \t]*!.*$/, "", s)
    gsub(/[ \t]*,[ \t]*/, ", ", s)
    key = pname SUBSEP base
    pdef[key] = 1
    start_decl(key, s, res)
    if (!(pname in anycall)) anycall[pname] = (kind == "subroutine" ? "call " : "") s
    if (pblock == "") return
    if (!(key in pdoc)) {
        pdoc[key] = docstring(pblock)
        pcall[key] = (kind == "subroutine" ? "call " : "") s
        c = citation(pblock, 0)
        pcite[key] = c
        if (c != "" && !(c in cited)) { cited[c] = key }
    }
    # Any file: for commands registered with a procedure from elsewhere.
    if (!(pname in anydoc)) anydoc[pname] = pdoc[key]
}

# The call line of a procedure behind operator OP, from its Fortran call
# line CALL (`version_less(v1, v2)'): `v1 < v2', or `OP a' for a unary
# operator.
function operator_call(op, call,    a, n, args) {
    a = call
    sub(/^[^(]*\(/, "", a)
    sub(/\)[^)]*$/, "", a)
    n = split(a, args, /[ \t]*,[ \t]*/)
    if (n == 1) return op " " trim(args[1])
    return trim(args[1]) " " op " " trim(args[2])
}

# The module (its file) from which docstrings_mod can import TARGET, the
# default procedure of a procedure pointer declared in FILE: FILE itself
# when its `public ::` lists the target, else the file that defines the
# target and makes it public; "" when the target is private (the value
# then cannot be compared with it).
function target_module(target, file,    k, best) {
    if (target == "" || target == "null()") return ""
    if ((target SUBSEP file) in pubname) return file
    best = ""
    for (k in pubname) {
        split(k, tp, SUBSEP)
        if (tp[1] == target && (k in pdef) && (best == "" || tp[2] < best)) best = tp[2]
    }
    return best
}

# Queue the variable record of KEY under help name HNAME; the records are
# printed at the end, so that two variables documented under one name
# (two ports of one upstream variable) each get their own Fortran name.
function queue_variable(hname, file, fort, key) {
    nq++; qh[nq] = hname; qf[nq] = file; qn[nq] = fort; qk[nq] = key
    hcount[hname]++
}

function print_variables(    i, hname, key, target) {
    for (i = 1; i <= nq; i++) {
        hname = qh[i]
        if (hcount[hname] > 1 && hname != qn[i]) hname = qn[i]
        key = qk[i]
        # A named state with a startup list (see startdef).
        if ((key in startdef) && vtype[key] ~ /choice_setting_type\)/ && vinit[key] !~ /^(\.true\.|\.false\.|)$/)
            vinit[key] = vinit[key] ", " startdef[key]
        target = (vtype[key] ~ /^procedure/) ? target_module(vinit[key], qf[i]) : ""
        printf "V\037%s\037%s\037%s\037%s\037%d\037%s\037%s\037%s\n", hname, qf[i], qn[i], vtype[key], \
            varr[key], vinit[key], target, vdoc[key]
    }
}

END {
    for (key in generic) {
        split(key, kp, SUBSEP)
        pcall[key] = (gfirst[key] in anycall) ? anycall[gfirst[key]] : ""
        sub(gfirst[key] "\\(", kp[1] "(", pcall[key])
        pdecl[key] = (gfirst[key] in anydecl) ? anydecl[gfirst[key]] : ""
    }

    # The config API: names src/fortmacs.f90 takes from each module.
    file = "src/fortmacs.f90"
    inuse = 0
    while ((getline l < file) > 0) {
        if (l ~ /^[ \t]*implicit none/) break
        if (l ~ /^[ \t]*use [a-z0-9_]+, only:/) {
            mod = l; sub(/^[ \t]*use /, "", mod); sub(/,.*/, "", mod)
            sub(/^[^:]*only:/, "", l)
            inuse = 1
        }
        if (!inuse) continue
        cont = (l ~ /&[ \t]*$/)
        sub(/&[ \t]*$/, "", l)
        n = split(l, names, ",")
        for (i = 1; i <= n; i++) {
            nm = trim(names[i])
            if (nm != "") api[nm] = mod ".f90"
        }
        if (!cont) inuse = 0
    }

    # Functions: the API procedures, then the commands.
    for (nm in api) {
        key = nm SUBSEP api[nm]
        if (key in pdoc) {
            hname = pcite[key]
            if (hname == "") hname = help_name(nm)
            # Several API procedures can port one function (capitalize_char
            # and capitalize_string): the first by name, whatever order
            # this awk visits the array in.
            if ((hname in fowner) && fowner[hname] < nm) continue
            fowner[hname] = nm
            fdoc[hname] = pdoc[key]; ffile[hname] = api[nm]; fcall[hname] = pcall[key]; fdecl[hname] = pdecl[key]
            fcommand[hname] = 0
        } else if (key in vdoc) {
            hname = vcite[key]
            if (hname == "") hname = help_name(nm)
            queue_variable(hname, api[nm], nm, key)
        }
    }
    # The procedures behind operators, under their citation (when it is
    # a name: version<= is not) or else their own name.
    for (key in opof) {
        if (!(key in pdoc) || pdoc[key] == "") continue
        split(key, kp, SUBSEP)
        hname = pcite[key]
        if (hname !~ /^[a-z][a-z0-9_]*$/) hname = help_name(kp[1])
        if (hname in fdoc) continue
        fdoc[hname] = pdoc[key]; ffile[hname] = kp[2]; fcall[hname] = operator_call(opof[key], pcall[key])
        fdecl[hname] = pdecl[key]; fcommand[hname] = 0
    }
    # Every other variable that cites the upstream variable it ports
    # (upstream documents every DEFVAR and defvar, not only the
    # settings a configuration uses).
    for (key in vdoc) {
        if (vcite[key] == "") continue
        if (!vpub[key] && !(key in pubname)) continue
        split(key, kp, SUBSEP)
        if (kp[2] in submod) continue
        if ((kp[1] in api) && api[kp[1]] == kp[2]) continue
        queue_variable(vcite[key], kp[2], kp[1], key)
    }
    print_variables()
    for (name in cmd) {
        proc = cmd[name]
        # The docstring of the command itself, else of the API function, else
        # of the procedure citing it.
        d = (proc SUBSEP cmdfile[name] in pdoc) ? pdoc[proc SUBSEP cmdfile[name]] : anydoc[proc]
        if (d == "" && (name in fdoc)) d = fdoc[name]
        if (d == "" && (name in cited)) d = pdoc[cited[name]]
        fdoc[name] = d
        if (!(name in ffile)) { ffile[name] = cmdfile[name]; fcall[name] = ""; fdecl[name] = "" }
        fcommand[name] = (name in noncommand) ? 0 : 1
    }
    # A command without a docstring of its own that runs the procedure of
    # another command is an alias of it (join_line of delete_indentation):
    # it gets the docstring of that command, the first such name in order
    # whatever order this awk visits the array in.
    for (name in cmd) {
        if (fdoc[name] != "") continue
        best = ""
        for (other in cmd)
            if (other != name && cmd[other] == cmd[name] && fdoc[other] != "" && (best == "" || other < best))
                best = other
        if (best != "") alias_doc[name] = fdoc[best]
    }
    for (name in alias_doc) fdoc[name] = alias_doc[name]
    for (hname in fdoc)
        printf "F\037%s\037%s\037%s\037%d\037%s\037%s\n", hname, ffile[hname], fcall[hname], fcommand[hname], fdecl[hname], fdoc[hname]
}
EOF
find src -name '*.f90' ! -name docstrings_mod.f90 | sort |
    xargs awk -f tools/fortran-names-lib.awk -f "$tmp.pass1" |
    LC_ALL=C sort -t "$(printf '\037')" -k1,1 -k2,2 >"$tmp.records"

# The named constants of enumerated settings (integer variables whose
# value is one of a set of parameters, like search_upper_case): one
# record per parameter statement, the settings its comment names
# ("Values of `x' and `y'") then the statement, separated by \037.
# A declaration continued over several lines is joined into one.
find src -name '*.f90' ! -name docstrings_mod.f90 | sort |
    xargs awk '
        cont { sub(/^[ \t]*/, ""); line = line $0 }
        !cont && /^[ \t]*!>/ { c = $0; while (match(c, /`[a-z][a-z0-9_]*\x27/)) { names = names " " substr(c, RSTART + 1, RLENGTH - 2); c = substr(c, RSTART + RLENGTH) } ; next }
        !cont && /integer, parameter, public ::/ { line = $0 }
        !cont && !/integer, parameter, public ::/ { names = ""; next }
        {
            cont = sub(/[ \t]*&[ \t]*$/, " ", line)
            if (!cont) { print names "\037" line; names = "" }
        }' >"$tmp.params" || true

# The user options (tools/user-options), without comments.
grep -v '^#' tools/user-options | grep -v '^[ \t]*$' >"$tmp.uopts" || true

# Pass 2: the Fortran module.
awk -F '\037' -v params="$tmp.params" -v uopts="$tmp.uopts" '
function q(s) { gsub(/"/, "\"\"", s); return "\"" s "\"" }

# The text so far, s, with the next piece of a list: on a continuation
# line when the last line would pass 100 columns.
function wrapped(s, piece,    last) {
    last = s
    sub(/.*\n/, "", last)
    if (length(last) + length(piece) > 100) return s "&\n                " piece
    return s piece
}

# An array constructor of the N items ITEMS (Fortran text), with the type
# specification SPEC ("character(len=W) :: ", or ""), several items a
# line.  A list longer than 200 lines (the standard allows 255
# continuation lines a statement) is first printed as parameter arrays
# NAME_part_1, NAME_part_2, ... declared with DECL, and the constructor
# joins them.
function constructor(spec, decl, name, items, n,    nl, i, k, p, last, out, joined, line, size) {
    nl = 0
    line = ""
    for (i = 1; i <= n; i++) {
        if (line != "" && length(line) + length(items[i]) + 2 > 90) {
            ctext[++nl] = line
            cupto[nl] = i - 1
            line = ""
        }
        line = line (line == "" ? "" : ", ") items[i]
    }
    ctext[++nl] = line
    cupto[nl] = n
    p = 0
    joined = ""
    for (k = 1; k <= nl; k += 200) {
        last = (k + 199 < nl) ? k + 199 : nl
        out = "[" spec "&\n"
        for (i = k; i <= last; i++)
            out = out "        " ctext[i] (i < last ? ", &\n" : "]")
        if (nl <= 200) return out
        size = cupto[last] - (k > 1 ? cupto[k - 1] : 0)
        p++
        printf "    %s :: %s_part_%d(%d) = %s\n", decl, name, p, size, out
        joined = joined (p > 1 ? ", &\n        " : "&\n        ") name "_part_" p
    }
    print ""
    return "[" joined "]"
}

# A string literal for s, in pieces of at most 90 characters, each
# ending after a character that cannot be part of a name, so no name is
# split between two pieces (tools/fortran-names reads the pieces).
function lit(s, ind,    out, n) {
    out = ""
    while (length(s) > 90) {
        for (n = 90; n > 1; n--)
            if (substr(s, n, 1) !~ /[A-Za-z0-9_]/) break
        if (n == 1) n = 90
        out = out q(substr(s, 1, n)) "// &\n" ind
        s = substr(s, n + 1)
    }
    return out q(s)
}

# The kind of value set_variable reads for variable i: OPTION_LOGICAL,
# OPTION_INTEGER, OPTION_REAL, OPTION_STRING, OPTION_CHOICE (one of the
# named constants of an enumerated setting), or OPTION_NONE (hooks,
# procedures, arrays and other derived types).
function okind(i,    t) {
    t = vtype[i]
    if (varr[i]) return "OPTION_NONE"
    if (venum[i] != "" && (t ~ /^integer/ || t ~ /(integer|choice)_setting_type\)/)) return "OPTION_CHOICE"
    if (t ~ /logical_setting_type\)/ || t ~ /^logical/) return "OPTION_LOGICAL"
    if (t ~ /integer_setting_type\)/ || t ~ /^integer/) return "OPTION_INTEGER"
    if (t ~ /^real/) return "OPTION_REAL"
    if (t ~ /string_setting_type\)/ || t ~ /^character/) return "OPTION_STRING"
    return "OPTION_NONE"
}

# Whether variable i is a setting object (it can have buffer-local values).
function osetting(i) { return vtype[i] ~ /(logical|integer|string|choice)_setting_type\)/ }

# Whether variable i is a choice setting (.true., .false. and named
# constants), and whether it is one with a procedure component (an
# extension of choice_setting_type).
function ochoice(i) { return vtype[i] ~ /choice_setting_type\)/ }
function okinds(i) { return vtype[i] ~ /[a-z]_(choice|integer|logical|string)_setting_type\)/ }

# The named constants of a choice setting whose default is .true. or
# .false.: the parameter statement whose comment names it ("Values of
# x"), else the one whose constants start with the setting name; ""
# if none.
function choice_group(name,    c) {
    if (name in vgroup) return vgroup[name]
    for (c in group) if (index(c, toupper(name) "_") == 1) return group[c]
    return ""
}

# Fortran statements assigning `val` to variable i, given buffer-locally
# when `local` is true and the variable is a setting object.
function oassign(i, val, ind) {
    if (osetting(i))
        return ind "if (local) then\n" ind "    call " vfort[i] "%set_local(" val ")\n" ind "else\n" \
            ind "    " vfort[i] " = " val "\n" ind "end if"
    return ind vfort[i] " = " val
}

# Fortran expression for docstring d (lines joined by \036), indented.
function docexpr(d, ind,    n, i, lines, out, line, piece) {
    if (d == "") return "\"\""
    n = split(d, lines, "\036")
    out = ""
    for (i = 1; i <= n; i++) {
        line = lines[i]
        # Long lines go in pieces, to stay within 132 columns.
        while (length(line) > 90) {
            piece = substr(line, 1, 90)
            line = substr(line, 91)
            out = out q(piece) "// &\n" ind
        }
        out = out q(line)
        if (i < n) out = out "//LF// &\n" ind
    }
    return out
}

BEGIN {
    nu = 0
    while ((getline l < uopts) > 0) { uopt[l] = 1; nu++; uname[nu] = l }
    while ((getline l < params) > 0) {
        split(l, rec, "\037")
        l = rec[2]; sub(/.*::/, "", l)
        nn = split(rec[1], setting_names, " ")
        for (i = 1; i <= nn; i++) if (setting_names[i] != "") vgroup[setting_names[i]] = l
        sub(/.*::/, "", l)
        n = split(l, defs, ",")
        for (i = 1; i <= n; i++) {
            d = defs[i]; gsub(/[ \t]/, "", d)
            split(d, kv, "=")
            pval[kv[1]] = kv[2]
            group[kv[1]] = l
        }
    }
}

$1 == "F" { nf++; fname[nf] = $2; ffile[nf] = $3; fcall[nf] = $4; fcommand[nf] = $5; fdecl[nf] = $6; fdoc[nf] = $7 }
$1 == "V" {
    nv++; vname[nv] = $2; vfile[nv] = $3; vfort[nv] = $4; vtype[nv] = $5; varr[nv] = $6
    vinit[nv] = $7; vtarget[nv] = $8; vdoc[nv] = $9
    mods[$3] = mods[$3] (mods[$3] == "" ? "" : ", ") $4
    # The default procedure of a procedure pointer, to compare the value with.
    if ($8 != "" && !(($8 SUBSEP $7) in imported)) {
        imported[$8 SUBSEP $7] = 1
        mods[$8] = mods[$8] (mods[$8] == "" ? "" : ", ") $7
    }
    # The procedures naming the value of a procedure setting.
    if ($5 ~ /function_setting_type\)/ && !named_cmd++)
        mods["callint_mod.f90"] = mods["callint_mod.f90"] (mods["callint_mod.f90"] == "" ? "" : ", ") "command_name_of"
    # A prefix argument is shown as its structure constructor.
    if ($5 ~ /prefix_arg_type\)/ && !named_prefix++)
        mods["callint_mod.f90"] = mods["callint_mod.f90"] (mods["callint_mod.f90"] == "" ? "" : ", ") "prefix_arg_text"
    if ($5 ~ /indent_line_setting_type\)/ && !named_indent++)
        mods["indent_mod.f90"] = mods["indent_mod.f90"] (mods["indent_mod.f90"] == "" ? "" : ", ") "indent_line_function_name"
    # A choice setting: the reserved choices, and its named constants
    # found by its name when its default is .true. or .false..
    if ($5 ~ /choice_setting_type\)/) {
        # The parameter statement whose comment names the setting comes
        # first (a family shared by several settings may name only some),
        # then the family of its initial constant.
        cg = ($4 in vgroup) ? vgroup[$4] : (($7 in group) ? group[$7] : choice_group($4))
        if (cg != "") {
            venum[nv] = cg
            n = split(cg, defs, ",")
            for (i = 1; i <= n; i++) {
                d = defs[i]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
                mods[$3] = mods[$3] ", " d
            }
        }
    } else if ($7 ~ /^[A-Z][A-Z0-9_]*$/ && ($7 in group)) {
        venum[nv] = group[$7]
        n = split(group[$7], defs, ",")
        for (i = 1; i <= n; i++) {
            d = defs[i]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
            mods[$3] = mods[$3] ", " d
        }
    }
}

END {
    # Every user option must be a documented variable.
    for (i = 1; i <= nv; i++) vidx[vname[i]] = i
    for (k = 1; k <= nu; k++)
        if (!(uname[k] in vidx)) {
            print "tools/user-options: " uname[k] " is not a documented variable" > "/dev/stderr"
            exit 1
        }
    print "! docstrings_mod.f90 -- the documentation C-h f, C-h k and C-h v show."
    print "! Generated by tools/gen-docstrings.sh from the !> comments in src/; do"
    print "! not edit."
    print "!"
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
    print "!> The documentation of the commands, the config API'"'"'s procedures and"
    print "!> its variables, collected from their docstrings by"
    print "!> tools/gen-docstrings.sh (the counterpart of etc/DOC, which"
    print "!> lib-src/make-docfile.c collects from the C sources).  Variables are"
    print "!> read here, so their current values can be shown."
    print "module docstrings_mod"
    for (m in mods) mlist[++nm] = m
    # Sort the module names (few of them; insertion sort).
    for (i = 2; i <= nm; i++) {
        x = mlist[i]
        for (j = i - 1; j >= 1 && mlist[j] > x; j--) mlist[j + 1] = mlist[j]
        mlist[j + 1] = x
    }
    print "    use, intrinsic :: iso_fortran_env, only: int64, real64"
    print "    use print_mod, only: fortran_text"
    for (i = 1; i <= nm; i++) {
        m = mlist[i]; sub(/\.f90$/, "", m)
        line = "    use " m ", only: "
        n = split(mods[mlist[i]], ids, ", ")
        for (k = 1; k <= n; k++) {
            if (length(line) + length(ids[k]) > 100) {
                print line "&"
                line = "        "
            }
            line = line ids[k] (k < n ? ", " : "")
        }
        print line
    }
    print "    implicit none(type, external)"
    print "    private"
    print ""
    print "    public :: function_doc, variable_doc"
    print "    public :: user_option_info, set_user_option_logical, set_user_option_integer, set_user_option_real, &"
    print "              set_user_option_string, set_user_option_choice, user_option_constants"
    print "    public :: init_choice_values"
    print ""
    print "    !> Kinds of value of a user option, as `set_variable` reads it."
    print "    integer, parameter, public :: OPTION_NONE = 0, OPTION_LOGICAL = 1, OPTION_INTEGER = 2, OPTION_REAL = 3, &"
    print "                                  OPTION_STRING = 4, OPTION_CHOICE = 5"
    print ""
    print "    !> What help shows about a function or variable."
    print "    type, public :: doc_entry_type"
    print "        !> The docstring, before substitute-command-keys; \"\" if none."
    print "        character(len=:), allocatable :: doc"
    print "        !> The source file that defines it."
    print "        character(len=:), allocatable :: file"
    print "        !> A function: how Fortran calls it (\"\" for a command only)."
    print "        !> A variable: its Fortran name."
    print "        character(len=:), allocatable :: fortran"
    print "        !> A function: the declarations of its dummy arguments and"
    print "        !> result, one per line (\"\" if none), as C-h f shows them."
    print "        !> A variable: its declaration when it is a procedure pointer"
    print "        !> (`procedure(IFACE), pointer :: NAME`), \"\" otherwise."
    print "        character(len=:), allocatable :: declarations"
    print "        !> A function that is a command."
    print "        logical :: command = .false."
    print "        !> A variable: its value and its value at startup (\"\" when"
    print "        !> unknown), as Fortran."
    print "        character(len=:), allocatable :: value, original"
    print "        !> A variable that is a user option (tools/user-options)."
    print "        logical :: user_option = .false."
    print "    end type doc_entry_type"
    print ""
    # The name arrays, from the longest name so none is truncated.
    w = 1
    for (i = 1; i <= nf; i++) {
        citem[i] = q(fname[i])
        if (length(fname[i]) > w) w = length(fname[i])
    }
    spec = sprintf("character(len=%d)", w)
    c = constructor(spec " :: ", spec ", parameter", "function_names", citem, nf)
    printf "    !> The documented functions, sorted.\n"
    printf "    %s, parameter, public :: function_names(%d) = %s\n", spec, nf, c
    print ""
    w = 1
    for (i = 1; i <= nv; i++) {
        citem[i] = q(vname[i])
        if (length(vname[i]) > w) w = length(vname[i])
    }
    spec = sprintf("character(len=%d)", w)
    c = constructor(spec " :: ", spec ", parameter", "variable_names", citem, nv)
    printf "    !> The documented variables, sorted.\n"
    printf "    %s, parameter, public :: variable_names(%d) = %s\n", spec, nv, c
    print ""
    w = 1
    for (k = 1; k <= nu; k++) {
        citem[k] = q(uname[k])
        if (length(uname[k]) > w) w = length(uname[k])
    }
    spec = sprintf("character(len=%d)", w)
    c = constructor(spec " :: ", spec ", parameter", "user_option_names", citem, nu)
    printf "    !> The user options (tools/user-options), sorted.\n"
    printf "    %s, parameter, public :: user_option_names(%d) = %s\n", spec, nu, c
    print ""
    print "    character(len=*), parameter :: LF = achar(10)"
    print ""
    print "    interface value_text"
    print "        module procedure value_text_logical, value_text_int64s, value_text_integers, value_text_real, &"
    print "            value_text_real64, value_text_real64s, value_text_string, value_text_strings"
    print "    end interface value_text"
    print ""
    print "contains"
    print ""
    print "    !> The documentation of the function called `name`; `found` is false"
    print "    !> for an unknown name."
    print "    subroutine function_doc(name, entry, found)"
    print "        character(len=*), intent(in) :: name"
    print "        type(doc_entry_type), intent(out) :: entry"
    print "        logical, intent(out) :: found"
    print ""
    print "        found = .true."
    print "        entry%fortran = \"\""
    print "        entry%declarations = \"\""
    print ""
    print "        select case (name)"
    for (i = 1; i <= nf; i++) {
        print "        case (" q(fname[i]) ")"
        print "            entry%file = " q(ffile[i])
        if (fcall[i] != "") print "            entry%fortran = " lit(fcall[i], "                ")
        if (fdecl[i] != "") print "            entry%declarations = " docexpr(fdecl[i], "                                    ")
        if (fcommand[i]) print "            entry%command = .true."
        print "            entry%doc = " docexpr(fdoc[i], "                        ")
    }
    print "        case default"
    print "            found = .false."
    print "        end select"
    print "    end subroutine function_doc"
    print ""
    print "    !> The documentation and value of the variable called `name`;"
    print "    !> `found` is false for an unknown name."
    print "    subroutine variable_doc(name, entry, found)"
    print "        character(len=*), intent(in) :: name"
    print "        type(doc_entry_type), intent(out) :: entry"
    print "        logical, intent(out) :: found"
    print "        character(len=21) :: digits"
    print ""
    print "        found = .true."
    print "        entry%original = \"\""
    print "        entry%declarations = \"\""
    print ""
    print "        select case (name)"
    for (i = 1; i <= nv; i++) {
        print "        case (" q(vname[i]) ")"
        print "            entry%file = " q(vfile[i])
        print "            entry%fortran = " q(vfort[i])
        if (vname[i] in uopt) print "            entry%user_option = .true."
        if (vinit[i] != "") {
            init = vinit[i]
            sub(/_int(32|64)$/, "", init)
            print "            entry%original = " lit(init, "                ")
        }
        # A standard value the setup of the module sets (a procedure given at
        # startup) replaces the one of the declaration.
        if (ochoice(i) && okinds(i)) {
            print ""
            print "            if (len(" vfort[i] "%standard_value()) > 0) then"
            print "                entry%original = " vfort[i] "%standard_value()"
            print "            end if"
            print ""
        }
        if (ochoice(i)) {
            # A choice setting: a value of another kind (a procedure, a
            # number, a string), the named constant of its choice, or
            # .true./.false..
            ind = "            "
            if (okinds(i)) {
                print "            entry%value = " vfort[i] "%other_value()"
                print ""
                print "            if (len(entry%value) == 0) then"
                ind = "                "
            }
            print ind "select case (" vfort[i] "%choice)"
            n = (venum[i] == "") ? 0 : split(venum[i], defs, ",")
            for (k = 1; k <= n; k++) {
                d = defs[k]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
                print ind "case (" d ")"
                print ind "    entry%value = " q(d)
            }
            print ind "case default"
            print ind "    entry%value = value_text(" vfort[i] "%value)"
            print ind "end select"
            if (okinds(i)) print "            end if"
        } else if (venum[i] != "") {
            # The value of a setting object is its value component.
            ex = vfort[i]
            if (vtype[i] ~ /_setting_type\)/) ex = ex "%value"
            print "            select case (" ex ")"
            n = split(venum[i], defs, ",")
            for (k = 1; k <= n; k++) {
                d = defs[k]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
                print "            case (" d ")"
                print "                entry%value = " q(d)
            }
            print "            case default"
            if (vtype[i] ~ /^integer/ || vtype[i] ~ /integer_setting_type\)/) {
                # An integer outside its named constants: its decimal text
                # (an internal write, PORTING.md "Numbers to text").
                print "                write (digits, \"(i0)\") " ex
                print "                entry%value = trim(digits)"
            } else print "                entry%value = value_text(" ex ")"
            print "            end select"
        } else {
            # Values as Fortran: a string or array that is not allocated
            # is "unallocated", a procedure pointer the name of its procedure or
            # null(), a command pointer the name of its command or null(), a
            # prefix argument its structure constructor.  Other derived types
            # (hooks, keymaps, syntax tables) have no printed representation
            # yet: help leaves the value out.
            if (vtype[i] ~ /(integer|logical)_setting_type\)/ && okinds(i)) {
                print "            entry%value = " vfort[i] "%other_value()"
                print ""
                print "            if (len(entry%value) == 0) then"
                if (vtype[i] ~ /integer_setting_type\)/) {
                    print "                write (digits, \"(i0)\") " vfort[i] "%value"
                    print "                entry%value = trim(digits)"
                } else print "                entry%value = value_text(" vfort[i] "%value)"
                print "            end if"
            } else if (vtype[i] ~ /integer_setting_type\)/) {
                print "            write (digits, \"(i0)\") " vfort[i] "%value"
                print "            entry%value = trim(digits)"
            } else if (vtype[i] ~ /logical_setting_type\)/) print "            entry%value = value_text(" vfort[i] "%value)"
            else if (vtype[i] ~ /string_setting_type\)/) {
                print "            if (allocated(" vfort[i] "%value)) then"
                print "                entry%value = value_text(" vfort[i] "%value)"
                print "            else"
                print "                entry%value = \"unallocated\""
                print "            end if"
            } else if (vtype[i] ~ /function_setting_type\)/) {
                print "            if (associated(" vfort[i] "%value)) then"
                print "                entry%value = command_name_of(" vfort[i] "%value)"
                print "            else"
                print "                entry%value = \"null()\""
                print "            end if"
            } else if (vtype[i] ~ /^type\(command_type\), pointer/) {
                print "            if (associated(" vfort[i] ")) then"
                print "                entry%value = trim(" vfort[i] "%name)"
                print "            else"
                print "                entry%value = \"null()\""
                print "            end if"
            } else if (vtype[i] ~ /^procedure/) {
                # A plain procedure pointer: null(), its default procedure
                # when it still points there (a procedure has no name at
                # run time, so the value is compared with the default,
                # which must be public), else a procedure the configuration
                # set; empty (no value line) when the default is private and
                # cannot be compared with.
                decl = vtype[i]
                sub(/,[ \t]*public/, "", decl)
                print "            entry%declarations = " lit(decl " :: " vfort[i], "                                 ")
                print "            if (.not. associated(" vfort[i] ")) then"
                print "                entry%value = \"null()\""
                if (vtarget[i] != "") {
                    print "            else if (associated(" vfort[i] ", " vinit[i] ")) then"
                    print "                entry%value = " q(vinit[i])
                    print "            else"
                    print "                entry%value = \"a procedure set by the configuration\""
                } else if (vinit[i] == "" || vinit[i] == "null()") {
                    print "            else"
                    print "                entry%value = \"a procedure set by the configuration\""
                } else {
                    print "            else"
                    print "                entry%value = \"\""
                }
                print "            end if"
            } else if (vtype[i] ~ /^type\(prefix_arg_type\)/)
                print "            entry%value = prefix_arg_text(" vfort[i] ")"
            else if (vtype[i] ~ /indent_line_setting_type\)/)
                print "            entry%value = indent_line_function_name(" vfort[i] "%value)"
            else if (vtype[i] ~ /value_type/ && !varr[i]) print "            entry%value = fortran_text(" vfort[i] ")"
            else if (vtype[i] ~ /allocatable/ && vtype[i] ~ /^integer/ && !varr[i]) {
                print "            if (allocated(" vfort[i] ")) then"
                print "                write (digits, \"(i0)\") " vfort[i]
                print "                entry%value = trim(digits)"
                print "            else"
                print "                entry%value = \"unallocated\""
                print "            end if"
            } else if (vtype[i] ~ /allocatable/ && vtype[i] !~ /^type/) {
                print "            if (allocated(" vfort[i] ")) then"
                print "                entry%value = value_text(" vfort[i] ")"
                print "            else"
                print "                entry%value = \"unallocated\""
                print "            end if"
            } else if (vtype[i] ~ /^integer/ && !varr[i]) {
                print "            write (digits, \"(i0)\") " vfort[i]
                print "            entry%value = trim(digits)"
            } else if (vtype[i] !~ /^type/) print "            entry%value = value_text(" vfort[i] ")"
        }
        print "            entry%doc = " docexpr(vdoc[i], "                        ")
    }
    print "        case default"
    print "            found = .false."
    print "        end select"
    print "    end subroutine variable_doc"
    print ""
    print "    !> The kind of value the user option called `name` takes (OPTION_NONE"
    print "    !> if `set_variable` cannot set it, or for an unknown name), and"
    print "    !> whether it is a setting object, which can have buffer-local values."
    print "    subroutine user_option_info(name, kind, setting)"
    print "        character(len=*), intent(in) :: name"
    print "        integer, intent(out) :: kind"
    print "        logical, intent(out) :: setting"
    print ""
    print "        kind = OPTION_NONE"
    print "        setting = .false."
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) == "OPTION_NONE") continue
        print "        case (" q(vname[i]) ")"
        print "            kind = " okind(i)
        if (osetting(i)) print "            setting = .true."
    }
    print "        case default"
    print "        end select"
    print "    end subroutine user_option_info"
    print ""
    print "    !> Set the logical user option called `name` to `v`, buffer-locally if"
    print "    !> `local` (a setting object)."
    print "    subroutine set_user_option_logical(name, v, local)"
    print "        character(len=*), intent(in) :: name"
    print "        logical, intent(in) :: v, local"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_LOGICAL") continue
        print "        case (" q(vname[i]) ")"
        print oassign(i, "v", "            ")
    }
    print "        case default"
    print "            if (local) return"
    print "        end select"
    print "    end subroutine set_user_option_logical"
    print ""
    print "    !> Set the integer user option called `name` to `v`, buffer-locally if"
    print "    !> `local` (a setting object); `ok` is false if `v` is out of its range."
    print "    subroutine set_user_option_integer(name, v, local, ok)"
    print "        character(len=*), intent(in) :: name"
    print "        integer(int64), intent(in) :: v"
    print "        logical, intent(in) :: local"
    print "        logical, intent(out) :: ok"
    print ""
    print "        ok = .true."
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_INTEGER") continue
        print "        case (" q(vname[i]) ")"
        if (vtype[i] ~ /^integer,/ || vtype[i] ~ /^integer$/) {
            print "            if (v < -huge(0) - 1 .or. v > huge(0)) then"
            print "                ok = .false."
            print "                return"
            print "            end if"
            print oassign(i, "int(v)", "            ")
        } else print oassign(i, "v", "            ")
    }
    print "        case default"
    print "            if (local) return"
    print "        end select"
    print "    end subroutine set_user_option_integer"
    print ""
    print "    !> Set the real user option called `name` to `v`."
    print "    subroutine set_user_option_real(name, v)"
    print "        character(len=*), intent(in) :: name"
    print "        real, intent(in) :: v"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_REAL") continue
        print "        case (" q(vname[i]) ")"
        print "            " vfort[i] " = v"
    }
    print "        case default"
    print "            if (v > 0) return"
    print "        end select"
    print "    end subroutine set_user_option_real"
    print ""
    print "    !> Set the string user option called `name` to `v`, buffer-locally if"
    print "    !> `local` (a setting object)."
    print "    subroutine set_user_option_string(name, v, local)"
    print "        character(len=*), intent(in) :: name, v"
    print "        logical, intent(in) :: local"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_STRING") continue
        print "        case (" q(vname[i]) ")"
        print oassign(i, "v", "            ")
    }
    print "        case default"
    print "            if (local) return"
    print "        end select"
    print "    end subroutine set_user_option_string"
    print ""
    print "    !> Set the enumerated user option called `name` to its named constant"
    print "    !> `constant` (in upper case; .TRUE. or .FALSE. for a choice setting),"
    print "    !> buffer-locally if `local` (a setting object); `ok` is false if"
    print "    !> `constant` is not one of its constants."
    print "    subroutine set_user_option_choice(name, constant, local, ok)"
    print "        character(len=*), intent(in) :: name, constant"
    print "        logical, intent(in) :: local"
    print "        logical, intent(out) :: ok"
    print ""
    print "        ok = .true."
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_CHOICE") continue
        print "        case (" q(vname[i]) ")"
        print "            select case (constant)"
        if (ochoice(i)) {
            print "            case (\".TRUE.\")"
            print oassign(i, ".true.", "                ")
            print "            case (\".FALSE.\")"
            print oassign(i, ".false.", "                ")
        }
        n = split(venum[i], defs, ",")
        for (m = 1; m <= n; m++) {
            d = defs[m]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
            print "            case (" q(toupper(d)) ")"
            print oassign(i, d, "                ")
        }
        print "            case default"
        print "                ok = .false."
        print "            end select"
    }
    print "        case default"
    print "            ok = .false."
    print "        end select"
    print "    end subroutine set_user_option_choice"
    print ""
    # The constants all have the length of the longest, so none is
    # truncated.
    cw = length(".false.")
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_CHOICE") continue
        n = split(venum[i], defs, ",")
        for (m = 1; m <= n; m++) {
            d = defs[m]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
            if (length(d) > cw) cw = length(d)
        }
    }
    print "    !> The named constants of the enumerated user option called `name`"
    print "    !> (in upper case), all of one length, and their values; empty"
    print "    !> arrays for another name."
    print "    subroutine user_option_constants(name, constants, values)"
    print "        character(len=*), intent(in) :: name"
    print "        character(len=:), allocatable, intent(out) :: constants(:)"
    print "        integer(int64), allocatable, intent(out) :: values(:)"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_CHOICE") continue
        print "        case (" q(vname[i]) ")"
        n = split(venum[i], defs, ",")
        cl = "            constants = [character(len=" cw ") :: "; vl = "            values = [integer(int64) :: "
        # A choice setting also takes .true. and .false..
        if (ochoice(i)) {
            cl = cl q(".true.") ", " q(".false.") ", "
            vl = vl "1_int64, 0_int64, "
        }
        for (m = 1; m <= n; m++) {
            d = defs[m]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
            cl = wrapped(cl, q(toupper(d)) (m < n ? ", " : "]"))
            vl = wrapped(vl, "int(" d ", int64)" (m < n ? ", " : "]"))
        }
        print cl
        print vl
    }
    print "        case default"
    print "            allocate (character(len=" cw ") :: constants(0))"
    print "            allocate (values(0))"
    print "        end select"
    print "    end subroutine user_option_constants"
    print ""
    # The named constants each choice setting accepts (panel 2026-11-04):
    # those of its "Values of" parameter statement, none without one.
    print "    !> Declare the named constants each choice setting of the config API"
    print "    !> accepts: those of the parameter statement documenting its values"
    print "    !> (none when it has none).  Not a port."
    print "    subroutine init_choice_values()"
    for (i = 1; i <= nv; i++) {
        if (!ochoice(i)) continue
        n = (venum[i] == "") ? 0 : split(venum[i], defs, ",")
        print "        " vfort[i] "%choices = 0"
        if (n > 0) {
            line = "        " vfort[i] "%choices(1:" n ") = ["
            for (k = 1; k <= n; k++) {
                d = defs[k]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
                item = d (k < n ? ", " : "]")
                if (length(line) + length(item) > 110) { print line "&"; line = "            " }
                line = line item
            }
            print line
        }
        print "        " vfort[i] "%choices_declared = .true."
    }
    print "    end subroutine init_choice_values"
    print ""
    print "    function value_text_logical(v) result(s)"
    print "        logical, intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print ""
    print "        s = merge(\".true. \", \".false.\", v)"
    print "        s = trim(s)"
    print "    end function value_text_logical"
    print ""
    print "    ! A real with at least one digit"
    print "    ! after the point, no trailing zeros."
    print "    function value_text_real(v) result(s)"
    print "        real, intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print "        character(len=32) :: digits"
    print ""
    print "        write (digits, \"(f0.6)\") v"
    print "        s = trim(digits)"
    print "        do while (len(s) > 2)"
    print "            if (s(len(s):len(s)) /= \"0\" .or. s(len(s) - 1:len(s) - 1) == \".\") exit"
    print "            s = s(1:len(s) - 1)"
    print "        end do"
    print "        if (s(1:1) == \".\") s = \"0\"//s"
    print "        if (len(s) > 1) then"
    print "            if (s(1:2) == \"-.\") s = \"-0\"//s(2:)"
    print "        end if"
    print "    end function value_text_real"
    print ""
    print "    function value_text_real64(v) result(s)"
    print "        real(real64), intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print ""
    print "        s = value_text_real(real(v))"
    print "    end function value_text_real64"
    print ""
    print "    !> An array of reals as a Fortran array constructor."
    print "    function value_text_real64s(v) result(s)"
    print "        real(real64), intent(in) :: v(:)"
    print "        character(len=:), allocatable :: s"
    print "        integer :: i"
    print ""
    print "        s = \"[\""
    print ""
    print "        do i = 1, size(v)"
    print "            if (i > 1) s = s//\", \""
    print "            s = s//value_text_real64(v(i))"
    print "        end do"
    print ""
    print "        s = s//\"]\""
    print "    end function value_text_real64s"
    print ""
    print "    !> An array of integers as a Fortran array constructor."
    print "    function value_text_int64s(v) result(s)"
    print "        integer(int64), intent(in) :: v(:)"
    print "        character(len=:), allocatable :: s"
    print "        character(len=21) :: digits"
    print "        integer :: i"
    print ""
    print "        s = \"[\""
    print ""
    print "        do i = 1, size(v)"
    print "            if (i > 1) s = s//\", \""
    print "            write (digits, \"(i0)\") v(i)"
    print "            s = s//trim(digits)"
    print "        end do"
    print ""
    print "        s = s//\"]\""
    print "    end function value_text_int64s"
    print ""
    print "    !> An array of default integers as a Fortran array constructor."
    print "    function value_text_integers(v) result(s)"
    print "        integer, intent(in) :: v(:)"
    print "        character(len=:), allocatable :: s"
    print "        character(len=12) :: digits"
    print "        integer :: i"
    print ""
    print "        s = \"[\""
    print ""
    print "        do i = 1, size(v)"
    print "            if (i > 1) s = s//\", \""
    print "            write (digits, \"(i0)\") v(i)"
    print "            s = s//trim(digits)"
    print "        end do"
    print ""
    print "        s = s//\"]\""
    print "    end function value_text_integers"
    print ""
    print "    !> A string as a Fortran literal."
    print "    function value_text_string(v) result(s)"
    print "        character(len=*), intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print "        integer :: i"
    print ""
    print "        s = \"\"\"\""
    print ""
    print "        do i = 1, len_trim(v)"
    print "            s = s//v(i:i)"
    print "            if (v(i:i) == \"\"\"\") s = s//\"\"\"\""
    print "        end do"
    print ""
    print "        s = s//\"\"\"\""
    print "    end function value_text_string"
    print ""
    print "    !> An array of strings as a Fortran array constructor."
    print "    function value_text_strings(v) result(s)"
    print "        character(len=*), intent(in) :: v(:)"
    print "        character(len=:), allocatable :: s"
    print "        integer :: i"
    print ""
    print "        s = \"[\""
    print ""
    print "        do i = 1, size(v)"
    print "            if (i > 1) s = s//\", \""
    print "            s = s//value_text_string(v(i))"
    print "        end do"
    print ""
    print "        s = s//\"]\""
    print "    end function value_text_strings"
    print "end module docstrings_mod"
}
' "$tmp.records" >"$tmp.f90"

# The blank lines of CLAUDE.md (tools/blank-lines.awk).
awk -f tools/blank-lines.awk "$tmp.f90" >"$tmp.out"
if command -v fprettify >/dev/null 2>&1; then
    fprettify --config-file .fprettify.rc "$tmp.out" 2>/dev/null
fi
mv "$tmp.out" "$out"
