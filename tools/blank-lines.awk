# blank-lines.awk -- set the blank lines of a Fortran source file as
# CLAUDE.md asks; the other lines are printed unchanged.
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

# Usage: awk -f tools/blank-lines.awk FILE.f90 > OUT.f90
#
# In the bodies of procedures (and programs):
#   - one blank line after the declarations;
#   - one blank line before and after each block construct (if, do,
#     select, associate, block, where, forall, critical), at every
#     nesting level, also between two blocks;
#   - none between simple statements, at the start or the end of a block
#     body or before the end of the procedure;
#   - comments belong to the statement below them: a blank line goes
#     above them.
# One blank line before and after each `contains' (of a module or a
# procedure) and between procedures, none before the end of a module.
# Module specification parts, interface blocks and derived type
# definitions keep their blank lines.

# Set `code' to LINE without its comment and with the text of its
# strings removed, and `cont' to whether it continues on the next line.
# `quote' carries a string continued across lines.
function scan(line,    i, c, n, out) {
    out = ""
    n = length(line)
    i = 1
    if (quote != "") {
        # A continued string: it resumes after the optional `&'.
        sub(/^[ \t]*&/, "", line)
        n = length(line)
    }
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
        if (c == "\"" || c == "'") {
            quote = c
            out = out c
        } else {
            out = out c
        }
        i++
    }
    sub(/[ \t]+$/, "", out)
    cont = (out ~ /&$/)
    code = tolower(out)
}

function trim(s) {
    sub(/^[ \t]+/, "", s)
    sub(/[ \t]+$/, "", s)
    return s
}

# Whether the statement starting with FIRST (lowercased code) begins a
# procedure: [prefixes] [type] subroutine|function NAME, or a separate
# module procedure body.
function proc_start(first,    s, k) {
    s = first
    for (k = 0; k < 3; k++) {
        while (s ~ /^(pure|elemental|recursive|impure|non_recursive|module)[ \t]+/)
            sub(/^[a-z_]+[ \t]+/, "", s)
        if (s ~ /^(integer|real|logical|complex|character)[ \t]*\([^)]*\)[ \t]*/)
            sub(/^[a-z]+[ \t]*\([^)]*\)[ \t]*/, "", s)
        else if (s ~ /^(integer|real|logical|complex|character|double[ \t]+precision)[ \t]+/)
            sub(/^(double[ \t]+precision|[a-z]+)[ \t]+/, "", s)
        else if (s ~ /^(type|class)[ \t]*\([^)]*\)[ \t]*/)
            sub(/^[a-z]+[ \t]*\([^)]*\)[ \t]*/, "", s)
    }
    if (s ~ /^(subroutine|function)[ \t]+[a-z_]/) return 1
    if (first ~ /^module[ \t]+procedure[ \t]+[a-z_][a-z0-9_]*$/) return 1
    if (first ~ /^program[ \t]+[a-z_]/) return 1
    return 0
}

function proc_end(first) {
    return first ~ /^end[ \t]*(subroutine|function|procedure|program)([^a-z0-9_]|$)/ || first == "end"
}

# Whether the statement is a declaration (in the declaration part of a
# procedure).
function declaration(first) {
    if (first ~ /^[a-z_][a-z0-9_]*[ \t]*(\([^=]*\))?[ \t]*(%[a-z0-9_%()]*)?[ \t]*=/ && first !~ /::/)
        return 0
    return first ~ /^(use|implicit|import|integer|real|double|complex|logical|character|external|intrinsic|parameter|dimension|allocatable|save|data|namelist|common|equivalence|public|private|optional|intent|pointer|target|value|volatile|asynchronous|contiguous|protected|bind|format|include|enum|generic)([^a-z0-9_]|$)/ ||
        first ~ /^(type|class|procedure)[ \t]*(\(|,|::)/
}

function interface_start(first) {
    return first ~ /^(abstract[ \t]+)?interface([^a-z0-9_]|$)/
}

function typedef_start(first) {
    return first ~ /^type[ \t]*(,|::)/ || (first ~ /^type[ \t]+[a-z_]/ && first !~ /^type[ \t]+is[ \t]*\(/)
}

# The kind of an executable statement from its first and last lines:
# "open", "mid", "close" or "simple".
function exec_kind(first, last) {
    if (first ~ /^[a-z_][a-z0-9_]*[ \t]*:[ \t]*(do|if|select|block|associate|forall|where|critical)([^a-z0-9_]|$)/)
        sub(/^[a-z_][a-z0-9_]*[ \t]*:[ \t]*/, "", first)
    if (first ~ /^end[ \t]*(if|do|select|associate|block|where|forall|critical)([^a-z0-9_]|$)/) return "close"
    if (first ~ /^else([ \t(]|if|where|$)/) return "mid"
    if (first ~ /^case[ \t]*(\(|default)/) return "mid"
    if (first ~ /^(type[ \t]+is|class[ \t]+is|class[ \t]+default|rank)([ \t(]|$)/) return "mid"
    if (first ~ /^if[ \t]*\(/ && last ~ /(^|[^a-z0-9_])then$/) return "open"
    if (first ~ /^do([ \t]|$)/) return "open"
    if (first ~ /^select[ \t]*(case|type|rank)/) return "open"
    if (first ~ /^associate[ \t]*\(/) return "open"
    if (first ~ /^(block|critical)$/) return "open"
    if (first ~ /^(where|forall)[ \t]*\(/ && last ~ /\)$/) return "open"
    return "simple"
}

function blanks(n,    k) {
    for (k = 0; k < n; k++) print ""
}

# Print the pending comments and the statement S, with N blank lines
# before them.  Between procedures, a section banner (a comment between
# two rules of dashes) keeps a blank line after it.
function emit(n, s,    k, rules) {
    blanks(n)
    rules = 0
    for (k = 1; k <= ncomments; k++) {
        print comments[k]
        if (state[depth] == "between" && comments[k] ~ /^[ \t]*! -----+[ \t]*$/ && ++rules % 2 == 0)
            print ""
    }
    ncomments = 0
    for (k = ustart[s]; k <= uend[s]; k++) print lines[k]
}

{ lines[++nlines] = $0 }

END {
    # Group the lines into units: blank lines, comment lines and
    # statements (with their continuation lines).
    quote = ""
    nunits = 0
    i = 1
    while (i <= nlines) {
        nunits++
        ustart[nunits] = i
        if (lines[i] ~ /^[ \t]*$/) {
            ukind[nunits] = "blank"
        } else if (lines[i] ~ /^[ \t]*!/) {
            ukind[nunits] = "comment"
        } else {
            ukind[nunits] = "code"
            scan(lines[i])
            ufirst[nunits] = trim(code)
            ulast[nunits] = trim(code)
            while ((cont || quote != "") && i < nlines) {
                i++
                if (lines[i] ~ /^[ \t]*!/ || lines[i] ~ /^[ \t]*$/) continue
                scan(lines[i])
                ulast[nunits] = trim(code)
            }
            quote = ""
            sub(/[ \t]*&$/, "", ulast[nunits])
            sub(/[ \t]*&$/, "", ufirst[nunits])
        }
        uend[nunits] = i
        i++
    }

    # States: "file" (outside program units), "spec" (a module's
    # specification part), "between" (after a `contains'), "decl" and
    # "exec" (the parts of a procedure), "interface" and "type"
    # (printed as they are).
    depth = 1
    state[1] = "file"
    pendblank = 0
    ncomments = 0
    for (u = 1; u <= nunits; u++) {
        st = state[depth]
        kind = ukind[u]
        first = ufirst[u]
        if (st == "file" || st == "spec" || st == "interface" || st == "type") {
            if (kind == "blank") {
                pendblank++
                continue
            }
            if (kind == "comment") {
                blanks(pendblank)
                pendblank = 0
                print lines[ustart[u]]
                continue
            }
            if (st == "spec" && first == "contains") {
                blanks(1)
                pendblank = 0
                print lines[ustart[u]]
                state[depth] = "between"
                continue
            }
            if (st == "spec" && first ~ /^end[ \t]*(module|submodule)([^a-z0-9_]|$)/) {
                pendblank = 0
                for (k = ustart[u]; k <= uend[u]; k++) print lines[k]
                depth--
                continue
            }
            blanks(pendblank)
            pendblank = 0
            for (k = ustart[u]; k <= uend[u]; k++) print lines[k]
            if (st == "file") {
                if (first ~ /^module[ \t]+[a-z_]/ || first ~ /^submodule[ \t]*\(/) {
                    state[++depth] = "spec"
                } else if (first ~ /^program[ \t]+[a-z_]/) {
                    state[++depth] = "decl"
                    hasdecl[depth] = 0
                }
            } else if (st == "spec") {
                if (interface_start(first)) state[++depth] = "interface"
                else if (typedef_start(first)) state[++depth] = "type"
            } else if (st == "interface") {
                if (first ~ /^end[ \t]*interface([^a-z0-9_]|$)/) depth--
            } else if (st == "type") {
                if (first ~ /^end[ \t]*type([^a-z0-9_]|$)/) depth--
            }
            continue
        }
        # Inside procedures and between them, blank lines are made anew
        # and comments wait for their statement.
        if (kind == "blank") continue
        if (kind == "comment") {
            comments[++ncomments] = lines[ustart[u]]
            continue
        }
        if (st == "between") {
            if (proc_start(first)) {
                emit(1, u)
                state[++depth] = "decl"
                hasdecl[depth] = 0
            } else {
                emit(0, u)
                if (proc_end(first) || first ~ /^end[ \t]*(module|submodule)([^a-z0-9_]|$)/) depth--
            }
            continue
        }
        if (st == "decl") {
            if (proc_end(first)) {
                emit(0, u)
                depth--
                continue
            }
            if (first == "contains") {
                emit(1, u)
                state[depth] = "between"
                continue
            }
            if (interface_start(first)) {
                emit(0, u)
                hasdecl[depth] = 1
                state[++depth] = "interface"
                continue
            }
            if (typedef_start(first)) {
                emit(0, u)
                hasdecl[depth] = 1
                state[++depth] = "type"
                continue
            }
            if (declaration(first)) {
                emit(0, u)
                hasdecl[depth] = 1
                continue
            }
            # The first executable statement.
            state[depth] = "exec"
            level[depth] = 0
            prev[depth, 0] = hasdecl[depth] ? "decl" : "none"
            st = "exec"
        }
        # st == "exec"
        if (proc_end(first)) {
            emit(0, u)
            depth--
            continue
        }
        if (first == "contains") {
            emit(1, u)
            state[depth] = "between"
            continue
        }
        lv = level[depth]
        p = prev[depth, lv]
        ek = exec_kind(first, ulast[u])
        if (ek == "simple") {
            emit((p == "block" || p == "decl") ? 1 : 0, u)
            prev[depth, lv] = "simple"
        } else if (ek == "open") {
            emit((p == "none") ? 0 : 1, u)
            level[depth] = lv + 1
            prev[depth, lv + 1] = "none"
        } else if (ek == "mid") {
            emit(0, u)
            prev[depth, lv] = "none"
        } else {
            emit(0, u)
            if (lv > 0) level[depth] = lv - 1
            prev[depth, level[depth]] = "block"
        }
    }
    # Comments left at the end of the file.
    blanks(pendblank)
    for (k = 1; k <= ncomments; k++) print comments[k]
}
