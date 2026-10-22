# blank-lines.awk -- blank lines in Fortran sources: one between blocks of
# code (after the Bristol RSE Fortran guidelines), and never more than one.
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

# The blocks, made mechanical:
#
# - One blank line between procedures (and interface bodies), before and
#   after `contains`.
# - One blank line before and after every multi-line construct (if, do,
#   select, block, associate, where, forall, critical; type and interface
#   definitions), and between the declarations and the first statement.
# - None at the start or end of a body: after a construct or procedure
#   opens, before `else`, `case` and the like, before any `end` (of a
#   procedure, module or program too).
# - Elsewhere, the author's blank lines between runs of statements stay,
#   with runs of blank lines shrunk to one.
#
# Comments directly above a statement belong to it, so its blank lines go
# above them.  Prints the file with the rule applied.  Usage:
#   awk -f tools/blank-lines.awk FILE > FIXED

# The code of `line` without its comment (quotes respected).
function code_part(line,   i, c, q, n) {
    q = ""
    n = length(line)
    for (i = 1; i <= n; i++) {
        c = substr(line, i, 1)
        if (q != "") {
            if (c == q) q = ""
        } else if (c == "\"" || c == "'") {
            q = c
        } else if (c == "!") {
            return substr(line, 1, i - 1)
        }
    }
    return line
}

function trim(s) {
    sub(/^[ \t]+/, "", s)
    sub(/[ \t]+$/, "", s)
    return s
}

function continues(line,   code) {
    code = trim(code_part(line))
    return code ~ /&$/
}

# Position after the parenthesis that closes the one at `start` in `s`
# (0 if unbalanced).
function after_parens(s, start,   i, c, depth, n, q) {
    depth = 0
    q = ""
    n = length(s)
    for (i = start; i <= n; i++) {
        c = substr(s, i, 1)
        if (q != "") {
            if (c == q) q = ""
            continue
        }
        if (c == "\"" || c == "'") q = c
        else if (c == "(") depth++
        else if (c == ")") {
            depth--
            if (depth == 0) return i + 1
        }
    }
    return 0
}

function is_proc_start(s) {
    if (s ~ /^module[ \t]+procedure/) return 0
    if (s ~ /^procedure[ \t]*[(,:]/) return 0
    return s ~ /^((pure|elemental|recursive|impure|non_recursive|module)[ \t]+)*((integer|real|logical|character|complex|double[ \t]+precision)([ \t]*[(*][^)]*\)?)?[ \t]+|type[ \t]*\([^)]*\)[ \t]+|class[ \t]*\([^)]*\)[ \t]+)?((pure|elemental|recursive|impure|module)[ \t]+)*(subroutine|function)[ \t]+[a-z_]/
}

function is_declaration(s) {
    # An assignment is never a declaration, whatever its variable is called.
    if (s ~ /^[a-z_][a-z0-9_]*[ \t]*(\([^)]*\))?[ \t]*=([^=]|$)/) return 0
    return s ~ /^(use[ \t,]|implicit[ \t]|import([ \t,]|$)|(integer|real|logical|character|complex)([ \t(,*:]|$)|double[ \t]+precision|type[ \t]*\(|class[ \t]*\(|procedure[ \t]*\(|external([ \t,:]|$)|intrinsic([ \t,:]|$)|dimension[ \t(]|parameter[ \t(]|save([ \t,:]|$)|target([ \t,:]|$)|pointer([ \t,:]|$)|allocatable([ \t,:]|$)|optional([ \t,:]|$)|intent[ \t(]|private([ \t,:]|$)|public([ \t,:]|$)|protected([ \t,:]|$)|volatile([ \t,:]|$)|asynchronous([ \t,:]|$)|data[ \t]|namelist[ \t\/]|generic[ \t,:]|enumerator[ \t,:]|module[ \t]+procedure|procedure([ \t,:]|$)|final[ \t,:])/
}

# The role of statement `s` (lowercased code, continuations joined, label
# removed): unit_start, unit_end, contains, open, mid, close or simple;
# `kind` gets the kind of frame an open or unit_start makes.
function classify(s,   p) {
    kind = ""
    if (s ~ /^contains$/) return "contains"
    if (s ~ /^end([ \t]*(module|submodule|program|subroutine|function|procedure)([ \t]|$)|$)/) return "unit_end"
    if (s ~ /^end[ \t]*(if|do|select|block|associate|where|forall|critical|type|interface|enum)([ \t]|$)/) return "close"
    if (s ~ /^(else([ \t]|$|if)|elsewhere|case[ \t]*(\(|default)|class[ \t]+(is|default)|type[ \t]+is[ \t]*\(|rank[ \t]*(\(|default))/) return "mid"
    if (s ~ /^(module|submodule[ \t]*\(|program)[ \t]*[a-z_(]/ && s !~ /^module[ \t]+(procedure|subroutine|function)/ && s !~ /^module[ \t]+(pure|elemental|recursive|impure)/) {
        kind = "unit"
        return "unit_start"
    }
    if (s ~ /^module[ \t]+procedure[ \t]+[a-z_][a-z0-9_]*$/ && top_kind() != "interface") {
        kind = "proc"
        return "unit_start"
    }
    if (is_proc_start(s)) {
        kind = "proc"
        return "unit_start"
    }
    if (s ~ /^if[ \t]*\(/ && s ~ /then$/) { kind = "construct"; return "open" }
    if (s ~ /^do([ \t]|$)/) { kind = "construct"; return "open" }
    if (s ~ /^select[ \t]*(case|type|rank)[ \t]*\(/) { kind = "construct"; return "open" }
    if (s ~ /^block$/) { kind = "block"; return "open" }
    if (s ~ /^(associate|critical)([ \t(]|$)/) { kind = "construct"; return "open" }
    if (s ~ /^(where|forall)[ \t]*\(/) {
        p = after_parens(s, index(s, "("))
        if (p > 0 && trim(substr(s, p)) == "") { kind = "construct"; return "open" }
        return "simple"
    }
    if (s ~ /^(abstract[ \t]+)?interface([ \t(]|$)/) { kind = "interface"; return "open" }
    if (s ~ /^type[ \t]*(,|::)/ || (s ~ /^type[ \t]+[a-z_]/ && s !~ /^type[ \t]+is/)) { kind = "type"; return "open" }
    if (s ~ /^enum[ \t]*,/) { kind = "construct"; return "open" }
    return "simple"
}

function top_kind() { return sp > 0 ? stack[sp] : "file" }

# Blank lines wanted before statement `i`, given the previous one `p`.
function wanted(i, p,   r, pr, frame) {
    r = role[i]
    frame = ctx[i]
    pr = (p > 0) ? role[p] : ""
    if (p == 0) return keep(i)
    if (r == "mid" || r == "close") return 0
    if (r == "contains") return (frame == "type") ? 0 : 1
    if (pr == "contains") return (frame == "type") ? 0 : 1
    if (r == "unit_end") return 0
    if (pr == "open" || pr == "mid") return 0
    if (pr == "unit_start") return (kind_of[p] == "proc") ? 0 : keep(i)
    if (r == "unit_start" && pr == "unit_end") return 1
    if ((frame == "proc" || frame == "block") && decl[p] && !decl[i] && r != "unit_start") return 1
    if (pr == "close" || r == "open") return 1
    return keep(i)
}

function keep(i) { return before[i] > 0 ? 1 : 0 }

{ raw[NR] = $0 }

END {
    # Items: a statement (with its continuation lines) or a comment line.
    n = 0
    blanks = 0
    for (k = 1; k <= NR; k++) {
        line = raw[k]
        t = trim(line)
        if (t == "") { blanks++; continue }
        n++
        before[n] = blanks
        blanks = 0
        first[n] = k
        if (t ~ /^!/) {
            type[n] = "comment"
            last[n] = k
            continue
        }
        type[n] = "stmt"
        joined = trim(code_part(line))
        while (joined ~ /&$/ && k < NR) {
            sub(/&$/, "", joined)
            k++
            # Comment and blank lines inside a continued statement stay put.
            while (k < NR && (trim(raw[k]) == "" || trim(raw[k]) ~ /^!/)) k++
            nxt = trim(code_part(raw[k]))
            sub(/^&/, "", nxt)
            joined = joined " " nxt
        }
        last[n] = k
        stmt[n] = tolower(trim(joined))
    }

    # Roles and frames.
    sp = 0
    for (i = 1; i <= n; i++) {
        if (type[i] != "stmt") continue
        s = stmt[i]
        # A construct name: `name: do`.
        if (s ~ /^[a-z_][a-z0-9_]*[ \t]*:[^:]/) sub(/^[a-z_][a-z0-9_]*[ \t]*:[ \t]*/, "", s)
        r = classify(s)
        role[i] = r
        kind_of[i] = kind
        decl[i] = (r == "simple" && is_declaration(s))
        if (r == "unit_end" || r == "close") {
            if (sp > 0) sp--
            ctx[i] = top_kind()
        } else if (r == "mid") {
            ctx[i] = (sp > 1) ? stack[sp - 1] : "file"
        } else {
            ctx[i] = top_kind()
        }
        if (r == "unit_start" || r == "open") stack[++sp] = kind
    }

    # Output.
    p = 0
    for (i = 1; i <= n; i++) {
        if (type[i] == "comment") {
            # Find the statement this comment group belongs to: the next
            # one, if no blank line comes between.
            j = i
            while (j < n && type[j + 1] == "comment" && before[j + 1] == 0) j++
            if (j < n && type[j + 1] == "stmt" && before[j + 1] == 0) {
                # The group's blank lines count as the statement's.
                before[j + 1] = before[i]
                w = wanted(j + 1, p)
                attached = 1
            } else {
                # A comment on its own (a banner): one blank line after a
                # procedure, else the author's choice.
                w = keep(i)
                if (p > 0 && role[p] == "unit_end") w = 1
                attached = 0
            }
            if (banner_before) w = keep(i)
            banner_before = !attached
            if (i == 1) w = 0
            for (b = 0; b < w; b++) print ""
            for (m = i; m <= j; m++) {
                if (m > i && before[m] > 0) print ""
                for (k = first[m]; k <= last[m]; k++) print raw[k]
            }
            if (attached) {
                i = j + 1
                for (k = first[i]; k <= last[i]; k++) print raw[k]
                p = i
            } else {
                i = j
            }
            continue
        }
        w = banner_before ? (role[i] == "mid" || role[i] == "close" ? 0 : keep(i)) : wanted(i, p)
        banner_before = 0
        if (i == 1) w = 0
        for (b = 0; b < w; b++) print ""
        for (k = first[i]; k <= last[i]; k++) print raw[k]
        p = i
    }
}
