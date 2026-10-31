# fortran-names-lib.awk -- the functions that read and apply the Fortran
# names for Lisp terms (tools/fortran-names), for tools/fortran-names.awk
# and tools/gen-docstrings.sh.
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

# Usage: awk -f tools/fortran-names-lib.awk -f PROGRAM ...
#
# PROGRAM calls load_names(TABLE) once, then:
#   rename(TOK)          TOK, an identifier or word, renamed (protections
#                        aside: the caller decides those);
#   apply_line(LINE)     LINE with every unprotected word renamed;
#   check_file_name(P)   the base name of path P renamed;
#   comment_start(LINE)  where a Fortran comment starts in LINE;
#   whole_file_kept()    whether the current file (FILENAME) is kept.
# Its globals start with fn_.  The format of the table and what is
# protected are described at its top.

function fail(msg) {
    printf "fortran-names: %s\n", msg >"/dev/stderr"
    fn_names_failed = 1
    exit 2
}

# Reads TABLE.
function load_names(table, line, f, n, lineno) {
    fn_cand = ""
    fn_maxparts = 1
    fn_nliterals = 0
    lineno = 0
    while ((getline line <table) > 0) {
        lineno++
        if (line ~ /^#/ || line ~ /^[ \t]*$/) continue
        n = split(line, f, "\t")
        if (f[1] == "term" && n == 3) {
            fn_terms[tolower(f[2])] = f[3]
            fn_terms[tolower(f[2]) "s"] = f[3] "s"
            fn_cand = fn_cand (fn_cand == "" ? "" : "|") tolower(f[2])
        } else if (f[1] == "name" && n == 4) {
            add_scoped_name(f[2], f[3], f[4])
        } else if (f[1] == "name" && n == 3) {
            add_name(f[2], f[3])
        } else if (f[1] == "file" && n == 3) {
            add_name(f[2], f[3])
            if (f[2] ~ /_fm$/ && f[3] ~ /_fm$/) add_name(substr(f[2], 1, length(f[2]) - 3), substr(f[3], 1, length(f[3]) - 3))
        } else if (f[1] == "keep" && n == 2 && f[2] ~ /^".+"$/) {
            fn_literal[++fn_nliterals] = substr(f[2], 2, length(f[2]) - 2)
        } else if (f[1] == "keep" && n == 2 && f[2] ~ /\//) {
            fn_keepfiles[f[2]] = 1
        } else if (f[1] == "keep" && (n == 2 || n == 3)) {
            fn_keeps[tolower(f[2])] = (n == 3 ? " " f[3] " " : "")
        } else {
            fail(table ":" lineno ": not an entry: " line)
        }
    }
    close(table)
    if (lineno == 0) fail(table ": no such table")
}

# A name for the FILES only (paths separated by spaces).
function add_scoped_name(old, new, files, k, m, fl) {
    old = tolower(old)
    m = split(files, fl, " ")
    for (k = 1; k <= m; k++) {
        fn_sfiles[old] = fn_sfiles[old] " " fl[k]
        fn_snames[old, fl[k]] = new
    }
    m = split(old, fn_scratch, "_")
    if (m > fn_maxparts) fn_maxparts = m
    fn_cand = fn_cand (fn_cand == "" ? "" : "|") old
}

# The name KEY has in the current file by a scoped name entry, or "".
function scoped_name(key, k, m, fl) {
    if (!(key in fn_sfiles)) return ""
    m = split(fn_sfiles[key], fl, " ")
    for (k = 1; k <= m; k++) if (is_file(fl[k])) return fn_snames[key, fl[k]]
    return ""
}

# Whether the current file is kept whole.
function whole_file_kept(k) {
    for (k in fn_keepfiles) if (is_file(k)) return 1
    return 0
}

function add_name(old, new, parts) {
    old = tolower(old)
    fn_names[old] = new
    parts = split(old, fn_scratch, "_")
    if (parts > fn_maxparts) fn_maxparts = parts
    fn_cand = fn_cand (fn_cand == "" ? "" : "|") old
}

# NEW in the case of ORIG: all upper (with a plural "s": ALISTs), capitalized
# or as given.
function casefix(orig, new) {
    if (orig ~ /[A-Z]/ && orig !~ /[a-z]/) return toupper(new)
    if (orig ~ /^[A-Z0-9_]*[A-Z][A-Z0-9_]*s$/ && new ~ /s$/) return toupper(substr(new, 1, length(new) - 1)) "s"
    if (orig ~ /^[A-Z]/) return toupper(substr(new, 1, 1)) substr(new, 2)
    return new
}

# TOK (an identifier or word) renamed.  Sets `fn_first' to whether its first
# part changed.
function rename(tok, n, i, j, len, key, out, seg, done, lp, sn) {
    n = split(tok, fn_part, "_")
    out = ""
    fn_first = 0
    i = 1
    while (i <= n) {
        done = 0
        for (len = (fn_maxparts < n - i + 1 ? fn_maxparts : n - i + 1); len >= 1 && !done; len--) {
            seg = fn_part[i]
            for (j = i + 1; j < i + len; j++) seg = seg "_" fn_part[j]
            key = tolower(seg)
            sn = (i == 1 ? scoped_name(key) : "")
            if (sn != "") {
                out = out (i > 1 ? "_" : "") casefix(seg, sn)
                if (i == 1) fn_first = 1
                i += len
                done = 1
            } else if ((key in fn_keeps) && kept(key)) {
                out = out (i > 1 ? "_" : "") seg
                i += len
                done = 1
            } else if ((key in fn_names) && !(seg !~ /[a-z]/ && key ~ /p$/)) {
                # (An upper-case predicate name is a C macro: CHAR_HEAD_P.)
                out = out (i > 1 ? "_" : "") casefix(seg, fn_names[key])
                if (i == 1) fn_first = 1
                i += len
                done = 1
            }
        }
        if (!done) {
            lp = tolower(fn_part[i])
            if (lp in fn_terms) {
                out = out (i > 1 ? "_" : "") casefix(fn_part[i], fn_terms[lp])
                if (i == 1) fn_first = 1
            } else {
                out = out (i > 1 ? "_" : "") fn_part[i]
            }
            i++
        }
    }
    return out
}

# Whether the keep entry KEY applies to the current file: it names no
# file, or names this one (as the end of its path).
function kept(key, n, i, f) {
    if (fn_keeps[key] == "") return 1
    n = split(fn_keeps[key], f, " ")
    for (i = 1; i <= n; i++) {
        if (is_file(f[i])) return 1
    }
    return 0
}

# Whether the current file is PATH (relative to the repository).
function is_file(path) {
    return FILENAME == path || substr(FILENAME, length(FILENAME) - length(path)) == "/" path
}

# Marks in `lit' the characters of LINE that are inside a kept literal.
function mark_literals(line, i, j, p, rest, off) {
    for (i in fn_lit) delete fn_lit[i]
    for (i = 1; i <= fn_nliterals; i++) {
        rest = line
        off = 0
        while ((p = index(rest, fn_literal[i])) > 0) {
            for (j = off + p; j < off + p + length(fn_literal[i]); j++) fn_lit[j] = 1
            off += p
            rest = substr(rest, p + 1)
        }
    }
}

# Whether the word TOK, between BEFORE and AFTER, is an upstream name.
function protected(before, after, tok) {
    # Hyphenated: forward-sexp, auto-mode-alist.
    if (before ~ /[A-Za-z0-9]-+$/ || after ~ /^-[A-Za-z]/) return 1
    # A citation: lisp.el:beginning-of-defun, syntax.c:scan_lists.
    if (before ~ /[A-Za-z0-9]\.(el|c|h):$/) return 1
    # A C function, variable or symbol: Fscan_lists, Vauto_mode_alist.
    if (tok ~ /^[FVQ][a-z0-9]*_/) return 1
    # Marked: upstream's `scan_lists'.
    if (tolower(before) ~ /(^|[^a-z])upstream('s)? [`'"]?$/) return 1
    return 0
}

# LINE with the table applied (to all of it).  Counts the renames in
# `fn_nrenamed' and puts the first one in `fn_what'.
function apply_line(line, out, rest, tok, new, before, pos) {
    fn_nrenamed = 0
    if (tolower(line) !~ fn_cand) return line
    mark_literals(line)
    out = ""
    rest = line
    pos = 0
    while (match(rest, /[A-Za-z0-9_]+/)) {
        tok = substr(rest, RSTART, RLENGTH)
        before = out substr(rest, 1, RSTART - 1)
        rest = substr(rest, RSTART + RLENGTH)
        pos += RSTART
        new = tok
        if (!((pos in fn_lit) || protected(before, rest, tok))) new = rename(tok)
        pos += RLENGTH - 1
        if (new != tok) {
            if (fn_nrenamed++ == 0) fn_what = tok " -> " new
            # Fix the article: "a sexp" -> "an expression".
            if (fn_first && (before ~ /(^|[^A-Za-z0-9_])[Aa] $/) && new ~ /^[AEIOUaeiou]/) {
                before = substr(before, 1, length(before) - 1) "n "
            } else if (fn_first && (before ~ /(^|[^A-Za-z0-9_])[Aa]n $/) && new !~ /^[AEIOUaeiou]/) {
                before = substr(before, 1, length(before) - 2) " "
            }
        }
        out = before new
    }
    return out rest
}

# The index of LINE's comment character `!' (0 for none), outside strings.
function comment_start(line, i, c, q) {
    q = ""
    for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)
        if (q != "") {
            if (c == q) q = ""
        } else if (c == "\"" || c == "'") {
            q = c
        } else if (c == "!") {
            return i
        }
    }
    return 0
}

function check_file_name(path, base, stem, ext) {
    base = path
    sub(/.*\//, "", base)
    stem = base
    ext = ""
    if (match(stem, /\.[A-Za-z0-9]+$/)) {
        ext = substr(stem, RSTART)
        stem = substr(stem, 1, RSTART - 1)
    }
    return rename(stem) ext
}
