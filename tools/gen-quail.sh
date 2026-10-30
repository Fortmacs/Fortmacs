#!/bin/sh
# gen-quail.sh -- generate the built-in Quail input methods from the
# leim/quail files of GNU Emacs.
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

# Usage: tools/gen-quail.sh [UPSTREAM-DIR]
#
# Each file lisp/leim/quail/FILE.el in FILES becomes src/FILE_fm.f90
# (hyphens as underscores): a module whose procedure load_FILE defines
# the file's Quail packages with quail_fm (`quail_define_package', a
# `quail_map_type' built with `%add' for each rule of quail-define-rules,
# `quail_install_map'), as loading the file does upstream.  It also
# writes src/leim_list_fm.f90, the registration of those input methods
# that upstream's leim-list.el (made by quail-update-leim-list-file)
# does: register-input-method with each package's name, language, title
# and the first line of its docstring, the files in the order of
# directory-files.  The files are formatted with fprettify and
# tools/blank-lines.awk, as tools/check.sh requires.
#
# The forms understood are those the files use: quail-define-package
# with a string or nil docstring, GUIDANCE t or nil, TRANSLATION-KEYS
# nil or a quoted list of (KEY . COMMAND), the flags as t or nil and no
# decode map, overlay plist, update function or conversion keys;
# quail-define-rules without annotations, each rule a key string and a
# character, a string or a vector of characters or strings; and the
# backwards-compatible copy of a package under another name that
# latin-post.el pushes on quail-package-alist.  Anything else stops the
# script.

set -eu

cd "$(dirname "$0")/.."
upstream=${1:-upstream/emacs}
files="latin-post latin-pre"
tmp=${TMPDIR:-/tmp}/gen-quail.$$
trap 'rm -f "$tmp".*' EXIT

: >"$tmp.leim"

for f in $files; do
    src=$upstream/lisp/leim/quail/$f.el
    mod=$(printf '%s' "$f" | tr '-' '_')
    out=src/${mod}_fm.f90
    LC_ALL=C awk -v file="$f" -v mod="$mod" -v leim="$tmp.leim" '
function fail(msg) {
    printf "gen-quail.sh: %s.el: %s (token %d: %s %s, after %s %s)\n", file, msg, p, ttype[p], tval[p], ttype[p - 1], tval[p - 1] > "/dev/stderr"
    exit 1
}

# The number of bytes of the UTF-8 sequence starting with byte C.
function seqlen(c,    b) {
    b = ord[c]
    if (b < 128) return 1
    if (b >= 240) return 4
    if (b >= 224) return 3
    return 2
}

# The UTF-8 bytes of the character code C.
function utf8(c) {
    if (c < 128) return sprintf("%c", c)
    if (c < 2048) return sprintf("%c%c", 192 + int(c / 64), 128 + c % 64)
    if (c < 65536) return sprintf("%c%c%c", 224 + int(c / 4096), 128 + int(c / 64) % 64, 128 + c % 64)
    return sprintf("%c%c%c%c", 240 + int(c / 262144), 128 + int(c / 4096) % 64, 128 + int(c / 64) % 64, 128 + c % 64)
}

# The value of the hexadecimal digits S.
function hex(s,    v, k) {
    v = 0
    for (k = 1; k <= length(s); k++) v = v * 16 + index("0123456789abcdef", tolower(substr(s, k, 1))) - 1
    return v
}

# A Fortran character literal for the bytes S.
function flit(s) {
    gsub(/"/, "\"\"", s)
    return "\"" s "\""
}

# Lisp names as Fortran names.
function fname(s) {
    gsub(/-/, "_", s)
    return s
}

# Split the source into tokens: ttype[] is one of ( ) [ ] QUOTE DOT STR
# CHR SYM and tval[] the decoded text of STR, CHR and SYM.
function tokenize(    i, n, c, d, s, k, l) {
    n = length(text)
    i = 1
    ntok = 0
    while (i <= n) {
        c = substr(text, i, 1)
        if (c == " " || c == "\t" || c == "\n" || c == "\f") { i++; continue }
        if (c == ";") {
            while (i <= n && substr(text, i, 1) != "\n") i++
            continue
        }
        if (c == "(" || c == ")" || c == "[" || c == "]") {
            ttype[++ntok] = c; i++; continue
        }
        if (c == "'"'"'") { ttype[++ntok] = "QUOTE"; i++; continue }
        if (c == "\"") {
            s = ""
            i++
            while (1) {
                if (i > n) fail("unterminated string")
                c = substr(text, i, 1)
                if (c == "\"") { i++; break }
                if (c == "\\") {
                    d = substr(text, i + 1, 1)
                    i += 2
                    if (d == "\n") continue
                    if (d == "n") { s = s "\n"; continue }
                    if (d == "t") { s = s "\t"; continue }
                    if (d == "C" && substr(text, i, 1) == "-") {
                        d = substr(text, i + 1, 1)
                        i += 2
                        if (d == "?") s = s sprintf("%c", 127)
                        else s = s sprintf("%c", ord[toupper(d)] % 32)
                        continue
                    }
                    s = s d
                    continue
                }
                s = s c
                i++
            }
            ttype[++ntok] = "STR"; tval[ntok] = s
            continue
        }
        if (c == "?") {
            i++
            if (substr(text, i, 1) == "\\") i++
            l = seqlen(substr(text, i, 1))
            ttype[++ntok] = "CHR"; tval[ntok] = substr(text, i, l)
            i += l
            continue
        }
        s = ""
        while (i <= n) {
            c = substr(text, i, 1)
            if (index(" \t\n\f()[]\";'"'"'", c) > 0) break
            s = s c
            i++
        }
        if (s == ".") ttype[++ntok] = "DOT"
        else if (s ~ /^#x[0-9a-fA-F]+$/) { ttype[++ntok] = "CHR"; tval[ntok] = utf8(hex(substr(s, 3))) }
        else { ttype[++ntok] = "SYM"; tval[ntok] = s }
    }
}

function expect(type, what) {
    if (ttype[p] != type) fail("expected " what " at token " p " (" ttype[p] " " tval[p] ")")
    p++
}

# Skip the form starting at token P.
function skip(    depth) {
    depth = 0
    do {
        if (ttype[p] == "(" || ttype[p] == "[") depth++
        else if (ttype[p] == ")" || ttype[p] == "]") depth--
        p++
    } while (depth > 0)
}

# A key string as key_parse reads it.
function keydesc(s,    out, i, c, b) {
    out = ""
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        b = ord[c]
        if (out != "") out = out " "
        if (b == 127) out = out "DEL"
        else if (b < 32) out = out "C-" tolower(sprintf("%c", b + 64))
        else if (c == " ") out = out "SPC"
        else out = out c
    }
    return out
}

# Emit the statements that append the docstring S to `doc`.
function emit_doc(s,    nl, lines, k, line, chunk, j) {
    print "        doc = \"\""
    nl = split(s, lines, "\n")
    for (k = 1; k <= nl; k++) {
        line = lines[k]
        while (length(line) > 95) {
            j = 95
            while (j > 1 && ord[substr(line, j + 1, 1)] >= 128 && ord[substr(line, j + 1, 1)] < 192) j--
            chunk = substr(line, 1, j)
            line = substr(line, j + 1)
            print "        doc = doc//" flit(chunk)
        }
        if (k < nl && line == "") print "        doc = doc//LF"
        else if (k < nl) print "        doc = doc//" flit(line) "//LF"
        else if (line != "") print "        doc = doc//" flit(line)
    }
}

function flag(i) {
    if (i > nargs) return ".false."
    if (atype[i] != "SYM" || (aval[i] != "t" && aval[i] != "nil")) fail("argument " i " of " name " is not t or nil")
    return aval[i] == "t" ? ".true." : ".false."
}

function define_package(    i, k, keys, first, docline) {
    p++
    nargs = 0
    while (ttype[p] != ")") {
        nargs++
        if (ttype[p] == "QUOTE") {
            p++
            atype[nargs] = "LIST"
            aval[nargs] = p
            skip()
        } else {
            atype[nargs] = ttype[p]
            aval[nargs] = tval[p]
            p++
        }
    }
    p++
    if (nargs < 3) fail("quail-define-package with too few arguments")
    name = aval[1]
    for (i = 11; i <= 15; i++)
        if (i <= nargs && !(atype[i] == "SYM" && aval[i] == "nil")) fail("argument " i " of " name " is not ported")
    docline = ""
    if (nargs >= 5 && atype[5] == "STR") {
        emit_doc(aval[5])
        docline = aval[5]
        sub(/\n.*/, "", docline)
    } else {
        print "        doc = \"\""
    }
    printf "        call quail_define_package(%s, %s, %s, guidance=%s, docstring=doc", flit(aval[1]), flit(aval[2]), flit(aval[3]), flag(4)
    if (nargs >= 6 && atype[6] == "LIST") {
        printf ", &\n                                  translation_keys=["
        k = aval[6]
        if (ttype[k] != "(") fail("TRANSLATION-KEYS of " name)
        k++
        first = 1
        while (ttype[k] == "(") {
            k++
            if (ttype[k] == "STR") { keys = keydesc(tval[k]); k++ }
            else if (ttype[k] == "[" && ttype[k + 1] == "SYM" && ttype[k + 2] == "]") { keys = "<" tval[k + 1] ">"; k += 3 }
            else fail("a key of TRANSLATION-KEYS of " name)
            if (ttype[k] != "DOT" || ttype[k + 1] != "SYM" || ttype[k + 2] != ")") fail("a binding of TRANSLATION-KEYS of " name)
            printf "%s &\n                                  quail_translation_key_type(key=%s, command=%s)", first ? "" : ",", flit(keys), flit(fname(tval[k + 1]))
            first = 0
            k += 3
        }
        printf "]"
    } else if (nargs >= 6 && !(atype[6] == "SYM" && aval[6] == "nil")) {
        fail("TRANSLATION-KEYS of " name)
    }
    printf ", &\n                                  forget_last_selection=%s, deterministic=%s, kbd_translate=%s, &\n", flag(7), flag(8), flag(9)
    printf "                                  show_layout=%s, maximum_shortest=%s, simple=%s)\n", flag(10), flag(12), flag(16)
    printf "call register_quail_input_method(%s, %s, %s, &\n                                 %s, &\n                                 load_%s)\n", flit(aval[1]), flit(aval[2]), flit(aval[3]), flit(docline), mod >> leim
    npackages++
}

function define_rules(    key, s, k) {
    p++
    print "        map = quail_map_type()"
    while (ttype[p] != ")") {
        expect("(", "a rule")
        if (ttype[p] != "STR") fail("a rule without a key string (annotations are not ported)")
        key = tval[p]
        p++
        if (ttype[p] == "CHR" || ttype[p] == "STR") {
            print "        call map%add(" flit(key) ", " flit(tval[p]) ")"
            p++
        } else if (ttype[p] == "[") {
            p++
            s = ""
            k = 0
            # A vector of characters is the string of its characters (the
            # decode map makes both a vector of one-character strings).
            while (ttype[p] == "CHR") { s = s tval[p]; p++; k++ }
            if (ttype[p] == "]") {
                print "        call map%add(" flit(key) ", " flit(s) ")"
            } else {
                if (k > 0) fail("a vector of characters and strings")
                s = ""
                while (ttype[p] == "STR") {
                    if (tval[p] ~ / $/ || tval[p] == "") fail("a candidate ending in a blank")
                    s = s (s == "" ? "" : ", ") flit(tval[p])
                    p++
                }
                if (ttype[p] != "]") fail("a vector of other objects")
                print "        call map%add(" flit(key) ", [character(len=" maxlen(p) ") :: " s "])"
            }
            p++
        } else {
            fail("a translation that is not a character, a string or a vector")
        }
        expect(")", "the end of a rule")
    }
    p++
    print "        call quail_install_map(map, status)"
    print "        if (.not. status%ok()) return"
}

# The longest string of the vector ending before token E.
function maxlen(e,    k, m) {
    m = 1
    for (k = e - 1; ttype[k] == "STR"; k--) if (length(tval[k]) > m) m = length(tval[k])
    return m
}

function copy_package() {
    # (push (cons NEW (cdr (assoc OLD quail-package-alist))) quail-package-alist)
    if (ttype[p + 1] != "(" || tval[p + 2] != "cons" || ttype[p + 3] != "STR" || ttype[p + 4] != "(" || tval[p + 5] != "cdr" || \
        ttype[p + 6] != "(" || tval[p + 7] != "assoc" || ttype[p + 8] != "STR" || tval[p + 9] != "quail-package-alist")
        fail("an unknown push")
    print "        call quail_copy_package(" flit(tval[p + 3]) ", " flit(tval[p + 8]) ")"
    p = p - 1
    skip()
}

BEGIN {
    for (i = 0; i < 256; i++) ord[sprintf("%c", i)] = i
}
{
    lines[NR] = $0
    text = text $0 "\n"
}
END {
    tokenize()
    print "! " mod "_fm.f90 -- the Quail packages of " file ".el."
    print "! Generated by tools/gen-quail.sh from lisp/leim/quail/" file ".el; do not edit."
    print "!"
    for (i = 3; i <= NR && lines[i] != ""; i++) {
        l = lines[i]
        sub(/^;;/, "!", l)
        print l
    }
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
    print "!> The Quail packages of leim/quail/" file ".el, defined by `load_" mod "`."
    print "module " mod "_fm"
    print "    use error_fm, only: status_type"
    print "    use quail_fm, only: quail_define_package, quail_map_type, quail_install_map, quail_translation_key_type, &"
    print "                        quail_copy_package"
    print "    implicit none(type, external)"
    print "    private"
    print ""
    print "    public :: load_" mod
    print ""
    print "contains"
    print ""
    print "    !> Define the Quail packages of " file ".el, as loading the file does"
    print "    !> upstream.  Port of leim/quail/" file ".el."
    print "    subroutine load_" mod "(status)"
    print "        type(status_type), intent(out) :: status"
    print "        character(len=*), parameter :: LF = new_line(\"a\")"
    print "        type(quail_map_type) :: map"
    print "        character(len=:), allocatable :: doc"
    print ""
    print "        status = status_type()"
    p = 1
    while (p <= ntok) {
        expect("(", "a form")
        if (ttype[p] != "SYM") fail("a form not starting with a symbol")
        if (tval[p] == "require") { p--; skip(); continue }
        if (tval[p] == "quail-define-package") { define_package(); continue }
        if (tval[p] == "quail-define-rules") { define_rules(); continue }
        if (tval[p] == "push") { copy_package(); continue }
        fail("an unknown form " tval[p])
    }
    print "    end subroutine load_" mod
    print "end module " mod "_fm"
}' "$src" >"$tmp.out"
    fprettify --config-file .fprettify.rc --silent "$tmp.out" 2>/dev/null || fprettify --config-file .fprettify.rc "$tmp.out"
    awk -f tools/blank-lines.awk "$tmp.out" >"$out"
done

{
cat <<'EOF'
! leim_list_fm.f90 -- the built-in input methods.
! Generated by tools/gen-quail.sh from lisp/leim/quail; do not edit.
!
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> The input methods that come with Fortmacs, registered as upstream's
!> leim-list.el (generated by quail-update-leim-list-file) registers them.
module leim_list_fm
EOF
for f in $files; do
    mod=$(printf '%s' "$f" | tr '-' '_')
    echo "    use ${mod}_fm, only: load_${mod}"
done
cat <<'EOF'
    use quail_fm, only: register_quail_input_method
    implicit none(type, external)
    private

    public :: load_leim_list

contains

    !> Register the built-in Quail input methods: the forms of leim-list.el.
    !> Port of leim-list.el (the Quail part).
    subroutine load_leim_list()
EOF
sed 's/^/        /' "$tmp.leim"
cat <<'EOF'
    end subroutine load_leim_list
end module leim_list_fm
EOF
} >"$tmp.leimf90"
fprettify --config-file .fprettify.rc --silent "$tmp.leimf90" 2>/dev/null || fprettify --config-file .fprettify.rc "$tmp.leimf90"
awk -f tools/blank-lines.awk "$tmp.leimf90" >src/leim_list_fm.f90
