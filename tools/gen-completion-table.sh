#!/bin/sh
# gen-completion-table.sh -- generate src/fortran_completion_table_mod.f90,
# the configuration API's derived types for fortran_completion_mode.
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

# The configuration API is what src/fortmacs.f90 gathers with its `use
# MODULE, only: NAMES' statements.  This script follows each name to the
# module that declares it (through the modules' own `use ..., only:'
# lists), keeps the variables declared with a derived type and the
# derived types themselves, then every type those reach: parent types
# (`extends') and the types of components.  It writes, for each type, its
# public components (with their types) and type-bound procedures, and for
# each variable its type, as calls that fill a `declarations_type'
# (fortran_declarations_mod) when fortran_completion_mode first needs them.
# Private components and bindings are left out: init.f90 cannot reach
# them.  It reads the generated src/commands_mod.f90 and
# src/face_table_mod.f90, so tools/build.sh runs it after their
# generators.
#
# Usage: tools/gen-completion-table.sh [OUTPUT]
#   The default output is src/fortran_completion_table_mod.f90.  It is
#   formatted with fprettify, as tools/check.sh requires.

set -eu

cd "$(dirname "$0")/.."
out=${1:-src/fortran_completion_table_mod.f90}
tmp=${TMPDIR:-/tmp}/gen-completion-table.$$
trap 'rm -f "$tmp".*' EXIT

# Pass 1: the records, one per line, fields separated by tabs:
#   D  module  type  parent
#   T  module  type  1  component  component-type
#   T  module  type  2  binding
#   V  module  variable  type
#   U  module  local-name  from-module  name-there
find src -name '*.f90' ! -name docstrings_mod.f90 ! -name fortran_completion_table_mod.f90 | sort |
    xargs awk '
function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }

# S without its comment (a "!" outside a character literal).
function strip_comment(s,    i, c, q) {
    q = ""
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (q != "") { if (c == q) q = "" }
        else if (c == "\"" || c == "'\''") q = c
        else if (c == "!") return substr(s, 1, i - 1)
    }
    return s
}

# The name S begins with ("" if none).
function leading_name(s) {
    if (match(s, /^[a-z][a-z0-9_]*/)) return substr(s, 1, RLENGTH)
    return ""
}

# The index of P in S outside parentheses and literals, 0 if none.
function top_index(s, p,    i, c, q, d) {
    q = ""; d = 0
    for (i = 1; i <= length(s) - length(p) + 1; i++) {
        c = substr(s, i, 1)
        if (q != "") { if (c == q) q = "" }
        else if (c == "\"" || c == "'\''") q = c
        else if (c == "(") d++
        else if (c == ")") d--
        else if (d == 0 && substr(s, i, length(p)) == p) return i
    }
    return 0
}

# The leading names of the comma-separated items of S, into the array
# out; returns their number.
function split_names(s, out,    i, c, q, d, first, n, item) {
    q = ""; d = 0; first = 1; n = 0
    for (i = 1; i <= length(s) + 1; i++) {
        c = (i <= length(s)) ? substr(s, i, 1) : ","
        if (q != "") { if (c == q) q = ""; continue }
        if (c == "\"" || c == "'\''") { q = c; continue }
        if (c == "(" || c == "[") { d++; continue }
        if (c == ")" || c == "]") { d--; continue }
        if (c != "," || d > 0) continue
        item = leading_name(trim(substr(s, first, i - first)))
        first = i + 1
        if (item != "") out[++n] = item
    }
    return n
}

# The derived type of the declaration S ("" for none); sets is_declaration.
function declared_type(s,    w, rest, d, i, c) {
    is_declaration = 0
    if (top_index(s, "::") == 0) return ""
    w = leading_name(s)
    rest = trim(substr(s, length(w) + 1))
    if (w == "type" || w == "class") {
        if (substr(rest, 1, 1) != "(") return ""
        is_declaration = 1
        return leading_name(trim(substr(rest, 2)))
    }
    if (w ~ /^(integer|real|logical|character|complex|double|doubleprecision)$/ &&
        (w == "double" || substr(rest, 1, 1) ~ /[(,:*]/)) is_declaration = 1
    return ""
}

# Whether the attributes before "::" in S make the entity private,
# public, or neither (the default of its part).
function access(s,    before) {
    before = substr(s, 1, top_index(s, "::") - 1)
    if (before ~ /,[ \t]*private[ \t]*(,|$)/) return "private"
    if (before ~ /,[ \t]*public[ \t]*(,|$)/) return "public"
    return ""
}

FNR == 1 { module = ""; contains = 0; type = ""; statement = ""; continued = 0 }

{
    line = tolower($0)
    line = trim(strip_comment(line))
    if (continued) sub(/^&/, "", line)
    else statement = ""
    continued = 0
    if (line ~ /&$/) { sub(/&$/, "", line); continued = 1 }
    statement = statement line
    if (continued) next
    s = trim(statement)
    if (s == "") next
    w = leading_name(s)
    rest = trim(substr(s, length(w) + 1))
    second = leading_name(rest)

    if (w == "module" && second != "" && rest == second) { module = second; contains = 0; next }
    if (w == "submodule" || w == "program") { module = ""; next }
    if (module == "") next
    if (s ~ /^end[ \t]*module/) { module = ""; next }

    if (type != "") {
        if (s ~ /^end[ \t]*type/) { type = ""; next }
        if (s == "contains") { binding_part = 1; next }
        if (s == "private") {
            if (binding_part) binding_default = "private"; else component_default = "private"
            next
        }
        if (!binding_part) {
            if (w == "procedure") { is_declaration = top_index(s, "::") > 0; t = "" }
            else t = declared_type(s)
            if (!is_declaration) next
            a = access(s)
            if (a == "") a = component_default
            if (a == "private") next
            n = split_names(substr(s, top_index(s, "::") + 2), names)
            for (i = 1; i <= n; i++) print "T\t" type "\t1\t" names[i] "\t" t
            next
        }
        if (w == "procedure" || w == "generic") {
            a = (top_index(s, "::") > 0) ? access(s) : ""
            if (a == "") a = binding_default
            if (a == "private") next
            if (top_index(s, "::") > 0) list = substr(s, top_index(s, "::") + 2)
            else { list = rest; if (substr(list, 1, 1) == "(") sub(/^\([^)]*\)/, "", list) }
            if (w == "generic") {
                sub(/=>.*/, "", list)
                if (list ~ /\(/) next
                print "T\t" type "\t2\t" leading_name(trim(list))
                next
            }
            n = split_names(list, names)
            for (i = 1; i <= n; i++) print "T\t" type "\t2\t" names[i]
        }
        next
    }

    if (w == "type" && rest != "" && substr(rest, 1, 1) != "(" && !(second == "is" && rest ~ /\(/)) {
        k = top_index(s, "::")
        if (k > 0) { attributes = substr(s, 5, k - 5); type = leading_name(trim(substr(s, k + 2))) }
        else { attributes = ""; type = leading_name(rest) }
        parent = ""
        if (match(attributes, /extends[ \t]*\([ \t]*[a-z][a-z0-9_]*/)) {
            parent = substr(attributes, RSTART, RLENGTH)
            sub(/^extends[ \t]*\([ \t]*/, "", parent)
        }
        binding_part = 0; component_default = ""; binding_default = ""
        if (type == "") next
        print "D\t" module "\t" type "\t" parent
        type = module "\t" type
        next
    }

    if (s == "contains") { contains = 1; next }
    if (contains) next
    if (w ~ /^(abstract)?$/ && second == "interface" || w == "interface") { in_interface = 1; next }
    if (s ~ /^end[ \t]*interface/) { in_interface = 0; next }
    if (in_interface) next

    if (w == "use") {
        if (!match(s, /[ \t,]only[ \t]*:/)) next
        k = RSTART + 1
        from = rest
        sub(/^,[^:]*::/, "", from)
        sub(/^::/, "", from)
        from = leading_name(trim(from))
        list = substr(s, k + 4)
        sub(/^[ \t]*:/, "", list)
        n = split(list, items, ",")
        for (i = 1; i <= n; i++) {
            item = trim(items[i])
            if (item == "") continue
            if (index(item, "=>")) {
                local = leading_name(trim(substr(item, 1, index(item, "=>") - 1)))
                remote = leading_name(trim(substr(item, index(item, "=>") + 2)))
            } else local = remote = leading_name(item)
            if (local != "") print "U\t" module "\t" local "\t" from "\t" remote
        }
        next
    }

    if (w == "type" || w == "class") {
        t = declared_type(s)
        if (!is_declaration || t == "") next
        n = split_names(substr(s, top_index(s, "::") + 2), names)
        for (i = 1; i <= n; i++) print "V\t" module "\t" names[i] "\t" t
    }
}' > "$tmp.raw"

# Pass 2 writes the records of the output, fields separated by tabs:
#   T  type  0  parent
#   T  type  1  component  component-type
#   T  type  2  binding
#   V  variable  type
# It resolves the configuration API from the records of pass 1, read in
# sorted order so that the output does not depend on the order of the files.
LC_ALL=C sort -u "$tmp.raw" > "$tmp.sorted"
awk -F '\t' '
# The module that declares NAME as seen from MODULE (following use lists).
function source(module, name,    depth, key) {
    for (depth = 0; depth < 20; depth++) {
        key = module SUBSEP name
        if ((key in variable_type) || (key in defined)) return module
        if (!(key in used_from)) return ""
        split(used_from[key], parts, SUBSEP)
        module = parts[1]; name = parts[2]
    }
    return ""
}

# The name NAME has in the module source() returned (after renames).
function source_name(module, name,    depth, key) {
    for (depth = 0; depth < 20; depth++) {
        key = module SUBSEP name
        if ((key in variable_type) || (key in defined)) return name
        if (!(key in used_from)) return ""
        split(used_from[key], parts, SUBSEP)
        module = parts[1]; name = parts[2]
    }
    return ""
}

# The module whose definition of type NAME a reference from MODULE means.
function type_module(module, name,    m) {
    if ((module SUBSEP name) in defined) return module
    m = source(module, name)
    if (m != "" && ((m SUBSEP name) in defined)) return m
    if (name in first_module) return first_module[name]
    return ""
}

function reach(module, name,    key) {
    if (name == "") return
    module = type_module(module, name)
    if (module == "") return
    key = module SUBSEP name
    if (key in reached) return
    if (name in reached_name) return
    reached[key] = 1
    reached_name[name] = 1
    queue[++queued] = key
}

$1 == "D" { defined[$2 SUBSEP $3] = $4; if (!($3 in first_module)) first_module[$3] = $2; next }
$1 == "T" && $4 == 1 { components[$2 SUBSEP $3] = components[$2 SUBSEP $3] $5 SUBSEP $6 "\n"; next }
$1 == "T" && $4 == 2 { bindings[$2 SUBSEP $3] = bindings[$2 SUBSEP $3] $5 "\n"; next }
$1 == "V" { variable_type[$2 SUBSEP $3] = $4; next }
$1 == "U" {
    used_from[$2 SUBSEP $3] = $4 SUBSEP $5
    if ($2 == "fortmacs") gathered[++gathered_count] = $3
    next
}

END {
    # The names src/fortmacs.f90 gathers.
    for (g = 1; g <= gathered_count; g++) {
        m = source("fortmacs", gathered[g])
        if (m == "") continue
        n = source_name("fortmacs", gathered[g])
        if ((m SUBSEP n) in variable_type) {
            t = variable_type[m SUBSEP n]
            if (type_module(m, t) == "") continue
            print "V\t" gathered[g] "\t" t
            reach(m, t)
        } else if ((m SUBSEP n) in defined) reach(m, n)
    }

    for (q = 1; q <= queued; q++) {
        split(queue[q], parts, SUBSEP)
        m = parts[1]; t = parts[2]
        print "T\t" t "\t0\t" defined[queue[q]]
        reach(m, defined[queue[q]])
        c = split(components[queue[q]], lines, "\n")
        for (i = 1; i < c; i++) {
            split(lines[i], fields, SUBSEP)
            print "T\t" t "\t1\t" fields[1] "\t" fields[2]
            reach(m, fields[2])
        }
        b = split(bindings[queue[q]], lines, "\n")
        for (i = 1; i < b; i++) print "T\t" t "\t2\t" lines[i]
    }
}' "$tmp.sorted" | LC_ALL=C sort -u -t "$(printf '\t')" -k1,1r -k2,2 -k3,3n -k4,4 > "$tmp.records"

{
cat <<'EOF'
! fortran_completion_table_mod.f90 -- the configuration API's derived types.
! Generated by tools/gen-completion-table.sh from src/fortmacs.f90 and the
! modules it uses; do not edit.
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

!> The derived types of the configuration API, for
!> `fortran_completion_mode': the objects src/fortmacs.f90 gathers that
!> are declared with a derived type, the types it gathers, and every type
!> those reach (parent types and the types of components), with their
!> public components and type-bound procedures.  Generated by
!> tools/gen-completion-table.sh.
module fortran_completion_table_mod
    use fortran_declarations_mod, only: declarations_type
    implicit none(type, external)
    private

    public :: load_configuration_declarations

contains

    !> Add the configuration API's derived types and the objects declared
    !> with them to DECLARATIONS.  Not a port.
    subroutine load_configuration_declarations(declarations)
        type(declarations_type), intent(inout) :: declarations

EOF
awk -F '\t' '
# A call longer than 132 columns continues its second argument under the first.
function emit(procedure, first, second,   head, line) {
    head = "        call declarations%" procedure "("
    line = head "\"" first "\", \"" second "\")"
    if (length(line) <= 132) { print line; return }
    print head "\"" first "\", &"
    printf "%" length(head) "s\"%s\")\n", "", second
}
$1 == "T" && $3 == 0 { emit("append_type", $2, $4) }
$1 == "T" && $3 == 1 { emit("append_component", $4, $5) }
$1 == "T" && $3 == 2 { printf "        call declarations%%append_binding(\"%s\")\n", $4 }
$1 == "V" { emit("append_variable", $2, $3) }
' "$tmp.records"
cat <<'EOF'
    end subroutine load_configuration_declarations
end module fortran_completion_table_mod
EOF
} > "$tmp.f90"

if command -v fprettify >/dev/null 2>&1; then
    fprettify --config-file .fprettify.rc "$tmp.f90" 2>/dev/null
fi
awk -f tools/blank-lines.awk "$tmp.f90" >"$tmp.bl" && mv "$tmp.bl" "$tmp.f90"
mv "$tmp.f90" "$out"
