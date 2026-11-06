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
#                defvar_false, prin1_to_string, prin1_string, cons_repr,
#                cons_arg).
#   predicate    a procedure named like a Lisp predicate: ending in `_p'
#                (region_active_p), or in `p' after a type word
#                (charsetp, processp, bobp).  Name it is_* or has_*.
#   constant     a named constant ending in _NIL, _T, _TRUE or _FALSE,
#                or in _DISABLED, _NEVER, _OFF, _ON or _NO_LIMIT.  Name
#                the states for their meaning with the vocabulary _NO,
#                _YES, _NONE, _ASK, _ALWAYS and _UNLIMITED (R2-Q16,
#                R2-Q17); two states make a logical.
#   yes-no       a named constant ending in _YES or _NO (R6-38): a
#                setting with more than two states is a choice setting
#                (data_mod's choice_setting_type), whose `=' takes .true.
#                and .false. for its on and off states; only its other
#                states are named constants.  The baseline lists the
#                internal enumerations (arguments, return codes, record
#                fields) and the settings not converted yet.  A name
#                ending in _YES_OR_NO or _Y_OR_N names an upstream
#                function (yes-or-no-p, y-or-n-p; R6-37), not a state,
#                and is not reported.
#   vector       a name (variable, component, named constant, type or
#                procedure) ending in `_vector'.  Use a plural.
#   interactive  a component `interactive' of command_type (it is
#                `is_command').
#   verb         a procedure, procedure pointer or type-bound procedure
#                named `run', `run_*' or `call_*': execute_ is the only
#                verb for running something (`execute_with_timer',
#                `commands%paste%execute'), and a procedure pointer is
#                called directly, with no call_* wrapper (PORTING.md,
#                "One word per meaning").
#   command-state  the command loop's state declared as a `value_type'
#                (A7): this_command, last_command, real_this_command,
#                real_last_command, last_repeatable_command and
#                current_minibuffer_command are `type(command_type),
#                pointer'; prefix_argument, current_prefix_argument and
#                last_prefix_argument are `type(prefix_argument_type)'.
#   abbreviation a name with the part `proc' or `procs' (R2-Q10): spell
#                out process, or name the procedure for its job.
#   interface    an abstract interface whose name does not end in
#                `_procedure' (R2-Q10, R2-Q19).
#   type         a derived type whose name does not end in `_type'
#                (R2-Q19).
#   underscores  a name with a double underscore (R2-Q20): upstream's
#                private marker; a suffix names the role where the plain
#                name is taken (smie_parent_variable).
#   regexp       a name ending in `_re' or `_regex': it is `_regexp'
#                (V3a).
#   test         a name with the part `predicate', `predicates', `pred'
#                or `preds' (R2-Q10): a passed logical procedure is a
#                test, a setting holding one X_test_function, and an
#                array is filtered with a logical mask.
#   write-wrapper  a function or subroutine whose executable part is a
#                `write' into a local buffer followed by `trim' of it
#                (`write (buffer, "(i0)") n' then `text = trim(buffer)'),
#                or one `write' into its result (R6-44): numbers become
#                text with an internal write where they are used, never
#                through a helper.
#   number-text  a reference to a procedure named number_to_string,
#                int_text, integer_text, itoa, int_str, int2str or
#                to_string (R6-44, R6-45): fixed-format text is one
#                internal `write' with edit descriptors.
# fortran-names: off
#   percent-format  a call of format_text, styled_format or format_message,
#                or a use of format_arguments_type: Emacs's %-directive
#                formats (%s, %d, %S) are never used; text built from a
#                format in the source is one internal `write' with edit
#                descriptors, or a concatenation of strings (owner,
#                2026-11-06); a format the user types is a Fortran
#                format (editing_functions_mod's write_with_format).
# fortran-names: on
#   char-loop    a `do K = 1, min(len(A), len(B))' loop that orders the
#                characters `A(K:K)' and `B(K:K)' with `<' or `>' (also
#                through ichar or iachar): string-lessp by hand.  utility_functions_mod's
#                is_string_less is its port (`<' pads the shorter string
#                with blanks) (R6-45).
#   blank-skip   a `do while' whose body is only `if (S(K:K) /= C1 .and.
#                S(K:K) /= C2 ...) exit', each C a character literal,
#                constant or achar(N), and `K = K + 1' (or `- 1'): it is
#                `verify(S, C1//C2...)' (with `back=.true.'), 0 meaning
#                all blank (R6-45).
#   constructor-wrapper  a function whose one executable statement
#                assigns a structure constructor of its dummies to the
#                result (`v = value_type(text=s)'): write the constructor
#                where it is used (R6-45).
#   duplicate-helper  a definition of is_string_less, is_string_prefix,
#                string_to_number, int_text, string_trim, version_less,
#                version_less_or_equal (or version_le, is_version_le,
#                is_version_less, is_version_less_or_equal,
#                version_to_numbers), character_text or list_delq outside the
#                module that holds the one port (utility_functions_mod, files_directory_mod,
#                data_mod, none, none, basic_subprograms_mod: its version_type with the
#                operators < <= == /= >= >, editing_functions_mod for char_to_string,
#                none (pack)): a private copy of a Lisp function (R6-45).
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
    pfirst[depth] = ""
    pexec[depth] = 0
    pdone[depth] = 0
    for (i in dummy) if (index(i, depth SUBSEP) == 1) delete dummy[i]
    n = split(args, a, ",")
    for (i = 1; i <= n; i++) dummy[depth, tolower(trim(a[i]))] = 1
    if (tolower(name) in lisp) finding(line, "lisp-helper", name, "")
    check_helper(line, name)
    check_predicate(line, name)
    if (tolower(name) ~ /_vectors?$/) finding(line, "vector", name, "")
    check_verb(line, name)
    check_name(line, name, name)
}

# The rules on the words of any declared NAME (reported as SHOWN):
# abbreviation, underscores, regexp and test.
function check_name(line, name, shown, n) {
    n = "_" tolower(name) "_"
    if (n ~ /_procs?_/) finding(line, "abbreviation", shown, "")
    if (n ~ /[a-z0-9]__[a-z0-9]/) finding(line, "underscores", shown, "")
    if (n ~ /_(re|regex)_$/) finding(line, "regexp", shown, "")
    if (n ~ /_(predicates?|preds?)_/) finding(line, "test", shown, "")
}

# Whether NAME is a private copy of a Lisp function that has one port
# (the duplicate-helper rule): a definition outside that port's file.
function check_helper(line, name, n, base) {
    n = tolower(name)
    if (!(n in helper)) return
    base = file
    sub(/^.*\//, "", base)
    if (base == helper[n]) return
    if (helper[n] == "") finding(line, "duplicate-helper", name, " (no port: use the intrinsics)")
    else finding(line, "duplicate-helper", name, " (the one port is in " helper[n] ")")
}

# The regexp of a reference to character K of string NAME: `NAME(K:K)'.
function charref(name, k) {
    return name "[ \t]*\\([ \t]*" k "[ \t]*:[ \t]*" k "[ \t]*\\)"
}

# The loops of the char-loop and blank-skip rules, on the statement L
# (lower case, no strings) at line LINE.  A `do K = 1, min(len(A),
# len(B))' opens a char loop until its first ordering comparison of
# `A(K:K)' with `B(K:K)'; a `do while' opens a blank skip, which the
# exact body `if (S(K:K) /= C1 .and. S(K:K) /= C2 ...) exit' (each C a
# character literal, constant or achar(N)), `K = K + 1' and `end do'
# completes.
function check_loops(line, l, s, k, a, b, ca, cb, cond, terms, n, i, t, name, kk) {
    if (match(l, /^do[ \t]+[a-z_][a-z0-9_]*[ \t]*=[ \t]*1[ \t]*,[ \t]*min[ \t]*\([ \t]*len[ \t]*\([ \t]*[a-z_][a-z0-9_]*[ \t]*\)[ \t]*,[ \t]*len[ \t]*\([ \t]*[a-z_][a-z0-9_]*[ \t]*\)[ \t]*\)$/)) {
        s = l
        sub(/^do[ \t]+/, "", s)
        k = s
        sub(/[ \t]*=.*/, "", k)
        sub(/^[^(]*\([ \t]*len[ \t]*\([ \t]*/, "", s)
        a = s
        sub(/[ \t]*\).*/, "", a)
        sub(/^[^,]*,[ \t]*len[ \t]*\([ \t]*/, "", s)
        b = s
        sub(/[ \t]*\).*/, "", b)
        incl = 1
        clline = line
        ca = charref(a, k)
        cb = charref(b, k)
        clregexp = "(^|[^a-z0-9_])((ichar|iachar)[ \t]*\\([ \t]*)?" ca "[ \t]*\\)?[ \t]*(<|>|<=|>=)[ \t]*((ichar|iachar)[ \t]*\\([ \t]*)?" cb
        clreverse = "(^|[^a-z0-9_])((ichar|iachar)[ \t]*\\([ \t]*)?" cb "[ \t]*\\)?[ \t]*(<|>|<=|>=)[ \t]*((ichar|iachar)[ \t]*\\([ \t]*)?" ca
        return
    }
    if (incl && (l ~ clregexp || l ~ clreverse)) {
        finding(clline, "char-loop", depth > 0 ? pname[depth] : "?", " (use utility_functions_mod's is_string_less)")
        incl = 0
    }
    if (incl && l ~ /^end[ \t]*(function|subroutine|procedure)/) incl = 0
    if (l ~ /^do[ \t]+while[ \t]*\(/) {
        bs = 1
        bsline = line
        return
    }
    if (bs == 1) {
        bs = 0
        if (!match(l, /^if[ \t]*\(.*\)[ \t]*exit$/)) return
        cond = l
        sub(/^if[ \t]*\(/, "", cond)
        sub(/\)[ \t]*exit$/, "", cond)
        n = split(cond, terms, /\.and\./)

        for (i = 1; i <= n; i++) {
            t = trim(terms[i])
            if (!match(t, /^[a-z_][a-z0-9_]*[ \t]*\([ \t]*[a-z_][a-z0-9_]*[ \t]*:[ \t]*[a-z_][a-z0-9_]*[ \t]*\)[ \t]*\/=[ \t]*([a-z_][a-z0-9_]*|a?char\([0-9]+\))$/)) return
            name = t
            sub(/[ \t]*\(.*/, "", name)
            kk = t
            sub(/^[^(]*\([ \t]*/, "", kk)
            sub(/[ \t]*\).*/, "", kk)
            s = kk
            sub(/[ \t]*:.*/, "", s)
            sub(/^.*:[ \t]*/, "", kk)
            if (s != kk) return
            if (i == 1) {
                bsvar = name
                bsk = kk
            } else if (name != bsvar || kk != bsk) {
                return
            }
        }
        bs = 2
        return
    }
    if (bs == 2) {
        bs = 0
        if (l ~ ("^" bsk "[ \t]*=[ \t]*" bsk "[ \t]*[-+][ \t]*1$")) bs = 3
        return
    }
    if (bs == 3) {
        bs = 0
        if (l ~ /^end[ \t]*do$/) finding(bsline, "blank-skip", depth > 0 ? pname[depth] : "?", " (use `verify')")
    }
}

# Whether the argument list ARGS of a call names only the innermost
# procedure's dummies (with components and elements), keywords, kinds
# and literals: no other call.  NDUMMIES counts the dummies.
function only_dummies(args, t, tok) {
    ndummies = 0
    gsub(/\.[a-z]+\./, " ", args)
    # (A comparison is not a keyword: `list%name == name'.)
    gsub(/==|\/=|<=|>=/, " ", args)
    gsub(/%[a-z_][a-z0-9_]*/, "", args)
    gsub(/[a-z_][a-z0-9_]*[ \t]*=/, " ", args)
    t = args
    while (match(t, /[a-z_][a-z0-9_]*/)) {
        tok = substr(t, RSTART, RLENGTH)
        t = substr(t, RSTART + RLENGTH)
        if ((depth, tok) in dummy) ndummies++
        if ((depth, tok) in dummy || tok in kinds || tok == "s") continue
        return 0
    }
    return 1
}

# Whether NAME runs something with another verb than execute (reported
# as SHOWN, or NAME).
function check_verb(line, name, shown) {
    if (shown == "") shown = name
    if (tolower(name) ~ /^(run|run_[a-z0-9_]+|call_[a-z0-9_]+)$/) finding(line, "verb", shown, "")
}

# Checks the names a procedure declaration statement L (a procedure
# pointer, dummy procedure or binding) declares.
function check_procedure_decl(line, l, names, n, i) {
    n = decl_names(l, names)
    for (i = 1; i <= n; i++) {
        check_verb(line, names[i], (intype != "" ? intype "%" : "") names[i])
        check_name(line, names[i], (intype != "" ? intype "%" : "") names[i])
    }
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
        if (l ~ /(^|,)[ \t]*parameter([ \t]*(,|::))/ && nm ~ /_(nil|t|true|false|disabled|never|off|on|no_limit)$/) {
            finding(line, "constant", toupper(nm), "")
        }
        if (l ~ /(^|,)[ \t]*parameter([ \t]*(,|::))/ && nm ~ /_(yes|no)$/ && nm !~ /_(yes_or_no|y_or_n)$/) {
            finding(line, "yes-no", toupper(nm), "")
        }
        if (nm ~ /_vectors?$/) finding(line, "vector", nm, "")
        check_name(line, nm, (intype != "" ? intype "%" : "") nm)
        if (intype == "command_type" && nm == "interactive") finding(line, "interactive", "command_type%interactive", "")
        if (l ~ /^type[ \t]*\([ \t]*value_type[ \t]*\)/ && nm in cmdstate) finding(line, "command-state", nm, "")
    }
}

# Whether the innermost procedure only writes a number into a buffer and
# trims it: one `write' into its result, or a `write' into a local
# variable and then `RESULT = trim(LOCAL)' (or `RESULT = LOCAL').
function is_write_wrapper(first, last, buffer, lhs) {
    first = tolower(pfirst[depth])
    if (!match(first, /^write[ \t]*\([ \t]*[a-z_][a-z0-9_]*[ \t]*[,)]/)) return 0
    buffer = substr(first, 1, RLENGTH)
    sub(/^write[ \t]*\([ \t]*/, "", buffer)
    sub(/[ \t]*[,)]$/, "", buffer)
    if (pstmts[depth] == 1) return buffer == tolower(pres[depth])
    if (pstmts[depth] != 2) return 0
    last = tolower(pstmt[depth])
    if (!match(last, /^[a-z_][a-z0-9_]*[ \t]*=[ \t]*/)) return 0
    lhs = trim(substr(last, 1, RLENGTH))
    sub(/[ \t]*=$/, "", lhs)
    if (lhs != tolower(pres[depth]) && !((depth, lhs) in dummy)) return 0
    last = substr(last, RLENGTH + 1)
    return last == buffer || last == "trim(" buffer ")" || last ~ ("^trim[ \t]*\\([ \t]*" buffer "[ \t]*\\)$")
}

# Judges the body of the innermost procedure, once.
function judge(stmt, lhs, rest, intr, args) {
    if (pdone[depth]) return
    pdone[depth] = 1
    if (is_write_wrapper()) {
        finding(pline[depth], "write-wrapper", pname[depth], " (write the number where it is used)")
        return
    }
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
    args = substr(rest, RLENGTH + 1)
    if (args !~ /\)[ \t]*$/) return
    sub(/\)[ \t]*$/, "", args)
    # A structure constructor of the dummies (every derived type is
    # named *_type).
    if (intr ~ /_type$/) {
        if (only_dummies(args) && ndummies > 0) {
            finding(pline[depth], "constructor-wrapper", pname[depth], " (write the constructor where it is used)")
        }
        return
    }
    if (!(intr in intrinsic)) return
    if (!only_dummies(args)) return
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
              "boundp fboundp makunbound keywordp defvar_false prin1_to_string prin1_string cons_repr cons_arg", w, " ")
    for (i = 1; i <= n; i++) lisp[w[i]] = 1
    # Words that, with `p' added, name a Lisp type or position predicate.
    n = split("buffer marker string window frame process timer charset symbol keymap list cons fixnum " \
              "number integer natnum float overlay category char keyword command minibuffer function " \
              "sequence vector array atom boolean bound fbound bob eob bol eol less greater blank " \
              "graphic alphabetic alphanumeric uppercase lowercase printable syntax_table case_table " \
              "hash_table record subr", w, " ")
    for (i = 1; i <= n; i++) typeword[w[i]] = 1
    n = split("this_command last_command real_this_command real_last_command last_repeatable_command " \
              "current_minibuffer_command prefix_argument current_prefix_argument last_prefix_argument", w, " ")
    for (i = 1; i <= n; i++) cmdstate[w[i]] = 1
    n = split("int8 int16 int32 int64 real32 real64 real128 c_int c_long c_char", w, " ")
    for (i = 1; i <= n; i++) kinds[w[i]] = 1
    # The Lisp functions with one port, and its file (none for the last two).
    helper["is_string_less"] = "utility_functions_mod.f90"
    helper["is_string_prefix"] = "files_directory_mod.f90"
    helper["string_to_number"] = "data_mod.f90"
    helper["int_text"] = ""
    helper["string_trim"] = ""
    helper["version_less"] = "basic_subprograms_mod.f90"
    helper["version_less_or_equal"] = "basic_subprograms_mod.f90"
    helper["version_le"] = "basic_subprograms_mod.f90"
    helper["is_version_le"] = "basic_subprograms_mod.f90"
    helper["is_version_less"] = "basic_subprograms_mod.f90"
    helper["is_version_less_or_equal"] = "basic_subprograms_mod.f90"
    helper["version_to_numbers"] = "basic_subprograms_mod.f90"
    helper["character_text"] = "editing_functions_mod.f90"
    helper["list_delq"] = ""
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
    inabstract = 0
    ifdepth = 0
    intype = ""
    cont = ""
    incl = 0
    bs = 0
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

# A number made text through a helper (the number-text rule).
{
    t = l

    while (match(t, /(^|[^a-z0-9_])(number_to_string|int_text|integer_text|itoa|int_str|int2str|to_string)[ \t]*\(/)) {
        name = substr(t, RSTART, RLENGTH)
        sub(/^[^a-z]/, "", name)
        sub(/[ \t]*\($/, "", name)
        finding(startline, "number-text", name, "")
        t = substr(t, RSTART + RLENGTH)
    }
}

# Emacs's %-directive formatting (the percent-format rule).
{
    t = l

    while (match(t, /(^|[^a-z0-9_])((format_text|styled_format|format_message)[ \t]*\(|format_arguments_type([^a-z0-9_]|$))/)) {  # fortran-names: keep
        name = substr(t, RSTART, RLENGTH)
        sub(/^[^a-z]/, "", name)
        sub(/[^a-z0-9_]+$/, "", name)
        finding(startline, "percent-format", name, "")
        t = substr(t, RSTART + RLENGTH)
    }
}

# The hand-written string comparisons and blank skips (the char-loop and
# blank-skip rules).
{ check_loops(startline, l) }

# Derived-type definitions (for the component rule) and declarations
# anywhere (for the constant and vector rules).
l ~ /^type([ \t]*,[^:]*)?[ \t]*::[ \t]*[a-z_][a-z0-9_]*[ \t]*$/ || l ~ /^type[ \t]+[a-z_][a-z0-9_]*[ \t]*$/ {
    intype = l
    sub(/^.*[: \t]/, "", intype)
    if (intype ~ /_vectors?$/) finding(startline, "vector", intype, "")
    if (intype !~ /_type$/) finding(startline, "type", intype, "")
    check_name(startline, intype, intype)
    next
}
l ~ /^end[ \t]*type/ { intype = ""; next }
l ~ /::/ && l !~ /^(use|procedure[ \t]*\(|generic)/ { check_decl(startline, l) }
l ~ /^procedure([ \t]*\(|[ \t]*,|[ \t]*::)/ && l ~ /::/ { check_procedure_decl(startline, l) }

l ~ /^(abstract[ \t]+)?interface([ \t]|$)/ { ininterface++; inabstract = (l ~ /^abstract/); next }
l ~ /^end[ \t]*interface/ { ininterface--; inabstract = 0; next }
# The names of abstract interfaces (their dummies are not looked at).
ininterface && inabstract && ifdepth == 0 && \
l ~ /^((pure|elemental|recursive|impure|module)[ \t]+|(logical|integer|real|complex|character|type|class)(\([^)]*\))?[ \t]+)*(function|subroutine)[ \t]+[a-z_][a-z0-9_]*/ {
    name = l
    sub(/^.*(function|subroutine)[ \t]+/, "", name)
    sub(/[^a-z0-9_].*/, "", name)
    if (name !~ /_procedure$/) finding(startline, "interface", name, "")
    check_name(startline, name, name)
    ifdepth = 1
    next
}
ininterface && l ~ /^end[ \t]*(function|subroutine)/ { ifdepth = 0; next }
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
    if (pstmts[depth] == 1) pfirst[depth] = l
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
