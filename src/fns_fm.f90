! fns_fm.f90 -- equality, sequences, property lists, symbol properties.
! Ported from GNU Emacs 31.1 src/fns.c.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Where upstream takes a PREDICATE function object, these take an
!> optional Fortran procedure with the `value_predicate' interface; the
!> default is `eq' (data_fm:value_eq), as upstream.
module fns_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT, FM_ARGS_OUT_OF_RANGE
    use print_fm, only: prin1_string
    use character_fm, only: multibyte_chars_in_text, string_char_and_length
    use casetab_fm, only: upcase
    use lisp_fm, only: atom_type, list, list_elements, list_elt, list_length, list_tail, listp, &
                       make_list, symbols, v_buffer, v_int, v_keymap, v_list, v_marker, v_nil, &
                       v_string, v_symbol, value_type, vtag, xsymbol
    use lread_fm, only: ensure_obarray
    use data_fm, only: value_eq, check_symbol
    use marker_fm, only: markers, resolve_marker
    use keymap_fm, only: keymap_equal
    implicit none(type, external)
    private

    abstract interface
        !> A two-argument test such as `eq', `equal' or `string<'.
        logical function value_predicate(a, b)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: a, b
        end function value_predicate
    end interface

    public :: value_predicate
    public :: value_equal, length, nth, nthcdr, memq, member
    public :: string_lessp, string_greaterp, compare_strings, COMPARE_EQUAL, schars, substring

    !> The value of `compare_strings' for strings that match (t).
    integer(int64), parameter :: COMPARE_EQUAL = 0
    public :: plist_get, plist_put, plist_member, get, put
    public :: yes_or_no_p

    !> Non-nil means `yes_or_no_p' accepts single-key answers "y" or "n".
    !> When non-nil, `yes_or_no_p' will use `y_or_n_p' to read the answer.
    !> This means the user will be able to press just one key to answer, whereas
    !> by default the user needs to type the full "yes" or "no" response
    !> and then press RET.  Port of fns.c use-short-answers.
    logical, public :: use_short_answers = .false.

    !> String to append when `yes_or_no_p' asks a question.
    !> For best results this should end in a space.  Port of fns.c
    !> yes-or-no-prompt.
    character(len=:), allocatable, public :: yes_or_no_prompt

    interface
        !> Ask user a yes-or-no question.
        !> Return t if answer is yes, and nil if the answer is no.
        !>
        !> PROMPT is the string to display to ask the question; `yes_or_no_p'
        !> appends `yes_or_no_prompt' (default "(yes or no) ") to it.  If
        !> PROMPT is a non-empty string, and it ends with a non-space character,
        !> a space character will be appended to it.
        !>
        !> The user must confirm the answer with RET, and can edit it until it
        !> has been confirmed.
        !>
        !> If the `use_short_answers' variable is non-nil, instead of asking for
        !> "yes" or "no", this function will ask for "y" or "n" (and
        !> ignore the value of `yes_or_no_prompt').  Port of
        !> fns.c:Fyes_or_no_p (no dialog boxes).  (In submodule fns_ask_fm,
        !> as the minibuffer depends on this module.)
        recursive module subroutine yes_or_no_p(prompt, answer, status)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine yes_or_no_p
    end interface

contains

    !> Structural equality.  Markers are equal when they point at the same
    !> place in the same buffer (or both nowhere); keymaps when their
    !> bindings, prompts and parents are.  Port of fns.c:Fequal
    !> (internal_equal, without text properties or floats).
    recursive logical function value_equal(a, b) result(eq)
        type(value_type), intent(in) :: a, b
        integer(int64) :: k
        eq = .false.
        if (vtag(a) /= vtag(b)) return
        if (vtag(a) == V_NIL) then
            eq = .true.
            return
        end if
        ! Same preorder atoms <=> same tree.
        if (size(a%a) /= size(b%a)) return
        do k = 1, size(a%a, kind=int64)
            if (.not. atom_equal(a%a(k), b%a(k))) return
        end do
        eq = .true.
    end function value_equal

    recursive logical function atom_equal(x, y) result(eq)
        type(atom_type), intent(in) :: x, y
        integer(int64) :: ix, iy
        type(status_type) :: sx, sy
        eq = .false.
        if (x%tag /= y%tag) return
        select case (x%tag)
        case (V_NIL)
            eq = .true.
        case (V_INT, V_SYMBOL, V_LIST)
            eq = x%i == y%i
        case (V_STRING)
            eq = len(x%s) == len(y%s)
            if (eq) eq = x%s == y%s
        case (V_BUFFER)
            eq = x%h%index == y%h%index .and. x%h%generation == y%h%generation
        case (V_KEYMAP)
            ! A keymap is a list upstream: compare its contents.
            eq = keymap_equal(x%h, y%h, value_equal)
        case (V_MARKER)
            call resolve_marker(x%h, ix, sx)
            call resolve_marker(y%h, iy, sy)
            if (.not. (sx%ok() .and. sy%ok())) then
                eq = x%h%index == y%h%index .and. x%h%generation == y%h%generation
            else
                eq = markers(ix)%buffer == markers(iy)%buffer .and. &
                     (markers(ix)%buffer == 0 .or. markers(ix)%charpos == markers(iy)%charpos)
            end if
        case default
            ! Nothing to do for other values.
        end select
    end function atom_equal

    !> Elements of a list, or characters of a string.
    !> Port of fns.c:Flength (lists and strings).
    subroutine length(sequence, n, status)
        type(value_type), intent(in) :: sequence
        integer(int64), intent(out) :: n
        type(status_type), intent(out) :: status
        type(status_type) :: st
        n = 0
        select case (vtag(sequence))
        case (V_NIL)
            n = 0
        case (V_LIST)
            n = list_length(sequence)
        case (V_STRING)
            call multibyte_chars_in_text(sequence%a(1)%s, n, st)
            if (.not. st%ok()) n = len(sequence%a(1)%s, kind=int64)
        case default
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "sequencep")
        end select
    end subroutine length

    !> The `n'th element (0-based) of `list', or nil.  Port of fns.c:Fnth.
    subroutine nth(n, list, v, status)
        integer(int64), intent(in) :: n
        type(value_type), intent(in) :: list
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        select case (vtag(list))
        case (V_NIL)
        case (V_LIST)
            v = list_elt(list, max(n, 0_int64) + 1)
        case default
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "listp")
        end select
    end subroutine nth

    !> `list' without its first `n' elements.  Port of fns.c:Fnthcdr.
    subroutine nthcdr(n, list, v, status)
        integer(int64), intent(in) :: n
        type(value_type), intent(in) :: list
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        if (n <= 0) then
            v = list
            return
        end if
        select case (vtag(list))
        case (V_NIL)
        case (V_LIST)
            v = list_tail(list, n)
        case default
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "listp")
        end select
    end subroutine nthcdr

    !> The tail of `list' starting with an element `eq' to `elt', or nil.
    !> Port of fns.c:Fmemq.
    function memq(elt, list) result(tail)
        type(value_type), intent(in) :: elt, list
        type(value_type) :: tail
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        call list_elements(list, elems)
        do k = 1, size(elems, kind=int64)
            if (value_eq(elems(k), elt)) then
                tail = list_tail(list, k - 1)
                return
            end if
        end do
    end function memq

    !> Like `memq', comparing with `equal'.  Port of fns.c:Fmember.
    function member(elt, list) result(tail)
        type(value_type), intent(in) :: elt, list
        type(value_type) :: tail
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        call list_elements(list, elems)
        do k = 1, size(elems, kind=int64)
            if (value_equal(elems(k), elt)) then
                tail = list_tail(list, k - 1)
                return
            end if
        end do
    end function member

    !> Text of a string, or the name of a symbol; `ok' false otherwise.
    subroutine string_or_name(v, text, ok)
        type(value_type), intent(in) :: v
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: ok
        ok = .true.
        select case (vtag(v))
        case (V_STRING)
            text = v%a(1)%s
        case (V_SYMBOL, V_NIL)
            call ensure_obarray()
            text = symbols(xsymbol(v))%name
        case default
            ok = .false.
            text = ""
        end select
    end subroutine string_or_name

    !> True if `a' sorts before `b', comparing characters by code.
    !> Symbols stand for their names; anything else compares false (the
    !> predicate interface has no status).  Port of fns.c:Fstring_lessp
    !> (string_cmp).
    logical function string_lessp(a, b)
        type(value_type), intent(in) :: a, b
        character(len=:), allocatable :: sa, sb
        logical :: oka, okb
        integer(int64) :: pa, pb
        integer(int32) :: ca, cb, na, nb
        string_lessp = .false.
        call string_or_name(a, sa, oka)
        call string_or_name(b, sb, okb)
        if (.not. (oka .and. okb)) return
        pa = 1
        pb = 1
        do while (pa <= len(sa, kind=int64) .and. pb <= len(sb, kind=int64))
            call string_char_and_length(sa, pa, ca, na)
            call string_char_and_length(sb, pb, cb, nb)
            if (ca /= cb) then
                string_lessp = ca < cb
                return
            end if
            pa = pa + na
            pb = pb + nb
        end do
        ! Equal up to the shorter length: the shorter one is less.
        string_lessp = pa > len(sa, kind=int64) .and. pb <= len(sb, kind=int64)
    end function string_lessp

    !> Compare the contents of two strings.
    !> The arguments START1, END1, START2, and END2, if present, are
    !> positions specifying which parts of STR1 or STR2 to compare.  In
    !> string STR1, compare the part between START1 (inclusive) and END1
    !> (exclusive).  If START1 is absent, it defaults to 0, the beginning
    !> of the string; if END1 is absent, it defaults to the length of the
    !> string.  Likewise, in string STR2, compare the part between START2
    !> and END2.  Like in `substring', negative values are counted from the
    !> end.
    !>
    !> The strings are compared by the numeric values of their characters.
    !> For instance, STR1 is "less than" STR2 if its first differing
    !> character has a smaller numeric value.  If IGNORE-CASE is true,
    !> characters are converted to upper-case before comparing them.
    !>
    !> The value is COMPARE_EQUAL (t) if the strings (or specified
    !> portions) match.  If string STR1 is less, the value is a negative
    !> number N; - 1 - N is the number of characters that match at the
    !> beginning.  If string STR1 is greater, the value is a positive
    !> number N; N - 1 is the number of characters that match at the
    !> beginning.
    !>
    !> Positions out of range signal args-out-of-range in STATUS (with
    !> STATUS absent the caller guarantees them; the value is then
    !> COMPARE_EQUAL).  Port of fns.c:Fcompare_strings.
    integer(int64) function compare_strings(str1, str2, ignore_case, start1, end1, start2, end2, status) result(r)
        character(len=*), intent(in) :: str1, str2
        logical, intent(in), optional :: ignore_case
        integer(int64), intent(in), optional :: start1, end1, start2, end2
        type(status_type), intent(out), optional :: status
        integer(int32), allocatable :: c1(:), c2(:)
        integer(int64) :: from1, to1, from2, to2, i1, i2, e1, e2
        integer(int32) :: a, b
        logical :: fold, ok
        type(status_type) :: st
        r = COMPARE_EQUAL
        call string_chars(str1, c1)
        call string_chars(str2, c2)
        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        ! For backward compatibility, silently bring too-large positive end
        ! values into range.
        e1 = size(c1, kind=int64)
        if (present(end1)) e1 = min(end1, e1)
        e2 = size(c2, kind=int64)
        if (present(end2)) e2 = min(end2, e2)
        call validate_subarray(str1, size(c1, kind=int64), from1, to1, ok, st, start1, e1, present(end1))
        if (ok) call validate_subarray(str2, size(c2, kind=int64), from2, to2, ok, st, start2, e2, present(end2))
        if (present(status)) status = st
        if (.not. ok) return
        i1 = from1
        i2 = from2
        do while (i1 < to1 .and. i2 < to2)
            a = c1(i1 + 1)
            b = c2(i2 + 1)
            i1 = i1 + 1
            i2 = i2 + 1
            if (a == b) cycle
            if (fold) then
                a = upcase(a)
                b = upcase(b)
            end if
            if (a == b) cycle
            ! I1 has already been incremented past the character compared.
            if (a < b) then
                r = -(i1 - from1)
            else
                r = i1 - from1
            end if
            return
        end do
        if (i1 < to1) then
            r = i1 - from1 + 1
        else if (i2 < to2) then
            r = -(i1 - from1 + 1)
        else
            r = COMPARE_EQUAL
        end if
    end function compare_strings

    !> Check the range FROM..TO of the N characters of STRING
    !> (negative values count from the end; absent FROM is 0, TO absent
    !> or not `to_given' is N) and give it as IFROM..ITO, or signal
    !> args-out-of-range (OK false).  Port of fns.c:validate_subarray.
    pure subroutine validate_subarray(string, n, ifrom, ito, ok, status, from, to, to_given)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: n
        integer(int64), intent(out) :: ifrom, ito
        logical, intent(out) :: ok
        type(status_type), intent(inout) :: status
        integer(int64), intent(in), optional :: from, to
        logical, intent(in), optional :: to_given
        integer(int64) :: f, t
        logical :: has_to
        character(len=:), allocatable :: from_text, to_text
        f = 0
        from_text = "nil"
        if (present(from)) then
            f = from
            from_text = int_text(from)
            if (f < 0) f = f + n
        end if
        has_to = present(to)
        if (present(to_given)) has_to = has_to .and. to_given
        t = n
        to_text = "nil"
        if (has_to) then
            t = to
            to_text = int_text(to)
            if (t < 0) t = t + n
        end if
        ok = 0 <= f .and. f <= t .and. t <= n
        ifrom = f
        ito = t
        if (.not. ok) call set_error(status, FM_ARGS_OUT_OF_RANGE, prin1_string(string)//", "//from_text//", "//to_text)
    end subroutine validate_subarray

    !> The decimal text of N.
    pure function int_text(n) result(text)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: text
        character(len=21) :: buf
        write (buf, "(i0)") n
        text = trim(buf)
    end function int_text

    !> Number of characters in UTF-8 string `s'.  Port of lisp.h:SCHARS.
    pure integer(int64) function schars(s)
        character(len=*), intent(in) :: s
        integer :: i
        schars = 0
        do i = 1, len(s)
            ! Count the bytes that start a character.
            if (iand(iachar(s(i:i)), 192) /= 128) schars = schars + 1
        end do
    end function schars

    !> Return a new string whose contents are a substring of STRING.
    !> The returned string consists of the characters between index FROM
    !> (inclusive) and index TO (exclusive) of STRING.  FROM and TO are
    !> zero-indexed: 0 means the first character of STRING.  Negative
    !> values are counted from the end of STRING.  If TO is absent, the
    !> substring runs to the end of STRING.
    !>
    !> A range outside STRING signals args-out-of-range in STATUS (with
    !> STATUS absent the caller guarantees the range; the value is then
    !> empty).  Port of fns.c:Fsubstring.
    function substring(string, from, to, status) result(r)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: from
        integer(int64), intent(in), optional :: to
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: r
        integer(int64) :: a, b, count
        integer :: i, first, last
        logical :: ok
        type(status_type) :: st
        r = ""
        call validate_subarray(string, schars(string), a, b, ok, st, from, to)
        if (present(status)) status = st
        if (.not. ok) return
        ! Byte positions of characters a and b.
        first = len(string) + 1
        last = len(string) + 1
        count = 0
        do i = 1, len(string)
            if (iand(iachar(string(i:i)), 192) /= 128) then
                if (count == a) first = min(first, i)
                if (count == b) then
                    last = i
                    exit
                end if
                count = count + 1
            end if
        end do
        r = string(first:last - 1)
    end function substring

    !> The characters of UTF-8 string `s'.
    pure subroutine string_chars(s, chars)
        character(len=*), intent(in) :: s
        integer(int32), allocatable, intent(out) :: chars(:)
        integer(int32), allocatable :: tmp(:)
        integer(int64) :: i
        integer(int32) :: c, clen
        integer :: n
        allocate (tmp(len(s)))
        n = 0
        i = 1
        do while (i <= len(s, kind=int64))
            call string_char_and_length(s, i, c, clen)
            n = n + 1
            tmp(n) = c
            i = i + clen
        end do
        chars = tmp(1:n)
    end subroutine string_chars

    !> Port of fns.c:Fstring_greaterp.
    logical function string_greaterp(a, b)
        type(value_type), intent(in) :: a, b
        string_greaterp = string_lessp(b, a)
    end function string_greaterp

    !> Compare with `predicate', or `eq' by default.
    logical function matches(a, b, predicate)
        type(value_type), intent(in) :: a, b
        procedure(value_predicate), optional :: predicate
        if (present(predicate)) then
            matches = predicate(a, b)
        else
            matches = value_eq(a, b)
        end if
    end function matches

    !> Value of `prop' in property list `plist', or nil.  Degenerate
    !> plists (odd length) are tolerated.  Port of fns.c:Fplist_get.
    function plist_get(plist, prop, predicate) result(v)
        type(value_type), intent(in) :: plist, prop
        procedure(value_predicate), optional :: predicate
        type(value_type) :: v
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        call list_elements(plist, elems)
        do k = 1, size(elems, kind=int64) - 1, 2
            if (matches(elems(k), prop, predicate)) then
                v = elems(k + 1)
                return
            end if
        end do
    end function plist_get

    !> Set `prop' to `val' in `plist', adding the pair at the end if absent.
    !> An odd-length plist is a wrong-type-argument (plistp), as upstream.
    !> Port of fns.c:Fplist_put.
    subroutine plist_put(plist, prop, val, status, predicate)
        type(value_type), intent(inout) :: plist
        type(value_type), intent(in) :: prop, val
        type(status_type), intent(out) :: status
        procedure(value_predicate), optional :: predicate
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k, n
        if (.not. listp(plist)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "plistp")
            return
        end if
        call list_elements(plist, elems)
        n = size(elems, kind=int64)
        do k = 1, n - 1, 2
            if (matches(elems(k), prop, predicate)) then
                elems(k + 1) = val
                plist = make_list(elems)
                return
            end if
        end do
        if (mod(n, 2_int64) /= 0) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "plistp")
            return
        end if
        elems = [elems, prop, val]
        plist = make_list(elems)
    end subroutine plist_put

    !> The tail of `plist' starting at `prop', or nil.
    !> Port of fns.c:Fplist_member.
    function plist_member(plist, prop, predicate) result(tail)
        type(value_type), intent(in) :: plist, prop
        procedure(value_predicate), optional :: predicate
        type(value_type) :: tail
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        call list_elements(plist, elems)
        do k = 1, size(elems, kind=int64), 2
            if (matches(elems(k), prop, predicate)) then
                tail = list_tail(plist, k - 1)
                return
            end if
        end do
    end function plist_member

    !> Property `prop' of `symbol'.  Port of fns.c:Fget (without
    !> `overriding_plist_environment').
    subroutine get(symbol, prop, v, status)
        type(value_type), intent(in) :: symbol, prop
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        v = plist_get(symbols(xsymbol(symbol))%plist, prop)
    end subroutine get

    !> Set property `prop' of `symbol' to `val'.  Port of fns.c:Fput.
    subroutine put(symbol, prop, val, status)
        type(value_type), intent(in) :: symbol, prop, val
        type(status_type), intent(out) :: status
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        call plist_put(symbols(xsymbol(symbol))%plist, prop, val, status)
    end subroutine put
end module fns_fm
