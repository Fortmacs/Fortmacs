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
!> optional Fortran procedure with the `value_predicate` interface; the
!> default is `eq` (data_fm:value_eq), as upstream.
module fns_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use character_fm, only: multibyte_chars_in_text, string_char_and_length
    use lisp_fm, only: atom_type, list, list_elements, list_elt, list_length, list_tail, listp, &
                       make_list, symbols, v_buffer, v_int, v_keymap, v_list, v_marker, v_nil, &
                       v_string, v_symbol, value_type, vtag, xsymbol
    use lread_fm, only: ensure_obarray
    use data_fm, only: value_eq, check_symbol
    use marker_fm, only: markers, resolve_marker
    implicit none(type, external)
    private

    abstract interface
        !> A two-argument test such as `eq`, `equal` or `string<`.
        logical function value_predicate(a, b)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: a, b
        end function value_predicate
    end interface

    public :: value_predicate
    public :: value_equal, length, nth, nthcdr, memq, member
    public :: string_lessp, string_greaterp
    public :: plist_get, plist_put, plist_member, get, put
    public :: yes_or_no_p

    interface
        !> Ask a yes-or-no question in the minibuffer: `answer` is true for
        !> "yes", false for "no"; anything else asks again.  Port of
        !> fns.c:Fyes_or_no_p.  (In submodule fns_ask_fm, as the minibuffer
        !> depends on this module.)
        recursive module subroutine yes_or_no_p(prompt, answer, status)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine yes_or_no_p
    end interface

contains

    !> Structural equality.  Markers are equal when they point at the same
    !> place in the same buffer (or both nowhere).  Port of fns.c:Fequal
    !> (internal_equal, without text properties or floats).
    logical function value_equal(a, b) result(eq)
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

    logical function atom_equal(x, y) result(eq)
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
        case (V_BUFFER, V_KEYMAP)
            eq = x%h%index == y%h%index .and. x%h%generation == y%h%generation
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

    !> The `n`th element (0-based) of `list`, or nil.  Port of fns.c:Fnth.
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

    !> `list` without its first `n` elements.  Port of fns.c:Fnthcdr.
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

    !> The tail of `list` starting with an element `eq` to `elt`, or nil.
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

    !> Like `memq`, comparing with `equal`.  Port of fns.c:Fmember.
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

    !> Text of a string, or the name of a symbol; `ok` false otherwise.
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

    !> True if `a` sorts before `b`, comparing characters by code.
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

    !> Port of fns.c:Fstring_greaterp.
    logical function string_greaterp(a, b)
        type(value_type), intent(in) :: a, b

        string_greaterp = string_lessp(b, a)
    end function string_greaterp

    !> Compare with `predicate`, or `eq` by default.
    logical function matches(a, b, predicate)
        type(value_type), intent(in) :: a, b
        procedure(value_predicate), optional :: predicate

        if (present(predicate)) then
            matches = predicate(a, b)
        else
            matches = value_eq(a, b)
        end if
    end function matches

    !> Value of `prop` in property list `plist`, or nil.  Degenerate
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

    !> Set `prop` to `val` in `plist`, adding the pair at the end if absent.
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

    !> The tail of `plist` starting at `prop`, or nil.
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

    !> Property `prop` of `symbol`.  Port of fns.c:Fget (without
    !> `overriding-plist-environment`).
    subroutine get(symbol, prop, v, status)
        type(value_type), intent(in) :: symbol, prop
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        v = plist_get(symbols(xsymbol(symbol))%plist, prop)
    end subroutine get

    !> Set property `prop` of `symbol` to `val`.  Port of fns.c:Fput.
    subroutine put(symbol, prop, val, status)
        type(value_type), intent(in) :: symbol, prop, val
        type(status_type), intent(out) :: status

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        call plist_put(symbols(xsymbol(symbol))%plist, prop, val, status)
    end subroutine put
end module fns_fm
