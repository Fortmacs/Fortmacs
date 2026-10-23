! print_fm.f90 -- printed representation of values.
! Ported from GNU Emacs 31.1 src/print.c.
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

!> The printed representation of a value, as `prin1' writes it: what
!> help shows for a variable holding a value.
!>
!> Deferred: `princ', print streams, `print_length' and `print_level',
!> escaping in symbol names, circular structure, floats, the char-table
!> of a full keymap (made by `make_keymap'; printed "#<keymap>"), and
!> the place of character ranges among a sparse keymap's other bindings
!> (they are printed first).
module print_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, vtag, V_NIL, V_INT, V_SYMBOL, V_STRING, V_LIST, V_BUFFER, V_MARKER, &
                       V_KEYMAP, xfixnum, xstring, xhandle, list_elements
    use data_fm, only: symbol_name
    use buffer_fm, only: buffer_name, buffers
    use handle_fm, only: handle_type
    use marker_fm, only: markers, resolve_marker
    use keymap_fm, only: keymap_live_p, keymap_full_p, keymap_prompt, keymap_parent, keymap_entries
    implicit none(type, external)
    private

    public :: prin1_to_string, prin1_string

contains

    !> Return a string containing the printed representation of OBJECT.
    !> OBJECT can be any Lisp object.  Strings are quoted, a backslash
    !> escaping each double quote and backslash in them.  Port of
    !> print.c:Fprin1_to_string (NOESCAPE nil).
    recursive function prin1_to_string(object) result(s)
        type(value_type), intent(in) :: object
        character(len=:), allocatable :: s
        type(value_type), allocatable :: elems(:)
        type(status_type) :: status
        character(len=21) :: buf
        character(len=:), allocatable :: text
        integer :: k
        select case (vtag(object))
        case (V_NIL)
            s = "nil"
        case (V_INT)
            write (buf, "(i0)") xfixnum(object)
            s = trim(buf)
        case (V_SYMBOL)
            call symbol_name(object, s, status)
            if (.not. status%ok()) s = "#<symbol>"
        case (V_STRING)
            text = xstring(object)
            s = prin1_string(text)
        case (V_LIST)
            call list_elements(object, elems)
            s = "("
            do k = 1, size(elems)
                if (k > 1) s = s//" "
                s = s//prin1_to_string(elems(k))
            end do
            s = s//")"
        case (V_BUFFER)
            call buffer_name(xhandle(object), text, status)
            if (status%ok()) then
                s = "#<buffer "//text//">"
            else
                s = "#<killed buffer>"
            end if
        case (V_MARKER)
            s = marker_repr(xhandle(object))
        case (V_KEYMAP)
            if (keymap_live_p(xhandle(object)) .and. .not. keymap_full_p(xhandle(object))) then
                s = "("//keymap_elements(xhandle(object))//")"
            else
                s = "#<keymap>"
            end if
        case default
            s = "#<unknown>"
        end select
    end function prin1_to_string

    !> The printed representation of the string TEXT, as `prin1' writes
    !> it: in double quotes, a backslash escaping each double quote and
    !> backslash.  Port of the string case of print.c:print_object.
    pure function prin1_string(text) result(s)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        integer :: k
        s = """"
        do k = 1, len(text)
            if (text(k:k) == """" .or. text(k:k) == "\") s = s//"\"
            s = s//text(k:k)
        end do
        s = s//""""
    end function prin1_string

    !> "#<marker at N in BUFFER>", "#<marker in no buffer>", after
    !> "(moves after insertion) " for that insertion type.  The marker
    !> case of print.c:print_vectorlike.
    function marker_repr(m) result(s)
        type(handle_type), intent(in) :: m
        character(len=:), allocatable :: s
        character(len=:), allocatable :: name
        type(status_type) :: status
        character(len=21) :: buf
        integer(int64) :: i
        s = "#<marker "
        call resolve_marker(m, i, status)
        if (.not. status%ok()) then
            s = s//"in no buffer>"
            return
        end if
        if (markers(i)%insertion_type) s = s//"(moves after insertion) "
        if (markers(i)%buffer == 0) then
            s = s//"in no buffer"
        else
            write (buf, "(i0)") markers(i)%charpos
            call marker_buffer_name(markers(i)%buffer, name)
            s = s//"at "//trim(buf)//" in "//name
        end if
        s = s//">"
    end function marker_repr

    !> The name of the buffer in registry slot `slot'.
    subroutine marker_buffer_name(slot, name)
        integer(int64), intent(in) :: slot
        character(len=:), allocatable, intent(out) :: name
        name = buffers(slot)%name
    end subroutine marker_buffer_name

    !> The elements of keymap `km''s list printed, "keymap (97 . undo)
    !> ...": its bindings newest first, its prompt, then its parent's.
    !> As print.c prints the list (keymap ENTRIES... PROMPT . PARENT).
    recursive function keymap_elements(km) result(s)
        type(handle_type), intent(in) :: km
        character(len=:), allocatable :: s
        type(value_type), allocatable :: events(:), bindings(:), range_bindings(:)
        integer(int64), allocatable :: range_from(:), range_to(:)
        type(value_type) :: prompt
        type(handle_type) :: parent
        character(len=21) :: a, b
        integer :: k
        s = "keymap"
        call keymap_entries(km, events, bindings, range_from, range_to, range_bindings)
        do k = 1, size(range_from)
            write (a, "(i0)") range_from(k)
            write (b, "(i0)") range_to(k)
            s = s//" "//cons_repr("("//trim(a)//" . "//trim(b)//")", range_bindings(k))
        end do
        do k = 1, size(events)
            s = s//" "//cons_repr(prin1_to_string(events(k)), bindings(k))
        end do
        prompt = keymap_prompt(km)
        if (vtag(prompt) /= V_NIL) s = s//" "//prin1_to_string(prompt)
        parent = keymap_parent(km)
        if (keymap_live_p(parent)) then
            if (keymap_full_p(parent)) then
                s = s//" . #<keymap>"
            else
                s = s//" "//keymap_elements(parent)
            end if
        end if
    end function keymap_elements

    !> The cons (CAR . CDR) printed, `car' already printed: "(CAR)" for a
    !> nil CDR, "(CAR ELEMENTS...)" for a list or keymap.
    recursive function cons_repr(car, cdr) result(s)
        character(len=*), intent(in) :: car
        type(value_type), intent(in) :: cdr
        character(len=:), allocatable :: s
        character(len=:), allocatable :: inner
        select case (vtag(cdr))
        case (V_NIL)
            s = "("//car//")"
        case (V_LIST)
            inner = prin1_to_string(cdr)
            s = "("//car//" "//inner(2:)
        case (V_KEYMAP)
            if (keymap_live_p(xhandle(cdr)) .and. .not. keymap_full_p(xhandle(cdr))) then
                s = "("//car//" "//keymap_elements(xhandle(cdr))//")"
            else
                s = "("//car//" . #<keymap>)"
            end if
        case default
            s = "("//car//" . "//prin1_to_string(cdr)//")"
        end select
    end function cons_repr
end module print_fm
