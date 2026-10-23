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
!> escaping in symbol names, circular structure, floats.
module print_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, vtag, V_NIL, V_INT, V_SYMBOL, V_STRING, V_LIST, V_BUFFER, V_MARKER, &
                       V_KEYMAP, xfixnum, xstring, xhandle, list_elements
    use data_fm, only: symbol_name
    use buffer_fm, only: buffer_name
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
            s = "#<marker>"
        case (V_KEYMAP)
            s = "#<keymap>"
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
end module print_fm
