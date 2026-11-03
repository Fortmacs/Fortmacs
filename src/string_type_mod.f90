! string_type_mod.f90 -- a string of any length, as an array element.
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

!> The element type of a stored list of strings of different lengths
!> (a command line, an environment, a file list, a history): Fortran
!> has no array of deferred-length strings whose elements differ in
!> length, so each element is a `string_type' holding its own `text'.
!> The name is fortran-lang stdlib's (stdlib_string_type).  Not a port:
!> upstream's lists of strings are Lisp lists.
!>
!> Such a list grows one plain literal at a time, never through a
!> structure constructor inside an array constructor (a leak with
!> gfortran 13; PORTING.md "Hazards"):
!>
!>     call append_string(args, "-c", command)
module string_type_mod
    implicit none(type, external)
    private

    public :: append_string, prepend_string

    !> A string of any length, as an element of an array of strings.
    type, public :: string_type
        character(len=:), allocatable :: text
    end type string_type

contains

    !> Add TEXT, then TEXT2 to TEXT8 when they are present, at the end
    !> of LIST (allocated empty first when it is not allocated), as the
    !> intrinsics `max' and `min' take several arguments:
    !> `call append_string(command, "sh", "-c", script)'.  Not a port.
    subroutine append_string(list, text, text2, text3, text4, text5, text6, text7, text8)
        type(string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: text2, text3, text4, text5, text6, text7, text8

        if (.not. allocated(list)) allocate (list(0))
        call append_one_string(list, text)
        if (present(text2)) call append_one_string(list, text2)
        if (present(text3)) call append_one_string(list, text3)
        if (present(text4)) call append_one_string(list, text4)
        if (present(text5)) call append_one_string(list, text5)
        if (present(text6)) call append_one_string(list, text6)
        if (present(text7)) call append_one_string(list, text7)
        if (present(text8)) call append_one_string(list, text8)
    end subroutine append_string

    ! Add TEXT at the end of the allocated LIST.  Not a port.
    subroutine append_one_string(list, text)
        type(string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: text
        type(string_type) :: string

        string%text = text
        list = [list, string]
    end subroutine append_one_string

    !> Add TEXT at the start of LIST (allocated empty first when it is
    !> not allocated).  Not a port.
    subroutine prepend_string(list, text)
        type(string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: text
        type(string_type) :: string

        if (.not. allocated(list)) allocate (list(0))
        string%text = text
        list = [string, list]
    end subroutine prepend_string
end module string_type_mod
