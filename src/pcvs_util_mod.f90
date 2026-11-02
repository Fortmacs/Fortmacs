! pcvs_util_mod.f90 -- utility functions for PCL-CVS.
! Ported from GNU Emacs 31.1 lisp/vc/pcvs-util.el.
!
! Copyright (C) 1991-2026 Free Software Foundation, Inc.
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

!> What Log Edit mode uses of pcvs-util.el: `cvs_insert_strings'.
!>
!> Deferred: the rest of pcvs-util.el (the buffer and string utilities
!> of PCL-CVS, which is not ported).
module pcvs_util_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use insdel_mod, only: insert
    use editfns_mod, only: is_bol, delete_region, point
    use indent_mod, only: current_column
    use xdisp_mod, only: tab_width
    use frame_mod, only: frame_selected_window_ref
    use window_mod, only: window_type, window_text_cols
    implicit none(type, external)
    private

    public :: cvs_string_type, cvs_insert_strings

    !> One string of a list given to `cvs_insert_strings'.
    type :: cvs_string_type
        character(len=:), allocatable :: s
    end type cvs_string_type

contains

    !> Insert a list of STRINGS into the current buffer.
    !> Uses columns to keep the listing readable but compact.  Port of
    !> pcvs-util.el:cvs-insert-strings.
    subroutine cvs_insert_strings(strings, status)
        type(cvs_string_type), intent(in) :: strings(:)
        type(status_type), intent(out) :: status
        type(window_type), pointer :: w
        integer :: length, wwidth, columns, colwidth, k

        status = status_type()
        if (size(strings) == 0) return
        length = 0

        do k = 1, size(strings)
            length = max(length, nchars(strings(k)%s))
        end do

        w => frame_selected_window_ref()
        wwidth = window_text_cols(w) - 1
        ! At least 2 columns; at least 2 spaces between columns.
        ! Don't allocate more columns than we can fill.
        ! Windows can't show less than 3 lines anyway.
        columns = min(max(2, wwidth/(2 + length)), max(1, size(strings)/2))
        colwidth = wwidth/columns
        ! Use tab-width rather than indent-to.
        call tab_width%set_local(int(colwidth, int64))

        ! The insertion should be "sensible" no matter what choices were made.
        do k = 1, size(strings)
            if (.not. is_bol()) then
                call insert(" "//achar(9), status)
                if (.not. status%ok()) return

                if (wwidth < max(colwidth, nchars(strings(k)%s)) + current_column()) then
                    call delete_region(point() - 2, point(), status)
                    if (.not. status%ok()) return
                    call insert(achar(10), status)
                    if (.not. status%ok()) return
                end if
            end if

            call insert(strings(k)%s, status)
            if (.not. status%ok()) return
        end do
    end subroutine cvs_insert_strings

    ! The number of characters of the UTF-8 string S (its `length').
    ! Not a port.
    integer function nchars(s)
        character(len=*), intent(in) :: s
        integer :: i

        nchars = 0

        do i = 1, len(s)
            if (iand(ichar(s(i:i)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars
end module pcvs_util_mod
