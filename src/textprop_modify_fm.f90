! textprop_modify_fm.f90 -- text property changes as buffer modifications,
! a submodule of textprop_fm.
! Ported from GNU Emacs 31.1 src/textprop.c.
!
! Copyright (C) 1993-2026 Free Software Foundation, Inc.
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

!> Part of textprop_fm: changing text properties checks and records the
!> modification and runs the change hooks of insdel_fm.  A submodule
!> because insdel_fm uses textprop_fm.
submodule(textprop_fm) textprop_modify_fm
    use buffer_fm, only: modiff_incr, buf_compute_unchanged
    use undo_fm, only: record_first_change
    use insdel_fm, only: prepare_to_modify_buffer_1, signal_after_change
    implicit none(type, external)

contains

    !> Prepare to modify the text properties of the current buffer from
    !> START to END.  Port of textprop.c:modify_text_properties
    !> (`point_before_scroll' is not ported).
    recursive module subroutine modify_text_properties(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call prepare_to_modify_buffer_1(start, end, status)
        if (.not. status%ok()) return
        call buf_compute_unchanged(buffers(current), start - 1, end)
        if (buffers(current)%text%modiff <= buffers(current)%text%save_modiff) call record_first_change()
        call modiff_incr(buffers(current)%text%modiff, 1_int64)
    end subroutine modify_text_properties

    !> signal_after_change (START, END - START, END - START), as textprop.c
    !> calls it after changing the properties of START..END.
    recursive module subroutine signal_text_properties_change(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call signal_after_change(start, end - start, end - start, status)
    end subroutine signal_text_properties_change
end submodule textprop_modify_fm
