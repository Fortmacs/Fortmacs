! simple_kill_fm.f90 -- feedback for copied text, a submodule of
! simple_fm.
! Ported from GNU Emacs 31.1 lisp/simple.el.
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> `indicate_copied_region': a submodule because it uses replace_fm,
!> which uses simple_fm.
submodule(simple_fm) simple_kill_fm
    use replace_fm, only: query_replace_descr
    use keyboard_fm, only: sit_for
    implicit none(type, external)

contains

    module procedure indicate_copied_region
    integer(int64) :: mk, pt, n
    character(len=:), allocatable :: text
    type(handle_type) :: m
    type(status_type) :: st
    type(window_type), pointer :: w
    logical :: visible
    mk = mark_position()
    pt = point()
    w => selected_window_ref()
    visible = .false.
    if (shows_current_buffer(w)) visible = pos_visible_in_window_p(mk, w)
    if (visible) then
        ! Swap point-and-mark quickly so as to show the region that was
        ! selected.  Don't do it if the region is highlighted.
        if (copy_region_blink_delay > 0) then
            if (copy_region_blink_predicate()) then
                ! Swap point and mark.
                call mark_marker(m)
                call set_marker(m, pt, st)
                call goto_char(mk)
                call sit_for(int(copy_region_blink_delay*1000, int32))
                ! Swap back.
                call set_marker(m, mk, st)
                call goto_char(pt)
            end if
        end if
    else
        n = 40
        if (present(message_len)) n = message_len
        n = min(abs(mk - pt), n)
        if (pt < mk) then
            ! Don't say "killed" or "saved"; that is misleading.
            call buffer_substring(mk - n, mk, text, st)
            call message('Copied text until "'//query_replace_descr(text)//'"')
        else
            call buffer_substring(mk, mk + n, text, st)
            call message('Copied text from "'//query_replace_descr(text)//'"')
        end if
    end if
    end procedure indicate_copied_region
end submodule simple_kill_fm
