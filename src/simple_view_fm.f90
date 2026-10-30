! simple_view_fm.f90 -- Read-Only mode and what-line, a submodule of
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

!> `read_only_mode' (C-x C-q) and `what_line': a submodule because
!> Read-Only mode uses view_fm (View mode), which uses simple_fm.
submodule(simple_fm) simple_view_fm
    use commands_fm, only: commands
    use modes_fm, only: minor_modes
    use derived_fm, only: major_mode_type, major_mode_named
    use editfns_fm, only: point_min, widen, narrow_to_region
    use view_fm, only: view_mode_enabled, view_exit_and_edit, view_read_only_value, set_view_read_only_local, &
                       view_mode_enter
    implicit none(type, external)

contains

    !> The body of `read_only_mode': set `buffer_read_only' (the mode's
    !> variable), then enter or leave View mode.  Port of
    !> simple.el:read-only-mode (its body).
    recursive subroutine read_only_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(major_mode_type), pointer :: p
        logical :: special

        status = status_type()
        buffers(cur)%read_only = enable
        ! We're saving this value here so that we can restore the
        ! readedness state after reverting the buffer to the value that's
        ! been explicitly set by the user.
        buffers(cur)%read_only_mode_state = buffers(cur)%read_only
        buffers(cur)%read_only_mode_state_set = .true.

        if (.not. buffers(cur)%read_only .and. view_mode_enabled()) then
            call view_exit_and_edit(status)
            ! Must leave view mode.
            call set_view_read_only_local(.true.)
        else if (buffers(cur)%read_only .and. view_read_only_value() .and. .not. view_mode_enabled()) then
            ! If view-mode is already active, `view_mode_enter' is a nop.
            special = .false.

            if (allocated(buffers(cur)%major_mode)) then
                p => major_mode_named(buffers(cur)%major_mode)
                if (associated(p)) special = p%special
            end if

            if (.not. special) call view_mode_enter(status)
        end if
    end subroutine read_only_body

    module procedure read_only_mode
    call minor_modes%read_only_mode%command(status)
    end procedure read_only_mode

    module procedure what_line
    character(len=:), allocatable :: text

    status = status_type()
    call what_line_text(text)
    end procedure what_line

    module procedure what_line_text
    integer(int64) :: start, n, begv, zv
    character(len=24) :: a, b
    type(status_type) :: status

    start = point_min()
    n = line_number_at_pos()
    write (a, "(i0)") n

    if (start == 1) then
        text = "Line "//trim(a)
        call message(text)
    else
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call widen()
        write (b, "(i0)") n + line_number_at_pos(start) - 1
        text = "line "//trim(b)//" (narrowed line "//trim(a)//")"
        call message(text)
        call narrow_to_region(begv, zv, status)
    end if
    end procedure what_line_text

    module procedure syms_of_simple_view
    call minor_modes%read_only_mode%define("read_only_mode", variable="buffer_read_only", hook=read_only_mode_hook, &
                                           body=read_only_body)
    commands%read_only_mode%run => read_only_mode
    commands%what_line%run => what_line
    end procedure syms_of_simple_view
end submodule simple_view_fm
