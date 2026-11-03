! keyboard_suspend_mod.f90 -- suspending Emacs (C-z).
! Ported from GNU Emacs 31.1 src/keyboard.c (Fsuspend_emacs,
! stuff_buffered_input) and lisp/frame.el (suspend-frame).
!
! Copyright (C) 1985-1989, 1993-1997, 1999-2026 Free Software Foundation,
! Inc.
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

!> Part of keyboard_mod: suspending Fortmacs, which needs the terminal
!> (term_mod) and the frame size change (dispnew_mod), both above
!> keyboard_mod, so it lives in a submodule.  There is one terminal, the
!> selected frame's, so upstream's check for other tty frames never fails.
!> GNU/Linux has job control, so Fortmacs stops itself (SIGTSTP); the
!> subshell upstream runs instead when `cannot-suspend' is set is not
!> ported.
submodule(keyboard_mod) keyboard_suspend_mod
    use commands_mod, only: commands
    use frame_mod, only: selected_frame
    use term_mod, only: reset_all_sys_modes, init_all_sys_modes, is_controlling_tty
    use dispnew_mod, only: change_frame_size
    use sysdep_mod, only: get_tty_size, sys_suspend, stuff_char
    implicit none(type, external)

contains

    module procedure suspend_fortmacs
    integer(int32) :: old_width, old_height, width, height
    type(status_type) :: st

    call suspend_hook%execute(status)
    if (.not. status%ok()) return
    old_width = 0
    old_height = 0
    if (selected_frame%tty%input >= 0) call get_tty_size(selected_frame%tty%input, old_width, old_height, st)
    call reset_all_sys_modes()
    call stuff_buffered_input(stuffstring)
    call sys_suspend()
    call init_all_sys_modes()
    ! Check if terminal/window size has changed.
    width = 0
    height = 0
    if (selected_frame%tty%input >= 0) call get_tty_size(selected_frame%tty%input, width, height, st)

    if (width /= old_width .or. height /= old_height) then
        call change_frame_size(selected_frame, width, height, .false., .false.)
    end if

    call suspend_resume_hook%execute(status)
    end procedure suspend_fortmacs

    !> If STUFFSTRING is present, stuff its contents and then a newline as
    !> pending terminal input.  Then stuff anything Fortmacs has read ahead
    !> and not used.  However, do nothing if stuffing does not work.
    !> Port of keyboard.c:stuff_buffered_input (with sysdep.c:stuff_char's
    !> check that the selected frame is on a terminal).
    subroutine stuff_buffered_input(stuffstring)
        character(len=*), intent(in), optional :: stuffstring
        integer(int64) :: k, c
        integer :: i
        logical :: ok

        ! Only a terminal frame's input can be stuffed.
        ok = .not. associated(selected_frame%rif) .and. selected_frame%tty%input >= 0

        if (present(stuffstring) .and. ok) then
            do i = 1, len(stuffstring)
                call stuff_char(selected_frame%tty%input, stuffstring(i:i), ok)
                if (.not. ok) exit
            end do

            if (ok) call stuff_char(selected_frame%tty%input, char(10), ok)
        end if

        ! Anything we have read ahead, put back for the shell to read: the
        ! characters typed (ASCII_KEYSTROKE_EVENT, whose code leaves out the
        ! modifier bits).
        do k = head, tail
            if (.not. ok) exit
            if (vtag(events(k)) /= V_INTEGER) cycle
            c = iand(xfixnum(events(k)), not(int(CHAR_MODIFIER_MASK, int64)))
            if (c < 128) call stuff_char(selected_frame%tty%input, char(int(c)), ok)
        end do

        head = 1
        tail = 0
        if (allocated(events)) deallocate (events)
    end subroutine stuff_buffered_input

    module procedure suspend_frame
    if (associated(selected_frame%rif)) then
        ! display-multi-frame-p: a graphical frame.
        call set_error(status, FM_GENERIC_ERROR, "iconify_or_deiconify_frame is not ported yet")
    else if (is_controlling_tty(selected_frame%tty)) then
        call suspend_fortmacs(status)
    else
        call set_error(status, FM_GENERIC_ERROR, "suspend_tty is not ported yet")
    end if
    end procedure suspend_frame

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Stop Fortmacs and return to superior process.  You can resume later.
    !>
    !> Before suspending, run the normal hook `suspend_hook'.
    !> After resumption run the normal hook `suspend_resume_hook'.
    !> Port of keyboard.c:Fsuspend_emacs (the command, "": no
    !> STUFFSTRING).
    recursive subroutine cmd_suspend_fortmacs(status)
        type(status_type), intent(out) :: status

        call suspend_fortmacs(status)
    end subroutine cmd_suspend_fortmacs

    !> Do whatever is right to suspend the current frame.
    !> Calls `suspend_fortmacs' if invoked from the controlling tty device,
    !> `suspend_tty' from a secondary tty device, and
    !> `iconify_or_deiconify_frame' from a graphical frame.
    !> Port of frame.el:suspend-frame (the command).
    recursive subroutine cmd_suspend_frame(status)
        type(status_type), intent(out) :: status

        call suspend_frame(status)
    end subroutine cmd_suspend_frame

    module procedure init_keyboard_suspend_commands
    commands%suspend_fortmacs%execute => cmd_suspend_fortmacs
    commands%suspend_frame%execute => cmd_suspend_frame
    end procedure init_keyboard_suspend_commands
end submodule keyboard_suspend_mod
