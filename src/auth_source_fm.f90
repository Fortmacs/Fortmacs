! auth_source_fm.f90 -- reading passwords.
! Ported from GNU Emacs 31.1 lisp/auth-source.el.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> `read_passwd' and `read_passwd_mode': reading a password in the
!> minibuffer, its characters shown as `read_hide_char' (TAB shows them,
!> C-u clears the input).
!>
!> Deferred: the rest of auth-source.el (the authentication sources);
!> the mode line icon of `read_passwd_mode' (icons and
!> `global_mode_string' are not ported); the buffer-local
!> `show_paren_mode', `select_active_regions' and `inhibit__record_char'
!> of the password's minibuffer, and text conversion styles.
module auth_source_fm
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: nil, make_string
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, buffer_live_p, erase_buffer
    use editfns_fm, only: buffer_size
    use textprop_fm, only: put_text_property, remove_list_of_text_properties
    use character_fm, only: char_string, MAX_MULTIBYTE_LENGTH
    use keymap_fm, only: keymap_set, set_keymap_parent, use_local_map
    use keyboard_fm, only: post_command_hook, sit_for
    use cmds_fm, only: post_self_insert_hook
    use modes_fm, only: minor_modes
    use commands_fm, only: commands
    use minibuf_fm, only: minibuffer_prompt_end, minibuffer_setup_function, read_string, &
                          enable_recursive_minibuffers, minibuffer_local_map, read_hide_char
    use window_fm, only: windows, window_valid_p
    use frame_fm, only: selected_frame
    use xdisp_fm, only: message
    implicit none(type, external)
    private

    public :: read_passwd, read_passwd_toggle_visibility, read_passwd_mode, syms_of_auth_source

    ! Flag indicating whether password in minibuffer is hidden
    ! (read-passwd--password-hidden).
    logical :: password_hidden = .false.

    ! A minibuffer where `read_passwd' is active.
    type :: buffer_ref_type
        type(handle_type) :: buffer
    end type buffer_ref_type

    ! List of minibuffers where `read-passwd' is active
    ! (read-passwd--mini-buffers), the newest first.
    type(buffer_ref_type), allocatable :: mini_buffers(:)

    ! The minibuffer `read_passwd' is reading in.
    type(handle_type) :: minibuf

contains

    ! Make password in minibuffer hidden or visible.  Port of
    ! auth-source.el:read-passwd--hide-password.
    recursive subroutine read_passwd_hide_password(status)
        type(status_type), intent(out) :: status
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nbytes
        integer(int64) :: beg, i
        type(status_type) :: st
        logical :: changed
        status = status_type()
        beg = minibuffer_prompt_end()
        if (read_hide_char >= 0) then
            call char_string(read_hide_char, buf, nbytes, st)
        else
            buf = "*"
            nbytes = 1
        end if
        do i = 0, buffer_size() - beg
            if (password_hidden) then
                call put_text_property(i + beg, i + beg + 1, sym("display"), make_string(buf(1:nbytes)), st)
            else
                call remove_list_of_text_properties(i + beg, i + beg + 1, [sym("display")], changed, st)
            end if
            call put_text_property(i + beg, i + beg + 1, sym("help_echo"), &
                                   make_string("C-u: Clear password"//achar(10)//"TAB: Toggle password visibility"), st)
        end do
    end subroutine read_passwd_hide_password

    !> Toggle minibuffer contents visibility.
    !> Adapt also mode line.  If optional FORCE is true, hide the
    !> minibuffer contents.  Port of
    !> auth-source.el:read-passwd-toggle-visibility (no mode line icon).
    recursive subroutine read_passwd_toggle_visibility(status, force)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: force
        type(handle_type) :: old, mbuf
        type(status_type) :: st
        logical :: forced
        status = status_type()
        forced = .false.
        if (present(force)) forced = force
        if (.not. window_valid_p(selected_frame%minibuffer_window)) then
            call set_error(status, FM_GENERIC_ERROR, "No active minibuffer")
            return
        end if
        mbuf = windows(selected_frame%minibuffer_window%index)%buffer
        if (.not. buffer_live_p(mbuf)) then
            call set_error(status, FM_GENERIC_ERROR, "No active minibuffer")
            return
        end if
        ! FIXME: In case of a recursive minibuffer, this may select the
        ! wrong mini-buffer.
        old = current_buffer()
        call set_buffer(mbuf, st)
        if (minor_modes%read_passwd_mode%enabled()) then
            password_hidden = forced .or. .not. password_hidden
            call read_passwd_hide_password(st)
        end if
        call set_buffer(old, st)
    end subroutine read_passwd_toggle_visibility

    subroutine cmd_read_passwd_toggle_visibility(status)
        type(status_type), intent(out) :: status
        call read_passwd_toggle_visibility(status)
    end subroutine cmd_read_passwd_toggle_visibility

    ! The body of `read_passwd_mode' (ENABLE is its new state).  Port of
    ! auth-source.el read-passwd-mode (no mode line icon).
    recursive subroutine read_passwd_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(buffer_ref_type), allocatable :: rest(:)
        type(handle_type) :: old, cb
        type(status_type) :: st
        integer :: i, n
        status = status_type()
        if (.not. allocated(mini_buffers)) allocate (mini_buffers(0))
        if (.not. enable) then
            cb = current_buffer()
            allocate (rest(0))
            do i = 1, size(mini_buffers)
                if (mini_buffers(i)%buffer%index == cb%index .and. mini_buffers(i)%buffer%generation == cb%generation) cycle
                rest = [rest, mini_buffers(i)]
            end do
            call move_alloc(rest, mini_buffers)
        end if
        if (enable) then
            n = size(mini_buffers)
            allocate (rest(n + 1))
            rest(1)%buffer = current_buffer()
            do i = 1, n
                rest(i + 1) = mini_buffers(i)
            end do
            call move_alloc(rest, mini_buffers)
        end if
        ! Always hide the current password.
        if (size(mini_buffers) > 0) then
            old = current_buffer()
            call set_buffer(mini_buffers(1)%buffer, st)
            call read_passwd_toggle_visibility(st, force=.true.)
            call set_buffer(old, st)
        end if
    end subroutine read_passwd_mode_body

    !> Toggle visibility of password in minibuffer.  Port of
    !> auth-source.el read-passwd-mode (the command).
    subroutine read_passwd_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%read_passwd_mode%command(status)
    end subroutine read_passwd_mode

    ! The minibuffer setup of `read_passwd'.
    recursive subroutine read_passwd_setup()
        type(status_type) :: st
        minibuf = current_buffer()
        ! Turn off electricity.
        call post_self_insert_hook%clear()
        buffers(cur)%undo_enabled = .false.
        call use_local_map(minor_modes%read_passwd_mode%map, st)
        call minor_modes%read_passwd_mode%enable(st)
        call post_command_hook%add(read_passwd_hide_password, local=.true.)
    end subroutine read_passwd_setup

    !> Read a password, prompting with PROMPT, and return password as a
    !> string.  If optional CONFIRM is true, read the password twice to
    !> make sure.  Optional DEFAULT is a default password to use instead
    !> of empty input.
    !>
    !> This function echoes `*' for each character that the user types.
    !> You could set `read_hide_char' to another hiding character,
    !> though.
    !>
    !> Once the caller uses the password, it can erase the password by
    !> overwriting STRING.  Port of auth-source.el:read-passwd.
    recursive subroutine read_passwd(prompt, result, status, confirm, default)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: confirm
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: first, second
        type(handle_type) :: old
        type(status_type) :: st
        logical :: confirming, saved_recursive
        integer(int32) :: saved_hide
        status = status_type()
        confirming = .false.
        if (present(confirm)) confirming = confirm
        if (confirming) then
            do
                call read_passwd(prompt, first, status, default=default)
                if (.not. status%ok()) return
                call read_passwd("Confirm password: ", second, status, default=default)
                if (.not. status%ok()) return
                if (first == second .and. len(first) == len(second)) then
                    result = first
                    second(:) = " "
                    return
                end if
                first(:) = " "
                second(:) = " "
                call message("Password not repeated accurately; please start over")
                call sit_for(1000)
            end do
        end if
        minibuffer_setup_function => read_passwd_setup
        saved_recursive = enable_recursive_minibuffers
        saved_hide = read_hide_char
        enable_recursive_minibuffers = .true.
        if (read_hide_char < 0) read_hide_char = iachar("*")
        ! "t" = "no history"
        call read_string(prompt, result, status, history="t", default=default)
        enable_recursive_minibuffers = saved_recursive
        read_hide_char = saved_hide
        if (buffer_live_p(minibuf)) then
            old = current_buffer()
            call set_buffer(minibuf, st)
            call minor_modes%read_passwd_mode%disable(st)
            ! Not sure why but it seems that there might be cases where
            ! the minibuffer is not always properly reset later on, so
            ! undo whatever we've done here (bug#11392).
            call post_command_hook%remove(read_passwd_hide_password, local=.true.)
            call post_self_insert_hook%kill_local()
            ! And of course, don't keep the sensitive data around.
            call erase_buffer(st)
            call set_buffer(old, st)
        end if
    end subroutine read_passwd

    !> Define `read_passwd_mode' and its keymap `read_passwd_map'
    !> (keymap used while reading passwords).
    subroutine syms_of_auth_source()
        type(status_type) :: st
        call minor_modes%read_passwd_mode%define("read_passwd_mode", body=read_passwd_mode_body, keymap=.true.)
        call set_keymap_parent(minor_modes%read_passwd_mode%map, minibuffer_local_map, st)
        ! bug#12570
        call keymap_set(minor_modes%read_passwd_mode%map, "C-u", "delete_minibuffer_contents", st)
        call keymap_set(minor_modes%read_passwd_mode%map, "TAB", "read_passwd_toggle_visibility", st)
        commands%read_passwd_toggle_visibility%run => cmd_read_passwd_toggle_visibility
        commands%read_passwd_mode%run => read_passwd_mode
    end subroutine syms_of_auth_source
end module auth_source_fm
