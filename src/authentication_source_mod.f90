! authentication_source_mod.f90 -- reading passwords.
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

!> `read_password' and `read_password_mode': reading a password in the
!> minibuffer, its characters shown as `read_hide_character' (TAB shows them,
!> C-u clears the input).
!>
!> Deferred: the rest of auth-source.el (the authentication sources);
!> the mode line icon of `read_password_mode' (icons and
!> `global_mode_string' are not ported); the buffer-local
!> `show_parenthesis_mode', `select_active_regions' and `inhibit__record_character'
!> of the password's minibuffer, and text conversion styles.
module authentication_source_mod
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: list, value_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, erase_buffer
    use editing_functions_mod, only: buffer_size
    use text_properties_mod, only: put_text_property, remove_list_of_text_properties
    use character_mod, only: character_string, MAXIMUM_MULTIBYTE_LENGTH
    use keymap_mod, only: keymap_set, set_keymap_parent, use_local_map
    use keyboard_mod, only: post_command_hook, sit_for
    use editing_commands_mod, only: post_self_insert_hook
    use modes_mod, only: minor_modes
    use commands_mod, only: commands
    use minibuffer_reading_mod, only: minibuffer_prompt_end, minibuffer_setup_function, read_string, &
                                      enable_recursive_minibuffers, minibuffer_local_map, read_hide_character
    use window_mod, only: windows, is_window_valid
    use frame_mod, only: selected_frame
    use display_engine_mod, only: message
    implicit none(type, external)
    private

    public :: read_password, read_password_toggle_visibility, read_password_mode, initialize_authentication_source

    ! Flag indicating whether password in minibuffer is hidden
    ! (read-passwd--password-hidden).
    logical :: password_hidden = .false.

    ! A minibuffer where `read_password' is active.
    type :: buffer_reference_type
        type(handle_type) :: buffer
    end type buffer_reference_type

    ! List of minibuffers where `read-passwd' is active
    ! (read-passwd--mini-buffers), the newest first.
    type(buffer_reference_type), allocatable :: mini_buffers(:)

    ! The minibuffer `read_password' is reading in.
    type(handle_type) :: minibuf

contains

    ! Make password in minibuffer hidden or visible.  Port of
    ! auth-source.el:read-passwd--hide-password.
    recursive subroutine read_password_hide_password(status)
        type(status_type), intent(out) :: status
        character(len=MAXIMUM_MULTIBYTE_LENGTH) :: buffer
        integer(int32) :: nbytes
        integer(int64) :: beginning, i
        type(status_type) :: st
        logical :: changed

        status = status_type()
        beginning = minibuffer_prompt_end()

        if (read_hide_character >= 0) then
            call character_string(read_hide_character, buffer, nbytes, st)
        else
            buffer = "*"
            nbytes = 1
        end if

        do i = 0, buffer_size() - beginning
            if (password_hidden) then
                call put_text_property(i + beginning, i + beginning + 1, value_type(name="display"), &
                                       value_type(text=buffer(1:nbytes)), st)
            else
                call remove_list_of_text_properties(i + beginning, i + beginning + 1, list(value_type(name="display")), changed, st)
            end if

            call put_text_property(i + beginning, i + beginning + 1, value_type(name="help_echo"), &
                                   value_type(text="C-u: Clear password"//achar(10)//"TAB: Toggle password visibility"), st)
        end do
    end subroutine read_password_hide_password

    !> Toggle minibuffer contents visibility.
    !> Adapt also mode line.  If optional FORCE is true, hide the
    !> minibuffer contents.  Port of
    !> auth-source.el:read-passwd-toggle-visibility (no mode line icon).
    recursive subroutine read_password_toggle_visibility(status, force)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: force
        type(handle_type) :: old, mbuf
        type(status_type) :: st
        logical :: forced

        status = status_type()
        forced = .false.
        if (present(force)) forced = force

        if (.not. is_window_valid(selected_frame%minibuffer_window)) then
            call set_error(status, GENERIC_ERROR_CODE, "No active minibuffer")
            return
        end if

        mbuf = windows(selected_frame%minibuffer_window%index)%buffer

        if (.not. is_buffer_live(mbuf)) then
            call set_error(status, GENERIC_ERROR_CODE, "No active minibuffer")
            return
        end if

        ! FIXME: In case of a recursive minibuffer, this may select the
        ! wrong mini-buffer.
        old = current_buffer()
        call set_buffer(mbuf, st)

        if (minor_modes%read_password_mode%enabled()) then
            password_hidden = forced .or. .not. password_hidden
            call read_password_hide_password(st)
        end if

        call set_buffer(old, st)
    end subroutine read_password_toggle_visibility

    subroutine execute_read_password_toggle_visibility(status)
        type(status_type), intent(out) :: status

        call read_password_toggle_visibility(status)
    end subroutine execute_read_password_toggle_visibility

    ! The body of `read_password_mode' (ENABLE is its new state).  Port of
    ! auth-source.el read-passwd-mode (no mode line icon).
    recursive subroutine read_password_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(buffer_reference_type), allocatable :: rest(:)
        type(handle_type) :: old, cb
        type(status_type) :: st
        integer :: i

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

        if (enable) mini_buffers = [buffer_reference_type(current_buffer()), mini_buffers]

        ! Always hide the current password.
        if (size(mini_buffers) > 0) then
            old = current_buffer()
            call set_buffer(mini_buffers(1)%buffer, st)
            call read_password_toggle_visibility(st, force=.true.)
            call set_buffer(old, st)
        end if
    end subroutine read_password_mode_body

    !> Toggle visibility of password in minibuffer.  Port of
    !> auth-source.el read-passwd-mode (the command).
    subroutine read_password_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%read_password_mode%command(status)
    end subroutine read_password_mode

    ! The minibuffer setup of `read_password'.  Not a port.
    recursive subroutine read_password_setup()
        type(status_type) :: st

        minibuf = current_buffer()
        ! Turn off electricity.
        call post_self_insert_hook%clear()
        buffers(current)%undo_enabled = .false.
        call use_local_map(minor_modes%read_password_mode%map, st)
        call minor_modes%read_password_mode%enable(st)
        call post_command_hook%prepend(read_password_hide_password, local=.true.)
    end subroutine read_password_setup

    !> Read a password, prompting with PROMPT, and return password as a
    !> string.  If optional CONFIRM is true, read the password twice to
    !> make sure.  Optional DEFAULT is a default password to use instead
    !> of empty input.
    !>
    !> This function echoes `*' for each character that the user types.
    !> You could set `read_hide_character' to another hiding character,
    !> though.
    !>
    !> Once the caller uses the password, it can erase the password by
    !> overwriting STRING.  Port of auth-source.el:read-passwd.
    recursive subroutine read_password(prompt, result, status, confirm, default)
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
                call read_password(prompt, first, status, default=default)
                if (.not. status%ok()) return
                call read_password("Confirm password: ", second, status, default=default)
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

        minibuffer_setup_function => read_password_setup
        saved_recursive = enable_recursive_minibuffers
        saved_hide = read_hide_character
        enable_recursive_minibuffers = .true.
        if (read_hide_character < 0) read_hide_character = iachar("*")
        ! "t" = "no history"
        call read_string(prompt, result, status, history="t", default=default)
        enable_recursive_minibuffers = saved_recursive
        read_hide_character = saved_hide

        if (is_buffer_live(minibuf)) then
            old = current_buffer()
            call set_buffer(minibuf, st)
            call minor_modes%read_password_mode%disable(st)
            ! Not sure why but it seems that there might be cases where
            ! the minibuffer is not always properly reset later on, so
            ! undo whatever we've done here (bug#11392).
            call post_command_hook%remove(read_password_hide_password, local=.true.)
            call post_self_insert_hook%clear_local()
            ! And of course, don't keep the sensitive data around.
            call erase_buffer(st)
            call set_buffer(old, st)
        end if
    end subroutine read_password

    !> Define `read_password_mode' and its keymap `read_password_map'
    !> (keymap used while reading passwords).  Not a port.
    subroutine initialize_authentication_source()
        type(status_type) :: st

        call minor_modes%read_password_mode%define("read_password_mode", body=read_password_mode_body, keymap=.true.)
        call set_keymap_parent(minor_modes%read_password_mode%map, minibuffer_local_map, st)
        ! bug#12570
        call keymap_set(minor_modes%read_password_mode%map, "C-u", "delete_minibuffer_contents", st)
        call keymap_set(minor_modes%read_password_mode%map, "TAB", "read_password_toggle_visibility", st)
        commands%read_password_toggle_visibility%execute => execute_read_password_toggle_visibility
        commands%read_password_mode%execute => read_password_mode
    end subroutine initialize_authentication_source
end module authentication_source_mod
