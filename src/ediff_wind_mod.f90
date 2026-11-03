! ediff_wind_mod.f90 -- the windows of an Ediff session.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-wind.el (in part: one
! terminal frame, no control frame).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> The window layout of an Ediff session on one frame: buffers A and B
!> (and C) in windows split by `ediff_split_window_function', the
!> control buffer in a window across the bottom, and the control
!> buffer's mode line naming the current difference.
module ediff_wind_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, header_line_format_type
    use window_mod, only: windows, select_window, split_window, delete_other_windows, set_window_buffer, &
                          set_window_dedication, get_buffer_window, is_window_live, is_window_minibuffer, other_window, &
                          window_buffer, same_handle, pop_to_buffer, split_window_below, split_window_right, DEDICATED_NONE
    use frame_mod, only: selected_frame
    use data_mod, only: number_to_string
    use ediff_init_mod, only: ediff_sessions, ediff_split_window_procedure, ediff_before_setup_windows_hook, &
                              ediff_after_setup_windows_hook, EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_B, &
                              EDIFF_BUFFER_TYPE_C, EDIFF_BUFFER_TYPE_ANCESTOR
    use ediff_util_mod, only: set_up_ediff_control_buffer
    implicit none(type, external)
    private

    public :: ediff_window_setup_procedure, set_up_ediff_windows, set_up_ediff_windows_default, set_up_ediff_windows_plain, &
              set_up_ediff_windows_plain_compare, refresh_ediff_mode_lines, make_ediff_wide_control_buffer_id, &
              ediff_get_visible_buffer_window, is_ediff_window_config_kept, ediff_window_config, init_ediff_wind

    abstract interface
        !> A function that sets up the windows of an Ediff session
        !> (`ediff_window_setup_function'): BUFFER_A, BUFFER_B, BUFFER_C
        !> (null in 2-way jobs) and CONTROL_BUFFER.  Not a port.
        recursive subroutine ediff_window_setup_procedure(buffer_a, buffer_b, buffer_c, control_buffer, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
            type(status_type), intent(out) :: status
        end subroutine ediff_window_setup_procedure
    end interface

    !> Function called to set up windows.
    !> On a text terminal, Ediff sets up its windows with
    !> `set_up_ediff_windows_plain', which does everything in one frame;
    !> `set_up_ediff_windows_default' (the default) does the same.
    !>
    !> If you don't like the provided function, write your own one.
    !> The basic guidelines:
    !>     1. It should leave the control buffer current and, if showing,
    !>        the control window selected if showing these windows.
    !>     2. It should set the session's windows to the windows that
    !>        display the corresponding buffers, or the null handle if the
    !>        corresponding window is not shown.
    !>     3. It should accept the following arguments:
    !>        buffer_a, buffer_b, buffer_c, control_buffer
    !>        Buffer C may not be used in jobs that compare only two
    !>        buffers.
    !> Port of ediff-wind.el ediff-window-setup-function (the
    !> multiframe setup waits for the GUI).
    procedure(ediff_window_setup_procedure), pointer, public :: ediff_window_setup_function => null()

    !> The function used to split the main window between buffer-A and
    !> buffer-B.
    !> You can set it to a horizontal split instead of the default
    !> vertical split by setting this variable to `split_window_right'.
    !> You can also have your own function to do fancy splits as long as
    !> that function returns the new window it made.  Port of ediff-wind.el
    !> ediff-split-window-function (`split-window-vertically' and
    !> `split-window-horizontally' are `split_window_below' and
    !> `split_window_right').
    procedure(ediff_split_window_procedure), pointer, public :: ediff_split_window_function => null()

contains

    !> Set up the window configuration of the session of CONTROL_BUFFER,
    !> unless it is still as Ediff left it: run
    !> `ediff_before_setup_windows_hook', `ediff_window_setup_function' and
    !> `ediff_after_setup_windows_hook'.  Port of
    !> ediff-wind.el:ediff-setup-windows.
    recursive subroutine set_up_ediff_windows(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call ediff_before_setup_windows_hook%execute(status)
        if (.not. status%ok()) return

        ! Make sure we are not in the minibuffer window when we try to
        ! delete all other windows.
        if (is_window_minibuffer(selected_frame%selected_window, st)) call other_window(1_int64, st)

        if (.not. is_ediff_window_config_kept(control_buffer)) then
            ! In case user did a no-no on a tty.
            if (.not. associated(selected_frame%rif) .or. .not. associated(ediff_window_setup_function)) then
                call set_up_ediff_windows_plain(buffer_a, buffer_b, buffer_c, control_buffer, status)
            else
                call ediff_window_setup_function(buffer_a, buffer_b, buffer_c, control_buffer, status)
            end if

            if (.not. status%ok()) return
        end if

        call ediff_after_setup_windows_hook%execute(status)
    end subroutine set_up_ediff_windows

    !> Set up the windows: in one frame (a separate control frame on a
    !> graphical display waits for the GUI).  Port of
    !> ediff-wind.el:ediff-setup-windows-default.
    recursive subroutine set_up_ediff_windows_default(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status

        call set_up_ediff_windows_plain(buffer_a, buffer_b, buffer_c, control_buffer, status)
    end subroutine set_up_ediff_windows_default

    !> Just set up 3 windows.  Port of ediff-wind.el:ediff-setup-windows-plain
    !> (the merge layout is deferred with merging).
    recursive subroutine set_up_ediff_windows_plain(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status

        call set_up_ediff_windows_plain_compare(buffer_a, buffer_b, buffer_c, control_buffer, status)
    end subroutine set_up_ediff_windows_plain

    !> The windows of all comparison jobs, including 3way jobs: buffers A,
    !> B (and C) in windows made by the session's split function, the
    !> control buffer in a window at the bottom of the frame.  Port of
    !> ediff-wind.el:ediff-setup-windows-plain-compare (with
    !> `window-combination-resize' .false.: the windows of B and C share
    !> what the first split gave B; windows jobs are deferred).
    recursive subroutine set_up_ediff_windows_plain_compare(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status
        procedure(ediff_split_window_procedure), pointer :: split_window_function
        type(handle_type) :: wind_a, wind_b, wind_c, control_window
        type(status_type) :: st
        logical :: three_way_comparison
        integer :: k

        k = session_of(control_buffer)
        if (k == 0) return
        ! This lets us have local versions of ediff-split-window-function.
        split_window_function => ediff_sessions(k)%split_window_function
        if (.not. associated(split_window_function)) split_window_function => split_window_below
        three_way_comparison = ediff_sessions(k)%three_way_comparison_job
        call delete_other_windows(selected_frame%selected_window, status)
        if (.not. status%ok()) return
        ! Set up wind-A, wind-B and if necessary wind-C.
        call set_window_dedication(selected_frame%selected_window, DEDICATED_NONE, st)
        wind_a = selected_frame%selected_window
        call set_window_buffer(wind_a, buffer_a, status)
        if (.not. status%ok()) return
        call split_window_function(wind_b, status)
        if (.not. status%ok()) return
        call set_window_buffer(wind_b, buffer_b, status)
        if (.not. status%ok()) return
        call select_window(wind_b, status)
        if (.not. status%ok()) return

        if (three_way_comparison) then
            call split_window_function(wind_c, status)
            if (.not. status%ok()) return
            call set_window_buffer(wind_c, buffer_c, status)
            if (.not. status%ok()) return
        end if

        ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_A) = wind_a
        ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_B) = wind_b
        ediff_sessions(k)%windows(EDIFF_BUFFER_TYPE_C) = wind_c
        ! The control buffer in a new window at the bottom of the frame
        ! (display-buffer-in-direction, direction bottom); if making a new
        ! window fails, reuse an existing one.
        call split_window(selected_frame%root_window, 0, .false., .false., control_window, st)

        if (st%ok()) then
            call set_window_buffer(control_window, control_buffer, status)
            if (.not. status%ok()) return
            call select_window(control_window, status)
        else
            call pop_to_buffer(control_buffer, status)
        end if

        if (.not. status%ok()) return
        call set_up_ediff_control_buffer(k, control_buffer)
    end subroutine set_up_ediff_windows_plain_compare

    !> The session of control buffer BUFFER, 0 if none.  Not a port.
    integer function session_of(buffer) result(k)
        use ediff_init_mod, only: ediff_session_index
        type(handle_type), intent(in) :: buffer

        k = ediff_session_index(buffer)
    end function session_of

    !> Revise the mode line of session K's control buffer to show which
    !> difference is selected.  Port of ediff-wind.el:ediff-refresh-mode-lines
    !> (the mode lines of buffers A, B and C, which show the state of the
    !> difference, wait for `mode_line_format'; the control buffer's mode
    !> line keeps the standard form around its identification).
    recursive subroutine refresh_ediff_mode_lines(k)
        integer, intent(in) :: k
        type(handle_type) :: control

        control = ediff_sessions(k)%control_buffer
        if (.not. is_buffer_live(control)) return
        ! Control buffer id.
        buffers(control%index)%mode_line_buffer_identification = make_ediff_wide_control_buffer_id(k)
    end subroutine refresh_ediff_mode_lines

    !> The control buffer's identification in the mode line: its name and
    !> where session K stands ("diff 2 of 5").  Port of
    !> ediff-wind.el:ediff-make-wide-control-buffer-id.
    function make_ediff_wide_control_buffer_id(k) result(id)
        integer, intent(in) :: k
        type(header_line_format_type) :: id
        character(len=:), allocatable :: where

        associate (s => ediff_sessions(k))
            if (s%current_difference < 0) then
                where = "At start of "//number_to_string(s%number_of_differences)//" diffs"
            else if (s%current_difference >= s%number_of_differences) then
                where = "At end of "//number_to_string(s%number_of_differences)//" diffs"
            else
                where = "diff "//number_to_string(s%current_difference + 1)//" of "// &
                        number_to_string(s%number_of_differences)
            end if
        end associate

        call id%append("%b   ")
        call id%append(where, "mode_line_buffer_id")
    end function make_ediff_wide_control_buffer_id

    !> A window of the selected frame showing BUFFER, the null handle if
    !> none (or BUFFER is not live).  Port of
    !> ediff-wind.el:ediff-get-visible-buffer-window.
    function ediff_get_visible_buffer_window(buffer) result(window)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: window

        window = null_handle()
        if (is_buffer_live(buffer)) window = get_buffer_window(buffer)
    end function ediff_get_visible_buffer_window

    !> What `ediff_window_config_saved' records of session K: its windows,
    !> split function and display.  Not a port: the string
    !> (format "%S%S%S%S%S%S%S%S" ...) of ediff-setup-control-buffer and
    !> ediff-keep-window-config.
    function ediff_window_config(k) result(config)
        integer, intent(in) :: k
        character(len=:), allocatable :: config
        integer :: t

        associate (s => ediff_sessions(k))
            config = window_id(s%control_window)

            do t = EDIFF_BUFFER_TYPE_A, EDIFF_BUFFER_TYPE_ANCESTOR
                config = config//window_id(s%windows(t))
            end do

            if (associated(s%split_window_function, split_window_right)) then
                config = config//"split_window_right"
            else if (associated(s%split_window_function, split_window_below)) then
                config = config//"split_window_below"
            else
                config = config//"a procedure"
            end if
        end associate

    contains

        ! The window W as one field.
        function window_id(w) result(id)
            type(handle_type), intent(in) :: w
            character(len=:), allocatable :: id

            id = "#<window "//number_to_string(w%index)//"."//number_to_string(w%generation)//">"
        end function window_id
    end function ediff_window_config

    !> Whether the windows of the session of CONTROL_BUFFER are still as
    !> Ediff set them up, so they need not be set up again.  Port of
    !> ediff-wind.el:ediff-keep-window-config.
    logical function is_ediff_window_config_kept(control_buffer) result(kept)
        type(handle_type), intent(in) :: control_buffer
        type(status_type) :: st
        integer :: k

        kept = .false.
        if (.not. is_buffer_live(control_buffer)) return
        if (.not. same_handle(current_buffer(), control_buffer)) return
        if (buffers(current)%zv - buffers(current)%begv == 0) return
        k = session_of(control_buffer)
        if (k == 0) return

        associate (s => ediff_sessions(k))
            if (.not. is_window_live(s%windows(EDIFF_BUFFER_TYPE_A))) return
            if (.not. is_window_live(s%windows(EDIFF_BUFFER_TYPE_B))) return
            ! If buffer C is defined then take it into account.
            if (s%three_way_job .and. .not. is_window_live(s%windows(EDIFF_BUFFER_TYPE_C))) return
            if (.not. same_handle(window_buffer(s%windows(EDIFF_BUFFER_TYPE_A), st), s%buffers(EDIFF_BUFFER_TYPE_A))) return
            if (.not. same_handle(window_buffer(s%windows(EDIFF_BUFFER_TYPE_B), st), s%buffers(EDIFF_BUFFER_TYPE_B))) return

            if (s%three_way_job) then
                if (.not. same_handle(window_buffer(s%windows(EDIFF_BUFFER_TYPE_C), st), &
                                      s%buffers(EDIFF_BUFFER_TYPE_C))) return
            end if

            if (.not. allocated(s%window_config_saved)) return
            kept = s%window_config_saved == ediff_window_config(k)
        end associate
    end function is_ediff_window_config_kept

    !> Give the window options their defaults.  Not a port.
    subroutine init_ediff_wind()
        if (.not. associated(ediff_window_setup_function)) ediff_window_setup_function => set_up_ediff_windows_default
        if (.not. associated(ediff_split_window_function)) ediff_split_window_function => split_window_below
    end subroutine init_ediff_wind
end module ediff_wind_mod
