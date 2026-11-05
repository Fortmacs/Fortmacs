! fortmacs_diff_window_mod.f90 -- the windows of an Ediff session.
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
!> (and C) in windows split by `fortmacs_diff_split_window_function', the
!> control buffer in a window across the bottom, and the control
!> buffer's mode line naming the current difference.
module fortmacs_diff_window_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: ieee_arithmetic, only: ieee_rint
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, header_line_format_type
    use window_mod, only: windows, select_window, split_window, delete_other_windows, set_window_buffer, &
                          set_window_dedication, get_buffer_window, is_window_live, is_window_minibuffer, other_window, &
                          window_buffer, same_handle, pop_to_buffer, split_window_below, split_window_right, window_list_1, &
                          window_top_line, window_total_height, switch_to_buffer, window_minimum_height, DEDICATED_NONE
    use frame_mod, only: selected_frame
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, fortmacs_diff_split_window_procedure, &
                                          fortmacs_diff_before_setup_windows_hook, fortmacs_diff_after_setup_windows_hook, &
                                          FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_B, FORTMACS_DIFF_BUFFER_TYPE_C, &
                                          FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR, fortmacs_diff_show_ancestor
    use fortmacs_diff_utilities_mod, only: set_up_fortmacs_diff_control_buffer
    implicit none(type, external)
    private

    public :: fortmacs_diff_window_setup_procedure, set_up_fortmacs_diff_windows, set_up_fortmacs_diff_windows_default, &
              set_up_fortmacs_diff_windows_plain, &
              set_up_fortmacs_diff_windows_plain_compare, set_up_fortmacs_diff_windows_plain_merge, &
              select_lowest_fortmacs_diff_window, &
              refresh_fortmacs_diff_mode_lines, make_fortmacs_diff_wide_control_buffer_id, &
              get_fortmacs_diff_visible_buffer_window, is_fortmacs_diff_window_configuration_kept, &
              fortmacs_diff_window_configuration, initialize_fortmacs_diff_wind

    abstract interface
        !> A function that sets up the windows of an Ediff session
        !> (`fortmacs_diff_window_setup_function'): BUFFER_A, BUFFER_B, BUFFER_C
        !> (null in 2-way jobs) and CONTROL_BUFFER.  Not a port.
        recursive subroutine fortmacs_diff_window_setup_procedure(buffer_a, buffer_b, buffer_c, control_buffer, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
            type(status_type), intent(out) :: status
        end subroutine fortmacs_diff_window_setup_procedure
    end interface

    !> Function called to set up windows.
    !> On a text terminal, Ediff sets up its windows with
    !> `set_up_fortmacs_diff_windows_plain', which does everything in one frame;
    !> `set_up_fortmacs_diff_windows_default' (the default) does the same.
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
    !> multiframe setup waits for the GRAPHICAL_USER_INTERFACE).
    procedure(fortmacs_diff_window_setup_procedure), pointer, public :: fortmacs_diff_window_setup_function => null()

    !> The function used to split the main window between buffer-A and
    !> buffer-B.
    !> You can set it to a horizontal split instead of the default
    !> vertical split by setting this variable to `split_window_right'.
    !> You can also have your own function to do fancy splits as long as
    !> that function returns the new window it made.  Port of ediff-wind.el
    !> ediff-split-window-function (`split-window-vertically' and
    !> `split-window-horizontally' are `split_window_below' and
    !> `split_window_right').
    procedure(fortmacs_diff_split_window_procedure), pointer, public :: fortmacs_diff_split_window_function => null()

    !> The function used to split the main window between buffer-A and
    !> buffer-B.
    !> You can set it to a vertical split instead of the default
    !> horizontal split by setting this variable to `split_window_below'.
    !> You can also have your own function to do fancy splits as long as
    !> that function returns the new window it made.  Port of ediff-wind.el
    !> ediff-merge-split-window-function (`split_window_right' is
    !> `split-window-horizontally').
    procedure(fortmacs_diff_split_window_procedure), pointer, public :: fortmacs_diff_merge_split_window_function => null()

contains

    !> Set up the window configuration of the session of CONTROL_BUFFER,
    !> unless it is still as Ediff left it: run
    !> `fortmacs_diff_before_setup_windows_hook', `fortmacs_diff_window_setup_function' and
    !> `fortmacs_diff_after_setup_windows_hook'.  Port of
    !> ediff-wind.el:ediff-setup-windows.
    recursive subroutine set_up_fortmacs_diff_windows(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status
        logical :: in_minibuffer, kept

        call fortmacs_diff_before_setup_windows_hook%execute(status)
        if (.not. status%ok()) return
        ! Make sure we are not in the minibuffer window when we try to
        ! delete all other windows.
        in_minibuffer = is_window_minibuffer(selected_frame%selected_window, status)
        if (.not. status%ok()) return

        if (in_minibuffer) then
            call other_window(1_int64, status)
            if (.not. status%ok()) return
        end if

        kept = is_fortmacs_diff_window_configuration_kept(control_buffer, status)
        if (.not. status%ok()) return

        if (.not. kept) then
            ! In case user did a no-no on a tty.
            if (.not. associated(selected_frame%redisplay_interface) .or. .not. associated(fortmacs_diff_window_setup_function)) &
                then
                call set_up_fortmacs_diff_windows_plain(buffer_a, buffer_b, buffer_c, control_buffer, status)
            else
                call fortmacs_diff_window_setup_function(buffer_a, buffer_b, buffer_c, control_buffer, status)
            end if

            if (.not. status%ok()) return
        end if

        call fortmacs_diff_after_setup_windows_hook%execute(status)
    end subroutine set_up_fortmacs_diff_windows

    !> Set up the windows: in one frame (a separate control frame on a
    !> graphical display waits for the GRAPHICAL_USER_INTERFACE).  Port of
    !> ediff-wind.el:ediff-setup-windows-default.
    recursive subroutine set_up_fortmacs_diff_windows_default(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status

        call set_up_fortmacs_diff_windows_plain(buffer_a, buffer_b, buffer_c, control_buffer, status)
    end subroutine set_up_fortmacs_diff_windows_default

    !> Just set up 3 windows: the merge layout in merge jobs, else the
    !> comparison layout.  Port of ediff-wind.el:ediff-setup-windows-plain.
    recursive subroutine set_up_fortmacs_diff_windows_plain(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = session_of(control_buffer)
        if (k == 0) return

        if (fortmacs_diff_sessions(k)%merge_job) then
            call set_up_fortmacs_diff_windows_plain_merge(buffer_a, buffer_b, buffer_c, control_buffer, status)
        else
            call set_up_fortmacs_diff_windows_plain_compare(buffer_a, buffer_b, buffer_c, control_buffer, status)
        end if
    end subroutine set_up_fortmacs_diff_windows_plain

    !> Select the lowest window of the frame: the first one, from the
    !> selected window on in the cyclic order of windows, whose bottom
    !> edge is lowest.  Port of ediff-wind.el:ediff-select-lowest-window.
    recursive subroutine select_lowest_fortmacs_diff_window(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: cycle_order(:)
        type(handle_type) :: lowest_window
        integer :: bottom_edge, next_bottom_edge, j

        cycle_order = window_list_1(selected_frame%selected_window, status=status)
        if (.not. status%ok()) return
        if (size(cycle_order) == 0) return
        lowest_window = cycle_order(1)
        bottom_edge = -1

        do j = 1, size(cycle_order)
            next_bottom_edge = window_top_line(cycle_order(j), status)
            if (.not. status%ok()) return
            next_bottom_edge = next_bottom_edge + window_total_height(cycle_order(j), status)
            if (.not. status%ok()) return

            if (bottom_edge < next_bottom_edge) then
                bottom_edge = next_bottom_edge
                lowest_window = cycle_order(j)
            end if
        end do

        call select_window(lowest_window, status)
    end subroutine select_lowest_fortmacs_diff_window

    !> The windows of merge jobs: the control window at the bottom, above
    !> it buffers A and B split by the session's split function (by
    !> default `fortmacs_diff_merge_split_window_function'), and between them and
    !> the control window the merge buffer C, sharing the frame by the
    !> session's `fortmacs_diff_merge_window_share' (with the ancestor beside it
    !> when `fortmacs_diff_show_ancestor').  Port of
    !> ediff-wind.el:ediff-setup-windows-plain-merge.
    recursive subroutine set_up_fortmacs_diff_windows_plain_merge(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status
        procedure(fortmacs_diff_split_window_procedure), pointer :: split_window_function
        type(handle_type) :: wind_a, wind_b, wind_c, wind_ancestor, new, buffer_ancestor
        real :: merge_window_share
        integer :: merge_window_lines, k, saved_window_minimum_height, height
        logical :: with_ancestor

        status = status_type()
        k = session_of(control_buffer)
        if (k == 0) return
        with_ancestor = fortmacs_diff_sessions(k)%merge_with_ancestor_job
        buffer_ancestor = fortmacs_diff_sessions(k)%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR)
        merge_window_share = fortmacs_diff_sessions(k)%merge_window_share
        ! This lets us have local versions of ediff-split-window-function.
        split_window_function => fortmacs_diff_sessions(k)%split_window_function
        if (.not. associated(split_window_function)) split_window_function => split_window_right
        wind_ancestor = null_handle()
        ! window_minimum_height is 1 while the windows are set up.
        saved_window_minimum_height = window_minimum_height
        window_minimum_height = 1

        body: block
            call delete_other_windows(selected_frame%selected_window, status)
            if (.not. status%ok()) exit body
            call set_window_dedication(selected_frame%selected_window, DEDICATED_NONE, status)
            if (.not. status%ok()) exit body
            call split_window_below(new, status)
            if (.not. status%ok()) exit body
            call select_lowest_fortmacs_diff_window(status)
            if (.not. status%ok()) exit body
            call set_up_fortmacs_diff_control_buffer(k, control_buffer, status)
            if (.not. status%ok()) exit body
            ! Go to the upper window and split it betw A, B, and possibly C.
            call other_window(1_int64, status)
            if (.not. status%ok()) exit body
            height = window_total_height(selected_frame%selected_window, status)
            if (.not. status%ok()) exit body
            merge_window_lines = max(2, int(ieee_rint(real(height)*merge_window_share)))
            call switch_to_buffer(buffers(buffer_a%index)%name, status)
            if (.not. status%ok()) exit body
            wind_a = selected_frame%selected_window
            call split_window(wind_a, max(2, height - merge_window_lines), .true., .false., wind_c, status)
            if (.not. status%ok()) exit body
            call select_window(wind_c, status)
            if (.not. status%ok()) exit body
            call switch_to_buffer(buffers(buffer_c%index)%name, status)
            if (.not. status%ok()) exit body

            if (fortmacs_diff_show_ancestor .and. with_ancestor) then
                call split_window_function(wind_ancestor, status)
                if (.not. status%ok()) exit body
                call select_window(wind_ancestor, status)
                if (.not. status%ok()) exit body
                call switch_to_buffer(buffers(buffer_ancestor%index)%name, status)
                if (.not. status%ok()) exit body
            end if

            call select_window(wind_a, status)
            if (.not. status%ok()) exit body
            call split_window_function(wind_b, status)
            if (.not. status%ok()) exit body
            call select_window(wind_b, status)
            if (.not. status%ok()) exit body
            call switch_to_buffer(buffers(buffer_b%index)%name, status)
            if (.not. status%ok()) exit body
            fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_A) = wind_a
            fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_B) = wind_b
            fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_C) = wind_c
            fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR) = wind_ancestor
            call select_lowest_fortmacs_diff_window(status)
            if (.not. status%ok()) exit body
            call set_up_fortmacs_diff_control_buffer(k, control_buffer, status)
        end block body

        window_minimum_height = saved_window_minimum_height
    end subroutine set_up_fortmacs_diff_windows_plain_merge

    !> The windows of all comparison jobs, including 3way jobs: buffers A,
    !> B (and C) in windows made by the session's split function, the
    !> control buffer in a window at the bottom of the frame.  Port of
    !> ediff-wind.el:ediff-setup-windows-plain-compare (with
    !> `window-combination-resize' .false.: the windows of B and C share
    !> what the first split gave B; windows jobs are deferred).
    recursive subroutine set_up_fortmacs_diff_windows_plain_compare(buffer_a, buffer_b, buffer_c, control_buffer, status)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c, control_buffer
        type(status_type), intent(out) :: status
        procedure(fortmacs_diff_split_window_procedure), pointer :: split_window_function
        type(handle_type) :: wind_a, wind_b, wind_c, control_window
        type(status_type) :: st
        logical :: three_way_comparison
        integer :: k

        k = session_of(control_buffer)
        if (k == 0) return
        ! This lets us have local versions of ediff-split-window-function.
        split_window_function => fortmacs_diff_sessions(k)%split_window_function
        if (.not. associated(split_window_function)) split_window_function => split_window_below
        three_way_comparison = fortmacs_diff_sessions(k)%three_way_comparison_job
        call delete_other_windows(selected_frame%selected_window, status)
        if (.not. status%ok()) return
        ! Set up wind-A, wind-B and if necessary wind-C.
        call set_window_dedication(selected_frame%selected_window, DEDICATED_NONE, status)
        if (.not. status%ok()) return
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

        fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_A) = wind_a
        fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_B) = wind_b
        fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_C) = wind_c
        ! The control buffer in a new window at the bottom of the frame
        ! (display-buffer-in-direction, direction bottom); if making a new
        ! window fails, reuse an existing one (its error is not passed on).
        call split_window(selected_frame%root_window, 0, .false., .false., control_window, st)

        if (st%ok()) then
            call set_window_buffer(control_window, control_buffer, status)
            if (.not. status%ok()) return
            call select_window(control_window, status)
        else
            call pop_to_buffer(control_buffer, status)
        end if

        if (.not. status%ok()) return
        call set_up_fortmacs_diff_control_buffer(k, control_buffer, status)
    end subroutine set_up_fortmacs_diff_windows_plain_compare

    !> The session of control buffer BUFFER, 0 if none.  Not a port.
    integer function session_of(buffer) result(k)
        use fortmacs_diff_settings_mod, only: fortmacs_diff_session_index
        type(handle_type), intent(in) :: buffer

        k = fortmacs_diff_session_index(buffer)
    end function session_of

    !> Revise the mode line of session K's control buffer to show which
    !> difference is selected.  Port of ediff-wind.el:ediff-refresh-mode-lines
    !> (the mode lines of buffers A, B and C, which show the state of the
    !> difference, wait for `mode_line_format'; the control buffer's mode
    !> line keeps the standard form around its identification).
    recursive subroutine refresh_fortmacs_diff_mode_lines(k)
        integer, intent(in) :: k
        type(handle_type) :: control

        control = fortmacs_diff_sessions(k)%control_buffer
        if (.not. is_buffer_live(control)) return
        ! Control buffer id.
        buffers(control%index)%mode_line_buffer_identification = make_fortmacs_diff_wide_control_buffer_id(k)
    end subroutine refresh_fortmacs_diff_mode_lines

    !> The control buffer's identification in the mode line: its name and
    !> where session K stands ("diff 2 of 5").  Port of
    !> ediff-wind.el:ediff-make-wide-control-buffer-id.
    function make_fortmacs_diff_wide_control_buffer_id(k) result(id)
        integer, intent(in) :: k
        type(header_line_format_type) :: id
        character(len=64) :: position

        associate (s => fortmacs_diff_sessions(k))
            if (s%current_difference < 0) then
                write (position, '("At start of ", i0, " diffs")') s%number_of_differences
            else if (s%current_difference >= s%number_of_differences) then
                write (position, '("At end of ", i0, " diffs")') s%number_of_differences
            else
                write (position, '("diff ", i0, " of ", i0)') s%current_difference + 1, s%number_of_differences
            end if
        end associate

        call id%append("%b   ")
        call id%append(trim(position), "mode_line_buffer_id")
    end function make_fortmacs_diff_wide_control_buffer_id

    !> A window of the selected frame showing BUFFER, the null handle if
    !> none (or BUFFER is not live).  Port of
    !> ediff-wind.el:ediff-get-visible-buffer-window.
    function get_fortmacs_diff_visible_buffer_window(buffer) result(window)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: window

        window = null_handle()
        if (is_buffer_live(buffer)) window = get_buffer_window(buffer)
    end function get_fortmacs_diff_visible_buffer_window

    !> What `fortmacs_diff_window_configuration_saved' records of session K: its windows,
    !> split function and display.  Not a port: the string
    !> (format "%S%S%S%S%S%S%S%S" ...) of ediff-setup-control-buffer and
    !> ediff-keep-window-config.
    function fortmacs_diff_window_configuration(k) result(configuration)
        integer, intent(in) :: k
        character(len=:), allocatable :: configuration
        integer :: t
        character(len=240) :: windows_text

        associate (s => fortmacs_diff_sessions(k))
            ! The control window, then the windows by buffer type.
            write (windows_text, '(5("#<window ", i0, ".", i0, ">"))') s%control_window%index, &
                s%control_window%generation, (s%windows(t)%index, s%windows(t)%generation, &
                                              t=FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR)
            configuration = trim(windows_text)

            if (associated(s%split_window_function, split_window_right)) then
                configuration = configuration//"split_window_right"
            else if (associated(s%split_window_function, split_window_below)) then
                configuration = configuration//"split_window_below"
            else
                configuration = configuration//"a procedure"
            end if
        end associate
    end function fortmacs_diff_window_configuration

    !> Whether the windows of the session of CONTROL_BUFFER are still as
    !> Ediff set them up, so they need not be set up again.  Port of
    !> ediff-wind.el:ediff-keep-window-config.
    logical function is_fortmacs_diff_window_configuration_kept(control_buffer, status) result(kept)
        type(handle_type), intent(in) :: control_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: shown
        integer :: k, t

        status = status_type()
        kept = .false.
        if (.not. is_buffer_live(control_buffer)) return
        if (.not. same_handle(current_buffer(), control_buffer)) return
        if (buffers(current)%end_of_accessible_portion - buffers(current)%beginning_of_accessible_portion == 0) return
        k = session_of(control_buffer)
        if (k == 0) return

        associate (s => fortmacs_diff_sessions(k))
            if (.not. is_window_live(s%windows(FORTMACS_DIFF_BUFFER_TYPE_A))) return
            if (.not. is_window_live(s%windows(FORTMACS_DIFF_BUFFER_TYPE_B))) return
            ! If buffer C is defined then take it into account.
            if (s%three_way_job .and. .not. is_window_live(s%windows(FORTMACS_DIFF_BUFFER_TYPE_C))) return

            if (s%merge_with_ancestor_job .and. fortmacs_diff_show_ancestor) then
                if (.not. is_window_live(s%windows(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR))) return
            end if

            do t = FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_C
                if (t == FORTMACS_DIFF_BUFFER_TYPE_C .and. .not. s%three_way_job) exit
                shown = window_buffer(s%windows(t), status)
                if (.not. status%ok()) return
                if (.not. same_handle(shown, s%buffers(t))) return
            end do

            if (s%merge_with_ancestor_job .and. fortmacs_diff_show_ancestor) then
                shown = window_buffer(s%windows(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR), status)
                if (.not. status%ok()) return
                if (.not. same_handle(shown, s%buffers(FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR))) return
            end if

            if (.not. allocated(s%window_configuration_saved)) return
            kept = s%window_configuration_saved == fortmacs_diff_window_configuration(k)
        end associate
    end function is_fortmacs_diff_window_configuration_kept

    !> Give the window options their defaults.  Not a port.
    subroutine initialize_fortmacs_diff_wind()
        if (.not. associated(fortmacs_diff_window_setup_function)) then
            fortmacs_diff_window_setup_function => set_up_fortmacs_diff_windows_default
        end if

        if (.not. associated(fortmacs_diff_split_window_function)) fortmacs_diff_split_window_function => split_window_below

        if (.not. associated(fortmacs_diff_merge_split_window_function)) then
            fortmacs_diff_merge_split_window_function => split_window_right
        end if
    end subroutine initialize_fortmacs_diff_wind
end module fortmacs_diff_window_mod
