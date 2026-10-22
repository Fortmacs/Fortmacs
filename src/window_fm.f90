! window_fm.f90 -- windows: a buffer shown in part of a frame.
! Ported from GNU Emacs 31.1 src/window.c and src/window.h (the parts in
! use).
!
! Copyright (C) 1985-1987, 1993-1998, 2000-2026 Free Software Foundation,
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

!> A frame has a root window (no splitting yet) and a one-line minibuffer
!> window, which shows the echo area.  A window's start and point are
!> markers, so they follow insertions and deletions in its buffer.
module window_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle
    use buffer_fm, only: buffers, resolve_buffer
    use marker_fm, only: make_marker, free_marker, set_marker, marker_position
    use lisp_fm, only: value_type
    implicit none(type, external)
    private

    public :: set_window_buffer, window_start, set_window_start, window_point, &
              set_window_point, window_body_lines, free_window
    public :: scroll_up, scroll_down, recenter, move_to_window_line, &
              scroll_up_command, scroll_down_command, recenter_top_bottom, &
              move_to_window_line_top_bottom, quit_window, syms_of_window

    !> Number of lines of continuity when scrolling by screenfuls.
    !> Port of window.c next-screen-context-lines.
    integer, public :: next_screen_context_lines = 2

    !> Move point on scroll error if non-nil.
    !> If nil, scroll commands signal an error when no more scrolling is
    !> possible.  If non-nil, they move point to the beginning or end of
    !> the buffer instead (and then signal at the edge).  Port of
    !> window.el scroll-error-top-bottom.
    logical, public :: scroll_error_top_bottom = .false.

    !> Port of the parts of window.h:struct window in use.
    type, public :: window_type
        !> The buffer shown (contents); the null handle for none.
        type(handle_type) :: buffer
        !> Markers: where the display starts, and point when not selected.
        type(handle_type) :: start, pointm
        !> Position on the frame (0-based rows) and size, in lines and
        !> columns, including the mode line.
        integer :: top_line = 0, total_lines = 0, left_col = 0, total_cols = 0
        !> The buffer shown before `buffer` (window.el window-prev-buffers,
        !> one deep); the null handle for none.
        type(handle_type) :: prev_buffer
        !> The minibuffer window: no mode line.
        logical :: mini = .false.
        !> Where redisplay put the cursor, relative to the window; -1: none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> Columns scrolled off the left (only the minibuffer window
        !> scrolls horizontally so far: auto-hscroll).
        integer :: hscroll = 0
        !> The start was set explicitly; redisplay must keep it.
        logical :: force_start = .false.
        !> Last display ended at window end (for "Bot"/"All" in the mode line).
        integer(int64) :: window_end_pos = 0
        logical :: window_end_valid = .false.
    end type window_type

    ! Scrolling and recentering, in submodule window_scroll_fm: they
    ! move by screen lines (indent_fm), which depends on this module.
    interface
        !> Scroll text of selected window upward ARG lines.
        !> If ARG is omitted or nil, scroll upward by a near full screen.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll downward.  If ARG is the
        !> atom `-', scroll downward by nearly full screen.  When calling
        !> from a program, supply as argument a number, nil, or `-'.
        !> Port of window.c:Fscroll_up.
        module subroutine scroll_up(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_up

        !> Scroll text of selected window down ARG lines.
        !> If ARG is omitted or nil, scroll down by a near full screen.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll upward.  If ARG is the
        !> atom `-', scroll upward by nearly full screen.  When calling
        !> from a program, supply as argument a number, nil, or `-'.
        !> Port of window.c:Fscroll_down.
        module subroutine scroll_down(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_down

        !> Center point in selected window and maybe redisplay frame.
        !> With a numeric prefix argument ARG, recenter putting point on
        !> screen line ARG relative to the selected window.  If ARG is
        !> negative, it counts up from the bottom of the window.  (ARG
        !> should be less than the height of the window.)
        !>
        !> If ARG is omitted or nil, then recenter with point on the middle
        !> line of the selected window; if REDISPLAY is true, also erase the
        !> entire frame and redraw it.  Interactively, REDISPLAY is always
        !> true.
        !>
        !> Just C-u as prefix means put point in the center of the window
        !> and redisplay normally--don't erase and redraw the frame.
        !> Port of window.c:Frecenter (`recenter-redisplay' is `tty').
        module subroutine recenter(arg, redisplay, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            logical, intent(in) :: redisplay
            type(status_type), intent(out) :: status
        end subroutine recenter

        !> Position point relative to window.
        !> ARG nil means position point at center of window.
        !> Else, ARG specifies vertical position within the window;
        !> zero means top of window, negative means relative to bottom
        !> of window, -1 meaning the last fully visible display line
        !> of the window.
        !>
        !> `line` is the screen line of the window point moved to, counting
        !> from the top of the window.  Port of window.c:Fmove_to_window_line.
        module subroutine move_to_window_line(arg, line, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            integer(int64), intent(out) :: line
            type(status_type), intent(out) :: status
        end subroutine move_to_window_line

        !> Scroll text of selected window upward ARG lines; or near full
        !> screen if no ARG.  Interactively, giving this command a numerical
        !> prefix will scroll up by that many lines (and down by that many
        !> lines if the number is negative).  Without a prefix, scroll up by
        !> a full screen.  If given a `C-u -' prefix, scroll a full page
        !> down instead.
        !>
        !> If `scroll-error-top-bottom' is non-nil and `scroll-up' cannot
        !> scroll window further, move cursor to the bottom line.
        !> When point is already on that position, then signal an error.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll downward.
        !>
        !> If ARG is the atom `-', scroll downward by nearly full screen.
        !>
        !> The command \[set-goal-column] can be used to create a
        !> semipermanent goal column for this command.  Port of
        !> window.el:scroll-up-command.
        recursive module subroutine scroll_up_command(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_up_command

        !> Scroll text of selected window down ARG lines; or near full
        !> screen if no ARG.  Interactively, giving this command a numerical
        !> prefix will scroll down by that many lines (and up by that many
        !> lines if the number is negative).  Without a prefix, scroll down
        !> by a full screen.  If given a `C-u -' prefix, scroll a full page
        !> up instead.
        !>
        !> If `scroll-error-top-bottom' is non-nil and `scroll-down' cannot
        !> scroll window further, move cursor to the top line.
        !> When point is already on that position, then signal an error.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll upward.
        !>
        !> If ARG is the atom `-', scroll upward by nearly full screen.
        !>
        !> The command \[set-goal-column] can be used to create a
        !> semipermanent goal column for this command.  Port of
        !> window.el:scroll-down-command.
        recursive module subroutine scroll_down_command(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_down_command

        !> Scroll the window so that current line is in the middle of the
        !> window.  Successive invocations scroll the window in a cyclical
        !> order to put the current line at certain places within the
        !> window, as determined by `recenter-positions'.  By default, the
        !> second invocation puts the current line at the top-most window
        !> line, the third invocation puts it on the bottom-most window
        !> line, and then the order is reused in a cyclical manner.
        !>
        !> With numeric prefix ARG, move current line ARG lines below the
        !> window top.  With plain \[universal-argument], move current line
        !> to window center.  Port of window.el:recenter-top-bottom.
        module subroutine recenter_top_bottom(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine recenter_top_bottom

        !> Position point relative to window.
        !>
        !> With a prefix argument ARG, acts like `move-to-window-line'.
        !>
        !> With no argument, positions point at center of window.
        !> Successive calls position point at positions defined
        !> by `recenter-positions'.  Port of
        !> window.el:move-to-window-line-top-bottom.
        module subroutine move_to_window_line_top_bottom(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine move_to_window_line_top_bottom

        !> Quit WINDOW and bury its buffer.  WINDOW is the selected one: it
        !> shows the buffer it showed before, or some other buffer.  Port of
        !> window.el:quit-window (no KILL, burying or quit-restore
        !> parameters; the selected window shows the current buffer, so this
        !> makes that buffer current).
        module subroutine quit_window(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine quit_window

        !> Register the scrolling commands and `quit-window`.
        module subroutine syms_of_window()
            implicit none(type, external)
        end subroutine syms_of_window
    end interface

contains

    !> Show buffer `buffer` in `w`, starting at its accessible beginning,
    !> with the window's point at the buffer's point.  The buffer shown
    !> before is recorded as `prev_buffer` (window.el
    !> record-window-buffer) unless `record` is false, as for a temporary
    !> display.  Port of window.c:set_window_buffer (no hooks).
    subroutine set_window_buffer(w, buffer, status, record)
        type(window_type), intent(inout) :: w
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: record
        integer(int64) :: i
        logical :: rec
        call resolve_buffer(buffer, i, status)
        if (.not. status%ok()) return
        rec = .true.
        if (present(record)) rec = record
        if (rec .and. w%buffer%index /= 0 .and. .not. (w%buffer%index == buffer%index .and. &
                                                      w%buffer%generation == buffer%generation)) then
            w%prev_buffer = w%buffer
        end if
        if (w%start%index == 0) call make_marker(w%start)
        if (w%pointm%index == 0) call make_marker(w%pointm)
        w%buffer = buffer
        call set_marker(w%start, buffers(i)%begv, status, buffer)
        if (.not. status%ok()) return
        call set_marker(w%pointm, buffers(i)%pt, status, buffer)
        w%force_start = .false.
        w%window_end_valid = .false.
    end subroutine set_window_buffer

    !> Where the display of `w` starts (1 if it shows no buffer).  Port of
    !> window.c:Fwindow_start.
    integer(int64) function window_start(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status
        pos = 1
        if (w%start%index == 0) return
        call marker_position(w%start, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_start

    !> Make `w` start at `pos`; with `noforce`, redisplay may move it to
    !> show point.  Port of window.c:Fset_window_start.
    subroutine set_window_start(w, pos, status, noforce)
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noforce
        if (w%start%index == 0) call make_marker(w%start)
        call set_marker(w%start, pos, status, w%buffer)
        w%force_start = .true.
        if (present(noforce)) w%force_start = .not. noforce
        w%window_end_valid = .false.
    end subroutine set_window_start

    !> The window's point marker position.  (The selected window's point
    !> is its buffer's point; see window.c:Fwindow_point.)
    integer(int64) function window_point(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status
        pos = 1
        if (w%pointm%index == 0) return
        call marker_position(w%pointm, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_point

    !> Port of window.c:Fset_window_point (for a non-selected window).
    subroutine set_window_point(w, pos, status)
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        if (w%pointm%index == 0) call make_marker(w%pointm)
        call set_marker(w%pointm, pos, status, w%buffer)
    end subroutine set_window_point

    !> Lines for text: the total minus the mode line.  Port of
    !> window.c:window_body_height (in lines).
    integer function window_body_lines(w)
        type(window_type), intent(in) :: w
        window_body_lines = w%total_lines
        if (.not. w%mini) window_body_lines = max(0, w%total_lines - 1)
    end function window_body_lines

    !> Free the window's markers.  Not a port (upstream relies on GC).
    subroutine free_window(w)
        type(window_type), intent(inout) :: w
        type(status_type) :: status
        if (w%start%index /= 0) call free_marker(w%start, status)
        if (w%pointm%index /= 0) call free_marker(w%pointm, status)
        w%start = null_handle()
        w%pointm = null_handle()
        w%buffer = null_handle()
    end subroutine free_window
end module window_fm
