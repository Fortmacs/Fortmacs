! test_dispnew_mod.f90 -- tests for dispnew_mod and frame_mod: the bytes an
! xterm receives when the desired screen changes, frame size changes, and
! suspending Emacs (keyboard_suspend_mod).
! Own tests; upstream has no tests for frame updates.
!
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

module test_dispnew_mod
    use, intrinsic :: iso_fortran_env, only: int32
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use error_mod, only: status_type
    use terminfo_mod, only: terminfo_type, builtin_xterm
    use termchar_mod, only: tty_take_output
    use term_mod, only: init_tty_entry, delete_tty
    use dispextern_mod, only: glyph_type
    use frame_mod, only: frame_type
    use dispnew_mod, only: adjust_frame_glyphs, set_desired_row, update_frame, internal_show_cursor, &
                           is_internal_show_cursor, send_string_to_terminal, redraw_frame, change_frame_size, &
                           do_pending_window_change, is_frame_size_change_delayed, handle_window_change_signal
    use frame_mod, only: selected_frame, frame_lines, frame_cols
    use handle_mod, only: handle_type, null_handle
    use window_mod, only: windows, delete_window_tree, delete_window_object, split_window_internal, &
                          set_window_buffer_internal
    use buffer_mod, only: get_or_make_buffer
    use xdisp_mod, only: init_frame_windows
    use sysdep_mod, only: open_pty, close_fd, set_tty_size, emacs_tty_type, emacs_get_tty, signal_disposition_type, &
                          ignore_signal, restore_signal, emacs_getpid, emacs_getpgrp, sys_setpgid, SIGTSTP
    use keyboard_mod, only: suspend_emacs, suspend_hook, suspend_resume_hook, discard_input
    implicit none(type, external)
    private

    public :: collect_dispnew_mod

    character, parameter :: ESC = achar(27)
    !> What tty_update_end sends to show a hidden cursor.
    !> The functions of suspend_hook and suspend_resume_hook that ran, in
    !> order, and what the terminal had been sent when each ran.
    character(len=:), allocatable :: suspend_trace
    integer :: output_at_suspend = -1, output_at_resume = -1
    logical :: garbaged_at_resume = .false.
    character(len=*), parameter :: SHOW = ESC//"[?12l"//ESC//"[?25h"//ESC//"[?12;25h", &
                                   HIDE = ESC//"[?25l"

contains

    !> Collect all exported unit tests.
    subroutine collect_dispnew_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("first-update-redraws", test_first_update), &
                    new_unittest("unchanged-sends-nothing", test_unchanged), &
                    new_unittest("changed-tail", test_changed_tail), &
                    new_unittest("insert-character", test_insert), &
                    new_unittest("delete-character", test_delete), &
                    new_unittest("short-suffix-is-rewritten", test_short_suffix), &
                    new_unittest("shorter-line", test_shorter), &
                    new_unittest("no-char-ins-del", test_no_ins_del), &
                    new_unittest("last-row-first-and-cursor", test_order_cursor), &
                    new_unittest("bottom-right-cell", test_bottom_right), &
                    new_unittest("internal-show-cursor", test_show_cursor), &
                    new_unittest("send-string-to-terminal", test_send_string), &
                    new_unittest("redraw-frame", test_redraw_frame), &
                    new_unittest("change-frame-size", test_change_frame_size), &
                    new_unittest("change-frame-size/windows", test_change_frame_size_windows), &
                    new_unittest("window-change-signal", test_window_change_signal), &
                    new_unittest("suspend-emacs", test_suspend_emacs) &
                    ]
    end subroutine collect_dispnew_mod

    function g(text) result(glyphs)
        character(len=*), intent(in) :: text
        type(glyph_type) :: glyphs(len(text))
        integer :: k

        do k = 1, len(text)
            glyphs(k)%ch = iachar(text(k:k))
        end do
    end function g

    !> A built-in xterm 80x24 frame already showing `line` in row 0 (the
    !> first update's output is discarded).
    subroutine frame_showing(f, line)
        type(frame_type), intent(out) :: f
        character(len=*), intent(in) :: line
        type(terminfo_type) :: ti
        type(status_type) :: st
        character(len=:), allocatable :: discarded

        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        call adjust_frame_glyphs(f)
        call set_desired_row(f, 0, g(line))
        call update_frame(f, st)
        discarded = tty_take_output(f%tty)
    end subroutine frame_showing

    !> Output of updating `f` with row 0 changed to `line`.
    function update_row0(f, line) result(out)
        type(frame_type), intent(inout) :: f
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: out
        type(status_type) :: st

        call set_desired_row(f, 0, g(line))
        call update_frame(f, st)
        out = tty_take_output(f%tty)
    end function update_row0

    subroutine test_first_update(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(status_type) :: st

        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        call adjust_frame_glyphs(f)
        call check(error, f%garbaged .and. f%current_matrix%nrows == 24, "garbaged")
        if (allocated(error)) return
        call set_desired_row(f, 0, g("hello"))
        call update_frame(f, st)
        call check(error, tty_take_output(f%tty), ESC//"[H"//ESC//"[2J"//HIDE//"hello"// &
                   ESC//"[K"//SHOW)
        if (allocated(error)) return
        call check(error,.not. f%garbaged .and. f%current_matrix%rows(0)%used == 5, "current")
    end subroutine test_first_update

    subroutine test_unchanged(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f

        call frame_showing(f, "hello")
        call check(error, update_row0(f, "hello"), "")
    end subroutine test_unchanged

    subroutine test_changed_tail(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f

        call frame_showing(f, "hello")
        ! Only "p!" is new; the cursor backs up two columns to write it.
        call check(error, update_row0(f, "help!"), achar(8)//achar(8)//HIDE//"p!"//SHOW)
    end subroutine test_changed_tail

    subroutine test_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        character(len=*), parameter :: tail = "0123456789abcdefghijklmnopqrst"

        call frame_showing(f, "ab"//tail)
        ! A long unchanged suffix: insert one character instead.
        call check(error, update_row0(f, "abX"//tail), ESC//"[1;3H"//HIDE//ESC//"[1@X"//SHOW)
    end subroutine test_insert

    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        character(len=*), parameter :: tail = "0123456789abcdefghijklmnopqrst"

        call frame_showing(f, "abX"//tail)
        call check(error, update_row0(f, "ab"//tail), ESC//"[1;3H"//HIDE//ESC//"[1P"//SHOW)
    end subroutine test_delete

    !> A suffix shorter than the cost of insertion is simply rewritten.
    subroutine test_short_suffix(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f

        call frame_showing(f, "abcdefgh")
        call check(error, update_row0(f, "abXcdefgh"), ESC//"[1;3H"//HIDE//"Xcdefgh"//SHOW)
    end subroutine test_short_suffix

    subroutine test_shorter(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f

        call frame_showing(f, "hello")
        call check(error, update_row0(f, "he"), achar(8)//achar(8)//achar(8)//ESC//"[K")
    end subroutine test_shorter

    !> Without character insertion, each differing run is rewritten.
    subroutine test_no_ins_del(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f

        call frame_showing(f, "abcdef")
        f%tty%char_ins_del_ok = .false.
        ! From column 6: CR and one right is cheapest to column 1.
        call check(error, update_row0(f, "aXcdYf"), achar(13)//ESC//"[C"//HIDE//"X"// &
                   ESC//"[1;5HY"//SHOW)
    end subroutine test_no_ins_del

    subroutine test_order_cursor(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(status_type) :: st

        call frame_showing(f, "")
        call set_desired_row(f, 1, g("top"))
        call set_desired_row(f, 23, g("echo"))
        f%cursor_vpos = 1
        f%cursor_hpos = 1
        call update_frame(f, st)
        ! The echo area (last row) first, then the rest; the cursor ends up
        ! at row 1, column 1.
        call check(error, tty_take_output(f%tty), ESC//"[24d"//HIDE//"echo"//ESC//"[K"// &
                   ESC//"[H"//achar(10)//"top"//ESC//"[K"//SHOW//achar(8)//achar(8))
    end subroutine test_order_cursor

    !> A full last row: its bottom-right cell is inserted, not written.
    subroutine test_bottom_right(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(status_type) :: st
        character(len=80) :: line
        integer :: k

        call frame_showing(f, "")

        do k = 1, 80
            line(k:k) = achar(iachar("a") + mod(k - 1, 26))
        end do

        call set_desired_row(f, 23, g(line))
        call update_frame(f, st)
        call check(error, tty_take_output(f%tty), ESC//"[24d"//HIDE//line(2:80)//achar(13)// &
                   ESC//"[1@"//line(1:1)//achar(8)//SHOW)
    end subroutine test_bottom_right

    !> internal-show-cursor: the update leaves the cursor hidden while the
    !> selected window's flag is off.
    subroutine test_show_cursor(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(status_type) :: st
        character(len=:), allocatable :: out

        call frame_showing(f, "abc")
        call internal_show_cursor(.false., st)
        call check(error,.not. is_internal_show_cursor(st) .and. st%ok(), "flag off")
        if (allocated(error)) return
        out = update_row0(f, "abd")
        call check(error, index(out, SHOW) == 0, "cursor not shown")
        if (allocated(error)) return
        call internal_show_cursor(.true., st)
        out = update_row0(f, "abe")
        call check(error, index(out, SHOW) > 0 .and. is_internal_show_cursor(st), "cursor shown again")
    end subroutine test_show_cursor

    !> send-string-to-terminal writes the string as it is.
    subroutine test_send_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: out

        out = tty_take_output(selected_frame%tty)
        call send_string_to_terminal(ESC//"]0;title"//achar(7), st)
        out = tty_take_output(selected_frame%tty)
        call check(error, st%ok() .and. out == ESC//"]0;title"//achar(7), out)
    end subroutine test_send_string

    !> redraw-frame clears the screen, and the next update writes every
    !> row again.
    subroutine test_redraw_frame(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        character(len=:), allocatable :: out

        call frame_showing(f, "abc")
        call redraw_frame(f)
        out = tty_take_output(f%tty)
        call check(error, index(out, ESC//"[H"//ESC//"[2J") > 0 .or. index(out, ESC//"[2J") > 0, "cleared")
        if (allocated(error)) return
        out = update_row0(f, "abc")
        call check(error, index(out, "abc") > 0, "row written again")
    end subroutine test_redraw_frame

    !> change_frame_size gives the frame, its windows and its matrices the
    !> new size, and the frame is redrawn; a frame no smaller than its
    !> windows can safely be.
    subroutine test_change_frame_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(status_type) :: st

        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        call adjust_frame_glyphs(f)
        call init_frame_windows(f)
        f%garbaged = .false.
        call change_frame_size(f, 100, 30, .false., .false.)
        call check(error, frame_cols(f) == 100 .and. frame_lines(f) == 30 .and. f%garbaged, "frame size")
        if (allocated(error)) return

        associate (r => windows(f%root_window%index), m => windows(f%minibuffer_window%index))
            call check(error, r%top_line == 0 .and. r%total_lines == 29 .and. r%total_cols == 100, "root window")
            if (allocated(error)) return
            call check(error, m%top_line == 29 .and. m%total_lines == 1 .and. m%total_cols == 100, "minibuffer window")
            if (allocated(error)) return
        end associate

        call check(error, f%current_matrix%nrows == 30 .and. f%current_matrix%ncols == 100 .and. &
                   f%desired_matrix%nrows == 30 .and. f%desired_matrix%ncols == 100, "matrices")
        if (allocated(error)) return
        ! Too small: one text line, the minibuffer line and two columns.
        call change_frame_size(f, 1, 1, .false., .false.)
        call check(error, frame_cols(f) == 2 .and. frame_lines(f) == 2 .and. &
                   windows(f%root_window%index)%total_lines == 1, "minimum size")
        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
    end subroutine test_change_frame_size

    !> The windows of a split frame share its new size.
    subroutine test_change_frame_size_windows(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(status_type) :: st
        type(handle_type) :: lower, buffer

        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        call adjust_frame_glyphs(f)
        call init_frame_windows(f)
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_window_buffer_internal(f%selected_window, buffer, .false.)
        call split_window_internal(f%selected_window, .false., 11, f%root_window, lower, st)
        call check(error, st%ok(), "split")
        if (allocated(error)) return
        call change_frame_size(f, 60, 47, .false., .false.)

        associate (upper => windows(f%selected_window%index), low => windows(lower%index), &
                   m => windows(f%minibuffer_window%index))
            call check(error, upper%top_line == 0 .and. upper%total_cols == 60 .and. low%total_cols == 60 .and. &
                       low%top_line == upper%total_lines .and. upper%total_lines + low%total_lines == 46 .and. &
                       m%top_line == 46, "windows")
        end associate

        call delete_window_tree(f%root_window)
        call delete_window_object(f%minibuffer_window)
    end subroutine test_change_frame_size_windows

    !> A size change read from the terminal waits for
    !> do_pending_window_change, which makes it.
    subroutine test_window_change_signal(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: saved
        type(terminfo_type) :: ti
        type(status_type) :: st
        integer(int32) :: master, slave

        call open_pty(master, slave, st)
        call check(error, st%ok(), "open_pty")
        if (allocated(error)) return
        call set_tty_size(slave, 80_int32, 24_int32, st)
        saved = selected_frame
        selected_frame%root_window = null_handle()
        selected_frame%selected_window = null_handle()
        selected_frame%minibuffer_window = null_handle()
        call builtin_xterm(ti)
        call init_tty_entry(selected_frame%tty, ti, "xterm-256color", slave, st)
        ! Output is collected, not written to the pty.
        selected_frame%tty%output = -1
        call adjust_frame_glyphs(selected_frame)
        call init_frame_windows(selected_frame)
        call set_tty_size(slave, 72_int32, 20_int32, st)
        call handle_window_change_signal()
        call check(error, is_frame_size_change_delayed(selected_frame) .and. frame_cols(selected_frame) == 80, &
                   "delayed")

        if (.not. allocated(error)) then
            call do_pending_window_change(.false.)
            call check(error,.not. is_frame_size_change_delayed(selected_frame) .and. &
                       frame_cols(selected_frame) == 72 .and. frame_lines(selected_frame) == 20 .and. &
                       windows(selected_frame%root_window%index)%total_lines == 19 .and. &
                       selected_frame%current_matrix%nrows == 20, "changed")
        end if

        call delete_window_tree(selected_frame%root_window)
        call delete_window_object(selected_frame%minibuffer_window)
        call delete_tty(selected_frame%tty, st)
        selected_frame = saved
        call close_fd(slave)
        call close_fd(master)
    end subroutine test_window_change_signal

    !> A function of suspend_hook.
    subroutine on_suspend(status)
        type(status_type), intent(out) :: status

        status = status_type()
        suspend_trace = suspend_trace//"suspend "
        output_at_suspend = int(selected_frame%tty%olen)
    end subroutine on_suspend

    !> A function of suspend_resume_hook.
    subroutine on_resume(status)
        type(status_type), intent(out) :: status

        status = status_type()
        suspend_trace = suspend_trace//"resume"
        output_at_resume = int(selected_frame%tty%olen)
        garbaged_at_resume = selected_frame%garbaged
    end subroutine on_resume

    !> suspend-emacs runs suspend_hook, gives the terminal back (its last
    !> line cleared, full-screen mode left, its modes restored), stops the
    !> process, then takes the terminal again, redraws the frame and runs
    !> suspend_resume_hook.  The test process first gets a process group
    !> of its own and ignores SIGTSTP, so the signal stops nothing.
    subroutine test_suspend_emacs(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: saved
        type(terminfo_type) :: ti
        type(status_type) :: st, suspend_status
        type(signal_disposition_type) :: old_tstp
        type(emacs_tty_type) :: original, now
        character(len=:), allocatable :: out, rmcup, smcup
        integer(int32) :: pgrp, err, master, slave

        pgrp = emacs_getpgrp()

        if (pgrp == emacs_getpid()) then
            call skip_test(error, "the test process leads its process group, which other processes may share")
            return
        end if

        call sys_setpgid(0_int32, 0_int32, err)

        if (err /= 0) then
            call skip_test(error, "setpgid(0, 0) failed")
            return
        end if

        call ignore_signal(SIGTSTP, old_tstp)
        call open_pty(master, slave, st)

        if (.not. st%ok()) then
            call restore_signal(SIGTSTP, old_tstp)
            call sys_setpgid(0_int32, pgrp, err)
            call check(error, .false., "open_pty")
            return
        end if

        call set_tty_size(slave, 80_int32, 24_int32, st)
        call emacs_get_tty(slave, original, st)
        saved = selected_frame
        call builtin_xterm(ti)
        call init_tty_entry(selected_frame%tty, ti, "xterm-256color", slave, st)
        ! Output is collected, not written to the pty.
        selected_frame%tty%output = -1
        out = tty_take_output(selected_frame%tty)
        call discard_input()
        suspend_trace = ""
        call suspend_hook%append(on_suspend)
        call suspend_resume_hook%append(on_resume)
        selected_frame%garbaged = .false.
        call suspend_emacs(suspend_status)
        call suspend_hook%remove(on_suspend)
        call suspend_resume_hook%remove(on_resume)
        out = tty_take_output(selected_frame%tty)
        rmcup = "?"
        smcup = "?"
        if (allocated(selected_frame%tty%ts_end_termcap_modes)) rmcup = selected_frame%tty%ts_end_termcap_modes
        if (allocated(selected_frame%tty%ts_termcap_modes)) smcup = selected_frame%tty%ts_termcap_modes
        call emacs_get_tty(slave, now, st)
        ! The modes saved again on resuming are the terminal's own.
        call check(error, selected_frame%tty%modes_saved .and. &
                   selected_frame%tty%old_tty%main%c_lflag == original%main%c_lflag .and. &
                   now%main%c_lflag /= original%main%c_lflag, "terminal modes")
        call delete_tty(selected_frame%tty, st)
        selected_frame = saved
        call close_fd(slave)
        call close_fd(master)
        call restore_signal(SIGTSTP, old_tstp)
        call sys_setpgid(0_int32, pgrp, err)
        if (allocated(error)) return
        call check(error, suspend_status%ok() .and. suspend_trace == "suspend resume", suspend_trace)
        if (allocated(error)) return
        call check(error, output_at_suspend == 0 .and. garbaged_at_resume, "hooks ran outside the terminal change")
        if (allocated(error)) return
        ! The last line cleared, full-screen mode left, then entered again.
        call check(error, output_at_resume == len(out) .and. index(out, achar(27)//"[24;1H"//achar(27)//"[K") == 1 &
                   .and. index(out, rmcup) > 0 .and. index(out, smcup) > index(out, rmcup), out)
    end subroutine test_suspend_emacs
end module test_dispnew_mod
