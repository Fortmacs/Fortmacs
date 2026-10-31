! test_dispnew_fm.f90 -- tests for dispnew_fm and frame_fm: the bytes an
! xterm receives when the desired screen changes.
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

module test_dispnew_fm
    use, intrinsic :: iso_fortran_env, only: int32
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use terminfo_fm, only: terminfo_type, builtin_xterm
    use termchar_fm, only: tty_take_output
    use term_fm, only: init_tty_entry
    use dispextern_fm, only: glyph_type
    use frame_fm, only: frame_type
    use dispnew_fm, only: adjust_frame_glyphs, set_desired_row, update_frame, internal_show_cursor, &
                          is_internal_show_cursor, send_string_to_terminal, redraw_frame
    use frame_fm, only: selected_frame
    implicit none(type, external)
    private

    public :: collect_dispnew_fm

    character, parameter :: ESC = achar(27)
    !> What tty_update_end sends to show a hidden cursor.
    character(len=*), parameter :: SHOW = ESC//"[?12l"//ESC//"[?25h"//ESC//"[?12;25h", &
                                   HIDE = ESC//"[?25l"

contains

    !> Collect all exported unit tests.
    subroutine collect_dispnew_fm(testsuite)
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
                    new_unittest("redraw-frame", test_redraw_frame) &
                    ]
    end subroutine collect_dispnew_fm

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
end module test_dispnew_fm
