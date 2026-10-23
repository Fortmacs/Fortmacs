! test_indent_fm.f90 -- tests for indent_fm and vertical motion in
! simple_fm (next-line, previous-line).
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

!> Own tests: indent-tests.el needs invisible text, and line-move has no
!> ERT tests that run without a display.
module test_indent_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_END_OF_BUFFER, FM_BEGINNING_OF_BUFFER
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type
    use indent_fm, only: current_column, move_to_column, vertical_motion, screen_column, indent_to, &
                         current_indentation, indent_tabs_mode, indent_line_to, indent_relative, &
                         indent_next_tab_stop, indent_accumulate_tab_stops, tab_stop_list, indent_rigidly, &
                         indent_region
    use simple_fm, only: back_to_indentation, fill_prefix
    use editfns_fm, only: buffer_string, point_max
    use keymap_fm, only: key_parse
    use simple_fm, only: line_move, goal_column, line_move_visual
    use keyboard_fm, only: unread_command_events, discard_input, command_loop, last_command
    use lisp_fm, only: nil
    use bindings_fm, only: init_bindings
    use keyboard_fm, only: last_message
    use data_fm, only: kill_local_variable
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur
    use simple_fm, only: region_active_p
    use indent_fm, only: indent_line_function, indent_for_tab_command, indent_region_line_by_line
    implicit none(type, external)
    private

    public :: collect_indent_fm

    character(len=*), parameter :: nl = achar(10)

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

contains

    function itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    subroutine collect_indent_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("current-column", test_current_column), &
                    new_unittest("move-to-column", test_move_to_column), &
                    new_unittest("vertical-motion-lines", test_vertical_motion), &
                    new_unittest("vertical-motion-wrapped", test_wrapped), &
                    new_unittest("next-line-goal-column", test_goal_column), &
                    new_unittest("line-move-errors", test_errors), &
                    new_unittest("logical-line-move", test_logical), &
                    new_unittest("indent-to-and-current-indentation", test_indent_to), &
                    new_unittest("indent-line-to-relative-tab-stops", test_indent_commands), &
                    new_unittest("indent-rigidly-and-region", test_indent_region), &
                    new_unittest("indent-keys", test_indent_keys), &
                    new_unittest("indent-rigidly-transient-map", test_indent_rigidly_map), &
                    new_unittest("indent-for-tab-noindent-fallback", test_noindent_fallback), &
                    new_unittest("indent-region-progress", test_indent_region_progress), &
                    new_unittest("current-indentation-unibyte-nbsp", test_indentation_unibyte) &
                    ]
    end subroutine collect_indent_fm

    !> A fresh buffer holding `text`, point at 1, in a window `cols` wide.
    subroutine enter(text, cols, h)
        character(len=*), intent(in) :: text
        integer, intent(in) :: cols
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        type(window_type), pointer :: sw
        call get_buffer_create("indent-test", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call goto_char(1_int64)
        ! A fresh goal column: the last command wasn't a line move.
        last_command = nil()
        selected_frame%minibuffer_selected = .false.
        sw => frame_selected_window_ref()
        sw%total_cols = cols
        sw%divider = .false.
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        type(window_type), pointer :: sw
        call kill_buffer(h, st)
        sw => frame_selected_window_ref()
        sw%total_cols = 0
    end subroutine leave

    subroutine test_current_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        ! "a<TAB>b", C-a (^A), a raw byte, then a wide character.
        call enter("a"//achar(9)//"b"//achar(1)//"c"//achar(195)//achar(169)//"x"// &
                   achar(228)//achar(184)//achar(173)//"y", 80, h)
        call check(error, current_column() == 0)
        if (allocated(error)) return
        call goto_char(3_int64)
        call check(error, current_column() == 8, "tab to column 8")
        if (allocated(error)) return
        call goto_char(5_int64)
        call check(error, current_column() == 11, "^A is two columns")
        if (allocated(error)) return
        call goto_char(8_int64)
        call check(error, current_column() == 14, "é and x")
        if (allocated(error)) return
        call goto_char(9_int64)
        call check(error, current_column() == 16, "a wide character is two columns")
        call leave(h)
    end subroutine test_current_column

    subroutine test_move_to_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: reached
        call enter("ab"//achar(9)//"cd"//nl//"next", 80, h)
        call move_to_column(1_int64, reached)
        call check(error, point() == 2 .and. reached == 1)
        if (allocated(error)) return
        ! Inside the tab: point goes after it.
        call move_to_column(5_int64, reached)
        call check(error, point() == 4 .and. reached == 8, "after the tab")
        if (allocated(error)) return
        ! Past the end of the line: the end of the line.
        call move_to_column(50_int64, reached)
        call check(error, point() == 6 .and. reached == 10, "end of line")
        call leave(h)
    end subroutine test_move_to_column

    subroutine test_vertical_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: moved
        call enter("one"//nl//"two"//nl//"three", 80, h)
        call vertical_motion(1_int64, moved)
        call check(error, moved == 1 .and. point() == 5)
        if (allocated(error)) return
        call vertical_motion(5_int64, moved, 3)
        call check(error, moved == 1 .and. point() == 12, "stops on the last line, at column 3")
        if (allocated(error)) return
        call vertical_motion(-9_int64, moved)
        call check(error, moved == -2 .and. point() == 1)
        call leave(h)
    end subroutine test_vertical_motion

    !> Lines wider than the window continue on the next screen line.
    subroutine test_wrapped(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: moved
        ! An 11-column window shows 10 characters per row.
        call enter("abcdefghijklmnopqrstuvwxy"//nl//"z", 11, h)
        call goto_char(4_int64)
        call vertical_motion(1_int64, moved, 3)
        call check(error, moved == 1 .and. point() == 14, "row 2, column 3")
        if (allocated(error)) return
        call check(error, screen_column() == 3)
        if (allocated(error)) return
        call vertical_motion(1_int64, moved, 7)
        call check(error, point() == 26, "the last row is short: its end")
        if (allocated(error)) return
        call vertical_motion(1_int64, moved, 7)
        call check(error, moved == 1 .and. point() == 28, "next line")
        if (allocated(error)) return
        ! Back up into the continued row's end: the last character on it.
        call goto_char(1_int64)
        call vertical_motion(1_int64, moved, 20)
        call check(error, point() == 20, "end of a continued row")
        call leave(h)
    end subroutine test_wrapped

    !> C-n keeps the column across a short line (temporary-goal-column).
    subroutine test_goal_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: keys(2)
        call init_bindings()
        call discard_input()
        call enter("first line"//nl//"ab"//nl//"third line", 80, h)
        call goto_char(7_int64)
        keys(1) = make_fixnum(14_int64)
        keys(2) = make_fixnum(14_int64)
        call unread_command_events(keys)
        call command_loop(st)
        call check(error, point() == 21, "column 6 on the third line")
        if (allocated(error)) return
        ! C-p twice: back to column 6.
        keys(1) = make_fixnum(16_int64)
        keys(2) = make_fixnum(16_int64)
        call unread_command_events(keys)
        call command_loop(st)
        call check(error, point() == 7)
        call leave(h)
    end subroutine test_goal_column

    subroutine test_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("one"//nl//"two", 80, h)
        call line_move(-1_int64, st)
        call check(error, st%code == FM_BEGINNING_OF_BUFFER)
        if (allocated(error)) return
        call line_move(5_int64, st)
        call check(error, st%code == FM_END_OF_BUFFER)
        if (allocated(error)) return
        call line_move(5_int64, st, noerror=.true.)
        call check(error, st%ok())
        call leave(h)
    end subroutine test_errors

    !> With line-move-visual off (or a goal column), lines are buffer
    !> lines even when wrapped.
    subroutine test_logical(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("abcdefghijklmnopqrstuvwxy"//nl//"z1234"//nl//"q", 11, h)
        call goto_char(3_int64)
        line_move_visual = .false.
        body: block
            call line_move(1_int64, st)
            call check(error, st%ok() .and. point() == 29, "column 2 of the next buffer line")
            if (allocated(error)) exit body
            goal_column = 4
            call line_move(-1_int64, st)
            call check(error, point() == 5, "goal-column 4")
        end block body
        line_move_visual = .true.
        goal_column = -1
        call leave(h)
    end subroutine test_logical

    !> indent-to, current-indentation and move-to-column with FORCE.
    subroutine test_indent_to(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: col
        call enter("ab", 80, h)
        call goto_char(2_int64)
        indent_tabs_mode = .true.
        col = indent_to(19_int64, st)
        call check(error, col == 19 .and. buffer_string() == "a"//TAB//TAB//"   b", buffer_string())
        if (allocated(error)) return
        indent_tabs_mode = .false.
        call goto_char(point_max())
        col = indent_to(19_int64, st, 2_int64)
        call check(error, col == 22 .and. buffer_string() == "a"//TAB//TAB//"   b  ")
        indent_tabs_mode = .true.
        call leave(h)
        if (allocated(error)) return
        call enter("  "//TAB//"x", 80, h)
        call goto_char(point_max())
        call check(error, current_indentation() == 8)
        if (allocated(error)) return
        ! FORCE splits a tab and pads short lines.
        call move_to_column(4_int64, col, .true., st)
        call check(error, col == 4 .and. point() == 5 .and. buffer_string() == "    "//TAB//"x", buffer_string())
        if (allocated(error)) return
        call move_to_column(12_int64, col, .true., st)
        call check(error, col == 12 .and. buffer_string() == "    "//TAB//"x   ", buffer_string())
        call leave(h)
    end subroutine test_indent_to

    !> indent-line-to, indent-relative, tab stops and back-to-indentation.
    subroutine test_indent_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("    foo  bar"//LF//"x", 80, h)
        indent_tabs_mode = .false.
        call goto_char(point_max())
        call indent_line_to(6_int64, st)
        call check(error, buffer_string() == "    foo  bar"//LF//"      x" .and. point() == 20, buffer_string())
        if (allocated(error)) return
        call indent_line_to(2_int64, st)
        call check(error, buffer_string() == "    foo  bar"//LF//"  x")
        if (allocated(error)) return
        ! indent-relative: under the next indent point of the previous line.
        call erase_buffer(st)
        call insert("    foo  bar"//LF, st)
        call indent_relative(st)
        call check(error, current_column() == 4, "relative 1")
        if (allocated(error)) return
        call indent_relative(st)
        call check(error, current_column() == 9, "relative 2")
        if (allocated(error)) return
        ! No more indent points: tab-to-tab-stop.
        call indent_relative(st)
        call check(error, current_column() == 16, "relative tab stop")
        if (allocated(error)) return
        call check(error, indent_next_tab_stop(3_int64) == 8 .and. indent_next_tab_stop(8_int64) == 16 .and. &
                   indent_next_tab_stop(8_int64, .true.) == 0 .and. indent_next_tab_stop(9_int64, .true.) == 8, "tab stops")
        if (allocated(error)) return
        tab_stop_list = [int(4, int64), int(10, int64)]
        call check(error, indent_next_tab_stop(0_int64) == 4 .and. indent_next_tab_stop(4_int64) == 10 .and. &
                   indent_next_tab_stop(10_int64) == 16 .and. indent_next_tab_stop(10_int64, .true.) == 4 .and. &
                   indent_next_tab_stop(17_int64, .true.) == 16, "tab_stop_list")
        if (allocated(error)) return
        call check(error, all(indent_accumulate_tab_stops(22_int64) == [int(4, int64), 10_int64, 16_int64, 22_int64]), "accumulate")
        deallocate (tab_stop_list)
        if (allocated(error)) return
        call goto_char(1_int64)
        call goto_char(8_int64)
        call back_to_indentation()
        call check(error, point() == 5, "back_to_indentation")
        indent_tabs_mode = .true.
        call leave(h)
    end subroutine test_indent_commands

    !> indent-rigidly and indent-region (to a column, with a fill
    !> prefix, and line by line).
    subroutine test_indent_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("a"//LF//"  b"//LF//LF//"c"//LF, 80, h)
        indent_tabs_mode = .false.
        call indent_rigidly(1_int64, point_max(), 2_int64, st)
        call check(error, buffer_string() == "  a"//LF//"    b"//LF//LF//"  c"//LF, buffer_string())
        if (allocated(error)) return
        call indent_rigidly(1_int64, point_max(), -3_int64, st)
        call check(error, buffer_string() == "a"//LF//" b"//LF//LF//"c"//LF, buffer_string())
        if (allocated(error)) return
        call indent_region(1_int64, point_max(), st, 3_int64)
        call check(error, buffer_string() == "   a"//LF//"   b"//LF//LF//"   c"//LF, buffer_string())
        if (allocated(error)) return
        fill_prefix = "> "
        call indent_region(1_int64, point_max(), st)
        call check(error, buffer_string() == "> "//"   a"//LF//"> "//"   b"//LF//LF//"> "//"   c"//LF, buffer_string())
        deallocate (fill_prefix%value)
        if (allocated(error)) return
        ! Line by line with indent-according-to-mode: like the previous
        ! nonblank line.
        call leave(h)
        call enter("  a"//LF//"b"//LF//"    c"//LF//"d"//LF, 80, h)
        call indent_region(5_int64, point_max(), st)
        call check(error, buffer_string() == "  a"//LF//"  b"//LF//"  c"//LF//"  d"//LF, buffer_string())
        indent_tabs_mode = .true.
        call leave(h)
    end subroutine test_indent_region

    !> TAB, M-i, M-m, C-x TAB with the arrows and C-M-\ through the
    !> command loop.
    subroutine test_indent_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call init_bindings()
        call enter("    a"//LF//"b", 80, h)
        indent_tabs_mode = .false.
        call goto_char(7_int64)
        call key_parse("TAB", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == "    a"//LF//"    b", buffer_string())
        if (allocated(error)) return
        call key_parse("C-e M-i M-m", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == "    a"//LF//"    b   " .and. point() == 11, buffer_string()//itoa(point()))
        if (allocated(error)) return
        call key_parse("M-< C-SPC M-> C-x TAB <right> <right> <left>", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == "     a"//LF//"     b   ", buffer_string())
        if (allocated(error)) return
        call key_parse("C-g M-< C-SPC M-> C-u 2 C-M-\", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == "  a"//LF//"  b   ", buffer_string())
        indent_tabs_mode = .true.
        call discard_input()
        call leave(h)
    end subroutine test_indent_keys
    !> C-x TAB: the transient map's message names its keys and comes back
    !> after each of its commands, which keep the region and undo
    !> together; another key ends it, clearing the message and the mark.
    subroutine test_indent_rigidly_map(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call init_bindings()
        call enter("a"//LF//"b", 80, h)
        indent_tabs_mode = .false.
        call key_parse("C-SPC M-> C-x TAB", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, last_message, "Type TAB, <left>, <right>, S-<left>, S-<right> to indent region interactively")
        if (allocated(error)) return
        call key_parse("<right>", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call key_parse("<right>", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == "  a"//LF//"  b" .and. region_active_p() .and. &
                   last_message == "Type TAB, <left>, <right>, S-<left>, S-<right> to indent region interactively", &
                   buffer_string()//"|"//last_message)
        if (allocated(error)) return
        call key_parse("C-e", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error,.not. region_active_p() .and. last_message == "", "exit: "//last_message)
        if (allocated(error)) return
        ! The second <right> removed the undo boundary after the first:
        ! one undo takes back both.
        call key_parse("C-_", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string(), "a"//LF//"b")
        indent_tabs_mode = .true.
        call discard_input()
        call leave(h)
    end subroutine test_indent_rigidly_map

    ! An indent-line-function that cannot indent.
    subroutine cannot_indent(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        status = status_type()
        noindent = .true.
    end subroutine cannot_indent

    !> indent-for-tab-command: when the indent-line-function returns
    !> `noindent', the default one (indent-relative) indents.
    subroutine test_noindent_fallback(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("    a"//LF//"b", 80, h)
        indent_tabs_mode = .false.
        call goto_char(7_int64)
        call indent_line_function%set_local(cannot_indent)
        call indent_for_tab_command(nil(), st)
        call kill_local_variable(sym("indent_line_function"), st)
        call check(error, buffer_string(), "    a"//LF//"    b")
        indent_tabs_mode = .true.
        call leave(h)
    end subroutine test_noindent_fallback

    !> indent-region-line-by-line reports its progress.
    subroutine test_indent_region_progress(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("a"//LF//"b"//LF, 80, h)
        last_message = ""
        call indent_region_line_by_line(1_int64, point_max(), st)
        call check(error, last_message, "Indenting region...done")
        call leave(h)
    end subroutine test_indent_region_progress

    !> In a unibyte buffer the byte 0240 counts as a space.
    subroutine test_indentation_unibyte(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("", 80, h)
        buffers(cur)%multibyte = .false.
        call insert(" "//achar(160)//"x", st)
        call goto_char(1_int64)
        call check(error, current_indentation(), 2_int64)
        buffers(cur)%multibyte = .true.
        call leave(h)
    end subroutine test_indentation_unibyte
end module test_indent_fm
