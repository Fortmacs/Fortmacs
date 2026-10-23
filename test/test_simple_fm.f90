! test_simple_fm.f90 -- tests for the mark, the region and the kill ring in
! simple_fm.
! Ported from GNU Emacs 31.1 test/lisp/simple-tests.el (the
! kill-whole-line tests), plus our own.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

module test_simple_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, FM_BEGINNING_OF_BUFFER, FM_END_OF_BUFFER, &
                        FM_BUFFER_READ_ONLY
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, list_elt, list_length, xstring, make_fixnum, list
    use lread_fm, only: sym
    use data_fm, only: set, value_eq
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, &
                         set_buffer_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_string, buffer_substring, &
                          delete_region, mark_position, region_beginning, region_end
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_command, this_command, &
                           last_message
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use simple_fm, only: kill_ring, kill_new, kill_append, current_kill, kill_region, &
                         copy_region_as_kill, kill_line, kill_whole_line, yank, yank_pop, push_mark, &
                         pop_to_mark_command, deactivate_mark, region_active_p, set_mark, &
                         exchange_point_and_mark, mark_whole_buffer, kill_ring_yank_pointer, &
                         delete_indentation, fill_prefix, transpose_chars, transpose_words, transpose_lines, &
                         delete_horizontal_space, just_one_space, fixup_whitespace, delete_blank_lines, &
                         cycle_spacing
    use cmds_fm, only: forward_line, end_of_line, self_insert_command, delete_char, auto_fill_chars
    use simple_fm, only: newline, open_line, turn_on_auto_fill, turn_off_auto_fill, inhibit_auto_fill, &
                         end_of_buffer, indicate_copied_region, transient_mark_mode_hook, special_mode, &
                         fundamental_mode
    use keymap_fm, only: key_parse, key_binding
    use buffer_fm, only: left_margin, fill_column, transient_mark_mode, auto_fill_function
    use undo_fm, only: this_command_amalgamating
    use callint_fm, only: mark_even_if_inactive, current_prefix_arg
    use keyboard_fm, only: unread_command_events
    use character_fm, only: CHAR_META
    use error_fm, only: FM_MARK_INACTIVE
    use handle_fm, only: null_handle
    use frame_fm, only: selected_frame, frame_selected_window_ref, ensure_frame_windows
    use window_fm, only: window_type, set_window_buffer, window_start, delete_window_tree, resize_window_tree, &
                         set_window_start
    use indent_fm, only: indent_tabs_mode
    use editfns_fm, only: narrow_to_region, widen, pos_bol, pos_eol
    implicit none(type, external)
    private

    public :: collect_simple_fm

    character(len=*), parameter :: point_tag = "<POINT>", mark_tag = "<MARK>"
    character(len=1), parameter :: nl = achar(10)
    integer :: fill_calls = 0

contains

    subroutine collect_simple_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("kill-whole-line-after-other-kill", test_kwl_after_other_kill), &
                    new_unittest("kill-whole-line-buffer-boundaries", test_kwl_buffer_boundaries), &
                    new_unittest("kill-whole-line-line-boundaries", test_kwl_line_boundaries), &
                    new_unittest("kill-ring-new-append-rotate", test_kill_ring), &
                    new_unittest("kill-region-appends", test_kill_region), &
                    new_unittest("kill-region-read-only", test_kill_read_only), &
                    new_unittest("kill-line", test_kill_line), &
                    new_unittest("yank-and-yank-pop", test_yank), &
                    new_unittest("mark-ring", test_mark_ring), &
                    new_unittest("exchange-point-and-mark", test_exchange), &
                    new_unittest("mark-and-kill-keys", test_keys), &
                    new_unittest("simple-delete-indentation-no-region", test_delete_indentation_no_region), &
                    new_unittest("simple-delete-indentation-inactive-region", test_delete_indentation_inactive_region), &
                    new_unittest("simple-delete-indentation-blank-line", test_delete_indentation_blank_line), &
                    new_unittest("simple-delete-indentation-boundaries", test_delete_indentation_boundaries), &
                    new_unittest("simple-delete-indentation-region", test_delete_indentation_region), &
                    new_unittest("simple-delete-indentation-prefix", test_delete_indentation_prefix), &
                    new_unittest("transpose-chars-words-lines", test_transpose), &
                    new_unittest("whitespace-commands", test_whitespace), &
                    new_unittest("transpose-and-whitespace-keys", test_space_keys), &
                    new_unittest("newline", test_newline), &
                    new_unittest("open-line", test_open_line), &
                    new_unittest("open-line-margin-and-prefix", test_open_line_margin_and_prefix), &
                    new_unittest("newline-arg-inhibits-auto-fill", test_newline_auto_fill), &
                    new_unittest("auto-fill-chars-and-amalgamation", test_auto_fill_chars), &
                    new_unittest("delete-char-killflag", test_delete_char_killflag), &
                    new_unittest("quoted-insert", test_quoted_insert), &
                    new_unittest("keypad-digit-argument", test_keypad_digit), &
                    new_unittest("transpose-with-inactive-mark", test_transpose_mark), &
                    new_unittest("yank-pop-read-only", test_yank_pop_read_only), &
                    new_unittest("end-of-buffer-recenters", test_end_of_buffer), &
                    new_unittest("kill-ring-save-indicates", test_indicate_copied_region), &
                    new_unittest("special-mode-suppresses-self-insert", test_special_mode_suppress) &
                    ]
    end subroutine collect_simple_fm

    !> A fresh current buffer for a test.
    subroutine fresh(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call get_buffer_create("*simple-test*", h, st)
        call set_buffer(h, st)
        call set_buffer_read_only(h, .false., st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call set(sym("kill_ring"), nil(), st)
        call show_alone()
    end subroutine fresh

    !> Set the current buffer's text, point and mark according to
    !> DESCRIPTION.  Port of simple-tests.el:simple-test--set-buffer-text-point-mark.
    subroutine set_text_point_mark(description)
        character(len=*), intent(in) :: description
        type(status_type) :: st
        integer :: k
        call erase_buffer(st)
        call insert(description, st)
        k = index(buffer_string(), mark_tag)
        if (k > 0) then
            call delete_region(int(k, int64), int(k + len(mark_tag), int64), st)
            call push_mark(int(k, int64), .true., .true.)
        end if
        k = index(buffer_string(), point_tag)
        call goto_char(point_min())
        if (k > 0) then
            call delete_region(int(k, int64), int(k + len(point_tag), int64), st)
            call goto_char(int(k, int64))
        end if
    end subroutine set_text_point_mark

    !> Inverse of `set_text_point_mark`.  Port of
    !> simple-tests.el:simple-test--get-buffer-text-point-mark.
    function get_text_point_mark() result(s)
        character(len=:), allocatable :: s
        character(len=:), allocatable :: a, b, c
        integer(int64) :: m, p
        type(status_type) :: st
        p = point()
        m = mark_position()
        if (.not. buffers(cur)%mark_active) then
            call buffer_substring(point_min(), p, a, st)
            call buffer_substring(p, point_max(), b, st)
            s = a//point_tag//b
        else if (m < p) then
            call buffer_substring(point_min(), m, a, st)
            call buffer_substring(m, p, b, st)
            call buffer_substring(p, point_max(), c, st)
            s = a//mark_tag//b//point_tag//c
        else
            call buffer_substring(point_min(), p, a, st)
            call buffer_substring(p, m, b, st)
            call buffer_substring(m, point_max(), c, st)
            s = a//point_tag//b//mark_tag//c
        end if
    end function get_text_point_mark

    !> The car of the kill ring.
    function latest_kill() result(s)
        character(len=:), allocatable :: s
        s = "<empty>"
        if (list_length(kill_ring()) > 0) s = xstring(list_elt(kill_ring(), 1_int64))
    end function latest_kill

    !> One subtest of the kill-whole-line tests: kill-whole-line ARG on
    !> BEFORE gives AFTER and kills KILLED.
    subroutine kwl_case(error, label, before, arg, after, killed)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: label, before, after, killed
        integer(int64), intent(in) :: arg
        type(status_type) :: st
        if (allocated(error)) return
        call set_text_point_mark(before)
        last_command = sym("ignore")
        call kill_whole_line(arg, st)
        call check(error, st%ok(), label//": "//format_error(st))
        if (allocated(error)) return
        call check(error, get_text_point_mark(), after)
        if (allocated(error)) return
        call check(error, latest_kill(), killed)
    end subroutine kwl_case

    !> kill-whole-line ARG on BEFORE signals the error `code`.
    subroutine kwl_error(error, label, before, arg, code)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: label, before
        integer(int64), intent(in) :: arg
        integer, intent(in) :: code
        type(status_type) :: st
        if (allocated(error)) return
        call set_text_point_mark(before)
        call kill_whole_line(arg, st)
        call check(error, st%code == code, label//": "//format_error(st))
    end subroutine kwl_error

    subroutine test_kwl_after_other_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call set_text_point_mark("A"//point_tag//"X"//mark_tag//"B")
        last_command = sym("ignore")
        call kill_region(point(), mark_position(), st)
        call deactivate_mark(.true.)
        last_command = sym("kill_region")
        call kill_whole_line(1_int64, st)
        call check(error, latest_kill(), "AXB")
        if (.not. allocated(error)) call check(error, get_text_point_mark(), point_tag)
        call kill_buffer(h, st)
    end subroutine test_kwl_after_other_kill

    subroutine test_kwl_buffer_boundaries(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call kwl_error(error, "0", point_tag, -1_int64, FM_BEGINNING_OF_BUFFER)
        call kwl_error(error, "0", point_tag, 1_int64, FM_END_OF_BUFFER)
        call kwl_error(error, "1a", "-1"//nl//point_tag, 1_int64, FM_END_OF_BUFFER)
        call kwl_case(error, "1b", "-1"//nl//"A"//point_tag, 1_int64, "-1"//nl//point_tag, "A")
        call kwl_error(error, "2", point_tag//nl//"1", -1_int64, FM_BEGINNING_OF_BUFFER)
        call kwl_case(error, "2b", point_tag//"A"//nl//"1", 1_int64, point_tag//"1", "A"//nl)
        call kill_buffer(h, st)
    end subroutine test_kwl_buffer_boundaries

    subroutine test_kwl_line_boundaries(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call kwl_case(error, "1a", "-1"//nl//point_tag//nl//"1"//nl, 1_int64, "-1"//nl//point_tag//"1"//nl, nl)
        call kwl_case(error, "1b", "-1"//nl//point_tag//nl//"1"//nl, -1_int64, "-1"//point_tag//nl//"1"//nl, nl)
        call kwl_case(error, "2a", "-1"//nl//"A"//point_tag//nl//"1"//nl, 1_int64, "-1"//nl//point_tag//"1"//nl, &
                      "A"//nl)
        call kwl_case(error, "2b", "-1"//nl//"A"//point_tag//nl//"1"//nl, -1_int64, "-1"//point_tag//nl//"1"//nl, &
                      nl//"A")
        call kwl_case(error, "3a", "-1"//nl//point_tag//"A"//nl//"1"//nl, 1_int64, "-1"//nl//point_tag//"1"//nl, &
                      "A"//nl)
        call kwl_case(error, "3b", "-1"//nl//point_tag//"A"//nl//"1"//nl, -1_int64, "-1"//point_tag//nl//"1"//nl, &
                      nl//"A")
        call kill_buffer(h, st)
    end subroutine test_kwl_line_boundaries

    subroutine test_kill_ring(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh(h)
        call current_kill(0_int64, text, st)
        call check(error, format_error(st), "Kill ring is empty")
        if (allocated(error)) return
        call kill_new("one")
        call kill_new("two")
        call kill_new("three")
        call check(error, list_length(kill_ring()) == 3 .and. latest_kill() == "three", "three kills")
        if (allocated(error)) return
        call kill_append("!", .false.)
        call check(error, latest_kill(), "three!")
        if (allocated(error)) return
        call kill_append("<", .true.)
        call check(error, latest_kill(), "<three!")
        if (allocated(error)) return
        call kill_new("3", .true.)
        call check(error, list_length(kill_ring()) == 3 .and. latest_kill() == "3", "replace")
        if (allocated(error)) return
        ! Rotating: 1 is the next older kill, wrapping around.
        call current_kill(1_int64, text, st)
        call check(error, text, "two")
        if (allocated(error)) return
        call current_kill(1_int64, text, st)
        call check(error, text, "one")
        if (allocated(error)) return
        call current_kill(1_int64, text, st)
        call check(error, text, "3")
        if (allocated(error)) return
        call current_kill(-1_int64, text, st, do_not_move=.true.)
        call check(error, text == "one" .and. kill_ring_yank_pointer == 1, "do-not-move")
        call kill_buffer(h, st)
    end subroutine test_kill_ring

    subroutine test_kill_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert("abcdef", st)
        last_command = sym("ignore")
        call kill_region(3_int64, 5_int64, st)
        call check(error, buffer_string() == "abef" .and. latest_kill() == "cd", "kill")
        if (allocated(error)) return
        call check(error, value_eq(this_command, sym("kill_region")), "this-command")
        if (allocated(error)) return
        ! A kill after a kill appends (forward) or prepends (backward).
        last_command = sym("kill_region")
        call kill_region(3_int64, 4_int64, st)
        call check(error, buffer_string() == "abf" .and. latest_kill() == "cde", "append")
        if (allocated(error)) return
        call kill_region(3_int64, 1_int64, st)
        call check(error, buffer_string() == "f" .and. latest_kill() == "abcde", "prepend")
        if (allocated(error)) return
        call check(error, list_length(kill_ring()), 1_int64)
        if (allocated(error)) return
        last_command = sym("ignore")
        call copy_region_as_kill(1_int64, 2_int64, st)
        call check(error, buffer_string() == "f" .and. latest_kill() == "f", "copy")
        if (allocated(error)) return
        call check(error, list_length(kill_ring()), 2_int64)
        call kill_buffer(h, st)
    end subroutine test_kill_region

    !> Killing in a read-only buffer copies the text and signals.
    subroutine test_kill_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert("locked", st)
        call set_buffer_read_only(h, .true., st)
        last_command = sym("ignore")
        call kill_region(1_int64, 4_int64, st)
        call check(error, st%code == FM_BUFFER_READ_ONLY, format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string() == "locked" .and. latest_kill() == "loc", "copied")
        call set_buffer_read_only(h, .false., st)
        call kill_buffer(h, st)
    end subroutine test_kill_read_only

    subroutine test_kill_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call set_text_point_mark(point_tag//"one"//nl//"  "//nl//"three")
        last_command = sym("ignore")
        call kill_line(0_int64, .false., st)
        call check(error, get_text_point_mark() == point_tag//nl//"  "//nl//"three" .and. latest_kill() == "one", &
                   "rest of the line")
        if (allocated(error)) return
        ! At the end of a line: the newline; then blanks and the newline.
        last_command = sym("kill_region")
        call kill_line(0_int64, .false., st)
        call kill_line(0_int64, .false., st)
        call check(error, get_text_point_mark() == point_tag//"three" .and. latest_kill() == "one"//nl//"  "//nl, &
                   get_text_point_mark())
        if (allocated(error)) return
        call kill_line(0_int64, .false., st)
        call kill_line(0_int64, .false., st)
        call check(error, st%code == FM_END_OF_BUFFER, "at the end")
        if (allocated(error)) return
        ! With an argument: that many lines; 0 kills back to the start.
        call set_text_point_mark("a"//nl//"b"//point_tag//"c"//nl//"d"//nl)
        last_command = sym("ignore")
        call kill_line(2_int64, .true., st)
        call check(error, get_text_point_mark() == "a"//nl//"b"//point_tag .and. latest_kill() == "c"//nl//"d"//nl, &
                   get_text_point_mark())
        if (allocated(error)) return
        call kill_line(0_int64, .true., st)
        call check(error, get_text_point_mark() == "a"//nl//point_tag .and. latest_kill() == "b", get_text_point_mark())
        call kill_buffer(h, st)
    end subroutine test_kill_line

    subroutine test_yank(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call kill_new("first")
        call kill_new("second")
        call insert("<>", st)
        call goto_char(2_int64)
        call yank(nil(), st)
        call check(error, buffer_string() == "<second>" .and. point() == 8 .and. mark_position() == 2, "yank")
        if (allocated(error)) return
        call check(error,.not. region_active_p(), "the mark is not activated")
        if (allocated(error)) return
        last_command = sym("yank")
        call yank_pop(1_int64, st)
        call check(error, buffer_string() == "<first>" .and. point() == 7, buffer_string())
        if (allocated(error)) return
        call yank_pop(1_int64, st)
        call check(error, buffer_string(), "<second>")
        if (allocated(error)) return
        last_command = sym("ignore")
        call yank_pop(1_int64, st)
        call check(error, format_error(st), "Previous command was not a yank")
        if (allocated(error)) return
        ! C-u C-y: point before the text, mark after it.
        call goto_char(1_int64)
        call yank(list(make_fixnum(4_int64)), st)
        call check(error, buffer_string() == "second<second>" .and. point() == 1 .and. mark_position() == 7, &
                   "C-u C-y")
        if (allocated(error)) return
        ! M-2 C-y: the second most recent kill.
        call yank(make_fixnum(2_int64), st)
        call check(error, buffer_string(), "firstsecond<second>")
        call kill_buffer(h, st)
    end subroutine test_yank

    subroutine test_mark_ring(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert("0123456789", st)
        call push_mark(2_int64, .true.)
        call push_mark(4_int64, .true.)
        call push_mark(6_int64, .true.)
        call goto_char(10_int64)
        call check(error, mark_position(), 6_int64)
        if (allocated(error)) return
        call pop_to_mark_command(st)
        call check(error, point() == 6 .and. mark_position() == 4, "first pop")
        if (allocated(error)) return
        call pop_to_mark_command(st)
        call check(error, point() == 4 .and. mark_position() == 2, "second pop")
        if (allocated(error)) return
        ! The ring is circular: the popped marks come round again.
        call pop_to_mark_command(st)
        call pop_to_mark_command(st)
        call check(error, point() == 6 .and. mark_position() == 4, "round again")
        if (allocated(error)) return
        call set_mark(0_int64)
        call check(error, mark_position() == 0 .and. .not. region_active_p(), "no mark")
        if (allocated(error)) return
        call pop_to_mark_command(st)
        call check(error, format_error(st), "No mark set in this buffer")
        call kill_buffer(h, st)
    end subroutine test_mark_ring

    subroutine test_exchange(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: b, e
        call fresh(h)
        call set_text_point_mark("ab"//point_tag//"cd"//mark_tag//"ef")
        call exchange_point_and_mark(.false., st)
        call check(error, get_text_point_mark(), "ab"//mark_tag//"cd"//point_tag//"ef")
        if (allocated(error)) return
        call region_beginning(b, st)
        call region_end(e, st)
        call check(error, b == 3 .and. e == 5, "region-beginning and region-end")
        if (allocated(error)) return
        ! A prefix argument leaves the mark inactive.
        call exchange_point_and_mark(.true., st)
        call check(error, get_text_point_mark(), "ab"//point_tag//"cdef")
        if (allocated(error)) return
        call mark_whole_buffer()
        call check(error, get_text_point_mark(), point_tag//"abcdef"//mark_tag)
        call kill_buffer(h, st)
    end subroutine test_exchange

    !> The bindings, through the command loop: C-SPC, motion, C-w, C-y,
    !> M-w, and deactivation by changes and C-g.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call discard_input()
        prefix_arg = nil()
        call init_bindings()
        ! hello world, C-a, C-SPC (NUL), C-f x5, C-w, C-e, C-y
        call kbd_store_bytes("hello world"//achar(1)//achar(0)//repeat(achar(6), 5)//achar(23)//achar(5)// &
                             achar(25))
        call command_loop(st)
        call check(error, buffer_string(), " worldhello")
        if (allocated(error)) return
        call check(error, latest_kill(), "hello")
        if (allocated(error)) return
        ! C-a C-SPC C-f M-w: copied, not killed; typing then deactivates.
        call kbd_store_bytes(achar(1)//achar(0)//achar(6)//achar(27)//"w")
        call command_loop(st)
        call check(error, buffer_string() == " worldhello" .and. latest_kill() == " ", "M-w")
        if (allocated(error)) return
        call kbd_store_bytes(achar(0)//achar(6))
        call command_loop(st)
        call check(error, region_active_p(), "C-SPC C-f activates")
        if (allocated(error)) return
        call kbd_store_bytes("x")
        call command_loop(st)
        call check(error,.not. region_active_p(), "inserting deactivates")
        if (allocated(error)) return
        ! C-SPC C-f C-g: quitting deactivates too.
        call kbd_store_bytes(achar(0)//achar(6)//achar(7))
        call command_loop(st)
        call check(error,.not. region_active_p(), "C-g deactivates")
        if (allocated(error)) return
        ! DEL with an active region deletes it.
        call kbd_store_bytes(achar(1)//achar(0)//achar(6)//achar(6)//achar(127))
        call command_loop(st)
        call check(error, buffer_string(), "xorldhello")
        if (allocated(error)) return
        ! C-SPC C-SPC: toggles the region.
        call kbd_store_bytes(achar(0)//achar(0))
        call command_loop(st)
        call check(error,.not. region_active_p() .and. last_message == "Mark deactivated", last_message)
        call kill_buffer(h, st)
    end subroutine test_keys

    !> Check (buffer-substring before point . after point): port of
    !> simple-tests.el:simple-test--buffer-substrings.
    subroutine check_substrings(error, before, after)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: before, after
        character(len=:), allocatable :: b, a
        type(status_type) :: st
        call buffer_substring(point_min(), point(), b, st)
        call buffer_substring(point(), point_max(), a, st)
        call check(error, b == before .and. a == after .and. len(b) == len(before) .and. len(a) == len(after), &
                   "got """//b//""" . """//a//"""")
    end subroutine check_substrings

    subroutine test_delete_indentation_no_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert(" first "//nl//" second "//nl//" third "//nl//" fourth ", st)
        ! Without prefix argument.
        call delete_indentation(.false., st)
        call check_substrings(error, " first "//nl//" second "//nl//" third", " fourth ")
        if (allocated(error)) return
        call delete_indentation(.false., st)
        call check_substrings(error, " first "//nl//" second", " third fourth ")
        if (allocated(error)) return
        ! With prefix argument.
        call goto_char(point_min())
        call delete_indentation(.true., st)
        call check_substrings(error, " first", " second third fourth ")
        call kill_buffer(h, st)
    end subroutine test_delete_indentation_no_region

    subroutine test_delete_indentation_inactive_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert(" first "//nl//" second "//nl//" third ", st)
        call set_mark(point_min())
        call deactivate_mark(.true.)
        call delete_indentation(.false., st)
        call check_substrings(error, " first "//nl//" second", " third ")
        call kill_buffer(h, st)
    end subroutine test_delete_indentation_inactive_region

    subroutine test_delete_indentation_blank_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert(nl//nl//" third "//nl//" "//nl//" "//nl//" sixth "//nl//nl, st)
        ! Without prefix argument.
        call delete_indentation(.false., st)
        call check_substrings(error, nl//nl//" third "//nl//" "//nl//" "//nl//" sixth "//nl, "")
        if (allocated(error)) return
        call delete_indentation(.false., st)
        call check_substrings(error, nl//nl//" third "//nl//" "//nl//" "//nl//" sixth", "")
        if (allocated(error)) return
        call delete_indentation(.false., st)
        call check_substrings(error, nl//nl//" third "//nl//" "//nl, "sixth")
        if (allocated(error)) return
        ! With prefix argument.
        call goto_char(point_min())
        call delete_indentation(.true., st)
        call check_substrings(error, "", nl//" third "//nl//" "//nl//"sixth")
        if (allocated(error)) return
        call delete_indentation(.true., st)
        call check_substrings(error, "", "third "//nl//" "//nl//"sixth")
        if (allocated(error)) return
        call delete_indentation(.true., st)
        call check_substrings(error, "third", nl//"sixth")
        if (allocated(error)) return
        call delete_indentation(.true., st)
        call check_substrings(error, "third", " sixth")
        call kill_buffer(h, st)
    end subroutine test_delete_indentation_blank_line

    subroutine test_delete_indentation_boundaries(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: shortage
        call fresh(h)
        call insert(" first "//nl//" second "//nl//" third ", st)
        ! Stay at EOB.
        call delete_indentation(.true., st)
        call check_substrings(error, " first "//nl//" second "//nl//" third ", "")
        if (allocated(error)) return
        ! Stay at BOB.
        shortage = forward_line(-1_int64)
        call narrow_to_region(point(), pos_eol(), st)
        call delete_indentation(.false., st)
        call check_substrings(error, "", " second ")
        if (allocated(error)) return
        ! Go to EOB.
        call delete_indentation(.true., st)
        call check_substrings(error, " second ", "")
        if (allocated(error)) return
        call widen()
        ! Go to BOB.
        call end_of_line(0_int64)
        call delete_indentation(.false., st)
        call check_substrings(error, "", " first "//nl//" second "//nl//" third ")
        call kill_buffer(h, st)
    end subroutine test_delete_indentation_boundaries

    subroutine test_delete_indentation_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        ! Empty region.
        call insert(" first ", st)
        call delete_indentation(.false., st, point(), point())
        call check_substrings(error, " first ", "")
        if (allocated(error)) return
        ! Single line.
        call delete_indentation(.false., st, pos_bol(), point() - 1)
        call check_substrings(error, "", " first ")
        if (allocated(error)) return
        call delete_indentation(.false., st, point() + 1, pos_eol())
        call check_substrings(error, " ", "first ")
        if (allocated(error)) return
        call delete_indentation(.false., st, pos_bol(), pos_eol())
        call check_substrings(error, "", " first ")
        if (allocated(error)) return
        ! Multiple lines.
        call goto_char(point_max())
        call insert(nl//" second "//nl//" third "//nl//" fourth ", st)
        call goto_char(point_min())
        call delete_indentation(.false., st, pos_eol(), pos_bol(2_int64))
        call check_substrings(error, " first", " second "//nl//" third "//nl//" fourth ")
        if (allocated(error)) return
        call delete_indentation(.false., st, point(), 1 + pos_bol(2_int64))
        call check_substrings(error, " first second", " third "//nl//" fourth ")
        if (allocated(error)) return
        ! Prefix argument overrides region.
        call delete_indentation(.true., st, point_min(), point())
        call check_substrings(error, " first second third", " fourth ")
        call kill_buffer(h, st)
    end subroutine test_delete_indentation_region

    subroutine test_delete_indentation_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert("> first "//nl//"> second "//nl//"> third "//nl//"> fourth ", st)
        fill_prefix = ""
        call delete_indentation(.false., st)
        call check_substrings(error, "> first "//nl//"> second "//nl//"> third", " > fourth ")
        if (allocated(error)) return
        fill_prefix = "<"
        call delete_indentation(.false., st)
        call check_substrings(error, "> first "//nl//"> second", " > third > fourth ")
        if (allocated(error)) return
        fill_prefix = ">"
        call delete_indentation(.false., st)
        call check_substrings(error, "> first", " second > third > fourth ")
        deallocate (fill_prefix%value)
        call kill_buffer(h, st)
    end subroutine test_delete_indentation_prefix

    !> Fortmacs: transpose-chars, -words and -lines, with positive,
    !> negative and zero arguments.
    subroutine test_transpose(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call set_text_point_mark("ab"//point_tag//"cd")
        call transpose_chars(1_int64, st)
        call check(error, get_text_point_mark(), "acb"//point_tag//"d")
        if (allocated(error)) return
        ! At end of line, the previous two chars are exchanged.
        call set_text_point_mark("abcd"//point_tag)
        call transpose_chars(1_int64, st)
        call check(error, get_text_point_mark(), "abdc"//point_tag)
        if (allocated(error)) return
        call set_text_point_mark("ab"//point_tag//"cde")
        call transpose_chars(2_int64, st)
        call check(error, get_text_point_mark(), "acdb"//point_tag//"e")
        if (allocated(error)) return
        call set_text_point_mark("abc"//point_tag//"de")
        call transpose_chars(-1_int64, st)
        call check(error, get_text_point_mark(), "ac"//point_tag//"bde")
        if (allocated(error)) return
        call set_text_point_mark("foo bar"//point_tag//" baz")
        call transpose_words(1_int64, st)
        call check(error, get_text_point_mark(), "foo baz bar"//point_tag)
        if (allocated(error)) return
        call set_text_point_mark("foo "//point_tag//"bar baz")
        call transpose_words(1_int64, st)
        call check(error, get_text_point_mark(), "bar foo"//point_tag//" baz")
        if (allocated(error)) return
        call set_text_point_mark("one"//mark_tag//" two three"//point_tag)
        call transpose_words(0_int64, st)
        call check(error, buffer_string() == "one three two", buffer_string())
        if (allocated(error)) return
        call kill_buffer(h, st)
        call fresh(h)
        call set_text_point_mark("one"//nl//"tw"//point_tag//"o"//nl//"three"//nl)
        call transpose_lines(1_int64, st)
        call check(error, get_text_point_mark(), "two"//nl//"one"//nl//point_tag//"three"//nl)
        if (allocated(error)) return
        ! Lines are created at the end if necessary.
        call set_text_point_mark("one"//nl//point_tag//"two")
        call transpose_lines(1_int64, st)
        call check(error, get_text_point_mark(), "two"//nl//"one"//nl//point_tag)
        if (allocated(error)) return
        call transpose_chars(1_int64, st)
        call kill_buffer(h, st)
        call fresh(h)
        call insert("a", st)
        call goto_char(point_min())
        ! Nothing before point to drag.
        call transpose_chars(1_int64, st)
        call check(error,.not. st%ok() .and. st%code == FM_BEGINNING_OF_BUFFER, format_error(st))
        call kill_buffer(h, st)
    end subroutine test_transpose

    !> Fortmacs: delete-horizontal-space, just-one-space, fixup-whitespace,
    !> delete-blank-lines and cycle-spacing.
    subroutine test_whitespace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call set_text_point_mark("a "//achar(9)//" "//point_tag//"  b")
        call delete_horizontal_space(.false., st)
        call check(error, get_text_point_mark(), "a"//point_tag//"b")
        if (allocated(error)) return
        call set_text_point_mark("a  "//point_tag//"  b")
        call delete_horizontal_space(.true., st)
        call check(error, get_text_point_mark(), "a"//point_tag//"  b")
        if (allocated(error)) return
        call set_text_point_mark("a  "//point_tag//"  b")
        call just_one_space(1_int64, st)
        call check(error, get_text_point_mark(), "a "//point_tag//"b")
        if (allocated(error)) return
        call set_text_point_mark("a "//nl//point_tag//"  b")
        call just_one_space(-2_int64, st)
        call check(error, get_text_point_mark(), "a  "//point_tag//"b")
        if (allocated(error)) return
        call set_text_point_mark("foo  "//point_tag//"  bar")
        call fixup_whitespace(st)
        call check(error, get_text_point_mark(), "foo"//point_tag//" bar")
        if (allocated(error)) return
        call set_text_point_mark("(  "//point_tag//"foo")
        call fixup_whitespace(st)
        call check(error, get_text_point_mark(), "("//point_tag//"foo")
        if (allocated(error)) return
        ! delete-blank-lines: on a blank line, keep one; on an isolated
        ! one, delete it; on a nonblank line, delete the following ones.
        call set_text_point_mark("a"//nl//nl//point_tag//nl//nl//"b")
        call delete_blank_lines(st)
        call check(error, get_text_point_mark(), "a"//nl//point_tag//nl//"b")
        if (allocated(error)) return
        call delete_blank_lines(st)
        call check(error, get_text_point_mark(), "a"//nl//point_tag//"b")
        if (allocated(error)) return
        call set_text_point_mark("a"//point_tag//nl//nl//" "//nl//"b")
        call delete_blank_lines(st)
        call check(error, get_text_point_mark(), "a"//point_tag//nl//"b")
        if (allocated(error)) return
        ! cycle-spacing: one space, no space, the original spacing.
        call set_text_point_mark("a  "//point_tag//" b")
        this_command = sym("cycle_spacing")
        last_command = nil()
        call cycle_spacing(nil(), st)
        call check(error, get_text_point_mark(), "a "//point_tag//"b")
        if (allocated(error)) return
        last_command = this_command
        call cycle_spacing(nil(), st)
        call check(error, get_text_point_mark(), "a"//point_tag//"b")
        if (allocated(error)) return
        call cycle_spacing(nil(), st)
        call check(error, get_text_point_mark(), "a  "//point_tag//" b")
        if (allocated(error)) return
        call cycle_spacing(nil(), st)
        call check(error, get_text_point_mark(), "a "//point_tag//"b")
        last_command = nil()
        this_command = nil()
        call kill_buffer(h, st)
    end subroutine test_whitespace

    !> Fortmacs: the bindings (C-t, M-t, C-x C-t, M-SPC, M-\, C-x C-o,
    !> M-^, M-u, M-l, M-c).
    subroutine test_space_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call fresh(h)
        call insert("ab cd", st)
        call goto_char(2_int64)
        call kbd_store_bytes(achar(20))
        call command_loop(st)
        call check(error, buffer_string() == "ba cd", buffer_string())
        if (allocated(error)) return
        call goto_char(point_min())
        call kbd_store_bytes(achar(27)//"u"//achar(27)//"c"//achar(1)//achar(27)//"f"//achar(27)//"t")
        call command_loop(st)
        call check(error, buffer_string() == "Cd BA", buffer_string())
        if (allocated(error)) return
        call goto_char(point_min())
        call kbd_store_bytes(achar(27)//"l"//achar(5)//achar(27)//"^")
        call command_loop(st)
        call check(error, buffer_string() == "cd BA", buffer_string())
        if (allocated(error)) return
        call goto_char(3_int64)
        call kbd_store_bytes(achar(27)//"\")
        call command_loop(st)
        call check(error, buffer_string() == "cdBA", buffer_string())
        if (allocated(error)) return
        call kbd_store_bytes(achar(27)//" ")
        call command_loop(st)
        call check(error, buffer_string() == "cd BA", buffer_string())
        if (allocated(error)) return
        call goto_char(point_max())
        call insert(nl//nl//nl//"x", st)
        call goto_char(point_min())
        call kbd_store_bytes(achar(24)//achar(15))
        call command_loop(st)
        call check(error, buffer_string() == "cd BA"//nl//"x", buffer_string())
        if (allocated(error)) return
        call goto_char(point_max())
        call kbd_store_bytes(achar(24)//achar(20))
        call command_loop(st)
        call check(error, buffer_string() == "x"//nl//"cd BA"//nl, buffer_string())
        call kill_buffer(h, st)
    end subroutine test_space_keys
    !> A buffer holding "(a b" before point and " c d)" after, as
    !> simple-tests.el:simple-test--dummy-buffer (indent-tabs-mode nil
    !> does not matter here).
    subroutine dummy_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call fresh(h)
        indent_tabs_mode = .false.
        call insert("(a b c d)", st)
        call goto_char(5_int64)
    end subroutine dummy_buffer

    !> Port of simple-tests.el:newline (no electric indentation).
    subroutine test_newline(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call dummy_buffer(h)
        call newline(st, -1_int64)
        call check(error,.not. st%ok(), "(newline -1) signals")
        if (allocated(error)) return
        call newline(st, 1_int64)
        call check_substrings(error, "(a b"//nl, " c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call newline(st)
        call check_substrings(error, "(a b"//nl, " c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call newline(st, 5_int64)
        call check_substrings(error, "(a b"//repeat(nl, 5), " c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call goto_char(point() + 1)
        call newline(st, 1_int64)
        call check_substrings(error, "(a b "//nl, "c d)")
        indent_tabs_mode = .true.
        call kill_buffer(h, st)
    end subroutine test_newline

    !> Port of simple-tests.el:open-line.
    subroutine test_open_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call dummy_buffer(h)
        call open_line(-1_int64, st)
        call check(error,.not. st%ok(), "(open-line -1) signals")
        if (allocated(error)) return
        call open_line(1_int64, st)
        call check_substrings(error, "(a b", nl//" c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call open_line(5_int64, st)
        call check_substrings(error, "(a b", repeat(nl, 5)//" c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call goto_char(point() + 1)
        call open_line(1_int64, st)
        call check_substrings(error, "(a b ", nl//"c d)")
        indent_tabs_mode = .true.
        call kill_buffer(h, st)
    end subroutine test_open_line

    !> Port of simple-tests.el:open-line-margin-and-prefix.
    subroutine test_open_line_margin_and_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call dummy_buffer(h)
        left_margin = 10_int64
        call open_line(3_int64, st)
        left_margin = 0_int64
        call check_substrings(error, "(a b", repeat(nl, 3)//"          c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call goto_char(point_min())
        left_margin = 2_int64
        call open_line(1_int64, st)
        left_margin = 0_int64
        call check_substrings(error, "  ", nl//"  (a b c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        fill_prefix = "- - "
        call open_line(1_int64, st)
        call check_substrings(error, "(a b", nl//" c d)")
        if (allocated(error)) return
        call dummy_buffer(h)
        call goto_char(point_min())
        call open_line(1_int64, st)
        deallocate (fill_prefix%value)
        call check_substrings(error, "- - ", nl//"(a b c d)")
        indent_tabs_mode = .true.
        call kill_buffer(h, st)
    end subroutine test_open_line_margin_and_prefix

    !> `newline' with an argument does not auto-fill; without one it does.
    subroutine test_newline_auto_fill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call init_bindings()
        call turn_on_auto_fill(st)
        fill_column = 5_int64
        call insert("aaa bbb ccc", st)
        call newline(st, 1_int64)
        call check(error, buffer_string(), "aaa bbb ccc"//nl)
        if (allocated(error)) return
        call check(error,.not. inhibit_auto_fill, "inhibit_auto_fill restored")
        if (allocated(error)) return
        call erase_buffer(st)
        call insert("aaa bbb ccc", st)
        call newline(st)
        call check(error, buffer_string(), "aaa"//nl//"bbb"//nl//"ccc"//nl)
        call turn_off_auto_fill(st)
        fill_column = 70_int64
        call kill_buffer(h, st)
    end subroutine test_newline_auto_fill

    ! An auto-fill function that only counts its calls.
    subroutine count_fill(status)
        type(status_type), intent(out) :: status
        status = status_type()
        fill_calls = fill_calls + 1
    end subroutine count_fill

    !> Characters in `auto_fill_chars' (CJK too) invoke auto-filling, and
    !> a self-insertion that filled is not amalgamated for undo.
    subroutine test_auto_fill_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call init_bindings()
        call check(error, allocated(auto_fill_chars), "auto_fill_chars set")
        if (allocated(error)) return
        auto_fill_function%value => count_fill
        fill_calls = 0
        call self_insert_command(1_int64, int(z'4E00', int64), st)
        call self_insert_command(1_int64, 97_int64, st)
        call self_insert_command(1_int64, 32_int64, st)
        call check(error, fill_calls, 2)
        if (allocated(error)) return
        ! The real auto-fill: breaking the line resets the amalgamation.
        call turn_on_auto_fill(st)
        fill_column = 5_int64
        call erase_buffer(st)
        call insert("aaa", st)
        call self_insert_command(1_int64, 32_int64, st)
        call check(error, this_command_amalgamating, "no fill: amalgamating")
        if (allocated(error)) return
        call insert("bbbbbb", st)
        call self_insert_command(1_int64, 32_int64, st)
        call check(error, buffer_string(), "aaa"//nl//"bbbbbb ")
        if (allocated(error)) return
        call check(error,.not. this_command_amalgamating, "filled: not amalgamating")
        call turn_off_auto_fill(st)
        fill_column = 70_int64
        call kill_buffer(h, st)
    end subroutine test_auto_fill_chars

    !> C-d with a prefix argument kills (KILLFLAG); DEL too.
    subroutine test_delete_char_killflag(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert("hello world", st)
        call goto_char(1_int64)
        last_command = nil()
        call delete_char(5_int64, st, .true.)
        call check(error, buffer_string() == " world" .and. latest_kill() == "hello", "killed")
        if (allocated(error)) return
        call delete_char(1_int64, st)
        call check(error, buffer_string() == "world" .and. latest_kill() == "hello", "deleted")
        if (allocated(error)) return
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        ! C-u 2 C-d
        call kbd_store_bytes(achar(21)//"2"//achar(4))
        call command_loop(st)
        call check(error, buffer_string() == "rld" .and. latest_kill() == "wo", "C-u 2 C-d")
        current_prefix_arg = nil()
        call kill_buffer(h, st)
    end subroutine test_delete_char_killflag

    !> C-q: octal codes, meta characters, non-character events, and the
    !> read-only check before reading.  Port of the behaviour of
    !> simple.el:read-quoted-char.
    subroutine test_quoted_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        ! C-q 1 0 1 RET: octal 101 is A, RET is discarded.
        call kbd_store_bytes(achar(17)//"101"//achar(13))
        call command_loop(st)
        call check(error, buffer_string(), "A")
        if (allocated(error)) return
        ! C-q 1 2 x: octal 12 is newline; x is then typed.
        call kbd_store_bytes(achar(17)//"12x")
        call command_loop(st)
        call check(error, buffer_string(), "A"//nl//"x")
        if (allocated(error)) return
        ! C-q M-a inserts a with the 0200 bit: 225.
        call erase_buffer(st)
        call unread_command_events([make_fixnum(17_int64), make_fixnum(ior(97_int64, int(CHAR_META, int64)))])
        call command_loop(st)
        call check(error, buffer_string(), achar(195)//achar(161))
        if (allocated(error)) return
        ! C-u 3 C-q C-a
        call erase_buffer(st)
        call kbd_store_bytes(achar(21)//"3"//achar(17)//achar(1))
        call command_loop(st)
        call check(error, buffer_string(), repeat(achar(1), 3))
        if (allocated(error)) return
        ! A read-only buffer: an error before reading, so `b' is inserted
        ! by the next command after the buffer is made writable.
        call erase_buffer(st)
        call set_buffer_read_only(h, .true., st)
        call kbd_store_bytes(achar(17))
        call command_loop(st)
        call check(error, index(last_message, "Buffer is read-only") == 1, last_message)
        if (allocated(error)) return
        call set_buffer_read_only(h, .false., st)
        call kbd_store_bytes("b")
        call command_loop(st)
        call check(error, buffer_string(), "b")
        current_prefix_arg = nil()
        call kill_buffer(h, st)
    end subroutine test_quoted_insert

    !> Keypad digits after C-u use their `ascii_character'.
    subroutine test_keypad_digit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        call unread_command_events([make_fixnum(21_int64), sym("kp-1"), sym("kp-2"), make_fixnum(97_int64)])
        call command_loop(st)
        call check(error, buffer_string(), repeat("a", 12))
        current_prefix_arg = nil()
        call kill_buffer(h, st)
    end subroutine test_keypad_digit

    !> transpose with ARG 0 uses (mark), which signals when the mark is
    !> inactive in Transient Mark mode without mark-even-if-inactive.
    subroutine test_transpose_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call insert("ab cd", st)
        call set_mark(1_int64)
        call deactivate_mark()
        call goto_char(4_int64)
        mark_even_if_inactive = .false.
        call transpose_words(0_int64, st)
        mark_even_if_inactive = .true.
        call check(error, st%code == FM_MARK_INACTIVE .and. buffer_string() == "ab cd", "mark inactive")
        if (allocated(error)) return
        call transpose_words(0_int64, st)
        call check(error, buffer_string(), "cd ab")
        call kill_buffer(h, st)
    end subroutine test_transpose_mark

    !> yank-pop's inhibit-read-only covers the insertion too.
    subroutine test_yank_pop_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call kill_new("one")
        call kill_new("two")
        call yank(nil(), st)
        last_command = sym("yank")
        call set_buffer_read_only(h, .true., st)
        call yank_pop(1_int64, st)
        call set_buffer_read_only(h, .false., st)
        call check(error, st%ok() .and. buffer_string() == "one", buffer_string())
        call kill_buffer(h, st)
    end subroutine test_yank_pop_read_only

    !> A 30-line buffer shown in an 11-line window (10 body lines) from
    !> its start, point at 1.
    subroutine enter_window(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        type(window_type), pointer :: w
        character(len=7) :: text
        integer :: k
        call fresh(h)
        do k = 1, 30
            write (text, '("line", i2.2, a)') k, achar(10)
            call insert(text, st)
        end do
        call goto_char(1_int64)
        selected_frame%minibuffer_selected = .false.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 11, 80, 80)
        w => frame_selected_window_ref()
        call set_window_buffer(w, h, st)
        this_command = nil()
        last_command = nil()
    end subroutine enter_window

    subroutine leave_window(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        call kill_buffer(h, st)
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        selected_frame%garbaged = .true.
    end subroutine leave_window

    !> end-of-buffer puts the end on the third line from the bottom when
    !> it was not visible; not when it was.
    subroutine test_end_of_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        call enter_window(h)
        call end_of_buffer(nil())
        ! Point (the empty last line, 31) on body line 8 of 10.
        call check(error, point() == point_max() .and. window_start(frame_selected_window_ref()) == 7*24 - 6, &
                   "recentered")
        if (allocated(error)) return
        call end_of_buffer(nil())
        call check(error, window_start(frame_selected_window_ref()) == 7*24 - 6, "visible: unchanged")
        call leave_window(h)
    end subroutine test_end_of_buffer

    !> kill-ring-save: a message with a sample when the mark is not
    !> visible; when it is, the cursor blinks there and comes back.
    subroutine test_indicate_copied_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter_window(h)
        call set_window_start(frame_selected_window_ref(), 7_int64*20 + 1, st)
        call set_mark(1_int64)
        call goto_char(7_int64*25 + 1)
        last_message = ""
        call indicate_copied_region()
        call check(error, last_message, 'Copied text from "line01'//"^J"//"line02"//"^J"//"line03"//"^J"//"line04"// &
                   "^J"//"line05"//"^J"//'line0"')
        if (allocated(error)) return
        call set_window_start(frame_selected_window_ref(), 1_int64, st)
        call goto_char(1_int64)
        call set_mark(7_int64*25 + 1)
        last_message = ""
        call indicate_copied_region(10_int64)
        call check(error, last_message, 'Copied text until "24'//"^J"//"line25"//"^J"//'"')
        if (allocated(error)) return
        ! Visible: no message; point and mark are back.
        call set_mark(8_int64)
        call deactivate_mark()
        last_message = ""
        call indicate_copied_region()
        call check(error, last_message == "" .and. point() == 1 .and. mark_position() == 8, "blink")
        call leave_window(h)
    end subroutine test_indicate_copied_region

    !> special-mode-map is a suppressed keymap: self-insert-command is
    !> remapped to `undefined', so letters are undefined keys there.
    subroutine test_special_mode_suppress(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: binding
        type(value_type), allocatable :: key(:)
        call fresh(h)
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        call special_mode(st)
        call key_parse("x", key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, sym("undefined")), "key-binding remaps")
        if (allocated(error)) return
        call key_binding(key, binding, st, no_remap=.true.)
        call check(error, value_eq(binding, sym("self_insert_command")), "no remap")
        if (allocated(error)) return
        call kbd_store_bytes("x")
        call command_loop(st)
        call check(error, buffer_string() == "" .and. last_message == "x is undefined", last_message)
        call fundamental_mode(st)
        call kill_buffer(h, st)
    end subroutine test_special_mode_suppress
end module test_simple_fm
