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
                         exchange_point_and_mark, mark_whole_buffer, kill_ring_yank_pointer
    implicit none(type, external)
    private

    public :: collect_simple_fm

    character(len=*), parameter :: point_tag = "<POINT>", mark_tag = "<MARK>"
    character(len=1), parameter :: nl = achar(10)

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
                    new_unittest("mark-and-kill-keys", test_keys) &
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
        call set(sym("kill-ring"), nil(), st)
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
        last_command = sym("kill-region")
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
        call check(error, value_eq(this_command, sym("kill-region")), "this-command")
        if (allocated(error)) return
        ! A kill after a kill appends (forward) or prepends (backward).
        last_command = sym("kill-region")
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
        last_command = sym("kill-region")
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
end module test_simple_fm
