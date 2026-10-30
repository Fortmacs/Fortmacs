! test_ibuffer_fm.f90 -- tests for ibuffer_fm.
! Ported from GNU Emacs 31.1 test/lisp/ibuffer-tests.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Ports ibuffer-test-Bug25000 of ibuffer-tests.el (the filter tests come
!> with the filters, step 36c; ibuffer-0autoload is not applicable: Ibuffer
!> is built in); own tests of the list, the formats, sorting, marks and
!> the kill operations.
module test_ibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, current_buffer, set_buffer, kill_buffer, get_buffer_create, get_buffer, &
                         buffer_live_p, generate_new_buffer_name
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point_min, point_max, pos_bol, pos_eol, buffer_substring, buffer_string
    use cmds_fm, only: beginning_of_line
    use search_fm, only: search_forward
    use bindings_fm, only: init_bindings
    use ibuffer_fm, only: ibuffer, ibuffer_update, ibuffer_unmark_all_marks, ibuffer_change_marks, &
                          ibuffer_buffer_names_with_mark, ibuffer_marked_char, ibuffer_save_marks_begin, &
                          ibuffer_save_marks_end, ibuffer_mark_forward, ibuffer_mark_for_delete, ibuffer_current_buffer, &
                          ibuffer_current_mark, ibuffer_do_kill_on_deletion_marks, ibuffer_expert, ibuffer_switch_format, &
                          ibuffer_toggle_marks, ibuffer_get_marked_buffers, ibuffer_do_sort_by, ibuffer_formats, &
                          ibuffer_column, ibuffer_text
    use ibuf_macs_fm, only: ibuffer_formats_type, IBUFFER_ALIGN_RIGHT
    implicit none(type, external)
    private

    public :: collect_ibuffer_fm

    character(len=*), parameter :: LF = achar(10)

contains

    subroutine collect_ibuffer_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ibuffer-test-Bug25000", test_bug25000), &
                    new_unittest("ibuffer-listing", test_listing), &
                    new_unittest("ibuffer-marks", test_marks), &
                    new_unittest("ibuffer-kill-on-deletion-marks", test_kill), &
                    new_unittest("ibuffer-sort-alphabetic", test_sort), &
                    new_unittest("ibuffer-formats", test_formats) &
                    ]
    end subroutine collect_ibuffer_fm

    ! A new buffer named after NAME holding TEXT.
    function make_buffer(name, text) result(h)
        character(len=*), intent(in) :: name, text
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(name), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call set_buffer(old, st)
    end function make_buffer

    ! Run `ibuffer' and make *Ibuffer* current; OLD is the buffer before.
    subroutine start(old, ib, error)
        type(handle_type), intent(out) :: old, ib
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        old = current_buffer()
        call ibuffer(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ib = get_buffer("*Ibuffer*")
        call set_buffer(ib, st)
    end subroutine start

    subroutine finish(old, ib)
        type(handle_type), intent(in) :: old, ib
        type(status_type) :: st

        if (buffer_live_p(ib)) call kill_buffer(ib, st)
        call set_buffer(old, st)
    end subroutine finish

    ! Move to the line of buffer NAME; FOUND false if it is not listed.
    subroutine goto_buffer_line(name, found)
        character(len=*), intent(in) :: name
        logical, intent(out) :: found
        integer(int64) :: pos
        type(status_type) :: st

        call goto_char(point_min())
        call search_forward(" "//name, pos, st, noerror=1)
        found = st%ok() .and. pos /= 0
        if (found) call beginning_of_line()
    end subroutine goto_buffer_line

    function current_line() result(line)
        character(len=:), allocatable :: line
        type(status_type) :: st

        call buffer_substring(pos_bol(), pos_eol(), line, st)
    end function current_line

    !> Test for https://debbugs.gnu.org/25000 .
    subroutine test_bug25000(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, buf1, buf2
        type(status_type) :: st
        character(len=256), allocatable :: names(:)
        integer :: n

        call init_bindings()
        buf1 = make_buffer("ibuffer-test-Bug25000-buf1", "")
        buf2 = make_buffer("ibuffer-test-Bug25000-buf2", "")
        call start(old, ib, error)
        if (allocated(error)) return
        call ibuffer_save_marks_begin(st)
        call ibuffer_unmark_all_marks(st)
        call mark_by_name(buffers(buf1%index)%name, st)
        call ibuffer_change_marks(ibuffer_marked_char, "L", st)
        call mark_by_name(buffers(buf2%index)%name, st)
        call ibuffer_change_marks(ibuffer_marked_char, "l", st)
        call ibuffer_buffer_names_with_mark("l", names, n, st)
        call check(error, n <= 1, "more than one buffer marked l")
        call ibuffer_save_marks_end(st)
        call kill_buffer(buf1, st)
        call kill_buffer(buf2, st)
        call finish(old, ib)
    end subroutine test_bug25000

    ! (ibuffer-mark-by-name-regexp NAME) for a literal NAME.
    subroutine mark_by_name(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        logical :: found

        call goto_buffer_line(name, found)
        if (found) call ibuffer_mark_forward(status)
    end subroutine mark_by_name

    !> The default format: title, the group, the columns and the summary.
    subroutine test_listing(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, a
        type(status_type) :: st
        character(len=:), allocatable :: text, line
        logical :: found

        call init_bindings()
        a = make_buffer("ibuf-alpha", "12345")
        call start(old, ib, error)
        if (allocated(error)) return
        text = buffer_string()
        call check(error, text(1:len(" MRL Name")) == " MRL Name", text(1:min(40, len(text))))
        if (.not. allocated(error)) call check(error, index(text, LF//"[ Default ]"//LF) > 0, "no Default group")
        if (.not. allocated(error)) call check(error, index(text, " buffers") > 0, "no summary")

        if (.not. allocated(error)) then
            call goto_buffer_line("ibuf-alpha", found)
            call check(error, found, "ibuf-alpha not listed")
        end if

        if (.not. allocated(error)) then
            line = current_line()
            call check(error, index(line, "        5 Fundamental") > 0, line)
        end if

        if (.not. allocated(error)) then
            call goto_buffer_line("*Ibuffer*", found)
            call check(error,.not. found, "*Ibuffer* lists itself")
        end if

        call kill_buffer(a, st)
        call finish(old, ib)
    end subroutine test_listing

    !> m marks, t toggles.
    subroutine test_marks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, a, b
        type(handle_type), allocatable :: marked(:)
        type(status_type) :: st
        logical :: found

        call init_bindings()
        a = make_buffer("ibuf-beta", "")
        call start(old, ib, error)
        if (allocated(error)) return
        call goto_buffer_line("ibuf-beta", found)
        call ibuffer_mark_forward(st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call goto_buffer_line("ibuf-beta", found)
            call check(error, ibuffer_current_mark() == ">", "not marked")
        end if

        if (.not. allocated(error)) then
            call ibuffer_get_marked_buffers(marked, st)
            call check(error, size(marked) == 1, "marked buffers")
            if (.not. allocated(error)) call check(error, marked(1)%index == a%index, "marked buffer")
        end if

        if (.not. allocated(error)) then
            call ibuffer_toggle_marks(st)
            call goto_buffer_line("ibuf-beta", found)
            call check(error, ibuffer_current_mark() == " ", "still marked after t")
        end if

        if (.not. allocated(error)) then
            call goto_char(point_min())
            b = ibuffer_current_buffer(st)
            call ibuffer_get_marked_buffers(marked, st)
            call check(error, size(marked) >= 1, "t marked nothing")
        end if

        call kill_buffer(a, st)
        call finish(old, ib)
    end subroutine test_marks

    !> d then x kills the buffer and removes its line.
    subroutine test_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, a
        type(status_type) :: st
        logical :: found, saved_expert

        call init_bindings()
        saved_expert = ibuffer_expert
        ibuffer_expert = .true.
        a = make_buffer("ibuf-gamma", "")
        call start(old, ib, error)
        if (allocated(error)) return
        call goto_buffer_line("ibuf-gamma", found)
        call ibuffer_mark_for_delete(st)
        call goto_buffer_line("ibuf-gamma", found)
        call check(error, ibuffer_current_mark() == "D", "not flagged")

        if (.not. allocated(error)) then
            call ibuffer_do_kill_on_deletion_marks(st)
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) call check(error,.not. buffer_live_p(a), "not killed")

        if (.not. allocated(error)) then
            call goto_buffer_line("ibuf-gamma", found)
            call check(error,.not. found, "still listed")
        end if

        ibuffer_expert = saved_expert
        if (buffer_live_p(a)) call kill_buffer(a, st)
        call finish(old, ib)
    end subroutine test_kill

    !> s a sorts by name; again reverses.
    subroutine test_sort(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, a, b
        type(status_type) :: st
        character(len=:), allocatable :: text

        call init_bindings()
        a = make_buffer("ibuf-zzz", "")
        b = make_buffer("ibuf-aaa", "")
        call start(old, ib, error)
        if (allocated(error)) return
        call ibuffer_do_sort_by("alphabetic", st)
        text = buffer_string()
        call check(error, index(text, "ibuf-aaa") < index(text, "ibuf-zzz"), "not sorted")

        if (.not. allocated(error)) then
            call ibuffer_do_sort_by("alphabetic", st)
            text = buffer_string()
            call check(error, index(text, "ibuf-aaa") > index(text, "ibuf-zzz"), "not reversed")
        end if

        call kill_buffer(a, st)
        call kill_buffer(b, st)
        call finish(old, ib)
    end subroutine test_sort

    !> ` switches to the second format; a FORMATS argument.
    subroutine test_formats(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, ib, a
        type(status_type) :: st
        type(ibuffer_formats_type) :: formats
        character(len=:), allocatable :: line

        call init_bindings()
        a = make_buffer("ibuf-delta", "abc")
        call start(old, ib, error)
        if (allocated(error)) return
        call ibuffer_switch_format(st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call goto_char(point_min())
            line = current_line()
            call check(error, line == "  Name             Filename", line)
        end if

        call finish(old, ib)
        if (allocated(error)) return
        call formats%add([ibuffer_column("name"), ibuffer_text(":"), ibuffer_column("size", 5, -1, IBUFFER_ALIGN_RIGHT)])
        call ibuffer(st, formats=formats)
        ib = get_buffer("*Ibuffer*")
        call set_buffer(ib, st)
        call goto_char(point_min())
        call check(error, index(buffer_string(), LF//"ibuf-delta:    3"//LF) > 0, buffer_string())
        call kill_buffer(a, st)
        call finish(old, ib)
        if (ibuffer_formats%count() /= 2) call check(error, .false., "global formats changed")
    end subroutine test_formats
end module test_ibuffer_fm
