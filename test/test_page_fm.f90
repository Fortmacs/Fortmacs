! test_page_fm.f90 -- tests for the page commands (page_fm).
! Ports GNU Emacs 31.1 test/lisp/textmodes/page-tests.el.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/textmodes/page-tests.el (all its tests).
module test_page_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_string, buffer_substring, widen, is_bob, &
                          region_beginning, region_end
    use cmds_fm, only: forward_char, forward_line
    use search_fm, only: looking_at
    use bindings_fm, only: init_bindings
    use page_fm, only: forward_page, backward_page, mark_page, narrow_to_page, page_count_lines_page, page_what_page
    implicit none(type, external)
    private

    public :: collect_page_fm

    character(len=*), parameter :: TEXT = "foo"//achar(10)//achar(12)//achar(10)//"bar"//achar(10)//achar(12)// &
                                   achar(10)//"baz"

contains

    !> Collect all exported unit tests.
    subroutine collect_page_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("page-tests-forward-page", test_forward_page), &
                    new_unittest("page-tests-backward-page", test_backward_page), &
                    new_unittest("page-tests-mark-page", test_mark_page), &
                    new_unittest("page-tests-narrow-to-page", test_narrow_to_page), &
                    new_unittest("page-tests-count-lines-page", test_count_lines_page), &
                    new_unittest("page-tests-what-page", test_what_page) &
                    ]
    end subroutine collect_page_fm

    !> A fresh buffer holding TEXT, point at its end (with-temp-buffer,
    !> insert).
    subroutine temp_buffer()
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *page-test*", buf, st)
        call set_buffer(buf, st)
        call widen()
        call erase_buffer(st)
        call insert(TEXT, st)
    end subroutine temp_buffer

    !> (looking-at-p (concat "\n" S))
    logical function at_page(s)
        character(len=*), intent(in) :: s
        type(status_type) :: st

        at_page = looking_at(achar(10)//s, st)
    end function at_page

    !> The contents of the region as a string.
    function region_string() result(s)
        character(len=:), allocatable :: s
        integer(int64) :: b, e
        type(status_type) :: st

        call region_beginning(b, st)
        call region_end(e, st)
        call buffer_substring(b, e, s, st)
    end function region_string

    subroutine test_forward_page(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call forward_page(st, -1_int64)
        call check(error, at_page("baz"), "-1")
        if (allocated(error)) return
        call forward_page(st, -2_int64)
        call check(error, point() == point_min(), "-2")
        if (allocated(error)) return
        call forward_page(st, 1_int64)
        call check(error, at_page("bar"), "1")
        if (allocated(error)) return
        call forward_page(st)
        call check(error, at_page("baz"), "nil")
        if (allocated(error)) return
        call forward_page(st, 1_int64)
        call check(error, point() == point_max(), "1 to the end")
    end subroutine test_forward_page

    subroutine test_backward_page(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call backward_page(st, 1_int64)
        call check(error, at_page("baz"), "1")
        if (allocated(error)) return
        call backward_page(st)
        call check(error, at_page("bar"), "nil")
        if (allocated(error)) return
        call backward_page(st, 1_int64)
        call check(error, point() == point_min(), "1 to the start")
        if (allocated(error)) return
        call backward_page(st, -1_int64)
        call check(error, at_page("bar"), "-1")
        if (allocated(error)) return
        call backward_page(st, -2_int64)
        call check(error, point() == point_max(), "-2")
    end subroutine test_backward_page

    subroutine test_mark_page(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call mark_page(0_int64, st)
        call check(error, region_string(), achar(10)//"baz")
        if (allocated(error)) return
        call mark_page(-2_int64, st)
        call check(error, region_string(), "foo"//achar(10)//achar(12))
        if (allocated(error)) return
        call mark_page(1_int64, st)
        call check(error, region_string(), achar(10)//"bar"//achar(10)//achar(12))
    end subroutine test_mark_page

    subroutine test_narrow_to_page(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer()
        call goto_char(point_min())

        checks: block
            call narrow_to_page(0_int64, st)
            call check(error, buffer_string(), "foo"//achar(10))
            if (allocated(error)) exit checks
            call narrow_to_page(2_int64, st)
            call check(error, buffer_string(), "baz")
            if (allocated(error)) exit checks
            call narrow_to_page(-1_int64, st)
            call check(error, buffer_string(), "bar"//achar(10))
            if (allocated(error)) exit checks
            call widen()
            call goto_char(point_min())
            call narrow_to_page(0_int64, st)
            call check(error, buffer_string(), "foo"//achar(10))
            if (allocated(error)) exit checks
            call goto_char(point_max())
            call narrow_to_page(2_int64, st)
            call check(error, buffer_string(), "baz")
            if (allocated(error)) exit checks
            call goto_char(point_max())
            call narrow_to_page(-1_int64, st)
            call check(error, buffer_string(), "bar"//achar(10))
        end block checks

        call widen()
    end subroutine test_narrow_to_page

    subroutine test_count_lines_page(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: total, before, after

        call temp_buffer()
        call goto_char(point_min())
        call page_count_lines_page(total, before, after)
        call check(error, total == 1 .and. before == 0 .and. after == 1, "(1 0 1)")
        if (allocated(error)) return
        call goto_char(point_max())
        call page_count_lines_page(total, before, after)
        call check(error, total == 2 .and. before == 2 .and. after == 0, "(2 2 0)")
    end subroutine test_count_lines_page

    subroutine test_what_page(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: page, line, shortage
        type(status_type) :: st

        call temp_buffer()
        call goto_char(point_min())
        call page_what_page(page, line)
        call check(error, page == 1 .and. line == 1, "(1 1)")
        if (allocated(error)) return
        call forward_char(st)
        call page_what_page(page, line)
        call check(error, page == 1 .and. line == 1, "(1 1) after a char")
        if (allocated(error)) return
        call forward_page(st)
        call page_what_page(page, line)
        call check(error, page == 2 .and. line == 1, "(2 1)")
        if (allocated(error)) return
        shortage = forward_line()
        call page_what_page(page, line)
        call check(error, page == 2 .and. line == 2, "(2 2)")
    end subroutine test_what_page
end module test_page_fm
