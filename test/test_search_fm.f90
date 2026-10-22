! test_search_fm.f90 -- tests for string search (search_fm).
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

!> Own tests: search-tests.el covers replace-match and regexps, which
!> aren't ported yet.
module test_search_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, FM_SEARCH_FAILED
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, narrow_to_region, widen
    use search_fm, only: search_forward, search_backward, match_beginning, match_end, case_fold_search, &
                         SEARCH_NOERROR, SEARCH_MOVE
    implicit none(type, external)
    private

    public :: collect_search_fm

contains

    subroutine collect_search_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("search-forward", test_forward), &
                    new_unittest("search-backward", test_backward), &
                    new_unittest("search-case-fold", test_case_fold), &
                    new_unittest("search-bound-noerror", test_bound), &
                    new_unittest("search-multibyte", test_multibyte) &
                    ]
    end subroutine collect_search_fm

    subroutine enter(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_buffer_create("search-test", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
        case_fold_search = .true.
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        case_fold_search = .true.
        call kill_buffer(h, st)
    end subroutine leave

    subroutine test_forward(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st, mst
        integer(int64) :: pos

        call enter("foo bar foo baz", h)
        call search_forward("foo", pos, st)
        call check(error, st%ok() .and. pos == 4 .and. point() == 4)
        if (allocated(error)) return
        call check(error, match_beginning(0, mst) == 1 .and. match_end(0, mst) == 4, "match data")
        if (allocated(error)) return
        call search_forward("foo", pos, st)
        call check(error, pos == 12 .and. match_beginning(0, mst) == 9, "again")
        if (allocated(error)) return
        call search_forward("foo", pos, st)
        call check(error, st%code == FM_SEARCH_FAILED .and. point() == 12, "fails")
        if (allocated(error)) return
        call check(error, format_error(st) == 'Search failed: "foo"', format_error(st))
        if (allocated(error)) return
        ! COUNT; the empty string matches at point.
        call goto_char(1_int64)
        call search_forward("o", pos, st, count=3_int64)
        call check(error, pos == 11, "count")
        if (allocated(error)) return
        call search_forward("", pos, st)
        call check(error, pos == 11 .and. match_beginning(0, mst) == 11, "empty")
        if (allocated(error)) return
        ! A negative count searches backward.
        call search_forward("ba", pos, st, count=-1_int64)
        call check(error, pos == 5, "negative count")
        call leave(h)
    end subroutine test_forward

    subroutine test_backward(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st, mst
        integer(int64) :: pos

        call enter("foo bar foo baz", h)
        call goto_char(16_int64)
        call search_backward("foo", pos, st)
        call check(error, st%ok() .and. pos == 9 .and. match_end(0, mst) == 12)
        if (allocated(error)) return
        call search_backward("foo", pos, st)
        call check(error, pos == 1, "again")
        if (allocated(error)) return
        call search_backward("foo", pos, st)
        call check(error, st%code == FM_SEARCH_FAILED .and. point() == 1, "fails")
        if (allocated(error)) return
        ! A match must end before the starting point.
        call goto_char(11_int64)
        call search_backward("foo", pos, st)
        call check(error, pos == 1, "ends before point")
        call leave(h)
    end subroutine test_backward

    subroutine test_case_fold(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: pos

        call enter("Foo FOO foo", h)
        call search_forward("foo", pos, st)
        call check(error, pos == 4, "folding")
        if (allocated(error)) return
        call search_forward("FOO", pos, st)
        call check(error, pos == 8, "upper-case pattern folds too")
        if (allocated(error)) return
        case_fold_search = .false.
        call goto_char(1_int64)
        call search_forward("foo", pos, st)
        call check(error, pos == 12, "case-sensitive")
        call leave(h)
    end subroutine test_case_fold

    subroutine test_bound(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: pos

        call enter("abc abc abc", h)
        ! The match must end by BOUND.
        call search_forward("abc", pos, st, bound=2_int64, noerror=SEARCH_NOERROR)
        call check(error, st%ok() .and. pos == 0 .and. point() == 1, "noerror t")
        if (allocated(error)) return
        call search_forward("abc", pos, st, bound=4_int64)
        call check(error, pos == 4, "exactly at bound")
        if (allocated(error)) return
        call search_forward("abc", pos, st, bound=10_int64, noerror=SEARCH_MOVE)
        call check(error, st%ok() .and. pos == 8, "found within bound")
        if (allocated(error)) return
        call search_forward("x", pos, st, bound=10_int64, noerror=SEARCH_MOVE)
        call check(error, pos == 0 .and. point() == 10, "noerror moves to bound")
        if (allocated(error)) return
        call search_forward("abc", pos, st, bound=2_int64)
        call check(error,.not. st%ok() .and. format_error(st) == "Invalid search bound (wrong side of point)", &
                   format_error(st))
        if (allocated(error)) return
        ! Narrowing limits the search.
        call goto_char(1_int64)
        call narrow_to_region(1_int64, 6_int64, st)
        call search_forward("abc", pos, st, count=2_int64, noerror=SEARCH_NOERROR)
        call check(error, pos == 0, "narrowed")
        call widen()
        call leave(h)
    end subroutine test_bound

    subroutine test_multibyte(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st, mst
        integer(int64) :: pos

        ! "Σίσυφος ΣΟΦΟΣ" with final sigmas: σ, ς and Σ all match.
        call enter("Σίσυφος ΣΟΦΟΣ", h)
        call search_forward("σοφος", pos, st)
        call check(error, st%ok() .and. match_beginning(0, mst) == 9 .and. pos == 14, "Greek")
        if (allocated(error)) return
        call search_backward("ΟΣ", pos, st)
        call check(error, pos == 12, "backward over multibyte")
        if (allocated(error)) return
        call search_backward("φος", pos, st)
        call check(error, pos == 5, "lower-case final sigma")
        call leave(h)
    end subroutine test_multibyte
end module test_search_fm
