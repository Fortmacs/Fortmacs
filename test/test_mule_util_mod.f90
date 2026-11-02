! test_mule_util_mod.f90 -- tests for mule_util_mod.
! Ports GNU Emacs 31.1 test/lisp/international/mule-util-tests.el in
! part; the filepos and bufferpos tests wait for those functions.
!
! Copyright (C) 2002-2026 Free Software Foundation, Inc.
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

!> Ports the `mule-util-test-truncate-NN' tests of mule-util-tests.el
!> (the data of `mule-util-test-truncate-data', with
!> `truncate_string_ellipsis_variable' "...").  The filepos/bufferpos tests are
!> deferred with those functions.
module test_mule_util_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use mule_util_mod, only: truncate_string_to_width, truncate_string_ellipsis_variable
    implicit none(type, external)
    private

    public :: collect_mule_util_mod

    !> Test number, for the messages.
    integer :: n = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_mule_util_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [new_unittest("mule-util-test-truncate", test_truncate)]
    end subroutine collect_mule_util_mod

    !> One element of the test data: STRING truncated to END_COLUMN (with
    !> START_COLUMN, PADDING, ELLIPSIS or the default ellipsis) is
    !> EXPECTED.
    subroutine truncate(error, string, end_column, expected, start_column, padding, ellipsis, default_ellipsis)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: string, expected
        integer, intent(in) :: end_column
        integer, intent(in), optional :: start_column
        character(len=*), intent(in), optional :: padding, ellipsis
        logical, intent(in), optional :: default_ellipsis
        character(len=:), allocatable :: got
        character(len=8) :: num

        n = n + 1
        if (allocated(error)) return
        got = truncate_string_to_width(string, end_column, start_column, padding, ellipsis, default_ellipsis)
        write (num, "(i2.2)") n - 1
        call check(error, got == expected .and. len(got) == len(expected), &
                   "mule-util-test-truncate-"//trim(num)//": expected '"//expected//"' but got '"//got//"'")
    end subroutine truncate

    subroutine test_truncate(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: zhong = "中", wen = "文", kor = "kor한e글an", jp = "こhんeにlちlはo", &
                                       yue = "粵語", nihongo = "日本語"

        n = 0
        truncate_string_ellipsis_variable = "..."
        call truncate(error, "", 0, "")
        call truncate(error, "x", 1, "x")
        call truncate(error, "xy", 1, "x")
        call truncate(error, "xy", 2, "y", 1)
        call truncate(error, "xy", 0, "")
        call truncate(error, "xy", 3, "xy")
        call truncate(error, zhong, 0, "")
        call truncate(error, zhong, 1, "")
        call truncate(error, zhong, 2, zhong)
        call truncate(error, zhong, 1, " ", padding=" ")
        call truncate(error, zhong//wen, 3, "  ", 1, " ")
        call truncate(error, "x"//zhong//"x", 2, "x")
        call truncate(error, "x"//zhong//"x", 3, "x"//zhong)
        call truncate(error, "x"//zhong//"x", 3, "x"//zhong)
        call truncate(error, "x"//zhong//"x", 4, zhong//"x", 1)
        call truncate(error, kor, 8, "or한e글", 1, " ")
        call truncate(error, kor, 7, "r한e ", 2, " ")
        call truncate(error, "", 0, "", ellipsis="...")
        call truncate(error, "x", 3, "x", ellipsis="...")
        call truncate(error, zhong, 3, zhong, ellipsis="...")
        call truncate(error, "foo", 3, "foo", ellipsis="...")
        call truncate(error, "foo", 2, "fo", ellipsis="...")
        call truncate(error, "foobar", 6, "foobar", 0, ellipsis="...")
        call truncate(error, "foobarbaz", 6, "foo...", ellipsis="...")
        call truncate(error, "foobarbaz", 7, "ob...", 2, ellipsis="...")
        call truncate(error, "foobarbaz", 9, "barbaz", 3, ellipsis="...")
        call truncate(error, jp, 15, " hんeにlちlはo", 1, " ", default_ellipsis=.true.)
        call truncate(error, jp, 14, " hんeにlち...", 1, " ", default_ellipsis=.true.)
        call truncate(error, "x", 3, "x", ellipsis=yue)
        call truncate(error, zhong, 2, zhong, ellipsis=yue)
        call truncate(error, zhong, 1, "x", padding="x", ellipsis=yue)
        call truncate(error, zhong//wen, 3, zhong//" ", padding=" ", ellipsis=yue)
        call truncate(error, "foobarbaz", 4, yue, ellipsis=yue)
        call truncate(error, "foobarbaz", 5, "f"//yue, ellipsis=yue)
        call truncate(error, "foobarbaz", 6, "fo"//yue, ellipsis=yue)
        call truncate(error, "foobarbaz", 8, "b"//yue, 3, ellipsis=yue)
        call truncate(error, jp, 14, "xeに"//nihongo, 4, "x", nihongo)
        call truncate(error, jp, 13, "xex"//nihongo, 4, "x", nihongo)
        deallocate (truncate_string_ellipsis_variable)
    end subroutine test_truncate
end module test_mule_util_mod
