! test_namelist_fm.f90 -- tests for namelist_fm.
! Own tests; namelist_fm ports nothing.
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

!> Own tests (namelist_fm ports nothing): writing a group and reading it
!> back, the syntax a namelist allows, and syntax errors.
module test_namelist_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, condition_matches, FM_INVALID_READ_SYNTAX
    use namelist_fm, only: namelist_group_type, namelist_item_type, read_namelist, namelist_text, &
                           NAMELIST_STRING, NAMELIST_INTEGER, NAMELIST_LOGICAL
    implicit none(type, external)
    private

    public :: collect_namelist_fm

    character(len=1), parameter :: nl = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_namelist_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("namelist-write-read", test_write_read), &
                    new_unittest("namelist-syntax", test_syntax), &
                    new_unittest("namelist-errors", test_errors) &
                    ]
    end subroutine collect_namelist_fm

    !> A group written and read back is the same.
    subroutine test_write_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(namelist_group_type) :: group
        type(namelist_item_type) :: item
        type(namelist_group_type), allocatable :: groups(:)
        type(status_type) :: st
        character(len=:), allocatable :: text

        group%name = "savehist"
        item = namelist_item_type(name="file_name_history", subscripted=.true., lower=1, upper=2)
        call item%add_string('~/say "hi".txt')
        call item%add_string("two"//nl//"lines")
        call group%add(item)
        item = namelist_item_type(name="place", component="position", subscripted=.true., lower=3, upper=3)
        call item%add_integer(-42_int64)
        call group%add(item)
        item = namelist_item_type(name="flag")
        call item%add_logical(.true.)
        call group%add(item)
        text = namelist_text(group)
        call check(error, text, "&savehist"//nl// &
                   ' file_name_history(1:2) = "~/say ""hi"".txt", "two'//nl//'lines",'//nl// &
                   " place(3)%position = -42,"//nl// &
                   " flag = .true.,"//nl//"/"//nl)
        if (allocated(error)) return
        call read_namelist(text, groups, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(groups) == 1 .and. size(groups(1)%items) == 3, "one group, three objects")
        if (allocated(error)) return

        associate (it => groups(1)%items)
            call check(error, it(1)%name == "file_name_history" .and. it(1)%subscripted .and. it(1)%lower == 1 .and. &
                       it(1)%upper == 2 .and. size(it(1)%values) == 2, "the history")
            if (allocated(error)) return
            call check(error, it(1)%values(1)%string, '~/say "hi".txt')
            if (allocated(error)) return
            call check(error, it(1)%values(2)%string, "two"//nl//"lines")
            if (allocated(error)) return
            call check(error, it(2)%component == "position" .and. it(2)%lower == 3 .and. &
                       it(2)%values(1)%kind == NAMELIST_INTEGER .and. it(2)%values(1)%integer == -42, "the position")
            if (allocated(error)) return
            call check(error,.not. it(3)%subscripted .and. it(3)%values(1)%kind == NAMELIST_LOGICAL .and. &
                       it(3)%values(1)%logical, "the flag")
        end associate
    end subroutine test_write_read

    !> Comments, single quotes, blanks for separators, T and F, two
    !> groups.
    subroutine test_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        type(namelist_group_type), allocatable :: groups(:)
        type(status_type) :: st

        call read_namelist("! a comment"//nl//"&one a = 'x''y' 'z' ! more"//nl//" b=T c = .f. /"//nl// &
                           "&two t(1:1)='!'/", groups, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(groups) == 2, "two groups")
        if (allocated(error)) return

        associate (it => groups(1)%items)
            call check(error, size(it) == 3, "three objects")
            if (allocated(error)) return
            call check(error, size(it(1)%values) == 2, "a")
            if (.not. allocated(error)) call check(error, it(1)%values(1)%string == "x'y" .and. &
                                                   it(1)%values(2)%string == "z", "a")
            if (allocated(error)) return
            call check(error, it(2)%name == "b" .and. it(2)%values(1)%kind == NAMELIST_LOGICAL .and. &
                       it(2)%values(1)%logical, "b")
            if (allocated(error)) return
            call check(error, it(3)%values(1)%kind == NAMELIST_LOGICAL .and. .not. it(3)%values(1)%logical, "c")
            if (allocated(error)) return
        end associate

        call check(error, groups(2)%name == "two" .and. groups(2)%items(1)%values(1)%kind == NAMELIST_STRING .and. &
                   groups(2)%items(1)%values(1)%string == "!", "a ! in a string")
    end subroutine test_syntax

    !> An unterminated group or string, and a missing `=', are
    !> invalid_read_syntax errors.
    subroutine test_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(namelist_group_type), allocatable :: groups(:)
        type(status_type) :: st

        call read_namelist("&g a = 1", groups, st)
        call check(error, condition_matches(st%code, FM_INVALID_READ_SYNTAX), "no /")
        if (allocated(error)) return
        call read_namelist('&g a = "x'//nl//"/", groups, st)
        call check(error, condition_matches(st%code, FM_INVALID_READ_SYNTAX), "unterminated string")
        if (allocated(error)) return
        call read_namelist(nl//"&g a 1 /", groups, st)
        call check(error, condition_matches(st%code, FM_INVALID_READ_SYNTAX), "no =")
        if (allocated(error)) return
        call check(error, index(format_error(st), "line 2") > 0, format_error(st))
    end subroutine test_errors
end module test_namelist_fm
