! test_dired_fm.f90 -- tests for dired_fm.
! Ported from GNU Emacs 31.1 test/lisp/dired-tests.el
! (dired-test-directory-files, without the MATCH regexps).
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

module test_dired_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, list_length, list_elements, xstring
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer
    use fileio_fm, only: write_region, delete_file_internal, delete_directory_internal, &
                         make_directory_internal
    use files_fm, only: temporary_file_directory
    use dired_fm, only: directory_files
    implicit none(type, external)
    private

    public :: collect_dired_fm

contains

    subroutine collect_dired_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("dired-test-directory-files", test_directory_files) &
                    ]
    end subroutine collect_dired_fm


    !> The names in `files`, joined.
    function joined(files) result(s)
        type(value_type), intent(in) :: files
        character(len=:), allocatable :: s
        type(value_type), allocatable :: names(:)
        integer :: k

        call list_elements(files, names)
        s = ""

        do k = 1, size(names)
            s = s//xstring(names(k))
        end do
    end function joined


    subroutine test_directory_files(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=1), parameter :: made(4) = ["a", "b", "c", "d"]
        character(len=:), allocatable :: testdir
        type(value_type) :: files
        type(handle_type) :: h
        type(status_type) :: st
        integer :: k

        testdir = temporary_file_directory()//"directory-files-test"
        call get_buffer_create(" *dired-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call make_directory_internal(testdir, st)

        body: block
            call directory_files(testdir, files, st)
            call check(error, st%ok() .and. list_length(files) == 2, "empty: . and ..")
            if (allocated(error)) exit body
            call check(error, joined(files), "...")
            if (allocated(error)) exit body

            do k = 1, size(made)
                call write_region(testdir//"/"//made(k), .false., st)
            end do

            call directory_files(testdir, files, st)
            call check(error, list_length(files) == 6, "six")
            if (allocated(error)) exit body
            call check(error, joined(files), "...abcd")
            if (allocated(error)) exit body
            call directory_files(testdir, files, st, count=3_int64)
            call check(error, list_length(files) == 3, "count")
            if (allocated(error)) exit body
            call directory_files(testdir, files, st, full=.true.)
            call check(error, joined(files), testdir//"/."//testdir//"/.."//testdir//"/a"//testdir//"/b"// &
                       testdir//"/c"//testdir//"/d")
            if (allocated(error)) exit body
            call directory_files(testdir//"/missing", files, st)
            call check(error, format_error(st), "Opening directory: No such file or directory, "// &
                       testdir//"/missing")
        end block body

        do k = 1, size(made)
            call delete_file_internal(testdir//"/"//made(k), st)
        end do

        call delete_directory_internal(testdir, st)
    end subroutine test_directory_files

end module test_dired_fm
