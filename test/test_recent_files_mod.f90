! test_recent_files_mod.f90 -- tests for recent_files_mod.
! Own tests; upstream has no recentf tests.
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

!> Own tests (upstream has no recentf-tests.el): adding files, the
!> exclude and keep checks, cleanup, saving and loading, and
!> `recent_files_open_most_recent_file''s errors.
module test_recent_files_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use value_mod, only: value_type, list_length, list_element, xstring
    use data_mod, only: set, symbol_value
    use files_mod, only: create_temporary_file, delete_file
    use bindings_mod, only: initialize_bindings
    use recent_files_mod, only: recent_files_files, recent_files_add_file, recent_files_exclude, recent_files_check_type, &
                                recent_files_keep, recent_files_keep_default_test, is_recent_files_exclude_file_by_extension, &
                                recent_files_exclude_ignored_extensions, recent_files_remove_if_non_kept, recent_files_cleanup, &
                                recent_files_save_list, recent_files_load_list, recent_files_save_file, &
                                recent_files_open_most_recent_file, recent_files_show_messages
    implicit none(type, external)
    private

    public :: collect_recent_files_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_recent_files_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("recentf-add-file", test_add_file), &
                    new_unittest("recentf-exclude", test_exclude), &
                    new_unittest("recentf-keep-and-cleanup", test_keep_cleanup), &
                    new_unittest("recentf-save-and-load", test_save_load), &
                    new_unittest("recentf-open-most-recent-file-errors", test_open_errors) &
                    ]
    end subroutine collect_recent_files_mod

    !> Defaults, and an empty list.
    subroutine start()
        call initialize_bindings()
        if (allocated(recent_files_files)) deallocate (recent_files_files)
        allocate (recent_files_files(0))
        if (allocated(recent_files_exclude)) deallocate (recent_files_exclude)
        allocate (recent_files_exclude(0))
        if (allocated(recent_files_exclude_ignored_extensions)) deallocate (recent_files_exclude_ignored_extensions)
        allocate (recent_files_exclude_ignored_extensions(0))
        if (allocated(recent_files_keep)) deallocate (recent_files_keep)
        allocate (recent_files_keep(1))
        recent_files_keep(1)%test => recent_files_keep_default_test
        recent_files_show_messages = .false.
    end subroutine start

    !> The names of `recent_files_files' joined by "|".
    function joined() result(s)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(recent_files_files)
            if (k > 1) s = s//"|"
            s = s//recent_files_files(k)%name
        end do
    end function joined

    !> Adding a file puts it first, once.
    subroutine test_add_file(error)
        type(error_type), allocatable, intent(out) :: error

        call start()
        call recent_files_add_file("/tmp/recentf-x")
        call recent_files_add_file("/tmp/recentf-y")
        call recent_files_add_file("/tmp/recentf-x")
        call check(error, joined(), "/tmp/recentf-x|/tmp/recentf-y")
        if (allocated(error)) return
        call recent_files_add_file("/tmp/../tmp/recentf-y")
        call check(error, joined(), "/tmp/recentf-y|/tmp/recentf-x")
    end subroutine test_add_file

    !> A regexp and a test of `recent_files_exclude'.
    subroutine test_exclude(error)
        type(error_type), allocatable, intent(out) :: error

        call start()
        recent_files_exclude = [recent_files_check_type(regular_expression="\.tmp\'"), &
                                recent_files_check_type(test=is_recent_files_exclude_file_by_extension)]
        recent_files_exclude_ignored_extensions = [character(len=32) :: ".o"]
        call recent_files_add_file("/tmp/a.tmp")
        call recent_files_add_file("/tmp/a.o")
        call recent_files_add_file("/tmp/a.f90")
        call check(error, joined(), "/tmp/a.f90")
        if (allocated(error)) return
        call check(error, &
                  is_recent_files_exclude_file_by_extension("x.o") .and. .not. is_recent_files_exclude_file_by_extension("x.f90"), &
                   "by extension")
    end subroutine test_exclude

    !> Files that are not readable are not kept; cleanup drops them and
    !> duplicates.
    subroutine test_keep_cleanup(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(status_type) :: st
        logical :: removed

        call start()
        call create_temporary_file("recentf-test", tmpfile, st)
        call recent_files_add_file("/this/file/does/not/exist")
        call recent_files_add_file(tmpfile)
        call recent_files_remove_if_non_kept("/this/file/does/not/exist", removed)
        call check(error, removed, "removed")
        if (allocated(error)) return
        call recent_files_remove_if_non_kept(tmpfile, removed)
        call check(error,.not. removed, "kept")
        if (allocated(error)) return
        ! A duplicate and a file that can't be read, then cleanup.
        deallocate (recent_files_files)
        allocate (recent_files_files(3))
        recent_files_files(1)%name = tmpfile
        recent_files_files(2)%name = "/this/file/does/not/exist"
        recent_files_files(3)%name = tmpfile
        call recent_files_cleanup()
        call check(error, joined(), tmpfile)
        call delete_file(tmpfile, st)
    end subroutine test_keep_cleanup

    !> The list saved and loaded back; an empty `file_name_history' gets
    !> it.
    subroutine test_save_load(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(status_type) :: st
        type(value_type) :: history

        call start()
        call create_temporary_file("recentf-test", tmpfile, st)
        recent_files_save_file = tmpfile
        deallocate (recent_files_files)
        allocate (recent_files_files(2))
        recent_files_files(1)%name = "/tmp/one"
        recent_files_files(2)%name = '/tmp/two "quoted"'
        call recent_files_save_list()
        deallocate (recent_files_files)
        allocate (recent_files_files(0))
        call set(value_type(name="file_name_history"), value_type(), st)
        call recent_files_load_list(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, joined(), '/tmp/one|/tmp/two "quoted"')
        if (allocated(error)) return
        call symbol_value(value_type(name="file_name_history"), history, st)
        call check(error, list_length(history) == 2, "file_name_history")
        if (allocated(error)) return
        call check(error, xstring(list_element(history, 1_int64)), "/tmp/one")
        call delete_file(tmpfile, st)
    end subroutine test_save_load

    subroutine test_open_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call start()
        call recent_files_open_most_recent_file(11_int64, st)
        call check(error, format_error(st), "Recent file number out of range [0-9], 11")
        if (allocated(error)) return
        call recent_files_open_most_recent_file(1_int64, st)
        call check(error, format_error(st), "Not that many recent files")
    end subroutine test_open_errors
end module test_recent_files_mod
