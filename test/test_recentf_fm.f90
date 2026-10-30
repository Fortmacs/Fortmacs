! test_recentf_fm.f90 -- tests for recentf_fm.
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
!> `recentf_open_most_recent_file''s errors.
module test_recentf_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use lisp_fm, only: value_type, nil, list_length, list_elt, xstring
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use files_fm, only: make_temp_file, delete_file
    use bindings_fm, only: init_bindings
    use recentf_fm, only: recentf_list, recentf_add_file, recentf_exclude, recentf_check_type, recentf_keep, &
                          recentf_keep_default_predicate, recentf_exclude_file_by_extension_p, &
                          recentf_exclude_ignored_extensions, recentf_remove_if_non_kept, recentf_cleanup, &
                          recentf_save_list, recentf_load_list, recentf_save_file, recentf_open_most_recent_file, &
                          recentf_show_messages
    implicit none(type, external)
    private

    public :: collect_recentf_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_recentf_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("recentf-add-file", test_add_file), &
                    new_unittest("recentf-exclude", test_exclude), &
                    new_unittest("recentf-keep-and-cleanup", test_keep_cleanup), &
                    new_unittest("recentf-save-and-load", test_save_load), &
                    new_unittest("recentf-open-most-recent-file-errors", test_open_errors) &
                    ]
    end subroutine collect_recentf_fm

    !> Defaults, and an empty list.
    subroutine start()
        call init_bindings()
        if (allocated(recentf_list)) deallocate (recentf_list)
        allocate (recentf_list(0))
        if (allocated(recentf_exclude)) deallocate (recentf_exclude)
        allocate (recentf_exclude(0))
        if (allocated(recentf_exclude_ignored_extensions)) deallocate (recentf_exclude_ignored_extensions)
        allocate (recentf_exclude_ignored_extensions(0))
        if (allocated(recentf_keep)) deallocate (recentf_keep)
        allocate (recentf_keep(1))
        recentf_keep(1)%predicate => recentf_keep_default_predicate
        recentf_show_messages = .false.
    end subroutine start

    !> The names of `recentf_list' joined by "|".
    function joined() result(s)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(recentf_list)
            if (k > 1) s = s//"|"
            s = s//recentf_list(k)%name
        end do
    end function joined

    !> Adding a file puts it first, once.
    subroutine test_add_file(error)
        type(error_type), allocatable, intent(out) :: error

        call start()
        call recentf_add_file("/tmp/recentf-x")
        call recentf_add_file("/tmp/recentf-y")
        call recentf_add_file("/tmp/recentf-x")
        call check(error, joined(), "/tmp/recentf-x|/tmp/recentf-y")
        if (allocated(error)) return
        call recentf_add_file("/tmp/../tmp/recentf-y")
        call check(error, joined(), "/tmp/recentf-y|/tmp/recentf-x")
    end subroutine test_add_file

    !> A regexp and a predicate of `recentf_exclude'.
    subroutine test_exclude(error)
        type(error_type), allocatable, intent(out) :: error

        call start()
        recentf_exclude = [recentf_check_type(regexp="\.tmp\'"), &
                           recentf_check_type(predicate=recentf_exclude_file_by_extension_p)]
        recentf_exclude_ignored_extensions = [character(len=32) :: ".o"]
        call recentf_add_file("/tmp/a.tmp")
        call recentf_add_file("/tmp/a.o")
        call recentf_add_file("/tmp/a.f90")
        call check(error, joined(), "/tmp/a.f90")
        if (allocated(error)) return
        call check(error, recentf_exclude_file_by_extension_p("x.o") .and. .not. recentf_exclude_file_by_extension_p("x.f90"), &
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
        call make_temp_file("recentf-test", tmpfile, st)
        call recentf_add_file("/this/file/does/not/exist")
        call recentf_add_file(tmpfile)
        call recentf_remove_if_non_kept("/this/file/does/not/exist", removed)
        call check(error, removed, "removed")
        if (allocated(error)) return
        call recentf_remove_if_non_kept(tmpfile, removed)
        call check(error,.not. removed, "kept")
        if (allocated(error)) return
        ! A duplicate and a file that can't be read, then cleanup.
        deallocate (recentf_list)
        allocate (recentf_list(3))
        recentf_list(1)%name = tmpfile
        recentf_list(2)%name = "/this/file/does/not/exist"
        recentf_list(3)%name = tmpfile
        call recentf_cleanup()
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
        call make_temp_file("recentf-test", tmpfile, st)
        recentf_save_file = tmpfile
        deallocate (recentf_list)
        allocate (recentf_list(2))
        recentf_list(1)%name = "/tmp/one"
        recentf_list(2)%name = '/tmp/two "quoted"'
        call recentf_save_list()
        deallocate (recentf_list)
        allocate (recentf_list(0))
        call set(sym("file_name_history"), nil(), st)
        call recentf_load_list(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, joined(), '/tmp/one|/tmp/two "quoted"')
        if (allocated(error)) return
        call symbol_value(sym("file_name_history"), history, st)
        call check(error, list_length(history) == 2, "file_name_history")
        if (allocated(error)) return
        call check(error, xstring(list_elt(history, 1_int64)), "/tmp/one")
        call delete_file(tmpfile, st)
    end subroutine test_save_load

    subroutine test_open_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call start()
        call recentf_open_most_recent_file(11_int64, st)
        call check(error, format_error(st), "Recent file number out of range [0-9], 11")
        if (allocated(error)) return
        call recentf_open_most_recent_file(1_int64, st)
        call check(error, format_error(st), "Not that many recent files")
    end subroutine test_open_errors
end module test_recentf_fm
