! test_saveplace_fm.f90 -- tests for saveplace_fm.
! Ports GNU Emacs 31.1 test/lisp/saveplace-tests.el.
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

!> Ports the five tests of saveplace-tests.el; the resource file
!> test/saveplace-resources/saveplace is upstream's in namelist syntax.
module test_saveplace_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: current_buffer, set_buffer, kill_buffer, restore_buffer_modified_p, get_buffer_create, &
                         generate_new_buffer_name
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string
    use fileio_fm, only: expand_file_name, emacs_wd, init_fileio, insert_file_contents, file_name_as_directory
    use files_fm, only: make_temp_file, delete_file, find_file, file_truename
    use dired_mode_fm, only: dired
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use saveplace_fm, only: save_place_table, save_place_type, save_place_loaded, save_place_file, &
                            save_place_to_table, save_place_forget_unreadable_files, save_place_table_to_file, &
                            save_place_load_table_from_file
    implicit none(type, external)
    private

    public :: collect_saveplace_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_saveplace_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("saveplace-test-save-place-to-alist/dir", test_to_table_dir), &
                    new_unittest("saveplace-test-save-place-to-alist/file", test_to_table_file), &
                    new_unittest("saveplace-test-forget-unreadable-files", test_forget_unreadable), &
                    new_unittest("saveplace-test-place-alist-to-file", test_table_to_file), &
                    new_unittest("saveplace-test-load-alist-from-file", test_load_table) &
                    ]
    end subroutine collect_saveplace_fm

    !> The directory of the resources, ending in a slash.
    function resource_directory() result(dir)
        character(len=:), allocatable :: dir
        type(status_type) :: st

        call init_fileio()
        call expand_file_name("test/saveplace-resources", dir, st, emacs_wd)
        dir = file_name_as_directory(dir)
    end function resource_directory

    !> (save-place-mode), with SAVE_PLACE_TABLE empty and loaded as
    !> LOADED.
    subroutine start(loaded)
        logical, intent(in) :: loaded
        type(status_type) :: st

        call init_bindings()
        call minor_modes%save_place_mode%enable(st)
        if (allocated(save_place_table)) deallocate (save_place_table)
        allocate (save_place_table(0))
        save_place_loaded = loaded
    end subroutine start

    !> Make the table the places FILES with POSITIONS.
    subroutine set_table(files, positions)
        character(len=*), intent(in) :: files(:)
        integer(int64), intent(in) :: positions(:)
        integer :: k

        if (allocated(save_place_table)) deallocate (save_place_table)
        allocate (save_place_table(size(files)))

        do k = 1, size(files)
            save_place_table(k)%file = trim(files(k))
            save_place_table(k)%position = positions(k)
        end do
    end subroutine set_table

    subroutine test_to_table_dir(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: loc

        call start(.true.)
        loc = resource_directory()
        old = current_buffer()
        call show_alone()
        call dired(loc, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call save_place_to_table()
        call check(error, size(save_place_table) == 1, "one place")
        if (allocated(error)) return
        call check(error, save_place_table(1)%file, loc)
        if (allocated(error)) return
        call check(error, allocated(save_place_table(1)%dired_filename), "a Dired place")
        if (allocated(error)) return
        call check(error, save_place_table(1)%dired_filename, loc//"saveplace")
        call kill_buffer(current_buffer(), st)
        call set_buffer(old, st)
    end subroutine test_to_table_dir

    subroutine test_to_table_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: tmp, tmpfile

        call start(.true.)
        call make_temp_file("saveplace-test", tmp, st)
        call file_truename(tmp, tmpfile, st)
        old = current_buffer()
        call show_alone()
        call find_file(tmpfile, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! (Must insert something.)
        call insert("abc", st)
        call save_place_to_table()
        call check(error, size(save_place_table) == 1, "one place")
        if (allocated(error)) return
        call check(error, save_place_table(1)%file, tmpfile)
        if (allocated(error)) return
        call check(error, save_place_table(1)%position == 4 .and. .not. allocated(save_place_table(1)%dired_filename), &
                   "at 4")
        h = current_buffer()
        call restore_buffer_modified_p(.false.)
        call kill_buffer(h, st)
        call set_buffer(old, st)
        call delete_file(tmpfile, st)
    end subroutine test_to_table_file

    subroutine test_forget_unreadable(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: tmpfile

        call start(.true.)
        call make_temp_file("saveplace-test", tmpfile, st)
        call set_table([character(len=len(tmpfile) + 32) :: "/this/file/does/not/exist", tmpfile], [10_int64, 1917_int64])
        call save_place_forget_unreadable_files()
        call check(error, size(save_place_table) == 1, "one place left")
        if (allocated(error)) return
        call check(error, save_place_table(1)%file, tmpfile)
        if (allocated(error)) return
        call check(error, save_place_table(1)%position == 1917, "1917")
        call delete_file(tmpfile, st)
    end subroutine test_forget_unreadable

    subroutine test_table_to_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: old, h
        character(len=:), allocatable :: tmpfile, tmpfile2, text
        integer(int64) :: inserted

        call start(.true.)
        call make_temp_file("saveplace-test", tmpfile, st)
        call make_temp_file("saveplace-test", tmpfile2, st)
        save_place_file = tmpfile
        call set_table([tmpfile2], [99_int64])
        call save_place_table_to_file()
        if (allocated(save_place_table)) deallocate (save_place_table)
        allocate (save_place_table(0))
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *saveplace-test*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(tmpfile, .false., inserted, st)
        text = buffer_string()
        call kill_buffer(h, st)
        call set_buffer(old, st)
        call check(error, index(text, tmpfile2) > 0, text)
        call delete_file(tmpfile, st)
        call delete_file(tmpfile2, st)
    end subroutine test_table_to_file

    subroutine test_load_table(error)
        type(error_type), allocatable, intent(out) :: error

        call start(.false.)
        save_place_file = resource_directory()//"saveplace"
        call save_place_load_table_from_file()
        call check(error, size(save_place_table) == 2, "two places")
        if (allocated(error)) return
        call check(error, save_place_table(1)%file, "/home/skangas/.emacs.d/cache/recentf")
        if (allocated(error)) return
        call check(error, save_place_table(1)%position == 1306 .and. .not. allocated(save_place_table(1)%dired_filename), &
                   "1306")
        if (allocated(error)) return
        call check(error, save_place_table(2)%file, "/home/skangas/wip/emacs/")
        if (allocated(error)) return
        call check(error, allocated(save_place_table(2)%dired_filename), "a Dired place")
        if (allocated(error)) return
        call check(error, save_place_table(2)%dired_filename, "/home/skangas/wip/emacs/COPYING")
    end subroutine test_load_table
end module test_saveplace_fm
