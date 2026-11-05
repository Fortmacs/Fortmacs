! test_saveplace_mod.f90 -- tests for saveplace_mod.
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
module test_saveplace_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: current_buffer, set_buffer, close_buffer, restore_buffer_modified, get_or_make_buffer, &
                          generate_new_buffer_name
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, initialize_fileio, insert_file_contents, file_name_as_directory
    use files_mod, only: create_temporary_file, delete_file, find_file, file_truename
    use directory_editor_mode_mod, only: dired
    use bindings_mod, only: initialize_bindings
    use modes_mod, only: minor_modes
    use saveplace_mod, only: save_places, save_place_type, save_place_loaded, save_place_file, &
                             save_place_store, save_place_forget_unreadable_files, save_places_to_file, &
                             save_place_load_places_from_file
    implicit none(type, external)
    private

    public :: collect_saveplace_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_saveplace_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("saveplace-test-save-place-to-alist/dir", test_to_table_directory), &
                    new_unittest("saveplace-test-save-place-to-alist/file", test_to_table_file), &
                    new_unittest("saveplace-test-forget-unreadable-files", test_forget_unreadable), &
                    new_unittest("saveplace-test-place-alist-to-file", test_table_to_file), &
                    new_unittest("saveplace-test-load-alist-from-file", test_load_table) &
                    ]
    end subroutine collect_saveplace_mod

    !> The directory of the resources, ending in a slash.
    function resource_directory() result(directory)
        character(len=:), allocatable :: directory
        type(status_type) :: st

        call initialize_fileio()
        call expand_file_name("test/saveplace-resources", directory, st, fortmacs_wd)
        directory = file_name_as_directory(directory)
    end function resource_directory

    !> (save-place-mode), with SAVE_PLACES empty and loaded as
    !> LOADED.
    subroutine start(loaded)
        logical, intent(in) :: loaded
        type(status_type) :: st

        call initialize_bindings()
        call minor_modes%save_place_mode%enable(st)
        if (allocated(save_places)) deallocate (save_places)
        allocate (save_places(0))
        save_place_loaded = loaded
    end subroutine start

    !> Make the table the places FILES with POSITIONS.
    subroutine set_table(files, positions)
        character(len=*), intent(in) :: files(:)
        integer(int64), intent(in) :: positions(:)
        integer :: k

        if (allocated(save_places)) deallocate (save_places)
        allocate (save_places(size(files)))

        do k = 1, size(files)
            save_places(k)%file = trim(files(k))
            save_places(k)%position = positions(k)
        end do
    end subroutine set_table

    subroutine test_to_table_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: location

        call start(.true.)
        location = resource_directory()
        old = current_buffer()
        call show_alone()
        call dired(location, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call save_place_store()
        call check(error, size(save_places) == 1, "one place")
        if (allocated(error)) return
        call check(error, save_places(1)%file, location)
        if (allocated(error)) return
        call check(error, allocated(save_places(1)%dired_filename), "a Dired place")
        if (allocated(error)) return
        call check(error, save_places(1)%dired_filename, location//"saveplace")
        call close_buffer(current_buffer(), st)
        call set_buffer(old, st)
    end subroutine test_to_table_directory

    subroutine test_to_table_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: temporary, tmpfile

        call start(.true.)
        call create_temporary_file("saveplace-test", temporary, st)
        call file_truename(temporary, tmpfile, st)
        old = current_buffer()
        call show_alone()
        call find_file(tmpfile, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! (Must insert something.)
        call insert("abc", st)
        call save_place_store()
        call check(error, size(save_places) == 1, "one place")
        if (allocated(error)) return
        call check(error, save_places(1)%file, tmpfile)
        if (allocated(error)) return
        call check(error, save_places(1)%position == 4 .and. .not. allocated(save_places(1)%dired_filename), &
                   "at 4")
        h = current_buffer()
        call restore_buffer_modified(.false.)
        call close_buffer(h, st)
        call set_buffer(old, st)
        call delete_file(tmpfile, st)
    end subroutine test_to_table_file

    subroutine test_forget_unreadable(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: tmpfile

        call start(.true.)
        call create_temporary_file("saveplace-test", tmpfile, st)
        call set_table([character(len=len(tmpfile) + 32) :: "/this/file/does/not/exist", tmpfile], [10_int64, 1917_int64])
        call save_place_forget_unreadable_files()
        call check(error, size(save_places) == 1, "one place left")
        if (allocated(error)) return
        call check(error, save_places(1)%file, tmpfile)
        if (allocated(error)) return
        call check(error, save_places(1)%position == 1917, "1917")
        call delete_file(tmpfile, st)
    end subroutine test_forget_unreadable

    subroutine test_table_to_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: old, h
        character(len=:), allocatable :: tmpfile, tmpfile2, text
        integer(int64) :: inserted

        call start(.true.)
        call create_temporary_file("saveplace-test", tmpfile, st)
        call create_temporary_file("saveplace-test", tmpfile2, st)
        save_place_file = tmpfile
        call set_table([tmpfile2], [99_int64])
        call save_places_to_file()
        if (allocated(save_places)) deallocate (save_places)
        allocate (save_places(0))
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *saveplace-test*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(tmpfile, .false., inserted, st)
        text = buffer_string()
        call close_buffer(h, st)
        call set_buffer(old, st)
        call check(error, index(text, tmpfile2) > 0, text)
        call delete_file(tmpfile, st)
        call delete_file(tmpfile2, st)
    end subroutine test_table_to_file

    subroutine test_load_table(error)
        type(error_type), allocatable, intent(out) :: error

        call start(.false.)
        save_place_file = resource_directory()//"saveplace"
        call save_place_load_places_from_file()
        call check(error, size(save_places) == 2, "two places")
        if (allocated(error)) return
        call check(error, save_places(1)%file, "/home/skangas/.emacs.d/cache/recentf")
        if (allocated(error)) return
        call check(error, save_places(1)%position == 1306 .and. .not. allocated(save_places(1)%dired_filename), &
                   "1306")
        if (allocated(error)) return
        call check(error, save_places(2)%file, "/home/skangas/wip/emacs/")
        if (allocated(error)) return
        call check(error, allocated(save_places(2)%dired_filename), "a Dired place")
        if (allocated(error)) return
        call check(error, save_places(2)%dired_filename, "/home/skangas/wip/emacs/COPYING")
    end subroutine test_load_table
end module test_saveplace_mod
