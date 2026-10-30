! test_info_look_fm.f90 -- tests for info_look_fm.
! Own tests; upstream has no info-look tests.
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

!> Tests for info_look_fm (no upstream ERT tests): looking up symbols in
!> the index of test/info-resources/test.info, guessing the item at
!> point, and `info_display_manual'.
module test_info_look_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: current_buffer, set_buffer, kill_buffer, get_buffer, get_buffer_create
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char
    use fileio_fm, only: expand_file_name, emacs_wd, init_fileio, file_name_as_directory
    use window_fm, only: window_configuration_type, current_window_configuration, set_window_configuration, &
                         switch_to_buffer
    use bindings_fm, only: init_bindings
    use info_fm, only: info_directory_list, info_current_node, info_display_manual
    use info_look_fm, only: info_lookup_add_help, info_lookup_doc_specs_type, info_lookup_symbol, &
                            info_lookup_reset, info_lookup_guess_default
    implicit none(type, external)
    private

    public :: collect_info_look_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_info_look_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("info-lookup-symbol", test_lookup_symbol), &
                    new_unittest("info-lookup-guess-default", test_guess_default), &
                    new_unittest("info-display-manual", test_display_manual) &
                    ]
    end subroutine collect_info_look_fm

    !> Search Info files in test/info-resources only, with help for the
    !> mode test_look_mode from the index of test.info.
    subroutine setup()
        character(len=:), allocatable :: dir
        type(info_lookup_doc_specs_type) :: specs
        type(status_type) :: st
        type(handle_type) :: b

        call init_bindings()
        call init_fileio()
        call expand_file_name("test/info-resources", dir, st, emacs_wd)
        dir = file_name_as_directory(dir)
        if (allocated(info_directory_list)) deallocate (info_directory_list)
        ! (dir1 has a `dir' file for the directory node.)
        allocate (character(len=len(dir) + 5) :: info_directory_list(2))
        info_directory_list(1) = dir
        info_directory_list(2) = dir//"dir1/"
        call specs%add("(test)Index")
        call info_lookup_add_help(mode="test_look_mode", regexp="[a-z]+", doc_spec=specs)
        call info_lookup_reset(st)
        ! The buffer looked up from, in the selected window; no Info
        ! buffer left by an earlier lookup.
        call switch_to_buffer(" info-look from", st)
        b = get_buffer(" temp-info-look")
        if (b%index /= 0) call kill_buffer(b, st)
    end subroutine setup

    !> Kill *info* and go back to OLD and the window configuration CONFIG.
    subroutine teardown(old, config)
        type(handle_type), intent(in) :: old
        type(window_configuration_type), intent(in) :: config
        type(handle_type) :: b
        type(status_type) :: st

        call set_window_configuration(config)
        b = get_buffer("*info*")
        if (b%index /= 0) call kill_buffer(b, st)
        call set_buffer(old, st)
    end subroutine teardown

    !> The current node of *info*, "" for none.
    function info_node() result(n)
        character(len=:), allocatable :: n
        type(handle_type) :: b, old
        type(status_type) :: st

        n = ""
        b = get_buffer("*info*")
        if (b%index == 0) return
        old = current_buffer()
        call set_buffer(b, st)
        n = info_current_node()
        if (.not. allocated(n)) n = ""
        call set_buffer(old, st)
    end function info_node

    subroutine test_lookup_symbol(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, now
        type(window_configuration_type) :: config
        type(status_type) :: st

        call setup()
        old = current_buffer()
        call current_window_configuration(config)
        call info_lookup_symbol("section", st, mode="test_look_mode")
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, info_node(), "Section One")
        ! The window of the buffer looked up from is selected again.
        now = current_buffer()
        if (.not. allocated(error)) call check(error, now%index == old%index, "back in the buffer")

        ! Only the first word of "first chapter" is an item.
        if (.not. allocated(error)) then
            call info_lookup_symbol("first", st, mode="test_look_mode")
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) call check(error, info_node(), "First Chapter")

        if (.not. allocated(error)) then
            call info_lookup_symbol("nothing", st, mode="test_look_mode")
            call check(error, format_error(st), "Not documented as a symbol: nothing")
        end if

        call teardown(old, config)
    end subroutine test_lookup_symbol

    subroutine test_guess_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, b
        type(status_type) :: st
        character(len=:), allocatable :: guess
        logical :: found

        call setup()
        old = current_buffer()
        call get_buffer_create(" info-look test", b, st)
        call set_buffer(b, st)
        call insert("see the section here", st)
        call goto_char(11_int64)
        call info_lookup_guess_default("symbol", "test_look_mode", guess, found)
        call check(error, found, "found a guess")
        if (.not. allocated(error)) call check(error, guess, "section")
        call set_buffer(old, st)
        call kill_buffer(b, st)
    end subroutine test_guess_default

    subroutine test_display_manual(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, b, now
        type(window_configuration_type) :: config
        type(status_type) :: st

        call setup()
        old = current_buffer()
        call current_window_configuration(config)
        ! A new Info buffer for the manual.
        call info_display_manual("test", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, info_current_node(), "Top")

        if (.not. allocated(error)) then
            b = current_buffer()
            call set_buffer(old, st)
            ! The existing buffer is shown again.
            call info_display_manual("test", st)
            now = current_buffer()
            call check(error, now%index == b%index, "the same buffer")
            call kill_buffer(b, st)
        end if

        call teardown(old, config)
    end subroutine test_display_manual
end module test_info_look_fm
