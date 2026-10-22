! main.f90 -- test-drive runner for all Fortmacs test suites.
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

!> Runs every suite, or one suite (and optionally one test) by name:
!>   fpm test -- [suite [test]]
!> New suites are registered in the `testsuites` array below.
program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, run_selected, new_testsuite, &
                         testsuite_type, select_suite
    use test_version_fm, only: collect_version_fm
    use test_error_fm, only: collect_error_fm
    use test_handle_fm, only: collect_handle_fm
    use test_character_fm, only: collect_character_fm
    use test_buffer_fm, only: collect_buffer_fm
    use test_insdel_fm, only: collect_insdel_fm
    use test_marker_fm, only: collect_marker_fm
    use test_sysdep_fm, only: collect_sysdep_fm
    use test_editfns_fm, only: collect_editfns_fm
    use test_cmds_fm, only: collect_cmds_fm
    use test_values_fm, only: collect_values_fm
    use test_textprop_fm, only: collect_textprop_fm
    use test_keymap_fm, only: collect_keymap_fm
    use test_keyboard_fm, only: collect_keyboard_fm
    use test_bindings_fm, only: collect_bindings_fm
    use test_term_xterm_fm, only: collect_term_xterm_fm
    use test_terminfo_fm, only: collect_terminfo_fm
    use test_term_fm, only: collect_term_fm
    use test_dispnew_fm, only: collect_dispnew_fm
    use test_xdisp_fm, only: collect_xdisp_fm
    use test_fileio_fm, only: collect_fileio_fm
    use test_files_fm, only: collect_files_fm
    use test_minibuf_fm, only: collect_minibuf_fm
    use test_indent_fm, only: collect_indent_fm
    use test_emacs_fm, only: collect_emacs_fm
    use test_callproc_fm, only: collect_callproc_fm
    use test_config_fm, only: collect_config_fm
    implicit none(type, external)

    ! test-drive's interfaces take default integers.
    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    testsuites = [ &
                 new_testsuite("version_fm", collect_version_fm), &
                 new_testsuite("error_fm", collect_error_fm), &
                 new_testsuite("handle_fm", collect_handle_fm), &
                 new_testsuite("character_fm", collect_character_fm), &
                 new_testsuite("buffer_fm", collect_buffer_fm), &
                 new_testsuite("insdel_fm", collect_insdel_fm), &
                 new_testsuite("marker_fm", collect_marker_fm), &
                 new_testsuite("sysdep_fm", collect_sysdep_fm), &
                 new_testsuite("editfns_fm", collect_editfns_fm), &
                 new_testsuite("cmds_fm", collect_cmds_fm), &
                 new_testsuite("values_fm", collect_values_fm), &
                 new_testsuite("textprop_fm", collect_textprop_fm), &
                 new_testsuite("keymap_fm", collect_keymap_fm), &
                 new_testsuite("keyboard_fm", collect_keyboard_fm), &
                 new_testsuite("bindings_fm", collect_bindings_fm), &
                 new_testsuite("term_xterm_fm", collect_term_xterm_fm), &
                 new_testsuite("terminfo_fm", collect_terminfo_fm), &
                 new_testsuite("term_fm", collect_term_fm), &
                 new_testsuite("dispnew_fm", collect_dispnew_fm), &
                 new_testsuite("xdisp_fm", collect_xdisp_fm), &
                 new_testsuite("fileio_fm", collect_fileio_fm), &
                 new_testsuite("files_fm", collect_files_fm), &
                 new_testsuite("minibuf_fm", collect_minibuf_fm), &
                 new_testsuite("indent_fm", collect_indent_fm), &
                 new_testsuite("emacs_fm", collect_emacs_fm), &
                 new_testsuite("callproc_fm", collect_callproc_fm), &
                 new_testsuite("config_fm", collect_config_fm) &
                 ]

    call get_argument(1, suite_name)
    call get_argument(2, test_name)

    if (allocated(suite_name)) then
        is = select_suite(testsuites, suite_name)

        if (is > 0 .and. is <= size(testsuites)) then
            if (allocated(test_name)) then
                write (error_unit, fmt) "Suite:", testsuites(is)%name
                call run_selected(testsuites(is)%collect, test_name, &
                                  error_unit, stat)
                if (stat < 0) error stop 1
            else
                write (error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat)
            end if
        else
            write (error_unit, fmt) "Available testsuites"

            do is = 1, size(testsuites)
                write (error_unit, fmt) "-", testsuites(is)%name
            end do

            error stop 1
        end if
    else
        do is = 1, size(testsuites)
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
        end do
    end if

    if (stat > 0) then
        write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
        error stop 1
    end if

contains

    !> Fetch command-line argument `idx`, leaving `arg` unallocated if absent.
    subroutine get_argument(idx, arg)
        integer, intent(in) :: idx
        character(len=:), allocatable, intent(out) :: arg
        integer :: length, astat

        call get_command_argument(idx, length=length, status=astat)
        if (astat /= 0 .or. length == 0) return
        allocate (character(len=length) :: arg)
        call get_command_argument(idx, arg, status=astat)
        if (astat /= 0) deallocate (arg)
    end subroutine get_argument

end program tester
