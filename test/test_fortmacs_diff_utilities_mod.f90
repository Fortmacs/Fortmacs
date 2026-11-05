! test_fortmacs_diff_utilities_mod.f90 -- tests for fortmacs_diff_utilities_mod and fortmacs_diff_mod.
! Own tests; upstream has no tests of ediff-util.el or ediff.el.
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

!> An Ediff session of two buffers: the control buffer and its windows,
!> moving between the differences, copying a difference and restoring
!> it, the help message, comparing two files, and quitting.
module test_fortmacs_diff_utilities_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, &
                          get_buffer, is_buffer_live, restore_buffer_modified
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string
    use keyboard_mod, only: discard_input, keyboard_store_bytes, command_loop
    use execute_interactively_mod, only: prefix_argument_type, PREFIX_NONE, PREFIX_RAW
    use bindings_mod, only: initialize_bindings
    use frame_mod, only: selected_frame
    use window_mod, only: window_buffer, is_window_live
    use file_input_output_mod, only: initialize_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, &
                                     create_directory_internal, write_region
    use files_mod, only: delete_directory, find_file_noselect
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, fortmacs_diff_session_index, &
                                          fortmacs_diff_fortmacs_help_in_control_buffer, FORTMACS_DIFF_BUFFER_TYPE_A, &
                                          FORTMACS_DIFF_BUFFER_TYPE_B
    use fortmacs_diff_utilities_mod, only: next_fortmacs_diff_difference, previous_fortmacs_diff_difference, &
                                           jump_to_fortmacs_diff_difference, copy_fortmacs_diff_diff_to_diff, &
                                           restore_fortmacs_diff_diff, quit_fortmacs_diff_session, toggle_fortmacs_diff_help, &
                                           fortmacs_diff_unique_buffer_name
    use fortmacs_diff_mod, only: fortmacs_diff_buffers, fortmacs_diff_files
    use commands_mod, only: commands
    implicit none(type, external)
    private

    public :: collect_fortmacs_diff_utilities_mod

    character, parameter :: LF = achar(10)

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdirectory = "build/test-ediff"

contains

    !> Collect all exported unit tests.
    subroutine collect_fortmacs_diff_utilities_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ediff-unique-buffer-name", test_unique_buffer_name), &
                    new_unittest("ediff-setup-windows", test_setup_windows), &
                    new_unittest("ediff-navigation", test_navigation), &
                    new_unittest("ediff-copy-and-restore", test_copy_and_restore), &
                    new_unittest("ediff-copy-all", test_copy_all), &
                    new_unittest("ediff-keys", test_keys), &
                    new_unittest("ediff-help-key", test_help_key), &
                    new_unittest("ediff-toggle-help", test_toggle_help), &
                    new_unittest("ediff-files", test_files), &
                    new_unittest("ediff-quit", test_quit) &
                    ]
    end subroutine collect_fortmacs_diff_utilities_mod

    !> Whether diff is on PATH, skipping the test if not.
    logical function has_diff(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path, directory
        integer :: n, st, k

        has_diff = .false.
        call get_environment_variable("PATH", length=n, status=st)

        if (st == 0) then
            allocate (character(len=n) :: path)
            call get_environment_variable("PATH", path)

            do while (len(path) > 0 .and. .not. has_diff)
                k = index(path, ":")
                if (k == 0) k = len(path) + 1
                directory = path(1:k - 1)

                if (k > len(path)) then
                    path = ""
                else
                    path = path(k + 1:)
                end if

                if (len(directory) > 0) inquire (file=directory//"/diff", exist=has_diff)
            end do
        end if

        if (.not. has_diff) call skip_test(error, "no diff")
    end function has_diff

    !> A fresh buffer NAME holding TEXT.
    subroutine temporary_buffer(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call set_buffer(old, st)
    end subroutine temporary_buffer

    !> The text of buffer H.
    function text_of(h) result(s)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: s
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(h, st)
        s = buffer_string()
        call set_buffer(old, st)
    end function text_of

    !> *scratch* current, shown alone.
    subroutine setup()
        type(handle_type) :: buffer
        type(status_type) :: st

        call initialize_bindings()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
    end subroutine setup

    !> Compare two buffers holding three differences (lines 2, 4 and 6);
    !> A and B are the buffers, the control buffer is current and K is
    !> its session.
    subroutine start_session(a, b, k, status)
        type(handle_type), intent(out) :: a, b
        integer, intent(out) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: control

        call setup()
        call temporary_buffer("ediff-a", "1"//LF//"a"//LF//"3"//LF//"b"//LF//"5"//LF//"c"//LF//"7"//LF, a)
        call temporary_buffer("ediff-b", "1"//LF//"A"//LF//"3"//LF//"B"//LF//"5"//LF//"C"//LF//"7"//LF, b)
        call fortmacs_diff_buffers(a, b, status)
        k = 0
        if (.not. status%ok()) return
        control = get_buffer("*Ediff Control Panel*")
        call set_buffer(control, status)
        k = fortmacs_diff_session_index(control)
    end subroutine start_session

    !> Quit session K and close A and B.
    subroutine end_session(a, b)
        type(handle_type), intent(in) :: a, b
        type(handle_type) :: control
        type(status_type) :: st
        integer :: k

        control = get_buffer("*Ediff Control Panel*")

        if (is_buffer_live(control)) then
            call set_buffer(control, st)
            k = fortmacs_diff_session_index(control)
            if (k > 0) call quit_fortmacs_diff_session(k, .false., st)
        end if

        call discard(a)
        call discard(b)
    end subroutine end_session

    !> Close buffer H without saving it.
    subroutine discard(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: old
        type(status_type) :: st

        if (.not. is_buffer_live(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        call restore_buffer_modified(.false.)
        call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine discard

    !> Names are made unique by <N> before the suffix.
    subroutine test_unique_buffer_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call setup()
        call check(error, fortmacs_diff_unique_buffer_name("*ediff-test", "*"), "*ediff-test*")
        if (allocated(error)) return
        call get_or_make_buffer("*ediff-test*", h, st)
        call check(error, fortmacs_diff_unique_buffer_name("*ediff-test", "*"), "*ediff-test<2>*")
        call close_buffer(h, st)
    end subroutine test_unique_buffer_name

    !> A and B side by side above the control window, which is selected
    !> and shows the brief help.
    subroutine test_setup_windows(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, shown
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, is_window_live(fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_A)), "window A")
            if (allocated(error)) exit body
            shown = window_buffer(fortmacs_diff_sessions(k)%windows(FORTMACS_DIFF_BUFFER_TYPE_B), st)
            call check(error, shown%index, b%index)
            if (allocated(error)) exit body
            call check(error, selected_frame%selected_window%index, fortmacs_diff_sessions(k)%control_window%index)
            if (allocated(error)) exit body
            call check(error, index(text_of(current_buffer()), "Type ? for help") > 0, text_of(current_buffer()))
            if (allocated(error)) exit body
            ! The job name is the command that started the session.
            call check(error, associated(fortmacs_diff_sessions(k)%job_name, commands%fortmacs_diff_buffers), &
                       "job fortmacs_diff_buffers")
            if (allocated(error)) exit body
            call check(error,.not. fortmacs_diff_sessions(k)%three_way_comparison_job, "a 2-way job")
        end block body

        call end_session(a, b)
    end subroutine test_setup_windows

    !> n, p and j move between the differences; at the end, n is an error.
    subroutine test_navigation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_sessions(k)%current_difference, -1)
            if (allocated(error)) exit body
            call next_fortmacs_diff_difference(st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 0)
            if (allocated(error)) exit body
            call next_fortmacs_diff_difference(st, 2_int64)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 2)
            if (allocated(error)) exit body
            call previous_fortmacs_diff_difference(st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 1)
            if (allocated(error)) exit body
            call jump_to_fortmacs_diff_difference(3_int64, st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 2)
            if (allocated(error)) exit body
            call jump_to_fortmacs_diff_difference(-1_int64, st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 2)
            if (allocated(error)) exit body
            call next_fortmacs_diff_difference(st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 3)
            if (allocated(error)) exit body
            call next_fortmacs_diff_difference(st)
            call check(error, format_error(st), "At end of the difference list")
            if (allocated(error)) exit body
            call jump_to_fortmacs_diff_difference(9_int64, st)
            call check(error,.not. st%ok(), "a bad difference number")
        end block body

        call end_session(a, b)
    end subroutine test_navigation

    !> a copies the current difference from A to B; rb restores B's.
    subroutine test_copy_and_restore(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call jump_to_fortmacs_diff_difference(2_int64, st)
            call copy_fortmacs_diff_diff_to_diff(prefix_argument_type(PREFIX_NONE, 0_int64), "ab", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(b), "1"//LF//"A"//LF//"3"//LF//"b"//LF//"5"//LF//"C"//LF//"7"//LF)
            if (allocated(error)) exit body
            ! The replaced region is kept: difference 2 (1 counted from 0) of B.
            call check(error, size(fortmacs_diff_sessions(k)%replaced_regions), 1)
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_sessions(k)%replaced_regions(1)%n, 1)
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_sessions(k)%replaced_regions(1)%text, "B"//LF)
            if (allocated(error)) exit body
            call restore_fortmacs_diff_diff(prefix_argument_type(PREFIX_NONE, 0_int64), "b", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(b), "1"//LF//"A"//LF//"3"//LF//"B"//LF//"5"//LF//"C"//LF//"7"//LF)
            if (allocated(error)) exit body
            ! Nothing is saved any more.
            call restore_fortmacs_diff_diff(prefix_argument_type(PREFIX_NONE, 0_int64), "b", st)
            call check(error, format_error(st), "Nothing saved for diff 2 in buffer B")
        end block body

        call end_session(a, b)
    end subroutine test_copy_and_restore

    !> With C-u, b copies all the differences from B to A.
    subroutine test_copy_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call copy_fortmacs_diff_diff_to_diff(prefix_argument_type(PREFIX_RAW, 4_int64), "ba", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, text_of(a), text_of(b))
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_sessions(k)%current_difference, 2)
        end block body

        call end_session(a, b)
    end subroutine test_copy_all

    !> The keys of the control buffer: n, p, a and r b.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call keyboard_store_bytes("nnp")
            call command_loop(st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 0)
            if (allocated(error)) exit body
            call keyboard_store_bytes("a")
            call command_loop(st)
            call check(error, text_of(b), "1"//LF//"a"//LF//"3"//LF//"B"//LF//"5"//LF//"C"//LF//"7"//LF)
            if (allocated(error)) exit body
            call keyboard_store_bytes("rb")
            call command_loop(st)
            call check(error, text_of(b), "1"//LF//"A"//LF//"3"//LF//"B"//LF//"5"//LF//"C"//LF//"7"//LF)
        end block body

        call end_session(a, b)
    end subroutine test_keys

    !> With `fortmacs_diff_fortmacs_help_in_control_buffer' .false., C-h moves to
    !> the previous difference.
    subroutine test_help_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        fortmacs_diff_fortmacs_help_in_control_buffer = .false.
        call start_session(a, b, k, st)
        fortmacs_diff_fortmacs_help_in_control_buffer = .true.

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call keyboard_store_bytes("nn"//achar(8))
            call command_loop(st)
            call check(error, fortmacs_diff_sessions(k)%current_difference, 0)
        end block body

        call end_session(a, b)
    end subroutine test_help_key

    !> ? shows the long help message, and ? again the brief one.
    subroutine test_toggle_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, control
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = current_buffer()
            call toggle_fortmacs_diff_help(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, index(text_of(control), "-vert/horiz split") > 0, text_of(control))
            if (allocated(error)) exit body
            call set_buffer(control, st)
            call toggle_fortmacs_diff_help(st)
            call check(error, index(text_of(control), "Type ? for help") > 0, text_of(control))
        end block body

        call end_session(a, b)
    end subroutine test_toggle_help

    !> fortmacs_diff_files visits both files; the session sees their differences.
    subroutine test_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b, control
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k

        if (.not. has_diff(error)) return
        call setup()
        call initialize_fileio()
        directory = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdirectory)
        if (is_file_directory(directory)) call delete_directory(directory, st, .true.)
        call create_directory_internal(directory, st)
        call make_file(directory//"old.txt", "one"//LF//"two"//LF)
        call make_file(directory//"new.txt", "one"//LF//"three"//LF)
        call fortmacs_diff_files(directory//"old.txt", directory//"new.txt", st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            control = get_buffer("*Ediff Control Panel*")
            k = fortmacs_diff_session_index(control)
            call check(error, k > 0, "a session")
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_sessions(k)%number_of_differences, 1)
            if (allocated(error)) exit body
            call check(error, associated(fortmacs_diff_sessions(k)%job_name, commands%fortmacs_diff_files), &
                       "job fortmacs_diff_files")
        end block body

        call find_file_noselect(directory//"old.txt", a, st)
        call find_file_noselect(directory//"new.txt", b, st)
        call end_session(a, b)
        call delete_directory(directory, st, .true.)

    contains

        ! Create FILE holding TEXT.
        subroutine make_file(file, text)
            character(len=*), intent(in) :: file, text
            type(handle_type) :: h, old
            type(status_type) :: st2

            old = current_buffer()
            call get_or_make_buffer(" *ediff-test-file*", h, st2)
            call set_buffer(h, st2)
            call erase_buffer(st2)
            call insert(text, st2)
            call write_region(file, .false., st2)
            call close_buffer(h, st2)
            call set_buffer(old, st2)
        end subroutine make_file
    end subroutine test_files

    !> Quitting closes the control buffer and keeps buffers A and B.
    subroutine test_quit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        integer :: k

        if (.not. has_diff(error)) return
        call start_session(a, b, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call next_fortmacs_diff_difference(st)
            call quit_fortmacs_diff_session(k, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. is_buffer_live(get_buffer("*Ediff Control Panel*")), "the control buffer is closed")
            if (allocated(error)) exit body
            call check(error,.not. is_buffer_live(get_buffer("*ediff-diff*")), "the diff buffer is closed")
            if (allocated(error)) exit body
            call check(error, is_buffer_live(a) .and. is_buffer_live(b), "buffers A and B are kept")
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_session_index(get_buffer("*Ediff Control Panel*")), 0)
        end block body

        call end_session(a, b)
    end subroutine test_quit
end module test_fortmacs_diff_utilities_mod
