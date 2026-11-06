! test_fortmacs_diff_multiple_mod.f90 -- tests for fortmacs_diff_multiple_mod and Ediff's
! directory jobs.
! Own tests; upstream has no tests of ediff-mult.el.
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

!> Session groups: the intersection of two directories, the group
!> buffer and its records, the keys that move, mark and hide sessions and
!> mark equal files, starting a session from the group and coming back
!> to it, the registry, the directory differences and quitting the group.
module test_fortmacs_diff_multiple_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type, same_handle
    use buffer_mod, only: buffers, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, get_buffer, &
                          is_buffer_live, restore_buffer_modified
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point_minimum, goto_character
    use keyboard_mod, only: discard_input, keyboard_store_bytes, command_loop
    use bindings_mod, only: initialize_bindings
    use file_input_output_mod, only: initialize_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, &
                                     create_directory_internal, write_region, file_exists
    use frame_mod, only: selected_frame
    use window_mod, only: set_window_buffer
    use files_mod, only: delete_directory
    use commands_mod, only: commands
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, fortmacs_diff_session_index, abbreviate_fortmacs_diff_job_name
    use fortmacs_diff_utilities_mod, only: quit_fortmacs_diff_session
    use fortmacs_diff_mod, only: fortmacs_diff_directories
    use fortmacs_diff_multiple_mod, only: fortmacs_diff_session_group_type, fortmacs_diff_session_groups, &
                                          fortmacs_diff_group_index, intersect_fortmacs_diff_directories, &
                                          fortmacs_diff_session_registry, fortmacs_diff_registry_buffer, &
                                          show_fortmacs_diff_registry, show_fortmacs_diff_meta_buffer, &
                                          FORTMACS_DIFF_SESSION_STATUS_MARKED, FORTMACS_DIFF_SESSION_STATUS_HIDE, &
                                          FORTMACS_DIFF_SESSION_STATUS_HIDDEN, FORTMACS_DIFF_MEMBERSHIP_CODE1, &
                                          FORTMACS_DIFF_MEMBERSHIP_CODE2, fortmacs_diff_filtering_regular_expression_history, &
                                          fortmacs_diff_default_filtering_regular_expression, &
                                          get_fortmacs_diff_default_filtering_regular_expression, &
                                          get_latest_fortmacs_diff_filtering_regular_expression, update_fortmacs_diff_registry, &
                                          update_fortmacs_diff_meta_buffer
    implicit none(type, external)
    private

    public :: collect_fortmacs_diff_multiple_mod

    character, parameter :: LF = achar(10)

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdirectory = "build/test-ediff-multiple"

contains

    !> Collect all exported unit tests.
    subroutine collect_fortmacs_diff_multiple_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ediff-intersect-directories", test_intersect), &
                    new_unittest("ediff-directories", test_directories), &
                    new_unittest("ediff-meta-marks", test_marks), &
                    new_unittest("ediff-meta-mark-equal-files", test_mark_equal_files), &
                    new_unittest("ediff-filegroup-action", test_filegroup_action), &
                    new_unittest("ediff-quit-meta-buffer", test_quit_meta_buffer), &
                    new_unittest("ediff-default-filtering-regexp", test_default_filtering_regular_expression), &
                    new_unittest("ediff-directory-diff-copy", test_directory_diff_copy) &
                    ]
    end subroutine collect_fortmacs_diff_multiple_mod

    !> Whether PROGRAM is on PATH, skipping the test if not.
    logical function has_program(program, error)
        character(len=*), intent(in) :: program
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path, directory
        integer :: n, st, k

        has_program = .false.
        call get_environment_variable("PATH", length=n, status=st)

        if (st == 0) then
            allocate (character(len=n) :: path)
            call get_environment_variable("PATH", path)

            do while (len(path) > 0 .and. .not. has_program)
                k = index(path, ":")
                if (k == 0) k = len(path) + 1
                directory = path(1:k - 1)

                if (k > len(path)) then
                    path = ""
                else
                    path = path(k + 1:)
                end if

                if (len(directory) > 0) inquire (file=directory//"/"//program, exist=has_program)
            end do
        end if

        if (.not. has_program) call skip_test(error, "no "//program)
    end function has_program

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

    !> Create FILE holding TEXT.
    subroutine make_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(" *ediff-test-file*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call write_region(file, .false., st)
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine make_file

    !> *scratch* current, shown alone, and directories A and B under the
    !> test directory DIRECTORY: both have f1 (equal), f2 (different) and the
    !> subdirectory sub; only A has only_a, only B has only_b.
    subroutine setup(directory)
        character(len=:), allocatable, intent(out) :: directory
        type(handle_type) :: buffer
        type(status_type) :: st

        call initialize_bindings()
        call initialize_fileio()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
        directory = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdirectory)
        if (is_file_directory(directory)) call delete_directory(directory, st, .true.)
        call create_directory_internal(directory, st)
        call create_directory_internal(directory//"a", st)
        call create_directory_internal(directory//"b", st)
        call create_directory_internal(directory//"a/sub", st)
        call create_directory_internal(directory//"b/sub", st)
        call make_file(directory//"a/f1", "same"//LF)
        call make_file(directory//"b/f1", "same"//LF)
        call make_file(directory//"a/f2", "one"//LF//"two"//LF)
        call make_file(directory//"b/f2", "one"//LF//"three"//LF)
        call make_file(directory//"a/only_a", "a"//LF)
        call make_file(directory//"b/only_b", "b"//LF)
    end subroutine setup

    !> Close the group buffers and the registry, the buffers of the test
    !> files and the test directory DIRECTORY.
    subroutine teardown(directory)
        character(len=*), intent(in) :: directory
        type(status_type) :: st
        integer :: k

        if (allocated(fortmacs_diff_session_groups)) then
            do k = 1, size(fortmacs_diff_session_groups)
                if (is_buffer_live(fortmacs_diff_session_groups(k)%directory_diffs_buffer)) then
                    call close_buffer(fortmacs_diff_session_groups(k)%directory_diffs_buffer, st)
                end if

                if (is_buffer_live(fortmacs_diff_session_groups(k)%buffer)) call close_buffer(fortmacs_diff_session_groups( &
                                                                                              k)%buffer, st)
            end do
        end if

        call discard(get_buffer("f1"))
        call discard(get_buffer("f2"))
        call discard(get_buffer("f2<2>"))
        call delete_directory(directory, st, .true.)
    end subroutine teardown

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

    !> Start M-x fortmacs_diff_directories on A and B; GROUP is its buffer's
    !> group, which is current.
    subroutine start_group(directory, k, status)
        character(len=*), intent(in) :: directory
        integer, intent(out) :: k
        type(status_type), intent(out) :: status

        k = 0
        call fortmacs_diff_directories(directory//"a", directory//"b", "", status)
        if (.not. status%ok()) return
        k = fortmacs_diff_group_index(get_buffer("*Ediff Session Group Panel*"))
        if (k > 0) call set_buffer(fortmacs_diff_session_groups(k)%buffer, status)
    end subroutine start_group

    !> The files both directories have are the sessions, sorted, a
    !> directory with a slash; the others are the directory differences
    !> with their membership codes.
    subroutine test_intersect(error)
        type(error_type), allocatable, intent(out) :: error
        type(fortmacs_diff_session_group_type) :: group
        type(status_type) :: st
        character(len=:), allocatable :: directory

        call setup(directory)
        call intersect_fortmacs_diff_directories(commands%fortmacs_diff_directories, "", directory//"a", directory//"b", "", "", &
                                                 group, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, size(group%sessions), 3)
            if (allocated(error)) exit body
            call check(error, group%sessions(1)%files(1)%name, directory//"a/f1")
            if (allocated(error)) exit body
            call check(error, group%sessions(2)%files(2)%name, directory//"b/f2")
            if (allocated(error)) exit body
            call check(error, group%sessions(3)%files(1)%name, directory//"a/sub/")
            if (allocated(error)) exit body
            call check(error,.not. allocated(group%sessions(1)%files(3)%name), "no file C")
            if (allocated(error)) exit body
            call check(error, size(group%directory_differences), 2)
            if (allocated(error)) exit body
            call check(error, group%directory_differences(1)%file, "only_a")
            if (allocated(error)) exit body
            call check(error, group%directory_differences(1)%membership, FORTMACS_DIFF_MEMBERSHIP_CODE1)
            if (allocated(error)) exit body
            call check(error, group%directory_differences(2)%membership, FORTMACS_DIFF_MEMBERSHIP_CODE2)
            if (allocated(error)) exit body
            ! A filter keeps the matching names only.
            call intersect_fortmacs_diff_directories(commands%fortmacs_diff_directories, "^f", directory//"a", directory//"b", "", &
                                                     "", group, st)
            call check(error, size(group%sessions), 2)
        end block body

        call teardown(directory)
    end subroutine test_intersect

    !> M-x fortmacs_diff_directories makes the group buffer, lists the sessions,
    !> registers the group and draws the directory differences.
    subroutine test_directories(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: directory, text
        integer :: k

        call setup(directory)
        call start_group(directory, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, k > 0, "a group")
            if (allocated(error)) exit body
            call check(error, associated(fortmacs_diff_session_groups(k)%job_name, commands%fortmacs_diff_directories), "job")
            if (allocated(error)) exit body
            call check(error, abbreviate_fortmacs_diff_job_name(fortmacs_diff_session_groups(k)%job_name), &
                       "Compare two directories")
            if (allocated(error)) exit body
            text = text_of(fortmacs_diff_session_groups(k)%buffer)
            call check(error, index(text, "Ediff Session Group Panel: Compare two directories") == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "  Session 3:"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "*** Directory A: ") > 0, text)
            if (allocated(error)) exit body
            call check(error, trim(buffers(fortmacs_diff_session_groups(k)%buffer%index)%mode_name), "MetaEdiff")
            if (allocated(error)) exit body
            call check(error, any(same_handle(fortmacs_diff_session_registry, fortmacs_diff_session_groups(k)%buffer)), &
                       "registered")
            if (allocated(error)) exit body
            call check(error, index(text_of(fortmacs_diff_session_groups(k)%directory_diffs_buffer), "only_a") > 0, &
                       "directory diffs")
            if (allocated(error)) exit body
            ! The registry lists the group.
            call show_fortmacs_diff_registry(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, index(text_of(fortmacs_diff_registry_buffer), "*group*") > 0, text_of(fortmacs_diff_registry_buffer))
            if (allocated(error)) exit body
            ! ? shows the verbose help.
            call show_fortmacs_diff_meta_buffer(st, fortmacs_diff_session_groups(k)%buffer)
            call keyboard_store_bytes("?")
            call command_loop(st)
            call check(error, index(text_of(fortmacs_diff_session_groups(k)%buffer), "Useful commands") > 0, "verbose help")
        end block body

        call teardown(directory)
    end subroutine test_directories

    !> n moves to a session, m marks it for an operation, h for hiding,
    !> and x hides the sessions marked for hiding.
    subroutine test_marks(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k

        call setup(directory)
        call start_group(directory, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call goto_character(point_minimum())
            call keyboard_store_bytes("nm")
            call command_loop(st)
            call check(error, fortmacs_diff_session_groups(k)%sessions(1)%status, FORTMACS_DIFF_SESSION_STATUS_MARKED)
            if (allocated(error)) exit body
            ! m moved to session 2; h marks it for hiding, x hides it.
            call keyboard_store_bytes("hx")
            call command_loop(st)
            call check(error, fortmacs_diff_session_groups(k)%sessions(2)%status, FORTMACS_DIFF_SESSION_STATUS_HIDDEN)
            if (allocated(error)) exit body
            call check(error, index(text_of(fortmacs_diff_session_groups(k)%buffer), "Session 2:") == 0, "session 2 hidden")
            if (allocated(error)) exit body
            ! um unmarks session 1.
            call keyboard_store_bytes("um")
            call command_loop(st)
            call check(error, fortmacs_diff_session_groups(k)%sessions(1)%status, " ")
        end block body

        call teardown(directory)
    end subroutine test_marks

    !> == shows which files are equal, =h also marks their sessions for
    !> hiding.
    subroutine test_mark_equal_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k

        if (.not. has_program("cmp", error)) return
        call setup(directory)
        call start_group(directory, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            ! Comparing the subdirectories asks whether to recurse: n.
            call keyboard_store_bytes("=hn")
            call command_loop(st)
            call check(error, fortmacs_diff_session_groups(k)%sessions(1)%files(1)%is_equal, "f1 equal")
            if (allocated(error)) exit body
            call check(error,.not. fortmacs_diff_session_groups(k)%sessions(2)%files(1)%is_equal, "f2 different")
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_session_groups(k)%sessions(1)%status, FORTMACS_DIFF_SESSION_STATUS_HIDE)
            if (allocated(error)) exit body
            call check(error, index(text_of(fortmacs_diff_session_groups(k)%buffer), "=  ") > 0, "the = marker")
        end block body

        call teardown(directory)
    end subroutine test_mark_equal_files

    !> RET on a session starts Ediff on its files as a session of the
    !> group; quitting it brings the group back, the session finished.
    subroutine test_filegroup_action(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: control, meta
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k, s

        if (.not. has_program("diff", error)) return
        call setup(directory)
        call start_group(directory, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            meta = fortmacs_diff_session_groups(k)%buffer
            call goto_character(point_minimum())
            call keyboard_store_bytes("nn"//achar(13))
            call command_loop(st)
            control = fortmacs_diff_session_groups(k)%sessions(2)%buffer
            call check(error, is_buffer_live(control), "session 2 started")
            if (allocated(error)) exit body
            s = fortmacs_diff_session_index(control)
            call check(error, s > 0, "a session")
            if (allocated(error)) exit body
            call check(error, same_handle(fortmacs_diff_sessions(s)%meta_buffer, meta), "its group")
            if (allocated(error)) exit body
            call check(error, fortmacs_diff_sessions(s)%meta_session_number, 2)
            if (allocated(error)) exit body
            call check(error, any(same_handle(fortmacs_diff_session_registry, control)), "registered")
            if (allocated(error)) exit body
            call check(error, index(text_of(meta), "+   Session 2:") > 0, text_of(meta))
            if (allocated(error)) exit body
            call set_buffer(control, st)
            call quit_fortmacs_diff_session(s, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. is_buffer_live(control), "control buffer closed")
            if (allocated(error)) exit body
            call check(error, same_handle(current_buffer(), meta), "back in the group")
            if (allocated(error)) exit body
            call check(error, index(text_of(meta), "-   Session 2:") > 0, text_of(meta))
        end block body

        call teardown(directory)
    end subroutine test_filegroup_action

    !> q in a group with no active session asks and closes it, then shows
    !> the registry.
    subroutine test_quit_meta_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: meta
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k

        call setup(directory)
        call start_group(directory, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            meta = fortmacs_diff_session_groups(k)%buffer
            call keyboard_store_bytes("qy")
            call command_loop(st)
            call check(error,.not. is_buffer_live(meta), "group closed")
            if (allocated(error)) exit body
            call check(error,.not. any(same_handle(fortmacs_diff_session_registry, meta)), "unregistered")
            if (allocated(error)) exit body
            call check(error, same_handle(current_buffer(), fortmacs_diff_registry_buffer), "the registry")
        end block body

        call teardown(directory)
    end subroutine test_quit_meta_buffer

    !> The default filter is a regular expression set with = or a
    !> procedure set with =>, such as the last filter typed.
    subroutine test_default_filtering_regular_expression(error)
        type(error_type), allocatable, intent(out) :: error

        call fortmacs_diff_filtering_regular_expression_history%append("^f")
        fortmacs_diff_default_filtering_regular_expression%procedure => get_latest_fortmacs_diff_filtering_regular_expression
        call check(error, get_fortmacs_diff_default_filtering_regular_expression(), "^f")
        if (allocated(error)) return
        fortmacs_diff_default_filtering_regular_expression = "\.f90$"
        call check(error,.not. associated(fortmacs_diff_default_filtering_regular_expression%procedure), "= clears the procedure")
        if (allocated(error)) return
        call check(error, get_fortmacs_diff_default_filtering_regular_expression(), "\.f90$")
        call fortmacs_diff_default_filtering_regular_expression%clear()
    end subroutine test_default_filtering_regular_expression

    !> C in the directory difference buffer copies a file to the
    !> directory that lacks it, which makes it a session of the group; q
    !> then closes the buffer.  Updating the registry without redrawing it
    !> is an error, as upstream.
    subroutine test_directory_diff_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: diffs
        type(status_type) :: st
        character(len=:), allocatable :: directory
        integer :: k, at

        call setup(directory)
        call start_group(directory, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            diffs = fortmacs_diff_session_groups(k)%directory_diffs_buffer
            call set_window_buffer(selected_frame%selected_window, diffs, st)
            call set_buffer(diffs, st)
            at = index(text_of(diffs), "only_a")
            call goto_character(int(at, int64))
            call keyboard_store_bytes("Cy")
            call command_loop(st)
            call check(error, file_exists(directory//"b/only_a"), "only_a copied to B")
            if (allocated(error)) exit body
            call check(error, size(fortmacs_diff_session_groups(k)%sessions), 4)
            if (allocated(error)) exit body
            call check(error, size(fortmacs_diff_session_groups(k)%directory_differences), 1)
            if (allocated(error)) exit body
            call check(error, is_buffer_live(diffs), "still shown")
            if (allocated(error)) exit body
            call keyboard_store_bytes("q")
            call command_loop(st)
            call check(error,.not. is_buffer_live(diffs), "q closes it after C")
            if (allocated(error)) exit body
            call update_fortmacs_diff_registry(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call update_fortmacs_diff_meta_buffer(fortmacs_diff_registry_buffer, st)
            call check(error,.not. st%ok(), "the registry is no group")
        end block body

        call teardown(directory)
    end subroutine test_directory_diff_copy
end module test_fortmacs_diff_multiple_mod
