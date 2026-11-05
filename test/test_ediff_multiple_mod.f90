! test_ediff_multiple_mod.f90 -- tests for ediff_multiple_mod and Ediff's
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
module test_ediff_multiple_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type, same_handle
    use buffer_mod, only: buffers, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, get_buffer, &
                          is_buffer_live, restore_buffer_modified
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string, point_min, goto_char
    use keyboard_mod, only: discard_input, kbd_store_bytes, command_loop
    use bindings_mod, only: init_bindings
    use fileio_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, create_directory_internal, &
                          write_region
    use files_mod, only: delete_directory
    use commands_mod, only: commands
    use ediff_init_mod, only: ediff_sessions, ediff_session_index, abbreviate_ediff_job_name
    use ediff_util_mod, only: quit_ediff_session
    use ediff_mod, only: ediff_directories
    use ediff_multiple_mod, only: ediff_session_group_type, ediff_session_groups, ediff_group_index, &
                                  intersect_ediff_directories, ediff_session_registry, ediff_registry_buffer, &
                                  show_ediff_registry, show_ediff_meta_buffer, EDIFF_SESSION_STATUS_MARKED, &
                                  EDIFF_SESSION_STATUS_HIDE, &
                                  EDIFF_SESSION_STATUS_HIDDEN, EDIFF_MEMBERSHIP_CODE1, EDIFF_MEMBERSHIP_CODE2
    implicit none(type, external)
    private

    public :: collect_ediff_multiple_mod

    character, parameter :: LF = achar(10)

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-ediff-multiple"

contains

    !> Collect all exported unit tests.
    subroutine collect_ediff_multiple_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ediff-intersect-directories", test_intersect), &
                    new_unittest("ediff-directories", test_directories), &
                    new_unittest("ediff-meta-marks", test_marks), &
                    new_unittest("ediff-meta-mark-equal-files", test_mark_equal_files), &
                    new_unittest("ediff-filegroup-action", test_filegroup_action), &
                    new_unittest("ediff-quit-meta-buffer", test_quit_meta_buffer) &
                    ]
    end subroutine collect_ediff_multiple_mod

    !> Whether PROGRAM is on PATH, skipping the test if not.
    logical function has_program(program, error)
        character(len=*), intent(in) :: program
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path, dir
        integer :: n, st, k

        has_program = .false.
        call get_environment_variable("PATH", length=n, status=st)

        if (st == 0) then
            allocate (character(len=n) :: path)
            call get_environment_variable("PATH", path)

            do while (len(path) > 0 .and. .not. has_program)
                k = index(path, ":")
                if (k == 0) k = len(path) + 1
                dir = path(1:k - 1)

                if (k > len(path)) then
                    path = ""
                else
                    path = path(k + 1:)
                end if

                if (len(dir) > 0) inquire (file=dir//"/"//program, exist=has_program)
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
    !> test directory DIR: both have f1 (equal), f2 (different) and the
    !> subdirectory sub; only A has only_a, only B has only_b.
    subroutine setup(dir)
        character(len=:), allocatable, intent(out) :: dir
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call show_alone()
        dir = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdir)
        if (is_file_directory(dir)) call delete_directory(dir, st, .true.)
        call create_directory_internal(dir, st)
        call create_directory_internal(dir//"a", st)
        call create_directory_internal(dir//"b", st)
        call create_directory_internal(dir//"a/sub", st)
        call create_directory_internal(dir//"b/sub", st)
        call make_file(dir//"a/f1", "same"//LF)
        call make_file(dir//"b/f1", "same"//LF)
        call make_file(dir//"a/f2", "one"//LF//"two"//LF)
        call make_file(dir//"b/f2", "one"//LF//"three"//LF)
        call make_file(dir//"a/only_a", "a"//LF)
        call make_file(dir//"b/only_b", "b"//LF)
    end subroutine setup

    !> Close the group buffers and the registry, the buffers of the test
    !> files and the test directory DIR.
    subroutine teardown(dir)
        character(len=*), intent(in) :: dir
        type(status_type) :: st
        integer :: k

        if (allocated(ediff_session_groups)) then
            do k = 1, size(ediff_session_groups)
                if (is_buffer_live(ediff_session_groups(k)%dir_diffs_buffer)) then
                    call close_buffer(ediff_session_groups(k)%dir_diffs_buffer, st)
                end if

                if (is_buffer_live(ediff_session_groups(k)%buffer)) call close_buffer(ediff_session_groups(k)%buffer, st)
            end do
        end if

        call discard(get_buffer("f1"))
        call discard(get_buffer("f2"))
        call discard(get_buffer("f2<2>"))
        call delete_directory(dir, st, .true.)
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

    !> Start M-x ediff_directories on A and B; GROUP is its buffer's
    !> group, which is current.
    subroutine start_group(dir, k, status)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: k
        type(status_type), intent(out) :: status

        k = 0
        call ediff_directories(dir//"a", dir//"b", "", status)
        if (.not. status%ok()) return
        k = ediff_group_index(get_buffer("*Ediff Session Group Panel*"))
        if (k > 0) call set_buffer(ediff_session_groups(k)%buffer, status)
    end subroutine start_group

    !> The files both directories have are the sessions, sorted, a
    !> directory with a slash; the others are the directory differences
    !> with their membership codes.
    subroutine test_intersect(error)
        type(error_type), allocatable, intent(out) :: error
        type(ediff_session_group_type) :: group
        type(status_type) :: st
        character(len=:), allocatable :: dir

        call setup(dir)
        call intersect_ediff_directories(commands%ediff_directories, "", dir//"a", dir//"b", "", "", group, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, size(group%sessions), 3)
            if (allocated(error)) exit body
            call check(error, group%sessions(1)%files(1)%name, dir//"a/f1")
            if (allocated(error)) exit body
            call check(error, group%sessions(2)%files(2)%name, dir//"b/f2")
            if (allocated(error)) exit body
            call check(error, group%sessions(3)%files(1)%name, dir//"a/sub/")
            if (allocated(error)) exit body
            call check(error,.not. allocated(group%sessions(1)%files(3)%name), "no file C")
            if (allocated(error)) exit body
            call check(error, size(group%dir_differences), 2)
            if (allocated(error)) exit body
            call check(error, group%dir_differences(1)%file, "only_a")
            if (allocated(error)) exit body
            call check(error, group%dir_differences(1)%membership, EDIFF_MEMBERSHIP_CODE1)
            if (allocated(error)) exit body
            call check(error, group%dir_differences(2)%membership, EDIFF_MEMBERSHIP_CODE2)
            if (allocated(error)) exit body
            ! A filter keeps the matching names only.
            call intersect_ediff_directories(commands%ediff_directories, "^f", dir//"a", dir//"b", "", "", group, st)
            call check(error, size(group%sessions), 2)
        end block body

        call teardown(dir)
    end subroutine test_intersect

    !> M-x ediff_directories makes the group buffer, lists the sessions,
    !> registers the group and draws the directory differences.
    subroutine test_directories(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: dir, text
        integer :: k

        call setup(dir)
        call start_group(dir, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, k > 0, "a group")
            if (allocated(error)) exit body
            call check(error, associated(ediff_session_groups(k)%job_name, commands%ediff_directories), "job")
            if (allocated(error)) exit body
            call check(error, abbreviate_ediff_job_name(ediff_session_groups(k)%job_name), "Compare two directories")
            if (allocated(error)) exit body
            text = text_of(ediff_session_groups(k)%buffer)
            call check(error, index(text, "Ediff Session Group Panel: Compare two directories") == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "  Session 3:"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "*** Directory A: ") > 0, text)
            if (allocated(error)) exit body
            call check(error, trim(buffers(ediff_session_groups(k)%buffer%index)%mode_name), "MetaEdiff")
            if (allocated(error)) exit body
            call check(error, any(same_handle(ediff_session_registry, ediff_session_groups(k)%buffer)), "registered")
            if (allocated(error)) exit body
            call check(error, index(text_of(ediff_session_groups(k)%dir_diffs_buffer), "only_a") > 0, "dir diffs")
            if (allocated(error)) exit body
            ! The registry lists the group.
            call show_ediff_registry(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, index(text_of(ediff_registry_buffer), "*group*") > 0, text_of(ediff_registry_buffer))
            if (allocated(error)) exit body
            ! ? shows the verbose help.
            call show_ediff_meta_buffer(st, ediff_session_groups(k)%buffer)
            call kbd_store_bytes("?")
            call command_loop(st)
            call check(error, index(text_of(ediff_session_groups(k)%buffer), "Useful commands") > 0, "verbose help")
        end block body

        call teardown(dir)
    end subroutine test_directories

    !> n moves to a session, m marks it for an operation, h for hiding,
    !> and x hides the sessions marked for hiding.
    subroutine test_marks(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: dir
        integer :: k

        call setup(dir)
        call start_group(dir, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call goto_char(point_min())
            call kbd_store_bytes("nm")
            call command_loop(st)
            call check(error, ediff_session_groups(k)%sessions(1)%status, EDIFF_SESSION_STATUS_MARKED)
            if (allocated(error)) exit body
            ! m moved to session 2; h marks it for hiding, x hides it.
            call kbd_store_bytes("hx")
            call command_loop(st)
            call check(error, ediff_session_groups(k)%sessions(2)%status, EDIFF_SESSION_STATUS_HIDDEN)
            if (allocated(error)) exit body
            call check(error, index(text_of(ediff_session_groups(k)%buffer), "Session 2:") == 0, "session 2 hidden")
            if (allocated(error)) exit body
            ! um unmarks session 1.
            call kbd_store_bytes("um")
            call command_loop(st)
            call check(error, ediff_session_groups(k)%sessions(1)%status, " ")
        end block body

        call teardown(dir)
    end subroutine test_marks

    !> == shows which files are equal, =h also marks their sessions for
    !> hiding.
    subroutine test_mark_equal_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: dir
        integer :: k

        if (.not. has_program("cmp", error)) return
        call setup(dir)
        call start_group(dir, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            ! Comparing the subdirectories asks whether to recurse: n.
            call kbd_store_bytes("=hn")
            call command_loop(st)
            call check(error, ediff_session_groups(k)%sessions(1)%files(1)%is_equal, "f1 equal")
            if (allocated(error)) exit body
            call check(error,.not. ediff_session_groups(k)%sessions(2)%files(1)%is_equal, "f2 different")
            if (allocated(error)) exit body
            call check(error, ediff_session_groups(k)%sessions(1)%status, EDIFF_SESSION_STATUS_HIDE)
            if (allocated(error)) exit body
            call check(error, index(text_of(ediff_session_groups(k)%buffer), "=  ") > 0, "the = marker")
        end block body

        call teardown(dir)
    end subroutine test_mark_equal_files

    !> RET on a session starts Ediff on its files as a session of the
    !> group; quitting it brings the group back, the session finished.
    subroutine test_filegroup_action(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: control, meta
        type(status_type) :: st
        character(len=:), allocatable :: dir
        integer :: k, s

        if (.not. has_program("diff", error)) return
        call setup(dir)
        call start_group(dir, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            meta = ediff_session_groups(k)%buffer
            call goto_char(point_min())
            call kbd_store_bytes("nn"//achar(13))
            call command_loop(st)
            control = ediff_session_groups(k)%sessions(2)%buffer
            call check(error, is_buffer_live(control), "session 2 started")
            if (allocated(error)) exit body
            s = ediff_session_index(control)
            call check(error, s > 0, "a session")
            if (allocated(error)) exit body
            call check(error, same_handle(ediff_sessions(s)%meta_buffer, meta), "its group")
            if (allocated(error)) exit body
            call check(error, ediff_sessions(s)%meta_session_number, 2)
            if (allocated(error)) exit body
            call check(error, any(same_handle(ediff_session_registry, control)), "registered")
            if (allocated(error)) exit body
            call check(error, index(text_of(meta), "+   Session 2:") > 0, text_of(meta))
            if (allocated(error)) exit body
            call set_buffer(control, st)
            call quit_ediff_session(s, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. is_buffer_live(control), "control buffer closed")
            if (allocated(error)) exit body
            call check(error, same_handle(current_buffer(), meta), "back in the group")
            if (allocated(error)) exit body
            call check(error, index(text_of(meta), "-   Session 2:") > 0, text_of(meta))
        end block body

        call teardown(dir)
    end subroutine test_filegroup_action

    !> q in a group with no active session asks and closes it, then shows
    !> the registry.
    subroutine test_quit_meta_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: meta
        type(status_type) :: st
        character(len=:), allocatable :: dir
        integer :: k

        call setup(dir)
        call start_group(dir, k, st)

        body: block
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            meta = ediff_session_groups(k)%buffer
            call kbd_store_bytes("qy")
            call command_loop(st)
            call check(error,.not. is_buffer_live(meta), "group closed")
            if (allocated(error)) exit body
            call check(error,.not. any(same_handle(ediff_session_registry, meta)), "unregistered")
            if (allocated(error)) exit body
            call check(error, same_handle(current_buffer(), ediff_registry_buffer), "the registry")
        end block body

        call teardown(dir)
    end subroutine test_quit_meta_buffer
end module test_ediff_multiple_mod
