! test_version_control_mod.f90 -- tests for version_control_hooks_mod, version_control_dispatcher_mod, version_control_mod and
! version_control_git_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/vc-tests/vc-tests.el,
! test/lisp/vc/vc-tests/vc-test-misc.el and test/lisp/vc/vc-git-tests.el
! in part; only the Git tests of the ported commands.
!
! Copyright (C) 2014-2026 Free Software Foundation, Inc.
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

!> Ports of vc-tests.el for Git (vc-test-git00-create-repo to
!> -git05-rename-file and -git10-rename-directory), of vc-test-misc.el
!> (vc-test-do-command-1 to -3 and -5 to -7, vc-test-exec-after-1 to -3)
!> and of vc-git-tests.el's program-version tests; own tests of the mode
!> line, diffs and reverting.  The Git tests are skipped without git.
!> Not ported: vc-test-git06-version-diff and the checkin, working-tree
!> and patch tests (they commit through Log Edit mode), vc-test-do-command-4
!> (an asynchronous command's standard error is not kept apart), and
!> vc-test-exec-after-4 and -5.
module test_version_control_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, error_unit
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, condition_matches, FILE_ALREADY_EXISTS_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, &
                          get_buffer, is_buffer_live, generate_new_buffer_name, get_file_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point, point_minimum, goto_character, is_beginning_of_buffer
    use search_mod, only: regular_expression_search_forward, SEARCH_NOERROR
    use file_input_output_mod, only: initialize_fileio, fortmacs_wd, file_name_as_directory, file_exists, is_file_directory, &
                                     create_directory_internal, write_region, expand_file_name
    use files_mod, only: delete_directory, find_file_noselect, executable_find
    use execute_process_mod, only: execute_process
    use process_mod, only: make_process, is_process_live, accept_process_output
    use bindings_mod, only: initialize_bindings
    use version_control_hooks_mod, only: version_control_backend, version_control_registered, version_control_state, &
                                         version_control_working_revision, version_control_checkout_model, &
                                         version_control_file_clearprops, version_control_backends, version_control_backend_named, &
                                         version_control_clear_context, version_control_handled_backends, &
                                         VERSION_CONTROL_STATE_UNKNOWN, VERSION_CONTROL_UNREGISTERED, VERSION_CONTROL_ADDED, &
                                         VERSION_CONTROL_UP_TO_DATE, VERSION_CONTROL_EDITED, VERSION_CONTROL_MODEL_IMPLICIT
    use version_control_dispatcher_mod, only: version_control_do_command, version_control_exec_after, version_control_action_type, &
                                              VERSION_CONTROL_DESTINATION_CURRENT, VERSION_CONTROL_DESTINATION_DISCARD, &
                                              VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS
    use version_control_mod, only: version_control_fileset_type, version_control_register, version_control_responsible_backend, &
                                   version_control_rename_file, version_control_create_repository, version_control_diff_internal, &
                                   version_control_revert_files
    use version_control_git_mod, only: version_control_git_parse_program_version
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: collect_version_control_mod

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdirectory = "build/test-vc/"

    !> What the exec-after tests' actions set.
    logical :: success = .false.

contains

    !> Collect all exported unit tests.
    subroutine collect_version_control_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("vc-git-test-program-version-general", test_version_general), &
                    new_unittest("vc-git-test-program-version-windows", test_version_windows), &
                    new_unittest("vc-git-test-program-version-apple", test_version_apple), &
                    new_unittest("vc-git-test-program-version-other", test_version_other), &
                    new_unittest("vc-git-test-program-version-invalid-leading-string", test_version_leading_string), &
                    new_unittest("vc-git-test-program-version-invalid-leading-dot", test_version_leading_dot), &
                    new_unittest("vc-test-do-command-1", test_do_command_1), &
                    new_unittest("vc-test-do-command-2", test_do_command_2), &
                    new_unittest("vc-test-do-command-3", test_do_command_3), &
                    new_unittest("vc-test-do-command-5", test_do_command_5), &
                    new_unittest("vc-test-do-command-6", test_do_command_6), &
                    new_unittest("vc-test-do-command-7", test_do_command_7), &
                    new_unittest("vc-test-exec-after-1", test_exec_after_1), &
                    new_unittest("vc-test-exec-after-2", test_exec_after_2), &
                    new_unittest("vc-test-exec-after-3", test_exec_after_3), &
                    new_unittest("vc-test-git00-create-repo", test_create_repository), &
                    new_unittest("vc-test-git01-register", test_register), &
                    new_unittest("vc-test-git02-state", test_state), &
                    new_unittest("vc-test-git03-working-revision", test_working_revision), &
                    new_unittest("vc-test-git04-checkout-model", test_checkout_model), &
                    new_unittest("vc-test-git05-rename-file", test_rename_file), &
                    new_unittest("vc-test-git06-version-diff", test_version_diff), &
                    new_unittest("vc-test-git10-rename-directory", test_rename_directory), &
                    new_unittest("vc-git-mode-line", test_mode_line), &
                    new_unittest("vc-git-diff", test_diff), &
                    new_unittest("vc-git-print-log", test_print_log), &
                    new_unittest("vc-git-print-root-log", test_print_root_log), &
                    new_unittest("vc-dir-git", test_version_control_directory), &
                    new_unittest("vc-git-revert", test_revert), &
                    new_unittest("vc-git-test-branch-remotes", test_branch_remotes), &
                    new_unittest("vc-git-tags-and-branches", test_tags_and_branches), &
                    new_unittest("vc-git-branch-logs", test_branch_logs), &
                    new_unittest("vc-git-push-pull-incoming-outgoing", test_push_pull) &
                    ]
    end subroutine collect_version_control_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A list of the strings given.
    function strings(a1, a2, a3, a4) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4
        type(string_type), allocatable :: list(:)
        integer :: n

        n = 1
        if (present(a2)) n = 2
        if (present(a3)) n = 3
        if (present(a4)) n = 4
        allocate (list(n))
        list(1)%text = a1
        if (present(a2)) list(2)%text = a2
        if (present(a3)) list(3)%text = a3
        if (present(a4)) list(4)%text = a4
    end function strings

    !> A fresh temporary buffer, current.
    subroutine temporary_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(generate_new_buffer_name(" *vc-test*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = file_name_as_directory(fortmacs_wd)
    end subroutine temporary_buffer

    !> The test repository directory.
    function tdir() result(directory)
        character(len=:), allocatable :: directory

        directory = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdirectory)
    end function tdir

    !> Whether git is available (else the test is skipped).
    logical function has_git(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        call executable_find("git", path)
        has_git = len(path) > 0
        if (.not. has_git) call skip_test(error, "no git")
    end function has_git

    !> A buffer in a fresh, empty test directory (`default_directory'),
    !> made a Git repository with `version_control_create_repository'; only Git is handled.
    subroutine setup(h, status)
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call initialize_bindings()
        call initialize_fileio()
        call version_control_clear_context()
        version_control_handled_backends = [character(len=8) :: "Git"]
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(file_name_as_directory(fortmacs_wd)//"build", st)
        call create_directory_internal(tdir(), st)
        call temporary_buffer(h)
        buffers(current)%directory = tdir()
        call version_control_create_repository(version_control_backend_named("Git"), status)
        call set_buffer(h, st)
    end subroutine setup

    !> Remove the test directory and buffer H; handle all backends again.
    subroutine teardown(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        type(handle_type) :: version_control

        version_control = get_buffer("*vc*")
        if (is_buffer_live(version_control)) call close_buffer(version_control, st)
        if (is_buffer_live(h)) call close_buffer(h, st)
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        version_control_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        call version_control_clear_context()
    end subroutine teardown

    !> Write TEXT to FILE (write-region ... 'nomessage).
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: message_text

        old = current_buffer()
        call temporary_buffer(h)
        call insert(text, st)
        call write_region(file, .false., st, message=message_text)
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> Run git with ARGUMENTS in the test directory.  A failure (a nonzero exit
    !> status) is reported on standard error with git's output, so that a
    !> test that then fails names its cause.
    subroutine git(a1, a2, a3, a4, a5, a6)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6
        character(len=:), allocatable :: out

        call git_at(tdir(), out, a1, a2, a3, a4, a5, a6)
    end subroutine git

    !> Run git with ARGUMENTS in directory DIRECTORY, its output (standard error
    !> included) given as OUT.  A failure is reported as `git' does.
    subroutine git_at(directory, out, a1, a2, a3, a4, a5, a6)
        character(len=*), intent(in) :: directory, a1
        character(len=:), allocatable, intent(out) :: out
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6
        type(value_type) :: arguments(10)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n

        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        arguments(1) = value_type(text="-c")
        arguments(2) = value_type(text="user.name=t")
        arguments(3) = value_type(text="-c")
        arguments(4) = value_type(text="user.email=t@t")
        arguments(5) = value_type(text=a1)
        n = 5
        if (present(a2)) call append(a2)
        if (present(a3)) call append(a3)
        if (present(a4)) call append(a4)
        if (present(a5)) call append(a5)
        if (present(a6)) call append(a6)
        old = current_buffer()
        call temporary_buffer(h)
        buffers(current)%directory = directory
        call execute_process("git", result, st, destination=value_type(.true.), arguments=make_list(arguments(1:n)))
        out = buffer_string()

        if (.not. st%ok()) then
            write (error_unit, "(a)") "git "//a1//": "//format_error(st)
        else if (vtag(result) == V_INTEGER) then
            if (xfixnum(result) /= 0) write (error_unit, "(a)") "git "//a1//" failed: "//buffer_string()
        end if

        call close_buffer(h, st)
        call set_buffer(old, st)

    contains

        subroutine append(a)
            character(len=*), intent(in) :: a

            n = n + 1
            arguments(n) = value_type(text=a)
        end subroutine append
    end subroutine git_at

    !> The output of git with ARGUMENTS in the test directory, without its final
    !> newline.
    function git_output(a1, a2, a3) result(out)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3
        character(len=:), allocatable :: out

        call git_at(tdir(), out, a1, a2, a3)

        if (len(out) > 0) then
            if (out(len(out):) == achar(10)) out = out(1:len(out) - 1)
        end if
    end function git_output

    !> The Git backend's index.
    integer function git_b()
        git_b = version_control_backend_named("Git")
    end function git_b

    ! ------------------------------------------------------------------
    ! vc-git-tests.el
    ! ------------------------------------------------------------------

    subroutine test_version_general(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, version_control_git_parse_program_version("git version 2.30.1.0"), "2.30.1.0")
    end subroutine test_version_general

    subroutine test_version_windows(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, version_control_git_parse_program_version("git version 2.30.1.1.windows.1"), "2.30.1.1")
    end subroutine test_version_windows

    subroutine test_version_apple(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, version_control_git_parse_program_version("git version 2.30.1.2 (Apple Git-130)"), "2.30.1.2")
    end subroutine test_version_apple

    subroutine test_version_other(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, version_control_git_parse_program_version("git version 2.30.1.3.foo.bar"), "2.30.1.3")
    end subroutine test_version_other

    subroutine test_version_leading_string(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, version_control_git_parse_program_version("git version foo.bar.2.30.1.4"), "0")
    end subroutine test_version_leading_string

    subroutine test_version_leading_dot(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, version_control_git_parse_program_version("git version .2.30.1.5"), "0")
    end subroutine test_version_leading_dot

    ! ------------------------------------------------------------------
    ! vc-test-misc.el
    ! ------------------------------------------------------------------

    !> Run `version_control_do_command' of sh -c "echo foo; echo >&2 bar" in a temporary
    !> buffer: synchronously (or with ASYNCHRONOUS), the output in the buffer
    !> (TO_BUFFER), standard error mixed in (STDERR); gives the buffer's
    !> text.
    function do_command(to_buffer, stderr, asynchronous) result(text)
        logical, intent(in) :: to_buffer, stderr
        logical, intent(in), optional :: asynchronous
        character(len=:), allocatable :: text
        type(string_type) :: none(0)
        type(handle_type) :: h, process
        type(status_type) :: st
        logical :: got
        integer :: k, destination, okstatus

        call initialize_fileio()
        call temporary_buffer(h)
        destination = VERSION_CONTROL_DESTINATION_DISCARD
        if (to_buffer) destination = VERSION_CONTROL_DESTINATION_CURRENT
        okstatus = 0

        if (present(asynchronous)) then
            if (asynchronous) okstatus = VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS
        end if

        call version_control_do_command(destination, okstatus, "sh", none, strings("-c", "echo foo; echo >&2 bar"), st, &
                                        stderr=stderr, &
                                        process=process)

        if (okstatus == VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS) then
            do k = 1, 600
                if (.not. is_process_live(process)) exit
                call accept_process_output(got, st, process, 0.05)
            end do

            call accept_process_output(got, st, seconds=0.05)
        end if

        call set_buffer(h, st)
        text = buffer_string()
        call close_buffer(h, st)
    end function do_command

    !> Test `version_control_do_command' synchronous, discarding stderr.
    subroutine test_do_command_1(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, do_command(.true., .false.), "foo"//achar(10))
    end subroutine test_do_command_1

    !> Test `version_control_do_command' synchronous, keeping stderr.
    subroutine test_do_command_2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text

        text = do_command(.true., .true.)
        call check(error, index(text, "foo") > 0 .and. index(text, "bar") > 0, text)
    end subroutine test_do_command_2

    !> Test `version_control_do_command' synchronous, discarding both.
    subroutine test_do_command_3(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, do_command(.false., .true.), "")
    end subroutine test_do_command_3

    !> Test `version_control_do_command' asynchronous, keeping stderr.
    subroutine test_do_command_5(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text

        text = do_command(.true., .true., .true.)
        call check(error, index(text, "foo") > 0 .and. index(text, "bar") > 0, text)
    end subroutine test_do_command_5

    !> Test `version_control_do_command' asynchronous, discarding both.
    subroutine test_do_command_6(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, do_command(.false., .true., .true.), "")
    end subroutine test_do_command_6

    !> Test `version_control_do_command' setting up the buffer.
    subroutine test_do_command_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type) :: none(0)
        type(handle_type) :: buffer, h
        type(status_type) :: st

        call initialize_fileio()
        call temporary_buffer(h)
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), buffer, st)
        call set_buffer(h, st)
        call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, 0, "sh", none, strings("-c", &
                                                                                     "echo foo; echo >&2 bar"), st, buffer=buffer, &
                                        stderr=.false.)
        call set_buffer(buffer, st)
        call check(error, buffer_string(), "foo"//achar(10))
        call close_buffer(buffer, st)
        call close_buffer(h, st)
    end subroutine test_do_command_7

    !> The action of the exec-after tests: set `success'.
    recursive subroutine set_success(action, status)
        type(version_control_action_type), intent(in) :: action
        type(status_type), intent(out) :: status

        status = status_type()
        if (action%n < 0) return
        success = .true.
    end subroutine set_success

    !> Start sh -c COMMAND in buffer H.
    subroutine start_shell(command, h, process)
        character(len=*), intent(in) :: command
        type(handle_type), intent(in) :: h
        type(handle_type), intent(out) :: process
        type(string_type), allocatable :: arguments(:)
        type(status_type) :: st

        call append_string(arguments, "sh", "-c", command)
        call make_process("test", arguments, process, st, buffer=h)
    end subroutine start_shell

    !> Wait for PROCESS to exit, its sentinels run (vc-test--exec-after-wait).
    subroutine exec_after_wait(process)
        type(handle_type), intent(in) :: process
        type(status_type) :: st
        logical :: got
        integer :: k

        do k = 1, 600
            if (.not. is_process_live(process)) exit
            call accept_process_output(got, st, process, 0.05)
        end do

        call accept_process_output(got, st, seconds=0.05)
    end subroutine exec_after_wait

    !> Test `version_control_exec_after' adding a sentinel.
    subroutine test_exec_after_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, process
        type(version_control_action_type) :: action
        type(status_type) :: st

        call initialize_fileio()
        call temporary_buffer(h)
        call start_shell("sleep 0.2; echo hello", h, process)
        success = .false.
        action%execute => set_success
        call version_control_exec_after(action, st)
        call check(error,.not. success, "not yet")

        if (.not. allocated(error)) then
            call exec_after_wait(process)
            call check(error, success, "after the process")
        end if

        call close_buffer(h, st)
    end subroutine test_exec_after_1

    !> Test `version_control_exec_after' executing the code immediately.
    subroutine test_exec_after_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, process
        type(version_control_action_type) :: action
        type(status_type) :: st

        call initialize_fileio()
        call temporary_buffer(h)
        call start_shell("sleep 0.2; echo hello", h, process)
        call exec_after_wait(process)
        success = .false.
        action%execute => set_success
        call version_control_exec_after(action, st)
        call check(error, success)
        call close_buffer(h, st)
    end subroutine test_exec_after_2

    !> Test OKSTATUS of `version_control_exec_after'.
    subroutine test_exec_after_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, process
        type(version_control_action_type) :: action
        type(status_type) :: st

        call initialize_fileio()
        call temporary_buffer(h)
        call start_shell("true", h, process)
        success = .false.
        action%execute => set_success
        action%okstatus = 0
        call version_control_exec_after(action, st)
        call exec_after_wait(process)
        call check(error, success, "true")
        call close_buffer(h, st)
        if (allocated(error)) return
        call temporary_buffer(h)
        call start_shell("false", h, process)
        success = .false.
        call version_control_exec_after(action, st)
        call exec_after_wait(process)
        call check(error,.not. success, "false")
        call close_buffer(h, st)
    end subroutine test_exec_after_3

    ! ------------------------------------------------------------------
    ! vc-tests.el
    ! ------------------------------------------------------------------

    !> Create a test repository in `default_directory'.
    subroutine test_create_repository(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_git(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, is_file_directory(tdir()), "the directory")
            if (allocated(error)) exit body
            call check(error, version_control_responsible_backend(tdir(), st) == git_b(), "responsible backend")
        end block body

        call teardown(h)
    end subroutine test_create_repository

    !> Register and unregister a file.
    !> This checks also `version_control_backend' and `version_control_responsible_backend'.
    subroutine test_register(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name1, temporary_name2

        if (.not. has_git(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            ! For file oriented backends CONCURRENT_VERSIONS_SYSTEM, RCS and SUBVERSION the backend is
            ! returned, and the directory is registered already.
            call check(error,.not. version_control_registered(tdir()), "directory not registered")
            if (allocated(error)) exit body
            call check(error, version_control_responsible_backend(tdir(), st) == git_b(), "responsible backend")
            if (allocated(error)) exit body
            temporary_name1 = tdir()//"foo"
            temporary_name2 = "bla"
            ! Register files.  Check for it.
            call write_file(temporary_name1, "foo")
            call check(error, file_exists(temporary_name1) .and. version_control_backend(temporary_name1) == 0 .and. &
                       version_control_responsible_backend(temporary_name1, &
                                                      st) == git_b() .and. .not. version_control_registered(temporary_name1), "foo")
            if (allocated(error)) exit body
            call write_file(tdir()//temporary_name2, "bla")
            call check(error, &
                       version_control_backend(temporary_name2) == 0 .and. version_control_responsible_backend(temporary_name2, &
                                                                                                              st) == git_b() .and. &
                       .not. version_control_registered(temporary_name2), "bla")
            if (allocated(error)) exit body
            fs%backend = git_b()
            fs%files = strings(temporary_name1, temporary_name2)
            call version_control_register(st, fs)
            call check(error, st%ok(), "register: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, &
                       version_control_backend(temporary_name1) == git_b() .and. version_control_registered(temporary_name1), &
                       "foo registered")
            if (allocated(error)) exit body
            call check(error, &
                       version_control_backend(temporary_name2) == git_b() .and. version_control_registered(temporary_name2), &
                       "bla registered")
            if (allocated(error)) exit body
            ! Unregister the files.
            call version_control_backends(git_b())%unregister(temporary_name1, st)
            call version_control_file_clearprops(temporary_name1)
            call check(error, &
                       version_control_backend(temporary_name1) == 0 .and. .not. version_control_registered(temporary_name1), &
                       "foo unregistered")
            if (allocated(error)) exit body
            call version_control_backends(git_b())%unregister(temporary_name2, st)
            call version_control_file_clearprops(temporary_name2)
            call check(error, &
                       version_control_backend(temporary_name2) == 0 .and. .not. version_control_registered(temporary_name2), &
                       "bla unregistered")
            if (allocated(error)) exit body
            ! The files should still exist.
            call check(error, file_exists(temporary_name1) .and. file_exists(tdir()//temporary_name2), "still there")
        end block body

        call teardown(h)
    end subroutine test_register

    !> Check the different states of a file.
    subroutine test_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name
        integer :: s

        if (.not. has_git(error)) return
        call setup(h, st)

        body: block
            temporary_name = tdir()//"foo"
            ! Check state of a nonexistent file.
            call check(error, version_control_state(temporary_name) == VERSION_CONTROL_STATE_UNKNOWN, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check state.
            call write_file(temporary_name, "foo")
            s = version_control_state(temporary_name, git_b())
            call check(error, s == VERSION_CONTROL_STATE_UNKNOWN .or. s == VERSION_CONTROL_UNREGISTERED, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check state.
            fs%backend = git_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            s = version_control_state(temporary_name)
            call check(error, &
                       s == VERSION_CONTROL_STATE_UNKNOWN .or. s == VERSION_CONTROL_ADDED .or. s == VERSION_CONTROL_UP_TO_DATE, &
                       "registered")
            if (allocated(error)) exit body
            ! Unregister the file.  Check state.
            call version_control_backends(git_b())%unregister(temporary_name, st)
            call version_control_file_clearprops(temporary_name)
            s = version_control_state(temporary_name, git_b())
            call check(error, s == VERSION_CONTROL_STATE_UNKNOWN .or. s == VERSION_CONTROL_UNREGISTERED, "unregistered")
        end block body

        call teardown(h)
    end subroutine test_state

    !> Check the working revision of a repository.
    subroutine test_working_revision(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name, revision
        logical :: found

        if (.not. has_git(error)) return
        call setup(h, st)

        body: block
            call version_control_working_revision(tdir(), revision, found)
            call check(error,.not. found .or. revision == "0", "repository")
            if (allocated(error)) exit body
            temporary_name = tdir()//"foo"
            ! Check initial working revision, should be nil until it's
            ! registered.
            call version_control_working_revision(temporary_name, revision, found)
            call check(error,.not. found, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check working revision.
            call write_file(temporary_name, "foo")
            call version_control_working_revision(temporary_name, revision, found)
            call check(error,.not. found, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check working revision.
            fs%backend = git_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            call version_control_working_revision(temporary_name, revision, found)
            ! nil is fine, at least in Git's case, because `version_control_register'
            ! only makes the file `added' in this case.
            call check(error,.not. found, "registered")
            if (allocated(error)) exit body
            ! Unregister the file.  Check working revision.
            call version_control_backends(git_b())%unregister(temporary_name, st)
            call version_control_file_clearprops(temporary_name)
            call version_control_working_revision(temporary_name, revision, found)
            call check(error,.not. found, "unregistered")
        end block body

        call teardown(h)
    end subroutine test_working_revision

    !> Check the checkout model of a repository.
    subroutine test_checkout_model(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name

        if (.not. has_git(error)) return
        call setup(h, st)

        body: block
            call check(error, version_control_checkout_model(git_b(), strings(tdir())) == VERSION_CONTROL_MODEL_IMPLICIT, &
                       "repository")
            if (allocated(error)) exit body
            temporary_name = tdir()//"foo"
            call check(error, version_control_checkout_model(git_b(), strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, &
                       "nonexistent")
            if (allocated(error)) exit body
            call write_file(temporary_name, "foo")
            call check(error, version_control_checkout_model(git_b(), strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, &
                       "new file")
            if (allocated(error)) exit body
            fs%backend = git_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            call check(error, version_control_checkout_model(git_b(), strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, &
                       "registered")
            if (allocated(error)) exit body
            call version_control_backends(git_b())%unregister(temporary_name, st)
            call version_control_file_clearprops(temporary_name)
            call check(error, version_control_checkout_model(git_b(), strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, &
                       "unregistered")
        end block body

        call teardown(h)
    end subroutine test_checkout_model

    !> Check the rename-file action.
    subroutine test_rename_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name, new_name, new_directory, saved

        if (.not. has_git(error)) return
        call setup(h, st)

        body: block
            temporary_name = tdir()//"foo"
            new_name = tdir()//"bar"
            ! Write a new file.
            call write_file(temporary_name, "foo")
            ! Register it.  Renaming can fail otherwise.
            fs%backend = git_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            ! Test that `version_control_rename_file' isn't affected by
            ! `default_directory' except for the meaning of OLD and NEW if
            ! they are relative file names.
            saved = buffers(current)%directory
            buffers(current)%directory = file_name_as_directory(fortmacs_wd)//"build/"
            call version_control_rename_file("test-vc/foo", "test-vc/bar", st)
            buffers(current)%directory = saved
            call check(error, st%ok(), "rename: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(temporary_name) .and. file_exists(new_name), "renamed")
            if (allocated(error)) exit body
            call check(error, version_control_state(new_name) == VERSION_CONTROL_ADDED, "added")
            if (allocated(error)) exit body
            ! Test OK-IF-ALREADY-EXISTS.
            temporary_name = tdir()//"qux"
            new_name = tdir()//"quuux"
            call write_file(temporary_name, "qux")
            call write_file(new_name, "quuux")
            fs%files = strings("qux", "quuux")
            call version_control_register(st, fs)
            call version_control_rename_file(temporary_name, new_name, st)
            call check(error, condition_matches(st%code, FILE_ALREADY_EXISTS_CODE), "file-already-exists")
            if (allocated(error)) exit body
            call version_control_rename_file(temporary_name, new_name, st, ok_if_already_exists=.true.)
            call check(error, st%ok(), "ok-if-already-exists: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(temporary_name) .and. file_exists(new_name), "replaced")
            if (allocated(error)) exit body
            ! Test moving into an existing directory.
            temporary_name = tdir()//"quux"
            new_directory = tdir()//"dir1/"
            new_name = tdir()//"dir1/quux"
            call create_directory_internal(new_directory, st)
            call write_file(temporary_name, "quux")
            fs%files = strings("dir1/", "quux")
            call version_control_register(st, fs)
            call version_control_rename_file(temporary_name, new_directory, st)
            call check(error, st%ok(), "into a directory: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(temporary_name) .and. file_exists(new_name), "moved")
        end block body

        call teardown(h)
    end subroutine test_rename_file

    !> Check the diff version of a repository.
    subroutine test_version_diff(error)
        use version_control_mod, only: version_control_checkin, version_control_version_diff
        use log_edit_mod, only: log_edit_done
        use environment_mod, only: setenv
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: names(3) = [character(len=18) :: "EMAIL", "GIT_AUTHOR_NAME", &
                                                                                                               "GIT_COMMITTER_NAME"]
        character(len=*), parameter :: values(3) = [character(len=20) :: "joh.doe@example.com", "A", "C"]
        type(handle_type) :: h, buffer, diff
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=256) :: saved(3)
        integer :: saved_status(3), k
        character(len=:), allocatable :: temporary_name, rawtext

        if (.not. has_git(error)) return

        ! vc-test--with-author-identity: git tries various approaches to
        ! guess a user name and email, which can fail depending on how the
        ! system is configured.
        do k = 1, 3
            call get_environment_variable(trim(names(k)), saved(k), status=saved_status(k))
            call setenv(trim(names(k)), trim(values(k)))
        end do

        call setup(h, st)

        body: block
            temporary_name = tdir()//"foo"
            fs%backend = git_b()
            fs%files = strings("foo")
            ! Write and register a new file.
            call write_file(temporary_name, "originaltext")
            call version_control_register(st, fs)
            call find_file_noselect(temporary_name, buffer, st)
            call set_buffer(buffer, st)
            ! Checkin file.
            call version_control_checkin(fs%files, fs%backend, st)
            call check(error, st%ok(), "checkin: "//format_error(st))
            if (allocated(error)) exit body
            call insert("Testing vc-version-diff", st)
            call check(error, st%ok(), "insert: "//format_error(st))
            if (allocated(error)) exit body
            call log_edit_done(st)
            call check(error, st%ok(), "log-edit-done: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(h, st)
            ! Modify file content.
            call write_file(temporary_name, "updatedtext")
            ! Check version diff.
            call version_control_version_diff("", "", st)
            call check(error, st%ok(), "version-diff: "//format_error(st))
            if (allocated(error)) exit body
            diff = get_buffer("*vc-diff*")
            call check(error, is_buffer_live(diff), "*vc-diff*")
            if (allocated(error)) exit body
            call set_buffer(diff, st)
            rawtext = buffer_string()
            call set_buffer(h, st)
            call check(error, index(rawtext, "-originaltext") > 0, "-originaltext in: "//rawtext)
            if (allocated(error)) exit body
            call check(error, index(rawtext, "+updatedtext") > 0, "+updatedtext in: "//rawtext)
        end block body

        diff = get_buffer("*vc-diff*")
        if (is_buffer_live(diff)) call close_buffer(diff, st)
        diff = get_buffer("*vc-log*")
        if (is_buffer_live(diff)) call close_buffer(diff, st)
        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)

        do k = 1, 3
            if (saved_status(k) == 0) then
                call setenv(trim(names(k)), trim(saved(k)))
            else
                call setenv(trim(names(k)))
            end if
        end do
    end subroutine test_version_diff

    !> Check the rename-file action for directories.
    subroutine test_rename_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_directory, new_directory

        if (.not. has_git(error)) return
        call setup(h, st)
        fs%backend = git_b()

        body: block
            ! Test mix of registered and unregistered files.
            temporary_directory = tdir()//"dir1/"
            new_directory = tdir()//"dir2/"
            call create_directory_internal(temporary_directory, st)
            call write_file(temporary_directory//"foo", "foo")
            call write_file(temporary_directory//"bar", "bar")
            ! Register TMP-NAME1 but *not* TMP-NAME2.
            fs%files = strings("dir1/foo")
            call version_control_register(st, fs)
            call version_control_rename_file(tdir()//"dir1", tdir()//"dir2", st)
            call check(error, st%ok(), "mixed: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(temporary_directory//"foo") .and. .not. file_exists(temporary_directory//"bar") &
                       .and. &
                       file_exists(new_directory//"foo") .and. version_control_registered(new_directory//"foo") .and. &
                       file_exists(new_directory//"bar") .and. .not. version_control_registered(new_directory//"bar"), "mixed")
            if (allocated(error)) exit body
            ! Test only unregistered files.
            temporary_directory = tdir()//"dir3/"
            new_directory = tdir()//"dir4/"
            call create_directory_internal(temporary_directory, st)
            call write_file(temporary_directory//"foo", "foo")
            call version_control_rename_file(tdir()//"dir3", tdir()//"dir4", st)
            call check(error, st%ok(), "unregistered: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(temporary_directory//"foo") .and. file_exists(new_directory//"foo") .and. &
                       .not. version_control_registered(new_directory//"foo"), "unregistered")
            if (allocated(error)) exit body
            ! Test only registered files.
            temporary_directory = tdir()//"dir5/"
            new_directory = tdir()//"dir6/"
            call create_directory_internal(temporary_directory, st)
            call write_file(temporary_directory//"foo", "foo")
            fs%files = strings("dir5/foo")
            call version_control_register(st, fs)
            call version_control_rename_file(tdir()//"dir5", tdir()//"dir6", st)
            call check(error, st%ok(), "registered: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(temporary_directory//"foo") .and. file_exists(new_directory//"foo") .and. &
                       version_control_registered(new_directory//"foo"), "registered")
        end block body

        call teardown(h)
    end subroutine test_rename_directory

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> A committed file a.txt ("one\ntwo\n") on branch main, visited; its
    !> buffer is current and shown alone.
    subroutine committed_file(h, buffer, status)
        type(handle_type), intent(out) :: h, buffer
        type(status_type), intent(out) :: status

        call setup(h, status)
        call git("checkout", "-q", "-b", "main")
        call write_file(tdir()//"a.txt", "one"//achar(10)//"two"//achar(10))
        call git("add", "a.txt")
        call git("commit", "-q", "-m", "init")
        call find_file_noselect(tdir()//"a.txt", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        call show_alone()
    end subroutine committed_file

    !> Visiting a file under Git shows Git-BRANCH in the mode line, and
    !> Git:BRANCH once it is edited and saved.
    subroutine test_mode_line(error)
        use files_mod, only: save_buffer
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer
        type(status_type) :: st

        if (.not. has_git(error)) return
        call committed_file(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, allocated(buffers(current)%version_control_mode), "version_control_mode set")
            if (allocated(error)) exit body
            call check(error, buffers(current)%version_control_mode, " Git-main")
            if (allocated(error)) exit body
            call check(error, buffers(current)%backup_inhibited, "backup-inhibited")
            if (allocated(error)) exit body
            call goto_character(point_minimum())
            call insert("X", st)
            call save_buffer(1_int64, .false., st)
            call check(error, st%ok(), "save: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, version_control_state(buffers(current)%filename) == VERSION_CONTROL_EDITED, "edited")
            if (allocated(error)) exit body
            call check(error, buffers(current)%version_control_mode, " Git:main")
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_mode_line

    !> `version_control_diff_internal' of an edited file gives its diff in *vc-diff*, in
    !> Diff mode; none for an unchanged one.
    subroutine test_diff(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, d
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: text
        logical :: changes

        if (.not. has_git(error)) return
        call committed_file(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            fs%backend = git_b()
            fs%files = strings(tdir()//"a.txt")
            call version_control_diff_internal(.false., fs, changes, st)
            call check(error, st%ok() .and. .not. changes, "unchanged: "//format_error(st))
            if (allocated(error)) exit body
            call write_file(tdir()//"a.txt", "one"//achar(10)//"three"//achar(10))
            call set_buffer(buffer, st)
            call version_control_diff_internal(.false., fs, changes, st)
            call check(error, st%ok() .and. changes, "changed: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, "-two"//achar(10)//"+three"//achar(10)) > 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(current)%major_mode, "diff_mode")
            if (allocated(error)) exit body
            call close_buffer(d, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_diff

    !> Wait for the process of the log buffer BUFFER to finish (its
    !> `version_control_exec_after' actions run).
    subroutine wait_buffer_process(buffer)
        use process_mod, only: get_buffer_process
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: process
        type(status_type) :: st
        logical :: got
        integer :: k

        process = get_buffer_process(buffer)

        if (process%index /= 0) then
            do k = 1, 600
                if (.not. is_process_live(process)) exit
                call accept_process_output(got, st, process, 0.05)
            end do
        end if

        call accept_process_output(got, st, seconds=0.05)
    end subroutine wait_buffer_process

    !> A Git repository whose a.txt has two commits ("init" and "second"),
    !> visited in BUFFER.
    subroutine two_commits(h, buffer, status)
        type(handle_type), intent(out) :: h, buffer
        type(status_type), intent(out) :: status

        call committed_file(h, buffer, status)
        if (.not. status%ok()) return
        call write_file(tdir()//"a.txt", "one"//achar(10)//"three"//achar(10))
        call git("commit", "-q", "-a", "-m", "second")
    end subroutine two_commits

    !> C-x v l in a file under Git shows its long log in Git-Log-View mode,
    !> where n and p move between commits, d shows the diff of a commit,
    !> m marks it and w copies its revision.
    subroutine test_print_log(error)
        use version_control_mod, only: version_control_print_log
        use log_view_mod, only: log_view_message_next, log_view_message_previous, log_view_current_tag, log_view_diff, &
                                log_view_mark_entry, log_view_get_marked, log_view_unmark_all_entries
        use font_lock_mod, only: font_lock_ensure
        use editing_functions_mod, only: point, point_maximum
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log, d
        type(status_type) :: st
        type(string_type), allocatable :: marked(:)
        character(len=:), allocatable :: text, newest, oldest
        logical :: has

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            call version_control_print_log(st, "", 500_int64)
            call check(error, st%ok(), "vc-print-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call check(error, is_buffer_live(log), "*vc-change-log*")
            if (allocated(error)) exit body
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, buffers(current)%major_mode, "version_control_git_log_view_mode")
            if (allocated(error)) exit body
            call check(error, index(text, "    second") > 0 .and. index(text, "    init") > index(text, "    second"), text)
            if (allocated(error)) exit body
            call check(error, buffers(current)%read_only, "read-only")
            if (allocated(error)) exit body
            ! n and p.
            call goto_character(point_minimum())
            call log_view_current_tag(newest, has)
            call check(error, has .and. len(newest) == 40, "newest tag: "//newest)
            if (allocated(error)) exit body
            call log_view_message_next(st)
            call log_view_current_tag(oldest, has)
            call check(error, st%ok() .and. has .and. oldest /= newest, "n: "//format_error(st))
            if (allocated(error)) exit body
            ! n on the last entry goes to the end (easy-mmode-define-navigation).
            call log_view_message_next(st)
            call check(error, st%ok() .and. point() == point_maximum(), "n at the last entry")
            if (allocated(error)) exit body
            call log_view_message_previous(st)
            call log_view_current_tag(text, has)
            call check(error, st%ok() .and. text == oldest, "p from the end: "//format_error(st))
            if (allocated(error)) exit body
            call log_view_message_previous(st)
            call log_view_current_tag(text, has)
            call check(error, st%ok() .and. text == newest, "p: "//format_error(st))
            if (allocated(error)) exit body
            ! d on the newest commit: its diff with the previous one.
            call log_view_diff(point(), point(), st)
            call check(error, st%ok(), "d: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, "-two"//achar(10)//"+three"//achar(10)) > 0, text)
            if (allocated(error)) exit body
            call close_buffer(d, st)
            ! m marks the entry (the marks are found through the faces).
            call set_buffer(log, st)
            call font_lock_ensure(st)
            call goto_character(point_minimum())
            call log_view_mark_entry(1_int64, st)
            call log_view_get_marked(marked)
            call check(error, size(marked) == 1, "one marked")
            if (allocated(error)) exit body
            call check(error, marked(1)%text, newest)
            if (allocated(error)) exit body
            call log_view_unmark_all_entries(st)
            call log_view_get_marked(marked)
            call check(error, size(marked) == 0, "unmarked")
            if (allocated(error)) exit body
            call close_buffer(log, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_print_log

    !> C-x v L shows the short log of the whole tree, whose entries RET
    !> expands to the full log message and collapses again.
    subroutine test_print_root_log(error)
        use version_control_mod, only: version_control_print_root_log
        use log_view_mod, only: log_view_message_regular_expression, log_view_toggle_entry_display
        use search_mod, only: regular_expression_search_forward
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log
        type(status_type) :: st
        character(len=:), allocatable :: short, text
        integer(int64) :: found
        integer :: entries

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            call version_control_print_root_log(st, 500_int64)
            call check(error, st%ok(), "vc-print-root-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            short = buffer_string()
            call goto_character(point_minimum())
            entries = 0

            do
                call regular_expression_search_forward(log_view_message_regular_expression%value, found, st, noerror=SEARCH_NOERROR)
                if (found == 0) exit
                entries = entries + 1
            end do

            call check(error, entries == 2, "two short entries in:"//achar(10)//short)
            if (allocated(error)) exit body
            call check(error, index(short, "second") > 0 .and. index(short, "Author:") == 0, short)
            if (allocated(error)) exit body
            ! RET expands the first entry.
            call goto_character(point_minimum())
            call log_view_toggle_entry_display(st)
            text = buffer_string()
            call check(error, st%ok() .and. index(text, "Author:") > 0, "expanded: "//format_error(st)//achar(10)//text)
            if (allocated(error)) exit body
            call check(error, index(text, "second") < index(text, "Author:"), text)
            if (allocated(error)) exit body
            ! RET again collapses it.
            call goto_character(point_minimum())
            call log_view_toggle_entry_display(st)
            text = buffer_string()
            call check(error, st%ok() .and. text == short, "collapsed: "//format_error(st)//achar(10)//text)
            if (allocated(error)) exit body
            call close_buffer(log, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_print_root_log

    !> Wait for the status process of the VC-Dir buffer BUFFER to finish.
    subroutine wait_version_control_directory(buffer)
        use version_control_directory_mod, only: version_control_directory_busy
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: old
        type(status_type) :: st
        logical :: got, busy
        integer :: k

        old = current_buffer()

        do k = 1, 600
            call set_buffer(buffer, st)
            busy = version_control_directory_busy()
            call set_buffer(old, st)
            if (.not. busy) exit
            call accept_process_output(got, st, seconds=0.05)
        end do

        call accept_process_output(got, st, seconds=0.05)
    end subroutine wait_version_control_directory

    !> C-x v d lists the edited and unregistered files of a Git repository
    !> under its headers; n moves to the files, m marks one, and the VERSION_CONTROL
    !> commands then act on the marked files.
    subroutine test_version_control_directory(error)
        use version_control_directory_mod, only: version_control_directory, version_control_directory_next_line, &
                                                 version_control_directory_mark, version_control_directory_marked_files, &
                                                 version_control_directory_deduce_fileset, version_control_directory_current_file, &
                                                 version_control_directory_mark_all_files, &
                                                 version_control_directory_unmark_all_files, version_control_directory_hide_state
        use version_control_mod, only: version_control_deduce_fileset
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, directory
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        type(string_type), allocatable :: marked(:)
        character(len=:), allocatable :: text, file

        if (.not. has_git(error)) return
        call committed_file(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call write_file(tdir()//"a.txt", "one"//achar(10)//"three"//achar(10))
            call write_file(tdir()//"b.txt", "b"//achar(10))
            call create_directory_internal(tdir()//"sub", st)
            call write_file(tdir()//"sub/c.txt", "c"//achar(10))
            call set_buffer(h, st)
            call version_control_directory(tdir(), st)
            call check(error, st%ok(), "vc-dir: "//format_error(st))
            if (allocated(error)) exit body
            directory = get_buffer("*vc-dir*")
            call check(error, is_buffer_live(directory), "*vc-dir*")
            if (allocated(error)) exit body
            call wait_version_control_directory(directory)
            call set_buffer(directory, st)
            text = buffer_string()
            call check(error, buffers(current)%major_mode, "version_control_directory_mode")
            if (allocated(error)) exit body
            call check(error, index(text, "VC backend : Git"//achar(10)) == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "Branch     : main"//achar(10)) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "     edited              a.txt") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "     unregistered        b.txt") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "                         sub/") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "     unregistered        sub/c.txt") > 0, text)
            if (allocated(error)) exit body
            ! n from the headers goes past "./" to a.txt.
            call goto_character(point_minimum())
            call version_control_directory_next_line(1, st)
            call version_control_directory_current_file(file, st)
            call check(error, st%ok() .and. file == tdir()//"a.txt", "n: "//file)
            if (allocated(error)) exit body
            ! m marks a.txt and moves to b.txt.
            call version_control_directory_mark(st)
            call version_control_directory_marked_files(marked)
            call check(error, size(marked) == 1, "one marked")
            if (allocated(error)) exit body
            call check(error, marked(1)%text, tdir()//"a.txt")
            if (allocated(error)) exit body
            call version_control_directory_current_file(file, st)
            call check(error, file == tdir()//"b.txt", "m moves down: "//file)
            if (allocated(error)) exit body
            text = buffer_string()
            call check(error, index(text, "  *  edited              a.txt") > 0, text)
            if (allocated(error)) exit body
            ! The VERSION_CONTROL commands act on the marked files.
            call version_control_deduce_fileset(fs, st, state_model_only_files=.true.)
            call check(error, st%ok() .and. size(fs%files) == 1, "fileset: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, fs%files(1)%text == tdir()//"a.txt" .and. fs%state == VERSION_CONTROL_EDITED, "fileset of a.txt")
            if (allocated(error)) exit body
            ! M marks the files in the state of the current one.
            call version_control_directory_mark_all_files(.false., st)
            call version_control_directory_marked_files(marked)
            call check(error, st%ok() .and. size(marked) == 3, "M marks the unregistered files too")
            if (allocated(error)) exit body
            call version_control_directory_unmark_all_files(.true.)
            call version_control_directory_marked_files(marked)
            call check(error, size(marked) == 0, "C-u U unmarks all")
            if (allocated(error)) exit body
            ! v on a.txt commits it (with the author identity of
            ! vc-test--with-author-identity), and VC-Dir then shows it
            ! up-to-date; x hides it.
            call goto_character(point_minimum())
            call version_control_directory_next_line(1, st)
            call commit_from_version_control_directory(st)
            call check(error, st%ok(), "commit: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(directory, st)
            text = buffer_string()
            call check(error, index(text, "     up-to-date          a.txt") > 0, text)
            if (allocated(error)) exit body
            call version_control_directory_hide_state()
            text = buffer_string()
            call check(error, index(text, "a.txt") == 0 .and. index(text, "b.txt") > 0, text)
            if (allocated(error)) exit body
            call close_buffer(directory, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_version_control_directory

    !> C-x v v in a VC-Dir buffer on an edited file, with a log message:
    !> the commit, run with an author identity.
    subroutine commit_from_version_control_directory(status)
        use version_control_mod, only: version_control_next_action
        use log_edit_mod, only: log_edit_done
        use environment_mod, only: setenv
        type(status_type), intent(out) :: status
        character(len=*), parameter :: names(3) = [character(len=18) :: "EMAIL", "GIT_AUTHOR_NAME", &
                                                                                                               "GIT_COMMITTER_NAME"]
        character(len=*), parameter :: values(3) = [character(len=20) :: "joh.doe@example.com", "A", "C"]
        character(len=256) :: saved(3)
        integer :: saved_status(3), k
        type(status_type) :: st

        do k = 1, 3
            call get_environment_variable(trim(names(k)), saved(k), status=saved_status(k))
            call setenv(trim(names(k)), trim(values(k)))
        end do

        call version_control_next_action(.false., status)

        if (status%ok()) then
            call insert("Commit from VC-Dir", st)
            call log_edit_done(status)
        end if

        do k = 1, 3
            if (saved_status(k) == 0) then
                call setenv(trim(names(k)), trim(saved(k)))
            else
                call setenv(trim(names(k)))
            end if
        end do
    end subroutine commit_from_version_control_directory

    !> `version_control_revert_files' puts back the committed text of a file.
    subroutine test_revert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, chk
        type(status_type) :: st

        if (.not. has_git(error)) return
        call committed_file(h, buffer, st)

        body: block
            call write_file(tdir()//"a.txt", "changed"//achar(10))
            call version_control_revert_files(git_b(), strings(tdir()//"a.txt"), st)
            call check(error, st%ok(), "revert: "//format_error(st))
            if (allocated(error)) exit body
            ! Visit the file afresh: reverting its buffer is not ported.
            call close_buffer(buffer, st)
            call find_file_noselect(tdir()//"a.txt", chk, st)
            call set_buffer(chk, st)
            call check(error, buffer_string(), "one"//achar(10)//"two"//achar(10))
            call close_buffer(chk, st)
            call set_buffer(h, st)
            if (allocated(error)) exit body
            call check(error, version_control_state(tdir()//"a.txt") == VERSION_CONTROL_UP_TO_DATE, "up to date")
        end block body

        call teardown(h)
    end subroutine test_revert

    ! ------------------------------------------------------------------
    ! Tags, branches, pulling and pushing
    ! ------------------------------------------------------------------

    !> Queue TEXT as input events (for the minibuffer).
    subroutine type_text(text)
        use keyboard_mod, only: unread_command_events
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(text)))

        do i = 1, len(text)
            evs(i) = value_type(int(iachar(text(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_text

    !> Port of vc-git-tests.el vc-git-test-branch-remotes: the remote
    !> branches of `version_control_git_branch_remotes' as the configuration changes.
    subroutine test_branch_remotes(error)
        use version_control_git_mod, only: version_control_git_branch_remotes, version_control_git_branch_remotes_type
        type(error_type), allocatable, intent(out) :: error
        type(version_control_git_branch_remotes_type) :: remotes
        type(handle_type) :: h, buffer
        type(status_type) :: st

        if (.not. has_git(error)) return
        call committed_file(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call version_control_git_branch_remotes(remotes)
            call check(error,.not. allocated(remotes%upstream) .and. .not. allocated(remotes%push), "none")
            if (allocated(error)) exit body
            call git("config", "branch.main.remote", "origin")
            call version_control_git_branch_remotes(remotes)
            call check(error,.not. allocated(remotes%upstream) .and. .not. allocated(remotes%push), "remote only")
            if (allocated(error)) exit body
            call git("config", "branch.main.merge", "main")
            call version_control_git_branch_remotes(remotes)
            call check(error, allocated(remotes%upstream) .and. .not. allocated(remotes%push), "upstream")
            if (allocated(error)) exit body
            call git("config", "branch.main.pushRemote", "fork")
            call version_control_git_branch_remotes(remotes)
            call check(error, allocated(remotes%upstream) .and. allocated(remotes%push), "push remote")
            if (allocated(error)) exit body
            call check(error, remotes%push, "fork/main")
            if (allocated(error)) exit body
            call git("config", "--unset", "branch.main.pushRemote")
            call git("config", "remote.pushDefault", "fork")
            call version_control_git_branch_remotes(remotes)
            call check(error, allocated(remotes%upstream) .and. allocated(remotes%push), "push default")
            if (allocated(error)) exit body
            call check(error, remotes%push, "fork/main")
            if (allocated(error)) exit body
            call git("config", "remote.pushDefault", "origin")
            call version_control_git_branch_remotes(remotes)
            call check(error, allocated(remotes%upstream) .and. .not. allocated(remotes%push), "push default origin")
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_branch_remotes

    !> C-x v s makes a tag, C-x v b c a branch (from the start point
    !> read, here the default), C-x v b s switches back and C-x v r
    !> retrieves the tag; a tag is not made over modified files when the
    !> user says no.
    subroutine test_tags_and_branches(error)
        use version_control_mod, only: version_control_create_tag, version_control_create_branch, version_control_switch_branch, &
                                       version_control_retrieve_tag
        use version_control_git_mod, only: version_control_git_branches
        use error_mod, only: USER_ERROR_CODE
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer
        type(status_type) :: st
        type(string_type), allocatable :: branches(:)
        character(len=:), allocatable :: tagged

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            call version_control_create_tag(tdir(), "v1", .false., st)
            call check(error, st%ok(), "create tag: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, git_output("tag"), "v1")
            if (allocated(error)) exit body
            tagged = git_output("rev-parse", "v1")
            ! C-x v b c: the start point defaults to the current branch.
            call type_text(achar(13))
            call version_control_create_branch(tdir(), "feature", st)
            call check(error, st%ok(), "create branch: "//format_error(st))
            if (allocated(error)) exit body
            call version_control_git_branches(branches)
            call check(error, allocated(branches(1)%text), "a current branch")
            if (allocated(error)) exit body
            call check(error, branches(1)%text, "feature")
            if (allocated(error)) exit body
            call check(error, size(branches) == 2, "two branches")
            if (allocated(error)) exit body
            call check(error, branches(2)%text, "main")
            if (allocated(error)) exit body
            ! C-x v b s.
            call version_control_switch_branch(tdir(), "main", st)
            call check(error, st%ok(), "switch branch: "//format_error(st))
            if (allocated(error)) exit body
            call version_control_git_branches(branches)
            call check(error, branches(1)%text, "main")
            if (allocated(error)) exit body
            ! A branch needs a name.
            call version_control_retrieve_tag(tdir(), "", st, .true.)
            call check(error, condition_matches(st%code, USER_ERROR_CODE), "branch name required")
            if (allocated(error)) exit body
            ! A new commit, then C-x v r back to the tag.
            call write_file(tdir()//"a.txt", "one"//achar(10)//"four"//achar(10))
            call git("commit", "-q", "-a", "-m", "third")
            call version_control_retrieve_tag(tdir(), "v1", st)
            call check(error, st%ok(), "retrieve tag: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, git_output("rev-parse", "HEAD"), tagged)
            if (allocated(error)) exit body
            ! Modified files: the tag is made only if the user says so.
            call write_file(tdir()//"a.txt", "changed"//achar(10))
            call type_text("n")
            call version_control_create_tag(tdir(), "v2", .false., st)
            call check(error, condition_matches(st%code, USER_ERROR_CODE), "modified files: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, git_output("tag"), "v1")
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_tags_and_branches

    !> C-x v b L logs another branch from its tip; C-x v b l logs the
    !> current file on it.
    subroutine test_branch_logs(error)
        use version_control_mod, only: version_control_print_root_branch_log, version_control_print_fileset_branch_log
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call git("branch", "other")
            call write_file(tdir()//"a.txt", "one"//achar(10)//"four"//achar(10))
            call git("commit", "-q", "-a", "-m", "third")
            call set_buffer(buffer, st)
            call version_control_print_root_branch_log("other", st)
            call check(error, st%ok(), "root branch log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, index(text, "second") > 0 .and. index(text, "third") == 0, text)
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            call version_control_print_fileset_branch_log("main", st)
            call check(error, st%ok(), "fileset branch log: "//format_error(st))
            if (allocated(error)) exit body
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, index(text, "third") > 0 .and. index(text, "init") > 0, text)
            if (allocated(error)) exit body
            call close_buffer(log, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_branch_logs

    !> With a bare remote and a second clone: C-x v O shows the commits
    !> not pushed yet, C-x v P pushes them (in VC-Compilation mode),
    !> C-x v I shows the commits pushed from the other clone and C-x v +
    !> pulls them.
    subroutine test_push_pull(error)
        use version_control_mod, only: version_control_root_log_outgoing, version_control_root_log_incoming, version_control_push, &
                                       version_control_pull
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log, out
        type(status_type) :: st
        character(len=:), allocatable :: text, remote, clone, junk

        if (.not. has_git(error)) return
        remote = file_name_as_directory(fortmacs_wd)//"build/test-vc-remote.git"
        clone = file_name_as_directory(file_name_as_directory(fortmacs_wd)//"build/test-vc-clone")
        if (is_file_directory(remote)) call delete_directory(remote, st, .true.)
        if (is_file_directory(clone)) call delete_directory(clone, st, .true.)
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call git_at(file_name_as_directory(fortmacs_wd), junk, "init", "-q", "--bare", remote)
            call git("remote", "add", "origin", remote)
            call git("push", "-q", "-u", "origin", "main")
            ! An outgoing commit.
            call write_file(tdir()//"a.txt", "one"//achar(10)//"four"//achar(10))
            call git("commit", "-q", "-a", "-m", "third")
            call set_buffer(buffer, st)
            call version_control_root_log_outgoing(st)
            call check(error, st%ok(), "outgoing: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-outgoing*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, index(text, "third") > 0 .and. index(text, "second") == 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(current)%major_mode, "version_control_git_log_view_mode")
            if (allocated(error)) exit body
            call close_buffer(log, st)
            ! C-x v P.
            call set_buffer(buffer, st)
            call version_control_push(.false., st)
            call check(error, st%ok(), "push: "//format_error(st))
            if (allocated(error)) exit body
            out = get_buffer("*vc-git : "//tdir()//"*")
            call check(error, is_buffer_live(out), "the *vc-git* buffer")
            if (allocated(error)) exit body
            call wait_buffer_process(out)
            call set_buffer(out, st)
            text = buffer_string()
            call check(error, index(text, "Running 'git push'...") == 1 .and. index(text, "Finished in ") > 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(current)%major_mode, "version_control_compilation_mode")
            if (allocated(error)) exit body
            call check(error, buffers(current)%mode_name, "VC-Compilation")
            if (allocated(error)) exit body
            call check(error, git_output("rev-parse", "origin/main"), git_output("rev-parse", "HEAD"))
            if (allocated(error)) exit body
            ! Nothing outgoing now.
            call set_buffer(buffer, st)
            call version_control_root_log_outgoing(st)
            log = get_buffer("*vc-outgoing*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, st%ok() .and. index(text, "third") == 0, "after the push: "//format_error(st)//text)
            if (allocated(error)) exit body
            call close_buffer(log, st)
            ! A commit pushed from another clone is incoming.
            call git_at(file_name_as_directory(fortmacs_wd), junk, "clone", "-q", "-b", "main", remote, clone)
            call write_file(clone//"a.txt", "one"//achar(10)//"five"//achar(10))
            call git_at(clone, junk, "commit", "-q", "-a", "-m", "fourth")
            call git_at(clone, junk, "push", "-q")
            call set_buffer(buffer, st)
            call version_control_root_log_incoming(st)
            call check(error, st%ok(), "incoming: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-incoming*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, index(text, "fourth") > 0 .and. index(text, "third") == 0, text)
            if (allocated(error)) exit body
            call close_buffer(log, st)
            ! C-x v +.
            call set_buffer(buffer, st)
            call version_control_pull(.false., st)
            call check(error, st%ok(), "pull: "//format_error(st))
            if (allocated(error)) exit body
            call wait_buffer_process(out)
            call set_buffer(out, st)
            text = buffer_string()
            call check(error, index(text, "Running 'git pull --stat'...") > 0, text)
            if (allocated(error)) exit body
            call check(error, git_output("log", "-1", "--format=%s"), "fourth")
            if (allocated(error)) exit body
            call close_buffer(out, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
        if (is_file_directory(remote)) call delete_directory(remote, st, .true.)
        if (is_file_directory(clone)) call delete_directory(clone, st, .true.)
    end subroutine test_push_pull
end module test_version_control_mod
