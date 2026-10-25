! test_vc_fm.f90 -- tests for vc_hooks_fm, vc_dispatcher_fm, vc_fm and vc_git_fm.
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
module test_vc_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, condition_matches, FM_FILE_ALREADY_EXISTS
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_string, make_list
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, current_buffer, &
                         get_buffer, buffer_live_p, generate_new_buffer_name, get_file_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, point, point_min, goto_char, bobp
    use search_fm, only: re_search_forward, SEARCH_NOERROR
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_exists_p, file_directory_p, &
                         make_directory_internal, write_region, expand_file_name
    use files_fm, only: delete_directory, find_file_noselect, executable_find
    use callproc_fm, only: call_process
    use process_fm, only: make_process, process_arg_type, process_live_p, accept_process_output
    use bindings_fm, only: init_bindings
    use vc_hooks_fm, only: vc_string_type, vc_backend, vc_registered, vc_state, vc_working_revision, &
                           vc_checkout_model, vc_file_clearprops, vc_backends, vc_backend_named, vc_clear_context, &
                           vc_handled_backends, VC_STATE_NIL, VC_UNREGISTERED, VC_ADDED, VC_UP_TO_DATE, VC_EDITED, &
                           VC_MODEL_IMPLICIT
    use vc_dispatcher_fm, only: vc_do_command, vc_exec_after, vc_action_type, VC_DEST_CURRENT, VC_DEST_DISCARD, &
                                VC_OKSTATUS_ASYNC
    use vc_fm, only: vc_fileset_type, vc_register, vc_responsible_backend, vc_rename_file, vc_create_repo, &
                     vc_diff_internal, vc_revert_files
    use vc_git_fm, only: vc_git_parse_program_version
    implicit none(type, external)
    private

    public :: collect_vc_fm

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-vc/"

    !> What the exec-after tests' actions set.
    logical :: success = .false.

contains

    subroutine collect_vc_fm(testsuite)
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
                    new_unittest("vc-test-git00-create-repo", test_create_repo), &
                    new_unittest("vc-test-git01-register", test_register), &
                    new_unittest("vc-test-git02-state", test_state), &
                    new_unittest("vc-test-git03-working-revision", test_working_revision), &
                    new_unittest("vc-test-git04-checkout-model", test_checkout_model), &
                    new_unittest("vc-test-git05-rename-file", test_rename_file), &
                    new_unittest("vc-test-git06-version-diff", test_version_diff), &
                    new_unittest("vc-test-git10-rename-directory", test_rename_directory), &
                    new_unittest("vc-git-mode-line", test_mode_line), &
                    new_unittest("vc-git-diff", test_diff), &
                    new_unittest("vc-git-revert", test_revert) &
                    ]
    end subroutine collect_vc_fm

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A list of the strings given.
    function strings(a1, a2, a3, a4) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4
        type(vc_string_type), allocatable :: list(:)
        integer :: n
        n = 1
        if (present(a2)) n = 2
        if (present(a3)) n = 3
        if (present(a4)) n = 4
        allocate (list(n))
        list(1)%s = a1
        if (present(a2)) list(2)%s = a2
        if (present(a3)) list(3)%s = a3
        if (present(a4)) list(4)%s = a4
    end function strings

    !> A fresh temporary buffer, current.
    subroutine temp_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call get_buffer_create(generate_new_buffer_name(" *vc-test*"), h, st)
        call set_buffer(h, st)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
    end subroutine temp_buffer

    !> The test repository directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    !> Whether git is available (else the test is skipped).
    logical function have_git(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path
        call executable_find("git", path)
        have_git = len(path) > 0
        if (.not. have_git) call skip_test(error, "no git")
    end function have_git

    !> A buffer in a fresh, empty test directory (`default_directory'),
    !> made a Git repository with `vc_create_repo'; only Git is handled.
    subroutine setup(h, status)
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        type(status_type) :: st
        call init_bindings()
        call init_fileio()
        call vc_clear_context()
        vc_handled_backends = [character(len=8) :: "Git"]
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        call make_directory_internal(file_name_as_directory(emacs_wd)//"build", st)
        call make_directory_internal(tdir(), st)
        call temp_buffer(h)
        buffers(cur)%directory = tdir()
        call vc_create_repo(vc_backend_named("Git"), status)
        call set_buffer(h, st)
    end subroutine setup

    !> Remove the test directory and buffer H; handle all backends again.
    subroutine teardown(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        type(handle_type) :: vc
        vc = get_buffer("*vc*")
        if (buffer_live_p(vc)) call kill_buffer(vc, st)
        if (buffer_live_p(h)) call kill_buffer(h, st)
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        vc_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        call vc_clear_context()
    end subroutine teardown

    !> Write TEXT to FILE (write-region ... 'nomessage).
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg
        old = current_buffer()
        call temp_buffer(h)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call kill_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> Run git with ARGS in the test directory (output discarded).
    subroutine git(a1, a2, a3, a4, a5, a6)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6
        type(value_type) :: args(10)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n
        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        args(1) = make_string("-c")
        args(2) = make_string("user.name=t")
        args(3) = make_string("-c")
        args(4) = make_string("user.email=t@t")
        args(5) = make_string(a1)
        n = 5
        if (present(a2)) call push(a2)
        if (present(a3)) call push(a3)
        if (present(a4)) call push(a4)
        if (present(a5)) call push(a5)
        if (present(a6)) call push(a6)
        old = current_buffer()
        call temp_buffer(h)
        buffers(cur)%directory = tdir()
        call call_process("git", result, st, args=make_list(args(1:n)))
        call kill_buffer(h, st)
        call set_buffer(old, st)
    contains
        subroutine push(a)
            character(len=*), intent(in) :: a
            n = n + 1
            args(n) = make_string(a)
        end subroutine push
    end subroutine git

    !> The Git backend's index.
    integer function git_b()
        git_b = vc_backend_named("Git")
    end function git_b

    ! ------------------------------------------------------------------
    ! vc-git-tests.el
    ! ------------------------------------------------------------------

    subroutine test_version_general(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, vc_git_parse_program_version("git version 2.30.1.0"), "2.30.1.0")
    end subroutine test_version_general

    subroutine test_version_windows(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, vc_git_parse_program_version("git version 2.30.1.1.windows.1"), "2.30.1.1")
    end subroutine test_version_windows

    subroutine test_version_apple(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, vc_git_parse_program_version("git version 2.30.1.2 (Apple Git-130)"), "2.30.1.2")
    end subroutine test_version_apple

    subroutine test_version_other(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, vc_git_parse_program_version("git version 2.30.1.3.foo.bar"), "2.30.1.3")
    end subroutine test_version_other

    subroutine test_version_leading_string(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, vc_git_parse_program_version("git version foo.bar.2.30.1.4"), "0")
    end subroutine test_version_leading_string

    subroutine test_version_leading_dot(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, vc_git_parse_program_version("git version .2.30.1.5"), "0")
    end subroutine test_version_leading_dot

    ! ------------------------------------------------------------------
    ! vc-test-misc.el
    ! ------------------------------------------------------------------

    !> Run `vc_do_command' of sh -c "echo foo; echo >&2 bar" in a temporary
    !> buffer: synchronously (or with ASYNC), the output in the buffer
    !> (TO_BUFFER), standard error mixed in (STDERR); gives the buffer's
    !> text.
    function do_command(to_buffer, stderr, async) result(text)
        logical, intent(in) :: to_buffer, stderr
        logical, intent(in), optional :: async
        character(len=:), allocatable :: text
        type(vc_string_type) :: none(0)
        type(handle_type) :: h, proc
        type(status_type) :: st
        logical :: got
        integer :: k, dest, okstatus
        call init_fileio()
        call temp_buffer(h)
        dest = VC_DEST_DISCARD
        if (to_buffer) dest = VC_DEST_CURRENT
        okstatus = 0
        if (present(async)) then
            if (async) okstatus = VC_OKSTATUS_ASYNC
        end if
        call vc_do_command(dest, okstatus, "sh", none, strings("-c", "echo foo; echo >&2 bar"), st, stderr=stderr, &
                           process=proc)
        if (okstatus == VC_OKSTATUS_ASYNC) then
            do k = 1, 600
                if (.not. process_live_p(proc)) exit
                call accept_process_output(got, st, proc, 0.05)
            end do
            call accept_process_output(got, st, seconds=0.05)
        end if
        call set_buffer(h, st)
        text = buffer_string()
        call kill_buffer(h, st)
    end function do_command

    !> Test `vc_do_command' synchronous, discarding stderr.
    subroutine test_do_command_1(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, do_command(.true., .false.), "foo"//achar(10))
    end subroutine test_do_command_1

    !> Test `vc_do_command' synchronous, keeping stderr.
    subroutine test_do_command_2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        text = do_command(.true., .true.)
        call check(error, index(text, "foo") > 0 .and. index(text, "bar") > 0, text)
    end subroutine test_do_command_2

    !> Test `vc_do_command' synchronous, discarding both.
    subroutine test_do_command_3(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, do_command(.false., .true.), "")
    end subroutine test_do_command_3

    !> Test `vc_do_command' asynchronous, keeping stderr.
    subroutine test_do_command_5(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        text = do_command(.true., .true., .true.)
        call check(error, index(text, "foo") > 0 .and. index(text, "bar") > 0, text)
    end subroutine test_do_command_5

    !> Test `vc_do_command' asynchronous, discarding both.
    subroutine test_do_command_6(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, do_command(.false., .true., .true.), "")
    end subroutine test_do_command_6

    !> Test `vc_do_command' setting up the buffer.
    subroutine test_do_command_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(vc_string_type) :: none(0)
        type(handle_type) :: buf, h
        type(status_type) :: st
        call init_fileio()
        call temp_buffer(h)
        call get_buffer_create(generate_new_buffer_name(" *temp*"), buf, st)
        call set_buffer(h, st)
        call vc_do_command(VC_DEST_CURRENT, 0, "sh", none, strings("-c", "echo foo; echo >&2 bar"), st, buffer=buf, &
                           stderr=.false.)
        call set_buffer(buf, st)
        call check(error, buffer_string(), "foo"//achar(10))
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
    end subroutine test_do_command_7

    !> The action of the exec-after tests: set `success'.
    recursive subroutine set_success(action, status)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        status = status_type()
        if (action%n < 0) return
        success = .true.
    end subroutine set_success

    !> Start sh -c COMMAND in buffer H.
    subroutine start_shell(command, h, proc)
        character(len=*), intent(in) :: command
        type(handle_type), intent(in) :: h
        type(handle_type), intent(out) :: proc
        type(process_arg_type) :: args(3)
        type(status_type) :: st
        args(1)%s = "sh"
        args(2)%s = "-c"
        args(3)%s = command
        call make_process("test", args, proc, st, buffer=h)
    end subroutine start_shell

    !> Wait for PROC to exit, its sentinels run (vc-test--exec-after-wait).
    subroutine exec_after_wait(proc)
        type(handle_type), intent(in) :: proc
        type(status_type) :: st
        logical :: got
        integer :: k
        do k = 1, 600
            if (.not. process_live_p(proc)) exit
            call accept_process_output(got, st, proc, 0.05)
        end do
        call accept_process_output(got, st, seconds=0.05)
    end subroutine exec_after_wait

    !> Test `vc_exec_after' adding a sentinel.
    subroutine test_exec_after_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, proc
        type(vc_action_type) :: action
        type(status_type) :: st
        call init_fileio()
        call temp_buffer(h)
        call start_shell("sleep 0.2; echo hello", h, proc)
        success = .false.
        action%run => set_success
        call vc_exec_after(action, st)
        call check(error,.not. success, "not yet")
        if (.not. allocated(error)) then
            call exec_after_wait(proc)
            call check(error, success, "after the process")
        end if
        call kill_buffer(h, st)
    end subroutine test_exec_after_1

    !> Test `vc_exec_after' executing the code immediately.
    subroutine test_exec_after_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, proc
        type(vc_action_type) :: action
        type(status_type) :: st
        call init_fileio()
        call temp_buffer(h)
        call start_shell("sleep 0.2; echo hello", h, proc)
        call exec_after_wait(proc)
        success = .false.
        action%run => set_success
        call vc_exec_after(action, st)
        call check(error, success)
        call kill_buffer(h, st)
    end subroutine test_exec_after_2

    !> Test OKSTATUS of `vc_exec_after'.
    subroutine test_exec_after_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, proc
        type(vc_action_type) :: action
        type(status_type) :: st
        call init_fileio()
        call temp_buffer(h)
        call start_shell("true", h, proc)
        success = .false.
        action%run => set_success
        action%okstatus = 0
        call vc_exec_after(action, st)
        call exec_after_wait(proc)
        call check(error, success, "true")
        call kill_buffer(h, st)
        if (allocated(error)) return
        call temp_buffer(h)
        call start_shell("false", h, proc)
        success = .false.
        call vc_exec_after(action, st)
        call exec_after_wait(proc)
        call check(error,.not. success, "false")
        call kill_buffer(h, st)
    end subroutine test_exec_after_3

    ! ------------------------------------------------------------------
    ! vc-tests.el
    ! ------------------------------------------------------------------

    !> Create a test repository in `default_directory'.
    subroutine test_create_repo(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        if (.not. have_git(error)) return
        call setup(h, st)
        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, file_directory_p(tdir()), "the directory")
            if (allocated(error)) exit body
            call check(error, vc_responsible_backend(tdir(), st) == git_b(), "responsible backend")
        end block body
        call teardown(h)
    end subroutine test_create_repo

    !> Register and unregister a file.
    !> This checks also `vc_backend' and `vc_responsible_backend'.
    subroutine test_register(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name1, tmp_name2
        if (.not. have_git(error)) return
        call setup(h, st)
        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            ! For file oriented backends CVS, RCS and SVN the backend is
            ! returned, and the directory is registered already.
            call check(error,.not. vc_registered(tdir()), "directory not registered")
            if (allocated(error)) exit body
            call check(error, vc_responsible_backend(tdir(), st) == git_b(), "responsible backend")
            if (allocated(error)) exit body
            tmp_name1 = tdir()//"foo"
            tmp_name2 = "bla"
            ! Register files.  Check for it.
            call write_file(tmp_name1, "foo")
            call check(error, file_exists_p(tmp_name1) .and. vc_backend(tmp_name1) == 0 .and. &
                       vc_responsible_backend(tmp_name1, st) == git_b() .and. .not. vc_registered(tmp_name1), "foo")
            if (allocated(error)) exit body
            call write_file(tdir()//tmp_name2, "bla")
            call check(error, vc_backend(tmp_name2) == 0 .and. vc_responsible_backend(tmp_name2, st) == git_b() .and. &
                       .not. vc_registered(tmp_name2), "bla")
            if (allocated(error)) exit body
            fs%backend = git_b()
            fs%files = strings(tmp_name1, tmp_name2)
            call vc_register(st, fs)
            call check(error, st%ok(), "register: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, vc_backend(tmp_name1) == git_b() .and. vc_registered(tmp_name1), "foo registered")
            if (allocated(error)) exit body
            call check(error, vc_backend(tmp_name2) == git_b() .and. vc_registered(tmp_name2), "bla registered")
            if (allocated(error)) exit body
            ! Unregister the files.
            call vc_backends(git_b())%unregister(tmp_name1, st)
            call vc_file_clearprops(tmp_name1)
            call check(error, vc_backend(tmp_name1) == 0 .and. .not. vc_registered(tmp_name1), "foo unregistered")
            if (allocated(error)) exit body
            call vc_backends(git_b())%unregister(tmp_name2, st)
            call vc_file_clearprops(tmp_name2)
            call check(error, vc_backend(tmp_name2) == 0 .and. .not. vc_registered(tmp_name2), "bla unregistered")
            if (allocated(error)) exit body
            ! The files should still exist.
            call check(error, file_exists_p(tmp_name1) .and. file_exists_p(tdir()//tmp_name2), "still there")
        end block body
        call teardown(h)
    end subroutine test_register

    !> Check the different states of a file.
    subroutine test_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name
        integer :: s
        if (.not. have_git(error)) return
        call setup(h, st)
        body: block
            tmp_name = tdir()//"foo"
            ! Check state of a nonexistent file.
            call check(error, vc_state(tmp_name) == VC_STATE_NIL, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check state.
            call write_file(tmp_name, "foo")
            s = vc_state(tmp_name, git_b())
            call check(error, s == VC_STATE_NIL .or. s == VC_UNREGISTERED, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check state.
            fs%backend = git_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            s = vc_state(tmp_name)
            call check(error, s == VC_STATE_NIL .or. s == VC_ADDED .or. s == VC_UP_TO_DATE, "registered")
            if (allocated(error)) exit body
            ! Unregister the file.  Check state.
            call vc_backends(git_b())%unregister(tmp_name, st)
            call vc_file_clearprops(tmp_name)
            s = vc_state(tmp_name, git_b())
            call check(error, s == VC_STATE_NIL .or. s == VC_UNREGISTERED, "unregistered")
        end block body
        call teardown(h)
    end subroutine test_state

    !> Check the working revision of a repository.
    subroutine test_working_revision(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name, rev
        logical :: found
        if (.not. have_git(error)) return
        call setup(h, st)
        body: block
            call vc_working_revision(tdir(), rev, found)
            call check(error,.not. found .or. rev == "0", "repository")
            if (allocated(error)) exit body
            tmp_name = tdir()//"foo"
            ! Check initial working revision, should be nil until it's
            ! registered.
            call vc_working_revision(tmp_name, rev, found)
            call check(error,.not. found, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check working revision.
            call write_file(tmp_name, "foo")
            call vc_working_revision(tmp_name, rev, found)
            call check(error,.not. found, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check working revision.
            fs%backend = git_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            call vc_working_revision(tmp_name, rev, found)
            ! nil is fine, at least in Git's case, because `vc_register'
            ! only makes the file `added' in this case.
            call check(error,.not. found, "registered")
            if (allocated(error)) exit body
            ! Unregister the file.  Check working revision.
            call vc_backends(git_b())%unregister(tmp_name, st)
            call vc_file_clearprops(tmp_name)
            call vc_working_revision(tmp_name, rev, found)
            call check(error,.not. found, "unregistered")
        end block body
        call teardown(h)
    end subroutine test_working_revision

    !> Check the checkout model of a repository.
    subroutine test_checkout_model(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name
        if (.not. have_git(error)) return
        call setup(h, st)
        body: block
            call check(error, vc_checkout_model(git_b(), strings(tdir())) == VC_MODEL_IMPLICIT, "repository")
            if (allocated(error)) exit body
            tmp_name = tdir()//"foo"
            call check(error, vc_checkout_model(git_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "nonexistent")
            if (allocated(error)) exit body
            call write_file(tmp_name, "foo")
            call check(error, vc_checkout_model(git_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "new file")
            if (allocated(error)) exit body
            fs%backend = git_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            call check(error, vc_checkout_model(git_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "registered")
            if (allocated(error)) exit body
            call vc_backends(git_b())%unregister(tmp_name, st)
            call vc_file_clearprops(tmp_name)
            call check(error, vc_checkout_model(git_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "unregistered")
        end block body
        call teardown(h)
    end subroutine test_checkout_model

    !> Check the rename-file action.
    subroutine test_rename_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name, new_name, new_dir, saved
        if (.not. have_git(error)) return
        call setup(h, st)
        body: block
            tmp_name = tdir()//"foo"
            new_name = tdir()//"bar"
            ! Write a new file.
            call write_file(tmp_name, "foo")
            ! Register it.  Renaming can fail otherwise.
            fs%backend = git_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            ! Test that `vc_rename_file' isn't affected by
            ! `default_directory' except for the meaning of OLD and NEW if
            ! they are relative file names.
            saved = buffers(cur)%directory
            buffers(cur)%directory = file_name_as_directory(emacs_wd)//"build/"
            call vc_rename_file("test-vc/foo", "test-vc/bar", st)
            buffers(cur)%directory = saved
            call check(error, st%ok(), "rename: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tmp_name) .and. file_exists_p(new_name), "renamed")
            if (allocated(error)) exit body
            call check(error, vc_state(new_name) == VC_ADDED, "added")
            if (allocated(error)) exit body
            ! Test OK-IF-ALREADY-EXISTS.
            tmp_name = tdir()//"qux"
            new_name = tdir()//"quuux"
            call write_file(tmp_name, "qux")
            call write_file(new_name, "quuux")
            fs%files = strings("qux", "quuux")
            call vc_register(st, fs)
            call vc_rename_file(tmp_name, new_name, st)
            call check(error, condition_matches(st%code, FM_FILE_ALREADY_EXISTS), "file-already-exists")
            if (allocated(error)) exit body
            call vc_rename_file(tmp_name, new_name, st, ok_if_already_exists=.true.)
            call check(error, st%ok(), "ok-if-already-exists: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tmp_name) .and. file_exists_p(new_name), "replaced")
            if (allocated(error)) exit body
            ! Test moving into an existing directory.
            tmp_name = tdir()//"quux"
            new_dir = tdir()//"dir1/"
            new_name = tdir()//"dir1/quux"
            call make_directory_internal(new_dir, st)
            call write_file(tmp_name, "quux")
            fs%files = strings("dir1/", "quux")
            call vc_register(st, fs)
            call vc_rename_file(tmp_name, new_dir, st)
            call check(error, st%ok(), "into a directory: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tmp_name) .and. file_exists_p(new_name), "moved")
        end block body
        call teardown(h)
    end subroutine test_rename_file

    !> Check the diff version of a repository.
    subroutine test_version_diff(error)
        use vc_fm, only: vc_checkin, vc_version_diff
        use log_edit_fm, only: log_edit_done
        use sysdep_fm, only: sys_setenv, sys_unsetenv
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: names(3) = [character(len=18) :: "EMAIL", "GIT_AUTHOR_NAME", &
                                                                                                               "GIT_COMMITTER_NAME"]
        character(len=*), parameter :: values(3) = [character(len=20) :: "joh.doe@example.com", "A", "C"]
        type(handle_type) :: h, buff, diff
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=256) :: saved(3)
        integer :: saved_status(3), k, err
        character(len=:), allocatable :: tmp_name, rawtext
        if (.not. have_git(error)) return
        ! vc-test--with-author-identity: git tries various approaches to
        ! guess a user name and email, which can fail depending on how the
        ! system is configured.
        do k = 1, 3
            call get_environment_variable(trim(names(k)), saved(k), status=saved_status(k))
            err = sys_setenv(trim(names(k)), trim(values(k)))

        end do
        call setup(h, st)
        body: block
            tmp_name = tdir()//"foo"
            fs%backend = git_b()
            fs%files = strings("foo")
            ! Write and register a new file.
            call write_file(tmp_name, "originaltext")
            call vc_register(st, fs)
            call find_file_noselect(tmp_name, buff, st)
            call set_buffer(buff, st)
            ! Checkin file.
            call vc_checkin(fs%files, fs%backend, st)
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
            call write_file(tmp_name, "updatedtext")
            ! Check version diff.
            call vc_version_diff("", "", st)
            call check(error, st%ok(), "version-diff: "//format_error(st))
            if (allocated(error)) exit body
            diff = get_buffer("*vc-diff*")
            call check(error, buffer_live_p(diff), "*vc-diff*")
            if (allocated(error)) exit body
            call set_buffer(diff, st)
            rawtext = buffer_string()
            call set_buffer(h, st)
            call check(error, index(rawtext, "-originaltext") > 0, "-originaltext in: "//rawtext)
            if (allocated(error)) exit body
            call check(error, index(rawtext, "+updatedtext") > 0, "+updatedtext in: "//rawtext)
        end block body
        diff = get_buffer("*vc-diff*")
        if (buffer_live_p(diff)) call kill_buffer(diff, st)
        diff = get_buffer("*vc-log*")
        if (buffer_live_p(diff)) call kill_buffer(diff, st)
        if (buffer_live_p(buff)) call kill_buffer(buff, st)
        call teardown(h)
        do k = 1, 3
            if (saved_status(k) == 0) then
                err = sys_setenv(trim(names(k)), trim(saved(k)))
            else
                err = sys_unsetenv(trim(names(k)))
            end if
        end do
    end subroutine test_version_diff

    !> Check the rename-file action for directories.
    subroutine test_rename_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_dir, new_dir
        if (.not. have_git(error)) return
        call setup(h, st)
        fs%backend = git_b()
        body: block
            ! Test mix of registered and unregistered files.
            tmp_dir = tdir()//"dir1/"
            new_dir = tdir()//"dir2/"
            call make_directory_internal(tmp_dir, st)
            call write_file(tmp_dir//"foo", "foo")
            call write_file(tmp_dir//"bar", "bar")
            ! Register TMP-NAME1 but *not* TMP-NAME2.
            fs%files = strings("dir1/foo")
            call vc_register(st, fs)
            call vc_rename_file(tdir()//"dir1", tdir()//"dir2", st)
            call check(error, st%ok(), "mixed: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tmp_dir//"foo") .and. .not. file_exists_p(tmp_dir//"bar") .and. &
                       file_exists_p(new_dir//"foo") .and. vc_registered(new_dir//"foo") .and. &
                       file_exists_p(new_dir//"bar") .and. .not. vc_registered(new_dir//"bar"), "mixed")
            if (allocated(error)) exit body
            ! Test only unregistered files.
            tmp_dir = tdir()//"dir3/"
            new_dir = tdir()//"dir4/"
            call make_directory_internal(tmp_dir, st)
            call write_file(tmp_dir//"foo", "foo")
            call vc_rename_file(tdir()//"dir3", tdir()//"dir4", st)
            call check(error, st%ok(), "unregistered: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tmp_dir//"foo") .and. file_exists_p(new_dir//"foo") .and. &
                       .not. vc_registered(new_dir//"foo"), "unregistered")
            if (allocated(error)) exit body
            ! Test only registered files.
            tmp_dir = tdir()//"dir5/"
            new_dir = tdir()//"dir6/"
            call make_directory_internal(tmp_dir, st)
            call write_file(tmp_dir//"foo", "foo")
            fs%files = strings("dir5/foo")
            call vc_register(st, fs)
            call vc_rename_file(tdir()//"dir5", tdir()//"dir6", st)
            call check(error, st%ok(), "registered: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tmp_dir//"foo") .and. file_exists_p(new_dir//"foo") .and. &
                       vc_registered(new_dir//"foo"), "registered")
        end block body
        call teardown(h)
    end subroutine test_rename_directory

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> A committed file a.txt ("one\ntwo\n") on branch main, visited; its
    !> buffer is current and shown alone.
    subroutine committed_file(h, buf, status)
        type(handle_type), intent(out) :: h, buf
        type(status_type), intent(out) :: status
        call setup(h, status)
        call git("checkout", "-q", "-b", "main")
        call write_file(tdir()//"a.txt", "one"//achar(10)//"two"//achar(10))
        call git("add", "a.txt")
        call git("commit", "-q", "-m", "init")
        call find_file_noselect(tdir()//"a.txt", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        call show_alone()
    end subroutine committed_file

    !> Visiting a file under Git shows Git-BRANCH in the mode line, and
    !> Git:BRANCH once it is edited and saved.
    subroutine test_mode_line(error)
        use files_fm, only: save_buffer
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        if (.not. have_git(error)) return
        call committed_file(h, buf, st)
        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, allocated(buffers(cur)%vc_mode), "vc_mode set")
            if (allocated(error)) exit body
            call check(error, buffers(cur)%vc_mode, " Git-main")
            if (allocated(error)) exit body
            call check(error, buffers(cur)%backup_inhibited, "backup-inhibited")
            if (allocated(error)) exit body
            call goto_char(point_min())
            call insert("X", st)
            call save_buffer(1_int64, .false., st)
            call check(error, st%ok(), "save: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, vc_state(buffers(cur)%filename) == VC_EDITED, "edited")
            if (allocated(error)) exit body
            call check(error, buffers(cur)%vc_mode, " Git:main")
        end block body
        if (buffer_live_p(buf)) call kill_buffer(buf, st)
        call teardown(h)
    end subroutine test_mode_line

    !> `vc_diff_internal' of an edited file gives its diff in *vc-diff*, in
    !> Diff mode; none for an unchanged one.
    subroutine test_diff(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf, d
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: text
        logical :: changes
        if (.not. have_git(error)) return
        call committed_file(h, buf, st)
        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            fs%backend = git_b()
            fs%files = strings(tdir()//"a.txt")
            call vc_diff_internal(.false., fs, changes, st)
            call check(error, st%ok() .and. .not. changes, "unchanged: "//format_error(st))
            if (allocated(error)) exit body
            call write_file(tdir()//"a.txt", "one"//achar(10)//"three"//achar(10))
            call set_buffer(buf, st)
            call vc_diff_internal(.false., fs, changes, st)
            call check(error, st%ok() .and. changes, "changed: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, "-two"//achar(10)//"+three"//achar(10)) > 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(cur)%major_mode, "diff_mode")
            if (allocated(error)) exit body
            call kill_buffer(d, st)
        end block body
        if (buffer_live_p(buf)) call kill_buffer(buf, st)
        call teardown(h)
    end subroutine test_diff

    !> `vc_revert_files' puts back the committed text of a file.
    subroutine test_revert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf, chk
        type(status_type) :: st
        if (.not. have_git(error)) return
        call committed_file(h, buf, st)
        body: block
            call write_file(tdir()//"a.txt", "changed"//achar(10))
            call vc_revert_files(git_b(), strings(tdir()//"a.txt"), st)
            call check(error, st%ok(), "revert: "//format_error(st))
            if (allocated(error)) exit body
            ! Visit the file afresh: reverting its buffer is not ported.
            call kill_buffer(buf, st)
            call find_file_noselect(tdir()//"a.txt", chk, st)
            call set_buffer(chk, st)
            call check(error, buffer_string(), "one"//achar(10)//"two"//achar(10))
            call kill_buffer(chk, st)
            call set_buffer(h, st)
            if (allocated(error)) exit body
            call check(error, vc_state(tdir()//"a.txt") == VC_UP_TO_DATE, "up to date")
        end block body
        call teardown(h)
    end subroutine test_revert
end module test_vc_fm
