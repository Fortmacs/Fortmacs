! test_vc_svn_mod.f90 -- tests for vc_svn_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/vc-tests/vc-tests.el in part: its
! backend-generic tests, run for SVN.
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

!> Ports of vc-tests.el for SVN (vc-test-svn00-create-repo to
!> -svn06-version-diff; upstream skips -svn10-rename-directory, and the
!> working-tree and patch tests are Git and Hg only); own tests of
!> parsing `svn status', the previous revision, C-x v l and C-x v =.
!> The tests that run svn are skipped without svn and svnadmin (there is
!> no vc-svn-tests.el upstream).
module test_vc_svn_mod
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, condition_matches, FM_FILE_ALREADY_EXISTS
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, current_buffer, get_buffer, &
                          is_buffer_live, generate_new_buffer_name
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string
    use fileio_mod, only: init_fileio, emacs_wd, file_name_as_directory, file_exists, is_file_directory, &
                          create_directory_internal, write_region
    use files_mod, only: delete_directory, find_file_noselect, executable_find
    use callproc_mod, only: execute_process
    use bindings_mod, only: init_bindings
    use vc_hooks_mod, only: vc_string_type, vc_backend, vc_registered, vc_state, vc_working_revision, &
                            vc_checkout_model, vc_file_clearprops, vc_file_get_state, vc_file_get_working_revision, &
                            vc_backends, vc_backend_named, vc_clear_context, vc_handled_backends, VC_STATE_UNKNOWN, &
                            VC_UNREGISTERED, VC_ADDED, VC_UP_TO_DATE, VC_EDITED, VC_CONFLICT, &
                            VC_REMOVED, VC_MODEL_IMPLICIT
    use vc_mod, only: vc_fileset_type, vc_register, vc_responsible_backend, vc_rename_file, vc_create_repo
    use vc_svn_mod, only: vc_svn_parse_status, vc_svn_previous_revision
    implicit none(type, external)
    private

    public :: collect_vc_svn_mod

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-vc-svn/"

    character, parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_vc_svn_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("vc-svn-parse-status", test_parse_status), &
                    new_unittest("vc-svn-previous-revision", test_previous_revision), &
                    new_unittest("vc-test-svn00-create-repo", test_create_repo), &
                    new_unittest("vc-test-svn01-register", test_register), &
                    new_unittest("vc-test-svn02-state", test_state), &
                    new_unittest("vc-test-svn03-working-revision", test_working_revision), &
                    new_unittest("vc-test-svn04-checkout-model", test_checkout_model), &
                    new_unittest("vc-test-svn05-rename-file", test_rename_file), &
                    new_unittest("vc-test-svn06-version-diff", test_version_diff), &
                    new_unittest("vc-svn-print-log", test_print_log), &
                    new_unittest("vc-svn-diff", test_diff), &
                    new_unittest("vc-dir-svn", test_vc_dir) &
                    ]
    end subroutine collect_vc_svn_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A list of the strings given.
    function strings(a1, a2) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2
        type(vc_string_type), allocatable :: list(:)

        if (present(a2)) then
            allocate (list(2))
            list(2)%s = a2
        else
            allocate (list(1))
        end if

        list(1)%s = a1
    end function strings

    !> A fresh temporary buffer, current.
    subroutine temp_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(generate_new_buffer_name(" *vc-test*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = file_name_as_directory(emacs_wd)
    end subroutine temp_buffer

    !> The test working copy directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    !> Whether svn and svnadmin are available (else the test is skipped;
    !> vc-test--svn-enabled).
    logical function has_svn(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        call executable_find("svnadmin", path)
        has_svn = len(path) > 0

        if (has_svn) then
            call executable_find("svn", path)
            has_svn = len(path) > 0
        end if

        if (.not. has_svn) call skip_test(error, "no svn")
    end function has_svn

    !> The SVN backend's index.
    integer function svn_b()
        svn_b = vc_backend_named("SVN")
    end function svn_b

    !> A buffer in a fresh, empty test directory (`default_directory'),
    !> made an SVN working copy with `vc_create_repo'; only SVN is handled.
    subroutine setup(h, status)
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call vc_clear_context()
        vc_handled_backends = [character(len=8) :: "SVN"]
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(file_name_as_directory(emacs_wd)//"build", st)
        call create_directory_internal(tdir(), st)
        call temp_buffer(h)
        buffers(current)%directory = tdir()
        call vc_create_repo(svn_b(), status)
        call set_buffer(h, st)
    end subroutine setup

    !> Remove the test directory and buffer H; handle all backends again.
    subroutine teardown(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        type(handle_type) :: vc

        vc = get_buffer("*vc*")
        if (is_buffer_live(vc)) call close_buffer(vc, st)
        if (is_buffer_live(h)) call close_buffer(h, st)
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
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
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> Run svn with ARGS in the test directory.  A failure is reported on
    !> standard error with svn's output.
    subroutine svn(a1, a2, a3, a4)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4
        type(value_type) :: args(5)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n

        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        args(1) = value_type(text="--non-interactive")
        args(2) = value_type(text=a1)
        n = 2
        if (present(a2)) call append(a2)
        if (present(a3)) call append(a3)
        if (present(a4)) call append(a4)
        old = current_buffer()
        call temp_buffer(h)
        buffers(current)%directory = tdir()
        call execute_process("svn", result, st, destination=value_type(.true.), args=make_list(args(1:n)))

        if (.not. st%ok()) then
            write (error_unit, "(a)") "svn "//a1//": "//format_error(st)
        else if (vtag(result) == V_INTEGER) then
            if (xfixnum(result) /= 0) write (error_unit, "(a)") "svn "//a1//" failed: "//buffer_string()
        end if

        call close_buffer(h, st)
        call set_buffer(old, st)

    contains

        subroutine append(a)
            character(len=*), intent(in) :: a

            n = n + 1
            args(n) = value_type(text=a)
        end subroutine append
    end subroutine svn

    ! ------------------------------------------------------------------
    ! Without svn
    ! ------------------------------------------------------------------

    !> `vc_svn_parse_status' sets the state and working revision of each
    !> file of `svn status -v' output.
    subroutine test_parse_status(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: dir, rev
        logical :: found
        integer :: s

        call init_bindings()
        call init_fileio()
        call vc_clear_context()
        call temp_buffer(h)
        dir = "/nonexistent/wc/"
        buffers(current)%directory = dir
        call insert("                 3        2 alice        ."//LF// &
                    "M                3        3 bob          edited.txt"//LF// &
                    "A                0        ?  ?           new.txt"//LF// &
                    "C                3        3 bob          clash.txt"//LF// &
                    "D                3        2 alice        gone.txt"//LF// &
                    "?                                        stray.txt"//LF, st)

        body: block
            s = vc_svn_parse_status(dir)
            ! The first line is FILENAME's.
            call check(error, s == VC_UP_TO_DATE, "the directory")
            if (allocated(error)) exit body
            call vc_file_get_working_revision(dir, rev, found)
            call check(error, found .and. rev == "2", "the last-modified revision: "//rev)
            if (allocated(error)) exit body
            call check(error, vc_file_get_state(dir//"edited.txt") == VC_EDITED, "edited")
            if (allocated(error)) exit body
            call vc_file_get_working_revision(dir//"edited.txt", rev, found)
            call check(error, found .and. rev == "3", "edited revision: "//rev)
            if (allocated(error)) exit body
            call check(error, vc_file_get_state(dir//"new.txt") == VC_ADDED, "added")
            if (allocated(error)) exit body
            call vc_file_get_working_revision(dir//"new.txt", rev, found)
            call check(error, found .and. rev == "0", "added revision: "//rev)
            if (allocated(error)) exit body
            call check(error, vc_file_get_state(dir//"clash.txt") == VC_CONFLICT, "conflict")
            if (allocated(error)) exit body
            call check(error, vc_file_get_state(dir//"gone.txt") == VC_REMOVED, "removed")
            if (allocated(error)) exit body
            call check(error, vc_file_get_state(dir//"stray.txt") == VC_UNREGISTERED, "unregistered")
        end block body

        call close_buffer(h, st)
        call vc_clear_context()
    end subroutine test_parse_status

    !> The previous revision of an SVN revision is one less, none before 1.
    subroutine test_previous_revision(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: prev
        logical :: found

        call vc_svn_previous_revision("f", "12", prev, found)
        call check(error, found .and. prev == "11", "12: "//prev)
        if (allocated(error)) return
        call vc_svn_previous_revision("f", "1", prev, found)
        call check(error,.not. found, "1")
    end subroutine test_previous_revision

    ! ------------------------------------------------------------------
    ! vc-tests.el
    ! ------------------------------------------------------------------

    !> Create a test repository in `default_directory'.
    subroutine test_create_repo(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, is_file_directory(tdir()//".svn"), "the working copy")
            if (allocated(error)) exit body
            call check(error, vc_responsible_backend(tdir(), st) == svn_b(), "responsible backend")
        end block body

        call teardown(h)
    end subroutine test_create_repo

    !> Register a file (SVN does not support unregistering).
    !> This checks also `vc_backend' and `vc_responsible_backend'.
    subroutine test_register(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name1, tmp_name2

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body

            ! For file oriented backends CVS, RCS and SVN the backend is
            ! returned, and the directory is registered already.
            if (vc_backend(tdir()) /= 0) then
                call check(error, vc_registered(tdir()), "directory registered")
            else
                call check(error,.not. vc_registered(tdir()), "directory not registered")
            end if

            if (allocated(error)) exit body
            call check(error, vc_responsible_backend(tdir(), st) == svn_b(), "responsible backend")
            if (allocated(error)) exit body
            tmp_name1 = tdir()//"foo"
            tmp_name2 = "bla"
            ! Register files.  Check for it.
            call write_file(tmp_name1, "foo")
            call check(error, file_exists(tmp_name1) .and. vc_backend(tmp_name1) == 0 .and. &
                       vc_responsible_backend(tmp_name1, st) == svn_b() .and. .not. vc_registered(tmp_name1), "foo")
            if (allocated(error)) exit body
            call write_file(tdir()//tmp_name2, "bla")
            call check(error, vc_backend(tmp_name2) == 0 .and. vc_responsible_backend(tmp_name2, st) == svn_b() .and. &
                       .not. vc_registered(tmp_name2), "bla")
            if (allocated(error)) exit body
            fs%backend = svn_b()
            fs%files = strings(tmp_name1, tmp_name2)
            call vc_register(st, fs)
            call check(error, st%ok(), "register: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, vc_backend(tmp_name1) == svn_b() .and. vc_registered(tmp_name1), "foo registered")
            if (allocated(error)) exit body
            call check(error, vc_backend(tmp_name2) == svn_b() .and. vc_registered(tmp_name2), "bla registered")
            if (allocated(error)) exit body
            ! Unregistering is not supported: `vc_not_supported'.
            call check(error,.not. associated(vc_backends(svn_b())%unregister), "no unregister")
            if (allocated(error)) exit body
            ! The files should still exist.
            call check(error, file_exists(tmp_name1) .and. file_exists(tdir()//tmp_name2), "still there")
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

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            tmp_name = tdir()//"foo"
            ! Check state of a nonexistent file.
            call check(error, vc_state(tmp_name) == VC_STATE_UNKNOWN, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check state.
            call write_file(tmp_name, "foo")
            s = vc_state(tmp_name, svn_b())
            call check(error, s == VC_STATE_UNKNOWN .or. s == VC_UNREGISTERED, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check state.
            fs%backend = svn_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            s = vc_state(tmp_name)
            call check(error, s == VC_STATE_UNKNOWN .or. s == VC_ADDED .or. s == VC_UP_TO_DATE, "registered")
            ! Unregistering is not supported.
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

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            ! "0": SVN
            call vc_working_revision(tdir(), rev, found)
            call check(error,.not. found .or. rev == "0", "repository: "//rev)
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
            fs%backend = svn_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            call vc_working_revision(tmp_name, rev, found)
            ! "0": Bzr CVS Hg SRC SVN
            call check(error,.not. found .or. rev == "0", "registered: "//rev)
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

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            call check(error, vc_checkout_model(svn_b(), strings(tdir())) == VC_MODEL_IMPLICIT, "repository")
            if (allocated(error)) exit body
            tmp_name = tdir()//"foo"
            call check(error, vc_checkout_model(svn_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "nonexistent")
            if (allocated(error)) exit body
            call write_file(tmp_name, "foo")
            call check(error, vc_checkout_model(svn_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "new file")
            if (allocated(error)) exit body
            fs%backend = svn_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            call check(error, vc_checkout_model(svn_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "registered")
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

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            tmp_name = tdir()//"foo"
            new_name = tdir()//"bar"
            ! Write a new file.
            call write_file(tmp_name, "foo")
            ! Register it.  Renaming can fail otherwise.
            fs%backend = svn_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            ! Test that `vc_rename_file' isn't affected by
            ! `default_directory' except for the meaning of OLD and NEW if
            ! they are relative file names.
            saved = buffers(current)%directory
            buffers(current)%directory = file_name_as_directory(emacs_wd)//"build/"
            call vc_rename_file("test-vc-svn/foo", "test-vc-svn/bar", st)
            buffers(current)%directory = saved
            call check(error, st%ok(), "rename: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tmp_name) .and. file_exists(new_name), "renamed")
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
            ! OK-IF-ALREADY-EXISTS deletes NEW with `vc_delete_file'; `svn
            ! remove' refuses an added file without --force (as upstream's
            ! vc-svn-delete-file does), so that an error condition occurs.
            call vc_rename_file(tmp_name, new_name, st, ok_if_already_exists=.true.)
            call check(error,.not. st%ok() .and. file_exists(tmp_name) .and. file_exists(new_name), "not replaced")
            if (allocated(error)) exit body
            ! Test moving into an existing directory.
            tmp_name = tdir()//"quux"
            new_dir = tdir()//"dir1/"
            new_name = tdir()//"dir1/quux"
            call create_directory_internal(new_dir, st)
            call write_file(tmp_name, "quux")
            fs%files = strings("dir1/", "quux")
            call vc_register(st, fs)
            call vc_rename_file(tmp_name, new_dir, st)
            call check(error, st%ok(), "into a directory: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tmp_name) .and. file_exists(new_name), "moved")
        end block body

        call teardown(h)
    end subroutine test_rename_file

    !> Check the diff version of a repository.
    subroutine test_version_diff(error)
        use vc_mod, only: vc_checkin, vc_version_diff
        use log_edit_mod, only: log_edit_done
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buff, diff
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_name, rawtext

        if (.not. has_svn(error)) return
        call setup(h, st)

        body: block
            tmp_name = tdir()//"foo"
            fs%backend = svn_b()
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
        if (is_buffer_live(buff)) call close_buffer(buff, st)
        call teardown(h)
    end subroutine test_version_diff

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> A working copy whose a.txt has two commits ("init" and "second"),
    !> updated, visited in BUFFER (current, shown alone).
    subroutine two_commits(h, buffer, status)
        type(handle_type), intent(out) :: h, buffer
        type(status_type), intent(out) :: status

        call setup(h, status)
        if (.not. status%ok()) return
        call write_file(tdir()//"a.txt", "one"//LF//"two"//LF)
        call svn("add", "a.txt")
        call svn("commit", "-m", "init")
        call write_file(tdir()//"a.txt", "one"//LF//"three"//LF)
        call svn("commit", "-m", "second")
        call svn("update")
        call find_file_noselect(tdir()//"a.txt", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        call show_alone()
    end subroutine two_commits

    !> Wait for the process of BUFFER to finish (its `vc_exec_after'
    !> actions run).
    subroutine wait_buffer_process(buffer)
        use process_mod, only: get_buffer_process, is_process_live, accept_process_output
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

    !> C-x v l in a file under SVN shows its log in SVN-Log-View mode, with
    !> both revisions, newest first, where n and p move between them.
    subroutine test_print_log(error)
        use vc_mod, only: vc_print_log
        use log_view_mod, only: log_view_msg_next, log_view_current_tag
        use editfns_mod, only: point_min, goto_char
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log
        type(status_type) :: st
        character(len=:), allocatable :: text, tag
        logical :: has

        if (.not. has_svn(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, allocated(buffers(current)%vc_mode), "vc_mode set")
            if (allocated(error)) exit body
            call check(error, buffers(current)%vc_mode, " SVN-2")
            if (allocated(error)) exit body
            call vc_print_log(st, "", 500_int64)
            call check(error, st%ok(), "vc-print-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call check(error, is_buffer_live(log), "*vc-change-log*")
            if (allocated(error)) exit body
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, buffers(current)%major_mode, "vc_svn_log_view_mode")
            if (allocated(error)) exit body
            call check(error, index(text, "Working file: ") == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "second") > 0 .and. index(text, "init") > index(text, "second"), text)
            if (allocated(error)) exit body
            call goto_char(point_min())
            call log_view_msg_next(st)
            call log_view_current_tag(tag, has)
            call check(error, st%ok() .and. has .and. tag == "2", "n: "//tag)
            if (allocated(error)) exit body
            call log_view_msg_next(st)
            call log_view_current_tag(tag, has)
            call check(error, st%ok() .and. has .and. tag == "1", "n again: "//tag)
            if (allocated(error)) exit body
            call close_buffer(log, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_print_log

    !> C-x v = of an edited file under SVN gives its diff in *vc-diff*, in
    !> Diff mode; none for an unchanged one.
    subroutine test_diff(error)
        use vc_mod, only: vc_diff
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, d
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (.not. has_svn(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call vc_diff(st)
            call check(error, st%ok(), "unchanged: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")

            if (is_buffer_live(d)) then
                call set_buffer(d, st)
                text = buffer_string()
                call check(error, len(text) == 0, "no diff: "//text)
                if (allocated(error)) exit body
            end if

            call write_file(tdir()//"a.txt", "one"//LF//"four"//LF)
            call set_buffer(buffer, st)
            call vc_diff(st)
            call check(error, st%ok(), "changed: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")
            call wait_buffer_process(d)
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, "-three"//LF//"+four"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, buffers(current)%major_mode, "diff_mode")
            if (allocated(error)) exit body
            call close_buffer(d, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_diff

    !> C-x v d in an SVN working copy lists its edited and unregistered
    !> files under the Repository header.
    subroutine test_vc_dir(error)
        use vc_dir_mod, only: vc_dir, vc_dir_busy
        use process_mod, only: accept_process_output
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, dir
        type(status_type) :: st
        character(len=:), allocatable :: text
        logical :: got, busy
        integer :: k

        if (.not. has_svn(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call write_file(tdir()//"a.txt", "one"//LF//"four"//LF)
            call write_file(tdir()//"b.txt", "b"//LF)
            call set_buffer(h, st)
            call vc_dir(tdir(), st)
            call check(error, st%ok(), "vc-dir: "//format_error(st))
            if (allocated(error)) exit body
            dir = get_buffer("*vc-dir*")
            call check(error, is_buffer_live(dir), "*vc-dir*")
            if (allocated(error)) exit body

            do k = 1, 600
                call set_buffer(dir, st)
                busy = vc_dir_busy()
                if (.not. busy) exit
                call accept_process_output(got, st, seconds=0.05)
            end do

            call accept_process_output(got, st, seconds=0.05)
            call set_buffer(dir, st)
            text = buffer_string()
            call check(error, index(text, "VC backend : SVN"//LF) == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "Repository : file://"//tdir()//"SVN"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    edited               a.txt") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    unregistered         b.txt") > 0, text)
            if (allocated(error)) exit body
            call close_buffer(dir, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_vc_dir
end module test_vc_svn_mod
