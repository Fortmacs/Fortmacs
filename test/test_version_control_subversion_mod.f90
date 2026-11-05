! test_version_control_subversion_mod.f90 -- tests for version_control_subversion_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/vc-tests/vc-tests.el in part: its
! backend-generic tests, run for SUBVERSION.
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

!> Ports of vc-tests.el for SUBVERSION (vc-test-svn00-create-repo to
!> -svn06-version-diff; upstream skips -svn10-rename-directory, and the
!> working-tree and patch tests are Git and Hg only); own tests of
!> parsing `svn status', the previous revision, C-x v l and C-x v =.
!> The tests that run svn are skipped without svn and svnadmin (there is
!> no vc-svn-tests.el upstream).
module test_version_control_subversion_mod
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, condition_matches, FILE_ALREADY_EXISTS_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, current_buffer, get_buffer, &
                          is_buffer_live, generate_new_buffer_name
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string
    use file_input_output_mod, only: initialize_fileio, fortmacs_wd, file_name_as_directory, file_exists, is_file_directory, &
                                     create_directory_internal, write_region
    use files_mod, only: delete_directory, find_file_noselect, executable_find
    use execute_process_mod, only: execute_process
    use bindings_mod, only: initialize_bindings
    use version_control_hooks_mod, only: version_control_backend, version_control_registered, version_control_state, &
                                         version_control_working_revision, version_control_checkout_model, &
                                         version_control_file_clearprops, version_control_file_get_state, &
                                         version_control_file_get_working_revision, version_control_backends, &
                                         version_control_backend_named, version_control_clear_context, &
                                         version_control_handled_backends, VERSION_CONTROL_STATE_UNKNOWN, &
                                         VERSION_CONTROL_UNREGISTERED, VERSION_CONTROL_ADDED, VERSION_CONTROL_UP_TO_DATE, &
                                         VERSION_CONTROL_EDITED, VERSION_CONTROL_CONFLICT, VERSION_CONTROL_REMOVED, &
                                         VERSION_CONTROL_MODEL_IMPLICIT
    use version_control_mod, only: version_control_fileset_type, version_control_register, version_control_responsible_backend, &
                                   version_control_rename_file, version_control_create_repository
    use string_type_mod, only: string_type, append_string
    use version_control_subversion_mod, only: version_control_subversion_parse_status, version_control_subversion_previous_revision
    implicit none(type, external)
    private

    public :: collect_version_control_subversion_mod

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdirectory = "build/test-vc-svn/"

    character, parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_version_control_subversion_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("vc-svn-parse-status", test_parse_status), &
                    new_unittest("vc-svn-previous-revision", test_previous_revision), &
                    new_unittest("vc-test-svn00-create-repo", test_create_repository), &
                    new_unittest("vc-test-svn01-register", test_register), &
                    new_unittest("vc-test-svn02-state", test_state), &
                    new_unittest("vc-test-svn03-working-revision", test_working_revision), &
                    new_unittest("vc-test-svn04-checkout-model", test_checkout_model), &
                    new_unittest("vc-test-svn05-rename-file", test_rename_file), &
                    new_unittest("vc-test-svn06-version-diff", test_version_diff), &
                    new_unittest("vc-svn-print-log", test_print_log), &
                    new_unittest("vc-svn-diff", test_diff), &
                    new_unittest("vc-dir-svn", test_version_control_directory) &
                    ]
    end subroutine collect_version_control_subversion_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A list of the strings given.
    function strings(a1, a2) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2
        type(string_type), allocatable :: list(:)

        call append_string(list, a1, a2)
    end function strings

    !> A fresh temporary buffer, current.
    subroutine temporary_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(generate_new_buffer_name(" *vc-test*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = file_name_as_directory(fortmacs_wd)
    end subroutine temporary_buffer

    !> The test working copy directory.
    function tdir() result(directory)
        character(len=:), allocatable :: directory

        directory = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdirectory)
    end function tdir

    !> Whether svn and svnadmin are available (else the test is skipped;
    !> vc-test--svn-enabled).
    logical function has_subversion(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        call executable_find("svnadmin", path)
        has_subversion = len(path) > 0

        if (has_subversion) then
            call executable_find("svn", path)
            has_subversion = len(path) > 0
        end if

        if (.not. has_subversion) call skip_test(error, "no svn")
    end function has_subversion

    !> The SUBVERSION backend's index.
    integer function subversion_b()
        subversion_b = version_control_backend_named("SVN")
    end function subversion_b

    !> A buffer in a fresh, empty test directory (`default_directory'),
    !> made a SUBVERSION working copy with `version_control_create_repository'; only SUBVERSION is handled.
    subroutine setup(h, status)
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call initialize_bindings()
        call initialize_fileio()
        call version_control_clear_context()
        version_control_handled_backends = [character(len=8) :: "SVN"]
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(file_name_as_directory(fortmacs_wd)//"build", st)
        call create_directory_internal(tdir(), st)
        call temporary_buffer(h)
        buffers(current)%directory = tdir()
        call version_control_create_repository(subversion_b(), status)
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

    !> Run svn with ARGUMENTS in the test directory.  A failure is reported on
    !> standard error with svn's output.
    subroutine subversion(a1, a2, a3, a4)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4
        type(value_type) :: arguments(5)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n

        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        arguments(1) = value_type(text="--non-interactive")
        arguments(2) = value_type(text=a1)
        n = 2
        if (present(a2)) call append(a2)
        if (present(a3)) call append(a3)
        if (present(a4)) call append(a4)
        old = current_buffer()
        call temporary_buffer(h)
        buffers(current)%directory = tdir()
        call execute_process("svn", result, st, destination=value_type(.true.), arguments=make_list(arguments(1:n)))

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
            arguments(n) = value_type(text=a)
        end subroutine append
    end subroutine subversion

    ! ------------------------------------------------------------------
    ! Without svn
    ! ------------------------------------------------------------------

    !> `version_control_subversion_parse_status' sets the state and working revision of each
    !> file of `svn status -v' output.
    subroutine test_parse_status(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: directory, revision
        logical :: found
        integer :: s

        call initialize_bindings()
        call initialize_fileio()
        call version_control_clear_context()
        call temporary_buffer(h)
        directory = "/nonexistent/wc/"
        buffers(current)%directory = directory
        call insert("                 3        2 alice        ."//LF// &
                    "M                3        3 bob          edited.txt"//LF// &
                    "A                0        ?  ?           new.txt"//LF// &
                    "C                3        3 bob          clash.txt"//LF// &
                    "D                3        2 alice        gone.txt"//LF// &
                    "?                                        stray.txt"//LF, st)

        body: block
            s = version_control_subversion_parse_status(directory)
            ! The first line is FILENAME's.
            call check(error, s == VERSION_CONTROL_UP_TO_DATE, "the directory")
            if (allocated(error)) exit body
            call version_control_file_get_working_revision(directory, revision, found)
            call check(error, found .and. revision == "2", "the last-modified revision: "//revision)
            if (allocated(error)) exit body
            call check(error, version_control_file_get_state(directory//"edited.txt") == VERSION_CONTROL_EDITED, "edited")
            if (allocated(error)) exit body
            call version_control_file_get_working_revision(directory//"edited.txt", revision, found)
            call check(error, found .and. revision == "3", "edited revision: "//revision)
            if (allocated(error)) exit body
            call check(error, version_control_file_get_state(directory//"new.txt") == VERSION_CONTROL_ADDED, "added")
            if (allocated(error)) exit body
            call version_control_file_get_working_revision(directory//"new.txt", revision, found)
            call check(error, found .and. revision == "0", "added revision: "//revision)
            if (allocated(error)) exit body
            call check(error, version_control_file_get_state(directory//"clash.txt") == VERSION_CONTROL_CONFLICT, "conflict")
            if (allocated(error)) exit body
            call check(error, version_control_file_get_state(directory//"gone.txt") == VERSION_CONTROL_REMOVED, "removed")
            if (allocated(error)) exit body
            call check(error, version_control_file_get_state(directory//"stray.txt") == VERSION_CONTROL_UNREGISTERED, &
                       "unregistered")
        end block body

        call close_buffer(h, st)
        call version_control_clear_context()
    end subroutine test_parse_status

    !> The previous revision of a SUBVERSION revision is one less, none before 1.
    subroutine test_previous_revision(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: previous
        logical :: found

        call version_control_subversion_previous_revision("f", "12", previous, found)
        call check(error, found .and. previous == "11", "12: "//previous)
        if (allocated(error)) return
        call version_control_subversion_previous_revision("f", "1", previous, found)
        call check(error,.not. found, "1")
    end subroutine test_previous_revision

    ! ------------------------------------------------------------------
    ! vc-tests.el
    ! ------------------------------------------------------------------

    !> Create a test repository in `default_directory'.
    subroutine test_create_repository(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, is_file_directory(tdir()//".svn"), "the working copy")
            if (allocated(error)) exit body
            call check(error, version_control_responsible_backend(tdir(), st) == subversion_b(), "responsible backend")
        end block body

        call teardown(h)
    end subroutine test_create_repository

    !> Register a file (SUBVERSION does not support unregistering).
    !> This checks also `version_control_backend' and `version_control_responsible_backend'.
    subroutine test_register(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name1, temporary_name2

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body

            ! For file oriented backends CONCURRENT_VERSIONS_SYSTEM, RCS and SUBVERSION the backend is
            ! returned, and the directory is registered already.
            if (version_control_backend(tdir()) /= 0) then
                call check(error, version_control_registered(tdir()), "directory registered")
            else
                call check(error,.not. version_control_registered(tdir()), "directory not registered")
            end if

            if (allocated(error)) exit body
            call check(error, version_control_responsible_backend(tdir(), st) == subversion_b(), "responsible backend")
            if (allocated(error)) exit body
            temporary_name1 = tdir()//"foo"
            temporary_name2 = "bla"
            ! Register files.  Check for it.
            call write_file(temporary_name1, "foo")
            call check(error, file_exists(temporary_name1) .and. version_control_backend(temporary_name1) == 0 .and. &
                       version_control_responsible_backend(temporary_name1, &
                                               st) == subversion_b() .and. .not. version_control_registered(temporary_name1), "foo")
            if (allocated(error)) exit body
            call write_file(tdir()//temporary_name2, "bla")
            call check(error, &
                       version_control_backend(temporary_name2) == 0 .and. version_control_responsible_backend(temporary_name2, &
                                                                                                       st) == subversion_b() .and. &
                       .not. version_control_registered(temporary_name2), "bla")
            if (allocated(error)) exit body
            fs%backend = subversion_b()
            fs%files = strings(temporary_name1, temporary_name2)
            call version_control_register(st, fs)
            call check(error, st%ok(), "register: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, &
                     version_control_backend(temporary_name1) == subversion_b() .and. version_control_registered(temporary_name1), &
                       "foo registered")
            if (allocated(error)) exit body
            call check(error, &
                     version_control_backend(temporary_name2) == subversion_b() .and. version_control_registered(temporary_name2), &
                       "bla registered")
            if (allocated(error)) exit body
            ! Unregistering is not supported: `version_control_not_supported'.
            call check(error,.not. associated(version_control_backends(subversion_b())%unregister), "no unregister")
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

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            temporary_name = tdir()//"foo"
            ! Check state of a nonexistent file.
            call check(error, version_control_state(temporary_name) == VERSION_CONTROL_STATE_UNKNOWN, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check state.
            call write_file(temporary_name, "foo")
            s = version_control_state(temporary_name, subversion_b())
            call check(error, s == VERSION_CONTROL_STATE_UNKNOWN .or. s == VERSION_CONTROL_UNREGISTERED, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check state.
            fs%backend = subversion_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            s = version_control_state(temporary_name)
            call check(error, &
                       s == VERSION_CONTROL_STATE_UNKNOWN .or. s == VERSION_CONTROL_ADDED .or. s == VERSION_CONTROL_UP_TO_DATE, &
                       "registered")
            ! Unregistering is not supported.
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

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            ! "0": SUBVERSION
            call version_control_working_revision(tdir(), revision, found)
            call check(error,.not. found .or. revision == "0", "repository: "//revision)
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
            fs%backend = subversion_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            call version_control_working_revision(temporary_name, revision, found)
            ! "0": Bzr CONCURRENT_VERSIONS_SYSTEM Hg SOURCE SUBVERSION
            call check(error,.not. found .or. revision == "0", "registered: "//revision)
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

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            call check(error, version_control_checkout_model(subversion_b(), strings(tdir())) == VERSION_CONTROL_MODEL_IMPLICIT, &
                       "repository")
            if (allocated(error)) exit body
            temporary_name = tdir()//"foo"
            call check(error, version_control_checkout_model(subversion_b(), &
                                                          strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, "nonexistent")
            if (allocated(error)) exit body
            call write_file(temporary_name, "foo")
            call check(error, version_control_checkout_model(subversion_b(), &
                                                             strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, "new file")
            if (allocated(error)) exit body
            fs%backend = subversion_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            call check(error, version_control_checkout_model(subversion_b(), &
                                                           strings(temporary_name)) == VERSION_CONTROL_MODEL_IMPLICIT, "registered")
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

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            temporary_name = tdir()//"foo"
            new_name = tdir()//"bar"
            ! Write a new file.
            call write_file(temporary_name, "foo")
            ! Register it.  Renaming can fail otherwise.
            fs%backend = subversion_b()
            fs%files = strings("foo")
            call version_control_register(st, fs)
            ! Test that `version_control_rename_file' isn't affected by
            ! `default_directory' except for the meaning of OLD and NEW if
            ! they are relative file names.
            saved = buffers(current)%directory
            buffers(current)%directory = file_name_as_directory(fortmacs_wd)//"build/"
            call version_control_rename_file("test-vc-svn/foo", "test-vc-svn/bar", st)
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
            ! OK-IF-ALREADY-EXISTS deletes NEW with `version_control_delete_file'; `svn
            ! remove' refuses an added file without --force (as upstream's
            ! vc-svn-delete-file does), so that an error condition occurs.
            call version_control_rename_file(temporary_name, new_name, st, ok_if_already_exists=.true.)
            call check(error,.not. st%ok() .and. file_exists(temporary_name) .and. file_exists(new_name), "not replaced")
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
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, diff
        type(status_type) :: st
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: temporary_name, rawtext

        if (.not. has_subversion(error)) return
        call setup(h, st)

        body: block
            temporary_name = tdir()//"foo"
            fs%backend = subversion_b()
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
        call subversion("add", "a.txt")
        call subversion("commit", "-m", "init")
        call write_file(tdir()//"a.txt", "one"//LF//"three"//LF)
        call subversion("commit", "-m", "second")
        call subversion("update")
        call find_file_noselect(tdir()//"a.txt", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        call show_alone()
    end subroutine two_commits

    !> Wait for the process of BUFFER to finish (its `version_control_exec_after'
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

    !> C-x v l in a file under SUBVERSION shows its log in SVN-Log-View mode, with
    !> both revisions, newest first, where n and p move between them.
    subroutine test_print_log(error)
        use version_control_mod, only: version_control_print_log
        use log_view_mod, only: log_view_message_next, log_view_current_tag
        use editing_functions_mod, only: point_minimum, goto_character
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log
        type(status_type) :: st
        character(len=:), allocatable :: text, tag
        logical :: has

        if (.not. has_subversion(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, allocated(buffers(current)%version_control_mode), "version_control_mode set")
            if (allocated(error)) exit body
            call check(error, buffers(current)%version_control_mode, " SVN-2")
            if (allocated(error)) exit body
            call version_control_print_log(st, "", 500_int64)
            call check(error, st%ok(), "vc-print-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call check(error, is_buffer_live(log), "*vc-change-log*")
            if (allocated(error)) exit body
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, buffers(current)%major_mode, "version_control_subversion_log_view_mode")
            if (allocated(error)) exit body
            call check(error, index(text, "Working file: ") == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "second") > 0 .and. index(text, "init") > index(text, "second"), text)
            if (allocated(error)) exit body
            call goto_character(point_minimum())
            call log_view_message_next(st)
            call log_view_current_tag(tag, has)
            call check(error, st%ok() .and. has .and. tag == "2", "n: "//tag)
            if (allocated(error)) exit body
            call log_view_message_next(st)
            call log_view_current_tag(tag, has)
            call check(error, st%ok() .and. has .and. tag == "1", "n again: "//tag)
            if (allocated(error)) exit body
            call close_buffer(log, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_print_log

    !> C-x v = of an edited file under SUBVERSION gives its diff in *vc-diff*, in
    !> Diff mode; none for an unchanged one.
    subroutine test_diff(error)
        use version_control_mod, only: version_control_diff
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, d
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (.not. has_subversion(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call version_control_diff(st)
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
            call version_control_diff(st)
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

    !> C-x v d in a SUBVERSION working copy lists its edited and unregistered
    !> files under the Repository header.
    subroutine test_version_control_directory(error)
        use version_control_directory_mod, only: version_control_directory, version_control_directory_busy
        use process_mod, only: accept_process_output
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, directory
        type(status_type) :: st
        character(len=:), allocatable :: text
        logical :: got, busy
        integer :: k

        if (.not. has_subversion(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call write_file(tdir()//"a.txt", "one"//LF//"four"//LF)
            call write_file(tdir()//"b.txt", "b"//LF)
            call set_buffer(h, st)
            call version_control_directory(tdir(), st)
            call check(error, st%ok(), "vc-dir: "//format_error(st))
            if (allocated(error)) exit body
            directory = get_buffer("*vc-dir*")
            call check(error, is_buffer_live(directory), "*vc-dir*")
            if (allocated(error)) exit body

            do k = 1, 600
                call set_buffer(directory, st)
                busy = version_control_directory_busy()
                if (.not. busy) exit
                call accept_process_output(got, st, seconds=0.05)
            end do

            call accept_process_output(got, st, seconds=0.05)
            call set_buffer(directory, st)
            text = buffer_string()
            call check(error, index(text, "VC backend : SVN"//LF) == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "Repository : file://"//tdir()//"SVN"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    edited               a.txt") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    unregistered         b.txt") > 0, text)
            if (allocated(error)) exit body
            call close_buffer(directory, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_version_control_directory
end module test_version_control_subversion_mod
