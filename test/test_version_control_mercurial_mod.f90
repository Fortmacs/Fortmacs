! test_version_control_mercurial_mod.f90 -- tests for version_control_mercurial_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/vc-hg-tests.el and the Hg tests of
! test/lisp/vc/vc-tests/vc-tests.el, in part.
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

!> Ports of vc-hg-tests.el (vc-hg-after-dir-status and
!> vc-hg-diff-revision-arguments) and of vc-tests.el for Hg
!> (vc-test-hg00-create-repo to -hg06-version-diff and
!> -hg10-rename-directory); own tests of the version parsing, the mode
!> line, the previous and next revisions, the logs and VC-Dir.  The tests
!> running hg are skipped without hg.  Not ported: the annotate tests of
!> vc-hg-tests.el (annotate is deferred) and vc-test-hg07 to -hg09
!> (working trees and patches are deferred).
module test_version_control_mercurial_mod
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, condition_matches, FM_FILE_ALREADY_EXISTS
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, current_buffer, get_buffer, &
                          is_buffer_live, generate_new_buffer_name
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point_min, goto_char
    use search_mod, only: re_search_forward, SEARCH_NOERROR
    use file_input_output_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, file_exists, is_file_directory, &
                                     create_directory_internal, write_region
    use files_mod, only: delete_directory, find_file_noselect, executable_find
    use execute_process_mod, only: execute_process
    use process_mod, only: is_process_live, accept_process_output
    use bindings_mod, only: init_bindings
    use string_type_mod, only: string_type
    use version_control_hooks_mod, only: vc_dir_entry_type, vc_backend, vc_registered, vc_state, &
                                        vc_working_revision, vc_checkout_model, vc_file_clearprops, vc_backends, vc_backend_named, &
                                         vc_clear_context, vc_handled_backends, VC_STATE_UNKNOWN, VC_UNREGISTERED, VC_ADDED, &
                                         VC_UP_TO_DATE, VC_EDITED, VC_REMOVED, VC_MODEL_IMPLICIT, VC_RENAME_COPY, VC_RENAME_RENAME
    use version_control_mod, only: vc_fileset_type, vc_register, vc_responsible_backend, vc_rename_file, vc_create_repo
    use version_control_mercurial_mod, only: vc_hg_after_dir_status, vc_hg_diff_flags, vc_hg_parse_program_version, &
                                             vc_hg_previous_revision, vc_hg_next_revision, vc_hg_diff_switches
    implicit none(type, external)
    private

    public :: collect_version_control_mercurial_mod

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-vc-hg/"

    !> The entries `vc_hg_after_dir_status' passed to its update function.
    type(vc_dir_entry_type), allocatable :: updated(:)

contains

    !> Collect all exported unit tests.
    subroutine collect_version_control_mercurial_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("vc-hg-after-dir-status", test_after_dir_status), &
                    new_unittest("vc-hg-diff-revision-arguments", test_diff_revision_arguments), &
                    new_unittest("vc-hg-program-version", test_program_version), &
                    new_unittest("vc-test-hg00-create-repo", test_create_repo), &
                    new_unittest("vc-test-hg01-register", test_register), &
                    new_unittest("vc-test-hg02-state", test_state), &
                    new_unittest("vc-test-hg03-working-revision", test_working_revision), &
                    new_unittest("vc-test-hg04-checkout-model", test_checkout_model), &
                    new_unittest("vc-test-hg05-rename-file", test_rename_file), &
                    new_unittest("vc-test-hg06-version-diff", test_version_diff), &
                    new_unittest("vc-test-hg10-rename-directory", test_rename_directory), &
                    new_unittest("vc-hg-mode-line", test_mode_line), &
                    new_unittest("vc-hg-previous-next-revision", test_previous_next_revision), &
                    new_unittest("vc-hg-print-log", test_print_log), &
                    new_unittest("vc-hg-print-root-log", test_print_root_log), &
                    new_unittest("vc-dir-hg", test_vc_dir) &
                    ]
    end subroutine collect_version_control_mercurial_mod

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

    !> Whether FLAGS are the strings A1 ... A6.
    logical function flags_are(flags, a1, a2, a3, a4, a5) result(same)
        type(string_type), intent(in) :: flags(:)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5
        integer :: n

        n = 1
        if (present(a2)) n = 2
        if (present(a3)) n = 3
        if (present(a4)) n = 4
        if (present(a5)) n = 5
        same = .false.
        if (size(flags) /= n) return
        if (flags(1)%text /= a1) return

        if (present(a2)) then
            if (flags(2)%text /= a2) return
        end if

        if (present(a3)) then
            if (flags(3)%text /= a3) return
        end if

        if (present(a4)) then
            if (flags(4)%text /= a4) return
        end if

        if (present(a5)) then
            if (flags(5)%text /= a5) return
        end if

        same = .true.
    end function flags_are

    !> A fresh temporary buffer, current.
    subroutine temp_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(generate_new_buffer_name(" *vc-hg-test*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = file_name_as_directory(fortmacs_wd)
    end subroutine temp_buffer

    !> The test repository directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdir)
    end function tdir

    !> Whether hg is available (else the test is skipped, as upstream's
    !> vc-test--hg-enabled).
    logical function has_hg(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        call executable_find("hg", path)
        has_hg = len(path) > 0
        if (.not. has_hg) call skip_test(error, "no hg")
    end function has_hg

    !> A buffer in a fresh, empty test directory (`default_directory'),
    !> made a Mercurial repository with `vc_create_repo'; only Hg is
    !> handled.
    subroutine setup(h, status)
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call vc_clear_context()
        vc_handled_backends = [character(len=8) :: "Hg"]
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(file_name_as_directory(fortmacs_wd)//"build", st)
        call create_directory_internal(tdir(), st)
        call temp_buffer(h)
        buffers(current)%directory = tdir()
        call vc_create_repo(hg_b(), status)
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

    !> Run hg with ARGS in the test directory, as the user "t".  A failure
    !> (a nonzero exit status) is reported on standard error with hg's
    !> output, so that a test that then fails names its cause.
    subroutine hg(a1, a2, a3, a4, a5)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5
        type(value_type) :: args(7)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n

        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        args(1) = value_type(text="--config")
        args(2) = value_type(text="ui.username=t")
        args(3) = value_type(text=a1)
        n = 3
        if (present(a2)) call append(a2)
        if (present(a3)) call append(a3)
        if (present(a4)) call append(a4)
        if (present(a5)) call append(a5)
        old = current_buffer()
        call temp_buffer(h)
        buffers(current)%directory = tdir()
        call execute_process("hg", result, st, destination=value_type(.true.), args=make_list(args(1:n)))

        if (.not. st%ok()) then
            write (error_unit, "(a)") "hg "//a1//": "//format_error(st)
        else if (vtag(result) == V_INTEGER) then
            if (xfixnum(result) /= 0) write (error_unit, "(a)") "hg "//a1//" failed: "//buffer_string()
        end if

        call close_buffer(h, st)
        call set_buffer(old, st)

    contains

        subroutine append(a)
            character(len=*), intent(in) :: a

            n = n + 1
            args(n) = value_type(text=a)
        end subroutine append
    end subroutine hg

    !> The Hg backend's index.
    integer function hg_b()
        hg_b = vc_backend_named("Hg")
    end function hg_b

    !> Set and restore the author identity of vc-test--with-author-identity
    !> (Mercurial takes the user name from EMAIL).
    subroutine author_identity(begin, saved, saved_status)
        use environment_mod, only: setenv
        logical, intent(in) :: begin
        character(len=256), intent(inout) :: saved
        integer, intent(inout) :: saved_status

        if (begin) then
            call get_environment_variable("EMAIL", saved, status=saved_status)
            call setenv("EMAIL", "joh.doe@example.com")
        else if (saved_status == 0) then
            call setenv("EMAIL", trim(saved))
        else
            call setenv("EMAIL")
        end if
    end subroutine author_identity

    ! ------------------------------------------------------------------
    ! vc-hg-tests.el
    ! ------------------------------------------------------------------

    !> The update function of `test_after_dir_status': keep ENTRIES.
    recursive subroutine keep_entries(entries, buffer, more_to_come)
        type(vc_dir_entry_type), intent(in) :: entries(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: more_to_come

        if (buffer%index < 0 .or. more_to_come) continue
        updated = entries
    end subroutine keep_entries

    !> Call `vc_hg_after_dir_status' on IN and keep the entries it gives in
    !> `updated'.  Port of vc-hg-tests.el:vc-hg-test--after-dir-status-expect
    !> (the comparison is the caller's).
    subroutine after_dir_status(in)
        character(len=*), intent(in) :: in
        type(handle_type) :: old, h
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        old = current_buffer()
        call temp_buffer(h)
        call insert(in, st)
        call vc_hg_after_dir_status(keep_entries, null_handle())
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine after_dir_status

    !> Whether entry K of `updated' is NAME in STATE, with the extra
    !> information RENAME (VC_RENAME_COPY or VC_RENAME_RENAME) of ORIG.
    logical function is_entry(k, name, state, rename, orig)
        integer, intent(in) :: k, state, rename
        character(len=*), intent(in) :: name, orig

        is_entry = .false.
        if (k > size(updated)) return
        if (updated(k)%name /= name .or. updated(k)%state /= state .or. .not. updated(k)%has_extra) return
        if (updated(k)%rename_state /= rename) return
        is_entry = updated(k)%orig_name == orig
    end function is_entry

    !> Test `vc_hg_after_dir_status'.  (renamed-to is VC_RENAME_RENAME on a
    !> removed file, renamed-from on an added one.)
    subroutine test_after_dir_status(error)
        type(error_type), allocatable, intent(out) :: error
        character, parameter :: nl = achar(10)

        call after_dir_status("A bar2"//nl//"  bar"//nl//"A foo2"//nl//"  foo"//nl//"R bar")
        call check(error, size(updated) == 3, "three entries")
        if (allocated(error)) return
        call check(error, is_entry(1, "bar", VC_REMOVED, VC_RENAME_RENAME, "bar2"), "bar renamed-to bar2")
        if (allocated(error)) return
        call check(error, is_entry(2, "foo2", VC_ADDED, VC_RENAME_COPY, "foo"), "foo2 copied foo")
        if (allocated(error)) return
        call check(error, is_entry(3, "bar2", VC_ADDED, VC_RENAME_RENAME, "bar"), "bar2 renamed-from bar")
        if (allocated(error)) return
        call after_dir_status("A bar2"//nl//"  bar"//nl//"A foo2"//nl//"  foo"//nl//"R bar"//nl//"R foo")
        call check(error, size(updated) == 4, "four entries")
        if (allocated(error)) return
        call check(error, is_entry(1, "foo", VC_REMOVED, VC_RENAME_RENAME, "foo2"), "foo renamed-to foo2")
        if (allocated(error)) return
        call check(error, is_entry(2, "bar", VC_REMOVED, VC_RENAME_RENAME, "bar2"), "bar renamed-to bar2")
        if (allocated(error)) return
        call check(error, is_entry(3, "foo2", VC_ADDED, VC_RENAME_RENAME, "foo"), "foo2 renamed-from foo")
        if (allocated(error)) return
        call check(error, is_entry(4, "bar2", VC_ADDED, VC_RENAME_RENAME, "bar"), "bar2 renamed-from bar")
    end subroutine test_after_dir_status

    !> Test `vc_hg_diff' revision arguments (the flags `vc_hg_command' is
    !> given; the buffer is *vc-diff* and the files ("foo") as given).
    subroutine test_diff_revision_arguments(error)
        use version_control_hooks_mod, only: vc_switches_type, VC_SWITCHES_NONE
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: flags(:)
        type(vc_switches_type) :: saved

        call init_bindings()
        call init_fileio()
        saved = vc_hg_diff_switches
        vc_hg_diff_switches = vc_switches_type(kind=VC_SWITCHES_NONE)

        body: block
            ! REV1 REV2 both absent: diff file against working revision
            call vc_hg_diff_flags(strings("foo"), flags)
            call check(error, flags_are(flags, "diff"), "no revisions")
            if (allocated(error)) exit body
            ! REV1 absent and REV2 present: diff REV2 against empty tree
            call vc_hg_diff_flags(strings("foo"), flags, newvers="22222")
            call check(error, flags_are(flags, "diff", "-r", "null", "-r", "22222"), "REV2")
            if (allocated(error)) exit body
            ! REV1 present and REV2 absent: diff working copy against older revision
            call vc_hg_diff_flags(strings("foo"), flags, "11111")
            call check(error, flags_are(flags, "diff", "-r", "11111"), "REV1")
            if (allocated(error)) exit body
            ! REV1 REV2 both present: diff two revisions
            call vc_hg_diff_flags(strings("foo"), flags, "11111", "22222")
            call check(error, flags_are(flags, "diff", "-r", "11111", "-r", "22222"), "REV1 and REV2")
        end block body

        vc_hg_diff_switches = saved
    end subroutine test_diff_revision_arguments

    !> The version of `hg version''s output.
    subroutine test_program_version(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, vc_hg_parse_program_version("Mercurial Distributed SCM (version 6.7.2)"), "6.7.2")
        if (allocated(error)) return
        call check(error, vc_hg_parse_program_version("Mercurial Distributed SCM (version 4.2.)"), "4.2")
        if (allocated(error)) return
        call check(error, vc_hg_parse_program_version("hg: unknown command"), "0")
    end subroutine test_program_version

    ! ------------------------------------------------------------------
    ! vc-tests.el
    ! ------------------------------------------------------------------

    !> Create a test repository in `default_directory'.
    subroutine test_create_repo(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        if (.not. has_hg(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, is_file_directory(tdir()//".hg"), "the repository")
            if (allocated(error)) exit body
            call check(error, vc_responsible_backend(tdir(), st) == hg_b(), "responsible backend")
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

        if (.not. has_hg(error)) return
        call setup(h, st)

        body: block
            call check(error, st%ok(), "create repo: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. vc_registered(tdir()), "directory not registered")
            if (allocated(error)) exit body
            call check(error, vc_responsible_backend(tdir(), st) == hg_b(), "responsible backend")
            if (allocated(error)) exit body
            tmp_name1 = tdir()//"foo"
            tmp_name2 = "bla"
            ! Register files.  Check for it.
            call write_file(tmp_name1, "foo")
            call check(error, file_exists(tmp_name1) .and. vc_backend(tmp_name1) == 0 .and. &
                       vc_responsible_backend(tmp_name1, st) == hg_b() .and. .not. vc_registered(tmp_name1), "foo")
            if (allocated(error)) exit body
            call write_file(tdir()//tmp_name2, "bla")
            call check(error, vc_backend(tmp_name2) == 0 .and. vc_responsible_backend(tmp_name2, st) == hg_b() .and. &
                       .not. vc_registered(tmp_name2), "bla")
            if (allocated(error)) exit body
            fs%backend = hg_b()
            fs%files = strings(tmp_name1, tmp_name2)
            call vc_register(st, fs)
            call check(error, st%ok(), "register: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, vc_backend(tmp_name1) == hg_b() .and. vc_registered(tmp_name1), "foo registered")
            if (allocated(error)) exit body
            call check(error, vc_backend(tmp_name2) == hg_b() .and. vc_registered(tmp_name2), "bla registered")
            if (allocated(error)) exit body
            ! Unregister the files.
            call vc_backends(hg_b())%unregister(tmp_name1, st)
            call vc_file_clearprops(tmp_name1)
            call check(error, vc_backend(tmp_name1) == 0 .and. .not. vc_registered(tmp_name1), "foo unregistered")
            if (allocated(error)) exit body
            call vc_backends(hg_b())%unregister(tmp_name2, st)
            call vc_file_clearprops(tmp_name2)
            call check(error, vc_backend(tmp_name2) == 0 .and. .not. vc_registered(tmp_name2), "bla unregistered")
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

        if (.not. has_hg(error)) return
        call setup(h, st)

        body: block
            tmp_name = tdir()//"foo"
            ! Check state of a nonexistent file.
            call check(error, vc_state(tmp_name) == VC_STATE_UNKNOWN, "nonexistent")
            if (allocated(error)) exit body
            ! Write a new file.  Check state.
            call write_file(tmp_name, "foo")
            s = vc_state(tmp_name, hg_b())
            call check(error, s == VC_STATE_UNKNOWN .or. s == VC_UNREGISTERED, "new file")
            if (allocated(error)) exit body
            ! Register a file.  Check state.
            fs%backend = hg_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            s = vc_state(tmp_name)
            call check(error, s == VC_STATE_UNKNOWN .or. s == VC_ADDED .or. s == VC_UP_TO_DATE, "registered")
            if (allocated(error)) exit body
            ! Unregister the file.  Check state.
            call vc_backends(hg_b())%unregister(tmp_name, st)
            call vc_file_clearprops(tmp_name)
            s = vc_state(tmp_name, hg_b())
            call check(error, s == VC_STATE_UNKNOWN .or. s == VC_UNREGISTERED, "unregistered")
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

        if (.not. has_hg(error)) return
        call setup(h, st)

        body: block
            call vc_working_revision(tdir(), rev, found)
            call check(error,.not. found .or. rev == "0", "repository")
            if (allocated(error)) exit body
            tmp_name = tdir()//"foo"
            ! Check initial working revision, should be none until it's
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
            fs%backend = hg_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            call vc_working_revision(tmp_name, rev, found)
            ! "0": Hg; "0000000000000000000000000000000000000000": Hg;
            ! "-1": Hg versions before 5 (probably).
            if (found) call check(error, rev == "0" .or. rev == "-1" .or. rev == repeat("0", 40), "registered: "//rev)
            if (allocated(error)) exit body
            ! Unregister the file.  Check working revision.
            call vc_backends(hg_b())%unregister(tmp_name, st)
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

        if (.not. has_hg(error)) return
        call setup(h, st)

        body: block
            call check(error, vc_checkout_model(hg_b(), strings(tdir())) == VC_MODEL_IMPLICIT, "repository")
            if (allocated(error)) exit body
            tmp_name = tdir()//"foo"
            call check(error, vc_checkout_model(hg_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "nonexistent")
            if (allocated(error)) exit body
            call write_file(tmp_name, "foo")
            call check(error, vc_checkout_model(hg_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "new file")
            if (allocated(error)) exit body
            fs%backend = hg_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            call check(error, vc_checkout_model(hg_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "registered")
            if (allocated(error)) exit body
            call vc_backends(hg_b())%unregister(tmp_name, st)
            call vc_file_clearprops(tmp_name)
            call check(error, vc_checkout_model(hg_b(), strings(tmp_name)) == VC_MODEL_IMPLICIT, "unregistered")
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

        if (.not. has_hg(error)) return
        call setup(h, st)

        body: block
            tmp_name = tdir()//"foo"
            new_name = tdir()//"bar"
            ! Write a new file.
            call write_file(tmp_name, "foo")
            ! Register it.  Renaming can fail otherwise.
            fs%backend = hg_b()
            fs%files = strings("foo")
            call vc_register(st, fs)
            ! Test that `vc_rename_file' isn't affected by
            ! `default_directory' except for the meaning of OLD and NEW if
            ! they are relative file names.
            saved = buffers(current)%directory
            buffers(current)%directory = file_name_as_directory(fortmacs_wd)//"build/"
            call vc_rename_file("test-vc-hg/foo", "test-vc-hg/bar", st)
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
            call vc_rename_file(tmp_name, new_name, st, ok_if_already_exists=.true.)
            call check(error, st%ok(), "ok-if-already-exists: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tmp_name) .and. file_exists(new_name), "replaced")
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
        use version_control_mod, only: vc_checkin, vc_version_diff
        use log_edit_mod, only: log_edit_done
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buff, diff
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=256) :: saved
        integer :: saved_status
        character(len=:), allocatable :: tmp_name, rawtext

        if (.not. has_hg(error)) return
        call author_identity(.true., saved, saved_status)
        buff = null_handle()
        call setup(h, st)

        body: block
            tmp_name = tdir()//"foo"
            fs%backend = hg_b()
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
        call author_identity(.false., saved, saved_status)
    end subroutine test_version_diff

    !> Check the rename-file action for directories.
    subroutine test_rename_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: tmp_dir, new_dir

        if (.not. has_hg(error)) return
        call setup(h, st)
        fs%backend = hg_b()

        body: block
            ! Test mix of registered and unregistered files.
            tmp_dir = tdir()//"dir1/"
            new_dir = tdir()//"dir2/"
            call create_directory_internal(tmp_dir, st)
            call write_file(tmp_dir//"foo", "foo")
            call write_file(tmp_dir//"bar", "bar")
            ! Register TMP-NAME1 but *not* TMP-NAME2.
            fs%files = strings("dir1/foo")
            call vc_register(st, fs)
            call vc_rename_file(tdir()//"dir1", tdir()//"dir2", st)
            call check(error, st%ok(), "mixed: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tmp_dir//"foo") .and. .not. file_exists(tmp_dir//"bar") .and. &
                       file_exists(new_dir//"foo") .and. vc_registered(new_dir//"foo") .and. &
                       file_exists(new_dir//"bar") .and. .not. vc_registered(new_dir//"bar"), "mixed")
            if (allocated(error)) exit body
            ! Test only unregistered files.
            tmp_dir = tdir()//"dir3/"
            new_dir = tdir()//"dir4/"
            call create_directory_internal(tmp_dir, st)
            call write_file(tmp_dir//"foo", "foo")
            call vc_rename_file(tdir()//"dir3", tdir()//"dir4", st)
            call check(error, st%ok(), "unregistered: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tmp_dir//"foo") .and. file_exists(new_dir//"foo") .and. &
                       .not. vc_registered(new_dir//"foo"), "unregistered")
            if (allocated(error)) exit body
            ! Test only registered files.
            tmp_dir = tdir()//"dir5/"
            new_dir = tdir()//"dir6/"
            call create_directory_internal(tmp_dir, st)
            call write_file(tmp_dir//"foo", "foo")
            fs%files = strings("dir5/foo")
            call vc_register(st, fs)
            call vc_rename_file(tdir()//"dir5", tdir()//"dir6", st)
            call check(error, st%ok(), "registered: "//format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tmp_dir//"foo") .and. file_exists(new_dir//"foo") .and. &
                       vc_registered(new_dir//"foo"), "registered")
        end block body

        call teardown(h)
    end subroutine test_rename_directory

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> A committed file a.txt ("one\ntwo\n"), visited; its buffer is
    !> current and shown alone.
    subroutine committed_file(h, buffer, status)
        type(handle_type), intent(out) :: h, buffer
        type(status_type), intent(out) :: status

        call setup(h, status)
        call write_file(tdir()//"a.txt", "one"//achar(10)//"two"//achar(10))
        call hg("add", "a.txt")
        call hg("commit", "-q", "-m", "init")
        call find_file_noselect(tdir()//"a.txt", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        call show_alone()
    end subroutine committed_file

    !> A repository whose a.txt has two commits ("init" and "second"),
    !> visited in BUFFER.
    subroutine two_commits(h, buffer, status)
        type(handle_type), intent(out) :: h, buffer
        type(status_type), intent(out) :: status

        call committed_file(h, buffer, status)
        if (.not. status%ok()) return
        call write_file(tdir()//"a.txt", "one"//achar(10)//"three"//achar(10))
        call hg("commit", "-q", "-m", "second")
    end subroutine two_commits

    !> Visiting a file under Hg shows Hg-REV in the mode line (REV the
    !> shortest unique prefix of six or more characters), and Hg:REV once
    !> it is edited and saved; an active bookmark replaces REV.
    subroutine test_mode_line(error)
        use files_mod, only: save_buffer
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer
        type(status_type) :: st
        character(len=:), allocatable :: mode

        if (.not. has_hg(error)) return
        call committed_file(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, allocated(buffers(current)%vc_mode), "vc_mode set")
            if (allocated(error)) exit body
            mode = buffers(current)%vc_mode
            call check(error, index(mode, " Hg-") == 1 .and. len(mode) == 10, mode)
            if (allocated(error)) exit body
            call check(error, buffers(current)%backup_inhibited, "backup-inhibited")
            if (allocated(error)) exit body
            call goto_char(point_min())
            call insert("X", st)
            call save_buffer(1_int64, .false., st)
            call check(error, st%ok(), "save: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, vc_state(buffers(current)%filename) == VC_EDITED, "edited")
            if (allocated(error)) exit body
            call check(error, buffers(current)%vc_mode, " Hg:"//mode(5:))
            if (allocated(error)) exit body
            ! An active bookmark.
            call hg("bookmark", "topic")
            call insert("Y", st)
            call save_buffer(1_int64, .false., st)
            call check(error, buffers(current)%vc_mode, " Hg:topic")
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_mode_line

    !> The previous revision of "." is ".~1", of ".~1" ".~2", and of a
    !> revision number the one before it; the next revision of 0 is 1, of
    !> the tip none.
    subroutine test_previous_next_revision(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer
        type(status_type) :: st
        character(len=:), allocatable :: rev
        logical :: found

        if (.not. has_hg(error)) return
        call two_commits(h, buffer, st)

        body: block
            call vc_hg_previous_revision(tdir()//"a.txt", ".", rev, found)
            call check(error, found .and. rev == ".~1", "previous of .: "//rev)
            if (allocated(error)) exit body
            call vc_hg_previous_revision(tdir()//"a.txt", ".~1", rev, found)
            call check(error, found .and. rev == ".~2", "previous of .~1: "//rev)
            if (allocated(error)) exit body
            call set_buffer(h, st)
            call vc_hg_previous_revision(tdir()//"a.txt", "1", rev, found)
            call check(error, found .and. rev == "0", "previous of 1: "//rev)
            if (allocated(error)) exit body
            call vc_hg_next_revision(tdir()//"a.txt", "0", rev, found)
            call check(error, found .and. rev == "1", "next of 0: "//rev)
            if (allocated(error)) exit body
            call vc_hg_next_revision(tdir()//"a.txt", "1", rev, found)
            call check(error,.not. found, "next of the tip: "//rev)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_previous_next_revision

    !> Wait for the process of the log buffer BUFFER to finish (its
    !> `vc_exec_after' actions run).
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

    !> C-x v l in a file under Hg shows its long log in Hg-Log-View mode,
    !> where n moves between changesets and d shows a changeset's diff.
    subroutine test_print_log(error)
        use version_control_mod, only: vc_print_log
        use log_view_mod, only: log_view_msg_next, log_view_current_tag, log_view_diff
        use editing_functions_mod, only: point
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log, d
        type(status_type) :: st
        character(len=:), allocatable :: text, tag
        logical :: has

        if (.not. has_hg(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            call vc_print_log(st, "", 500_int64)
            call check(error, st%ok(), "vc-print-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call check(error, is_buffer_live(log), "*vc-change-log*")
            if (allocated(error)) exit body
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, buffers(current)%major_mode, "vc_hg_log_view_mode")
            if (allocated(error)) exit body
            call check(error, index(text, "changeset:   1:") == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "summary:     second") > 0 .and. &
                       index(text, "summary:     init") > index(text, "summary:     second"), text)
            if (allocated(error)) exit body
            call goto_char(point_min())
            call log_view_current_tag(tag, has)
            call check(error, has .and. tag == "1", "newest tag: "//tag)
            if (allocated(error)) exit body
            call log_view_msg_next(st)
            call log_view_current_tag(tag, has)
            call check(error, st%ok() .and. has .and. tag == "0", "n: "//tag)
            if (allocated(error)) exit body
            ! d on the newest changeset: its diff with the previous one.
            call goto_char(point_min())
            call log_view_diff(point(), point(), st)
            call check(error, st%ok(), "d: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, "-two"//achar(10)//"+three"//achar(10)) > 0, text)
            if (allocated(error)) exit body
            call close_buffer(d, st)
            call close_buffer(log, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_print_log

    !> C-x v L shows the short log of the whole tree, whose entries RET
    !> expands to the long log entry and collapses again.
    subroutine test_print_root_log(error)
        use version_control_mod, only: vc_print_root_log
        use log_view_mod, only: log_view_message_regexp, log_view_toggle_entry_display
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log
        type(status_type) :: st
        character(len=:), allocatable :: short, text
        integer(int64) :: found
        integer :: entries

        if (.not. has_hg(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            call vc_print_root_log(st, 500_int64)
            call check(error, st%ok(), "vc-print-root-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            short = buffer_string()
            call goto_char(point_min())
            entries = 0

            do
                call re_search_forward(log_view_message_regexp(), found, st, noerror=SEARCH_NOERROR)
                if (found == 0) exit
                entries = entries + 1
            end do

            call check(error, entries == 2, "two short entries in:"//achar(10)//short)
            if (allocated(error)) exit body
            call check(error, index(short, "1:::tip:t ") == 1 .and. index(short, "second") > 0, short)
            if (allocated(error)) exit body
            ! RET expands the first entry.
            call goto_char(point_min())
            call log_view_toggle_entry_display(st)
            text = buffer_string()
            call check(error, st%ok() .and. index(text, "  changeset:   1:") > 0, &
                       "expanded: "//format_error(st)//achar(10)//text)
            if (allocated(error)) exit body
            ! RET again collapses it.
            call goto_char(point_min())
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
    subroutine wait_vc_dir(buffer)
        use version_control_directory_mod, only: vc_dir_busy
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: old
        type(status_type) :: st
        logical :: got, busy
        integer :: k

        old = current_buffer()

        do k = 1, 600
            call set_buffer(buffer, st)
            busy = vc_dir_busy()
            call set_buffer(old, st)
            if (.not. busy) exit
            call accept_process_output(got, st, seconds=0.05)
        end do

        call accept_process_output(got, st, seconds=0.05)
    end subroutine wait_vc_dir

    !> C-x v d lists the edited, unregistered and renamed files of a
    !> Mercurial repository under the "hg summary" headers.
    subroutine test_vc_dir(error)
        use version_control_directory_mod, only: vc_dir
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, dir
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (.not. has_hg(error)) return
        call committed_file(h, buffer, st)

        body: block
            call check(error, st%ok(), "visit: "//format_error(st))
            if (allocated(error)) exit body
            call write_file(tdir()//"b.txt", "b"//achar(10))
            call write_file(tdir()//"c.txt", "c"//achar(10))
            call hg("add", "c.txt")
            call hg("commit", "-q", "-m", "c")
            call hg("mv", "c.txt", "d.txt")
            call write_file(tdir()//"a.txt", "one"//achar(10)//"three"//achar(10))
            call set_buffer(h, st)
            call vc_dir(tdir(), st)
            call check(error, st%ok(), "vc-dir: "//format_error(st))
            if (allocated(error)) exit body
            dir = get_buffer("*vc-dir*")
            call check(error, is_buffer_live(dir), "*vc-dir*")
            if (allocated(error)) exit body
            call wait_vc_dir(dir)
            call set_buffer(dir, st)
            text = buffer_string()
            call check(error, buffers(current)%major_mode, "vc_dir_mode")
            if (allocated(error)) exit body
            call check(error, index(text, "VC backend : Hg"//achar(10)) == 1, text)
            if (allocated(error)) exit body
            call check(error, index(text, "Parent     : 1:") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "Branch     : default"//achar(10)) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    edited               a.txt") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    unregistered         b.txt") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    removed              c.txt   (renamed to d.txt)") > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, "    added                d.txt   (renamed from c.txt)") > 0, text)
            if (allocated(error)) exit body
            call close_buffer(dir, st)
        end block body

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        call teardown(h)
    end subroutine test_vc_dir
end module test_version_control_mercurial_mod
