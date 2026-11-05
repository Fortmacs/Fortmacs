! test_add_log_mod.f90 -- tests for add_log_mod.
! Ports GNU Emacs 31.1 test/lisp/vc/add-log-tests.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> The add-log-tests.el tests (`add_log_current_subprogram' in Shell-script
!> mode), and our own: `add_change_log_entry' into the `*changes to DIR*'
!> buffer, `insert_change_log_entries', `search_change_log_file_name', and
!> diff-mode.el's `diff_add_log_current_subprograms' and
!> `diff_current_subprogram'.
module test_add_log_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, &
                          get_buffer, is_buffer_live
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_char, point, point_min, buffer_string
    use search_mod, only: search_forward, replace_match
    use file_input_output_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, &
                                     create_directory_internal, write_region
    use files_mod, only: delete_directory, abbreviate_file_name
    use keyboard_mod, only: discard_input
    use bindings_mod, only: init_bindings
    use shell_script_mod, only: sh_mode
    use diff_mode_mod, only: diff_mode, diff_add_log_current_subprograms
    use log_edit_mod, only: log_edit, generate_log_edit_changelog_from_diff
    use window_mod, only: pop_to_buffer, get_buffer_window, is_window_live
    use add_log_mod, only: add_log_current_subprogram, add_change_log_entry, insert_change_log_entries, &
                           search_change_log_file_name, change_log_entry_type, add_log_full_name, &
                           add_log_mailing_address, add_log_time_format, add_log_pseudo_changelog_buffer_name, &
                           change_log_mode, add_log_iso8601_time_string
    implicit none(type, external)
    private

    public :: collect_add_log_mod

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)
    character(len=*), parameter :: subdir = "build/test-add-log"
    character(len=*), parameter :: HEADER = "2026-11-03  Jane Doe  <jane@example.com>"
    !> The diff of the Diff mode tests.
    character(len=*), parameter :: DIFF_TEXT = "--- a/a.sh"//LF//"+++ b/a.sh"//LF//"@@ -1,6 +1,7 @@"//LF//" #!/bin/sh"//LF// &
                                   " foo()"//LF//" {"//LF//" "//TAB//"echo foo"//LF//"+"//TAB//"echo new"//LF// &
                                   " }"//LF//" "//LF

contains

    !> Collect all exported unit tests.
    subroutine collect_add_log_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("add-log-current-defun-test-sh-func1", test_sh_func1), &
                    new_unittest("add-log-current-defun-test-sh-func2", test_sh_func2), &
                    new_unittest("add-log-current-defun-test-sh-func3", test_sh_func3), &
                    new_unittest("add-log-current-defun-test-sh-var", test_sh_var), &
                    new_unittest("add-change-log-entry", test_add_change_log_entry), &
                    new_unittest("change-log-insert-entries", test_insert_entries), &
                    new_unittest("change-log-search-file-name", test_search_file_name), &
                    new_unittest("diff-add-log-current-defuns", test_diff_subprograms), &
                    new_unittest("log-edit-generate-changelog-from-diff", test_generate_changelog) &
                    ]
    end subroutine collect_add_log_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The absolute name of the test directory, as a directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdir)
    end function tdir

    !> A fresh, empty test directory, where `find_change_log' stops (it
    !> holds a `.bzr' directory).
    subroutine setup()
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call discard_input()
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(tdir(), st)
        call create_directory_internal(tdir()//".bzr", st)
    end subroutine setup

    !> Remove the test directory.
    subroutine teardown()
        type(status_type) :: st

        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
    end subroutine teardown

    !> A buffer NAME holding TEXT in the test directory, current.
    subroutine temp_buffer(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        buffers(current)%directory = tdir()
        call insert(text, st)
    end subroutine temp_buffer

    !> The date of the headers of the tests.
    subroutine fixed_date(s)
        character(len=:), allocatable, intent(out) :: s

        s = "2026-11-03"
    end subroutine fixed_date

    !> Port of add-log-tests.el:add-log-current-defun-deftest: in a
    !> Shell-script mode buffer holding CONTENT, `add_log_current_subprogram'
    !> where MARKER was gives EXPECTED.
    subroutine check_current_subprogram(error, content, marker, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: content, marker, expected
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: found

        call init_bindings()
        call init_fileio()
        call temp_buffer(" *add-log-test*", content, h)
        call goto_char(point_min())
        call sh_mode(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call search_forward(marker, found, st)
        call check(error, found > 0, "marker not found")

        if (.not. allocated(error)) then
            call replace_match("", st, fixedcase=.false., literal=.true.)
            call add_log_current_subprogram(name)
            call check(error, allocated(name), "no name")
            if (.not. allocated(error)) call check(error, name, expected)
        end if

        call close_buffer(h, st)
    end subroutine check_current_subprogram

    ! ------------------------------------------------------------------
    ! add-log-tests.el
    ! ------------------------------------------------------------------

    !> Test sh-current-defun-name can find function.
    subroutine test_sh_func1(error)
        type(error_type), allocatable, intent(out) :: error

        call check_current_subprogram(error, LF//"function foo"//LF//"{"//LF//TAB//"><"//LF//"}", "><", "foo")
    end subroutine test_sh_func1

    !> Test sh-current-defun-name can find function.
    subroutine test_sh_func2(error)
        type(error_type), allocatable, intent(out) :: error

        call check_current_subprogram(error, LF//"foo()"//LF//"{"//LF//TAB//"><"//LF//"}", "><", "foo")
    end subroutine test_sh_func2

    !> Test sh-current-defun-name can find function.
    subroutine test_sh_func3(error)
        type(error_type), allocatable, intent(out) :: error

        call check_current_subprogram(error, LF//"function foo()"//LF//"{"//LF//TAB//"><"//LF//"}", "><", "foo")
    end subroutine test_sh_func3

    !> Test sh-current-defun-name can find variable definition.
    subroutine test_sh_var(error)
        type(error_type), allocatable, intent(out) :: error

        call check_current_subprogram(error, LF//"PATH=a:/ab:/usr/abc"//LF//"DIR=/pr><oc", "><", "DIR")
    end subroutine test_sh_var

    ! ------------------------------------------------------------------
    ! Our own
    ! ------------------------------------------------------------------

    !> \[add_change_log_entry] in a Shell-script buffer: a new record with
    !> the subprogram, a second subprogram of the same file on a line of its
    !> own, the same one again, no subprogram, and NEW_ENTRY (the texts
    !> GNU Emacs gives).
    subroutine test_add_change_log_entry(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: source, log
        type(status_type) :: st
        integer(int64) :: found
        character(len=:), allocatable :: item

        call setup()
        add_log_full_name = "Jane Doe"
        add_log_mailing_address = "jane@example.com"
        add_log_time_format => fixed_date
        call temp_buffer("foo.sh", "#!/bin/sh"//LF//"foo()"//LF//"{"//LF//TAB//"echo foo"//LF//"}"//LF//LF// &
                         "bar()"//LF//"{"//LF//TAB//"echo bar"//LF//"}"//LF, source)
        call sh_mode(st)
        buffers(current)%filename = tdir()//"foo.sh"
        call show_alone()
        call goto_char(point_min())
        call search_forward("echo foo", found, st)
        call add_change_log_entry(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        log = current_buffer()
        call check(error, buffers(current)%name, add_log_pseudo_changelog_buffer_name(tdir()//"ChangeLog"))
        if (allocated(error)) return
        item = TAB//"* foo.sh (foo): "
        call check(error, buffer_string(), HEADER//LF//LF//item//LF//LF)
        if (allocated(error)) return
        call check(error, point() == 60, "point")
        if (allocated(error)) return
        call insert("Change foo.", st)
        item = TAB//"* foo.sh (foo): Change foo."//LF//TAB//"(bar): "
        call entry_at("echo bar")
        if (allocated(error)) return
        call check(error, buffer_string(), HEADER//LF//LF//item//LF//LF)
        if (allocated(error)) return
        call check(error, point() == 80, "point")
        if (allocated(error)) return
        call entry_at("echo bar")
        if (allocated(error)) return
        call check(error, buffer_string(), HEADER//LF//LF//item//LF//LF)
        if (allocated(error)) return
        call check(error, point() == 80, "point")
        if (allocated(error)) return
        call entry_at("")
        if (allocated(error)) return
        item = item//LF//TAB
        call check(error, buffer_string(), HEADER//LF//LF//item//LF//LF)
        if (allocated(error)) return
        call check(error, point() == 82, "point")
        if (allocated(error)) return
        call set_buffer(source, st)
        call show_alone()
        call goto_char(point_min())
        call search_forward("echo foo", found, st)
        call add_change_log_entry(st, new_entry=.true.)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string(), HEADER//LF//LF//TAB//"* foo.sh (foo): "//LF//LF//item//LF//LF)
        if (allocated(error)) return
        call check(error, point() == 60, "point")
        deallocate (add_log_full_name, add_log_mailing_address)
        add_log_time_format => add_log_iso8601_time_string
        call close_buffer(log, st)
        call close_buffer(source, st)
        call teardown()

    contains

        ! (switch-to-buffer "foo.sh"), then go after TEXT and
        ! (add-change-log-entry).
        subroutine entry_at(text)
            character(len=*), intent(in) :: text
            integer(int64) :: found

            call set_buffer(source, st)
            call show_alone()
            call goto_char(point_min())
            if (len(text) > 0) call search_forward(text, found, st)
            call add_change_log_entry(st)
            call check(error, st%ok(), format_error(st))
        end subroutine entry_at
    end subroutine test_add_change_log_entry

    !> `insert_change_log_entries' formats the entries as GNU Emacs's
    !> change-log-insert-entries does.
    subroutine test_insert_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(change_log_entry_type) :: entries(2)
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call temp_buffer(" *add-log-test*", "", h)
        entries(1)%file = "a.sh"
        allocate (entries(1)%subprograms(2))
        entries(1)%subprograms(1)%text = "foo"
        entries(1)%subprograms(2)%text = "bar"
        entries(2)%file = "b.txt"
        allocate (entries(2)%subprograms(0))
        call insert_change_log_entries(entries, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "* a.sh (foo):"//LF//"(bar):"//LF//"* b.txt:"//LF)
        call close_buffer(h, st)
    end subroutine test_insert_entries

    !> `search_change_log_file_name' finds the file of the change under
    !> point: from the date line, on the file line, and below it.
    subroutine test_search_file_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: found

        call init_bindings()
        call init_fileio()
        call temp_buffer(" *add-log-test*", HEADER//LF//LF//TAB//"* src/a.f90 (foo): Fix."//LF//TAB//"(bar): Too."//LF//LF// &
                         TAB//"* b.txt: New."//LF, h)
        call change_log_mode(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call search_change_log_file_name(point_min(), name)
        call check(error, allocated(name), "date line")
        if (.not. allocated(error)) call check(error, name, "src/a.f90")
        if (allocated(error)) return
        call goto_char(point_min())
        call search_forward("(bar)", found, st)
        call search_change_log_file_name(point(), name)
        call check(error, allocated(name), "list line")
        if (.not. allocated(error)) call check(error, name, "src/a.f90")
        if (allocated(error)) return
        call search_forward("New", found, st)
        call search_change_log_file_name(point(), name)
        call check(error, allocated(name), "file line")
        if (.not. allocated(error)) call check(error, name, "b.txt")
        call close_buffer(h, st)
    end subroutine test_search_file_name

    !> `diff_add_log_current_subprograms' on a hunk of a Shell-script file
    !> (the entries GNU Emacs gives), and `add_log_current_subprogram' in
    !> the Diff buffer (`diff_current_subprogram').
    subroutine test_diff_subprograms(error)
        type(error_type), allocatable, intent(out) :: error
        type(change_log_entry_type), allocatable :: entries(:)
        type(handle_type) :: h, file_buffer
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: found

        call setup()
        call temp_buffer(" *add-log-file*", "#!/bin/sh"//LF//"foo()"//LF//"{"//LF//TAB//"echo foo"//LF//TAB//"echo new"// &
                         LF//"}"//LF//LF//"bar()"//LF//"{"//LF//TAB//"echo bar"//LF//"}"//LF, h)
        call write_region(tdir()//"a.sh", .false., st)
        call close_buffer(h, st)
        call temp_buffer("*add-log-diff*", DIFF_TEXT, h)
        call diff_mode(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call diff_add_log_current_subprograms(entries, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(entries) == 1, "one file")
        if (allocated(error)) return
        call check(error, entries(1)%file, "a.sh")
        if (allocated(error)) return
        call check(error, size(entries(1)%subprograms) == 1, "one subprogram")
        if (allocated(error)) return
        call check(error, entries(1)%subprograms(1)%text, "foo")
        if (allocated(error)) return
        call goto_char(point_min())
        call search_forward("+"//TAB//"echo new", found, st)
        call add_log_current_subprogram(name)
        call check(error, allocated(name), "no name in the Diff buffer")
        if (.not. allocated(error)) call check(error, name, "foo")
        call close_buffer(h, st)
        file_buffer = get_buffer("a.sh")
        if (is_buffer_live(file_buffer)) call close_buffer(file_buffer, st)
        call teardown()
    end subroutine test_diff_subprograms

    !> Make a.sh of the test directory, and show its diff in a Diff mode
    !> buffer: the diff function of the Log Edit buffer of
    !> `test_generate_changelog'.
    recursive subroutine show_test_diff(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: h, old

        old = current_buffer()
        call temp_buffer(" *add-log-file*", "#!/bin/sh"//LF//"foo()"//LF//"{"//LF//TAB//"echo foo"//LF//TAB//"echo new"// &
                         LF//"}"//LF//LF//"bar()"//LF//"{"//LF//TAB//"echo bar"//LF//"}"//LF, h)
        call write_region(tdir()//"a.sh", .false., status)
        call close_buffer(h, status)
        call set_buffer(old, status)
        call temp_buffer("*add-log-diff*", DIFF_TEXT, h)
        call diff_mode(status)
        if (status%ok()) call pop_to_buffer(h, status)
    end subroutine show_test_diff

    ! The callback of the Log Edit buffer.  Not a port.
    recursive subroutine ignore_commit(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine ignore_commit

    !> \[generate_log_edit_changelog_from_diff] inserts the entries of the
    !> diff its diff function shows, without displaying it, and joins a
    !> single entry to the Summary header point is on.
    subroutine test_generate_changelog(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: log, diff_buffer, file_buffer
        type(status_type) :: st

        call setup()
        call temp_buffer("*vc-log*", "", log)
        call show_alone()
        call log_edit(ignore_commit, st, diff_function=show_test_diff)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call generate_log_edit_changelog_from_diff(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(current)%name, "*vc-log*")
        if (allocated(error)) return
        call check(error, buffer_string(), "* a.sh (foo):"//LF)
        if (allocated(error)) return
        diff_buffer = get_buffer("*add-log-diff*")
        call check(error,.not. is_window_live(get_buffer_window(diff_buffer)), "the diff is displayed")
        if (allocated(error)) return
        call erase_buffer(st)
        call insert("Summary: "//LF//LF, st)
        call goto_char(10_int64)
        call generate_log_edit_changelog_from_diff(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "Summary: * a.sh (foo): "//LF//LF)
        call close_buffer(log, st)
        call close_buffer(diff_buffer, st)
        file_buffer = get_buffer("a.sh")
        if (is_buffer_live(file_buffer)) call close_buffer(file_buffer, st)
        call teardown()
    end subroutine test_generate_changelog
end module test_add_log_mod
