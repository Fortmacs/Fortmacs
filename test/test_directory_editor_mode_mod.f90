! test_directory_editor_mode_mod.f90 -- tests for directory_editor_mode_mod (dired.el,
! dired-aux.el) and the Dired parts of files_mod (files.el
! `insert-directory').
! Ports GNU Emacs 31.1 test/lisp/dired-tests.el and
! test/lisp/files-tests.el in part; the others are not ported yet.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

module test_directory_editor_mode_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use bindings_mod, only: initialize_bindings
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type, xstring, vtag, V_STRING, choice_type
    use text_properties_mod, only: get_text_property
    use buffer_mod, only: buffers, current, get_or_make_buffer, get_buffer, set_buffer, close_buffer, current_buffer, &
                          is_buffer_live, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, point_minimum, goto_character, buffer_string, is_end_of_buffer, &
                                     line_beginning_position, line_end_position, delete_region
    use editing_commands_mod, only: forward_line
    use keyboard_mod, only: unread_command_events, discard_input
    use file_input_output_mod, only: initialize_fileio, fortmacs_wd, file_name_as_directory, file_exists, is_file_directory, &
                                     write_region, create_directory_internal
    use files_mod, only: find_file_noselect, delete_directory, abbreviate_file_name, is_insert_directory_wildcard_in_directory, &
                         file_relative_name
    use directory_editor_mode_mod, only: directory_editor, directory_editor_noselect, directory_editor_get_filename, &
                                         directory_editor_goto_file, directory_editor_get_marked_files, &
                                         directory_editor_toggle_marks, directory_editor_mark_files_regular_expression, &
                                         directory_editor_mark_files_containing_regular_expression, &
                                         directory_editor_flag_file_deletion, directory_editor_do_delete, &
                                         directory_editor_do_flagged_delete, directory_editor_find_file, &
                                         directory_editor_up_directory, directory_editor_revert, &
                                         directory_editor_create_directory, directory_editor_file_name_at_point, &
                                         directory_editor_buffers_for_directory, directory_editor_sort_toggle, &
                                         directory_editor_unmark_all_files, directory_editor_mark_1, &
                                         directory_editor_insert_directory, directory_editor_deletion_confirmer, &
                                         directory_editor_auto_revert_buffer, directory_editor_always_read_filesystem, &
                                         directory_editor_free_space, DIRECTORY_EDITOR_FREE_SPACE_SEPARATE, &
                                         directory_editor_switches_in_mode_line, &
                                         DIRECTORY_EDITOR_SWITCHES_IN_MODE_LINE_SORT_ORDER, &
                                         DIRECTORY_EDITOR_SWITCHES_IN_MODE_LINE_AS_IS, DIRECTORY_EDITOR_LOCALP_ABSOLUTE, &
                                         DIRECTORY_EDITOR_LOCALP_RELATIVE, DIRECTORY_EDITOR_LOCALP_NO_DIRECTORY, &
                                         DIRECTORY_EDITOR_ARGUMENT_NONE
    use string_type_mod, only: string_type
    use data_mod, only: choice_state
    implicit none(type, external)
    private

    public :: collect_directory_editor_mode_mod

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdirectory = "build/test-dired-mode"

contains

    !> Collect all exported unit tests.
    subroutine collect_directory_editor_mode_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("dired-buffer", test_directory_editor_buffer), &
                    new_unittest("dired-marks", test_marks), &
                    new_unittest("dired-mark-regexp", test_mark_regular_expression), &
                    new_unittest("dired-flag-and-delete", test_flag_and_delete), &
                    new_unittest("dired-create-directory", test_create_directory), &
                    new_unittest("dired-up-directory", test_up_directory), &
                    new_unittest("dired-sort-toggle", test_sort_toggle), &
                    new_unittest("dired-keys", test_keys), &
                    new_unittest("dired-c-x-d-and-x", test_cx_d), &
                    new_unittest("dired-test-bug22694", test_bug22694), &
                    new_unittest("dired-test-bug27243-01", test_bug27243_01), &
                    new_unittest("dired-test-bug27243-02", test_bug27243_02), &
                    new_unittest("dired-test-bug27968", test_bug27968), &
                    new_unittest("dired-test-bug27940", test_bug27940), &
                    new_unittest("dired-test-ls-error-message", test_ls_error), &
                    new_unittest("files-tests-insert-directory-shows-files", test_insert_directory_files), &
                    new_unittest("files-tests-insert-directory-wildcard-in-dir-p", test_wildcard_in_directory) &
                    ]
    end subroutine collect_directory_editor_mode_mod

    !> The absolute name of the test directory, as a directory.
    function tdir() result(directory)
        character(len=:), allocatable :: directory

        directory = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdirectory)
    end function tdir

    !> A fresh, empty test directory; *scratch* current and shown alone.
    subroutine setup()
        type(handle_type) :: buffer
        type(status_type) :: st

        call initialize_bindings()
        call initialize_fileio()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        buffers(current)%directory = file_name_as_directory(fortmacs_wd)
        call show_alone()
        call kill_directory_editor_buffers()
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(tdir(), st)
        directory_editor_deletion_confirmer => always_yes
        directory_editor_auto_revert_buffer = .false.
        directory_editor_always_read_filesystem = .false.
    end subroutine setup

    !> Remove the test directory and the buffers showing it.
    subroutine teardown()
        type(status_type) :: st
        type(handle_type) :: buffer

        call kill_directory_editor_buffers()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        directory_editor_deletion_confirmer => null()
    end subroutine teardown

    !> Kill every Dired buffer of the test directory and its subdirectories.
    subroutine kill_directory_editor_buffers()
        type(handle_type), allocatable :: buffers(:)
        type(status_type) :: st
        integer :: k

        buffers = directory_editor_buffers_for_directory(tdir())

        do k = 1, size(buffers)
            call close_buffer(buffers(k), st)
        end do

        buffers = directory_editor_buffers_for_directory(tdir()//"sub/")

        do k = 1, size(buffers)
            call close_buffer(buffers(k), st)
        end do
    end subroutine kill_directory_editor_buffers

    !> A `directory_editor_deletion_confirmer' that always says yes.
    recursive subroutine always_yes(prompt, answer, status)
        character(len=*), intent(in) :: prompt
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status

        status = status_type()
        answer = len(prompt) >= 0
    end subroutine always_yes

    !> Create the file NAME (relative to the test directory) holding TEXT.
    subroutine make_file(name, text)
        character(len=*), intent(in) :: name, text
        type(handle_type) :: buffer, old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(" *dired-mode-test-file*", buffer, st)
        call set_buffer(buffer, st)
        call erase_buffer(st)
        call insert(text, st)
        call write_region(tdir()//name, .false., st)
        call close_buffer(buffer, st)
        call set_buffer(old, st)
    end subroutine make_file

    !> Unread the characters of KEYS.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(keys)))

        do i = 1, len(keys)
            evs(i) = value_type(int(iachar(keys(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_keys

    !> Whether handles A and B are the same.
    pure logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b

        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> The current line's text.
    function this_line() result(s)
        character(len=:), allocatable :: s

        s = buffer_string()
        s = s(line_beginning_position():line_end_position() - 1)
    end function this_line

    !> The first character of the current line (its mark).
    function line_mark() result(c)
        character(len=:), allocatable :: line
        character :: c

        line = this_line()
        c = " "
        if (len(line) > 0) c = line(1:1)
    end function line_mark

    !> The file names (relative) of the marked files, joined by spaces.
    function marked_names() result(s)
        character(len=:), allocatable :: s
        type(string_type), allocatable :: files(:)
        type(status_type) :: st
        integer :: k

        call directory_editor_get_marked_files(files, st, localp=DIRECTORY_EDITOR_LOCALP_NO_DIRECTORY)
        s = ""

        do k = 1, size(files)
            if (k > 1) s = s//" "
            s = s//files(k)%text
        end do
    end function marked_names

    !> The standard test tree: a.txt, b.txt, c.el and sub/.
    subroutine make_tree()
        type(status_type) :: st

        call make_file("a.txt", "a")
        call make_file("b.txt", "b")
        call make_file("c.el", "c")
        call create_directory_internal(tdir()//"sub", st)
    end subroutine make_tree

    subroutine test_directory_editor_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: file, text, dname, disp
        type(value_type) :: v
        logical :: found
        integer(int64) :: i, position

        call setup()
        call make_tree()

        body: block
            call directory_editor_noselect(tdir(), buffer, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buffer, st)
            i = current
            call check(error, buffers(i)%major_mode == "directory_editor_mode" .and. buffers(i)%read_only, "mode and read-only")
            if (allocated(error)) exit body
            ! The header line, then `ls -al' output indented by two.
            text = buffer_string()
            dname = tdir()
            dname = dname(1:len(dname) - 1)
            call check(error, index(text, "  "//dname//":"//achar(10)) == 1, text)
            if (allocated(error)) exit body
            ! With `directory_editor_free_space' `first', the "total" line is gone and the
            ! header line's colon shows the available space.
            call check(error, index(text, achar(10)//"  total") == 0, text)
            if (allocated(error)) exit body
            call get_text_property(int(len_trim(dname) + 3, int64), value_type(name="display"), v, st)
            call check(error, vtag(v) == V_STRING, "display property on the colon")
            if (allocated(error)) exit body
            disp = xstring(v)
            call check(error, index(disp, ": (") == 1 .and. index(disp, " available)") > 0, disp)
            if (allocated(error)) exit body
            ! Point starts on the first file that is not . or ..
            call directory_editor_get_filename(file, found, st, DIRECTORY_EDITOR_LOCALP_NO_DIRECTORY)
            call check(error, found .and. file == "a.txt", "point on a.txt")
            if (allocated(error)) exit body
            position = directory_editor_goto_file(tdir()//"c.el", st)
            call check(error, position > 0 .and. position == point(), "goto c.el")
            if (allocated(error)) exit body
            call directory_editor_get_filename(file, found, st)
            call check(error, found .and. file == tdir()//"c.el", file)
            if (allocated(error)) exit body
            call directory_editor_get_filename(file, found, st, DIRECTORY_EDITOR_LOCALP_RELATIVE)
            call check(error, found .and. file == "c.el", file)
            if (allocated(error)) exit body
            ! A second `directory_editor' of the same directory reuses the buffer.
            call directory_editor(tdir(), st)
            call check(error, st%ok() .and. same_handle(current_buffer(), buffer), "same buffer")
        end block body

        call teardown()
    end subroutine test_directory_editor_buffer

    subroutine test_marks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: position

        call setup()
        call make_tree()

        body: block
            call directory_editor_noselect(tdir(), buffer, st)
            call set_buffer(buffer, st)
            position = directory_editor_goto_file(tdir()//"b.txt", st)
            call directory_editor_mark_1(1_int64, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, marked_names(), "b.txt")
            if (allocated(error)) exit body
            call goto_character(position)
            call check(error, line_mark() == "*", this_line())
            if (allocated(error)) exit body
            ! Toggling marks all the others (not . and ..).
            call directory_editor_toggle_marks(st)
            call check(error, marked_names(), "a.txt c.el sub")
            if (allocated(error)) exit body
            call directory_editor_unmark_all_files(achar(13), st)
            call check(error, marked_count() == 0, "no marks left")
        end block body

        call teardown()
    end subroutine test_marks

    subroutine test_mark_regular_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup()
        call make_tree()

        body: block
            call directory_editor_noselect(tdir(), buffer, st)
            call set_buffer(buffer, st)
            call directory_editor_mark_files_regular_expression("\.txt\'", st)
            call check(error, marked_names(), "a.txt b.txt")
        end block body

        call teardown()
    end subroutine test_mark_regular_expression

    subroutine test_flag_and_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: position

        call setup()
        call make_tree()

        body: block
            call directory_editor_noselect(tdir(), buffer, st)
            call set_buffer(buffer, st)
            position = directory_editor_goto_file(tdir()//"a.txt", st)
            call directory_editor_flag_file_deletion(1_int64, .false., st)
            call goto_character(position)
            call check(error, line_mark() == "D", this_line())
            if (allocated(error)) exit body
            call directory_editor_do_flagged_delete(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists(tdir()//"a.txt"), "a.txt deleted")
            if (allocated(error)) exit body
            call check(error, directory_editor_goto_file(tdir()//"a.txt", st) == 0, "entry removed")
            if (allocated(error)) exit body
            call check(error, directory_editor_goto_file(tdir()//"b.txt", st) > 0, "b.txt still listed")
            if (allocated(error)) exit body
            ! `directory_editor_do_delete' with no marks deletes the file at point.
            call directory_editor_do_delete(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, st)
            call check(error, st%ok() .and. .not. file_exists(tdir()//"b.txt"), format_error(st))
        end block body

        call teardown()
    end subroutine test_flag_and_delete

    subroutine test_create_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup()
        call make_tree()

        body: block
            call directory_editor_noselect(tdir(), buffer, st)
            call set_buffer(buffer, st)
            call directory_editor_create_directory(tdir()//"new/deep", st)
            call check(error, st%ok() .and. is_file_directory(tdir()//"new/deep"), format_error(st))
            if (allocated(error)) exit body
            ! The topmost new directory is added to the listing.
            call check(error, directory_editor_goto_file(tdir()//"new", st) > 0, "new listed")
            if (allocated(error)) exit body
            call directory_editor_create_directory(tdir()//"new", st)
            call check(error, format_error(st), "Cannot create directory "//tdir()//"new: file exists")
        end block body

        call teardown()
    end subroutine test_create_directory

    subroutine test_up_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: found

        call setup()
        call make_tree()

        body: block
            call directory_editor(tdir()//"sub/", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            buffer = current_buffer()
            call directory_editor_up_directory(st)
            call check(error, st%ok() .and. .not. (same_handle(current_buffer(), buffer)), format_error(st))
            if (allocated(error)) exit body
            call directory_editor_get_filename(file, found, st, DIRECTORY_EDITOR_LOCALP_NO_DIRECTORY)
            call check(error, found .and. file == "sub", "point on sub")
        end block body

        call teardown()
    end subroutine test_up_directory

    subroutine test_sort_toggle(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st

        call setup()
        call make_tree()

        body: block
            call directory_editor_noselect(tdir(), buffer, st)
            call set_buffer(buffer, st)
            call check(error, buffers(current)%mode_name, "Dired by name")
            if (allocated(error)) exit body
            call directory_editor_sort_toggle(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffers(current)%mode_name, "Dired by date")
            if (allocated(error)) exit body
            call directory_editor_sort_toggle(st)
            call check(error, buffers(current)%mode_name, "Dired by name")
            if (allocated(error)) exit body
            ! The other values of `directory_editor_switches_in_mode_line'.
            directory_editor_switches_in_mode_line%characters = 0
            call directory_editor_sort_toggle(st)
            call check(error, buffers(current)%mode_name, "Dired")
            if (allocated(error)) exit body
            directory_editor_switches_in_mode_line = DIRECTORY_EDITOR_SWITCHES_IN_MODE_LINE_AS_IS
            call check(error, directory_editor_switches_in_mode_line%characters < 0, "number cleared")
            if (allocated(error)) exit body
            call directory_editor_sort_toggle(st)
            call check(error, index(buffers(current)%mode_name, "Dired -") == 1, buffers(current)%mode_name)
            if (allocated(error)) exit body
            directory_editor_switches_in_mode_line%procedure => switches_length
            call directory_editor_sort_toggle(st)
            call check(error, buffers(current)%mode_name(1:len("Dired length ")), "Dired length ")
        end block body

        directory_editor_switches_in_mode_line = DIRECTORY_EDITOR_SWITCHES_IN_MODE_LINE_SORT_ORDER
        call teardown()

    contains

        subroutine switches_length(switches, text)
            character(len=*), intent(in) :: switches
            character(len=:), allocatable, intent(out) :: text
            character(len=8) :: digits

            write (digits, "(i0)") len(switches)
            text = "length "//trim(digits)
        end subroutine switches_length
    end subroutine test_sort_toggle

    !> Keys typed in a Dired buffer: m, n, u and t.
    subroutine test_keys(error)
        use keyboard_mod, only: keyboard_store_bytes, command_loop
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call make_tree()

        body: block
            call directory_editor(tdir(), st)
            call show_alone()
            ! On a.txt: mark it and b.txt (m m), then go back and unmark b.txt.
            call keyboard_store_bytes("mmpu")
            call command_loop(st)
            call check(error, marked_names(), "a.txt")
            if (allocated(error)) exit body
            call keyboard_store_bytes("t")
            call command_loop(st)
            call check(error, marked_names(), "b.txt c.el sub")
        end block body

        call teardown()
    end subroutine test_keys

    !> C-x d RET lists the default directory; d flags a file and x deletes
    !> it after a yes-or-no question.
    subroutine test_cx_d(error)
        use keyboard_mod, only: keyboard_store_bytes, command_loop, last_message
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: found

        call setup()
        call make_tree()

        body: block
            buffers(current)%directory = tdir()
            directory_editor_deletion_confirmer => null()
            call keyboard_store_bytes(achar(24)//"d"//achar(13))
            call command_loop(st)
            call check(error, buffers(current)%major_mode == "directory_editor_mode" .and. &
                       buffers(current)%directory == abbreviate_file_name(tdir()), "C-x d RET")
            if (allocated(error)) exit body
            call directory_editor_get_filename(file, found, st, DIRECTORY_EDITOR_LOCALP_NO_DIRECTORY)
            call check(error, found .and. file == "a.txt", "on a.txt")
            if (allocated(error)) exit body
            call keyboard_store_bytes("dxyes"//achar(13))
            call command_loop(st)
            call check(error,.not. file_exists(tdir()//"a.txt"), last_message)
            if (allocated(error)) exit body
            call check(error, file_exists(tdir()//"b.txt"), "b.txt kept")
        end block body

        call teardown()
    end subroutine test_cx_d

    !> Port of dired-test-bug22694.
    subroutine test_bug22694(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: fbuf
        type(status_type) :: st
        type(string_type), allocatable :: files(:)
        logical :: one

        call setup()

        body: block
            directory_editor_always_read_filesystem = .true.
            call make_file("test", "foo")
            call find_file_noselect(tdir()//"test", fbuf, st)
            call directory_editor(tdir(), st)
            call make_file("test", "bar")
            call directory_editor_mark_files_containing_regular_expression("bar", st)
            call directory_editor_get_marked_files(files, st, distinguish_one_marked=.true., one_marked=one)
            call check(error, one .and. size(files) == 1, "one marked")
            if (allocated(error)) exit body
            call check(error, files(1)%text, tdir()//"test")
            call close_buffer(fbuf, st)
        end block body

        call teardown()
    end subroutine test_bug22694

    !> Port of dired-test-bug27243-01.
    subroutine test_bug27243_01(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer, dirbuf
        type(handle_type), allocatable :: buffers(:)
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: found
        integer(int64) :: pt1, pt2

        call setup()

        body: block
            directory_editor_auto_revert_buffer = .true.
            buffers = directory_editor_buffers_for_directory(tdir())
            call check(error, size(buffers) == 0, "no Dired buffer yet")
            if (allocated(error)) exit body
            call find_file_noselect(tdir(), dirbuf, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call set_buffer(dirbuf, st)
            call create_directory_internal(tdir()//"test-subdir", st)
            ! Point must be at end-of-buffer.
            call check(error, is_end_of_buffer(), "at eob")
            if (allocated(error)) exit body
            call directory_editor(tdir(), st)
            ! Previous dired call shouldn't create a new buffer.
            buffers = directory_editor_buffers_for_directory(tdir())
            call check(error, size(buffers) == 1 .and. same_handle(current_buffer(), dirbuf), "one buffer")
            if (allocated(error)) exit body
            buffer = current_buffer()
            pt1 = point()
            call make_file("test-subdir/test-file", "Test")
            ! Sanity check: point should now be on the subdirectory.
            call directory_editor_file_name_at_point(name, found)
            call check(error, found .and. name == abbreviate_file_name(tdir()//"test-subdir/"), name)
            if (allocated(error)) exit body
            call directory_editor_find_file(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            pt2 = point()
            call pop_to(buffer)
            ! Sanity check: point should now be back on the subdirectory.
            call check(error, point() == pt1, "back on test-subdir")
            if (allocated(error)) exit body
            call directory_editor_find_file(st)
            call check(error, point() == pt2, "on test-file again")
        end block body

        call teardown()
    end subroutine test_bug27243_01

    !> Port of dired-test-bug27243-02.
    subroutine test_bug27243_02(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer, dirbuf
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: found
        integer(int64) :: pt1

        call setup()

        body: block
            directory_editor_auto_revert_buffer = .true.
            call find_file_noselect(tdir(), dirbuf, st)
            call create_directory_internal(tdir()//"test-subdir", st)
            call directory_editor(tdir(), st)
            buffer = current_buffer()
            pt1 = point()
            call make_file("test-subdir/test-file", "Test")
            call directory_editor_file_name_at_point(name, found)
            call check(error, found .and. name == abbreviate_file_name(tdir()//"test-subdir/"), name)
            if (allocated(error)) exit body
            call directory_editor_find_file(st)
            call pop_to(buffer)
            call check(error, point() == pt1, "back on test-subdir")
            if (allocated(error)) exit body
            call directory_editor(tdir(), st)
            call directory_editor_file_name_at_point(name, found)
            call check(error, found .and. name == abbreviate_file_name(tdir()//"test-subdir/"), name)
        end block body

        call teardown()
    end subroutine test_bug27243_02

    !> Switch the selected window to BUFFER.
    subroutine pop_to(buffer)
        use window_mod, only: pop_to_buffer_same_window
        type(handle_type), intent(in) :: buffer
        type(status_type) :: st

        call pop_to_buffer_same_window(buffer, st)
    end subroutine pop_to

    !> Port of dired-test-bug27968.
    subroutine test_bug27968(error)
        use simple_mod, only: line_number_at_position
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: file, new_header
        logical :: found
        integer(int64) :: original_length, len2, diff, position, line_nb, shortage, original_point

        call setup()

        body: block
            call create_directory_internal(tdir()//"subdir", st)
            call directory_editor_noselect(tdir()//"subdir", buffer, st)
            call set_buffer(buffer, st)
            original_length = header_length()
            position = point()
            line_nb = line_number_at_position()
            call create_directory_internal(tdir()//"subdir/subdir", st)
            call directory_editor_revert(.false., .false., st)
            ! Change the header line.
            original_point = point()
            call goto_character(1_int64)
            shortage = forward_line(1_int64)
            new_header = "  test-bug27968"
            if (original_length == len(new_header)) new_header = new_header//" :-)"

            block
                use data_mod, only: set
                use value_mod, only: value_type
                call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
                call delete_region(point(), line_end_position(), st)
                call insert(new_header, st)
                call set(value_type(name="inhibit_read_only"), value_type(), st)
            end block

            len2 = header_length()
            diff = len2 - original_length
            ! (save-excursion: point moved with the text before it)
            call goto_character(original_point + diff)
            call check(error, diff /= 0, "header length has changed")
            if (allocated(error)) exit body
            call check(error, line_nb == line_number_at_position() .and. line_nb == line_number_at_position(position + diff), &
                       "same line")
            if (allocated(error)) exit body
            ! After revert, the point must be in 'subdir' line.
            call directory_editor_get_filename(file, found, st, DIRECTORY_EDITOR_LOCALP_RELATIVE, .true.)
            call check(error, found .and. file == "subdir", file)
        end block body

        call teardown()

    contains

        integer(int64) function header_length()
            integer(int64) :: p, s

            p = point()
            call goto_character(1_int64)
            s = forward_line(1_int64)
            header_length = line_end_position() - point()
            call goto_character(p)
        end function header_length
    end subroutine test_bug27968

    !> The five empty directories and zeta-empty-dir of dired-test-with-temp-dirs,
    !> plus five non-empty ones unless JUST_EMPTY_DIRECTORIES; then all marked.
    subroutine bug27940_tree(just_empty_directories)
        logical, intent(in) :: just_empty_directories
        type(status_type) :: st
        character(len=1) :: d
        integer :: i

        call setup()

        do i = 0, 4
            write (d, "(i1)") i
            call create_directory_internal(tdir()//"empty-dir-"//d, st)
        end do

        if (.not. just_empty_directories) then
            do i = 0, 4
                write (d, "(i1)") i
                call create_directory_internal(tdir()//"non-empty-"//d, st)
                call create_directory_internal(tdir()//"non-empty-"//d//"/foo", st)
            end do
        end if

        call create_directory_internal(tdir()//"zeta-empty-dir", st)
        call directory_editor(tdir(), st)
        call directory_editor_toggle_marks(st)
    end subroutine bug27940_tree

    !> The number of marked files.
    integer function marked_count()
        type(string_type), allocatable :: files(:)
        type(status_type) :: st
        logical :: one

        call directory_editor_get_marked_files(files, st, distinguish_one_marked=.true., one_marked=one)
        marked_count = size(files)
        if (size(files) == 1 .and. .not. one) marked_count = 0
    end function marked_count

    !> Port of dired-test-bug27940 (the answers to `read_answer' typed).
    subroutine test_bug27940(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        body: block
            ! If just empty dirs we shouldn't be prompted.
            call bug27940_tree(.true.)
            call type_keys("x")
            call directory_editor_do_delete(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, marked_count() == 0, "all dirs deleted")
            if (allocated(error)) exit body
            call discard_input()
            ! Answer yes (each time).
            call bug27940_tree(.false.)
            call type_keys(repeat("yes"//achar(13), 5))
            call directory_editor_do_delete(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, st)
            call check(error, marked_count() == 0, "yes: all dirs deleted")
            if (allocated(error)) exit body
            ! Answer no.
            call bug27940_tree(.false.)
            call type_keys(repeat("no"//achar(13), 5))
            call directory_editor_do_delete(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, st)
            call check(error, marked_count() == 5, "no: just the empty dirs deleted")
            if (allocated(error)) exit body
            ! Answer all.
            call bug27940_tree(.false.)
            call type_keys("all"//achar(13))
            call directory_editor_do_delete(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, st)
            call check(error, marked_count() == 0, "all: all dirs deleted")
            if (allocated(error)) exit body
            ! Answer quit.
            call bug27940_tree(.false.)
            call type_keys("quit"//achar(13))
            call directory_editor_do_delete(DIRECTORY_EDITOR_ARGUMENT_NONE, 0_int64, st)
            call check(error, marked_count() == 6, "quit: all empty dirs but zeta-empty-dir deleted")
        end block body

        call discard_input()
        call teardown()
    end subroutine test_bug27940

    !> Port of dired-test-ls-error-message (Posix part, without comparing
    !> the message with one the shell makes).
    subroutine test_ls_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: errbuf
        type(status_type) :: st
        character(len=:), allocatable :: text

        call setup()

        body: block
            call directory_editor(tdir()//"bla", st)
            errbuf = get_buffer("*ls error*")
            call check(error, is_buffer_live(errbuf), "*ls error* exists")
            if (allocated(error)) exit body
            call check(error,.not. (buffers(current)%name == "bla"), "the buffer visiting bla is killed")
            if (allocated(error)) exit body
            call set_buffer(errbuf, st)
            text = buffer_string()
            call check(error, index(text, "bla") > 0, text)
            call close_buffer(errbuf, st)
        end block body

        call teardown()
    end subroutine test_ls_error

    !> Port of files-tests-insert-directory-shows-files.
    subroutine test_insert_directory_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: text
        type(choice_type) :: saved

        call setup()
        call make_file("foo", "")
        call make_file("bar", "")
        call get_or_make_buffer("files-tests--insert-directory", buffer, st)
        call set_buffer(buffer, st)
        saved = choice_state(directory_editor_free_space)
        directory_editor_free_space = DIRECTORY_EDITOR_FREE_SPACE_SEPARATE
        call directory_editor_insert_directory(tdir(), "-l", st)
        directory_editor_free_space = saved
        text = buffer_string()
        call check(error, st%ok() .and. index(text, "foo") > 0 .and. index(text, "bar") > 0, text)

        if (.not. allocated(error)) then
            ! dired-free-space `separate': the total line says so, with the free space.
            call check(error, index(text, "total used in directory ") > 0 .and. index(text, " available ") > 0, text)
        end if

        call close_buffer(buffer, st)
        call teardown()
    end subroutine test_insert_directory_files

    !> Port of files-tests-insert-directory-wildcard-in-dir-p.
    subroutine test_wildcard_in_directory(error)
        type(error_type), allocatable, intent(out) :: error

        call one("/home/user/*/.txt", "/home/user/", "*/.txt")
        if (allocated(error)) return
        call one("/home/user/.txt", "", "")
        if (allocated(error)) return
        call one("/home/*/.txt", "/home/", "*/.txt")
        if (allocated(error)) return
        call one("/home/*/", "/home/", "*/")
        if (allocated(error)) return
        call one("/*/.txt", "/", "*/.txt")
        if (allocated(error)) return
        call one("c:/tmp/*/*.txt", "c:/tmp/", "*/*.txt")
        if (allocated(error)) return
        call one("c:/tmp/*.txt", "", "")
        if (allocated(error)) return
        call one("c:/tmp/*/", "c:/tmp/", "*/")
        if (allocated(error)) return
        call one("c:/*/*.txt", "c:/", "*/*.txt")

    contains

        subroutine one(path, directory, wild)
            character(len=*), intent(in) :: path, directory, wild
            character(len=:), allocatable :: d, w
            logical :: found

            found = is_insert_directory_wildcard_in_directory(path, d, w)

            if (len(directory) == 0) then
                call check(error,.not. found, path)
            else
                call check(error, found, path)
                if (allocated(error)) return
                call check(error, d//"|"//w, directory//"|"//wild)
            end if
        end subroutine one
    end subroutine test_wildcard_in_directory
end module test_directory_editor_mode_mod
