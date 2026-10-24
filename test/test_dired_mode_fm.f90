! test_dired_mode_fm.f90 -- tests for dired_mode_fm (dired.el, dired-aux.el)
! and the Dired parts of files_fm (files.el `insert-directory').
! Ported from GNU Emacs 31.1 test/lisp/dired-tests.el (dired-test-bug22694,
! dired-test-bug27243-01 and -02, dired-test-bug27968, dired-test-bug27940,
! files-tests-insert-directory-shows-files, dired-test-ls-error-message
! without the shell's sample message) and test/lisp/files-tests.el
! (files-tests-insert-directory-wildcard-in-dir-p); the others are our own.
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

module test_dired_mode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use bindings_fm, only: init_bindings
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, stringp, xstring
    use lread_fm, only: sym
    use textprop_fm, only: get_text_property
    use buffer_fm, only: buffers, cur, get_buffer_create, get_buffer, set_buffer, kill_buffer, current_buffer, &
                         buffer_live_p, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, goto_char, buffer_string, eobp, line_beginning_position, &
                          line_end_position, delete_region
    use cmds_fm, only: forward_line
    use keyboard_fm, only: unread_command_events, discard_input
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_exists_p, file_directory_p, &
                         write_region, make_directory_internal
    use files_fm, only: find_file_noselect, delete_directory, insert_directory_wildcard_in_dir_p, &
                        file_relative_name
    use dired_mode_fm, only: dired, dired_noselect, dired_get_filename, dired_goto_file, dired_get_marked_files, &
                             dired_string_type, dired_toggle_marks, dired_mark_files_regexp, &
                             dired_mark_files_containing_regexp, dired_flag_file_deletion, dired_do_delete, &
                             dired_do_flagged_delete, dired_find_file, dired_up_directory, dired_revert, &
                             dired_create_directory, dired_file_name_at_point, dired_buffers_for_dir, &
                             dired_sort_toggle, dired_unmark_all_files, dired_mark_1, dired_insert_directory, &
                             dired_deletion_confirmer, dired_auto_revert_buffer, dired_always_read_filesystem, &
                             dired_free_space, DIRED_FREE_SPACE_SEPARATE, &
                             DIRED_LOCALP_NIL, DIRED_LOCALP_T, DIRED_LOCALP_NO_DIR, DIRED_ARG_NIL
    implicit none(type, external)
    private

    public :: collect_dired_mode_fm

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-dired-mode"

contains

    subroutine collect_dired_mode_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("dired-buffer", test_dired_buffer), &
                    new_unittest("dired-marks", test_marks), &
                    new_unittest("dired-mark-regexp", test_mark_regexp), &
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
                    new_unittest("files-tests-insert-directory-wildcard-in-dir-p", test_wildcard_in_dir_p) &
                    ]
    end subroutine collect_dired_mode_fm

    !> The absolute name of the test directory, as a directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    !> A fresh, empty test directory; *scratch* current and shown alone.
    subroutine setup()
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call init_fileio()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
        call show_alone()
        call kill_dired_buffers()
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        call make_directory_internal(tdir(), st)
        dired_deletion_confirmer => always_yes
        dired_auto_revert_buffer = .false.
        dired_always_read_filesystem = .false.
    end subroutine setup

    !> Remove the test directory and the buffers showing it.
    subroutine teardown()
        type(status_type) :: st
        type(handle_type) :: buf
        call kill_dired_buffers()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        dired_deletion_confirmer => null()
    end subroutine teardown

    !> Kill every Dired buffer of the test directory and its subdirectories.
    subroutine kill_dired_buffers()
        type(handle_type), allocatable :: bufs(:)
        type(status_type) :: st
        integer :: k
        bufs = dired_buffers_for_dir(tdir())
        do k = 1, size(bufs)
            call kill_buffer(bufs(k), st)
        end do
        bufs = dired_buffers_for_dir(tdir()//"sub/")
        do k = 1, size(bufs)
            call kill_buffer(bufs(k), st)
        end do
    end subroutine kill_dired_buffers

    !> A `dired_deletion_confirmer' that always says yes.
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
        type(handle_type) :: buf, old
        type(status_type) :: st
        old = current_buffer()
        call get_buffer_create(" *dired-mode-test-file*", buf, st)
        call set_buffer(buf, st)
        call erase_buffer(st)
        call insert(text, st)
        call write_region(tdir()//name, .false., st)
        call kill_buffer(buf, st)
        call set_buffer(old, st)
    end subroutine make_file

    !> Unread the characters of KEYS.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        integer :: i
        allocate (evs(len(keys)))
        do i = 1, len(keys)
            evs(i) = make_fixnum(int(iachar(keys(i:i)), int64))
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
        type(dired_string_type), allocatable :: files(:)
        type(status_type) :: st
        integer :: k
        call dired_get_marked_files(files, st, localp=DIRED_LOCALP_NO_DIR)
        s = ""
        do k = 1, size(files)
            if (k > 1) s = s//" "
            s = s//files(k)%s
        end do
    end function marked_names

    !> The standard test tree: a.txt, b.txt, c.el and sub/.
    subroutine make_tree()
        type(status_type) :: st
        call make_file("a.txt", "a")
        call make_file("b.txt", "b")
        call make_file("c.el", "c")
        call make_directory_internal(tdir()//"sub", st)
    end subroutine make_tree

    subroutine test_dired_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        character(len=:), allocatable :: file, text, dname, disp
        type(value_type) :: v
        logical :: found
        integer(int64) :: i, pos
        call setup()
        call make_tree()
        body: block
            call dired_noselect(tdir(), buf, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call set_buffer(buf, st)
            i = cur
            call check(error, buffers(i)%major_mode == "dired_mode" .and. buffers(i)%read_only, "mode and read-only")
            if (allocated(error)) exit body
            ! The header line, then `ls -al' output indented by two.
            text = buffer_string()
            dname = tdir()
            dname = dname(1:len(dname) - 1)
            call check(error, index(text, "  "//dname//":"//achar(10)) == 1, text)
            if (allocated(error)) exit body
            ! With `dired_free_space' `first', the "total" line is gone and the
            ! header line's colon shows the available space.
            call check(error, index(text, achar(10)//"  total") == 0, text)
            if (allocated(error)) exit body
            call get_text_property(int(len_trim(dname) + 3, int64), sym("display"), v, st)
            call check(error, stringp(v), "display property on the colon")
            if (allocated(error)) exit body
            disp = xstring(v)
            call check(error, index(disp, ": (") == 1 .and. index(disp, " available)") > 0, disp)
            if (allocated(error)) exit body
            ! Point starts on the first file that is not . or ..
            call dired_get_filename(file, found, st, DIRED_LOCALP_NO_DIR)
            call check(error, found .and. file == "a.txt", "point on a.txt")
            if (allocated(error)) exit body
            pos = dired_goto_file(tdir()//"c.el", st)
            call check(error, pos > 0 .and. pos == point(), "goto c.el")
            if (allocated(error)) exit body
            call dired_get_filename(file, found, st)
            call check(error, found .and. file == tdir()//"c.el", file)
            if (allocated(error)) exit body
            call dired_get_filename(file, found, st, DIRED_LOCALP_T)
            call check(error, found .and. file == "c.el", file)
            if (allocated(error)) exit body
            ! A second `dired' of the same directory reuses the buffer.
            call dired(tdir(), st)
            call check(error, st%ok() .and. same_handle(current_buffer(), buf), "same buffer")
        end block body
        call teardown()
    end subroutine test_dired_buffer

    subroutine test_marks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: pos
        call setup()
        call make_tree()
        body: block
            call dired_noselect(tdir(), buf, st)
            call set_buffer(buf, st)
            pos = dired_goto_file(tdir()//"b.txt", st)
            call dired_mark_1(1_int64, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, marked_names(), "b.txt")
            if (allocated(error)) exit body
            call goto_char(pos)
            call check(error, line_mark() == "*", this_line())
            if (allocated(error)) exit body
            ! Toggling marks all the others (not . and ..).
            call dired_toggle_marks(st)
            call check(error, marked_names(), "a.txt c.el sub")
            if (allocated(error)) exit body
            call dired_unmark_all_files(achar(13), st)
            call check(error, marked_count() == 0, "no marks left")
        end block body
        call teardown()
    end subroutine test_marks

    subroutine test_mark_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        call setup()
        call make_tree()
        body: block
            call dired_noselect(tdir(), buf, st)
            call set_buffer(buf, st)
            call dired_mark_files_regexp("\.txt\'", st)
            call check(error, marked_names(), "a.txt b.txt")
        end block body
        call teardown()
    end subroutine test_mark_regexp

    subroutine test_flag_and_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: pos
        call setup()
        call make_tree()
        body: block
            call dired_noselect(tdir(), buf, st)
            call set_buffer(buf, st)
            pos = dired_goto_file(tdir()//"a.txt", st)
            call dired_flag_file_deletion(1_int64, .false., st)
            call goto_char(pos)
            call check(error, line_mark() == "D", this_line())
            if (allocated(error)) exit body
            call dired_do_flagged_delete(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error,.not. file_exists_p(tdir()//"a.txt"), "a.txt deleted")
            if (allocated(error)) exit body
            call check(error, dired_goto_file(tdir()//"a.txt", st) == 0, "entry removed")
            if (allocated(error)) exit body
            call check(error, dired_goto_file(tdir()//"b.txt", st) > 0, "b.txt still listed")
            if (allocated(error)) exit body
            ! `dired_do_delete' with no marks deletes the file at point.
            call dired_do_delete(DIRED_ARG_NIL, 0_int64, st)
            call check(error, st%ok() .and. .not. file_exists_p(tdir()//"b.txt"), format_error(st))
        end block body
        call teardown()
    end subroutine test_flag_and_delete

    subroutine test_create_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        call setup()
        call make_tree()
        body: block
            call dired_noselect(tdir(), buf, st)
            call set_buffer(buf, st)
            call dired_create_directory(tdir()//"new/deep", st)
            call check(error, st%ok() .and. file_directory_p(tdir()//"new/deep"), format_error(st))
            if (allocated(error)) exit body
            ! The topmost new directory is added to the listing.
            call check(error, dired_goto_file(tdir()//"new", st) > 0, "new listed")
            if (allocated(error)) exit body
            call dired_create_directory(tdir()//"new", st)
            call check(error, format_error(st), "Cannot create directory "//tdir()//"new: file exists")
        end block body
        call teardown()
    end subroutine test_create_directory

    subroutine test_up_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: found
        call setup()
        call make_tree()
        body: block
            call dired(tdir()//"sub/", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            buf = current_buffer()
            call dired_up_directory(st)
            call check(error, st%ok() .and. .not. (same_handle(current_buffer(), buf)), format_error(st))
            if (allocated(error)) exit body
            call dired_get_filename(file, found, st, DIRED_LOCALP_NO_DIR)
            call check(error, found .and. file == "sub", "point on sub")
        end block body
        call teardown()
    end subroutine test_up_directory

    subroutine test_sort_toggle(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        call setup()
        call make_tree()
        body: block
            call dired_noselect(tdir(), buf, st)
            call set_buffer(buf, st)
            call check(error, buffers(cur)%mode_name, "Dired by name")
            if (allocated(error)) exit body
            call dired_sort_toggle(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, buffers(cur)%mode_name, "Dired by date")
            if (allocated(error)) exit body
            call dired_sort_toggle(st)
            call check(error, buffers(cur)%mode_name, "Dired by name")
        end block body
        call teardown()
    end subroutine test_sort_toggle

    !> Keys typed in a Dired buffer: m, n, u and t.
    subroutine test_keys(error)
        use keyboard_fm, only: kbd_store_bytes, command_loop
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        call make_tree()
        body: block
            call dired(tdir(), st)
            call show_alone()
            ! On a.txt: mark it and b.txt (m m), then go back and unmark b.txt.
            call kbd_store_bytes("mmpu")
            call command_loop(st)
            call check(error, marked_names(), "a.txt")
            if (allocated(error)) exit body
            call kbd_store_bytes("t")
            call command_loop(st)
            call check(error, marked_names(), "b.txt c.el sub")
        end block body
        call teardown()
    end subroutine test_keys

    !> C-x d RET lists the default directory; d flags a file and x deletes
    !> it after a yes-or-no question.
    subroutine test_cx_d(error)
        use keyboard_fm, only: kbd_store_bytes, command_loop, last_message
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: found
        call setup()
        call make_tree()
        body: block
            buffers(cur)%directory = tdir()
            dired_deletion_confirmer => null()
            call kbd_store_bytes(achar(24)//"d"//achar(13))
            call command_loop(st)
            call check(error, buffers(cur)%major_mode == "dired_mode" .and. &
                       buffers(cur)%directory == tdir(), "C-x d RET")
            if (allocated(error)) exit body
            call dired_get_filename(file, found, st, DIRED_LOCALP_NO_DIR)
            call check(error, found .and. file == "a.txt", "on a.txt")
            if (allocated(error)) exit body
            call kbd_store_bytes("dxyes"//achar(13))
            call command_loop(st)
            call check(error,.not. file_exists_p(tdir()//"a.txt"), last_message)
            if (allocated(error)) exit body
            call check(error, file_exists_p(tdir()//"b.txt"), "b.txt kept")
        end block body
        call teardown()
    end subroutine test_cx_d

    !> Port of dired-test-bug22694.
    subroutine test_bug22694(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: fbuf
        type(status_type) :: st
        type(dired_string_type), allocatable :: files(:)
        logical :: one
        call setup()
        body: block
            dired_always_read_filesystem = .true.
            call make_file("test", "foo")
            call find_file_noselect(tdir()//"test", fbuf, st)
            call dired(tdir(), st)
            call make_file("test", "bar")
            call dired_mark_files_containing_regexp("bar", st)
            call dired_get_marked_files(files, st, distinguish_one_marked=.true., one_marked=one)
            call check(error, one .and. size(files) == 1, "one marked")
            if (allocated(error)) exit body
            call check(error, files(1)%s, tdir()//"test")
            call kill_buffer(fbuf, st)
        end block body
        call teardown()
    end subroutine test_bug22694

    !> Port of dired-test-bug27243-01.
    subroutine test_bug27243_01(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, dirbuf
        type(handle_type), allocatable :: bufs(:)
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: found
        integer(int64) :: pt1, pt2
        call setup()
        body: block
            dired_auto_revert_buffer = .true.
            bufs = dired_buffers_for_dir(tdir())
            call check(error, size(bufs) == 0, "no Dired buffer yet")
            if (allocated(error)) exit body
            call find_file_noselect(tdir(), dirbuf, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call set_buffer(dirbuf, st)
            call make_directory_internal(tdir()//"test-subdir", st)
            ! Point must be at end-of-buffer.
            call check(error, eobp(), "at eob")
            if (allocated(error)) exit body
            call dired(tdir(), st)
            ! Previous dired call shouldn't create a new buffer.
            bufs = dired_buffers_for_dir(tdir())
            call check(error, size(bufs) == 1 .and. same_handle(current_buffer(), dirbuf), "one buffer")
            if (allocated(error)) exit body
            buf = current_buffer()
            pt1 = point()
            call make_file("test-subdir/test-file", "Test")
            ! Sanity check: point should now be on the subdirectory.
            call dired_file_name_at_point(name, found)
            call check(error, found .and. name == tdir()//"test-subdir/", name)
            if (allocated(error)) exit body
            call dired_find_file(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            pt2 = point()
            call pop_to(buf)
            ! Sanity check: point should now be back on the subdirectory.
            call check(error, point() == pt1, "back on test-subdir")
            if (allocated(error)) exit body
            call dired_find_file(st)
            call check(error, point() == pt2, "on test-file again")
        end block body
        call teardown()
    end subroutine test_bug27243_01

    !> Port of dired-test-bug27243-02.
    subroutine test_bug27243_02(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, dirbuf
        type(status_type) :: st
        character(len=:), allocatable :: name
        logical :: found
        integer(int64) :: pt1
        call setup()
        body: block
            dired_auto_revert_buffer = .true.
            call find_file_noselect(tdir(), dirbuf, st)
            call make_directory_internal(tdir()//"test-subdir", st)
            call dired(tdir(), st)
            buf = current_buffer()
            pt1 = point()
            call make_file("test-subdir/test-file", "Test")
            call dired_file_name_at_point(name, found)
            call check(error, found .and. name == tdir()//"test-subdir/", name)
            if (allocated(error)) exit body
            call dired_find_file(st)
            call pop_to(buf)
            call check(error, point() == pt1, "back on test-subdir")
            if (allocated(error)) exit body
            call dired(tdir(), st)
            call dired_file_name_at_point(name, found)
            call check(error, found .and. name == tdir()//"test-subdir/", name)
        end block body
        call teardown()
    end subroutine test_bug27243_02

    !> Switch the selected window to BUF.
    subroutine pop_to(buf)
        use window_fm, only: pop_to_buffer_same_window
        type(handle_type), intent(in) :: buf
        type(status_type) :: st
        call pop_to_buffer_same_window(buf, st)
    end subroutine pop_to

    !> Port of dired-test-bug27968.
    subroutine test_bug27968(error)
        use simple_fm, only: line_number_at_pos
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        character(len=:), allocatable :: file, new_header
        logical :: found
        integer(int64) :: orig_len, len2, diff, pos, line_nb, shortage, opoint
        call setup()
        body: block
            call make_directory_internal(tdir()//"subdir", st)
            call dired_noselect(tdir()//"subdir", buf, st)
            call set_buffer(buf, st)
            orig_len = header_len()
            pos = point()
            line_nb = line_number_at_pos()
            call make_directory_internal(tdir()//"subdir/subdir", st)
            call dired_revert(.false., .false., st)
            ! Change the header line.
            opoint = point()
            call goto_char(1_int64)
            shortage = forward_line(1_int64)
            new_header = "  test-bug27968"
            if (orig_len == len(new_header)) new_header = new_header//" :-)"
            block
                use data_fm, only: set
                use lread_fm, only: sym
                use lisp_fm, only: t_value, nil
                call set(sym("inhibit_read_only"), t_value(), st)
                call delete_region(point(), line_end_position(), st)
                call insert(new_header, st)
                call set(sym("inhibit_read_only"), nil(), st)
            end block
            len2 = header_len()
            diff = len2 - orig_len
            ! (save-excursion: point moved with the text before it)
            call goto_char(opoint + diff)
            call check(error, diff /= 0, "header length has changed")
            if (allocated(error)) exit body
            call check(error, line_nb == line_number_at_pos() .and. line_nb == line_number_at_pos(pos + diff), &
                       "same line")
            if (allocated(error)) exit body
            ! After revert, the point must be in 'subdir' line.
            call dired_get_filename(file, found, st, DIRED_LOCALP_T, .true.)
            call check(error, found .and. file == "subdir", file)
        end block body
        call teardown()
    contains
        integer(int64) function header_len()
            integer(int64) :: p, s
            p = point()
            call goto_char(1_int64)
            s = forward_line(1_int64)
            header_len = line_end_position() - point()
            call goto_char(p)
        end function header_len
    end subroutine test_bug27968

    !> The five empty directories and zeta-empty-dir of dired-test-with-temp-dirs,
    !> plus five non-empty ones unless JUST_EMPTY_DIRS; then all marked.
    subroutine bug27940_tree(just_empty_dirs)
        logical, intent(in) :: just_empty_dirs
        type(status_type) :: st
        character(len=1) :: d
        integer :: i
        call setup()
        do i = 0, 4
            write (d, "(i1)") i
            call make_directory_internal(tdir()//"empty-dir-"//d, st)
        end do
        if (.not. just_empty_dirs) then
            do i = 0, 4
                write (d, "(i1)") i
                call make_directory_internal(tdir()//"non-empty-"//d, st)
                call make_directory_internal(tdir()//"non-empty-"//d//"/foo", st)
            end do
        end if
        call make_directory_internal(tdir()//"zeta-empty-dir", st)
        call dired(tdir(), st)
        call dired_toggle_marks(st)
    end subroutine bug27940_tree

    !> The number of marked files.
    integer function marked_count()
        type(dired_string_type), allocatable :: files(:)
        type(status_type) :: st
        logical :: one
        call dired_get_marked_files(files, st, distinguish_one_marked=.true., one_marked=one)
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
            call dired_do_delete(DIRED_ARG_NIL, 0_int64, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, marked_count() == 0, "all dirs deleted")
            if (allocated(error)) exit body
            call discard_input()
            ! Answer yes (each time).
            call bug27940_tree(.false.)
            call type_keys(repeat("yes"//achar(13), 5))
            call dired_do_delete(DIRED_ARG_NIL, 0_int64, st)
            call check(error, marked_count() == 0, "yes: all dirs deleted")
            if (allocated(error)) exit body
            ! Answer no.
            call bug27940_tree(.false.)
            call type_keys(repeat("no"//achar(13), 5))
            call dired_do_delete(DIRED_ARG_NIL, 0_int64, st)
            call check(error, marked_count() == 5, "no: just the empty dirs deleted")
            if (allocated(error)) exit body
            ! Answer all.
            call bug27940_tree(.false.)
            call type_keys("all"//achar(13))
            call dired_do_delete(DIRED_ARG_NIL, 0_int64, st)
            call check(error, marked_count() == 0, "all: all dirs deleted")
            if (allocated(error)) exit body
            ! Answer quit.
            call bug27940_tree(.false.)
            call type_keys("quit"//achar(13))
            call dired_do_delete(DIRED_ARG_NIL, 0_int64, st)
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
            call dired(tdir()//"bla", st)
            errbuf = get_buffer("*ls error*")
            call check(error, buffer_live_p(errbuf), "*ls error* exists")
            if (allocated(error)) exit body
            call check(error,.not. (buffers(cur)%name == "bla"), "the buffer visiting bla is killed")
            if (allocated(error)) exit body
            call set_buffer(errbuf, st)
            text = buffer_string()
            call check(error, index(text, "bla") > 0, text)
            call kill_buffer(errbuf, st)
        end block body
        call teardown()
    end subroutine test_ls_error

    !> Port of files-tests-insert-directory-shows-files.
    subroutine test_insert_directory_files(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: saved
        call setup()
        call make_file("foo", "")
        call make_file("bar", "")
        call get_buffer_create("files-tests--insert-directory", buf, st)
        call set_buffer(buf, st)
        saved = dired_free_space
        dired_free_space = DIRED_FREE_SPACE_SEPARATE
        call dired_insert_directory(tdir(), "-l", st)
        dired_free_space = saved
        text = buffer_string()
        call check(error, st%ok() .and. index(text, "foo") > 0 .and. index(text, "bar") > 0, text)
        if (.not. allocated(error)) then
            ! dired-free-space `separate': the total line says so, with the free space.
            call check(error, index(text, "total used in directory ") > 0 .and. index(text, " available ") > 0, text)
        end if
        call kill_buffer(buf, st)
        call teardown()
    end subroutine test_insert_directory_files

    !> Port of files-tests-insert-directory-wildcard-in-dir-p.
    subroutine test_wildcard_in_dir_p(error)
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
        subroutine one(path, dir, wild)
            character(len=*), intent(in) :: path, dir, wild
            character(len=:), allocatable :: d, w
            logical :: found
            found = insert_directory_wildcard_in_dir_p(path, d, w)
            if (len(dir) == 0) then
                call check(error,.not. found, path)
            else
                call check(error, found, path)
                if (allocated(error)) return
                call check(error, d//"|"//w, dir//"|"//wild)
            end if
        end subroutine one
    end subroutine test_wildcard_in_dir_p
end module test_dired_mode_fm
