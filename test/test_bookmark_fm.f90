! test_bookmark_fm.f90 -- tests for bookmark_fm.
! Ports GNU Emacs 31.1 test/lisp/bookmark-tests.el.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> Ports the tests of bookmark-tests.el; the resource files
!> test/bookmark-resources/test.bmk and test-list.bmk are upstream's in
!> namelist syntax.  `bookmark_make_record_function' returning upstream's
!> SECOND format (a nested table) is not applicable: records are typed.
module test_bookmark_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, list_length, list_elt, xstring
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, kill_buffer, get_buffer_create, &
                         generate_new_buffer_name, restore_buffer_modified_p, get_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, goto_char, point_min, point_max, point
    use cmds_fm, only: forward_line, beginning_of_line, forward_char
    use regexp_opt_fm, only: regexp_unmatchable
    use fileio_fm, only: expand_file_name, emacs_wd, init_fileio, file_name_as_directory, file_attributes
    use sysdep_fm, only: file_stat_type
    use files_fm, only: make_temp_file, delete_file, find_file_noselect, abbreviate_file_name
    use search_fm, only: string_match, looking_at, re_search_forward
    use derived_fm, only: derived_mode_p
    use window_fm, only: switch_to_buffer, window_configuration_type, current_window_configuration, same_handle, &
                         set_window_configuration
    use subr_fm, only: history_delete_duplicates
    use bindings_fm, only: init_bindings
    use bookmark_fm, only: bookmark_type, bookmark_table, bookmark_default_file, bookmark_bookmarks_timestamp, &
                           bookmark_save_flag, BOOKMARK_SAVE_FLAG_NIL, bookmark_use_annotations, &
                           bookmark_make_record_function, bookmark_all_names, bookmark_get_bookmark, &
                           bookmark_set_name, bookmark_get_filename, bookmark_set_filename, bookmark_get_position, &
                           bookmark_set_position, bookmark_get_front_context_string, &
                           bookmark_set_front_context_string, bookmark_get_rear_context_string, &
                           bookmark_set_rear_context_string, bookmark_maybe_historicize_string, &
                           bookmark_make_record, bookmark_make_record_default, bookmark_set, &
                           bookmark_set_no_overwrite, bookmark_set_internal, bookmark_kill_line, &
                           bookmark_default_annotation_text, bookmark_insert_annotation, bookmark_set_annotation, &
                           bookmark_get_annotation, bookmark_edit_annotation, bookmark_edit_annotation_confirm, &
                           bookmark_jump, bookmark_insert_location, bookmark_location, bookmark_rename, &
                           bookmark_insert, bookmark_delete, bookmark_delete_all, bookmark_save, bookmark_load, &
                           bookmark_maybe_rename, bookmark_store, BOOKMARK_SET_PUSH, bookmark_bmenu_buffer, &
                           bookmark_bmenu_list, bookmark_bmenu_edit_annotation, bookmark_bmenu_toggle_filenames, &
                           bookmark_bmenu_toggle_filenames_command, bookmark_bmenu_show_filenames, &
                           bookmark_bmenu_hide_filenames, bookmark_bmenu_bookmark, bookmark_bmenu_mark, &
                           bookmark_bmenu_any_marks, bookmark_bmenu_ensure_position, bookmark_bmenu_mark_all, &
                           bookmark_bmenu_unmark, bookmark_bmenu_unmark_all, bookmark_bmenu_delete, &
                           bookmark_bmenu_execute_deletions, bookmark_bmenu_delete_all, bookmark_bmenu_locate, &
                           bookmark_bmenu_filter_table_by_regexp
    use minibuf_fm, only: minibuffer_string_type
    implicit none(type, external)
    private

    public :: collect_bookmark_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_bookmark_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("bookmark-tests-all-names", test_all_names), &
                    new_unittest("bookmark-tests-get-bookmark", test_get_bookmark), &
                    new_unittest("bookmark-tests-get-bookmark-record", test_get_bookmark_record), &
                    new_unittest("bookmark-tests-all-names-list", test_all_names_list), &
                    new_unittest("bookmark-tests-get-bookmark-list", test_get_bookmark_list), &
                    new_unittest("bookmark-tests-get-bookmark-record-list", test_get_bookmark_record_list), &
                    new_unittest("bookmark-tests-record-getters-and-setters-new", test_getters_and_setters), &
                    new_unittest("bookmark-tests-maybe-historicize-string", test_maybe_historicize_string), &
                    new_unittest("bookmark-tests-make-record", test_make_record), &
                    new_unittest("bookmark-tests-make-record-list", test_make_record_list), &
                    new_unittest("bookmark-tests-make-record-function", test_make_record_function), &
                    new_unittest("bookmark-tests-set", test_set), &
                    new_unittest("bookmark-tests-set/bookmark-use-annotations-t", test_set_use_annotations), &
                    new_unittest("bookmark-tests-kill-line", test_kill_line), &
                    new_unittest("bookmark-tests-default-annotation-text", test_default_annotation_text), &
                    new_unittest("bookmark-tests-insert-annotation", test_insert_annotation), &
                    new_unittest("bookmark-tests-edit-annotation", test_edit_annotation), &
                    new_unittest("bookmark-tests-jump", test_jump), &
                    new_unittest("bookmark-tests-insert-location", test_insert_location), &
                    new_unittest("bookmark-tests-location", test_location), &
                    new_unittest("bookmark-tests-rename", test_rename), &
                    new_unittest("bookmark-tests-insert", test_insert), &
                    new_unittest("bookmark-tests-delete", test_delete), &
                    new_unittest("bookmark-tests-delete-all", test_delete_all), &
                    new_unittest("bookmark-tests-save", test_save), &
                    new_unittest("bookmark-tests-save/non-ascii-bookmark-name", test_save_non_ascii_name), &
                    new_unittest("bookmark-tests-save/non-ascii-annotation", test_save_non_ascii_annotation), &
                    new_unittest("bookmark-tests-maybe-rename", test_maybe_rename), &
                    new_unittest("bookmark-tests-load", test_load), &
                    new_unittest("bookmark-test-bmenu-edit-annotation/show-annotation", test_bmenu_edit_annotation), &
                    new_unittest("bookmark-test-bmenu-send-edited-annotation", test_bmenu_send_edited_annotation), &
                    new_unittest("bookmark-test-bmenu-send-edited-annotation/restore-focus", test_bmenu_restore_focus), &
                    new_unittest("bookmark-test-bmenu-toggle-filenames", test_bmenu_toggle_filenames), &
                    new_unittest("bookmark-test-bmenu-toggle-filenames/show", test_bmenu_toggle_filenames_show), &
                    new_unittest("bookmark-test-bmenu-show-filenames", test_bmenu_show_filenames), &
                    new_unittest("bookmark-test-bmenu-hide-filenames", test_bmenu_hide_filenames), &
                    new_unittest("bookmark-test-bmenu-bookmark", test_bmenu_bookmark), &
                    new_unittest("bookmark-test-bmenu-mark", test_bmenu_mark), &
                    new_unittest("bookmark-test-bmenu-any-marks", test_bmenu_any_marks), &
                    new_unittest("bookmark-test-bmenu-mark-all", test_bmenu_mark_all), &
                    new_unittest("bookmark-test-bmenu-any-marks-list", test_bmenu_any_marks_list), &
                    new_unittest("bookmark-test-bmenu-unmark", test_bmenu_unmark), &
                    new_unittest("bookmark-test-bmenu-unmark-all", test_bmenu_unmark_all), &
                    new_unittest("bookmark-test-bmenu-delete", test_bmenu_delete), &
                    new_unittest("bookmark-test-bmenu-delete-all", test_bmenu_delete_all), &
                    new_unittest("bookmark-test-bmenu-locate", test_bmenu_locate), &
                    new_unittest("bookmark-test-bmenu-filter-alist-by-regexp", test_bmenu_filter_table_by_regexp) &
                    ]
    end subroutine collect_bookmark_fm

    ! ------------------------------------------------------------------
    ! The environment of the tests
    ! ------------------------------------------------------------------

    !> The file NAME of the resources.
    function resource_file(name) result(file)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: file
        type(status_type) :: st

        call init_fileio()
        call expand_file_name("test/bookmark-resources", file, st, emacs_wd)
        file = file_name_as_directory(file)//name
    end function resource_file

    !> Example file used for testing (abbreviated, to match the behavior
    !> of `bookmark_buffer_file_name').
    function example_file() result(file)
        character(len=:), allocatable :: file

        file = abbreviate_file_name(resource_file("example.txt"))
    end function example_file

    !> A record of upstream's cached bookmarks.
    function cached(name, filename, front, rear, position) result(b)
        character(len=*), intent(in) :: name, filename, front, rear
        integer(int64), intent(in) :: position
        type(bookmark_type) :: b

        b%name = name
        b%filename = filename
        b%front_context_string = front
        b%rear_context_string = rear
        b%position = position
    end function cached

    !> The values below should match `bookmark-tests-bookmark-file'.
    function tests_bookmark() result(b)
        type(bookmark_type) :: b

        b = cached("name", "/some/file", "abc", "def", 3_int64)
    end function tests_bookmark

    !> Point the default file and its timestamp at resource FILE, with
    !> saving off.
    subroutine use_default_file(file)
        character(len=*), intent(in) :: file
        type(file_stat_type) :: stat
        logical :: found

        bookmark_default_file = resource_file(file)
        call file_attributes(bookmark_default_file, stat, found)
        bookmark_bookmarks_timestamp%file = bookmark_default_file
        bookmark_bookmarks_timestamp%mtime_known = found
        bookmark_bookmarks_timestamp%mtime_sec = stat%mtime_sec
        bookmark_bookmarks_timestamp%mtime_nsec = stat%mtime_nsec
        bookmark_save_flag = BOOKMARK_SAVE_FLAG_NIL
        bookmark_use_annotations = .false.
    end subroutine use_default_file

    !> (with-bookmark-test): a temporary buffer is current; `bookmark_table'
    !> is upstream's cached bookmark.
    subroutine with_bookmark_test(old)
        type(handle_type), intent(out) :: old
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call use_default_file("test.bmk")
        if (allocated(bookmark_table)) deallocate (bookmark_table)
        allocate (bookmark_table(1))
        bookmark_table(1) = tests_bookmark()
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
    end subroutine with_bookmark_test

    !> (with-bookmark-test-list): as `with_bookmark_test', with the three
    !> cached bookmarks of the list.
    subroutine with_bookmark_test_list(old)
        type(handle_type), intent(out) :: old
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call use_default_file("test-list.bmk")
        if (allocated(bookmark_table)) deallocate (bookmark_table)
        allocate (bookmark_table(3))
        bookmark_table(1) = cached("name-0", "/some/file-0", "ghi", "jkl", 4_int64)
        bookmark_table(2) = cached("name-1", "/some/file-1", "mno", "pqr", 5_int64)
        bookmark_table(3) = cached("name-2", "/some/file-2", "stu", "vwx", 6_int64)
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
    end subroutine with_bookmark_test_list

    !> The end of a test: kill the temporary buffer and go back to OLD.
    subroutine done(old)
        type(handle_type), intent(in) :: old
        type(status_type) :: st

        call restore_buffer_modified_p(.false.)
        call kill_buffer(current_buffer(), st)
        call set_buffer(old, st)
    end subroutine done

    !> The buffer visiting the example file.
    subroutine example_buffer(buffer)
        type(handle_type), intent(out) :: buffer
        type(status_type) :: st

        call find_file_noselect(example_file(), buffer, st)
    end subroutine example_buffer

    !> Whether bookmarks A and B are `equal' (LAST_MODIFIED left out with
    !> NO_TIME, as `bookmark-remove-last-modified' does).
    logical function same_bookmark(a, b, no_time) result(same)
        type(bookmark_type), intent(in) :: a, b
        logical, intent(in), optional :: no_time
        logical :: time
        integer :: k

        time = .true.
        if (present(no_time)) time = .not. no_time
        same = same_string(a%name, b%name) .and. same_string(a%filename, b%filename) .and. &
               a%position == b%position .and. same_string(a%front_context_string, b%front_context_string) .and. &
               same_string(a%rear_context_string, b%rear_context_string) .and. &
               same_string(a%annotation, b%annotation)
        if (.not. same) return

        if (time) then
            same = allocated(a%last_modified) .eqv. allocated(b%last_modified)
            if (.not. same) return
            if (allocated(a%last_modified)) same = all(a%last_modified == b%last_modified)
            if (.not. same) return
        end if

        same = allocated(a%defaults) .eqv. allocated(b%defaults)
        if (.not. same .or. .not. allocated(a%defaults)) return
        same = size(a%defaults) == size(b%defaults)
        if (.not. same) return

        do k = 1, size(a%defaults)
            same = same_string(a%defaults(k)%s, b%defaults(k)%s)
            if (.not. same) return
        end do

    contains

        logical function same_string(x, y)
            character(len=:), allocatable, intent(in) :: x, y

            same_string = allocated(x) .eqv. allocated(y)

            if (same_string .and. allocated(x)) then
                same_string = len(x) == len(y)
                if (same_string) same_string = x == y
            end if
        end function same_string
    end function same_bookmark

    !> Whether the bookmark names are NAMES.
    logical function names_are(names) result(same)
        character(len=*), intent(in) :: names(:)
        type(minibuffer_string_type), allocatable :: all(:)
        integer :: k

        all = bookmark_all_names()
        same = size(all) == size(names)
        if (.not. same) return

        do k = 1, size(names)
            same = all(k)%s == trim(names(k)) .and. len(all(k)%s) == len_trim(names(k))
            if (.not. same) return
        end do
    end function names_are

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    subroutine test_all_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test(old)
        call check(error, names_are(["name"]), "name")
        call done(old)
    end subroutine test_all_names

    subroutine test_get_bookmark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k

        call with_bookmark_test(old)
        k = bookmark_get_bookmark("name", st)
        call check(error, k == 1, "found")
        if (.not. allocated(error)) call check(error, same_bookmark(bookmark_table(k), tests_bookmark()), "cached")
        call done(old)
    end subroutine test_get_bookmark

    subroutine test_get_bookmark_record(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test(old)
        call check(error, bookmark_get_filename("name") == "/some/file" .and. bookmark_get_position("name") == 3 &
                   .and. bookmark_get_front_context_string("name") == "abc" .and. &
                   bookmark_get_rear_context_string("name") == "def", "the record")
        call done(old)
    end subroutine test_get_bookmark_record

    subroutine test_all_names_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test_list(old)
        call check(error, names_are([character(len=6) :: "name-0", "name-1", "name-2"]), "three names")
        call done(old)
    end subroutine test_all_names_list

    subroutine test_get_bookmark_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test_list(old)
        call check(error, same_bookmark(bookmark_table(bookmark_get_bookmark("name-0", st)), &
                                        cached("name-0", "/some/file-0", "ghi", "jkl", 4_int64)), "name-0")

        if (.not. allocated(error)) then
            call check(error, same_bookmark(bookmark_table(bookmark_get_bookmark("name-1", st)), &
                                            cached("name-1", "/some/file-1", "mno", "pqr", 5_int64)), "name-1")
        end if

        if (.not. allocated(error)) then
            call check(error, same_bookmark(bookmark_table(bookmark_get_bookmark("name-2", st)), &
                                            cached("name-2", "/some/file-2", "stu", "vwx", 6_int64)), "name-2")
        end if

        call done(old)
    end subroutine test_get_bookmark_list

    subroutine test_get_bookmark_record_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test_list(old)
        call check(error, bookmark_get_filename("name-0") == "/some/file-0" .and. &
                   bookmark_get_position("name-0") == 4 .and. bookmark_get_front_context_string("name-1") == "mno" &
                   .and. bookmark_get_rear_context_string("name-2") == "vwx", "the records")
        call done(old)
    end subroutine test_get_bookmark_record_list

    subroutine test_getters_and_setters(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(bookmark_type) :: bmk
        type(status_type) :: st

        call with_bookmark_test(old)
        buffers(cur)%filename = "test"
        call bookmark_make_record(bmk, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! (The record is stored, so that the setters, which take a name,
        ! reach it.)
        call bookmark_store(bmk%name, bmk, .true., st)
        call bookmark_set_name(bmk%name, "foobar", st)
        call check(error, bookmark_get_bookmark("foobar", st) == 1, "foobar")
        if (allocated(error)) return
        call bookmark_set_filename("foobar", "file/name", st)
        call check(error, bookmark_get_filename("foobar"), "file/name")
        if (allocated(error)) return
        call bookmark_set_position("foobar", 123_int64, st)
        call check(error, bookmark_get_position("foobar") == 123, "123")
        if (allocated(error)) return
        call bookmark_set_front_context_string("foobar", "front", st)
        call check(error, bookmark_get_front_context_string("foobar"), "front")
        if (allocated(error)) return
        call bookmark_set_rear_context_string("foobar", "rear", st)
        call check(error, bookmark_get_rear_context_string("foobar"), "rear")
        deallocate (buffers(cur)%filename)
        call done(old)
    end subroutine test_getters_and_setters

    subroutine test_maybe_historicize_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: history
        type(status_type) :: st
        logical :: saved

        call init_bindings()
        call set(sym("bookmark_history"), nil(), st)
        saved = history_delete_duplicates
        history_delete_duplicates = .true.
        call bookmark_maybe_historicize_string("foo")
        call bookmark_maybe_historicize_string("foo")
        history_delete_duplicates = saved
        call symbol_value(sym("bookmark_history"), history, st)
        call check(error, list_length(history) == 1, "one element")
        if (allocated(error)) return
        call check(error, xstring(list_elt(history, 1_int64)), "foo")
    end subroutine test_maybe_historicize_string

    !> The record made at position 3 of the example file.
    function example_record() result(b)
        type(bookmark_type) :: b

        b%name = "example.txt"
        b%filename = example_file()
        b%front_context_string = "is text file is "
        b%position = 3
        allocate (b%defaults(1))
        b%defaults(1)%s = "example.txt"
    end function example_record

    subroutine make_record_twice(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer
        type(bookmark_type) :: record
        type(status_type) :: st
        integer :: n

        call example_buffer(buffer)
        call set_buffer(buffer, st)
        call goto_char(3_int64)

        do n = 1, 2
            ! calling twice gives same record
            call bookmark_make_record(record, st)
            call check(error, same_bookmark(record, example_record(), no_time=.true.), "the record")
            if (allocated(error)) exit
        end do

        call kill_buffer(buffer, st)
    end subroutine make_record_twice

    subroutine test_make_record(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test(old)
        call make_record_twice(error)
        call done(old)
    end subroutine test_make_record

    subroutine test_make_record_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test_list(old)
        call make_record_twice(error)
        call done(old)
    end subroutine test_make_record_list

    recursive subroutine named_record(record, status)
        type(bookmark_type), intent(out) :: record
        type(status_type), intent(out) :: status

        status = status_type()
        record%name = "<name>"
    end subroutine named_record

    recursive subroutine position_record(record, status)
        type(bookmark_type), intent(out) :: record
        type(status_type), intent(out) :: status

        status = status_type()
        record%position = 2
    end subroutine position_record

    subroutine test_make_record_function(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(bookmark_type) :: record, expected
        type(status_type) :: st

        call with_bookmark_test(old)
        buffers(cur)%filename = "test"
        ! Named bookmark
        bookmark_make_record_function => named_record
        call bookmark_make_record(record, st)
        expected%name = "<name>"
        call check(error, same_bookmark(record, expected), "named")

        if (.not. allocated(error)) then
            ! (CURRENT format)
            bookmark_make_record_function => position_record
            call bookmark_make_record(record, st)
            expected%name = "test"
            expected%position = 2
            allocate (expected%defaults(1))
            expected%defaults(1)%s = "test"
            call check(error, same_bookmark(record, expected), "position")
        end if

        bookmark_make_record_function => default_record
        deallocate (buffers(cur)%filename)
        call done(old)
    end subroutine test_make_record_function

    recursive subroutine default_record(record, status)
        type(bookmark_type), intent(out) :: record
        type(status_type), intent(out) :: status

        call bookmark_make_record_default(record, status)
    end subroutine default_record

    !> Whether `bookmark_table' (without last-modified times) is RECORDS.
    logical function table_is(records) result(same)
        type(bookmark_type), intent(in) :: records(:)
        integer :: k

        same = size(bookmark_table) == size(records)
        if (.not. same) return

        do k = 1, size(records)
            same = same_bookmark(bookmark_table(k), records(k), no_time=.true.)
            if (.not. same) return
        end do
    end function table_is

    subroutine test_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, buffer
        type(bookmark_type) :: bmk1, bmk2, two(2)
        type(status_type) :: st

        call with_bookmark_test(old)
        bmk1%name = "foo"
        bmk1%filename = example_file()
        bmk1%front_context_string = "This text file i"
        bmk1%position = 1
        bmk2%name = "foo"
        bmk2%filename = example_file()
        bmk2%rear_context_string = ".txt ends here."//achar(10)
        bmk2%position = 72
        deallocate (bookmark_table)
        allocate (bookmark_table(0))
        call example_buffer(buffer)
        call set_buffer(buffer, st)

        steps: block
            ! 1. bookmark-set
            ! Set first bookmark
            call goto_char(point_min())
            call bookmark_set(st, "foo")
            call check(error, table_is([bmk1]), "bmk1")
            if (allocated(error)) exit steps
            ! Replace that bookmark
            call goto_char(point_max())
            call bookmark_set(st, "foo")
            call check(error, table_is([bmk2]), "bmk2")
            if (allocated(error)) exit steps
            ! Push another bookmark with the same name
            call goto_char(point_min())
            ! NO-OVERWRITE is t
            call bookmark_set(st, "foo", .true.)
            two(1) = bmk1
            two(2) = bmk2
            call check(error, table_is(two), "bmk1 bmk2")
            if (allocated(error)) exit steps
            ! 2. bookmark-set-no-overwrite
            ! Don't overwrite
            call bookmark_set_no_overwrite(st, "foo")
            call check(error,.not. st%ok(), "should error")
            if (allocated(error)) exit steps
            ! Set new bookmark
            deallocate (bookmark_table)
            allocate (bookmark_table(0))
            call bookmark_set_no_overwrite(st, "foo")
            call check(error, table_is([bmk1]), "bmk1 again")
            if (allocated(error)) exit steps
            ! Push another bookmark with the same name
            call goto_char(point_max())
            ! PUSH-BOOKMARK is t
            call bookmark_set_no_overwrite(st, "foo", .true.)
            two(1) = bmk2
            two(2) = bmk1
            call check(error, table_is(two), "bmk2 bmk1")
            if (allocated(error)) exit steps
            ! 3. bookmark-set-internal
            call bookmark_set_internal("foo", BOOKMARK_SET_PUSH + 1, st, "bar")
            call check(error,.not. st%ok(), "should error")
        end block steps

        call kill_buffer(buffer, st)
        call done(old)
    end subroutine test_set

    subroutine test_set_use_annotations(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, buffer
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bookmark_test(old)
        bookmark_use_annotations = .true.
        call show_alone()
        call current_window_configuration(config)
        call example_buffer(buffer)
        call switch_to_buffer(buffers(buffer%index)%name, st)

        steps: block
            ! Should jump to edit annotation buffer
            call bookmark_set(st, "foo")
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit steps
            call check(error, derived_mode_p("bookmark_edit_annotation_mode"), "in the annotation mode")
            if (allocated(error)) exit steps
            ! Should return to the original buffer
            call bookmark_edit_annotation_confirm(st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit steps
            call check(error, same_handle(current_buffer(), buffer), "back in the buffer")
        end block steps

        bookmark_use_annotations = .false.
        call set_window_configuration(config)
        call kill_buffer(buffer, st)
        call set_buffer(old, st)
    end subroutine test_set_use_annotations

    subroutine test_kill_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test(old)
        call insert("foobar"//achar(10), st)
        call goto_char(point_min())
        call bookmark_kill_line()
        call check(error, buffer_string(), achar(10))
        call done(old)
        if (allocated(error)) return
        call with_bookmark_test(old)
        call insert("foobar"//achar(10), st)
        call goto_char(point_min())
        ! including newline
        call bookmark_kill_line(.true.)
        call check(error, buffer_string(), "")
        call done(old)
    end subroutine test_kill_line

    subroutine test_default_annotation_text(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call init_bindings()
        call check(error, string_match("foo", bookmark_default_annotation_text("foo"), st) > 0, "foo")
    end subroutine test_default_annotation_text

    subroutine test_insert_annotation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test(old)
        call bookmark_insert_annotation("a missing bookmark", st)
        call check(error,.not. st%ok(), "should error")

        if (.not. allocated(error)) then
            call bookmark_insert_annotation("name", st)
            call check(error, string_match("Type the annotation", buffer_string(), st) > 0, buffer_string())
        end if

        call done(old)
        if (allocated(error)) return
        call with_bookmark_test(old)
        call bookmark_set_annotation("name", "some stuff", st)
        call bookmark_insert_annotation("name", st)
        call check(error, string_match("some stuff", buffer_string(), st) > 0, buffer_string())
        call done(old)
    end subroutine test_insert_annotation

    subroutine test_edit_annotation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bookmark_test(old)
        call current_window_configuration(config)
        call bookmark_edit_annotation("name", st)
        call insert("new text", st)
        call bookmark_edit_annotation_confirm(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, bookmark_get_annotation("name"), "new text")
        call set_window_configuration(config)
        call done(old)
    end subroutine test_edit_annotation

    subroutine test_jump(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, buffer
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bookmark_test(old)
        call current_window_configuration(config)
        call example_buffer(buffer)
        call set_buffer(buffer, st)
        call bookmark_set(st, "test")
        call set_buffer(old, st)
        call bookmark_jump("test", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, same_handle(current_buffer(), buffer), "in the buffer")

        if (.not. allocated(error)) then
            call bookmark_jump(status=st)
            call check(error,.not. st%ok(), "should error")
        end if

        call set_window_configuration(config)
        call kill_buffer(buffer, st)
        call set_buffer(old, st)
        call done(old)
    end subroutine test_jump

    subroutine test_insert_location(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test(old)
        call bookmark_insert_location("name", st)
        call check(error, buffer_string(), "/some/file")
        call done(old)
    end subroutine test_insert_location

    subroutine test_location(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old

        call with_bookmark_test(old)
        call check(error, bookmark_location("name"), "/some/file")
        call done(old)
    end subroutine test_location

    subroutine test_rename(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test(old)
        call bookmark_rename("name", st, "newname")
        call check(error, names_are(["newname"]), "newname")
        call done(old)
    end subroutine test_rename

    subroutine test_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, buffer, temp
        type(status_type) :: st

        call with_bookmark_test(old)
        temp = current_buffer()
        call example_buffer(buffer)
        call set_buffer(buffer, st)
        call bookmark_set(st, "test")
        call set_buffer(temp, st)
        call bookmark_insert("test", st)
        call check(error, string_match("^This text file is used by bookmark-tests.el", buffer_string(), st) == 1, &
                   buffer_string())
        call kill_buffer(buffer, st)
        call set_buffer(temp, st)
        call done(old)
    end subroutine test_insert

    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test(old)
        call bookmark_delete("name", st)
        call check(error, size(bookmark_table) == 0, "no bookmarks")
        call done(old)
    end subroutine test_delete

    subroutine test_delete_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(status_type) :: st

        call with_bookmark_test_list(old)
        call bookmark_delete_all(st, .true.)
        call check(error, size(bookmark_table) == 0, "no bookmarks")
        call done(old)
    end subroutine test_delete_all

    !> (with-bookmark-test-save-load): after BODY (CHANGE), save the
    !> bookmarks to a temporary file and load them back.
    subroutine save_load(error, change, value)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: change
        character(len=*), intent(in) :: value
        type(handle_type) :: old
        type(bookmark_type), allocatable :: old_table(:)
        type(status_type) :: st
        character(len=:), allocatable :: file
        integer :: k

        call with_bookmark_test(old)
        call make_temp_file("bookmark-tests", file, st)
        bookmark_default_file = file

        select case (change)
        case (1)
            call bookmark_set_name("name", value, st)
        case (2)
            call bookmark_set_annotation("name", value, st)
        case default
            continue
        end select

        allocate (old_table(size(bookmark_table)))

        do k = 1, size(bookmark_table)
            old_table(k) = bookmark_table(k)
        end do

        call bookmark_save(st, file=file, make_default=.true.)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            deallocate (bookmark_table)
            allocate (bookmark_table(0))
            call bookmark_load(file, st, no_msg=.true.)
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) call check(error, table_is(old_table), "the bookmarks again")
        call delete_file(file, st)
        call delete_file(file//"~", st)
        call done(old)
    end subroutine save_load

    !> Non-ASCII text, with a character beyond Unicode (the
    !> chinese-gb18030 decoding of upstream's "\xE3\x32\x9A\x36", #x110000)
    !> and a raw byte (#x81), which do not yield valid UTF-8 byte
    !> sequences.
    function non_ascii_data() result(s)
        character(len=:), allocatable :: s

        s = "Здра́вствуйте!"//"中文,普通话,汉语"//"åäöøñ"//"こんにちは"//"你好"//"Dobrý deň"// &
            "∀ p ∈ world • hello p  □"//char(244)//char(144)//char(128)//char(128)//char(193)//char(129)
    end function non_ascii_data

    subroutine test_save(error)
        type(error_type), allocatable, intent(out) :: error

        ! Just run the default test according to the macro.
        call save_load(error, 0, "")
    end subroutine test_save

    subroutine test_save_non_ascii_name(error)
        type(error_type), allocatable, intent(out) :: error

        call save_load(error, 1, non_ascii_data())
    end subroutine test_save_non_ascii_name

    subroutine test_save_non_ascii_annotation(error)
        type(error_type), allocatable, intent(out) :: error

        call save_load(error, 2, non_ascii_data())
    end subroutine test_save_non_ascii_annotation

    subroutine test_maybe_rename(error)
        type(error_type), allocatable, intent(out) :: error
        type(bookmark_type) :: bookmark
        type(minibuffer_string_type) :: names(1)

        bookmark%name = "foo"
        names(1)%s = "foo"
        call bookmark_maybe_rename(bookmark, names)
        call check(error, bookmark%name, "foo<2>")
    end subroutine test_maybe_rename

    subroutine test_load(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(bookmark_type) :: one(1), two(2)
        type(status_type) :: st

        call with_bookmark_test(old)
        one(1) = tests_bookmark()

        steps: block
            call bookmark_load("no/such/file", st)
            call check(error,.not. st%ok(), "should error")
            if (allocated(error)) exit steps
            deallocate (bookmark_table)
            allocate (bookmark_table(0))
            call bookmark_load(resource_file("test.bmk"), st, no_msg=.true.)
            call check(error, table_is(one), "loaded")
            if (allocated(error)) exit steps
            ! OVERWRITE is t
            call bookmark_load(resource_file("test.bmk"), st, overwrite=.true., no_msg=.true.)
            call check(error, table_is(one), "overwritten")
            if (allocated(error)) exit steps
            ! Append
            ! OVERWRITE is nil
            call bookmark_load(resource_file("test.bmk"), st, no_msg=.true.)
            two(1) = tests_bookmark()
            two(2) = tests_bookmark()
            two(2)%name = "name<2>"
            call check(error, table_is(two), "appended")
        end block steps

        call done(old)
    end subroutine test_load

    ! ------------------------------------------------------------------
    ! The bookmark list
    ! ------------------------------------------------------------------

    !> (with-bookmark-bmenu-test): the bookmark list of the cached
    !> bookmark (of the list, with LIST), in "*Bookmark List - Testing*".
    subroutine with_bmenu_test(old, config, list)
        type(handle_type), intent(out) :: old
        type(window_configuration_type), intent(out) :: config
        logical, intent(in), optional :: list
        type(status_type) :: st
        logical :: l

        l = .false.
        if (present(list)) l = list

        if (l) then
            call with_bookmark_test_list(old)
        else
            call with_bookmark_test(old)
        end if

        bookmark_bmenu_buffer = "*Bookmark List - Testing*"
        call current_window_configuration(config)
        call bookmark_bmenu_list(st)
    end subroutine with_bmenu_test

    subroutine bmenu_done(old, config)
        type(handle_type), intent(in) :: old
        type(window_configuration_type), intent(in) :: config
        type(status_type) :: st

        call set_window_configuration(config)
        call kill_buffer(get_buffer(bookmark_bmenu_buffer), st)
        bookmark_bmenu_buffer = "*Bookmark List*"
        call set_buffer(old, st)
        call done(old)
    end subroutine bmenu_done

    !> Whether the text at point matches REGEXP.
    logical function looking(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st

        looking = looking_at(regexp, st)
    end function looking

    !> (re-search-forward REGEXP nil t) from point.
    logical function search_regexp(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st
        integer(int64) :: found

        call re_search_forward(regexp, found, st, noerror=1)
        search_regexp = found /= 0
    end function search_regexp

    subroutine test_bmenu_edit_annotation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)
        call bookmark_set_annotation("name", "foo", st)
        call bookmark_bmenu_edit_annotation(st)
        call check(error, string_match("foo", buffer_string(), st) > 0, buffer_string())
        call kill_buffer(current_buffer(), st)
        call bmenu_done(old, config)
    end subroutine test_bmenu_edit_annotation

    subroutine test_bmenu_send_edited_annotation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)
        call bookmark_bmenu_edit_annotation(st)
        call insert("foo", st)
        call bookmark_edit_annotation_confirm(st)
        call check(error, bookmark_get_annotation("name"), "foo")
        call bmenu_done(old, config)
    end subroutine test_bmenu_send_edited_annotation

    !> Test for https://debbugs.gnu.org/20150 .
    subroutine test_bmenu_restore_focus(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)

        steps: block
            call bookmark_bmenu_edit_annotation(st)
            call insert("foo", st)
            call bookmark_edit_annotation_confirm(st)
            call check(error, buffers(cur)%name, bookmark_bmenu_buffer)
            if (allocated(error)) exit steps
            call beginning_of_line()
            call forward_char(st, 4_int64)
            call check(error, looking("name"), "at the name")
        end block steps

        call bmenu_done(old, config)
    end subroutine test_bmenu_restore_focus

    subroutine test_bmenu_toggle_filenames(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)

        steps: block
            call check(error, search_regexp("/some/file"), "the file")
            if (allocated(error)) exit steps
            call bookmark_bmenu_toggle_filenames_command(st)
            call goto_char(point_min())
            call check(error,.not. search_regexp("/some/file"), "no file")
        end block steps

        call bmenu_done(old, config)
    end subroutine test_bmenu_toggle_filenames

    subroutine test_bmenu_toggle_filenames_show(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)
        call bookmark_bmenu_toggle_filenames_command(st, .true.)
        call check(error, search_regexp("/some/file"), "the file")
        call bmenu_done(old, config)
    end subroutine test_bmenu_toggle_filenames_show

    subroutine test_bmenu_show_filenames(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config

        call with_bmenu_test(old, config)
        call bookmark_bmenu_show_filenames()
        call check(error, search_regexp("/some/file"), "the file")
        call bmenu_done(old, config)
    end subroutine test_bmenu_show_filenames

    subroutine test_bmenu_hide_filenames(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config

        call with_bmenu_test(old, config)
        call bookmark_bmenu_hide_filenames()
        call goto_char(point_min())
        call check(error,.not. search_regexp("/some/file"), "no file")
        bookmark_bmenu_toggle_filenames = .true.
        call bmenu_done(old, config)
    end subroutine test_bmenu_hide_filenames

    subroutine test_bmenu_bookmark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config

        call with_bmenu_test(old, config)
        call check(error, bookmark_bmenu_bookmark(), "name")
        call bmenu_done(old, config)
    end subroutine test_bmenu_bookmark

    subroutine test_bmenu_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: shortage

        call with_bmenu_test(old, config)
        call bookmark_bmenu_mark(st)
        shortage = forward_line(-1_int64)
        call beginning_of_line()
        call check(error, looking("^>"), "marked")
        call bmenu_done(old, config)
    end subroutine test_bmenu_mark

    subroutine test_bmenu_any_marks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)
        call bookmark_bmenu_mark(st)
        call beginning_of_line()
        call check(error, bookmark_bmenu_any_marks(), "a mark")
        call bmenu_done(old, config)
    end subroutine test_bmenu_any_marks

    !> Whether the three lines from point are tagged TAG and are the
    !> bookmarks of the list.
    subroutine check_list_lines(error, tags)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: tags(3)
        type(status_type) :: st
        integer(int64) :: shortage
        integer :: k

        call goto_char(point_min())
        call bookmark_bmenu_ensure_position()

        do k = 1, 3
            call check(error, looking("^"//tags(k)), "tag "//tags(k))
            if (allocated(error)) return
            call check(error, same_bookmark(bookmark_table(bookmark_get_bookmark(bookmark_bmenu_bookmark(), st)), &
                                            list_bookmark(k)), "bookmark")
            if (allocated(error)) return
            shortage = forward_line(1_int64)
        end do
    end subroutine check_list_lines

    function list_bookmark(k) result(b)
        integer, intent(in) :: k
        type(bookmark_type) :: b

        select case (k)
        case (1)
            b = cached("name-0", "/some/file-0", "ghi", "jkl", 4_int64)
        case (2)
            b = cached("name-1", "/some/file-1", "mno", "pqr", 5_int64)
        case default
            b = cached("name-2", "/some/file-2", "stu", "vwx", 6_int64)
        end select
    end function list_bookmark

    subroutine test_bmenu_mark_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: here

        call with_bmenu_test(old, config, .true.)

        steps: block
            here = point_max()
            ! Expect to not move the point
            call goto_char(here)
            call bookmark_bmenu_mark_all(st)
            call check(error, point() == here, "point stays")
            if (allocated(error)) exit steps
            ! Verify that all bookmarks are marked
            call check_list_lines(error, [character(len=2) :: "> ", "> ", "> "])
        end block steps

        call bmenu_done(old, config)
    end subroutine test_bmenu_mark_all

    subroutine test_bmenu_any_marks_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: shortage

        call with_bmenu_test(old, config, .true.)

        steps: block
            ! Mark just the second item
            call goto_char(point_min())
            call bookmark_bmenu_ensure_position()
            shortage = forward_line(1_int64)
            call bookmark_bmenu_mark(st)
            ! Verify that only the second item is marked
            call check_list_lines(error, [character(len=2) :: "  ", "> ", "  "])
            if (allocated(error)) exit steps
            ! There should be at least one mark
            call check(error, bookmark_bmenu_any_marks(), "a mark")
        end block steps

        call bmenu_done(old, config)
    end subroutine test_bmenu_any_marks_list

    subroutine test_bmenu_unmark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: shortage

        call with_bmenu_test(old, config)
        call bookmark_bmenu_mark(st)
        call goto_char(point_min())
        call bookmark_bmenu_unmark(st)
        shortage = forward_line(-1_int64)
        call beginning_of_line()
        call check(error, looking("^  "), "unmarked")
        call bmenu_done(old, config)
    end subroutine test_bmenu_unmark

    subroutine test_bmenu_unmark_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: here

        call with_bmenu_test(old, config, .true.)

        steps: block
            call bookmark_bmenu_mark_all(st)
            here = point_max()
            ! Expect to not move the point
            call goto_char(here)
            call bookmark_bmenu_unmark_all(st)
            call check(error, point() == here, "point stays")
            if (allocated(error)) exit steps
            ! Verify that all bookmarks are unmarked
            call check_list_lines(error, [character(len=2) :: "  ", "  ", "  "])
        end block steps

        call bmenu_done(old, config)
    end subroutine test_bmenu_unmark_all

    subroutine test_bmenu_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)
        call bookmark_bmenu_delete(st)
        call bookmark_bmenu_execute_deletions(st)
        call check(error, size(bookmark_table) == 0, "no bookmarks")
        call bmenu_done(old, config)
    end subroutine test_bmenu_delete

    subroutine test_bmenu_delete_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        integer(int64) :: here

        call with_bmenu_test(old, config, .true.)

        steps: block
            ! Verify that unmarked bookmarks aren't deleted
            call bookmark_bmenu_execute_deletions(st)
            call check(error, size(bookmark_table) > 0, "bookmarks left")
            if (allocated(error)) exit steps
            here = point_max()
            ! Expect to not move the point
            call goto_char(here)
            call bookmark_bmenu_delete_all(st)
            call check(error, point() == here, "point stays")
            if (allocated(error)) exit steps
            ! Verify that all bookmarks are marked for deletion
            call check_list_lines(error, [character(len=2) :: "D ", "D ", "D "])
            if (allocated(error)) exit steps
            ! Verify that all bookmarks are deleted
            call bookmark_bmenu_execute_deletions(st)
            call check(error, size(bookmark_table) == 0, "no bookmarks")
        end block steps

        call bmenu_done(old, config)
    end subroutine test_bmenu_delete_all

    subroutine test_bmenu_locate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old, messages
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=:), allocatable :: text

        call with_bmenu_test(old, config)
        call bookmark_bmenu_locate(st)
        messages = get_buffer("*Messages*")
        call set_buffer(messages, st)
        text = buffer_string()
        call set_buffer(get_buffer(bookmark_bmenu_buffer), st)
        call check(error, len(text) >= 11, text)
        if (.not. allocated(error)) call check(error, text(len(text) - 10:), "/some/file"//achar(10))
        call bmenu_done(old, config)
    end subroutine test_bmenu_locate

    subroutine test_bmenu_filter_table_by_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st

        call with_bmenu_test(old, config)
        call bookmark_bmenu_filter_table_by_regexp(regexp_unmatchable, st)
        call goto_char(point_min())
        call check(error, looking("^$"), "empty")
        call bmenu_done(old, config)
    end subroutine test_bmenu_filter_table_by_regexp
end module test_bookmark_fm
