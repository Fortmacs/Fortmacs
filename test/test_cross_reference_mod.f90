! test_cross_reference_mod.f90 -- tests for cross_reference_mod.
! Ports GNU Emacs 31.1 test/lisp/progmodes/xref-tests.el in part; the
! xref--buf-pairs-iterator tests need xref-query-replace-in-results, which
! is not ported.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> xref-tests.el's cases (the resource files are in test/xref-resources),
!> and own tests: the *xref* buffer, its motion and jumps, the history
!> (M-, and C-M-,), `xref_regular_expression_to_extended' and C-h v of
!> `xref_prompt_for_identifier'.
module test_cross_reference_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, USER_ERROR_CODE
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, buffers, current
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point, goto_character, point_minimum
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, initialize_fileio, file_name_as_directory, &
                                     directory_file_name, file_name_directory
    use bindings_mod, only: initialize_bindings
    use cross_reference_mod, only: xref_item_type, xref_file_location_type, xref_items_fetcher_type, make_xref, &
                                   make_xref_buffer_location, append_xref_item, xref_matches_in_directory, xref_matches_in_files, &
                                   xref_group_name_for_display, xref_file_name_display, show_xrefs, xref_items_at, &
                                   next_xref_line_no_show, goto_xref, go_back_in_xref_history, go_forward_in_xref_history, &
                                   clear_xref_marker_stack, is_xref_marker_stack_empty, xref_regular_expression_to_extended, &
                                   XREF_FILE_NAME_DISPLAY_ABSOLUTE, XREF_FILE_NAME_DISPLAY_NONDIRECTORY, &
                                   XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE, XREF_DISPLAY_ACTION_NONE, xref_prompt_for_identifier, &
                                   XREF_PROMPT_FOR_IDENTIFIER_LISTED, XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED
    use help_functions_mod, only: describe_variable_text
    implicit none(type, external)
    private

    public :: collect_cross_reference_mod

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_cross_reference_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("xref-matches-in-directory-finds-none-for-some-regexp", test_finds_none), &
                    new_unittest("xref-matches-in-directory-finds-some-for-bar", test_finds_bar), &
                    new_unittest("xref-matches-in-directory-filters-with-ignores", test_filters_ignores), &
                    new_unittest("xref-matches-in-directory-finds-two-matches-on-the-same-line", test_same_line), &
                    new_unittest("xref-matches-in-directory-finds-an-empty-line-regexp-match", test_empty_line), &
                    new_unittest("xref-matches-in-files-includes-matches-from-all-the-files", test_files_all), &
                    new_unittest("xref-matches-in-files-trims-summary-for-matches-on-same-line", test_files_trims), &
                    new_unittest("xref--xref-file-name-display-is-abs", test_display_absolute), &
                    new_unittest("xref--xref-file-name-display-is-nondirectory", test_display_nondirectory), &
                    new_unittest("xref--xref-file-name-display-is-relative-to-project-root", test_display_relative), &
                    new_unittest("xref-buffer", test_xref_buffer), &
                    new_unittest("xref-history", test_history), &
                    new_unittest("xref-regexp-to-extended", test_regular_expression_to_extended), &
                    new_unittest("xref-prompt-for-identifier-help", test_prompt_for_identifier_help) &
                    ]
    end subroutine collect_cross_reference_mod

    !> Port of xref-tests.el xref-tests--data-dir: the resource directory.
    function data_directory() result(directory)
        character(len=:), allocatable :: directory
        type(status_type) :: st
        type(handle_type) :: h

        call initialize_fileio()
        call initialize_bindings()
        call expand_file_name("test/xref-resources/", directory, st, fortmacs_wd)
        directory = file_name_as_directory(directory)

        if (current == 0) then
            call get_or_make_buffer("xref-tests", h, st)
            call set_buffer(h, st)
            buffers(current)%directory = file_name_as_directory(fortmacs_wd)
        end if
    end function data_directory

    !> Port of xref-tests.el xref-tests--matches-in-data-dir.
    subroutine matches_in_data_directory(regular_expression, items, status)
        character(len=*), intent(in) :: regular_expression
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        character(len=1) :: none(0)

        call xref_matches_in_directory(regular_expression, "*", data_directory(), none, items, status)
    end subroutine matches_in_data_directory

    !> Port of xref-tests.el xref-tests--locations-in-data-dir: the items
    !> sorted by their location's group (a stable sort).
    subroutine locations_in_data_directory(regular_expression, items, status)
        character(len=*), intent(in) :: regular_expression
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        type(xref_item_type) :: item
        integer :: i, j

        call matches_in_data_directory(regular_expression, items, status)

        do i = 2, size(items)
            item = items(i)
            j = i - 1

            do while (j >= 1)
                if (.not. llt(item%location%group(), items(j)%location%group())) exit
                items(j + 1) = items(j)
                j = j - 1
            end do

            items(j + 1) = item
        end do
    end subroutine locations_in_data_directory

    ! Whether STRING ends with SUFFIX.  Not a port.
    logical function ends_with(string, suffix)
        character(len=*), intent(in) :: string, suffix

        ends_with = .false.
        if (len(string) >= len(suffix)) ends_with = string(len(string) - len(suffix) + 1:) == suffix
    end function ends_with

    !> Port of xref-tests.el
    !> xref-matches-in-directory-finds-none-for-some-regexp.
    subroutine test_finds_none(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st

        call matches_in_data_directory("zzz", items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 0)
    end subroutine test_finds_none

    !> Port of xref-tests.el xref-matches-in-directory-finds-some-for-bar.
    subroutine test_finds_bar(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st

        call locations_in_data_directory("bar", items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 2)
        if (allocated(error)) return
        call check(error, ends_with(items(1)%location%group(), "file1.txt"), items(1)%location%group())
        if (allocated(error)) return
        call check(error, ends_with(items(2)%location%group(), "file2.txt"), items(2)%location%group())
    end subroutine test_finds_bar

    !> Port of xref-tests.el xref-matches-in-directory-filters-with-ignores.
    subroutine test_filters_ignores(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st

        call xref_matches_in_directory("bar", "*", data_directory(), [character(len=9) :: "./file1.*"], items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 1)
        if (allocated(error)) return
        call check(error, ends_with(items(1)%location%group(), "file2.txt"), items(1)%location%group())
    end subroutine test_filters_ignores

    !> Port of xref-tests.el
    !> xref-matches-in-directory-finds-two-matches-on-the-same-line.
    subroutine test_same_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st
        integer :: i
        integer(int64) :: columns(2)

        call locations_in_data_directory("foo", items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 2)
        if (allocated(error)) return

        do i = 1, 2
            call check(error, ends_with(items(i)%location%group(), "file1.txt"), items(i)%location%group())
            if (allocated(error)) return
            call check(error, items(i)%location%line_number() == 1_int64, "line 1")
            if (allocated(error)) return

            select type (l => items(i)%location)
            type is (xref_file_location_type)
                columns(i) = l%column
            class default
                columns(i) = -1
            end select
        end do

        call check(error, columns(1) == 1_int64 .and. columns(2) == 5_int64, "columns 1 and 5")
    end subroutine test_same_line

    !> Port of xref-tests.el
    !> xref-matches-in-directory-finds-an-empty-line-regexp-match.
    subroutine test_empty_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st
        integer(int64) :: column

        call locations_in_data_directory("^$", items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 1)
        if (allocated(error)) return
        call check(error, ends_with(items(1)%location%group(), "file2.txt"), items(1)%location%group())
        if (allocated(error)) return
        call check(error, items(1)%location%line_number() == 1_int64, "line 1")
        if (allocated(error)) return
        column = -1

        select type (l => items(1)%location)
        type is (xref_file_location_type)
            column = l%column
        class default
        end select

        call check(error, column == 0_int64, "column 0")
    end subroutine test_empty_line

    ! The files of the data directory, absolute (directory-files with
    ! "\\`[^.]").  Not a port.
    function data_files() result(files)
        character(len=:), allocatable :: files(:)
        character(len=:), allocatable :: directory

        directory = data_directory()
        allocate (character(len=len(directory) + 9) :: files(3))
        files(1) = directory//"file1.txt"
        files(2) = directory//"file2.txt"
        files(3) = directory//"file3.txt"
    end function data_files

    !> Port of xref-tests.el
    !> xref-matches-in-files-includes-matches-from-all-the-files.
    subroutine test_files_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st
        integer :: i

        call xref_matches_in_files("bar", data_files(), items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 2)
        if (allocated(error)) return

        do i = 1, size(items)
            call check(error, items(i)%summary, "bar")
            if (allocated(error)) return
        end do
    end subroutine test_files_all

    !> Port of xref-tests.el
    !> xref-matches-in-files-trims-summary-for-matches-on-same-line.
    subroutine test_files_trims(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st

        call xref_matches_in_files("match", data_files(), items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(items), 3)
        if (allocated(error)) return
        call check(error, items(1)%summary, " match some words ")
        if (allocated(error)) return
        call check(error, items(2)%summary, "match more ")
        if (allocated(error)) return
        call check(error, items(3)%summary, "match ends here")
    end subroutine test_files_trims

    ! The distinct display names of the groups of the locations of
    ! "\(bar\|foo\)", given the PROJECT_ROOT.  Not a port.
    subroutine display_names(project_root, names, error)
        character(len=*), intent(in) :: project_root
        character(len=:), allocatable, intent(out) :: names
        type(error_type), allocatable, intent(out) :: error
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st
        character(len=:), allocatable :: name, last
        integer :: i

        names = ""
        last = ""
        call locations_in_data_directory("\(bar\|foo\)", items, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return

        do i = 1, size(items)
            name = xref_group_name_for_display(items(i)%location%group(), project_root)

            if (name /= last) then
                names = names//name//LF
                last = name
            end if
        end do
    end subroutine display_names

    !> Port of xref-tests.el xref--xref-file-name-display-is-abs.
    subroutine test_display_absolute(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: names, directory

        directory = data_directory()
        xref_file_name_display = XREF_FILE_NAME_DISPLAY_ABSOLUTE
        call display_names("", names, error)
        xref_file_name_display = XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE
        if (allocated(error)) return
        call check(error, names, directory//"file1.txt"//LF//directory//"file2.txt"//LF)
    end subroutine test_display_absolute

    !> Port of xref-tests.el xref--xref-file-name-display-is-nondirectory.
    subroutine test_display_nondirectory(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: names

        xref_file_name_display = XREF_FILE_NAME_DISPLAY_NONDIRECTORY
        call display_names("", names, error)
        xref_file_name_display = XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE
        if (allocated(error)) return
        call check(error, names, "file1.txt"//LF//"file2.txt"//LF)
    end subroutine test_display_nondirectory

    !> Port of xref-tests.el
    !> xref--xref-file-name-display-is-relative-to-project-root.
    subroutine test_display_relative(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: names, parent

        parent = file_name_directory(directory_file_name(data_directory()))
        xref_file_name_display = XREF_FILE_NAME_DISPLAY_PROJECT_RELATIVE
        call display_names(parent, names, error)
        if (allocated(error)) return
        call check(error, names, "xref-resources/file1.txt"//LF//"xref-resources/file2.txt"//LF)
    end subroutine test_display_relative

    ! A buffer "xref-test" with two lines, and an items fetcher of two
    ! items at their beginnings.  Not a port.
    subroutine make_source(source, fetcher)
        type(handle_type), intent(out) :: source
        type(xref_items_fetcher_type), intent(out) :: fetcher
        type(xref_item_type) :: item
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer("xref-test", source, st)
        call set_buffer(source, st)
        call erase_buffer(st)
        call insert("first line"//LF//"second line"//LF, st)
        call goto_character(point_minimum())
        call show_alone()
        item = make_xref("first line", make_xref_buffer_location(source, 1_int64))
        call append_xref_item(fetcher%items, item)
        item = make_xref("second line", make_xref_buffer_location(source, 12_int64))
        call append_xref_item(fetcher%items, item)
    end subroutine make_source

    !> The *xref* buffer: the group heading and the items' lines, motion
    !> to the first item and the jump to its location.  Not a port.
    subroutine test_xref_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_items_fetcher_type) :: fetcher
        type(xref_item_type), allocatable :: shown(:)
        type(handle_type) :: source, xref_buffer, b
        type(status_type) :: st

        call make_source(source, fetcher)
        call show_xrefs(fetcher, XREF_DISPLAY_ACTION_NONE, xref_buffer, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(xref_buffer, st)
        call check(error, buffer_string(), "(buffer xref-test)"//LF//"  first line"//LF//"  second line"//LF)
        if (allocated(error)) return
        call xref_items_at(xref_buffer, shown)
        call check(error, size(shown), 2)
        if (allocated(error)) return
        call goto_character(point_minimum())
        call next_xref_line_no_show(st)
        call next_xref_line_no_show(st)
        call goto_xref(.false., st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        b = current_buffer()
        call check(error, b%index == source%index, "in xref-test")
        if (allocated(error)) return
        call check(error, point() == 12_int64, "at the second line")
        call close_buffer(xref_buffer, st)
        call close_buffer(source, st)
    end subroutine test_xref_buffer

    !> The history: `go_back_in_xref_history' returns to where `show_xrefs' was
    !> called, `go_forward_in_xref_history' goes again where it came back from, and
    !> the ends of the history are user errors.  Not a port.
    subroutine test_history(error)
        type(error_type), allocatable, intent(out) :: error
        type(xref_items_fetcher_type) :: fetcher
        type(handle_type) :: source, xref_buffer
        type(status_type) :: st

        call make_source(source, fetcher)
        call clear_xref_marker_stack()
        call goto_character(5_int64)
        call show_xrefs(fetcher, XREF_DISPLAY_ACTION_NONE, xref_buffer, st)
        call check(error,.not. is_xref_marker_stack_empty(), "a marker pushed")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call next_xref_line_no_show(st)
        call next_xref_line_no_show(st)
        call goto_xref(.false., st)
        call check(error, point() == 12_int64, "at the second line")
        if (allocated(error)) return
        call go_back_in_xref_history(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, point() == 5_int64, "back where it started")
        if (allocated(error)) return
        call go_back_in_xref_history(st)
        call check(error, st%code == USER_ERROR_CODE .and. format_error(st) == "At start of xref history", format_error(st))
        if (allocated(error)) return
        call go_forward_in_xref_history(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, point() == 12_int64, "forward to the second line")
        if (allocated(error)) return
        call go_forward_in_xref_history(st)
        call check(error, format_error(st), "At end of xref history")
        call clear_xref_marker_stack()
        call close_buffer(xref_buffer, st)
        call close_buffer(source, st)
    end subroutine test_history

    !> `xref_regular_expression_to_extended': the operators lose their backslash, the
    !> literal characters gain one, bracket expressions stay.  Not a port.
    subroutine test_regular_expression_to_extended(error)
        type(error_type), allocatable, intent(out) :: error

        call initialize_bindings()
        call check(error, xref_regular_expression_to_extended("\(bar\|foo\)"), "(bar|foo)")
        if (allocated(error)) return
        call check(error, xref_regular_expression_to_extended("a(b)"), "a\(b\)")
        if (allocated(error)) return
        call check(error, xref_regular_expression_to_extended("x\{2\}"), "x{2}")
        if (allocated(error)) return
        call check(error, xref_regular_expression_to_extended("[(]a"), "[(]a")
        if (allocated(error)) return
        call check(error, xref_regular_expression_to_extended("plain"), "plain")
    end subroutine test_regular_expression_to_extended

    !> C-h v xref_prompt_for_identifier shows its commands with the state
    !> that uses them, as upstream shows the whole list
    !> "(not xref-find-definitions ...)"; in the states .true. and .false.
    !> the commands are not shown and do not make the setting count as on.
    subroutine test_prompt_for_identifier_help(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: COMMANDS_TEXT = "%commands = [""find_xref_definitions"", "// &
                                       """find_xref_definitions_other_window"", ""find_xref_definitions_other_frame""]"
        character(len=:), allocatable :: text
        logical :: found

        call initialize_bindings()
        call describe_variable_text("xref_prompt_for_identifier", text, found)
        call check(error, found .and. index(text, "Its value is"//LF//"XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED, "// &
                                            COMMANDS_TEXT//LF) > 0 .and. index(text, "Original value") == 0, text)
        if (allocated(error)) return
        xref_prompt_for_identifier = XREF_PROMPT_FOR_IDENTIFIER_LISTED
        call describe_variable_text("xref_prompt_for_identifier", text, found)
        call check(error, index(text, "XREF_PROMPT_FOR_IDENTIFIER_LISTED, "//COMMANDS_TEXT//LF) > 0 .and. &
                   index(text, "Original value was "//LF//"XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED, ") > 0, text)
        if (allocated(error)) return
        xref_prompt_for_identifier = .false.
        call describe_variable_text("xref_prompt_for_identifier", text, found)
        call check(error, index(text, "Its value is .false."//LF) > 0 .and. xref_prompt_for_identifier%is_off(), text)
        if (allocated(error)) return
        xref_prompt_for_identifier = .true.
        call describe_variable_text("xref_prompt_for_identifier", text, found)
        call check(error, index(text, "Its value is .true."//LF) > 0 .and. .not. xref_prompt_for_identifier%is_off(), text)
        xref_prompt_for_identifier = XREF_PROMPT_FOR_IDENTIFIER_EXCEPT_LISTED
    end subroutine test_prompt_for_identifier_help
end module test_cross_reference_mod
