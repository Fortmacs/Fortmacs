! test_abbreviation_mod.f90 -- tests for abbreviation_mod.
! Ports GNU Emacs 31.1 test/lisp/abbrev-tests.el in part;
! abbrev-table-p, abbrev-table-get/put and reading abbrev files do not
! apply to typed tables, and edit-abbrevs-redefine and define-abbrevs
! are not ported.
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

!> The abbrev-tests.el cases that need no Lisp (abbrev-table-p,
!> abbrev-table-get/put and reading abbrev files are not applicable to
!> typed tables compiled into the init file; edit-abbrevs-redefine and
!> define-abbrevs are not ported), and own tests through the command
!> loop: expansion on self-insertion, case handling, the prefix mark,
!> the abbrev commands, table properties, mode tables, saving.
module test_abbreviation_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: prefix_argument_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, buffers, current, buffer_name, &
                          current_buffer, buffer_list, resolve_buffer, restore_buffer_modified
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_character, buffer_string, point, point_minimum, point_maximum
    use search_mod, only: search_forward, SEARCH_NOERROR
    use keymap_mod, only: key_parse
    use keyboard_mod, only: unread_command_events, discard_input, command_loop, last_message
    use execute_interactively_mod, only: prefix_argument, current_prefix_argument
    use bindings_mod, only: initialize_bindings
    use simple_mod, only: fundamental_mode, push_mark
    use indent_mod, only: indent_tabs_mode
    use text_mode_mod, only: text_mode, text_mode_abbreviation_table
    use help_mode_mod, only: help_mode, help_mode_abbreviation_table
    use simple_mod, only: special_mode_abbreviation_table
    use display_engine_mod, only: mode_line_modes
    use names_mod, only: user_initialization_file
    use file_input_output_mod, only: expand_file_name, create_directory_internal, delete_file_internal, fortmacs_wd, &
                                     initialize_fileio, file_exists
    use files_mod, only: save_some_buffers
    use abbreviation_mod, only: abbreviation_table_type, abbreviation_reference_type, global_abbreviation_table, &
                                fundamental_mode_abbreviation_table, abbreviation_tables, abbreviation_mode, &
                                abbreviation_all_caps, only_global_abbreviations, abbreviations_changed, abbreviation_file_name, &
                                save_abbreviations, last_abbreviation_text, clear_all_abbreviations, copy_abbreviation_table, &
                                insert_abbreviations, abbreviation_table_name, prepare_abbreviation_list_buffer, &
                                write_abbreviation_file, inverse_add_abbreviation, abbreviation_expansion, &
                                abbreviation_table_names, expand_abbreviation, unexpand_abbreviation, expand_region_abbreviations, &
                                define_global_abbreviation, abbreviation_possibly_save, local_abbreviation_table, &
                                abbreviation_name
    implicit none(type, external)
    private

    public :: collect_abbreviation_mod

    character(len=*), parameter :: LF = achar(10)

    !> abbrev-tests.el's ert-test-abbrevs and ert-save-test-table.
    type(abbreviation_table_type), target :: ert_test_abbreviations

    !> A table that is not listed (make-abbrev-table).
    type(abbreviation_table_type), target :: unlisted_table

    !> Calls of the hooks.
    integer :: hook_calls = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_abbreviation_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("abbrev--table-symbols-test", test_table_symbols), &
                    new_unittest("copy-abbrev-table-test", test_copy_abbreviation_table), &
                    new_unittest("abbrev-table-empty-p-test", test_empty), &
                    new_unittest("kill-all-abbrevs-test", test_clear_all_abbreviations), &
                    new_unittest("abbrev-table-name-test", test_table_name), &
                    new_unittest("clear-abbrev-table-test", test_clear), &
                    new_unittest("list-abbrevs-test", test_list_abbreviations), &
                    new_unittest("prepare-abbrev-list-buffer-test", test_prepare_list_buffer), &
                    new_unittest("insert-abbrevs-test", test_insert_abbreviations), &
                    new_unittest("read-write-abbrev-file-test", test_write_abbreviation_file), &
                    new_unittest("read-write-abbrev-file-test-with-props", test_write_abbreviation_file_properties), &
                    new_unittest("inverse-add-abbrev-skips-trailing-nonword", test_inverse_add_1), &
                    new_unittest("inverse-add-abbrev-skips-trailing-nonword/positive-arg", test_inverse_add_2), &
                    new_unittest("inverse-add-abbrev-skips-trailing-nonword/negative-arg", test_inverse_add_3), &
                    new_unittest("abbrev--possibly-save-test", test_possibly_save), &
                    new_unittest("self-insert-expands", test_self_insert_expands), &
                    new_unittest("case-handling", test_case_handling), &
                    new_unittest("abbrev-all-caps", test_all_caps), &
                    new_unittest("expand-abbrev-keys", test_expand_keys), &
                    new_unittest("unexpand-abbrev", test_unexpand), &
                    new_unittest("abbrev-prefix-mark", test_prefix_mark), &
                    new_unittest("add-global-abbrev", test_add_global_abbreviation), &
                    new_unittest("add-mode-abbrev", test_add_mode_abbreviation), &
                    new_unittest("inverse-add-global-abbrev-keys", test_inverse_add_keys), &
                    new_unittest("expand-region-abbrevs", test_expand_region), &
                    new_unittest("define-global-abbrev-check-chars", test_check_characters), &
                    new_unittest("table-properties", test_table_properties), &
                    new_unittest("abbrev-hooks", test_hooks), &
                    new_unittest("mode-tables", test_mode_tables), &
                    new_unittest("abbrevs-changed", test_abbreviations_changed), &
                    new_unittest("abbrev-mode-lighter", test_lighter), &
                    new_unittest("open-line-no-expansion", test_open_line), &
                    new_unittest("save-some-buffers-saves-abbrevs", test_save_some_buffers) &
                    ]
    end subroutine collect_abbreviation_mod

    ! ------------------------------------------------------------------
    ! Fixtures
    ! ------------------------------------------------------------------

    !> A fresh Fundamental buffer shown alone, no abbrevs, the default
    !> settings.
    subroutine fresh(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call discard_input()
        prefix_argument = prefix_argument_type()
        current_prefix_argument = prefix_argument_type()
        call get_or_make_buffer("*abbrev-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call fundamental_mode(st)
        call show_alone()
        call clear_all_abbreviations()
        abbreviation_mode = .false.
        indent_tabs_mode = .true.
        abbreviation_all_caps = .false.
        only_global_abbreviations = .false.
        abbreviations_changed = .false.
        last_message = ""
    end subroutine fresh

    !> setup-test-abbrev-table: ert_test_abbreviations with "a-e-t".
    subroutine setup_test_abbreviation_table()
        call ert_test_abbreviations%define_table("ert_test_abbreviations")
        call ert_test_abbreviations%define("a-e-t", "abbrev-ert-test")
    end subroutine setup_test_abbreviation_table

    !> Type KEYS (kbd syntax) to the command loop.
    subroutine execute_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
    end subroutine execute_keys

    !> Queue TEXT as typed characters (for the minibuffer).
    subroutine type_text(text)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(text)))

        do i = 1, len(text)
            evs(i) = value_type(int(iachar(text(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_text

    !> The expansion of ABBREVIATION in TABLE, "" for none.
    function expansion_of(abbreviation, table) result(s)
        character(len=*), intent(in) :: abbreviation
        type(abbreviation_table_type), intent(in), target, optional :: table
        character(len=:), allocatable :: s, e

        call abbreviation_expansion(abbreviation, e, table)
        s = "<none>"
        if (allocated(e)) s = e
    end function expansion_of

    !> Whether the current buffer contains TEXT.
    logical function buffer_has(text)
        character(len=*), intent(in) :: text

        buffer_has = index(buffer_string(), text) > 0
    end function buffer_has

    subroutine write_text(file, text)
        character(len=*), intent(in) :: file, text
        integer :: u

        open (newunit=u, file=file, access="stream", form="unformatted", status="replace", action="write")
        write (u) text
        close (u)
    end subroutine write_text

    function read_text(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text
        integer :: u, n

        open (newunit=u, file=file, access="stream", form="unformatted", status="old", action="read")
        inquire (unit=u, size=n)
        allocate (character(len=n) :: text)
        read (u) text
        close (u)
    end function read_text

    !> A test init file under build/, removed first.
    function test_file(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        type(status_type) :: st

        call initialize_fileio()
        call expand_file_name("build/test-abbrev", path, st, fortmacs_wd)
        call create_directory_internal(path, st)
        path = path//"/"//name
        call delete_file_internal(path, st)
    end function test_file

    ! ------------------------------------------------------------------
    ! Ported from abbrev-tests.el
    ! ------------------------------------------------------------------

    subroutine test_table_symbols(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: names(:)

        call fresh(h)
        call setup_test_abbreviation_table()
        call ert_test_abbreviations%define("sys", "system abbrev", system=.true.)
        call abbreviation_table_names(ert_test_abbreviations, names)
        call check(error, size(names), 1)
        if (allocated(error)) return
        call check(error, trim(names(1)), "a-e-t")
        if (allocated(error)) return
        call abbreviation_table_names(ert_test_abbreviations, names, system=.true.)
        call check(error, size(names), 2)
        if (allocated(error)) return
        call check(error, trim(names(1)) == "a-e-t" .and. trim(names(2)) == "sys", "with system abbrevs")
        call close_buffer(h, st)
    end subroutine test_table_symbols

    subroutine test_copy_abbreviation_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_table_type), target :: new_table

        call fresh(h)
        call setup_test_abbreviation_table()
        ! Bug 21828
        call copy_abbreviation_table(ert_test_abbreviations, new_table)
        call check(error, expansion_of("a-e-t", new_table), "abbrev-ert-test")
        if (allocated(error)) return
        call check(error, len_trim(new_table%name), 0)
        call close_buffer(h, st)
    end subroutine test_copy_abbreviation_table

    subroutine test_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_table_type) :: table

        call fresh(h)
        call check(error, table%empty())
        if (allocated(error)) return
        call table%define("sys", "system abbrev", system=.true.)
        call check(error,.not. table%empty())
        if (allocated(error)) return
        call check(error, table%empty(ignore_system=.true.))
        call close_buffer(h, st)
    end subroutine test_empty

    !> Test undefining all defined abbrevs.
    subroutine test_clear_all_abbreviations(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer :: number_tables, k, empty

        call fresh(h)
        ! ensure at least one abbrev exists
        call setup_test_abbreviation_table()
        number_tables = size(abbreviation_tables)
        call clear_all_abbreviations()
        ! no tables should have been removed/added
        call check(error, size(abbreviation_tables), number_tables)
        if (allocated(error)) return
        ! number of empty tables should be the same as number of tables
        empty = 0

        do k = 1, size(abbreviation_tables)
            if (abbreviation_tables(k)%p%empty()) empty = empty + 1
        end do

        call check(error, empty, number_tables)
        call close_buffer(h, st)
    end subroutine test_clear_all_abbreviations

    !> Test returning name of abbrev-table.
    subroutine test_table_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call setup_test_abbreviation_table()
        call check(error, abbreviation_table_name(ert_test_abbreviations), "ert_test_abbreviations")
        if (allocated(error)) return
        call check(error, abbreviation_table_name(unlisted_table), "")
        call close_buffer(h, st)
    end subroutine test_table_name

    !> Test clearing single abbrev table.
    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call setup_test_abbreviation_table()
        call check(error, expansion_of("a-e-t", ert_test_abbreviations), "abbrev-ert-test")
        if (allocated(error)) return
        call ert_test_abbreviations%clear()
        call check(error, expansion_of("a-e-t", ert_test_abbreviations), "<none>")
        if (allocated(error)) return
        call check(error, ert_test_abbreviations%empty())
        call close_buffer(h, st)
    end subroutine test_clear

    !> Test generation of abbrev list buffer.
    subroutine test_list_abbreviations(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, abbreviation_buffer
        type(status_type) :: st
        character(len=:), allocatable :: name

        call fresh(h)
        ! all abbrevs
        call prepare_abbreviation_list_buffer(abbreviation_buffer, st)
        call buffer_name(abbreviation_buffer, name, st)
        call check(error, name, "*Abbrevs*")
        if (allocated(error)) return
        call close_buffer(abbreviation_buffer, st)
        ! mode-specific abbrevs
        call prepare_abbreviation_list_buffer(abbreviation_buffer, st, local=.true.)
        call buffer_name(abbreviation_buffer, name, st)
        call check(error, name, "*Abbrevs*")
        call close_buffer(abbreviation_buffer, st)
        call close_buffer(h, st)
    end subroutine test_list_abbreviations

    !> Test generation of abbrev list buffer.
    subroutine test_prepare_list_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, abbreviation_buffer
        type(status_type) :: st
        integer(int64) :: found

        call fresh(h)
        ! all abbrevs
        call setup_test_abbreviation_table()
        call prepare_abbreviation_list_buffer(abbreviation_buffer, st)
        call set_buffer(abbreviation_buffer, st)
        ! Check for a couple of abbrev-table names in buffer.
        call goto_character(point_minimum())
        call search_forward(abbreviation_table_name(ert_test_abbreviations), found, st, noerror=SEARCH_NOERROR)
        call check(error, found > 0, "ert_test_abbreviations")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call search_forward("global_abbreviation_table", found, st, noerror=SEARCH_NOERROR)
        call check(error, found > 0, "global_abbreviation_table")
        if (allocated(error)) return
        call check(error, buffers(current)%major_mode, "edit_abbreviations_mode")
        if (allocated(error)) return
        ! The format of the listing.
        call check(error, index(buffer_string(), "(ert_test_abbreviations)"//LF//LF//"""a-e-t""" &
                                //achar(9)//"       0    ""abbrev-ert-test"""//LF//LF//LF) > 0, buffer_string())
        if (allocated(error)) return
        call close_buffer(abbreviation_buffer, st)
        ! mode-specific abbrevs (temp buffer uses fundamental-mode)
        call set_buffer(h, st)
        call prepare_abbreviation_list_buffer(abbreviation_buffer, st, local=.true.)
        call set_buffer(abbreviation_buffer, st)
        call goto_character(point_minimum())
        call search_forward("fundamental_mode_abbreviation_table", found, st, noerror=SEARCH_NOERROR)
        call check(error, found > 0, "fundamental_mode_abbreviation_table")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call search_forward("global_abbreviation_table", found, st, noerror=SEARCH_NOERROR)
        call check(error, found == 0, "no global_abbreviation_table")
        if (allocated(error)) return
        call check(error, buffers(current)%major_mode /= "edit_abbreviations_mode", "not Edit-Abbrevs")
        call close_buffer(abbreviation_buffer, st)
        call close_buffer(h, st)
    end subroutine test_prepare_list_buffer

    !> Test inserting abbrev definitions into buffer.
    subroutine test_insert_abbreviations(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert_abbreviations(st)
        call check(error, st%ok() .and. buffer_has("(global_abbreviation_table)"), buffer_string())
        if (allocated(error)) return
        ! Point stays, the mark is after the text.
        call check(error, point(), 1_int64)
        call close_buffer(h, st)
    end subroutine test_insert_abbreviations

    !> Test writing abbrevs to a file (reading it is compiling the init
    !> file: its abbrevs are `define' calls in `configure').
    subroutine test_write_abbreviation_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: file, text

        call fresh(h)
        file = test_file("abbrev-init.f90")
        call setup_test_abbreviation_table()
        call global_abbreviation_table%define("teh", "the", count=3_int64)
        call global_abbreviation_table%define("sys", "system", system=.true.)
        call write_abbreviation_file(st, file)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = read_text(file)
        call check(error, &
                   text == "module user_configuration"//LF//"    use fortmacs"//LF//"    implicit none(type, external)"//LF// &
                   "    private"//LF//LF//"    public :: configure"//LF//LF//"contains"//LF//LF// &
                   "    subroutine configure(status)"//LF//"        type(status_type), intent(out) :: status"//LF// &
                   "        ! Abbrevs, written by write_abbreviation_file:"//LF// &
                   "        call ert_test_abbreviations%define(""a-e-t"", &"//LF// &
                   "            ""abbrev-ert-test"", count=0_int64)"//LF// &
                   "        call global_abbreviation_table%define(""teh"", &"//LF// &
                   "            ""the"", count=3_int64)"//LF// &
                   "        ! End of abbrevs."//LF// &
                   "    end subroutine configure"//LF//"end module user_configuration"//LF, text)
        if (allocated(error)) return
        ! Writing again replaces the abbrevs written before.
        call ert_test_abbreviations%clear()
        call write_abbreviation_file(st, file)
        text = read_text(file)
        call check(error, index(text, "a-e-t") == 0 .and. index(text, "count=3_int64") > 0 .and. &
                   index(text, "! End of abbrevs.") > 0, text)
        call delete_file_internal(file, st)
        call close_buffer(h, st)
    end subroutine test_write_abbreviation_file

    !> Test writing abbrevs with properties to a file; long and
    !> multi-line expansions are continued.
    subroutine test_write_abbreviation_file_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: file, text

        call fresh(h)
        file = test_file("abbrev-props.f90")
        call write_text(file, "module user_configuration"//LF//"    use fortmacs"//LF//"contains"//LF// &
                        "    subroutine configure(st)"//LF//"        type(status_type), intent(out) :: st"//LF// &
                        "        tab_width = 4"//LF//"    end subroutine configure"//LF//"end module user_configuration"//LF)
        call ert_test_abbreviations%define_table("ert_test_abbreviations")
        call ert_test_abbreviations%define("fb", "fooBar", case_fixed=.true.)
        call global_abbreviation_table%define("nl", "one"//LF//"""two""")
        call write_abbreviation_file(st, file)
        text = read_text(file)
        call check(error, text == "module user_configuration"//LF//"    use fortmacs"//LF//"contains"//LF// &
                   "    subroutine configure(st)"//LF//"        type(status_type), intent(out) :: st"//LF// &
                   "        tab_width = 4"//LF// &
                   "        ! Abbrevs, written by write_abbreviation_file:"//LF// &
                   "        call ert_test_abbreviations%define(""fb"", &"//LF// &
                   "            ""fooBar"", count=0_int64, case_fixed=.true.)"//LF// &
                   "        call global_abbreviation_table%define(""nl"", &"//LF// &
                   "            ""one""// &"//LF//"            achar(10)// &"//LF// &
                   "            """"""two"""""", count=0_int64)"//LF// &
                   "        ! End of abbrevs."//LF// &
                   "    end subroutine configure"//LF//"end module user_configuration"//LF, text)
        call delete_file_internal(file, st)
        call close_buffer(h, st)
    end subroutine test_write_abbreviation_file_properties

    !> Test that adding an inverse abbrev skips trailing nonword characters.
    subroutine test_inverse_add_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_table_type), target :: table

        call fresh(h)
        call insert("some text foo ", st)
        call type_text("bar"//achar(13))
        call inverse_add_abbreviation(table, "Global", 1_int64, st)
        call check(error, expansion_of("foo", table), "bar")
        call close_buffer(h, st)
    end subroutine test_inverse_add_1

    !> Test that adding an inverse abbrev skips trailing nonword characters.
    subroutine test_inverse_add_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_table_type), target :: table

        call fresh(h)
        call insert("some text foo ", st)
        call type_text("bar"//achar(13))
        call inverse_add_abbreviation(table, "Global", 2_int64, st)
        call check(error, expansion_of("text", table), "bar")
        call close_buffer(h, st)
    end subroutine test_inverse_add_2

    !> Test that adding an inverse abbrev skips trailing nonword characters.
    subroutine test_inverse_add_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_table_type), target :: table

        call fresh(h)
        call insert("some     text foo", st)
        call goto_character(point_minimum())
        call type_text("bar"//achar(13))
        call inverse_add_abbreviation(table, "Global", -1_int64, st)
        call check(error, expansion_of("text", table), "bar")
        call close_buffer(h, st)
    end subroutine test_inverse_add_3

    !> Test that `abbreviation_possibly_save' properly resets
    !> `abbreviations_changed'.
    subroutine test_possibly_save(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: result

        call fresh(h)
        abbreviation_file_name = test_file("abbrev-save.f90")
        save_abbreviations = .true.
        ! Save
        abbreviations_changed = .true.
        call abbreviation_possibly_save(.false., .true., result, st)
        call check(error,.not. result .and. .not. abbreviations_changed .and. file_exists(abbreviation_file_name), "save")
        if (allocated(error)) return
        ! Don't save
        abbreviations_changed = .true.
        call type_text("n"//achar(13))
        call abbreviation_possibly_save(.false., .false., result, st)
        call check(error, result .and. .not. abbreviations_changed, "don't save")
        if (allocated(error)) return
        ! Query mode.
        abbreviations_changed = .true.
        call abbreviation_possibly_save(.true., .false., result, st)
        call check(error, result .and. abbreviations_changed, "query")
        call delete_file_internal(abbreviation_file_name, st)
        deallocate (abbreviation_file_name)
        abbreviations_changed = .false.
        call discard_input()
        call close_buffer(h, st)
    end subroutine test_possibly_save

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> In Abbrev mode, a character without word syntax after a word
    !> expands it (cmds.c's self-insert-command); a word character does
    !> not, nor does Abbrev mode off.
    subroutine test_self_insert_expands(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        call execute_keys("teh SPC")
        call check(error, buffer_string(), "teh ")
        if (allocated(error)) return
        call erase_buffer(st)
        call execute_keys("M-x abbreviation_mode RET")
        call check(error, abbreviation_mode%value, "abbreviation_mode on")
        if (allocated(error)) return
        call execute_keys("teh SPC tehx SPC . SPC teh RET")
        call check(error, buffer_string(), "the tehx . the"//LF)
        if (allocated(error)) return
        call check(error, last_abbreviation_text, "teh")
        call close_buffer(h, st)
    end subroutine test_self_insert_expands

    !> A capitalized abbrev capitalizes the expansion, an all-caps one
    !> upcases it, a multi-word expansion gets its initials upcased.
    subroutine test_case_handling(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        call global_abbreviation_table%define("foo", "find outer otter")
        abbreviation_mode = .true.
        call execute_keys("Teh SPC TEH SPC tEh SPC FOO SPC Foo SPC")
        call check(error, buffer_string(), "The THE The Find Outer Otter Find outer otter ")
        call close_buffer(h, st)
    end subroutine test_case_handling

    subroutine test_all_caps(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("foo", "find outer otter")
        abbreviation_mode = .true.
        abbreviation_all_caps = .true.
        call execute_keys("FOO SPC")
        call check(error, buffer_string(), "FIND OUTER OTTER ")
        abbreviation_all_caps = .false.
        call close_buffer(h, st)
    end subroutine test_all_caps

    !> C-x ' and C-x a e expand without Abbrev mode, point after the
    !> abbrev or after spaces.
    subroutine test_expand_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        call execute_keys("teh C-x ' SPC teh C-x a e")
        call check(error, buffer_string(), "the the")
        if (allocated(error)) return
        ! An abbrev that is not defined does nothing.
        call execute_keys("SPC xyz C-x '")
        call check(error, buffer_string(), "the the xyz")
        call close_buffer(h, st)
    end subroutine test_expand_keys

    subroutine test_unexpand(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        abbreviation_mode = .true.
        call execute_keys("teh SPC ok")
        call check(error, buffer_string(), "the ok")
        if (allocated(error)) return
        call execute_keys("M-x unexpand_abbreviation RET")
        call check(error, buffer_string(), "teh ok")
        if (allocated(error)) return
        call check(error, point(), 7_int64)
        if (allocated(error)) return
        call check(error,.not. allocated(last_abbreviation_text), "last_abbreviation_text cleared")
        call close_buffer(h, st)
    end subroutine test_unexpand

    !> M-' marks the start of the abbrev and expands an abbrev before it;
    !> the hyphen goes when the abbrev expands.
    subroutine test_prefix_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        call global_abbreviation_table%define("ab", "about")
        abbreviation_mode = .true.
        call execute_keys("re M-' teh SPC")
        call check(error, buffer_string(), "rethe ")
        if (allocated(error)) return
        call erase_buffer(st)
        ! The prefix is itself an abbrev: it expands first.
        call execute_keys("ab M-' teh SPC")
        call check(error, buffer_string(), "aboutthe ")
        call close_buffer(h, st)
    end subroutine test_prefix_mark

    !> C-x a g defines a global abbrev for the word before point; a
    !> negative argument undefines it.
    subroutine test_add_global_abbreviation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert("the", st)
        call execute_keys("C-x a g TH RET")
        call check(error, expansion_of("th"), "the")
        if (allocated(error)) return
        call check(error, abbreviations_changed, "abbreviations_changed")
        if (allocated(error)) return
        ! Two words.
        call insert(" quick fox", st)
        call execute_keys("C-u 2 C-x a g qf RET")
        call check(error, expansion_of("qf", global_abbreviation_table), "quick fox")
        if (allocated(error)) return
        ! Redefining asks.
        call execute_keys("C-x a g qf RET n")
        call check(error, expansion_of("qf", global_abbreviation_table), "quick fox")
        if (allocated(error)) return
        call execute_keys("C-x a g qf RET y")
        call check(error, expansion_of("qf", global_abbreviation_table), "fox")
        if (allocated(error)) return
        call execute_keys("C-u - C-x a g qf RET")
        call check(error, expansion_of("qf", global_abbreviation_table), "<none>")
        call discard_input()
        call close_buffer(h, st)
    end subroutine test_add_global_abbreviation

    !> C-x a l defines a mode abbrev, a global one with
    !> `only_global_abbreviations'; the region is the expansion.
    subroutine test_add_mode_abbreviation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert("hello world", st)
        call execute_keys("C-x a l hw RET")
        call check(error, expansion_of("hw", fundamental_mode_abbreviation_table), "world")
        if (allocated(error)) return
        call check(error, expansion_of("hw", global_abbreviation_table), "<none>")
        if (allocated(error)) return
        only_global_abbreviations = .true.
        call push_mark(1_int64, .true., .true.)
        call execute_keys("C-x a l hello RET")
        call check(error, expansion_of("hello", global_abbreviation_table), "hello world")
        only_global_abbreviations = .false.
        call close_buffer(h, st)
    end subroutine test_add_mode_abbreviation

    !> C-x a i g reads the expansion of the word before point, defines
    !> it and expands it.
    subroutine test_inverse_add_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert("say foo.", st)
        call execute_keys("C-x a i g bar RET")
        call check(error, expansion_of("foo", global_abbreviation_table), "bar")
        if (allocated(error)) return
        call check(error, buffer_string(), "say bar.")
        call close_buffer(h, st)
    end subroutine test_inverse_add_keys

    subroutine test_expand_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        call insert("teh cat and teh dog", st)
        call expand_region_abbreviations(point_minimum(), point_maximum(), st, noquery=.true.)
        call check(error, buffer_string(), "the cat and the dog")
        if (allocated(error)) return
        ! Asking: y, then n.
        call erase_buffer(st)
        call insert("teh cat and teh dog", st)
        call type_text("yn")
        call expand_region_abbreviations(point_minimum(), point_maximum(), st)
        call check(error, buffer_string(), "the cat and teh dog")
        call close_buffer(h, st)
    end subroutine test_expand_region

    subroutine test_check_characters(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call define_global_abbreviation("a-b.c-", "x", st)
        call check(error, format_error(st), &
                   "Some abbrev characters (-.) are not word constituents in the standard syntax")
        if (allocated(error)) return
        call define_global_abbreviation("ABC", "x", st)
        call check(error, st%ok() .and. expansion_of("abc", global_abbreviation_table) == "x", "lower-cased")
        call close_buffer(h, st)
    end subroutine test_check_characters

    logical function never()
        never = .false.
    end function never

    !> case_fixed, enable_function, regexp and parents.
    subroutine test_table_properties(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_reference_type) :: symbol

        call fresh(h)
        abbreviation_mode = .true.
        ! An abbrev that is case-fixed only expands as defined.
        call global_abbreviation_table%define("fb", "fooBar", case_fixed=.true.)
        call execute_keys("Fb SPC fb SPC")
        call check(error, buffer_string(), "Fb fooBar ")
        if (allocated(error)) return
        ! A table that is case-fixed.
        call erase_buffer(st)
        global_abbreviation_table%case_fixed = .true.
        call global_abbreviation_table%define("teh", "the")
        call execute_keys("Teh SPC")
        call check(error, buffer_string(), "Teh ")
        global_abbreviation_table%case_fixed = .false.
        if (allocated(error)) return
        ! A table whose enable function says no.
        call erase_buffer(st)
        global_abbreviation_table%enable_function => never
        call execute_keys("teh SPC")
        global_abbreviation_table%enable_function => null()
        call check(error, buffer_string(), "teh ")
        if (allocated(error)) return
        ! A regexp naming the abbrev before point.
        call erase_buffer(st)
        global_abbreviation_table%regular_expression = "\(\#[a-z]+\)"
        call global_abbreviation_table%define("#inc", "#include")
        call execute_keys("x#inc SPC")
        deallocate (global_abbreviation_table%regular_expression)
        call check(error, buffer_string(), "x#include ")
        if (allocated(error)) return
        ! Parents: unlisted_table inherits global_abbreviation_table.
        call unlisted_table%append_parent(global_abbreviation_table)
        call abbreviation_name("teh", symbol, unlisted_table)
        call check(error, associated(symbol%table, global_abbreviation_table), "found in the parent")
        call close_buffer(h, st)
    end subroutine test_table_properties

    subroutine count_hook(value, status)
        logical, intent(out) :: value
        type(status_type), intent(out) :: status

        status = status_type()
        hook_calls = hook_calls + 1
        value = .true.
    end subroutine count_hook

    subroutine refuse_hook(value, status)
        logical, intent(out) :: value
        type(status_type), intent(out) :: status

        status = status_type()
        hook_calls = hook_calls + 1
        value = .false.
    end subroutine refuse_hook

    !> An abbrev's hook runs after the expansion; with `no_self_insert',
    !> its value says whether the character is inserted.
    subroutine test_hooks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_reference_type) :: symbol

        call fresh(h)
        abbreviation_mode = .true.
        hook_calls = 0
        call global_abbreviation_table%define("hk", "hooked", hook=count_hook)
        call execute_keys("hk SPC")
        call check(error, buffer_string() == "hooked " .and. hook_calls == 1, buffer_string())
        if (allocated(error)) return
        call erase_buffer(st)
        call global_abbreviation_table%define("ns", "nospace", hook=count_hook, no_self_insert=.true.)
        call execute_keys("ns SPC")
        call check(error, buffer_string() == "nospace" .and. hook_calls == 2, buffer_string())
        if (allocated(error)) return
        ! A no_self_insert hook returning false: no expansion took place.
        call erase_buffer(st)
        call global_abbreviation_table%define("rf", "refused", hook=refuse_hook, no_self_insert=.true.)
        call insert("rf", st)
        call expand_abbreviation(st, symbol)
        call check(error, buffer_string() == "refused" .and. .not. associated(symbol%table), "refused")
        call close_buffer(h, st)
    end subroutine test_hooks

    !> Each derived mode has its abbrev table, whose parent is the parent
    !> mode's; the local table goes back to Fundamental mode's on a mode
    !> change.
    subroutine test_mode_tables(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(abbreviation_table_type), pointer :: table

        call fresh(h)
        call text_mode(st)
        table => local_abbreviation_table%get_current()
        call check(error, associated(table, text_mode_abbreviation_table), "text_mode_abbreviation_table")
        if (allocated(error)) return
        call check(error, abbreviation_table_name(text_mode_abbreviation_table), "text_mode_abbreviation_table")
        if (allocated(error)) return
        call text_mode_abbreviation_table%define("txt", "text only")
        abbreviation_mode = .true.
        call execute_keys("txt SPC")
        call check(error, buffer_string(), "text only ")
        if (allocated(error)) return
        call fundamental_mode(st)
        table => local_abbreviation_table%get_current()
        call check(error, associated(table, fundamental_mode_abbreviation_table), "fundamental_mode_abbreviation_table")
        if (allocated(error)) return
        call erase_buffer(st)
        call execute_keys("txt SPC")
        call check(error, buffer_string(), "txt ")
        if (allocated(error)) return
        ! Help mode's table inherits Special mode's.
        call help_mode(st)
        table => local_abbreviation_table%get_current()
        call check(error, associated(table, help_mode_abbreviation_table), "help_mode_abbreviation_table")
        if (allocated(error)) return
        call check(error, allocated(help_mode_abbreviation_table%parents), "parents")
        if (allocated(error)) return
        call check(error, associated(help_mode_abbreviation_table%parents(1)%p, special_mode_abbreviation_table), "parent")
        call fundamental_mode(st)
        call close_buffer(h, st)
    end subroutine test_mode_tables

    !> Defining an abbrev changes `abbreviations_changed', unless it is a
    !> system abbrev or the definition is the same.
    subroutine test_abbreviations_changed(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("sy", "system", system=.true.)
        call check(error,.not. abbreviations_changed, "system")
        if (allocated(error)) return
        call global_abbreviation_table%define("us", "user")
        call check(error, abbreviations_changed, "user")
        if (allocated(error)) return
        abbreviations_changed = .false.
        call global_abbreviation_table%define("us", "user")
        call check(error,.not. abbreviations_changed, "same")
        if (allocated(error)) return
        ! A system abbrev doesn't override a user one, unless forced.
        call global_abbreviation_table%define("us", "other", system=.true.)
        call check(error, expansion_of("us", global_abbreviation_table), "user")
        if (allocated(error)) return
        call global_abbreviation_table%define("us", "other", system=.true., force=.true.)
        call check(error, expansion_of("us", global_abbreviation_table), "other")
        call close_buffer(h, st)
    end subroutine test_abbreviations_changed

    subroutine test_lighter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call check(error, mode_line_modes(h%index), "Fundamental")
        if (allocated(error)) return
        call execute_keys("M-x abbreviation_mode RET M-x auto_fill_mode RET")
        ! (Abbrev comes first when abbreviation_mode is defined first, as at startup.)
        call check(error, index(mode_line_modes(h%index), " Abbrev") > 0 .and. &
                   index(mode_line_modes(h%index), " Fill") > 0, mode_line_modes(h%index))
        if (allocated(error)) return
        call check(error, last_message, "Auto-Fill mode enabled in current buffer")
        call execute_keys("M-x abbreviation_mode RET")
        call check(error, last_message, "Abbreviation mode disabled in current buffer")
        call execute_keys("M-x auto_fill_mode RET")
        call close_buffer(h, st)
    end subroutine test_lighter

    !> C-o inserts a newline without expanding the abbrev before point;
    !> RET expands it.
    subroutine test_open_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call global_abbreviation_table%define("teh", "the")
        abbreviation_mode = .true.
        call execute_keys("teh C-o")
        call check(error, buffer_string(), "teh"//LF)
        if (allocated(error)) return
        call check(error, abbreviation_mode%value, "abbreviation_mode restored")
        call close_buffer(h, st)
    end subroutine test_open_line

    !> save_some_buffers offers to save the abbrevs (here with ARGUMENT: without
    !> asking).
    subroutine test_save_some_buffers(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(handle_type), allocatable :: all(:)
        integer(int64) :: i
        integer :: k
        character(len=:), allocatable :: text

        call fresh(h)
        abbreviation_file_name = test_file("abbrev-some.f90")
        ! Only the abbrevs need saving (not files earlier suites left modified).
        all = buffer_list()

        do k = 1, size(all)
            call resolve_buffer(all(k), i, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(i)%filename)) cycle
            call set_buffer(all(k), st)
            call restore_buffer_modified(.false.)
        end do

        call set_buffer(h, st)
        save_abbreviations = .true.
        call global_abbreviation_table%define("teh", "the")
        call save_some_buffers(.true., .false., st)
        call check(error, st%ok() .and. .not. abbreviations_changed, "saved: "//format_error(st))
        if (allocated(error)) return
        text = read_text(abbreviation_file_name)
        call check(error, index(text, "call global_abbreviation_table%define(""teh"",") > 0, text)
        if (allocated(error)) return
        ! save_abbreviations off: nothing happens.
        call delete_file_internal(abbreviation_file_name, st)
        save_abbreviations = .false.
        abbreviations_changed = .true.
        call save_some_buffers(.true., .false., st)
        call check(error, abbreviations_changed .and. .not. file_exists(abbreviation_file_name), "not saved")
        save_abbreviations = .true.
        abbreviations_changed = .false.
        deallocate (abbreviation_file_name)
        call close_buffer(h, st)
    end subroutine test_save_some_buffers
end module test_abbreviation_mod
