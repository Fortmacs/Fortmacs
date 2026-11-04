! test_etags_mod.f90 -- tests for etags_mod.
! Ports GNU Emacs 31.1 test/lisp/progmodes/etags-tests.el in part; its
! tags tables (test/manual/etags) are replaced by small ones of the same
! shape, and a buffer-local `tags_table_list' is not ported.
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

!> etags-tests.el's cases on the tags tables of test/etags-resources:
!> ETAGS.good_1 has two definitions of PrintAdd and of
!> LL_Task_Procedure_Access/t, ETAGS.good_3 also intNumber (upstream's
!> tables of the same names are generated from the etags test sources).
!> Own tests: the files and tag names of a table, `find_tag' with its
!> next and pop back, `list_tags', `apropos_tags' and `find_xref_apropos'.
module test_etags_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error, FM_USER_ERROR
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, current_buffer, buffers, current, &
                          get_buffer
    use editfns_mod, only: buffer_string, point, line_beginning_position
    use fileio_mod, only: expand_file_name, fortmacs_wd, init_fileio, file_name_as_directory, file_name_nondirectory
    use bindings_mod, only: init_bindings
    use simple_mod, only: fundamental_mode
    use xref_mod, only: xref_item_type, find_xref_definitions, find_xref_apropos, xref_items_at
    use etags_mod, only: visit_tags_table, visit_tags_table_buffer, reset_tags_tables, tags_file_name, tags_table_list, &
                         tags_add_tables, find_tag, list_tags, apropos_tags, TAGS_ADD_TABLES_NO, TAGS_ADD_TABLES_ASK, &
                         FIND_TAG_NEXT_YES, FIND_TAG_NEXT_POP_BACK
    implicit none(type, external)
    private

    public :: collect_etags_mod

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_etags_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("etags-bug-158", test_bug_158), &
                    new_unittest("etags-bug-23164", test_bug_23164), &
                    new_unittest("etags-buffer-local-tags-table-list", test_tags_table_list), &
                    new_unittest("etags-find-tag", test_find_tag), &
                    new_unittest("etags-list-tags", test_list_tags), &
                    new_unittest("etags-tags-apropos", test_apropos_tags), &
                    new_unittest("etags-xref-apropos", test_xref_apropos) &
                    ]
    end subroutine collect_etags_mod

    !> The file NAME of test/etags-resources (the directory for "").
    !> Port of etags-tests.el etags-tests--test-dir.
    function resource(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        type(status_type) :: st

        call init_fileio()
        call expand_file_name("test/etags-resources/"//name, path, st, fortmacs_wd)
    end function resource

    ! A fresh buffer NAME in the resource directory, shown alone, with the
    ! tags state reset.  Not a port.
    subroutine fresh_buffer(name, h)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call reset_tags_tables(st)
        tags_add_tables = TAGS_ADD_TABLES_ASK
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        buffers(current)%directory = file_name_as_directory(resource(""))
        call show_alone()
    end subroutine fresh_buffer

    ! Find the definitions of IDENTIFIER in buffer FROM: XREF_BUFFER is the
    ! *xref* buffer listing them (closed after), if any.  Not a port.
    subroutine definitions(from, identifier, xref_buffer, status)
        type(handle_type), intent(in) :: from
        character(len=*), intent(in) :: identifier
        type(handle_type), intent(out) :: xref_buffer
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call set_buffer(from, st)
        call show_alone()
        call find_xref_definitions(identifier, xref_buffer, status)
        call set_buffer(from, st)
    end subroutine definitions

    !> Test finding tags with local and global tags tables.  Port of
    !> etags-tests.el etags-bug-158 (the answer of `read_y_or_n' is
    !> `tags_add_tables' = TAGS_ADD_TABLES_NO).
    subroutine test_bug_158(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global_buffer, local_buffer, xref_buffer
        type(status_type) :: st

        call fresh_buffer("*buf-local*", local_buffer)
        call fresh_buffer("*buf-global*", global_buffer)
        call visit_tags_table(resource("ETAGS.good_1"), st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! Check that tags in ETAGS.good_1 are recognized.
        call definitions(global_buffer, "LL_Task_Procedure_Access/t", xref_buffer, st)
        call check(error, st%ok() .and. xref_buffer%index /= 0, "LL_Task_Procedure_Access/t: "//format_error(st))
        if (allocated(error)) return
        call close_buffer(xref_buffer, st)
        call definitions(global_buffer, "PrintAdd", xref_buffer, st)
        call check(error, st%ok() .and. xref_buffer%index /= 0, "PrintAdd: "//format_error(st))
        if (allocated(error)) return
        call close_buffer(xref_buffer, st)
        ! Check that tags not in ETAGS.good_1, but in ETAGS.good_3, are NOT
        ! recognized.
        call definitions(global_buffer, "intNumber", xref_buffer, st)
        call check(error, st%code == FM_USER_ERROR, "intNumber in good_1: "//format_error(st))
        if (allocated(error)) return
        call set_buffer(local_buffer, st)
        tags_add_tables = TAGS_ADD_TABLES_NO
        call visit_tags_table(resource("ETAGS.good_3"), st, local=.true.)
        tags_add_tables = TAGS_ADD_TABLES_ASK
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! Check that tags in ETAGS.good_1 are recognized.
        call definitions(local_buffer, "LL_Task_Procedure_Access/t", xref_buffer, st)
        call check(error, st%ok() .and. xref_buffer%index /= 0, "local LL_Task_Procedure_Access/t: "//format_error(st))
        if (allocated(error)) return
        call close_buffer(xref_buffer, st)
        call definitions(local_buffer, "PrintAdd", xref_buffer, st)
        call check(error, st%ok() .and. xref_buffer%index /= 0, "local PrintAdd: "//format_error(st))
        if (allocated(error)) return
        call close_buffer(xref_buffer, st)
        ! Check that tags in ETAGS.good_3 are recognized.  This is a test
        ! for bug#158.
        call definitions(local_buffer, "intNumber", xref_buffer, st)
        call check(error, st%ok(), "intNumber in good_3: "//format_error(st))
        if (allocated(error)) return
        if (xref_buffer%index /= 0) call close_buffer(xref_buffer, st)
        ! Bug #17326
        call set_buffer(local_buffer, st)
        call check(error, file_name_nondirectory(tags_file_name%value), "ETAGS.good_3")
        if (allocated(error)) return
        call set_buffer(global_buffer, st)
        call check(error, file_name_nondirectory(tags_file_name%value), "ETAGS.good_1")
        call close_buffer(local_buffer, st)
        call close_buffer(global_buffer, st)
    end subroutine test_bug_158

    !> Test that setting a local value of tags table doesn't cause an
    !> errors.  Port of etags-tests.el etags-bug-23164.
    subroutine test_bug_23164(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, xref_buffer
        type(status_type) :: st

        call fresh_buffer("*foobar*", h)
        call fundamental_mode(st)
        call visit_tags_table(resource("ETAGS.good_3"), st, local=.true.)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call definitions(h, "foobar123", xref_buffer, st)
        call check(error, st%code == FM_USER_ERROR, "a user error")
        if (allocated(error)) return
        call check(error, format_error(st), "No definitions found for: foobar123")
        call close_buffer(h, st)
    end subroutine test_bug_23164

    !> Test that `tags_table_list' is used without prompting.  Port of
    !> etags-tests.el etags-buffer-local-tags-table-list (with the global
    !> value: a buffer-local `tags_table_list' is not ported).
    subroutine test_tags_table_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: visited

        call fresh_buffer("etag-test-tmpfile", h)
        call fundamental_mode(st)
        tags_table_list = [character(len=len(resource("ETAGS.good_3"))) :: resource("ETAGS.good_3")]
        call tags_file_name%clear()
        call visit_tags_table_buffer(visited, st)
        call check(error, st%ok() .and. visited, format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call check(error, tags_file_name%value, resource("ETAGS.good_3"))
        call reset_tags_tables(st)
        call close_buffer(h, st)
    end subroutine test_tags_table_list

    !> `find_tag' goes to the first definition of PrintAdd, the next one
    !> with FIND_TAG_NEXT_YES, and back to the last tag gone to with
    !> FIND_TAG_NEXT_POP_BACK.  Not a port.
    subroutine test_find_tag(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, b
        type(status_type) :: st
        character(len=:), allocatable :: name

        call fresh_buffer("*find-tag*", h)
        call visit_tags_table(resource("ETAGS.good_1"), st)
        call find_tag("PrintAdd", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, file_name_nondirectory(buffers(current)%filename), "a.c")
        if (allocated(error)) return
        call check(error, point() == 1_int64, "at the first line")
        if (allocated(error)) return
        call find_tag("", st, FIND_TAG_NEXT_YES)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, file_name_nondirectory(buffers(current)%filename), "a2.c")
        if (allocated(error)) return
        call check(error, point() == line_beginning_position() .and. point() == 28_int64, "at the second line")
        if (allocated(error)) return
        call find_tag("", st, FIND_TAG_NEXT_YES)
        call check(error, format_error(st), "No more tags containing PrintAdd")
        if (allocated(error)) return
        call find_tag("", st, FIND_TAG_NEXT_POP_BACK)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        name = ""
        b = current_buffer()
        if (allocated(buffers(current)%filename)) name = file_name_nondirectory(buffers(current)%filename)
        ! The most recent tag location: popping does not go back to where
        ! the search started (that is `go_back_in_xref_history').
        call check(error, name, "a2.c")
        call close_buffer(h, st)
    end subroutine test_find_tag

    !> `list_tags' lists the tags of a file in *Tags List*.  Not a port.
    subroutine test_list_tags(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, list
        type(status_type) :: st

        call fresh_buffer("*list-tags*", h)
        call visit_tags_table(resource("ETAGS.good_3"), st)
        call list_tags("src/b.adb", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        list = get_buffer("*Tags List*")
        call set_buffer(list, st)
        call check(error, buffer_string(), "Tags in file ‘src/b.adb’:"//LF//LF//"   task type LL_Task_Procedure_Access"//LF)
        if (allocated(error)) return
        call set_buffer(h, st)
        call list_tags("src/none.c", st)
        call check(error, format_error(st), "File src/none.c not in current tags tables")
        call close_buffer(list, st)
        call close_buffer(h, st)
    end subroutine test_list_tags

    !> `apropos_tags' lists the tags a regexp matches.  Not a port.
    subroutine test_apropos_tags(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, list
        type(status_type) :: st

        call fresh_buffer("*tags-apropos*", h)
        call visit_tags_table(resource("ETAGS.good_3"), st)
        call apropos_tags("intNum", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        list = get_buffer("*Tags List*")
        call set_buffer(list, st)
        call check(error, buffer_string(), "Click mouse-2 to follow tags."//LF//LF//"Tags matching regexp ‘intNum’:"//LF//LF// &
                   "[src/c.cpp]:    int intNumber;"//LF)
        call close_buffer(list, st)
        call close_buffer(h, st)
    end subroutine test_apropos_tags

    !> `find_xref_apropos' with the etags backend lists the tags of a word.
    !> Not a port.
    subroutine test_xref_apropos(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, xref_buffer
        type(xref_item_type), allocatable :: items(:)
        type(status_type) :: st

        call fresh_buffer("*xref-apropos*", h)
        call visit_tags_table(resource("ETAGS.good_3"), st)
        call find_xref_apropos("PrintAdd", xref_buffer, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call xref_items_at(xref_buffer, items)
        call check(error, size(items), 2)
        if (allocated(error)) return
        call check(error, items(1)%summary, "int PrintAdd (int x)")
        if (allocated(error)) return
        call check(error, items(2)%summary, "int PrintAdd (int y)")
        call close_buffer(xref_buffer, st)
        call close_buffer(h, st)
    end subroutine test_xref_apropos
end module test_etags_mod
