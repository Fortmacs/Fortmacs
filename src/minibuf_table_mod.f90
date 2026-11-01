! minibuf_table_mod.f90 -- completion tables.
! Ported from GNU Emacs 31.1 src/minibuf.c and lisp/minibuffer.el.
!
! Copyright (C) 1985-1986, 1993-2026 Free Software Foundation, Inc.
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

!> Completion tables (COLLECTION arguments: arrays, the name table and
!> functions) and the completion data the functions of
!> `completion_at_point_functions' return.  Split from minibuf_mod so
!> hooks_mod's hooks can hold such functions (minibuf_mod re-exports the
!> tables).
module minibuf_table_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use value_mod, only: value_type
    implicit none(type, external)
    private

    public :: completion_list_table, completion_obarray_table, completion_function_table, completion_test_procedure, &
              completion_procedure, completion_table_with_quoting, completion_unquote_procedure, completion_quote_procedure, &
              completion_requote_procedure, completion_exit_function_procedure, capf_perform_procedure

    !> Actions passed to a completion function (programmed completion): as
    !> `try_completion' (COMPLETE_TRY), `all_completions' (COMPLETE_ALL), `is_exact_completion'
    !> (COMPLETE_TEST), and the boundaries of SUFFIX (COMPLETE_BOUNDARIES).
    integer, parameter, public :: COMPLETE_TRY = 0, COMPLETE_ALL = 1, COMPLETE_TEST = 2, &
                                  COMPLETE_BOUNDARIES = 3

    !> Kinds of completion table: a list (an array of strings or names,
    !> or a table of (string, value) entries), the name table, a function, or
    !> a function operating on quoted text (`completion_table_with_quoting').
    integer, parameter, public :: TABLE_LIST = 0, TABLE_OBARRAY = 1, TABLE_FUNCTION = 2, TABLE_QUOTING = 3

    !> How a completion finished, as an `:exit_function' is told:
    !> `exact' (an exact match, maybe not unique), `sole' (the sole
    !> completion, which may be extended further), `finished' (no further
    !> completion is possible), or `unknown' (completion--done finds out).
    integer, parameter, public :: COMPLETION_EXACT = 1, COMPLETION_SOLE = 2, COMPLETION_FINISHED = 3, &
                                  COMPLETION_UNKNOWN = 4

    abstract interface
        !> A completion test: whether the element `elt' (the list
        !> element or the name) is an acceptable completion.  Not a port.
        logical function completion_test_procedure(elt)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: elt
        end function completion_test_procedure

        !> A function completion table.  ACTION says what to compute for
        !> STRING: COMPLETE_TRY (the `try_completion' value: none, exact or a
        !> string), COMPLETE_ALL (an array of strings), COMPLETE_TEST (.true. or
        !> .false.) or COMPLETE_BOUNDARIES (START and END for STRING and
        !> SUFFIX, or nothing for the default).  Not a port.
        recursive subroutine completion_procedure(string, action, result, status, test, suffix)
            import :: value_type, status_type, completion_test_procedure
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer, intent(in) :: action
            type(value_type), intent(out) :: result
            type(status_type), intent(out) :: status
            procedure(completion_test_procedure), optional :: test
            character(len=*), intent(in), optional :: suffix
        end subroutine completion_procedure

        !> The UNQUOTE function of a quoting table: STRING unquoted, as
        !> USTRING.
        recursive subroutine completion_unquote_procedure(string, ustring)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: ustring
        end subroutine completion_unquote_procedure

        !> A QFUN of a quoting table: STRING quoted for its position, as
        !> QSTRING.
        recursive subroutine completion_quote_procedure(string, qstring)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: qstring
        end subroutine completion_quote_procedure

        !> An `:exit_function': run after completion of STRING is
        !> performed, FINISHED saying how (a COMPLETION_* value).
        recursive subroutine completion_exit_function_procedure(string, finished)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer, intent(in) :: finished
        end subroutine completion_exit_function_procedure

        !> A function a function of `completion_at_point_functions' may
        !> return instead of completion data: it performs completion
        !> itself, returning whether it did anything.
        recursive logical function capf_perform_procedure()
            implicit none(type, external)
        end function capf_perform_procedure
    end interface

    abstract interface
        !> The REQUOTE function of a quoting table: QSTR is a string
        !> entered by the user (and hence indicating the user's preferred
        !> form of quoting), and UPOS a position within the unquoted form
        !> of QSTR; QPOS is the position corresponding to UPOS but in
        !> QSTR, and QFUN a function returning its argument appropriately
        !> quoted for use at QPOS.
        recursive subroutine completion_requote_procedure(upos, qstr, qpos, qfun)
            import :: int64, completion_quote_procedure
            implicit none(type, external)
            integer(int64), intent(in) :: upos
            character(len=*), intent(in) :: qstr
            integer(int64), intent(out) :: qpos
            procedure(completion_quote_procedure), pointer, intent(out) :: qfun
        end subroutine completion_requote_procedure
    end interface

    !> A completion table (COLLECTION).
    type, public :: completion_table_type
        integer :: kind = TABLE_LIST
        !> TABLE_LIST: the list.
        type(value_type) :: list
        !> TABLE_FUNCTION: the function; TABLE_QUOTING: the function
        !> operating on the unquoted text.
        procedure(completion_procedure), pointer, nopass :: fn => null()
        !> TABLE_QUOTING: the functions unquoting and requoting the text.
        procedure(completion_unquote_procedure), pointer, nopass :: unquote => null()
        procedure(completion_requote_procedure), pointer, nopass :: requote => null()
        !> The completion category (the `category' entry of the table's
        !> metadata), empty for none.
        !> Whether the table's metadata gives `identity' as its
        !> `display_sort_function' and `cycle_sort_function': the
        !> candidates are shown in the table's order.
        logical :: unsorted = .false.
        character(len=:), allocatable :: category
    end type completion_table_type

    !> What a function of `completion_at_point_functions' returns: nothing
    !> (`found' false: not applicable at point), or the text from `start'
    !> to `end' to complete with `table' (the list (START END COLLECTION .
    !> PROPS)), or a function `perform' that performs completion itself.
    !> (Functions that perform completion themselves while being called
    !> are not supported; the other properties of
    !> `completion_extra_properties' are deferred.)
    type, public :: capf_data_type
        logical :: found = .false.
        integer(int64) :: start = 0, end = 0
        type(completion_table_type) :: table
        !> The function returned instead of completion data, if associated.
        procedure(capf_perform_procedure), pointer, nopass :: perform => null()
        !> The `:test' property: candidates must satisfy it.
        procedure(completion_test_procedure), pointer, nopass :: test => null()
        !> The `:exit_function' property: run after completion is performed.
        procedure(completion_exit_function_procedure), pointer, nopass :: exit_function => null()
        !> The `:exclusive' property: false (`no') means that if the
        !> table fails to match the text at point, the next function is
        !> tried instead of reporting a completion failure.
        logical :: exclusive = .true.
    end type capf_data_type

contains

    !> A completion table holding the list `list' (strings, names, or
    !> lists whose first element is the completion), in completion
    !> category `category'.  Not a port.
    function completion_list_table(list, category) result(table)
        type(value_type), intent(in) :: list
        character(len=*), intent(in), optional :: category
        type(completion_table_type) :: table

        table%kind = TABLE_LIST
        table%list = list
        if (present(category)) table%category = category
    end function completion_list_table

    !> Return a new completion table operating on quoted text.
    !> TABLE operates on the unquoted text.  UNQUOTE is a function that
    !> takes a string and returns a new unquoted string.  REQUOTE is a
    !> function of 2 args (UPOS QSTR) where QSTR is a string entered by the
    !> user (and hence indicating the user's preferred form of quoting);
    !> and UPOS is a position within the unquoted form of QSTR.  REQUOTE
    !> should return two values, QPOS and QFUN, such that QPOS is the position
    !> corresponding to UPOS but in QSTR, and QFUN is a function of one
    !> argument (a string) which returns that argument appropriately
    !> quoted for use at QPOS.  Port of
    !> minibuffer.el:completion-table-with-quoting (TABLE a function; its
    !> procedure is minibuf_mod's `quoting_table_call').
    function completion_table_with_quoting(table, unquote, requote) result(qtable)
        procedure(completion_procedure) :: table
        procedure(completion_unquote_procedure) :: unquote
        procedure(completion_requote_procedure) :: requote
        type(completion_table_type) :: qtable

        qtable%kind = TABLE_QUOTING
        qtable%fn => table
        qtable%unquote => unquote
        qtable%requote => requote
    end function completion_table_with_quoting

    !> A completion table holding the names of all interned names, in
    !> completion category `category'.  Not a port.
    function completion_obarray_table(category) result(table)
        character(len=*), intent(in), optional :: category
        type(completion_table_type) :: table

        table%kind = TABLE_OBARRAY
        if (present(category)) table%category = category
    end function completion_obarray_table

    !> A completion table computed by `fn' (programmed completion), in
    !> completion category `category' (the table's metadata).  Not a port.
    function completion_function_table(fn, category) result(table)
        procedure(completion_procedure) :: fn
        character(len=*), intent(in), optional :: category
        type(completion_table_type) :: table

        table%kind = TABLE_FUNCTION
        table%fn => fn
        if (present(category)) table%category = category
    end function completion_function_table
end module minibuf_table_mod
