! minibuf_table_fm.f90 -- completion tables.
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

!> Completion tables (COLLECTION arguments: lists, the obarray and
!> functions) and the completion data the functions of
!> `completion_at_point_functions' return.  Split from minibuf_fm so
!> eval_fm's hooks can hold such functions (minibuf_fm re-exports the
!> tables).
module minibuf_table_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type
    implicit none(type, external)
    private

    public :: completion_list_table, completion_obarray_table, completion_function_table, completion_predicate, &
              completion_function

    !> Actions passed to a completion function (programmed completion): as
    !> `try_completion' (nil), `all_completions' (t), `test_completion'
    !> (lambda), and `(boundaries . SUFFIX)'.
    integer, parameter, public :: COMPLETE_TRY = 0, COMPLETE_ALL = 1, COMPLETE_TEST = 2, &
                                  COMPLETE_BOUNDARIES = 3

    !> Kinds of completion table: a list (of strings, symbols, or lists
    !> whose first element is one: an alist), the obarray, or a function.
    integer, parameter, public :: TABLE_LIST = 0, TABLE_OBARRAY = 1, TABLE_FUNCTION = 2

    abstract interface
        !> A completion predicate: whether the element `elt' (the list
        !> element or the symbol) is an acceptable completion.
        logical function completion_predicate(elt)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: elt
        end function completion_predicate

        !> A function completion table.  ACTION says what to compute for
        !> STRING: COMPLETE_TRY (the `try_completion' value: nil, t or a
        !> string), COMPLETE_ALL (a list of strings), COMPLETE_TEST (t or
        !> nil) or COMPLETE_BOUNDARIES (a list (START END) for STRING and
        !> SUFFIX, or nil for the default).
        recursive subroutine completion_function(string, action, result, status, pred, suffix)
            import :: value_type, status_type, completion_predicate
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer, intent(in) :: action
            type(value_type), intent(out) :: result
            type(status_type), intent(out) :: status
            procedure(completion_predicate), optional :: pred
            character(len=*), intent(in), optional :: suffix
        end subroutine completion_function

    end interface

    !> A completion table (COLLECTION).
    type, public :: completion_table_type
        integer :: kind = TABLE_LIST
        !> TABLE_LIST: the list.
        type(value_type) :: list
        !> TABLE_FUNCTION: the function.
        procedure(completion_function), pointer, nopass :: fn => null()
        !> The completion category (the `category' entry of the table's
        !> metadata), empty for none.
        character(len=:), allocatable :: category
    end type completion_table_type

    !> What a function of `completion_at_point_functions' returns: nothing
    !> (`found' false: not applicable at point), or the text from `start'
    !> to `end' to complete with `table' (the list (START END COLLECTION .
    !> PROPS)).  (Functions that return a function to perform completion,
    !> or perform it themselves, are not supported; the other properties
    !> of `completion_extra_properties' are deferred.)
    type, public :: capf_data_type
        logical :: found = .false.
        integer(int64) :: start = 0, end = 0
        type(completion_table_type) :: table
        !> The `:predicate' property: candidates must satisfy it.
        procedure(completion_predicate), pointer, nopass :: predicate => null()
        !> The `:exclusive' property: false (`no') means that if the
        !> table fails to match the text at point, the next function is
        !> tried instead of reporting a completion failure.
        logical :: exclusive = .true.
    end type capf_data_type

contains

    !> A completion table holding the list `list' (strings, symbols, or
    !> lists whose first element is the completion), in completion
    !> category `category'.
    function completion_list_table(list, category) result(table)
        type(value_type), intent(in) :: list
        character(len=*), intent(in), optional :: category
        type(completion_table_type) :: table
        table%kind = TABLE_LIST
        table%list = list
        if (present(category)) table%category = category
    end function completion_list_table

    !> A completion table holding the names of all interned symbols, in
    !> completion category `category'.
    function completion_obarray_table(category) result(table)
        character(len=*), intent(in), optional :: category
        type(completion_table_type) :: table
        table%kind = TABLE_OBARRAY
        if (present(category)) table%category = category
    end function completion_obarray_table

    !> A completion table computed by `fn' (programmed completion), in
    !> completion category `category' (the table's metadata).
    function completion_function_table(fn, category) result(table)
        procedure(completion_function) :: fn
        character(len=*), intent(in), optional :: category
        type(completion_table_type) :: table
        table%kind = TABLE_FUNCTION
        table%fn => fn
        if (present(category)) table%category = category
    end function completion_function_table
end module minibuf_table_fm
