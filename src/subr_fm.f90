! subr_fm.f90 -- basic functions from subr.el.
! Ported from GNU Emacs 31.1 lisp/subr.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Questions in the echo area (`read-key`, `y-or-n-p`) and
!> `add-to-history`.
!>
!> Without a minibuffer yet, `y-or-n-p` behaves as upstream does with
!> `y-or-n-p-use-read-key` set: it reads a single key in the echo area.
module subr_fm
    use error_fm, only: status_type, set_error, FM_QUIT
    use, intrinsic :: iso_fortran_env, only: int64
    use lisp_fm, only: value_type, fixnump, xfixnum, nil, make_string, cons, list_length, list_elt, &
                       list_elements, make_list, stringp, xstring
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set
    use keyboard_fm, only: read_event, discard_input
    use xdisp_fm, only: message, cursor_in_echo_area
    use keymap_fm, only: define_key, current_global_map
    implicit none(type, external)
    private

    public :: read_key, y_or_n_p, query_replace_answer, add_to_history
    public :: global_set_key

    !> Give KEY a global binding as COMMAND (a command name or any key
    !> definition).
    interface global_set_key
        module procedure global_set_key_value, global_set_key_command
    end interface global_set_key

    !> Maximum length of history lists before truncation takes place.
    !> A number means truncate to that length; truncation deletes old
    !> elements, and is done just after inserting a new element.
    !> Port of minibuf.c history-length (no t for "no truncation" yet).
    integer, public :: history_length = 100
    public :: ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, ANSWER_ACT_AND_EXIT, &
              ANSWER_AUTOMATIC, ANSWER_RECENTER, ANSWER_HELP, ANSWER_QUIT, ANSWER_NONE

    ! The `query-replace-map` bindings in use (replace.el).
    integer, parameter :: ANSWER_NONE = 0, ANSWER_ACT = 1, ANSWER_SKIP = 2, ANSWER_EXIT = 3, &
                          ANSWER_ACT_AND_EXIT = 4, ANSWER_AUTOMATIC = 5, ANSWER_RECENTER = 6, &
                          ANSWER_HELP = 7, ANSWER_QUIT = 8

contains

    !> Read one key with `prompt` in the echo area and the cursor after it.
    !> End of input reads as a quit.  Port of subr.el:read-key (single
    !> events; no keymap lookup of multi-event keys yet).
    subroutine read_key(prompt, key, status)
        character(len=*), intent(in) :: prompt
        type(value_type), intent(out) :: key
        type(status_type), intent(out) :: status
        logical :: have

        call message(prompt)
        cursor_in_echo_area = .true.
        call read_event(key, have, status)
        cursor_in_echo_area = .false.
        if (.not. status%ok()) return
        if (.not. have) call set_error(status, FM_QUIT)
    end subroutine read_key


    !> What `key` means in replace.el:query-replace-map (the bindings
    !> y-or-n-p and map-y-or-n-p use), or ANSWER_NONE.
    integer function query_replace_answer(key) result(answer)
        type(value_type), intent(in) :: key

        answer = ANSWER_NONE
        if (.not. fixnump(key)) return

        select case (xfixnum(key))
        case (iachar(" "), iachar("y"), iachar("Y"))
            answer = ANSWER_ACT
        case (127, iachar("n"), iachar("N"))
            answer = ANSWER_SKIP
        case (iachar("q"), 13)
            answer = ANSWER_EXIT
        case (iachar("."))
            answer = ANSWER_ACT_AND_EXIT
        case (iachar("!"))
            answer = ANSWER_AUTOMATIC
        case (12)
            answer = ANSWER_RECENTER
        case (8, iachar("?"))
            answer = ANSWER_HELP
            ! C-g, C-], and ESC (exit-prefix)
        case (7, 29, 27)
            answer = ANSWER_QUIT
        case default
            answer = ANSWER_NONE
        end select
    end function query_replace_answer


    !> Ask a "y or n" question in the echo area: `answer` is true for y
    !> (or SPC), false for n (or DEL); C-g quits.  Other keys ask again,
    !> with "Please answer y or n.".  Port of subr.el:y-or-n-p, the
    !> `y-or-n-p-use-read-key` branch (no help-form, scrolling or ding).
    subroutine y_or_n_p(prompt, answer, status)
        character(len=*), intent(in) :: prompt
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: padded
        type(value_type) :: key
        integer :: def
        logical :: first

        answer = .false.
        padded = prompt

        if (len(prompt) > 0) then
            if (prompt(len(prompt):len(prompt)) /= " ") padded = padded//" "
        end if

        padded = padded//"(y or n) "
        first = .true.

        do
            if (first) then
                call read_key(padded, key, status)
            else
                call read_key("Please answer y or n.  "//padded, key, status)
            end if

            if (.not. status%ok()) return
            first = .false.
            def = query_replace_answer(key)

            select case (def)
            case (ANSWER_ACT, ANSWER_SKIP)
                exit
            case (ANSWER_RECENTER)
                first = .true.
            case (ANSWER_QUIT)
                ! quit, or exit-prefix (ESC)
                call set_error(status, FM_QUIT)
                return
            case default
                call discard_input()
            end select
        end do

        answer = def == ANSWER_ACT

        if (answer) then
            call message(padded//"y")
        else
            call message(padded//"n")
        end if
    end subroutine y_or_n_p


    !> Add `newelt` to the front of history list `history_var` (a symbol
    !> name) unless it is already first, keeping at most history-length
    !> elements.  Port of subr.el:add-to-history
    !> (history-delete-duplicates nil).
    subroutine add_to_history(history_var, newelt)
        character(len=*), intent(in) :: history_var, newelt
        type(value_type) :: history, first, longer
        type(value_type), allocatable :: elems(:)
        type(status_type) :: status
        logical :: ok

        call symbol_value(sym(history_var), history, status)
        if (.not. status%ok()) history = nil()

        if (list_length(history) > 0) then
            first = list_elt(history, 1_int64)

            if (stringp(first)) then
                if (xstring(first) == newelt .and. len(xstring(first)) == len(newelt)) return
            end if
        end if

        call cons(make_string(newelt), history, longer, ok)
        if (.not. ok) return

        if (list_length(longer) > history_length) then
            call list_elements(longer, elems)
            longer = make_list(elems(1:history_length))
        end if

        call set(sym(history_var), longer, status)
    end subroutine add_to_history


    !> Give KEY a global binding as COMMAND.
    !> This is a legacy function; see `keymap_global_set` for the
    !> recommended function to use instead.
    !>
    !> COMMAND is the command definition to use; usually it is
    !> a symbol naming an interactively-callable function.
    !> KEY is a key sequence: a vector of events, as `key_parse` makes.
    !>
    !> Note that if KEY has a local binding in the current buffer,
    !> that local binding will continue to shadow any global binding
    !> that you make with this function.  Port of subr.el:global-set-key.
    subroutine global_set_key_value(key, command, status)
        type(value_type), intent(in) :: key(:), command
        type(status_type), intent(out) :: status

        call define_key(current_global_map(), key, command, status)
    end subroutine global_set_key_value


    !> `global_set_key` with the command named COMMAND.
    subroutine global_set_key_command(key, command, status)
        type(value_type), intent(in) :: key(:)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status

        call define_key(current_global_map(), key, sym(command), status)
    end subroutine global_set_key_command

end module subr_fm
