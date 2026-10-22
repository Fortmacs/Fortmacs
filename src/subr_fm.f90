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

!> Questions in the echo area (`read-key`, `y-or-n-p`),
!> `add-to-history`, and the match data helpers (`match-string`,
!> `string-match-p`, `looking-at-p`, `looking-back`).
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
    use search_fm, only: string_match, looking_at, re_search_backward, match_beginning, match_end, &
                         match_in_string_p, string_index_to_offset, SEARCH_NOERROR
    use editfns_fm, only: buffer_substring, point, goto_char, point_min, point_max, narrow_to_region, widen
    implicit none(type, external)
    private

    public :: read_key, y_or_n_p, query_replace_answer, add_to_history
    public :: global_set_key
    public :: match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back

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

    !> Add NEWELT to the history list stored in the variable HISTORY-VAR.
    !> If MAXELT is present, it specifies the maximum length of the history.
    !> Otherwise, the maximum history length is the value of the
    !> `history_length` variable.
    !> If KEEP-ALL is true, add NEWELT to history even if it is empty or
    !> duplicates the most recent entry in the history.  Port of
    !> subr.el:add-to-history (history-delete-duplicates nil).
    subroutine add_to_history(history_var, newelt, maxelt, keep_all)
        character(len=*), intent(in) :: history_var, newelt
        integer, intent(in), optional :: maxelt
        logical, intent(in), optional :: keep_all
        type(value_type) :: history, first, longer
        type(value_type), allocatable :: elems(:)
        type(status_type) :: status
        logical :: ok, all
        integer :: limit
        limit = history_length
        if (present(maxelt)) limit = maxelt
        all = .false.
        if (present(keep_all)) all = keep_all
        call symbol_value(sym(history_var), history, status)
        if (.not. status%ok()) history = nil()
        if (.not. all .and. len(newelt) == 0) return
        if (.not. all .and. list_length(history) > 0) then
            first = list_elt(history, 1_int64)
            if (stringp(first)) then
                if (xstring(first) == newelt .and. len(xstring(first)) == len(newelt)) return
            end if
        end if
        call cons(make_string(newelt), history, longer, ok)
        if (.not. ok) return
        if (limit <= 0) then
            longer = nil()
        else if (list_length(longer) > limit) then
            call list_elements(longer, elems)
            longer = make_list(elems(1:limit))
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
    !> Return the string of text matched by the previous search or regexp
    !> operation.  NUM specifies the number of the parenthesized
    !> sub-expression in the last regexp whose match to return.  Zero
    !> means the entire text matched by the whole regexp or whole string.
    !>
    !> The return value is nil (`matched` false, an empty string) if NUMth
    !> pair didn't match, or there were less than NUM pairs.
    !>
    !> STRING should be given if the last search was by `string-match' on
    !> STRING.  If STRING is nil, the current buffer should be the same
    !> buffer as the one in which the search/match was performed.  Port of
    !> subr.el:match-string.
    function match_string(num, string, matched) result(text)
        integer, intent(in) :: num
        character(len=*), intent(in), optional :: string
        logical, intent(out), optional :: matched
        character(len=:), allocatable :: text
        integer(int64) :: b, e
        type(status_type) :: status
        text = ""
        if (present(matched)) matched = .false.
        b = match_beginning(num, status)
        if (.not. status%ok() .or. b == 0) return
        e = match_end(num, status)
        if (present(matched)) matched = .true.
        if (present(string)) then
            text = string(string_index_to_offset(string, b) + 1:string_index_to_offset(string, e))
        else
            call buffer_substring(b, e, text, status)
        end if
    end function match_string

    !> Return string of text matched by last search, without text
    !> properties.  NUM specifies which parenthesized expression in the
    !> last regexp.  Value is nil if NUMth pair didn't match, or there
    !> were less than NUM pairs.  Zero means the entire text matched by
    !> the whole regexp or whole string.  STRING should be given if the
    !> last search was by `string-match' on STRING.  If STRING is nil, the
    !> current buffer should be the same buffer as the one in which the
    !> search/match was performed.  Port of
    !> subr.el:match-string-no-properties.
    function match_string_no_properties(num, string, matched) result(text)
        integer, intent(in) :: num
        character(len=*), intent(in), optional :: string
        logical, intent(out), optional :: matched
        character(len=:), allocatable :: text
        text = match_string(num, string, matched)
    end function match_string_no_properties

    !> Same as `string-match' except this function does not change the
    !> match data.  Port of subr.el:string-match-p.
    integer(int64) function string_match_p(regexp, string, status, start) result(idx)
        character(len=*), intent(in) :: regexp, string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        idx = string_match(regexp, string, status, start, inhibit_modify=.true.)
    end function string_match_p

    !> Same as `looking-at' except this function does not change the match
    !> data.  Port of subr.el:looking-at-p.
    logical function looking_at_p(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        looking_at_p = looking_at(regexp, status, inhibit_modify=.true.)
    end function looking_at_p

    !> Return non-nil if text before point matches regular expression
    !> REGEXP.  Like `looking-at' except matches before point, and is
    !> slower.  LIMIT if non-nil speeds up the search by specifying a
    !> minimum starting position, to avoid checking matches that would
    !> start before LIMIT.
    !>
    !> If GREEDY is non-nil, extend the match backwards as far as possible,
    !> stopping when a single additional previous character cannot be part
    !> of a match for REGEXP.  When the match is extended, its starting
    !> position is allowed to occur before LIMIT.
    !>
    !> As a general recommendation, try to avoid using `looking-back'
    !> wherever possible, since it is slow.  Port of subr.el:looking-back.
    logical function looking_back(regexp, status, limit, greedy) result(found)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        logical, intent(in), optional :: greedy
        integer(int64) :: start, pos, saved_begv, saved_zv
        logical :: ignored
        start = point()
        call re_search_backward("\(?:"//regexp//"\)\=", pos, status, bound=limit, noerror=SEARCH_NOERROR)
        found = status%ok() .and. pos > 0
        if (found) pos = point()
        call goto_char(start)
        if (.not. found) return
        if (.not. present(greedy)) return
        if (.not. greedy) return
        saved_begv = point_min()
        saved_zv = point_max()
        call narrow_to_region(point_min(), start, status)
        do while (pos > point_min())
            call goto_char(pos - 1)
            if (.not. looking_at("\(?:"//regexp//"\)\'", status)) exit
            pos = pos - 1
        end do
        call goto_char(pos)
        ignored = looking_at("\(?:"//regexp//"\)\'", status)
        call widen()
        call narrow_to_region(saved_begv, saved_zv, status)
        call goto_char(start)
    end function looking_back
end module subr_fm
