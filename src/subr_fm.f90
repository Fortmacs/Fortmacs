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

!> Questions in the echo area (`read_key', `y_or_n_p'),
!> `add_to_history', and the match data helpers (`match_string',
!> `string_match_p', `looking_at_p', `looking_back').
!>
!> Without a minibuffer yet, `y_or_n_p' behaves as upstream does with
!> `y_or_n_p_use_read_key' set: it reads a single key in the echo area.
module subr_fm
    use error_fm, only: status_type, set_error, FM_QUIT
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use lisp_fm, only: value_type, fixnump, xfixnum, nil, make_string, cons, list_length, list_elt, &
                       list_elements, make_list, stringp, xstring
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set
    use eval_fm, only: hook_type
    use keyboard_fm, only: read_event, discard_input
    use xdisp_fm, only: message, cursor_in_echo_area
    use keymap_fm, only: define_key, current_global_map
    use search_fm, only: string_match, looking_at, re_search_backward, match_beginning, match_end, &
                         match_in_string_p, string_index_to_offset, SEARCH_NOERROR, match_data, &
                         set_match_data, match_data_translate, replace_match
    use editfns_fm, only: buffer_substring, point, goto_char, point_min, point_max, narrow_to_region, widen
    implicit none(type, external)
    private

    public :: read_key, y_or_n_p, query_replace_answer, add_to_history
    public :: global_set_key, read_number
    public :: make_progress_reporter, progress_reporter_update, progress_reporter_done

    !> A numerical progress reporter (`make_progress_reporter' with
    !> MIN_VALUE and MAX_VALUE): the value at which to report next (the
    !> car of upstream's cons) and the parameters vector.
    type, public :: progress_reporter_type
        real(real64) :: next_value = 0
        !> The time of the next echo area update, in seconds.
        real(real64) :: update_time = 0
        integer(int64) :: min_value = 0, max_value = 0
        character(len=:), allocatable :: text
        integer :: min_change = 1
        real(real64) :: min_time = 0.2_real64
    end type progress_reporter_type
    public :: run_mode_hooks, delay_mode_hooks_begin, delay_mode_hooks_end, delaying_mode_hooks, &
              kill_delayed_mode_hooks

    !> Normal hook run in major mode functions, before the mode hooks.
    !> Port of subr.el change-major-mode-after-body-hook.
    type(hook_type), public, target :: change_major_mode_after_body_hook = &
                                       hook_type(name="change_major_mode_after_body_hook")

    !> Normal hook run at the very end of major mode functions.  Port of
    !> subr.el after-change-major-mode-hook.
    type(hook_type), public, target :: after_change_major_mode_hook = hook_type(name="after_change_major_mode_hook")
    public :: match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back, &
              match_substitute_replacement

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
              ANSWER_AUTOMATIC, ANSWER_RECENTER, ANSWER_HELP, ANSWER_QUIT, ANSWER_NONE, &
              ANSWER_EDIT_REPLACEMENT, ANSWER_EDIT_REPLACEMENT_EXACT_CASE, ANSWER_ACT_AND_SHOW, &
              ANSWER_EDIT, ANSWER_DELETE_AND_EDIT, ANSWER_BACKUP, ANSWER_UNDO, ANSWER_UNDO_ALL, &
              ANSWER_DIFF, ANSWER_SCROLL_UP, ANSWER_EXIT_PREFIX

    ! The `query-replace-map` bindings in use (replace.el).
    integer, parameter :: ANSWER_NONE = 0, ANSWER_ACT = 1, ANSWER_SKIP = 2, ANSWER_EXIT = 3, &
                          ANSWER_ACT_AND_EXIT = 4, ANSWER_AUTOMATIC = 5, ANSWER_RECENTER = 6, &
                          ANSWER_HELP = 7, ANSWER_QUIT = 8, ANSWER_EDIT_REPLACEMENT = 9, &
                          ANSWER_EDIT_REPLACEMENT_EXACT_CASE = 10, ANSWER_ACT_AND_SHOW = 11, &
                          ANSWER_EDIT = 12, ANSWER_DELETE_AND_EDIT = 13, ANSWER_BACKUP = 14, &
                          ANSWER_UNDO = 15, ANSWER_UNDO_ALL = 16, ANSWER_DIFF = 17, &
                          ANSWER_SCROLL_UP = 18, ANSWER_EXIT_PREFIX = 19

    interface
        !> Read from the minibuffer and return a numeric value, prompting with PROMPT.
        !> DEFAULT specifies a default value to return if the user just types RET.
        !> For historical reasons, the value of DEFAULT is always inserted into
        !> PROMPT, so it's recommended to use `format' instead of `format_prompt'
        !> to generate PROMPT.  HIST specifies a history list variable.  See
        !> `read_from_minibuffer' for details of the HIST argument.
        !>
        !> This function is used by the `interactive' code letter "n".  Port
        !> of subr.el:read-number (integers only).  (In subr_read_fm.)
        module subroutine read_number(prompt, n, status, default, hist)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer(int64), intent(out) :: n
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: default
            character(len=*), intent(in), optional :: hist
        end subroutine read_number

        !> Run mode hooks `delayed_mode_hooks' and HOOK, or delay HOOK.
        !>
        !> If the variable `delay_mode_hooks' is non-nil, does not do anything,
        !> just adds HOOK to the list `delayed_mode_hooks'.
        !> Otherwise, runs hooks in the sequence: `change_major_mode_after_body_hook',
        !> `delayed_mode_hooks' (in reverse order), HOOK, then
        !> runs the hook `after_change_major_mode_hook'.
        !>
        !> Major mode functions should use this instead of `run_hooks' when
        !> running their FOO-mode-hook.  Port of subr.el:run-mode-hooks
        !> (no file-local variables yet).  (In subr_hooks_fm.)
        recursive module subroutine run_mode_hooks(status, hook)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(hook_type), intent(in), target, optional :: hook
        end subroutine run_mode_hooks

        !> Start `delay_mode_hooks': until the matching
        !> delay_mode_hooks_end, `run_mode_hooks' only records its hooks
        !> (in the current buffer).  SAVED keeps the outer value.  Port of
        !> subr.el:delay-mode-hooks.  (In subr_hooks_fm.)
        module subroutine delay_mode_hooks_begin(saved)
            implicit none(type, external)
            logical, intent(out) :: saved
        end subroutine delay_mode_hooks_begin

        !> End `delay_mode_hooks', restoring the value SAVED.  (In
        !> subr_hooks_fm.)
        module subroutine delay_mode_hooks_end(saved)
            implicit none(type, external)
            logical, intent(in) :: saved
        end subroutine delay_mode_hooks_end

        !> Forget the current buffer's `delayed_mode_hooks' (a local
        !> variable that `kill_all_local_variables' kills).  (In
        !> subr_hooks_fm.)
        module subroutine kill_delayed_mode_hooks()
            implicit none(type, external)
        end subroutine kill_delayed_mode_hooks

        !> Whether mode hooks are being delayed in the current buffer (the
        !> variable `delay_mode_hooks').  (In subr_hooks_fm.)
        module logical function delaying_mode_hooks()
            implicit none(type, external)
        end function delaying_mode_hooks
    end interface

contains

    !> Read one key with `prompt' in the echo area and the cursor after it.
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

    !> What `key' means in replace.el:query-replace-map (the bindings
    !> y-or-n-p, map-y-or-n-p and perform-replace use), or ANSWER_NONE.
    !> ESC is `exit_prefix' (upstream binds [escape]; a terminal reads
    !> it as ESC).
    integer function query_replace_answer(key) result(answer)
        type(value_type), intent(in) :: key
        answer = ANSWER_NONE
        if (.not. fixnump(key)) return
        select case (xfixnum(key))
        case (iachar(" "), iachar("y"), iachar("Y"))
            answer = ANSWER_ACT
        case (127, iachar("n"), iachar("N"))
            answer = ANSWER_SKIP
        case (iachar("e"))
            answer = ANSWER_EDIT_REPLACEMENT
        case (iachar("E"))
            answer = ANSWER_EDIT_REPLACEMENT_EXACT_CASE
        case (iachar(","))
            answer = ANSWER_ACT_AND_SHOW
        case (iachar("q"), 13)
            answer = ANSWER_EXIT
        case (iachar("."))
            answer = ANSWER_ACT_AND_EXIT
        case (18)
            answer = ANSWER_EDIT
        case (23)
            answer = ANSWER_DELETE_AND_EDIT
        case (12)
            answer = ANSWER_RECENTER
        case (iachar("!"))
            answer = ANSWER_AUTOMATIC
        case (iachar("^"))
            answer = ANSWER_BACKUP
        case (iachar("u"))
            answer = ANSWER_UNDO
        case (iachar("U"))
            answer = ANSWER_UNDO_ALL
        case (iachar("d"))
            answer = ANSWER_DIFF
        case (8, iachar("?"))
            answer = ANSWER_HELP
        case (7, 29)
            answer = ANSWER_QUIT
        case (22)
            answer = ANSWER_SCROLL_UP
        case (27)
            answer = ANSWER_EXIT_PREFIX
        case default
            answer = ANSWER_NONE
        end select
    end function query_replace_answer

    !> Ask a "y or n" question in the echo area: `answer' is true for y
    !> (or SPC), false for n (or DEL); C-g quits.  Other keys ask again,
    !> with "Please answer y or n.".  Port of subr.el:y-or-n-p, the
    !> `y_or_n_p_use_read_key' branch (no help-form, scrolling or ding).
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
            case (ANSWER_QUIT, ANSWER_EXIT_PREFIX)
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
    !> `history_length' variable.
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
    !> This is a legacy function; see `keymap_global_set' for the
    !> recommended function to use instead.
    !>
    !> COMMAND is the command definition to use; usually it is
    !> a symbol naming an interactively-callable function.
    !> KEY is a key sequence: a vector of events, as `key_parse' makes.
    !>
    !> Note that if KEY has a local binding in the current buffer,
    !> that local binding will continue to shadow any global binding
    !> that you make with this function.  Port of subr.el:global-set-key.
    subroutine global_set_key_value(key, command, status)
        type(value_type), intent(in) :: key(:), command
        type(status_type), intent(out) :: status
        call define_key(current_global_map(), key, command, status)
    end subroutine global_set_key_value

    !> `global_set_key' with the command named COMMAND.
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
    !> The return value is nil (`matched' false, an empty string) if NUMth
    !> pair didn't match, or there were less than NUM pairs.
    !>
    !> STRING should be given if the last search was by `string_match' on
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
    !> last search was by `string_match' on STRING.  If STRING is nil, the
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

    !> Return REPLACEMENT as it will be inserted by `replace_match'.
    !> In other words, all back-references in the form `\&' and `\N'
    !> are substituted with actual strings matched by the last search.
    !> Optional FIXEDCASE, LITERAL, STRING and SUBEXP have the same
    !> meaning as for `replace_match'.  Port of
    !> subr.el:match-substitute-replacement.
    function match_substitute_replacement(replacement, status, fixedcase, literal, string, subexp) result(text)
        character(len=*), intent(in) :: replacement
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fixedcase, literal
        character(len=*), intent(in), optional :: string
        integer, intent(in), optional :: subexp
        character(len=:), allocatable :: text, match
        integer(int64), allocatable :: saved(:)
        integer(int64) :: start
        text = ""
        match = match_string(0, string)
        ! save-match-data
        saved = match_data()
        start = match_beginning(0, status)
        if (.not. status%ok()) return
        call match_data_translate(1 - start)
        call replace_match(replacement, status, fixedcase, literal, match, subexp, text)
        call set_match_data(saved)
    end function match_substitute_replacement

    !> Same as `string_match' except this function does not change the
    !> match data.  Port of subr.el:string-match-p.
    integer(int64) function string_match_p(regexp, string, status, start) result(idx)
        character(len=*), intent(in) :: regexp, string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        idx = string_match(regexp, string, status, start, inhibit_modify=.true.)
    end function string_match_p

    !> Same as `looking_at' except this function does not change the match
    !> data.  Port of subr.el:looking-at-p.
    logical function looking_at_p(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        looking_at_p = looking_at(regexp, status, inhibit_modify=.true.)
    end function looking_at_p

    !> Return non-nil if text before point matches regular expression
    !> REGEXP.  Like `looking_at' except matches before point, and is
    !> slower.  LIMIT if non-nil speeds up the search by specifying a
    !> minimum starting position, to avoid checking matches that would
    !> start before LIMIT.
    !>
    !> If GREEDY is non-nil, extend the match backwards as far as possible,
    !> stopping when a single additional previous character cannot be part
    !> of a match for REGEXP.  When the match is extended, its starting
    !> position is allowed to occur before LIMIT.
    !>
    !> As a general recommendation, try to avoid using `looking_back'
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

    !> Seconds since some fixed time (float-time, for time differences).
    real(real64) function float_time()
        integer(int64) :: count, rate
        call system_clock(count, rate)
        float_time = real(count, real64)/real(rate, real64)
    end function float_time

    !> Return progress reporter object for use with `progress_reporter_update'.
    !>
    !> MESSAGE is shown in the echo area, with a status indicator
    !> appended to the end.  When you call `progress_reporter_done', the
    !> word "done" is printed after the MESSAGE.
    !>
    !> MIN_VALUE and MAX_VALUE are starting (0% complete) and final (100%
    !> complete) states of operation; the latter should be larger.  The
    !> status message shows the percentage progress.  The minimal change
    !> to report is 1%, the minimum interval between echo area updates
    !> 0.2 seconds.  Port of subr.el:make-progress-reporter (numerical
    !> reporters, default CURRENT-VALUE, MIN-CHANGE and MIN-TIME).
    subroutine make_progress_reporter(reporter, message_text, min_value, max_value)
        type(progress_reporter_type), intent(out) :: reporter
        character(len=*), intent(in) :: message_text
        integer(int64), intent(in) :: min_value, max_value
        type(status_type) :: st
        reporter%text = message_text
        if (string_match("[[:alnum:]]\'", message_text, st) > 0) reporter%text = message_text//"..."
        reporter%next_value = real(min_value, real64)
        reporter%update_time = float_time()
        reporter%min_value = min_value
        reporter%max_value = max_value
        ! Force a call to `message' now.
        call progress_reporter_update(reporter, min_value)
    end subroutine make_progress_reporter

    !> Report progress of an operation in the echo area.
    !> VALUE should be a number between MIN_VALUE and MAX_VALUE.
    !>
    !> This function is relatively inexpensive.  If the change since
    !> last update is too small or insufficient time has passed, it does
    !> nothing.  Port of subr.el:progress-reporter-update (with
    !> progress-reporter-do-update and progress-reporter-echo-area).
    subroutine progress_reporter_update(reporter, value)
        type(progress_reporter_type), intent(inout) :: reporter
        integer(int64), intent(in) :: value
        real(real64) :: one_percent, now
        integer(int64) :: percentage
        logical :: enough_time_passed
        character(len=24) :: buf
        if (real(value, real64) < reporter%next_value) return
        ! See if enough time has passed since the last update.
        now = float_time()
        enough_time_passed = reporter%update_time < now
        ! Calculate time for the next update.
        if (enough_time_passed) reporter%update_time = reporter%update_time + reporter%min_time
        one_percent = real(reporter%max_value - reporter%min_value, real64)/100.0_real64
        if (reporter%max_value == reporter%min_value) then
            percentage = 0
        else
            percentage = int((value - reporter%min_value)/one_percent, int64)
        end if
        ! If we are not printing message because not enough time has
        ! passed, use 1 instead of MIN-CHANGE.
        if (enough_time_passed) then
            reporter%next_value = reporter%min_value + (percentage + reporter%min_change)*one_percent
        else
            reporter%next_value = reporter%min_value + (percentage + 1)*one_percent
        end if
        reporter%next_value = min(reporter%next_value, real(reporter%max_value, real64))
        reporter%next_value = real(ceiling(reporter%next_value, int64), real64)
        ! Print message only if enough time has passed.
        if (.not. enough_time_passed) return
        if (percentage > 0) then
            write (buf, "(i0)") percentage
            call message(reporter%text//trim(buf)//"%")
        else
            call message(reporter%text)
        end if
    end subroutine progress_reporter_update

    !> Print reporter's message followed by word "done" in echo area.
    !> Port of subr.el:progress-reporter-done.
    subroutine progress_reporter_done(reporter)
        type(progress_reporter_type), intent(in) :: reporter
        call message(reporter%text//"done")
    end subroutine progress_reporter_done
end module subr_fm
