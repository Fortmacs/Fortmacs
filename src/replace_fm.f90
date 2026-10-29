! replace_fm.f90 -- replace commands.
! Ported from GNU Emacs 31.1 lisp/replace.el.
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

!> Query replace: `query_replace' (M-%), `query_replace_regexp' (C-M-%),
!> `map_query_replace_regexp', `replace_string', `replace_regexp' and
!> their engine `perform_replace', with the `query_replace_map' answers
!> (act, skip, backup, undo, edit, ...).
!>
!> Differences from upstream, for want of Lisp and of display features:
!>
!> - REPLACEMENTS as a (FUNCTION . DATA) cons is a Fortran function of
!>   the replacement count (`replacement_function'); `\,' (a Lisp
!>   expression) in an interactive replacement is an error, while `\#'
!>   (the replacement count) works;
!> - the FROM and TO of a history entry are split at the separator text
!>   itself (upstream marks the separator with a text property);
!> - no highlighting (query-replace-highlight, lazy highlight), no
!>   invisible-text or isearch-filter-predicate skipping, no character
!>   folding, no noncontiguous regions, no multi-buffer answers and no
!>   `diff' answer yet; `occur' is a later step;
!> - of the keep-lines family only `flush_lines' as a function (no command).
module replace_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, FM_QUIT, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, make_string, make_list, stringp, xstring, nilp, list_elements, &
                       list_length, list_elt
    use lread_fm, only: sym
    use fns_fm, only: schars
    use data_fm, only: value_eq, symbol_value, set
    use buffer_fm, only: barf_if_buffer_read_only
    use marker_fm, only: make_marker, set_marker, marker_position, free_marker
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, buffer_substring, &
                          region_beginning, region_end
    use cmds_fm, only: forward_char
    use textprop_fm, only: text_property_not_all
    use undo_fm, only: undo_boundary
    use search_fm, only: case_fold_search, search_spaces_regexp, re_search_forward, re_search_backward, &
                         looking_at, match_beginning, match_end, match_data, set_match_data, replace_match, &
                         regexp_quote, string_match, SEARCH_NOERROR
    use subr_fm, only: read_key, add_to_history, defvar_nil, history_length, query_replace_answer, match_string, &
                       looking_back, match_substitute_replacement, find_tag_default, find_tag_default_as_regexp, &
                       find_tag_default_as_symbol_regexp, &
                       ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, ANSWER_ACT_AND_EXIT, ANSWER_AUTOMATIC, &
                       ANSWER_RECENTER, ANSWER_HELP, ANSWER_QUIT, ANSWER_EDIT_REPLACEMENT, &
                       ANSWER_EDIT_REPLACEMENT_EXACT_CASE, ANSWER_ACT_AND_SHOW, ANSWER_EDIT, &
                       ANSWER_DELETE_AND_EDIT, ANSWER_BACKUP, ANSWER_UNDO, ANSWER_UNDO_ALL, ANSWER_DIFF, &
                       ANSWER_SCROLL_UP, ANSWER_SCROLL_DOWN
    use isearch_fm, only: search_upper_case, UPPER_CASE_NIL, search_whitespace_regexp, &
                          isearch_no_upper_case_p, word_search_regexp
    use minibuf_fm, only: read_from_minibuffer, read_string, history_add_new_input, minibuffer_string_type
    use keyboard_fm, only: this_command, unread_command_events, sit_for, recursive_edit_1
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use simple_fm, only: push_mark, use_region_p, deactivate_mark
    use window_fm, only: recenter_top_bottom, scroll_up, scroll_down
    use help_fm, only: with_help_window
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use faces_fm, only: named_face_type, display_when, face_attrs, init_faces, FACE_CLASS_COLOR, FACE_CLASS_MONO, &
                        FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    implicit none(type, external)
    private

    public :: perform_replace, replace_search, replace_match_maybe_edit, replace_quote, flush_lines
    public :: query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, replace_regexp
    public :: query_replace_descr, query_replace_read_from, query_replace_read_to, query_replace_read_args, &
              query_replace_compile_replacement, replace_expand_count, syms_of_replace
    public :: replacement_function, read_regexp, read_regexp_suggestions, query_replace_read_from_suggestions, &
              format_prompt

    !> Face used to highlight matches permanently.  Port of replace.el
    !> match.
    type(named_face_type), target, public :: match

    !> A FROM_STRING and TO_STRING pair of `query_replace_defaults'.
    type, public :: replace_pair_type
        character(len=:), allocatable :: from, to
    end type replace_pair_type

    abstract interface
        !> A function computing each replacement string from the number of
        !> replacements done so far: the (FUNCTION . DATA) form of
        !> `perform_replace''s REPLACEMENTS.
        function replacement_function(count) result(text)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: count
            character(len=:), allocatable :: text
        end function replacement_function
    end interface

    !> True means `query_replace' should preserve case in replacements.
    !> Port of replace.el case-replace.
    logical, public :: case_replace = .true.

    !> True means `query_replace' matches a sequence of whitespace chars.
    !> When you enter a space or spaces in the strings to be replaced,
    !> it will match any sequence matched by the regexp
    !> `search_whitespace_regexp'.  Port of replace.el
    !> replace-lax-whitespace.
    logical, public :: replace_lax_whitespace = .false.

    !> True means `query_replace_regexp' matches a sequence of whitespace
    !> chars.  When you enter a space or spaces in the regexps to be
    !> replaced, it will match any sequence matched by the regexp
    !> `search_whitespace_regexp'.  Port of replace.el
    !> replace-regexp-lax-whitespace.
    logical, public :: replace_regexp_lax_whitespace = .false.

    !> Default values of FROM_STRING and TO_STRING for `query_replace'.
    !> This is an array of FROM_STRING and TO_STRING pairs, or unallocated
    !> if there are no default values.  Port of replace.el
    !> query-replace-defaults (most recent first).
    type(replace_pair_type), allocatable, public :: query_replace_defaults(:)

    !> String that separates FROM and TO in the history of replacement
    !> pairs.  When unallocated, the pair will not be added to the
    !> history (same behavior as in Emacs 24.5).  Port of replace.el
    !> query-replace-from-to-separator.
    character(len=:), allocatable, public :: query_replace_from_to_separator

    !> History list to use for the FROM argument of `query_replace'
    !> commands.  The value of this variable should be the name of
    !> a variable that holds a history list for the strings
    !> or patterns to be replaced.  Port of replace.el
    !> query-replace-from-history-variable.
    character(len=:), allocatable, public :: query_replace_from_history_variable

    !> History list to use for the TO argument of `query_replace'
    !> commands.  The value of this variable should be the name of
    !> a variable that holds a history list for replacement
    !> strings or patterns.  Port of replace.el
    !> query-replace-to-history-variable.
    character(len=:), allocatable, public :: query_replace_to_history_variable

    !> True means `query_replace' and friends ignore read-only matches.
    !> Port of replace.el query-replace-skip-read-only.
    logical, public :: query_replace_skip_read_only = .false.

    !> True means show substituted replacement text in the minibuffer.
    !> This variable affects only `query_replace_regexp'.  Port of
    !> replace.el query-replace-show-replacement.
    logical, public :: query_replace_show_replacement = .true.

    !> Number of replacements done so far.
    !> See `replace_regexp'.  Port of replace.el replace-count.
    integer(int64), public :: replace_count = 0

    character(len=*), parameter :: LF = achar(10)

    !> Help message while in `query_replace'.  Port of replace.el
    !> query-replace-help (keys substituted; no multi-buffer answers).
    character(len=*), parameter :: query_replace_help = &
                                   "Type SPC or y to replace one match, Delete or n to skip to next,"//LF// &
                                   "RET or q to exit, Period to replace one match and exit,"//LF// &
                                   ", to replace but not move point immediately,"//LF// &
                                   "! to replace all remaining matches in this buffer with no more questions,"//LF// &
                                   "C-r to enter recursive edit (C-M-c to get out again),"//LF// &
                                   "C-w to delete match and then enter recursive edit,"//LF// &
                                   "^ to move point back to previous match,"//LF// &
                                   "u to undo previous replacement,"//LF// &
                                   "U to undo all replacements,"//LF// &
                                   "e to edit the replacement string."//LF// &
                                   "E to edit the replacement string with exact case."//LF// &
                                   "C-l to clear the screen, redisplay, and offer same replacement again,"//LF// &
                                   "Any other character exits the interactive replacement loop, and is then"//LF// &
                                   "re-executed as a normal key sequence."

    ! The values of perform-replace's match-again: nil, t (a match is
    ! possible at point) and match data.
    integer, parameter :: AGAIN_NIL = 0, AGAIN_T = 1, AGAIN_DATA = 2

    ! An element of perform-replace's stack, (POINT REPLACED MATCH-DATA
    ! SEARCH-STR NEXT-REPLACE NEXT-REPLACEMENT MATCH-AGAIN), as
    ! replace--push-stack makes it.
    type :: stack_entry_type
        integer(int64) :: point = 0
        logical :: replaced = .false.
        integer(int64), allocatable :: data(:)
        character(len=:), allocatable :: search_str, next_replace, next_replacement
        integer :: again = AGAIN_NIL
        integer(int64), allocatable :: again_match(:)
    end type stack_entry_type

contains

    !> The decimal representation of `n'.
    function number_to_string(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function number_to_string

    !> STRING with control characters shown as ^C and DEL as ^?, as the
    !> prompts display it.  Port of replace.el:query-replace-descr (which
    !> uses display properties).
    function query_replace_descr(string) result(descr)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: descr
        integer :: i, c
        descr = ""
        do i = 1, len(string)
            c = iachar(string(i:i))
            if (c < 32) then
                descr = descr//"^"//achar(c + 64)
            else if (c == 127) then
                descr = descr//"^?"
            else
                descr = descr//string(i:i)
            end if
        end do
    end function query_replace_descr

    !> Quote a replacement string.
    !> This just doubles all backslashes in REPLACEMENT and
    !> returns the resulting string.  Port of replace.el:replace-quote
    !> (REPLACEMENT is a string).
    pure function replace_quote(replacement) result(quoted)
        character(len=*), intent(in) :: replacement
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = ""
        do i = 1, len(replacement)
            if (replacement(i:i) == "\") then
                quoted = quoted//"\\"
            else
                quoted = quoted//replacement(i:i)
            end if
        end do
    end function replace_quote

    !> The position of the first backslash of `string' at or after
    !> `from', not itself escaped, that is followed by one of `chars'; or
    !> 0.  (What the regexp \(\`\|[^\]\)\(\\\\\)*\\[CHARS] finds.)
    integer function find_directive(string, from, chars) result(at)
        character(len=*), intent(in) :: string, chars
        integer, intent(in) :: from
        integer :: i
        at = 0
        i = from
        do while (i < len(string))
            if (string(i:i) == "\") then
                if (index(chars, string(i + 1:i + 1)) > 0) then
                    at = i
                    return
                end if
                ! Skip the escaped character.
                i = i + 2
            else
                i = i + 1
            end if
        end do
    end function find_directive

    !> Maybe convert a regexp replacement TO for `perform_replace'.
    !> REGEXP-FLAG non-nil means TO is a regexp.  `counted' is true when TO
    !> contains `\#', which stands for the number of replacements done so
    !> far (`replace_expand_count' substitutes it).  `\,' (a Lisp
    !> expression) is an error: there is no Lisp.  Port of
    !> replace.el:query-replace-compile-replacement.
    subroutine query_replace_compile_replacement(to, regexp_flag, counted, status)
        character(len=*), intent(in) :: to
        logical, intent(in) :: regexp_flag
        logical, intent(out) :: counted
        type(status_type), intent(out) :: status
        integer :: at
        status = status_type()
        counted = .false.
        if (.not. regexp_flag) return
        at = find_directive(to, 1, ",#")
        do while (at > 0)
            if (to(at + 1:at + 1) == ",") then
                call set_error(status, FM_USER_ERROR, "\, in a replacement is not applicable in Fortmacs")
                return
            end if
            counted = .true.
            at = find_directive(to, at + 2, ",#")
        end do
    end subroutine query_replace_compile_replacement

    !> TO with each `\#' replaced by COUNT, as the compiled replacement
    !> (concat ... (number-to-string replace-count) ...) computes it.
    function replace_expand_count(to, count) result(text)
        character(len=*), intent(in) :: to
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: text
        integer :: at, from
        text = ""
        from = 1
        at = find_directive(to, 1, "#")
        do while (at > 0)
            text = text//to(from:at - 1)//number_to_string(count)
            from = at + 2
            at = find_directive(to, from, "#")
        end do
        text = text//to(from:)
    end function replace_expand_count

    !> Prompt with " (default DEFAULT)" if DEFAULT is given, then ": ".
    !> Port of minibuffer.el:format-prompt.
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: full
        full = prompt
        if (present(default)) full = full//" (default "//default//")"
        full = full//": "
    end function format_prompt

    !> The first string of the list in variable `name', if any.
    subroutine history_head(name, head, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: head
        logical, intent(out) :: found
        type(value_type) :: list, first
        type(status_type) :: status
        head = ""
        found = .false.
        call symbol_value(sym(name), list, status)
        if (.not. status%ok()) return
        if (list_length(list) == 0) return
        first = list_elt(list, 1_int64)
        if (.not. stringp(first)) return
        head = xstring(first)
        found = .true.
    end subroutine history_head

    !> Add (FROM . TO) to `query_replace_defaults'.  Port of the
    !> (add-to-history 'query-replace-defaults (cons from to) nil t)
    !> calls.
    subroutine push_defaults(from, to)
        character(len=*), intent(in) :: from, to
        type(replace_pair_type), allocatable :: longer(:)
        integer :: n, k
        n = 0
        if (allocated(query_replace_defaults)) n = size(query_replace_defaults)
        allocate (longer(min(n + 1, max(history_length, 0))))
        if (size(longer) == 0) then
            call move_alloc(longer, query_replace_defaults)
            return
        end if
        longer(1)%from = from
        longer(1)%to = to
        do k = 2, size(longer)
            longer(k) = query_replace_defaults(k - 1)
        end do
        call move_alloc(longer, query_replace_defaults)
    end subroutine push_defaults

    !> Append `s' to the list `list' (allocated, `n' used) unless absent.
    subroutine push_string(list, n, s)
        type(minibuffer_string_type), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: s
        type(minibuffer_string_type), allocatable :: grown(:)
        integer :: k
        if (n == size(list)) then
            allocate (grown(max(4, 2*n)))
            do k = 1, n
                call move_alloc(list(k)%s, grown(k)%s)
            end do
            call move_alloc(grown, list)
        end if
        n = n + 1
        list(n)%s = s
    end subroutine push_string

    !> The region's text, if `use_region_p'.
    subroutine region_text(text, found)
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        integer(int64) :: b, e
        type(status_type) :: st
        found = use_region_p()
        text = ""
        if (.not. found) return
        call region_beginning(b, st)
        call region_end(e, st)
        call buffer_substring(b, e, text, st)
    end subroutine region_text

    !> Return a list of standard suggestions for `query_replace_read_from'.
    !> By default, the list includes the active region, the identifier
    !> (a.k.a. "tag") at point, the last isearch string, and the last
    !> replacement regexp.  `query_replace_read_from' appends the list
    !> returned by this function to the end of values available via M-n.
    !> Port of replace.el:query-replace-read-from-suggestions.
    subroutine query_replace_read_from_suggestions(list, n)
        type(minibuffer_string_type), allocatable, intent(out) :: list(:)
        integer, intent(out) :: n
        character(len=:), allocatable :: s
        logical :: found
        allocate (list(4))
        n = 0
        call region_text(s, found)
        if (found) call push_string(list, n, s)
        call find_tag_default(s, found)
        if (found) call push_string(list, n, s)
        call history_head("search_ring", s, found)
        if (found) call push_string(list, n, s)
        call history_head(query_replace_from_history_variable, s, found)
        if (found) call push_string(list, n, s)
    end subroutine query_replace_read_from_suggestions

    !> Return a list of standard suggestions for `read_regexp'.
    !> By default, the list includes the active region, the identifier
    !> (a.k.a. "tag") at point, the last isearch regexp, the last isearch
    !> string, and the last replacement regexp.  `read_regexp' appends the
    !> list returned by this function to the end of values available via
    !> M-n.  Port of replace.el:read-regexp-suggestions (nil elements are
    !> left out, as read-regexp drops them).
    subroutine read_regexp_suggestions(list, n)
        type(minibuffer_string_type), allocatable, intent(out) :: list(:)
        integer, intent(out) :: n
        character(len=:), allocatable :: s
        logical :: found
        allocate (list(6))
        n = 0
        call region_text(s, found)
        if (found) call push_string(list, n, s)
        call find_tag_default_as_regexp(s, found)
        if (found) call push_string(list, n, s)
        call find_tag_default_as_symbol_regexp(s, found)
        if (found) call push_string(list, n, s)
        call history_head("regexp_search_ring", s, found)
        if (found) call push_string(list, n, s)
        call history_head("search_ring", s, found)
        call push_string(list, n, regexp_quote(s))
        call history_head(query_replace_from_history_variable, s, found)
        if (found) call push_string(list, n, s)
    end subroutine read_regexp_suggestions

    !> Read and return a regular expression as a string.
    !> Prompt with the string PROMPT.  If PROMPT ends in ":" (followed by
    !> optional whitespace), use it as is.  Otherwise, add ": " to the end,
    !> possibly preceded by the default result (see below).
    !>
    !> The optional argument DEFAULTS is a list of default values: the
    !> first is the default when the user enters empty input, and the
    !> rest come before `read_regexp_suggestions' in the values M-n
    !> offers.
    !>
    !> The optional argument HISTORY is a symbol to use for the history list.
    !> If nil, use `regexp_history'.  Port of replace.el:read-regexp
    !> (DEFAULTS a list of strings; no read-regexp-defaults-function,
    !> read-regexp-map or case-fold toggle).
    recursive subroutine read_regexp(prompt, result, status, defaults, history)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), intent(in), optional :: defaults(:)
        character(len=*), intent(in), optional :: history
        type(minibuffer_string_type), allocatable :: more(:), suggestions(:)
        character(len=:), allocatable :: full, input, histvar
        integer :: n, nmore, k, j
        logical :: saved_add, have_default, dup
        histvar = "regexp_history"
        if (present(history)) histvar = history
        have_default = .false.
        if (present(defaults)) have_default = size(defaults) > 0
        call read_regexp_suggestions(more, nmore)
        ! (delete-dups (delq nil (delete "" suggestions)))
        allocate (suggestions(4))
        n = 0
        if (present(defaults)) then
            do k = 1, size(defaults)
                call add_suggestion(defaults(k)%s)
            end do
        end if
        do k = 1, nmore
            call add_suggestion(more(k)%s)
        end do
        ! The prompt
        full = prompt
        if (.not. ends_in_colon(prompt)) then
            full = prompt
            if (have_default) then
                if (len(defaults(1)%s) > 0) full = full//" (default "//query_replace_descr(defaults(1)%s)//")"
            end if
            full = full//": "
        end if
        ! Do not automatically add default to the history for empty input.
        saved_add = history_add_new_input
        history_add_new_input = .false.
        call read_from_minibuffer(full, input, status, history=histvar, defaults=suggestions(1:n))
        history_add_new_input = saved_add
        if (.not. status%ok()) return
        ! Return the default value when the user enters empty input.
        result = input
        if (len(input) == 0) then
            if (.not. have_default) return
            result = defaults(1)%s
        end if
        call add_to_history(histvar, result)
    contains

        subroutine add_suggestion(s)
            character(len=*), intent(in) :: s
            if (len(s) == 0) return
            dup = .false.
            do j = 1, n
                if (suggestions(j)%s == s .and. len(suggestions(j)%s) == len(s)) dup = .true.
            end do
            if (.not. dup) call push_string(suggestions, n, s)
        end subroutine add_suggestion

        ! (string-match-p ":[ \t]*\\'" prompt)
        logical function ends_in_colon(p)
            character(len=*), intent(in) :: p
            integer :: i
            ends_in_colon = .false.
            do i = len(p), 1, -1
                if (p(i:i) == " " .or. p(i:i) == achar(9)) cycle
                ends_in_colon = p(i:i) == ":"
                return
            end do
        end function ends_in_colon
    end subroutine read_regexp

    !> Query and return the FROM argument of a `query_replace' operation.
    !> Prompt with PROMPT.  REGEXP-FLAG non-nil means the response should
    !> be a regexp.  The return value can also be a pair (FROM . TO)
    !> indicating that the user wants to replace FROM with TO: then
    !> `have_to' is true, and `counted' says whether TO uses `\#'.  Port
    !> of replace.el:query-replace-read-from.
    recursive subroutine query_replace_read_from(prompt, regexp_flag, from, to, have_to, counted, status)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: regexp_flag
        character(len=:), allocatable, intent(out) :: from, to
        logical, intent(out) :: have_to, counted
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: full_prompt
        type(value_type) :: saved_history, from_history
        type(value_type), allocatable :: older(:), entries(:)
        type(minibuffer_string_type), allocatable :: suggestions(:)
        type(status_type) :: st
        integer(int64) :: saved_point
        logical :: saved_add, have_separator
        integer :: k, n, at, nsuggestions
        to = ""
        have_to = .false.
        counted = .false.
        have_separator = allocated(query_replace_from_to_separator)
        ! The minibuffer history: the default pairs, then the FROM history.
        call symbol_value(sym("minibuffer_history"), saved_history, st)
        if (.not. st%ok()) saved_history = nil()
        call symbol_value(sym(query_replace_from_history_variable), from_history, st)
        if (.not. st%ok()) from_history = nil()
        call list_elements(from_history, older)
        n = 0
        if (have_separator .and. allocated(query_replace_defaults)) n = size(query_replace_defaults)
        allocate (entries(n + size(older)))
        do k = 1, n
            entries(k) = make_string(query_replace_defaults(k)%from//query_replace_from_to_separator// &
                                     query_replace_defaults(k)%to)
        end do
        if (size(older) > 0) entries(n + 1:) = older
        call set(sym("minibuffer_history"), make_list(entries), st)
        if (n > 0) then
            full_prompt = format_prompt(prompt, query_replace_descr(query_replace_defaults(1)%from)// &
                                        query_replace_from_to_separator// &
                                        query_replace_descr(query_replace_defaults(1)%to))
        else if (allocated(query_replace_defaults)) then
            if (size(query_replace_defaults) > 0) then
                full_prompt = format_prompt(prompt, query_replace_descr(query_replace_defaults(1)%from)//" -> "// &
                                            query_replace_descr(query_replace_defaults(1)%to))
            else
                full_prompt = format_prompt(prompt)
            end if
        else
            full_prompt = format_prompt(prompt)
        end if
        ! The save-excursion here is in case the user marks and copies a
        ! region in order to specify the minibuffer input.
        saved_point = point()
        if (regexp_flag) then
            call read_regexp(full_prompt, from, status, history="minibuffer_history")
        else
            call query_replace_read_from_suggestions(suggestions, nsuggestions)
            saved_add = history_add_new_input
            history_add_new_input = .false.
            call read_from_minibuffer(full_prompt, from, status, history="minibuffer_history", &
                                      defaults=suggestions(1:nsuggestions))
            history_add_new_input = saved_add
        end if
        call goto_char(saved_point)
        call set(sym("minibuffer_history"), saved_history, st)
        if (.not. status%ok()) return
        if (len(from) == 0 .and. allocated(query_replace_defaults)) then
            if (size(query_replace_defaults) > 0) then
                to = query_replace_defaults(1)%to
                from = query_replace_defaults(1)%from
                have_to = .true.
                call query_replace_compile_replacement(to, regexp_flag, counted, status)
                return
            end if
        end if
        if (have_separator .and. len(query_replace_from_to_separator) > 0) then
            at = index(from, query_replace_from_to_separator)
            if (at > 0) then
                to = from(at + len(query_replace_from_to_separator):)
                from = from(1:at - 1)
                have_to = .true.
            end if
        end if
        call add_to_history(query_replace_from_history_variable, from, keep_all=.true.)
        ! Warn if user types \n or \t, but don't reject the input.
        if (regexp_flag) then
            at = find_directive(from, 1, "nt")
            if (at > 0) then
                if (from(at + 1:at + 1) == "n") then
                    call message(format_message("Note: `\n' here doesn't match a newline; to do that, type C-q C-j instead"))
                else
                    call message(format_message("Note: `\t' here doesn't match a tab; to do that, just type TAB"))
                end if
                call sit_for(2000)
            end if
        end if
        if (.not. have_to) return
        call add_to_history(query_replace_to_history_variable, to, keep_all=.true.)
        call push_defaults(from, to)
        call query_replace_compile_replacement(to, regexp_flag, counted, status)
    end subroutine query_replace_read_from

    !> Query and return the TO argument of a `query_replace' operation.
    !> Prompt with PROMPT.  REGEXP-FLAG non-nil means the response
    !> should a regexp.  `counted' says whether TO uses `\#'.  Port of
    !> replace.el:query-replace-read-to.
    recursive subroutine query_replace_read_to(from, prompt, regexp_flag, to, counted, status)
        character(len=*), intent(in) :: from, prompt
        logical, intent(in) :: regexp_flag
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: counted
        type(status_type), intent(out) :: status
        logical :: saved_add
        counted = .false.
        saved_add = history_add_new_input
        history_add_new_input = .false.
        call read_from_minibuffer(prompt//" "//query_replace_descr(from)//" with: ", to, status, &
                                  history=query_replace_to_history_variable, default=from)
        history_add_new_input = saved_add
        if (.not. status%ok()) return
        call add_to_history(query_replace_to_history_variable, to, keep_all=.true.)
        call push_defaults(from, to)
        call query_replace_compile_replacement(to, regexp_flag, counted, status)
    end subroutine query_replace_read_to

    !> Read the arguments of a query-replace command: FROM, TO, whether to
    !> replace whole words only (DELIMITED: a prefix argument other than
    !> `-') and whether to go BACKWARD (the prefix argument `-').  Unless
    !> NOERROR, a read-only buffer is an error.  Port of
    !> replace.el:query-replace-read-args.
    recursive subroutine query_replace_read_args(prompt, regexp_flag, from, to, delimited, backward, counted, &
                                                 status, noerror)
        character(len=*), intent(in) :: prompt
        logical, intent(in) :: regexp_flag
        character(len=:), allocatable, intent(out) :: from, to
        logical, intent(out) :: delimited, backward, counted
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        logical :: have_to
        delimited = .false.
        backward = .false.
        counted = .false.
        to = ""
        from = ""
        status = status_type()
        if (.not. present_true(noerror)) then
            call barf_if_buffer_read_only(status)
            if (.not. status%ok()) return
        end if
        delimited = .not. nilp(current_prefix_arg) .and. .not. value_eq(current_prefix_arg, sym("-"))
        backward = value_eq(current_prefix_arg, sym("-"))
        call query_replace_read_from(prompt, regexp_flag, from, to, have_to, counted, status)
        if (.not. status%ok()) return
        if (.not. have_to) call query_replace_read_to(from, prompt, regexp_flag, to, counted, status)
    end subroutine query_replace_read_args

    logical function present_true(x)
        logical, intent(in), optional :: x
        present_true = .false.
        if (present(x)) present_true = x
    end function present_true

    !> Search for the next occurrence of SEARCH-STRING to replace.  Port
    !> of replace.el:replace-search with isearch-search-fun-default
    !> (non-incremental: no lax word search).
    logical function replace_search(search_string, limit, regexp_flag, delimited_flag, case_fold, status, &
                                    backward) result(found)
        character(len=*), intent(in) :: search_string
        integer(int64), intent(in) :: limit
        logical, intent(in) :: regexp_flag, delimited_flag, case_fold
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: regexp, saved_spaces
        integer(int64) :: pos
        logical :: had_spaces, saved_fold
        if (delimited_flag) then
            regexp = word_search_regexp(search_string, .false.)
        else if (regexp_flag) then
            regexp = search_string
        else
            regexp = regexp_quote(search_string)
        end if
        had_spaces = allocated(search_spaces_regexp)
        if (had_spaces) saved_spaces = search_spaces_regexp
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        if (merge(replace_regexp_lax_whitespace, replace_lax_whitespace, regexp_flag) .and. &
            allocated(search_whitespace_regexp)) search_spaces_regexp = search_whitespace_regexp
        saved_fold = case_fold_search%value
        case_fold_search = case_fold
        if (present_true(backward)) then
            if (limit > 0) then
                call re_search_backward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
            else
                call re_search_backward(regexp, pos, status, noerror=SEARCH_NOERROR)
            end if
        else
            if (limit > 0) then
                call re_search_forward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(regexp, pos, status, noerror=SEARCH_NOERROR)
            end if
        end if
        case_fold_search = saved_fold
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        if (had_spaces) search_spaces_regexp = saved_spaces
        found = status%ok() .and. pos > 0
    end function replace_search

    !> Make a replacement with `replace_match', editing `\?'.
    !> FIXEDCASE, LITERAL are passed to `replace_match' (which see).
    !> After possibly editing it (if `\?' is present), NEWTEXT is also
    !> passed to `replace_match'.  If NOEDIT is true, no check for `\?'
    !> is made (to save time).
    !> MATCH-DATA is used for the replacement, and is a data structure
    !> as returned from the `match_data' function.  BACKWARD is
    !> used to reverse the replacement direction.
    !>
    !> The return value is non-nil if there has been no `\?' or NOEDIT was
    !> passed in.  If LITERAL is set, no checking is done, anyway.  Port
    !> of replace.el:replace-match-maybe-edit (the edited text is read
    !> with point at its end).
    recursive logical function replace_match_maybe_edit(newtext, fixedcase, literal, noedit, data, status, &
                                                        backward) result(unedited)
        character(len=*), intent(in) :: newtext
        logical, intent(in) :: fixedcase, literal, noedit
        integer(int64), intent(in) :: data(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: text, edited
        integer :: at
        unedited = noedit
        text = newtext
        status = status_type()
        if (.not. (literal .or. noedit)) then
            unedited = .true.
            at = find_directive(text, 1, "?")
            do while (at > 0)
                call read_string("Edit replacement string: ", edited, status, &
                                 initial=text(1:at - 1)//text(at + 2:), position=1 + schars(text(1:at - 1)))
                if (.not. status%ok()) return
                text = edited
                unedited = .false.
                at = find_directive(text, 1, "?")
            end do
        end if
        call set_match_data(data)
        call replace_match(text, status, fixedcase, literal)
        if (.not. status%ok()) return
        ! `replace_match' leaves point at the end of the replacement text,
        ! so move point to the beginning when replacing backward.
        if (present_true(backward)) call goto_char(data(1))
    end function replace_match_maybe_edit

    !> Push an element on perform-replace's stack.  Port of
    !> replace.el:replace--push-stack.
    subroutine push_stack(stack, nstack, replaced, search_str, next_replace, next_replacement, again, again_match)
        type(stack_entry_type), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: nstack
        logical, intent(in) :: replaced
        character(len=:), allocatable, intent(in) :: search_str, next_replace
        character(len=*), intent(in) :: next_replacement
        integer, intent(in) :: again
        integer(int64), intent(in) :: again_match(:)
        type(stack_entry_type), allocatable :: grown(:)
        type(status_type) :: status
        integer :: k
        if (.not. allocated(stack)) allocate (stack(16))
        if (nstack == size(stack)) then
            allocate (grown(2*size(stack)))
            do k = 1, nstack
                grown(k) = stack(k)
            end do
            call move_alloc(grown, stack)
        end if
        nstack = nstack + 1
        stack(nstack)%point = point()
        stack(nstack)%replaced = replaced
        if (replaced) then
            ! If the replacement has already happened, all we need is the
            ! current match start and end.
            stack(nstack)%data = [match_beginning(0, status), match_end(0, status)]
        else
            stack(nstack)%data = match_data()
        end if
        stack(nstack)%search_str = ""
        if (allocated(search_str)) stack(nstack)%search_str = search_str
        stack(nstack)%next_replace = ""
        if (allocated(next_replace)) stack(nstack)%next_replace = next_replace
        stack(nstack)%next_replacement = next_replacement
        stack(nstack)%again = again
        stack(nstack)%again_match = again_match
    end subroutine push_stack

    !> The match data after `looking_at(regexp)' at `pos', point left
    !> where it was (save-excursion).
    function match_data_at(pos, regexp) result(data)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: regexp
        integer(int64), allocatable :: data(:)
        integer(int64) :: opoint
        type(status_type) :: status
        logical :: matched
        opoint = point()
        call goto_char(pos)
        matched = looking_at(regexp, status)
        data = match_data()
        call goto_char(opoint)
    end function match_data_at

    !> Subroutine of `query_replace'.  Its complexity handles interactive
    !> queries.  Don't use this in your own program unless you want to
    !> query and set the mark just as `query_replace' does.  Instead,
    !> write a simple loop like this:
    !>
    !>   do
    !>       call re_search_forward("foo[ \t]+bar", found, status, noerror=SEARCH_NOERROR)
    !>       if (found == 0) exit
    !>       call replace_match("foobar", status)
    !>   end do
    !>
    !> which will run faster and probably do exactly what you want.  Please
    !> see the documentation of `replace_match' to find out how to simulate
    !> `case_replace'.
    !>
    !> This function returns nil if there were no matches to make, or
    !> the user canceled the call.
    !>
    !> REPLACEMENTS is either a string, a list of strings, or a cons cell
    !> containing a function and its first argument.  The function is
    !> called to generate each replacement like this:
    !>   (funcall (car replacements) (cdr replacements) replace-count)
    !> It must return a string.
    !>
    !> In Fortmacs, REPLACEMENTS is the string `replacements', unless
    !> `replacement_list' (a list of strings, each used REPEAT-COUNT times
    !> in turn) or `replacement_fn' (called with the replacement count) is
    !> given; `counted' means the string uses `\#' (see
    !> `query_replace_compile_replacement').  START and END give the
    !> region to operate on.  `result' is the return value.  Port of
    !> replace.el:perform-replace (no MAP, no noncontiguous regions).
    recursive subroutine perform_replace(from_string, replacements, query_flag, regexp_flag, delimited_flag, &
                                         status, repeat_count, start, end, backward, replacement_list, &
                                         replacement_fn, counted, result)
        character(len=*), intent(in) :: from_string, replacements
        logical, intent(in) :: query_flag, regexp_flag, delimited_flag
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: repeat_count, start, end
        logical, intent(in), optional :: backward, counted
        type(value_type), intent(in), optional :: replacement_list
        procedure(replacement_function), optional :: replacement_fn
        logical, intent(out), optional :: result
        type(stack_entry_type), allocatable :: stack(:)
        type(stack_entry_type) :: elt
        type(value_type), allocatable :: items(:)
        type(value_type) :: key
        type(handle_type) :: limit_marker, opos
        type(status_type) :: st
        integer(int64), allocatable :: real_data(:), again_match(:)
        integer(int64) :: limit, opoint, skip_read_only_count, rotate_count, repeat, ro_pos
        character(len=:), allocatable :: next_replacement, search_string_replaced, next_replacement_replaced, &
                                         presentation, msg_prefix, last_replacement, undo_search, help_text, &
                                         edited
        logical :: fold, saved_fold, nocasify, literal, noedit, keep_going, last_was_undo, &
                   last_was_act_and_show, update_stack, nonempty_match, have_limit, back, found, done, &
                   replaced, querying, use_items, use_fn, is_counted, unread, matched, ro_found
        integer :: again, def, nstack, item, stack_idx, stack_len, num_replacements
        status = status_type()
        back = present_true(backward)
        is_counted = present_true(counted)
        querying = query_flag
        if (present(result)) result = .false.
        fold = case_fold_search%value
        if (case_fold_search%value .and. search_upper_case /= UPPER_CASE_NIL) then
            fold = isearch_no_upper_case_p(from_string, regexp_flag)
        end if
        nocasify = .not. (case_replace .and. fold)
        literal = .not. regexp_flag
        noedit = .false.
        keep_going = .true.
        nstack = 0
        last_was_undo = .false.
        last_was_act_and_show = .false.
        update_stack = .true.
        replace_count = 0
        skip_read_only_count = 0
        again = AGAIN_T
        allocate (again_match(0), real_data(0))
        unread = .false.
        if (querying) then
            msg_prefix = "Query replacing "
            if (back) msg_prefix = msg_prefix//"backward "
            if (delimited_flag) msg_prefix = msg_prefix//"word "
            if (regexp_flag) msg_prefix = msg_prefix//"regexp "
        end if
        ! If region is active, in Transient Mark mode, operate on region.
        limit = 0
        have_limit = .false.
        if (present(start) .and. present(end)) then
            have_limit = .true.
            call make_marker(limit_marker)
            if (back) then
                call set_marker(limit_marker, min(start, end), st)
                call goto_char(max(start, end))
            else
                call set_marker(limit_marker, max(start, end), st)
                call goto_char(min(start, end))
            end if
            call deactivate_mark()
        end if
        next_replacement = replacements
        use_items = .false.
        use_fn = present(replacement_fn)
        if (.not. use_fn .and. present(replacement_list)) then
            call list_elements(replacement_list, items)
            use_items = size(items) > 0
            ! This is a hand-made `iterator'.
            repeat = 1
            if (present(repeat_count)) repeat = max(1_int64, repeat_count)
            rotate_count = repeat
            item = 1
        end if
        call push_mark()
        call undo_boundary()
        saved_fold = case_fold_search%value
        case_fold_search = fold
        ! Loop finding occurrences that perhaps should be replaced.
        main: do while (keep_going)
            if (have_limit) call marker_position(limit_marker, limit, st)
            if (back) then
                if (bobp()) exit main
                if (have_limit .and. point() <= limit) exit main
            else
                if (eobp()) exit main
                if (have_limit .and. point() >= limit) exit main
            end if
            ! Use the next match if it is already known; otherwise, search
            ! for a match after moving forward one char if progress is
            ! required.
            select case (again)
            case (AGAIN_DATA)
                if (back) then
                    call goto_char(again_match(1))
                else
                    call goto_char(again_match(2))
                end if
                real_data = again_match
                found = .true.
            case (AGAIN_T)
                ! MATCH-AGAIN non-nil means accept an adjacent match.
                found = replace_search(from_string, merge(limit, 0_int64, have_limit), regexp_flag, &
                                       delimited_flag, fold, status, back)
                if (.not. status%ok()) exit main
                if (found) real_data = match_data()
            case default
                found = .false.
                if (back) then
                    if (point() - 1 > point_min()) found = .not. have_limit .or. point() - 1 > limit
                else
                    if (point() + 1 < point_max()) found = .not. have_limit .or. point() + 1 < limit
                end if
                if (found) then
                    ! If not accepting adjacent matches, move one char to
                    ! the right before searching again.  Undo the motion if
                    ! the search fails.
                    opoint = point()
                    call forward_char(st, merge(-1_int64, 1_int64, back))
                    found = replace_search(from_string, merge(limit, 0_int64, have_limit), regexp_flag, &
                                           delimited_flag, fold, status, back)
                    if (.not. status%ok()) exit main
                    if (found) then
                        real_data = match_data()
                    else
                        call goto_char(opoint)
                    end if
                end if
            end select
            if (.not. found) exit main
            ! Record whether the match is nonempty, to avoid an infinite
            ! loop repeatedly matching the same empty string.
            nonempty_match = real_data(1) /= real_data(2)
            ! If the match is empty, record that the next one can't be
            ! adjacent.  Otherwise, if matching a regular expression, do
            ! the next match now, since the replacement for this match may
            ! affect whether the next match is adjacent to this one.  If
            ! that match is empty, don't use it.
            again = AGAIN_NIL
            if (nonempty_match) then
                if (.not. regexp_flag) then
                    again = AGAIN_T
                else
                    if (back) then
                        matched = looking_back(from_string, st)
                    else
                        matched = looking_at(from_string, st)
                    end if
                    if (matched) then
                        again_match = match_data()
                        if (again_match(1) /= again_match(2)) again = AGAIN_DATA
                    end if
                end if
            end if
            ! Optionally ignore matches that have a read-only property.
            if (query_replace_skip_read_only) then
                call text_property_not_all(real_data(1), real_data(2), sym("read_only"), nil(), ro_pos, &
                                           ro_found, st)
                if (ro_found) then
                    skip_read_only_count = skip_read_only_count + 1
                    cycle main
                end if
            end if
            ! Calculate the replacement string, if necessary.
            if (use_fn) then
                call set_match_data(real_data)
                next_replacement = replacement_fn(replace_count)
            else if (use_items) then
                ! replace-loop-through-replacements
                if (rotate_count == replace_count) then
                    rotate_count = replace_count + repeat
                    item = item + 1
                    if (item > size(items)) item = 1
                end if
                next_replacement = xstring(items(item))
            else if (is_counted) then
                next_replacement = replace_expand_count(replacements, replace_count)
            end if
            if (.not. querying) then
                call act(next_replacement, nocasify)
                if (.not. status%ok()) exit main
                replace_count = replace_count + 1
                cycle main
            end if
            call undo_boundary()
            done = .false.
            replaced = .false.
            ! Loop reading commands until one of them sets done, which
            ! means it has finished handling this occurrence.  Any command
            ! that sets `done' should leave behind proper match data for the
            ! stack.  Commands not setting `done' need to adjust
            ! `real_match_data'.
            do while (.not. done)
                call set_match_data(real_data)
                ! Obtain the matched groups: needed only when regexp-flag
                ! non nil.
                if (last_was_undo .and. regexp_flag) then
                    last_was_undo = .false.
                    real_data = match_data_at(real_data(1), from_string)
                end if
                ! Matched string and next-replacement-replaced stored in
                ! stack.
                call buffer_substring(real_data(1), real_data(2), search_string_replaced, st)
                call set_match_data(real_data)
                presentation = match_substitute_replacement(next_replacement, st, nocasify, literal)
                if (.not. st%ok()) presentation = next_replacement
                ! (query-replace-descr only adds display properties: the
                ! text stays the replacement.)
                next_replacement_replaced = presentation
                if (.not. query_replace_show_replacement) presentation = next_replacement
                call read_key(msg_prefix//query_replace_descr(from_string)//" with "// &
                              query_replace_descr(presentation)//": (? for help) ", key, status)
                if (.not. status%ok()) exit main
                ! Necessary in case something happens during read-event
                ! that clobbers the match data.
                call set_match_data(real_data)
                def = query_replace_answer(key)
                select case (def)
                case (ANSWER_HELP)
                    help_text = "Query replacing "
                    if (back) help_text = help_text//"backward "
                    if (delimited_flag) help_text = help_text//"word "
                    if (regexp_flag) help_text = help_text//"regexp "
                    call with_help_window(help_text//from_string//" with "//next_replacement//"."//LF//LF// &
                                          query_replace_help, st)
                case (ANSWER_EXIT)
                    keep_going = .false.
                    done = .true.
                case (ANSWER_BACKUP)
                    if (nstack > 0) then
                        elt = stack(nstack)
                        nstack = nstack - 1
                        call goto_char(elt%point)
                        replaced = elt%replaced
                        real_data = elt%data
                        next_replacement = elt%next_replacement
                        again = elt%again
                        again_match = elt%again_match
                    else
                        call message("No previous match")
                        call ding()
                        call sit_for(1000)
                    end if
                case (ANSWER_UNDO, ANSWER_UNDO_ALL)
                    if (nstack == 0) then
                        call message("Nothing to undo")
                        call ding()
                        call sit_for(1000)
                    else
                        stack_idx = 0
                        stack_len = nstack
                        num_replacements = 0
                        do while (stack_idx < stack_len .and. nstack > 0 .and. &
                                  (.not. replaced .or. last_was_act_and_show))
                            elt = stack(nstack - stack_idx)
                            stack_idx = stack_idx + 1
                            replaced = elt%replaced
                            ! Bind swapped values (search-string <--> replacement)
                            if (replaced) then
                                undo_search = elt%next_replace
                                last_replacement = elt%search_str
                            else
                                undo_search = elt%search_str
                                last_replacement = elt%next_replace
                            end if
                            search_string_replaced = undo_search
                            next_replacement_replaced = last_replacement
                            last_was_act_and_show = .false.
                            if (stack_idx == stack_len .and. .not. replaced .and. num_replacements == 0) then
                                call message("Nothing to undo")
                                call ding()
                                call sit_for(1000)
                            end if
                            if (replaced) then
                                nstack = nstack - stack_idx
                                call goto_char(elt%point)
                                call set_match_data(elt%data)
                                ! We must quote the string (Bug#37073)
                                real_data = match_data_at(elt%data(1), regexp_quote(undo_search))
                                ! Undo must preserve case (Bug#31073).
                                call act(last_replacement, .true.)
                                if (.not. status%ok()) exit main
                                replace_count = replace_count - 1
                                if (regexp_flag) then
                                    real_data = match_data_at(match_beginning(0, st), last_replacement)
                                else
                                    real_data = match_data_at(match_beginning(0, st), regexp_quote(last_replacement))
                                end if
                                if (regexp_flag) next_replacement = elt%next_replace
                                ! Set replaced nil to keep in loop
                                if (def == ANSWER_UNDO_ALL) then
                                    replaced = .false.
                                    stack_len = stack_len - stack_idx
                                    stack_idx = 0
                                    num_replacements = num_replacements + 1
                                end if
                            end if
                        end do
                        if (def == ANSWER_UNDO_ALL .and. num_replacements /= 0) then
                            if (num_replacements == 1) then
                                call message("Undid 1 replacement")
                            else
                                call message("Undid "//number_to_string(int(num_replacements, int64))// &
                                             " replacements")
                            end if
                            call ding()
                            call sit_for(1000)
                        end if
                        replaced = .false.
                        last_was_undo = .true.
                        last_was_act_and_show = .false.
                    end if
                case (ANSWER_ACT)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                    end if
                    done = .true.
                    replaced = .true.
                    update_stack = .not. last_was_act_and_show
                case (ANSWER_ACT_AND_EXIT)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                    end if
                    keep_going = .false.
                    done = .true.
                    replaced = .true.
                case (ANSWER_ACT_AND_SHOW)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                        real_data = match_data()
                        replaced = .true.
                        last_was_act_and_show = .true.
                        call push_stack(stack, nstack, replaced, search_string_replaced, &
                                        next_replacement_replaced, next_replacement, again, again_match)
                    end if
                case (ANSWER_AUTOMATIC)
                    if (.not. replaced) then
                        call act(next_replacement, nocasify)
                        if (.not. status%ok()) exit main
                        replace_count = replace_count + 1
                    end if
                    done = .true.
                    querying = .false.
                    replaced = .true.
                case (ANSWER_SKIP)
                    done = .true.
                    update_stack = .not. last_was_act_and_show
                case (ANSWER_RECENTER)
                    call recenter_top_bottom(nil(), st)
                case (ANSWER_EDIT)
                    opoint = point()
                    call make_marker(opos)
                    call set_marker(opos, opoint, st)
                    call goto_char(match_beginning(0, st))
                    ! save-excursion
                    opoint = point()
                    call recursive_edit_1(status)
                    if (.not. status%ok()) then
                        call free_marker(opos, st)
                        exit main
                    end if
                    call goto_char(opoint)
                    call marker_position(opos, opoint, st)
                    call goto_char(opoint)
                    call free_marker(opos, st)
                    ! Before we make the replacement, decide whether the
                    ! search string can match again just after this match.
                    if (regexp_flag .and. nonempty_match) then
                        again = AGAIN_NIL
                        if (looking_at(from_string, st)) then
                            again = AGAIN_DATA
                            again_match = match_data()
                        end if
                    end if
                case (ANSWER_EDIT_REPLACEMENT, ANSWER_EDIT_REPLACEMENT_EXACT_CASE)
                    if (def == ANSWER_EDIT_REPLACEMENT_EXACT_CASE) then
                        call read_string("Edit replacement string (exact case): ", edited, status, &
                                         initial=next_replacement)
                    else
                        call read_string("Edit replacement string: ", edited, status, initial=next_replacement)
                    end if
                    if (.not. status%ok()) exit main
                    next_replacement = edited
                    noedit = .false.
                    if (replaced) then
                        call set_match_data(real_data)
                    else
                        call act(next_replacement, def == ANSWER_EDIT_REPLACEMENT_EXACT_CASE .or. nocasify)
                        if (.not. status%ok()) exit main
                        replaced = .true.
                    end if
                    next_replacement_replaced = next_replacement
                    done = .true.
                case (ANSWER_DELETE_AND_EDIT)
                    call replace_match("", status, .true., .true.)
                    if (.not. status%ok()) exit main
                    call adjust_markers(real_data(1), real_data(2))
                    real_data = match_data()
                    opoint = point()
                    call recursive_edit_1(status)
                    if (.not. status%ok()) exit main
                    call goto_char(opoint)
                    replaced = .true.
                case (ANSWER_DIFF)
                    call message("The diff answer needs diff_mode, which is not ported yet")
                    call ding()
                    call sit_for(1000)
                case (ANSWER_SCROLL_UP, ANSWER_SCROLL_DOWN)
                    ! (commandp def t): (call-interactively def).
                    if (def == ANSWER_SCROLL_UP) then
                        call scroll_up(nil(), status)
                    else
                        call scroll_down(nil(), status)
                    end if
                    if (.not. status%ok()) exit main
                case (ANSWER_QUIT)
                    call set_error(status, FM_QUIT)
                    exit main
                case default
                    ! Any other key (and exit-prefix) exits, and is then
                    ! re-executed as a normal key sequence.  (The commands
                    ! scroll-other-window and scroll-other-window-down are
                    ! not ported: Deferred.)
                    this_command = sym("mode_exited")
                    keep_going = .false.
                    call unread_command_events([key])
                    unread = .true.
                    done = .true.
                end select
            end do
            ! Record previous position for ^ when we move on.
            if (update_stack) call push_stack(stack, nstack, replaced, search_string_replaced, &
                                              next_replacement_replaced, next_replacement, again, again_match)
            if (allocated(next_replacement_replaced)) deallocate (next_replacement_replaced)
            if (allocated(search_string_replaced)) deallocate (search_string_replaced)
            last_was_act_and_show = .false.
        end do main
        case_fold_search = saved_fold
        if (have_limit) call free_marker(limit_marker, st)
        if (.not. status%ok()) return
        if (.not. unread) then
            help_text = "Replaced "//number_to_string(replace_count)//" occurrence"
            if (replace_count /= 1) help_text = help_text//"s"
            if (skip_read_only_count > 0) then
                help_text = help_text//" (skipped "//number_to_string(skip_read_only_count)//" read-only)"
            end if
            call message(help_text)
        end if
        if (present(result)) result = keep_going .and. nstack > 0

    contains

        ! (setq noedit (replace-match-maybe-edit TEXT FIXEDCASE literal noedit
        ! real-match-data backward)), then move the positions that are
        ! markers upstream (match-again and the match data of the stack).
        recursive subroutine act(text, fixedcase)
            character(len=*), intent(in) :: text
            logical, intent(in) :: fixedcase
            integer(int64) :: from, to
            from = real_data(1)
            to = real_data(2)
            noedit = replace_match_maybe_edit(text, fixedcase, literal, noedit, real_data, status, back)
            if (status%ok()) call adjust_markers(from, to)
        end subroutine act

        ! Adjust the stored positions for the replacement of [FROM, TO)
        ! by the current match, as replace_range adjusts markers.
        subroutine adjust_markers(from, to)
            integer(int64), intent(in) :: from, to
            type(status_type) :: s
            integer(int64) :: delta
            integer :: k
            delta = match_end(0, s) - match_beginning(0, s) - (to - from)
            call adjust(again_match, from, to, delta)
            do k = 1, nstack
                if (.not. stack(k)%replaced) call adjust(stack(k)%data, from, to, delta)
            end do
        end subroutine adjust_markers

        ! Move POSITIONS as markers move when [FROM, TO) is replaced by
        ! text DELTA characters longer.
        pure subroutine adjust(positions, from, to, delta)
            integer(int64), intent(inout) :: positions(:)
            integer(int64), intent(in) :: from, to, delta
            integer :: i
            do i = 1, size(positions)
                if (positions(i) <= 0) cycle
                if (positions(i) >= to) then
                    positions(i) = positions(i) + delta
                else if (positions(i) > from) then
                    positions(i) = from
                end if
            end do
        end subroutine adjust
    end subroutine perform_replace

    ! ------------------------------------------------------------------
    ! The commands
    ! ------------------------------------------------------------------

    !> Replace some occurrences of FROM_STRING with TO_STRING.
    !> As each match is found, the user must type a character saying
    !> what to do with it.  Type SPC or `y' to replace the match,
    !> DEL or `n' to skip and go to the next match.  For more directions,
    !> type C-h at that time.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> In interactive use, the prefix arg (true DELIMITED in
    !> non-interactive use), means replace only matches surrounded by
    !> word boundaries.  A negative prefix arg means replace backward.
    !>
    !> Use M-n to pull the last incremental search string to the minibuffer
    !> that reads FROM_STRING, or invoke replacements from incremental
    !> search with a key sequence like `C-s C-s M-%' to use its current
    !> search string as the string to replace.
    !>
    !> Matching is independent of case if both `case_fold_search'
    !> and `search_upper_case' are true (not UPPER_CASE_NIL) and FROM_STRING has no
    !> uppercase letters; if `search_upper_case' is UPPER_CASE_NIL, then whether
    !> matching ignores case depends on `case_fold_search' regardless
    !> of whether there are uppercase letters in FROM_STRING.
    !> Replacement transfers the case pattern of the old text to the
    !> new text, if both `case_fold_search' and `case_replace' are
    !> true and FROM_STRING has no uppercase letters.
    !> (Transferring the case pattern means that if the old text
    !> matched is all caps, or all of its words are capitalized, then its
    !> replacement is respectively upcased or capitalized.  For more
    !> details about this, see `replace_match'.)
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_lax_whitespace' is true, a space or spaces in the string
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> To customize possible responses, change the bindings in
    !> `query_replace_map'.
    !>
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> Arguments FROM_STRING, TO_STRING, DELIMITED, START, END and BACKWARD
    !> are passed to `perform_replace' (which see).  Port of
    !> replace.el:query-replace.
    recursive subroutine query_replace(from_string, to_string, status, delimited, start, end, backward)
        character(len=*), intent(in) :: from_string, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward
        integer(int64), intent(in), optional :: start, end
        call perform_replace(from_string, to_string, .true., .false., present_true(delimited), status, &
                             start=start, end=end, backward=backward)
    end subroutine query_replace

    !> Replace some things after point matching REGEXP with TO_STRING.
    !> As each match is found, the user must type a character saying
    !> what to do with it.  Type SPC or `y' to replace the match,
    !> DEL or `n' to skip and go to the next match.  For more directions,
    !> type C-h at that time.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> When invoked interactively, matching a newline with `\n' will not work;
    !> use `C-q C-j' instead.  To match a tab character (`\t'), just press `TAB'.
    !>
    !> Matching is independent of case if both `case_fold_search'
    !> and `search_upper_case' are true (not UPPER_CASE_NIL) and REGEXP has no uppercase
    !> letters; if `search_upper_case' is UPPER_CASE_NIL, then whether matching
    !> ignores case depends on `case_fold_search' regardless of whether
    !> there are uppercase letters in REGEXP.
    !> Replacement transfers the case pattern of the old text to the new
    !> text, if both `case_fold_search' and `case_replace' are true
    !> and REGEXP has no uppercase letters.  (Transferring the case pattern
    !> means that if the old text matched is all caps, or all of its words
    !> are capitalized, then its replacement is respectively upcased or
    !> capitalized.  For more details about this, see `replace_match'.)
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_regexp_lax_whitespace' is true, a space or spaces in the regexp
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> Third arg DELIMITED (prefix arg if interactive), if true, means replace
    !> only matches surrounded by word boundaries.  A negative prefix arg means
    !> replace backward.
    !>
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> In TO_STRING, `\&' stands for whatever matched the whole of REGEXP,
    !> and `\N' (where N is a digit) stands for whatever matched
    !> the Nth `\(...\)' (1-based) in REGEXP.  The `\(...\)' groups are
    !> counted from 1.
    !> `\?' lets you edit the replacement text in the minibuffer
    !> at the given position for each replacement.
    !>
    !> In interactive calls, `\#' in the replacement text stands for the
    !> number of replacements done so far (starting with zero); `counted'
    !> says that TO_STRING uses it.
    !>
    !> Arguments REGEXP, TO_STRING, DELIMITED, START, END, and BACKWARD
    !> are passed to `perform_replace' (which see).  Port of
    !> replace.el:query-replace-regexp (`\,' followed by an expression to
    !> evaluate is not applicable: there is no run-time evaluation).
    recursive subroutine query_replace_regexp(regexp, to_string, status, delimited, start, end, backward, counted)
        character(len=*), intent(in) :: regexp, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward, counted
        integer(int64), intent(in), optional :: start, end
        call perform_replace(regexp, to_string, .true., .true., present_true(delimited), status, &
                             start=start, end=end, backward=backward, counted=counted)
    end subroutine query_replace_regexp

    !> Replace some matches for REGEXP with various strings, in rotation.
    !> The second argument TO_STRINGS contains the replacement strings,
    !> separated by spaces.  This command works like
    !> `query_replace_regexp' except that each successive replacement
    !> uses the next successive replacement string, wrapping around from
    !> the last such string to the first.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> As each match is found, the user must type a character saying
    !> what to do with it.  Type SPC or `y' to replace the match,
    !> DEL or `n' to skip and go to the next match.  For more directions,
    !> type C-h at that time.
    !>
    !> A prefix argument N says to use each replacement string N times
    !> before rotating to the next.
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> Arguments REGEXP, START, and END are passed to `perform_replace'
    !> (which see).  Port of replace.el:map-query-replace-regexp.
    recursive subroutine map_query_replace_regexp(regexp, to_strings, status, n, start, end)
        character(len=*), intent(in) :: regexp, to_strings
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: n, start, end
        type(value_type), allocatable :: items(:), grown(:)
        character(len=:), allocatable :: rest
        integer :: at
        allocate (items(0))
        rest = to_strings
        do while (len(rest) /= 0)
            at = index(rest, " ")
            allocate (grown(size(items) + 1))
            grown(1:size(items)) = items
            if (at > 0) then
                grown(size(grown)) = make_string(rest(1:at - 1))
                rest = rest(at + 1:)
            else
                grown(size(grown)) = make_string(rest)
                rest = ""
            end if
            call move_alloc(grown, items)
        end do
        call perform_replace(regexp, "", .true., .true., .false., status, repeat_count=n, start=start, end=end, &
                             replacement_list=make_list(items))
    end subroutine map_query_replace_regexp

    !> Replace occurrences of FROM_STRING with TO_STRING.
    !> Preserve case in each match if `case_replace' and `case_fold_search'
    !> are true and FROM_STRING has no uppercase letters.
    !> (Preserving case means that if the string matched is all caps, or capitalized,
    !> then its replacement is upcased or capitalized.)
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_lax_whitespace' is true, a space or spaces in the string
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> Third arg DELIMITED (prefix arg if interactive), if true, means replace
    !> only matches surrounded by word boundaries.  A negative prefix arg means
    !> replace backward.
    !>
    !> Operates on the region between START and END (if both are absent, from point
    !> to the end of the buffer).  Interactively, if Transient Mark mode is
    !> enabled and the mark is active, operates on the contents of the region;
    !> otherwise from point to the end of the buffer's accessible portion.
    !>
    !> Argument BACKWARD is passed to `perform_replace' (which see).
    !>
    !> This function is usually the wrong thing to use in a program.
    !> What you probably want is a loop like this:
    !>   do
    !>       call search_forward(FROM_STRING, found, status, noerror=SEARCH_NOERROR)
    !>       if (found == 0) exit
    !>       call replace_match(TO_STRING, status, literal=.true.)
    !>   end do
    !> which will run faster and will not set the mark or print anything.
    !> (You may need a more complex loop if FROM_STRING can match the null string
    !> and TO_STRING is also null.)  Port of replace.el:replace-string.
    recursive subroutine replace_string(from_string, to_string, status, delimited, start, end, backward)
        character(len=*), intent(in) :: from_string, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward
        integer(int64), intent(in), optional :: start, end
        call perform_replace(from_string, to_string, .false., .false., present_true(delimited), status, &
                             start=start, end=end, backward=backward)
    end subroutine replace_string

    !> Replace things after point matching REGEXP with TO_STRING.
    !> Preserve case in each match if `case_replace' and `case_fold_search'
    !> are true and REGEXP has no uppercase letters.
    !>
    !> Ignore read-only matches if `query_replace_skip_read_only' is true.
    !>
    !> If `replace_regexp_lax_whitespace' is true, a space or spaces in the regexp
    !> to be replaced will match a sequence of whitespace chars defined by the
    !> regexp in `search_whitespace_regexp'.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region.  Otherwise, operate from point to the end of the buffer's
    !> accessible portion.
    !>
    !> Third arg DELIMITED (prefix arg if interactive), if true, means replace
    !> only matches surrounded by word boundaries.  A negative prefix arg means
    !> replace backward.
    !>
    !> Fourth and fifth arg START and END specify the region to operate on.
    !>
    !> Argument BACKWARD is passed to `perform_replace' (which see).
    !>
    !> In TO_STRING, `\&' stands for whatever matched the whole of REGEXP,
    !> and `\N' (where N is a digit) stands for whatever matched
    !> the Nth `\(...\)' (1-based) in REGEXP.
    !> `\?' lets you edit the replacement text in the minibuffer
    !> at the given position for each replacement.
    !>
    !> In interactive calls, `\#' in the replacement text stands for
    !> `replace_count', the number of replacements occurred so far,
    !> starting from zero; `counted' says that TO_STRING uses it.
    !> `replacement_fn', if given, computes each replacement instead.
    !>
    !> This function is usually the wrong thing to use in a program.
    !> What you probably want is a loop like this:
    !>   do
    !>       call re_search_forward(REGEXP, found, status, noerror=SEARCH_NOERROR)
    !>       if (found == 0) exit
    !>       call replace_match(TO_STRING, status)
    !>   end do
    !> which will run faster and will not set the mark or print anything.
    !> Port of replace.el:replace-regexp.
    recursive subroutine replace_regexp(regexp, to_string, status, delimited, start, end, backward, counted, &
                                        replacement_fn)
        character(len=*), intent(in) :: regexp, to_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delimited, backward, counted
        integer(int64), intent(in), optional :: start, end
        procedure(replacement_function), optional :: replacement_fn
        call perform_replace(regexp, to_string, .false., .true., present_true(delimited), status, &
                             start=start, end=end, backward=backward, counted=counted, &
                             replacement_fn=replacement_fn)
    end subroutine replace_regexp

    !> Delete lines containing matches for REGEXP.
    !> When called from a program (and usually when called interactively as
    !> well, see below), applies to the part of the buffer after point.
    !> The line point is in is deleted if and only if it contains a
    !> match for regexp starting after point.
    !>
    !> If REGEXP contains upper case characters (excluding those preceded by `\')
    !> and `search_upper_case' is true, the matching is case-sensitive.
    !>
    !> Second and third arg RSTART and REND specify the region to operate on.
    !> Lines partially contained in this region are deleted if and only if
    !> they contain a match entirely contained in it.
    !>
    !> Interactively, in Transient Mark mode when the mark is active, operate
    !> on the contents of the region.  Otherwise, operate from point to the
    !> end of (the accessible portion of) the buffer.  When calling this function
    !> from a program, you can pretend that it was called interactively by passing
    !> a true INTERACTIVE argument.
    !>
    !> If a match is split across lines, all the lines it lies in are deleted.
    !> They are deleted _before_ looking for the next match.  Hence, a match
    !> starting on the same line at which another match ended is ignored.
    !>
    !> Return the number of deleted matching lines.  When called interactively,
    !> also print the number.
    !>
    !> If you want to not just delete the lines, but also add them to
    !> the kill ring, use the \[kill_matching_lines] command instead.
    !> Port of replace.el:flush-lines (the command's argument reading is
    !> Deferred).
    recursive subroutine flush_lines(regexp, count, status, rstart, rend, interactive)
        use cmds_fm, only: forward_line
        use editfns_fm, only: delete_region
        character(len=*), intent(in) :: regexp
        integer(int64), intent(out) :: count
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: rstart, rend
        logical, intent(in), optional :: interactive
        type(handle_type) :: end_marker, opoint
        integer(int64) :: limit, found, beg, shortage
        logical :: inter, saved_fold
        type(status_type) :: st
        count = 0
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        call make_marker(end_marker)
        if (present(rstart) .and. present(rend)) then
            call goto_char(min(rstart, rend))
            call set_marker(end_marker, max(rstart, rend), st)
        else if (inter .and. use_region_p()) then
            call region_end(limit, st)
            call set_marker(end_marker, limit, st)
            call region_beginning(limit, st)
            call goto_char(limit)
        else
            call set_marker(end_marker, point_max(), st)
        end if
        saved_fold = case_fold_search%value
        if (case_fold_search%value .and. search_upper_case /= UPPER_CASE_NIL) then
            case_fold_search%value = isearch_no_upper_case_p(regexp, .true.)
        end if
        call make_marker(opoint)
        call set_marker(opoint, point(), st)
        do
            call marker_position(end_marker, limit, st)
            if (point() >= limit) exit
            call re_search_forward(regexp, found, status, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit
            call goto_char(match_beginning(0, st))
            shortage = forward_line(0_int64)
            beg = point()
            call goto_char(match_end(0, st))
            shortage = forward_line(1_int64)
            call delete_region(beg, point(), status)
            if (.not. status%ok()) exit
            count = count + 1
        end do
        case_fold_search%value = saved_fold
        call marker_position(opoint, beg, st)
        call goto_char(beg)
        call free_marker(opoint, st)
        call free_marker(end_marker, st)
        if (.not. status%ok()) return
        if (inter) then
            if (count == 1) then
                call message("Deleted 1 matching line")
            else
                call message("Deleted "//number_to_string(count)//" matching lines")
            end if
        end if
    end subroutine flush_lines

    !> The interactive prompt of a replace command: BASE, then " backward"
    !> or " word" for the prefix argument, then WHAT and " in region".
    function command_prompt(base, what) result(prompt)
        character(len=*), intent(in) :: base, what
        character(len=:), allocatable :: prompt
        prompt = base
        if (.not. nilp(current_prefix_arg)) then
            if (value_eq(current_prefix_arg, sym("-"))) then
                prompt = prompt//" backward"
            else
                prompt = prompt//" word"
            end if
        end if
        prompt = prompt//what
        if (use_region_p()) prompt = prompt//" in region"
    end function command_prompt

    ! The interactive commands: read the arguments, then operate on the
    ! region when it is in use.
    recursive subroutine run_command(base, what, regexp_flag, query, status)
        character(len=*), intent(in) :: base, what
        logical, intent(in) :: regexp_flag, query
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        logical :: delimited, backward, counted
        integer(int64) :: b, e
        type(status_type) :: st
        call query_replace_read_args(command_prompt(base, what), regexp_flag, from, to, delimited, backward, &
                                     counted, status)
        if (.not. status%ok()) return
        if (use_region_p()) then
            call region_beginning(b, st)
            call region_end(e, st)
            call perform_replace(from, to, query, regexp_flag, delimited, status, start=b, end=e, &
                                 backward=backward, counted=counted)
        else
            call perform_replace(from, to, query, regexp_flag, delimited, status, backward=backward, &
                                 counted=counted)
        end if
    end subroutine run_command

    recursive subroutine cmd_query_replace(status)
        type(status_type), intent(out) :: status
        call run_command("Query replace", "", .false., .true., status)
    end subroutine cmd_query_replace

    recursive subroutine cmd_query_replace_regexp(status)
        type(status_type), intent(out) :: status
        call run_command("Query replace", " regexp", .true., .true., status)
    end subroutine cmd_query_replace_regexp

    recursive subroutine cmd_replace_string(status)
        type(status_type), intent(out) :: status
        call run_command("Replace", " string", .false., .false., status)
    end subroutine cmd_replace_string

    recursive subroutine cmd_replace_regexp(status)
        type(status_type), intent(out) :: status
        call run_command("Replace", " regexp", .true., .false., status)
    end subroutine cmd_replace_regexp

    recursive subroutine cmd_map_query_replace_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        integer(int64) :: b, e
        type(status_type) :: st
        call read_regexp("Map query replace (regexp): ", from, status, history=query_replace_from_history_variable)
        if (.not. status%ok()) return
        call read_from_minibuffer("Query replace "//query_replace_descr(from)//" with (space-separated strings): ", &
                                  to, status, history=query_replace_to_history_variable, default=from)
        if (.not. status%ok()) return
        if (use_region_p()) then
            call region_beginning(b, st)
            call region_end(e, st)
            if (nilp(current_prefix_arg)) then
                call map_query_replace_regexp(from, to, status, start=b, end=e)
            else
                call map_query_replace_regexp(from, to, status, n=prefix_numeric_value(current_prefix_arg), &
                                              start=b, end=e)
            end if
        else if (nilp(current_prefix_arg)) then
            call map_query_replace_regexp(from, to, status)
        else
            call map_query_replace_regexp(from, to, status, n=prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_map_query_replace_regexp

    !> Give the variables their defaults and register the commands.
    subroutine syms_of_replace()
        integer, parameter :: C = FACE_CLASS_COLOR, M = FACE_CLASS_MONO, LIGHT = FACE_BACKGROUND_LIGHT, &
                              DARK = FACE_BACKGROUND_DARK
        type(status_type) :: st
        type(value_type) :: history
        ! (faces.el is loaded before replace.el.)
        call init_faces()
        call match%define("match", "Face used to highlight matches permanently.")
        call match%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                              face_attrs(background="khaki1"))
        call match%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                              face_attrs(background="RoyalBlue3"))
        call match%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[LIGHT]), &
                              face_attrs(background="yellow", foreground="black"))
        call match%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[DARK]), &
                              face_attrs(background="blue", foreground="white"))
        call match%spec_entry(display_when(types=[character(len=3) :: "tty"], classes=[M]), &
                              face_attrs(inverse_video=.true.))
        call match%spec_entry(display_when(), face_attrs(background="gray"))
        query_replace_from_to_separator = " → "
        query_replace_from_history_variable = "query_replace_history"
        call defvar_nil("query_replace_history")
        call defvar_nil("regexp_history")
        query_replace_to_history_variable = "query_replace_history"
        ! Default history list for `query_replace' commands.
        call symbol_value(sym("query_replace_history"), history, st)
        if (.not. st%ok()) call set(sym("query_replace_history"), nil(), st)
        commands%query_replace%run => cmd_query_replace
        commands%query_replace_regexp%run => cmd_query_replace_regexp
        commands%map_query_replace_regexp%run => cmd_map_query_replace_regexp
        commands%replace_string%run => cmd_replace_string
        commands%replace_regexp%run => cmd_replace_regexp
    end subroutine syms_of_replace
end module replace_fm
