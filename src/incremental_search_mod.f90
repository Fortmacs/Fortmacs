! incremental_search_mod.f90 -- incremental search minor mode.
! Ported from GNU Emacs 31.1 lisp/isearch.el.
!
! Copyright (C) 1992-1997, 1999-2026 Free Software Foundation, Inc.
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

!> Incremental search: C-s and C-r.  As upstream, `isearch_mode' makes
!> `isearch_mode_map' the overriding terminal-local map and installs a
!> pre-command hook; keys bound there extend or move the search, and any
!> other command ends the search first and then runs as usual.
!>
!> Literal and regexp search (C-M-s, C-M-r, M-r), with lax whitespace
!> matching (M-s SPC).  Modes change how the search is done with the
!> buffer-local `isearch_search_function_function', `isearch_wrap_function',
!> `isearch_push_state_function' and `isearch_filter_test_function', and
!> `isearch_mode_hook' and `isearch_mode_end_hook' (Info searches through
!> its nodes).  Not ported yet: word, identifier and char-fold search
!> (`isearch_regular_expression_function', `search_default_mode'),
!> highlighting the match and lazy highlighting (faces from overlays),
!> C-M-w (identifier syntax: forward-symbol), reading a kill with the
!> minibuffer (`isearch_paste_from_clipboard_history', C-u M-y), search through invisible text, scrolling during search
!> (`isearch_allow_scroll'), `isearch_allow_motion', query-replace and
!> occur from isearch, the help map, input methods, completion, the
!> minibuffer's own isearch keys while editing the string.
module incremental_search_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: prefix_argument_type, PREFIX_NONE, PREFIX_NUMBER
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, QUIT_CODE, USER_ERROR_CODE, FM_INVALID_REGULAR_EXPRESSION, SEARCH_FAILED_CODE, &
                         USER_SEARCH_FAILED_CODE, error_message
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, xfixnum, xstring, make_keymap_reference, list_length, list_element, list_elements, make_list, &
                         vtag, V_INTEGER, V_STRING
    use data_mod, only: symbol_value, set, append_locals_clearer, history_setting_type, forward_setting
    use window_mod, only: same_handle
    use hooks_mod, only: hook_type
    use character_mod, only: string_character_and_length, MAXIMUM_CHARACTER, &
                             CHARACTER_SHIFT
    use case_table_mod, only: downcase
    use buffer_mod, only: buffers, current, transient_mark_mode, buf_fetch_char
    use marker_mod, only: character_to_byte
    use syntax_mod, only: character_syntax, forward_word
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_buffer, &
                                     is_end_of_buffer, is_end_of_line, position_end_of_line, buffer_substring, character_to_string
    use editing_commands_mod, only: forward_character
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, match_beginning, match_end, &
                          looking_at, string_match, regular_expression_quote, case_fold_search, search_spaces_regular_expression, &
                          SEARCH_NOERROR
    use keymap_mod, only: make_keymap, make_sparse_keymap, define_key, define_key_range, key_parse, &
                          lookup_key, is_keymap_live, destroy_keymap, overriding_terminal_local_map
    use keyboard_mod, only: this_command, last_command, last_command_event, this_command_keys, &
                            pre_command_hook, discard_input, is_input_pending, &
                            read_event, sit_for
    use execute_interactively_mod, only: is_command, current_prefix_argument, prefix_numeric_value
    use basic_subprograms_mod, only: add_to_history, history_delete_duplicates
    use minibuffer_reading_mod, only: read_from_minibuffer, history_append_new_input, minibuf_level
    use display_engine_mod, only: message, message_nolog
    use display_update_mod, only: ding
    use simple_mod, only: push_mark, current_clipboard_entry
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: isearch_forward_command, isearch_backward_command, isearch_mode, isearch_done, &
              isearch_update, isearch_exit, isearch_cancel, isearch_abort, &
              isearch_repeat_forward, isearch_repeat_backward, isearch_delete_character, &
              isearch_delete_end_character, isearch_printing_character, isearch_process_search_string, &
              isearch_paste_string, isearch_paste_character, isearch_paste_word_or_character, isearch_paste_line, &
              isearch_paste, isearch_paste_previous_only, &
              isearch_beginning_of_buffer, isearch_end_of_buffer, isearch_ring_advance, &
              isearch_ring_retreat, isearch_toggle_case_fold, isearch_edit_string, &
              isearch_message_prefix, initialize_isearch, isearch_forward_regular_expression_command, &
              isearch_backward_regular_expression_command, isearch_toggle_regular_expression, isearch_toggle_lax_whitespace, &
              is_isearch_no_upper_case, word_search_regular_expression, isearch_search_function_default, set_isearch_wrapped
    public :: isearch_search_procedure, isearch_search_function_procedure, isearch_wrap_procedure, isearch_pop_procedure, &
              isearch_push_state_procedure, isearch_filter_procedure, isearch_message_procedure, isearch_message_display, &
              set_isearch_barrier, set_isearch_original_point

    !> Values of `isearch_case_fold_search': CASE_FOLD_NO, CASE_FOLD_YES, and CASE_FOLD_YES (set by
    !> M-c: fold even when the string has upper-case letters).
    integer, parameter, public :: CASE_FOLD_NO = 0, CASE_FOLD_YES = 1, CASE_FOLD_FORCED = 2

    !> Values of `search_upper_case' besides .true. and .false.
    integer, parameter, public :: SEARCH_UPPER_CASE_NOT_PASTED = 2

    !> If true, upper case chars disable case fold searching.
    !> That is, upper and lower case chars must match exactly.
    !> This applies no matter where the chars come from, but does not
    !> apply to chars in regexps that are prefixed with `\'.
    !> If this value is SEARCH_UPPER_CASE_NOT_PASTED, text pasted into the search string
    !> in Isearch mode is always downcased.  Port of isearch.el
    !> search-upper-case.
    type(choice_setting_type), public :: search_upper_case = &
                                         choice_setting_type(name="search_upper_case", choice=SEARCH_UPPER_CASE_NOT_PASTED)

    !> True means random control characters terminate incremental
    !> search.  (Only true and false: `edit', `append', `move' and
    !> `shift_move' are not ported.)  Port of isearch.el search-exit-option.
    logical, public :: search_exit_option = .true.

    !> If true, do a nonincremental search instead of an incremental
    !> one.  This applies to RET typed immediately after entering
    !> incremental search.  Port of isearch.el
    !> search-nonincremental-instead.
    logical, public :: search_nonincremental_instead = .true.

    !> Maximum length of search ring before oldest elements are
    !> thrown away.  Port of isearch.el search-ring-max.
    integer, public :: search_ring_maximum = 16

    !> Maximum length of regexp search ring before oldest elements are
    !> thrown away.  Port of isearch.el regexp-search-ring-max.
    integer, public :: regular_expression_search_ring_maximum = 16

    !> If allocated, regular expression to match a sequence of whitespace
    !> chars.  When you enter a space or spaces in the incremental search,
    !> it will match any sequence matched by this regexp.  As an
    !> exception, spaces are treated normally in regexp incremental search
    !> if they occur in a regexp construct like [...] or *, + or ?.
    !>
    !> If the value is a string, it applies to both ordinary and regexp
    !> incremental search.  If the value is unallocated, or
    !> `isearch_lax_whitespace' is false for ordinary incremental search, or
    !> `isearch_regular_expression_lax_whitespace' is false for regexp incremental
    !> search, then each space you type matches literally, against one
    !> space.  Port of isearch.el search-whitespace-regexp (made "[ \t]+"
    !> by initialize_isearch).
    character(len=:), allocatable, public :: search_whitespace_regular_expression

    !> If true, a space will match a sequence of whitespace chars.
    !> When you enter a space or spaces in ordinary incremental search, it
    !> will match any sequence matched by the regexp defined by the
    !> variable `search_whitespace_regular_expression'.  If the value is false, each
    !> space you type matches literally, against one space.  You can
    !> toggle the value of this variable by the command
    !> `isearch_toggle_lax_whitespace', usually bound to `M-s SPC' during
    !> isearch.  Port of isearch.el isearch-lax-whitespace.
    logical, public :: isearch_lax_whitespace = .true.

    !> If true, a space will match a sequence of whitespace chars.
    !> When you enter a space or spaces in regexp incremental search, it
    !> will match any sequence matched by the regexp defined by the
    !> variable `search_whitespace_regular_expression'.  If the value is false, each
    !> space you type matches literally, against one space.  You can
    !> toggle the value of this variable by the command
    !> `isearch_toggle_lax_whitespace', usually bound to `M-s SPC' during
    !> isearch.  Port of isearch.el isearch-regexp-lax-whitespace.
    logical, public :: isearch_regular_expression_lax_whitespace = .false.

    !> isearch.el isearch-mode: whether Isearch mode is on.
    logical, public, protected :: isearch_active = .false.

    !> Keymap for `isearch_mode'.  Port of isearch.el isearch-mode-map.
    type(handle_type), public, protected :: isearch_mode_map

    !> The search string and the message shown for it (isearch.el's
    !> isearch-string and isearch-message), in UTF-8.
    character(len=:), allocatable, public, protected :: isearch_string, isearch_message
    !> Why the search string fails (isearch-error; unallocated for none).
    character(len=:), allocatable, public, protected :: isearch_error
    !> Searching forward (isearch-forward), and whether the search succeeds
    !> (isearch-success).
    logical, public, protected :: isearch_forward = .true., isearch_success = .true.
    !> Whether the search wrapped (isearch-wrapped).
    logical, public, protected :: isearch_wrapped = .false.
    !> Searching for a regexp (isearch-regexp).
    logical, public, protected :: isearch_regular_expression = .false.
    !> The other end of the match, where the search started and the
    !> barrier (isearch-other-end, isearch-opoint, isearch-barrier; 0 for
    !> none).
    integer(int64), public, protected :: isearch_other_end = 0, isearch_original_point = 0, isearch_barrier = 0
    !> Whether the search ignores case (isearch-case-fold-search; a CASE_FOLD_
    !> value).
    integer, public, protected :: isearch_case_fold_search = CASE_FOLD_YES
    integer :: isearch_last_case_fold_search = CASE_FOLD_YES
    !> isearch-adjusted: ADJUSTED_NO, ADJUSTED_PENDING (shows "pending"), or ADJUSTED_TOGGLE.
    integer, parameter :: ADJUSTED_NO = 0, ADJUSTED_PENDING = 1, ADJUSTED_TOGGLE = 2
    integer :: isearch_adjusted = ADJUSTED_NO
    logical :: isearch_paste_flag = .false.
    logical :: isearch_just_started = .false.
    !> Whether the search is nonincremental (isearch-nonincremental).
    logical, public, protected :: isearch_nonincremental = .false.
    integer(int64) :: isearch_paste_previous_point = 0

    !> isearch.el search-ring-yank-pointer and
    !> regexp-search-ring-yank-pointer: 0-based, -1 for none.
    integer :: search_ring_paste_pointer = -1, regular_expression_search_ring_paste_pointer = -1

    abstract interface
        !> A search function, as `isearch_search_function_function' returns:
        !> search for the first occurrence of STRING from point, BOUND (0
        !> for none) and NOERROR as for `regular_expression_search_forward'.  POSITION is where
        !> the search left point, 0 if it found nothing.  Not a port.
        recursive subroutine isearch_search_procedure(string, bound, noerror, position, status)
            import :: int64, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer(int64), intent(in) :: bound
            logical, intent(in) :: noerror
            integer(int64), intent(out) :: position
            type(status_type), intent(out) :: status
        end subroutine isearch_search_procedure

        !> An `isearch_wrap_function': wrap the search when it has failed,
        !> typically moving point.  Not a port.
        recursive subroutine isearch_wrap_procedure()
            implicit none(type, external)
        end subroutine isearch_wrap_procedure

        !> The function an `isearch_push_state_function' saves: restore
        !> the mode-specific state DATA it saved.  Not a port.
        recursive subroutine isearch_pop_procedure(data)
            implicit none(type, external)
            character(len=*), intent(in) :: data
        end subroutine isearch_pop_procedure

        !> An `isearch_message_function': display the search prompt
        !> (C_Q_HACK and ELLIPSIS as for `isearch_message').  Not a port.
        recursive subroutine isearch_message_procedure(c_q_hack, ellipsis)
            implicit none(type, external)
            logical, intent(in) :: c_q_hack, ellipsis
        end subroutine isearch_message_procedure

        !> An `isearch_filter_test_function': whether the match from BEGINNING to
        !> END is one to stop at.  Not a port.
        recursive logical function isearch_filter_procedure(beginning, end)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
        end function isearch_filter_procedure
    end interface

    abstract interface
        !> An `isearch_search_function_function': set FN to the search
        !> function to use.  Not a port.
        recursive subroutine isearch_search_function_procedure(fn)
            import :: isearch_search_procedure
            implicit none(type, external)
            procedure(isearch_search_procedure), pointer, intent(out) :: fn
        end subroutine isearch_search_function_procedure

        !> An `isearch_push_state_function': save the mode-specific
        !> state as DATA, to be restored by POP.  Not a port.
        recursive subroutine isearch_push_state_procedure(data, pop)
            import :: isearch_pop_procedure
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: data
            procedure(isearch_pop_procedure), pointer, intent(out) :: pop
        end subroutine isearch_push_state_procedure
    end interface

    !> One element of isearch-isearch_commands (isearch.el isearch--state).
    type :: isearch_state_type
        character(len=:), allocatable :: string, message, error
        integer(int64) :: point = 0, other_end = 0, barrier = 0
        logical :: success = .true., forward = .true., wrapped = .false.
        integer :: case_fold = CASE_FOLD_YES
        !> The function restoring the mode-specific state, and that state.
        procedure(isearch_pop_procedure), pointer, nopass :: pop_function => null()
        character(len=:), allocatable :: pop_data
    end type isearch_state_type

    !> One buffer's values of the Isearch function variables.
    type :: local_functions_type
        integer(int64) :: serial = 0
        procedure(isearch_search_function_procedure), pointer, nopass :: search_function => null()
        procedure(isearch_wrap_procedure), pointer, nopass :: wrap => null()
        procedure(isearch_push_state_procedure), pointer, nopass :: push_state => null()
        procedure(isearch_filter_procedure), pointer, nopass :: filter => null()
        procedure(isearch_message_procedure), pointer, nopass :: message => null()
    end type local_functions_type

    !> The buffers' values of the Isearch function variables.
    type(local_functions_type), allocatable :: local_functions(:)

    !> Non-default value overrides the behavior of
    !> `isearch_search_function_default'.  This variable's value should be a
    !> subroutine, which will be called with one argument, and should set
    !> it to a function that takes three arguments: STRING, BOUND, and
    !> NOERROR.  STRING is the string to be searched for.  See
    !> `regular_expression_search_forward' for the meaning of BOUND and NOERROR
    !> arguments.
    !>
    !> This returned function will be used by `isearch_search_string' to
    !> search for the first occurrence of STRING.  Port of isearch.el
    !> isearch-search-fun-function (set with `set_local'; null for
    !> `isearch_search_function_default').
    type, public :: isearch_search_function_function_type
        character(len=32) :: name = "isearch_search_function_function"
    contains
        !> Set the current buffer's value to null (unset).
        procedure, nopass :: clear_local => search_function_clear_local
        !> Make FN the current buffer's value (set_local).
        procedure, nopass :: set_local => search_function_set_local
        !> Set FN to the current buffer's value (null for the default).
        procedure, nopass :: current => current_search_function
    end type isearch_search_function_function_type

    !> Function to call to wrap the search when search is failed.
    !> The function is called with no parameters, and would typically
    !> move point.
    !>
    !> If null, move point to the beginning of the buffer for a forward
    !> search, or to the end of the buffer for a backward search.  Port
    !> of isearch.el isearch-wrap-function (set with `set_local').
    type, public :: isearch_wrap_function_type
        character(len=32) :: name = "isearch_wrap_function"
    contains
        !> Set the current buffer's value to null (unset).
        procedure, nopass :: clear_local => wrap_clear_local
        !> Make FN the current buffer's value (set_local).
        procedure, nopass :: set_local => wrap_set_local
        !> The current buffer's value (null when unset).
        procedure, nopass :: current => current_wrap
    end type isearch_wrap_function_type

    !> Function to save a function restoring the mode-specific Isearch
    !> state to the search status stack.  Port of isearch.el
    !> isearch-push-state-function (set with `set_local').
    type, public :: isearch_push_state_function_type
        character(len=32) :: name = "isearch_push_state_function"
    contains
        !> Set the current buffer's value to null (unset).
        procedure, nopass :: clear_local => push_state_clear_local
        !> Make FN the current buffer's value (set_local).
        procedure, nopass :: set_local => push_state_set_local
        !> The current buffer's value (null when unset).
        procedure, nopass :: current => current_push_state
    end type isearch_push_state_function_type

    !> Test to filter hits of Isearch and replace commands.
    !> Isearch hits that don't satisfy the test will be skipped.
    !> The value should be a function of two arguments; it will be
    !> called with the positions of the start and the end of the text
    !> matched by Isearch and replace commands.  If this function
    !> returns false, Isearch and replace commands will continue
    !> searching without stopping at resp. replacing this match.  Port
    !> of isearch.el isearch-filter-predicate (set with `set_local'; the
    !> default `isearch_filter_visible' is Deferred with search through
    !> invisible text, so null stops at every match).
    type, public :: isearch_filter_test_function_type
        character(len=32) :: name = "isearch_filter_test_function"
    contains
        !> Set the current buffer's value to null (unset).
        procedure, nopass :: clear_local => filter_clear_local
        !> Make FN the current buffer's value (set_local).
        procedure, nopass :: set_local => filter_set_local
        !> The current buffer's value (null for the default).
        procedure, nopass :: current => current_filter
    end type isearch_filter_test_function_type

    !> Function to call to display the search prompt.
    !> If null, use function `isearch_message'.  Port of isearch.el
    !> isearch-message-function (set with `set_local').
    type, public :: isearch_message_function_type
        character(len=32) :: name = "isearch_message_function"
    contains
        !> Set the current buffer's value to null (unset).
        procedure, nopass :: clear_local => message_clear_local
        !> Make FN the current buffer's value (set_local).
        procedure, nopass :: set_local => message_set_local
        !> The current buffer's value (null when unset).
        procedure, nopass :: current => current_message_function
    end type isearch_message_function_type

    !> See the type.  Port of isearch.el isearch-message-function.
    type(isearch_message_function_type), public :: isearch_message_function

    !> A string added to the Isearch prompt (unallocated for none), as
    !> "history " when searching a comint's input history.  Port of
    !> isearch.el isearch-message-prefix-add.
    character(len=:), allocatable, public :: isearch_message_prefix_add

    !> See the type.  Port of isearch.el isearch-search-fun-function.
    type(isearch_search_function_function_type), public :: isearch_search_function_function
    !> See the type.  Port of isearch.el isearch-wrap-function.
    type(isearch_wrap_function_type), public :: isearch_wrap_function
    !> See the type.  Port of isearch.el isearch-push-state-function.
    type(isearch_push_state_function_type), public :: isearch_push_state_function
    !> See the type.  Port of isearch.el isearch-filter-predicate.
    type(isearch_filter_test_function_type), public :: isearch_filter_test_function

    !> Function(s) to call after starting up an incremental search.
    !> Port of isearch.el isearch-mode-hook.
    type(hook_type), target, public :: isearch_mode_hook = hook_type(name="isearch_mode_hook")

    !> Function(s) to call after terminating an incremental search.
    !> When these functions are called, `isearch_mode_end_hook_quit' is
    !> true if the user quits the search.  Port of isearch.el
    !> isearch-mode-end-hook.
    type(hook_type), target, public :: isearch_mode_end_hook = hook_type(name="isearch_mode_end_hook")

    !> True while running `isearch_mode_end_hook' if the user quits the
    !> search.  Port of isearch.el isearch-mode-end-hook-quit.
    logical, public, protected :: isearch_mode_end_hook_quit = .false.

    !> isearch-isearch_commands: the states, newest last.
    type(isearch_state_type), allocatable :: isearch_commands(:)
    integer :: ncmds = 0

    !> isearch--saved-local-map and isearch--saved-overriding-local-map.
    type(handle_type) :: saved_local_map, saved_overriding_local_map
    !> The pre-command hook isearch-mode replaced.

    !> Face for highlighting Isearch matches.  Port of isearch.el isearch
    !> (Isearch's own highlighting is Deferred: the face serves Ispell).
    type(named_face_type), target, public :: isearch

    !> Face for lazy highlighting of matches other than the current one.
    !> Used in Isearch when `isearch_lazy_highlight' is true, and in
    !> `query_replace' when `query_replace_lazy_highlight' is true (both
    !> Deferred: the face serves Occur's current line).  Port of isearch.el
    !> lazy-highlight.
    type(named_face_type), target, public :: lazy_highlight

    !> List of search string sequences.  Port of isearch.el search-ring.
    type(history_setting_type), target, public :: search_ring = history_setting_type(name="search_ring")
    !> List of regular expression search string sequences.  Port of
    !> isearch.el regexp-search-ring.
    type(history_setting_type), target, public :: regular_expression_search_ring = history_setting_type( &
                                                  name="regular_expression_search_ring")

contains

    ! ------------------------------------------------------------------
    ! Strings
    ! ------------------------------------------------------------------

    !> The characters of UTF-8 `s'.  Not a port.
    subroutine decode(s, chars, n)
        character(len=*), intent(in) :: s
        integer(int32), allocatable, intent(out) :: chars(:)
        integer, intent(out) :: n
        integer(int64) :: i
        integer(int32) :: c, clen

        allocate (chars(len(s)))
        n = 0
        i = 1

        do while (i <= len(s))
            call string_character_and_length(s, i, c, clen)
            n = n + 1
            chars(n) = c
            i = i + clen
        end do
    end subroutine decode

    !> Number of characters in UTF-8 `s'.  Not a port.
    integer function nchars(s)
        character(len=*), intent(in) :: s
        integer(int32), allocatable :: chars(:)

        call decode(s, chars, nchars)
    end function nchars

    !> `s' without its last `n' characters.  Not a port.
    function drop_characters(s, n) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: r
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        character(len=:), allocatable :: one
        type(status_type) :: st

        call decode(s, chars, m)
        r = ""

        do k = 1, m - n
            call character_to_string(chars(k), one, st)
            r = r//one
        end do
    end function drop_characters

    !> Port of casefiddle.c:Fdowncase for a string (character by
    !> character through the case table).
    function downcase_string(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        character(len=:), allocatable :: one
        type(status_type) :: st

        call decode(s, chars, m)
        r = ""

        do k = 1, m
            call character_to_string(downcase(chars(k)), one, st)
            r = r//one
        end do
    end function downcase_string

    !> Return true if there are no upper case chars in STRING.
    !> If REGEXP-FLAG is true, disregard letters preceded by `\' (but
    !> not `\\') since they have special meaning in a regexp.  Port of
    !> isearch.el:isearch-no-upper-case-p.
    logical function is_isearch_no_upper_case(string, regular_expression_flag) result(none)
        character(len=*), intent(in) :: string
        logical, intent(in) :: regular_expression_flag
        integer(int32), allocatable :: chars(:)
        integer :: k, m, at
        logical :: quote_flag
        type(status_type) :: status
        integer(int64) :: i

        call decode(string, chars, m)
        none = .true.
        quote_flag = .false.

        do k = 1, m
            if (regular_expression_flag .and. chars(k) == iachar("\")) then
                quote_flag = .not. quote_flag
            else
                if (.not. quote_flag .and. downcase(chars(k)) /= chars(k)) then
                    none = .false.
                    return
                end if

                quote_flag = .false.
            end if
        end do

        ! Even if there's no uppercase char, we want to detect the use of
        ! [:upper:] or [:lower:] char-class, which indicates clearly that
        ! the user cares about case distinction.
        if (regular_expression_flag) then
            at = index(string, "[:upper:]")
            if (at == 0) at = index(string, "[:lower:]")

            if (at > 0) then
                i = string_match(string(1:at - 1), "", status, inhibit_modify=.true.)

                if (status%code == FM_INVALID_REGULAR_EXPRESSION) then
                    if (status%detail == """Unmatched [ or [^""") none = .false.
                end if
            end if
        end if
    end function is_isearch_no_upper_case

    !> Return a regexp which matches words, ignoring punctuation.
    !> Given STRING, a string of words separated by word delimiters,
    !> compute a regexp that matches those exact words separated by
    !> arbitrary punctuation.  If the string begins or ends in whitespace,
    !> the beginning or the end of the string matches arbitrary whitespace.
    !> Otherwise if LAX is .true., the beginning or the end of the string
    !> need not match a word boundary.
    !>
    !> Used in `word_search_forward', `word_search_backward',
    !> `word_search_forward_lax', `word_search_backward_lax'.  Port of
    !> isearch.el:word-search-regexp.
    function word_search_regular_expression(string, lax) result(regular_expression)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: lax
        character(len=:), allocatable :: regular_expression, word, one
        integer(int32), allocatable :: chars(:)
        integer :: k, m, nwords
        logical :: is_lax
        type(status_type) :: st

        is_lax = .false.
        if (present(lax)) is_lax = lax
        regular_expression = ""
        if (len(string) == 0) return
        call decode(string, chars, m)

        if (.not. any([(character_syntax(int(chars(k))) == "w", k=1, m)])) then
            regular_expression = "\W+"
            return
        end if

        if (character_syntax(int(chars(1))) /= "w") then
            regular_expression = "\W+"
        else
            regular_expression = "\<"
        end if

        ! (mapconcat 'regexp-quote (split-string string "\\W+" t) "\\W+")
        nwords = 0
        word = ""

        do k = 1, m + 1
            if (k <= m) then
                if (character_syntax(int(chars(k))) == "w") then
                    call character_to_string(chars(k), one, st)
                    word = word//one
                    cycle
                end if
            end if

            if (len(word) > 0) then
                if (nwords > 0) regular_expression = regular_expression//"\W+"
                regular_expression = regular_expression//regular_expression_quote(word)
                nwords = nwords + 1
                word = ""
            end if
        end do

        if (character_syntax(int(chars(m))) /= "w") then
            regular_expression = regular_expression//"\W+"
        else if (.not. is_lax) then
            regular_expression = regular_expression//"\>"
        end if
    end function word_search_regular_expression

    ! ------------------------------------------------------------------
    ! The search ring
    ! ------------------------------------------------------------------

    !> The name of the search ring for regexp search if `regular_expression', else of
    !> the ordinary search ring.  Not a port.
    function ring_name(regular_expression) result(name)
        logical, intent(in) :: regular_expression
        character(len=:), allocatable :: name

        if (regular_expression) then
            name = "regular_expression_search_ring"
        else
            name = "search_ring"
        end if
    end function ring_name

    !> The search ring (isearch.el search-ring or regexp-search-ring, as
    !> for the current search), most recent first.  Not a port.
    subroutine search_ring_entries(elements)
        type(value_type), allocatable, intent(out) :: elements(:)
        type(value_type) :: ring
        type(status_type) :: status

        call symbol_value(value_type(name=ring_name(isearch_regular_expression)), ring, status)
        if (.not. status%ok()) ring = value_type()
        call list_elements(ring, elements)
    end subroutine search_ring_entries

    !> Add STRING to the beginning of the search ring.
    !> REGULAR_EXPRESSION if .true. says use the regexp search ring.  Port of
    !> isearch.el:isearch-update-ring (no text properties on STRING).
    subroutine isearch_update_ring(string, regular_expression)
        character(len=*), intent(in) :: string
        logical, intent(in) :: regular_expression
        logical :: saved

        saved = history_delete_duplicates
        history_delete_duplicates = .true.
        call add_to_history(ring_name(regular_expression), string, merge(regular_expression_search_ring_maximum, &
                                                                         search_ring_maximum, regular_expression), &
                            keep_all=.true.)
        history_delete_duplicates = saved
    end subroutine isearch_update_ring

    !> Port of isearch.el:isearch-ring-adjust1.
    subroutine isearch_ring_adjust1(advance)
        logical, intent(in) :: advance
        type(value_type), allocatable :: ring(:)
        integer :: length, paste_pointer

        call search_ring_entries(ring)
        length = size(ring)
        if (length == 0) return

        if (isearch_regular_expression) then
            paste_pointer = regular_expression_search_ring_paste_pointer
        else
            paste_pointer = search_ring_paste_pointer
        end if

        if (paste_pointer < 0) paste_pointer = merge(0, -1, advance)
        paste_pointer = modulo(paste_pointer + merge(-1, 1, advance), length)

        if (isearch_regular_expression) then
            regular_expression_search_ring_paste_pointer = paste_pointer
        else
            search_ring_paste_pointer = paste_pointer
        end if

        if (vtag(ring(paste_pointer + 1)) == V_STRING) then
            isearch_string = xstring(ring(paste_pointer + 1))
            isearch_message = isearch_string
        end if
    end subroutine isearch_ring_adjust1

    !> Port of isearch.el:isearch-ring-adjust (`search_ring_update' .false.:
    !> edit the string in the minibuffer).
    subroutine isearch_ring_adjust(advance, status)
        logical, intent(in) :: advance
        type(status_type), intent(out) :: status

        call isearch_ring_adjust1(advance)
        ! Otherwise, edit the search string instead.
        call isearch_edit_string(status)
    end subroutine isearch_ring_adjust

    !> Advance to the next search string in the ring.  Port of
    !> isearch.el:isearch-ring-advance.
    subroutine isearch_ring_advance(status)
        type(status_type), intent(out) :: status

        call isearch_ring_adjust(.true., status)
    end subroutine isearch_ring_advance

    !> Retreat to the previous search string in the ring.  Port of
    !> isearch.el:isearch-ring-retreat.
    subroutine isearch_ring_retreat(status)
        type(status_type), intent(out) :: status

        call isearch_ring_adjust(.false., status)
    end subroutine isearch_ring_retreat

    ! ------------------------------------------------------------------
    ! States
    ! ------------------------------------------------------------------

    !> Port of isearch.el:isearch--get-state.
    recursive function get_state() result(s)
        type(isearch_state_type) :: s

        s%string = isearch_string
        s%message = isearch_message
        s%point = point()
        s%success = isearch_success
        s%forward = isearch_forward
        s%other_end = isearch_other_end
        if (allocated(isearch_error)) s%error = isearch_error
        s%wrapped = isearch_wrapped
        s%barrier = isearch_barrier
        s%case_fold = isearch_case_fold_search

        block
            procedure(isearch_push_state_procedure), pointer :: push
            push => isearch_push_state_function%current()
            if (associated(push)) call push(s%pop_data, s%pop_function)
        end block
    end function get_state

    !> Port of isearch.el:isearch--set-state.
    recursive subroutine set_state(s)
        type(isearch_state_type), intent(in) :: s

        isearch_string = s%string
        isearch_message = s%message
        isearch_success = s%success
        isearch_forward = s%forward
        isearch_other_end = s%other_end
        if (allocated(isearch_error)) deallocate (isearch_error)
        if (allocated(s%error)) isearch_error = s%error
        isearch_wrapped = s%wrapped
        isearch_barrier = s%barrier
        isearch_case_fold_search = s%case_fold
        if (associated(s%pop_function)) call s%pop_function(s%pop_data)
        call goto_character(s%point)
    end subroutine set_state

    ! Not a port.
    logical function same_state(a, b)
        type(isearch_state_type), intent(in) :: a, b

        same_state = a%string == b%string .and. len(a%string) == len(b%string) .and. &
                     a%message == b%message .and. len(a%message) == len(b%message) .and. &
                     a%point == b%point .and. (a%success .eqv. b%success) .and. &
                     (a%forward .eqv. b%forward) .and. a%other_end == b%other_end .and. &
                     (allocated(a%error) .eqv. allocated(b%error)) .and. &
                     (a%wrapped .eqv. b%wrapped) .and. a%barrier == b%barrier .and. &
                     a%case_fold == b%case_fold
    end function same_state

    !> Port of isearch.el:isearch-push-state.
    recursive subroutine isearch_push_state()
        type(isearch_state_type), allocatable :: grown(:)
        integer :: k

        if (.not. allocated(isearch_commands)) allocate (isearch_commands(16))

        if (ncmds == size(isearch_commands)) then
            allocate (grown(2*size(isearch_commands)))

            do k = 1, ncmds
                grown(k) = isearch_commands(k)
            end do

            call move_alloc(grown, isearch_commands)
        end if

        ncmds = ncmds + 1
        isearch_commands(ncmds) = get_state()
    end subroutine isearch_push_state

    !> Port of isearch.el:isearch-pop-state.
    recursive subroutine isearch_pop_state()
        ncmds = ncmds - 1
        if (ncmds > 0) call set_state(isearch_commands(ncmds))
    end subroutine isearch_pop_state

    ! ------------------------------------------------------------------
    ! Starting and ending
    ! ------------------------------------------------------------------

    !> Do incremental search forward.
    !> With a prefix argument, do an incremental regular expression
    !> search instead.
    !> As you type characters, they add to the search string and are
    !> found.  Type DEL to cancel last input item from end of search
    !> string.  Type RET to exit, leaving point at location found.  Type
    !> LFD (C-j) to match end of line.  Type C-s to search again forward,
    !> C-r to search again backward.  Type C-q to quote control character
    !> to search for it.  Type M-c to toggle search case-sensitivity.
    !> Type M-e to edit the search string in the minibuffer.  Other
    !> control and meta characters terminate the search and are then
    !> executed normally.  C-g while searching or when search has failed
    !> cancels input back to what has been found successfully.  C-g when
    !> search is successful aborts and moves point to starting point.
    !> Port of isearch.el:isearch-forward.
    subroutine isearch_forward_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_mode(.true., current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine isearch_forward_command

    !> Do incremental search backward.
    !> With a prefix argument, do a regular expression search instead.
    !> See the command `isearch_forward' for more information.  Port of
    !> isearch.el:isearch-backward.
    subroutine isearch_backward_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_mode(.false., current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine isearch_backward_command

    !> Do incremental search forward for regular expression.
    !> With a prefix argument, do a regular string search instead.
    !> Like ordinary incremental search except that your input is treated
    !> as a regexp.  See the command `isearch_forward' for more
    !> information.
    !>
    !> In incremental searches, a space or spaces normally matches any
    !> whitespace defined by the variable `search_whitespace_regular_expression'.  To
    !> search for a literal space and nothing else, enter C-q SPC.  To
    !> toggle whitespace matching, use `isearch_toggle_lax_whitespace',
    !> usually bound to `M-s SPC' during isearch.  This command does not
    !> support character folding.  Port of
    !> isearch.el:isearch-forward-regexp.
    subroutine isearch_forward_regular_expression_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_mode(.true., current_prefix_argument%kind == PREFIX_NONE)
    end subroutine isearch_forward_regular_expression_command

    !> Do incremental search backward for regular expression.
    !> With a prefix argument, do a regular string search instead.
    !> Like ordinary incremental search except that your input is treated
    !> as a regexp.  See the command `isearch_forward_regular_expression' for more
    !> information.  Port of isearch.el:isearch-backward-regexp.
    subroutine isearch_backward_regular_expression_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_mode(.false., current_prefix_argument%kind == PREFIX_NONE)
    end subroutine isearch_backward_regular_expression_command

    !> Start Isearch minor mode.
    !> It is called by the function `isearch_forward' and other related
    !> functions.  The arg FORWARD means searching in the forward
    !> direction.  The arg REGULAR_EXPRESSION .true. does an incremental regular
    !> expression search.  Port of isearch.el:isearch-mode (no OP-FUN,
    !> RECURSIVE-EDIT or REGEXP-FUNCTION).
    recursive subroutine isearch_mode(forward, regular_expression)
        logical, intent(in) :: forward
        logical, intent(in), optional :: regular_expression
        type(status_type) :: status

        ! Initialize global vars.
        isearch_forward = forward
        isearch_regular_expression = flag(regular_expression)
        isearch_last_case_fold_search = isearch_case_fold_search
        isearch_case_fold_search = merge(CASE_FOLD_YES, CASE_FOLD_NO, case_fold_search%value)
        isearch_string = ""
        isearch_message = ""
        ncmds = 0
        isearch_success = .true.
        isearch_wrapped = .false.
        isearch_barrier = point()
        isearch_adjusted = ADJUSTED_NO
        isearch_paste_flag = .false.
        if (allocated(isearch_error)) deallocate (isearch_error)
        isearch_other_end = 0
        isearch_just_started = .true.
        isearch_original_point = point()
        search_ring_paste_pointer = -1
        regular_expression_search_ring_paste_pointer = -1
        isearch_active = .true.

        ! Prevent successive calls overriding original map in
        ! `isearch_done'.  (Bug#79368)
        if (.not. same_handle(overriding_terminal_local_map, isearch_mode_map)) then
            saved_local_map = overriding_terminal_local_map
        end if

        overriding_terminal_local_map = isearch_mode_map
        call isearch_mode_hook%execute(status)
        ! Remember the initial map possibly modified by external packages
        ! in isearch-mode-hook.  (Bug#16035)
        saved_overriding_local_map = overriding_terminal_local_map
        ! Pushing the initial state used to be before running
        ! isearch-mode-hook, but a hook might set
        ! `isearch_push_state_function' used in `isearch_push_state' to
        ! save mode-specific initial state.  (Bug#4994)
        call isearch_push_state()
        call isearch_update()
        call pre_command_hook%prepend(isearch_pre_command_hook)
    end subroutine isearch_mode

    !> This is called after every isearch command to update the display.
    !> Port of isearch.el:isearch-update (no slow-terminal window, hscroll
    !> or highlighting).
    subroutine isearch_update()
        if (.not. is_input_pending()) call isearch_message_show()
        isearch_adjusted = ADJUSTED_NO
        isearch_paste_flag = .false.
    end subroutine isearch_update

    !> Exit Isearch mode.
    !> Called by all commands that terminate isearch-mode.
    !> For successful search, pass no args.
    !> For a failing search, NOPUSH is t.
    !> For going to the minibuffer to edit the search string,
    !> NOPUSH is t and EDIT is t.
    !> If NOPUSH is .true., we don't push the string on the search ring.
    !> Port of isearch.el:isearch-done (no intangible text).
    recursive subroutine isearch_done(nopush, edit)
        logical, intent(in), optional :: nopush, edit
        type(status_type) :: status

        call pre_command_hook%remove(isearch_pre_command_hook)
        overriding_terminal_local_map = saved_local_map
        saved_local_map = null_handle()
        isearch_active = .false.

        if (len(isearch_string) > 0 .and. .not. flag(nopush)) then
            ! Update the ring data.
            call isearch_update_ring(isearch_string, isearch_regular_expression)
        end if

        isearch_mode_end_hook_quit = flag(nopush) .and. .not. flag(edit)
        call isearch_mode_end_hook%execute(status)
        isearch_mode_end_hook_quit = .false.

        ! If there was movement, mark the starting position.  Maybe should
        ! test difference between and set mark only if > threshold.
        if (point() /= isearch_original_point) then
            if (.not. (transient_mark_mode%value .and. buffers(current)%mark_active)) then
                call push_mark(isearch_original_point, nomsg=.true.)
                if (minibuf_level == 0 .and. .not. flag(edit)) call message("Mark saved where search started")
            end if
        end if
    end subroutine isearch_done

    ! Not a port.
    logical function flag(x)
        logical, intent(in), optional :: x

        flag = .false.
        if (present(x)) flag = x
    end function flag

    !> Exit search normally.
    !> However, if this is the first command after starting incremental
    !> search and `search_nonincremental_instead' is true, do a
    !> nonincremental search instead via `isearch_edit_string'.  Port of
    !> isearch.el:isearch-exit.
    subroutine isearch_exit(status)
        type(status_type), intent(out) :: status
        logical :: saved

        status = status_type()

        if (search_nonincremental_instead .and. len(isearch_string) == 0) then
            saved = isearch_nonincremental
            isearch_nonincremental = .true.
            ! This calls isearch-done as well.
            call isearch_edit_string(status)
            isearch_nonincremental = saved
        else
            call isearch_done()
        end if
    end subroutine isearch_exit

    !> Terminate the search and go back to the starting point.  Port of
    !> isearch.el:isearch-cancel.
    subroutine isearch_cancel(status)
        type(status_type), intent(out) :: status

        call goto_character(isearch_original_point)
        ! Exit isearch..
        call isearch_done(nopush=.true.)
        ! ..and pass on quit signal.
        call set_error(status, QUIT_CODE)
    end subroutine isearch_cancel

    !> Abort incremental search mode if searching is successful, making a
    !> `quit' occur.  Otherwise, revert to previous successful search and continue
    !> searching.  Use `isearch_exit' to quit without that.  Port of
    !> isearch.el:isearch-abort.
    subroutine isearch_abort(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call discard_input()

        if (isearch_success .and. .not. allocated(isearch_error)) then
            ! If search is successful and has no incomplete regexp, move
            ! back to starting point and really do quit.
            isearch_success = .false.
            call isearch_cancel(status)
        else
            ! If search is failing, or has an incomplete regexp, rub out
            ! until it is once more successful.
            do while ((.not. isearch_success .or. allocated(isearch_error)) .and. ncmds > 1)
                call isearch_pop_state()
            end do

            call isearch_update()
        end if
    end subroutine isearch_abort

    !> Decide whether to exit Isearch mode before executing the command.
    !> Don't exit Isearch if the key sequence that invoked this command
    !> is bound in `isearch_mode_map', or if the invoked command is a
    !> prefix argument command.  Otherwise, exit Isearch (when
    !> `search_exit_option' is t) before the command is executed globally
    !> with terminated Isearch.  Port of isearch.el:isearch-pre-command-hook
    !> (isearch-allow-prefix t, no scrolling or motion).
    recursive subroutine isearch_pre_command_hook(hook_status)
        type(status_type), intent(out) :: hook_status
        type(value_type) :: binding
        type(status_type) :: status

        hook_status = status_type()
        ! Don't exit Isearch if we're in the middle of some
        ! `set_transient_map' thingy like `universal_argument__mode'.
        if (.not. same_handle(overriding_terminal_local_map, saved_overriding_local_map)) return

        ! Don't exit Isearch for isearch key bindings.
        if (allocated(this_command_keys)) then
            if (size(this_command_keys) > 0) then
                call lookup_key(make_keymap_reference(isearch_mode_map), this_command_keys, binding, status)
                if (status%ok() .and. vtag(binding) /= V_INTEGER .and. is_command(binding)) return
            end if
        end if

        ! Handle a prefix argument.
        if (associated(this_command, commands%universal_argument) .or. &
            associated(this_command, commands%universal_argument_more) .or. &
            associated(this_command, commands%digit_argument) .or. &
            associated(this_command, commands%negative_argument)) return
        ! Other characters terminate the search and are then executed
        ! normally.
        if (search_exit_option) call isearch_done()
    end subroutine isearch_pre_command_hook

    ! ------------------------------------------------------------------
    ! Messages
    ! ------------------------------------------------------------------

    !> Generate and print the message string.  Port of
    !> isearch.el:isearch-message (no face on the failing part).
    ! (funcall (or isearch-message-function #'isearch-message) C-Q-HACK
    ! ELLIPSIS).
    recursive subroutine isearch_message_show(c_q_hack, ellipsis)
        logical, intent(in), optional :: c_q_hack, ellipsis
        procedure(isearch_message_procedure), pointer :: f

        f => current_message_function()

        if (associated(f)) then
            call f(flag(c_q_hack), flag(ellipsis))
        else
            call isearch_message_display(flag(c_q_hack), flag(ellipsis))
        end if
    end subroutine isearch_message_show

    !> Generate and print the message string.  Port of
    !> isearch.el:isearch-message (no face for the failing part).
    subroutine isearch_message_display(c_q_hack, ellipsis)
        logical, intent(in) :: c_q_hack, ellipsis

        call message_nolog(isearch_message_string(c_q_hack, ellipsis))
    end subroutine isearch_message_display

    ! Port of isearch.el:isearch-message, in part.
    function isearch_message_string(c_q_hack, ellipsis) result(m)
        logical, intent(in) :: c_q_hack
        logical, intent(in), optional :: ellipsis
        character(len=:), allocatable :: m

        m = isearch_message_prefix(isearch_nonincremental, flag(ellipsis))
        if (allocated(isearch_message)) m = m//isearch_text_description(isearch_message)
        m = m//isearch_message_suffix(c_q_hack)
    end function isearch_message_string

    !> Return position of first mismatch in search string, or -1 if
    !> none.  Port of isearch.el:isearch-fail-pos (MESSAGE absent).
    integer(int64) function isearch_fail_position() result(position)
        integer :: k

        position = -1
        if (isearch_success .and. .not. allocated(isearch_error)) return
        k = ncmds

        do while (k >= 1)
            if (isearch_commands(k)%success .and. .not. allocated(isearch_commands(k)%error)) exit
            k = k - 1
        end do

        position = 0
        if (k < 1) return
        if (.not. allocated(isearch_commands(k)%string) .or. .not. allocated(isearch_string)) return

        if (len(isearch_commands(k)%string) < len(isearch_string)) then
            if (isearch_string(1:len(isearch_commands(k)%string)) == isearch_commands(k)%string) position = count_characters_of( &
                                                                                                         isearch_commands(k)%string)
        end if
    end function isearch_fail_position

    !> The number of characters of `s'.  Not a port.
    integer(int64) function count_characters_of(s)
        character(len=*), intent(in) :: s
        integer :: i

        count_characters_of = 0

        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) count_characters_of = count_characters_of + 1
        end do
    end function count_characters_of

    !> How `isearch_message' displays: each character as
    !> isearch-text-char-description shows it, control characters as ^C
    !> and DEL as ^? (upstream puts these on the characters as `display'
    !> properties).  Port of isearch.el:isearch-text-char-description.
    function isearch_text_description(text) result(shown)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: shown
        integer :: i, c

        shown = ""

        do i = 1, len(text)
            c = iachar(text(i:i))

            if (c < 32) then
                shown = shown//"^"//achar(c + 64)
            else if (c == 127) then
                shown = shown//"^?"
            else
                shown = shown//text(i:i)
            end if
        end do
    end function isearch_text_description

    !> The prompt before the search string ("I-search: ", "Failing
    !> wrapped I-search backward: ", ...).  Port of
    !> isearch.el:isearch-message-prefix (no lazy count, multi-buffer or
    !> input method).
    function isearch_message_prefix(nonincremental, ellipsis) result(m)
        logical, intent(in) :: nonincremental
        logical, intent(in), optional :: ellipsis
        character(len=:), allocatable :: m
        integer(int32) :: c
        integer(int64) :: position
        type(status_type) :: status

        ! If about to search, and previous search regexp was invalid, check
        ! that it still is.  If it is valid now, let the message we display
        ! while searching say that it is valid.
        if (allocated(isearch_error) .and. flag(ellipsis)) then
            call regular_expression_search_forward(isearch_string, position, status, bound=point(), noerror=SEARCH_NOERROR)
            if (status%ok()) deallocate (isearch_error)
        end if

        m = ""
        if (.not. isearch_success) m = "failing "
        if (isearch_adjusted == ADJUSTED_PENDING) m = m//"pending "

        if (isearch_wrapped) then
            if (isearch_forward) then
                if (point() > isearch_original_point) m = m//"over"
            else
                if (point() < isearch_original_point) m = m//"over"
            end if

            m = m//"wrapped "
        end if

        if (.not. isearch_success) then
            ! widen-automatically is t.
            associate (b => buffers(current))
                if (b%beginning_of_accessible_portion /= 1 .or. b%end_of_accessible_portion /= b%text%z) m = m//"narrowed "
            end associate

            if (isearch_case_fold_search == CASE_FOLD_NO) m = m//"case-sensitive "
        end if

        ! isearch--describe-regexp-mode.
        if (isearch_regular_expression) m = m//"regexp "
        if (allocated(isearch_message_prefix_add)) m = m//isearch_message_prefix_add

        if (nonincremental) then
            m = m//"search"
        else
            m = m//"I-search"
        end if

        if (.not. isearch_forward) m = m//" backward"
        m = m//": "
        ! Capitalize the first letter (ASCII here).
        c = iachar(m(1:1))
        if (c >= iachar("a") .and. c <= iachar("z")) m(1:1) = achar(c - 32)
    end function isearch_message_prefix

    !> Port of isearch.el:isearch-message-suffix.
    function isearch_message_suffix(c_q_hack) result(m)
        logical, intent(in) :: c_q_hack
        character(len=:), allocatable :: m

        m = ""
        if (c_q_hack) m = "^Q"
        if (allocated(isearch_error)) m = m//" ["//isearch_error//"]"
    end function isearch_message_suffix

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

    !> Return the function to use for the search.
    !> Can be changed via `isearch_search_function_function' for special
    !> needs.  Port of isearch.el:isearch-search-fun.
    recursive function isearch_search_function() result(fn)
        procedure(isearch_search_procedure), pointer :: fn
        procedure(isearch_search_function_procedure), pointer :: f

        call isearch_search_function_function%current(f)

        if (associated(f)) then
            call f(fn)
        else
            fn => isearch_search_function_default()
        end if
    end function isearch_search_function

    !> Return default functions to use for the search.  Port of
    !> isearch.el:isearch-search-fun-default (no `isearch_regular_expression_function').
    function isearch_search_function_default() result(fn)
        procedure(isearch_search_procedure), pointer :: fn

        fn => default_search
    end function isearch_search_function_default

    ! The function isearch-search-fun-default returns.
    ! Port of isearch.el:isearch-search-fun-default (the function it returns).
    recursive subroutine default_search(string, bound, noerror, position, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: bound
        logical, intent(in) :: noerror
        integer(int64), intent(out) :: position
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression, saved_spaces
        integer :: ne
        logical :: had_spaces

        if (isearch_regular_expression) then
            regular_expression = string
        else
            regular_expression = regular_expression_quote(string)
        end if

        ! Use lax versions to not fail at the end of the word while the
        ! user adds and removes characters in the search string (or when
        ! using nonincremental word isearch)
        had_spaces = allocated(search_spaces_regular_expression)
        if (had_spaces) saved_spaces = search_spaces_regular_expression
        if (allocated(search_spaces_regular_expression)) deallocate (search_spaces_regular_expression)
        if (merge(isearch_regular_expression_lax_whitespace, isearch_lax_whitespace, isearch_regular_expression) .and. &
            allocated(search_whitespace_regular_expression)) search_spaces_regular_expression = search_whitespace_regular_expression
        ne = 0
        if (noerror) ne = SEARCH_NOERROR

        if (bound > 0) then
            if (isearch_forward) then
                call regular_expression_search_forward(regular_expression, position, status, bound=bound, noerror=ne)
            else
                call regular_expression_search_backward(regular_expression, position, status, bound=bound, noerror=ne)
            end if
        else if (isearch_forward) then
            call regular_expression_search_forward(regular_expression, position, status, noerror=ne)
        else
            call regular_expression_search_backward(regular_expression, position, status, noerror=ne)
        end if

        if (allocated(search_spaces_regular_expression)) deallocate (search_spaces_regular_expression)
        if (had_spaces) search_spaces_regular_expression = saved_spaces
        if (.not. status%ok()) position = 0
    end subroutine default_search

    !> Search for the first occurrence of STRING.  If found, move point
    !> to the end of the occurrence, update the match data, and return
    !> true.  An error of the search is in `status'.  Port of
    !> isearch.el:isearch-search-string (BOUND absent, NOERROR .true.; no
    !> translation-table-for-input).
    recursive logical function isearch_search_string(string, status) result(found)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        procedure(isearch_search_procedure), pointer :: function_name
        integer(int64) :: position, original_point

        function_name => isearch_search_function()
        original_point = point()
        call function_name(string, 0_int64, .true., position, status)
        found = status%ok() .and. position > 0

        if (found) then
            call goto_character(position)
        else
            call goto_character(original_point)
        end if
    end function isearch_search_string

    ! (funcall isearch-filter-predicate BEGINNING END).  Not a port.
    recursive logical function filter_accepts(beginning, end) result(accepts)
        integer(int64), intent(in) :: beginning, end
        procedure(isearch_filter_procedure), pointer :: f

        f => isearch_filter_test_function%current()
        accepts = .true.
        if (associated(f)) accepts = f(beginning, end)
    end function filter_accepts

    !> Do the search with the current search string.  Port of
    !> isearch.el:isearch-search.
    recursive subroutine isearch_search()
        type(status_type) :: status, st
        logical :: saved_fold, found, retry
        integer(int64) :: mb, me

        if (isearch_case_fold_search == CASE_FOLD_YES .and. .not. search_upper_case%is_off()) then
            isearch_case_fold_search = merge(CASE_FOLD_YES, CASE_FOLD_NO, &
                                             is_isearch_no_upper_case(isearch_string, isearch_regular_expression))
        end if

        saved_fold = case_fold_search%value
        case_fold_search = isearch_case_fold_search /= CASE_FOLD_NO
        if (allocated(isearch_error)) deallocate (isearch_error)
        mb = 0
        me = 0
        retry = .true.

        do while (retry)
            found = isearch_search_string(isearch_string, status)
            ! An error leaves isearch-success as it was.
            if (.not. status%ok()) exit
            isearch_success = found

            ! Clear RETRY unless the search test says to skip this
            ! search hit.
            if (isearch_success) then
                mb = match_beginning(0, st)
                me = match_end(0, st)
            end if

            if (.not. isearch_success) then
                retry = .false.
            else if (filter_accepts(mb, me)) then
                retry = .false.
            else if (mb == me) then
                ! Advance point on empty matches before retrying
                if (merge(is_end_of_buffer(), is_beginning_of_buffer(), isearch_forward)) then
                    retry = .false.
                    isearch_success = .false.
                else
                    call forward_character(st, merge(1_int64, -1_int64, isearch_forward))
                end if
            end if
        end do

        case_fold_search = saved_fold

        if (status%code == FM_INVALID_REGULAR_EXPRESSION) then
            ! (car (cdr lossage)): the message string, without the quotes
            ! the error data print with.
            isearch_error = status%detail(2:len(status%detail) - 1)

            if (index(isearch_error, "Premature ") == 1 .or. index(isearch_error, "Unmatched ") == 1) then
                isearch_error = "incomplete input"
            else if (.not. isearch_regular_expression .and. index(isearch_error, "Regular expression too big") == 1 .and. &
                     isearch_lax_whitespace .and. allocated(search_whitespace_regular_expression)) then
                isearch_error = "Too many spaces for whitespace matching"
            end if
        else if (status%code == SEARCH_FAILED_CODE .or. status%code == USER_SEARCH_FAILED_CODE) then
            isearch_success = .false.
            call failure_data(status)
        else if (.not. status%ok()) then
            ! Stack overflow in regexp search.
            isearch_error = error_message(status%code)
            if (allocated(status%detail)) isearch_error = status%detail
        end if

        isearch_just_started = .false.

        if (isearch_success) then
            if (isearch_forward) then
                isearch_other_end = mb
            else
                isearch_other_end = me
            end if
        else
            ! Ding if failed this time after succeeding last time.
            if (isearch_commands(ncmds)%success) call ding()
            if (associated(isearch_commands(ncmds)%pop_function)) call isearch_commands(ncmds)%pop_function(isearch_commands( &
                                                                                                            ncmds)%pop_data)
            call goto_character(isearch_commands(ncmds)%point)
        end if
    end subroutine isearch_search

    ! (setq isearch-error (nth 2 lossage)): the second datum of a
    ! search-failed error, whose data print as "REGULAR_EXPRESSION", "MESSAGE".
    ! Not a port.
    subroutine failure_data(status)
        type(status_type), intent(in) :: status
        character(len=:), allocatable :: d
        integer :: i, n

        if (.not. allocated(status%detail)) return
        d = status%detail
        n = len(d)
        if (n < 2) return
        if (d(n:n) /= '"') return

        ! The opening quote of the last datum: a quote that is not
        ! escaped, after ", ".
        do i = n - 1, 4, -1
            if (d(i:i) == '"' .and. d(i - 2:i - 1) == ", ") then
                if (d(i - 3:i - 3) == '"') then
                    isearch_error = unquote(d(i + 1:n - 1))
                    return
                end if
            end if
        end do

    contains

        function unquote(q) result(u)
            character(len=*), intent(in) :: q
            character(len=:), allocatable :: u
            integer :: j
            logical :: escape

            u = ""
            escape = .false.

            do j = 1, len(q)
                if (q(j:j) == "\" .and. .not. escape) then
                    escape = .true.
                else
                    u = u//q(j:j)
                    escape = .false.
                end if
            end do
        end function unquote
    end subroutine failure_data

    !> Whether the text at point matches the search string (as a regexp,
    !> or literally), folding case with `fold'; `mend' is where the match
    !> ends.  An invalid regexp doesn't match.  Not a port.
    logical function looking_at_string(string, fold, mend) result(found)
        character(len=*), intent(in) :: string
        logical, intent(in) :: fold
        integer(int64), intent(out) :: mend
        type(status_type) :: status
        logical :: saved_fold

        mend = 0
        saved_fold = case_fold_search%value
        case_fold_search = fold

        if (isearch_regular_expression) then
            found = looking_at(string, status)
        else
            found = looking_at(regular_expression_quote(string), status)
        end if

        case_fold_search = saved_fold
        found = found .and. status%ok()
        if (found) mend = match_end(0, status)
    end function looking_at_string

    !> Do the search and update the display.  Port of
    !> isearch.el:isearch-search-and-update.
    subroutine isearch_search_and_update()
        integer(int64) :: mend
        logical :: fold

        ! Unsuccessful regexp search may become successful by addition of
        ! characters which make isearch-string valid.
        if (isearch_success .or. isearch_regular_expression) then
            ! In reverse search, adding stuff at the end may cause zero or
            ! many more chars to be matched, in the string following point.
            ! Allow all those possibilities without moving point as long as
            ! the match does not extend past search origin.
            fold = isearch_case_fold_search /= CASE_FOLD_NO

            if (isearch_case_fold_search == CASE_FOLD_YES .and. .not. search_upper_case%is_off()) then
                fold = is_isearch_no_upper_case(isearch_string, isearch_regular_expression)
            end if

            if (reverse_match_here()) then
                isearch_success = .true.
                if (allocated(isearch_error)) deallocate (isearch_error)
                isearch_other_end = mend

                if (isearch_case_fold_search == CASE_FOLD_YES .and. .not. search_upper_case%is_off()) then
                    isearch_case_fold_search = merge(CASE_FOLD_YES, CASE_FOLD_NO, &
                                                     is_isearch_no_upper_case(isearch_string, isearch_regular_expression))
                end if
            else
                ! Not regexp, not reverse, or no match at point.
                ! Do the following before moving point.
                call isearch_message_show(ellipsis=.true.)

                if (isearch_other_end /= 0 .and. isearch_adjusted == ADJUSTED_NO) then
                    if (isearch_forward) then
                        call goto_character(isearch_other_end)
                    else
                        call goto_character(min(isearch_original_point, isearch_barrier, isearch_other_end + 1))
                    end if
                end if

                call isearch_search()
            end if
        end if

        call isearch_push_state()
        call isearch_update()

    contains

        logical function reverse_match_here()
            reverse_match_here = .false.
            if (isearch_forward .or. isearch_adjusted /= ADJUSTED_NO) return
            if (.not. looking_at_string(isearch_string, fold, mend)) return
            reverse_match_here = isearch_paste_flag .or. mend <= min(isearch_original_point, isearch_barrier)
        end function reverse_match_here
    end subroutine isearch_search_and_update

    !> Utility for isearch-repeat-forward and isearch-repeat-backward.
    !> Port of isearch.el:isearch-repeat (isearch-wrap-pause t,
    !> `isearch_repeat_on_direction_change' .false.).
    recursive subroutine isearch_repeat(forward, count)
        logical, intent(in) :: forward
        integer(int64), intent(in), optional :: count
        procedure(isearch_wrap_procedure), pointer :: wrap
        type(value_type), allocatable :: ring(:)
        type(status_type) :: status
        integer(int64) :: n
        integer :: outer

        if (isearch_forward .eqv. forward) then
            ! C-s in forward or C-r in reverse.
            if (len(isearch_string) == 0) then
                ! If search string is empty, use last one.
                call search_ring_entries(ring)

                if (size(ring) == 0) then
                    isearch_error = "No previous search string"
                else
                    if (vtag(ring(1)) == V_STRING) isearch_string = xstring(ring(1))
                    isearch_message = isearch_string
                    isearch_case_fold_search = isearch_last_case_fold_search
                    ! After taking the last element, adjust ring to
                    ! previous one.
                    call isearch_ring_adjust1(.false.)
                end if
            else if (.not. isearch_success) then
                ! If already have what to search for, repeat it.
                ! Set isearch-wrapped before calling isearch-wrap-function
                isearch_wrapped = .true.
                wrap => isearch_wrap_function%current()

                if (associated(wrap)) then
                    call wrap()
                else if (isearch_forward) then
                    call goto_character(point_minimum())
                else
                    call goto_character(point_maximum())
                end if
            end if
        else
            ! C-s in reverse or C-r in forward, change direction.
            isearch_forward = .not. isearch_forward
            isearch_success = .true.
        end if

        ! For subsequent \| if regexp.
        isearch_barrier = point()

        if (len(isearch_string) == 0) then
            isearch_success = .true.
        else
            ! For the case when count > 1, don't keep intermediate states
            ! added to isearch-isearch_commands by isearch-push-state in this loop.
            outer = ncmds
            n = 1
            if (present(count)) n = count

            do
                n = n - 1
                if (n < 0) exit

                if (isearch_success .and. point() == isearch_other_end .and. .not. isearch_just_started) then
                    ! If repeating a search that found an empty string,
                    ! ensure we advance.
                    if (merge(is_end_of_buffer(), is_beginning_of_buffer(), isearch_forward)) then
                        ! If there's nowhere to advance to, fail (and wrap
                        ! next time).
                        isearch_success = .false.
                        call ding()
                    else
                        call forward_character(status, merge(1_int64, -1_int64, isearch_forward))
                        call isearch_search()
                    end if
                else
                    call isearch_search()
                end if

                ! Update isearch-isearch_commands, so if isearch-search fails later, it
                ! can restore old successful state from isearch-isearch_commands.
                if (n > 0) call isearch_push_state()
                ! Stop looping on failure.
                if (.not. isearch_success .or. allocated(isearch_error)) n = 0
            end do

            ncmds = outer
        end if

        call isearch_push_state()
        call isearch_update()
    end subroutine isearch_repeat

    !> Repeat incremental search forwards.
    !> With a numeric argument, repeat the search ARGUMENT times.
    !> A negative argument searches backwards.  Port of
    !> isearch.el:isearch-repeat-forward.
    recursive subroutine isearch_repeat_forward(argument)
        type(prefix_argument_type), intent(in) :: argument
        integer(int64) :: count

        if (argument%kind == PREFIX_NONE) then
            call isearch_repeat(.true.)
            return
        end if

        count = prefix_numeric_value(argument)

        if (count < 0) then
            call isearch_repeat_backward(prefix_argument_type(PREFIX_NUMBER, abs(count)))
            ! Reverse the direction back.
            call isearch_repeat(.true.)
        else
            ! Take into account one iteration to reverse direction.
            if (.not. isearch_forward) count = count + 1
            call isearch_repeat(.true., count)
        end if
    end subroutine isearch_repeat_forward

    !> Repeat incremental search backwards.
    !> With a numeric argument, repeat the search ARGUMENT times.
    !> A negative argument searches forwards.  Port of
    !> isearch.el:isearch-repeat-backward.
    recursive subroutine isearch_repeat_backward(argument)
        type(prefix_argument_type), intent(in) :: argument
        integer(int64) :: count

        if (argument%kind == PREFIX_NONE) then
            call isearch_repeat(.false.)
            return
        end if

        count = prefix_numeric_value(argument)

        if (count < 0) then
            call isearch_repeat_forward(prefix_argument_type(PREFIX_NUMBER, abs(count)))
            ! Reverse the direction back.
            call isearch_repeat(.false.)
        else
            ! Take into account one iteration to reverse direction.
            if (isearch_forward) count = count + 1
            call isearch_repeat(.false., count)
        end if
    end subroutine isearch_repeat_backward

    !> Go to the first occurrence of the current search string.
    !> Move point to the beginning of the buffer and search forwards from
    !> the top.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the beginning of the buffer.  Port of
    !> isearch.el:isearch-beginning-of-buffer.
    recursive subroutine isearch_beginning_of_buffer(argument)
        integer(int64), intent(in) :: argument
        logical :: current_forward

        if (argument < 0) then
            call isearch_end_of_buffer(abs(argument))
            return
        end if

        ! For the case when the match is at is_beginning_of_buffer, don't forward char in
        ! isearch-repeat.
        isearch_just_started = .true.
        call goto_character(point_minimum())
        current_forward = isearch_forward
        call isearch_repeat(.true., argument)
        if (.not. (current_forward .eqv. isearch_forward)) call isearch_repeat(current_forward)
    end subroutine isearch_beginning_of_buffer

    !> Go to the last occurrence of the current search string.
    !> Move point to the end of the buffer and search backwards from the
    !> bottom.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the end of the buffer.  Port of
    !> isearch.el:isearch-end-of-buffer.
    recursive subroutine isearch_end_of_buffer(argument)
        integer(int64), intent(in) :: argument
        logical :: current_forward

        if (argument < 0) then
            call isearch_beginning_of_buffer(abs(argument))
            return
        end if

        isearch_just_started = .true.
        call goto_character(point_maximum())
        current_forward = isearch_forward
        call isearch_repeat(.false., argument)
        if (.not. (current_forward .eqv. isearch_forward)) call isearch_repeat(current_forward)
    end subroutine isearch_end_of_buffer

    ! ------------------------------------------------------------------
    ! Editing the search string
    ! ------------------------------------------------------------------

    !> Undo last input item during a search.
    !> An input item is the result of a command that pushes a new state
    !> of isearch to `isearch_commands'.  If no input items have been entered
    !> yet, just beep.  Port of isearch.el:isearch-delete-char.
    subroutine isearch_delete_character()
        if (ncmds <= 1) then
            call ding()
        else
            call isearch_pop_state()
        end if

        call isearch_update()
    end subroutine isearch_delete_character

    !> Delete character from end of search string and search again.
    !> Unlike `isearch_delete_character', it only deletes the last character,
    !> but doesn't cancel the effect of other isearch command.
    !> If search string is empty, just beep.  Port of
    !> isearch.el:isearch-del-char.
    subroutine isearch_delete_end_character(argument)
        integer(int64), intent(in) :: argument

        if (len(isearch_string) == 0) then
            call ding()
        else
            isearch_string = drop_characters(isearch_string, int(min(argument, int(nchars(isearch_string), int64))))
            isearch_message = isearch_string
        end if

        ! Do the following before moving point.
        call isearch_message_show()
        ! Use the isearch-other-end as new starting point to be able to
        ! find the remaining part of the search string again.
        if (isearch_other_end /= 0) call goto_character(isearch_other_end)
        call isearch_search()
        call isearch_push_state()
        call isearch_update()
    end subroutine isearch_delete_end_character

    !> Add this ordinary printing CHAR to the search string and search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-printing-char.
    subroutine isearch_printing_character(char, count)
        integer(int64), intent(in) :: char, count
        integer(int64) :: c

        c = char
        if (c == ior(int(CHARACTER_SHIFT, int64), 32_int64)) c = 32
        call isearch_process_search_character(c, count)
    end subroutine isearch_printing_character

    !> Add CHAR to the search string, COUNT times.
    !> Search is updated accordingly.  Port of
    !> isearch.el:isearch-process-search-char.
    subroutine isearch_process_search_character(char, count)
        integer(int64), intent(in) :: char, count
        character(len=:), allocatable :: one, string
        type(status_type) :: st

        ! * and ? are special in regexps when not preceded by \.  } and |
        ! are special in regexps when preceded by \.  Nothing special for
        ! + because it matches at least once.
        select case (char)
        case (iachar("*"), iachar("?"))
            call isearch_fallback(.false.)
        case (iachar("}"))
            call isearch_fallback(.true., .true.)
        case (iachar("|"))
            call isearch_fallback(.true., .false., .true.)
        case default
            continue
        end select

        call character_to_string(int(char, int32), one, st)
        string = one
        if (count > 1) string = repeat(one, int(count))
        ! Append the char(s) to the search string, update the message and
        ! re-search.
        call isearch_process_search_string(string, string)
    end subroutine isearch_process_search_character

    !> Port of isearch.el:isearch-process-search-string.
    subroutine isearch_process_search_string(string, message)
        character(len=*), intent(in) :: string, message

        isearch_string = isearch_string//string
        isearch_message = isearch_message//message
        call isearch_search_and_update()
    end subroutine isearch_process_search_string

    !> Return .true. if STRING ends in an odd number of backslashes.  Port of
    !> isearch.el:isearch-backslash.
    pure logical function isearch_backslash(string)
        character(len=*), intent(in) :: string
        integer :: n

        n = 0

        do while (n < len(string))
            if (string(len(string) - n:len(string) - n) /= "\") exit
            n = n + 1
        end do

        isearch_backslash = mod(n, 2) == 1
    end function isearch_backslash

    !> Return point to previous successful match to allow regexp
    !> liberalization.  Respects C-s and C-r by stopping at
    !> `isearch_barrier' as needed.
    !>
    !> Do nothing if a backslash is escaping the liberalizing character.
    !> If WANT-BACKSLASH is .true., invert this behavior (for \} and \|).
    !>
    !> Do nothing if regexp has recently been invalid unless optional
    !> ALLOW-INVALID .true..
    !>
    !> If optional TO-BARRIER .true., ignore previous matches and go
    !> exactly to the barrier.  Port of isearch.el:isearch-fallback.
    subroutine isearch_fallback(want_backslash, allow_invalid, to_barrier)
        logical, intent(in) :: want_backslash
        logical, intent(in), optional :: allow_invalid, to_barrier
        integer :: frame, previous
        integer(int64) :: last_other_end
        character(len=:), allocatable :: string
        integer(int32) :: lchar
        logical :: skip

        if (.not. isearch_regular_expression) return
        if (isearch_backslash(isearch_string) .neqv. want_backslash) return

        ! We have to check 2 stack frames because the last might be
        ! invalid just because of a backslash.
        if (allocated(isearch_error) .and. ncmds >= 2 .and. .not. flag(allow_invalid)) then
            if (allocated(isearch_commands(ncmds - 1)%error)) return
        end if

        if (flag(to_barrier)) then
            call goto_character(isearch_barrier)
            isearch_adjusted = ADJUSTED_PENDING
            return
        end if

        ! Walk down the stack looking for a valid regexp (as of course only
        ! they can be the previous successful match); this conveniently
        ! removes all bracket-sets and groups that might be in the way, as
        ! well as partial \{\} constructs that the code below leaves
        ! behind.  Also skip over postfix operators.
        frame = ncmds
        previous = ncmds - 1

        do while (previous >= 1)
            skip = allocated(isearch_commands(frame)%error)

            if (.not. skip) then
                string = isearch_commands(frame)%string
                lchar = 0
                if (len(string) > 0) lchar = iachar(string(len(string):len(string)))

                ! The operators aren't always operators; check backslashes.
                if (isearch_backslash(isearch_commands(previous)%string)) then
                    skip = lchar == iachar("}")
                else
                    skip = lchar == iachar("*") .or. lchar == iachar("?") .or. lchar == iachar("+")
                end if
            end if

            if (.not. skip) exit
            frame = previous
            previous = previous - 1
        end do

        if (frame < 1) return
        ! `frame' now refers the most recent valid regexp that is not at
        ! all optional in its last term.  Now dig one level deeper and find
        ! what matched before that.
        last_other_end = isearch_barrier

        if (previous >= 1) then
            if (isearch_commands(previous)%other_end /= 0) last_other_end = isearch_commands(previous)%other_end
        end if

        if (isearch_forward) then
            call goto_character(max(last_other_end, isearch_barrier))
        else
            call goto_character(min(last_other_end, isearch_barrier))
        end if

        isearch_adjusted = ADJUSTED_PENDING
    end subroutine isearch_fallback

    !> Pull STRING into search string.  Port of
    !> isearch.el:isearch-yank-string.
    subroutine isearch_paste_string(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: s

        s = string

        ! Downcase the string if not supposed to case-fold pasted strings.
        if (isearch_case_fold_search /= CASE_FOLD_NO .and. search_upper_case%choice == SEARCH_UPPER_CASE_NOT_PASTED) then
            s = downcase_string(s)
        end if

        if (isearch_regular_expression) s = regular_expression_quote(s)
        ! Don't move cursor in reverse search.
        isearch_paste_flag = .true.
        call isearch_process_search_string(s, s)
    end subroutine isearch_paste_string

    !> Pull the text from point to the point reached by `jump': 1 moves
    !> forward `argument' characters, 2 to the end of the line (`argument' lines
    !> on), 3 over `argument' words or characters (isearch--yank-char-or-syntax
    !> with word syntax).  Port of isearch.el:isearch-yank-internal.
    subroutine isearch_paste_internal(jump, argument)
        integer, intent(in) :: jump
        integer(int64), intent(in) :: argument
        character(len=:), allocatable :: text
        type(status_type) :: status
        integer(int64) :: saved, from, k

        saved = point()
        if (.not. isearch_forward .and. isearch_other_end /= 0) call goto_character(isearch_other_end)
        if (.not. isearch_success .and. isearch_paste_previous_point /= 0) call goto_character(isearch_paste_previous_point)
        from = point()

        if (jump == 1) then
            call forward_character(status, argument)
        else if (jump == 3) then
            do k = 1, argument
                if (character_syntax(character_after(point())) == "w" .or. character_syntax(character_after(point() + 1)) == "w") &
                    then
                    call forward_word(1_int64)
                else
                    call forward_character(status, 1_int64)
                end if
            end do
        else
            call goto_character(position_end_of_line(merge(argument + 1, argument, is_end_of_line())))
        end if

        isearch_paste_previous_point = point()
        if (isearch_success) isearch_paste_previous_point = 0
        call buffer_substring(from, point(), text, status)
        call goto_character(saved)
        call isearch_paste_string(text)
    end subroutine isearch_paste_internal

    !> Pull next character from buffer into search string.
    !> If optional ARGUMENT is given, pull in the next ARGUMENT characters.  Port of
    !> isearch.el:isearch-yank-char.
    subroutine isearch_paste_character(argument)
        integer(int64), intent(in) :: argument

        call isearch_paste_internal(1, argument)
    end subroutine isearch_paste_character

    !> Pull next character or word from buffer into search string.
    !> If optional ARGUMENT is given, pull in the next ARGUMENT characters/words.
    !> Port of isearch.el:isearch-yank-word-or-char.
    subroutine isearch_paste_word_or_character(argument)
        integer(int64), intent(in) :: argument

        call isearch_paste_internal(3, argument)
    end subroutine isearch_paste_word_or_character

    !> The character at `position', or 0 at the end of the accessible region
    !> ((or (char-after POSITION) 0)).  Not a port.
    integer function character_after(position)
        integer(int64), intent(in) :: position

        character_after = 0

        if (position >= buffers(current)%beginning_of_accessible_portion .and. position < buffers( &
            current)%end_of_accessible_portion) then
            character_after = buf_fetch_char(buffers(current), character_to_byte(position))
        end if
    end function character_after

    !> Pull rest of line from buffer into search string.
    !> If optional ARGUMENT is given, paste the next ARGUMENT lines.  Port of
    !> isearch.el:isearch-yank-line.
    subroutine isearch_paste_line(argument)
        integer(int64), intent(in) :: argument

        call isearch_paste_internal(2, argument)
    end subroutine isearch_paste_line

    !> Pull string from clipboard history into search string.  Port of
    !> isearch.el:isearch-yank-kill.
    subroutine isearch_paste(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        if (.not. isearch_active) call isearch_mode(.true.)
        call current_clipboard_entry(0_int64, text, status)
        if (status%ok()) call isearch_paste_string(text)
    end subroutine isearch_paste

    !> Replace just-pasted search string with previously cut string.
    !> When this command is called not immediately after a
    !> `isearch_paste' or a `isearch_paste_previous_only', it only pops the
    !> last cut string.  Port of isearch.el:isearch-yank-pop-only (no
    !> C-u: reading from the clipboard history needs the minibuffer completion).
    subroutine isearch_paste_previous_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        ! (Upstream's list also has isearch-yank-pop, which is not ported:
        ! Deferred.)
        if (.not. (associated(last_command, commands%isearch_paste) .or. &
                   associated(last_command, commands%isearch_paste_previous_only))) then
            call isearch_paste(status)
        else
            call isearch_pop_state()
            call current_clipboard_entry(1_int64, text, status)
            if (status%ok()) call isearch_paste_string(text)
        end if
    end subroutine isearch_paste_previous_only

    !> Print STRING at the end of the isearch prompt for 1 second.
    !> Port of isearch.el:isearch--momentary-message.
    subroutine isearch_momentary_message(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: m

        m = isearch_message_prefix(isearch_nonincremental)
        if (allocated(isearch_message)) m = m//isearch_text_description(isearch_message)
        call message_nolog(m//" ["//string//"]")
        call sit_for(1000)
    end subroutine isearch_momentary_message

    !> Toggle case folding in searching on or off.
    !> Toggles the value of the variable `isearch_case_fold_search'.
    !> Port of isearch.el:isearch-toggle-case-fold.
    subroutine isearch_toggle_case_fold()
        if (.not. isearch_active) call isearch_mode(.true.)

        if (isearch_case_fold_search /= CASE_FOLD_NO) then
            isearch_case_fold_search = CASE_FOLD_NO
            call isearch_momentary_message("case sensitive")
        else
            isearch_case_fold_search = CASE_FOLD_FORCED
            call isearch_momentary_message("case insensitive")
        end if

        isearch_success = .true.
        isearch_adjusted = ADJUSTED_TOGGLE
        call isearch_update()
    end subroutine isearch_toggle_case_fold

    !> Toggle regexp searching on or off.  Port of
    !> isearch.el:isearch-toggle-regexp.
    subroutine isearch_toggle_regular_expression()
        if (.not. isearch_active) call isearch_mode(.true.)
        isearch_regular_expression = .not. isearch_regular_expression
        isearch_success = .true.
        isearch_adjusted = ADJUSTED_TOGGLE
        call isearch_update()
    end subroutine isearch_toggle_regular_expression

    !> Toggle lax-whitespace searching on or off.
    !> In ordinary search, toggles the value of the variable
    !> `isearch_lax_whitespace'.  In regexp search, toggles the value of
    !> the variable `isearch_regular_expression_lax_whitespace'.  Port of
    !> isearch.el:isearch-toggle-lax-whitespace.
    subroutine isearch_toggle_lax_whitespace()
        logical :: on

        if (.not. isearch_active) call isearch_mode(.true.)

        if (isearch_regular_expression) then
            isearch_regular_expression_lax_whitespace = .not. isearch_regular_expression_lax_whitespace
            on = isearch_regular_expression_lax_whitespace
        else
            isearch_lax_whitespace = .not. isearch_lax_whitespace
            on = isearch_lax_whitespace
        end if

        if (on) then
            call isearch_momentary_message("match spaces loosely")
        else
            call isearch_momentary_message("match spaces literally")
        end if

        isearch_success = .true.
        isearch_adjusted = ADJUSTED_TOGGLE
        call isearch_update()
    end subroutine isearch_toggle_lax_whitespace

    !> Edit the search string in the minibuffer.
    !> RET resumes incremental searching with the edited string.  Port of
    !> isearch.el:isearch-edit-string with with-isearch-suspended (no
    !> minibuffer-local-isearch-map).
    recursive subroutine isearch_edit_string(status)
        type(status_type), intent(out) :: status
        type(isearch_state_type), allocatable :: saved_commands(:)
        type(value_type), allocatable :: ring(:)
        character(len=:), allocatable :: new_string, new_message, saved_error, input
        logical :: new_forward, new_nonincremental, saved_success, saved_wrapped, &
                   saved_paste_flag, had_error, saved_history_append
        integer :: new_case_fold, saved_ncmds, saved_adjusted
        integer(int64) :: saved_barrier, saved_original_point, old_point, old_other_end, fail_position
        integer :: histpos
        type(isearch_state_type) :: now

        status = status_type()
        new_nonincremental = isearch_nonincremental
        ! Locally bind all isearch global variables to protect them from
        ! recursive isearching.
        new_string = isearch_string
        new_message = isearch_message
        new_forward = isearch_forward
        new_case_fold = isearch_case_fold_search
        saved_ncmds = ncmds
        if (ncmds > 0) saved_commands = isearch_commands(1:ncmds)
        saved_success = isearch_success
        saved_wrapped = isearch_wrapped
        saved_barrier = isearch_barrier
        saved_adjusted = isearch_adjusted
        saved_paste_flag = isearch_paste_flag
        had_error = allocated(isearch_error)
        if (had_error) saved_error = isearch_error
        saved_original_point = isearch_original_point
        fail_position = isearch_fail_position()

        if (isearch_regular_expression) then
            histpos = regular_expression_search_ring_paste_pointer + 1
        else
            histpos = search_ring_paste_pointer + 1
        end if

        ! Actually terminate isearching until editing is done.
        call isearch_done(nopush=.true., edit=.true.)
        ! Save old point and isearch-other-end before reading from
        ! minibuffer that can change their values.
        old_point = point()
        old_other_end = isearch_other_end
        ! Don't add a new search string to the search ring here; it should
        ! be added only by `isearch_update_ring' called from `isearch_done'.
        saved_history_append = history_append_new_input
        history_append_new_input = .false.
        if (fail_position < 0) fail_position = count_characters_of(isearch_string)
        call read_from_minibuffer(isearch_message_prefix(isearch_nonincremental), input, status, &
                                  initial_input=isearch_string, position=fail_position + 1, &
                                  history=ring_name(isearch_regular_expression), &
                                  history_position=histpos)
        history_append_new_input = saved_history_append

        if (status%ok()) then
            new_string = input
            new_message = input
        end if

        ! Always resume isearching by restarting it.
        if (.not. isearch_active) call isearch_mode(isearch_forward, isearch_regular_expression)
        ! Copy new local values to isearch globals.
        isearch_string = new_string
        isearch_message = new_message
        isearch_forward = new_forward
        isearch_nonincremental = new_nonincremental
        isearch_case_fold_search = new_case_fold
        ! Restore the minibuffer message before moving point.
        call isearch_message_show()
        ! Set point at the start (end) of old match if forward (backward),
        ! so after exiting minibuffer isearch resumes at the start (end) of
        ! this match and can find it again.
        if (old_other_end /= 0 .and. old_point == point()) call goto_character(old_other_end)

        if (status%ok()) then
            ! Empty isearch-string means use default.
            if (len(isearch_string) == 0) then
                call search_ring_entries(ring)
                isearch_string = ""
                if (size(ring) > 0) isearch_string = xstring(ring(1))
                isearch_message = isearch_string
                ! After taking the last element, adjust ring to previous
                ! one.
                call isearch_ring_adjust1(.false.)
            end if

            ! Reinvoke the pending search.
            call isearch_search()
            ! If no code has changed the search parameters, then pushing a
            ! new state of Isearch should not be necessary.
            now = get_state()

            if (ncmds == 0) then
                call isearch_push_state()
            else if (.not. same_state(isearch_commands(ncmds), now)) then
                call isearch_push_state()
            end if

            call isearch_update()

            if (isearch_nonincremental) then
                call isearch_done()
                ! The search done message is confusing when the string is
                ! empty, so erase it.
                if (len(isearch_string) == 0) call message("")
            end if
        end if

        ! The let's bindings end.
        ncmds = saved_ncmds
        if (saved_ncmds > 0) isearch_commands(1:saved_ncmds) = saved_commands
        isearch_success = saved_success
        isearch_wrapped = saved_wrapped
        isearch_barrier = saved_barrier
        isearch_adjusted = saved_adjusted
        isearch_paste_flag = saved_paste_flag
        if (allocated(isearch_error)) deallocate (isearch_error)
        if (had_error) isearch_error = saved_error
        isearch_original_point = saved_original_point
        ! Handle abort-recursive-edit.
        if (status%code == QUIT_CODE) call isearch_abort(status)
    end subroutine isearch_edit_string

    !> Quote special characters for incremental search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-quote-char.
    subroutine isearch_quote_character(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have
        integer(int64) :: c

        call message_nolog(isearch_message_string(.true.))
        call read_event(ev, have, status)
        if (.not. status%ok() .or. .not. have) return
        c = -1
        if (vtag(ev) == V_INTEGER) c = xfixnum(ev)

        if (c < 0 .or. c > MAXIMUM_CHARACTER) then
            call set_error(status, USER_ERROR_CODE, "Not a valid character")
            return
        end if

        call isearch_process_search_character(c, count)
    end subroutine isearch_quote_character

    ! ------------------------------------------------------------------
    ! Commands and the keymap
    ! ------------------------------------------------------------------

    ! Not a port.
    integer(int64) function numeric_prefix_argument()
        numeric_prefix_argument = prefix_numeric_value(current_prefix_argument)
    end function numeric_prefix_argument

    subroutine execute_isearch_exit(status)
        type(status_type), intent(out) :: status

        call isearch_exit(status)
    end subroutine execute_isearch_exit

    subroutine execute_isearch_cancel(status)
        type(status_type), intent(out) :: status

        call isearch_cancel(status)
    end subroutine execute_isearch_cancel

    subroutine execute_isearch_abort(status)
        type(status_type), intent(out) :: status

        call isearch_abort(status)
    end subroutine execute_isearch_abort

    subroutine execute_isearch_repeat_forward(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_repeat_forward(current_prefix_argument)
    end subroutine execute_isearch_repeat_forward

    subroutine execute_isearch_repeat_backward(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_repeat_backward(current_prefix_argument)
    end subroutine execute_isearch_repeat_backward

    subroutine execute_isearch_delete_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_delete_character()
    end subroutine execute_isearch_delete_character

    subroutine execute_isearch_delete_end_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_delete_end_character(numeric_prefix_argument())
    end subroutine execute_isearch_delete_end_character

    subroutine execute_isearch_printing_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (vtag(last_command_event) /= V_INTEGER) return
        call isearch_printing_character(xfixnum(last_command_event), numeric_prefix_argument())
    end subroutine execute_isearch_printing_character

    subroutine execute_isearch_quote_character(status)
        type(status_type), intent(out) :: status

        call isearch_quote_character(numeric_prefix_argument(), status)
    end subroutine execute_isearch_quote_character

    subroutine execute_isearch_paste_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_paste_character(numeric_prefix_argument())
    end subroutine execute_isearch_paste_character

    subroutine execute_isearch_paste_word_or_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_paste_word_or_character(numeric_prefix_argument())
    end subroutine execute_isearch_paste_word_or_character

    subroutine execute_isearch_paste_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_paste_line(numeric_prefix_argument())
    end subroutine execute_isearch_paste_line

    subroutine execute_isearch_beginning_of_buffer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_beginning_of_buffer(numeric_prefix_argument())
    end subroutine execute_isearch_beginning_of_buffer

    subroutine execute_isearch_end_of_buffer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_end_of_buffer(numeric_prefix_argument())
    end subroutine execute_isearch_end_of_buffer

    subroutine execute_isearch_paste(status)
        type(status_type), intent(out) :: status

        call isearch_paste(status)
    end subroutine execute_isearch_paste

    subroutine execute_isearch_paste_previous_only(status)
        type(status_type), intent(out) :: status

        call isearch_paste_previous_only(status)
    end subroutine execute_isearch_paste_previous_only

    subroutine execute_isearch_toggle_case_fold(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_toggle_case_fold()
    end subroutine execute_isearch_toggle_case_fold

    subroutine execute_isearch_toggle_regular_expression(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_toggle_regular_expression()
    end subroutine execute_isearch_toggle_regular_expression

    subroutine execute_isearch_toggle_lax_whitespace(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call isearch_toggle_lax_whitespace()
    end subroutine execute_isearch_toggle_lax_whitespace

    subroutine execute_isearch_edit_string(status)
        type(status_type), intent(out) :: status

        call isearch_edit_string(status)
    end subroutine execute_isearch_edit_string

    subroutine execute_isearch_ring_advance(status)
        type(status_type), intent(out) :: status

        call isearch_ring_advance(status)
    end subroutine execute_isearch_ring_advance

    subroutine execute_isearch_ring_retreat(status)
        type(status_type), intent(out) :: status

        call isearch_ring_retreat(status)
    end subroutine execute_isearch_ring_retreat

    !> Bind `keys' (kbd syntax) to command `command' in `map'.  Not a port.
    subroutine bind(map, keys, command)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, command
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, value_type(name=command), status)
    end subroutine bind

    !> Register the isearch commands, build `isearch_mode_map', and bind
    !> C-s, C-r, C-M-s and C-M-r in `global_map'.  Port of isearch.el's
    !> Assign WRAPPED to `isearch_wrapped', for an `isearch_wrap_function'.
    subroutine set_isearch_wrapped(wrapped)
        logical, intent(in) :: wrapped

        isearch_wrapped = wrapped
    end subroutine set_isearch_wrapped

    !> Assign POSITION to `isearch_barrier', for a search function.  Not a port.
    subroutine set_isearch_barrier(position)
        integer(int64), intent(in) :: position

        isearch_barrier = position
    end subroutine set_isearch_barrier

    !> Assign POSITION to `isearch_original_point', for a search function.  Not a port.
    subroutine set_isearch_original_point(position)
        integer(int64), intent(in) :: position

        isearch_original_point = position
    end subroutine set_isearch_original_point

    ! ------------------------------------------------------------------
    ! The buffer-local function variables
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry in `local_functions' (0 if none).  Not a port.
    integer function local_slot() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(local_functions)) return
        k = findloc(local_functions%serial, buffers(current)%serial, dim=1)
    end function local_slot

    !> Find or make the current buffer's entry in `local_functions' (reusing a free one), its
    !> index K (0 without a current buffer).  Not a port.
    subroutine make_local_slot(k)
        integer, intent(out) :: k
        type(local_functions_type) :: entry

        k = local_slot()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(local_functions)) allocate (local_functions(0))
        k = findloc(local_functions%serial, 0_int64, dim=1)

        if (k == 0) then
            local_functions = [local_functions, entry]
            k = size(local_functions)
        end if

        local_functions(k)%serial = buffers(current)%serial
    end subroutine make_local_slot

    ! Forget the current buffer's values (kill-all-local-variables).
    ! Not a port.
    subroutine clear_isearch_locals()
        integer :: k

        k = local_slot()
        if (k > 0) local_functions(k) = local_functions_type()
    end subroutine clear_isearch_locals

    ! Not a port.
    subroutine search_function_set_local(fn)
        procedure(isearch_search_function_procedure) :: fn
        integer :: k

        call make_local_slot(k)
        if (k > 0) local_functions(k)%search_function => fn
    end subroutine search_function_set_local

    ! Not a port.
    subroutine current_search_function(fn)
        procedure(isearch_search_function_procedure), pointer, intent(out) :: fn
        integer :: k

        fn => null()
        k = local_slot()
        if (k > 0) fn => local_functions(k)%search_function
    end subroutine current_search_function

    ! Not a port.
    subroutine wrap_set_local(fn)
        procedure(isearch_wrap_procedure) :: fn
        integer :: k

        call make_local_slot(k)
        if (k > 0) local_functions(k)%wrap => fn
    end subroutine wrap_set_local

    ! Not a port.
    function current_wrap() result(fn)
        procedure(isearch_wrap_procedure), pointer :: fn
        integer :: k

        fn => null()
        k = local_slot()
        if (k > 0) fn => local_functions(k)%wrap
    end function current_wrap

    ! Not a port.
    subroutine push_state_set_local(fn)
        procedure(isearch_push_state_procedure) :: fn
        integer :: k

        call make_local_slot(k)
        if (k > 0) local_functions(k)%push_state => fn
    end subroutine push_state_set_local

    ! Not a port.
    function current_push_state() result(fn)
        procedure(isearch_push_state_procedure), pointer :: fn
        integer :: k

        fn => null()
        k = local_slot()
        if (k > 0) fn => local_functions(k)%push_state
    end function current_push_state

    ! Not a port.
    subroutine message_set_local(fn)
        procedure(isearch_message_procedure) :: fn
        integer :: k

        call make_local_slot(k)
        if (k > 0) local_functions(k)%message => fn
    end subroutine message_set_local

    ! Not a port.
    function current_message_function() result(fn)
        procedure(isearch_message_procedure), pointer :: fn
        integer :: k

        fn => null()
        k = local_slot()
        if (k > 0) fn => local_functions(k)%message
    end function current_message_function

    ! Not a port.
    subroutine search_function_clear_local()
        integer :: k

        k = local_slot()
        if (k > 0) local_functions(k)%search_function => null()
    end subroutine search_function_clear_local

    ! Not a port.
    subroutine wrap_clear_local()
        integer :: k

        k = local_slot()
        if (k > 0) local_functions(k)%wrap => null()
    end subroutine wrap_clear_local

    ! Not a port.
    subroutine push_state_clear_local()
        integer :: k

        k = local_slot()
        if (k > 0) local_functions(k)%push_state => null()
    end subroutine push_state_clear_local

    ! Not a port.
    subroutine filter_clear_local()
        integer :: k

        k = local_slot()
        if (k > 0) local_functions(k)%filter => null()
    end subroutine filter_clear_local

    ! Not a port.
    subroutine message_clear_local()
        integer :: k

        k = local_slot()
        if (k > 0) local_functions(k)%message => null()
    end subroutine message_clear_local

    ! Not a port.
    subroutine filter_set_local(fn)
        procedure(isearch_filter_procedure) :: fn
        integer :: k

        call make_local_slot(k)
        if (k > 0) local_functions(k)%filter => fn
    end subroutine filter_set_local

    ! Not a port.
    function current_filter() result(fn)
        procedure(isearch_filter_procedure), pointer :: fn
        integer :: k

        fn => null()
        k = local_slot()
        if (k > 0) fn => local_functions(k)%filter
    end function current_filter

    !> isearch-mode-map and global bindings.  Not a port.
    subroutine initialize_isearch(global_map)
        type(handle_type), intent(in) :: global_map
        type(handle_type) :: meta_map
        type(value_type) :: key(1)
        type(status_type) :: status

        call append_locals_clearer(clear_isearch_locals)
        call isearch%define("isearch", "Face for highlighting Isearch matches.")
        call isearch%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                      backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                         face_attributes(background="magenta3", foreground="lightskyblue1"))
        call isearch%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                      backgrounds=[FACE_BACKGROUND_DARK]), &
                                         face_attributes(background="palevioletred2", foreground="brown4"))
        call isearch%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                         face_attributes(background="magenta4", foreground="cyan1"))
        call isearch%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=8), &
                                         face_attributes(background="magenta4", foreground="cyan1"))
        call isearch%specification_entry(display_when(), face_attributes(inverse_video=.true.))
        call lazy_highlight%define("lazy_highlight", "Face for lazy highlighting of matches other than the current one."// &
                                   new_line("a")//"Used in Isearch when `isearch_lazy_highlight' is true,"// &
                                   new_line("a")//"and in `query_replace' when `query_replace_lazy_highlight' is true.")
        call lazy_highlight%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                             backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                                face_attributes(background="paleturquoise", distant_foreground="black"))
        call lazy_highlight%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                             backgrounds=[FACE_BACKGROUND_DARK]), &
                                                face_attributes(background="paleturquoise4", distant_foreground="white"))
        call lazy_highlight%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                                face_attributes(background="turquoise3", distant_foreground="white"))
        call lazy_highlight%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=8), &
                                                face_attributes(background="turquoise3", distant_foreground="white"))
        call lazy_highlight%specification_entry(display_when(), face_attributes(underline=.true.))
        call forward_setting(search_ring)
        call forward_setting(regular_expression_search_ring)
        commands%isearch_forward%execute => isearch_forward_command
        commands%isearch_backward%execute => isearch_backward_command
        commands%isearch_forward_regular_expression%execute => isearch_forward_regular_expression_command
        commands%isearch_backward_regular_expression%execute => isearch_backward_regular_expression_command
        commands%isearch_toggle_regular_expression%execute => execute_isearch_toggle_regular_expression
        commands%isearch_toggle_lax_whitespace%execute => execute_isearch_toggle_lax_whitespace
        commands%isearch_exit%execute => execute_isearch_exit
        commands%isearch_cancel%execute => execute_isearch_cancel
        commands%isearch_abort%execute => execute_isearch_abort
        commands%isearch_repeat_forward%execute => execute_isearch_repeat_forward
        commands%isearch_repeat_backward%execute => execute_isearch_repeat_backward
        commands%isearch_delete_character%execute => execute_isearch_delete_character
        commands%isearch_delete_end_character%execute => execute_isearch_delete_end_character
        commands%isearch_printing_character%execute => execute_isearch_printing_character
        commands%isearch_quote_character%execute => execute_isearch_quote_character
        commands%isearch_paste_character%execute => execute_isearch_paste_character
        commands%isearch_paste_line%execute => execute_isearch_paste_line
        commands%isearch_paste_word_or_character%execute => execute_isearch_paste_word_or_character
        commands%isearch_beginning_of_buffer%execute => execute_isearch_beginning_of_buffer
        commands%isearch_end_of_buffer%execute => execute_isearch_end_of_buffer
        commands%isearch_toggle_case_fold%execute => execute_isearch_toggle_case_fold
        commands%isearch_edit_string%execute => execute_isearch_edit_string
        commands%isearch_ring_advance%execute => execute_isearch_ring_advance
        commands%isearch_ring_retreat%execute => execute_isearch_ring_retreat
        commands%isearch_paste%execute => execute_isearch_paste
        commands%isearch_paste_previous_only%execute => execute_isearch_paste_previous_only
        if (.not. allocated(search_whitespace_regular_expression)) search_whitespace_regular_expression = "[ "//achar(9)//"]+"
        if (is_keymap_live(isearch_mode_map)) call destroy_keymap(isearch_mode_map, status)
        call make_keymap(isearch_mode_map)
        ! Make all multibyte characters search for themselves.
        call define_key_range(isearch_mode_map, 256_int64, int(MAXIMUM_CHARACTER, int64), &
                              value_type(name="isearch_printing_character"), status)
        ! Single-byte printing chars extend the search string by default.
        call define_key_range(isearch_mode_map, 32_int64, 255_int64, value_type(name="isearch_printing_character"), status)
        ! To handle local bindings with meta char prefix keys, define
        ! another full keymap.
        call make_sparse_keymap(meta_map)
        key(1) = value_type(27_int64)
        call define_key(isearch_mode_map, key, make_keymap_reference(meta_map), status)
        ! Several non-printing chars change the searching behavior.
        call bind(isearch_mode_map, "C-s", "isearch_repeat_forward")
        call bind(isearch_mode_map, "C-r", "isearch_repeat_backward")
        ! Define M-C-s and M-C-r like C-s and C-r so that the same key
        ! combinations can be used to repeat regexp isearches that can be
        ! used to start these searches.
        call bind(isearch_mode_map, "C-M-s", "isearch_repeat_forward")
        call bind(isearch_mode_map, "C-M-r", "isearch_repeat_backward")
        call bind(isearch_mode_map, "DEL", "isearch_delete_character")
        call bind(isearch_mode_map, "<backspace>", "undefined")
        call bind(isearch_mode_map, "C-g", "isearch_abort")
        call bind(isearch_mode_map, "ESC ESC ESC", "isearch_cancel")
        call bind(isearch_mode_map, "C-q", "isearch_quote_character")
        call bind(isearch_mode_map, "RET", "isearch_exit")
        call bind(isearch_mode_map, "<return>", "isearch_exit")
        call bind(isearch_mode_map, "C-j", "isearch_printing_character")
        call bind(isearch_mode_map, "TAB", "isearch_printing_character")
        call bind(isearch_mode_map, "S-SPC", "isearch_printing_character")
        call bind(isearch_mode_map, "C-M-d", "isearch_delete_end_character")
        call bind(isearch_mode_map, "C-w", "isearch_paste_word_or_character")
        call bind(isearch_mode_map, "C-M-y", "isearch_paste_character")
        call bind(isearch_mode_map, "M-s C-e", "isearch_paste_line")
        call bind(isearch_mode_map, "M-s M-<", "isearch_beginning_of_buffer")
        call bind(isearch_mode_map, "M-s M->", "isearch_end_of_buffer")
        call bind(isearch_mode_map, "M-n", "isearch_ring_advance")
        call bind(isearch_mode_map, "M-p", "isearch_ring_retreat")
        call bind(isearch_mode_map, "C-y", "isearch_paste")
        call bind(isearch_mode_map, "M-y", "isearch_paste_previous_only")
        call bind(isearch_mode_map, "M-c", "isearch_toggle_case_fold")
        call bind(isearch_mode_map, "M-s c", "isearch_toggle_case_fold")
        call bind(isearch_mode_map, "M-r", "isearch_toggle_regular_expression")
        call bind(isearch_mode_map, "M-s r", "isearch_toggle_regular_expression")
        call bind(isearch_mode_map, "M-s SPC", "isearch_toggle_lax_whitespace")
        call bind(isearch_mode_map, "M-e", "isearch_edit_string")
        call bind(isearch_mode_map, "M-s e", "isearch_edit_string")
        call bind(global_map, "C-s", "isearch_forward")
        call bind(global_map, "C-r", "isearch_backward")
        call bind(global_map, "C-M-s", "isearch_forward_regular_expression")
        call bind(global_map, "C-M-r", "isearch_backward_regular_expression")
    end subroutine initialize_isearch
end module incremental_search_mod
