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

!> Incremental search: C-s and C-r.  As upstream, `incremental_search_mode' makes
!> `incremental_search_mode_map' the overriding terminal-local map and installs a
!> pre-command hook; keys bound there extend or move the search, and any
!> other command ends the search first and then runs as usual.
!>
!> Literal and regexp search (C-M-s, C-M-r, M-r), with lax whitespace
!> matching (M-s SPC).  Modes change how the search is done with the
!> buffer-local `incremental_search_search_function_function', `incremental_search_wrap_function',
!> `incremental_search_push_state_function' and `incremental_search_filter_test_function', and
!> `incremental_search_mode_hook' and `incremental_search_mode_end_hook' (Info searches through
!> its nodes).  Not ported yet: word, identifier and char-fold search
!> (`incremental_search_regular_expression_function', `search_default_mode'),
!> highlighting the match and lazy highlighting (faces from overlays),
!> C-M-w (identifier syntax: forward-symbol), reading a kill with the
!> minibuffer (`incremental_search_paste_from_clipboard_history', C-u M-y), search through invisible text, scrolling during search
!> (`incremental_search_allow_scroll'), `incremental_search_allow_motion', query-replace and
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
    use buffer_mod, only: buffers, current, transient_mark_mode, buffer_fetch_character
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
    use buffer_local_setting_mod, only: buffer_local_setting_type
    implicit none(type, external)
    private

    public :: incremental_search_forward_command, incremental_search_backward_command, incremental_search_mode, &
              incremental_search_done, &
              incremental_search_update, incremental_search_exit, incremental_search_cancel, incremental_search_abort, &
              incremental_search_repeat_forward, incremental_search_repeat_backward, incremental_search_delete_character, &
              incremental_search_delete_end_character, incremental_search_printing_character, &
              incremental_search_process_search_string, &
              incremental_search_paste_string, incremental_search_paste_character, incremental_search_paste_word_or_character, &
              incremental_search_paste_line, &
              incremental_search_paste, incremental_search_paste_previous_only, &
              incremental_search_beginning_of_buffer, incremental_search_end_of_buffer, incremental_search_ring_advance, &
              incremental_search_ring_retreat, incremental_search_toggle_case_fold, incremental_search_edit_string, &
              incremental_search_message_prefix, initialize_incremental_search, &
              incremental_search_forward_regular_expression_command, &
              incremental_search_backward_regular_expression_command, incremental_search_toggle_regular_expression, &
              incremental_search_toggle_lax_whitespace, &
              is_incremental_search_no_upper_case, word_search_regular_expression, incremental_search_search_function_default, &
              set_incremental_search_wrapped
    public :: incremental_search_search_procedure, incremental_search_search_function_procedure, &
              incremental_search_wrap_procedure, incremental_search_pop_procedure, &
              incremental_search_push_state_procedure, incremental_search_filter_procedure, incremental_search_message_procedure, &
              incremental_search_message_display, &
              set_incremental_search_barrier, set_incremental_search_original_point

    !> Values of `incremental_search_case_fold_search': CASE_FOLD_NO, CASE_FOLD_YES, and CASE_FOLD_YES (set by
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
    !> `incremental_search_lax_whitespace' is false for ordinary incremental search, or
    !> `incremental_search_regular_expression_lax_whitespace' is false for regexp incremental
    !> search, then each space you type matches literally, against one
    !> space.  Port of isearch.el search-whitespace-regexp (made "[ \t]+"
    !> by initialize_incremental_search).
    character(len=:), allocatable, public :: search_whitespace_regular_expression

    !> If true, a space will match a sequence of whitespace chars.
    !> When you enter a space or spaces in ordinary incremental search, it
    !> will match any sequence matched by the regexp defined by the
    !> variable `search_whitespace_regular_expression'.  If the value is false, each
    !> space you type matches literally, against one space.  You can
    !> toggle the value of this variable by the command
    !> `incremental_search_toggle_lax_whitespace', usually bound to `M-s SPC' during
    !> isearch.  Port of isearch.el isearch-lax-whitespace.
    logical, public :: incremental_search_lax_whitespace = .true.

    !> If true, a space will match a sequence of whitespace chars.
    !> When you enter a space or spaces in regexp incremental search, it
    !> will match any sequence matched by the regexp defined by the
    !> variable `search_whitespace_regular_expression'.  If the value is false, each
    !> space you type matches literally, against one space.  You can
    !> toggle the value of this variable by the command
    !> `incremental_search_toggle_lax_whitespace', usually bound to `M-s SPC' during
    !> isearch.  Port of isearch.el isearch-regexp-lax-whitespace.
    logical, public :: incremental_search_regular_expression_lax_whitespace = .false.

    !> isearch.el isearch-mode: whether Isearch mode is on.
    logical, public, protected :: incremental_search_active = .false.

    !> Keymap for `incremental_search_mode'.  Port of isearch.el isearch-mode-map.
    type(handle_type), public, protected :: incremental_search_mode_map

    !> The search string and the message shown for it (isearch.el's
    !> isearch-string and isearch-message), in UTF-8.
    character(len=:), allocatable, public, protected :: incremental_search_string, incremental_search_message
    !> Why the search string fails (isearch-error; unallocated for none).
    character(len=:), allocatable, public, protected :: incremental_search_error
    !> Searching forward (isearch-forward), and whether the search succeeds
    !> (isearch-success).
    logical, public, protected :: incremental_search_forward = .true., incremental_search_success = .true.
    !> Whether the search wrapped (isearch-wrapped).
    logical, public, protected :: incremental_search_wrapped = .false.
    !> Searching for a regexp (isearch-regexp).
    logical, public, protected :: incremental_search_regular_expression = .false.
    !> The other end of the match, where the search started and the
    !> barrier (isearch-other-end, isearch-opoint, isearch-barrier; 0 for
    !> none).
    integer(int64), public, protected :: incremental_search_other_end = 0, incremental_search_original_point = 0, &
                                         incremental_search_barrier = 0
    !> Whether the search ignores case (isearch-case-fold-search; a CASE_FOLD_
    !> value).
    integer, public, protected :: incremental_search_case_fold_search = CASE_FOLD_YES
    integer :: incremental_search_last_case_fold_search = CASE_FOLD_YES
    !> isearch-adjusted: ADJUSTED_NO, ADJUSTED_PENDING (shows "pending"), or ADJUSTED_TOGGLE.
    integer, parameter :: ADJUSTED_NO = 0, ADJUSTED_PENDING = 1, ADJUSTED_TOGGLE = 2
    integer :: incremental_search_adjusted = ADJUSTED_NO
    logical :: incremental_search_paste_flag = .false.
    logical :: incremental_search_just_started = .false.
    !> Whether the search is nonincremental (isearch-nonincremental).
    logical, public, protected :: incremental_search_nonincremental = .false.
    integer(int64) :: incremental_search_paste_previous_point = 0

    !> isearch.el search-ring-yank-pointer and
    !> regexp-search-ring-yank-pointer: 0-based, -1 for none.
    integer :: search_ring_paste_pointer = -1, regular_expression_search_ring_paste_pointer = -1

    abstract interface
        !> A search function, as `incremental_search_search_function_function' returns:
        !> search for the first occurrence of STRING from point, BOUND (0
        !> for none) and NOERROR as for `regular_expression_search_forward'.  POSITION is where
        !> the search left point, 0 if it found nothing.  Not a port.
        recursive subroutine incremental_search_search_procedure(string, bound, noerror, position, status)
            import :: int64, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer(int64), intent(in) :: bound
            logical, intent(in) :: noerror
            integer(int64), intent(out) :: position
            type(status_type), intent(out) :: status
        end subroutine incremental_search_search_procedure

        !> An `incremental_search_wrap_function': wrap the search when it has failed,
        !> typically moving point.  Not a port.
        recursive subroutine incremental_search_wrap_procedure()
            implicit none(type, external)
        end subroutine incremental_search_wrap_procedure

        !> The function an `incremental_search_push_state_function' saves: restore
        !> the mode-specific state DATA it saved.  Not a port.
        recursive subroutine incremental_search_pop_procedure(data)
            implicit none(type, external)
            character(len=*), intent(in) :: data
        end subroutine incremental_search_pop_procedure

        !> An `incremental_search_message_function': display the search prompt
        !> (C_Q_HACK and ELLIPSIS as for `incremental_search_message').  Not a port.
        recursive subroutine incremental_search_message_procedure(c_q_hack, ellipsis)
            implicit none(type, external)
            logical, intent(in) :: c_q_hack, ellipsis
        end subroutine incremental_search_message_procedure

        !> An `incremental_search_filter_test_function': whether the match from BEGINNING to
        !> END is one to stop at.  Not a port.
        recursive logical function incremental_search_filter_procedure(beginning, end)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
        end function incremental_search_filter_procedure
    end interface

    abstract interface
        !> An `incremental_search_search_function_function': set FN to the search
        !> function to use.  Not a port.
        recursive subroutine incremental_search_search_function_procedure(fn)
            import :: incremental_search_search_procedure
            implicit none(type, external)
            procedure(incremental_search_search_procedure), pointer, intent(out) :: fn
        end subroutine incremental_search_search_function_procedure

        !> An `incremental_search_push_state_function': save the mode-specific
        !> state as DATA, to be restored by POP.  Not a port.
        recursive subroutine incremental_search_push_state_procedure(data, pop)
            import :: incremental_search_pop_procedure
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: data
            procedure(incremental_search_pop_procedure), pointer, intent(out) :: pop
        end subroutine incremental_search_push_state_procedure
    end interface

    !> One element of isearch-isearch_commands (isearch.el isearch--state).
    type :: incremental_search_state_type
        character(len=:), allocatable :: string, message, error
        integer(int64) :: point = 0, other_end = 0, barrier = 0
        logical :: success = .true., forward = .true., wrapped = .false.
        integer :: case_fold = CASE_FOLD_YES
        !> The function restoring the mode-specific state, and that state.
        procedure(incremental_search_pop_procedure), pointer, nopass :: pop_function => null()
        character(len=:), allocatable :: pop_data
    end type incremental_search_state_type

    !> One buffer's own value of
    !> `incremental_search_search_function_function'.
    type :: incremental_search_search_function_entry_type
        procedure(incremental_search_search_function_procedure), pointer, nopass :: procedure => null()
    end type incremental_search_search_function_entry_type

    !> The type of
    !> `incremental_search_search_function_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: incremental_search_search_function_setting_type
        procedure(incremental_search_search_function_procedure), pointer, nopass :: procedure => null()
        type(incremental_search_search_function_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_incremental_search_search_function_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_incremental_search_search_function_current
        procedure :: append_local_entry => append_incremental_search_search_function_entry
        procedure :: clear_local_entry => clear_incremental_search_search_function_entry
    end type incremental_search_search_function_setting_type

    !> One buffer's own value of
    !> `incremental_search_wrap_function'.
    type :: incremental_search_wrap_entry_type
        procedure(incremental_search_wrap_procedure), pointer, nopass :: procedure => null()
    end type incremental_search_wrap_entry_type

    !> The type of
    !> `incremental_search_wrap_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: incremental_search_wrap_setting_type
        procedure(incremental_search_wrap_procedure), pointer, nopass :: procedure => null()
        type(incremental_search_wrap_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_incremental_search_wrap_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_incremental_search_wrap_current
        procedure :: append_local_entry => append_incremental_search_wrap_entry
        procedure :: clear_local_entry => clear_incremental_search_wrap_entry
    end type incremental_search_wrap_setting_type

    !> One buffer's own value of
    !> `incremental_search_push_state_function'.
    type :: incremental_search_push_state_entry_type
        procedure(incremental_search_push_state_procedure), pointer, nopass :: procedure => null()
    end type incremental_search_push_state_entry_type

    !> The type of
    !> `incremental_search_push_state_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: incremental_search_push_state_setting_type
        procedure(incremental_search_push_state_procedure), pointer, nopass :: procedure => null()
        type(incremental_search_push_state_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_incremental_search_push_state_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_incremental_search_push_state_current
        procedure :: append_local_entry => append_incremental_search_push_state_entry
        procedure :: clear_local_entry => clear_incremental_search_push_state_entry
    end type incremental_search_push_state_setting_type

    !> One buffer's own value of
    !> `incremental_search_filter_test_function'.
    type :: incremental_search_filter_test_entry_type
        procedure(incremental_search_filter_procedure), pointer, nopass :: procedure => null()
    end type incremental_search_filter_test_entry_type

    !> The type of
    !> `incremental_search_filter_test_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: incremental_search_filter_test_setting_type
        procedure(incremental_search_filter_procedure), pointer, nopass :: procedure => null()
        type(incremental_search_filter_test_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_incremental_search_filter_test_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_incremental_search_filter_test_current
        procedure :: append_local_entry => append_incremental_search_filter_test_entry
        procedure :: clear_local_entry => clear_incremental_search_filter_test_entry
    end type incremental_search_filter_test_setting_type

    !> One buffer's own value of
    !> `incremental_search_message_function'.
    type :: incremental_search_message_entry_type
        procedure(incremental_search_message_procedure), pointer, nopass :: procedure => null()
    end type incremental_search_message_entry_type

    !> The type of
    !> `incremental_search_message_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: incremental_search_message_setting_type
        procedure(incremental_search_message_procedure), pointer, nopass :: procedure => null()
        type(incremental_search_message_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_incremental_search_message_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_incremental_search_message_current
        procedure :: append_local_entry => append_incremental_search_message_entry
        procedure :: clear_local_entry => clear_incremental_search_message_entry
    end type incremental_search_message_setting_type

    !> Function to call to display the search prompt.
    !> If null, use function `incremental_search_message'.  Port of isearch.el
    !> isearch-message-function (`%procedure' is the global
    !> value; `%set_local' gives a buffer its own).
    type(incremental_search_message_setting_type), public :: incremental_search_message_function

    !> A string added to the Isearch prompt (unallocated for none), as
    !> "history " when searching a comint's input history.  Port of
    !> isearch.el isearch-message-prefix-add.
    character(len=:), allocatable, public :: incremental_search_message_prefix_add

    !> Non-default value overrides the behavior of
    !> `incremental_search_search_function_default'.  This variable's value should be a
    !> subroutine, which will be called with one argument, and should set
    !> it to a function that takes three arguments: STRING, BOUND, and
    !> NOERROR.  STRING is the string to be searched for.  See
    !> `regular_expression_search_forward' for the meaning of BOUND and NOERROR
    !> arguments.
    !>
    !> This returned function will be used by `incremental_search_search_string' to
    !> search for the first occurrence of STRING.  Port of isearch.el
    !> isearch-search-fun-function (`%procedure' is the global value, null
    !> for `incremental_search_search_function_default'; `%set_local' gives
    !> a buffer its own).
    type(incremental_search_search_function_setting_type), public :: incremental_search_search_function_function
    !> Function to call to wrap the search when search is failed.
    !> The function is called with no parameters, and would typically
    !> move point.
    !>
    !> If null, move point to the beginning of the buffer for a forward
    !> search, or to the end of the buffer for a backward search.  Port
    !> of isearch.el isearch-wrap-function (`%procedure' is the global
    !> value; `%set_local' gives a buffer its own).
    type(incremental_search_wrap_setting_type), public :: incremental_search_wrap_function
    !> Function to save a function restoring the mode-specific Isearch
    !> state to the search status stack.  Port of isearch.el
    !> isearch-push-state-function (`%procedure' is the global
    !> value; `%set_local' gives a buffer its own).
    type(incremental_search_push_state_setting_type), public :: incremental_search_push_state_function
    !> Test to filter hits of Isearch and replace commands.
    !> Isearch hits that don't satisfy the test will be skipped.
    !> The value should be a function of two arguments; it will be
    !> called with the positions of the start and the end of the text
    !> matched by Isearch and replace commands.  If this function
    !> returns false, Isearch and replace commands will continue
    !> searching without stopping at resp. replacing this match.  Port
    !> of isearch.el isearch-filter-predicate (`%procedure' is the global
    !> value; `%set_local' gives a buffer its own; the default
    !> `incremental_search_filter_visible' is Deferred with search through
    !> invisible text, so null stops at every match).
    type(incremental_search_filter_test_setting_type), public :: incremental_search_filter_test_function

    !> Function(s) to call after starting up an incremental search.
    !> Port of isearch.el isearch-mode-hook.
    type(hook_type), target, public :: incremental_search_mode_hook = hook_type(name="incremental_search_mode_hook")

    !> Function(s) to call after terminating an incremental search.
    !> When these functions are called, `incremental_search_mode_end_hook_quit' is
    !> true if the user quits the search.  Port of isearch.el
    !> isearch-mode-end-hook.
    type(hook_type), target, public :: incremental_search_mode_end_hook = hook_type(name="incremental_search_mode_end_hook")

    !> True while running `incremental_search_mode_end_hook' if the user quits the
    !> search.  Port of isearch.el isearch-mode-end-hook-quit.
    logical, public, protected :: incremental_search_mode_end_hook_quit = .false.

    !> isearch-isearch_commands: the states, newest last.
    type(incremental_search_state_type), allocatable :: incremental_search_commands(:)
    integer :: ncmds = 0

    !> isearch--saved-local-map and isearch--saved-overriding-local-map.
    type(handle_type) :: saved_local_map, saved_overriding_local_map
    !> The pre-command hook isearch-mode replaced.

    !> Face for highlighting Isearch matches.  Port of isearch.el isearch
    !> (Isearch's own highlighting is Deferred: the face serves Ispell).
    type(named_face_type), target, public :: incremental_search

    !> Face for lazy highlighting of matches other than the current one.
    !> Used in Isearch when `incremental_search_lazy_highlight' is true, and in
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
    logical function is_incremental_search_no_upper_case(string, regular_expression_flag) result(none)
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
    end function is_incremental_search_no_upper_case

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

        call symbol_value(value_type(name=ring_name(incremental_search_regular_expression)), ring, status)
        if (.not. status%ok()) ring = value_type()
        call list_elements(ring, elements)
    end subroutine search_ring_entries

    !> Add STRING to the beginning of the search ring.
    !> REGULAR_EXPRESSION if .true. says use the regexp search ring.  Port of
    !> isearch.el:isearch-update-ring (no text properties on STRING).
    subroutine incremental_search_update_ring(string, regular_expression)
        character(len=*), intent(in) :: string
        logical, intent(in) :: regular_expression
        logical :: saved

        saved = history_delete_duplicates
        history_delete_duplicates = .true.
        call add_to_history(ring_name(regular_expression), string, merge(regular_expression_search_ring_maximum, &
                                                                         search_ring_maximum, regular_expression), &
                            keep_all=.true.)
        history_delete_duplicates = saved
    end subroutine incremental_search_update_ring

    !> Port of isearch.el:isearch-ring-adjust1.
    subroutine incremental_search_ring_adjust1(advance)
        logical, intent(in) :: advance
        type(value_type), allocatable :: ring(:)
        integer :: length, paste_pointer

        call search_ring_entries(ring)
        length = size(ring)
        if (length == 0) return

        if (incremental_search_regular_expression) then
            paste_pointer = regular_expression_search_ring_paste_pointer
        else
            paste_pointer = search_ring_paste_pointer
        end if

        if (paste_pointer < 0) paste_pointer = merge(0, -1, advance)
        paste_pointer = modulo(paste_pointer + merge(-1, 1, advance), length)

        if (incremental_search_regular_expression) then
            regular_expression_search_ring_paste_pointer = paste_pointer
        else
            search_ring_paste_pointer = paste_pointer
        end if

        if (vtag(ring(paste_pointer + 1)) == V_STRING) then
            incremental_search_string = xstring(ring(paste_pointer + 1))
            incremental_search_message = incremental_search_string
        end if
    end subroutine incremental_search_ring_adjust1

    !> Port of isearch.el:isearch-ring-adjust (`search_ring_update' .false.:
    !> edit the string in the minibuffer).
    subroutine incremental_search_ring_adjust(advance, status)
        logical, intent(in) :: advance
        type(status_type), intent(out) :: status

        call incremental_search_ring_adjust1(advance)
        ! Otherwise, edit the search string instead.
        call incremental_search_edit_string(status)
    end subroutine incremental_search_ring_adjust

    !> Advance to the next search string in the ring.  Port of
    !> isearch.el:isearch-ring-advance.
    subroutine incremental_search_ring_advance(status)
        type(status_type), intent(out) :: status

        call incremental_search_ring_adjust(.true., status)
    end subroutine incremental_search_ring_advance

    !> Retreat to the previous search string in the ring.  Port of
    !> isearch.el:isearch-ring-retreat.
    subroutine incremental_search_ring_retreat(status)
        type(status_type), intent(out) :: status

        call incremental_search_ring_adjust(.false., status)
    end subroutine incremental_search_ring_retreat

    ! ------------------------------------------------------------------
    ! States
    ! ------------------------------------------------------------------

    !> Port of isearch.el:isearch--get-state.
    recursive function get_state() result(s)
        type(incremental_search_state_type) :: s

        s%string = incremental_search_string
        s%message = incremental_search_message
        s%point = point()
        s%success = incremental_search_success
        s%forward = incremental_search_forward
        s%other_end = incremental_search_other_end
        if (allocated(incremental_search_error)) s%error = incremental_search_error
        s%wrapped = incremental_search_wrapped
        s%barrier = incremental_search_barrier
        s%case_fold = incremental_search_case_fold_search

        block
            procedure(incremental_search_push_state_procedure), pointer :: push
            push => incremental_search_push_state_function%get_current()
            if (associated(push)) call push(s%pop_data, s%pop_function)
        end block
    end function get_state

    !> Port of isearch.el:isearch--set-state.
    recursive subroutine set_state(s)
        type(incremental_search_state_type), intent(in) :: s

        incremental_search_string = s%string
        incremental_search_message = s%message
        incremental_search_success = s%success
        incremental_search_forward = s%forward
        incremental_search_other_end = s%other_end
        if (allocated(incremental_search_error)) deallocate (incremental_search_error)
        if (allocated(s%error)) incremental_search_error = s%error
        incremental_search_wrapped = s%wrapped
        incremental_search_barrier = s%barrier
        incremental_search_case_fold_search = s%case_fold
        if (associated(s%pop_function)) call s%pop_function(s%pop_data)
        call goto_character(s%point)
    end subroutine set_state

    ! Not a port.
    logical function same_state(a, b)
        type(incremental_search_state_type), intent(in) :: a, b

        same_state = a%string == b%string .and. len(a%string) == len(b%string) .and. &
                     a%message == b%message .and. len(a%message) == len(b%message) .and. &
                     a%point == b%point .and. (a%success .eqv. b%success) .and. &
                     (a%forward .eqv. b%forward) .and. a%other_end == b%other_end .and. &
                     (allocated(a%error) .eqv. allocated(b%error)) .and. &
                     (a%wrapped .eqv. b%wrapped) .and. a%barrier == b%barrier .and. &
                     a%case_fold == b%case_fold
    end function same_state

    !> Port of isearch.el:isearch-push-state.
    recursive subroutine incremental_search_push_state()
        type(incremental_search_state_type), allocatable :: grown(:)
        integer :: k

        if (.not. allocated(incremental_search_commands)) allocate (incremental_search_commands(16))

        if (ncmds == size(incremental_search_commands)) then
            allocate (grown(2*size(incremental_search_commands)))

            do k = 1, ncmds
                grown(k) = incremental_search_commands(k)
            end do

            call move_alloc(grown, incremental_search_commands)
        end if

        ncmds = ncmds + 1
        incremental_search_commands(ncmds) = get_state()
    end subroutine incremental_search_push_state

    !> Port of isearch.el:isearch-pop-state.
    recursive subroutine incremental_search_pop_state()
        ncmds = ncmds - 1
        if (ncmds > 0) call set_state(incremental_search_commands(ncmds))
    end subroutine incremental_search_pop_state

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
    subroutine incremental_search_forward_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_mode(.true., current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine incremental_search_forward_command

    !> Do incremental search backward.
    !> With a prefix argument, do a regular expression search instead.
    !> See the command `incremental_search_forward' for more information.  Port of
    !> isearch.el:isearch-backward.
    subroutine incremental_search_backward_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_mode(.false., current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine incremental_search_backward_command

    !> Do incremental search forward for regular expression.
    !> With a prefix argument, do a regular string search instead.
    !> Like ordinary incremental search except that your input is treated
    !> as a regexp.  See the command `incremental_search_forward' for more
    !> information.
    !>
    !> In incremental searches, a space or spaces normally matches any
    !> whitespace defined by the variable `search_whitespace_regular_expression'.  To
    !> search for a literal space and nothing else, enter C-q SPC.  To
    !> toggle whitespace matching, use `incremental_search_toggle_lax_whitespace',
    !> usually bound to `M-s SPC' during isearch.  This command does not
    !> support character folding.  Port of
    !> isearch.el:isearch-forward-regexp.
    subroutine incremental_search_forward_regular_expression_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_mode(.true., current_prefix_argument%kind == PREFIX_NONE)
    end subroutine incremental_search_forward_regular_expression_command

    !> Do incremental search backward for regular expression.
    !> With a prefix argument, do a regular string search instead.
    !> Like ordinary incremental search except that your input is treated
    !> as a regexp.  See the command `incremental_search_forward_regular_expression' for more
    !> information.  Port of isearch.el:isearch-backward-regexp.
    subroutine incremental_search_backward_regular_expression_command(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_mode(.false., current_prefix_argument%kind == PREFIX_NONE)
    end subroutine incremental_search_backward_regular_expression_command

    !> Start Isearch minor mode.
    !> It is called by the function `incremental_search_forward' and other related
    !> functions.  The arg FORWARD means searching in the forward
    !> direction.  The arg REGULAR_EXPRESSION .true. does an incremental regular
    !> expression search.  Port of isearch.el:isearch-mode (no OP-FUN,
    !> RECURSIVE-EDIT or REGEXP-FUNCTION).
    recursive subroutine incremental_search_mode(forward, regular_expression)
        logical, intent(in) :: forward
        logical, intent(in), optional :: regular_expression
        type(status_type) :: status

        ! Initialize global vars.
        incremental_search_forward = forward
        incremental_search_regular_expression = flag(regular_expression)
        incremental_search_last_case_fold_search = incremental_search_case_fold_search
        incremental_search_case_fold_search = merge(CASE_FOLD_YES, CASE_FOLD_NO, case_fold_search%value)
        incremental_search_string = ""
        incremental_search_message = ""
        ncmds = 0
        incremental_search_success = .true.
        incremental_search_wrapped = .false.
        incremental_search_barrier = point()
        incremental_search_adjusted = ADJUSTED_NO
        incremental_search_paste_flag = .false.
        if (allocated(incremental_search_error)) deallocate (incremental_search_error)
        incremental_search_other_end = 0
        incremental_search_just_started = .true.
        incremental_search_original_point = point()
        search_ring_paste_pointer = -1
        regular_expression_search_ring_paste_pointer = -1
        incremental_search_active = .true.

        ! Prevent successive calls overriding original map in
        ! `incremental_search_done'.  (Bug#79368)
        if (.not. same_handle(overriding_terminal_local_map, incremental_search_mode_map)) then
            saved_local_map = overriding_terminal_local_map
        end if

        overriding_terminal_local_map = incremental_search_mode_map
        call incremental_search_mode_hook%execute(status)
        ! Remember the initial map possibly modified by external packages
        ! in isearch-mode-hook.  (Bug#16035)
        saved_overriding_local_map = overriding_terminal_local_map
        ! Pushing the initial state used to be before running
        ! isearch-mode-hook, but a hook might set
        ! `incremental_search_push_state_function' used in `incremental_search_push_state' to
        ! save mode-specific initial state.  (Bug#4994)
        call incremental_search_push_state()
        call incremental_search_update()
        call pre_command_hook%prepend(incremental_search_pre_command_hook)
    end subroutine incremental_search_mode

    !> This is called after every isearch command to update the display.
    !> Port of isearch.el:isearch-update (no slow-terminal window, hscroll
    !> or highlighting).
    subroutine incremental_search_update()
        if (.not. is_input_pending()) call incremental_search_message_show()
        incremental_search_adjusted = ADJUSTED_NO
        incremental_search_paste_flag = .false.
    end subroutine incremental_search_update

    !> Exit Isearch mode.
    !> Called by all commands that terminate isearch-mode.
    !> For successful search, pass no args.
    !> For a failing search, NOPUSH is t.
    !> For going to the minibuffer to edit the search string,
    !> NOPUSH is t and EDIT is t.
    !> If NOPUSH is .true., we don't push the string on the search ring.
    !> Port of isearch.el:isearch-done (no intangible text).
    recursive subroutine incremental_search_done(nopush, edit)
        logical, intent(in), optional :: nopush, edit
        type(status_type) :: status

        call pre_command_hook%remove(incremental_search_pre_command_hook)
        overriding_terminal_local_map = saved_local_map
        saved_local_map = null_handle()
        incremental_search_active = .false.

        if (len(incremental_search_string) > 0 .and. .not. flag(nopush)) then
            ! Update the ring data.
            call incremental_search_update_ring(incremental_search_string, incremental_search_regular_expression)
        end if

        incremental_search_mode_end_hook_quit = flag(nopush) .and. .not. flag(edit)
        call incremental_search_mode_end_hook%execute(status)
        incremental_search_mode_end_hook_quit = .false.

        ! If there was movement, mark the starting position.  Maybe should
        ! test difference between and set mark only if > threshold.
        if (point() /= incremental_search_original_point) then
            if (.not. (transient_mark_mode%value .and. buffers(current)%mark_active)) then
                call push_mark(incremental_search_original_point, nomsg=.true.)
                if (minibuf_level == 0 .and. .not. flag(edit)) call message("Mark saved where search started")
            end if
        end if
    end subroutine incremental_search_done

    ! Not a port.
    logical function flag(x)
        logical, intent(in), optional :: x

        flag = .false.
        if (present(x)) flag = x
    end function flag

    !> Exit search normally.
    !> However, if this is the first command after starting incremental
    !> search and `search_nonincremental_instead' is true, do a
    !> nonincremental search instead via `incremental_search_edit_string'.  Port of
    !> isearch.el:isearch-exit.
    subroutine incremental_search_exit(status)
        type(status_type), intent(out) :: status
        logical :: saved

        status = status_type()

        if (search_nonincremental_instead .and. len(incremental_search_string) == 0) then
            saved = incremental_search_nonincremental
            incremental_search_nonincremental = .true.
            ! This calls isearch-done as well.
            call incremental_search_edit_string(status)
            incremental_search_nonincremental = saved
        else
            call incremental_search_done()
        end if
    end subroutine incremental_search_exit

    !> Terminate the search and go back to the starting point.  Port of
    !> isearch.el:isearch-cancel.
    subroutine incremental_search_cancel(status)
        type(status_type), intent(out) :: status

        call goto_character(incremental_search_original_point)
        ! Exit isearch..
        call incremental_search_done(nopush=.true.)
        ! ..and pass on quit signal.
        call set_error(status, QUIT_CODE)
    end subroutine incremental_search_cancel

    !> Abort incremental search mode if searching is successful, making a
    !> `quit' occur.  Otherwise, revert to previous successful search and continue
    !> searching.  Use `incremental_search_exit' to quit without that.  Port of
    !> isearch.el:isearch-abort.
    subroutine incremental_search_abort(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call discard_input()

        if (incremental_search_success .and. .not. allocated(incremental_search_error)) then
            ! If search is successful and has no incomplete regexp, move
            ! back to starting point and really do quit.
            incremental_search_success = .false.
            call incremental_search_cancel(status)
        else
            ! If search is failing, or has an incomplete regexp, rub out
            ! until it is once more successful.
            do while ((.not. incremental_search_success .or. allocated(incremental_search_error)) .and. ncmds > 1)
                call incremental_search_pop_state()
            end do

            call incremental_search_update()
        end if
    end subroutine incremental_search_abort

    !> Decide whether to exit Isearch mode before executing the command.
    !> Don't exit Isearch if the key sequence that invoked this command
    !> is bound in `incremental_search_mode_map', or if the invoked command is a
    !> prefix argument command.  Otherwise, exit Isearch (when
    !> `search_exit_option' is t) before the command is executed globally
    !> with terminated Isearch.  Port of isearch.el:isearch-pre-command-hook
    !> (isearch-allow-prefix t, no scrolling or motion).
    recursive subroutine incremental_search_pre_command_hook(hook_status)
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
                call lookup_key(make_keymap_reference(incremental_search_mode_map), this_command_keys, binding, status)
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
        if (search_exit_option) call incremental_search_done()
    end subroutine incremental_search_pre_command_hook

    ! ------------------------------------------------------------------
    ! Messages
    ! ------------------------------------------------------------------

    !> Generate and print the message string.  Port of
    !> isearch.el:isearch-message (no face on the failing part).
    ! (funcall (or isearch-message-function #'isearch-message) C-Q-HACK
    ! ELLIPSIS).
    recursive subroutine incremental_search_message_show(c_q_hack, ellipsis)
        logical, intent(in), optional :: c_q_hack, ellipsis
        procedure(incremental_search_message_procedure), pointer :: f

        f => incremental_search_message_function%get_current()

        if (associated(f)) then
            call f(flag(c_q_hack), flag(ellipsis))
        else
            call incremental_search_message_display(flag(c_q_hack), flag(ellipsis))
        end if
    end subroutine incremental_search_message_show

    !> Generate and print the message string.  Port of
    !> isearch.el:isearch-message (no face for the failing part).
    subroutine incremental_search_message_display(c_q_hack, ellipsis)
        logical, intent(in) :: c_q_hack, ellipsis

        call message_nolog(incremental_search_message_string(c_q_hack, ellipsis))
    end subroutine incremental_search_message_display

    ! Port of isearch.el:isearch-message, in part.
    function incremental_search_message_string(c_q_hack, ellipsis) result(m)
        logical, intent(in) :: c_q_hack
        logical, intent(in), optional :: ellipsis
        character(len=:), allocatable :: m

        m = incremental_search_message_prefix(incremental_search_nonincremental, flag(ellipsis))
        if (allocated(incremental_search_message)) m = m//incremental_search_text_description(incremental_search_message)
        m = m//incremental_search_message_suffix(c_q_hack)
    end function incremental_search_message_string

    !> Return position of first mismatch in search string, or -1 if
    !> none.  Port of isearch.el:isearch-fail-pos (MESSAGE absent).
    integer(int64) function incremental_search_fail_position() result(position)
        integer :: k

        position = -1
        if (incremental_search_success .and. .not. allocated(incremental_search_error)) return
        k = ncmds

        do while (k >= 1)
            if (incremental_search_commands(k)%success .and. .not. allocated(incremental_search_commands(k)%error)) exit
            k = k - 1
        end do

        position = 0
        if (k < 1) return
        if (.not. allocated(incremental_search_commands(k)%string) .or. .not. allocated(incremental_search_string)) return

        if (len(incremental_search_commands(k)%string) < len(incremental_search_string)) then
          if (incremental_search_string(1:len(incremental_search_commands(k)%string)) == incremental_search_commands(k)%string) then
                position = count_characters_of( &
                           incremental_search_commands(k)%string)
            end if
        end if
    end function incremental_search_fail_position

    !> The number of characters of `s'.  Not a port.
    integer(int64) function count_characters_of(s)
        character(len=*), intent(in) :: s
        integer :: i

        count_characters_of = 0

        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) count_characters_of = count_characters_of + 1
        end do
    end function count_characters_of

    !> How `incremental_search_message' displays: each character as
    !> isearch-text-char-description shows it, control characters as ^C
    !> and DEL as ^? (upstream puts these on the characters as `display'
    !> properties).  Port of isearch.el:isearch-text-char-description.
    function incremental_search_text_description(text) result(shown)
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
    end function incremental_search_text_description

    !> The prompt before the search string ("I-search: ", "Failing
    !> wrapped I-search backward: ", ...).  Port of
    !> isearch.el:isearch-message-prefix (no lazy count, multi-buffer or
    !> input method).
    function incremental_search_message_prefix(nonincremental, ellipsis) result(m)
        logical, intent(in) :: nonincremental
        logical, intent(in), optional :: ellipsis
        character(len=:), allocatable :: m
        integer(int32) :: c
        integer(int64) :: position
        type(status_type) :: status

        ! If about to search, and previous search regexp was invalid, check
        ! that it still is.  If it is valid now, let the message we display
        ! while searching say that it is valid.
        if (allocated(incremental_search_error) .and. flag(ellipsis)) then
            call regular_expression_search_forward(incremental_search_string, position, status, bound=point(), &
                                                   noerror=SEARCH_NOERROR)
            if (status%ok()) deallocate (incremental_search_error)
        end if

        m = ""
        if (.not. incremental_search_success) m = "failing "
        if (incremental_search_adjusted == ADJUSTED_PENDING) m = m//"pending "

        if (incremental_search_wrapped) then
            if (incremental_search_forward) then
                if (point() > incremental_search_original_point) m = m//"over"
            else
                if (point() < incremental_search_original_point) m = m//"over"
            end if

            m = m//"wrapped "
        end if

        if (.not. incremental_search_success) then
            ! widen-automatically is t.
            associate (b => buffers(current))
                if (b%beginning_of_accessible_portion /= 1 .or. b%end_of_accessible_portion /= b%text%z) m = m//"narrowed "
            end associate

            if (incremental_search_case_fold_search == CASE_FOLD_NO) m = m//"case-sensitive "
        end if

        ! isearch--describe-regexp-mode.
        if (incremental_search_regular_expression) m = m//"regexp "
        if (allocated(incremental_search_message_prefix_add)) m = m//incremental_search_message_prefix_add

        if (nonincremental) then
            m = m//"search"
        else
            m = m//"I-search"
        end if

        if (.not. incremental_search_forward) m = m//" backward"
        m = m//": "
        ! Capitalize the first letter (ASCII here).
        c = iachar(m(1:1))
        if (c >= iachar("a") .and. c <= iachar("z")) m(1:1) = achar(c - 32)
    end function incremental_search_message_prefix

    !> Port of isearch.el:isearch-message-suffix.
    function incremental_search_message_suffix(c_q_hack) result(m)
        logical, intent(in) :: c_q_hack
        character(len=:), allocatable :: m

        m = ""
        if (c_q_hack) m = "^Q"
        if (allocated(incremental_search_error)) m = m//" ["//incremental_search_error//"]"
    end function incremental_search_message_suffix

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

    !> Return the function to use for the search.
    !> Can be changed via `incremental_search_search_function_function' for special
    !> needs.  Port of isearch.el:isearch-search-fun.
    recursive function incremental_search_search_function() result(fn)
        procedure(incremental_search_search_procedure), pointer :: fn
        procedure(incremental_search_search_function_procedure), pointer :: f

        call incremental_search_search_function_function%get_current(f)

        if (associated(f)) then
            call f(fn)
        else
            fn => incremental_search_search_function_default()
        end if
    end function incremental_search_search_function

    !> Return default functions to use for the search.  Port of
    !> isearch.el:isearch-search-fun-default (no `incremental_search_regular_expression_function').
    function incremental_search_search_function_default() result(fn)
        procedure(incremental_search_search_procedure), pointer :: fn

        fn => default_search
    end function incremental_search_search_function_default

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

        if (incremental_search_regular_expression) then
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
        if (merge(incremental_search_regular_expression_lax_whitespace, incremental_search_lax_whitespace, &
                  incremental_search_regular_expression) .and. &
            allocated(search_whitespace_regular_expression)) search_spaces_regular_expression = search_whitespace_regular_expression
        ne = 0
        if (noerror) ne = SEARCH_NOERROR

        if (bound > 0) then
            if (incremental_search_forward) then
                call regular_expression_search_forward(regular_expression, position, status, bound=bound, noerror=ne)
            else
                call regular_expression_search_backward(regular_expression, position, status, bound=bound, noerror=ne)
            end if
        else if (incremental_search_forward) then
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
    recursive logical function incremental_search_search_string(string, status) result(found)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        procedure(incremental_search_search_procedure), pointer :: function_name
        integer(int64) :: position, original_point

        function_name => incremental_search_search_function()
        original_point = point()
        call function_name(string, 0_int64, .true., position, status)
        found = status%ok() .and. position > 0

        if (found) then
            call goto_character(position)
        else
            call goto_character(original_point)
        end if
    end function incremental_search_search_string

    ! (funcall isearch-filter-predicate BEGINNING END).  Not a port.
    recursive logical function filter_accepts(beginning, end) result(accepts)
        integer(int64), intent(in) :: beginning, end
        procedure(incremental_search_filter_procedure), pointer :: f

        f => incremental_search_filter_test_function%get_current()
        accepts = .true.
        if (associated(f)) accepts = f(beginning, end)
    end function filter_accepts

    !> Do the search with the current search string.  Port of
    !> isearch.el:isearch-search.
    recursive subroutine incremental_search_search()
        type(status_type) :: status, st
        logical :: saved_fold, found, retry
        integer(int64) :: mb, me

        if (incremental_search_case_fold_search == CASE_FOLD_YES .and. .not. search_upper_case%is_off()) then
            incremental_search_case_fold_search = merge(CASE_FOLD_YES, CASE_FOLD_NO, &
                                                        is_incremental_search_no_upper_case(incremental_search_string, &
                                                                                            incremental_search_regular_expression))
        end if

        saved_fold = case_fold_search%value
        case_fold_search = incremental_search_case_fold_search /= CASE_FOLD_NO
        if (allocated(incremental_search_error)) deallocate (incremental_search_error)
        mb = 0
        me = 0
        retry = .true.

        do while (retry)
            found = incremental_search_search_string(incremental_search_string, status)
            ! An error leaves isearch-success as it was.
            if (.not. status%ok()) exit
            incremental_search_success = found

            ! Clear RETRY unless the search test says to skip this
            ! search hit.
            if (incremental_search_success) then
                mb = match_beginning(0, st)
                me = match_end(0, st)
            end if

            if (.not. incremental_search_success) then
                retry = .false.
            else if (filter_accepts(mb, me)) then
                retry = .false.
            else if (mb == me) then
                ! Advance point on empty matches before retrying
                if (merge(is_end_of_buffer(), is_beginning_of_buffer(), incremental_search_forward)) then
                    retry = .false.
                    incremental_search_success = .false.
                else
                    call forward_character(st, merge(1_int64, -1_int64, incremental_search_forward))
                end if
            end if
        end do

        case_fold_search = saved_fold

        if (status%code == FM_INVALID_REGULAR_EXPRESSION) then
            ! (car (cdr lossage)): the message string, without the quotes
            ! the error data print with.
            incremental_search_error = status%detail(2:len(status%detail) - 1)

            if (index(incremental_search_error, "Premature ") == 1 .or. index(incremental_search_error, "Unmatched ") == 1) then
                incremental_search_error = "incomplete input"
            else if (.not. incremental_search_regular_expression .and. index(incremental_search_error, &
                                                                             "Regular expression too big") == 1 .and. &
                     incremental_search_lax_whitespace .and. allocated(search_whitespace_regular_expression)) then
                incremental_search_error = "Too many spaces for whitespace matching"
            end if
        else if (status%code == SEARCH_FAILED_CODE .or. status%code == USER_SEARCH_FAILED_CODE) then
            incremental_search_success = .false.
            call failure_data(status)
        else if (.not. status%ok()) then
            ! Stack overflow in regexp search.
            incremental_search_error = error_message(status%code)
            if (allocated(status%detail)) incremental_search_error = status%detail
        end if

        incremental_search_just_started = .false.

        if (incremental_search_success) then
            if (incremental_search_forward) then
                incremental_search_other_end = mb
            else
                incremental_search_other_end = me
            end if
        else
            ! Ding if failed this time after succeeding last time.
            if (incremental_search_commands(ncmds)%success) call ding()
            if (associated(incremental_search_commands(ncmds)%pop_function)) call incremental_search_commands(ncmds)%pop_function( &
                incremental_search_commands( &
                ncmds)%pop_data)
            call goto_character(incremental_search_commands(ncmds)%point)
        end if
    end subroutine incremental_search_search

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
                    incremental_search_error = unquote(d(i + 1:n - 1))
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

        if (incremental_search_regular_expression) then
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
    subroutine incremental_search_search_and_update()
        integer(int64) :: mend
        logical :: fold

        ! Unsuccessful regexp search may become successful by addition of
        ! characters which make isearch-string valid.
        if (incremental_search_success .or. incremental_search_regular_expression) then
            ! In reverse search, adding stuff at the end may cause zero or
            ! many more chars to be matched, in the string following point.
            ! Allow all those possibilities without moving point as long as
            ! the match does not extend past search origin.
            fold = incremental_search_case_fold_search /= CASE_FOLD_NO

            if (incremental_search_case_fold_search == CASE_FOLD_YES .and. .not. search_upper_case%is_off()) then
                fold = is_incremental_search_no_upper_case(incremental_search_string, incremental_search_regular_expression)
            end if

            if (reverse_match_here()) then
                incremental_search_success = .true.
                if (allocated(incremental_search_error)) deallocate (incremental_search_error)
                incremental_search_other_end = mend

                if (incremental_search_case_fold_search == CASE_FOLD_YES .and. .not. search_upper_case%is_off()) then
                    incremental_search_case_fold_search = merge(CASE_FOLD_YES, CASE_FOLD_NO, &
                                                                is_incremental_search_no_upper_case(incremental_search_string, &
                                                                                             incremental_search_regular_expression))
                end if
            else
                ! Not regexp, not reverse, or no match at point.
                ! Do the following before moving point.
                call incremental_search_message_show(ellipsis=.true.)

                if (incremental_search_other_end /= 0 .and. incremental_search_adjusted == ADJUSTED_NO) then
                    if (incremental_search_forward) then
                        call goto_character(incremental_search_other_end)
                    else
                        call goto_character(min(incremental_search_original_point, incremental_search_barrier, &
                                                incremental_search_other_end + 1))
                    end if
                end if

                call incremental_search_search()
            end if
        end if

        call incremental_search_push_state()
        call incremental_search_update()

    contains

        logical function reverse_match_here()
            reverse_match_here = .false.
            if (incremental_search_forward .or. incremental_search_adjusted /= ADJUSTED_NO) return
            if (.not. looking_at_string(incremental_search_string, fold, mend)) return
            reverse_match_here = incremental_search_paste_flag .or. mend <= min(incremental_search_original_point, &
                                                                                incremental_search_barrier)
        end function reverse_match_here
    end subroutine incremental_search_search_and_update

    !> Utility for isearch-repeat-forward and isearch-repeat-backward.
    !> Port of isearch.el:isearch-repeat (isearch-wrap-pause t,
    !> `incremental_search_repeat_on_direction_change' .false.).
    recursive subroutine incremental_search_repeat(forward, count)
        logical, intent(in) :: forward
        integer(int64), intent(in), optional :: count
        procedure(incremental_search_wrap_procedure), pointer :: wrap
        type(value_type), allocatable :: ring(:)
        type(status_type) :: status
        integer(int64) :: n
        integer :: outer

        if (incremental_search_forward .eqv. forward) then
            ! C-s in forward or C-r in reverse.
            if (len(incremental_search_string) == 0) then
                ! If search string is empty, use last one.
                call search_ring_entries(ring)

                if (size(ring) == 0) then
                    incremental_search_error = "No previous search string"
                else
                    if (vtag(ring(1)) == V_STRING) incremental_search_string = xstring(ring(1))
                    incremental_search_message = incremental_search_string
                    incremental_search_case_fold_search = incremental_search_last_case_fold_search
                    ! After taking the last element, adjust ring to
                    ! previous one.
                    call incremental_search_ring_adjust1(.false.)
                end if
            else if (.not. incremental_search_success) then
                ! If already have what to search for, repeat it.
                ! Set isearch-wrapped before calling isearch-wrap-function
                incremental_search_wrapped = .true.
                wrap => incremental_search_wrap_function%get_current()

                if (associated(wrap)) then
                    call wrap()
                else if (incremental_search_forward) then
                    call goto_character(point_minimum())
                else
                    call goto_character(point_maximum())
                end if
            end if
        else
            ! C-s in reverse or C-r in forward, change direction.
            incremental_search_forward = .not. incremental_search_forward
            incremental_search_success = .true.
        end if

        ! For subsequent \| if regexp.
        incremental_search_barrier = point()

        if (len(incremental_search_string) == 0) then
            incremental_search_success = .true.
        else
            ! For the case when count > 1, don't keep intermediate states
            ! added to isearch-isearch_commands by isearch-push-state in this loop.
            outer = ncmds
            n = 1
            if (present(count)) n = count

            do
                n = n - 1
                if (n < 0) exit

                if (incremental_search_success .and. point() == incremental_search_other_end &
                    .and. .not. incremental_search_just_started) then
                    ! If repeating a search that found an empty string,
                    ! ensure we advance.
                    if (merge(is_end_of_buffer(), is_beginning_of_buffer(), incremental_search_forward)) then
                        ! If there's nowhere to advance to, fail (and wrap
                        ! next time).
                        incremental_search_success = .false.
                        call ding()
                    else
                        call forward_character(status, merge(1_int64, -1_int64, incremental_search_forward))
                        call incremental_search_search()
                    end if
                else
                    call incremental_search_search()
                end if

                ! Update isearch-isearch_commands, so if isearch-search fails later, it
                ! can restore old successful state from isearch-isearch_commands.
                if (n > 0) call incremental_search_push_state()
                ! Stop looping on failure.
                if (.not. incremental_search_success .or. allocated(incremental_search_error)) n = 0
            end do

            ncmds = outer
        end if

        call incremental_search_push_state()
        call incremental_search_update()
    end subroutine incremental_search_repeat

    !> Repeat incremental search forwards.
    !> With a numeric argument, repeat the search ARGUMENT times.
    !> A negative argument searches backwards.  Port of
    !> isearch.el:isearch-repeat-forward.
    recursive subroutine incremental_search_repeat_forward(argument)
        type(prefix_argument_type), intent(in) :: argument
        integer(int64) :: count

        if (argument%kind == PREFIX_NONE) then
            call incremental_search_repeat(.true.)
            return
        end if

        count = prefix_numeric_value(argument)

        if (count < 0) then
            call incremental_search_repeat_backward(prefix_argument_type(PREFIX_NUMBER, abs(count)))
            ! Reverse the direction back.
            call incremental_search_repeat(.true.)
        else
            ! Take into account one iteration to reverse direction.
            if (.not. incremental_search_forward) count = count + 1
            call incremental_search_repeat(.true., count)
        end if
    end subroutine incremental_search_repeat_forward

    !> Repeat incremental search backwards.
    !> With a numeric argument, repeat the search ARGUMENT times.
    !> A negative argument searches forwards.  Port of
    !> isearch.el:isearch-repeat-backward.
    recursive subroutine incremental_search_repeat_backward(argument)
        type(prefix_argument_type), intent(in) :: argument
        integer(int64) :: count

        if (argument%kind == PREFIX_NONE) then
            call incremental_search_repeat(.false.)
            return
        end if

        count = prefix_numeric_value(argument)

        if (count < 0) then
            call incremental_search_repeat_forward(prefix_argument_type(PREFIX_NUMBER, abs(count)))
            ! Reverse the direction back.
            call incremental_search_repeat(.false.)
        else
            ! Take into account one iteration to reverse direction.
            if (incremental_search_forward) count = count + 1
            call incremental_search_repeat(.false., count)
        end if
    end subroutine incremental_search_repeat_backward

    !> Go to the first occurrence of the current search string.
    !> Move point to the beginning of the buffer and search forwards from
    !> the top.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the beginning of the buffer.  Port of
    !> isearch.el:isearch-beginning-of-buffer.
    recursive subroutine incremental_search_beginning_of_buffer(argument)
        integer(int64), intent(in) :: argument
        logical :: current_forward

        if (argument < 0) then
            call incremental_search_end_of_buffer(abs(argument))
            return
        end if

        ! For the case when the match is at is_beginning_of_buffer, don't forward char in
        ! isearch-repeat.
        incremental_search_just_started = .true.
        call goto_character(point_minimum())
        current_forward = incremental_search_forward
        call incremental_search_repeat(.true., argument)
        if (.not. (current_forward .eqv. incremental_search_forward)) call incremental_search_repeat(current_forward)
    end subroutine incremental_search_beginning_of_buffer

    !> Go to the last occurrence of the current search string.
    !> Move point to the end of the buffer and search backwards from the
    !> bottom.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the end of the buffer.  Port of
    !> isearch.el:isearch-end-of-buffer.
    recursive subroutine incremental_search_end_of_buffer(argument)
        integer(int64), intent(in) :: argument
        logical :: current_forward

        if (argument < 0) then
            call incremental_search_beginning_of_buffer(abs(argument))
            return
        end if

        incremental_search_just_started = .true.
        call goto_character(point_maximum())
        current_forward = incremental_search_forward
        call incremental_search_repeat(.false., argument)
        if (.not. (current_forward .eqv. incremental_search_forward)) call incremental_search_repeat(current_forward)
    end subroutine incremental_search_end_of_buffer

    ! ------------------------------------------------------------------
    ! Editing the search string
    ! ------------------------------------------------------------------

    !> Undo last input item during a search.
    !> An input item is the result of a command that pushes a new state
    !> of isearch to `incremental_search_commands'.  If no input items have been entered
    !> yet, just beep.  Port of isearch.el:isearch-delete-char.
    subroutine incremental_search_delete_character()
        if (ncmds <= 1) then
            call ding()
        else
            call incremental_search_pop_state()
        end if

        call incremental_search_update()
    end subroutine incremental_search_delete_character

    !> Delete character from end of search string and search again.
    !> Unlike `incremental_search_delete_character', it only deletes the last character,
    !> but doesn't cancel the effect of other isearch command.
    !> If search string is empty, just beep.  Port of
    !> isearch.el:isearch-del-char.
    subroutine incremental_search_delete_end_character(argument)
        integer(int64), intent(in) :: argument

        if (len(incremental_search_string) == 0) then
            call ding()
        else
            incremental_search_string = drop_characters(incremental_search_string, int(min(argument, &
                                                                                    int(nchars(incremental_search_string), int64))))
            incremental_search_message = incremental_search_string
        end if

        ! Do the following before moving point.
        call incremental_search_message_show()
        ! Use the isearch-other-end as new starting point to be able to
        ! find the remaining part of the search string again.
        if (incremental_search_other_end /= 0) call goto_character(incremental_search_other_end)
        call incremental_search_search()
        call incremental_search_push_state()
        call incremental_search_update()
    end subroutine incremental_search_delete_end_character

    !> Add this ordinary printing CHAR to the search string and search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-printing-char.
    subroutine incremental_search_printing_character(char, count)
        integer(int64), intent(in) :: char, count
        integer(int64) :: c

        c = char
        if (c == ior(int(CHARACTER_SHIFT, int64), 32_int64)) c = 32
        call incremental_search_process_search_character(c, count)
    end subroutine incremental_search_printing_character

    !> Add CHAR to the search string, COUNT times.
    !> Search is updated accordingly.  Port of
    !> isearch.el:isearch-process-search-char.
    subroutine incremental_search_process_search_character(char, count)
        integer(int64), intent(in) :: char, count
        character(len=:), allocatable :: one, string
        type(status_type) :: st

        ! * and ? are special in regexps when not preceded by \.  } and |
        ! are special in regexps when preceded by \.  Nothing special for
        ! + because it matches at least once.
        select case (char)
        case (iachar("*"), iachar("?"))
            call incremental_search_fallback(.false.)
        case (iachar("}"))
            call incremental_search_fallback(.true., .true.)
        case (iachar("|"))
            call incremental_search_fallback(.true., .false., .true.)
        case default
            continue
        end select

        call character_to_string(int(char, int32), one, st)
        string = one
        if (count > 1) string = repeat(one, int(count))
        ! Append the char(s) to the search string, update the message and
        ! re-search.
        call incremental_search_process_search_string(string, string)
    end subroutine incremental_search_process_search_character

    !> Port of isearch.el:isearch-process-search-string.
    subroutine incremental_search_process_search_string(string, message)
        character(len=*), intent(in) :: string, message

        incremental_search_string = incremental_search_string//string
        incremental_search_message = incremental_search_message//message
        call incremental_search_search_and_update()
    end subroutine incremental_search_process_search_string

    !> Return .true. if STRING ends in an odd number of backslashes.  Port of
    !> isearch.el:isearch-backslash.
    pure logical function incremental_search_backslash(string)
        character(len=*), intent(in) :: string
        integer :: n

        n = 0

        do while (n < len(string))
            if (string(len(string) - n:len(string) - n) /= "\") exit
            n = n + 1
        end do

        incremental_search_backslash = mod(n, 2) == 1
    end function incremental_search_backslash

    !> Return point to previous successful match to allow regexp
    !> liberalization.  Respects C-s and C-r by stopping at
    !> `incremental_search_barrier' as needed.
    !>
    !> Do nothing if a backslash is escaping the liberalizing character.
    !> If WANT-BACKSLASH is .true., invert this behavior (for \} and \|).
    !>
    !> Do nothing if regexp has recently been invalid unless optional
    !> ALLOW-INVALID .true..
    !>
    !> If optional TO-BARRIER .true., ignore previous matches and go
    !> exactly to the barrier.  Port of isearch.el:isearch-fallback.
    subroutine incremental_search_fallback(want_backslash, allow_invalid, to_barrier)
        logical, intent(in) :: want_backslash
        logical, intent(in), optional :: allow_invalid, to_barrier
        integer :: frame, previous
        integer(int64) :: last_other_end
        character(len=:), allocatable :: string
        integer(int32) :: lchar
        logical :: skip

        if (.not. incremental_search_regular_expression) return
        if (incremental_search_backslash(incremental_search_string) .neqv. want_backslash) return

        ! We have to check 2 stack frames because the last might be
        ! invalid just because of a backslash.
        if (allocated(incremental_search_error) .and. ncmds >= 2 .and. .not. flag(allow_invalid)) then
            if (allocated(incremental_search_commands(ncmds - 1)%error)) return
        end if

        if (flag(to_barrier)) then
            call goto_character(incremental_search_barrier)
            incremental_search_adjusted = ADJUSTED_PENDING
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
            skip = allocated(incremental_search_commands(frame)%error)

            if (.not. skip) then
                string = incremental_search_commands(frame)%string
                lchar = 0
                if (len(string) > 0) lchar = iachar(string(len(string):len(string)))

                ! The operators aren't always operators; check backslashes.
                if (incremental_search_backslash(incremental_search_commands(previous)%string)) then
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
        last_other_end = incremental_search_barrier

        if (previous >= 1) then
            if (incremental_search_commands(previous)%other_end /= 0) last_other_end = incremental_search_commands( &
                                                                                       previous)%other_end
        end if

        if (incremental_search_forward) then
            call goto_character(max(last_other_end, incremental_search_barrier))
        else
            call goto_character(min(last_other_end, incremental_search_barrier))
        end if

        incremental_search_adjusted = ADJUSTED_PENDING
    end subroutine incremental_search_fallback

    !> Pull STRING into search string.  Port of
    !> isearch.el:isearch-yank-string.
    subroutine incremental_search_paste_string(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: s

        s = string

        ! Downcase the string if not supposed to case-fold pasted strings.
        if (incremental_search_case_fold_search /= CASE_FOLD_NO .and. search_upper_case%choice == SEARCH_UPPER_CASE_NOT_PASTED) then
            s = downcase_string(s)
        end if

        if (incremental_search_regular_expression) s = regular_expression_quote(s)
        ! Don't move cursor in reverse search.
        incremental_search_paste_flag = .true.
        call incremental_search_process_search_string(s, s)
    end subroutine incremental_search_paste_string

    !> Pull the text from point to the point reached by `jump': 1 moves
    !> forward `argument' characters, 2 to the end of the line (`argument' lines
    !> on), 3 over `argument' words or characters (isearch--yank-char-or-syntax
    !> with word syntax).  Port of isearch.el:isearch-yank-internal.
    subroutine incremental_search_paste_internal(jump, argument)
        integer, intent(in) :: jump
        integer(int64), intent(in) :: argument
        character(len=:), allocatable :: text
        type(status_type) :: status
        integer(int64) :: saved, from, k

        saved = point()
        if (.not. incremental_search_forward .and. incremental_search_other_end /= 0) call goto_character( &
            incremental_search_other_end)
        if (.not. incremental_search_success .and. incremental_search_paste_previous_point /= 0) call goto_character( &
            incremental_search_paste_previous_point)
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

        incremental_search_paste_previous_point = point()
        if (incremental_search_success) incremental_search_paste_previous_point = 0
        call buffer_substring(from, point(), text, status)
        call goto_character(saved)
        call incremental_search_paste_string(text)
    end subroutine incremental_search_paste_internal

    !> Pull next character from buffer into search string.
    !> If optional ARGUMENT is given, pull in the next ARGUMENT characters.  Port of
    !> isearch.el:isearch-yank-char.
    subroutine incremental_search_paste_character(argument)
        integer(int64), intent(in) :: argument

        call incremental_search_paste_internal(1, argument)
    end subroutine incremental_search_paste_character

    !> Pull next character or word from buffer into search string.
    !> If optional ARGUMENT is given, pull in the next ARGUMENT characters/words.
    !> Port of isearch.el:isearch-yank-word-or-char.
    subroutine incremental_search_paste_word_or_character(argument)
        integer(int64), intent(in) :: argument

        call incremental_search_paste_internal(3, argument)
    end subroutine incremental_search_paste_word_or_character

    !> The character at `position', or 0 at the end of the accessible region
    !> ((or (char-after POSITION) 0)).  Not a port.
    integer function character_after(position)
        integer(int64), intent(in) :: position

        character_after = 0

        if (position >= buffers(current)%beginning_of_accessible_portion .and. position < buffers( &
            current)%end_of_accessible_portion) then
            character_after = buffer_fetch_character(buffers(current), character_to_byte(position))
        end if
    end function character_after

    !> Pull rest of line from buffer into search string.
    !> If optional ARGUMENT is given, paste the next ARGUMENT lines.  Port of
    !> isearch.el:isearch-yank-line.
    subroutine incremental_search_paste_line(argument)
        integer(int64), intent(in) :: argument

        call incremental_search_paste_internal(2, argument)
    end subroutine incremental_search_paste_line

    !> Pull string from clipboard history into search string.  Port of
    !> isearch.el:isearch-yank-kill.
    subroutine incremental_search_paste(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        if (.not. incremental_search_active) call incremental_search_mode(.true.)
        call current_clipboard_entry(0_int64, text, status)
        if (status%ok()) call incremental_search_paste_string(text)
    end subroutine incremental_search_paste

    !> Replace just-pasted search string with previously cut string.
    !> When this command is called not immediately after a
    !> `incremental_search_paste' or a `incremental_search_paste_previous_only', it only pops the
    !> last cut string.  Port of isearch.el:isearch-yank-pop-only (no
    !> C-u: reading from the clipboard history needs the minibuffer completion).
    subroutine incremental_search_paste_previous_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        ! (Upstream's list also has isearch-yank-pop, which is not ported:
        ! Deferred.)
        if (.not. (associated(last_command, commands%incremental_search_paste) .or. &
                   associated(last_command, commands%incremental_search_paste_previous_only))) then
            call incremental_search_paste(status)
        else
            call incremental_search_pop_state()
            call current_clipboard_entry(1_int64, text, status)
            if (status%ok()) call incremental_search_paste_string(text)
        end if
    end subroutine incremental_search_paste_previous_only

    !> Print STRING at the end of the isearch prompt for 1 second.
    !> Port of isearch.el:isearch--momentary-message.
    subroutine incremental_search_momentary_message(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: m

        m = incremental_search_message_prefix(incremental_search_nonincremental)
        if (allocated(incremental_search_message)) m = m//incremental_search_text_description(incremental_search_message)
        call message_nolog(m//" ["//string//"]")
        call sit_for(1000)
    end subroutine incremental_search_momentary_message

    !> Toggle case folding in searching on or off.
    !> Toggles the value of the variable `incremental_search_case_fold_search'.
    !> Port of isearch.el:isearch-toggle-case-fold.
    subroutine incremental_search_toggle_case_fold()
        if (.not. incremental_search_active) call incremental_search_mode(.true.)

        if (incremental_search_case_fold_search /= CASE_FOLD_NO) then
            incremental_search_case_fold_search = CASE_FOLD_NO
            call incremental_search_momentary_message("case sensitive")
        else
            incremental_search_case_fold_search = CASE_FOLD_FORCED
            call incremental_search_momentary_message("case insensitive")
        end if

        incremental_search_success = .true.
        incremental_search_adjusted = ADJUSTED_TOGGLE
        call incremental_search_update()
    end subroutine incremental_search_toggle_case_fold

    !> Toggle regexp searching on or off.  Port of
    !> isearch.el:isearch-toggle-regexp.
    subroutine incremental_search_toggle_regular_expression()
        if (.not. incremental_search_active) call incremental_search_mode(.true.)
        incremental_search_regular_expression = .not. incremental_search_regular_expression
        incremental_search_success = .true.
        incremental_search_adjusted = ADJUSTED_TOGGLE
        call incremental_search_update()
    end subroutine incremental_search_toggle_regular_expression

    !> Toggle lax-whitespace searching on or off.
    !> In ordinary search, toggles the value of the variable
    !> `incremental_search_lax_whitespace'.  In regexp search, toggles the value of
    !> the variable `incremental_search_regular_expression_lax_whitespace'.  Port of
    !> isearch.el:isearch-toggle-lax-whitespace.
    subroutine incremental_search_toggle_lax_whitespace()
        logical :: on

        if (.not. incremental_search_active) call incremental_search_mode(.true.)

        if (incremental_search_regular_expression) then
            incremental_search_regular_expression_lax_whitespace = .not. incremental_search_regular_expression_lax_whitespace
            on = incremental_search_regular_expression_lax_whitespace
        else
            incremental_search_lax_whitespace = .not. incremental_search_lax_whitespace
            on = incremental_search_lax_whitespace
        end if

        if (on) then
            call incremental_search_momentary_message("match spaces loosely")
        else
            call incremental_search_momentary_message("match spaces literally")
        end if

        incremental_search_success = .true.
        incremental_search_adjusted = ADJUSTED_TOGGLE
        call incremental_search_update()
    end subroutine incremental_search_toggle_lax_whitespace

    !> Edit the search string in the minibuffer.
    !> RET resumes incremental searching with the edited string.  Port of
    !> isearch.el:isearch-edit-string with with-isearch-suspended (no
    !> minibuffer-local-isearch-map).
    recursive subroutine incremental_search_edit_string(status)
        type(status_type), intent(out) :: status
        type(incremental_search_state_type), allocatable :: saved_commands(:)
        type(value_type), allocatable :: ring(:)
        character(len=:), allocatable :: new_string, new_message, saved_error, input
        logical :: new_forward, new_nonincremental, saved_success, saved_wrapped, &
                   saved_paste_flag, had_error, saved_history_append
        integer :: new_case_fold, saved_ncmds, saved_adjusted
        integer(int64) :: saved_barrier, saved_original_point, old_point, old_other_end, fail_position
        integer :: histpos
        type(incremental_search_state_type) :: now

        status = status_type()
        new_nonincremental = incremental_search_nonincremental
        ! Locally bind all isearch global variables to protect them from
        ! recursive isearching.
        new_string = incremental_search_string
        new_message = incremental_search_message
        new_forward = incremental_search_forward
        new_case_fold = incremental_search_case_fold_search
        saved_ncmds = ncmds
        if (ncmds > 0) saved_commands = incremental_search_commands(1:ncmds)
        saved_success = incremental_search_success
        saved_wrapped = incremental_search_wrapped
        saved_barrier = incremental_search_barrier
        saved_adjusted = incremental_search_adjusted
        saved_paste_flag = incremental_search_paste_flag
        had_error = allocated(incremental_search_error)
        if (had_error) saved_error = incremental_search_error
        saved_original_point = incremental_search_original_point
        fail_position = incremental_search_fail_position()

        if (incremental_search_regular_expression) then
            histpos = regular_expression_search_ring_paste_pointer + 1
        else
            histpos = search_ring_paste_pointer + 1
        end if

        ! Actually terminate isearching until editing is done.
        call incremental_search_done(nopush=.true., edit=.true.)
        ! Save old point and isearch-other-end before reading from
        ! minibuffer that can change their values.
        old_point = point()
        old_other_end = incremental_search_other_end
        ! Don't add a new search string to the search ring here; it should
        ! be added only by `incremental_search_update_ring' called from `incremental_search_done'.
        saved_history_append = history_append_new_input
        history_append_new_input = .false.
        if (fail_position < 0) fail_position = count_characters_of(incremental_search_string)
        call read_from_minibuffer(incremental_search_message_prefix(incremental_search_nonincremental), input, status, &
                                  initial_input=incremental_search_string, position=fail_position + 1, &
                                  history=ring_name(incremental_search_regular_expression), &
                                  history_position=histpos)
        history_append_new_input = saved_history_append

        if (status%ok()) then
            new_string = input
            new_message = input
        end if

        ! Always resume isearching by restarting it.
        if (.not. incremental_search_active) call incremental_search_mode(incremental_search_forward, &
                                                                          incremental_search_regular_expression)
        ! Copy new local values to isearch globals.
        incremental_search_string = new_string
        incremental_search_message = new_message
        incremental_search_forward = new_forward
        incremental_search_nonincremental = new_nonincremental
        incremental_search_case_fold_search = new_case_fold
        ! Restore the minibuffer message before moving point.
        call incremental_search_message_show()
        ! Set point at the start (end) of old match if forward (backward),
        ! so after exiting minibuffer isearch resumes at the start (end) of
        ! this match and can find it again.
        if (old_other_end /= 0 .and. old_point == point()) call goto_character(old_other_end)

        if (status%ok()) then
            ! Empty isearch-string means use default.
            if (len(incremental_search_string) == 0) then
                call search_ring_entries(ring)
                incremental_search_string = ""
                if (size(ring) > 0) incremental_search_string = xstring(ring(1))
                incremental_search_message = incremental_search_string
                ! After taking the last element, adjust ring to previous
                ! one.
                call incremental_search_ring_adjust1(.false.)
            end if

            ! Reinvoke the pending search.
            call incremental_search_search()
            ! If no code has changed the search parameters, then pushing a
            ! new state of Isearch should not be necessary.
            now = get_state()

            if (ncmds == 0) then
                call incremental_search_push_state()
            else if (.not. same_state(incremental_search_commands(ncmds), now)) then
                call incremental_search_push_state()
            end if

            call incremental_search_update()

            if (incremental_search_nonincremental) then
                call incremental_search_done()
                ! The search done message is confusing when the string is
                ! empty, so erase it.
                if (len(incremental_search_string) == 0) call message("")
            end if
        end if

        ! The let's bindings end.
        ncmds = saved_ncmds
        if (saved_ncmds > 0) incremental_search_commands(1:saved_ncmds) = saved_commands
        incremental_search_success = saved_success
        incremental_search_wrapped = saved_wrapped
        incremental_search_barrier = saved_barrier
        incremental_search_adjusted = saved_adjusted
        incremental_search_paste_flag = saved_paste_flag
        if (allocated(incremental_search_error)) deallocate (incremental_search_error)
        if (had_error) incremental_search_error = saved_error
        incremental_search_original_point = saved_original_point
        ! Handle abort-recursive-edit.
        if (status%code == QUIT_CODE) call incremental_search_abort(status)
    end subroutine incremental_search_edit_string

    !> Quote special characters for incremental search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-quote-char.
    subroutine incremental_search_quote_character(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have
        integer(int64) :: c

        call message_nolog(incremental_search_message_string(.true.))
        call read_event(ev, have, status)
        if (.not. status%ok() .or. .not. have) return
        c = -1
        if (vtag(ev) == V_INTEGER) c = xfixnum(ev)

        if (c < 0 .or. c > MAXIMUM_CHARACTER) then
            call set_error(status, USER_ERROR_CODE, "Not a valid character")
            return
        end if

        call incremental_search_process_search_character(c, count)
    end subroutine incremental_search_quote_character

    ! ------------------------------------------------------------------
    ! Commands and the keymap
    ! ------------------------------------------------------------------

    ! Not a port.
    integer(int64) function numeric_prefix_argument()
        numeric_prefix_argument = prefix_numeric_value(current_prefix_argument)
    end function numeric_prefix_argument

    subroutine execute_incremental_search_exit(status)
        type(status_type), intent(out) :: status

        call incremental_search_exit(status)
    end subroutine execute_incremental_search_exit

    subroutine execute_incremental_search_cancel(status)
        type(status_type), intent(out) :: status

        call incremental_search_cancel(status)
    end subroutine execute_incremental_search_cancel

    subroutine execute_incremental_search_abort(status)
        type(status_type), intent(out) :: status

        call incremental_search_abort(status)
    end subroutine execute_incremental_search_abort

    subroutine execute_incremental_search_repeat_forward(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_repeat_forward(current_prefix_argument)
    end subroutine execute_incremental_search_repeat_forward

    subroutine execute_incremental_search_repeat_backward(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_repeat_backward(current_prefix_argument)
    end subroutine execute_incremental_search_repeat_backward

    subroutine execute_incremental_search_delete_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_delete_character()
    end subroutine execute_incremental_search_delete_character

    subroutine execute_incremental_search_delete_end_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_delete_end_character(numeric_prefix_argument())
    end subroutine execute_incremental_search_delete_end_character

    subroutine execute_incremental_search_printing_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (vtag(last_command_event) /= V_INTEGER) return
        call incremental_search_printing_character(xfixnum(last_command_event), numeric_prefix_argument())
    end subroutine execute_incremental_search_printing_character

    subroutine execute_incremental_search_quote_character(status)
        type(status_type), intent(out) :: status

        call incremental_search_quote_character(numeric_prefix_argument(), status)
    end subroutine execute_incremental_search_quote_character

    subroutine execute_incremental_search_paste_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_paste_character(numeric_prefix_argument())
    end subroutine execute_incremental_search_paste_character

    subroutine execute_incremental_search_paste_word_or_character(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_paste_word_or_character(numeric_prefix_argument())
    end subroutine execute_incremental_search_paste_word_or_character

    subroutine execute_incremental_search_paste_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_paste_line(numeric_prefix_argument())
    end subroutine execute_incremental_search_paste_line

    subroutine execute_incremental_search_beginning_of_buffer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_beginning_of_buffer(numeric_prefix_argument())
    end subroutine execute_incremental_search_beginning_of_buffer

    subroutine execute_incremental_search_end_of_buffer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_end_of_buffer(numeric_prefix_argument())
    end subroutine execute_incremental_search_end_of_buffer

    subroutine execute_incremental_search_paste(status)
        type(status_type), intent(out) :: status

        call incremental_search_paste(status)
    end subroutine execute_incremental_search_paste

    subroutine execute_incremental_search_paste_previous_only(status)
        type(status_type), intent(out) :: status

        call incremental_search_paste_previous_only(status)
    end subroutine execute_incremental_search_paste_previous_only

    subroutine execute_incremental_search_toggle_case_fold(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_toggle_case_fold()
    end subroutine execute_incremental_search_toggle_case_fold

    subroutine execute_incremental_search_toggle_regular_expression(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_toggle_regular_expression()
    end subroutine execute_incremental_search_toggle_regular_expression

    subroutine execute_incremental_search_toggle_lax_whitespace(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call incremental_search_toggle_lax_whitespace()
    end subroutine execute_incremental_search_toggle_lax_whitespace

    subroutine execute_incremental_search_edit_string(status)
        type(status_type), intent(out) :: status

        call incremental_search_edit_string(status)
    end subroutine execute_incremental_search_edit_string

    subroutine execute_incremental_search_ring_advance(status)
        type(status_type), intent(out) :: status

        call incremental_search_ring_advance(status)
    end subroutine execute_incremental_search_ring_advance

    subroutine execute_incremental_search_ring_retreat(status)
        type(status_type), intent(out) :: status

        call incremental_search_ring_retreat(status)
    end subroutine execute_incremental_search_ring_retreat

    !> Bind `keys' (kbd syntax) to command `command' in `map'.  Not a port.
    subroutine bind(map, keys, command)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, command
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, value_type(name=command), status)
    end subroutine bind

    !> Register the isearch commands, build `incremental_search_mode_map', and bind
    !> C-s, C-r, C-M-s and C-M-r in `global_map'.  Port of isearch.el's
    !> Assign WRAPPED to `incremental_search_wrapped', for an `incremental_search_wrap_function'.
    subroutine set_incremental_search_wrapped(wrapped)
        logical, intent(in) :: wrapped

        incremental_search_wrapped = wrapped
    end subroutine set_incremental_search_wrapped

    !> Assign POSITION to `incremental_search_barrier', for a search function.  Not a port.
    subroutine set_incremental_search_barrier(position)
        integer(int64), intent(in) :: position

        incremental_search_barrier = position
    end subroutine set_incremental_search_barrier

    !> Assign POSITION to `incremental_search_original_point', for a search function.  Not a port.
    subroutine set_incremental_search_original_point(position)
        integer(int64), intent(in) :: position

        incremental_search_original_point = position
    end subroutine set_incremental_search_original_point

    ! ------------------------------------------------------------------
    ! The buffer-local function variables
    ! ------------------------------------------------------------------

    ! Forget the current buffer's values (kill-all-local-variables).
    ! Not a port.
    subroutine clear_incremental_search_locals()
        call incremental_search_search_function_function%clear_local()
        call incremental_search_wrap_function%clear_local()
        call incremental_search_push_state_function%clear_local()
        call incremental_search_filter_test_function%clear_local()
        call incremental_search_message_function%clear_local()
    end subroutine clear_incremental_search_locals

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `incremental_search_search_function_function'.  Not a port.
    subroutine set_incremental_search_search_function_local(self, local_procedure)
        class(incremental_search_search_function_setting_type), intent(inout) :: self
        procedure(incremental_search_search_function_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_incremental_search_search_function_local

    !> Set CURRENT_PROCEDURE to the current buffer's value of
    !> `incremental_search_search_function_function':
    !> its own, else the global one.  Not a port.
    subroutine get_incremental_search_search_function_current(self, current_procedure)
        class(incremental_search_search_function_setting_type), intent(in) :: self
        procedure(incremental_search_search_function_procedure), pointer, intent(out) :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end subroutine get_incremental_search_search_function_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_incremental_search_search_function_entry(self)
        class(incremental_search_search_function_setting_type), intent(inout) :: self
        type(incremental_search_search_function_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_incremental_search_search_function_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_incremental_search_search_function_entry(self, k)
        class(incremental_search_search_function_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_incremental_search_search_function_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `incremental_search_wrap_function'.  Not a port.
    subroutine set_incremental_search_wrap_local(self, local_procedure)
        class(incremental_search_wrap_setting_type), intent(inout) :: self
        procedure(incremental_search_wrap_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_incremental_search_wrap_local

    !> The current buffer's value of
    !> `incremental_search_wrap_function':
    !> its own, else the global one.  Not a port.
    function get_incremental_search_wrap_current(self) result(current_procedure)
        class(incremental_search_wrap_setting_type), intent(in) :: self
        procedure(incremental_search_wrap_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_incremental_search_wrap_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_incremental_search_wrap_entry(self)
        class(incremental_search_wrap_setting_type), intent(inout) :: self
        type(incremental_search_wrap_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_incremental_search_wrap_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_incremental_search_wrap_entry(self, k)
        class(incremental_search_wrap_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_incremental_search_wrap_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `incremental_search_push_state_function'.  Not a port.
    subroutine set_incremental_search_push_state_local(self, local_procedure)
        class(incremental_search_push_state_setting_type), intent(inout) :: self
        procedure(incremental_search_push_state_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_incremental_search_push_state_local

    !> The current buffer's value of
    !> `incremental_search_push_state_function':
    !> its own, else the global one.  Not a port.
    function get_incremental_search_push_state_current(self) result(current_procedure)
        class(incremental_search_push_state_setting_type), intent(in) :: self
        procedure(incremental_search_push_state_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_incremental_search_push_state_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_incremental_search_push_state_entry(self)
        class(incremental_search_push_state_setting_type), intent(inout) :: self
        type(incremental_search_push_state_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_incremental_search_push_state_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_incremental_search_push_state_entry(self, k)
        class(incremental_search_push_state_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_incremental_search_push_state_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `incremental_search_message_function'.  Not a port.
    subroutine set_incremental_search_message_local(self, local_procedure)
        class(incremental_search_message_setting_type), intent(inout) :: self
        procedure(incremental_search_message_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_incremental_search_message_local

    !> The current buffer's value of
    !> `incremental_search_message_function':
    !> its own, else the global one.  Not a port.
    function get_incremental_search_message_current(self) result(current_procedure)
        class(incremental_search_message_setting_type), intent(in) :: self
        procedure(incremental_search_message_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_incremental_search_message_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_incremental_search_message_entry(self)
        class(incremental_search_message_setting_type), intent(inout) :: self
        type(incremental_search_message_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_incremental_search_message_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_incremental_search_message_entry(self, k)
        class(incremental_search_message_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_incremental_search_message_entry

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `incremental_search_filter_test_function'.  Not a port.
    subroutine set_incremental_search_filter_test_local(self, local_procedure)
        class(incremental_search_filter_test_setting_type), intent(inout) :: self
        procedure(incremental_search_filter_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_incremental_search_filter_test_local

    !> The current buffer's value of
    !> `incremental_search_filter_test_function':
    !> its own, else the global one.  Not a port.
    function get_incremental_search_filter_test_current(self) result(current_procedure)
        class(incremental_search_filter_test_setting_type), intent(in) :: self
        procedure(incremental_search_filter_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_incremental_search_filter_test_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_incremental_search_filter_test_entry(self)
        class(incremental_search_filter_test_setting_type), intent(inout) :: self
        type(incremental_search_filter_test_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_incremental_search_filter_test_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_incremental_search_filter_test_entry(self, k)
        class(incremental_search_filter_test_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_incremental_search_filter_test_entry

    !> isearch-mode-map and global bindings.  Not a port.
    subroutine initialize_incremental_search(global_map)
        type(handle_type), intent(in) :: global_map
        type(handle_type) :: meta_map
        type(value_type) :: key(1)
        type(status_type) :: status

        call append_locals_clearer(clear_incremental_search_locals)
        call incremental_search%define("incremental_search", "Face for highlighting Isearch matches.")
        call incremental_search%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                                 backgrounds=[FACE_BACKGROUND_LIGHT]), &
                                                    face_attributes(background="magenta3", foreground="lightskyblue1"))
        call incremental_search%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=88, &
                                                                 backgrounds=[FACE_BACKGROUND_DARK]), &
                                                    face_attributes(background="palevioletred2", foreground="brown4"))
        call incremental_search%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=16), &
                                                    face_attributes(background="magenta4", foreground="cyan1"))
        call incremental_search%specification_entry(display_when(classes=[FACE_CLASS_COLOR], minimum_colors=8), &
                                                    face_attributes(background="magenta4", foreground="cyan1"))
        call incremental_search%specification_entry(display_when(), face_attributes(inverse_video=.true.))
        call lazy_highlight%define("lazy_highlight", "Face for lazy highlighting of matches other than the current one."// &
                                   new_line("a")//"Used in Isearch when `incremental_search_lazy_highlight' is true,"// &
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
        commands%incremental_search_forward%execute => incremental_search_forward_command
        commands%incremental_search_backward%execute => incremental_search_backward_command
        commands%incremental_search_forward_regular_expression%execute => incremental_search_forward_regular_expression_command
        commands%incremental_search_backward_regular_expression%execute => incremental_search_backward_regular_expression_command
        commands%incremental_search_toggle_regular_expression%execute => execute_incremental_search_toggle_regular_expression
        commands%incremental_search_toggle_lax_whitespace%execute => execute_incremental_search_toggle_lax_whitespace
        commands%incremental_search_exit%execute => execute_incremental_search_exit
        commands%incremental_search_cancel%execute => execute_incremental_search_cancel
        commands%incremental_search_abort%execute => execute_incremental_search_abort
        commands%incremental_search_repeat_forward%execute => execute_incremental_search_repeat_forward
        commands%incremental_search_repeat_backward%execute => execute_incremental_search_repeat_backward
        commands%incremental_search_delete_character%execute => execute_incremental_search_delete_character
        commands%incremental_search_delete_end_character%execute => execute_incremental_search_delete_end_character
        commands%incremental_search_printing_character%execute => execute_incremental_search_printing_character
        commands%incremental_search_quote_character%execute => execute_incremental_search_quote_character
        commands%incremental_search_paste_character%execute => execute_incremental_search_paste_character
        commands%incremental_search_paste_line%execute => execute_incremental_search_paste_line
        commands%incremental_search_paste_word_or_character%execute => execute_incremental_search_paste_word_or_character
        commands%incremental_search_beginning_of_buffer%execute => execute_incremental_search_beginning_of_buffer
        commands%incremental_search_end_of_buffer%execute => execute_incremental_search_end_of_buffer
        commands%incremental_search_toggle_case_fold%execute => execute_incremental_search_toggle_case_fold
        commands%incremental_search_edit_string%execute => execute_incremental_search_edit_string
        commands%incremental_search_ring_advance%execute => execute_incremental_search_ring_advance
        commands%incremental_search_ring_retreat%execute => execute_incremental_search_ring_retreat
        commands%incremental_search_paste%execute => execute_incremental_search_paste
        commands%incremental_search_paste_previous_only%execute => execute_incremental_search_paste_previous_only
        if (.not. allocated(search_whitespace_regular_expression)) search_whitespace_regular_expression = "[ "//achar(9)//"]+"
        if (is_keymap_live(incremental_search_mode_map)) call destroy_keymap(incremental_search_mode_map, status)
        call make_keymap(incremental_search_mode_map)
        ! Make all multibyte characters search for themselves.
        call define_key_range(incremental_search_mode_map, 256_int64, int(MAXIMUM_CHARACTER, int64), &
                              value_type(name="incremental_search_printing_character"), status)
        ! Single-byte printing chars extend the search string by default.
        call define_key_range(incremental_search_mode_map, 32_int64, 255_int64, &
                              value_type(name="incremental_search_printing_character"), status)
        ! To handle local bindings with meta char prefix keys, define
        ! another full keymap.
        call make_sparse_keymap(meta_map)
        key(1) = value_type(27_int64)
        call define_key(incremental_search_mode_map, key, make_keymap_reference(meta_map), status)
        ! Several non-printing chars change the searching behavior.
        call bind(incremental_search_mode_map, "C-s", "incremental_search_repeat_forward")
        call bind(incremental_search_mode_map, "C-r", "incremental_search_repeat_backward")
        ! Define M-C-s and M-C-r like C-s and C-r so that the same key
        ! combinations can be used to repeat regexp isearches that can be
        ! used to start these searches.
        call bind(incremental_search_mode_map, "C-M-s", "incremental_search_repeat_forward")
        call bind(incremental_search_mode_map, "C-M-r", "incremental_search_repeat_backward")
        call bind(incremental_search_mode_map, "DEL", "incremental_search_delete_character")
        call bind(incremental_search_mode_map, "<backspace>", "undefined")
        call bind(incremental_search_mode_map, "C-g", "incremental_search_abort")
        call bind(incremental_search_mode_map, "ESC ESC ESC", "incremental_search_cancel")
        call bind(incremental_search_mode_map, "C-q", "incremental_search_quote_character")
        call bind(incremental_search_mode_map, "RET", "incremental_search_exit")
        call bind(incremental_search_mode_map, "<return>", "incremental_search_exit")
        call bind(incremental_search_mode_map, "C-j", "incremental_search_printing_character")
        call bind(incremental_search_mode_map, "TAB", "incremental_search_printing_character")
        call bind(incremental_search_mode_map, "S-SPC", "incremental_search_printing_character")
        call bind(incremental_search_mode_map, "C-M-d", "incremental_search_delete_end_character")
        call bind(incremental_search_mode_map, "C-w", "incremental_search_paste_word_or_character")
        call bind(incremental_search_mode_map, "C-M-y", "incremental_search_paste_character")
        call bind(incremental_search_mode_map, "M-s C-e", "incremental_search_paste_line")
        call bind(incremental_search_mode_map, "M-s M-<", "incremental_search_beginning_of_buffer")
        call bind(incremental_search_mode_map, "M-s M->", "incremental_search_end_of_buffer")
        call bind(incremental_search_mode_map, "M-n", "incremental_search_ring_advance")
        call bind(incremental_search_mode_map, "M-p", "incremental_search_ring_retreat")
        call bind(incremental_search_mode_map, "C-y", "incremental_search_paste")
        call bind(incremental_search_mode_map, "M-y", "incremental_search_paste_previous_only")
        call bind(incremental_search_mode_map, "M-c", "incremental_search_toggle_case_fold")
        call bind(incremental_search_mode_map, "M-s c", "incremental_search_toggle_case_fold")
        call bind(incremental_search_mode_map, "M-r", "incremental_search_toggle_regular_expression")
        call bind(incremental_search_mode_map, "M-s r", "incremental_search_toggle_regular_expression")
        call bind(incremental_search_mode_map, "M-s SPC", "incremental_search_toggle_lax_whitespace")
        call bind(incremental_search_mode_map, "M-e", "incremental_search_edit_string")
        call bind(incremental_search_mode_map, "M-s e", "incremental_search_edit_string")
        call bind(global_map, "C-s", "incremental_search_forward")
        call bind(global_map, "C-r", "incremental_search_backward")
        call bind(global_map, "C-M-s", "incremental_search_forward_regular_expression")
        call bind(global_map, "C-M-r", "incremental_search_backward_regular_expression")
    end subroutine initialize_incremental_search
end module incremental_search_mod
