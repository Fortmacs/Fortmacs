! spellcheck_mode_mod.f90 -- on-the-fly spell checker.
! Ported from GNU Emacs 31.1 lisp/textmodes/flyspell.el (in part).
!
! Copyright (C) 1998, 2000-2026 Free Software Foundation, Inc.
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

!> Flyspell, a minor mode performing on-the-fly spelling checking over
!> spellcheck_mod's speller process.  `spellcheck_mode' checks the words point
!> moves across (or, with `spellcheck_check_changes', the words
!> edited) after each command and highlights the misspelled ones with
!> the `spellcheck_incorrect' face, repeated ones with `spellcheck_duplicate';
!> `spellcheck_programming_mode' checks only comments and strings.
!> `spellcheck_region_highlight' and `spellcheck_buffer_highlight' check text (a large region
!> through a separate `ispell -l' run), `goto_next_spellcheck_error' (C-,)
!> moves to the next highlighted word and `auto_correct_spellcheck_word'
!> (M-TAB, C-.) cycles through the corrections of the word at point.
!> What is not ported is listed in docs/status/spellcheck_mode_mod.md.
module spellcheck_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, format_message
    use handle_mod, only: handle_type, null_handle, same_handle
    use value_mod, only: value_type, command_type, list_elements, make_list, vtag, V_NAME, symbols, xsymbol
    use data_mod, only: choice_setting_type
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use modes_mod, only: minor_modes, major_modes
    use derived_mod, only: major_mode_named, mode_test_procedure
    use easy_minor_mode_mod, only: mode_called_interactively
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, erase_buffer, &
                          is_buffer_live, deactivate_mark_variable, buffer_handle
    use insert_delete_mod, only: insert, after_change_functions
    use editing_functions_mod, only: goto_character, point, point_minimum, point_maximum, line_end_position, &
                                     is_beginning_of_buffer, is_end_of_buffer, delete_region, buffer_substring_no_properties, &
                                     character_after
    use editing_commands_mod, only: forward_character, backward_character, beginning_of_line
    use indent_mod, only: current_column
    use syntax_mod, only: character_syntax, forward_word, skip_characters_backward
    use simple_mod, only: backward_word, deactivate_mark
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, looking_at, &
                          string_match, match_beginning, match_end, regular_expression_quote, case_fold_search, SEARCH_NOERROR, &
                          SEARCH_MOVE
    use basic_subprograms_mod, only: remove_overlays, split_string
    use casefiddle_mod, only: downcase_string, upcase_string
    use utility_functions_mod, only: count_characters
    use file_input_output_mod, only: expand_file_name
    use string_type_mod, only: string_type, append_string, prepend_string
    use print_mod, only: fortran_text, fortran_string
    use process_mod, only: set_process_query_on_exit_flag
    use interval_tree_mod, only: make_overlay, overlay_put, overlay_get, delete_overlay, is_overlay, overlays_at, overlays_in, &
                                 next_overlay_change, previous_overlay_change, overlay_start, overlay_recenter
    use text_properties_mod, only: text_properties_at, get_text_property
    use faces_mod, only: named_face_type, face_attributes, display_when, FACE_STYLE_WAVE, highlight
    use font_lock_mod, only: font_lock_string_face, font_lock_comment_face, font_lock_doc_face
    use keyboard_mod, only: post_command_hook, pre_command_hook, last_command, sit_for, sit_for_input, &
                            is_input_pending, has_unread_command_events, is_executing_keyboard_macro, this_command_keys
    use execute_interactively_mod, only: this_command, execute_interactively, find_command
    use keymap_mod, only: keymap_set, key_binding, where_is_internal, key_description
    use help_mod, only: register_keymap_name
    use timer_mod, only: execute_with_idle_timer, cancel_timer
    use frame_mod, only: selected_window
    use window_mod, only: window_start, window_end, windows
    use editing_functions_mod, only: narrow_to_region, widen
    use display_engine_mod, only: message, add_minor_mode_lighter
    use spellcheck_mod, only: spellcheck_set_spellchecker_parameters, spellcheck_change_dictionary, &
                              spellcheck_accept_buffer_local_definitions, spellcheck_send_string, spellcheck_accept_output, &
                              spellcheck_parse_output, spellcheck_output_type, spellcheck_filter_variable, spellcheck_process, &
                              spellcheck_quit, spellcheck_pdict_save, spellcheck_word, spellcheck_get_casechars, &
                              spellcheck_get_not_casechars, spellcheck_get_otherchars, spellcheck_get_many_otherchars, &
                              spellcheck_get_coding_system, spellcheck_get_extended_character_mode, &
                              spellcheck_get_spellcheck_arguments, spellcheck_buffer_session_localwords, &
                              spellcheck_execute_process_region, spellcheck_local_dictionary, spellcheck_dictionary, &
                              spellcheck_local_pdict, spellcheck_personal_dictionary, spellcheck_current_dictionary, &
                              spellcheck_current_personal_dictionary, spellcheck_extra_arguments, spellcheck_encoding8_command, &
                              spellcheck_really_aspell, spellcheck_really_hunspell, spellcheck_really_enchant, &
                              spellcheck_program_name, spellcheck_words_keyword, spellcheck_parser, spellcheck_highlight_face, &
                              spellcheck_kill_spellcheck_hook_variable, SPELLCHECK_PARSER_TEX, SPELLCHECK_OUTPUT_CORRECT, &
                              SPELLCHECK_OUTPUT_ROOT, SPELLCHECK_OUTPUT_MISSPELLED, SPELLCHECK_OUTPUT_ERROR
    use send_mail_mod, only: mail_header_separator
    use sgml_mode_mod, only: sgml_lexical_context, SGML_CONTEXT_TAG, SGML_CONTEXT_PI
    implicit none(type, external)
    private

    public :: spellcheck_mode, spellcheck_programming_mode, spellcheck_word_highlight, spellcheck_region_highlight, &
              spellcheck_buffer_highlight, &
              goto_next_spellcheck_error, auto_correct_spellcheck_word, auto_correct_previous_spellcheck_word, &
              check_previous_highlighted_spellcheck_word, set_spellcheck_delayed_command, set_spellcheck_deplacement_command, &
              set_spellcheck_delayed_commands, set_spellcheck_deplacement_commands, spellcheck_pre_command_hook, &
              spellcheck_post_command_hook, check_spellcheck_changes, spellcheck_auto_correct_previous_hook, &
              get_spellcheck_word, is_spellcheck_overlay, delete_all_spellcheck_overlays, &
              delete_spellcheck_region_overlays, &
              is_spellcheck_correction_less, is_spellcheck_programming_text, is_spellcheck_mail_text, is_spellcheck_sgml_text, &
              set_local_spellcheck_generic_check_word_test_function, spellcheck_sort_procedure, spellcheck_insert_procedure, &
              initialize_spellcheck_mode

    abstract interface
        !> A function of `spellcheck_sort_corrections_function': whether
        !> CORRECTION1 sorts before CORRECTION2, the corrections of WORD.
        !> Not a port.
        logical function spellcheck_sort_procedure(correction1, correction2, word)
            implicit none(type, external)
            character(len=*), intent(in) :: correction1, correction2, word
        end function spellcheck_sort_procedure

        !> A function of `spellcheck_insert_function': insert TEXT at point.
        !> Not a port.
        recursive subroutine spellcheck_insert_procedure(text, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: text
            type(status_type), intent(out) :: status
        end subroutine spellcheck_insert_procedure
    end interface

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> How Flyspell should indicate misspelled words.
    !> .true. means use highlight, .false. means use minibuffer messages.
    !> Port of flyspell.el flyspell-highlight-flag.
    logical, public :: spellcheck_highlight_flag = .true.

    !> .true. means Flyspell reports a repeated word as an error.
    !> See `spellcheck_mark_duplications_exceptions' to add exceptions to
    !> this rule.  Detection of repeated words is not implemented in
    !> "large" regions; see variable `spellcheck_large_region_variable'.
    !> Port of flyspell.el flyspell-mark-duplications-flag.
    logical, public :: spellcheck_mark_duplications_flag = .true.

    !> .true. means Flyspell matches duplicate words case-insensitively.
    !> Port of flyspell.el flyspell-case-fold-duplications.
    logical, public :: spellcheck_case_fold_duplications = .true.

    !> An element of `spellcheck_mark_duplications_exceptions': a WORD that
    !> may be repeated when the dictionary matches LANGUAGE.  Not a port.
    type, public :: spellcheck_duplications_exception_type
        character(len=64) :: language = ""
        character(len=32) :: word = ""
    end type spellcheck_duplications_exception_type

    !> A list of exceptions for duplicated words, records of
    !> `spellcheck_duplications_exception_type' (LANGUAGE, WORD).
    !>
    !> LANGUAGE can be "", which means the exception applies regardless of
    !> the current dictionary, or a regular expression matching the
    !> dictionary name (`spellcheck_local_dictionary' or `spellcheck_dictionary')
    !> for which the exception should apply.
    !>
    !> The checked word is downcased before comparing with WORD.  Port of
    !> flyspell.el flyspell-mark-duplications-exceptions (one record for
    !> each word of upstream's EXCEPTION-LIST).
    type(spellcheck_duplications_exception_type), allocatable, public :: spellcheck_mark_duplications_exceptions(:)

    !> If .true., sort the corrections before popping them.
    !> The sorting is controlled by the `spellcheck_sort_corrections_function'
    !> variable, and defaults to sorting alphabetically.  Port of
    !> flyspell.el flyspell-sort-corrections.
    logical, public :: spellcheck_sort_corrections = .false.

    !> The function used to sort corrections.
    !> This only happens if `spellcheck_sort_corrections' is .true.  The
    !> function takes three parameters -- the two correction candidates
    !> to be sorted, and the third parameter is the word that's being
    !> corrected.  Port of flyspell.el flyspell-sort-corrections-function.
    procedure(spellcheck_sort_procedure), pointer, public :: spellcheck_sort_corrections_function => is_spellcheck_correction_less

    !> The value of `spellcheck_duplicate_distance' that searches the whole
    !> buffer.
    integer(int64), parameter, public :: SPELLCHECK_DUPLICATE_DISTANCE_UNLIMITED = -1

    !> The maximum distance for finding duplicates of unrecognized words.
    !> This applies to the feature that when a word is not found in the
    !> dictionary, if the same spelling occurs elsewhere in the buffer,
    !> Flyspell uses a different face (`spellcheck_duplicate') to highlight it.
    !> This variable specifies how far to search to find such a duplicate.
    !> SPELLCHECK_DUPLICATE_DISTANCE_UNLIMITED (-1) means no limit (search
    !> the whole buffer).
    !> 0 means do not search for duplicate unrecognized spellings.  Port of
    !> flyspell.el flyspell-duplicate-distance.
    integer(int64), public :: spellcheck_duplicate_distance = 400000

    !> The number of seconds to wait before checking, after a "delayed"
    !> command.  Port of flyspell.el flyspell-delay.
    real, public :: spellcheck_delay = 3.0

    !> .true. means misspelled words remain highlighted until corrected.
    !> If this variable is .false., only the most recently detected
    !> misspelled word is highlighted, and the highlight is turned off as
    !> soon as point moves off the misspelled word.
    !>
    !> Make sure this variable is .true. if you use `spellcheck_region_highlight'.
    !> Port of flyspell.el flyspell-persistent-highlight.
    logical, public :: spellcheck_persistent_highlight = .true.

    !> .true. means highlight incorrect words even if a property exists
    !> for this word.  Port of flyspell.el flyspell-highlight-properties.
    logical, public :: spellcheck_highlight_properties = .true.

    !> The standard list of delayed commands for Flyspell.
    !> See `spellcheck_delayed_commands'.  Port of flyspell.el
    !> flyspell-default-delayed-commands.
    character(len=64), allocatable, public :: spellcheck_default_delayed_commands(:)

    !> List of commands that are "delayed" for Flyspell mode.
    !> After these commands, Flyspell checking is delayed for a short time,
    !> whose length is specified by `spellcheck_delay'.  Port of flyspell.el
    !> flyspell-delayed-commands.
    character(len=64), allocatable, public :: spellcheck_delayed_commands(:)

    !> The standard list of deplacement commands for Flyspell.
    !> See variable `spellcheck_deplacement_commands'.  Port of flyspell.el
    !> flyspell-default-deplacement-commands.
    character(len=64), allocatable, public :: spellcheck_default_deplacement_commands(:)

    !> List of commands that are "deplacement" for Flyspell mode.
    !> After these commands, Flyspell checking is performed only if the
    !> previous command was not the very same command.  Port of flyspell.el
    !> flyspell-deplacement-commands.
    character(len=64), allocatable, public :: spellcheck_deplacement_commands(:)

    !> .true. means that Flyspell should display a welcome message when
    !> started.  Port of flyspell.el flyspell-issue-welcome-flag.
    logical, public :: spellcheck_issue_welcome_flag = .true.

    !> .true. means that Flyspell emits messages when checking words.
    !> Port of flyspell.el flyspell-issue-message-flag.
    logical, public :: spellcheck_issue_message_flag = .true.

    !> A string that is the name of the default dictionary.
    !> This is passed to the `spellcheck_change_dictionary' when flyspell is
    !> started.  If the variable `spellcheck_local_dictionary' or
    !> `spellcheck_dictionary' is set when flyspell is started, the value of
    !> that variable is used instead of `spellcheck_default_dictionary' to
    !> select the default dictionary.  Otherwise, if
    !> `spellcheck_default_dictionary' is "", it means to use Ispell's
    !> ultimate default dictionary.  Port of flyspell.el
    !> flyspell-default-dictionary.
    character(len=:), allocatable, public :: spellcheck_default_dictionary

    !> A string that is the regular expression that matches TeX commands.
    !> Port of flyspell.el flyspell-tex-command-regexp.
    character(len=:), allocatable, public :: spellcheck_tex_command_regular_expression

    !> .true. means check even inside TeX math environment.
    !> TeX math environments are discovered by `texmathp', implemented
    !> inside AUCTeX package.  That package may be found at
    !> URL `https://www.gnu.org/software/auctex/'.  Port of flyspell.el
    !> flyspell-check-tex-math-command.
    logical, public :: spellcheck_check_tex_math_command = .false.

    !> List of dictionary names that consider `-' as word delimiter.
    !> Port of flyspell.el
    !> flyspell-dictionaries-that-consider-dash-as-word-delimiter.
    character(len=32), allocatable, public :: spellcheck_dictionaries_that_consider_dash_as_word_delimiter(:)

    !> String displayed on the mode line when flyspell is active.
    !> Set this to "" if you don't want a mode line indicator.  Port of
    !> flyspell.el flyspell-mode-line-string.
    character(len=:), allocatable, public :: spellcheck_mode_line_string

    !> The setting `spellcheck_large_region_variable': .false. or a number
    !> of characters (R6-39: `spellcheck_large_region_variable%characters =
    !> 1000'; negative when no number is set).  Not a port.
    type, extends(choice_setting_type), public :: spellcheck_large_region_setting_type
        integer(int64) :: characters = -1
    contains
        procedure :: clear_kinds => spellcheck_large_region_clear_kinds
        procedure :: other_value => spellcheck_large_region_other_value
        procedure :: kind_hint => spellcheck_large_region_kind_hint
    end type spellcheck_large_region_setting_type

    !> The threshold that determines if a region is small
    !> (`spellcheck_large_region_variable%characters').
    !> If the region is smaller than this number of characters,
    !> `spellcheck_region_highlight' checks the words sequentially using regular
    !> flyspell methods.  Else, if the region is large, a new Ispell
    !> process is spawned for speed.
    !>
    !> Doubled words are not detected in a large region, because Ispell
    !> does not check for them.
    !>
    !> If this variable is .false., all regions are treated as small.
    !> Port of flyspell.el flyspell-large-region.
    type(spellcheck_large_region_setting_type), public :: spellcheck_large_region_variable = &
                                                     spellcheck_large_region_setting_type(name="spellcheck_large_region_variable", &
                                                                                               characters=1000, &
                                                                                               true_valid=.false.)

    !> Function for inserting word by flyspell upon correction.  Port of
    !> flyspell.el flyspell-insert-function.
    procedure(spellcheck_insert_procedure), pointer, public :: spellcheck_insert_function => insert

    !> String used to indicate an incorrect word starting ("" for none).
    !> Port of flyspell.el flyspell-before-incorrect-word-string.
    character(len=:), allocatable, public :: spellcheck_before_incorrect_word_string

    !> String used to indicate an incorrect word ending ("" for none).
    !> Port of flyspell.el flyspell-after-incorrect-word-string.
    character(len=:), allocatable, public :: spellcheck_after_incorrect_word_string

    !> .true. means that flyspell uses M-TAB to correct word.  (Read when
    !> `spellcheck_mode_map' is made, at startup.)  Port of flyspell.el
    !> flyspell-use-meta-tab.
    logical, public :: spellcheck_use_meta_tab = .true.

    !> The key binding for flyspell auto correction (`keymap_set' syntax;
    !> read when `spellcheck_mode_map' is made, at startup).  Port of
    !> flyspell.el flyspell-auto-correct-binding.
    character(len=32), public :: spellcheck_auto_correct_binding = "C-;"

    !> If .true., spell-check only words that were edited.
    !> By default, this is .false., and Flyspell checks every word across
    !> which you move point, even if you haven't edited the word.
    !> Customizing this option to .true. will not flag mis-spelled words
    !> across which you move point without editing them.  Port of
    !> flyspell.el flyspell-check-changes.
    logical, public :: spellcheck_check_changes = .false.

    !> Whether Flyspell should use a timer for waiting after a delayed
    !> command.
    !>
    !> If this is .true., Flyspell sets up a timer for checking the word at
    !> point `spellcheck_delay' seconds after you invoke a delayed command.
    !> Otherwise, if this option is .false., Flyspell uses `sit_for' to wait
    !> for that duration instead.  Port of flyspell.el
    !> flyspell-delay-use-timer.
    logical, public :: spellcheck_delay_use_timer = .false.

    !> A pointer to a face, an element of `spellcheck_programming_text_faces'.
    !> Not a port.
    type, public :: spellcheck_face_reference_type
        type(named_face_type), pointer :: face => null()
    end type spellcheck_face_reference_type

    !> Faces corresponding to text in programming-mode buffers.  Port of
    !> flyspell.el flyspell-prog-text-faces.
    type(spellcheck_face_reference_type), allocatable, public :: spellcheck_programming_text_faces(:)

    !> Flyspell face for misspelled words.  Port of flyspell.el
    !> flyspell-incorrect.
    type(named_face_type), target, public :: spellcheck_incorrect

    !> Flyspell face for words that appear twice in a row.
    !> See also `spellcheck_duplicate_distance'.  Port of flyspell.el
    !> flyspell-duplicate.
    type(named_face_type), target, public :: spellcheck_duplicate

    !> Hook run after entering or leaving `spellcheck_mode'.  Port of
    !> flyspell.el flyspell-mode-hook.
    type(hook_type), target, public :: spellcheck_mode_hook = hook_type(name="spellcheck_mode_hook")

    !> Hook run by `spellcheck_programming_mode' after it turns Flyspell mode on.
    !> Port of flyspell.el flyspell-prog-mode-hook.
    type(hook_type), target, public :: spellcheck_programming_mode_hook = hook_type(name="spellcheck_programming_mode_hook")

    !> Minor mode keymap for Flyspell mode--for the whole buffer.  Port of
    !> flyspell.el flyspell-mode-map.
    type(handle_type), public :: spellcheck_mode_map

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    ! A position upstream holds as nil.
    integer(int64), parameter :: NO_POSITION = -huge(1_int64)

    ! A change recorded by `record_spellcheck_change' (an element of
    ! upstream's flyspell-changes).
    type :: change_type
        integer(int64) :: start = 0, stop = 0
    end type change_type

    ! A buffer's own Flyspell variables (upstream's buffer-local ones),
    ! found by the buffer's serial number.
    type :: buffer_state_type
        integer(int64) :: serial = 0
        ! flyspell-word-cache-start, -end, -word and -result (`has_result'
        ! false for upstream's `_').
        integer(int64) :: cache_start = NO_POSITION, cache_end = NO_POSITION
        character(len=:), allocatable :: cache_word
        logical :: cache_result = .true.
        ! flyspell-auto-correct-pos, -region (start and length), -ring (the
        ! corrections and the misspelled word, the current one at
        ! `ring_index': upstream's circular list) and -word.
        integer(int64) :: auto_correct_position = NO_POSITION
        logical :: has_auto_correct_region = .false.
        integer(int64) :: auto_correct_start = 0, auto_correct_length = 0
        type(string_type), allocatable :: auto_correct_ring(:)
        integer :: ring_index = 0
        character(len=:), allocatable :: auto_correct_word
        ! flyspell-changes, the newest first.
        type(change_type), allocatable :: changes(:)
        ! flyspell-consider-dash-as-word-delimiter-flag,
        ! flyspell-dash-dictionary and flyspell-dash-local-dictionary
        ! (unallocated: never set).
        logical :: consider_dash = .false.
        character(len=:), allocatable :: dash_dictionary, dash_local_dictionary
        ! flyspell-pre-point.
        integer(int64) :: pre_point = NO_POSITION
        ! upstream's flyspell-generic-check-word-predicate.
        procedure(mode_test_procedure), pointer, nopass :: test_function => null()
    end type buffer_state_type

    type(buffer_state_type), allocatable :: states(:)

    ! flyspell-overlay.
    type(handle_type) :: spellcheck_highlight_overlay
    ! flyspell-last-buffer.
    type(handle_type) :: spellcheck_last_buffer
    ! flyspell-pre-buffer, flyspell-pre-column, flyspell-pre-pre-buffer and
    ! flyspell-pre-pre-point.
    type(handle_type) :: spellcheck_pre_buffer, spellcheck_pre_pre_buffer
    integer(int64) :: spellcheck_pre_column = NO_POSITION, spellcheck_pre_pre_point = NO_POSITION
    ! flyspell-previous-command.
    type(command_type), pointer :: spellcheck_previous_command => null()
    ! flyspell--timer and the buffer its function checks.
    type(handle_type) :: spellcheck_timer, spellcheck_timer_buffer
    ! flyspell-external-ispell-buffer, flyspell-large-region-buffer,
    ! flyspell-large-region-beg and flyspell-large-region-end.
    type(handle_type) :: spellcheck_external_spellcheck_buffer, spellcheck_large_region_buffer
    integer(int64) :: spellcheck_large_region_beginning = 1, spellcheck_large_region_end = 1
    ! flyspell-old-buffer-error and flyspell-old-pos-error.
    type(handle_type) :: spellcheck_old_buffer_error
    integer(int64) :: spellcheck_old_position_error = NO_POSITION
    ! flyspell-auto-correct-previous-pos.
    integer(int64) :: spellcheck_auto_correct_previous_position = NO_POSITION

    ! The names of the commands and properties used as values.
    character(len=*), parameter :: AUTO_CORRECT_WORD_NAME = "auto_correct_spellcheck_word"
    character(len=*), parameter :: AUTO_CORRECT_PREVIOUS_WORD_NAME = "auto_correct_previous_spellcheck_word"
    character(len=*), parameter :: P_SPELLCHECK_HIGHLIGHT_OVERLAY = "spellcheck_highlight_overlay", P_FACE = "face", &
                                   P_LOCAL_MAP = "local_map"

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), FF = achar(12)

contains

    ! ------------------------------------------------------------------
    ! Buffer state
    ! ------------------------------------------------------------------

    ! The index of the current buffer's state in `states', made if it has
    ! none.  Not a port.
    integer function state() result(k)
        type(buffer_state_type) :: fresh

        k = 0
        if (current == 0) return
        if (.not. allocated(states)) allocate (states(0))
        k = findloc(states%serial, buffers(current)%serial, dim=1)
        if (k /= 0) return
        fresh%serial = buffers(current)%serial
        allocate (fresh%changes(0))
        allocate (fresh%auto_correct_ring(0))
        states = [states, fresh]
        k = size(states)
    end function state

    !> Make FN the current buffer's
    !> `spellcheck_generic_check_word_test_function': the function providing
    !> per-mode customization over which words are flyspelled.  It returns
    !> .true. to continue checking, .false. otherwise.  Flyspell mode sets
    !> this variable to whatever is the `spellcheck_mode_test_function' of
    !> the major mode.  Port of flyspell.el
    !> flyspell-generic-check-word-predicate (setting its local value).
    subroutine set_local_spellcheck_generic_check_word_test_function(fn)
        procedure(mode_test_procedure) :: fn
        integer :: k

        k = state()
        if (k > 0) states(k)%test_function => fn
    end subroutine set_local_spellcheck_generic_check_word_test_function

    ! Whether the current buffer's
    ! `spellcheck_generic_check_word_test_function' is set and rejects the
    ! word at point.  Not a port.
    logical function is_word_rejected() result(rejected)
        integer :: k

        rejected = .false.
        k = state()
        if (k == 0) return

        if (associated(states(k)%test_function)) then
            rejected = .not. states(k)%test_function()
        end if
    end function is_word_rejected

    ! The dictionary in use: `spellcheck_local_dictionary', else
    ! `spellcheck_dictionary' ("" for the default).  Not a port.
    function dictionary() result(dictionary_name)
        character(len=:), allocatable :: dictionary_name

        dictionary_name = ""
        if (allocated(spellcheck_local_dictionary%value)) dictionary_name = spellcheck_local_dictionary%value
        if (len(dictionary_name) == 0) dictionary_name = spellcheck_dictionary
    end function dictionary

    ! Whether `this_command' is a named command (not a keyboard macro).
    ! Not a port.
    logical function is_named_command()
        is_named_command = associated(this_command)
        if (is_named_command) is_named_command = this_command%macro == 0
    end function is_named_command

    ! ------------------------------------------------------------------
    ! The large region setting
    ! ------------------------------------------------------------------

    !> Forget the characters of `spellcheck_large_region_variable', as any
    !> assignment to it does.  Not a port.
    subroutine spellcheck_large_region_clear_kinds(self)
        class(spellcheck_large_region_setting_type), intent(inout) :: self

        self%characters = -1
    end subroutine spellcheck_large_region_clear_kinds

    !> The characters of `spellcheck_large_region_variable' as help shows
    !> them (`%characters = N'); "" when none is set.  Not a port.
    function spellcheck_large_region_other_value(self) result(text)
        class(spellcheck_large_region_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=40) :: component_text

        text = ""

        if (self%characters >= 0) then
            write (component_text, '("%characters = ", i0)') self%characters
            text = trim(component_text)
        end if
    end function spellcheck_large_region_other_value

    !> The assignment that sets the number `spellcheck_large_region_variable'
    !> takes, for the error of a rejected value.  Not a port.
    function spellcheck_large_region_kind_hint(self, number) result(text)
        class(spellcheck_large_region_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of characters is `"//trim(self%name)//"%characters = "//number//"'"
    end function spellcheck_large_region_kind_hint

    ! ------------------------------------------------------------------
    ! Sorting corrections
    ! ------------------------------------------------------------------

    !> `spellcheck_sort_corrections_function's default: whether CORRECTION1
    !> sorts alphabetically before CORRECTION2.  Port of
    !> flyspell.el:flyspell-sort-corrections-alphabetically.
    logical function is_spellcheck_correction_less(correction1, correction2, word)
        character(len=*), intent(in) :: correction1, correction2, word

        if (len(word) < 0) continue
        is_spellcheck_correction_less = llt(correction1, correction2)
    end function is_spellcheck_correction_less

    !> CORRECTIONS of WORD, sorted with `spellcheck_sort_corrections_function'
    !> if `spellcheck_sort_corrections' is .true.  Port of
    !> flyspell.el:flyspell-sort (a stable insertion sort).
    subroutine sort_spellcheck_corrections(corrections, word)
        type(string_type), intent(inout) :: corrections(:)
        character(len=*), intent(in) :: word
        type(string_type) :: moving
        integer :: i, j

        if (.not. spellcheck_sort_corrections) return

        do i = 2, size(corrections)
            moving = corrections(i)
            j = i - 1

            do while (j >= 1)
                if (.not. spellcheck_sort_corrections_function(moving%text, corrections(j)%text, word)) exit
                corrections(j + 1) = corrections(j)
                j = j - 1
            end do

            corrections(j + 1) = moving
        end do
    end subroutine sort_spellcheck_corrections

    ! ------------------------------------------------------------------
    ! Mode specific tests
    ! ------------------------------------------------------------------

    !> Function used for `spellcheck_generic_check_word_test_function' in
    !> Mail mode.  Port of flyspell.el:mail-mode-flyspell-verify (message.el
    !> is not ported: its signature separator is not looked for).
    recursive logical function is_spellcheck_mail_text() result(check)
        integer(int64) :: saved, header_end, signature_begin, found, end
        type(status_type) :: st

        saved = point()
        call goto_character(point_minimum())
        call regular_expression_search_forward("^\(?:"//regular_expression_quote(mail_header_separator)//"\)?$", found, st, &
                                               noerror=SEARCH_NOERROR)
        header_end = point()
        call goto_character(saved)
        signature_begin = point_maximum()

        if (point() < header_end) then
            call goto_character(saved)
            call beginning_of_line()
            check = looking_at("^Subject:", st)
            end = match_end(0, st)
            call goto_character(saved)
            if (check) check = point() > end
        else if (point() > signature_begin) then
            check = .false.
        else
            call beginning_of_line()
            check = .not. looking_at("[>}|]\|To:", st)
            call goto_character(saved)
        end if
    end function is_spellcheck_mail_text

    !> Function used for `spellcheck_generic_check_word_test_function' in
    !> SGML mode.  Tag and attribute names are not spell checked,
    !> everything else is.
    !>
    !> String values of attributes are checked because they can be text
    !> like <img alt="Some thing.">.  Port of
    !> flyspell.el:sgml-mode-flyspell-verify.
    recursive logical function is_spellcheck_sgml_text() result(check)
        integer :: ctype
        integer(int64) :: start

        call sgml_lexical_context(ctype, start)
        check = ctype /= SGML_CONTEXT_TAG .and. ctype /= SGML_CONTEXT_PI
    end function is_spellcheck_sgml_text

    !> Used for `spellcheck_generic_check_word_test_function' in programming
    !> modes.  Port of flyspell.el:flyspell-generic-progmode-verify.
    recursive logical function is_spellcheck_programming_text() result(check)
        ! Point might be in front of, inside, or behind the misspelled word.
        check = has_programming_face(point())
        if (.not. check .and. point() /= point_minimum()) check = has_programming_face(point() - 1)

    contains

        ! Whether the face of the text at POSITION is one of
        ! `spellcheck_programming_text_faces'.
        logical function has_programming_face(position)
            integer(int64), intent(in) :: position
            type(value_type) :: face
            type(status_type) :: st
            integer :: k

            has_programming_face = .false.
            call get_text_property(position, value_type(name=P_FACE), face, st)
            if (.not. st%ok() .or. vtag(face) /= V_NAME) return

            do k = 1, size(spellcheck_programming_text_faces)
                if (.not. associated(spellcheck_programming_text_faces(k)%face)) cycle

                if (trim(spellcheck_programming_text_faces(k)%face%name) == symbols(xsymbol(face))%name) then
                    has_programming_face = .true.
                    return
                end if
            end do
        end function has_programming_face
    end function is_spellcheck_programming_text

    !> Turn on `spellcheck_mode' for comments and strings.  Port of
    !> flyspell.el:flyspell-prog-mode.
    recursive subroutine spellcheck_programming_mode(status)
        type(status_type), intent(out) :: status

        call set_local_spellcheck_generic_check_word_test_function(is_spellcheck_programming_text)
        call minor_modes%spellcheck_mode%enable(status)
        if (.not. status%ok()) return
        call spellcheck_programming_mode_hook%execute(status)
    end subroutine spellcheck_programming_mode

    ! ------------------------------------------------------------------
    ! The minor mode
    ! ------------------------------------------------------------------

    !> Toggle on-the-fly spell checking (Flyspell mode).
    !>
    !> Flyspell mode is a buffer-local minor mode.  When enabled, it
    !> spawns a single Ispell process and checks each word.  The default
    !> flyspell behavior is to highlight incorrect words.
    !>
    !> This mode is geared toward text modes.  In buffers that contain
    !> code, `spellcheck_programming_mode' is usually a better choice.
    !>
    !> Bindings:
    !> \[spellcheck_word]: correct words (using Ispell).
    !> \[auto_correct_spellcheck_word]: automatically correct word.
    !> \[auto_correct_previous_spellcheck_word]: automatically correct the last misspelled word.
    !>
    !> Hooks:
    !> This runs `spellcheck_mode_hook' after flyspell mode is entered or exit.
    !>
    !> Remark:
    !> `spellcheck_mode' uses `spellcheck_mode'.  Thus all Ispell options are
    !> valid.  For instance, a different dictionary can be used by
    !> invoking `spellcheck_change_dictionary'.
    !>
    !> Consider using the `spellcheck_parser' to check your text.  For instance
    !> consider setting it to SPELLCHECK_PARSER_TEX in `tex_mode_hook'.
    !>
    !> \[spellcheck_region_highlight] checks all words inside a region.
    !> \[spellcheck_buffer_highlight] checks the whole buffer.  Port of
    !> flyspell.el:flyspell-mode.
    recursive subroutine spellcheck_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%spellcheck_mode%command(status)
    end subroutine spellcheck_mode

    ! The body of Flyspell mode.  Port of flyspell.el:flyspell-mode (the
    ! body).
    recursive subroutine spellcheck_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()

        if (enable) then
            call enable_spellcheck_mode(mode_called_interactively, st)

            if (.not. st%ok()) then
                if (allocated(st%detail)) then
                    call message("Error enabling Flyspell mode:"//LF//st%detail)
                else
                    call message("Error enabling Flyspell mode:"//LF)
                end if

                call minor_modes%spellcheck_mode%disable(status)
            end if
        else
            call disable_spellcheck_mode(status)
        end if
    end subroutine spellcheck_mode_body

    ! The mode line text of Flyspell mode in buffer slot BI: "" when it is
    ! off or `spellcheck_mode_line_string' is "", else that string and the
    ! first two characters of the dictionary.  Port of flyspell.el's
    ! :lighter of flyspell-mode.
    subroutine spellcheck_lighter(bi, text)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable, intent(out) :: text
        character(len=:), allocatable :: dictionary_name
        type(handle_type) :: saved
        type(status_type) :: st

        text = ""
        if (.not. minor_modes%spellcheck_mode%enabled_in(bi)) return
        if (len(spellcheck_mode_line_string) == 0) return
        saved = current_buffer()
        call set_buffer(buffer_handle(bi), st)
        dictionary_name = dictionary()
        if (saved%index /= 0) call set_buffer(saved, st)
        if (len(dictionary_name) == 0) dictionary_name = "--"
        text = spellcheck_mode_line_string//"/"//dictionary_name(1:min(2, len(dictionary_name)))
    end subroutine spellcheck_lighter

    !> Use the correct dictionary: accept the buffer's local definitions
    !> (`spellcheck_accept_buffer_local_definitions') when the buffer has changed
    !> since the last flyspell operation, or with FORCE.  Port of
    !> flyspell.el:flyspell-accept-buffer-local-defs.
    recursive subroutine accept_spellcheck_buffer_local_definitions(force, status)
        logical, intent(in) :: force
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        character(len=:), allocatable :: local
        integer :: k

        status = status_type()
        if (.not. (force .or. .not. is_current(spellcheck_last_buffer))) return
        spellcheck_last_buffer = current_buffer()
        saved = current_buffer()
        call spellcheck_accept_buffer_local_definitions(status)
        if (is_buffer_live(saved)) call set_buffer(saved, status)
        if (.not. status%ok()) return
        k = state()
        local = ""
        if (allocated(spellcheck_local_dictionary%value)) local = spellcheck_local_dictionary%value

        if (allocated(states(k)%dash_dictionary) .and. allocated(states(k)%dash_local_dictionary)) then
            if (states(k)%dash_dictionary == spellcheck_dictionary .and. states(k)%dash_local_dictionary == local) return
        end if

        ! The dictionary has changed.
        states(k)%dash_dictionary = spellcheck_dictionary
        states(k)%dash_local_dictionary = local
        states(k)%consider_dash = any(spellcheck_dictionaries_that_consider_dash_as_word_delimiter == dictionary()) .and. &
                                  len(dictionary()) > 0
    end subroutine accept_spellcheck_buffer_local_definitions

    !> Flush Flyspell's caches when the Ispell process is killed.  Port of
    !> flyspell.el:flyspell-kill-ispell-hook.
    recursive subroutine spellcheck_kill_spellcheck_hook(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        spellcheck_last_buffer = null_handle()
        if (.not. allocated(states)) return

        do k = 1, size(states)
            if (allocated(states(k)%cache_word)) deallocate (states(k)%cache_word)
        end do
    end subroutine spellcheck_kill_spellcheck_hook

    ! Turn Flyspell mode on.  If SHOW_MESSAGE is true, show a welcome message
    ! if `spellcheck_issue_message_flag' and `spellcheck_issue_welcome_flag'
    ! are both true.  Port of flyspell.el:flyspell--mode-on.
    recursive subroutine enable_spellcheck_mode(show_message, status)
        logical, intent(in) :: show_message
        type(status_type), intent(out) :: status
        type(value_type) :: keys
        type(value_type), allocatable :: events(:)

        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        spellcheck_highlight_face => spellcheck_incorrect

        if (len(dictionary()) == 0 .and. len(spellcheck_default_dictionary) > 0) then
            call spellcheck_change_dictionary(spellcheck_default_dictionary, status)
            if (.not. status%ok()) return
        end if

        ! We have to force ispell to accept the local definition or
        ! otherwise it could be too late, the local dictionary may be
        ! forgotten!  Pass the `force' argument for the case where flyspell
        ! was active already but the buffer's local-defs have been edited.
        call accept_spellcheck_buffer_local_definitions(.true., status)
        if (.not. status%ok()) return
        call set_spellcheck_delayed_commands()
        call set_spellcheck_deplacement_commands()

        if (spellcheck_check_changes) then
            call post_command_hook%append(check_spellcheck_changes, local=.true.)
        else
            call post_command_hook%append(spellcheck_post_command_hook, local=.true.)
        end if

        call pre_command_hook%append(spellcheck_pre_command_hook, local=.true.)
        call after_change_functions%prepend(record_spellcheck_change, local=.true.)
        call take_mode_test_function()

        if (spellcheck_issue_message_flag .and. spellcheck_issue_welcome_flag .and. show_message) then
            call where_is_internal(value_type(name=AUTO_CORRECT_WORD_NAME), keys, firstonly=.true.)

            if (allocated(keys%a)) then
                call list_elements(keys, events)
                call message(format_message("Welcome to Flyspell. Use `"//key_description(events)// &
                                            "' or `Mouse-2' to correct words."))
            else
                call message(format_message("Welcome to Flyspell. Use `Mouse-2' to correct words."))
            end if
        end if

    contains

        ! The major mode's `spellcheck_mode_test_function', if it has one,
        ! becomes the buffer's `spellcheck_generic_check_word_test_function'.
        subroutine take_mode_test_function()
            use derived_mod, only: major_mode_type
            type(major_mode_type), pointer :: major

            if (.not. allocated(buffers(current)%major_mode)) return
            major => major_mode_named(buffers(current)%major_mode)
            if (.not. associated(major)) return
            if (.not. associated(major%spellcheck_mode_test_function)) return
            call set_local_spellcheck_generic_check_word_test_function(major%spellcheck_mode_test_function)
        end subroutine take_mode_test_function
    end subroutine enable_spellcheck_mode

    !> Install the standard set of Flyspell delayed commands.  Port of
    !> flyspell.el:flyspell-delay-commands.
    subroutine set_spellcheck_delayed_commands()
        integer :: k

        do k = 1, size(spellcheck_default_delayed_commands)
            call set_spellcheck_delayed_command(trim(spellcheck_default_delayed_commands(k)))
        end do

        do k = 1, size(spellcheck_delayed_commands)
            call set_spellcheck_delayed_command(trim(spellcheck_delayed_commands(k)))
        end do
    end subroutine set_spellcheck_delayed_commands

    !> Set COMMAND to be delayed, for Flyspell.
    !> When flyspell `post_command_hook' is invoked because a delayed
    !> command has been used, the current word is not immediately checked.
    !> It will be checked only after `spellcheck_delay' seconds.  Port of
    !> flyspell.el:flyspell-delay-command (a name that is no command is
    !> left).
    subroutine set_spellcheck_delayed_command(command)
        character(len=*), intent(in) :: command
        type(command_type), pointer :: p

        p => find_command(command)
        if (associated(p)) p%spellcheck_delayed = .true.
    end subroutine set_spellcheck_delayed_command

    ! The command `set_spellcheck_delayed_command'.
    recursive subroutine execute_set_spellcheck_delayed_command(status)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Delay Flyspell after Command: ", name, status)
        if (status%ok()) call set_spellcheck_delayed_command(name)
    end subroutine execute_set_spellcheck_delayed_command

    !> Install the standard set of Flyspell deplacement commands.  Port of
    !> flyspell.el:flyspell-deplacement-commands.
    subroutine set_spellcheck_deplacement_commands()
        integer :: k

        do k = 1, size(spellcheck_default_deplacement_commands)
            call set_spellcheck_deplacement_command(trim(spellcheck_default_deplacement_commands(k)))
        end do

        do k = 1, size(spellcheck_deplacement_commands)
            call set_spellcheck_deplacement_command(trim(spellcheck_deplacement_commands(k)))
        end do
    end subroutine set_spellcheck_deplacement_commands

    !> Set COMMAND that implement cursor movements, for Flyspell.
    !> When flyspell `post_command_hook' is invoked because a deplacement
    !> command has been used, the current word is not checked.  Port of
    !> flyspell.el:flyspell-deplacement-command (a name that is no command
    !> is left).
    subroutine set_spellcheck_deplacement_command(command)
        character(len=*), intent(in) :: command
        type(command_type), pointer :: p

        p => find_command(command)
        if (associated(p)) p%spellcheck_deplacement = .true.
    end subroutine set_spellcheck_deplacement_command

    ! The command `set_spellcheck_deplacement_command'.
    recursive subroutine execute_set_spellcheck_deplacement_command(status)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Deplacement Flyspell after Command: ", name, status)
        if (status%ok()) call set_spellcheck_deplacement_command(name)
    end subroutine execute_set_spellcheck_deplacement_command

    !> Save the current buffer and point for Flyspell's post-command hook.
    !> Port of flyspell.el:flyspell-pre-command-hook.
    recursive subroutine spellcheck_pre_command_hook(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        spellcheck_pre_buffer = current_buffer()
        k = state()
        if (k > 0) states(k)%pre_point = point()
        spellcheck_pre_column = current_column()
    end subroutine spellcheck_pre_command_hook

    ! Turn Flyspell mode off.  Port of flyspell.el:flyspell--mode-off.
    recursive subroutine disable_spellcheck_mode(status)
        type(status_type), intent(out) :: status
        integer :: k

        call post_command_hook%remove(check_spellcheck_changes, local=.true.)
        call post_command_hook%remove(spellcheck_post_command_hook, local=.true.)
        call pre_command_hook%remove(spellcheck_pre_command_hook, local=.true.)
        call after_change_functions%remove(record_spellcheck_change, local=.true.)
        call delete_all_spellcheck_overlays(status)
        spellcheck_pre_buffer = null_handle()
        k = state()
        if (k > 0) states(k)%pre_point = NO_POSITION
    end subroutine disable_spellcheck_mode

    ! Whether S, a one-character string, matches OTHERCHARS (and
    ! OTHERCHARS is not "").  Not a port.
    logical function is_otherchar(otherchars, s)
        character(len=*), intent(in) :: otherchars, s
        type(status_type) :: st

        is_otherchar = .false.
        if (len(otherchars) == 0) return
        is_otherchar = string_match(otherchars, s, st) > 0
    end function is_otherchar

    ! The character after POSITION as a string ("" at the end of the buffer).
    ! Not a port.
    function character_string_at(position) result(s)
        integer(int64), intent(in) :: position
        character(len=:), allocatable :: s
        type(status_type) :: st

        s = ""
        if (position < point_minimum() .or. position >= point_maximum()) return
        call buffer_substring_no_properties(position, position + 1, s, st)
    end function character_string_at

    ! Whether the character after POSITION has word syntax.  Not a port.
    logical function is_word_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        call character_after(c, found, position)
        is_word_character_at = .false.
        if (found) is_word_character_at = character_syntax(int(c)) == "w"
    end function is_word_character_at

    !> Return .true. if we should check the word before point.
    !> More precisely, it applies to the word that was before point
    !> before the current command.  Port of
    !> flyspell.el:flyspell-check-pre-word-p.
    logical function is_spellcheck_check_pre_word() result(check)
        character(len=:), allocatable :: otherchars
        integer(int64) :: pre_point
        integer(int32) :: c
        logical :: found
        integer :: k

        otherchars = spellcheck_get_otherchars()
        k = state()
        pre_point = states(k)%pre_point
        check = .false.

        if (pre_point == NO_POSITION .or. .not. is_current(spellcheck_pre_buffer)) then
            return
        else if (spellcheck_pre_pre_point == pre_point .and. same_handle(spellcheck_pre_pre_buffer, spellcheck_pre_buffer)) then
            return
        else if (pre_point == point() - 1 .and. &
                 (is_word_character_at(pre_point) .or. is_otherchar(otherchars, character_string_at(pre_point)))) then
            return
        else if (pre_point == point() .or. pre_point == point() + 1) then
            return
        end if

        if (is_named_command() .and. .not. is_executing_keyboard_macro()) then
            if (this_command%spellcheck_delayed .or. &
                (this_command%spellcheck_deplacement .and. associated(spellcheck_previous_command, this_command))) then
                call character_after(c, found, pre_point)
                if (current_column() == 0 .or. current_column() == spellcheck_pre_column .or. &
                    ! If other post-command-hooks change the buffer,
                    ! flyspell-pre-point can lie past eob (bug#468).
                    .not. found) return
                if (is_word_character_at(pre_point) .or. is_otherchar(otherchars, character_string_at(pre_point))) return
            end if
        end if

        if (.not. is_current(spellcheck_pre_buffer)) then
            check = .true.
        else if (states(k)%cache_start == NO_POSITION .or. states(k)%cache_end == NO_POSITION) then
            check = .true.
        else
            check = pre_point < states(k)%cache_start .or. pre_point > states(k)%cache_end
        end if
    end function is_spellcheck_check_pre_word

    ! Whether H is the current buffer.  Not a port.
    logical function is_current(h)
        type(handle_type), intent(in) :: h

        is_current = same_handle(h, current_buffer())
    end function is_current

    !> Save the current buffer and point for Flyspell's post-command hook:
    !> record the change from START to STOP.  Port of
    !> flyspell.el:flyspell-after-change-function.
    recursive subroutine record_spellcheck_change(start, stop, old_length, status)
        integer(int64), intent(in) :: start, stop, old_length
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        if (old_length < 0) continue
        k = state()
        if (k > 0) states(k)%changes = [change_type(start=start, stop=stop), states(k)%changes]
    end subroutine record_spellcheck_change

    !> Return .true. when the changed word has to be checked.
    !> The answer depends of several criteria.
    !> Mostly we check word delimiters.  Port of
    !> flyspell.el:flyspell-check-changed-word-p.
    logical function is_spellcheck_check_changed_word(start, stop) result(check)
        integer(int64), intent(in) :: start, stop
        integer(int64) :: pre_point, position
        integer(int32) :: c
        logical :: found, delimiter
        integer :: k

        k = state()
        pre_point = states(k)%pre_point
        call character_after(c, found, start)
        delimiter = .false.
        if (found) delimiter = (c == 10 .or. c == 32) .and. stop > start
        position = point()
        check = .true.
        if (delimiter .or. pre_point == NO_POSITION) return

        if ((pre_point >= start .and. pre_point <= stop) .or. position >= start .or. position <= stop .or. position == stop + 1) &
            then
            check = .false.
        end if
    end function is_spellcheck_check_changed_word

    !> Return .true. when the word at `point' has to be checked.
    !> The answer depends of several criteria.
    !> Mostly we check word delimiters.  Port of
    !> flyspell.el:flyspell-check-word-p.
    recursive logical function is_spellcheck_check_word() result(check)
        character(len=:), allocatable :: otherchars
        integer(int64) :: saved
        type(status_type) :: st
        integer :: k

        otherchars = spellcheck_get_otherchars()
        k = state()

        if (point_maximum() - 1 <= point_minimum()) then
            ! The buffer is not filled enough.
            check = .false.
            return
        end if

        if (current_column() > 0 .and. current_column() /= spellcheck_pre_column) then
            saved = point()
            call backward_character(st)
            check = looking_at(spellcheck_get_not_casechars(), st)
            if (check .and. len(otherchars) > 0) check = .not. looking_at(otherchars, st)
            if (check .and. .not. states(k)%consider_dash) check = .not. looking_at("-", st)
            call goto_character(saved)
            ! Yes because we have reached or typed a word delimiter.
            if (check) return
        end if

        check = .true.
        if (.not. is_named_command()) return

        if (this_command%spellcheck_deplacement) then
            check = .not. associated(spellcheck_previous_command, this_command)
        else if (this_command%spellcheck_delayed) then
            ! In case we're using `delete-selection-mode', make the region
            ! be updated immediately.
            call deactivate_mark()
            ! The current command is not delayed, that is that we must check
            ! the word now.
            check = .not. has_unread_command_events()
            if (.not. check) return

            if (.not. spellcheck_delay_use_timer) then
                check = sit_for_input(nint(spellcheck_delay*1000.0, int32))
            else
                spellcheck_timer_buffer = current_buffer()
                call execute_with_idle_timer(spellcheck_timer, real(spellcheck_delay, real64), .false., &
                                             check_word_when_idle, st)
                check = .false.
            end if
        end if
    end function is_spellcheck_check_word

    ! The function of `spellcheck_timer': check the word at point if the
    ! buffer is still current.  Port of the timer function of
    ! flyspell.el:flyspell-check-word-p.
    recursive subroutine check_word_when_idle(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_current(spellcheck_timer_buffer)) call spellcheck_word_highlight(status)
    end subroutine check_word_when_idle

    !> The `post_command_hook' used by flyspell to check a word on-the-fly.
    !>
    !> It is possible that we check several words:
    !> 1- the current word is checked if the test
    !>    `is_spellcheck_check_word' is true
    !> 2- the word that used to be the current word before the
    !>    `this_command' is checked if:
    !>     a- the previous word is different from the current word
    !>     b- the previous word has not just been checked by the
    !>        previous `spellcheck_post_command_hook'
    !> 3- the words changed by the `this_command' that are neither the
    !>    previous word nor the current word.  Port of
    !> flyspell.el:flyspell-post-command-hook.
    recursive subroutine spellcheck_post_command_hook(status)
        type(status_type), intent(out) :: status
        type(command_type), pointer :: command
        type(change_type) :: change
        logical :: saved_deactivate
        integer(int64) :: saved
        integer :: k

        status = status_type()
        call cancel_spellcheck_timer()
        if (.not. minor_modes%spellcheck_mode%enabled()) return
        command => this_command
        ! Prevent anything we do from affecting the mark.
        saved_deactivate = deactivate_mark_variable
        deactivate_mark_variable = .false.
        k = state()

        if (associated(command, commands%transpose_characters) .and. states(k)%pre_point /= NO_POSITION) then
            saved = point()
            call goto_character(states(k)%pre_point - 1)
            call spellcheck_word_highlight(status)
            call goto_character(saved)
        end if

        if (status%ok() .and. is_spellcheck_check_pre_word()) then
            saved = point()
            call goto_character(states(k)%pre_point)
            call spellcheck_word_highlight(status)
            call goto_character(saved)
        end if

        if (status%ok()) then
            if (is_spellcheck_check_word()) then
                call spellcheck_word_highlight(status)
                ! We remember which word we have just checked.  This will
                ! be used next time we will check a word to compare the next
                ! current word with the word that has been registered in the
                ! pre-command-hook that is these variables are used within
                ! the test `is_spellcheck_check_pre_word'.
                spellcheck_pre_pre_buffer = current_buffer()
                spellcheck_pre_pre_point = point()
            else
                spellcheck_pre_pre_buffer = null_handle()
                spellcheck_pre_pre_point = NO_POSITION

                ! When a word is not checked because of a delayed command we
                ! do not disable the ispell cache.
                if (is_named_command()) then
                    if (this_command%spellcheck_delayed) then
                        states(k)%cache_end = -1
                        states(k)%cache_result = .true.
                    end if
                end if
            end if
        end if

        do while (status%ok() .and. .not. is_input_pending() .and. size(states(k)%changes) > 0)
            change = states(k)%changes(1)

            if (is_spellcheck_check_changed_word(change%start, change%stop)) then
                saved = point()
                call goto_character(change%start)
                call spellcheck_word_highlight(status)
                call goto_character(saved)
            end if

            states(k)%changes = states(k)%changes(2:)
        end do

        spellcheck_previous_command => command
        deactivate_mark_variable = saved_deactivate
    end subroutine spellcheck_post_command_hook

    ! Cancel `spellcheck_timer' (cl-callf cancel-timer flyspell--timer).
    ! Not a port.
    subroutine cancel_spellcheck_timer()
        if (spellcheck_timer%index /= 0) call cancel_timer(spellcheck_timer)
        spellcheck_timer = null_handle()
    end subroutine cancel_spellcheck_timer

    !> Function to spell-check only edited words when point moves off the
    !> word.  This is installed by flyspell as `post_command_hook' when the
    !> user option `spellcheck_check_changes' is .true.  It
    !> spell-checks a word on moving point from the word only if the word
    !> was edited before the move.  Port of
    !> flyspell.el:flyspell-check-changes.
    recursive subroutine check_spellcheck_changes(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: word
        integer(int64) :: start, stop, word_start, word_end, saved
        logical :: found
        integer :: k

        status = status_type()
        if (.not. minor_modes%spellcheck_mode%enabled()) return
        k = state()
        if (size(states(k)%changes) == 0) return
        start = states(k)%changes(1)%start
        stop = states(k)%changes(1)%stop
        saved = point()
        call get_spellcheck_word(.false., word, word_start, word_end, found, status)
        call goto_character(saved)
        if (.not. status%ok()) return
        if (found .and. word_start <= start .and. word_end >= stop) return
        call goto_character(start)
        call spellcheck_word_highlight(status)
        call goto_character(saved)
        states(k)%changes = states(k)%changes(1:0)
    end subroutine check_spellcheck_changes

    ! ------------------------------------------------------------------
    ! Checking a word
    ! ------------------------------------------------------------------

    ! The corrections of POSITIONS as the message shows them: a Fortran array
    ! of strings (fortran_text), or the root word as a string.  Not a port.
    function replacements_text(positions, word) result(text)
        type(spellcheck_output_type), intent(in) :: positions
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text
        type(string_type), allocatable :: corrections(:)
        type(value_type), allocatable :: elements(:)
        integer :: k

        if (positions%kind == SPELLCHECK_OUTPUT_ROOT) then
            text = fortran_string(positions%word)
            return
        end if

        allocate (corrections(0))
        if (allocated(positions%misses)) corrections = positions%misses
        call sort_spellcheck_corrections(corrections, word)
        allocate (elements(size(corrections)))

        do k = 1, size(corrections)
            elements(k) = value_type(text=corrections(k)%text)
        end do

        text = fortran_text(make_list(elements))
    end function replacements_text

    ! Report the misspelling of WORD (corrections POSITIONS) in the echo area.
    ! Port of flyspell.el:flyspell-notify-misspell.
    subroutine notify_spellcheck_misspelling(word, positions)
        character(len=*), intent(in) :: word
        type(spellcheck_output_type), intent(in) :: positions

        if (spellcheck_issue_message_flag) then
            call message(format_message("misspelling `")//word//format_message("'  ")//replacements_text(positions, word))
        end if
    end subroutine notify_spellcheck_misspelling

    ! The position of WORD before point, searching back to BOUND (no bound
    ! when absent); 0 if none.  With IGNORE_CASE, compare case-
    ! insensitively.  Port of flyspell.el:flyspell-word-search-backward.
    recursive integer(int64) function search_spellcheck_word_backward(word, bound, ignore_case) result(r)
        character(len=*), intent(in) :: word
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: ignore_case
        character(len=:), allocatable :: not_casechars, word_regular_expression, found_word
        integer(int64) :: saved, found, p, lw_start, lw_end
        logical :: has_bound, fold, got
        integer(int64) :: b
        type(status_type) :: st

        r = 0
        saved = point()
        not_casechars = spellcheck_get_not_casechars()
        has_bound = .false.

        if (present(bound)) then
            if (bound > point_minimum()) then
                has_bound = .true.
                b = bound - 1
            end if
        end if

        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        word_regular_expression = "\(?:"//not_casechars//"\|\`\)"//regular_expression_quote(word)//not_casechars

        do while (r == 0)
            if (has_bound) then
                call regular_expression_search_backward(word_regular_expression, found, st, bound=b, noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_backward(word_regular_expression, found, st, noerror=SEARCH_NOERROR)
            end if

            if (found == 0 .or. .not. st%ok()) exit
            if (.not. is_beginning_of_buffer()) call forward_character(st)
            p = point()
            call get_spellcheck_word(.false., found_word, lw_start, lw_end, got, st)

            if (got) then
                if (fold) then
                    got = downcase_string(found_word) == downcase_string(word)
                else
                    got = found_word == word
                end if
            end if

            if (got) then
                r = p
            else
                call goto_character(p)
            end if
        end do

        call goto_character(saved)
    end function search_spellcheck_word_backward

    ! The position of WORD after point, searching up to BOUND (no bound when
    ! absent); 0 if none.  Port of flyspell.el:flyspell-word-search-forward.
    recursive integer(int64) function search_spellcheck_word_forward(word, bound) result(r)
        character(len=*), intent(in) :: word
        integer(int64), intent(in), optional :: bound
        character(len=:), allocatable :: not_casechars, word_regular_expression, found_word
        integer(int64) :: saved, found, p, lw_start, lw_end, b
        logical :: has_bound, got
        type(status_type) :: st

        r = 0
        saved = point()
        not_casechars = spellcheck_get_not_casechars()
        has_bound = .false.

        if (present(bound)) then
            if (bound < point_maximum()) then
                has_bound = .true.
                b = bound + 1
            end if
        end if

        word_regular_expression = not_casechars//regular_expression_quote(word)//"\(?:"//not_casechars//"\|\'\)"

        do while (r == 0)
            if (has_bound) then
                call regular_expression_search_forward(word_regular_expression, found, st, bound=b, noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_forward(word_regular_expression, found, st, noerror=SEARCH_NOERROR)
            end if

            if (found == 0 .or. .not. st%ok()) exit
            if (.not. is_end_of_buffer()) call backward_character(st)
            p = point()
            call get_spellcheck_word(.false., found_word, lw_start, lw_end, got, st)
            if (got) got = found_word == word

            if (got) then
                r = p
            else
                call goto_character(p + 1)
            end if
        end do

        call goto_character(saved)
    end function search_spellcheck_word_forward

    ! Send WORD to the speller in verbose mode and parse its answer into
    ! POSITIONS (an empty answer is the word valid); with QUIET_EXIT, mark the
    ! process so it can be killed when Fortmacs is exited without query.
    ! SECONDS is how long each wait for output lasts (none: until output).
    ! Port of the speller exchange of flyspell.el:flyspell-word,
    ! flyspell-auto-correct-word and flyspell-correct-word-before-point.
    recursive subroutine ask_speller(word, positions, status, quiet_exit, seconds)
        character(len=*), intent(in) :: word
        type(spellcheck_output_type), intent(out) :: positions
        type(status_type), intent(out) :: status
        logical, intent(in) :: quiet_exit
        real, intent(in), optional :: seconds
        type(status_type) :: st
        integer :: n

        if (allocated(spellcheck_filter_variable)) deallocate (spellcheck_filter_variable)
        allocate (spellcheck_filter_variable(0))
        ! Put in verbose mode.
        call spellcheck_send_string("%"//LF, status)
        if (.not. status%ok()) return
        call spellcheck_send_string("^"//word//LF, status)
        if (.not. status%ok()) return
        if (quiet_exit) call set_process_query_on_exit_flag(spellcheck_process, .false., st)

        ! Wait until ispell has processed word.
        do
            call spellcheck_accept_output(status, seconds)
            if (.not. status%ok()) return
            n = size(spellcheck_filter_variable)

            if (n > 0) then
                if (len(spellcheck_filter_variable(n)%text) == 0) exit
            end if
        end do

        ! Remove leading empty element.
        spellcheck_filter_variable = spellcheck_filter_variable(1:n - 1)
        n = n - 1

        ! Ispell process should return something after word is sent.  Tag
        ! word as valid (i.e., skip) otherwise.
        if (n == 0) then
            call spellcheck_parse_output("*", positions)
        else
            call spellcheck_parse_output(spellcheck_filter_variable(n)%text, positions)
        end if
    end subroutine ask_speller

    ! Whether WORD may be repeated with the current dictionary
    ! (`spellcheck_mark_duplications_exceptions').  Not a port.
    logical function is_duplication_exception(word) result(exception)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: dictionary_name, lower
        type(status_type) :: st
        integer :: k
        logical :: applies

        exception = .false.
        dictionary_name = dictionary()
        lower = downcase_string(word)

        do k = 1, size(spellcheck_mark_duplications_exceptions)
            associate (except => spellcheck_mark_duplications_exceptions(k))
                applies = len_trim(except%language) == 0

                if (.not. applies .and. len(dictionary_name) > 0) then
                    applies = string_match(trim(except%language), dictionary_name, st) > 0
                end if

                if (applies .and. trim(except%word) == lower) then
                    exception = .true.
                    return
                end if
            end associate
        end do
    end function is_duplication_exception

    !> The command `spellcheck_word_highlight': check the word before point, or the
    !> following word when `spellcheck_following_word' is true.
    recursive subroutine execute_spellcheck_word_highlight(status)
        use spellcheck_mod, only: spellcheck_following_word
        type(status_type), intent(out) :: status

        call spellcheck_word_highlight(status, spellcheck_following_word)
    end subroutine execute_spellcheck_word_highlight

    !> Spell check a word.
    !> If the optional argument FOLLOWING, or, when called interactively
    !> `spellcheck_following_word', is true, checks the following (rather than
    !> preceding) word when the cursor is not over a word.  If optional
    !> argument KNOWN_MISSPELLING is true considers word a misspelling and
    !> skips redundant spell-checking step.  CORRECT, if present, is false
    !> when the word was found misspelled or repeated.
    !>
    !> See `get_spellcheck_word' for details of how this finds the word to
    !> spell-check.  Port of flyspell.el:flyspell-word.
    recursive subroutine spellcheck_word_highlight(status, following, known_misspelling, correct)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: following, known_misspelling
        logical, intent(out), optional :: correct
        character(len=:), allocatable :: word
        integer(int64) :: saved, cursor_location, start, end, hstart, hend, p, bound
        type(spellcheck_output_type) :: positions
        logical :: found, is_following, known, ok, duplicate
        integer(int32) :: c
        integer :: k
        type(status_type) :: st

        ok = .true.
        is_following = .false.
        if (present(following)) is_following = following
        known = .false.
        if (present(known_misspelling)) known = known_misspelling
        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        saved = point()
        ! Use the correct dictionary.
        call accept_spellcheck_buffer_local_definitions(.false., status)

        if (.not. status%ok()) then
            call goto_character(saved)
            return
        end if

        k = state()
        cursor_location = point()
        call get_spellcheck_word(is_following, word, start, end, found, status)

        if (.not. status%ok() .or. .not. found) then
            call goto_character(saved)
            if (present(correct)) correct = .true.
            return
        end if

        if (is_word_rejected()) then
            call goto_character(saved)
            if (present(correct)) correct = .true.
            return
        end if

        ! Before checking in the directory, we check for doublons.
        duplicate = .false.

        if (spellcheck_mark_duplications_flag .and. .not. is_duplication_exception(word)) then
            duplicate = spellcheck_parser%value /= SPELLCHECK_PARSER_TEX

            if (.not. duplicate .and. start > point_minimum()) then
                call character_after(c, found, start - 1)
                duplicate = .not. (found .and. (c == iachar("}") .or. c == iachar("\")))
            end if

            if (duplicate) then
                call goto_character(start)
                bound = start - (end - start) + skip_characters_backward(" "//TAB//LF//FF, st)
                call goto_character(start)
                p = 0
                if (bound >= point_minimum()) p = search_spellcheck_word_backward(word, bound, &
                                                                                  spellcheck_case_fold_duplications)
                duplicate = p /= 0 .and. p /= start
            end if
        end if

        if (duplicate) then
            ! Yes, this is a doublon.
            call highlight_spellcheck_incorrect_region(start, end, .true., status)
            ok = .false.
        else if (is_cache_hit()) then
            ! This word had been already checked, we skip.
            ok = states(k)%cache_result
        else if (spellcheck_parser%value == SPELLCHECK_PARSER_TEX .and. is_spellcheck_tex_command(start)) then
            ! This is a correct word (because a tex command).
            call unhighlight_word()
        else
            ! We setup the cache.
            states(k)%cache_start = start
            states(k)%cache_end = end
            states(k)%cache_word = word

            ! Now check spelling of word.
            if (.not. known) then
                call ask_speller(word, positions, status, .true., 1.0)

                if (.not. status%ok()) then
                    call goto_character(saved)
                    return
                end if
            else
                ! Else, this was a known misspelling to begin with, and we
                ! should forge an ispell return value.
                positions%kind = SPELLCHECK_OUTPUT_MISSPELLED
                positions%word = word
                positions%offset = 1
                allocate (positions%misses(0), positions%guesses(0))
            end if

            if (positions%kind == SPELLCHECK_OUTPUT_CORRECT .or. (positions%kind == SPELLCHECK_OUTPUT_ROOT &
                                                                  .and. spellcheck_highlight_flag) .or. &
                positions%kind == SPELLCHECK_OUTPUT_ERROR) then
                ! Correct.
                states(k)%cache_result = .true.
                call unhighlight_word()
            else if (is_duplicate_misspelling()) then
                ! This is a misspelled word which occurs twice within
                ! flyspell-duplicate-distance.
                states(k)%cache_result = .false.

                if (spellcheck_highlight_flag) then
                    call highlight_spellcheck_duplicate_region(start, end, status)
                else
                    call message(format_message("duplicate `")//word//format_message("'"))
                end if

                ok = .false.
            else
                states(k)%cache_result = .false.

                ! Highlight the location as incorrect, including offset
                ! specified in POSITIONS and only for the length of the
                ! misspelled word specified by POSITIONS.
                if (spellcheck_highlight_flag) then
                    hstart = start
                    hend = end

                    if (positions%kind == SPELLCHECK_OUTPUT_MISSPELLED) then
                        hstart = start + positions%offset - 1
                        ! POSITIONS includes the misspelled word; use that to
                        ! figure out how many characters to highlight.
                        if (allocated(positions%word)) hend = hstart + count_characters(positions%word)
                    end if

                    call highlight_spellcheck_incorrect_region(hstart, hend, .false., status)
                else
                    call notify_spellcheck_misspelling(word, positions)
                end if

                ok = .false.
            end if

            ! Return to original location.
            call goto_character(cursor_location)
            if (spellcheck_quit) spellcheck_quit = .false.
        end if

        call goto_character(saved)
        if (present(correct)) correct = ok

    contains

        logical function is_cache_hit()
            is_cache_hit = .false.
            if (states(k)%cache_start /= start .or. states(k)%cache_end /= end) return
            if (.not. allocated(states(k)%cache_word)) return
            is_cache_hit = states(k)%cache_word == word
        end function is_cache_hit

        subroutine unhighlight_word()
            call unhighlight_spellcheck_at(start, status)
            if (end > start .and. status%ok()) call unhighlight_spellcheck_at(end - 1, status)
        end subroutine unhighlight_word

        logical function is_duplicate_misspelling()
            integer(int64) :: here

            is_duplicate_misspelling = .false.
            here = point()

            if (spellcheck_duplicate_distance < 0) then
                call goto_character(start)
                is_duplicate_misspelling = search_spellcheck_word_backward(word, point_minimum()) /= 0

                if (.not. is_duplicate_misspelling) then
                    call goto_character(end)
                    is_duplicate_misspelling = search_spellcheck_word_forward(word, point_maximum()) /= 0
                end if
            else if (spellcheck_duplicate_distance > 0) then
                call goto_character(start)
                is_duplicate_misspelling = search_spellcheck_word_backward(word, start - spellcheck_duplicate_distance) /= 0

                if (.not. is_duplicate_misspelling) then
                    call goto_character(end)
                    is_duplicate_misspelling = search_spellcheck_word_forward(word, end + spellcheck_duplicate_distance) /= 0
                end if
            end if

            call goto_character(here)
        end function is_duplicate_misspelling
    end subroutine spellcheck_word_highlight

    !> Whether point is within a TeX math environment, as AUCTeX's
    !> `texmathp' tells; always .false. here, as `texmathp' is not
    !> defined (AUCTeX is not part of Emacs).  Port of
    !> flyspell.el:flyspell-math-tex-command-p.
    logical function is_spellcheck_math_tex_command()
        is_spellcheck_math_tex_command = .false.
    end function is_spellcheck_math_tex_command

    !> Return .true. if the word starting at START (point after it) is a
    !> TeX command.  Port of flyspell.el:flyspell-tex-command-p.
    recursive logical function is_spellcheck_tex_command(start) result(tex)
        integer(int64), intent(in) :: start
        integer(int64) :: saved, found, back_end
        type(status_type) :: st

        saved = point()
        call regular_expression_search_backward("\\", found, st, bound=max(point() - 100, point_minimum()), noerror=SEARCH_NOERROR)
        tex = .false.

        if (found /= 0) then
            back_end = match_end(0, st)

            if (back_end == start) then
                tex = .true.
            else
                call goto_character(back_end)
                tex = looking_at(spellcheck_tex_command_regular_expression, st)
                if (tex) tex = match_end(0, st) >= start
            end if
        end if

        call goto_character(saved)
        if (.not. tex) tex = is_spellcheck_math_tex_command()
    end function is_spellcheck_tex_command

    !> The word for spell-checking according to Ispell syntax, from START
    !> to END (FOUND false when there is none).
    !> Optional argument FOLLOWING true means to get the following (rather
    !> than preceding) word when the cursor is not over a word.  Optional
    !> argument EXTRA_OTHERCHARS is a regexp of characters that may be
    !> included as part of a word (see `spellcheck_dictionary_type').
    !>
    !> This finds the word to spell-check by searching for CASECHARS
    !> defined in `spellcheck_dictionaries' for the current dictionary.  Thus,
    !> the word could be far away from point if point is inside whitespace
    !> or punctuation characters, or in text that belongs to a different
    !> language.  Port of flyspell.el:flyspell-get-word.
    recursive subroutine get_spellcheck_word(following, word, start, end, found, status, extra_otherchars)
        logical, intent(in) :: following
        character(len=:), allocatable, intent(out) :: word
        integer(int64), intent(out) :: start, end
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: extra_otherchars
        character(len=:), allocatable :: casechars, not_casechars, otherchars, word_regular_expression, extra
        integer(int64) :: at, prevpt
        logical :: many, did_it_once, has_extra, again

        word = ""
        start = 0
        end = 0
        found = .false.
        casechars = spellcheck_get_casechars()
        not_casechars = spellcheck_get_not_casechars()
        otherchars = spellcheck_get_otherchars()
        many = spellcheck_get_many_otherchars()
        has_extra = present(extra_otherchars)
        extra = ""
        if (has_extra) extra = extra_otherchars
        word_regular_expression = casechars//"+\("
        if (len(otherchars) > 0) word_regular_expression = word_regular_expression//otherchars//"?"
        if (has_extra) word_regular_expression = word_regular_expression//extra//"?"
        word_regular_expression = word_regular_expression//casechars//"+\)"

        if (many .or. has_extra) then
            word_regular_expression = word_regular_expression//"*"
        else
            word_regular_expression = word_regular_expression//"?"
        end if

        ! Find the word.
        if (.not. looking_at(casechars, status)) then
            if (following) then
                call regular_expression_search_forward(casechars, at, status, noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_backward(casechars, at, status, noerror=SEARCH_NOERROR)
            end if
        end if

        if (.not. status%ok()) return
        ! Move to front of word.
        call regular_expression_search_backward(not_casechars, at, status, noerror=SEARCH_MOVE)
        if (.not. status%ok()) return
        did_it_once = .false.
        prevpt = NO_POSITION

        do
            again = .false.
            if (len(otherchars) > 0) again = looking_at(otherchars, status)
            if (.not. again .and. has_extra) again = looking_at(extra, status)
            if (.not. again .or. is_beginning_of_buffer()) exit
            if (did_it_once .and. .not. many) exit
            if (prevpt == point()) exit

            if (has_extra .and. looking_at(extra, status)) then
                call backward_character(status)
                if (looking_at(casechars, status)) call regular_expression_search_backward(not_casechars, at, status, &
                                                                                           noerror=SEARCH_MOVE)
            else
                did_it_once = .true.
                prevpt = point()
                call backward_character(status)

                if (looking_at(casechars, status)) then
                    call regular_expression_search_backward(not_casechars, at, status, noerror=SEARCH_MOVE)
                else
                    call forward_character(status)
                end if
            end if

            if (.not. status%ok()) return
        end do

        ! Now mark the word and save to string.
        call regular_expression_search_forward(word_regular_expression, at, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok() .or. at == 0) return
        start = match_beginning(0, status)
        end = point()
        call buffer_substring_no_properties(start, end, word, status)
        found = status%ok()
    end subroutine get_spellcheck_word

    ! ------------------------------------------------------------------
    ! Regions
    ! ------------------------------------------------------------------

    !> Flyspell text between BEGINNING and END.
    !>
    !> This function is intended to work on small regions, as determined by
    !> `spellcheck_large_region_variable'.  Port of
    !> flyspell.el:flyspell-small-region.
    recursive subroutine spellcheck_small_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved, lo, hi, here
        integer :: count
        character(len=40) :: text
        type(status_type) :: st

        saved = point()
        lo = min(beginning, end)
        hi = max(beginning, end)
        call goto_character(lo)
        count = 0

        do while (point() < hi)
            if (spellcheck_issue_message_flag .and. count == 100) then
                write (text, '("Spell Checking...", i0, "%")') floor(100.0_real64*real(point() - lo, real64)/real(hi - lo, real64))
                call message(trim(text))
                count = 0
            else
                count = count + 1
            end if

            call spellcheck_word_highlight(status)
            if (.not. status%ok()) exit
            call sit_for(0)
            here = point()
            call forward_word(1_int64)
            if (point() < hi .and. point() > here + 1) call backward_character(st)
        end do

        if (status%ok()) then
            call backward_character(status)
            if (spellcheck_issue_message_flag) call message("Spell Checking completed.")
            if (status%ok()) call spellcheck_word_highlight(status)
        end if

        call goto_character(saved)
    end subroutine spellcheck_small_region

    !> Mark words from a buffer listing incorrect words in order of
    !> appearance.  The list of incorrect words should be in
    !> `spellcheck_external_spellcheck_buffer'.  (We finish by killing that
    !> buffer and setting the variable to the null handle.)  The buffer to
    !> mark them in is `spellcheck_large_region_buffer'.  Port of
    !> flyspell.el:flyspell-external-point-words.
    recursive subroutine mark_spellcheck_external_words(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: words_not_found(:), pieces(:)
        character(len=:), allocatable :: casechars, otherchars, word_characters, word, found, note
        integer(int64) :: buffer_scan_position, wordpos, at, found_start, found_end, saved
        logical :: many, keep, saved_fold, got, matched
        character(len=40) :: text
        integer(int32) :: c
        type(status_type) :: st
        integer :: k

        allocate (words_not_found(0))
        casechars = spellcheck_get_casechars()
        otherchars = spellcheck_get_otherchars()
        many = spellcheck_get_many_otherchars()
        word_characters = casechars//"+\("
        if (len(otherchars) > 0) word_characters = word_characters//otherchars//"?"
        word_characters = word_characters//casechars//"+\)"

        if (many) then
            word_characters = word_characters//"*"
        else
            word_characters = word_characters//"?"
        end if

        buffer_scan_position = spellcheck_large_region_beginning
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        call set_buffer(spellcheck_external_spellcheck_buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())

        ! Loop over incorrect words, in the order they were reported, which
        ! is also the order they appear in the buffer being checked.
        do
            call regular_expression_search_forward("\([^"//LF//"]+\)"//LF, at, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. at == 0) exit
            ! Bind WORD to the next one.
            call buffer_substring_no_properties(match_beginning(1, st), match_end(1, st), word, status)
            wordpos = point()

            if (spellcheck_issue_message_flag) then
                write (text, '("Spell Checking...", i0, "% [")') floor(100.0_real64*real(point(), real64)/real(point_maximum(), &
                                                                                                               real64))
                call message(trim(text)//word//"]")
            end if

            call set_buffer(spellcheck_large_region_buffer, status)
            if (.not. status%ok()) exit
            call goto_character(buffer_scan_position)
            keep = .true.

            ! Iterate on string search until string is found as word, not as
            ! substring.
            do while (keep)
                call search_forward(word, at, status, bound=spellcheck_large_region_end, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit

                if (at /= 0) then
                    saved = point()
                    ! Move back into the match so `get_spellcheck_word' will
                    ! find it.
                    call backward_character(st)
                    got = .false.
                    found = ""
                    ! Is this a word that matches the current dictionary?
                    if (looking_at(word_characters, st)) call get_spellcheck_word(.false., found, found_start, found_end, &
                                                                                  got, st)
                    call goto_character(saved)

                    if (got) then
                        call split_string(found, otherchars, pieces, st)
                        matched = .false.

                        do k = 1, size(pieces)
                            if (pieces(k)%text == word) matched = .true.
                        end do

                        ! Size matches, we really found it; matches as part
                        ! of a boundary-char separated word; the misspelling
                        ! has higher length than what flyspell considers the
                        ! word.
                        matched = matched .or. count_characters(found) <= count_characters(word)

                        ! ispell treats beginning of some TeX commands as
                        ! nroff control sequences and strips them in the list
                        ! of misspelled words thus giving a non-existent word.
                        ! Skip if ispell is used, string is a TeX command
                        ! (char before beginning of word is backslash) and
                        ! none of the previous conditions match.
                        if (.not. matched .and. len(spellcheck_really_aspell) == 0 .and. &
                            len(spellcheck_really_hunspell) == 0 .and. len(spellcheck_really_enchant) == 0) then
                            call character_after(c, matched, found_start - 1)
                            if (matched) matched = c == iachar("\")
                        end if
                    else
                        ! Misspelled word is not from the language supported
                        ! by the current dictionary.
                        matched = .true.
                    end if

                    if (matched) then
                        keep = .false.
                        ! Don't try spell-checking words whose characters
                        ! don't match CASECHARS, because `spellcheck_word_highlight' will
                        ! then consider as misspelling the preceding word
                        ! that matches CASECHARS.
                        if (got) call spellcheck_word_highlight(status, known_misspelling=.true.)
                        ! Search for next misspelled word will begin from end
                        ! of last validated match.
                        buffer_scan_position = point()
                    end if
                else
                    ! Record if misspelling is not found and try new one.
                    write (text, "(i0)") wordpos
                    note = " -> "//word//" - "//trim(text)

                    if (.not. any([(words_not_found(k)%text == note, k=1, size(words_not_found))])) then
                        call prepend_string(words_not_found, note)
                    end if

                    keep = .false.
                end if
            end do

            if (.not. status%ok()) exit
            call set_buffer(spellcheck_external_spellcheck_buffer, status)
            if (.not. status%ok()) exit
        end do

        case_fold_search = saved_fold
        if (.not. status%ok()) return
        ! We are done.
        if (spellcheck_issue_message_flag) call message("Spell Checking completed.")
        call set_buffer(spellcheck_large_region_buffer, status)
        if (.not. status%ok()) return

        ! Warn about not found misspellings.
        do k = 1, size(words_not_found)
            call message(words_not_found(k)%text//": word not found")
        end do

        ! Kill and forget the buffer with the list of incorrect words.
        call close_buffer(spellcheck_external_spellcheck_buffer, status)
        spellcheck_external_spellcheck_buffer = null_handle()
    end subroutine mark_spellcheck_external_words

    !> Prevent marking of words explicitly declared correct: remove the
    !> buffer's local words from MISSPELLINGS_BUFFER.  Port of
    !> flyspell.el:flyspell-process-localwords.
    recursive subroutine remove_spellcheck_localwords(misspellings_buffer, status)
        type(handle_type), intent(in) :: misspellings_buffer
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: localwords(:)
        character(len=:), allocatable :: casechars, string
        integer(int64) :: saved, end, at
        logical :: saved_fold
        type(handle_type) :: original
        type(status_type) :: st
        integer :: k

        call spellcheck_buffer_session_localwords(localwords)
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        casechars = spellcheck_get_casechars()
        ! Get localwords from the original buffer.
        saved = point()
        call goto_character(point_minimum())

        ! Localwords parsing copied from ispell.el.
        do
            call search_forward(spellcheck_words_keyword, at, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. at == 0) exit
            end = line_end_position()

            ! Buffer-local words separated by a space, and can contain any
            ! character other than a space.  Not rigorous enough.
            do
                call regular_expression_search_forward(" *\([^ ]+\)", at, status, bound=end, noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. at == 0) exit
                call buffer_substring_no_properties(match_beginning(1, st), match_end(1, st), string, status)

                ! This can fail when string contains a word with invalid
                ! chars.  Error handling needs to be added between Ispell and
                ! Fortmacs.
                if (count_characters(string) > 1 .and. string_match(casechars, string, st) == 1) then
                    call prepend_string(localwords, string)
                end if
            end do
        end do

        call goto_character(saved)

        if (.not. status%ok()) then
            case_fold_search = saved_fold
            return
        end if

        ! Remove localwords matches from misspellings-buffer.  The usual
        ! mechanism of communicating the local words to ispell does not
        ! affect the separate ispell process used by flyspell-large-region.
        original = current_buffer()
        call set_buffer(misspellings_buffer, status)

        if (status%ok()) then
            saved = point()

            do k = 1, size(localwords)
                call goto_character(point_minimum())

                do
                    call regular_expression_search_forward("^"//localwords(k)%text//LF, at, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok() .or. at == 0) exit
                    call delete_region(match_beginning(0, st), match_end(0, st), status)
                    if (.not. status%ok()) exit
                end do

                if (.not. status%ok()) exit
            end do

            call goto_character(saved)
            call set_buffer(original, st)
        end if

        case_fold_search = saved_fold
    end subroutine remove_spellcheck_localwords

    !> Check for adjacent duplicated words (doublons) in the given region.
    !> Port of flyspell.el:flyspell-check-region-doublons.
    recursive subroutine check_spellcheck_region_doublons(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved, at

        saved = point()
        call goto_character(beginning)
        ! Make sure current word is checked.
        call spellcheck_word_highlight(status)
        if (status%ok()) call backward_word(1_int64)

        do while (status%ok() .and. point() < end)
            call regular_expression_search_forward("\<\(\w+\)\>[ "//LF//TAB//FF//"]+\1\>", at, status, bound=end, &
                                                   noerror=SEARCH_MOVE)
            if (.not. status%ok() .or. at == 0) exit
            call spellcheck_word_highlight(status)
            if (status%ok()) call backward_word(1_int64)
        end do

        if (status%ok()) call spellcheck_word_highlight(status)
        call goto_character(saved)
    end subroutine check_spellcheck_region_doublons

    !> Check the region from BEGINNING to END with a separate speller run
    !> (`ispell -l', `aspell list'), then mark the words it reports.
    !> Port of flyspell.el:flyspell-large-region (the region is sent as
    !> the buffer holds it).
    recursive subroutine spellcheck_large_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(handle_type) :: curbuf, buffer
        type(value_type), allocatable :: elements(:)
        type(string_type), allocatable :: arguments(:)
        character(len=64) :: dictionary_arguments(4)
        character(len=:), allocatable :: encoding, extended, personal
        type(value_type) :: c
        integer(int64) :: at
        type(status_type) :: st
        integer :: k

        curbuf = current_buffer()
        call get_or_make_buffer("*flyspell-region*", buffer, status)
        if (.not. status%ok()) return
        spellcheck_external_spellcheck_buffer = buffer
        spellcheck_large_region_buffer = curbuf
        spellcheck_large_region_beginning = beginning
        spellcheck_large_region_end = end
        call accept_spellcheck_buffer_local_definitions(.false., status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (status%ok()) call erase_buffer(status)
        ! This is done, we can start checking...
        if (spellcheck_issue_message_flag) call message("Checking region...")
        call set_buffer(curbuf, st)
        if (.not. status%ok()) return
        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        ! Local dictionary becomes the global dictionary in use.
        spellcheck_current_dictionary = dictionary()
        personal = ""
        if (allocated(spellcheck_local_pdict%value)) personal = spellcheck_local_pdict%value
        if (len(personal) == 0) personal = spellcheck_personal_dictionary
        spellcheck_current_personal_dictionary = personal
        call spellcheck_get_spellcheck_arguments(dictionary_arguments)
        encoding = spellcheck_get_coding_system()
        allocate (arguments(0))

        do k = 1, size(dictionary_arguments)
            if (len_trim(dictionary_arguments(k)) > 0) call append_string(arguments, trim(dictionary_arguments(k)))
        end do

        ! Use specified dictionary, only define if not overridden.
        if (len(spellcheck_current_dictionary) > 0 .and. .not. any(dictionary_arguments == "-d")) then
            call prepend_string(arguments, spellcheck_current_dictionary)
            call prepend_string(arguments, "-d")
        end if

        ! Use specified pers dict.
        if (len(spellcheck_current_personal_dictionary) > 0) then
            call expand_file_name(spellcheck_current_personal_dictionary, personal, status)
            if (.not. status%ok()) return
            call append_string(arguments, "-p", personal)
        end if

        ! Check for extended character mode.
        extended = spellcheck_get_extended_character_mode()

        if (len(extended) > 0) then
            if (string_match("[^~]+$", extended, st) > 0) then
                personal = "-T"//extended(match_beginning(0, st):match_end(0, st) - 1)
                if (.not. any([(arguments(k)%text == personal, k=1, size(arguments))])) call prepend_string(arguments, personal)
            end if
        end if

        ! Add ispell-extra-args.
        do k = 1, size(spellcheck_extra_arguments)
            call append_string(arguments, trim(spellcheck_extra_arguments(k)))
        end do

        ! If we are using recent aspell or hunspell, make sure we use the
        ! right encoding for communication.  ispell or older aspell/hunspell
        ! does not support this.
        if (len(spellcheck_encoding8_command) > 0) then
            if (len(spellcheck_really_hunspell) > 0) then
                call append_string(arguments, spellcheck_encoding8_command, upcase_string(encoding))
            else
                call append_string(arguments, spellcheck_encoding8_command//encoding)
            end if
        end if

        allocate (elements(size(arguments) + 1))

        if (len(spellcheck_really_aspell) > 0) then
            elements(1) = value_type(text="list")
        else
            elements(1) = value_type(text="-l")
        end if

        do k = 1, size(arguments)
            elements(k + 1) = value_type(text=arguments(k)%text)
        end do

        call spellcheck_execute_process_region(beginning, end, spellcheck_program_name, "*flyspell-region*", make_list(elements), &
                                               c, &
                                               status)
        if (.not. status%ok()) return
        at = -1
        if (vtag(c) /= V_NAME .and. allocated(c%a)) call integer_result(c, at)

        if (at == 0) then
            call remove_spellcheck_localwords(buffer, status)
            if (status%ok()) call delete_spellcheck_region_overlays(beginning, end, status)
            if (status%ok()) call check_spellcheck_region_doublons(beginning, end, status)
            if (status%ok()) call mark_spellcheck_external_words(status)
        else
            call set_error(status, GENERIC_ERROR_CODE, "Can't check region")
        end if

    contains

        ! The exit status V holds, -1 if it is no integer.
        subroutine integer_result(v, n)
            use value_mod, only: xfixnum, V_INTEGER
            type(value_type), intent(in) :: v
            integer(int64), intent(out) :: n

            n = -1
            if (vtag(v) == V_INTEGER) n = xfixnum(v)
        end subroutine integer_result
    end subroutine spellcheck_large_region

    !> Flyspell text between BEGINNING and END.
    !>
    !> Make sure `spellcheck_mode' is turned on if you want the highlight of a
    !> misspelled word removed when you've corrected it.  Port of
    !> flyspell.el:flyspell-region.
    recursive subroutine spellcheck_region_highlight(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved, lo, hi

        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        if (beginning == end) return
        saved = point()
        lo = min(beginning, end)
        hi = max(beginning, end)

        if (spellcheck_large_region_variable%characters >= 0 .and. hi - lo > spellcheck_large_region_variable%characters) then
            call spellcheck_large_region(lo, hi, status)
        else
            call spellcheck_small_region(lo, hi, status)
        end if

        call goto_character(saved)
    end subroutine spellcheck_region_highlight

    ! The command `spellcheck_region_highlight'.
    recursive subroutine execute_spellcheck_region_highlight(status)
        use simple_mod, only: mark
        type(status_type), intent(out) :: status
        integer(int64) :: m

        call mark(m, status)
        if (.not. status%ok()) return

        if (m <= 0) then
            call set_error(status, GENERIC_ERROR_CODE, "The mark is not set now, so there is no region")
            return
        end if

        call spellcheck_region_highlight(min(point(), m), max(point(), m), status)
    end subroutine execute_spellcheck_region_highlight

    !> Flyspell whole buffer.  Port of flyspell.el:flyspell-buffer.
    recursive subroutine spellcheck_buffer_highlight(status)
        type(status_type), intent(out) :: status

        call spellcheck_region_highlight(point_minimum(), point_maximum(), status)
    end subroutine spellcheck_buffer_highlight

    ! ------------------------------------------------------------------
    ! Errors and overlays
    ! ------------------------------------------------------------------

    !> Go to the next error.
    !> If PREVIOUS (interactively, the prefix), go to the previous error
    !> instead.
    !>
    !> In general `goto_next_spellcheck_error' must be used after
    !> `spellcheck_buffer_highlight'.  Port of flyspell.el:flyspell-goto-next-error.
    recursive subroutine goto_next_spellcheck_error(status, previous)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: previous
        integer(int64) :: position, max
        logical :: back
        type(handle_type), allocatable :: found(:)

        status = status_type()
        back = .false.
        if (present(previous)) back = previous
        position = point()

        if (back) then
            max = point_minimum()
        else
            max = point_maximum()
        end if

        if (is_current(spellcheck_old_buffer_error) .and. position == spellcheck_old_position_error) then
            if (back) then
                if (spellcheck_old_position_error == max) then
                    call message("Restarting from end of the buffer")
                    call goto_character(point_maximum())
                else
                    call forward_word(-1_int64)
                end if
            else
                if (spellcheck_old_position_error == max) then
                    call message("Restarting from beginning of buffer")
                    call goto_character(point_minimum())
                else
                    call forward_word(1_int64)
                end if
            end if

            position = point()
        end if

        ! Seek the next error.
        do while (position /= max)
            if (back) then
                position = previous_overlay_change(position)
            else
                position = next_overlay_change(position)
            end if

            call overlays_at(position, found)
            if (any_spellcheck_overlay(found)) exit
        end do

        call goto_character(position)
        ! Save the current location for next invocation.
        spellcheck_old_position_error = point()
        spellcheck_old_buffer_error = current_buffer()
        if (point() == max) call message("No more miss-spelled words")
    end subroutine goto_next_spellcheck_error

    ! The command `goto_next_spellcheck_error'.
    recursive subroutine execute_goto_next_spellcheck_error(status)
        use execute_interactively_mod, only: current_prefix_argument, PREFIX_NONE
        type(status_type), intent(out) :: status

        call goto_next_spellcheck_error(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_goto_next_spellcheck_error

    ! Whether one of OVERLAYS is used by flyspell.  Not a port.
    logical function any_spellcheck_overlay(overlays) result(found)
        type(handle_type), intent(in) :: overlays(:)
        integer :: k

        found = .false.

        do k = 1, size(overlays)
            if (is_spellcheck_overlay(overlays(k))) then
                found = .true.
                return
            end if
        end do
    end function any_spellcheck_overlay

    !> Return .true. if O is an overlay used by flyspell.  Port of
    !> flyspell.el:flyspell-overlay-p.
    logical function is_spellcheck_overlay(o)
        type(handle_type), intent(in) :: o
        type(value_type) :: property

        is_spellcheck_overlay = is_overlay(o)
        if (.not. is_spellcheck_overlay) return
        property = overlay_get(o, P_SPELLCHECK_HIGHLIGHT_OVERLAY)
        is_spellcheck_overlay = allocated(property%a)
    end function is_spellcheck_overlay

    !> Delete overlays used by flyspell in a given region.  Port of
    !> flyspell.el:flyspell-delete-region-overlays.
    subroutine delete_spellcheck_region_overlays(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        call remove_overlays(beginning, end, P_SPELLCHECK_HIGHLIGHT_OVERLAY, value_type(.true.), status)
    end subroutine delete_spellcheck_region_overlays

    !> Delete all the overlays used by flyspell.  Port of
    !> flyspell.el:flyspell-delete-all-overlays.
    subroutine delete_all_spellcheck_overlays(status)
        type(status_type), intent(out) :: status

        call delete_spellcheck_region_overlays(point_minimum(), point_maximum(), status)
    end subroutine delete_all_spellcheck_overlays

    !> Remove the flyspell overlay that are located at POSITION.  Port of
    !> flyspell.el:flyspell-unhighlight-at.
    subroutine unhighlight_spellcheck_at(position, status)
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: found(:)
        integer :: k

        status = status_type()

        if (spellcheck_persistent_highlight) then
            call overlays_at(position, found)

            do k = 1, size(found)
                if (is_spellcheck_overlay(found(k))) call delete_overlay(found(k))
            end do
        else if (is_spellcheck_overlay(spellcheck_highlight_overlay)) then
            call delete_overlay(spellcheck_highlight_overlay)
        end if
    end subroutine unhighlight_spellcheck_at

    !> Return .true. if there is a text property at POSITION, not counting
    !> `local_map'.  If variable `spellcheck_highlight_properties' is set to
    !> .false., text with properties are not checked.  This function is
    !> used to discover if the character at POSITION has any other property.
    !> Port of flyspell.el:flyspell-properties-at-p.
    logical function has_spellcheck_properties_at(position) result(has)
        integer(int64), intent(in) :: position
        type(value_type) :: plist
        type(value_type), allocatable :: properties(:)
        type(status_type) :: st
        integer :: k

        has = .false.
        call text_properties_at(position, plist, st)
        if (.not. st%ok() .or. .not. allocated(plist%a)) return
        call list_elements(plist, properties)
        k = 1

        do while (k <= size(properties))
            if (vtag(properties(k)) == V_NAME .and. k < size(properties)) then
                if (symbols(xsymbol(properties(k)))%name == P_LOCAL_MAP) then
                    k = k + 2
                    cycle
                end if
            end if

            has = .true.
            return
        end do
    end function has_spellcheck_properties_at

    !> Allocate an overlay to highlight an incorrect word, in OVERLAY.
    !> BEGINNING and END specify the range in the buffer of that word.
    !> FACE and MOUSE_FACE specify the `face' and `mouse_face' properties
    !> for the overlay.  Port of flyspell.el:make-flyspell-overlay (the
    !> mouse keymap is not ported).
    subroutine make_spellcheck_overlay(beginning, end, face, mouse_face, overlay, status)
        integer(int64), intent(in) :: beginning, end
        type(named_face_type), intent(in) :: face, mouse_face
        type(handle_type), intent(out) :: overlay
        type(status_type), intent(out) :: status

        call make_overlay(overlay, beginning, end, status, front_advance=.false., rear_advance=.true.)
        if (status%ok()) call overlay_put(overlay, "face", value_type(name=trim(face%name)), status)
        if (status%ok()) call overlay_put(overlay, "mouse_face", value_type(name=trim(mouse_face%name)), status)
        if (status%ok()) call overlay_put(overlay, P_SPELLCHECK_HIGHLIGHT_OVERLAY, value_type(.true.), status)
        if (status%ok()) call overlay_put(overlay, "evaporate", value_type(.true.), status)
        if (status%ok()) call overlay_put(overlay, "help_echo", value_type(text="mouse-2: correct word at point"), status)
        if (.not. status%ok()) return

        if (trim(face%name) == trim(spellcheck_incorrect%name)) then
            if (len(spellcheck_before_incorrect_word_string) > 0) then
                call overlay_put(overlay, "before_string", value_type(text=spellcheck_before_incorrect_word_string), status)
            end if

            if (status%ok() .and. len(spellcheck_after_incorrect_word_string) > 0) then
                call overlay_put(overlay, "after_string", value_type(text=spellcheck_after_incorrect_word_string), status)
            end if
        end if
    end subroutine make_spellcheck_overlay

    !> Set up an overlay on a misspelled word, in the buffer from BEGINNING
    !> to END.  DOUBLON true means the word is itself incorrect, but
    !> suspiciously repeated.  Port of
    !> flyspell.el:flyspell-highlight-incorrect-region
    !> (`flyspell-incorrect-hook' is not ported).
    subroutine highlight_spellcheck_incorrect_region(beginning, end, doublon, status)
        integer(int64), intent(in) :: beginning, end
        logical, intent(in) :: doublon
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: found(:)
        integer :: k

        status = status_type()
        if (.not. (spellcheck_highlight_properties .or. .not. has_spellcheck_properties_at(beginning))) return

        ! We cleanup all the overlay that are in the region, not beginning
        ! at the word start position.
        if (beginning + 1 < end) then
            call overlays_in(beginning + 1, end, found)

            do k = 1, size(found)
                if (is_spellcheck_overlay(found(k))) call delete_overlay(found(k))
            end do
        end if

        ! We cleanup current overlay at the same position.
        call unhighlight_spellcheck_at(beginning, status)

        ! Now we can use a new overlay.
        if (status%ok()) then
            if (doublon) then
                call make_spellcheck_overlay(beginning, end, spellcheck_duplicate, highlight, spellcheck_highlight_overlay, status)
            else
                call make_spellcheck_overlay(beginning, end, spellcheck_incorrect, highlight, spellcheck_highlight_overlay, status)
            end if
        end if
    end subroutine highlight_spellcheck_incorrect_region

    !> Set up overlay on duplicate misspelled word, in the buffer from
    !> BEGINNING to END.  Port of
    !> flyspell.el:flyspell-highlight-duplicate-region
    !> (`flyspell-incorrect-hook' is not ported).
    subroutine highlight_spellcheck_duplicate_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. (spellcheck_highlight_properties .or. .not. has_spellcheck_properties_at(beginning))) return
        ! We cleanup current overlay at the same position.
        call unhighlight_spellcheck_at(beginning, status)
        ! Now we can use a new overlay.
        if (status%ok()) call make_spellcheck_overlay(beginning, end, spellcheck_duplicate, highlight, &
                                                      spellcheck_highlight_overlay, status)
    end subroutine highlight_spellcheck_duplicate_region

    ! ------------------------------------------------------------------
    ! Correcting words
    ! ------------------------------------------------------------------

    !> Correct the closest previous word that is highlighted as misspelled.
    !> This function scans for a word which starts before point that has
    !> been highlighted by Flyspell as misspelled.  If it finds one, it
    !> proposes a replacement for that word.  With prefix arg N (ARGUMENT),
    !> check the Nth word before point that's highlighted as misspelled.
    !> Port of flyspell.el:flyspell-check-previous-highlighted-word.
    recursive subroutine check_previous_highlighted_spellcheck_word(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        integer(int64) :: position1, position, n, saved
        type(handle_type), allocatable :: found(:)
        logical :: hit
        integer :: k

        position1 = point()
        position = point()
        n = 1
        if (present(argument)) n = argument
        if (n < 1) n = 1
        hit = .false.

        outer: do
            position = previous_overlay_change(position)
            if (position == position1) exit outer
            position1 = position

            if (position >= point_minimum()) then
                call overlays_at(position, found)

                do k = 1, size(found)
                    if (is_spellcheck_overlay(found(k))) then
                        n = n - 1

                        if (n == 0) then
                            hit = .true.
                            exit outer
                        end if
                    end if
                end do
            end if
        end do outer

        if (.not. hit) then
            call set_error(status, GENERIC_ERROR_CODE, "No word to correct before point")
            return
        end if

        saved = point()
        call goto_character(position)
        call spellcheck_word(status)

        if (status%ok()) then
            ! Force `spellcheck_word_highlight' re-check.
            k = state()
            if (allocated(states(k)%cache_word)) deallocate (states(k)%cache_word)
            call spellcheck_word_highlight(status)
        end if

        call goto_character(saved)
    end subroutine check_previous_highlighted_spellcheck_word

    ! The command `check_previous_highlighted_spellcheck_word'.
    recursive subroutine execute_check_previous_highlighted_spellcheck_word(status)
        use execute_interactively_mod, only: current_prefix_argument, PREFIX_NUMBER, PREFIX_MINUS
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NUMBER) then
            call check_previous_highlighted_spellcheck_word(status, current_prefix_argument%value)
        else if (current_prefix_argument%kind == PREFIX_MINUS) then
            call check_previous_highlighted_spellcheck_word(status, -1_int64)
        else
            call check_previous_highlighted_spellcheck_word(status)
        end if
    end subroutine execute_check_previous_highlighted_spellcheck_word

    ! Show CORRECTIONS (from the ring entry START on, cycling) in the echo
    ! area: "Corrections:" and the words while the text is shorter than 80
    ! characters.  Port of flyspell.el:flyspell-display-next-corrections
    ! (the misspelled word's `spellcheck_incorrect' face in the message is
    ! not ported).
    subroutine display_next_spellcheck_corrections(ring, start, first)
        type(string_type), intent(in) :: ring(:)
        integer, intent(in) :: start
        character(len=*), intent(in), optional :: first
        character(len=:), allocatable :: string
        integer :: k

        string = "Corrections:"
        if (size(ring) == 0) return
        k = start

        if (present(first)) then
            string = string//" "//first
        end if

        do while (count_characters(string) < 80)
            string = string//" "//ring(k)%text
            k = modulo(k, size(ring)) + 1
        end do

        call message(string)
    end subroutine display_next_spellcheck_corrections

    !> Correct the current word.
    !> This command proposes various successive corrections for the
    !> current word.  If invoked repeatedly on the same position, it
    !> cycles through the possible corrections of the current word.
    !>
    !> See `get_spellcheck_word' for details of how this finds the word to
    !> spell-check.  Port of flyspell.el:flyspell-auto-correct-word
    !> (`flyspell-abbrev-p' is not ported).
    recursive subroutine auto_correct_spellcheck_word(status)
        type(status_type), intent(out) :: status
        integer(int64) :: position, old_maximum, start, end, length
        type(value_type) :: next_command
        character(len=:), allocatable :: word, new_word
        type(spellcheck_output_type) :: positions
        type(string_type), allocatable :: replacements(:)
        logical :: found, was_on
        integer :: k

        status = status_type()
        position = point()
        old_maximum = point_maximum()

        ! If we are not in the construct where flyspell should be active,
        ! invoke the original binding of M-TAB, if that was recorded.
        if (is_word_rejected()) then
            was_on = minor_modes%spellcheck_mode%enabled()
            call minor_modes%spellcheck_mode%set_state(.false.)

            if (allocated(this_command_keys)) then
                call key_binding(this_command_keys, next_command, status)
            end if

            call minor_modes%spellcheck_mode%set_state(was_on)
            if (.not. status%ok()) return

            if (allocated(next_command%a)) then
                call execute_interactively(next_command, status)
                return
            end if
        end if

        k = state()

        ! Flush a possibly stale cache from previous invocations of
        ! `auto_correct_spellcheck_word'/`auto_correct_previous_spellcheck_word'.
        if (.not. (associated(last_command, commands%auto_correct_spellcheck_word) .or. &
                   associated(last_command, commands%auto_correct_previous_spellcheck_word))) then
            states(k)%has_auto_correct_region = .false.
        end if

        ! Use the correct dictionary.
        call accept_spellcheck_buffer_local_definitions(.false., status)
        if (.not. status%ok()) return

        if (states(k)%auto_correct_position == position .and. states(k)%has_auto_correct_region) then
            ! We have already been using the function at the same location.
            start = states(k)%auto_correct_start
            length = states(k)%auto_correct_length
            call unhighlight_spellcheck_at(start, status)
            if (status%ok()) call delete_region(start, start + length, status)
            if (.not. status%ok()) return
            states(k)%ring_index = modulo(states(k)%ring_index, size(states(k)%auto_correct_ring)) + 1
            word = states(k)%auto_correct_ring(states(k)%ring_index)%text
            states(k)%auto_correct_length = count_characters(word)
            call goto_character(start)
            call spellcheck_insert_function(word, status)
            if (.not. status%ok()) return
            call spellcheck_word_highlight(status)
            if (.not. status%ok()) return
            call display_next_spellcheck_corrections(states(k)%auto_correct_ring, states(k)%ring_index)
            call adjust_spellcheck_cursor_point(position, point(), old_maximum)
            states(k)%auto_correct_position = point()
            return
        end if

        ! Fetch the word to be checked.
        call get_spellcheck_word(.false., word, start, end, found, status)
        if (.not. status%ok()) return

        if (found) then
            states(k)%auto_correct_word = word
            ! Now check spelling of word.
            call ask_speller(word, positions, status, .false.)
            if (.not. status%ok()) return

            select case (positions%kind)
            case (SPELLCHECK_OUTPUT_CORRECT, SPELLCHECK_OUTPUT_ROOT)
                ! Don't correct word.
                continue
            case (SPELLCHECK_OUTPUT_ERROR)
                ! Ispell error.
                call set_error(status, GENERIC_ERROR_CODE, "Ispell: error in Ispell process")
                return
            case default
                ! The word is incorrect, we have to propose a replacement.
                allocate (replacements(0))
                if (allocated(positions%misses)) replacements = positions%misses
                call sort_spellcheck_corrections(replacements, word)
                states(k)%has_auto_correct_region = .false.

                if (size(replacements) > 0) then
                    new_word = replacements(1)%text

                    if (new_word /= positions%word) then
                        ! Then save the current replacements.
                        states(k)%has_auto_correct_region = .true.
                        states(k)%auto_correct_start = start
                        states(k)%auto_correct_length = count_characters(new_word)
                        states(k)%auto_correct_ring = replacements
                        call append_string(states(k)%auto_correct_ring, positions%word)
                        states(k)%ring_index = 1
                        call unhighlight_spellcheck_at(start, status)
                        if (status%ok()) call delete_region(start, end, status)
                        if (status%ok()) call spellcheck_insert_function(new_word, status)
                        if (status%ok()) call spellcheck_word_highlight(status)
                        if (.not. status%ok()) return
                        call display_next_spellcheck_corrections(states(k)%auto_correct_ring, 1, new_word)
                        call adjust_spellcheck_cursor_point(position, point(), old_maximum)
                    end if
                end if
            end select

            states(k)%auto_correct_position = point()
            call spellcheck_pdict_save(status, no_query=.true.)
        end if
    end subroutine auto_correct_spellcheck_word

    !> Hook to track successive calls to
    !> `auto_correct_previous_spellcheck_word'.  Sets
    !> `spellcheck_auto_correct_previous_position' to none.  Port of
    !> flyspell.el:flyspell-auto-correct-previous-hook.
    recursive subroutine spellcheck_auto_correct_previous_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call pre_command_hook%remove(spellcheck_auto_correct_previous_hook, local=.true.)

        if (.not. associated(this_command, commands%auto_correct_previous_spellcheck_word)) then
            spellcheck_auto_correct_previous_position = NO_POSITION
        end if
    end subroutine spellcheck_auto_correct_previous_hook

    !> Auto correct the first misspelled word that occurs before point
    !> (POSITION).  But don't look beyond what's visible on the screen.
    !> Port of flyspell.el:flyspell-auto-correct-previous-word.
    recursive subroutine auto_correct_previous_spellcheck_word(position, status)
        use spellcheck_mod, only: spellcheck_following_word
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        integer(int64) :: top, bot, saved, beginning_of_accessible_portion, end_of_accessible_portion
        type(handle_type), allocatable :: overlay_list(:)
        type(handle_type) :: window
        integer, allocatable :: order(:)
        logical :: saved_following, got
        type(status_type) :: st
        integer :: k, j, best

        window = selected_window()
        top = window_start(windows(window%index))
        bot = window_end(window, st)
        saved = point()
        beginning_of_accessible_portion = point_minimum()
        end_of_accessible_portion = point_maximum()
        call narrow_to_region(top, bot, status)
        if (.not. status%ok()) return
        call overlay_recenter(point())
        call pre_command_hook%append(spellcheck_auto_correct_previous_hook, local=.true.)

        if (spellcheck_auto_correct_previous_position == NO_POSITION) then
            ! Only reset if a new overlay exists.
            call overlays_in(point_minimum(), position, overlay_list)
            ! Sorted by overlay start, the last starting first.
            allocate (order(size(overlay_list)))
            order = [(k, k=1, size(overlay_list))]

            do k = 2, size(order)
                best = order(k)
                j = k - 1

                do while (j >= 1)
                    if (overlay_start(overlay_list(order(j))) >= overlay_start(overlay_list(best))) exit
                    order(j + 1) = order(j)
                    j = j - 1
                end do

                order(j + 1) = best
            end do

            ! Search for previous (new) flyspell overlay.
            got = .false.

            do k = 1, size(order)
                if (.not. is_spellcheck_overlay(overlay_list(order(k)))) cycle
                ! Check if its face has changed.
                if (.not. has_incorrect_face(overlay_start(overlay_list(order(k))))) cycle
                ! The length of the word may change so go to the start.
                spellcheck_auto_correct_previous_position = overlay_start(overlay_list(order(k)))
                got = .true.
                exit
            end do
        end if

        if (spellcheck_auto_correct_previous_position /= NO_POSITION) then
            call goto_character(spellcheck_auto_correct_previous_position)
            ! Point is at start.
            saved_following = spellcheck_following_word
            spellcheck_following_word = .true.
            call goto_character(spellcheck_auto_correct_previous_position)
            call auto_correct_spellcheck_word(status)
            spellcheck_following_word = saved_following
            ! The point may have moved so reset this.
            spellcheck_auto_correct_previous_position = point()
        end if

        call widen()
        call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, st)
        call goto_character(saved)

    contains

        ! Whether the face at POSITION is `spellcheck_incorrect'.
        logical function has_incorrect_face(position_value)
            use text_properties_mod, only: get_character_property
            integer(int64), intent(in) :: position_value
            type(value_type) :: face

            face = get_character_property(position_value, value_type(name=P_FACE))
            has_incorrect_face = .false.
            if (vtag(face) == V_NAME) has_incorrect_face = symbols(xsymbol(face))%name == trim(spellcheck_incorrect%name)
        end function has_incorrect_face
    end subroutine auto_correct_previous_spellcheck_word

    ! The command `auto_correct_previous_spellcheck_word'.
    recursive subroutine execute_auto_correct_previous_spellcheck_word(status)
        type(status_type), intent(out) :: status

        call auto_correct_previous_spellcheck_word(point(), status)
    end subroutine execute_auto_correct_previous_spellcheck_word

    ! Move point after a correction: back to SAVE, shifted by the change in
    ! size if SAVE was at or after CURSOR_LOCATION.  Port of
    ! flyspell.el:flyspell-adjust-cursor-point.
    subroutine adjust_spellcheck_cursor_point(save, cursor_location, old_maximum)
        integer(int64), intent(in) :: save, cursor_location, old_maximum
        integer(int64) :: new_position

        if (save >= cursor_location) then
            new_position = save + (point_maximum() - old_maximum)
            call goto_character(max(point_minimum(), min(new_position, point_maximum())))
        else
            call goto_character(save)
        end if
    end subroutine adjust_spellcheck_cursor_point

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define Flyspell mode, its keymap, faces, commands and the defaults
    !> of the options, and put `spellcheck_kill_spellcheck_hook' on
    !> `spellcheck_kill_spellcheck_hook_variable'.  Not a port.
    subroutine initialize_spellcheck_mode()
        type(status_type) :: st
        type(named_face_type), pointer :: f

        commands%spellcheck_mode%execute => spellcheck_mode
        commands%spellcheck_programming_mode%execute => spellcheck_programming_mode
        commands%spellcheck_word_highlight%execute => execute_spellcheck_word_highlight
        commands%spellcheck_region_highlight%execute => execute_spellcheck_region_highlight
        commands%spellcheck_buffer_highlight%execute => spellcheck_buffer_highlight
        commands%goto_next_spellcheck_error%execute => execute_goto_next_spellcheck_error
        commands%auto_correct_spellcheck_word%execute => auto_correct_spellcheck_word
        commands%auto_correct_previous_spellcheck_word%execute => execute_auto_correct_previous_spellcheck_word
        commands%check_previous_highlighted_spellcheck_word%execute => execute_check_previous_highlighted_spellcheck_word
        commands%set_spellcheck_delayed_command%execute => execute_set_spellcheck_delayed_command
        commands%set_spellcheck_deplacement_command%execute => execute_set_spellcheck_deplacement_command
        commands%spellcheck_pre_command_hook%execute => spellcheck_pre_command_hook
        commands%spellcheck_post_command_hook%execute => spellcheck_post_command_hook
        commands%spellcheck_auto_correct_previous_hook%execute => spellcheck_auto_correct_previous_hook

        if (.not. allocated(spellcheck_mark_duplications_exceptions)) then
            ! Common defaults for English.
            spellcheck_mark_duplications_exceptions = [spellcheck_duplications_exception_type(language="", word="that"), &
                                                       spellcheck_duplications_exception_type(language="", word="had"), &
                                                       spellcheck_duplications_exception_type(language="\`francais", word="nous"), &
                                                       spellcheck_duplications_exception_type(language="\`francais", word="vous")]
        end if

        if (.not. allocated(spellcheck_default_delayed_commands)) then
            spellcheck_default_delayed_commands = [character(len=64) :: "self_insert_command", "delete_backward_character", &
                                                   "backward_or_forward_delete_character", "delete_character", &
                                                   "scrollbar_vertical_drag", &
                                                   "backward_delete_character_untabify"]
        end if

        if (.not. allocated(spellcheck_delayed_commands)) allocate (spellcheck_delayed_commands(0))

        if (.not. allocated(spellcheck_default_deplacement_commands)) then
            spellcheck_default_deplacement_commands = [character(len=64) :: "next_line", "previous_line", "handle_switch_frame", &
                                                       "handle_select_window", "scroll_up", "scroll_down"]
        end if

        if (.not. allocated(spellcheck_deplacement_commands)) allocate (spellcheck_deplacement_commands(0))
        if (.not. allocated(spellcheck_default_dictionary)) spellcheck_default_dictionary = ""

        if (.not. allocated(spellcheck_tex_command_regular_expression)) then
            spellcheck_tex_command_regular_expression = "\(\(begin\|end\)[ "//TAB &
                                                        //"]*{\|\(cite[a-z*]*\|label\|ref\|eqref\|usepackage\|"// &
                                                        "documentclass\)[ "//TAB//"]*\(\[[^]]*\]\)?{[^{}]*\)"
        end if

        if (.not. allocated(spellcheck_dictionaries_that_consider_dash_as_word_delimiter)) then
            spellcheck_dictionaries_that_consider_dash_as_word_delimiter = [character(len=32) :: "francais", "deutsch8", "norsk"]
        end if

        if (.not. allocated(spellcheck_mode_line_string)) spellcheck_mode_line_string = " Fly"
        if (.not. allocated(spellcheck_before_incorrect_word_string)) spellcheck_before_incorrect_word_string = ""
        if (.not. allocated(spellcheck_after_incorrect_word_string)) spellcheck_after_incorrect_word_string = ""

        if (.not. allocated(spellcheck_programming_text_faces)) then
            spellcheck_programming_text_faces = [spellcheck_face_reference_type(font_lock_string_face), &
                                                 spellcheck_face_reference_type(font_lock_comment_face), &
                                                 spellcheck_face_reference_type(font_lock_doc_face)]
        end if

        f => spellcheck_incorrect
        call f%define("spellcheck_incorrect", "Flyspell face for misspelled words.")
        call f%specification_entry(display_when(), face_attributes(underline_color="Red1", underline_style=FACE_STYLE_WAVE), &
                                   supports=face_attributes(underline_style=FACE_STYLE_WAVE))
        call f%specification_entry(display_when(), face_attributes(underline=.true., inherit="error"))
        f => spellcheck_duplicate
        call f%define("spellcheck_duplicate", "Flyspell face for words that appear twice in a row."//LF// &
                      "See also `spellcheck_duplicate_distance'.")
        call f%specification_entry(display_when(), face_attributes(underline_color="DarkOrange", underline_style=FACE_STYLE_WAVE), &
                                   supports=face_attributes(underline_style=FACE_STYLE_WAVE))
        call f%specification_entry(display_when(), face_attributes(underline=.true., inherit="warning"))
        call minor_modes%spellcheck_mode%define("spellcheck_mode", hook=spellcheck_mode_hook, keymap=.true., &
                                                body=spellcheck_mode_body)
        call add_minor_mode_lighter("spellcheck_mode", "", text=spellcheck_lighter)
        spellcheck_mode_map = minor_modes%spellcheck_mode%map
        call register_keymap_name("spellcheck_mode_map", spellcheck_mode_map)
        if (spellcheck_use_meta_tab) call keymap_set(spellcheck_mode_map, "M-TAB", AUTO_CORRECT_WORD_NAME, st)
        call keymap_set(spellcheck_mode_map, trim(spellcheck_auto_correct_binding), AUTO_CORRECT_PREVIOUS_WORD_NAME, st)
        call keymap_set(spellcheck_mode_map, "C-,", "goto_next_spellcheck_error", st)
        call keymap_set(spellcheck_mode_map, "C-.", AUTO_CORRECT_WORD_NAME, st)
        ! The `spellcheck_mode_test_function' of the modes (upstream's
        ! flyspell-mode-predicate properties: Mail and SGML modes; message,
        ! TeX, Texinfo and nXML modes are not ported).
        major_modes%mail_mode%spellcheck_mode_test_function => is_spellcheck_mail_text
        major_modes%sgml_mode%spellcheck_mode_test_function => is_spellcheck_sgml_text
        major_modes%html_mode%spellcheck_mode_test_function => is_spellcheck_sgml_text
        ! Make sure we flush our caches when needed.  Do it here rather than
        ! in `enable_spellcheck_mode', since `spellcheck_region_highlight' may be used
        ! without ever turning on flyspell-mode.
        call spellcheck_kill_spellcheck_hook_variable%prepend(spellcheck_kill_spellcheck_hook)
    end subroutine initialize_spellcheck_mode
end module spellcheck_mode_mod
