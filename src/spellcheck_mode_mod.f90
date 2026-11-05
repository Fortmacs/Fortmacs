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
!> spellcheck_mod's speller process.  `flyspell_mode' checks the words point
!> moves across (or, with `flyspell_check_changes', the words
!> edited) after each command and highlights the misspelled ones with
!> the `flyspell_incorrect' face, repeated ones with `flyspell_duplicate';
!> `flyspell_prog_mode' checks only comments and strings.
!> `flyspell_region' and `flyspell_buffer' check text (a large region
!> through a separate `ispell -l' run), `goto_next_flyspell_error' (C-,)
!> moves to the next highlighted word and `auto_correct_flyspell_word'
!> (M-TAB, C-.) cycles through the corrections of the word at point.
!> What is not ported is listed in docs/status/spellcheck_mode_mod.md.
module spellcheck_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR, format_message
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
    use editing_functions_mod, only: goto_char, point, point_min, point_max, line_end_position, is_bob, is_eob, delete_region, &
                                     buffer_substring_no_properties, char_after
    use editing_commands_mod, only: forward_char, backward_char, beginning_of_line
    use indent_mod, only: current_column
    use syntax_mod, only: char_syntax, forward_word, skip_chars_backward
    use simple_mod, only: backward_word, deactivate_mark
    use search_mod, only: re_search_forward, re_search_backward, search_forward, looking_at, string_match, &
                          match_beginning, match_end, regexp_quote, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
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
    use faces_mod, only: named_face_type, face_attrs, display_when, FACE_STYLE_WAVE, highlight
    use font_lock_mod, only: font_lock_string_face, font_lock_comment_face, font_lock_doc_face
    use keyboard_mod, only: post_command_hook, pre_command_hook, last_command, sit_for, sit_for_input, &
                            is_input_pending, has_unread_command_events, is_executing_kbd_macro, this_command_keys
    use execute_interactively_mod, only: this_command, execute_interactively, find_command
    use keymap_mod, only: keymap_set, key_binding, where_is_internal, key_description
    use help_mod, only: register_keymap_name
    use timer_mod, only: execute_with_idle_timer, cancel_timer
    use frame_mod, only: selected_window
    use window_mod, only: window_start, window_end, windows
    use editing_functions_mod, only: narrow_to_region, widen
    use display_engine_mod, only: message, add_minor_mode_lighter
    use spellcheck_mod, only: ispell_set_spellchecker_params, ispell_change_dictionary, ispell_accept_buffer_local_defs, &
                              ispell_send_string, ispell_accept_output, ispell_parse_output, ispell_output_type, &
                              ispell_filter_variable, ispell_process, ispell_quit, ispell_pdict_save, ispell_word, &
                              ispell_get_casechars, ispell_get_not_casechars, ispell_get_otherchars, &
                              ispell_get_many_otherchars, ispell_get_coding_system, ispell_get_extended_character_mode, &
                              ispell_get_ispell_args, ispell_buffer_session_localwords, ispell_execute_process_region, &
                              ispell_local_dictionary, ispell_dictionary, ispell_local_pdict, ispell_personal_dictionary, &
                              ispell_current_dictionary, ispell_current_personal_dictionary, ispell_extra_args, &
                              ispell_encoding8_command, ispell_really_aspell, ispell_really_hunspell, ispell_really_enchant, &
                              ispell_program_name, ispell_words_keyword, ispell_parser, ispell_highlight_face, &
                              ispell_kill_ispell_hook, ISPELL_PARSER_TEX, ISPELL_OUTPUT_CORRECT, ISPELL_OUTPUT_ROOT, &
                              ISPELL_OUTPUT_MISSPELLED, ISPELL_OUTPUT_ERROR
    use send_mail_mod, only: mail_header_separator
    use sgml_mode_mod, only: sgml_lexical_context, SGML_CONTEXT_TAG, SGML_CONTEXT_PI
    implicit none(type, external)
    private

    public :: flyspell_mode, flyspell_prog_mode, flyspell_word, flyspell_region, flyspell_buffer, &
              goto_next_flyspell_error, auto_correct_flyspell_word, auto_correct_previous_flyspell_word, &
              check_previous_highlighted_flyspell_word, set_flyspell_delayed_command, set_flyspell_deplacement_command, &
              set_flyspell_delayed_commands, set_flyspell_deplacement_commands, flyspell_pre_command_hook, &
              flyspell_post_command_hook, check_flyspell_changes, flyspell_auto_correct_previous_hook, &
              get_flyspell_word, is_flyspell_overlay, delete_all_flyspell_overlays, delete_flyspell_region_overlays, &
              is_flyspell_correction_less, is_flyspell_prog_text, is_flyspell_mail_text, is_flyspell_sgml_text, &
              set_local_flyspell_generic_check_word_test_function, flyspell_sort_procedure, flyspell_insert_procedure, &
              init_flyspell

    abstract interface
        !> A function of `flyspell_sort_corrections_function': whether
        !> CORRECTION1 sorts before CORRECTION2, the corrections of WORD.
        !> Not a port.
        logical function flyspell_sort_procedure(correction1, correction2, word)
            implicit none(type, external)
            character(len=*), intent(in) :: correction1, correction2, word
        end function flyspell_sort_procedure

        !> A function of `flyspell_insert_function': insert TEXT at point.
        !> Not a port.
        recursive subroutine flyspell_insert_procedure(text, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: text
            type(status_type), intent(out) :: status
        end subroutine flyspell_insert_procedure
    end interface

    ! ------------------------------------------------------------------
    ! User options
    ! ------------------------------------------------------------------

    !> How Flyspell should indicate misspelled words.
    !> .true. means use highlight, .false. means use minibuffer messages.
    !> Port of flyspell.el flyspell-highlight-flag.
    logical, public :: flyspell_highlight_flag = .true.

    !> .true. means Flyspell reports a repeated word as an error.
    !> See `flyspell_mark_duplications_exceptions' to add exceptions to
    !> this rule.  Detection of repeated words is not implemented in
    !> "large" regions; see variable `flyspell_large_region_variable'.
    !> Port of flyspell.el flyspell-mark-duplications-flag.
    logical, public :: flyspell_mark_duplications_flag = .true.

    !> .true. means Flyspell matches duplicate words case-insensitively.
    !> Port of flyspell.el flyspell-case-fold-duplications.
    logical, public :: flyspell_case_fold_duplications = .true.

    !> An element of `flyspell_mark_duplications_exceptions': a WORD that
    !> may be repeated when the dictionary matches LANGUAGE.  Not a port.
    type, public :: flyspell_duplications_exception_type
        character(len=64) :: language = ""
        character(len=32) :: word = ""
    end type flyspell_duplications_exception_type

    !> A list of exceptions for duplicated words, records of
    !> `flyspell_duplications_exception_type' (LANGUAGE, WORD).
    !>
    !> LANGUAGE can be "", which means the exception applies regardless of
    !> the current dictionary, or a regular expression matching the
    !> dictionary name (`ispell_local_dictionary' or `ispell_dictionary')
    !> for which the exception should apply.
    !>
    !> The checked word is downcased before comparing with WORD.  Port of
    !> flyspell.el flyspell-mark-duplications-exceptions (one record for
    !> each word of upstream's EXCEPTION-LIST).
    type(flyspell_duplications_exception_type), allocatable, public :: flyspell_mark_duplications_exceptions(:)

    !> If .true., sort the corrections before popping them.
    !> The sorting is controlled by the `flyspell_sort_corrections_function'
    !> variable, and defaults to sorting alphabetically.  Port of
    !> flyspell.el flyspell-sort-corrections.
    logical, public :: flyspell_sort_corrections = .false.

    !> The function used to sort corrections.
    !> This only happens if `flyspell_sort_corrections' is .true.  The
    !> function takes three parameters -- the two correction candidates
    !> to be sorted, and the third parameter is the word that's being
    !> corrected.  Port of flyspell.el flyspell-sort-corrections-function.
    procedure(flyspell_sort_procedure), pointer, public :: flyspell_sort_corrections_function => is_flyspell_correction_less

    !> The value of `flyspell_duplicate_distance' that searches the whole
    !> buffer.
    integer(int64), parameter, public :: FLYSPELL_DUPLICATE_DISTANCE_UNLIMITED = -1

    !> The maximum distance for finding duplicates of unrecognized words.
    !> This applies to the feature that when a word is not found in the
    !> dictionary, if the same spelling occurs elsewhere in the buffer,
    !> Flyspell uses a different face (`flyspell_duplicate') to highlight it.
    !> This variable specifies how far to search to find such a duplicate.
    !> FLYSPELL_DUPLICATE_DISTANCE_UNLIMITED (-1) means no limit (search
    !> the whole buffer).
    !> 0 means do not search for duplicate unrecognized spellings.  Port of
    !> flyspell.el flyspell-duplicate-distance.
    integer(int64), public :: flyspell_duplicate_distance = 400000

    !> The number of seconds to wait before checking, after a "delayed"
    !> command.  Port of flyspell.el flyspell-delay.
    real, public :: flyspell_delay = 3.0

    !> .true. means misspelled words remain highlighted until corrected.
    !> If this variable is .false., only the most recently detected
    !> misspelled word is highlighted, and the highlight is turned off as
    !> soon as point moves off the misspelled word.
    !>
    !> Make sure this variable is .true. if you use `flyspell_region'.
    !> Port of flyspell.el flyspell-persistent-highlight.
    logical, public :: flyspell_persistent_highlight = .true.

    !> .true. means highlight incorrect words even if a property exists
    !> for this word.  Port of flyspell.el flyspell-highlight-properties.
    logical, public :: flyspell_highlight_properties = .true.

    !> The standard list of delayed commands for Flyspell.
    !> See `flyspell_delayed_commands'.  Port of flyspell.el
    !> flyspell-default-delayed-commands.
    character(len=64), allocatable, public :: flyspell_default_delayed_commands(:)

    !> List of commands that are "delayed" for Flyspell mode.
    !> After these commands, Flyspell checking is delayed for a short time,
    !> whose length is specified by `flyspell_delay'.  Port of flyspell.el
    !> flyspell-delayed-commands.
    character(len=64), allocatable, public :: flyspell_delayed_commands(:)

    !> The standard list of deplacement commands for Flyspell.
    !> See variable `flyspell_deplacement_commands'.  Port of flyspell.el
    !> flyspell-default-deplacement-commands.
    character(len=64), allocatable, public :: flyspell_default_deplacement_commands(:)

    !> List of commands that are "deplacement" for Flyspell mode.
    !> After these commands, Flyspell checking is performed only if the
    !> previous command was not the very same command.  Port of flyspell.el
    !> flyspell-deplacement-commands.
    character(len=64), allocatable, public :: flyspell_deplacement_commands(:)

    !> .true. means that Flyspell should display a welcome message when
    !> started.  Port of flyspell.el flyspell-issue-welcome-flag.
    logical, public :: flyspell_issue_welcome_flag = .true.

    !> .true. means that Flyspell emits messages when checking words.
    !> Port of flyspell.el flyspell-issue-message-flag.
    logical, public :: flyspell_issue_message_flag = .true.

    !> A string that is the name of the default dictionary.
    !> This is passed to the `ispell_change_dictionary' when flyspell is
    !> started.  If the variable `ispell_local_dictionary' or
    !> `ispell_dictionary' is set when flyspell is started, the value of
    !> that variable is used instead of `flyspell_default_dictionary' to
    !> select the default dictionary.  Otherwise, if
    !> `flyspell_default_dictionary' is "", it means to use Ispell's
    !> ultimate default dictionary.  Port of flyspell.el
    !> flyspell-default-dictionary.
    character(len=:), allocatable, public :: flyspell_default_dictionary

    !> A string that is the regular expression that matches TeX commands.
    !> Port of flyspell.el flyspell-tex-command-regexp.
    character(len=:), allocatable, public :: flyspell_tex_command_regexp

    !> .true. means check even inside TeX math environment.
    !> TeX math environments are discovered by `texmathp', implemented
    !> inside AUCTeX package.  That package may be found at
    !> URL `https://www.gnu.org/software/auctex/'.  Port of flyspell.el
    !> flyspell-check-tex-math-command.
    logical, public :: flyspell_check_tex_math_command = .false.

    !> List of dictionary names that consider `-' as word delimiter.
    !> Port of flyspell.el
    !> flyspell-dictionaries-that-consider-dash-as-word-delimiter.
    character(len=32), allocatable, public :: flyspell_dictionaries_that_consider_dash_as_word_delimiter(:)

    !> String displayed on the mode line when flyspell is active.
    !> Set this to "" if you don't want a mode line indicator.  Port of
    !> flyspell.el flyspell-mode-line-string.
    character(len=:), allocatable, public :: flyspell_mode_line_string

    !> The setting `flyspell_large_region_variable': .false. or a number
    !> of characters (R6-39: `flyspell_large_region_variable%characters =
    !> 1000'; negative when no number is set).  Not a port.
    type, extends(choice_setting_type), public :: flyspell_large_region_setting_type
        integer(int64) :: characters = -1
    contains
        procedure :: clear_kinds => flyspell_large_region_clear_kinds
        procedure :: other_value => flyspell_large_region_other_value
        procedure :: kind_hint => flyspell_large_region_kind_hint
    end type flyspell_large_region_setting_type

    !> The threshold that determines if a region is small
    !> (`flyspell_large_region_variable%characters').
    !> If the region is smaller than this number of characters,
    !> `flyspell_region' checks the words sequentially using regular
    !> flyspell methods.  Else, if the region is large, a new Ispell
    !> process is spawned for speed.
    !>
    !> Doubled words are not detected in a large region, because Ispell
    !> does not check for them.
    !>
    !> If this variable is .false., all regions are treated as small.
    !> Port of flyspell.el flyspell-large-region.
    type(flyspell_large_region_setting_type), public :: flyspell_large_region_variable = &
                                        flyspell_large_region_setting_type(name="flyspell_large_region_variable", characters=1000, &
                                                                                           true_valid=.false.)

    !> Function for inserting word by flyspell upon correction.  Port of
    !> flyspell.el flyspell-insert-function.
    procedure(flyspell_insert_procedure), pointer, public :: flyspell_insert_function => insert

    !> String used to indicate an incorrect word starting ("" for none).
    !> Port of flyspell.el flyspell-before-incorrect-word-string.
    character(len=:), allocatable, public :: flyspell_before_incorrect_word_string

    !> String used to indicate an incorrect word ending ("" for none).
    !> Port of flyspell.el flyspell-after-incorrect-word-string.
    character(len=:), allocatable, public :: flyspell_after_incorrect_word_string

    !> .true. means that flyspell uses M-TAB to correct word.  (Read when
    !> `flyspell_mode_map' is made, at startup.)  Port of flyspell.el
    !> flyspell-use-meta-tab.
    logical, public :: flyspell_use_meta_tab = .true.

    !> The key binding for flyspell auto correction (`keymap_set' syntax;
    !> read when `flyspell_mode_map' is made, at startup).  Port of
    !> flyspell.el flyspell-auto-correct-binding.
    character(len=32), public :: flyspell_auto_correct_binding = "C-;"

    !> If .true., spell-check only words that were edited.
    !> By default, this is .false., and Flyspell checks every word across
    !> which you move point, even if you haven't edited the word.
    !> Customizing this option to .true. will not flag mis-spelled words
    !> across which you move point without editing them.  Port of
    !> flyspell.el flyspell-check-changes.
    logical, public :: flyspell_check_changes = .false.

    !> Whether Flyspell should use a timer for waiting after a delayed
    !> command.
    !>
    !> If this is .true., Flyspell sets up a timer for checking the word at
    !> point `flyspell_delay' seconds after you invoke a delayed command.
    !> Otherwise, if this option is .false., Flyspell uses `sit_for' to wait
    !> for that duration instead.  Port of flyspell.el
    !> flyspell-delay-use-timer.
    logical, public :: flyspell_delay_use_timer = .false.

    !> A pointer to a face, an element of `flyspell_prog_text_faces'.
    !> Not a port.
    type, public :: flyspell_face_ref_type
        type(named_face_type), pointer :: face => null()
    end type flyspell_face_ref_type

    !> Faces corresponding to text in programming-mode buffers.  Port of
    !> flyspell.el flyspell-prog-text-faces.
    type(flyspell_face_ref_type), allocatable, public :: flyspell_prog_text_faces(:)

    !> Flyspell face for misspelled words.  Port of flyspell.el
    !> flyspell-incorrect.
    type(named_face_type), target, public :: flyspell_incorrect

    !> Flyspell face for words that appear twice in a row.
    !> See also `flyspell_duplicate_distance'.  Port of flyspell.el
    !> flyspell-duplicate.
    type(named_face_type), target, public :: flyspell_duplicate

    !> Hook run after entering or leaving `flyspell_mode'.  Port of
    !> flyspell.el flyspell-mode-hook.
    type(hook_type), target, public :: flyspell_mode_hook = hook_type(name="flyspell_mode_hook")

    !> Hook run by `flyspell_prog_mode' after it turns Flyspell mode on.
    !> Port of flyspell.el flyspell-prog-mode-hook.
    type(hook_type), target, public :: flyspell_prog_mode_hook = hook_type(name="flyspell_prog_mode_hook")

    !> Minor mode keymap for Flyspell mode--for the whole buffer.  Port of
    !> flyspell.el flyspell-mode-map.
    type(handle_type), public :: flyspell_mode_map

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    ! A position upstream holds as nil.
    integer(int64), parameter :: NO_POSITION = -huge(1_int64)

    ! A change recorded by `record_flyspell_change' (an element of
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
        integer(int64) :: auto_correct_pos = NO_POSITION
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
    type(handle_type) :: flyspell_overlay
    ! flyspell-last-buffer.
    type(handle_type) :: flyspell_last_buffer
    ! flyspell-pre-buffer, flyspell-pre-column, flyspell-pre-pre-buffer and
    ! flyspell-pre-pre-point.
    type(handle_type) :: flyspell_pre_buffer, flyspell_pre_pre_buffer
    integer(int64) :: flyspell_pre_column = NO_POSITION, flyspell_pre_pre_point = NO_POSITION
    ! flyspell-previous-command.
    type(command_type), pointer :: flyspell_previous_command => null()
    ! flyspell--timer and the buffer its function checks.
    type(handle_type) :: flyspell_timer, flyspell_timer_buffer
    ! flyspell-external-ispell-buffer, flyspell-large-region-buffer,
    ! flyspell-large-region-beg and flyspell-large-region-end.
    type(handle_type) :: flyspell_external_ispell_buffer, flyspell_large_region_buffer
    integer(int64) :: flyspell_large_region_beginning = 1, flyspell_large_region_end = 1
    ! flyspell-old-buffer-error and flyspell-old-pos-error.
    type(handle_type) :: flyspell_old_buffer_error
    integer(int64) :: flyspell_old_pos_error = NO_POSITION
    ! flyspell-auto-correct-previous-pos.
    integer(int64) :: flyspell_auto_correct_previous_pos = NO_POSITION

    ! The names of the commands and properties used as values.
    character(len=*), parameter :: AUTO_CORRECT_WORD_NAME = "auto_correct_flyspell_word"
    character(len=*), parameter :: AUTO_CORRECT_PREVIOUS_WORD_NAME = "auto_correct_previous_flyspell_word"
    character(len=*), parameter :: P_FLYSPELL_OVERLAY = "flyspell_overlay", P_FACE = "face", P_LOCAL_MAP = "local_map"

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
    !> `flyspell_generic_check_word_test_function': the function providing
    !> per-mode customization over which words are flyspelled.  It returns
    !> .true. to continue checking, .false. otherwise.  Flyspell mode sets
    !> this variable to whatever is the `flyspell_mode_test_function' of
    !> the major mode.  Port of flyspell.el
    !> flyspell-generic-check-word-predicate (setting its local value).
    subroutine set_local_flyspell_generic_check_word_test_function(fn)
        procedure(mode_test_procedure) :: fn
        integer :: k

        k = state()
        if (k > 0) states(k)%test_function => fn
    end subroutine set_local_flyspell_generic_check_word_test_function

    ! Whether the current buffer's
    ! `flyspell_generic_check_word_test_function' is set and rejects the
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

    ! The dictionary in use: `ispell_local_dictionary', else
    ! `ispell_dictionary' ("" for the default).  Not a port.
    function dictionary() result(dict)
        character(len=:), allocatable :: dict

        dict = ""
        if (allocated(ispell_local_dictionary%value)) dict = ispell_local_dictionary%value
        if (len(dict) == 0) dict = ispell_dictionary
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

    !> Forget the characters of `flyspell_large_region_variable', as any
    !> assignment to it does.  Not a port.
    subroutine flyspell_large_region_clear_kinds(self)
        class(flyspell_large_region_setting_type), intent(inout) :: self

        self%characters = -1
    end subroutine flyspell_large_region_clear_kinds

    !> The characters of `flyspell_large_region_variable' as help shows
    !> them (`%characters = N'); "" when none is set.  Not a port.
    function flyspell_large_region_other_value(self) result(text)
        class(flyspell_large_region_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=40) :: component_text

        text = ""

        if (self%characters >= 0) then
            write (component_text, '("%characters = ", i0)') self%characters
            text = trim(component_text)
        end if
    end function flyspell_large_region_other_value

    !> The assignment that sets the number `flyspell_large_region_variable'
    !> takes, for the error of a rejected value.  Not a port.
    function flyspell_large_region_kind_hint(self, number) result(text)
        class(flyspell_large_region_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of characters is `"//trim(self%name)//"%characters = "//number//"'"
    end function flyspell_large_region_kind_hint

    ! ------------------------------------------------------------------
    ! Sorting corrections
    ! ------------------------------------------------------------------

    !> `flyspell_sort_corrections_function's default: whether CORRECTION1
    !> sorts alphabetically before CORRECTION2.  Port of
    !> flyspell.el:flyspell-sort-corrections-alphabetically.
    logical function is_flyspell_correction_less(correction1, correction2, word)
        character(len=*), intent(in) :: correction1, correction2, word

        if (len(word) < 0) continue
        is_flyspell_correction_less = llt(correction1, correction2)
    end function is_flyspell_correction_less

    !> CORRECTIONS of WORD, sorted with `flyspell_sort_corrections_function'
    !> if `flyspell_sort_corrections' is .true.  Port of
    !> flyspell.el:flyspell-sort (a stable insertion sort).
    subroutine sort_flyspell_corrections(corrections, word)
        type(string_type), intent(inout) :: corrections(:)
        character(len=*), intent(in) :: word
        type(string_type) :: moving
        integer :: i, j

        if (.not. flyspell_sort_corrections) return

        do i = 2, size(corrections)
            moving = corrections(i)
            j = i - 1

            do while (j >= 1)
                if (.not. flyspell_sort_corrections_function(moving%text, corrections(j)%text, word)) exit
                corrections(j + 1) = corrections(j)
                j = j - 1
            end do

            corrections(j + 1) = moving
        end do
    end subroutine sort_flyspell_corrections

    ! ------------------------------------------------------------------
    ! Mode specific tests
    ! ------------------------------------------------------------------

    !> Function used for `flyspell_generic_check_word_test_function' in
    !> Mail mode.  Port of flyspell.el:mail-mode-flyspell-verify (message.el
    !> is not ported: its signature separator is not looked for).
    recursive logical function is_flyspell_mail_text() result(check)
        integer(int64) :: saved, header_end, signature_begin, found, end
        type(status_type) :: st

        saved = point()
        call goto_char(point_min())
        call re_search_forward("^\(?:"//regexp_quote(mail_header_separator)//"\)?$", found, st, noerror=SEARCH_NOERROR)
        header_end = point()
        call goto_char(saved)
        signature_begin = point_max()

        if (point() < header_end) then
            call goto_char(saved)
            call beginning_of_line()
            check = looking_at("^Subject:", st)
            end = match_end(0, st)
            call goto_char(saved)
            if (check) check = point() > end
        else if (point() > signature_begin) then
            check = .false.
        else
            call beginning_of_line()
            check = .not. looking_at("[>}|]\|To:", st)
            call goto_char(saved)
        end if
    end function is_flyspell_mail_text

    !> Function used for `flyspell_generic_check_word_test_function' in
    !> SGML mode.  Tag and attribute names are not spell checked,
    !> everything else is.
    !>
    !> String values of attributes are checked because they can be text
    !> like <img alt="Some thing.">.  Port of
    !> flyspell.el:sgml-mode-flyspell-verify.
    recursive logical function is_flyspell_sgml_text() result(check)
        integer :: ctype
        integer(int64) :: start

        call sgml_lexical_context(ctype, start)
        check = ctype /= SGML_CONTEXT_TAG .and. ctype /= SGML_CONTEXT_PI
    end function is_flyspell_sgml_text

    !> Used for `flyspell_generic_check_word_test_function' in programming
    !> modes.  Port of flyspell.el:flyspell-generic-progmode-verify.
    recursive logical function is_flyspell_prog_text() result(check)
        ! Point might be in front of, inside, or behind the misspelled word.
        check = has_prog_face(point())
        if (.not. check .and. point() /= point_min()) check = has_prog_face(point() - 1)

    contains

        ! Whether the face of the text at POS is one of
        ! `flyspell_prog_text_faces'.
        logical function has_prog_face(pos)
            integer(int64), intent(in) :: pos
            type(value_type) :: face
            type(status_type) :: st
            integer :: k

            has_prog_face = .false.
            call get_text_property(pos, value_type(name=P_FACE), face, st)
            if (.not. st%ok() .or. vtag(face) /= V_NAME) return

            do k = 1, size(flyspell_prog_text_faces)
                if (.not. associated(flyspell_prog_text_faces(k)%face)) cycle

                if (trim(flyspell_prog_text_faces(k)%face%name) == symbols(xsymbol(face))%name) then
                    has_prog_face = .true.
                    return
                end if
            end do
        end function has_prog_face
    end function is_flyspell_prog_text

    !> Turn on `flyspell_mode' for comments and strings.  Port of
    !> flyspell.el:flyspell-prog-mode.
    recursive subroutine flyspell_prog_mode(status)
        type(status_type), intent(out) :: status

        call set_local_flyspell_generic_check_word_test_function(is_flyspell_prog_text)
        call minor_modes%flyspell_mode%enable(status)
        if (.not. status%ok()) return
        call flyspell_prog_mode_hook%execute(status)
    end subroutine flyspell_prog_mode

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
    !> code, `flyspell_prog_mode' is usually a better choice.
    !>
    !> Bindings:
    !> \[ispell_word]: correct words (using Ispell).
    !> \[auto_correct_flyspell_word]: automatically correct word.
    !> \[auto_correct_previous_flyspell_word]: automatically correct the last misspelled word.
    !>
    !> Hooks:
    !> This runs `flyspell_mode_hook' after flyspell mode is entered or exit.
    !>
    !> Remark:
    !> `flyspell_mode' uses `ispell_mode'.  Thus all Ispell options are
    !> valid.  For instance, a different dictionary can be used by
    !> invoking `ispell_change_dictionary'.
    !>
    !> Consider using the `ispell_parser' to check your text.  For instance
    !> consider setting it to ISPELL_PARSER_TEX in `tex_mode_hook'.
    !>
    !> \[flyspell_region] checks all words inside a region.
    !> \[flyspell_buffer] checks the whole buffer.  Port of
    !> flyspell.el:flyspell-mode.
    recursive subroutine flyspell_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%flyspell_mode%command(status)
    end subroutine flyspell_mode

    ! The body of Flyspell mode.  Port of flyspell.el:flyspell-mode (the
    ! body).
    recursive subroutine flyspell_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()

        if (enable) then
            call enable_flyspell_mode(mode_called_interactively, st)

            if (.not. st%ok()) then
                if (allocated(st%detail)) then
                    call message("Error enabling Flyspell mode:"//LF//st%detail)
                else
                    call message("Error enabling Flyspell mode:"//LF)
                end if

                call minor_modes%flyspell_mode%disable(status)
            end if
        else
            call disable_flyspell_mode(status)
        end if
    end subroutine flyspell_mode_body

    ! The mode line text of Flyspell mode in buffer slot BI: "" when it is
    ! off or `flyspell_mode_line_string' is "", else that string and the
    ! first two characters of the dictionary.  Port of flyspell.el's
    ! :lighter of flyspell-mode.
    subroutine flyspell_lighter(bi, text)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable, intent(out) :: text
        character(len=:), allocatable :: dict
        type(handle_type) :: saved
        type(status_type) :: st

        text = ""
        if (.not. minor_modes%flyspell_mode%enabled_in(bi)) return
        if (len(flyspell_mode_line_string) == 0) return
        saved = current_buffer()
        call set_buffer(buffer_handle(bi), st)
        dict = dictionary()
        if (saved%index /= 0) call set_buffer(saved, st)
        if (len(dict) == 0) dict = "--"
        text = flyspell_mode_line_string//"/"//dict(1:min(2, len(dict)))
    end subroutine flyspell_lighter

    !> Use the correct dictionary: accept the buffer's local definitions
    !> (`ispell_accept_buffer_local_defs') when the buffer has changed
    !> since the last flyspell operation, or with FORCE.  Port of
    !> flyspell.el:flyspell-accept-buffer-local-defs.
    recursive subroutine accept_flyspell_buffer_local_defs(force, status)
        logical, intent(in) :: force
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        character(len=:), allocatable :: local
        integer :: k

        status = status_type()
        if (.not. (force .or. .not. is_current(flyspell_last_buffer))) return
        flyspell_last_buffer = current_buffer()
        saved = current_buffer()
        call ispell_accept_buffer_local_defs(status)
        if (is_buffer_live(saved)) call set_buffer(saved, status)
        if (.not. status%ok()) return
        k = state()
        local = ""
        if (allocated(ispell_local_dictionary%value)) local = ispell_local_dictionary%value

        if (allocated(states(k)%dash_dictionary) .and. allocated(states(k)%dash_local_dictionary)) then
            if (states(k)%dash_dictionary == ispell_dictionary .and. states(k)%dash_local_dictionary == local) return
        end if

        ! The dictionary has changed.
        states(k)%dash_dictionary = ispell_dictionary
        states(k)%dash_local_dictionary = local
        states(k)%consider_dash = any(flyspell_dictionaries_that_consider_dash_as_word_delimiter == dictionary()) .and. &
                                  len(dictionary()) > 0
    end subroutine accept_flyspell_buffer_local_defs

    !> Flush Flyspell's caches when the Ispell process is killed.  Port of
    !> flyspell.el:flyspell-kill-ispell-hook.
    recursive subroutine flyspell_kill_ispell_hook(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        flyspell_last_buffer = null_handle()
        if (.not. allocated(states)) return

        do k = 1, size(states)
            if (allocated(states(k)%cache_word)) deallocate (states(k)%cache_word)
        end do
    end subroutine flyspell_kill_ispell_hook

    ! Turn Flyspell mode on.  If SHOW_MSG is true, show a welcome message
    ! if `flyspell_issue_message_flag' and `flyspell_issue_welcome_flag'
    ! are both true.  Port of flyspell.el:flyspell--mode-on.
    recursive subroutine enable_flyspell_mode(show_msg, status)
        logical, intent(in) :: show_msg
        type(status_type), intent(out) :: status
        type(value_type) :: keys
        type(value_type), allocatable :: events(:)

        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        ispell_highlight_face => flyspell_incorrect

        if (len(dictionary()) == 0 .and. len(flyspell_default_dictionary) > 0) then
            call ispell_change_dictionary(flyspell_default_dictionary, status)
            if (.not. status%ok()) return
        end if

        ! We have to force ispell to accept the local definition or
        ! otherwise it could be too late, the local dictionary may be
        ! forgotten!  Pass the `force' argument for the case where flyspell
        ! was active already but the buffer's local-defs have been edited.
        call accept_flyspell_buffer_local_defs(.true., status)
        if (.not. status%ok()) return
        call set_flyspell_delayed_commands()
        call set_flyspell_deplacement_commands()

        if (flyspell_check_changes) then
            call post_command_hook%append(check_flyspell_changes, local=.true.)
        else
            call post_command_hook%append(flyspell_post_command_hook, local=.true.)
        end if

        call pre_command_hook%append(flyspell_pre_command_hook, local=.true.)
        call after_change_functions%prepend(record_flyspell_change, local=.true.)
        call take_mode_test_function()

        if (flyspell_issue_message_flag .and. flyspell_issue_welcome_flag .and. show_msg) then
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

        ! The major mode's `flyspell_mode_test_function', if it has one,
        ! becomes the buffer's `flyspell_generic_check_word_test_function'.
        subroutine take_mode_test_function()
            use derived_mod, only: major_mode_type
            type(major_mode_type), pointer :: major

            if (.not. allocated(buffers(current)%major_mode)) return
            major => major_mode_named(buffers(current)%major_mode)
            if (.not. associated(major)) return
            if (.not. associated(major%flyspell_mode_test_function)) return
            call set_local_flyspell_generic_check_word_test_function(major%flyspell_mode_test_function)
        end subroutine take_mode_test_function
    end subroutine enable_flyspell_mode

    !> Install the standard set of Flyspell delayed commands.  Port of
    !> flyspell.el:flyspell-delay-commands.
    subroutine set_flyspell_delayed_commands()
        integer :: k

        do k = 1, size(flyspell_default_delayed_commands)
            call set_flyspell_delayed_command(trim(flyspell_default_delayed_commands(k)))
        end do

        do k = 1, size(flyspell_delayed_commands)
            call set_flyspell_delayed_command(trim(flyspell_delayed_commands(k)))
        end do
    end subroutine set_flyspell_delayed_commands

    !> Set COMMAND to be delayed, for Flyspell.
    !> When flyspell `post_command_hook' is invoked because a delayed
    !> command has been used, the current word is not immediately checked.
    !> It will be checked only after `flyspell_delay' seconds.  Port of
    !> flyspell.el:flyspell-delay-command (a name that is no command is
    !> left).
    subroutine set_flyspell_delayed_command(command)
        character(len=*), intent(in) :: command
        type(command_type), pointer :: p

        p => find_command(command)
        if (associated(p)) p%flyspell_delayed = .true.
    end subroutine set_flyspell_delayed_command

    ! The command `set_flyspell_delayed_command'.
    recursive subroutine cmd_set_flyspell_delayed_command(status)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Delay Flyspell after Command: ", name, status)
        if (status%ok()) call set_flyspell_delayed_command(name)
    end subroutine cmd_set_flyspell_delayed_command

    !> Install the standard set of Flyspell deplacement commands.  Port of
    !> flyspell.el:flyspell-deplacement-commands.
    subroutine set_flyspell_deplacement_commands()
        integer :: k

        do k = 1, size(flyspell_default_deplacement_commands)
            call set_flyspell_deplacement_command(trim(flyspell_default_deplacement_commands(k)))
        end do

        do k = 1, size(flyspell_deplacement_commands)
            call set_flyspell_deplacement_command(trim(flyspell_deplacement_commands(k)))
        end do
    end subroutine set_flyspell_deplacement_commands

    !> Set COMMAND that implement cursor movements, for Flyspell.
    !> When flyspell `post_command_hook' is invoked because a deplacement
    !> command has been used, the current word is not checked.  Port of
    !> flyspell.el:flyspell-deplacement-command (a name that is no command
    !> is left).
    subroutine set_flyspell_deplacement_command(command)
        character(len=*), intent(in) :: command
        type(command_type), pointer :: p

        p => find_command(command)
        if (associated(p)) p%flyspell_deplacement = .true.
    end subroutine set_flyspell_deplacement_command

    ! The command `set_flyspell_deplacement_command'.
    recursive subroutine cmd_set_flyspell_deplacement_command(status)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Deplacement Flyspell after Command: ", name, status)
        if (status%ok()) call set_flyspell_deplacement_command(name)
    end subroutine cmd_set_flyspell_deplacement_command

    !> Save the current buffer and point for Flyspell's post-command hook.
    !> Port of flyspell.el:flyspell-pre-command-hook.
    recursive subroutine flyspell_pre_command_hook(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        flyspell_pre_buffer = current_buffer()
        k = state()
        if (k > 0) states(k)%pre_point = point()
        flyspell_pre_column = current_column()
    end subroutine flyspell_pre_command_hook

    ! Turn Flyspell mode off.  Port of flyspell.el:flyspell--mode-off.
    recursive subroutine disable_flyspell_mode(status)
        type(status_type), intent(out) :: status
        integer :: k

        call post_command_hook%remove(check_flyspell_changes, local=.true.)
        call post_command_hook%remove(flyspell_post_command_hook, local=.true.)
        call pre_command_hook%remove(flyspell_pre_command_hook, local=.true.)
        call after_change_functions%remove(record_flyspell_change, local=.true.)
        call delete_all_flyspell_overlays(status)
        flyspell_pre_buffer = null_handle()
        k = state()
        if (k > 0) states(k)%pre_point = NO_POSITION
    end subroutine disable_flyspell_mode

    ! Whether S, a one-character string, matches OTHERCHARS (and
    ! OTHERCHARS is not "").  Not a port.
    logical function is_otherchar(otherchars, s)
        character(len=*), intent(in) :: otherchars, s
        type(status_type) :: st

        is_otherchar = .false.
        if (len(otherchars) == 0) return
        is_otherchar = string_match(otherchars, s, st) > 0
    end function is_otherchar

    ! The character after POS as a string ("" at the end of the buffer).
    ! Not a port.
    function char_string_at(pos) result(s)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: s
        type(status_type) :: st

        s = ""
        if (pos < point_min() .or. pos >= point_max()) return
        call buffer_substring_no_properties(pos, pos + 1, s, st)
    end function char_string_at

    ! Whether the character after POS has word syntax.  Not a port.
    logical function is_word_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        call char_after(c, found, pos)
        is_word_char_at = .false.
        if (found) is_word_char_at = char_syntax(int(c)) == "w"
    end function is_word_char_at

    !> Return .true. if we should check the word before point.
    !> More precisely, it applies to the word that was before point
    !> before the current command.  Port of
    !> flyspell.el:flyspell-check-pre-word-p.
    logical function is_flyspell_check_pre_word() result(check)
        character(len=:), allocatable :: otherchars
        integer(int64) :: pre_point
        integer(int32) :: c
        logical :: found
        integer :: k

        otherchars = ispell_get_otherchars()
        k = state()
        pre_point = states(k)%pre_point
        check = .false.

        if (pre_point == NO_POSITION .or. .not. is_current(flyspell_pre_buffer)) then
            return
        else if (flyspell_pre_pre_point == pre_point .and. same_handle(flyspell_pre_pre_buffer, flyspell_pre_buffer)) then
            return
        else if (pre_point == point() - 1 .and. &
                 (is_word_char_at(pre_point) .or. is_otherchar(otherchars, char_string_at(pre_point)))) then
            return
        else if (pre_point == point() .or. pre_point == point() + 1) then
            return
        end if

        if (is_named_command() .and. .not. is_executing_kbd_macro()) then
            if (this_command%flyspell_delayed .or. &
                (this_command%flyspell_deplacement .and. associated(flyspell_previous_command, this_command))) then
                call char_after(c, found, pre_point)
                if (current_column() == 0 .or. current_column() == flyspell_pre_column .or. &
                    ! If other post-command-hooks change the buffer,
                    ! flyspell-pre-point can lie past eob (bug#468).
                    .not. found) return
                if (is_word_char_at(pre_point) .or. is_otherchar(otherchars, char_string_at(pre_point))) return
            end if
        end if

        if (.not. is_current(flyspell_pre_buffer)) then
            check = .true.
        else if (states(k)%cache_start == NO_POSITION .or. states(k)%cache_end == NO_POSITION) then
            check = .true.
        else
            check = pre_point < states(k)%cache_start .or. pre_point > states(k)%cache_end
        end if
    end function is_flyspell_check_pre_word

    ! Whether H is the current buffer.  Not a port.
    logical function is_current(h)
        type(handle_type), intent(in) :: h

        is_current = same_handle(h, current_buffer())
    end function is_current

    !> Save the current buffer and point for Flyspell's post-command hook:
    !> record the change from START to STOP.  Port of
    !> flyspell.el:flyspell-after-change-function.
    recursive subroutine record_flyspell_change(start, stop, old_len, status)
        integer(int64), intent(in) :: start, stop, old_len
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        if (old_len < 0) continue
        k = state()
        if (k > 0) states(k)%changes = [change_type(start=start, stop=stop), states(k)%changes]
    end subroutine record_flyspell_change

    !> Return .true. when the changed word has to be checked.
    !> The answer depends of several criteria.
    !> Mostly we check word delimiters.  Port of
    !> flyspell.el:flyspell-check-changed-word-p.
    logical function is_flyspell_check_changed_word(start, stop) result(check)
        integer(int64), intent(in) :: start, stop
        integer(int64) :: pre_point, pos
        integer(int32) :: c
        logical :: found, delimiter
        integer :: k

        k = state()
        pre_point = states(k)%pre_point
        call char_after(c, found, start)
        delimiter = .false.
        if (found) delimiter = (c == 10 .or. c == 32) .and. stop > start
        pos = point()
        check = .true.
        if (delimiter .or. pre_point == NO_POSITION) return

        if ((pre_point >= start .and. pre_point <= stop) .or. pos >= start .or. pos <= stop .or. pos == stop + 1) then
            check = .false.
        end if
    end function is_flyspell_check_changed_word

    !> Return .true. when the word at `point' has to be checked.
    !> The answer depends of several criteria.
    !> Mostly we check word delimiters.  Port of
    !> flyspell.el:flyspell-check-word-p.
    recursive logical function is_flyspell_check_word() result(check)
        character(len=:), allocatable :: otherchars
        integer(int64) :: saved
        type(status_type) :: st
        integer :: k

        otherchars = ispell_get_otherchars()
        k = state()

        if (point_max() - 1 <= point_min()) then
            ! The buffer is not filled enough.
            check = .false.
            return
        end if

        if (current_column() > 0 .and. current_column() /= flyspell_pre_column) then
            saved = point()
            call backward_char(st)
            check = looking_at(ispell_get_not_casechars(), st)
            if (check .and. len(otherchars) > 0) check = .not. looking_at(otherchars, st)
            if (check .and. .not. states(k)%consider_dash) check = .not. looking_at("-", st)
            call goto_char(saved)
            ! Yes because we have reached or typed a word delimiter.
            if (check) return
        end if

        check = .true.
        if (.not. is_named_command()) return

        if (this_command%flyspell_deplacement) then
            check = .not. associated(flyspell_previous_command, this_command)
        else if (this_command%flyspell_delayed) then
            ! In case we're using `delete-selection-mode', make the region
            ! be updated immediately.
            call deactivate_mark()
            ! The current command is not delayed, that is that we must check
            ! the word now.
            check = .not. has_unread_command_events()
            if (.not. check) return

            if (.not. flyspell_delay_use_timer) then
                check = sit_for_input(nint(flyspell_delay*1000.0, int32))
            else
                flyspell_timer_buffer = current_buffer()
                call execute_with_idle_timer(flyspell_timer, real(flyspell_delay, real64), .false., &
                                             check_word_when_idle, st)
                check = .false.
            end if
        end if
    end function is_flyspell_check_word

    ! The function of `flyspell_timer': check the word at point if the
    ! buffer is still current.  Port of the timer function of
    ! flyspell.el:flyspell-check-word-p.
    recursive subroutine check_word_when_idle(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_current(flyspell_timer_buffer)) call flyspell_word(status)
    end subroutine check_word_when_idle

    !> The `post_command_hook' used by flyspell to check a word on-the-fly.
    !>
    !> It is possible that we check several words:
    !> 1- the current word is checked if the test
    !>    `is_flyspell_check_word' is true
    !> 2- the word that used to be the current word before the
    !>    `this_command' is checked if:
    !>     a- the previous word is different from the current word
    !>     b- the previous word has not just been checked by the
    !>        previous `flyspell_post_command_hook'
    !> 3- the words changed by the `this_command' that are neither the
    !>    previous word nor the current word.  Port of
    !> flyspell.el:flyspell-post-command-hook.
    recursive subroutine flyspell_post_command_hook(status)
        type(status_type), intent(out) :: status
        type(command_type), pointer :: command
        type(change_type) :: change
        logical :: saved_deactivate
        integer(int64) :: saved
        integer :: k

        status = status_type()
        call cancel_flyspell_timer()
        if (.not. minor_modes%flyspell_mode%enabled()) return
        command => this_command
        ! Prevent anything we do from affecting the mark.
        saved_deactivate = deactivate_mark_variable
        deactivate_mark_variable = .false.
        k = state()

        if (associated(command, commands%transpose_chars) .and. states(k)%pre_point /= NO_POSITION) then
            saved = point()
            call goto_char(states(k)%pre_point - 1)
            call flyspell_word(status)
            call goto_char(saved)
        end if

        if (status%ok() .and. is_flyspell_check_pre_word()) then
            saved = point()
            call goto_char(states(k)%pre_point)
            call flyspell_word(status)
            call goto_char(saved)
        end if

        if (status%ok()) then
            if (is_flyspell_check_word()) then
                call flyspell_word(status)
                ! We remember which word we have just checked.  This will
                ! be used next time we will check a word to compare the next
                ! current word with the word that has been registered in the
                ! pre-command-hook that is these variables are used within
                ! the test `is_flyspell_check_pre_word'.
                flyspell_pre_pre_buffer = current_buffer()
                flyspell_pre_pre_point = point()
            else
                flyspell_pre_pre_buffer = null_handle()
                flyspell_pre_pre_point = NO_POSITION

                ! When a word is not checked because of a delayed command we
                ! do not disable the ispell cache.
                if (is_named_command()) then
                    if (this_command%flyspell_delayed) then
                        states(k)%cache_end = -1
                        states(k)%cache_result = .true.
                    end if
                end if
            end if
        end if

        do while (status%ok() .and. .not. is_input_pending() .and. size(states(k)%changes) > 0)
            change = states(k)%changes(1)

            if (is_flyspell_check_changed_word(change%start, change%stop)) then
                saved = point()
                call goto_char(change%start)
                call flyspell_word(status)
                call goto_char(saved)
            end if

            states(k)%changes = states(k)%changes(2:)
        end do

        flyspell_previous_command => command
        deactivate_mark_variable = saved_deactivate
    end subroutine flyspell_post_command_hook

    ! Cancel `flyspell_timer' (cl-callf cancel-timer flyspell--timer).
    ! Not a port.
    subroutine cancel_flyspell_timer()
        if (flyspell_timer%index /= 0) call cancel_timer(flyspell_timer)
        flyspell_timer = null_handle()
    end subroutine cancel_flyspell_timer

    !> Function to spell-check only edited words when point moves off the
    !> word.  This is installed by flyspell as `post_command_hook' when the
    !> user option `flyspell_check_changes' is .true.  It
    !> spell-checks a word on moving point from the word only if the word
    !> was edited before the move.  Port of
    !> flyspell.el:flyspell-check-changes.
    recursive subroutine check_flyspell_changes(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: word
        integer(int64) :: start, stop, word_start, word_end, saved
        logical :: found
        integer :: k

        status = status_type()
        if (.not. minor_modes%flyspell_mode%enabled()) return
        k = state()
        if (size(states(k)%changes) == 0) return
        start = states(k)%changes(1)%start
        stop = states(k)%changes(1)%stop
        saved = point()
        call get_flyspell_word(.false., word, word_start, word_end, found, status)
        call goto_char(saved)
        if (.not. status%ok()) return
        if (found .and. word_start <= start .and. word_end >= stop) return
        call goto_char(start)
        call flyspell_word(status)
        call goto_char(saved)
        states(k)%changes = states(k)%changes(1:0)
    end subroutine check_flyspell_changes

    ! ------------------------------------------------------------------
    ! Checking a word
    ! ------------------------------------------------------------------

    ! The corrections of POSS as the message shows them: a Fortran array
    ! of strings (fortran_text), or the root word as a string.  Not a port.
    function replacements_text(poss, word) result(text)
        type(ispell_output_type), intent(in) :: poss
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text
        type(string_type), allocatable :: corrections(:)
        type(value_type), allocatable :: elems(:)
        integer :: k

        if (poss%kind == ISPELL_OUTPUT_ROOT) then
            text = fortran_string(poss%word)
            return
        end if

        allocate (corrections(0))
        if (allocated(poss%misses)) corrections = poss%misses
        call sort_flyspell_corrections(corrections, word)
        allocate (elems(size(corrections)))

        do k = 1, size(corrections)
            elems(k) = value_type(text=corrections(k)%text)
        end do

        text = fortran_text(make_list(elems))
    end function replacements_text

    ! Report the misspelling of WORD (corrections POSS) in the echo area.
    ! Port of flyspell.el:flyspell-notify-misspell.
    subroutine notify_flyspell_misspelling(word, poss)
        character(len=*), intent(in) :: word
        type(ispell_output_type), intent(in) :: poss

        if (flyspell_issue_message_flag) then
            call message(format_message("misspelling `")//word//format_message("'  ")//replacements_text(poss, word))
        end if
    end subroutine notify_flyspell_misspelling

    ! The position of WORD before point, searching back to BOUND (no bound
    ! when absent); 0 if none.  With IGNORE_CASE, compare case-
    ! insensitively.  Port of flyspell.el:flyspell-word-search-backward.
    recursive integer(int64) function search_flyspell_word_backward(word, bound, ignore_case) result(r)
        character(len=*), intent(in) :: word
        integer(int64), intent(in), optional :: bound
        logical, intent(in), optional :: ignore_case
        character(len=:), allocatable :: not_casechars, word_regexp, found_word
        integer(int64) :: saved, found, p, lw_start, lw_end
        logical :: has_bound, fold, got
        integer(int64) :: b
        type(status_type) :: st

        r = 0
        saved = point()
        not_casechars = ispell_get_not_casechars()
        has_bound = .false.

        if (present(bound)) then
            if (bound > point_min()) then
                has_bound = .true.
                b = bound - 1
            end if
        end if

        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        word_regexp = "\(?:"//not_casechars//"\|\`\)"//regexp_quote(word)//not_casechars

        do while (r == 0)
            if (has_bound) then
                call re_search_backward(word_regexp, found, st, bound=b, noerror=SEARCH_NOERROR)
            else
                call re_search_backward(word_regexp, found, st, noerror=SEARCH_NOERROR)
            end if

            if (found == 0 .or. .not. st%ok()) exit
            if (.not. is_bob()) call forward_char(st)
            p = point()
            call get_flyspell_word(.false., found_word, lw_start, lw_end, got, st)

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
                call goto_char(p)
            end if
        end do

        call goto_char(saved)
    end function search_flyspell_word_backward

    ! The position of WORD after point, searching up to BOUND (no bound when
    ! absent); 0 if none.  Port of flyspell.el:flyspell-word-search-forward.
    recursive integer(int64) function search_flyspell_word_forward(word, bound) result(r)
        character(len=*), intent(in) :: word
        integer(int64), intent(in), optional :: bound
        character(len=:), allocatable :: not_casechars, word_regexp, found_word
        integer(int64) :: saved, found, p, lw_start, lw_end, b
        logical :: has_bound, got
        type(status_type) :: st

        r = 0
        saved = point()
        not_casechars = ispell_get_not_casechars()
        has_bound = .false.

        if (present(bound)) then
            if (bound < point_max()) then
                has_bound = .true.
                b = bound + 1
            end if
        end if

        word_regexp = not_casechars//regexp_quote(word)//"\(?:"//not_casechars//"\|\'\)"

        do while (r == 0)
            if (has_bound) then
                call re_search_forward(word_regexp, found, st, bound=b, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(word_regexp, found, st, noerror=SEARCH_NOERROR)
            end if

            if (found == 0 .or. .not. st%ok()) exit
            if (.not. is_eob()) call backward_char(st)
            p = point()
            call get_flyspell_word(.false., found_word, lw_start, lw_end, got, st)
            if (got) got = found_word == word

            if (got) then
                r = p
            else
                call goto_char(p + 1)
            end if
        end do

        call goto_char(saved)
    end function search_flyspell_word_forward

    ! Send WORD to the speller in verbose mode and parse its answer into
    ! POSS (an empty answer is the word valid); with QUIET_EXIT, mark the
    ! process so it can be killed when Fortmacs is exited without query.
    ! SECONDS is how long each wait for output lasts (none: until output).
    ! Port of the speller exchange of flyspell.el:flyspell-word,
    ! flyspell-auto-correct-word and flyspell-correct-word-before-point.
    recursive subroutine ask_speller(word, poss, status, quiet_exit, seconds)
        character(len=*), intent(in) :: word
        type(ispell_output_type), intent(out) :: poss
        type(status_type), intent(out) :: status
        logical, intent(in) :: quiet_exit
        real, intent(in), optional :: seconds
        type(status_type) :: st
        integer :: n

        if (allocated(ispell_filter_variable)) deallocate (ispell_filter_variable)
        allocate (ispell_filter_variable(0))
        ! Put in verbose mode.
        call ispell_send_string("%"//LF, status)
        if (.not. status%ok()) return
        call ispell_send_string("^"//word//LF, status)
        if (.not. status%ok()) return
        if (quiet_exit) call set_process_query_on_exit_flag(ispell_process, .false., st)

        ! Wait until ispell has processed word.
        do
            call ispell_accept_output(status, seconds)
            if (.not. status%ok()) return
            n = size(ispell_filter_variable)

            if (n > 0) then
                if (len(ispell_filter_variable(n)%text) == 0) exit
            end if
        end do

        ! Remove leading empty element.
        ispell_filter_variable = ispell_filter_variable(1:n - 1)
        n = n - 1

        ! Ispell process should return something after word is sent.  Tag
        ! word as valid (i.e., skip) otherwise.
        if (n == 0) then
            call ispell_parse_output("*", poss)
        else
            call ispell_parse_output(ispell_filter_variable(n)%text, poss)
        end if
    end subroutine ask_speller

    ! Whether WORD may be repeated with the current dictionary
    ! (`flyspell_mark_duplications_exceptions').  Not a port.
    logical function is_duplication_exception(word) result(exception)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: dict, lower
        type(status_type) :: st
        integer :: k
        logical :: applies

        exception = .false.
        dict = dictionary()
        lower = downcase_string(word)

        do k = 1, size(flyspell_mark_duplications_exceptions)
            associate (except => flyspell_mark_duplications_exceptions(k))
                applies = len_trim(except%language) == 0

                if (.not. applies .and. len(dict) > 0) then
                    applies = string_match(trim(except%language), dict, st) > 0
                end if

                if (applies .and. trim(except%word) == lower) then
                    exception = .true.
                    return
                end if
            end associate
        end do
    end function is_duplication_exception

    !> The command `flyspell_word': check the word before point, or the
    !> following word when `ispell_following_word' is true.
    recursive subroutine cmd_flyspell_word(status)
        use spellcheck_mod, only: ispell_following_word
        type(status_type), intent(out) :: status

        call flyspell_word(status, ispell_following_word)
    end subroutine cmd_flyspell_word

    !> Spell check a word.
    !> If the optional argument FOLLOWING, or, when called interactively
    !> `ispell_following_word', is true, checks the following (rather than
    !> preceding) word when the cursor is not over a word.  If optional
    !> argument KNOWN_MISSPELLING is true considers word a misspelling and
    !> skips redundant spell-checking step.  CORRECT, if present, is false
    !> when the word was found misspelled or repeated.
    !>
    !> See `get_flyspell_word' for details of how this finds the word to
    !> spell-check.  Port of flyspell.el:flyspell-word.
    recursive subroutine flyspell_word(status, following, known_misspelling, correct)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: following, known_misspelling
        logical, intent(out), optional :: correct
        character(len=:), allocatable :: word
        integer(int64) :: saved, cursor_location, start, end, hstart, hend, p, bound
        type(ispell_output_type) :: poss
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
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        saved = point()
        ! Use the correct dictionary.
        call accept_flyspell_buffer_local_defs(.false., status)

        if (.not. status%ok()) then
            call goto_char(saved)
            return
        end if

        k = state()
        cursor_location = point()
        call get_flyspell_word(is_following, word, start, end, found, status)

        if (.not. status%ok() .or. .not. found) then
            call goto_char(saved)
            if (present(correct)) correct = .true.
            return
        end if

        if (is_word_rejected()) then
            call goto_char(saved)
            if (present(correct)) correct = .true.
            return
        end if

        ! Before checking in the directory, we check for doublons.
        duplicate = .false.

        if (flyspell_mark_duplications_flag .and. .not. is_duplication_exception(word)) then
            duplicate = ispell_parser%value /= ISPELL_PARSER_TEX

            if (.not. duplicate .and. start > point_min()) then
                call char_after(c, found, start - 1)
                duplicate = .not. (found .and. (c == iachar("}") .or. c == iachar("\")))
            end if

            if (duplicate) then
                call goto_char(start)
                bound = start - (end - start) + skip_chars_backward(" "//TAB//LF//FF, st)
                call goto_char(start)
                p = 0
                if (bound >= point_min()) p = search_flyspell_word_backward(word, bound, flyspell_case_fold_duplications)
                duplicate = p /= 0 .and. p /= start
            end if
        end if

        if (duplicate) then
            ! Yes, this is a doublon.
            call highlight_flyspell_incorrect_region(start, end, .true., status)
            ok = .false.
        else if (is_cache_hit()) then
            ! This word had been already checked, we skip.
            ok = states(k)%cache_result
        else if (ispell_parser%value == ISPELL_PARSER_TEX .and. is_flyspell_tex_command(start)) then
            ! This is a correct word (because a tex command).
            call unhighlight_word()
        else
            ! We setup the cache.
            states(k)%cache_start = start
            states(k)%cache_end = end
            states(k)%cache_word = word

            ! Now check spelling of word.
            if (.not. known) then
                call ask_speller(word, poss, status, .true., 1.0)

                if (.not. status%ok()) then
                    call goto_char(saved)
                    return
                end if
            else
                ! Else, this was a known misspelling to begin with, and we
                ! should forge an ispell return value.
                poss%kind = ISPELL_OUTPUT_MISSPELLED
                poss%word = word
                poss%offset = 1
                allocate (poss%misses(0), poss%guesses(0))
            end if

            if (poss%kind == ISPELL_OUTPUT_CORRECT .or. (poss%kind == ISPELL_OUTPUT_ROOT .and. flyspell_highlight_flag) .or. &
                poss%kind == ISPELL_OUTPUT_ERROR) then
                ! Correct.
                states(k)%cache_result = .true.
                call unhighlight_word()
            else if (is_duplicate_misspelling()) then
                ! This is a misspelled word which occurs twice within
                ! flyspell-duplicate-distance.
                states(k)%cache_result = .false.

                if (flyspell_highlight_flag) then
                    call highlight_flyspell_duplicate_region(start, end, status)
                else
                    call message(format_message("duplicate `")//word//format_message("'"))
                end if

                ok = .false.
            else
                states(k)%cache_result = .false.

                ! Highlight the location as incorrect, including offset
                ! specified in POSS and only for the length of the
                ! misspelled word specified by POSS.
                if (flyspell_highlight_flag) then
                    hstart = start
                    hend = end

                    if (poss%kind == ISPELL_OUTPUT_MISSPELLED) then
                        hstart = start + poss%offset - 1
                        ! POSS includes the misspelled word; use that to
                        ! figure out how many characters to highlight.
                        if (allocated(poss%word)) hend = hstart + count_characters(poss%word)
                    end if

                    call highlight_flyspell_incorrect_region(hstart, hend, .false., status)
                else
                    call notify_flyspell_misspelling(word, poss)
                end if

                ok = .false.
            end if

            ! Return to original location.
            call goto_char(cursor_location)
            if (ispell_quit) ispell_quit = .false.
        end if

        call goto_char(saved)
        if (present(correct)) correct = ok

    contains

        logical function is_cache_hit()
            is_cache_hit = .false.
            if (states(k)%cache_start /= start .or. states(k)%cache_end /= end) return
            if (.not. allocated(states(k)%cache_word)) return
            is_cache_hit = states(k)%cache_word == word
        end function is_cache_hit

        subroutine unhighlight_word()
            call unhighlight_flyspell_at(start, status)
            if (end > start .and. status%ok()) call unhighlight_flyspell_at(end - 1, status)
        end subroutine unhighlight_word

        logical function is_duplicate_misspelling()
            integer(int64) :: here

            is_duplicate_misspelling = .false.
            here = point()

            if (flyspell_duplicate_distance < 0) then
                call goto_char(start)
                is_duplicate_misspelling = search_flyspell_word_backward(word, point_min()) /= 0

                if (.not. is_duplicate_misspelling) then
                    call goto_char(end)
                    is_duplicate_misspelling = search_flyspell_word_forward(word, point_max()) /= 0
                end if
            else if (flyspell_duplicate_distance > 0) then
                call goto_char(start)
                is_duplicate_misspelling = search_flyspell_word_backward(word, start - flyspell_duplicate_distance) /= 0

                if (.not. is_duplicate_misspelling) then
                    call goto_char(end)
                    is_duplicate_misspelling = search_flyspell_word_forward(word, end + flyspell_duplicate_distance) /= 0
                end if
            end if

            call goto_char(here)
        end function is_duplicate_misspelling
    end subroutine flyspell_word

    !> Whether point is within a TeX math environment, as AUCTeX's
    !> `texmathp' tells; always .false. here, as `texmathp' is not
    !> defined (AUCTeX is not part of Emacs).  Port of
    !> flyspell.el:flyspell-math-tex-command-p.
    logical function is_flyspell_math_tex_command()
        is_flyspell_math_tex_command = .false.
    end function is_flyspell_math_tex_command

    !> Return .true. if the word starting at START (point after it) is a
    !> TeX command.  Port of flyspell.el:flyspell-tex-command-p.
    recursive logical function is_flyspell_tex_command(start) result(tex)
        integer(int64), intent(in) :: start
        integer(int64) :: saved, found, back_end
        type(status_type) :: st

        saved = point()
        call re_search_backward("\\", found, st, bound=max(point() - 100, point_min()), noerror=SEARCH_NOERROR)
        tex = .false.

        if (found /= 0) then
            back_end = match_end(0, st)

            if (back_end == start) then
                tex = .true.
            else
                call goto_char(back_end)
                tex = looking_at(flyspell_tex_command_regexp, st)
                if (tex) tex = match_end(0, st) >= start
            end if
        end if

        call goto_char(saved)
        if (.not. tex) tex = is_flyspell_math_tex_command()
    end function is_flyspell_tex_command

    !> The word for spell-checking according to Ispell syntax, from START
    !> to END (FOUND false when there is none).
    !> Optional argument FOLLOWING true means to get the following (rather
    !> than preceding) word when the cursor is not over a word.  Optional
    !> argument EXTRA_OTHERCHARS is a regexp of characters that may be
    !> included as part of a word (see `ispell_dictionary_type').
    !>
    !> This finds the word to spell-check by searching for CASECHARS
    !> defined in `ispell_dictionaries' for the current dictionary.  Thus,
    !> the word could be far away from point if point is inside whitespace
    !> or punctuation characters, or in text that belongs to a different
    !> language.  Port of flyspell.el:flyspell-get-word.
    recursive subroutine get_flyspell_word(following, word, start, end, found, status, extra_otherchars)
        logical, intent(in) :: following
        character(len=:), allocatable, intent(out) :: word
        integer(int64), intent(out) :: start, end
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: extra_otherchars
        character(len=:), allocatable :: casechars, not_casechars, otherchars, word_regexp, extra
        integer(int64) :: at, prevpt
        logical :: many, did_it_once, has_extra, again

        word = ""
        start = 0
        end = 0
        found = .false.
        casechars = ispell_get_casechars()
        not_casechars = ispell_get_not_casechars()
        otherchars = ispell_get_otherchars()
        many = ispell_get_many_otherchars()
        has_extra = present(extra_otherchars)
        extra = ""
        if (has_extra) extra = extra_otherchars
        word_regexp = casechars//"+\("
        if (len(otherchars) > 0) word_regexp = word_regexp//otherchars//"?"
        if (has_extra) word_regexp = word_regexp//extra//"?"
        word_regexp = word_regexp//casechars//"+\)"

        if (many .or. has_extra) then
            word_regexp = word_regexp//"*"
        else
            word_regexp = word_regexp//"?"
        end if

        ! Find the word.
        if (.not. looking_at(casechars, status)) then
            if (following) then
                call re_search_forward(casechars, at, status, noerror=SEARCH_NOERROR)
            else
                call re_search_backward(casechars, at, status, noerror=SEARCH_NOERROR)
            end if
        end if

        if (.not. status%ok()) return
        ! Move to front of word.
        call re_search_backward(not_casechars, at, status, noerror=SEARCH_MOVE)
        if (.not. status%ok()) return
        did_it_once = .false.
        prevpt = NO_POSITION

        do
            again = .false.
            if (len(otherchars) > 0) again = looking_at(otherchars, status)
            if (.not. again .and. has_extra) again = looking_at(extra, status)
            if (.not. again .or. is_bob()) exit
            if (did_it_once .and. .not. many) exit
            if (prevpt == point()) exit

            if (has_extra .and. looking_at(extra, status)) then
                call backward_char(status)
                if (looking_at(casechars, status)) call re_search_backward(not_casechars, at, status, noerror=SEARCH_MOVE)
            else
                did_it_once = .true.
                prevpt = point()
                call backward_char(status)

                if (looking_at(casechars, status)) then
                    call re_search_backward(not_casechars, at, status, noerror=SEARCH_MOVE)
                else
                    call forward_char(status)
                end if
            end if

            if (.not. status%ok()) return
        end do

        ! Now mark the word and save to string.
        call re_search_forward(word_regexp, at, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok() .or. at == 0) return
        start = match_beginning(0, status)
        end = point()
        call buffer_substring_no_properties(start, end, word, status)
        found = status%ok()
    end subroutine get_flyspell_word

    ! ------------------------------------------------------------------
    ! Regions
    ! ------------------------------------------------------------------

    !> Flyspell text between BEGINNING and END.
    !>
    !> This function is intended to work on small regions, as determined by
    !> `flyspell_large_region_variable'.  Port of
    !> flyspell.el:flyspell-small-region.
    recursive subroutine flyspell_small_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved, lo, hi, here
        integer :: count
        character(len=40) :: text
        type(status_type) :: st

        saved = point()
        lo = min(beginning, end)
        hi = max(beginning, end)
        call goto_char(lo)
        count = 0

        do while (point() < hi)
            if (flyspell_issue_message_flag .and. count == 100) then
                write (text, '("Spell Checking...", i0, "%")') floor(100.0_real64*real(point() - lo, real64)/real(hi - lo, real64))
                call message(trim(text))
                count = 0
            else
                count = count + 1
            end if

            call flyspell_word(status)
            if (.not. status%ok()) exit
            call sit_for(0)
            here = point()
            call forward_word(1_int64)
            if (point() < hi .and. point() > here + 1) call backward_char(st)
        end do

        if (status%ok()) then
            call backward_char(status)
            if (flyspell_issue_message_flag) call message("Spell Checking completed.")
            if (status%ok()) call flyspell_word(status)
        end if

        call goto_char(saved)
    end subroutine flyspell_small_region

    !> Mark words from a buffer listing incorrect words in order of
    !> appearance.  The list of incorrect words should be in
    !> `flyspell_external_ispell_buffer'.  (We finish by killing that
    !> buffer and setting the variable to the null handle.)  The buffer to
    !> mark them in is `flyspell_large_region_buffer'.  Port of
    !> flyspell.el:flyspell-external-point-words.
    recursive subroutine mark_flyspell_external_words(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: words_not_found(:), pieces(:)
        character(len=:), allocatable :: casechars, otherchars, word_chars, word, found, note
        integer(int64) :: buffer_scan_pos, wordpos, at, found_start, found_end, saved
        logical :: many, keep, saved_fold, got, matched
        character(len=40) :: text
        integer(int32) :: c
        type(status_type) :: st
        integer :: k

        allocate (words_not_found(0))
        casechars = ispell_get_casechars()
        otherchars = ispell_get_otherchars()
        many = ispell_get_many_otherchars()
        word_chars = casechars//"+\("
        if (len(otherchars) > 0) word_chars = word_chars//otherchars//"?"
        word_chars = word_chars//casechars//"+\)"

        if (many) then
            word_chars = word_chars//"*"
        else
            word_chars = word_chars//"?"
        end if

        buffer_scan_pos = flyspell_large_region_beginning
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        call set_buffer(flyspell_external_ispell_buffer, status)
        if (.not. status%ok()) return
        call goto_char(point_min())

        ! Loop over incorrect words, in the order they were reported, which
        ! is also the order they appear in the buffer being checked.
        do
            call re_search_forward("\([^"//LF//"]+\)"//LF, at, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. at == 0) exit
            ! Bind WORD to the next one.
            call buffer_substring_no_properties(match_beginning(1, st), match_end(1, st), word, status)
            wordpos = point()

            if (flyspell_issue_message_flag) then
                write (text, '("Spell Checking...", i0, "% [")') floor(100.0_real64*real(point(), real64)/real(point_max(), real64))
                call message(trim(text)//word//"]")
            end if

            call set_buffer(flyspell_large_region_buffer, status)
            if (.not. status%ok()) exit
            call goto_char(buffer_scan_pos)
            keep = .true.

            ! Iterate on string search until string is found as word, not as
            ! substring.
            do while (keep)
                call search_forward(word, at, status, bound=flyspell_large_region_end, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit

                if (at /= 0) then
                    saved = point()
                    ! Move back into the match so `get_flyspell_word' will
                    ! find it.
                    call backward_char(st)
                    got = .false.
                    found = ""
                    ! Is this a word that matches the current dictionary?
                    if (looking_at(word_chars, st)) call get_flyspell_word(.false., found, found_start, found_end, got, st)
                    call goto_char(saved)

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
                        if (.not. matched .and. len(ispell_really_aspell) == 0 .and. &
                            len(ispell_really_hunspell) == 0 .and. len(ispell_really_enchant) == 0) then
                            call char_after(c, matched, found_start - 1)
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
                        ! don't match CASECHARS, because `flyspell_word' will
                        ! then consider as misspelling the preceding word
                        ! that matches CASECHARS.
                        if (got) call flyspell_word(status, known_misspelling=.true.)
                        ! Search for next misspelled word will begin from end
                        ! of last validated match.
                        buffer_scan_pos = point()
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
            call set_buffer(flyspell_external_ispell_buffer, status)
            if (.not. status%ok()) exit
        end do

        case_fold_search = saved_fold
        if (.not. status%ok()) return
        ! We are done.
        if (flyspell_issue_message_flag) call message("Spell Checking completed.")
        call set_buffer(flyspell_large_region_buffer, status)
        if (.not. status%ok()) return

        ! Warn about not found misspellings.
        do k = 1, size(words_not_found)
            call message(words_not_found(k)%text//": word not found")
        end do

        ! Kill and forget the buffer with the list of incorrect words.
        call close_buffer(flyspell_external_ispell_buffer, status)
        flyspell_external_ispell_buffer = null_handle()
    end subroutine mark_flyspell_external_words

    !> Prevent marking of words explicitly declared correct: remove the
    !> buffer's local words from MISSPELLINGS_BUFFER.  Port of
    !> flyspell.el:flyspell-process-localwords.
    recursive subroutine remove_flyspell_localwords(misspellings_buffer, status)
        type(handle_type), intent(in) :: misspellings_buffer
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: localwords(:)
        character(len=:), allocatable :: casechars, string
        integer(int64) :: saved, end, at
        logical :: saved_fold
        type(handle_type) :: original
        type(status_type) :: st
        integer :: k

        call ispell_buffer_session_localwords(localwords)
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        casechars = ispell_get_casechars()
        ! Get localwords from the original buffer.
        saved = point()
        call goto_char(point_min())

        ! Localwords parsing copied from ispell.el.
        do
            call search_forward(ispell_words_keyword, at, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. at == 0) exit
            end = line_end_position()

            ! Buffer-local words separated by a space, and can contain any
            ! character other than a space.  Not rigorous enough.
            do
                call re_search_forward(" *\([^ ]+\)", at, status, bound=end, noerror=SEARCH_NOERROR)
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

        call goto_char(saved)

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
                call goto_char(point_min())

                do
                    call re_search_forward("^"//localwords(k)%text//LF, at, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok() .or. at == 0) exit
                    call delete_region(match_beginning(0, st), match_end(0, st), status)
                    if (.not. status%ok()) exit
                end do

                if (.not. status%ok()) exit
            end do

            call goto_char(saved)
            call set_buffer(original, st)
        end if

        case_fold_search = saved_fold
    end subroutine remove_flyspell_localwords

    !> Check for adjacent duplicated words (doublons) in the given region.
    !> Port of flyspell.el:flyspell-check-region-doublons.
    recursive subroutine check_flyspell_region_doublons(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved, at

        saved = point()
        call goto_char(beginning)
        ! Make sure current word is checked.
        call flyspell_word(status)
        if (status%ok()) call backward_word(1_int64)

        do while (status%ok() .and. point() < end)
            call re_search_forward("\<\(\w+\)\>[ "//LF//TAB//FF//"]+\1\>", at, status, bound=end, noerror=SEARCH_MOVE)
            if (.not. status%ok() .or. at == 0) exit
            call flyspell_word(status)
            if (status%ok()) call backward_word(1_int64)
        end do

        if (status%ok()) call flyspell_word(status)
        call goto_char(saved)
    end subroutine check_flyspell_region_doublons

    !> Check the region from BEGINNING to END with a separate speller run
    !> (`ispell -l', `aspell list'), then mark the words it reports.
    !> Port of flyspell.el:flyspell-large-region (the region is sent as
    !> the buffer holds it).
    recursive subroutine flyspell_large_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(handle_type) :: curbuf, buffer
        type(value_type), allocatable :: elems(:)
        type(string_type), allocatable :: args(:)
        character(len=64) :: dict_args(4)
        character(len=:), allocatable :: encoding, extended, personal
        type(value_type) :: c
        integer(int64) :: at
        type(status_type) :: st
        integer :: k

        curbuf = current_buffer()
        call get_or_make_buffer("*flyspell-region*", buffer, status)
        if (.not. status%ok()) return
        flyspell_external_ispell_buffer = buffer
        flyspell_large_region_buffer = curbuf
        flyspell_large_region_beginning = beginning
        flyspell_large_region_end = end
        call accept_flyspell_buffer_local_defs(.false., status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (status%ok()) call erase_buffer(status)
        ! This is done, we can start checking...
        if (flyspell_issue_message_flag) call message("Checking region...")
        call set_buffer(curbuf, st)
        if (.not. status%ok()) return
        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        ! Local dictionary becomes the global dictionary in use.
        ispell_current_dictionary = dictionary()
        personal = ""
        if (allocated(ispell_local_pdict%value)) personal = ispell_local_pdict%value
        if (len(personal) == 0) personal = ispell_personal_dictionary
        ispell_current_personal_dictionary = personal
        call ispell_get_ispell_args(dict_args)
        encoding = ispell_get_coding_system()
        allocate (args(0))

        do k = 1, size(dict_args)
            if (len_trim(dict_args(k)) > 0) call append_string(args, trim(dict_args(k)))
        end do

        ! Use specified dictionary, only define if not overridden.
        if (len(ispell_current_dictionary) > 0 .and. .not. any(dict_args == "-d")) then
            call prepend_string(args, ispell_current_dictionary)
            call prepend_string(args, "-d")
        end if

        ! Use specified pers dict.
        if (len(ispell_current_personal_dictionary) > 0) then
            call expand_file_name(ispell_current_personal_dictionary, personal, status)
            if (.not. status%ok()) return
            call append_string(args, "-p", personal)
        end if

        ! Check for extended character mode.
        extended = ispell_get_extended_character_mode()

        if (len(extended) > 0) then
            if (string_match("[^~]+$", extended, st) > 0) then
                personal = "-T"//extended(match_beginning(0, st):match_end(0, st) - 1)
                if (.not. any([(args(k)%text == personal, k=1, size(args))])) call prepend_string(args, personal)
            end if
        end if

        ! Add ispell-extra-args.
        do k = 1, size(ispell_extra_args)
            call append_string(args, trim(ispell_extra_args(k)))
        end do

        ! If we are using recent aspell or hunspell, make sure we use the
        ! right encoding for communication.  ispell or older aspell/hunspell
        ! does not support this.
        if (len(ispell_encoding8_command) > 0) then
            if (len(ispell_really_hunspell) > 0) then
                call append_string(args, ispell_encoding8_command, upcase_string(encoding))
            else
                call append_string(args, ispell_encoding8_command//encoding)
            end if
        end if

        allocate (elems(size(args) + 1))

        if (len(ispell_really_aspell) > 0) then
            elems(1) = value_type(text="list")
        else
            elems(1) = value_type(text="-l")
        end if

        do k = 1, size(args)
            elems(k + 1) = value_type(text=args(k)%text)
        end do

        call ispell_execute_process_region(beginning, end, ispell_program_name, "*flyspell-region*", make_list(elems), c, &
                                           status)
        if (.not. status%ok()) return
        at = -1
        if (vtag(c) /= V_NAME .and. allocated(c%a)) call integer_result(c, at)

        if (at == 0) then
            call remove_flyspell_localwords(buffer, status)
            if (status%ok()) call delete_flyspell_region_overlays(beginning, end, status)
            if (status%ok()) call check_flyspell_region_doublons(beginning, end, status)
            if (status%ok()) call mark_flyspell_external_words(status)
        else
            call set_error(status, FM_GENERIC_ERROR, "Can't check region")
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
    end subroutine flyspell_large_region

    !> Flyspell text between BEGINNING and END.
    !>
    !> Make sure `flyspell_mode' is turned on if you want the highlight of a
    !> misspelled word removed when you've corrected it.  Port of
    !> flyspell.el:flyspell-region.
    recursive subroutine flyspell_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved, lo, hi

        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        if (beginning == end) return
        saved = point()
        lo = min(beginning, end)
        hi = max(beginning, end)

        if (flyspell_large_region_variable%characters >= 0 .and. hi - lo > flyspell_large_region_variable%characters) then
            call flyspell_large_region(lo, hi, status)
        else
            call flyspell_small_region(lo, hi, status)
        end if

        call goto_char(saved)
    end subroutine flyspell_region

    ! The command `flyspell_region'.
    recursive subroutine cmd_flyspell_region(status)
        use simple_mod, only: mark
        type(status_type), intent(out) :: status
        integer(int64) :: m

        call mark(m, status)
        if (.not. status%ok()) return

        if (m <= 0) then
            call set_error(status, FM_GENERIC_ERROR, "The mark is not set now, so there is no region")
            return
        end if

        call flyspell_region(min(point(), m), max(point(), m), status)
    end subroutine cmd_flyspell_region

    !> Flyspell whole buffer.  Port of flyspell.el:flyspell-buffer.
    recursive subroutine flyspell_buffer(status)
        type(status_type), intent(out) :: status

        call flyspell_region(point_min(), point_max(), status)
    end subroutine flyspell_buffer

    ! ------------------------------------------------------------------
    ! Errors and overlays
    ! ------------------------------------------------------------------

    !> Go to the next error.
    !> If PREVIOUS (interactively, the prefix), go to the previous error
    !> instead.
    !>
    !> In general `goto_next_flyspell_error' must be used after
    !> `flyspell_buffer'.  Port of flyspell.el:flyspell-goto-next-error.
    recursive subroutine goto_next_flyspell_error(status, previous)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: previous
        integer(int64) :: pos, max
        logical :: back
        type(handle_type), allocatable :: found(:)

        status = status_type()
        back = .false.
        if (present(previous)) back = previous
        pos = point()

        if (back) then
            max = point_min()
        else
            max = point_max()
        end if

        if (is_current(flyspell_old_buffer_error) .and. pos == flyspell_old_pos_error) then
            if (back) then
                if (flyspell_old_pos_error == max) then
                    call message("Restarting from end of the buffer")
                    call goto_char(point_max())
                else
                    call forward_word(-1_int64)
                end if
            else
                if (flyspell_old_pos_error == max) then
                    call message("Restarting from beginning of buffer")
                    call goto_char(point_min())
                else
                    call forward_word(1_int64)
                end if
            end if

            pos = point()
        end if

        ! Seek the next error.
        do while (pos /= max)
            if (back) then
                pos = previous_overlay_change(pos)
            else
                pos = next_overlay_change(pos)
            end if

            call overlays_at(pos, found)
            if (any_flyspell_overlay(found)) exit
        end do

        call goto_char(pos)
        ! Save the current location for next invocation.
        flyspell_old_pos_error = point()
        flyspell_old_buffer_error = current_buffer()
        if (point() == max) call message("No more miss-spelled words")
    end subroutine goto_next_flyspell_error

    ! The command `goto_next_flyspell_error'.
    recursive subroutine cmd_goto_next_flyspell_error(status)
        use execute_interactively_mod, only: current_prefix_arg, PREFIX_NONE
        type(status_type), intent(out) :: status

        call goto_next_flyspell_error(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_goto_next_flyspell_error

    ! Whether one of OVERLAYS is used by flyspell.  Not a port.
    logical function any_flyspell_overlay(overlays) result(found)
        type(handle_type), intent(in) :: overlays(:)
        integer :: k

        found = .false.

        do k = 1, size(overlays)
            if (is_flyspell_overlay(overlays(k))) then
                found = .true.
                return
            end if
        end do
    end function any_flyspell_overlay

    !> Return .true. if O is an overlay used by flyspell.  Port of
    !> flyspell.el:flyspell-overlay-p.
    logical function is_flyspell_overlay(o)
        type(handle_type), intent(in) :: o
        type(value_type) :: property

        is_flyspell_overlay = is_overlay(o)
        if (.not. is_flyspell_overlay) return
        property = overlay_get(o, P_FLYSPELL_OVERLAY)
        is_flyspell_overlay = allocated(property%a)
    end function is_flyspell_overlay

    !> Delete overlays used by flyspell in a given region.  Port of
    !> flyspell.el:flyspell-delete-region-overlays.
    subroutine delete_flyspell_region_overlays(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        call remove_overlays(beginning, end, P_FLYSPELL_OVERLAY, value_type(.true.), status)
    end subroutine delete_flyspell_region_overlays

    !> Delete all the overlays used by flyspell.  Port of
    !> flyspell.el:flyspell-delete-all-overlays.
    subroutine delete_all_flyspell_overlays(status)
        type(status_type), intent(out) :: status

        call delete_flyspell_region_overlays(point_min(), point_max(), status)
    end subroutine delete_all_flyspell_overlays

    !> Remove the flyspell overlay that are located at POS.  Port of
    !> flyspell.el:flyspell-unhighlight-at.
    subroutine unhighlight_flyspell_at(pos, status)
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: found(:)
        integer :: k

        status = status_type()

        if (flyspell_persistent_highlight) then
            call overlays_at(pos, found)

            do k = 1, size(found)
                if (is_flyspell_overlay(found(k))) call delete_overlay(found(k))
            end do
        else if (is_flyspell_overlay(flyspell_overlay)) then
            call delete_overlay(flyspell_overlay)
        end if
    end subroutine unhighlight_flyspell_at

    !> Return .true. if there is a text property at POS, not counting
    !> `local_map'.  If variable `flyspell_highlight_properties' is set to
    !> .false., text with properties are not checked.  This function is
    !> used to discover if the character at POS has any other property.
    !> Port of flyspell.el:flyspell-properties-at-p.
    logical function has_flyspell_properties_at(pos) result(has)
        integer(int64), intent(in) :: pos
        type(value_type) :: plist
        type(value_type), allocatable :: props(:)
        type(status_type) :: st
        integer :: k

        has = .false.
        call text_properties_at(pos, plist, st)
        if (.not. st%ok() .or. .not. allocated(plist%a)) return
        call list_elements(plist, props)
        k = 1

        do while (k <= size(props))
            if (vtag(props(k)) == V_NAME .and. k < size(props)) then
                if (symbols(xsymbol(props(k)))%name == P_LOCAL_MAP) then
                    k = k + 2
                    cycle
                end if
            end if

            has = .true.
            return
        end do
    end function has_flyspell_properties_at

    !> Allocate an overlay to highlight an incorrect word, in OVERLAY.
    !> BEGINNING and END specify the range in the buffer of that word.
    !> FACE and MOUSE_FACE specify the `face' and `mouse_face' properties
    !> for the overlay.  Port of flyspell.el:make-flyspell-overlay (the
    !> mouse keymap is not ported).
    subroutine make_flyspell_overlay(beginning, end, face, mouse_face, overlay, status)
        integer(int64), intent(in) :: beginning, end
        type(named_face_type), intent(in) :: face, mouse_face
        type(handle_type), intent(out) :: overlay
        type(status_type), intent(out) :: status

        call make_overlay(overlay, beginning, end, status, front_advance=.false., rear_advance=.true.)
        if (status%ok()) call overlay_put(overlay, "face", value_type(name=trim(face%name)), status)
        if (status%ok()) call overlay_put(overlay, "mouse_face", value_type(name=trim(mouse_face%name)), status)
        if (status%ok()) call overlay_put(overlay, P_FLYSPELL_OVERLAY, value_type(.true.), status)
        if (status%ok()) call overlay_put(overlay, "evaporate", value_type(.true.), status)
        if (status%ok()) call overlay_put(overlay, "help_echo", value_type(text="mouse-2: correct word at point"), status)
        if (.not. status%ok()) return

        if (trim(face%name) == trim(flyspell_incorrect%name)) then
            if (len(flyspell_before_incorrect_word_string) > 0) then
                call overlay_put(overlay, "before_string", value_type(text=flyspell_before_incorrect_word_string), status)
            end if

            if (status%ok() .and. len(flyspell_after_incorrect_word_string) > 0) then
                call overlay_put(overlay, "after_string", value_type(text=flyspell_after_incorrect_word_string), status)
            end if
        end if
    end subroutine make_flyspell_overlay

    !> Set up an overlay on a misspelled word, in the buffer from BEGINNING
    !> to END.  DOUBLON true means the word is itself incorrect, but
    !> suspiciously repeated.  Port of
    !> flyspell.el:flyspell-highlight-incorrect-region
    !> (`flyspell-incorrect-hook' is not ported).
    subroutine highlight_flyspell_incorrect_region(beginning, end, doublon, status)
        integer(int64), intent(in) :: beginning, end
        logical, intent(in) :: doublon
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: found(:)
        integer :: k

        status = status_type()
        if (.not. (flyspell_highlight_properties .or. .not. has_flyspell_properties_at(beginning))) return

        ! We cleanup all the overlay that are in the region, not beginning
        ! at the word start position.
        if (beginning + 1 < end) then
            call overlays_in(beginning + 1, end, found)

            do k = 1, size(found)
                if (is_flyspell_overlay(found(k))) call delete_overlay(found(k))
            end do
        end if

        ! We cleanup current overlay at the same position.
        call unhighlight_flyspell_at(beginning, status)

        ! Now we can use a new overlay.
        if (status%ok()) then
            if (doublon) then
                call make_flyspell_overlay(beginning, end, flyspell_duplicate, highlight, flyspell_overlay, status)
            else
                call make_flyspell_overlay(beginning, end, flyspell_incorrect, highlight, flyspell_overlay, status)
            end if
        end if
    end subroutine highlight_flyspell_incorrect_region

    !> Set up overlay on duplicate misspelled word, in the buffer from
    !> BEGINNING to END.  Port of
    !> flyspell.el:flyspell-highlight-duplicate-region
    !> (`flyspell-incorrect-hook' is not ported).
    subroutine highlight_flyspell_duplicate_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. (flyspell_highlight_properties .or. .not. has_flyspell_properties_at(beginning))) return
        ! We cleanup current overlay at the same position.
        call unhighlight_flyspell_at(beginning, status)
        ! Now we can use a new overlay.
        if (status%ok()) call make_flyspell_overlay(beginning, end, flyspell_duplicate, highlight, flyspell_overlay, status)
    end subroutine highlight_flyspell_duplicate_region

    ! ------------------------------------------------------------------
    ! Correcting words
    ! ------------------------------------------------------------------

    !> Correct the closest previous word that is highlighted as misspelled.
    !> This function scans for a word which starts before point that has
    !> been highlighted by Flyspell as misspelled.  If it finds one, it
    !> proposes a replacement for that word.  With prefix arg N (ARG),
    !> check the Nth word before point that's highlighted as misspelled.
    !> Port of flyspell.el:flyspell-check-previous-highlighted-word.
    recursive subroutine check_previous_highlighted_flyspell_word(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: pos1, pos, n, saved
        type(handle_type), allocatable :: found(:)
        logical :: hit
        integer :: k

        pos1 = point()
        pos = point()
        n = 1
        if (present(arg)) n = arg
        if (n < 1) n = 1
        hit = .false.

        outer: do
            pos = previous_overlay_change(pos)
            if (pos == pos1) exit outer
            pos1 = pos

            if (pos >= point_min()) then
                call overlays_at(pos, found)

                do k = 1, size(found)
                    if (is_flyspell_overlay(found(k))) then
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
            call set_error(status, FM_GENERIC_ERROR, "No word to correct before point")
            return
        end if

        saved = point()
        call goto_char(pos)
        call ispell_word(status)

        if (status%ok()) then
            ! Force `flyspell_word' re-check.
            k = state()
            if (allocated(states(k)%cache_word)) deallocate (states(k)%cache_word)
            call flyspell_word(status)
        end if

        call goto_char(saved)
    end subroutine check_previous_highlighted_flyspell_word

    ! The command `check_previous_highlighted_flyspell_word'.
    recursive subroutine cmd_check_previous_highlighted_flyspell_word(status)
        use execute_interactively_mod, only: current_prefix_arg, PREFIX_NUMBER, PREFIX_MINUS
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind == PREFIX_NUMBER) then
            call check_previous_highlighted_flyspell_word(status, current_prefix_arg%value)
        else if (current_prefix_arg%kind == PREFIX_MINUS) then
            call check_previous_highlighted_flyspell_word(status, -1_int64)
        else
            call check_previous_highlighted_flyspell_word(status)
        end if
    end subroutine cmd_check_previous_highlighted_flyspell_word

    ! Show CORRECTIONS (from the ring entry START on, cycling) in the echo
    ! area: "Corrections:" and the words while the text is shorter than 80
    ! characters.  Port of flyspell.el:flyspell-display-next-corrections
    ! (the misspelled word's `flyspell_incorrect' face in the message is
    ! not ported).
    subroutine display_next_flyspell_corrections(ring, start, first)
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
    end subroutine display_next_flyspell_corrections

    !> Correct the current word.
    !> This command proposes various successive corrections for the
    !> current word.  If invoked repeatedly on the same position, it
    !> cycles through the possible corrections of the current word.
    !>
    !> See `get_flyspell_word' for details of how this finds the word to
    !> spell-check.  Port of flyspell.el:flyspell-auto-correct-word
    !> (`flyspell-abbrev-p' is not ported).
    recursive subroutine auto_correct_flyspell_word(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pos, old_max, start, end, length
        type(value_type) :: next_cmd
        character(len=:), allocatable :: word, new_word
        type(ispell_output_type) :: poss
        type(string_type), allocatable :: replacements(:)
        logical :: found, was_on
        integer :: k

        status = status_type()
        pos = point()
        old_max = point_max()

        ! If we are not in the construct where flyspell should be active,
        ! invoke the original binding of M-TAB, if that was recorded.
        if (is_word_rejected()) then
            was_on = minor_modes%flyspell_mode%enabled()
            call minor_modes%flyspell_mode%set_state(.false.)

            if (allocated(this_command_keys)) then
                call key_binding(this_command_keys, next_cmd, status)
            end if

            call minor_modes%flyspell_mode%set_state(was_on)
            if (.not. status%ok()) return

            if (allocated(next_cmd%a)) then
                call execute_interactively(next_cmd, status)
                return
            end if
        end if

        k = state()

        ! Flush a possibly stale cache from previous invocations of
        ! `auto_correct_flyspell_word'/`auto_correct_previous_flyspell_word'.
        if (.not. (associated(last_command, commands%auto_correct_flyspell_word) .or. &
                   associated(last_command, commands%auto_correct_previous_flyspell_word))) then
            states(k)%has_auto_correct_region = .false.
        end if

        ! Use the correct dictionary.
        call accept_flyspell_buffer_local_defs(.false., status)
        if (.not. status%ok()) return

        if (states(k)%auto_correct_pos == pos .and. states(k)%has_auto_correct_region) then
            ! We have already been using the function at the same location.
            start = states(k)%auto_correct_start
            length = states(k)%auto_correct_length
            call unhighlight_flyspell_at(start, status)
            if (status%ok()) call delete_region(start, start + length, status)
            if (.not. status%ok()) return
            states(k)%ring_index = modulo(states(k)%ring_index, size(states(k)%auto_correct_ring)) + 1
            word = states(k)%auto_correct_ring(states(k)%ring_index)%text
            states(k)%auto_correct_length = count_characters(word)
            call goto_char(start)
            call flyspell_insert_function(word, status)
            if (.not. status%ok()) return
            call flyspell_word(status)
            if (.not. status%ok()) return
            call display_next_flyspell_corrections(states(k)%auto_correct_ring, states(k)%ring_index)
            call adjust_flyspell_cursor_point(pos, point(), old_max)
            states(k)%auto_correct_pos = point()
            return
        end if

        ! Fetch the word to be checked.
        call get_flyspell_word(.false., word, start, end, found, status)
        if (.not. status%ok()) return

        if (found) then
            states(k)%auto_correct_word = word
            ! Now check spelling of word.
            call ask_speller(word, poss, status, .false.)
            if (.not. status%ok()) return

            select case (poss%kind)
            case (ISPELL_OUTPUT_CORRECT, ISPELL_OUTPUT_ROOT)
                ! Don't correct word.
                continue
            case (ISPELL_OUTPUT_ERROR)
                ! Ispell error.
                call set_error(status, FM_GENERIC_ERROR, "Ispell: error in Ispell process")
                return
            case default
                ! The word is incorrect, we have to propose a replacement.
                allocate (replacements(0))
                if (allocated(poss%misses)) replacements = poss%misses
                call sort_flyspell_corrections(replacements, word)
                states(k)%has_auto_correct_region = .false.

                if (size(replacements) > 0) then
                    new_word = replacements(1)%text

                    if (new_word /= poss%word) then
                        ! Then save the current replacements.
                        states(k)%has_auto_correct_region = .true.
                        states(k)%auto_correct_start = start
                        states(k)%auto_correct_length = count_characters(new_word)
                        states(k)%auto_correct_ring = replacements
                        call append_string(states(k)%auto_correct_ring, poss%word)
                        states(k)%ring_index = 1
                        call unhighlight_flyspell_at(start, status)
                        if (status%ok()) call delete_region(start, end, status)
                        if (status%ok()) call flyspell_insert_function(new_word, status)
                        if (status%ok()) call flyspell_word(status)
                        if (.not. status%ok()) return
                        call display_next_flyspell_corrections(states(k)%auto_correct_ring, 1, new_word)
                        call adjust_flyspell_cursor_point(pos, point(), old_max)
                    end if
                end if
            end select

            states(k)%auto_correct_pos = point()
            call ispell_pdict_save(status, no_query=.true.)
        end if
    end subroutine auto_correct_flyspell_word

    !> Hook to track successive calls to
    !> `auto_correct_previous_flyspell_word'.  Sets
    !> `flyspell_auto_correct_previous_pos' to none.  Port of
    !> flyspell.el:flyspell-auto-correct-previous-hook.
    recursive subroutine flyspell_auto_correct_previous_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call pre_command_hook%remove(flyspell_auto_correct_previous_hook, local=.true.)

        if (.not. associated(this_command, commands%auto_correct_previous_flyspell_word)) then
            flyspell_auto_correct_previous_pos = NO_POSITION
        end if
    end subroutine flyspell_auto_correct_previous_hook

    !> Auto correct the first misspelled word that occurs before point
    !> (POSITION).  But don't look beyond what's visible on the screen.
    !> Port of flyspell.el:flyspell-auto-correct-previous-word.
    recursive subroutine auto_correct_previous_flyspell_word(position, status)
        use spellcheck_mod, only: ispell_following_word
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        integer(int64) :: top, bot, saved, begv, zv
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
        begv = point_min()
        zv = point_max()
        call narrow_to_region(top, bot, status)
        if (.not. status%ok()) return
        call overlay_recenter(point())
        call pre_command_hook%append(flyspell_auto_correct_previous_hook, local=.true.)

        if (flyspell_auto_correct_previous_pos == NO_POSITION) then
            ! Only reset if a new overlay exists.
            call overlays_in(point_min(), position, overlay_list)
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
                if (.not. is_flyspell_overlay(overlay_list(order(k)))) cycle
                ! Check if its face has changed.
                if (.not. has_incorrect_face(overlay_start(overlay_list(order(k))))) cycle
                ! The length of the word may change so go to the start.
                flyspell_auto_correct_previous_pos = overlay_start(overlay_list(order(k)))
                got = .true.
                exit
            end do
        end if

        if (flyspell_auto_correct_previous_pos /= NO_POSITION) then
            call goto_char(flyspell_auto_correct_previous_pos)
            ! Point is at start.
            saved_following = ispell_following_word
            ispell_following_word = .true.
            call goto_char(flyspell_auto_correct_previous_pos)
            call auto_correct_flyspell_word(status)
            ispell_following_word = saved_following
            ! The point may have moved so reset this.
            flyspell_auto_correct_previous_pos = point()
        end if

        call widen()
        call narrow_to_region(begv, zv, st)
        call goto_char(saved)

    contains

        ! Whether the face at POS is `flyspell_incorrect'.
        logical function has_incorrect_face(pos)
            use text_properties_mod, only: get_char_property
            integer(int64), intent(in) :: pos
            type(value_type) :: face

            face = get_char_property(pos, value_type(name=P_FACE))
            has_incorrect_face = .false.
            if (vtag(face) == V_NAME) has_incorrect_face = symbols(xsymbol(face))%name == trim(flyspell_incorrect%name)
        end function has_incorrect_face
    end subroutine auto_correct_previous_flyspell_word

    ! The command `auto_correct_previous_flyspell_word'.
    recursive subroutine cmd_auto_correct_previous_flyspell_word(status)
        type(status_type), intent(out) :: status

        call auto_correct_previous_flyspell_word(point(), status)
    end subroutine cmd_auto_correct_previous_flyspell_word

    ! Move point after a correction: back to SAVE, shifted by the change in
    ! size if SAVE was at or after CURSOR_LOCATION.  Port of
    ! flyspell.el:flyspell-adjust-cursor-point.
    subroutine adjust_flyspell_cursor_point(save, cursor_location, old_max)
        integer(int64), intent(in) :: save, cursor_location, old_max
        integer(int64) :: new_pos

        if (save >= cursor_location) then
            new_pos = save + (point_max() - old_max)
            call goto_char(max(point_min(), min(new_pos, point_max())))
        else
            call goto_char(save)
        end if
    end subroutine adjust_flyspell_cursor_point

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define Flyspell mode, its keymap, faces, commands and the defaults
    !> of the options, and put `flyspell_kill_ispell_hook' on
    !> `ispell_kill_ispell_hook'.  Not a port.
    subroutine init_flyspell()
        type(status_type) :: st
        type(named_face_type), pointer :: f

        commands%flyspell_mode%execute => flyspell_mode
        commands%flyspell_prog_mode%execute => flyspell_prog_mode
        commands%flyspell_word%execute => cmd_flyspell_word
        commands%flyspell_region%execute => cmd_flyspell_region
        commands%flyspell_buffer%execute => flyspell_buffer
        commands%goto_next_flyspell_error%execute => cmd_goto_next_flyspell_error
        commands%auto_correct_flyspell_word%execute => auto_correct_flyspell_word
        commands%auto_correct_previous_flyspell_word%execute => cmd_auto_correct_previous_flyspell_word
        commands%check_previous_highlighted_flyspell_word%execute => cmd_check_previous_highlighted_flyspell_word
        commands%set_flyspell_delayed_command%execute => cmd_set_flyspell_delayed_command
        commands%set_flyspell_deplacement_command%execute => cmd_set_flyspell_deplacement_command
        commands%flyspell_pre_command_hook%execute => flyspell_pre_command_hook
        commands%flyspell_post_command_hook%execute => flyspell_post_command_hook
        commands%flyspell_auto_correct_previous_hook%execute => flyspell_auto_correct_previous_hook

        if (.not. allocated(flyspell_mark_duplications_exceptions)) then
            ! Common defaults for English.
            flyspell_mark_duplications_exceptions = [flyspell_duplications_exception_type(language="", word="that"), &
                                                     flyspell_duplications_exception_type(language="", word="had"), &
                                                     flyspell_duplications_exception_type(language="\`francais", word="nous"), &
                                                     flyspell_duplications_exception_type(language="\`francais", word="vous")]
        end if

        if (.not. allocated(flyspell_default_delayed_commands)) then
            flyspell_default_delayed_commands = [character(len=64) :: "self_insert_command", "delete_backward_char", &
                                                 "backward_or_forward_delete_char", "delete_char", "scrollbar_vertical_drag", &
                                                 "backward_delete_char_untabify"]
        end if

        if (.not. allocated(flyspell_delayed_commands)) allocate (flyspell_delayed_commands(0))

        if (.not. allocated(flyspell_default_deplacement_commands)) then
            flyspell_default_deplacement_commands = [character(len=64) :: "next_line", "previous_line", "handle_switch_frame", &
                                                     "handle_select_window", "scroll_up", "scroll_down"]
        end if

        if (.not. allocated(flyspell_deplacement_commands)) allocate (flyspell_deplacement_commands(0))
        if (.not. allocated(flyspell_default_dictionary)) flyspell_default_dictionary = ""

        if (.not. allocated(flyspell_tex_command_regexp)) then
            flyspell_tex_command_regexp = "\(\(begin\|end\)[ "//TAB//"]*{\|\(cite[a-z*]*\|label\|ref\|eqref\|usepackage\|"// &
                                          "documentclass\)[ "//TAB//"]*\(\[[^]]*\]\)?{[^{}]*\)"
        end if

        if (.not. allocated(flyspell_dictionaries_that_consider_dash_as_word_delimiter)) then
            flyspell_dictionaries_that_consider_dash_as_word_delimiter = [character(len=32) :: "francais", "deutsch8", "norsk"]
        end if

        if (.not. allocated(flyspell_mode_line_string)) flyspell_mode_line_string = " Fly"
        if (.not. allocated(flyspell_before_incorrect_word_string)) flyspell_before_incorrect_word_string = ""
        if (.not. allocated(flyspell_after_incorrect_word_string)) flyspell_after_incorrect_word_string = ""

        if (.not. allocated(flyspell_prog_text_faces)) then
            flyspell_prog_text_faces = [flyspell_face_ref_type(font_lock_string_face), &
                                        flyspell_face_ref_type(font_lock_comment_face), &
                                        flyspell_face_ref_type(font_lock_doc_face)]
        end if

        f => flyspell_incorrect
        call f%define("flyspell_incorrect", "Flyspell face for misspelled words.")
        call f%spec_entry(display_when(), face_attrs(underline_color="Red1", underline_style=FACE_STYLE_WAVE), &
                          supports=face_attrs(underline_style=FACE_STYLE_WAVE))
        call f%spec_entry(display_when(), face_attrs(underline=.true., inherit="error"))
        f => flyspell_duplicate
        call f%define("flyspell_duplicate", "Flyspell face for words that appear twice in a row."//LF// &
                      "See also `flyspell_duplicate_distance'.")
        call f%spec_entry(display_when(), face_attrs(underline_color="DarkOrange", underline_style=FACE_STYLE_WAVE), &
                          supports=face_attrs(underline_style=FACE_STYLE_WAVE))
        call f%spec_entry(display_when(), face_attrs(underline=.true., inherit="warning"))
        call minor_modes%flyspell_mode%define("flyspell_mode", hook=flyspell_mode_hook, keymap=.true., body=flyspell_mode_body)
        call add_minor_mode_lighter("flyspell_mode", "", text=flyspell_lighter)
        flyspell_mode_map = minor_modes%flyspell_mode%map
        call register_keymap_name("flyspell_mode_map", flyspell_mode_map)
        if (flyspell_use_meta_tab) call keymap_set(flyspell_mode_map, "M-TAB", AUTO_CORRECT_WORD_NAME, st)
        call keymap_set(flyspell_mode_map, trim(flyspell_auto_correct_binding), AUTO_CORRECT_PREVIOUS_WORD_NAME, st)
        call keymap_set(flyspell_mode_map, "C-,", "goto_next_flyspell_error", st)
        call keymap_set(flyspell_mode_map, "C-.", AUTO_CORRECT_WORD_NAME, st)
        ! The `flyspell_mode_test_function' of the modes (upstream's
        ! flyspell-mode-predicate properties: Mail and SGML modes; message,
        ! TeX, Texinfo and nXML modes are not ported).
        major_modes%mail_mode%flyspell_mode_test_function => is_flyspell_mail_text
        major_modes%sgml_mode%flyspell_mode_test_function => is_flyspell_sgml_text
        major_modes%html_mode%flyspell_mode_test_function => is_flyspell_sgml_text
        ! Make sure we flush our caches when needed.  Do it here rather than
        ! in `enable_flyspell_mode', since `flyspell_region' may be used
        ! without ever turning on flyspell-mode.
        call ispell_kill_ispell_hook%prepend(flyspell_kill_ispell_hook)
    end subroutine init_flyspell
end module spellcheck_mode_mod
