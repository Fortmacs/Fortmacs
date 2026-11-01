! minibuffer_fm.f90 -- minibuffer commands and reading file names.
! Ported from GNU Emacs 31.1 lisp/minibuffer.el.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> Leaving the minibuffer, its basic keys, `read_file_name', and
!> completion: `completing_read', the completion styles (in the
!> submodule minibuffer_styles_fm), TAB, SPC, ? and RET in the completion
!> maps, `minibuffer_message', the *Completions* list, and completion in
!> buffers: `completion_at_point' with `completion_at_point_functions',
!> `completion_in_region' and `completion_in_region_mode', with the
!> `:exit_function' of the completion data; tables with quoting
!> (`completion_table_with_quoting', in minibuf_fm).
!>
!> Not ported yet: cycling, completion metadata other than the category
!> and identity sorting (groups, other sorting functions, the rest of
!> `completion_extra_properties' but its `:annotation_function'), moving through and
!> choosing from *Completions* (M-<up>, M-<down>, M-RET, mouse),
!> `completion_show_help', and completion of environment variables and
!> ~USER in file names.
module minibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use callint_fm, only: command_type
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use buffer_fm, only: buffers, cur, get_buffer, get_buffer_create, set_buffer, current_buffer, erase_buffer, &
                         set_buffer_read_only, buffer_list, bury_buffer_internal, deactivate_mark_variable
    use insdel_fm, only: del_range, insert
    use keymap_fm, only: define_key, key_parse, make_sparse_keymap, set_keymap_parent, is_keymap_live, &
                         free_keymap
    use lisp_fm, only: value_type, list, list_elements, list_length, list_elt, xfixnum, xstring, make_list, vtag, &
                       V_NULL, V_INTEGER, V_NAME, V_STRING
    use data_fm, only: value_eq, symbol_value, set
    use character_fm, only: string_char_and_length
    use editfns_fm, only: point, point_max, goto_char, buffer_substring
    use faces_fm, only: named_face_type, face_attrs, display_when
    use textprop_fm, only: add_face_text_property
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, windows, set_window_start, is_window_live, same_window, get_buffer_window, &
                         temp_buffer_window_show, fit_window_to_buffer, display_action_table_type, delete_window, &
                         switch_to_prev_buffer, scroll_window, is_pos_visible_in_window, minibuffer_scroll_window, &
                         display_buffer_mark_dedicated, DEDICATED_NONE, DEDICATED_WEAK, BK_BURY, &
                         DISPLAY_ACTION_MAYBE_SAME_WINDOW, DISPLAY_ACTION_REUSE_WINDOW, DISPLAY_ACTION_AT_BOTTOM, &
                         recenter_top_bottom, scroll_up_command, &
                         scroll_down_command, set_window_buffer
    use callint_fm, only: current_prefix_arg
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use fns_fm, only: compare_strings, COMPARE_EQUAL, schars, substring, is_string_less, read_yes_or_no
    use dired_fm, only: file_name_completion, file_name_all_completions
    use keyboard_fm, only: throw_exit, command_loop_level, this_command, last_command, minibuffer_message_text, &
                           minibuffer_message_timeout, last_message
    use timer_fm, only: run_at_time, free_timer, is_timer
    use minibuf_fm, only: is_minibuffer, minibuf_level, minibuffer_prompt_end, abort_minibuffers, &
                          is_minibuffer_innermost_command_loop, is_innermost_minibuffer, &
                          minibuffer_local_map, read_from_minibuffer, completion_table_type, &
                          completion_predicate, completion_function_table, try_completion, all_completions, &
                          test_completion, completion_ignore_case, TABLE_FUNCTION, COMPLETE_TRY, COMPLETE_ALL, &
                          COMPLETE_TEST, COMPLETE_BOUNDARIES, minibuffer_calling_buffer, completion_list_table, &
                          minibuffer_setup_function, minibuffer_setup_proc, history_added, minibuffer_default, &
                          minibuffer_string_type, quoting_table_call
    use fileio_fm, only: is_file_name_absolute, expand_file_name, substitute_in_file_name, file_name_directory, &
                         file_name_nondirectory, file_exists, file_name_as_directory, is_file_directory
    use subr_fm, only: add_to_history, history_delete_duplicates
    use files_fm, only: abbreviate_file_name
    use minibuf_table_fm, only: capf_data_type, COMPLETION_EXACT, COMPLETION_FINISHED, COMPLETION_UNKNOWN, &
                                completion_exit_function_proc, TABLE_QUOTING
    use eval_fm, only: capf_hook_type, capf_function, capf_entry_type
    use marker_fm, only: copy_marker_at, copy_marker, free_marker, marker_buffer, marker_position
    use keymap_fm, only: push_minor_mode_overriding_map, remove_minor_mode_overriding_map
    use keyboard_fm, only: post_command_hook, has_unread_command_events, this_command_keys
    use window_fm, only: same_handle
    use editfns_fm, only: line_end_position
    use window_fm, only: set_window_point
    use modes_fm, only: minor_modes
    implicit none(type, external)
    private

    public :: exit_minibuffer, delete_minibuffer_contents, read_file_name, syms_of_minibuffer, read_buffer, &
              read_directory_name, completion_file_name_table
    public :: completion_insert_strings
    public :: completion_in_region, completion_in_region_default, completion_at_point, completion_help_at_point, &
              completion_at_point_run, &
              cir_predicate_proc, completion_in_region_proc
    public :: internal_complete_buffer, minibuffer_maybe_quote_filename, completions_visible
    public :: completing_read, completion_try_completion, completion_all_completions, completion_boundaries, &
              completion_table_with_context, completion_complete_and_exit, completion_do_completion, &
              completion_prompt_end, completion_exit_proc, &
              minibuffer_complete, minibuffer_complete_word, minibuffer_complete_and_exit, &
              minibuffer_completion_help, minibuffer_hide_completions, minibuffer_message, &
              read_file_name_internal

    !> Values of REQUIRE_MATCH for `completing_read', also those of
    !> `minibuffer_completion_confirm': REQUIRE_MATCH_NO, REQUIRE_MATCH_YES,
    !> REQUIRE_MATCH_CONFIRM, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION and REQUIRE_MATCH_OTHER.
    integer, parameter, public :: REQUIRE_MATCH_NO = 0, REQUIRE_MATCH_YES = 1, REQUIRE_MATCH_CONFIRM = 2, &
                                  REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION = 3, REQUIRE_MATCH_OTHER = 4

    !> Kinds of `completion_try_completion' result: none (TRY_NONE), exact (TRY_EXACT), and a STRING with a
    !> POINT (TRY_STRING).
    integer, parameter, public :: TRY_NONE = 0, TRY_EXACT = 1, TRY_STRING = 2

    !> A `completion_try_completion' result.
    type, public :: try_result_type
        integer :: kind = TRY_NONE
        character(len=:), allocatable :: string
        integer(int64) :: point = 0
    end type try_result_type

    !> A string in an array.
    type :: string_box
        character(len=:), allocatable :: s
    end type string_box

    !> The completion table of the active minibuffer (minibuffer.el
    !> minibuffer-completion-table).
    type(completion_table_type), public :: minibuffer_completion_table
    !> The predicate of `minibuffer_completion_table' (null for none).
    procedure(completion_predicate), pointer, public :: minibuffer_completion_predicate => null()
    !> minibuffer.el minibuffer-completion-confirm (a REQUIRE_MATCH_ value).
    integer, public :: minibuffer_completion_confirm = REQUIRE_MATCH_NO

    !> Whether the minibuffer is completing a file name.  Port of
    !> minibuffer.el minibuffer-completing-file-name.
    logical, public :: minibuffer_completing_file_name = .false.

    !> True means automatically provide help for invalid completion
    !> input.  (Only true and false: `lazy', `always' and `visible' are not
    !> ported.)  Port of minibuffer.el completion-auto-help.
    logical, public :: completion_auto_help = .true.

    !> If true, print helpful inline messages during completion.
    !> Port of minibuffer.el completion-show-inline-help.
    logical, public :: completion_show_inline_help = .true.

    !> If true, stay quiet when there is no match.  Port of
    !> minibuffer.el completion-fail-discreetly.
    logical, public :: completion_fail_discreetly = .false.

    !> Local keymap for minibuffer input with completion; the one for an
    !> exact match; and both with SPC unbound for file names.  Ports of
    !> minibuffer.el minibuffer-local-completion-map,
    !> minibuffer-local-must-match-map and (composed with each)
    !> minibuffer-local-filename-completion-map.
    type(handle_type), public :: minibuffer_local_completion_map, minibuffer_local_must_match_map, &
                                 minibuffer_local_filename_completion_map, &
                                 minibuffer_local_filename_must_match_map

    !> The last text `minibuffer_message' showed (not cleared by the next
    !> command, unlike the display), for programs and tests.
    character(len=:), allocatable, public :: last_minibuffer_message

    !> The timer that removes the text `minibuffer_message' shows.
    !> Port of minibuffer.el minibuffer--message-timer.
    type(handle_type) :: minibuffer_message_timer

    !> Maximum height for *Completions* buffer window (0 for no maximum).
    !> Port of minibuffer.el completions-max-height.
    integer, public :: completions_max_height = 0

    !> True means when reading a filename start with default dir in minibuffer.
    !>
    !> When the initial minibuffer contents show a name of a file or a directory,
    !> typing RETURN without editing the initial contents is equivalent to typing
    !> the default file name.
    !>
    !> If this variable is true, the minibuffer contents are always
    !> initially non-empty, and typing RETURN without editing will fetch the
    !> default name, if one is provided.
    !>
    !> If this variable is false, the minibuffer often starts out empty.
    !> Port of minibuffer.el insert-default-directory.
    logical, public :: insert_default_directory = .true.

    !> True means when reading a file name completion ignores case.
    !> Port of minibuffer.el read-file-name-completion-ignore-case (.false. on
    !> GNU/Linux).
    logical, public :: read_file_name_completion_ignore_case = .false.

    !> What `read_file_name_setup' sets up: the minibuffer's directory and
    !> the initial input (none unless `rfn_have_insdef').
    character(len=:), allocatable :: rfn_dir, rfn_insdef
    logical :: rfn_have_insdef = .false.

    !> True means completion ignores case when reading a buffer name.
    !> Port of minibuf.c read-buffer-completion-ignore-case.
    logical, public :: read_buffer_completion_ignore_case = .false.

    !> The buffer name internal-complete-buffer-except leaves out
    !> (unallocated for none) and the predicate it combines with its own.
    character(len=:), allocatable :: rbts_except
    procedure(completion_predicate), pointer :: rbts_pred2 => null()

    !> completing-read-default's minibuffer setup: the ambient
    !> `completion_ignore_case', and the setup function of an enclosing
    !> minibuffer-with-setup-hook, run after it.
    logical :: crd_ignore_case = .false.
    procedure(minibuffer_setup_proc), pointer :: crd_outer_setup => null()

    !> A completion category's settings: the styles to use for it.
    type, public :: completion_category_type
        character(len=24) :: category = ""
        character(len=24), allocatable :: styles(:)
    end type completion_category_type

    !> List of completion styles to use.
    !> The available styles are basic, partial-completion, emacs22,
    !> emacs21, initials, substring and flex (shorthand never matches, as
    !> there are no symbol shorthands).
    !>
    !> Note that `completion_category_overrides' may override these
    !> styles for specific categories, such as files, buffers, etc.
    !> Port of minibuffer.el completion-styles.
    character(len=24), allocatable, public :: completion_styles(:)

    !> Default settings for specific completion categories.
    !> Each entry gives the styles to use for that category.  Port of
    !> minibuffer.el completion-category-defaults.
    type(completion_category_type), allocatable, public :: completion_category_defaults(:)

    !> List of category-specific user overrides for completion styles.
    !> Each entry gives the styles to use for that category; these take
    !> precedence over `completion_category_defaults'.  Port of
    !> minibuffer.el completion-category-overrides.
    type(completion_category_type), allocatable, public :: completion_category_overrides(:)

    !> A string of characters treated as word delimiters for completion.
    !> Some arcane rules:
    !> If `]' is in this string, it must come first.
    !> If `^' is in this string, it must not come first.
    !> If `-' is in this string, it must come first or right after `]'.
    !> In other words, if S is this string, then `[S]' must be a valid Emacs
    !> regular expression (not containing character ranges like `a-z').
    !> Port of minibuffer.el completion-pcm-word-delimiters.
    character(len=:), allocatable, public :: completion_pcm_word_delimiters

    !> If true, partial-completion behaves as if each word is preceded by
    !> wildcard.  If false, only the first word is not preceded by
    !> wildcard.  Port of minibuffer.el completion-pcm-leading-wildcard.
    logical, public :: completion_pcm_leading_wildcard = .false.

    !> True if `flex' completion rejects spaces in search pattern.
    !> Port of minibuffer.el completion-flex-nospace.
    logical, public :: completion_flex_nospace = .false.

    !> Whether the last `completion_all_completions' used the flex style
    !> with a pattern, so *Completions* sorts by `flex_costs'
    !> (completion--flex-adjust-metadata).
    logical :: completion_flex_sorting = .false.

    !> A completion and its flex score (lower is better).
    type :: flex_cost_type
        character(len=:), allocatable :: s
        integer :: cost = 0
    end type flex_cost_type

    !> The flex pattern and the costs of the last flex completions.
    character(len=:), allocatable :: flex_pattern_str
    type(flex_cost_type), allocatable :: flex_costs(:)

    abstract interface
        !> The EXIT_FUNCTION of `completion_complete_and_exit'.  Not a port.
        recursive subroutine completion_exit_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine completion_exit_proc

        !> A completion-in-region-mode-predicate: whether
        !> `completion_in_region_mode' should stay on.  Not a port.
        recursive logical function cir_predicate_proc()
            implicit none(type, external)
        end function cir_predicate_proc

        !> A `completion_in_region_function'.  Not a port.
        recursive logical function completion_in_region_proc(start, end, collection, predicate)
            import :: int64, completion_table_type, completion_predicate
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(completion_table_type), intent(in) :: collection
            procedure(completion_predicate), optional :: predicate
        end function completion_in_region_proc

        !> An `:annotation_function' of completion-extra-properties: the
        !> annotation shown after CANDIDATE in *Completions* ("" for none).
        !> Not a port.
        subroutine completion_annotation_proc(candidate, annotation)
            implicit none(type, external)
            character(len=*), intent(in) :: candidate
            character(len=:), allocatable, intent(out) :: annotation
        end subroutine completion_annotation_proc
    end interface

    !> Special hook to find the completion table for the entity at point.
    !> Each function on this hook is called in turn and should return
    !> either nothing, meaning it is not applicable at point, or the
    !> completion data (START END COLLECTION . PROPS), where:
    !>  START and END delimit the entity to complete and should include point,
    !>  COLLECTION is the completion table to use to complete the entity, and
    !>  PROPS holds attributes for additional information.
    !> Currently supported properties are:
    !>  `predicate'        a predicate that completion candidates need to satisfy.
    !>  `exclusive'        false means that if the completion table fails to
    !>    match the text at point, then instead of reporting a completion
    !>    failure, the completion should try the next completion function.
    !> As is the case with most hooks, the functions are responsible for
    !> preserving things like point and current buffer.
    !>
    !> NOTE: These functions should be cheap to run since they're sometimes
    !> run from `post_command_hook'; and they should ideally only choose
    !> which kind of completion table to use, and not pre-filter it based
    !> on the current text between START and END (e.g., they should not
    !> obey `completion_styles').  Port of minibuffer.el
    !> completion-at-point-functions (its default,
    !> tags-completion-at-point-function, is deferred with etags; it finds
    !> nothing without a tags table).
    type(capf_hook_type), target, public :: completion_at_point_functions = &
                                            capf_hook_type(name="completion_at_point_functions")

    !> Function to perform the job of `completion_in_region'.
    !> The function is called with 4 arguments: START END COLLECTION
    !> PREDICATE.  The arguments and expected return value are as
    !> specified for `completion_in_region'; null for the default,
    !> `completion_in_region_default'.  Port of minibuffer.el
    !> completion-in-region-function.
    procedure(completion_in_region_proc), pointer, public :: completion_in_region_function => null()

    !> Predicate to tell `completion_in_region_mode' when to exit.
    !> It is called with no argument and should return false when
    !> `completion_in_region_mode' should exit (and hence pop down
    !> the *Completions* buffer).  Port of minibuffer.el
    !> completion-in-region-mode-predicate.
    procedure(cir_predicate_proc), pointer, public :: completion_in_region_mode_predicate => null()

    !> Copy of the value of `completion_in_region_mode_predicate'.
    !> This holds the value `completion_in_region_mode_predicate' had when
    !> we entered `completion_in_region_mode'.  Port of minibuffer.el
    !> completion-in-region-mode--predicate.
    procedure(cir_predicate_proc), pointer :: cir_mode_predicate => null()

    !> The `:exit_function' of completion-extra-properties: function to
    !> run after completion is performed (null for none).  Port of
    !> minibuffer.el completion-extra-properties (its `:exit-function').
    procedure(completion_exit_function_proc), pointer, public :: completion_exit_function => null()

    public :: completion_annotation_proc

    !> The `:annotation_function' of completion-extra-properties: function
    !> giving the annotation *Completions* shows after a candidate (null for
    !> none).  Port of minibuffer.el completion-extra-properties (its
    !> `:annotation-function').
    procedure(completion_annotation_proc), pointer, public :: completion_annotation_function => null()

    !> Face to use for annotations in the *Completions* buffer.
    !> This face is only used if the strings used for completions
    !> doesn't already specify a face.  Port of minibuffer.el
    !> completions-annotations.
    type(named_face_type), target, public :: completions_annotations

    !> completion-in-region--data: the start and end markers, the table
    !> and the predicate (`cir_data' .false. for none).
    logical :: cir_data = .false.
    type(handle_type) :: cir_start, cir_end
    type(completion_table_type) :: cir_table
    procedure(completion_predicate), pointer :: cir_pred => null()

    !> The closure of completion-at-point's predicate `capf_same_field':
    !> the hook function and the start marker, while completing; and the
    !> copy kept while `completion_in_region_mode' is on.
    procedure(capf_function), pointer :: capf_field_fun => null(), cir_mode_capf_fun => null()
    type(handle_type) :: capf_field_start, cir_mode_capf_start

    !> Keymap activated during `completion_in_region'.  Port of
    !> minibuffer.el completion-in-region-mode-map (RET, M-<up>, M-<down>
    !> and M-RET, which choose from *Completions*, are deferred).
    type(handle_type), public :: completion_in_region_mode_map

    interface
        !> Try to complete STRING using completion table TABLE.
        !> Only the elements of table that satisfy predicate PRED are
        !> considered.  POINT is the position of point within STRING.  The
        !> result is TRY_NONE to indicate that there is no completion, TRY_EXACT to
        !> indicate that STRING is the only possible completion, or TRY_STRING with
        !> NEWSTRING and NEWPOINT, the completed result string together
        !> with a new position for point.  Port of
        !> minibuffer.el:completion-try-completion.
        module subroutine completion_try_completion(string, table, point, res, pred)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            type(completion_table_type), intent(in) :: table
            integer(int64), intent(in) :: point
            type(try_result_type), intent(out) :: res
            procedure(completion_predicate), optional :: pred
        end subroutine completion_try_completion

        !> List the possible completions of STRING in completion table
        !> TABLE.  Only the elements of table that satisfy predicate PRED
        !> are considered.  POINT is the position of point within STRING.
        !> The completions go to `all' and the base-size to `base_size'.
        !> Port of minibuffer.el:completion-all-completions.
        module subroutine completion_all_completions(string, table, point, all, base_size, pred)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            type(completion_table_type), intent(in) :: table
            integer(int64), intent(in) :: point
            type(value_type), intent(out) :: all
            integer(int64), intent(out) :: base_size
            procedure(completion_predicate), optional :: pred
        end subroutine completion_all_completions

        !> Set the completion style settings not set yet to their
        !> defaults.  Not a port.
        module subroutine completion_settings_defaults()
            implicit none(type, external)
        end subroutine completion_settings_defaults
    end interface

    ! The prefix and predicate of `completion_table_with_context'.
    character(len=:), allocatable :: context_prefix
    procedure(completion_predicate), pointer :: context_pred => null()

contains

    !> Terminate this minibuffer argument.  Port of
    !> minibuffer.el:exit-minibuffer.
    subroutine exit_minibuffer(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (is_minibuffer()) then
            if (.not. is_minibuffer_innermost_command_loop()) then
                call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
                return
            end if

            if (.not. is_innermost_minibuffer()) then
                call set_error(status, FM_GENERIC_ERROR, "Not in most nested minibuffer")
                return
            end if
        end if

        ! If the command that uses this has made modifications in the
        ! minibuffer, we don't want them to cause deactivation of the mark
        ! in the original buffer.
        deactivate_mark_variable = .false.
        call throw_exit(.false.)
    end subroutine exit_minibuffer

    !> Delete the text after the prompt.  Port of
    !> minibuffer.el:delete-minibuffer-contents.
    subroutine delete_minibuffer_contents(status)
        type(status_type), intent(out) :: status

        call del_range(minibuffer_prompt_end(), buffers(cur)%zv, status)
    end subroutine delete_minibuffer_contents

    !> Read a file name, prompting with PROMPT and completing in directory DIR.
    !> Return the file name as a string.
    !> The return value is not expanded---you must call `expand_file_name'
    !> yourself.
    !>
    !> DIR is the directory to use for completing relative file names.
    !> It should be an absolute directory name, or absent (which means the
    !> current buffer's value of `default_directory').
    !>
    !> DEFAULT_FILENAME specifies the default file name to return if the
    !> user exits the minibuffer with the same non-empty string inserted
    !> by this function.  If DEFAULT_FILENAME is absent, then if INITIAL
    !> is present, the default is DIR combined with INITIAL; otherwise, if
    !> the current buffer is visiting a file, that file serves as the
    !> default; otherwise, the default is simply the string inserted into
    !> the minibuffer.
    !>
    !> If the user exits with an empty minibuffer, return an empty
    !> string.  (This happens only if the user erases the pre-inserted
    !> contents, or if `insert_default_directory' is false.)
    !>
    !> Fourth arg MUSTMATCH can take the following values:
    !> - REQUIRE_MATCH_NO means that the user can exit with any input.
    !> - REQUIRE_MATCH_YES means that the user is not allowed to exit unless
    !>   the input is (or completes to) an existing file.
    !> - REQUIRE_MATCH_CONFIRM means that the user can exit with any input,
    !>   but she needs to confirm her choice if the input is not an
    !>   existing file.
    !> - REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION means that the user can
    !>   exit with any input, but she needs to confirm her choice if she
    !>   called `minibuffer_complete' right before
    !>   `minibuffer_complete_and_exit' and the input is not an existing
    !>   file.
    !> - REQUIRE_MATCH_OTHER behaves like REQUIRE_MATCH_YES except that
    !>   typing RET does not exit if it does non-null completion.
    !>
    !> Fifth arg INITIAL specifies text to start with.  It will be
    !> interpreted as the trailing part of DEFAULT_FILENAME, so using a
    !> full file name for INITIAL will usually lead to surprising
    !> results.
    !>
    !> Sixth arg PREDICATE, if present, restricts the completions to the
    !> file names it accepts.
    !>
    !> See also `read_file_name_completion_ignore_case'.  Port of
    !> minibuffer.el:read-file-name-default (function MUSTMATCH, list DEFAULT_FILENAME, file dialogs,
    !> minibuffer-default-add-function, minibuffer-local-filename-syntax or
    !> file-name-quote for names containing "/~": Deferred).
    recursive subroutine read_file_name(prompt, result, status, dir, default_filename, mustmatch, initial, predicate)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        integer, intent(in), optional :: mustmatch
        !> Restricts the completions to the file names it accepts.
        procedure(completion_predicate), optional :: predicate
        character(len=:), allocatable :: d, default, insdef, val, val1, expanded
        type(value_type) :: history
        type(value_type), allocatable :: elems(:), kept(:)
        type(status_type) :: st
        logical :: have_default, have_insdef, saved_completing, used_default, replace_in_history, add_history
        integer :: match, k, n

        if (present(dir)) then
            d = dir
        else if (allocated(buffers(cur)%directory)) then
            d = buffers(cur)%directory
        else
            d = "~/"
        end if

        if (.not. is_file_name_absolute(d)) then
            call expand_file_name(d, val, status)
            if (.not. status%ok()) return
            d = val
        end if

        have_default = .true.

        if (present(default_filename)) then
            default = default_filename
        else if (.not. present(initial)) then
            have_default = allocated(buffers(cur)%filename)
            if (have_default) default = buffers(cur)%filename
        else if (len(initial) == 0) then
            ! Special-case "" because (expand-file-name "" "/tmp/")
            ! returns "/tmp" rather than "/tmp/" (bug#39057).
            default = d
        else
            call expand_file_name(initial, default, status, d)
            if (.not. status%ok()) return
        end if

        ! If dir starts with user's homedir, change that to ~.
        d = abbreviate_file_name(d)
        ! Likewise for default-filename.
        if (have_default) default = abbreviate_file_name(default)

        if (insert_default_directory) then
            if (present(initial)) then
                insdef = minibuffer_maybe_quote_filename(d//initial)
            else
                insdef = minibuffer_maybe_quote_filename(d)
            end if
        else if (present(initial)) then
            insdef = minibuffer_maybe_quote_filename(initial)
        else
            insdef = ""
        end if

        have_insdef = insert_default_directory .or. present(initial)
        match = REQUIRE_MATCH_NO
        if (present(mustmatch)) match = mustmatch
        ! The minibuffer's default-directory: DIR as a directory name.
        call expand_file_name(d, expanded, status)
        if (.not. status%ok()) return
        rfn_dir = file_name_as_directory(expanded)
        rfn_insdef = insdef
        rfn_have_insdef = have_insdef
        minibuffer_setup_function => read_file_name_setup
        saved_completing = minibuffer_completing_file_name
        minibuffer_completing_file_name = .true.

        if (have_insdef .and. have_default) then
            call completing_read(prompt, completion_function_table(read_file_name_internal, "file"), val, status, &
                                 predicate=predicate, require_match=match, initial=insdef, history="file_name_history", &
                                 default=default, used_default=used_default)
        else if (have_insdef) then
            call completing_read(prompt, completion_function_table(read_file_name_internal, "file"), val, status, &
                                 predicate=predicate, require_match=match, initial=insdef, history="file_name_history", &
                                 used_default=used_default)
        else if (have_default) then
            call completing_read(prompt, completion_function_table(read_file_name_internal, "file"), val, status, &
                                 predicate=predicate, require_match=match, history="file_name_history", default=default, &
                                 used_default=used_default)
        else
            call completing_read(prompt, completion_function_table(read_file_name_internal, "file"), val, status, &
                                 predicate=predicate, require_match=match, history="file_name_history", used_default=used_default)
        end if

        minibuffer_completing_file_name = saved_completing
        minibuffer_setup_function => null()
        if (.not. status%ok()) return
        ! What completing-read returned is the history's first element
        ! (`eq') if read_from_minibuffer added it.
        replace_in_history = history_added
        add_history = .false.

        ! If completing-read returned the inserted default string itself
        ! (rather than a new string with the same contents), it has to
        ! mean that the user typed RET with the minibuffer empty.  In that
        ! case, we really want to return "" so that commands such as
        ! set-visited-file-name can distinguish.
        if (used_default) then
            ! In this case, completing-read has not added an element to
            ! the history.  Maybe we should.
            if (.not. replace_in_history) add_history = .true.
            val = ""
        end if

        if (have_default .and. have_insdef) then
            if (val == insdef .and. len(val) == len(insdef)) val = default
        end if

        val = substitute_in_file_name(val)

        if (replace_in_history) then
            ! Replace what completing_read added to the history with what
            ! we will actually return.  As an exception, if that's the
            ! same as the second item in file-name-history, it's really a
            ! repeat (Bug#4657).
            val1 = minibuffer_maybe_quote_filename(val)
            call symbol_value(value_type(name="file_name_history"), history, st)

            if (st%ok()) then
                call list_elements(history, elems)

                if (size(elems) > 0) then
                    allocate (kept(size(elems)))
                    kept(1) = elems(1)
                    n = 1

                    do k = 2, size(elems)
                        if (history_delete_duplicates .and. string_equal(elems(k), val1)) cycle
                        n = n + 1
                        kept(n) = elems(k)
                    end do

                    if (n >= 2) then
                        if (string_equal(kept(2), val1)) then
                            call set(value_type(name="file_name_history"), make_list(kept(2:n)), st)
                        else
                            kept(1) = value_type(text=val1)
                            call set(value_type(name="file_name_history"), make_list(kept(1:n)), st)
                        end if
                    else
                        kept(1) = value_type(text=val1)
                        call set(value_type(name="file_name_history"), make_list(kept(1:n)), st)
                    end if
                end if
            end if
        else if (add_history) then
            call add_to_history("file_name_history", minibuffer_maybe_quote_filename(val))
        end if

        result = val

    contains

        logical function string_equal(v, s)
            type(value_type), intent(in) :: v
            character(len=*), intent(in) :: s

            string_equal = vtag(v) == V_STRING
            if (string_equal) string_equal = xstring(v) == s .and. len(xstring(v)) == len(s)
        end function string_equal
    end subroutine read_file_name

    !> Read directory name, prompting with PROMPT and completing in
    !> directory DIR.  Value is not expanded---you must call
    !> `expand_file_name' yourself.
    !> Default name to DEFAULT_DIRNAME if user exits with the same
    !> non-empty string that was inserted by this function.
    !> (If DEFAULT_DIRNAME is absent, DIR or INITIAL is used.)
    !> If the user exits with an empty minibuffer, this function returns
    !> an empty string.  (This can happen only if the user erased the
    !> pre-inserted contents or if `insert_default_directory' is false.)
    !> Fourth arg MUSTMATCH is as for `read_file_name'.
    !> Fifth arg INITIAL specifies text to start with.
    !> DIR should be an absolute directory name.  It defaults to the value
    !> of `default_directory'.  Port of files.el:read-directory-name (here
    !> because files_fm comes before minibuffer_fm).
    recursive subroutine read_directory_name(prompt, result, status, dir, default_dirname, mustmatch, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_dirname, initial
        integer, intent(in), optional :: mustmatch
        character(len=:), allocatable :: d, default

        if (present(dir)) then
            d = dir
        else if (allocated(buffers(cur)%directory)) then
            d = buffers(cur)%directory
        else
            d = "~/"
        end if

        if (present(default_dirname)) then
            default = default_dirname
        else if (present(initial)) then
            call expand_file_name(initial, default, status, d)
            if (.not. status%ok()) return
        else
            default = d
        end if

        call read_file_name(prompt, result, status, d, default, mustmatch, initial, directory_predicate)
    end subroutine read_directory_name

    !> file-directory-p as a completion predicate.  Not a port.
    logical function directory_predicate(elt)
        type(value_type), intent(in) :: elt

        directory_predicate = .false.
        if (vtag(elt) == V_STRING) directory_predicate = is_file_directory(xstring(elt))
    end function directory_predicate

    !> The minibuffer-with-setup-hook function of `read_file_name': the
    !> minibuffer's default-directory is the directory read in, a first
    !> default equal to the initial input is dropped, and
    !> `completion_ignore_case' is `read_file_name_completion_ignore_case'.
    !> Port of minibuffer.el:read-file-name-default in part.
    subroutine read_file_name_setup()
        type(minibuffer_string_type), allocatable :: rest(:)
        integer :: k

        buffers(cur)%directory = rfn_dir

        ! When the first default in `minibuffer-default' duplicates
        ! initial input `insdef', reset `minibuffer-default' to nil.
        if (allocated(minibuffer_default) .and. rfn_have_insdef) then
            if (size(minibuffer_default) > 0) then
                if (minibuffer_default(1)%s == rfn_insdef .and. len(minibuffer_default(1)%s) == len(rfn_insdef)) then
                    ! (cdr-safe minibuffer-default)
                    allocate (rest(size(minibuffer_default) - 1))

                    do k = 2, size(minibuffer_default)
                        rest(k - 1)%s = minibuffer_default(k)%s
                    end do

                    deallocate (minibuffer_default)
                    if (size(rest) > 0) call move_alloc(rest, minibuffer_default)
                end if
            end if
        end if

        call completion_ignore_case%set_local(read_file_name_completion_ignore_case)
    end subroutine read_file_name_setup

    !> Protect FILENAME from `substitute_in_file_name', as needed.
    !> Useful to give the user default values that won't be substituted.
    !> Port of minibuffer.el:minibuffer-maybe-quote-filename (file-name-quote
    !> for names with "/~" is not ported: they are only $-doubled).
    function minibuffer_maybe_quote_filename(filename) result(quoted)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: quoted

        quoted = double_dollars(filename)
    end function minibuffer_maybe_quote_filename

    !> Return STR with dollar signs doubled.  Port of
    !> minibuffer.el:minibuffer--double-dollars.
    pure function double_dollars(str) result(doubled)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: doubled
        integer :: i

        doubled = ""

        do i = 1, len(str)
            if (str(i:i) == "$") then
                doubled = doubled//"$$"
            else
                doubled = doubled//str(i:i)
            end if
        end do
    end function double_dollars

    !> Terminate this minibuffer argument.
    !> Port of minibuffer.el:exit-minibuffer.
    subroutine cmd_exit_minibuffer(status)
        type(status_type), intent(out) :: status

        call exit_minibuffer(status)
    end subroutine cmd_exit_minibuffer

    !> Abort the current minibuffer.
    !> If we are not currently in the innermost minibuffer, prompt the user to
    !> confirm the aborting of the current minibuffer and all contained ones.
    !> Port of minibuf.c:abort-minibuffers.
    subroutine cmd_abort_minibuffers(status)
        type(status_type), intent(out) :: status

        call abort_minibuffers(status, read_yes_or_no)
    end subroutine cmd_abort_minibuffers

    !> Transient minor mode used during `completion_in_region'.  Port of
    !> minibuffer.el:completion-in-region-mode.
    subroutine cmd_completion_in_region_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%completion_in_region_mode%command(status)
    end subroutine cmd_completion_in_region_mode

    subroutine cmd_minibuffer_complete(status)
        type(status_type), intent(out) :: status

        call minibuffer_complete(status)
    end subroutine cmd_minibuffer_complete

    subroutine cmd_minibuffer_complete_word(status)
        type(status_type), intent(out) :: status

        call minibuffer_complete_word(status)
    end subroutine cmd_minibuffer_complete_word

    subroutine cmd_minibuffer_completion_help(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call minibuffer_completion_help()
    end subroutine cmd_minibuffer_completion_help

    subroutine cmd_minibuffer_complete_and_exit(status)
        type(status_type), intent(out) :: status

        call minibuffer_complete_and_exit(status)
    end subroutine cmd_minibuffer_complete_and_exit

    !> Call `exit_minibuffer' (there is no selected completion to insert
    !> first yet).  Port of minibuffer.el:minibuffer-completion-exit.
    subroutine cmd_minibuffer_completion_exit(status)
        type(status_type), intent(out) :: status

        call exit_minibuffer(status)
    end subroutine cmd_minibuffer_completion_exit

    !> Run `recenter_top_bottom' from the minibuffer in its original window.
    !> Port of minibuffer.el:minibuffer-recenter-top-bottom.
    subroutine cmd_minibuffer_recenter_top_bottom(status)
        type(status_type), intent(out) :: status

        call with_minibuffer_selected_window(1, status)
    end subroutine cmd_minibuffer_recenter_top_bottom

    !> Run `scroll_up_command' from the minibuffer in its original window.
    !> Port of minibuffer.el:minibuffer-scroll-up-command (no shift
    !> selection).
    subroutine cmd_minibuffer_scroll_up_command(status)
        type(status_type), intent(out) :: status

        call with_minibuffer_selected_window(2, status)
    end subroutine cmd_minibuffer_scroll_up_command

    !> Run `scroll_down_command' from the minibuffer in its original window.
    !> Port of minibuffer.el:minibuffer-scroll-down-command (no shift
    !> selection).
    subroutine cmd_minibuffer_scroll_down_command(status)
        type(status_type), intent(out) :: status

        call with_minibuffer_selected_window(3, status)
    end subroutine cmd_minibuffer_scroll_down_command

    !> Run command `which' (1 recenter-top-bottom, 2 scroll-up-command,
    !> 3 scroll-down-command) with the prefix argument in the window that
    !> was selected when the minibuffer was entered.  Port of
    !> minibuffer.el:with-minibuffer-selected-window.
    recursive subroutine with_minibuffer_selected_window(which, status)
        integer, intent(in) :: which
        type(status_type), intent(out) :: status
        type(handle_type) :: mini
        type(window_type), pointer :: sw
        type(status_type) :: st
        logical :: saved

        status = status_type()
        if (.not. selected_frame%minibuffer_selected .or. minibuf_level == 0) return
        mini = current_buffer()
        saved = selected_frame%minibuffer_selected
        selected_frame%minibuffer_selected = .false.
        sw => frame_selected_window_ref()
        call set_buffer(sw%buffer, st)

        select case (which)
        case (1)
            call recenter_top_bottom(current_prefix_arg, status)
        case (2)
            call scroll_up_command(current_prefix_arg, status)
        case default
            call scroll_down_command(current_prefix_arg, status)
        end select

        selected_frame%minibuffer_selected = saved
        call set_buffer(mini, st)
    end subroutine with_minibuffer_selected_window

    !> Register the commands, minibuffer.el's minibuffer-local-map keys, and
    !> the completion keymaps.  Not a port.
    subroutine syms_of_minibuffer()
        type(status_type) :: status
        type(named_face_type), pointer :: f

        commands%exit_minibuffer%run => cmd_exit_minibuffer
        commands%abort_minibuffers%run => cmd_abort_minibuffers
        commands%minibuffer_complete%run => cmd_minibuffer_complete
        commands%minibuffer_complete_word%run => cmd_minibuffer_complete_word
        commands%minibuffer_completion_help%run => cmd_minibuffer_completion_help
        commands%minibuffer_complete_and_exit%run => cmd_minibuffer_complete_and_exit
        commands%minibuffer_completion_exit%run => cmd_minibuffer_completion_exit
        commands%minibuffer_recenter_top_bottom%run => cmd_minibuffer_recenter_top_bottom
        commands%minibuffer_scroll_up_command%run => cmd_minibuffer_scroll_up_command
        commands%minibuffer_scroll_down_command%run => cmd_minibuffer_scroll_down_command
        call completion_settings_defaults()
        f => completions_annotations
        call f%define("completions_annotations", "Face to use for annotations in the *Completions* buffer."//achar(10)// &
                      "This face is only used if the strings used for completions"//achar(10)// &
                      "doesn't already specify a face.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=12) :: "italic", "shadow"]))
        commands%completion_at_point%run => completion_at_point
        commands%completion_help_at_point%run => completion_help_at_point
        commands%completion_in_region_mode%run => cmd_completion_in_region_mode
        call minor_modes%completion_in_region_mode%define("completion_in_region_mode", global=.true., &
                                                          body=completion_in_region_mode_body)
        if (is_keymap_live(completion_in_region_mode_map)) call free_keymap(completion_in_region_mode_map, status)
        call make_sparse_keymap(completion_in_region_mode_map)
        call bind(completion_in_region_mode_map, "M-?", "completion_help_at_point")
        call bind(completion_in_region_mode_map, "TAB", "completion_at_point")
        call bind(minibuffer_local_map, "C-g", "abort_minibuffers")
        call bind(minibuffer_local_map, "C-j", "exit_minibuffer")
        call bind(minibuffer_local_map, "RET", "exit_minibuffer")
        ! Rebuilding: drop the old maps.
        if (is_keymap_live(minibuffer_local_completion_map)) call free_keymap(minibuffer_local_completion_map, status)
        if (is_keymap_live(minibuffer_local_must_match_map)) call free_keymap(minibuffer_local_must_match_map, status)

        if (is_keymap_live(minibuffer_local_filename_completion_map)) then
            call free_keymap(minibuffer_local_filename_completion_map, status)
        end if

        if (is_keymap_live(minibuffer_local_filename_must_match_map)) then
            call free_keymap(minibuffer_local_filename_must_match_map, status)
        end if

        call make_sparse_keymap(minibuffer_local_completion_map)
        call set_keymap_parent(minibuffer_local_completion_map, minibuffer_local_map, status)
        call bind(minibuffer_local_completion_map, "TAB", "minibuffer_complete")
        call bind(minibuffer_local_completion_map, "<backtab>", "minibuffer_complete")
        call bind(minibuffer_local_completion_map, "SPC", "minibuffer_complete_word")
        call bind(minibuffer_local_completion_map, "RET", "minibuffer_completion_exit")
        call bind(minibuffer_local_completion_map, "?", "minibuffer_completion_help")
        call make_sparse_keymap(minibuffer_local_must_match_map)
        call set_keymap_parent(minibuffer_local_must_match_map, minibuffer_local_completion_map, status)
        call bind(minibuffer_local_must_match_map, "RET", "minibuffer_complete_and_exit")
        call bind(minibuffer_local_must_match_map, "C-j", "minibuffer_complete_and_exit")
        ! minibuffer-local-filename-completion-map ("SPC" nil) composed with
        ! each: an explicit nil hides the parent's binding.
        call make_sparse_keymap(minibuffer_local_filename_completion_map)
        call set_keymap_parent(minibuffer_local_filename_completion_map, minibuffer_local_completion_map, status)
        call bind_nil(minibuffer_local_filename_completion_map, "SPC")
        call make_sparse_keymap(minibuffer_local_filename_must_match_map)
        call set_keymap_parent(minibuffer_local_filename_must_match_map, minibuffer_local_must_match_map, status)
        call bind_nil(minibuffer_local_filename_must_match_map, "SPC")

    contains

        subroutine bind(map, keys, command)
            type(handle_type), intent(in) :: map
            character(len=*), intent(in) :: keys, command
            type(value_type), allocatable :: seq(:)
            type(status_type) :: st

            call key_parse(keys, seq, st)
            if (st%ok()) call define_key(map, seq, value_type(name=command), st)
        end subroutine bind

        subroutine bind_nil(map, keys)
            type(handle_type), intent(in) :: map
            character(len=*), intent(in) :: keys
            type(value_type), allocatable :: seq(:)
            type(status_type) :: st

            call key_parse(keys, seq, st)
            if (st%ok()) call define_key(map, seq, value_type(), st)
        end subroutine bind_nil
    end subroutine syms_of_minibuffer

    ! ------------------------------------------------------------------
    ! Completion styles
    ! ------------------------------------------------------------------

    !> Return the boundaries of text on which COLLECTION will operate.
    !> STRING is the string on which completion will be performed.
    !> SUFFIX is the string after point.  If COLLECTION is a function, it
    !> is called with STRING, PRED and COMPLETE_BOUNDARIES with SUFFIX.
    !>
    !> The result is (START . END) where START is the position in STRING
    !> of the beginning of the completion field and END is the position in
    !> SUFFIX of the end of the completion field.  E.g. for simple
    !> completion tables, the result is always (0 . (length SUFFIX)) and
    !> for file names the result is the positions delimited by the
    !> closest directory separators.  Port of
    !> minibuffer.el:completion-boundaries.
    recursive subroutine completion_boundaries(string, collection, suffix, bstart, bend, pred)
        character(len=*), intent(in) :: string, suffix
        type(completion_table_type), intent(in) :: collection
        integer(int64), intent(out) :: bstart, bend
        procedure(completion_predicate), optional :: pred
        type(value_type) :: boundaries
        type(status_type) :: status

        bstart = 0
        bend = schars(suffix)

        if (collection%kind == TABLE_QUOTING) then
            call quoting_table_call(collection, string, COMPLETE_BOUNDARIES, boundaries, status, pred, suffix)
        else if (collection%kind == TABLE_FUNCTION) then
            call collection%fn(string, COMPLETE_BOUNDARIES, boundaries, status, pred, suffix)
        else
            return
        end if

        if (.not. status%ok() .or. list_length(boundaries) /= 2) return
        if (vtag(list_elt(boundaries, 1_int64)) == V_INTEGER) bstart = xfixnum(list_elt(boundaries, 1_int64))
        if (vtag(list_elt(boundaries, 2_int64)) == V_INTEGER) bend = xfixnum(list_elt(boundaries, 2_int64))
    end subroutine completion_boundaries

    ! The minibuffer's table and predicate (a null predicate pointer is
    ! passed as an absent argument).  Not a port.
    subroutine mb_boundaries(string, suffix, bstart, bend)
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(out) :: bstart, bend

        if (associated(minibuffer_completion_predicate)) then
            call completion_boundaries(string, minibuffer_completion_table, suffix, bstart, bend, &
                                       minibuffer_completion_predicate)
        else
            call completion_boundaries(string, minibuffer_completion_table, suffix, bstart, bend)
        end if
    end subroutine mb_boundaries

    ! Not a port.
    subroutine mb_try_completion(string, point, res)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res

        if (associated(minibuffer_completion_predicate)) then
            call completion_try_completion(string, minibuffer_completion_table, point, res, &
                                           minibuffer_completion_predicate)
        else
            call completion_try_completion(string, minibuffer_completion_table, point, res)
        end if
    end subroutine mb_try_completion

    ! Not a port.
    subroutine mb_all_completions(string, point, all, base_size)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size

        if (associated(minibuffer_completion_predicate)) then
            call completion_all_completions(string, minibuffer_completion_table, point, all, base_size, &
                                            minibuffer_completion_predicate)
        else
            call completion_all_completions(string, minibuffer_completion_table, point, all, base_size)
        end if
    end subroutine mb_all_completions

    ! Not a port.
    logical function mb_test(string)
        character(len=*), intent(in) :: string

        if (associated(minibuffer_completion_predicate)) then
            mb_test = test_completion(string, minibuffer_completion_table, minibuffer_completion_predicate)
        else
            mb_test = test_completion(string, minibuffer_completion_table)
        end if
    end function mb_test

    ! Not a port.
    subroutine mb_try(string, result, status)
        character(len=*), intent(in) :: string
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status

        if (associated(minibuffer_completion_predicate)) then
            call try_completion(string, minibuffer_completion_table, result, status, minibuffer_completion_predicate)
        else
            call try_completion(string, minibuffer_completion_table, result, status)
        end if
    end subroutine mb_try

    ! ------------------------------------------------------------------
    ! Completion commands
    ! ------------------------------------------------------------------

    !> Temporarily display MESSAGE at the end of minibuffer text.
    !> This function is designed to be called from the minibuffer, i.e.,
    !> when Emacs prompts the user for some input, and the user types
    !> into the minibuffer.  If called when the current buffer is not
    !> the minibuffer, this function just calls `message', and thus
    !> displays MESSAGE in the echo-area.
    !> When called from the minibuffer, this function displays MESSAGE
    !> at the end of minibuffer text for `minibuffer_message_timeout'
    !> seconds, or until the next input event arrives, whichever comes first.
    !> It encloses MESSAGE in [...] if it is not yet enclosed.
    !> The intent is to show the message without hiding what the user typed.
    !> Port of minibuffer.el:minibuffer-message (no ARGS, text properties
    !> or `minibuffer_message' property; the overlay is
    !> minibuffer_message_text).
    subroutine minibuffer_message(msg)
        character(len=*), intent(in) :: msg
        integer :: k
        type(status_type) :: st

        if (.not. is_minibuffer()) then
            call message(msg)
            return
        end if

        ! Clear out any old echo-area message to make way for our new
        ! thing.
        call delete_message_overlay()
        call message("")
        minibuffer_message_text = " ["//msg//"]"
        k = verify(msg, " ")

        if (k > 0 .and. len(msg) >= 2) then
            if (msg(k:k) == "[" .and. msg(len(msg):) == "]" .and. len(msg) - k >= 2) minibuffer_message_text = msg
        end if

        last_minibuffer_message = minibuffer_message_text
        call run_at_time(minibuffer_message_timer, real(minibuffer_message_timeout, real64), &
                         delete_message_overlay_timer, st)
    end subroutine minibuffer_message

    !> Remove the text `minibuffer_message' shows.  Port of
    !> minibuffer.el:minibuffer--delete-message-overlay.
    subroutine delete_message_overlay()
        if (is_timer(minibuffer_message_timer)) call free_timer(minibuffer_message_timer)
        if (allocated(minibuffer_message_text)) deallocate (minibuffer_message_text)
    end subroutine delete_message_overlay

    !> `delete_message_overlay' as a timer function.  Not a port.
    subroutine delete_message_overlay_timer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call delete_message_overlay()
    end subroutine delete_message_overlay_timer

    !> Port of minibuffer.el:completion--message.
    subroutine completion_message(msg)
        character(len=*), intent(in) :: msg

        if (completion_show_inline_help) call minibuffer_message(msg)
    end subroutine completion_message

    !> Run the `:exit_function' of `completion_extra_properties' on STRING
    !> with FINISHED (UNKNOWN finds out whether STRING is a finished
    !> completion), and show MESSAGE unless that function showed one.
    !> Port of minibuffer.el:completion--done.
    recursive subroutine completion_done(string, finished, msg)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished
        character(len=*), intent(in), optional :: msg
        character(len=:), allocatable :: pre_msg, post_msg
        type(value_type) :: tried
        type(status_type) :: st
        integer :: how
        logical :: show

        how = finished
        pre_msg = ""

        if (associated(completion_exit_function)) then
            if (allocated(last_message)) pre_msg = last_message

            if (how == COMPLETION_UNKNOWN) then
                if (associated(minibuffer_completion_predicate)) then
                    call try_completion(string, minibuffer_completion_table, tried, st, minibuffer_completion_predicate)
                else
                    call try_completion(string, minibuffer_completion_table, tried, st)
                end if

                if ((vtag(tried) == V_NAME .or. vtag(tried) == V_NULL) .and. allocated(tried%a)) then
                    how = COMPLETION_FINISHED
                else
                    how = COMPLETION_EXACT
                end if
            end if

            call completion_exit_function(string, how)
        end if

        if (.not. present(msg)) return
        ! Don't output any message if the exit-fun already did so.
        show = .true.

        if (associated(completion_exit_function)) then
            post_msg = ""
            if (allocated(last_message)) post_msg = last_message
            show = pre_msg == post_msg .and. len(pre_msg) == len(post_msg)
        end if

        if (show) call completion_message(msg)
    end subroutine completion_done

    !> Replace the buffer text between BEG and END with NEWTEXT.
    !> Moves point to the end of the new text.  Port of
    !> minibuffer.el:completion--replace.
    subroutine completion_replace(beg, end, newtext)
        integer(int64), intent(in) :: beg, end
        character(len=*), intent(in) :: newtext
        type(status_type) :: status

        call goto_char(beg)
        call del_range(beg, end, status)
        call insert(newtext, status)
        call goto_char(beg + schars(newtext))
    end subroutine completion_replace

    !> Return true if POS is in the current completion boundaries.
    !> Port of minibuffer.el:completion--in-boundaries-p.
    logical function is_in_boundaries(string, suffix, pos)
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(in) :: pos
        integer(int64) :: bstart, bend

        call mb_boundaries(string, suffix, bstart, bend)
        is_in_boundaries = bstart >= pos .and. pos >= schars(string) + bend
    end function is_in_boundaries

    !> Do the completion and return a summary of what happened.
    !> M = completion was performed, the text was Modified.
    !> C = there were available Completions.
    !> E = after completion we now have an Exact match.
    !>
    !>  MCE
    !>  000  0 no possible completion
    !>  001  1 was already an exact and unique completion
    !>  010  2 no completion happened
    !>  011  3 was already an exact completion
    !>  100  4 ??? impossible
    !>  101  5 ??? impossible
    !>  110  6 some completion happened
    !>  111  7 completed to an exact completion
    !>
    !> WORD uses `completion__try_word_completion' in place of
    !> `completion_try_completion'.  EXPECT-EXACT, if .true., means that
    !> there is no need to tell the user when the buffer's text is already
    !> an exact match.  Port of minibuffer.el:completion--do-completion
    !> (no cycling).
    recursive integer function completion_do_completion(beg, end, word, expect_exact) result(bits)
        integer(int64), intent(in) :: beg, end
        logical, intent(in) :: word, expect_exact
        character(len=:), allocatable :: string
        type(try_result_type) :: comp
        type(status_type) :: status
        integer(int64) :: pos, e, comp_pos
        logical :: completed, unchanged, only_changed_boundaries, exact

        call buffer_substring(beg, end, string, status)
        pos = point() - beg

        if (word) then
            call try_word_completion(string, pos, comp)
        else
            call mb_try_completion(string, pos, comp)
        end if

        select case (comp%kind)
        case (TRY_NONE)
            call minibuffer_hide_completions()

            if (.not. completion_fail_discreetly) then
                call ding()
                call completion_message("No match")
            end if

            bits = 0
        case (TRY_EXACT)
            call minibuffer_hide_completions()
            call goto_char(end)

            if (expect_exact) then
                call completion_done(string, COMPLETION_FINISHED)
            else
                call completion_done(string, COMPLETION_FINISHED, "Sole completion")
            end if

            ! Exact and unique match.
            bits = 1
        case default
            ! `completed' should be t if some completion was done, which
            ! doesn't include simply changing the case of the entered
            ! string.  However, for appearance, the string is rewritten if
            ! the case changes.
            comp_pos = comp%point
            completed = compare_strings(comp%string, string, .true.) /= COMPARE_EQUAL
            unchanged = comp%string == string .and. len(comp%string) == len(string)
            only_changed_boundaries = .false.

            if (.not. completed .and. comp_pos /= pos) then
                only_changed_boundaries = .not. is_in_boundaries(substring(string, 0_int64, pos), &
                                                                 substring(string, pos), comp_pos)
            end if

            e = end

            if (unchanged) then
                call goto_char(e)
            else
                ! Insert in minibuffer the chars we got.
                call completion_replace(beg, e, comp%string)
                e = beg + schars(comp%string)
            end if

            ! Move point to its completion-mandated destination.
            call goto_char(point() + comp_pos - schars(comp%string))

            if (.not. (unchanged .or. completed)) then
                ! The case of the string changed, but that's all.  We're
                ! not sure whether this is a unique completion or not, so
                ! try again using the real case (this shouldn't recurse
                ! again, because the next time try-completion will return
                ! either t or the exact string).
                bits = completion_do_completion(beg, e, word, expect_exact)
                return
            end if

            ! It did find a match.  Do we match some possibility exactly
            ! now?
            exact = mb_test(comp%string)

            if (completed .or. only_changed_boundaries) then
                call minibuffer_hide_completions()

                ! If completion did not put point at end of field, it's a
                ! sign that completion is not finished.
                if (exact) then
                    if (comp_pos < schars(comp%string)) then
                        call completion_done(comp%string, COMPLETION_EXACT)
                    else
                        call completion_done(comp%string, COMPLETION_UNKNOWN)
                    end if
                end if
            else if (.not. exact) then
                ! Show the completion table, if requested.
                if (completion_auto_help) then
                    call minibuffer_completion_help(beg, e)
                else
                    call completion_message("Next char not unique")
                end if
            else
                ! If the last exact completion and this one were the same,
                ! it means we've already given a "Complete, but not
                ! unique" message and the user's hit TAB again, so now we
                ! give him help.
                if (associated(this_command, last_command) .and. completion_auto_help) then
                    call minibuffer_completion_help(beg, e)
                end if

                if (expect_exact) then
                    call completion_done(comp%string, COMPLETION_EXACT)
                else
                    call completion_done(comp%string, COMPLETION_EXACT, "Complete, but not unique")
                end if
            end if

            bits = 2
            if (completed) bits = bits + 4
            if (exact) bits = bits + 1
        end select
    end function completion_do_completion

    !> Whether `c' has word syntax (ASCII letters and digits, and non-ASCII
    !> characters: the standard syntax table's main word characters).
    !> Not a port.
    elemental logical function is_word_char(c)
        integer(int32), intent(in) :: c

        is_word_char = (c >= iachar("a") .and. c <= iachar("z")) .or. (c >= iachar("A") .and. c <= iachar("Z")) &
                       .or. (c >= iachar("0") .and. c <= iachar("9")) .or. c >= 128
    end function is_word_char

    !> Port of minibuffer.el:completion--try-word-completion (the cut
    !> after the first word only when there is no text after point).
    subroutine try_word_completion(string, point, comp)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: comp
        type(try_result_type) :: tem
        character(len=:), allocatable :: before, after, completion
        character(len=1), parameter :: exts(2) = [" ", "-"]
        integer(int64) :: comppoint, nbefore, ncomp, j, mb1, k, cutpos
        integer(int32) :: c, clen
        integer(int64) :: byte
        integer :: e

        call mb_try_completion(string, point, comp)
        if (comp%kind /= TRY_STRING) return
        before = substring(string, 0_int64, point)
        after = substring(string, point)

        ! If completion finds next char not unique, consider adding a space
        ! or a hyphen.  If both " " and "-" lead to completions, prefer " "
        ! so SPC behaves a bit more like a self-inserting key (bug#17375).
        if (schars(string) == schars(comp%string)) then
            do e = 1, size(exts)
                call mb_try_completion(before//exts(e)//after, point + 1, tem)

                if (tem%kind == TRY_STRING) then
                    comp = tem
                    exit
                end if
            end do
        end if

        ! Completing a single word is actually more difficult than
        ! completing as much as possible, because we first have to find the
        ! "current position" in `completion' in order to find the end of
        ! the word we're completing.  Be as conservative as possible: only
        ! cut after the first word, if we're reasonably sure that our guess
        ! is correct.
        completion = comp%string
        comppoint = comp%point
        nbefore = schars(before)
        ncomp = schars(completion)
        if (len(after) > 0 .or. nbefore == 0) return
        if (comppoint /= ncomp) return
        ! The last occurrence of the text before point in the completion.
        j = -1

        do k = ncomp - nbefore, 0, -1
            if (substring(completion, k, k + nbefore) == before) then
                j = k
                exit
            end if
        end do

        if (j < 0) return
        mb1 = j + nbefore
        ! The first non-word character from there: cut after it.
        cutpos = -1
        byte = len(substring(completion, 0_int64, mb1)) + 1
        k = mb1

        do while (byte <= len(completion))
            call string_char_and_length(completion, byte, c, clen)

            if (.not. is_word_char(c)) then
                cutpos = k + 1
                exit
            end if

            byte = byte + clen
            k = k + 1
        end do

        ! Is there really something to cut?
        if (cutpos < 0 .or. comppoint <= cutpos) return
        comp%string = substring(completion, 0_int64, cutpos)//substring(completion, comppoint)
        comp%point = cutpos
    end subroutine try_word_completion

    !> Where completion starts in the minibuffer; an error in the prompt.
    !> Port of minibuffer.el:minibuffer--completion-prompt-end.
    integer(int64) function completion_prompt_end(status) result(pos)
        type(status_type), intent(out) :: status

        status = status_type()
        pos = minibuffer_prompt_end()
        if (point() < pos) call set_error(status, FM_USER_ERROR, format_message("Can't complete in prompt"))
    end function completion_prompt_end

    !> Complete the minibuffer contents as far as possible.
    !> Return false if there is no valid completion, else true.  If no
    !> characters can be completed, display a list of possible
    !> completions.  If you repeat this command after it displayed such a
    !> list, scroll the window of possible completions.  Port of
    !> minibuffer.el:minibuffer-complete (completion-in-region and
    !> completion--in-region-1).
    subroutine minibuffer_complete(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        logical :: res

        beg = completion_prompt_end(status)
        if (.not. status%ok()) return

        if (associated(minibuffer_completion_predicate)) then
            res = completion_in_region(beg, point_max(), minibuffer_completion_table, minibuffer_completion_predicate)
        else
            res = completion_in_region(beg, point_max(), minibuffer_completion_table)
        end if
    end subroutine minibuffer_complete

    !> Complete the text between START and END using COLLECTION.
    !> Point needs to be somewhere between START and END.
    !> PREDICATE says which candidates to accept (upstream's argument is a
    !> predicate on candidates, documented as one that says when to exit).
    !> This calls the function that `completion_in_region_function'
    !> specifies (passing the same four arguments that it received) to do
    !> the work, and returns whatever it does.  The return value should be
    !> false if there was no valid completion, else true.  Port of
    !> minibuffer.el:completion-in-region.
    recursive logical function completion_in_region(start, end, collection, predicate) result(res)
        integer(int64), intent(in) :: start, end
        type(completion_table_type), intent(in) :: collection
        procedure(completion_predicate), optional :: predicate

        if (associated(completion_in_region_function)) then
            res = completion_in_region_function(start, end, collection, predicate)
        else
            res = completion_in_region_default(start, end, collection, predicate)
        end if
    end function completion_in_region

    !> Default function to use for `completion_in_region_function'.
    !> Its arguments and return value are as specified for
    !> `completion_in_region'.  Port of minibuffer.el:completion--in-region
    !> (no `completion_in_region_functions', which is obsolete).
    recursive logical function completion_in_region_default(start, end, collection, predicate) result(res)
        integer(int64), intent(in) :: start, end
        type(completion_table_type), intent(in) :: collection
        procedure(completion_predicate), optional :: predicate
        type(completion_table_type) :: saved_table
        procedure(completion_predicate), pointer :: saved_pred
        type(status_type) :: st

        saved_table = minibuffer_completion_table
        saved_pred => minibuffer_completion_predicate
        minibuffer_completion_table = collection
        minibuffer_completion_predicate => null()
        if (present(predicate)) minibuffer_completion_predicate => predicate

        if (associated(completion_in_region_mode_predicate)) then
            call free_cir_data()
            call copy_marker_at(start, cir_start, .false., st)
            call copy_marker_at(end, cir_end, .true., st)
            cir_table = collection
            cir_pred => null()
            if (present(predicate)) cir_pred => predicate
            cir_data = .true.
            call minor_modes%completion_in_region_mode%enable(st)
        end if

        res = completion_in_region_1(start, end)
        minibuffer_completion_table = saved_table
        minibuffer_completion_predicate => saved_pred
    end function completion_in_region_default

    !> Port of minibuffer.el:completion--in-region-1 (no cycling and no
    !> `completion_auto_select').
    recursive logical function completion_in_region_1(beg, end) result(res)
        integer(int64), intent(in) :: beg, end
        type(handle_type) :: window
        type(status_type) :: st
        integer :: bits

        res = .false.
        ! If the previous command was not this, mark the completion buffer
        ! obsolete.
        this_command => commands%completion_at_point
        if (.not. associated(last_command, commands%completion_at_point)) minibuffer_scroll_window = null_handle()

        ! If there's a fresh completion window with a live buffer, and this
        ! command is repeated, scroll that window.
        if (is_window_live(minibuffer_scroll_window)) then
            window = minibuffer_scroll_window

            if (is_backtab()) then
                call reverse_scroll_completions(window)
            else
                call scroll_completions(window)
            end if

            return
        end if

        bits = completion_do_completion(beg, end, .false., .false.)
        res = bits /= 0
        if (.not. is_window_live(minibuffer_scroll_window)) call minor_modes%completion_in_region_mode%disable(st)

    contains

        !> Whether the keys of this command are [backtab].
        logical function is_backtab()
            is_backtab = .false.
            if (.not. allocated(this_command_keys)) return
            if (size(this_command_keys) /= 1) return
            is_backtab = value_eq(this_command_keys(1), value_type(name="backtab"))
        end function is_backtab
    end function completion_in_region_1

    !> Reverse tab in the completion list WINDOW: if its beginning is in
    !> view, scroll up to the end, else scroll down one screen.  Not a port.
    subroutine reverse_scroll_completions(window)
        type(handle_type), intent(in) :: window
        type(status_type) :: status
        type(handle_type) :: old
        integer(int64) :: zv, begv

        old = current_buffer()
        call set_buffer(windows(window%index)%buffer, status)
        if (.not. status%ok()) return
        zv = buffers(cur)%zv
        begv = buffers(cur)%begv
        call set_buffer(old, status)

        if (is_pos_visible_in_window(begv, window)) then
            call set_window_point(windows(window%index), zv, status)
        else
            call scroll_window(window, -1, status)
        end if
    end subroutine reverse_scroll_completions

    !> Forget completion-in-region--data, freeing its markers.  Not a port.
    subroutine free_cir_data()
        type(status_type) :: st

        if (cir_start%index /= 0) call free_marker(cir_start, st)
        if (cir_end%index /= 0) call free_marker(cir_end, st)
        cir_start = null_handle()
        cir_end = null_handle()
        cir_pred => null()
        cir_data = .false.
    end subroutine free_cir_data

    !> completion-in-region-mode's post-command-hook function: leave the
    !> mode once point leaves the completed text (up to the end of its
    !> line) or the mode's predicate says so.  Port of
    !> minibuffer.el:completion-in-region--postch.
    recursive subroutine completion_in_region_postch(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: b
        integer(int64) :: start, end, saved
        logical :: stay

        status = status_type()
        ! Don't pop down the completions in the middle of
        ! mouse-drag-region/mouse-set-point.
        if (has_unread_command_events()) return
        stay = .false.

        if (cir_data) then
            call marker_buffer(cir_start, b, status)

            if (status%ok() .and. same_handle(b, current_buffer())) then
                call marker_position(cir_start, start, status)
                call marker_position(cir_end, end, status)

                if (point() >= start) then
                    saved = point()
                    call goto_char(end)
                    end = line_end_position(1_int64)
                    call goto_char(saved)

                    if (point() <= end) then
                        if (associated(cir_mode_predicate)) stay = cir_mode_predicate()
                    end if
                end if
            end if
        end if

        status = status_type()
        if (.not. stay) call minor_modes%completion_in_region_mode%disable(status)
    end subroutine completion_in_region_postch

    !> The body of `completion_in_region_mode'.  Port of
    !> minibuffer.el:completion-in-region-mode.
    recursive subroutine completion_in_region_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(window_type), pointer :: w

        status = status_type()
        call post_command_hook%remove(completion_in_region_postch)
        call remove_minor_mode_overriding_map("completion_in_region_mode")

        if (.not. enable) then
            call free_cir_data()
            call free_capf_field(cir_mode_capf_start)
            cir_mode_capf_fun => null()
            w => frame_selected_window_ref()
            if (buffers(w%buffer%index)%name /= "*Completions*") call minibuffer_hide_completions()
        else
            cir_mode_predicate => completion_in_region_mode_predicate
            ! The closure of `capf_same_field'.
            call free_capf_field(cir_mode_capf_start)
            cir_mode_capf_fun => capf_field_fun
            if (capf_field_start%index /= 0) call copy_marker(capf_field_start, cir_mode_capf_start, .false., status)
            status = status_type()
            call post_command_hook%add(completion_in_region_postch)
            call push_minor_mode_overriding_map("completion_in_region_mode", completion_in_region_mode_map)
        end if
    end subroutine completion_in_region_mode_body

    ! Not a port.
    subroutine free_capf_field(m)
        type(handle_type), intent(inout) :: m
        type(status_type) :: st

        if (m%index /= 0) call free_marker(m, st)
        m = null_handle()
    end subroutine free_capf_field

    !> The completion data of the first function of
    !> `completion_at_point_functions' that applies at point (with a
    !> non-exclusive table, only if prefix completion of the text before
    !> point finds something).  Port of minibuffer.el:completion--capf-wrapper
    !> run through run-hook-wrapped.
    recursive subroutine run_capfs(data, fn, status, functions)
        type(capf_data_type), intent(out) :: data
        procedure(capf_function), pointer, intent(out) :: fn
        type(status_type), intent(out) :: status
        type(capf_entry_type), intent(in), optional :: functions(:)
        type(capf_entry_type), allocatable :: fns(:)
        type(value_type) :: v
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: k

        fn => null()

        if (present(functions)) then
            fns = functions
        else
            fns = completion_at_point_functions%functions()
        end if

        do k = 1, size(fns)
            call fns(k)%fn(data, status)
            if (.not. status%ok()) return
            if (.not. data%found) cycle

            if (.not. data%exclusive) then
                call buffer_substring(data%start, point(), text, st)

                if (associated(data%predicate)) then
                    call try_completion(text, data%table, v, st, data%predicate)
                else
                    call try_completion(text, data%table, v, st)
                end if

                if (.not. allocated(v%a)) then
                    data%found = .false.
                    cycle
                end if
            end if

            fn => fns(k)%fn
            return
        end do

        data = capf_data_type()
    end subroutine run_capfs

    !> completion-at-point's completion-in-region-mode-predicate: we're
    !> still in the same completion field.
    !> Port of minibuffer.el:completion-at-point in part.
    recursive logical function capf_same_field() result(same)
        type(capf_data_type) :: data
        type(status_type) :: st
        integer(int64) :: start

        same = .false.
        if (.not. associated(cir_mode_capf_fun)) return
        call cir_mode_capf_fun(data, st)
        if (.not. st%ok() .or. .not. data%found) return
        call marker_position(cir_mode_capf_start, start, st)
        same = data%start == start
    end function capf_same_field

    !> Perform completion on the text around point.
    !> The completion method is determined by
    !> `completion_at_point_functions'.  Port of
    !> minibuffer.el:completion-at-point.
    recursive subroutine completion_at_point(status)
        type(status_type), intent(out) :: status
        logical :: res

        call completion_at_point_run(res, status)
    end subroutine completion_at_point

    !> `completion_at_point' giving in RES whether there was a valid
    !> completion; with FUNCTIONS, those instead of
    !> `completion_at_point_functions' (saved and restored around the call; upstream binds the hook).
    !> Port of minibuffer.el:completion-at-point.
    recursive subroutine completion_at_point_run(res, status, functions)
        logical, intent(out) :: res
        type(status_type), intent(out) :: status
        type(capf_entry_type), intent(in), optional :: functions(:)
        type(capf_data_type) :: data
        procedure(capf_function), pointer :: fn, saved_fun
        procedure(cir_predicate_proc), pointer :: saved_pred
        procedure(completion_exit_function_proc), pointer :: saved_exit
        type(handle_type) :: saved_start

        status = status_type()
        res = .false.
        call run_capfs(data, fn, status, functions)
        if (.not. status%ok()) return
        if (.not. data%found) return

        if (associated(data%perform)) then
            res = data%perform()
            return
        end if

        saved_exit => completion_exit_function
        completion_exit_function => data%exit_function
        saved_fun => capf_field_fun
        saved_start = capf_field_start
        saved_pred => completion_in_region_mode_predicate
        capf_field_fun => fn
        call copy_marker_at(data%start, capf_field_start, .false., status)
        completion_in_region_mode_predicate => capf_same_field

        if (associated(data%predicate)) then
            res = completion_in_region(data%start, data%end, data%table, data%predicate)
        else
            res = completion_in_region(data%start, data%end, data%table)
        end if

        call free_capf_field(capf_field_start)
        capf_field_fun => saved_fun
        capf_field_start = saved_start
        completion_in_region_mode_predicate => saved_pred
        completion_exit_function => saved_exit
        status = status_type()
    end subroutine completion_at_point_run

    !> Display the completions on the text around point.
    !> The completion method is determined by
    !> `completion_at_point_functions'.  Port of
    !> minibuffer.el:completion-help-at-point (ONLY-IF-EAGER, for the
    !> deferred eager display, is left out).
    recursive subroutine completion_help_at_point(status)
        type(status_type), intent(out) :: status
        type(capf_data_type) :: data
        procedure(capf_function), pointer :: fn, saved_fun
        procedure(cir_predicate_proc), pointer :: saved_pred
        type(completion_table_type) :: saved_table
        procedure(completion_predicate), pointer :: saved_mb_pred
        type(handle_type) :: saved_start

        call run_capfs(data, fn, status)
        if (.not. status%ok()) return

        if (.not. data%found) then
            call message("Nothing to complete at point")
            return
        end if

        saved_table = minibuffer_completion_table
        saved_mb_pred => minibuffer_completion_predicate
        saved_fun => capf_field_fun
        saved_start = capf_field_start
        saved_pred => completion_in_region_mode_predicate
        minibuffer_completion_table = data%table
        minibuffer_completion_predicate => data%predicate
        capf_field_fun => fn
        call copy_marker_at(data%start, capf_field_start, .false., status)
        completion_in_region_mode_predicate => capf_same_field
        call free_cir_data()
        call copy_marker_at(data%start, cir_start, .false., status)
        call copy_marker_at(data%end, cir_end, .true., status)
        cir_table = data%table
        cir_pred => data%predicate
        cir_data = .true.
        call minor_modes%completion_in_region_mode%enable(status)
        call minibuffer_completion_help(data%start, data%end)
        call free_capf_field(capf_field_start)
        capf_field_fun => saved_fun
        capf_field_start = saved_start
        completion_in_region_mode_predicate => saved_pred
        minibuffer_completion_table = saved_table
        minibuffer_completion_predicate => saved_mb_pred
        status = status_type()
    end subroutine completion_help_at_point

    !> Complete the minibuffer contents at most a single word.
    !> After one word is completed as much as possible, a space or hyphen
    !> is added, provided that matches some possible completion.  Return
    !> false if there is no valid completion, else true.  Port of
    !> minibuffer.el:minibuffer-complete-word.
    subroutine minibuffer_complete_word(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        integer :: bits

        beg = completion_prompt_end(status)
        if (.not. status%ok()) return
        bits = completion_do_completion(beg, point_max(), .true., .false.)
    end subroutine minibuffer_complete_word

    !> Exit if the minibuffer contains a valid completion.
    !> Otherwise, try to complete the minibuffer contents.  If completion
    !> leads to a valid completion, a repetition of this command will
    !> exit.
    !>
    !> If `minibuffer_completion_confirm' is `confirm', do not try to
    !> complete; instead, ask for confirmation and accept any input if
    !> confirmed.  If `minibuffer_completion_confirm' is
    !> `confirm_after_completion', do not try to complete; instead, ask
    !> for confirmation if the preceding minibuffer command was a member
    !> of `minibuffer_confirm_exit_commands', and accept the input
    !> otherwise.  Port of minibuffer.el:minibuffer-complete-and-exit
    !> (with completion-complete-and-exit and
    !> completion--complete-and-exit).
    subroutine minibuffer_complete_and_exit(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg

        beg = completion_prompt_end(status)
        if (.not. status%ok()) return
        call completion_complete_and_exit(beg, point_max(), exit_minibuffer, status)
    end subroutine minibuffer_complete_and_exit

    !> Exit from `require_match' minibuffer.
    !> COMPLETION_FUNCTION is called if the current buffer's content does
    !> not appear to be a match: here the text from BEG to END is completed,
    !> and EXIT_FUNCTION called if that gives a valid completion.  Port of
    !> minibuffer.el:completion-complete-and-exit (with
    !> completion--complete-and-exit).
    recursive subroutine completion_complete_and_exit(beg, end, exit_function, status)
        integer(int64), intent(in) :: beg, end
        procedure(completion_exit_proc) :: exit_function
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        type(value_type) :: compl
        integer :: bits

        status = status_type()

        ! Allow user to specify null string.
        if (beg == end) then
            call exit_function(status)
            return
        end if

        call buffer_substring(beg, end, string, status)

        ! See if we have a completion from the table.
        if (mb_test(string)) then
            if (completion_ignore_case%value) then
                ! Fixup case of the field, if necessary.
                call mb_try(string, compl, status)

                if (vtag(compl) == V_STRING) then
                    if (xstring(compl) /= string .and. schars(xstring(compl)) == schars(string)) then
                        call completion_replace(beg, end, xstring(compl))
                    end if
                end if
            end if

            call exit_function(status)
            return
        end if

        ! The user is permitted to exit with an input that's rejected by
        ! test-completion, after confirming her choice.
        if (minibuffer_completion_confirm == REQUIRE_MATCH_CONFIRM .or. &
            minibuffer_completion_confirm == REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION) then
            ! hazard-ok(short-circuit): associated(a, b) compares, it guards nothing
            if (associated(last_command, this_command) .or. &
                (minibuffer_completion_confirm == REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION .and. &
                 .not. confirm_exit_command(last_command))) then
                call exit_function(status)
            else
                call minibuffer_message("Confirm")
            end if

            return
        end if

        ! Call do-completion, but ignore errors.
        bits = completion_do_completion(beg, end, .false., .true.)

        select case (bits)
        case (1, 3)
            call exit_function(status)
        case (7)
            if (minibuffer_completion_confirm == REQUIRE_MATCH_NO) then
                call exit_function(status)
            else
                call minibuffer_message("Confirm")
            end if
        case default
            ! Not a match: stay in the minibuffer.
        end select
    end subroutine completion_complete_and_exit

    !> Whether `cmd' is in minibuffer-confirm-exit-commands.  Not a port.
    logical function confirm_exit_command(cmd)
        type(command_type), pointer, intent(in) :: cmd

        confirm_exit_command = associated(cmd, commands%completion_at_point) .or. &
                               associated(cmd, commands%minibuffer_complete) .or. &
                               associated(cmd, commands%minibuffer_complete_word)
    end function confirm_exit_command

    ! ------------------------------------------------------------------
    ! The *Completions* buffer
    ! ------------------------------------------------------------------

    !> The window showing *Completions*, or the null handle.  Port of
    !> minibuffer.el:minibuffer--completions-visible.
    function completions_visible() result(window)
        type(handle_type) :: window
        type(handle_type) :: buf

        window = null_handle()
        buf = get_buffer("*Completions*")
        if (buf%index /= 0) window = get_buffer_window(buf)
    end function completions_visible

    !> Scroll the window of possible completions: to the top when its end
    !> is visible, else down a screen.  Port of
    !> minibuffer.el:completion--in-region-1 (its scrolling part).
    subroutine scroll_completions(window)
        type(handle_type), intent(in) :: window
        type(status_type) :: status
        type(handle_type) :: old
        integer(int64) :: zv, begv

        old = current_buffer()
        call set_buffer(windows(window%index)%buffer, status)
        if (.not. status%ok()) return
        zv = buffers(cur)%zv
        begv = buffers(cur)%begv
        call set_buffer(old, status)

        if (is_pos_visible_in_window(zv, window)) then
            ! If end is in view, scroll up to the end.
            call set_window_start(windows(window%index), begv, status)
        else
            ! Else scroll down one screen.
            call scroll_window(window, 1, status)
        end if
    end subroutine scroll_completions

    !> Display a list of possible completions of the current minibuffer
    !> contents.  The list goes in buffer *Completions*, shown in the
    !> frame's root window until the minibuffer is done (there is no
    !> window splitting yet).  Port of
    !> minibuffer.el:minibuffer-completion-help (completions-sort
    !> alphabetical, no annotations or groups).
    recursive subroutine minibuffer_completion_help(start, end)
        integer(int64), intent(in), optional :: start, end
        type(value_type) :: completions
        type(value_type), allocatable :: elts(:)
        type(string_box), allocatable :: strings(:)
        type(string_box), allocatable :: annotations(:)
        type(handle_type) :: mini, buf, window
        type(status_type) :: status
        type(display_action_table_type) :: table
        character(len=:), allocatable :: string
        integer(int64) :: s, e, base_size
        integer :: k, saved_mark

        call message("Making completion list...")
        s = minibuffer_prompt_end()
        if (present(start)) s = start
        e = point_max()
        if (present(end)) e = end
        call buffer_substring(s, e, string, status)
        call mb_all_completions(string, point() - s, completions, base_size)
        call message("")
        call list_elements(completions, elts)

        if (size(elts) == 0) then
            ! If there are no completions, or if the current input is
            ! already the sole completion, then hide (previous&stale)
            ! completions.
            call minibuffer_hide_completions()

            if (.not. completion_fail_discreetly) then
                call ding()
                call completion_message("No match")
            end if

            return
        end if

        if (size(elts) == 1) then
            if (xstring(elts(1)) == string .and. len(xstring(elts(1))) == len(string)) then
                call minibuffer_hide_completions()
                call completion_message("Sole completion")
                return
            end if
        end if

        allocate (strings(size(elts)))

        do k = 1, size(elts)
            strings(k)%s = xstring(elts(k))
        end do

        if (completion_flex_sorting) then
            ! The display-sort-function of completion--flex-adjust-metadata.
            call sort_by_flex_cost(strings)
        else if (.not. minibuffer_completion_table%unsorted) then
            ! minibuffer-sort-alphabetically.
            call sort_strings(strings)
        end if

        mini = current_buffer()
        call get_buffer_create("*Completions*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        call set_buffer_read_only(buf, .false., status)
        call erase_buffer(status)

        ! The `:annotation_function' of completion-extra-properties.
        if (associated(completion_annotation_function)) then
            allocate (annotations(size(strings)))

            do k = 1, size(strings)
                call completion_annotation_function(strings(k)%s, annotations(k)%s)
            end do

            call display_completion_list(strings, annotations)
        else
            call display_completion_list(strings)
        end if

        call set_buffer(mini, status)
        ! If the *Completions* buffer is shown in a new window, mark it
        ! as softly-dedicated, so bury-buffer in
        ! minibuffer-hide-completions will know whether to delete the
        ! window or not.
        saved_mark = display_buffer_mark_dedicated
        display_buffer_mark_dedicated = DEDICATED_WEAK
        ! A copy of `display_buffer_fallback_action' where
        ! `display_buffer_use_some_window' is replaced with
        ! `display_buffer_at_bottom' (with-current-buffer-window).
        table%window_height => completions_fit_window_to_buffer

        if (selected_frame%minibuffer_selected) then
            call temp_buffer_window_show(buf, window, status, [DISPLAY_ACTION_MAYBE_SAME_WINDOW, &
                                                               DISPLAY_ACTION_REUSE_WINDOW, DISPLAY_ACTION_AT_BOTTOM], table)
        else
            call temp_buffer_window_show(buf, window, status, [DISPLAY_ACTION_MAYBE_SAME_WINDOW, &
                                                               DISPLAY_ACTION_REUSE_WINDOW], table)
        end if

        display_buffer_mark_dedicated = saved_mark
    end subroutine minibuffer_completion_help

    !> Resize *Completions* buffer window.  Port of
    !> minibuffer.el:completions--fit-window-to-buffer
    !> (`temp_buffer_resize_mode' off).
    subroutine completions_fit_window_to_buffer(window)
        type(handle_type), intent(in) :: window

        if (completions_max_height > 0) then
            call fit_window_to_buffer(window, completions_max_height)
        else
            call fit_window_to_buffer(window)
        end if
    end subroutine completions_fit_window_to_buffer

    !> Sort `strings' as `is_string_less' does.  Not a port.
    subroutine sort_strings(strings)
        type(string_box), intent(inout) :: strings(:)
        character(len=:), allocatable :: tmp
        integer :: i, j

        do i = 2, size(strings)
            tmp = strings(i)%s
            j = i - 1

            do while (j >= 1)
                if (.not. is_string_less(value_type(text=tmp), value_type(text=strings(j)%s))) exit
                strings(j + 1)%s = strings(j)%s
                j = j - 1
            end do

            strings(j + 1)%s = tmp
        end do
    end subroutine sort_strings

    !> Sort `strings' by their flex cost in `flex_costs' (lower first,
    !> keeping the order of equal costs).  Not a port.
    subroutine sort_by_flex_cost(strings)
        type(string_box), intent(inout) :: strings(:)
        character(len=:), allocatable :: tmp
        integer, allocatable :: costs(:)
        integer :: i, j, c

        allocate (costs(size(strings)))

        do i = 1, size(strings)
            costs(i) = 0

            do j = 1, size(flex_costs)
                if (flex_costs(j)%s == strings(i)%s .and. len(flex_costs(j)%s) == len(strings(i)%s)) then
                    costs(i) = flex_costs(j)%cost
                    exit
                end if
            end do
        end do

        do i = 2, size(strings)
            tmp = strings(i)%s
            c = costs(i)
            j = i - 1

            do while (j >= 1)
                if (costs(j) <= c) exit
                strings(j + 1)%s = strings(j)%s
                costs(j + 1) = costs(j)
                j = j - 1
            end do

            strings(j + 1)%s = tmp
            costs(j + 1) = c
        end do
    end subroutine sort_by_flex_cost

    !> Display the list of completions, STRINGS, in the current buffer:
    !> the heading line, then the candidates in columns.  Port of
    !> minibuffer.el:display-completion-list with completion--insert-strings
    !> and completion--insert-horizontal (completions-format horizontal;
    !> spaces in place of the `display' alignment, no truncation); ANNOTATIONS
    !> are the candidates' suffix annotations, if any.
    subroutine display_completion_list(strings, annotations)
        type(string_box), intent(in) :: strings(:)
        type(string_box), intent(in), optional :: annotations(:)
        type(status_type) :: status
        character(len=20) :: count
        integer(int64) :: wwidth
        type(window_type), pointer :: sw

        call goto_char(point_max())
        ! completions-header-format
        write (count, "(i0)") size(strings)
        call insert(trim(count)//" possible completions:"//achar(10), status)
        if (size(strings) == 0) return
        wwidth = 79
        sw => frame_selected_window_ref()
        if (sw%total_cols > 0) wwidth = sw%total_cols - 1
        call insert_strings_horizontal(strings, wwidth, annotations)
    end subroutine display_completion_list

    !> Insert STRINGS into the current buffer in columns: the window
    !> showing the buffer gives the width (79 columns if none).  Port of
    !> minibuffer.el:completion--insert-strings with
    !> completion--insert-horizontal (no GROUP-FUN; spaces in place of the
    !> `display' alignment, no truncation).
    subroutine completion_insert_strings(strings)
        type(minibuffer_string_type), intent(in) :: strings(:)
        type(string_box), allocatable :: boxes(:)
        type(handle_type) :: window
        integer(int64) :: wwidth
        integer :: k

        if (size(strings) == 0) return
        allocate (boxes(size(strings)))

        do k = 1, size(strings)
            boxes(k)%s = strings(k)%s
        end do

        wwidth = 79
        window = get_buffer_window(current_buffer())
        if (is_window_live(window)) wwidth = windows(window%index)%total_cols - 1
        call insert_strings_horizontal(boxes, wwidth)
    end subroutine completion_insert_strings

    !> The candidates STRINGS in columns of a window WWIDTH columns wide,
    !> each followed by its annotation in ANNOTATIONS (in the face
    !> `completions_annotations'), if present.  Port of
    !> minibuffer.el:completion--insert-strings, completion--insert-horizontal
    !> and completion--insert (see above).
    subroutine insert_strings_horizontal(strings, wwidth, annotations)
        type(string_box), intent(in) :: strings(:)
        integer(int64), intent(in) :: wwidth
        type(string_box), intent(in), optional :: annotations(:)
        type(status_type) :: status
        character(len=:), allocatable :: last_string
        integer(int64), allocatable :: widths(:)
        integer(int64) :: length, columns, colwidth, column, cur_col, width, start
        integer :: k
        logical :: first

        allocate (widths(size(strings)))

        do k = 1, size(strings)
            widths(k) = schars(strings(k)%s)
            if (present(annotations)) widths(k) = widths(k) + schars(annotations(k)%s)
        end do

        length = 0
        if (size(widths) > 0) length = maxval(widths)
        ! At least 2 spaces between columns; don't allocate more columns
        ! than we can fill.
        columns = min(max(1_int64, wwidth/(2 + length)), max(1_int64, int(size(strings), int64)/2))
        colwidth = wwidth/columns
        column = 0
        cur_col = 0
        first = .true.

        do k = 1, size(strings)
            ! Remove (consecutive) duplicates.
            if (.not. first) then
                if (strings(k)%s == last_string .and. len(strings(k)%s) == len(last_string)) cycle
            end if

            last_string = strings(k)%s
            width = widths(k)

            if (.not. first) then
                if (wwidth < column + max(colwidth, width)) then
                    ! No space for `str' at point, move to next line.
                    call insert(achar(10), status)
                    column = 0
                    cur_col = 0
                else
                    ! A space, then alignment to the column.
                    call insert(" "//repeat(" ", int(max(0_int64, column - cur_col - 1))), status)
                    cur_col = max(cur_col + 1, column)
                end if
            end if

            first = .false.
            call insert(strings(k)%s, status)

            ! The suffix annotation, in the predefined face.
            if (present(annotations)) then
                start = point()
                call insert(annotations(k)%s, status)

                if (point() > start) then
                    call add_face_text_property(start, point(), value_type(name="completions_annotations"), status)
                end if
            end if

            cur_col = cur_col + width
            ! Next column to align to.
            column = column + colwidth*((length + colwidth - 1)/colwidth)
        end do
    end subroutine insert_strings_horizontal

    !> Get rid of an out-of-date *Completions* buffer: bury it in its
    !> window, which is deleted if it was made for it (dedicated), or
    !> else shows the buffer it showed before.  Port of
    !> minibuffer.el:minibuffer-hide-completions (with-selected-window and
    !> bury-buffer).
    subroutine minibuffer_hide_completions()
        type(status_type) :: status
        type(handle_type) :: window, buffer, b

        window = completions_visible()
        if (.not. is_window_live(window)) return
        buffer = windows(window%index)%buffer
        call bury_buffer_internal(buffer)

        ! window--delete with DEDICATED-ONLY.
        if (windows(window%index)%dedicated /= DEDICATED_NONE .and. &
            .not. same_window(window, selected_frame%root_window)) then
            call delete_window(window, status)
            if (status%ok()) return
        end if

        ! Switch to another buffer in window.
        windows(window%index)%dedicated = DEDICATED_NONE
        b = switch_to_prev_buffer(window, BK_BURY, status)
    end subroutine minibuffer_hide_completions

    ! ------------------------------------------------------------------
    ! Reading with completion
    ! ------------------------------------------------------------------

    !> Read the name of a buffer and return it as a string.
    !> Prompt with PROMPT, which should be a string ending with a colon and a space.
    !> Provides completion on buffer names the user types.
    !> Optional second arg DEF is value to return if user enters an empty line,
    !>  instead of that empty string; the prompt shows it.
    !> Optional third arg REQUIRE_MATCH has the same meaning as the
    !>  REQUIRE_MATCH argument of `completing_read'.
    !> If `read_buffer_completion_ignore_case' is true, completion ignores
    !> case while reading the buffer name.
    !> The buffer `except' (if given) is left out of the completions.
    !> Port of minibuf.c:Fread_buffer (`except' as internal-complete-buffer-except;
    !> read-buffer-to-switch puts that table in the minibuffer; no
    !> PREDICATE or read-buffer-function).
    recursive subroutine read_buffer(prompt, result, status, def, require_match, except)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: def
        integer, intent(in), optional :: require_match
        type(handle_type), intent(in), optional :: except
        type(completion_table_type) :: table
        character(len=:), allocatable :: p, saved_except
        logical :: saved_ignore_case, had_except
        integer :: match

        match = REQUIRE_MATCH_NO
        if (present(require_match)) match = require_match
        had_except = allocated(rbts_except)
        if (had_except) call move_alloc(rbts_except, saved_except)
        table = completion_function_table(internal_complete_buffer, "buffer")

        if (present(except)) then
            rbts_except = buffers(except%index)%name
            table = completion_function_table(internal_complete_buffer_except, "buffer")
        end if

        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case = read_buffer_completion_ignore_case
        p = prompt

        if (present(def)) then
            ! A default value was provided: we must change PROMPT, editing
            ! the default value in before the colon (format-prompt with
            ! minibuffer-default-prompt-format " (default %s)").
            if (len(p) >= 2) then
                if (p(len(p) - 1:) == ": ") then
                    p = p(1:len(p) - 2)
                else if (p(len(p):) == ":" .or. p(len(p):) == " ") then
                    p = p(1:len(p) - 1)
                end if
            else if (len(p) == 1) then
                if (p == ":" .or. p == " ") p = ""
            end if

            if (len(def) > 0) then
                p = p//" (default "//def//"): "
            else
                p = p//": "
            end if

            call completing_read(p, table, result, status, require_match=match, history="buffer_name_history", &
                                 default=def)
        else
            call completing_read(p, table, result, status, require_match=match, history="buffer_name_history")
        end if

        completion_ignore_case = saved_ignore_case
        if (allocated(rbts_except)) deallocate (rbts_except)
        if (had_except) call move_alloc(saved_except, rbts_except)
    end subroutine read_buffer

    !> The names of all buffers, in the order of `buffer_list'
    !> (Vbuffer_alist).  Not a port.
    function buffer_names() result(names)
        type(value_type) :: names
        type(handle_type), allocatable :: all(:)
        type(value_type), allocatable :: elts(:)
        integer :: k

        all = buffer_list()
        allocate (elts(size(all)))

        ! Most recently selected first.
        do k = 1, size(all)
            elts(k) = value_type(text=buffers(all(size(all) + 1 - k)%index)%name)
        end do

        names = make_list(elts)
    end function buffer_names

    !> Perform completion on buffer names.
    !> STRING and PREDICATE have the same meanings as in `try_completion',
    !> `all_completions', and `test_completion'.
    !>
    !> If FLAG is COMPLETE_TRY, invoke `try_completion'; if it is
    !> COMPLETE_ALL, invoke `all_completions'; otherwise invoke
    !> `test_completion'.  Port of minibuf.c:Finternal_complete_buffer.
    recursive subroutine internal_complete_buffer(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        type(value_type), allocatable :: res(:), bufs(:)
        integer :: k, n, total

        status = status_type()
        result = value_type()
        table = completion_list_table(buffer_names())

        select case (action)
        case (COMPLETE_TRY)
            call try_completion(string, table, result, status, pred)
        case (COMPLETE_ALL)
            call all_completions(string, table, result, status, pred)
            if (len(string) > 0) return
            ! Strip out internal buffers.
            call list_elements(result, res)
            allocate (bufs(size(res)))
            n = 0

            do k = 1, size(res)
                if (internal_name(res(k))) cycle
                n = n + 1
                bufs(n) = res(k)
            end do

            if (n == 0) then
                ! If all bufs are internal don't strip them out.
                total = int(list_length(buffer_names()))
                if (size(res) /= total) result = value_type()
                return
            end if

            result = make_list(bufs(1:n))
        case (COMPLETE_TEST)
            if (test_completion(string, table, pred)) result = value_type(.true.)
        case default
            continue
        end select

        if (present(suffix)) continue

    contains

        logical function internal_name(v)
            type(value_type), intent(in) :: v
            character(len=:), allocatable :: s

            s = xstring(v)
            internal_name = .false.
            if (len(s) > 0) internal_name = s(1:1) == " "
        end function internal_name
    end subroutine internal_complete_buffer

    !> Perform completion on all buffers excluding BUFFER (`rbts_except'):
    !> internal-complete-buffer through completion-table-with-predicate
    !> (not strict).  Port of
    !> minibuffer.el:internal-complete-buffer-except.
    recursive subroutine internal_complete_buffer_except(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        procedure(completion_predicate), pointer :: saved_pred2

        status = status_type()
        result = value_type()

        select case (action)
        case (COMPLETE_TEST)
            ! Ignore pred1 since it doesn't really have to apply anyway.
            call internal_complete_buffer(string, action, result, status, pred)
        case (COMPLETE_TRY, COMPLETE_ALL)
            saved_pred2 => rbts_pred2
            rbts_pred2 => null()
            if (present(pred)) rbts_pred2 => pred
            call internal_complete_buffer(string, action, result, status, not_except)
            rbts_pred2 => saved_pred2
            ! If completion failed and we're not applying pred1 strictly,
            ! try again without pred1.
            if (.not. allocated(result%a)) call internal_complete_buffer(string, action, result, status, pred)
        case default
            if (present(suffix)) continue
        end select
    end subroutine internal_complete_buffer_except

    !> internal-complete-buffer-except's predicate: the buffer is not
    !> `rbts_except' (and PRED2 accepts it).
    !> Port of minibuffer.el:internal-complete-buffer-except in part.
    recursive logical function not_except(elt)
        type(value_type), intent(in) :: elt

        not_except = .true.

        if (vtag(elt) == V_STRING .and. allocated(rbts_except)) then
            not_except = .not. (xstring(elt) == rbts_except .and. len(xstring(elt)) == len(rbts_except))
        end if

        if (not_except .and. associated(rbts_pred2)) not_except = rbts_pred2(elt)
    end function not_except

    !> Read a string in the minibuffer, with completion.
    !> PROMPT is a string to prompt with; normally it ends in a colon and
    !> a space.  COLLECTION, the completion table, is an array of strings,
    !> a table of (string, value) entries, or a completion function, or
    !> the name table (see `try_completion').  PREDICATE limits
    !> completion to a subset of COLLECTION.
    !>
    !> REQUIRE_MATCH can take the following values:
    !> - REQUIRE_MATCH_YES means that the user is not allowed to exit unless
    !>   the input is (or completes to) an element of COLLECTION or is
    !>   null.
    !> - REQUIRE_MATCH_NO means that the user can exit with any input.
    !> - REQUIRE_MATCH_CONFIRM means that the user can exit with any
    !>   input, but she needs to confirm her choice if the input is not an
    !>   element of COLLECTION.
    !> - REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION means that the user can
    !>   exit with any input, but she needs to confirm her choice if she
    !>   called `minibuffer_complete' right before
    !>   `minibuffer_complete_and_exit' and the input is not an element of
    !>   COLLECTION.
    !> - REQUIRE_MATCH_OTHER means that the user can exit only if the
    !>   input is (or completes to) an element of COLLECTION or is null,
    !>   but typing RET does not exit if it does non-null completion.
    !>
    !> If INITIAL-INPUT is present, insert it in the minibuffer initially.
    !> HISTORY names the history list (default `minibuffer_history').
    !> DEFAULT is the value for empty input, and for M-n.  Port of
    !> minibuf.c:Fcompleting_read with
    !> minibuffer.el:completing-read-default.
    recursive subroutine completing_read(prompt, collection, result, status, predicate, require_match, &
                                         initial, history, default, used_default, defaults)
        character(len=*), intent(in) :: prompt
        type(completion_table_type), intent(in) :: collection
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        integer, intent(in), optional :: require_match
        character(len=*), intent(in), optional :: initial, history, default
        !> A list of defaults instead of DEFAULT: the first is the value
        !> for empty input, all are offered by M-n.
        type(minibuffer_string_type), intent(in), optional :: defaults(:)
        !> Whether the input was empty and the value is DEFAULT itself (as
        !> `eq' tells upstream).
        logical, intent(out), optional :: used_default
        type(completion_table_type) :: saved_table
        procedure(completion_predicate), pointer :: saved_pred
        type(handle_type) :: keymap
        integer :: saved_confirm, match

        if (present(used_default)) used_default = .false.
        match = REQUIRE_MATCH_NO
        if (present(require_match)) match = require_match

        if (match /= REQUIRE_MATCH_NO) then
            keymap = minibuffer_local_must_match_map
            if (minibuffer_completing_file_name) keymap = minibuffer_local_filename_must_match_map
        else
            keymap = minibuffer_local_completion_map
            if (minibuffer_completing_file_name) keymap = minibuffer_local_filename_completion_map
        end if

        ! The minibuffer's local values (minibuffer-with-setup-hook).
        saved_table = minibuffer_completion_table
        saved_pred => minibuffer_completion_predicate
        saved_confirm = minibuffer_completion_confirm
        minibuffer_completion_table = collection
        minibuffer_completion_predicate => null()
        if (present(predicate)) minibuffer_completion_predicate => predicate
        minibuffer_completion_confirm = match
        if (match == REQUIRE_MATCH_YES) minibuffer_completion_confirm = REQUIRE_MATCH_NO
        ! (setq-local completion-ignore-case c-i-c) runs first, then the
        ! functions of enclosing minibuffer-with-setup-hook forms.
        crd_ignore_case = completion_ignore_case%value
        crd_outer_setup => minibuffer_setup_function
        minibuffer_setup_function => completing_read_setup

        if (present(history)) then
            call read_from_minibuffer(prompt, result, status, initial, keymap, history, default, defaults)
        else
            call read_from_minibuffer(prompt, result, status, initial, keymap, default=default, defaults=defaults)
        end if

        minibuffer_setup_function => null()
        minibuffer_completion_table = saved_table
        minibuffer_completion_predicate => saved_pred
        minibuffer_completion_confirm = saved_confirm
        if (.not. status%ok()) return

        if (len(result) == 0 .and. present(default)) then
            result = default
            if (present(used_default)) used_default = .true.
        else if (len(result) == 0 .and. present(defaults)) then
            if (size(defaults) > 0) then
                result = defaults(1)%s
                if (present(used_default)) used_default = .true.
            end if
        end if
    end subroutine completing_read

    !> completing-read-default's minibuffer-with-setup-hook function.
    !> Port of minibuffer.el:completing-read-default in part.
    subroutine completing_read_setup()
        procedure(minibuffer_setup_proc), pointer :: outer

        call completion_ignore_case%set_local(crd_ignore_case)
        outer => crd_outer_setup
        crd_outer_setup => null()
        if (associated(outer)) call outer()
    end subroutine completing_read_setup

    !> Completion table for file names.  Port of
    !> minibuffer.el:completion-file-name-table (no ~USER completion or
    !> file-name handlers).
    recursive subroutine completion_file_name_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        character(len=:), allocatable :: name, specdir, realdir, saved_dir
        type(value_type), allocatable :: all(:), comp(:)
        type(status_type) :: st
        integer :: slash, k, n
        logical :: had_dir

        status = status_type()
        result = value_type()

        select case (action)
        case (COMPLETE_BOUNDARIES)
            slash = 0
            if (present(suffix)) slash = index(suffix, "/")

            if (slash > 0) then
                result = list(value_type(min(schars(file_name_directory(string)), schars(string))), &
                              value_type(schars(suffix(1:slash - 1))))
            else
                result = list(value_type(min(schars(file_name_directory(string)), schars(string))), value_type())
            end if
        case (COMPLETE_TEST)
            if (len(string) == 0) return

            if (present(pred)) then
                if (pred(value_type(text=string))) result = value_type(.true.)
            else
                if (file_exists(string)) result = value_type(.true.)
            end if
        case default
            name = file_name_nondirectory(string)
            specdir = file_name_directory(string)
            realdir = specdir

            if (len(specdir) == 0) then
                realdir = "./"
                if (allocated(buffers(cur)%directory)) realdir = buffers(cur)%directory
            end if

            if (action == COMPLETE_TRY) then
                call file_name_completion(name, realdir, result, st, pred)
                if (.not. st%ok()) result = value_type()
                if (vtag(result) == V_STRING) result = value_type(text=specdir//xstring(result))
            else
                call file_name_all_completions(name, realdir, result, st)
                if (.not. st%ok()) result = value_type()

                ! Check the predicate, if necessary.
                if (present(pred) .and. st%ok()) then
                    call list_elements(result, all)
                    allocate (comp(size(all)))
                    n = 0
                    had_dir = allocated(buffers(cur)%directory)
                    if (had_dir) saved_dir = buffers(cur)%directory
                    call expand_file_name(realdir, buffers(cur)%directory, st)

                    do k = 1, size(all)
                        if (.not. pred(all(k))) cycle
                        n = n + 1
                        comp(n) = all(k)
                    end do

                    if (had_dir) then
                        buffers(cur)%directory = saved_dir
                    else
                        deallocate (buffers(cur)%directory)
                    end if

                    result = make_list(comp(1:n))
                end if
            end if
        end select
    end subroutine completion_file_name_table

    !> `substitute_in_file_name' on `filename', with any part before a
    !> "/~" followed by a word character or `*' or `-' cut off first.
    !> Port of minibuffer.el:completion--sifn-regardless-of-system-users.
    function sifn_regardless_of_system_users(filename) result(name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name
        integer :: k
        character :: c

        name = filename
        k = 1

        do while (k + 2 <= len(name))
            if (name(k:k + 1) == "/~") then
                c = name(k + 2:k + 2)

                if ((c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") .or. (c >= "0" .and. c <= "9") &
                    .or. c == "_" .or. c == "*" .or. c == "-" .or. iachar(c) >= 128) then
                    name = name(k + 1:)
                    k = 1
                    cycle
                end if
            end if

            k = k + 1
        end do

        name = substitute_in_file_name(name)
    end function sifn_regardless_of_system_users

    !> Return completion boundaries on file name STRING: the start in
    !> STRING (`string_start'), and the end in SUFFIX (`bend', -1 for
    !> none).  Runs `substitute_in_file_name' on STRING first, but returns
    !> completion boundaries for the original string.  Port of
    !> minibuffer.el:completion--sifn-boundaries.
    subroutine sifn_boundaries(string, suffix, string_start, bend, pred)
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(out) :: string_start, bend
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: sifned
        type(value_type) :: bounds
        type(status_type) :: st

        ! We want to compute the start boundary on the result of
        ! `substitute_in_file_name' (since that's what we use for actual
        ! completion), and then transform that into an offset in STRING
        ! instead.  We can't do this if we expand environment variables,
        ! so double the $s to prevent that.
        sifned = substitute_in_file_name(double_dollars(string))
        call completion_file_name_table(sifned, COMPLETE_BOUNDARIES, bounds, st, pred, suffix)
        ! Adjust SIFNED-START to be an offset in STRING instead of in
        ! SIFNED.
        string_start = xfixnum(list_elt(bounds, 1_int64)) - schars(sifned) + schars(string)
        bend = -1
        if (vtag(list_elt(bounds, 2_int64)) == V_INTEGER) bend = xfixnum(list_elt(bounds, 2_int64))
    end subroutine sifn_boundaries

    !> Internal subroutine for `read_file_name'.  Do not call this.
    !> This is a completion table for file names, like
    !> `completion_file_name_table' except that it passes the file name
    !> through `substitute_in_file_name'.  Port of
    !> minibuffer.el:completion--file-name-table (read-file-name-internal;
    !> completion--embedded-envvar-table, completing environment variable
    !> names, is not ported).
    recursive subroutine read_file_name_internal(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        character(len=:), allocatable :: sifned, orig_in_bounds, sifned_in_bounds, compl
        type(value_type), allocatable :: elts(:)
        type(value_type) :: bounds
        integer(int64) :: orig_start, sifned_start, bend
        logical :: only_need_double_dollars
        integer :: k

        status = status_type()
        result = value_type()

        if (action == COMPLETE_BOUNDARIES) then
            if (present(suffix)) then
                call sifn_boundaries(string, suffix, orig_start, bend, pred)
            else
                call sifn_boundaries(string, "", orig_start, bend, pred)
            end if

            if (bend >= 0) then
                result = list(value_type(orig_start), value_type(bend))
            else
                result = list(value_type(orig_start), value_type())
            end if

            return
        end if

        if (action == COMPLETE_TEST) then
            sifned = substitute_in_file_name(string)
        else
            sifned = sifn_regardless_of_system_users(string)
        end if

        call sifn_boundaries(string, "", orig_start, bend, pred)
        call completion_file_name_table(sifned, COMPLETE_BOUNDARIES, bounds, status, pred, "")
        sifned_start = xfixnum(list_elt(bounds, 1_int64))
        orig_in_bounds = substring(string, orig_start)
        sifned_in_bounds = substring(sifned, sifned_start)
        ! If true, sifn only un-doubled $s in ORIG, so we can fix a
        ! completion to match ORIG by just doubling $s again.  This
        ! preserves more text from the completion, behaving better with
        ! `completion_ignore_case' .true..
        only_need_double_dollars = orig_in_bounds == double_dollars(sifned_in_bounds) .and. &
                                   len(orig_in_bounds) == len(double_dollars(sifned_in_bounds))
        call completion_file_name_table(sifned, action, result, status, pred)

        select case (action)
        case (COMPLETE_TRY)
            if (vtag(result) == V_STRING) then
                if (only_need_double_dollars) then
                    ! Extract the newly added text, quote any dollar signs,
                    ! and append it to ORIG.
                    result = value_type(text=substring(string, 0_int64, orig_start)// &
                                        double_dollars(substring(xstring(result), sifned_start)))
                else
                    result = value_type(text=string//double_dollars(substring(xstring(result), schars(sifned))))
                end if
            end if
        case (COMPLETE_ALL)
            call list_elements(result, elts)

            do k = 1, size(elts)
                if (only_need_double_dollars) then
                    elts(k) = value_type(text=double_dollars(xstring(elts(k))))
                else
                    ! Extract the newly added text, quote any dollar signs,
                    ! and append it to the part of ORIG inside the
                    ! completion boundaries.
                    compl = xstring(elts(k))
                    elts(k) = value_type(text=orig_in_bounds//double_dollars(substring(compl, schars(sifned_in_bounds))))
                end if
            end do

            result = make_list(elts)
        case default
            continue
        end select
    end subroutine read_file_name_internal

    !> Complete with TABLE the STRING that follows PREFIX: the completion
    !> table of `completing_read_multiple' and the like (for ACTION, as a
    !> `completion_function').  Port of
    !> minibuffer.el:completion-table-with-context.
    recursive subroutine completion_table_with_context(prefix, table, string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: prefix, string
        type(completion_table_type), intent(in) :: table
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        character(len=:), allocatable :: saved_prefix
        procedure(completion_predicate), pointer :: saved_pred
        integer(int64) :: bstart, bend

        status = status_type()
        ! (The predicate sees PREFIX and the candidate.)
        saved_prefix = context_prefix
        saved_pred => context_pred
        context_prefix = prefix
        context_pred => null()
        if (present(pred)) context_pred => pred

        select case (action)
        case (COMPLETE_BOUNDARIES)
            if (present(pred)) then
                call completion_boundaries(string, table, suffix_of(), bstart, bend, context_predicate)
            else
                call completion_boundaries(string, table, suffix_of(), bstart, bend)
            end if

            result = list(value_type(bstart + schars(prefix)), value_type(bend))
        case (COMPLETE_TRY)
            if (present(pred)) then
                call try_completion(string, table, result, status, context_predicate)
            else
                call try_completion(string, table, result, status)
            end if

            ! In case of try-completion, add the prefix.
            if (status%ok() .and. vtag(result) == V_STRING) result = value_type(text=prefix//xstring(result))
        case (COMPLETE_ALL)
            if (present(pred)) then
                call all_completions(string, table, result, status, context_predicate)
            else
                call all_completions(string, table, result, status)
            end if
        case default
            result = value_type()

            if (present(pred)) then
                if (test_completion(string, table, context_predicate)) result = value_type(.true.)
            else
                if (test_completion(string, table)) result = value_type(.true.)
            end if
        end select

        context_prefix = saved_prefix
        context_pred => saved_pred

    contains

        function suffix_of() result(s)
            character(len=:), allocatable :: s

            s = ""
            if (present(suffix)) s = suffix
        end function suffix_of
    end subroutine completion_table_with_context

    ! The predicate of `completion_table_with_context': its PRED called
    ! with the prefix and the candidate.  Not a port.
    logical function context_predicate(elt)
        type(value_type), intent(in) :: elt

        context_predicate = .true.
        if (.not. associated(context_pred)) return

        if (vtag(elt) == V_STRING) then
            context_predicate = context_pred(value_type(text=context_prefix//xstring(elt)))
        else
            context_predicate = context_pred(elt)
        end if
    end function context_predicate
end module minibuffer_fm
