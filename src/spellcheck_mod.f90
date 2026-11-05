! spellcheck_mod.f90 -- interface to spell checkers (Ispell, Aspell, Hunspell).
! Ported from GNU Emacs 31.1 lisp/textmodes/ispell.el (in part).
!
! Copyright (C) 1994-1995, 1997-2026 Free Software Foundation, Inc.
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

!> Spelling over an external speller process (Ispell, Aspell, Hunspell
!> or Enchant run with `-a', the Ispell pipe protocol): M-$
!> (`spellcheck_word') checks the word at point, `spellcheck_region',
!> `spellcheck_buffer', `spellcheck' and `spellcheck_comments_and_strings' check
!> text, and a misspelled word shows its corrections in the *Choices*
!> buffer above the selected window, with single keys to choose (see
!> `spellcheck_help').  The dictionaries are records of
!> `spellcheck_dictionary_type' in `spellcheck_dictionaries' (built from
!> `spellcheck_base_dictionaries' and, for Aspell, the dictionaries it
!> reports) and `spellcheck_local_dictionaries' (the configuration's own).
!>
!> Deferred: Hunspell's and Enchant's dictionary discovery
!> (ispell-find-hunspell-dictionaries, ispell-parse-hunspell-affix-file,
!> ispell-find-enchant-dictionaries: their dictionaries are the base
!> ones), `ispell-base-dicts-override-alist', the synchronous mode for
!> systems without subprocesses, the debug buffer
!> (`ispell-buffer-with-debug'), `ispell-complete-word',
!> `ispell-completion-at-point', `ispell-minor-mode', `ispell-message'
!> and the mail parts of region checking (`ispell-checking-message',
!> `ispell-message-dictionary-alist', `ispell-message-fcc-skip'), the menu,
!> framepop, highlighting on a terminal without colors (upstream's
!> inverse-video kludge) and lazy highlighting of the other occurrences
!> (isearch's lazy highlighting is not ported), the *Choices* buffer's
!> mode line (no `mode_line_format'), electric help
!> (`spellcheck_help_in_buffer' electric, ehelp.el), horizontal scrolling
!> (`truncate_lines' is not ported).
module spellcheck_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, vtag, V_INTEGER, V_STRING, xfixnum, make_list, command_type, xstring, &
                         list_elements
    use data_mod, only: string_setting_type, logical_setting_type, integer_setting_type, forward_setting
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          erase_buffer, get_buffer, generate_new_buffer_name, is_buffer_live, fill_column, &
                          transient_mark_mode, set_buffer_modified, restore_buffer_modified, resolve_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_character, point, point_minimum, point_maximum, line_end_position, is_end_of_line, &
                                     is_beginning_of_buffer, delete_region, buffer_substring_no_properties, buffer_string, &
                                     character_after, region_beginning, region_end, character_to_string
    use editing_commands_mod, only: forward_line, forward_character, backward_character, end_of_line, beginning_of_line
    use indent_mod, only: current_column
    use marker_mod, only: make_marker, set_marker, marker_position, marker_buffer, destroy_marker
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, search_backward, &
                          looking_at, string_match, match_beginning, match_end, regular_expression_quote, case_fold_search, &
                          SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string_no_properties, read_y_or_n, read_key, version_type, version, operator(<=), &
                                     operator(>)
    use casefiddle_mod, only: upcase_string, downcase_string
    use regular_expression_optimization_mod, only: regular_expression_optimization, regular_expression_unmatchable
    use file_input_output_mod, only: expand_file_name, file_name_nondirectory, is_file_readable, is_file_accessible_directory, &
                                     is_file_name_absolute, insert_file_contents
    use files_mod, only: executable_find, temporary_file_directory
    use directory_editor_mod, only: directory_files
    use environment_mod, only: getenv
    use execute_process_mod, only: execute_process, execute_process_region
    use coding_mod, only: decode_coding_string, encode_coding_string
    use string_type_mod, only: string_type, append_string, prepend_string
    use process_mod, only: make_process, delete_process, is_process_live, process_status, &
                           set_process_filter, set_process_coding_system, set_process_query_on_exit_flag, &
                           accept_process_output, process_send_string, PROCESS_CONNECTION_PIPE, &
                           PROCESS_CONNECTION_PTY
    use syntax_mod, only: parse_state_type, parse_partial_expression, syntax_parser_state, COMMENTSTOP_SYNTAX_TABLE
    use expression_motion_mod, only: forward_expression
    use newcomment_mod, only: comment_start, comment_end, comment_normalize_variables
    use simple_mod, only: is_use_region, is_region_active, push_mark, newline, open_line
    use undo_mod, only: undo_boundary
    use abbreviation_mod, only: global_abbreviation_table
    use replace_mod, only: query_replace
    use minibuffer_reading_mod, only: read_string
    use minibuffer_reading_tables_mod, only: completion_table_type, TABLE_LIST
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use keyboard_mod, only: recursive_edit, sit_for, help_character, unread_command_events
    use keymap_mod, only: key_binding
    use execute_interactively_mod, only: current_prefix_argument, PREFIX_NONE, command_of
    use frame_mod, only: selected_frame, selected_window
    use window_mod, only: window_configuration_type, current_window_configuration, set_window_configuration, &
                          split_window, select_window, set_window_buffer, fit_window_to_buffer, get_buffer_window, &
                          display_buffer, window_body_width, window_resize, windows
    use interval_tree_mod, only: make_overlay, overlay_put, delete_overlay, move_overlay, is_overlay
    use faces_mod, only: named_face_type
    use incremental_search_mod, only: incremental_search
    use terminal_output_mod, only: is_terminal_display_color
    use display_update_mod, only: ding, redraw_display
    use help_mod, only: substitute_command_keys
    use display_engine_mod, only: message, message_log_maximum, message_log_maximum_choice_setting_type
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: spellcheck_word, spellcheck_region, spellcheck_buffer, spellcheck, spellcheck_continue, &
              spellcheck_comments_and_strings, &
              spellcheck_comment_or_string_at_point, spellcheck_change_dictionary, spellcheck_kill_spellcheck, &
              spellcheck_pdict_save, &
              spellcheck_check_version, spellcheck_help, spellcheck_lookup_words, spellcheck_set_spellchecker_parameters, &
              spellcheck_valid_dictionary_list, spellcheck_initialize_process, spellcheck_send_string, spellcheck_accept_output, &
              spellcheck_filter, spellcheck_parse_output, spellcheck_get_word, is_spellcheck_correct, spellcheck_process_status, &
              spellcheck_add_per_file_word_list, spellcheck_buffer_local_words, spellcheck_buffer_local_dictionary, &
              spellcheck_buffer_local_parsing, spellcheck_accept_buffer_local_definitions, spellcheck_internal_change_dictionary, &
              spellcheck_get_casechars, spellcheck_get_not_casechars, spellcheck_get_otherchars, spellcheck_get_many_otherchars, &
              spellcheck_get_coding_system, spellcheck_get_extended_character_mode, &
              prepend_spellcheck_buffer_session_localword, spellcheck_format_word_procedure, spellcheck_skip_procedure, &
              spellcheck_tex_argument_end, initialize_spellcheck, spellcheck_get_spellcheck_arguments, &
              spellcheck_buffer_session_localwords, &
              spellcheck_execute_process_region

    !> A dictionary and its parameters, an element of `spellcheck_dictionaries'
    !> and `spellcheck_local_dictionaries' (upstream's (DICTIONARY-NAME
    !> CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P ISPELL-ARGS
    !> EXTENDED-CHARACTER-MODE CHARACTER-SET)):
    !>
    !> `name' is a possible value of `spellcheck_dictionary'; "" means the
    !> default dictionary.
    !>
    !> `casechars' is a regular expression of valid characters that comprise
    !> a word.
    !>
    !> `not_casechars' is the opposite regexp of `casechars'.
    !>
    !> `otherchars' is a regexp of characters in the `not_casechars' set but
    !> which can be used to construct words in some special way.  If
    !> `otherchars' characters follow and precede characters from
    !> `casechars', they are parsed as part of a word, otherwise they
    !> become word-breaks.  As an example in English, assume the regular
    !> expression "[']" for `otherchars'.  Then "they're" and "Steven's"
    !> are parsed as single words including the "'" character, but
    !> "Stevens'" does not include the quote character as part of the
    !> word.  If you want `otherchars' to be empty, use the empty string.
    !> Hint: regexp syntax requires the hyphen to be declared first here.
    !>
    !> `casechars', `not_casechars', and `otherchars' are bytes of
    !> `coding_system'.  In addition, if they contain non-ASCII bytes, the
    !> regular expression must be a single `character set' construct that
    !> doesn't specify a character range for non-ASCII bytes.
    !>
    !> `many_otherchars' is true when multiple `otherchars' are allowed in a
    !> word.  Otherwise only a single `otherchars' character is allowed to
    !> be part of any single word.
    !>
    !> `arguments' are additional arguments passed to the ispell subprocess (""
    !> for none).
    !>
    !> `extended_character_mode' should be used when dictionaries are used
    !> which have been configured in an Ispell affix file.  (For example,
    !> umlauts can be encoded as \"a, a\", "a, ...)  Defaults are ~tex and
    !> ~nroff in English.  This has the same effect as the command-line
    !> `-T' option.  The buffer major mode controls Ispell's parsing in tex
    !> or nroff mode, but the dictionary can control the extended
    !> character mode.  Both defaults can be overruled in a buffer-local
    !> fashion.  See `spellcheck_parsing_keyword' for details on this.
    !>
    !> `coding_system' is used to encode text sent to the ispell subprocess
    !> when the language uses non-ASCII characters.
    type, public :: spellcheck_dictionary_type
        character(len=64) :: name = ""
        character(len=256) :: casechars = ""
        character(len=256) :: not_casechars = ""
        character(len=64) :: otherchars = ""
        logical :: many_otherchars = .false.
        character(len=64) :: arguments(4) = ""
        character(len=16) :: extended_character_mode = ""
        character(len=16) :: coding_system = ""
    end type spellcheck_dictionary_type

    abstract interface
        !> `spellcheck_format_word_function': WORD as shown in messages.
        !> Not a port.
        subroutine spellcheck_format_word_procedure(word, formatted)
            implicit none(type, external)
            character(len=*), intent(in) :: word
            character(len=:), allocatable, intent(out) :: formatted
        end subroutine spellcheck_format_word_procedure

        !> The function of a region to skip: moves point to the end of the
        !> region, given ARGUMENT (upstream's ARGS).  Not a port.
        recursive subroutine spellcheck_skip_procedure(argument, status)
            import :: status_type
            implicit none(type, external)
            integer, intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine spellcheck_skip_procedure
    end interface

    !> A region not to spell check, an element of `spellcheck_skip_regions'
    !> (upstream's (KEY), (KEY . REGULAR_EXPRESSION), (KEY REGULAR_EXPRESSION) and (KEY FUNCTION
    !> ARGUMENTS)): `beginning_regular_expression' is the key; the region ends after the
    !> next match of `end_regular_expression' (searched up to the end of the buffer),
    !> or where `skip' leaves point when called with `argument'; with neither,
    !> just the key is skipped.
    type, public :: spellcheck_skip_region_type
        character(len=256) :: beginning_regular_expression = ""
        character(len=256) :: end_regular_expression = ""
        procedure(spellcheck_skip_procedure), pointer, nopass :: skip => null()
        integer :: argument = 1
    end type spellcheck_skip_region_type

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    !> The `spellcheck_highlight' value besides no highlighting (.false.) and
    !> highlighting (.true.): highlighting assuming a block cursor on terminals.
    integer, parameter, public :: SPELLCHECK_HIGHLIGHT_BLOCK = 2

    !> Highlight spelling errors when true.
    !> When set to SPELLCHECK_HIGHLIGHT_BLOCK, assumes a block cursor with TERMINAL
    !> displays.  Port of ispell.el ispell-highlight-p.
    type(choice_setting_type), public :: spellcheck_highlight = &
                                         choice_setting_type(name="spellcheck_highlight", choice=SPELLCHECK_HIGHLIGHT_BLOCK)

    !> Face used for Ispell highlighting.
    !> This variable can be set by the user to whatever face they desire.
    !> It's most convenient if the cursor color and highlight color are
    !> slightly different.  Port of ispell.el ispell-highlight-face.
    type(named_face_type), pointer, public :: spellcheck_highlight_face => incremental_search

    !> The `spellcheck_check_comments' value besides comments not checked
    !> (.false.) and checked (.true.): the only text checked.
    integer, parameter, public :: SPELLCHECK_CHECK_COMMENTS_EXCLUSIVE = 2

    !> Spelling of comments checked when true.
    !> When set to SPELLCHECK_CHECK_COMMENTS_EXCLUSIVE, ONLY comments are checked.
    !> (For code comments).  Warning!  Not checking comments, when a
    !> comment start is embedded in strings, may produce undesired results.
    !> Port of ispell.el ispell-check-comments.
    type(choice_setting_type), public :: spellcheck_check_comments = &
                                         choice_setting_type(name="spellcheck_check_comments", value=.true.)

    !> Corrections made throughout region when true.
    !> Uses `query_replace' (M-%) for corrections.  Port of ispell.el
    !> ispell-query-replace-choices.
    logical, public :: spellcheck_query_replace_choices = .false.

    !> Whether to save spelling corrections as abbrevs by default.
    !> Determines the default behavior of Ispell after correcting a
    !> misspelled word.  True means to save a global abbrev that expands
    !> the misspelled word to its correction.  This behavior may be toggled
    !> on a per-word basis by typing C-u immediately before selecting a
    !> replacement in the Ispell command loop.  Port of ispell.el
    !> ispell-save-corrections-as-abbrevs.
    logical, public :: spellcheck_save_corrections_as_abbreviations = .false.

    !> Does not spell check `tib' bibliography references when true.
    !> Skips any text between strings matching regular expressions
    !> `spellcheck_tib_reference_beginning' and `spellcheck_tib_reference_end'.
    !>
    !> TeX users beware:  Any text between [. and .] will be skipped -- even
    !> if that's your whole buffer -- unless you set `spellcheck_skip_tib' to
    !> .false.  That includes the [.5mm] type of number...  Port of
    !> ispell.el ispell-skip-tib.
    logical, public :: spellcheck_skip_tib = .false.

    !> Regexp matching the beginning of a Tib reference.  Port of ispell.el
    !> ispell-tib-ref-beginning.
    character(len=:), allocatable, public :: spellcheck_tib_reference_beginning

    !> Regexp matching the end of a Tib reference.  Port of ispell.el
    !> ispell-tib-ref-end.
    character(len=:), allocatable, public :: spellcheck_tib_reference_end

    !> If true, keep the `*Choices*' window for the entire spelling session.
    !> This minimizes redisplay thrashing.  Port of ispell.el
    !> ispell-keep-choices-win.
    logical, public :: spellcheck_keep_choices_window = .true.

    !> The default size of the `*Choices*' window, including the mode line.
    !> Must be greater than 1.  Port of ispell.el
    !> ispell-choices-win-default-height.
    integer, public :: spellcheck_choices_window_default_height = 2

    !> Program invoked by M-$ and M-x spellcheck_region commands.  Port of
    !> ispell.el ispell-program-name (the first of aspell, ispell,
    !> hunspell and enchant-2 found in `exec_path', else "ispell").
    character(len=:), allocatable, public :: spellcheck_program_name

    !> Alternate plain word-list dictionary for spelling help.
    !> This is also used by `spellcheck_lookup_words'.  Port of ispell.el
    !> ispell-alternate-dictionary ("" for none).
    character(len=:), allocatable, public :: spellcheck_alternate_dictionary

    !> Plain word-list dictionary used for word completion.
    !> This word-list is used if it is different from
    !> `spellcheck_alternate_dictionary'.
    !> This is also used by `spellcheck_lookup_words'.  Port of ispell.el
    !> ispell-complete-word-dict ("" for none).
    character(len=:), allocatable, public :: spellcheck_complete_word_dictionary

    !> Name of the grep command for search processes.  Port of ispell.el
    !> ispell-grep-command.
    character(len=:), allocatable, public :: spellcheck_grep_command

    !> String of options to use when running the program in
    !> `spellcheck_grep_command'.  Should probably be "-Ei".  Port of ispell.el
    !> ispell-grep-options.
    character(len=:), allocatable, public :: spellcheck_grep_options

    !> Name of the look command for search processes.
    !> This must be an absolute file name.  Port of ispell.el
    !> ispell-look-command ("" for none).
    character(len=:), allocatable, public :: spellcheck_look_command

    !> String of command options for `spellcheck_look_command'.  Port of
    !> ispell.el ispell-look-options.
    character(len=:), allocatable, public :: spellcheck_look_options

    !> When true, Fortmacs uses ptys to communicate with Ispell.
    !> When false, Fortmacs uses pipes.  Port of ispell.el ispell-use-ptys-p.
    logical, public :: spellcheck_use_ptys = .false.

    !> True means `spellcheck_word' checks the word around or after point.
    !> Otherwise `spellcheck_word' checks the preceding word.  Port of ispell.el
    !> ispell-following-word.
    logical, public :: spellcheck_following_word = .false.

    !> True means display interactive keymap help in a buffer: pop up a
    !> new buffer and display a short help message there for a couple of
    !> seconds.  If false, expand the minibuffer and display the message
    !> there for a couple of seconds.  Port of ispell.el
    !> ispell-help-in-bufferp (its value `electric', a help buffer to
    !> browse and exit, is not ported: Deferred).
    logical, public :: spellcheck_help_in_buffer = .false.

    !> True means suppress messages in `spellcheck_word'.  Port of ispell.el
    !> ispell-quietly.
    logical, public :: spellcheck_quietly = .false.

    !> Formatting function for displaying word being spell checked.
    !> The function must take one string argument and return a string.
    !> Port of ispell.el ispell-format-word-function (`upcase').
    procedure(spellcheck_format_word_procedure), pointer, public :: spellcheck_format_word_function => upcase_word

    !> File name of your personal spelling dictionary, or "".
    !> If "", the default personal dictionary for your spelling checker is
    !> used.  Due to a misfeature of Hunspell, if the value is an absolute
    !> file name, the file by that name must already exist for Hunspell to
    !> be able to use it.  Port of ispell.el ispell-personal-dictionary.
    character(len=:), allocatable, public :: spellcheck_personal_dictionary

    !> When true, save personal dictionary without asking for confirmation.
    !> Port of ispell.el ispell-silently-savep.
    logical, public :: spellcheck_silently_save = .false.

    !> True means the user has explicitly set this buffer's Ispell
    !> dictionary.  Port of ispell.el ispell-local-dictionary-overridden.
    type(logical_setting_type), target, public :: spellcheck_local_dictionary_overridden = &
                                                  logical_setting_type(name="spellcheck_local_dictionary_overridden")

    !> If set, the dictionary to be used for Ispell commands in this buffer.
    !> The value must be a string dictionary name, or unset, which means
    !> use the global setting in `spellcheck_dictionary'.  Dictionary names are
    !> defined in `spellcheck_local_dictionaries' and `spellcheck_dictionaries'.
    !>
    !> Setting `spellcheck_local_dictionary' to a value has the same effect as
    !> calling M-x spellcheck_change_dictionary with that value.  This variable
    !> is automatically set when defined in the file with
    !> `spellcheck_dictionary_keyword'.  Port of ispell.el
    !> ispell-local-dictionary.
    type(string_setting_type), target, public :: spellcheck_local_dictionary = &
                                                 string_setting_type(name="spellcheck_local_dictionary")

    !> Default dictionary to use if `spellcheck_local_dictionary' is unset ("" for
    !> the default).  Port of ispell.el ispell-dictionary.
    character(len=:), allocatable, public :: spellcheck_dictionary

    !> Extra switches to pass to the Ispell program.
    !> For example, ["-W", "3"] to cause it to accept all 1-3 character
    !> words as correct.  See also `spellcheck_dictionaries', which may be used
    !> for language-specific arguments.  Port of ispell.el ispell-extra-args.
    character(len=64), allocatable, public :: spellcheck_extra_arguments(:)

    !> The `spellcheck_skip_html' value besides .true. and .false.
    integer, parameter, public :: SPELLCHECK_SKIP_HTML_USE_MODE_NAME = 2

    !> Indicates whether ispell should skip spell checking of SGML markup.
    !> If .true., always skip SGML markup; if
    !> .false., never skip; if SPELLCHECK_SKIP_HTML_USE_MODE_NAME,
    !> guess whether SGML markup should be skipped according to the name of
    !> the buffer's major mode.
    !>
    !> SGML markup is any text inside the brackets "<>" or entities such as
    !> "&amp;".  See `spellcheck_html_skip_regions' for more details.
    !>
    !> This variable affects spell-checking of HTML, XML, and SGML files.
    !> Port of ispell.el ispell-skip-html.
    type(choice_setting_type), target, public :: spellcheck_skip_html = &
                                                 choice_setting_type(name="spellcheck_skip_html", &
                                                                     choice=SPELLCHECK_SKIP_HTML_USE_MODE_NAME)

    !> List of local or customized dictionary definitions.
    !> These can override the values in `spellcheck_dictionaries'.  Port of
    !> ispell.el ispell-local-dictionary-alist.
    type(spellcheck_dictionary_type), allocatable, public :: spellcheck_local_dictionaries(:)

    !> The number of seconds to display the help text.  Port of ispell.el
    !> ispell-help-timeout.
    real, public :: spellcheck_help_timeout = 30.0

    !> Base value for `spellcheck_dictionaries'.
    !>
    !> Note that when the speller program is "aspell" or "hunspell", some
    !> parts of the database, notably `otherchars', will be overridden by
    !> parsing the dictionary data files.  Port of ispell.el
    !> ispell-dictionary-base-alist.
    type(spellcheck_dictionary_type), allocatable, public :: spellcheck_base_dictionaries(:)

    !> The dictionaries and their associated parameters (see
    !> `spellcheck_dictionary_type').
    !>
    !> Note that with "ispell" as the speller, the `casechars' and
    !> `otherchars' slots should contain the same character set as
    !> casechars and otherchars in the LANGUAGE.aff file (e.g.,
    !> english.aff).  Aspell and Hunspell don't have this limitation.  Port
    !> of ispell.el ispell-dictionary-alist.
    type(spellcheck_dictionary_type), allocatable, public :: spellcheck_dictionaries(:)

    !> The parsed Aspell dictionaries.  Internal use.  Port of ispell.el
    !> ispell-aspell-dictionary-alist.
    type(spellcheck_dictionary_type), allocatable, public :: spellcheck_aspell_dictionaries(:)

    !> The identified aspell dictionaries.  Port of ispell.el
    !> ispell--aspell-found-dictionaries.
    type(spellcheck_dictionary_type), allocatable, public :: spellcheck_aspell_found_dictionaries(:)

    !> A standard dictionary name and its locale, an element of
    !> `spellcheck_dictionaries_name2locale_equivs'.
    type, public :: spellcheck_locale_equiv_type
        character(len=16) :: name = ""
        character(len=8) :: locale = ""
    end type spellcheck_locale_equiv_type

    !> Known matching locales for standard dict names.
    !> Standard dict names are defined in `spellcheck_base_dictionaries'.  Port
    !> of ispell.el ispell-dicts-name2locale-equivs-alist.
    type(spellcheck_locale_equiv_type), allocatable, public :: spellcheck_dictionaries_name2locale_equivs(:)

    !> The version of Aspell if we can use Aspell extensions, else "".  Port
    !> of ispell.el ispell-really-aspell.
    character(len=:), allocatable, public :: spellcheck_really_aspell
    !> The version of Hunspell if we can use Hunspell extensions, else "".
    !> Port of ispell.el ispell-really-hunspell.
    character(len=:), allocatable, public :: spellcheck_really_hunspell
    !> The version of Enchant if we can use Enchant extensions, else "".
    !> Port of ispell.el ispell-really-enchant.
    character(len=:), allocatable, public :: spellcheck_really_enchant

    !> Command line option prefix to select encoding if supported, ""
    !> otherwise.  If setting the encoding is supported by spellchecker and
    !> is selectable from the command line, this variable will contain
    !> "--encoding=" for Aspell and "-i" for Hunspell, so the appropriate
    !> mime charset can be selected.  That will be set in
    !> `spellcheck_check_version' for Hunspell >= 1.1.6 and Aspell >= 0.60.
    !>
    !> For Aspell, non-empty also means to try to automatically find its
    !> dictionaries.  Port of ispell.el ispell-encoding8-command.
    character(len=:), allocatable, public :: spellcheck_encoding8_command

    !> Directory where ispell dictionaries reside ("" if unknown).  Port of
    !> ispell.el ispell-library-directory.
    character(len=:), allocatable, public :: spellcheck_library_directory

    !> Data directory of Aspell.  Port of ispell.el ispell-aspell-data-dir.
    character(len=:), allocatable, public :: spellcheck_aspell_data_directory

    !> Dictionary directory of Aspell.  Port of ispell.el
    !> ispell-aspell-dict-dir.
    character(len=:), allocatable, public :: spellcheck_aspell_dictionary_directory

    !> Normal hook run on spellchecker initialization.
    !> This hook is run when a spellchecker is used for the first time,
    !> before `spellcheck_dictionaries' is set.  Port of ispell.el
    !> ispell-initialize-spellchecker-hook.
    type(hook_type), target, public :: spellcheck_initialize_spellchecker_hook = &
                                       hook_type(name="spellcheck_initialize_spellchecker_hook")

    !> Hook run by `spellcheck_kill_spellcheck' (flyspell flushes its caches
    !> there).  Port of ispell.el ispell-kill-ispell-hook.
    type(hook_type), target, public :: spellcheck_kill_spellcheck_hook_variable = hook_type( &
                                       name="spellcheck_kill_spellcheck_hook_variable")

    !> Hook run after changing dictionary.  Port of ispell.el
    !> ispell-change-dictionary-hook.
    type(hook_type), target, public :: spellcheck_change_dictionary_hook = hook_type(name="spellcheck_change_dictionary_hook")

    !> A normal hook invoked from the ispell command loop.
    !> It is called once per iteration, before displaying a prompt to the
    !> user.  Port of ispell.el ispell-update-post-hook.
    type(hook_type), target, public :: spellcheck_update_post_hook = hook_type(name="spellcheck_update_post_hook")

    !> True means personal dictionary has modifications to be saved.  Port
    !> of ispell.el ispell-pdict-modified-p.
    logical, public :: spellcheck_pdict_modified = .false.

    !> If true, `spellcheck_word' does not try to correct the word.  Port of
    !> ispell.el ispell-check-only.
    logical, public :: spellcheck_check_only = .false.

    !> The name of the current dictionary, or "" for the default.
    !> This is passed to the Ispell process using the `-d' switch and is
    !> used as key in `spellcheck_local_dictionaries' and
    !> `spellcheck_dictionaries'.  Port of ispell.el ispell-current-dictionary.
    character(len=:), allocatable, public :: spellcheck_current_dictionary

    !> The name of the current personal dictionary, or "" for the default.
    !> This is passed to the Ispell process using the `-p' switch.  Port of
    !> ispell.el ispell-current-personal-dictionary.
    character(len=:), allocatable, public :: spellcheck_current_personal_dictionary

    !> The buffer used for the choices.  Port of ispell.el
    !> ispell-choices-buffer.
    character(len=*), parameter, public :: spellcheck_choices_buffer = "*Choices*"

    !> The keyword for local oddly-spelled words to accept.
    !> The keyword will be followed by any number of local word spellings.
    !> There can be multiple instances of this keyword in the file.  Port
    !> of ispell.el ispell-words-keyword.
    character(len=*), parameter, public :: spellcheck_words_keyword = "LocalWords: "

    !> The keyword for a local dictionary to use.
    !> The keyword must be followed by a valid dictionary name, defined in
    !> `spellcheck_local_dictionaries' or `spellcheck_dictionaries'.  When multiple
    !> occurrences exist, the last keyword definition is used.  Port of
    !> ispell.el ispell-dictionary-keyword.
    character(len=*), parameter, public :: spellcheck_dictionary_keyword = "Local IspellDict: "

    !> The keyword for defining buffer local dictionaries.
    !> Keyword must be followed by the filename of a personal dictionary.
    !> The last occurring definition in the buffer will be used.  Port of
    !> ispell.el ispell-pdict-keyword.
    character(len=*), parameter, public :: spellcheck_pdict_keyword = "Local IspellPersDict: "

    !> The keyword for overriding default Ispell parsing.
    !> The above keyword string should be followed by `latex_mode' or
    !> `nroff_mode' to put the current buffer into the desired parsing mode.
    !>
    !> Extended character mode can be changed for this buffer by placing a
    !> `~' followed by an extended-character mode -- such as `~.tex'.  The
    !> last occurring definition in the buffer will be used.  Port of
    !> ispell.el ispell-parsing-keyword.
    character(len=*), parameter, public :: spellcheck_parsing_keyword = "Local IspellParsing: "

    !> Beginning and end of regions not to spell check (see
    !> `spellcheck_skip_region_type').  Port of ispell.el
    !> ispell-skip-region-alist.
    type(spellcheck_skip_region_type), allocatable, public :: spellcheck_skip_regions(:)

    !> Regions to be skipped in TeX mode, used raw.  Delete or add any
    !> regions you want to be automatically selected for skipping in latex
    !> mode.  Port of ispell.el ispell-tex-skip-alists (its first list).
    type(spellcheck_skip_region_type), allocatable, public :: spellcheck_tex_skip_regions(:)

    !> Regions to be skipped in TeX mode, with the key placed inside
    !> \begin{}.  Port of ispell.el ispell-tex-skip-alists (its second
    !> list).
    type(spellcheck_skip_region_type), allocatable, public :: spellcheck_tex_skip_environments(:)

    !> Start and end keys to skip in HTML buffers.
    !> Same format as `spellcheck_skip_regions'.  Note - substrings of other
    !> matches must come last (e.g. "<[tT][tT]/" and "<[^ \t\n>]").  Port
    !> of ispell.el ispell-html-skip-alists.
    type(spellcheck_skip_region_type), allocatable, public :: spellcheck_html_skip_regions(:)

    !> A buffer local variable containing the current personal dictionary.
    !> If set, the value must be a string, which is a file name.
    !>
    !> If you specify a personal dictionary for the current buffer which is
    !> different from the current personal dictionary, the effect is
    !> similar to calling M-x spellcheck_change_dictionary.  This variable is
    !> automatically set when defined in the file with
    !> `spellcheck_pdict_keyword'.
    !>
    !> Due to a misfeature of Hunspell, if the value is an absolute file
    !> name, the file by that name must already exist for Hunspell to be
    !> able to use it.  Port of ispell.el ispell-local-pdict.
    type(string_setting_type), target, public :: spellcheck_local_pdict = string_setting_type(name="spellcheck_local_pdict")

    !> `spellcheck_parser' values.
    integer, parameter, public :: SPELLCHECK_PARSER_USE_MODE_NAME = 0, SPELLCHECK_PARSER_TEX = 1, SPELLCHECK_PARSER_NROFF = 2

    !> Indicates whether ispell should parse the current buffer as TeX Code.
    !> SPELLCHECK_PARSER_USE_MODE_NAME tries to guess using the name of the
    !> major mode.  Default parser is nroff.  Currently the only other valid
    !> parser is SPELLCHECK_PARSER_TEX.  Port of ispell.el ispell-parser.
    type(integer_setting_type), target, public :: spellcheck_parser = &
                                                  integer_setting_type(name="spellcheck_parser", &
                                                                       value=SPELLCHECK_PARSER_USE_MODE_NAME)

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    !> The process object for Ispell.  Port of ispell.el ispell-process.
    type(handle_type), public :: spellcheck_process
    ! ispell-process-directory: the directory where it was started.
    character(len=:), allocatable :: spellcheck_process_directory
    !> Output filter from piped calls to Ispell: the output lines, in the
    !> order they arrived (upstream's list holds the newest first).  Port
    !> of ispell.el ispell-filter.
    type(string_type), allocatable, public :: spellcheck_filter_variable(:)
    ! ispell-filter-continue: the last line is incomplete.
    logical :: spellcheck_filter_continue = .false.
    ! ispell-last-program-name (unallocated: never set).
    character(len=:), allocatable :: spellcheck_last_program_name
    !> Contains the buffer name if local word definitions were used.
    !> Ispell is then restarted because the local words could conflict
    !> ("" for none).  Port of ispell.el ispell-buffer-local-name.
    character(len=:), allocatable, public :: spellcheck_buffer_local_name
    !> True when the spell session is terminated (with
    !> `spellcheck_quit_position', the position to continue from).  Port of
    !> ispell.el ispell-quit.
    logical, public :: spellcheck_quit = .false.
    integer(int64) :: spellcheck_quit_position = 0
    ! ispell-region-end and ispell-recursive-edit-marker.
    type(handle_type) :: spellcheck_region_end, spellcheck_recursive_edit_marker
    ! ispell-overlay.
    type(handle_type) :: spellcheck_overlay
    ! ispell--abbrev-saving-allowed and ispell--save-correction-as-abbrev.
    logical :: spellcheck_abbreviation_saving_allowed = .false., spellcheck_save_correction_as_abbreviation = .false.

    !> The words of a buffer's `spellcheck_buffer_session_localwords'.
    type :: session_localwords_type
        integer(int64) :: serial = 0
        type(string_type), allocatable :: words(:)
    end type session_localwords_type

    ! ispell-buffer-session-localwords, per buffer.
    type(session_localwords_type), allocatable :: session_localwords(:)

    ! What `spellcheck_command_loop' returns (upstream's nil, 0, a string and
    ! the lists).
    integer, parameter :: REPLACE_KEEP = 0, REPLACE_LOCAL = 1, REPLACE_WORD = 2, REPLACE_RECHECK = 3, &
                          REPLACE_RECHECK_HERE = 4, REPLACE_QUERY = 5

    ! A result of `spellcheck_command_loop': `kind' and the word.
    type :: replacement_type
        integer :: kind = REPLACE_KEEP
        character(len=:), allocatable :: word
    end type replacement_type

    !> `spellcheck_parse_output' kinds.
    integer, parameter, public :: SPELLCHECK_OUTPUT_CORRECT = 0, SPELLCHECK_OUTPUT_ROOT = 1, SPELLCHECK_OUTPUT_MISSPELLED = 2, &
                                  SPELLCHECK_OUTPUT_ERROR = 3

    !> What `spellcheck_parse_output' returns: `kind' SPELLCHECK_OUTPUT_CORRECT for
    !> an exact match, SPELLCHECK_OUTPUT_ROOT with the root word matched via
    !> suffix removal in `word', SPELLCHECK_OUTPUT_MISSPELLED with the possibly
    !> misspelled `word', its line `offset', the possible correct spellings
    !> (`misses') and the possibly valid affix constructions (`guesses'),
    !> SPELLCHECK_OUTPUT_ERROR when an error has occurred.
    type, public :: spellcheck_output_type
        integer :: kind = SPELLCHECK_OUTPUT_ERROR
        character(len=:), allocatable :: word
        integer(int64) :: offset = 0
        type(string_type), allocatable :: misses(:), guesses(:)
    end type spellcheck_output_type

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    ! ------------------------------------------------------------------
    ! The speller program
    ! ------------------------------------------------------------------

    !> `spellcheck_format_word_function's default: WORD in upper case.
    !> Not a port.
    subroutine upcase_word(word, formatted)
        character(len=*), intent(in) :: word
        character(len=:), allocatable, intent(out) :: formatted

        formatted = upcase_string(word)
    end subroutine upcase_word

    ! WORD formatted with `spellcheck_format_word_function'.  Not a port.
    function formatted_word(word) result(text)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text

        call spellcheck_format_word_function(word, text)
    end function formatted_word

    ! The value of a string setting, "" when it is unset.  Not a port.
    function string_value(setting) result(text)
        type(string_setting_type), intent(in) :: setting
        character(len=:), allocatable :: text

        text = ""
        if (allocated(setting%value)) text = setting%value
    end function string_value

    ! The integer at the start of TEXT after blanks (string-to-number), 0 if
    ! none.  Not a port.
    integer(int64) function leading_integer(text) result(n)
        character(len=*), intent(in) :: text
        integer :: i, j, ios

        n = 0
        i = verify(text, " ")
        if (i == 0) return
        j = i
        if (text(j:j) == "-" .or. text(j:j) == "+") j = j + 1

        do while (j <= len(text))
            if (text(j:j) < "0" .or. text(j:j) > "9") exit
            j = j + 1
        end do

        if (j - 1 < i) return
        read (text(i:j - 1), *, iostat=ios) n
        if (ios /= 0) n = 0
    end function leading_integer

    ! Make a temporary buffer current (with-temp-buffer), in directory DIRECTORY
    ! (the current buffer's when absent); OLD is the buffer to go back to.
    ! Not a port.
    subroutine temporary_begin(old, temporary, directory_name)
        type(handle_type), intent(out) :: old, temporary
        character(len=*), intent(in), optional :: directory_name
        character(len=:), allocatable :: directory
        type(status_type) :: st

        old = current_buffer()
        directory = "./"

        if (current /= 0) then
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        end if

        if (present(directory_name)) directory = directory_name
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        buffers(current)%directory = directory
    end subroutine temporary_begin

    ! Close the temporary buffer TEMPORARY and make OLD current again.
    ! Not a port.
    subroutine temporary_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine temporary_end

    ! The current buffer's `default_directory' in SAVED (unallocated if it
    ! has none).  Not a port.
    subroutine save_directory(saved)
        character(len=:), allocatable, intent(out) :: saved

        if (allocated(buffers(current)%directory)) saved = buffers(current)%directory
    end subroutine save_directory

    ! Give the current buffer back the `default_directory' SAVED.
    ! Not a port.
    subroutine restore_directory(saved)
        character(len=:), allocatable, intent(in) :: saved

        if (allocated(saved)) then
            buffers(current)%directory = saved
        else if (allocated(buffers(current)%directory)) then
            deallocate (buffers(current)%directory)
        end if
    end subroutine restore_directory

    ! A reasonable default directory: the current buffer's if accessible,
    ! else "~/".  Port of ispell.el:ispell-with-safe-default-directory.
    function safe_default_directory() result(directory)
        character(len=:), allocatable :: directory
        type(status_type) :: st

        directory = "./"

        if (current /= 0) then
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        end if

        if (is_file_accessible_directory(directory)) return
        call expand_file_name("~/", directory, st)
    end function safe_default_directory

    ! Run PROGRAM with ARGUMENTS, its output inserted in the current buffer, in a
    ! safe default directory; RESULT is its exit status or the signal
    ! description.  Port of ispell.el:ispell-call-process.
    subroutine spellcheck_execute_process(program, arguments, result, status)
        character(len=*), intent(in) :: program
        type(value_type), intent(in) :: arguments
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved

        call save_directory(saved)
        buffers(current)%directory = safe_default_directory()
        call execute_process(program, result, status, destination=value_type(.true.), arguments=arguments)
        call restore_directory(saved)
    end subroutine spellcheck_execute_process

    !> Like `execute_process_region', but defend against bad
    !> `default_directory': run PROGRAM with ARGUMENTS on the text from START to
    !> END, its output inserted in buffer DESTINATION (a buffer name);
    !> RESULT is its exit status or the signal description.  Port of
    !> ispell.el:ispell-call-process-region.
    subroutine spellcheck_execute_process_region(start, end, program, destination, arguments, result, status)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: program, destination
        type(value_type), intent(in) :: arguments
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved

        call save_directory(saved)
        buffers(current)%directory = safe_default_directory()
        call execute_process_region(program, result, status, start=start, end=end, &
                                    destination=value_type(text=destination), arguments=arguments)
        call restore_directory(saved)
    end subroutine spellcheck_execute_process_region

    !> Ensure that `spellcheck_program_name' is valid and has the correct
    !> version.  Return version number if called interactively, or if
    !> INTERACTIVE is true.  Otherwise return the library directory name,
    !> if that is defined ("" otherwise).  Port of
    !> ispell.el:ispell-check-version.
    recursive subroutine spellcheck_check_version(result, status, interactive)
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: old, temporary
        type(value_type) :: code
        character(len=:), allocatable :: libvar, program_version, minver, envval
        logical :: envfound
        integer(int64) :: found
        logical :: saved_fold, interactivep, hunspell8
        type(version_type) :: program, really_version
        type(status_type) :: st
        character(len=40) :: code_text

        interactivep = .false.
        if (present(interactive)) interactivep = interactive
        result = ""
        call temporary_begin(old, temporary, temporary_file_directory())
        call spellcheck_execute_process(spellcheck_program_name, list(value_type(text="-vv")), code, status)

        if (.not. status%ok()) then
            call temporary_end(old, temporary)
            return
        end if

        call goto_character(point_minimum())

        if (interactivep) then
            ! Report version information of ispell.
            call end_of_line()
            call buffer_substring_no_properties(point_minimum(), point(), result, st)
            call message(result)
        else
            ! Return LIBDIR or LIBRARYVAR (overrides LIBDIR) env.
            result = configuration_variable("LIBDIR")
            libvar = configuration_variable("LIBRARYVAR")

            if (len(libvar) > 0) then
                call getenv(libvar, envval, envfound)
                if (envfound .and. len(envval) > 0) result = envval
            end if
        end if

        call goto_character(point_minimum())

        if (vtag(code) == V_STRING) then
            call set_error(status, GENERIC_ERROR_CODE, spellcheck_program_name//" exited with signal "//xstring(code))
            call temporary_end(old, temporary)
            return
        else if (vtag(code) == V_INTEGER) then
            if (xfixnum(code) /= 0) then
                write (code_text, '(" exited with code ", i0)') xfixnum(code)
                call set_error(status, GENERIC_ERROR_CODE, spellcheck_program_name//trim(code_text))
                call temporary_end(old, temporary)
                return
            end if
        end if

        ! Get relevant version strings.
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        program_version = ""
        call regular_expression_search_forward("\([0-9]+\.[0-9.]+\)", found, st, noerror=SEARCH_NOERROR)
        if (st%ok() .and. found > 0) program_version = match_string_no_properties(1)
        ! Make sure these variables are (re-)initialized to the default value.
        spellcheck_really_aspell = ""
        spellcheck_really_hunspell = ""
        spellcheck_really_enchant = ""
        spellcheck_encoding8_command = ""
        spellcheck_really_aspell = really("Aspell")
        if (len(spellcheck_really_aspell) == 0) spellcheck_really_hunspell = really("Hunspell")

        if (len(spellcheck_really_aspell) == 0 .and. len(spellcheck_really_hunspell) == 0) then
            spellcheck_really_enchant = really("Enchant")
        end if

        case_fold_search = saved_fold
        call temporary_end(old, temporary)
        minver = ""

        ! No version found (upstream's nil).
        if (len(program_version) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Version must be a string")
            return
        end if

        ! Invalid version syntax is an `error' error condition, as upstream.
        program = version(program_version)
        status = program%status
        if (.not. status%ok()) return

        if (version("3.1.12") > program) then
            minver = "3.1.12"
        else if (len(spellcheck_really_aspell) > 0) then
            really_version = version(spellcheck_really_aspell)
            status = really_version%status
            if (.not. status%ok()) return
            if (version("0.60") > really_version) minver = "0.60"
        else if (len(spellcheck_really_enchant) > 0) then
            really_version = version(spellcheck_really_enchant)
            status = really_version%status
            if (.not. status%ok()) return
            if (version("2.1.0") > really_version) minver = "2.1.0"
        end if

        if (len(minver) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, spellcheck_program_name//" release "//minver//" or greater is required")
            return
        end if

        if (len(spellcheck_really_aspell) > 0) then
            spellcheck_encoding8_command = "--encoding="
        else if (len(spellcheck_really_hunspell) > 0) then
            really_version = version(spellcheck_really_hunspell)
            status = really_version%status
            if (.not. status%ok()) return
            hunspell8 = version("1.1.6") <= really_version

            if (hunspell8) then
                spellcheck_encoding8_command = "-i"
            else
                spellcheck_really_hunspell = ""
            end if
        end if

    contains

        ! The value of the configuration variable VARIABLE in the output, "" if
        ! absent.
        function configuration_variable(variable) result(value)
            character(len=*), intent(in) :: variable
            character(len=:), allocatable :: value
            integer(int64) :: position
            type(status_type) :: s

            value = ""
            call regular_expression_search_forward(variable//" = \""\(.+?\)\""", position, s, noerror=SEARCH_NOERROR)
            if (s%ok() .and. position > 0) value = match_string_no_properties(1)
        end function configuration_variable

        ! The version after "(but really PROGRAM", "" if absent.
        function really(program) result(value)
            character(len=*), intent(in) :: program
            character(len=:), allocatable :: value
            integer(int64) :: position
            type(status_type) :: s

            value = ""
            call goto_character(point_minimum())
            call regular_expression_search_forward("(but really "//program//" \([0-9]+\.[0-9.]+\([-._+ ]?[a-zA-Z0-9]+\)?\)?)", &
                                                   position, s, noerror=SEARCH_NOERROR)
            if (s%ok() .and. position > 0) value = match_string_no_properties(1)
        end function really
    end subroutine spellcheck_check_version

    ! The output of PROGRAM run with ARGUMENTS, "" if it fails.  Not a port.
    function program_output(arguments) result(text)
        type(value_type), intent(in) :: arguments
        character(len=:), allocatable :: text
        type(handle_type) :: old, temporary
        type(value_type) :: code
        type(status_type) :: st

        call temporary_begin(old, temporary)
        call spellcheck_execute_process(spellcheck_program_name, arguments, code, st)
        text = buffer_string()
        call temporary_end(old, temporary)
    end function program_output

    ! The first whitespace-separated word of TEXT (car (split-string)), ""
    ! if none.  Not a port.
    function first_word(text) result(word)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: word
        integer :: i, j

        word = ""
        i = verify(text, " "//TAB//LF//achar(13)//achar(12))
        if (i == 0) return
        j = scan(text(i:), " "//TAB//LF//achar(13)//achar(12))

        if (j == 0) then
            word = text(i:)
        else
            word = text(i:i + j - 2)
        end if
    end function first_word

    !> Find Aspell's dictionaries, and record in `spellcheck_aspell_dictionaries'.
    !> Port of ispell.el:ispell-find-aspell-dictionaries.
    subroutine spellcheck_find_aspell_dictionaries()
        character(len=:), allocatable :: text, word
        type(spellcheck_dictionary_type), allocatable :: found(:)
        type(spellcheck_dictionary_type) :: dictionary
        logical :: ok
        integer :: k

        allocate (found(0))
        text = program_output(list(value_type(text="dicts")))

        do
            word = first_word(text)
            if (len(word) == 0) exit
            text = text(index(text, word) + len(word):)
            ! Search for the named dictionaries.
            call spellcheck_aspell_find_dictionary(word, dictionary, ok)
            if (ok) found = [found, dictionary]
        end do

        ! Ensure aspell's alias dictionary will override standard
        ! definitions.
        call spellcheck_aspell_add_aliases(found)
        spellcheck_aspell_found_dictionaries = found

        ! Merge into FOUND any elements from the standard
        ! `spellcheck_base_dictionaries' which have no element in FOUND at all.
        do k = 1, size(spellcheck_base_dictionaries)
            if (find_dictionary(found, spellcheck_base_dictionaries(k)%name) == 0) then
                found = [found, spellcheck_base_dictionaries(k)]
            end if
        end do

        ! Add a default entry.
        dictionary = spellcheck_dictionary_type(name="", casechars="[[:alpha:]]", not_casechars="[^[:alpha:]]", &
                                                otherchars="[']", many_otherchars=.false., &
                                                arguments=[character(len=64) :: "-B", "", "", ""], coding_system="utf-8")
        spellcheck_aspell_dictionaries = [dictionary, found]
    end subroutine spellcheck_find_aspell_dictionaries

    !> The parameters DICTIONARY for an Aspell dictionary DICTIONARY_NAME (in the form
    !> of `spellcheck_base_dictionaries' elements).  OK is false if no
    !> associated data file is found for the dictionary.  Port of
    !> ispell.el:ispell-aspell-find-dictionary.
    subroutine spellcheck_aspell_find_dictionary(dictionary_name, dictionary, ok)
        character(len=*), intent(in) :: dictionary_name
        type(spellcheck_dictionary_type), intent(out) :: dictionary
        logical, intent(out) :: ok
        character(len=:), allocatable :: data_file, path, specials, word, next, name
        character(len=64), allocatable :: otherchars(:)
        character(len=*), parameter :: regular_expressions(3) = [character(len=32) :: "^[[:alpha:]]+", "^[[:alpha:]_]+", &
                                                                                                       "^[[:alpha:]]+-\(alt\|old\)"]
        type(handle_type) :: old, temporary
        integer(int64) :: position, inserted
        type(status_type) :: st
        integer :: d, r

        ok = .false.
        ! Make sure `spellcheck_aspell_dictionary_directory' is defined.
        if (len(spellcheck_aspell_dictionary_directory) == 0) spellcheck_aspell_dictionary_directory = first_word(program_output( &
                                                                                                   list(value_type(text="config"), &
                                                                                                      value_type(text="dict-dir"))))
        ! Make sure `spellcheck_aspell_data_directory' is defined.
        if (len(spellcheck_aspell_data_directory) == 0) spellcheck_aspell_data_directory = first_word(program_output( &
                                                                                                   list(value_type(text="config"), &
                                                                                                      value_type(text="data-dir"))))
        ! Try finding associated datafile.  aspell will look for master
        ! .dat file in `dict-dir' and `data-dir'.  Associated .dat files
        ! must be in the same directory as master file.
        data_file = ""

        search: do d = 1, 2
            path = spellcheck_aspell_dictionary_directory
            if (d == 2) path = spellcheck_aspell_data_directory

            ! Try xx.dat first, strip out variant, country code, etc, then
            ! try xx_YY.dat (without stripping country code), then try
            ! xx-alt.dat, for de-alt etc.
            do r = 1, 3
                name = ""
                if (string_match(trim(regular_expressions(r)), dictionary_name, st) > 0) name = match_string_no_properties(0, &
                                                                                                                    dictionary_name)

                if (is_file_readable(path//"/"//name//".dat")) then
                    data_file = path//"/"//name//".dat"
                    exit search
                end if
            end do
        end do search

        if (len(data_file) == 0) return
        allocate (otherchars(0))
        call temporary_begin(old, temporary)
        call insert_file_contents(data_file, .false., inserted, st)
        call goto_character(point_minimum())
        ! There is zero or one line with special characters declarations.
        call regular_expression_search_forward("^special", position, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. position > 0) then
            call buffer_substring_no_properties(point(), line_end_position(), specials, st)

            ! The line looks like: special ' -** - -** . -** : -*-
            ! -** means that this character
            !    - doesn't appear at word start
            !    * may appear in the middle of a word
            !    * may appear at word end
            ! `otherchars' is about the middle case.
            do
                word = first_word(specials)
                if (len(word) == 0) exit
                specials = specials(index(specials, word) + len(word):)
                next = first_word(specials)
                if (len(next) == 0) exit
                specials = specials(index(specials, next) + len(next):)

                if (len(next) >= 2) then
                    if (next(2:2) == "*") otherchars = [character(len=64) :: word, otherchars]
                end if
            end do
        end if

        call temporary_end(old, temporary)
        dictionary%name = dictionary_name
        dictionary%casechars = "[[:alpha:]]"
        dictionary%not_casechars = "[^[:alpha:]]"
        dictionary%otherchars = regular_expression_optimization(otherchars)
        ! We can't tell, so set this to true.
        dictionary%many_otherchars = .true.
        dictionary%arguments(1) = "-d"
        dictionary%arguments(2) = dictionary_name
        ! Here we specify the encoding to use while communicating with
        ! aspell.
        dictionary%coding_system = "utf-8"
        ok = .true.
    end subroutine spellcheck_aspell_find_dictionary

    !> Find Aspell's dictionary aliases and add them to the dictionaries
    !> DICTIONARIES.  Port of ispell.el:ispell-aspell-add-aliases.
    subroutine spellcheck_aspell_add_aliases(dictionaries)
        type(spellcheck_dictionary_type), allocatable, intent(inout) :: dictionaries(:)
        type(value_type) :: files
        type(spellcheck_dictionary_type) :: alias
        type(value_type), allocatable :: elements(:)
        type(handle_type) :: old, temporary
        character(len=:), allocatable :: file, aliasname, realname
        integer(int64) :: position, inserted, k, n
        type(status_type) :: st
        integer :: j, real_index

        if (len(spellcheck_aspell_dictionary_directory) == 0) spellcheck_aspell_dictionary_directory = first_word(program_output( &
                                                                                                   list(value_type(text="config"), &
                                                                                                      value_type(text="dict-dir"))))
        ! (file-expand-wildcards "DICT-DIR/*.alias")
        call directory_files(spellcheck_aspell_dictionary_directory, files, st, full=.true.)
        n = 0
        allocate (elements(0))
        if (st%ok() .and. len(spellcheck_aspell_dictionary_directory) > 0) call list_elements(files, elements)
        n = size(elements)

        do k = 1, n
            file = xstring(elements(k))
            if (len(file) < 6) cycle
            if (file(len(file) - 5:) /= ".alias") cycle
            aliasname = file_name_nondirectory(file)
            if (aliasname(1:1) == ".") cycle
            aliasname = aliasname(1:len(aliasname) - 6)
            call temporary_begin(old, temporary)
            call insert_file_contents(file, .false., inserted, st)
            call goto_character(point_minimum())
            ! Look for a line "add FOO.multi", extract FOO.
            call regular_expression_search_forward("^add \([^.]+\)\.multi", position, st, noerror=SEARCH_NOERROR)
            realname = ""
            if (st%ok() .and. position > 0) realname = match_string_no_properties(1)
            call temporary_end(old, temporary)
            if (len(realname) == 0) cycle
            real_index = find_dictionary(dictionaries, realname)

            if (real_index > 0 .and. find_dictionary(dictionaries, aliasname) == 0) then
                alias = dictionaries(real_index)
                alias%name = aliasname
                dictionaries = [alias, dictionaries]
            end if
        end do

        ! Add entries for standard dict-names with found locale-matching
        ! entry.
        do j = 1, size(spellcheck_dictionaries_name2locale_equivs)
            associate (e => spellcheck_dictionaries_name2locale_equivs(j))
                ! Skip if already present.
                if (find_dictionary(dictionaries, trim(e%name)) > 0) cycle
                real_index = find_dictionary(dictionaries, trim(e%locale))

                if (real_index > 0) then
                    alias = dictionaries(real_index)
                    alias%name = e%name
                    dictionaries = [alias, dictionaries]
                end if
            end associate
        end do
    end subroutine spellcheck_aspell_add_aliases

    ! The index of the dictionary NAME in DICTIONARIES, 0 if absent.
    ! Not a port.
    integer function find_dictionary(dictionaries, name) result(k)
        type(spellcheck_dictionary_type), intent(in) :: dictionaries(:)
        character(len=*), intent(in) :: name

        do k = 1, size(dictionaries)
            if (dictionaries(k)%name == name) return
        end do

        k = 0
    end function find_dictionary

    !> Initialize some spellchecker parameters when changed or first used.
    !> Port of ispell.el:ispell-set-spellchecker-params (no
    !> `ispell-base-dicts-override-alist'; Hunspell's and Enchant's
    !> dictionaries are not discovered).
    recursive subroutine spellcheck_set_spellchecker_parameters(status)
        type(status_type), intent(out) :: status
        type(spellcheck_dictionary_type), allocatable :: found(:), base(:), all(:)
        type(spellcheck_dictionary_type) :: dictionary
        character(len=:), allocatable :: libdir
        type(status_type) :: st
        integer :: k, a, equiv
        logical :: checked

        status = status_type()

        if (allocated(spellcheck_last_program_name)) then
            if (spellcheck_last_program_name == spellcheck_program_name) return
        end if

        call spellcheck_kill_spellcheck(st, no_error=.true.)
        call spellcheck_check_version(libdir, st)
        checked = st%ok()
        if (checked) spellcheck_library_directory = libdir

        ! Auto-detection will only be used if spellchecker is not ispell
        ! and supports a way to set communication to UTF-8.
        if (checked .and. (len(spellcheck_encoding8_command) > 0 .or. len(spellcheck_really_enchant) > 0)) then
            if (len(spellcheck_really_aspell) > 0) then
                if (size(spellcheck_aspell_dictionaries) == 0) call spellcheck_find_aspell_dictionaries()
            end if
        end if

        ! Substitute `spellcheck_dictionaries' with the list of dictionaries
        ! corresponding to the given spellchecker.  With programs that
        ! support it, use the list of really installed dictionaries and add
        ! to it elements of the original list that are not present there.
        allocate (found(0))
        if (len(spellcheck_encoding8_command) > 0 .and. len(spellcheck_really_aspell) > 0) found = spellcheck_aspell_dictionaries
        base = spellcheck_base_dictionaries

        ! While ispell and aspell (through aliases) use the traditional
        ! dict naming originally expected by ispell.el, hunspell & Enchant
        ! use locale-based names with no alias.  We need to map standard
        ! names to locale based names to make default dict definitions
        ! available to these programs.
        if (len(spellcheck_really_hunspell) > 0 .or. len(spellcheck_really_enchant) > 0) then
            allocate (all(0))

            do k = 1, size(base)
                dictionary = base(k)
                ! Remove "-d" option from `arguments' if present.
                a = findloc(dictionary%arguments, "-d", dim=1)
                if (a > 0) dictionary%arguments(a:) = eoshift(dictionary%arguments(a:), 2)

                ! Unless default dict, re-add "-d" option with the mapped
                ! value.
                if (len_trim(dictionary%name) > 0) then
                    equiv = findloc(spellcheck_dictionaries_name2locale_equivs%name, dictionary%name(1:16), dim=1)

                    if (equiv == 0) then
                        call message("ispell-set-spellchecker-params: Missing equivalent for """//trim(dictionary%name)// &
                                     """. Skipping.")
                        cycle
                    end if

                    a = findloc(dictionary%arguments, "", dim=1)

                    if (a > 0 .and. a < size(dictionary%arguments)) then
                        dictionary%arguments(a) = "-d"
                        dictionary%arguments(a + 1) = spellcheck_dictionaries_name2locale_equivs(equiv)%locale
                    end if
                end if

                if (.not. has_dictionary(all, dictionary)) all = [dictionary, all]
            end do

            base = all
        end if

        call spellcheck_initialize_spellchecker_hook%execute(st)
        ! Add dicts to `spellcheck_dictionaries' unless already present.
        allocate (all(0))
        if (allocated(all)) deallocate (all)
        allocate (all(0))
        call add_new(found)
        call add_new(base)
        spellcheck_dictionaries = all
        ! If spellchecker supports UTF-8 via command-line option, use it in
        ! communication.  This does not affect definitions in your init
        ! file.
        deallocate (all)
        allocate (all(0))

        do k = 1, size(spellcheck_dictionaries)
            dictionary = spellcheck_dictionaries(k)
            if (len(spellcheck_encoding8_command) > 0 .or. len(spellcheck_really_enchant) > 0) dictionary%coding_system = "utf-8"
            if (.not. has_dictionary(all, dictionary)) all = [dictionary, all]
        end do

        spellcheck_dictionaries = all
        spellcheck_last_program_name = spellcheck_program_name

    contains

        ! Push the dictionaries of LIST not yet in ALL (by name).
        subroutine add_new(dictionaries)
            type(spellcheck_dictionary_type), intent(in) :: dictionaries(:)
            integer :: i

            do i = 1, size(dictionaries)
                if (find_dictionary(all, dictionaries(i)%name) == 0) all = [dictionaries(i), all]
            end do
        end subroutine add_new
    end subroutine spellcheck_set_spellchecker_parameters

    ! Whether DICTIONARIES has a dictionary equal to DICTIONARY in every component.
    ! Not a port.
    logical function has_dictionary(dictionaries, dictionary)
        type(spellcheck_dictionary_type), intent(in) :: dictionaries(:), dictionary
        integer :: k

        has_dictionary = .false.

        do k = 1, size(dictionaries)
            if (dictionaries(k)%name /= dictionary%name .or. dictionaries(k)%casechars /= dictionary%casechars) cycle
            if (dictionaries(k)%not_casechars /= dictionary%not_casechars .or. dictionaries(k)%otherchars &
                /= dictionary%otherchars) cycle
            if (dictionaries(k)%many_otherchars .neqv. dictionary%many_otherchars) cycle
            if (any(dictionaries(k)%arguments /= dictionary%arguments)) cycle
            if (dictionaries(k)%extended_character_mode /= dictionary%extended_character_mode) cycle
            if (dictionaries(k)%coding_system /= dictionary%coding_system) cycle
            has_dictionary = .true.
            return
        end do
    end function has_dictionary

    !> The names of the valid dictionaries, "default" first.
    !> The variable `spellcheck_library_directory' defines their location.
    !> Port of ispell.el:ispell-valid-dictionary-list.
    recursive subroutine spellcheck_valid_dictionary_list(names, status)
        character(len=64), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        type(spellcheck_dictionary_type), allocatable :: dictionaries(:)
        character(len=:), allocatable :: name, explicit, bname
        integer :: k, a

        call spellcheck_set_spellchecker_parameters(status)
        dictionaries = [spellcheck_local_dictionaries, spellcheck_dictionaries]
        names = [character(len=64) :: "default"]

        do k = 1, size(dictionaries)
            name = trim(dictionaries(k)%name)
            if (len(name) == 0) cycle
            ! Explicitly (via args) specified dictionary.
            explicit = ""
            a = findloc(dictionaries(k)%arguments, "-d", dim=1)

            if (a > 0 .and. a < size(dictionaries(k)%arguments)) then
                explicit = trim(dictionaries(k)%arguments(a + 1))
            end if

            bname = name
            if (len(explicit) > 0) bname = explicit
            if (is_valid(name, explicit, bname)) names = [character(len=64) :: name, names]
        end do

    contains

        ! Whether the dictionary NAME is installed.
        logical function is_valid(dictionary_name, dictionary_explicit, dictionary_bname)
            character(len=*), intent(in) :: dictionary_name, dictionary_explicit, dictionary_bname

            ! Include all for Aspell (we already know existing dicts).
            if (len(spellcheck_really_aspell) > 0) then
                is_valid = find_dictionary(spellcheck_aspell_found_dictionaries, dictionary_name) > 0
                if (is_valid) return
            end if

            ! Include all if `spellcheck_library_directory' is "" (Hunspell).
            is_valid = len(spellcheck_really_aspell) == 0 .and. len(spellcheck_library_directory) == 0
            if (is_valid) return

            ! If explicit (-d with an absolute path) and existing dict.
            if (len(dictionary_explicit) > 0) then
                if (is_file_name_absolute(dictionary_explicit)) then
                    is_valid = is_located(dictionary_explicit, "")
                    if (is_valid) return
                end if
            end if

            ! If dict located in `spellcheck_library_directory'.
            is_valid = is_located(dictionary_bname, spellcheck_library_directory)
        end function is_valid

        ! (locate-file (file-name-nondirectory DICTIONARY) (DIRECTORY) suffixes).
        logical function is_located(dictionary, directory_name)
            character(len=*), intent(in) :: dictionary, directory_name
            character(len=:), allocatable :: directory, base

            directory = directory_name

            if (len(directory) == 0) then
                directory = dictionary(1:index(dictionary, "/", back=.true.))
            end if

            if (len(directory) == 0) then
                is_located = .false.
                return
            end if

            if (directory(len(directory):) /= "/") directory = directory//"/"
            base = file_name_nondirectory(dictionary)

            if (index(base, ".") > 1) then
                is_located = is_file_readable(directory//base)
            else
                is_located = is_file_readable(directory//base//".hash") .or. is_file_readable(directory//base//".has")
            end if
        end function is_located
    end subroutine spellcheck_valid_dictionary_list

    ! ------------------------------------------------------------------
    ! The current dictionary
    ! ------------------------------------------------------------------

    ! The record of the current dictionary in DICTIONARY; FOUND is false if it is
    ! in neither list.  Not a port.
    subroutine current_dictionary(dictionary, found)
        type(spellcheck_dictionary_type), intent(out) :: dictionary
        logical, intent(out) :: found
        character(len=:), allocatable :: name
        integer :: k

        name = spellcheck_current_dictionary
        found = .true.
        k = 0
        if (allocated(spellcheck_local_dictionaries)) k = find_dictionary(spellcheck_local_dictionaries, name)

        if (k > 0) then
            dictionary = spellcheck_local_dictionaries(k)
            return
        end if

        k = 0
        if (allocated(spellcheck_dictionaries)) k = find_dictionary(spellcheck_dictionaries, name)

        if (k > 0) then
            dictionary = spellcheck_dictionaries(k)
            return
        end if

        found = .false.
    end subroutine current_dictionary

    !> Get the decoded string in slot N (1 CASECHARS, 2 NOT-CASECHARS, 3
    !> OTHERCHARS) of the descriptor of the current dict.  Port of
    !> ispell.el:ispell-get-decoded-string.
    subroutine spellcheck_get_decoded_string(n, text, status)
        integer, intent(in) :: n
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(spellcheck_dictionary_type) :: dictionary
        logical :: found

        text = ""
        call current_dictionary(dictionary, found)

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "No data for dictionary """//spellcheck_current_dictionary// &
                           """ in `spellcheck_local_dictionaries' or `spellcheck_dictionaries'")
            return
        end if

        select case (n)
        case (1)
            call decode_coding_string(trim(dictionary%casechars), trim(dictionary%coding_system), text, status)
        case (2)
            call decode_coding_string(trim(dictionary%not_casechars), trim(dictionary%coding_system), text, status)
        case default
            call decode_coding_string(trim(dictionary%otherchars), trim(dictionary%coding_system), text, status)
        end select
    end subroutine spellcheck_get_decoded_string

    !> Return CASECHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-casechars.
    function spellcheck_get_casechars() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call spellcheck_get_decoded_string(1, text, st)
    end function spellcheck_get_casechars

    !> Return NOT-CASECHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-not-casechars.
    function spellcheck_get_not_casechars() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call spellcheck_get_decoded_string(2, text, st)
    end function spellcheck_get_not_casechars

    !> Return OTHERCHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-otherchars.
    function spellcheck_get_otherchars() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call spellcheck_get_decoded_string(3, text, st)
    end function spellcheck_get_otherchars

    !> Return MANY-OTHERCHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-many-otherchars-p.
    logical function spellcheck_get_many_otherchars()
        type(spellcheck_dictionary_type) :: dictionary
        logical :: found

        call current_dictionary(dictionary, found)
        spellcheck_get_many_otherchars = found .and. dictionary%many_otherchars
    end function spellcheck_get_many_otherchars

    !> Return ARGUMENTS of a dictionary.  Port of ispell.el:ispell-get-ispell-args.
    subroutine spellcheck_get_spellcheck_arguments(arguments)
        character(len=64), intent(out) :: arguments(4)
        type(spellcheck_dictionary_type) :: dictionary
        logical :: found

        arguments = ""
        call current_dictionary(dictionary, found)
        if (found) arguments = dictionary%arguments
    end subroutine spellcheck_get_spellcheck_arguments

    !> Return extended character mode of a dictionary.  Port of
    !> ispell.el:ispell-get-extended-character-mode.
    function spellcheck_get_extended_character_mode() result(mode)
        character(len=:), allocatable :: mode
        type(spellcheck_dictionary_type) :: dictionary
        logical :: found

        mode = ""
        ! Hunspell treats ~word as ordinary words in pipe mode.  Disable
        ! extended-char-mode.
        if (len(spellcheck_really_hunspell) > 0) return
        call current_dictionary(dictionary, found)
        if (found) mode = trim(dictionary%extended_character_mode)
    end function spellcheck_get_extended_character_mode

    !> Return encoding of a dictionary.  Port of
    !> ispell.el:ispell-get-coding-system.
    function spellcheck_get_coding_system() result(coding)
        character(len=:), allocatable :: coding
        type(spellcheck_dictionary_type) :: dictionary
        logical :: found

        coding = ""
        call current_dictionary(dictionary, found)
        if (found) coding = trim(dictionary%coding_system)
    end function spellcheck_get_coding_system

    ! ------------------------------------------------------------------
    ! Talking to the process
    ! ------------------------------------------------------------------

    !> Wait for output from Ispell process, or for SECONDS.  Port of
    !> ispell.el:ispell-accept-output.
    recursive subroutine spellcheck_accept_output(status, seconds)
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        logical :: got

        if (.not. is_process_live(spellcheck_process)) then
            call set_error(status, GENERIC_ERROR_CODE, "No Ispell process to read output from!")
            return
        end if

        call accept_process_output(got, status, spellcheck_process, seconds)
    end subroutine spellcheck_accept_output

    !> Notify spell checker that MISSPELLED should be spelled REPLACEMENT.
    !> This allows improving the suggestion list based on actual
    !> misspellings.  Only works for Aspell and Enchant.  Port of
    !> ispell.el:ispell-send-replacement.
    subroutine spellcheck_send_replacement(misspelled, replacement)
        character(len=*), intent(in) :: misspelled, replacement
        type(status_type) :: st

        if (len(spellcheck_really_aspell) > 0 .or. len(spellcheck_really_enchant) > 0) then
            call spellcheck_send_string("$$ra "//misspelled//","//replacement//LF, st)
        end if
    end subroutine spellcheck_send_replacement

    !> Save MISSPELLED -> REPLACEMENT as an abbrev, if enabled.
    !> This is controlled by `spellcheck_save_corrections_as_abbreviations' and C-u in
    !> the command loop.  Port of
    !> ispell.el:ispell--maybe-save-correction-abbrev.
    subroutine spellcheck_maybe_save_correction_abbreviation(misspelled, replacement)
        character(len=*), intent(in) :: misspelled, replacement

        if (spellcheck_save_correction_as_abbreviation) then
            call global_abbreviation_table%define(misspelled, replacement)
            call message(""""//misspelled//""" now expands to """//replacement//""" globally")
        end if
    end subroutine spellcheck_maybe_save_correction_abbreviation

    !> Send the string STRING to the Ispell process.  Port of
    !> ispell.el:ispell-send-string (asynchronous processes).
    subroutine spellcheck_send_string(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_process_live(spellcheck_process)) call process_send_string(string, status, spellcheck_process)
    end subroutine spellcheck_send_string

    !> Filter function for ispell, grep, and look, process OUTPUT from
    !> subprocess.  OUTPUT can contain multiple lines, part of a single line,
    !> or both.  OUTPUT must end with a newline.  Port of
    !> ispell.el:ispell-filter.
    subroutine spellcheck_filter(process, output, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: output
        type(status_type), intent(out) :: status
        integer :: start, end, n
        logical :: more

        status = status_type()
        if (process%index < 0) return
        if (.not. allocated(spellcheck_filter_variable)) allocate (spellcheck_filter_variable(0))
        start = 1
        more = .true.

        do while (more)
            ! Get text up to the newline.
            end = index(output(start:), LF)
            if (end > 0) end = start + end - 1
            n = size(spellcheck_filter_variable)

            ! If we get out of sync and `spellcheck_filter_continue' is
            ! asserted when we are not continuing, treat the next item as a
            ! separate list.
            if (spellcheck_filter_continue .and. n > 0) then
                ! Continue with same line (item): add it to the prev item.
                if (end > 0) then
                    spellcheck_filter_variable(n)%text = spellcheck_filter_variable(n)%text//output(start:end - 1)
                else
                    spellcheck_filter_variable(n)%text = spellcheck_filter_variable(n)%text//output(start:)
                end if
            else
                ! This is a new line and item.
                if (end > 0) then
                    call append_string(spellcheck_filter_variable, output(start:end - 1))
                else
                    call append_string(spellcheck_filter_variable, output(start:))
                end if
            end if

            if (end == 0) then
                ! We've completed reading the output, but didn't finish the
                ! line.
                spellcheck_filter_continue = .true.
                more = .false.
            else
                ! Skip over newline, this line complete.
                spellcheck_filter_continue = .false.
                end = end + 1

                if (end > len(output)) then
                    ! No more lines in output, so we can exit the filter.
                    more = .false.
                else
                    start = end
                end if
            end if
        end do
    end subroutine spellcheck_filter

    ! Forget the output lines and the incomplete line flag.  Not a port.
    subroutine clear_filter()
        if (allocated(spellcheck_filter_variable)) deallocate (spellcheck_filter_variable)
        allocate (spellcheck_filter_variable(0))
        spellcheck_filter_continue = .false.
    end subroutine clear_filter

    ! Whether the newest output line is the empty line that ends an answer
    ! (string= "" (car ispell-filter)).  Not a port.
    logical function is_answer_complete()
        integer :: n

        is_answer_complete = .false.
        if (.not. allocated(spellcheck_filter_variable)) return
        n = size(spellcheck_filter_variable)
        if (n == 0) return
        is_answer_complete = len(spellcheck_filter_variable(n)%text) == 0
    end function is_answer_complete

    !> Parse the OUTPUT string from Ispell process into RESULT (see
    !> `spellcheck_output_type').
    !>
    !> Optional second arg ACCEPTED is the words already accepted.
    !> Optional third arg SHIFT is an offset to apply based on previous
    !> corrections.  Port of ispell.el:ispell-parse-output.
    subroutine spellcheck_parse_output(output, result, accepted, shift)
        character(len=*), intent(in) :: output
        type(spellcheck_output_type), intent(out) :: result
        type(string_type), intent(in), optional :: accepted(:)
        integer(int64), intent(in), optional :: shift
        character(len=:), allocatable :: rest, word
        integer(int64) :: count, current_count
        character :: kind
        type(status_type) :: st
        integer :: k, e

        if (output == "" .or. output == "*" .or. output == "-") then
            ! For startup with pipes, an exact match, a compound word match.
            result%kind = SPELLCHECK_OUTPUT_CORRECT
            return
        end if

        if (output(1:1) == "+") then
            ! Found because of root word.
            result%kind = SPELLCHECK_OUTPUT_ROOT
            result%word = output(min(3, len(output) + 1):)
            return
        end if

        if (string_match("[\r"//achar(13)//"a-zA-Z]", output, st) == 1) then
            ! Error message from ispell!
            call ding()
            call message("Ispell error: "//output)
            call sit_for(5000)
            result%kind = SPELLCHECK_OUTPUT_ERROR
            return
        end if

        ! Need to process &, ?, and #'s.
        kind = output(1:1)
        rest = output(3:)
        k = index(rest, " ")

        if (k == 0) then
            result%word = rest
            rest = ""
        else
            result%word = rest(1:k - 1)
            rest = rest(k:)
        end if

        current_count = 0
        allocate (result%misses(0), result%guesses(0))

        if (kind == "#") then
            ! No misses for type #.
            count = 0
        else
            ! Get number of misses.
            count = leading_integer(rest)
            k = index(rest(2:), " ")
            rest = rest(k + 2:)
        end if

        result%offset = leading_integer(rest)

        if (kind == "#") then
            ! No miss or guess list.
            rest = ""
        else
            k = index(rest(2:), " ")
            rest = rest(k + 2:)
        end if

        do while (len(rest) > 0)
            ! End of miss/guess.
            e = index(rest, ", ")
            current_count = current_count + 1

            if (e == 0) then
                word = rest
            else
                word = rest(1:e - 1)
            end if

            if (current_count > count) then
                call append_string(result%guesses, word)
            else
                call append_string(result%misses, word)
            end if

            if (e == 0) then
                ! No more misses or guesses.
                rest = ""
            else
                rest = rest(e + 2:)
            end if
        end do

        ! Accept word if it was already accepted; adjust offset.
        result%kind = SPELLCHECK_OUTPUT_MISSPELLED

        if (present(accepted)) then
            do k = 1, size(accepted)
                if (accepted(k)%text == result%word) then
                    result%kind = SPELLCHECK_OUTPUT_CORRECT
                    return
                end if
            end do
        end if

        if (present(shift)) result%offset = result%offset + shift
    end subroutine spellcheck_parse_output

    !> Return the status of the Ispell process ("" without one).  Port of
    !> ispell.el:ispell-process-status.
    function spellcheck_process_status() result(status_name)
        character(len=:), allocatable :: status_name

        status_name = ""
        if (spellcheck_process%index /= 0) status_name = process_status(spellcheck_process)
    end function spellcheck_process_status

    !> Start the Ispell process.  Port of ispell.el:ispell-start-process
    !> (asynchronous processes).
    subroutine spellcheck_start_process(process, status)
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status
        character(len=64) :: original(4)
        type(string_type), allocatable :: command(:)
        character(len=:), allocatable :: saved, directory, coding
        integer :: k, connection

        process = null_handle()
        call save_directory(saved)
        directory = "./"
        if (allocated(saved)) directory = saved
        ! Defend against bad `default_directory'.
        if (.not. is_file_accessible_directory(directory)) call expand_file_name("~/", directory, status)
        call spellcheck_get_spellcheck_arguments(original)
        ! Accept single input lines.
        call append_string(command, spellcheck_program_name, "-a")

        ! Make root/affix combos not in dict.  hunspell -m option means
        ! different.
        if (len(spellcheck_really_hunspell) > 0) then
            call append_string(command, "")
        else
            call append_string(command, "-m")
        end if

        ! Not for default dict; only define if not overridden.
        if (len(spellcheck_current_dictionary) > 0 .and. .not. any(original == "-d")) then
            call append_string(command, "-d", spellcheck_current_dictionary)
        end if

        do k = 1, size(original)
            if (len_trim(original(k)) > 0) call append_string(command, trim(original(k)))
        end do

        ! Use specified pers dict.
        if (len(spellcheck_current_personal_dictionary) > 0) then
            call append_string(command, "-p", spellcheck_current_personal_dictionary)
        end if

        ! If we are using recent aspell or hunspell, make sure we use the
        ! right encoding for communication.  ispell or older aspell/hunspell
        ! does not support this.
        if (len(spellcheck_encoding8_command) > 0) then
            coding = spellcheck_get_coding_system()

            if (len(spellcheck_really_hunspell) > 0) then
                call append_string(command, spellcheck_encoding8_command, upcase_string(coding))
            else
                call append_string(command, spellcheck_encoding8_command//coding)
            end if
        end if

        if (allocated(spellcheck_extra_arguments)) then
            do k = 1, size(spellcheck_extra_arguments)
                call append_string(command, trim(spellcheck_extra_arguments(k)))
            end do
        end if

        ! Initially we don't know any buffer's local words.
        spellcheck_buffer_local_name = ""
        connection = PROCESS_CONNECTION_PIPE
        if (spellcheck_use_ptys) connection = PROCESS_CONNECTION_PTY
        buffers(current)%directory = directory
        call make_process("ispell", command, process, status, connection_type=connection)
        call restore_directory(saved)
    end subroutine spellcheck_start_process

    !> Check status of Ispell process and start if necessary.  Port of
    !> ispell.el:ispell-init-process.
    recursive subroutine spellcheck_initialize_process(status)
        type(status_type), intent(out) :: status
        character(len=64) :: arguments(4)
        character(len=:), allocatable :: dictionary_bname, directory, home, libdir, coding, mode, text
        type(status_type) :: st
        integer :: a, k

        call spellcheck_get_spellcheck_arguments(arguments)
        ! Basename of dictionary used by the spell-checker.
        dictionary_bname = spellcheck_current_dictionary
        a = findloc(arguments, "-d", dim=1)
        if (a > 0 .and. a < size(arguments)) dictionary_bname = trim(arguments(a + 1))
        if (len(dictionary_bname) == 0) dictionary_bname = "default"
        call expand_file_name("~/", home, st)
        directory = "./"
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory

        ! Use "~/" as default directory unless using Ispell with per-dir
        ! personal dictionaries.
        if (len(spellcheck_really_aspell) > 0 .or. len(spellcheck_really_hunspell) > 0 .or. &
            .not. is_file_accessible_directory(directory)) then
            directory = home
        else if (.not. (is_file_readable(directory//".ispell_words") .or. is_file_readable(directory//".ispell_" &
                                                                                           //dictionary_bname))) then
            directory = home
        else
            call expand_file_name(directory, text, st)
            directory = text
        end if

        ! Check if process needs restart.
        if (spellcheck_process%index /= 0 .and. spellcheck_process_status() == "run" .and. &
            (len(string_value(spellcheck_local_pdict)) > 0 .or. len(spellcheck_personal_dictionary) > 0 .or. &
             spellcheck_process_directory == directory)) then
            call clear_filter()
            status = status_type()
            return
        end if

        ! May need to restart to select new personal dictionary.
        call spellcheck_kill_spellcheck(st, no_error=.true.)
        text = "default"

        if (len(string_value(spellcheck_local_dictionary)) > 0) then
            text = string_value(spellcheck_local_dictionary)
        else if (len(spellcheck_dictionary) > 0) then
            text = spellcheck_dictionary
        end if

        call message("Starting new Ispell process "//spellcheck_program_name//" with "//text//" dictionary...")
        call sit_for(0)
        call spellcheck_check_version(libdir, status)
        if (.not. status%ok()) return
        spellcheck_library_directory = libdir
        spellcheck_process_directory = directory
        call spellcheck_start_process(spellcheck_process, status)
        if (.not. status%ok()) return
        call clear_filter()
        call message("Starting new Ispell process "//spellcheck_program_name//" with "//text//" dictionary...done")

        if (spellcheck_process_directory /= home) then
            ! If not, kill ispell process when killing buffer.  It may be in
            ! a removable device that would otherwise become un-mountable.
            call prepend_kill_spellcheck_hook()
        end if

        call set_process_filter(spellcheck_process, st, spellcheck_filter)
        coding = spellcheck_get_coding_system()
        if (len(coding) > 0) call set_process_coding_system(spellcheck_process, coding, coding, st)
        ! Get version ID line.
        call spellcheck_accept_output(st, 3.0)
        ! Get more output if filter empty?
        if (size(spellcheck_filter_variable) == 0) call spellcheck_accept_output(st, 3.0)

        if (size(spellcheck_filter_variable) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, spellcheck_program_name//" did not output version line")
            return
        end if

        if (.not. has_version_line()) then
            ! Otherwise, it must be an error message.  Show the user.  But
            ! first wait to see if some more output is going to arrive.
            call sit_for(1000)
            if (is_process_live(spellcheck_process)) call spellcheck_accept_output(st, 3.0)
            text = ""

            do k = size(spellcheck_filter_variable), 1, -1
                text = text//spellcheck_filter_variable(k)%text
                if (k > 1) text = text//LF
            end do

            call set_error(status, GENERIC_ERROR_CODE, text)
            return
        end if

        ! Discard version ID line.
        call clear_filter()
        mode = spellcheck_get_extended_character_mode()
        if (len(mode) > 0) call spellcheck_send_string(mode//LF, st)
        call set_process_query_on_exit_flag(spellcheck_process, .false., st)

    contains

        ! Whether the output is the version line as expected (we already
        ! know it's the right version, so don't bother checking again).
        logical function has_version_line()
            integer :: n
            type(status_type) :: s

            n = size(spellcheck_filter_variable)

            if (string_match("warning: ", spellcheck_filter_variable(n)%text, s) > 0) then
                ! Was warn msg.
                call spellcheck_accept_output(s, 3.0)
                n = size(spellcheck_filter_variable)
            else if (n > 1) then
                has_version_line = .false.
                return
            end if

            has_version_line = string_match("^@(#) ", spellcheck_filter_variable(n)%text, s) > 0
        end function has_version_line
    end subroutine spellcheck_initialize_process

    ! Kill the Ispell process when the current buffer is closed (the
    ! procedure ispell-init-process adds to the buffer's own
    ! `kill-buffer-hook').
    ! Not a port.
    subroutine prepend_kill_spellcheck_hook()
        use files_mod, only: close_buffer_hook

        call close_buffer_hook%prepend(kill_spellcheck_quietly, local=.true.)
    end subroutine prepend_kill_spellcheck_hook

    ! Kill the Ispell process, without an error if there is none.
    ! Not a port.
    recursive subroutine kill_spellcheck_quietly(status)
        type(status_type), intent(out) :: status

        call spellcheck_kill_spellcheck(status, no_error=.true.)
    end subroutine kill_spellcheck_quietly

    !> Kill current Ispell process (so that you may start a fresh one).
    !> With NO_ERROR, just return if there was no Ispell running.
    !> With CLEAR, buffer session localwords are cleaned.  Port of
    !> ispell.el:ispell-kill-ispell (CLEAR is also true when called
    !> interactively).
    recursive subroutine spellcheck_kill_spellcheck(status, no_error, clear)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error, clear
        type(status_type) :: st
        logical :: quiet

        status = status_type()
        ! This hook is typically used by flyspell to flush some variables
        ! used to optimize the common cases.
        call spellcheck_kill_spellcheck_hook_variable%execute(st)

        if (present(clear)) then
            if (clear) call clear_session_localwords()
        end if

        quiet = .false.
        if (present(no_error)) quiet = no_error

        if (.not. (spellcheck_process%index /= 0 .and. spellcheck_process_status() == "run")) then
            if (.not. quiet) call set_error(status, GENERIC_ERROR_CODE, "There is no Ispell process running!")
            return
        end if

        call delete_process(spellcheck_process, status)
        spellcheck_process = null_handle()
        call message("Ispell process killed")
    end subroutine spellcheck_kill_spellcheck

    ! The command `spellcheck_kill_spellcheck'.
    recursive subroutine execute_spellcheck_kill_spellcheck(status)
        type(status_type), intent(out) :: status

        call spellcheck_kill_spellcheck(status, clear=.true.)
    end subroutine execute_spellcheck_kill_spellcheck

    ! ------------------------------------------------------------------
    ! Buffer session local words
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `session_localwords', 0 if
    ! none.  Not a port.
    integer function session_slot() result(k)
        k = 0
        if (current == 0 .or. .not. allocated(session_localwords)) return
        k = findloc(session_localwords%serial, buffers(current)%serial, dim=1)
    end function session_slot

    !> Add WORD to the front of the current buffer's
    !> `spellcheck_buffer_session_localwords', the words accepted for the
    !> session in this buffer, unless it is there already (cl-pushnew).
    !> Port of ispell.el ispell-buffer-session-localwords.
    subroutine prepend_spellcheck_buffer_session_localword(word)
        character(len=*), intent(in) :: word
        type(session_localwords_type) :: entry
        integer :: k, j

        k = session_slot()

        if (k == 0) then
            if (current == 0) return
            if (.not. allocated(session_localwords)) allocate (session_localwords(0))
            entry%serial = buffers(current)%serial
            allocate (entry%words(0))
            session_localwords = [session_localwords, entry]
            k = size(session_localwords)
        end if

        do j = 1, size(session_localwords(k)%words)
            if (session_localwords(k)%words(j)%text == word) return
        end do

        call prepend_string(session_localwords(k)%words, word)
    end subroutine prepend_spellcheck_buffer_session_localword

    !> The current buffer's `spellcheck_buffer_session_localwords' in WORDS: the
    !> words accepted for the session in this buffer, the newest first.
    !> Port of ispell.el ispell-buffer-session-localwords (its value).
    subroutine spellcheck_buffer_session_localwords(words)
        type(string_type), allocatable, intent(out) :: words(:)
        integer :: k

        k = session_slot()

        if (k == 0) then
            allocate (words(0))
        else
            words = session_localwords(k)%words
        end if
    end subroutine spellcheck_buffer_session_localwords

    ! Forget the current buffer's session local words.  Not a port.
    subroutine clear_session_localwords()
        integer :: k

        k = session_slot()
        if (k > 0) session_localwords(k)%serial = -1
    end subroutine clear_session_localwords

    ! ------------------------------------------------------------------
    ! Checking a word
    ! ------------------------------------------------------------------

    !> Check spelling of word under or before the cursor.
    !> If the word is not found in dictionary, display possible corrections
    !> in a window allowing you to choose one.
    !>
    !> If optional argument FOLLOWING is true or if `spellcheck_following_word'
    !> is true when called interactively, then the following word (rather
    !> than preceding) is checked when the cursor is not over a word.
    !> When the optional argument QUIETLY is true or `spellcheck_quietly' is
    !> true when called interactively, non-corrective messages are
    !> suppressed.
    !>
    !> With a prefix argument (or if RESUME is true (upstream's CONTINUE)), resume interrupted
    !> spell-checking of a buffer or region.
    !>
    !> Interactively, in Transient Mark mode when the mark is active, call
    !> `spellcheck_region' to check the active region for spelling errors.
    !> Non-interactively, this happens if REGION is true.
    !>
    !> Word syntax is controlled by the definition of the chosen dictionary,
    !> which is in `spellcheck_local_dictionaries' or `spellcheck_dictionaries'.
    !>
    !> This will check or reload the dictionary.  Use M-x
    !> spellcheck_change_dictionary or M-x spellcheck_region to update the Ispell
    !> process.  Port of ispell.el:ispell-word (no return value).
    recursive subroutine spellcheck_word(status, following, quietly, resume, region)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: following, quietly, resume, region
        logical :: is_following, is_quiet
        character(len=:), allocatable :: word, new_word
        integer(int64) :: cursor_location, start, end, shift
        type(spellcheck_output_type) :: positions
        type(replacement_type) :: replace
        type(window_configuration_type) :: configuration
        type(handle_type) :: choices, start_marker, end_marker
        type(status_type) :: st
        logical :: quit

        status = status_type()
        is_following = .false.
        if (present(following)) is_following = following
        is_quiet = .false.
        if (present(quietly)) is_quiet = quietly

        if (present(region)) then
            if (region .and. is_use_region()) then
                call spellcheck_region(active_region_beginning(), active_region_end(), status, quit=quit)
                return
            end if
        end if

        if (present(resume)) then
            if (resume) then
                call spellcheck_continue(status)
                return
            end if
        end if

        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        ! Use the correct dictionary.
        call spellcheck_accept_buffer_local_definitions(status)
        if (.not. status%ok()) return
        ! Retain cursor location.
        cursor_location = point()
        call spellcheck_get_word(is_following, word, start, end, status)
        if (.not. status%ok()) return
        ! At this point it used to ignore 2-letter words.  But that is
        ! silly; if the user asks for it, we should do it. - rms.
        if (.not. is_quiet) call message("Checking spelling of "//formatted_word(word)//"...")
        call spellcheck_run_on_word(word, positions, status)
        if (.not. status%ok()) return
        shift = 0
        replace = replacement_type()

        select case (positions%kind)
        case (SPELLCHECK_OUTPUT_CORRECT)
            if (.not. is_quiet) call message(formatted_word(word)//" is correct")
        case (SPELLCHECK_OUTPUT_ROOT)
            if (.not. is_quiet) call message(formatted_word(word)//" is correct because of root "// &
                                             formatted_word(positions%word))
        case (SPELLCHECK_OUTPUT_ERROR)
            call message(error_checking_word(word))
        case default
            if (spellcheck_check_only) then
                ! Called from ispell minor mode.
                call ding()
                call message(formatted_word(word)//" is incorrect")
            else
                ! Prompt for correct word.
                spellcheck_abbreviation_saving_allowed = .true.
                spellcheck_save_correction_as_abbreviation = spellcheck_save_corrections_as_abbreviations
                call make_marker(start_marker)
                call set_marker(start_marker, start, st)
                call make_marker(end_marker)
                call set_marker(end_marker, end, st)
                call current_window_configuration(configuration)
                call spellcheck_command_loop(positions%misses, positions%guesses, positions%word, start, end, replace, status)
                call set_window_configuration(configuration)
                spellcheck_abbreviation_saving_allowed = .false.
                start = marked_position(start_marker)
                end = marked_position(end_marker)

                if (.not. status%ok()) then
                    call destroy_marker(start_marker, st)
                    call destroy_marker(end_marker, st)
                    return
                end if

                if (replace%kind == REPLACE_LOCAL) then
                    call spellcheck_add_per_file_word_list(positions%word, status)
                else if (replace%kind /= REPLACE_KEEP) then
                    new_word = replace%word
                    cursor_location = cursor_location + (length_characters(word) - (end - start))
                    call spellcheck_maybe_save_correction_abbreviation(positions%word, new_word)

                    if (new_word /= positions%word) then
                        call goto_character(start)
                        ! Insert first and then delete, to avoid collapsing
                        ! markers before and after into a single place.
                        call insert(new_word, st)
                        call delete_region(point(), marked_position(end_marker), st)
                        ! It is meaningless to preserve the cursor position
                        ! inside a word that has changed.
                        cursor_location = point()
                        end = point()
                    end if

                    if (replace%kind /= REPLACE_WORD) then
                        ! Recheck spelling of replacement.
                        if (replace%kind == REPLACE_QUERY) then
                            call current_window_configuration(configuration)
                            call query_replace(word, new_word, st, delimited=.true.)
                            call set_window_configuration(configuration)
                        end if

                        call goto_character(start)
                        ! Single word could be split into multiple words.
                        call spellcheck_region(start, end, status, shift=shift, quit=quit)
                        spellcheck_quit = quit
                    end if
                end if

                ! Keep if rechecking word and we keep choices win.
                choices = get_buffer(spellcheck_choices_buffer)
                if (choices%index /= 0) call close_buffer(choices, st)
                call destroy_marker(start_marker, st)
                call destroy_marker(end_marker, st)
            end if
        end select

        call spellcheck_pdict_save(st, spellcheck_silently_save)

        ! NB: Cancels ispell-quit incorrectly if called from ispell-region.
        if (spellcheck_quit) then
            spellcheck_quit = .false.
            spellcheck_quit_position = 0
        end if

        ! Return to original location.
        call goto_character(cursor_location)
    end subroutine spellcheck_word

    ! The command `spellcheck_word' (M-$).
    recursive subroutine execute_spellcheck_word(status)
        type(status_type), intent(out) :: status

        call spellcheck_word(status, spellcheck_following_word, spellcheck_quietly, current_prefix_argument%kind /= PREFIX_NONE, &
                             .true.)
    end subroutine execute_spellcheck_word

    ! The number of characters of the UTF-8 TEXT.  Not a port.
    integer(int64) function length_characters(text) result(n)
        character(len=*), intent(in) :: text
        integer :: k

        n = 0

        do k = 1, len(text)
            if (iand(iachar(text(k:k)), 192) /= 128) n = n + 1
        end do
    end function length_characters

    !> Run ispell on WORD, the result in POSITIONS.  Port of
    !> ispell.el:ispell--run-on-word.
    subroutine spellcheck_run_on_word(word, positions, status)
        character(len=*), intent(in) :: word
        type(spellcheck_output_type), intent(out) :: positions
        type(status_type), intent(out) :: status
        integer :: n

        ! Put the speller in verbose mode.
        call spellcheck_send_string("%"//LF, status)
        call spellcheck_send_string("^"//word//LF, status)

        ! Wait until ispell has processed word.
        do
            call spellcheck_accept_output(status)
            if (.not. status%ok()) return
            if (is_answer_complete()) exit
        end do

        n = size(spellcheck_filter_variable)
        spellcheck_filter_variable = spellcheck_filter_variable(1:n - 1)
        n = n - 1

        if (n > 1) then
            call set_error(status, GENERIC_ERROR_CODE, "Ispell and its process have different character maps: "// &
                           filter_text())
            return
        end if

        if (n == 1) then
            call spellcheck_parse_output(spellcheck_filter_variable(1)%text, positions)
        else
            ! (ispell-filter empty: the word is not checked.)
            positions%kind = SPELLCHECK_OUTPUT_ERROR
        end if

    contains

        ! The output lines, newest first, as upstream prints the list.
        function filter_text() result(text)
            character(len=:), allocatable :: text
            integer :: k

            text = "("

            do k = size(spellcheck_filter_variable), 1, -1
                text = text//""""//spellcheck_filter_variable(k)%text//""""
                if (k > 1) text = text//" "
            end do

            text = text//")"
        end function filter_text
    end subroutine spellcheck_run_on_word

    ! A string describing that checking for WORD failed.  Port of
    ! ispell.el:ispell-error-checking-word.
    function error_checking_word(word) result(text)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text
        character(len=:), allocatable :: dictionary

        dictionary = spellcheck_current_dictionary
        if (len(dictionary) == 0) dictionary = "default"
        text = "Error checking word "//formatted_word(word)//" using "//file_name_nondirectory(spellcheck_program_name)// &
               " with "//dictionary//" dictionary"
    end function error_checking_word

    !> Return true if the word at point is correct, false otherwise.
    !>
    !> If optional argument FOLLOWING is true then the following word
    !> (rather than preceding) is checked when the cursor is not over a
    !> word.  Port of ispell.el:ispell-correct-p.
    logical function is_spellcheck_correct(status, following) result(correct)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: following
        character(len=:), allocatable :: word
        integer(int64) :: start, end, saved
        type(spellcheck_output_type) :: positions
        logical :: is_following

        correct = .false.
        is_following = .false.
        if (present(following)) is_following = following
        saved = point()
        ! Reset the filter so it only contains the result of spell-checking
        ! the current word.
        call clear_filter()
        call spellcheck_get_word(is_following, word, start, end, status)

        if (status%ok()) then
            call spellcheck_run_on_word(word, positions, status)

            if (status%ok()) then
                if (positions%kind == SPELLCHECK_OUTPUT_ERROR) then
                    call set_error(status, GENERIC_ERROR_CODE, error_checking_word(word))
                else
                    correct = positions%kind == SPELLCHECK_OUTPUT_CORRECT .or. positions%kind == SPELLCHECK_OUTPUT_ROOT
                end if
            end if
        end if

        call goto_character(saved)
    end function is_spellcheck_correct

    !> The word for spell-checking according to ispell syntax, from START
    !> to END.  If optional argument FOLLOWING is true or if
    !> `spellcheck_following_word' is true when called interactively, then the
    !> following word (rather than preceding) is checked when the cursor is
    !> not over a word.  Optional EXTRA_OTHERCHARS contains otherchars that
    !> can be included in word many times (see `spellcheck_dictionary_type'
    !> about otherchars).
    !>
    !> Word syntax is controlled by the definition of the chosen dictionary,
    !> which is in `spellcheck_local_dictionaries' or `spellcheck_dictionaries'.
    !> EXTRA_OTHERCHARS, if present, is the set of extra characters to add
    !> to the dictionary's OTHERCHARS.  Port of ispell.el:ispell-get-word.
    recursive subroutine spellcheck_get_word(following, word, start, end, status, extra_otherchars)
        logical, intent(in) :: following
        character(len=:), allocatable, intent(out) :: word
        integer(int64), intent(out) :: start, end
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: extra_otherchars
        character(len=:), allocatable :: casechars, not_casechars, otherchars, extra, word_regular_expression
        integer(int64) :: found, prevpt
        logical :: many, did_it_once, has_extra
        type(status_type) :: st

        word = ""
        start = point()
        end = point()
        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        call spellcheck_get_decoded_string(1, casechars, status)
        if (.not. status%ok()) return
        call spellcheck_get_decoded_string(2, not_casechars, status)
        call spellcheck_get_decoded_string(3, otherchars, status)
        many = spellcheck_get_many_otherchars()
        extra = ""
        if (present(extra_otherchars)) extra = extra_otherchars
        has_extra = present(extra_otherchars)
        word_regular_expression = casechars//"+\("
        if (len(otherchars) > 0) word_regular_expression = word_regular_expression//otherchars//"?"
        if (has_extra) word_regular_expression = word_regular_expression//extra//"?"
        word_regular_expression = word_regular_expression//casechars//"+\)"

        if (many .or. has_extra) then
            word_regular_expression = word_regular_expression//"*"
        else
            word_regular_expression = word_regular_expression//"?"
        end if

        did_it_once = .false.
        prevpt = -1

        ! Find the word.
        if (.not. looking_at(casechars, st)) then
            if (following) then
                call regular_expression_search_forward(casechars, found, st, point_maximum(), SEARCH_NOERROR)
            else
                call regular_expression_search_backward(casechars, found, st, point_minimum(), SEARCH_NOERROR)
            end if
        end if

        ! Move to front of word.
        call regular_expression_search_backward(not_casechars, found, st, point_minimum(), SEARCH_MOVE)

        do
            if (.not. (is_looking_at_other() .and. .not. is_beginning_of_buffer() .and. (.not. did_it_once .or. many) .and. &
                       prevpt /= point())) exit

            if (is_looking_at_extra()) then
                call backward_character(st)
                if (looking_at(casechars, st)) call regular_expression_search_backward(not_casechars, found, st, point_minimum(), &
                                                                                       SEARCH_MOVE)
            else
                did_it_once = .true.
                prevpt = point()
                call backward_character(st)

                if (looking_at(casechars, st)) then
                    call regular_expression_search_backward(not_casechars, found, st, point_minimum(), SEARCH_MOVE)
                else
                    call forward_character(st)
                end if
            end if
        end do

        ! Now mark the word and save to string.
        call regular_expression_search_forward(word_regular_expression, found, st, point_maximum(), SEARCH_NOERROR)

        if (.not. st%ok() .or. found == 0) then
            if (spellcheck_check_only) then
                ! Return dummy word when just flagging misspellings.
                start = point()
                end = point()
            else
                call set_error(status, USER_ERROR_CODE, "No word found to check!")
            end if

            return
        end if

        start = match_beginning(0, st)
        end = point()
        call buffer_substring_no_properties(start, end, word, st)

    contains

        ! Whether point is at OTHERCHARS or EXTRA_OTHERCHARS.
        logical function is_looking_at_other()
            type(status_type) :: s

            is_looking_at_other = .false.
            if (len(otherchars) > 0) is_looking_at_other = looking_at(otherchars, s)
            if (.not. is_looking_at_other) is_looking_at_other = is_looking_at_extra()
        end function is_looking_at_other

        ! Whether point is at EXTRA_OTHERCHARS.
        logical function is_looking_at_extra()
            type(status_type) :: s

            is_looking_at_extra = .false.
            if (has_extra) is_looking_at_extra = looking_at(extra, s)
        end function is_looking_at_extra
    end subroutine spellcheck_get_word

    !> Check to see if the personal dictionary has been modified.
    !> If so, ask if it needs to be saved.
    !> If NO_QUERY is true, save the personal dictionary without asking.
    !> Interactively, if `spellcheck_silently_save' is true, don't ask.
    !> If FORCE_SAVE is true, suggest to save the personal dictionary even if
    !> not modified; this always happens interactively.  Port of
    !> ispell.el:ispell-pdict-save.
    recursive subroutine spellcheck_pdict_save(status, no_query, force_save)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_query, force_save
        logical :: save, answer

        status = status_type()
        save = spellcheck_pdict_modified
        if (present(force_save)) save = save .or. force_save

        if (save) then
            answer = .false.
            if (present(no_query)) answer = no_query

            if (.not. answer) then
                call read_y_or_n("Personal dictionary modified.  Save? ", answer, status)
                if (.not. status%ok()) return
            end if

            if (answer) then
                ! Save dictionary.
                call spellcheck_send_string("#"//LF, status)
                call message("Personal dictionary saved.")
            end if
        end if

        ! Unassert variable, even if not saved to avoid questioning.
        spellcheck_pdict_modified = .false.
    end subroutine spellcheck_pdict_save

    ! The command `spellcheck_pdict_save'.
    recursive subroutine execute_spellcheck_pdict_save(status)
        type(status_type), intent(out) :: status

        call spellcheck_pdict_save(status, spellcheck_silently_save, .true.)
    end subroutine execute_spellcheck_pdict_save

    ! ------------------------------------------------------------------
    ! The command loop
    ! ------------------------------------------------------------------

    ! Insert the choices MISS into the current buffer as "(0) word  ",
    ! skipping COMMAND_CHARACTERS; COUNT is the number of choices.
    ! Not a port.
    subroutine insert_choices(miss, command_characters, count)
        type(string_type), intent(in) :: miss(:)
        integer, intent(in) :: command_characters(:)
        integer, intent(out) :: count
        integer :: c, skipped, k, width
        type(status_type) :: st
        character(len=:), allocatable :: text

        c = iachar("0")
        skipped = 0
        width = window_body_width(selected_window(), st)

        do k = 1, size(miss)
            if (7 + current_column() + length_characters(miss(k)%text) + merge(3, 0, c > iachar("~")) > width) then
                call insert(LF, st)
            end if

            ! Not so good if there are over 20 or 30 options, but then, if
            ! there are that many you don't want to scan them all anyway...
            do while (any(command_characters == c))
                ! Skip command characters.
                c = c + 1
                skipped = skipped + 1
            end do

            call character_to_string(int(c, int32), text, st)
            call insert("("//text//") "//miss(k)%text//"  ", st)
            c = c + 1
        end do

        count = c - iachar("0") - skipped
    end subroutine insert_choices

    !> Display possible corrections from MISS using GUESS for WORD in
    !> START..END.  MISS lists possible corrections.  GUESS lists possibly
    !> valid affix construction of WORD.  RESULT is REPLACE_KEEP to keep the
    !> word unchanged, REPLACE_LOCAL to insert locally into buffer-local
    !> dictionary, REPLACE_WORD for the chosen replacement word,
    !> REPLACE_RECHECK and REPLACE_RECHECK_HERE for a new replacement word
    !> (will be rechecked), REPLACE_QUERY when `query_replace' is to be
    !> used.  Highlight the word, which is assumed to run from START to
    !> END.  `spellcheck_quit' is set to start location to continue spell
    !> session.  Port of ispell.el:ispell-command-loop.
    recursive subroutine spellcheck_command_loop(miss, guess, word, start, end, result, status)
        type(string_type), intent(in) :: miss(:), guess(:)
        character(len=*), intent(in) :: word
        integer(int64), intent(in) :: start, end
        type(replacement_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer, allocatable :: command_characters(:)
        type(string_type), allocatable :: choices(:)
        integer :: count, number, skipped, k, width
        integer(int64) :: char
        type(handle_type) :: choices_buffer, textwin, text_buffer
        type(value_type) :: key
        character(len=:), allocatable :: answer, prompt
        logical :: abbreviation_prefix, again, yes, highlighted
        type(status_type) :: st

        status = status_type()
        command_characters = [32, iachar("i"), iachar("a"), iachar("A"), iachar("r"), iachar("R"), iachar("?"), &
                              iachar("x"), iachar("X"), iachar("q"), iachar("l"), iachar("u"), iachar("m")]
        if (spellcheck_abbreviation_saving_allowed) command_characters = [command_characters, 21]
        choices = miss
        abbreviation_prefix = .false.
        text_buffer = current_buffer()
        ! Setup the *Choices* buffer with valid data.
        call get_or_make_buffer(spellcheck_choices_buffer, choices_buffer, st)
        call set_buffer(choices_buffer, st)
        call erase_buffer(st)
        width = window_body_width(selected_window(), st)

        if (size(guess) > 0) then
            call insert("Affix rules generate and capitalize this word as shown below:"//LF//TAB, st)

            do k = 1, size(guess)
                if (4 + current_column() + length_characters(guess(k)%text) > width) call insert(LF//TAB, st)
                call insert(guess(k)%text//"    ", st)
            end do

            call insert(substitute_command_keys(LF//"Use option `i' to accept this spelling and put it in your "// &
                                                "personal dictionary."//LF), st)
        end if

        call insert_choices(choices, command_characters, count)
        call set_buffer(text_buffer, st)
        call spellcheck_update_post_hook%execute(st)
        ! Ensure word is visible.
        call sit_for(0)
        ! Display choices for misspelled word.
        textwin = selected_window()
        call spellcheck_show_choices(status)
        if (.not. status%ok()) return
        call select_window(textwin, st)
        call set_buffer(text_buffer, st)
        ! Highlight word, protecting current buffer status.
        highlighted = .not. spellcheck_highlight%is_off()
        if (highlighted) call spellcheck_highlight_spelling_error(start, end, .true.)

        ! Loop until a valid choice is made.
        do
            again = .false.
            call undo_boundary()
            prompt = "C-h or ? for more options; SPC to leave unchanged, Character to replace word"

            if (spellcheck_abbreviation_saving_allowed .and. abbreviation_prefix) then
                if (spellcheck_save_correction_as_abbreviation) then
                    prompt = prompt//" [won't save as abbrev]"
                else
                    prompt = prompt//" [will save as abbrev]"
                end if
            end if

            call message_unlogged(prompt)
            ! If the user types C-g, or generates some other non-character
            ! event, stop ispell.
            call read_key("", key, st)

            if (.not. st%ok() .or. vtag(key) /= V_INTEGER) then
                char = iachar("X")
            else
                char = xfixnum(key)
                if (char == 7) char = iachar("X")
            end if

            ! Adjust num to array offset skipping command characters.
            skipped = count_if_between(command_characters, int(char))
            number = int(char) - iachar("0") - skipped

            if (abbreviation_prefix .and. (char == iachar("r") .or. char == iachar("R") .or. (number >= 0 .and. number < count))) &
                then
                ! If the user typed `C-u' before this replacement command,
                ! then toggle abbrev saving for this correction.
                spellcheck_save_correction_as_abbreviation = .not. spellcheck_save_correction_as_abbreviation
                abbreviation_prefix = .false.
            else if (abbreviation_prefix .and. char /= 21) then
                ! If the user typed `C-u' but not before a replacement
                ! command, then nullify the effect of `C-u' for subsequent
                ! commands.
                abbreviation_prefix = .false.
            end if

            result = replacement_type()

            if (char == 32) then
                ! Accept word this time only.
                continue
            else if (char == iachar("i")) then
                ! Accept and insert word into pers dict.
                call spellcheck_send_string("*"//word//LF, st)
                ! Dictionary modified!
                spellcheck_pdict_modified = .true.
            else if (char == iachar("a") .or. char == iachar("A")) then
                ! Accept word without insert.
                call spellcheck_send_string("@"//word//LF, st)
                call prepend_spellcheck_buffer_session_localword(word)
                ! Session localwords might conflict.
                if (len(spellcheck_buffer_local_name) == 0) spellcheck_buffer_local_name = buffers(current)%name
                ! Return REPLACE_LOCAL for ispell-add buffer-local.
                if (char == iachar("A")) result%kind = REPLACE_LOCAL
            else if (char == iachar("r") .or. char == iachar("R")) then
                ! Type in replacement.
                if (spellcheck_highlight%choice == SPELLCHECK_HIGHLIGHT_BLOCK) call refresh_highlight()

                if (char == iachar("R") .or. spellcheck_query_replace_choices) then
                    call read_string("Query-replacement for "//word//": ", answer, status, initial_input=word)
                    result%kind = REPLACE_QUERY
                else
                    call read_string("Replacement for: ", answer, status, initial_input=word)
                    result%kind = REPLACE_RECHECK
                end if

                if (.not. status%ok()) exit
                result%word = answer
            else if (char == iachar("?") .or. char == help_character .or. char == 8) then
                if (spellcheck_highlight%choice == SPELLCHECK_HIGHLIGHT_BLOCK) call refresh_highlight()
                call spellcheck_help(status)
                if (.not. status%ok()) exit
                again = .true.
            else if (char == iachar("x")) then
                ! Quit and move point back.
                call spellcheck_pdict_save(st, spellcheck_silently_save)
                call message("Exited spell-checking")
                spellcheck_quit = .true.
                spellcheck_quit_position = 0
            else if (char == iachar("X")) then
                ! Quit and preserve point.
                call spellcheck_pdict_save(st, spellcheck_silently_save)
                call message(substitute_command_keys("Spell-checking suspended; use \[universal_argument] "// &
                                                     "\[spellcheck_word] to resume"))
                spellcheck_quit = .true.
                spellcheck_quit_position = start
            else if (char == iachar("q")) then
                call read_y_or_n("Really kill Ispell process? ", yes, status)
                if (.not. status%ok()) exit

                if (yes) then
                    ! Terminate process.
                    call spellcheck_kill_spellcheck(st, no_error=.true.)
                    spellcheck_quit = .true.
                    spellcheck_quit_position = 0
                    spellcheck_pdict_modified = .false.
                else
                    ! Continue if they don't quit.
                    again = .true.
                end if
            else if (char == iachar("l")) then
                if (spellcheck_highlight%choice == SPELLCHECK_HIGHLIGHT_BLOCK) call refresh_highlight()
                call read_string("Lookup string (`*' is wildcard): ", answer, status, initial_input=word)
                if (.not. status%ok()) exit
                call spellcheck_lookup_words(answer, choices, status)
                if (.not. status%ok()) exit
                call set_buffer(choices_buffer, st)
                call erase_buffer(st)
                call insert_choices(choices, command_characters, count)
                call set_buffer(text_buffer, st)
                textwin = selected_window()
                call spellcheck_show_choices(status)
                if (.not. status%ok()) exit
                call select_window(textwin, st)
                call set_buffer(text_buffer, st)
                ! Reselect from new choices.
                again = .true.
            else if (char == iachar("u")) then
                ! Insert lowercase into dictionary.
                call spellcheck_send_string("*"//downcase_string(word)//LF, st)
                ! Dictionary modified!
                spellcheck_pdict_modified = .true.
            else if (char == iachar("m")) then
                ! Type in what to insert.
                call read_string("Insert: ", answer, status, initial_input=word)
                if (.not. status%ok()) exit
                call spellcheck_send_string("*"//answer//LF, st)
                spellcheck_pdict_modified = .true.
                result%kind = REPLACE_RECHECK
                result%word = word
            else if (number >= 0 .and. number < count) then
                result%word = choices(number + 1)%text

                if (spellcheck_query_replace_choices) then
                    ! Query replace flag.
                    result%kind = REPLACE_QUERY
                else
                    result%kind = REPLACE_WORD
                end if
            else if (char == 12) then
                call redraw_display()
                again = .true.
            else if (char == 18) then
                ! This may have alignment errors if current line is edited.
                call recursive_edit_session(status)
                if (.not. status%ok()) exit
                ! Recheck starting at this word.
                result%kind = REPLACE_RECHECK_HERE
                result%word = word
            else if (char == 26) then
                call execute_key_binding(26_int64, st)
                again = .true.
            else if (char == 21 .and. spellcheck_abbreviation_saving_allowed) then
                abbreviation_prefix = .not. abbreviation_prefix
                again = .true.
            else
                call ding()
                again = .true.
            end if

            if (.not. again) exit
        end do

        ! Unhighlight.
        if (highlighted) call spellcheck_highlight_spelling_error(start, end, .false.)

    contains

        ! Refresh the highlighting (tty's block cursor).
        subroutine refresh_highlight()
            call spellcheck_highlight_spelling_error(start, end, .false., .true.)
        end subroutine refresh_highlight

        ! The C-r command: a recursive edit, coming back to START.
        recursive subroutine recursive_edit_session(stat)
            type(status_type), intent(out) :: stat
            integer(int64) :: position, saved
            type(handle_type) :: b
            type(window_configuration_type) :: configuration
            type(status_type) :: s

            stat = status_type()
            position = 0
            if (spellcheck_recursive_edit_marker%index /= 0) call marker_position(spellcheck_recursive_edit_marker, position, s)

            if (position /= 0) then
                call message("Only one recursive edit session supported")
                call ding()
                call sit_for(2000)
                return
            end if

            if (spellcheck_recursive_edit_marker%index == 0) call make_marker(spellcheck_recursive_edit_marker)
            call set_marker(spellcheck_recursive_edit_marker, start, s)
            ! Unhighlight.
            if (.not. spellcheck_highlight%is_off()) call spellcheck_highlight_spelling_error(start, end, .false.)
            call message(substitute_command_keys("Exit recursive edit with \[exit_recursive_edit]"))
            call current_window_configuration(configuration)
            saved = point()
            call recursive_edit(stat)
            call goto_character(saved)
            call set_window_configuration(configuration)
            call marker_position(spellcheck_recursive_edit_marker, position, s)
            call marker_buffer(spellcheck_recursive_edit_marker, b, s)
            call goto_character(position)

            if (b%index /= current) then
                call set_marker(spellcheck_recursive_edit_marker, 0_int64, s)
                if (stat%ok()) call set_error(stat, GENERIC_ERROR_CODE, "Cannot continue ispell from this buffer")
                return
            end if

            call set_marker(spellcheck_recursive_edit_marker, 0_int64, s)
        end subroutine recursive_edit_session
    end subroutine spellcheck_command_loop

    ! The number of COMMAND_CHARACTERS above 0 and below C.  Not a port.
    integer function count_if_between(command_characters, c) result(n)
        integer, intent(in) :: command_characters(:)
        integer, intent(in) :: c

        n = count(command_characters > iachar("0") .and. command_characters < c)
    end function count_if_between

    ! Show TEXT in the echo area without logging it (message-log-max nil).
    ! Not a port.
    subroutine message_unlogged(text)
        character(len=*), intent(in) :: text
        type(message_log_maximum_choice_setting_type) :: saved

        saved = message_log_maximum
        message_log_maximum%lines = 0
        call message(text)
        message_log_maximum = saved
    end subroutine message_unlogged

    ! Execute the command KEY is bound to.
    ! Not a port.
    recursive subroutine execute_key_binding(key, status)
        integer(int64), intent(in) :: key
        type(status_type), intent(out) :: status
        type(value_type) :: binding
        type(command_type), pointer :: command
        type(value_type) :: keys(1)

        keys(1) = value_type(key)
        call key_binding(keys, binding, status)
        if (.not. status%ok()) return
        command => command_of(binding)
        if (associated(command)) call command%execute(status)
    end subroutine execute_key_binding

    !> Show the choices in another buffer or frame.  Port of
    !> ispell.el:ispell-show-choices (no framepop).
    recursive subroutine spellcheck_show_choices(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        ! Display choices above selected window.
        call get_or_make_buffer(spellcheck_choices_buffer, buffer, status)
        if (.not. status%ok()) return
        call spellcheck_display_buffer(buffer, status)
    end subroutine spellcheck_show_choices

    !> Display a list of the options available when a misspelling is
    !> encountered.
    !>
    !> Selections are:
    !>
    !> 0..9  Replace the word with a digit offered in the *Choices* buffer.
    !> SPC Accept word this time.
    !> i   Accept word and insert into personal dictionary.
    !> a   Accept word for this session.
    !> A   Accept word and place in `buffer-local dictionary'.
    !> r   Replace word with typed-in value.  Rechecked.
    !> R   Replace word with typed-in value.  Query-replaced in buffer.
    !>     Rechecked.
    !> ?   Show these commands.
    !> x   Exit spelling buffer.  Move cursor to original point.
    !> X   Exit spelling buffer.  Leaves cursor at the current point, and
    !>     permits the aborted check to be completed later.
    !> q   Quit spelling session (Kills ispell process).
    !> l   Look up typed-in replacement in alternate dictionary.  Wildcards
    !>     okay.
    !> u   Like i, but the word is lower-cased first.
    !> m   Place typed-in value in personal dictionary, then recheck current
    !>     word.
    !> C-l Redraw screen.
    !> C-r Recursive edit.
    !> C-u Toggle abbrev saving for an immediately subsequent replacement
    !>     command.
    !> C-z Suspend Fortmacs or iconify frame.  Port of ispell.el:ispell-help
    !> (no electric help).
    recursive subroutine spellcheck_help(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: help_1, help_2, help_3, help_4, text
        type(window_configuration_type) :: configuration
        type(handle_type) :: buffer, old, mini
        type(status_type) :: st

        status = status_type()
        help_1 = "[r/R]eplace word; [a/A]ccept for this session; [i]nsert into personal dictionary"
        help_2 = "[l]ook a word up in alternate dictionary;  e[x/X]it;  [q]uit session"
        help_3 = "[u]ncapitalized insert into dict.  "
        if (spellcheck_abbreviation_saving_allowed) help_3 = help_3//"C-u toggles abbrev saving (next replacement)."
        help_4 = "Type `x C-h f spellcheck_help' for more help"
        text = help_1//LF//help_2//LF//help_3//LF//help_4
        call current_window_configuration(configuration)
        old = current_buffer()

        if (spellcheck_help_in_buffer) then
            call get_or_make_buffer("*Ispell Help*", buffer, st)
            call set_buffer(buffer, st)
            call insert(text, st)
            call set_buffer(old, st)
            call spellcheck_display_buffer(buffer, status)
            call sit_for(int(1000*max(0.5, spellcheck_help_timeout)))
            call close_buffer(buffer, st)
        else
            mini = windows(selected_frame%minibuffer_window%index)%buffer
            call set_buffer(mini, st)
            call erase_buffer(st)
            call message("")
            call window_resize(selected_frame%minibuffer_window, 2, .false., st)
            call insert(text, st)
            call sit_for(int(1000*max(0.5, spellcheck_help_timeout)))
            call erase_buffer(st)
        end if

        call set_window_configuration(configuration)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine spellcheck_help

    !> Look up WORD in optional word-list dictionary LOOKUP_DICTIONARY, the words
    !> found in RESULTS.  A `*' serves as a wild card.  If no wild cards,
    !> `look' is used if it exists.  Otherwise the variable
    !> `spellcheck_grep_command' contains the command (usually "grep") used to
    !> search for the words.
    !>
    !> Optional second argument contains the dictionary to use; the default
    !> is `spellcheck_alternate_dictionary', overridden by
    !> `spellcheck_complete_word_dictionary' if defined.  If none of LOOKUP_DICTIONARY,
    !> `spellcheck_alternate_dictionary', and `spellcheck_complete_word_dictionary' name an
    !> existing word-list file, an error condition occurs.  Port of
    !> ispell.el:ispell-lookup-words.
    subroutine spellcheck_lookup_words(word, results, status, lookup_dictionary)
        character(len=*), intent(in) :: word
        type(string_type), allocatable, intent(out) :: results(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: lookup_dictionary
        character(len=:), allocatable :: dictionary, programming, arguments, pattern, line
        type(value_type) :: code, arglist
        type(handle_type) :: old, temporary
        logical :: wild, look, has_character
        integer(int32) :: c
        integer(int64) :: location, shortage
        integer :: k
        type(status_type) :: st

        allocate (results(0))
        dictionary = ""
        if (present(lookup_dictionary)) dictionary = lookup_dictionary

        if (len(dictionary) == 0) then
            dictionary = spellcheck_complete_word_dictionary
            if (len(dictionary) == 0) dictionary = spellcheck_alternate_dictionary
        end if

        if (len(dictionary) > 0) then
            if (.not. is_file_readable(dictionary)) then
                call set_error(status, GENERIC_ERROR_CODE, "ispell-lookup-words: Unreadable or missing plain word-list "// &
                               dictionary)
                return
            end if
        else
            call set_error(status, GENERIC_ERROR_CODE, "ispell-lookup-words: No plain word-list found at system "// &
                           "default locations.  Customize `spellcheck_alternate_dictionary' to set yours.")
            return
        end if

        wild = index(word, "*") > 0
        look = .false.

        if (len(spellcheck_look_command) > 0 .and. .not. wild) then
            ! Only use look for an exact match.
            look = is_file_readable(spellcheck_look_command)
        end if

        if (look) then
            programming = spellcheck_look_command
            arguments = spellcheck_look_options
            pattern = word
        else
            programming = spellcheck_grep_command
            arguments = spellcheck_grep_options
            pattern = "^"//word
            ! When there are no wildcards, append one, for consistency with
            ! `look' behavior.
            if (.not. wild) pattern = pattern//"*"
            pattern = pattern//"$"
            line = ""

            ! Convert * to .*
            do k = 1, len(pattern)
                if (pattern(k:k) == "*") line = line//"."
                line = line//pattern(k:k)
            end do

            pattern = line
        end if

        call temporary_begin(old, temporary)
        call message("Starting """//file_name_nondirectory(programming)//""" process...")

        if (len(arguments) > 0) then
            arglist = list(value_type(text=arguments), value_type(text=pattern), value_type(text=dictionary))
        else if (look) then
            arglist = list(value_type(text=pattern), value_type(text=dictionary))
        else
            arglist = list(value_type(text="-e"), value_type(text=pattern), value_type(text=dictionary))
        end if

        call spellcheck_execute_process(programming, arglist, code, status)

        if (status%ok() .and. vtag(code) == V_STRING) then
            ! `grep' returns status 1 and no output when word not found,
            ! which is a perfectly normal thing.
            call set_error(status, GENERIC_ERROR_CODE, "Error: "//file_name_nondirectory(programming)//" exited with signal "// &
                           xstring(code))
        end if

        if (status%ok()) then
            ! Collect words into RESULTS in FIFO order.
            call goto_character(point_maximum())

            ! Assure we've ended with \n.
            if (.not. is_beginning_of_buffer()) then
                call character_after(c, has_character, point() - 1)
                if (c /= 10) call insert(LF, st)
            end if

            do while (.not. is_beginning_of_buffer())
                location = point()
                shortage = forward_line(-1_int64)
                call buffer_substring_no_properties(point(), location - 1, line, st)
                call prepend_string(results, line)
            end do
        end if

        call temporary_end(old, temporary)
        if (.not. status%ok()) return

        if (size(results) > 0) then
            if (string_match(".+: ", results(1)%text, st) > 0) then
                call set_error(status, GENERIC_ERROR_CODE, spellcheck_grep_command//" error: "//results(1)%text)
            end if
        end if
    end subroutine spellcheck_lookup_words

    !> Highlight the word from START to END using overlays.
    !> When HIGHLIGHT is true, the word is highlighted, otherwise it is
    !> displayed normally.
    !>
    !> The variable `spellcheck_highlight_face' selects the face to use for
    !> highlighting.  Port of
    !> ispell.el:ispell-highlight-spelling-error-overlay (no lazy
    !> highlighting).
    subroutine spellcheck_highlight_spelling_error_overlay(start, end, highlight)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: highlight
        type(status_type) :: st

        if (highlight) then
            if (is_overlay(spellcheck_overlay)) then
                call move_overlay(spellcheck_overlay, start, end, st, current_buffer())
            else
                call make_overlay(spellcheck_overlay, start, end, st)
                ! Higher than lazy overlays.
                call overlay_put(spellcheck_overlay, "priority", value_type(1001_int64), st)
                call overlay_put(spellcheck_overlay, "face", value_type(name=trim(spellcheck_highlight_face%name)), st)
            end if
        else if (is_overlay(spellcheck_overlay)) then
            call delete_overlay(spellcheck_overlay)
            spellcheck_overlay = null_handle()
        end if
    end subroutine spellcheck_highlight_spelling_error_overlay

    !> Highlight spelling error between START and END.
    !> HIGHLIGHT true means to highlight the misspelling.  REFRESH is for
    !> the highlighting without colors.  Port of
    !> ispell.el:ispell-highlight-spelling-error (on a terminal without
    !> colors, ispell-highlight-spelling-error-generic is Deferred).
    subroutine spellcheck_highlight_spelling_error(start, end, highlight, refresh)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: highlight
        logical, intent(in), optional :: refresh

        if (present(refresh)) then
            if (refresh) return
        end if

        if (is_terminal_display_color(selected_frame%terminal)) call spellcheck_highlight_spelling_error_overlay(start, end, &
                                                                                                                 highlight)
    end subroutine spellcheck_highlight_spelling_error

    !> Show BUFFER in new window above selected one.
    !> Also position fit window to BUFFER and select it.  Port of
    !> ispell.el:ispell-display-buffer (frames are splittable).
    recursive subroutine spellcheck_display_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        type(status_type) :: st

        status = status_type()
        window = get_buffer_window(buffer)

        if (window%index == 0) then
            call split_window(selected_window(), -spellcheck_choices_window_default_height, .true., .false., window, st, &
                              before=.true.)
            if (.not. st%ok()) window = null_handle()
        end if

        if (window%index == 0) call display_buffer(buffer, window, st)

        if (window%index == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Couldn't make window for *Choices*")
            return
        end if

        call select_window(window, status)
        if (.not. status%ok()) return
        call set_window_buffer(window, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, st)
        call goto_character(point_minimum())
        call fit_window_to_buffer(window)
    end subroutine spellcheck_display_buffer

    ! ------------------------------------------------------------------
    ! Dictionaries
    ! ------------------------------------------------------------------

    !> Change to dictionary DICTIONARY for Ispell.
    !> If ARGUMENT is true (interactively, the prefix arg), set it "globally",
    !> for all buffers.  Otherwise, set it "locally", just for this buffer.
    !>
    !> By just answering RET you can find out the name of the current
    !> dictionary.  Port of ispell.el:ispell-change-dictionary.
    recursive subroutine spellcheck_change_dictionary(dictionary, status, argument, called_interactively)
        character(len=*), intent(in) :: dictionary
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument, called_interactively
        character(len=:), allocatable :: name, current_name
        logical :: global
        type(status_type) :: st

        global = .false.
        if (present(argument)) global = argument
        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return
        if (.not. global) call spellcheck_buffer_local_dictionary(status, no_reload=.true.)
        name = dictionary
        if (name == "default") name = ""
        current_name = ""
        if (.not. global) current_name = string_value(spellcheck_local_dictionary)
        if (len(current_name) == 0) current_name = spellcheck_dictionary
        if (len(current_name) == 0) current_name = "default"

        if (len(dictionary) == 0) then
            ! This relies on completing-read's bug of returning "" for no
            ! match.
            call spellcheck_internal_change_dictionary()
            call message("Using "//current_name//" dictionary")
            call spellcheck_change_dictionary_hook%execute(st)
        else if (name == current_name) then
            ! Specified dictionary is the default already.  Could reload
            ! the dictionaries if needed.
            call spellcheck_internal_change_dictionary()

            if (present(called_interactively)) then
                if (called_interactively) call message("No change, using "//name//" dictionary")
            end if
        else
            ! Reset dictionary!
            if (find_dictionary(spellcheck_local_dictionaries, name) == 0 .and. &
                find_dictionary(spellcheck_dictionaries, name) == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Undefined dictionary: "//name)
                return
            end if

            if (global) then
                ! Set default dictionary.
                spellcheck_dictionary = name
            else
                ! Set local dictionary.
                if (len(name) == 0) then
                    call spellcheck_local_dictionary%set_local("")
                    call spellcheck_local_dictionary%clear()
                else
                    call spellcheck_local_dictionary%set_local(name)
                end if

                call spellcheck_local_dictionary_overridden%set_local(.true.)
            end if

            call spellcheck_internal_change_dictionary()
            call clear_session_localwords()

            if (global) then
                call message("Global Ispell dictionary set to "//dictionary)
            else
                call message("Local Ispell dictionary set to "//dictionary)
            end if

            call spellcheck_change_dictionary_hook%execute(st)
        end if
    end subroutine spellcheck_change_dictionary

    ! The command `spellcheck_change_dictionary'.
    recursive subroutine execute_spellcheck_change_dictionary(status)
        type(status_type), intent(out) :: status
        character(len=64), allocatable :: names(:)
        type(completion_table_type) :: table
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: dictionary
        integer :: k

        call spellcheck_valid_dictionary_list(names, status)
        if (.not. status%ok()) return
        allocate (items(size(names)))

        do k = 1, size(names)
            items(k) = value_type(text=trim(names(k)))
        end do

        table%kind = TABLE_LIST
        table%list = make_list(items)
        call completing_read("Use new dictionary (RET for current, SPC to complete): ", table, dictionary, status, &
                             require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call spellcheck_change_dictionary(dictionary, status, current_prefix_argument%kind /= PREFIX_NONE, .true.)
    end subroutine execute_spellcheck_change_dictionary

    !> Update the dictionary and the personal dictionary used by Ispell.
    !> This may kill the Ispell process; if so, a new one will be started
    !> when needed.  Port of ispell.el:ispell-internal-change-dictionary.
    recursive subroutine spellcheck_internal_change_dictionary()
        character(len=:), allocatable :: dictionary, pdict, expanded
        type(status_type) :: st

        dictionary = string_value(spellcheck_local_dictionary)
        if (len(dictionary) == 0) dictionary = spellcheck_dictionary
        pdict = string_value(spellcheck_local_pdict)
        if (len(pdict) == 0) pdict = spellcheck_personal_dictionary
        expanded = ""
        if (len(pdict) > 0) call expand_file_name(pdict, expanded, st)
        if (spellcheck_current_dictionary == dictionary .and. spellcheck_current_personal_dictionary == expanded) return
        call spellcheck_kill_spellcheck(st, no_error=.true.)
        spellcheck_current_dictionary = dictionary
        spellcheck_current_personal_dictionary = expanded
    end subroutine spellcheck_internal_change_dictionary

    ! ------------------------------------------------------------------
    ! Checking regions
    ! ------------------------------------------------------------------

    !> Interactively check region between REG_START and REG_END for
    !> spelling errors.  Leave the mark at the last misspelled word that
    !> the user was queried about.
    !>
    !> QUIT is true if the spell session was terminated; otherwise SHIFT is
    !> the shift offset amount for the last line processed.  Port of
    !> ispell.el:ispell-region.
    recursive subroutine spellcheck_region(reg_start, reg_end, status, recheck, shift, quit)
        integer(int64), intent(in) :: reg_start, reg_end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recheck
        integer(int64), intent(inout), optional :: shift
        logical, intent(out), optional :: quit
        type(handle_type) :: skip_region_start, rstart, choices
        character(len=:), allocatable :: region_type, program_basename, dictionary, key, in_comment, add_comment, &
                                         string, skip_regular_expression
        integer(int64) :: maximum_word, found, region_end_position, end, saved_point, position, spellcheck_start, spellcheck_end, &
                          my_shift, limit
        type(window_configuration_type) :: configuration
        logical :: recheckp, saved_fold, has_line, has_maximum
        type(status_type) :: st

        status = status_type()
        recheckp = .false.
        if (present(recheck)) recheckp = recheck
        my_shift = 0
        if (present(shift)) my_shift = shift
        if (present(quit)) quit = .false.
        ! Initialize variables and dicts alists.
        call spellcheck_set_spellchecker_parameters(status)
        if (.not. status%ok()) return

        ! Set up dictionary, local words, etc.
        if (.not. recheckp) then
            call spellcheck_accept_buffer_local_definitions(status)
            if (.not. status%ok()) return
        end if

        call make_marker(skip_region_start)
        call make_marker(rstart)

        if (reg_start == point_minimum() .and. reg_end == point_maximum()) then
            region_type = buffers(current)%name
        else
            region_type = "region"
        end if

        program_basename = file_name_nondirectory(spellcheck_program_name)
        dictionary = spellcheck_current_dictionary
        if (len(dictionary) == 0) dictionary = "default"
        has_maximum = .false.
        maximum_word = 0
        saved_point = point()
        call message("Spell-checking "//region_type//" using "//program_basename//" with "//dictionary// &
                     " dictionary...")
        ! Returns cursor to original location.
        call current_window_configuration(configuration)
        call goto_character(reg_start)
        saved_fold = case_fold_search%value
        in_comment = ""
        key = ""
        skip_regular_expression = spellcheck_begin_skip_region_regular_expression()
        call regular_expression_search_forward(skip_regular_expression, found, st, reg_end, SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            key = match_string_no_properties(0)
            call set_marker(skip_region_start, point() - length_characters(key), st)
            call goto_character(reg_start)
        end if

        call set_marker(rstart, reg_start, st)
        if (spellcheck_region_end%index == 0) call make_marker(spellcheck_region_end)
        call set_marker(spellcheck_region_end, reg_end, st)
        end = reg_end

        do while (.not. spellcheck_quit .and. status%ok())
            region_end_position = marked_position(spellcheck_region_end)
            if (point() >= region_end_position) exit
            position = marked_position(skip_region_start)

            ! Spell-check region with skipping.
            if (position /= 0 .and. position <= point()) then
                ! If region inside line comment, must keep comment start.
                in_comment = ""

                if (allocated(comment_start%value)) then
                    if (string_value(comment_end) == "") then
                        limit = point()
                        call beginning_of_line()
                        call regular_expression_search_forward(comment_start%value, found, st, limit, SEARCH_NOERROR)
                        if (st%ok() .and. found > 0) in_comment = comment_start%value
                        call goto_character(limit)
                    end if
                end if

                call goto_character(position)
                ! Can change skip-regexps (in ispell-message).  Moves point
                ! past region.
                call spellcheck_skip_region(key, status)
                if (.not. status%ok()) exit
                call set_marker(rstart, point(), st)

                if (marked_position(rstart) < marked_position(spellcheck_region_end)) then
                    call regular_expression_search_forward(spellcheck_begin_skip_region_regular_expression(), found, st, &
                                                           marked_position(spellcheck_region_end), &
                                                           SEARCH_NOERROR)
                else
                    found = 0
                end if

                if (st%ok() .and. found > 0) then
                    key = match_string_no_properties(0)
                    call set_marker(skip_region_start, point() - length_characters(key), st)
                    call goto_character(marked_position(rstart))
                else
                    call set_marker(skip_region_start, 0_int64, st)
                end if
            end if

            position = marked_position(skip_region_start)

            if (position /= 0) then
                end = max(point(), min(position, marked_position(spellcheck_region_end)))
            else
                end = max(point(), marked_position(spellcheck_region_end))
            end if

            spellcheck_start = point()
            spellcheck_end = min(line_end_position(), end)
            ! See if line must be prefixed by comment string to let ispell
            ! know this is part of a comment string.  This is only
            ! supported in some modes.  In particular, this is not
            ! supported in autoconf mode where adding the comment string
            ! messes everything up because ispell tries to spellcheck the
            ! `dnl' string header causing misalignments in some cases
            ! (debbugs.gnu.org: #12768).
            add_comment = ""
            if (len(in_comment) > 0 .and. in_comment /= "dnl ") add_comment = in_comment
            call spellcheck_get_line(spellcheck_start, spellcheck_end, add_comment, string, has_line)

            if (len(add_comment) > 0) then
                ! Account for comment chars added.
                spellcheck_start = spellcheck_start - length_characters(add_comment)
                ! Reset `in_comment' (and indirectly `add_comment') for new
                ! line.
                in_comment = ""
            end if

            ! "end" tracks region retrieved.
            spellcheck_end = point()

            ! There is something to spell check!
            if (has_line) then
                if (.not. recheckp) my_shift = 0
                call spellcheck_process_line(string, spellcheck_start, spellcheck_end, my_shift, found, status)

                if (found > 0) then
                    maximum_word = found
                    has_maximum = .true.
                end if
            end if

            call goto_character(spellcheck_end)
        end do

        case_fold_search = saved_fold
        call goto_character(saved_point)
        call set_window_configuration(configuration)

        ! (The protected part.)
        if (.not. (recheckp .and. spellcheck_keep_choices_window)) then
            choices = get_buffer(spellcheck_choices_buffer)
            if (choices%index /= 0) call close_buffer(choices, st)
        end if

        call destroy_marker(skip_region_start, st)
        call destroy_marker(rstart, st)
        ! Allow the user to pop back to the last position.
        if (has_maximum) call push_mark(maximum_word, .true.)

        if (spellcheck_quit) then
            if (present(quit)) quit = .true.

            ! Preserve or clear the region for ispell-continue.
            if (spellcheck_quit_position == 0) then
                call set_marker(spellcheck_region_end, 0_int64, st)
            else
                ! Ispell-continue enabled - ispell-region-end is set.
                call goto_character(spellcheck_quit_position)
            end if

            if (.not. recheckp) then
                spellcheck_quit = .false.
                spellcheck_quit_position = 0
            end if
        else
            if (.not. recheckp) call set_marker(spellcheck_region_end, 0_int64, st)

            if (status%ok()) then
                ! Only save if successful exit.
                call spellcheck_pdict_save(st, spellcheck_silently_save)
                call message("Spell-checking "//region_type//" using "//program_basename//" with "//dictionary// &
                             " dictionary...done")
            end if
        end if

        if (present(shift)) shift = my_shift
    end subroutine spellcheck_region

    ! The command `spellcheck_region'.
    recursive subroutine execute_spellcheck_region(status)
        type(status_type), intent(out) :: status

        call spellcheck_region(active_region_beginning(), active_region_end(), status)
    end subroutine execute_spellcheck_region

    ! The beginning of the region (region-beginning).  Not a port.
    integer(int64) function active_region_beginning() result(position)
        type(status_type) :: st

        call region_beginning(position, st)
        if (.not. st%ok()) position = point()
    end function active_region_beginning

    ! The end of the region (region-end).  Not a port.
    integer(int64) function active_region_end() result(position)
        type(status_type) :: st

        call region_end(position, st)
        if (.not. st%ok()) position = point()
    end function active_region_end

    ! The position of marker M, 0 if it points nowhere.  Not a port.
    integer(int64) function marked_position(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        position = 0
        if (m%index == 0) return
        call marker_position(m, position, st)
        if (.not. st%ok()) position = 0
    end function marked_position

    !> Return a regexp of the search keys for region skipping.
    !> Includes `spellcheck_skip_regions' plus tex, tib, html, and comment keys.
    !> Must be called after `spellcheck_buffer_local_parsing' due to dependence
    !> on mode.  Port of ispell.el:ispell-begin-skip-region-regexp.
    function spellcheck_begin_skip_region_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression
        integer :: k

        regular_expression = ""
        k = 0
        ! Tex.
        if (spellcheck_parser%value == SPELLCHECK_PARSER_TEX) call add(spellcheck_begin_tex_skip_regular_expression())
        ! Html stuff.
        if (.not. spellcheck_skip_html%is_off()) call add(spellcheck_begin_skip_region(spellcheck_html_skip_regions))
        ! Tib.
        if (spellcheck_skip_tib) call add(spellcheck_tib_reference_beginning)

        ! Comments.
        if (spellcheck_check_comments%choice == SPELLCHECK_CHECK_COMMENTS_EXCLUSIVE .and. allocated(comment_start%value)) then
            ! Search from end of current comment to start of next comment.
            if (string_value(comment_end) == "") then
                call add("^")
            else
                call add(regular_expression_quote(comment_end%value))
            end if
        end if

        if (spellcheck_check_comments%is_off() .and. allocated(comment_start%value)) then
            call add(regular_expression_quote(comment_start%value))
        end if

        if (size(spellcheck_skip_regions) > 0) call add(spellcheck_begin_skip_region(spellcheck_skip_regions))
        call add(spellcheck_make_filename_or_url_regular_expression())

    contains

        ! Add PART, separated by "\|".
        subroutine add(part)
            character(len=*), intent(in) :: part

            if (len(regular_expression) > 0 .or. k > 0) then
                regular_expression = regular_expression//"\|"//part
            else
                regular_expression = part
            end if

            k = k + 1
        end subroutine add
    end function spellcheck_begin_skip_region_regular_expression

    !> Regular expression for start of regions to skip generated from
    !> SKIPS.  Each selection should be a key of SKIPS; otherwise, the
    !> current line is skipped.  Port of ispell.el:ispell-begin-skip-region.
    function spellcheck_begin_skip_region(skips) result(regular_expression)
        type(spellcheck_skip_region_type), intent(in) :: skips(:)
        character(len=:), allocatable :: regular_expression
        integer :: k

        regular_expression = ""

        do k = 1, size(skips)
            if (k > 1) regular_expression = regular_expression//"\|"
            regular_expression = regular_expression//trim(skips(k)%beginning_regular_expression)
        end do
    end function spellcheck_begin_skip_region

    !> Regular expression of tex commands to skip.
    !> Generated from `spellcheck_tex_skip_regions' and
    !> `spellcheck_tex_skip_environments'.  Port of
    !> ispell.el:ispell-begin-tex-skip-regexp.
    function spellcheck_begin_tex_skip_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression
        integer :: k

        ! Raw tex keys.
        regular_expression = spellcheck_begin_skip_region(spellcheck_tex_skip_regions)//"\|"

        ! Keys wrapped in begin{}.
        do k = 1, size(spellcheck_tex_skip_environments)
            if (k > 1) regular_expression = regular_expression//"\|"
            regular_expression = regular_expression//"\\begin[ "//TAB//LF//"]*{"//trim(spellcheck_tex_skip_environments( &
                                                                                       k)%beginning_regular_expression)//"}"
        end do
    end function spellcheck_begin_tex_skip_regular_expression

    !> Return the key and body regions to skip for this buffer.
    !> Includes regions defined by `spellcheck_skip_regions', tex mode, and
    !> `spellcheck_html_skip_regions'.  Manual checking must include comments
    !> and tib references.  Must be called after
    !> `spellcheck_buffer_local_parsing' due to dependence on mode.  Port of
    !> ispell.el:ispell-skip-region-list (no `ispell-checking-message').
    subroutine spellcheck_skip_region_list(skips)
        type(spellcheck_skip_region_type), allocatable, intent(out) :: skips(:)
        type(spellcheck_skip_region_type) :: url

        url%beginning_regular_expression = spellcheck_make_filename_or_url_regular_expression()
        skips = [url, spellcheck_skip_regions]

        ! Only additional explicit region definition is tex.
        if (spellcheck_parser%value == SPELLCHECK_PARSER_TEX) then
            case_fold_search = .false.
            skips = [spellcheck_tex_skip_regions, spellcheck_tex_skip_environments, skips]
        end if

        if (.not. spellcheck_skip_html%is_off()) skips = [spellcheck_html_skip_regions, skips]
    end subroutine spellcheck_skip_region_list

    !> Return CHAR as a string when CHAR doesn't have "word" syntax, ""
    !> otherwise.  Port of ispell.el:ispell--\\w-filter.
    function spellcheck_w_filter(char) result(text)
        character(len=*), intent(in) :: char
        character(len=:), allocatable :: text
        type(status_type) :: st

        text = ""
        if (string_match("\w", char, st) == 0) text = char
    end function spellcheck_w_filter

    !> Make a regular expression like "\(\w\|[-_]\)".
    !> This (parenthesized) expression matches either a character of "word"
    !> syntax or one in CHARS.
    !>
    !> CHARS is a string of characters.  A member of CHARS is omitted from
    !> the expression if it already has word syntax.  (Be careful about
    !> special characters such as \, ^, ], and - in CHARS.)  If after this
    !> filtering there are no chars left, or only one, a special form of the
    !> expression is generated.  Port of
    !> ispell.el:ispell--make-\\w-expression.
    function spellcheck_make_w_expression(chars) result(regular_expression)
        character(len=*), intent(in) :: chars
        character(len=:), allocatable :: regular_expression
        character(len=:), allocatable :: filtered
        integer :: k

        filtered = ""

        do k = 1, len(chars)
            filtered = filtered//spellcheck_w_filter(chars(k:k))
        end do

        regular_expression = "\(\w"

        if (len(filtered) == 0) then
            regular_expression = regular_expression//"\)"
        else if (len(filtered) == 1) then
            regular_expression = regular_expression//"\|"//filtered//"\)"
        else
            regular_expression = regular_expression//"\|["//filtered//"]\)"
        end if
    end function spellcheck_make_w_expression

    !> Construct a regexp to match some file names or URLs or email
    !> addresses.  The expression is crafted to match as great a variety of
    !> these objects as practicable, without too many false matches
    !> happening.  Port of ispell.el:ispell--make-filename-or-URL-re.
    function spellcheck_make_filename_or_url_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "\(/\w\|\("//spellcheck_make_w_expression("-_")//"+[.:@]\)\)"//spellcheck_make_w_expression("-_")// &
                             "*\([.:/@]+"//spellcheck_make_w_expression("-_~=?&")//"+\)+"
    end function spellcheck_make_filename_or_url_regular_expression

    !> Skip across ARGUMENT number of braces.  Port of
    !> ispell.el:ispell-tex-arg-end.
    recursive subroutine spellcheck_tex_argument_end(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=64) :: text

        status = status_type()

        do while (looking_at("[ "//TAB//LF//"]*\[", st))
            call forward_expression(1_int64, st)
            if (.not. st%ok()) exit
        end do

        if (st%ok()) call forward_expression(int(argument, int64), st)

        if (.not. st%ok()) then
            write (text, '("Error skipping expressions at point ", i0, ".")') point()
            call message(trim(text))
            call ding()
            call sit_for(2000)
        end if
    end subroutine spellcheck_tex_argument_end

    ! forward-line as a function of a region to skip.  Not a port.
    recursive subroutine skip_line(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        status = status_type()
        shortage = forward_line(int(argument, int64))
    end subroutine skip_line

    !> Skip across KEY and then to end of region.
    !> Key lookup determines region to skip.
    !> Point is placed at end of skipped region.  Port of
    !> ispell.el:ispell-skip-region.
    recursive subroutine spellcheck_skip_region(key, status)
        character(len=*), intent(in) :: key
        type(status_type), intent(out) :: status
        type(spellcheck_skip_region_type), allocatable :: skips(:)
        integer(int64) :: start, found, shortage
        logical :: null_skip
        type(status_type) :: st
        character(len=40) :: point_text
        integer :: k

        status = status_type()
        ! Move over key to begin checking.
        call forward_character(st, length_characters(key))
        start = point()
        ! Regenerate each call... This function can change region
        ! definition.
        call spellcheck_skip_region_list(skips)
        null_skip = .false.

        if (spellcheck_check_comments%is_off() .and. allocated(comment_start%value) .and. &
            key == string_value(comment_start)) then
            ! What about quoted comment, or comment inside strings?
            if (string_value(comment_end) == "") then
                shortage = forward_line(1_int64)
            else
                call search_forward(comment_end%value, found, st, marked_position(spellcheck_region_end), SEARCH_NOERROR)
            end if
        else if (spellcheck_check_comments%choice == SPELLCHECK_CHECK_COMMENTS_EXCLUSIVE .and. allocated(comment_start%value) &
                 .and. &
                 key == string_value(comment_end)) then
            call search_forward(comment_start%value, found, st, marked_position(spellcheck_region_end), SEARCH_MOVE)
        else if (spellcheck_skip_tib .and. string_match(spellcheck_tib_reference_beginning, key, st) > 0) then
            call regular_expression_search_forward(spellcheck_tib_reference_end, found, st, &
                                                   marked_position(spellcheck_region_end), SEARCH_NOERROR)
        else
            ! Markings from the list.
            do k = 1, size(skips)
                if (string_match(trim(skips(k)%beginning_regular_expression), key, st) == 0) cycle

                if (associated(skips(k)%skip)) then
                    ! Error handling in functions!
                    null_skip = .true.
                    call skips(k)%skip(skips(k)%argument, st)
                else if (len_trim(skips(k)%end_regular_expression) > 0) then
                    ! Search past end of spell region to find this region
                    ! end.
                    call regular_expression_search_forward(trim(skips(k)%end_regular_expression), found, st, point_maximum(), &
                                                           SEARCH_NOERROR)
                else
                    ! Done!  Just skip key.
                    null_skip = .true.
                end if

                exit
            end do
        end if

        if (start == point() .and. .not. null_skip) then
            write (point_text, '(" point ", i0, " not found")') point()
            call message("Matching region end for `"//key//"'"//trim(point_text))
            call ding()
            call sit_for(2000)
        end if
    end subroutine spellcheck_skip_region

    !> Grab the next line of text between START and END into STRING (HAS
    !> false when there is none), prefixed with IN_COMMENT.  Port of
    !> ispell.el:ispell-get-line.
    subroutine spellcheck_get_line(start, end, in_comment, string, has)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: in_comment
        character(len=:), allocatable, intent(out) :: string
        logical, intent(out) :: has
        character(len=:), allocatable :: casechars, text
        integer(int64) :: found, shortage
        type(status_type) :: st

        has = .false.
        string = ""
        casechars = spellcheck_get_casechars()

        ! Look at this line and skip or process.
        if (is_end_of_line()) then
            ! End of line, just go to next line.
            shortage = forward_line(1_int64)
            return
        end if

        call regular_expression_search_forward(casechars, found, st, end, SEARCH_NOERROR)
        ! Or math commands.
        if (.not. (st%ok() .and. found > 0)) call regular_expression_search_forward("[][()${}]", found, st, end, SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            ! Text exists.
            call buffer_substring_no_properties(start, end, text, st)
            string = "^"//in_comment//text//LF
            has = .true.
        end if

        ! Empty line, skip it.
        call goto_character(end)
    end subroutine spellcheck_get_line

    !> Whether the text at point is STRING, compared in the dictionary's
    !> coding system.  Port of ispell.el:ispell-looking-at.
    logical function spellcheck_looking_at(string) result(matched)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: coding, text, a, b
        integer(int64) :: n
        type(status_type) :: st

        matched = .false.
        coding = spellcheck_get_coding_system()
        n = length_characters(string)
        if (point() + n > point_maximum()) return
        call buffer_substring_no_properties(point(), point() + n, text, st)
        call encode_coding_string(string, coding, a, st)
        call encode_coding_string(text, coding, b, st)
        matched = a == b
    end function spellcheck_looking_at

    !> Send STRING, a line of text, to ispell and process the result.
    !> This will modify the buffer for spelling errors.  SPELLCHECK_START and
    !> SPELLCHECK_END are the line's bounds (upstream's ispell-start and ispell-end, saved and
    !> restored by its callers).
    !>
    !> SHIFT is the sum of the shifts due to changes in word replacements,
    !> MAXIMUM_WORD the location of the final word that was queried about (0
    !> if none).  Port of ispell.el:ispell-process-line.
    recursive subroutine spellcheck_process_line(string, spellcheck_start, spellcheck_end, shift, maximum_word, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(inout) :: spellcheck_start, spellcheck_end
        integer(int64), intent(inout) :: shift
        integer(int64), intent(out) :: maximum_word
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: accept_list(:), recheck_filter(:), lines(:)
        type(spellcheck_output_type) :: positions
        type(replacement_type) :: replace
        type(handle_type) :: word_start, line_end, line_start, region_end
        type(window_configuration_type) :: configuration
        integer(int64) :: word_length, word_position, sub_shift, actual
        logical :: recheck_region, sub_quit
        type(status_type) :: st
        character(len=64) :: point_text
        integer :: n

        status = status_type()
        maximum_word = 0
        allocate (accept_list(0))
        ! Send string to spell process and get input.
        call spellcheck_send_string(string, status)

        do
            call spellcheck_accept_output(status)
            if (.not. status%ok()) return
            ! Last item of output contains a blank line.
            if (is_answer_complete()) exit
        end do

        ! Parse all inputs from the stream one word at a time.  Remove the
        ! blank item.
        n = size(spellcheck_filter_variable)
        lines = spellcheck_filter_variable(1:n - 1)
        call clear_filter()

        do while (.not. spellcheck_quit .and. size(lines) > 0)
            ! Get next word, accounting for accepted words and start shifts.
            call spellcheck_parse_output(lines(1)%text, positions, accept_list, shift)

            if (positions%kind == SPELLCHECK_OUTPUT_MISSPELLED) then
                ! Whenever we have misspellings, we can change the buffer.
                ! Keep boundaries as markers.  There is a -1 offset here as
                ! the string is escaped with '^' to prevent us accidentally
                ! sending any ispell commands.
                call make_marker(word_start)
                call set_marker(word_start, spellcheck_start - 1 + positions%offset, st)
                word_length = length_characters(positions%word)
                call make_marker(line_end)
                call set_marker(line_end, spellcheck_end, st)
                call make_marker(line_start)
                call set_marker(line_start, spellcheck_start, st)
                recheck_region = .false.
                spellcheck_abbreviation_saving_allowed = .true.
                spellcheck_save_correction_as_abbreviation = spellcheck_save_corrections_as_abbreviations
                word_position = marked_position(word_start)
                call goto_character(word_position)

                ! Alignment cannot be tracked and this error will occur when
                ! `query_replace' makes multiple corrections on the starting
                ! line.
                if (.not. spellcheck_looking_at(positions%word)) then
                    ! This error occurs due to filter pipe problems.
                    actual = word_position
                    write (point_text, '(" point ", i0, "; probably incompatible versions")') actual
                    call set_error(status, GENERIC_ERROR_CODE, "Ispell misalignment: word `"//positions%word//"'"//trim(point_text))
                    call release_markers()
                    return
                end if

                maximum_word = word_position

                ! The command loop can go recursive & change buffer.
                if (spellcheck_keep_choices_window) then
                    call spellcheck_command_loop(positions%misses, positions%guesses, positions%word, word_position, &
                                                 word_position + word_length, replace, status)
                else
                    call current_window_configuration(configuration)
                    call spellcheck_command_loop(positions%misses, positions%guesses, positions%word, word_position, &
                                                 word_position + word_length, replace, status)
                    call set_window_configuration(configuration)
                end if

                spellcheck_abbreviation_saving_allowed = .false.

                if (.not. status%ok()) then
                    call release_markers()
                    return
                end if

                word_position = marked_position(word_start)
                call goto_character(word_position)

                ! Recheck when query replace edit changes misspelled word.
                ! Error in tex mode when a potential math mode change exists.
                if (replace%kind == REPLACE_QUERY .or. replace%kind == REPLACE_RECHECK_HERE) then
                    if (spellcheck_parser%value == SPELLCHECK_PARSER_TEX .and. &
                        string_match("[\][]()[]\|\\begin\|\$", regular_expression_quote(string), st) > 0) then
                        call set_error(status, GENERIC_ERROR_CODE, &
                                       "Don't start query replace on a line with math characters")
                        call release_markers()
                        return
                    end if

                    call set_marker(line_end, point(), st)
                    recheck_region = .true.
                end if

                ! Insert correction if needed.
                if (replace%kind == REPLACE_KEEP .or. replace%kind == REPLACE_LOCAL) then
                    ! Accept/insert; buffer-local dict add.
                    if (replace%kind == REPLACE_LOCAL) call spellcheck_add_per_file_word_list(positions%word, st)
                    ! Do not recheck accepted word on this line.
                    call prepend_string(accept_list, positions%word)
                else
                    ! Replacement word selected or entered.
                    call delete_region(point(), point() + word_length, st)

                    if (replace%kind == REPLACE_WORD) then
                        ! Insert dictionary word.
                        call insert(replace%word, st)
                        call spellcheck_send_replacement(positions%word, replace%word)
                        call spellcheck_maybe_save_correction_abbreviation(positions%word, replace%word)
                        call prepend_string(accept_list, replace%word)
                    else
                        ! Recheck hand entered replacement word.
                        call insert(replace%word, st)
                        call spellcheck_send_replacement(positions%word, replace%word)
                        call spellcheck_maybe_save_correction_abbreviation(positions%word, replace%word)

                        if (replace%kind == REPLACE_QUERY) then
                            call current_window_configuration(configuration)
                            ! Assume case-replace & case-fold-search correct?
                            call query_replace(positions%word, replace%word, st, delimited=.true.)
                            call set_window_configuration(configuration)
                        end if

                        call goto_character(marked_position(word_start))

                        ! Do not recheck if already accepted.
                        if (has_word(accept_list, replace%word)) then
                            call prepend_string(accept_list, replace%word)
                        else
                            call make_marker(region_end)
                            call set_marker(region_end, marked_position(spellcheck_region_end), st)
                            ! Save filter.
                            recheck_filter = lines
                            ! Already accounted.
                            sub_shift = 0
                            word_position = marked_position(word_start)
                            call spellcheck_region(word_position, word_position + length_characters(replace%word), status, .true., &
                                                   sub_shift, sub_quit)
                            ! Quitting check.
                            shift = sub_shift
                            if (sub_quit) shift = 0
                            call set_marker(spellcheck_region_end, marked_position(region_end), st)
                            call destroy_marker(region_end, st)
                            lines = recheck_filter
                            recheck_region = .false.

                            if (.not. status%ok()) then
                                call release_markers()
                                return
                            end if
                        end if
                    end if

                    shift = shift + (length_characters(replace%word) - word_length)
                end if

                if (.not. spellcheck_quit) then
                    call message_unlogged("Continuing spelling check using "// &
                                          file_name_nondirectory(spellcheck_program_name)//" with "// &
                                          dictionary_name()//" dictionary...")
                end if

                call sit_for(0)
                spellcheck_start = marked_position(line_start)
                spellcheck_end = marked_position(line_end)
                word_position = marked_position(word_start)
                ! Adjust markers when end of region lost from highlighting.
                if (.not. recheck_region .and. spellcheck_end < word_position + word_length) spellcheck_end = word_position &
                                                                                                              + word_length
                if (word_position == marked_position(spellcheck_region_end)) call set_marker(spellcheck_region_end, &
                                                                                             word_position + word_length, st)
                ! Going out of scope - unneeded.
                call release_markers()

                if (recheck_region) then
                    ! The filter is emptied: the rest of the line is
                    ! checked again.
                    deallocate (lines)
                    allocate (lines(0))
                    cycle
                end if
            end if

            ! Finished with misspelling!
            if (size(lines) > 0) lines = lines(2:)
        end do

    contains

        ! Destroy the markers of the misspelling.
        subroutine release_markers()
            type(status_type) :: s

            call destroy_marker(line_start, s)
            call destroy_marker(word_start, s)
            call destroy_marker(line_end, s)
        end subroutine release_markers
    end subroutine spellcheck_process_line

    ! The current dictionary's name, "default" for the default.  Not a port.
    function dictionary_name() result(name)
        character(len=:), allocatable :: name

        name = spellcheck_current_dictionary
        if (len(name) == 0) name = "default"
    end function dictionary_name

    ! Whether WORDS contains WORD.  Not a port.
    logical function has_word(words, word)
        type(string_type), intent(in) :: words(:)
        character(len=*), intent(in) :: word
        integer :: k

        has_word = .false.

        do k = 1, size(words)
            if (words(k)%text == word) then
                has_word = .true.
                return
            end if
        end do
    end function has_word

    !> Check comments and strings in the current buffer for spelling
    !> errors.  If called interactively with an active region, check only
    !> comments and strings in the region.  When called from a program,
    !> START and END buffer positions can be provided to limit the check.
    !> Port of ispell.el:ispell-comments-and-strings.
    recursive subroutine spellcheck_comments_and_strings(status, start, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(parse_state_type) :: state, next
        integer(int64) :: limit, from, saved
        logical :: done, quit

        status = status_type()
        limit = point_maximum()
        if (present(end)) limit = end

        if (present(start)) then
            call goto_character(start)
        else
            call goto_character(point_minimum())
        end if

        done = .false.

        do while (.not. done)
            done = .true.
            call parse_partial_expression(point(), limit, next, status, oldstate=state, &
                                          commentstop=COMMENTSTOP_SYNTAX_TABLE)
            if (.not. status%ok()) return
            state = next

            if (state%instring >= 0 .or. state%incomment /= 0) then
                from = point()
                call parse_partial_expression(from, limit, next, status, oldstate=state, &
                                              commentstop=COMMENTSTOP_SYNTAX_TABLE)
                if (.not. status%ok()) return
                state = next

                if (state%instring >= 0 .or. state%incomment /= 0) then
                    call set_error(status, GENERIC_ERROR_CODE, "Unterminated string or comment")
                    return
                end if

                saved = point()
                call spellcheck_region(from, point(), status, quit=quit)
                call goto_character(saved)
                if (.not. status%ok()) return
                done = quit
            end if
        end do
    end subroutine spellcheck_comments_and_strings

    ! The command `spellcheck_comments_and_strings'.
    recursive subroutine execute_spellcheck_comments_and_strings(status)
        type(status_type), intent(out) :: status

        if (is_use_region()) then
            call spellcheck_comments_and_strings(status, active_region_beginning(), active_region_end())
        else
            call spellcheck_comments_and_strings(status)
        end if
    end subroutine execute_spellcheck_comments_and_strings

    !> Check the comment or string containing point for spelling errors.
    !> Port of ispell.el:ispell-comment-or-string-at-point.
    recursive subroutine spellcheck_comment_or_string_at_point(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state, next
        integer(int64) :: saved, start

        status = status_type()
        saved = point()
        call syntax_parser_state(state)

        if (state%instring >= 0 .or. state%incomment /= 0) then
            start = state%comstr_start
            call parse_partial_expression(point(), point_maximum(), next, status, oldstate=state, &
                                          commentstop=COMMENTSTOP_SYNTAX_TABLE)
            if (status%ok()) call spellcheck_region(start, point(), status)
        else
            call set_error(status, USER_ERROR_CODE, "Not inside a string or comment")
        end if

        call goto_character(saved)
    end subroutine spellcheck_comment_or_string_at_point

    !> Check the current buffer for spelling errors interactively.
    !> Leave the mark at the last misspelled word that the user was queried
    !> about.  Port of ispell.el:ispell-buffer.
    recursive subroutine spellcheck_buffer(status)
        type(status_type), intent(out) :: status

        call spellcheck_region(point_minimum(), point_maximum(), status)
    end subroutine spellcheck_buffer

    !> Continue a halted spelling session beginning with the current word.
    !> Port of ispell.el:ispell-continue.
    recursive subroutine spellcheck_continue(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: b
        character(len=:), allocatable :: word
        integer(int64) :: start, end
        type(status_type) :: st

        status = status_type()

        if (marked_position(spellcheck_region_end) == 0) then
            call message("No session to continue.  Use `X' command when checking!")
            return
        end if

        call marker_buffer(spellcheck_region_end, b, st)

        if (b%index /= current) then
            call message("Must continue ispell from buffer "//buffers(b%index)%name)
            return
        end if

        ! Find beginning of current word.
        call spellcheck_get_word(.true., word, start, end, status)
        if (.not. status%ok()) return
        call spellcheck_region(start, marked_position(spellcheck_region_end), status)
    end subroutine spellcheck_continue

    !> Interactively check a region or buffer for spelling errors.
    !> If `transient_mark_mode' is on, and a region is active, spell-check
    !> that region.  Otherwise spell-check the buffer.
    !>
    !> Ispell dictionaries are not distributed with Fortmacs.  If you are
    !> looking for a dictionary, please see the distribution of the GNU
    !> ispell program, or do an Internet search; there are various
    !> dictionaries available on the net.  Port of ispell.el:ispell.
    recursive subroutine spellcheck(status)
        type(status_type), intent(out) :: status

        if (is_region_active()) then
            call spellcheck_region(active_region_beginning(), active_region_end(), status)
        else
            call spellcheck_buffer(status)
        end if
    end subroutine spellcheck

    ! The command `spellcheck_check_version'.
    recursive subroutine execute_spellcheck_check_version(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: result

        call spellcheck_check_version(result, status, .true.)
    end subroutine execute_spellcheck_check_version

    ! ------------------------------------------------------------------
    ! Buffer local functions
    ! ------------------------------------------------------------------

    !> Load all buffer-local information, restarting Ispell when necessary.
    !> Port of ispell.el:ispell-accept-buffer-local-defs.
    recursive subroutine spellcheck_accept_buffer_local_definitions(status)
        type(status_type), intent(out) :: status

        ! May kill ispell-process.
        call spellcheck_buffer_local_dictionary(status)
        if (.not. status%ok()) return
        ! Will initialize ispell-process.
        call spellcheck_buffer_local_words(status)
        if (.not. status%ok()) return
        call spellcheck_buffer_local_parsing(status)
    end subroutine spellcheck_accept_buffer_local_definitions

    !> Place Ispell into parsing mode for this buffer.
    !> Overrides the default parsing mode.
    !> Includes LaTeX/Nroff modes and extended character mode.  Port of
    !> ispell.el:ispell-buffer-local-parsing.
    recursive subroutine spellcheck_buffer_local_parsing(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: mode, major, string
        integer(int64) :: saved, found, end
        type(status_type) :: st

        ! Put process in terse mode.
        call spellcheck_send_string("!"//LF, status)
        major = ""
        if (allocated(buffers(current)%major_mode)) major = buffers(current)%major_mode

        ! We assume all major modes with "tex_mode" in them should use latex
        ! parsing.  When exclusively checking comments, set to raw text
        ! mode (nroff).
        if (spellcheck_check_comments%choice /= SPELLCHECK_CHECK_COMMENTS_EXCLUSIVE .and. &
            ((spellcheck_parser%value == SPELLCHECK_PARSER_USE_MODE_NAME .and. &
              string_match("[Tt][Ee][Xx]_mode", major, st) > 0) .or. spellcheck_parser%value == SPELLCHECK_PARSER_TEX)) then
            ! Set ispell mode to tex.
            call spellcheck_send_string("+"//LF, st)
            if (spellcheck_parser%value /= SPELLCHECK_PARSER_TEX) call spellcheck_parser%set_local(SPELLCHECK_PARSER_TEX)
        else
            ! Set mode to normal (nroff).
            call spellcheck_send_string("-"//LF, st)
        end if

        ! If needed, test for SGML & HTML modes and set a buffer local value.
        if (spellcheck_skip_html%choice == SPELLCHECK_SKIP_HTML_USE_MODE_NAME) then
            if (string_match("sgml\|html\|xml", downcase_string(major), st) > 0) then
                call spellcheck_skip_html%set_local(.true.)
            else
                call spellcheck_skip_html%set_local(.false.)
            end if
        end if

        ! Set default extended character mode for given buffer, if any.
        mode = spellcheck_get_extended_character_mode()
        if (len(mode) > 0) call spellcheck_send_string(mode//LF, st)
        ! Set buffer-local parsing mode and extended character mode, if
        ! specified.
        saved = point()
        call goto_character(point_maximum())
        ! Uses last occurrence of `spellcheck_parsing_keyword'.
        call search_backward(spellcheck_parsing_keyword, found, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            end = line_end_position()
            call search_forward(spellcheck_parsing_keyword, found, st)

            do
                call regular_expression_search_forward(" *\([^ ""]+\)", found, st, end, SEARCH_NOERROR)
                if (.not. st%ok() .or. found == 0) exit
                ! Space separated definitions.
                string = downcase_string(match_string_no_properties(1))

                if (index(string, &
                          "latex_mode") > 0 .and. spellcheck_check_comments%choice /= SPELLCHECK_CHECK_COMMENTS_EXCLUSIVE) then
                    call spellcheck_send_string("+"//LF//"~tex"//LF, st)
                else if (index(string, "nroff_mode") > 0) then
                    call spellcheck_send_string("-"//LF//"~nroff"//LF, st)
                else if (index(string, "~") > 0) then
                    ! Set extended character mode.
                    call spellcheck_send_string(string//LF, st)
                else
                    call message("Invalid Ispell Parsing argument!")
                    call sit_for(2000)
                end if
            end do
        end if

        call goto_character(saved)
    end subroutine spellcheck_buffer_local_parsing

    !> Initialize local dictionary and local personal dictionary.
    !> If optional NO_RELOAD is true, do not reload any dictionary.
    !> When a dictionary is defined in the buffer (see variable
    !> `spellcheck_dictionary_keyword'), it will override the local setting from
    !> M-x spellcheck_change_dictionary.  Both should not be used to define a
    !> buffer-local dictionary.  Port of ispell.el:ispell-buffer-local-dict.
    recursive subroutine spellcheck_buffer_local_dictionary(status, no_reload)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_reload
        integer(int64) :: saved, found, end
        type(status_type) :: st

        status = status_type()
        saved = point()
        ! Override the local variable definition.  Uses last occurrence of
        ! `spellcheck_dictionary_keyword'.
        call goto_character(point_maximum())

        if (.not. spellcheck_local_dictionary_overridden%value) then
            call search_backward(spellcheck_dictionary_keyword, found, st, noerror=SEARCH_NOERROR)

            if (st%ok() .and. found > 0) then
                call search_forward(spellcheck_dictionary_keyword, found, st)
                end = line_end_position()
                call regular_expression_search_forward(" *\([^ ""]+\)", found, st, end, SEARCH_NOERROR)
                if (st%ok() .and. found > 0) call spellcheck_local_dictionary%set_local(match_string_no_properties(1))
            end if
        end if

        call goto_character(point_maximum())
        call search_backward(spellcheck_pdict_keyword, found, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            call search_forward(spellcheck_pdict_keyword, found, st)
            end = line_end_position()
            call regular_expression_search_forward(" *\([^ ""]+\)", found, st, end, SEARCH_NOERROR)
            if (st%ok() .and. found > 0) call spellcheck_local_pdict%set_local(match_string_no_properties(1))
        end if

        call goto_character(saved)

        if (present(no_reload)) then
            if (no_reload) return
        end if

        ! Reload if new dictionary (maybe the personal one) defined.
        call spellcheck_internal_change_dictionary()
    end subroutine spellcheck_buffer_local_dictionary

    !> Load the buffer-local dictionary in the current buffer.  Port of
    !> ispell.el:ispell-buffer-local-words.
    recursive subroutine spellcheck_buffer_local_words(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: casechars, string
        integer(int64) :: saved, found, end
        type(status_type) :: st
        integer :: k, j

        ! If there's an existing ispell process that's wrong for this use,
        ! kill it.
        if (len(spellcheck_buffer_local_name) > 0 .and. spellcheck_buffer_local_name /= buffers(current)%name) then
            call spellcheck_kill_spellcheck(st, no_error=.true.)
        end if

        ! Actually start a new ispell process, because we need to send
        ! commands now to specify the local words to it.
        call spellcheck_initialize_process(status)
        if (.not. status%ok()) return
        k = session_slot()

        if (k > 0) then
            do j = 1, size(session_localwords(k)%words)
                call spellcheck_send_string("@"//session_localwords(k)%words(j)%text//LF, st)
            end do

            if (len(spellcheck_buffer_local_name) == 0 .and. size(session_localwords(k)%words) > 0) then
                spellcheck_buffer_local_name = buffers(current)%name
            end if
        end if

        saved = point()
        call goto_character(point_minimum())

        do
            call search_forward(spellcheck_words_keyword, found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found == 0) exit
            if (len(spellcheck_buffer_local_name) == 0) spellcheck_buffer_local_name = buffers(current)%name
            end = line_end_position()
            casechars = spellcheck_get_casechars()

            ! Buffer-local words separated by a space, and can contain any
            ! character other than a space.  Not rigorous enough.
            do
                call regular_expression_search_forward(" *\([^ ]+\)", found, st, end, SEARCH_NOERROR)
                if (.not. st%ok() .or. found == 0) exit
                string = match_string_no_properties(1)

                ! This can fail when string contains a word with invalid
                ! chars.  Error handling needs to be added between ispell
                ! and Fortmacs.
                if (length_characters(string) > 1) then
                    if (string_match(casechars, string, st) == 1) call spellcheck_send_string("@"//string//LF, st)
                end if
            end do
        end do

        call goto_character(saved)
    end subroutine spellcheck_buffer_local_words

    ! Return the comment marker for the current mode.  Port of
    ! ispell.el:ispell--comment-prefix.
    function spellcheck_comment_prefix() result(prefix)
        character(len=:), allocatable :: prefix
        type(status_type) :: st

        call comment_normalize_variables(st)
        ! (Upstream also calls comment-padright, whose result it drops.)
        prefix = comment_start%value
    end function spellcheck_comment_prefix

    !> Add WORD to the per-file word list.  Port of
    !> ispell.el:ispell-add-per-file-word-list.
    recursive subroutine spellcheck_add_per_file_word_list(word, status)
        character(len=*), intent(in) :: word
        type(status_type), intent(out) :: status
        integer(int64) :: saved, found
        logical :: line_okay, done, has_found, searched, empty_comment_end, saved_fold
        type(status_type) :: st

        status = status_type()
        if (len(spellcheck_buffer_local_name) == 0) spellcheck_buffer_local_name = buffers(current)%name
        saved = point()
        call goto_character(point_minimum())
        done = .false.
        has_found = .false.

        do while (.not. done)
            saved_fold = case_fold_search%value
            case_fold_search = .false.
            call search_forward(spellcheck_words_keyword, found, st, noerror=SEARCH_NOERROR)
            case_fold_search = saved_fold
            searched = st%ok() .and. found > 0
            has_found = has_found .or. searched
            call end_of_line()
            ! 1 for space after word.
            line_okay = length_characters(word) + 1 + current_column() < fill_column%value

            if ((searched .and. line_okay) .or. .not. searched) then
                done = .true.

                if (.not. searched) then
                    empty_comment_end = string_value(comment_end) == ""

                    if (has_found) then
                        ! After an existing LocalWords.
                        call insert(LF, st)

                        if (empty_comment_end .and. allocated(comment_start%value)) then
                            call insert(spellcheck_comment_prefix()//" ", st)
                        end if
                    else
                        ! No LocalWords, go to end of file.
                        call goto_character(point_maximum())
                        call open_line(1_int64, st)
                        call newline(st)

                        ! Insert an end marker if needed, preceded by a
                        ! newline.
                        if (.not. empty_comment_end) then
                            found = point()
                            call newline(st)
                            call insert(comment_end%value, st)
                            call goto_character(found)
                        end if

                        if (allocated(comment_start%value)) then
                            if (empty_comment_end) then
                                call insert(spellcheck_comment_prefix()//" ", st)
                            else
                                call insert(spellcheck_comment_prefix()//LF, st)
                            end if
                        end if
                    end if

                    call insert(spellcheck_words_keyword, st)
                end if

                call insert(" "//word, st)
            end if
        end do

        call goto_character(saved)
        if (saved > point_maximum()) call goto_character(point_maximum())
    end subroutine spellcheck_add_per_file_word_list

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Register the commands, bind M-$ and give the settings their defaults.
    !> Not a port.
    subroutine initialize_spellcheck()
        character(len=:), allocatable :: path

        commands%spellcheck_word%execute => execute_spellcheck_word
        commands%spellcheck_region%execute => execute_spellcheck_region
        commands%spellcheck_buffer%execute => spellcheck_buffer
        commands%spellcheck%execute => spellcheck
        commands%spellcheck_continue%execute => spellcheck_continue
        commands%spellcheck_comments_and_strings%execute => execute_spellcheck_comments_and_strings
        commands%spellcheck_comment_or_string_at_point%execute => spellcheck_comment_or_string_at_point
        commands%spellcheck_change_dictionary%execute => execute_spellcheck_change_dictionary
        commands%spellcheck_kill_spellcheck%execute => execute_spellcheck_kill_spellcheck
        commands%spellcheck_pdict_save%execute => execute_spellcheck_pdict_save
        commands%spellcheck_check_version%execute => execute_spellcheck_check_version
        call forward_setting(spellcheck_local_dictionary, local_if_set=.true.)
        call forward_setting(spellcheck_local_dictionary_overridden, local_if_set=.true.)
        call forward_setting(spellcheck_skip_html, local_if_set=.true.)
        call forward_setting(spellcheck_local_pdict, local_if_set=.true.)
        call forward_setting(spellcheck_parser)

        if (.not. allocated(spellcheck_program_name)) then
            spellcheck_program_name = "ispell"
            call executable_find("aspell", path)
            if (len(path) == 0) call executable_find("ispell", path)
            if (len(path) == 0) call executable_find("hunspell", path)
            ! Enchant is commonly installed as `enchant-2', so use this
            ! name and avoid old versions of `enchant'.
            if (len(path) == 0) call executable_find("enchant-2", path)
            if (len(path) > 0) spellcheck_program_name = path
        end if

        if (.not. allocated(spellcheck_alternate_dictionary)) call initialize_alternate_dictionary()
        if (.not. allocated(spellcheck_complete_word_dictionary)) spellcheck_complete_word_dictionary = ""
        if (.not. allocated(spellcheck_grep_command)) spellcheck_grep_command = "grep"
        if (.not. allocated(spellcheck_grep_options)) spellcheck_grep_options = "-Ei"

        if (.not. allocated(spellcheck_look_command)) then
            call executable_find("look", path)
            spellcheck_look_command = path
        end if

        if (.not. allocated(spellcheck_look_options)) spellcheck_look_options = "-df"
        if (.not. allocated(spellcheck_personal_dictionary)) spellcheck_personal_dictionary = ""
        if (.not. allocated(spellcheck_dictionary)) spellcheck_dictionary = ""
        if (.not. allocated(spellcheck_extra_arguments)) allocate (spellcheck_extra_arguments(0))
        if (.not. allocated(spellcheck_tib_reference_beginning)) spellcheck_tib_reference_beginning = "[[<]\."
        if (.not. allocated(spellcheck_tib_reference_end)) spellcheck_tib_reference_end = "\.[]>]"
        if (.not. allocated(spellcheck_local_dictionaries)) allocate (spellcheck_local_dictionaries(0))
        if (.not. allocated(spellcheck_dictionaries)) allocate (spellcheck_dictionaries(0))
        if (.not. allocated(spellcheck_aspell_dictionaries)) allocate (spellcheck_aspell_dictionaries(0))
        if (.not. allocated(spellcheck_aspell_found_dictionaries)) allocate (spellcheck_aspell_found_dictionaries(0))
        if (.not. allocated(spellcheck_really_aspell)) spellcheck_really_aspell = ""
        if (.not. allocated(spellcheck_really_hunspell)) spellcheck_really_hunspell = ""
        if (.not. allocated(spellcheck_really_enchant)) spellcheck_really_enchant = ""
        if (.not. allocated(spellcheck_encoding8_command)) spellcheck_encoding8_command = ""
        if (.not. allocated(spellcheck_library_directory)) spellcheck_library_directory = ""
        if (.not. allocated(spellcheck_aspell_data_directory)) spellcheck_aspell_data_directory = ""
        if (.not. allocated(spellcheck_aspell_dictionary_directory)) spellcheck_aspell_dictionary_directory = ""
        if (.not. allocated(spellcheck_current_dictionary)) spellcheck_current_dictionary = ""
        if (.not. allocated(spellcheck_current_personal_dictionary)) spellcheck_current_personal_dictionary = ""
        if (.not. allocated(spellcheck_buffer_local_name)) spellcheck_buffer_local_name = ""
        if (.not. allocated(spellcheck_process_directory)) spellcheck_process_directory = ""
        if (.not. allocated(spellcheck_filter_variable)) allocate (spellcheck_filter_variable(0))
        if (.not. allocated(spellcheck_base_dictionaries)) call initialize_base_dictionaries()
        if (.not. allocated(spellcheck_dictionaries_name2locale_equivs)) call initialize_locale_equivs()
        if (.not. allocated(spellcheck_skip_regions)) call initialize_skip_regions()
    end subroutine initialize_spellcheck

    ! `spellcheck_alternate_dictionary's default: the first readable word list.
    ! Not a port.
    subroutine initialize_alternate_dictionary()
        character(len=*), parameter :: files(7) = [character(len=25) :: "/usr/dict/web2", "/usr/share/dict/web2", &
                                                                "/usr/dict/words", "/usr/lib/dict/words", "/usr/share/dict/words", &
                                                                                           "/usr/share/lib/dict/words", "/sys/dict"]
        integer :: k

        spellcheck_alternate_dictionary = ""

        do k = 1, size(files)
            if (is_file_readable(trim(files(k)))) then
                spellcheck_alternate_dictionary = trim(files(k))
                return
            end if
        end do
    end subroutine initialize_alternate_dictionary

    ! `spellcheck_dictionaries_name2locale_equivs's default.  Not a port.
    subroutine initialize_locale_equivs()
        character(len=16), parameter :: names(31) = [character(len=16) :: "american", "brasileiro", "british", &
                                                             "castellano", "castellano8", "czech", "dansk", "deutsch", "deutsch8", &
                                                                  "english", "esperanto", "esperanto-tex", "finnish", "francais7", &
                                                                      "francais", "francais-tex", "german", "german8", "italiano", &
                                                                     "nederlands", "nederlands8", "norsk", "norsk7-tex", "polish", &
                                                                        "portugues", "russian", "russianw", "slovak", "slovenian", &
                                                                                                                "svenska", "hebrew"]
        character(len=8), parameter :: locales(31) = [character(len=8) :: "en_US", "pt_BR", "en_GB", "es_ES", "es_ES", &
                                                                 "cs_CZ", "da_DK", "de_DE", "de_DE", "en_US", "eo", "eo", "fi_FI", &
                                                                    "fr_FR", "fr_FR", "fr_FR", "de_DE", "de_DE", "it_IT", "nl_NL", &
                                                                    "nl_NL", "nn_NO", "nn_NO", "pl_PL", "pt_PT", "ru_RU", "ru_RU", &
                                                                                                 "sk_SK", "sl_SI", "sv_SE", "he_IL"]
        integer :: k

        allocate (spellcheck_dictionaries_name2locale_equivs(size(names)))

        do k = 1, size(names)
            spellcheck_dictionaries_name2locale_equivs(k)%name = names(k)
            spellcheck_dictionaries_name2locale_equivs(k)%locale = locales(k)
        end do
    end subroutine initialize_locale_equivs

    ! The regions to skip: `spellcheck_skip_regions', the TeX and HTML ones.
    ! Not a port.
    subroutine initialize_skip_regions()
        allocate (spellcheck_skip_regions(8))
        spellcheck_skip_regions(1)%beginning_regular_expression = keyword_regular_expression(spellcheck_words_keyword)
        spellcheck_skip_regions(1)%skip => skip_line
        spellcheck_skip_regions(2)%beginning_regular_expression = keyword_regular_expression(spellcheck_dictionary_keyword)
        spellcheck_skip_regions(2)%skip => skip_line
        spellcheck_skip_regions(3)%beginning_regular_expression = keyword_regular_expression(spellcheck_pdict_keyword)
        spellcheck_skip_regions(3)%skip => skip_line
        spellcheck_skip_regions(4)%beginning_regular_expression = keyword_regular_expression(spellcheck_parsing_keyword)
        spellcheck_skip_regions(4)%skip => skip_line
        spellcheck_skip_regions(5)%beginning_regular_expression = "^---*BEGIN PGP [A-Z ]*--*"
        spellcheck_skip_regions(5)%end_regular_expression = "^---*END PGP [A-Z ]*--*"
        ! Assume multiline uuencoded file? "\nM.*$"?
        spellcheck_skip_regions(6)%beginning_regular_expression = "^begin [0-9][0-9][0-9] [^ "//TAB//"]+$"
        spellcheck_skip_regions(6)%end_regular_expression = LF//"end"//LF
        spellcheck_skip_regions(7)%beginning_regular_expression = "^%!PS-Adobe-[123].0"
        spellcheck_skip_regions(7)%end_regular_expression = LF//"%%EOF"//LF
        spellcheck_skip_regions(8)%beginning_regular_expression = "^---* \(Start of \)?[Ff]orwarded [Mm]essage"
        spellcheck_skip_regions(8)%end_regular_expression = "^---* End of [Ff]orwarded [Mm]essage"
        ! All the standard LaTeX keywords from L. Lamport's guide: \cite,
        ! \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
        ! \label, \nocite, \rule (in ispell - rest included here).
        allocate (spellcheck_tex_skip_regions(8))
        call set_tex(1, "\\addcontentsline", 2)
        call set_tex(2, "\\add\(tocontents\|vspace\)", 1)
        call set_tex(3, "\\\([aA]lph\|arabic\)", 1)
        call set_tex(4, "\\cref", 1)
        call set_tex(5, "\\bibliographystyle", 1)
        call set_tex(6, "\\makebox", 0)
        call set_tex(7, "\\e?psfig", 1)
        spellcheck_tex_skip_regions(8)%beginning_regular_expression = "\\document\(class\|style\)"
        spellcheck_tex_skip_regions(8)%end_regular_expression = "\\begin[ "//TAB//LF//"]*{document}"
        ! Delimited with \begin.  In ispell: displaymath, eqnarray,
        ! eqnarray*, equation, minipage, picture, tabular, tabular* (ispell).
        allocate (spellcheck_tex_skip_environments(4))
        spellcheck_tex_skip_environments(1)%beginning_regular_expression = "\(figure\|table\)\*?"
        spellcheck_tex_skip_environments(1)%skip => spellcheck_tex_argument_end
        spellcheck_tex_skip_environments(1)%argument = 0
        spellcheck_tex_skip_environments(2)%beginning_regular_expression = "list"
        spellcheck_tex_skip_environments(2)%skip => spellcheck_tex_argument_end
        spellcheck_tex_skip_environments(2)%argument = 2
        spellcheck_tex_skip_environments(3)%beginning_regular_expression = "program"
        spellcheck_tex_skip_environments(3)%end_regular_expression = "\\end[ "//TAB//"]*{program}"
        spellcheck_tex_skip_environments(4)%beginning_regular_expression = "verbatim\*?"
        spellcheck_tex_skip_environments(4)%end_regular_expression = "\\end[ "//TAB//"]*{verbatim\*?}"
        allocate (spellcheck_html_skip_regions(7))
        call set_html(1, "<[cC][oO][dD][eE]\>[^>]*>", "</[cC][oO][dD][eE]*>")
        call set_html(2, "<[sS][cC][rR][iI][pP][tT]\>[^>]*>", "</[sS][cC][rR][iI][pP][tT]>")
        call set_html(3, "<[aA][pP][pP][lL][eE][tT]\>[^>]*>", "</[aA][pP][pP][lL][eE][tT]>")
        call set_html(4, "<[vV][eE][rR][bB]\>[^>]*>", "<[vV][eE][rR][bB]\>[^>]*>")
        call set_html(5, "<[tT][tT]/", "/")
        call set_html(6, "<[^ "//TAB//LF//">]", ">")
        call set_html(7, "&[^ "//TAB//LF//";]", "[; "//TAB//LF//"]")

    contains

        ! KEYWORD as a regexp whose trailing space survives the fixed length
        ! (the keywords end with a space).
        function keyword_regular_expression(keyword) result(regular_expression)
            character(len=*), intent(in) :: keyword
            character(len=:), allocatable :: regular_expression

            regular_expression = keyword(1:len(keyword) - 1)//"[ ]"
        end function keyword_regular_expression

        ! A TeX region skipped by `spellcheck_tex_argument_end' with ARGUMENT.
        subroutine set_tex(k, key, argument)
            integer, intent(in) :: k, argument
            character(len=*), intent(in) :: key

            spellcheck_tex_skip_regions(k)%beginning_regular_expression = key
            spellcheck_tex_skip_regions(k)%skip => spellcheck_tex_argument_end
            spellcheck_tex_skip_regions(k)%argument = argument
        end subroutine set_tex

        ! An HTML region from KEY to END.
        subroutine set_html(k, key, end)
            integer, intent(in) :: k
            character(len=*), intent(in) :: key, end

            spellcheck_html_skip_regions(k)%beginning_regular_expression = key
            spellcheck_html_skip_regions(k)%end_regular_expression = end
        end subroutine set_html
    end subroutine initialize_skip_regions

    ! `spellcheck_base_dictionaries's default.  The character sets are in the
    ! dictionaries' coding systems, as upstream's unibyte strings.
    ! Not a port.
    subroutine initialize_base_dictionaries()
        type(spellcheck_dictionary_type) :: d

        allocate (spellcheck_base_dictionaries(32))
        d = spellcheck_dictionary_type(name="", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(1) = d
        d = spellcheck_dictionary_type(name="american", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(2) = d
        d = spellcheck_dictionary_type(name="brasileiro", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%casechars = "[A-Z"//char(193)//char(201)//char(205)//char(211)//char(218)//char(192)// &
                      char(200)//char(204)//char(210)//char(217)//char(195)//char(213)//char(199)// &
                      char(220)//char(194)//char(202)//char(212)//"a-z"//char(225)//char(233)// &
                      char(237)//char(243)//char(250)//char(224)//char(232)//char(236)//char(242)// &
                      char(249)//char(227)//char(245)//char(231)//char(252)//char(226)//char(234)// &
                      char(244)//"]"
        d%not_casechars = "[^A-Z"//char(193)//char(201)//char(205)//char(211)//char(218)//char(192)// &
                          char(200)//char(204)//char(210)//char(217)//char(195)//char(213)// &
                          char(199)//char(220)//char(194)//char(202)//char(212)//"a-z"//char(225)// &
                          char(233)//char(237)//char(243)//char(250)//char(224)//char(232)// &
                          char(236)//char(242)//char(249)//char(227)//char(245)//char(231)// &
                          char(252)//char(226)//char(234)//char(244)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(3) = d
        d = spellcheck_dictionary_type(name="british", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(4) = d
        d = spellcheck_dictionary_type(name="castellano", many_otherchars=.false., extended_character_mode="~tex", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%casechars = "[A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                      char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                      char(250)//char(252)//"]"
        d%not_casechars = "[^A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                          char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                          char(250)//char(252)//"]"
        d%otherchars = "[-]"
        spellcheck_base_dictionaries(5) = d
        d = spellcheck_dictionary_type(name="castellano8", many_otherchars=.false., extended_character_mode="~latin1", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%arguments(2) = "-d"
        d%arguments(3) = "castellano"
        d%casechars = "[A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                      char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                      char(250)//char(252)//"]"
        d%not_casechars = "[^A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                          char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                          char(250)//char(252)//"]"
        d%otherchars = "[-]"
        spellcheck_base_dictionaries(6) = d
        d = spellcheck_dictionary_type(name="czech", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-2")
        d%arguments(1) = "-B"
        d%casechars = "[A-Za-z"//char(193)//char(201)//char(204)//char(205)//char(211)//char(218)// &
                      char(217)//char(221)//char(174)//char(169)//char(200)//char(216)//char(207)// &
                      char(171)//char(210)//char(225)//char(233)//char(236)//char(237)//char(243)// &
                      char(250)//char(249)//char(253)//char(190)//char(185)//char(232)//char(248)// &
                      char(239)//char(187)//char(242)//"]"
        d%not_casechars = "[^A-Za-z"//char(193)//char(201)//char(204)//char(205)//char(211)// &
                          char(218)//char(217)//char(221)//char(174)//char(169)//char(200)// &
                          char(216)//char(207)//char(171)//char(210)//char(225)//char(233)// &
                          char(236)//char(237)//char(243)//char(250)//char(249)//char(253)// &
                          char(190)//char(185)//char(232)//char(248)//char(239)//char(187)// &
                          char(242)//"]"
        d%otherchars = ""
        spellcheck_base_dictionaries(7) = d
        d = spellcheck_dictionary_type(name="dansk", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[A-Z"//char(198)//char(216)//char(197)//"a-z"//char(230)//char(248)// &
                      char(229)//"]"
        d%not_casechars = "[^A-Z"//char(198)//char(216)//char(197)//"a-z"//char(230)//char(248)// &
                          char(229)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(8) = d
        d = spellcheck_dictionary_type(name="deutsch", many_otherchars=.true., extended_character_mode="~tex", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[a-zA-Z""]"
        d%not_casechars = "[^a-zA-Z""]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(9) = d
        d = spellcheck_dictionary_type(name="deutsch8", many_otherchars=.true., extended_character_mode="~latin1", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%arguments(2) = "-d"
        d%arguments(3) = "deutsch"
        d%casechars = "[a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)//char(223)// &
                      char(252)//"]"
        d%not_casechars = "[^a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)// &
                          char(223)//char(252)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(10) = d
        d = spellcheck_dictionary_type(name="english", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(11) = d
        d = spellcheck_dictionary_type(name="esperanto", many_otherchars=.true., extended_character_mode="~latin3", &
                                       coding_system="iso-8859-3")
        d%arguments(1) = "-C"
        d%casechars = "[A-Za-z"//char(166)//char(172)//char(182)//char(188)//char(198)//char(216)// &
                      char(221)//char(222)//char(230)//char(248)//char(253)//char(254)//"]"
        d%not_casechars = "[^A-Za-z"//char(166)//char(172)//char(182)//char(188)//char(198)// &
                          char(216)//char(221)//char(222)//char(230)//char(248)//char(253)// &
                          char(254)//"]"
        d%otherchars = "[-']"
        spellcheck_base_dictionaries(12) = d
        d = spellcheck_dictionary_type(name="esperanto-tex", many_otherchars=.true., extended_character_mode="~tex", &
                                       coding_system="iso-8859-3")
        d%arguments(1) = "-C"
        d%arguments(2) = "-d"
        d%arguments(3) = "esperanto"
        d%casechars = "[A-Za-z^\]"
        d%not_casechars = "[^A-Za-z^\]"
        d%otherchars = "[-'`""]"
        spellcheck_base_dictionaries(13) = d
        d = spellcheck_dictionary_type(name="finnish", many_otherchars=.false., extended_character_mode="~list", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[A-Za-z"//char(229)//char(228)//char(246)//char(197)//char(196)//char(214)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(229)//char(228)//char(246)//char(197)//char(196)// &
                          char(214)//"]"
        d%otherchars = "[:]"
        spellcheck_base_dictionaries(14) = d
        d = spellcheck_dictionary_type(name="francais7", many_otherchars=.true., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[`'^-]"
        spellcheck_base_dictionaries(15) = d
        d = spellcheck_dictionary_type(name="francais", many_otherchars=.true., extended_character_mode="~list", &
                                       coding_system="iso-8859-1")
        d%casechars = "[A-Za-z"//char(192)//char(194)//char(198)//char(199)//char(200)//char(201)// &
                      char(202)//char(203)//char(206)//char(207)//char(212)//char(217)//char(219)// &
                      char(220)//char(224)//char(226)//char(231)//char(232)//char(233)//char(234)// &
                      char(235)//char(238)//char(239)//char(244)//char(249)//char(251)//char(252)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(192)//char(194)//char(198)//char(199)//char(200)// &
                          char(201)//char(202)//char(203)//char(206)//char(207)//char(212)// &
                          char(217)//char(219)//char(220)//char(224)//char(226)//char(231)// &
                          char(232)//char(233)//char(234)//char(235)//char(238)//char(239)// &
                          char(244)//char(249)//char(251)//char(252)//"]"
        d%otherchars = "[-'.@]"
        spellcheck_base_dictionaries(16) = d
        d = spellcheck_dictionary_type(name="francais-tex", many_otherchars=.true., extended_character_mode="~tex", &
                                       coding_system="iso-8859-1")
        d%casechars = "[A-Za-z"//char(192)//char(194)//char(198)//char(199)//char(200)//char(201)// &
                      char(202)//char(203)//char(206)//char(207)//char(212)//char(217)//char(219)// &
                      char(220)//char(224)//char(226)//char(231)//char(232)//char(233)//char(234)// &
                      char(235)//char(238)//char(239)//char(244)//char(249)//char(251)//char(252)// &
                      "\]"
        d%not_casechars = "[^A-Za-z"//char(192)//char(194)//char(198)//char(199)//char(200)// &
                          char(201)//char(202)//char(203)//char(206)//char(207)//char(212)// &
                          char(217)//char(219)//char(220)//char(224)//char(226)//char(231)// &
                          char(232)//char(233)//char(234)//char(235)//char(238)//char(239)// &
                          char(244)//char(249)//char(251)//char(252)//"\]"
        d%otherchars = "[-'^`"".@]"
        spellcheck_base_dictionaries(17) = d
        d = spellcheck_dictionary_type(name="german", many_otherchars=.true., extended_character_mode="~tex", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[a-zA-Z""]"
        d%not_casechars = "[^a-zA-Z""]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(18) = d
        d = spellcheck_dictionary_type(name="german8", many_otherchars=.true., extended_character_mode="~latin1", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%arguments(2) = "-d"
        d%arguments(3) = "german"
        d%casechars = "[a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)//char(223)// &
                      char(252)//"]"
        d%not_casechars = "[^a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)// &
                          char(223)//char(252)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(19) = d
        d = spellcheck_dictionary_type(name="italiano", many_otherchars=.false., extended_character_mode="~tex", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-B"
        d%arguments(2) = "-d"
        d%arguments(3) = "italian"
        d%casechars = "[A-Z"//char(192)//char(193)//char(200)//char(201)//char(204)//char(205)// &
                      char(210)//char(211)//char(217)//char(218)//"a-z"//char(224)//char(225)// &
                      char(232)//char(233)//char(236)//char(237)//char(243)//char(249)//char(250)// &
                      "]"
        d%not_casechars = "[^A-Z"//char(192)//char(193)//char(200)//char(201)//char(204)//char(205)// &
                          char(210)//char(211)//char(217)//char(218)//"a-z"//char(224)//char(225)// &
                          char(232)//char(233)//char(236)//char(237)//char(243)//char(249)// &
                          char(250)//"]"
        d%otherchars = "[-.]"
        spellcheck_base_dictionaries(20) = d
        d = spellcheck_dictionary_type(name="nederlands", many_otherchars=.true., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[A-Za-z"//char(192)//char(193)//char(194)//char(195)//char(196)//char(197)// &
                      char(199)//char(200)//char(201)//char(202)//char(203)//char(204)//char(205)// &
                      char(206)//char(207)//char(210)//char(211)//char(212)//char(213)//char(214)// &
                      char(217)//char(218)//char(219)//char(220)//char(224)//char(225)//char(226)// &
                      char(227)//char(228)//char(229)//char(231)//char(232)//char(233)//char(234)// &
                      char(235)//char(236)//char(237)//char(238)//char(239)//char(241)//char(242)// &
                      char(243)//char(244)//char(245)//char(246)//char(249)//char(250)//char(251)// &
                      char(252)//"]"
        d%not_casechars = "[^A-Za-z"//char(192)//char(193)//char(194)//char(195)//char(196)// &
                          char(197)//char(199)//char(200)//char(201)//char(202)//char(203)// &
                          char(204)//char(205)//char(206)//char(207)//char(210)//char(211)// &
                          char(212)//char(213)//char(214)//char(217)//char(218)//char(219)// &
                          char(220)//char(224)//char(225)//char(226)//char(227)//char(228)// &
                          char(229)//char(231)//char(232)//char(233)//char(234)//char(235)// &
                          char(236)//char(237)//char(238)//char(239)//char(241)//char(242)// &
                          char(243)//char(244)//char(245)//char(246)//char(249)//char(250)// &
                          char(251)//char(252)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(21) = d
        d = spellcheck_dictionary_type(name="nederlands8", many_otherchars=.true., extended_character_mode="", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[A-Za-z"//char(192)//char(193)//char(194)//char(195)//char(196)//char(197)// &
                      char(199)//char(200)//char(201)//char(202)//char(203)//char(204)//char(205)// &
                      char(206)//char(207)//char(210)//char(211)//char(212)//char(213)//char(214)// &
                      char(217)//char(218)//char(219)//char(220)//char(224)//char(225)//char(226)// &
                      char(227)//char(228)//char(229)//char(231)//char(232)//char(233)//char(234)// &
                      char(235)//char(236)//char(237)//char(238)//char(239)//char(241)//char(242)// &
                      char(243)//char(244)//char(245)//char(246)//char(249)//char(250)//char(251)// &
                      char(252)//"]"
        d%not_casechars = "[^A-Za-z"//char(192)//char(193)//char(194)//char(195)//char(196)// &
                          char(197)//char(199)//char(200)//char(201)//char(202)//char(203)// &
                          char(204)//char(205)//char(206)//char(207)//char(210)//char(211)// &
                          char(212)//char(213)//char(214)//char(217)//char(218)//char(219)// &
                          char(220)//char(224)//char(225)//char(226)//char(227)//char(228)// &
                          char(229)//char(231)//char(232)//char(233)//char(234)//char(235)// &
                          char(236)//char(237)//char(238)//char(239)//char(241)//char(242)// &
                          char(243)//char(244)//char(245)//char(246)//char(249)//char(250)// &
                          char(251)//char(252)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(22) = d
        d = spellcheck_dictionary_type(name="norsk", many_otherchars=.false., extended_character_mode="~list", &
                                       coding_system="iso-8859-1")
        d%casechars = "[A-Za-z"//char(197)//char(198)//char(199)//char(200)//char(201)//char(210)// &
                      char(212)//char(216)//char(229)//char(230)//char(231)//char(232)//char(233)// &
                      char(242)//char(244)//char(248)//"]"
        d%not_casechars = "[^A-Za-z"//char(197)//char(198)//char(199)//char(200)//char(201)// &
                          char(210)//char(212)//char(216)//char(229)//char(230)//char(231)// &
                          char(232)//char(233)//char(242)//char(244)//char(248)//"]"
        d%otherchars = "[""]"
        spellcheck_base_dictionaries(23) = d
        d = spellcheck_dictionary_type(name="norsk7-tex", many_otherchars=.false., extended_character_mode="~plaintex", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-d"
        d%arguments(2) = "norsk"
        d%casechars = "[A-Za-z{}\'^`]"
        d%not_casechars = "[^A-Za-z{}\'^`]"
        d%otherchars = "[""]"
        spellcheck_base_dictionaries(24) = d
        d = spellcheck_dictionary_type(name="polish", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-2")
        d%casechars = "[A-Za-z"//char(161)//char(163)//char(166)//char(172)//char(175)//char(177)// &
                      char(179)//char(182)//char(188)//char(191)//char(198)//char(202)//char(209)// &
                      char(211)//char(230)//char(234)//char(241)//char(243)//"]"
        d%not_casechars = "[^A-Za-z"//char(161)//char(163)//char(166)//char(172)//char(175)// &
                          char(177)//char(179)//char(182)//char(188)//char(191)//char(198)// &
                          char(202)//char(209)//char(211)//char(230)//char(234)//char(241)// &
                          char(243)//"]"
        d%otherchars = "[.]"
        spellcheck_base_dictionaries(25) = d
        d = spellcheck_dictionary_type(name="portugues", many_otherchars=.true., extended_character_mode="~latin1", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[a-zA-Z"//char(193)//char(194)//char(199)//char(201)//char(211)//char(224)// &
                      char(225)//char(226)//char(233)//char(234)//char(237)//char(243)//char(227)// &
                      char(231)//char(250)//"]"
        d%not_casechars = "[^a-zA-Z"//char(193)//char(194)//char(199)//char(201)//char(211)// &
                          char(224)//char(225)//char(226)//char(233)//char(234)//char(237)// &
                          char(243)//char(227)//char(231)//char(250)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(26) = d
        d = spellcheck_dictionary_type(name="russian", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="koi8-r")
        d%casechars = "["//char(225)//char(226)//char(247)//char(231)//char(228)//char(229)// &
                      char(179)//char(246)//char(250)//char(233)//char(234)//char(235)//char(236)// &
                      char(237)//char(238)//char(239)//char(240)//char(242)//char(243)//char(244)// &
                      char(245)//char(230)//char(232)//char(227)//char(254)//char(251)//char(253)// &
                      char(248)//char(249)//char(255)//char(252)//char(224)//char(241)//char(193)// &
                      char(194)//char(215)//char(199)//char(196)//char(197)//char(163)//char(214)// &
                      char(218)//char(201)//char(202)//char(203)//char(204)//char(205)//char(206)// &
                      char(207)//char(208)//char(210)//char(211)//char(212)//char(213)//char(198)// &
                      char(200)//char(195)//char(222)//char(219)//char(221)//char(216)//char(217)// &
                      char(223)//char(220)//char(192)//char(209)//"]"
        d%not_casechars = "[^"//char(225)//char(226)//char(247)//char(231)//char(228)//char(229)// &
                          char(179)//char(246)//char(250)//char(233)//char(234)//char(235)// &
                          char(236)//char(237)//char(238)//char(239)//char(240)//char(242)// &
                          char(243)//char(244)//char(245)//char(230)//char(232)//char(227)// &
                          char(254)//char(251)//char(253)//char(248)//char(249)//char(255)// &
                          char(252)//char(224)//char(241)//char(193)//char(194)//char(215)// &
                          char(199)//char(196)//char(197)//char(163)//char(214)//char(218)// &
                          char(201)//char(202)//char(203)//char(204)//char(205)//char(206)// &
                          char(207)//char(208)//char(210)//char(211)//char(212)//char(213)// &
                          char(198)//char(200)//char(195)//char(222)//char(219)//char(221)// &
                          char(216)//char(217)//char(223)//char(220)//char(192)//char(209)//"]"
        d%otherchars = ""
        spellcheck_base_dictionaries(27) = d
        d = spellcheck_dictionary_type(name="russianw", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="windows-1251")
        d%casechars = "["//char(192)//char(193)//char(194)//char(195)//char(196)//char(197)// &
                      char(168)//char(198)//char(199)//char(200)//char(201)//char(202)//char(203)// &
                      char(204)//char(205)//char(206)//char(207)//char(208)//char(209)//char(210)// &
                      char(211)//char(212)//char(213)//char(214)//char(215)//char(216)//char(217)// &
                      char(220)//char(219)//char(218)//char(221)//char(222)//char(223)//char(224)// &
                      char(225)//char(226)//char(227)//char(228)//char(229)//char(184)//char(230)// &
                      char(231)//char(232)//char(233)//char(234)//char(235)//char(236)//char(237)// &
                      char(238)//char(239)//char(240)//char(241)//char(242)//char(243)//char(244)// &
                      char(245)//char(246)//char(247)//char(248)//char(249)//char(252)//char(251)// &
                      char(250)//char(253)//char(254)//char(255)//"]"
        d%not_casechars = "[^"//char(192)//char(193)//char(194)//char(195)//char(196)//char(197)// &
                          char(168)//char(198)//char(199)//char(200)//char(201)//char(202)// &
                          char(203)//char(204)//char(205)//char(206)//char(207)//char(208)// &
                          char(209)//char(210)//char(211)//char(212)//char(213)//char(214)// &
                          char(215)//char(216)//char(217)//char(220)//char(219)//char(218)// &
                          char(221)//char(222)//char(223)//char(224)//char(225)//char(226)// &
                          char(227)//char(228)//char(229)//char(184)//char(230)//char(231)// &
                          char(232)//char(233)//char(234)//char(235)//char(236)//char(237)// &
                          char(238)//char(239)//char(240)//char(241)//char(242)//char(243)// &
                          char(244)//char(245)//char(246)//char(247)//char(248)//char(249)// &
                          char(252)//char(251)//char(250)//char(253)//char(254)//char(255)//"]"
        d%otherchars = ""
        spellcheck_base_dictionaries(28) = d
        d = spellcheck_dictionary_type(name="slovak", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-2")
        d%arguments(1) = "-B"
        d%casechars = "[A-Za-z"//char(193)//char(196)//char(201)//char(205)//char(211)//char(218)// &
                      char(212)//char(192)//char(197)//char(165)//char(221)//char(174)//char(169)// &
                      char(200)//char(207)//char(171)//char(210)//char(225)//char(228)//char(233)// &
                      char(237)//char(243)//char(250)//char(244)//char(224)//char(229)//char(181)// &
                      char(253)//char(190)//char(185)//char(232)//char(239)//char(187)//char(242)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(193)//char(196)//char(201)//char(205)//char(211)// &
                          char(218)//char(212)//char(192)//char(197)//char(165)//char(221)// &
                          char(174)//char(169)//char(200)//char(207)//char(171)//char(210)// &
                          char(225)//char(228)//char(233)//char(237)//char(243)//char(250)// &
                          char(244)//char(224)//char(229)//char(181)//char(253)//char(190)// &
                          char(185)//char(232)//char(239)//char(187)//char(242)//"]"
        d%otherchars = ""
        spellcheck_base_dictionaries(29) = d
        d = spellcheck_dictionary_type(name="slovenian", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="iso-8859-2")
        d%arguments(1) = "-B"
        d%arguments(2) = "-d"
        d%arguments(3) = "slovenian"
        d%casechars = "[A-Za-z"//char(193)//char(196)//char(201)//char(205)//char(211)//char(218)// &
                      char(212)//char(192)//char(197)//char(165)//char(221)//char(174)//char(169)// &
                      char(200)//char(207)//char(171)//char(210)//char(225)//char(228)//char(233)// &
                      char(237)//char(243)//char(250)//char(244)//char(224)//char(229)//char(181)// &
                      char(253)//char(190)//char(185)//char(232)//char(239)//char(187)//char(242)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(193)//char(196)//char(201)//char(205)//char(211)// &
                          char(218)//char(212)//char(192)//char(197)//char(165)//char(221)// &
                          char(174)//char(169)//char(200)//char(207)//char(171)//char(210)// &
                          char(225)//char(228)//char(233)//char(237)//char(243)//char(250)// &
                          char(244)//char(224)//char(229)//char(181)//char(253)//char(190)// &
                          char(185)//char(232)//char(239)//char(187)//char(242)//"]"
        d%otherchars = ""
        spellcheck_base_dictionaries(30) = d
        d = spellcheck_dictionary_type(name="svenska", many_otherchars=.false., extended_character_mode="~list", &
                                       coding_system="iso-8859-1")
        d%arguments(1) = "-C"
        d%casechars = "[A-Za-z"//char(229)//char(228)//char(246)//char(233)//char(224)//char(252)// &
                      char(232)//char(230)//char(248)//char(231)//char(197)//char(196)//char(214)// &
                      char(201)//char(192)//char(220)//char(200)//char(198)//char(216)//char(199)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(229)//char(228)//char(246)//char(233)//char(224)// &
                          char(252)//char(232)//char(230)//char(248)//char(231)//char(197)// &
                          char(196)//char(214)//char(201)//char(192)//char(220)//char(200)// &
                          char(198)//char(216)//char(199)//"]"
        d%otherchars = "[']"
        spellcheck_base_dictionaries(31) = d
        d = spellcheck_dictionary_type(name="hebrew", many_otherchars=.false., extended_character_mode="", &
                                       coding_system="cp1255")
        d%arguments(1) = "-B"
        d%casechars = "["//char(224)//char(225)//char(226)//char(227)//char(228)//char(229)// &
                      char(230)//char(231)//char(232)//char(233)//char(235)//char(234)//char(236)// &
                      char(238)//char(237)//char(240)//char(239)//char(241)//char(242)//char(244)// &
                      char(243)//char(247)//char(246)//char(245)//char(248)//char(249)//char(250)// &
                      "]"
        d%not_casechars = "[^"//char(224)//char(225)//char(226)//char(227)//char(228)//char(229)// &
                          char(230)//char(231)//char(232)//char(233)//char(235)//char(234)// &
                          char(236)//char(238)//char(237)//char(240)//char(239)//char(241)// &
                          char(242)//char(244)//char(243)//char(247)//char(246)//char(245)// &
                          char(248)//char(249)//char(250)//"]"
        d%otherchars = ""
        spellcheck_base_dictionaries(32) = d
    end subroutine initialize_base_dictionaries
end module spellcheck_mod
