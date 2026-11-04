! ispell_mod.f90 -- interface to spell checkers (Ispell, Aspell, Hunspell).
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
!> (`ispell_word') checks the word at point, `ispell_region',
!> `ispell_buffer', `ispell' and `ispell_comments_and_strings' check
!> text, and a misspelled word shows its corrections in the *Choices*
!> buffer above the selected window, with single keys to choose (see
!> `ispell_help').  The dictionaries are records of
!> `ispell_dictionary_type' in `ispell_dictionaries' (built from
!> `ispell_base_dictionaries' and, for Aspell, the dictionaries it
!> reports) and `ispell_local_dictionaries' (the configuration's own).
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
!> (`ispell_help_in_buffer' electric, ehelp.el), horizontal scrolling
!> (`truncate_lines' is not ported) and flyspell.el.
module ispell_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, vtag, V_INTEGER, V_STRING, xfixnum, make_list, command_type, xstring, &
                         list_elements
    use data_mod, only: string_setting_type, logical_setting_type, integer_setting_type, forward_setting
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          erase_buffer, get_buffer, generate_new_buffer_name, is_buffer_live, fill_column, &
                          transient_mark_mode, set_buffer_modified, restore_buffer_modified, resolve_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char, point, point_min, point_max, line_end_position, is_eol, is_bob, &
                           delete_region, buffer_substring_no_properties, buffer_string, char_after, region_beginning, &
                           region_end
    use cmds_mod, only: forward_line, forward_char, backward_char, end_of_line, beginning_of_line
    use indent_mod, only: current_column
    use marker_mod, only: make_marker, set_marker, marker_position, marker_buffer, destroy_marker
    use search_mod, only: re_search_forward, re_search_backward, search_forward, search_backward, looking_at, &
                          string_match, match_beginning, match_end, regexp_quote, case_fold_search, SEARCH_NOERROR, &
                          SEARCH_MOVE
    use subr_mod, only: match_string_no_properties, read_y_or_n, read_key
    use casefiddle_mod, only: upcase_string, downcase_string
    use regexp_opt_mod, only: regexp_opt, regexp_unmatchable
    use fileio_mod, only: expand_file_name, file_name_nondirectory, is_file_readable, is_file_accessible_directory, &
                          is_file_name_absolute, insert_file_contents
    use files_mod, only: executable_find, temporary_file_directory
    use dired_mod, only: directory_files
    use env_mod, only: getenv
    use callproc_mod, only: execute_process
    use coding_mod, only: decode_coding_string, encode_coding_string
    use string_type_mod, only: string_type, append_string, prepend_string
    use process_mod, only: make_process, delete_process, is_process_live, process_status, &
                           set_process_filter, set_process_coding_system, set_process_query_on_exit_flag, &
                           accept_process_output, process_send_string, PROCESS_CONNECTION_PIPE, &
                           PROCESS_CONNECTION_PTY
    use syntax_mod, only: parse_state_type, parse_partial_expression, syntax_ppss, COMMENTSTOP_SYNTAX_TABLE
    use expression_motion_mod, only: forward_expression
    use newcomment_mod, only: comment_start, comment_end, comment_normalize_vars
    use simple_mod, only: is_use_region, is_region_active, push_mark, newline, open_line
    use undo_mod, only: undo_boundary
    use abbrev_mod, only: global_abbrev_table
    use replace_mod, only: query_replace
    use minibuf_mod, only: read_string
    use minibuf_table_mod, only: completion_table_type, TABLE_LIST
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use keyboard_mod, only: recursive_edit, sit_for, help_char, unread_command_events
    use keymap_mod, only: key_binding
    use callint_mod, only: current_prefix_arg, PREFIX_NONE, command_of
    use frame_mod, only: selected_frame, selected_window
    use window_mod, only: window_configuration_type, current_window_configuration, set_window_configuration, &
                          split_window, select_window, set_window_buffer, fit_window_to_buffer, get_buffer_window, &
                          display_buffer, window_body_width, window_resize, windows
    use itree_mod, only: make_overlay, overlay_put, delete_overlay, move_overlay, is_overlay
    use faces_mod, only: named_face_type
    use isearch_mod, only: isearch
    use term_mod, only: is_tty_display_color
    use dispnew_mod, only: ding, redraw_display
    use help_mod, only: substitute_command_keys
    use xdisp_mod, only: message, message_log_max, message_log_max_choice_setting_type
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: ispell_word, ispell_region, ispell_buffer, ispell, ispell_continue, ispell_comments_and_strings, &
              ispell_comment_or_string_at_point, ispell_change_dictionary, ispell_kill_ispell, ispell_pdict_save, &
              ispell_check_version, ispell_help, ispell_lookup_words, ispell_set_spellchecker_params, &
              ispell_valid_dictionary_list, ispell_init_process, ispell_send_string, ispell_accept_output, &
              ispell_filter, ispell_parse_output, ispell_get_word, is_ispell_correct, ispell_process_status, &
              ispell_add_per_file_word_list, ispell_buffer_local_words, ispell_buffer_local_dict, &
              ispell_buffer_local_parsing, ispell_accept_buffer_local_defs, ispell_internal_change_dictionary, &
              ispell_get_casechars, ispell_get_not_casechars, ispell_get_otherchars, ispell_get_many_otherchars, &
              ispell_get_coding_system, ispell_get_extended_character_mode, &
              prepend_ispell_buffer_session_localword, ispell_format_word_procedure, ispell_skip_procedure, &
              ispell_tex_arg_end, init_ispell

    !> A dictionary and its parameters, an element of `ispell_dictionaries'
    !> and `ispell_local_dictionaries' (upstream's (DICTIONARY-NAME
    !> CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P ISPELL-ARGS
    !> EXTENDED-CHARACTER-MODE CHARACTER-SET)):
    !>
    !> `name' is a possible value of `ispell_dictionary'; "" means the
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
    !> `args' are additional arguments passed to the ispell subprocess (""
    !> for none).
    !>
    !> `extended_character_mode' should be used when dictionaries are used
    !> which have been configured in an Ispell affix file.  (For example,
    !> umlauts can be encoded as \"a, a\", "a, ...)  Defaults are ~tex and
    !> ~nroff in English.  This has the same effect as the command-line
    !> `-T' option.  The buffer major mode controls Ispell's parsing in tex
    !> or nroff mode, but the dictionary can control the extended
    !> character mode.  Both defaults can be overruled in a buffer-local
    !> fashion.  See `ispell_parsing_keyword' for details on this.
    !>
    !> `coding_system' is used to encode text sent to the ispell subprocess
    !> when the language uses non-ASCII characters.
    type, public :: ispell_dictionary_type
        character(len=64) :: name = ""
        character(len=256) :: casechars = ""
        character(len=256) :: not_casechars = ""
        character(len=64) :: otherchars = ""
        logical :: many_otherchars = .false.
        character(len=64) :: args(4) = ""
        character(len=16) :: extended_character_mode = ""
        character(len=16) :: coding_system = ""
    end type ispell_dictionary_type

    abstract interface
        !> `ispell_format_word_function': WORD as shown in messages.
        !> Not a port.
        subroutine ispell_format_word_procedure(word, formatted)
            implicit none(type, external)
            character(len=*), intent(in) :: word
            character(len=:), allocatable, intent(out) :: formatted
        end subroutine ispell_format_word_procedure

        !> The function of a region to skip: moves point to the end of the
        !> region, given ARG (upstream's ARGS).  Not a port.
        recursive subroutine ispell_skip_procedure(arg, status)
            import :: status_type
            implicit none(type, external)
            integer, intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine ispell_skip_procedure
    end interface

    !> A region not to spell check, an element of `ispell_skip_regions'
    !> (upstream's (KEY), (KEY . REGEXP), (KEY REGEXP) and (KEY FUNCTION
    !> ARGS)): `beginning_regexp' is the key; the region ends after the
    !> next match of `end_regexp' (searched up to the end of the buffer),
    !> or where `skip' leaves point when called with `arg'; with neither,
    !> just the key is skipped.
    type, public :: ispell_skip_region_type
        character(len=256) :: beginning_regexp = ""
        character(len=256) :: end_regexp = ""
        procedure(ispell_skip_procedure), pointer, nopass :: skip => null()
        integer :: arg = 1
    end type ispell_skip_region_type

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    !> The `ispell_highlight' value besides no highlighting (.false.) and
    !> highlighting (.true.): highlighting assuming a block cursor on terminals.
    integer, parameter, public :: ISPELL_HIGHLIGHT_BLOCK = 2

    !> Highlight spelling errors when true.
    !> When set to ISPELL_HIGHLIGHT_BLOCK, assumes a block cursor with TTY
    !> displays.  Port of ispell.el ispell-highlight-p.
    type(choice_setting_type), public :: ispell_highlight = &
                                         choice_setting_type(name="ispell_highlight", choice=ISPELL_HIGHLIGHT_BLOCK)

    !> Face used for Ispell highlighting.
    !> This variable can be set by the user to whatever face they desire.
    !> It's most convenient if the cursor color and highlight color are
    !> slightly different.  Port of ispell.el ispell-highlight-face.
    type(named_face_type), pointer, public :: ispell_highlight_face => isearch

    !> The `ispell_check_comments' value besides comments not checked
    !> (.false.) and checked (.true.): the only text checked.
    integer, parameter, public :: ISPELL_CHECK_COMMENTS_EXCLUSIVE = 2

    !> Spelling of comments checked when true.
    !> When set to ISPELL_CHECK_COMMENTS_EXCLUSIVE, ONLY comments are checked.
    !> (For code comments).  Warning!  Not checking comments, when a
    !> comment start is embedded in strings, may produce undesired results.
    !> Port of ispell.el ispell-check-comments.
    type(choice_setting_type), public :: ispell_check_comments = &
                                         choice_setting_type(name="ispell_check_comments", value=.true.)

    !> Corrections made throughout region when true.
    !> Uses `query_replace' (M-%) for corrections.  Port of ispell.el
    !> ispell-query-replace-choices.
    logical, public :: ispell_query_replace_choices = .false.

    !> Whether to save spelling corrections as abbrevs by default.
    !> Determines the default behavior of Ispell after correcting a
    !> misspelled word.  True means to save a global abbrev that expands
    !> the misspelled word to its correction.  This behavior may be toggled
    !> on a per-word basis by typing C-u immediately before selecting a
    !> replacement in the Ispell command loop.  Port of ispell.el
    !> ispell-save-corrections-as-abbrevs.
    logical, public :: ispell_save_corrections_as_abbrevs = .false.

    !> Does not spell check `tib' bibliography references when true.
    !> Skips any text between strings matching regular expressions
    !> `ispell_tib_ref_beginning' and `ispell_tib_ref_end'.
    !>
    !> TeX users beware:  Any text between [. and .] will be skipped -- even
    !> if that's your whole buffer -- unless you set `ispell_skip_tib' to
    !> .false.  That includes the [.5mm] type of number...  Port of
    !> ispell.el ispell-skip-tib.
    logical, public :: ispell_skip_tib = .false.

    !> Regexp matching the beginning of a Tib reference.  Port of ispell.el
    !> ispell-tib-ref-beginning.
    character(len=:), allocatable, public :: ispell_tib_ref_beginning

    !> Regexp matching the end of a Tib reference.  Port of ispell.el
    !> ispell-tib-ref-end.
    character(len=:), allocatable, public :: ispell_tib_ref_end

    !> If true, keep the `*Choices*' window for the entire spelling session.
    !> This minimizes redisplay thrashing.  Port of ispell.el
    !> ispell-keep-choices-win.
    logical, public :: ispell_keep_choices_win = .true.

    !> The default size of the `*Choices*' window, including the mode line.
    !> Must be greater than 1.  Port of ispell.el
    !> ispell-choices-win-default-height.
    integer, public :: ispell_choices_win_default_height = 2

    !> Program invoked by M-$ and M-x ispell_region commands.  Port of
    !> ispell.el ispell-program-name (the first of aspell, ispell,
    !> hunspell and enchant-2 found in `exec_path', else "ispell").
    character(len=:), allocatable, public :: ispell_program_name

    !> Alternate plain word-list dictionary for spelling help.
    !> This is also used by `ispell_lookup_words'.  Port of ispell.el
    !> ispell-alternate-dictionary ("" for none).
    character(len=:), allocatable, public :: ispell_alternate_dictionary

    !> Plain word-list dictionary used for word completion.
    !> This word-list is used if it is different from
    !> `ispell_alternate_dictionary'.
    !> This is also used by `ispell_lookup_words'.  Port of ispell.el
    !> ispell-complete-word-dict ("" for none).
    character(len=:), allocatable, public :: ispell_complete_word_dict

    !> Name of the grep command for search processes.  Port of ispell.el
    !> ispell-grep-command.
    character(len=:), allocatable, public :: ispell_grep_command

    !> String of options to use when running the program in
    !> `ispell_grep_command'.  Should probably be "-Ei".  Port of ispell.el
    !> ispell-grep-options.
    character(len=:), allocatable, public :: ispell_grep_options

    !> Name of the look command for search processes.
    !> This must be an absolute file name.  Port of ispell.el
    !> ispell-look-command ("" for none).
    character(len=:), allocatable, public :: ispell_look_command

    !> String of command options for `ispell_look_command'.  Port of
    !> ispell.el ispell-look-options.
    character(len=:), allocatable, public :: ispell_look_options

    !> When true, Fortmacs uses ptys to communicate with Ispell.
    !> When false, Fortmacs uses pipes.  Port of ispell.el ispell-use-ptys-p.
    logical, public :: ispell_use_ptys = .false.

    !> True means `ispell_word' checks the word around or after point.
    !> Otherwise `ispell_word' checks the preceding word.  Port of ispell.el
    !> ispell-following-word.
    logical, public :: ispell_following_word = .false.

    !> True means display interactive keymap help in a buffer: pop up a
    !> new buffer and display a short help message there for a couple of
    !> seconds.  If false, expand the minibuffer and display the message
    !> there for a couple of seconds.  Port of ispell.el
    !> ispell-help-in-bufferp (its value `electric', a help buffer to
    !> browse and exit, is not ported: Deferred).
    logical, public :: ispell_help_in_buffer = .false.

    !> True means suppress messages in `ispell_word'.  Port of ispell.el
    !> ispell-quietly.
    logical, public :: ispell_quietly = .false.

    !> Formatting function for displaying word being spell checked.
    !> The function must take one string argument and return a string.
    !> Port of ispell.el ispell-format-word-function (`upcase').
    procedure(ispell_format_word_procedure), pointer, public :: ispell_format_word_function => upcase_word

    !> File name of your personal spelling dictionary, or "".
    !> If "", the default personal dictionary for your spelling checker is
    !> used.  Due to a misfeature of Hunspell, if the value is an absolute
    !> file name, the file by that name must already exist for Hunspell to
    !> be able to use it.  Port of ispell.el ispell-personal-dictionary.
    character(len=:), allocatable, public :: ispell_personal_dictionary

    !> When true, save personal dictionary without asking for confirmation.
    !> Port of ispell.el ispell-silently-savep.
    logical, public :: ispell_silently_save = .false.

    !> True means the user has explicitly set this buffer's Ispell
    !> dictionary.  Port of ispell.el ispell-local-dictionary-overridden.
    type(logical_setting_type), target, public :: ispell_local_dictionary_overridden = &
                                                  logical_setting_type(name="ispell_local_dictionary_overridden")

    !> If set, the dictionary to be used for Ispell commands in this buffer.
    !> The value must be a string dictionary name, or unset, which means
    !> use the global setting in `ispell_dictionary'.  Dictionary names are
    !> defined in `ispell_local_dictionaries' and `ispell_dictionaries'.
    !>
    !> Setting `ispell_local_dictionary' to a value has the same effect as
    !> calling M-x ispell_change_dictionary with that value.  This variable
    !> is automatically set when defined in the file with
    !> `ispell_dictionary_keyword'.  Port of ispell.el
    !> ispell-local-dictionary.
    type(string_setting_type), target, public :: ispell_local_dictionary = &
                                                 string_setting_type(name="ispell_local_dictionary")

    !> Default dictionary to use if `ispell_local_dictionary' is unset ("" for
    !> the default).  Port of ispell.el ispell-dictionary.
    character(len=:), allocatable, public :: ispell_dictionary

    !> Extra switches to pass to the Ispell program.
    !> For example, ["-W", "3"] to cause it to accept all 1-3 character
    !> words as correct.  See also `ispell_dictionaries', which may be used
    !> for language-specific arguments.  Port of ispell.el ispell-extra-args.
    character(len=64), allocatable, public :: ispell_extra_args(:)

    !> The `ispell_skip_html' value besides .true. and .false.
    integer, parameter, public :: ISPELL_SKIP_HTML_USE_MODE_NAME = 2

    !> Indicates whether ispell should skip spell checking of SGML markup.
    !> If .true., always skip SGML markup; if
    !> .false., never skip; if ISPELL_SKIP_HTML_USE_MODE_NAME,
    !> guess whether SGML markup should be skipped according to the name of
    !> the buffer's major mode.
    !>
    !> SGML markup is any text inside the brackets "<>" or entities such as
    !> "&amp;".  See `ispell_html_skip_regions' for more details.
    !>
    !> This variable affects spell-checking of HTML, XML, and SGML files.
    !> Port of ispell.el ispell-skip-html.
    type(choice_setting_type), target, public :: ispell_skip_html = &
                                                 choice_setting_type(name="ispell_skip_html", choice=ISPELL_SKIP_HTML_USE_MODE_NAME)

    !> List of local or customized dictionary definitions.
    !> These can override the values in `ispell_dictionaries'.  Port of
    !> ispell.el ispell-local-dictionary-alist.
    type(ispell_dictionary_type), allocatable, public :: ispell_local_dictionaries(:)

    !> The number of seconds to display the help text.  Port of ispell.el
    !> ispell-help-timeout.
    real, public :: ispell_help_timeout = 30.0

    !> Base value for `ispell_dictionaries'.
    !>
    !> Note that when the speller program is "aspell" or "hunspell", some
    !> parts of the database, notably `otherchars', will be overridden by
    !> parsing the dictionary data files.  Port of ispell.el
    !> ispell-dictionary-base-alist.
    type(ispell_dictionary_type), allocatable, public :: ispell_base_dictionaries(:)

    !> The dictionaries and their associated parameters (see
    !> `ispell_dictionary_type').
    !>
    !> Note that with "ispell" as the speller, the `casechars' and
    !> `otherchars' slots should contain the same character set as
    !> casechars and otherchars in the LANGUAGE.aff file (e.g.,
    !> english.aff).  Aspell and Hunspell don't have this limitation.  Port
    !> of ispell.el ispell-dictionary-alist.
    type(ispell_dictionary_type), allocatable, public :: ispell_dictionaries(:)

    !> The parsed Aspell dictionaries.  Internal use.  Port of ispell.el
    !> ispell-aspell-dictionary-alist.
    type(ispell_dictionary_type), allocatable, public :: ispell_aspell_dictionaries(:)

    !> The identified aspell dictionaries.  Port of ispell.el
    !> ispell--aspell-found-dictionaries.
    type(ispell_dictionary_type), allocatable, public :: ispell_aspell_found_dictionaries(:)

    !> A standard dictionary name and its locale, an element of
    !> `ispell_dicts_name2locale_equivs'.
    type, public :: ispell_locale_equiv_type
        character(len=16) :: name = ""
        character(len=8) :: locale = ""
    end type ispell_locale_equiv_type

    !> Known matching locales for standard dict names.
    !> Standard dict names are defined in `ispell_base_dictionaries'.  Port
    !> of ispell.el ispell-dicts-name2locale-equivs-alist.
    type(ispell_locale_equiv_type), allocatable, public :: ispell_dicts_name2locale_equivs(:)

    !> The version of Aspell if we can use Aspell extensions, else "".  Port
    !> of ispell.el ispell-really-aspell.
    character(len=:), allocatable, public :: ispell_really_aspell
    !> The version of Hunspell if we can use Hunspell extensions, else "".
    !> Port of ispell.el ispell-really-hunspell.
    character(len=:), allocatable, public :: ispell_really_hunspell
    !> The version of Enchant if we can use Enchant extensions, else "".
    !> Port of ispell.el ispell-really-enchant.
    character(len=:), allocatable, public :: ispell_really_enchant

    !> Command line option prefix to select encoding if supported, ""
    !> otherwise.  If setting the encoding is supported by spellchecker and
    !> is selectable from the command line, this variable will contain
    !> "--encoding=" for Aspell and "-i" for Hunspell, so the appropriate
    !> mime charset can be selected.  That will be set in
    !> `ispell_check_version' for Hunspell >= 1.1.6 and Aspell >= 0.60.
    !>
    !> For Aspell, non-empty also means to try to automatically find its
    !> dictionaries.  Port of ispell.el ispell-encoding8-command.
    character(len=:), allocatable, public :: ispell_encoding8_command

    !> Directory where ispell dictionaries reside ("" if unknown).  Port of
    !> ispell.el ispell-library-directory.
    character(len=:), allocatable, public :: ispell_library_directory

    !> Data directory of Aspell.  Port of ispell.el ispell-aspell-data-dir.
    character(len=:), allocatable, public :: ispell_aspell_data_dir

    !> Dictionary directory of Aspell.  Port of ispell.el
    !> ispell-aspell-dict-dir.
    character(len=:), allocatable, public :: ispell_aspell_dict_dir

    !> Normal hook run on spellchecker initialization.
    !> This hook is run when a spellchecker is used for the first time,
    !> before `ispell_dictionaries' is set.  Port of ispell.el
    !> ispell-initialize-spellchecker-hook.
    type(hook_type), target, public :: ispell_initialize_spellchecker_hook = &
                                       hook_type(name="ispell_initialize_spellchecker_hook")

    !> Hook run by `ispell_kill_ispell' (flyspell flushes its caches
    !> there).  Port of ispell.el ispell-kill-ispell-hook.
    type(hook_type), target, public :: ispell_kill_ispell_hook = hook_type(name="ispell_kill_ispell_hook")

    !> Hook run after changing dictionary.  Port of ispell.el
    !> ispell-change-dictionary-hook.
    type(hook_type), target, public :: ispell_change_dictionary_hook = hook_type(name="ispell_change_dictionary_hook")

    !> A normal hook invoked from the ispell command loop.
    !> It is called once per iteration, before displaying a prompt to the
    !> user.  Port of ispell.el ispell-update-post-hook.
    type(hook_type), target, public :: ispell_update_post_hook = hook_type(name="ispell_update_post_hook")

    !> True means personal dictionary has modifications to be saved.  Port
    !> of ispell.el ispell-pdict-modified-p.
    logical, public :: ispell_pdict_modified = .false.

    !> If true, `ispell_word' does not try to correct the word.  Port of
    !> ispell.el ispell-check-only.
    logical, public :: ispell_check_only = .false.

    !> The name of the current dictionary, or "" for the default.
    !> This is passed to the Ispell process using the `-d' switch and is
    !> used as key in `ispell_local_dictionaries' and
    !> `ispell_dictionaries'.  Port of ispell.el ispell-current-dictionary.
    character(len=:), allocatable, public :: ispell_current_dictionary

    !> The name of the current personal dictionary, or "" for the default.
    !> This is passed to the Ispell process using the `-p' switch.  Port of
    !> ispell.el ispell-current-personal-dictionary.
    character(len=:), allocatable, public :: ispell_current_personal_dictionary

    !> The buffer used for the choices.  Port of ispell.el
    !> ispell-choices-buffer.
    character(len=*), parameter, public :: ispell_choices_buffer = "*Choices*"

    !> The keyword for local oddly-spelled words to accept.
    !> The keyword will be followed by any number of local word spellings.
    !> There can be multiple instances of this keyword in the file.  Port
    !> of ispell.el ispell-words-keyword.
    character(len=*), parameter, public :: ispell_words_keyword = "LocalWords: "

    !> The keyword for a local dictionary to use.
    !> The keyword must be followed by a valid dictionary name, defined in
    !> `ispell_local_dictionaries' or `ispell_dictionaries'.  When multiple
    !> occurrences exist, the last keyword definition is used.  Port of
    !> ispell.el ispell-dictionary-keyword.
    character(len=*), parameter, public :: ispell_dictionary_keyword = "Local IspellDict: "

    !> The keyword for defining buffer local dictionaries.
    !> Keyword must be followed by the filename of a personal dictionary.
    !> The last occurring definition in the buffer will be used.  Port of
    !> ispell.el ispell-pdict-keyword.
    character(len=*), parameter, public :: ispell_pdict_keyword = "Local IspellPersDict: "

    !> The keyword for overriding default Ispell parsing.
    !> The above keyword string should be followed by `latex_mode' or
    !> `nroff_mode' to put the current buffer into the desired parsing mode.
    !>
    !> Extended character mode can be changed for this buffer by placing a
    !> `~' followed by an extended-character mode -- such as `~.tex'.  The
    !> last occurring definition in the buffer will be used.  Port of
    !> ispell.el ispell-parsing-keyword.
    character(len=*), parameter, public :: ispell_parsing_keyword = "Local IspellParsing: "

    !> Beginning and end of regions not to spell check (see
    !> `ispell_skip_region_type').  Port of ispell.el
    !> ispell-skip-region-alist.
    type(ispell_skip_region_type), allocatable, public :: ispell_skip_regions(:)

    !> Regions to be skipped in TeX mode, used raw.  Delete or add any
    !> regions you want to be automatically selected for skipping in latex
    !> mode.  Port of ispell.el ispell-tex-skip-alists (its first list).
    type(ispell_skip_region_type), allocatable, public :: ispell_tex_skip_regions(:)

    !> Regions to be skipped in TeX mode, with the key placed inside
    !> \begin{}.  Port of ispell.el ispell-tex-skip-alists (its second
    !> list).
    type(ispell_skip_region_type), allocatable, public :: ispell_tex_skip_environments(:)

    !> Start and end keys to skip in HTML buffers.
    !> Same format as `ispell_skip_regions'.  Note - substrings of other
    !> matches must come last (e.g. "<[tT][tT]/" and "<[^ \t\n>]").  Port
    !> of ispell.el ispell-html-skip-alists.
    type(ispell_skip_region_type), allocatable, public :: ispell_html_skip_regions(:)

    !> A buffer local variable containing the current personal dictionary.
    !> If set, the value must be a string, which is a file name.
    !>
    !> If you specify a personal dictionary for the current buffer which is
    !> different from the current personal dictionary, the effect is
    !> similar to calling M-x ispell_change_dictionary.  This variable is
    !> automatically set when defined in the file with
    !> `ispell_pdict_keyword'.
    !>
    !> Due to a misfeature of Hunspell, if the value is an absolute file
    !> name, the file by that name must already exist for Hunspell to be
    !> able to use it.  Port of ispell.el ispell-local-pdict.
    type(string_setting_type), target, public :: ispell_local_pdict = string_setting_type(name="ispell_local_pdict")

    !> `ispell_parser' values.
    integer, parameter, public :: ISPELL_PARSER_USE_MODE_NAME = 0, ISPELL_PARSER_TEX = 1, ISPELL_PARSER_NROFF = 2

    !> Indicates whether ispell should parse the current buffer as TeX Code.
    !> ISPELL_PARSER_USE_MODE_NAME tries to guess using the name of the
    !> major mode.  Default parser is nroff.  Currently the only other valid
    !> parser is ISPELL_PARSER_TEX.  Port of ispell.el ispell-parser.
    type(integer_setting_type), target, public :: ispell_parser = &
                                                  integer_setting_type(name="ispell_parser", value=ISPELL_PARSER_USE_MODE_NAME)

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    ! ispell-process: the process object for Ispell.
    type(handle_type) :: ispell_process
    ! ispell-process-directory: the directory where it was started.
    character(len=:), allocatable :: ispell_process_directory
    ! ispell-filter: the output lines, in the order they arrived.
    type(string_type), allocatable :: ispell_filter_variable(:)
    ! ispell-filter-continue: the last line is incomplete.
    logical :: ispell_filter_continue = .false.
    ! ispell-last-program-name (unallocated: never set).
    character(len=:), allocatable :: ispell_last_program_name
    ! ispell-buffer-local-name ("" for none).
    character(len=:), allocatable :: ispell_buffer_local_name
    ! ispell-quit: the session is terminated; with `quit_position', the
    ! position to continue from.
    logical :: ispell_quit = .false.
    integer(int64) :: ispell_quit_position = 0
    ! ispell-region-end and ispell-recursive-edit-marker.
    type(handle_type) :: ispell_region_end, ispell_recursive_edit_marker
    ! ispell-overlay.
    type(handle_type) :: ispell_overlay
    ! ispell--abbrev-saving-allowed and ispell--save-correction-as-abbrev.
    logical :: ispell_abbrev_saving_allowed = .false., ispell_save_correction_as_abbrev = .false.

    !> The words of a buffer's `ispell_buffer_session_localwords'.
    type :: session_localwords_type
        integer(int64) :: serial = 0
        type(string_type), allocatable :: words(:)
    end type session_localwords_type

    ! ispell-buffer-session-localwords, per buffer.
    type(session_localwords_type), allocatable :: session_localwords(:)

    ! What `ispell_command_loop' returns (upstream's nil, 0, a string and
    ! the lists).
    integer, parameter :: REPLACE_KEEP = 0, REPLACE_LOCAL = 1, REPLACE_WORD = 2, REPLACE_RECHECK = 3, &
                          REPLACE_RECHECK_HERE = 4, REPLACE_QUERY = 5

    ! A result of `ispell_command_loop': `kind' and the word.
    type :: replacement_type
        integer :: kind = REPLACE_KEEP
        character(len=:), allocatable :: word
    end type replacement_type

    !> `ispell_parse_output' kinds.
    integer, parameter, public :: ISPELL_OUTPUT_CORRECT = 0, ISPELL_OUTPUT_ROOT = 1, ISPELL_OUTPUT_MISSPELLED = 2, &
                                  ISPELL_OUTPUT_ERROR = 3

    !> What `ispell_parse_output' returns: `kind' ISPELL_OUTPUT_CORRECT for
    !> an exact match, ISPELL_OUTPUT_ROOT with the root word matched via
    !> suffix removal in `word', ISPELL_OUTPUT_MISSPELLED with the possibly
    !> misspelled `word', its line `offset', the possible correct spellings
    !> (`misses') and the possibly valid affix constructions (`guesses'),
    !> ISPELL_OUTPUT_ERROR when an error has occurred.
    type, public :: ispell_output_type
        integer :: kind = ISPELL_OUTPUT_ERROR
        character(len=:), allocatable :: word
        integer(int64) :: offset = 0
        type(string_type), allocatable :: misses(:), guesses(:)
    end type ispell_output_type

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    ! ------------------------------------------------------------------
    ! The speller program
    ! ------------------------------------------------------------------

    !> `ispell_format_word_function's default: WORD in upper case.
    !> Not a port.
    subroutine upcase_word(word, formatted)
        character(len=*), intent(in) :: word
        character(len=:), allocatable, intent(out) :: formatted

        formatted = upcase_string(word)
    end subroutine upcase_word

    ! WORD formatted with `ispell_format_word_function'.  Not a port.
    function formatted_word(word) result(text)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text

        call ispell_format_word_function(word, text)
    end function formatted_word

    ! The value of a string setting, "" when it is unset.  Not a port.
    function string_value(setting) result(text)
        type(string_setting_type), intent(in) :: setting
        character(len=:), allocatable :: text

        text = ""
        if (allocated(setting%value)) text = setting%value
    end function string_value

    ! Whether version A is lower than or equal to B (version<=, for versions
    ! of numbers and dots).  Not a port.
    logical function is_version_le(a, b)
        character(len=*), intent(in) :: a, b
        integer :: ia, ib, na, nb

        ia = 1
        ib = 1

        do
            na = next_number(a, ia)
            nb = next_number(b, ib)

            if (na /= nb) then
                is_version_le = na < nb
                return
            end if

            if (ia > len(a) .and. ib > len(b)) then
                is_version_le = .true.
                return
            end if
        end do

    contains

        integer function next_number(s, i) result(n)
            character(len=*), intent(in) :: s
            integer, intent(inout) :: i

            n = 0
            if (i > len(s)) return

            do while (i <= len(s))
                if (s(i:i) == ".") exit
                if (s(i:i) >= "0" .and. s(i:i) <= "9") n = 10*n + iachar(s(i:i)) - 48
                i = i + 1
            end do

            i = i + 1
        end function next_number
    end function is_version_le

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

    ! Make a temporary buffer current (with-temp-buffer), in directory DIR
    ! (the current buffer's when absent); OLD is the buffer to go back to.
    ! Not a port.
    subroutine temp_begin(old, temp, dir)
        type(handle_type), intent(out) :: old, temp
        character(len=*), intent(in), optional :: dir
        character(len=:), allocatable :: directory
        type(status_type) :: st

        old = current_buffer()
        directory = "./"

        if (current /= 0) then
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        end if

        if (present(dir)) directory = dir
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temp, st)
        call set_buffer(temp, st)
        buffers(current)%directory = directory
    end subroutine temp_begin

    ! Close the temporary buffer TEMP and make OLD current again.
    ! Not a port.
    subroutine temp_end(old, temp)
        type(handle_type), intent(in) :: old, temp
        type(status_type) :: st

        call close_buffer(temp, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine temp_end

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
    function safe_default_directory() result(dir)
        character(len=:), allocatable :: dir
        type(status_type) :: st

        dir = "./"

        if (current /= 0) then
            if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
        end if

        if (is_file_accessible_directory(dir)) return
        call expand_file_name("~/", dir, st)
    end function safe_default_directory

    ! Run PROGRAM with ARGS, its output inserted in the current buffer, in a
    ! safe default directory; RESULT is its exit status or the signal
    ! description.  Port of ispell.el:ispell-call-process.
    subroutine ispell_execute_process(program, args, result, status)
        character(len=*), intent(in) :: program
        type(value_type), intent(in) :: args
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved

        call save_directory(saved)
        buffers(current)%directory = safe_default_directory()
        call execute_process(program, result, status, destination=value_type(.true.), args=args)
        call restore_directory(saved)
    end subroutine ispell_execute_process

    !> Ensure that `ispell_program_name' is valid and has the correct
    !> version.  Return version number if called interactively, or if
    !> INTERACTIVE is true.  Otherwise return the library directory name,
    !> if that is defined ("" otherwise).  Port of
    !> ispell.el:ispell-check-version.
    recursive subroutine ispell_check_version(result, status, interactive)
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: old, temp
        type(value_type) :: code
        character(len=:), allocatable :: libvar, version, minver, envval
        logical :: envfound
        integer(int64) :: found
        logical :: saved_fold, interactivep
        type(status_type) :: st

        interactivep = .false.
        if (present(interactive)) interactivep = interactive
        result = ""
        call temp_begin(old, temp, temporary_file_directory())
        call ispell_execute_process(ispell_program_name, list(value_type(text="-vv")), code, status)

        if (.not. status%ok()) then
            call temp_end(old, temp)
            return
        end if

        call goto_char(point_min())

        if (interactivep) then
            ! Report version information of ispell.
            call end_of_line()
            call buffer_substring_no_properties(point_min(), point(), result, st)
            call message(result)
        else
            ! Return LIBDIR or LIBRARYVAR (overrides LIBDIR) env.
            result = config_var("LIBDIR")
            libvar = config_var("LIBRARYVAR")

            if (len(libvar) > 0) then
                call getenv(libvar, envval, envfound)
                if (envfound .and. len(envval) > 0) result = envval
            end if
        end if

        call goto_char(point_min())

        if (vtag(code) == V_STRING) then
            call set_error(status, FM_GENERIC_ERROR, ispell_program_name//" exited with signal "//xstring(code))
            call temp_end(old, temp)
            return
        else if (vtag(code) == V_INTEGER) then
            if (xfixnum(code) /= 0) then
                call set_error(status, FM_GENERIC_ERROR, ispell_program_name//" exited with code "// &
                               integer_text(xfixnum(code)))
                call temp_end(old, temp)
                return
            end if
        end if

        ! Get relevant version strings.
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        version = ""
        call re_search_forward("\([0-9]+\.[0-9.]+\)", found, st, noerror=SEARCH_NOERROR)
        if (st%ok() .and. found > 0) version = match_string_no_properties(1)
        ! Make sure these variables are (re-)initialized to the default value.
        ispell_really_aspell = ""
        ispell_really_hunspell = ""
        ispell_really_enchant = ""
        ispell_encoding8_command = ""
        ispell_really_aspell = really("Aspell")
        if (len(ispell_really_aspell) == 0) ispell_really_hunspell = really("Hunspell")

        if (len(ispell_really_aspell) == 0 .and. len(ispell_really_hunspell) == 0) then
            ispell_really_enchant = really("Enchant")
        end if

        case_fold_search = saved_fold
        call temp_end(old, temp)
        minver = ""

        if (.not. is_version_le("3.1.12", version)) then
            minver = "3.1.12"
        else if (len(ispell_really_aspell) > 0) then
            if (.not. is_version_le("0.60", ispell_really_aspell)) minver = "0.60"
        else if (len(ispell_really_enchant) > 0) then
            if (.not. is_version_le("2.1.0", ispell_really_enchant)) minver = "2.1.0"
        end if

        if (len(minver) > 0) then
            call set_error(status, FM_GENERIC_ERROR, ispell_program_name//" release "//minver//" or greater is required")
            return
        end if

        if (len(ispell_really_aspell) > 0) then
            ispell_encoding8_command = "--encoding="
        else if (len(ispell_really_hunspell) > 0) then
            if (is_version_le("1.1.6", ispell_really_hunspell)) then
                ispell_encoding8_command = "-i"
            else
                ispell_really_hunspell = ""
            end if
        end if

    contains

        ! The value of the configuration variable VAR in the output, "" if
        ! absent.
        function config_var(var) result(value)
            character(len=*), intent(in) :: var
            character(len=:), allocatable :: value
            integer(int64) :: pos
            type(status_type) :: s

            value = ""
            call re_search_forward(var//" = \""\(.+?\)\""", pos, s, noerror=SEARCH_NOERROR)
            if (s%ok() .and. pos > 0) value = match_string_no_properties(1)
        end function config_var

        ! The version after "(but really PROGRAM", "" if absent.
        function really(program) result(value)
            character(len=*), intent(in) :: program
            character(len=:), allocatable :: value
            integer(int64) :: pos
            type(status_type) :: s

            value = ""
            call goto_char(point_min())
            call re_search_forward("(but really "//program//" \([0-9]+\.[0-9.]+\([-._+ ]?[a-zA-Z0-9]+\)?\)?)", &
                                   pos, s, noerror=SEARCH_NOERROR)
            if (s%ok() .and. pos > 0) value = match_string_no_properties(1)
        end function really
    end subroutine ispell_check_version

    ! The decimal text of N.  Not a port.
    function integer_text(n) result(text)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, "(i0)") n
        text = trim(buffer)
    end function integer_text

    ! The output of PROGRAM run with ARGS, "" if it fails.  Not a port.
    function program_output(args) result(text)
        type(value_type), intent(in) :: args
        character(len=:), allocatable :: text
        type(handle_type) :: old, temp
        type(value_type) :: code
        type(status_type) :: st

        call temp_begin(old, temp)
        call ispell_execute_process(ispell_program_name, args, code, st)
        text = buffer_string()
        call temp_end(old, temp)
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

    !> Find Aspell's dictionaries, and record in `ispell_aspell_dictionaries'.
    !> Port of ispell.el:ispell-find-aspell-dictionaries.
    subroutine ispell_find_aspell_dictionaries()
        character(len=:), allocatable :: text, word
        type(ispell_dictionary_type), allocatable :: found(:)
        type(ispell_dictionary_type) :: dict
        logical :: ok
        integer :: k

        allocate (found(0))
        text = program_output(list(value_type(text="dicts")))

        do
            word = first_word(text)
            if (len(word) == 0) exit
            text = text(index(text, word) + len(word):)
            ! Search for the named dictionaries.
            call ispell_aspell_find_dictionary(word, dict, ok)
            if (ok) found = [found, dict]
        end do

        ! Ensure aspell's alias dictionary will override standard
        ! definitions.
        call ispell_aspell_add_aliases(found)
        ispell_aspell_found_dictionaries = found

        ! Merge into FOUND any elements from the standard
        ! `ispell_base_dictionaries' which have no element in FOUND at all.
        do k = 1, size(ispell_base_dictionaries)
            if (find_dictionary(found, ispell_base_dictionaries(k)%name) == 0) then
                found = [found, ispell_base_dictionaries(k)]
            end if
        end do

        ! Add a default entry.
        dict = ispell_dictionary_type(name="", casechars="[[:alpha:]]", not_casechars="[^[:alpha:]]", &
                                      otherchars="[']", many_otherchars=.false., &
                                      args=[character(len=64) :: "-B", "", "", ""], coding_system="utf-8")
        ispell_aspell_dictionaries = [dict, found]
    end subroutine ispell_find_aspell_dictionaries

    !> The parameters DICT for an Aspell dictionary DICT_NAME (in the form
    !> of `ispell_base_dictionaries' elements).  OK is false if no
    !> associated data file is found for the dictionary.  Port of
    !> ispell.el:ispell-aspell-find-dictionary.
    subroutine ispell_aspell_find_dictionary(dict_name, dict, ok)
        character(len=*), intent(in) :: dict_name
        type(ispell_dictionary_type), intent(out) :: dict
        logical, intent(out) :: ok
        character(len=:), allocatable :: data_file, path, specials, word, next, name
        character(len=64), allocatable :: otherchars(:)
        character(len=*), parameter :: regexps(3) = [character(len=32) :: "^[[:alpha:]]+", "^[[:alpha:]_]+", &
                                                                                                       "^[[:alpha:]]+-\(alt\|old\)"]
        type(handle_type) :: old, temp
        integer(int64) :: pos, inserted
        type(status_type) :: st
        integer :: d, r

        ok = .false.
        ! Make sure `ispell_aspell_dict_dir' is defined.
        if (len(ispell_aspell_dict_dir) == 0) ispell_aspell_dict_dir = first_word(program_output( &
                                                                                  list(value_type(text="config"), &
                                                                                       value_type(text="dict-dir"))))
        ! Make sure `ispell_aspell_data_dir' is defined.
        if (len(ispell_aspell_data_dir) == 0) ispell_aspell_data_dir = first_word(program_output( &
                                                                                  list(value_type(text="config"), &
                                                                                       value_type(text="data-dir"))))
        ! Try finding associated datafile.  aspell will look for master
        ! .dat file in `dict-dir' and `data-dir'.  Associated .dat files
        ! must be in the same directory as master file.
        data_file = ""

        search: do d = 1, 2
            path = ispell_aspell_dict_dir
            if (d == 2) path = ispell_aspell_data_dir

            ! Try xx.dat first, strip out variant, country code, etc, then
            ! try xx_YY.dat (without stripping country code), then try
            ! xx-alt.dat, for de-alt etc.
            do r = 1, 3
                name = ""
                if (string_match(trim(regexps(r)), dict_name, st) > 0) name = match_string_no_properties(0, dict_name)

                if (is_file_readable(path//"/"//name//".dat")) then
                    data_file = path//"/"//name//".dat"
                    exit search
                end if
            end do
        end do search

        if (len(data_file) == 0) return
        allocate (otherchars(0))
        call temp_begin(old, temp)
        call insert_file_contents(data_file, .false., inserted, st)
        call goto_char(point_min())
        ! There is zero or one line with special characters declarations.
        call re_search_forward("^special", pos, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. pos > 0) then
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

        call temp_end(old, temp)
        dict%name = dict_name
        dict%casechars = "[[:alpha:]]"
        dict%not_casechars = "[^[:alpha:]]"
        dict%otherchars = regexp_opt(otherchars)
        ! We can't tell, so set this to true.
        dict%many_otherchars = .true.
        dict%args(1) = "-d"
        dict%args(2) = dict_name
        ! Here we specify the encoding to use while communicating with
        ! aspell.
        dict%coding_system = "utf-8"
        ok = .true.
    end subroutine ispell_aspell_find_dictionary

    !> Find Aspell's dictionary aliases and add them to the dictionaries
    !> DICTS.  Port of ispell.el:ispell-aspell-add-aliases.
    subroutine ispell_aspell_add_aliases(dicts)
        type(ispell_dictionary_type), allocatable, intent(inout) :: dicts(:)
        type(value_type) :: files
        type(ispell_dictionary_type) :: alias
        type(value_type), allocatable :: elems(:)
        type(handle_type) :: old, temp
        character(len=:), allocatable :: file, aliasname, realname
        integer(int64) :: pos, inserted, k, n
        type(status_type) :: st
        integer :: j, real_index

        if (len(ispell_aspell_dict_dir) == 0) ispell_aspell_dict_dir = first_word(program_output( &
                                                                                  list(value_type(text="config"), &
                                                                                       value_type(text="dict-dir"))))
        ! (file-expand-wildcards "DICT-DIR/*.alias")
        call directory_files(ispell_aspell_dict_dir, files, st, full=.true.)
        n = 0
        allocate (elems(0))
        if (st%ok() .and. len(ispell_aspell_dict_dir) > 0) call list_elements(files, elems)
        n = size(elems)

        do k = 1, n
            file = xstring(elems(k))
            if (len(file) < 6) cycle
            if (file(len(file) - 5:) /= ".alias") cycle
            aliasname = file_name_nondirectory(file)
            if (aliasname(1:1) == ".") cycle
            aliasname = aliasname(1:len(aliasname) - 6)
            call temp_begin(old, temp)
            call insert_file_contents(file, .false., inserted, st)
            call goto_char(point_min())
            ! Look for a line "add FOO.multi", extract FOO.
            call re_search_forward("^add \([^.]+\)\.multi", pos, st, noerror=SEARCH_NOERROR)
            realname = ""
            if (st%ok() .and. pos > 0) realname = match_string_no_properties(1)
            call temp_end(old, temp)
            if (len(realname) == 0) cycle
            real_index = find_dictionary(dicts, realname)

            if (real_index > 0 .and. find_dictionary(dicts, aliasname) == 0) then
                alias = dicts(real_index)
                alias%name = aliasname
                dicts = [alias, dicts]
            end if
        end do

        ! Add entries for standard dict-names with found locale-matching
        ! entry.
        do j = 1, size(ispell_dicts_name2locale_equivs)
            associate (e => ispell_dicts_name2locale_equivs(j))
                ! Skip if already present.
                if (find_dictionary(dicts, trim(e%name)) > 0) cycle
                real_index = find_dictionary(dicts, trim(e%locale))

                if (real_index > 0) then
                    alias = dicts(real_index)
                    alias%name = e%name
                    dicts = [alias, dicts]
                end if
            end associate
        end do
    end subroutine ispell_aspell_add_aliases

    ! The index of the dictionary NAME in DICTS, 0 if absent.
    ! Not a port.
    integer function find_dictionary(dicts, name) result(k)
        type(ispell_dictionary_type), intent(in) :: dicts(:)
        character(len=*), intent(in) :: name

        do k = 1, size(dicts)
            if (dicts(k)%name == name) return
        end do

        k = 0
    end function find_dictionary

    !> Initialize some spellchecker parameters when changed or first used.
    !> Port of ispell.el:ispell-set-spellchecker-params (no
    !> `ispell-base-dicts-override-alist'; Hunspell's and Enchant's
    !> dictionaries are not discovered).
    recursive subroutine ispell_set_spellchecker_params(status)
        type(status_type), intent(out) :: status
        type(ispell_dictionary_type), allocatable :: found(:), base(:), all(:)
        type(ispell_dictionary_type) :: dict
        character(len=:), allocatable :: libdir
        type(status_type) :: st
        integer :: k, a, equiv
        logical :: checked

        status = status_type()

        if (allocated(ispell_last_program_name)) then
            if (ispell_last_program_name == ispell_program_name) return
        end if

        call ispell_kill_ispell(st, no_error=.true.)
        call ispell_check_version(libdir, st)
        checked = st%ok()
        if (checked) ispell_library_directory = libdir

        ! Auto-detection will only be used if spellchecker is not ispell
        ! and supports a way to set communication to UTF-8.
        if (checked .and. (len(ispell_encoding8_command) > 0 .or. len(ispell_really_enchant) > 0)) then
            if (len(ispell_really_aspell) > 0) then
                if (size(ispell_aspell_dictionaries) == 0) call ispell_find_aspell_dictionaries()
            end if
        end if

        ! Substitute `ispell_dictionaries' with the list of dictionaries
        ! corresponding to the given spellchecker.  With programs that
        ! support it, use the list of really installed dictionaries and add
        ! to it elements of the original list that are not present there.
        allocate (found(0))
        if (len(ispell_encoding8_command) > 0 .and. len(ispell_really_aspell) > 0) found = ispell_aspell_dictionaries
        base = ispell_base_dictionaries

        ! While ispell and aspell (through aliases) use the traditional
        ! dict naming originally expected by ispell.el, hunspell & Enchant
        ! use locale-based names with no alias.  We need to map standard
        ! names to locale based names to make default dict definitions
        ! available to these programs.
        if (len(ispell_really_hunspell) > 0 .or. len(ispell_really_enchant) > 0) then
            allocate (all(0))

            do k = 1, size(base)
                dict = base(k)
                ! Remove "-d" option from `args' if present.
                a = findloc(dict%args, "-d", dim=1)
                if (a > 0) dict%args(a:) = eoshift(dict%args(a:), 2)

                ! Unless default dict, re-add "-d" option with the mapped
                ! value.
                if (len_trim(dict%name) > 0) then
                    equiv = findloc(ispell_dicts_name2locale_equivs%name, dict%name(1:16), dim=1)

                    if (equiv == 0) then
                        call message("ispell-set-spellchecker-params: Missing equivalent for """//trim(dict%name)// &
                                     """. Skipping.")
                        cycle
                    end if

                    a = findloc(dict%args, "", dim=1)

                    if (a > 0 .and. a < size(dict%args)) then
                        dict%args(a) = "-d"
                        dict%args(a + 1) = ispell_dicts_name2locale_equivs(equiv)%locale
                    end if
                end if

                if (.not. has_dictionary(all, dict)) all = [dict, all]
            end do

            base = all
        end if

        call ispell_initialize_spellchecker_hook%execute(st)
        ! Add dicts to `ispell_dictionaries' unless already present.
        allocate (all(0))
        if (allocated(all)) deallocate (all)
        allocate (all(0))
        call add_new(found)
        call add_new(base)
        ispell_dictionaries = all
        ! If spellchecker supports UTF-8 via command-line option, use it in
        ! communication.  This does not affect definitions in your init
        ! file.
        deallocate (all)
        allocate (all(0))

        do k = 1, size(ispell_dictionaries)
            dict = ispell_dictionaries(k)
            if (len(ispell_encoding8_command) > 0 .or. len(ispell_really_enchant) > 0) dict%coding_system = "utf-8"
            if (.not. has_dictionary(all, dict)) all = [dict, all]
        end do

        ispell_dictionaries = all
        ispell_last_program_name = ispell_program_name

    contains

        ! Push the dictionaries of LIST not yet in ALL (by name).
        subroutine add_new(dicts)
            type(ispell_dictionary_type), intent(in) :: dicts(:)
            integer :: i

            do i = 1, size(dicts)
                if (find_dictionary(all, dicts(i)%name) == 0) all = [dicts(i), all]
            end do
        end subroutine add_new
    end subroutine ispell_set_spellchecker_params

    ! Whether DICTS has a dictionary equal to DICT in every component.
    ! Not a port.
    logical function has_dictionary(dicts, dict)
        type(ispell_dictionary_type), intent(in) :: dicts(:), dict
        integer :: k

        has_dictionary = .false.

        do k = 1, size(dicts)
            if (dicts(k)%name /= dict%name .or. dicts(k)%casechars /= dict%casechars) cycle
            if (dicts(k)%not_casechars /= dict%not_casechars .or. dicts(k)%otherchars /= dict%otherchars) cycle
            if (dicts(k)%many_otherchars .neqv. dict%many_otherchars) cycle
            if (any(dicts(k)%args /= dict%args)) cycle
            if (dicts(k)%extended_character_mode /= dict%extended_character_mode) cycle
            if (dicts(k)%coding_system /= dict%coding_system) cycle
            has_dictionary = .true.
            return
        end do
    end function has_dictionary

    !> The names of the valid dictionaries, "default" first.
    !> The variable `ispell_library_directory' defines their location.
    !> Port of ispell.el:ispell-valid-dictionary-list.
    recursive subroutine ispell_valid_dictionary_list(names, status)
        character(len=64), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        type(ispell_dictionary_type), allocatable :: dicts(:)
        character(len=:), allocatable :: name, explicit, bname
        integer :: k, a

        call ispell_set_spellchecker_params(status)
        dicts = [ispell_local_dictionaries, ispell_dictionaries]
        names = [character(len=64) :: "default"]

        do k = 1, size(dicts)
            name = trim(dicts(k)%name)
            if (len(name) == 0) cycle
            ! Explicitly (via args) specified dictionary.
            explicit = ""
            a = findloc(dicts(k)%args, "-d", dim=1)

            if (a > 0 .and. a < size(dicts(k)%args)) then
                explicit = trim(dicts(k)%args(a + 1))
            end if

            bname = name
            if (len(explicit) > 0) bname = explicit
            if (is_valid(name, explicit, bname)) names = [character(len=64) :: name, names]
        end do

    contains

        ! Whether the dictionary NAME is installed.
        logical function is_valid(dict_name, dict_explicit, dict_bname)
            character(len=*), intent(in) :: dict_name, dict_explicit, dict_bname

            ! Include all for Aspell (we already know existing dicts).
            if (len(ispell_really_aspell) > 0) then
                is_valid = find_dictionary(ispell_aspell_found_dictionaries, dict_name) > 0
                if (is_valid) return
            end if

            ! Include all if `ispell_library_directory' is "" (Hunspell).
            is_valid = len(ispell_really_aspell) == 0 .and. len(ispell_library_directory) == 0
            if (is_valid) return

            ! If explicit (-d with an absolute path) and existing dict.
            if (len(dict_explicit) > 0) then
                if (is_file_name_absolute(dict_explicit)) then
                    is_valid = is_located(dict_explicit, "")
                    if (is_valid) return
                end if
            end if

            ! If dict located in `ispell_library_directory'.
            is_valid = is_located(dict_bname, ispell_library_directory)
        end function is_valid

        ! (locate-file (file-name-nondirectory DICT) (DIR) suffixes).
        logical function is_located(dict, dir)
            character(len=*), intent(in) :: dict, dir
            character(len=:), allocatable :: directory, base

            directory = dir

            if (len(directory) == 0) then
                directory = dict(1:index(dict, "/", back=.true.))
            end if

            if (len(directory) == 0) then
                is_located = .false.
                return
            end if

            if (directory(len(directory):) /= "/") directory = directory//"/"
            base = file_name_nondirectory(dict)

            if (index(base, ".") > 1) then
                is_located = is_file_readable(directory//base)
            else
                is_located = is_file_readable(directory//base//".hash") .or. is_file_readable(directory//base//".has")
            end if
        end function is_located
    end subroutine ispell_valid_dictionary_list

    ! ------------------------------------------------------------------
    ! The current dictionary
    ! ------------------------------------------------------------------

    ! The record of the current dictionary in DICT; FOUND is false if it is
    ! in neither list.  Not a port.
    subroutine current_dictionary(dict, found)
        type(ispell_dictionary_type), intent(out) :: dict
        logical, intent(out) :: found
        character(len=:), allocatable :: name
        integer :: k

        name = ispell_current_dictionary
        found = .true.
        k = 0
        if (allocated(ispell_local_dictionaries)) k = find_dictionary(ispell_local_dictionaries, name)

        if (k > 0) then
            dict = ispell_local_dictionaries(k)
            return
        end if

        k = 0
        if (allocated(ispell_dictionaries)) k = find_dictionary(ispell_dictionaries, name)

        if (k > 0) then
            dict = ispell_dictionaries(k)
            return
        end if

        found = .false.
    end subroutine current_dictionary

    !> Get the decoded string in slot N (1 CASECHARS, 2 NOT-CASECHARS, 3
    !> OTHERCHARS) of the descriptor of the current dict.  Port of
    !> ispell.el:ispell-get-decoded-string.
    subroutine ispell_get_decoded_string(n, text, status)
        integer, intent(in) :: n
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(ispell_dictionary_type) :: dict
        logical :: found

        text = ""
        call current_dictionary(dict, found)

        if (.not. found) then
            call set_error(status, FM_GENERIC_ERROR, "No data for dictionary """//ispell_current_dictionary// &
                           """ in `ispell_local_dictionaries' or `ispell_dictionaries'")
            return
        end if

        select case (n)
        case (1)
            call decode_coding_string(trim(dict%casechars), trim(dict%coding_system), text, status)
        case (2)
            call decode_coding_string(trim(dict%not_casechars), trim(dict%coding_system), text, status)
        case default
            call decode_coding_string(trim(dict%otherchars), trim(dict%coding_system), text, status)
        end select
    end subroutine ispell_get_decoded_string

    !> Return CASECHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-casechars.
    function ispell_get_casechars() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call ispell_get_decoded_string(1, text, st)
    end function ispell_get_casechars

    !> Return NOT-CASECHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-not-casechars.
    function ispell_get_not_casechars() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call ispell_get_decoded_string(2, text, st)
    end function ispell_get_not_casechars

    !> Return OTHERCHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-otherchars.
    function ispell_get_otherchars() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: st

        call ispell_get_decoded_string(3, text, st)
    end function ispell_get_otherchars

    !> Return MANY-OTHERCHARS of a dictionary.  Port of
    !> ispell.el:ispell-get-many-otherchars-p.
    logical function ispell_get_many_otherchars()
        type(ispell_dictionary_type) :: dict
        logical :: found

        call current_dictionary(dict, found)
        ispell_get_many_otherchars = found .and. dict%many_otherchars
    end function ispell_get_many_otherchars

    !> Return ARGS of a dictionary.  Port of ispell.el:ispell-get-ispell-args.
    subroutine ispell_get_ispell_args(args)
        character(len=64), intent(out) :: args(4)
        type(ispell_dictionary_type) :: dict
        logical :: found

        args = ""
        call current_dictionary(dict, found)
        if (found) args = dict%args
    end subroutine ispell_get_ispell_args

    !> Return extended character mode of a dictionary.  Port of
    !> ispell.el:ispell-get-extended-character-mode.
    function ispell_get_extended_character_mode() result(mode)
        character(len=:), allocatable :: mode
        type(ispell_dictionary_type) :: dict
        logical :: found

        mode = ""
        ! Hunspell treats ~word as ordinary words in pipe mode.  Disable
        ! extended-char-mode.
        if (len(ispell_really_hunspell) > 0) return
        call current_dictionary(dict, found)
        if (found) mode = trim(dict%extended_character_mode)
    end function ispell_get_extended_character_mode

    !> Return encoding of a dictionary.  Port of
    !> ispell.el:ispell-get-coding-system.
    function ispell_get_coding_system() result(coding)
        character(len=:), allocatable :: coding
        type(ispell_dictionary_type) :: dict
        logical :: found

        coding = ""
        call current_dictionary(dict, found)
        if (found) coding = trim(dict%coding_system)
    end function ispell_get_coding_system

    ! ------------------------------------------------------------------
    ! Talking to the process
    ! ------------------------------------------------------------------

    !> Wait for output from Ispell process, or for SECONDS.  Port of
    !> ispell.el:ispell-accept-output.
    recursive subroutine ispell_accept_output(status, seconds)
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        logical :: got

        if (.not. is_process_live(ispell_process)) then
            call set_error(status, FM_GENERIC_ERROR, "No Ispell process to read output from!")
            return
        end if

        call accept_process_output(got, status, ispell_process, seconds)
    end subroutine ispell_accept_output

    !> Notify spell checker that MISSPELLED should be spelled REPLACEMENT.
    !> This allows improving the suggestion list based on actual
    !> misspellings.  Only works for Aspell and Enchant.  Port of
    !> ispell.el:ispell-send-replacement.
    subroutine ispell_send_replacement(misspelled, replacement)
        character(len=*), intent(in) :: misspelled, replacement
        type(status_type) :: st

        if (len(ispell_really_aspell) > 0 .or. len(ispell_really_enchant) > 0) then
            call ispell_send_string("$$ra "//misspelled//","//replacement//LF, st)
        end if
    end subroutine ispell_send_replacement

    !> Save MISSPELLED -> REPLACEMENT as an abbrev, if enabled.
    !> This is controlled by `ispell_save_corrections_as_abbrevs' and C-u in
    !> the command loop.  Port of
    !> ispell.el:ispell--maybe-save-correction-abbrev.
    subroutine ispell_maybe_save_correction_abbrev(misspelled, replacement)
        character(len=*), intent(in) :: misspelled, replacement

        if (ispell_save_correction_as_abbrev) then
            call global_abbrev_table%define(misspelled, replacement)
            call message(""""//misspelled//""" now expands to """//replacement//""" globally")
        end if
    end subroutine ispell_maybe_save_correction_abbrev

    !> Send the string STRING to the Ispell process.  Port of
    !> ispell.el:ispell-send-string (asynchronous processes).
    subroutine ispell_send_string(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_process_live(ispell_process)) call process_send_string(string, status, ispell_process)
    end subroutine ispell_send_string

    !> Filter function for ispell, grep, and look, process OUTPUT from
    !> subprocess.  OUTPUT can contain multiple lines, part of a single line,
    !> or both.  OUTPUT must end with a newline.  Port of
    !> ispell.el:ispell-filter.
    subroutine ispell_filter(process, output, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: output
        type(status_type), intent(out) :: status
        integer :: start, end, n
        logical :: more

        status = status_type()
        if (process%index < 0) return
        if (.not. allocated(ispell_filter_variable)) allocate (ispell_filter_variable(0))
        start = 1
        more = .true.

        do while (more)
            ! Get text up to the newline.
            end = index(output(start:), LF)
            if (end > 0) end = start + end - 1
            n = size(ispell_filter_variable)

            ! If we get out of sync and `ispell_filter_continue' is
            ! asserted when we are not continuing, treat the next item as a
            ! separate list.
            if (ispell_filter_continue .and. n > 0) then
                ! Continue with same line (item): add it to the prev item.
                if (end > 0) then
                    ispell_filter_variable(n)%text = ispell_filter_variable(n)%text//output(start:end - 1)
                else
                    ispell_filter_variable(n)%text = ispell_filter_variable(n)%text//output(start:)
                end if
            else
                ! This is a new line and item.
                if (end > 0) then
                    call append_string(ispell_filter_variable, output(start:end - 1))
                else
                    call append_string(ispell_filter_variable, output(start:))
                end if
            end if

            if (end == 0) then
                ! We've completed reading the output, but didn't finish the
                ! line.
                ispell_filter_continue = .true.
                more = .false.
            else
                ! Skip over newline, this line complete.
                ispell_filter_continue = .false.
                end = end + 1

                if (end > len(output)) then
                    ! No more lines in output, so we can exit the filter.
                    more = .false.
                else
                    start = end
                end if
            end if
        end do
    end subroutine ispell_filter

    ! Forget the output lines and the incomplete line flag.  Not a port.
    subroutine clear_filter()
        if (allocated(ispell_filter_variable)) deallocate (ispell_filter_variable)
        allocate (ispell_filter_variable(0))
        ispell_filter_continue = .false.
    end subroutine clear_filter

    ! Whether the newest output line is the empty line that ends an answer
    ! (string= "" (car ispell-filter)).  Not a port.
    logical function is_answer_complete()
        integer :: n

        is_answer_complete = .false.
        if (.not. allocated(ispell_filter_variable)) return
        n = size(ispell_filter_variable)
        if (n == 0) return
        is_answer_complete = len(ispell_filter_variable(n)%text) == 0
    end function is_answer_complete

    !> Parse the OUTPUT string from Ispell process into RESULT (see
    !> `ispell_output_type').
    !>
    !> Optional second arg ACCEPTED is the words already accepted.
    !> Optional third arg SHIFT is an offset to apply based on previous
    !> corrections.  Port of ispell.el:ispell-parse-output.
    subroutine ispell_parse_output(output, result, accepted, shift)
        character(len=*), intent(in) :: output
        type(ispell_output_type), intent(out) :: result
        type(string_type), intent(in), optional :: accepted(:)
        integer(int64), intent(in), optional :: shift
        character(len=:), allocatable :: rest, word
        integer(int64) :: count, current_count
        character :: kind
        type(status_type) :: st
        integer :: k, e

        if (output == "" .or. output == "*" .or. output == "-") then
            ! For startup with pipes, an exact match, a compound word match.
            result%kind = ISPELL_OUTPUT_CORRECT
            return
        end if

        if (output(1:1) == "+") then
            ! Found because of root word.
            result%kind = ISPELL_OUTPUT_ROOT
            result%word = output(min(3, len(output) + 1):)
            return
        end if

        if (string_match("[\r"//achar(13)//"a-zA-Z]", output, st) == 1) then
            ! Error message from ispell!
            call ding()
            call message("Ispell error: "//output)
            call sit_for(5000)
            result%kind = ISPELL_OUTPUT_ERROR
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
        result%kind = ISPELL_OUTPUT_MISSPELLED

        if (present(accepted)) then
            do k = 1, size(accepted)
                if (accepted(k)%text == result%word) then
                    result%kind = ISPELL_OUTPUT_CORRECT
                    return
                end if
            end do
        end if

        if (present(shift)) result%offset = result%offset + shift
    end subroutine ispell_parse_output

    !> Return the status of the Ispell process ("" without one).  Port of
    !> ispell.el:ispell-process-status.
    function ispell_process_status() result(status_name)
        character(len=:), allocatable :: status_name

        status_name = ""
        if (ispell_process%index /= 0) status_name = process_status(ispell_process)
    end function ispell_process_status

    !> Start the Ispell process.  Port of ispell.el:ispell-start-process
    !> (asynchronous processes).
    subroutine ispell_start_process(process, status)
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status
        character(len=64) :: orig(4)
        type(string_type), allocatable :: command(:)
        character(len=:), allocatable :: saved, dir, coding
        integer :: k, connection

        process = null_handle()
        call save_directory(saved)
        dir = "./"
        if (allocated(saved)) dir = saved
        ! Defend against bad `default_directory'.
        if (.not. is_file_accessible_directory(dir)) call expand_file_name("~/", dir, status)
        call ispell_get_ispell_args(orig)
        ! Accept single input lines.
        call append_string(command, ispell_program_name, "-a")

        ! Make root/affix combos not in dict.  hunspell -m option means
        ! different.
        if (len(ispell_really_hunspell) > 0) then
            call append_string(command, "")
        else
            call append_string(command, "-m")
        end if

        ! Not for default dict; only define if not overridden.
        if (len(ispell_current_dictionary) > 0 .and. .not. any(orig == "-d")) then
            call append_string(command, "-d", ispell_current_dictionary)
        end if

        do k = 1, size(orig)
            if (len_trim(orig(k)) > 0) call append_string(command, trim(orig(k)))
        end do

        ! Use specified pers dict.
        if (len(ispell_current_personal_dictionary) > 0) then
            call append_string(command, "-p", ispell_current_personal_dictionary)
        end if

        ! If we are using recent aspell or hunspell, make sure we use the
        ! right encoding for communication.  ispell or older aspell/hunspell
        ! does not support this.
        if (len(ispell_encoding8_command) > 0) then
            coding = ispell_get_coding_system()

            if (len(ispell_really_hunspell) > 0) then
                call append_string(command, ispell_encoding8_command, upcase_string(coding))
            else
                call append_string(command, ispell_encoding8_command//coding)
            end if
        end if

        if (allocated(ispell_extra_args)) then
            do k = 1, size(ispell_extra_args)
                call append_string(command, trim(ispell_extra_args(k)))
            end do
        end if

        ! Initially we don't know any buffer's local words.
        ispell_buffer_local_name = ""
        connection = PROCESS_CONNECTION_PIPE
        if (ispell_use_ptys) connection = PROCESS_CONNECTION_PTY
        buffers(current)%directory = dir
        call make_process("ispell", command, process, status, connection_type=connection)
        call restore_directory(saved)
    end subroutine ispell_start_process

    !> Check status of Ispell process and start if necessary.  Port of
    !> ispell.el:ispell-init-process.
    recursive subroutine ispell_init_process(status)
        type(status_type), intent(out) :: status
        character(len=64) :: args(4)
        character(len=:), allocatable :: dict_bname, dir, home, libdir, coding, mode, text
        type(status_type) :: st
        integer :: a, k

        call ispell_get_ispell_args(args)
        ! Basename of dictionary used by the spell-checker.
        dict_bname = ispell_current_dictionary
        a = findloc(args, "-d", dim=1)
        if (a > 0 .and. a < size(args)) dict_bname = trim(args(a + 1))
        if (len(dict_bname) == 0) dict_bname = "default"
        call expand_file_name("~/", home, st)
        dir = "./"
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory

        ! Use "~/" as default directory unless using Ispell with per-dir
        ! personal dictionaries.
        if (len(ispell_really_aspell) > 0 .or. len(ispell_really_hunspell) > 0 .or. &
            .not. is_file_accessible_directory(dir)) then
            dir = home
        else if (.not. (is_file_readable(dir//".ispell_words") .or. is_file_readable(dir//".ispell_"//dict_bname))) then
            dir = home
        else
            call expand_file_name(dir, text, st)
            dir = text
        end if

        ! Check if process needs restart.
        if (ispell_process%index /= 0 .and. ispell_process_status() == "run" .and. &
            (len(string_value(ispell_local_pdict)) > 0 .or. len(ispell_personal_dictionary) > 0 .or. &
             ispell_process_directory == dir)) then
            call clear_filter()
            status = status_type()
            return
        end if

        ! May need to restart to select new personal dictionary.
        call ispell_kill_ispell(st, no_error=.true.)
        text = "default"

        if (len(string_value(ispell_local_dictionary)) > 0) then
            text = string_value(ispell_local_dictionary)
        else if (len(ispell_dictionary) > 0) then
            text = ispell_dictionary
        end if

        call message("Starting new Ispell process "//ispell_program_name//" with "//text//" dictionary...")
        call sit_for(0)
        call ispell_check_version(libdir, status)
        if (.not. status%ok()) return
        ispell_library_directory = libdir
        ispell_process_directory = dir
        call ispell_start_process(ispell_process, status)
        if (.not. status%ok()) return
        call clear_filter()
        call message("Starting new Ispell process "//ispell_program_name//" with "//text//" dictionary...done")

        if (ispell_process_directory /= home) then
            ! If not, kill ispell process when killing buffer.  It may be in
            ! a removable device that would otherwise become un-mountable.
            call prepend_kill_ispell_hook()
        end if

        call set_process_filter(ispell_process, st, ispell_filter)
        coding = ispell_get_coding_system()
        if (len(coding) > 0) call set_process_coding_system(ispell_process, coding, coding, st)
        ! Get version ID line.
        call ispell_accept_output(st, 3.0)
        ! Get more output if filter empty?
        if (size(ispell_filter_variable) == 0) call ispell_accept_output(st, 3.0)

        if (size(ispell_filter_variable) == 0) then
            call set_error(status, FM_GENERIC_ERROR, ispell_program_name//" did not output version line")
            return
        end if

        if (.not. has_version_line()) then
            ! Otherwise, it must be an error message.  Show the user.  But
            ! first wait to see if some more output is going to arrive.
            call sit_for(1000)
            if (is_process_live(ispell_process)) call ispell_accept_output(st, 3.0)
            text = ""

            do k = size(ispell_filter_variable), 1, -1
                text = text//ispell_filter_variable(k)%text
                if (k > 1) text = text//LF
            end do

            call set_error(status, FM_GENERIC_ERROR, text)
            return
        end if

        ! Discard version ID line.
        call clear_filter()
        mode = ispell_get_extended_character_mode()
        if (len(mode) > 0) call ispell_send_string(mode//LF, st)
        call set_process_query_on_exit_flag(ispell_process, .false., st)

    contains

        ! Whether the output is the version line as expected (we already
        ! know it's the right version, so don't bother checking again).
        logical function has_version_line()
            integer :: n
            type(status_type) :: s

            n = size(ispell_filter_variable)

            if (string_match("warning: ", ispell_filter_variable(n)%text, s) > 0) then
                ! Was warn msg.
                call ispell_accept_output(s, 3.0)
                n = size(ispell_filter_variable)
            else if (n > 1) then
                has_version_line = .false.
                return
            end if

            has_version_line = string_match("^@(#) ", ispell_filter_variable(n)%text, s) > 0
        end function has_version_line
    end subroutine ispell_init_process

    ! Kill the Ispell process when the current buffer is closed (the
    ! procedure ispell-init-process adds to the buffer's own
    ! `kill-buffer-hook').
    ! Not a port.
    subroutine prepend_kill_ispell_hook()
        use files_mod, only: close_buffer_hook

        call close_buffer_hook%prepend(kill_ispell_quietly, local=.true.)
    end subroutine prepend_kill_ispell_hook

    ! Kill the Ispell process, without an error if there is none.
    ! Not a port.
    recursive subroutine kill_ispell_quietly(status)
        type(status_type), intent(out) :: status

        call ispell_kill_ispell(status, no_error=.true.)
    end subroutine kill_ispell_quietly

    !> Kill current Ispell process (so that you may start a fresh one).
    !> With NO_ERROR, just return if there was no Ispell running.
    !> With CLEAR, buffer session localwords are cleaned.  Port of
    !> ispell.el:ispell-kill-ispell (CLEAR is also true when called
    !> interactively).
    recursive subroutine ispell_kill_ispell(status, no_error, clear)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error, clear
        type(status_type) :: st
        logical :: quiet

        status = status_type()
        ! This hook is typically used by flyspell to flush some variables
        ! used to optimize the common cases.
        call ispell_kill_ispell_hook%execute(st)

        if (present(clear)) then
            if (clear) call clear_session_localwords()
        end if

        quiet = .false.
        if (present(no_error)) quiet = no_error

        if (.not. (ispell_process%index /= 0 .and. ispell_process_status() == "run")) then
            if (.not. quiet) call set_error(status, FM_GENERIC_ERROR, "There is no Ispell process running!")
            return
        end if

        call delete_process(ispell_process, status)
        ispell_process = null_handle()
        call message("Ispell process killed")
    end subroutine ispell_kill_ispell

    ! The command `ispell_kill_ispell'.
    recursive subroutine cmd_ispell_kill_ispell(status)
        type(status_type), intent(out) :: status

        call ispell_kill_ispell(status, clear=.true.)
    end subroutine cmd_ispell_kill_ispell

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
    !> `ispell_buffer_session_localwords', the words accepted for the
    !> session in this buffer, unless it is there already (cl-pushnew).
    !> Port of ispell.el ispell-buffer-session-localwords.
    subroutine prepend_ispell_buffer_session_localword(word)
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
    end subroutine prepend_ispell_buffer_session_localword

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
    !> If optional argument FOLLOWING is true or if `ispell_following_word'
    !> is true when called interactively, then the following word (rather
    !> than preceding) is checked when the cursor is not over a word.
    !> When the optional argument QUIETLY is true or `ispell_quietly' is
    !> true when called interactively, non-corrective messages are
    !> suppressed.
    !>
    !> With a prefix argument (or if RESUME is true (upstream's CONTINUE)), resume interrupted
    !> spell-checking of a buffer or region.
    !>
    !> Interactively, in Transient Mark mode when the mark is active, call
    !> `ispell_region' to check the active region for spelling errors.
    !> Non-interactively, this happens if REGION is true.
    !>
    !> Word syntax is controlled by the definition of the chosen dictionary,
    !> which is in `ispell_local_dictionaries' or `ispell_dictionaries'.
    !>
    !> This will check or reload the dictionary.  Use M-x
    !> ispell_change_dictionary or M-x ispell_region to update the Ispell
    !> process.  Port of ispell.el:ispell-word (no return value).
    recursive subroutine ispell_word(status, following, quietly, resume, region)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: following, quietly, resume, region
        logical :: is_following, is_quiet
        character(len=:), allocatable :: word, new_word
        integer(int64) :: cursor_location, start, end, shift
        type(ispell_output_type) :: poss
        type(replacement_type) :: replace
        type(window_configuration_type) :: config
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
                call ispell_region(active_region_beginning(), active_region_end(), status, quit=quit)
                return
            end if
        end if

        if (present(resume)) then
            if (resume) then
                call ispell_continue(status)
                return
            end if
        end if

        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        ! Use the correct dictionary.
        call ispell_accept_buffer_local_defs(status)
        if (.not. status%ok()) return
        ! Retain cursor location.
        cursor_location = point()
        call ispell_get_word(is_following, word, start, end, status)
        if (.not. status%ok()) return
        ! At this point it used to ignore 2-letter words.  But that is
        ! silly; if the user asks for it, we should do it. - rms.
        if (.not. is_quiet) call message("Checking spelling of "//formatted_word(word)//"...")
        call ispell_run_on_word(word, poss, status)
        if (.not. status%ok()) return
        shift = 0
        replace = replacement_type()

        select case (poss%kind)
        case (ISPELL_OUTPUT_CORRECT)
            if (.not. is_quiet) call message(formatted_word(word)//" is correct")
        case (ISPELL_OUTPUT_ROOT)
            if (.not. is_quiet) call message(formatted_word(word)//" is correct because of root "// &
                                             formatted_word(poss%word))
        case (ISPELL_OUTPUT_ERROR)
            call message(error_checking_word(word))
        case default
            if (ispell_check_only) then
                ! Called from ispell minor mode.
                call ding()
                call message(formatted_word(word)//" is incorrect")
            else
                ! Prompt for correct word.
                ispell_abbrev_saving_allowed = .true.
                ispell_save_correction_as_abbrev = ispell_save_corrections_as_abbrevs
                call make_marker(start_marker)
                call set_marker(start_marker, start, st)
                call make_marker(end_marker)
                call set_marker(end_marker, end, st)
                call current_window_configuration(config)
                call ispell_command_loop(poss%misses, poss%guesses, poss%word, start, end, replace, status)
                call set_window_configuration(config)
                ispell_abbrev_saving_allowed = .false.
                start = marker_pos(start_marker)
                end = marker_pos(end_marker)

                if (.not. status%ok()) then
                    call destroy_marker(start_marker, st)
                    call destroy_marker(end_marker, st)
                    return
                end if

                if (replace%kind == REPLACE_LOCAL) then
                    call ispell_add_per_file_word_list(poss%word, status)
                else if (replace%kind /= REPLACE_KEEP) then
                    new_word = replace%word
                    cursor_location = cursor_location + (len_chars(word) - (end - start))
                    call ispell_maybe_save_correction_abbrev(poss%word, new_word)

                    if (new_word /= poss%word) then
                        call goto_char(start)
                        ! Insert first and then delete, to avoid collapsing
                        ! markers before and after into a single place.
                        call insert(new_word, st)
                        call delete_region(point(), marker_pos(end_marker), st)
                        ! It is meaningless to preserve the cursor position
                        ! inside a word that has changed.
                        cursor_location = point()
                        end = point()
                    end if

                    if (replace%kind /= REPLACE_WORD) then
                        ! Recheck spelling of replacement.
                        if (replace%kind == REPLACE_QUERY) then
                            call current_window_configuration(config)
                            call query_replace(word, new_word, st, delimited=.true.)
                            call set_window_configuration(config)
                        end if

                        call goto_char(start)
                        ! Single word could be split into multiple words.
                        call ispell_region(start, end, status, shift=shift, quit=quit)
                        ispell_quit = quit
                    end if
                end if

                ! Keep if rechecking word and we keep choices win.
                choices = get_buffer(ispell_choices_buffer)
                if (choices%index /= 0) call close_buffer(choices, st)
                call destroy_marker(start_marker, st)
                call destroy_marker(end_marker, st)
            end if
        end select

        call ispell_pdict_save(st, ispell_silently_save)

        ! NB: Cancels ispell-quit incorrectly if called from ispell-region.
        if (ispell_quit) then
            ispell_quit = .false.
            ispell_quit_position = 0
        end if

        ! Return to original location.
        call goto_char(cursor_location)
    end subroutine ispell_word

    ! The command `ispell_word' (M-$).
    recursive subroutine cmd_ispell_word(status)
        type(status_type), intent(out) :: status

        call ispell_word(status, ispell_following_word, ispell_quietly, current_prefix_arg%kind /= PREFIX_NONE, .true.)
    end subroutine cmd_ispell_word

    ! The number of characters of the UTF-8 TEXT.  Not a port.
    integer(int64) function len_chars(text) result(n)
        character(len=*), intent(in) :: text
        integer :: k

        n = 0

        do k = 1, len(text)
            if (iand(iachar(text(k:k)), 192) /= 128) n = n + 1
        end do
    end function len_chars

    !> Run ispell on WORD, the result in POSS.  Port of
    !> ispell.el:ispell--run-on-word.
    subroutine ispell_run_on_word(word, poss, status)
        character(len=*), intent(in) :: word
        type(ispell_output_type), intent(out) :: poss
        type(status_type), intent(out) :: status
        integer :: n

        ! Put the speller in verbose mode.
        call ispell_send_string("%"//LF, status)
        call ispell_send_string("^"//word//LF, status)

        ! Wait until ispell has processed word.
        do
            call ispell_accept_output(status)
            if (.not. status%ok()) return
            if (is_answer_complete()) exit
        end do

        n = size(ispell_filter_variable)
        ispell_filter_variable = ispell_filter_variable(1:n - 1)
        n = n - 1

        if (n > 1) then
            call set_error(status, FM_GENERIC_ERROR, "Ispell and its process have different character maps: "// &
                           filter_text())
            return
        end if

        if (n == 1) then
            call ispell_parse_output(ispell_filter_variable(1)%text, poss)
        else
            ! (ispell-filter empty: the word is not checked.)
            poss%kind = ISPELL_OUTPUT_ERROR
        end if

    contains

        ! The output lines, newest first, as upstream prints the list.
        function filter_text() result(text)
            character(len=:), allocatable :: text
            integer :: k

            text = "("

            do k = size(ispell_filter_variable), 1, -1
                text = text//""""//ispell_filter_variable(k)%text//""""
                if (k > 1) text = text//" "
            end do

            text = text//")"
        end function filter_text
    end subroutine ispell_run_on_word

    ! A string describing that checking for WORD failed.  Port of
    ! ispell.el:ispell-error-checking-word.
    function error_checking_word(word) result(text)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text
        character(len=:), allocatable :: dict

        dict = ispell_current_dictionary
        if (len(dict) == 0) dict = "default"
        text = "Error checking word "//formatted_word(word)//" using "//file_name_nondirectory(ispell_program_name)// &
               " with "//dict//" dictionary"
    end function error_checking_word

    !> Return true if the word at point is correct, false otherwise.
    !>
    !> If optional argument FOLLOWING is true then the following word
    !> (rather than preceding) is checked when the cursor is not over a
    !> word.  Port of ispell.el:ispell-correct-p.
    logical function is_ispell_correct(status, following) result(correct)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: following
        character(len=:), allocatable :: word
        integer(int64) :: start, end, saved
        type(ispell_output_type) :: poss
        logical :: is_following

        correct = .false.
        is_following = .false.
        if (present(following)) is_following = following
        saved = point()
        ! Reset the filter so it only contains the result of spell-checking
        ! the current word.
        call clear_filter()
        call ispell_get_word(is_following, word, start, end, status)

        if (status%ok()) then
            call ispell_run_on_word(word, poss, status)

            if (status%ok()) then
                if (poss%kind == ISPELL_OUTPUT_ERROR) then
                    call set_error(status, FM_GENERIC_ERROR, error_checking_word(word))
                else
                    correct = poss%kind == ISPELL_OUTPUT_CORRECT .or. poss%kind == ISPELL_OUTPUT_ROOT
                end if
            end if
        end if

        call goto_char(saved)
    end function is_ispell_correct

    !> The word for spell-checking according to ispell syntax, from START
    !> to END.  If optional argument FOLLOWING is true or if
    !> `ispell_following_word' is true when called interactively, then the
    !> following word (rather than preceding) is checked when the cursor is
    !> not over a word.  Optional EXTRA_OTHERCHARS contains otherchars that
    !> can be included in word many times (see `ispell_dictionary_type'
    !> about otherchars).
    !>
    !> Word syntax is controlled by the definition of the chosen dictionary,
    !> which is in `ispell_local_dictionaries' or `ispell_dictionaries'.
    !> EXTRA_OTHERCHARS, if present, is the set of extra characters to add
    !> to the dictionary's OTHERCHARS.  Port of ispell.el:ispell-get-word.
    recursive subroutine ispell_get_word(following, word, start, end, status, extra_otherchars)
        logical, intent(in) :: following
        character(len=:), allocatable, intent(out) :: word
        integer(int64), intent(out) :: start, end
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: extra_otherchars
        character(len=:), allocatable :: casechars, not_casechars, otherchars, extra, word_regexp
        integer(int64) :: found, prevpt
        logical :: many, did_it_once, has_extra
        type(status_type) :: st

        word = ""
        start = point()
        end = point()
        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        call ispell_get_decoded_string(1, casechars, status)
        if (.not. status%ok()) return
        call ispell_get_decoded_string(2, not_casechars, status)
        call ispell_get_decoded_string(3, otherchars, status)
        many = ispell_get_many_otherchars()
        extra = ""
        if (present(extra_otherchars)) extra = extra_otherchars
        has_extra = present(extra_otherchars)
        word_regexp = casechars//"+\("
        if (len(otherchars) > 0) word_regexp = word_regexp//otherchars//"?"
        if (has_extra) word_regexp = word_regexp//extra//"?"
        word_regexp = word_regexp//casechars//"+\)"

        if (many .or. has_extra) then
            word_regexp = word_regexp//"*"
        else
            word_regexp = word_regexp//"?"
        end if

        did_it_once = .false.
        prevpt = -1

        ! Find the word.
        if (.not. looking_at(casechars, st)) then
            if (following) then
                call re_search_forward(casechars, found, st, point_max(), SEARCH_NOERROR)
            else
                call re_search_backward(casechars, found, st, point_min(), SEARCH_NOERROR)
            end if
        end if

        ! Move to front of word.
        call re_search_backward(not_casechars, found, st, point_min(), SEARCH_MOVE)

        do
            if (.not. (is_looking_at_other() .and. .not. is_bob() .and. (.not. did_it_once .or. many) .and. &
                       prevpt /= point())) exit

            if (is_looking_at_extra()) then
                call backward_char(st)
                if (looking_at(casechars, st)) call re_search_backward(not_casechars, found, st, point_min(), SEARCH_MOVE)
            else
                did_it_once = .true.
                prevpt = point()
                call backward_char(st)

                if (looking_at(casechars, st)) then
                    call re_search_backward(not_casechars, found, st, point_min(), SEARCH_MOVE)
                else
                    call forward_char(st)
                end if
            end if
        end do

        ! Now mark the word and save to string.
        call re_search_forward(word_regexp, found, st, point_max(), SEARCH_NOERROR)

        if (.not. st%ok() .or. found == 0) then
            if (ispell_check_only) then
                ! Return dummy word when just flagging misspellings.
                start = point()
                end = point()
            else
                call set_error(status, FM_USER_ERROR, "No word found to check!")
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
    end subroutine ispell_get_word

    !> Check to see if the personal dictionary has been modified.
    !> If so, ask if it needs to be saved.
    !> If NO_QUERY is true, save the personal dictionary without asking.
    !> Interactively, if `ispell_silently_save' is true, don't ask.
    !> If FORCE_SAVE is true, suggest to save the personal dictionary even if
    !> not modified; this always happens interactively.  Port of
    !> ispell.el:ispell-pdict-save.
    recursive subroutine ispell_pdict_save(status, no_query, force_save)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_query, force_save
        logical :: save, answer

        status = status_type()
        save = ispell_pdict_modified
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
                call ispell_send_string("#"//LF, status)
                call message("Personal dictionary saved.")
            end if
        end if

        ! Unassert variable, even if not saved to avoid questioning.
        ispell_pdict_modified = .false.
    end subroutine ispell_pdict_save

    ! The command `ispell_pdict_save'.
    recursive subroutine cmd_ispell_pdict_save(status)
        type(status_type), intent(out) :: status

        call ispell_pdict_save(status, ispell_silently_save, .true.)
    end subroutine cmd_ispell_pdict_save

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

        c = iachar("0")
        skipped = 0
        width = window_body_width(selected_window(), st)

        do k = 1, size(miss)
            if (7 + current_column() + len_chars(miss(k)%text) + merge(3, 0, c > iachar("~")) > width) then
                call insert(LF, st)
            end if

            ! Not so good if there are over 20 or 30 options, but then, if
            ! there are that many you don't want to scan them all anyway...
            do while (any(command_characters == c))
                ! Skip command characters.
                c = c + 1
                skipped = skipped + 1
            end do

            call insert("("//char_text(c)//") "//miss(k)%text//"  ", st)
            c = c + 1
        end do

        count = c - iachar("0") - skipped
    end subroutine insert_choices

    ! The UTF-8 text of character C.  Not a port.
    function char_text(c) result(text)
        integer, intent(in) :: c
        character(len=:), allocatable :: text

        if (c < 128) then
            text = achar(c)
        else if (c < 2048) then
            text = achar(192 + c/64)//achar(128 + iand(c, 63))
        else
            text = achar(224 + c/4096)//achar(128 + iand(c/64, 63))//achar(128 + iand(c, 63))
        end if
    end function char_text

    !> Display possible corrections from MISS using GUESS for WORD in
    !> START..END.  MISS lists possible corrections.  GUESS lists possibly
    !> valid affix construction of WORD.  RESULT is REPLACE_KEEP to keep the
    !> word unchanged, REPLACE_LOCAL to insert locally into buffer-local
    !> dictionary, REPLACE_WORD for the chosen replacement word,
    !> REPLACE_RECHECK and REPLACE_RECHECK_HERE for a new replacement word
    !> (will be rechecked), REPLACE_QUERY when `query_replace' is to be
    !> used.  Highlight the word, which is assumed to run from START to
    !> END.  `ispell_quit' is set to start location to continue spell
    !> session.  Port of ispell.el:ispell-command-loop.
    recursive subroutine ispell_command_loop(miss, guess, word, start, end, result, status)
        type(string_type), intent(in) :: miss(:), guess(:)
        character(len=*), intent(in) :: word
        integer(int64), intent(in) :: start, end
        type(replacement_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer, allocatable :: command_characters(:)
        type(string_type), allocatable :: choices(:)
        integer :: count, num, skipped, k, width
        integer(int64) :: char
        type(handle_type) :: choices_buffer, textwin, text_buffer
        type(value_type) :: key
        character(len=:), allocatable :: answer, prompt
        logical :: abbrev_prefix, again, yes, highlighted
        type(status_type) :: st

        status = status_type()
        command_characters = [32, iachar("i"), iachar("a"), iachar("A"), iachar("r"), iachar("R"), iachar("?"), &
                              iachar("x"), iachar("X"), iachar("q"), iachar("l"), iachar("u"), iachar("m")]
        if (ispell_abbrev_saving_allowed) command_characters = [command_characters, 21]
        choices = miss
        abbrev_prefix = .false.
        text_buffer = current_buffer()
        ! Setup the *Choices* buffer with valid data.
        call get_or_make_buffer(ispell_choices_buffer, choices_buffer, st)
        call set_buffer(choices_buffer, st)
        call erase_buffer(st)
        width = window_body_width(selected_window(), st)

        if (size(guess) > 0) then
            call insert("Affix rules generate and capitalize this word as shown below:"//LF//TAB, st)

            do k = 1, size(guess)
                if (4 + current_column() + len_chars(guess(k)%text) > width) call insert(LF//TAB, st)
                call insert(guess(k)%text//"    ", st)
            end do

            call insert(substitute_command_keys(LF//"Use option `i' to accept this spelling and put it in your "// &
                                                "personal dictionary."//LF), st)
        end if

        call insert_choices(choices, command_characters, count)
        call set_buffer(text_buffer, st)
        call ispell_update_post_hook%execute(st)
        ! Ensure word is visible.
        call sit_for(0)
        ! Display choices for misspelled word.
        textwin = selected_window()
        call ispell_show_choices(status)
        if (.not. status%ok()) return
        call select_window(textwin, st)
        call set_buffer(text_buffer, st)
        ! Highlight word, protecting current buffer status.
        highlighted = .not. ispell_highlight%is_off()
        if (highlighted) call ispell_highlight_spelling_error(start, end, .true.)

        ! Loop until a valid choice is made.
        do
            again = .false.
            call undo_boundary()
            prompt = "C-h or ? for more options; SPC to leave unchanged, Character to replace word"

            if (ispell_abbrev_saving_allowed .and. abbrev_prefix) then
                if (ispell_save_correction_as_abbrev) then
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
            num = int(char) - iachar("0") - skipped

            if (abbrev_prefix .and. (char == iachar("r") .or. char == iachar("R") .or. (num >= 0 .and. num < count))) then
                ! If the user typed `C-u' before this replacement command,
                ! then toggle abbrev saving for this correction.
                ispell_save_correction_as_abbrev = .not. ispell_save_correction_as_abbrev
                abbrev_prefix = .false.
            else if (abbrev_prefix .and. char /= 21) then
                ! If the user typed `C-u' but not before a replacement
                ! command, then nullify the effect of `C-u' for subsequent
                ! commands.
                abbrev_prefix = .false.
            end if

            result = replacement_type()

            if (char == 32) then
                ! Accept word this time only.
                continue
            else if (char == iachar("i")) then
                ! Accept and insert word into pers dict.
                call ispell_send_string("*"//word//LF, st)
                ! Dictionary modified!
                ispell_pdict_modified = .true.
            else if (char == iachar("a") .or. char == iachar("A")) then
                ! Accept word without insert.
                call ispell_send_string("@"//word//LF, st)
                call prepend_ispell_buffer_session_localword(word)
                ! Session localwords might conflict.
                if (len(ispell_buffer_local_name) == 0) ispell_buffer_local_name = buffers(current)%name
                ! Return REPLACE_LOCAL for ispell-add buffer-local.
                if (char == iachar("A")) result%kind = REPLACE_LOCAL
            else if (char == iachar("r") .or. char == iachar("R")) then
                ! Type in replacement.
                if (ispell_highlight%choice == ISPELL_HIGHLIGHT_BLOCK) call refresh_highlight()

                if (char == iachar("R") .or. ispell_query_replace_choices) then
                    call read_string("Query-replacement for "//word//": ", answer, status, initial_input=word)
                    result%kind = REPLACE_QUERY
                else
                    call read_string("Replacement for: ", answer, status, initial_input=word)
                    result%kind = REPLACE_RECHECK
                end if

                if (.not. status%ok()) exit
                result%word = answer
            else if (char == iachar("?") .or. char == help_char .or. char == 8) then
                if (ispell_highlight%choice == ISPELL_HIGHLIGHT_BLOCK) call refresh_highlight()
                call ispell_help(status)
                if (.not. status%ok()) exit
                again = .true.
            else if (char == iachar("x")) then
                ! Quit and move point back.
                call ispell_pdict_save(st, ispell_silently_save)
                call message("Exited spell-checking")
                ispell_quit = .true.
                ispell_quit_position = 0
            else if (char == iachar("X")) then
                ! Quit and preserve point.
                call ispell_pdict_save(st, ispell_silently_save)
                call message(substitute_command_keys("Spell-checking suspended; use \[universal_argument] "// &
                                                     "\[ispell_word] to resume"))
                ispell_quit = .true.
                ispell_quit_position = start
            else if (char == iachar("q")) then
                call read_y_or_n("Really kill Ispell process? ", yes, status)
                if (.not. status%ok()) exit

                if (yes) then
                    ! Terminate process.
                    call ispell_kill_ispell(st, no_error=.true.)
                    ispell_quit = .true.
                    ispell_quit_position = 0
                    ispell_pdict_modified = .false.
                else
                    ! Continue if they don't quit.
                    again = .true.
                end if
            else if (char == iachar("l")) then
                if (ispell_highlight%choice == ISPELL_HIGHLIGHT_BLOCK) call refresh_highlight()
                call read_string("Lookup string (`*' is wildcard): ", answer, status, initial_input=word)
                if (.not. status%ok()) exit
                call ispell_lookup_words(answer, choices, status)
                if (.not. status%ok()) exit
                call set_buffer(choices_buffer, st)
                call erase_buffer(st)
                call insert_choices(choices, command_characters, count)
                call set_buffer(text_buffer, st)
                textwin = selected_window()
                call ispell_show_choices(status)
                if (.not. status%ok()) exit
                call select_window(textwin, st)
                call set_buffer(text_buffer, st)
                ! Reselect from new choices.
                again = .true.
            else if (char == iachar("u")) then
                ! Insert lowercase into dictionary.
                call ispell_send_string("*"//downcase_string(word)//LF, st)
                ! Dictionary modified!
                ispell_pdict_modified = .true.
            else if (char == iachar("m")) then
                ! Type in what to insert.
                call read_string("Insert: ", answer, status, initial_input=word)
                if (.not. status%ok()) exit
                call ispell_send_string("*"//answer//LF, st)
                ispell_pdict_modified = .true.
                result%kind = REPLACE_RECHECK
                result%word = word
            else if (num >= 0 .and. num < count) then
                result%word = choices(num + 1)%text

                if (ispell_query_replace_choices) then
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
            else if (char == 21 .and. ispell_abbrev_saving_allowed) then
                abbrev_prefix = .not. abbrev_prefix
                again = .true.
            else
                call ding()
                again = .true.
            end if

            if (.not. again) exit
        end do

        ! Unhighlight.
        if (highlighted) call ispell_highlight_spelling_error(start, end, .false.)

    contains

        ! Refresh the highlighting (tty's block cursor).
        subroutine refresh_highlight()
            call ispell_highlight_spelling_error(start, end, .false., .true.)
        end subroutine refresh_highlight

        ! The C-r command: a recursive edit, coming back to START.
        recursive subroutine recursive_edit_session(stat)
            type(status_type), intent(out) :: stat
            integer(int64) :: pos, saved
            type(handle_type) :: b
            type(window_configuration_type) :: config
            type(status_type) :: s

            stat = status_type()
            pos = 0
            if (ispell_recursive_edit_marker%index /= 0) call marker_position(ispell_recursive_edit_marker, pos, s)

            if (pos /= 0) then
                call message("Only one recursive edit session supported")
                call ding()
                call sit_for(2000)
                return
            end if

            if (ispell_recursive_edit_marker%index == 0) call make_marker(ispell_recursive_edit_marker)
            call set_marker(ispell_recursive_edit_marker, start, s)
            ! Unhighlight.
            if (.not. ispell_highlight%is_off()) call ispell_highlight_spelling_error(start, end, .false.)
            call message(substitute_command_keys("Exit recursive edit with \[exit_recursive_edit]"))
            call current_window_configuration(config)
            saved = point()
            call recursive_edit(stat)
            call goto_char(saved)
            call set_window_configuration(config)
            call marker_position(ispell_recursive_edit_marker, pos, s)
            call marker_buffer(ispell_recursive_edit_marker, b, s)
            call goto_char(pos)

            if (b%index /= current) then
                call set_marker(ispell_recursive_edit_marker, 0_int64, s)
                if (stat%ok()) call set_error(stat, FM_GENERIC_ERROR, "Cannot continue ispell from this buffer")
                return
            end if

            call set_marker(ispell_recursive_edit_marker, 0_int64, s)
        end subroutine recursive_edit_session
    end subroutine ispell_command_loop

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
        type(message_log_max_choice_setting_type) :: saved

        saved = message_log_max
        message_log_max%lines = 0
        call message(text)
        message_log_max = saved
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
    recursive subroutine ispell_show_choices(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        ! Display choices above selected window.
        call get_or_make_buffer(ispell_choices_buffer, buffer, status)
        if (.not. status%ok()) return
        call ispell_display_buffer(buffer, status)
    end subroutine ispell_show_choices

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
    recursive subroutine ispell_help(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: help_1, help_2, help_3, help_4, text
        type(window_configuration_type) :: config
        type(handle_type) :: buffer, old, mini
        type(status_type) :: st

        status = status_type()
        help_1 = "[r/R]eplace word; [a/A]ccept for this session; [i]nsert into personal dictionary"
        help_2 = "[l]ook a word up in alternate dictionary;  e[x/X]it;  [q]uit session"
        help_3 = "[u]ncapitalized insert into dict.  "
        if (ispell_abbrev_saving_allowed) help_3 = help_3//"C-u toggles abbrev saving (next replacement)."
        help_4 = "Type `x C-h f ispell_help' for more help"
        text = help_1//LF//help_2//LF//help_3//LF//help_4
        call current_window_configuration(config)
        old = current_buffer()

        if (ispell_help_in_buffer) then
            call get_or_make_buffer("*Ispell Help*", buffer, st)
            call set_buffer(buffer, st)
            call insert(text, st)
            call set_buffer(old, st)
            call ispell_display_buffer(buffer, status)
            call sit_for(int(1000*max(0.5, ispell_help_timeout)))
            call close_buffer(buffer, st)
        else
            mini = windows(selected_frame%minibuffer_window%index)%buffer
            call set_buffer(mini, st)
            call erase_buffer(st)
            call message("")
            call window_resize(selected_frame%minibuffer_window, 2, .false., st)
            call insert(text, st)
            call sit_for(int(1000*max(0.5, ispell_help_timeout)))
            call erase_buffer(st)
        end if

        call set_window_configuration(config)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine ispell_help

    !> Look up WORD in optional word-list dictionary LOOKUP_DICT, the words
    !> found in RESULTS.  A `*' serves as a wild card.  If no wild cards,
    !> `look' is used if it exists.  Otherwise the variable
    !> `ispell_grep_command' contains the command (usually "grep") used to
    !> search for the words.
    !>
    !> Optional second argument contains the dictionary to use; the default
    !> is `ispell_alternate_dictionary', overridden by
    !> `ispell_complete_word_dict' if defined.  If none of LOOKUP_DICT,
    !> `ispell_alternate_dictionary', and `ispell_complete_word_dict' name an
    !> existing word-list file, an error condition occurs.  Port of
    !> ispell.el:ispell-lookup-words.
    subroutine ispell_lookup_words(word, results, status, lookup_dict)
        character(len=*), intent(in) :: word
        type(string_type), allocatable, intent(out) :: results(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: lookup_dict
        character(len=:), allocatable :: dict, prog, args, pattern, line
        type(value_type) :: code, arglist
        type(handle_type) :: old, temp
        logical :: wild, look, has_char
        integer(int32) :: c
        integer(int64) :: loc, shortage
        integer :: k
        type(status_type) :: st

        allocate (results(0))
        dict = ""
        if (present(lookup_dict)) dict = lookup_dict

        if (len(dict) == 0) then
            dict = ispell_complete_word_dict
            if (len(dict) == 0) dict = ispell_alternate_dictionary
        end if

        if (len(dict) > 0) then
            if (.not. is_file_readable(dict)) then
                call set_error(status, FM_GENERIC_ERROR, "ispell-lookup-words: Unreadable or missing plain word-list "// &
                               dict)
                return
            end if
        else
            call set_error(status, FM_GENERIC_ERROR, "ispell-lookup-words: No plain word-list found at system "// &
                           "default locations.  Customize `ispell_alternate_dictionary' to set yours.")
            return
        end if

        wild = index(word, "*") > 0
        look = .false.

        if (len(ispell_look_command) > 0 .and. .not. wild) then
            ! Only use look for an exact match.
            look = is_file_readable(ispell_look_command)
        end if

        if (look) then
            prog = ispell_look_command
            args = ispell_look_options
            pattern = word
        else
            prog = ispell_grep_command
            args = ispell_grep_options
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

        call temp_begin(old, temp)
        call message("Starting """//file_name_nondirectory(prog)//""" process...")

        if (len(args) > 0) then
            arglist = list(value_type(text=args), value_type(text=pattern), value_type(text=dict))
        else if (look) then
            arglist = list(value_type(text=pattern), value_type(text=dict))
        else
            arglist = list(value_type(text="-e"), value_type(text=pattern), value_type(text=dict))
        end if

        call ispell_execute_process(prog, arglist, code, status)

        if (status%ok() .and. vtag(code) == V_STRING) then
            ! `grep' returns status 1 and no output when word not found,
            ! which is a perfectly normal thing.
            call set_error(status, FM_GENERIC_ERROR, "Error: "//file_name_nondirectory(prog)//" exited with signal "// &
                           xstring(code))
        end if

        if (status%ok()) then
            ! Collect words into RESULTS in FIFO order.
            call goto_char(point_max())

            ! Assure we've ended with \n.
            if (.not. is_bob()) then
                call char_after(c, has_char, point() - 1)
                if (c /= 10) call insert(LF, st)
            end if

            do while (.not. is_bob())
                loc = point()
                shortage = forward_line(-1_int64)
                call buffer_substring_no_properties(point(), loc - 1, line, st)
                call prepend_string(results, line)
            end do
        end if

        call temp_end(old, temp)
        if (.not. status%ok()) return

        if (size(results) > 0) then
            if (string_match(".+: ", results(1)%text, st) > 0) then
                call set_error(status, FM_GENERIC_ERROR, ispell_grep_command//" error: "//results(1)%text)
            end if
        end if
    end subroutine ispell_lookup_words

    !> Highlight the word from START to END using overlays.
    !> When HIGHLIGHT is true, the word is highlighted, otherwise it is
    !> displayed normally.
    !>
    !> The variable `ispell_highlight_face' selects the face to use for
    !> highlighting.  Port of
    !> ispell.el:ispell-highlight-spelling-error-overlay (no lazy
    !> highlighting).
    subroutine ispell_highlight_spelling_error_overlay(start, end, highlight)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: highlight
        type(status_type) :: st

        if (highlight) then
            if (is_overlay(ispell_overlay)) then
                call move_overlay(ispell_overlay, start, end, st, current_buffer())
            else
                call make_overlay(ispell_overlay, start, end, st)
                ! Higher than lazy overlays.
                call overlay_put(ispell_overlay, "priority", value_type(1001_int64), st)
                call overlay_put(ispell_overlay, "face", value_type(name=trim(ispell_highlight_face%name)), st)
            end if
        else if (is_overlay(ispell_overlay)) then
            call delete_overlay(ispell_overlay)
            ispell_overlay = null_handle()
        end if
    end subroutine ispell_highlight_spelling_error_overlay

    !> Highlight spelling error between START and END.
    !> HIGHLIGHT true means to highlight the misspelling.  REFRESH is for
    !> the highlighting without colors.  Port of
    !> ispell.el:ispell-highlight-spelling-error (on a terminal without
    !> colors, ispell-highlight-spelling-error-generic is Deferred).
    subroutine ispell_highlight_spelling_error(start, end, highlight, refresh)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: highlight
        logical, intent(in), optional :: refresh

        if (present(refresh)) then
            if (refresh) return
        end if

        if (is_tty_display_color(selected_frame%tty)) call ispell_highlight_spelling_error_overlay(start, end, highlight)
    end subroutine ispell_highlight_spelling_error

    !> Show BUFFER in new window above selected one.
    !> Also position fit window to BUFFER and select it.  Port of
    !> ispell.el:ispell-display-buffer (frames are splittable).
    recursive subroutine ispell_display_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        type(status_type) :: st

        status = status_type()
        window = get_buffer_window(buffer)

        if (window%index == 0) then
            call split_window(selected_window(), -ispell_choices_win_default_height, .true., .false., window, st, &
                              before=.true.)
            if (.not. st%ok()) window = null_handle()
        end if

        if (window%index == 0) call display_buffer(buffer, window, st)

        if (window%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Couldn't make window for *Choices*")
            return
        end if

        call select_window(window, status)
        if (.not. status%ok()) return
        call set_window_buffer(window, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, st)
        call goto_char(point_min())
        call fit_window_to_buffer(window)
    end subroutine ispell_display_buffer

    ! ------------------------------------------------------------------
    ! Dictionaries
    ! ------------------------------------------------------------------

    !> Change to dictionary DICT for Ispell.
    !> If ARG is true (interactively, the prefix arg), set it "globally",
    !> for all buffers.  Otherwise, set it "locally", just for this buffer.
    !>
    !> By just answering RET you can find out the name of the current
    !> dictionary.  Port of ispell.el:ispell-change-dictionary.
    recursive subroutine ispell_change_dictionary(dict, status, arg, called_interactively)
        character(len=*), intent(in) :: dict
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg, called_interactively
        character(len=:), allocatable :: name, current_name
        logical :: global
        type(status_type) :: st

        global = .false.
        if (present(arg)) global = arg
        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return
        if (.not. global) call ispell_buffer_local_dict(status, no_reload=.true.)
        name = dict
        if (name == "default") name = ""
        current_name = ""
        if (.not. global) current_name = string_value(ispell_local_dictionary)
        if (len(current_name) == 0) current_name = ispell_dictionary
        if (len(current_name) == 0) current_name = "default"

        if (len(dict) == 0) then
            ! This relies on completing-read's bug of returning "" for no
            ! match.
            call ispell_internal_change_dictionary()
            call message("Using "//current_name//" dictionary")
            call ispell_change_dictionary_hook%execute(st)
        else if (name == current_name) then
            ! Specified dictionary is the default already.  Could reload
            ! the dictionaries if needed.
            call ispell_internal_change_dictionary()

            if (present(called_interactively)) then
                if (called_interactively) call message("No change, using "//name//" dictionary")
            end if
        else
            ! Reset dictionary!
            if (find_dictionary(ispell_local_dictionaries, name) == 0 .and. &
                find_dictionary(ispell_dictionaries, name) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "Undefined dictionary: "//name)
                return
            end if

            if (global) then
                ! Set default dictionary.
                ispell_dictionary = name
            else
                ! Set local dictionary.
                if (len(name) == 0) then
                    call ispell_local_dictionary%set_local("")
                    call ispell_local_dictionary%clear()
                else
                    call ispell_local_dictionary%set_local(name)
                end if

                call ispell_local_dictionary_overridden%set_local(.true.)
            end if

            call ispell_internal_change_dictionary()
            call clear_session_localwords()

            if (global) then
                call message("Global Ispell dictionary set to "//dict)
            else
                call message("Local Ispell dictionary set to "//dict)
            end if

            call ispell_change_dictionary_hook%execute(st)
        end if
    end subroutine ispell_change_dictionary

    ! The command `ispell_change_dictionary'.
    recursive subroutine cmd_ispell_change_dictionary(status)
        type(status_type), intent(out) :: status
        character(len=64), allocatable :: names(:)
        type(completion_table_type) :: table
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: dict
        integer :: k

        call ispell_valid_dictionary_list(names, status)
        if (.not. status%ok()) return
        allocate (items(size(names)))

        do k = 1, size(names)
            items(k) = value_type(text=trim(names(k)))
        end do

        table%kind = TABLE_LIST
        table%list = make_list(items)
        call completing_read("Use new dictionary (RET for current, SPC to complete): ", table, dict, status, &
                             require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ispell_change_dictionary(dict, status, current_prefix_arg%kind /= PREFIX_NONE, .true.)
    end subroutine cmd_ispell_change_dictionary

    !> Update the dictionary and the personal dictionary used by Ispell.
    !> This may kill the Ispell process; if so, a new one will be started
    !> when needed.  Port of ispell.el:ispell-internal-change-dictionary.
    recursive subroutine ispell_internal_change_dictionary()
        character(len=:), allocatable :: dict, pdict, expanded
        type(status_type) :: st

        dict = string_value(ispell_local_dictionary)
        if (len(dict) == 0) dict = ispell_dictionary
        pdict = string_value(ispell_local_pdict)
        if (len(pdict) == 0) pdict = ispell_personal_dictionary
        expanded = ""
        if (len(pdict) > 0) call expand_file_name(pdict, expanded, st)
        if (ispell_current_dictionary == dict .and. ispell_current_personal_dictionary == expanded) return
        call ispell_kill_ispell(st, no_error=.true.)
        ispell_current_dictionary = dict
        ispell_current_personal_dictionary = expanded
    end subroutine ispell_internal_change_dictionary

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
    recursive subroutine ispell_region(reg_start, reg_end, status, recheck, shift, quit)
        integer(int64), intent(in) :: reg_start, reg_end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recheck
        integer(int64), intent(inout), optional :: shift
        logical, intent(out), optional :: quit
        type(handle_type) :: skip_region_start, rstart, choices
        character(len=:), allocatable :: region_type, program_basename, dictionary, key, in_comment, add_comment, &
                                         string, skip_regexp
        integer(int64) :: max_word, found, region_end_pos, end, saved_point, pos, ispell_start, ispell_end, my_shift, limit
        type(window_configuration_type) :: config
        logical :: recheckp, saved_fold, has_line, has_max
        type(status_type) :: st

        status = status_type()
        recheckp = .false.
        if (present(recheck)) recheckp = recheck
        my_shift = 0
        if (present(shift)) my_shift = shift
        if (present(quit)) quit = .false.
        ! Initialize variables and dicts alists.
        call ispell_set_spellchecker_params(status)
        if (.not. status%ok()) return

        ! Set up dictionary, local words, etc.
        if (.not. recheckp) then
            call ispell_accept_buffer_local_defs(status)
            if (.not. status%ok()) return
        end if

        call make_marker(skip_region_start)
        call make_marker(rstart)

        if (reg_start == point_min() .and. reg_end == point_max()) then
            region_type = buffers(current)%name
        else
            region_type = "region"
        end if

        program_basename = file_name_nondirectory(ispell_program_name)
        dictionary = ispell_current_dictionary
        if (len(dictionary) == 0) dictionary = "default"
        has_max = .false.
        max_word = 0
        saved_point = point()
        call message("Spell-checking "//region_type//" using "//program_basename//" with "//dictionary// &
                     " dictionary...")
        ! Returns cursor to original location.
        call current_window_configuration(config)
        call goto_char(reg_start)
        saved_fold = case_fold_search%value
        in_comment = ""
        key = ""
        skip_regexp = ispell_begin_skip_region_regexp()
        call re_search_forward(skip_regexp, found, st, reg_end, SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            key = match_string_no_properties(0)
            call set_marker(skip_region_start, point() - len_chars(key), st)
            call goto_char(reg_start)
        end if

        call set_marker(rstart, reg_start, st)
        if (ispell_region_end%index == 0) call make_marker(ispell_region_end)
        call set_marker(ispell_region_end, reg_end, st)
        end = reg_end

        do while (.not. ispell_quit .and. status%ok())
            region_end_pos = marker_pos(ispell_region_end)
            if (point() >= region_end_pos) exit
            pos = marker_pos(skip_region_start)

            ! Spell-check region with skipping.
            if (pos /= 0 .and. pos <= point()) then
                ! If region inside line comment, must keep comment start.
                in_comment = ""

                if (allocated(comment_start%value)) then
                    if (string_value(comment_end) == "") then
                        limit = point()
                        call beginning_of_line()
                        call re_search_forward(comment_start%value, found, st, limit, SEARCH_NOERROR)
                        if (st%ok() .and. found > 0) in_comment = comment_start%value
                        call goto_char(limit)
                    end if
                end if

                call goto_char(pos)
                ! Can change skip-regexps (in ispell-message).  Moves point
                ! past region.
                call ispell_skip_region(key, status)
                if (.not. status%ok()) exit
                call set_marker(rstart, point(), st)

                if (marker_pos(rstart) < marker_pos(ispell_region_end)) then
                    call re_search_forward(ispell_begin_skip_region_regexp(), found, st, marker_pos(ispell_region_end), &
                                           SEARCH_NOERROR)
                else
                    found = 0
                end if

                if (st%ok() .and. found > 0) then
                    key = match_string_no_properties(0)
                    call set_marker(skip_region_start, point() - len_chars(key), st)
                    call goto_char(marker_pos(rstart))
                else
                    call set_marker(skip_region_start, 0_int64, st)
                end if
            end if

            pos = marker_pos(skip_region_start)

            if (pos /= 0) then
                end = max(point(), min(pos, marker_pos(ispell_region_end)))
            else
                end = max(point(), marker_pos(ispell_region_end))
            end if

            ispell_start = point()
            ispell_end = min(line_end_position(), end)
            ! See if line must be prefixed by comment string to let ispell
            ! know this is part of a comment string.  This is only
            ! supported in some modes.  In particular, this is not
            ! supported in autoconf mode where adding the comment string
            ! messes everything up because ispell tries to spellcheck the
            ! `dnl' string header causing misalignments in some cases
            ! (debbugs.gnu.org: #12768).
            add_comment = ""
            if (len(in_comment) > 0 .and. in_comment /= "dnl ") add_comment = in_comment
            call ispell_get_line(ispell_start, ispell_end, add_comment, string, has_line)

            if (len(add_comment) > 0) then
                ! Account for comment chars added.
                ispell_start = ispell_start - len_chars(add_comment)
                ! Reset `in_comment' (and indirectly `add_comment') for new
                ! line.
                in_comment = ""
            end if

            ! "end" tracks region retrieved.
            ispell_end = point()

            ! There is something to spell check!
            if (has_line) then
                if (.not. recheckp) my_shift = 0
                call ispell_process_line(string, ispell_start, ispell_end, my_shift, found, status)

                if (found > 0) then
                    max_word = found
                    has_max = .true.
                end if
            end if

            call goto_char(ispell_end)
        end do

        case_fold_search = saved_fold
        call goto_char(saved_point)
        call set_window_configuration(config)

        ! (The protected part.)
        if (.not. (recheckp .and. ispell_keep_choices_win)) then
            choices = get_buffer(ispell_choices_buffer)
            if (choices%index /= 0) call close_buffer(choices, st)
        end if

        call destroy_marker(skip_region_start, st)
        call destroy_marker(rstart, st)
        ! Allow the user to pop back to the last position.
        if (has_max) call push_mark(max_word, .true.)

        if (ispell_quit) then
            if (present(quit)) quit = .true.

            ! Preserve or clear the region for ispell-continue.
            if (ispell_quit_position == 0) then
                call set_marker(ispell_region_end, 0_int64, st)
            else
                ! Ispell-continue enabled - ispell-region-end is set.
                call goto_char(ispell_quit_position)
            end if

            if (.not. recheckp) then
                ispell_quit = .false.
                ispell_quit_position = 0
            end if
        else
            if (.not. recheckp) call set_marker(ispell_region_end, 0_int64, st)

            if (status%ok()) then
                ! Only save if successful exit.
                call ispell_pdict_save(st, ispell_silently_save)
                call message("Spell-checking "//region_type//" using "//program_basename//" with "//dictionary// &
                             " dictionary...done")
            end if
        end if

        if (present(shift)) shift = my_shift
    end subroutine ispell_region

    ! The command `ispell_region'.
    recursive subroutine cmd_ispell_region(status)
        type(status_type), intent(out) :: status

        call ispell_region(active_region_beginning(), active_region_end(), status)
    end subroutine cmd_ispell_region

    ! The beginning of the region (region-beginning).  Not a port.
    integer(int64) function active_region_beginning() result(pos)
        type(status_type) :: st

        call region_beginning(pos, st)
        if (.not. st%ok()) pos = point()
    end function active_region_beginning

    ! The end of the region (region-end).  Not a port.
    integer(int64) function active_region_end() result(pos)
        type(status_type) :: st

        call region_end(pos, st)
        if (.not. st%ok()) pos = point()
    end function active_region_end

    ! The position of marker M, 0 if it points nowhere.  Not a port.
    integer(int64) function marker_pos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        pos = 0
        if (m%index == 0) return
        call marker_position(m, pos, st)
        if (.not. st%ok()) pos = 0
    end function marker_pos

    !> Return a regexp of the search keys for region skipping.
    !> Includes `ispell_skip_regions' plus tex, tib, html, and comment keys.
    !> Must be called after `ispell_buffer_local_parsing' due to dependence
    !> on mode.  Port of ispell.el:ispell-begin-skip-region-regexp.
    function ispell_begin_skip_region_regexp() result(regexp)
        character(len=:), allocatable :: regexp
        integer :: k

        regexp = ""
        k = 0
        ! Tex.
        if (ispell_parser%value == ISPELL_PARSER_TEX) call add(ispell_begin_tex_skip_regexp())
        ! Html stuff.
        if (.not. ispell_skip_html%is_off()) call add(ispell_begin_skip_region(ispell_html_skip_regions))
        ! Tib.
        if (ispell_skip_tib) call add(ispell_tib_ref_beginning)

        ! Comments.
        if (ispell_check_comments%choice == ISPELL_CHECK_COMMENTS_EXCLUSIVE .and. allocated(comment_start%value)) then
            ! Search from end of current comment to start of next comment.
            if (string_value(comment_end) == "") then
                call add("^")
            else
                call add(regexp_quote(comment_end%value))
            end if
        end if

        if (ispell_check_comments%is_off() .and. allocated(comment_start%value)) then
            call add(regexp_quote(comment_start%value))
        end if

        if (size(ispell_skip_regions) > 0) call add(ispell_begin_skip_region(ispell_skip_regions))
        call add(ispell_make_filename_or_url_regexp())

    contains

        ! Add PART, separated by "\|".
        subroutine add(part)
            character(len=*), intent(in) :: part

            if (len(regexp) > 0 .or. k > 0) then
                regexp = regexp//"\|"//part
            else
                regexp = part
            end if

            k = k + 1
        end subroutine add
    end function ispell_begin_skip_region_regexp

    !> Regular expression for start of regions to skip generated from
    !> SKIPS.  Each selection should be a key of SKIPS; otherwise, the
    !> current line is skipped.  Port of ispell.el:ispell-begin-skip-region.
    function ispell_begin_skip_region(skips) result(regexp)
        type(ispell_skip_region_type), intent(in) :: skips(:)
        character(len=:), allocatable :: regexp
        integer :: k

        regexp = ""

        do k = 1, size(skips)
            if (k > 1) regexp = regexp//"\|"
            regexp = regexp//trim(skips(k)%beginning_regexp)
        end do
    end function ispell_begin_skip_region

    !> Regular expression of tex commands to skip.
    !> Generated from `ispell_tex_skip_regions' and
    !> `ispell_tex_skip_environments'.  Port of
    !> ispell.el:ispell-begin-tex-skip-regexp.
    function ispell_begin_tex_skip_regexp() result(regexp)
        character(len=:), allocatable :: regexp
        integer :: k

        ! Raw tex keys.
        regexp = ispell_begin_skip_region(ispell_tex_skip_regions)//"\|"

        ! Keys wrapped in begin{}.
        do k = 1, size(ispell_tex_skip_environments)
            if (k > 1) regexp = regexp//"\|"
            regexp = regexp//"\\begin[ "//TAB//LF//"]*{"//trim(ispell_tex_skip_environments(k)%beginning_regexp)//"}"
        end do
    end function ispell_begin_tex_skip_regexp

    !> Return the key and body regions to skip for this buffer.
    !> Includes regions defined by `ispell_skip_regions', tex mode, and
    !> `ispell_html_skip_regions'.  Manual checking must include comments
    !> and tib references.  Must be called after
    !> `ispell_buffer_local_parsing' due to dependence on mode.  Port of
    !> ispell.el:ispell-skip-region-list (no `ispell-checking-message').
    subroutine ispell_skip_region_list(skips)
        type(ispell_skip_region_type), allocatable, intent(out) :: skips(:)
        type(ispell_skip_region_type) :: url

        url%beginning_regexp = ispell_make_filename_or_url_regexp()
        skips = [url, ispell_skip_regions]

        ! Only additional explicit region definition is tex.
        if (ispell_parser%value == ISPELL_PARSER_TEX) then
            case_fold_search = .false.
            skips = [ispell_tex_skip_regions, ispell_tex_skip_environments, skips]
        end if

        if (.not. ispell_skip_html%is_off()) skips = [ispell_html_skip_regions, skips]
    end subroutine ispell_skip_region_list

    !> Return CHAR as a string when CHAR doesn't have "word" syntax, ""
    !> otherwise.  Port of ispell.el:ispell--\\w-filter.
    function ispell_w_filter(char) result(text)
        character(len=*), intent(in) :: char
        character(len=:), allocatable :: text
        type(status_type) :: st

        text = ""
        if (string_match("\w", char, st) == 0) text = char
    end function ispell_w_filter

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
    function ispell_make_w_expression(chars) result(regexp)
        character(len=*), intent(in) :: chars
        character(len=:), allocatable :: regexp
        character(len=:), allocatable :: filtered
        integer :: k

        filtered = ""

        do k = 1, len(chars)
            filtered = filtered//ispell_w_filter(chars(k:k))
        end do

        regexp = "\(\w"

        if (len(filtered) == 0) then
            regexp = regexp//"\)"
        else if (len(filtered) == 1) then
            regexp = regexp//"\|"//filtered//"\)"
        else
            regexp = regexp//"\|["//filtered//"]\)"
        end if
    end function ispell_make_w_expression

    !> Construct a regexp to match some file names or URLs or email
    !> addresses.  The expression is crafted to match as great a variety of
    !> these objects as practicable, without too many false matches
    !> happening.  Port of ispell.el:ispell--make-filename-or-URL-re.
    function ispell_make_filename_or_url_regexp() result(regexp)
        character(len=:), allocatable :: regexp

        regexp = "\(/\w\|\("//ispell_make_w_expression("-_")//"+[.:@]\)\)"//ispell_make_w_expression("-_")// &
                 "*\([.:/@]+"//ispell_make_w_expression("-_~=?&")//"+\)+"
    end function ispell_make_filename_or_url_regexp

    !> Skip across ARG number of braces.  Port of
    !> ispell.el:ispell-tex-arg-end.
    recursive subroutine ispell_tex_arg_end(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()

        do while (looking_at("[ "//TAB//LF//"]*\[", st))
            call forward_expression(1_int64, st)
            if (.not. st%ok()) exit
        end do

        if (st%ok()) call forward_expression(int(arg, int64), st)

        if (.not. st%ok()) then
            call message("Error skipping expressions at point "//integer_text(point())//".")
            call ding()
            call sit_for(2000)
        end if
    end subroutine ispell_tex_arg_end

    ! forward-line as a function of a region to skip.  Not a port.
    recursive subroutine skip_line(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        status = status_type()
        shortage = forward_line(int(arg, int64))
    end subroutine skip_line

    !> Skip across KEY and then to end of region.
    !> Key lookup determines region to skip.
    !> Point is placed at end of skipped region.  Port of
    !> ispell.el:ispell-skip-region.
    recursive subroutine ispell_skip_region(key, status)
        character(len=*), intent(in) :: key
        type(status_type), intent(out) :: status
        type(ispell_skip_region_type), allocatable :: skips(:)
        integer(int64) :: start, found, shortage
        logical :: null_skip
        type(status_type) :: st
        integer :: k

        status = status_type()
        ! Move over key to begin checking.
        call forward_char(st, len_chars(key))
        start = point()
        ! Regenerate each call... This function can change region
        ! definition.
        call ispell_skip_region_list(skips)
        null_skip = .false.

        if (ispell_check_comments%is_off() .and. allocated(comment_start%value) .and. &
            key == string_value(comment_start)) then
            ! What about quoted comment, or comment inside strings?
            if (string_value(comment_end) == "") then
                shortage = forward_line(1_int64)
            else
                call search_forward(comment_end%value, found, st, marker_pos(ispell_region_end), SEARCH_NOERROR)
            end if
        else if (ispell_check_comments%choice == ISPELL_CHECK_COMMENTS_EXCLUSIVE .and. allocated(comment_start%value) .and. &
                 key == string_value(comment_end)) then
            call search_forward(comment_start%value, found, st, marker_pos(ispell_region_end), SEARCH_MOVE)
        else if (ispell_skip_tib .and. string_match(ispell_tib_ref_beginning, key, st) > 0) then
            call re_search_forward(ispell_tib_ref_end, found, st, marker_pos(ispell_region_end), SEARCH_NOERROR)
        else
            ! Markings from the list.
            do k = 1, size(skips)
                if (string_match(trim(skips(k)%beginning_regexp), key, st) == 0) cycle

                if (associated(skips(k)%skip)) then
                    ! Error handling in functions!
                    null_skip = .true.
                    call skips(k)%skip(skips(k)%arg, st)
                else if (len_trim(skips(k)%end_regexp) > 0) then
                    ! Search past end of spell region to find this region
                    ! end.
                    call re_search_forward(trim(skips(k)%end_regexp), found, st, point_max(), SEARCH_NOERROR)
                else
                    ! Done!  Just skip key.
                    null_skip = .true.
                end if

                exit
            end do
        end if

        if (start == point() .and. .not. null_skip) then
            call message("Matching region end for `"//key//"' point "//integer_text(point())//" not found")
            call ding()
            call sit_for(2000)
        end if
    end subroutine ispell_skip_region

    !> Grab the next line of text between START and END into STRING (HAS
    !> false when there is none), prefixed with IN_COMMENT.  Port of
    !> ispell.el:ispell-get-line.
    subroutine ispell_get_line(start, end, in_comment, string, has)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: in_comment
        character(len=:), allocatable, intent(out) :: string
        logical, intent(out) :: has
        character(len=:), allocatable :: casechars, text
        integer(int64) :: found, shortage
        type(status_type) :: st

        has = .false.
        string = ""
        casechars = ispell_get_casechars()

        ! Look at this line and skip or process.
        if (is_eol()) then
            ! End of line, just go to next line.
            shortage = forward_line(1_int64)
            return
        end if

        call re_search_forward(casechars, found, st, end, SEARCH_NOERROR)
        ! Or math commands.
        if (.not. (st%ok() .and. found > 0)) call re_search_forward("[][()${}]", found, st, end, SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            ! Text exists.
            call buffer_substring_no_properties(start, end, text, st)
            string = "^"//in_comment//text//LF
            has = .true.
        end if

        ! Empty line, skip it.
        call goto_char(end)
    end subroutine ispell_get_line

    !> Whether the text at point is STRING, compared in the dictionary's
    !> coding system.  Port of ispell.el:ispell-looking-at.
    logical function ispell_looking_at(string) result(matched)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: coding, text, a, b
        integer(int64) :: n
        type(status_type) :: st

        matched = .false.
        coding = ispell_get_coding_system()
        n = len_chars(string)
        if (point() + n > point_max()) return
        call buffer_substring_no_properties(point(), point() + n, text, st)
        call encode_coding_string(string, coding, a, st)
        call encode_coding_string(text, coding, b, st)
        matched = a == b
    end function ispell_looking_at

    !> Send STRING, a line of text, to ispell and process the result.
    !> This will modify the buffer for spelling errors.  ISPELL_START and
    !> ISPELL_END are the line's bounds (upstream's ispell-start and ispell-end, saved and
    !> restored by its callers).
    !>
    !> SHIFT is the sum of the shifts due to changes in word replacements,
    !> MAX_WORD the location of the final word that was queried about (0
    !> if none).  Port of ispell.el:ispell-process-line.
    recursive subroutine ispell_process_line(string, ispell_start, ispell_end, shift, max_word, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(inout) :: ispell_start, ispell_end
        integer(int64), intent(inout) :: shift
        integer(int64), intent(out) :: max_word
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: accept_list(:), recheck_filter(:), lines(:)
        type(ispell_output_type) :: poss
        type(replacement_type) :: replace
        type(handle_type) :: word_start, line_end, line_start, region_end
        type(window_configuration_type) :: config
        integer(int64) :: word_len, ws, sub_shift, actual
        logical :: recheck_region, sub_quit
        type(status_type) :: st
        integer :: n

        status = status_type()
        max_word = 0
        allocate (accept_list(0))
        ! Send string to spell process and get input.
        call ispell_send_string(string, status)

        do
            call ispell_accept_output(status)
            if (.not. status%ok()) return
            ! Last item of output contains a blank line.
            if (is_answer_complete()) exit
        end do

        ! Parse all inputs from the stream one word at a time.  Remove the
        ! blank item.
        n = size(ispell_filter_variable)
        lines = ispell_filter_variable(1:n - 1)
        call clear_filter()

        do while (.not. ispell_quit .and. size(lines) > 0)
            ! Get next word, accounting for accepted words and start shifts.
            call ispell_parse_output(lines(1)%text, poss, accept_list, shift)

            if (poss%kind == ISPELL_OUTPUT_MISSPELLED) then
                ! Whenever we have misspellings, we can change the buffer.
                ! Keep boundaries as markers.  There is a -1 offset here as
                ! the string is escaped with '^' to prevent us accidentally
                ! sending any ispell commands.
                call make_marker(word_start)
                call set_marker(word_start, ispell_start - 1 + poss%offset, st)
                word_len = len_chars(poss%word)
                call make_marker(line_end)
                call set_marker(line_end, ispell_end, st)
                call make_marker(line_start)
                call set_marker(line_start, ispell_start, st)
                recheck_region = .false.
                ispell_abbrev_saving_allowed = .true.
                ispell_save_correction_as_abbrev = ispell_save_corrections_as_abbrevs
                ws = marker_pos(word_start)
                call goto_char(ws)

                ! Alignment cannot be tracked and this error will occur when
                ! `query_replace' makes multiple corrections on the starting
                ! line.
                if (.not. ispell_looking_at(poss%word)) then
                    ! This error occurs due to filter pipe problems.
                    actual = ws
                    call set_error(status, FM_GENERIC_ERROR, "Ispell misalignment: word `"//poss%word//"' point "// &
                                   integer_text(actual)//"; probably incompatible versions")
                    call release_markers()
                    return
                end if

                max_word = ws

                ! The command loop can go recursive & change buffer.
                if (ispell_keep_choices_win) then
                    call ispell_command_loop(poss%misses, poss%guesses, poss%word, ws, ws + word_len, replace, status)
                else
                    call current_window_configuration(config)
                    call ispell_command_loop(poss%misses, poss%guesses, poss%word, ws, ws + word_len, replace, status)
                    call set_window_configuration(config)
                end if

                ispell_abbrev_saving_allowed = .false.

                if (.not. status%ok()) then
                    call release_markers()
                    return
                end if

                ws = marker_pos(word_start)
                call goto_char(ws)

                ! Recheck when query replace edit changes misspelled word.
                ! Error in tex mode when a potential math mode change exists.
                if (replace%kind == REPLACE_QUERY .or. replace%kind == REPLACE_RECHECK_HERE) then
                    if (ispell_parser%value == ISPELL_PARSER_TEX .and. &
                        string_match("[\][]()[]\|\\begin\|\$", regexp_quote(string), st) > 0) then
                        call set_error(status, FM_GENERIC_ERROR, &
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
                    if (replace%kind == REPLACE_LOCAL) call ispell_add_per_file_word_list(poss%word, st)
                    ! Do not recheck accepted word on this line.
                    call prepend_string(accept_list, poss%word)
                else
                    ! Replacement word selected or entered.
                    call delete_region(point(), point() + word_len, st)

                    if (replace%kind == REPLACE_WORD) then
                        ! Insert dictionary word.
                        call insert(replace%word, st)
                        call ispell_send_replacement(poss%word, replace%word)
                        call ispell_maybe_save_correction_abbrev(poss%word, replace%word)
                        call prepend_string(accept_list, replace%word)
                    else
                        ! Recheck hand entered replacement word.
                        call insert(replace%word, st)
                        call ispell_send_replacement(poss%word, replace%word)
                        call ispell_maybe_save_correction_abbrev(poss%word, replace%word)

                        if (replace%kind == REPLACE_QUERY) then
                            call current_window_configuration(config)
                            ! Assume case-replace & case-fold-search correct?
                            call query_replace(poss%word, replace%word, st, delimited=.true.)
                            call set_window_configuration(config)
                        end if

                        call goto_char(marker_pos(word_start))

                        ! Do not recheck if already accepted.
                        if (has_word(accept_list, replace%word)) then
                            call prepend_string(accept_list, replace%word)
                        else
                            call make_marker(region_end)
                            call set_marker(region_end, marker_pos(ispell_region_end), st)
                            ! Save filter.
                            recheck_filter = lines
                            ! Already accounted.
                            sub_shift = 0
                            ws = marker_pos(word_start)
                            call ispell_region(ws, ws + len_chars(replace%word), status, .true., sub_shift, sub_quit)
                            ! Quitting check.
                            shift = sub_shift
                            if (sub_quit) shift = 0
                            call set_marker(ispell_region_end, marker_pos(region_end), st)
                            call destroy_marker(region_end, st)
                            lines = recheck_filter
                            recheck_region = .false.

                            if (.not. status%ok()) then
                                call release_markers()
                                return
                            end if
                        end if
                    end if

                    shift = shift + (len_chars(replace%word) - word_len)
                end if

                if (.not. ispell_quit) then
                    call message_unlogged("Continuing spelling check using "// &
                                          file_name_nondirectory(ispell_program_name)//" with "// &
                                          dictionary_name()//" dictionary...")
                end if

                call sit_for(0)
                ispell_start = marker_pos(line_start)
                ispell_end = marker_pos(line_end)
                ws = marker_pos(word_start)
                ! Adjust markers when end of region lost from highlighting.
                if (.not. recheck_region .and. ispell_end < ws + word_len) ispell_end = ws + word_len
                if (ws == marker_pos(ispell_region_end)) call set_marker(ispell_region_end, ws + word_len, st)
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
    end subroutine ispell_process_line

    ! The current dictionary's name, "default" for the default.  Not a port.
    function dictionary_name() result(name)
        character(len=:), allocatable :: name

        name = ispell_current_dictionary
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
    recursive subroutine ispell_comments_and_strings(status, start, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(parse_state_type) :: state, next
        integer(int64) :: limit, from, saved
        logical :: done, quit

        status = status_type()
        limit = point_max()
        if (present(end)) limit = end

        if (present(start)) then
            call goto_char(start)
        else
            call goto_char(point_min())
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
                    call set_error(status, FM_GENERIC_ERROR, "Unterminated string or comment")
                    return
                end if

                saved = point()
                call ispell_region(from, point(), status, quit=quit)
                call goto_char(saved)
                if (.not. status%ok()) return
                done = quit
            end if
        end do
    end subroutine ispell_comments_and_strings

    ! The command `ispell_comments_and_strings'.
    recursive subroutine cmd_ispell_comments_and_strings(status)
        type(status_type), intent(out) :: status

        if (is_use_region()) then
            call ispell_comments_and_strings(status, active_region_beginning(), active_region_end())
        else
            call ispell_comments_and_strings(status)
        end if
    end subroutine cmd_ispell_comments_and_strings

    !> Check the comment or string containing point for spelling errors.
    !> Port of ispell.el:ispell-comment-or-string-at-point.
    recursive subroutine ispell_comment_or_string_at_point(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state, next
        integer(int64) :: saved, start

        status = status_type()
        saved = point()
        call syntax_ppss(state)

        if (state%instring >= 0 .or. state%incomment /= 0) then
            start = state%comstr_start
            call parse_partial_expression(point(), point_max(), next, status, oldstate=state, &
                                          commentstop=COMMENTSTOP_SYNTAX_TABLE)
            if (status%ok()) call ispell_region(start, point(), status)
        else
            call set_error(status, FM_USER_ERROR, "Not inside a string or comment")
        end if

        call goto_char(saved)
    end subroutine ispell_comment_or_string_at_point

    !> Check the current buffer for spelling errors interactively.
    !> Leave the mark at the last misspelled word that the user was queried
    !> about.  Port of ispell.el:ispell-buffer.
    recursive subroutine ispell_buffer(status)
        type(status_type), intent(out) :: status

        call ispell_region(point_min(), point_max(), status)
    end subroutine ispell_buffer

    !> Continue a halted spelling session beginning with the current word.
    !> Port of ispell.el:ispell-continue.
    recursive subroutine ispell_continue(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: b
        character(len=:), allocatable :: word
        integer(int64) :: start, end
        type(status_type) :: st

        status = status_type()

        if (marker_pos(ispell_region_end) == 0) then
            call message("No session to continue.  Use `X' command when checking!")
            return
        end if

        call marker_buffer(ispell_region_end, b, st)

        if (b%index /= current) then
            call message("Must continue ispell from buffer "//buffers(b%index)%name)
            return
        end if

        ! Find beginning of current word.
        call ispell_get_word(.true., word, start, end, status)
        if (.not. status%ok()) return
        call ispell_region(start, marker_pos(ispell_region_end), status)
    end subroutine ispell_continue

    !> Interactively check a region or buffer for spelling errors.
    !> If `transient_mark_mode' is on, and a region is active, spell-check
    !> that region.  Otherwise spell-check the buffer.
    !>
    !> Ispell dictionaries are not distributed with Fortmacs.  If you are
    !> looking for a dictionary, please see the distribution of the GNU
    !> ispell program, or do an Internet search; there are various
    !> dictionaries available on the net.  Port of ispell.el:ispell.
    recursive subroutine ispell(status)
        type(status_type), intent(out) :: status

        if (is_region_active()) then
            call ispell_region(active_region_beginning(), active_region_end(), status)
        else
            call ispell_buffer(status)
        end if
    end subroutine ispell

    ! The command `ispell_check_version'.
    recursive subroutine cmd_ispell_check_version(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: result

        call ispell_check_version(result, status, .true.)
    end subroutine cmd_ispell_check_version

    ! ------------------------------------------------------------------
    ! Buffer local functions
    ! ------------------------------------------------------------------

    !> Load all buffer-local information, restarting Ispell when necessary.
    !> Port of ispell.el:ispell-accept-buffer-local-defs.
    recursive subroutine ispell_accept_buffer_local_defs(status)
        type(status_type), intent(out) :: status

        ! May kill ispell-process.
        call ispell_buffer_local_dict(status)
        if (.not. status%ok()) return
        ! Will initialize ispell-process.
        call ispell_buffer_local_words(status)
        if (.not. status%ok()) return
        call ispell_buffer_local_parsing(status)
    end subroutine ispell_accept_buffer_local_defs

    !> Place Ispell into parsing mode for this buffer.
    !> Overrides the default parsing mode.
    !> Includes LaTeX/Nroff modes and extended character mode.  Port of
    !> ispell.el:ispell-buffer-local-parsing.
    recursive subroutine ispell_buffer_local_parsing(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: mode, major, string
        integer(int64) :: saved, found, end
        type(status_type) :: st

        ! Put process in terse mode.
        call ispell_send_string("!"//LF, status)
        major = ""
        if (allocated(buffers(current)%major_mode)) major = buffers(current)%major_mode

        ! We assume all major modes with "tex_mode" in them should use latex
        ! parsing.  When exclusively checking comments, set to raw text
        ! mode (nroff).
        if (ispell_check_comments%choice /= ISPELL_CHECK_COMMENTS_EXCLUSIVE .and. &
            ((ispell_parser%value == ISPELL_PARSER_USE_MODE_NAME .and. &
              string_match("[Tt][Ee][Xx]_mode", major, st) > 0) .or. ispell_parser%value == ISPELL_PARSER_TEX)) then
            ! Set ispell mode to tex.
            call ispell_send_string("+"//LF, st)
            if (ispell_parser%value /= ISPELL_PARSER_TEX) call ispell_parser%set_local(ISPELL_PARSER_TEX)
        else
            ! Set mode to normal (nroff).
            call ispell_send_string("-"//LF, st)
        end if

        ! If needed, test for SGML & HTML modes and set a buffer local value.
        if (ispell_skip_html%choice == ISPELL_SKIP_HTML_USE_MODE_NAME) then
            if (string_match("sgml\|html\|xml", downcase_string(major), st) > 0) then
                call ispell_skip_html%set_local(.true.)
            else
                call ispell_skip_html%set_local(.false.)
            end if
        end if

        ! Set default extended character mode for given buffer, if any.
        mode = ispell_get_extended_character_mode()
        if (len(mode) > 0) call ispell_send_string(mode//LF, st)
        ! Set buffer-local parsing mode and extended character mode, if
        ! specified.
        saved = point()
        call goto_char(point_max())
        ! Uses last occurrence of `ispell_parsing_keyword'.
        call search_backward(ispell_parsing_keyword, found, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            end = line_end_position()
            call search_forward(ispell_parsing_keyword, found, st)

            do
                call re_search_forward(" *\([^ ""]+\)", found, st, end, SEARCH_NOERROR)
                if (.not. st%ok() .or. found == 0) exit
                ! Space separated definitions.
                string = downcase_string(match_string_no_properties(1))

                if (index(string, "latex_mode") > 0 .and. ispell_check_comments%choice /= ISPELL_CHECK_COMMENTS_EXCLUSIVE) then
                    call ispell_send_string("+"//LF//"~tex"//LF, st)
                else if (index(string, "nroff_mode") > 0) then
                    call ispell_send_string("-"//LF//"~nroff"//LF, st)
                else if (index(string, "~") > 0) then
                    ! Set extended character mode.
                    call ispell_send_string(string//LF, st)
                else
                    call message("Invalid Ispell Parsing argument!")
                    call sit_for(2000)
                end if
            end do
        end if

        call goto_char(saved)
    end subroutine ispell_buffer_local_parsing

    !> Initialize local dictionary and local personal dictionary.
    !> If optional NO_RELOAD is true, do not reload any dictionary.
    !> When a dictionary is defined in the buffer (see variable
    !> `ispell_dictionary_keyword'), it will override the local setting from
    !> M-x ispell_change_dictionary.  Both should not be used to define a
    !> buffer-local dictionary.  Port of ispell.el:ispell-buffer-local-dict.
    recursive subroutine ispell_buffer_local_dict(status, no_reload)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_reload
        integer(int64) :: saved, found, end
        type(status_type) :: st

        status = status_type()
        saved = point()
        ! Override the local variable definition.  Uses last occurrence of
        ! `ispell_dictionary_keyword'.
        call goto_char(point_max())

        if (.not. ispell_local_dictionary_overridden%value) then
            call search_backward(ispell_dictionary_keyword, found, st, noerror=SEARCH_NOERROR)

            if (st%ok() .and. found > 0) then
                call search_forward(ispell_dictionary_keyword, found, st)
                end = line_end_position()
                call re_search_forward(" *\([^ ""]+\)", found, st, end, SEARCH_NOERROR)
                if (st%ok() .and. found > 0) call ispell_local_dictionary%set_local(match_string_no_properties(1))
            end if
        end if

        call goto_char(point_max())
        call search_backward(ispell_pdict_keyword, found, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            call search_forward(ispell_pdict_keyword, found, st)
            end = line_end_position()
            call re_search_forward(" *\([^ ""]+\)", found, st, end, SEARCH_NOERROR)
            if (st%ok() .and. found > 0) call ispell_local_pdict%set_local(match_string_no_properties(1))
        end if

        call goto_char(saved)

        if (present(no_reload)) then
            if (no_reload) return
        end if

        ! Reload if new dictionary (maybe the personal one) defined.
        call ispell_internal_change_dictionary()
    end subroutine ispell_buffer_local_dict

    !> Load the buffer-local dictionary in the current buffer.  Port of
    !> ispell.el:ispell-buffer-local-words.
    recursive subroutine ispell_buffer_local_words(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: casechars, string
        integer(int64) :: saved, found, end
        type(status_type) :: st
        integer :: k, j

        ! If there's an existing ispell process that's wrong for this use,
        ! kill it.
        if (len(ispell_buffer_local_name) > 0 .and. ispell_buffer_local_name /= buffers(current)%name) then
            call ispell_kill_ispell(st, no_error=.true.)
        end if

        ! Actually start a new ispell process, because we need to send
        ! commands now to specify the local words to it.
        call ispell_init_process(status)
        if (.not. status%ok()) return
        k = session_slot()

        if (k > 0) then
            do j = 1, size(session_localwords(k)%words)
                call ispell_send_string("@"//session_localwords(k)%words(j)%text//LF, st)
            end do

            if (len(ispell_buffer_local_name) == 0 .and. size(session_localwords(k)%words) > 0) then
                ispell_buffer_local_name = buffers(current)%name
            end if
        end if

        saved = point()
        call goto_char(point_min())

        do
            call search_forward(ispell_words_keyword, found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found == 0) exit
            if (len(ispell_buffer_local_name) == 0) ispell_buffer_local_name = buffers(current)%name
            end = line_end_position()
            casechars = ispell_get_casechars()

            ! Buffer-local words separated by a space, and can contain any
            ! character other than a space.  Not rigorous enough.
            do
                call re_search_forward(" *\([^ ]+\)", found, st, end, SEARCH_NOERROR)
                if (.not. st%ok() .or. found == 0) exit
                string = match_string_no_properties(1)

                ! This can fail when string contains a word with invalid
                ! chars.  Error handling needs to be added between ispell
                ! and Fortmacs.
                if (len_chars(string) > 1) then
                    if (string_match(casechars, string, st) == 1) call ispell_send_string("@"//string//LF, st)
                end if
            end do
        end do

        call goto_char(saved)
    end subroutine ispell_buffer_local_words

    ! Return the comment marker for the current mode.  Port of
    ! ispell.el:ispell--comment-prefix.
    function ispell_comment_prefix() result(prefix)
        character(len=:), allocatable :: prefix
        type(status_type) :: st

        call comment_normalize_vars(st)
        ! (Upstream also calls comment-padright, whose result it drops.)
        prefix = comment_start%value
    end function ispell_comment_prefix

    !> Add WORD to the per-file word list.  Port of
    !> ispell.el:ispell-add-per-file-word-list.
    recursive subroutine ispell_add_per_file_word_list(word, status)
        character(len=*), intent(in) :: word
        type(status_type), intent(out) :: status
        integer(int64) :: saved, found
        logical :: line_okay, done, has_found, searched, empty_comment_end, saved_fold
        type(status_type) :: st

        status = status_type()
        if (len(ispell_buffer_local_name) == 0) ispell_buffer_local_name = buffers(current)%name
        saved = point()
        call goto_char(point_min())
        done = .false.
        has_found = .false.

        do while (.not. done)
            saved_fold = case_fold_search%value
            case_fold_search = .false.
            call search_forward(ispell_words_keyword, found, st, noerror=SEARCH_NOERROR)
            case_fold_search = saved_fold
            searched = st%ok() .and. found > 0
            has_found = has_found .or. searched
            call end_of_line()
            ! 1 for space after word.
            line_okay = len_chars(word) + 1 + current_column() < fill_column%value

            if ((searched .and. line_okay) .or. .not. searched) then
                done = .true.

                if (.not. searched) then
                    empty_comment_end = string_value(comment_end) == ""

                    if (has_found) then
                        ! After an existing LocalWords.
                        call insert(LF, st)

                        if (empty_comment_end .and. allocated(comment_start%value)) then
                            call insert(ispell_comment_prefix()//" ", st)
                        end if
                    else
                        ! No LocalWords, go to end of file.
                        call goto_char(point_max())
                        call open_line(1_int64, st)
                        call newline(st)

                        ! Insert an end marker if needed, preceded by a
                        ! newline.
                        if (.not. empty_comment_end) then
                            found = point()
                            call newline(st)
                            call insert(comment_end%value, st)
                            call goto_char(found)
                        end if

                        if (allocated(comment_start%value)) then
                            if (empty_comment_end) then
                                call insert(ispell_comment_prefix()//" ", st)
                            else
                                call insert(ispell_comment_prefix()//LF, st)
                            end if
                        end if
                    end if

                    call insert(ispell_words_keyword, st)
                end if

                call insert(" "//word, st)
            end if
        end do

        call goto_char(saved)
        if (saved > point_max()) call goto_char(point_max())
    end subroutine ispell_add_per_file_word_list

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Register the commands, bind M-$ and give the settings their defaults.
    !> Not a port.
    subroutine init_ispell()
        character(len=:), allocatable :: path

        commands%ispell_word%execute => cmd_ispell_word
        commands%ispell_region%execute => cmd_ispell_region
        commands%ispell_buffer%execute => ispell_buffer
        commands%ispell%execute => ispell
        commands%ispell_continue%execute => ispell_continue
        commands%ispell_comments_and_strings%execute => cmd_ispell_comments_and_strings
        commands%ispell_comment_or_string_at_point%execute => ispell_comment_or_string_at_point
        commands%ispell_change_dictionary%execute => cmd_ispell_change_dictionary
        commands%ispell_kill_ispell%execute => cmd_ispell_kill_ispell
        commands%ispell_pdict_save%execute => cmd_ispell_pdict_save
        commands%ispell_check_version%execute => cmd_ispell_check_version
        call forward_setting(ispell_local_dictionary, local_if_set=.true.)
        call forward_setting(ispell_local_dictionary_overridden, local_if_set=.true.)
        call forward_setting(ispell_skip_html, local_if_set=.true.)
        call forward_setting(ispell_local_pdict, local_if_set=.true.)
        call forward_setting(ispell_parser)

        if (.not. allocated(ispell_program_name)) then
            ispell_program_name = "ispell"
            call executable_find("aspell", path)
            if (len(path) == 0) call executable_find("ispell", path)
            if (len(path) == 0) call executable_find("hunspell", path)
            ! Enchant is commonly installed as `enchant-2', so use this
            ! name and avoid old versions of `enchant'.
            if (len(path) == 0) call executable_find("enchant-2", path)
            if (len(path) > 0) ispell_program_name = path
        end if

        if (.not. allocated(ispell_alternate_dictionary)) call init_alternate_dictionary()
        if (.not. allocated(ispell_complete_word_dict)) ispell_complete_word_dict = ""
        if (.not. allocated(ispell_grep_command)) ispell_grep_command = "grep"
        if (.not. allocated(ispell_grep_options)) ispell_grep_options = "-Ei"

        if (.not. allocated(ispell_look_command)) then
            call executable_find("look", path)
            ispell_look_command = path
        end if

        if (.not. allocated(ispell_look_options)) ispell_look_options = "-df"
        if (.not. allocated(ispell_personal_dictionary)) ispell_personal_dictionary = ""
        if (.not. allocated(ispell_dictionary)) ispell_dictionary = ""
        if (.not. allocated(ispell_extra_args)) allocate (ispell_extra_args(0))
        if (.not. allocated(ispell_tib_ref_beginning)) ispell_tib_ref_beginning = "[[<]\."
        if (.not. allocated(ispell_tib_ref_end)) ispell_tib_ref_end = "\.[]>]"
        if (.not. allocated(ispell_local_dictionaries)) allocate (ispell_local_dictionaries(0))
        if (.not. allocated(ispell_dictionaries)) allocate (ispell_dictionaries(0))
        if (.not. allocated(ispell_aspell_dictionaries)) allocate (ispell_aspell_dictionaries(0))
        if (.not. allocated(ispell_aspell_found_dictionaries)) allocate (ispell_aspell_found_dictionaries(0))
        if (.not. allocated(ispell_really_aspell)) ispell_really_aspell = ""
        if (.not. allocated(ispell_really_hunspell)) ispell_really_hunspell = ""
        if (.not. allocated(ispell_really_enchant)) ispell_really_enchant = ""
        if (.not. allocated(ispell_encoding8_command)) ispell_encoding8_command = ""
        if (.not. allocated(ispell_library_directory)) ispell_library_directory = ""
        if (.not. allocated(ispell_aspell_data_dir)) ispell_aspell_data_dir = ""
        if (.not. allocated(ispell_aspell_dict_dir)) ispell_aspell_dict_dir = ""
        if (.not. allocated(ispell_current_dictionary)) ispell_current_dictionary = ""
        if (.not. allocated(ispell_current_personal_dictionary)) ispell_current_personal_dictionary = ""
        if (.not. allocated(ispell_buffer_local_name)) ispell_buffer_local_name = ""
        if (.not. allocated(ispell_process_directory)) ispell_process_directory = ""
        if (.not. allocated(ispell_filter_variable)) allocate (ispell_filter_variable(0))
        if (.not. allocated(ispell_base_dictionaries)) call init_base_dictionaries()
        if (.not. allocated(ispell_dicts_name2locale_equivs)) call init_locale_equivs()
        if (.not. allocated(ispell_skip_regions)) call init_skip_regions()
    end subroutine init_ispell

    ! `ispell_alternate_dictionary's default: the first readable word list.
    ! Not a port.
    subroutine init_alternate_dictionary()
        character(len=*), parameter :: files(7) = [character(len=25) :: "/usr/dict/web2", "/usr/share/dict/web2", &
                                                                "/usr/dict/words", "/usr/lib/dict/words", "/usr/share/dict/words", &
                                                                                           "/usr/share/lib/dict/words", "/sys/dict"]
        integer :: k

        ispell_alternate_dictionary = ""

        do k = 1, size(files)
            if (is_file_readable(trim(files(k)))) then
                ispell_alternate_dictionary = trim(files(k))
                return
            end if
        end do
    end subroutine init_alternate_dictionary

    ! `ispell_dicts_name2locale_equivs's default.  Not a port.
    subroutine init_locale_equivs()
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

        allocate (ispell_dicts_name2locale_equivs(size(names)))

        do k = 1, size(names)
            ispell_dicts_name2locale_equivs(k)%name = names(k)
            ispell_dicts_name2locale_equivs(k)%locale = locales(k)
        end do
    end subroutine init_locale_equivs

    ! The regions to skip: `ispell_skip_regions', the TeX and HTML ones.
    ! Not a port.
    subroutine init_skip_regions()
        allocate (ispell_skip_regions(8))
        ispell_skip_regions(1)%beginning_regexp = keyword_regexp(ispell_words_keyword)
        ispell_skip_regions(1)%skip => skip_line
        ispell_skip_regions(2)%beginning_regexp = keyword_regexp(ispell_dictionary_keyword)
        ispell_skip_regions(2)%skip => skip_line
        ispell_skip_regions(3)%beginning_regexp = keyword_regexp(ispell_pdict_keyword)
        ispell_skip_regions(3)%skip => skip_line
        ispell_skip_regions(4)%beginning_regexp = keyword_regexp(ispell_parsing_keyword)
        ispell_skip_regions(4)%skip => skip_line
        ispell_skip_regions(5)%beginning_regexp = "^---*BEGIN PGP [A-Z ]*--*"
        ispell_skip_regions(5)%end_regexp = "^---*END PGP [A-Z ]*--*"
        ! Assume multiline uuencoded file? "\nM.*$"?
        ispell_skip_regions(6)%beginning_regexp = "^begin [0-9][0-9][0-9] [^ "//TAB//"]+$"
        ispell_skip_regions(6)%end_regexp = LF//"end"//LF
        ispell_skip_regions(7)%beginning_regexp = "^%!PS-Adobe-[123].0"
        ispell_skip_regions(7)%end_regexp = LF//"%%EOF"//LF
        ispell_skip_regions(8)%beginning_regexp = "^---* \(Start of \)?[Ff]orwarded [Mm]essage"
        ispell_skip_regions(8)%end_regexp = "^---* End of [Ff]orwarded [Mm]essage"
        ! All the standard LaTeX keywords from L. Lamport's guide: \cite,
        ! \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
        ! \label, \nocite, \rule (in ispell - rest included here).
        allocate (ispell_tex_skip_regions(8))
        call set_tex(1, "\\addcontentsline", 2)
        call set_tex(2, "\\add\(tocontents\|vspace\)", 1)
        call set_tex(3, "\\\([aA]lph\|arabic\)", 1)
        call set_tex(4, "\\cref", 1)
        call set_tex(5, "\\bibliographystyle", 1)
        call set_tex(6, "\\makebox", 0)
        call set_tex(7, "\\e?psfig", 1)
        ispell_tex_skip_regions(8)%beginning_regexp = "\\document\(class\|style\)"
        ispell_tex_skip_regions(8)%end_regexp = "\\begin[ "//TAB//LF//"]*{document}"
        ! Delimited with \begin.  In ispell: displaymath, eqnarray,
        ! eqnarray*, equation, minipage, picture, tabular, tabular* (ispell).
        allocate (ispell_tex_skip_environments(4))
        ispell_tex_skip_environments(1)%beginning_regexp = "\(figure\|table\)\*?"
        ispell_tex_skip_environments(1)%skip => ispell_tex_arg_end
        ispell_tex_skip_environments(1)%arg = 0
        ispell_tex_skip_environments(2)%beginning_regexp = "list"
        ispell_tex_skip_environments(2)%skip => ispell_tex_arg_end
        ispell_tex_skip_environments(2)%arg = 2
        ispell_tex_skip_environments(3)%beginning_regexp = "program"
        ispell_tex_skip_environments(3)%end_regexp = "\\end[ "//TAB//"]*{program}"
        ispell_tex_skip_environments(4)%beginning_regexp = "verbatim\*?"
        ispell_tex_skip_environments(4)%end_regexp = "\\end[ "//TAB//"]*{verbatim\*?}"
        allocate (ispell_html_skip_regions(7))
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
        function keyword_regexp(keyword) result(regexp)
            character(len=*), intent(in) :: keyword
            character(len=:), allocatable :: regexp

            regexp = keyword(1:len(keyword) - 1)//"[ ]"
        end function keyword_regexp

        ! A TeX region skipped by `ispell_tex_arg_end' with ARG.
        subroutine set_tex(k, key, arg)
            integer, intent(in) :: k, arg
            character(len=*), intent(in) :: key

            ispell_tex_skip_regions(k)%beginning_regexp = key
            ispell_tex_skip_regions(k)%skip => ispell_tex_arg_end
            ispell_tex_skip_regions(k)%arg = arg
        end subroutine set_tex

        ! An HTML region from KEY to END.
        subroutine set_html(k, key, end)
            integer, intent(in) :: k
            character(len=*), intent(in) :: key, end

            ispell_html_skip_regions(k)%beginning_regexp = key
            ispell_html_skip_regions(k)%end_regexp = end
        end subroutine set_html
    end subroutine init_skip_regions

    ! `ispell_base_dictionaries's default.  The character sets are in the
    ! dictionaries' coding systems, as upstream's unibyte strings.
    ! Not a port.
    subroutine init_base_dictionaries()
        type(ispell_dictionary_type) :: d

        allocate (ispell_base_dictionaries(32))
        d = ispell_dictionary_type(name="", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        ispell_base_dictionaries(1) = d
        d = ispell_dictionary_type(name="american", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        ispell_base_dictionaries(2) = d
        d = ispell_dictionary_type(name="brasileiro", many_otherchars=.false., extended_character_mode="", &
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
        ispell_base_dictionaries(3) = d
        d = ispell_dictionary_type(name="british", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        ispell_base_dictionaries(4) = d
        d = ispell_dictionary_type(name="castellano", many_otherchars=.false., extended_character_mode="~tex", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%casechars = "[A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                      char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                      char(250)//char(252)//"]"
        d%not_casechars = "[^A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                          char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                          char(250)//char(252)//"]"
        d%otherchars = "[-]"
        ispell_base_dictionaries(5) = d
        d = ispell_dictionary_type(name="castellano8", many_otherchars=.false., extended_character_mode="~latin1", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%args(2) = "-d"
        d%args(3) = "castellano"
        d%casechars = "[A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                      char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                      char(250)//char(252)//"]"
        d%not_casechars = "[^A-Z"//char(193)//char(201)//char(205)//char(209)//char(211)//char(218)// &
                          char(220)//"a-z"//char(225)//char(233)//char(237)//char(241)//char(243)// &
                          char(250)//char(252)//"]"
        d%otherchars = "[-]"
        ispell_base_dictionaries(6) = d
        d = ispell_dictionary_type(name="czech", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-2")
        d%args(1) = "-B"
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
        ispell_base_dictionaries(7) = d
        d = ispell_dictionary_type(name="dansk", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%casechars = "[A-Z"//char(198)//char(216)//char(197)//"a-z"//char(230)//char(248)// &
                      char(229)//"]"
        d%not_casechars = "[^A-Z"//char(198)//char(216)//char(197)//"a-z"//char(230)//char(248)// &
                          char(229)//"]"
        d%otherchars = "[']"
        ispell_base_dictionaries(8) = d
        d = ispell_dictionary_type(name="deutsch", many_otherchars=.true., extended_character_mode="~tex", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%casechars = "[a-zA-Z""]"
        d%not_casechars = "[^a-zA-Z""]"
        d%otherchars = "[']"
        ispell_base_dictionaries(9) = d
        d = ispell_dictionary_type(name="deutsch8", many_otherchars=.true., extended_character_mode="~latin1", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%args(2) = "-d"
        d%args(3) = "deutsch"
        d%casechars = "[a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)//char(223)// &
                      char(252)//"]"
        d%not_casechars = "[^a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)// &
                          char(223)//char(252)//"]"
        d%otherchars = "[']"
        ispell_base_dictionaries(10) = d
        d = ispell_dictionary_type(name="english", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[']"
        ispell_base_dictionaries(11) = d
        d = ispell_dictionary_type(name="esperanto", many_otherchars=.true., extended_character_mode="~latin3", &
                                   coding_system="iso-8859-3")
        d%args(1) = "-C"
        d%casechars = "[A-Za-z"//char(166)//char(172)//char(182)//char(188)//char(198)//char(216)// &
                      char(221)//char(222)//char(230)//char(248)//char(253)//char(254)//"]"
        d%not_casechars = "[^A-Za-z"//char(166)//char(172)//char(182)//char(188)//char(198)// &
                          char(216)//char(221)//char(222)//char(230)//char(248)//char(253)// &
                          char(254)//"]"
        d%otherchars = "[-']"
        ispell_base_dictionaries(12) = d
        d = ispell_dictionary_type(name="esperanto-tex", many_otherchars=.true., extended_character_mode="~tex", &
                                   coding_system="iso-8859-3")
        d%args(1) = "-C"
        d%args(2) = "-d"
        d%args(3) = "esperanto"
        d%casechars = "[A-Za-z^\]"
        d%not_casechars = "[^A-Za-z^\]"
        d%otherchars = "[-'`""]"
        ispell_base_dictionaries(13) = d
        d = ispell_dictionary_type(name="finnish", many_otherchars=.false., extended_character_mode="~list", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%casechars = "[A-Za-z"//char(229)//char(228)//char(246)//char(197)//char(196)//char(214)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(229)//char(228)//char(246)//char(197)//char(196)// &
                          char(214)//"]"
        d%otherchars = "[:]"
        ispell_base_dictionaries(14) = d
        d = ispell_dictionary_type(name="francais7", many_otherchars=.true., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%casechars = "[A-Za-z]"
        d%not_casechars = "[^A-Za-z]"
        d%otherchars = "[`'^-]"
        ispell_base_dictionaries(15) = d
        d = ispell_dictionary_type(name="francais", many_otherchars=.true., extended_character_mode="~list", &
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
        ispell_base_dictionaries(16) = d
        d = ispell_dictionary_type(name="francais-tex", many_otherchars=.true., extended_character_mode="~tex", &
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
        ispell_base_dictionaries(17) = d
        d = ispell_dictionary_type(name="german", many_otherchars=.true., extended_character_mode="~tex", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%casechars = "[a-zA-Z""]"
        d%not_casechars = "[^a-zA-Z""]"
        d%otherchars = "[']"
        ispell_base_dictionaries(18) = d
        d = ispell_dictionary_type(name="german8", many_otherchars=.true., extended_character_mode="~latin1", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%args(2) = "-d"
        d%args(3) = "german"
        d%casechars = "[a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)//char(223)// &
                      char(252)//"]"
        d%not_casechars = "[^a-zA-Z"//char(196)//char(214)//char(220)//char(228)//char(246)// &
                          char(223)//char(252)//"]"
        d%otherchars = "[']"
        ispell_base_dictionaries(19) = d
        d = ispell_dictionary_type(name="italiano", many_otherchars=.false., extended_character_mode="~tex", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-B"
        d%args(2) = "-d"
        d%args(3) = "italian"
        d%casechars = "[A-Z"//char(192)//char(193)//char(200)//char(201)//char(204)//char(205)// &
                      char(210)//char(211)//char(217)//char(218)//"a-z"//char(224)//char(225)// &
                      char(232)//char(233)//char(236)//char(237)//char(243)//char(249)//char(250)// &
                      "]"
        d%not_casechars = "[^A-Z"//char(192)//char(193)//char(200)//char(201)//char(204)//char(205)// &
                          char(210)//char(211)//char(217)//char(218)//"a-z"//char(224)//char(225)// &
                          char(232)//char(233)//char(236)//char(237)//char(243)//char(249)// &
                          char(250)//"]"
        d%otherchars = "[-.]"
        ispell_base_dictionaries(20) = d
        d = ispell_dictionary_type(name="nederlands", many_otherchars=.true., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
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
        ispell_base_dictionaries(21) = d
        d = ispell_dictionary_type(name="nederlands8", many_otherchars=.true., extended_character_mode="", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
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
        ispell_base_dictionaries(22) = d
        d = ispell_dictionary_type(name="norsk", many_otherchars=.false., extended_character_mode="~list", &
                                   coding_system="iso-8859-1")
        d%casechars = "[A-Za-z"//char(197)//char(198)//char(199)//char(200)//char(201)//char(210)// &
                      char(212)//char(216)//char(229)//char(230)//char(231)//char(232)//char(233)// &
                      char(242)//char(244)//char(248)//"]"
        d%not_casechars = "[^A-Za-z"//char(197)//char(198)//char(199)//char(200)//char(201)// &
                          char(210)//char(212)//char(216)//char(229)//char(230)//char(231)// &
                          char(232)//char(233)//char(242)//char(244)//char(248)//"]"
        d%otherchars = "[""]"
        ispell_base_dictionaries(23) = d
        d = ispell_dictionary_type(name="norsk7-tex", many_otherchars=.false., extended_character_mode="~plaintex", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-d"
        d%args(2) = "norsk"
        d%casechars = "[A-Za-z{}\'^`]"
        d%not_casechars = "[^A-Za-z{}\'^`]"
        d%otherchars = "[""]"
        ispell_base_dictionaries(24) = d
        d = ispell_dictionary_type(name="polish", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-2")
        d%casechars = "[A-Za-z"//char(161)//char(163)//char(166)//char(172)//char(175)//char(177)// &
                      char(179)//char(182)//char(188)//char(191)//char(198)//char(202)//char(209)// &
                      char(211)//char(230)//char(234)//char(241)//char(243)//"]"
        d%not_casechars = "[^A-Za-z"//char(161)//char(163)//char(166)//char(172)//char(175)// &
                          char(177)//char(179)//char(182)//char(188)//char(191)//char(198)// &
                          char(202)//char(209)//char(211)//char(230)//char(234)//char(241)// &
                          char(243)//"]"
        d%otherchars = "[.]"
        ispell_base_dictionaries(25) = d
        d = ispell_dictionary_type(name="portugues", many_otherchars=.true., extended_character_mode="~latin1", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%casechars = "[a-zA-Z"//char(193)//char(194)//char(199)//char(201)//char(211)//char(224)// &
                      char(225)//char(226)//char(233)//char(234)//char(237)//char(243)//char(227)// &
                      char(231)//char(250)//"]"
        d%not_casechars = "[^a-zA-Z"//char(193)//char(194)//char(199)//char(201)//char(211)// &
                          char(224)//char(225)//char(226)//char(233)//char(234)//char(237)// &
                          char(243)//char(227)//char(231)//char(250)//"]"
        d%otherchars = "[']"
        ispell_base_dictionaries(26) = d
        d = ispell_dictionary_type(name="russian", many_otherchars=.false., extended_character_mode="", &
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
        ispell_base_dictionaries(27) = d
        d = ispell_dictionary_type(name="russianw", many_otherchars=.false., extended_character_mode="", &
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
        ispell_base_dictionaries(28) = d
        d = ispell_dictionary_type(name="slovak", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-2")
        d%args(1) = "-B"
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
        ispell_base_dictionaries(29) = d
        d = ispell_dictionary_type(name="slovenian", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="iso-8859-2")
        d%args(1) = "-B"
        d%args(2) = "-d"
        d%args(3) = "slovenian"
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
        ispell_base_dictionaries(30) = d
        d = ispell_dictionary_type(name="svenska", many_otherchars=.false., extended_character_mode="~list", &
                                   coding_system="iso-8859-1")
        d%args(1) = "-C"
        d%casechars = "[A-Za-z"//char(229)//char(228)//char(246)//char(233)//char(224)//char(252)// &
                      char(232)//char(230)//char(248)//char(231)//char(197)//char(196)//char(214)// &
                      char(201)//char(192)//char(220)//char(200)//char(198)//char(216)//char(199)// &
                      "]"
        d%not_casechars = "[^A-Za-z"//char(229)//char(228)//char(246)//char(233)//char(224)// &
                          char(252)//char(232)//char(230)//char(248)//char(231)//char(197)// &
                          char(196)//char(214)//char(201)//char(192)//char(220)//char(200)// &
                          char(198)//char(216)//char(199)//"]"
        d%otherchars = "[']"
        ispell_base_dictionaries(31) = d
        d = ispell_dictionary_type(name="hebrew", many_otherchars=.false., extended_character_mode="", &
                                   coding_system="cp1255")
        d%args(1) = "-B"
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
        ispell_base_dictionaries(32) = d
    end subroutine init_base_dictionaries
end module ispell_mod
