! shell_script_mod.f90 -- shell-script editing commands.
! Ported from GNU Emacs 31.1 lisp/progmodes/sh-script.el.
!
! Copyright (C) 1993-1997, 1999, 2001-2026 Free Software Foundation, Inc.
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

!> Major mode for editing shell scripts.  Bourne, C and rc shells as well
!> as various derivatives are supported and easily derived from.
!>
!> The shell-dependent settings are feature tables (`shell_feature'): one
!> entry per shell, an entry may append the value of another shell's
!> (sh-append), and a shell without an entry uses its ancestor's
!> (`shell_ancestors'), finally sh's.  The indentation variables are choice
!> settings: a number of columns in `%columns' (`shell_indent_for_fi%columns
!> = 0') or the named constants SHELL_PLUS (+), SHELL_MINUS (-), SHELL_PLUS_PLUS
!> (++), SHELL_MINUS_MINUS (--), SHELL_STAR (*) and SHELL_SLASH (/)
!> (`shell_indent_after_if = SHELL_PLUS'), which `shell_variable_value' interprets.
!>
!> The inferior shell commands (C-c C-z, C-c C-d, C-c C-n) find or start
!> the shell with `shell_shell_process'; completion at point offers the
!> variables assigned before point, commands and keywords
!> (`shell_completion_at_point_function'), then
!> `shell_script_dynamic_complete_functions' through comint.  `shell_styles' is
!> an array of `shell_style_type', saved as a Fortran assignment.
!>
!> The statement skeletons (`shell_case', `shell_for', `shell_function', `shell_if',
!> `shell_indexed_loop', `shell_repeat', `shell_select', `shell_temporary_file', `shell_until',
!> `shell_while', `shell_while_getopts') are `skeleton_variants_type' tables, one
!> skeleton per shell, chosen by `shell_skeleton_feature'.
!>
!> Deferred: `executable_interpret' (C-c C-x; needs `compilation_start' with a
!> buffer name function and rule specifications), `outline_regular_expression', `escaped_string_quote' (with
!> `paste_in_context'), the Flymake backend (`shell_shellcheck_flymake'), the
!> `shell_shell' file-local variable (`hack_local_variables_hook'), the menu
!> and `bash_ts_mode' (tree-sitter).
module shell_script_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, ARITHMETIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, xstring, list, list_elements, make_list, list_length, list_element, vtag, V_STRING, &
                         V_INTEGER, V_NAME, xfixnum, symbols, xsymbol, choice_type
    use data_mod, only: integer_setting_type, string_setting_type, forward_setting, append_locals_clearer, choice_setting_type, &
                        setting_type
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use keyboard_mod, only: this_command
    use buffer_mod, only: buffers, current, buffer_serial_live, barf_if_buffer_read_only, current_buffer, set_buffer, &
                          get_or_make_buffer
    use hooks_mod, only: hook_type
    use modes_mod, only: major_modes, minor_modes
    use add_log_mod, only: add_log_current_subprogram_function
    use keymap_mod, only: keymap_set, define_key
    use syntax_mod, only: copy_syntax_table, make_syntax_table, modify_syntax_entry, is_syntax_table_live, destroy_syntax_table, &
                          set_syntax_table, syntax_descriptor_type, string_to_syntax, parse_state_type, syntax_parser_state, &
                          forward_comment, forward_word, skip_characters_forward, skip_characters_backward, skip_syntax_forward, &
                          skip_syntax_backward, &
                          syntax_descriptor_value, syntax_entry_at, ST_STRING_STYLE
    use editing_functions_mod, only: interactive_region, point, point_minimum, point_maximum, goto_character, &
                                     is_beginning_of_line, is_end_of_line, is_beginning_of_buffer, is_end_of_buffer, &
                                     character_after, character_before, delete_region, buffer_substring, line_beginning_position, &
                                     line_end_position, preceding_character
    use editing_commands_mod, only: forward_line, forward_character, beginning_of_line, end_of_line, delete_character, &
                                    self_insert_command, post_self_insert_hook
    use insert_delete_mod, only: insert
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use indent_mod, only: current_column, current_indentation, indent_to, indent_line_function
    use search_mod, only: looking_at, regular_expression_search_forward, regular_expression_search_backward, search_forward, &
                          search_backward, replace_match, match_beginning, match_end, regular_expression_quote, string_match, &
                          case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string, looking_back
    use simple_mod, only: delete_horizontal_space, back_to_indentation, count_lines, shell_command_on_region, is_use_region
    use text_properties_mod, only: get_text_property, put_text_property
    use display_engine_mod, only: message, set_mode_line_process, mode_line_part_type, tab_width
    use window_mod, only: window_text_columns
    use frame_mod, only: frame_selected_window_reference
    use files_mod, only: auto_mode_interpreter_regular_expression
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, font_lock_keywords_type, &
                             font_lock_syntax_entry_type, FONT_LOCK_OVERRIDE_REPLACE
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_flush, font_lock_set_defaults, &
                             font_lock_forget_defaults
    use regular_expression_optimization_mod, only: regular_expression_optimization, REGULAR_EXPRESSION_OPTIMIZATION_GROUP
    use expression_motion_mod, only: forward_expression_default_function
    use newcomment_mod, only: comment_start, comment_start_skip
    use paragraphs_mod, only: paragraph_start, paragraph_separate, page_delimiter
    use command_interpreter_mod, only: command_interpreter_prompt_regular_expression, &
                                       command_interpreter_dynamic_complete_functions, command_interpreter_filename_completion, &
                                       command_interpreter_completion_at_point, command_interpreter_send_string
    use shell_mod, only: shell_command_completion, shell_environment_variable_completion, shell, explicit_shell_file_name
    use process_mod, only: is_process_live, process_list, process_command, process_buffer, get_buffer_process
    use window_mod, only: pop_to_buffer, display_buffer_overriding_action, display_action_type, DISPLAY_ACTION_SAME_WINDOW
    use hooks_mod, only: completion_at_point_function_hook_type, completion_at_point_function_entry_type
    use minibuffer_reading_tables_mod, only: completion_at_point_function_data_type, TABLE_FUNCTION, COMPLETE_ALL, &
                                             COMPLETE_BOUNDARIES, completion_test_procedure
    use minibuffer_reading_mod, only: complete_with_action, read_from_minibuffer
    use minibuffer_mod, only: completion_at_point_functions, REQUIRE_MATCH_YES
    use files_mod, only: locate_file_completion_table
    use expression_motion_mod, only: subprogram_prompt_regular_expression
    use basic_subprograms_mod, only: read_y_or_n
    use abbreviation_mod, only: abbreviation_table_type, local_abbreviation_table
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use file_input_output_mod, only: file_name_nondirectory
    use environment_mod, only: getenv
    use execute_process_mod, only: shell_file_name
    use minibuffer_reading_mod, only: completion_table_type
    use minibuffer_mod, only: completing_read
    use replace_mod, only: format_prompt
    use electric_mod, only: electric_indent_mode
    use executable_mod, only: executable_set_magic, executable_font_lock_keywords, executable_query, &
                              EXECUTABLE_QUERY_FUNCTION
    use syntax_propertize_mod, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type, &
                                     syntax_property_type, syntax_propertize_extend_region_functions, &
                                     syntax_propertize_multiline
    use simple_minded_indentation_engine_mod, only: simple_minded_indentation_engine_backus_naur_form_type, &
                                                    simple_minded_indentation_engine_precs_type, &
                                                    simple_minded_indentation_engine_prec2_type, &
                                                    simple_minded_indentation_engine_grammar_type, &
                                                    simple_minded_indentation_engine_rule_result_type, &
                                                    simple_minded_indentation_engine_indent_type, &
                                                    simple_minded_indentation_engine_expression_type, &
                                                    simple_minded_indentation_engine_toklevels_type, &
                                                    simple_minded_indentation_engine_backus_naur_form_to_prec2, &
                                                    simple_minded_indentation_engine_prec2_to_grammar, &
                                                    simple_minded_indentation_engine_setup, &
                                                    simple_minded_indentation_engine_forward_token, &
                                                    simple_minded_indentation_engine_backward_token, &
                                                    simple_minded_indentation_engine_grammar_token, &
                                                    simple_minded_indentation_engine_backward_expression, &
                                                    is_simple_minded_indentation_engine_rule_parent, &
                                                    is_simple_minded_indentation_engine_rule_previous, &
                                                    is_simple_minded_indentation_engine_rule_hanging, &
                                                    is_simple_minded_indentation_engine_rule_beginning_of_line, &
                                                    simple_minded_indentation_engine_rule_parent, &
                                                    simple_minded_indentation_engine_rule_separator, &
                                                    simple_minded_indentation_engine_indent_virtual, &
                                                    simple_minded_indentation_engine_indent_calculate, &
                                                    simple_minded_indentation_engine_indent_keyword, &
                                                    simple_minded_indentation_engine_indent_functions, &
                                                    simple_minded_indentation_engine_hanging_line_test_function, &
                                                    is_simple_minded_indentation_engine_default_hanging_end_of_line, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_AFTER, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                    SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE
    use system_dependent_mod, only: fortmacs_environment
    use skeleton_mod, only: skeleton_element_type, skeleton_variants_type, skeleton_pairs_type, skeleton_state, skeleton_text, &
                            skeleton_literal, skeleton_delete, skeleton_prompt, skeleton_interactor, skeleton_action, &
                            skeleton_value, skeleton_variants_command, skeleton_next_element, skeleton_read_string, &
                            skeleton_default_pairs, skeleton_pairs, skeleton_pair_filter_function, &
                            skeleton_filter_function, skeleton_newline_indent_rigidly, SKELETON_NEWLINE, &
                            SKELETON_INDENT, SKELETON_POINT, SKELETON_STRING, SKELETON_END, SKELETON_RESUME, SKELETON_V1, &
                            SKELETON_V2, SKELETON_IF_MOVED, SKELETON_IF_NOT_MOVED, SKELETON_NO_INTERACTOR
    use execute_interactively_mod, only: prefix_argument_type
    use minibuffer_reading_mod, only: read_string
    use error_mod, only: WRONG_TYPE_ARGUMENT_CODE
    use index_menu_mod, only: index_menu_patterns, index_menu_pattern_type, index_menu_case_fold_search, index_menu_index_type, &
                              make_index_menu_index
    use string_type_mod, only: string_type, prepend_string
    implicit none(type, external)
    private

    public :: initialize_shell_script, shell_script_mode, shell_base_mode, shell_set_shell, shell_canonicalize_shell, &
              shell_feature_shell, &
              shell_variable_value, shell_basic_indent_line, shell_execute_region, shell_remember_variable, is_shell_quoted, &
              is_shell_backslash_quoted, &
              shell_in_comment_or_string, execute_shell_make_variables_local, shell_reset_indent_variables_to_global_values, &
              shell_add, &
              shell_shell_initialize_variables, execute_shell_beginning_of_command, execute_shell_end_of_command, &
              shell_backslash_region, &
              shell_current_function_name, shell_mkword_regexpr, shell_indent_supported_here, shell_shell_variables, &
              shell_syntax_propertize_function, shell_font_lock_syntactic_face_function, shell_script_font_lock_keywords, &
              shell_script_font_lock_keywords_1, shell_script_font_lock_keywords_2, &
              shell_indentation_engine_default_forward_token, &
              shell_indentation_engine_default_backward_token, shell_indentation_engine_shell_forward_token, &
              shell_indentation_engine_shell_backward_token, shell_indentation_engine_shell_rules, &
              shell_indentation_engine_rc_forward_token, shell_indentation_engine_rc_backward_token, &
              shell_indentation_engine_rc_rules, shell_indentation_engine_indent_continuation, &
              shell_electric_here_document_mode, shell_assignment, shell_shell_process, shell_show_shell, shell_send_text, &
              shell_cd_here, &
              shell_send_line_or_region_and_step, shell_variables_before_point, shell_completion_at_point_function, &
              shell_name_style, &
              shell_load_style, shell_save_styles_to_buffer, shell_case, shell_for, shell_indexed_loop, shell_function, shell_if, &
              shell_repeat, shell_select, &
              shell_temporary_file, shell_until, shell_while, shell_while_getopts, shell_modify, shell_skeleton_feature

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> Values of `shell_indent_for_fi', `shell_indent_for_done',
    !> `shell_indent_after_else', `shell_indent_after_if', `shell_indent_for_then',
    !> `shell_indent_for_do', `shell_indent_after_do',
    !> `shell_indent_after_loop_construct', `shell_indent_after_done',
    !> `shell_indent_for_case_label', `shell_indent_for_case_alt',
    !> `shell_indent_for_continuation', `shell_indent_after_open',
    !> `shell_indent_after_function', `shell_indent_after_switch',
    !> `shell_indent_after_case' besides a number of columns (`%columns'): + - ++
    !> -- * /, which `shell_variable_value' interprets.
    integer, parameter, public :: SHELL_PLUS = 2, SHELL_MINUS = 3, SHELL_PLUS_PLUS = 4, SHELL_MINUS_MINUS = 5, SHELL_STAR = 6, &
                                  SHELL_SLASH = 7

    ! The `%columns' of an indentation variable set to a named constant.
    integer, parameter :: COLUMNS_UNSET = -huge(0)

    !> An indentation variable (sh-script.el's sh-number-or-symbol-list
    !> type): a choice setting whose value can also be a number of columns
    !> (R6-39: `shell_indent_for_fi%columns = 0'; COLUMNS_UNSET, the most
    !> negative integer, when no number is set, since a negative number
    !> indents left), or one of SHELL_PLUS ... SHELL_SLASH by plain assignment
    !> (`shell_indent_after_if = SHELL_PLUS').  Not a port.
    type, extends(choice_setting_type), public :: shell_indent_choice_setting_type
        integer :: columns = COLUMNS_UNSET
    contains
        procedure :: clear_kinds => shell_indent_clear_kinds
        procedure :: other_value => shell_indent_other_value
        procedure :: kind_hint => shell_indent_kind_hint
        procedure :: get_kind_value => shell_indent_get_kind_value
        procedure :: set_kind_value => shell_indent_set_kind_value
    end type shell_indent_choice_setting_type

    !> The value of an indentation variable in an `shell_style_type': a
    !> number of COLUMNS, or the named constant CHOICE (SHELL_PLUS ...
    !> SHELL_SLASH; 0 for a number of columns).  Not a port.
    type, public :: shell_indent_type
        integer :: columns = 0
        integer :: choice = 0
    end type shell_indent_type

    !> Values of `shell_indent_after_continuation' besides .true. and .false.
    integer, parameter, public :: SHELL_INDENT_AFTER_CONTINUATION_ALWAYS = 2

    !> A shell and its direct ancestor (an element of `shell_ancestors').
    type, public :: shell_ancestor_type
        character(len=:), allocatable :: shell, ancestor
    end type shell_ancestor_type

    !> An element of `shell_aliases': a regular expression matched
    !> against the full path of the interpreter (or, with `basename', a
    !> name matched against its basename) and the shell it names.
    type, public :: shell_alias_type
        character(len=:), allocatable :: key
        logical :: basename = .false.
        character(len=:), allocatable :: shell
    end type shell_alias_type

    !> One shell's entry of a feature table of string lists: the strings
    !> and, for (SHELL sh-append ANCESTOR STRINGS...), the shell whose
    !> value follows them (`append').
    type, public :: shell_strings_entry_type
        character(len=:), allocatable :: shell, append
        type(string_type), allocatable :: strings(:)
    end type shell_strings_entry_type

    !> A feature table whose values are lists of strings (see `shell_feature').
    type, public :: shell_strings_feature_type
        type(shell_strings_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to STRINGS (after them APPEND's value, if given).
        procedure :: append => strings_feature_append
        !> The value for the current shell.
        procedure :: get_current => current_strings_feature
    end type shell_strings_feature_type

    !> One shell's entry of a feature table of font-lock keywords.
    type, public :: shell_keywords_entry_type
        character(len=:), allocatable :: shell, append
        type(font_lock_keyword_type), allocatable :: keywords(:)
    end type shell_keywords_entry_type

    !> A feature table whose values are lists of font-lock keywords.
    type, public :: shell_keywords_feature_type
        type(shell_keywords_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to KEYWORDS (after them APPEND's value).
        procedure :: append => keywords_feature_append
        !> The value for the current shell.
        procedure :: get_current => current_keywords_feature
    end type shell_keywords_feature_type

    !> One shell's entry of a feature table of strings: `value'
    !> unallocated for none.
    type, public :: shell_string_entry_type
        character(len=:), allocatable :: shell
        character(len=:), allocatable :: value
    end type shell_string_entry_type

    !> A feature table whose values are strings or none.
    type, public :: shell_string_feature_type
        type(shell_string_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to VALUE (absent for none).
        procedure :: append => string_feature_append
        !> The value for the current shell (unallocated for none).
        procedure :: get_current => current_string_feature
    end type shell_string_feature_type

    !> One shell's entry of `shell_index_menu_patterns'.
    type, public :: shell_index_menu_entry_type
        character(len=:), allocatable :: shell
        type(index_menu_pattern_type), allocatable :: patterns(:)
    end type shell_index_menu_entry_type

    !> A feature table whose values are Imenu generic expressions.
    type, public :: shell_index_menu_feature_type
        type(shell_index_menu_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to MATCHERS.
        procedure :: append => index_menu_feature_append
        !> The value for the current shell.
        procedure :: get_current => current_index_menu_feature
    end type shell_index_menu_feature_type

    !> A syntax modification: CHAR gets SYNTAX.
    type, public :: shell_syntax_change_type
        integer :: char = 0
        character(len=:), allocatable :: syntax
    end type shell_syntax_change_type

    !> One shell's entry of `shell_script_mode_syntax_table_input': none (no
    !> `changes'), or TABLE with CHAR and SYNTAX entries: a copy of `table' with
    !> `changes'.
    type, public :: shell_syntax_entry_type
        character(len=:), allocatable :: shell
        type(handle_type) :: table
        type(shell_syntax_change_type), allocatable :: changes(:)
    end type shell_syntax_entry_type

    !> A feature table of syntax tables.
    type, public :: shell_syntax_feature_type
        type(shell_syntax_entry_type), allocatable :: entries(:)
    end type shell_syntax_feature_type

    ! A buffer's own variables.
    type :: shell_local_type
        integer(int64) :: serial = 0
        ! sh-shell-variables (setq-local in `shell_set_shell').
        logical :: has_variables = .false.
        type(string_type), allocatable :: variables(:)
        logical :: variables_initialized = .false.
        ! sh-indent-supported-here.
        logical :: has_indent_supported = .false.
        character(len=:), allocatable :: indent_supported
        ! sh-header-marker.
        logical :: has_header = .false.
        type(handle_type) :: header_marker
        ! sh-mode-syntax-table (setq-local in `shell_set_shell').
        logical :: has_table = .false.
        type(handle_type) :: table
        ! sh-shell-process.
        type(handle_type) :: process
    end type shell_local_type

    type(shell_local_type), allocatable :: locals(:)

    !> Functions for doing TAB dynamic completion.  Port of sh-script.el
    !> sh-dynamic-complete-functions.
    type(completion_at_point_function_hook_type), target, public :: shell_script_dynamic_complete_functions = &
                                                                    completion_at_point_function_hook_type( &
                                                                    name="shell_script_dynamic_complete_functions")

    !> The keywords `shell_completion_at_point_function' offers.  Port of
    !> sh-script.el sh--completion-keywords.
    character(len=*), parameter :: shell_completion_keywords(5) = [character(len=5) :: "if", "while", "until", "for", "then"]

    !> An indentation style of `shell_styles': its NAME and the VALUES of
    !> the indentation variables in the order of `shell_indentation_variables'
    !> (`shell_style_type(name="mine", values=[shell_indent_type(columns=4),
    !> shell_indent_type(choice=SHELL_PLUS), ...])').
    type, public :: shell_style_type
        character(len=64) :: name = ""
        type(shell_indent_type) :: values(15)
    end type shell_style_type

    !> The indentation variables (sh-script.el sh-var-list), as their
    !> names.
    character(len=*), parameter, public :: shell_indentation_variables(15) = [character(len=33) :: "shell_basic_offset", &
                                                                               "shell_indent_after_case", "shell_indent_after_do", &
                                                                                                        "shell_indent_after_done", &
                                                                               "shell_indent_after_else", "shell_indent_after_if", &
                                                                                              "shell_indent_after_loop_construct", &
                                                                           "shell_indent_after_open", "shell_indent_for_case_alt", &
                                                                                                    "shell_indent_for_case_label", &
                                                                           "shell_indent_for_continuation", "shell_indent_for_do", &
                                                                                                          "shell_indent_for_done", &
                                                                                                            "shell_indent_for_fi", &
                                                                                                            "shell_indent_for_then"]

    !> A list of all known shell indentation styles.  Port of sh-script.el
    !> sh-styles-alist.
    type(shell_style_type), allocatable, public :: shell_styles(:)

    ! The state of completion-table-with-cache in
    ! `shell_completion_at_point_function': the last string and its
    ! candidates.
    character(len=:), allocatable :: command_cache_argument
    type(value_type) :: command_cache_result

    ! The default values of sh-shell-variables and
    ! sh-shell-variables-initialized.
    type(string_type), allocatable :: default_variables(:)
    logical :: default_variables_initialized = .false.

    !> Table showing the direct ancestor of various shells.
    !> This is the basis for `shell_feature'.  See also `shell_aliases'.
    !> By default we have the following three hierarchies:
    !>
    !> csh          C Shell
    !>   jcsh       C Shell with Job Control
    !>   tcsh       TENEX C Shell
    !>     itcsh    Ian's TENEX C Shell
    !> rc           Plan 9 Shell
    !>   es         Extensible Shell
    !> sh           Bourne Shell
    !>   ash        Almquist Shell
    !>     dash     Debian Almquist Shell
    !>   jsh        Bourne Shell with Job Control
    !>     bash     GNU Bourne Again Shell
    !>     ksh88    Korn Shell '88
    !>       ksh    Korn Shell '93
    !>         dtksh        CDE Desktop Korn Shell
    !>       pdksh  Public Domain Korn Shell
    !>         mksh    MirOS BSD Korn Shell
    !>       wksh   Window Korn Shell
    !>       zsh    Z Shell
    !>   oash       SCO OA (curses) Shell
    !>   posix      IEEE 1003.2 Shell Standard
    !>   wsh        ? Shell
    !> Port of sh-script.el sh-ancestor-alist.
    type(shell_ancestor_type), allocatable, public :: shell_ancestors(:)

    !> Table for transforming shell names to what they really are.
    !> Use this where the name of the executable doesn't correspond to
    !> the type of shell it really is.  Keys are regular expressions
    !> matched against the full path of the interpreter.  (For backward
    !> compatibility, keys may also be names, which are matched
    !> against the interpreter's basename.  The values are names
    !> naming the shell.  Port of sh-script.el sh-alias-alist.
    type(shell_alias_type), allocatable, public :: shell_aliases(:)

    !> The executable file name for the shell being programmed.  Port of
    !> sh-script.el sh-shell-file.
    type(string_setting_type), target, public :: shell_shell_file = string_setting_type(name="shell_shell_file")

    !> Regular expressions for recognizing shell function definitions,
    !> per shell.  See `shell_feature' and `index_menu_patterns'.  Port
    !> of sh-script.el sh-imenu-generic-expression.
    type(shell_index_menu_feature_type), public :: shell_index_menu_patterns

    !> Single argument string for the magic number.  See `shell_feature'.
    !> Port of sh-script.el sh-shell-arg.
    type(shell_string_feature_type), public :: shell_shell_argument

    !> The shell being programmed.  This is set by \[shell_set_shell].
    !> Port of sh-script.el sh-shell.
    type(string_setting_type), target, public :: shell_shell = string_setting_type(name="shell_shell")

    !> The abbrev table of Shell-script mode.  Port of sh-script.el
    !> sh-mode-abbrev-table.
    type(abbreviation_table_type), target, public :: shell_script_mode_abbreviation_table

    !> The abbrev table of `shell_base_mode'.
    type(abbreviation_table_type), target, public :: shell_base_mode_abbreviation_table

    !> The syntax table to use for Shell-Script mode.
    !> This is buffer-local in every such buffer.  Port of sh-script.el
    !> sh-mode-syntax-table.
    type(handle_type), public :: shell_script_mode_syntax_table

    !> The syntax table of `shell_base_mode'.
    type(handle_type), public :: shell_base_mode_syntax_table

    !> Syntax-table used in Shell-Script mode.  See `shell_feature'.  Port of
    !> sh-script.el sh-mode-syntax-table-input.
    type(shell_syntax_feature_type), public :: shell_script_mode_syntax_table_input

    !> Keymap used in Shell-Script mode.  Port of sh-script.el sh-mode-map.
    type(handle_type), public :: shell_script_mode_map

    !> The keymap of `shell_base_mode'.
    type(handle_type), public :: shell_base_mode_map

    !> Regexp for the variable name and what may follow in an assignment.
    !> First grouping matches the variable name.  This is up to and including the `='
    !> sign.  See `shell_feature'.  Port of sh-script.el sh-assignment-regexp.
    type(shell_string_feature_type), public :: shell_assignment_regular_expression

    !> Don't remember variables less than this length for completing reads.
    !> Port of sh-script.el sh-remember-variable-min.
    integer, public :: shell_remember_variable_minimum = 3

    !> Regexp to determine the beginning of a shell command.
    !> The actual command starts at the beginning of the second \(grouping\).
    !> Port of sh-script.el sh-beginning-of-command.
    character(len=:), allocatable, public :: shell_beginning_of_command

    !> Regexp to determine the end of a shell command.
    !> The actual command ends at the end of the first \(grouping\).
    !> Port of sh-script.el sh-end-of-command.
    character(len=:), allocatable, public :: shell_end_of_command

    !> Word to delimit here documents.
    !> If the first character of this string is "-", this is taken as
    !> part of the redirection operator, rather than part of the
    !> word (that is, "<<-" instead of "<<").  This is a feature
    !> used by some shells (for example Bash) to indicate that leading
    !> tabs inside the here document should be ignored.  In this case,
    !> Fortmacs indents the initial body and end of the here document with
    !> tabs, to the same level as the start (note that apart from this
    !> there is no support for indentation of here documents).  This
    !> will only work correctly if `shell_basic_offset' is a multiple of
    !> `tab_width'.
    !>
    !> Any quote characters or leading whitespace in the word are
    !> removed when closing the here document.  Port of sh-script.el
    !> sh-here-document-word.
    character(len=:), allocatable, public :: shell_here_document_word

    !> List of all shell builtins for completing read and fontification.
    !> Note that on some systems not all builtins are available or some are
    !> implemented as aliases.  See `shell_feature'.  Port of sh-script.el
    !> sh-builtins.
    type(shell_strings_feature_type), public :: shell_builtins

    !> How to indent statements following && in Shell-Script mode.
    !> If true, indent to align with &&.
    !> If false, indent to align with the previous line's indentation.
    !> Port of sh-script.el sh-indent-statement-after-and.
    logical, public :: shell_indent_statement_after_and = .true.

    !> List of keywords that may be immediately followed by a builtin or keyword.
    !> Given some confusion between keywords and builtins depending on shell and
    !> system, the distinction here has been based on whether they influence the
    !> flow of control or syntax.  See `shell_feature'.  Port of sh-script.el
    !> sh-leading-keywords.
    type(shell_strings_feature_type), public :: shell_leading_keywords

    !> List of keywords not in `shell_leading_keywords'.
    !> See `shell_feature'.  Port of sh-script.el sh-other-keywords.
    type(shell_strings_feature_type), public :: shell_other_keywords

    !> List of all shell variables available for completing read.
    !> See `shell_feature'.  Port of sh-script.el sh-variables.
    type(shell_strings_feature_type), public :: shell_variables

    !> Face to show a here-document.  Port of sh-script.el:sh-heredoc.
    type(named_face_type), target, public :: shell_heredoc

    !> Face to show quoted execs like `blabla`.  Port of
    !> sh-script.el:sh-quoted-exec.
    type(named_face_type), target, public :: shell_quoted_exec

    !> Face used for (non-escaped) backslash at end of a line in Shell-script mode.
    !> Port of sh-script.el:sh-escaped-newline.
    type(named_face_type), target, public :: shell_escaped_newline

    !> Default expressions to highlight in Shell Script modes.  See `shell_feature'.
    !> Port of sh-script.el sh-font-lock-keywords-var.
    type(shell_keywords_feature_type), public :: shell_script_font_lock_keywords_variable

    !> Subdued level highlighting for Shell Script modes.  Port of
    !> sh-script.el sh-font-lock-keywords-var-1.
    type(shell_keywords_feature_type), public :: shell_script_font_lock_keywords_variable_1

    !> Gaudy level highlighting for Shell Script modes.  Port of
    !> sh-script.el sh-font-lock-keywords-var-2.
    type(shell_keywords_feature_type), public :: shell_script_font_lock_keywords_variable_2

    !> Hook run by `shell_set_shell'.  Port of sh-script.el sh-set-shell-hook.
    type(hook_type), target, public :: shell_set_shell_hook = hook_type(name="shell_set_shell_hook")

    !> Hook run by `shell_script_mode'.  Port of sh-script.el sh-mode-hook.
    type(hook_type), target, public :: shell_script_mode_hook = hook_type(name="shell_script_mode_hook")

    !> Hook run after entering `shell_base_mode'.
    type(hook_type), target, public :: shell_base_mode_hook = hook_type(name="shell_base_mode_hook")

    !> The default indentation increment.
    !> This value is used for the `+' and `-' values in an indentation variable.
    !> Port of sh-script.el sh-basic-offset.
    type(integer_setting_type), target, public :: shell_basic_offset = integer_setting_type(name="shell_basic_offset", value=4)

    !> How much to indent a `fi' relative to its `if'.  Usually 0.  Port of
    !> sh-script.el sh-indent-for-fi.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_fi = &
                             shell_indent_choice_setting_type(name="shell_indent_for_fi", true_valid=.false., false_valid=.false., &
                                                                                               columns=0)

    !> How much to indent a `done' relative to its matching stmt.  Usually 0.
    !> Port of sh-script.el sh-indent-for-done.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_done = &
                           shell_indent_choice_setting_type(name="shell_indent_for_done", true_valid=.false., false_valid=.false., &
                                                                                               columns=0)

    !> How much to indent a statement after an `else' statement.  Port of
    !> sh-script.el sh-indent-after-else.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_else = &
                         shell_indent_choice_setting_type(name="shell_indent_after_else", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent a statement after an `if' statement.
    !> This includes lines after `else' and `elif' statements, too, but
    !> does not affect the `else', `elif' or `fi' statements themselves.
    !> Port of sh-script.el sh-indent-after-if.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_if = &
                           shell_indent_choice_setting_type(name="shell_indent_after_if", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent a `then' relative to its `if'.  Port of
    !> sh-script.el sh-indent-for-then.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_then = &
                           shell_indent_choice_setting_type(name="shell_indent_for_then", true_valid=.false., false_valid=.false., &
                                                                                               columns=0)

    !> How much to indent a `do' statement.
    !> This is relative to the statement before the `do', typically a
    !> `while', `until', `for', `repeat' or `select' statement.  Port of
    !> sh-script.el sh-indent-for-do.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_do = &
                             shell_indent_choice_setting_type(name="shell_indent_for_do", true_valid=.false., false_valid=.false., &
                                                                                               columns=0)

    !> How much to indent a line after a `do' statement.
    !> This is used when the `do' is the first word of the line.
    !> This is relative to the statement before the `do', typically a
    !> `while', `until', `for', `repeat' or `select' statement.  Port of
    !> sh-script.el sh-indent-after-do.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_do = &
                           shell_indent_choice_setting_type(name="shell_indent_after_do", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent a statement after a loop construct.
    !>
    !> This variable is used when the keyword `do' is on the same line as the
    !> loop statement (e.g., `until', `while' or `for').
    !> If the `do' is on a line by itself, then `shell_indent_after_do' is used instead.
    !> Port of sh-script.el sh-indent-after-loop-construct.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_loop_construct = &
               shell_indent_choice_setting_type(name="shell_indent_after_loop_construct", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent a statement after a `done' keyword.
    !> Normally this is 0, which aligns the `done' to the matching
    !> looping construct line.
    !> Setting it non-zero allows you to have the `do' statement on a line
    !> by itself and align the done under to do.  Port of sh-script.el
    !> sh-indent-after-done.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_done = &
                         shell_indent_choice_setting_type(name="shell_indent_after_done", true_valid=.false., false_valid=.false., &
                                                                                               columns=0)

    !> How much to indent a case label statement.
    !> This is relative to the line containing the `case' statement.
    !> Port of sh-script.el sh-indent-for-case-label.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_case_label = &
                     shell_indent_choice_setting_type(name="shell_indent_for_case_label", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent statements after the case label.
    !> This is relative to the line containing the `case' statement.
    !> Port of sh-script.el sh-indent-for-case-alt.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_case_alt = &
                       shell_indent_choice_setting_type(name="shell_indent_for_case_alt", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS_PLUS)

    !> How much to indent for a continuation statement.  Port of
    !> sh-script.el sh-indent-for-continuation.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_for_continuation = &
                   shell_indent_choice_setting_type(name="shell_indent_for_continuation", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent after a line with an opening parenthesis or brace.
    !> For an open paren after a function, `shell_indent_after_function' is used.
    !> Port of sh-script.el sh-indent-after-open.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_open = &
                         shell_indent_choice_setting_type(name="shell_indent_after_open", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent after a function line.  Port of sh-script.el
    !> sh-indent-after-function.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_function = &
                     shell_indent_choice_setting_type(name="shell_indent_after_function", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent a `case' statement relative to the `switch' statement.
    !> This is for the rc shell.  Port of sh-script.el sh-indent-after-switch.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_switch = &
                       shell_indent_choice_setting_type(name="shell_indent_after_switch", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> How much to indent a statement relative to the `case' statement.
    !> This is for the rc shell.  Port of sh-script.el sh-indent-after-case.
    type(shell_indent_choice_setting_type), target, public :: shell_indent_after_case = &
                         shell_indent_choice_setting_type(name="shell_indent_after_case", true_valid=.false., false_valid=.false., &
                                                                                               choice=SHELL_PLUS)

    !> Column in which `shell_backslash_region' inserts backslashes.  Port of
    !> sh-script.el sh-backslash-column.
    integer, public :: shell_backslash_column = 48

    !> If true, `shell_backslash_region' will align backslashes.  Port of
    !> sh-script.el sh-backslash-align.
    logical, public :: shell_backslash_align = .true.

    !> Indentation rule set to use for each shell type.  Port of
    !> sh-script.el sh-indent-supported.
    type(shell_string_feature_type), public :: shell_indent_supported

    !> Controls whether indentation variables are local to the buffer.
    !> If true, indentation variables are made local initially.
    !> If false, you can later make the variables local by invoking
    !> command `shell_make_variables_local'.
    !> The default is true because I assume that in one Fortmacs session one is
    !> frequently editing existing scripts with different styles.  Port of
    !> sh-script.el sh-make-vars-local.
    logical, public :: shell_make_variables_local = .true.

    !> If true, indent relative to the continued line's beginning.
    !> Continued lines can either be indented as "one long wrapped line" without
    !> paying attention to the actual syntactic structure, as in:
    !>
    !>    for f \
    !>        in a; do \
    !>        toto; \
    !>        done
    !>
    !> or as lines that just don't have implicit semi-colons between them, as in:
    !>
    !>    for f \
    !>    in a; do \
    !>        toto; \
    !>    done
    !>
    !> With `SHELL_INDENT_AFTER_CONTINUATION_ALWAYS' you get the former behavior whereas with
    !> .false. you get the latter.  With .true., you
    !> get the latter as long as that would indent the continuation line
    !> deeper than the initial line.  Port of sh-script.el
    !> sh-indent-after-continuation.
    type(choice_setting_type), public :: shell_indent_after_continuation = &
                                         choice_setting_type(name="shell_indent_after_continuation", value=.true.)

    !> The SIMPLE_MINDED_INDENTATION_ENGINE grammar of sh.  Port of sh-script.el sh-smie-sh-grammar.
    type(simple_minded_indentation_engine_grammar_type), target, public :: shell_indentation_engine_shell_grammar

    !> The SIMPLE_MINDED_INDENTATION_ENGINE grammar of rc.  Port of sh-script.el sh-smie-rc-grammar.
    type(simple_minded_indentation_engine_grammar_type), target, public :: shell_indentation_engine_rc_grammar

    ! sh-smie--sh-operators-re and sh-smie--sh-operators-back-re.
    character(len=:), allocatable :: shell_operators_regular_expression, shell_operators_back_regular_expression

    ! sh-escaped-line-re and sh-here-doc-open-re.
    character(len=:), allocatable :: shell_escaped_line_regular_expression, shell_here_doc_open_regular_expression

    ! The rules of `shell_syntax_propertize_function', and the END it was
    ! called with (for the code of the rules).
    type(syntax_propertize_rules_type) :: propertize_rules
    integer(int64) :: propertize_end = 0

    ! The states of `shell_font_lock_quoted_subshell'.
    integer, parameter :: QS_NONE = 0, QS_DOUBLE_QUOTE = 1, QS_BACKQUOTE = 2, QS_CODE = 3

    !> An entry of `shell_test': a SHELL, the initial INPUT and the POSITION of
    !> point in it.
    type, public :: shell_test_entry_type
        character(len=16) :: shell = ""
        character(len=16) :: input = ""
        integer :: position = 0
    end type shell_test_entry_type

    !> Initial input in Bourne if, while and until skeletons.  See `shell_feature'.
    !> Port of sh-script.el sh-test.
    type(shell_test_entry_type), allocatable, public :: shell_test(:)

    ! The skeletons of the statement commands, by shell.
    type(skeleton_variants_type) :: shell_case_skeletons, shell_for_skeletons, shell_indexed_loop_skeletons, &
                                    shell_function_skeletons, &
                                    shell_if_skeletons, shell_repeat_skeletons, shell_select_skeletons, &
                                    shell_temporary_file_skeletons, &
                                    shell_until_skeletons, shell_while_skeletons, shell_while_getopts_skeletons

    !> Value to use for `skeleton_default_pairs' in Shell-Script mode.
    !> Port of sh-script.el sh-skeleton-pair-default-alist.
    type(skeleton_pairs_type), public :: shell_skeleton_default_pairs

    ! The v1 of sh's while getopts skeleton: the option letters left, and
    ! the position before the usage message's options.
    character(len=:), allocatable :: getopts_letters
    integer(int64) :: getopts_mark = 0

contains

    ! ------------------------------------------------------------------
    ! Setting up
    ! ------------------------------------------------------------------

    !> Define Shell-script mode: its options, feature tables, faces, syntax
    !> tables, keymap, grammars and commands.  Not a port.
    subroutine initialize_shell_script()
        type(status_type) :: st
        character(len=:), allocatable :: shell
        logical :: found

        if (is_syntax_table_live(shell_script_mode_syntax_table)) return
        call forward_setting(shell_shell_file)
        call forward_setting(shell_shell)
        call forward_setting(shell_basic_offset)
        call forward_setting(shell_indent_for_fi)
        call forward_setting(shell_indent_for_done)
        call forward_setting(shell_indent_after_else)
        call forward_setting(shell_indent_after_if)
        call forward_setting(shell_indent_for_then)
        call forward_setting(shell_indent_for_do)
        call forward_setting(shell_indent_after_do)
        call forward_setting(shell_indent_after_loop_construct)
        call forward_setting(shell_indent_after_done)
        call forward_setting(shell_indent_for_case_label)
        call forward_setting(shell_indent_for_case_alt)
        call forward_setting(shell_indent_for_continuation)
        call forward_setting(shell_indent_after_open)
        call forward_setting(shell_indent_after_function)
        call forward_setting(shell_indent_after_switch)
        call forward_setting(shell_indent_after_case)
        call initialize_shells()
        if (.not. allocated(default_variables)) allocate (default_variables(0))

        if (.not. allocated(shell_shell_file%value)) then
            call getenv("SHELL", shell, found)

            if (found) then
                shell_shell_file = shell
            else
                shell_shell_file = "/bin/sh"
            end if
        end if

        if (.not. allocated(shell_shell%value)) shell_shell = shell_canonicalize_shell(shell_shell_file%value)

        if (.not. allocated(shell_beginning_of_command)) then
            shell_beginning_of_command = "\([;({`|&]\|\`\|[^\]"//LF//"\)["//" "//TAB//"]*\([/~[:alnum:]:]\)"
        end if

        if (.not. allocated(shell_end_of_command)) then
            shell_end_of_command = "\([/~[:alnum:]:]\)[ "//TAB//"]*\([;#)}`|&]\|$\)"
        end if

        if (.not. allocated(shell_here_document_word)) shell_here_document_word = "EOF"
        shell_escaped_line_regular_expression = "\(?:\(?:.*[^\"//LF//"]\)?\(?:\\\\\)*\\"//LF//"\)*.*"
        shell_here_doc_open_regular_expression = "[^<]<<-?\s-*\\?\(\(?:['""][^'""]+['""]\|\sw\|[-/~._@]\)+\)" &
                                                 //shell_escaped_line_regular_expression// &
                                                 "\("//LF//"\)"
        call initialize_features()
        call initialize_syntax_tables(st)
        call define_faces()
        call initialize_font_lock_keywords()
        call initialize_rules()
        call initialize_grammars(st)
        call initialize_skeletons()
        call shell_skeleton_default_pairs%append(40, [text("("), SKELETON_POINT, text(")")])
        call shell_skeleton_default_pairs%append(41, [text(")")])
        call shell_skeleton_default_pairs%append(91, [text("["), text(" "), SKELETON_POINT, text(" "), text("]")])
        call shell_skeleton_default_pairs%append(93, [text("]")])
        call shell_skeleton_default_pairs%append(123, [text("{"), SKELETON_POINT, text("}")])
        call shell_skeleton_default_pairs%append(125, [text("}")])
        call shell_script_mode_hook%prepend(shell_electric_here_document_mode)
        call major_modes%shell_base_mode%define("shell_base_mode", "Shell-script", shell_base_mode, shell_base_mode_hook, &
                                                parent=major_modes%programming_mode, syntax_table=shell_base_mode_syntax_table, &
                                                abbreviation_table=shell_base_mode_abbreviation_table)
        shell_base_mode_map = major_modes%shell_base_mode%map
        call major_modes%shell_script_mode%define("shell_script_mode", "Shell-script", shell_script_mode, shell_script_mode_hook, &
                                                  parent=major_modes%shell_base_mode, &
                                                  syntax_table=shell_script_mode_syntax_table, &
                                                  abbreviation_table=shell_script_mode_abbreviation_table)
        shell_script_mode_map = major_modes%shell_script_mode%map
        call keymap_set(shell_script_mode_map, "C-c C-\", "shell_backslash_region", st)
        call keymap_set(shell_script_mode_map, "C-c +", "shell_add", st)
        call keymap_set(shell_script_mode_map, "C-M-x", "shell_execute_region", st)
        call keymap_set(shell_script_mode_map, "C-c C-n", "shell_send_line_or_region_and_step", st)
        call keymap_set(shell_script_mode_map, "C-c C-d", "shell_cd_here", st)
        call keymap_set(shell_script_mode_map, "C-c C-z", "shell_show_shell", st)
        call keymap_set(shell_script_mode_map, "C-c :", "shell_set_shell", st)
        call keymap_set(shell_script_mode_map, "C-c (", "shell_function", st)
        call keymap_set(shell_script_mode_map, "C-c C-w", "shell_while", st)
        call keymap_set(shell_script_mode_map, "C-c C-u", "shell_until", st)
        call keymap_set(shell_script_mode_map, "C-c C-t", "shell_temporary_file", st)
        call keymap_set(shell_script_mode_map, "C-c C-s", "shell_select", st)
        call keymap_set(shell_script_mode_map, "C-c C-r", "shell_repeat", st)
        call keymap_set(shell_script_mode_map, "C-c C-o", "shell_while_getopts", st)
        call keymap_set(shell_script_mode_map, "C-c C-l", "shell_indexed_loop", st)
        call keymap_set(shell_script_mode_map, "C-c C-i", "shell_if", st)
        call keymap_set(shell_script_mode_map, "C-c C-f", "shell_for", st)
        call keymap_set(shell_script_mode_map, "C-c C-c", "shell_case", st)
        call keymap_set(shell_script_mode_map, "C-c ?", "simple_minded_indentation_engine_configuration_show_indent", st)
        call keymap_set(shell_script_mode_map, "C-c =", "simple_minded_indentation_engine_configuration_set_indent", st)
        call keymap_set(shell_script_mode_map, "C-c <", "simple_minded_indentation_engine_configuration_set_indent", st)
        call keymap_set(shell_script_mode_map, "C-c >", "simple_minded_indentation_engine_configuration_guess", st)
        call bind_remap("delete_backward_character", "backward_delete_character_untabify")
        call bind_remap("backward_sentence", "shell_beginning_of_command")
        call bind_remap("forward_sentence", "shell_end_of_command")
        call minor_modes%shell_electric_here_document_mode%define("shell_electric_here_document_mode", &
                                                                  body=shell_electric_here_document_mode_body)
        commands%shell_base_mode%execute => shell_base_mode
        commands%shell_script_mode%execute => shell_script_mode
        commands%shell_script_mode%execute => shell_script_mode
        commands%shell_set_shell%execute => execute_shell_set_shell
        commands%shell_basic_indent_line%execute => execute_shell_basic_indent_line
        commands%shell_execute_region%execute => execute_shell_execute_region
        commands%shell_make_variables_local%execute => execute_shell_make_variables_local
        commands%shell_reset_indent_variables_to_global_values%execute => shell_reset_indent_variables_to_global_values
        commands%shell_add%execute => execute_shell_add
        commands%shell_assignment%execute => execute_shell_assignment
        commands%shell_electric_here_document_mode%execute => shell_electric_here_document_mode
        commands%shell_beginning_of_command%execute => execute_shell_beginning_of_command
        commands%shell_end_of_command%execute => execute_shell_end_of_command
        commands%shell_backslash_region%execute => execute_shell_backslash_region
        commands%shell_show_shell%execute => shell_show_shell
        commands%shell_cd_here%execute => shell_cd_here
        commands%shell_send_line_or_region_and_step%execute => shell_send_line_or_region_and_step
        commands%shell_name_style%execute => execute_shell_name_style
        commands%shell_load_style%execute => execute_shell_load_style
        commands%shell_save_styles_to_buffer%execute => execute_shell_save_styles_to_buffer
        commands%shell_case%execute => execute_shell_case
        commands%shell_for%execute => execute_shell_for
        commands%shell_indexed_loop%execute => execute_shell_indexed_loop
        commands%shell_function%execute => execute_shell_function
        commands%shell_if%execute => execute_shell_if
        commands%shell_repeat%execute => execute_shell_repeat
        commands%shell_select%execute => execute_shell_select
        commands%shell_temporary_file%execute => execute_shell_temporary_file
        commands%shell_until%execute => execute_shell_until
        commands%shell_while%execute => execute_shell_while
        commands%shell_while_getopts%execute => execute_shell_while_getopts
        ! (The default value, in reverse order: `prepend' puts a function first.)
        call shell_script_dynamic_complete_functions%prepend(command_interpreter_filename_completion)
        call shell_script_dynamic_complete_functions%prepend(shell_command_completion)
        call shell_script_dynamic_complete_functions%prepend(shell_environment_variable_completion)
        if (.not. allocated(shell_styles)) allocate (shell_styles(0))
        call append_locals_clearer(clear_shell_script_locals)
    end subroutine initialize_shell_script

    ! Bind [remap FROM] to COMMAND in `shell_script_mode_map'.  Not a port.
    subroutine bind_remap(from, command)
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call list_elements(list(value_type(name="remap"), value_type(name=from)), events)
        call define_key(shell_script_mode_map, events, value_type(name=command), st)
    end subroutine bind_remap

    ! Append the pair (SHELL . ANCESTOR) to `shell_ancestors'.
    ! Not a port.
    subroutine append_ancestor(shell, ancestor)
        character(len=*), intent(in) :: shell, ancestor
        type(shell_ancestor_type) :: entry

        if (.not. allocated(shell_ancestors)) allocate (shell_ancestors(0))
        entry%shell = shell
        entry%ancestor = ancestor
        shell_ancestors = [shell_ancestors, entry]
    end subroutine append_ancestor

    ! Append (KEY . SHELL) to `shell_aliases'.  Not a port.
    subroutine append_alias(key, shell, basename)
        character(len=*), intent(in) :: key, shell
        logical, intent(in) :: basename
        type(shell_alias_type) :: entry

        if (.not. allocated(shell_aliases)) allocate (shell_aliases(0))
        entry%key = key
        entry%basename = basename
        entry%shell = shell
        shell_aliases = [shell_aliases, entry]
    end subroutine append_alias

    ! `shell_ancestors' and `shell_aliases'.  Not a port.
    subroutine initialize_shells()
        if (allocated(shell_ancestors)) return
        call append_ancestor("ash", "sh")
        call append_ancestor("bash", "jsh")
        call append_ancestor("bash2", "jsh")
        call append_ancestor("dash", "ash")
        call append_ancestor("dtksh", "ksh")
        call append_ancestor("es", "rc")
        call append_ancestor("itcsh", "tcsh")
        call append_ancestor("jcsh", "csh")
        call append_ancestor("jsh", "sh")
        call append_ancestor("ksh", "ksh88")
        call append_ancestor("ksh88", "jsh")
        call append_ancestor("oash", "sh")
        call append_ancestor("pdksh", "ksh88")
        call append_ancestor("mksh", "pdksh")
        call append_ancestor("posix", "sh")
        call append_ancestor("tcsh", "csh")
        call append_ancestor("wksh", "ksh88")
        call append_ancestor("wsh", "sh")
        call append_ancestor("zsh", "ksh88")
        call append_ancestor("rpm", "sh")
        ! (eq system-type 'gnu/linux)
        call append_alias("csh", "tcsh", .true.)
        call append_alias("ksh", "pdksh", .true.)
        ! for the time being
        call append_alias("ksh", "ksh88", .true.)
        call append_alias("bash2", "bash", .true.)
        call append_alias("sh5", "sh", .true.)
        ! Android's system shell
        call append_alias("^/system/bin/sh$", "mksh", .false.)
    end subroutine initialize_shells

    ! The feature tables of strings.  Not a port.
    subroutine initialize_features()
        if (allocated(shell_builtins%entries)) return
        call shell_shell_argument%append("bash")
        call shell_shell_argument%append("csh", "-f")
        call shell_shell_argument%append("pdksh")
        ! Bill_Mann@praxisint.com says -p with ksh can do harm.
        call shell_shell_argument%append("ksh88")
        ! -p means don't initialize functions from the environment.
        call shell_shell_argument%append("rc", "-p")
        ! Someone proposed -motif, but we don't want to encourage
        ! use of a non-free widget set.
        call shell_shell_argument%append("wksh")
        ! -f means don't run .zshrc.
        call shell_shell_argument%append("zsh", "-f")
        ! The "\\[.+\\]" matches the "[index]" in "arrayvar[index]=value".
        call shell_assignment_regular_expression%append("csh", "\<\([[:alnum:]_]+\)\(\[.+\]\)?[ "//TAB//"]*[-+*/%^]?=")
        ! actually spaces are only supported in let/(( ... ))
        call shell_assignment_regular_expression%append("ksh88", "\<\([[:alnum:]_]+\)\(\[.+\]\)?"// &
                                                        "[ "//TAB//"]*\(?:[-+*/%&|~^]\|<<\|>>\)?=")
        call shell_assignment_regular_expression%append("bash", "\<\([[:alnum:]_]+\)\(\[.+\]\)?\+?=")
        call shell_assignment_regular_expression%append("rc", "\<\([[:alnum:]_*]+\)[ "//TAB//"]*=")
        call shell_assignment_regular_expression%append("sh", "\<\([[:alnum:]_]+\)=")
        call shell_indent_supported%append("sh", "sh")
        call shell_indent_supported%append("csh")
        call shell_indent_supported%append("rc", "rc")
        call shell_builtins%append("bash", [character(len=9) :: &
                                            ".", "alias", "bg", "bind", "builtin", "caller", "compgen", "complete", &
                                            "declare", "dirs", "disown", "enable", "fc", "fg", "help", "history", &
                                            "jobs", "kill", "let", "local", "popd", "printf", "pushd", "shopt", &
                                            "source", "suspend", "typeset", "unalias", &
                                            "mapfile", "readarray", "coproc"], "posix")
        ! The next entry is only used for defining the others
        call shell_builtins%append("bourne", [character(len=8) :: "eval", "export", "getopts", "newgrp", "pwd", "read", &
                                              "readonly", "times", "ulimit"], "shell")
        call shell_builtins%append("csh", [character(len=7) :: "alias", "chdir", "glob", "history", "limit", "nice", "nohup", &
                                           "rehash", "setenv", "source", "time", "unalias", "unhash"], "shell")
        call shell_builtins%append("dtksh", [character(len=1) ::], "wksh")
        call shell_builtins%append("es", [character(len=7) :: "access", "apids", "cd", "echo", "eval", "false", "let", "limit", &
                                          "local", "newpgrp", "result", "time", "umask", "var", "vars", "wait", "whatis"])
        call shell_builtins%append("jsh", [character(len=7) :: "bg", "fg", "jobs", "kill", "stop", "suspend"], "sh")
        call shell_builtins%append("jcsh", [character(len=7) :: "bg", "fg", "jobs", "kill", "notify", "stop", "suspend"], "csh")
        call shell_builtins%append("ksh88", [character(len=7) :: "alias", "bg", "false", "fc", "fg", "jobs", "kill", "let", &
                                             "print", "time", "typeset", "unalias", "whence"], "bourne")
        call shell_builtins%append("oash", [character(len=9) :: "checkwin", "dateline", "error", "form", "menu", "newwin", &
                                            "oadeinit", "oaed", "oahelp", "oainit", "pp", "ppfile", "scan", "scrollok", &
                                            "wattr", "wclear", "werase", "win", "wmclose", "wmmessage", "wmopen", "wmove", &
                                            "wmtitle", "wrefresh"], "sh")
        call shell_builtins%append("pdksh", [character(len=4) :: "bind"], "ksh88")
        call shell_builtins%append("posix", [character(len=7) :: "command"], "sh")
        call shell_builtins%append("rc", [character(len=7) :: "builtin", "cd", "echo", "eval", "limit", "newpgrp", "shift", &
                                          "umask", "wait", "whatis"])
        call shell_builtins%append("sh", [character(len=4) :: "hash", "test", "type"], "bourne")
        ! The next entry is only used for defining the others
        call shell_builtins%append("shell", [character(len=5) :: "cd", "echo", "eval", "set", "shift", "umask", "unset", "wait"])
        call shell_builtins%append("wksh", [character(len=1) ::], "ksh88")
        call shell_builtins%append("zsh", [character(len=10) :: "autoload", "always", &
                                           "bindkey", "builtin", "chdir", "compctl", "declare", "dirs", &
                                           "disable", "disown", "echotc", "enable", "functions", "getln", "hash", &
                                           "history", "integer", "limit", "local", "log", "popd", "pushd", "r", &
                                           "readonly", "rehash", "sched", "setopt", "source", "suspend", "true", &
                                           "ttyctl", "type", "unfunction", "unhash", "unlimit", "unsetopt", "vared", &
                                           "which"], "ksh88")
        call shell_leading_keywords%append("bash", [character(len=4) :: "time"], "sh")
        call shell_leading_keywords%append("csh", [character(len=4) :: "else"])
        call shell_leading_keywords%append("es", [character(len=14) :: "true", "unwind-protect", "whatis"])
        call shell_leading_keywords%append("rc", [character(len=4) :: "else"])
        call shell_leading_keywords%append("sh", [character(len=5) :: "!", "do", "elif", "else", "if", "then", "trap", "type", &
                                                  "until", "while"])
        call shell_other_keywords%append("bash", [character(len=6) :: "bye", "logout", "select"], "bourne")
        ! The next entry is only used for defining the others
        call shell_other_keywords%append("bourne", [character(len=8) :: "function"], "sh")
        call shell_other_keywords%append("csh", [character(len=7) :: "breaksw", "default", "end", "endif", "endsw", "foreach", &
                                                 "goto", "if", "logout", "onintr", "repeat", "switch", "then", "while"], "shell")
        call shell_other_keywords%append("es", [character(len=7) :: "break", "catch", "exec", "exit", "fn", "for", "forever", &
                                                "fork", "if", "return", "throw", "while"])
        call shell_other_keywords%append("ksh88", [character(len=6) :: "select"], "bourne")
        call shell_other_keywords%append("rc", [character(len=6) :: "break", "case", "exec", "exit", "fn", "for", "if", "in", &
                                                "return", "switch", "while"])
        call shell_other_keywords%append("sh", [character(len=6) :: "done", "esac", "fi", "for", "in", "return"], "shell")
        ! The next entry is only used for defining the others
        call shell_other_keywords%append("shell", [character(len=8) :: "break", "case", "continue", "exec", "exit"])
        call shell_other_keywords%append("zsh", [character(len=7) :: "select", "foreach"], "bash")
        call shell_variables%append("bash", [character(len=25) :: &
                                             "allow_null_glob_expansion", "auto_resume", "BASH", "BASH_ENV", &
                                             "BASH_VERSINFO", "BASH_VERSION", "cdable_vars", "COMP_CWORD", &
                                             "COMP_LINE", "COMP_POINT", "COMP_WORDS", "COMPREPLY", "DIRSTACK", &
                                             "ENV", "EUID", "FCEDIT", "FIGNORE", "FUNCNAME", &
                                             "glob_dot_filenames", "GLOBIGNORE", "GROUPS", "histchars", &
                                             "HISTCMD", "HISTCONTROL", "HISTFILE", "HISTFILESIZE", &
                                             "HISTIGNORE", "history_control", "HISTSIZE", &
                                             "hostname_completion_file", "HOSTFILE", "HOSTTYPE", "IGNOREEOF", &
                                             "ignoreeof", "INPUTRC", "LINENO", "MACHTYPE", "MAIL_WARNING", &
                                             "noclobber", "nolinks", "notify", "no_exit_on_failed_exec", &
                                             "NO_PROMPT_VARS", "OLDPWD", "OPTERR", "OSTYPE", "PIPESTATUS", &
                                             "PPID", "POSIXLY_CORRECT", "PROMPT_COMMAND", "PS3", "PS4", &
                                             "pushd_silent", "PWD", "RANDOM", "REPLY", "SECONDS", "SHELLOPTS", &
                                             "SHLVL", "TIMEFORMAT", "TMOUT", "UID"], "sh")
        call shell_variables%append("csh", [character(len=9) :: &
                                            "argv", "cdpath", "child", "echo", "histchars", "history", "home", &
                                            "ignoreeof", "mail", "noclobber", "noglob", "nonomatch", "path", "prompt", &
                                            "shell", "status", "time", "verbose"], "shell")
        call shell_variables%append("es", [character(len=8) :: &
                                           "apid", "cdpath", "CDPATH", "history", "home", "ifs", "noexport", "path", &
                                           "pid", "prompt", "signals"], "shell")
        call shell_variables%append("jcsh", [character(len=6) :: "notify"], "csh")
        call shell_variables%append("ksh88", [character(len=8) :: &
                                              "ENV", "ERRNO", "FCEDIT", "FPATH", "HISTFILE", "HISTSIZE", "LINENO", &
                                              "OLDPWD", "PPID", "PS3", "PS4", "PWD", "RANDOM", "REPLY", "SECONDS", &
                                              "TMOUT"], "sh")
        call shell_variables%append("oash", [character(len=9) :: &
                                             "FIELD", "FIELD_MAX", "LAST_KEY", "OALIB", "PP_ITEM", "PP_NUM"], "sh")
        call shell_variables%append("rc", [character(len=7) :: &
                                           "apid", "apids", "cdpath", "CDPATH", "history", "home", "ifs", "path", "pid", &
                                           "prompt", "status"], "shell")
        call shell_variables%append("sh", [character(len=6) :: "CDPATH", "IFS", "OPTARG", "OPTIND", "PS1", "PS2"], "shell")
        ! The next entry is only used for defining the others
        call shell_variables%append("shell", [character(len=11) :: &
                                              "COLUMNS", "EDITOR", "HOME", "HUSHLOGIN", "LANG", "LC_COLLATE", &
                                              "LC_CTYPE", "LC_MESSAGES", "LC_MONETARY", "LC_NUMERIC", "LC_TIME", &
                                              "LINES", "LOGNAME", "MAIL", "MAILCHECK", "MAILPATH", "PAGER", "PATH", &
                                              "SHELL", "TERM", "TERMCAP", "TERMINFO", "VISUAL"])
        call shell_variables%append("tcsh", [character(len=26) :: &
                                             "addsuffix", "ampm", "autocorrect", "autoexpand", "autolist", &
                                             "autologout", "chase_symlinks", "correct", "dextract", "edit", "el", &
                                             "fignore", "gid", "histlit", "HOST", "HOSTTYPE", "HPATH", &
                                             "ignore_symlinks", "listjobs", "listlinks", "listmax", "matchbeep", &
                                             "nobeep", "NOREBIND", "oid", "printexitvalue", "prompt2", "prompt3", &
                                             "pushdsilent", "pushdtohome", "recexact", "recognize_only_executables", &
                                             "rmstar", "savehist", "SHLVL", "showdots", "sl", "SYSTYPE", "tcsh", "term", &
                                             "tperiod", "tty", "uid", "version", "visiblebell", "watch", "who", &
                                             "wordchars"], "csh")
        call shell_variables%append("zsh", [character(len=12) :: &
                                            "BAUD", "bindcmds", "cdpath", "DIRSTACKSIZE", "fignore", "FIGNORE", "fpath", &
                                            "HISTCHARS", "hostcmds", "hosts", "HOSTS", "LISTMAX", "LITHISTSIZE", &
                                            "LOGCHECK", "mailpath", "manpath", "NULLCMD", "optcmds", "path", "POSTEDIT", &
                                            "prompt", "PROMPT", "PROMPT2", "PROMPT3", "PROMPT4", "psvar", "PSVAR", &
                                            "READNULLCMD", "REPORTTIME", "RPROMPT", "RPS1", "SAVEHIST", "SPROMPT", &
                                            "STTY", "TIMEFMT", "TMOUT", "TMPPREFIX", "varcmds", "watch", "WATCH", &
                                            "WATCHFMT", "WORDCHARS", "ZDOTDIR"], "ksh88")
        call initialize_index_menu_feature()
    end subroutine initialize_features

    ! The entries of `shell_index_menu_patterns'.  Port of
    ! sh-script.el sh-imenu-generic-expression (its value).
    subroutine initialize_index_menu_feature()
        ! The characters a mksh function name has none of.
        character(len=*), parameter :: MKSH_NAME = "\([^]"//achar(0)//TAB//LF//" ""$&'();<=>\`|#*?[/]+\)"
        type(index_menu_pattern_type) :: patterns(2)

        ! function FOO
        ! function FOO()
        patterns(1) = index_menu_pattern_type(menu_title="", regular_expression="^\s-*function\s-+\([[:alpha:]_][[:alnum:]_]*\)"// &
                                              "\s-*\(?:()\)?", subexpression=1)
        ! FOO()
        patterns(2) = index_menu_pattern_type(menu_title="", regular_expression="^\s-*\([[:alpha:]_][[:alnum:]_]*\)\s-*()", &
                                              subexpression=1)
        call shell_index_menu_patterns%append("sh", patterns)
        ! The difference between the Bash regular expression and the sh regular
        ! expression is that Bash also allows hyphens (-) in function names.
        patterns(1) = index_menu_pattern_type(menu_title="", &
                                              regular_expression="^\s-*function\s-+\([[:alpha:]_][[:alnum:]_-]*\)"// &
                                              "\s-*\(?:()\)?", subexpression=1)
        patterns(2) = index_menu_pattern_type(menu_title="", regular_expression="^\s-*\([[:alpha:]_][[:alnum:]_-]*\)\s-*()", &
                                              subexpression=1)
        call shell_index_menu_patterns%append("bash", patterns)
        patterns(1) = index_menu_pattern_type(menu_title="", regular_expression="^\s-*function\s-+"//MKSH_NAME//"\s-*\(?:()\)?", &
                                              subexpression=1)
        patterns(2) = index_menu_pattern_type(menu_title="", regular_expression="^\s-*"//MKSH_NAME//"\s-*()", subexpression=1)
        call shell_index_menu_patterns%append("mksh", patterns)
    end subroutine initialize_index_menu_feature

    ! Not a port.
    subroutine index_menu_feature_append(self, shell, patterns)
        class(shell_index_menu_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        type(index_menu_pattern_type), intent(in) :: patterns(:)
        type(shell_index_menu_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%shell = shell
        allocate (entry%patterns(size(patterns)))
        entry%patterns(:) = patterns(:)
        self%entries = [self%entries, entry]
    end subroutine index_menu_feature_append

    !> Index the table by the current shell.  Port of
    !> sh-script.el:sh-feature.
    function current_index_menu_feature(self) result(patterns)
        class(shell_index_menu_feature_type), intent(in) :: self
        type(index_menu_pattern_type), allocatable :: patterns(:)
        character(len=16), allocatable :: shells(:)
        integer :: k, i

        allocate (patterns(0))
        if (.not. allocated(self%entries)) return
        allocate (shells(size(self%entries)))

        do i = 1, size(self%entries)
            shells(i) = self%entries(i)%shell
        end do

        k = shell_feature_shell(shells, current_shell())
        if (k == 0) return
        deallocate (patterns)
        allocate (patterns(size(self%entries(k)%patterns)))
        patterns(:) = self%entries(k)%patterns(:)
    end function current_index_menu_feature

    ! `shell_script_mode_syntax_table', `shell_base_mode_syntax_table' and
    ! `shell_script_mode_syntax_table_input'.  Not a port.
    subroutine initialize_syntax_tables(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: t

        call make_syntax_table(shell_base_mode_syntax_table, status)
        ! (sh-mode-syntax-table () ...): a copy of the standard table.
        call copy_syntax_table(shell_script_mode_syntax_table, status)
        t = shell_script_mode_syntax_table
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", status, t)
        call modify_syntax_entry(10, 10, ">#", status, t)
        call modify_syntax_entry(iachar('"'), iachar('"'), """""", status, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """'", status, t)
        call modify_syntax_entry(iachar("`"), iachar("`"), """`", status, t)
        ! ?$ might also have a ". p" syntax. Both "'" and ". p" seem
        ! to work fine. This is needed so that dabbrev-expand
        ! $VARNAME works.
        call modify_syntax_entry(iachar("$"), iachar("$"), "'", status, t)
        call punct("!")
        call punct("%")
        call punct(":")
        call punct(".")
        call punct("^")
        call punct("~")
        call punct(",")
        call punct("=")
        call punct("/")
        call punct(";")
        call punct("|")
        call punct("&")
        call punct("<")
        call punct(">")
        allocate (shell_script_mode_syntax_table_input%entries(2))
        shell_script_mode_syntax_table_input%entries(1)%shell = "sh"
        allocate (shell_script_mode_syntax_table_input%entries(1)%changes(0))
        ! Treat ' as punctuation rather than a string delimiter, as RPM
        ! files often contain prose with apostrophes.
        shell_script_mode_syntax_table_input%entries(2)%shell = "rpm"
        shell_script_mode_syntax_table_input%entries(2)%table = shell_script_mode_syntax_table
        allocate (shell_script_mode_syntax_table_input%entries(2)%changes(1))
        shell_script_mode_syntax_table_input%entries(2)%changes(1)%char = iachar("'")
        shell_script_mode_syntax_table_input%entries(2)%changes(1)%syntax = "."

    contains

        subroutine punct(c)
            character(len=1), intent(in) :: c
            type(status_type) :: st

            call modify_syntax_entry(iachar(c), iachar(c), ".", st, t)
        end subroutine punct
    end subroutine initialize_syntax_tables

    ! The faces.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK

        f => shell_heredoc
        call f%define("shell_heredoc", "Face to show a here-document.")
        call f%specification_entry(display_when(classes=[C], minimum_colors=88, backgrounds=[DARK]), &
                                   face_attributes(foreground="yellow1", weight="bold"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[DARK]), face_attributes(foreground="yellow", &
                                                                                                  weight="bold"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attributes(foreground="tan1"))
        call f%specification_entry(display_when(), face_attributes(weight="bold"))
        f => shell_quoted_exec
        call f%define("shell_quoted_exec", "Face to show quoted execs like `blabla`.")
        call f%specification_entry(display_when(classes=[C], backgrounds=[DARK]), face_attributes(foreground="salmon"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attributes(foreground="magenta"))
        call f%specification_entry(display_when(), face_attributes(weight="bold"))
        f => shell_escaped_newline
        call f%define("shell_escaped_newline", "Face used for (non-escaped) backslash at end of a line in Shell-script mode.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_string_face"))
    end subroutine define_faces

    ! A highlight of subexpression N in FACE.  Not a port.
    function h(n, face, override, lax) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        integer, intent(in), optional :: override
        logical, intent(in), optional :: lax
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, override=override, laxmatch=lax)
    end function h

    ! `shell_script_font_lock_keywords_variable', `shell_script_font_lock_keywords_variable_1' and
    ! `shell_script_font_lock_keywords_variable_2'.  Not a port.
    subroutine initialize_font_lock_keywords()
        character(len=*), parameter :: VA = "font_lock_variable_name_face", KW = "font_lock_keyword_face", &
                                       FN = "font_lock_function_name_face", ST = "font_lock_string_face"
        type(font_lock_keyword_type) :: k1(1), k2(2), k3(3), k4(4)
        type(font_lock_keyword_type), allocatable :: none(:)
        type(font_lock_highlight_type) :: h1(1), h2(2)

        if (allocated(shell_script_font_lock_keywords_variable%entries)) return
        allocate (none(0))
        k1(1) = font_lock_keyword("\${?[#?]?\([[:alpha:]_][[:alnum:]_]*\|0\)", face=VA, subexp=1)
        call shell_script_font_lock_keywords_variable%append("csh", k1, "shell")
        k1(1) = font_lock_keyword("\$#?\([[:alpha:]_][[:alnum:]_]*\|[0-9]+\)", face=VA, subexp=1)
        call shell_script_font_lock_keywords_variable%append("es", k1, "executable_font_lock_keywords")
        call shell_script_font_lock_keywords_variable%append("rc", none, "es")
        h1(1) = h(1, "shell_quoted_exec", FONT_LOCK_OVERRIDE_REPLACE)
        k1(1) = font_lock_keyword("\$(\([^)"//LF//"]+\)", highlights=h1)
        call shell_script_font_lock_keywords_variable%append("bash", k1, "sh")
        ! Variable names.
        k4(1) = font_lock_keyword("\$\({#?\)?\([[:alpha:]_][[:alnum:]_]*\|[-#?@!]\)", face=VA, subexp=2)
        ! Function names.
        k4(2) = font_lock_keyword("^\(\sw+\)[ "//TAB//"]*(", face=FN, subexp=1)
        h2(1) = h(1, KW)
        h2(2) = h(2, FN, lax=.true.)
        k4(3) = font_lock_keyword("\<\(function\)\>[ "//TAB//"]*\(\sw+\)?", highlights=h2)
        k4(4) = font_lock_keyword("\(?:^\s *\|[[();&|]\s *\|\(?:\s +-[ao]\|if\|else\|then\|while\|do\)\s +\)\(!\)", &
                                  face="font_lock_negation_character_face", subexp=1)
        call shell_script_font_lock_keywords_variable%append("sh", k4, "shell")
        ! The next entry is only used for defining the others
        ! Using font-lock-string-face here confuses sh-get-indent-info.
        k3(1) = font_lock_keyword("\(^\|[^\]\)\(\\\\\)*\(\\\)$", face="shell_escaped_newline", subexp=3)
        k3(2) = font_lock_keyword("\\[^[:alnum:]]", face=ST, subexp=0)
        k3(3) = font_lock_keyword("\${?\([[:alpha:]_][[:alnum:]_]*\|[0-9]+\|[$*_]\)", face=VA, subexp=1)
        call shell_script_font_lock_keywords_variable%append("shell", k3)
        k2(1) = font_lock_keyword("^\s-*%\(\sw+\)", face=KW, subexp=1)
        k2(2) = font_lock_keyword("%{?\([!?]*[[:alpha:]_][[:alnum:]_]*\|[0-9]+\|[%*#]\*?\|!?-[[:alpha:]]\*?\)", &
                                  face=VA, subexp=1)
        call shell_script_font_lock_keywords_variable%append("rpm", k2, "rpm2")
        h1(1) = h(1, "font_lock_doc_face", FONT_LOCK_OVERRIDE_REPLACE)
        k2(1) = font_lock_keyword("^Summary:\(.*\)$", highlights=h1)
        h2(1) = h(1, VA)
        h2(2) = h(3, ST, lax=.true.)
        k2(2) = font_lock_keyword("^\(\sw+\)\((\(\sw+\))\)?:", highlights=h2)
        call shell_script_font_lock_keywords_variable%append("rpm2", k2, "shell")
        k1(1) = font_lock_keyword("[ "//TAB//"]\(in\|do\)\>")
        call shell_script_font_lock_keywords_variable_1%append("sh", k1)
        allocate (shell_script_font_lock_keywords_variable_2%entries(0))
    end subroutine initialize_font_lock_keywords

    ! Append the keywords B to A.  Not a port.
    subroutine append_keywords(a, b)
        type(font_lock_keyword_type), allocatable, intent(inout) :: a(:)
        type(font_lock_keyword_type), intent(in) :: b(:)
        type(font_lock_keyword_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(a)) allocate (a(0))
        n = size(a)
        allocate (grown(n + size(b)))

        do i = 1, n
            grown(i) = a(i)
        end do

        do i = 1, size(b)
            grown(n + i) = b(i)
        end do

        call move_alloc(grown, a)
    end subroutine append_keywords

    !> Function to get simple fontification based on `shell_script_font_lock_keywords'.
    !> This adds rules for comments and assignments.  Port of
    !> sh-script.el:sh-font-lock-keywords.
    subroutine shell_script_font_lock_keywords(result, keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: result(:)
        type(font_lock_keyword_type), intent(in), optional :: keywords(:)
        type(font_lock_keyword_type), allocatable :: list(:)
        type(font_lock_keyword_type) :: assignment(1)
        character(len=:), allocatable :: regular_expression

        call shell_script_font_lock_keywords_variable%get_current(list)
        call string_feature_value(shell_assignment_regular_expression, regular_expression)

        if (.not. allocated(regular_expression)) then
            call move_alloc(list, result)
            return
        end if

        assignment(1) = font_lock_keyword(regular_expression, face="font_lock_variable_name_face", subexp=1)
        call append_keywords(result, assignment)
        if (present(keywords)) call append_keywords(result, keywords)
        call append_keywords(result, list)
        if (allocated(executable_font_lock_keywords)) call append_keywords(result, executable_font_lock_keywords)
    end subroutine shell_script_font_lock_keywords

    !> Function to get better fontification including keywords.  Port of
    !> sh-script.el:sh-font-lock-keywords-1.
    subroutine shell_script_font_lock_keywords_1(result, builtins)
        type(font_lock_keyword_type), allocatable, intent(out) :: result(:)
        logical, intent(in), optional :: builtins
        type(font_lock_keyword_type), allocatable :: keywords(:), more(:)
        type(font_lock_keyword_type) :: one(1)
        type(font_lock_highlight_type) :: h2(2)
        character(len=:), allocatable :: regular_expression
        logical :: with_builtins

        with_builtins = .false.
        if (present(builtins)) with_builtins = builtins
        regular_expression = "\([;(){}`|&]\|^\)[ "//TAB//"]*\(\("//regular_expression_optimization(strings_of( &
                                                                                                   shell_leading_keywords), &
                                                                                          REGULAR_EXPRESSION_OPTIMIZATION_GROUP)// &
                     "[ "//TAB//"]+\)?"//regular_expression_optimization(leading_and_other(), REGULAR_EXPRESSION_OPTIMIZATION_GROUP)
        allocate (keywords(0))

        if (with_builtins) then
            h2(1) = h(2, "font_lock_keyword_face", lax=.true.)
            h2(2) = h(6, "font_lock_builtin_face")
            one(1) = font_lock_keyword(regular_expression//"[ "//TAB//"]+\)?"//regular_expression_optimization(strings_of( &
                                                                                                               shell_builtins), &
                                                                                     REGULAR_EXPRESSION_OPTIMIZATION_GROUP)//"\>", &
                                       highlights=h2)
            call append_keywords(keywords, one)
            call shell_script_font_lock_keywords_variable_2%get_current(more)
            call append_keywords(keywords, more)
        end if

        one(1) = font_lock_keyword(regular_expression//"\)\>", face="font_lock_keyword_face", subexp=2)
        call append_keywords(keywords, one)
        call shell_script_font_lock_keywords_variable_1%get_current(more)
        call append_keywords(keywords, more)
        call shell_script_font_lock_keywords(result, keywords)
    end subroutine shell_script_font_lock_keywords_1

    !> Function to get better fontification including keywords and builtins.
    !> Port of sh-script.el:sh-font-lock-keywords-2.
    subroutine shell_script_font_lock_keywords_2(result)
        type(font_lock_keyword_type), allocatable, intent(out) :: result(:)

        call shell_script_font_lock_keywords_1(result, .true.)
    end subroutine shell_script_font_lock_keywords_2

    ! The decoration levels of `font_lock_defaults'.  Not a port.
    subroutine keywords_level_0(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)

        call shell_script_font_lock_keywords(keywords)
    end subroutine keywords_level_0

    ! Not a port.
    subroutine keywords_level_1(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)

        call shell_script_font_lock_keywords_1(keywords)
    end subroutine keywords_level_1

    ! The strings of FEATURE for the current shell, as an array of
    ! characters.  Not a port.
    function strings_of(feature) result(a)
        type(shell_strings_feature_type), intent(in) :: feature
        character(len=:), allocatable :: a(:)
        type(string_type), allocatable :: s(:)

        call feature%get_current(s)
        a = to_characters(s)
    end function strings_of

    ! (append (sh-feature sh-leading-keywords) (sh-feature sh-other-keywords)).
    ! Not a port.
    function leading_and_other() result(a)
        character(len=:), allocatable :: a(:)
        type(string_type), allocatable :: s1(:), s2(:), s(:)
        integer :: i

        call shell_leading_keywords%get_current(s1)
        call shell_other_keywords%get_current(s2)
        allocate (s(size(s1) + size(s2)))

        do i = 1, size(s1)
            s(i)%text = s1(i)%text
        end do

        do i = 1, size(s2)
            s(size(s1) + i)%text = s2(i)%text
        end do

        a = to_characters(s)
    end function leading_and_other

    ! STRINGS as a character array.  Not a port.
    function to_characters(strings) result(a)
        type(string_type), intent(in) :: strings(:)
        character(len=:), allocatable :: a(:)
        integer :: i, n

        n = 1

        do i = 1, size(strings)
            n = max(n, len(strings(i)%text))
        end do

        allocate (character(len=n) :: a(size(strings)))

        do i = 1, size(strings)
            a(i) = strings(i)%text
        end do
    end function to_characters

    ! ------------------------------------------------------------------
    ! Feature tables
    ! ------------------------------------------------------------------

    !> The shell whose entry of a feature table with entries for SHELLS
    !> applies to SHELL: SHELL itself, else its successive ancestors (see
    !> `shell_ancestors'), finally sh (0 when there is none).  Port of
    !> sh-script.el:sh-feature (the lookup part).
    integer function shell_feature_shell(shells, shell) result(k)
        character(len=*), intent(in) :: shells(:)
        character(len=*), intent(in) :: shell
        character(len=:), allocatable :: s
        integer :: depth

        s = shell

        do depth = 1, 100
            k = find_shell(shells, s)
            if (k > 0) return
            s = ancestor_of(s)
            if (len(s) == 0) exit
        end do

        ! If the shell is not known, treat it as sh.
        k = find_shell(shells, "sh")
    end function shell_feature_shell

    ! The index of SHELL in SHELLS, 0 if none.  Not a port.
    integer function find_shell(shells, shell) result(k)
        character(len=*), intent(in) :: shells(:)
        character(len=*), intent(in) :: shell

        do k = 1, size(shells)
            if (trim(shells(k)) == shell) return
        end do

        k = 0
    end function find_shell

    ! The direct ancestor of SHELL ("" for none).  Not a port.
    function ancestor_of(shell) result(a)
        character(len=*), intent(in) :: shell
        character(len=:), allocatable :: a
        integer :: i

        a = ""
        if (.not. allocated(shell_ancestors)) return

        do i = 1, size(shell_ancestors)
            if (shell_ancestors(i)%shell == shell) then
                a = shell_ancestors(i)%ancestor
                return
            end if
        end do
    end function ancestor_of

    ! The current shell.  Not a port.
    function current_shell() result(s)
        character(len=:), allocatable :: s

        if (allocated(shell_shell%value)) then
            s = shell_shell%value
        else
            s = "sh"
        end if
    end function current_shell

    ! Not a port.
    subroutine strings_feature_append(self, shell, strings, append)
        class(shell_strings_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        character(len=*), intent(in) :: strings(:)
        character(len=*), intent(in), optional :: append
        type(shell_strings_entry_type) :: entry
        integer :: i

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%shell = shell
        if (present(append)) entry%append = append
        allocate (entry%strings(size(strings)))

        do i = 1, size(strings)
            entry%strings(i)%text = trim(strings(i))
        end do

        self%entries = [self%entries, entry]
    end subroutine strings_feature_append

    ! The shells of the entries of a strings feature.  Not a port.
    function strings_shells(self) result(shells)
        class(shell_strings_feature_type), intent(in) :: self
        character(len=16), allocatable :: shells(:)
        integer :: i

        allocate (shells(size(self%entries)))

        do i = 1, size(self%entries)
            shells(i) = self%entries(i)%shell
        end do
    end function strings_shells

    !> Index the table by the current shell (see `shell_feature_shell'); an
    !> entry with sh-append is followed by the value for the shell it
    !> names.  Port of sh-script.el:sh-feature.
    recursive subroutine current_strings_feature(self, strings, shell)
        class(shell_strings_feature_type), intent(in) :: self
        type(string_type), allocatable, intent(out) :: strings(:)
        character(len=*), intent(in), optional :: shell
        type(string_type), allocatable :: rest(:)
        integer :: k, i, n

        allocate (strings(0))
        if (.not. allocated(self%entries)) return

        if (present(shell)) then
            k = shell_feature_shell(strings_shells(self), shell)
        else
            k = shell_feature_shell(strings_shells(self), current_shell())
        end if

        if (k == 0) return
        n = size(self%entries(k)%strings)

        if (allocated(self%entries(k)%append)) then
            call current_strings_feature(self, rest, self%entries(k)%append)
        else
            allocate (rest(0))
        end if

        deallocate (strings)
        allocate (strings(n + size(rest)))

        do i = 1, n
            strings(i)%text = self%entries(k)%strings(i)%text
        end do

        do i = 1, size(rest)
            strings(n + i)%text = rest(i)%text
        end do
    end subroutine current_strings_feature

    ! Not a port.
    subroutine keywords_feature_append(self, shell, keywords, append)
        class(shell_keywords_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        character(len=*), intent(in), optional :: append
        type(shell_keywords_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%shell = shell
        if (present(append)) entry%append = append
        call append_keywords(entry%keywords, keywords)
        self%entries = [self%entries, entry]
    end subroutine keywords_feature_append

    !> Index the table by the current shell, as `current_strings_feature'.
    !> Port of sh-script.el:sh-feature.
    recursive subroutine current_keywords_feature(self, keywords, shell)
        class(shell_keywords_feature_type), intent(in) :: self
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        character(len=*), intent(in), optional :: shell
        type(font_lock_keyword_type), allocatable :: rest(:)
        character(len=16), allocatable :: shells(:)
        integer :: k, i

        allocate (keywords(0))
        if (.not. allocated(self%entries)) return
        allocate (shells(size(self%entries)))

        do i = 1, size(self%entries)
            shells(i) = self%entries(i)%shell
        end do

        if (present(shell)) then
            k = shell_feature_shell(shells, shell)
        else
            k = shell_feature_shell(shells, current_shell())
        end if

        if (k == 0) return
        call append_keywords(keywords, self%entries(k)%keywords)

        if (allocated(self%entries(k)%append)) then
            call current_keywords_feature(self, rest, self%entries(k)%append)
            call append_keywords(keywords, rest)
        end if
    end subroutine current_keywords_feature

    ! Not a port.
    subroutine string_feature_append(self, shell, value)
        class(shell_string_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        character(len=*), intent(in), optional :: value
        type(shell_string_entry_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%shell = shell
        if (present(value)) entry%value = value
        self%entries = [self%entries, entry]
    end subroutine string_feature_append

    !> Index the table by the current shell.  Port of
    !> sh-script.el:sh-feature.
    function current_string_feature(self) result(value)
        class(shell_string_feature_type), intent(in) :: self
        character(len=:), allocatable :: value
        character(len=16), allocatable :: shells(:)
        integer :: k, i

        if (.not. allocated(self%entries)) return
        allocate (shells(size(self%entries)))

        do i = 1, size(self%entries)
            shells(i) = self%entries(i)%shell
        end do

        k = shell_feature_shell(shells, current_shell())
        if (k == 0) return
        if (allocated(self%entries(k)%value)) value = self%entries(k)%value
    end function current_string_feature

    ! The value of the string feature table FEATURE for the current shell,
    ! in VALUE (unallocated for none): an unallocated function result
    ! cannot be assigned.  Not a port.
    subroutine string_feature_value(feature, value)
        type(shell_string_feature_type), intent(in) :: feature
        character(len=:), allocatable, intent(out) :: value
        character(len=16), allocatable :: shells(:)
        integer :: k, i

        if (.not. allocated(feature%entries)) return
        allocate (shells(size(feature%entries)))

        do i = 1, size(feature%entries)
            shells(i) = feature%entries(i)%shell
        end do

        k = shell_feature_shell(shells, current_shell())
        if (k == 0) return
        if (allocated(feature%entries(k)%value)) value = feature%entries(k)%value
    end subroutine string_feature_value

    ! The entry of `shell_script_mode_syntax_table_input' for the current shell (0
    ! for none).  Not a port.
    integer function syntax_input_index() result(k)
        character(len=16), allocatable :: shells(:)
        integer :: i

        k = 0
        if (.not. allocated(shell_script_mode_syntax_table_input%entries)) return
        allocate (shells(size(shell_script_mode_syntax_table_input%entries)))

        do i = 1, size(shells)
            shells(i) = shell_script_mode_syntax_table_input%entries(i)%shell
        end do

        k = shell_feature_shell(shells, current_shell())
    end function syntax_input_index

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals' (0 if none).
    ! Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, dim=1)
    end function local_index

    ! Find or make the current buffer's entry in `locals', reusing the
    ! entry of a closed buffer; K is its index (0 without a current
    ! buffer).  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(shell_local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) then
            locals = [locals, entry]
        else
            call clear_local(k)
        end if

        locals(k) = shell_local_type(serial=buffers(current)%serial)
    end subroutine make_local_entry

    ! Free the objects of locals(K).  Not a port.
    subroutine clear_local(k)
        integer, intent(in) :: k
        type(status_type) :: st

        if (locals(k)%has_header) call destroy_marker(locals(k)%header_marker, st)
        if (locals(k)%has_table) call destroy_syntax_table(locals(k)%table, st)
        locals(k)%has_header = .false.
        locals(k)%has_table = .false.
    end subroutine clear_local

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine clear_shell_script_locals()
        integer :: k

        k = local_index()
        if (k == 0) return
        call clear_local(k)
        locals(k) = shell_local_type()
    end subroutine clear_shell_script_locals

    !> The current buffer's `shell_indent_supported_here': the indentation
    !> rule set ("sh" or "rc"), "" if we don't support indentation for its
    !> shell type.  Port of sh-script.el sh-indent-supported-here.
    function shell_indent_supported_here() result(s)
        character(len=:), allocatable :: s
        integer :: k

        s = ""
        k = local_index()
        if (k == 0) return
        if (locals(k)%has_indent_supported) s = locals(k)%indent_supported
    end function shell_indent_supported_here

    !> The current buffer's `shell_shell_variables': the shell variable names
    !> that should be included in completion.  These are used for
    !> completion in addition to all the variables named in
    !> `process_environment'.  Port of sh-script.el sh-shell-variables.
    subroutine shell_shell_variables(names)
        type(string_type), allocatable, intent(out) :: names(:)
        integer :: k

        k = local_index()

        if (k > 0) then
            if (locals(k)%has_variables) then
                names = locals(k)%variables
                return
            end if
        end if

        names = default_variables
    end subroutine shell_shell_variables

    ! ------------------------------------------------------------------
    ! Syntax properties
    ! ------------------------------------------------------------------

    ! The position of marker M.  Not a port.
    integer(int64) function marked_position(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, position, st)
    end function marked_position

    ! (char-after POSITION), -1 for nil.  Not a port.
    integer function ca(position) result(c)
        integer(int64), intent(in) :: position
        integer(int32) :: ch
        logical :: found

        call character_after(ch, found, position)
        c = -1
        if (found) c = ch
    end function ca

    ! (char-before POSITION), -1 for nil.  Not a port.
    integer function cb(position) result(c)
        integer(int64), intent(in) :: position
        integer(int32) :: ch
        logical :: found

        call character_before(ch, found, position)
        c = -1
        if (found) c = ch
    end function cb

    ! Put the `syntax_table' property SYNTAX (a descriptor string) on
    ! START..END.  Not a port.
    subroutine put_syntax(start, end, syntax)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: syntax
        type(syntax_descriptor_type) :: d
        type(status_type) :: st

        call string_to_syntax(syntax, d, st)
        call put_text_property(start, end, value_type(name="syntax_table"), syntax_descriptor_value(d), st)
    end subroutine put_syntax

    ! Mark START..END to be refontified together (syntax-multiline).
    ! Not a port.
    subroutine put_multiline(start, end)
        integer(int64), intent(in) :: start, end
        type(status_type) :: st

        call put_text_property(start, end, value_type(name="syntax_multiline"), value_type(.true.), st)
    end subroutine put_multiline

    ! (nth 1 (syntax-ppss POSITION)) and the like.  Not a port.
    function parser_state(position) result(state)
        integer(int64), intent(in), optional :: position
        type(parse_state_type) :: state

        call syntax_parser_state(state, position)
    end function parser_state

    ! Port of sh-script.el:sh--inside-noncommand-expression.
    recursive logical function shell_inside_noncommand_expression(position) result(inside)
        integer(int64), intent(in) :: position
        type(parse_state_type) :: state
        integer(int64) :: here
        integer :: c

        inside = .false.
        here = point()
        state = parser_state(position)

        if (state%innermost_start /= 0) then
            call goto_character(state%innermost_start)
            c = ca(point())

            if (c == iachar("(")) then
                ! ((...)) or $((...)) or $[...] or ${...}. Nested
                ! parenthesis can occur inside the first of these forms, so
                ! parse backward recursively.
                inside = cb(point()) == iachar("(")
            else if (c == iachar("{") .or. c == iachar("[")) then
                inside = cb(point()) == iachar("$")
            end if

            if (.not. inside) inside = shell_inside_noncommand_expression(point() - 1)
        end if

        call goto_character(here)
    end function shell_inside_noncommand_expression

    ! Determine the syntax of the \n after a <<EOF.
    ! START is the position of <<.
    ! STRING is the actual word used as delimiter (e.g. "EOF").
    ! Point is at the beginning of the next line.  PROPERTY gets the syntax.
    ! Port of sh-script.el:sh-font-lock-open-heredoc.
    recursive subroutine shell_font_lock_open_heredoc(start, string, end_of_line, property)
        integer(int64), intent(in) :: start, end_of_line
        character(len=*), intent(in) :: string
        type(syntax_property_type), intent(out) :: property
        type(parse_state_type) :: state
        type(status_type) :: st
        character(len=:), allocatable :: piece
        integer(int64) :: here
        integer :: c, i

        c = cb(start)
        if (c == iachar("<") .or. c == iachar(">")) return
        if (shell_in_comment_or_string(start)) return
        if (shell_inside_noncommand_expression(start)) return
        ! We're looking at <<STRING, so we add "^STRING$" to the syntactic
        ! font-lock keywords to detect the end of this here document.
        piece = ""

        do i = 1, len(string)
            if (string(i:i) /= "'" .and. string(i:i) /= '"') piece = piece//string(i:i)
        end do

        here = point()
        state = parser_state(end_of_line)
        call goto_character(here)
        ! The \n not only starts the heredoc but also closes a comment.
        ! Let's close the comment just before the \n.
        if (state%incomment /= 0) call put_syntax(end_of_line - 1, end_of_line, ">")
        ! If the sh-escaped-line-re part of sh-here-doc-open-re has matched
        ! several lines, make sure we refontify them together.
        ! Furthermore, if (nth 5 ppss) is set (i.e. the \n is
        ! escaped), it means the right \n is actually further down.
        ! Don't bother fixing it now, but place a multiline property so
        ! that when jit-lock-context-* refontifies the rest of the
        ! buffer, it also refontifies the current line with it.
        if (state%quoted .or. count_lines(start, end_of_line) > 1) call put_multiline(start, end_of_line + 1)
        call put_text_property(end_of_line, end_of_line + 1, value_type(name="shell_here_doc_marker"), value_type(text=piece), st)
        property%apply = .true.
        call string_to_syntax("|", property%descriptor, st)
        call goto_character(start + 2)
    end subroutine shell_font_lock_open_heredoc

    ! Port of sh-script.el:sh-syntax-propertize-here-doc.
    recursive subroutine shell_syntax_propertize_here_doc(end)
        integer(int64), intent(in) :: end
        type(parse_state_type) :: state
        type(value_type) :: key
        type(status_type) :: st
        character(len=:), allocatable :: k
        integer(int64) :: found, end_of_line
        logical :: saved

        state = parser_state()
        if (state%instring /= ST_STRING_STYLE) return
        call get_text_property(state%comstr_start, value_type(name="shell_here_doc_marker"), key, st)
        k = ""
        if (vtag(key) == V_STRING) k = xstring(key)
        saved = case_fold_search%value
        case_fold_search%value = .false.
        call regular_expression_search_forward("^\([ "//TAB//"]*\)"//regular_expression_quote(k)//"\("//LF//"\)", found, st, &
                                               bound=end, &
                                               noerror=SEARCH_MOVE)
        case_fold_search%value = saved

        if (found /= 0) then
            end_of_line = match_beginning(2, st)
            call put_syntax(end_of_line, end_of_line + 1, "|")
        end if
    end subroutine shell_syntax_propertize_here_doc

    ! Search for a subshell embedded in a string.
    ! Find all the unescaped " characters within said subshell, remembering that
    ! subshells can nest.  Port of sh-script.el:sh-font-lock-quoted-subshell.
    recursive subroutine shell_font_lock_quoted_subshell(limit, status)
        integer(int64), intent(in) :: limit
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state0
        integer, allocatable :: states(:)
        integer(int64) :: startpos, spos, epos, found, moved
        integer :: state, c

        status = status_type()
        state0 = parser_state()
        ! Check we matched an opening quote.
        if (state0%instring /= iachar('"')) return

        ! bingo we have a $( or a ` inside a ""
        ! `state' can be: double-quote, backquote, code.
        if (cb(point()) == iachar("`")) then
            state = QS_BACKQUOTE
        else
            state = QS_CODE
        end if

        startpos = point()
        ! Stacked states in the context.
        states = [QS_DOUBLE_QUOTE]

        do while (state /= QS_NONE)
            moved = skip_characters_forward("^'\""`$()", status, limit)
            if (.not. (point() < limit)) exit
            c = ca(point())

            ! unescape " inside a $( ... ) construct.
            select case (c)
            case (iachar("'"))
                if (state /= QS_DOUBLE_QUOTE) then
                    call forward_character(status, 1_int64)
                    ! FIXME: mark skipped double quotes as punctuation syntax.
                    spos = point()
                    moved = skip_characters_forward("^'", status, limit)
                    epos = point()
                    call goto_character(spos)

                    do
                        call search_forward('"', found, status, bound=epos, noerror=SEARCH_NOERROR)
                        if (found == 0) exit
                        call put_syntax(point() - 1, point(), ".")
                    end do

                    call goto_character(epos)
                end if
            case (iachar("\"))
                call forward_character(status, 1_int64)
            case (iachar('"'))
                if (state == QS_DOUBLE_QUOTE) then
                    call pop(state)
                else
                    states = [state, states]
                    state = QS_DOUBLE_QUOTE
                end if

                if (state /= QS_NONE) call put_syntax(point(), point() + 1, ".")
            case (iachar("`"))
                if (state == QS_BACKQUOTE) then
                    call pop(state)
                else
                    states = [state, states]
                    state = QS_BACKQUOTE
                end if
            case (iachar("$"))
                if (ca(point() + 1) == iachar("(")) then
                    call forward_character(status, 1_int64)
                    states = [state, states]
                    state = QS_CODE
                end if
            case (iachar("("))
                if (state /= QS_DOUBLE_QUOTE) then
                    states = [state, states]
                    state = QS_CODE
                end if
            case (iachar(")"))
                if (state /= QS_DOUBLE_QUOTE) call pop(state)
            case default
                call set_error(status, GENERIC_ERROR_CODE, "Internal error in sh-font-lock-quoted-subshell")
                return
            end select

            if (.not. status%ok()) return
            call forward_character(status, 1_int64)
            if (.not. status%ok()) return
        end do

        if (startpos < line_beginning_position()) then
            call put_multiline(startpos, point())
            call syntax_propertize_extend_region_functions%prepend(syntax_propertize_multiline, local=.true.)
        end if

    contains

        subroutine pop(s)
            integer, intent(out) :: s

            if (size(states) == 0) then
                s = QS_NONE
            else
                s = states(1)
                states = states(2:)
            end if
        end subroutine pop
    end subroutine shell_font_lock_quoted_subshell

    !> Whether the character before POSITION is an unquoted backslash.  Port of
    !> sh-script.el:sh-is-quoted-p.
    recursive logical function is_shell_backslash_quoted(position) result(quoted)
        integer(int64), intent(in) :: position

        quoted = .false.
        if (cb(position) == iachar("\")) quoted = .not. is_shell_backslash_quoted(position - 1)
    end function is_shell_backslash_quoted

    ! Port of sh-script.el:sh-font-lock-paren: the syntax of the `)' at
    ! START (PROPERTY), punctuation in a case pattern.
    recursive subroutine shell_font_lock_parenthesis(start, property)
        integer(int64), intent(in) :: start
        type(syntax_property_type), intent(out) :: property
        type(parse_state_type) :: state
        type(status_type) :: st
        integer(int64) :: here, open
        logical :: again, ok

        state = parser_state()
        if (state%comstr_start /= 0) return
        here = point()
        open = 0
        call goto_character(start)

        ! Skip through all patterns
        do
            do
                ok = forward_comment(-point_maximum())

                if (is_end_of_line() .and. is_shell_backslash_quoted(point())) then
                    call forward_character(st, -1_int64)
                    cycle
                end if

                exit
            end do

            ! Skip through one pattern
            do
                if (skip_syntax_backward("w_") /= 0) cycle
                if (skip_characters_backward("-$=?[]*@/\\!%:.^~,", st) /= 0) cycle

                if (is_shell_backslash_quoted(point() - 1)) then
                    call goto_character(point() - 2)
                    cycle
                end if

                if (cb(point()) == iachar('"') .or. cb(point()) == iachar("'") .or. cb(point()) == iachar("}")) then
                    call plain_forward_expression(-1_int64, st)
                    if (st%ok()) cycle
                end if

                exit
            end do

            ! Patterns can be preceded by an open-paren (bug#1320).
            if (cb(point()) == iachar("(")) then
                call forward_character(st, -1_int64)
                open = point()
            end if

            do
                ok = forward_comment(-point_maximum())
                ! Maybe we've bumped into an escaped newline.
                if (.not. is_shell_backslash_quoted(point())) exit
                call forward_character(st, -1_int64)
            end do

            again = .false.

            if (cb(point()) == iachar("|")) then
                if (cb(point() - 1) /= iachar(";")) then
                    call forward_character(st, -1_int64)
                    again = .true.
                end if
            end if

            if (.not. again) exit
        end do

        if (point() > point_minimum() + 1) then
            call forward_character(st, -2_int64)
            if (start > line_end_position()) call put_multiline(point(), start + 1)

            ! FIXME: The `in' may just be a random argument to
            ! a normal command rather than the real `in' keyword.
            ! I.e. we should look back to try and find the
            ! corresponding `case'.
            ! Also recognize OpenBSD's case X { ... } (bug#55764).
            if (looking_at(";\(?:;&?\|[&|]\)\|\_<in\|.{", st)) then
                ! ";; esac )" is a case that looks
                ! like a case-pattern but it's really just a close
                ! paren after a case statement.  I.e. if we skipped
                ! over `esac' just now, we're not looking
                ! at a case-pattern.
                if (.not. looking_at("..[ "//TAB//LF//"]+esac[^[:word:]_]", st)) then
                    if (open /= 0) call put_syntax(open, open + 1, ".")
                    property%apply = .true.
                    call string_to_syntax(".", property%descriptor, st)
                end if
            end if
        end if

        call goto_character(here)
    end subroutine shell_font_lock_parenthesis

    ! (let ((forward-sexp-function nil)) (forward-sexp N)).  Not a port.
    recursive subroutine plain_forward_expression(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call forward_expression_default_function(n, status)
    end subroutine plain_forward_expression

    ! Port of sh-script.el:sh-font-lock-backslash-quote.
    recursive subroutine backslash_quote_value(property, status)
        type(syntax_property_type), intent(out) :: property
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer(int64) :: here

        here = point()
        state = parser_state(match_beginning(0, status))
        call goto_character(here)

        ! In a '...' the backslash is not escaping.
        if (state%instring == iachar("'")) then
            property%apply = .true.
            call string_to_syntax(".", property%descriptor, status)
        end if

        status = status_type()
    end subroutine backslash_quote_value

    ! (2 (sh-font-lock-open-heredoc (1+ (match-beginning 0)) (match-string 1) (match-beginning 2))).
    ! Not a port.
    recursive subroutine open_heredoc_value(property, status)
        type(syntax_property_type), intent(out) :: property
        type(status_type), intent(out) :: status
        integer(int64) :: start, end_of_line
        character(len=:), allocatable :: word

        start = match_beginning(0, status) + 1
        word = match_string(1)
        end_of_line = match_beginning(2, status)
        status = status_type()
        call shell_font_lock_open_heredoc(start, word, end_of_line, property)
    end subroutine open_heredoc_value

    ! (0 (prog1 nil (sh-syntax-propertize-here-doc end))).  Not a port.
    recursive subroutine here_doc_after(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call shell_syntax_propertize_here_doc(propertize_end)
    end subroutine here_doc_after

    ! (0 (sh-font-lock-paren (match-beginning 0))).  Not a port.
    recursive subroutine parenthesis_value(property, status)
        type(syntax_property_type), intent(out) :: property
        type(status_type), intent(out) :: status
        integer(int64) :: mb

        mb = match_beginning(0, status)
        status = status_type()
        call shell_font_lock_parenthesis(mb, property)
    end subroutine parenthesis_value

    ! Highlight (possibly nested) subshells inside "" quoted regions
    ! correctly.  Not a port.
    recursive subroutine quoted_subshell_after(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer(int64) :: here, mb

        status = status_type()
        here = point()
        mb = match_beginning(0, status)
        state = parser_state(mb)
        call goto_character(here)

        if (state%comstr_start /= 0) then
            call goto_character(mb + 1)
        else
            ! Save excursion because we want to also apply other
            ! syntax-propertize rules within the affected region.
            call shell_font_lock_quoted_subshell(propertize_end, status)
            call goto_character(here)
        end if
    end subroutine quoted_subshell_after

    ! The rules of `shell_syntax_propertize_function'.  Not a port.
    subroutine initialize_rules()
        type(syntax_highlight_type) :: hl
        character(len=*), parameter :: META = "|&;<>(`\""' "//TAB//LF

        if (allocated(propertize_rules%rules)) return
        allocate (propertize_rules%rules(9))
        propertize_rules%rules(1)%regular_expression = shell_here_doc_open_regular_expression
        hl = syntax_highlight_type(group=2)
        hl%value => open_heredoc_value
        propertize_rules%rules(1)%highlights = [hl]
        propertize_rules%rules(2)%regular_expression = "\s|"
        hl = syntax_highlight_type(group=0)
        hl%after => here_doc_after
        propertize_rules%rules(2)%highlights = [hl]
        ! A `#' begins a comment when it is unquoted and at the
        ! beginning of a word.  In the shell, words are separated by
        ! metacharacters.  The list of special chars is taken from
        ! the single-unix spec of the shell command language (under
        ! `quoting') but with `$' removed.  Also -- if there's something like
        ! \ #foo, then that's not a comment, unless the backslash itself
        ! is backslashed.
        propertize_rules%rules(3)%regular_expression = "\(?:[^"//META//"]\|\${\|\(?:[^\]\|^\)\\\(?:\\\\\)*.\)\(#+\)"
        hl = syntax_highlight_type(group=1, syntax="_")
        propertize_rules%rules(3)%highlights = [hl]
        ! In addition, `#' at the beginning of closed parentheses
        ! does not start a comment if the parentheses are not isolated
        ! by metacharacters, excluding [()].
        ! (e.g. `foo(#q/)' and `(#b)foo' in zsh)
        propertize_rules%rules(4)%regular_expression = "[^"//META//"](\(#+\)"
        propertize_rules%rules(4)%highlights = [hl]
        propertize_rules%rules(5)%regular_expression = "(\(#\)[^)]+?)[^|&;<>)`\""' "//TAB//LF//"]"
        propertize_rules%rules(5)%highlights = [hl]
        ! In a '...' the backslash is not escaping.
        propertize_rules%rules(6)%regular_expression = "\(\\\)'"
        hl = syntax_highlight_type(group=1)
        hl%value => backslash_quote_value
        propertize_rules%rules(6)%highlights = [hl]
        ! Make sure $@ and $? are correctly recognized as expressions.
        propertize_rules%rules(7)%regular_expression = "\$\([?@]\)"
        hl = syntax_highlight_type(group=1, syntax="_")
        propertize_rules%rules(7)%highlights = [hl]
        ! Distinguish the special close-paren in `case'.
        propertize_rules%rules(8)%regular_expression = ")"
        hl = syntax_highlight_type(group=0)
        hl%value => parenthesis_value
        propertize_rules%rules(8)%highlights = [hl]
        ! Highlight (possibly nested) subshells inside "" quoted
        ! regions correctly.
        propertize_rules%rules(9)%regular_expression = """\(?:[^\""]\|\\.\)*?\(\$(\|`\)"
        hl = syntax_highlight_type(group=1)
        hl%after => quoted_subshell_after
        propertize_rules%rules(9)%highlights = [hl]
    end subroutine initialize_rules

    !> The `syntax_propertize_function' of Shell-script mode.  Port of
    !> sh-script.el:sh-syntax-propertize-function.
    recursive subroutine shell_syntax_propertize_function(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        status = status_type()
        saved = propertize_end
        propertize_end = end
        call goto_character(start)
        call shell_syntax_propertize_here_doc(end)
        call propertize_rules%apply(point(), end, status)
        propertize_end = saved
    end subroutine shell_syntax_propertize_function

    !> The `font_lock_syntactic_face_function' of Shell-script mode.
    !> Port of sh-script.el:sh-font-lock-syntactic-face-function.
    subroutine shell_font_lock_syntactic_face_function(state, face)
        type(parse_state_type), intent(in) :: state
        character(len=:), allocatable, intent(out) :: face

        if (state%instring /= -1) then
            if (state%instring /= ST_STRING_STYLE) then
                if (state%instring == iachar("`")) then
                    face = "shell_quoted_exec"
                else
                    face = "font_lock_string_face"
                end if
            else
                face = "shell_heredoc"
            end if
        else
            face = "font_lock_comment_face"
        end if
    end subroutine shell_font_lock_syntactic_face_function

    !> Return true if START is in a comment or string.  Port of
    !> sh-script.el:sh-in-comment-or-string.
    recursive logical function shell_in_comment_or_string(start) result(in)
        integer(int64), intent(in) :: start
        type(parse_state_type) :: state
        integer(int64) :: here

        here = point()
        state = parser_state(start)
        call goto_character(here)
        in = state%instring /= -1 .or. state%incomment /= 0
    end function shell_in_comment_or_string

    ! ------------------------------------------------------------------
    ! Indentation and navigation with SIMPLE_MINDED_INDENTATION_ENGINE
    ! ------------------------------------------------------------------

    ! (zerop (length TOKEN)).  Not a port.
    logical function empty(token)
        character(len=:), allocatable, intent(in) :: token

        empty = .true.
        if (allocated(token)) empty = len(token) == 0
    end function empty

    ! (assoc TOKEN smie-grammar).  Not a port.
    logical function in_grammar(token)
        character(len=:), allocatable, intent(in) :: token
        type(simple_minded_indentation_engine_toklevels_type) :: t

        in_grammar = .false.
        if (.not. allocated(token)) return
        t = simple_minded_indentation_engine_grammar_token(token)
        in_grammar = .not. t%null
    end function in_grammar

    ! (string-match "\\`[a-z]" TOKEN).  Not a port.
    logical function starts_lower(token)
        character(len=:), allocatable, intent(in) :: token

        starts_lower = .false.
        if (.not. allocated(token)) return
        if (len(token) == 0) return
        starts_lower = token(1:1) >= "a" .and. token(1:1) <= "z"
    end function starts_lower

    ! Whether TOKEN is allocated and equal to S.  Not a port.
    logical function is(token, s)
        character(len=:), allocatable, intent(in) :: token
        character(len=*), intent(in) :: s

        is = .false.
        if (allocated(token)) is = token == s .and. len(token) == len(s)
    end function is

    ! True if we're at a keyword position.
    ! A keyword position is one where if we're looking at something that looks
    ! like a keyword, then it is a keyword.  Port of
    ! sh-script.el:sh-smie--keyword-p.
    recursive logical function is_shell_indentation_engine_keyword() result(p)
        character(len=:), allocatable :: previous
        type(status_type) :: st

        call simple_minded_indentation_engine_backward_token(previous, st)

        if (empty(previous)) then
            p = looking_back("\`\|\s(", st, point() - 1)
        else
            p = in_grammar(previous)
        end if
    end function is_shell_indentation_engine_keyword

    ! Return .true. if a newline should be treated as a semi-colon.
    ! Here we assume that a newline should be treated as a semi-colon unless it
    ! comes right after a special keyword.
    ! This function does not pay attention to line-continuations.
    ! If TOKEN is absent, point should be before the newline; otherwise, TOKEN is the token
    ! before the newline and in that case point should be just before the token.
    ! Port of sh-script.el:sh-smie--newline-semi-p.
    recursive logical function is_shell_indentation_engine_newline_semi(token_in) result(semi)
        character(len=:), allocatable, intent(in), optional :: token_in
        character(len=:), allocatable :: token
        type(simple_minded_indentation_engine_toklevels_type) :: t
        type(status_type) :: st
        integer(int64) :: here

        here = point()

        if (present(token_in)) then
            if (allocated(token_in)) token = token_in
        end if

        if (.not. allocated(token)) call simple_minded_indentation_engine_backward_token(token, st)

        if (empty(token) .and. looking_back("\s(", st, point() - 1)) then
            semi = .false.
        else
            semi = .true.

            if (allocated(token)) then
                t = simple_minded_indentation_engine_grammar_token(token)
                if (.not. t%null) semi = t%right%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER
            end if
        end if

        call goto_character(here)
    end function is_shell_indentation_engine_newline_semi

    ! When looking at TOKEN (either "in" or "do"), .true. if TOKEN is a keyword.
    ! Does not preserve point.  Port of sh-script.el:sh-smie--sh-keyword-in/do-p.
    recursive logical function is_shell_indentation_engine_shell_keyword_in_do(token) result(keyword)
        character(len=*), intent(in) :: token
        integer, parameter :: UNDECIDED = 0, WORD = 1, KEYWORD_RES = 2, UNKNOWN = 3
        character(len=:), allocatable :: previous
        type(status_type) :: st
        logical :: words, newline
        integer :: res

        words = .false. ! We've seen words.
        newline = .false. ! We've seen newlines after the words.
        res = UNDECIDED

        do while (res == UNDECIDED)
            call simple_minded_indentation_engine_backward_token(previous, st)

            if (empty(previous)) then
                if (newline) then
                    res = WORD
                else if (is_beginning_of_buffer()) then
                    res = WORD
                else
                    words = .true.
                    call plain_forward_expression(-1_int64, st)
                    if (.not. st%ok()) res = UNKNOWN
                end if
            else if (previous == ";") then
                if (words) then
                    newline = .true.
                else
                    res = KEYWORD_RES
                end if
            else if (previous == "for" .or. previous == "select" .or. (token == "in" .and. previous == "case")) then
                res = KEYWORD_RES
            else if (in_grammar(previous)) then
                res = WORD
            else
                if (newline) then
                    res = WORD
                else
                    words = .true.
                end if
            end if
        end do

        keyword = res == KEYWORD_RES
    end function is_shell_indentation_engine_shell_keyword_in_do

    ! True if TOKEN (at which we're looking) really is a keyword.  Port of
    ! sh-script.el:sh-smie--sh-keyword-p.
    recursive logical function is_shell_indentation_engine_shell_keyword(token) result(p)
        character(len=*), intent(in) :: token
        type(status_type) :: st

        if (looking_at("[[:alnum:]_]+=", st)) then
            p = .false.
        else if (token == "in" .or. token == "do") then
            p = is_shell_indentation_engine_shell_keyword_in_do(token)
        else
            p = is_shell_indentation_engine_keyword()
        end if
    end function is_shell_indentation_engine_shell_keyword

    !> Port of sh-script.el:sh-smie--default-forward-token.
    recursive subroutine shell_indentation_engine_default_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: start, moved
        logical :: ok

        status = status_type()
        ok = forward_comment(point_maximum())
        start = point()

        if (skip_syntax_forward(".") == 0) then
            do
                moved = skip_syntax_forward("w_'")
                if (.not. looking_at("\\", status)) exit
                call forward_character(status, 2_int64)
                if (.not. status%ok()) return
            end do
        end if

        call buffer_substring(start, point(), token, status)
    end subroutine shell_indentation_engine_default_forward_token

    !> Port of sh-script.el:sh-smie--default-backward-token.
    recursive subroutine shell_indentation_engine_default_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: position, n, p, moved
        logical :: ok, words, more

        status = status_type()
        ok = forward_comment(-point())
        position = point()
        n = skip_syntax_backward(".")
        words = n == 0

        if (.not. words .and. n == -1) then
            ! Skip past quoted white space.
            p = point()

            if (mod(skip_syntax_backward("\"), 2_int64) == -1) then
                words = .true.
            else
                call goto_character(p)
            end if
        end if

        if (words) then
            do
                ! Skip past words, but stop at semicolons.
                do
                    if (skip_syntax_backward("w_'") == 0) exit
                    if (cb(point()) == iachar(";")) exit
                    moved = skip_syntax_backward(".")
                end do

                more = skip_syntax_backward("\") /= 0

                if (.not. more) then
                    ! Skip past quoted white space.
                    if (cb(point() - 1) == iachar("\")) then
                        p = point()
                        call forward_character(status, -1_int64)

                        if (mod(skip_syntax_backward("\"), 2_int64) == -1) then
                            more = .true.
                        else
                            call goto_character(p)
                        end if
                    end if
                end if

                if (.not. more) exit
            end do
        else
            p = point()
            call goto_character(p - mod(skip_syntax_backward("\"), 2_int64))
        end if

        status = status_type()
        call buffer_substring(point(), position, token, status)
    end subroutine shell_indentation_engine_default_backward_token

    !> The `simple_minded_indentation_engine_forward_token_function' of sh.  Port of
    !> sh-script.el:sh-smie-sh-forward-token.
    recursive subroutine shell_indentation_engine_shell_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token_text
        type(parse_state_type) :: state
        integer(int64) :: position, here, moved, me1
        logical :: ok, semi, hanging

        status = status_type()
        hanging = looking_at("[ "//TAB//"]*\(?:#\|\(\s|\)\|$\)", status)

        if (hanging) then
            me1 = match_end(1, status)
            here = point()
            moved = skip_characters_backward(" "//TAB, status)
            hanging = .not. is_beginning_of_line()
            call goto_character(here)
        end if

        if (hanging) then
            ok = .false.

            if (me1 /= 0) then
                state = parser_state()
                ok = state%instring == -1
            end if

            if (ok) then
                ! Right before a here-doc.
                call plain_forward_expression(1_int64, status)
                if (.not. status%ok()) return
                ! Pretend the here-document is a "newline representing a
                ! semi-colon", since the here-doc otherwise covers the newline(s).
                token = ";"
            else if (.not. is_end_of_buffer()) then
                semi = is_shell_indentation_engine_newline_semi()
                moved = forward_line(1_int64)

                if (semi .or. is_end_of_buffer()) then
                    token = ";"
                else
                    call shell_indentation_engine_shell_forward_token(token, status)
                end if
            end if

            return
        end if

        ok = forward_comment(point_maximum())

        if (looking_at("\\"//LF, status)) then
            moved = forward_line(1_int64)
            call shell_indentation_engine_shell_forward_token(token, status)
        else if (looking_at(shell_operators_regular_expression, status)) then
            call goto_character(match_end(0, status))
            token_text = match_string(0)

            if (len(token_text) > 0) then
                if (index(";&|", token_text(len(token_text):len(token_text))) > 0) then
                    if (looking_at("[ "//TAB//"]*\(?:#\|$\)", status)) moved = forward_line(1_int64)
                end if
            end if

            token = token_text
        else
            position = point()
            call shell_indentation_engine_default_forward_token(token_text, status)
            if (.not. status%ok()) return

            if (is(token_text, ")")) then
                token = "case-)"
            else if (is(token_text, "(")) then
                token = "case-("
            else if (starts_lower(token_text) .and. in_grammar(token_text)) then
                here = point()
                call goto_character(position)
                ok = is_shell_indentation_engine_shell_keyword(token_text)
                call goto_character(here)

                if (ok) then
                    token = token_text
                else
                    token = " word "
                end if
            else
                call move_alloc(token_text, token)
            end if
        end if
    end subroutine shell_indentation_engine_shell_forward_token

    ! Port of sh-script.el:sh-smie--looking-back-at-continuation-p.
    recursive logical function is_shell_indentation_engine_looking_back_at_continuation() result(p)
        type(status_type) :: st
        integer(int64) :: here

        here = point()

        if (cb(point()) == 10) then
            call forward_character(st, -1_int64)
            p = .true.
        else
            p = is_end_of_line()
        end if

        if (p) p = looking_back("\(?:^\|[^\]\)\(?:\\\\\)*\\", st, line_beginning_position())
        call goto_character(here)
    end function is_shell_indentation_engine_looking_back_at_continuation

    ! (equal (syntax-after (1- (point))) (string-to-syntax "|")).
    ! Not a port.
    logical function after_fence()
        type(syntax_descriptor_type) :: d

        d = syntax_entry_at(ca(point() - 1), point() - 1)
        after_fence = d%code == 15 .and. d%match == -1
    end function after_fence

    !> The `simple_minded_indentation_engine_backward_token_function' of sh.  Port of
    !> sh-script.el:sh-smie-sh-backward-token.
    recursive subroutine shell_indentation_engine_shell_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token_text
        type(parse_state_type) :: state
        integer(int64) :: beginning_of_line, here
        logical :: ok, fence

        status = status_type()
        beginning_of_line = line_beginning_position()
        ok = forward_comment(-point())
        fence = .false.

        if (is_beginning_of_line() .and. .not. is_beginning_of_buffer()) then
            if (after_fence()) then
                state = parser_state()
                fence = state%instring == -1
            end if
        end if

        if (fence) then
            ! Right after a here-document.
            call plain_forward_expression(-1_int64, status)
            if (.not. status%ok()) return
            ! Pretend the here-document is a "newline representing a
            ! semi-colon", since the here-doc otherwise covers the newline(s).
            token = ";"
        else if (point() < beginning_of_line) then
            if (is_shell_indentation_engine_looking_back_at_continuation()) then
                call forward_character(status, -1_int64)
                call simple_minded_indentation_engine_backward_token(token, status)
            else if (is_shell_indentation_engine_newline_semi()) then
                token = ";"
            else
                call simple_minded_indentation_engine_backward_token(token, status)
            end if
        else if (looking_back(shell_operators_back_regular_expression, status, line_beginning_position(), greedy=.true.)) then
            call goto_character(match_beginning(1, status))
            token = match_string(1)
        else
            call shell_indentation_engine_default_backward_token(token_text, status)
            if (.not. status%ok()) return

            if (is(token_text, ")")) then
                token = "case-)"
            else if (is(token_text, "(")) then
                token = "case-("
            else if (starts_lower(token_text) .and. in_grammar(token_text)) then
                here = point()
                ok = is_shell_indentation_engine_shell_keyword(token_text)
                call goto_character(here)

                if (ok) then
                    token = token_text
                else
                    token = " word "
                end if
            else
                call move_alloc(token_text, token)
            end if
        end if
    end subroutine shell_indentation_engine_shell_backward_token

    !> The function sh adds to `simple_minded_indentation_engine_indent_functions': the indentation of
    !> a continued line.  Port of sh-script.el:sh-smie--indent-continuation.
    recursive subroutine shell_indentation_engine_indent_continuation(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: here, moved, ci, maximum_ci
        type(choice_setting_type) :: saved
        logical :: continued

        status = status_type()
        indent = simple_minded_indentation_engine_indent_type()
        here = point()
        continued = .false.

        if (.not. shell_indent_after_continuation%is_off()) then
            moved = skip_characters_backward(" "//TAB, status)
            continued = is_shell_indentation_engine_looking_back_at_continuation()
            call goto_character(here)
            status = status_type()
        end if

        if (.not. continued) return

        if (shell_indent_after_continuation%choice == SHELL_INDENT_AFTER_CONTINUATION_ALWAYS) then
            moved = forward_line(-1_int64)
            indent%kind = SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN

            if (is_shell_indentation_engine_looking_back_at_continuation()) then
                indent%column = int(current_indentation())
            else
                indent%column = int(current_indentation()) + shell_variable_value(shell_indent_for_continuation)
            end if

            call goto_character(here)
            return
        end if

        ! Just make sure a line-continuation is indented deeper.
        saved = shell_indent_after_continuation
        shell_indent_after_continuation = .false.
        indent = simple_minded_indentation_engine_indent_calculate(status)
        shell_indent_after_continuation = saved
        call goto_character(here)
        if (.not. status%ok()) return
        if (indent%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) return
        maximum_ci = huge(0_int64)

        do
            moved = forward_line(-1_int64)
            ci = current_indentation()

            if (ci < indent%column) then
                ! Previous line is less indented, we're good.
                exit
            else if (is_shell_indentation_engine_looking_back_at_continuation()) then
                maximum_ci = min(maximum_ci, ci)
                ! Previous line is itself a continuation.
                ! If it's indented like us, we're good, otherwise
                ! check the line before that one.
                if (.not. (ci > indent%column)) exit
            else
                ! Previous line is the beginning of the continued line.
                indent%column = int(min(ci + shell_variable_value(shell_indent_for_continuation), maximum_ci))
                exit
            end if
        end do

        call goto_character(here)
    end subroutine shell_indentation_engine_indent_continuation

    ! A rule result: OFFSET.  Not a port.
    function offset_result(offset) result(r)
        integer, intent(in) :: offset
        type(simple_minded_indentation_engine_rule_result_type) :: r

        r%kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
        r%value = offset
    end function offset_result

    ! A rule result: (column . INDENT), nil if INDENT is nil.  Not a port.
    function column_result(indent) result(r)
        type(simple_minded_indentation_engine_indent_type), intent(in) :: indent
        type(simple_minded_indentation_engine_rule_result_type) :: r

        if (indent%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) return
        r%kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN
        r%value = indent%column
    end function column_result

    ! A rule result: (column . COLUMN).  Not a port.
    function column_at(column) result(r)
        integer(int64), intent(in) :: column
        type(simple_minded_indentation_engine_rule_result_type) :: r

        r%kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN
        r%value = int(column)
    end function column_at

    !> The `simple_minded_indentation_engine_rules_function' of sh.  Port of
    !> sh-script.el:sh-smie-sh-rules.
    recursive subroutine shell_indentation_engine_shell_rules(method, argument, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: argument
        type(simple_minded_indentation_engine_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_expression_type) :: data
        type(simple_minded_indentation_engine_indent_type) :: v
        character(len=:), allocatable :: token_text
        integer(int64) :: here
        logical :: beginning_of_line

        status = status_type()
        result = simple_minded_indentation_engine_rule_result_type()

        select case (method)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT)
            if (argument == "basic") result = offset_result(int(shell_basic_offset%value))
        case (SIMPLE_MINDED_INDENTATION_ENGINE_AFTER)
            select case (argument)
            case ("case-)")
                result = offset_result(shell_variable_value(shell_indent_for_case_alt) - shell_variable_value( &
                                       shell_indent_for_case_label))
            case (";;", ";&", ";;&", ";|")
                ! with-demoted-errors
                call simple_minded_indentation_engine_backward_expression(data, status, token=argument)

                if (status%ok()) then
                    beginning_of_line = is_simple_minded_indentation_engine_rule_beginning_of_line()

                    if (.not. beginning_of_line) then
                        here = point()
                        call simple_minded_indentation_engine_backward_token(token_text, status)
                        if (is(token_text, "in") .or. is(token_text, &
                                             ";;")) beginning_of_line = is_simple_minded_indentation_engine_rule_beginning_of_line()
                        call goto_character(here)
                    end if

                    if (beginning_of_line) then
                        result = column_at(current_column())
                    else
                        v = simple_minded_indentation_engine_indent_calculate(status)
                        if (status%ok()) result = column_result(v)
                    end if
                end if

                if (.not. status%ok()) then
                    call message("SMIE rule error: "//format_error(status))
                    status = status_type()
                    result = simple_minded_indentation_engine_rule_result_type()
                end if
            case ("else")
                result = offset_result(shell_variable_value(shell_indent_after_else))
            case ("if")
                result = offset_result(shell_variable_value(shell_indent_after_if))
            case ("do")
                if (is_simple_minded_indentation_engine_rule_hanging(status)) then
                    result = offset_result(shell_variable_value(shell_indent_after_loop_construct))
                else
                    result = offset_result(shell_variable_value(shell_indent_after_do))
                end if
            case ("in")
                result = offset_result(shell_variable_value(shell_indent_for_case_label))
            case ("(", "{", "[")
                if (.not. looking_at(".[ "//TAB//"]*[^"//LF//" "//TAB//"#]", status)) then
                    result = offset_result(shell_variable_value(shell_indent_after_open))
                else
                    call goto_character(match_end(0, status) - 1)
                    result = column_at(current_column())
                end if
            case default
            end select
        case (SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE)
            select case (argument)
            case ("(", "{", "[", "while", "if", "for", "case")
                if (argument == "{" .and. is_simple_minded_indentation_engine_rule_parent([character(len=4) :: "for", "case"], &
                                                                                          status)) then
                    call simple_minded_indentation_engine_backward_expression(data, status, token="in")

                    if (is(data%token, "for") .or. is(data%token, "case")) then
                        v = simple_minded_indentation_engine_indent_virtual(status)
                        result = column_result(v)
                    end if
                else if (.not. is_simple_minded_indentation_engine_rule_previous([character(len=2) :: "&&", "||", "|"], &
                                                                                 status)) then
                    if (is_simple_minded_indentation_engine_rule_hanging(status)) result = &
                        simple_minded_indentation_engine_rule_parent(status)
                else
                    if (.not. is_simple_minded_indentation_engine_rule_beginning_of_line()) then
                        do
                            call simple_minded_indentation_engine_backward_expression(data, status, halfexpression=.true.)
                            if (.not. is(data%token, "|")) exit
                        end do

                        v = simple_minded_indentation_engine_indent_virtual(status)
                        result = column_result(v)
                    end if
                end if
                ! FIXME: Maybe this handling of ;; should be made into
                ! a smie-rule-terminator function that takes the substitute ";" as arg.
            case (";;", ";&", ";;&", ";|")
                if (is_simple_minded_indentation_engine_rule_beginning_of_line() .and. looking_at(";\(?:;&?\|[&|]\)?[ "//TAB &
                                                                                                  //"]*\(#\|$\)", status)) then
                    call simple_minded_indentation_engine_indent_keyword(v, status, ";")
                    result = column_result(v)
                else
                    result = simple_minded_indentation_engine_rule_separator(method, status)
                end if
            case ("|", "&&", "||")
                if (.not. is_simple_minded_indentation_engine_rule_parent([argument], status)) then
                    if (argument /= "&&" .or. shell_indent_statement_after_and) then
                        call simple_minded_indentation_engine_backward_expression(data, status, token=argument)
                        v = simple_minded_indentation_engine_indent_virtual(status)

                        if (v%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
                            v%column = int(shell_basic_offset%value) + v%column
                            result = column_result(v)
                        end if
                    end if
                end if
                ! Attempt at backward compatibility with the old config variables.
            case ("fi")
                result = offset_result(shell_variable_value(shell_indent_for_fi))
            case ("done")
                result = offset_result(shell_variable_value(shell_indent_for_done))
            case ("then")
                result = offset_result(shell_variable_value(shell_indent_for_then))
            case ("do")
                result = offset_result(shell_variable_value(shell_indent_for_do))
            case default
            end select
        case default
        end select
        ! sh-indent-after-done: aligned completely differently.
        ! sh-indent-for-continuation: Line continuations are handled differently.
        ! sh-indent-after-function: we don't handle it differently.
    end subroutine shell_indentation_engine_shell_rules

    ! Check if we're after the first arg of an if/while/for/... construct.
    ! Returns the construct's token and moves point before it, if so.
    ! Port of sh-script.el:sh-smie--rc-after-special-arg-p.
    recursive subroutine shell_indentation_engine_rc_after_special_argument(construct)
        character(len=:), allocatable, intent(out) :: construct
        character(len=:), allocatable :: token
        type(status_type) :: st
        logical :: ok

        ok = forward_comment(-point())
        if (.not. looking_back(")\|\_<not", st, point() - 3)) return
        ! ignore-errors
        call plain_forward_expression(-1_int64, st)
        if (.not. st%ok()) return
        call simple_minded_indentation_engine_backward_token(token, st)
        if (.not. st%ok()) return
        if (is(token, "if") .or. is(token, "for") .or. is(token, "switch") .or. is(token, "while")) call move_alloc(token, &
                                                                                                                    construct)
    end subroutine shell_indentation_engine_rc_after_special_argument

    ! Return .true. if a newline should be treated as a semi-colon.
    ! Point should be before the newline.  Port of
    ! sh-script.el:sh-smie--rc-newline-semi-p.
    recursive logical function is_shell_indentation_engine_rc_newline_semi() result(semi)
        character(len=:), allocatable :: token, construct
        type(status_type) :: st
        integer(int64) :: here
        logical :: special

        here = point()
        call simple_minded_indentation_engine_backward_token(token, st)
        special = .false.

        if (is(token, "not")) then
            call forward_word_strictly()
            special = .true.
        else if (empty(token) .and. cb(point()) == iachar(")")) then
            special = .true.
        end if

        if (special) then
            call shell_indentation_engine_rc_after_special_argument(construct)
            semi = .not. allocated(construct)
        else
            semi = is_shell_indentation_engine_newline_semi(token)
        end if

        call goto_character(here)
    end function is_shell_indentation_engine_rc_newline_semi

    ! (forward-word-strictly 1).  Not a port.
    subroutine forward_word_strictly()
        call forward_word(1_int64)
    end subroutine forward_word_strictly

    !> The `simple_minded_indentation_engine_forward_token_function' of rc.  Port of
    !> sh-script.el:sh-smie-rc-forward-token.
    recursive subroutine shell_indentation_engine_rc_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token_text
        type(parse_state_type) :: state
        integer(int64) :: position, here, moved, me1
        logical :: ok, semi, hanging

        status = status_type()
        ! FIXME: Code duplication with sh-smie-sh-forward-token.
        hanging = looking_at("[ "//TAB//"]*\(?:#\|\(\s|\)\|$\)", status)

        if (hanging) then
            me1 = match_end(1, status)
            here = point()
            moved = skip_characters_backward(" "//TAB, status)
            hanging = .not. is_beginning_of_line()
            call goto_character(here)
        end if

        if (hanging) then
            ok = .false.

            if (me1 /= 0) then
                state = parser_state()
                ok = state%instring == -1
            end if

            if (ok) then
                ! Right before a here-doc.
                call plain_forward_expression(1_int64, status)
                if (.not. status%ok()) return
                ! Pretend the here-document is a "newline representing a
                ! semi-colon", since the here-doc otherwise covers the newline(s).
                token = ";"
            else
                semi = is_shell_indentation_engine_rc_newline_semi()
                moved = forward_line(1_int64)

                if (semi .or. is_end_of_buffer()) then
                    token = ";"
                else
                    call shell_indentation_engine_rc_forward_token(token, status)
                end if
            end if

            return
        end if

        ok = forward_comment(point_maximum())

        if (looking_at("\\"//LF, status)) then
            moved = forward_line(1_int64)
            call shell_indentation_engine_rc_forward_token(token, status)
        else
            position = point()
            call shell_indentation_engine_default_forward_token(token_text, status)
            if (.not. status%ok()) return

            if (starts_lower(token_text) .and. in_grammar(token_text)) then
                here = point()
                call goto_character(position)
                ok = is_shell_indentation_engine_keyword()
                call goto_character(here)

                if (ok) then
                    call move_alloc(token_text, token)
                else
                    token = " word "
                end if
            else
                call move_alloc(token_text, token)
            end if
        end if
    end subroutine shell_indentation_engine_rc_forward_token

    !> The `simple_minded_indentation_engine_backward_token_function' of rc.  Port of
    !> sh-script.el:sh-smie-rc-backward-token.
    recursive subroutine shell_indentation_engine_rc_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token_text
        type(parse_state_type) :: state
        integer(int64) :: beginning_of_line, here
        logical :: ok, fence

        status = status_type()
        ! FIXME: Code duplication with sh-smie-sh-backward-token.
        beginning_of_line = line_beginning_position()
        ok = forward_comment(-point())
        fence = .false.

        if (is_beginning_of_line() .and. .not. is_beginning_of_buffer()) then
            if (after_fence()) then
                state = parser_state()
                fence = state%instring == -1
            end if
        end if

        if (fence) then
            ! Right after a here-document.
            call plain_forward_expression(-1_int64, status)
            if (.not. status%ok()) return
            ! Pretend the here-document is a "newline representing a
            ! semi-colon", since the here-doc otherwise covers the newline(s).
            token = ";"
        else if (point() < beginning_of_line) then
            ! We skipped over a newline.
            if (is_end_of_line() .and. looking_back("\(?:^\|[^\]\)\(?:\\\\\)*\\", status, line_beginning_position())) then
                ! A continued line.
                call forward_character(status, -1_int64)
                call simple_minded_indentation_engine_backward_token(token, status)
            else if (is_shell_indentation_engine_rc_newline_semi()) then
                token = ";"
            else
                call simple_minded_indentation_engine_backward_token(token, status)
            end if
        else
            call shell_indentation_engine_default_backward_token(token_text, status)
            if (.not. status%ok()) return

            if (starts_lower(token_text) .and. in_grammar(token_text)) then
                here = point()
                ok = is_shell_indentation_engine_keyword()
                call goto_character(here)

                if (ok) then
                    call move_alloc(token_text, token)
                else
                    token = " word "
                end if
            else
                call move_alloc(token_text, token)
            end if
        end if
    end subroutine shell_indentation_engine_rc_backward_token

    !> The `simple_minded_indentation_engine_rules_function' of rc.  Port of
    !> sh-script.el:sh-smie-rc-rules.
    recursive subroutine shell_indentation_engine_rc_rules(method, argument, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: argument
        type(simple_minded_indentation_engine_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: construct
        integer(int64) :: here

        status = status_type()
        result = simple_minded_indentation_engine_rule_result_type()

        select case (method)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT)
            if (argument == "basic") result = offset_result(int(shell_basic_offset%value))
        case (SIMPLE_MINDED_INDENTATION_ENGINE_AFTER)
            if (argument == ";") then
                if (is_simple_minded_indentation_engine_rule_parent([character(len=4) :: "case"], status)) then
                    result = simple_minded_indentation_engine_rule_parent(status, shell_variable_value(shell_indent_after_case))
                end if
            end if
        case (SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE)
            select case (argument)
            case ("{")
                here = point()
                call shell_indentation_engine_rc_after_special_argument(construct)
                if (allocated(construct)) result = column_at(current_column())
                call goto_character(here)
            case ("(", "[")
                if (is_simple_minded_indentation_engine_rule_hanging(status)) result = &
                    simple_minded_indentation_engine_rule_parent(status)
            case default
            end select
        case (SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO)
            ! FIXME: SIMPLE_MINDED_INDENTATION_ENGINE parses "if (exp) cmd" as "(if ((exp) cmd))" so "cmd" is
            ! treated as an arg to (exp) by default, which indents it all wrong.
            ! To handle it right, we should extend smie-indent-exps so that the
            ! preceding keyword can give special rules.  Currently the only special
            ! rule we have is the :list-intro hack, which we use here to align "cmd"
            ! with "(exp)", which is rarely the right thing to do, but is better
            ! than nothing.
            if (argument == "for" .or. argument == "if" .or. argument == "while") result%kind = &
                SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES
        case default
        end select
        ! sh-indent-after-switch: handled implicitly by the default { rule.
    end subroutine shell_indentation_engine_rc_rules

    ! The grammars.  Not a port.
    subroutine initialize_grammars(status)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_backus_naur_form_type) :: backus_naur_form
        type(simple_minded_indentation_engine_precs_type) :: precs(2)
        type(simple_minded_indentation_engine_prec2_type) :: prec2
        character(len=:), allocatable :: ops(:)
        integer :: i, n

        status = status_type()
        if (.not. shell_indentation_engine_shell_grammar%unset) return
        ! A constant, or a $var, or a sequence of them...
        call backus_naur_form%append_nonterminal("exp")
        call backus_naur_form%append("cmd", [character(len=8) :: "case", "exp", "in", "branches", "esac"])
        call backus_naur_form%append("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "fi"])
        call backus_naur_form%append("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "else", "cmd", "fi"])
        call backus_naur_form%append("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "elif", "cmd", "then", "cmd", "fi"])
        call backus_naur_form%append("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "elif", "cmd", "then", "cmd", "else", &
                                             "cmd", &
                                             "fi"])
        call backus_naur_form%append("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "elif", "cmd", "then", "cmd", &
                                             "elif", "cmd", "then", "cmd", "else", "cmd", "fi"])
        call backus_naur_form%append("cmd", [character(len=5) :: "while", "cmd", "do", "cmd", "done"])
        call backus_naur_form%append("cmd", [character(len=5) :: "until", "cmd", "do", "cmd", "done"])
        call backus_naur_form%append("cmd", [character(len=4) :: "for", "exp", "in", "cmd", "do", "cmd", "done"])
        call backus_naur_form%append("cmd", [character(len=4) :: "for", "exp", "do", "cmd", "done"])
        ! bash&zsh&ksh88.
        call backus_naur_form%append("cmd", [character(len=6) :: "select", "exp", "in", "cmd", "do", "cmd", "done"])
        call backus_naur_form%append("cmd", [character(len=6) :: "repeat", "exp", "do", "cmd", "done"]) ! zsh.
        call backus_naur_form%append("cmd", [character(len=6) :: "exp", "always", "exp"]) ! zsh.
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "|", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "|&", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "&&", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "||", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", ";", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "&", "cmd"])
        call backus_naur_form%append("rpattern", [character(len=8) :: "rpattern", "|", "rpattern"])
        call backus_naur_form%append("pattern", [character(len=8) :: "rpattern"])
        call backus_naur_form%append("pattern", [character(len=8) :: "case-(", "rpattern"])
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";;", "branches"])
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";&", "branches"])
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";;&", "branches"]) ! bash.
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";|", "branches"]) ! zsh.
        call backus_naur_form%append("branches", [character(len=7) :: "pattern", "case-)", "cmd"])
        call precs(1)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=3) :: ";;", ";&", ";;&", ";|"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=1) :: ";", "&"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=2) :: "&&", "||"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=2) :: "|", "|&"])
        call simple_minded_indentation_engine_backus_naur_form_to_prec2(backus_naur_form, prec2, status, precs=precs)
        if (.not. status%ok()) return
        call simple_minded_indentation_engine_prec2_to_grammar(prec2, shell_indentation_engine_shell_grammar, status)
        if (.not. status%ok()) return
        ! sh-smie--sh-operators: the tokens not starting with a letter.
        n = 0

        do i = 1, size(shell_indentation_engine_shell_grammar%tokens)
            if (is_operator(shell_indentation_engine_shell_grammar%tokens(i)%text)) n = n + 1
        end do

        allocate (character(len=3) :: ops(n))
        n = 0

        do i = 1, size(shell_indentation_engine_shell_grammar%tokens)
            if (is_operator(shell_indentation_engine_shell_grammar%tokens(i)%text)) then
                n = n + 1
                ops(n) = shell_indentation_engine_shell_grammar%tokens(i)%text
            end if
        end do

        shell_operators_regular_expression = regular_expression_optimization(ops)
        shell_operators_back_regular_expression = "\(?:^\|[^\]\)\(?:\\\\\)*"//"\("//shell_operators_regular_expression//"\)"
        ! rc.
        backus_naur_form = simple_minded_indentation_engine_backus_naur_form_type()
        call backus_naur_form%append_nonterminal("exp")
        call backus_naur_form%append("cmd", [character(len=4) :: "cmd", "case", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "if", "exp"])
        call backus_naur_form%append("cmd", [character(len=6) :: "switch", "exp"])
        call backus_naur_form%append("cmd", [character(len=3) :: "for", "exp"])
        call backus_naur_form%append("cmd", [character(len=5) :: "while", "exp"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "|", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "|&", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "&&", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "||", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", ";", "cmd"])
        call backus_naur_form%append("cmd", [character(len=3) :: "cmd", "&", "cmd"])
        call backus_naur_form%append("pattern", [character(len=7) :: "pattern", "|", "pattern"])
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";;", "branches"])
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";&", "branches"])
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";;&", "branches"]) ! bash.
        call backus_naur_form%append("branches", [character(len=8) :: "branches", ";|", "branches"]) ! zsh.
        call backus_naur_form%append("branches", [character(len=7) :: "pattern", "case-)", "cmd"])
        precs(1) = simple_minded_indentation_engine_precs_type()
        precs(2) = simple_minded_indentation_engine_precs_type()
        call precs(1)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=3) :: ";;", ";&", ";;&", ";|"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=4) :: "case"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=1) :: ";", "&"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=2) :: "&&", "||"])
        call precs(2)%append(SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC, [character(len=2) :: "|", "|&"])
        prec2 = simple_minded_indentation_engine_prec2_type()
        call simple_minded_indentation_engine_backus_naur_form_to_prec2(backus_naur_form, prec2, status, precs=precs)
        if (.not. status%ok()) return
        call simple_minded_indentation_engine_prec2_to_grammar(prec2, shell_indentation_engine_rc_grammar, status)

    contains

        logical function is_operator(token)
            character(len=*), intent(in) :: token

            is_operator = .true.
            if (len(token) > 0) is_operator = .not. (token(1:1) >= "a" .and. token(1:1) <= "z")
        end function is_operator
    end subroutine initialize_grammars

    ! The around advice sh puts on smie--hanging-eolp-function.  Not a port.
    recursive logical function is_shell_hanging_end_of_line() result(end_of_line)
        type(status_type) :: st

        if (looking_at("[ "//TAB//"]*\\"//LF, st)) then
            call goto_character(match_end(0, st))
            end_of_line = .true.
        else
            end_of_line = is_simple_minded_indentation_engine_default_hanging_end_of_line()
        end if
    end function is_shell_hanging_end_of_line

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    ! Guess the shell used in the current buffer.
    ! Return the name of the shell suitable for `shell_set_shell'.  Port of
    ! sh-script.el:sh--guess-shell.
    function shell_guess_shell() result(shell)
        character(len=:), allocatable :: shell
        character(len=:), allocatable :: name, base
        type(status_type) :: st
        integer(int64) :: here
        logical :: matched

        here = point()
        call goto_character(point_minimum())
        matched = .false.
        if (allocated(auto_mode_interpreter_regular_expression)) matched = looking_at(auto_mode_interpreter_regular_expression, st)
        if (matched) shell = match_string(2)
        call goto_character(here)
        if (matched) return
        shell = shell_shell_file%value
        if (.not. allocated(buffers(current)%filename)) return
        ! Checks that use `buffer_file_name' follow.
        name = buffers(current)%filename
        base = file_name_nondirectory(name)

        if (string_match("\.m?spec\'", name, st) > 0) then
            shell = "rpm"
        else if (string_match("[.]sh\>", name, st) > 0) then
            shell = "sh"
        else if (string_match("[.]bash\(rc\)?\>", name, st) > 0) then
            shell = "bash"
        else if (string_match("[.]ksh\>", name, st) > 0) then
            shell = "ksh"
        else if (string_match("[.]mkshrc\>", name, st) > 0) then
            shell = "mksh"
        else if (string_match("[.]t?csh\(rc\)?\>", name, st) > 0) then
            shell = "csh"
        else if (string_match("[.]zsh\(rc\|env\)?\>", name, st) > 0) then
            shell = "zsh"
        else if (base == ".profile") then
            shell = "sh"
        else if (base == "PKGBUILD") then
            shell = "bash"
        end if
    end function shell_guess_shell

    !> Generic major mode for editing shell scripts.
    !>
    !> This is a generic major mode intended to be inherited by concrete
    !> implementations.  Currently there are two: `shell_script_mode' and
    !> `bash_ts_mode'.  Port of sh-script.el:sh-base-mode.
    recursive subroutine shell_base_mode(status)
        type(status_type), intent(out) :: status
        type(skeleton_pairs_type) :: backquote_pairs
        type(completion_at_point_function_entry_type), allocatable :: functions(:)
        logical :: saved
        integer :: k

        call major_modes%shell_base_mode%begin(saved, status)
        if (.not. status%ok()) return
        call shell_shell_file%make_local()
        call shell_shell%make_local()
        call skeleton_default_pairs%set_local(shell_skeleton_default_pairs)
        call paragraph_start%set_local(page_delimiter%value//"\|$")
        call paragraph_separate%set_local(paragraph_start%value//"\|#!/")
        call comment_start%set_local("# ")
        call comment_start_skip%set_local("#+["//TAB//" ]*")
        call local_abbreviation_table%set_local(shell_script_mode_abbreviation_table)
        ! (setq-local comint-dynamic-complete-functions
        ! sh-dynamic-complete-functions)
        functions = shell_script_dynamic_complete_functions%functions()
        call command_interpreter_dynamic_complete_functions%prepend(command_interpreter_filename_completion, local=.true.)
        call command_interpreter_dynamic_complete_functions%clear()

        do k = size(functions), 1, -1
            call command_interpreter_dynamic_complete_functions%prepend(functions(k)%fn, local=.true.)
        end do

        call completion_at_point_functions%prepend(command_interpreter_completion_at_point, local=.true.)
        ! we can't look if previous line ended with `\'
        call command_interpreter_prompt_regular_expression%set_local("^[ "//TAB//"]*")
        call syntax_propertize_function%set_local(shell_syntax_propertize_function)
        call syntax_propertize_extend_region_functions%append(syntax_propertize_multiline, local=.true.)
        backquote_pairs = skeleton_pairs_type()
        call backquote_pairs%append(96, [text("`"), SKELETON_POINT, text("`")])
        call skeleton_pairs%set_local(backquote_pairs)
        call skeleton_pair_filter_function%set_local(is_shell_quoted)
        call skeleton_filter_function%set_local(shell_skeleton_feature)
        call skeleton_newline_indent_rigidly%set_local(.true.)
        call subprogram_prompt_regular_expression%set_local("^\(\(function[ "//TAB//"]\)?[ "//TAB//"]*[[:alnum:]_]+[ "//TAB// &
                                                            "]*([ "//TAB//"]*)\|function[ "//TAB//"]+[[:alnum:]_]+[ "//TAB// &
                                                            "]*\(([ "//TAB//"]*)\)?\)[ "//TAB//"]*")
        call completion_at_point_functions%prepend(shell_completion_at_point_function, local=.true.)
        call add_log_current_subprogram_function%set_local(shell_current_function_name)
        call major_modes%shell_base_mode%end(saved, status)
    end subroutine shell_base_mode

    !> Major mode for editing shell scripts.
    !> This mode works for many shells, since they all have roughly the same syntax,
    !> as far as commands, arguments, variables, pipes, comments etc. are concerned.
    !> Unless the file's magic number indicates the shell, your usual shell is
    !> assumed.  Since filenames rarely give a clue, they are not further analyzed.
    !>
    !> This mode adapts to the variations between shells (see `shell_set_shell') by
    !> means of an inheritance based feature lookup (see `shell_feature').  This
    !> mechanism applies to all variables (including skeletons) that pertain to
    !> shell-specific features.  Shell script files can use the `shell_shell' local
    !> variable to indicate the shell variant to be used for the file.
    !>
    !> The default style of this mode is that of Rosenblatt's Korn shell book.
    !> The syntax of the statements varies with the shell being used.  The
    !> following commands are available, based on the current shell's syntax:
    !> \<shell_script_mode_map>
    !> \[shell_case]    case statement
    !> \[shell_for]     for loop
    !> \[shell_function]        function definition
    !> \[shell_if]      if statement
    !> \[shell_indexed_loop]    indexed loop from 1 to n
    !> \[shell_while_getopts]   while getopts loop
    !> \[shell_repeat]  repeat loop
    !> \[shell_select]  select loop
    !> \[shell_until]   until loop
    !> \[shell_while]   while loop
    !>
    !> For sh and rc shells indentation commands are:
    !> \[simple_minded_indentation_engine_configuration_show_indent]    Show the rules controlling this line's indentation.
    !> \[simple_minded_indentation_engine_configuration_set_indent]     Change the rules controlling this line's indentation.
    !> \[simple_minded_indentation_engine_configuration_guess]  Try to tweak the indentation rules so the
    !> buffer indents as it currently is indented.
    !>
    !>
    !> \[backward_delete_character_untabify]      Delete backward one position, even if it was a tab.
    !> \[shell_end_of_command]  Go to end of successive commands.
    !> \[shell_beginning_of_command]    Go to beginning of successive commands.
    !> \[shell_set_shell]       Set this buffer's shell, and maybe its magic number.
    !> \[shell_execute_region]  Have optional header and region be executed in a subshell.
    !>
    !> `shell_electric_here_document_mode' controls whether insertion of two
    !> unquoted < insert a here document.  You can control this behavior by
    !> modifying `shell_script_mode_hook'.
    !>
    !> If you generally program a shell different from your login shell you can
    !> set `shell_shell_file' accordingly.  If your shell's file name doesn't correctly
    !> indicate what shell it is use `shell_aliases' to translate.
    !>
    !> If your shell gives error messages with line numbers, you can use \[executable_interpret]
    !> with your script for an edit-interpret-debug cycle.  Port of
    !> sh-script.el:sh-mode.
    recursive subroutine shell_script_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keywords_type) :: levels(3)
        type(font_lock_syntax_entry_type) :: syntax(1)
        logical :: saved

        call major_modes%shell_script_mode%begin(saved, status)
        if (.not. status%ok()) return
        levels(1)%function => keywords_level_0
        levels(2)%function => keywords_level_1
        levels(3)%function => shell_script_font_lock_keywords_2
        syntax(1)%chars = "/~.-_"
        syntax(1)%syntax = "w"
        call font_lock_defaults%set_local(levels=levels, keywords_only=.false., case_fold=.false., syntax_modifications=syntax, &
                                          syntactic_face_function=shell_font_lock_syntactic_face_function)
        call index_menu_case_fold_search%set_local(.false.)
        ! Parse or insert magic number for exec, and set all variables depending
        ! on the shell thus determined.
        call shell_set_shell(shell_guess_shell(), status, .false., .false.)
        if (.not. status%ok()) return
        call major_modes%shell_script_mode%end(saved, status)
    end subroutine shell_script_mode

    !> Set this buffer's shell to SHELL (a string).
    !> When used interactively, insert the proper starting #!-line,
    !> and make the visited file executable via `executable_set_magic',
    !> perhaps querying depending on the value of `executable_query'.
    !> (If given a prefix (i.e., \[universal_argument]) don't insert any starting #!
    !> line.)
    !>
    !> When this function is called noninteractively, INSERT_FLAG (the third
    !> argument) controls whether to insert a #!-line and think about making
    !> the visited file executable, and NO_QUERY_FLAG (the second argument)
    !> controls whether to query about making the visited file executable.
    !>
    !> Calls the value of `shell_set_shell_hook' if set.
    !>
    !> Shell script files can cause this function be called automatically
    !> when the file is visited by having a `shell_shell' file-local variable
    !> whose value is the shell name (don't quote it).  Port of
    !> sh-script.el:sh-set-shell.
    recursive subroutine shell_set_shell(shell_in, status, no_query_flag, insert_flag)
        character(len=*), intent(in) :: shell_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_query_flag, insert_flag
        character(len=:), allocatable :: shell, file, argument, supported
        type(string_type), allocatable :: variables(:)
        type(mode_line_part_type) :: parts(1)
        type(handle_type) :: table
        type(status_type) :: st
        logical :: insert_it
        integer :: k, i, e

        status = status_type()
        shell = shell_in

        if (len(shell) >= 4) then
            if (shell(len(shell) - 3:) == ".exe") shell = shell(1:len(shell) - 4)
        end if

        shell_shell = shell_canonicalize_shell(shell)
        insert_it = .false.
        if (present(insert_flag)) insert_it = insert_flag

        if (insert_it) then
            call string_feature_value(shell_shell_argument, argument)

            if (allocated(argument)) then
                call executable_set_magic(shell, file, status, argument, no_query_flag, insert_flag)
            else
                call executable_set_magic(shell, file, status, no_query_flag=no_query_flag, insert_flag=insert_flag)
            end if

            if (.not. status%ok()) return
            shell_shell_file = file
        end if

        parts(1)%text = "["//shell_shell%value//"]"
        parts(1)%face = ""
        call set_mode_line_process(parts)
        call make_local_entry(k)
        if (k == 0) return
        locals(k)%has_variables = .true.
        if (allocated(locals(k)%variables)) deallocate (locals(k)%variables)
        allocate (locals(k)%variables(0))
        locals(k)%variables_initialized = .false.
        call index_menu_patterns%set_local(shell_index_menu_patterns%get_current())
        e = syntax_input_index()

        if (e > 0) then
            if (shell_script_mode_syntax_table_input%entries(e)%table%index /= 0) then
                call copy_syntax_table(table, st, shell_script_mode_syntax_table_input%entries(e)%table)

                do i = 1, size(shell_script_mode_syntax_table_input%entries(e)%changes)
                    associate (c => shell_script_mode_syntax_table_input%entries(e)%changes(i))
                        call modify_syntax_entry(c%char, c%char, c%syntax, st, table)
                    end associate
                end do

                if (locals(k)%has_table) call destroy_syntax_table(locals(k)%table, st)
                locals(k)%has_table = .true.
                locals(k)%table = table
                call set_syntax_table(table, st)
            end if
        end if

        call shell_variables%get_current(variables)

        do i = 1, size(variables)
            call shell_remember_variable(variables(i)%text)
        end do

        call string_feature_value(shell_indent_supported, supported)
        call make_local_entry(k)
        locals(k)%has_indent_supported = allocated(supported)

        if (allocated(supported)) then
            locals(k)%indent_supported = supported
            call message("Setting up indent for shell type "//shell_shell%value)
            call simple_minded_indentation_engine_hanging_line_test_function%set_local(is_shell_hanging_end_of_line)
            call simple_minded_indentation_engine_indent_functions%prepend(shell_indentation_engine_indent_continuation, &
                                                                           local=.true.)

            if (supported == "rc") then
                call simple_minded_indentation_engine_setup(shell_indentation_engine_rc_grammar, &
                                                     shell_indentation_engine_rc_rules, shell_indentation_engine_rc_forward_token, &
                                                            shell_indentation_engine_rc_backward_token)
            else
                call simple_minded_indentation_engine_setup(shell_indentation_engine_shell_grammar, &
                                               shell_indentation_engine_shell_rules, shell_indentation_engine_shell_forward_token, &
                                                            shell_indentation_engine_shell_backward_token)
            end if

            if (shell_make_variables_local) call execute_shell_make_variables_local(st)
            call message("Indentation setup for shell type "//shell_shell%value)
        else
            call message("No indentation for this shell type.")
            call indent_line_function%set_local(shell_basic_indent_line_function)
        end if

        if (minor_modes%font_lock_mode%enabled()) then
            call font_lock_forget_defaults()
            call font_lock_set_defaults(st)
            call font_lock_flush(st)
        end if

        call shell_set_shell_hook%execute(status)
    end subroutine shell_set_shell

    ! `shell_set_shell' as a command.
    recursive subroutine execute_shell_set_shell(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: shell
        integer :: i, n

        ! This used to use interpreter-mode-alist, but that is
        ! no longer appropriate now that uses regexps.
        ! Maybe there could be a separate variable that lists
        ! the shells, used here and to construct i-mode-alist.
        ! But the following is probably good enough:
        n = size(shell_ancestors)
        allocate (names(n + 3))

        do i = 1, n
            names(i) = value_type(text=shell_ancestors(i)%shell)
        end do

        names(n + 1) = value_type(text="csh")
        names(n + 2) = value_type(text="rc")
        names(n + 3) = value_type(text="sh")
        table%list = make_list(names)
        call completing_read(format_prompt("Shell", shell_shell_file%value), table, shell, status, &
                             default=shell_shell_file%value)
        if (.not. status%ok()) return
        call shell_set_shell(shell, status, executable_query%choice == EXECUTABLE_QUERY_FUNCTION, &
                             current_prefix_argument%kind == PREFIX_NONE)
    end subroutine execute_shell_set_shell

    !> Convert a shell name SHELL to the one we should handle it as.
    !> SHELL is a full path to the shell interpreter; return a shell
    !> name.  Port of sh-script.el:sh-canonicalize-shell.
    function shell_canonicalize_shell(shell_in) result(name)
        character(len=*), intent(in) :: shell_in
        character(len=:), allocatable :: name
        character(len=:), allocatable :: shell, base
        type(status_type) :: st
        integer :: i

        shell = shell_in

        if (len(shell) >= 4) then
            if (shell(len(shell) - 3:) == ".exe") shell = shell(1:len(shell) - 4)
        end if

        base = file_name_nondirectory(shell)

        if (allocated(shell_aliases)) then
            do i = 1, size(shell_aliases)
                if (.not. shell_aliases(i)%basename) then
                    if (string_match(shell_aliases(i)%key, shell, st) > 0) then
                        name = shell_aliases(i)%shell
                        return
                    end if
                end if

                if (shell_aliases(i)%basename .and. shell_aliases(i)%key == base) then
                    name = shell_aliases(i)%shell
                    return
                end if
            end do
        end if

        name = base
    end function shell_canonicalize_shell

    !> Return the value of the indentation variable VARIABLE, interpreting the
    !> names SHELL_PLUS, SHELL_MINUS, SHELL_PLUS_PLUS, SHELL_MINUS_MINUS, SHELL_STAR and
    !> SHELL_SLASH by `shell_basic_offset'.  Port of sh-script.el:sh-var-value.
    integer function shell_variable_value(variable) result(value)
        type(shell_indent_choice_setting_type), intent(in) :: variable
        integer :: offset

        offset = int(shell_basic_offset%value)

        if (variable%columns /= COLUMNS_UNSET) then
            value = variable%columns
            return
        end if

        select case (variable%choice)
        case (SHELL_PLUS)
            value = offset
        case (SHELL_MINUS)
            value = -offset
        case (SHELL_PLUS_PLUS)
            value = 2*offset
        case (SHELL_MINUS_MINUS)
            value = 2*(-offset)
        case (SHELL_STAR)
            value = offset/2
        case (SHELL_SLASH)
            value = (-offset)/2
        case default
            ! .true. and .false. (upstream t and nil).
            value = 0
        end select
    end function shell_variable_value

    !> Indent a line for Sh mode (shell script mode).
    !> Indent as far as preceding non-empty line, then by steps of `shell_basic_offset'.
    !> Lines containing only comments are considered empty.  Port of
    !> sh-script.el:sh-basic-indent-line.
    recursive subroutine shell_basic_indent_line(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: mark
        integer(int64) :: previous, current, here, moved, column
        logical :: newline_command

        status = status_type()
        here = point()

        do
            call beginning_of_line()
            if (is_beginning_of_buffer()) exit
            moved = forward_line(-1_int64)
            call back_to_indentation()
            if (.not. (is_end_of_line() .or. ca(point()) == iachar("#"))) exit
        end do

        previous = current_column()
        call goto_character(here)
        call copy_marker_at(point(), mark, .false., status)
        newline_command = associated(this_command, commands%newline_and_indent)

        if (.not. newline_command .and. minor_modes%electric_indent_mode%enabled()) then
            newline_command = associated(this_command, commands%newline)
        end if

        if (newline_command) then
            column = previous
        else
            here = current_column()
            call back_to_indentation()
            current = current_column()

            if (here < current) then
                if (is_end_of_line()) then
                    column = previous
                else
                    column = 0
                end if
            else
                call delete_region(point(), line_beginning_position(), status)

                if (shell_basic_offset%value == 0) then
                    call destroy_marker(mark, st)
                    call set_error(status, ARITHMETIC_ERROR_CODE, "Arithmetic error")
                    return
                end if

                if (is_end_of_line()) then
                    column = max(previous, (1 + current/shell_basic_offset%value)*shell_basic_offset%value)
                else
                    column = (1 + current/shell_basic_offset%value)*shell_basic_offset%value
                end if
            end if
        end if

        column = indent_to(column, status)
        call goto_character(marked_position(mark))
        call destroy_marker(mark, st)
        if (current_column() < current_indentation()) moved = skip_characters_forward(" "//TAB, status)
    end subroutine shell_basic_indent_line

    ! `shell_basic_indent_line' as an `indent_line_function'.  Not a port.
    recursive subroutine shell_basic_indent_line_function(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent

        noindent = .false.
        call shell_basic_indent_line(status)
    end subroutine shell_basic_indent_line_function

    ! `shell_basic_indent_line' as a command.
    recursive subroutine execute_shell_basic_indent_line(status)
        type(status_type), intent(out) :: status

        call shell_basic_indent_line(status)
    end subroutine execute_shell_basic_indent_line

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Pass optional header and region to a subshell for noninteractive execution.
    !> The working directory is that of the buffer, and only environment variables
    !> are already set which is why you can mark a header within the script.
    !>
    !> The executed subshell is `shell_shell_file'.
    !>
    !> With a positive prefix ARGUMENT, instead of sending region, define header from
    !> beginning of buffer to point.  With a negative prefix ARGUMENT, instead of sending
    !> region, clear header.  Port of sh-script.el:sh-execute-region.
    recursive subroutine shell_execute_region(start, end_in, status, flag)
        integer(int64), intent(in) :: start, end_in
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64), intent(in), optional :: flag
        character(len=:), allocatable :: saved_shell, text
        integer(int64) :: end, header, here, saved_undo
        integer :: k

        status = status_type()
        call make_local_entry(k)
        if (k == 0) return

        if (present(flag)) then
            if (locals(k)%has_header) call destroy_marker(locals(k)%header_marker, st)
            locals(k)%has_header = .false.

            if (flag > 0) then
                call copy_marker_at(point(), locals(k)%header_marker, .false., status)
                locals(k)%has_header = .true.
            end if

            return
        end if

        if (allocated(shell_file_name%value)) saved_shell = shell_file_name%value
        shell_file_name = shell_shell_file%value

        if (locals(k)%has_header) then
            here = point()
            ! (let (buffer-undo-list) ...)
            saved_undo = buffers(current)%undo_count
            header = marked_position(locals(k)%header_marker)
            call goto_character(header)
            call buffer_substring(start, end_in, text, status)
            if (status%ok()) call insert(text, status)
            end = header + (end_in - start)
            if (status%ok()) call shell_command_on_region(point_minimum(), end, shell_shell_file%value, status)
            k = local_index()
            header = marked_position(locals(k)%header_marker)
            call delete_region(header, end, status)
            buffers(current)%undo_count = saved_undo
            call goto_character(here)
        else
            call shell_command_on_region(start, end_in, shell_shell_file%value//" -", status)
        end if

        if (allocated(saved_shell)) then
            shell_file_name = saved_shell
        else
            call shell_file_name%clear()
        end if
    end subroutine shell_execute_region

    ! `shell_execute_region' as a command.
    recursive subroutine execute_shell_execute_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call shell_execute_region(b, e, status)
        else
            call shell_execute_region(b, e, status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_shell_execute_region

    !> Make VARIABLE available for future completing reads in this buffer.
    !> Port of sh-script.el:sh-remember-variable.
    subroutine shell_remember_variable(variable)
        character(len=*), intent(in) :: variable
        character(len=:), allocatable :: value
        logical :: found
        integer :: k, i

        if (len(variable) < shell_remember_variable_minimum) return
        call getenv(variable, value, found)
        if (found) return
        k = local_index()

        if (k > 0) then
            if (.not. locals(k)%has_variables) k = 0
        end if

        if (k > 0) then
            do i = 1, size(locals(k)%variables)
                if (locals(k)%variables(i)%text == variable) return
            end do

            call prepend_string(locals(k)%variables, variable)
        else
            do i = 1, size(default_variables)
                if (default_variables(i)%text == variable) return
            end do

            call prepend_string(default_variables, variable)
        end if
    end subroutine shell_remember_variable

    !> Is point preceded by an odd number of backslashes?  Port of
    !> sh-script.el:sh-quoted-p.
    logical function is_shell_quoted()
        type(status_type) :: st
        integer(int64) :: here

        here = point()
        is_shell_quoted = mod(skip_characters_backward("\\", st), 2_int64) == -1
        call goto_character(here)
    end function is_shell_quoted

    ! The variables `shell_make_variables_local' and
    ! `shell_reset_indent_variables_to_global_values' handle (sh-var-list).
    ! Not a port.
    subroutine for_variable_list(make, kill)
        logical, intent(in) :: make, kill

        call one(shell_basic_offset)
        call one(shell_indent_after_case)
        call one(shell_indent_after_do)
        call one(shell_indent_after_done)
        call one(shell_indent_after_else)
        call one(shell_indent_after_if)
        call one(shell_indent_after_loop_construct)
        call one(shell_indent_after_open)
        call one(shell_indent_for_case_alt)
        call one(shell_indent_for_case_label)
        call one(shell_indent_for_continuation)
        call one(shell_indent_for_do)
        call one(shell_indent_for_done)
        call one(shell_indent_for_fi)
        call one(shell_indent_for_then)

    contains

        subroutine one(variable)
            class(setting_type), intent(inout) :: variable

            if (kill) call variable%clear_local()
            if (make) call variable%make_local()
        end subroutine one
    end subroutine for_variable_list

    !> Make the indentation variables local to this buffer.
    !> Normally they already are local.  This command is provided in case
    !> variable `shell_make_variables_local' has been set to false.
    !>
    !> To revert all these variables to the global values, use
    !> command `shell_reset_indent_variables_to_global_values'.  Port of
    !> sh-script.el:sh-make-vars-local.
    subroutine execute_shell_make_variables_local(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call for_variable_list(.true., .false.)
        call message("Indentation variables are now local.")
    end subroutine execute_shell_make_variables_local

    !> Reset local indentation variables to the global values.
    !> Then, if variable `shell_make_variables_local' is true, make them local.
    !> Port of sh-script.el:sh-reset-indent-vars-to-global-values.
    subroutine shell_reset_indent_variables_to_global_values(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call for_variable_list(.false., .true.)
        if (shell_make_variables_local) call for_variable_list(.true., .false.)
    end subroutine shell_reset_indent_variables_to_global_values

    !> Scan the buffer for variable assignments.
    !> Add these variables to `shell_shell_variables'.  Port of
    !> sh-script.el:sh-shell-initialize-variables.
    subroutine shell_shell_initialize_variables(status)
        type(status_type), intent(out) :: status
        integer(int64) :: here, found
        integer :: k

        call message("Scanning buffer `"//buffers(current)%name//"' for variable assignments...")
        here = point()
        call goto_character(point_minimum())
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_variables) then
                locals(k)%variables_initialized = .true.
            else
                default_variables_initialized = .true.
            end if
        else
            default_variables_initialized = .true.
        end if

        do
            call search_forward("=", found, status, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call shell_assignment_collect(status)
        end do

        call goto_character(here)
        call post_self_insert_hook%prepend(shell_assignment_collect, local=.true.)
        call message("Scanning buffer `"//buffers(current)%name//"' for variable assignments...done")
    end subroutine shell_shell_initialize_variables

    ! Whether `shell_shell_variables' is initialized (sh-shell-variables-initialized).
    ! Not a port.
    logical function variables_initialized()
        integer :: k

        variables_initialized = default_variables_initialized
        k = local_index()
        if (k == 0) return
        if (locals(k)%has_variables) variables_initialized = locals(k)%variables_initialized
    end function variables_initialized

    !> Insert an addition of VARIABLE and prefix DELTA for Bourne (type) shell.
    !> Port of sh-script.el:sh-add.
    subroutine shell_add(variable, delta, status)
        character(len=*), intent(in) :: variable
        integer(int64), intent(in) :: delta
        type(status_type), intent(out) :: status
        type(shell_string_feature_type) :: before, after
        character(len=24) :: digits
        character(len=:), allocatable :: op

        call before%append("bash", "$(( ")
        call before%append("ksh88", "$(( ")
        call before%append("posix", "$(( ")
        call before%append("rc", "`{expr $")
        call before%append("sh", "`expr $")
        call before%append("zsh", "$[ ")
        call after%append("bash", " ))")
        call after%append("ksh88", " ))")
        call after%append("posix", " ))")
        call after%append("rc", "}")
        call after%append("sh", "`")
        call after%append("zsh", " ]")
        call shell_remember_variable(variable)

        if (delta < 0) then
            op = " - "
        else
            op = " + "
        end if

        write (digits, "(i0)") abs(delta)
        call insert(before%get_current()//variable//op//trim(digits)//after%get_current(), status)
    end subroutine shell_add

    ! `shell_add' as a command.
    recursive subroutine execute_shell_add(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(string_type), allocatable :: variables(:)
        type(string_type), allocatable :: environment(:)
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: variable
        integer :: i, n, eq

        ! Do completion using `shell_shell_variables', but initialize it first
        ! (sh-add-completer).
        if (.not. variables_initialized()) call shell_shell_initialize_variables(status)
        environment = fortmacs_environment()
        call shell_shell_variables(variables)
        n = size(environment)
        allocate (names(n + size(variables)))

        do i = 1, n
            eq = index(environment(i)%text, "=")

            if (eq > 0) then
                names(i) = value_type(text=environment(i)%text(1:eq - 1))
            else
                names(i) = value_type(text=environment(i)%text)
            end if
        end do

        do i = 1, size(variables)
            names(n + i) = value_type(text=variables(i)%text)
        end do

        table%list = make_list(names)
        call completing_read("Variable: ", table, variable, status)
        if (.not. status%ok()) return
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call shell_add(variable, prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_shell_add

    !> Remember preceding identifier for future completion and do self-insert.
    !> Port of sh-script.el:sh-assignment (obsolete since 27.1).
    recursive subroutine shell_assignment(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        call self_insert_command(argument, int(iachar("="), int64), status)
        if (.not. status%ok()) return
        call shell_assignment_collect(status)
    end subroutine shell_assignment

    ! `shell_assignment' as a command.
    recursive subroutine execute_shell_assignment(status)
        type(status_type), intent(out) :: status

        call shell_assignment(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_shell_assignment

    ! Port of sh-script.el:sh--assignment-collect.
    recursive subroutine shell_assignment_collect(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer(int64) :: here, found

        status = status_type()
        if (cb(point()) /= iachar("=")) return
        here = point()
        call string_feature_value(shell_assignment_regular_expression, regular_expression)
        call beginning_of_line(1_int64)
        call regular_expression_search_forward(regular_expression, found, status, bound=here, noerror=SEARCH_NOERROR)
        if (found /= 0) call shell_remember_variable(match_string(1))
        call goto_character(here)
        status = status_type()
    end subroutine shell_assignment_collect

    ! Port of sh-script.el:sh--maybe-here-document.
    recursive subroutine shell_maybe_here_document(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        character(len=:), allocatable :: tabs, delim
        integer(int64) :: here, n
        integer :: i, k
        logical :: ok

        status = status_type()
        if (.not. looking_back("[^<]<<[ E-]", status, line_beginning_position())) return
        here = point()
        call goto_character(here - 2)
        ok = .not. (is_shell_quoted() .or. shell_inside_noncommand_expression(point()))
        call goto_character(here)
        if (.not. ok) return
        state = parser_state()
        if (state%comstr_start /= 0) return
        tabs = ""

        if (len(shell_here_document_word) > 0) then
            if (shell_here_document_word(1:1) == "-") then
                n = current_indentation()/max(1_int64, tab_width%value)
                tabs = repeat(TAB, int(n))
            end if
        end if

        delim = ""

        do i = 1, len(shell_here_document_word)
            if (shell_here_document_word(i:i) /= "'" .and. shell_here_document_word(i:i) /= '"') then
                delim = delim//shell_here_document_word(i:i)
            end if
        end do

        ! If we're at <<-, we don't want to delete the previous char.
        if (preceding_character() /= iachar("-")) call delete_character(-1_int64, status)
        if (.not. status%ok()) return
        call insert(shell_here_document_word, status)
        if (.not. (is_end_of_line() .or. looking_at("[ "//TAB//"]", status))) call insert(" ", status)
        call end_of_line(1_int64)

        do while (is_shell_quoted())
            call end_of_line(2_int64)
        end do

        call insert(LF//tabs, status)
        here = point()
        ! (replace-regexp-in-string "\\`-?[ \t]*" "" delim)
        i = 1

        if (len(delim) > 0) then
            if (delim(1:1) == "-") i = 2
        end if

        k = verify(delim(i:), " "//TAB)
        i = merge(len(delim) + 1, i + k - 1, k == 0)
        call insert(LF//tabs//delim(i:), status)
        call goto_character(here)
    end subroutine shell_maybe_here_document

    !> Make << insert a here document skeleton.  Port of
    !> sh-script.el:sh-electric-here-document-mode.
    recursive subroutine shell_electric_here_document_mode(status)
        type(status_type), intent(out) :: status

        ! Called as a command it toggles; from Fortran (a hook) it turns
        ! the mode on, as calling it with no argument does upstream.
        if (associated(this_command, commands%shell_electric_here_document_mode)) then
            call minor_modes%shell_electric_here_document_mode%command(status)
        else
            call minor_modes%shell_electric_here_document_mode%enable(status)
        end if
    end subroutine shell_electric_here_document_mode

    ! The body of `shell_electric_here_document_mode'.  Not a port.
    recursive subroutine shell_electric_here_document_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (enable) then
            call post_self_insert_hook%prepend(shell_maybe_here_document, local=.true.)
        else
            call post_self_insert_hook%remove(shell_maybe_here_document, local=.true.)
        end if
    end subroutine shell_electric_here_document_mode_body

    !> Move point to successive beginnings of commands.  Port of
    !> sh-script.el:sh-beginning-of-command.
    subroutine execute_shell_beginning_of_command(status)
        type(status_type), intent(out) :: status
        integer(int64) :: found

        ! FIXME: Redefine using SIMPLE_MINDED_INDENTATION_ENGINE.
        call regular_expression_search_backward(shell_beginning_of_command, found, status, noerror=SEARCH_NOERROR)
        if (found /= 0) call goto_character(match_beginning(2, status))
    end subroutine execute_shell_beginning_of_command

    !> Move point to successive ends of commands.  Port of
    !> sh-script.el:sh-end-of-command.
    subroutine execute_shell_end_of_command(status)
        type(status_type), intent(out) :: status
        integer(int64) :: found

        ! FIXME: Redefine using SIMPLE_MINDED_INDENTATION_ENGINE.
        call regular_expression_search_forward(shell_end_of_command, found, status, noerror=SEARCH_NOERROR)
        if (found /= 0) call goto_character(match_end(1, status))
    end subroutine execute_shell_end_of_command

    !> Insert, align, or delete end-of-line backslashes on the lines in the region.
    !> With no argument, inserts backslashes and aligns existing backslashes.
    !> With an argument, deletes the backslashes.
    !>
    !> This function does not modify the last line of the region if the region ends
    !> right at the start of the following line; it does not modify blank lines
    !> at the start of the region.  So you can put the region around an entire
    !> shell command and conveniently use this command.  Port of
    !> sh-script.el:sh-backslash-region.
    subroutine shell_backslash_region(from, to, delete_flag, status)
        integer(int64), intent(in) :: from, to
        logical, intent(in) :: delete_flag
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: endmark
        integer(int64) :: here, column, moved, adjusted, tw, next

        status = status_type()
        here = point()
        call goto_character(from)
        column = shell_backslash_column
        call copy_marker_at(to, endmark, .false., status)

        ! Compute the smallest column number past the ends of all the lines.
        if (shell_backslash_align) then
            if (.not. delete_flag) then
                do while (point() < to)
                    call end_of_line()

                    if (preceding_character() == iachar("\")) then
                        call forward_character(status, -1_int64)
                        moved = skip_characters_backward(" "//TAB, status)
                    end if

                    column = max(column, 1 + current_column())
                    moved = forward_line(1_int64)
                end do
            end if

            ! Adjust upward to a tab column, if that doesn't push
            ! past the margin.
            tw = tab_width%value

            if (mod(column, tw) > 0) then
                adjusted = ((column + tw - 1)/tw)*tw
                if (adjusted < window_width()) column = adjusted
            end if
        end if

        ! Don't modify blank lines at start of region.
        call goto_character(from)

        do while (point() < marked_position(endmark) .and. is_end_of_line())
            moved = forward_line(1_int64)
        end do

        ! Add or remove backslashes on all the lines.
        do while (point() < marked_position(endmark))
            ! Don't backslashify the last line
            ! if the region ends right at the start of the next line.
            next = point()
            moved = forward_line(1_int64)

            if (.not. (point() < marked_position(endmark))) then
                call goto_character(next)
                exit
            end if

            call goto_character(next)

            if (.not. delete_flag) then
                call shell_append_backslash(column, status)
            else
                call shell_delete_backslash(status)
            end if

            moved = forward_line(1_int64)
        end do

        call destroy_marker(endmark, st)
        call goto_character(here)
    end subroutine shell_backslash_region

    ! `shell_backslash_region' as a command.
    subroutine execute_shell_backslash_region(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: mark
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        ! (save-excursion ...)
        call copy_marker_at(point(), mark, .false., status)
        call shell_backslash_region(b, e, current_prefix_argument%kind /= PREFIX_NONE, status)
        call goto_character(marked_position(mark))
        call destroy_marker(mark, st)
    end subroutine execute_shell_backslash_region

    ! The width of the selected window (window-width).  Not a port.
    integer function window_width()
        window_width = window_text_columns(frame_selected_window_reference())
    end function window_width

    ! Port of sh-script.el:sh-append-backslash.
    subroutine shell_append_backslash(column, status)
        integer(int64), intent(in) :: column
        type(status_type), intent(out) :: status
        integer(int64) :: minimum, column_number

        call end_of_line()
        minimum = 1
        if (shell_backslash_align) minimum = 0

        ! Note that "\\\\" is needed to get one backslash.
        if (preceding_character() == iachar("\")) then
            call forward_character(status, -1_int64)
            call delete_horizontal_space(.false., status)
            column_number = indent_to(column, status, minimum)
        else
            column_number = indent_to(column, status, minimum)
            call insert("\", status)
        end if
    end subroutine shell_append_backslash

    ! Port of sh-script.el:sh-delete-backslash.
    subroutine shell_delete_backslash(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved, here

        status = status_type()
        call end_of_line()
        if (is_beginning_of_line()) return
        call forward_character(status, -1_int64)

        if (looking_at("\\", status)) then
            here = point() + 1
            moved = skip_characters_backward(" "//TAB, status)
            call delete_region(here, point(), status)
        end if
    end subroutine shell_delete_backslash

    !> Find the name of function or variable at point.
    !> For use in `add_log_current_subprogram_function' (unallocated for none).
    !> Port of sh-script.el:sh-current-defun-name.
    recursive subroutine shell_current_function_name(name)
        character(len=:), allocatable, intent(out) :: name
        type(status_type) :: st
        integer(int64) :: here, found
        integer :: n
        logical :: matched

        here = point()
        call end_of_line()
        ! function FOO, function FOO(), FOO() and FOO=.
        call regular_expression_search_backward("\(?:"// &
                                                "^\s-*function\s-+\([[:alpha:]_][[:alnum:]_]*\)\s-*\(?:()\)?"// &
                                                "\)\|\(?:"// &
                                                "^\s-*\([[:alpha:]_][[:alnum:]_]*\)\s-*()"// &
                                                "\)\|\(?:"// &
                                                "^\([[:alpha:]_][[:alnum:]_]*\)="// &
                                                "\)", found, st, noerror=SEARCH_NOERROR)

        if (found /= 0) then
            do n = 1, 3
                name = match_string(n, matched=matched)
                if (matched) exit
                deallocate (name)
            end do
        end if

        call goto_character(here)
    end subroutine shell_current_function_name

    !> Make a regexp which matches WORD as a word.
    !> This specifically excludes an occurrence of WORD followed by
    !> punctuation characters like `-'.  Port of
    !> sh-script.el:sh-mkword-regexpr.
    pure function shell_mkword_regexpr(word) result(regular_expression)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: regular_expression

        regular_expression = word//"\([^-[:alnum:]_]\|$\)"
    end function shell_mkword_regexpr

    ! ------------------------------------------------------------------
    ! Inferior shell interaction
    ! ------------------------------------------------------------------

    !> Get a shell process for interaction.
    !> If FORCE is true and no process found, create one.  Port of
    !> sh-script.el:sh-shell-process.
    recursive subroutine shell_shell_process(force, process, status)
        logical, intent(in) :: force
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: processes(:)
        type(string_type), allocatable :: command(:)
        type(handle_type) :: saved_buffer, shell_buffer
        type(display_action_type) :: saved_action
        character(len=:), allocatable :: saved_file
        logical :: had_file
        integer :: k, i

        status = status_type()
        call make_local_entry(k)

        if (is_process_live(locals(k)%process)) then
            process = locals(k)%process
            return
        end if

        process = handle_type()
        processes = process_list()

        do i = 1, size(processes)
            if (.not. is_process_live(processes(i))) exit
            call process_command(processes(i), command)
            if (size(command) == 0) exit

            if (file_name_nondirectory(command(1)%text) == shell_shell%value) then
                process = processes(i)
                exit
            end if
        end do

        if (process%index == 0 .and. force) then
            ! (let ((explicit-shell-file-name sh-shell-file)
            !       (display-buffer-overriding-action
            !        '(nil . ((inhibit-same-window . t)))))
            !   (save-current-buffer (shell)))
            ! We must prevent this `(shell)' call from switching buffers,
            ! so that the variable `sh-shell-process' is set locally in
            ! the correct buffer.
            had_file = allocated(explicit_shell_file_name%value)
            if (had_file) saved_file = explicit_shell_file_name%value
            explicit_shell_file_name = shell_shell_file%value
            saved_action = display_buffer_overriding_action
            display_buffer_overriding_action = display_action_type(has_inhibit_same_window=.true., &
                                                                   inhibit_same_window=.true.)
            saved_buffer = current_buffer()
            call shell(shell_buffer, status)
            call set_buffer(saved_buffer, status)
            display_buffer_overriding_action = saved_action

            if (had_file) then
                explicit_shell_file_name = saved_file
            else
                call explicit_shell_file_name%clear()
            end if

            if (.not. status%ok()) return
            process = get_buffer_process(shell_buffer)
        end if

        call make_local_entry(k)
        locals(k)%process = process
    end subroutine shell_shell_process

    !> Pop the shell interaction buffer.  Port of sh-script.el:sh-show-shell.
    recursive subroutine shell_show_shell(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process

        call shell_shell_process(.true., process, status)
        if (.not. status%ok()) return
        call pop_to_buffer(process_buffer(process), status, [DISPLAY_ACTION_SAME_WINDOW])
    end subroutine shell_show_shell

    !> Send TEXT to `shell_shell_process'.  Port of sh-script.el:sh-send-text.
    recursive subroutine shell_send_text(text, status)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: process

        call shell_shell_process(.true., process, status)
        if (.not. status%ok()) return
        call command_interpreter_send_string(process, text//LF, status)
    end subroutine shell_send_text

    !> Change directory in the current interaction shell to the current one.
    !> Port of sh-script.el:sh-cd-here.
    recursive subroutine shell_cd_here(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory

        directory = "./"
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        call shell_send_text("cd "//directory, status)
    end subroutine shell_cd_here

    !> Send the current line to the inferior shell and step to the next line.
    !> When the region is active, send the region instead.  Port of
    !> sh-script.el:sh-send-line-or-region-and-step.
    recursive subroutine shell_send_line_or_region_and_step(status)
        use editing_functions_mod, only: region_beginning, region_end
        type(status_type), intent(out) :: status
        integer(int64) :: from, to, end
        character(len=:), allocatable :: text

        if (is_use_region()) then
            call region_beginning(from, status)
            if (status%ok()) call region_end(to, status)
            if (.not. status%ok()) return
            end = to
        else
            from = line_beginning_position()
            to = line_end_position()
            end = to + 1
        end if

        call buffer_substring(from, to, text, status)
        if (.not. status%ok()) return
        call shell_send_text(text, status)
        if (.not. status%ok()) return
        call goto_character(min(end, point_maximum()))
    end subroutine shell_send_line_or_region_and_step

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    !> The variables assigned before point.  Port of
    !> sh-script.el:sh--vars-before-point.
    function shell_variables_before_point() result(variables)
        type(value_type) :: variables
        type(value_type), allocatable :: items(:)
        type(value_type) :: item
        integer(int64) :: original_point, found
        type(status_type) :: st

        original_point = point()
        allocate (items(0))

        do
            call regular_expression_search_backward("^[ "//TAB//"]*\([[:alnum:]_]+\)=", found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            item = value_type(text=match_string(1))
            items = [item, items]
        end do

        call goto_character(original_point)
        variables = make_list(items)
    end function shell_variables_before_point

    ! The directories of `exec_path' (PATH; an empty entry is the default
    ! directory, given as an empty name).
    ! Not a port.
    subroutine exec_path_directories(directories)
        character(len=:), allocatable, intent(out) :: directories(:)
        character(len=:), allocatable :: path
        logical :: found
        integer :: b, e, n, longest, k

        call getenv("PATH", path, found)

        if (.not. found) then
            allocate (character(len=1) :: directories(0))
            return
        end if

        n = 1
        longest = 0
        b = 1

        do k = 1, len(path)
            if (path(k:k) == ":") then
                n = n + 1
                longest = max(longest, k - b)
                b = k + 1
            end if
        end do

        longest = max(longest, len(path) - b + 1, 1)
        allocate (character(len=longest) :: directories(n))
        n = 0
        b = 1

        do while (b <= len(path) + 1)
            e = index(path(b:), ":")

            if (e == 0) then
                e = len(path) + 1
            else
                e = b + e - 1
            end if

            n = n + 1
            directories(n) = path(b:e - 1)
            b = e + 1
        end do
    end subroutine exec_path_directories

    !> The candidates of `shell_completion_at_point_function' for STRING
    !> before a keyword position: the top-level names of the Imenu index,
    !> the variables assigned before point (with "="), the executables and
    !> the keywords.  Port of sh-script.el:sh--cmd-completion-table-gen.
    recursive function shell_command_completion_table_gen(string) result(candidates)
        character(len=*), intent(in) :: string
        type(value_type) :: candidates
        type(value_type), allocatable :: items(:)
        type(value_type) :: variables, files
        type(index_menu_index_type) :: index
        character(len=:), allocatable :: directories(:)
        integer, allocatable :: top(:)
        type(status_type) :: st
        integer(int64) :: k, nv, nf, ni
        integer :: j

        ! The names of the Imenu index (none where it is unavailable).
        call make_index_menu_index(index, st)
        allocate (top(0))
        if (st%ok()) top = index%children(0)
        ni = size(top)
        variables = shell_variables_before_point()
        call exec_path_directories(directories)
        call locate_file_completion_table(directories, [""], string, COMPLETE_ALL, files, st)
        nv = list_length(variables)
        nf = list_length(files)
        allocate (items(ni + nv + nf + size(shell_completion_keywords)))

        do k = 1, ni
            items(k) = value_type(text=index%entries(top(k))%name)
        end do

        do k = 1, nv
            items(ni + k) = value_type(text=xstring(list_element(variables, k))//"=")
        end do

        do k = 1, nf
            items(ni + nv + k) = list_element(files, k)
        end do

        do j = 1, size(shell_completion_keywords)
            items(ni + nv + nf + j) = value_type(text=trim(shell_completion_keywords(j)))
        end do

        candidates = make_list(items)
    end function shell_command_completion_table_gen

    ! The completion table of commands (completion-table-with-cache of
    ! `shell_command_completion_table_gen').
    ! Port of sh-script.el:sh-completion-at-point-function (in part: its table).
    recursive subroutine shell_command_completion_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        logical :: cached

        status = status_type()

        if (action == COMPLETE_BOUNDARIES) then
            result = value_type()
            return
        end if

        cached = .false.

        if (allocated(command_cache_argument)) then
            if (len(command_cache_argument) <= len(string)) cached = string(1:len(command_cache_argument)) == command_cache_argument
        end if

        if (.not. cached) then
            command_cache_result = shell_command_completion_table_gen(string)
            command_cache_argument = string
        end if

        table%list = command_cache_result
        call complete_with_action(action, table, string, result, status, test, suffix)
    end subroutine shell_command_completion_table

    !> The completion data of the variable or command name at point (a
    !> function of `completion_at_point_functions').  Port of
    !> sh-script.el:sh-completion-at-point-function.
    recursive subroutine shell_completion_at_point_function(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, start, end, moved

        status = status_type()
        original_point = point()
        moved = skip_characters_forward("[:alnum:]_", status)
        end = point()
        moved = skip_characters_backward("[:alnum:]_", status)
        start = point()

        if (preceding_character() == iachar("$")) then
            data%found = .true.
            data%start = start
            data%end = end
            data%table%list = shell_variables_before_point()
        else if (is_shell_indentation_engine_keyword()) then
            data%found = .true.
            data%start = start
            data%end = end
            data%table%kind = TABLE_FUNCTION
            data%table%fn => shell_command_completion_table
            if (allocated(command_cache_argument)) deallocate (command_cache_argument)
        end if

        call goto_character(original_point)
    end subroutine shell_completion_at_point_function

    ! ------------------------------------------------------------------
    ! Styles
    ! ------------------------------------------------------------------

    ! The values of the indentation variables (sh-var-list).
    ! Not a port.
    function current_style_values() result(values)
        type(shell_indent_type) :: values(15)

        values(1)%columns = int(shell_basic_offset%value)
        values(2) = style_value(shell_indent_after_case)
        values(3) = style_value(shell_indent_after_do)
        values(4) = style_value(shell_indent_after_done)
        values(5) = style_value(shell_indent_after_else)
        values(6) = style_value(shell_indent_after_if)
        values(7) = style_value(shell_indent_after_loop_construct)
        values(8) = style_value(shell_indent_after_open)
        values(9) = style_value(shell_indent_for_case_alt)
        values(10) = style_value(shell_indent_for_case_label)
        values(11) = style_value(shell_indent_for_continuation)
        values(12) = style_value(shell_indent_for_do)
        values(13) = style_value(shell_indent_for_done)
        values(14) = style_value(shell_indent_for_fi)
        values(15) = style_value(shell_indent_for_then)

    contains

        ! The value of indentation variable VARIABLE in a style.
        function style_value(variable) result(v)
            type(shell_indent_choice_setting_type), intent(in) :: variable
            type(shell_indent_type) :: v

            if (variable%columns /= COLUMNS_UNSET) then
                v%columns = variable%columns
            else if (variable%choice >= SHELL_PLUS .and. variable%choice <= SHELL_SLASH) then
                v%choice = variable%choice
            end if
        end function style_value
    end function current_style_values

    ! The index of the style NAME in `shell_styles', 0 if none.
    ! Not a port.
    integer function style_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, size(shell_styles)
            if (trim(shell_styles(k)%name) == name) return
        end do

        k = 0
    end function style_index

    !> Name the current indentation settings as a style called NAME.
    !> If this name exists, the command will prompt whether it should be
    !> overwritten if
    !> - - it was called interactively with a prefix argument, or
    !> - - called non-interactively with optional CONFIRM_OVERWRITE true.
    !> Port of sh-script.el:sh-name-style.
    recursive subroutine shell_name_style(name, status, confirm_overwrite)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: confirm_overwrite
        type(shell_style_type) :: style
        logical :: confirm, ok
        integer :: k

        status = status_type()
        confirm = .false.
        if (present(confirm_overwrite)) confirm = confirm_overwrite
        k = style_index(name)

        if (k > 0) then
            ok = .true.

            if (confirm) then
                call read_y_or_n("This style exists.  Overwrite it? ", ok, status)
                if (.not. status%ok()) return
            end if

            if (.not. ok) then
                call message("Not changing style "//name)
            else
                call message("Updating style "//name)
                shell_styles(k)%values = current_style_values()
            end if
        else
            call message("Creating new style "//name)
            style%name = name
            style%values = current_style_values()
            shell_styles = [style, shell_styles]
        end if
    end subroutine shell_name_style

    !> Name the current indentation settings as a style (the command).
    !> Port of sh-script.el:sh-name-style.
    recursive subroutine execute_shell_name_style(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_from_minibuffer("Name for this style? ", name, status)
        if (.not. status%ok()) return
        call shell_name_style(name, status, current_prefix_argument%kind == PREFIX_NONE)
    end subroutine execute_shell_name_style

    !> Set shell indentation values for this buffer from those in style NAME.
    !> Port of sh-script.el:sh-load-style.
    recursive subroutine shell_load_style(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = style_index(name)

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "shell_load_style: Style "//name//" not known")
            return
        end if

        associate (v => shell_styles(k)%values)
            shell_basic_offset = v(1)%columns
            call load(shell_indent_after_case, v(2))
            call load(shell_indent_after_do, v(3))
            call load(shell_indent_after_done, v(4))
            call load(shell_indent_after_else, v(5))
            call load(shell_indent_after_if, v(6))
            call load(shell_indent_after_loop_construct, v(7))
            call load(shell_indent_after_open, v(8))
            call load(shell_indent_for_case_alt, v(9))
            call load(shell_indent_for_case_label, v(10))
            call load(shell_indent_for_continuation, v(11))
            call load(shell_indent_for_do, v(12))
            call load(shell_indent_for_done, v(13))
            call load(shell_indent_for_fi, v(14))
            call load(shell_indent_for_then, v(15))
        end associate

    contains

        ! Set indentation variable VARIABLE to the style's value V.
        subroutine load(variable, v)
            type(shell_indent_choice_setting_type), intent(inout) :: variable
            type(shell_indent_type), intent(in) :: v

            if (v%choice /= 0) then
                variable = v%choice
            else
                ! (A plain state first: the assignment clears the columns.)
                variable = choice_type()
                variable%columns = v%columns
            end if
        end subroutine load
    end subroutine shell_load_style

    !> Set shell indentation values for this buffer from those in a style
    !> (the command).  Port of sh-script.el:sh-load-style.
    recursive subroutine execute_shell_load_style(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: name
        integer :: k

        allocate (names(size(shell_styles)))

        do k = 1, size(names)
            names(k) = value_type(text=trim(shell_styles(k)%name))
        end do

        table%list = make_list(names)
        call completing_read("Which style to use for this buffer? ", table, name, status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call shell_load_style(name, status)
    end subroutine execute_shell_load_style

    !> Save all current styles as Fortran to buffer BUFFER.
    !> This is always added to the end of the buffer.  Port of
    !> sh-script.el:sh-save-styles-to-buffer (an assignment of
    !> `shell_styles', for init.f90, in place of an assignment).
    recursive subroutine shell_save_styles_to_buffer(buffer_handle, status)
        character(len=*), intent(in) :: buffer_handle
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, saved
        character(len=:), allocatable :: text
        integer :: k, j

        call get_or_make_buffer(buffer_handle, buffer, status)
        if (.not. status%ok()) return
        saved = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return

        if (size(shell_styles) == 0) then
            text = "shell_styles = [shell_style_type ::]"//LF
        else
            text = "shell_styles = ["

            do k = 1, size(shell_styles)
                if (k > 1) text = text//", &"//LF//"                   "
                text = text//"shell_style_type(name="""//trim(shell_styles(k)%name)//""", values=["

                do j = 1, size(shell_styles(k)%values)
                    if (j > 1) text = text//", &"//LF//"                       "
                    text = text//value_text(shell_styles(k)%values(j))
                end do

                text = text//"])"
            end do

            text = text//"]"//LF
        end if

        call goto_character(point_maximum())
        call insert(LF//text, status)
        call set_buffer(saved, status)

    contains

        ! An indentation value as Fortran: its `shell_indent_type'.
        function value_text(v) result(t)
            type(shell_indent_type), intent(in) :: v
            character(len=:), allocatable :: t
            character(len=16) :: digits

            select case (v%choice)
            case (SHELL_PLUS)
                t = "shell_indent_type(choice=SHELL_PLUS)"
            case (SHELL_MINUS)
                t = "shell_indent_type(choice=SHELL_MINUS)"
            case (SHELL_PLUS_PLUS)
                t = "shell_indent_type(choice=SHELL_PLUS_PLUS)"
            case (SHELL_MINUS_MINUS)
                t = "shell_indent_type(choice=SHELL_MINUS_MINUS)"
            case (SHELL_STAR)
                t = "shell_indent_type(choice=SHELL_STAR)"
            case (SHELL_SLASH)
                t = "shell_indent_type(choice=SHELL_SLASH)"
            case default
                write (digits, "(i0)") v%columns
                t = "shell_indent_type(columns="//trim(digits)//")"
            end select
        end function value_text
    end subroutine shell_save_styles_to_buffer

    !> Save all current styles to a buffer (the command).  Port of
    !> sh-script.el:sh-save-styles-to-buffer.
    recursive subroutine execute_shell_save_styles_to_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: buffer

        call read_from_minibuffer("Buffer to save styles in? ", buffer, status, initial_input="*scratch*")
        if (.not. status%ok()) return
        call shell_save_styles_to_buffer(buffer, status)
    end subroutine execute_shell_save_styles_to_buffer

    ! ------------------------------------------------------------------
    ! Statement skeletons
    ! ------------------------------------------------------------------

    !> Modify a copy of SKELETON by replacing the element at INDEXES(1) (0
    !> being the interactor) with REPLACEMENTS(1), at INDEXES(2) with
    !> REPLACEMENTS(2) ...  Port of sh-script.el:sh-modify.
    function shell_modify(skeleton, indexes, replacements) result(out)
        type(skeleton_element_type), intent(in) :: skeleton(:)
        integer, intent(in) :: indexes(:)
        type(skeleton_element_type), intent(in) :: replacements(:)
        type(skeleton_element_type), allocatable :: out(:)
        integer :: i, n, next, k

        allocate (out(0))
        i = 1
        n = 0

        do while (i <= size(skeleton))
            next = skeleton_next_element(skeleton, i)
            k = findloc(indexes, n, dim=1)

            if (k > 0) then
                out = [out, replacements(k)]
            else
                out = [out, skeleton(i:min(next, size(skeleton) + 1) - 1)]
            end if

            i = next
            n = n + 1
        end do
    end function shell_modify

    !> The skeleton of VARIANTS for the current shell (see `shell_feature'):
    !> sh-mode's `skeleton_filter_function'.  Port of
    !> sh-script.el:sh-feature (for skeletons).
    recursive subroutine shell_skeleton_feature(variants, skeleton, status)
        type(skeleton_variants_type), intent(in) :: variants
        type(skeleton_element_type), allocatable, intent(out) :: skeleton(:)
        type(status_type), intent(out) :: status
        character(len=16), allocatable :: names(:)
        integer :: i, k

        status = status_type()
        allocate (names(size(variants%entries)))

        do i = 1, size(names)
            names(i) = variants%entries(i)%name
        end do

        k = shell_feature_shell(names, current_shell())

        if (k > 0) then
            skeleton = variants%entries(k)%elements
        else
            allocate (skeleton(0))
        end if
    end subroutine shell_skeleton_feature

    ! <: delete back to the previous indentation step (sh-mode's further
    ! element, (- (min sh-basic-offset (current-column)))).  Not a port.
    recursive subroutine outdent_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%value = [skeleton_delete(int(min(shell_basic_offset%value, current_column())))]
    end subroutine outdent_value

    ! '<: the quoted outdent element of the csh variants, which, evaluated
    ! for its side effects, does nothing.  Not a port.
    recursive subroutine quoted_outdent_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine quoted_outdent_action

    ! (read-string PROMPT) as a literal element.  Not a port.
    recursive subroutine read_value(prompt, status)
        character(len=*), intent(in) :: prompt
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call read_string(prompt, answer, status)
        if (status%ok()) skeleton_state%value = [skeleton_literal(answer)]
    end subroutine read_value

    ! (read-string "pattern: ").  Not a port.
    recursive subroutine pattern_value(status)
        type(status_type), intent(out) :: status

        call read_value("pattern: ", status)
    end subroutine pattern_value

    ! (read-string "upper limit: ").  Not a port.
    recursive subroutine upper_limit_value(status)
        type(status_type), intent(out) :: status

        call read_value("upper limit: ", status)
    end subroutine upper_limit_value

    ! string, or unallocated.  Not a port.
    function string_value() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(skeleton_state%string)) s = skeleton_state%string
    end function string_value

    ! (sh-remember-variable string).  Not a port.
    recursive subroutine remember_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call shell_remember_variable(string_value())
        skeleton_state%value = [skeleton_literal(string_value())]
    end subroutine remember_value

    ! (sh-add (sh-remember-variable string) 1).  Not a port.
    recursive subroutine add_one_value(status)
        type(status_type), intent(out) :: status

        call shell_remember_variable(string_value())
        call shell_add(string_value(), 1_int64, status)
    end subroutine add_one_value

    ! (sh-add "OPTIND" -1).  Not a port.
    recursive subroutine optind_value(status)
        type(status_type), intent(out) :: status

        call shell_add("OPTIND", -1_int64, status)
    end subroutine optind_value

    ! null-device.  Not a port.
    recursive subroutine null_device_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%value = [skeleton_literal("/dev/null")]
    end subroutine null_device_value

    ! (and (goto-char (point-max)) (not (is_beginning_of_line)) ?\n).  Not a port.
    recursive subroutine end_newline_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call goto_character(point_maximum())
        if (.not. is_beginning_of_line()) skeleton_state%value = [skeleton_literal(LF)]
    end subroutine end_newline_value

    ! sh-non-closing-paren: a ")" with punctuation syntax, not
    ! rear-sticky.  Port of sh-script.el sh-non-closing-paren.
    recursive subroutine non_closing_parenthesis_action(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start

        start = point()
        call insert(")", status)
        if (.not. status%ok()) return
        call put_syntax(start, point(), ".")
        call put_text_property(start, point(), value_type(name="rear_nonsticky"), value_type(.true.), status)
    end subroutine non_closing_parenthesis_action

    ! '(setq input (sh-feature sh-test)).  Not a port.
    recursive subroutine test_input_action(status)
        type(status_type), intent(out) :: status
        character(len=16) :: names(size(shell_test))
        integer :: k

        status = status_type()
        names = shell_test%shell
        k = shell_feature_shell(names, current_shell())

        if (k > 0) then
            skeleton_state%initial_input = trim(shell_test(k)%input)
            skeleton_state%initial_position = shell_test(k)%position
        end if
    end subroutine test_input_action

    ! (file-name-nondirectory (buffer-file-name)).  Not a port.
    recursive subroutine file_name_interactor(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. allocated(buffers(current)%filename)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected character(len=*), got unallocated")
            return
        end if

        string = file_name_nondirectory(buffers(current)%filename)
    end subroutine file_name_interactor

    ! '(setq v1 (append (vconcat string) nil)): the option letters left.
    ! Not a port.
    recursive subroutine getopts_letters_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
        getopts_letters = string_value()
    end subroutine getopts_letters_action

    ! The interactor of the options of sh's while getopts loop: the next
    ! letter, setting v2 to "$OPTARG" when it takes an argument.
    ! Not a port.
    recursive subroutine getopts_letter_interactor(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. allocated(getopts_letters)) getopts_letters = ""
        if (len(getopts_letters) > 0) string = getopts_letters(1:1)

        if (index(getopts_letters, ":") == 2) then
            getopts_letters = getopts_letters(3:)
            skeleton_state%v2 = [skeleton_literal('"$OPTARG"')]
        else
            if (len(getopts_letters) > 0) getopts_letters = getopts_letters(2:)
            if (allocated(skeleton_state%v2)) deallocate (skeleton_state%v2)
            allocate (skeleton_state%v2(0))
        end if
    end subroutine getopts_letter_interactor

    ! '(setq v1 (point)).  Not a port.
    recursive subroutine getopts_mark_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
        getopts_mark = point()
    end subroutine getopts_mark_action

    ! '(save-excursion (while (search-backward ":" v1 t) (replace-match
    ! " ARGUMENT] [+-" t t))).  Not a port.
    recursive subroutine getopts_arguments_action(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: found, back

        status = status_type()
        call copy_marker_at(point(), m, .false., st)

        do
            call search_backward(":", found, status, bound=getopts_mark, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit
            call replace_match(" ARG] [+-", status, fixedcase=.true., literal=.true.)
            if (.not. status%ok()) exit
            call goto_character(match_beginning(0, st))
        end do

        call marker_position(m, back, st)
        call goto_character(back)
        call destroy_marker(m, st)
    end subroutine getopts_arguments_action

    ! (if (eq (preceding-char) ?-) -5).  Not a port.
    recursive subroutine getopts_dash_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (preceding_character() == iachar("-")) skeleton_state%value = [skeleton_delete(5)]
    end subroutine getopts_dash_value

    ! (if (and (sequencep v1) (length v1)) "] " "} "): v1 is the position
    ! set before, no sequence.  Not a port.
    recursive subroutine getopts_close_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%value = [skeleton_literal("} ")]
    end subroutine getopts_close_value

    ! '(setq input '("- x" . 2)).  Not a port.
    recursive subroutine csh_option_input_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%initial_input = "- x"
        skeleton_state%initial_position = 2
    end subroutine csh_option_input_action

    ! '(eval string): read string now.  Not a port.
    recursive subroutine read_string_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (skeleton_state%string_pending) call skeleton_read_string(status)
    end subroutine read_string_action

    ! '(if (string-match " +" string) (setq v1 (substring string (match-end 0))
    ! string (substring string 0 (match-beginning 0))) (setq v1 nil)).
    ! Not a port.
    recursive subroutine csh_option_split_action(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s
        integer :: i, j

        status = status_type()
        s = string_value()
        i = index(s, " ")
        if (allocated(skeleton_state%v1)) deallocate (skeleton_state%v1)
        allocate (skeleton_state%v1(0))
        if (i == 0) return
        j = i

        do while (j < len(s))
            if (s(j + 1:j + 1) /= " ") exit
            j = j + 1
        end do

        skeleton_state%v1 = [skeleton_literal(s(j + 1:))]
        skeleton_state%string = s(1:i - 1)
    end subroutine csh_option_split_action

    ! (if v1 "shift").  Not a port.
    recursive subroutine csh_shift_value(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (allocated(skeleton_state%v1)) then
            if (size(skeleton_state%v1) > 0) skeleton_state%value = [skeleton_literal("shift")]
        end if
    end subroutine csh_shift_value

    ! A text element (`skeleton_text', shorter to write).  Not a port.
    function text(string) result(element)
        character(len=*), intent(in) :: string
        type(skeleton_element_type) :: element

        element = skeleton_text(string)
    end function text

    ! Make the statement skeletons.  Port of sh-script.el's
    ! define-skeleton forms (sh-case, sh-for, sh-indexed-loop,
    ! sh-function, sh-if, sh-repeat, sh-select, sh-tmp-file, sh-until,
    ! sh-while, sh-while-getopts).
    subroutine initialize_skeletons()
        type(skeleton_element_type), allocatable :: shell_name(:), rc(:), ksh88(:), posix(:), empty(:)
        type(skeleton_element_type) :: lt, ncp, quoted_lt, pattern, upper, remember, null_device, test_input
        type(skeleton_element_type), parameter :: N = SKELETON_NEWLINE, I = SKELETON_INDENT, P = SKELETON_POINT, &
                                                  S = SKELETON_STRING, E = SKELETON_END, R = SKELETON_RESUME

        allocate (empty(0))
        lt = skeleton_value(outdent_value)
        quoted_lt = skeleton_action(quoted_outdent_action)
        ncp = skeleton_action(non_closing_parenthesis_action)
        pattern = skeleton_value(pattern_value)
        upper = skeleton_value(upper_limit_value)
        remember = skeleton_value(remember_value)
        null_device = skeleton_value(null_device_value)
        test_input = skeleton_action(test_input_action)
        shell_test = [shell_test_entry_type(shell="sh", input="[  ]", position=3), &
                      shell_test_entry_type(shell="ksh88", input="[[  ]]", position=4)]
        ! sh-case
        call shell_case_skeletons%append("csh", [skeleton_prompt("expression: "), text("switch( "), S, text(" )"), N, I, &
                                                 text("case "), pattern, text(":"), N, I, P, N, text("breaksw"), N, &
                                              skeleton_prompt("other pattern, %s: "), lt, text("case "), S, text(":"), N, I, P, N, &
                                              text("breaksw"), N, E, lt, text("default:"), N, I, P, N, R, lt, lt, text("endsw"), N])
        call shell_case_skeletons%append("es", empty)
        call shell_case_skeletons%append("rc", [skeleton_prompt("expression: "), I, text("switch( "), S, text(" ) {"), N, I, &
                                                text("case "), pattern, N, I, P, N, skeleton_prompt("other pattern, %s: "), &
                                             text("case "), S, I, N, I, P, N, E, text("case *"), I, N, I, P, N, R, text("}"), I, N])
        call shell_case_skeletons%append("sh", [skeleton_prompt("expression: "), I, text("case "), S, text(" in"), N, &
                                                skeleton_prompt("pattern, %s: "), I, S, ncp, N, I, P, N, text(";;"), N, E, I, &
                                                text("*"), ncp, N, I, P, N, R, text("esac"), I, N])
        ! sh-for
        shell_name = [skeleton_prompt("Index variable: "), I, text("for "), S, text(" in "), P, text("; do"), N, I, P, &
                      SKELETON_IF_NOT_MOVED, text("$"), SKELETON_IF_MOVED, remember, N, text("done"), I, N]
        rc = shell_modify(shell_name, [2, 6, 15], [text("for( "), text(" ) {"), text("}")])
        call shell_for_skeletons%append("csh", shell_modify(shell_name, [1, 2, 4, 6, 15, 16], [text(""), text("foreach "), &
                                                                                               text(" ( "), &
                                                                                               text(" )"), quoted_lt, text("end")]))
        call shell_for_skeletons%append("es", shell_modify(rc, [4], [text(" = ")]))
        call shell_for_skeletons%append("rc", rc)
        call shell_for_skeletons%append("sh", shell_name)
        ! sh-indexed-loop
        posix = [skeleton_prompt("Index variable: "), I, S, text("=1"), N, text("while [ $"), S, text(" -le "), upper, &
                 text(" ]; do"), N, I, P, text("$"), S, N, S, text("="), skeleton_value(add_one_value), N, text("done"), I, N]
        rc = [skeleton_prompt("Index variable: "), I, text("for( "), S, text(" in"), &
              text(" `{awk 'BEGIN { for( i=1; i<="), upper, text("; i++ ) print i }'`}) {"), N, I, P, text("$"), remember, N, &
              text("}"), I, N]
        call shell_indexed_loop_skeletons%append("bash", posix)
        call shell_indexed_loop_skeletons%append("csh", [skeleton_prompt("Index variable: "), text("@ "), S, text(" = 1"), N, &
                                                         text("while( $"), S, text(" <= "), upper, text(" )"), N, I, P, &
                                                         text("$"), S, N, &
                                                         text("@ "), S, text("++"), N, lt, text("end"), N])
        call shell_indexed_loop_skeletons%append("es", shell_modify(rc, [4], [text(" =")]))
        call shell_indexed_loop_skeletons%append("ksh88", [skeleton_prompt("Index variable: "), I, text("integer "), S, &
                                                           text("=0"), N, I, text("while (( ( "), S, text(" += 1 ) <= "), upper, &
                                                           text(" )); do"), N, I, P, text("$"), remember, I, N, text("done"), I, N])
        call shell_indexed_loop_skeletons%append("posix", posix)
        call shell_indexed_loop_skeletons%append("rc", rc)
        call shell_indexed_loop_skeletons%append("sh", [skeleton_prompt("Index variable: "), I, text("for "), S, &
                                                        text(" in `awk 'BEGIN { for( i=1; i<="), upper, &
                                                        text("; i++ ) print i }'`; do"), N, I, P, text("$"), remember, N, &
                                                        text("done"), I, N])
        ! sh-function
        ksh88 = [skeleton_prompt("name: "), text("function "), S, text(" {"), N, I, P, N, lt, text("}"), N]
        call shell_function_skeletons%append("bash", shell_modify(ksh88, [3], [text("() {")]))
        call shell_function_skeletons%append("ksh88", ksh88)
        call shell_function_skeletons%append("rc", shell_modify(ksh88, [1], [text("fn ")]))
        call shell_function_skeletons%append("sh", [SKELETON_NO_INTERACTOR, text("() {"), N, I, P, N, lt, text("}"), N])
        ! sh-if
        call shell_if_skeletons%append("csh", [skeleton_prompt("condition: "), text("if( "), S, text(" ) then"), N, I, P, N, &
                                               skeleton_prompt("other condition, %s: "), lt, text("else if( "), S, &
                                             text(" ) then"), N, I, P, N, E, lt, text("else"), N, I, P, N, R, lt, text("endif"), N])
        call shell_if_skeletons%append("es", [skeleton_prompt("condition: "), I, text("if { "), S, text(" } {"), N, I, P, N, &
                                              skeleton_prompt("other condition, %s: "), text("} { "), S, text(" } {"), I, N, I, P, &
                                              N, E, text("} {"), I, N, I, P, N, R, text("}"), I, N])
        call shell_if_skeletons%append("rc", [skeleton_prompt("condition: "), I, text("if( "), S, text(" ) {"), N, I, P, N, &
                                             skeleton_prompt("other condition, %s: "), text("} else if( "), S, text(" ) {"), I, N, &
                                              I, P, N, E, text("} else {"), I, N, I, P, N, R, text("}"), I, N])
        call shell_if_skeletons%append("sh", [skeleton_prompt("condition: "), test_input, I, text("if "), S, text("; then"), N, &
                                              I, P, N, skeleton_prompt("other condition, %s: "), I, text("elif "), S, &
                                              text("; then"), I, N, I, N, E, text("else"), I, N, I, N, R, text("fi"), I, N])
        ! sh-repeat
        call shell_repeat_skeletons%append("es", [SKELETON_NO_INTERACTOR, I, text("forever {"), N, I, P, N, text("}"), I, N])
        call shell_repeat_skeletons%append("zsh", [skeleton_prompt("factor: "), I, text("repeat "), S, text("; do"), I, N, I, N, &
                                                   text("done"), I, N])
        ! sh-select
        ksh88 = [skeleton_prompt("Index variable: "), I, text("select "), S, text(" in "), P, text("; do"), N, I, text("$"), S, N, &
                 text("done"), I, N]
        call shell_select_skeletons%append("ksh88", ksh88)
        call shell_select_skeletons%append("bash", ksh88)
        ! sh-tmp-file
        shell_name = [skeleton_interactor(file_name_interactor), I, text("TMP=`mktemp -t "), S, text(".XXXXXX`"), N, &
                      text('trap "rm $TMP* 2>'), null_device, text('" '), text("0"), N]
        ksh88 = shell_modify(shell_name, [7], [text("EXIT")])
        call shell_temporary_file_skeletons%append("bash", ksh88)
        call shell_temporary_file_skeletons%append("csh", [skeleton_interactor(file_name_interactor), &
                                                           text("set tmp = `mktemp -t "), &
                                                           S, text(".XXXXXX`"), N, text("onintr exit"), N, P, &
                                                        skeleton_value(end_newline_value), text("exit:"//LF), text("rm $tmp* >&"), &
                                                           null_device, I, N])
        call shell_temporary_file_skeletons%append("es", [skeleton_interactor(file_name_interactor), I, &
                                                          text("local( signals = $signals sighup sigint;"), N, I, &
                                                          text("tmp = `{ mktemp -t "), S, text(".XXXXXX } ) {"), N, I, &
                                                          text("catch @ e {"), N, I, text("rm $tmp^* >[2]"), null_device, N, &
                                                          text("throw $e"), N, &
                                                          text("} {"), I, N, P, &
                                                          N, text("}"), I, N, text("}"), I, N])
        call shell_temporary_file_skeletons%append("ksh88", ksh88)
        call shell_temporary_file_skeletons%append("rc", [skeleton_interactor(file_name_interactor), I, &
                                                          text("tmp = `{ mktemp -t "), &
                                                        S, text(".XXXXXX }"), N, text("fn sigexit { rm $tmp^* >[2]"), null_device, &
                                                          text(" }"), N])
        call shell_temporary_file_skeletons%append("sh", shell_name)
        ! sh-until
        call shell_until_skeletons%append("sh", [skeleton_prompt("condition: "), test_input, I, text("until "), S, text("; do"), &
                                                 N, &
                                                 I, P, N, text("done"), I, N])
        ! sh-while
        shell_name = [skeleton_prompt("condition: "), test_input, I, text("while "), S, text("; do"), N, I, P, N, text("done"), I, &
                      N]
        call shell_while_skeletons%append("csh", shell_modify(shell_name, [2, 3, 5, 10, 11], [text(""), text("while( "), &
                                                                                              text(" )"), quoted_lt, &
                                                                                              text("end")]))
        call shell_while_skeletons%append("es", shell_modify(shell_name, [3, 5, 10], [text("while { "), text(" } {"), text("}")]))
        call shell_while_skeletons%append("rc", shell_modify(shell_name, [3, 5, 10], [text("while( "), text(" ) {"), text("}")]))
        call shell_while_skeletons%append("sh", shell_name)
        ! sh-while-getopts
        shell_name = [skeleton_prompt("optstring: "), I, text("while getopts :"), S, text(" OPT; do"), N, I, text("case $OPT in"), &
                      N, &
                     skeleton_action(getopts_letters_action), skeleton_interactor(getopts_letter_interactor), I, S, text("|+"), S, &
                      ncp, N, I, P, SKELETON_V2, N, I, text(";;"), N, E, I, text("*"), ncp, N, I, text("echo"), text(' "usage: '), &
                      text("`basename $0`"), text(" [+-"), skeleton_action(getopts_mark_action), S, &
                      skeleton_action(getopts_arguments_action), skeleton_value(getopts_dash_value), &
                      skeleton_value(getopts_close_value), text('[--] ARGS..."'), N, text("exit 2"), I, N, text("esac"), I, N, &
                      text("done"), I, N, text("shift "), skeleton_value(optind_value), N, text("OPTIND=1"), N]
        call shell_while_getopts_skeletons%append("bash", shell_modify(shell_name, [18], [text("${0##*/}")]))
        call shell_while_getopts_skeletons%append("csh", [SKELETON_NO_INTERACTOR, text("while( 1 )"), N, I, &
                                                          text('switch( "$1" )'), N, skeleton_action(csh_option_input_action), I, &
                                                          I, skeleton_prompt("option, %s: "), lt, text("case "), &
                                                          skeleton_action(read_string_action), &
                                                          skeleton_action(csh_option_split_action), S, text(":"), N, I, &
                                                          text("set "), SKELETON_V1, SKELETON_IF_MOVED, text(" = $2"), &
                                                          SKELETON_IF_NOT_MOVED, skeleton_delete(4), SKELETON_IF_MOVED, P, N, &
                                                          skeleton_value(csh_shift_value), SKELETON_IF_MOVED, N, &
                                                          text("breaksw"), N, E, lt, text("case --:"), N, I, text("shift"), N, lt, &
                                                          text("default:"), N, I, text("break"), N, R, lt, lt, text("endsw"), N, &
                                                          text("shift"), N, lt, text("end"), N])
        call shell_while_getopts_skeletons%append("ksh88", shell_modify(shell_name, [16, 18, 37], [text("print"), &
                                                                                                   text("${0##*/}"), &
                                                                                                   text("OPTIND-1")]))
        call shell_while_getopts_skeletons%append("posix", shell_modify(shell_name, [18], [text("$(basename $0)")]))
        call shell_while_getopts_skeletons%append("sh", shell_name)
    end subroutine initialize_skeletons

    !> Insert a case/switch statement.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-case.
    recursive subroutine shell_case(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_case_skeletons, status, string, argument)
    end subroutine shell_case

    !> Insert a for loop.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-for.
    recursive subroutine shell_for(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_for_skeletons, status, string, argument)
    end subroutine shell_for

    !> Insert an indexed loop from 1 to n.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-indexed-loop.
    recursive subroutine shell_indexed_loop(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_indexed_loop_skeletons, status, string, argument)
    end subroutine shell_indexed_loop

    !> Insert a function definition.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-function.
    recursive subroutine shell_function(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_function_skeletons, status, string, argument)
    end subroutine shell_function

    !> Insert an if statement.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-if.
    recursive subroutine shell_if(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_if_skeletons, status, string, argument)
    end subroutine shell_if

    !> Insert a repeat loop definition.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-repeat.
    recursive subroutine shell_repeat(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_repeat_skeletons, status, string, argument)
    end subroutine shell_repeat

    !> Insert a select statement.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-select.
    recursive subroutine shell_select(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_select_skeletons, status, string, argument)
    end subroutine shell_select

    !> Insert code to setup temporary file handling.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-tmp-file.
    recursive subroutine shell_temporary_file(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_temporary_file_skeletons, status, string, argument)
    end subroutine shell_temporary_file

    !> Insert an until loop.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-until.
    recursive subroutine shell_until(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_until_skeletons, status, string, argument)
    end subroutine shell_until

    !> Insert a while loop.  See `shell_feature'.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-while.
    recursive subroutine shell_while(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_while_skeletons, status, string, argument)
    end subroutine shell_while

    !> Insert a while getopts loop.  See `shell_feature'.
    !> Prompts for an options string which consists of letters for each recognized
    !> option followed by a colon `:' if the option accepts an argument.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sh-script.el:sh-while-getopts.
    recursive subroutine shell_while_getopts(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_variants_command(shell_while_getopts_skeletons, status, string, argument)
    end subroutine shell_while_getopts

    recursive subroutine execute_shell_case(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_case(status, argument=current_prefix_argument)
    end subroutine execute_shell_case

    recursive subroutine execute_shell_for(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_for(status, argument=current_prefix_argument)
    end subroutine execute_shell_for

    recursive subroutine execute_shell_indexed_loop(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_indexed_loop(status, argument=current_prefix_argument)
    end subroutine execute_shell_indexed_loop

    recursive subroutine execute_shell_function(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_function(status, argument=current_prefix_argument)
    end subroutine execute_shell_function

    recursive subroutine execute_shell_if(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_if(status, argument=current_prefix_argument)
    end subroutine execute_shell_if

    recursive subroutine execute_shell_repeat(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_repeat(status, argument=current_prefix_argument)
    end subroutine execute_shell_repeat

    recursive subroutine execute_shell_select(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_select(status, argument=current_prefix_argument)
    end subroutine execute_shell_select

    recursive subroutine execute_shell_temporary_file(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_temporary_file(status, argument=current_prefix_argument)
    end subroutine execute_shell_temporary_file

    recursive subroutine execute_shell_until(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_until(status, argument=current_prefix_argument)
    end subroutine execute_shell_until

    recursive subroutine execute_shell_while(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_while(status, argument=current_prefix_argument)
    end subroutine execute_shell_while

    recursive subroutine execute_shell_while_getopts(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call shell_while_getopts(status, argument=current_prefix_argument)
    end subroutine execute_shell_while_getopts

    !> Forget the number of columns of an indentation variable, as any
    !> assignment to it does.  Not a port.
    subroutine shell_indent_clear_kinds(self)
        class(shell_indent_choice_setting_type), intent(inout) :: self

        self%columns = COLUMNS_UNSET
    end subroutine shell_indent_clear_kinds

    !> The number of columns of an indentation variable as help shows it
    !> (`%columns = N'); "" when none is set.  Not a port.
    function shell_indent_other_value(self) result(text)
        class(shell_indent_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=16) :: digits

        text = ""

        if (self%columns /= COLUMNS_UNSET) then
            write (digits, "(i0)") self%columns
            text = "%columns = "//trim(digits)
        end if
    end function shell_indent_other_value

    !> The value a buffer keeps of an indentation variable: the number of
    !> columns, or the name of a named constant (upstream's + - ++ -- *
    !> /); FOUND is false for .true. and .false..  Not a port.
    subroutine shell_indent_get_kind_value(self, v, found)
        class(shell_indent_choice_setting_type), intent(in) :: self
        type(value_type), intent(out) :: v
        logical, intent(out) :: found
        character(len=2), parameter :: names(SHELL_PLUS:SHELL_SLASH) = [character(len=2) :: "+", "-", "++", "--", "*", "/"]

        found = .true.

        if (self%columns /= COLUMNS_UNSET) then
            v = value_type(int(self%columns, int64))
        else if (self%choice >= SHELL_PLUS .and. self%choice <= SHELL_SLASH) then
            v = value_type(name=trim(names(self%choice)))
        else
            found = .false.
            v = value_type()
        end if
    end subroutine shell_indent_get_kind_value

    !> Set an indentation variable from V, a value a buffer kept (as
    !> `shell_indent_get_kind_value' gives it); DONE is false when V is
    !> .true. or .false..  Not a port.
    subroutine shell_indent_set_kind_value(self, v, done)
        class(shell_indent_choice_setting_type), intent(inout) :: self
        type(value_type), intent(in) :: v
        logical, intent(out) :: done
        character(len=2), parameter :: names(SHELL_PLUS:SHELL_SLASH) = [character(len=2) :: "+", "-", "++", "--", "*", "/"]
        integer :: k

        done = .false.

        if (vtag(v) == V_INTEGER) then
            self = choice_type()
            self%columns = int(xfixnum(v))
            done = .true.
        else if (vtag(v) == V_NAME) then
            k = findloc(names, symbols(xsymbol(v))%name, dim=1)

            if (k > 0) then
                self = SHELL_PLUS + k - 1
                done = .true.
            end if
        end if
    end subroutine shell_indent_set_kind_value

    !> The assignment that sets the number an indentation variable takes, for the error of a rejected
    !> value.  Not a port.
    function shell_indent_kind_hint(self, number) result(text)
        class(shell_indent_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of columns is `"//trim(self%name)//"%columns = "//number//"'"
    end function shell_indent_kind_hint
end module shell_script_mod
