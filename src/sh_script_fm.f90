! sh_script_fm.f90 -- shell-script editing commands.
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
!> The shell-dependent settings are feature tables (`sh_feature'): one
!> entry per shell, an entry may append the value of another shell's
!> (sh-append), and a shell without an entry uses its ancestor's
!> (`sh_ancestor_table'), finally sh's.  The indentation variables take
!> integers or the named constants SH_PLUS (+), SH_MINUS (-),
!> SH_PLUS_PLUS (++), SH_MINUS_MINUS (--), SH_STAR (*) and SH_SLASH (/)
!> that `sh_var_value' interprets.
!>
!> The inferior shell commands (C-c C-z, C-c C-d, C-c C-n) find or start
!> the shell with `sh_shell_process'; completion at point offers the
!> variables assigned before point, commands and keywords
!> (`sh_completion_at_point_function'), then
!> `sh_dynamic_complete_functions' through comint.  `sh_styles_table' is
!> an array of `sh_style_type', saved as a Fortran assignment.
!>
!> Deferred: the statement skeletons (`sh_case', `sh_for', `sh_function',
!> `sh_if', `sh_indexed_loop', `sh_repeat', `sh_select', `sh_tmp_file',
!> `sh_until', `sh_while', `sh_while_getopts', their keys, `sh_test' and
!> the skeleton-pair settings; skeleton.el), the smie-config commands
!> (C-c ?, C-c =, C-c <, C-c >; smie-config is not ported),
!> `executable_interpret' (C-c C-x; needs `compilation_start' with a
!> buffer name function and rule specifications), Imenu
!> (`sh_imenu_generic_expression', and the Imenu candidates of
!> completion), `add_log_current_subprogram_function' (`sh_current_function_name'
!> is here), `outline_regexp', `escaped_string_quote' (with
!> `yank_in_context'), the Flymake backend (`sh_shellcheck_flymake'), the
!> `sh_shell' file-local variable (`hack_local_variables_hook'), the menu
!> and `bash_ts_mode' (tree-sitter).
module sh_script_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_ARITH_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, stringp, xstring, make_string, list, list_elements, t_value, make_list, nil, &
                       list_length, list_elt
    use lread_fm, only: sym
    use data_fm, only: integer_setting, string_setting, forward_setting, add_local_variables_killer, value_eq
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command
    use buffer_fm, only: buffers, cur, buffer_serial_live, barf_if_buffer_read_only, current_buffer, set_buffer, &
                         get_buffer_create
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes, minor_modes
    use keymap_fm, only: keymap_set, define_key
    use syntax_fm, only: copy_syntax_table, make_syntax_table, modify_syntax_entry, is_syntax_table_live, free_syntax_table, &
                         set_syntax_table, syntax_descriptor_type, string_to_syntax, parse_state_type, syntax_ppss, &
                         forward_comment, forward_word, skip_chars_forward, skip_chars_backward, skip_syntax_forward, &
                         skip_syntax_backward, &
                         syntax_descriptor_value, syntax_entry_at, ST_STRING_STYLE
    use editfns_fm, only: interactive_region, point, point_min, point_max, goto_char, is_bol, is_eol, is_bob, is_eob, &
                          char_after, char_before, delete_region, buffer_substring, line_beginning_position, &
                          line_end_position, preceding_char
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line, delete_char, self_insert_command, &
                       post_self_insert_hook
    use insdel_fm, only: insert
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use indent_fm, only: current_column, current_indentation, indent_to, indent_line_function
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_forward, match_beginning, match_end, &
                         regexp_quote, string_match, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string, looking_back
    use simple_fm, only: delete_horizontal_space, back_to_indentation, count_lines, shell_command_on_region, is_use_region
    use textprop_fm, only: get_text_property, put_text_property
    use xdisp_fm, only: message, set_mode_line_process, mode_line_part_type, tab_width
    use window_fm, only: window_text_cols
    use frame_fm, only: frame_selected_window_ref
    use files_fm, only: auto_mode_interpreter_regexp
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, font_lock_keywords_type, &
                            font_lock_syntax_entry_type, FONT_LOCK_OVERRIDE_YES
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_flush, font_lock_set_defaults, &
                            font_lock_forget_defaults
    use regexp_opt_fm, only: regexp_opt, REGEXP_OPT_GROUP
    use expression_motion_fm, only: forward_expression_default_function
    use newcomment_fm, only: comment_start, comment_start_skip
    use paragraphs_fm, only: paragraph_start, paragraph_separate, page_delimiter
    use comint_fm, only: comint_prompt_regexp, comint_dynamic_complete_functions, comint_filename_completion, &
                         comint_completion_at_point, comint_send_string
    use shell_fm, only: shell_command_completion, shell_environment_variable_completion, shell, explicit_shell_file_name
    use process_fm, only: is_process_live, process_list, process_command, process_buffer, get_buffer_process, &
                          process_arg_type
    use window_fm, only: pop_to_buffer, display_buffer_overriding_action, display_action_type, DISPLAY_ACTION_SAME_WINDOW
    use eval_fm, only: capf_hook_type, capf_entry_type
    use minibuf_table_fm, only: capf_data_type, TABLE_FUNCTION, COMPLETE_ALL, COMPLETE_BOUNDARIES, completion_predicate
    use minibuf_fm, only: complete_with_action, read_from_minibuffer
    use minibuffer_fm, only: completion_at_point_functions, REQUIRE_MATCH_YES
    use files_fm, only: locate_file_completion_table
    use expression_motion_fm, only: subprogram_prompt_regexp
    use subr_fm, only: read_y_or_n
    use abbrev_fm, only: abbrev_table_type, set_local_abbrev_table
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                        FACE_BACKGROUND_DARK
    use fileio_fm, only: file_name_nondirectory
    use env_fm, only: getenv
    use callproc_fm, only: shell_file_name
    use minibuf_fm, only: completion_table_type
    use minibuffer_fm, only: completing_read
    use replace_fm, only: format_prompt
    use electric_fm, only: electric_indent_mode
    use executable_fm, only: executable_set_magic, executable_font_lock_keywords, executable_query, &
                             EXECUTABLE_QUERY_FUNCTION
    use syntax_propertize_fm, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type, &
                                    syntax_property_type, syntax_propertize_extend_region_functions, &
                                    syntax_propertize_multiline
    use smie_fm, only: smie_bnf_type, smie_precs_type, smie_prec2_type, smie_grammar_type, smie_rule_result_type, &
                       smie_indent_type, smie_expression_type, smie_toklevels_type, smie_bnf_to_prec2, smie_prec2_to_grammar, &
                       smie_setup, smie_forward_token, smie_backward_token, smie_grammar_token, smie_backward_expression, &
                       is_smie_rule_parent, is_smie_rule_prev, is_smie_rule_hanging, is_smie_rule_bol, smie_rule_parent, &
                       smie_rule_separator, smie_indent_virtual, smie_indent_calculate, smie_indent_keyword, &
                       smie_indent_functions, smie_set_is_hanging_eol_function, is_smie_default_hanging_eol, &
                       SMIE_ASSOC, SMIE_AFTER, SMIE_BEFORE, SMIE_ELEM, SMIE_LIST_INTRO, SMIE_LEVEL_NUMBER, SMIE_RULE_NONE, &
                       SMIE_RULE_OFFSET, SMIE_RULE_COLUMN, SMIE_RULE_YES, SMIE_INDENT_COLUMN, SMIE_INDENT_NONE
    use sysdep_fm, only: sys_string_type, emacs_environment
    implicit none(type, external)
    private

    public :: syms_of_sh_script, sh_mode, sh_base_mode, sh_set_shell, sh_canonicalize_shell, sh_feature_shell, &
              sh_var_value, sh_basic_indent_line, sh_execute_region, sh_remember_variable, is_sh_quoted, is_sh_backslash_quoted, &
              sh_in_comment_or_string, cmd_sh_make_vars_local, sh_reset_indent_vars_to_global_values, sh_add, &
              sh_shell_initialize_variables, cmd_sh_beginning_of_command, cmd_sh_end_of_command, sh_backslash_region, &
              sh_current_function_name, sh_mkword_regexpr, sh_indent_supported_here, sh_shell_variables, &
              sh_syntax_propertize_function, sh_font_lock_syntactic_face_function, sh_font_lock_keywords, &
              sh_font_lock_keywords_1, sh_font_lock_keywords_2, sh_smie_default_forward_token, &
              sh_smie_default_backward_token, sh_smie_sh_forward_token, sh_smie_sh_backward_token, sh_smie_sh_rules, &
              sh_smie_rc_forward_token, sh_smie_rc_backward_token, sh_smie_rc_rules, sh_smie_indent_continuation, &
              sh_electric_here_document_mode, sh_assignment, sh_shell_process, sh_show_shell, sh_send_text, sh_cd_here, &
              sh_send_line_or_region_and_step, sh_vars_before_point, sh_completion_at_point_function, sh_name_style, &
              sh_load_style, sh_save_styles_to_buffer

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> The symbols an indentation variable can hold, for `sh_var_value':
    !> + - ++ -- * /.
    integer, parameter, public :: SH_PLUS = -1000001, SH_MINUS = -1000002, SH_PLUS_PLUS = -1000003, &
                                  SH_MINUS_MINUS = -1000004, SH_STAR = -1000005, SH_SLASH = -1000006

    !> Values of `sh_indent_after_continuation': nil, t and `always'.
    integer, parameter, public :: SH_CONTINUATION_NO = 0, SH_CONTINUATION_DEEPER = 1, SH_CONTINUATION_ALWAYS = 2

    !> A string of a list.
    type, public :: sh_string_type
        character(len=:), allocatable :: s
    end type sh_string_type

    !> A shell and its direct ancestor (an element of `sh_ancestor_table').
    type, public :: sh_ancestor_type
        character(len=:), allocatable :: shell, ancestor
    end type sh_ancestor_type

    !> An element of `sh_alias_table': a regular expression matched
    !> against the full path of the interpreter (or, with `basename', a
    !> name matched against its basename) and the shell it names.
    type, public :: sh_alias_type
        character(len=:), allocatable :: key
        logical :: basename = .false.
        character(len=:), allocatable :: shell
    end type sh_alias_type

    !> One shell's entry of a feature table of string lists: the strings
    !> and, for (SHELL sh-append ANCESTOR STRINGS...), the shell whose
    !> value follows them (`append').
    type, public :: sh_strings_entry_type
        character(len=:), allocatable :: shell, append
        type(sh_string_type), allocatable :: strings(:)
    end type sh_strings_entry_type

    !> A feature table whose values are lists of strings (see `sh_feature').
    type, public :: sh_strings_feature_type
        type(sh_strings_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to STRINGS (after them APPEND's value, if given).
        procedure :: add => strings_feature_add
        !> The value for the current shell.
        procedure :: get => strings_feature_get
    end type sh_strings_feature_type

    !> One shell's entry of a feature table of font-lock keywords.
    type, public :: sh_keywords_entry_type
        character(len=:), allocatable :: shell, append
        type(font_lock_keyword_type), allocatable :: keywords(:)
    end type sh_keywords_entry_type

    !> A feature table whose values are lists of font-lock keywords.
    type, public :: sh_keywords_feature_type
        type(sh_keywords_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to KEYWORDS (after them APPEND's value).
        procedure :: add => keywords_feature_add
        !> The value for the current shell.
        procedure :: get => keywords_feature_get
    end type sh_keywords_feature_type

    !> One shell's entry of a feature table of strings: `value'
    !> unallocated is nil.
    type, public :: sh_string_entry_type
        character(len=:), allocatable :: shell
        character(len=:), allocatable :: value
    end type sh_string_entry_type

    !> A feature table whose values are strings or nil.
    type, public :: sh_string_feature_type
        type(sh_string_entry_type), allocatable :: entries(:)
    contains
        !> Set SHELL's entry to VALUE (absent for nil).
        procedure :: add => string_feature_add
        !> The value for the current shell (unallocated for nil).
        procedure :: get => string_feature_get
    end type sh_string_feature_type

    !> A syntax modification: CHAR gets SYNTAX.
    type, public :: sh_syntax_change_type
        integer :: char = 0
        character(len=:), allocatable :: syntax
    end type sh_syntax_change_type

    !> One shell's entry of `sh_mode_syntax_table_input': nil (no
    !> `changes'), or (TABLE CHAR SYNTAX ...): a copy of `table' with
    !> `changes'.
    type, public :: sh_syntax_entry_type
        character(len=:), allocatable :: shell
        type(handle_type) :: table
        type(sh_syntax_change_type), allocatable :: changes(:)
    end type sh_syntax_entry_type

    !> A feature table of syntax tables.
    type, public :: sh_syntax_feature_type
        type(sh_syntax_entry_type), allocatable :: entries(:)
    end type sh_syntax_feature_type

    ! A buffer's own variables.
    type :: sh_local_type
        integer(int64) :: serial = 0
        ! sh-shell-variables (setq-local in `sh_set_shell').
        logical :: has_variables = .false.
        type(sh_string_type), allocatable :: variables(:)
        logical :: variables_initialized = .false.
        ! sh-indent-supported-here.
        logical :: has_indent_supported = .false.
        character(len=:), allocatable :: indent_supported
        ! sh-header-marker.
        logical :: has_header = .false.
        type(handle_type) :: header_marker
        ! sh-mode-syntax-table (setq-local in `sh_set_shell').
        logical :: has_table = .false.
        type(handle_type) :: table
        ! sh-shell-process.
        type(handle_type) :: process
    end type sh_local_type

    type(sh_local_type), allocatable :: locals(:)

    !> Functions for doing TAB dynamic completion.  Port of sh-script.el
    !> sh-dynamic-complete-functions.
    type(capf_hook_type), target, public :: sh_dynamic_complete_functions = &
                                            capf_hook_type(name="sh_dynamic_complete_functions")

    !> The keywords `sh_completion_at_point_function' offers.  Port of
    !> sh-script.el sh--completion-keywords.
    character(len=*), parameter :: sh_completion_keywords(5) = [character(len=5) :: "if", "while", "until", "for", "then"]

    !> An indentation style of `sh_styles_table': its NAME and the VALUES of
    !> the indentation variables in the order of `sh_var_list'.
    type, public :: sh_style_type
        character(len=64) :: name = ""
        integer :: values(15) = 0
    end type sh_style_type

    !> The indentation variables (sh-script.el sh-var-list), as their
    !> names.
    character(len=*), parameter, public :: sh_var_list(15) = [character(len=30) :: "sh_basic_offset", &
                                                             "sh_indent_after_case", "sh_indent_after_do", "sh_indent_after_done", &
                                                   "sh_indent_after_else", "sh_indent_after_if", "sh_indent_after_loop_construct", &
                                                     "sh_indent_after_open", "sh_indent_for_case_alt", "sh_indent_for_case_label", &
                                                           "sh_indent_for_continuation", "sh_indent_for_do", "sh_indent_for_done", &
                                                                                           "sh_indent_for_fi", "sh_indent_for_then"]

    !> A list of all known shell indentation styles.  Port of sh-script.el
    !> sh-styles-alist.
    type(sh_style_type), allocatable, public :: sh_styles_table(:)

    ! The state of completion-table-with-cache in
    ! `sh_completion_at_point_function': the last string and its
    ! candidates.
    character(len=:), allocatable :: cmd_cache_arg
    type(value_type) :: cmd_cache_result

    ! The default values of sh-shell-variables and
    ! sh-shell-variables-initialized.
    type(sh_string_type), allocatable :: default_variables(:)
    logical :: default_variables_initialized = .false.

    !> Table showing the direct ancestor of various shells.
    !> This is the basis for `sh_feature'.  See also `sh_alias_table'.
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
    type(sh_ancestor_type), allocatable, public :: sh_ancestor_table(:)

    !> Table for transforming shell names to what they really are.
    !> Use this where the name of the executable doesn't correspond to
    !> the type of shell it really is.  Keys are regular expressions
    !> matched against the full path of the interpreter.  (For backward
    !> compatibility, keys may also be symbols, which are matched
    !> against the interpreter's basename.  The values are symbols
    !> naming the shell.  Port of sh-script.el sh-alias-alist.
    type(sh_alias_type), allocatable, public :: sh_alias_table(:)

    !> The executable file name for the shell being programmed.  Port of
    !> sh-script.el sh-shell-file.
    type(string_setting), target, public :: sh_shell_file = string_setting(name="sh_shell_file")

    !> Single argument string for the magic number.  See `sh_feature'.
    !> Port of sh-script.el sh-shell-arg.
    type(sh_string_feature_type), public :: sh_shell_arg

    !> The shell being programmed.  This is set by \[sh_set_shell].
    !> Port of sh-script.el sh-shell.
    type(string_setting), target, public :: sh_shell = string_setting(name="sh_shell")

    !> The abbrev table of Shell-script mode.  Port of sh-script.el
    !> sh-mode-abbrev-table.
    type(abbrev_table_type), target, public :: sh_mode_abbrev_table

    !> The abbrev table of `sh_base_mode'.
    type(abbrev_table_type), target, public :: sh_base_mode_abbrev_table

    !> The syntax table to use for Shell-Script mode.
    !> This is buffer-local in every such buffer.  Port of sh-script.el
    !> sh-mode-syntax-table.
    type(handle_type), public :: sh_mode_syntax_table

    !> The syntax table of `sh_base_mode'.
    type(handle_type), public :: sh_base_mode_syntax_table

    !> Syntax-table used in Shell-Script mode.  See `sh_feature'.  Port of
    !> sh-script.el sh-mode-syntax-table-input.
    type(sh_syntax_feature_type), public :: sh_mode_syntax_table_input

    !> Keymap used in Shell-Script mode.  Port of sh-script.el sh-mode-map.
    type(handle_type), public :: sh_mode_map

    !> The keymap of `sh_base_mode'.
    type(handle_type), public :: sh_base_mode_map

    !> Regexp for the variable name and what may follow in an assignment.
    !> First grouping matches the variable name.  This is up to and including the `='
    !> sign.  See `sh_feature'.  Port of sh-script.el sh-assignment-regexp.
    type(sh_string_feature_type), public :: sh_assignment_regexp

    !> Don't remember variables less than this length for completing reads.
    !> Port of sh-script.el sh-remember-variable-min.
    integer, public :: sh_remember_variable_min = 3

    !> Regexp to determine the beginning of a shell command.
    !> The actual command starts at the beginning of the second \(grouping\).
    !> Port of sh-script.el sh-beginning-of-command.
    character(len=:), allocatable, public :: sh_beginning_of_command

    !> Regexp to determine the end of a shell command.
    !> The actual command ends at the end of the first \(grouping\).
    !> Port of sh-script.el sh-end-of-command.
    character(len=:), allocatable, public :: sh_end_of_command

    !> Word to delimit here documents.
    !> If the first character of this string is "-", this is taken as
    !> part of the redirection operator, rather than part of the
    !> word (that is, "<<-" instead of "<<").  This is a feature
    !> used by some shells (for example Bash) to indicate that leading
    !> tabs inside the here document should be ignored.  In this case,
    !> Emacs indents the initial body and end of the here document with
    !> tabs, to the same level as the start (note that apart from this
    !> there is no support for indentation of here documents).  This
    !> will only work correctly if `sh_basic_offset' is a multiple of
    !> `tab_width'.
    !>
    !> Any quote characters or leading whitespace in the word are
    !> removed when closing the here document.  Port of sh-script.el
    !> sh-here-document-word.
    character(len=:), allocatable, public :: sh_here_document_word

    !> List of all shell builtins for completing read and fontification.
    !> Note that on some systems not all builtins are available or some are
    !> implemented as aliases.  See `sh_feature'.  Port of sh-script.el
    !> sh-builtins.
    type(sh_strings_feature_type), public :: sh_builtins

    !> How to indent statements following && in Shell-Script mode.
    !> If true, indent to align with &&.
    !> If false, indent to align with the previous line's indentation.
    !> Port of sh-script.el sh-indent-statement-after-and.
    logical, public :: sh_indent_statement_after_and = .true.

    !> List of keywords that may be immediately followed by a builtin or keyword.
    !> Given some confusion between keywords and builtins depending on shell and
    !> system, the distinction here has been based on whether they influence the
    !> flow of control or syntax.  See `sh_feature'.  Port of sh-script.el
    !> sh-leading-keywords.
    type(sh_strings_feature_type), public :: sh_leading_keywords

    !> List of keywords not in `sh_leading_keywords'.
    !> See `sh_feature'.  Port of sh-script.el sh-other-keywords.
    type(sh_strings_feature_type), public :: sh_other_keywords

    !> List of all shell variables available for completing read.
    !> See `sh_feature'.  Port of sh-script.el sh-variables.
    type(sh_strings_feature_type), public :: sh_variables

    !> Face to show a here-document.  Port of sh-script.el:sh-heredoc.
    type(named_face_type), target, public :: sh_heredoc

    !> Face to show quoted execs like `blabla`.  Port of
    !> sh-script.el:sh-quoted-exec.
    type(named_face_type), target, public :: sh_quoted_exec

    !> Face used for (non-escaped) backslash at end of a line in Shell-script mode.
    !> Port of sh-script.el:sh-escaped-newline.
    type(named_face_type), target, public :: sh_escaped_newline

    !> Default expressions to highlight in Shell Script modes.  See `sh_feature'.
    !> Port of sh-script.el sh-font-lock-keywords-var.
    type(sh_keywords_feature_type), public :: sh_font_lock_keywords_var

    !> Subdued level highlighting for Shell Script modes.  Port of
    !> sh-script.el sh-font-lock-keywords-var-1.
    type(sh_keywords_feature_type), public :: sh_font_lock_keywords_var_1

    !> Gaudy level highlighting for Shell Script modes.  Port of
    !> sh-script.el sh-font-lock-keywords-var-2.
    type(sh_keywords_feature_type), public :: sh_font_lock_keywords_var_2

    !> Hook run by `sh_set_shell'.  Port of sh-script.el sh-set-shell-hook.
    type(hook_type), target, public :: sh_set_shell_hook = hook_type(name="sh_set_shell_hook")

    !> Hook run by `sh_mode'.  Port of sh-script.el sh-mode-hook.
    type(hook_type), target, public :: sh_mode_hook = hook_type(name="sh_mode_hook")

    !> Hook run after entering `sh_base_mode'.
    type(hook_type), target, public :: sh_base_mode_hook = hook_type(name="sh_base_mode_hook")

    !> The default indentation increment.
    !> This value is used for the `+' and `-' symbols in an indentation variable.
    !> Port of sh-script.el sh-basic-offset.
    type(integer_setting), target, public :: sh_basic_offset = integer_setting(name="sh_basic_offset", value=4)

    !> How much to indent a `fi' relative to its `if'.  Usually 0.  Port of
    !> sh-script.el sh-indent-for-fi.
    type(integer_setting), target, public :: sh_indent_for_fi = integer_setting(name="sh_indent_for_fi", value=0)

    !> How much to indent a `done' relative to its matching stmt.  Usually 0.
    !> Port of sh-script.el sh-indent-for-done.
    type(integer_setting), target, public :: sh_indent_for_done = integer_setting(name="sh_indent_for_done", value=0)

    !> How much to indent a statement after an `else' statement.  Port of
    !> sh-script.el sh-indent-after-else.
    type(integer_setting), target, public :: sh_indent_after_else = &
                                             integer_setting(name="sh_indent_after_else", value=SH_PLUS)

    !> How much to indent a statement after an `if' statement.
    !> This includes lines after `else' and `elif' statements, too, but
    !> does not affect the `else', `elif' or `fi' statements themselves.
    !> Port of sh-script.el sh-indent-after-if.
    type(integer_setting), target, public :: sh_indent_after_if = integer_setting(name="sh_indent_after_if", value=SH_PLUS)

    !> How much to indent a `then' relative to its `if'.  Port of
    !> sh-script.el sh-indent-for-then.
    type(integer_setting), target, public :: sh_indent_for_then = integer_setting(name="sh_indent_for_then", value=0)

    !> How much to indent a `do' statement.
    !> This is relative to the statement before the `do', typically a
    !> `while', `until', `for', `repeat' or `select' statement.  Port of
    !> sh-script.el sh-indent-for-do.
    type(integer_setting), target, public :: sh_indent_for_do = integer_setting(name="sh_indent_for_do", value=0)

    !> How much to indent a line after a `do' statement.
    !> This is used when the `do' is the first word of the line.
    !> This is relative to the statement before the `do', typically a
    !> `while', `until', `for', `repeat' or `select' statement.  Port of
    !> sh-script.el sh-indent-after-do.
    type(integer_setting), target, public :: sh_indent_after_do = integer_setting(name="sh_indent_after_do", value=SH_PLUS)

    !> How much to indent a statement after a loop construct.
    !>
    !> This variable is used when the keyword `do' is on the same line as the
    !> loop statement (e.g., `until', `while' or `for').
    !> If the `do' is on a line by itself, then `sh_indent_after_do' is used instead.
    !> Port of sh-script.el sh-indent-after-loop-construct.
    type(integer_setting), target, public :: sh_indent_after_loop_construct = &
                                             integer_setting(name="sh_indent_after_loop_construct", value=SH_PLUS)

    !> How much to indent a statement after a `done' keyword.
    !> Normally this is 0, which aligns the `done' to the matching
    !> looping construct line.
    !> Setting it non-zero allows you to have the `do' statement on a line
    !> by itself and align the done under to do.  Port of sh-script.el
    !> sh-indent-after-done.
    type(integer_setting), target, public :: sh_indent_after_done = integer_setting(name="sh_indent_after_done", value=0)

    !> How much to indent a case label statement.
    !> This is relative to the line containing the `case' statement.
    !> Port of sh-script.el sh-indent-for-case-label.
    type(integer_setting), target, public :: sh_indent_for_case_label = &
                                             integer_setting(name="sh_indent_for_case_label", value=SH_PLUS)

    !> How much to indent statements after the case label.
    !> This is relative to the line containing the `case' statement.
    !> Port of sh-script.el sh-indent-for-case-alt.
    type(integer_setting), target, public :: sh_indent_for_case_alt = &
                                             integer_setting(name="sh_indent_for_case_alt", value=SH_PLUS_PLUS)

    !> How much to indent for a continuation statement.  Port of
    !> sh-script.el sh-indent-for-continuation.
    type(integer_setting), target, public :: sh_indent_for_continuation = &
                                             integer_setting(name="sh_indent_for_continuation", value=SH_PLUS)

    !> How much to indent after a line with an opening parenthesis or brace.
    !> For an open paren after a function, `sh_indent_after_function' is used.
    !> Port of sh-script.el sh-indent-after-open.
    type(integer_setting), target, public :: sh_indent_after_open = &
                                             integer_setting(name="sh_indent_after_open", value=SH_PLUS)

    !> How much to indent after a function line.  Port of sh-script.el
    !> sh-indent-after-function.
    type(integer_setting), target, public :: sh_indent_after_function = &
                                             integer_setting(name="sh_indent_after_function", value=SH_PLUS)

    !> How much to indent a `case' statement relative to the `switch' statement.
    !> This is for the rc shell.  Port of sh-script.el sh-indent-after-switch.
    type(integer_setting), target, public :: sh_indent_after_switch = &
                                             integer_setting(name="sh_indent_after_switch", value=SH_PLUS)

    !> How much to indent a statement relative to the `case' statement.
    !> This is for the rc shell.  Port of sh-script.el sh-indent-after-case.
    type(integer_setting), target, public :: sh_indent_after_case = &
                                             integer_setting(name="sh_indent_after_case", value=SH_PLUS)

    !> Column in which `sh_backslash_region' inserts backslashes.  Port of
    !> sh-script.el sh-backslash-column.
    integer, public :: sh_backslash_column = 48

    !> If true, `sh_backslash_region' will align backslashes.  Port of
    !> sh-script.el sh-backslash-align.
    logical, public :: sh_backslash_align = .true.

    !> Indentation rule set to use for each shell type.  Port of
    !> sh-script.el sh-indent-supported.
    type(sh_string_feature_type), public :: sh_indent_supported

    !> Controls whether indentation variables are local to the buffer.
    !> If true, indentation variables are made local initially.
    !> If false, you can later make the variables local by invoking
    !> command `sh_make_vars_local'.
    !> The default is true because I assume that in one Emacs session one is
    !> frequently editing existing scripts with different styles.  Port of
    !> sh-script.el sh-make-vars-local.
    logical, public :: sh_make_vars_local = .true.

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
    !> With `SH_CONTINUATION_ALWAYS' you get the former behavior whereas with
    !> `SH_CONTINUATION_NO' you get the latter.  With `SH_CONTINUATION_DEEPER', you
    !> get the latter as long as that would indent the continuation line
    !> deeper than the initial line.  Port of sh-script.el
    !> sh-indent-after-continuation.
    integer, public :: sh_indent_after_continuation = SH_CONTINUATION_DEEPER

    !> The SMIE grammar of sh.  Port of sh-script.el sh-smie-sh-grammar.
    type(smie_grammar_type), target, public :: sh_smie_sh_grammar

    !> The SMIE grammar of rc.  Port of sh-script.el sh-smie-rc-grammar.
    type(smie_grammar_type), target, public :: sh_smie_rc_grammar

    ! sh-smie--sh-operators-re and sh-smie--sh-operators-back-re.
    character(len=:), allocatable :: sh_operators_re, sh_operators_back_re

    ! sh-escaped-line-re and sh-here-doc-open-re.
    character(len=:), allocatable :: sh_escaped_line_re, sh_here_doc_open_re

    ! The rules of `sh_syntax_propertize_function', and the END it was
    ! called with (for the code of the rules).
    type(syntax_propertize_rules_type) :: propertize_rules
    integer(int64) :: propertize_end = 0

    ! The states of `sh_font_lock_quoted_subshell'.
    integer, parameter :: QS_NONE = 0, QS_DOUBLE_QUOTE = 1, QS_BACKQUOTE = 2, QS_CODE = 3

contains

    ! ------------------------------------------------------------------
    ! Setting up
    ! ------------------------------------------------------------------

    !> Define Shell-script mode: its options, feature tables, faces, syntax
    !> tables, keymap, grammars and commands.  Not a port.
    subroutine syms_of_sh_script()
        type(status_type) :: st
        character(len=:), allocatable :: shell
        logical :: found

        if (is_syntax_table_live(sh_mode_syntax_table)) return
        call forward_setting(sh_shell_file)
        call forward_setting(sh_shell)
        call forward_setting(sh_basic_offset)
        call forward_setting(sh_indent_for_fi)
        call forward_setting(sh_indent_for_done)
        call forward_setting(sh_indent_after_else)
        call forward_setting(sh_indent_after_if)
        call forward_setting(sh_indent_for_then)
        call forward_setting(sh_indent_for_do)
        call forward_setting(sh_indent_after_do)
        call forward_setting(sh_indent_after_loop_construct)
        call forward_setting(sh_indent_after_done)
        call forward_setting(sh_indent_for_case_label)
        call forward_setting(sh_indent_for_case_alt)
        call forward_setting(sh_indent_for_continuation)
        call forward_setting(sh_indent_after_open)
        call forward_setting(sh_indent_after_function)
        call forward_setting(sh_indent_after_switch)
        call forward_setting(sh_indent_after_case)
        call init_shells()
        if (.not. allocated(default_variables)) allocate (default_variables(0))

        if (.not. allocated(sh_shell_file%value)) then
            call getenv("SHELL", shell, found)

            if (found) then
                sh_shell_file = shell
            else
                sh_shell_file = "/bin/sh"
            end if
        end if

        if (.not. allocated(sh_shell%value)) sh_shell = sh_canonicalize_shell(sh_shell_file%value)

        if (.not. allocated(sh_beginning_of_command)) then
            sh_beginning_of_command = "\([;({`|&]\|\`\|[^\]"//LF//"\)["//" "//TAB//"]*\([/~[:alnum:]:]\)"
        end if

        if (.not. allocated(sh_end_of_command)) then
            sh_end_of_command = "\([/~[:alnum:]:]\)[ "//TAB//"]*\([;#)}`|&]\|$\)"
        end if

        if (.not. allocated(sh_here_document_word)) sh_here_document_word = "EOF"
        sh_escaped_line_re = "\(?:\(?:.*[^\"//LF//"]\)?\(?:\\\\\)*\\"//LF//"\)*.*"
        sh_here_doc_open_re = "[^<]<<-?\s-*\\?\(\(?:['""][^'""]+['""]\|\sw\|[-/~._@]\)+\)"//sh_escaped_line_re// &
                              "\("//LF//"\)"
        call init_features()
        call init_syntax_tables(st)
        call define_faces()
        call init_font_lock_keywords()
        call init_rules()
        call init_grammars(st)
        call sh_mode_hook%add(sh_electric_here_document_mode)
        call major_modes%sh_base_mode%define("sh_base_mode", "Shell-script", sh_base_mode, sh_base_mode_hook, &
                                             parent=major_modes%prog_mode, syntax_table=sh_base_mode_syntax_table, &
                                             abbrev_table=sh_base_mode_abbrev_table)
        sh_base_mode_map = major_modes%sh_base_mode%map
        call major_modes%sh_mode%define("sh_mode", "Shell-script", sh_mode, sh_mode_hook, parent=major_modes%sh_base_mode, &
                                        syntax_table=sh_mode_syntax_table, abbrev_table=sh_mode_abbrev_table)
        sh_mode_map = major_modes%sh_mode%map
        call keymap_set(sh_mode_map, "C-c C-\", "sh_backslash_region", st)
        call keymap_set(sh_mode_map, "C-c +", "sh_add", st)
        call keymap_set(sh_mode_map, "C-M-x", "sh_execute_region", st)
        call keymap_set(sh_mode_map, "C-c C-n", "sh_send_line_or_region_and_step", st)
        call keymap_set(sh_mode_map, "C-c C-d", "sh_cd_here", st)
        call keymap_set(sh_mode_map, "C-c C-z", "sh_show_shell", st)
        call keymap_set(sh_mode_map, "C-c :", "sh_set_shell", st)
        call bind_remap("delete_backward_char", "backward_delete_char_untabify")
        call bind_remap("backward_sentence", "sh_beginning_of_command")
        call bind_remap("forward_sentence", "sh_end_of_command")
        call minor_modes%sh_electric_here_document_mode%define("sh_electric_here_document_mode", &
                                                               body=sh_electric_here_document_mode_body)
        commands%sh_base_mode%run => sh_base_mode
        commands%sh_mode%run => sh_mode
        commands%shell_script_mode%run => sh_mode
        commands%sh_set_shell%run => cmd_sh_set_shell
        commands%sh_basic_indent_line%run => cmd_sh_basic_indent_line
        commands%sh_execute_region%run => cmd_sh_execute_region
        commands%sh_make_vars_local%run => cmd_sh_make_vars_local
        commands%sh_reset_indent_vars_to_global_values%run => sh_reset_indent_vars_to_global_values
        commands%sh_add%run => cmd_sh_add
        commands%sh_assignment%run => cmd_sh_assignment
        commands%sh_electric_here_document_mode%run => sh_electric_here_document_mode
        commands%sh_beginning_of_command%run => cmd_sh_beginning_of_command
        commands%sh_end_of_command%run => cmd_sh_end_of_command
        commands%sh_backslash_region%run => cmd_sh_backslash_region
        commands%sh_show_shell%run => sh_show_shell
        commands%sh_cd_here%run => sh_cd_here
        commands%sh_send_line_or_region_and_step%run => sh_send_line_or_region_and_step
        commands%sh_name_style%run => cmd_sh_name_style
        commands%sh_load_style%run => cmd_sh_load_style
        commands%sh_save_styles_to_buffer%run => cmd_sh_save_styles_to_buffer
        ! (The default value, in reverse order: `add' puts a function first.)
        call sh_dynamic_complete_functions%add(comint_filename_completion)
        call sh_dynamic_complete_functions%add(shell_command_completion)
        call sh_dynamic_complete_functions%add(shell_environment_variable_completion)
        if (.not. allocated(sh_styles_table)) allocate (sh_styles_table(0))
        call add_local_variables_killer(kill_sh_locals)
    end subroutine syms_of_sh_script

    ! Bind [remap FROM] to COMMAND in `sh_mode_map'.  Not a port.
    subroutine bind_remap(from, command)
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call list_elements(list(sym("remap"), sym(from)), events)
        call define_key(sh_mode_map, events, sym(command), st)
    end subroutine bind_remap

    ! Append the pair (SHELL . ANCESTOR) to `sh_ancestor_table'.
    ! Not a port.
    subroutine add_ancestor(shell, ancestor)
        character(len=*), intent(in) :: shell, ancestor
        type(sh_ancestor_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(sh_ancestor_table)) allocate (sh_ancestor_table(0))
        n = size(sh_ancestor_table)
        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(sh_ancestor_table(i)%shell, grown(i)%shell)
            call move_alloc(sh_ancestor_table(i)%ancestor, grown(i)%ancestor)
        end do

        grown(n + 1)%shell = shell
        grown(n + 1)%ancestor = ancestor
        call move_alloc(grown, sh_ancestor_table)
    end subroutine add_ancestor

    ! Append (KEY . SHELL) to `sh_alias_table'.  Not a port.
    subroutine add_alias(key, shell, basename)
        character(len=*), intent(in) :: key, shell
        logical, intent(in) :: basename
        type(sh_alias_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(sh_alias_table)) allocate (sh_alias_table(0))
        n = size(sh_alias_table)
        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(sh_alias_table(i)%key, grown(i)%key)
            grown(i)%basename = sh_alias_table(i)%basename
            call move_alloc(sh_alias_table(i)%shell, grown(i)%shell)
        end do

        grown(n + 1)%key = key
        grown(n + 1)%basename = basename
        grown(n + 1)%shell = shell
        call move_alloc(grown, sh_alias_table)
    end subroutine add_alias

    ! `sh_ancestor_table' and `sh_alias_table'.  Not a port.
    subroutine init_shells()
        if (allocated(sh_ancestor_table)) return
        call add_ancestor("ash", "sh")
        call add_ancestor("bash", "jsh")
        call add_ancestor("bash2", "jsh")
        call add_ancestor("dash", "ash")
        call add_ancestor("dtksh", "ksh")
        call add_ancestor("es", "rc")
        call add_ancestor("itcsh", "tcsh")
        call add_ancestor("jcsh", "csh")
        call add_ancestor("jsh", "sh")
        call add_ancestor("ksh", "ksh88")
        call add_ancestor("ksh88", "jsh")
        call add_ancestor("oash", "sh")
        call add_ancestor("pdksh", "ksh88")
        call add_ancestor("mksh", "pdksh")
        call add_ancestor("posix", "sh")
        call add_ancestor("tcsh", "csh")
        call add_ancestor("wksh", "ksh88")
        call add_ancestor("wsh", "sh")
        call add_ancestor("zsh", "ksh88")
        call add_ancestor("rpm", "sh")
        ! (eq system-type 'gnu/linux)
        call add_alias("csh", "tcsh", .true.)
        call add_alias("ksh", "pdksh", .true.)
        ! for the time being
        call add_alias("ksh", "ksh88", .true.)
        call add_alias("bash2", "bash", .true.)
        call add_alias("sh5", "sh", .true.)
        ! Android's system shell
        call add_alias("^/system/bin/sh$", "mksh", .false.)
    end subroutine init_shells

    ! The feature tables of strings.  Not a port.
    subroutine init_features()
        if (allocated(sh_builtins%entries)) return
        call sh_shell_arg%add("bash")
        call sh_shell_arg%add("csh", "-f")
        call sh_shell_arg%add("pdksh")
        ! Bill_Mann@praxisint.com says -p with ksh can do harm.
        call sh_shell_arg%add("ksh88")
        ! -p means don't initialize functions from the environment.
        call sh_shell_arg%add("rc", "-p")
        ! Someone proposed -motif, but we don't want to encourage
        ! use of a non-free widget set.
        call sh_shell_arg%add("wksh")
        ! -f means don't run .zshrc.
        call sh_shell_arg%add("zsh", "-f")
        ! The "\\[.+\\]" matches the "[index]" in "arrayvar[index]=value".
        call sh_assignment_regexp%add("csh", "\<\([[:alnum:]_]+\)\(\[.+\]\)?[ "//TAB//"]*[-+*/%^]?=")
        ! actually spaces are only supported in let/(( ... ))
        call sh_assignment_regexp%add("ksh88", "\<\([[:alnum:]_]+\)\(\[.+\]\)?"// &
                                      "[ "//TAB//"]*\(?:[-+*/%&|~^]\|<<\|>>\)?=")
        call sh_assignment_regexp%add("bash", "\<\([[:alnum:]_]+\)\(\[.+\]\)?\+?=")
        call sh_assignment_regexp%add("rc", "\<\([[:alnum:]_*]+\)[ "//TAB//"]*=")
        call sh_assignment_regexp%add("sh", "\<\([[:alnum:]_]+\)=")
        call sh_indent_supported%add("sh", "sh")
        call sh_indent_supported%add("csh")
        call sh_indent_supported%add("rc", "rc")
        call sh_builtins%add("bash", [character(len=9) :: &
                                      ".", "alias", "bg", "bind", "builtin", "caller", "compgen", "complete", &
                                      "declare", "dirs", "disown", "enable", "fc", "fg", "help", "history", &
                                      "jobs", "kill", "let", "local", "popd", "printf", "pushd", "shopt", &
                                      "source", "suspend", "typeset", "unalias", &
                                      "mapfile", "readarray", "coproc"], "posix")
        ! The next entry is only used for defining the others
        call sh_builtins%add("bourne", [character(len=8) :: "eval", "export", "getopts", "newgrp", "pwd", "read", &
                                        "readonly", "times", "ulimit"], "shell")
        call sh_builtins%add("csh", [character(len=7) :: "alias", "chdir", "glob", "history", "limit", "nice", "nohup", &
                                     "rehash", "setenv", "source", "time", "unalias", "unhash"], "shell")
        call sh_builtins%add("dtksh", [character(len=1) ::], "wksh")
        call sh_builtins%add("es", [character(len=7) :: "access", "apids", "cd", "echo", "eval", "false", "let", "limit", &
                                    "local", "newpgrp", "result", "time", "umask", "var", "vars", "wait", "whatis"])
        call sh_builtins%add("jsh", [character(len=7) :: "bg", "fg", "jobs", "kill", "stop", "suspend"], "sh")
        call sh_builtins%add("jcsh", [character(len=7) :: "bg", "fg", "jobs", "kill", "notify", "stop", "suspend"], "csh")
        call sh_builtins%add("ksh88", [character(len=7) :: "alias", "bg", "false", "fc", "fg", "jobs", "kill", "let", &
                                       "print", "time", "typeset", "unalias", "whence"], "bourne")
        call sh_builtins%add("oash", [character(len=9) :: "checkwin", "dateline", "error", "form", "menu", "newwin", &
                                      "oadeinit", "oaed", "oahelp", "oainit", "pp", "ppfile", "scan", "scrollok", &
                                      "wattr", "wclear", "werase", "win", "wmclose", "wmmessage", "wmopen", "wmove", &
                                      "wmtitle", "wrefresh"], "sh")
        call sh_builtins%add("pdksh", [character(len=4) :: "bind"], "ksh88")
        call sh_builtins%add("posix", [character(len=7) :: "command"], "sh")
        call sh_builtins%add("rc", [character(len=7) :: "builtin", "cd", "echo", "eval", "limit", "newpgrp", "shift", &
                                    "umask", "wait", "whatis"])
        call sh_builtins%add("sh", [character(len=4) :: "hash", "test", "type"], "bourne")
        ! The next entry is only used for defining the others
        call sh_builtins%add("shell", [character(len=5) :: "cd", "echo", "eval", "set", "shift", "umask", "unset", "wait"])
        call sh_builtins%add("wksh", [character(len=1) ::], "ksh88")
        call sh_builtins%add("zsh", [character(len=10) :: "autoload", "always", &
                                     "bindkey", "builtin", "chdir", "compctl", "declare", "dirs", &
                                     "disable", "disown", "echotc", "enable", "functions", "getln", "hash", &
                                     "history", "integer", "limit", "local", "log", "popd", "pushd", "r", &
                                     "readonly", "rehash", "sched", "setopt", "source", "suspend", "true", &
                                     "ttyctl", "type", "unfunction", "unhash", "unlimit", "unsetopt", "vared", &
                                     "which"], "ksh88")
        call sh_leading_keywords%add("bash", [character(len=4) :: "time"], "sh")
        call sh_leading_keywords%add("csh", [character(len=4) :: "else"])
        call sh_leading_keywords%add("es", [character(len=14) :: "true", "unwind-protect", "whatis"])
        call sh_leading_keywords%add("rc", [character(len=4) :: "else"])
        call sh_leading_keywords%add("sh", [character(len=5) :: "!", "do", "elif", "else", "if", "then", "trap", "type", &
                                            "until", "while"])
        call sh_other_keywords%add("bash", [character(len=6) :: "bye", "logout", "select"], "bourne")
        ! The next entry is only used for defining the others
        call sh_other_keywords%add("bourne", [character(len=8) :: "function"], "sh")
        call sh_other_keywords%add("csh", [character(len=7) :: "breaksw", "default", "end", "endif", "endsw", "foreach", &
                                           "goto", "if", "logout", "onintr", "repeat", "switch", "then", "while"], "shell")
        call sh_other_keywords%add("es", [character(len=7) :: "break", "catch", "exec", "exit", "fn", "for", "forever", &
                                          "fork", "if", "return", "throw", "while"])
        call sh_other_keywords%add("ksh88", [character(len=6) :: "select"], "bourne")
        call sh_other_keywords%add("rc", [character(len=6) :: "break", "case", "exec", "exit", "fn", "for", "if", "in", &
                                          "return", "switch", "while"])
        call sh_other_keywords%add("sh", [character(len=6) :: "done", "esac", "fi", "for", "in", "return"], "shell")
        ! The next entry is only used for defining the others
        call sh_other_keywords%add("shell", [character(len=8) :: "break", "case", "continue", "exec", "exit"])
        call sh_other_keywords%add("zsh", [character(len=7) :: "select", "foreach"], "bash")
        call sh_variables%add("bash", [character(len=25) :: &
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
        call sh_variables%add("csh", [character(len=9) :: &
                                      "argv", "cdpath", "child", "echo", "histchars", "history", "home", &
                                      "ignoreeof", "mail", "noclobber", "noglob", "nonomatch", "path", "prompt", &
                                      "shell", "status", "time", "verbose"], "shell")
        call sh_variables%add("es", [character(len=8) :: &
                                     "apid", "cdpath", "CDPATH", "history", "home", "ifs", "noexport", "path", &
                                     "pid", "prompt", "signals"], "shell")
        call sh_variables%add("jcsh", [character(len=6) :: "notify"], "csh")
        call sh_variables%add("ksh88", [character(len=8) :: &
                                        "ENV", "ERRNO", "FCEDIT", "FPATH", "HISTFILE", "HISTSIZE", "LINENO", &
                                        "OLDPWD", "PPID", "PS3", "PS4", "PWD", "RANDOM", "REPLY", "SECONDS", &
                                        "TMOUT"], "sh")
        call sh_variables%add("oash", [character(len=9) :: &
                                       "FIELD", "FIELD_MAX", "LAST_KEY", "OALIB", "PP_ITEM", "PP_NUM"], "sh")
        call sh_variables%add("rc", [character(len=7) :: &
                                     "apid", "apids", "cdpath", "CDPATH", "history", "home", "ifs", "path", "pid", &
                                     "prompt", "status"], "shell")
        call sh_variables%add("sh", [character(len=6) :: "CDPATH", "IFS", "OPTARG", "OPTIND", "PS1", "PS2"], "shell")
        ! The next entry is only used for defining the others
        call sh_variables%add("shell", [character(len=11) :: &
                                        "COLUMNS", "EDITOR", "HOME", "HUSHLOGIN", "LANG", "LC_COLLATE", &
                                        "LC_CTYPE", "LC_MESSAGES", "LC_MONETARY", "LC_NUMERIC", "LC_TIME", &
                                        "LINES", "LOGNAME", "MAIL", "MAILCHECK", "MAILPATH", "PAGER", "PATH", &
                                        "SHELL", "TERM", "TERMCAP", "TERMINFO", "VISUAL"])
        call sh_variables%add("tcsh", [character(len=26) :: &
                                       "addsuffix", "ampm", "autocorrect", "autoexpand", "autolist", &
                                       "autologout", "chase_symlinks", "correct", "dextract", "edit", "el", &
                                       "fignore", "gid", "histlit", "HOST", "HOSTTYPE", "HPATH", &
                                       "ignore_symlinks", "listjobs", "listlinks", "listmax", "matchbeep", &
                                       "nobeep", "NOREBIND", "oid", "printexitvalue", "prompt2", "prompt3", &
                                       "pushdsilent", "pushdtohome", "recexact", "recognize_only_executables", &
                                       "rmstar", "savehist", "SHLVL", "showdots", "sl", "SYSTYPE", "tcsh", "term", &
                                       "tperiod", "tty", "uid", "version", "visiblebell", "watch", "who", &
                                       "wordchars"], "csh")
        call sh_variables%add("zsh", [character(len=12) :: &
                                      "BAUD", "bindcmds", "cdpath", "DIRSTACKSIZE", "fignore", "FIGNORE", "fpath", &
                                      "HISTCHARS", "hostcmds", "hosts", "HOSTS", "LISTMAX", "LITHISTSIZE", &
                                      "LOGCHECK", "mailpath", "manpath", "NULLCMD", "optcmds", "path", "POSTEDIT", &
                                      "prompt", "PROMPT", "PROMPT2", "PROMPT3", "PROMPT4", "psvar", "PSVAR", &
                                      "READNULLCMD", "REPORTTIME", "RPROMPT", "RPS1", "SAVEHIST", "SPROMPT", &
                                      "STTY", "TIMEFMT", "TMOUT", "TMPPREFIX", "varcmds", "watch", "WATCH", &
                                      "WATCHFMT", "WORDCHARS", "ZDOTDIR"], "ksh88")
    end subroutine init_features

    ! `sh_mode_syntax_table', `sh_base_mode_syntax_table' and
    ! `sh_mode_syntax_table_input'.  Not a port.
    subroutine init_syntax_tables(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: t

        call make_syntax_table(sh_base_mode_syntax_table, status)
        ! (sh-mode-syntax-table () ...): a copy of the standard table.
        call copy_syntax_table(sh_mode_syntax_table, status)
        t = sh_mode_syntax_table
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
        allocate (sh_mode_syntax_table_input%entries(2))
        sh_mode_syntax_table_input%entries(1)%shell = "sh"
        allocate (sh_mode_syntax_table_input%entries(1)%changes(0))
        ! Treat ' as punctuation rather than a string delimiter, as RPM
        ! files often contain prose with apostrophes.
        sh_mode_syntax_table_input%entries(2)%shell = "rpm"
        sh_mode_syntax_table_input%entries(2)%table = sh_mode_syntax_table
        allocate (sh_mode_syntax_table_input%entries(2)%changes(1))
        sh_mode_syntax_table_input%entries(2)%changes(1)%char = iachar("'")
        sh_mode_syntax_table_input%entries(2)%changes(1)%syntax = "."

    contains

        subroutine punct(c)
            character(len=1), intent(in) :: c
            type(status_type) :: st

            call modify_syntax_entry(iachar(c), iachar(c), ".", st, t)
        end subroutine punct
    end subroutine init_syntax_tables

    ! The faces.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK

        f => sh_heredoc
        call f%define("sh_heredoc", "Face to show a here-document.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                          face_attrs(foreground="yellow1", weight="bold"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[DARK]), face_attrs(foreground="yellow", weight="bold"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attrs(foreground="tan1"))
        call f%spec_entry(display_when(), face_attrs(weight="bold"))
        f => sh_quoted_exec
        call f%define("sh_quoted_exec", "Face to show quoted execs like `blabla`.")
        call f%spec_entry(display_when(classes=[C], backgrounds=[DARK]), face_attrs(foreground="salmon"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attrs(foreground="magenta"))
        call f%spec_entry(display_when(), face_attrs(weight="bold"))
        f => sh_escaped_newline
        call f%define("sh_escaped_newline", "Face used for (non-escaped) backslash at end of a line in Shell-script mode.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_string_face"))
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

    ! `sh_font_lock_keywords_var', `sh_font_lock_keywords_var_1' and
    ! `sh_font_lock_keywords_var_2'.  Not a port.
    subroutine init_font_lock_keywords()
        character(len=*), parameter :: VA = "font_lock_variable_name_face", KW = "font_lock_keyword_face", &
                                       FN = "font_lock_function_name_face", ST = "font_lock_string_face"
        type(font_lock_keyword_type) :: k1(1), k2(2), k3(3), k4(4)
        type(font_lock_keyword_type), allocatable :: none(:)
        type(font_lock_highlight_type) :: h1(1), h2(2)

        if (allocated(sh_font_lock_keywords_var%entries)) return
        allocate (none(0))
        k1(1) = font_lock_keyword("\${?[#?]?\([[:alpha:]_][[:alnum:]_]*\|0\)", face=VA, subexp=1)
        call sh_font_lock_keywords_var%add("csh", k1, "shell")
        k1(1) = font_lock_keyword("\$#?\([[:alpha:]_][[:alnum:]_]*\|[0-9]+\)", face=VA, subexp=1)
        call sh_font_lock_keywords_var%add("es", k1, "executable_font_lock_keywords")
        call sh_font_lock_keywords_var%add("rc", none, "es")
        h1(1) = h(1, "sh_quoted_exec", FONT_LOCK_OVERRIDE_YES)
        k1(1) = font_lock_keyword("\$(\([^)"//LF//"]+\)", highlights=h1)
        call sh_font_lock_keywords_var%add("bash", k1, "sh")
        ! Variable names.
        k4(1) = font_lock_keyword("\$\({#?\)?\([[:alpha:]_][[:alnum:]_]*\|[-#?@!]\)", face=VA, subexp=2)
        ! Function names.
        k4(2) = font_lock_keyword("^\(\sw+\)[ "//TAB//"]*(", face=FN, subexp=1)
        h2(1) = h(1, KW)
        h2(2) = h(2, FN, lax=.true.)
        k4(3) = font_lock_keyword("\<\(function\)\>[ "//TAB//"]*\(\sw+\)?", highlights=h2)
        k4(4) = font_lock_keyword("\(?:^\s *\|[[();&|]\s *\|\(?:\s +-[ao]\|if\|else\|then\|while\|do\)\s +\)\(!\)", &
                                  face="font_lock_negation_char_face", subexp=1)
        call sh_font_lock_keywords_var%add("sh", k4, "shell")
        ! The next entry is only used for defining the others
        ! Using font-lock-string-face here confuses sh-get-indent-info.
        k3(1) = font_lock_keyword("\(^\|[^\]\)\(\\\\\)*\(\\\)$", face="sh_escaped_newline", subexp=3)
        k3(2) = font_lock_keyword("\\[^[:alnum:]]", face=ST, subexp=0)
        k3(3) = font_lock_keyword("\${?\([[:alpha:]_][[:alnum:]_]*\|[0-9]+\|[$*_]\)", face=VA, subexp=1)
        call sh_font_lock_keywords_var%add("shell", k3)
        k2(1) = font_lock_keyword("^\s-*%\(\sw+\)", face=KW, subexp=1)
        k2(2) = font_lock_keyword("%{?\([!?]*[[:alpha:]_][[:alnum:]_]*\|[0-9]+\|[%*#]\*?\|!?-[[:alpha:]]\*?\)", &
                                  face=VA, subexp=1)
        call sh_font_lock_keywords_var%add("rpm", k2, "rpm2")
        h1(1) = h(1, "font_lock_doc_face", FONT_LOCK_OVERRIDE_YES)
        k2(1) = font_lock_keyword("^Summary:\(.*\)$", highlights=h1)
        h2(1) = h(1, VA)
        h2(2) = h(3, ST, lax=.true.)
        k2(2) = font_lock_keyword("^\(\sw+\)\((\(\sw+\))\)?:", highlights=h2)
        call sh_font_lock_keywords_var%add("rpm2", k2, "shell")
        k1(1) = font_lock_keyword("[ "//TAB//"]\(in\|do\)\>")
        call sh_font_lock_keywords_var_1%add("sh", k1)
        allocate (sh_font_lock_keywords_var_2%entries(0))
    end subroutine init_font_lock_keywords

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

    !> Function to get simple fontification based on `sh_font_lock_keywords'.
    !> This adds rules for comments and assignments.  Port of
    !> sh-script.el:sh-font-lock-keywords.
    subroutine sh_font_lock_keywords(result, keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: result(:)
        type(font_lock_keyword_type), intent(in), optional :: keywords(:)
        type(font_lock_keyword_type), allocatable :: list(:)
        type(font_lock_keyword_type) :: assignment(1)
        character(len=:), allocatable :: re

        call sh_font_lock_keywords_var%get(list)
        re = sh_assignment_regexp%get()

        if (.not. allocated(re)) then
            call move_alloc(list, result)
            return
        end if

        assignment(1) = font_lock_keyword(re, face="font_lock_variable_name_face", subexp=1)
        call append_keywords(result, assignment)
        if (present(keywords)) call append_keywords(result, keywords)
        call append_keywords(result, list)
        if (allocated(executable_font_lock_keywords)) call append_keywords(result, executable_font_lock_keywords)
    end subroutine sh_font_lock_keywords

    !> Function to get better fontification including keywords.  Port of
    !> sh-script.el:sh-font-lock-keywords-1.
    subroutine sh_font_lock_keywords_1(result, builtins)
        type(font_lock_keyword_type), allocatable, intent(out) :: result(:)
        logical, intent(in), optional :: builtins
        type(font_lock_keyword_type), allocatable :: keywords(:), more(:)
        type(font_lock_keyword_type) :: one(1)
        type(font_lock_highlight_type) :: h2(2)
        character(len=:), allocatable :: re
        logical :: with_builtins

        with_builtins = .false.
        if (present(builtins)) with_builtins = builtins
        re = "\([;(){}`|&]\|^\)[ "//TAB//"]*\(\("//regexp_opt(strings_of(sh_leading_keywords), REGEXP_OPT_GROUP)// &
             "[ "//TAB//"]+\)?"//regexp_opt(leading_and_other(), REGEXP_OPT_GROUP)
        allocate (keywords(0))

        if (with_builtins) then
            h2(1) = h(2, "font_lock_keyword_face", lax=.true.)
            h2(2) = h(6, "font_lock_builtin_face")
            one(1) = font_lock_keyword(re//"[ "//TAB//"]+\)?"//regexp_opt(strings_of(sh_builtins), REGEXP_OPT_GROUP)//"\>", &
                                       highlights=h2)
            call append_keywords(keywords, one)
            call sh_font_lock_keywords_var_2%get(more)
            call append_keywords(keywords, more)
        end if

        one(1) = font_lock_keyword(re//"\)\>", face="font_lock_keyword_face", subexp=2)
        call append_keywords(keywords, one)
        call sh_font_lock_keywords_var_1%get(more)
        call append_keywords(keywords, more)
        call sh_font_lock_keywords(result, keywords)
    end subroutine sh_font_lock_keywords_1

    !> Function to get better fontification including keywords and builtins.
    !> Port of sh-script.el:sh-font-lock-keywords-2.
    subroutine sh_font_lock_keywords_2(result)
        type(font_lock_keyword_type), allocatable, intent(out) :: result(:)

        call sh_font_lock_keywords_1(result, .true.)
    end subroutine sh_font_lock_keywords_2

    ! The decoration levels of `font_lock_defaults'.  Not a port.
    subroutine keywords_level_0(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)

        call sh_font_lock_keywords(keywords)
    end subroutine keywords_level_0

    ! Not a port.
    subroutine keywords_level_1(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)

        call sh_font_lock_keywords_1(keywords)
    end subroutine keywords_level_1

    ! The strings of FEATURE for the current shell, as an array of
    ! characters.  Not a port.
    function strings_of(feature) result(a)
        type(sh_strings_feature_type), intent(in) :: feature
        character(len=:), allocatable :: a(:)
        type(sh_string_type), allocatable :: s(:)

        call feature%get(s)
        a = to_chars(s)
    end function strings_of

    ! (append (sh-feature sh-leading-keywords) (sh-feature sh-other-keywords)).
    ! Not a port.
    function leading_and_other() result(a)
        character(len=:), allocatable :: a(:)
        type(sh_string_type), allocatable :: s1(:), s2(:), s(:)
        integer :: i

        call sh_leading_keywords%get(s1)
        call sh_other_keywords%get(s2)
        allocate (s(size(s1) + size(s2)))

        do i = 1, size(s1)
            s(i)%s = s1(i)%s
        end do

        do i = 1, size(s2)
            s(size(s1) + i)%s = s2(i)%s
        end do

        a = to_chars(s)
    end function leading_and_other

    ! STRINGS as a character array.  Not a port.
    function to_chars(strings) result(a)
        type(sh_string_type), intent(in) :: strings(:)
        character(len=:), allocatable :: a(:)
        integer :: i, n

        n = 1

        do i = 1, size(strings)
            n = max(n, len(strings(i)%s))
        end do

        allocate (character(len=n) :: a(size(strings)))

        do i = 1, size(strings)
            a(i) = strings(i)%s
        end do
    end function to_chars

    ! ------------------------------------------------------------------
    ! Feature tables
    ! ------------------------------------------------------------------

    !> The shell whose entry of a feature table with entries for SHELLS
    !> applies to SHELL: SHELL itself, else its successive ancestors (see
    !> `sh_ancestor_table'), finally sh (0 when there is none).  Port of
    !> sh-script.el:sh-feature (the lookup part).
    integer function sh_feature_shell(shells, shell) result(k)
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
    end function sh_feature_shell

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
        if (.not. allocated(sh_ancestor_table)) return

        do i = 1, size(sh_ancestor_table)
            if (sh_ancestor_table(i)%shell == shell) then
                a = sh_ancestor_table(i)%ancestor
                return
            end if
        end do
    end function ancestor_of

    ! The current shell.  Not a port.
    function current_shell() result(s)
        character(len=:), allocatable :: s

        if (allocated(sh_shell%value)) then
            s = sh_shell%value
        else
            s = "sh"
        end if
    end function current_shell

    ! Not a port.
    subroutine strings_feature_add(self, shell, strings, append)
        class(sh_strings_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        character(len=*), intent(in) :: strings(:)
        character(len=*), intent(in), optional :: append
        type(sh_strings_entry_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = self%entries(i)
        end do

        grown(n + 1)%shell = shell
        if (present(append)) grown(n + 1)%append = append
        allocate (grown(n + 1)%strings(size(strings)))

        do i = 1, size(strings)
            grown(n + 1)%strings(i)%s = trim(strings(i))
        end do

        call move_alloc(grown, self%entries)
    end subroutine strings_feature_add

    ! The shells of the entries of a strings feature.  Not a port.
    function strings_shells(self) result(shells)
        class(sh_strings_feature_type), intent(in) :: self
        character(len=16), allocatable :: shells(:)
        integer :: i

        allocate (shells(size(self%entries)))

        do i = 1, size(self%entries)
            shells(i) = self%entries(i)%shell
        end do
    end function strings_shells

    !> Index the table by the current shell (see `sh_feature_shell'); an
    !> entry with sh-append is followed by the value for the shell it
    !> names.  Port of sh-script.el:sh-feature.
    recursive subroutine strings_feature_get(self, strings, shell)
        class(sh_strings_feature_type), intent(in) :: self
        type(sh_string_type), allocatable, intent(out) :: strings(:)
        character(len=*), intent(in), optional :: shell
        type(sh_string_type), allocatable :: rest(:)
        integer :: k, i, n

        allocate (strings(0))
        if (.not. allocated(self%entries)) return

        if (present(shell)) then
            k = sh_feature_shell(strings_shells(self), shell)
        else
            k = sh_feature_shell(strings_shells(self), current_shell())
        end if

        if (k == 0) return
        n = size(self%entries(k)%strings)

        if (allocated(self%entries(k)%append)) then
            call strings_feature_get(self, rest, self%entries(k)%append)
        else
            allocate (rest(0))
        end if

        deallocate (strings)
        allocate (strings(n + size(rest)))

        do i = 1, n
            strings(i)%s = self%entries(k)%strings(i)%s
        end do

        do i = 1, size(rest)
            strings(n + i)%s = rest(i)%s
        end do
    end subroutine strings_feature_get

    ! Not a port.
    subroutine keywords_feature_add(self, shell, keywords, append)
        class(sh_keywords_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        character(len=*), intent(in), optional :: append
        type(sh_keywords_entry_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = self%entries(i)
        end do

        grown(n + 1)%shell = shell
        if (present(append)) grown(n + 1)%append = append
        call append_keywords(grown(n + 1)%keywords, keywords)
        call move_alloc(grown, self%entries)
    end subroutine keywords_feature_add

    !> Index the table by the current shell, as `strings_feature_get'.
    !> Port of sh-script.el:sh-feature.
    recursive subroutine keywords_feature_get(self, keywords, shell)
        class(sh_keywords_feature_type), intent(in) :: self
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
            k = sh_feature_shell(shells, shell)
        else
            k = sh_feature_shell(shells, current_shell())
        end if

        if (k == 0) return
        call append_keywords(keywords, self%entries(k)%keywords)

        if (allocated(self%entries(k)%append)) then
            call keywords_feature_get(self, rest, self%entries(k)%append)
            call append_keywords(keywords, rest)
        end if
    end subroutine keywords_feature_get

    ! Not a port.
    subroutine string_feature_add(self, shell, value)
        class(sh_string_feature_type), intent(inout) :: self
        character(len=*), intent(in) :: shell
        character(len=*), intent(in), optional :: value
        type(sh_string_entry_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = self%entries(i)
        end do

        grown(n + 1)%shell = shell
        if (present(value)) grown(n + 1)%value = value
        call move_alloc(grown, self%entries)
    end subroutine string_feature_add

    !> Index the table by the current shell.  Port of
    !> sh-script.el:sh-feature.
    function string_feature_get(self) result(value)
        class(sh_string_feature_type), intent(in) :: self
        character(len=:), allocatable :: value
        character(len=16), allocatable :: shells(:)
        integer :: k, i

        if (.not. allocated(self%entries)) return
        allocate (shells(size(self%entries)))

        do i = 1, size(self%entries)
            shells(i) = self%entries(i)%shell
        end do

        k = sh_feature_shell(shells, current_shell())
        if (k == 0) return
        if (allocated(self%entries(k)%value)) value = self%entries(k)%value
    end function string_feature_get

    ! The entry of `sh_mode_syntax_table_input' for the current shell (0
    ! for none).  Not a port.
    integer function syntax_input_index() result(k)
        character(len=16), allocatable :: shells(:)
        integer :: i

        k = 0
        if (.not. allocated(sh_mode_syntax_table_input%entries)) return
        allocate (shells(size(sh_mode_syntax_table_input%entries)))

        do i = 1, size(shells)
            shells(i) = sh_mode_syntax_table_input%entries(i)%shell
        end do

        k = sh_feature_shell(shells, current_shell())
    end function syntax_input_index

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    ! Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(sh_local_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) return
        end do

        k = 0
        if (.not. make) return

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) then
                call free_local(k)
                locals(k) = sh_local_type(serial=buffers(cur)%serial)
                return
            end if
        end do

        n = size(locals)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = locals(i)
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, locals)
        k = n + 1
    end function local_index

    ! Free the objects of locals(K).  Not a port.
    subroutine free_local(k)
        integer, intent(in) :: k
        type(status_type) :: st

        if (locals(k)%has_header) call free_marker(locals(k)%header_marker, st)
        if (locals(k)%has_table) call free_syntax_table(locals(k)%table, st)
        locals(k)%has_header = .false.
        locals(k)%has_table = .false.
    end subroutine free_local

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine kill_sh_locals()
        integer :: k

        k = local_index(.false.)
        if (k == 0) return
        call free_local(k)
        locals(k) = sh_local_type()
    end subroutine kill_sh_locals

    !> The current buffer's `sh_indent_supported_here': the indentation
    !> rule set ("sh" or "rc"), "" if we don't support indentation for its
    !> shell type.  Port of sh-script.el sh-indent-supported-here.
    function sh_indent_supported_here() result(s)
        character(len=:), allocatable :: s
        integer :: k

        s = ""
        k = local_index(.false.)
        if (k == 0) return
        if (locals(k)%has_indent_supported) s = locals(k)%indent_supported
    end function sh_indent_supported_here

    !> The current buffer's `sh_shell_variables': the shell variable names
    !> that should be included in completion.  These are used for
    !> completion in addition to all the variables named in
    !> `process_environment'.  Port of sh-script.el sh-shell-variables.
    subroutine sh_shell_variables(names)
        type(sh_string_type), allocatable, intent(out) :: names(:)
        integer :: k

        k = local_index(.false.)

        if (k > 0) then
            if (locals(k)%has_variables) then
                names = locals(k)%variables
                return
            end if
        end if

        names = default_variables
    end subroutine sh_shell_variables

    ! ------------------------------------------------------------------
    ! Syntax properties
    ! ------------------------------------------------------------------

    ! The position of marker M.  Not a port.
    integer(int64) function mpos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, pos, st)
    end function mpos

    ! (char-after POS), -1 for nil.  Not a port.
    integer function ca(pos) result(c)
        integer(int64), intent(in) :: pos
        integer(int32) :: ch
        logical :: found

        call char_after(ch, found, pos)
        c = -1
        if (found) c = ch
    end function ca

    ! (char-before POS), -1 for nil.  Not a port.
    integer function cb(pos) result(c)
        integer(int64), intent(in) :: pos
        integer(int32) :: ch
        logical :: found

        call char_before(ch, found, pos)
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
        call put_text_property(start, end, sym("syntax_table"), syntax_descriptor_value(d), st)
    end subroutine put_syntax

    ! Mark START..END to be refontified together (syntax-multiline).
    ! Not a port.
    subroutine put_multiline(start, end)
        integer(int64), intent(in) :: start, end
        type(status_type) :: st

        call put_text_property(start, end, sym("syntax_multiline"), t_value(), st)
    end subroutine put_multiline

    ! (nth 1 (syntax-ppss POS)) and the like.  Not a port.
    function ppss(pos) result(state)
        integer(int64), intent(in), optional :: pos
        type(parse_state_type) :: state

        call syntax_ppss(state, pos)
    end function ppss

    ! Port of sh-script.el:sh--inside-noncommand-expression.
    recursive logical function sh_inside_noncommand_expression(pos) result(inside)
        integer(int64), intent(in) :: pos
        type(parse_state_type) :: state
        integer(int64) :: here
        integer :: c

        inside = .false.
        here = point()
        state = ppss(pos)

        if (state%innermost_start /= 0) then
            call goto_char(state%innermost_start)
            c = ca(point())

            if (c == iachar("(")) then
                ! ((...)) or $((...)) or $[...] or ${...}. Nested
                ! parenthesis can occur inside the first of these forms, so
                ! parse backward recursively.
                inside = cb(point()) == iachar("(")
            else if (c == iachar("{") .or. c == iachar("[")) then
                inside = cb(point()) == iachar("$")
            end if

            if (.not. inside) inside = sh_inside_noncommand_expression(point() - 1)
        end if

        call goto_char(here)
    end function sh_inside_noncommand_expression

    ! Determine the syntax of the \n after a <<EOF.
    ! START is the position of <<.
    ! STRING is the actual word used as delimiter (e.g. "EOF").
    ! Point is at the beginning of the next line.  PROP gets the syntax.
    ! Port of sh-script.el:sh-font-lock-open-heredoc.
    recursive subroutine sh_font_lock_open_heredoc(start, string, eol, prop)
        integer(int64), intent(in) :: start, eol
        character(len=*), intent(in) :: string
        type(syntax_property_type), intent(out) :: prop
        type(parse_state_type) :: state
        type(status_type) :: st
        character(len=:), allocatable :: str
        integer(int64) :: here
        integer :: c, i

        c = cb(start)
        if (c == iachar("<") .or. c == iachar(">")) return
        if (sh_in_comment_or_string(start)) return
        if (sh_inside_noncommand_expression(start)) return
        ! We're looking at <<STRING, so we add "^STRING$" to the syntactic
        ! font-lock keywords to detect the end of this here document.
        str = ""

        do i = 1, len(string)
            if (string(i:i) /= "'" .and. string(i:i) /= '"') str = str//string(i:i)
        end do

        here = point()
        state = ppss(eol)
        call goto_char(here)
        ! The \n not only starts the heredoc but also closes a comment.
        ! Let's close the comment just before the \n.
        if (state%incomment /= 0) call put_syntax(eol - 1, eol, ">")
        ! If the sh-escaped-line-re part of sh-here-doc-open-re has matched
        ! several lines, make sure we refontify them together.
        ! Furthermore, if (nth 5 ppss) is non-nil (i.e. the \n is
        ! escaped), it means the right \n is actually further down.
        ! Don't bother fixing it now, but place a multiline property so
        ! that when jit-lock-context-* refontifies the rest of the
        ! buffer, it also refontifies the current line with it.
        if (state%quoted .or. count_lines(start, eol) > 1) call put_multiline(start, eol + 1)
        call put_text_property(eol, eol + 1, sym("sh_here_doc_marker"), make_string(str), st)
        prop%apply = .true.
        call string_to_syntax("|", prop%descriptor, st)
        call goto_char(start + 2)
    end subroutine sh_font_lock_open_heredoc

    ! Port of sh-script.el:sh-syntax-propertize-here-doc.
    recursive subroutine sh_syntax_propertize_here_doc(end)
        integer(int64), intent(in) :: end
        type(parse_state_type) :: state
        type(value_type) :: key
        type(status_type) :: st
        character(len=:), allocatable :: k
        integer(int64) :: found, eol
        logical :: saved

        state = ppss()
        if (state%instring /= ST_STRING_STYLE) return
        call get_text_property(state%comstr_start, sym("sh_here_doc_marker"), key, st)
        k = ""
        if (stringp(key)) k = xstring(key)
        saved = case_fold_search%value
        case_fold_search%value = .false.
        call re_search_forward("^\([ "//TAB//"]*\)"//regexp_quote(k)//"\("//LF//"\)", found, st, bound=end, &
                               noerror=SEARCH_MOVE)
        case_fold_search%value = saved

        if (found /= 0) then
            eol = match_beginning(2, st)
            call put_syntax(eol, eol + 1, "|")
        end if
    end subroutine sh_syntax_propertize_here_doc

    ! Search for a subshell embedded in a string.
    ! Find all the unescaped " characters within said subshell, remembering that
    ! subshells can nest.  Port of sh-script.el:sh-font-lock-quoted-subshell.
    recursive subroutine sh_font_lock_quoted_subshell(limit, status)
        integer(int64), intent(in) :: limit
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state0
        integer, allocatable :: states(:)
        integer(int64) :: startpos, spos, epos, found, moved
        integer :: state, c

        status = status_type()
        state0 = ppss()
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
            moved = skip_chars_forward("^'\""`$()", status, limit)
            if (.not. (point() < limit)) exit
            c = ca(point())

            ! unescape " inside a $( ... ) construct.
            select case (c)
            case (iachar("'"))
                if (state /= QS_DOUBLE_QUOTE) then
                    call forward_char(status, 1_int64)
                    ! FIXME: mark skipped double quotes as punctuation syntax.
                    spos = point()
                    moved = skip_chars_forward("^'", status, limit)
                    epos = point()
                    call goto_char(spos)

                    do
                        call search_forward('"', found, status, bound=epos, noerror=SEARCH_NOERROR)
                        if (found == 0) exit
                        call put_syntax(point() - 1, point(), ".")
                    end do

                    call goto_char(epos)
                end if
            case (iachar("\"))
                call forward_char(status, 1_int64)
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
                    call forward_char(status, 1_int64)
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
                call set_error(status, FM_GENERIC_ERROR, "Internal error in sh-font-lock-quoted-subshell")
                return
            end select

            if (.not. status%ok()) return
            call forward_char(status, 1_int64)
            if (.not. status%ok()) return
        end do

        if (startpos < line_beginning_position()) then
            call put_multiline(startpos, point())
            call syntax_propertize_extend_region_functions%add(syntax_propertize_multiline, local=.true.)
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
    end subroutine sh_font_lock_quoted_subshell

    !> Whether the character before POS is an unquoted backslash.  Port of
    !> sh-script.el:sh-is-quoted-p.
    recursive logical function is_sh_backslash_quoted(pos) result(quoted)
        integer(int64), intent(in) :: pos

        quoted = .false.
        if (cb(pos) == iachar("\")) quoted = .not. is_sh_backslash_quoted(pos - 1)
    end function is_sh_backslash_quoted

    ! Port of sh-script.el:sh-font-lock-paren: the syntax of the `)' at
    ! START (PROP), punctuation in a case pattern.
    recursive subroutine sh_font_lock_paren(start, prop)
        integer(int64), intent(in) :: start
        type(syntax_property_type), intent(out) :: prop
        type(parse_state_type) :: state
        type(status_type) :: st
        integer(int64) :: here, open
        logical :: again, ok

        state = ppss()
        if (state%comstr_start /= 0) return
        here = point()
        open = 0
        call goto_char(start)

        ! Skip through all patterns
        do
            do
                ok = forward_comment(-point_max())

                if (is_eol() .and. is_sh_backslash_quoted(point())) then
                    call forward_char(st, -1_int64)
                    cycle
                end if

                exit
            end do

            ! Skip through one pattern
            do
                if (skip_syntax_backward("w_") /= 0) cycle
                if (skip_chars_backward("-$=?[]*@/\\!%:.^~,", st) /= 0) cycle

                if (is_sh_backslash_quoted(point() - 1)) then
                    call goto_char(point() - 2)
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
                call forward_char(st, -1_int64)
                open = point()
            end if

            do
                ok = forward_comment(-point_max())
                ! Maybe we've bumped into an escaped newline.
                if (.not. is_sh_backslash_quoted(point())) exit
                call forward_char(st, -1_int64)
            end do

            again = .false.

            if (cb(point()) == iachar("|")) then
                if (cb(point() - 1) /= iachar(";")) then
                    call forward_char(st, -1_int64)
                    again = .true.
                end if
            end if

            if (.not. again) exit
        end do

        if (point() > point_min() + 1) then
            call forward_char(st, -2_int64)
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
                    prop%apply = .true.
                    call string_to_syntax(".", prop%descriptor, st)
                end if
            end if
        end if

        call goto_char(here)
    end subroutine sh_font_lock_paren

    ! (let ((forward-sexp-function nil)) (forward-sexp N)).  Not a port.
    recursive subroutine plain_forward_expression(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call forward_expression_default_function(n, status)
    end subroutine plain_forward_expression

    ! Port of sh-script.el:sh-font-lock-backslash-quote.
    recursive subroutine backslash_quote_value(prop, status)
        type(syntax_property_type), intent(out) :: prop
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer(int64) :: here

        here = point()
        state = ppss(match_beginning(0, status))
        call goto_char(here)

        ! In a '...' the backslash is not escaping.
        if (state%instring == iachar("'")) then
            prop%apply = .true.
            call string_to_syntax(".", prop%descriptor, status)
        end if

        status = status_type()
    end subroutine backslash_quote_value

    ! (2 (sh-font-lock-open-heredoc (1+ (match-beginning 0)) (match-string 1) (match-beginning 2))).
    ! Not a port.
    recursive subroutine open_heredoc_value(prop, status)
        type(syntax_property_type), intent(out) :: prop
        type(status_type), intent(out) :: status
        integer(int64) :: start, eol
        character(len=:), allocatable :: word

        start = match_beginning(0, status) + 1
        word = match_string(1)
        eol = match_beginning(2, status)
        status = status_type()
        call sh_font_lock_open_heredoc(start, word, eol, prop)
    end subroutine open_heredoc_value

    ! (0 (prog1 nil (sh-syntax-propertize-here-doc end))).  Not a port.
    recursive subroutine here_doc_after(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call sh_syntax_propertize_here_doc(propertize_end)
    end subroutine here_doc_after

    ! (0 (sh-font-lock-paren (match-beginning 0))).  Not a port.
    recursive subroutine paren_value(prop, status)
        type(syntax_property_type), intent(out) :: prop
        type(status_type), intent(out) :: status
        integer(int64) :: mb

        mb = match_beginning(0, status)
        status = status_type()
        call sh_font_lock_paren(mb, prop)
    end subroutine paren_value

    ! Highlight (possibly nested) subshells inside "" quoted regions
    ! correctly.  Not a port.
    recursive subroutine quoted_subshell_after(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer(int64) :: here, mb

        status = status_type()
        here = point()
        mb = match_beginning(0, status)
        state = ppss(mb)
        call goto_char(here)

        if (state%comstr_start /= 0) then
            call goto_char(mb + 1)
        else
            ! Save excursion because we want to also apply other
            ! syntax-propertize rules within the affected region.
            call sh_font_lock_quoted_subshell(propertize_end, status)
            call goto_char(here)
        end if
    end subroutine quoted_subshell_after

    ! The rules of `sh_syntax_propertize_function'.  Not a port.
    subroutine init_rules()
        type(syntax_highlight_type) :: hl
        character(len=*), parameter :: META = "|&;<>(`\""' "//TAB//LF

        if (allocated(propertize_rules%rules)) return
        allocate (propertize_rules%rules(9))
        propertize_rules%rules(1)%regexp = sh_here_doc_open_re
        hl = syntax_highlight_type(group=2)
        hl%value => open_heredoc_value
        propertize_rules%rules(1)%highlights = [hl]
        propertize_rules%rules(2)%regexp = "\s|"
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
        propertize_rules%rules(3)%regexp = "\(?:[^"//META//"]\|\${\|\(?:[^\]\|^\)\\\(?:\\\\\)*.\)\(#+\)"
        hl = syntax_highlight_type(group=1, syntax="_")
        propertize_rules%rules(3)%highlights = [hl]
        ! In addition, `#' at the beginning of closed parentheses
        ! does not start a comment if the parentheses are not isolated
        ! by metacharacters, excluding [()].
        ! (e.g. `foo(#q/)' and `(#b)foo' in zsh)
        propertize_rules%rules(4)%regexp = "[^"//META//"](\(#+\)"
        propertize_rules%rules(4)%highlights = [hl]
        propertize_rules%rules(5)%regexp = "(\(#\)[^)]+?)[^|&;<>)`\""' "//TAB//LF//"]"
        propertize_rules%rules(5)%highlights = [hl]
        ! In a '...' the backslash is not escaping.
        propertize_rules%rules(6)%regexp = "\(\\\)'"
        hl = syntax_highlight_type(group=1)
        hl%value => backslash_quote_value
        propertize_rules%rules(6)%highlights = [hl]
        ! Make sure $@ and $? are correctly recognized as expressions.
        propertize_rules%rules(7)%regexp = "\$\([?@]\)"
        hl = syntax_highlight_type(group=1, syntax="_")
        propertize_rules%rules(7)%highlights = [hl]
        ! Distinguish the special close-paren in `case'.
        propertize_rules%rules(8)%regexp = ")"
        hl = syntax_highlight_type(group=0)
        hl%value => paren_value
        propertize_rules%rules(8)%highlights = [hl]
        ! Highlight (possibly nested) subshells inside "" quoted
        ! regions correctly.
        propertize_rules%rules(9)%regexp = """\(?:[^\""]\|\\.\)*?\(\$(\|`\)"
        hl = syntax_highlight_type(group=1)
        hl%after => quoted_subshell_after
        propertize_rules%rules(9)%highlights = [hl]
    end subroutine init_rules

    !> The `syntax_propertize_function' of Shell-script mode.  Port of
    !> sh-script.el:sh-syntax-propertize-function.
    recursive subroutine sh_syntax_propertize_function(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        status = status_type()
        saved = propertize_end
        propertize_end = end
        call goto_char(start)
        call sh_syntax_propertize_here_doc(end)
        call propertize_rules%apply(point(), end, status)
        propertize_end = saved
    end subroutine sh_syntax_propertize_function

    !> The `font_lock_syntactic_face_function' of Shell-script mode.
    !> Port of sh-script.el:sh-font-lock-syntactic-face-function.
    subroutine sh_font_lock_syntactic_face_function(state, face)
        type(parse_state_type), intent(in) :: state
        character(len=:), allocatable, intent(out) :: face

        if (state%instring /= -1) then
            if (state%instring /= ST_STRING_STYLE) then
                if (state%instring == iachar("`")) then
                    face = "sh_quoted_exec"
                else
                    face = "font_lock_string_face"
                end if
            else
                face = "sh_heredoc"
            end if
        else
            face = "font_lock_comment_face"
        end if
    end subroutine sh_font_lock_syntactic_face_function

    !> Return true if START is in a comment or string.  Port of
    !> sh-script.el:sh-in-comment-or-string.
    recursive logical function sh_in_comment_or_string(start) result(in)
        integer(int64), intent(in) :: start
        type(parse_state_type) :: state
        integer(int64) :: here

        here = point()
        state = ppss(start)
        call goto_char(here)
        in = state%instring /= -1 .or. state%incomment /= 0
    end function sh_in_comment_or_string

    ! ------------------------------------------------------------------
    ! Indentation and navigation with SMIE
    ! ------------------------------------------------------------------

    ! (zerop (length TOK)).  Not a port.
    logical function empty(tok)
        character(len=:), allocatable, intent(in) :: tok

        empty = .true.
        if (allocated(tok)) empty = len(tok) == 0
    end function empty

    ! (assoc TOK smie-grammar).  Not a port.
    logical function in_grammar(tok)
        character(len=:), allocatable, intent(in) :: tok
        type(smie_toklevels_type) :: t

        in_grammar = .false.
        if (.not. allocated(tok)) return
        t = smie_grammar_token(tok)
        in_grammar = .not. t%null
    end function in_grammar

    ! (string-match "\\`[a-z]" TOK).  Not a port.
    logical function starts_lower(tok)
        character(len=:), allocatable, intent(in) :: tok

        starts_lower = .false.
        if (.not. allocated(tok)) return
        if (len(tok) == 0) return
        starts_lower = tok(1:1) >= "a" .and. tok(1:1) <= "z"
    end function starts_lower

    ! Whether TOK is allocated and equal to S.  Not a port.
    logical function is(tok, s)
        character(len=:), allocatable, intent(in) :: tok
        character(len=*), intent(in) :: s

        is = .false.
        if (allocated(tok)) is = tok == s .and. len(tok) == len(s)
    end function is

    ! Non-nil if we're at a keyword position.
    ! A keyword position is one where if we're looking at something that looks
    ! like a keyword, then it is a keyword.  Port of
    ! sh-script.el:sh-smie--keyword-p.
    recursive logical function is_sh_smie_keyword() result(p)
        character(len=:), allocatable :: prev
        type(status_type) :: st

        call smie_backward_token(prev, st)

        if (empty(prev)) then
            p = looking_back("\`\|\s(", st, point() - 1)
        else
            p = in_grammar(prev)
        end if
    end function is_sh_smie_keyword

    ! Return non-nil if a newline should be treated as a semi-colon.
    ! Here we assume that a newline should be treated as a semi-colon unless it
    ! comes right after a special keyword.
    ! This function does not pay attention to line-continuations.
    ! If TOK is absent, point should be before the newline; otherwise, TOK is the token
    ! before the newline and in that case point should be just before the token.
    ! Port of sh-script.el:sh-smie--newline-semi-p.
    recursive logical function is_sh_smie_newline_semi(tok_in) result(semi)
        character(len=:), allocatable, intent(in), optional :: tok_in
        character(len=:), allocatable :: tok
        type(smie_toklevels_type) :: t
        type(status_type) :: st
        integer(int64) :: here

        here = point()

        if (present(tok_in)) then
            if (allocated(tok_in)) tok = tok_in
        end if

        if (.not. allocated(tok)) call smie_backward_token(tok, st)

        if (empty(tok) .and. looking_back("\s(", st, point() - 1)) then
            semi = .false.
        else
            semi = .true.

            if (allocated(tok)) then
                t = smie_grammar_token(tok)
                if (.not. t%null) semi = t%right%kind /= SMIE_LEVEL_NUMBER
            end if
        end if

        call goto_char(here)
    end function is_sh_smie_newline_semi

    ! When looking at TOK (either "in" or "do"), non-nil if TOK is a keyword.
    ! Does not preserve point.  Port of sh-script.el:sh-smie--sh-keyword-in/do-p.
    recursive logical function is_sh_smie_sh_keyword_in_do(tok) result(keyword)
        character(len=*), intent(in) :: tok
        integer, parameter :: UNDECIDED = 0, WORD = 1, KEYWORD_RES = 2, UNKNOWN = 3
        character(len=:), allocatable :: prev
        type(status_type) :: st
        logical :: words, newline
        integer :: res

        words = .false. ! We've seen words.
        newline = .false. ! We've seen newlines after the words.
        res = UNDECIDED

        do while (res == UNDECIDED)
            call smie_backward_token(prev, st)

            if (empty(prev)) then
                if (newline) then
                    res = WORD
                else if (is_bob()) then
                    res = WORD
                else
                    words = .true.
                    call plain_forward_expression(-1_int64, st)
                    if (.not. st%ok()) res = UNKNOWN
                end if
            else if (prev == ";") then
                if (words) then
                    newline = .true.
                else
                    res = KEYWORD_RES
                end if
            else if (prev == "for" .or. prev == "select" .or. (tok == "in" .and. prev == "case")) then
                res = KEYWORD_RES
            else if (in_grammar(prev)) then
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
    end function is_sh_smie_sh_keyword_in_do

    ! Non-nil if TOK (at which we're looking) really is a keyword.  Port of
    ! sh-script.el:sh-smie--sh-keyword-p.
    recursive logical function is_sh_smie_sh_keyword(tok) result(p)
        character(len=*), intent(in) :: tok
        type(status_type) :: st

        if (looking_at("[[:alnum:]_]+=", st)) then
            p = .false.
        else if (tok == "in" .or. tok == "do") then
            p = is_sh_smie_sh_keyword_in_do(tok)
        else
            p = is_sh_smie_keyword()
        end if
    end function is_sh_smie_sh_keyword

    !> Port of sh-script.el:sh-smie--default-forward-token.
    recursive subroutine sh_smie_default_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: start, moved
        logical :: ok

        status = status_type()
        ok = forward_comment(point_max())
        start = point()

        if (skip_syntax_forward(".") == 0) then
            do
                moved = skip_syntax_forward("w_'")
                if (.not. looking_at("\\", status)) exit
                call forward_char(status, 2_int64)
                if (.not. status%ok()) return
            end do
        end if

        call buffer_substring(start, point(), token, status)
    end subroutine sh_smie_default_forward_token

    !> Port of sh-script.el:sh-smie--default-backward-token.
    recursive subroutine sh_smie_default_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: pos, n, p, moved
        logical :: ok, words, more

        status = status_type()
        ok = forward_comment(-point())
        pos = point()
        n = skip_syntax_backward(".")
        words = n == 0

        if (.not. words .and. n == -1) then
            ! Skip past quoted white space.
            p = point()

            if (mod(skip_syntax_backward("\"), 2_int64) == -1) then
                words = .true.
            else
                call goto_char(p)
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
                        call forward_char(status, -1_int64)

                        if (mod(skip_syntax_backward("\"), 2_int64) == -1) then
                            more = .true.
                        else
                            call goto_char(p)
                        end if
                    end if
                end if

                if (.not. more) exit
            end do
        else
            p = point()
            call goto_char(p - mod(skip_syntax_backward("\"), 2_int64))
        end if

        status = status_type()
        call buffer_substring(point(), pos, token, status)
    end subroutine sh_smie_default_backward_token

    !> The `smie_forward_token_function' of sh.  Port of
    !> sh-script.el:sh-smie-sh-forward-token.
    recursive subroutine sh_smie_sh_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tok
        type(parse_state_type) :: state
        integer(int64) :: pos, here, moved, me1
        logical :: ok, semi, hanging

        status = status_type()
        hanging = looking_at("[ "//TAB//"]*\(?:#\|\(\s|\)\|$\)", status)

        if (hanging) then
            me1 = match_end(1, status)
            here = point()
            moved = skip_chars_backward(" "//TAB, status)
            hanging = .not. is_bol()
            call goto_char(here)
        end if

        if (hanging) then
            ok = .false.

            if (me1 /= 0) then
                state = ppss()
                ok = state%instring == -1
            end if

            if (ok) then
                ! Right before a here-doc.
                call plain_forward_expression(1_int64, status)
                if (.not. status%ok()) return
                ! Pretend the here-document is a "newline representing a
                ! semi-colon", since the here-doc otherwise covers the newline(s).
                token = ";"
            else if (.not. is_eob()) then
                semi = is_sh_smie_newline_semi()
                moved = forward_line(1_int64)

                if (semi .or. is_eob()) then
                    token = ";"
                else
                    call sh_smie_sh_forward_token(token, status)
                end if
            end if

            return
        end if

        ok = forward_comment(point_max())

        if (looking_at("\\"//LF, status)) then
            moved = forward_line(1_int64)
            call sh_smie_sh_forward_token(token, status)
        else if (looking_at(sh_operators_re, status)) then
            call goto_char(match_end(0, status))
            tok = match_string(0)

            if (len(tok) > 0) then
                if (index(";&|", tok(len(tok):len(tok))) > 0) then
                    if (looking_at("[ "//TAB//"]*\(?:#\|$\)", status)) moved = forward_line(1_int64)
                end if
            end if

            token = tok
        else
            pos = point()
            call sh_smie_default_forward_token(tok, status)
            if (.not. status%ok()) return

            if (is(tok, ")")) then
                token = "case-)"
            else if (is(tok, "(")) then
                token = "case-("
            else if (starts_lower(tok) .and. in_grammar(tok)) then
                here = point()
                call goto_char(pos)
                ok = is_sh_smie_sh_keyword(tok)
                call goto_char(here)

                if (ok) then
                    token = tok
                else
                    token = " word "
                end if
            else
                call move_alloc(tok, token)
            end if
        end if
    end subroutine sh_smie_sh_forward_token

    ! Port of sh-script.el:sh-smie--looking-back-at-continuation-p.
    recursive logical function is_sh_smie_looking_back_at_continuation() result(p)
        type(status_type) :: st
        integer(int64) :: here

        here = point()

        if (cb(point()) == 10) then
            call forward_char(st, -1_int64)
            p = .true.
        else
            p = is_eol()
        end if

        if (p) p = looking_back("\(?:^\|[^\]\)\(?:\\\\\)*\\", st, line_beginning_position())
        call goto_char(here)
    end function is_sh_smie_looking_back_at_continuation

    ! (equal (syntax-after (1- (point))) (string-to-syntax "|")).
    ! Not a port.
    logical function after_fence()
        type(syntax_descriptor_type) :: d

        d = syntax_entry_at(ca(point() - 1), point() - 1)
        after_fence = d%code == 15 .and. d%match == -1
    end function after_fence

    !> The `smie_backward_token_function' of sh.  Port of
    !> sh-script.el:sh-smie-sh-backward-token.
    recursive subroutine sh_smie_sh_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tok
        type(parse_state_type) :: state
        integer(int64) :: bol, here
        logical :: ok, fence

        status = status_type()
        bol = line_beginning_position()
        ok = forward_comment(-point())
        fence = .false.

        if (is_bol() .and. .not. is_bob()) then
            if (after_fence()) then
                state = ppss()
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
        else if (point() < bol) then
            if (is_sh_smie_looking_back_at_continuation()) then
                call forward_char(status, -1_int64)
                call smie_backward_token(token, status)
            else if (is_sh_smie_newline_semi()) then
                token = ";"
            else
                call smie_backward_token(token, status)
            end if
        else if (looking_back(sh_operators_back_re, status, line_beginning_position(), greedy=.true.)) then
            call goto_char(match_beginning(1, status))
            token = match_string(1)
        else
            call sh_smie_default_backward_token(tok, status)
            if (.not. status%ok()) return

            if (is(tok, ")")) then
                token = "case-)"
            else if (is(tok, "(")) then
                token = "case-("
            else if (starts_lower(tok) .and. in_grammar(tok)) then
                here = point()
                ok = is_sh_smie_sh_keyword(tok)
                call goto_char(here)

                if (ok) then
                    token = tok
                else
                    token = " word "
                end if
            else
                call move_alloc(tok, token)
            end if
        end if
    end subroutine sh_smie_sh_backward_token

    !> The function sh adds to `smie_indent_functions': the indentation of
    !> a continued line.  Port of sh-script.el:sh-smie--indent-continuation.
    recursive subroutine sh_smie_indent_continuation(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: here, moved, ci, max_ci
        integer :: saved
        logical :: continued

        status = status_type()
        indent = smie_indent_type()
        here = point()
        continued = .false.

        if (sh_indent_after_continuation /= SH_CONTINUATION_NO) then
            moved = skip_chars_backward(" "//TAB, status)
            continued = is_sh_smie_looking_back_at_continuation()
            call goto_char(here)
            status = status_type()
        end if

        if (.not. continued) return

        if (sh_indent_after_continuation == SH_CONTINUATION_ALWAYS) then
            moved = forward_line(-1_int64)
            indent%kind = SMIE_INDENT_COLUMN

            if (is_sh_smie_looking_back_at_continuation()) then
                indent%column = int(current_indentation())
            else
                indent%column = int(current_indentation()) + sh_var_value(sh_indent_for_continuation)
            end if

            call goto_char(here)
            return
        end if

        ! Just make sure a line-continuation is indented deeper.
        saved = sh_indent_after_continuation
        sh_indent_after_continuation = SH_CONTINUATION_NO
        indent = smie_indent_calculate(status)
        sh_indent_after_continuation = saved
        call goto_char(here)
        if (.not. status%ok()) return
        if (indent%kind /= SMIE_INDENT_COLUMN) return
        max_ci = huge(0_int64)

        do
            moved = forward_line(-1_int64)
            ci = current_indentation()

            if (ci < indent%column) then
                ! Previous line is less indented, we're good.
                exit
            else if (is_sh_smie_looking_back_at_continuation()) then
                max_ci = min(max_ci, ci)
                ! Previous line is itself a continuation.
                ! If it's indented like us, we're good, otherwise
                ! check the line before that one.
                if (.not. (ci > indent%column)) exit
            else
                ! Previous line is the beginning of the continued line.
                indent%column = int(min(ci + sh_var_value(sh_indent_for_continuation), max_ci))
                exit
            end if
        end do

        call goto_char(here)
    end subroutine sh_smie_indent_continuation

    ! A rule result: OFFSET.  Not a port.
    function offset_result(offset) result(r)
        integer, intent(in) :: offset
        type(smie_rule_result_type) :: r

        r%kind = SMIE_RULE_OFFSET
        r%value = offset
    end function offset_result

    ! A rule result: (column . INDENT), nil if INDENT is nil.  Not a port.
    function column_result(indent) result(r)
        type(smie_indent_type), intent(in) :: indent
        type(smie_rule_result_type) :: r

        if (indent%kind /= SMIE_INDENT_COLUMN) return
        r%kind = SMIE_RULE_COLUMN
        r%value = indent%column
    end function column_result

    ! A rule result: (column . COLUMN).  Not a port.
    function column_at(column) result(r)
        integer(int64), intent(in) :: column
        type(smie_rule_result_type) :: r

        r%kind = SMIE_RULE_COLUMN
        r%value = int(column)
    end function column_at

    !> The `smie_rules_function' of sh.  Port of
    !> sh-script.el:sh-smie-sh-rules.
    recursive subroutine sh_smie_sh_rules(method, arg, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: arg
        type(smie_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(smie_expression_type) :: data
        type(smie_indent_type) :: v
        character(len=:), allocatable :: tok
        integer(int64) :: here
        logical :: bol

        status = status_type()
        result = smie_rule_result_type()

        select case (method)
        case (SMIE_ELEM)
            if (arg == "basic") result = offset_result(int(sh_basic_offset%value))
        case (SMIE_AFTER)
            select case (arg)
            case ("case-)")
                result = offset_result(sh_var_value(sh_indent_for_case_alt) - sh_var_value(sh_indent_for_case_label))
            case (";;", ";&", ";;&", ";|")
                ! with-demoted-errors
                call smie_backward_expression(data, status, token=arg)

                if (status%ok()) then
                    bol = is_smie_rule_bol()

                    if (.not. bol) then
                        here = point()
                        call smie_backward_token(tok, status)
                        if (is(tok, "in") .or. is(tok, ";;")) bol = is_smie_rule_bol()
                        call goto_char(here)
                    end if

                    if (bol) then
                        result = column_at(current_column())
                    else
                        v = smie_indent_calculate(status)
                        if (status%ok()) result = column_result(v)
                    end if
                end if

                if (.not. status%ok()) then
                    call message("SMIE rule error: "//format_error(status))
                    status = status_type()
                    result = smie_rule_result_type()
                end if
            case ("else")
                result = offset_result(sh_var_value(sh_indent_after_else))
            case ("if")
                result = offset_result(sh_var_value(sh_indent_after_if))
            case ("do")
                if (is_smie_rule_hanging(status)) then
                    result = offset_result(sh_var_value(sh_indent_after_loop_construct))
                else
                    result = offset_result(sh_var_value(sh_indent_after_do))
                end if
            case ("in")
                result = offset_result(sh_var_value(sh_indent_for_case_label))
            case ("(", "{", "[")
                if (.not. looking_at(".[ "//TAB//"]*[^"//LF//" "//TAB//"#]", status)) then
                    result = offset_result(sh_var_value(sh_indent_after_open))
                else
                    call goto_char(match_end(0, status) - 1)
                    result = column_at(current_column())
                end if
            case default
            end select
        case (SMIE_BEFORE)
            select case (arg)
            case ("(", "{", "[", "while", "if", "for", "case")
                if (arg == "{" .and. is_smie_rule_parent([character(len=4) :: "for", "case"], status)) then
                    call smie_backward_expression(data, status, token="in")

                    if (is(data%token, "for") .or. is(data%token, "case")) then
                        v = smie_indent_virtual(status)
                        result = column_result(v)
                    end if
                else if (.not. is_smie_rule_prev([character(len=2) :: "&&", "||", "|"], status)) then
                    if (is_smie_rule_hanging(status)) result = smie_rule_parent(status)
                else
                    if (.not. is_smie_rule_bol()) then
                        do
                            call smie_backward_expression(data, status, halfexpression=.true.)
                            if (.not. is(data%token, "|")) exit
                        end do

                        v = smie_indent_virtual(status)
                        result = column_result(v)
                    end if
                end if
                ! FIXME: Maybe this handling of ;; should be made into
                ! a smie-rule-terminator function that takes the substitute ";" as arg.
            case (";;", ";&", ";;&", ";|")
                if (is_smie_rule_bol() .and. looking_at(";\(?:;&?\|[&|]\)?[ "//TAB//"]*\(#\|$\)", status)) then
                    call smie_indent_keyword(v, status, ";")
                    result = column_result(v)
                else
                    result = smie_rule_separator(method, status)
                end if
            case ("|", "&&", "||")
                if (.not. is_smie_rule_parent([arg], status)) then
                    if (arg /= "&&" .or. sh_indent_statement_after_and) then
                        call smie_backward_expression(data, status, token=arg)
                        v = smie_indent_virtual(status)

                        if (v%kind == SMIE_INDENT_COLUMN) then
                            v%column = int(sh_basic_offset%value) + v%column
                            result = column_result(v)
                        end if
                    end if
                end if
                ! Attempt at backward compatibility with the old config variables.
            case ("fi")
                result = offset_result(sh_var_value(sh_indent_for_fi))
            case ("done")
                result = offset_result(sh_var_value(sh_indent_for_done))
            case ("then")
                result = offset_result(sh_var_value(sh_indent_for_then))
            case ("do")
                result = offset_result(sh_var_value(sh_indent_for_do))
            case default
            end select
        case default
        end select
        ! sh-indent-after-done: aligned completely differently.
        ! sh-indent-for-continuation: Line continuations are handled differently.
        ! sh-indent-after-function: we don't handle it differently.
    end subroutine sh_smie_sh_rules

    ! Check if we're after the first arg of an if/while/for/... construct.
    ! Returns the construct's token and moves point before it, if so.
    ! Port of sh-script.el:sh-smie--rc-after-special-arg-p.
    recursive subroutine sh_smie_rc_after_special_arg_p(construct)
        character(len=:), allocatable, intent(out) :: construct
        character(len=:), allocatable :: tok
        type(status_type) :: st
        logical :: ok

        ok = forward_comment(-point())
        if (.not. looking_back(")\|\_<not", st, point() - 3)) return
        ! ignore-errors
        call plain_forward_expression(-1_int64, st)
        if (.not. st%ok()) return
        call smie_backward_token(tok, st)
        if (.not. st%ok()) return
        if (is(tok, "if") .or. is(tok, "for") .or. is(tok, "switch") .or. is(tok, "while")) call move_alloc(tok, construct)
    end subroutine sh_smie_rc_after_special_arg_p

    ! Return non-nil if a newline should be treated as a semi-colon.
    ! Point should be before the newline.  Port of
    ! sh-script.el:sh-smie--rc-newline-semi-p.
    recursive logical function is_sh_smie_rc_newline_semi() result(semi)
        character(len=:), allocatable :: tok, construct
        type(status_type) :: st
        integer(int64) :: here
        logical :: special

        here = point()
        call smie_backward_token(tok, st)
        special = .false.

        if (is(tok, "not")) then
            call forward_word_strictly()
            special = .true.
        else if (empty(tok) .and. cb(point()) == iachar(")")) then
            special = .true.
        end if

        if (special) then
            call sh_smie_rc_after_special_arg_p(construct)
            semi = .not. allocated(construct)
        else
            semi = is_sh_smie_newline_semi(tok)
        end if

        call goto_char(here)
    end function is_sh_smie_rc_newline_semi

    ! (forward-word-strictly 1).  Not a port.
    subroutine forward_word_strictly()
        call forward_word(1_int64)
    end subroutine forward_word_strictly

    !> The `smie_forward_token_function' of rc.  Port of
    !> sh-script.el:sh-smie-rc-forward-token.
    recursive subroutine sh_smie_rc_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tok
        type(parse_state_type) :: state
        integer(int64) :: pos, here, moved, me1
        logical :: ok, semi, hanging

        status = status_type()
        ! FIXME: Code duplication with sh-smie-sh-forward-token.
        hanging = looking_at("[ "//TAB//"]*\(?:#\|\(\s|\)\|$\)", status)

        if (hanging) then
            me1 = match_end(1, status)
            here = point()
            moved = skip_chars_backward(" "//TAB, status)
            hanging = .not. is_bol()
            call goto_char(here)
        end if

        if (hanging) then
            ok = .false.

            if (me1 /= 0) then
                state = ppss()
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
                semi = is_sh_smie_rc_newline_semi()
                moved = forward_line(1_int64)

                if (semi .or. is_eob()) then
                    token = ";"
                else
                    call sh_smie_rc_forward_token(token, status)
                end if
            end if

            return
        end if

        ok = forward_comment(point_max())

        if (looking_at("\\"//LF, status)) then
            moved = forward_line(1_int64)
            call sh_smie_rc_forward_token(token, status)
        else
            pos = point()
            call sh_smie_default_forward_token(tok, status)
            if (.not. status%ok()) return

            if (starts_lower(tok) .and. in_grammar(tok)) then
                here = point()
                call goto_char(pos)
                ok = is_sh_smie_keyword()
                call goto_char(here)

                if (ok) then
                    call move_alloc(tok, token)
                else
                    token = " word "
                end if
            else
                call move_alloc(tok, token)
            end if
        end if
    end subroutine sh_smie_rc_forward_token

    !> The `smie_backward_token_function' of rc.  Port of
    !> sh-script.el:sh-smie-rc-backward-token.
    recursive subroutine sh_smie_rc_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tok
        type(parse_state_type) :: state
        integer(int64) :: bol, here
        logical :: ok, fence

        status = status_type()
        ! FIXME: Code duplication with sh-smie-sh-backward-token.
        bol = line_beginning_position()
        ok = forward_comment(-point())
        fence = .false.

        if (is_bol() .and. .not. is_bob()) then
            if (after_fence()) then
                state = ppss()
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
        else if (point() < bol) then
            ! We skipped over a newline.
            if (is_eol() .and. looking_back("\(?:^\|[^\]\)\(?:\\\\\)*\\", status, line_beginning_position())) then
                ! A continued line.
                call forward_char(status, -1_int64)
                call smie_backward_token(token, status)
            else if (is_sh_smie_rc_newline_semi()) then
                token = ";"
            else
                call smie_backward_token(token, status)
            end if
        else
            call sh_smie_default_backward_token(tok, status)
            if (.not. status%ok()) return

            if (starts_lower(tok) .and. in_grammar(tok)) then
                here = point()
                ok = is_sh_smie_keyword()
                call goto_char(here)

                if (ok) then
                    call move_alloc(tok, token)
                else
                    token = " word "
                end if
            else
                call move_alloc(tok, token)
            end if
        end if
    end subroutine sh_smie_rc_backward_token

    !> The `smie_rules_function' of rc.  Port of
    !> sh-script.el:sh-smie-rc-rules.
    recursive subroutine sh_smie_rc_rules(method, arg, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: arg
        type(smie_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: construct
        integer(int64) :: here

        status = status_type()
        result = smie_rule_result_type()

        select case (method)
        case (SMIE_ELEM)
            if (arg == "basic") result = offset_result(int(sh_basic_offset%value))
        case (SMIE_AFTER)
            if (arg == ";") then
                if (is_smie_rule_parent([character(len=4) :: "case"], status)) then
                    result = smie_rule_parent(status, sh_var_value(sh_indent_after_case))
                end if
            end if
        case (SMIE_BEFORE)
            select case (arg)
            case ("{")
                here = point()
                call sh_smie_rc_after_special_arg_p(construct)
                if (allocated(construct)) result = column_at(current_column())
                call goto_char(here)
            case ("(", "[")
                if (is_smie_rule_hanging(status)) result = smie_rule_parent(status)
            case default
            end select
        case (SMIE_LIST_INTRO)
            ! FIXME: SMIE parses "if (exp) cmd" as "(if ((exp) cmd))" so "cmd" is
            ! treated as an arg to (exp) by default, which indents it all wrong.
            ! To handle it right, we should extend smie-indent-exps so that the
            ! preceding keyword can give special rules.  Currently the only special
            ! rule we have is the :list-intro hack, which we use here to align "cmd"
            ! with "(exp)", which is rarely the right thing to do, but is better
            ! than nothing.
            if (arg == "for" .or. arg == "if" .or. arg == "while") result%kind = SMIE_RULE_YES
        case default
        end select
        ! sh-indent-after-switch: handled implicitly by the default { rule.
    end subroutine sh_smie_rc_rules

    ! The grammars.  Not a port.
    subroutine init_grammars(status)
        type(status_type), intent(out) :: status
        type(smie_bnf_type) :: bnf
        type(smie_precs_type) :: precs(2)
        type(smie_prec2_type) :: prec2
        character(len=:), allocatable :: ops(:)
        integer :: i, n

        status = status_type()
        if (.not. sh_smie_sh_grammar%unset) return
        ! A constant, or a $var, or a sequence of them...
        call bnf%add_nonterminal("exp")
        call bnf%add("cmd", [character(len=8) :: "case", "exp", "in", "branches", "esac"])
        call bnf%add("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "fi"])
        call bnf%add("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "else", "cmd", "fi"])
        call bnf%add("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "elif", "cmd", "then", "cmd", "fi"])
        call bnf%add("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "elif", "cmd", "then", "cmd", "else", "cmd", &
                             "fi"])
        call bnf%add("cmd", [character(len=4) :: "if", "cmd", "then", "cmd", "elif", "cmd", "then", "cmd", &
                             "elif", "cmd", "then", "cmd", "else", "cmd", "fi"])
        call bnf%add("cmd", [character(len=5) :: "while", "cmd", "do", "cmd", "done"])
        call bnf%add("cmd", [character(len=5) :: "until", "cmd", "do", "cmd", "done"])
        call bnf%add("cmd", [character(len=4) :: "for", "exp", "in", "cmd", "do", "cmd", "done"])
        call bnf%add("cmd", [character(len=4) :: "for", "exp", "do", "cmd", "done"])
        call bnf%add("cmd", [character(len=6) :: "select", "exp", "in", "cmd", "do", "cmd", "done"]) ! bash&zsh&ksh88.
        call bnf%add("cmd", [character(len=6) :: "repeat", "exp", "do", "cmd", "done"]) ! zsh.
        call bnf%add("cmd", [character(len=6) :: "exp", "always", "exp"]) ! zsh.
        call bnf%add("cmd", [character(len=3) :: "cmd", "|", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "|&", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "&&", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "||", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", ";", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "&", "cmd"])
        call bnf%add("rpattern", [character(len=8) :: "rpattern", "|", "rpattern"])
        call bnf%add("pattern", [character(len=8) :: "rpattern"])
        call bnf%add("pattern", [character(len=8) :: "case-(", "rpattern"])
        call bnf%add("branches", [character(len=8) :: "branches", ";;", "branches"])
        call bnf%add("branches", [character(len=8) :: "branches", ";&", "branches"])
        call bnf%add("branches", [character(len=8) :: "branches", ";;&", "branches"]) ! bash.
        call bnf%add("branches", [character(len=8) :: "branches", ";|", "branches"]) ! zsh.
        call bnf%add("branches", [character(len=7) :: "pattern", "case-)", "cmd"])
        call precs(1)%add(SMIE_ASSOC, [character(len=3) :: ";;", ";&", ";;&", ";|"])
        call precs(2)%add(SMIE_ASSOC, [character(len=1) :: ";", "&"])
        call precs(2)%add(SMIE_ASSOC, [character(len=2) :: "&&", "||"])
        call precs(2)%add(SMIE_ASSOC, [character(len=2) :: "|", "|&"])
        call smie_bnf_to_prec2(bnf, prec2, status, precs=precs)
        if (.not. status%ok()) return
        call smie_prec2_to_grammar(prec2, sh_smie_sh_grammar, status)
        if (.not. status%ok()) return
        ! sh-smie--sh-operators: the tokens not starting with a letter.
        n = 0

        do i = 1, size(sh_smie_sh_grammar%tokens)
            if (is_operator(sh_smie_sh_grammar%tokens(i)%s)) n = n + 1
        end do

        allocate (character(len=3) :: ops(n))
        n = 0

        do i = 1, size(sh_smie_sh_grammar%tokens)
            if (is_operator(sh_smie_sh_grammar%tokens(i)%s)) then
                n = n + 1
                ops(n) = sh_smie_sh_grammar%tokens(i)%s
            end if
        end do

        sh_operators_re = regexp_opt(ops)
        sh_operators_back_re = "\(?:^\|[^\]\)\(?:\\\\\)*"//"\("//sh_operators_re//"\)"
        ! rc.
        bnf = smie_bnf_type()
        call bnf%add_nonterminal("exp")
        call bnf%add("cmd", [character(len=4) :: "cmd", "case", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "if", "exp"])
        call bnf%add("cmd", [character(len=6) :: "switch", "exp"])
        call bnf%add("cmd", [character(len=3) :: "for", "exp"])
        call bnf%add("cmd", [character(len=5) :: "while", "exp"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "|", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "|&", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "&&", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "||", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", ";", "cmd"])
        call bnf%add("cmd", [character(len=3) :: "cmd", "&", "cmd"])
        call bnf%add("pattern", [character(len=7) :: "pattern", "|", "pattern"])
        call bnf%add("branches", [character(len=8) :: "branches", ";;", "branches"])
        call bnf%add("branches", [character(len=8) :: "branches", ";&", "branches"])
        call bnf%add("branches", [character(len=8) :: "branches", ";;&", "branches"]) ! bash.
        call bnf%add("branches", [character(len=8) :: "branches", ";|", "branches"]) ! zsh.
        call bnf%add("branches", [character(len=7) :: "pattern", "case-)", "cmd"])
        precs(1) = smie_precs_type()
        precs(2) = smie_precs_type()
        call precs(1)%add(SMIE_ASSOC, [character(len=3) :: ";;", ";&", ";;&", ";|"])
        call precs(2)%add(SMIE_ASSOC, [character(len=4) :: "case"])
        call precs(2)%add(SMIE_ASSOC, [character(len=1) :: ";", "&"])
        call precs(2)%add(SMIE_ASSOC, [character(len=2) :: "&&", "||"])
        call precs(2)%add(SMIE_ASSOC, [character(len=2) :: "|", "|&"])
        prec2 = smie_prec2_type()
        call smie_bnf_to_prec2(bnf, prec2, status, precs=precs)
        if (.not. status%ok()) return
        call smie_prec2_to_grammar(prec2, sh_smie_rc_grammar, status)

    contains

        logical function is_operator(tok)
            character(len=*), intent(in) :: tok

            is_operator = .true.
            if (len(tok) > 0) is_operator = .not. (tok(1:1) >= "a" .and. tok(1:1) <= "z")
        end function is_operator
    end subroutine init_grammars

    ! The around advice sh puts on smie--hanging-eolp-function.  Not a port.
    recursive logical function is_sh_hanging_eol() result(eol)
        type(status_type) :: st

        if (looking_at("[ "//TAB//"]*\\"//LF, st)) then
            call goto_char(match_end(0, st))
            eol = .true.
        else
            eol = is_smie_default_hanging_eol()
        end if
    end function is_sh_hanging_eol

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    ! Guess the shell used in the current buffer.
    ! Return the name of the shell suitable for `sh_set_shell'.  Port of
    ! sh-script.el:sh--guess-shell.
    function sh_guess_shell() result(shell)
        character(len=:), allocatable :: shell
        character(len=:), allocatable :: name, base
        type(status_type) :: st
        integer(int64) :: here
        logical :: matched

        here = point()
        call goto_char(point_min())
        matched = .false.
        if (allocated(auto_mode_interpreter_regexp)) matched = looking_at(auto_mode_interpreter_regexp, st)
        if (matched) shell = match_string(2)
        call goto_char(here)
        if (matched) return
        shell = sh_shell_file%value
        if (.not. allocated(buffers(cur)%filename)) return
        ! Checks that use `buffer_file_name' follow.
        name = buffers(cur)%filename
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
    end function sh_guess_shell

    !> Generic major mode for editing shell scripts.
    !>
    !> This is a generic major mode intended to be inherited by concrete
    !> implementations.  Currently there are two: `sh_mode' and
    !> `bash_ts_mode'.  Port of sh-script.el:sh-base-mode.
    recursive subroutine sh_base_mode(status)
        type(status_type), intent(out) :: status
        type(capf_entry_type), allocatable :: fns(:)
        logical :: saved
        integer :: k

        call major_modes%sh_base_mode%begin(saved, status)
        if (.not. status%ok()) return
        call sh_shell_file%make_local()
        call sh_shell%make_local()
        call paragraph_start%set_local(page_delimiter%value//"\|$")
        call paragraph_separate%set_local(paragraph_start%value//"\|#!/")
        call comment_start%set_local("# ")
        call comment_start_skip%set_local("#+["//TAB//" ]*")
        call set_local_abbrev_table(sh_mode_abbrev_table)
        ! (setq-local comint-dynamic-complete-functions
        ! sh-dynamic-complete-functions)
        fns = sh_dynamic_complete_functions%functions()
        call comint_dynamic_complete_functions%add(comint_filename_completion, local=.true.)
        call comint_dynamic_complete_functions%clear()

        do k = size(fns), 1, -1
            call comint_dynamic_complete_functions%add(fns(k)%fn, local=.true.)
        end do

        call completion_at_point_functions%add(comint_completion_at_point, local=.true.)
        ! we can't look if previous line ended with `\'
        call comint_prompt_regexp%set_local("^[ "//TAB//"]*")
        call syntax_propertize_function%set_local(sh_syntax_propertize_function)
        call syntax_propertize_extend_region_functions%add(syntax_propertize_multiline, depth=90, local=.true.)
        call subprogram_prompt_regexp%set_local("^\(\(function[ "//TAB//"]\)?[ "//TAB//"]*[[:alnum:]_]+[ "//TAB// &
                                                "]*([ "//TAB//"]*)\|function[ "//TAB//"]+[[:alnum:]_]+[ "//TAB// &
                                                "]*\(([ "//TAB//"]*)\)?\)[ "//TAB//"]*")
        call completion_at_point_functions%add(sh_completion_at_point_function, local=.true.)
        call major_modes%sh_base_mode%finish(saved, status)
    end subroutine sh_base_mode

    !> Major mode for editing shell scripts.
    !> This mode works for many shells, since they all have roughly the same syntax,
    !> as far as commands, arguments, variables, pipes, comments etc. are concerned.
    !> Unless the file's magic number indicates the shell, your usual shell is
    !> assumed.  Since filenames rarely give a clue, they are not further analyzed.
    !>
    !> This mode adapts to the variations between shells (see `sh_set_shell') by
    !> means of an inheritance based feature lookup (see `sh_feature').  This
    !> mechanism applies to all variables (including skeletons) that pertain to
    !> shell-specific features.  Shell script files can use the `sh_shell' local
    !> variable to indicate the shell variant to be used for the file.
    !>
    !> The default style of this mode is that of Rosenblatt's Korn shell book.
    !> The syntax of the statements varies with the shell being used.  The
    !> following commands are available, based on the current shell's syntax:
    !> \<sh_mode_map>
    !> \[sh_case]    case statement
    !> \[sh_for]     for loop
    !> \[sh_function]        function definition
    !> \[sh_if]      if statement
    !> \[sh_indexed_loop]    indexed loop from 1 to n
    !> \[sh_while_getopts]   while getopts loop
    !> \[sh_repeat]  repeat loop
    !> \[sh_select]  select loop
    !> \[sh_until]   until loop
    !> \[sh_while]   while loop
    !>
    !> For sh and rc shells indentation commands are:
    !> \[smie_config_show_indent]    Show the rules controlling this line's indentation.
    !> \[smie_config_set_indent]     Change the rules controlling this line's indentation.
    !> \[smie_config_guess]  Try to tweak the indentation rules so the
    !> buffer indents as it currently is indented.
    !>
    !>
    !> \[backward_delete_char_untabify]      Delete backward one position, even if it was a tab.
    !> \[sh_end_of_command]  Go to end of successive commands.
    !> \[sh_beginning_of_command]    Go to beginning of successive commands.
    !> \[sh_set_shell]       Set this buffer's shell, and maybe its magic number.
    !> \[sh_execute_region]  Have optional header and region be executed in a subshell.
    !>
    !> `sh_electric_here_document_mode' controls whether insertion of two
    !> unquoted < insert a here document.  You can control this behavior by
    !> modifying `sh_mode_hook'.
    !>
    !> If you generally program a shell different from your login shell you can
    !> set `sh_shell_file' accordingly.  If your shell's file name doesn't correctly
    !> indicate what shell it is use `sh_alias_table' to translate.
    !>
    !> If your shell gives error messages with line numbers, you can use \[executable_interpret]
    !> with your script for an edit-interpret-debug cycle.  Port of
    !> sh-script.el:sh-mode.
    recursive subroutine sh_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keywords_type) :: levels(3)
        type(font_lock_syntax_entry_type) :: syntax(1)
        logical :: saved

        call major_modes%sh_mode%begin(saved, status)
        if (.not. status%ok()) return
        levels(1)%function => keywords_level_0
        levels(2)%function => keywords_level_1
        levels(3)%function => sh_font_lock_keywords_2
        syntax(1)%chars = "/~.-_"
        syntax(1)%syntax = "w"
        call font_lock_defaults%set_local(levels=levels, keywords_only=.false., case_fold=.false., syntax_modifications=syntax, &
                                          syntactic_face_function=sh_font_lock_syntactic_face_function)
        ! Parse or insert magic number for exec, and set all variables depending
        ! on the shell thus determined.
        call sh_set_shell(sh_guess_shell(), status, .false., .false.)
        if (.not. status%ok()) return
        call major_modes%sh_mode%finish(saved, status)
    end subroutine sh_mode

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
    !> Calls the value of `sh_set_shell_hook' if set.
    !>
    !> Shell script files can cause this function be called automatically
    !> when the file is visited by having a `sh_shell' file-local variable
    !> whose value is the shell name (don't quote it).  Port of
    !> sh-script.el:sh-set-shell.
    recursive subroutine sh_set_shell(shell_in, status, no_query_flag, insert_flag)
        character(len=*), intent(in) :: shell_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_query_flag, insert_flag
        character(len=:), allocatable :: shell, file, arg, supported
        type(sh_string_type), allocatable :: vars(:)
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

        sh_shell = sh_canonicalize_shell(shell)
        insert_it = .false.
        if (present(insert_flag)) insert_it = insert_flag

        if (insert_it) then
            arg = sh_shell_arg%get()

            if (allocated(arg)) then
                call executable_set_magic(shell, file, status, arg, no_query_flag, insert_flag)
            else
                call executable_set_magic(shell, file, status, no_query_flag=no_query_flag, insert_flag=insert_flag)
            end if

            if (.not. status%ok()) return
            sh_shell_file = file
        end if

        parts(1)%text = "["//sh_shell%value//"]"
        parts(1)%face = ""
        call set_mode_line_process(parts)
        k = local_index(.true.)
        if (k == 0) return
        locals(k)%has_variables = .true.
        if (allocated(locals(k)%variables)) deallocate (locals(k)%variables)
        allocate (locals(k)%variables(0))
        locals(k)%variables_initialized = .false.
        e = syntax_input_index()

        if (e > 0) then
            if (sh_mode_syntax_table_input%entries(e)%table%index /= 0) then
                call copy_syntax_table(table, st, sh_mode_syntax_table_input%entries(e)%table)

                do i = 1, size(sh_mode_syntax_table_input%entries(e)%changes)
                    associate (c => sh_mode_syntax_table_input%entries(e)%changes(i))
                        call modify_syntax_entry(c%char, c%char, c%syntax, st, table)
                    end associate
                end do

                if (locals(k)%has_table) call free_syntax_table(locals(k)%table, st)
                locals(k)%has_table = .true.
                locals(k)%table = table
                call set_syntax_table(table, st)
            end if
        end if

        call sh_variables%get(vars)

        do i = 1, size(vars)
            call sh_remember_variable(vars(i)%s)
        end do

        supported = sh_indent_supported%get()
        k = local_index(.true.)
        locals(k)%has_indent_supported = allocated(supported)

        if (allocated(supported)) then
            locals(k)%indent_supported = supported
            call message("Setting up indent for shell type "//sh_shell%value)
            call smie_set_is_hanging_eol_function(is_sh_hanging_eol)
            call smie_indent_functions%add(sh_smie_indent_continuation, local=.true.)

            if (supported == "rc") then
                call smie_setup(sh_smie_rc_grammar, sh_smie_rc_rules, sh_smie_rc_forward_token, sh_smie_rc_backward_token)
            else
                call smie_setup(sh_smie_sh_grammar, sh_smie_sh_rules, sh_smie_sh_forward_token, sh_smie_sh_backward_token)
            end if

            if (sh_make_vars_local) call cmd_sh_make_vars_local(st)
            call message("Indentation setup for shell type "//sh_shell%value)
        else
            call message("No indentation for this shell type.")
            call indent_line_function%set_local(sh_basic_indent_line_function)
        end if

        if (minor_modes%font_lock_mode%enabled()) then
            call font_lock_forget_defaults()
            call font_lock_set_defaults(st)
            call font_lock_flush(st)
        end if

        call sh_set_shell_hook%run(status)
    end subroutine sh_set_shell

    ! `sh_set_shell' as a command.
    recursive subroutine cmd_sh_set_shell(status)
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
        n = size(sh_ancestor_table)
        allocate (names(n + 3))

        do i = 1, n
            names(i) = make_string(sh_ancestor_table(i)%shell)
        end do

        names(n + 1) = make_string("csh")
        names(n + 2) = make_string("rc")
        names(n + 3) = make_string("sh")
        table%list = make_list(names)
        call completing_read(format_prompt("Shell", sh_shell_file%value), table, shell, status, &
                             default=sh_shell_file%value)
        if (.not. status%ok()) return
        call sh_set_shell(shell, status, executable_query == EXECUTABLE_QUERY_FUNCTION, nilp(current_prefix_arg))
    end subroutine cmd_sh_set_shell

    !> Convert a shell name SHELL to the one we should handle it as.
    !> SHELL is a full path to the shell interpreter; return a shell
    !> name.  Port of sh-script.el:sh-canonicalize-shell.
    function sh_canonicalize_shell(shell_in) result(name)
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

        if (allocated(sh_alias_table)) then
            do i = 1, size(sh_alias_table)
                if (.not. sh_alias_table(i)%basename) then
                    if (string_match(sh_alias_table(i)%key, shell, st) > 0) then
                        name = sh_alias_table(i)%shell
                        return
                    end if
                end if

                if (sh_alias_table(i)%basename .and. sh_alias_table(i)%key == base) then
                    name = sh_alias_table(i)%shell
                    return
                end if
            end do
        end if

        name = base
    end function sh_canonicalize_shell

    !> Return the value of the indentation variable VAR, interpreting the
    !> symbols SH_PLUS, SH_MINUS, SH_PLUS_PLUS, SH_MINUS_MINUS, SH_STAR and
    !> SH_SLASH by `sh_basic_offset'.  Port of sh-script.el:sh-var-value.
    integer function sh_var_value(var) result(value)
        type(integer_setting), intent(in) :: var
        integer :: offset

        offset = int(sh_basic_offset%value)

        select case (var%value)
        case (SH_PLUS)
            value = offset
        case (SH_MINUS)
            value = -offset
        case (SH_PLUS_PLUS)
            value = 2*offset
        case (SH_MINUS_MINUS)
            value = 2*(-offset)
        case (SH_STAR)
            value = offset/2
        case (SH_SLASH)
            value = (-offset)/2
        case default
            value = int(var%value)
        end select
    end function sh_var_value

    !> Indent a line for Sh mode (shell script mode).
    !> Indent as far as preceding non-empty line, then by steps of `sh_basic_offset'.
    !> Lines containing only comments are considered empty.  Port of
    !> sh-script.el:sh-basic-indent-line.
    recursive subroutine sh_basic_indent_line(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: mark
        integer(int64) :: previous, current, here, moved, col
        logical :: newline_cmd

        status = status_type()
        here = point()

        do
            call beginning_of_line()
            if (is_bob()) exit
            moved = forward_line(-1_int64)
            call back_to_indentation()
            if (.not. (is_eol() .or. ca(point()) == iachar("#"))) exit
        end do

        previous = current_column()
        call goto_char(here)
        call copy_marker_at(point(), mark, .false., status)
        newline_cmd = value_eq(this_command, sym("newline_and_indent"))

        if (.not. newline_cmd .and. minor_modes%electric_indent_mode%enabled()) then
            newline_cmd = value_eq(this_command, sym("newline"))
        end if

        if (newline_cmd) then
            col = previous
        else
            here = current_column()
            call back_to_indentation()
            current = current_column()

            if (here < current) then
                if (is_eol()) then
                    col = previous
                else
                    col = 0
                end if
            else
                call delete_region(point(), line_beginning_position(), status)

                if (sh_basic_offset%value == 0) then
                    call free_marker(mark, st)
                    call set_error(status, FM_ARITH_ERROR, "Arithmetic error")
                    return
                end if

                if (is_eol()) then
                    col = max(previous, (1 + current/sh_basic_offset%value)*sh_basic_offset%value)
                else
                    col = (1 + current/sh_basic_offset%value)*sh_basic_offset%value
                end if
            end if
        end if

        col = indent_to(col, status)
        call goto_char(mpos(mark))
        call free_marker(mark, st)
        if (current_column() < current_indentation()) moved = skip_chars_forward(" "//TAB, status)
    end subroutine sh_basic_indent_line

    ! `sh_basic_indent_line' as an `indent_line_function'.  Not a port.
    recursive subroutine sh_basic_indent_line_function(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent

        noindent = .false.
        call sh_basic_indent_line(status)
    end subroutine sh_basic_indent_line_function

    ! `sh_basic_indent_line' as a command.
    recursive subroutine cmd_sh_basic_indent_line(status)
        type(status_type), intent(out) :: status

        call sh_basic_indent_line(status)
    end subroutine cmd_sh_basic_indent_line

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Pass optional header and region to a subshell for noninteractive execution.
    !> The working directory is that of the buffer, and only environment variables
    !> are already set which is why you can mark a header within the script.
    !>
    !> The executed subshell is `sh_shell_file'.
    !>
    !> With a positive prefix ARG, instead of sending region, define header from
    !> beginning of buffer to point.  With a negative prefix ARG, instead of sending
    !> region, clear header.  Port of sh-script.el:sh-execute-region.
    recursive subroutine sh_execute_region(start, end_in, status, flag)
        integer(int64), intent(in) :: start, end_in
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64), intent(in), optional :: flag
        character(len=:), allocatable :: saved_shell, text
        integer(int64) :: end, header, here, saved_undo
        integer :: k

        status = status_type()
        k = local_index(.true.)
        if (k == 0) return

        if (present(flag)) then
            if (locals(k)%has_header) call free_marker(locals(k)%header_marker, st)
            locals(k)%has_header = .false.

            if (flag > 0) then
                call copy_marker_at(point(), locals(k)%header_marker, .false., status)
                locals(k)%has_header = .true.
            end if

            return
        end if

        if (allocated(shell_file_name%value)) saved_shell = shell_file_name%value
        shell_file_name = sh_shell_file%value

        if (locals(k)%has_header) then
            here = point()
            ! (let (buffer-undo-list) ...)
            saved_undo = buffers(cur)%undo_count
            header = mpos(locals(k)%header_marker)
            call goto_char(header)
            call buffer_substring(start, end_in, text, status)
            if (status%ok()) call insert(text, status)
            end = header + (end_in - start)
            if (status%ok()) call shell_command_on_region(point_min(), end, sh_shell_file%value, status)
            k = local_index(.false.)
            header = mpos(locals(k)%header_marker)
            call delete_region(header, end, status)
            buffers(cur)%undo_count = saved_undo
            call goto_char(here)
        else
            call shell_command_on_region(start, end_in, sh_shell_file%value//" -", status)
        end if

        if (allocated(saved_shell)) then
            shell_file_name = saved_shell
        else
            call shell_file_name%clear()
        end if
    end subroutine sh_execute_region

    ! `sh_execute_region' as a command.
    recursive subroutine cmd_sh_execute_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return

        if (nilp(current_prefix_arg)) then
            call sh_execute_region(b, e, status)
        else
            call sh_execute_region(b, e, status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_sh_execute_region

    !> Make VAR available for future completing reads in this buffer.
    !> Port of sh-script.el:sh-remember-variable.
    subroutine sh_remember_variable(var)
        character(len=*), intent(in) :: var
        type(sh_string_type), allocatable :: grown(:)
        character(len=:), allocatable :: value
        logical :: found
        integer :: k, i

        if (len(var) < sh_remember_variable_min) return
        call getenv(var, value, found)
        if (found) return
        k = local_index(.false.)

        if (k > 0) then
            if (.not. locals(k)%has_variables) k = 0
        end if

        if (k > 0) then
            do i = 1, size(locals(k)%variables)
                if (locals(k)%variables(i)%s == var) return
            end do

            allocate (grown(size(locals(k)%variables) + 1))
            grown(1)%s = var

            do i = 1, size(locals(k)%variables)
                call move_alloc(locals(k)%variables(i)%s, grown(i + 1)%s)
            end do

            call move_alloc(grown, locals(k)%variables)
        else
            do i = 1, size(default_variables)
                if (default_variables(i)%s == var) return
            end do

            allocate (grown(size(default_variables) + 1))
            grown(1)%s = var

            do i = 1, size(default_variables)
                call move_alloc(default_variables(i)%s, grown(i + 1)%s)
            end do

            call move_alloc(grown, default_variables)
        end if
    end subroutine sh_remember_variable

    !> Is point preceded by an odd number of backslashes?  Port of
    !> sh-script.el:sh-quoted-p.
    logical function is_sh_quoted()
        type(status_type) :: st
        integer(int64) :: here

        here = point()
        is_sh_quoted = mod(skip_chars_backward("\\", st), 2_int64) == -1
        call goto_char(here)
    end function is_sh_quoted

    ! The variables `sh_make_vars_local' and
    ! `sh_reset_indent_vars_to_global_values' handle (sh-var-list).
    ! Not a port.
    subroutine for_var_list(make, kill)
        logical, intent(in) :: make, kill

        call one(sh_basic_offset)
        call one(sh_indent_after_case)
        call one(sh_indent_after_do)
        call one(sh_indent_after_done)
        call one(sh_indent_after_else)
        call one(sh_indent_after_if)
        call one(sh_indent_after_loop_construct)
        call one(sh_indent_after_open)
        call one(sh_indent_for_case_alt)
        call one(sh_indent_for_case_label)
        call one(sh_indent_for_continuation)
        call one(sh_indent_for_do)
        call one(sh_indent_for_done)
        call one(sh_indent_for_fi)
        call one(sh_indent_for_then)

    contains

        subroutine one(var)
            type(integer_setting), intent(inout) :: var

            if (kill) call var%kill_local()
            if (make) call var%make_local()
        end subroutine one
    end subroutine for_var_list

    !> Make the indentation variables local to this buffer.
    !> Normally they already are local.  This command is provided in case
    !> variable `sh_make_vars_local' has been set to false.
    !>
    !> To revert all these variables to the global values, use
    !> command `sh_reset_indent_vars_to_global_values'.  Port of
    !> sh-script.el:sh-make-vars-local.
    subroutine cmd_sh_make_vars_local(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call for_var_list(.true., .false.)
        call message("Indentation variables are now local.")
    end subroutine cmd_sh_make_vars_local

    !> Reset local indentation variables to the global values.
    !> Then, if variable `sh_make_vars_local' is true, make them local.
    !> Port of sh-script.el:sh-reset-indent-vars-to-global-values.
    subroutine sh_reset_indent_vars_to_global_values(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call for_var_list(.false., .true.)
        if (sh_make_vars_local) call for_var_list(.true., .false.)
    end subroutine sh_reset_indent_vars_to_global_values

    !> Scan the buffer for variable assignments.
    !> Add these variables to `sh_shell_variables'.  Port of
    !> sh-script.el:sh-shell-initialize-variables.
    subroutine sh_shell_initialize_variables(status)
        type(status_type), intent(out) :: status
        integer(int64) :: here, found
        integer :: k

        call message("Scanning buffer `"//buffers(cur)%name//"' for variable assignments...")
        here = point()
        call goto_char(point_min())
        k = local_index(.false.)

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
            call sh_assignment_collect(status)
        end do

        call goto_char(here)
        call post_self_insert_hook%add(sh_assignment_collect, local=.true.)
        call message("Scanning buffer `"//buffers(cur)%name//"' for variable assignments...done")
    end subroutine sh_shell_initialize_variables

    ! Whether `sh_shell_variables' is initialized (sh-shell-variables-initialized).
    ! Not a port.
    logical function variables_initialized()
        integer :: k

        variables_initialized = default_variables_initialized
        k = local_index(.false.)
        if (k == 0) return
        if (locals(k)%has_variables) variables_initialized = locals(k)%variables_initialized
    end function variables_initialized

    !> Insert an addition of VAR and prefix DELTA for Bourne (type) shell.
    !> Port of sh-script.el:sh-add.
    subroutine sh_add(var, delta, status)
        character(len=*), intent(in) :: var
        integer(int64), intent(in) :: delta
        type(status_type), intent(out) :: status
        type(sh_string_feature_type) :: before, after
        character(len=24) :: digits
        character(len=:), allocatable :: op

        call before%add("bash", "$(( ")
        call before%add("ksh88", "$(( ")
        call before%add("posix", "$(( ")
        call before%add("rc", "`{expr $")
        call before%add("sh", "`expr $")
        call before%add("zsh", "$[ ")
        call after%add("bash", " ))")
        call after%add("ksh88", " ))")
        call after%add("posix", " ))")
        call after%add("rc", "}")
        call after%add("sh", "`")
        call after%add("zsh", " ]")
        call sh_remember_variable(var)

        if (delta < 0) then
            op = " - "
        else
            op = " + "
        end if

        write (digits, "(i0)") abs(delta)
        call insert(before%get()//var//op//trim(digits)//after%get(), status)
    end subroutine sh_add

    ! `sh_add' as a command.
    recursive subroutine cmd_sh_add(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(sh_string_type), allocatable :: vars(:)
        type(sys_string_type), allocatable :: env(:)
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: var
        integer :: i, n, eq

        ! Do completion using `sh_shell_variables', but initialize it first
        ! (sh-add-completer).
        if (.not. variables_initialized()) call sh_shell_initialize_variables(status)
        env = emacs_environment()
        call sh_shell_variables(vars)
        n = size(env)
        allocate (names(n + size(vars)))

        do i = 1, n
            eq = index(env(i)%s, "=")

            if (eq > 0) then
                names(i) = make_string(env(i)%s(1:eq - 1))
            else
                names(i) = make_string(env(i)%s)
            end if
        end do

        do i = 1, size(vars)
            names(n + i) = make_string(vars(i)%s)
        end do

        table%list = make_list(names)
        call completing_read("Variable: ", table, var, status)
        if (.not. status%ok()) return
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call sh_add(var, prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_sh_add

    !> Remember preceding identifier for future completion and do self-insert.
    !> Port of sh-script.el:sh-assignment (obsolete since 27.1).
    recursive subroutine sh_assignment(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call self_insert_command(arg, int(iachar("="), int64), status)
        if (.not. status%ok()) return
        call sh_assignment_collect(status)
    end subroutine sh_assignment

    ! `sh_assignment' as a command.
    recursive subroutine cmd_sh_assignment(status)
        type(status_type), intent(out) :: status

        call sh_assignment(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_sh_assignment

    ! Port of sh-script.el:sh--assignment-collect.
    recursive subroutine sh_assignment_collect(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: re
        integer(int64) :: here, found

        status = status_type()
        if (cb(point()) /= iachar("=")) return
        here = point()
        re = sh_assignment_regexp%get()
        call beginning_of_line(1_int64)
        call re_search_forward(re, found, status, bound=here, noerror=SEARCH_NOERROR)
        if (found /= 0) call sh_remember_variable(match_string(1))
        call goto_char(here)
        status = status_type()
    end subroutine sh_assignment_collect

    ! Port of sh-script.el:sh--maybe-here-document.
    recursive subroutine sh_maybe_here_document(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        character(len=:), allocatable :: tabs, delim
        integer(int64) :: here, n
        integer :: i
        logical :: ok

        status = status_type()
        if (.not. looking_back("[^<]<<[ E-]", status, line_beginning_position())) return
        here = point()
        call goto_char(here - 2)
        ok = .not. (is_sh_quoted() .or. sh_inside_noncommand_expression(point()))
        call goto_char(here)
        if (.not. ok) return
        state = ppss()
        if (state%comstr_start /= 0) return
        tabs = ""

        if (len(sh_here_document_word) > 0) then
            if (sh_here_document_word(1:1) == "-") then
                n = current_indentation()/max(1_int64, tab_width%value)
                tabs = repeat(TAB, int(n))
            end if
        end if

        delim = ""

        do i = 1, len(sh_here_document_word)
            if (sh_here_document_word(i:i) /= "'" .and. sh_here_document_word(i:i) /= '"') then
                delim = delim//sh_here_document_word(i:i)
            end if
        end do

        ! If we're at <<-, we don't want to delete the previous char.
        if (preceding_char() /= iachar("-")) call delete_char(-1_int64, status)
        if (.not. status%ok()) return
        call insert(sh_here_document_word, status)
        if (.not. (is_eol() .or. looking_at("[ "//TAB//"]", status))) call insert(" ", status)
        call end_of_line(1_int64)

        do while (is_sh_quoted())
            call end_of_line(2_int64)
        end do

        call insert(LF//tabs, status)
        here = point()
        ! (replace-regexp-in-string "\\`-?[ \t]*" "" delim)
        i = 1

        if (len(delim) > 0) then
            if (delim(1:1) == "-") i = 2
        end if

        do while (i <= len(delim))
            if (delim(i:i) /= " " .and. delim(i:i) /= TAB) exit
            i = i + 1
        end do

        call insert(LF//tabs//delim(i:), status)
        call goto_char(here)
    end subroutine sh_maybe_here_document

    !> Make << insert a here document skeleton.  Port of
    !> sh-script.el:sh-electric-here-document-mode.
    recursive subroutine sh_electric_here_document_mode(status)
        type(status_type), intent(out) :: status

        ! Called as a command it toggles; from Fortran (a hook) it turns
        ! the mode on, as calling it with no argument does upstream.
        if (value_eq(this_command, sym("sh_electric_here_document_mode"))) then
            call minor_modes%sh_electric_here_document_mode%command(status)
        else
            call minor_modes%sh_electric_here_document_mode%enable(status)
        end if
    end subroutine sh_electric_here_document_mode

    ! The body of `sh_electric_here_document_mode'.  Not a port.
    recursive subroutine sh_electric_here_document_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (enable) then
            call post_self_insert_hook%add(sh_maybe_here_document, local=.true.)
        else
            call post_self_insert_hook%remove(sh_maybe_here_document, local=.true.)
        end if
    end subroutine sh_electric_here_document_mode_body

    !> Move point to successive beginnings of commands.  Port of
    !> sh-script.el:sh-beginning-of-command.
    subroutine cmd_sh_beginning_of_command(status)
        type(status_type), intent(out) :: status
        integer(int64) :: found

        ! FIXME: Redefine using SMIE.
        call re_search_backward(sh_beginning_of_command, found, status, noerror=SEARCH_NOERROR)
        if (found /= 0) call goto_char(match_beginning(2, status))
    end subroutine cmd_sh_beginning_of_command

    !> Move point to successive ends of commands.  Port of
    !> sh-script.el:sh-end-of-command.
    subroutine cmd_sh_end_of_command(status)
        type(status_type), intent(out) :: status
        integer(int64) :: found

        ! FIXME: Redefine using SMIE.
        call re_search_forward(sh_end_of_command, found, status, noerror=SEARCH_NOERROR)
        if (found /= 0) call goto_char(match_end(1, status))
    end subroutine cmd_sh_end_of_command

    !> Insert, align, or delete end-of-line backslashes on the lines in the region.
    !> With no argument, inserts backslashes and aligns existing backslashes.
    !> With an argument, deletes the backslashes.
    !>
    !> This function does not modify the last line of the region if the region ends
    !> right at the start of the following line; it does not modify blank lines
    !> at the start of the region.  So you can put the region around an entire
    !> shell command and conveniently use this command.  Port of
    !> sh-script.el:sh-backslash-region.
    subroutine sh_backslash_region(from, to, delete_flag, status)
        integer(int64), intent(in) :: from, to
        logical, intent(in) :: delete_flag
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: endmark
        integer(int64) :: here, column, moved, adjusted, tw, next

        status = status_type()
        here = point()
        call goto_char(from)
        column = sh_backslash_column
        call copy_marker_at(to, endmark, .false., status)

        ! Compute the smallest column number past the ends of all the lines.
        if (sh_backslash_align) then
            if (.not. delete_flag) then
                do while (point() < to)
                    call end_of_line()

                    if (preceding_char() == iachar("\")) then
                        call forward_char(status, -1_int64)
                        moved = skip_chars_backward(" "//TAB, status)
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
        call goto_char(from)

        do while (point() < mpos(endmark) .and. is_eol())
            moved = forward_line(1_int64)
        end do

        ! Add or remove backslashes on all the lines.
        do while (point() < mpos(endmark))
            ! Don't backslashify the last line
            ! if the region ends right at the start of the next line.
            next = point()
            moved = forward_line(1_int64)

            if (.not. (point() < mpos(endmark))) then
                call goto_char(next)
                exit
            end if

            call goto_char(next)

            if (.not. delete_flag) then
                call sh_append_backslash(column, status)
            else
                call sh_delete_backslash(status)
            end if

            moved = forward_line(1_int64)
        end do

        call free_marker(endmark, st)
        call goto_char(here)
    end subroutine sh_backslash_region

    ! `sh_backslash_region' as a command.
    subroutine cmd_sh_backslash_region(status)
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
        call sh_backslash_region(b, e,.not. nilp(current_prefix_arg), status)
        call goto_char(mpos(mark))
        call free_marker(mark, st)
    end subroutine cmd_sh_backslash_region

    ! The width of the selected window (window-width).  Not a port.
    integer function window_width()
        window_width = window_text_cols(frame_selected_window_ref())
    end function window_width

    ! Port of sh-script.el:sh-append-backslash.
    subroutine sh_append_backslash(column, status)
        integer(int64), intent(in) :: column
        type(status_type), intent(out) :: status
        integer(int64) :: minimum, col

        call end_of_line()
        minimum = 1
        if (sh_backslash_align) minimum = 0

        ! Note that "\\\\" is needed to get one backslash.
        if (preceding_char() == iachar("\")) then
            call forward_char(status, -1_int64)
            call delete_horizontal_space(.false., status)
            col = indent_to(column, status, minimum)
        else
            col = indent_to(column, status, minimum)
            call insert("\", status)
        end if
    end subroutine sh_append_backslash

    ! Port of sh-script.el:sh-delete-backslash.
    subroutine sh_delete_backslash(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved, here

        status = status_type()
        call end_of_line()
        if (is_bol()) return
        call forward_char(status, -1_int64)

        if (looking_at("\\", status)) then
            here = point() + 1
            moved = skip_chars_backward(" "//TAB, status)
            call delete_region(here, point(), status)
        end if
    end subroutine sh_delete_backslash

    !> Find the name of function or variable at point.
    !> For use in `add_log_current_subprogram_function' (unallocated for nil).
    !> Port of sh-script.el:sh-current-defun-name.
    function sh_current_function_name() result(name)
        character(len=:), allocatable :: name
        type(status_type) :: st
        integer(int64) :: here, found
        integer :: n
        logical :: matched

        here = point()
        call end_of_line()
        ! function FOO, function FOO(), FOO() and FOO=.
        call re_search_backward("\(?:"// &
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

        call goto_char(here)
    end function sh_current_function_name

    !> Make a regexp which matches WORD as a word.
    !> This specifically excludes an occurrence of WORD followed by
    !> punctuation characters like `-'.  Port of
    !> sh-script.el:sh-mkword-regexpr.
    pure function sh_mkword_regexpr(word) result(re)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: re

        re = word//"\([^-[:alnum:]_]\|$\)"
    end function sh_mkword_regexpr

    ! ------------------------------------------------------------------
    ! Inferior shell interaction
    ! ------------------------------------------------------------------

    !> Get a shell process for interaction.
    !> If FORCE is true and no process found, create one.  Port of
    !> sh-script.el:sh-shell-process.
    recursive subroutine sh_shell_process(force, proc, status)
        logical, intent(in) :: force
        type(handle_type), intent(out) :: proc
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: procs(:)
        type(process_arg_type), allocatable :: command(:)
        type(handle_type) :: saved_buffer, shell_buffer
        type(display_action_type) :: saved_action
        character(len=:), allocatable :: saved_file
        logical :: had_file
        integer :: k, i

        status = status_type()
        k = local_index(.true.)

        if (is_process_live(locals(k)%process)) then
            proc = locals(k)%process
            return
        end if

        proc = handle_type()
        procs = process_list()

        do i = 1, size(procs)
            if (.not. is_process_live(procs(i))) exit
            call process_command(procs(i), command)
            if (size(command) == 0) exit

            if (file_name_nondirectory(command(1)%s) == sh_shell%value) then
                proc = procs(i)
                exit
            end if
        end do

        if (proc%index == 0 .and. force) then
            ! (let ((explicit-shell-file-name sh-shell-file)
            !       (display-buffer-overriding-action
            !        '(nil . ((inhibit-same-window . t)))))
            !   (save-current-buffer (shell)))
            ! We must prevent this `(shell)' call from switching buffers,
            ! so that the variable `sh-shell-process' is set locally in
            ! the correct buffer.
            had_file = allocated(explicit_shell_file_name%value)
            if (had_file) saved_file = explicit_shell_file_name%value
            explicit_shell_file_name = sh_shell_file%value
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
            proc = get_buffer_process(shell_buffer)
        end if

        k = local_index(.true.)
        locals(k)%process = proc
    end subroutine sh_shell_process

    !> Pop the shell interaction buffer.  Port of sh-script.el:sh-show-shell.
    recursive subroutine sh_show_shell(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc

        call sh_shell_process(.true., proc, status)
        if (.not. status%ok()) return
        call pop_to_buffer(process_buffer(proc), status, [DISPLAY_ACTION_SAME_WINDOW])
    end subroutine sh_show_shell

    !> Send TEXT to `sh_shell_process'.  Port of sh-script.el:sh-send-text.
    recursive subroutine sh_send_text(text, status)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: proc

        call sh_shell_process(.true., proc, status)
        if (.not. status%ok()) return
        call comint_send_string(proc, text//LF, status)
    end subroutine sh_send_text

    !> Change directory in the current interaction shell to the current one.
    !> Port of sh-script.el:sh-cd-here.
    recursive subroutine sh_cd_here(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir

        dir = "./"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call sh_send_text("cd "//dir, status)
    end subroutine sh_cd_here

    !> Send the current line to the inferior shell and step to the next line.
    !> When the region is active, send the region instead.  Port of
    !> sh-script.el:sh-send-line-or-region-and-step.
    recursive subroutine sh_send_line_or_region_and_step(status)
        use editfns_fm, only: region_beginning, region_end
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
        call sh_send_text(text, status)
        if (.not. status%ok()) return
        call goto_char(min(end, point_max()))
    end subroutine sh_send_line_or_region_and_step

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    !> The variables assigned before point.  Port of
    !> sh-script.el:sh--vars-before-point.
    function sh_vars_before_point() result(vars)
        type(value_type) :: vars
        type(value_type), allocatable :: items(:), grown(:)
        integer(int64) :: opoint, found
        type(status_type) :: st
        integer :: n

        opoint = point()
        allocate (items(0))

        do
            call re_search_backward("^[ "//TAB//"]*\([[:alnum:]_]+\)=", found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            n = size(items)
            allocate (grown(n + 1))
            grown(1) = make_string(match_string(1))
            grown(2:) = items
            call move_alloc(grown, items)
        end do

        call goto_char(opoint)
        vars = make_list(items)
    end function sh_vars_before_point

    ! The directories of `exec_path' (PATH; an empty entry is the default
    ! directory, given as an empty name).
    ! Not a port.
    subroutine exec_path_dirs(dirs)
        character(len=:), allocatable, intent(out) :: dirs(:)
        character(len=:), allocatable :: path
        logical :: found
        integer :: b, e, n, longest, k

        call getenv("PATH", path, found)

        if (.not. found) then
            allocate (character(len=1) :: dirs(0))
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
        allocate (character(len=longest) :: dirs(n))
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
            dirs(n) = path(b:e - 1)
            b = e + 1
        end do
    end subroutine exec_path_dirs

    !> The candidates of `sh_completion_at_point_function' for STRING
    !> before a keyword position: the variables assigned before point
    !> (with "="), the executables and the keywords.  Port of
    !> sh-script.el:sh--cmd-completion-table-gen (Imenu is not loaded).
    recursive function sh_cmd_completion_table_gen(string) result(candidates)
        character(len=*), intent(in) :: string
        type(value_type) :: candidates
        type(value_type), allocatable :: items(:)
        type(value_type) :: vars, files
        character(len=:), allocatable :: dirs(:)
        type(status_type) :: st
        integer(int64) :: k, nv, nf
        integer :: j

        vars = sh_vars_before_point()
        call exec_path_dirs(dirs)
        call locate_file_completion_table(dirs, [""], string, COMPLETE_ALL, files, st)
        nv = list_length(vars)
        nf = list_length(files)
        allocate (items(nv + nf + size(sh_completion_keywords)))

        do k = 1, nv
            items(k) = make_string(xstring(list_elt(vars, k))//"=")
        end do

        do k = 1, nf
            items(nv + k) = list_elt(files, k)
        end do

        do j = 1, size(sh_completion_keywords)
            items(nv + nf + j) = make_string(trim(sh_completion_keywords(j)))
        end do

        candidates = make_list(items)
    end function sh_cmd_completion_table_gen

    ! The completion table of commands (completion-table-with-cache of
    ! `sh_cmd_completion_table_gen').
    ! Port of sh-script.el:sh-completion-at-point-function (in part: its table).
    recursive subroutine sh_cmd_completion_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        logical :: cached

        status = status_type()

        if (action == COMPLETE_BOUNDARIES) then
            result = nil()
            return
        end if

        cached = .false.

        if (allocated(cmd_cache_arg)) then
            if (len(cmd_cache_arg) <= len(string)) cached = string(1:len(cmd_cache_arg)) == cmd_cache_arg
        end if

        if (.not. cached) then
            cmd_cache_result = sh_cmd_completion_table_gen(string)
            cmd_cache_arg = string
        end if

        table%list = cmd_cache_result
        call complete_with_action(action, table, string, result, status, pred, suffix)
    end subroutine sh_cmd_completion_table

    !> The completion data of the variable or command name at point (a
    !> function of `completion_at_point_functions').  Port of
    !> sh-script.el:sh-completion-at-point-function.
    recursive subroutine sh_completion_at_point_function(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, start, end, moved

        status = status_type()
        opoint = point()
        moved = skip_chars_forward("[:alnum:]_", status)
        end = point()
        moved = skip_chars_backward("[:alnum:]_", status)
        start = point()

        if (preceding_char() == iachar("$")) then
            data%found = .true.
            data%start = start
            data%end = end
            data%table%list = sh_vars_before_point()
        else if (is_sh_smie_keyword()) then
            data%found = .true.
            data%start = start
            data%end = end
            data%table%kind = TABLE_FUNCTION
            data%table%fn => sh_cmd_completion_table
            if (allocated(cmd_cache_arg)) deallocate (cmd_cache_arg)
        end if

        call goto_char(opoint)
    end subroutine sh_completion_at_point_function

    ! ------------------------------------------------------------------
    ! Styles
    ! ------------------------------------------------------------------

    ! The values of the indentation variables (sh-var-list).
    ! Not a port.
    function current_style_values() result(values)
        integer :: values(15)

        values = [int(sh_basic_offset%value), int(sh_indent_after_case%value), int(sh_indent_after_do%value), &
                  int(sh_indent_after_done%value), int(sh_indent_after_else%value), int(sh_indent_after_if%value), &
                  int(sh_indent_after_loop_construct%value), int(sh_indent_after_open%value), &
                  int(sh_indent_for_case_alt%value), int(sh_indent_for_case_label%value), &
                  int(sh_indent_for_continuation%value), int(sh_indent_for_do%value), int(sh_indent_for_done%value), &
                  int(sh_indent_for_fi%value), int(sh_indent_for_then%value)]
    end function current_style_values

    ! The index of the style NAME in `sh_styles_table', 0 if none.
    ! Not a port.
    integer function style_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, size(sh_styles_table)
            if (trim(sh_styles_table(k)%name) == name) return
        end do

        k = 0
    end function style_index

    !> Name the current indentation settings as a style called NAME.
    !> If this name exists, the command will prompt whether it should be
    !> overwritten if
    !> - - it was called interactively with a prefix argument, or
    !> - - called non-interactively with optional CONFIRM_OVERWRITE true.
    !> Port of sh-script.el:sh-name-style.
    recursive subroutine sh_name_style(name, status, confirm_overwrite)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: confirm_overwrite
        type(sh_style_type), allocatable :: grown(:)
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
                sh_styles_table(k)%values = current_style_values()
            end if
        else
            call message("Creating new style "//name)
            allocate (grown(size(sh_styles_table) + 1))
            grown(1)%name = name
            grown(1)%values = current_style_values()
            grown(2:) = sh_styles_table
            call move_alloc(grown, sh_styles_table)
        end if
    end subroutine sh_name_style

    !> Name the current indentation settings as a style (the command).
    !> Port of sh-script.el:sh-name-style.
    recursive subroutine cmd_sh_name_style(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_from_minibuffer("Name for this style? ", name, status)
        if (.not. status%ok()) return
        call sh_name_style(name, status, nilp(current_prefix_arg))
    end subroutine cmd_sh_name_style

    !> Set shell indentation values for this buffer from those in style NAME.
    !> Port of sh-script.el:sh-load-style.
    recursive subroutine sh_load_style(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = style_index(name)

        if (k == 0) then
            call set_error(status, FM_GENERIC_ERROR, "sh_load_style: Style "//name//" not known")
            return
        end if

        associate (v => sh_styles_table(k)%values)
            sh_basic_offset = v(1)
            sh_indent_after_case = v(2)
            sh_indent_after_do = v(3)
            sh_indent_after_done = v(4)
            sh_indent_after_else = v(5)
            sh_indent_after_if = v(6)
            sh_indent_after_loop_construct = v(7)
            sh_indent_after_open = v(8)
            sh_indent_for_case_alt = v(9)
            sh_indent_for_case_label = v(10)
            sh_indent_for_continuation = v(11)
            sh_indent_for_do = v(12)
            sh_indent_for_done = v(13)
            sh_indent_for_fi = v(14)
            sh_indent_for_then = v(15)
        end associate
    end subroutine sh_load_style

    !> Set shell indentation values for this buffer from those in a style
    !> (the command).  Port of sh-script.el:sh-load-style.
    recursive subroutine cmd_sh_load_style(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: name
        integer :: k

        allocate (names(size(sh_styles_table)))

        do k = 1, size(names)
            names(k) = make_string(trim(sh_styles_table(k)%name))
        end do

        table%list = make_list(names)
        call completing_read("Which style to use for this buffer? ", table, name, status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call sh_load_style(name, status)
    end subroutine cmd_sh_load_style

    !> Save all current styles as Fortran to buffer BUFF.
    !> This is always added to the end of the buffer.  Port of
    !> sh-script.el:sh-save-styles-to-buffer (an assignment of
    !> `sh_styles_table', for init.f90, in place of a setq).
    recursive subroutine sh_save_styles_to_buffer(buff, status)
        character(len=*), intent(in) :: buff
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, saved
        character(len=:), allocatable :: text
        integer :: k, j

        call get_buffer_create(buff, buffer, status)
        if (.not. status%ok()) return
        saved = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return

        if (size(sh_styles_table) == 0) then
            text = "sh_styles_table = [sh_style_type ::]"//LF
        else
            text = "sh_styles_table = ["

            do k = 1, size(sh_styles_table)
                if (k > 1) text = text//", &"//LF//"                   "
                text = text//"sh_style_type(name="""//trim(sh_styles_table(k)%name)//""", values=["

                do j = 1, size(sh_styles_table(k)%values)
                    if (j > 1) text = text//", "
                    text = text//value_text(sh_styles_table(k)%values(j))
                end do

                text = text//"])"
            end do

            text = text//"]"//LF
        end if

        call goto_char(point_max())
        call insert(LF//text, status)
        call set_buffer(saved, status)

    contains

        ! An indentation value as Fortran: an integer or a named constant.
        function value_text(v) result(t)
            integer, intent(in) :: v
            character(len=:), allocatable :: t
            character(len=16) :: buf

            select case (v)
            case (SH_PLUS)
                t = "SH_PLUS"
            case (SH_MINUS)
                t = "SH_MINUS"
            case (SH_PLUS_PLUS)
                t = "SH_PLUS_PLUS"
            case (SH_MINUS_MINUS)
                t = "SH_MINUS_MINUS"
            case (SH_STAR)
                t = "SH_STAR"
            case (SH_SLASH)
                t = "SH_SLASH"
            case default
                write (buf, "(i0)") v
                t = trim(buf)
            end select
        end function value_text
    end subroutine sh_save_styles_to_buffer

    !> Save all current styles to a buffer (the command).  Port of
    !> sh-script.el:sh-save-styles-to-buffer.
    recursive subroutine cmd_sh_save_styles_to_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: buff

        call read_from_minibuffer("Buffer to save styles in? ", buff, status, initial="*scratch*")
        if (.not. status%ok()) return
        call sh_save_styles_to_buffer(buff, status)
    end subroutine cmd_sh_save_styles_to_buffer
end module sh_script_fm
