! make_mode_mod.f90 -- makefile editing commands for Fortmacs.
! Ported from GNU Emacs 31.1 lisp/progmodes/make-mode.el.
!
! Copyright (C) 1992-2026 Free Software Foundation, Inc.
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

!> Makefile mode: `makefile_mode' and its variants `makefile_automake_mode',
!> `makefile_gnu_make_mode', `makefile_makepp_mode', `makefile_bsdmake_mode'
!> and `makefile_imake_mode', with dependency motion (M-n, M-p), target and
!> macro pickup and insertion (C-c C-p, C-c C-f, C-c :, the electric keys
!> of `makefile_electric_keys'), completion at point, GNU make function
!> calls (C-c TAB), backslash alignment (C-c C-\), filling, the up-to-date
!> overview (C-c C-u) and the checks run when saving
!> (`write_file_functions').
!>
!> The tables of known targets and macros are buffer-local in Makefile
!> mode buffers (`makefile_target_table', `makefile_macro_table' read
!> them).  The keyword lists are arrays of `font_lock_keyword_type';
!> `makefile_gnumake_functions' is an array of
!> `makefile_gnumake_function_type'.
!>
!> Deferred: the obsolete macro and target browser (C-c C-b,
!> `makefile_switch_to_browser' and the `makefile_browser_*' commands and
!> options, obsolete since 30.1), the Makefile menu, `c_preprocessor_font_lock_keywords' in
!> `makefile_imake_font_lock_keywords' (font-lock.el's C preprocessor
!> keywords), and the obsolete `makefile_generate_temporary_filename'.
module make_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type, set_error, WRONG_TYPE_ARGUMENT_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, xfixnum, xstring, make_list, list, list_length, list_element, vtag, V_INTEGER
    use utility_functions_mod, only: is_string_less
    use data_mod, only: integer_setting_type, logical_setting_type, string_setting_type, forward_setting, &
                        append_locals_clearer
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, execute_interactively
    use keyboard_mod, only: last_command_event
    use buffer_mod, only: buffers, current, buffer_serial_live, barf_if_buffer_read_only, get_buffer, get_or_make_buffer, &
                          set_buffer, current_buffer, erase_buffer, close_buffer
    use hooks_mod, only: hook_type
    use modes_mod, only: major_modes
    use add_log_mod, only: add_log_current_subprogram_function
    use derived_mod, only: is_derived_mode
    use keymap_mod, only: keymap_set
    use syntax_mod, only: make_syntax_table, modify_syntax_entry, is_syntax_table_live, skip_characters_forward, &
                          skip_characters_backward
    use editing_functions_mod, only: interactive_region, point, point_minimum, point_maximum, goto_character, &
                                     is_beginning_of_line, is_end_of_line, is_beginning_of_buffer, is_end_of_buffer, &
                                     character_after, character_before, preceding_character, buffer_substring, &
                                     line_beginning_position, line_end_position, delete_region, narrow_to_region, widen, &
                                     buffer_size
    use editing_commands_mod, only: forward_line, forward_character, beginning_of_line, end_of_line, self_insert_command
    use insert_delete_mod, only: insert
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use indent_mod, only: current_column, indent_to, indent_line_function, indent_to_left_margin, indent_tabs_mode
    use search_mod, only: looking_at, regular_expression_search_forward, string_match, match_beginning, match_end, match_data, &
                          set_match_data, regular_expression_quote, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string_no_properties, read_y_or_n
    use simple_mod, only: delete_horizontal_space, count_lines, line_number_at_position
    use text_properties_mod, only: get_text_property, remove_text_properties
    use display_engine_mod, only: message, tab_width
    use window_mod, only: window_text_columns, pop_to_buffer, shrink_window_if_larger_than_buffer
    use frame_mod, only: frame_selected_window_reference
    use buffer_mod, only: fill_column
    use fill_mod, only: fill_paragraph, fill_paragraph_function
    use files_mod, only: write_file_functions, delete_file
    use file_input_output_mod, only: is_file_directory, file_name_directory, write_region, make_temporary_name
    use directory_editor_mod, only: file_name_all_completions
    use execute_process_mod, only: execute_process
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                             font_lock_syntax_entry_type, FONT_LOCK_OVERRIDE_REPLACE, FONT_LOCK_OVERRIDE_PREPEND, &
                             FONT_LOCK_OVERRIDE_APPEND
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use regular_expression_optimization_mod, only: regular_expression_optimization, REGULAR_EXPRESSION_OPTIMIZATION_GROUP
    use newcomment_mod, only: comment_start, comment_end, comment_start_skip
    use abbreviation_mod, only: abbreviation_table_type
    use dynamic_abbreviation_mod, only: dynamic_abbreviation_skip_leading_regular_expression
    use faces_mod, only: named_face_type, display_when, face_attributes, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                         FACE_BACKGROUND_DARK
    use minibuffer_reading_mod, only: completion_table_type, read_string
    use minibuffer_reading_tables_mod, only: completion_at_point_function_data_type, COMPLETION_SOLE, COMPLETION_FINISHED
    use minibuffer_mod, only: completing_read, completion_at_point_functions
    use syntax_propertize_mod, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type
    use index_menu_mod, only: index_menu_patterns, index_menu_pattern_type
    use string_type_mod, only: string_type, prepend_string
    implicit none(type, external)
    private

    public :: initialize_make_mode, makefile_mode, makefile_automake_mode, makefile_gnu_make_mode, makefile_makepp_mode, &
              makefile_bsdmake_mode, makefile_imake_mode, makefile_next_dependency, makefile_previous_dependency, &
              makefile_electric_dot, makefile_insert_special_target, makefile_electric_equal, makefile_insert_macro, &
              makefile_insert_macro_reference, makefile_insert_target, makefile_insert_target_reference, makefile_electric_colon, &
              makefile_pickup_targets, makefile_pickup_macros, makefile_pickup_everything, &
              makefile_pickup_filenames_as_targets, makefile_completions_at_point, makefile_backslash_region, &
              makefile_fill_paragraph, makefile_create_up_to_date_overview, makefile_query_by_make_minus_q, &
              makefile_cleanup_continuations_function, makefile_warn_suspicious_lines, makefile_warn_continuations, &
              makefile_insert_gnu_make_function, makefile_match_function_end, makefile_match_dependency, &
              makefile_match_action, makefile_do_macro_insertion, makefile_remember_target, makefile_remember_macro, &
              makefile_format_macro_reference, makefile_add_log_target, makefile_syntax_propertize_function, &
              makefile_make_font_lock_keywords, makefile_target_table, makefile_macro_table, makefile_has_prereqs, &
              makefile_query_procedure

    !> Face to use for highlighting leading spaces in Font-Lock mode.
    !> Port of make-mode.el:makefile-space.
    type(named_face_type), target, public :: makefile_space

    !> Face to use for additionally highlighting rule targets in Font-Lock mode.
    !> Port of make-mode.el:makefile-targets.
    type(named_face_type), target, public :: makefile_targets

    !> Face to use for additionally highlighting Shell commands in Font-Lock mode.
    !> Port of make-mode.el:makefile-shell.
    type(named_face_type), target, public :: makefile_shell

    !> Face to use for additionally highlighting Perl code in Font-Lock mode.
    !> Port of make-mode.el:makefile-makepp-perl.
    type(named_face_type), target, public :: makefile_makepp_perl

    !> String to append to all target names inserted by `makefile_insert_target'.
    !> ":" or "::" are common values.  Port of make-mode.el
    !> makefile-target-colon.
    type(string_setting_type), target, public :: makefile_target_colon = string_setting_type(name="makefile_target_colon")

    !> String to append to all macro names inserted by `makefile_insert_macro'.
    !> The normal value should be " = ", since this is what
    !> standard make expects.  However, newer makes such as dmake
    !> allow a larger variety of different macro assignments, so you
    !> might prefer to use " += " or " := " .  Port of make-mode.el
    !> makefile-macro-assign.
    type(string_setting_type), target, public :: makefile_macro_assign = string_setting_type(name="makefile_macro_assign")

    !> If true, Makefile mode should install electric keybindings.
    !> Default is .false..  Port of make-mode.el makefile-electric-keys.
    type(logical_setting_type), target, public :: makefile_electric_keys = &
                                                  logical_setting_type(name="makefile_electric_keys", value=.false.)

    !> Controls the style of generated macro references.
    !> True means macro references should use curly braces, like `${this}'.
    !> .false. means use parentheses, like `$(this)'.  Port of make-mode.el
    !> makefile-use-curly-braces-for-macros-p.
    type(logical_setting_type), target, public :: makefile_use_curly_braces_for_macros = &
                                                  logical_setting_type(name="makefile_use_curly_braces_for_macros", value=.false.)

    !> If true, insert a TAB after a target colon.
    !> Otherwise, a space is inserted.
    !> The default is .true..  Port of make-mode.el
    !> makefile-tab-after-target-colon.
    type(logical_setting_type), target, public :: makefile_tab_after_target_colon = &
                                                  logical_setting_type(name="makefile_tab_after_target_colon", value=.true.)

    !> Column in which `makefile_backslash_region' inserts backslashes.
    !> Port of make-mode.el makefile-backslash-column.
    type(integer_setting_type), target, public :: makefile_backslash_column = &
                                                  integer_setting_type(name="makefile_backslash_column", value=48)

    !> If true, `makefile_backslash_region' will align backslashes.
    !> Port of make-mode.el makefile-backslash-align.
    type(logical_setting_type), target, public :: makefile_backslash_align = &
                                                  logical_setting_type(name="makefile_backslash_align", value=.true.)

    !> If true, `makefile_pickup_everything' picks up filenames as targets.
    !> This means it calls `makefile_pickup_filenames_as_targets'.
    !> Otherwise filenames are omitted.  Port of make-mode.el
    !> makefile-pickup-everything-picks-up-filenames-p.
    type(logical_setting_type), target, public :: makefile_pickup_everything_picks_up_filenames = &
                                                  logical_setting_type(name="makefile_pickup_everything_picks_up_filenames", &
                                                                       value=.false.)

    !> If true, automatically clean up continuation lines when saving.
    !> A line is cleaned up by removing all whitespace following a trailing
    !> backslash.  This is done silently.
    !> IMPORTANT: Please note that enabling this option causes Makefile mode
    !> to MODIFY A FILE WITHOUT YOUR CONFIRMATION when "it seems necessary".
    !> Port of make-mode.el makefile-cleanup-continuations.
    type(logical_setting_type), target, public :: makefile_cleanup_continuations = &
                                                  logical_setting_type(name="makefile_cleanup_continuations", value=.false.)

    !> Normal hook run by `makefile_mode'.  Port of make-mode.el
    !> makefile-mode-hook.
    type(hook_type), target, public :: makefile_mode_hook = hook_type(name="makefile_mode_hook")
    !> Normal hook run by `makefile_automake_mode'.
    type(hook_type), target, public :: makefile_automake_mode_hook = hook_type(name="makefile_automake_mode_hook")
    !> Normal hook run by `makefile_gnu_make_mode'.
    type(hook_type), target, public :: makefile_gnu_make_mode_hook = hook_type(name="makefile_gnu_make_mode_hook")
    !> Normal hook run by `makefile_makepp_mode'.
    type(hook_type), target, public :: makefile_makepp_mode_hook = hook_type(name="makefile_makepp_mode_hook")
    !> Normal hook run by `makefile_bsdmake_mode'.
    type(hook_type), target, public :: makefile_bsdmake_mode_hook = hook_type(name="makefile_bsdmake_mode_hook")
    !> Normal hook run by `makefile_imake_mode'.
    type(hook_type), target, public :: makefile_imake_mode_hook = hook_type(name="makefile_imake_mode_hook")

    !> List of special targets.
    !> You will be offered to complete on one of those in the minibuffer whenever
    !> you enter a "." at the beginning of a line in `makefile_mode'.
    !> Port of make-mode.el makefile-special-targets-list.
    character(len=16), allocatable, public :: makefile_special_targets(:)

    !> List of macros that are resolved by make at runtime.
    !> If you insert a macro reference using `makefile_insert_macro_reference', the name
    !> of the macro is checked against this list.  If it can be found its name will
    !> not be enclosed in { } or ( ).  Port of make-mode.el
    !> makefile-runtime-macros-list.
    character(len=8), allocatable, public :: makefile_runtime_macros(:)

    !> Regex used to find dependency lines in a makefile.  Port of
    !> make-mode.el makefile-dependency-regex.
    type(string_setting_type), target, public :: makefile_dependency_regular_expression = string_setting_type( &
                                                 name="makefile_dependency_regular_expression")

    !> Regex used to find dependency lines in a BSD makefile.  Port of
    !> make-mode.el makefile-bsdmake-dependency-regex.
    character(len=:), allocatable, public :: makefile_bsdmake_dependency_regular_expression

    !> Characters to skip to find a line that might be a dependency.
    !> Port of make-mode.el makefile-dependency-skip.
    type(string_setting_type), target, public :: makefile_dependency_skip = string_setting_type(name="makefile_dependency_skip")

    !> Regex used to highlight rule action lines in font lock mode.
    !> Port of make-mode.el makefile-rule-action-regex.
    type(string_setting_type), target, public :: makefile_rule_action_regular_expression = &
                                                 string_setting_type(name="makefile_rule_action_regular_expression")

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> Regex used to highlight makepp rule action lines in font lock mode.
    !> Port of make-mode.el makefile-makepp-rule-action-regex.
    character(len=*), parameter, public :: makefile_makepp_rule_action_regular_expression = &
                                           "^"//TAB//"[ "//TAB//"]*\(\(?:\(?:noecho\|ignore[-_]error\|[-@]+\)[ "//TAB// &
                                           "]*\)*\)\(\(&\S +\)?\(?:.*\\"//LF//"\)*.*\)"

    !> Regex used to highlight BSD rule action lines in font lock mode.
    !> Port of make-mode.el makefile-bsdmake-rule-action-regex.
    character(len=:), allocatable, public :: makefile_bsdmake_rule_action_regular_expression

    !> Imenu generic expression for Makefile mode.  See
    !> `index_menu_patterns'.  Port of make-mode.el
    !> makefile-imenu-generic-expression.
    type(index_menu_pattern_type), allocatable, public :: makefile_index_menu_patterns(:)

    !> Regex used to find macro assignment lines in a makefile.  Port of
    !> make-mode.el makefile-macroassign-regex.
    character(len=*), parameter, public :: makefile_macroassign_regular_expression = &
                                           "\(?:^\|^export\|^override\|:\|:[ "//TAB//"]*override\)[ "//TAB//"]*\([^ "//LF// &
                                           TAB//"][^:#= "//TAB//LF//"]*\)[ "//TAB//"]*\(?:!=\|[*:+]?[:?]?=\)"

    !> Regex used to find $(macro) uses in a makefile.  Port of
    !> make-mode.el makefile-var-use-regex.
    character(len=*), parameter, public :: makefile_variable_use_regular_expression = &
                                           "\(^\|[^$]\)\$[({]\([-a-zA-Z0-9_.]+\|[@%<?^+*][FD]?\)"

    !> Regex for filenames that will NOT be included in the target list.
    !> Port of make-mode.el makefile-ignored-files-in-pickup-regex.
    character(len=*), parameter, public :: makefile_ignored_files_in_pickup_regular_expression = &
                                           "\(^\..*\)\|\(.*~$\)\|\(.*,v$\)\|\(\.[chy]\)"

    !> List of keywords understood by standard make.  Port of make-mode.el
    !> makefile-statements.
    character(len=*), parameter, public :: makefile_statements(*) = [character(len=7) :: "include"]

    !> List of keywords understood by automake.  Port of make-mode.el
    !> makefile-automake-statements.
    character(len=*), parameter, public :: makefile_automake_statements(*) = &
                                           [character(len=7) :: "if", "else", "endif", "include"]

    !> List of keywords understood by gmake.  Port of make-mode.el
    !> makefile-gmake-statements.
    character(len=*), parameter, public :: makefile_gnu_make_statements(*) = &
                                           [character(len=15) :: "-sinclude", "sinclude", "ifdef", "ifndef", "ifeq", "ifneq", &
                                                           "-include", "define", "endef", "export", "override define", "override", &
                                                                  "unexport", "vpath", "undefine", "else", "endif", "include"]

    !> List of keywords understood by makepp (upstream's leading `t': a
    !> `-' in them alternately means `_').  Port of make-mode.el
    !> makefile-makepp-statements.
    character(len=*), parameter, public :: makefile_makepp_statements(*) = [character(len=25) :: &
                                                                   "ifperl", "ifmakeperl", "ifsys", "ifnsys", "iftrue", "ifntrue", &
                                                                 "and ifdef", "and ifndef", "and ifeq", "and ifneq", "and ifperl", &
                                                         "and ifmakeperl", "and ifsys", "and ifnsys", "and iftrue", "and ifntrue", &
                                                            "else ifdef", "else ifndef", "else ifeq", "else ifneq", "else ifperl", &
                                                    "else ifmakeperl", "else ifsys", "else ifnsys", "else iftrue", "else ifntrue", &
                                                                      "or ifdef", "or ifndef", "or ifeq", "or ifneq", "or ifperl", &
                                                              "or ifmakeperl", "or ifsys", "or ifnsys", "or iftrue", "or ifntrue", &
                                                              "autoload", "build-cache", "build-check", "enddef", "export define", &
                                                            "global", "global build-cache", "global build-check", "global define", &
                                                                 "global signature", "global override signature", "load-makefile", &
                                                          "make", "makeperl", "makesub", "no-implicit-load", "perl", "perl-begin", &
                                                  "perl-end", "prebuild", "override export", "override global", "register-parser", &
                                                                            "register-command-parser", "register-input-suffix", &
                                                                  "register-scanner", "repository", "runtime", "signature", "sub", &
                                                      "ifdef", "ifndef", "ifeq", "ifneq", "-include", "define", "endef", "export", &
                                                  "override define", "override", "unexport", "vpath", "undefine", "else", "endif", &
                                                                            "include"]

    !> List of keywords understood by BSD make.  Port of make-mode.el
    !> makefile-bsdmake-statements.
    character(len=*), parameter, public :: makefile_bsdmake_statements(*) = &
                                           [character(len=9) :: "elif", "elifdef", "elifmake", "elifndef", "elifnmake", &
                                                   "else", "endfor", "endif", "for", "if", "ifdef", "ifmake", "ifndef", "ifnmake", &
                                                                 "poison", "undef", "include"]

    !> Keywords to highlight in Makefile mode.  Port of make-mode.el
    !> makefile-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: makefile_font_lock_keywords(:)
    !> Keywords to highlight in Makefile.am mode.  Port of make-mode.el
    !> makefile-automake-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: makefile_automake_font_lock_keywords(:)
    !> Keywords to highlight in GNUmakefile mode.  Port of make-mode.el
    !> makefile-gmake-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: makefile_gnu_make_font_lock_keywords(:)
    !> Keywords to highlight in Makeppfile mode.  Port of make-mode.el
    !> makefile-makepp-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: makefile_makepp_font_lock_keywords(:)
    !> Keywords to highlight in BSDmakefile mode.  Port of make-mode.el
    !> makefile-bsdmake-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: makefile_bsdmake_font_lock_keywords(:)
    !> Keywords to highlight in Imakefile mode.  Port of make-mode.el
    !> makefile-imake-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: makefile_imake_font_lock_keywords(:)

    !> Abstract interface of `makefile_query_one_target_method_function'.
    abstract interface
        !> Whether TARGET of makefile FILENAME needs rebuilding (.false. when up to date).
        logical function makefile_query_procedure(target, filename)
            implicit none(type, external)
            character(len=*), intent(in) :: target, filename
        end function makefile_query_procedure
    end interface

    !> How to invoke make, for `makefile_query_targets'.
    !> This should identify a `make' command that can handle the `-q' option.
    !> Port of make-mode.el makefile-brave-make.
    type(string_setting_type), target, public :: makefile_brave_make = string_setting_type(name="makefile_brave_make")

    !> Function to call to determine whether a make target is up to date.
    !> The function must satisfy this calling convention:
    !>
    !> * As its first argument, it must accept the name of the target to
    !>   be checked, as a string.
    !>
    !> * As its second argument, it may accept the name of a makefile
    !>   as a string.  Depending on what you're going to do you may
    !>   not need this.
    !>
    !> * It must return .false. if the given target should be considered
    !>   up-to-date in the context of the given makefile, .true. otherwise.
    !>
    !> Port of make-mode.el makefile-query-one-target-method-function.
    procedure(makefile_query_procedure), pointer, public :: makefile_query_one_target_method_function => null()

    !> Name of the Up-to-date overview buffer.  Port of make-mode.el
    !> makefile-up-to-date-buffer-name.
    type(string_setting_type), target, public :: makefile_up_to_date_buffer_name = &
                                                 string_setting_type(name="makefile_up_to_date_buffer_name")

    !> Abbrev table in use in Makefile buffers.  Port of make-mode.el
    !> makefile-mode-abbrev-table.
    type(abbreviation_table_type), target, public :: makefile_mode_abbreviation_table

    !> The keymap that is used in Makefile mode.  Port of make-mode.el
    !> makefile-mode-map.
    type(handle_type), public :: makefile_mode_map

    !> Syntax table used in `makefile_mode'.  Port of make-mode.el
    !> makefile-mode-syntax-table.
    type(handle_type), public :: makefile_mode_syntax_table
    !> Syntax table used in `makefile_imake_mode'.  Port of make-mode.el
    !> makefile-imake-mode-syntax-table.
    type(handle_type), public :: makefile_imake_mode_syntax_table

    !> A GNU make function of `makefile_gnumake_functions': its NAME and
    !> the PROMPTS for its required arguments (blank ones are unused).
    type, public :: makefile_gnumake_function_type
        character(len=10) :: name = ""
        character(len=11) :: prompts(6) = ""
    end type makefile_gnumake_function_type

    !> Table of GNU Make functions and their arguments.  Port of
    !> make-mode.el makefile-gnumake-functions-alist.
    type(makefile_gnumake_function_type), allocatable, public :: makefile_gnumake_functions(:)

    ! The tables of known targets and macros (makefile-target-table,
    ! makefile-macro-table, newest first), makefile-has-prereqs and
    ! whether they must be picked up again (makefile-need-target-pickup,
    ! makefile-need-macro-pickup): the default values, and Makefile mode
    ! buffers' own.
    type :: make_state_type
        integer(int64) :: serial = 0
        type(string_type), allocatable :: targets(:), macros(:), prereqs(:)
        logical :: need_target_pickup = .true., need_macro_pickup = .true.
    end type make_state_type

    type(make_state_type), target :: default_state
    type(make_state_type), allocatable, target :: locals(:)

    ! Whether make-mode.el has been loaded (the first Makefile mode).
    logical :: loaded = .false.

    ! The rules of `makefile_syntax_propertize_function'.
    type(syntax_propertize_rules_type) :: propertize_rules

    ! The suffix inserted by the exit function of
    ! `makefile_completions_at_point' (its closure).
    character(len=:), allocatable :: completion_suffix

contains

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the Makefile modes, their keymap, syntax tables and keywords.
    !> Not a port.
    subroutine initialize_make_mode()
        type(status_type) :: st

        if (is_syntax_table_live(makefile_mode_syntax_table)) return
        call forward_setting(makefile_target_colon)
        call forward_setting(makefile_macro_assign)
        call forward_setting(makefile_electric_keys)
        call forward_setting(makefile_use_curly_braces_for_macros)
        call forward_setting(makefile_tab_after_target_colon)
        call forward_setting(makefile_backslash_column)
        call forward_setting(makefile_backslash_align)
        call forward_setting(makefile_pickup_everything_picks_up_filenames)
        call forward_setting(makefile_cleanup_continuations)
        call forward_setting(makefile_dependency_regular_expression)
        call forward_setting(makefile_dependency_skip)
        call forward_setting(makefile_rule_action_regular_expression)
        call forward_setting(makefile_brave_make)
        call forward_setting(makefile_up_to_date_buffer_name)
        if (.not. allocated(makefile_target_colon%value)) makefile_target_colon = ":"
        if (.not. allocated(makefile_macro_assign%value)) makefile_macro_assign = " = "
        if (.not. allocated(makefile_dependency_skip%value)) makefile_dependency_skip = "^:"
        if (.not. allocated(makefile_brave_make%value)) makefile_brave_make = "make"

        if (.not. allocated(makefile_up_to_date_buffer_name%value)) then
            makefile_up_to_date_buffer_name = "*Makefile Up-to-date overview*"
        end if

        if (.not. associated(makefile_query_one_target_method_function)) then
            makefile_query_one_target_method_function => makefile_query_by_make_minus_q
        end if

        if (.not. allocated(makefile_rule_action_regular_expression%value)) then
            makefile_rule_action_regular_expression = "^"//TAB//"[ "//TAB//"]*\(?:\([-@]+\)[ "//TAB//"]*\)\(.*\(?:\\"//LF//".*\)*\)"
        end if

        makefile_bsdmake_rule_action_regular_expression = string_replace("-@", "-+@", makefile_rule_action_regular_expression%value)

        if (.not. allocated(makefile_dependency_regular_expression%value)) then
            ! Allow for two nested levels $(v1:$(v2:$(v3:a=b)=c)=d)
            makefile_dependency_regular_expression = "^\("//elements_regular_expression(3, &
                                                                                   .true.)//"\)\(:\)\(?:[ "//TAB//"]*$\|[^="//LF// &
                                                     "]\(?:[^#"//LF//"]*?;[ "//TAB//"]*\(.+\)\)?\)"
        end if

        makefile_bsdmake_dependency_regular_expression = replace_first("\(:\)", "\([:!]\)", &
                                                                       makefile_dependency_regular_expression%value)

        if (.not. allocated(makefile_special_targets)) then
            makefile_special_targets = [character(len=16) :: &
                                        "DEFAULT", "DONE", "ERROR", "EXPORT", &
                                        "FAILED", "GROUPEPILOG", "GROUPPROLOG", "IGNORE", &
                                        "IMPORT", "INCLUDE", "INCLUDEDIRS", "INIT", &
                                        "KEEP_STATE", "MAKEFILES", "MAKE_VERSION", "NO_PARALLEL", &
                                        "PARALLEL", "PHONY", "PRECIOUS", "REMOVE", &
                                        "SCCS_GET", "SILENT", "SOURCE", "SUFFIXES", &
                                        "WAIT", "c.o", "C.o", "m.o", &
                                        "el.elc", "y.c", "s.o"]
        end if

        if (.not. allocated(makefile_index_menu_patterns)) then
            allocate (makefile_index_menu_patterns(2))
            makefile_index_menu_patterns(1) = index_menu_pattern_type(menu_title="Dependencies", &
                                                                      matcher=makefile_previous_dependency, subexpression=1)
            makefile_index_menu_patterns(2) = index_menu_pattern_type(menu_title="Macro Assignment", &
                                                                      regular_expression=makefile_macroassign_regular_expression, &
                                                                      subexpression=1)
        end if

        if (.not. allocated(makefile_runtime_macros)) then
            makefile_runtime_macros = [character(len=8) :: "@", "&", ">", "<", "*", "^", "+", "?", "%", "$"]
        end if

        call initialize_gnumake_functions()
        call define_faces()
        call initialize_syntax_tables()
        call initialize_keywords()
        call initialize_rules()
        call makefile_mode_abbreviation_table%define_table("makefile_mode_abbreviation_table")
        call append_locals_clearer(clear_make_locals)
        call major_modes%makefile_mode%define("makefile_mode", "Makefile", makefile_mode, makefile_mode_hook, &
                                              parent=major_modes%programming_mode, syntax_table=makefile_mode_syntax_table, &
                                              abbreviation_table=makefile_mode_abbreviation_table)
        makefile_mode_map = major_modes%makefile_mode%map
        call major_modes%makefile_automake_mode%define("makefile_automake_mode", "Makefile.am", makefile_automake_mode, &
                                                       makefile_automake_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_gnu_make_mode%define("makefile_gnu_make_mode", "GNUmakefile", makefile_gnu_make_mode, &
                                                       makefile_gnu_make_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_makepp_mode%define("makefile_makepp_mode", "Makeppfile", makefile_makepp_mode, &
                                                     makefile_makepp_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_bsdmake_mode%define("makefile_bsdmake_mode", "BSDmakefile", makefile_bsdmake_mode, &
                                                      makefile_bsdmake_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_imake_mode%define("makefile_imake_mode", "Imakefile", makefile_imake_mode, &
                                                    makefile_imake_mode_hook, parent=major_modes%makefile_mode, &
                                                    syntax_table=makefile_imake_mode_syntax_table)
        call keymap_set(makefile_mode_map, "C-c :", "makefile_insert_target_reference", st)
        call keymap_set(makefile_mode_map, "C-c C-f", "makefile_pickup_filenames_as_targets", st)
        call keymap_set(makefile_mode_map, "C-c C-c", "comment_region", st)
        call keymap_set(makefile_mode_map, "C-c C-p", "makefile_pickup_everything", st)
        call keymap_set(makefile_mode_map, "C-c C-u", "makefile_create_up_to_date_overview", st)
        call keymap_set(makefile_mode_map, "C-c TAB", "makefile_insert_gnu_make_function", st)
        call keymap_set(makefile_mode_map, "C-c C-\", "makefile_backslash_region", st)
        call keymap_set(makefile_mode_map, "C-c RET C-a", "makefile_automake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET C-b", "makefile_bsdmake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET C-g", "makefile_gnu_make_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET TAB", "makefile_imake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET RET", "makefile_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET C-p", "makefile_makepp_mode", st)
        call keymap_set(makefile_mode_map, "M-p", "makefile_previous_dependency", st)
        call keymap_set(makefile_mode_map, "M-n", "makefile_next_dependency", st)
        call keymap_set(makefile_mode_map, "M-q", "fill_paragraph", st)
        call keymap_set(makefile_mode_map, "C-M-i", "completion_at_point", st)
        commands%makefile_mode%execute => makefile_mode
        commands%makefile_automake_mode%execute => makefile_automake_mode
        commands%makefile_gnu_make_mode%execute => makefile_gnu_make_mode
        commands%makefile_makepp_mode%execute => makefile_makepp_mode
        commands%makefile_bsdmake_mode%execute => makefile_bsdmake_mode
        commands%makefile_imake_mode%execute => makefile_imake_mode
        commands%makefile_next_dependency%execute => execute_makefile_next_dependency
        commands%makefile_previous_dependency%execute => execute_makefile_previous_dependency
        commands%makefile_electric_dot%execute => execute_makefile_electric_dot
        commands%makefile_insert_special_target%execute => makefile_insert_special_target
        commands%makefile_electric_equal%execute => execute_makefile_electric_equal
        commands%makefile_insert_macro%execute => execute_makefile_insert_macro
        commands%makefile_insert_macro_reference%execute => execute_makefile_insert_macro_reference
        commands%makefile_insert_target%execute => execute_makefile_insert_target
        commands%makefile_insert_target_reference%execute => execute_makefile_insert_target_reference
        commands%makefile_electric_colon%execute => execute_makefile_electric_colon
        commands%makefile_pickup_targets%execute => makefile_pickup_targets
        commands%makefile_pickup_macros%execute => makefile_pickup_macros
        commands%makefile_pickup_everything%execute => execute_makefile_pickup_everything
        commands%makefile_pickup_filenames_as_targets%execute => makefile_pickup_filenames_as_targets
        commands%makefile_backslash_region%execute => execute_makefile_backslash_region
        commands%makefile_create_up_to_date_overview%execute => makefile_create_up_to_date_overview
        commands%makefile_insert_gnu_make_function%execute => makefile_insert_gnu_make_function
    end subroutine initialize_make_mode

    !> One level of makefile-dependency-regex's elems-re.
    !> Port of make-mode.el:makefile-dependency-regex (its elems-re).
    recursive function elements_regular_expression(n, outer) result(regular_expression)
        integer, intent(in) :: n
        logical, intent(in) :: outer
        character(len=:), allocatable :: regular_expression

        if (n < 1) then
            regular_expression = "[^"//LF//"$#})]+?"
        else if (outer) then
            regular_expression = "\(?:\$\(?:[({]"//elements_regular_expression(n - 1, &
                                                                             .false.)//"[})]\|[^({]\)\|[^"//LF//"$#"//TAB//":=]\)+?"
        else
            regular_expression = "\(?:\$\(?:[({]"//elements_regular_expression(n - 1, .false.)//"[})]\|[^({]\)\|[^"//LF//"$#)}]\)+?"
        end if
    end function elements_regular_expression

    !> TEXT with every FROM replaced by TO (subr.el's string-replace).
    !> Not a port.
    function string_replace(from, to, text) result(out)
        character(len=*), intent(in) :: from, to, text
        character(len=:), allocatable :: out
        integer :: i, k

        out = ""
        i = 1

        do
            k = index(text(i:), from)

            if (k == 0) then
                out = out//text(i:)
                exit
            end if

            out = out//text(i:i + k - 2)//to
            i = i + k - 1 + len(from)
        end do
    end function string_replace

    !> TEXT with its first FROM replaced by TO.
    !> Not a port.
    function replace_first(from, to, text) result(out)
        character(len=*), intent(in) :: from, to, text
        character(len=:), allocatable :: out
        integer :: k

        k = index(text, from)

        if (k == 0) then
            out = text
        else
            out = text(1:k - 1)//to//text(k + len(from):)
        end if
    end function replace_first

    ! Not a port.
    subroutine initialize_gnumake_functions()
        integer :: k

        if (allocated(makefile_gnumake_functions)) return
        allocate (makefile_gnumake_functions(39))
        k = 0
        ! Functions for String Substitution and Analysis
        call append("subst", [character(len=11) :: "From", "To", "In"])
        call append("patsubst", [character(len=11) :: "Pattern", "Replacement", "In"])
        call append("strip", [character(len=11) :: "Text"])
        call append("findstring", [character(len=11) :: "Find what", "In"])
        call append("filter", [character(len=11) :: "Pattern", "Text"])
        call append("filter-out", [character(len=11) :: "Pattern", "Text"])
        call append("sort", [character(len=11) :: "List"])
        call append("word", [character(len=11) :: "Index", "Text"])
        call append("wordlist", [character(len=11) :: "S", "E", "Text"])
        call append("words", [character(len=11) :: "Text"])
        call append("firstword", [character(len=11) :: "Text"])
        call append("lastword", [character(len=11) :: "Names"])
        ! Functions for File Names
        call append("dir", [character(len=11) :: "Names"])
        call append("notdir", [character(len=11) :: "Names"])
        call append("suffix", [character(len=11) :: "Names"])
        call append("basename", [character(len=11) :: "Names"])
        call append("addsuffix", [character(len=11) :: "Suffix", "Names"])
        call append("addprefix", [character(len=11) :: "Prefix", "Names"])
        call append("join", [character(len=11) :: "List 1", "List 2"])
        call append("wildcard", [character(len=11) :: "Pattern"])
        call append("realpath", [character(len=11) :: "Names"])
        call append("abspath", [character(len=11) :: "Names"])
        ! Functions for Conditionals
        call append("if", [character(len=11) :: "Condition", "Then-part", "Else-part"])
        call append("or", [character(len=11) :: "Condition 1", "Condition 2", "Condition 3", "Condition 4"])
        call append("and", [character(len=11) :: "Condition 1", "Condition 2", "Condition 3", "Condition 4"])
        ! Misc functions
        call append("foreach", [character(len=11) :: "Variable", "List", "Text"])
        call append("file", [character(len=11) :: "Op", "Filename", "Text"])
        call append("call", [character(len=11) :: "Variable", "Param 1", "Param 2", "Param 3", "Param 4", "Param 5"])
        call append("value", [character(len=11) :: "Variable"])
        call append("eval", [character(len=11) :: "statement"])
        call append("origin", [character(len=11) :: "Variable"])
        call append("flavor", [character(len=11) :: "Variable"])
        call append("shell", [character(len=11) :: "Command"])
        call append("guile", [character(len=11) :: "Program"])
        ! Functions that control make
        call append("error", [character(len=11) :: "Text"])
        call append("warning", [character(len=11) :: "Text"])
        call append("info", [character(len=11) :: "Text"])

    contains

        subroutine append(name, prompts)
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: prompts(:)

            k = k + 1
            makefile_gnumake_functions(k)%name = name
            makefile_gnumake_functions(k)%prompts(1:size(prompts)) = prompts
        end subroutine append
    end subroutine initialize_gnumake_functions

    ! The faces.
    ! Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK

        f => makefile_space
        call f%define("makefile_space", "Face to use for highlighting leading spaces in Font-Lock mode.")
        call f%specification_entry(display_when(classes=[C]), face_attributes(background="hotpink"))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true.))
        f => makefile_targets
        ! This needs to go along both with foreground and background colors (i.e. shell)
        call f%define("makefile_targets", "Face to use for additionally highlighting rule targets in Font-Lock mode.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_function_name_face"))
        f => makefile_shell
        call f%define("makefile_shell", "Face to use for additionally highlighting Shell commands in Font-Lock mode.")
        call f%specification_entry(display_when(), face_attributes())
        f => makefile_makepp_perl
        call f%define("makefile_makepp_perl", "Face to use for additionally highlighting Perl code in Font-Lock mode.")
        ! Camel Book
        call f%specification_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attributes(background="LightBlue1"))
        call f%specification_entry(display_when(classes=[C], backgrounds=[DARK]), face_attributes(background="DarkBlue"))
        call f%specification_entry(display_when(), face_attributes(inverse_video=.true.))
    end subroutine define_faces

    ! Not a port.
    subroutine initialize_syntax_tables()
        type(status_type) :: st
        type(handle_type) :: t

        call make_syntax_table(makefile_mode_syntax_table, st)
        t = makefile_mode_syntax_table
        call modify_syntax_entry(iachar("("), iachar("("), "()", st, t)
        call modify_syntax_entry(iachar(")"), iachar(")"), ")(", st, t)
        call modify_syntax_entry(iachar("["), iachar("["), "(]", st, t)
        call modify_syntax_entry(iachar("]"), iachar("]"), ")[", st, t)
        call modify_syntax_entry(iachar("{"), iachar("{"), "(}", st, t)
        call modify_syntax_entry(iachar("}"), iachar("}"), "){", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar("`"), iachar("`"), """", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        call modify_syntax_entry(10, 10, ">", st, t)
        call modify_syntax_entry(iachar("="), iachar("="), ".", st, t)
        call modify_syntax_entry(iachar("$"), iachar("$"), ".", st, t)
        call make_syntax_table(makefile_imake_mode_syntax_table, st, makefile_mode_syntax_table)
        t = makefile_imake_mode_syntax_table
        call modify_syntax_entry(iachar("/"), iachar("/"), ". 14", st, t)
        call modify_syntax_entry(iachar("*"), iachar("*"), ". 23", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "'", st, t)
        call modify_syntax_entry(10, 10, ". b", st, t)
    end subroutine initialize_syntax_tables

    ! A highlight of subexpression N in FACE.
    ! Not a port.
    function h(n, face, override, lax) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        integer, intent(in), optional :: override
        logical, intent(in), optional :: lax
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, override=override, laxmatch=lax)
    end function h

    !> The keywords of a Makefile mode: macro assignments, rule actions,
    !> the variable references matching VARIABLE, automatic variables, the
    !> statements KEYWORDS (none if absent; with DASH, a `-' in them
    !> alternately means `_'), the NEGATION regexp, with SPACE suspicious
    !> whitespace, FL_KEYWORDS and dependencies.  Port of
    !> make-mode.el:makefile-make-font-lock-keywords.
    function makefile_make_font_lock_keywords(variable, space, keywords, negation, fl_keywords, dash) result(kw)
        character(len=*), intent(in) :: variable
        logical, intent(in) :: space
        character(len=*), intent(in), optional :: keywords(:)
        character(len=*), intent(in), optional :: negation
        type(font_lock_keyword_type), intent(in), optional :: fl_keywords(:)
        logical, intent(in), optional :: dash
        type(font_lock_keyword_type), allocatable :: kw(:)
        character(len=*), parameter :: VA = "font_lock_variable_name_face", NEG = "font_lock_negation_character_face"
        type(font_lock_keyword_type) :: all(64)
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3)
        character(len=:), allocatable :: regular_expression
        integer :: n, k
        logical :: d

        n = 0
        ! Do macro assignments.  These get the "variable-name" face.
        h3(1) = h(1, VA)
        ! This is for after !=
        h3(2) = h(2, "makefile_shell", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        ! This is for after normal assignment
        h3(3) = h(3, "font_lock_string_face", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        call append(font_lock_keyword(makefile_macroassign_regular_expression, highlights=h3))
        ! Rule actions.
        ! FIXME: When this spans multiple lines we need font-lock-multiline.
        h3(1) = h(1, "font_lock_type_face", lax=.true.)
        h3(2) = h(2, "makefile_shell", FONT_LOCK_OVERRIDE_PREPEND)
        ! Only makepp has builtin commands.
        h3(3) = h(3, "font_lock_builtin_face", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        call append(font_lock_keyword(matcher=makefile_match_action, highlights=h3))
        ! Variable references even in targets/strings/comments.
        h1(1) = h(2, VA, FONT_LOCK_OVERRIDE_PREPEND)
        call append(font_lock_keyword(variable, highlights=h1))
        ! Automatic variable references and single character variable references,
        ! but not shell variables references.
        h1(1) = h(1, "font_lock_constant_face", FONT_LOCK_OVERRIDE_PREPEND)
        call append(font_lock_keyword("[^$]\$\([@%<?^+*_]\|[a-zA-Z0-9]\>\)", highlights=h1))
        h1(1) = h(1, "makefile_targets", FONT_LOCK_OVERRIDE_APPEND)
        call append(font_lock_keyword("[^$]\(\$[@%*]\)", highlights=h1))

        if (present(keywords)) then
            ! Fontify conditionals and includes.
            d = .false.
            if (present(dash)) d = dash

            if (d) then
                regular_expression = string_replace("-", "[_-]", regular_expression_optimization(keywords, &
                                                                                             REGULAR_EXPRESSION_OPTIMIZATION_GROUP))
            else
                regular_expression = regular_expression_optimization(keywords, REGULAR_EXPRESSION_OPTIMIZATION_GROUP)
            end if

            h2(1) = h(1, "font_lock_keyword_face")
            h2(2) = h(2, VA)
            call append(font_lock_keyword("^\(?: [ "//TAB//"]*\)?"//string_replace(" ", "[ "//TAB//"]+", &
                                                                                   regular_expression)//"\>[ "//TAB// &
                                          "]*\([^: "//TAB//LF//"#]*\)", highlights=h2))
        end if

        if (present(negation)) then
            h2(1) = h(1, NEG, FONT_LOCK_OVERRIDE_PREPEND)
            h2(2) = h(2, NEG, FONT_LOCK_OVERRIDE_PREPEND, .true.)
            call append(font_lock_keyword(negation, highlights=h2))
        end if

        if (space) then
            ! Highlight lines that contain just whitespace.
            ! They can cause trouble, especially if they start with a tab.
            call append(font_lock_keyword("^[ "//TAB//"]+$", face="makefile_space"))
            ! Highlight shell comments that Make treats as commands,
            ! since these can fool people.
            h1(1) = h(0, "makefile_space", FONT_LOCK_OVERRIDE_REPLACE)
            call append(font_lock_keyword("^"//TAB//"+#", highlights=h1))
            ! Highlight spaces that precede tabs.
            ! They can make a tab fail to be effective.
            h1(1) = h(1, "makefile_space")
            call append(font_lock_keyword("^\( +\)"//TAB, highlights=h1))
        end if

        if (present(fl_keywords)) then
            do k = 1, size(fl_keywords)
                call append(fl_keywords(k))
            end do
        end if

        ! Do dependencies.
        h2(1) = h(1, "makefile_targets", FONT_LOCK_OVERRIDE_PREPEND)
        h2(2) = h(3, "makefile_shell", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        call append(font_lock_keyword(matcher=makefile_match_dependency, highlights=h2))
        allocate (kw(n))
        kw(:) = all(1:n)

    contains

        subroutine append(k)
            type(font_lock_keyword_type), intent(in) :: k

            n = n + 1
            all(n) = k
        end subroutine append
    end function makefile_make_font_lock_keywords

    ! Not a port.
    subroutine initialize_keywords()
        character(len=*), parameter :: KW = "font_lock_keyword_face", FN = "font_lock_function_name_face", &
                                       VA = "font_lock_variable_name_face", TY = "font_lock_type_face", &
                                       PERL = "makefile_makepp_perl", SHELL = "makefile_shell", TG = "makefile_targets"
        integer, parameter :: PREPEND = FONT_LOCK_OVERRIDE_PREPEND, APPEND = FONT_LOCK_OVERRIDE_APPEND, &
                              T = FONT_LOCK_OVERRIDE_REPLACE
        type(font_lock_keyword_type) :: extra(16)
        type(font_lock_keyword_type), allocatable :: base(:)
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3), a(1)
        integer :: n

        makefile_font_lock_keywords = makefile_make_font_lock_keywords(makefile_variable_use_regular_expression, .true., &
                                                                       makefile_statements)
        makefile_automake_font_lock_keywords = makefile_make_font_lock_keywords(makefile_variable_use_regular_expression, .true., &
                                                                                makefile_automake_statements)
        ! GNU make.
        h1(1) = h(1, TG, APPEND)
        extra(1) = font_lock_keyword("[^$]\(\$[({][@%*][DF][})]\)", highlights=h1)
        ! Automatic variables.
        extra(2) = font_lock_keyword("[^$]\(\$[@%*?+^|]\)", highlights=h1)
        ! $(function ...) ${function ...}
        h1(1) = h(1, FN, PREPEND)
        extra(3) = font_lock_keyword("[^$]\$[({]\([-a-zA-Z0-9_.]+\s \)", highlights=h1)
        ! $(shell ...) ${shell ...}
        h1(1) = h(1, SHELL, PREPEND, .true.)
        a(1) = font_lock_anchored(h1, matcher=makefile_match_function_end)
        extra(4) = font_lock_keyword("[^$]\$\([({]\)shell[ "//TAB//"]+", highlights=a)
        makefile_gnu_make_font_lock_keywords = makefile_make_font_lock_keywords(makefile_variable_use_regular_expression, .true., &
                                                                                makefile_gnu_make_statements, &
                                                                                "^\(?: [ "//TAB//"]*\)?if\(n\)\(?:def\|eq\)\>", &
                                                                                extra(1:4))
        ! makepp.
        h1(1) = h(1, TG, APPEND)
        extra(1) = font_lock_keyword("[^$]\(\$[({]\(?:output\|stem\|target\)s?\_>.*?[})]\)", highlights=h1)
        ! Colon modifier keywords.
        h3(1) = h(1, TY, T)
        h3(2) = h(2, KW, T)
        h3(3) = h(3, VA, T)
        extra(2) = font_lock_keyword("\(:\s *\)\(build_c\(?:ache\|heck\)\|env\(?:ironment\)?\|foreach\|signature\|" &
                                     //"scanner\|quickscan\|smartscan\)\>\([^:"//LF//"]*\)", highlights=h3)
        ! $(function ...) $((function ...)) ${...} ${{...}} $[...] $[[...]]
        h1(1) = h(1, FN, PREPEND)
        extra(3) = font_lock_keyword("[^$]\$\(?:((?\|{{?\|\[\[?\)\([-a-zA-Z0-9_.]+\s \)", highlights=h1)
        ! $(shell ...) $((shell ...)) ${...} ${{...}} $[...] $[[...]]
        h1(1) = h(1, SHELL, PREPEND, .true.)
        a(1) = font_lock_anchored(h1, matcher=makefile_match_function_end)
        extra(4) = font_lock_keyword("[^$]\$\(((?\|{{?\|\[\[?\)shell\(?:[-_]\(?:global[-_]\)?once\)?[ "//TAB//"]+", &
                                     highlights=a)
        ! $(perl ...) $((perl ...)) ${...} ${{...}} $[...] $[[...]]
        h1(1) = h(1, PERL, PREPEND, .true.)
        a(1) = font_lock_anchored(h1, matcher=makefile_match_function_end)
        extra(5) = font_lock_keyword("[^$]\$\(((?\|{{?\|\[\[?\)makeperl[ "//TAB//"]+", highlights=a)
        h1(1) = h(1, PERL, T, .true.)
        a(1) = font_lock_anchored(h1, matcher=makefile_match_function_end)
        extra(6) = font_lock_keyword("[^$]\$\(((?\|{{?\)perl[ "//TAB//"]+", highlights=a)
        ! Can we unify these with (if (match-end 1) 'prepend t)?
        h1(1) = h(1, PERL, PREPEND)
        extra(7) = font_lock_keyword("ifmakeperl\s +\(.*\)", highlights=h1)
        h1(1) = h(1, PERL, T)
        extra(8) = font_lock_keyword("ifperl\s +\(.*\)", highlights=h1)
        ! Perl block single- or multiline, as statement or rule action.
        ! Don't know why the initial newline in 2nd variant of group 2 doesn't get skipped.
        h2(1) = h(1, PERL, PREPEND, .true.)
        h2(2) = h(2, PERL, PREPEND, .true.)
        extra(9) = font_lock_keyword("\<make\(?:perl\|sub\s +\S +\)\s *"//LF//"?\s *{\(?:{\s *"//LF//"?\(\(?:.*"//LF// &
                                     "\)+?\)\s *}\|\s *\(\(?:.*?\|"//LF//"?\(?:.*"//LF//"\)+?\)\)\)}", highlights=h2)
        h2(1) = h(1, PERL, T, .true.)
        h2(2) = h(2, PERL, T, .true.)
        extra(10) = font_lock_keyword("\<\(?:perl\|sub\s +\S +\)\s *"//LF//"?\s *{\(?:{\s *"//LF//"?\(\(?:.*"//LF// &
                                      "\)+?\)\s *}\|\s *\(\(?:.*?\|"//LF//"?\(?:.*"//LF//"\)+?\)\)\)}", highlights=h2)
        ! Statement style perl block.
        h1(1) = h(1, PERL, T)
        extra(11) = font_lock_keyword("perl[-_]begin\s *\(?:\s #.*\)?"//LF//"\(\(?:.*"//LF//"\)+?\)\s *perl[-_]end\>", &
                                      highlights=h1)
        makefile_makepp_font_lock_keywords = &
            makefile_make_font_lock_keywords(makefile_variable_use_regular_expression, .false., makefile_makepp_statements, &
                                             "^\(?: [ "//TAB//"]*\)?\(?:and[ "//TAB//"]+\|else[ "//TAB//"]+\|or[ "//TAB// &
                                             "]+\)?if\(n\)\(?:def\|eq\|sys\|true\)\>", extra(1:11), dash=.true.)
        ! BSD make.  A lot more could be done for variables here.
        h1(1) = h(1, KW)
        extra(1) = font_lock_keyword("^[ "//TAB//"]*\.for[ "//TAB//"].+[ "//TAB//"]\(in\)\>", highlights=h1)
        base = makefile_make_font_lock_keywords(makefile_variable_use_regular_expression, .true., &
                                                negation="^\(?: [ "//TAB//"]*\)?\.\(?:el\)?if\(n?\)\(?:def\|make\)?\>[ " &
                                                //TAB//"]*\(!?\)", fl_keywords=extra(1:1))
        n = size(base)
        allocate (makefile_bsdmake_font_lock_keywords(n + 1))
        makefile_bsdmake_font_lock_keywords(1:n) = base
        makefile_bsdmake_font_lock_keywords(n + 1) = font_lock_keyword("^\. *"//regular_expression_optimization( &
                                                                       makefile_bsdmake_statements) &
                                                                       //"\>", face=KW)
        ! Imake.
        n = size(makefile_font_lock_keywords)
        allocate (makefile_imake_font_lock_keywords(n + 3))
        makefile_imake_font_lock_keywords(1) = font_lock_keyword("XCOMM.*$", face="font_lock_comment_face")
        h1(1) = h(0, KW, PREPEND)
        makefile_imake_font_lock_keywords(2) = font_lock_keyword("XVAR\(?:use\|def\)[0-9]", highlights=h1)
        makefile_imake_font_lock_keywords(3) = font_lock_keyword("@@", face="font_lock_preprocessor_face")
        makefile_imake_font_lock_keywords(4:) = makefile_font_lock_keywords
    end subroutine initialize_keywords

    ! The rules of `makefile_syntax_propertize_function'.
    ! Not a port.
    subroutine initialize_rules()
        type(syntax_highlight_type) :: hl

        if (allocated(propertize_rules%rules)) return
        allocate (propertize_rules%rules(2))
        ! From sh-script.el.
        ! A `#' begins a comment in sh when it is unquoted and at the beginning
        ! of a word.  In the shell, words are separated by metacharacters.
        ! The list of special chars is taken from the single-unix spec of the
        ! shell command language (under `quoting') but with `$' removed.
        propertize_rules%rules(1)%regular_expression = "[^|&;<>()`\""' "//TAB//LF//"]\(#+\)"
        hl = syntax_highlight_type(group=1, syntax="_")
        propertize_rules%rules(1)%highlights = [hl]
        ! Change the syntax of a quoted newline so that it does not end a comment.
        propertize_rules%rules(2)%regular_expression = "\\"//LF
        hl = syntax_highlight_type(group=0, syntax=".")
        propertize_rules%rules(2)%highlights = [hl]
    end subroutine initialize_rules

    !> The `syntax_propertize_function' of Makefile mode.  Port of
    !> make-mode.el makefile-syntax-propertize-function.
    recursive subroutine makefile_syntax_propertize_function(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call goto_character(start)
        call propertize_rules%apply(start, end, status)
    end subroutine makefile_syntax_propertize_function

    ! ------------------------------------------------------------------
    ! Buffer-local tables
    ! ------------------------------------------------------------------

    ! The current buffer's tables (its own in Makefile mode buffers).
    ! Not a port.
    function state() result(s)
        type(make_state_type), pointer :: s
        integer :: k

        s => default_state
        if (current == 0 .or. .not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, dim=1)
        if (k > 0) s => locals(k)
    end function state

    ! Give the current buffer its own tables (make-local-variable: with
    ! the default values).
    ! Not a port.
    subroutine make_local()
        integer :: k

        if (.not. allocated(locals)) allocate (locals(0))
        if (any(locals%serial == buffers(current)%serial)) return

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, default_state]
        locals(k) = default_state
        locals(k)%serial = buffers(current)%serial
    end subroutine make_local

    ! kill-all-local-variables: forget the current buffer's tables.
    ! Not a port.
    subroutine clear_make_locals()
        if (.not. allocated(locals)) return
        where (locals%serial == buffers(current)%serial) locals%serial = 0
    end subroutine clear_make_locals

    ! Whether NAME is one of NAMES (assoc).
    ! Not a port.
    logical function member_name(names, name) result(found)
        type(string_type), allocatable, intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: k

        found = .false.
        if (.not. allocated(names)) return

        do k = 1, size(names)
            if (names(k)%text == name .and. len(names(k)%text) == len(name)) then
                found = .true.
                return
            end if
        end do
    end function member_name

    ! The empty table.
    ! Not a port.
    subroutine clear_names(names)
        type(string_type), allocatable, intent(inout) :: names(:)

        if (allocated(names)) deallocate (names)
        allocate (names(0))
    end subroutine clear_names

    !> Table of all target names known for this buffer, newest first.
    !> Port of make-mode.el makefile-target-table.
    function makefile_target_table() result(names)
        type(string_type), allocatable :: names(:)
        type(make_state_type), pointer :: s

        s => state()

        if (allocated(s%targets)) then
            names = s%targets
        else
            allocate (names(0))
        end if
    end function makefile_target_table

    !> Table of all macro names known for this buffer, newest first.
    !> Port of make-mode.el makefile-macro-table.
    function makefile_macro_table() result(names)
        type(string_type), allocatable :: names(:)
        type(make_state_type), pointer :: s

        s => state()

        if (allocated(s%macros)) then
            names = s%macros
        else
            allocate (names(0))
        end if
    end function makefile_macro_table

    !> The targets known to have prerequisites, newest first.  Port of
    !> make-mode.el makefile-has-prereqs.
    function makefile_has_prereqs() result(names)
        type(string_type), allocatable :: names(:)
        type(make_state_type), pointer :: s

        s => state()

        if (allocated(s%prereqs)) then
            names = s%prereqs
        else
            allocate (names(0))
        end if
    end function makefile_has_prereqs

    ! The names of NAMES as a list of strings.
    ! Not a port.
    function names_list(names) result(v)
        type(string_type), allocatable, intent(in) :: names(:)
        type(value_type) :: v
        type(value_type), allocatable :: items(:)
        integer :: k

        if (.not. allocated(names)) then
            v = value_type()
            return
        end if

        allocate (items(size(names)))

        do k = 1, size(names)
            items(k) = value_type(text=names(k)%text)
        end do

        v = make_list(items)
    end function names_list

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    ! What loading make-mode.el does once options are set: the electric
    ! keys.
    ! Not a port.
    subroutine load_make_mode()
        type(status_type) :: st

        if (loaded) return
        loaded = .true.

        if (makefile_electric_keys%value) then
            call keymap_set(makefile_mode_map, "$", "makefile_insert_macro_reference", st)
            call keymap_set(makefile_mode_map, ":", "makefile_electric_colon", st)
            call keymap_set(makefile_mode_map, "=", "makefile_electric_equal", st)
            call keymap_set(makefile_mode_map, ".", "makefile_electric_dot", st)
        end if
    end subroutine load_make_mode

    ! FONT_LOCK as the keywords of `font_lock_defaults', with the rest of
    ! Makefile mode's.
    ! Not a port.
    subroutine set_font_lock_defaults(font_lock)
        type(font_lock_keyword_type), intent(in) :: font_lock(:)
        type(font_lock_syntax_entry_type) :: syntax(1)

        syntax(1)%chars = "$"
        syntax(1)%syntax = "."
        call font_lock_defaults%set_local(keywords=font_lock, syntax_modifications=syntax)
    end subroutine set_font_lock_defaults

    !> Major mode for editing standard Makefiles.
    !>
    !> If you are editing a file for a different make, try one of the
    !> variants `makefile_automake_mode', `makefile_gnu_make_mode',
    !> `makefile_makepp_mode', `makefile_bsdmake_mode' or,
    !> `makefile_imake_mode'.  All but the last should be correctly
    !> chosen based on the file name, except if it is *.mk.  This
    !> function ends by invoking the function(s) `makefile_mode_hook'.
    !>
    !> It is strongly recommended to use `font_lock_mode', because that
    !> provides additional parsing information.  This is used for
    !> example to see that a rule action `echo foo: bar' is a not rule
    !> dependency, despite the colon.
    !>
    !> \{makefile_mode_map}
    !>
    !> Makefile mode can be configured by modifying the following variables:
    !>
    !> `makefile_target_colon':
    !>     The string that gets appended to all target names
    !>     inserted by `makefile_insert_target'.
    !>     ":" or "::" are quite common values.
    !>
    !> `makefile_macro_assign':
    !>    The string that gets appended to all macro names
    !>    inserted by `makefile_insert_macro'.
    !>    The normal value should be " = ", since this is what
    !>    standard make expects.  However, newer makes such as dmake
    !>    allow a larger variety of different macro assignments, so you
    !>    might prefer to use " += " or " := " .
    !>
    !> `makefile_tab_after_target_colon':
    !>    If you want a TAB (instead of a space) to be appended after the
    !>    target colon, then set this to a true value.
    !>
    !> `makefile_pickup_everything_picks_up_filenames':
    !>    If this variable is set to a true value then
    !>    `makefile_pickup_everything' also picks up filenames as targets
    !>    (i.e. it calls `makefile_pickup_filenames_as_targets'), otherwise
    !>    filenames are omitted.
    !>
    !> `makefile_cleanup_continuations':
    !>    If this variable is set to a true value then Makefile mode
    !>    will assure that no line in the file ends with a backslash
    !>    (the continuation character) followed by any whitespace.
    !>    This is done by silently removing the trailing whitespace, leaving
    !>    the backslash itself intact.
    !>    IMPORTANT: Please note that enabling this option causes Makefile mode
    !>    to MODIFY A FILE WITHOUT YOUR CONFIRMATION when "it seems necessary".
    !>
    !> `makefile_special_targets':
    !>    List of special targets.  You will be offered to complete
    !>    on one of those in the minibuffer whenever you enter a `.'.
    !>    at the beginning of a line in Makefile mode.
    !>
    !> Port of make-mode.el:makefile-mode.
    recursive subroutine makefile_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(makefile_mode_syntax_table)) call initialize_make_mode()
        call load_make_mode()
        call major_modes%makefile_mode%begin(saved, status)
        if (.not. status%ok()) return
        call completion_at_point_functions%prepend(makefile_completions_at_point, local=.true.)
        call write_file_functions%prepend(makefile_warn_suspicious_lines, local=.true.)
        call write_file_functions%prepend(makefile_warn_continuations, local=.true.)
        call write_file_functions%prepend(makefile_cleanup_continuations_function, local=.true.)
        call make_local()
        ! Font lock.
        call set_font_lock_defaults(makefile_font_lock_keywords)
        call syntax_propertize_function%set_local(makefile_syntax_propertize_function)
        ! Add-log.
        call add_log_current_subprogram_function%set_local(makefile_add_log_target)
        ! Imenu.
        call index_menu_patterns%set_local(makefile_index_menu_patterns)
        ! Dabbrev.
        call dynamic_abbreviation_skip_leading_regular_expression%set_local("\$")
        ! Filling.
        call fill_paragraph_function%set_local(makefile_fill_paragraph)
        ! Comment stuff.
        call comment_start%set_local("#")
        call comment_end%set_local("")
        call comment_start_skip%set_local("#+[ "//TAB//"]*")
        ! Make sure TAB really inserts \t.
        call indent_line_function%set_local(indent_to_left_margin)
        ! Real TABs are important in makefiles
        call indent_tabs_mode%set_local(.true.)
        call major_modes%makefile_mode%end(saved, status)
    end subroutine makefile_mode

    !> An adapted `makefile_mode' that knows about automake.  Port of
    !> make-mode.el:makefile-automake-mode.
    recursive subroutine makefile_automake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(makefile_mode_syntax_table)) call initialize_make_mode()
        call major_modes%makefile_automake_mode%begin(saved, status)
        if (.not. status%ok()) return
        call set_font_lock_defaults(makefile_automake_font_lock_keywords)
        call major_modes%makefile_automake_mode%end(saved, status)
    end subroutine makefile_automake_mode

    !> An adapted `makefile_mode' that knows about gmake.  Port of
    !> make-mode.el:makefile-gmake-mode.
    recursive subroutine makefile_gnu_make_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(makefile_mode_syntax_table)) call initialize_make_mode()
        call major_modes%makefile_gnu_make_mode%begin(saved, status)
        if (.not. status%ok()) return
        call set_font_lock_defaults(makefile_gnu_make_font_lock_keywords)
        call major_modes%makefile_gnu_make_mode%end(saved, status)
    end subroutine makefile_gnu_make_mode

    !> An adapted `makefile_mode' that knows about makepp.  Port of
    !> make-mode.el:makefile-makepp-mode.
    recursive subroutine makefile_makepp_mode(status)
        type(status_type), intent(out) :: status
        type(index_menu_pattern_type), allocatable :: old(:), patterns(:)
        logical :: saved

        if (.not. is_syntax_table_live(makefile_mode_syntax_table)) call initialize_make_mode()
        call major_modes%makefile_makepp_mode%begin(saved, status)
        if (.not. status%ok()) return
        call makefile_rule_action_regular_expression%set_local(makefile_makepp_rule_action_regular_expression)
        call set_font_lock_defaults(makefile_makepp_font_lock_keywords)
        old = index_menu_patterns%get_current()
        allocate (patterns(size(old) + 1))
        patterns(1) = index_menu_pattern_type(menu_title="Functions", regular_expression="^[ "//TAB//"]*\(?:make\)?sub[ "//TAB// &
                                              "]+\([A-Za-z0-9_]+\)", subexpression=1)
        patterns(2:) = old
        call index_menu_patterns%set_local(patterns)
        call major_modes%makefile_makepp_mode%end(saved, status)
    end subroutine makefile_makepp_mode

    !> An adapted `makefile_mode' that knows about BSD make.  Port of
    !> make-mode.el:makefile-bsdmake-mode.
    recursive subroutine makefile_bsdmake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(makefile_mode_syntax_table)) call initialize_make_mode()
        call major_modes%makefile_bsdmake_mode%begin(saved, status)
        if (.not. status%ok()) return
        call makefile_dependency_regular_expression%set_local(makefile_bsdmake_dependency_regular_expression)
        call makefile_dependency_skip%set_local("^:!")
        call makefile_rule_action_regular_expression%set_local(makefile_bsdmake_rule_action_regular_expression)
        call set_font_lock_defaults(makefile_bsdmake_font_lock_keywords)
        call major_modes%makefile_bsdmake_mode%end(saved, status)
    end subroutine makefile_bsdmake_mode

    !> An adapted `makefile_mode' that knows about imake.  Port of
    !> make-mode.el:makefile-imake-mode.
    recursive subroutine makefile_imake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(makefile_mode_syntax_table)) call initialize_make_mode()
        call major_modes%makefile_imake_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! (setq-local syntax-propertize-function nil)
        call syntax_propertize_function%clear_local()
        call set_font_lock_defaults(makefile_imake_font_lock_keywords)
        call comment_start%set_local("XCOMM")
        call comment_start_skip%set_local("XCOMM[ "//TAB//"]*")
        call major_modes%makefile_imake_mode%end(saved, status)
    end subroutine makefile_imake_mode

    ! ------------------------------------------------------------------
    ! Motion
    ! ------------------------------------------------------------------

    !> Move point to the beginning of the next dependency line (FOUND, if
    !> there is one).  Port of make-mode.el:makefile-next-dependency.
    subroutine makefile_next_dependency(found)
        logical, intent(out) :: found
        integer(int64) :: here

        here = point()
        call end_of_line()
        call match_dependency(0_int64, found)

        if (found) then
            call beginning_of_line()
        else
            call goto_character(here)
        end if
    end subroutine makefile_next_dependency

    !> Move point to the beginning of the next dependency line.  Port of
    !> make-mode.el:makefile-next-dependency.
    subroutine execute_makefile_next_dependency(status)
        type(status_type), intent(out) :: status
        logical :: found

        status = status_type()
        call makefile_next_dependency(found)
    end subroutine execute_makefile_next_dependency

    !> Move point to the beginning of the previous dependency line (FOUND,
    !> if there is one).  Port of make-mode.el:makefile-previous-dependency.
    subroutine makefile_previous_dependency(found)
        logical, intent(out) :: found
        integer(int64) :: point_value, moved
        type(status_type) :: st
        logical :: is_equal

        point_value = point()
        call beginning_of_line()
        found = .false.

        ! makefile-match-dependency done backwards:
        do
            moved = skip_characters_backward(makefile_dependency_skip%value, st)
            if (is_beginning_of_buffer()) exit
            is_equal = after_character() == iachar("=")
            call goto_character(point() - 1)
            if (is_equal) cycle
            if (has_face(point())) cycle
            call beginning_of_line()

            if (point() > point_minimum() + 2) then
                if (character_before_at(point() - 1) == iachar("\")) cycle
            end if

            if (looking_at(makefile_dependency_regular_expression%value, st)) then
                found = .true.
                return
            end if
        end do

        call goto_character(point_value)
    end subroutine makefile_previous_dependency

    !> Move point to the beginning of the previous dependency line.  Port
    !> of make-mode.el:makefile-previous-dependency.
    subroutine execute_makefile_previous_dependency(status)
        type(status_type), intent(out) :: status
        logical :: found

        status = status_type()
        call makefile_previous_dependency(found)
    end subroutine execute_makefile_previous_dependency

    ! ------------------------------------------------------------------
    ! Electric keys
    ! ------------------------------------------------------------------

    ! The character of the key that invoked the command.
    ! Not a port.
    integer(int64) function event_character() result(c)
        c = -1
        if (vtag(last_command_event) == V_INTEGER) c = xfixnum(last_command_event)
    end function event_character

    !> Prompt for the name of a special target to insert.
    !> Only does electric insertion at beginning of line.
    !> Anywhere else just self-inserts.  Port of
    !> make-mode.el:makefile-electric-dot.
    recursive subroutine makefile_electric_dot(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (is_beginning_of_line()) then
            call makefile_insert_special_target(status)
        else
            call self_insert_command(argument, event_character(), status)
        end if
    end subroutine makefile_electric_dot

    !> Prompt for the name of a special target to insert (the command).
    recursive subroutine execute_makefile_electric_dot(status)
        type(status_type), intent(out) :: status

        call makefile_electric_dot(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_makefile_electric_dot

    !> Prompt for and insert a special target name.
    !> Uses `makefile_special_targets' list.  Port of
    !> make-mode.el:makefile-insert-special-target.
    recursive subroutine makefile_insert_special_target(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: special_target
        integer :: k

        call makefile_pickup_targets(status)
        if (.not. status%ok()) return
        allocate (items(size(makefile_special_targets)))

        do k = 1, size(items)
            items(k) = value_type(text=trim(makefile_special_targets(k)))
        end do

        table%list = make_list(items)
        call completing_read("Special target: ", table, special_target, status)
        if (.not. status%ok()) return
        if (len(special_target) == 0) return
        call insert("."//special_target//":", status)
        if (.not. status%ok()) return
        call makefile_forward_after_target_colon(status)
    end subroutine makefile_insert_special_target

    !> Prompt for name of a macro to insert.
    !> Only does prompting if point is at beginning of line.
    !> Anywhere else just self-inserts.  Port of
    !> make-mode.el:makefile-electric-equal.
    recursive subroutine makefile_electric_equal(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        call makefile_pickup_macros(status)
        if (.not. status%ok()) return

        if (is_beginning_of_line()) then
            call execute_interactively(value_type(name="makefile_insert_macro"), status)
        else
            call self_insert_command(argument, event_character(), status)
        end if
    end subroutine makefile_electric_equal

    !> Prompt for name of a macro to insert (the command).
    recursive subroutine execute_makefile_electric_equal(status)
        type(status_type), intent(out) :: status

        call makefile_electric_equal(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_makefile_electric_equal

    !> Prepare definition of a new macro named MACRO_NAME.
    !> Interactively, prompt for the name of the macro.  Port of
    !> make-mode.el:makefile-insert-macro.
    recursive subroutine makefile_insert_macro(macro_name, status)
        character(len=*), intent(in) :: macro_name
        type(status_type), intent(out) :: status
        logical :: added

        call makefile_pickup_macros(status)
        if (.not. status%ok()) return
        if (len(macro_name) == 0) return
        call beginning_of_line()
        call insert(macro_name//makefile_macro_assign%value, status)
        if (.not. status%ok()) return

        state_need_macro: block
            type(make_state_type), pointer :: s
            s => state()
            s%need_macro_pickup = .true.
        end block state_need_macro

        added = makefile_remember_macro(macro_name)
    end subroutine makefile_insert_macro

    !> Prepare definition of a new macro (the command).
    recursive subroutine execute_makefile_insert_macro(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Macro Name: ", name, status)
        if (.not. status%ok()) return
        call makefile_insert_macro(name, status)
    end subroutine execute_makefile_insert_macro

    !> Complete on a list of known macros, then insert complete ref at point.
    !> Port of make-mode.el:makefile-insert-macro-ref.
    recursive subroutine makefile_insert_macro_reference(macro_name, status)
        character(len=*), intent(in) :: macro_name
        type(status_type), intent(out) :: status

        call makefile_do_macro_insertion(macro_name, status)
    end subroutine makefile_insert_macro_reference

    !> Complete on a list of known macros, then insert complete ref at point
    !> (the command).
    recursive subroutine execute_makefile_insert_macro_reference(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        character(len=:), allocatable :: name
        type(make_state_type), pointer :: s

        call makefile_pickup_macros(status)
        if (.not. status%ok()) return
        s => state()
        table%list = names_list(s%macros)
        call completing_read("Refer to macro: ", table, name, status)
        if (.not. status%ok()) return
        call makefile_insert_macro_reference(name, status)
    end subroutine execute_makefile_insert_macro_reference

    !> Prepare definition of a new target (dependency line).  Port of
    !> make-mode.el:makefile-insert-target.
    recursive subroutine makefile_insert_target(target_name, status)
        character(len=*), intent(in) :: target_name
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s
        logical :: has

        status = status_type()
        if (len(target_name) == 0) return
        call beginning_of_line()
        call insert(target_name//makefile_target_colon%value, status)
        if (.not. status%ok()) return
        call makefile_forward_after_target_colon(status)
        if (.not. status%ok()) return
        call end_of_line()
        s => state()
        s%need_target_pickup = .true.
        has = makefile_remember_target(target_name)
    end subroutine makefile_insert_target

    !> Prepare definition of a new target (the command).
    recursive subroutine execute_makefile_insert_target(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Target: ", name, status)
        if (.not. status%ok()) return
        call makefile_insert_target(name, status)
    end subroutine execute_makefile_insert_target

    !> Complete on a list of known targets, then insert TARGET_NAME at point.
    !> Port of make-mode.el:makefile-insert-target-ref.
    recursive subroutine makefile_insert_target_reference(target_name, status)
        character(len=*), intent(in) :: target_name
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(target_name) == 0) return
        call insert(target_name//" ", status)
    end subroutine makefile_insert_target_reference

    !> Complete on a list of known targets, then insert it at point (the
    !> command).
    recursive subroutine execute_makefile_insert_target_reference(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        character(len=:), allocatable :: name
        type(make_state_type), pointer :: s

        call makefile_pickup_targets(status)
        if (.not. status%ok()) return
        s => state()
        table%list = names_list(s%targets)
        call completing_read("Refer to target: ", table, name, status)
        if (.not. status%ok()) return
        call makefile_insert_target_reference(name, status)
    end subroutine execute_makefile_insert_target_reference

    !> Prompt for name of new target.
    !> Prompting only happens at beginning of line.
    !> Anywhere else just self-inserts.  Port of
    !> make-mode.el:makefile-electric-colon.
    recursive subroutine makefile_electric_colon(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (is_beginning_of_line()) then
            call execute_interactively(value_type(name="makefile_insert_target"), status)
        else
            call self_insert_command(argument, event_character(), status)
        end if
    end subroutine makefile_electric_colon

    !> Prompt for name of new target (the command).
    recursive subroutine execute_makefile_electric_colon(status)
        type(status_type), intent(out) :: status

        call makefile_electric_colon(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_makefile_electric_colon

    ! ------------------------------------------------------------------
    ! Extracting targets and macros from an existing makefile
    ! ------------------------------------------------------------------

    !> Notice names of all target definitions in Makefile.  Port of
    !> make-mode.el:makefile-pickup-targets.
    recursive subroutine makefile_pickup_targets(status)
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s
        character(len=:), allocatable :: target_name
        character(len=32) :: line_text
        integer(int64) :: original_point, start, moved, shortage
        logical :: found, has_prereqs
        type(status_type) :: st

        status = status_type()
        s => state()
        if (.not. s%need_target_pickup) return
        s%need_target_pickup = .false.
        call clear_names(s%targets)
        call clear_names(s%prereqs)
        original_point = point()
        call goto_character(point_minimum())

        do
            call match_dependency(0_int64, found)
            if (.not. found) exit
            call goto_character(match_beginning(1, st))

            do
                start = point()
                moved = skip_characters_forward("^ "//TAB//":#", st)
                call buffer_substring(start, point(), target_name, st)
                has_prereqs = .not. looking_at(":["//TAB//" ]*$", st)

                if (makefile_remember_target(target_name, has_prereqs)) then
                    write (line_text, '(""" from line ", i0)') line_number_at_position()
                    call message("Picked up target """//target_name//trim(line_text))
                end if

                moved = skip_characters_forward(" "//TAB, st)
                if (is_end_of_line()) exit
                if (after_character() == iachar(":")) exit
            end do

            shortage = forward_line()
        end do

        call goto_character(original_point)
        call message("Read targets OK")
    end subroutine makefile_pickup_targets

    !> Notice names of all macro definitions in Makefile.  Port of
    !> make-mode.el:makefile-pickup-macros.
    recursive subroutine makefile_pickup_macros(status)
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s
        character(len=:), allocatable :: macro_name
        character(len=32) :: line_text
        integer(int64) :: original_point, start, moved, shortage, found
        type(status_type) :: st

        status = status_type()
        s => state()
        if (.not. s%need_macro_pickup) return
        s%need_macro_pickup = .false.
        call clear_names(s%macros)
        original_point = point()
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(makefile_macroassign_regular_expression, found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            call goto_character(match_beginning(1, st))
            start = point()
            moved = skip_characters_forward("^ "//TAB//":#=*", st)
            call buffer_substring(start, point(), macro_name, st)

            if (makefile_remember_macro(macro_name)) then
                write (line_text, '(""" from line ", i0)') line_number_at_position()
                call message("Picked up macro """//macro_name//trim(line_text))
            end if

            shortage = forward_line()
        end do

        call goto_character(original_point)
        call message("Read macros OK")
    end subroutine makefile_pickup_macros

    !> Notice names of all macros and targets in Makefile.
    !> Prefix argument ARGUMENT means force pickups to be redone.  Port of
    !> make-mode.el:makefile-pickup-everything.
    recursive subroutine makefile_pickup_everything(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s

        if (argument) then
            s => state()
            s%need_target_pickup = .true.
            s%need_macro_pickup = .true.
        end if

        call makefile_pickup_macros(status)
        if (.not. status%ok()) return
        call makefile_pickup_targets(status)
        if (.not. status%ok()) return
        if (makefile_pickup_everything_picks_up_filenames%value) call makefile_pickup_filenames_as_targets(status)
    end subroutine makefile_pickup_everything

    !> Notice names of all macros and targets in Makefile (the command).
    recursive subroutine execute_makefile_pickup_everything(status)
        type(status_type), intent(out) :: status

        call makefile_pickup_everything(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_makefile_pickup_everything

    !> Scan the current directory for filenames to use as targets.
    !> Checks each filename against `makefile_ignored_files_in_pickup_regular_expression'
    !> and adds all qualifying names to the list of known targets.  Port of
    !> make-mode.el:makefile-pickup-filenames-as-targets.
    recursive subroutine makefile_pickup_filenames_as_targets(status)
        type(status_type), intent(out) :: status
        type(value_type) :: names
        character(len=:), allocatable :: name, directory
        integer(int64) :: k
        type(status_type) :: st

        if (.not. allocated(buffers(current)%filename)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected character(len=*)")
            return
        end if

        directory = file_name_directory(buffers(current)%filename)
        call file_name_all_completions("", directory, names, status)
        if (.not. status%ok()) return

        do k = 1, list_length(names)
            name = xstring(list_element(names, k))
            if (is_file_directory(name)) cycle
            if (string_match(makefile_ignored_files_in_pickup_regular_expression, name, st) > 0) cycle
            if (makefile_remember_target(name)) call message("Picked up file """//name//""" as target")
        end do
    end subroutine makefile_pickup_filenames_as_targets

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    !> The completion data of the target or macro name before point (for
    !> `completion_at_point_functions').  Port of
    !> make-mode.el:makefile-completions-at-point.
    recursive subroutine makefile_completions_at_point(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, original_point, moved
        integer :: pc, parenthesis
        logical :: do_macros, at_beginning_of_line
        type(make_state_type), pointer :: s
        type(value_type) :: targets, macros, all
        integer(int64) :: k, n

        status = status_type()
        original_point = point()
        moved = skip_characters_backward("^$(){}:#= "//TAB//LF, status)
        beginning = point()
        parenthesis = 0
        do_macros = .false.
        pc = before_character()

        if (pc == iachar("$")) then
            ! Preceding "$" means macros only.
            do_macros = .true.
        else if (pc == iachar("{") .or. pc == iachar("(")) then
            ! Preceding "$(" or "${" means macros only.
            if (pc == iachar("{")) then
                parenthesis = iachar("}")
            else
                parenthesis = iachar(")")
            end if

            call goto_character(point() - 1)
            do_macros = before_character() == iachar("$")
        end if

        call goto_character(beginning)
        at_beginning_of_line = is_beginning_of_line()
        call goto_character(original_point)

        if (do_macros) then
            if (parenthesis /= 0) then
                completion_suffix = achar(parenthesis)
            else if (allocated(completion_suffix)) then
                deallocate (completion_suffix)
            end if
        else if (at_beginning_of_line) then
            completion_suffix = ":"
        else
            completion_suffix = " "
        end if

        s => state()
        macros = names_list(s%macros)

        if (do_macros) then
            all = macros
        else
            targets = names_list(s%targets)
            n = list_length(targets)

            all_list: block
                type(value_type), allocatable :: items(:)
                allocate (items(n + list_length(macros)))

                do k = 1, n
                    items(k) = list_element(targets, k)
                end do

                do k = 1, list_length(macros)
                    items(n + k) = list_element(macros, k)
                end do

                all = make_list(items)
            end block all_list
        end if

        data%found = .true.
        data%start = beginning
        data%end = point()
        data%table%list = all
        if (allocated(completion_suffix)) data%exit_function => completion_exit
    end subroutine makefile_completions_at_point

    ! The exit function of `makefile_completions_at_point'.
    ! Port of make-mode.el:makefile-completions-at-point (its exit function).
    recursive subroutine completion_exit(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished
        type(status_type) :: st

        if (len(string) < 0) return
        if (finished /= COMPLETION_SOLE .and. finished /= COMPLETION_FINISHED) return

        if (looking_at(regular_expression_quote(completion_suffix), st)) then
            call goto_character(match_end(0, st))
        else
            call insert(completion_suffix, st)
        end if
    end subroutine completion_exit

    ! ------------------------------------------------------------------
    ! Backslashification
    ! ------------------------------------------------------------------

    !> Insert, align, or delete end-of-line backslashes on the lines in the region.
    !> With no argument, insert backslashes and align existing backslashes.
    !> With an argument, delete the backslashes.
    !>
    !> This function does not modify the last line of the region if the region ends
    !> right at the start of the following line; it does not modify blank lines
    !> at the start of the region.  So you can put the region around an entire macro
    !> definition and conveniently use this command.  Port of
    !> make-mode.el:makefile-backslash-region.
    subroutine makefile_backslash_region(from, to, delete_flag, status)
        integer(int64), intent(in) :: from, to
        logical, intent(in) :: delete_flag
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: endmark
        integer(int64) :: here, column, moved, adjusted, tw, next

        status = status_type()
        here = point()
        call goto_character(from)
        column = makefile_backslash_column%value
        call copy_marker_at(to, endmark, .false., status)

        ! Compute the smallest column number past the ends of all the lines.
        if (makefile_backslash_align%value .and. .not. delete_flag) then
            do while (point() < to)
                call end_of_line()

                if (preceding_character() == iachar("\")) then
                    call forward_character(status, -1_int64)
                    moved = skip_characters_backward(" "//TAB, status)
                end if

                column = max(column, 1 + current_column())
                moved = forward_line(1_int64)
            end do

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
                call makefile_append_backslash(column, status)
            else
                call makefile_delete_backslash(status)
            end if

            moved = forward_line(1_int64)
        end do

        call destroy_marker(endmark, st)
        call goto_character(here)
    end subroutine makefile_backslash_region

    !> Insert, align, or delete end-of-line backslashes on the lines in the
    !> region (the command).  Port of make-mode.el:makefile-backslash-region.
    subroutine execute_makefile_backslash_region(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: mark
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return
        ! (save-excursion ...)
        call copy_marker_at(point(), mark, .false., status)
        call makefile_backslash_region(b, e, current_prefix_argument%kind /= PREFIX_NONE, status)
        call goto_character(marked_position(mark))
        call destroy_marker(mark, st)
    end subroutine execute_makefile_backslash_region

    ! The width of the selected window (window-width).
    ! Not a port.
    integer function window_width()
        window_width = window_text_columns(frame_selected_window_reference())
    end function window_width

    ! The position of marker M.
    ! Not a port.
    integer(int64) function marked_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, marked_position, st)
    end function marked_position

    ! Port of make-mode.el:makefile-append-backslash.
    subroutine makefile_append_backslash(column, status)
        integer(int64), intent(in) :: column
        type(status_type), intent(out) :: status
        integer(int64) :: minimum, column_number

        call end_of_line()
        minimum = 1
        if (makefile_backslash_align%value) minimum = 0

        ! Note that "\\\\" is needed to get one backslash.
        if (preceding_character() == iachar("\")) then
            call forward_character(status, -1_int64)
            call delete_horizontal_space(.false., status)
            column_number = indent_to(column, status, minimum)
        else
            column_number = indent_to(column, status, minimum)
            call insert("\", status)
        end if
    end subroutine makefile_append_backslash

    ! Port of make-mode.el:makefile-delete-backslash.
    subroutine makefile_delete_backslash(status)
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
    end subroutine makefile_delete_backslash

    ! ------------------------------------------------------------------
    ! Filling
    ! ------------------------------------------------------------------

    !> Function used for `fill_paragraph_function' in Makefile mode.
    !> Fill comments, backslashed lines, and variable definitions specially.
    !> Port of make-mode.el:makefile-fill-paragraph.
    recursive subroutine makefile_fill_paragraph(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        type(handle_type) :: original_point
        type(status_type) :: st
        integer(int64) :: beginning, end, beginning_of_accessible_portion, end_of_accessible_portion_from_end, moved
        logical :: changed

        status = status_type()
        done = .true.
        if (present(justify)) continue
        ! (save-excursion)
        call copy_marker_at(point(), original_point, .false., status)
        if (.not. status%ok()) return
        call beginning_of_line()

        if (looking_at("^[ "//TAB//"]*#+\s-*", st)) then
            ! Found a comment.  Return nil to let normal filling take place.
            done = .false.
        else if (character_before_at(line_end_position(1_int64)) == iachar("\") &
                 .or. character_before_at(line_end_position(0_int64)) == iachar("\")) then
            ! Must look for backslashed-region before looking for variable
            ! assignment.
            ! A backslash region.  Find beginning and end, remove
            ! backslashes, fill, and then reapply backslashes.
            call end_of_line()
            beginning = point()
            call end_of_line(0_int64)

            do while (preceding_character() == iachar("\"))
                call end_of_line(0_int64)
            end do

            ! Maybe we hit is_beginning_of_buffer, in which case we are not at END_OF_LINE.
            if (is_end_of_line()) then
                beginning = point() + 1
            else
                beginning = point()
            end if

            call end_of_line()

            do while (preceding_character() == iachar("\") .and. .not. is_end_of_buffer())
                call end_of_line(2_int64)
            end do

            end = point()
            ! (save-restriction)
            beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
            end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
            call narrow_to_region(beginning, end, status)

            if (status%ok()) then
                call makefile_backslash_region(point_minimum(), point_maximum(), .true., status)
                ! Backslashed newlines are marked as punctuation, so when
                ! fill-delete-newlines turns the LF into SPC, we end up with spaces
                ! which back-to-indentation (called via fill-newline ->
                ! fill-indent-to-left-margin -> indent-line-to) thinks are real code
                ! (bug#13179).
                call remove_text_properties(point_minimum(), point_maximum(), list(value_type(name="syntax_table"), value_type()), &
                                            changed, st)
                ! Adjust fill-column to allow space for the backslash.
                call fill_one_less(status)
                if (status%ok()) call makefile_backslash_region(point_minimum(), point_maximum(), .false., status)
                call goto_character(point_maximum())
                moved = skip_characters_backward(LF, st)
                if (moved < 0) call delete_region(point(), point_maximum(), st)
                call widen()
                call narrow_to_region(beginning_of_accessible_portion, &
                                      buffers(current)%text%z - end_of_accessible_portion_from_end, st)
            end if
        else if (looking_at(makefile_macroassign_regular_expression, st)) then
            ! Have a macro assign.  Fill just this line, and then backslash
            ! resulting region.
            beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
            end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
            call narrow_to_region(point(), line_beginning_position(2_int64), status)

            if (status%ok()) then
                call fill_one_less(status)
                if (status%ok()) call makefile_backslash_region(point_minimum(), point_maximum(), .false., status)
                call widen()
                call narrow_to_region(beginning_of_accessible_portion, &
                                      buffers(current)%text%z - end_of_accessible_portion_from_end, st)
            end if
        end if

        call goto_character(marked_position(original_point))
        call destroy_marker(original_point, st)
    end subroutine makefile_fill_paragraph

    ! (let ((fill-paragraph-function nil) (fill-column (- fill-column 1)))
    !   (fill-paragraph nil))
    ! (Within `fill_paragraph_function', fill-paragraph doesn't call it.)
    ! Port of make-mode.el:makefile-fill-paragraph (in part).
    recursive subroutine fill_one_less(status)
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        saved = fill_column%value
        fill_column%value = saved - 1
        call fill_paragraph(status)
        fill_column%value = saved
    end subroutine fill_one_less

    ! ------------------------------------------------------------------
    ! Up-to-date overview buffer
    ! ------------------------------------------------------------------

    !> Create a buffer containing an overview of the state of all known targets.
    !> Known targets are targets that are explicitly defined in that makefile;
    !> in other words, all targets that appear on the left hand side of a
    !> dependency in the makefile.  Port of
    !> make-mode.el:makefile-create-up-to-date-overview.
    recursive subroutine makefile_create_up_to_date_overview(status)
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s
        type(string_type), allocatable :: saved_target_table(:), real_targets(:), prereqs(:)
        type(handle_type) :: this_buffer, up_to_date_buffer, b
        character(len=:), allocatable :: filename
        logical :: ok
        type(status_type) :: st

        call read_y_or_n("Are you sure that the makefile being edited is consistent? ", ok, status)
        if (.not. status%ok() .or. .not. ok) return
        ! The rest of this function operates on a temporary makefile, created by
        ! writing the current contents of the makefile buffer.
        s => state()
        if (allocated(s%targets)) saved_target_table = s%targets
        this_buffer = current_buffer()
        call get_or_make_buffer(makefile_up_to_date_buffer_name%value, up_to_date_buffer, status)
        if (.not. status%ok()) return
        call makefile_save_temporary(filename, status)
        if (.not. status%ok()) return
        ! Forget the target table because it may contain picked-up filenames
        ! that are not really targets in the current makefile.
        ! We don't want to query these, so get a new target-table with just the
        ! targets that can be found in the makefile buffer.
        ! The 'old' target table will be restored later.
        call makefile_pickup_targets(status)

        if (status%ok()) then
            s => state()
            real_targets = makefile_target_table()
            prereqs = makefile_has_prereqs()
            call set_buffer(up_to_date_buffer, status)
        end if

        if (status%ok()) then
            buffers(current)%read_only = .false.
            call erase_buffer(status)
            if (status%ok()) call makefile_query_targets(filename, real_targets, prereqs, status)

            ! if it did not get us anything
            if (status%ok() .and. buffer_size() == 0) then
                call close_buffer(current_buffer(), status)
                call message("No overview created!")
            end if

            call set_buffer(this_buffer, st)
            s => state()

            if (allocated(saved_target_table)) then
                s%targets = saved_target_table
            else if (allocated(s%targets)) then
                deallocate (s%targets)
            end if

            b = get_buffer(makefile_up_to_date_buffer_name%value)

            if (status%ok() .and. b%index /= 0) then
                call pop_to_buffer(b, status)

                if (status%ok()) then
                    call shrink_window_if_larger_than_buffer()
                    call sort_lines_in_buffer(status)
                    buffers(current)%read_only = .true.
                end if
            end if
        end if

        call delete_file(filename, st)
    end subroutine makefile_create_up_to_date_overview

    ! (sort-lines nil (point-min) (point-max)): sort.el is not ported, the
    ! lines are ordered as `is_string_less' orders them (sort-fold-case
    ! nil).
    ! Not a port.
    subroutine sort_lines_in_buffer(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: lines(:)
        type(string_type) :: temporary
        character(len=:), allocatable :: text
        integer :: i, j, n, k, start
        logical :: final_newline

        call buffer_substring(point_minimum(), point_maximum(), text, status)
        if (.not. status%ok()) return
        if (len(text) == 0) return
        final_newline = text(len(text):len(text)) == LF
        if (final_newline) text = text(1:len(text) - 1)
        n = 1

        do k = 1, len(text)
            if (text(k:k) == LF) n = n + 1
        end do

        allocate (lines(n))
        i = 0
        start = 1

        do k = 1, len(text) + 1
            if (k > len(text)) then
                i = i + 1
                lines(i)%text = text(start:)
            else if (text(k:k) == LF) then
                i = i + 1
                lines(i)%text = text(start:k - 1)
                start = k + 1
            end if
        end do

        do i = 2, n
            temporary = lines(i)
            j = i - 1

            do while (j >= 1)
                if (.not. is_string_less(temporary%text, lines(j)%text)) exit
                lines(j + 1) = lines(j)
                j = j - 1
            end do

            lines(j + 1) = temporary
        end do

        text = ""

        do i = 1, n
            text = text//lines(i)%text
            if (i < n .or. final_newline) text = text//LF
        end do

        call delete_region(point_minimum(), point_maximum(), status)
        if (status%ok()) call insert(text, status)
        call goto_character(point_minimum())
    end subroutine sort_lines_in_buffer

    !> Create a temporary file from the current makefile buffer.  Port of
    !> make-mode.el:makefile-save-temporary.
    subroutine makefile_save_temporary(filename, status)
        character(len=:), allocatable, intent(out) :: filename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: message_text

        call make_temporary_name("mktmp.", filename, status)
        if (.not. status%ok()) return
        call write_region(filename, .false., status, point_minimum(), point_maximum(), message=message_text)
    end subroutine makefile_save_temporary

    !> Fill the up-to-date overview buffer.
    !> Checks each target in TARGET_TABLE using
    !> `makefile_query_one_target_method_function'
    !> and generates the overview, one line per target name.  Port of
    !> make-mode.el:makefile-query-targets.
    subroutine makefile_query_targets(filename, target_table, prerequisites, status)
        character(len=*), intent(in) :: filename
        type(string_type), intent(in) :: target_table(:), prerequisites(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        logical :: no_prereqs, needs_rebuild
        integer :: k, j
        type(status_type) :: st

        text = ""

        do k = 1, size(target_table)
            no_prereqs = .true.

            do j = 1, size(prerequisites)
                if (prerequisites(j)%text == target_table(k)%text &
                    .and. len(prerequisites(j)%text) == len(target_table(k)%text)) no_prereqs = .false.
            end do

            needs_rebuild = no_prereqs
            if (.not. needs_rebuild) needs_rebuild = makefile_query_one_target_method_function(target_table(k)%text, filename)
            if (k > 1) text = text//LF

            if (no_prereqs) then
                text = text//TAB//target_table(k)%text//"  .. has no prerequisites"
            else if (needs_rebuild) then
                text = text//TAB//target_table(k)%text//"  .. NEEDS REBUILD"
            else
                text = text//TAB//target_table(k)%text//"  .. is up to date"
            end if
        end do

        call insert(text, status)
        call goto_character(point_minimum())
        ! remove the tmpfile
        call delete_file(filename, st)
    end subroutine makefile_query_targets

    !> Whether TARGET needs a rebuild, as `make -q' says with the makefile
    !> FILENAME.  Port of make-mode.el:makefile-query-by-make-minus-q.
    logical function makefile_query_by_make_minus_q(target, filename) result(rebuild)
        character(len=*), intent(in) :: target, filename
        type(value_type) :: result
        type(status_type) :: st

        call execute_process(makefile_brave_make%value, result, st, &
                             arguments=list(value_type(text="-f"), value_type(text=filename), value_type(text="-q"), &
                                            value_type(text=target)))
        rebuild = .true.
        if (vtag(result) == V_INTEGER) rebuild = xfixnum(result) /= 0
    end function makefile_query_by_make_minus_q

    ! ------------------------------------------------------------------
    ! Continuation cleanup and warnings of suspicious lines
    ! ------------------------------------------------------------------

    !> Remove the whitespace after trailing backslashes, when
    !> `makefile_cleanup_continuations' says so (a function of
    !> `write_file_functions').  Port of
    !> make-mode.el:makefile-cleanup-continuations.
    recursive subroutine makefile_cleanup_continuations_function(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, found
        type(status_type) :: st

        status = status_type()
        done = .false.
        if (.not. is_derived_mode("makefile_mode")) return
        if (.not. makefile_cleanup_continuations%value .or. buffers(current)%read_only) return
        original_point = point()
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("\\[ "//TAB//"]+$", found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            call delete_region(match_beginning(0, st) + 1, match_end(0, st), status)
            if (.not. status%ok()) exit
        end do

        call goto_character(min(original_point, point_maximum()))
    end subroutine makefile_cleanup_continuations_function

    !> Ask whether to save a Makefile with a suspicious line (one of only
    !> tabs, or spaces before a tab); DONE (canceling the save) if not
    !> (a function of `write_file_functions').  Port of
    !> make-mode.el:makefile-warn-suspicious-lines.
    recursive subroutine makefile_warn_suspicious_lines(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status

        call warn_lines("^\("//TAB//"+$\| +"//TAB//"\)", "Suspicious line ", done, status)
    end subroutine makefile_warn_suspicious_lines

    !> Ask whether to save a Makefile with whitespace after a trailing
    !> backslash; DONE (canceling the save) if not (a function of
    !> `write_file_functions').  Port of
    !> make-mode.el:makefile-warn-continuations.
    recursive subroutine makefile_warn_continuations(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status

        call warn_lines("\\[ "//TAB//"]+$", "Suspicious continuation in line ", done, status)
    end subroutine makefile_warn_continuations

    ! The body of the warnings: look for REGULAR_EXPRESSION and ask with PROMPT.
    ! Port of make-mode.el:makefile-warn-suspicious-lines (and makefile-warn-continuations).
    recursive subroutine warn_lines(regular_expression, prompt, done, status)
        character(len=*), intent(in) :: regular_expression, prompt
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, found
        type(status_type) :: st
        character(len=40) :: line_text
        logical :: ok

        status = status_type()
        done = .false.
        if (.not. is_derived_mode("makefile_mode")) return
        original_point = point()
        call goto_character(point_minimum())
        call regular_expression_search_forward(regular_expression, found, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            write (line_text, '(i0, ".  Save anyway?")') count_lines(point_minimum(), point())
            call read_y_or_n(prompt//trim(line_text), ok, status)
            done = .not. ok
        end if

        call goto_character(original_point)
    end subroutine warn_lines

    ! ------------------------------------------------------------------
    ! GNU make function support
    ! ------------------------------------------------------------------

    !> Insert a GNU make function call.
    !> Asks for the name of the function to use (with completion).
    !> Then prompts for all required parameters.  Port of
    !> make-mode.el:makefile-insert-gmake-function.
    recursive subroutine makefile_insert_gnu_make_function(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: name, arguments, argument
        integer :: k, j

        allocate (items(size(makefile_gnumake_functions)))

        do k = 1, size(items)
            items(k) = value_type(text=trim(makefile_gnumake_functions(k)%name))
        end do

        table%list = make_list(items)
        call completing_read("Function: ", table, name, status, require_match=1)
        if (.not. status%ok()) return
        if (len(name) == 0) return
        arguments = ""

        do k = 1, size(makefile_gnumake_functions)
            if (trim(makefile_gnumake_functions(k)%name) /= name) cycle

            do j = 1, size(makefile_gnumake_functions(k)%prompts)
                if (len_trim(makefile_gnumake_functions(k)%prompts(j)) == 0) exit
                call read_string("["//name//"] "//trim(makefile_gnumake_functions(k)%prompts(j))//": ", argument, status)
                if (.not. status%ok()) return
                if (j > 1) arguments = arguments//","
                arguments = arguments//argument
            end do

            exit
        end do

        call insert(makefile_format_macro_reference(name//" "//arguments)//" ", status)
    end subroutine makefile_insert_gnu_make_function

    ! ------------------------------------------------------------------
    ! Utility functions
    ! ------------------------------------------------------------------

    !> To be called as an anchored matcher by font-lock.
    !> The anchor must have matched the opening parens in the first group.
    !> Port of make-mode.el:makefile-match-function-end.
    recursive subroutine makefile_match_function_end(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s, closer
        integer(int64) :: f

        status = status_type()
        found = .false.
        if (limit < 0) return
        s = match_string_no_properties(1)

        ! FIXME forward-sexp or somesuch would be better?
        select case (s)
        case ("(")
            closer = ")"
        case ("{")
            closer = "}"
        case ("[")
            closer = "]"
        case ("((")
            closer = "))"
        case ("{{")
            closer = "}}"
        case ("[[")
            closer = "]]"
        case default
            return
        end select

        call regular_expression_search_forward("\(.*\)[ "//TAB//"]*"//closer, f, status, bound=line_end_position(), &
                                               noerror=SEARCH_NOERROR)
        found = status%ok() .and. f > 0
    end subroutine makefile_match_function_end

    !> Search for `makefile_dependency_regular_expression' up to LIMIT.
    !> Checks that the colon has not already been fontified, else we
    !> matched in a rule action.  Port of make-mode.el:makefile-match-dependency.
    recursive subroutine makefile_match_dependency(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call match_dependency(limit, found)
    end subroutine makefile_match_dependency

    ! makefile-match-dependency with BOUND (0 for nil).
    ! Port of make-mode.el:makefile-match-dependency.
    subroutine match_dependency(bound, found)
        integer(int64), intent(in) :: bound
        logical, intent(out) :: found
        integer(int64) :: point_value, limit, moved, deps_end, original_point
        integer(int64), allocatable :: data(:)
        type(status_type) :: st
        logical :: matched

        found = .false.
        point_value = point()

        do
            if (bound > 0) then
                limit = bound
                moved = skip_characters_forward(makefile_dependency_skip%value, st, bound)
            else
                limit = point_maximum()
                moved = skip_characters_forward(makefile_dependency_skip%value, st)
            end if

            if (.not. point() < limit) exit
            call goto_character(point() + 1)
            ! The GNU immediate assignment operator is ":=", while the
            ! POSIX operator is "::=".
            if (looking_at(":?=", st)) cycle
            if (has_face(point() - 1)) cycle

            if (line_beginning_position() > point_minimum() + 2) then
                if (character_before_at(line_end_position(0_int64)) == iachar("\")) cycle
            end if

            original_point = point()
            call beginning_of_line()
            matched = looking_at(makefile_dependency_regular_expression%value, st)

            if (matched) then
                deps_end = match_end(1, st)
                data = match_data()
                call goto_character(deps_end)
                moved = skip_characters_backward(" "//TAB, st)
                deps_end = point()
                call beginning_of_line()
                moved = skip_characters_forward(" "//TAB, st)
                ! Alter the bounds recorded for subexp 1,
                ! which is what is supposed to match the targets.
                data(3) = point()
                data(4) = deps_end
                call set_match_data(data)
                call goto_character(original_point)
                call end_of_line()
                found = .true.
                return
            end if

            call goto_character(original_point)
        end do

        call goto_character(point_value)
    end subroutine match_dependency

    !> Search for a rule action (`makefile_rule_action_regular_expression') up to LIMIT,
    !> not after a continued line.  Port of make-mode.el:makefile-match-action.
    recursive subroutine makefile_match_action(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: f

        status = status_type()
        found = .false.

        do
            call regular_expression_search_forward(makefile_rule_action_regular_expression%value, f, status, bound=limit, &
                                                   noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. f <= 0) return

            if (after_character_at(match_beginning(0, status) - 2) /= iachar("\")) then
                found = .true.
                return
            end if
        end do
    end subroutine makefile_match_action

    !> Insert a macro reference.  Port of
    !> make-mode.el:makefile-do-macro-insertion.
    subroutine makefile_do_macro_insertion(macro_name, status)
        character(len=*), intent(in) :: macro_name
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        if (len(macro_name) == 0) return

        do k = 1, size(makefile_runtime_macros)
            if (trim(makefile_runtime_macros(k)) == macro_name &
                .and. len_trim(makefile_runtime_macros(k)) == len(macro_name)) then
                call insert("$"//macro_name, status)
                return
            end if
        end do

        call insert(makefile_format_macro_reference(macro_name), status)
    end subroutine makefile_do_macro_insertion

    !> Remember a given target if it is not already remembered for this buffer.
    !> The result is true when HAS_PREREQS is (upstream's value).  Port of
    !> make-mode.el:makefile-remember-target.
    logical function makefile_remember_target(target_name, has_prereqs) result(value)
        character(len=*), intent(in) :: target_name
        logical, intent(in), optional :: has_prereqs
        type(make_state_type), pointer :: s

        value = .false.
        if (len(target_name) == 0) return
        s => state()
        if (.not. member_name(s%targets, target_name)) call prepend_string(s%targets, target_name)

        if (present(has_prereqs)) then
            if (has_prereqs) then
                call prepend_string(s%prereqs, target_name)
                value = .true.
            end if
        end if
    end function makefile_remember_target

    !> Remember a given macro if it is not already remembered for this buffer.
    !> The result is whether it was new.  Port of
    !> make-mode.el:makefile-remember-macro.
    logical function makefile_remember_macro(macro_name) result(value)
        character(len=*), intent(in) :: macro_name
        type(make_state_type), pointer :: s

        value = .false.
        if (len(macro_name) == 0) return
        s => state()

        if (.not. member_name(s%macros, macro_name)) then
            call prepend_string(s%macros, macro_name)
            value = .true.
        end if
    end function makefile_remember_macro

    !> Move point forward after inserting the terminating colon of a target.
    !> This acts according to the value of `makefile_tab_after_target_colon'.
    !> Port of make-mode.el:makefile-forward-after-target-colon.
    subroutine makefile_forward_after_target_colon(status)
        type(status_type), intent(out) :: status

        if (makefile_tab_after_target_colon%value) then
            call insert(TAB, status)
        else
            call insert(" ", status)
        end if
    end subroutine makefile_forward_after_target_colon

    !> Format a macro reference.
    !> Uses `makefile_use_curly_braces_for_macros'.  Port of
    !> make-mode.el:makefile-format-macro-ref.
    function makefile_format_macro_reference(macro_name) result(reference)
        character(len=*), intent(in) :: macro_name
        character(len=:), allocatable :: reference
        logical :: open

        open = .false.
        if (len(macro_name) > 0) open = macro_name(1:1) == "(" .or. macro_name(1:1) == "{"

        if (open) then
            reference = "$"//macro_name
        else if (makefile_use_curly_braces_for_macros%value) then
            reference = "${"//macro_name//"}"
        else
            reference = "$("//macro_name//")"
        end if
    end function makefile_format_macro_reference

    !> Return name of target or variable assignment that point is in.
    !> If it isn't in one, FOUND is unallocated.  Port of
    !> make-mode.el:makefile-add-log-defun.
    subroutine makefile_add_log_target(found)
        character(len=:), allocatable, intent(out) :: found
        integer(int64) :: original_point, shortage
        type(status_type) :: st
        integer :: k

        original_point = point()
        call beginning_of_line()

        ! Scan back line by line, noticing when we come to a
        ! variable or rule definition, and giving up when we see
        ! a line that is not part of either of those.
        do
            if (looking_at(makefile_macroassign_regular_expression, st)) then
                found = match_string_no_properties(1)
            else if (looking_at(makefile_dependency_regular_expression%value, st)) then
                found = match_string_no_properties(1)
            end if

            if (allocated(found)) exit
            ! Don't keep looking across a blank line or comment.
            if (looking_at("$\|#", st)) exit
            shortage = forward_line(-1_int64)
            if (shortage /= 0) exit
        end do

        ! Remove leading and trailing whitespace.
        if (allocated(found)) then
            k = verify(found, " "//TAB)

            if (k == 0) then
                found = ""
            else
                found = found(k:verify(found, " "//TAB, back=.true.))
            end if
        end if

        call goto_character(original_point)
    end subroutine makefile_add_log_target

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Whether the character at POSITION has a `face' property.
    ! Not a port.
    logical function has_face(position)
        integer(int64), intent(in) :: position
        type(value_type) :: face
        type(status_type) :: st

        has_face = .false.
        if (position < point_minimum()) return
        call get_text_property(position, value_type(name="face"), face, st)
        if (st%ok()) has_face = allocated(face%a)
    end function has_face

    ! The character after point, -1 if none.
    ! Not a port.
    integer function after_character()
        integer(int32) :: c
        logical :: found

        call character_after(c, found)
        after_character = -1
        if (found) after_character = c
    end function after_character

    ! The character after POSITION, -1 if none.
    ! Not a port.
    integer function after_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        after_character_at = -1
        if (position < point_minimum()) return
        call character_after(c, found, position)
        if (found) after_character_at = c
    end function after_character_at

    ! The character before point, -1 if none.
    ! Not a port.
    integer function before_character()
        integer(int32) :: c
        logical :: found

        call character_before(c, found)
        before_character = -1
        if (found) before_character = c
    end function before_character

    ! The character before POSITION, -1 if none.
    ! Not a port.
    integer function character_before_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        character_before_at = -1
        if (position <= point_minimum()) return
        call character_before(c, found, position)
        if (found) character_before_at = c
    end function character_before_at
end module make_mode_mod
