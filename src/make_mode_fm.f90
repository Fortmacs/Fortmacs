! make_mode_fm.f90 -- makefile editing commands for Fortmacs.
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
!> `makefile_gmake_mode', `makefile_makepp_mode', `makefile_bsdmake_mode'
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
!> `makefile_gnumake_functions_table' is an array of
!> `makefile_gnumake_function_type'.
!>
!> Deferred: the obsolete macro and target browser (C-c C-b,
!> `makefile_switch_to_browser' and the `makefile_browser_*' commands and
!> options, obsolete since 30.1), the Makefile menu, Imenu
!> (`makefile_imenu_generic_expression' and makepp's "Functions"),
!> `add_log_current_subprogram_function' (add-log's subprogram lookup is not ported;
!> `makefile_add_log_subprogram' is here), `cpp_font_lock_keywords' in
!> `makefile_imake_font_lock_keywords' (font-lock.el's C preprocessor
!> keywords), and the obsolete `makefile_generate_temporary_filename'.
module make_mode_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, fixnump, xfixnum, xstring, stringp, make_string, make_list, list, &
                       list_length, list_elt
    use lread_fm, only: sym
    use data_fm, only: integer_setting, logical_setting, string_setting, forward_setting, add_local_variables_killer
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, call_interactively
    use keyboard_fm, only: last_command_event
    use buffer_fm, only: buffers, cur, buffer_serial_live, barf_if_buffer_read_only, get_buffer, get_buffer_create, &
                         set_buffer, current_buffer, erase_buffer, kill_buffer
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use derived_fm, only: derived_mode_p
    use keymap_fm, only: keymap_set
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, syntax_table_live_p, skip_chars_forward, &
                         skip_chars_backward
    use editfns_fm, only: interactive_region, point, point_min, point_max, goto_char, bolp, eolp, bobp, eobp, char_after, &
                          char_before, preceding_char, buffer_substring, line_beginning_position, line_end_position, &
                          delete_region, narrow_to_region, widen, buffer_size
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line, self_insert_command
    use insdel_fm, only: insert
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use indent_fm, only: current_column, indent_to, indent_line_function, indent_to_left_margin, indent_tabs_mode
    use search_fm, only: looking_at, re_search_forward, string_match, match_beginning, match_end, match_data, &
                         set_match_data, regexp_quote, SEARCH_NOERROR
    use subr_fm, only: match_string_no_properties, y_or_n_p
    use simple_fm, only: delete_horizontal_space, count_lines, line_number_at_pos
    use textprop_fm, only: get_text_property, remove_text_properties
    use xdisp_fm, only: message, tab_width
    use window_fm, only: window_text_cols, pop_to_buffer, shrink_window_if_larger_than_buffer
    use frame_fm, only: frame_selected_window_ref
    use buffer_fm, only: fill_column
    use fill_fm, only: fill_paragraph, fill_paragraph_function
    use files_fm, only: write_file_functions, delete_file
    use fileio_fm, only: file_directory_p, file_name_directory, write_region, make_temp_name
    use dired_fm, only: file_name_all_completions
    use callproc_fm, only: call_process
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_syntax_entry_type, FONT_LOCK_OVERRIDE_T, FONT_LOCK_OVERRIDE_PREPEND, &
                            FONT_LOCK_OVERRIDE_APPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use regexp_opt_fm, only: regexp_opt, REGEXP_OPT_T
    use newcomment_fm, only: comment_start, comment_end, comment_start_skip
    use abbrev_fm, only: abbrev_table_type
    use dabbrev_fm, only: dabbrev_abbrev_skip_leading_regexp
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_BACKGROUND_LIGHT, &
                        FACE_BACKGROUND_DARK
    use minibuf_fm, only: completion_table_type, read_string
    use minibuf_table_fm, only: capf_data_type, COMPLETION_SOLE, COMPLETION_FINISHED
    use minibuffer_fm, only: completing_read, completion_at_point_functions
    use syntax_propertize_fm, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type
    implicit none(type, external)
    private

    public :: syms_of_make_mode, makefile_mode, makefile_automake_mode, makefile_gmake_mode, makefile_makepp_mode, &
              makefile_bsdmake_mode, makefile_imake_mode, makefile_next_dependency, makefile_previous_dependency, &
              makefile_electric_dot, makefile_insert_special_target, makefile_electric_equal, makefile_insert_macro, &
              makefile_insert_macro_ref, makefile_insert_target, makefile_insert_target_ref, makefile_electric_colon, &
              makefile_pickup_targets, makefile_pickup_macros, makefile_pickup_everything, &
              makefile_pickup_filenames_as_targets, makefile_completions_at_point, makefile_backslash_region, &
              makefile_fill_paragraph, makefile_create_up_to_date_overview, makefile_query_by_make_minus_q, &
              makefile_cleanup_continuations_function, makefile_warn_suspicious_lines, makefile_warn_continuations, &
              makefile_insert_gmake_function, makefile_match_function_end, makefile_match_dependency, &
              makefile_match_action, makefile_do_macro_insertion, makefile_remember_target, makefile_remember_macro, &
              makefile_format_macro_ref, makefile_add_log_subprogram, makefile_syntax_propertize_function, &
              makefile_make_font_lock_keywords, makefile_target_table, makefile_macro_table, makefile_has_prereqs, &
              makefile_query_proc

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
    type(string_setting), target, public :: makefile_target_colon = string_setting(name="makefile_target_colon")

    !> String to append to all macro names inserted by `makefile_insert_macro'.
    !> The normal value should be " = ", since this is what
    !> standard make expects.  However, newer makes such as dmake
    !> allow a larger variety of different macro assignments, so you
    !> might prefer to use " += " or " := " .  Port of make-mode.el
    !> makefile-macro-assign.
    type(string_setting), target, public :: makefile_macro_assign = string_setting(name="makefile_macro_assign")

    !> If true, Makefile mode should install electric keybindings.
    !> Default is .false..  Port of make-mode.el makefile-electric-keys.
    type(logical_setting), target, public :: makefile_electric_keys = &
                                             logical_setting(name="makefile_electric_keys", value=.false.)

    !> Controls the style of generated macro references.
    !> True means macro references should use curly braces, like `${this}'.
    !> .false. means use parentheses, like `$(this)'.  Port of make-mode.el
    !> makefile-use-curly-braces-for-macros-p.
    type(logical_setting), target, public :: makefile_use_curly_braces_for_macros_p = &
                                             logical_setting(name="makefile_use_curly_braces_for_macros_p", value=.false.)

    !> If true, insert a TAB after a target colon.
    !> Otherwise, a space is inserted.
    !> The default is .true..  Port of make-mode.el
    !> makefile-tab-after-target-colon.
    type(logical_setting), target, public :: makefile_tab_after_target_colon = &
                                             logical_setting(name="makefile_tab_after_target_colon", value=.true.)

    !> Column in which `makefile_backslash_region' inserts backslashes.
    !> Port of make-mode.el makefile-backslash-column.
    type(integer_setting), target, public :: makefile_backslash_column = &
                                             integer_setting(name="makefile_backslash_column", value=48)

    !> If true, `makefile_backslash_region' will align backslashes.
    !> Port of make-mode.el makefile-backslash-align.
    type(logical_setting), target, public :: makefile_backslash_align = &
                                             logical_setting(name="makefile_backslash_align", value=.true.)

    !> If true, `makefile_pickup_everything' picks up filenames as targets.
    !> This means it calls `makefile_pickup_filenames_as_targets'.
    !> Otherwise filenames are omitted.  Port of make-mode.el
    !> makefile-pickup-everything-picks-up-filenames-p.
    type(logical_setting), target, public :: makefile_pickup_everything_picks_up_filenames_p = &
                                             logical_setting(name="makefile_pickup_everything_picks_up_filenames_p", &
                                                             value=.false.)

    !> If true, automatically clean up continuation lines when saving.
    !> A line is cleaned up by removing all whitespace following a trailing
    !> backslash.  This is done silently.
    !> IMPORTANT: Please note that enabling this option causes Makefile mode
    !> to MODIFY A FILE WITHOUT YOUR CONFIRMATION when "it seems necessary".
    !> Port of make-mode.el makefile-cleanup-continuations.
    type(logical_setting), target, public :: makefile_cleanup_continuations = &
                                             logical_setting(name="makefile_cleanup_continuations", value=.false.)

    !> Normal hook run by `makefile_mode'.  Port of make-mode.el
    !> makefile-mode-hook.
    type(hook_type), target, public :: makefile_mode_hook = hook_type(name="makefile_mode_hook")
    !> Normal hook run by `makefile_automake_mode'.
    type(hook_type), target, public :: makefile_automake_mode_hook = hook_type(name="makefile_automake_mode_hook")
    !> Normal hook run by `makefile_gmake_mode'.
    type(hook_type), target, public :: makefile_gmake_mode_hook = hook_type(name="makefile_gmake_mode_hook")
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
    character(len=16), allocatable, public :: makefile_special_targets_list(:)

    !> List of macros that are resolved by make at runtime.
    !> If you insert a macro reference using `makefile_insert_macro_ref', the name
    !> of the macro is checked against this list.  If it can be found its name will
    !> not be enclosed in { } or ( ).  Port of make-mode.el
    !> makefile-runtime-macros-list.
    character(len=8), allocatable, public :: makefile_runtime_macros_list(:)

    !> Regex used to find dependency lines in a makefile.  Port of
    !> make-mode.el makefile-dependency-regex.
    type(string_setting), target, public :: makefile_dependency_regex = string_setting(name="makefile_dependency_regex")

    !> Regex used to find dependency lines in a BSD makefile.  Port of
    !> make-mode.el makefile-bsdmake-dependency-regex.
    character(len=:), allocatable, public :: makefile_bsdmake_dependency_regex

    !> Characters to skip to find a line that might be a dependency.
    !> Port of make-mode.el makefile-dependency-skip.
    type(string_setting), target, public :: makefile_dependency_skip = string_setting(name="makefile_dependency_skip")

    !> Regex used to highlight rule action lines in font lock mode.
    !> Port of make-mode.el makefile-rule-action-regex.
    type(string_setting), target, public :: makefile_rule_action_regex = string_setting(name="makefile_rule_action_regex")

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> Regex used to highlight makepp rule action lines in font lock mode.
    !> Port of make-mode.el makefile-makepp-rule-action-regex.
    character(len=*), parameter, public :: makefile_makepp_rule_action_regex = &
                                           "^"//TAB//"[ "//TAB//"]*\(\(?:\(?:noecho\|ignore[-_]error\|[-@]+\)[ "//TAB// &
                                           "]*\)*\)\(\(&\S +\)?\(?:.*\\"//LF//"\)*.*\)"

    !> Regex used to highlight BSD rule action lines in font lock mode.
    !> Port of make-mode.el makefile-bsdmake-rule-action-regex.
    character(len=:), allocatable, public :: makefile_bsdmake_rule_action_regex

    !> Regex used to find macro assignment lines in a makefile.  Port of
    !> make-mode.el makefile-macroassign-regex.
    character(len=*), parameter, public :: makefile_macroassign_regex = &
                                           "\(?:^\|^export\|^override\|:\|:[ "//TAB//"]*override\)[ "//TAB//"]*\([^ "//LF// &
                                           TAB//"][^:#= "//TAB//LF//"]*\)[ "//TAB//"]*\(?:!=\|[*:+]?[:?]?=\)"

    !> Regex used to find $(macro) uses in a makefile.  Port of
    !> make-mode.el makefile-var-use-regex.
    character(len=*), parameter, public :: makefile_var_use_regex = &
                                           "\(^\|[^$]\)\$[({]\([-a-zA-Z0-9_.]+\|[@%<?^+*][FD]?\)"

    !> Regex for filenames that will NOT be included in the target list.
    !> Port of make-mode.el makefile-ignored-files-in-pickup-regex.
    character(len=*), parameter, public :: makefile_ignored_files_in_pickup_regex = &
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
    character(len=*), parameter, public :: makefile_gmake_statements(*) = &
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
    type(font_lock_keyword_type), allocatable, public :: makefile_gmake_font_lock_keywords(:)
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
        logical function makefile_query_proc(target, filename)
            implicit none(type, external)
            character(len=*), intent(in) :: target, filename
        end function makefile_query_proc
    end interface

    !> How to invoke make, for `makefile_query_targets'.
    !> This should identify a `make' command that can handle the `-q' option.
    !> Port of make-mode.el makefile-brave-make.
    type(string_setting), target, public :: makefile_brave_make = string_setting(name="makefile_brave_make")

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
    procedure(makefile_query_proc), pointer, public :: makefile_query_one_target_method_function => null()

    !> Name of the Up-to-date overview buffer.  Port of make-mode.el
    !> makefile-up-to-date-buffer-name.
    type(string_setting), target, public :: makefile_up_to_date_buffer_name = &
                                            string_setting(name="makefile_up_to_date_buffer_name")

    !> Abbrev table in use in Makefile buffers.  Port of make-mode.el
    !> makefile-mode-abbrev-table.
    type(abbrev_table_type), target, public :: makefile_mode_abbrev_table

    !> The keymap that is used in Makefile mode.  Port of make-mode.el
    !> makefile-mode-map.
    type(handle_type), public :: makefile_mode_map

    !> Syntax table used in `makefile_mode'.  Port of make-mode.el
    !> makefile-mode-syntax-table.
    type(handle_type), public :: makefile_mode_syntax_table
    !> Syntax table used in `makefile_imake_mode'.  Port of make-mode.el
    !> makefile-imake-mode-syntax-table.
    type(handle_type), public :: makefile_imake_mode_syntax_table

    !> A GNU make function of `makefile_gnumake_functions_table': its NAME and
    !> the PROMPTS for its required arguments (blank ones are unused).
    type, public :: makefile_gnumake_function_type
        character(len=10) :: name = ""
        character(len=11) :: prompts(6) = ""
    end type makefile_gnumake_function_type

    !> Table of GNU Make functions and their arguments.  Port of
    !> make-mode.el makefile-gnumake-functions-alist.
    type(makefile_gnumake_function_type), allocatable, public :: makefile_gnumake_functions_table(:)

    !> A target or macro name of the tables.
    type, public :: makefile_name_type
        character(len=:), allocatable :: name
    end type makefile_name_type

    ! The tables of known targets and macros (makefile-target-table,
    ! makefile-macro-table, newest first), makefile-has-prereqs and
    ! whether they must be picked up again (makefile-need-target-pickup,
    ! makefile-need-macro-pickup): the default values, and Makefile mode
    ! buffers' own.
    type :: make_state_type
        integer(int64) :: serial = 0
        type(makefile_name_type), allocatable :: targets(:), macros(:), prereqs(:)
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
    subroutine syms_of_make_mode()
        type(status_type) :: st

        if (syntax_table_live_p(makefile_mode_syntax_table)) return
        call forward_setting(makefile_target_colon)
        call forward_setting(makefile_macro_assign)
        call forward_setting(makefile_electric_keys)
        call forward_setting(makefile_use_curly_braces_for_macros_p)
        call forward_setting(makefile_tab_after_target_colon)
        call forward_setting(makefile_backslash_column)
        call forward_setting(makefile_backslash_align)
        call forward_setting(makefile_pickup_everything_picks_up_filenames_p)
        call forward_setting(makefile_cleanup_continuations)
        call forward_setting(makefile_dependency_regex)
        call forward_setting(makefile_dependency_skip)
        call forward_setting(makefile_rule_action_regex)
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

        if (.not. allocated(makefile_rule_action_regex%value)) then
            makefile_rule_action_regex = "^"//TAB//"[ "//TAB//"]*\(?:\([-@]+\)[ "//TAB//"]*\)\(.*\(?:\\"//LF//".*\)*\)"
        end if

        makefile_bsdmake_rule_action_regex = string_replace("-@", "-+@", makefile_rule_action_regex%value)

        if (.not. allocated(makefile_dependency_regex%value)) then
            ! Allow for two nested levels $(v1:$(v2:$(v3:a=b)=c)=d)
            makefile_dependency_regex = "^\("//elems_re(3, .true.)//"\)\(:\)\(?:[ "//TAB//"]*$\|[^="//LF// &
                                        "]\(?:[^#"//LF//"]*?;[ "//TAB//"]*\(.+\)\)?\)"
        end if

        makefile_bsdmake_dependency_regex = replace_first("\(:\)", "\([:!]\)", makefile_dependency_regex%value)

        if (.not. allocated(makefile_special_targets_list)) then
            makefile_special_targets_list = [character(len=16) :: &
                                             "DEFAULT", "DONE", "ERROR", "EXPORT", &
                                             "FAILED", "GROUPEPILOG", "GROUPPROLOG", "IGNORE", &
                                             "IMPORT", "INCLUDE", "INCLUDEDIRS", "INIT", &
                                             "KEEP_STATE", "MAKEFILES", "MAKE_VERSION", "NO_PARALLEL", &
                                             "PARALLEL", "PHONY", "PRECIOUS", "REMOVE", &
                                             "SCCS_GET", "SILENT", "SOURCE", "SUFFIXES", &
                                             "WAIT", "c.o", "C.o", "m.o", &
                                             "el.elc", "y.c", "s.o"]
        end if

        if (.not. allocated(makefile_runtime_macros_list)) then
            makefile_runtime_macros_list = [character(len=8) :: "@", "&", ">", "<", "*", "^", "+", "?", "%", "$"]
        end if

        call init_gnumake_functions()
        call define_faces()
        call init_syntax_tables()
        call init_keywords()
        call init_rules()
        call makefile_mode_abbrev_table%define_table("makefile_mode_abbrev_table")
        call add_local_variables_killer(kill_make_locals)
        call major_modes%makefile_mode%define("makefile_mode", "Makefile", makefile_mode, makefile_mode_hook, &
                                              parent=major_modes%prog_mode, syntax_table=makefile_mode_syntax_table, &
                                              abbrev_table=makefile_mode_abbrev_table)
        makefile_mode_map = major_modes%makefile_mode%map
        call major_modes%makefile_automake_mode%define("makefile_automake_mode", "Makefile.am", makefile_automake_mode, &
                                                       makefile_automake_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_gmake_mode%define("makefile_gmake_mode", "GNUmakefile", makefile_gmake_mode, &
                                                    makefile_gmake_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_makepp_mode%define("makefile_makepp_mode", "Makeppfile", makefile_makepp_mode, &
                                                     makefile_makepp_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_bsdmake_mode%define("makefile_bsdmake_mode", "BSDmakefile", makefile_bsdmake_mode, &
                                                      makefile_bsdmake_mode_hook, parent=major_modes%makefile_mode)
        call major_modes%makefile_imake_mode%define("makefile_imake_mode", "Imakefile", makefile_imake_mode, &
                                                    makefile_imake_mode_hook, parent=major_modes%makefile_mode, &
                                                    syntax_table=makefile_imake_mode_syntax_table)
        call keymap_set(makefile_mode_map, "C-c :", "makefile_insert_target_ref", st)
        call keymap_set(makefile_mode_map, "C-c C-f", "makefile_pickup_filenames_as_targets", st)
        call keymap_set(makefile_mode_map, "C-c C-c", "comment_region", st)
        call keymap_set(makefile_mode_map, "C-c C-p", "makefile_pickup_everything", st)
        call keymap_set(makefile_mode_map, "C-c C-u", "makefile_create_up_to_date_overview", st)
        call keymap_set(makefile_mode_map, "C-c TAB", "makefile_insert_gmake_function", st)
        call keymap_set(makefile_mode_map, "C-c C-\", "makefile_backslash_region", st)
        call keymap_set(makefile_mode_map, "C-c RET C-a", "makefile_automake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET C-b", "makefile_bsdmake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET C-g", "makefile_gmake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET TAB", "makefile_imake_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET RET", "makefile_mode", st)
        call keymap_set(makefile_mode_map, "C-c RET C-p", "makefile_makepp_mode", st)
        call keymap_set(makefile_mode_map, "M-p", "makefile_previous_dependency", st)
        call keymap_set(makefile_mode_map, "M-n", "makefile_next_dependency", st)
        call keymap_set(makefile_mode_map, "M-q", "fill_paragraph", st)
        call keymap_set(makefile_mode_map, "C-M-i", "completion_at_point", st)
        commands%makefile_mode%run => makefile_mode
        commands%makefile_automake_mode%run => makefile_automake_mode
        commands%makefile_gmake_mode%run => makefile_gmake_mode
        commands%makefile_makepp_mode%run => makefile_makepp_mode
        commands%makefile_bsdmake_mode%run => makefile_bsdmake_mode
        commands%makefile_imake_mode%run => makefile_imake_mode
        commands%makefile_next_dependency%run => cmd_makefile_next_dependency
        commands%makefile_previous_dependency%run => cmd_makefile_previous_dependency
        commands%makefile_electric_dot%run => cmd_makefile_electric_dot
        commands%makefile_insert_special_target%run => makefile_insert_special_target
        commands%makefile_electric_equal%run => cmd_makefile_electric_equal
        commands%makefile_insert_macro%run => cmd_makefile_insert_macro
        commands%makefile_insert_macro_ref%run => cmd_makefile_insert_macro_ref
        commands%makefile_insert_target%run => cmd_makefile_insert_target
        commands%makefile_insert_target_ref%run => cmd_makefile_insert_target_ref
        commands%makefile_electric_colon%run => cmd_makefile_electric_colon
        commands%makefile_pickup_targets%run => makefile_pickup_targets
        commands%makefile_pickup_macros%run => makefile_pickup_macros
        commands%makefile_pickup_everything%run => cmd_makefile_pickup_everything
        commands%makefile_pickup_filenames_as_targets%run => makefile_pickup_filenames_as_targets
        commands%makefile_backslash_region%run => cmd_makefile_backslash_region
        commands%makefile_create_up_to_date_overview%run => makefile_create_up_to_date_overview
        commands%makefile_insert_gmake_function%run => makefile_insert_gmake_function
    end subroutine syms_of_make_mode

    !> One level of makefile-dependency-regex's elems-re.
    !> Port of make-mode.el:makefile-dependency-regex (its elems-re).
    recursive function elems_re(n, outer) result(re)
        integer, intent(in) :: n
        logical, intent(in) :: outer
        character(len=:), allocatable :: re

        if (n < 1) then
            re = "[^"//LF//"$#})]+?"
        else if (outer) then
            re = "\(?:\$\(?:[({]"//elems_re(n - 1, .false.)//"[})]\|[^({]\)\|[^"//LF//"$#"//TAB//":=]\)+?"
        else
            re = "\(?:\$\(?:[({]"//elems_re(n - 1, .false.)//"[})]\|[^({]\)\|[^"//LF//"$#)}]\)+?"
        end if
    end function elems_re

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
    subroutine init_gnumake_functions()
        integer :: k

        if (allocated(makefile_gnumake_functions_table)) return
        allocate (makefile_gnumake_functions_table(39))
        k = 0
        ! Functions for String Substitution and Analysis
        call add("subst", [character(len=11) :: "From", "To", "In"])
        call add("patsubst", [character(len=11) :: "Pattern", "Replacement", "In"])
        call add("strip", [character(len=11) :: "Text"])
        call add("findstring", [character(len=11) :: "Find what", "In"])
        call add("filter", [character(len=11) :: "Pattern", "Text"])
        call add("filter-out", [character(len=11) :: "Pattern", "Text"])
        call add("sort", [character(len=11) :: "List"])
        call add("word", [character(len=11) :: "Index", "Text"])
        call add("wordlist", [character(len=11) :: "S", "E", "Text"])
        call add("words", [character(len=11) :: "Text"])
        call add("firstword", [character(len=11) :: "Text"])
        call add("lastword", [character(len=11) :: "Names"])
        ! Functions for File Names
        call add("dir", [character(len=11) :: "Names"])
        call add("notdir", [character(len=11) :: "Names"])
        call add("suffix", [character(len=11) :: "Names"])
        call add("basename", [character(len=11) :: "Names"])
        call add("addsuffix", [character(len=11) :: "Suffix", "Names"])
        call add("addprefix", [character(len=11) :: "Prefix", "Names"])
        call add("join", [character(len=11) :: "List 1", "List 2"])
        call add("wildcard", [character(len=11) :: "Pattern"])
        call add("realpath", [character(len=11) :: "Names"])
        call add("abspath", [character(len=11) :: "Names"])
        ! Functions for Conditionals
        call add("if", [character(len=11) :: "Condition", "Then-part", "Else-part"])
        call add("or", [character(len=11) :: "Condition 1", "Condition 2", "Condition 3", "Condition 4"])
        call add("and", [character(len=11) :: "Condition 1", "Condition 2", "Condition 3", "Condition 4"])
        ! Misc functions
        call add("foreach", [character(len=11) :: "Variable", "List", "Text"])
        call add("file", [character(len=11) :: "Op", "Filename", "Text"])
        call add("call", [character(len=11) :: "Variable", "Param 1", "Param 2", "Param 3", "Param 4", "Param 5"])
        call add("value", [character(len=11) :: "Variable"])
        call add("eval", [character(len=11) :: "statement"])
        call add("origin", [character(len=11) :: "Variable"])
        call add("flavor", [character(len=11) :: "Variable"])
        call add("shell", [character(len=11) :: "Command"])
        call add("guile", [character(len=11) :: "Program"])
        ! Functions that control make
        call add("error", [character(len=11) :: "Text"])
        call add("warning", [character(len=11) :: "Text"])
        call add("info", [character(len=11) :: "Text"])

    contains

        subroutine add(name, prompts)
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: prompts(:)

            k = k + 1
            makefile_gnumake_functions_table(k)%name = name
            makefile_gnumake_functions_table(k)%prompts(1:size(prompts)) = prompts
        end subroutine add
    end subroutine init_gnumake_functions

    ! The faces.
    ! Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK

        f => makefile_space
        call f%define("makefile_space", "Face to use for highlighting leading spaces in Font-Lock mode.")
        call f%spec_entry(display_when(classes=[C]), face_attrs(background="hotpink"))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        f => makefile_targets
        ! This needs to go along both with foreground and background colors (i.e. shell)
        call f%define("makefile_targets", "Face to use for additionally highlighting rule targets in Font-Lock mode.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => makefile_shell
        call f%define("makefile_shell", "Face to use for additionally highlighting Shell commands in Font-Lock mode.")
        call f%spec_entry(display_when(), face_attrs())
        f => makefile_makepp_perl
        call f%define("makefile_makepp_perl", "Face to use for additionally highlighting Perl code in Font-Lock mode.")
        ! Camel Book
        call f%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attrs(background="LightBlue1"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[DARK]), face_attrs(background="DarkBlue"))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true.))
    end subroutine define_faces

    ! Not a port.
    subroutine init_syntax_tables()
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
    end subroutine init_syntax_tables

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
    !> the variable references matching VAR, automatic variables, the
    !> statements KEYWORDS (none if absent; with DASH, a `-' in them
    !> alternately means `_'), the NEGATION regexp, with SPACE suspicious
    !> whitespace, FL_KEYWORDS and dependencies.  Port of
    !> make-mode.el:makefile-make-font-lock-keywords.
    function makefile_make_font_lock_keywords(var, space, keywords, negation, fl_keywords, dash) result(kw)
        character(len=*), intent(in) :: var
        logical, intent(in) :: space
        character(len=*), intent(in), optional :: keywords(:)
        character(len=*), intent(in), optional :: negation
        type(font_lock_keyword_type), intent(in), optional :: fl_keywords(:)
        logical, intent(in), optional :: dash
        type(font_lock_keyword_type), allocatable :: kw(:)
        character(len=*), parameter :: VA = "font_lock_variable_name_face", NEG = "font_lock_negation_char_face"
        type(font_lock_keyword_type) :: all(64)
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3)
        character(len=:), allocatable :: re
        integer :: n, k
        logical :: d

        n = 0
        ! Do macro assignments.  These get the "variable-name" face.
        h3(1) = h(1, VA)
        ! This is for after !=
        h3(2) = h(2, "makefile_shell", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        ! This is for after normal assignment
        h3(3) = h(3, "font_lock_string_face", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        call push(font_lock_keyword(makefile_macroassign_regex, highlights=h3))
        ! Rule actions.
        ! FIXME: When this spans multiple lines we need font-lock-multiline.
        h3(1) = h(1, "font_lock_type_face", lax=.true.)
        h3(2) = h(2, "makefile_shell", FONT_LOCK_OVERRIDE_PREPEND)
        ! Only makepp has builtin commands.
        h3(3) = h(3, "font_lock_builtin_face", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        call push(font_lock_keyword(matcher=makefile_match_action, highlights=h3))
        ! Variable references even in targets/strings/comments.
        h1(1) = h(2, VA, FONT_LOCK_OVERRIDE_PREPEND)
        call push(font_lock_keyword(var, highlights=h1))
        ! Automatic variable references and single character variable references,
        ! but not shell variables references.
        h1(1) = h(1, "font_lock_constant_face", FONT_LOCK_OVERRIDE_PREPEND)
        call push(font_lock_keyword("[^$]\$\([@%<?^+*_]\|[a-zA-Z0-9]\>\)", highlights=h1))
        h1(1) = h(1, "makefile_targets", FONT_LOCK_OVERRIDE_APPEND)
        call push(font_lock_keyword("[^$]\(\$[@%*]\)", highlights=h1))

        if (present(keywords)) then
            ! Fontify conditionals and includes.
            d = .false.
            if (present(dash)) d = dash

            if (d) then
                re = string_replace("-", "[_-]", regexp_opt(keywords, REGEXP_OPT_T))
            else
                re = regexp_opt(keywords, REGEXP_OPT_T)
            end if

            h2(1) = h(1, "font_lock_keyword_face")
            h2(2) = h(2, VA)
            call push(font_lock_keyword("^\(?: [ "//TAB//"]*\)?"//string_replace(" ", "[ "//TAB//"]+", re)//"\>[ "//TAB// &
                                        "]*\([^: "//TAB//LF//"#]*\)", highlights=h2))
        end if

        if (present(negation)) then
            h2(1) = h(1, NEG, FONT_LOCK_OVERRIDE_PREPEND)
            h2(2) = h(2, NEG, FONT_LOCK_OVERRIDE_PREPEND, .true.)
            call push(font_lock_keyword(negation, highlights=h2))
        end if

        if (space) then
            ! Highlight lines that contain just whitespace.
            ! They can cause trouble, especially if they start with a tab.
            call push(font_lock_keyword("^[ "//TAB//"]+$", face="makefile_space"))
            ! Highlight shell comments that Make treats as commands,
            ! since these can fool people.
            h1(1) = h(0, "makefile_space", FONT_LOCK_OVERRIDE_T)
            call push(font_lock_keyword("^"//TAB//"+#", highlights=h1))
            ! Highlight spaces that precede tabs.
            ! They can make a tab fail to be effective.
            h1(1) = h(1, "makefile_space")
            call push(font_lock_keyword("^\( +\)"//TAB, highlights=h1))
        end if

        if (present(fl_keywords)) then
            do k = 1, size(fl_keywords)
                call push(fl_keywords(k))
            end do
        end if

        ! Do dependencies.
        h2(1) = h(1, "makefile_targets", FONT_LOCK_OVERRIDE_PREPEND)
        h2(2) = h(3, "makefile_shell", FONT_LOCK_OVERRIDE_PREPEND, .true.)
        call push(font_lock_keyword(matcher=makefile_match_dependency, highlights=h2))
        allocate (kw(n))
        kw(:) = all(1:n)

    contains

        subroutine push(k)
            type(font_lock_keyword_type), intent(in) :: k

            n = n + 1
            all(n) = k
        end subroutine push
    end function makefile_make_font_lock_keywords

    ! Not a port.
    subroutine init_keywords()
        character(len=*), parameter :: KW = "font_lock_keyword_face", FN = "font_lock_function_name_face", &
                                       VA = "font_lock_variable_name_face", TY = "font_lock_type_face", &
                                       PERL = "makefile_makepp_perl", SH = "makefile_shell", TG = "makefile_targets"
        integer, parameter :: PREPEND = FONT_LOCK_OVERRIDE_PREPEND, APPEND = FONT_LOCK_OVERRIDE_APPEND, &
                              T = FONT_LOCK_OVERRIDE_T
        type(font_lock_keyword_type) :: extra(16)
        type(font_lock_keyword_type), allocatable :: base(:)
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3), a(1)
        integer :: n

        makefile_font_lock_keywords = makefile_make_font_lock_keywords(makefile_var_use_regex, .true., makefile_statements)
        makefile_automake_font_lock_keywords = makefile_make_font_lock_keywords(makefile_var_use_regex, .true., &
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
        h1(1) = h(1, SH, PREPEND, .true.)
        a(1) = font_lock_anchored(h1, matcher=makefile_match_function_end)
        extra(4) = font_lock_keyword("[^$]\$\([({]\)shell[ "//TAB//"]+", highlights=a)
        makefile_gmake_font_lock_keywords = makefile_make_font_lock_keywords(makefile_var_use_regex, .true., &
                                                                             makefile_gmake_statements, &
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
        h1(1) = h(1, SH, PREPEND, .true.)
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
            makefile_make_font_lock_keywords(makefile_var_use_regex, .false., makefile_makepp_statements, &
                                             "^\(?: [ "//TAB//"]*\)?\(?:and[ "//TAB//"]+\|else[ "//TAB//"]+\|or[ "//TAB// &
                                             "]+\)?if\(n\)\(?:def\|eq\|sys\|true\)\>", extra(1:11), dash=.true.)
        ! BSD make.  A lot more could be done for variables here.
        h1(1) = h(1, KW)
        extra(1) = font_lock_keyword("^[ "//TAB//"]*\.for[ "//TAB//"].+[ "//TAB//"]\(in\)\>", highlights=h1)
        base = makefile_make_font_lock_keywords(makefile_var_use_regex, .true., &
                                                negation="^\(?: [ "//TAB//"]*\)?\.\(?:el\)?if\(n?\)\(?:def\|make\)?\>[ " &
                                                //TAB//"]*\(!?\)", fl_keywords=extra(1:1))
        n = size(base)
        allocate (makefile_bsdmake_font_lock_keywords(n + 1))
        makefile_bsdmake_font_lock_keywords(1:n) = base
        makefile_bsdmake_font_lock_keywords(n + 1) = font_lock_keyword("^\. *"//regexp_opt(makefile_bsdmake_statements) &
                                                                       //"\>", face=KW)
        ! Imake.
        n = size(makefile_font_lock_keywords)
        allocate (makefile_imake_font_lock_keywords(n + 3))
        makefile_imake_font_lock_keywords(1) = font_lock_keyword("XCOMM.*$", face="font_lock_comment_face")
        h1(1) = h(0, KW, PREPEND)
        makefile_imake_font_lock_keywords(2) = font_lock_keyword("XVAR\(?:use\|def\)[0-9]", highlights=h1)
        makefile_imake_font_lock_keywords(3) = font_lock_keyword("@@", face="font_lock_preprocessor_face")
        makefile_imake_font_lock_keywords(4:) = makefile_font_lock_keywords
    end subroutine init_keywords

    ! The rules of `makefile_syntax_propertize_function'.
    ! Not a port.
    subroutine init_rules()
        type(syntax_highlight_type) :: hl

        if (allocated(propertize_rules%rules)) return
        allocate (propertize_rules%rules(2))
        ! From sh-script.el.
        ! A `#' begins a comment in sh when it is unquoted and at the beginning
        ! of a word.  In the shell, words are separated by metacharacters.
        ! The list of special chars is taken from the single-unix spec of the
        ! shell command language (under `quoting') but with `$' removed.
        propertize_rules%rules(1)%regexp = "[^|&;<>()`\""' "//TAB//LF//"]\(#+\)"
        hl = syntax_highlight_type(group=1, syntax="_")
        propertize_rules%rules(1)%highlights = [hl]
        ! Change the syntax of a quoted newline so that it does not end a comment.
        propertize_rules%rules(2)%regexp = "\\"//LF
        hl = syntax_highlight_type(group=0, syntax=".")
        propertize_rules%rules(2)%highlights = [hl]
    end subroutine init_rules

    !> The `syntax_propertize_function' of Makefile mode.  Port of
    !> make-mode.el makefile-syntax-propertize-function.
    recursive subroutine makefile_syntax_propertize_function(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call goto_char(start)
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
        if (cur == 0 .or. .not. allocated(locals)) return

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) then
                s => locals(k)
                return
            end if
        end do
    end function state

    ! Give the current buffer its own tables (make-local-variable: with
    ! the default values).
    ! Not a port.
    subroutine make_local()
        type(make_state_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) return
        end do

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) then
                locals(k) = default_state
                locals(k)%serial = buffers(cur)%serial
                return
            end if
        end do

        n = size(locals)
        allocate (grown(n + 1))

        do k = 1, n
            grown(k) = locals(k)
        end do

        grown(n + 1) = default_state
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, locals)
    end subroutine make_local

    ! kill-all-local-variables: forget the current buffer's tables.
    ! Not a port.
    subroutine kill_make_locals()
        integer :: k

        if (.not. allocated(locals)) return

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) locals(k)%serial = 0
        end do
    end subroutine kill_make_locals

    ! NAMES with NAME added in front (cons).
    ! Not a port.
    subroutine push_front(names, name)
        type(makefile_name_type), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: name
        type(makefile_name_type), allocatable :: grown(:)
        integer :: k, n

        n = 0
        if (allocated(names)) n = size(names)
        allocate (grown(n + 1))
        grown(1)%name = name

        do k = 1, n
            grown(k + 1) = names(k)
        end do

        call move_alloc(grown, names)
    end subroutine push_front

    ! Whether NAME is one of NAMES (assoc).
    ! Not a port.
    logical function member_name(names, name) result(found)
        type(makefile_name_type), allocatable, intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: k

        found = .false.
        if (.not. allocated(names)) return

        do k = 1, size(names)
            if (names(k)%name == name .and. len(names(k)%name) == len(name)) then
                found = .true.
                return
            end if
        end do
    end function member_name

    ! The empty table.
    ! Not a port.
    subroutine clear_names(names)
        type(makefile_name_type), allocatable, intent(inout) :: names(:)

        if (allocated(names)) deallocate (names)
        allocate (names(0))
    end subroutine clear_names

    !> Table of all target names known for this buffer, newest first.
    !> Port of make-mode.el makefile-target-table.
    function makefile_target_table() result(names)
        type(makefile_name_type), allocatable :: names(:)
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
        type(makefile_name_type), allocatable :: names(:)
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
        type(makefile_name_type), allocatable :: names(:)
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
        type(makefile_name_type), allocatable, intent(in) :: names(:)
        type(value_type) :: v
        type(value_type), allocatable :: items(:)
        integer :: k

        if (.not. allocated(names)) then
            v = nil()
            return
        end if

        allocate (items(size(names)))

        do k = 1, size(names)
            items(k) = make_string(names(k)%name)
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
            call keymap_set(makefile_mode_map, "$", "makefile_insert_macro_ref", st)
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
    !> variants `makefile_automake_mode', `makefile_gmake_mode',
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
    !> `makefile_pickup_everything_picks_up_filenames_p':
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
    !> `makefile_special_targets_list':
    !>    List of special targets.  You will be offered to complete
    !>    on one of those in the minibuffer whenever you enter a `.'.
    !>    at the beginning of a line in Makefile mode.
    !>
    !> Port of make-mode.el:makefile-mode.
    recursive subroutine makefile_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(makefile_mode_syntax_table)) call syms_of_make_mode()
        call load_make_mode()
        call major_modes%makefile_mode%begin(saved, status)
        if (.not. status%ok()) return
        call completion_at_point_functions%add(makefile_completions_at_point, local=.true.)
        call write_file_functions%add(makefile_warn_suspicious_lines, local=.true.)
        call write_file_functions%add(makefile_warn_continuations, local=.true.)
        call write_file_functions%add(makefile_cleanup_continuations_function, local=.true.)
        call make_local()
        ! Font lock.
        call set_font_lock_defaults(makefile_font_lock_keywords)
        call syntax_propertize_function%set_local(makefile_syntax_propertize_function)
        ! Dabbrev.
        call dabbrev_abbrev_skip_leading_regexp%set_local("\$")
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
        call major_modes%makefile_mode%finish(saved, status)
    end subroutine makefile_mode

    !> An adapted `makefile_mode' that knows about automake.  Port of
    !> make-mode.el:makefile-automake-mode.
    recursive subroutine makefile_automake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(makefile_mode_syntax_table)) call syms_of_make_mode()
        call major_modes%makefile_automake_mode%begin(saved, status)
        if (.not. status%ok()) return
        call set_font_lock_defaults(makefile_automake_font_lock_keywords)
        call major_modes%makefile_automake_mode%finish(saved, status)
    end subroutine makefile_automake_mode

    !> An adapted `makefile_mode' that knows about gmake.  Port of
    !> make-mode.el:makefile-gmake-mode.
    recursive subroutine makefile_gmake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(makefile_mode_syntax_table)) call syms_of_make_mode()
        call major_modes%makefile_gmake_mode%begin(saved, status)
        if (.not. status%ok()) return
        call set_font_lock_defaults(makefile_gmake_font_lock_keywords)
        call major_modes%makefile_gmake_mode%finish(saved, status)
    end subroutine makefile_gmake_mode

    !> An adapted `makefile_mode' that knows about makepp.  Port of
    !> make-mode.el:makefile-makepp-mode.
    recursive subroutine makefile_makepp_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(makefile_mode_syntax_table)) call syms_of_make_mode()
        call major_modes%makefile_makepp_mode%begin(saved, status)
        if (.not. status%ok()) return
        call makefile_rule_action_regex%set_local(makefile_makepp_rule_action_regex)
        call set_font_lock_defaults(makefile_makepp_font_lock_keywords)
        call major_modes%makefile_makepp_mode%finish(saved, status)
    end subroutine makefile_makepp_mode

    !> An adapted `makefile_mode' that knows about BSD make.  Port of
    !> make-mode.el:makefile-bsdmake-mode.
    recursive subroutine makefile_bsdmake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(makefile_mode_syntax_table)) call syms_of_make_mode()
        call major_modes%makefile_bsdmake_mode%begin(saved, status)
        if (.not. status%ok()) return
        call makefile_dependency_regex%set_local(makefile_bsdmake_dependency_regex)
        call makefile_dependency_skip%set_local("^:!")
        call makefile_rule_action_regex%set_local(makefile_bsdmake_rule_action_regex)
        call set_font_lock_defaults(makefile_bsdmake_font_lock_keywords)
        call major_modes%makefile_bsdmake_mode%finish(saved, status)
    end subroutine makefile_bsdmake_mode

    !> An adapted `makefile_mode' that knows about imake.  Port of
    !> make-mode.el:makefile-imake-mode.
    recursive subroutine makefile_imake_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(makefile_mode_syntax_table)) call syms_of_make_mode()
        call major_modes%makefile_imake_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! (setq-local syntax-propertize-function nil)
        call syntax_propertize_function%kill_local()
        call set_font_lock_defaults(makefile_imake_font_lock_keywords)
        call comment_start%set_local("XCOMM")
        call comment_start_skip%set_local("XCOMM[ "//TAB//"]*")
        call major_modes%makefile_imake_mode%finish(saved, status)
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
            call goto_char(here)
        end if
    end subroutine makefile_next_dependency

    !> Move point to the beginning of the next dependency line.  Port of
    !> make-mode.el:makefile-next-dependency.
    subroutine cmd_makefile_next_dependency(status)
        type(status_type), intent(out) :: status
        logical :: found

        status = status_type()
        call makefile_next_dependency(found)
    end subroutine cmd_makefile_next_dependency

    !> Move point to the beginning of the previous dependency line (FOUND,
    !> if there is one).  Port of make-mode.el:makefile-previous-dependency.
    subroutine makefile_previous_dependency(found)
        logical, intent(out) :: found
        integer(int64) :: pt, moved
        type(status_type) :: st
        logical :: is_equal

        pt = point()
        call beginning_of_line()
        found = .false.

        ! makefile-match-dependency done backwards:
        do
            moved = skip_chars_backward(makefile_dependency_skip%value, st)
            if (bobp()) exit
            is_equal = after_char() == iachar("=")
            call goto_char(point() - 1)
            if (is_equal) cycle
            if (has_face(point())) cycle
            call beginning_of_line()

            if (point() > point_min() + 2) then
                if (char_before_at(point() - 1) == iachar("\")) cycle
            end if

            if (looking_at(makefile_dependency_regex%value, st)) then
                found = .true.
                return
            end if
        end do

        call goto_char(pt)
    end subroutine makefile_previous_dependency

    !> Move point to the beginning of the previous dependency line.  Port
    !> of make-mode.el:makefile-previous-dependency.
    subroutine cmd_makefile_previous_dependency(status)
        type(status_type), intent(out) :: status
        logical :: found

        status = status_type()
        call makefile_previous_dependency(found)
    end subroutine cmd_makefile_previous_dependency

    ! ------------------------------------------------------------------
    ! Electric keys
    ! ------------------------------------------------------------------

    ! The character of the key that invoked the command.
    ! Not a port.
    integer(int64) function event_char() result(c)
        c = -1
        if (fixnump(last_command_event)) c = xfixnum(last_command_event)
    end function event_char

    !> Prompt for the name of a special target to insert.
    !> Only does electric insertion at beginning of line.
    !> Anywhere else just self-inserts.  Port of
    !> make-mode.el:makefile-electric-dot.
    recursive subroutine makefile_electric_dot(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        if (bolp()) then
            call makefile_insert_special_target(status)
        else
            call self_insert_command(arg, event_char(), status)
        end if
    end subroutine makefile_electric_dot

    !> Prompt for the name of a special target to insert (the command).
    recursive subroutine cmd_makefile_electric_dot(status)
        type(status_type), intent(out) :: status

        call makefile_electric_dot(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_makefile_electric_dot

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
        allocate (items(size(makefile_special_targets_list)))

        do k = 1, size(items)
            items(k) = make_string(trim(makefile_special_targets_list(k)))
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
    recursive subroutine makefile_electric_equal(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call makefile_pickup_macros(status)
        if (.not. status%ok()) return

        if (bolp()) then
            call call_interactively(sym("makefile_insert_macro"), status)
        else
            call self_insert_command(arg, event_char(), status)
        end if
    end subroutine makefile_electric_equal

    !> Prompt for name of a macro to insert (the command).
    recursive subroutine cmd_makefile_electric_equal(status)
        type(status_type), intent(out) :: status

        call makefile_electric_equal(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_makefile_electric_equal

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
    recursive subroutine cmd_makefile_insert_macro(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Macro Name: ", name, status)
        if (.not. status%ok()) return
        call makefile_insert_macro(name, status)
    end subroutine cmd_makefile_insert_macro

    !> Complete on a list of known macros, then insert complete ref at point.
    !> Port of make-mode.el:makefile-insert-macro-ref.
    recursive subroutine makefile_insert_macro_ref(macro_name, status)
        character(len=*), intent(in) :: macro_name
        type(status_type), intent(out) :: status

        call makefile_do_macro_insertion(macro_name, status)
    end subroutine makefile_insert_macro_ref

    !> Complete on a list of known macros, then insert complete ref at point
    !> (the command).
    recursive subroutine cmd_makefile_insert_macro_ref(status)
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
        call makefile_insert_macro_ref(name, status)
    end subroutine cmd_makefile_insert_macro_ref

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
    recursive subroutine cmd_makefile_insert_target(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_string("Target: ", name, status)
        if (.not. status%ok()) return
        call makefile_insert_target(name, status)
    end subroutine cmd_makefile_insert_target

    !> Complete on a list of known targets, then insert TARGET_NAME at point.
    !> Port of make-mode.el:makefile-insert-target-ref.
    recursive subroutine makefile_insert_target_ref(target_name, status)
        character(len=*), intent(in) :: target_name
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(target_name) == 0) return
        call insert(target_name//" ", status)
    end subroutine makefile_insert_target_ref

    !> Complete on a list of known targets, then insert it at point (the
    !> command).
    recursive subroutine cmd_makefile_insert_target_ref(status)
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
        call makefile_insert_target_ref(name, status)
    end subroutine cmd_makefile_insert_target_ref

    !> Prompt for name of new target.
    !> Prompting only happens at beginning of line.
    !> Anywhere else just self-inserts.  Port of
    !> make-mode.el:makefile-electric-colon.
    recursive subroutine makefile_electric_colon(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        if (bolp()) then
            call call_interactively(sym("makefile_insert_target"), status)
        else
            call self_insert_command(arg, event_char(), status)
        end if
    end subroutine makefile_electric_colon

    !> Prompt for name of new target (the command).
    recursive subroutine cmd_makefile_electric_colon(status)
        type(status_type), intent(out) :: status

        call makefile_electric_colon(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_makefile_electric_colon

    ! ------------------------------------------------------------------
    ! Extracting targets and macros from an existing makefile
    ! ------------------------------------------------------------------

    !> Notice names of all target definitions in Makefile.  Port of
    !> make-mode.el:makefile-pickup-targets.
    recursive subroutine makefile_pickup_targets(status)
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s
        character(len=:), allocatable :: target_name
        integer(int64) :: opoint, start, moved, shortage
        logical :: found, has_prereqs
        type(status_type) :: st

        status = status_type()
        s => state()
        if (.not. s%need_target_pickup) return
        s%need_target_pickup = .false.
        call clear_names(s%targets)
        call clear_names(s%prereqs)
        opoint = point()
        call goto_char(point_min())

        do
            call match_dependency(0_int64, found)
            if (.not. found) exit
            call goto_char(match_beginning(1, st))

            do
                start = point()
                moved = skip_chars_forward("^ "//TAB//":#", st)
                call buffer_substring(start, point(), target_name, st)
                has_prereqs = .not. looking_at(":["//TAB//" ]*$", st)

                if (makefile_remember_target(target_name, has_prereqs)) then
                    call message("Picked up target """//target_name//""" from line "//itoa(line_number_at_pos()))
                end if

                moved = skip_chars_forward(" "//TAB, st)
                if (eolp()) exit
                if (after_char() == iachar(":")) exit
            end do

            shortage = forward_line()
        end do

        call goto_char(opoint)
        call message("Read targets OK")
    end subroutine makefile_pickup_targets

    !> Notice names of all macro definitions in Makefile.  Port of
    !> make-mode.el:makefile-pickup-macros.
    recursive subroutine makefile_pickup_macros(status)
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s
        character(len=:), allocatable :: macro_name
        integer(int64) :: opoint, start, moved, shortage, found
        type(status_type) :: st

        status = status_type()
        s => state()
        if (.not. s%need_macro_pickup) return
        s%need_macro_pickup = .false.
        call clear_names(s%macros)
        opoint = point()
        call goto_char(point_min())

        do
            call re_search_forward(makefile_macroassign_regex, found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            call goto_char(match_beginning(1, st))
            start = point()
            moved = skip_chars_forward("^ "//TAB//":#=*", st)
            call buffer_substring(start, point(), macro_name, st)

            if (makefile_remember_macro(macro_name)) then
                call message("Picked up macro """//macro_name//""" from line "//itoa(line_number_at_pos()))
            end if

            shortage = forward_line()
        end do

        call goto_char(opoint)
        call message("Read macros OK")
    end subroutine makefile_pickup_macros

    !> Notice names of all macros and targets in Makefile.
    !> Prefix argument ARG means force pickups to be redone.  Port of
    !> make-mode.el:makefile-pickup-everything.
    recursive subroutine makefile_pickup_everything(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        type(make_state_type), pointer :: s

        if (arg) then
            s => state()
            s%need_target_pickup = .true.
            s%need_macro_pickup = .true.
        end if

        call makefile_pickup_macros(status)
        if (.not. status%ok()) return
        call makefile_pickup_targets(status)
        if (.not. status%ok()) return
        if (makefile_pickup_everything_picks_up_filenames_p%value) call makefile_pickup_filenames_as_targets(status)
    end subroutine makefile_pickup_everything

    !> Notice names of all macros and targets in Makefile (the command).
    recursive subroutine cmd_makefile_pickup_everything(status)
        type(status_type), intent(out) :: status

        call makefile_pickup_everything(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_makefile_pickup_everything

    !> Scan the current directory for filenames to use as targets.
    !> Checks each filename against `makefile_ignored_files_in_pickup_regex'
    !> and adds all qualifying names to the list of known targets.  Port of
    !> make-mode.el:makefile-pickup-filenames-as-targets.
    recursive subroutine makefile_pickup_filenames_as_targets(status)
        type(status_type), intent(out) :: status
        type(value_type) :: names
        character(len=:), allocatable :: name, dir
        integer(int64) :: k
        type(status_type) :: st

        if (.not. allocated(buffers(cur)%filename)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp")
            return
        end if

        dir = file_name_directory(buffers(cur)%filename)
        call file_name_all_completions("", dir, names, status)
        if (.not. status%ok()) return

        do k = 1, list_length(names)
            name = xstring(list_elt(names, k))
            if (file_directory_p(name)) cycle
            if (string_match(makefile_ignored_files_in_pickup_regex, name, st) > 0) cycle
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
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        integer(int64) :: beg, opoint, moved
        integer :: pc, paren
        logical :: do_macros, at_bol
        type(make_state_type), pointer :: s
        type(value_type) :: targets, macros, all
        integer(int64) :: k, n

        status = status_type()
        opoint = point()
        moved = skip_chars_backward("^$(){}:#= "//TAB//LF, status)
        beg = point()
        paren = 0
        do_macros = .false.
        pc = before_char()

        if (pc == iachar("$")) then
            ! Preceding "$" means macros only.
            do_macros = .true.
        else if (pc == iachar("{") .or. pc == iachar("(")) then
            ! Preceding "$(" or "${" means macros only.
            if (pc == iachar("{")) then
                paren = iachar("}")
            else
                paren = iachar(")")
            end if

            call goto_char(point() - 1)
            do_macros = before_char() == iachar("$")
        end if

        call goto_char(beg)
        at_bol = bolp()
        call goto_char(opoint)

        if (do_macros) then
            if (paren /= 0) then
                completion_suffix = achar(paren)
            else if (allocated(completion_suffix)) then
                deallocate (completion_suffix)
            end if
        else if (at_bol) then
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
                    items(k) = list_elt(targets, k)
                end do

                do k = 1, list_length(macros)
                    items(n + k) = list_elt(macros, k)
                end do

                all = make_list(items)
            end block all_list
        end if

        data%found = .true.
        data%start = beg
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

        if (looking_at(regexp_quote(completion_suffix), st)) then
            call goto_char(match_end(0, st))
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
        call goto_char(from)
        column = makefile_backslash_column%value
        call copy_marker_at(to, endmark, .false., status)

        ! Compute the smallest column number past the ends of all the lines.
        if (makefile_backslash_align%value .and. .not. delete_flag) then
            do while (point() < to)
                call end_of_line()

                if (preceding_char() == iachar("\")) then
                    call forward_char(status, -1_int64)
                    moved = skip_chars_backward(" "//TAB, status)
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
        call goto_char(from)

        do while (point() < mpos(endmark) .and. eolp())
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
                call makefile_append_backslash(column, status)
            else
                call makefile_delete_backslash(status)
            end if

            moved = forward_line(1_int64)
        end do

        call free_marker(endmark, st)
        call goto_char(here)
    end subroutine makefile_backslash_region

    !> Insert, align, or delete end-of-line backslashes on the lines in the
    !> region (the command).  Port of make-mode.el:makefile-backslash-region.
    subroutine cmd_makefile_backslash_region(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(handle_type) :: mark
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return
        ! (save-excursion ...)
        call copy_marker_at(point(), mark, .false., status)
        call makefile_backslash_region(b, e,.not. nilp(current_prefix_arg), status)
        call goto_char(mpos(mark))
        call free_marker(mark, st)
    end subroutine cmd_makefile_backslash_region

    ! The width of the selected window (window-width).
    ! Not a port.
    integer function window_width()
        window_width = window_text_cols(frame_selected_window_ref())
    end function window_width

    ! The position of marker M.
    ! Not a port.
    integer(int64) function mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, mpos, st)
    end function mpos

    ! Port of make-mode.el:makefile-append-backslash.
    subroutine makefile_append_backslash(column, status)
        integer(int64), intent(in) :: column
        type(status_type), intent(out) :: status
        integer(int64) :: minimum, col

        call end_of_line()
        minimum = 1
        if (makefile_backslash_align%value) minimum = 0

        ! Note that "\\\\" is needed to get one backslash.
        if (preceding_char() == iachar("\")) then
            call forward_char(status, -1_int64)
            call delete_horizontal_space(.false., status)
            col = indent_to(column, status, minimum)
        else
            col = indent_to(column, status, minimum)
            call insert("\", status)
        end if
    end subroutine makefile_append_backslash

    ! Port of make-mode.el:makefile-delete-backslash.
    subroutine makefile_delete_backslash(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved, here

        status = status_type()
        call end_of_line()
        if (bolp()) return
        call forward_char(status, -1_int64)

        if (looking_at("\\", status)) then
            here = point() + 1
            moved = skip_chars_backward(" "//TAB, status)
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
        type(handle_type) :: opoint
        type(status_type) :: st
        integer(int64) :: beginning, end, begv, zv_from_end, moved
        logical :: changed

        status = status_type()
        done = .true.
        if (present(justify)) continue
        ! (save-excursion)
        call copy_marker_at(point(), opoint, .false., status)
        if (.not. status%ok()) return
        call beginning_of_line()

        if (looking_at("^[ "//TAB//"]*#+\s-*", st)) then
            ! Found a comment.  Return nil to let normal filling take place.
            done = .false.
        else if (char_before_at(line_end_position(1_int64)) == iachar("\") &
                 .or. char_before_at(line_end_position(0_int64)) == iachar("\")) then
            ! Must look for backslashed-region before looking for variable
            ! assignment.
            ! A backslash region.  Find beginning and end, remove
            ! backslashes, fill, and then reapply backslashes.
            call end_of_line()
            beginning = point()
            call end_of_line(0_int64)

            do while (preceding_char() == iachar("\"))
                call end_of_line(0_int64)
            end do

            ! Maybe we hit bobp, in which case we are not at EOL.
            if (eolp()) then
                beginning = point() + 1
            else
                beginning = point()
            end if

            call end_of_line()

            do while (preceding_char() == iachar("\") .and. .not. eobp())
                call end_of_line(2_int64)
            end do

            end = point()
            ! (save-restriction)
            begv = buffers(cur)%begv
            zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
            call narrow_to_region(beginning, end, status)

            if (status%ok()) then
                call makefile_backslash_region(point_min(), point_max(), .true., status)
                ! Backslashed newlines are marked as punctuation, so when
                ! fill-delete-newlines turns the LF into SPC, we end up with spaces
                ! which back-to-indentation (called via fill-newline ->
                ! fill-indent-to-left-margin -> indent-line-to) thinks are real code
                ! (bug#13179).
                call remove_text_properties(point_min(), point_max(), list(sym("syntax_table"), nil()), changed, st)
                ! Adjust fill-column to allow space for the backslash.
                call fill_one_less(status)
                if (status%ok()) call makefile_backslash_region(point_min(), point_max(), .false., status)
                call goto_char(point_max())
                moved = skip_chars_backward(LF, st)
                if (moved < 0) call delete_region(point(), point_max(), st)
                call widen()
                call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
            end if
        else if (looking_at(makefile_macroassign_regex, st)) then
            ! Have a macro assign.  Fill just this line, and then backslash
            ! resulting region.
            begv = buffers(cur)%begv
            zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
            call narrow_to_region(point(), line_beginning_position(2_int64), status)

            if (status%ok()) then
                call fill_one_less(status)
                if (status%ok()) call makefile_backslash_region(point_min(), point_max(), .false., status)
                call widen()
                call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
            end if
        end if

        call goto_char(mpos(opoint))
        call free_marker(opoint, st)
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
        type(makefile_name_type), allocatable :: saved_target_table(:), real_targets(:), prereqs(:)
        type(handle_type) :: this_buffer, up_to_date_buffer, b
        character(len=:), allocatable :: filename
        logical :: ok
        type(status_type) :: st

        call y_or_n_p("Are you sure that the makefile being edited is consistent? ", ok, status)
        if (.not. status%ok() .or. .not. ok) return
        ! The rest of this function operates on a temporary makefile, created by
        ! writing the current contents of the makefile buffer.
        s => state()
        if (allocated(s%targets)) saved_target_table = s%targets
        this_buffer = current_buffer()
        call get_buffer_create(makefile_up_to_date_buffer_name%value, up_to_date_buffer, status)
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
            buffers(cur)%read_only = .false.
            call erase_buffer(status)
            if (status%ok()) call makefile_query_targets(filename, real_targets, prereqs, status)

            ! if it did not get us anything
            if (status%ok() .and. buffer_size() == 0) then
                call kill_buffer(current_buffer(), status)
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
                    buffers(cur)%read_only = .true.
                end if
            end if
        end if

        call delete_file(filename, st)
    end subroutine makefile_create_up_to_date_overview

    ! (sort-lines nil (point-min) (point-max)): sort.el is not ported, the
    ! lines are ordered as `string_lessp' orders them (sort-fold-case
    ! nil).
    ! Not a port.
    subroutine sort_lines_in_buffer(status)
        type(status_type), intent(out) :: status
        type(makefile_name_type), allocatable :: lines(:)
        type(makefile_name_type) :: tmp
        character(len=:), allocatable :: text
        integer :: i, j, n, k, start
        logical :: final_newline

        call buffer_substring(point_min(), point_max(), text, status)
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
                lines(i)%name = text(start:)
            else if (text(k:k) == LF) then
                i = i + 1
                lines(i)%name = text(start:k - 1)
                start = k + 1
            end if
        end do

        do i = 2, n
            tmp = lines(i)
            j = i - 1

            do while (j >= 1)
                if (.not. lessp(tmp%name, lines(j)%name)) exit
                lines(j + 1) = lines(j)
                j = j - 1
            end do

            lines(j + 1) = tmp
        end do

        text = ""

        do i = 1, n
            text = text//lines(i)%name
            if (i < n .or. final_newline) text = text//LF
        end do

        call delete_region(point_min(), point_max(), status)
        if (status%ok()) call insert(text, status)
        call goto_char(point_min())

    contains

        ! string-lessp on UTF-8 bytes (the order of the characters).
        logical function lessp(a, b)
            character(len=*), intent(in) :: a, b
            integer :: m

            do m = 1, min(len(a), len(b))
                if (a(m:m) /= b(m:m)) then
                    lessp = iachar(a(m:m)) < iachar(b(m:m))
                    return
                end if
            end do

            lessp = len(a) < len(b)
        end function lessp
    end subroutine sort_lines_in_buffer

    !> Create a temporary file from the current makefile buffer.  Port of
    !> make-mode.el:makefile-save-temporary.
    subroutine makefile_save_temporary(filename, status)
        character(len=:), allocatable, intent(out) :: filename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: msg

        call make_temp_name("mktmp.", filename, status)
        if (.not. status%ok()) return
        call write_region(filename, .false., status, point_min(), point_max(), message=msg)
    end subroutine makefile_save_temporary

    !> Fill the up-to-date overview buffer.
    !> Checks each target in TARGET_TABLE using
    !> `makefile_query_one_target_method_function'
    !> and generates the overview, one line per target name.  Port of
    !> make-mode.el:makefile-query-targets.
    subroutine makefile_query_targets(filename, target_table, prereq_list, status)
        character(len=*), intent(in) :: filename
        type(makefile_name_type), intent(in) :: target_table(:), prereq_list(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        logical :: no_prereqs, needs_rebuild
        integer :: k, j
        type(status_type) :: st

        text = ""

        do k = 1, size(target_table)
            no_prereqs = .true.

            do j = 1, size(prereq_list)
                if (prereq_list(j)%name == target_table(k)%name &
                    .and. len(prereq_list(j)%name) == len(target_table(k)%name)) no_prereqs = .false.
            end do

            needs_rebuild = no_prereqs
            if (.not. needs_rebuild) needs_rebuild = makefile_query_one_target_method_function(target_table(k)%name, filename)
            if (k > 1) text = text//LF

            if (no_prereqs) then
                text = text//TAB//target_table(k)%name//"  .. has no prerequisites"
            else if (needs_rebuild) then
                text = text//TAB//target_table(k)%name//"  .. NEEDS REBUILD"
            else
                text = text//TAB//target_table(k)%name//"  .. is up to date"
            end if
        end do

        call insert(text, status)
        call goto_char(point_min())
        ! remove the tmpfile
        call delete_file(filename, st)
    end subroutine makefile_query_targets

    !> Whether TARGET needs a rebuild, as `make -q' says with the makefile
    !> FILENAME.  Port of make-mode.el:makefile-query-by-make-minus-q.
    logical function makefile_query_by_make_minus_q(target, filename) result(rebuild)
        character(len=*), intent(in) :: target, filename
        type(value_type) :: result
        type(status_type) :: st

        call call_process(makefile_brave_make%value, result, st, &
                          args=list(make_string("-f"), make_string(filename), make_string("-q"), make_string(target)))
        rebuild = .true.
        if (fixnump(result)) rebuild = xfixnum(result) /= 0
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
        integer(int64) :: opoint, found
        type(status_type) :: st

        status = status_type()
        done = .false.
        if (.not. derived_mode_p("makefile_mode")) return
        if (.not. makefile_cleanup_continuations%value .or. buffers(cur)%read_only) return
        opoint = point()
        call goto_char(point_min())

        do
            call re_search_forward("\\[ "//TAB//"]+$", found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            call delete_region(match_beginning(0, st) + 1, match_end(0, st), status)
            if (.not. status%ok()) exit
        end do

        call goto_char(min(opoint, point_max()))
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

    ! The body of the warnings: look for REGEXP and ask with PROMPT.
    ! Port of make-mode.el:makefile-warn-suspicious-lines (and makefile-warn-continuations).
    recursive subroutine warn_lines(regexp, prompt, done, status)
        character(len=*), intent(in) :: regexp, prompt
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, found
        type(status_type) :: st
        logical :: ok

        status = status_type()
        done = .false.
        if (.not. derived_mode_p("makefile_mode")) return
        opoint = point()
        call goto_char(point_min())
        call re_search_forward(regexp, found, st, noerror=SEARCH_NOERROR)

        if (st%ok() .and. found > 0) then
            call y_or_n_p(prompt//itoa(count_lines(point_min(), point()))//".  Save anyway?", ok, status)
            done = .not. ok
        end if

        call goto_char(opoint)
    end subroutine warn_lines

    ! ------------------------------------------------------------------
    ! GNU make function support
    ! ------------------------------------------------------------------

    !> Insert a GNU make function call.
    !> Asks for the name of the function to use (with completion).
    !> Then prompts for all required parameters.  Port of
    !> make-mode.el:makefile-insert-gmake-function.
    recursive subroutine makefile_insert_gmake_function(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: name, args, arg
        integer :: k, j

        allocate (items(size(makefile_gnumake_functions_table)))

        do k = 1, size(items)
            items(k) = make_string(trim(makefile_gnumake_functions_table(k)%name))
        end do

        table%list = make_list(items)
        call completing_read("Function: ", table, name, status, require_match=1)
        if (.not. status%ok()) return
        if (len(name) == 0) return
        args = ""

        do k = 1, size(makefile_gnumake_functions_table)
            if (trim(makefile_gnumake_functions_table(k)%name) /= name) cycle

            do j = 1, size(makefile_gnumake_functions_table(k)%prompts)
                if (len_trim(makefile_gnumake_functions_table(k)%prompts(j)) == 0) exit
                call read_string("["//name//"] "//trim(makefile_gnumake_functions_table(k)%prompts(j))//": ", arg, status)
                if (.not. status%ok()) return
                if (j > 1) args = args//","
                args = args//arg
            end do

            exit
        end do

        call insert(makefile_format_macro_ref(name//" "//args)//" ", status)
    end subroutine makefile_insert_gmake_function

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

        call re_search_forward("\(.*\)[ "//TAB//"]*"//closer, f, status, bound=line_end_position(), noerror=SEARCH_NOERROR)
        found = status%ok() .and. f > 0
    end subroutine makefile_match_function_end

    !> Search for `makefile_dependency_regex' up to LIMIT.
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
        integer(int64) :: pt, limit, moved, deps_end, opoint
        integer(int64), allocatable :: data(:)
        type(status_type) :: st
        logical :: matched

        found = .false.
        pt = point()

        do
            if (bound > 0) then
                limit = bound
                moved = skip_chars_forward(makefile_dependency_skip%value, st, bound)
            else
                limit = point_max()
                moved = skip_chars_forward(makefile_dependency_skip%value, st)
            end if

            if (.not. point() < limit) exit
            call goto_char(point() + 1)
            ! The GNU immediate assignment operator is ":=", while the
            ! POSIX operator is "::=".
            if (looking_at(":?=", st)) cycle
            if (has_face(point() - 1)) cycle

            if (line_beginning_position() > point_min() + 2) then
                if (char_before_at(line_end_position(0_int64)) == iachar("\")) cycle
            end if

            opoint = point()
            call beginning_of_line()
            matched = looking_at(makefile_dependency_regex%value, st)

            if (matched) then
                deps_end = match_end(1, st)
                data = match_data()
                call goto_char(deps_end)
                moved = skip_chars_backward(" "//TAB, st)
                deps_end = point()
                call beginning_of_line()
                moved = skip_chars_forward(" "//TAB, st)
                ! Alter the bounds recorded for subexp 1,
                ! which is what is supposed to match the targets.
                data(3) = point()
                data(4) = deps_end
                call set_match_data(data)
                call goto_char(opoint)
                call end_of_line()
                found = .true.
                return
            end if

            call goto_char(opoint)
        end do

        call goto_char(pt)
    end subroutine match_dependency

    !> Search for a rule action (`makefile_rule_action_regex') up to LIMIT,
    !> not after a continued line.  Port of make-mode.el:makefile-match-action.
    recursive subroutine makefile_match_action(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: f

        status = status_type()
        found = .false.

        do
            call re_search_forward(makefile_rule_action_regex%value, f, status, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. f <= 0) return

            if (after_char_at(match_beginning(0, status) - 2) /= iachar("\")) then
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

        do k = 1, size(makefile_runtime_macros_list)
            if (trim(makefile_runtime_macros_list(k)) == macro_name &
                .and. len_trim(makefile_runtime_macros_list(k)) == len(macro_name)) then
                call insert("$"//macro_name, status)
                return
            end if
        end do

        call insert(makefile_format_macro_ref(macro_name), status)
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
        if (.not. member_name(s%targets, target_name)) call push_front(s%targets, target_name)

        if (present(has_prereqs)) then
            if (has_prereqs) then
                call push_front(s%prereqs, target_name)
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
            call push_front(s%macros, macro_name)
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
    !> Uses `makefile_use_curly_braces_for_macros_p'.  Port of
    !> make-mode.el:makefile-format-macro-ref.
    function makefile_format_macro_ref(macro_name) result(ref)
        character(len=*), intent(in) :: macro_name
        character(len=:), allocatable :: ref
        logical :: open

        open = .false.
        if (len(macro_name) > 0) open = macro_name(1:1) == "(" .or. macro_name(1:1) == "{"

        if (open) then
            ref = "$"//macro_name
        else if (makefile_use_curly_braces_for_macros_p%value) then
            ref = "${"//macro_name//"}"
        else
            ref = "$("//macro_name//")"
        end if
    end function makefile_format_macro_ref

    !> Return name of target or variable assignment that point is in.
    !> If it isn't in one, FOUND is unallocated.  Port of
    !> make-mode.el:makefile-add-log-defun.
    subroutine makefile_add_log_subprogram(found)
        character(len=:), allocatable, intent(out) :: found
        integer(int64) :: opoint, shortage
        type(status_type) :: st
        integer :: k

        opoint = point()
        call beginning_of_line()

        ! Scan back line by line, noticing when we come to a
        ! variable or rule definition, and giving up when we see
        ! a line that is not part of either of those.
        do
            if (looking_at(makefile_macroassign_regex, st)) then
                found = match_string_no_properties(1)
            else if (looking_at(makefile_dependency_regex%value, st)) then
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
            k = len(found)

            do while (k > 0)
                if (found(k:k) /= " " .and. found(k:k) /= TAB) exit
                k = k - 1
            end do

            found = found(1:k)
            k = 1

            do while (k <= len(found))
                if (found(k:k) /= " " .and. found(k:k) /= TAB) exit
                k = k + 1
            end do

            found = found(k:)
        end if

        call goto_char(opoint)
    end subroutine makefile_add_log_subprogram

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Whether the character at POS has a `face' property.
    ! Not a port.
    logical function has_face(pos)
        integer(int64), intent(in) :: pos
        type(value_type) :: face
        type(status_type) :: st

        has_face = .false.
        if (pos < point_min()) return
        call get_text_property(pos, sym("face"), face, st)
        if (st%ok()) has_face = .not. nilp(face)
    end function has_face

    ! The character after point, -1 if none.
    ! Not a port.
    integer function after_char()
        integer(int32) :: c
        logical :: found

        call char_after(c, found)
        after_char = -1
        if (found) after_char = c
    end function after_char

    ! The character after POS, -1 if none.
    ! Not a port.
    integer function after_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        after_char_at = -1
        if (pos < point_min()) return
        call char_after(c, found, pos)
        if (found) after_char_at = c
    end function after_char_at

    ! The character before point, -1 if none.
    ! Not a port.
    integer function before_char()
        integer(int32) :: c
        logical :: found

        call char_before(c, found)
        before_char = -1
        if (found) before_char = c
    end function before_char

    ! The character before POS, -1 if none.
    ! Not a port.
    integer function char_before_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        char_before_at = -1
        if (pos <= point_min()) return
        call char_before(c, found, pos)
        if (found) char_before_at = c
    end function char_before_at

    ! N as a decimal string.
    ! Not a port.
    function itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa
end module make_mode_fm
