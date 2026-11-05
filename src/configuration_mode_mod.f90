! configuration_mode_mod.f90 -- simple major mode for editing conf/ini/properties files.
! Ported from GNU Emacs 31.1 lisp/textmodes/conf-mode.el (and files.el
! conf-mode-maybe).
!
! Copyright (C) 2004-2026 Free Software Foundation, Inc.
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

!> Conf mode: `configuration_mode' (which guesses one of the sub-modes from the
!> buffer's contents) and its sub-modes `configuration_unix_mode',
!> `configuration_windows_mode', `configuration_javaprop_mode', `configuration_space_mode',
!> `configuration_colon_mode', `configuration_ppd_mode', `configuration_xdefaults_mode',
!> `configuration_toml_mode', `configuration_desktop_mode' and `configuration_npmrc_mode', with
!> `configuration_align_assignments' (C-c C-a), `configuration_quote_normal' (C-c C-q) and
!> `configuration_space_keywords' (C-c C-s), and files.el's `configuration_mode_maybe'.
!>
!> The keyword lists (`configuration_font_lock_keywords' and the others) are
!> arrays of `font_lock_keyword_type'.  `configuration_assignment_sign' is a string
!> (unallocated for none).  `configuration_space_keywords_by_file' holds
!> `configuration_space_keywords_entry_type' entries.
!>
!> Deferred: Outline mode's variables (`outline_regular_expression',
!> `outline_heading_end_regular_expression', `outline_level' with
!> `configuration_outline_level'), the Conf menu, `text_conversion_style', the modes' abbrev tables, file-local
!> variables (`hack_local_variables_hook', so a file's own
!> `configuration_space_keywords'), and XML mode in `configuration_mode_maybe' (files that
!> start like XML keep their mode).
module configuration_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use data_mod, only: integer_setting_type, logical_setting_type, string_setting_type, forward_setting
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use buffer_mod, only: buffers, current
    use hooks_mod, only: hook_type
    use modes_mod, only: major_modes
    use keymap_mod, only: keymap_set
    use basic_subprograms_mod, only: delaying_mode_hooks, delay_mode_hooks_begin, delay_mode_hooks_end, execute_mode_hooks
    use syntax_mod, only: make_syntax_table, copy_syntax_table, modify_syntax_entry, syntax_table, set_syntax_table, &
                          is_syntax_table_live, parse_expression_ignore_comments, forward_comment, scan_groups
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, is_end_of_buffer, is_end_of_line, &
                                     character_after, character_before, delete_region, narrow_to_region, widen, region_beginning, &
                                     region_end, line_end_position
    use editing_commands_mod, only: forward_line, forward_character, end_of_line
    use insert_delete_mod, only: insert
    use indent_mod, only: current_column, indent_to
    use search_mod, only: looking_at, regular_expression_search_forward, search_forward, string_match, match_beginning, match_end, &
                          set_match_data, regular_expression_quote, SEARCH_NOERROR
    use simple_mod, only: is_use_region
    use minibuffer_reading_mod, only: read_string
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                             font_lock_mode_enabled, FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_flush, font_lock_refresh_defaults
    use newcomment_mod, only: comment_start, comment_start_skip, comment_beginning
    use index_menu_mod, only: imenu_patterns, imenu_pattern_type
    implicit none(type, external)
    private

    public :: initialize_configuration_mode, configuration_mode, configuration_unix_mode, configuration_windows_mode, &
              configuration_javaprop_mode, configuration_space_mode, &
              configuration_colon_mode, configuration_ppd_mode, configuration_xdefaults_mode, configuration_toml_mode, &
              configuration_desktop_mode, configuration_npmrc_mode, &
              configuration_mode_maybe, configuration_align_assignments, configuration_quote_normal, &
              configuration_space_keywords_command, &
              configuration_mode_initialize, configuration_toml_recognize_section, configuration_space_keywords_entry_type

    !> Align assignments to this column by default with \[configuration_align_assignments].
    !> If this number is negative, the `=' comes before the whitespace.  Use 0 to
    !> not align (only setting space according to `configuration_assignment_space').
    !> Port of conf-mode.el conf-assignment-column.
    type(integer_setting_type), target, public :: configuration_assignment_column = &
                                                  integer_setting_type(name="configuration_assignment_column", value=24)

    !> Value for `configuration_assignment_column' in Java properties buffers.
    !> Port of conf-mode.el conf-javaprop-assignment-column.
    type(integer_setting_type), target, public :: configuration_javaprop_assignment_column = &
                                                  integer_setting_type(name="configuration_javaprop_assignment_column", value=32)

    !> Value for `configuration_assignment_column' in Java properties buffers.
    !> Port of conf-mode.el conf-colon-assignment-column.
    type(integer_setting_type), target, public :: configuration_colon_assignment_column = &
                                                  integer_setting_type(name="configuration_colon_assignment_column", value=-24)

    !> Put at least one space around assignments when aligning.
    !> Port of conf-mode.el conf-assignment-space.
    type(logical_setting_type), target, public :: configuration_assignment_space = &
                                                  logical_setting_type(name="configuration_assignment_space", value=.true.)

    !> Value for `configuration_assignment_space' in colon style Conf mode buffers.
    !> Port of conf-mode.el conf-colon-assignment-space.
    type(logical_setting_type), target, public :: configuration_colon_assignment_space = &
                                                  logical_setting_type(name="configuration_colon_assignment_space", value=.false.)

    !> Local keymap for `configuration_mode' buffers.  Port of conf-mode.el
    !> conf-mode-map.
    type(handle_type), public :: configuration_mode_map

    !> Syntax table in use in Windows style `configuration_mode' buffers.  Port of
    !> conf-mode.el conf-mode-syntax-table.
    type(handle_type), public :: configuration_mode_syntax_table
    !> Syntax table in use in Unix style `configuration_mode' buffers.  Port of
    !> conf-mode.el conf-unix-mode-syntax-table.
    type(handle_type), public :: configuration_unix_mode_syntax_table
    !> Syntax table in use in npmrc `configuration_mode' buffers.  Port of
    !> conf-mode.el conf-npmrc-mode-syntax-table.
    type(handle_type), public :: configuration_npmrc_mode_syntax_table
    !> Syntax table in use in Java properties buffers.  Port of
    !> conf-mode.el conf-javaprop-mode-syntax-table (obsolete).
    type(handle_type), public :: configuration_javaprop_mode_syntax_table
    !> Syntax table in use in PPD `configuration_mode' buffers.  Port of
    !> conf-mode.el conf-ppd-mode-syntax-table.
    type(handle_type), public :: configuration_ppd_mode_syntax_table
    !> Syntax table in use in Xdefaults style `configuration_mode' buffers.  Port of
    !> conf-mode.el conf-xdefaults-mode-syntax-table.
    type(handle_type), public :: configuration_xdefaults_mode_syntax_table
    !> Syntax table in use in TOML style `configuration_mode' buffers.  Port of
    !> conf-mode.el conf-toml-mode-syntax-table.
    type(handle_type), public :: configuration_toml_mode_syntax_table

    !> Keywords to highlight in Conf mode.  Port of conf-mode.el
    !> conf-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: configuration_font_lock_keywords(:)
    !> Keywords to highlight in Conf Java Properties mode.  Port of
    !> conf-mode.el conf-javaprop-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: configuration_javaprop_font_lock_keywords(:)
    !> Keywords to highlight in Conf Space mode, before the one made from
    !> `configuration_space_keywords' (var val).  Port of conf-mode.el
    !> conf-space-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: configuration_space_font_lock_keywords(:)
    !> Keywords to highlight in Conf Colon mode.  Port of conf-mode.el
    !> conf-colon-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: configuration_colon_font_lock_keywords(:)
    !> Keywords to highlight in Conf TOML mode.  Port of conf-mode.el
    !> conf-toml-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: configuration_toml_font_lock_keywords(:)
    !> Keywords to highlight in Conf Desktop mode.  Port of conf-mode.el
    !> conf-desktop-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: configuration_desktop_font_lock_keywords(:)

    !> An entry of `configuration_space_keywords_by_file': files whose name matches
    !> REGULAR_EXPRESSION get KEYWORDS as `configuration_space_keywords'.
    type :: configuration_space_keywords_entry_type
        character(len=:), allocatable :: regular_expression, keywords
    end type configuration_space_keywords_entry_type

    !> File-name-based settings for the variable `configuration_space_keywords'.
    !> Port of conf-mode.el conf-space-keywords-alist.
    type(configuration_space_keywords_entry_type), allocatable, public :: configuration_space_keywords_by_file(:)

    !> Regexps for functions that may come before a space assignment.
    !> This allows constructs such as
    !> keyword var value
    !> This variable is best set in the file local variables, or through
    !> `configuration_space_keywords_by_file'.  Port of conf-mode.el
    !> conf-space-keywords.
    type(string_setting_type), target, public :: configuration_space_keywords = string_setting_type( &
                                                 name="configuration_space_keywords")

    !> Sign used for assignments (char or string).  Port of conf-mode.el
    !> conf-assignment-sign (unallocated for none).
    type(string_setting_type), target, public :: configuration_assignment_sign = string_setting_type( &
                                                 name="configuration_assignment_sign")

    !> Regexp to recognize assignments.
    !> It is anchored after the first expression on a line.  There must be a
    !> grouping for the assignment sign, including leading and trailing
    !> whitespace.  Port of conf-mode.el conf-assignment-regexp.
    type(string_setting_type), target, public :: configuration_assignment_regular_expression = string_setting_type( &
                                                 name="configuration_assignment_regular_expression")

    !> Hook run at the end of `configuration_mode'.
    type(hook_type), target, public :: configuration_mode_hook = hook_type(name="configuration_mode_hook")
    !> Hook run at the end of `configuration_unix_mode'.
    type(hook_type), target, public :: configuration_unix_mode_hook = hook_type(name="configuration_unix_mode_hook")
    !> Hook run at the end of `configuration_windows_mode'.
    type(hook_type), target, public :: configuration_windows_mode_hook = hook_type(name="configuration_windows_mode_hook")
    !> Hook run at the end of `configuration_javaprop_mode'.
    type(hook_type), target, public :: configuration_javaprop_mode_hook = hook_type(name="configuration_javaprop_mode_hook")
    !> Hook run at the end of `configuration_space_mode'.
    type(hook_type), target, public :: configuration_space_mode_hook = hook_type(name="configuration_space_mode_hook")
    !> Hook run at the end of `configuration_colon_mode'.
    type(hook_type), target, public :: configuration_colon_mode_hook = hook_type(name="configuration_colon_mode_hook")
    !> Hook run at the end of `configuration_ppd_mode'.
    type(hook_type), target, public :: configuration_ppd_mode_hook = hook_type(name="configuration_ppd_mode_hook")
    !> Hook run at the end of `configuration_xdefaults_mode'.
    type(hook_type), target, public :: configuration_xdefaults_mode_hook = hook_type(name="configuration_xdefaults_mode_hook")
    !> Hook run at the end of `configuration_toml_mode'.
    type(hook_type), target, public :: configuration_toml_mode_hook = hook_type(name="configuration_toml_mode_hook")
    !> Hook run at the end of `configuration_desktop_mode'.
    type(hook_type), target, public :: configuration_desktop_mode_hook = hook_type(name="configuration_desktop_mode_hook")
    !> Hook run at the end of `configuration_npmrc_mode'.
    type(hook_type), target, public :: configuration_npmrc_mode_hook = hook_type(name="configuration_npmrc_mode_hook")

    !> Whether `configuration_mode' is running its guessed sub-mode (so it doesn't
    !> guess again).  Port of conf-mode.el conf-mode--recursing.
    logical :: configuration_mode_recursing = .false.

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the Conf modes, their keymap, syntax tables and keywords (as
    !> loading conf-mode.el does).  Not a port.
    subroutine initialize_configuration_mode()
        type(status_type) :: st

        if (is_syntax_table_live(configuration_mode_syntax_table)) return
        call forward_setting(configuration_assignment_column)
        call forward_setting(configuration_javaprop_assignment_column)
        call forward_setting(configuration_colon_assignment_column)
        call forward_setting(configuration_assignment_space)
        call forward_setting(configuration_colon_assignment_space)
        call forward_setting(configuration_space_keywords)
        call forward_setting(configuration_assignment_sign)
        call forward_setting(configuration_assignment_regular_expression)
        if (.not. allocated(configuration_assignment_sign%value)) configuration_assignment_sign = "="
        if (.not. allocated(configuration_assignment_regular_expression%value)) configuration_assignment_regular_expression = &
            ".+?\([ "//TAB//"]*=[ "//TAB//"]*\)"
        call initialize_syntax_tables()
        call initialize_keywords()
        call initialize_space_keywords_table()
        call major_modes%configuration_mode%define("configuration_mode", "Conf[?]", configuration_mode, configuration_mode_hook, &
                                                   syntax_table=configuration_mode_syntax_table)
        configuration_mode_map = major_modes%configuration_mode%map
        call major_modes%configuration_unix_mode%define("configuration_unix_mode", "Conf[Unix]", configuration_unix_mode, &
                                                        configuration_unix_mode_hook, &
                                                        parent=major_modes%configuration_mode, &
                                                        syntax_table=configuration_unix_mode_syntax_table)
        call major_modes%configuration_windows_mode%define("configuration_windows_mode", "Conf[WinIni]", &
                                                           configuration_windows_mode, &
                                                           configuration_windows_mode_hook, parent=major_modes%configuration_mode)
        call major_modes%configuration_javaprop_mode%define("configuration_javaprop_mode", "Conf[JavaProp]", &
                                                            configuration_javaprop_mode, &
                                                          configuration_javaprop_mode_hook, parent=major_modes%configuration_mode, &
                                                            syntax_table=configuration_javaprop_mode_syntax_table)
        call major_modes%configuration_space_mode%define("configuration_space_mode", "Conf[Space]", configuration_space_mode, &
                                                         configuration_space_mode_hook, &
                                                         parent=major_modes%configuration_unix_mode)
        call major_modes%configuration_colon_mode%define("configuration_colon_mode", "Conf[Colon]", configuration_colon_mode, &
                                                         configuration_colon_mode_hook, &
                                                         parent=major_modes%configuration_unix_mode)
        call major_modes%configuration_ppd_mode%define("configuration_ppd_mode", "Conf[PPD]", configuration_ppd_mode, &
                                                       configuration_ppd_mode_hook, &
                                                       parent=major_modes%configuration_colon_mode, &
                                                       syntax_table=configuration_ppd_mode_syntax_table)
        call major_modes%configuration_xdefaults_mode%define("configuration_xdefaults_mode", "Conf[Xdefaults]", &
                                                             configuration_xdefaults_mode, &
                                                             configuration_xdefaults_mode_hook, &
                                                             parent=major_modes%configuration_colon_mode, &
                                                             syntax_table=configuration_xdefaults_mode_syntax_table)
        call major_modes%configuration_toml_mode%define("configuration_toml_mode", "Conf[TOML]", configuration_toml_mode, &
                                                        configuration_toml_mode_hook, &
                                                        parent=major_modes%configuration_mode, &
                                                        syntax_table=configuration_toml_mode_syntax_table)
        call major_modes%configuration_desktop_mode%define("configuration_desktop_mode", "Conf[Desktop]", &
                                                           configuration_desktop_mode, &
                                                        configuration_desktop_mode_hook, parent=major_modes%configuration_unix_mode)
        call major_modes%configuration_npmrc_mode%define("configuration_npmrc_mode", "Conf[npmrc]", configuration_npmrc_mode, &
                                                         configuration_npmrc_mode_hook, &
                                                         parent=major_modes%configuration_mode, &
                                                         syntax_table=configuration_npmrc_mode_syntax_table)
        call keymap_set(configuration_mode_map, "C-c C-u", "configuration_unix_mode", st)
        call keymap_set(configuration_mode_map, "C-c C-w", "configuration_windows_mode", st)
        call keymap_set(configuration_mode_map, "C-c C-j", "configuration_javaprop_mode", st)
        call keymap_set(configuration_mode_map, "C-c C-s", "configuration_space_keywords", st)
        call keymap_set(configuration_mode_map, "C-c SPC", "configuration_space_keywords", st)
        call keymap_set(configuration_mode_map, "C-c C-c", "configuration_colon_mode", st)
        call keymap_set(configuration_mode_map, "C-c :", "configuration_colon_mode", st)
        call keymap_set(configuration_mode_map, "C-c C-x", "configuration_xdefaults_mode", st)
        call keymap_set(configuration_mode_map, "C-c C-p", "configuration_ppd_mode", st)
        call keymap_set(configuration_mode_map, "C-c C-q", "configuration_quote_normal", st)
        call keymap_set(configuration_mode_map, "C-c """, "configuration_quote_normal", st)
        call keymap_set(configuration_mode_map, "C-c '", "configuration_quote_normal", st)
        call keymap_set(configuration_mode_map, "C-c C-a", "configuration_align_assignments", st)
        commands%configuration_mode%execute => configuration_mode
        commands%configuration_unix_mode%execute => configuration_unix_mode
        commands%configuration_windows_mode%execute => configuration_windows_mode
        commands%configuration_javaprop_mode%execute => configuration_javaprop_mode
        commands%configuration_space_mode%execute => configuration_space_mode
        commands%configuration_colon_mode%execute => configuration_colon_mode
        commands%configuration_ppd_mode%execute => configuration_ppd_mode
        commands%configuration_xdefaults_mode%execute => configuration_xdefaults_mode
        commands%configuration_toml_mode%execute => configuration_toml_mode
        commands%configuration_desktop_mode%execute => configuration_desktop_mode
        commands%configuration_npmrc_mode%execute => configuration_npmrc_mode
        commands%configuration_space_keywords%execute => execute_configuration_space_keywords
        commands%configuration_align_assignments%execute => execute_configuration_align_assignments
        commands%configuration_quote_normal%execute => execute_configuration_quote_normal
        commands%configuration_mode_maybe%execute => configuration_mode_maybe
        commands%configuration_mode_maybe%is_command = .false.
    end subroutine initialize_configuration_mode

    ! Make the syntax tables of the Conf modes.  Port of the values of
    ! conf-mode.el's conf-*-mode-syntax-table variables.
    subroutine initialize_syntax_tables()
        type(status_type) :: st
        type(handle_type) :: t

        call make_syntax_table(configuration_mode_syntax_table, st)
        t = configuration_mode_syntax_table
        call modify_syntax_entry(iachar("="), iachar("="), ".", st, t)
        call modify_syntax_entry(iachar("_"), iachar("_"), "_", st, t)
        call modify_syntax_entry(iachar("-"), iachar("-"), "_", st, t)
        call modify_syntax_entry(iachar("."), iachar("."), "_", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar(";"), iachar(";"), "<", st, t)
        call modify_syntax_entry(10, 10, ">", st, t)
        call modify_syntax_entry(13, 13, ">", st, t)
        call make_syntax_table(configuration_unix_mode_syntax_table, st, configuration_mode_syntax_table)
        t = configuration_unix_mode_syntax_table
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        ! override
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
        call make_syntax_table(configuration_npmrc_mode_syntax_table, st, configuration_mode_syntax_table)
        t = configuration_npmrc_mode_syntax_table
        call modify_syntax_entry(iachar(";"), iachar(";"), "<", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        call make_syntax_table(configuration_javaprop_mode_syntax_table, st, configuration_unix_mode_syntax_table)
        call make_syntax_table(configuration_ppd_mode_syntax_table, st, configuration_mode_syntax_table)
        t = configuration_ppd_mode_syntax_table
        call modify_syntax_entry(iachar("*"), iachar("*"), ". 1", st, t)
        call modify_syntax_entry(iachar("%"), iachar("%"), ". 2", st, t)
        ! override
        call modify_syntax_entry(iachar("'"), iachar("'"), ".", st, t)
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
        call make_syntax_table(configuration_xdefaults_mode_syntax_table, st, configuration_mode_syntax_table)
        t = configuration_xdefaults_mode_syntax_table
        call modify_syntax_entry(iachar("!"), iachar("!"), "<", st, t)
        ! override
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
        call make_syntax_table(configuration_toml_mode_syntax_table, st, configuration_mode_syntax_table)
        t = configuration_toml_mode_syntax_table
        call modify_syntax_entry(iachar('"'), iachar('"'), """", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar("\"), iachar("\"), "\", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        ! override
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
    end subroutine initialize_syntax_tables

    !> A highlight of subexpression N in FACE (LAX: it need not match).
    !> Not a port.
    function hl(n, face, lax, prepend) result(h)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        logical, intent(in), optional :: lax, prepend
        type(font_lock_highlight_type) :: h
        logical :: l, p

        l = .false.
        if (present(lax)) l = lax
        p = .false.
        if (present(prepend)) p = prepend

        if (p) then
            h = font_lock_highlight(n, face=face, override=FONT_LOCK_OVERRIDE_PREPEND, laxmatch=l)
        else
            h = font_lock_highlight(n, face=face, laxmatch=l)
        end if
    end function hl

    ! Set the font-lock keywords of the Conf modes.  Port of the values of
    ! conf-mode.el's conf-*-font-lock-keywords variables.
    subroutine initialize_keywords()
        character(len=*), parameter :: SECTION = "^[ "//TAB//"]*\[\(.+\)\]"
        type(font_lock_highlight_type) :: h1(1), h2(2), h7(7)

        allocate (configuration_font_lock_keywords(3))
        ! [section] (do this first because it may look like a parameter)
        h1(1) = hl(1, "font_lock_type_face")
        configuration_font_lock_keywords(1) = font_lock_keyword(SECTION, highlights=h1)
        ! var=val or var[index]=val
        h2(1) = hl(1, "font_lock_variable_name_face")
        h2(2) = hl(2, "font_lock_constant_face", lax=.true.)
        configuration_font_lock_keywords(2) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)\(?:\[\(.*?\)\]\)?[ "//TAB//"]*=", &
                                                                highlights=h2)
        ! section { ... } (do this last because some assign ...{...)
        h1(1) = hl(1, "font_lock_type_face", prepend=.true.)
        configuration_font_lock_keywords(3) = font_lock_keyword("^[ "//TAB//"]*\([^#=:"//LF//"]+?\)[ "//TAB//LF//"]*{[^{}]*?$", &
                                                                highlights=h1)
        ! var=val
        allocate (configuration_javaprop_font_lock_keywords(1))
        h7(1) = hl(1, "font_lock_variable_name_face")
        h7(2) = hl(2, "font_lock_constant_face", lax=.true.)
        h7(3) = hl(3, "font_lock_variable_name_face", lax=.true.)
        h7(4) = hl(4, "font_lock_constant_face", lax=.true.)
        h7(5) = hl(5, "font_lock_variable_name_face", lax=.true.)
        h7(6) = hl(6, "font_lock_constant_face", lax=.true.)
        h7(7) = hl(7, "font_lock_variable_name_face", lax=.true.)
        configuration_javaprop_font_lock_keywords(1) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)\(?:\.\([0-9]+\)\(?:\.\(.+?\)" &
                                                                         //"\(?:\.\([0-9]+\)\(?:\.\(.+?\)\(?:\.\([0-9]+\)" &
                                                                         //"\(\..+?\)?\)?\)?\)?\)?\)?\([:= "//TAB//"]\|$\)", &
                                                                         highlights=h7)
        allocate (configuration_space_font_lock_keywords(2))
        ! [section] (do this first because it may look like a parameter)
        h1(1) = hl(1, "font_lock_type_face")
        configuration_space_font_lock_keywords(1) = font_lock_keyword(SECTION, highlights=h1)
        ! section { ... } (do this first because it looks like a parameter)
        configuration_space_font_lock_keywords(2) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)[ "//TAB//LF//"]*{[^{}]*?$", &
                                                                      highlights=h1)
        allocate (configuration_colon_font_lock_keywords(3))
        ! [section] (do this first because it may look like a parameter)
        configuration_colon_font_lock_keywords(1) = font_lock_keyword(SECTION, highlights=h1)
        ! var: val
        h1(1) = hl(1, "font_lock_variable_name_face")
        configuration_colon_font_lock_keywords(2) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)[ "//TAB//"]*:", highlights=h1)
        ! section { ... } (do this last because some assign ...{...)
        h1(1) = hl(1, "font_lock_type_face", prepend=.true.)
        configuration_colon_font_lock_keywords(3) = font_lock_keyword("^[ "//TAB//"]*\([^:"//LF//"]+\)[ "//TAB//LF//"]*{[^{}]*?$", &
                                                                      highlights=h1)
        allocate (configuration_toml_font_lock_keywords(3))
        ! [section] (do this first because it may look like a parameter)
        h1(1) = hl(0, "font_lock_type_face", prepend=.true.)
        configuration_toml_font_lock_keywords(1) = font_lock_keyword(matcher=configuration_toml_recognize_section, highlights=h1)
        ! var=val or var[index]=val
        configuration_toml_font_lock_keywords(2) = font_lock_keyword("^\s-*\(.+?\)\(?:\[\(.*?\)\]\)?\s-*=", highlights=h2)
        ! Must be lower-case according to the TOML spec.
        h1(1) = hl(0, "font_lock_keyword_face")
        configuration_toml_font_lock_keywords(3) = font_lock_keyword("\_<false\|true\_>", highlights=h1)
        allocate (configuration_desktop_font_lock_keywords(5))
        configuration_desktop_font_lock_keywords(1:3) = configuration_font_lock_keywords(1:3)
        h1(1) = hl(0, "font_lock_constant_face")
        configuration_desktop_font_lock_keywords(4) = font_lock_keyword("\_<false\|true\_>", highlights=h1)
        configuration_desktop_font_lock_keywords(5) = font_lock_keyword("\_<%[uUfFick%]\_>", highlights=h1)
    end subroutine initialize_keywords

    ! Set `configuration_space_keywords_by_file'.  Port of the value of conf-mode.el's
    ! conf-space-keywords-alist.
    subroutine initialize_space_keywords_table()
        if (allocated(configuration_space_keywords_by_file)) return
        allocate (configuration_space_keywords_by_file(9))
        call set_entry(1, "\`/etc/gpm/", "key\|name\|foreground\|background\|border\|head")
        call set_entry(2, "\`/etc/magic\'", "[^ "//TAB//"]+[ "//TAB//"]+\(?:[bl]?e?\(?:short\|long\)\|byte\|string\)[^ " &
                       //TAB//"]*")
        call set_entry(3, "/mod\(?:ules\|probe\)\.conf", "alias\|in\(?:clude\|stall\)\|options\|remove")
        call set_entry(4, "/manpath\.config", "MAN\(?:DATORY_MANPATH\|PATH_MAP\|DB_MAP\)")
        call set_entry(5, "/sensors\.conf", "chip\|bus\|label\|compute\|set\|ignore")
        call set_entry(6, "/sane\(\.d\)?/", "option\|device\|port\|usb\|sc\(?:si\|anner\)")
        call set_entry(7, "/resmgr\.conf", "class\|add\|allow\|deny")
        call set_entry(8, "/dictionary\.lst\'", "DICT\|HYPH\|THES")
        call set_entry(9, "/tuxracer/options", "set")

    contains

        subroutine set_entry(k, regular_expression, keywords)
            integer, intent(in) :: k
            character(len=*), intent(in) :: regular_expression, keywords

            configuration_space_keywords_by_file(k)%regular_expression = regular_expression
            configuration_space_keywords_by_file(k)%keywords = keywords
        end subroutine set_entry
    end subroutine initialize_space_keywords_table

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Align the assignments in the buffer or active region.
    !> In Transient Mark mode, if the mark is active, operate on the
    !> contents of the region.  Otherwise, operate on the whole buffer.
    !> Port of conf-mode.el:conf-align-assignments (ARGUMENT absent for no prefix).
    subroutine configuration_align_assignments(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        integer(int64) :: column, original_point, cs, b, e, column_number, shortage, end_of_accessible_portion_from_end, &
                          beginning_of_accessible_portion
        logical :: narrowed, moved
        type(status_type) :: st
        integer :: c

        status = status_type()

        if (present(argument)) then
            column = argument
        else
            column = configuration_assignment_column%value
        end if

        original_point = point()
        ! (save-restriction)
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        narrowed = .false.

        if (is_use_region()) then
            call region_beginning(b, status)
            if (status%ok()) call region_end(e, status)
            if (.not. status%ok()) return
            call narrow_to_region(b, e, status)
            if (.not. status%ok()) return
            narrowed = .true.
        end if

        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            ! go before comment if within
            cs = comment_beginning()
            if (cs > 0) call goto_character(cs)

            do
                moved = forward_comment(9_int64)
                if (.not. moved) exit
            end do

            if (.not. is_end_of_buffer()) then
                if (looking_at(configuration_assignment_regular_expression%value, st)) then
                    b = match_beginning(1, st)
                    e = match_end(1, st)
                    call goto_character(b)
                    call delete_region(point(), e, status)
                    if (.not. status%ok()) exit

                    if (allocated(configuration_assignment_sign%value)) then
                        if (column >= 0) then
                            column_number = indent_to(column, status)
                            if (.not. status%ok()) exit
                            c = before_character()
                            if (configuration_assignment_space%value .and. c /= 32 .and. c /= 9) call insert(" ", status)

                            if (configuration_assignment_space%value .and. .not. is_end_of_line()) then
                                call insert(configuration_assignment_sign%value//" ", status)
                            else
                                call insert(configuration_assignment_sign%value, status)
                            end if
                        else
                            if (configuration_assignment_space%value) then
                                call insert(" "//configuration_assignment_sign%value, status)
                            else
                                call insert(configuration_assignment_sign%value, status)
                            end if

                            if (status%ok() .and. .not. is_end_of_line()) then
                                column_number = indent_to(-column, status)

                                if (status%ok()) then
                                    c = before_character()
                                    if (configuration_assignment_space%value .and. c /= 32 .and. c /= 9) call insert(" ", status)
                                end if
                            end if
                        end if
                    else if (.not. is_end_of_line()) then
                        if (current_column() >= abs(column)) then
                            call insert(" ", status)
                        else
                            column_number = indent_to(abs(column), status)
                        end if
                    end if

                    if (.not. status%ok()) exit
                end if
            end if

            shortage = forward_line(1_int64)
        end do

        ! (end of save-restriction and save-excursion)
        if (narrowed) then
            call widen()
            call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
        end if

        call goto_character(min(max(original_point, point_minimum()), point_maximum()))
    end subroutine configuration_align_assignments

    !> The character before point, -1 if none.  Not a port.
    integer function before_character()
        integer(int32) :: c
        logical :: found

        call character_before(c, found)
        before_character = -1
        if (found) before_character = c
    end function before_character

    !> The character after point, -1 if none.  Not a port.
    integer function after_character()
        integer(int32) :: c
        logical :: found

        call character_after(c, found)
        after_character = -1
        if (found) after_character = c
    end function after_character

    !> Align the assignments in the buffer or active region.
    !> In Transient Mark mode, if the mark is active, operate on the
    !> contents of the region.  Otherwise, operate on the whole buffer.
    !> Port of conf-mode.el:conf-align-assignments (the command).
    subroutine execute_configuration_align_assignments(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call configuration_align_assignments(status)
        else
            call configuration_align_assignments(status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_configuration_align_assignments

    !> Set the syntax of \=' and " to punctuation.
    !> With prefix ARGUMENT, only do it for \=' if 1, or only for " if 2.
    !> This only affects the current buffer.  Some conf files use quotes
    !> to delimit strings, while others allow quotes as simple parts of
    !> the assigned value.  In those files font locking will be wrong,
    !> and you can correct it with this command.  (Some files even do
    !> both, i.e. quotes delimit strings, except when they are
    !> unbalanced, but hey...)  Port of conf-mode.el:conf-quote-normal
    !> (ARGUMENT absent for no prefix).
    subroutine configuration_quote_normal(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        type(handle_type) :: table

        call copy_syntax_table(table, status, syntax_table())
        if (.not. status%ok()) return

        if (.not. present(argument)) then
            call modify_syntax_entry(iachar("'"), iachar("'"), ".", status, table)
            call modify_syntax_entry(iachar('"'), iachar('"'), ".", status, table)
        else
            if (argument == 1) call modify_syntax_entry(iachar("'"), iachar("'"), ".", status, table)
            if (argument == 2) call modify_syntax_entry(iachar('"'), iachar('"'), ".", status, table)
        end if

        call set_syntax_table(table, status)
        if (.not. status%ok()) return
        call font_lock_flush(status)
    end subroutine configuration_quote_normal

    !> Set the syntax of \=' and " to punctuation (the command).  Port of
    !> conf-mode.el:conf-quote-normal.
    subroutine execute_configuration_quote_normal(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call configuration_quote_normal(status)
        else
            call configuration_quote_normal(status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_configuration_quote_normal

    !> Try to guess sub-mode of `configuration_mode' based on buffer content, and
    !> run it.  Port of conf-mode.el:conf--guess-mode.
    recursive subroutine configuration_guess_mode(status)
        type(status_type), intent(out) :: status
        integer :: unix, window, equal, colon, space, jp, c
        integer(int64) :: original_point, moved, shortage, e
        type(status_type) :: st

        unix = 0
        window = 0
        equal = 0
        colon = 0
        space = 0
        jp = 0
        original_point = point()
        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            moved = skip_blanks()
            c = after_character()

            if (c == iachar("#")) then
                unix = unix + 1
            else if (c == iachar(";")) then
                window = window + 1
            else if (c == iachar("[")) then
                continue ! nop
            else if (is_end_of_line()) then
                continue ! nop
            else if (c == iachar("}")) then
                continue ! nop
            else if (looking_at("[^ "//TAB//LF//"=:]+\(?:  ?[^ "//TAB//LF//"=:]+\)*[ "//TAB//"]*[=:]", st)) then
                ! recognize at most double spaces within names
                e = match_end(0, st)

                if (before_character_at(e) == iachar("=")) then
                    equal = equal + 1
                else
                    colon = colon + 1
                end if
            else if (looking_at("/[/*]", st)) then
                jp = jp + 1
            else if (looking_at(".*{", st)) then
                continue ! nop
            else
                space = space + 1
            end if

            shortage = forward_line(1_int64)
        end do

        call goto_character(original_point)

        if (jp > max(unix, window, 3)) then
            call configuration_javaprop_mode(status)
        else if (colon > max(equal, space)) then
            call configuration_colon_mode(status)
        else if (space > max(equal, colon)) then
            call configuration_space_mode(status)
        else if (window > unix) then
            call configuration_windows_mode(status)
        else
            call configuration_unix_mode(status)
        end if
    end subroutine configuration_guess_mode

    !> (skip-chars-forward " \t\f") Not a port.
    integer(int64) function skip_blanks() result(moved)
        use syntax_mod, only: skip_characters_forward
        type(status_type) :: st

        moved = skip_characters_forward(" "//TAB//achar(12), st)
    end function skip_blanks

    !> The character before POSITION, -1 if none.  Not a port.
    integer function before_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        call character_before(c, found, position)
        before_character_at = -1
        if (found) before_character_at = c
    end function before_character_at

    !> Mode for Unix and Windows Conf files and Java properties.
    !> Most conf files know only three kinds of constructs: parameter
    !> assignments optionally grouped into sections and comments.  Yet
    !> there is a great range of variation in the exact syntax of conf
    !> files.  See below for various wrapper commands that set up the
    !> details for some of the most widespread variants.
    !>
    !> This mode sets up font locking, outline, imenu and it provides
    !> alignment support through `configuration_align_assignments'.  If strings
    !> come out wrong, try `configuration_quote_normal'.
    !>
    !> Some files allow continuation lines, either with a backslash at
    !> the end of line, or by indenting the next line (further).  These
    !> constructs cannot currently be recognized.
    !>
    !> Because of this great variety of nuances, which are often not
    !> even clearly specified, please don't expect it to get every file
    !> quite right.  Patches that clearly identify some special case,
    !> without breaking the general ones, are welcome.
    !>
    !> If instead you start this mode with the generic `configuration_mode'
    !> command, it will parse the buffer.  It will generally well
    !> identify the first four cases listed below.  If the buffer
    !> doesn't have enough contents to decide, this is identical to
    !> `configuration_windows_mode' on Windows, elsewhere to `configuration_unix_mode'.
    !> See also `configuration_space_mode', `configuration_colon_mode', `configuration_javaprop_mode',
    !> `configuration_ppd_mode' and `configuration_xdefaults_mode'.
    !>
    !> \{configuration_mode_map}
    !>
    !> Port of conf-mode.el:conf-mode (with its advice: called directly, it
    !> runs the guessed sub-mode).
    recursive subroutine configuration_mode(status)
        type(status_type), intent(out) :: status
        type(imenu_pattern_type), allocatable :: patterns(:)
        logical :: saved, recursing

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()

        ! Redirect to one of the submodes when called directly.  The file
        ! may have "mode: conf" in the local variable block, in which case
        ! we'll be called recursively infinitely.  Inhibit that.
        if (.not. (delaying_mode_hooks() .or. configuration_mode_recursing)) then
            recursing = configuration_mode_recursing
            configuration_mode_recursing = .true.
            call configuration_guess_mode(status)
            configuration_mode_recursing = recursing
            return
        end if

        call major_modes%configuration_mode%begin(saved, status)
        if (.not. status%ok()) return
        call font_lock_defaults%set_local(keywords=configuration_font_lock_keywords, case_fold=.true.)
        ! Let newcomment.el decide this for itself.
        call parse_expression_ignore_comments%set_local(.true.)
        allocate (patterns(3))
        patterns(1) = imenu_pattern_type(menu_title="Parameters", regular_expression="^[ "//TAB//"]*\(.+?\)[ "//TAB//"]*=", &
                                         subexpression=1)
        ! [section]
        patterns(2) = imenu_pattern_type(menu_title="", regular_expression="^[ "//TAB//"]*\[[ "//TAB//"]*\(.+\)[ "//TAB//"]*\]", &
                                         subexpression=1)
        ! section { ... }
        patterns(3) = imenu_pattern_type(menu_title="", regular_expression="^[ "//TAB//"]*\([^=:{} "//TAB//LF//"][^=:{}"//LF// &
                                         "]+\)[ "//TAB//LF//"]*{", subexpression=1)
        call imenu_patterns%set_local(patterns)
        call major_modes%configuration_mode%end(saved, status)
    end subroutine configuration_mode

    !> Initializations for sub-modes of `configuration_mode'.
    !> COMMENT initializes `comment_start' and `comment_start_skip'.
    !> The optional arg FONT_LOCK is the value for FONT_LOCK_KEYWORDS.
    !> Port of conf-mode.el:conf-mode-initialize.
    subroutine configuration_mode_initialize(comment, font_lock)
        character(len=*), intent(in) :: comment
        type(font_lock_keyword_type), intent(in), optional :: font_lock(:)

        call comment_start%set_local(comment)
        call comment_start_skip%set_local(regular_expression_quote(comment)//"+\s *")
        if (present(font_lock)) call font_lock_defaults%set_local(keywords=font_lock, case_fold=.true.)
    end subroutine configuration_mode_initialize

    !> Conf Mode starter for Unix style Conf files.
    !> Comments start with `#'.  For details see `configuration_mode'.  Port of
    !> conf-mode.el:conf-unix-mode.
    recursive subroutine configuration_unix_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_unix_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#")
        call major_modes%configuration_unix_mode%end(saved, status)
    end subroutine configuration_unix_mode

    !> Conf Mode starter for Windows style Conf files.
    !> Comments start with `;'.
    !> For details see `configuration_mode'.  Example:
    !>
    !> ; Conf mode font-locks this right on Windows and with \[configuration_windows_mode]
    !>
    !> [ExtShellFolderViews]
    !> Default={5984FFE0-28D4-11CF-AE66-08002B2E1262}
    !> {5984FFE0-28D4-11CF-AE66-08002B2E1262}={5984FFE0-28D4-11CF-AE66-08002B2E1262}
    !>
    !> [{5984FFE0-28D4-11CF-AE66-08002B2E1262}]
    !> PersistMoniker=file://Folder.htt
    !>
    !> Port of conf-mode.el:conf-windows-mode.
    recursive subroutine configuration_windows_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_windows_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize(";")
        call major_modes%configuration_windows_mode%end(saved, status)
    end subroutine configuration_windows_mode

    !> Conf Mode starter for Java properties files.
    !> Comments start with `#'.  Example:
    !>
    !> # Conf mode font-locks this right with \[configuration_javaprop_mode] (Java properties)
    !>
    !> name:value
    !> name=value
    !> name value
    !> x.1 =
    !> x.2.y.1.z.1 =
    !> x.2.y.1.z.2.zz =
    !>
    !> Port of conf-mode.el:conf-javaprop-mode.
    recursive subroutine configuration_javaprop_mode(status)
        type(status_type), intent(out) :: status
        type(imenu_pattern_type), allocatable :: patterns(:)
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_javaprop_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#", configuration_javaprop_font_lock_keywords)
        call configuration_assignment_column%set_local(configuration_javaprop_assignment_column%value)
        call configuration_assignment_regular_expression%set_local(".+?\([ "//TAB//"]*[=: "//TAB//"][ "//TAB//"]*\|$\)")
        allocate (patterns(1))
        patterns(1) = imenu_pattern_type(menu_title="Parameters", regular_expression="^[ "//TAB//"]*\(.+?\)[=: "//TAB//"]", &
                                         subexpression=1)
        call imenu_patterns%set_local(patterns)
        call major_modes%configuration_javaprop_mode%end(saved, status)
    end subroutine configuration_javaprop_mode

    !> Conf Mode starter for space separated conf files.
    !> "Assignments" are with ` '.  Keywords before the parameters are
    !> recognized according to the variable `configuration_space_keywords_by_file'.
    !> Alternatively, you can specify a value for the file local variable
    !> `configuration_space_keywords'.
    !> Use the function `configuration_space_keywords' if you want to specify keywords
    !> in an interactive fashion instead.
    !>
    !> For details see `configuration_mode'.  Example:
    !>
    !> # Conf mode font-locks this right with \[configuration_space_mode] (space separated)
    !>
    !> image/jpeg                      jpeg jpg jpe
    !> image/png                       png
    !> image/tiff                      tiff tif
    !>
    !> # Or with keywords (from a recognized file name):
    !> class desktop
    !> # Standard multimedia devices
    !> add /dev/audio          desktop
    !> add /dev/mixer          desktop
    !>
    !> Port of conf-mode.el:conf-space-mode.
    recursive subroutine configuration_space_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k
        type(status_type) :: st

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_space_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#", configuration_space_font_lock_keywords)
        call configuration_assignment_sign%make_local()
        call configuration_assignment_sign%clear()

        if (allocated(buffers(current)%filename)) then
            ! We set conf-space-keywords directly, but a value which is
            ! in the local variables list or interactively specified
            ! (see the function conf-space-keywords) takes precedence.
            call configuration_space_keywords%make_local()
            call configuration_space_keywords%clear()

            do k = 1, size(configuration_space_keywords_by_file)
                if (string_match(configuration_space_keywords_by_file(k)%regular_expression, buffers(current)%filename, &
                                 st) > 0) then
                    call configuration_space_keywords%set_local(configuration_space_keywords_by_file(k)%keywords)
                    exit
                end if
            end do
        end if

        call configuration_space_mode_internal(status)
        if (.not. status%ok()) return
        call major_modes%configuration_space_mode%end(saved, status)
    end subroutine configuration_space_mode

    !> Enter Conf Space mode using regexp KEYWORDS to match the keywords.
    !> See `configuration_space_mode'.  Port of conf-mode.el:conf-space-keywords.
    recursive subroutine configuration_space_keywords_command(keywords, status)
        character(len=*), intent(in) :: keywords
        type(status_type), intent(out) :: status
        logical :: saved

        call delay_mode_hooks_begin(saved)
        call configuration_space_mode(status)
        call delay_mode_hooks_end(saved)
        if (.not. status%ok()) return

        if (len(keywords) == 0) then
            call configuration_space_keywords%make_local()
            call configuration_space_keywords%clear()
        else
            call configuration_space_keywords%set_local(keywords)
        end if

        call configuration_space_mode_internal(status)
        if (.not. status%ok()) return
        call execute_mode_hooks(status)
    end subroutine configuration_space_keywords_command

    !> Enter Conf Space mode using regexp KEYWORDS to match the keywords
    !> (the command `configuration_space_keywords').  Port of
    !> conf-mode.el:conf-space-keywords.
    recursive subroutine execute_configuration_space_keywords(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: keywords

        call read_string("Conf Space keyword regexp: ", keywords, status)
        if (.not. status%ok()) return
        call configuration_space_keywords_command(keywords, status)
    end subroutine execute_configuration_space_keywords

    !> Recompute what depends on `configuration_space_keywords': the assignment
    !> regexp and the font-lock keywords.  Port of
    !> conf-mode.el:conf-space-mode-internal.
    subroutine configuration_space_mode_internal(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        type(font_lock_highlight_type) :: h1(1), h2(2)
        type(imenu_pattern_type), allocatable :: old(:), patterns(:)
        integer :: n, k

        status = status_type()

        if (allocated(configuration_space_keywords%value)) then
            call configuration_assignment_regular_expression%set_local("\(?:"//configuration_space_keywords%value//"\)[ "//TAB &
                                                                       //"]+.+?\([ "//TAB// &
                                                                       "]+\|$\)")
        else
            call configuration_assignment_regular_expression%set_local(".+?\([ "//TAB//"]+\|$\)")
        end if

        ! The keywords' (eval ...) entry, var val.
        n = size(configuration_space_font_lock_keywords)
        allocate (keywords(n + 1))
        keywords(1:n) = configuration_space_font_lock_keywords

        if (allocated(configuration_space_keywords%value)) then
            h2(1) = hl(1, "font_lock_keyword_face")
            h2(2) = hl(2, "font_lock_variable_name_face")
            keywords(n + 1) = font_lock_keyword("^[ "//TAB//"]*\("//configuration_space_keywords%value//"\)[ "//TAB// &
                                                "]+\([^"//achar(0)//"- ]+\)", highlights=h2)
        else
            h1(1) = hl(1, "font_lock_variable_name_face")
            keywords(n + 1) = font_lock_keyword("^[ "//TAB//"]*\([^"//achar(0)//"- ]+\)", highlights=h1)
        end if

        call font_lock_defaults%set_local(keywords=keywords, case_fold=.true.)
        ! If Font Lock is already enabled, reenable it with new
        ! conf-assignment-regexp.
        if (font_lock_mode_enabled()) call font_lock_refresh_defaults(status)
        ! Get rid of any existing Parameters element.
        old = imenu_patterns%current()
        allocate (patterns(size(old) + 1))
        n = 1

        do k = 1, size(old)
            if (k == parameters_index(old)) cycle
            n = n + 1
            patterns(n) = old(k)
        end do

        ! Add a new one based on conf-space-keywords.
        if (allocated(configuration_space_keywords%value)) then
            patterns(1) = imenu_pattern_type(menu_title="Parameters", regular_expression="^[ "//TAB//"]*\(?:"// &
                                             configuration_space_keywords%value//"\)[ "//TAB//"]+\([^ "//TAB//LF// &
                                             "]+\)\(?:[ "//TAB//"]\|$\)", subexpression=1)
        else
            patterns(1) = imenu_pattern_type(menu_title="Parameters", regular_expression="^[ "//TAB//"]*\([^ "//TAB//LF// &
                                             "[]+\)\(?:[ "//TAB//"]\|$\)", subexpression=1)
        end if

        call imenu_patterns%set_local(patterns(1:n))

    contains

        ! The index of the first element of MATCHERS titled "Parameters", 0
        ! if none.
        integer function parameters_index(patterns) result(i)
            type(imenu_pattern_type), intent(in) :: patterns(:)

            do i = 1, size(patterns)
                if (patterns(i)%menu_title == "Parameters") return
            end do

            i = 0
        end function parameters_index
    end subroutine configuration_space_mode_internal

    !> Conf Mode starter for Colon files.
    !> "Assignments" are with `:'.
    !> For details see `configuration_mode'.  Example:
    !>
    !> # Conf mode font-locks this right with \[configuration_colon_mode] (colon)
    !>
    !> <Multi_key> <exclam> <exclam>           : "\241"        exclamdown
    !> <Multi_key> <c> <slash>                 : "\242"        cent
    !>
    !> Port of conf-mode.el:conf-colon-mode.
    recursive subroutine configuration_colon_mode(status)
        type(status_type), intent(out) :: status
        type(imenu_pattern_type), allocatable :: old(:)
        type(imenu_pattern_type), allocatable :: patterns(:)
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_colon_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#", configuration_colon_font_lock_keywords)
        call configuration_assignment_space%set_local(configuration_colon_assignment_space%value)
        call configuration_assignment_column%set_local(configuration_colon_assignment_column%value)
        call configuration_assignment_sign%set_local(":")
        call configuration_assignment_regular_expression%set_local(".+?\([ "//TAB//"]*:[ "//TAB//"]*\)")
        old = imenu_patterns%current()
        allocate (patterns(max(1, size(old))))
        patterns(1) = imenu_pattern_type(menu_title="Parameters", regular_expression="^[ "//TAB//"]*\(.+?\)[ "//TAB//"]*:", &
                                         subexpression=1)
        if (size(old) > 1) patterns(2:) = old(2:)
        call imenu_patterns%set_local(patterns)
        call major_modes%configuration_colon_mode%end(saved, status)
    end subroutine configuration_colon_mode

    !> Conf Mode starter for Adobe/CUPS PPD files.
    !> Comments start with `*%' and "assignments" are with `:'.
    !> For details see `configuration_mode'.  Example:
    !>
    !> *% Conf mode font-locks this right with \[configuration_ppd_mode] (PPD)
    !>
    !> *DefaultTransfer: Null
    !> *Transfer Null.Inverse: "{ 1 exch sub }"
    !>
    !> Port of conf-mode.el:conf-ppd-mode.
    recursive subroutine configuration_ppd_mode(status)
        type(status_type), intent(out) :: status
        type(imenu_pattern_type), allocatable :: patterns(:)
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_ppd_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("*%")
        ! no sections, they match within PostScript code
        patterns = imenu_patterns%current()
        if (size(patterns) > 1) call imenu_patterns%set_local(patterns(1:1))
        call major_modes%configuration_ppd_mode%end(saved, status)
    end subroutine configuration_ppd_mode

    !> Conf Mode starter for Xdefaults files.
    !> Comments start with `!' and "assignments" are with `:'.
    !> For details see `configuration_mode'.  Example:
    !>
    !> ! Conf mode font-locks this right with \[configuration_xdefaults_mode] (.Xdefaults)
    !>
    !> *background:                    gray99
    !> *foreground:                    black
    !>
    !> Port of conf-mode.el:conf-xdefaults-mode.
    recursive subroutine configuration_xdefaults_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_xdefaults_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("!")
        call major_modes%configuration_xdefaults_mode%end(saved, status)
    end subroutine configuration_xdefaults_mode

    !> Font-lock helper function for `configuration_toml_mode'.
    !> Handles recognizing TOML section names, like [section],
    !> [[section]], or [something."else".section].  Port of
    !> conf-mode.el:conf-toml-recognize-section.
    subroutine configuration_toml_recognize_section(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, position, f
        type(status_type) :: st

        status = status_type()
        found = .false.
        ! Skip any number of "[" to handle things like [[section]].
        call regular_expression_search_forward("^\s-*\[+", f, st, bound=limit, noerror=SEARCH_NOERROR)
        if (.not. st%ok() .or. f <= 0) return
        start = point()
        call forward_character(st, -1_int64)
        position = scan_groups(point(), 1_int64, 0_int64, st)

        if (st%ok() .and. position > 0) then
            call goto_character(position)
            end = min(limit, point() - 1)
        else
            call end_of_line(1_int64)
            end = min(limit, point())
        end if

        ! If there is a comma in the text, then we assume this is
        ! an array and not a section.  (This could be refined to
        ! look only for unquoted commas if necessary.)
        position = point()
        call goto_character(start)
        call search_forward(",", f, st, bound=end, noerror=SEARCH_NOERROR)
        call goto_character(position)

        if (f <= 0) then
            call set_match_data([start, end])
            found = .true.
        end if
    end subroutine configuration_toml_recognize_section

    !> Conf Mode starter for TOML files.
    !> Comments start with `#' and "assignments" are with `='.
    !> For details see `configuration_mode'.  Example:
    !>
    !> # Conf mode font-locks this right with \[configuration_toml_mode]
    !>
    !> [entry]
    !> value = "some string"
    !>
    !> Port of conf-mode.el:conf-toml-mode.
    recursive subroutine configuration_toml_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_toml_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#")
        ! Booleans are "always lowercase", so we must *not* use case
        ! folding.  Therefore, we can't set it using `conf-mode-initialize'.
        call font_lock_defaults%set_local(keywords=configuration_toml_font_lock_keywords, case_fold=.false.)
        call configuration_assignment_column%set_local(0)
        call configuration_assignment_sign%set_local("=")
        call major_modes%configuration_toml_mode%end(saved, status)
    end subroutine configuration_toml_mode

    !> Conf Mode started for freedesktop.org Desktop files.
    !> Comments start with `#' and "assignments" are with `='.
    !> For details see `configuration_mode'.
    !>
    !> # Conf mode font-locks this correctly with \[configuration_desktop_mode]
    !>         [Desktop Entry]
    !>         Name=GNU Image Manipulation Program
    !>         Name[oc]=Editor d'imatge GIMP
    !>         Exec=gimp-2.8 %U
    !>         Terminal=false
    !>
    !> Port of conf-mode.el:conf-desktop-mode.
    recursive subroutine configuration_desktop_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_desktop_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#", configuration_desktop_font_lock_keywords)
        call configuration_quote_normal(status)
        if (.not. status%ok()) return
        call major_modes%configuration_desktop_mode%end(saved, status)
    end subroutine configuration_desktop_mode

    !> Conf Mode starter for .npmrc files.
    !> Comments start with `#' and `;'. For details see `configuration_mode'.
    !> Port of conf-mode.el:conf-npmrc-mode.
    recursive subroutine configuration_npmrc_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(configuration_mode_syntax_table)) call initialize_configuration_mode()
        call major_modes%configuration_npmrc_mode%begin(saved, status)
        if (.not. status%ok()) return
        call configuration_mode_initialize("#")
        call major_modes%configuration_npmrc_mode%end(saved, status)
    end subroutine configuration_npmrc_mode

    !> Select Conf mode or XML mode according to start of file.  Port of
    !> files.el:conf-mode-maybe (XML mode is deferred: such files keep
    !> their mode).
    recursive subroutine configuration_mode_maybe(status)
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, beginning_of_accessible_portion, end_of_accessible_portion_from_end
        logical :: xml
        type(status_type) :: st

        status = status_type()
        original_point = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        call widen()
        call goto_character(point_minimum())
        xml = looking_at("<\?xml \|<!-- \|<!DOCTYPE ", st)
        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
        call goto_character(original_point)
        if (xml) return
        call configuration_mode(status)
    end subroutine configuration_mode_maybe
end module configuration_mode_mod
