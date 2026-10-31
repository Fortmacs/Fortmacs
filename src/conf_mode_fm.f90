! conf_mode_fm.f90 -- simple major mode for editing conf/ini/properties files.
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

!> Conf mode: `conf_mode' (which guesses one of the sub-modes from the
!> buffer's contents) and its sub-modes `conf_unix_mode',
!> `conf_windows_mode', `conf_javaprop_mode', `conf_space_mode',
!> `conf_colon_mode', `conf_ppd_mode', `conf_xdefaults_mode',
!> `conf_toml_mode', `conf_desktop_mode' and `conf_npmrc_mode', with
!> `conf_align_assignments' (C-c C-a), `conf_quote_normal' (C-c C-q) and
!> `conf_space_keywords' (C-c C-s), and files.el's `conf_mode_maybe'.
!>
!> The keyword lists (`conf_font_lock_keywords' and the others) are
!> arrays of `font_lock_keyword_type'.  `conf_assignment_sign' is a string
!> (unallocated for nil).  `conf_space_keywords_table' holds
!> `conf_space_keywords_entry_type' pairs.
!>
!> Deferred: Outline mode's variables (`outline_regexp',
!> `outline_heading_end_regexp', `outline_level' with
!> `conf_outline_level'), Imenu's `imenu_generic_expression', the Conf
!> menu, `text_conversion_style', the modes' abbrev tables, file-local
!> variables (`hack_local_variables_hook', so a file's own
!> `conf_space_keywords'), and XML mode in `conf_mode_maybe' (files that
!> start like XML keep their mode).
module conf_mode_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: nilp
    use data_fm, only: integer_setting, logical_setting, string_setting, forward_setting
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use buffer_fm, only: buffers, cur
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use keymap_fm, only: keymap_set
    use subr_fm, only: delaying_mode_hooks, delay_mode_hooks_begin, delay_mode_hooks_end, run_mode_hooks
    use syntax_fm, only: make_syntax_table, copy_syntax_table, modify_syntax_entry, syntax_table, set_syntax_table, &
                         is_syntax_table_live, parse_expression_ignore_comments, forward_comment, scan_groups
    use editfns_fm, only: point, point_min, point_max, goto_char, is_eob, is_eol, char_after, char_before, delete_region, &
                          narrow_to_region, widen, region_beginning, region_end, line_end_position
    use cmds_fm, only: forward_line, forward_char, end_of_line
    use insdel_fm, only: insert
    use indent_fm, only: current_column, indent_to
    use search_fm, only: looking_at, re_search_forward, search_forward, string_match, match_beginning, match_end, &
                         set_match_data, regexp_quote, SEARCH_NOERROR
    use simple_fm, only: is_use_region
    use minibuf_fm, only: read_string
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_mode_enabled, FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_flush, font_lock_refresh_defaults
    use newcomment_fm, only: comment_start, comment_start_skip, comment_beginning
    implicit none(type, external)
    private

    public :: syms_of_conf_mode, conf_mode, conf_unix_mode, conf_windows_mode, conf_javaprop_mode, conf_space_mode, &
              conf_colon_mode, conf_ppd_mode, conf_xdefaults_mode, conf_toml_mode, conf_desktop_mode, conf_npmrc_mode, &
              conf_mode_maybe, conf_align_assignments, conf_quote_normal, conf_space_keywords_command, &
              conf_mode_initialize, conf_toml_recognize_section, conf_space_keywords_entry_type

    !> Align assignments to this column by default with \[conf_align_assignments].
    !> If this number is negative, the `=' comes before the whitespace.  Use 0 to
    !> not align (only setting space according to `conf_assignment_space').
    !> Port of conf-mode.el conf-assignment-column.
    type(integer_setting), target, public :: conf_assignment_column = &
                                             integer_setting(name="conf_assignment_column", value=24)

    !> Value for `conf_assignment_column' in Java properties buffers.
    !> Port of conf-mode.el conf-javaprop-assignment-column.
    type(integer_setting), target, public :: conf_javaprop_assignment_column = &
                                             integer_setting(name="conf_javaprop_assignment_column", value=32)

    !> Value for `conf_assignment_column' in Java properties buffers.
    !> Port of conf-mode.el conf-colon-assignment-column.
    type(integer_setting), target, public :: conf_colon_assignment_column = &
                                             integer_setting(name="conf_colon_assignment_column", value=-24)

    !> Put at least one space around assignments when aligning.
    !> Port of conf-mode.el conf-assignment-space.
    type(logical_setting), target, public :: conf_assignment_space = &
                                             logical_setting(name="conf_assignment_space", value=.true.)

    !> Value for `conf_assignment_space' in colon style Conf mode buffers.
    !> Port of conf-mode.el conf-colon-assignment-space.
    type(logical_setting), target, public :: conf_colon_assignment_space = &
                                             logical_setting(name="conf_colon_assignment_space", value=.false.)

    !> Local keymap for `conf_mode' buffers.  Port of conf-mode.el
    !> conf-mode-map.
    type(handle_type), public :: conf_mode_map

    !> Syntax table in use in Windows style `conf_mode' buffers.  Port of
    !> conf-mode.el conf-mode-syntax-table.
    type(handle_type), public :: conf_mode_syntax_table
    !> Syntax table in use in Unix style `conf_mode' buffers.  Port of
    !> conf-mode.el conf-unix-mode-syntax-table.
    type(handle_type), public :: conf_unix_mode_syntax_table
    !> Syntax table in use in npmrc `conf_mode' buffers.  Port of
    !> conf-mode.el conf-npmrc-mode-syntax-table.
    type(handle_type), public :: conf_npmrc_mode_syntax_table
    !> Syntax table in use in Java properties buffers.  Port of
    !> conf-mode.el conf-javaprop-mode-syntax-table (obsolete).
    type(handle_type), public :: conf_javaprop_mode_syntax_table
    !> Syntax table in use in PPD `conf_mode' buffers.  Port of
    !> conf-mode.el conf-ppd-mode-syntax-table.
    type(handle_type), public :: conf_ppd_mode_syntax_table
    !> Syntax table in use in Xdefaults style `conf_mode' buffers.  Port of
    !> conf-mode.el conf-xdefaults-mode-syntax-table.
    type(handle_type), public :: conf_xdefaults_mode_syntax_table
    !> Syntax table in use in TOML style `conf_mode' buffers.  Port of
    !> conf-mode.el conf-toml-mode-syntax-table.
    type(handle_type), public :: conf_toml_mode_syntax_table

    !> Keywords to highlight in Conf mode.  Port of conf-mode.el
    !> conf-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: conf_font_lock_keywords(:)
    !> Keywords to highlight in Conf Java Properties mode.  Port of
    !> conf-mode.el conf-javaprop-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: conf_javaprop_font_lock_keywords(:)
    !> Keywords to highlight in Conf Space mode, before the one made from
    !> `conf_space_keywords' (var val).  Port of conf-mode.el
    !> conf-space-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: conf_space_font_lock_keywords(:)
    !> Keywords to highlight in Conf Colon mode.  Port of conf-mode.el
    !> conf-colon-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: conf_colon_font_lock_keywords(:)
    !> Keywords to highlight in Conf TOML mode.  Port of conf-mode.el
    !> conf-toml-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: conf_toml_font_lock_keywords(:)
    !> Keywords to highlight in Conf Desktop mode.  Port of conf-mode.el
    !> conf-desktop-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: conf_desktop_font_lock_keywords(:)

    !> An entry of `conf_space_keywords_table': files whose name matches
    !> REGEXP get KEYWORDS as `conf_space_keywords'.
    type :: conf_space_keywords_entry_type
        character(len=:), allocatable :: regexp, keywords
    end type conf_space_keywords_entry_type

    !> File-name-based settings for the variable `conf_space_keywords'.
    !> Port of conf-mode.el conf-space-keywords-alist.
    type(conf_space_keywords_entry_type), allocatable, public :: conf_space_keywords_table(:)

    !> Regexps for functions that may come before a space assignment.
    !> This allows constructs such as
    !> keyword var value
    !> This variable is best set in the file local variables, or through
    !> `conf_space_keywords_table'.  Port of conf-mode.el
    !> conf-space-keywords.
    type(string_setting), target, public :: conf_space_keywords = string_setting(name="conf_space_keywords")

    !> Sign used for assignments (char or string).  Port of conf-mode.el
    !> conf-assignment-sign (unallocated for nil).
    type(string_setting), target, public :: conf_assignment_sign = string_setting(name="conf_assignment_sign")

    !> Regexp to recognize assignments.
    !> It is anchored after the first expression on a line.  There must be a
    !> grouping for the assignment sign, including leading and trailing
    !> whitespace.  Port of conf-mode.el conf-assignment-regexp.
    type(string_setting), target, public :: conf_assignment_regexp = string_setting(name="conf_assignment_regexp")

    !> Hook run at the end of `conf_mode'.
    type(hook_type), target, public :: conf_mode_hook = hook_type(name="conf_mode_hook")
    !> Hook run at the end of `conf_unix_mode'.
    type(hook_type), target, public :: conf_unix_mode_hook = hook_type(name="conf_unix_mode_hook")
    !> Hook run at the end of `conf_windows_mode'.
    type(hook_type), target, public :: conf_windows_mode_hook = hook_type(name="conf_windows_mode_hook")
    !> Hook run at the end of `conf_javaprop_mode'.
    type(hook_type), target, public :: conf_javaprop_mode_hook = hook_type(name="conf_javaprop_mode_hook")
    !> Hook run at the end of `conf_space_mode'.
    type(hook_type), target, public :: conf_space_mode_hook = hook_type(name="conf_space_mode_hook")
    !> Hook run at the end of `conf_colon_mode'.
    type(hook_type), target, public :: conf_colon_mode_hook = hook_type(name="conf_colon_mode_hook")
    !> Hook run at the end of `conf_ppd_mode'.
    type(hook_type), target, public :: conf_ppd_mode_hook = hook_type(name="conf_ppd_mode_hook")
    !> Hook run at the end of `conf_xdefaults_mode'.
    type(hook_type), target, public :: conf_xdefaults_mode_hook = hook_type(name="conf_xdefaults_mode_hook")
    !> Hook run at the end of `conf_toml_mode'.
    type(hook_type), target, public :: conf_toml_mode_hook = hook_type(name="conf_toml_mode_hook")
    !> Hook run at the end of `conf_desktop_mode'.
    type(hook_type), target, public :: conf_desktop_mode_hook = hook_type(name="conf_desktop_mode_hook")
    !> Hook run at the end of `conf_npmrc_mode'.
    type(hook_type), target, public :: conf_npmrc_mode_hook = hook_type(name="conf_npmrc_mode_hook")

    !> Whether `conf_mode' is running its guessed sub-mode (so it doesn't
    !> guess again).  Port of conf-mode.el conf-mode--recursing.
    logical :: conf_mode_recursing = .false.

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the Conf modes, their keymap, syntax tables and keywords (as
    !> loading conf-mode.el does).  Not a port.
    subroutine syms_of_conf_mode()
        type(status_type) :: st

        if (is_syntax_table_live(conf_mode_syntax_table)) return
        call forward_setting(conf_assignment_column)
        call forward_setting(conf_javaprop_assignment_column)
        call forward_setting(conf_colon_assignment_column)
        call forward_setting(conf_assignment_space)
        call forward_setting(conf_colon_assignment_space)
        call forward_setting(conf_space_keywords)
        call forward_setting(conf_assignment_sign)
        call forward_setting(conf_assignment_regexp)
        if (.not. allocated(conf_assignment_sign%value)) conf_assignment_sign = "="
        if (.not. allocated(conf_assignment_regexp%value)) conf_assignment_regexp = ".+?\([ "//TAB//"]*=[ "//TAB//"]*\)"
        call init_syntax_tables()
        call init_keywords()
        call init_space_keywords_table()
        call major_modes%conf_mode%define("conf_mode", "Conf[?]", conf_mode, conf_mode_hook, &
                                          syntax_table=conf_mode_syntax_table)
        conf_mode_map = major_modes%conf_mode%map
        call major_modes%conf_unix_mode%define("conf_unix_mode", "Conf[Unix]", conf_unix_mode, conf_unix_mode_hook, &
                                               parent=major_modes%conf_mode, syntax_table=conf_unix_mode_syntax_table)
        call major_modes%conf_windows_mode%define("conf_windows_mode", "Conf[WinIni]", conf_windows_mode, &
                                                  conf_windows_mode_hook, parent=major_modes%conf_mode)
        call major_modes%conf_javaprop_mode%define("conf_javaprop_mode", "Conf[JavaProp]", conf_javaprop_mode, &
                                                   conf_javaprop_mode_hook, parent=major_modes%conf_mode, &
                                                   syntax_table=conf_javaprop_mode_syntax_table)
        call major_modes%conf_space_mode%define("conf_space_mode", "Conf[Space]", conf_space_mode, conf_space_mode_hook, &
                                                parent=major_modes%conf_unix_mode)
        call major_modes%conf_colon_mode%define("conf_colon_mode", "Conf[Colon]", conf_colon_mode, conf_colon_mode_hook, &
                                                parent=major_modes%conf_unix_mode)
        call major_modes%conf_ppd_mode%define("conf_ppd_mode", "Conf[PPD]", conf_ppd_mode, conf_ppd_mode_hook, &
                                              parent=major_modes%conf_colon_mode, syntax_table=conf_ppd_mode_syntax_table)
        call major_modes%conf_xdefaults_mode%define("conf_xdefaults_mode", "Conf[Xdefaults]", conf_xdefaults_mode, &
                                                    conf_xdefaults_mode_hook, parent=major_modes%conf_colon_mode, &
                                                    syntax_table=conf_xdefaults_mode_syntax_table)
        call major_modes%conf_toml_mode%define("conf_toml_mode", "Conf[TOML]", conf_toml_mode, conf_toml_mode_hook, &
                                               parent=major_modes%conf_mode, syntax_table=conf_toml_mode_syntax_table)
        call major_modes%conf_desktop_mode%define("conf_desktop_mode", "Conf[Desktop]", conf_desktop_mode, &
                                                  conf_desktop_mode_hook, parent=major_modes%conf_unix_mode)
        call major_modes%conf_npmrc_mode%define("conf_npmrc_mode", "Conf[npmrc]", conf_npmrc_mode, conf_npmrc_mode_hook, &
                                                parent=major_modes%conf_mode, syntax_table=conf_npmrc_mode_syntax_table)
        call keymap_set(conf_mode_map, "C-c C-u", "conf_unix_mode", st)
        call keymap_set(conf_mode_map, "C-c C-w", "conf_windows_mode", st)
        call keymap_set(conf_mode_map, "C-c C-j", "conf_javaprop_mode", st)
        call keymap_set(conf_mode_map, "C-c C-s", "conf_space_keywords", st)
        call keymap_set(conf_mode_map, "C-c SPC", "conf_space_keywords", st)
        call keymap_set(conf_mode_map, "C-c C-c", "conf_colon_mode", st)
        call keymap_set(conf_mode_map, "C-c :", "conf_colon_mode", st)
        call keymap_set(conf_mode_map, "C-c C-x", "conf_xdefaults_mode", st)
        call keymap_set(conf_mode_map, "C-c C-p", "conf_ppd_mode", st)
        call keymap_set(conf_mode_map, "C-c C-q", "conf_quote_normal", st)
        call keymap_set(conf_mode_map, "C-c """, "conf_quote_normal", st)
        call keymap_set(conf_mode_map, "C-c '", "conf_quote_normal", st)
        call keymap_set(conf_mode_map, "C-c C-a", "conf_align_assignments", st)
        commands%conf_mode%run => conf_mode
        commands%conf_unix_mode%run => conf_unix_mode
        commands%conf_windows_mode%run => conf_windows_mode
        commands%conf_javaprop_mode%run => conf_javaprop_mode
        commands%conf_space_mode%run => conf_space_mode
        commands%conf_colon_mode%run => conf_colon_mode
        commands%conf_ppd_mode%run => conf_ppd_mode
        commands%conf_xdefaults_mode%run => conf_xdefaults_mode
        commands%conf_toml_mode%run => conf_toml_mode
        commands%conf_desktop_mode%run => conf_desktop_mode
        commands%conf_npmrc_mode%run => conf_npmrc_mode
        commands%conf_space_keywords%run => cmd_conf_space_keywords
        commands%conf_align_assignments%run => cmd_conf_align_assignments
        commands%conf_quote_normal%run => cmd_conf_quote_normal
        commands%conf_mode_maybe%run => conf_mode_maybe
        commands%conf_mode_maybe%interactive = .false.
    end subroutine syms_of_conf_mode

    ! Make the syntax tables of the Conf modes.  Port of the values of
    ! conf-mode.el's conf-*-mode-syntax-table variables.
    subroutine init_syntax_tables()
        type(status_type) :: st
        type(handle_type) :: t

        call make_syntax_table(conf_mode_syntax_table, st)
        t = conf_mode_syntax_table
        call modify_syntax_entry(iachar("="), iachar("="), ".", st, t)
        call modify_syntax_entry(iachar("_"), iachar("_"), "_", st, t)
        call modify_syntax_entry(iachar("-"), iachar("-"), "_", st, t)
        call modify_syntax_entry(iachar("."), iachar("."), "_", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar(";"), iachar(";"), "<", st, t)
        call modify_syntax_entry(10, 10, ">", st, t)
        call modify_syntax_entry(13, 13, ">", st, t)
        call make_syntax_table(conf_unix_mode_syntax_table, st, conf_mode_syntax_table)
        t = conf_unix_mode_syntax_table
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        ! override
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
        call make_syntax_table(conf_npmrc_mode_syntax_table, st, conf_mode_syntax_table)
        t = conf_npmrc_mode_syntax_table
        call modify_syntax_entry(iachar(";"), iachar(";"), "<", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        call make_syntax_table(conf_javaprop_mode_syntax_table, st, conf_unix_mode_syntax_table)
        call make_syntax_table(conf_ppd_mode_syntax_table, st, conf_mode_syntax_table)
        t = conf_ppd_mode_syntax_table
        call modify_syntax_entry(iachar("*"), iachar("*"), ". 1", st, t)
        call modify_syntax_entry(iachar("%"), iachar("%"), ". 2", st, t)
        ! override
        call modify_syntax_entry(iachar("'"), iachar("'"), ".", st, t)
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
        call make_syntax_table(conf_xdefaults_mode_syntax_table, st, conf_mode_syntax_table)
        t = conf_xdefaults_mode_syntax_table
        call modify_syntax_entry(iachar("!"), iachar("!"), "<", st, t)
        ! override
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
        call make_syntax_table(conf_toml_mode_syntax_table, st, conf_mode_syntax_table)
        t = conf_toml_mode_syntax_table
        call modify_syntax_entry(iachar('"'), iachar('"'), """", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar("\"), iachar("\"), "\", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        ! override
        call modify_syntax_entry(iachar(";"), iachar(";"), ".", st, t)
    end subroutine init_syntax_tables

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
    subroutine init_keywords()
        character(len=*), parameter :: SECTION = "^[ "//TAB//"]*\[\(.+\)\]"
        type(font_lock_highlight_type) :: h1(1), h2(2), h7(7)

        allocate (conf_font_lock_keywords(3))
        ! [section] (do this first because it may look like a parameter)
        h1(1) = hl(1, "font_lock_type_face")
        conf_font_lock_keywords(1) = font_lock_keyword(SECTION, highlights=h1)
        ! var=val or var[index]=val
        h2(1) = hl(1, "font_lock_variable_name_face")
        h2(2) = hl(2, "font_lock_constant_face", lax=.true.)
        conf_font_lock_keywords(2) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)\(?:\[\(.*?\)\]\)?[ "//TAB//"]*=", &
                                                       highlights=h2)
        ! section { ... } (do this last because some assign ...{...)
        h1(1) = hl(1, "font_lock_type_face", prepend=.true.)
        conf_font_lock_keywords(3) = font_lock_keyword("^[ "//TAB//"]*\([^#=:"//LF//"]+?\)[ "//TAB//LF//"]*{[^{}]*?$", &
                                                       highlights=h1)
        ! var=val
        allocate (conf_javaprop_font_lock_keywords(1))
        h7(1) = hl(1, "font_lock_variable_name_face")
        h7(2) = hl(2, "font_lock_constant_face", lax=.true.)
        h7(3) = hl(3, "font_lock_variable_name_face", lax=.true.)
        h7(4) = hl(4, "font_lock_constant_face", lax=.true.)
        h7(5) = hl(5, "font_lock_variable_name_face", lax=.true.)
        h7(6) = hl(6, "font_lock_constant_face", lax=.true.)
        h7(7) = hl(7, "font_lock_variable_name_face", lax=.true.)
        conf_javaprop_font_lock_keywords(1) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)\(?:\.\([0-9]+\)\(?:\.\(.+?\)" &
                                                                //"\(?:\.\([0-9]+\)\(?:\.\(.+?\)\(?:\.\([0-9]+\)" &
                                                                //"\(\..+?\)?\)?\)?\)?\)?\)?\([:= "//TAB//"]\|$\)", &
                                                                highlights=h7)
        allocate (conf_space_font_lock_keywords(2))
        ! [section] (do this first because it may look like a parameter)
        h1(1) = hl(1, "font_lock_type_face")
        conf_space_font_lock_keywords(1) = font_lock_keyword(SECTION, highlights=h1)
        ! section { ... } (do this first because it looks like a parameter)
        conf_space_font_lock_keywords(2) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)[ "//TAB//LF//"]*{[^{}]*?$", &
                                                             highlights=h1)
        allocate (conf_colon_font_lock_keywords(3))
        ! [section] (do this first because it may look like a parameter)
        conf_colon_font_lock_keywords(1) = font_lock_keyword(SECTION, highlights=h1)
        ! var: val
        h1(1) = hl(1, "font_lock_variable_name_face")
        conf_colon_font_lock_keywords(2) = font_lock_keyword("^[ "//TAB//"]*\(.+?\)[ "//TAB//"]*:", highlights=h1)
        ! section { ... } (do this last because some assign ...{...)
        h1(1) = hl(1, "font_lock_type_face", prepend=.true.)
        conf_colon_font_lock_keywords(3) = font_lock_keyword("^[ "//TAB//"]*\([^:"//LF//"]+\)[ "//TAB//LF//"]*{[^{}]*?$", &
                                                             highlights=h1)
        allocate (conf_toml_font_lock_keywords(3))
        ! [section] (do this first because it may look like a parameter)
        h1(1) = hl(0, "font_lock_type_face", prepend=.true.)
        conf_toml_font_lock_keywords(1) = font_lock_keyword(matcher=conf_toml_recognize_section, highlights=h1)
        ! var=val or var[index]=val
        conf_toml_font_lock_keywords(2) = font_lock_keyword("^\s-*\(.+?\)\(?:\[\(.*?\)\]\)?\s-*=", highlights=h2)
        ! Must be lower-case according to the TOML spec.
        h1(1) = hl(0, "font_lock_keyword_face")
        conf_toml_font_lock_keywords(3) = font_lock_keyword("\_<false\|true\_>", highlights=h1)
        allocate (conf_desktop_font_lock_keywords(5))
        conf_desktop_font_lock_keywords(1:3) = conf_font_lock_keywords(1:3)
        h1(1) = hl(0, "font_lock_constant_face")
        conf_desktop_font_lock_keywords(4) = font_lock_keyword("\_<false\|true\_>", highlights=h1)
        conf_desktop_font_lock_keywords(5) = font_lock_keyword("\_<%[uUfFick%]\_>", highlights=h1)
    end subroutine init_keywords

    ! Set `conf_space_keywords_table'.  Port of the value of conf-mode.el's
    ! conf-space-keywords-alist.
    subroutine init_space_keywords_table()
        if (allocated(conf_space_keywords_table)) return
        allocate (conf_space_keywords_table(9))
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

        subroutine set_entry(k, regexp, keywords)
            integer, intent(in) :: k
            character(len=*), intent(in) :: regexp, keywords

            conf_space_keywords_table(k)%regexp = regexp
            conf_space_keywords_table(k)%keywords = keywords
        end subroutine set_entry
    end subroutine init_space_keywords_table

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Align the assignments in the buffer or active region.
    !> In Transient Mark mode, if the mark is active, operate on the
    !> contents of the region.  Otherwise, operate on the whole buffer.
    !> Port of conf-mode.el:conf-align-assignments (ARG absent for nil).
    subroutine conf_align_assignments(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: column, opoint, cs, b, e, col, shortage, zv_from_end, begv
        logical :: narrowed, moved
        type(status_type) :: st
        integer :: c

        status = status_type()

        if (present(arg)) then
            column = arg
        else
            column = conf_assignment_column%value
        end if

        opoint = point()
        ! (save-restriction)
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        narrowed = .false.

        if (is_use_region()) then
            call region_beginning(b, status)
            if (status%ok()) call region_end(e, status)
            if (.not. status%ok()) return
            call narrow_to_region(b, e, status)
            if (.not. status%ok()) return
            narrowed = .true.
        end if

        call goto_char(point_min())

        do while (.not. is_eob())
            ! go before comment if within
            cs = comment_beginning()
            if (cs > 0) call goto_char(cs)

            do
                moved = forward_comment(9_int64)
                if (.not. moved) exit
            end do

            if (.not. is_eob()) then
                if (looking_at(conf_assignment_regexp%value, st)) then
                    b = match_beginning(1, st)
                    e = match_end(1, st)
                    call goto_char(b)
                    call delete_region(point(), e, status)
                    if (.not. status%ok()) exit

                    if (allocated(conf_assignment_sign%value)) then
                        if (column >= 0) then
                            col = indent_to(column, status)
                            if (.not. status%ok()) exit
                            c = before_char()
                            if (conf_assignment_space%value .and. c /= 32 .and. c /= 9) call insert(" ", status)

                            if (conf_assignment_space%value .and. .not. is_eol()) then
                                call insert(conf_assignment_sign%value//" ", status)
                            else
                                call insert(conf_assignment_sign%value, status)
                            end if
                        else
                            if (conf_assignment_space%value) then
                                call insert(" "//conf_assignment_sign%value, status)
                            else
                                call insert(conf_assignment_sign%value, status)
                            end if

                            if (status%ok() .and. .not. is_eol()) then
                                col = indent_to(-column, status)

                                if (status%ok()) then
                                    c = before_char()
                                    if (conf_assignment_space%value .and. c /= 32 .and. c /= 9) call insert(" ", status)
                                end if
                            end if
                        end if
                    else if (.not. is_eol()) then
                        if (current_column() >= abs(column)) then
                            call insert(" ", status)
                        else
                            col = indent_to(abs(column), status)
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
            call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
        end if

        call goto_char(min(max(opoint, point_min()), point_max()))
    end subroutine conf_align_assignments

    !> The character before point, -1 if none.  Not a port.
    integer function before_char()
        integer(int32) :: c
        logical :: found

        call char_before(c, found)
        before_char = -1
        if (found) before_char = c
    end function before_char

    !> The character after point, -1 if none.  Not a port.
    integer function after_char()
        integer(int32) :: c
        logical :: found

        call char_after(c, found)
        after_char = -1
        if (found) after_char = c
    end function after_char

    !> Align the assignments in the buffer or active region.
    !> In Transient Mark mode, if the mark is active, operate on the
    !> contents of the region.  Otherwise, operate on the whole buffer.
    !> Port of conf-mode.el:conf-align-assignments (the command).
    subroutine cmd_conf_align_assignments(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call conf_align_assignments(status)
        else
            call conf_align_assignments(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_conf_align_assignments

    !> Set the syntax of \=' and " to punctuation.
    !> With prefix ARG, only do it for \=' if 1, or only for " if 2.
    !> This only affects the current buffer.  Some conf files use quotes
    !> to delimit strings, while others allow quotes as simple parts of
    !> the assigned value.  In those files font locking will be wrong,
    !> and you can correct it with this command.  (Some files even do
    !> both, i.e. quotes delimit strings, except when they are
    !> unbalanced, but hey...)  Port of conf-mode.el:conf-quote-normal
    !> (ARG absent for nil).
    subroutine conf_quote_normal(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(handle_type) :: table

        call copy_syntax_table(table, status, syntax_table())
        if (.not. status%ok()) return

        if (.not. present(arg)) then
            call modify_syntax_entry(iachar("'"), iachar("'"), ".", status, table)
            call modify_syntax_entry(iachar('"'), iachar('"'), ".", status, table)
        else
            if (arg == 1) call modify_syntax_entry(iachar("'"), iachar("'"), ".", status, table)
            if (arg == 2) call modify_syntax_entry(iachar('"'), iachar('"'), ".", status, table)
        end if

        call set_syntax_table(table, status)
        if (.not. status%ok()) return
        call font_lock_flush(status)
    end subroutine conf_quote_normal

    !> Set the syntax of \=' and " to punctuation (the command).  Port of
    !> conf-mode.el:conf-quote-normal.
    subroutine cmd_conf_quote_normal(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call conf_quote_normal(status)
        else
            call conf_quote_normal(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_conf_quote_normal

    !> Try to guess sub-mode of `conf_mode' based on buffer content, and
    !> run it.  Port of conf-mode.el:conf--guess-mode.
    recursive subroutine conf_guess_mode(status)
        type(status_type), intent(out) :: status
        integer :: unix, win, equal, colon, space, jp, c
        integer(int64) :: opoint, moved, shortage, e
        type(status_type) :: st

        unix = 0
        win = 0
        equal = 0
        colon = 0
        space = 0
        jp = 0
        opoint = point()
        call goto_char(point_min())

        do while (.not. is_eob())
            moved = skip_blanks()
            c = after_char()

            if (c == iachar("#")) then
                unix = unix + 1
            else if (c == iachar(";")) then
                win = win + 1
            else if (c == iachar("[")) then
                continue ! nop
            else if (is_eol()) then
                continue ! nop
            else if (c == iachar("}")) then
                continue ! nop
            else if (looking_at("[^ "//TAB//LF//"=:]+\(?:  ?[^ "//TAB//LF//"=:]+\)*[ "//TAB//"]*[=:]", st)) then
                ! recognize at most double spaces within names
                e = match_end(0, st)

                if (before_char_at(e) == iachar("=")) then
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

        call goto_char(opoint)

        if (jp > max(unix, win, 3)) then
            call conf_javaprop_mode(status)
        else if (colon > max(equal, space)) then
            call conf_colon_mode(status)
        else if (space > max(equal, colon)) then
            call conf_space_mode(status)
        else if (win > unix) then
            call conf_windows_mode(status)
        else
            call conf_unix_mode(status)
        end if
    end subroutine conf_guess_mode

    !> (skip-chars-forward " \t\f") Not a port.
    integer(int64) function skip_blanks() result(moved)
        use syntax_fm, only: skip_chars_forward
        type(status_type) :: st

        moved = skip_chars_forward(" "//TAB//achar(12), st)
    end function skip_blanks

    !> The character before POS, -1 if none.  Not a port.
    integer function before_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        call char_before(c, found, pos)
        before_char_at = -1
        if (found) before_char_at = c
    end function before_char_at

    !> Mode for Unix and Windows Conf files and Java properties.
    !> Most conf files know only three kinds of constructs: parameter
    !> assignments optionally grouped into sections and comments.  Yet
    !> there is a great range of variation in the exact syntax of conf
    !> files.  See below for various wrapper commands that set up the
    !> details for some of the most widespread variants.
    !>
    !> This mode sets up font locking, outline, imenu and it provides
    !> alignment support through `conf_align_assignments'.  If strings
    !> come out wrong, try `conf_quote_normal'.
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
    !> If instead you start this mode with the generic `conf_mode'
    !> command, it will parse the buffer.  It will generally well
    !> identify the first four cases listed below.  If the buffer
    !> doesn't have enough contents to decide, this is identical to
    !> `conf_windows_mode' on Windows, elsewhere to `conf_unix_mode'.
    !> See also `conf_space_mode', `conf_colon_mode', `conf_javaprop_mode',
    !> `conf_ppd_mode' and `conf_xdefaults_mode'.
    !>
    !> \{conf_mode_map}
    !>
    !> Port of conf-mode.el:conf-mode (with its advice: called directly, it
    !> runs the guessed sub-mode).
    recursive subroutine conf_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved, recursing

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()

        ! Redirect to one of the submodes when called directly.  The file
        ! may have "mode: conf" in the local variable block, in which case
        ! we'll be called recursively infinitely.  Inhibit that.
        if (.not. (delaying_mode_hooks() .or. conf_mode_recursing)) then
            recursing = conf_mode_recursing
            conf_mode_recursing = .true.
            call conf_guess_mode(status)
            conf_mode_recursing = recursing
            return
        end if

        call major_modes%conf_mode%begin(saved, status)
        if (.not. status%ok()) return
        call font_lock_defaults%set_local(keywords=conf_font_lock_keywords, case_fold=.true.)
        ! Let newcomment.el decide this for itself.
        call parse_expression_ignore_comments%set_local(.true.)
        call major_modes%conf_mode%finish(saved, status)
    end subroutine conf_mode

    !> Initializations for sub-modes of `conf_mode'.
    !> COMMENT initializes `comment_start' and `comment_start_skip'.
    !> The optional arg FONT_LOCK is the value for FONT_LOCK_KEYWORDS.
    !> Port of conf-mode.el:conf-mode-initialize.
    subroutine conf_mode_initialize(comment, font_lock)
        character(len=*), intent(in) :: comment
        type(font_lock_keyword_type), intent(in), optional :: font_lock(:)

        call comment_start%set_local(comment)
        call comment_start_skip%set_local(regexp_quote(comment)//"+\s *")
        if (present(font_lock)) call font_lock_defaults%set_local(keywords=font_lock, case_fold=.true.)
    end subroutine conf_mode_initialize

    !> Conf Mode starter for Unix style Conf files.
    !> Comments start with `#'.  For details see `conf_mode'.  Port of
    !> conf-mode.el:conf-unix-mode.
    recursive subroutine conf_unix_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_unix_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#")
        call major_modes%conf_unix_mode%finish(saved, status)
    end subroutine conf_unix_mode

    !> Conf Mode starter for Windows style Conf files.
    !> Comments start with `;'.
    !> For details see `conf_mode'.  Example:
    !>
    !> ; Conf mode font-locks this right on Windows and with \[conf_windows_mode]
    !>
    !> [ExtShellFolderViews]
    !> Default={5984FFE0-28D4-11CF-AE66-08002B2E1262}
    !> {5984FFE0-28D4-11CF-AE66-08002B2E1262}={5984FFE0-28D4-11CF-AE66-08002B2E1262}
    !>
    !> [{5984FFE0-28D4-11CF-AE66-08002B2E1262}]
    !> PersistMoniker=file://Folder.htt
    !>
    !> Port of conf-mode.el:conf-windows-mode.
    recursive subroutine conf_windows_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_windows_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize(";")
        call major_modes%conf_windows_mode%finish(saved, status)
    end subroutine conf_windows_mode

    !> Conf Mode starter for Java properties files.
    !> Comments start with `#'.  Example:
    !>
    !> # Conf mode font-locks this right with \[conf_javaprop_mode] (Java properties)
    !>
    !> name:value
    !> name=value
    !> name value
    !> x.1 =
    !> x.2.y.1.z.1 =
    !> x.2.y.1.z.2.zz =
    !>
    !> Port of conf-mode.el:conf-javaprop-mode.
    recursive subroutine conf_javaprop_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_javaprop_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#", conf_javaprop_font_lock_keywords)
        call conf_assignment_column%set_local(conf_javaprop_assignment_column%value)
        call conf_assignment_regexp%set_local(".+?\([ "//TAB//"]*[=: "//TAB//"][ "//TAB//"]*\|$\)")
        call major_modes%conf_javaprop_mode%finish(saved, status)
    end subroutine conf_javaprop_mode

    !> Conf Mode starter for space separated conf files.
    !> "Assignments" are with ` '.  Keywords before the parameters are
    !> recognized according to the variable `conf_space_keywords_table'.
    !> Alternatively, you can specify a value for the file local variable
    !> `conf_space_keywords'.
    !> Use the function `conf_space_keywords' if you want to specify keywords
    !> in an interactive fashion instead.
    !>
    !> For details see `conf_mode'.  Example:
    !>
    !> # Conf mode font-locks this right with \[conf_space_mode] (space separated)
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
    recursive subroutine conf_space_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k
        type(status_type) :: st

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_space_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#", conf_space_font_lock_keywords)
        call conf_assignment_sign%make_local()
        call conf_assignment_sign%clear()

        if (allocated(buffers(cur)%filename)) then
            ! We set conf-space-keywords directly, but a value which is
            ! in the local variables list or interactively specified
            ! (see the function conf-space-keywords) takes precedence.
            call conf_space_keywords%make_local()
            call conf_space_keywords%clear()

            do k = 1, size(conf_space_keywords_table)
                if (string_match(conf_space_keywords_table(k)%regexp, buffers(cur)%filename, st) > 0) then
                    call conf_space_keywords%set_local(conf_space_keywords_table(k)%keywords)
                    exit
                end if
            end do
        end if

        call conf_space_mode_internal(status)
        if (.not. status%ok()) return
        call major_modes%conf_space_mode%finish(saved, status)
    end subroutine conf_space_mode

    !> Enter Conf Space mode using regexp KEYWORDS to match the keywords.
    !> See `conf_space_mode'.  Port of conf-mode.el:conf-space-keywords.
    recursive subroutine conf_space_keywords_command(keywords, status)
        character(len=*), intent(in) :: keywords
        type(status_type), intent(out) :: status
        logical :: saved

        call delay_mode_hooks_begin(saved)
        call conf_space_mode(status)
        call delay_mode_hooks_end(saved)
        if (.not. status%ok()) return

        if (len(keywords) == 0) then
            call conf_space_keywords%make_local()
            call conf_space_keywords%clear()
        else
            call conf_space_keywords%set_local(keywords)
        end if

        call conf_space_mode_internal(status)
        if (.not. status%ok()) return
        call run_mode_hooks(status)
    end subroutine conf_space_keywords_command

    !> Enter Conf Space mode using regexp KEYWORDS to match the keywords
    !> (the command `conf_space_keywords').  Port of
    !> conf-mode.el:conf-space-keywords.
    recursive subroutine cmd_conf_space_keywords(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: keywords

        call read_string("Conf Space keyword regexp: ", keywords, status)
        if (.not. status%ok()) return
        call conf_space_keywords_command(keywords, status)
    end subroutine cmd_conf_space_keywords

    !> Recompute what depends on `conf_space_keywords': the assignment
    !> regexp and the font-lock keywords.  Port of
    !> conf-mode.el:conf-space-mode-internal.
    subroutine conf_space_mode_internal(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        type(font_lock_highlight_type) :: h1(1), h2(2)
        integer :: n

        status = status_type()

        if (allocated(conf_space_keywords%value)) then
            call conf_assignment_regexp%set_local("\(?:"//conf_space_keywords%value//"\)[ "//TAB//"]+.+?\([ "//TAB// &
                                                  "]+\|$\)")
        else
            call conf_assignment_regexp%set_local(".+?\([ "//TAB//"]+\|$\)")
        end if

        ! The keywords' (eval ...) entry, var val.
        n = size(conf_space_font_lock_keywords)
        allocate (keywords(n + 1))
        keywords(1:n) = conf_space_font_lock_keywords

        if (allocated(conf_space_keywords%value)) then
            h2(1) = hl(1, "font_lock_keyword_face")
            h2(2) = hl(2, "font_lock_variable_name_face")
            keywords(n + 1) = font_lock_keyword("^[ "//TAB//"]*\("//conf_space_keywords%value//"\)[ "//TAB// &
                                                "]+\([^"//achar(0)//"- ]+\)", highlights=h2)
        else
            h1(1) = hl(1, "font_lock_variable_name_face")
            keywords(n + 1) = font_lock_keyword("^[ "//TAB//"]*\([^"//achar(0)//"- ]+\)", highlights=h1)
        end if

        call font_lock_defaults%set_local(keywords=keywords, case_fold=.true.)
        ! If Font Lock is already enabled, reenable it with new
        ! conf-assignment-regexp.
        if (font_lock_mode_enabled()) call font_lock_refresh_defaults(status)
    end subroutine conf_space_mode_internal

    !> Conf Mode starter for Colon files.
    !> "Assignments" are with `:'.
    !> For details see `conf_mode'.  Example:
    !>
    !> # Conf mode font-locks this right with \[conf_colon_mode] (colon)
    !>
    !> <Multi_key> <exclam> <exclam>           : "\241"        exclamdown
    !> <Multi_key> <c> <slash>                 : "\242"        cent
    !>
    !> Port of conf-mode.el:conf-colon-mode.
    recursive subroutine conf_colon_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_colon_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#", conf_colon_font_lock_keywords)
        call conf_assignment_space%set_local(conf_colon_assignment_space%value)
        call conf_assignment_column%set_local(conf_colon_assignment_column%value)
        call conf_assignment_sign%set_local(":")
        call conf_assignment_regexp%set_local(".+?\([ "//TAB//"]*:[ "//TAB//"]*\)")
        call major_modes%conf_colon_mode%finish(saved, status)
    end subroutine conf_colon_mode

    !> Conf Mode starter for Adobe/CUPS PPD files.
    !> Comments start with `*%' and "assignments" are with `:'.
    !> For details see `conf_mode'.  Example:
    !>
    !> *% Conf mode font-locks this right with \[conf_ppd_mode] (PPD)
    !>
    !> *DefaultTransfer: Null
    !> *Transfer Null.Inverse: "{ 1 exch sub }"
    !>
    !> Port of conf-mode.el:conf-ppd-mode.
    recursive subroutine conf_ppd_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_ppd_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("*%")
        call major_modes%conf_ppd_mode%finish(saved, status)
    end subroutine conf_ppd_mode

    !> Conf Mode starter for Xdefaults files.
    !> Comments start with `!' and "assignments" are with `:'.
    !> For details see `conf_mode'.  Example:
    !>
    !> ! Conf mode font-locks this right with \[conf_xdefaults_mode] (.Xdefaults)
    !>
    !> *background:                    gray99
    !> *foreground:                    black
    !>
    !> Port of conf-mode.el:conf-xdefaults-mode.
    recursive subroutine conf_xdefaults_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_xdefaults_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("!")
        call major_modes%conf_xdefaults_mode%finish(saved, status)
    end subroutine conf_xdefaults_mode

    !> Font-lock helper function for `conf_toml_mode'.
    !> Handles recognizing TOML section names, like [section],
    !> [[section]], or [something."else".section].  Port of
    !> conf-mode.el:conf-toml-recognize-section.
    subroutine conf_toml_recognize_section(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, pos, f
        type(status_type) :: st

        status = status_type()
        found = .false.
        ! Skip any number of "[" to handle things like [[section]].
        call re_search_forward("^\s-*\[+", f, st, bound=limit, noerror=SEARCH_NOERROR)
        if (.not. st%ok() .or. f <= 0) return
        start = point()
        call forward_char(st, -1_int64)
        pos = scan_groups(point(), 1_int64, 0_int64, st)

        if (st%ok() .and. pos > 0) then
            call goto_char(pos)
            end = min(limit, point() - 1)
        else
            call end_of_line(1_int64)
            end = min(limit, point())
        end if

        ! If there is a comma in the text, then we assume this is
        ! an array and not a section.  (This could be refined to
        ! look only for unquoted commas if necessary.)
        pos = point()
        call goto_char(start)
        call search_forward(",", f, st, bound=end, noerror=SEARCH_NOERROR)
        call goto_char(pos)

        if (f <= 0) then
            call set_match_data([start, end])
            found = .true.
        end if
    end subroutine conf_toml_recognize_section

    !> Conf Mode starter for TOML files.
    !> Comments start with `#' and "assignments" are with `='.
    !> For details see `conf_mode'.  Example:
    !>
    !> # Conf mode font-locks this right with \[conf_toml_mode]
    !>
    !> [entry]
    !> value = "some string"
    !>
    !> Port of conf-mode.el:conf-toml-mode.
    recursive subroutine conf_toml_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_toml_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#")
        ! Booleans are "always lowercase", so we must *not* use case
        ! folding.  Therefore, we can't set it using `conf-mode-initialize'.
        call font_lock_defaults%set_local(keywords=conf_toml_font_lock_keywords, case_fold=.false.)
        call conf_assignment_column%set_local(0)
        call conf_assignment_sign%set_local("=")
        call major_modes%conf_toml_mode%finish(saved, status)
    end subroutine conf_toml_mode

    !> Conf Mode started for freedesktop.org Desktop files.
    !> Comments start with `#' and "assignments" are with `='.
    !> For details see `conf_mode'.
    !>
    !> # Conf mode font-locks this correctly with \[conf_desktop_mode]
    !>         [Desktop Entry]
    !>         Name=GNU Image Manipulation Program
    !>         Name[oc]=Editor d'imatge GIMP
    !>         Exec=gimp-2.8 %U
    !>         Terminal=false
    !>
    !> Port of conf-mode.el:conf-desktop-mode.
    recursive subroutine conf_desktop_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_desktop_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#", conf_desktop_font_lock_keywords)
        call conf_quote_normal(status)
        if (.not. status%ok()) return
        call major_modes%conf_desktop_mode%finish(saved, status)
    end subroutine conf_desktop_mode

    !> Conf Mode starter for .npmrc files.
    !> Comments start with `#' and `;'. For details see `conf_mode'.
    !> Port of conf-mode.el:conf-npmrc-mode.
    recursive subroutine conf_npmrc_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_syntax_table_live(conf_mode_syntax_table)) call syms_of_conf_mode()
        call major_modes%conf_npmrc_mode%begin(saved, status)
        if (.not. status%ok()) return
        call conf_mode_initialize("#")
        call major_modes%conf_npmrc_mode%finish(saved, status)
    end subroutine conf_npmrc_mode

    !> Select Conf mode or XML mode according to start of file.  Port of
    !> files.el:conf-mode-maybe (XML mode is deferred: such files keep
    !> their mode).
    recursive subroutine conf_mode_maybe(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, begv, zv_from_end
        logical :: xml
        type(status_type) :: st

        status = status_type()
        opoint = point()
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call goto_char(point_min())
        xml = looking_at("<\?xml \|<!-- \|<!DOCTYPE ", st)
        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
        call goto_char(opoint)
        if (xml) return
        call conf_mode(status)
    end subroutine conf_mode_maybe
end module conf_mode_fm
