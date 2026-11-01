! f90_fm.f90 -- Fortran-90 mode (free format).
! Ported from GNU Emacs 31.1 lisp/progmodes/f90.el.
!
! Copyright (C) 1995-2026 Free Software Foundation, Inc.
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

!> F90 mode, `f90_mode', for free format Fortran: indentation of lines,
!> regions and subprograms, motion by statements, blocks and subprograms,
!> smart END statements, line breaking and joining with continuation
!> markers, auto-fill, filling, keyword case changes, abbrevs (` and a
!> letter or two) and four levels of highlighting.
!>
!> Values: `f90_smart_end' is F90_SMART_END_BLINK, _NO_BLINK or _NIL;
!> `f90_auto_keyword_case' F90_KEYWORD_CASE_NONE, _DOWNCASE_WORD,
!> _UPCASE_WORD or _CAPITALIZE_WORD; `f90_directive_comment_re' a string
!> (unallocated when unset).  Functions upstream returns as (KIND NAME)
!> give an `f90_block_type'.
!>
!> Deferred: the F90 menu, Imenu (`f90_imenu_generic_expression',
!> `f90_add_imenu_menu', `f90_imenu_type_matcher'), Hideshow
!> (`hs_block_start_regexp' with `f90_start_block_re' and
!> `f90_end_block_re'), Add Log (`f90_current_subprogram'), `f90_backslash_not_special'
!> (obsolete), the `delete_selection' property of
!> `f90_electric_insert', and `f90_cache_position' as a buffer-local
!> variable (it is set only while a region command runs).
module f90_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use callint_fm, only: PREFIX_NONE
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, command_proc, vtag, V_INTEGER
    use data_fm, only: integer_setting, logical_setting, string_setting, forward_setting
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: set_transient_map, sit_for
    use buffer_fm, only: buffers, cur, current_buffer, get_buffer_create, set_buffer, barf_if_buffer_read_only, &
                         fill_column, auto_fill_function, is_buffer_modified, restore_buffer_modified, erase_buffer
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use keymap_fm, only: keymap_set, make_sparse_keymap, define_key
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, is_syntax_table_live, parse_partial_expression, &
                         parse_state_type, scan_groups, skip_chars_forward, skip_chars_backward, forward_word
    use editfns_fm, only: interactive_region, point, point_min, point_max, goto_char, is_bol, is_eol, is_bob, is_eob, &
                          char_after, char_before, delete_region, buffer_substring, line_beginning_position, &
                          line_end_position
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line, delete_char, self_insert_command
    use insdel_fm, only: insert
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use indent_fm, only: current_column, current_indentation, move_to_column, indent_to, indent_according_to_mode, &
                         indent_line_function, indent_line_proc, indent_region_function, indent_region, indent_tabs_mode
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_forward, match_beginning, &
                         match_end, replace_match, regexp_quote, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use simple_fm, only: newline, delete_horizontal_space, fixup_whitespace, push_mark, normal_auto_fill_function, &
                         what_line_text, blink_matching_delay, deactivate_mark
    use casefiddle_fm, only: upcase_word, downcase_word, capitalize_word
    use xdisp_fm, only: message
    use window_fm, only: windows, window_start, display_buffer
    use frame_fm, only: selected_frame
    use abbrev_fm, only: abbrev_table_type, abbrev_mode, abbrev_all_caps, expand_abbrev, &
                         insert_abbrev_table_description, edit_abbrevs_mode
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_keywords_type, font_lock_keywords_level, FONT_LOCK_OVERRIDE_YES, &
                            FONT_LOCK_OVERRIDE_KEEP, FONT_LOCK_OVERRIDE_APPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_anchored, font_lock_flush
    use fill_fm, only: fill_paragraph_function, fill_comment_paragraph
    use expression_motion_fm, only: forward_expression, beginning_of_subprogram_function, end_of_subprogram_function
    use regexp_opt_fm, only: regexp_opt, REGEXP_OPT_GROUP
    use newcomment_fm, only: comment_start, comment_start_skip, comment_indent_function, comment_indent_type, &
                             comment_column, comment_auto_fill_only_comments, COMMENT_INDENT_COLUMN
    implicit none(type, external)
    private

    public :: syms_of_f90, f90_mode, f90_block_type
    public :: f90_in_string, f90_in_comment, f90_line_continued, f90_current_indentation, f90_indent_to, &
              f90_looking_at_do, f90_looking_at_select_case, f90_looking_at_if_then, f90_looking_at_associate, &
              f90_looking_at_critical, f90_looking_at_end_critical, f90_looking_at_where_or_forall, &
              f90_looking_at_type_like, f90_looking_at_program_block_start, f90_looking_at_program_block_end, &
              f90_comment_indent, f90_calculate_indent, f90_previous_statement, f90_next_statement, &
              f90_beginning_of_subprogram, f90_end_of_subprogram, f90_end_of_block, f90_beginning_of_block, &
              f90_next_block, f90_previous_block, f90_mark_subprogram, f90_comment_region_command, f90_indent_line, &
              f90_indent_new_line, f90_indent_region, f90_indent_subprogram, f90_break_line, f90_do_auto_fill, &
              f90_join_lines, f90_fill_region, f90_fill_paragraph, f90_match_end, f90_insert_end, f90_change_keywords, &
              f90_typedef_matcher, f90_typedec_matcher, f90_font_lock_n

    !> `f90_smart_end' values: F90_SMART_END_BLINK, F90_SMART_END_NO_BLINK and F90_SMART_END_NO.
    integer, parameter, public :: F90_SMART_END_NO = 0, F90_SMART_END_BLINK = 1, F90_SMART_END_NO_BLINK = 2

    !> `f90_auto_keyword_case' values (and the CHANGE_WORD argument of
    !> `f90_change_keywords').
    integer, parameter, public :: F90_KEYWORD_CASE_NONE = 0, F90_KEYWORD_CASE_DOWNCASE_WORD = 1, &
                                  F90_KEYWORD_CASE_UPCASE_WORD = 2, F90_KEYWORD_CASE_CAPITALIZE_WORD = 3

    !> What `f90_present_statement_cont' returns: single, begin, end, middle.
    integer, parameter, public :: CONT_SINGLE = 0, CONT_BEGIN = 1, CONT_END = 2, CONT_MIDDLE = 3

    !> What upstream's f90-looking-at-* functions return: (KIND NAME), or
    !> nothing (FOUND .false.).  NAME unallocated means none.
    type :: f90_block_type
        logical :: found = .false.
        character(len=:), allocatable :: kind, name
    end type f90_block_type

    !> Extra indentation applied to DO blocks.  Port of f90.el f90-do-indent.
    type(integer_setting), target, public :: f90_do_indent = integer_setting(name="f90_do_indent", value=3)
    !> Extra indentation applied to IF, SELECT CASE, WHERE and FORALL blocks.
    !> Port of f90.el f90-if-indent.
    type(integer_setting), target, public :: f90_if_indent = integer_setting(name="f90_if_indent", value=3)
    !> Extra indentation applied to TYPE, ENUM, INTERFACE and BLOCK DATA blocks.
    !> Port of f90.el f90-type-indent.
    type(integer_setting), target, public :: f90_type_indent = integer_setting(name="f90_type_indent", value=3)
    !> Extra indentation applied to PROGRAM, MODULE, SUBROUTINE, FUNCTION blocks.
    !> Port of f90.el f90-program-indent.
    type(integer_setting), target, public :: f90_program_indent = integer_setting(name="f90_program_indent", value=2)
    !> Extra indentation applied to ASSOCIATE blocks.  Port of f90.el
    !> f90-associate-indent.
    type(integer_setting), target, public :: f90_associate_indent = integer_setting(name="f90_associate_indent", value=2)
    !> Extra indentation applied to BLOCK, CRITICAL blocks.  Port of f90.el
    !> f90-critical-indent.
    type(integer_setting), target, public :: f90_critical_indent = integer_setting(name="f90_critical_indent", value=2)
    !> Extra indentation applied to continuation lines.  Port of f90.el
    !> f90-continuation-indent.
    type(integer_setting), target, public :: f90_continuation_indent = &
                                             integer_setting(name="f90_continuation_indent", value=5)
    !> String inserted by \[f90_comment_region] at start of each line in region.
    !> Port of f90.el f90-comment-region.
    type(string_setting), target, public :: f90_comment_region = string_setting(name="f90_comment_region")
    !> Regexp matching comments to indent as code.  Port of f90.el
    !> f90-indented-comment-re.
    type(string_setting), target, public :: f90_indented_comment_re = string_setting(name="f90_indented_comment_re")
    !> Regexp of comment-like directive like "!HPF\\$", not to be indented.
    !> Port of f90.el f90-directive-comment-re.
    type(string_setting), target, public :: f90_directive_comment_re = string_setting(name="f90_directive_comment_re")
    !> True gives automatic insertion of `&' at start of continuation line.
    !> Port of f90.el f90-beginning-ampersand.
    type(logical_setting), target, public :: f90_beginning_ampersand = &
                                             logical_setting(name="f90_beginning_ampersand", value=.true.)
    !> Qualification of END statements according to the matching block start.
    !> For example, change the END that closes an IF block to END IF.
    !> If the block has a label, add it as well (unless `f90_smart_end_names'
    !> says not to).  Allowed values are F90_SMART_END_BLINK,
    !> F90_SMART_END_NO_BLINK, and F90_SMART_END_NO.  If F90_SMART_END_NO,
    !> nothing is done.  The other two settings have the same effect, but F90_SMART_END_BLINK
    !> additionally blinks the cursor to the start of the block.  Port of
    !> f90.el f90-smart-end.
    type(integer_setting), target, public :: f90_smart_end = integer_setting(name="f90_smart_end", &
                                                                             value=F90_SMART_END_BLINK)
    !> Whether completion of END statements should insert optional block names.
    !> For example, when closing a "PROGRAM PROGNAME" block, "PROGNAME" is
    !> optional in the "END PROGRAM" statement.  The same is true for modules,
    !> functions, subroutines, and types.  Some people prefer to omit the name
    !> from the END statement, since it makes it easier to change the name.
    !>
    !> This does not apply to named DO, IF, etc. blocks.  If such blocks
    !> start with a label, they must end with one.
    !>
    !> If an end statement has a name that does not match the start, it is always
    !> corrected, regardless of the value of this variable.  Port of f90.el
    !> f90-smart-end-names.
    type(logical_setting), target, public :: f90_smart_end_names = &
                                             logical_setting(name="f90_smart_end_names", value=.true.)
    !> Regexp matching delimiter characters at which lines may be broken.
    !> There are some common two-character tokens where one or more of
    !> the members matches this regexp.  Although Fortran allows breaks
    !> within lexical tokens (provided the next line has a beginning ampersand),
    !> the constant `f90_no_break_re' ensures that such tokens are not split.
    !> Port of f90.el f90-break-delimiters.
    type(string_setting), target, public :: f90_break_delimiters = string_setting(name="f90_break_delimiters")
    !> True causes `f90_do_auto_fill' to break lines before delimiters.
    !> Port of f90.el f90-break-before-delimiters.
    type(logical_setting), target, public :: f90_break_before_delimiters = &
                                             logical_setting(name="f90_break_before_delimiters", value=.true.)
    !> Automatic case conversion of keywords.
    !> The options are F90_KEYWORD_CASE_DOWNCASE_WORD, _UPCASE_WORD,
    !> _CAPITALIZE_WORD and _NIL.  Port of f90.el f90-auto-keyword-case.
    type(integer_setting), target, public :: f90_auto_keyword_case = &
                                             integer_setting(name="f90_auto_keyword_case", value=F90_KEYWORD_CASE_NONE)
    !> If true, line numbers are not left justified.  Port of f90.el
    !> f90-leave-line-no.
    type(logical_setting), target, public :: f90_leave_line_no = logical_setting(name="f90_leave_line_no", value=.false.)
    !> Hook run when entering F90 mode.  Port of f90.el f90-mode-hook.
    type(hook_type), target, public :: f90_mode_hook = hook_type(name="f90_mode_hook")

    !> Regexp used by the function `f90_change_keywords'.  Port of f90.el
    !> f90-keywords-re.
    character(len=:), allocatable, public :: f90_keywords_re
    !> Keyword-regexp for font-lock level >= 3.  Port of f90.el
    !> f90-keywords-level-3-re.
    character(len=:), allocatable, public :: f90_keywords_level_3_re
    !> Regexp whose first part matches F90 intrinsic procedures.  Port of
    !> f90.el f90-procedures-re.
    character(len=:), allocatable, public :: f90_procedures_re
    !> Regexp matching intrinsic operators.  Port of f90.el f90-operators-re.
    character(len=:), allocatable, public :: f90_operators_re
    !> Regexp for all HPF keywords, procedures and directives.  Port of
    !> f90.el f90-hpf-keywords-re.
    character(len=:), allocatable, public :: f90_hpf_keywords_re
    !> Regexp for Fortran intrinsic constants.  Port of f90.el
    !> f90-constants-re.
    character(len=:), allocatable, public :: f90_constants_re
    !> Regexp potentially indicating a "block" of F90 code.  Port of f90.el
    !> f90-blocks-re.
    character(len=:), allocatable, public :: f90_blocks_re
    !> Regexp used to locate the start/end of a "subprogram".  Port of
    !> f90.el f90-program-block-re.
    character(len=:), allocatable, public :: f90_program_block_re
    !> Regexp matching an ELSE IF, ELSEWHERE, CASE, CLASS/TYPE IS statement.
    !> Port of f90.el f90-else-like-re.
    character(len=*), parameter, public :: f90_else_like_re = "\(else\([ "//achar(9)//"]*if\|where\)?\|case[ " &
                                           //achar(9)//"]*\(default\|(\)\|\(class\|type\)[ " &
                                           //achar(9)//"]*is[ "//achar(9)//"]*(\|class[ " &
                                           //achar(9)//"]*default\)"
    !> Regexp matching the end of an IF, SELECT, WHERE, FORALL block.  Port of
    !> f90.el f90-end-if-re.
    character(len=:), allocatable, public :: f90_end_if_re
    !> Regexp matching the end of a TYPE, ENUM, INTERFACE, BLOCK DATA section.
    !> Port of f90.el f90-end-type-re.
    character(len=*), parameter, public :: f90_end_type_re = "end[ "//achar(9)//"]*\(type\|enum\|interface\|block[ " &
                                           //achar(9)//"]*data\)\_>"
    !> Regexp matching the end of an ASSOCIATE block.  Port of f90.el
    !> f90-end-associate-re.
    character(len=*), parameter, public :: f90_end_associate_re = "end[ "//achar(9)//"]*associate\_>"
    !> Regexp matching the definition of a derived type.  Port of f90.el
    !> f90-type-def-re.
    character(len=*), parameter, public :: f90_type_def_re = "\_<\(type\)\_>\(?:\(?:[^()"//achar(10)//"]*\|" &
                                           //".*,[ "//achar(9)//"]*\(?:bind\|extends\)[ " &
                                           //achar(9)//"]*(.*).*\)::\)?[ "//achar(9) &
                                           //"]*\(\(?:\sw\|\s_\)+\)"
    !> Regexp matching a CLASS/TYPE IS statement.  Port of f90.el
    !> f90-typeis-re.
    character(len=*), parameter, public :: f90_typeis_re = "\_<\(class\|type\)[ "//achar(9)//"]*is[ "//achar(9)//"]*("
    !> Regexp specifying two-character tokens not to split when breaking lines.
    !> Each token has one or more of the characters from `f90_break_delimiters'.
    !> Note that if only one of the characters is from that variable,
    !> then the presence of the token here allows a line-break before or
    !> after the other character, where a break would not normally be
    !> allowed.  This minor issue currently only affects "(/" and "/)".
    !> Port of f90.el f90-no-break-re.
    character(len=:), allocatable, public :: f90_no_break_re
    !> Regexp matching the end of an F90 "block", from the line start.
    !> Used in the F90 entry in `hs_block_end_regexp'.  Port of f90.el
    !> f90-end-block-re.
    character(len=:), allocatable, public :: f90_end_block_re
    !> Regexp matching the start of an F90 "block", from the line start.
    !> A simple regexp cannot do this in fully correct fashion, so this
    !> tries to strike a compromise between complexity and flexibility.
    !> Used in the F90 entry in `hs_block_start_regexp'.  Port of f90.el
    !> f90-start-block-re.
    character(len=:), allocatable, public :: f90_start_block_re
    !> Block types where including the name in the end statement is optional.
    !> Port of f90.el f90-end-block-optional-name.
    character(len=*), parameter, public :: f90_end_block_optional_name(5) = &
                                           [character(len=10) :: "program", "module", "subroutine", "function", "type"]

    !> This does fairly subdued highlighting of comments and function calls.
    !> Port of f90.el f90-font-lock-keywords-1.
    type(font_lock_keyword_type), allocatable, public :: f90_font_lock_keywords_1(:)
    !> Highlights declarations, do-loops and other constructs.  Port of
    !> f90.el f90-font-lock-keywords-2.
    type(font_lock_keyword_type), allocatable, public :: f90_font_lock_keywords_2(:)
    !> Highlights all F90 keywords and intrinsic procedures.  Port of
    !> f90.el f90-font-lock-keywords-3.
    type(font_lock_keyword_type), allocatable, public :: f90_font_lock_keywords_3(:)
    !> Highlights all F90 and HPF keywords and constants.  Port of f90.el
    !> f90-font-lock-keywords-4.
    type(font_lock_keyword_type), allocatable, public :: f90_font_lock_keywords_4(:)
    !> Default expressions to highlight in F90 mode.
    !> Can be overridden by the value of `font_lock_maximum_decoration'.
    !> Port of f90.el f90-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: f90_font_lock_keywords(:)

    !> Syntax table used in F90 mode.  Port of f90.el f90-mode-syntax-table.
    type(handle_type), public :: f90_mode_syntax_table
    !> Keymap used in F90 mode.  Port of f90.el f90-mode-map.
    type(handle_type), public :: f90_mode_map
    !> Abbrev table for F90 mode.  Port of f90.el f90-mode-abbrev-table.
    type(abbrev_table_type), target, public :: f90_mode_abbrev_table

    !> Temporary position used to speed up region operations (0 for none).
    !> Port of f90.el f90-cache-position.
    integer(int64), public :: f90_cache_position = 0

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)
    !> \(?:\sw\|\s_\)
    character(len=*), parameter :: SYMRE = "\(?:\sw\|\s_\)"
    ! A character that is neither a continuation, a comment start nor a
    ! newline.
    character(len=*), parameter :: NOT_CONT = "[^"//achar(38)//"!"//achar(10)//"]"
    !> The help character (upstream's `help-char', C-h).
    integer(int64), parameter :: HELP_CHAR = 8

contains

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define F90 mode: its regexps, syntax table, keymap, abbrevs and
    !> keywords (as loading f90.el does).  Not a port.
    subroutine syms_of_f90()
        type(status_type) :: st

        if (is_syntax_table_live(f90_mode_syntax_table)) return
        call forward_setting(f90_do_indent)
        call forward_setting(f90_if_indent)
        call forward_setting(f90_type_indent)
        call forward_setting(f90_program_indent)
        call forward_setting(f90_associate_indent)
        call forward_setting(f90_critical_indent)
        call forward_setting(f90_continuation_indent)
        call forward_setting(f90_comment_region)
        call forward_setting(f90_indented_comment_re)
        call forward_setting(f90_directive_comment_re)
        call forward_setting(f90_beginning_ampersand)
        call forward_setting(f90_smart_end)
        call forward_setting(f90_smart_end_names)
        call forward_setting(f90_break_delimiters)
        call forward_setting(f90_break_before_delimiters)
        call forward_setting(f90_auto_keyword_case)
        call forward_setting(f90_leave_line_no)
        if (.not. allocated(f90_comment_region%value)) f90_comment_region = "!!$"
        if (.not. allocated(f90_indented_comment_re%value)) f90_indented_comment_re = "!"
        if (.not. allocated(f90_directive_comment_re%value)) f90_directive_comment_re = "!hpf\$"
        if (.not. allocated(f90_break_delimiters%value)) f90_break_delimiters = "[-+\*/><=,% "//TAB//"]"
        call init_regexps()
        call init_syntax_table()
        call init_keywords()
        call init_abbrevs()
        call major_modes%f90_mode%define("f90_mode", "F90", f90_mode, f90_mode_hook, parent=major_modes%prog_mode, &
                                         syntax_table=f90_mode_syntax_table, abbrev_table=f90_mode_abbrev_table)
        f90_mode_map = major_modes%f90_mode%map
        call keymap_set(f90_mode_map, "`", "f90_abbrev_start", st)
        call keymap_set(f90_mode_map, "C-c ;", "f90_comment_region", st)
        call keymap_set(f90_mode_map, "C-M-a", "f90_beginning_of_subprogram", st)
        call keymap_set(f90_mode_map, "C-M-e", "f90_end_of_subprogram", st)
        call keymap_set(f90_mode_map, "C-M-h", "f90_mark_subprogram", st)
        call keymap_set(f90_mode_map, "C-M-n", "f90_end_of_block", st)
        call keymap_set(f90_mode_map, "C-M-p", "f90_beginning_of_block", st)
        call keymap_set(f90_mode_map, "C-M-q", "f90_indent_subprogram", st)
        call keymap_set(f90_mode_map, "C-j", "f90_indent_new_line", st)
        call keymap_set(f90_mode_map, "C-c RET", "f90_break_line", st)
        call keymap_set(f90_mode_map, "C-c C-a", "f90_previous_block", st)
        call keymap_set(f90_mode_map, "C-c C-e", "f90_next_block", st)
        call keymap_set(f90_mode_map, "C-c C-d", "f90_join_lines", st)
        call keymap_set(f90_mode_map, "C-c C-f", "f90_fill_region", st)
        call keymap_set(f90_mode_map, "C-c C-p", "f90_previous_statement", st)
        call keymap_set(f90_mode_map, "C-c C-n", "f90_next_statement", st)
        call keymap_set(f90_mode_map, "C-c ]", "f90_insert_end", st)
        call keymap_set(f90_mode_map, "C-c C-w", "f90_insert_end", st)
        call keymap_set(f90_mode_map, ",", "f90_electric_insert", st)
        call keymap_set(f90_mode_map, "+", "f90_electric_insert", st)
        call keymap_set(f90_mode_map, "-", "f90_electric_insert", st)
        call keymap_set(f90_mode_map, "*", "f90_electric_insert", st)
        call keymap_set(f90_mode_map, "/", "f90_electric_insert", st)
        commands%f90_mode%execute => f90_mode
        commands%f90_font_lock_1%execute => cmd_f90_font_lock_1
        commands%f90_font_lock_2%execute => cmd_f90_font_lock_2
        commands%f90_font_lock_3%execute => cmd_f90_font_lock_3
        commands%f90_font_lock_4%execute => cmd_f90_font_lock_4
        commands%f90_electric_insert%execute => cmd_f90_electric_insert
        commands%f90_calculate_indent%execute => cmd_f90_calculate_indent
        commands%f90_previous_statement%execute => cmd_f90_previous_statement
        commands%f90_next_statement%execute => cmd_f90_next_statement
        commands%f90_beginning_of_subprogram%execute => cmd_f90_beginning_of_subprogram
        commands%f90_end_of_subprogram%execute => cmd_f90_end_of_subprogram
        commands%f90_end_of_block%execute => cmd_f90_end_of_block
        commands%f90_beginning_of_block%execute => cmd_f90_beginning_of_block
        commands%f90_next_block%execute => cmd_f90_next_block
        commands%f90_previous_block%execute => cmd_f90_previous_block
        commands%f90_mark_subprogram%execute => cmd_f90_mark_subprogram
        commands%f90_comment_region%execute => cmd_f90_comment_region
        commands%f90_indent_line%execute => cmd_f90_indent_line
        commands%f90_indent_new_line%execute => cmd_f90_indent_new_line
        commands%f90_indent_region%execute => cmd_f90_indent_region
        commands%f90_indent_subprogram%execute => cmd_f90_indent_subprogram
        commands%f90_break_line%execute => cmd_f90_break_line
        commands%f90_do_auto_fill%execute => cmd_f90_do_auto_fill
        commands%f90_join_lines%execute => cmd_f90_join_lines
        commands%f90_fill_region%execute => cmd_f90_fill_region
        commands%f90_fill_paragraph%execute => cmd_f90_fill_paragraph
        commands%f90_match_end%execute => cmd_f90_match_end
        commands%f90_insert_end%execute => cmd_f90_insert_end
        commands%f90_abbrev_start%execute => cmd_f90_abbrev_start
        commands%f90_abbrev_help%execute => cmd_f90_abbrev_help
        commands%f90_upcase_keywords%execute => cmd_f90_upcase_keywords
        commands%f90_capitalize_keywords%execute => cmd_f90_capitalize_keywords
        commands%f90_downcase_keywords%execute => cmd_f90_downcase_keywords
        commands%f90_upcase_region_keywords%execute => cmd_f90_upcase_region_keywords
        commands%f90_capitalize_region_keywords%execute => cmd_f90_capitalize_region_keywords
        commands%f90_downcase_region_keywords%execute => cmd_f90_downcase_region_keywords
    end subroutine syms_of_f90

    ! Set the regexps of F90 mode.  Port of the values of f90.el's regexp
    ! variables.
    subroutine init_regexps()
        character(len=24), parameter :: keywords(112) = [character(len=24) :: &
                                                         "allocatable", "allocate", "assign", "assignment", "backspace", &
                                                         "block", "call", "case", "character", "close", "common", "complex", &
                                                         "contains", "continue", "cycle", "data", "deallocate", &
                                                         "dimension", "do", "double", "else", "elseif", "elsewhere", "end", &
                                                         "enddo", "endfile", "endif", "entry", "equivalence", "exit", &
                                                         "external", "forall", "format", "function", "goto", "if", &
                                                         "implicit", "include", "inquire", "integer", "intent", &
                                                         "interface", "intrinsic", "logical", "module", "namelist", "none", &
                                                         "nullify", "only", "open", "operator", "optional", "parameter", &
                                                         "pause", "pointer", "precision", "print", "private", "procedure", &
                                                         "program", "public", "read", "real", "recursive", "result", "return", &
                                                         "rewind", "save", "select", "sequence", "stop", "subroutine", &
                                                         "target", "then", "type", "use", "where", "while", "write", &
                                                         "elemental", "pure", &
                                                         "abstract", "associate", "asynchronous", "bind", "class", &
                                                         "deferred", "enum", "enumerator", "extends", "extends_type_of", &
                                                         "final", "generic", "import", "non_intrinsic", "non_overridable", &
                                                         "nopass", "pass", "protected", "same_type_as", "value", "volatile", &
                                                         "contiguous", "submodule", "concurrent", "codimension", &
                                                         "sync all", "sync memory", "critical", "image_index", "error stop", &
                                                         "impure"]
        character(len=24), parameter :: level3(53) = [character(len=24) :: &
                                                      "allocatable", "allocate", "assign", "assignment", "backspace", &
                                                      "close", "deallocate", "dimension", "endfile", "entry", "equivalence", &
                                                      "external", "inquire", "intent", "intrinsic", "nullify", "only", "open", &
                                                      "operator", "optional", "parameter", "pause", "pointer", "print", "private", &
                                                      "public", "read", "recursive", "result", "rewind", "save", "select", &
                                                      "sequence", "target", "write", &
                                                      "elemental", "pure", &
                                                    "abstract", "deferred", "import", "final", "non_intrinsic", "non_overridable", &
                                                      "nopass", "pass", "protected", "value", "volatile", &
                                                      "contiguous", "concurrent", "codimension", "sync all", "sync memory"]
        character(len=32), parameter :: procedures(188) = [character(len=32) :: &
                                                           "abs", "achar", "acos", "adjustl", "adjustr", "aimag", "aint", &
                                                           "all", "allocated", "anint", "any", "asin", "associated", &
                                                           "atan", "atan2", "bit_size", "btest", "ceiling", "char", "cmplx", &
                                                           "conjg", "cos", "cosh", "count", "cshift", "date_and_time", "dble", &
                                                           "digits", "dim", "dot_product", "dprod", "eoshift", "epsilon", &
                                                           "exp", "exponent", "floor", "fraction", "huge", "iachar", "iand", &
                                                           "ibclr", "ibits", "ibset", "ichar", "ieor", "index", "int", "ior", &
                                                           "ishft", "ishftc", "kind", "lbound", "len", "len_trim", "lge", "lgt", &
                                                           "lle", "llt", "log", "log10", "logical", "matmul", "max", &
                                                           "maxexponent", "maxloc", "maxval", "merge", "min", "minexponent", &
                                                           "minloc", "minval", "mod", "modulo", "mvbits", "nearest", "nint", &
                                                           "not", "pack", "precision", "present", "product", "radix", &
                                                           "random_number", "random_seed", "range", &
                                                           "repeat", "reshape", "rrspacing", "scale", "scan", &
                                                           "selected_int_kind", "selected_real_kind", "set_exponent", &
                                                           "shape", "sign", "sin", "sinh", "size", "spacing", "spread", "sqrt", &
                                                           "sum", "system_clock", "tan", "tanh", "tiny", "transfer", &
                                                           "transpose", "trim", "ubound", "unpack", "verify", &
                                                           "null", "cpu_time", &
                                                           "move_alloc", "command_argument_count", "get_command", &
                                                           "get_command_argument", "get_environment_variable", &
                                                           "selected_char_kind", "wait", "flush", "new_line", &
                                                           "extends", "extends_type_of", "same_type_as", "bind", &
                                                           "ieee_support_underflow_control", "ieee_get_underflow_mode", &
                                                           "ieee_set_underflow_mode", &
                                                           "c_loc", "c_funloc", "c_associated", "c_f_pointer", &
                                                           "c_f_procpointer", &
                                                           "bge", "bgt", "ble", "blt", "dshiftl", "dshiftr", "leadz", "popcnt", &
                                                           "poppar", "trailz", "maskl", "maskr", "shifta", "shiftl", "shiftr", &
                                                           "merge_bits", "iall", "iany", "iparity", "storage_size", &
                                                           "bessel_j0", "bessel_j1", "bessel_jn", &
                                                           "bessel_y0", "bessel_y1", "bessel_yn", &
                                                           "erf", "erfc", "erfc_scaled", "gamma", "hypot", "log_gamma", &
                                                           "norm2", "parity", "findloc", "is_contiguous", &
                                                           "sync images", "lock", "unlock", "image_index", &
                                                           "lcobound", "ucobound", "num_images", "this_image", &
                                                           "acosh", "asinh", "atanh", &
                                                           "atomic_define", "atomic_ref", "execute_command_line", &
                                                           "compiler_options", "compiler_version", &
                                                           "c_sizeof"]
        character(len=8), parameter :: operators(13) = [character(len=8) :: "and", "eq", "eqv", "false", "ge", "gt", &
                                                                                      "le", "lt", "ne", "neqv", "not", "or", "true"]
        character(len=24), parameter :: hpf(67) = [character(len=24) :: &
                                                   "all_prefix", "all_scatter", "all_suffix", "any_prefix", &
                                                   "any_scatter", "any_suffix", "copy_prefix", "copy_scatter", &
                                                   "copy_suffix", "count_prefix", "count_scatter", "count_suffix", &
                                                   "grade_down", "grade_up", &
                                                   "hpf_alignment", "hpf_distribution", "hpf_template", "iall", "iall_prefix", &
                                                   "iall_scatter", "iall_suffix", "iany", "iany_prefix", "iany_scatter", &
                                                   "iany_suffix", "ilen", "iparity", "iparity_prefix", &
                                                   "iparity_scatter", "iparity_suffix", "leadz", "maxval_prefix", &
                                                   "maxval_scatter", "maxval_suffix", "minval_prefix", "minval_scatter", &
                                                   "minval_suffix", "number_of_processors", "parity", &
                                                   "parity_prefix", "parity_scatter", "parity_suffix", "popcnt", "poppar", &
                                                   "processors_shape", "product_prefix", "product_scatter", &
                                                   "product_suffix", "sum_prefix", "sum_scatter", "sum_suffix", &
                                                   "align", "distribute", "dynamic", "independent", "inherit", "processors", &
                                                   "realign", "redistribute", "template", &
                                                   "block", "cyclic", "extrinsic", "new", "onto", "pure", "with"]
        character(len=32), parameter :: constants(68) = [character(len=32) :: &
                                                         "iso_fortran_env", &
                                                         "input_unit", "output_unit", "error_unit", &
                                                         "iostat_end", "iostat_eor", &
                                                         "numeric_storage_size", "character_storage_size", &
                                                         "file_storage_size", &
                                                         "iso_c_binding", &
                                                         "c_int", "c_short", "c_long", "c_long_long", "c_signed_char", &
                                                         "c_size_t", &
                                                         "c_int8_t", "c_int16_t", "c_int32_t", "c_int64_t", &
                                                         "c_int_least8_t", "c_int_least16_t", "c_int_least32_t", &
                                                         "c_int_least64_t", &
                                                         "c_int_fast8_t", "c_int_fast16_t", "c_int_fast32_t", &
                                                         "c_int_fast64_t", &
                                                         "c_intmax_t", "c_intptr_t", &
                                                         "c_float", "c_double", "c_long_double", &
                                                         "c_float_complex", "c_double_complex", "c_long_double_complex", &
                                                         "c_bool", "c_char", &
                                                         "c_null_char", "c_alert", "c_backspace", "c_form_feed", &
                                                         "c_new_line", "c_carriage_return", "c_horizontal_tab", &
                                                         "c_vertical_tab", &
                                                         "c_ptr", "c_funptr", "c_null_ptr", "c_null_funptr", &
                                                         "ieee_exceptions", &
                                                         "ieee_arithmetic", &
                                                         "ieee_features", &
                                                         "character_kinds", "int8", "int16", "int32", "int64", &
                                                         "integer_kinds", "iostat_inquire_internal_unit", &
                                                         "logical_kinds", "real_kinds", "real32", "real64", "real128", &
                                                         "lock_type", "atomic_int_kind", "atomic_logical_kind"]
        character(len=12), parameter :: blocks(16) = [character(len=12) :: "do", "if", "interface", "function", &
                                                                     "module", "program", "select", "subroutine", "type", "where", &
                                                                    "forall", "enum", "associate", "submodule", "block", "critical"]
        character(len=12), parameter :: program_blocks(5) = [character(len=12) :: "program", "module", "subroutine", &
                                                                                                            "function", "submodule"]
        character(len=8), parameter :: end_ifs(4) = [character(len=8) :: "if", "select", "where", "forall"]
        character(len=4), parameter :: no_breaks(9) = [character(len=4) :: "**", "//", "=>", ">=", "<=", "==", "/=", &
                                                                                                                         "(/", "/)"]
        character(len=12), parameter :: end_blocks(16) = [character(len=12) :: "do", "if", "forall", "function", &
                                                                         "interface", "module", "program", "select", "subroutine", &
                                                                       "type", "where", "enum", "associate", "submodule", "block", &
                                                                                                                         "critical"]

        f90_keywords_re = "\_<"//regexp_opt(keywords)//"\_>"
        f90_keywords_level_3_re = "\_<"//regexp_opt(level3)//"\_>"
        ! A left parenthesis to avoid highlighting non-procedures.
        f90_procedures_re = "\_<"//regexp_opt(procedures, REGEXP_OPT_GROUP)//"[ "//TAB//"]*("
        f90_operators_re = "\."//regexp_opt(operators, REGEXP_OPT_GROUP)//"\."
        f90_hpf_keywords_re = "\_<"//regexp_opt(hpf)//"\_>"
        f90_constants_re = "\_<"//regexp_opt(constants)//"\_>"
        f90_blocks_re = "\(\(?:block[ "//TAB//"]*data\|"//regexp_opt(blocks)//"\)\_>\)"
        f90_program_block_re = regexp_opt(program_blocks, REGEXP_OPT_GROUP)
        f90_end_if_re = "end[ "//TAB//"]*"//regexp_opt(end_ifs, REGEXP_OPT_GROUP)//"\_>"
        f90_no_break_re = regexp_opt(no_breaks, REGEXP_OPT_GROUP)
        f90_end_block_re = "^[ "//TAB//"0-9]*\_<end[ "//TAB//"]*"//regexp_opt(end_blocks, REGEXP_OPT_GROUP)//"\_>"
        ! Ignore the fact that FUNCTION, SUBROUTINE, WHERE, FORALL have a
        ! following "(".  DO, CASE, IF can have labels.
        f90_start_block_re = "^[ "//TAB//"0-9]*" & ! statement number
                             //"\(\(" &
                             //"\("//SYMRE//"+[ "//TAB//"]*:[ "//TAB//"]*\)?" & ! structure label
                             //"\(do\|select[ "//TAB//"]*\(case\|type\)\|" &
                             ! See comments in fortran-start-block-re for the problems of IF.
                             //"if[ "//TAB//"]*(\(.*\|" &
                             //".*"//LF//"\([^if]*\([^i].\|.[^f]\|.\_>\)\)\)\_<then\|" &
                             ! Distinguish WHERE block from isolated WHERE.
                             //"\(where\|forall\)[ "//TAB//"]*(.*)[ "//TAB//"]*\(!\|$\)\)\)" &
                             //"\|" &
                             ! Avoid F2003 "type is" in "select type",
                             ! and also variables of derived type "type (foo)".
                             ! "type, foo" must be a block (?).
                             ! And a partial effort to avoid "class default".
                             //"\(?:type\|class\)[ "//TAB//",]\(" &
                             //"[^id(!"//LF//"""& "//TAB//"]\|" & ! not-id(
                             //"i[^s!"//LF//"""& "//TAB//"]\|" & ! i not-s
                             //"d[^e!"//LF//"""& "//TAB//"]\|" & ! d not-e
                             //"de[^f!"//LF//"""& "//TAB//"]\|" & ! de not-f
                             //"def[^a!"//LF//"""& "//TAB//"]\|" & ! def not-a
                             //"\(?:is\|default\)"//SYMRE//"\)\|" &
                             ! "abstract interface" is F2003; "submodule" is F2008.
                             //"program\|\(?:abstract[ "//TAB//"]*\)?interface\|\(?:sub\)?module\|" &
                             ! "enum", but not "enumerator".
                             //"function\|subroutine\|enum[^e]\|associate\|block\|critical" &
                             //"\)" &
                             //"[ "//TAB//"]*"
    end subroutine init_regexps

    ! Make `f90_mode_syntax_table'.  Port of the value of f90.el's
    ! f90-mode-syntax-table.
    subroutine init_syntax_table()
        type(status_type) :: st
        type(handle_type) :: t

        call make_syntax_table(f90_mode_syntax_table, st)
        t = f90_mode_syntax_table
        call modify_syntax_entry(iachar("!"), iachar("!"), "<", st, t) ! begin comment
        call modify_syntax_entry(10, 10, ">", st, t) ! end comment
        call modify_syntax_entry(iachar("_"), iachar("_"), "_", st, t) ! underscore in names
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t) ! string quote
        call modify_syntax_entry(iachar('"'), iachar('"'), """", st, t) ! string quote
        ! FIXME: We used to set ` to word syntax for the benefit of abbrevs, but
        ! we do not need it any more.  Not sure if it should be "_" or "." now.
        call modify_syntax_entry(iachar("`"), iachar("`"), "_", st, t)
        call modify_syntax_entry(13, 13, " ", st, t) ! return is whitespace
        call modify_syntax_entry(iachar("+"), iachar("+"), ".", st, t) ! punctuation
        call modify_syntax_entry(iachar("-"), iachar("-"), ".", st, t)
        call modify_syntax_entry(iachar("="), iachar("="), ".", st, t)
        call modify_syntax_entry(iachar("*"), iachar("*"), ".", st, t)
        call modify_syntax_entry(iachar("/"), iachar("/"), ".", st, t)
        call modify_syntax_entry(iachar("%"), iachar("%"), ".", st, t) ! bug#8820
        call modify_syntax_entry(iachar("\"), iachar("\"), ".", st, t)
    end subroutine init_syntax_table

    !> A highlight of subexpression N in FACE.  Not a port.
    function h(n, face, override, lax) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        integer, intent(in), optional :: override
        logical, intent(in), optional :: lax
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, override=override, laxmatch=lax)
    end function h

    ! Set the font-lock keywords of F90 mode.  Port of the values of
    ! f90.el's f90-font-lock-keywords-* variables.
    subroutine init_keywords()
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3), h4(4), a(1), anchor(1)
        type(font_lock_keyword_type) :: k2(23)
        character(len=*), parameter :: KW = "font_lock_keyword_face", FN = "font_lock_function_name_face", &
                                       TY = "font_lock_type_face", VA = "font_lock_variable_name_face", &
                                       CO = "font_lock_constant_face", PP = "font_lock_preprocessor_face"
        character(len=*), parameter :: B = "[ "//TAB//"]"
        integer :: n

        allocate (f90_font_lock_keywords_1(8))
        ! Special highlighting of "module procedure".
        h2(1) = h(1, KW)
        h2(2) = h(3, FN, lax=.true.)
        f90_font_lock_keywords_1(1) = font_lock_keyword("\_<\(module"//B//"*procedure\)\_>\([^()"//LF//"]*::\)?"// &
                                                        B//"*\("//NOT_CONT//"*\)", highlights=h2)
        ! Highlight definition of derived type.
        h2(1) = h(1, KW)
        h2(2) = h(2, FN)
        f90_font_lock_keywords_1(2) = font_lock_keyword(matcher=f90_typedef_matcher, highlights=h2)
        ! F2003.  Prevent operators being highlighted as functions.
        h1(1) = h(1, KW, FONT_LOCK_OVERRIDE_YES)
        f90_font_lock_keywords_1(3) = font_lock_keyword("\_<\(\(?:end"//B//"*\)?interface"//B// &
                                                        "*\(?:assignment\|operator\|read\|write\)\)"//B//"*(", &
                                                        highlights=h1)
        ! Other functions and declarations.  Named interfaces = F2003.
        ! F2008: end submodule submodule_name.
        ! F2008: module function|subroutine NAME.
        h2(1) = h(1, KW)
        h2(2) = h(3, FN, lax=.true.)
        f90_font_lock_keywords_1(4) = font_lock_keyword("\_<\(\(?:end"//B//"*\)?\(program\|\(?:module"//B// &
                                                        "*\)?\(?:function\|subroutine\)\|\(?:sub\)?module\|" &
                                                        //"associate\|interface\)\|use\|call\)\_>"//B//"*\("// &
                                                        SYMRE//"+\)?", highlights=h2)
        ! F2008: submodule (parent_name) submodule_name.
        h2(1) = h(1, KW)
        h2(2) = h(2, FN, lax=.true.)
        f90_font_lock_keywords_1(5) = font_lock_keyword("\_<\(submodule\)\_>"//B//"*([^)"//LF//"]+)"//B//"*\("// &
                                                        SYMRE//"+\)?", highlights=h2)
        ! F2003.
        h3(1) = h(1, KW)
        h3(2) = h(2, KW)
        h3(3) = h(3, FN)
        f90_font_lock_keywords_1(6) = font_lock_keyword("\_<\(use\)"//B//"*,"//B//"*\(\(?:non_\)?intrinsic\)"//B// &
                                                        "*::"//B//"*\("//SYMRE//"+\)", highlights=h3)
        f90_font_lock_keywords_1(7) = font_lock_keyword("\_<\(\(end"//B//"*\)?block"//B//"*data\|contains\)\_>")
        ! "abstract interface" is F2003.
        h1(1) = h(0, KW, FONT_LOCK_OVERRIDE_YES)
        f90_font_lock_keywords_1(8) = font_lock_keyword("\_<abstract"//B//"*interface\_>", highlights=h1)
        h1(1) = h(1, KW)
        k2(1) = font_lock_keyword("\(&\)"//B//"*\(!\|$\)", highlights=h1)
        ! Variable declarations (avoid the real function call).
        ! NB by accident (?), this correctly fontifies the "integer" in:
        ! integer () function foo ()
        ! because "() function foo ()" matches \3.
        ! The "pure" part does not really belong here, but was added to
        ! exploit that hack.
        ! The "function foo" bit is correctly fontified by keywords-1.
        ! TODO ? actually check for balanced parens in that case.
        h2(1) = h(1, TY, FONT_LOCK_OVERRIDE_YES)
        h2(2) = h(4, VA, FONT_LOCK_OVERRIDE_APPEND)
        k2(2) = font_lock_keyword("^[ "//TAB//"0-9]*\(?:pure\|elemental\)?"//B//"*\(real\|integer\|c\(haracter\|" &
                                  //"omplex\)\|enumerator\|generic\|procedure\|logical\|double"//B//"*precision\)" &
                                  //"\(.*::\|"//B//"*(.*)\)?\("//NOT_CONT//"*\(?:&"//LF//NOT_CONT//"*\)*\)", &
                                  highlights=h2)
        ! Derived type/class variables.
        h2(1) = h(1, TY)
        h2(2) = h(2, VA)
        k2(3) = font_lock_keyword(matcher=f90_typedec_matcher, highlights=h2)
        ! "real function foo (args)". Must override previous.  Note hack
        ! to get "args" unhighlighted again. Might not always be right,
        ! but probably better than leaving them as variables.
        h4(1) = h(1, TY, FONT_LOCK_OVERRIDE_YES)
        h4(2) = h(4, KW, FONT_LOCK_OVERRIDE_YES)
        h4(3) = h(5, FN, FONT_LOCK_OVERRIDE_YES)
        h4(4) = h(6, "default", FONT_LOCK_OVERRIDE_YES)
        k2(4) = font_lock_keyword("\_<\(\(real\|integer\|c\(haracter\|omplex\)\|logical\|double"//B//"*precision\|" &
                                  //"\(?:type\|class\)"//B//"*("//B//"*"//SYMRE//"+"//B//"*)\)"//B//"*\)\(function\)" &
                                  //"\_>"//B//"*\("//SYMRE//"+\)"//B//"*\(("//NOT_CONT//"*)\)", highlights=h4)
        ! enum (F2003; must be followed by ", bind(C)").
        h1(1) = h(1, KW)
        k2(5) = font_lock_keyword("\_<\(enum\)"//B//"*,", highlights=h1)
        ! end do, enum (F2003), if, select, where, and forall constructs.
        ! block, critical (F2008).
        h2(1) = h(1, KW)
        h2(2) = h(3, CO, lax=.true.)
        k2(6) = font_lock_keyword("\_<\(end"//B//"*\(do\|if\|enum\|select\|forall\|where\|block\|critical\)\)\_>" &
                                  //"\("//B//"+\("//SYMRE//"+\)\)?", highlights=h2)
        h2(1) = h(2, CO, lax=.true.)
        h2(2) = h(3, KW)
        k2(7) = font_lock_keyword("^[ "//TAB//"0-9]*\(\("//SYMRE//"+\)"//B//"*:"//B//"*\)?\(\(if\|do\("//B// &
                                  "*while\)?\|select"//B//"*\(?:case\|type\)\|where\|forall\|block\|critical\)\)\_>", &
                                  highlights=h2)
        ! Implicit declaration.
        h2(1) = h(1, KW)
        h2(2) = h(2, TY)
        k2(8) = font_lock_keyword("\_<\(implicit\)"//B//"+\(real\|integer\|c\(haracter\|omplex\)\|enumerator\|" &
                                  //"procedure\|logical\|double"//B//"*precision\|type"//B//"*("//SYMRE//"+)\|none\)" &
                                  //B//"*", highlights=h2)
        h2(1) = h(1, KW)
        h2(2) = h(2, CO, lax=.true.)
        k2(9) = font_lock_keyword("\_<\(namelist\|common\)"//B//"*/\("//SYMRE//"+\)?/", highlights=h2)
        k2(10) = font_lock_keyword("\_<else\("//B//"*if\|where\)?\_>")
        k2(11) = font_lock_keyword("\_<\(then\|continue\|format\|include\|\(?:error"//B//"+\)?stop\|return\)\_>")
        h2(1) = h(1, KW)
        h2(2) = h(2, CO, lax=.true.)
        k2(12) = font_lock_keyword("\_<\(exit\|cycle\)"//B//"+\("//SYMRE//"+\)?\_>", highlights=h2)
        h1(1) = h(1, KW)
        k2(13) = font_lock_keyword("\_<\(exit\|cycle\)\_>", highlights=h1)
        k2(14) = font_lock_keyword("\_<\(case\)"//B//"*\(default\|(\)", subexp=1)
        ! F2003 "class default".
        k2(15) = font_lock_keyword("\_<\(class\)"//B//"*default", subexp=1)
        ! F2003 "type is" in a "select type" block.
        h1(1) = h(1, KW, FONT_LOCK_OVERRIDE_YES)
        k2(16) = font_lock_keyword("\_<\(\(type\|class\)"//B//"*is\)"//B//"*(", highlights=h1)
        h2(1) = h(1, KW)
        h2(2) = h(2, CO)
        k2(17) = font_lock_keyword("\_<\(do\|go"//B//"*to\)\_>"//B//"*\([0-9]+\)", highlights=h2)
        ! Line numbers (lines whose first character after number is letter).
        h1(1) = h(1, CO, FONT_LOCK_OVERRIDE_YES)
        k2(18) = font_lock_keyword("^"//B//"*\([0-9]+\)"//B//"*[a-z]+", highlights=h1)
        ! Override eg for "#include".
        a(1) = h(0, PP)
        h2(1) = h(0, PP, FONT_LOCK_OVERRIDE_YES)
        h2(2) = font_lock_anchored(a, regexp="\_<defined\_>")
        k2(19) = font_lock_keyword("^#"//B//"*"//SYMRE//"+", highlights=h2)
        a(1) = h(0, CO, FONT_LOCK_OVERRIDE_YES)
        anchor(1) = font_lock_anchored(a, regexp="\(&&\|||\)")
        k2(20) = font_lock_keyword("^#", highlights=anchor)
        h1(1) = h(1, FN)
        k2(21) = font_lock_keyword("^#"//B//"*define"//B//"+\("//SYMRE//"+\)(", highlights=h1)
        h1(1) = h(1, VA)
        k2(22) = font_lock_keyword("^#"//B//"*define"//B//"+\("//SYMRE//"+\)", highlights=h1)
        h1(1) = h(1, "font_lock_string_face")
        k2(23) = font_lock_keyword("^#"//B//"*include"//B//"+\(<.+>\)", highlights=h1)
        n = size(f90_font_lock_keywords_1)
        allocate (f90_font_lock_keywords_2(n + 23))
        f90_font_lock_keywords_2(1:n) = f90_font_lock_keywords_1
        f90_font_lock_keywords_2(n + 1:) = k2
        n = size(f90_font_lock_keywords_2)
        allocate (f90_font_lock_keywords_3(n + 5))
        f90_font_lock_keywords_3(1:n) = f90_font_lock_keywords_2
        f90_font_lock_keywords_3(n + 1) = font_lock_keyword(f90_keywords_level_3_re)
        f90_font_lock_keywords_3(n + 2) = font_lock_keyword(f90_operators_re)
        ! FIXME why isn't this font-lock-builtin-face, which
        ! otherwise we hardly use, as in fortran.el?
        h1(1) = h(1, KW, FONT_LOCK_OVERRIDE_KEEP)
        f90_font_lock_keywords_3(n + 3) = font_lock_keyword(f90_procedures_re, highlights=h1)
        f90_font_lock_keywords_3(n + 4) = font_lock_keyword("\_<real\_>") ! avoid overwriting real defs
        ! As an attribute, but not as an optional argument.
        f90_font_lock_keywords_3(n + 5) = font_lock_keyword("\_<\(asynchronous\)"//B//"*[^=]", subexp=1)
        n = size(f90_font_lock_keywords_3)
        allocate (f90_font_lock_keywords_4(n + 2))
        f90_font_lock_keywords_4(1:n) = f90_font_lock_keywords_3
        f90_font_lock_keywords_4(n + 1) = font_lock_keyword(f90_constants_re, face=CO)
        f90_font_lock_keywords_4(n + 2) = font_lock_keyword(f90_hpf_keywords_re)
        f90_font_lock_keywords = f90_font_lock_keywords_2
    end subroutine init_keywords

    !> Abbrevs have generally two letters, except standard types `c, `i, `r, `t.
    !> Port of the value of f90.el's f90-mode-abbrev-table.
    subroutine init_abbrevs()
        call f90_mode_abbrev_table%define_table("f90_mode_abbrev_table")
        ! Accept ` as the first char of an abbrev.  Also allow _ in abbrevs.
        f90_mode_abbrev_table%regexp = "\(?:[^[:word:]_`]\|^\)\(`?[[:word:]_]+\)[^[:word:]_]*"
        call a("`al", "allocate")
        call a("`ab", "allocatable")
        call a("`ai", "abstract interface")
        call a("`as", "assignment")
        call a("`asy", "asynchronous")
        call a("`ba", "backspace")
        call a("`bd", "block data")
        call a("`bl", "block")
        call a("`c", "character")
        call a("`cl", "close")
        call a("`cm", "common")
        call a("`cx", "complex")
        call a("`cn", "contains")
        call a("`cr", "critical")
        call a("`cy", "cycle")
        call a("`de", "deallocate")
        call a("`df", "define")
        call a("`di", "dimension")
        call a("`dp", "double precision")
        call a("`dw", "do while")
        call a("`el", "else")
        call a("`eli", "else if")
        call a("`elw", "elsewhere")
        call a("`em", "elemental")
        call a("`e", "enumerator")
        call a("`eq", "equivalence")
        call a("`ex", "external")
        call a("`ey", "entry")
        call a("`fl", "forall")
        call a("`fo", "format")
        call a("`fu", "function")
        call a("`fa", ".false.")
        call a("`im", "implicit none")
        call a("`in", "include")
        call a("`i", "integer")
        call a("`it", "intent")
        call a("`if", "interface")
        call a("`lo", "logical")
        call a("`mo", "module")
        call a("`na", "namelist")
        call a("`nu", "nullify")
        call a("`op", "optional")
        call a("`pa", "parameter")
        call a("`po", "pointer")
        call a("`pr", "print")
        call a("`pi", "private")
        call a("`pm", "program")
        call a("`pr", "protected")
        call a("`pu", "public")
        call a("`r", "real")
        call a("`rc", "recursive")
        call a("`rt", "return")
        call a("`rw", "rewind")
        call a("`se", "select")
        call a("`sq", "sequence")
        call a("`su", "subroutine")
        call a("`ta", "target")
        call a("`tr", ".true.")
        call a("`t", "type")
        call a("`vo", "volatile")
        call a("`wh", "where")
        call a("`wr", "write")

    contains

        subroutine a(abbrev, expansion)
            character(len=*), intent(in) :: abbrev, expansion

            call f90_mode_abbrev_table%define(abbrev, expansion, system=.true.)
        end subroutine a
    end subroutine init_abbrevs

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    !> (looking-at RE) Not a port.
    logical function looking(re)
        character(len=*), intent(in) :: re
        type(status_type) :: st

        looking = looking_at(re, st)
        if (.not. st%ok()) looking = .false.
    end function looking

    !> Whether group N of the last match matched.  Not a port.
    logical function matched(n)
        integer, intent(in) :: n
        type(status_type) :: st

        matched = match_beginning(n, st) > 0
    end function matched

    !> The text of group N of the last (buffer) match, "" if it did not
    !> match.  Not a port.
    function ms(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        integer(int64) :: b, e
        type(status_type) :: st

        s = ""
        b = match_beginning(n, st)
        if (b <= 0) return
        e = match_end(n, st)
        call buffer_substring(b, e, s, st)
        if (.not. allocated(s)) s = ""
    end function ms

    !> The end of group N of the last match.  Not a port.
    integer(int64) function me(n)
        integer, intent(in) :: n
        type(status_type) :: st

        me = match_end(n, st)
    end function me

    !> The start of group N of the last match.  Not a port.
    integer(int64) function mb(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mb = match_beginning(n, st)
    end function mb

    !> (list (match-string KIND) (match-string NAME)).  Not a port.
    function blk(kind, name) result(b)
        integer, intent(in) :: kind
        integer, intent(in), optional :: name
        type(f90_block_type) :: b

        b%found = .true.
        if (matched(kind)) b%kind = ms(kind)

        if (present(name)) then
            if (matched(name)) b%name = ms(name)
        end if
    end function blk

    !> (list KIND NAME) with literal strings.  Not a port.
    function blk_of(kind, name) result(b)
        character(len=*), intent(in) :: kind
        character(len=*), intent(in), optional :: name
        type(f90_block_type) :: b

        b%found = .true.
        b%kind = kind
        if (present(name)) b%name = name
    end function blk_of

    !> Lower-case S (ASCII).  Not a port.
    pure function downcase(s) result(d)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: d
        integer :: i

        d = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") d(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function downcase

    !> (skip-chars-forward CHARS) Not a port.
    subroutine skip_forward(chars)
        character(len=*), intent(in) :: chars
        integer(int64) :: moved
        type(status_type) :: st

        moved = skip_chars_forward(chars, st)
    end subroutine skip_forward

    !> (skip-chars-backward CHARS) Not a port.
    subroutine skip_backward(chars)
        character(len=*), intent(in) :: chars
        integer(int64) :: moved
        type(status_type) :: st

        moved = skip_chars_backward(chars, st)
    end subroutine skip_backward

    !> The character before point, -1 if none (preceding-char gives 0).
    !> Not a port.
    integer function preceding()
        integer(int32) :: c
        logical :: found

        call char_before(c, found)
        preceding = 0
        if (found) preceding = c
    end function preceding

    !> The character after point, 0 if none (following-char).  Not a port.
    integer function following()
        integer(int32) :: c
        logical :: found

        call char_after(c, found)
        following = 0
        if (found) following = c
    end function following

    !> A marker at POS.  Not a port.
    function marker_at(pos) result(m)
        integer(int64), intent(in) :: pos
        type(handle_type) :: m
        type(status_type) :: st

        call copy_marker_at(pos, m, .false., st)
    end function marker_at

    !> The position of marker M.  Not a port.
    integer(int64) function mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, mpos, st)
    end function mpos

    !> Release marker M.  Not a port.
    subroutine unmark(m)
        type(handle_type), intent(inout) :: m
        type(status_type) :: st

        call free_marker(m, st)
    end subroutine unmark

    !> (re-search-forward RE BOUND t): true when found.  Not a port.
    logical function search_re(re, bound, noerror)
        character(len=*), intent(in) :: re
        integer(int64), intent(in), optional :: bound
        integer, intent(in) :: noerror
        integer(int64) :: found
        type(status_type) :: st

        call re_search_forward(re, found, st, bound=bound, noerror=noerror)
        search_re = st%ok() .and. found > 0
    end function search_re

    !> `re_search_backward' of RE, moving to the limit on failure: .true. when found.  Not a port.
    logical function search_re_backward_move(re)
        character(len=*), intent(in) :: re
        integer(int64) :: found
        type(status_type) :: st

        call re_search_backward(re, found, st, noerror=SEARCH_MOVE)
        search_re_backward_move = st%ok() .and. found > 0
    end function search_re_backward_move

    !> Bind `case_fold_search' to true; SAVED is for `restore_case'.
    !> Not a port.
    subroutine fold_case(saved)
        logical, intent(out) :: saved

        saved = case_fold_search%value
        case_fold_search%value = .true.
    end subroutine fold_case

    ! Restore `case_fold_search' from SAVED.  Not a port.
    subroutine restore_case(saved)
        logical, intent(in) :: saved

        case_fold_search%value = saved
    end subroutine restore_case

    ! ------------------------------------------------------------------
    ! Inline functions
    ! ------------------------------------------------------------------

    !> The start of the parse for f90-in-string and f90-in-comment.
    !> Not a port.
    integer(int64) function parse_start()
        parse_start = point_min()

        if (f90_cache_position > 0) then
            if (point() > f90_cache_position) parse_start = f90_cache_position
        end if
    end function parse_start

    !> Return true if point is inside a string.
    !> Checks from `point_min', or `f90_cache_position', if that is set
    !> and lies before point.  Port of f90.el:f90-in-string.
    logical function f90_in_string()
        type(parse_state_type) :: state
        integer(int64) :: pt
        type(status_type) :: st

        pt = point()
        call parse_partial_expression(parse_start(), pt, state, st)
        call goto_char(pt)
        f90_in_string = state%instring /= -1
    end function f90_in_string

    !> Return true if point is inside a comment.
    !> Checks from `point_min', or `f90_cache_position', if that is set
    !> and lies before point.  Port of f90.el:f90-in-comment.
    logical function f90_in_comment()
        type(parse_state_type) :: state
        integer(int64) :: pt
        type(status_type) :: st

        pt = point()
        call parse_partial_expression(parse_start(), pt, state, st)
        call goto_char(pt)
        f90_in_comment = state%incomment /= 0
    end function f90_in_comment

    !> Return true if the current line is a continued one.
    !> This includes comment or preprocessor lines embedded in continued lines,
    !> but not the last line of a continued statement.  Port of
    !> f90.el:f90-line-continued.
    logical function f90_line_continued() result(continued)
        integer(int64) :: opoint, found
        type(status_type) :: st

        opoint = point()
        call beginning_of_line(1_int64)

        do while (looking("[ "//TAB//"]*\([!#]\|$\)"))
            if (forward_line(-1_int64) /= 0) exit
        end do

        call end_of_line(1_int64)

        do while (f90_in_comment())
            call search_backward_bol("!", found)
            if (found <= 0) exit
            call skip_backward("!")
        end do

        call skip_backward(" "//TAB)
        continued = preceding() == iachar("&")
        call goto_char(opoint)
        st = status_type()
    end function f90_line_continued

    !> (search-backward S (line-beginning-position)) Not a port.
    subroutine search_backward_bol(s, found)
        use search_fm, only: search_backward
        character(len=*), intent(in) :: s
        integer(int64), intent(out) :: found
        type(status_type) :: st

        call search_backward(s, found, st, bound=line_beginning_position())
        if (.not. st%ok()) found = 0
    end subroutine search_backward_bol

    !> Return indentation of current line.
    !> Line-numbers are considered whitespace characters.  Port of
    !> f90.el:f90-current-indentation (the number of characters skipped).
    integer(int64) function f90_current_indentation() result(n)
        integer(int64) :: opoint
        type(status_type) :: st

        opoint = point()
        call beginning_of_line(1_int64)
        n = skip_chars_forward(" "//TAB//"0-9", st)
        call goto_char(opoint)
    end function f90_current_indentation

    !> Indent current line to column COL.
    !> If optional argument NO_LINE_NUMBER is false, jump over a possible
    !> line-number before indenting.  Port of f90.el:f90-indent-to.
    subroutine f90_indent_to(col, status, no_line_number)
        integer(int64), intent(in) :: col
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_line_number
        logical :: nln
        integer(int64) :: c

        nln = .false.
        if (present(no_line_number)) nln = no_line_number
        call beginning_of_line(1_int64)
        if (.not. nln) call skip_forward(" "//TAB//"0-9")
        call delete_horizontal_space(.false., status)
        if (.not. status%ok()) return

        ! Leave >= 1 space after line number.
        if (current_column() == 0) then
            c = indent_to(col, status, 0_int64)
        else
            c = indent_to(col, status, 1_int64)
        end if
    end subroutine f90_indent_to

    !> If point lies within a comment, return the string starting the comment.
    !> For example, "!" or "!!", followed by the appropriate amount of
    !> whitespace, if any.  FOUND .false. means none.  Port of
    !> f90.el:f90-get-present-comment-type.
    subroutine f90_get_present_comment_type(s, found, status)
        character(len=:), allocatable, intent(out) :: s
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, f

        status = status_type()
        s = ""
        found = .false.
        opoint = point()

        ! Include the whitespace for consistent auto-filling of comment blocks.
        if (f90_in_comment()) then
            call beginning_of_line(1_int64)
            call re_search_forward("!+[ "//TAB//"]*", f, status, bound=line_end_position())

            do while (status%ok())
                if (.not. f90_in_string()) exit
                call re_search_forward("!+[ "//TAB//"]*", f, status, bound=line_end_position())
            end do

            if (status%ok()) then
                s = ms(0)
                found = .true.
            end if
        end if

        call goto_char(opoint)
    end subroutine f90_get_present_comment_type

    !> Compare strings A and B neglecting case and allowing for no value
    !> (unallocated).  Port of f90.el:f90-equal-symbols.
    logical function f90_equal_names(a, b) result(equal)
        character(len=:), allocatable, intent(in) :: a, b

        if (.not. allocated(a) .or. .not. allocated(b)) then
            equal = .not. allocated(a) .and. .not. allocated(b)
        else
            equal = downcase(a) == downcase(b) .and. len(a) == len(b)
        end if
    end function f90_equal_names

    !> Return ("do" NAME) if a do statement starts after point.
    !> NAME is unallocated if the statement has no label.  Port of
    !> f90.el:f90-looking-at-do.
    function f90_looking_at_do() result(b)
        type(f90_block_type) :: b

        if (looking("\(\("//SYMRE//"+\)[ "//TAB//"]*:\)?[ "//TAB//"]*\(do\)\_>")) b = blk(3, 2)
    end function f90_looking_at_do

    !> Return ("select" NAME) if a select statement starts after point.
    !> NAME is unallocated if the statement has no label.  Port of
    !> f90.el:f90-looking-at-select-case.
    function f90_looking_at_select_case() result(b)
        type(f90_block_type) :: b

        if (looking("\(\("//SYMRE//"+\)[ "//TAB//"]*:\)?[ "//TAB//"]*\(select\)[ "//TAB//"]*\(case\|type\)[ "//TAB// &
                    "]*(")) b = blk(3, 2)
    end function f90_looking_at_select_case

    !> Return ("if" NAME) if an if () then statement starts after point.
    !> NAME is unallocated if the statement has no label.  Port of
    !> f90.el:f90-looking-at-if-then.
    recursive subroutine f90_looking_at_if_then(b, status)
        type(f90_block_type), intent(out) :: b
        type(status_type), intent(out) :: status
        type(f90_block_type) :: s
        integer(int64) :: opoint, pos
        logical :: ok, moved

        status = status_type()
        opoint = point()

        if (looking("\(\("//SYMRE//"+\)[ "//TAB//"]*:\)?[ "//TAB//"]*\(if\)\_>")) then
            s = blk(3, 2)
            pos = scan_groups(point(), 1_int64, 0_int64, status)

            if (.not. status%ok()) then
                call goto_char(opoint)
                return
            end if

            if (pos > 0) call goto_char(pos)
            call skip_forward(" "//TAB)
            ok = looking("then\_>")

            if (.not. ok) then
                if (f90_line_continued()) then
                    moved = f90_next_statement()
                    call skip_forward(" "//TAB//"0-9&")
                    ok = looking("then\_>")
                end if
            end if

            if (ok) b = s
        end if

        call goto_char(opoint)
    end subroutine f90_looking_at_if_then

    !> Return ("associate") if an associate block starts after point.
    !> Port of f90.el:f90-looking-at-associate.
    function f90_looking_at_associate() result(b)
        type(f90_block_type) :: b

        if (looking("\_<\(associate\)[ "//TAB//"]*(")) b = blk(1)
    end function f90_looking_at_associate

    !> Return (KIND NAME) if a critical or block block starts after point.
    !> Port of f90.el:f90-looking-at-critical.
    function f90_looking_at_critical() result(b)
        type(f90_block_type) :: b
        type(f90_block_type) :: s
        integer(int64) :: opoint
        logical :: ok

        if (looking("\(\("//SYMRE//"+\)[ "//TAB//"]*:\)?[ "//TAB//"]*\(critical\|block\)\_>")) then
            s = blk(3, 2)
            ok = s%kind /= "block"

            if (.not. ok) then
                opoint = point()
                call skip_forward(" "//TAB)
                ok = .not. looking("data\_>")
                call goto_char(opoint)
            end if

            if (ok) b = s
        end if
    end function f90_looking_at_critical

    !> Return true if a critical or block block ends after point.  Port of
    !> f90.el:f90-looking-at-end-critical.
    logical function f90_looking_at_end_critical() result(ends)
        integer(int64) :: opoint

        ends = .false.

        if (looking("end[ "//TAB//"]*\(critical\|block\)\_>")) then
            ends = ms(1) /= "block"

            if (.not. ends) then
                opoint = point()
                call skip_forward(" "//TAB)
                ends = .not. looking("data\_>")
                call goto_char(opoint)
            end if
        end if
    end function f90_looking_at_end_critical

    !> Return (KIND NAME) if a where or forall block starts after point.
    !> NAME is unallocated if the statement has no label.  Port of
    !> f90.el:f90-looking-at-where-or-forall.
    recursive subroutine f90_looking_at_where_or_forall(b, status)
        type(f90_block_type), intent(out) :: b
        type(status_type), intent(out) :: status
        type(f90_block_type) :: s
        integer(int64) :: opoint, pos

        status = status_type()
        opoint = point()

        if (looking("\(\("//SYMRE//"+\)[ "//TAB//"]*:\)?[ "//TAB//"]*\(where\|forall\)\_>")) then
            s = blk(3, 2)
            pos = scan_groups(point(), 1_int64, 0_int64, status)

            if (.not. status%ok()) then
                call goto_char(opoint)
                return
            end if

            if (pos > 0) call goto_char(pos)
            call skip_forward(" "//TAB)
            if (looking("\(!\|$\)")) b = s
        end if

        call goto_char(opoint)
    end subroutine f90_looking_at_where_or_forall

    !> Return KIND and NAME if a type/enum/interface/block-data starts after point.
    !> NAME is set only for type and certain interfaces.  Port of
    !> f90.el:f90-looking-at-type-like.
    function f90_looking_at_type_like() result(b)
        type(f90_block_type) :: b
        integer(int64) :: opoint
        logical :: ok
        character(len=:), allocatable :: kind

        opoint = point()
        ok = .false.

        if (looking("\_<type\_>[ "//TAB//"]*")) then
            call goto_char(me(0))

            if (.not. looking("\(is\_>\|(\)")) then
                ok = looking("\("//SYMRE//"+\)")
                if (.not. ok) ok = search_re("[ "//TAB//"]*::[ "//TAB//"]*\("//SYMRE//"+\)", line_end_position(), &
                                             SEARCH_NOERROR)
            end if
        end if

        call goto_char(opoint)

        if (ok) then
            b = blk_of("type")
            if (matched(1)) b%name = ms(1)
        else if (looking("\_<\(interface\)\_>[ "//TAB//"]*")) then
            kind = ms(1)
            b = blk_of(kind)
            call goto_char(me(0))

            if (looking("\(operator\|assignment\|read\|write\)[ "//TAB//"]*([^)"//LF//"]*)")) then
                b%name = ms(0)
            else if (looking(SYMRE//"+")) then
                b%name = ms(0)
            end if

            call goto_char(opoint)
        else if (looking("\(enum\|block[ "//TAB//"]*data\)\_>")) then
            b = blk(1)
        else if (looking("abstract[ "//TAB//"]*\(interface\)\_>")) then
            b = blk(1)
        end if
    end function f90_looking_at_type_like

    !> Return (KIND NAME) if a program block with name NAME starts after point.
    !> Port of f90.el:f90-looking-at-program-block-start.
    function f90_looking_at_program_block_start() result(b)
        type(f90_block_type) :: b

        if (looking("\(program\)[ "//TAB//"]+\("//SYMRE//"+\)\_>")) then
            b = blk(1, 2)
        else if (.not. looking("module[ "//TAB//"]*\(procedure\|function\|subroutine\)\_>") .and. &
                 looking("\(module\)[ "//TAB//"]+\("//SYMRE//"+\)\_>")) then
            b = blk(1, 2)
        else if (looking("\(submodule\)[ "//TAB//"]*([^)"//LF//"]+)[ "//TAB//"]*\("//SYMRE//"+\)\_>")) then
            b = blk(1, 2)
        else if (.not. looking("end[ "//TAB//"]*\(function\|procedure\|subroutine\)") .and. &
                 looking("[^!'""&"//LF//"]*\(?:module[ "//TAB//"]*\)?\(function\|subroutine\)[ "//TAB//"]+\("// &
                         SYMRE//"+\)")) then
            ! TODO: In F2008  "module procedure foo" may or may not start a block,
            ! It is impossible to tell the difference without parsing state.
            b = blk(1, 2)
        end if
    end function f90_looking_at_program_block_start

    !> Return (KIND NAME) if a block with name NAME ends after point.
    !> Port of f90.el:f90-looking-at-program-block-end.
    function f90_looking_at_program_block_end() result(b)
        type(f90_block_type) :: b

        if (looking("end[ "//TAB//"]*\(interface\)[ "//TAB//"]*\(\(?:assignment\|operator\|read\|write\)[ "//TAB// &
                    "]*([^)"//LF//"]*)\)")) then
            b = blk(1, 2)
        else if (looking("end[ "//TAB//"]*"//f90_blocks_re//"?\([ "//TAB//"]+\("//SYMRE//"+\)\)?\_>")) then
            b = blk(1, 3)
        end if
    end function f90_looking_at_program_block_end

    !> Whether `f90_looking_at_associate' finds a block.  Not a port.
    logical function found_associate()
        type(f90_block_type) :: b

        b = f90_looking_at_associate()
        found_associate = b%found
    end function found_associate

    !> Whether `f90_looking_at_do' finds a block.  Not a port.
    logical function found_do()
        type(f90_block_type) :: b

        b = f90_looking_at_do()
        found_do = b%found
    end function found_do

    !> Whether `f90_looking_at_type_like' finds a block.  Not a port.
    logical function found_type_like()
        type(f90_block_type) :: b

        b = f90_looking_at_type_like()
        found_type_like = b%found
    end function found_type_like

    !> Whether `f90_looking_at_program_block_start' finds a block.
    !> Not a port.
    logical function found_program_block_start()
        type(f90_block_type) :: b

        b = f90_looking_at_program_block_start()
        found_program_block_start = b%found
    end function found_program_block_start

    !> Whether `f90_looking_at_program_block_end' finds a block.
    !> Not a port.
    logical function found_program_block_end()
        type(f90_block_type) :: b

        b = f90_looking_at_program_block_end()
        found_program_block_end = b%found
    end function found_program_block_end

    !> Return the indentation to be used for a comment starting at point.
    !> Used for `comment_indent_function' by F90 mode.
    !> "!!!", `f90_directive_comment_re', variable `f90_comment_region' return 0.
    !> `f90_indented_comment_re' (if not trailing code) calls `f90_calculate_indent'.
    !> All others return `comment_column', leaving at least one space after code.
    !> Port of f90.el:f90-comment-indent.
    recursive subroutine f90_comment_indent(col, status)
        integer(int64), intent(out) :: col
        type(status_type), intent(out) :: status
        integer(int64) :: opoint
        logical :: at_bol, directive

        status = status_type()
        col = 0
        if (looking("!!!")) return
        directive = .false.
        if (allocated(f90_directive_comment_re%value)) directive = looking(f90_directive_comment_re%value)
        if (directive) return
        if (looking(regexp_quote(f90_comment_region%value))) return
        opoint = point()
        call skip_backward(" "//TAB)
        at_bol = is_bol()
        call goto_char(opoint)

        if (looking(f90_indented_comment_re%value) .and. at_bol) then
            ! Don't attempt to indent trailing comment as code.
            call f90_calculate_indent(col, status)
            return
        end if

        call skip_backward(" "//TAB)

        if (is_bol()) then
            col = max(0_int64, comment_column%value)
        else
            col = max(1 + current_column(), comment_column%value)
        end if

        call goto_char(opoint)
    end subroutine f90_comment_indent

    !> `f90_comment_indent' as a `comment_indent_function'.  Not a port.
    recursive subroutine f90_comment_indent_function(indent, status)
        type(comment_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: col

        call f90_comment_indent(col, status)
        indent%kind = COMMENT_INDENT_COLUMN
        indent%column = col
    end subroutine f90_comment_indent_function

    !> Return continuation properties of present statement.
    !> Possible return values are:
    !> CONT_SINGLE - statement is not continued.
    !> CONT_BEGIN  - current line is the first in a continued statement.
    !> CONT_END    - current line is the last in a continued statement
    !> CONT_MIDDLE - current line is neither first nor last in a continued statement.
    !> Comment lines embedded amongst continued lines return CONT_MIDDLE.
    !> Port of f90.el:f90-present-statement-cont.
    integer function f90_present_statement_cont() result(kind)
        logical :: pcont, cont
        integer(int64) :: opoint

        opoint = point()
        pcont = .false.
        if (f90_previous_statement()) pcont = f90_line_continued()
        call goto_char(opoint)
        cont = f90_line_continued()

        if (.not. pcont .and. .not. cont) then
            kind = CONT_SINGLE
        else if (.not. pcont .and. cont) then
            kind = CONT_BEGIN
        else if (pcont .and. .not. cont) then
            kind = CONT_END
        else
            kind = CONT_MIDDLE
        end if
    end function f90_present_statement_cont

    !> If `f90_leave_line_no' is false, left-justify a line number.
    !> Leaves point at the first non-blank character after the line number.
    !> Call from beginning of line.  Port of f90.el:f90-indent-line-no.
    subroutine f90_indent_line_no()
        type(status_type) :: st

        if (.not. f90_leave_line_no%value .and. looking("[ "//TAB//"]+[0-9]")) then
            call delete_horizontal_space(.false., st)
        end if

        call skip_forward(" "//TAB//"0-9")
    end subroutine f90_indent_line_no

    !> Return false if point is at the edge of a code block.
    !> Searches line forward for "function" or "subroutine",
    !> if all else fails.  Port of f90.el:f90-no-block-limit.
    logical function f90_no_block_limit() result(no_limit)
        integer(int64) :: opoint
        logical :: limit

        opoint = point()
        limit = looking("end")
        if (.not. limit) limit = looking("\(do\|if\|else\(if\|where\)?\|select[ "//TAB//"]*\(case\|type\)\|case\|" &
                                         //"where\|forall\|\(?:class\|type\)[ "//TAB//"]*is\|class[ "//TAB// &
                                         "]*default\|block\|critical\|enum\|associate\)\_>")
        if (.not. limit) limit = looking("\(program\|\(?:sub\)?module\|\(?:abstract[ "//TAB//"]*\)?interface\|" &
                                         //"block[ "//TAB//"]*data\)\_>")
        if (.not. limit) limit = looking("\(contains\|"//SYMRE//"+[ "//TAB//"]*:\)")
        if (.not. limit) limit = looking(f90_type_def_re)
        if (.not. limit) limit = search_re("\(function\|subroutine\)", line_end_position(), SEARCH_NOERROR)
        no_limit = .not. limit
        call goto_char(opoint)
    end function f90_no_block_limit

    !> Change case of current line as per `f90_auto_keyword_case'.  Port of
    !> f90.el:f90-update-line.
    subroutine f90_update_line(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (f90_auto_keyword_case%value /= F90_KEYWORD_CASE_NONE) then
            call f90_change_keywords(int(f90_auto_keyword_case%value), status, line_beginning_position(), &
                                     line_end_position())
        end if
    end subroutine f90_update_line

    ! ------------------------------------------------------------------
    ! Indentation
    ! ------------------------------------------------------------------

    !> Change keyword case and auto-fill line as operators are inserted.
    !> Port of f90.el:f90-electric-insert.
    recursive subroutine f90_electric_insert(status, arg)
        use keyboard_fm, only: last_command_event
        use lisp_fm, only: xfixnum, value_type
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n, c

        n = 1
        if (present(arg)) n = arg
        c = -1
        if (vtag(last_command_event) == V_INTEGER) c = xfixnum(last_command_event)
        call self_insert_command(n, c, status)
        if (.not. status%ok()) return

        if (associated(auto_fill_function%value)) then
            call f90_do_auto_fill(status) ! also updates line
        else
            call f90_update_line(status)
        end if
    end subroutine f90_electric_insert

    !> Change keyword case and auto-fill line as operators are inserted
    !> (the command).  Port of f90.el:f90-electric-insert.
    recursive subroutine cmd_f90_electric_insert(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_electric_insert(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_f90_electric_insert

    !> Whether an if-then, where/forall or select case starts after point
    !> (status as `f90_looking_at_if_then').  Not a port.
    recursive logical function if_like(status) result(yes)
        type(status_type), intent(out) :: status
        type(f90_block_type) :: b

        call f90_looking_at_if_then(b, status)
        yes = b%found
        if (yes .or. .not. status%ok()) return
        call f90_looking_at_where_or_forall(b, status)
        yes = b%found
        if (yes .or. .not. status%ok()) return
        b = f90_looking_at_select_case()
        yes = b%found
    end function if_like

    !> Get correct indent for a line starting with line number.
    !> Does not check type and subprogram indentation.  Port of
    !> f90.el:f90-get-correct-indent.
    recursive subroutine f90_get_correct_indent(icol, status)
        integer(int64), intent(out) :: icol
        type(status_type), intent(out) :: status
        integer(int64) :: epnt, opoint
        logical :: more
        type(f90_block_type) :: b

        status = status_type()
        epnt = line_end_position()
        opoint = point()

        do
            if (.not. f90_previous_statement()) exit
            more = any(f90_present_statement_cont() == [CONT_MIDDLE, CONT_END])
            if (.not. more) more = looking("[ "//TAB//"]*[0-9]")
            if (.not. more) exit
        end do

        icol = current_indentation()
        call beginning_of_line(1_int64)

        if (search_re("\(if\|do\|select\|where\|forall\)", line_end_position(), SEARCH_NOERROR)) then
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB)
            b = f90_looking_at_do()

            if (b%found) then
                icol = icol + f90_do_indent%value
            else if (if_like(status)) then
                icol = icol + f90_if_indent%value
            else if (found_associate()) then
                ! FIXME this makes no sense, because this section/function is
                ! only for if/do/select/where/forall ?
                icol = icol + f90_associate_indent%value
            end if

            if (.not. status%ok()) then
                call goto_char(opoint)
                return
            end if

            call end_of_line(1_int64)
        end if

        do while (search_re("\(if\|do\|select\|where\|forall\)", epnt, SEARCH_NOERROR))
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")
            b = f90_looking_at_do()

            if (b%found) then
                icol = icol + f90_do_indent%value
            else if (if_like(status)) then
                icol = icol + f90_if_indent%value
            else if (found_associate()) then
                ! FIXME this makes no sense, because this section/function is
                ! only for if/do/select/where/forall ?
                icol = icol + f90_associate_indent%value
            else if (looking(f90_end_if_re)) then
                icol = icol - f90_if_indent%value
            else if (looking(f90_end_associate_re)) then
                icol = icol - f90_associate_indent%value
            else if (f90_looking_at_end_critical()) then
                icol = icol - f90_critical_indent%value
            else if (looking("end[ "//TAB//"]*do\_>")) then
                icol = icol - f90_do_indent%value
            end if

            if (.not. status%ok()) exit
            call end_of_line(1_int64)
        end do

        call goto_char(opoint)
    end subroutine f90_get_correct_indent

    !> Calculate the indent column based on previous statements.  Port of
    !> f90.el:f90-calculate-indent.
    recursive subroutine f90_calculate_indent(icol, status)
        integer(int64), intent(out) :: icol
        type(status_type), intent(out) :: status
        integer(int64) :: pnt
        integer :: cont
        logical :: saved, start, moved, has_start
        type(f90_block_type) :: b

        status = status_type()
        call fold_case(saved)
        pnt = point()
        icol = 0

        if (.not. f90_previous_statement()) then
            ! If f90-previous-statement returns nil, we must have been
            ! called from on or before the first line of the first statement.
            call goto_char(pnt)
            call beginning_of_line(1_int64)
            ! Preprocessor line before code statement.
            start = looking("[ "//TAB//"]*#")
            call goto_char(point_min())
            call beginning_of_line(1_int64)

            if (.not. start) then
                ! f90-previous-statement has moved us over
                ! comment/blank lines, so we need to get
                ! back to the first code statement.
                if (looking("[ "//TAB//"]*\([!#]\|$\)")) moved = f90_next_statement()
                call skip_forward(" "//TAB//"0-9")
                has_start = found_program_block_start()
            else
                has_start = .true.
            end if

            if (has_start) then
                icol = 0
            else
                ! No explicit PROGRAM start statement.
                icol = f90_program_indent%value
            end if
        else
            cont = f90_present_statement_cont()

            if (cont == CONT_END) then
                do while (f90_present_statement_cont() /= CONT_BEGIN)
                    if (.not. f90_previous_statement()) exit
                end do
            end if

            if (cont == CONT_BEGIN) then
                icol = f90_current_indentation() + f90_continuation_indent%value
            else if (cont == CONT_MIDDLE) then
                icol = current_indentation()
            else
                icol = f90_current_indentation()
                call skip_forward(" "//TAB)

                if (looking("[0-9]")) then
                    call f90_get_correct_indent(icol, status)
                else
                    if (if_like(status) .or. looking(f90_else_like_re)) then
                        icol = icol + f90_if_indent%value
                    else if (.not. status%ok()) then
                        continue
                    else if (found_do()) then
                        icol = icol + f90_do_indent%value
                    else if (found_type_like()) then
                        icol = icol + f90_type_indent%value
                    else if (found_associate()) then
                        icol = icol + f90_associate_indent%value
                    else
                        b = f90_looking_at_critical()

                        if (b%found) then
                            icol = icol + f90_critical_indent%value
                        else if (found_program_block_start() .or. &
                                 looking("contains[ "//TAB//"]*\($\|!\)")) then
                            icol = icol + f90_program_indent%value
                        end if
                    end if
                end if

                call goto_char(pnt)
                call beginning_of_line(1_int64)

                if (status%ok()) then
                    if (looking("[ "//TAB//"]*$")) then
                        continue
                    else if (looking("[ "//TAB//"]*#")) then
                        ! check for cpp directive
                        icol = 0
                    else
                        call skip_forward(" "//TAB//"0-9")

                        if (looking(f90_else_like_re) .or. looking(f90_end_if_re)) then
                            icol = icol - f90_if_indent%value
                        else if (looking("end[ "//TAB//"]*do\_>")) then
                            icol = icol - f90_do_indent%value
                        else if (looking(f90_end_type_re)) then
                            icol = icol - f90_type_indent%value
                        else if (looking(f90_end_associate_re)) then
                            icol = icol - f90_associate_indent%value
                        else if (f90_looking_at_end_critical()) then
                            icol = icol - f90_critical_indent%value
                        else if (looking("contains[ "//TAB//"]*\(!\|$\)") .or. &
                                 found_program_block_end()) then
                            icol = icol - f90_program_indent%value
                        end if
                    end if
                end if
            end if
        end if

        call goto_char(pnt)
        call restore_case(saved)
    end subroutine f90_calculate_indent

    !> Calculate the indent column based on previous statements (the
    !> command).  Port of f90.el:f90-calculate-indent.
    recursive subroutine cmd_f90_calculate_indent(status)
        type(status_type), intent(out) :: status
        integer(int64) :: icol

        call f90_calculate_indent(icol, status)
    end subroutine cmd_f90_calculate_indent

    !> Move point to beginning of the previous F90 statement.
    !> If no previous statement is found (i.e. if called from the first
    !> statement in the buffer), move to the start of the buffer and
    !> return false.  A statement is a line which is neither blank nor a
    !> comment.  Port of f90.el:f90-previous-statement.
    logical function f90_previous_statement() result(not_first_statement)
        call beginning_of_line(1_int64)

        do
            not_first_statement = forward_line(-1_int64) == 0
            if (.not. not_first_statement) exit
            if (.not. looking("[ "//TAB//"0-9]*\(!\|$\|#\)")) exit
        end do
    end function f90_previous_statement

    !> Move point to beginning of the previous F90 statement (the command).
    !> Port of f90.el:f90-previous-statement.
    subroutine cmd_f90_previous_statement(status)
        type(status_type), intent(out) :: status
        logical :: moved

        status = status_type()
        moved = f90_previous_statement()
    end subroutine cmd_f90_previous_statement

    !> Move point to beginning of the next F90 statement.
    !> Return false if no later statement is found.  Port of
    !> f90.el:f90-next-statement.
    logical function f90_next_statement() result(not_last_statement)
        call beginning_of_line(1_int64)

        do
            not_last_statement = forward_line(1_int64) == 0
            if (not_last_statement) not_last_statement = .not. is_eob()
            if (.not. not_last_statement) exit
            if (.not. looking("[ "//TAB//"0-9]*\(!\|$\|#\)")) exit
        end do
    end function f90_next_statement

    !> Move point to beginning of the next F90 statement (the command).
    !> Port of f90.el:f90-next-statement.
    subroutine cmd_f90_next_statement(status)
        type(status_type), intent(out) :: status
        logical :: moved

        status = status_type()
        moved = f90_next_statement()
    end subroutine cmd_f90_next_statement

    !> Move point to the beginning of the current subprogram.
    !> Return TYPE and NAME, or nothing (FOUND .false.) if not found.  INTERACTIVE says it was
    !> called interactively.  Port of f90.el:f90-beginning-of-subprogram.
    function f90_beginning_of_subprogram(interactive) result(matching_beg)
        logical, intent(in), optional :: interactive
        type(f90_block_type) :: matching_beg
        type(f90_block_type) :: b
        integer :: count
        logical :: saved

        call fold_case(saved)
        count = 1
        call beginning_of_line(1_int64)
        ! Check whether we're already at the start of a subprogram.
        matching_beg = f90_looking_at_program_block_start()

        if (matching_beg%found) then
            call restore_case(saved)
            return
        end if

        ! We're not; search backwards.
        do while (count > 0)
            if (.not. search_re_backward_move(f90_program_block_re)) exit
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")

            ! Check if in string in case using non-standard feature where
            ! continued strings do not need "&" at start of continuations.
            if (f90_in_string()) then
                continue
            else
                b = f90_looking_at_program_block_start()

                if (b%found) then
                    matching_beg = b
                    count = count - 1
                else if (found_program_block_end()) then
                    count = count + 1
                end if
            end if
        end do

        call beginning_of_line(1_int64)

        if (count /= 0) then
            ! Note this includes the case of an un-named main program,
            ! in which case we go to (point-min).
            if (present(interactive)) then
                if (interactive) call message("No beginning found")
            end if

            matching_beg = f90_block_type()
        end if

        call restore_case(saved)
    end function f90_beginning_of_subprogram

    !> Move point to the beginning of the current subprogram (the command).
    !> Port of f90.el:f90-beginning-of-subprogram.
    subroutine cmd_f90_beginning_of_subprogram(status)
        type(status_type), intent(out) :: status
        type(f90_block_type) :: b

        status = status_type()
        b = f90_beginning_of_subprogram(.true.)
    end subroutine cmd_f90_beginning_of_subprogram

    ! `f90_beginning_of_subprogram' as `beginning_of_subprogram_function'
    ! (called with ARG, so INTERACTIVE is true).
    ! Not a port.
    recursive subroutine f90_subprogram_start(arg, found, status)
        integer(int64), intent(in) :: arg
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(f90_block_type) :: b

        status = status_type()
        ! ARG is the INTERACTIVE argument: true, as any number is.
        b = f90_beginning_of_subprogram(arg == arg)
        found = b%found
    end subroutine f90_subprogram_start

    ! `f90_end_of_subprogram' as `end_of_subprogram_function'.
    ! Not a port.
    recursive subroutine f90_subprogram_end(status)
        type(status_type), intent(out) :: status
        type(f90_block_type) :: b

        status = status_type()
        b = f90_end_of_subprogram()
    end subroutine f90_subprogram_end

    !> Move point to the end of the current subprogram.
    !> Return TYPE and NAME, or nothing (FOUND .false.) if not found.  Port of
    !> f90.el:f90-end-of-subprogram.
    function f90_end_of_subprogram(interactive) result(matching_end)
        logical, intent(in), optional :: interactive
        type(f90_block_type) :: matching_end
        type(f90_block_type) :: b
        integer :: count
        logical :: saved

        call fold_case(saved)
        count = 1
        call end_of_line(1_int64)

        do while (count > 0)
            if (.not. search_re(f90_program_block_re, noerror=SEARCH_MOVE)) exit
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")

            if (f90_in_string()) then
                continue
            else if (found_program_block_start()) then
                count = count + 1
            else
                b = f90_looking_at_program_block_end()

                if (b%found) then
                    matching_end = b
                    count = count - 1
                end if
            end if

            call end_of_line(1_int64)
        end do

        ! This means f90-end-of-subprogram followed by f90-start-of-subprogram
        ! has a net non-zero effect, which seems odd.
        if (count /= 0) then
            if (present(interactive)) then
                if (interactive) call message("No end found")
            end if

            matching_end = f90_block_type()
        end if

        call restore_case(saved)
    end function f90_end_of_subprogram

    !> Move point to the end of the current subprogram (the command).  Port of
    !> f90.el:f90-end-of-subprogram.
    subroutine cmd_f90_end_of_subprogram(status)
        type(status_type), intent(out) :: status
        type(f90_block_type) :: b

        status = status_type()
        b = f90_end_of_subprogram(.true.)
    end subroutine cmd_f90_end_of_subprogram

    !> The block that starts after point, if any (the alternatives
    !> `f90_end_of_block' and `f90_beginning_of_block' try).  Not a port.
    recursive subroutine block_start(b, status)
        type(f90_block_type), intent(out) :: b
        type(status_type), intent(out) :: status

        status = status_type()
        b = f90_looking_at_do()
        if (b%found) return
        b = f90_looking_at_select_case()
        if (b%found) return
        b = f90_looking_at_type_like()
        if (b%found) return
        b = f90_looking_at_associate()
        if (b%found) return
        b = f90_looking_at_critical()
        if (b%found) return
        b = f90_looking_at_program_block_start()
        if (b%found) return
        call f90_looking_at_if_then(b, status)
        if (b%found .or. .not. status%ok()) return
        call f90_looking_at_where_or_forall(b, status)
    end subroutine block_start

    !> A string for messages: S, or "nil" (upstream's message text).  Not a port.
    function or_nil(s) result(r)
        character(len=:), allocatable, intent(in) :: s
        character(len=:), allocatable :: r

        if (allocated(s)) then
            r = s
        else
            r = "nil"
        end if
    end function or_nil

    !> Move point forward to the end of the current code block.
    !> With optional argument NUM, go forward that many balanced blocks.
    !> If NUM is negative, go backward to the start of a block.  Checks
    !> for consistency of block types and labels (if present), and
    !> completes outermost block if `f90_smart_end' is true.
    !> Interactively, pushes mark before moving point.  Port of
    !> f90.el:f90-end-of-block.
    recursive subroutine f90_end_of_block(status, num, interactive)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: num
        logical, intent(in), optional :: interactive
        type(f90_block_type), allocatable :: start_list(:)
        type(f90_block_type) :: start_this
        character(len=:), allocatable :: end_type, end_label
        integer(int64) :: count, saved_smart, opoint
        logical :: saved

        status = status_type()

        ! Can move some distance.
        if (present(interactive)) then
            if (interactive) call push_mark(point(), .true.)
        end if

        if (present(num)) then
            if (num < 0) then
                call f90_beginning_of_block(status, -num)
                if (.not. status%ok()) return
            end if
        end if

        saved_smart = f90_smart_end%value
        ! for final match-end
        if (f90_smart_end%value /= F90_SMART_END_NO) f90_smart_end%value = F90_SMART_END_NO_BLINK
        call fold_case(saved)
        count = 1
        if (present(num)) count = num
        allocate (start_list(0))
        call end_of_line(1_int64) ! probably want this

        do while (count > 0)
            if (.not. search_re(f90_blocks_re, noerror=SEARCH_MOVE)) exit
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")

            if (f90_in_string() .or. f90_in_comment()) then
                continue
            else
                call block_start(start_this, status)
                if (.not. status%ok()) exit

                if (start_this%found) then
                    ! not add-to-list!
                    start_list = [start_this, start_list]
                    count = count + 1
                else if (looking("end[ "//TAB//"]*"//f90_blocks_re//"[ "//TAB//"]*\("//SYMRE//"+\)?")) then
                    if (matched(1)) end_type = ms(1)
                    if (matched(2)) end_label = ms(2)
                    count = count - 1

                    ! Check any internal blocks.
                    if (size(start_list) > 0) then
                        start_this = start_list(1)
                        start_list = start_list(2:)

                        if (.not. f90_equal_names(start_this%kind, end_type)) then
                            call set_error(status, FM_GENERIC_ERROR, format_message("End type `"//or_nil(end_type)// &
                                                                                    "' does not match start type `"// &
                                                                                    or_nil(start_this%kind)//"'"))
                            exit
                        end if

                        if (.not. f90_equal_names(start_this%name, end_label)) then
                            call set_error(status, FM_GENERIC_ERROR, format_message("End label `"//or_nil(end_label)// &
                                                                                    "' does not match start label `"// &
                                                                                    or_nil(start_this%name)//"'"))
                            exit
                        end if
                    end if

                    if (allocated(end_type)) deallocate (end_type)
                    if (allocated(end_label)) deallocate (end_label)
                end if
            end if

            call end_of_line(1_int64)
        end do

        if (status%ok() .and. count > 0) call set_error(status, FM_GENERIC_ERROR, "Missing block end")

        ! Check outermost block.
        if (status%ok() .and. f90_smart_end%value /= F90_SMART_END_NO) then
            opoint = point()
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")
            call f90_match_end(status)
            call goto_char(opoint)
        end if

        f90_smart_end%value = saved_smart
        call restore_case(saved)
    end subroutine f90_end_of_block

    !> Move point forward to the end of the current code block (the
    !> command).  Port of f90.el:f90-end-of-block.
    recursive subroutine cmd_f90_end_of_block(status)
        type(status_type), intent(out) :: status

        call f90_end_of_block(status, prefix_numeric_value(current_prefix_arg), .true.)
    end subroutine cmd_f90_end_of_block

    !> Move point backwards to the start of the current code block.
    !> With optional argument NUM, go backward that many balanced blocks.
    !> If NUM is negative, go forward to the end of a block.
    !> Checks for consistency of block types and labels (if present).
    !> Does not check the outermost block, because it may be incomplete.
    !> Interactively, pushes mark before moving point.  Port of
    !> f90.el:f90-beginning-of-block.
    recursive subroutine f90_beginning_of_block(status, num, interactive)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: num
        logical, intent(in), optional :: interactive
        type(f90_block_type), allocatable :: end_list(:)
        type(f90_block_type) :: end_this, start_this
        integer(int64) :: count
        logical :: saved

        status = status_type()

        if (present(interactive)) then
            if (interactive) call push_mark(point(), .true.)
        end if

        if (present(num)) then
            if (num < 0) then
                call f90_end_of_block(status, -num)
                if (.not. status%ok()) return
            end if
        end if

        call fold_case(saved)
        count = 1
        if (present(num)) count = num
        allocate (end_list(0))
        call beginning_of_line(1_int64) ! probably want this

        do while (count > 0)
            if (.not. search_re_backward_move(f90_blocks_re)) exit
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")

            if (f90_in_string() .or. f90_in_comment()) then
                continue
            else if (looking("end[ "//TAB//"]*"//f90_blocks_re//"[ "//TAB//"]*\("//SYMRE//"+\)?")) then
                end_this = blk(1, 2)
                end_list = [end_this, end_list]
                count = count + 1
            else
                call block_start(start_this, status)
                if (.not. status%ok()) exit

                if (start_this%found) then
                    count = count - 1

                    ! Check any internal blocks.
                    if (size(end_list) > 0) then
                        end_this = end_list(1)
                        end_list = end_list(2:)

                        if (.not. f90_equal_names(start_this%kind, end_this%kind)) then
                            call set_error(status, FM_GENERIC_ERROR, format_message("Start type `"// &
                                                                                    or_nil(start_this%kind)// &
                                                                                    "' does not match end type `"// &
                                                                                    or_nil(end_this%kind)//"'"))
                            exit
                        end if

                        if (.not. f90_equal_names(start_this%name, end_this%name)) then
                            call set_error(status, FM_GENERIC_ERROR, format_message("Start label `"// &
                                                                                    or_nil(start_this%name)// &
                                                                                    "' does not match end label `"// &
                                                                                    or_nil(end_this%name)//"'"))
                            exit
                        end if
                    end if
                end if
            end if
        end do

        ! Includes an un-named main program block.
        if (status%ok() .and. count > 0) call set_error(status, FM_GENERIC_ERROR, "Missing block start")
        call restore_case(saved)
    end subroutine f90_beginning_of_block

    !> Move point backwards to the start of the current code block (the
    !> command).  Port of f90.el:f90-beginning-of-block.
    recursive subroutine cmd_f90_beginning_of_block(status)
        type(status_type), intent(out) :: status

        call f90_beginning_of_block(status, prefix_numeric_value(current_prefix_arg), .true.)
    end subroutine cmd_f90_beginning_of_block

    !> Move point forward to the next end or start of a code block.
    !> With optional argument NUM, go forward that many blocks.
    !> If NUM is negative, go backwards.
    !> A block is a subroutine, if-endif, etc.  Port of
    !> f90.el:f90-next-block.
    recursive subroutine f90_next_block(status, num)
        type(status_type), intent(out) :: status
        integer(int64), intent(in) :: num
        type(f90_block_type) :: b
        integer(int64) :: count
        logical :: saved, found

        status = status_type()
        call fold_case(saved)
        count = abs(num)

        do while (count > 0)
            if (num > 0) then
                found = search_re(f90_blocks_re, noerror=SEARCH_MOVE)
            else
                found = search_re_backward_move(f90_blocks_re)
            end if

            if (.not. found) exit
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")

            if (f90_in_string() .or. f90_in_comment()) then
                continue
            else if (looking("end[ "//TAB//"]*")) then
                count = count - 1
            else
                call block_start(b, status)
                if (.not. status%ok()) exit
                if (b%found) count = count - 1
            end if

            if (num > 0) then
                call end_of_line(1_int64)
            else
                call beginning_of_line(1_int64)
            end if
        end do

        call restore_case(saved)
    end subroutine f90_next_block

    !> Move point forward to the next end or start of a code block (the
    !> command).  Port of f90.el:f90-next-block.
    recursive subroutine cmd_f90_next_block(status)
        type(status_type), intent(out) :: status

        call f90_next_block(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_f90_next_block

    !> Move point backward to the previous end or start of a code block.
    !> With optional argument NUM, go backward that many blocks.
    !> If NUM is negative, go forwards.
    !> A block is a subroutine, if-endif, etc.  Port of
    !> f90.el:f90-previous-block.
    recursive subroutine f90_previous_block(status, num)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: num
        integer(int64) :: n

        n = 1
        if (present(num)) n = num
        call f90_next_block(status, -n)
    end subroutine f90_previous_block

    !> Move point backward to the previous end or start of a code block
    !> (the command).  Port of f90.el:f90-previous-block.
    recursive subroutine cmd_f90_previous_block(status)
        type(status_type), intent(out) :: status

        call f90_previous_block(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_f90_previous_block

    !> Put mark at end of F90 subprogram, point at beginning, push mark.
    !> Port of f90.el:f90-mark-subprogram.
    function f90_mark_subprogram() result(program)
        use buffer_fm, only: deactivate_mark_variable
        type(f90_block_type) :: program
        type(f90_block_type) :: b
        integer(int64) :: pos

        pos = point()
        b = f90_end_of_subprogram()
        call push_mark()
        call goto_char(pos)
        program = f90_beginning_of_subprogram()
        buffers(cur)%mark_active = .true.
        deactivate_mark_variable = .false.
    end function f90_mark_subprogram

    !> Put mark at end of F90 subprogram, point at beginning, push mark
    !> (the command).  Port of f90.el:f90-mark-subprogram.
    subroutine cmd_f90_mark_subprogram(status)
        type(status_type), intent(out) :: status
        type(f90_block_type) :: b

        status = status_type()
        b = f90_mark_subprogram()
    end subroutine cmd_f90_mark_subprogram

    !> Comment/uncomment every line in the region.
    !> Insert the variable `f90_comment_region' at the start of every line
    !> in the region, or, if already present, remove it.  Port of
    !> f90.el:f90-comment-region (the command `f90_comment_region').
    subroutine f90_comment_region_command(beg_region, end_region, status)
        integer(int64), intent(in) :: beg_region, end_region
        type(status_type), intent(out) :: status
        type(handle_type) :: end
        character(len=:), allocatable :: re

        status = status_type()
        end = marker_at(end_region)
        re = regexp_quote(f90_comment_region%value)
        call goto_char(beg_region)
        call beginning_of_line(1_int64)
        call toggle()

        do while (status%ok())
            if (forward_line(1_int64) /= 0) exit
            if (.not. (point() < mpos(end))) exit
            call toggle()
        end do

        call unmark(end)

    contains

        subroutine toggle()
            if (looking(re)) then
                call delete_region(point(), me(0), status)
            else
                call insert(f90_comment_region%value, status)
            end if
        end subroutine toggle
    end subroutine f90_comment_region_command

    !> Comment/uncomment every line in the region (the command).  Port of
    !> f90.el:f90-comment-region.
    subroutine cmd_f90_comment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call barf_if_buffer_read_only(status)
        if (status%ok()) call interactive_region(b, e, status)
        if (status%ok()) call f90_comment_region_command(b, e, status)
    end subroutine cmd_f90_comment_region

    !> Indent current line as F90 code.
    !> Unless optional argument NO_UPDATE is true, call `f90_update_line'
    !> after indenting.  Port of f90.el:f90-indent-line.
    recursive subroutine f90_indent_line(status, no_update)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_update
        type(handle_type) :: pos
        integer(int64) :: indent, opoint
        logical :: saved, no_line_number, cont, noupd

        status = status_type()
        noupd = .false.
        if (present(no_update)) noupd = no_update
        call fold_case(saved)
        pos = marker_at(point())
        no_line_number = .false.
        call beginning_of_line(1_int64) ! digits after & \n are not line-nos
        opoint = point()
        cont = .false.
        if (f90_previous_statement()) cont = f90_line_continued()
        call goto_char(opoint)

        if (.not. cont) then
            call f90_indent_line_no()
        else
            no_line_number = .true.
            call skip_forward(" "//TAB)
        end if

        ! FIXME This means f90-calculate-indent gives different answers
        ! for comments and preprocessor lines to this function.
        ! Better to make f90-calculate-indent return the correct answer?
        if (following() == iachar("!")) then
            call f90_comment_indent(indent, status)
        else if (following() == iachar("#")) then
            indent = 0
        else
            if (f90_smart_end%value /= F90_SMART_END_NO .and. looking("end")) call f90_match_end(status)
            if (status%ok()) call f90_calculate_indent(indent, status)
        end if

        if (status%ok()) then
            if (indent /= current_column()) call f90_indent_to(indent, status, no_line_number)
        end if

        ! If initial point was within line's indentation,
        ! position after the indentation.  Else stay at same point in text.
        if (point() < mpos(pos)) call goto_char(mpos(pos))

        if (status%ok()) then
            if (associated(auto_fill_function%value)) then
                ! GM NO-UPDATE not honored, since this calls f90-update-line.
                call f90_do_auto_fill(status)
            else if (.not. noupd) then
                call f90_update_line(status)
            end if
        end if

        call unmark(pos)
        call restore_case(saved)
    end subroutine f90_indent_line

    !> `f90_indent_line' as an `indent_line_function'.  Not a port.
    recursive subroutine f90_indent_line_function(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent

        noindent = .false.
        call f90_indent_line(status)
    end subroutine f90_indent_line_function

    !> Indent current line as F90 code (the command).  Port of
    !> f90.el:f90-indent-line.
    recursive subroutine cmd_f90_indent_line(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_indent_line(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_f90_indent_line

    !> Re-indent current line, insert a newline and indent the newline.
    !> An abbrev before point is expanded if the variable `abbrev_mode' is true.
    !> If run in the middle of a line, the line is not broken.  Port of
    !> f90.el:f90-indent-new-line.
    recursive subroutine f90_indent_new_line(status)
        type(status_type), intent(out) :: status
        logical :: string, cont

        status = status_type()

        if (associated(abbrev_mode)) then
            if (abbrev_mode%value) call expand_abbrev(status)
        end if

        if (.not. status%ok()) return
        call beginning_of_line(1_int64) ! reindent where likely to be needed
        call f90_indent_line(status) ! calls indent-line-no, update-line
        if (.not. status%ok()) return
        call end_of_line(1_int64)
        call delete_horizontal_space(.false., status) ! destroy trailing whitespace
        if (.not. status%ok()) return
        string = f90_in_string()
        cont = f90_line_continued()
        if (string .and. .not. cont) call insert("&", status)
        if (status%ok()) call newline(status)
        if (.not. status%ok()) return
        if (string .or. (cont .and. f90_beginning_ampersand%value)) call insert("&", status)
        if (status%ok()) call f90_indent_line(status, .true.) ! nothing to update
    end subroutine f90_indent_new_line

    !> Re-indent current line, insert a newline and indent the newline (the
    !> command).  Port of f90.el:f90-indent-new-line.
    recursive subroutine cmd_f90_indent_new_line(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_indent_new_line(status)
    end subroutine cmd_f90_indent_new_line

    !> The block that starts after point for `f90_indent_region', and the
    !> extra indentation it gives.  Not a port.
    recursive subroutine region_block(struct, ind_b, has_ind, status, else_like, contains_too)
        type(f90_block_type), intent(out) :: struct
        integer(int64), intent(out) :: ind_b
        logical, intent(out) :: has_ind
        type(status_type), intent(out) :: status
        logical, intent(in) :: else_like, contains_too

        status = status_type()
        has_ind = .true.
        struct = f90_looking_at_do()

        if (struct%found) then
            ind_b = f90_do_indent%value
            return
        end if

        call f90_looking_at_if_then(struct, status)
        if (.not. status%ok()) return
        if (.not. struct%found) struct = f90_looking_at_select_case()

        if (.not. struct%found) then
            call f90_looking_at_where_or_forall(struct, status)
            if (.not. status%ok()) return
        end if

        if (struct%found) then
            ind_b = f90_if_indent%value
            return
        end if

        if (else_like) then
            if (looking(f90_else_like_re)) then
                ind_b = f90_if_indent%value
                return
            end if
        end if

        struct = f90_looking_at_type_like()

        if (struct%found) then
            ind_b = f90_type_indent%value
            return
        end if

        struct = f90_looking_at_associate()

        if (struct%found) then
            ind_b = f90_associate_indent%value
            return
        end if

        struct = f90_looking_at_critical()

        if (struct%found) then
            ind_b = f90_critical_indent%value
            return
        end if

        struct = f90_looking_at_program_block_start()

        if (struct%found) then
            ind_b = f90_program_indent%value
            return
        end if

        if (contains_too) then
            if (looking("contains[ "//TAB//"]*\($\|!\)")) then
                ind_b = f90_program_indent%value
                return
            end if
        end if

        has_ind = .false.
        ind_b = 0
    end subroutine region_block

    !> Indent every line in region by forward parsing.  Port of
    !> f90.el:f90-indent-region.
    recursive subroutine f90_indent_region(beg_region, end_region, status)
        integer(int64), intent(in) :: beg_region, end_region
        type(status_type), intent(out) :: status
        type(handle_type) :: end_region_mark, save_point
        type(f90_block_type), allocatable :: block_list(:)
        type(f90_block_type) :: struct, beg_struct, end_struct
        integer(int64) :: ind_lev, ind_curr, ind_b, col
        integer :: cont
        logical :: saved, has_ind

        status = status_type()
        end_region_mark = marker_at(end_region)
        save_point = marker_at(point())
        call fold_case(saved)
        allocate (block_list(0))

        body: block
            call goto_char(beg_region)
            ! First find a line which is not a continuation line or comment.
            call beginning_of_line(1_int64)

            do while (looking("[ "//TAB//"]*[0-9]*\(!\|#\|[ "//TAB//"]*$\)"))
                call f90_indent_line(status, .true.)
                if (.not. status%ok()) exit body
                if (forward_line(1_int64) /= 0) exit
                if (.not. (point() < mpos(end_region_mark))) exit
            end do

            cont = f90_present_statement_cont()

            do while (any(cont == [CONT_MIDDLE, CONT_END]))
                if (.not. f90_previous_statement()) exit
                cont = f90_present_statement_cont()
            end do

            ! Process present line for beginning of block.
            f90_cache_position = point()
            call f90_indent_line(status, .true.)
            if (.not. status%ok()) exit body
            ind_lev = f90_current_indentation()
            ind_curr = ind_lev
            call beginning_of_line(1_int64)
            call skip_forward(" "//TAB//"0-9")
            call region_block(struct, ind_b, has_ind, status, .true., .true.)
            if (.not. status%ok()) exit body
            if (has_ind) ind_lev = ind_lev + ind_b
            if (struct%found) block_list = [struct, block_list]

            do
                if (.not. f90_line_continued()) exit
                if (forward_line(1_int64) /= 0) exit
                if (.not. (point() < mpos(end_region_mark))) exit

                if (looking("[ "//TAB//"]*!")) then
                    call f90_comment_indent(col, status)
                    if (status%ok()) call f90_indent_to(col, status)
                else if (current_indentation() /= ind_curr + f90_continuation_indent%value) then
                    call f90_indent_to(ind_curr + f90_continuation_indent%value, status, .true.)
                end if

                if (.not. status%ok()) exit body
            end do

            ! Process all following lines.
            do
                if (forward_line(1_int64) /= 0) exit
                if (.not. (point() < mpos(end_region_mark))) exit
                call beginning_of_line(1_int64)
                call f90_indent_line_no()
                f90_cache_position = point()

                if (looking("[ "//TAB//"]*$")) then
                    ind_curr = 0
                else if (looking("[ "//TAB//"]*#")) then
                    ind_curr = 0
                else if (looking("!")) then
                    call f90_comment_indent(ind_curr, status)
                else if (f90_no_block_limit()) then
                    ind_curr = ind_lev
                else if (looking(f90_else_like_re)) then
                    ind_curr = ind_lev - f90_if_indent%value
                else if (looking("contains[ "//TAB//"]*\($\|!\)")) then
                    ind_curr = ind_lev - f90_program_indent%value
                else
                    call region_block(struct, ind_b, has_ind, status, .false., .false.)
                    if (.not. status%ok()) exit body

                    if (has_ind) then
                        ind_curr = ind_lev
                        ind_lev = ind_lev + ind_b
                        block_list = [struct, block_list]
                    else
                        end_struct = f90_looking_at_program_block_end()

                        if (end_struct%found) then
                            beg_struct = f90_block_type()

                            if (size(block_list) > 0) then
                                beg_struct = block_list(1)
                                block_list = block_list(2:)
                            end if

                            if (f90_smart_end%value /= F90_SMART_END_NO) then
                                col = point()
                                call f90_block_match(beg_struct%kind, beg_struct%name, end_struct%kind, end_struct%name, &
                                                     status)
                                call goto_char(col)
                                if (.not. status%ok()) exit body
                            end if

                            has_ind = .true.

                            if (looking(f90_end_if_re)) then
                                ind_b = f90_if_indent%value
                            else if (looking("end[ "//TAB//"]*do\_>")) then
                                ind_b = f90_do_indent%value
                            else if (looking(f90_end_type_re)) then
                                ind_b = f90_type_indent%value
                            else if (looking(f90_end_associate_re)) then
                                ind_b = f90_associate_indent%value
                            else if (f90_looking_at_end_critical()) then
                                ind_b = f90_critical_indent%value
                            else if (found_program_block_end()) then
                                ind_b = f90_program_indent%value
                            else
                                has_ind = .false.
                            end if

                            if (has_ind) ind_lev = ind_lev - ind_b
                            ind_curr = ind_lev
                        else
                            ind_curr = ind_lev
                        end if
                    end if
                end if

                if (.not. status%ok()) exit body
                ! Do the indentation if necessary.
                if (ind_curr /= current_column()) call f90_indent_to(ind_curr, status)
                if (.not. status%ok()) exit body

                do
                    if (.not. f90_line_continued()) exit
                    if (forward_line(1_int64) /= 0) exit
                    if (.not. (point() < mpos(end_region_mark))) exit

                    if (looking("[ "//TAB//"]*#")) then
                        call f90_indent_to(0_int64, status)
                    else if (looking("[ "//TAB//"]*!")) then
                        call f90_comment_indent(col, status)
                        if (status%ok()) call f90_indent_to(col, status)
                    else if (current_indentation() /= ind_curr + f90_continuation_indent%value) then
                        call f90_indent_to(ind_curr + f90_continuation_indent%value, status, .true.)
                    end if

                    if (.not. status%ok()) exit body
                end do
            end do
            ! Restore point, etc.
        end block body

        f90_cache_position = 0
        call goto_char(mpos(save_point))
        call unmark(end_region_mark)
        call unmark(save_point)
        call deactivate_mark()
        call restore_case(saved)
    end subroutine f90_indent_region

    !> Indent every line in region by forward parsing (the command).  Port of
    !> f90.el:f90-indent-region.
    recursive subroutine cmd_f90_indent_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call barf_if_buffer_read_only(status)
        if (status%ok()) call interactive_region(b, e, status)
        if (status%ok()) call f90_indent_region(b, e, status)
    end subroutine cmd_f90_indent_region

    !> Properly indent the subprogram containing point.  Port of
    !> f90.el:f90-indent-subprogram.
    recursive subroutine f90_indent_subprogram(status)
        use simple_fm, only: mark
        type(status_type), intent(out) :: status
        type(f90_block_type) :: program
        type(handle_type) :: opoint
        integer(int64) :: m
        character(len=:), allocatable :: what

        status = status_type()
        opoint = marker_at(point())
        program = f90_mark_subprogram()
        call mark(m, status)

        if (.not. status%ok()) then
            call goto_char(mpos(opoint))
            call unmark(opoint)
            return
        end if

        if (program%found) then
            what = or_nil(program%kind)//" "//or_nil(program%name)
            call message("Indenting "//what//"...")
            call indent_region(min(point(), m), max(point(), m), status)
            if (status%ok()) call message("Indenting "//what//"...done")
        else
            call message("Indenting the whole file...")
            call indent_region(min(point(), m), max(point(), m), status)
            if (status%ok()) call message("Indenting the whole file...done")
        end if

        call goto_char(mpos(opoint))
        call unmark(opoint)
    end subroutine f90_indent_subprogram

    !> Properly indent the subprogram containing point (the command).  Port of
    !> f90.el:f90-indent-subprogram.
    recursive subroutine cmd_f90_indent_subprogram(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_indent_subprogram(status)
    end subroutine cmd_f90_indent_subprogram

    !> Break line at point, insert continuation marker(s) and indent.
    !> Unless in a string or comment, or if the optional argument NO_UPDATE
    !> is true, call `f90_update_line' after inserting the continuation marker.
    !> Port of f90.el:f90-break-line.
    recursive subroutine f90_break_line(status, no_update)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_update
        character(len=:), allocatable :: ctype
        logical :: found, noupd

        noupd = .false.
        if (present(no_update)) noupd = no_update
        status = status_type()

        if (f90_in_string()) then
            call insert("&"//LF//"&", status)
        else if (f90_in_comment()) then
            call delete_horizontal_space(.false., status) ! remove trailing whitespace
            if (status%ok()) call f90_get_present_comment_type(ctype, found, status)
            if (status%ok()) call insert(LF//ctype, status)
        else
            call insert("&", status)
            if (status%ok() .and. .not. noupd) call f90_update_line(status)
            if (status%ok()) call newline(status, 1_int64)
            ! FIXME also need leading ampersand if split lexical token (eg ==).
            ! Or respect f90-no-break-re.
            if (status%ok() .and. f90_beginning_ampersand%value) call insert("&", status)
        end if

        if (status%ok()) call indent_according_to_mode(status)
    end subroutine f90_break_line

    !> Break line at point, insert continuation marker(s) and indent (the
    !> command).  Port of f90.el:f90-break-line.
    recursive subroutine cmd_f90_break_line(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_break_line(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_f90_break_line

    !> From `fill_column', search backward for break-delimiter.  Port of
    !> f90.el:f90-find-breakpoint.
    subroutine f90_find_breakpoint(status)
        type(status_type), intent(out) :: status
        integer(int64) :: found

        ! Commented text more likely than commented code.
        if (f90_in_comment()) then
            call re_search_backward("\s-", found, status, bound=line_beginning_position())
        else
            call re_search_backward(f90_break_delimiters%value, found, status, bound=line_beginning_position())
            if (.not. status%ok()) return

            if (.not. f90_break_before_delimiters%value) then
                if (looking(f90_no_break_re)) then
                    call forward_char(status, 2_int64)
                else
                    call forward_char(status, 1_int64)
                end if
            else
                call forward_char(status, -1_int64)
                if (status%ok() .and. .not. looking(f90_no_break_re)) call forward_char(status, 1_int64)
            end if
        end if
    end subroutine f90_find_breakpoint

    !> Break line if non-white characters beyond `fill_column'.
    !> Update keyword case first.  Port of f90.el:f90-do-auto-fill.
    recursive subroutine f90_do_auto_fill(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: pos_mark
        integer(int64) :: col

        ! Break line before or after last delimiter (non-word char) if
        ! position is beyond fill-column.
        ! Will not break **, //, or => (as specified by f90-no-break-re).
        call f90_update_line(status)
        if (.not. status%ok()) return
        ! Need this for `f90-electric-insert' and other f90- callers.
        if (comment_auto_fill_only_comments%value .and. .not. f90_in_comment()) return

        do while (current_column() > fill_column%value)
            pos_mark = marker_at(point())
            call move_to_column(fill_column%value, col)
            if (.not. f90_in_string()) call f90_find_breakpoint(status)
            if (status%ok()) call f90_break_line(status)
            call goto_char(mpos(pos_mark))
            call unmark(pos_mark)
            if (.not. status%ok()) exit
        end do
    end subroutine f90_do_auto_fill

    !> Break line if non-white characters beyond `fill_column' (the
    !> command).  Port of f90.el:f90-do-auto-fill.
    recursive subroutine cmd_f90_do_auto_fill(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_do_auto_fill(status)
    end subroutine cmd_f90_do_auto_fill

    !> Join current line to previous, fix whitespace, continuation, comments.
    !> With optional argument ARG, join current line to following line.
    !> Like `join_line', but handles F90 syntax.  Port of
    !> f90.el:f90-join-lines.
    recursive subroutine f90_join_lines(status, arg)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        integer(int64) :: shortage

        status = status_type()
        call beginning_of_line(1_int64)

        if (present(arg)) then
            if (arg) shortage = forward_line(1_int64)
        end if

        if (preceding() == 10) then
            call skip_forward(" "//TAB)
            if (looking("&")) call delete_char(1_int64, status)
            if (.not. status%ok()) return
            call beginning_of_line(1_int64)
            call delete_region(point(), point() - 1, status)
            if (.not. status%ok()) return
            call skip_backward(" "//TAB)
            if (preceding() == iachar("&")) call delete_char(-1_int64, status)
            if (.not. status%ok()) return

            if (f90_in_comment()) then
                if (looking("[ "//TAB//"]*!+")) call replace_match("", status)
                if (.not. status%ok()) return
            end if

            if (.not. f90_in_string()) call fixup_whitespace(status)
        end if
    end subroutine f90_join_lines

    !> Join current line to previous, fix whitespace, continuation, comments
    !> (the command).  Port of f90.el:f90-join-lines.
    recursive subroutine cmd_f90_join_lines(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_join_lines(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_f90_join_lines

    !> Fill every line in region by forward parsing.  Join lines if possible.
    !> Port of f90.el:f90-fill-region.
    recursive subroutine f90_fill_region(beg_region, end_region, status)
        type(status_type), intent(out) :: status
        integer(int64), intent(in) :: beg_region, end_region
        type(handle_type) :: end_region_mark
        integer(int64) :: saved_smart, saved_case, col, eol_col, opoint
        procedure(command_proc), pointer :: saved_fill
        logical :: go_on

        status = status_type()
        end_region_mark = marker_at(end_region)
        go_on = .true.
        saved_smart = f90_smart_end%value
        saved_case = f90_auto_keyword_case%value
        saved_fill => auto_fill_function%value
        f90_smart_end%value = F90_SMART_END_NO
        f90_auto_keyword_case%value = F90_KEYWORD_CASE_NONE
        auto_fill_function%value => null()
        call goto_char(beg_region)

        do while (go_on)
            ! Join as much as possible.
            do
                call end_of_line(1_int64)
                call skip_backward(" "//TAB)
                if (preceding() /= iachar("&")) exit
                call f90_join_lines(status, .true.)
                if (.not. status%ok()) exit
            end do

            if (.not. status%ok()) exit

            ! Chop the line if necessary.
            do
                opoint = point()
                call end_of_line(1_int64)
                eol_col = current_column()
                call goto_char(opoint)
                if (.not. (eol_col > fill_column%value)) exit
                call move_to_column(fill_column%value, col)
                call f90_find_breakpoint(status)
                if (status%ok()) call f90_break_line(status, .true.)
                if (.not. status%ok()) exit
            end do

            if (.not. status%ok()) exit
            go_on = point() < mpos(end_region_mark)
            if (go_on) go_on = forward_line(1_int64) == 0
            f90_cache_position = point()
        end do

        f90_cache_position = 0
        call unmark(end_region_mark)
        call deactivate_mark()
        f90_smart_end%value = saved_smart
        f90_auto_keyword_case%value = saved_case
        auto_fill_function%value => saved_fill
    end subroutine f90_fill_region

    !> Fill every line in region by forward parsing (the command).  Port of
    !> f90.el:f90-fill-region.
    recursive subroutine cmd_f90_fill_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call barf_if_buffer_read_only(status)
        if (status%ok()) call interactive_region(b, e, status)
        if (status%ok()) call f90_fill_region(b, e, status)
    end subroutine cmd_f90_fill_region

    !> In a comment, fill it as a paragraph, else fill the current statement.
    !> For use as the value of `fill_paragraph_function'.
    !> Passes optional argument JUSTIFY to `fill_comment_paragraph'.
    !> Always returns true (DONE, to prevent `fill_paragraph' being called).
    !> Port of f90.el:f90-fill-paragraph.
    recursive subroutine f90_fill_paragraph(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        integer(int64) :: opoint, end
        logical :: moved

        call fill_comment_paragraph(status, done, justify)

        if (done .or. .not. status%ok()) then
            done = .true.
            return
        end if

        opoint = point()
        moved = f90_next_statement()

        if (is_bob()) then
            end = point()
        else
            end = point() - 1
        end if

        moved = f90_previous_statement()
        call f90_fill_region(point(), end, status)
        call goto_char(min(opoint, point_max()))
        done = .true.
    end subroutine f90_fill_paragraph

    !> In a comment, fill it as a paragraph, else fill the current statement
    !> (the command).  Port of f90.el:f90-fill-paragraph.
    recursive subroutine cmd_f90_fill_paragraph(status)
        use fill_fm, only: JUSTIFY_FULL
        type(status_type), intent(out) :: status
        logical :: done

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return

        if (current_prefix_arg%kind == PREFIX_NONE) then
            call f90_fill_paragraph(status, done)
        else
            call f90_fill_paragraph(status, done, JUSTIFY_FULL)
        end if
    end subroutine cmd_f90_fill_paragraph

    !> Whether S is one of `f90_end_block_optional_name' (ignoring case).
    !> Not a port.
    logical function optional_name_block(s)
        character(len=*), intent(in) :: s
        integer :: i

        optional_name_block = .false.

        do i = 1, size(f90_end_block_optional_name)
            if (downcase(s) == trim(f90_end_block_optional_name(i))) optional_name_block = .true.
        end do
    end function optional_name_block

    !> (search-forward S (line-end-position)), case folded as
    !> `case_fold_search' says.  Not a port.
    subroutine search_eol(s, status)
        character(len=*), intent(in) :: s
        type(status_type), intent(out) :: status
        integer(int64) :: found

        call search_forward(s, found, status, bound=line_end_position())
    end subroutine search_eol

    !> (search-forward S) Not a port.
    subroutine search_fwd(s, status)
        character(len=*), intent(in) :: s
        type(status_type), intent(out) :: status
        integer(int64) :: found

        call search_forward(s, found, status)
    end subroutine search_fwd

    !> Match end-struct with beg-struct and complete end-block if possible.
    !> BEG_BLOCK is the type of block as indicated at the start (e.g., do).
    !> BEG_NAME is the block start name (may be unallocated).
    !> END_BLOCK is the type of block as indicated at the end (may be unallocated).
    !> END_NAME is the block end name (may be unallocated).
    !> If the block type matches `f90_end_block_optional_name', do not add
    !> an end name if `f90_smart_end_names' is false, but always update an
    !> incorrect end name if there already was one.
    !> Leave point at the end of line.  Port of f90.el:f90-block-match.
    recursive subroutine f90_block_match(beg_block_in, beg_name, end_block, end_name, status)
        character(len=:), allocatable, intent(in) :: beg_block_in, beg_name, end_block, end_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: beg_block

        status = status_type()

        ! Hack to deal with the case when this is called from
        ! f90-indent-region on a program block without an explicit PROGRAM
        ! statement at the start. Should really be an error (?).
        if (allocated(beg_block_in)) then
            beg_block = beg_block_in
        else
            beg_block = "program"
        end if

        call search_eol("end", status)
        if (.not. status%ok()) return

        no_match: block
            if (allocated(end_block)) then
                if (f90_equal_names(beg_block, end_block)) then
                    call search_fwd(end_block, status)
                    if (.not. status%ok()) return
                else
                    call message("END "//end_block//" does not match "//beg_block//".")
                    call end_of_line(1_int64)
                    exit no_match
                end if
            else
                call message("Inserting "//beg_block//".")
                call insert(" "//beg_block, status)
                if (.not. status%ok()) return
            end if

            if (f90_equal_names(beg_name, end_name)) then
                if (allocated(end_name)) call search_fwd(end_name, status)
                if (.not. status%ok()) return
            else if (allocated(beg_name) .and. .not. allocated(end_name)) then
                if (f90_smart_end_names%value .or. .not. optional_name_block(beg_block)) then
                    call message("Inserting "//beg_name//".")
                    call insert(" "//beg_name, status)
                    if (.not. status%ok()) return
                end if
            else if (allocated(beg_name) .and. allocated(end_name)) then
                call message("Replacing "//end_name//" with "//beg_name//".")
                call search_fwd(end_name, status)
                if (status%ok()) call replace_match(beg_name, status)
                if (.not. status%ok()) return
            else if (allocated(end_name)) then
                call message("Deleting "//end_name//".")
                call search_fwd(end_name, status)
                if (status%ok()) call replace_match("", status)
                if (.not. status%ok()) return
            end if

            if (.not. looking("[ "//TAB//"]*!")) call delete_horizontal_space(.false., status)
        end block no_match
    end subroutine f90_block_match

    !> From an end block statement, find the corresponding block and name.
    !> Port of f90.el:f90-match-end.
    recursive subroutine f90_match_end(status)
        type(status_type), intent(out) :: status
        type(f90_block_type) :: end_struct, matching_beg
        integer(int64) :: count, top_of_window, end_point
        type(handle_type) :: opoint
        character(len=:), allocatable :: text, line
        logical :: saved, has_end, found

        status = status_type()
        count = 1
        top_of_window = 1
        if (selected_frame%selected_window%index > 0) top_of_window = window_start(windows(selected_frame%selected_window%index))
        end_point = point()
        call fold_case(saved)
        ! Check if in string in case using non-standard feature where
        ! continued strings do not need "&" at start of continuations.
        opoint = marker_at(point())
        call beginning_of_line(1_int64)
        call skip_forward(" "//TAB//"0-9")
        has_end = .false.

        if (.not. f90_in_string()) then
            end_struct = f90_looking_at_program_block_end()
            has_end = end_struct%found
        end if

        call goto_char(mpos(opoint))

        if (has_end) then
            call beginning_of_line(1_int64)

            do while (count > 0)
                if (line_beginning_position() == point_min()) exit
                found = search_re_backward_move(f90_blocks_re)
                call beginning_of_line(1_int64)
                ! GM not a line number if continued line.
                call skip_forward(" "//TAB//"0-9")

                if (f90_in_string() .or. f90_in_comment()) then
                    continue
                else
                    call match_beg(matching_beg, status)
                    if (.not. status%ok()) exit

                    if (matching_beg%found) then
                        count = count - 1
                    else if (looking("end[ "//TAB//"]*"//f90_blocks_re)) then
                        count = count + 1
                    end if
                end if
            end do

            if (status%ok()) then
                if (count > 0) then
                    call message("No matching beginning.")
                else
                    call f90_update_line(status)

                    if (status%ok() .and. f90_smart_end%value == F90_SMART_END_BLINK) then
                        if (point() < top_of_window) then
                            call what_line_text(line)
                            call buffer_substring(line_beginning_position(), line_end_position(), text, status)
                            call message("Matches "//line//": "//text)
                        else
                            call sit_for(int(blink_matching_delay*1000, int32))
                        end if
                    end if

                    if (status%ok()) then
                        call goto_char(end_point)
                        call beginning_of_line(1_int64)
                        call f90_block_match(matching_beg%kind, matching_beg%name, end_struct%kind, end_struct%name, &
                                             status)
                    end if
                end if
            end if
        end if

        call goto_char(mpos(opoint))
        call unmark(opoint)
        call restore_case(saved)

    contains

        recursive subroutine match_beg(b, st)
            type(f90_block_type), intent(out) :: b
            type(status_type), intent(out) :: st

            st = status_type()
            b = f90_looking_at_do()
            if (b%found) return
            call f90_looking_at_if_then(b, st)
            if (b%found .or. .not. st%ok()) return
            call f90_looking_at_where_or_forall(b, st)
            if (b%found .or. .not. st%ok()) return
            b = f90_looking_at_select_case()
            if (b%found) return
            b = f90_looking_at_type_like()
            if (b%found) return
            b = f90_looking_at_associate()
            if (b%found) return
            b = f90_looking_at_critical()
            if (b%found) return
            b = f90_looking_at_program_block_start()
            if (b%found) return
            ! Interpret a single END without a block
            ! start to be the END of a program block
            ! without an initial PROGRAM line.
            if (line_beginning_position() == point_min()) b = blk_of("program")
        end subroutine match_beg
    end subroutine f90_match_end

    !> From an end block statement, find the corresponding block and name
    !> (the command).  Port of f90.el:f90-match-end.
    recursive subroutine cmd_f90_match_end(status)
        type(status_type), intent(out) :: status

        call f90_match_end(status)
    end subroutine cmd_f90_match_end

    !> Insert a complete end statement matching beginning of present block.
    !> Port of f90.el:f90-insert-end.
    recursive subroutine f90_insert_end(status)
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        saved = f90_smart_end%value
        if (f90_smart_end%value == F90_SMART_END_NO) f90_smart_end%value = F90_SMART_END_BLINK
        call insert("end", status)
        if (status%ok()) call f90_indent_new_line(status)
        f90_smart_end%value = saved
    end subroutine f90_insert_end

    !> Insert a complete end statement matching beginning of present block
    !> (the command).  Port of f90.el:f90-insert-end.
    recursive subroutine cmd_f90_insert_end(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_insert_end(status)
    end subroutine cmd_f90_insert_end

    ! ------------------------------------------------------------------
    ! Abbrevs and keywords
    ! ------------------------------------------------------------------

    !> Typing `\[help_command] or `? lists all the F90 abbrevs.
    !> Any other key combination is executed normally.  Port of
    !> f90.el:f90-abbrev-start.
    recursive subroutine cmd_f90_abbrev_start(status)
        use lisp_fm, only: value_type
        type(status_type), intent(out) :: status
        type(handle_type) :: map
        type(value_type) :: key(1)
        type(status_type) :: st

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call self_insert_command(1_int64, int(iachar("`"), int64), status)
        if (.not. status%ok()) return

        if (associated(abbrev_mode)) then
            if (abbrev_mode%value) then
                call make_sparse_keymap(map)
                key(1) = value_type(int(iachar("?"), int64))
                call define_key(map, key, value_type(name="f90_abbrev_help"), st)
                key(1) = value_type(HELP_CHAR)
                call define_key(map, key, value_type(name="f90_abbrev_help"), st)
                call set_transient_map(map)
            end if
        end if
    end subroutine cmd_f90_abbrev_start

    !> List the currently defined abbrevs in F90 mode.  Port of
    !> f90.el:f90-abbrev-help.
    recursive subroutine cmd_f90_abbrev_help(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window

        call message("Listing abbrev table...")
        call f90_prepare_abbrev_list_buffer(buffer, status)
        if (status%ok()) call display_buffer(buffer, window, status)
        if (status%ok()) call message("Listing abbrev table...done")
    end subroutine cmd_f90_abbrev_help

    !> Create a buffer listing the F90 mode abbreviations.  Port of
    !> f90.el:f90-prepare-abbrev-list-buffer.
    recursive subroutine f90_prepare_abbrev_list_buffer(buffer, status)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call get_buffer_create("*Abbrevs*", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (status%ok()) call erase_buffer(status)
        if (status%ok()) call insert_abbrev_table_description(f90_mode_abbrev_table, status, .true.)

        if (status%ok()) then
            call goto_char(point_min())
            call restore_buffer_modified(.false.)
            call edit_abbrevs_mode(status)
        end if

        call set_buffer(old, st)
    end subroutine f90_prepare_abbrev_list_buffer

    !> Upcase all F90 keywords in the buffer.  Port of
    !> f90.el:f90-upcase-keywords.
    subroutine cmd_f90_upcase_keywords(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_change_keywords(F90_KEYWORD_CASE_UPCASE_WORD, status)
    end subroutine cmd_f90_upcase_keywords

    !> Capitalize all F90 keywords in the buffer.  Port of
    !> f90.el:f90-capitalize-keywords.
    subroutine cmd_f90_capitalize_keywords(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_change_keywords(F90_KEYWORD_CASE_CAPITALIZE_WORD, status)
    end subroutine cmd_f90_capitalize_keywords

    !> Downcase all F90 keywords in the buffer.  Port of
    !> f90.el:f90-downcase-keywords.
    subroutine cmd_f90_downcase_keywords(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call f90_change_keywords(F90_KEYWORD_CASE_DOWNCASE_WORD, status)
    end subroutine cmd_f90_downcase_keywords

    !> Change the case of the keywords in the region to CHANGE_WORD.
    !> Not a port.
    subroutine region_keywords(change_word, status)
        integer, intent(in) :: change_word
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call barf_if_buffer_read_only(status)
        if (status%ok()) call interactive_region(b, e, status)
        if (status%ok()) call f90_change_keywords(change_word, status, b, e)
    end subroutine region_keywords

    !> Upcase all F90 keywords in the region.  Port of
    !> f90.el:f90-upcase-region-keywords.
    subroutine cmd_f90_upcase_region_keywords(status)
        type(status_type), intent(out) :: status

        call region_keywords(F90_KEYWORD_CASE_UPCASE_WORD, status)
    end subroutine cmd_f90_upcase_region_keywords

    !> Capitalize all F90 keywords in the region.  Port of
    !> f90.el:f90-capitalize-region-keywords.
    subroutine cmd_f90_capitalize_region_keywords(status)
        type(status_type), intent(out) :: status

        call region_keywords(F90_KEYWORD_CASE_CAPITALIZE_WORD, status)
    end subroutine cmd_f90_capitalize_region_keywords

    !> Downcase all F90 keywords in the region.  Port of
    !> f90.el:f90-downcase-region-keywords.
    subroutine cmd_f90_downcase_region_keywords(status)
        type(status_type), intent(out) :: status

        call region_keywords(F90_KEYWORD_CASE_DOWNCASE_WORD, status)
    end subroutine cmd_f90_downcase_region_keywords

    !> Change the case of F90 keywords in the region (if specified) or buffer.
    !> CHANGE_WORD should be one of F90_KEYWORD_CASE_UPCASE_WORD,
    !> _DOWNCASE_WORD, _CAPITALIZE_WORD.  Port of
    !> f90.el:f90-change-keywords.
    subroutine f90_change_keywords(change_word, status, beg, end)
        integer, intent(in) :: change_word
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        character(len=:), allocatable :: keyword_re, saveword, now
        type(handle_type) :: opoint, endm
        type(parse_state_type) :: state
        integer(int64) :: b, ref_point, back_point, pt, found
        logical :: modified, skip
        type(status_type) :: st

        status = status_type()
        opoint = marker_at(point())
        b = point_min()
        if (present(beg)) b = beg

        if (present(end)) then
            endm = marker_at(end)
        else
            endm = marker_at(point_max())
        end if

        keyword_re = "\("//f90_keywords_re//"\|"//f90_procedures_re//"\|"//f90_hpf_keywords_re//"\|"// &
                     f90_operators_re//"\)"
        ref_point = point_min()
        call is_buffer_modified(current_buffer(), modified, st)
        call goto_char(b)

        do
            call re_search_forward(keyword_re, found, st, bound=mpos(endm), noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found <= 0) exit
            pt = point()
            call parse_partial_expression(ref_point, pt, state, st)
            call goto_char(pt)
            skip = state%instring /= -1 .or. state%incomment /= 0

            if (.not. skip) then
                ! GM f90-directive-comment-re?
                ! check for cpp directive
                call beginning_of_line(1_int64)
                call skip_forward(" "//TAB//"0-9")
                skip = looking("#")
                call goto_char(pt)
            end if

            if (skip) cycle
            ref_point = point()
            ! FIXME this does not work for constructs with
            ! embedded space, eg "sync all".
            call forward_word(-1_int64)
            back_point = point()
            call goto_char(ref_point)
            call buffer_substring(back_point, ref_point, saveword, st)

            select case (change_word)
            case (F90_KEYWORD_CASE_UPCASE_WORD)
                call upcase_word(-1_int64, status)
            case (F90_KEYWORD_CASE_DOWNCASE_WORD)
                call downcase_word(-1_int64, status)
            case (F90_KEYWORD_CASE_CAPITALIZE_WORD)
                call capitalize_word(-1_int64, status)
            case default
                continue
            end select

            if (.not. status%ok()) exit
            call buffer_substring(back_point, ref_point, now, st)
            if (now /= saveword) modified = .true.
        end do

        if (.not. modified) call restore_buffer_modified(.false.)
        call goto_char(mpos(opoint))
        call unmark(opoint)
        call unmark(endm)
    end subroutine f90_change_keywords

    ! ------------------------------------------------------------------
    ! Highlighting
    ! ------------------------------------------------------------------

    !> Search for the start/end of the definition of a derived type, up to LIMIT.
    !> Set the match data so that subexpression 1,2 are the TYPE, and
    !> type-name parts, respectively.  Port of f90.el:f90-typedef-matcher.
    subroutine f90_typedef_matcher(limit, found, status)
        use search_fm, only: match_data, set_match_data
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: l(:)
        logical :: ok

        status = status_type()
        found = .false.

        do
            if (.not. search_re("\_<\(\(?:end[ "//TAB//"]*\)?type\)\_>[ "//TAB//"]*", limit, SEARCH_NOERROR)) exit
            l = match_data()

            if (.not. looking("\(is\_>\|(\)")) then
                if (looking("\("//SYMRE//"+\)")) then
                    call goto_char(me(0))
                    ok = .true.
                else
                    ok = search_re("[ "//TAB//"]*::[ "//TAB//"]*\("//SYMRE//"+\)", line_end_position(), SEARCH_NOERROR)
                end if

                if (ok) then
                    ! 0 is wrong, but we don't use it.
                    call set_match_data([l, mb(1), me(1)])
                    found = .true.
                    exit
                end if
            end if
        end do
    end subroutine f90_typedef_matcher

    !> Search for the declaration of variables of derived type, up to LIMIT.
    !> Set the match data so that subexpression 1,2 are the TYPE(...),
    !> and variable-name parts, respectively.  Port of
    !> f90.el:f90-typedec-matcher.
    subroutine f90_typedec_matcher(limit, found, status)
        use search_fm, only: set_match_data
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: l(2), m0
        logical :: ok
        type(status_type) :: st

        ! Matcher functions must return nil only when there are no more
        ! matches within the search range.
        status = status_type()
        found = .false.

        do
            if (.not. search_re("\_<\(type\|class\)[ "//TAB//"]*(", limit, SEARCH_NOERROR)) exit
            m0 = mb(0)
            ! Set l after this to just highlight the "type" part.
            call forward_char(st, -1_int64)
            ! Needed for: type( foo(...) ) :: bar
            call forward_expression(1_int64, st)
            if (.not. st%ok()) cycle
            l = [m0, point()]
            call skip_forward(" "//TAB)

            if (looking(SYMRE//"+")) then
                ! type (foo) bar, qux
                ok = search_re("\("//NOT_CONT//"+\)", line_end_position(), SEARCH_NOERROR)
            else
                ! type (foo), stuff :: bar, qux
                ok = search_re("::[ "//TAB//"]*\("//NOT_CONT//"+\)", line_end_position(), SEARCH_NOERROR)
            end if

            if (ok) then
                call set_match_data([l(1), me(1), l(1), l(2), mb(1), me(1)])
                found = .true.
                exit
            end if
        end do
    end subroutine f90_typedec_matcher

    !> Set `font_lock_keywords' to F90 level N keywords.  Port of
    !> f90.el:f90-font-lock-n.
    subroutine f90_font_lock_n(n, status)
        use modes_fm, only: minor_modes
        use font_lock_fm, only: set_font_lock_keywords
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call minor_modes%font_lock_mode%enable(status)
        if (.not. status%ok()) return

        select case (n)
        case (1)
            call set_font_lock_keywords(f90_font_lock_keywords_1)
        case (2)
            call set_font_lock_keywords(f90_font_lock_keywords_2)
        case (3)
            call set_font_lock_keywords(f90_font_lock_keywords_3)
        case (4)
            call set_font_lock_keywords(f90_font_lock_keywords_4)
        case default
            continue
        end select

        call font_lock_flush(status)
    end subroutine f90_font_lock_n

    !> Set `font_lock_keywords' to `f90_font_lock_keywords_1'.  Port of
    !> f90.el:f90-font-lock-1.
    subroutine cmd_f90_font_lock_1(status)
        type(status_type), intent(out) :: status

        call f90_font_lock_n(1, status)
    end subroutine cmd_f90_font_lock_1

    !> Set `font_lock_keywords' to `f90_font_lock_keywords_2'.  Port of
    !> f90.el:f90-font-lock-2.
    subroutine cmd_f90_font_lock_2(status)
        type(status_type), intent(out) :: status

        call f90_font_lock_n(2, status)
    end subroutine cmd_f90_font_lock_2

    !> Set `font_lock_keywords' to `f90_font_lock_keywords_3'.  Port of
    !> f90.el:f90-font-lock-3.
    subroutine cmd_f90_font_lock_3(status)
        type(status_type), intent(out) :: status

        call f90_font_lock_n(3, status)
    end subroutine cmd_f90_font_lock_3

    !> Set `font_lock_keywords' to `f90_font_lock_keywords_4'.  Port of
    !> f90.el:f90-font-lock-4.
    subroutine cmd_f90_font_lock_4(status)
        type(status_type), intent(out) :: status

        call f90_font_lock_n(4, status)
    end subroutine cmd_f90_font_lock_4

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for editing Fortran 90,95 code in free format.
    !> For fixed format code, use `fortran_mode'.
    !>
    !> \[f90_indent_line] indents the current line.
    !> \[f90_indent_new_line] indents current line and creates a new indented line.
    !> \[f90_indent_subprogram] indents the current subprogram.
    !>
    !> Type `? or `\[help_command] to display a list of built-in abbrevs for F90 keywords.
    !>
    !> Key definitions:
    !> \{f90_mode_map}
    !>
    !> Variables controlling indentation style and extra features:
    !>
    !> `f90_do_indent'
    !>   Extra indentation within do blocks (default 3).
    !> `f90_if_indent'
    !>   Extra indentation within if/select/where/forall blocks (default 3).
    !> `f90_type_indent'
    !>   Extra indentation within type/enum/interface/block-data blocks (default 3).
    !> `f90_program_indent'
    !>   Extra indentation within program/module/subroutine/function blocks
    !>   (default 2).
    !> `f90_associate_indent'
    !>   Extra indentation within associate blocks (default 2).
    !> `f90_critical_indent'
    !>   Extra indentation within critical/block blocks (default 2).
    !> `f90_continuation_indent'
    !>   Extra indentation applied to continuation lines (default 5).
    !> `f90_comment_region'
    !>   String inserted by function \[f90_comment_region] at start of each
    !>   line in region (default "!!!$").
    !> `f90_indented_comment_re'
    !>   Regexp determining the type of comment to be intended like code
    !>   (default "!").
    !> `f90_directive_comment_re'
    !>   Regexp of comment-like directive like "!HPF\\$", not to be indented
    !>   (default "!hpf\\$").
    !> `f90_break_delimiters'
    !>   Regexp holding list of delimiters at which lines may be broken
    !>   (default "[-+*/><=,% \t]").
    !> `f90_break_before_delimiters'
    !>   True causes `f90_do_auto_fill' to break lines before delimiters
    !>   (default .true.).
    !> `f90_beginning_ampersand'
    !>   Automatic insertion of `&' at beginning of continuation lines (default .true.).
    !> `f90_smart_end'
    !>   From an END statement, check and fill the end using matching block start.
    !>   Allowed values are F90_SMART_END_BLINK, F90_SMART_END_NO_BLINK, and
    !>   F90_SMART_END_NO, which determine whether to blink the matching
    !>   beginning (default F90_SMART_END_BLINK).
    !> `f90_auto_keyword_case'
    !>   Automatic change of case of keywords (default F90_KEYWORD_CASE_NONE).
    !>   The possibilities are F90_KEYWORD_CASE_DOWNCASE_WORD, _UPCASE_WORD,
    !>   _CAPITALIZE_WORD.
    !> `f90_leave_line_no'
    !>   Do not left-justify line numbers (default .false.).
    !>
    !> Turning on F90 mode calls the value of the variable `f90_mode_hook'
    !> with no args, if that value is true.  Port of f90.el:f90-mode.
    recursive subroutine f90_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keywords_type) :: levels(5)
        logical :: saved

        if (.not. is_syntax_table_live(f90_mode_syntax_table)) call syms_of_f90()
        call major_modes%f90_mode%begin(saved, status)
        if (.not. status%ok()) return
        call indent_line_function%set_local(f90_indent_line_function)
        call indent_region_function%set_local(f90_indent_region)
        call comment_start%set_local("!")
        call comment_start_skip%set_local("!+ *")
        call comment_indent_function%set_local(f90_comment_indent_function)
        call abbrev_all_caps%set_local(.true.)
        call normal_auto_fill_function%set_local(f90_do_auto_fill)
        call indent_tabs_mode%set_local(.false.) ! auto buffer local
        call fill_paragraph_function%set_local(f90_fill_paragraph)
        call beginning_of_subprogram_function%set_local(f90_subprogram_start)
        call end_of_subprogram_function%set_local(f90_subprogram_end)
        levels(1) = font_lock_keywords_level(f90_font_lock_keywords)
        levels(2) = font_lock_keywords_level(f90_font_lock_keywords_1)
        levels(3) = font_lock_keywords_level(f90_font_lock_keywords_2)
        levels(4) = font_lock_keywords_level(f90_font_lock_keywords_3)
        levels(5) = font_lock_keywords_level(f90_font_lock_keywords_4)
        call font_lock_defaults%set_local(levels=levels, keywords_only=.false., case_fold=.true.)
        call major_modes%f90_mode%finish(saved, status)
    end subroutine f90_mode
end module f90_fm
