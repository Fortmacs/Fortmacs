! python_fm.f90 -- Python's flying circus support for Fortmacs.
! Ported from GNU Emacs 31.1 lisp/progmodes/python.el.
!
! Copyright (C) 2003-2026 Free Software Foundation, Inc.
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

!> Python mode: `python_base_mode' and `python_mode', with the syntax
!> table and the triple-quote syntax properties, the three levels of
!> keywords (f-string holes, assignments, operators, escape sequences),
!> the indentation engine (`python_indent_context', TAB cycling through
!> the possible levels, the dedenters, `python_indent_region', the shift
!> commands, the electric colon and parens), statement, block, subprogram and
!> expression motion (`python_nav_*', used by C-M-a, C-M-e, C-M-f, C-M-u and the
!> sentence keys), `python_mark_definition', filling of comments, strings and
!> parens, and the `python_info_*' and `python_util_*' helpers.
!>
!> The regexps python.el builds with its `python_rx' macro are written
!> out as constants.  A syntax context (`python_syntax_context') is a
!> position, 0 for none; `python_syntax_context_type' gives one of the
!> PYTHON_CONTEXT_* constants.  Docstring styles are the
!> PYTHON_DOCSTRING_* constants.
!>
!> Deferred: the shell integration (`run_python', the `python_shell_*'
!> commands and variables, `inferior_python_mode', shell completion,
!> PDB tracking, `python_interpreter' and `python_interpreter_args'),
!> symbol completion (`python_completion_at_point'), the skeletons
!> (skeleton.el), FFAP, `python_check', ElDoc, Hideshow, Imenu and
!> `which_function', Outline mode's variables, `prettify_symbols_table',
!> Flymake, the import management commands (C-c C-i), tree-sitter
!> (`python_ts_mode'), `add_log_current_subprogram_function'
!> (`python_info_current_definition' is here), the menu,
!> `python_util_clone_local_variables' and the other utilities the shell
!> uses, the `delete_selection' property of
!> `python_indent_dedent_line_backspace', and the obsolete
!> `python_indent_current_level' and `python_indent_levels'.
module python_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_USER_ERROR, FM_SCAN_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, xfixnum, list, make_string, list_elements, vtag, V_INTEGER
    use lread_fm, only: sym
    use data_fm, only: integer_setting, logical_setting, string_setting, forward_setting, value_eq, &
                       add_local_variables_killer
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command, last_command, real_this_command, last_command_event
    use buffer_fm, only: buffers, cur, buffer_serial_live, current_buffer, buffer_chars_modified_tick, &
                         transient_mark_mode
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes, minor_modes
    use keymap_fm, only: keymap_set, define_key, make_sparse_keymap, set_repeat_map
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, is_syntax_table_live, syntax_ppss, parse_state_type, &
                         forward_comment, scan_groups, skip_chars_forward, skip_chars_backward, skip_syntax_forward, &
                         skip_syntax_backward, syntax_entry, standard_syntax_table, string_to_syntax, &
                         syntax_descriptor_type, syntax_entry_at, char_syntax, parse_expression_ignore_comments, &
                         parse_expression_lookup_properties, scan_error_data, ST_STRING_STYLE, syntax_table, set_syntax_table
    use editfns_fm, only: point, point_min, point_max, goto_char, is_bol, is_eol, is_bob, is_eob, char_after, char_before, &
                          buffer_substring, line_beginning_position, line_end_position, delete_region, narrow_to_region, &
                          widen, region_beginning, region_end, preceding_char, following_char, mark_position
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line, post_self_insert_hook
    use insdel_fm, only: insert
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use indent_fm, only: current_column, current_indentation, indent_line_to, indent_line_function, &
                         indent_region_function, indent_rigidly, indent_according_to_mode, indent_tabs_mode
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_backward, string_match, &
                         match_beginning, match_end, match_data, set_match_data, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string_no_properties, looking_back
    use simple_fm, only: back_to_indentation, line_number_at_pos, is_use_region, backward_delete_char_untabify, newline, &
                         count_lines
    use textprop_fm, only: put_text_property
    use xdisp_fm, only: message, tab_width
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, font_lock_keywords_type, &
                            font_lock_keywords_level, FONT_LOCK_OVERRIDE_YES, FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use expression_motion_fm, only: forward_expression, forward_expression_function, forward_expression_default_function, &
                                    up_group, mark_subprogram, &
                                    beginning_of_subprogram_function, end_of_subprogram_function
    use newcomment_fm, only: comment_start, comment_start_skip
    use paragraphs_fm, only: paragraph_start, paragraph_separate
    use fill_fm, only: fill_paragraph, fill_paragraph_function, fill_comment_paragraph, fill_indent_according_to_mode
    use simple_fm, only: normal_auto_fill_function, do_auto_fill
    use electric_fm, only: electric_indent_inhibit, electric_indent_chars_value, set_local_electric_indent_chars
    use prog_mode_fm, only: prog_first_column
    use syntax_propertize_fm, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type
    use grep_fm, only: grep_files_aliases, grep_files_alias_type, grep_find_ignored_directories, grep_ignore_type
    implicit none(type, external)
    private

    public :: syms_of_python, python_base_mode, python_mode, python_syntax_context, python_syntax_context_type, &
              is_python_syntax_comment_or_string, is_python_syntax_closing_paren, python_font_lock_syntactic_face_function, &
              python_syntax_count_quotes, python_syntax_stringify, python_indent_guess_indent_offset, &
              python_indent_context, python_indent_calculate_indentation, python_indent_calculate_levels, &
              python_indent_line, python_indent_line_function, python_indent_dedent_line, &
              python_indent_dedent_line_backspace, python_indent_region, python_indent_shift_left, &
              python_indent_shift_right, python_indent_post_self_insert_function, python_mark_definition, &
  python_nav_beginning_of_definition, python_nav_end_of_definition, python_nav_backward_definition, python_nav_forward_definition, &
              python_nav_beginning_of_statement, python_nav_end_of_statement, python_nav_backward_statement, &
              python_nav_forward_statement, python_nav_beginning_of_block, python_nav_end_of_block, &
              python_nav_backward_block, python_nav_forward_block, python_nav_forward_expression, python_nav_backward_expression, &
       python_nav_forward_expression_safe, python_nav_backward_expression_safe, python_nav_up_group, python_nav_backward_up_group, &
              python_nav_if_name_main, python_info_current_definition, is_python_info_statement_starts_block, &
              is_python_info_statement_ends_block, is_python_info_beginning_of_statement, is_python_info_end_of_statement, &
              is_python_info_beginning_of_block, is_python_info_end_of_block, python_info_dedenter_opening_block_position, &
              python_info_dedenter_opening_block_positions, python_info_dedenter_opening_block_message, &
              is_python_info_dedenter_statement, is_python_info_line_ends_backslash, python_info_beginning_of_backslash, &
              is_python_info_continuation_line, is_python_info_block_continuation_line, is_python_info_assignment_statement, &
              is_python_info_assignment_continuation_line, python_info_looking_at_beginning_of_definition, &
              python_info_looking_at_beginning_of_block, is_python_info_current_line_comment, &
              is_python_info_current_line_empty, is_python_info_docstring, is_python_info_triple_quoted_string, &
              python_info_encoding_from_cookie, python_info_encoding, python_util_goto_line, python_util_forward_comment, &
              python_fill_paragraph, python_fill_comment, python_fill_string, python_fill_decorator, python_fill_paren, &
              python_do_auto_fill, python_electric_pair_string_delimiter, python_fill_proc

    !> The syntax contexts of `python_syntax_context' and
    !> `python_syntax_context_type' (0 is none).
    integer, parameter, public :: PYTHON_CONTEXT_COMMENT = 1, PYTHON_CONTEXT_STRING = 2, &
                                  PYTHON_CONTEXT_SINGLE_QUOTED_STRING = 3, PYTHON_CONTEXT_TRIPLE_QUOTED_STRING = 4, &
                                  PYTHON_CONTEXT_PAREN = 5

    !> The values of `python_fill_docstring_style'.
    integer, parameter, public :: PYTHON_DOCSTRING_NONE = 0, PYTHON_DOCSTRING_DJANGO = 1, PYTHON_DOCSTRING_ONETWO = 2, &
                                  PYTHON_DOCSTRING_PEP_257 = 3, PYTHON_DOCSTRING_PEP_257_NN = 4, &
                                  PYTHON_DOCSTRING_SYMMETRIC = 5

    !> The indentation contexts of `python_indent_context' (python.el's
    !> keywords :no-indent and the others).
    integer, parameter, public :: PYTHON_INDENT_NO_INDENT = 1, PYTHON_INDENT_AFTER_COMMENT = 2, &
                                  PYTHON_INDENT_INSIDE_STRING = 3, PYTHON_INDENT_INSIDE_DOCSTRING = 4, &
                                  PYTHON_INDENT_INSIDE_PAREN = 5, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN = 6, &
                                  PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN = 7, &
                                  PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START = 8, &
                                  PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK = 9, &
                                  PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK = 10, &
                                  PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE = 11, PYTHON_INDENT_AFTER_BACKSLASH = 12, &
                                  PYTHON_INDENT_AFTER_BACKSLASH_ASSIGNMENT_CONTINUATION = 13, &
                                  PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION = 14, &
                                  PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION = 15, &
                                  PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE = 16, PYTHON_INDENT_AFTER_BLOCK_END = 17, &
                                  PYTHON_INDENT_AFTER_BLOCK_START = 18, PYTHON_INDENT_AFTER_LINE = 19, &
                                  PYTHON_INDENT_AT_DEDENTER_BLOCK_START = 20

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> If true, enable Python 2 support.
    !> Currently only affects highlighting.
    !>
    !> After customizing this variable, you must restart Emacs for it to take
    !> effect.  Port of python.el python-2-support.
    type(logical_setting), target, public :: python_2_support = logical_setting(name="python_2_support", value=.false.)

    !> Default indentation offset for Python.  Port of python.el
    !> python-indent-offset.
    type(integer_setting), target, public :: python_indent_offset = integer_setting(name="python_indent_offset", value=4)

    !> True tells Python mode to guess `python_indent_offset' value.  Port of
    !> python.el python-indent-guess-indent-offset.
    type(logical_setting), target, public :: python_indent_guess_indent_offset_variable = &
                                             logical_setting(name="python_indent_guess_indent_offset_variable", &
                                                             value=.true.)

    !> True means to emit a warning when indentation guessing fails.
    !> Port of python.el python-indent-guess-indent-offset-verbose.
    type(logical_setting), target, public :: python_indent_guess_indent_offset_verbose = &
                                             logical_setting(name="python_indent_guess_indent_offset_verbose", &
                                                             value=.true.)

    !> Commands that might trigger a `python_indent_line' call.  Port of
    !> python.el python-indent-trigger-commands.
    character(len=32), allocatable, public :: python_indent_trigger_commands(:)

    !> Multiplier applied to indentation inside multi-line blocks.
    !> The indentation in parens in the block header will be the current
    !> indentation plus `python_indent_offset' multiplied by this
    !> variable.  For example, the arguments are indented as follows if
    !> this variable is 1:
    !>
    !>     def do_something(
    !>         arg1,
    !>         arg2):
    !>         print('hello')
    !>
    !> if this variable is 2 (default):
    !>
    !>     def do_something(
    !>             arg1,
    !>             arg2):
    !>         print('hello')
    !>
    !> This variable has an effect on all blocks, not just def block.
    !> This variable only works if the opening paren is not followed by
    !> non-whitespace characters on the same line.  Modify
    !> `python_indent_block_paren_deeper' to customize the case where
    !> non-whitespace characters follow the opening paren on the same
    !> line.  Port of python.el python-indent-def-block-scale.
    type(integer_setting), target, public :: python_indent_def_block_scale = &
                                             integer_setting(name="python_indent_def_block_scale", value=2)

    !> Increase indentation inside parens of a block.
    !> If true, increase the indentation of the lines inside parens
    !> in a header of a block when they are indented to the same level
    !> as the body of the block:
    !>
    !>     if (some_expression
    !>             and another_expression):
    !>         do_something()
    !>
    !> instead of:
    !>
    !>     if (some_expression
    !>         and another_expression):
    !>         do_something()
    !>
    !> This variable only works if the opening paren is followed by
    !> non-whitespace characters on the same line.  Modify
    !> `python_indent_def_block_scale' to customize the case where
    !> non-whitespace character does not follow the opening paren on the
    !> same line.  Port of python.el python-indent-block-paren-deeper.
    type(logical_setting), target, public :: python_indent_block_paren_deeper = &
                                             logical_setting(name="python_indent_block_paren_deeper", value=.false.)

    !> Function to use when navigating between expressions.  Port of
    !> python.el python-forward-sexp-function (null for the default, "CC-mode
    !> like").
    procedure(python_forward_expression_proc), pointer, public :: python_forward_expression_function => null()

    !> Style used to fill docstrings.
    !> This affects `python_fill_string' behavior with regards to
    !> triple quotes positioning.
    !>
    !> Possible values are PYTHON_DOCSTRING_DJANGO, _ONETWO, _PEP_257,
    !> _PEP_257_NN, _SYMMETRIC, and PYTHON_DOCSTRING_NONE.  A value of
    !> PYTHON_DOCSTRING_NONE won't care about quotes position and will treat
    !> docstrings a normal string, any other value may result in one of
    !> the following docstring styles:
    !>
    !> PYTHON_DOCSTRING_DJANGO:
    !>
    !>     """
    !>     Process foo, return bar.
    !>     """
    !>
    !>     """
    !>     Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>     """
    !>
    !> PYTHON_DOCSTRING_ONETWO:
    !>
    !>     """Process foo, return bar."""
    !>
    !>     """
    !>     Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>
    !>     """
    !>
    !> PYTHON_DOCSTRING_PEP_257:
    !>
    !>     """Process foo, return bar."""
    !>
    !>     """Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>
    !>     """
    !>
    !> PYTHON_DOCSTRING_PEP_257_NN:
    !>
    !>     """Process foo, return bar."""
    !>
    !>     """Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>     """
    !>
    !> PYTHON_DOCSTRING_SYMMETRIC:
    !>
    !>     """Process foo, return bar."""
    !>
    !>     """
    !>     Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>     """
    !>
    !> Port of python.el python-fill-docstring-style.
    type(integer_setting), target, public :: python_fill_docstring_style = &
                                             integer_setting(name="python_fill_docstring_style", &
                                                             value=PYTHON_DOCSTRING_PEP_257)

    abstract interface
        !> A function of `python_forward_expression_function': move across ARG
        !> expressions.
        recursive subroutine python_forward_expression_proc(arg, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine python_forward_expression_proc

        !> A fill function of `python_fill_paragraph': DONE is its value
        !> (JUSTIFY absent for no justification).
        recursive subroutine python_fill_proc(status, done, justify)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: done
            integer, intent(in), optional :: justify
        end subroutine python_fill_proc
    end interface

    !> Function to fill comments.
    !> This is the function used by `python_fill_paragraph' to
    !> fill comments.  Port of python.el python-fill-comment-function.
    procedure(python_fill_proc), pointer, public :: python_fill_comment_function => null()
    !> Function to fill strings.
    !> This is the function used by `python_fill_paragraph' to
    !> fill strings.  Port of python.el python-fill-string-function.
    procedure(python_fill_proc), pointer, public :: python_fill_string_function => null()
    !> Function to fill decorators.
    !> This is the function used by `python_fill_paragraph' to
    !> fill decorators.  Port of python.el python-fill-decorator-function.
    procedure(python_fill_proc), pointer, public :: python_fill_decorator_function => null()
    !> Function to fill parens.
    !> This is the function used by `python_fill_paragraph' to
    !> fill parens.  Port of python.el python-fill-paren-function.
    procedure(python_fill_proc), pointer, public :: python_fill_paren_function => null()

    !> Regexp matching class or function definition.
    !> The name of the subprogram should be grouped so it can be retrieved
    !> via `match_string'.  Port of python.el
    !> python-nav-beginning-of-defun-regexp.
    character(len=:), allocatable, public :: python_nav_beginning_of_definition_regexp
    !> Regexp matching block start.  Port of python.el
    !> python-nav-beginning-of-block-regexp.
    character(len=:), allocatable, public :: python_nav_beginning_of_block_regexp

    !> Font lock keywords to use in `python_mode' for level 1 decoration.
    !>
    !> This is the minimum decoration level, including function and
    !> class declarations.  Port of python.el
    !> python-font-lock-keywords-level-1.
    type(font_lock_keyword_type), allocatable, public :: python_font_lock_keywords_level_1(:)
    !> Font lock keywords to use in `python_mode' for level 2 decoration.
    !>
    !> This is the medium decoration level, including everything in
    !> `python_font_lock_keywords_level_1', as well as keywords and
    !> builtins.  Port of python.el python-font-lock-keywords-level-2.
    type(font_lock_keyword_type), allocatable, public :: python_font_lock_keywords_level_2(:)
    !> Font lock keywords to use in `python_mode' for maximum decoration.
    !>
    !> This decoration level includes everything in
    !> `python_font_lock_keywords_level_2', as well as constants,
    !> decorators, exceptions, and assignments.  Port of python.el
    !> python-font-lock-keywords-maximum-decoration.
    type(font_lock_keyword_type), allocatable, public :: python_font_lock_keywords_maximum_decoration(:)

    !> Keymap for `python_base_mode'.  Port of python.el python-base-mode-map.
    type(handle_type), public :: python_base_mode_map
    !> Keymap for `python_mode'.  Port of python.el python-mode-map.
    type(handle_type), public :: python_mode_map
    !> Keymap to repeat Python indentation commands.
    !> Used in `repeat_mode'.  Port of python.el python-indent-repeat-map.
    type(handle_type), public :: python_indent_repeat_map

    !> Syntax table for Python files.  Port of python.el
    !> python-mode-syntax-table.
    type(handle_type), public :: python_mode_syntax_table
    !> Dotty syntax table for Python files.
    !> It makes underscores and dots word constituent chars.  Port of
    !> python.el python-dotty-syntax-table.
    type(handle_type), public :: python_dotty_syntax_table

    !> Normal hook run by `python_base_mode'.
    type(hook_type), target, public :: python_base_mode_hook = hook_type(name="python_base_mode_hook")
    !> Normal hook run by `python_mode'.
    type(hook_type), target, public :: python_mode_hook = hook_type(name="python_mode_hook")

    ! The rules of python-syntax-propertize-function.
    type(syntax_propertize_rules_type) :: propertize_rules

    ! Whether the grep settings were added (python--installed-grep-hook).
    logical :: installed_grep_hook = .false.

    ! One buffer's navigation cache (python-nav-cache and
    ! python-nav-cache-tick): KIND (1 beginning of block, 2 end of block)
    ! and the line give the target position.
    type :: nav_cache_entry_type
        integer :: kind = 0
        integer(int64) :: line = 0, target = 0
    end type nav_cache_entry_type

    type :: nav_cache_type
        integer(int64) :: serial = 0
        integer(int64) :: tick = 0
        type(nav_cache_entry_type), allocatable :: entries(:)
    end type nav_cache_type

    type(nav_cache_type), allocatable :: nav_caches(:)

    abstract interface
        ! A motion of `nav_syntactically', true if it moved.
        recursive logical function nav_motion()
            implicit none(type, external)
        end function nav_motion
    end interface

    integer, parameter :: NAV_BEGINNING_OF_BLOCK = 1, NAV_END_OF_BLOCK = 2

    ! A string of a list.
    type :: string_list_type
        character(len=:), allocatable :: s
    end type string_list_type

    character(len=*), parameter :: DEF_RE = &
                                   "\_<def[[:space:]]+\([_[:alpha:]][_[:word:]]*\)"
    character(len=*), parameter :: CLASS_RE = &
                                   "\_<class[[:space:]]+\([_[:alpha:]][_[:word:]]*\)"
    character(len=*), parameter :: KEYWORDS2_RE = &
                                   "\_<\(?:and\|del\|from\|not\|while\|as\|elif\|global\|or\|with\|assert\" &
                                   //"|else\|if\|pass\|yield\|break\|except\|import\|class\|in\|raise\|conti" &
                                   //"nue\|finally\|is\|return\|def\|for\|lambda\|try\|nonlocal\|async[[:spa" &
                                   //"ce:]]+\(?:def\|for\|with\)\|await\|match\|case\|self\)\_>"
    character(len=*), parameter :: BUILTINS_RE = &
                                   "\(?:\_<\(?:__\(?:\(?:a\(?:ll\|nnotations\)\|bases\|c\(?:\(?:losur\|od\" &
                                   //")e\)\|d\(?:efaults\|ict\|oc\)\|firstlineno\|globals\|import\|kwdefault" &
                                   //"s\|m\(?:odule\|ro\)\|name\|package\|qualname\|\(?:static_attribute\|ty" &
                                   //"pe_param\)s\)__\)\|a\(?:bs\|iter\|ll\|n\(?:ext\|y\)\|scii\)\|b\(?:in\|" &
                                   //"ool\|reakpoint\|yte\(?:array\|s\)\)\|c\(?:allable\|hr\|lassmethod\|omp" &
                                   //"\(?:ile\|lex\)\)\|d\(?:elattr\|i\(?:ct\|r\|vmod\)\)\|e\(?:numerate\|va" &
                                   //"l\|xec\)\|f\(?:ilter\|\(?:loa\|orma\|rozense\)t\)\|g\(?:etattr\|lobals" &
                                   //"\)\|h\(?:as\(?:attr\|h\)\|e\(?:lp\|x\)\)\|i\(?:d\|n\(?:\(?:pu\)?t\)\|s" &
                                   //"\(?:instance\|subclass\)\|ter\)\|l\(?:en\|ist\|ocals\)\|m\(?:a[px]\|em" &
                                   //"oryview\|in\)\|next\|o\(?:bject\|ct\|pen\|rd\)\|p\(?:ow\|r\(?:int\|ope" &
                                   //"rty\)\)\|r\(?:ange\|e\(?:pr\|versed\)\|ound\)\|s\(?:et\(?:attr\)?\|lic" &
                                   //"e\|orted\|t\(?:aticmethod\|r\)\|u\(?:m\|per\)\)\|t\(?:\(?:upl\|yp\)e\)" &
                                   //"\|vars\|zip\)\_>\)"
    character(len=*), parameter :: CONSTANTS_RE = &
                                   "\_<\(?:Ellipsis\|False\|No\(?:ne\|tImplemented\)\|True\|__debug__\|c\(" &
                                   //"?:opyright\|redits\)\|exit\|license\|quit\)\_>"
    character(len=*), parameter :: DECORATOR_RE = &
                                   "^["//TAB//" ]*\(@\(?:[[:word:]]\|_\)+\(?:\.\(?:[[:word:]]\|_\)+\)*\)"
    character(len=*), parameter :: EXCEPTIONS_RE = &
                                   "\(?:\_<\(?:A\(?:\(?:rithmetic\|ssertion\|ttribute\)Error\)\|B\(?:aseEx" &
                                   //"ception\(?:Group\)?\|lockingIOError\|rokenPipeError\|ufferError\|ytesW" &
                                   //"arning\)\|C\(?:\(?:hildProcess\|onnection\(?:Aborted\|Re\(?:fused\|set" &
                                   //"\)\)?\)Error\)\|DeprecationWarning\|E\(?:OFError\|n\(?:codingWarning\|" &
                                   //"vironmentError\)\|xception\(?:Group\)?\)\|F\(?:ile\(?:\(?:Exists\|NotF" &
                                   //"ound\)Error\)\|loatingPointError\|utureWarning\)\|GeneratorExit\|I\(?:" &
                                   //"OError\|mport\(?:Error\|Warning\)\|\(?:n\(?:de\(?:ntation\|x\)\|terrup" &
                                   //"ted\)\|sADirectory\)Error\)\|Key\(?:Error\|boardInterrupt\)\|LookupErr" &
                                   //"or\|M\(?:\(?:emory\|oduleNotFound\)Error\)\|N\(?:\(?:ame\|ot\(?:ADirec" &
                                   //"tory\|Implemented\)\)Error\)\|O\(?:\(?:S\|verflow\)Error\)\|P\(?:e\(?:" &
                                   //"ndingDeprecationWarning\|rmissionError\)\|\(?:rocessLookup\|ythonFinal" &
                                   //"ization\)Error\)\|R\(?:e\(?:cursionError\|ferenceError\|sourceWarning\" &
                                   //")\|untime\(?:Error\|Warning\)\)\|S\(?:top\(?:\(?:Async\)?Iteration\)\|" &
                                   //"y\(?:ntax\(?:Error\|Warning\)\|stemE\(?:rror\|xit\)\)\)\|T\(?:\(?:ab\|" &
                                   //"imeout\|ype\)Error\)\|U\(?:n\(?:boundLocalError\|icode\(?:DecodeError\" &
                                   //"|E\(?:\(?:ncodeE\)?rror\)\|TranslateError\|Warning\)\)\|serWarning\)\|" &
                                   //"V\(?:\(?:MS\|alue\)Error\)\|W\(?:arning\|indowsError\)\|ZeroDivisionEr" &
                                   //"ror\)\_>\)"
    character(len=*), parameter :: ASSIGN1_RE = &
                                   "\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*\([_[:alpha:]][_[:word:]]*\)\(?:\[" &
                                   //"[^]]+]\)?[[:space:]]*\(?::[[:space:]]*\([^"//LF//"%&*+/<->^|~-]+\)[[:space:]]" &
                                   //"*\)?\(\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\)"
    character(len=*), parameter :: SYMBOL_NAME_RE = &
                                   "[_[:alpha:]][_[:word:]]*"
    character(len=*), parameter :: ASSIGN2_RE = &
                                   "\(?:[([][[:space:]]*\)?\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*\([_[:alpha" &
                                   //":]][_[:word:]]*\)\(?:\[[^]]+]\)?[[:space:]]*,[[:space:]]*\(?:\*?\(?:[_" &
                                   //"[:alpha:]][_[:word:]]*\.\)*[_[:alpha:]][_[:word:]]*\(?:\[[^]]+]\)?[[:s" &
                                   //"pace:]]*,[[:space:]]*\)*\(?:\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*[_[:al" &
                                   //"pha:]][_[:word:]]*\(?:\[[^]]+]\)?[[:space:]]*\)?\(?:,[[:space:]]*\)?\(" &
                                   //"?:[])][[:space:]]*\)?\(\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\)"
    character(len=*), parameter :: GROUPED_TARGET_RE = &
                                   "\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*\([_[:alpha:]][_[:word:]]*\)\(?:\[" &
                                   //"[^]]+]\)?"
    character(len=*), parameter :: ASSIGN3_RE = &
                                   "\(?:^\|;\|=\)[[:space:]]*[([][[:space:]]*\*?\(?:[_[:alpha:]][_[:word:]" &
                                   //"]*\.\)*\([_[:alpha:]][_[:word:]]*\)\(?:\[[^]]+]\)?[[:space:]]*[])][[:s" &
                                   //"pace:]]*\(\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\)"
    character(len=*), parameter :: OPERATOR_RE = &
                                   "\(?:!=\|\*\*\|//\|<[<=]\|==\|>[=>]\|[%&*+/<=>|~^-]\)"
    character(len=*), parameter :: BYTES_ESCAPE_RE = &
                                   "[^\]\(\\\\\|\\'\|\\a\|\\b\|\\f\|\\n\|\\r\|\\t\|\\v\|\\[0-7]\{1,3\}\|\\" &
                                   //"x[[:xdigit:]][[:xdigit:]]\)"
    character(len=*), parameter :: STRING_ESCAPE_RE = &
                                   "[^\]\(\\\\\|\\'\|\\a\|\\b\|\\f\|\\n\|\\r\|\\t\|\\v\|\\[0-7]\{1,3\}\|\\" &
                                   //"x[[:xdigit:]][[:xdigit:]]\)\|[^\]\(?:\(?1:\\u[[:xdigit:]]\{4\}\)\|\(?1" &
                                   //":\\U[[:xdigit:]]\{8\}\)\|\(?1:\\N{[^z-a]*?}\)\)"
    character(len=*), parameter :: NOT_RAW_BYTES_START_RE = &
                                   "\(?:\`\|[^[:alnum:]]\)[Bb]\(?:""""""\|'''\|[""']\)\'"
    character(len=*), parameter :: NOT_RAW_STRING_START_RE = &
                                   "\`\(?:\(?:[^[:alnum:]]?[FUfu]\)?\(?:""""""\|'''\)\|[^z-a]?[^[:alnum:]]\(?" &
                                   //":""""""\|'''\)\|\(?:[^z-a]\{0,2\}[^[:alnum:]]\)?[FUfu][""']\|\(?:[^z-a]\{0" &
                                   //",3\}[^""'[:alnum:]]\)?[""']\)\'"
    character(len=*), parameter :: F_STRING_START_RE = &
                                   "\<\(?:F[Rr]\|R[Ff]\|f[Rr]\|r[Ff]\|[Ff]\)\(?:""""""\|'''\|[""']\)"
    character(len=*), parameter :: TRIPLE_QUOTES_RE = &
                                   "\(?:""""""\|'''\)"
    character(len=*), parameter :: BEGINNING_OF_SUBPROGRAM_RE = &
                                   "^[[:space:]]*\_<\(?:def\|class\|async[[:space:]]+def\)\_>\(?:[[:space:" &
                                   //"]]\|\\"//LF//"\)+\([_[:alpha:]][_[:word:]]*\)"
    character(len=*), parameter :: BEGINNING_OF_BLOCK_RE = &
                                   "^[[:space:]]*\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|" &
                                   //"for\|while\|with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)" &
                                   //"\_>"
    character(len=*), parameter :: LINE_START_BLOCK_START_RE = &
                                   "^\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|" &
                                   //"with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>"
    character(len=*), parameter :: BLOCK_START_RE = &
                                   "\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|w" &
                                   //"ith\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>"
    character(len=*), parameter :: BLOCK_START_SPACE_RE = &
                                   "\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|w" &
                                   //"ith\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>[[:space:]" &
                                   //"]*"
    character(len=*), parameter :: BLOCK_ENDER_RE = &
                                   "\_<\(?:\(?:break\|continue\|pass\|r\(?:aise\|eturn\)\)\_>\|\(?:\(?:os\" &
                                   //"._?\|sys\.\)?exit\|quit\)[[:space:]]*(\)"
    character(len=*), parameter :: DEDENTER_RE = &
                                   "\_<\(?:case\|e\(?:l\(?:if\|se\)\|xcept\)\|finally\)\_>"
    character(len=*), parameter :: INDENT_REGION_SKIP_RE = &
                                   "\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|w" &
                                   //"ith\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>\|\_<\(?:c" &
                                   //"ase\|e\(?:l\(?:if\|se\)\|xcept\)\|finally\)\_>\|\_<\(?:\(?:break\|cont" &
                                   //"inue\|pass\|r\(?:aise\|eturn\)\)\_>\|\(?:\(?:os\._?\|sys\.\)?exit\|qui" &
                                   //"t\)[[:space:]]*(\)"
    character(len=*), parameter :: FORWARD_BLOCK_RE = &
                                   "^[[:space:]]*\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|" &
                                   //"for\|while\|with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)" &
                                   //"\_>"
    character(len=*), parameter :: CLOSE_PAREN_RE = &
                                   "[])}]"
    character(len=*), parameter :: OPEN_PAREN_RE = &
                                   "[([{]"
    character(len=*), parameter :: IF_NAME_MAIN_RE = &
                                   "^if[[:space:]]+__name__[[:space:]]+==[[:space:]]+\(?1:[""']\)__main__\1" &
                                   //"[[:space:]]*:"
    character(len=*), parameter :: LINE_START_WS_RE = &
                                   "^[[:space:]]*"
    character(len=*), parameter :: ASSIGNMENT_STATEMENT_RE = &
                                   "[^"//LF//"%&*+/<->^|~-]\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\([^"//LF//"%&*+/<-" &
                                   //">^|~-]\)"
    character(len=*), parameter :: EMPTY_LINE_RE = &
                                   "^[[:space:]]*\(.*\)[[:space:]]*$"
    character(len=*), parameter :: CODING_COOKIE_RE = &
                                   "^#[[:space:]]*\(?:coding[:=][[:space:]]*\(?1:\(?:[[:word:]]\|-\)+\)\|-" &
                                   //"\*-[[:space:]]*coding:[[:space:]]*\(?1:\(?:[[:word:]]\|-\)+\)[[:space:" &
                                   //"]]*-\*-\|vim:[[:space:]]*set[[:space:]]+fileencoding[[:space:]]*=[[:sp" &
                                   //"ace:]]*\(?1:\(?:[[:word:]]\|-\)+\)[[:space:]]*:\)"
    character(len=*), parameter :: OUTLINE_RE = &
                                   "[[:space:]]*\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|f" &
                                   //"or\|while\|with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\" &
                                   //"_>"
    character(len=*), parameter :: SELF_RE = &
                                   "^\<self\>\."
    character(len=*), parameter :: STRING_DELIM_SYNTAX_RE = &
                                   "\s|"
    character(len=*), parameter :: AUTO_MODE_RE = &
                                   "\(?:\.\(?:p\(?:th\|y[iw]?\)\)\|/\(?:SCons\(?:\(?:crip\|truc\)t\)\)\)\'"
    character(len=*), parameter :: WALRUS_RE = &
                                   "[^[:space:]]:$"

contains

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the Python modes, their keymaps, syntax tables and keywords.
    !> Not a port.
    recursive subroutine syms_of_python()
        type(status_type) :: st

        if (is_syntax_table_live(python_mode_syntax_table)) return
        call forward_setting(python_2_support)
        call forward_setting(python_indent_offset)
        call forward_setting(python_indent_guess_indent_offset_variable)
        call forward_setting(python_indent_guess_indent_offset_verbose)
        call forward_setting(python_indent_def_block_scale)
        call forward_setting(python_indent_block_paren_deeper)
        call forward_setting(python_fill_docstring_style)

        if (.not. allocated(python_indent_trigger_commands)) then
            python_indent_trigger_commands = [character(len=32) :: "indent_for_tab_command", "yas_expand", "yas/expand"]
        end if

      if (.not. associated(python_forward_expression_function)) python_forward_expression_function => nav_forward_expression_default
        if (.not. associated(python_fill_comment_function)) python_fill_comment_function => python_fill_comment
        if (.not. associated(python_fill_string_function)) python_fill_string_function => python_fill_string
        if (.not. associated(python_fill_decorator_function)) python_fill_decorator_function => python_fill_decorator
        if (.not. associated(python_fill_paren_function)) python_fill_paren_function => python_fill_paren

        if (.not. allocated(python_nav_beginning_of_definition_regexp)) then
            python_nav_beginning_of_definition_regexp = BEGINNING_OF_SUBPROGRAM_RE
        end if

        if (.not. allocated(python_nav_beginning_of_block_regexp)) python_nav_beginning_of_block_regexp = BEGINNING_OF_BLOCK_RE
        call init_syntax_tables()
        call init_keywords()
        call init_rules()
        call add_local_variables_killer(kill_python_locals)
        call major_modes%python_base_mode%define("python_base_mode", "Python", python_base_mode, python_base_mode_hook, &
                                                 parent=major_modes%prog_mode, syntax_table=python_mode_syntax_table)
        python_base_mode_map = major_modes%python_base_mode%map
        call major_modes%python_mode%define("python_mode", "Python", python_mode, python_mode_hook, &
                                            parent=major_modes%python_base_mode)
        python_mode_map = major_modes%python_mode%map
        ! Movement
        call bind_remap("backward_sentence", "python_nav_backward_block")
        call bind_remap("forward_sentence", "python_nav_forward_block")
        call bind_remap("backward_up_group", "python_nav_backward_up_group")
        call bind_remap("up_group", "python_nav_up_group")
        call bind_remap("mark_subprogram", "python_mark_definition")
        ! Indent specific
        call keymap_set(python_base_mode_map, "DEL", "python_indent_dedent_line_backspace", st)
        call keymap_set(python_base_mode_map, "<backtab>", "python_indent_dedent_line", st)
        call keymap_set(python_base_mode_map, "C-c <", "python_indent_shift_left", st)
        call keymap_set(python_base_mode_map, "C-c >", "python_indent_shift_right", st)
        ! Utilities
        call bind_remap("complete_name", "completion_at_point")
        call make_sparse_keymap(python_indent_repeat_map)
        call keymap_set(python_indent_repeat_map, "<", "python_indent_shift_left", st)
        call keymap_set(python_indent_repeat_map, ">", "python_indent_shift_right", st)
        commands%python_base_mode%run => python_base_mode
        commands%python_mode%run => python_mode
        commands%python_indent_guess_indent_offset%run => cmd_python_indent_guess_indent_offset
        commands%python_indent_dedent_line%run => cmd_python_indent_dedent_line
        commands%python_indent_dedent_line_backspace%run => cmd_python_indent_dedent_line_backspace
        commands%python_indent_shift_left%run => cmd_python_indent_shift_left
        commands%python_indent_shift_right%run => cmd_python_indent_shift_right
        commands%python_mark_definition%run => cmd_python_mark_definition
        commands%python_nav_end_of_definition%run => cmd_python_nav_end_of_definition
        commands%python_nav_backward_definition%run => cmd_python_nav_backward_definition
        commands%python_nav_forward_definition%run => cmd_python_nav_forward_definition
        commands%python_nav_beginning_of_statement%run => cmd_python_nav_beginning_of_statement
        commands%python_nav_end_of_statement%run => cmd_python_nav_end_of_statement
        commands%python_nav_backward_statement%run => cmd_python_nav_backward_statement
        commands%python_nav_forward_statement%run => cmd_python_nav_forward_statement
        commands%python_nav_beginning_of_block%run => cmd_python_nav_beginning_of_block
        commands%python_nav_end_of_block%run => cmd_python_nav_end_of_block
        commands%python_nav_backward_block%run => cmd_python_nav_backward_block
        commands%python_nav_forward_block%run => cmd_python_nav_forward_block
        commands%python_nav_forward_expression%run => cmd_python_nav_forward_expression
        commands%python_nav_backward_expression%run => cmd_python_nav_backward_expression
        commands%python_nav_forward_expression_safe%run => cmd_python_nav_forward_expression_safe
        commands%python_nav_backward_expression_safe%run => cmd_python_nav_backward_expression_safe
        commands%python_nav_up_group%run => cmd_python_nav_up_group
        commands%python_nav_backward_up_group%run => cmd_python_nav_backward_up_group
        commands%python_nav_if_name_main%run => cmd_python_nav_if_name_main
        commands%python_fill_paragraph%run => cmd_python_fill_paragraph
        call set_repeat_map(python_indent_repeat_map)
    end subroutine syms_of_python

    ! Bind [remap FROM] to COMMAND in `python_base_mode_map'.
    ! Not a port.
    recursive subroutine bind_remap(from, command)
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call list_elements(list(sym("remap"), sym(from)), events)
        call define_key(python_base_mode_map, events, sym(command), st)
    end subroutine bind_remap

    ! Not a port.
    recursive subroutine init_syntax_tables()
        type(status_type) :: st
        type(handle_type) :: t
        type(syntax_descriptor_type) :: symbol, d
        integer :: i

        call make_syntax_table(python_mode_syntax_table, st)
        t = python_mode_syntax_table
        ! Give punctuation syntax to ASCII that normally has symbol
        ! syntax or has word syntax and isn't a letter.
        call string_to_syntax("_", symbol, st)

        do i = 0, 127
            if (i == iachar("_")) cycle
            d = syntax_entry(i, standard_syntax_table())
            if (d%code == symbol%code .and. d%match == symbol%match) call modify_syntax_entry(i, i, ".", st, t)
        end do

        call modify_syntax_entry(iachar("$"), iachar("$"), ".", st, t)
        call modify_syntax_entry(iachar("%"), iachar("%"), ".", st, t)
        ! exceptions
        call modify_syntax_entry(iachar("#"), iachar("#"), "<", st, t)
        call modify_syntax_entry(10, 10, ">", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar("`"), iachar("`"), "$", st, t)
        call make_syntax_table(python_dotty_syntax_table, st, python_mode_syntax_table)
        t = python_dotty_syntax_table
        call modify_syntax_entry(iachar("."), iachar("."), "w", st, t)
        call modify_syntax_entry(iachar("_"), iachar("_"), "w", st, t)
    end subroutine init_syntax_tables

    ! A highlight of subexpression N in FACE.
    ! Not a port.
    recursive function h(n, face, override, lax) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        integer, intent(in), optional :: override
        logical, intent(in), optional :: lax
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, override=override, laxmatch=lax)
    end function h

    ! Not a port.
    recursive subroutine init_keywords()
        character(len=*), parameter :: VA = "font_lock_variable_name_face", TY = "font_lock_type_face", &
                                       OP = "font_lock_operator_face", CO = "font_lock_constant_face"
        type(font_lock_keyword_type) :: kw(20)
        type(font_lock_highlight_type) :: h1(1), h2(2), a(1), h3(3)
        integer :: n

        h1(1) = h(1, "font_lock_function_name_face")
        kw(1) = font_lock_keyword(DEF_RE, highlights=h1)
        h1(1) = h(1, TY)
        kw(2) = font_lock_keyword(CLASS_RE, highlights=h1)
        python_font_lock_keywords_level_1 = kw(1:2)
        kw(3) = font_lock_keyword(KEYWORDS2_RE)
        ! Builtins
        kw(4) = font_lock_keyword(BUILTINS_RE, face="font_lock_builtin_face")
        python_font_lock_keywords_level_2 = kw(1:4)
        n = 0
        if (allocated(python_font_lock_keywords_maximum_decoration)) deallocate (python_font_lock_keywords_maximum_decoration)
        call push(font_lock_keyword(matcher=python_font_lock_f_strings))
        call push_level_2()
        ! Constants
        call push(font_lock_keyword(CONSTANTS_RE, face=CO))
        ! Decorators.
        h1(1) = h(1, TY)
        call push(font_lock_keyword(DECORATOR_RE, highlights=h1))
        ! Builtin Exceptions
        call push(font_lock_keyword(EXCEPTIONS_RE, face=TY))
        ! single assignment with/without type hints, e.g.
        !   a: int = 5
        !   b: Tuple[Optional[int], Union[Sequence[str], str]] = (None, 'foo')
        !   c: Collection = {1, 2, 3}
        !   d: Mapping[int, str] = {1: 'bar', 2: 'baz'}
        h1(1) = h(0, TY)
        a(1) = font_lock_anchored(h1, regexp=SYMBOL_NAME_RE, pre_match_form=assignment_1_pre_form)
        h3(1) = h(1, VA)
        h3(2) = h(3, OP)
        h3(3) = a(1)
        call push(font_lock_keyword(matcher=assignment_matcher_1, highlights=h3))
        ! multiple assignment
        ! (note that type hints are not allowed for multiple assignments)
        !   a, b, c = 1, 2, 3
        !   a, *b, c = 1, 2, 3, 4, 5
        !   [a, b] = (1, 2)
        !   (l[1], l[2]) = (10, 11)
        !   (a, b, c, *d) = *x, y = 5, 6, 7, 8, 9
        !   (a,) = 'foo'
        !   (*a,) = ['foo', 'bar', 'baz']
        !   d.x, d.y[0], *d.z = 'a', 'b', 'c', 'd', 'e'
        ! and variants thereof
        ! the cases
        !   (a) = 5
        !   [a] = 5,
        !   [*a] = 5, 6
        ! are handled separately below
        h1(1) = h(1, VA)
        a(1) = font_lock_anchored(h1, regexp=GROUPED_TARGET_RE, pre_match_form=assignment_2_pre_form)
        h3(1) = h(1, VA)
        h3(2) = h(2, OP)
        h3(3) = a(1)
        call push(font_lock_keyword(matcher=assignment_matcher_2, highlights=h3))
        ! special cases
        !   (a) = 5
        !   [a] = 5,
        !   [*a] = 5, 6
        h2(1) = h(1, VA)
        h2(2) = h(2, OP)
        call push(font_lock_keyword(matcher=assignment_matcher_3, highlights=h2))
        ! Operators.
        call push(font_lock_keyword(OPERATOR_RE, face=OP))
        ! escape sequences within bytes literals
        !   "\\" "\'" "\a" "\b" "\f" "\n" "\r" "\t" "\v"
        !   "\ooo" character with octal value ooo
        !   "\xhh" character with hex value hh
        h1(1) = h(1, CO, FONT_LOCK_OVERRIDE_YES)
        call push(font_lock_keyword(matcher=bytes_escape_matcher, highlights=h1))
        ! escape sequences within string literals, the same as appear in bytes
        ! literals in addition to:
        !   "\uxxxx" Character with 16-bit hex value xxxx
        !   "\Uxxxxxxxx" Character with 32-bit hex value xxxxxxxx
        !   "\N{name}" Character named name in the Unicode database
        call push(font_lock_keyword(matcher=string_escape_matcher, highlights=h1))

    contains

        subroutine push(k)
            type(font_lock_keyword_type), intent(in) :: k
            type(font_lock_keyword_type), allocatable :: grown(:)

            n = n + 1

            if (.not. allocated(python_font_lock_keywords_maximum_decoration)) then
                allocate (python_font_lock_keywords_maximum_decoration(1))
                python_font_lock_keywords_maximum_decoration(1) = k
                return
            end if

            allocate (grown(size(python_font_lock_keywords_maximum_decoration) + 1))
            grown(1:size(python_font_lock_keywords_maximum_decoration)) = python_font_lock_keywords_maximum_decoration
            grown(size(grown)) = k
            call move_alloc(grown, python_font_lock_keywords_maximum_decoration)
        end subroutine push

        subroutine push_level_2()
            integer :: k

            do k = 1, size(python_font_lock_keywords_level_2)
                call push(python_font_lock_keywords_level_2(k))
            end do
        end subroutine push_level_2
    end subroutine init_keywords

    ! The rules of python-syntax-propertize-function.
    ! Not a port.
    recursive subroutine init_rules()
        type(syntax_highlight_type) :: hl

        if (allocated(propertize_rules%rules)) return
        allocate (propertize_rules%rules(1))
        propertize_rules%rules(1)%regexp = TRIPLE_QUOTES_RE
        hl = syntax_highlight_type(group=0)
        hl%after => stringify_after
        propertize_rules%rules(1)%highlights = [hl]
    end subroutine init_rules

    ! (0 (ignore (python-syntax-stringify)))
    ! Port of python.el:python-syntax-propertize-function (in part).
    recursive subroutine stringify_after(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_syntax_stringify()
    end subroutine stringify_after

    ! The `syntax_propertize_function' of Python mode (python.el
    ! python-syntax-propertize-function).
    ! Port of python.el:python-syntax-propertize-function.
    recursive subroutine python_syntax_propertize_function(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call goto_char(start)
        call propertize_rules%apply(start, end, status)
    end subroutine python_syntax_propertize_function

    ! kill-all-local-variables: forget the current buffer's navigation
    ! cache.
    ! Not a port.
    recursive subroutine kill_python_locals()
        integer :: k

        k = nav_cache_index(.false.)
        if (k > 0) nav_caches(k)%serial = 0
    end subroutine kill_python_locals

    ! ------------------------------------------------------------------
    ! Font-lock and syntax
    ! ------------------------------------------------------------------

    ! The parse state at point (syntax-ppss).
    ! Not a port.
    recursive function ppss_here() result(ppss)
        type(parse_state_type) :: ppss

        call syntax_ppss(ppss)
    end function ppss_here

    ! The parse state at POS, leaving point there ((syntax-ppss POS)).
    ! Not a port.
    recursive function ppss_at(pos) result(ppss)
        integer(int64), intent(in) :: pos
        type(parse_state_type) :: ppss

        call goto_char(pos)
        call syntax_ppss(ppss)
    end function ppss_at

    !> Return the start position if point is on TYPE using SYNTAX_PPSS
    !> (0 if not).  TYPE can be PYTHON_CONTEXT_COMMENT,
    !> PYTHON_CONTEXT_STRING, PYTHON_CONTEXT_SINGLE_QUOTED_STRING,
    !> PYTHON_CONTEXT_TRIPLE_QUOTED_STRING or PYTHON_CONTEXT_PAREN.  It
    !> returns the start character address of the specified TYPE.  Port of
    !> python.el:python-syntax-context.
    recursive integer(int64) function python_syntax_context(type, syntax_ppss_state) result(pos)
        integer, intent(in) :: type
        type(parse_state_type), intent(in), optional :: syntax_ppss_state
        type(parse_state_type) :: ppss

        if (present(syntax_ppss_state)) then
            ppss = syntax_ppss_state
        else
            ppss = ppss_here()
        end if

        pos = 0

        select case (type)
        case (PYTHON_CONTEXT_COMMENT)
            if (ppss%incomment /= 0) pos = ppss%comstr_start
        case (PYTHON_CONTEXT_STRING)
            if (ppss%instring >= 0) pos = ppss%comstr_start
        case (PYTHON_CONTEXT_SINGLE_QUOTED_STRING)
            if (ppss%instring >= 0 .and. ppss%instring /= ST_STRING_STYLE) pos = ppss%comstr_start
        case (PYTHON_CONTEXT_TRIPLE_QUOTED_STRING)
            if (ppss%instring == ST_STRING_STYLE) pos = ppss%comstr_start
        case (PYTHON_CONTEXT_PAREN)
            pos = ppss%innermost_start
        case default
            continue
        end select
    end function python_syntax_context

    !> Return the context type using SYNTAX_PPSS.
    !> The type returned can be PYTHON_CONTEXT_COMMENT,
    !> PYTHON_CONTEXT_STRING or PYTHON_CONTEXT_PAREN (0 for none).  Port of
    !> python.el:python-syntax-context-type.
    recursive integer function python_syntax_context_type(syntax_ppss_state) result(type)
        type(parse_state_type), intent(in), optional :: syntax_ppss_state
        type(parse_state_type) :: ppss

        if (present(syntax_ppss_state)) then
            ppss = syntax_ppss_state
        else
            ppss = ppss_here()
        end if

        type = 0

        if (ppss%comstr_start > 0) then
            if (ppss%incomment /= 0) then
                type = PYTHON_CONTEXT_COMMENT
            else
                type = PYTHON_CONTEXT_STRING
            end if
        else if (ppss%innermost_start > 0) then
            type = PYTHON_CONTEXT_PAREN
        end if
    end function python_syntax_context_type

    !> Return the start of the comment or string PPSS is inside (0 if
    !> neither).  Port of python.el:python-syntax-comment-or-string-p.
    recursive integer(int64) function is_python_syntax_comment_or_string(syntax_ppss_state) result(pos)
        type(parse_state_type), intent(in), optional :: syntax_ppss_state
        type(parse_state_type) :: ppss

        if (present(syntax_ppss_state)) then
            ppss = syntax_ppss_state
        else
            ppss = ppss_here()
        end if

        pos = ppss%comstr_start
    end function is_python_syntax_comment_or_string

    !> Return true if char after point is a closing paren.  Port of
    !> python.el:python-syntax-closing-paren-p.
    recursive logical function is_python_syntax_closing_paren()
        type(syntax_descriptor_type) :: d, close
        type(status_type) :: st
        integer :: c

        is_python_syntax_closing_paren = .false.
        c = after_char_at(point())
        if (c < 0) return
        d = syntax_entry_at(c, point())
        call string_to_syntax(")", close, st)
        is_python_syntax_closing_paren = iand(d%code, 65535) == iand(close%code, 65535)
    end function is_python_syntax_closing_paren

    !> Return syntactic face given STATE.  Port of
    !> python.el:python-font-lock-syntactic-face-function.
    recursive subroutine python_font_lock_syntactic_face_function(state, face)
        type(parse_state_type), intent(in) :: state
        character(len=:), allocatable, intent(out) :: face

        if (state%instring >= 0) then
            if (is_python_info_docstring(state)) then
                face = "font_lock_doc_face"
            else
                face = "font_lock_string_face"
            end if
        else
            face = "font_lock_comment_face"
        end if
    end subroutine python_font_lock_syntactic_face_function

    ! Return true if the pos where PPSS was found is inside an f-string.
    ! Port of python.el:python--f-string-p.
    recursive logical function is_f_string(ppss)
        type(parse_state_type), intent(in) :: ppss
        character(len=:), allocatable :: before_quote
        integer(int64) :: spos
        type(status_type) :: st
        character :: syn

        is_f_string = .false.
        if (ppss%instring < 0) return
        spos = ppss%comstr_start - 1
        call buffer_substring(max(spos - 4, point_min()), min(spos + 2, point_max()), before_quote, st)
        if (string_match(F_STRING_START_RE, before_quote, st) <= 0) return

        if (point_min() < spos) then
            is_f_string = .true.
        else
            syn = char_syntax(before_char_at(spos))
            is_f_string = syn /= "w" .and. syn /= "_"
        end if
    end function is_f_string

    ! Mark {...} holes as being code.
    ! Remove the (presumably `font_lock_string_face') `face' property from
    ! the {...} holes that appear within f-strings.  Port of
    ! python.el:python--font-lock-f-strings.
    recursive subroutine python_font_lock_f_strings(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(parse_state_type) :: ppss
        integer(int64) :: send, beg, end, pos, f, opoint
        type(status_type) :: st
        logical :: saved_ignore

        status = status_type()
        found = .false.
        ppss = ppss_here()

        do
            do while (.not. is_f_string(ppss))
                call re_search_forward(F_STRING_START_RE, f, st, bound=limit, noerror=SEARCH_MOVE)
                if (.not. st%ok() .or. f <= 0) exit
                ppss = ppss_here()
            end do

            if (.not. point() < limit) exit
            ! (goto-char (nth 8 ppss)) and (forward-sexp 1) with the
            ! default function.
            opoint = point()
            call goto_char(ppss%comstr_start)
            call forward_expression_default_function(1_int64, st)

            if (st%ok()) then
                send = min(limit, point() - 1)
            else
                send = limit
            end if

            call goto_char(opoint)

            do
                call re_search_forward("{", f, st, bound=send, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit

                if (after_char_at(point()) == iachar("{")) then
                    ! Just skip over {{
                    call goto_char(point() + 1)
                else
                    beg = match_beginning(0, st)
                    saved_ignore = parse_expression_ignore_comments%value
                    parse_expression_ignore_comments%value = .false.
                    pos = scan_groups(point(), 1_int64, 1_int64, st)
                    parse_expression_ignore_comments%value = saved_ignore

                    if (st%ok() .and. pos > 0) then
                        end = min(send, pos)
                    else
                        end = send
                    end if

                    call goto_char(end)
                    call put_text_property(beg, end, sym("face"), value_type(), st)
                end if
            end do

            call goto_char(min(limit, send + 1))
            ppss = ppss_here()
        end do
    end subroutine python_font_lock_f_strings

    ! The matcher of the assignments for REGEXP: search for the next
    ! occurrence if REGEXP matched within a paren context (to avoid, e.g.,
    ! default values for arguments or passing arguments by name being
    ! treated as assignments) or is followed by an '=' sign (to avoid '=='
    ! being treated as an assignment).  Set point to the position one
    ! character before the end of the occurrence found so that subsequent
    ! searches can detect the '=' sign in chained assignment.  Port of
    ! python.el:python-font-lock-assignment-matcher.
    recursive subroutine assignment_matcher(regexp, limit, found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        integer(int64) :: f
        type(status_type) :: st
        integer(int64), allocatable :: md(:)

        found = .false.

        do
            call re_search_forward(regexp, f, st, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) return
            md = match_data()

            if (python_syntax_context(PYTHON_CONTEXT_PAREN) == 0 .and. after_char_at(point()) /= iachar("=")) then
                call set_match_data(md)
                call goto_char(point() - 1)
                found = .true.
                return
            end if

            call set_match_data(md)
        end do
    end subroutine assignment_matcher

    ! Not a port.
    recursive subroutine assignment_matcher_1(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call assignment_matcher(ASSIGN1_RE, limit, found)
    end subroutine assignment_matcher_1

    ! Not a port.
    recursive subroutine assignment_matcher_2(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call assignment_matcher(ASSIGN2_RE, limit, found)
    end subroutine assignment_matcher_2

    ! Not a port.
    recursive subroutine assignment_matcher_3(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call assignment_matcher(ASSIGN3_RE, limit, found)
    end subroutine assignment_matcher_3

    ! (progn (when-let* ((type-start (match-beginning 2)))
    !          (goto-char type-start))
    !        (match-end 0))
    ! Port of python.el:python-font-lock-keywords-maximum-decoration (in part).
    recursive subroutine assignment_1_pre_form(limit)
        integer(int64), intent(out) :: limit
        integer(int64) :: type_start
        type(status_type) :: st

        type_start = match_beginning(2, st)
        if (st%ok() .and. type_start > 0) call goto_char(type_start)
        limit = match_end(0, st)
    end subroutine assignment_1_pre_form

    ! (progn (goto-char (match-end 1)) (match-beginning 2))
    ! Port of python.el:python-font-lock-keywords-maximum-decoration (in part).
    recursive subroutine assignment_2_pre_form(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st

        ! go back after the first symbol
        call goto_char(match_end(1, st))
        ! limit the search until the assignment
        limit = match_beginning(2, st)
    end subroutine assignment_2_pre_form

    ! Match REGEXP within a string or bytes literal whose start matches
    ! START_REGEXP.  Port of python.el:python--string-bytes-literal-matcher.
    recursive subroutine string_bytes_literal_matcher(regexp, start_regexp, limit, found)
        character(len=*), intent(in) :: regexp, start_regexp
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        character(len=:), allocatable :: before_quote
        integer(int64), allocatable :: md(:)
        integer(int64) :: f, pos
        type(parse_state_type) :: ppss
        type(status_type) :: st

        found = .false.

        do
            call re_search_forward(regexp, f, st, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) return
            md = match_data()
            ppss = ppss_here()
            pos = ppss%comstr_start

            if (pos > 0) then
                call buffer_substring(max(pos - 4, point_min()), min(pos + 1, point_max()), before_quote, st)
                call goto_char(point() - 1)

                if (string_match(start_regexp, before_quote, st) > 0) then
                    call set_match_data(md)
                    found = .true.
                    return
                end if
            end if

            call set_match_data(md)
        end do
    end subroutine string_bytes_literal_matcher

    ! Not a port.
    recursive subroutine bytes_escape_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call string_bytes_literal_matcher(BYTES_ESCAPE_RE, NOT_RAW_BYTES_START_RE, limit, found)
    end subroutine bytes_escape_matcher

    ! Not a port.
    recursive subroutine string_escape_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call string_bytes_literal_matcher(STRING_ESCAPE_RE, NOT_RAW_STRING_START_RE, limit, found)
    end subroutine string_escape_matcher

    !> Count number of quotes around point (max is 3).
    !> QUOTE_CHAR is the quote char to count.  Optional argument POINT is
    !> the point where scan starts (defaults to current point), and LIMIT
    !> is used to limit the scan.  Port of python.el:python-syntax-count-quotes.
    recursive integer function python_syntax_count_quotes(quote_char, pos, limit) result(i)
        integer, intent(in) :: quote_char
        integer(int64), intent(in), optional :: pos, limit
        integer(int64) :: p

        p = point()
        if (present(pos)) p = pos
        i = 0

        do while (i < 3)
            if (present(limit)) then
                if (.not. p + i < limit) exit
            end if

            if (after_char_at(p + i) /= quote_char) exit
            i = i + 1
        end do
    end function python_syntax_count_quotes

    !> Put `syntax_table' property correctly on single/triple quotes.
    !> Port of python.el:python-syntax-stringify.
    recursive subroutine python_syntax_stringify()
        type(parse_state_type) :: ppss
        integer(int64) :: string_start, quote_starting_pos, quote_ending_pos, opoint
        type(status_type) :: st
        logical :: string_literal_concat, other_quotes

        opoint = point()
        call goto_char(point() - 3)
        ppss = ppss_here()
        call goto_char(opoint)
        string_start = 0
        if (ppss%instring == ST_STRING_STYLE) string_start = ppss%comstr_start
        other_quotes = .false.

        if (string_start > 0) then
            other_quotes = after_char_at(string_start) /= after_char_at(point() - 3)
        end if

        string_literal_concat = ppss%instring >= 0 .and. ppss%instring /= ST_STRING_STYLE
        quote_starting_pos = point() - 3
        quote_ending_pos = point()

        if (ppss%incomment /= 0) then
            ! Inside a comment: do nothing.
            continue
        else if (other_quotes) then
            ! Inside of a string quoted with different triple quotes.
            continue
        else if (ppss%quoted) then
            ! The first quote is escaped, so it's not part of a triple quote!
            call goto_char(quote_starting_pos + 1)
        else if (string_literal_concat) then
            ! Handle string literal concatenation (bug#45897)
            continue
        else if (string_start == 0) then
            ! This set of quotes delimit the start of a string.  Put
            ! string fence syntax on last quote. (bug#49518)
            ! FIXME: This makes sexp-movement a bit suboptimal since """a"""
            ! is now treated as 3 strings.
            ! We could probably have our cake and eat it too by
            ! putting the string fence on the first quote and then
            ! convincing `syntax-ppss-flush-cache' to flush to before
            ! that fence when any char of the 3-char delimiter
            ! is modified.
            call put_text_property(quote_ending_pos - 1, quote_ending_pos, sym("syntax_table"), fence(), st)
        else
            ! This set of quotes delimit the end of a string.  Put
            ! string fence syntax on first quote. (bug#49518)
            call put_text_property(quote_starting_pos, quote_starting_pos + 1, sym("syntax_table"), fence(), &
                                   st)
        end if
    end subroutine python_syntax_stringify

    ! (string-to-syntax "|") as a `syntax_table' property value.
    ! Not a port.
    recursive function fence() result(v)
        use syntax_fm, only: syntax_descriptor_value
        type(value_type) :: v
        type(syntax_descriptor_type) :: d
        type(status_type) :: st

        call string_to_syntax("|", d, st)
        v = syntax_descriptor_value(d)
    end function fence

    ! ------------------------------------------------------------------
    ! Navigation
    ! ------------------------------------------------------------------

    ! Internal implementation of `python_nav_beginning_of_definition'.
    ! With positive ARG search backwards, else search forwards.  Port of
    ! python.el:python-nav--beginning-of-defun.
    recursive logical function nav_beginning_of_subprogram_1(arg_in) result(found)
        integer(int64), intent(in) :: arg_in
        integer(int64) :: arg, line_beg_pos, line_content_start, pos, min_indentation, body_indentation, f, opoint
        logical :: backward, more
        type(status_type) :: st

        arg = arg_in
        if (arg == 0) arg = 1
        backward = arg > 0
        line_beg_pos = line_beginning_position()
        line_content_start = line_beg_pos + current_indentation()
        pos = point()

        if (is_python_info_current_line_empty()) then
            min_indentation = huge(1_int64)
        else
            min_indentation = current_indentation()
        end if

        body_indentation = 0

        if (arg > 0) then
            if (python_info_looking_at_beginning_of_definition(check_statement=.true.)) then
                opoint = point()
                f = python_nav_beginning_of_statement()
                body_indentation = current_indentation() + python_indent_offset%value
                call goto_char(opoint)
            else
                opoint = point()

                do
                    if (python_nav_backward_block() == 0) exit
                    more = .not. python_info_looking_at_beginning_of_definition()
                    if (.not. more) more = current_indentation() >= min_indentation
                    if (.not. more) exit
                    min_indentation = min(min_indentation, current_indentation())
                end do

                if (python_info_looking_at_beginning_of_definition()) then
                    body_indentation = current_indentation() + python_indent_offset%value
                else
                    body_indentation = 0
                end if

                call goto_char(opoint)
            end if
        end if

        if (python_info_looking_at_beginning_of_definition(check_statement=.true.)) then
            ! If looking at beginning of subprogram, and if
            ! pos is > line-content-start, ensure a
            ! backward re search match this subprogram by
            ! going to end of line before calling
            ! re-search-fn bug#40563
            if (arg < 0) then
                f = python_nav_end_of_statement()
            else if (is_python_info_continuation_line() > 0 .or. pos > line_content_start) then
                f = python_nav_end_of_statement()
            end if
        end if

        do
            if (backward) then
                call re_search_backward(python_nav_beginning_of_definition_regexp, f, st, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(python_nav_beginning_of_definition_regexp, f, st, noerror=SEARCH_NOERROR)
            end if

            if (.not. st%ok() .or. f <= 0) exit
            if (python_syntax_context_type() /= 0) cycle
            ! Handle nested subprograms when moving backwards by checking
            ! indentation.
            if (arg > 0 .and. current_indentation() /= 0 .and. current_indentation() >= body_indentation) cycle
            exit
        end do

        found = python_info_looking_at_beginning_of_definition(check_statement=.true.)

        if (found) then
            found = line_number_at_pos(pos) /= line_number_at_pos()
            if (.not. found) found = point() >= line_beg_pos .and. point() <= line_content_start .and. pos > line_content_start
        end if

        if (found) then
            if (arg < 0) f = python_nav_beginning_of_statement()
            call beginning_of_line(1_int64)
        else
            call goto_char(pos)
        end if
    end function nav_beginning_of_subprogram_1

    !> Move point to `beginning_of_subprogram'.
    !> With positive ARG search backwards else search forward.
    !> ARG 0 defaults to 1.  When searching backwards,
    !> nested subprograms are handled with care depending on current
    !> point position.  Return true if point is moved to
    !> `beginning_of_subprogram'.  Port of python.el:python-nav-beginning-of-defun
    !> (the `beginning_of_subprogram_function' of Python mode).
    recursive subroutine python_nav_beginning_of_definition(arg_in, found, status)
        integer(int64), intent(in) :: arg_in
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: arg

        status = status_type()
        arg = arg_in
        if (arg == 0) arg = 1
        found = .false.

        do while (arg /= 0)
            if (.not. nav_beginning_of_subprogram_1(arg)) exit
            found = .true.

            if (arg > 0) then
                arg = arg - 1
            else
                arg = arg + 1
            end if
        end do
    end subroutine python_nav_beginning_of_definition

    ! (python-nav-beginning-of-defun ARG) as a function.
    ! Not a port.
    recursive logical function nav_beginning_of_subprogram(arg) result(found)
        integer(int64), intent(in) :: arg
        type(status_type) :: st

        call python_nav_beginning_of_definition(arg, found, st)
    end function nav_beginning_of_subprogram

    !> Move point to the end of def or class.
    !> Returns false if point is not in a def or class.  Port of
    !> python.el:python-nav-end-of-defun.
    recursive logical function python_nav_end_of_definition() result(moved)
        integer(int64) :: beg_subprogram_indent, beg_pos, f, shortage
        logical :: ok, dummy

        beg_pos = point()
        moved = .false.
        ok = python_info_looking_at_beginning_of_definition()
        if (.not. ok) ok = nav_beginning_of_subprogram(1_int64)
        if (.not. ok) ok = nav_beginning_of_subprogram(-1_int64)
        if (.not. ok) return
        beg_subprogram_indent = current_indentation()

        do
            f = python_nav_end_of_statement()
            dummy = python_util_forward_comment(1)
            if (.not. (current_indentation() > beg_subprogram_indent .and. .not. is_eob())) exit
        end do

        dummy = python_util_forward_comment(-1)
        shortage = forward_line(1_int64)
        ! Ensure point moves forward.
        if (beg_pos > point()) call goto_char(beg_pos)
        ! Return non-nil if we did something (because then we were in a
        ! def/class).
        moved = beg_pos /= point()
    end function python_nav_end_of_definition

    ! `python_nav_end_of_definition' as `end_of_subprogram_function'.
    ! Not a port.
    recursive subroutine python_nav_end_of_definition_function(status)
        type(status_type), intent(out) :: status
        logical :: moved

        status = status_type()
        moved = python_nav_end_of_definition()
    end subroutine python_nav_end_of_definition_function

    ! Move point using FN avoiding places within comments, strings or
    ! parens: return the position (0 if none) where FN moved to out of
    ! them, further in direction DIR (1 forward, -1 backward) than the
    ! previous place found.  Port of python.el:python-nav--syntactically.
    recursive integer(int64) function nav_syntactically(fn, dir) result(found)
        procedure(nav_motion) :: fn
        integer, intent(in) :: dir
        integer(int64) :: start_pos, prev_pos, newpos
        integer :: context
        logical :: compare_ok

        start_pos = point()
        prev_pos = 0

        do
            newpos = 0
            if (fn()) newpos = point()
            context = python_syntax_context_type()
            compare_ok = .false.

            if (newpos > 0) then
                if (prev_pos == 0) then
                    compare_ok = .true.
                else if (dir > 0) then
                    compare_ok = newpos > prev_pos
                else
                    compare_ok = newpos < prev_pos
                end if
            end if

            if (context == 0 .and. newpos > 0 .and. compare_ok) then
                found = point()
                return
            else if (newpos > 0 .and. context /= 0) then
                prev_pos = point()
            else
                if (newpos == 0) call goto_char(start_pos)
                found = 0
                return
            end if
        end do
    end function nav_syntactically

    ! Port of python.el:python-nav--forward-defun (in part: its forward search).
    recursive logical function search_subprogram_forward()
        integer(int64) :: f
        type(status_type) :: st

        call re_search_forward(python_nav_beginning_of_definition_regexp, f, st, noerror=SEARCH_NOERROR)
        search_subprogram_forward = st%ok() .and. f > 0
    end function search_subprogram_forward

    ! Port of python.el:python-nav--forward-defun (in part: its backward search).
    recursive logical function search_subprogram_backward()
        integer(int64) :: f
        type(status_type) :: st

        call re_search_backward(python_nav_beginning_of_definition_regexp, f, st, noerror=SEARCH_NOERROR)
        search_subprogram_backward = st%ok() .and. f > 0
    end function search_subprogram_backward

    ! Internal implementation of python-nav-{backward,forward}-subprogram.
    ! Uses ARG to define which function to call, and how many times
    ! repeat it.  Port of python.el:python-nav--forward-defun.
    recursive integer(int64) function nav_forward_subprogram(arg_in) result(found)
        integer(int64), intent(in) :: arg_in
        integer(int64) :: arg

        arg = arg_in
        found = 0

        do while (arg > 0)
            found = nav_syntactically(search_subprogram_forward, 1)
            if (found == 0) exit
            arg = arg - 1
        end do

        do while (arg < 0)
            found = nav_syntactically(search_subprogram_backward, -1)
            if (found == 0) exit
            arg = arg + 1
        end do
    end function nav_forward_subprogram

    !> Navigate to closer subprogram backward ARG times.
    !> Unlikely `python_nav_beginning_of_definition' this doesn't care about
    !> nested definitions.  Port of python.el:python-nav-backward-defun.
    recursive integer(int64) function python_nav_backward_definition(arg) result(found)
        integer(int64), intent(in) :: arg

        found = nav_forward_subprogram(-arg)
    end function python_nav_backward_definition

    !> Navigate to closer subprogram forward ARG times.
    !> Unlikely `python_nav_beginning_of_definition' this doesn't care about
    !> nested definitions.  Port of python.el:python-nav-forward-defun.
    recursive integer(int64) function python_nav_forward_definition(arg) result(found)
        integer(int64), intent(in) :: arg

        found = nav_forward_subprogram(arg)
    end function python_nav_forward_definition

    !> Move to start of current statement.  Port of
    !> python.el:python-nav-beginning-of-statement.
    recursive integer(int64) function python_nav_beginning_of_statement() result(pos)
        type(parse_state_type) :: ppss
        integer(int64) :: context_point, shortage, opoint
        logical :: continued

        shortage = forward_line(0_int64)
        ppss = ppss_here()
        context_point = python_syntax_context(PYTHON_CONTEXT_PAREN, ppss)
        if (context_point == 0) context_point = python_syntax_context(PYTHON_CONTEXT_STRING, ppss)

        if (is_bob()) then
            continue
        else if (context_point > 0) then
            call goto_char(context_point)
            pos = python_nav_beginning_of_statement()
        else
            opoint = point()
            shortage = forward_line(-1_int64)
            continued = is_python_info_line_ends_backslash() > 0
            call goto_char(opoint)

            if (continued) then
                shortage = forward_line(-1_int64)
                pos = python_nav_beginning_of_statement()
            end if
        end if

        call back_to_indentation()
        pos = point()
    end function python_nav_beginning_of_statement

    !> Move to end of current statement.
    !> Optional argument NOEND is internal and makes the logic to not
    !> jump to the end of line when moving forward searching for the end
    !> of the statement.  Port of python.el:python-nav-end-of-statement.
    recursive integer(int64) function python_nav_end_of_statement(noend) result(pos)
        logical, intent(in), optional :: noend
        integer(int64) :: string_start, bs_pos, last_string_end, f, shortage, p
        character(len=:), allocatable :: message
        integer(int64) :: beg, end
        type(parse_state_type) :: ppss
        type(status_type) :: st
        logical :: no_end, found

        no_end = .false.
        if (present(noend)) no_end = noend
        last_string_end = 0

        do
            if (.not. no_end) call goto_char(line_end_position())
            if (is_eob()) exit
            string_start = python_syntax_context(PYTHON_CONTEXT_STRING)

            if (string_start > 0) then
                ! The condition can be nil if syntax table
                ! text properties and the `syntax-ppss' cache
                ! are somehow out of whack.  This has been
                ! observed when using `syntax-ppss' during
                ! narrowing.
                if (.not. string_start >= last_string_end) exit
                call goto_char(string_start)

                if (python_syntax_context(PYTHON_CONTEXT_PAREN) > 0) then
                    ! Ended up inside a paren, roll again.
                    p = python_nav_end_of_statement(.true.)
                else
                    ! This is not inside a paren, move to the
                    ! end of this string.
                    call goto_char(point() + python_syntax_count_quotes(after_char_at(point()), point()))
                    ppss = ppss_here()
                    found = .false.

                    if (ppss%instring == ST_STRING_STYLE) then
                        do
                            call re_search_forward(TRIPLE_QUOTES_RE, f, st, noerror=SEARCH_NOERROR)
                            if (.not. st%ok() .or. f <= 0) exit

                            if (python_syntax_context(PYTHON_CONTEXT_STRING) == 0) then
                                found = .true.
                                exit
                            end if
                        end do
                    else
                        call goto_char(string_start)
                        call nav_lisp_forward_expression(1_int64, st)
                        found = st%ok()
                    end if

                    if (.not. found) call goto_char(point_max())
                    last_string_end = point()
                end if
            else if (python_syntax_context(PYTHON_CONTEXT_PAREN) > 0) then
                ! The statement won't end before we've escaped
                ! at least one level of parenthesis.
                p = scan_groups(point(), 1_int64, -1_int64, st)

                if (st%ok()) then
                    call goto_char(p)
                else
                    call scan_error_data(st, message, beg, end)
                    call goto_char(end)
                end if
            else
                bs_pos = is_python_info_line_ends_backslash()
                if (bs_pos == 0) exit
                call goto_char(bs_pos)
                shortage = forward_line(1_int64)
            end if
        end do

        pos = point()
    end function python_nav_end_of_statement

    !> Move backward to previous statement.
    !> With ARG, repeat.  See `python_nav_forward_statement'.  Port of
    !> python.el:python-nav-backward-statement.
    recursive subroutine python_nav_backward_statement(arg)
        integer(int64), intent(in) :: arg

        call python_nav_forward_statement(-arg)
    end subroutine python_nav_backward_statement

    !> Move forward to next statement.
    !> With ARG, repeat.  With negative argument, move ARG times
    !> backward to previous statement.  Port of
    !> python.el:python-nav-forward-statement.
    recursive subroutine python_nav_forward_statement(arg_in)
        integer(int64), intent(in) :: arg_in
        integer(int64) :: arg, p
        logical :: dummy

        arg = arg_in

        do while (arg > 0)
            p = python_nav_end_of_statement()
            dummy = python_util_forward_comment()
            p = python_nav_beginning_of_statement()
            arg = arg - 1
        end do

        do while (arg < 0)
            p = python_nav_beginning_of_statement()
            dummy = python_util_forward_comment(-1)
            p = python_nav_beginning_of_statement()
            arg = arg + 1
        end do
    end subroutine python_nav_forward_statement

    ! The index of the current buffer's navigation cache (made if MAKE).
    ! Not a port.
    recursive integer function nav_cache_index(make) result(k)
        logical, intent(in) :: make
        type(nav_cache_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (cur == 0) return
        if (.not. allocated(nav_caches)) allocate (nav_caches(0))

        do k = 1, size(nav_caches)
            if (nav_caches(k)%serial == buffers(cur)%serial) return
        end do

        k = 0
        if (.not. make) return

        do k = 1, size(nav_caches)
            if (.not. buffer_serial_live(nav_caches(k)%serial)) then
                nav_caches(k) = nav_cache_type(serial=buffers(cur)%serial)
                return
            end if
        end do

        n = size(nav_caches)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = nav_caches(i)
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, nav_caches)
        k = n + 1
    end function nav_cache_index

    ! The current buffer's `buffer_chars_modified_tick'.
    ! Not a port.
    recursive integer(int64) function chars_tick() result(tick)
        type(status_type) :: st

        call buffer_chars_modified_tick(current_buffer(), tick, st)
    end function chars_tick

    ! Get value from the navigation cache.
    ! If the current buffer is not modified, the navigation cache is searched
    ! using KIND and the current line number as a key.  Port of
    ! python.el:python-nav-cache-get (0 for nil).
    recursive integer(int64) function nav_cache_get(kind) result(target)
        integer, intent(in) :: kind
        integer(int64) :: line
        integer :: k, i

        target = 0
        k = nav_cache_index(.false.)
        if (k == 0) return
        if (chars_tick() /= nav_caches(k)%tick) return
        if (.not. allocated(nav_caches(k)%entries)) return
        line = line_number_at_pos(absolute=.true.)

        do i = 1, size(nav_caches(k)%entries)
            if (nav_caches(k)%entries(i)%kind == kind .and. nav_caches(k)%entries(i)%line == line) then
                target = nav_caches(k)%entries(i)%target
                return
            end if
        end do
    end function nav_cache_get

    ! Add a key-value pair to the navigation cache.
    ! Invalidate the navigation cache if the current buffer has been modified.
    ! Then add a key-value pair to the navigation cache.  The key consists of
    ! KIND and CURRENT line number, and the value is TARGET position.
    ! Port of python.el:python-nav-cache-set.
    recursive subroutine nav_cache_set(kind, current, target)
        integer, intent(in) :: kind
        integer(int64), intent(in) :: current, target
        type(nav_cache_entry_type), allocatable :: grown(:)
        integer(int64) :: tick
        integer :: k, n

        k = nav_cache_index(.true.)
        if (k == 0) return
        tick = chars_tick()

        if (tick /= nav_caches(k)%tick .or. .not. allocated(nav_caches(k)%entries)) then
            if (allocated(nav_caches(k)%entries)) deallocate (nav_caches(k)%entries)
            allocate (nav_caches(k)%entries(0))
            nav_caches(k)%tick = tick
        end if

        n = size(nav_caches(k)%entries)
        allocate (grown(n + 1))
        grown(1) = nav_cache_entry_type(kind, current, target)
        grown(2:) = nav_caches(k)%entries
        call move_alloc(grown, nav_caches(k)%entries)
    end subroutine nav_cache_set

    !> Move to start of current block (its position, 0 if none).  Port of
    !> python.el:python-nav-beginning-of-block.
    recursive integer(int64) function python_nav_beginning_of_block() result(pos)
        integer(int64) :: target, current

        target = nav_cache_get(NAV_BEGINNING_OF_BLOCK)

        if (target > 0) then
            call goto_char(target)
            pos = point()
        else
            current = line_number_at_pos(absolute=.true.)
            pos = nav_beginning_of_block_1()
            call nav_cache_set(NAV_BEGINNING_OF_BLOCK, current, pos)
        end if
    end function python_nav_beginning_of_block

    ! Move to start of current block.
    ! This is an internal implementation of `python_nav_beginning_of_block'
    ! without the navigation cache.  Port of
    ! python.el:python-nav--beginning-of-block.
    recursive integer(int64) function nav_beginning_of_block_1() result(pos)
        integer(int64) :: starting_pos, p, shortage, block_matching_indent
        type(status_type) :: st

        starting_pos = point()

        ! Go to first line beginning a statement
        do
            if (is_bob()) exit
            p = python_nav_beginning_of_statement()
            if (.not. (is_python_info_current_line_comment() .or. is_python_info_current_line_empty())) exit
            shortage = forward_line(-1_int64)
        end do

        p = python_nav_beginning_of_statement()

        if (looking_at(BLOCK_START_RE, st)) then
            pos = point()
            return
        end if

        block_matching_indent = current_indentation() - python_indent_offset%value

        do
            if (python_nav_backward_block() == 0) exit
            if (.not. current_indentation() > block_matching_indent) exit
        end do

        pos = 0

        if (looking_at(BLOCK_START_RE, st)) then
            if (current_indentation() == block_matching_indent) pos = point()
        end if

        if (pos == 0) call goto_char(starting_pos)
    end function nav_beginning_of_block_1

    !> Move to end of current block (its position, 0 if none).  Port of
    !> python.el:python-nav-end-of-block.
    recursive integer(int64) function python_nav_end_of_block() result(pos)
        integer(int64) :: target, current

        target = nav_cache_get(NAV_END_OF_BLOCK)

        if (target > 0) then
            call goto_char(target)
            pos = point()
        else
            current = line_number_at_pos(absolute=.true.)
            pos = nav_end_of_block_1()
            call nav_cache_set(NAV_END_OF_BLOCK, current, pos)
        end if
    end function python_nav_end_of_block

    ! Move to end of current block.
    ! This is an internal implementation of `python_nav_end_of_block' without
    ! the navigation cache.  Port of python.el:python-nav--end-of-block.
    recursive integer(int64) function nav_end_of_block_1() result(pos)
        integer(int64) :: block_indentation, p, shortage
        logical :: dummy

        pos = 0
        if (python_nav_beginning_of_block() == 0) return
        block_indentation = current_indentation()
        p = python_nav_end_of_statement()

        do
            shortage = forward_line(1_int64)
            if (is_eob()) exit

            if (current_indentation() > block_indentation) then
                p = python_nav_end_of_statement()
                cycle
            end if

            if (is_python_info_current_line_comment() .or. is_python_info_current_line_empty()) cycle
            exit
        end do

        dummy = python_util_forward_comment(-1)
        pos = point()
    end function nav_end_of_block_1

    !> Move backward to previous block of code.
    !> With ARG, repeat.  See `python_nav_forward_block'.  Port of
    !> python.el:python-nav-backward-block.
    recursive integer(int64) function python_nav_backward_block(arg) result(pos)
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n

        n = 1
        if (present(arg)) n = arg
        pos = python_nav_forward_block(-n)
    end function python_nav_backward_block

    !> Move forward to next block of code.
    !> With ARG, repeat.  With negative argument, move ARG times
    !> backward to previous block.  Port of python.el:python-nav-forward-block.
    recursive integer(int64) function python_nav_forward_block(arg_in) result(pos)
        integer(int64), intent(in), optional :: arg_in
        integer(int64) :: arg, orig_arg, starting_pos, p, f
        type(status_type) :: st

        arg = 1
        if (present(arg_in)) arg = arg_in
        starting_pos = point()
        orig_arg = arg

        do while (arg > 0)
            p = python_nav_end_of_statement()

            do
                call re_search_forward(FORWARD_BLOCK_RE, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit
                if (python_syntax_context_type() == 0) exit
            end do

            arg = arg - 1
        end do

        do while (arg < 0)
            p = python_nav_beginning_of_statement()

            do
                call re_search_backward(FORWARD_BLOCK_RE, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit
                if (python_syntax_context_type() == 0) exit
            end do

            arg = arg + 1
        end do

        p = python_nav_beginning_of_statement()
        pos = 0

        if ((orig_arg > 0 .and. point() < starting_pos) .or. .not. looking_at(BLOCK_START_RE, st)) then
            call goto_char(starting_pos)
        else if (point() /= starting_pos) then
            pos = point()
        end if
    end function python_nav_forward_block

    ! Standard version `forward_expression'.
    ! It ignores completely the value of `forward_expression_function' by
    ! setting it to nil before calling `forward_expression'.  With positive
    ! ARG move forward only one expression, else move backwards.  Port of
    ! python.el:python-nav--lisp-forward-sexp.
    recursive subroutine nav_lisp_forward_expression(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        if (arg > 0) then
            call forward_expression_default_function(1_int64, status)
        else
            call forward_expression_default_function(-1_int64, status)
        end if
    end subroutine nav_lisp_forward_expression

    ! Safe version of standard `forward_expression'.
    ! When at end of expression (i.e. looking at an opening/closing paren)
    ! skips it instead of throwing an error.  With positive ARG move
    ! forward only one expression, else move backwards.  Port of
    ! python.el:python-nav--lisp-forward-sexp-safe.
    recursive subroutine nav_lisp_forward_expression_safe(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: f
        type(status_type) :: st

        call nav_lisp_forward_expression(arg, st)
        if (st%ok()) return

        do
            if (arg > 0) then
                call re_search_forward(CLOSE_PAREN_RE, f, st, noerror=SEARCH_NOERROR)
            else
                call re_search_backward(OPEN_PAREN_RE, f, st, noerror=SEARCH_NOERROR)
            end if

            if (.not. st%ok() .or. f <= 0) exit
            if (python_syntax_context(PYTHON_CONTEXT_PAREN) == 0) exit
        end do
    end subroutine nav_lisp_forward_expression_safe

    ! The syntax class of the character at POS (syntax-class (syntax-after
    ! POS)), -1 if none.
    ! Not a port.
    recursive integer function class_after(pos)
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d
        integer :: c

        class_after = -1
        c = after_char_at(pos)
        if (c < 0) return
        d = syntax_entry_at(c, pos)
        class_after = iand(d%code, 65535)
    end function class_after

    ! The syntax class of the descriptor STRING.
    ! Not a port.
    recursive integer function class_of(string)
        character(len=*), intent(in) :: string
        type(syntax_descriptor_type) :: d
        type(status_type) :: st

        call string_to_syntax(string, d, st)
        class_of = iand(d%code, 65535)
    end function class_of

    ! The kinds of place `nav_forward_expression_1' tells apart.
    ! Port of python.el:python-nav--forward-sexp (in part: its context tests).
    recursive integer function expression_context(next)
        logical, intent(in) :: next

        expression_context = 0

        if (is_python_info_beginning_of_block()) then
            expression_context = 1
        else if (is_python_info_end_of_block()) then
            expression_context = 2
        else if (is_python_info_beginning_of_statement()) then
            expression_context = 3
        else if (is_python_info_end_of_statement()) then
            expression_context = 4
        else if (next) then
            if (is_python_info_statement_starts_block()) then
                expression_context = 5
            else if (is_python_info_statement_ends_block()) then
                expression_context = 6
            end if
        end if
    end function expression_context

    ! Move to forward expression.
    ! With positive optional argument DIR direction move forward, else
    ! backwards.  When optional argument SAFE is true do not throw
    ! errors when at end of expression, skip it instead.  With optional
    ! argument SKIP_PARENS_P force expression motion to ignore parenthesized
    ! expressions when looking at them in either direction.  Port of
    ! python.el:python-nav--forward-sexp.
    recursive subroutine nav_forward_expression_1(dir_in, safe, skip_parens_p, status)
        integer(int64), intent(in) :: dir_in
        logical, intent(in) :: safe, skip_parens_p
        type(status_type), intent(out) :: status
        integer(int64), parameter :: BLOCK_START = 1, BLOCK_END = 2, STATEMENT_START = 3, STATEMENT_END = 4, &
                                     STARTS_BLOCK = 5, ENDS_BLOCK = 6
        integer(int64) :: dir, next_expression_pos, opoint, p
        integer :: context_type, context, next_expression_context
        logical :: forward_p, at_paren, dummy

        status = status_type()
        if (dir_in == 0) return

        if (dir_in > 0) then
            dir = 1
        else
            dir = -1
        end if

        forward_p = dir > 0
        context_type = python_syntax_context_type()

        if (context_type == PYTHON_CONTEXT_STRING .or. context_type == PYTHON_CONTEXT_COMMENT) then
            ! Inside of a string, get out of it.
            call forward_expression_default_function(dir, status)
            return
        end if

        at_paren = context_type == PYTHON_CONTEXT_PAREN

        if (.not. at_paren) then
            if (forward_p) then
                at_paren = class_after(point()) == class_of("(")
            else
                at_paren = class_after(point() - 1) == class_of(")")
            end if
        end if

        if (.not. skip_parens_p .and. at_paren) then
            ! Inside a paren or looking at it, lisp knows what to do.
            if (safe) then
                call nav_lisp_forward_expression_safe(dir)
            else
                call nav_lisp_forward_expression(dir, status)
            end if

            return
        end if

        ! This part handles the lispy feel of
        ! `python-nav-forward-sexp'.  Knowing everything about the
        ! current context and the context of the next expression tries to
        ! follow the lisp expression motion commands in a symmetric manner.
        context = expression_context(.false.)
        opoint = point()

        if (safe) then
            call nav_lisp_forward_expression_safe(dir)
        else
            call nav_lisp_forward_expression(dir, status)
        end if

        next_expression_pos = point()
        call goto_char(opoint)
        if (.not. status%ok()) return
        call goto_char(next_expression_pos)
        next_expression_context = expression_context(.true.)
        call goto_char(opoint)

        if (forward_p) then
            if (.not. is_eob() .and. is_python_info_current_line_empty()) then
                dummy = python_util_forward_comment(int(dir))
                call nav_forward_expression_1(dir, safe, skip_parens_p, status)
            else if (context == BLOCK_START) then
                p = python_nav_end_of_block()
            else if (context == STATEMENT_START) then
                p = python_nav_end_of_statement()
            else if ((context == STATEMENT_END .or. context == BLOCK_END) .and. next_expression_context == ENDS_BLOCK) then
                call goto_char(next_expression_pos)
                p = python_nav_end_of_block()
            else if ((context == STATEMENT_END .or. context == BLOCK_END) .and. next_expression_context == STARTS_BLOCK) then
                call goto_char(next_expression_pos)
                p = python_nav_end_of_block()
            else if (context == STATEMENT_END .or. context == BLOCK_END) then
                call goto_char(next_expression_pos)
                p = python_nav_end_of_statement()
            else
                call goto_char(next_expression_pos)
            end if
        else
            if (.not. is_bob() .and. is_python_info_current_line_empty()) then
                dummy = python_util_forward_comment(int(dir))
                call nav_forward_expression_1(dir, safe, skip_parens_p, status)
            else if (context == BLOCK_END) then
                p = python_nav_beginning_of_block()
            else if (context == STATEMENT_END) then
                p = python_nav_beginning_of_statement()
            else if ((context == STATEMENT_START .or. context == BLOCK_START) .and. next_expression_context == STARTS_BLOCK) then
                call goto_char(next_expression_pos)
                p = python_nav_beginning_of_block()
            else if ((context == STATEMENT_START .or. context == BLOCK_START) .and. next_expression_context == ENDS_BLOCK) then
                call goto_char(next_expression_pos)
                p = python_nav_beginning_of_block()
            else if (context == STATEMENT_START .or. context == BLOCK_START) then
                call goto_char(next_expression_pos)
                p = python_nav_beginning_of_statement()
            else
                call goto_char(next_expression_pos)
            end if
        end if
    end subroutine nav_forward_expression_1

    !> Move forward across expressions.
    !> With ARG, do it that many times.  Negative arg -N means move
    !> backward N times.  When optional argument SAFE is true, no error
    !> condition occurs at the end of an expression: it is skipped.  With optional
    !> argument SKIP_PARENS_P force expression motion to ignore parenthesized
    !> expressions when looking at them in either direction (forced to true
    !> in interactive calls).  Port of python.el:python-nav-forward-sexp
    !> (the default `python_forward_expression_function').
    recursive subroutine python_nav_forward_expression(arg_in, status, safe, skip_parens_p)
        integer(int64), intent(in) :: arg_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: safe, skip_parens_p
        integer(int64) :: arg
        logical :: sf, skip

        status = status_type()
        arg = arg_in
        sf = .false.
        if (present(safe)) sf = safe
        skip = .false.
        if (present(skip_parens_p)) skip = skip_parens_p

        ! Do not follow parens on interactive calls.  This hack to detect
        ! if the function was called interactively copes with the way
        ! `forward-sexp' works by calling `forward-sexp-function', losing
        ! interactive detection by checking `current-prefix-arg'.  The
        ! reason to make this distinction is that lisp functions like
        ! `blink-matching-open' get confused causing issues like the one in
        ! Bug#16191.  With this approach the user gets a symmetric behavior
        ! when working interactively while called functions expecting
        ! paren-based expression motion work just fine.
        if (.not. skip) then
        skip = value_eq(real_this_command, sym("forward_expression")) .or. value_eq(real_this_command, sym("backward_expression")) &
                   .or. value_eq(real_this_command, sym("python_nav_forward_expression")) &
                   .or. value_eq(real_this_command, sym("python_nav_backward_expression")) &
                   .or. value_eq(real_this_command, sym("python_nav_forward_expression_safe"))
        end if

        do while (arg > 0)
            call nav_forward_expression_1(1_int64, sf, skip, status)
            if (.not. status%ok()) return
            arg = arg - 1
        end do

        do while (arg < 0)
            call nav_forward_expression_1(-1_int64, sf, skip, status)
            if (.not. status%ok()) return
            arg = arg + 1
        end do
    end subroutine python_nav_forward_expression

    ! (python-nav-forward-sexp ARG) as the default
    ! `python_forward_expression_function'.
    ! Not a port.
    recursive subroutine nav_forward_expression_default(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call python_nav_forward_expression(arg, status)
    end subroutine nav_forward_expression_default

    !> Move backward across expressions.
    !> With ARG, do it that many times.  Negative arg -N means move
    !> forward N times.  When optional argument SAFE is true, no error
    !> condition occurs at the end of an expression: it is skipped.  With optional
    !> argument SKIP_PARENS_P force expression motion to ignore parenthesized
    !> expressions when looking at them in either direction (forced to true
    !> in interactive calls).  Port of python.el:python-nav-backward-sexp.
    recursive subroutine python_nav_backward_expression(arg, status, safe, skip_parens_p)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: safe, skip_parens_p

        call python_nav_forward_expression(-arg, status, safe, skip_parens_p)
    end subroutine python_nav_backward_expression

    !> Move forward safely across expressions.
    !> With ARG, do it that many times.  Negative arg -N means move
    !> backward N times.  With optional argument SKIP_PARENS_P force
    !> expression motion to ignore parenthesized expressions when looking at
    !> them in either direction (forced to true in interactive calls).
    !> Port of python.el:python-nav-forward-sexp-safe.
    recursive subroutine python_nav_forward_expression_safe(arg, status, skip_parens_p)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: skip_parens_p

        call python_nav_forward_expression(arg, status, .true., skip_parens_p)
    end subroutine python_nav_forward_expression_safe

    !> Move backward safely across expressions.
    !> With ARG, do it that many times.  Negative arg -N means move
    !> forward N times.  With optional argument SKIP_PARENS_P force expression
    !> motion to ignore parenthesized expressions when looking at them in
    !> either direction (forced to true in interactive calls).  Port of
    !> python.el:python-nav-backward-sexp-safe.
    recursive subroutine python_nav_backward_expression_safe(arg, status, skip_parens_p)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: skip_parens_p

        call python_nav_backward_expression(arg, status, .true., skip_parens_p)
    end subroutine python_nav_backward_expression_safe

    ! Internal implementation of `python_nav_up_group'.
    ! DIR is always 1 or -1 and comes sanitized from
    ! `python_nav_up_group' calls.  Port of python.el:python-nav--up-list.
    recursive subroutine nav_up_group_1(dir, status)
        integer(int64), intent(in) :: dir
        type(status_type), intent(out) :: status
        integer(int64) :: parent_end_pos, opoint, indentation, prev_block_pos, p
        integer :: context
        logical :: forward_p

        status = status_type()
        context = python_syntax_context_type()
        forward_p = dir > 0

        if (context == PYTHON_CONTEXT_STRING .or. context == PYTHON_CONTEXT_COMMENT) then
            continue
        else if (context == PYTHON_CONTEXT_PAREN) then
            call up_group_default(dir, status)
        else if (forward_p .and. is_python_info_end_of_block()) then
            opoint = point()
            indentation = 0
            if (python_nav_beginning_of_block() > 0) indentation = current_indentation()

            do while (indentation > 0)
                if (.not. current_indentation() >= indentation) exit
                if (python_nav_backward_block() == 0) exit
            end do

            parent_end_pos = python_nav_end_of_block()
            call goto_char(opoint)
            if (parent_end_pos > point()) call goto_char(parent_end_pos)
        else if (forward_p) then
            p = python_nav_end_of_block()
        else if (.not. forward_p .and. current_indentation() > 0 .and. is_python_info_beginning_of_block()) then
            opoint = point()
            indentation = current_indentation()

            do
                if (python_nav_backward_block() == 0) exit
                if (.not. current_indentation() >= indentation) exit
            end do

            prev_block_pos = point()
            call goto_char(opoint)
            if (point() > prev_block_pos) call goto_char(prev_block_pos)
        else if (.not. forward_p) then
            p = python_nav_beginning_of_block()
        end if
    end subroutine nav_up_group_1

    ! (let ((forward-sexp-function)) (up-list DIR)): up-list-default-function
    ! without a `forward_expression_function'.
    ! Not a port.
    recursive subroutine up_group_default(dir, status)
        integer(int64), intent(in) :: dir
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        pos = scan_groups(point(), dir, 1_int64, status)
        if (.not. status%ok()) return

        if (pos == 0) then
            if (dir > 0) then
                pos = point_max()
            else
                pos = point_min()
            end if
        end if

        call goto_char(pos)
    end subroutine up_group_default

    !> Move forward out of one level of parentheses (or blocks).
    !> With ARG, do this that many times.
    !> A negative argument means move backward but still to a less deep spot.
    !> This command assumes point is not in a string or comment.  Port of
    !> python.el:python-nav-up-list.
    recursive subroutine python_nav_up_group(arg_in, status)
        integer(int64), intent(in) :: arg_in
        type(status_type), intent(out) :: status
        integer(int64) :: arg

        status = status_type()
        arg = arg_in

        do while (arg > 0)
            call nav_up_group_1(1_int64, status)
            if (.not. status%ok()) return
            arg = arg - 1
        end do

        do while (arg < 0)
            call nav_up_group_1(-1_int64, status)
            if (.not. status%ok()) return
            arg = arg + 1
        end do
    end subroutine python_nav_up_group

    !> Move backward out of one level of parentheses (or blocks).
    !> With ARG, do this that many times.
    !> A negative argument means move forward but still to a less deep spot.
    !> This command assumes point is not in a string or comment.  Port of
    !> python.el:python-nav-backward-up-list.
    recursive subroutine python_nav_backward_up_group(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call python_nav_up_group(-arg, status)
    end subroutine python_nav_backward_up_group

    !> Move point at the beginning the __main__ block.
    !> When "if __name__ == '__main__':" is found returns its
    !> position, else returns 0.  Port of python.el:python-nav-if-name-main.
    recursive integer(int64) function python_nav_if_name_main() result(pos)
        integer(int64) :: opoint, f
        type(status_type) :: st

        opoint = point()
        call goto_char(point_min())
        pos = 0

        do
            call re_search_forward(IF_NAME_MAIN_RE, f, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) exit

            if (python_syntax_context_type() == 0) then
                call beginning_of_line()
                pos = point()
                return
            end if
        end do

        call goto_char(opoint)
    end function python_nav_if_name_main

    ! ------------------------------------------------------------------
    ! Misc helpers
    ! ------------------------------------------------------------------

    !> Return name of surrounding function with Python compatible dotty syntax
    !> (NAME unallocated if point is not inside a subprogram).
    !> Optional argument INCLUDE_TYPE indicates to include the type of the subprogram.
    !> This function can be used as the value of `add_log_current_subprogram_function'
    !> since it returns nothing if point is not inside a subprogram.  Port of
    !> python.el:python-info-current-defun.
    recursive subroutine python_info_current_definition(name, include_type)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(in), optional :: include_type
        type(string_list_type), allocatable :: names(:)
        character(len=:), allocatable :: type, matched, first, second
        integer(int64) :: opoint, begv, zv_from_end, starting_indentation, starting_pos, last_indent, min_indent, &
                          ending, p
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: first_run, has_last, accept, incl, moved
        integer :: k

        incl = .false.
        if (present(include_type)) incl = include_type
        ! (save-restriction (widen) (save-excursion ...))
        opoint = point()
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call end_of_line(1_int64)
        allocate (names(0))
        starting_indentation = current_indentation()
        starting_pos = point()
        first_run = .true.
        has_last = .false.
        last_indent = 0

        do
            if (.not. nav_beginning_of_subprogram(1_int64)) exit
            md = match_data()
            accept = .not. has_last
            if (.not. accept) accept = current_indentation() < last_indent

            if (accept) then
                accept = .false.

                if (first_run) then
                    ! If this is the first run, we may add
                    ! the current subprogram at point.
                    first_run = .false.
                    p = point()
                    call goto_char(starting_pos)
                    ending = python_nav_beginning_of_statement()
                    call beginning_of_line(1_int64)
                    accept = looking_at(python_nav_beginning_of_definition_regexp, st)
                    call goto_char(p)
                end if

                if (.not. accept) then
                    p = point()
                    min_indent = current_indentation() + python_indent_offset%value

                    if (starting_indentation < min_indent) then
                        ! If the starting indentation is not
                        ! within the min subprogram indent make the
                        ! check fail.
                        ending = starting_pos
                    else
                        ! Else go to the end of subprogram and add
                        ! up the current indentation to the
                        ! ending position.
                        moved = python_nav_end_of_definition()

                        if (current_indentation() >= min_indent) then
                            ending = point() + 1 + current_indentation()
                        else
                            ending = point()
                        end if
                    end if

                    call goto_char(p)
                    accept = starting_pos < ending
                end if
            end if

            call set_match_data(md)

            if (accept) then
                last_indent = current_indentation()
                has_last = .true.

                if (.not. incl .or. allocated(type)) then
                    call push_name(match_string_no_properties(1))
                else
                    matched = match_string_no_properties(0)
                    call split2(matched, first, second)
                    type = first
                    call push_name(second)
                end if
            end if

            ! Stop searching ASAP.
            if (current_indentation() == 0) exit
        end do

        if (size(names) > 0) then
            name = ""
            if (allocated(type)) name = type//" "

            do k = 1, size(names)
                if (k > 1) name = name//"."
                name = name//names(k)%s
            end do
        end if

        call goto_char(opoint)
        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)

    contains

        subroutine push_name(n)
            character(len=*), intent(in) :: n
            type(string_list_type), allocatable :: grown(:)
            integer :: j

            allocate (grown(size(names) + 1))
            grown(1)%s = n

            do j = 1, size(names)
                grown(j + 1) = names(j)
            end do

            call move_alloc(grown, names)
        end subroutine push_name

        ! The first two words of S (split-string at whitespace).
        subroutine split2(s, w1, w2)
            character(len=*), intent(in) :: s
            character(len=:), allocatable, intent(out) :: w1, w2
            integer :: i, b

            i = 1

            do while (i <= len(s))
                if (s(i:i) /= " " .and. s(i:i) /= TAB .and. s(i:i) /= LF) exit
                i = i + 1
            end do

            b = i

            do while (i <= len(s))
                if (s(i:i) == " " .or. s(i:i) == TAB .or. s(i:i) == LF) exit
                i = i + 1
            end do

            w1 = s(b:i - 1)

            do while (i <= len(s))
                if (s(i:i) /= " " .and. s(i:i) /= TAB .and. s(i:i) /= LF) exit
                i = i + 1
            end do

            b = i

            do while (i <= len(s))
                if (s(i:i) == " " .or. s(i:i) == TAB .or. s(i:i) == LF) exit
                i = i + 1
            end do

            w2 = s(b:i - 1)
        end subroutine split2
    end subroutine python_info_current_definition

    !> Return true if current statement opens a block.  Port of
    !> python.el:python-info-statement-starts-block-p.
    recursive logical function is_python_info_statement_starts_block() result(starts)
        integer(int64) :: opoint, p
        type(status_type) :: st

        opoint = point()
        p = python_nav_beginning_of_statement()
        starts = looking_at(BLOCK_START_RE, st)
        call goto_char(opoint)
    end function is_python_info_statement_starts_block

    !> Return true if point is at end of block.  Port of
    !> python.el:python-info-statement-ends-block-p.
    recursive logical function is_python_info_statement_ends_block() result(ends)
        integer(int64) :: opoint, current_statement, current_ind, next_statement, next_ind, end_of_statement_pos, &
                          end_of_block_pos
        logical :: dummy

        opoint = point()
        current_statement = python_nav_beginning_of_statement()
        current_ind = current_indentation()
        call goto_char(opoint)
        call python_nav_forward_statement(1_int64)
        next_statement = point()
        next_ind = current_indentation()
        call goto_char(opoint)
        ends = .false.
        if (current_statement < next_statement .and. current_ind <= next_ind) return
        end_of_statement_pos = python_nav_end_of_statement()
        dummy = python_util_forward_comment(-1)
        end_of_statement_pos = point()
        call goto_char(opoint)
        end_of_block_pos = python_nav_end_of_block()
        call goto_char(opoint)
        ends = end_of_block_pos /= 0 .and. end_of_block_pos == end_of_statement_pos
    end function is_python_info_statement_ends_block

    !> Return true if point is at beginning of statement.  Port of
    !> python.el:python-info-beginning-of-statement-p.
    recursive logical function is_python_info_beginning_of_statement() result(p)
        integer(int64) :: opoint, b

        opoint = point()
        b = python_nav_beginning_of_statement()
        call goto_char(opoint)
        p = opoint == b
    end function is_python_info_beginning_of_statement

    !> Return true if point is at end of statement.  Port of
    !> python.el:python-info-end-of-statement-p.
    recursive logical function is_python_info_end_of_statement() result(p)
        integer(int64) :: opoint, e

        opoint = point()
        e = python_nav_end_of_statement()
        call goto_char(opoint)
        p = opoint == e
    end function is_python_info_end_of_statement

    !> Return true if point is at beginning of block.  Port of
    !> python.el:python-info-beginning-of-block-p.
    recursive logical function is_python_info_beginning_of_block() result(p)
        p = is_python_info_beginning_of_statement()
        if (p) p = is_python_info_statement_starts_block()
    end function is_python_info_beginning_of_block

    !> Return true if point is at end of block.  Port of
    !> python.el:python-info-end-of-block-p.
    recursive logical function is_python_info_end_of_block() result(p)
        integer(int64) :: opoint, e
        logical :: dummy

        opoint = point()
        e = python_nav_end_of_statement()
        dummy = python_util_forward_comment(-1)
        e = point()
        call goto_char(opoint)
        p = opoint == e
        if (p) p = is_python_info_statement_ends_block()
    end function is_python_info_end_of_block

    !> Return the point of the closest block the current line closes.
    !> Returns 0 if point is not on a dedenter statement or no opening
    !> block can be detected.  The latter case meaning current file is
    !> likely an invalid python file.  Port of
    !> python.el:python-info-dedenter-opening-block-position.
    recursive integer(int64) function python_info_dedenter_opening_block_position() result(position)
        integer(int64), allocatable :: positions(:)
        integer(int64) :: indentation, opoint
        integer :: k

        positions = python_info_dedenter_opening_block_positions()
        indentation = current_indentation()
        position = 0
        opoint = point()

        do k = 1, size(positions)
            call goto_char(positions(k))

            if (current_indentation() <= indentation) then
                position = positions(k)
                exit
            end if
        end do

        call goto_char(opoint)
    end function python_info_dedenter_opening_block_position

    ! The motion of `python_info_dedenter_opening_block_positions': search
    ! back for a block start at the start of its line.
    ! Port of python.el:python-info-dedenter-opening-block-positions (in part).
    recursive logical function search_block_start_at_bol() result(found)
        integer(int64) :: f
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: at_bol

        found = .false.

        do
            call re_search_backward(BLOCK_START_RE, f, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) return
            md = match_data()
            at_bol = looking_back(LINE_START_WS_RE, st, line_beginning_position())
            call set_match_data(md)

            if (at_bol) then
                found = .true.
                return
            end if
        end do
    end function search_block_start_at_bol

    !> Return points of blocks the current line may close sorted by closer.
    !> Returns an empty array if point is not on a dedenter statement or no
    !> opening block can be detected.  The latter case meaning current file
    !> is likely an invalid python file.  Port of
    !> python.el:python-info-dedenter-opening-block-positions.
    recursive function python_info_dedenter_opening_block_positions() result(opening_blocks)
        integer(int64), allocatable :: opening_blocks(:)
        integer(int64), allocatable :: collected(:), md(:)
        character(len=:), allocatable :: dedenter, keyword
        integer(int64) :: opoint, dedenter_pos, cur_line, indentation, p, shortage
        logical :: no_back_indent, take

        allocate (opening_blocks(0), collected(0))
        opoint = point()
        dedenter_pos = is_python_info_dedenter_statement()

        if (dedenter_pos > 0) then
            call goto_char(dedenter_pos)
            cur_line = line_beginning_position()
            dedenter = match_string_no_properties(0)

            do
                if (nav_syntactically(search_block_start_at_bol, -1) == 0) exit
                indentation = current_indentation()
                take = .not. any(collected == indentation)
                if (take .and. size(collected) > 0) take = indentation < minval(collected)

                if (take) then
                    ! There must be no line with indentation
                    ! smaller than `indentation' (except for
                    ! blank lines) between the found opening
                    ! block and the current line, otherwise it
                    ! is not an opening block.
                    p = point()
                    md = match_data()
                    shortage = python_nav_end_of_statement()
                    shortage = forward_line(1_int64)
                    no_back_indent = .true.

                    do while (point() < cur_line)
                        no_back_indent = current_indentation() > indentation .or. is_python_info_current_line_empty() &
                                         .or. is_python_info_current_line_comment()
                        if (.not. no_back_indent) exit
                        shortage = forward_line(1_int64)
                    end do

                    call set_match_data(md)
                    call goto_char(p)
                    take = no_back_indent
                end if

                if (take) then
                    collected = [indentation, collected]
                    keyword = match_string_no_properties(0)
                    if (opens(dedenter, keyword)) opening_blocks = [point(), opening_blocks]
                end if

                if (indentation == 0) exit
            end do

            ! sort by closer
            opening_blocks = opening_blocks(size(opening_blocks):1:-1)
        end if

        call goto_char(opoint)

    contains

        ! Whether KEYWORD is one of the blocks DEDENTER may close.
        logical function opens(dedenter, keyword)
            character(len=*), intent(in) :: dedenter, keyword

            select case (dedenter)
            case ("elif")
                opens = keyword == "elif" .or. keyword == "if"
            case ("else")
                opens = keyword == "if" .or. keyword == "elif" .or. keyword == "except" .or. keyword == "for" &
                        .or. keyword == "while"
            case ("except")
                opens = keyword == "except" .or. keyword == "try"
            case ("finally")
                opens = keyword == "else" .or. keyword == "except" .or. keyword == "try"
            case ("case")
                opens = keyword == "case"
            case default
                opens = .false.
            end select
        end function opens
    end function python_info_dedenter_opening_block_positions

    !> Message the first line of the block the current statement closes.
    !> Port of python.el:python-info-dedenter-opening-block-message.
    recursive subroutine python_info_dedenter_opening_block_message()
        integer(int64) :: pos, opoint
        character(len=:), allocatable :: text
        type(status_type) :: st

        pos = python_info_dedenter_opening_block_position()
        if (pos == 0) return
        opoint = point()
        call goto_char(pos)
        call buffer_substring(point(), line_end_position(), text, st)
        call goto_char(opoint)
        call message("Closes "//text)
    end subroutine python_info_dedenter_opening_block_message

    !> Return point if current statement is a dedenter (0 if not).
    !> Sets `match_data' to the keyword that starts the dedenter
    !> statement.  Port of python.el:python-info-dedenter-statement-p.
    recursive integer(int64) function is_python_info_dedenter_statement() result(pos)
        integer(int64) :: opoint, p, q
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: first_case, dummy

        opoint = point()
        pos = 0
        p = python_nav_beginning_of_statement()

        if (python_syntax_context_type() == 0) then
            if (looking_at(DEDENTER_RE, st)) then
                ! Exclude the first "case" in the block.
                first_case = .false.

                if (match_string_no_properties(0) == "case") then
                    md = match_data()
                    q = point()
                    call back_to_indentation()
                    dummy = python_util_forward_comment(-1)
                    first_case = before_char_at(point()) == iachar(":")
                    call goto_char(q)
                    call set_match_data(md)
                end if

                if (.not. first_case) pos = point()
            end if
        end if

        call goto_char(opoint)
    end function is_python_info_dedenter_statement

    !> Return the backslash position if current line ends with backslash
    !> (0 if not).  With optional argument LINE_NUMBER, check that line
    !> instead.  Port of python.el:python-info-line-ends-backslash-p.
    recursive integer(int64) function is_python_info_line_ends_backslash(line_number) result(pos)
        integer(int64), intent(in), optional :: line_number
        integer(int64) :: opoint, shortage

        opoint = point()
        if (present(line_number)) call python_util_goto_line(line_number)

        do
            if (is_eob()) exit
            call goto_char(line_end_position())
            if (python_syntax_context(PYTHON_CONTEXT_PAREN) == 0) exit
            if (before_char_at(point()) == iachar("\")) exit
            shortage = forward_line(1_int64)
        end do

        pos = 0
        if (before_char_at(point()) == iachar("\")) pos = point()
        call goto_char(opoint)
    end function is_python_info_line_ends_backslash

    !> Return the point where the backslashed line starts (0 if not).
    !> Optional argument LINE_NUMBER forces the line number to check against.
    !> Port of python.el:python-info-beginning-of-backslash.
    recursive integer(int64) function python_info_beginning_of_backslash(line_number) result(pos)
        integer(int64), intent(in), optional :: line_number
        integer(int64) :: opoint, shortage, q
        logical :: in_paren

        opoint = point()
        pos = 0
        if (present(line_number)) call python_util_goto_line(line_number)

        if (is_python_info_line_ends_backslash() > 0) then
            do
                q = point()
                call goto_char(line_beginning_position())
                in_paren = python_syntax_context(PYTHON_CONTEXT_PAREN) > 0
                call goto_char(q)
                if (.not. in_paren) exit
                shortage = forward_line(-1_int64)
            end do

            call back_to_indentation()
            pos = point()
        end if

        call goto_char(opoint)
    end function python_info_beginning_of_backslash

    !> Check if current line is continuation of another.
    !> When current line is continuation of another return the point
    !> where the continued line ends (0 if not).  Port of
    !> python.el:python-info-continuation-line-p.
    recursive integer(int64) function is_python_info_continuation_line() result(pos)
        integer(int64) :: opoint, line_start, context_start
        integer :: context_type
        logical :: dummy

        opoint = point()
        pos = 0
        call back_to_indentation()
        context_type = python_syntax_context_type()
        line_start = line_number_at_pos()
        context_start = 0
        if (context_type /= 0) context_start = python_syntax_context(context_type)

        if (context_type == PYTHON_CONTEXT_PAREN) then
            ! Lines inside a paren are always a continuation line
            ! (except the first one).
            dummy = python_util_forward_comment(-1)
            pos = point()
        else if (context_type == PYTHON_CONTEXT_STRING .or. context_type == PYTHON_CONTEXT_COMMENT) then
            ! move forward an roll again
            call goto_char(context_start)
            dummy = python_util_forward_comment()
            pos = is_python_info_continuation_line()
        else
            ! Not within a paren, string or comment, the only way
            ! we are dealing with a continuation line is that
            ! previous line contains a backslash, and this can
            ! only be the previous line from current
            call back_to_indentation()
            dummy = python_util_forward_comment(-1)

            if (line_start - 1 == line_number_at_pos()) then
                if (is_python_info_line_ends_backslash() > 0) pos = point()
            end if
        end if

        call goto_char(opoint)
    end function is_python_info_continuation_line

    !> Return the start of the block if current line is a continuation of
    !> a block (0 if not).  Port of python.el:python-info-block-continuation-line-p.
    recursive integer(int64) function is_python_info_block_continuation_line() result(pos)
        integer(int64) :: opoint, shortage
        type(status_type) :: st

        opoint = point()
        pos = 0

        if (is_python_info_continuation_line() > 0) then
            shortage = forward_line(-1_int64)
            call back_to_indentation()
            if (looking_at(BLOCK_START_RE, st)) pos = point()
        end if

        call goto_char(opoint)
    end function is_python_info_block_continuation_line

    !> Check if current line is an assignment (the point after the
    !> operator, 0 if not).
    !> With argument CURRENT_LINE_ONLY is true, don't follow any
    !> continuations, just check the if current line is an assignment.
    !> Port of python.el:python-info-assignment-statement-p.
    recursive integer(int64) function is_python_info_assignment_statement(current_line_only) result(pos)
        logical, intent(in), optional :: current_line_only
        integer(int64) :: opoint, f, p, q
        type(status_type) :: st
        logical :: found, only

        opoint = point()
        only = .false.
        if (present(current_line_only)) only = current_line_only
        found = .false.
        pos = 0

        if (only) then
            call back_to_indentation()
        else
            p = python_nav_beginning_of_statement()
        end if

        do while (.not. found)
            call re_search_forward(ASSIGNMENT_STATEMENT_RE, f, st, bound=line_end_position(), noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) exit
            ! The assignment operator should not be inside a string.
            q = point()
            call goto_char(point() - len(match_string_no_properties(1)))
            found = python_syntax_context_type() == 0
            call goto_char(q)
        end do

        if (found) then
            p = skip_syntax_forward(" ")
            pos = point()
        end if

        call goto_char(opoint)
    end function is_python_info_assignment_statement

    !> Check if current line is the first continuation of an assignment.
    !> When current line is continuation of another with an assignment
    !> return the point of the first non-blank character after the
    !> operator (0 if not).  Port of
    !> python.el:python-info-assignment-continuation-line-p.
    recursive integer(int64) function is_python_info_assignment_continuation_line() result(pos)
        integer(int64) :: opoint, shortage

        opoint = point()
        pos = 0

        if (is_python_info_continuation_line() > 0) then
            shortage = forward_line(-1_int64)
            pos = is_python_info_assignment_statement(.true.)
        end if

        call goto_char(opoint)
    end function is_python_info_assignment_continuation_line

    !> Check if point is at `beginning_of_subprogram' using SYNTAX_PPSS.
    !> When CHECK_STATEMENT is true, the current statement is checked
    !> instead of the current physical line.  Port of
    !> python.el:python-info-looking-at-beginning-of-defun.
    recursive logical function python_info_looking_at_beginning_of_definition(syntax_ppss_state, check_statement) result(p)
        type(parse_state_type), intent(in), optional :: syntax_ppss_state
        logical, intent(in), optional :: check_statement
        integer(int64) :: opoint, q
        type(status_type) :: st
        logical :: check

        opoint = point()
        check = .false.
        if (present(check_statement)) check = check_statement
        if (check) q = python_nav_beginning_of_statement()
        call beginning_of_line(1_int64)

        if (present(syntax_ppss_state)) then
            p = python_syntax_context_type(syntax_ppss_state) == 0
        else
            p = python_syntax_context_type() == 0
        end if

        if (p) p = looking_at(python_nav_beginning_of_definition_regexp, st)
        call goto_char(opoint)
    end function python_info_looking_at_beginning_of_definition

    !> Check if point is at the beginning of block.  Port of
    !> python.el:python-info-looking-at-beginning-of-block.
    recursive logical function python_info_looking_at_beginning_of_block() result(p)
        integer(int64) :: pos, q
        type(status_type) :: st

        pos = point()
        q = python_nav_beginning_of_statement()
        call beginning_of_line()
        p = point() <= pos .and. pos <= point() + current_indentation()
        if (p) p = looking_at(python_nav_beginning_of_block_regexp, st)
        call goto_char(pos)
    end function python_info_looking_at_beginning_of_block

    !> Return true if current line is a comment line.  Port of
    !> python.el:python-info-current-line-comment-p.
    recursive logical function is_python_info_current_line_comment()
        is_python_info_current_line_comment = after_char_at(line_beginning_position() + current_indentation()) &
                                              == iachar("#")
    end function is_python_info_current_line_comment

    !> Return true if current line is empty, ignoring whitespace.  Port of
    !> python.el:python-info-current-line-empty-p.
    recursive logical function is_python_info_current_line_empty() result(empty)
        integer(int64) :: opoint
        type(status_type) :: st
        logical :: matched

        opoint = point()
        call beginning_of_line(1_int64)
        matched = looking_at(EMPTY_LINE_RE, st)
        empty = len(match_string_no_properties(1)) == 0
        call goto_char(opoint)
    end function is_python_info_current_line_empty

    !> Return true if point is in a docstring.
    !> When optional argument SYNTAX_PPSS is given, use that instead of
    !> point's current `syntax_ppss'.  Port of python.el:python-info-docstring-p.
    recursive logical function is_python_info_docstring(syntax_ppss_state) result(docstring)
        type(parse_state_type), intent(in), optional :: syntax_ppss_state
        character(len=*), parameter :: RE = "[uU]?[rR]?[""']"
        integer(int64) :: opoint, counter, indentation, backward_expression_point, last_backward_expression_point, p
        type(status_type) :: st
        logical :: dummy, more, has_last

        opoint = point()
        docstring = .false.

        if (present(syntax_ppss_state)) then
            if (python_syntax_context(PYTHON_CONTEXT_STRING, syntax_ppss_state) > 0) then
                call goto_char(syntax_ppss_state%comstr_start)
            end if
        end if

        p = python_nav_beginning_of_statement()
        counter = 1
        indentation = current_indentation()

        if (is_python_info_assignment_statement() == 0 .and. looking_at(RE, st)) then
            ! Allow up to two consecutive docstrings only.
            has_last = .false.
            last_backward_expression_point = 0

            do while (counter <= 2)
                p = point()
                call python_nav_backward_expression(1_int64, st)
                backward_expression_point = point()
                more = st%ok() .and. indentation == current_indentation()

                if (more) then
                    ! Make sure we're always moving point.
                    ! If we get stuck in the same position
                    ! on consecutive loop iterations,
                    ! bail out.
                    more = .not. (has_last .and. last_backward_expression_point == backward_expression_point)
                    last_backward_expression_point = backward_expression_point
                    has_last = .true.
                    if (more) more = looking_at(RE, st)
                end if

                call goto_char(p)
                if (.not. more) exit
                ! Previous expression was a string, restore point.
                call goto_char(backward_expression_point)
                counter = counter + 1
            end do

            if (counter <= 2) then
                dummy = python_util_forward_comment(-1)
                p = python_nav_beginning_of_statement()

                if (is_bob()) then
                    p = point()
                    dummy = python_util_forward_comment()
                    docstring = looking_at(RE, st)
                    call goto_char(p)
                end if

                if (.not. docstring) docstring = is_python_info_assignment_statement() > 0
                if (.not. docstring) docstring = python_info_looking_at_beginning_of_definition()
            end if
        end if

        call goto_char(opoint)
    end function is_python_info_docstring

    !> Check if point is in a triple quoted string including quotes.
    !> It returns the position of the third quote character of the start
    !> of the string (0 if not).  Port of python.el:python-info-triple-quoted-string-p.
    recursive integer(int64) function is_python_info_triple_quoted_string() result(found)
        integer, parameter :: OFFSETS(6) = [0, 3, -2, 2, -1, 1]
        integer(int64) :: pos, check_pos
        type(parse_state_type) :: ppss
        integer :: k

        pos = point()
        found = 0

        do k = 1, size(OFFSETS)
            check_pos = pos + OFFSETS(k)

            if (check_pos >= point_min() .and. check_pos <= point_max()) then
                ppss = ppss_at(check_pos)
                found = python_syntax_context(PYTHON_CONTEXT_TRIPLE_QUOTED_STRING, ppss)
                if (found > 0) exit
            end if
        end do

        call goto_char(pos)
    end function is_python_info_triple_quoted_string

    !> Detect current buffer's encoding from its coding cookie
    !> (ENCODING unallocated if there is none).  Port of
    !> python.el:python-info-encoding-from-cookie.
    recursive subroutine python_info_encoding_from_cookie(encoding)
        character(len=:), allocatable, intent(out) :: encoding
        character(len=:), allocatable :: first_two_lines
        integer(int64) :: opoint, begv, zv_from_end, shortage
        type(status_type) :: st

        opoint = point()
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call goto_char(point_min())
        shortage = forward_line(2_int64)
        call buffer_substring(point_min(), point(), first_two_lines, st)
        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
        call goto_char(opoint)
        if (string_match(CODING_COOKIE_RE, first_two_lines, st) > 0) encoding = match_string_no_properties(1, &
                                                                                                           first_two_lines)
    end subroutine python_info_encoding_from_cookie

    !> Return encoding for file.
    !> Try `python_info_encoding_from_cookie', if none is found then
    !> default to utf-8.  Port of python.el:python-info-encoding.
    recursive function python_info_encoding() result(encoding)
        character(len=:), allocatable :: encoding

        ! If no encoding is defined, then it's safe to use UTF-8: Python 2
        ! uses ASCII as default while Python 3 uses UTF-8.  This means that
        ! in the worst case scenario python.el will make things work for
        ! Python 2 files with unicode data and no encoding defined.
        call python_info_encoding_from_cookie(encoding)
        if (.not. allocated(encoding)) encoding = "utf-8"
    end function python_info_encoding

    ! ------------------------------------------------------------------
    ! Utility functions
    ! ------------------------------------------------------------------

    !> Move point to LINE_NUMBER.  Port of python.el:python-util-goto-line.
    recursive subroutine python_util_goto_line(line_number)
        integer(int64), intent(in) :: line_number
        integer(int64) :: shortage

        call goto_char(point_min())
        shortage = forward_line(line_number - 1)
    end subroutine python_util_goto_line

    !> Python mode specific version of `forward_comment'.
    !> Optional argument DIRECTION defines the direction to move to.
    !> Port of python.el:python-util-forward-comment.
    recursive logical function python_util_forward_comment(direction) result(moved)
        integer, intent(in), optional :: direction
        integer(int64) :: comment_start_pos, factor

        comment_start_pos = python_syntax_context(PYTHON_CONTEXT_COMMENT)
        factor = 99999

        if (present(direction)) then
            if (direction < 0) factor = -99999
        end if

        if (comment_start_pos > 0) call goto_char(comment_start_pos)
        moved = forward_comment(factor)
    end function python_util_forward_comment

    ! The character after POS, -1 if none.
    ! Not a port.
    recursive integer function after_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        after_char_at = -1
        if (pos < point_min() .or. pos >= point_max()) return
        call char_after(c, found, pos)
        if (found) after_char_at = c
    end function after_char_at

    ! The character before POS, -1 if none.
    ! Not a port.
    recursive integer function before_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        before_char_at = -1
        if (pos <= point_min() .or. pos > point_max()) return
        call char_before(c, found, pos)
        if (found) before_char_at = c
    end function before_char_at

    ! ------------------------------------------------------------------
    ! Indentation
    ! ------------------------------------------------------------------

    !> Guess and set `python_indent_offset' for the current buffer.  Port of
    !> python.el:python-indent-guess-indent-offset.
    recursive subroutine python_indent_guess_indent_offset()
        integer(int64) :: opoint, begv, zv_from_end, f, block_end, indentation, shortage
        character(len=24) :: buf
        type(status_type) :: st
        logical :: is_end, dummy

        opoint = point()
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call goto_char(point_min())
        block_end = 0

        do while (block_end == 0)
            call re_search_forward(LINE_START_BLOCK_START_RE, f, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) exit
            if (python_syntax_context_type() /= 0) cycle
            call goto_char(line_end_position())
            dummy = python_util_forward_comment(-1)

            if (before_char_at(point()) == iachar(":")) then
                is_end = .true.
            else
                is_end = .false.
                shortage = forward_line(1_int64)

                if (is_python_info_block_continuation_line() > 0) then
                    do while (is_python_info_continuation_line() > 0 .and. .not. is_eob())
                        shortage = forward_line(1_int64)
                    end do

                    dummy = python_util_forward_comment(-1)
                    is_end = before_char_at(point()) == iachar(":")
                end if
            end if

            if (is_end) block_end = point()
        end do

        indentation = 0

        if (block_end > 0) then
            call goto_char(block_end)
            dummy = python_util_forward_comment()
            indentation = current_indentation()
        end if

        if (block_end > 0 .and. indentation /= 0) then
            call python_indent_offset%set_local(indentation)
        else if (python_indent_guess_indent_offset_verbose%value) then
            write (buf, "(i0)") python_indent_offset%value
            call message("Can't guess python_indent_offset, using defaults: "//trim(buf))
        end if

        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
        call goto_char(opoint)
    end subroutine python_indent_guess_indent_offset

    !> Get information about the current indentation context.
    !> Context is returned as STATUS and START (python.el's (STATUS .
    !> START)).
    !>
    !> STATUS can be one of the following:
    !>
    !> PYTHON_INDENT_AFTER_COMMENT
    !>  - Point is after a comment line.
    !>  - START is the position of the "#" character.
    !> PYTHON_INDENT_INSIDE_STRING
    !>  - Point is inside string.
    !>  - START is the position of the first quote that starts it.
    !> PYTHON_INDENT_NO_INDENT
    !>  - No possible indentation case matches.
    !>  - START is always zero.
    !>
    !> PYTHON_INDENT_INSIDE_PAREN
    !>  - Fallback case when point is inside paren.
    !>  - START is the first non space char position *after* the open paren.
    !> PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN
    !>  - Point is on a line that contains a nested paren closer.
    !>  - START is the position of the open paren it closes.
    !> PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN
    !>  - Point is on a line that contains a paren closer.
    !>  - START is the position of the open paren.
    !> PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START
    !>  - Point is inside a paren with items starting in their own line.
    !>  - START is the position of the open paren.
    !> PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK
    !>  - Point is inside a paren with items starting in their own line
    !>    from a block start.
    !>  - START is the position of the open paren.
    !> PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK
    !>  - Point is inside a paren from a block start followed by some
    !>    items on the same line.
    !>  - START is the first non space char position *after* the open paren.
    !> PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE
    !>  - Point is on a continuation line inside a paren.
    !>  - START is the position where the previous line (excluding lines
    !>    for inner parens) starts.
    !>
    !> PYTHON_INDENT_AFTER_BACKSLASH
    !>  - Fallback case when point is after backslash.
    !>  - START is the char after the position of the backslash.
    !> PYTHON_INDENT_AFTER_BACKSLASH_ASSIGNMENT_CONTINUATION
    !>  - Point is after a backslashed assignment.
    !>  - START is the char after the position of the backslash.
    !> PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION
    !>  - Point is after a backslashed block continuation.
    !>  - START is the char after the position of the backslash.
    !> PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION
    !>  - Point is after a backslashed dotted continuation.  Previous
    !>    line must contain a dot to align with.
    !>  - START is the char after the position of the backslash.
    !> PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE
    !>  - First line following a backslashed continuation.
    !>  - START is the char after the position of the backslash.
    !>
    !> PYTHON_INDENT_AFTER_BLOCK_END
    !>  - Point is after a line containing a block ender.
    !>  - START is the position where the ender starts.
    !> PYTHON_INDENT_AFTER_BLOCK_START
    !>  - Point is after a line starting a block.
    !>  - START is the position where the block starts.
    !> PYTHON_INDENT_AFTER_LINE
    !>  - Point is after a simple line.
    !>  - START is the position where the previous line starts.
    !> PYTHON_INDENT_AT_DEDENTER_BLOCK_START
    !>  - Point is on a line starting a dedenter block.
    !>  - START is the position where the dedenter block starts.
    !>
    !> Port of python.el:python-indent-context.
    recursive subroutine python_indent_context(kind, start)
        integer, intent(out) :: kind
        integer(int64), intent(out) :: start
        type(parse_state_type) :: ppss
        integer(int64) :: opoint, s, continuation_start, prev_start, after_start, l1, p, block_continuation_start, &
                          assignment_continuation_start, shortage, f
        type(status_type) :: st
        logical :: starts_in_newline, cond, dummy

        opoint = point()
        call beginning_of_line()
        ppss = ppss_here()
        call goto_char(opoint)
        start = 0

        ! Beginning of buffer.
        if (line_number_at_pos() == 1) then
            kind = PYTHON_INDENT_NO_INDENT
            return
        end if

        ! Inside a string.
        s = python_syntax_context(PYTHON_CONTEXT_STRING, ppss)

        if (s > 0) then
            if (is_python_info_docstring()) then
                kind = PYTHON_INDENT_INSIDE_DOCSTRING
            else
                kind = PYTHON_INDENT_INSIDE_STRING
            end if

            start = s
            call goto_char(opoint)
            return
        end if

        ! Inside a paren.
        s = python_syntax_context(PYTHON_CONTEXT_PAREN, ppss)

        if (s > 0) then
            call goto_char(s)
            call goto_char(point() + 1)
            l1 = line_number_at_pos()
            dummy = python_util_forward_comment()
            starts_in_newline = l1 /= line_number_at_pos()
            call goto_char(opoint)
            continuation_start = 0
            shortage = forward_line(-1_int64)
            call back_to_indentation()
            ! Skip inner parens.
            prev_start = python_syntax_context(PYTHON_CONTEXT_PAREN)

            do while (prev_start > 0 .and. prev_start >= s)
                if (prev_start == s) then
                    continuation_start = point()
                    exit
                end if

                call goto_char(prev_start)
                call back_to_indentation()
                prev_start = python_syntax_context(PYTHON_CONTEXT_PAREN)
            end do

            call goto_char(opoint)
            ! Current line only holds the closing paren.
            p = skip_syntax_forward(" ")
            cond = is_python_syntax_closing_paren()

            if (cond) then
                call goto_char(point() + 1)
                cond = python_syntax_context(PYTHON_CONTEXT_PAREN) == 0
            end if

            call goto_char(opoint)

            if (cond) then
                kind = PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN
                start = s
                return
            end if

            ! Current line only holds a closing paren for nested.
            call back_to_indentation()
            cond = is_python_syntax_closing_paren()
            call goto_char(opoint)

            if (cond) then
                kind = PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN
                start = s
                return
            end if

            ! This line is a continuation of the previous line.
            if (continuation_start > 0) then
                kind = PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE
                start = continuation_start
                return
            end if

            ! This line starts from an opening block in its own line.
            if (starts_in_newline) then
                call goto_char(s)
                call back_to_indentation()
                cond = looking_at(BLOCK_START_RE, st)
                call goto_char(opoint)

                if (cond) then
                    kind = PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK
                else
                    kind = PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START
                end if

                start = s
                return
            end if

            ! General case.
            call goto_char(s + 1)
            p = skip_syntax_forward("(", 1_int64)
            p = skip_syntax_forward(" ")
            after_start = point()
            call goto_char(opoint)
            p = python_nav_beginning_of_statement()
            cond = python_info_looking_at_beginning_of_block()
            call goto_char(opoint)

            if (cond) then
                kind = PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK
            else
                kind = PYTHON_INDENT_INSIDE_PAREN
            end if

            start = after_start
            return
        end if

        ! After backslash.
        s = 0
        if (is_python_syntax_comment_or_string(ppss) == 0) s = is_python_info_line_ends_backslash(line_number_at_pos() - 1)

        if (s > 0) then
            ! Continuation of dotted expression.
            call back_to_indentation()

            if (after_char_at(point()) == iachar(".")) then
                ! Move point back until it's not inside a paren.
                do
                    shortage = forward_line(-1_int64)
                    if (is_bob()) exit
                    if (python_syntax_context(PYTHON_CONTEXT_PAREN) == 0) exit
                end do

                call goto_char(line_end_position())

                do
                    call search_backward(".", f, st, bound=line_beginning_position(), noerror=SEARCH_NOERROR)
                    if (.not. st%ok() .or. f <= 0) exit
                    if (python_syntax_context_type() == 0) exit
                end do

                ! Ensure previous statement has dot to align with.
                if (after_char_at(point()) == iachar(".") .and. python_syntax_context_type() == 0) then
                    kind = PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION
                    start = point()
                    call goto_char(opoint)
                    return
                end if
            end if

            call goto_char(opoint)
            ! Continuation of block definition.
            block_continuation_start = is_python_info_block_continuation_line()

            if (block_continuation_start > 0) then
                call goto_char(block_continuation_start)
                call re_search_forward(BLOCK_START_SPACE_RE, f, st, bound=line_end_position(), noerror=SEARCH_NOERROR)
                kind = PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION
                start = point()
                call goto_char(opoint)
                return
            end if

            ! Continuation of assignment.
            assignment_continuation_start = is_python_info_assignment_continuation_line()

            if (assignment_continuation_start > 0) then
                kind = PYTHON_INDENT_AFTER_BACKSLASH_ASSIGNMENT_CONTINUATION
                start = assignment_continuation_start
                return
            end if

            ! First line after backslash continuation start.
            call goto_char(s)
            cond = line_number_at_pos() == 1
            if (.not. cond) cond = python_info_beginning_of_backslash(line_number_at_pos() - 1) == 0
            call goto_char(opoint)

            if (cond) then
                kind = PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE
            else
                ! General case.
                kind = PYTHON_INDENT_AFTER_BACKSLASH
            end if

            start = s
            return
        end if

        ! After beginning of block.
        call back_to_indentation()
        dummy = python_util_forward_comment(-1)
        s = 0
        if (before_char_at(point()) == iachar(":")) s = python_nav_beginning_of_block()
        call goto_char(opoint)

        if (s > 0) then
            kind = PYTHON_INDENT_AFTER_BLOCK_START
            start = s
            return
        end if

        ! At dedenter statement.
        s = is_python_info_dedenter_statement()

        if (s > 0) then
            kind = PYTHON_INDENT_AT_DEDENTER_BLOCK_START
            start = s
            return
        end if

        ! After normal line, comment or ender (default case).
        call back_to_indentation()
        p = skip_chars_backward(" "//TAB//LF, st)

        if (is_bob()) then
            kind = PYTHON_INDENT_NO_INDENT
            start = 0
        else
            p = python_nav_beginning_of_statement()

            if (is_python_info_current_line_comment()) then
                kind = PYTHON_INDENT_AFTER_COMMENT
            else
                p = point()
                call goto_char(line_end_position())
                dummy = python_util_forward_comment(-1)
                f = python_nav_beginning_of_statement()
                cond = looking_at(BLOCK_ENDER_RE, st)
                call goto_char(p)

                if (cond) then
                    kind = PYTHON_INDENT_AFTER_BLOCK_END
                else
                    kind = PYTHON_INDENT_AFTER_LINE
                end if
            end if

            start = point()
        end if

        call goto_char(opoint)
    end subroutine python_indent_context

    ! Internal implementation of `python_indent_calculate_indentation':
    ! the maximum possible indentation at current context, or (LIST) a
    ! list of possible indentations (only happening for
    ! PYTHON_INDENT_AT_DEDENTER_BLOCK_START since the possibilities can be
    ! narrowed to specific indentation points).  Port of
    ! python.el:python-indent--calculate-indentation.
    recursive subroutine indent_calculate_indentation_1(values, is_list)
        integer(int64), allocatable, intent(out) :: values(:)
        logical, intent(out) :: is_list
        integer(int64), allocatable :: positions(:)
        integer(int64) :: opoint, start, line_indentation, base_indent, column, p
        integer :: kind, k
        type(status_type) :: st

        opoint = point()
        is_list = .false.
        call python_indent_context(kind, start)
        allocate (values(1))
        values(1) = prog_first_column()

        select case (kind)
        case (PYTHON_INDENT_NO_INDENT)
            ! usually 0
            values(1) = prog_first_column()
        case (PYTHON_INDENT_AFTER_LINE, PYTHON_INDENT_AFTER_COMMENT, PYTHON_INDENT_INSIDE_STRING, &
              PYTHON_INDENT_AFTER_BACKSLASH, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
            ! Copy previous indentation.
            call goto_char(start)
            values(1) = current_indentation()
        case (PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
            call goto_char(start + 1)

            if (looking_at("[ "//TAB//"]*\(?:#\|$\)", st)) then
                ! Copy previous indentation.
                values(1) = current_indentation()
            else
                ! Align with opening paren.
                values(1) = current_column()
            end if
        case (PYTHON_INDENT_INSIDE_DOCSTRING)
            line_indentation = current_indentation()
            call goto_char(start)
            base_indent = current_indentation()
            values(1) = max(line_indentation, base_indent)
        case (PYTHON_INDENT_AFTER_BLOCK_START, PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE, &
              PYTHON_INDENT_AFTER_BACKSLASH_ASSIGNMENT_CONTINUATION, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
            ! Add one indentation level.
            call goto_char(start)
            values(1) = current_indentation() + python_indent_offset%value
        case (PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION)
            call goto_char(start)
            column = current_column()

            if (column == current_indentation() + python_indent_offset%value) then
                ! Add one level to avoid same indent as next logical line.
                values(1) = column + python_indent_offset%value
            else
                values(1) = column
            end if
        case (PYTHON_INDENT_INSIDE_PAREN, PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION)
            ! Use the column given by the context.
            call goto_char(start)
            values(1) = current_column()
        case (PYTHON_INDENT_AFTER_BLOCK_END)
            ! Subtract one indentation level.
            call goto_char(start)
            values(1) = max(0_int64, current_indentation() - python_indent_offset%value)
        case (PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
            ! List all possible indentation levels from opening blocks.
            positions = python_info_dedenter_opening_block_positions()

            if (size(positions) > 0) then
                is_list = .true.
                deallocate (values)
                allocate (values(size(positions)))

                do k = 1, size(positions)
                    call goto_char(positions(k))
                    values(k) = current_indentation()
                end do
            end if
        case (PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK)
            call goto_char(start)
            values(1) = current_indentation() + python_indent_offset%value*python_indent_def_block_scale%value
        case (PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK)
            call goto_char(start)
            column = current_column()
            values(1) = column

            if (python_indent_block_paren_deeper%value) then
                p = python_nav_beginning_of_statement()

                if (column == current_indentation() + python_indent_offset%value) then
                    values(1) = column + python_indent_offset%value
                end if
            end if
        case default
            continue
        end select

        call goto_char(opoint)
    end subroutine indent_calculate_indentation_1

    ! Calculate levels list given INDENTATION (a list when IS_LIST).
    ! Levels are returned in ascending order, and in the
    ! case INDENTATION is a list, this order is enforced.  Port of
    ! python.el:python-indent--calculate-levels.
    recursive function indent_calculate_levels_1(indentation, is_list) result(levels)
        integer(int64), intent(in) :: indentation(:)
        logical, intent(in) :: is_list
        integer(int64), allocatable :: levels(:)
        integer(int64) :: tmp, v
        integer :: i, j, n

        if (is_list) then
            levels = indentation

            do i = 2, size(levels)
                tmp = levels(i)
                j = i - 1

                do while (j >= 1)
                    if (.not. levels(j) > tmp) exit
                    levels(j + 1) = levels(j)
                    j = j - 1
                end do

                levels(j + 1) = tmp
            end do

            return
        end if

        n = 0
        v = prog_first_column()

        if (python_indent_offset%value > 0) then
            do while (v <= indentation(1) - 1)
                n = n + 1
                v = v + python_indent_offset%value
            end do
        end if

        allocate (levels(n + 1))
        v = prog_first_column()

        do i = 1, n
            levels(i) = v
            v = v + python_indent_offset%value
        end do

        levels(n + 1) = indentation(1)
    end function indent_calculate_levels_1

    ! Return previous level from LEVELS relative to INDENTATION.  Port of
    ! python.el:python-indent--previous-level.
    recursive integer(int64) function indent_previous_level(levels, indentation) result(level)
        integer(int64), intent(in) :: levels(:)
        integer(int64), intent(in) :: indentation
        integer(int64) :: best
        logical :: found
        integer :: i

        level = maxval(levels)
        found = .false.
        best = 0

        do i = 1, size(levels)
            if (levels(i) < indentation) then
                if (.not. found .or. levels(i) > best) best = levels(i)
                found = .true.
            end if
        end do

        if (found) level = best
    end function indent_previous_level

    !> Calculate indentation.
    !> Get indentation of PREVIOUS level when argument is true.
    !> Return the max level of the cycle when indentation reaches the
    !> minimum.  Port of python.el:python-indent-calculate-indentation.
    recursive integer(int64) function python_indent_calculate_indentation(previous) result(indentation)
        logical, intent(in), optional :: previous
        integer(int64), allocatable :: values(:), levels(:)
        logical :: is_list, prev

        prev = .false.
        if (present(previous)) prev = previous
        call indent_calculate_indentation_1(values, is_list)
        levels = indent_calculate_levels_1(values, is_list)

        if (prev) then
            indentation = indent_previous_level(levels, current_indentation())
        else if (size(levels) > 0) then
            indentation = maxval(levels)
        else
            indentation = prog_first_column()
        end if
    end function python_indent_calculate_indentation

    !> Internal implementation of `python_indent_line_function'.
    !> Use the PREVIOUS level when argument is true, otherwise indent
    !> to the maximum available level.  When indentation is the minimum
    !> possible and PREVIOUS is true, cycle back to the maximum
    !> level.  Port of python.el:python-indent-line.
    recursive subroutine python_indent_line(status, previous)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: previous
        type(handle_type) :: m
        type(status_type) :: st
        logical :: follow_indentation_p

        ! Check if point is within indentation.
        follow_indentation_p = line_beginning_position() <= point() .and. &
                               line_beginning_position() + current_indentation() >= point()
        ! (save-excursion)
        call copy_marker_at(point(), m, .false., status)
        if (.not. status%ok()) return
        call indent_line_to(python_indent_calculate_indentation(previous), status)
        if (status%ok()) call python_info_dedenter_opening_block_message()
        call goto_char(mpos(m))
        call free_marker(m, st)
        if (follow_indentation_p) call back_to_indentation()
    end subroutine python_indent_line

    !> Return possible indentation levels.  Port of
    !> python.el:python-indent-calculate-levels.
    recursive function python_indent_calculate_levels() result(levels)
        integer(int64), allocatable :: levels(:)
        integer(int64), allocatable :: values(:)
        logical :: is_list

        call indent_calculate_indentation_1(values, is_list)
        levels = indent_calculate_levels_1(values, is_list)
    end function python_indent_calculate_levels

    !> `indent_line_function' for Python mode.
    !> When the variable `last_command' is equal to one of the symbols
    !> inside `python_indent_trigger_commands' it cycles possible
    !> indentation levels from right to left.  Port of
    !> python.el:python-indent-line-function.
    recursive subroutine python_indent_line_function(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        logical :: previous
        integer :: k

        noindent = .false.
        previous = .false.

        do k = 1, size(python_indent_trigger_commands)
            if (value_eq(this_command, sym(trim(python_indent_trigger_commands(k))))) previous = .true.
        end do

        if (previous) previous = value_eq(last_command, this_command)
        call python_indent_line(status, previous)
    end subroutine python_indent_line_function

    !> De-indent current line (true if it did).  Port of
    !> python.el:python-indent-dedent-line.
    recursive logical function python_indent_dedent_line() result(done)
        type(status_type) :: st

        done = .false.

        if (.not. is_bol() .and. is_python_syntax_comment_or_string() == 0 .and. current_indentation() == current_column()) then
            call python_indent_line(st, .true.)
            done = .true.
        end if
    end function python_indent_dedent_line

    !> De-indent current line.
    !> Argument ARG is passed to `backward_delete_char_untabify' when point is
    !> not in between the indentation or when Transient Mark mode is enabled,
    !> the mark is active, and ARG is 1.  Port of
    !> python.el:python-indent-dedent-line-backspace.
    recursive subroutine python_indent_dedent_line_backspace(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical :: delete

        status = status_type()
        delete = is_use_region() .and. arg == 1
        if (.not. delete) delete = .not. python_indent_dedent_line()
        if (delete) call backward_delete_char_untabify(arg, status)
    end subroutine python_indent_dedent_line_backspace

    !> Indent a Python region automagically.
    !>
    !> Called from a program, START and END specify the region to indent.
    !> Port of python.el:python-indent-region.
    recursive subroutine python_indent_region(start, end, status)
        use buffer_fm, only: deactivate_mark_variable
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(handle_type) :: endm, m
        integer(int64) :: shortage, q
        type(status_type) :: st
        logical :: saved_deactivate, line_is_comment_p, ok, comment_or_empty

        status = status_type()
        saved_deactivate = deactivate_mark_variable
        call copy_marker_at(point(), m, .false., st)
        call goto_char(end)
        call copy_marker_at(point(), endm, .false., st)
        call goto_char(start)
        if (.not. is_bol()) shortage = forward_line(1_int64)

        do while (point() < mpos(endm))
            if (.not. (is_bol() .and. is_eol())) then
                ! Skip if previous line is empty or a comment.
                q = point()
                line_is_comment_p = is_python_info_current_line_comment()
                shortage = forward_line(-1_int64)
                comment_or_empty = is_python_info_current_line_comment() .and. .not. line_is_comment_p
                if (.not. comment_or_empty) comment_or_empty = is_python_info_current_line_empty()
                ok = .not. comment_or_empty
                call goto_char(q)

                ! Don't mess with strings, unless it's the
                ! enclosing set of quotes or a docstring.
                if (ok) then
                    if (python_syntax_context(PYTHON_CONTEXT_STRING) > 0) then
                        ok = class_after(point() - 1 + current_indentation() &
                                         + python_syntax_count_quotes(after_char_at(point()), point())) == class_of("|")
                        if (.not. ok) ok = is_python_info_docstring()
                    end if
                end if

                ! Skip if current line is a block start, a
                ! dedenter or block ender.
                if (ok) then
                    q = point()
                    call back_to_indentation()
                    ok = .not. looking_at(INDENT_REGION_SKIP_RE, st)
                    call goto_char(q)
                end if

                if (ok) call python_indent_line(status)
            end if

            shortage = forward_line(1_int64)
        end do

        call free_marker(endm, st)
        call goto_char(mpos(m))
        call free_marker(m, st)
        deactivate_mark_variable = saved_deactivate
    end subroutine python_indent_region

    !> Shift lines contained in region START END by COUNT columns to the left.
    !> COUNT defaults to `python_indent_offset'.  If region isn't
    !> active, the current line is shifted.  The shifted region includes
    !> the lines in which START and END lie.  An error is signaled if
    !> any lines in the region are indented less than COUNT columns.
    !> Port of python.el:python-indent-shift-left.
    recursive subroutine python_indent_shift_left(start, end, status, count)
        use buffer_fm, only: deactivate_mark_variable
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n, opoint, shortage
        type(status_type) :: st
        logical :: saved

        status = status_type()
        n = python_indent_offset%value
        if (present(count)) n = count
        if (n <= 0) return
        saved = deactivate_mark_variable
        opoint = point()
        call goto_char(start)

        do while (point() < end)
            if (current_indentation() < n .and. .not. looking_at("[ "//TAB//"]*$", st)) then
                call goto_char(opoint)
                call set_error(status, FM_USER_ERROR, "Can't shift all lines enough")
                return
            end if

            shortage = forward_line()
        end do

        call indent_rigidly(start, end, -n, status)
        call goto_char(opoint)
        deactivate_mark_variable = saved
    end subroutine python_indent_shift_left

    !> Shift lines contained in region START END by COUNT columns to the right.
    !> COUNT defaults to `python_indent_offset'.  If region isn't
    !> active, the current line is shifted.  The shifted region includes
    !> the lines in which START and END lie.  Port of
    !> python.el:python-indent-shift-right.
    recursive subroutine python_indent_shift_right(start, end, status, count)
        use buffer_fm, only: deactivate_mark_variable
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n
        logical :: saved

        saved = deactivate_mark_variable
        n = python_indent_offset%value
        if (present(count)) n = count
        call indent_rigidly(start, end, n, status)
        deactivate_mark_variable = saved
    end subroutine python_indent_shift_right

    !> Adjust indentation after insertion of some characters.
    !> This function is intended to be added to `post_self_insert_hook.'
    !> If a line renders a paren alone, after adding a char before it,
    !> the line will be re-indented automatically if needed.  Port of
    !> python.el:python-indent-post-self-insert-function.
    recursive subroutine python_indent_post_self_insert_function(status)
        type(status_type), intent(out) :: status
        integer(int64) :: paren_start, indentation, opoint, dedenter_pos, ev
        type(handle_type) :: startm, endm
        type(status_type) :: st
        logical :: ok
        integer :: c

        status = status_type()
        if (.not. minor_modes%electric_indent_mode%enabled()) return
        ev = -1
        if (vtag(last_command_event) == V_INTEGER) ev = xfixnum(last_command_event)
        if (before_char_at(point()) /= ev) return
        if (python_syntax_context(PYTHON_CONTEXT_STRING) > 0) return
        opoint = point()
        call beginning_of_line()
        ok = python_syntax_context(PYTHON_CONTEXT_STRING, ppss_here()) == 0
        call goto_char(opoint)
        if (.not. ok) return
        ! Electric indent inside parens
        ok = .not. is_bol()

        if (ok) then
            paren_start = python_syntax_context(PYTHON_CONTEXT_PAREN)
            ! Check that point is inside parens.
            ok = paren_start > 0
            ! Filter the case where input is happening in the same
            ! line where the open paren is.
            if (ok) ok = line_number_at_pos() /= line_number_at_pos(paren_start)
        end if

        if (ok) then
            ! When content has been added before the closing paren or a
            ! comma has been inserted, it's ok to do the trick.
            c = after_char_at(point())
            ok = c == iachar(")") .or. c == iachar("]") .or. c == iachar("}") .or. before_char_at(point()) == iachar(",")
        end if

        if (ok) then
            call copy_marker_at(point(), startm, .false., st)
            call goto_char(line_beginning_position())
            indentation = python_indent_calculate_indentation()
            if (current_indentation() < indentation) call indent_line_to(indentation, status)
            call goto_char(mpos(startm))
            call free_marker(startm, st)
            return
        end if

        ! Electric colon
        if (ev /= iachar(":")) return
        if (.not. any(electric_indent_chars_value() == iachar(":"))) return
        if (allocated(current_prefix_arg%a)) return
        ! Trigger electric colon only at end of line
        if (.not. is_eol()) return
        ! Avoid re-indenting on extra colon
        if (before_char_at(point() - 1) == iachar(":")) return
        if (is_python_syntax_comment_or_string() /= 0) return
        ! Just re-indent dedenters
        dedenter_pos = is_python_info_dedenter_statement()
        if (dedenter_pos == 0) return
        call copy_marker_at(dedenter_pos, startm, .false., st)
        call copy_marker_at(point(), endm, .false., st)
        opoint = point()
        call goto_char(mpos(startm))
        call python_indent_line(status)

        ! Reindent region if this is a multiline statement
        if (line_number_at_pos(mpos(startm)) /= line_number_at_pos(mpos(endm))) then
            call python_indent_region(mpos(startm), mpos(endm), status)
        end if

        call goto_char(mpos(endm))
        call free_marker(startm, st)
        call free_marker(endm, st)
        if (opoint < 0) continue
    end subroutine python_indent_post_self_insert_function

    ! ------------------------------------------------------------------
    ! Mark
    ! ------------------------------------------------------------------

    !> Put mark at end of this subprogram, point at beginning.
    !> The subprogram marked is the one that contains point or follows point.
    !>
    !> Interactively (or with ALLOW_EXTEND true), if this command is
    !> repeated or (in Transient Mark mode) if the mark is active, it
    !> marks the next subprogram after the ones already marked.  Port of
    !> python.el:python-mark-defun.
    recursive subroutine python_mark_definition(allow_extend, status)
        integer(int64), intent(in) :: allow_extend
        type(status_type), intent(out) :: status

        if (python_info_looking_at_beginning_of_definition()) call end_of_line(1_int64)
        call mark_subprogram(allow_extend, status)
    end subroutine python_mark_definition

    ! ------------------------------------------------------------------
    ! Fill paragraph
    ! ------------------------------------------------------------------

    !> `fill_paragraph_function' handling multi-line strings and possibly comments.
    !> If any of the current line is in or at the end of a multi-line string,
    !> fill the string or the paragraph of it that point is in, preserving
    !> the string's indentation.
    !> Optional argument JUSTIFY defines if the paragraph should be justified.
    !> Port of python.el:python-fill-paragraph.
    recursive subroutine python_fill_paragraph(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: q, moved
        logical :: paren

        status = status_type()
        done = .true.
        ! (save-excursion)
        call copy_marker_at(point(), m, .false., st)

        if (python_syntax_context(PYTHON_CONTEXT_COMMENT) > 0) then
            ! Comments
            call python_fill_comment_function(status, done, justify)
        else if (is_python_info_triple_quoted_string() > 0) then
            ! Strings/Docstrings
            call python_fill_string_function(status, done, justify)
        else if (decorator_here()) then
            ! Decorators
            call python_fill_decorator_function(status, done, justify)
        else
            ! Parens
            paren = python_syntax_context(PYTHON_CONTEXT_PAREN) > 0
            if (.not. paren) paren = looking_at(OPEN_PAREN_RE, st)

            if (.not. paren) then
                q = point()
                moved = skip_syntax_forward("^(", line_end_position())
                paren = looking_at(OPEN_PAREN_RE, st)
                call goto_char(q)
            end if

            if (paren) call python_fill_paren_function(status, done, justify)
        end if

        call goto_char(mpos(m))
        call free_marker(m, st)

    contains

        ! (equal (char-after (save-excursion (python-nav-beginning-of-statement))) ?@)
        logical function decorator_here()
            integer(int64) :: q, b

            q = point()
            b = python_nav_beginning_of_statement()
            call goto_char(q)
            decorator_here = after_char_at(b) == iachar("@")
        end function decorator_here
    end subroutine python_fill_paragraph

    !> Comment fill function for `python_fill_paragraph'.
    !> JUSTIFY should be used (if applicable) as in `fill_paragraph'.  Port of
    !> python.el:python-fill-comment.
    recursive subroutine python_fill_comment(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify

        call fill_comment_paragraph(status, done, justify)
    end subroutine python_fill_comment

    !> String fill function for `python_fill_paragraph'.
    !> JUSTIFY should be used (if applicable) as in `fill_paragraph'.  Port of
    !> python.el:python-fill-string.
    recursive subroutine python_fill_string(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        type(handle_type) :: str_start, str_line_start, str_end
        character(len=:), allocatable :: text
        integer(int64) :: num_quotes, f, begv, zv_from_end, start_newlines, end_newlines, q, moved, e
        type(status_type) :: st
        logical :: multi_line_p

        done = .true.
        call copy_marker_at(is_python_info_triple_quoted_string(), str_start, .false., status)
        if (.not. status%ok()) return
        ! JT@2021-09-21: Since bug#49518's fix this will always be 1
        num_quotes = python_syntax_count_quotes(after_char_at(mpos(str_start)), mpos(str_start))
        q = point()
        call goto_char(mpos(str_start))
        call beginning_of_line()
        call copy_marker_at(point(), str_line_start, .false., st)
        call goto_char(mpos(str_start) + num_quotes)
        call re_search_forward(STRING_DELIM_SYNTAX_RE, f, st, noerror=SEARCH_NOERROR)
        if (.not. st%ok() .or. f <= 0) call goto_char(point_max())
        call copy_marker_at(point(), str_end, .false., st)
        call goto_char(q)
        ! Docstring styles may vary for one-liners and multi-liners.
        call buffer_substring(mpos(str_start), mpos(str_end), text, st)
        multi_line_p = index(text, LF) > 0
        ! delimiters-style is a cons cell with the form
        ! (START-NEWLINES .  END-NEWLINES). When any of the expressions
        ! is NIL means to not add any newlines for start or end
        ! of docstring.  See `python-fill-docstring-style' for a
        ! graphic idea of each style.
        start_newlines = 0
        end_newlines = 0

        select case (python_fill_docstring_style%value)
        case (PYTHON_DOCSTRING_DJANGO)
            start_newlines = 1
            end_newlines = 1
        case (PYTHON_DOCSTRING_ONETWO)
            if (multi_line_p) then
                start_newlines = 1
                end_newlines = 2
            end if
        case (PYTHON_DOCSTRING_PEP_257)
            if (multi_line_p) end_newlines = 2
        case (PYTHON_DOCSTRING_PEP_257_NN)
            if (multi_line_p) end_newlines = 1
        case (PYTHON_DOCSTRING_SYMMETRIC)
            if (multi_line_p) then
                start_newlines = 1
                end_newlines = 1
            end if
        case default
            continue
        end select

        ! (save-restriction (narrow-to-region ...) (fill-paragraph justify))
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv
        call narrow_to_region(mpos(str_line_start), mpos(str_end), status)
        if (status%ok()) call fill_paragraph(status, justify)
        call widen()
        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
        q = point()

        if (is_python_info_docstring() .and. python_fill_docstring_style%value /= PYTHON_DOCSTRING_NONE) then
            ! Add the number of newlines indicated by the selected style
            ! at the start of the docstring.
            call goto_char(mpos(str_start) + num_quotes)
            e = point()
            moved = skip_syntax_forward("> ")
            call delete_region(e, point(), st)
            call goto_char(e)

            if (start_newlines > 0) then
                call newline(st, start_newlines)
                ! Indent only if a newline is added.
                call indent_according_to_mode(st)
            end if

            ! Add the number of newlines indicated by the selected style
            ! at the end of the docstring.
            if (mpos(str_end) /= point_max()) then
                call goto_char(mpos(str_end) - num_quotes)
            else
                call goto_char(mpos(str_end))
            end if

            e = point()
            moved = skip_syntax_backward("> ")
            call delete_region(point(), e, st)

            ! Add newlines only if string ends.
            if (end_newlines > 0 .and. mpos(str_end) /= point_max()) then
                call newline(st, end_newlines)
                ! Again indent only if a newline is added.
                call indent_according_to_mode(st)
            end if
        end if

        call goto_char(q)
        call free_marker(str_start, st)
        call free_marker(str_line_start, st)
        call free_marker(str_end, st)
    end subroutine python_fill_string

    !> Decorator fill function for `python_fill_paragraph'.
    !> JUSTIFY should be used (if applicable) as in `fill_paragraph'.  Port of
    !> python.el:python-fill-decorator.
    recursive subroutine python_fill_decorator(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify

        status = status_type()
        done = .true.
        if (present(justify)) continue
    end subroutine python_fill_decorator

    !> Paren fill function for `python_fill_paragraph'.
    !> JUSTIFY should be used (if applicable) as in `fill_paragraph'.  Port of
    !> python.el:python-fill-paren.
    recursive subroutine python_fill_paren(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        character(len=:), allocatable :: saved_start, saved_separate
        integer(int64) :: begv, zv_from_end, b, e, moved, shortage
        type(status_type) :: st
        logical :: had_start, had_separate

        done = .true.
        status = status_type()
        ! (save-restriction (narrow-to-region ...))
        begv = buffers(cur)%begv
        zv_from_end = buffers(cur)%text%z - buffers(cur)%zv

        do while (python_syntax_context(PYTHON_CONTEXT_PAREN) > 0)
            call goto_char(point() - 1)
        end do

        b = line_beginning_position()

        if (python_syntax_context(PYTHON_CONTEXT_PAREN) == 0) then
            call end_of_line()
            if (python_syntax_context(PYTHON_CONTEXT_PAREN) == 0) moved = skip_syntax_backward("^)")
        end if

        do while (python_syntax_context(PYTHON_CONTEXT_PAREN) > 0 .and. .not. is_eob())
            call goto_char(point() + 1)
        end do

        e = point()
        call narrow_to_region(b, e, status)
        if (.not. status%ok()) return
        had_start = allocated(paragraph_start%value)
        if (had_start) saved_start = paragraph_start%value
        had_separate = allocated(paragraph_separate%value)
        if (had_separate) saved_separate = paragraph_separate%value
        paragraph_start%value = achar(12)//"\|[ "//TAB//"]*$"
        paragraph_separate%value = ","
        call goto_char(point_min())
        call fill_paragraph(status, justify)

        if (had_start) then
            paragraph_start%value = saved_start
        else
            deallocate (paragraph_start%value)
        end if

        if (had_separate) then
            paragraph_separate%value = saved_separate
        else
            deallocate (paragraph_separate%value)
        end if

        do while (.not. is_eob())
            shortage = forward_line(1_int64)
            call python_indent_line(st)
            call goto_char(line_end_position())
        end do

        call widen()
        call narrow_to_region(begv, buffers(cur)%text%z - zv_from_end, st)
    end subroutine python_fill_paren

    !> Like `do_auto_fill', but bind `fill_indent_according_to_mode'.
    !> Port of python.el:python-do-auto-fill.
    recursive subroutine python_do_auto_fill(status)
        type(status_type), intent(out) :: status
        logical :: saved

        ! See Bug#36056.
        saved = fill_indent_according_to_mode
        fill_indent_according_to_mode = .true.
        call do_auto_fill(status)
        fill_indent_according_to_mode = saved
    end subroutine python_do_auto_fill

    ! ------------------------------------------------------------------
    ! Major mode
    ! ------------------------------------------------------------------

    !> Insert the closing two quotes of a triple-quoted string when
    !> Electric Pair mode is on (a function of `post_self_insert_hook').
    !> Port of python.el:python-electric-pair-string-delimiter.
    recursive subroutine python_electric_pair_string_delimiter(status)
        integer(int64) :: ev, count
        type(status_type), intent(out) :: status
        integer(int64) :: opoint

        status = status_type()
        if (.not. minor_modes%electric_pair_mode%enabled()) return
        ev = -1
        if (vtag(last_command_event) == V_INTEGER) ev = xfixnum(last_command_event)
        if (ev /= iachar('"') .and. ev /= iachar("'")) return
        count = 0

        do while (before_char_at(point() - count) == ev)
            count = count + 1
        end do

        if (count /= 3) return
        if (after_char_at(point()) /= ev) return
        opoint = point()
        call insert(achar(ev)//achar(ev), status)
        call goto_char(opoint)
    end subroutine python_electric_pair_string_delimiter

    !> Generic major mode for editing Python files.
    !>
    !> This is a generic major mode intended to be inherited by
    !> concrete implementations.  Currently there are two concrete
    !> implementations: `python_mode' and `python_ts_mode'.  Port of
    !> python.el:python-base-mode.
    recursive subroutine python_base_mode(status)
        type(status_type), intent(out) :: status
        integer, allocatable :: chars(:)
        logical :: saved

        if (.not. is_syntax_table_live(python_mode_syntax_table)) call syms_of_python()
        call major_modes%python_base_mode%begin(saved, status)
        if (.not. status%ok()) return
        call tab_width%set_local(8_int64)
        call indent_tabs_mode%set_local(.false.)
        call comment_start%set_local("# ")
        call comment_start_skip%set_local("#+\s-*")
        call parse_expression_lookup_properties%set_local(.true.)
        call parse_expression_ignore_comments%set_local(.true.)

        if (associated(python_forward_expression_function)) then
            call forward_expression_function%set_local(python_forward_expression_function)
        else
            call forward_expression_function%set_local()
        end if

        call indent_line_function%set_local(python_indent_line_function)
        call indent_region_function%set_local(python_indent_region)
        ! Because indentation is not redundant, we cannot safely reindent code.
        call electric_indent_inhibit%set_local(.true.)
        chars = [iachar(":"), electric_indent_chars_value()]
        call set_local_electric_indent_chars(chars)
        ! Add """ ... """ pairing to electric-pair-mode.
        call post_self_insert_hook%add(python_electric_pair_string_delimiter, depth=90, local=.true.)
        call paragraph_start%set_local("\s-*$")
        call fill_paragraph_function%set_local(python_fill_paragraph)
        call normal_auto_fill_function%set_local(python_do_auto_fill)
        call beginning_of_subprogram_function%set_local(python_nav_beginning_of_definition)
        call end_of_subprogram_function%set_local(python_nav_end_of_definition_function)
        call post_self_insert_hook%add(python_indent_post_self_insert_function, depth=90, local=.true.)
        if (.not. installed_grep_hook) call install_grep_settings()
        call major_modes%python_base_mode%finish(saved, status)
    end subroutine python_base_mode

    ! (with-eval-after-load 'grep ...): the Python files alias and the
    ! directories `rgrep' ignores.
    ! Port of python.el:python-base-mode (in part: its grep settings).
    recursive subroutine install_grep_settings()
        character(len=*), parameter :: DIRS(6) = [character(len=14) :: ".mypy_cache", ".pytest_cache", &
                                                                                     ".ropeproject", ".ruff_cache", ".tox", ".venv"]
        type(grep_files_alias_type), allocatable :: aliases(:)
        type(grep_ignore_type), allocatable :: ignored(:)
        logical :: present_already
        integer :: k, j

        installed_grep_hook = .true.
        if (.not. allocated(grep_files_aliases)) allocate (grep_files_aliases(0))
        present_already = .false.

        do k = 1, size(grep_files_aliases)
            if (trim(grep_files_aliases(k)%name) == "py" .and. trim(grep_files_aliases(k)%files) == "*.py") then
                present_already = .true.
            end if
        end do

        if (.not. present_already) then
            allocate (aliases(size(grep_files_aliases) + 1))
            aliases(1) = grep_files_alias_type(name="py", files="*.py")
            aliases(2:) = grep_files_aliases
            call move_alloc(aliases, grep_files_aliases)
        end if

        if (.not. allocated(grep_find_ignored_directories)) allocate (grep_find_ignored_directories(0))

        do j = 1, size(DIRS)
            present_already = .false.

            do k = 1, size(grep_find_ignored_directories)
                if (trim(grep_find_ignored_directories(k)%name) == trim(DIRS(j)) &
                    .and. .not. associated(grep_find_ignored_directories(k)%applies)) present_already = .true.
            end do

            if (.not. present_already) then
                allocate (ignored(size(grep_find_ignored_directories) + 1))
                ignored(1)%name = DIRS(j)
                ignored(2:) = grep_find_ignored_directories
                call move_alloc(ignored, grep_find_ignored_directories)
            end if
        end do
    end subroutine install_grep_settings

    !> Major mode for editing Python files.
    !>
    !> \{python_mode_map}
    !>
    !> Port of python.el:python-mode.
    recursive subroutine python_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keywords_type) :: levels(4)
        logical :: saved

        if (.not. is_syntax_table_live(python_mode_syntax_table)) call syms_of_python()
        call major_modes%python_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! When `font-lock-maximum-decoration' is nil, 1, 2, and more than 1
        ! or t (which it is, by default).
        levels(1) = font_lock_keywords_level(python_font_lock_keywords_level_1)
        levels(2) = font_lock_keywords_level(python_font_lock_keywords_level_1)
        levels(3) = font_lock_keywords_level(python_font_lock_keywords_level_2)
        levels(4) = font_lock_keywords_level(python_font_lock_keywords_maximum_decoration)
        call font_lock_defaults%set_local(levels=levels, syntactic_face_function=python_font_lock_syntactic_face_function)
        call syntax_propertize_function%set_local(python_syntax_propertize_function)
        if (python_indent_guess_indent_offset_variable%value) call python_indent_guess_indent_offset()
        call major_modes%python_mode%finish(saved, status)
    end subroutine python_mode

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The position of marker M.
    ! Not a port.
    recursive integer(int64) function mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, mpos, st)
    end function mpos

    !> Guess and set `python_indent_offset' for the current buffer (the
    !> command).  Port of python.el:python-indent-guess-indent-offset.
    recursive subroutine cmd_python_indent_guess_indent_offset(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_indent_guess_indent_offset()
    end subroutine cmd_python_indent_guess_indent_offset

    !> De-indent current line (the command).  Port of
    !> python.el:python-indent-dedent-line.
    recursive subroutine cmd_python_indent_dedent_line(status)
        use buffer_fm, only: barf_if_buffer_read_only
        type(status_type), intent(out) :: status
        logical :: done

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        done = python_indent_dedent_line()
    end subroutine cmd_python_indent_dedent_line

    !> De-indent current line (the command).  Port of
    !> python.el:python-indent-dedent-line-backspace.
    recursive subroutine cmd_python_indent_dedent_line_backspace(status)
        use buffer_fm, only: barf_if_buffer_read_only
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call python_indent_dedent_line_backspace(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_indent_dedent_line_backspace

    ! The region or the current line, and the count, of the shift commands.
    ! Port of python.el:python-indent-shift-left (in part: its interactive spec).
    recursive subroutine shift_args(start, end, status)
        integer(int64), intent(out) :: start, end
        type(status_type), intent(out) :: status

        status = status_type()

        if (buffers(cur)%mark_active) then
            call region_beginning(start, status)
            if (status%ok()) call region_end(end, status)
        else
            start = line_beginning_position()
            end = line_end_position()
        end if
    end subroutine shift_args

    !> Shift lines contained in the region by COUNT columns to the left (the
    !> command).  Port of python.el:python-indent-shift-left.
    recursive subroutine cmd_python_indent_shift_left(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end

        call shift_args(start, end, status)
        if (.not. status%ok()) return

        if (.not. allocated(current_prefix_arg%a)) then
            call python_indent_shift_left(start, end, status)
        else
            call python_indent_shift_left(start, end, status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_python_indent_shift_left

    !> Shift lines contained in the region by COUNT columns to the right (the
    !> command).  Port of python.el:python-indent-shift-right.
    recursive subroutine cmd_python_indent_shift_right(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end

        call shift_args(start, end, status)
        if (.not. status%ok()) return

        if (.not. allocated(current_prefix_arg%a)) then
            call python_indent_shift_right(start, end, status)
        else
            call python_indent_shift_right(start, end, status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_python_indent_shift_right

    !> Put mark at end of this subprogram, point at beginning (the command).
    !> Port of python.el:python-mark-defun.
    recursive subroutine cmd_python_mark_definition(status)
        type(status_type), intent(out) :: status

        call python_mark_definition(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_mark_definition

    !> Move point to the end of def or class (the command).  Port of
    !> python.el:python-nav-end-of-defun.
    recursive subroutine cmd_python_nav_end_of_definition(status)
        type(status_type), intent(out) :: status
        logical :: moved

        status = status_type()
        moved = python_nav_end_of_definition()
    end subroutine cmd_python_nav_end_of_definition

    !> Navigate to closer subprogram backward ARG times (the command).  Port of
    !> python.el:python-nav-backward-defun.
    recursive subroutine cmd_python_nav_backward_definition(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_backward_definition(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_python_nav_backward_definition

    !> Navigate to closer subprogram forward ARG times (the command).  Port of
    !> python.el:python-nav-forward-defun.
    recursive subroutine cmd_python_nav_forward_definition(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_forward_definition(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_python_nav_forward_definition

    !> Move to start of current statement (the command).  Port of
    !> python.el:python-nav-beginning-of-statement.
    recursive subroutine cmd_python_nav_beginning_of_statement(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_beginning_of_statement()
    end subroutine cmd_python_nav_beginning_of_statement

    !> Move to end of current statement (the command).  Port of
    !> python.el:python-nav-end-of-statement.
    recursive subroutine cmd_python_nav_end_of_statement(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_end_of_statement()
    end subroutine cmd_python_nav_end_of_statement

    !> Move backward to previous statement (the command).  Port of
    !> python.el:python-nav-backward-statement.
    recursive subroutine cmd_python_nav_backward_statement(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_nav_backward_statement(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_python_nav_backward_statement

    !> Move forward to next statement (the command).  Port of
    !> python.el:python-nav-forward-statement.
    recursive subroutine cmd_python_nav_forward_statement(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_nav_forward_statement(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_python_nav_forward_statement

    !> Move to start of current block (the command).  Port of
    !> python.el:python-nav-beginning-of-block.
    recursive subroutine cmd_python_nav_beginning_of_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_beginning_of_block()
    end subroutine cmd_python_nav_beginning_of_block

    !> Move to end of current block (the command).  Port of
    !> python.el:python-nav-end-of-block.
    recursive subroutine cmd_python_nav_end_of_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_end_of_block()
    end subroutine cmd_python_nav_end_of_block

    !> Move backward to previous block of code (the command).  Port of
    !> python.el:python-nav-backward-block.
    recursive subroutine cmd_python_nav_backward_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_backward_block(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_python_nav_backward_block

    !> Move forward to next block of code (the command).  Port of
    !> python.el:python-nav-forward-block.
    recursive subroutine cmd_python_nav_forward_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_forward_block(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_python_nav_forward_block

    !> Move forward across expressions (the command).  Port of
    !> python.el:python-nav-forward-sexp.
    recursive subroutine cmd_python_nav_forward_expression(status)
        type(status_type), intent(out) :: status

        call python_nav_forward_expression(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_nav_forward_expression

    !> Move backward across expressions (the command).  Port of
    !> python.el:python-nav-backward-sexp.
    recursive subroutine cmd_python_nav_backward_expression(status)
        type(status_type), intent(out) :: status

        call python_nav_backward_expression(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_nav_backward_expression

    !> Move forward safely across expressions (the command).  Port of
    !> python.el:python-nav-forward-sexp-safe.
    recursive subroutine cmd_python_nav_forward_expression_safe(status)
        type(status_type), intent(out) :: status

        call python_nav_forward_expression_safe(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_nav_forward_expression_safe

    !> Move backward safely across expressions (the command).  Port of
    !> python.el:python-nav-backward-sexp-safe.
    recursive subroutine cmd_python_nav_backward_expression_safe(status)
        type(status_type), intent(out) :: status

        call python_nav_backward_expression_safe(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_nav_backward_expression_safe

    !> Move forward out of one level of parentheses (or blocks) (the
    !> command).  Port of python.el:python-nav-up-list.
    recursive subroutine cmd_python_nav_up_group(status)
        type(status_type), intent(out) :: status

        call python_nav_up_group(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_nav_up_group

    !> Move backward out of one level of parentheses (or blocks) (the
    !> command).  Port of python.el:python-nav-backward-up-list.
    recursive subroutine cmd_python_nav_backward_up_group(status)
        type(status_type), intent(out) :: status

        call python_nav_backward_up_group(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_python_nav_backward_up_group

    !> Move point at the beginning the __main__ block (the command).
    !> Port of python.el:python-nav-if-name-main.
    recursive subroutine cmd_python_nav_if_name_main(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_nav_if_name_main()
    end subroutine cmd_python_nav_if_name_main

    !> Fill a comment, string, decorator or paren (the command).  Port of
    !> python.el:python-fill-paragraph.
    recursive subroutine cmd_python_fill_paragraph(status)
        use fill_fm, only: JUSTIFY_FULL
        type(status_type), intent(out) :: status
        logical :: done

        if (.not. allocated(current_prefix_arg%a)) then
            call python_fill_paragraph(status, done)
        else
            call python_fill_paragraph(status, done, JUSTIFY_FULL)
        end if
    end subroutine cmd_python_fill_paragraph
end module python_fm
