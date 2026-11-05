! python_mod.f90 -- Python's flying circus support for Fortmacs.
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
!> expression motion (`python_navigation_*', used by C-M-a, C-M-e, C-M-f, C-M-u and the
!> sentence keys), `python_mark_definition', filling of comments, strings and
!> parens, and the `python_info_*' and `python_utility_*' helpers.
!>
!> The regexps python.el builds with its `python_rx' macro are written
!> out as constants.  A syntax context (`python_syntax_context') is a
!> position, 0 for none; `python_syntax_context_type' gives one of the
!> PYTHON_CONTEXT_* constants.  Docstring styles are the
!> PYTHON_DOCSTRING_* constants.
!>
!> Deferred: the shell integration (`run_python', the `python_shell_*'
!> commands and variables, `inferior_python_mode', shell completion,
!> PDB tracking, `python_interpreter' and `python_interpreter_arguments'),
!> symbol completion (`python_completion_at_point'), the skeletons
!> (skeleton.el), FFAP, `python_check', ElDoc, Hideshow,
!> `python_index_menu_create_flat_index', Outline mode's variables, `prettify_symbols',
!> Flymake, the import management commands (C-c C-i), tree-sitter
!> (`python_ts_mode'), the menu,
!> `python_utility_clone_local_variables' and the other utilities the shell
!> uses, the `delete_selection' property of
!> `python_indent_dedent_line_backspace', and the obsolete
!> `python_indent_current_level' and `python_indent_levels'.
module python_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use error_mod, only: status_type, set_error, USER_ERROR_CODE, SCAN_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, xfixnum, list, list_elements, vtag, V_INTEGER
    use data_mod, only: integer_setting_type, logical_setting_type, string_setting_type, forward_setting, &
                        append_locals_clearer
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use keyboard_mod, only: this_command, last_command, real_this_command, last_command_event
    use buffer_mod, only: buffers, current, buffer_serial_live, current_buffer, buffer_characters_modified_tick, &
                          transient_mark_mode
    use hooks_mod, only: hook_type
    use modes_mod, only: major_modes, minor_modes
    use add_log_mod, only: add_log_current_subprogram_function
    use keymap_mod, only: keymap_set, define_key, make_sparse_keymap, set_repeat_map
    use syntax_mod, only: make_syntax_table, modify_syntax_entry, is_syntax_table_live, syntax_parser_state, parse_state_type, &
                          forward_comment, scan_groups, skip_characters_forward, skip_characters_backward, skip_syntax_forward, &
                          skip_syntax_backward, syntax_entry, standard_syntax_table, string_to_syntax, &
                          syntax_descriptor_type, syntax_entry_at, character_syntax, parse_expression_ignore_comments, &
                          parse_expression_lookup_properties, scan_error_data, ST_STRING_STYLE, syntax_table, set_syntax_table
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, is_beginning_of_line, is_end_of_line, &
                                     is_beginning_of_buffer, is_end_of_buffer, character_after, character_before, &
                                     buffer_substring, line_beginning_position, line_end_position, delete_region, &
                                     narrow_to_region, widen, region_beginning, region_end, preceding_character, &
                                     following_character, mark_position
    use editing_commands_mod, only: forward_line, forward_character, beginning_of_line, end_of_line, post_self_insert_hook
    use insert_delete_mod, only: insert
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use indent_mod, only: current_column, current_indentation, indent_line_to, indent_line_function, &
                          indent_region_function, indent_rigidly, indent_according_to_mode, indent_tabs_mode
    use search_mod, only: looking_at, regular_expression_search_forward, regular_expression_search_backward, search_backward, &
                          string_match, match_beginning, match_end, match_data, set_match_data, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string_no_properties, looking_back
    use simple_mod, only: back_to_indentation, line_number_at_position, is_use_region, backward_delete_character_untabify, &
                          newline, count_lines
    use text_properties_mod, only: put_text_property
    use display_engine_mod, only: message, tab_width
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, font_lock_keywords_type, &
                             font_lock_keywords_level, FONT_LOCK_OVERRIDE_REPLACE, FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use expression_motion_mod, only: forward_expression_function, forward_expression_default_function, &
                                     up_group, mark_subprogram, beginning_of_subprogram_function, &
                                     end_of_subprogram_function
    use newcomment_mod, only: comment_start, comment_start_skip
    use paragraphs_mod, only: paragraph_start, paragraph_separate
    use fill_mod, only: fill_paragraph, fill_paragraph_function, fill_comment_paragraph, fill_indent_according_to_mode
    use simple_mod, only: normal_auto_fill_function, do_auto_fill
    use electric_mod, only: electric_indent_inhibit, electric_indent_characters
    use programming_mode_mod, only: programming_first_column
    use syntax_propertize_mod, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type
    use grep_mod, only: grep_files_aliases, grep_files_alias_type, grep_find_ignored_directories, grep_ignore_type
    use index_menu_mod, only: index_menu_create_index_function, index_menu_index_type
    use which_function_mod, only: which_function_functions
    use string_type_mod, only: string_type, prepend_string
    implicit none(type, external)
    private

    public :: create_python_index_menu_index, format_python_index_menu_item_label, format_python_index_menu_parent_item_label, &
              format_python_index_menu_parent_item_jump_label, python_index_menu_label_procedure
    public :: initialize_python, python_base_mode, python_mode, python_syntax_context, python_syntax_context_type, &
              is_python_syntax_comment_or_string, is_python_syntax_closing_parenthesis, python_font_lock_syntactic_face_function, &
              python_syntax_count_quotes, python_syntax_stringify, python_indent_guess_indent_offset, &
              python_indent_context, python_indent_calculate_indentation, python_indent_calculate_levels, &
              python_indent_line, python_indent_line_function, python_indent_dedent_line, &
              python_indent_dedent_line_backspace, python_indent_region, python_indent_shift_left, &
              python_indent_shift_right, python_indent_post_self_insert_function, python_mark_definition, &
            python_navigation_beginning_of_definition, python_navigation_end_of_definition, python_navigation_backward_definition, &
              python_navigation_forward_definition, &
              python_navigation_beginning_of_statement, python_navigation_end_of_statement, python_navigation_backward_statement, &
              python_navigation_forward_statement, python_navigation_beginning_of_block, python_navigation_end_of_block, &
              python_navigation_backward_block, python_navigation_forward_block, python_navigation_forward_expression, &
              python_navigation_backward_expression, &
              python_navigation_forward_expression_safe, python_navigation_backward_expression_safe, python_navigation_up_group, &
              python_navigation_backward_up_group, &
              python_navigation_if_name_main, python_info_current_definition, is_python_info_statement_starts_block, &
              is_python_info_statement_ends_block, is_python_info_beginning_of_statement, is_python_info_end_of_statement, &
              is_python_info_beginning_of_block, is_python_info_end_of_block, python_info_dedenter_opening_block_position, &
              python_info_dedenter_opening_block_positions, python_info_dedenter_opening_block_message, &
              is_python_info_dedenter_statement, is_python_info_line_ends_backslash, python_info_beginning_of_backslash, &
              is_python_info_continuation_line, is_python_info_block_continuation_line, is_python_info_assignment_statement, &
              is_python_info_assignment_continuation_line, python_info_looking_at_beginning_of_definition, &
              python_info_looking_at_beginning_of_block, is_python_info_current_line_comment, &
              is_python_info_current_line_empty, is_python_info_docstring, is_python_info_triple_quoted_string, &
              python_info_encoding_from_cookie, python_info_encoding, python_utility_goto_line, python_utility_forward_comment, &
              python_fill_paragraph, python_fill_comment, python_fill_string, python_fill_decorator, python_fill_parenthesis, &
              python_do_auto_fill, python_electric_pair_string_delimiter, python_fill_procedure

    !> The syntax contexts of `python_syntax_context' and
    !> `python_syntax_context_type' (0 is none).
    integer, parameter, public :: PYTHON_CONTEXT_COMMENT = 1, PYTHON_CONTEXT_STRING = 2, &
                                  PYTHON_CONTEXT_SINGLE_QUOTED_STRING = 3, PYTHON_CONTEXT_TRIPLE_QUOTED_STRING = 4, &
                                  PYTHON_CONTEXT_PARENTHESIS = 5

    !> The values of `python_fill_docstring_style'.
    integer, parameter, public :: PYTHON_FILL_DOCSTRING_STYLE_NONE = 0, PYTHON_FILL_DOCSTRING_STYLE_DJANGO = 1, &
                                  PYTHON_FILL_DOCSTRING_STYLE_ONETWO = 2, &
                                  PYTHON_FILL_DOCSTRING_STYLE_PEP_257 = 3, PYTHON_FILL_DOCSTRING_STYLE_PEP_257_NN = 4, &
                                  PYTHON_FILL_DOCSTRING_STYLE_SYMMETRIC = 5

    !> The indentation contexts of `python_indent_context' (python.el's
    !> keywords :no-indent and the others).
    integer, parameter, public :: PYTHON_INDENT_NO_INDENT = 1, PYTHON_INDENT_AFTER_COMMENT = 2, &
                                  PYTHON_INDENT_INSIDE_STRING = 3, PYTHON_INDENT_INSIDE_DOCSTRING = 4, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS = 5, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_NESTED_PARENTHESIS = 6, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_PARENTHESIS = 7, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START = 8, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START_FROM_BLOCK = 9, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS_FROM_BLOCK = 10, &
                                  PYTHON_INDENT_INSIDE_PARENTHESIS_CONTINUATION_LINE = 11, PYTHON_INDENT_AFTER_BACKSLASH = 12, &
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
    !> After customizing this variable, you must restart Fortmacs for it to take
    !> effect.  Port of python.el python-2-support.
    type(logical_setting_type), target, public :: python_2_support = logical_setting_type(name="python_2_support", value=.false.)

    !> Default indentation offset for Python.  Port of python.el
    !> python-indent-offset.
    type(integer_setting_type), target, public :: python_indent_offset = integer_setting_type(name="python_indent_offset", value=4)

    !> True tells Python mode to guess `python_indent_offset' value.  Port of
    !> python.el python-indent-guess-indent-offset.
    type(logical_setting_type), target, public :: python_indent_guess_indent_offset_variable = &
                                                  logical_setting_type(name="python_indent_guess_indent_offset_variable", &
                                                                       value=.true.)

    !> True means to emit a warning when indentation guessing fails.
    !> Port of python.el python-indent-guess-indent-offset-verbose.
    type(logical_setting_type), target, public :: python_indent_guess_indent_offset_verbose = &
                                                  logical_setting_type(name="python_indent_guess_indent_offset_verbose", &
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
    !> `python_indent_block_parenthesis_deeper' to customize the case where
    !> non-whitespace characters follow the opening paren on the same
    !> line.  Port of python.el python-indent-def-block-scale.
    type(integer_setting_type), target, public :: python_indent_def_block_scale = &
                                                  integer_setting_type(name="python_indent_def_block_scale", value=2)

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
    type(logical_setting_type), target, public :: python_indent_block_parenthesis_deeper = &
                                                  logical_setting_type(name="python_indent_block_parenthesis_deeper", value=.false.)

    !> Function to use when navigating between expressions.  Port of
    !> python.el python-forward-sexp-function (null for the default, "CC-mode
    !> like").
    procedure(python_forward_expression_procedure), pointer, public :: python_forward_expression_function => null()

    !> Style used to fill docstrings.
    !> This affects `python_fill_string' behavior with regards to
    !> triple quotes positioning.
    !>
    !> Possible values are PYTHON_FILL_DOCSTRING_STYLE_DJANGO, _ONETWO, _PEP_257,
    !> _PEP_257_NN, _SYMMETRIC, and PYTHON_FILL_DOCSTRING_STYLE_NONE.  A value of
    !> PYTHON_FILL_DOCSTRING_STYLE_NONE won't care about quotes position and will treat
    !> docstrings a normal string, any other value may result in one of
    !> the following docstring styles:
    !>
    !> PYTHON_FILL_DOCSTRING_STYLE_DJANGO:
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
    !> PYTHON_FILL_DOCSTRING_STYLE_ONETWO:
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
    !> PYTHON_FILL_DOCSTRING_STYLE_PEP_257:
    !>
    !>     """Process foo, return bar."""
    !>
    !>     """Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>
    !>     """
    !>
    !> PYTHON_FILL_DOCSTRING_STYLE_PEP_257_NN:
    !>
    !>     """Process foo, return bar."""
    !>
    !>     """Process foo, return bar.
    !>
    !>     If processing fails throw ProcessingError.
    !>     """
    !>
    !> PYTHON_FILL_DOCSTRING_STYLE_SYMMETRIC:
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
    type(integer_setting_type), target, public :: python_fill_docstring_style = &
                                                  integer_setting_type(name="python_fill_docstring_style", &
                                                                       value=PYTHON_FILL_DOCSTRING_STYLE_PEP_257)

    abstract interface
        !> A function of `python_forward_expression_function': move across ARGUMENT
        !> expressions.
        recursive subroutine python_forward_expression_procedure(argument, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine python_forward_expression_procedure

        !> A fill function of `python_fill_paragraph': DONE is its value
        !> (JUSTIFY absent for no justification).
        recursive subroutine python_fill_procedure(status, done, justify)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: done
            integer, intent(in), optional :: justify
        end subroutine python_fill_procedure
    end interface

    !> Function to fill comments.
    !> This is the function used by `python_fill_paragraph' to
    !> fill comments.  Port of python.el python-fill-comment-function.
    procedure(python_fill_procedure), pointer, public :: python_fill_comment_function => null()
    !> Function to fill strings.
    !> This is the function used by `python_fill_paragraph' to
    !> fill strings.  Port of python.el python-fill-string-function.
    procedure(python_fill_procedure), pointer, public :: python_fill_string_function => null()
    !> Function to fill decorators.
    !> This is the function used by `python_fill_paragraph' to
    !> fill decorators.  Port of python.el python-fill-decorator-function.
    procedure(python_fill_procedure), pointer, public :: python_fill_decorator_function => null()
    !> Function to fill parens.
    !> This is the function used by `python_fill_paragraph' to
    !> fill parens.  Port of python.el python-fill-paren-function.
    procedure(python_fill_procedure), pointer, public :: python_fill_parenthesis_function => null()

    !> Regexp matching class or function definition.
    !> The name of the subprogram should be grouped so it can be retrieved
    !> via `match_string'.  Port of python.el
    !> python-nav-beginning-of-defun-regexp.
    character(len=:), allocatable, public :: python_navigation_beginning_of_definition_regular_expression
    !> Regexp matching block start.  Port of python.el
    !> python-nav-beginning-of-block-regexp.
    character(len=:), allocatable, public :: python_navigation_beginning_of_block_regular_expression

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

    abstract interface
        !> A function formatting an Imenu label: LABEL gets the label of the
        !> subprogram of type TYPE named NAME.  Not a port.
        subroutine python_index_menu_label_procedure(type, name, label)
            implicit none(type, external)
            character(len=*), intent(in) :: type, name
            character(len=:), allocatable, intent(out) :: label
        end subroutine python_index_menu_label_procedure
    end interface

    !> Imenu function used to format an item label.
    !> It must be a function with two arguments: TYPE and NAME.  Port of
    !> python.el python-imenu-format-item-label-function.
    procedure(python_index_menu_label_procedure), pointer, public :: python_index_menu_format_item_label_function => null()

    !> Imenu function used to format a parent item label.
    !> It must be a function with two arguments: TYPE and NAME.  Port of
    !> python.el python-imenu-format-parent-item-label-function.
    procedure(python_index_menu_label_procedure), pointer, public :: python_index_menu_format_parent_item_label_function => null()

    !> Imenu function used to format a parent jump item label.
    !> It must be a function with two arguments: TYPE and NAME.  Port of
    !> python.el python-imenu-format-parent-item-jump-label-function.
    procedure(python_index_menu_label_procedure), pointer, public :: python_index_menu_format_parent_item_jump_label_function => &
        null()

    ! A node of the tree `create_python_index_menu_index' builds: an item
    ! LABEL at POSITION, or (PARENT) a submenu LABEL holding its jump item
    ! JUMP_LABEL at POSITION and the nodes CHILDREN.
    type :: index_menu_node_type
        character(len=:), allocatable :: label, jump_label
        integer(int64) :: position = 0
        logical :: parent = .false.
        integer, allocatable :: children(:)
    end type index_menu_node_type
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
    type :: navigation_cache_entry_type
        integer :: kind = 0
        integer(int64) :: line = 0, target = 0
    end type navigation_cache_entry_type

    type :: navigation_cache_type
        integer(int64) :: serial = 0
        integer(int64) :: tick = 0
        type(navigation_cache_entry_type), allocatable :: entries(:)
    end type navigation_cache_type

    type(navigation_cache_type), allocatable :: navigation_caches(:)

    abstract interface
        ! A motion of `navigation_syntactically', true if it moved.
        recursive logical function navigation_motion_procedure()
            implicit none(type, external)
        end function navigation_motion_procedure
    end interface

    integer, parameter :: NAVIGATION_BEGINNING_OF_BLOCK = 1, NAVIGATION_END_OF_BLOCK = 2

    character(len=*), parameter :: DEF_REGULAR_EXPRESSION = &
                                   "\_<def[[:space:]]+\([_[:alpha:]][_[:word:]]*\)"
    character(len=*), parameter :: CLASS_REGULAR_EXPRESSION = &
                                   "\_<class[[:space:]]+\([_[:alpha:]][_[:word:]]*\)"
    character(len=*), parameter :: KEYWORDS2_REGULAR_EXPRESSION = &
                                   "\_<\(?:and\|del\|from\|not\|while\|as\|elif\|global\|or\|with\|assert\" &
                                   //"|else\|if\|pass\|yield\|break\|except\|import\|class\|in\|raise\|conti" &
                                   //"nue\|finally\|is\|return\|def\|for\|lambda\|try\|nonlocal\|async[[:spa" &
                                   //"ce:]]+\(?:def\|for\|with\)\|await\|match\|case\|self\)\_>"
    character(len=*), parameter :: BUILTINS_REGULAR_EXPRESSION = &
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
    character(len=*), parameter :: CONSTANTS_REGULAR_EXPRESSION = &
                                   "\_<\(?:Ellipsis\|False\|No\(?:ne\|tImplemented\)\|True\|__debug__\|c\(" &
                                   //"?:opyright\|redits\)\|exit\|license\|quit\)\_>"
    character(len=*), parameter :: DECORATOR_REGULAR_EXPRESSION = &
                                   "^["//TAB//" ]*\(@\(?:[[:word:]]\|_\)+\(?:\.\(?:[[:word:]]\|_\)+\)*\)"
    character(len=*), parameter :: EXCEPTIONS_REGULAR_EXPRESSION = &
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
    character(len=*), parameter :: ASSIGN1_REGULAR_EXPRESSION = &
                                   "\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*\([_[:alpha:]][_[:word:]]*\)\(?:\[" &
                                   //"[^]]+]\)?[[:space:]]*\(?::[[:space:]]*\([^"//LF//"%&*+/<->^|~-]+\)[[:space:]]" &
                                   //"*\)?\(\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\)"
    character(len=*), parameter :: SYMBOL_NAME_REGULAR_EXPRESSION = &
                                   "[_[:alpha:]][_[:word:]]*"
    character(len=*), parameter :: ASSIGN2_REGULAR_EXPRESSION = &
                                   "\(?:[([][[:space:]]*\)?\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*\([_[:alpha" &
                                   //":]][_[:word:]]*\)\(?:\[[^]]+]\)?[[:space:]]*,[[:space:]]*\(?:\*?\(?:[_" &
                                   //"[:alpha:]][_[:word:]]*\.\)*[_[:alpha:]][_[:word:]]*\(?:\[[^]]+]\)?[[:s" &
                                   //"pace:]]*,[[:space:]]*\)*\(?:\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*[_[:al" &
                                   //"pha:]][_[:word:]]*\(?:\[[^]]+]\)?[[:space:]]*\)?\(?:,[[:space:]]*\)?\(" &
                                   //"?:[])][[:space:]]*\)?\(\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\)"
    character(len=*), parameter :: GROUPED_TARGET_REGULAR_EXPRESSION = &
                                   "\*?\(?:[_[:alpha:]][_[:word:]]*\.\)*\([_[:alpha:]][_[:word:]]*\)\(?:\[" &
                                   //"[^]]+]\)?"
    character(len=*), parameter :: ASSIGN3_REGULAR_EXPRESSION = &
                                   "\(?:^\|;\|=\)[[:space:]]*[([][[:space:]]*\*?\(?:[_[:alpha:]][_[:word:]" &
                                   //"]*\.\)*\([_[:alpha:]][_[:word:]]*\)\(?:\[[^]]+]\)?[[:space:]]*[])][[:s" &
                                   //"pace:]]*\(\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\)"
    character(len=*), parameter :: OPERATOR_REGULAR_EXPRESSION = &
                                   "\(?:!=\|\*\*\|//\|<[<=]\|==\|>[=>]\|[%&*+/<=>|~^-]\)"
    character(len=*), parameter :: BYTES_ESCAPE_REGULAR_EXPRESSION = &
                                   "[^\]\(\\\\\|\\'\|\\a\|\\b\|\\f\|\\n\|\\r\|\\t\|\\v\|\\[0-7]\{1,3\}\|\\" &
                                   //"x[[:xdigit:]][[:xdigit:]]\)"
    character(len=*), parameter :: STRING_ESCAPE_REGULAR_EXPRESSION = &
                                   "[^\]\(\\\\\|\\'\|\\a\|\\b\|\\f\|\\n\|\\r\|\\t\|\\v\|\\[0-7]\{1,3\}\|\\" &
                                   //"x[[:xdigit:]][[:xdigit:]]\)\|[^\]\(?:\(?1:\\u[[:xdigit:]]\{4\}\)\|\(?1" &
                                   //":\\U[[:xdigit:]]\{8\}\)\|\(?1:\\N{[^z-a]*?}\)\)"
    character(len=*), parameter :: NOT_RAW_BYTES_START_REGULAR_EXPRESSION = &
                                   "\(?:\`\|[^[:alnum:]]\)[Bb]\(?:""""""\|'''\|[""']\)\'"
    character(len=*), parameter :: NOT_RAW_STRING_START_REGULAR_EXPRESSION = &
                                   "\`\(?:\(?:[^[:alnum:]]?[FUfu]\)?\(?:""""""\|'''\)\|[^z-a]?[^[:alnum:]]\(?" &
                                   //":""""""\|'''\)\|\(?:[^z-a]\{0,2\}[^[:alnum:]]\)?[FUfu][""']\|\(?:[^z-a]\{0" &
                                   //",3\}[^""'[:alnum:]]\)?[""']\)\'"
    character(len=*), parameter :: F_STRING_START_REGULAR_EXPRESSION = &
                                   "\<\(?:F[Rr]\|R[Ff]\|f[Rr]\|r[Ff]\|[Ff]\)\(?:""""""\|'''\|[""']\)"
    character(len=*), parameter :: TRIPLE_QUOTES_REGULAR_EXPRESSION = &
                                   "\(?:""""""\|'''\)"
    character(len=*), parameter :: BEGINNING_OF_SUBPROGRAM_REGULAR_EXPRESSION = &
                                   "^[[:space:]]*\_<\(?:def\|class\|async[[:space:]]+def\)\_>\(?:[[:space:" &
                                   //"]]\|\\"//LF//"\)+\([_[:alpha:]][_[:word:]]*\)"
    character(len=*), parameter :: BEGINNING_OF_BLOCK_REGULAR_EXPRESSION = &
                                   "^[[:space:]]*\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|" &
                                   //"for\|while\|with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)" &
                                   //"\_>"
    character(len=*), parameter :: LINE_START_BLOCK_START_REGULAR_EXPRESSION = &
                                   "^\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|" &
                                   //"with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>"
    character(len=*), parameter :: BLOCK_START_REGULAR_EXPRESSION = &
                                   "\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|w" &
                                   //"ith\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>"
    character(len=*), parameter :: BLOCK_START_SPACE_REGULAR_EXPRESSION = &
                                   "\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|w" &
                                   //"ith\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>[[:space:]" &
                                   //"]*"
    character(len=*), parameter :: BLOCK_ENDER_REGULAR_EXPRESSION = &
                                   "\_<\(?:\(?:break\|continue\|pass\|r\(?:aise\|eturn\)\)\_>\|\(?:\(?:os\" &
                                   //"._?\|sys\.\)?exit\|quit\)[[:space:]]*(\)"
    character(len=*), parameter :: DEDENTER_REGULAR_EXPRESSION = &
                                   "\_<\(?:case\|e\(?:l\(?:if\|se\)\|xcept\)\|finally\)\_>"
    character(len=*), parameter :: INDENT_REGION_SKIP_REGULAR_EXPRESSION = &
                                   "\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|for\|while\|w" &
                                   //"ith\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\_>\|\_<\(?:c" &
                                   //"ase\|e\(?:l\(?:if\|se\)\|xcept\)\|finally\)\_>\|\_<\(?:\(?:break\|cont" &
                                   //"inue\|pass\|r\(?:aise\|eturn\)\)\_>\|\(?:\(?:os\._?\|sys\.\)?exit\|qui" &
                                   //"t\)[[:space:]]*(\)"
    character(len=*), parameter :: FORWARD_BLOCK_REGULAR_EXPRESSION = &
                                   "^[[:space:]]*\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|" &
                                   //"for\|while\|with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)" &
                                   //"\_>"
    character(len=*), parameter :: CLOSE_PARENTHESIS_REGULAR_EXPRESSION = &
                                   "[])}]"
    character(len=*), parameter :: OPEN_PARENTHESIS_REGULAR_EXPRESSION = &
                                   "[([{]"
    character(len=*), parameter :: IF_NAME_MAIN_REGULAR_EXPRESSION = &
                                   "^if[[:space:]]+__name__[[:space:]]+==[[:space:]]+\(?1:[""']\)__main__\1" &
                                   //"[[:space:]]*:"
    character(len=*), parameter :: LINE_START_WHITESPACE_REGULAR_EXPRESSION = &
                                   "^[[:space:]]*"
    character(len=*), parameter :: ASSIGNMENT_STATEMENT_REGULAR_EXPRESSION = &
                                   "[^"//LF//"%&*+/<->^|~-]\(?:\(?:\*\*\|//\|<<\|>>\|[%&*+/|^-]\)?=\)\([^"//LF//"%&*+/<-" &
                                   //">^|~-]\)"
    character(len=*), parameter :: EMPTY_LINE_REGULAR_EXPRESSION = &
                                   "^[[:space:]]*\(.*\)[[:space:]]*$"
    character(len=*), parameter :: CODING_COOKIE_REGULAR_EXPRESSION = &
                                   "^#[[:space:]]*\(?:coding[:=][[:space:]]*\(?1:\(?:[[:word:]]\|-\)+\)\|-" &
                                   //"\*-[[:space:]]*coding:[[:space:]]*\(?1:\(?:[[:word:]]\|-\)+\)[[:space:" &
                                   //"]]*-\*-\|vim:[[:space:]]*set[[:space:]]+fileencoding[[:space:]]*=[[:sp" &
                                   //"ace:]]*\(?1:\(?:[[:word:]]\|-\)+\)[[:space:]]*:\)"
    character(len=*), parameter :: OUTLINE_REGULAR_EXPRESSION = &
                                   "[[:space:]]*\_<\(?:def\|class\|if\|elif\|else\|try\|except\|finally\|f" &
                                   //"or\|while\|with\|match\|case\|async[[:space:]]+\(?:def\|for\|with\)\)\" &
                                   //"_>"
    character(len=*), parameter :: SELF_REGULAR_EXPRESSION = &
                                   "^\<self\>\."
    character(len=*), parameter :: STRING_DELIM_SYNTAX_REGULAR_EXPRESSION = &
                                   "\s|"
    character(len=*), parameter :: AUTO_MODE_REGULAR_EXPRESSION = &
                                   "\(?:\.\(?:p\(?:th\|y[iw]?\)\)\|/\(?:SCons\(?:\(?:crip\|truc\)t\)\)\)\'"
    character(len=*), parameter :: WALRUS_REGULAR_EXPRESSION = &
                                   "[^[:space:]]:$"

contains

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the Python modes, their keymaps, syntax tables and keywords.
    !> Not a port.
    recursive subroutine initialize_python()
        type(status_type) :: st

        if (is_syntax_table_live(python_mode_syntax_table)) return
        call forward_setting(python_2_support)

        if (.not. associated(python_index_menu_format_item_label_function)) then
            python_index_menu_format_item_label_function => format_python_index_menu_item_label
        end if

        if (.not. associated(python_index_menu_format_parent_item_label_function)) then
            python_index_menu_format_parent_item_label_function => format_python_index_menu_parent_item_label
        end if

        if (.not. associated(python_index_menu_format_parent_item_jump_label_function)) then
            python_index_menu_format_parent_item_jump_label_function => format_python_index_menu_parent_item_jump_label
        end if

        call forward_setting(python_indent_offset)
        call forward_setting(python_indent_guess_indent_offset_variable)
        call forward_setting(python_indent_guess_indent_offset_verbose)
        call forward_setting(python_indent_def_block_scale)
        call forward_setting(python_indent_block_parenthesis_deeper)
        call forward_setting(python_fill_docstring_style)

        if (.not. allocated(python_indent_trigger_commands)) then
            python_indent_trigger_commands = [character(len=32) :: "indent_for_tab_command", "yas_expand", "yas/expand"]
        end if

        if (.not. associated(python_forward_expression_function)) then
            python_forward_expression_function => navigation_forward_expression_default
        end if

        if (.not. associated(python_fill_comment_function)) python_fill_comment_function => python_fill_comment
        if (.not. associated(python_fill_string_function)) python_fill_string_function => python_fill_string
        if (.not. associated(python_fill_decorator_function)) python_fill_decorator_function => python_fill_decorator
        if (.not. associated(python_fill_parenthesis_function)) python_fill_parenthesis_function => python_fill_parenthesis

        if (.not. allocated(python_navigation_beginning_of_definition_regular_expression)) then
            python_navigation_beginning_of_definition_regular_expression = BEGINNING_OF_SUBPROGRAM_REGULAR_EXPRESSION
        end if

        if (.not. allocated(python_navigation_beginning_of_block_regular_expression)) then
            python_navigation_beginning_of_block_regular_expression = BEGINNING_OF_BLOCK_REGULAR_EXPRESSION
        end if

        call initialize_syntax_tables()
        call initialize_keywords()
        call initialize_rules()
        call append_locals_clearer(clear_python_locals)
        call major_modes%python_base_mode%define("python_base_mode", "Python", python_base_mode, python_base_mode_hook, &
                                                 parent=major_modes%programming_mode, syntax_table=python_mode_syntax_table)
        python_base_mode_map = major_modes%python_base_mode%map
        call major_modes%python_mode%define("python_mode", "Python", python_mode, python_mode_hook, &
                                            parent=major_modes%python_base_mode)
        python_mode_map = major_modes%python_mode%map
        ! Movement
        call bind_remap("backward_sentence", "python_navigation_backward_block")
        call bind_remap("forward_sentence", "python_navigation_forward_block")
        call bind_remap("backward_up_group", "python_navigation_backward_up_group")
        call bind_remap("up_group", "python_navigation_up_group")
        call bind_remap("mark_subprogram", "python_mark_definition")
        ! Indent specific
        call keymap_set(python_base_mode_map, "DEL", "python_indent_dedent_line_backspace", st)
        call keymap_set(python_base_mode_map, "<backtab>", "python_indent_dedent_line", st)
        call keymap_set(python_base_mode_map, "C-c <", "python_indent_shift_left", st)
        call keymap_set(python_base_mode_map, "C-c >", "python_indent_shift_right", st)
        call keymap_set(python_base_mode_map, "C-c C-j", "index_menu", st)
        ! Utilities
        call bind_remap("complete_name", "completion_at_point")
        call make_sparse_keymap(python_indent_repeat_map)
        call keymap_set(python_indent_repeat_map, "<", "python_indent_shift_left", st)
        call keymap_set(python_indent_repeat_map, ">", "python_indent_shift_right", st)
        commands%python_base_mode%execute => python_base_mode
        commands%python_mode%execute => python_mode
        commands%python_indent_guess_indent_offset%execute => execute_python_indent_guess_indent_offset
        commands%python_indent_dedent_line%execute => execute_python_indent_dedent_line
        commands%python_indent_dedent_line_backspace%execute => execute_python_indent_dedent_line_backspace
        commands%python_indent_shift_left%execute => execute_python_indent_shift_left
        commands%python_indent_shift_right%execute => execute_python_indent_shift_right
        commands%python_mark_definition%execute => execute_python_mark_definition
        commands%python_navigation_end_of_definition%execute => execute_python_navigation_end_of_definition
        commands%python_navigation_backward_definition%execute => execute_python_navigation_backward_definition
        commands%python_navigation_forward_definition%execute => execute_python_navigation_forward_definition
        commands%python_navigation_beginning_of_statement%execute => execute_python_navigation_beginning_of_statement
        commands%python_navigation_end_of_statement%execute => execute_python_navigation_end_of_statement
        commands%python_navigation_backward_statement%execute => execute_python_navigation_backward_statement
        commands%python_navigation_forward_statement%execute => execute_python_navigation_forward_statement
        commands%python_navigation_beginning_of_block%execute => execute_python_navigation_beginning_of_block
        commands%python_navigation_end_of_block%execute => execute_python_navigation_end_of_block
        commands%python_navigation_backward_block%execute => execute_python_navigation_backward_block
        commands%python_navigation_forward_block%execute => execute_python_navigation_forward_block
        commands%python_navigation_forward_expression%execute => execute_python_navigation_forward_expression
        commands%python_navigation_backward_expression%execute => execute_python_navigation_backward_expression
        commands%python_navigation_forward_expression_safe%execute => execute_python_navigation_forward_expression_safe
        commands%python_navigation_backward_expression_safe%execute => execute_python_navigation_backward_expression_safe
        commands%python_navigation_up_group%execute => execute_python_navigation_up_group
        commands%python_navigation_backward_up_group%execute => execute_python_navigation_backward_up_group
        commands%python_navigation_if_name_main%execute => execute_python_navigation_if_name_main
        commands%python_fill_paragraph%execute => execute_python_fill_paragraph
        call set_repeat_map(python_indent_repeat_map)
    end subroutine initialize_python

    ! Bind [remap FROM] to COMMAND in `python_base_mode_map'.
    ! Not a port.
    recursive subroutine bind_remap(from, command)
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call list_elements(list(value_type(name="remap"), value_type(name=from)), events)
        call define_key(python_base_mode_map, events, value_type(name=command), st)
    end subroutine bind_remap

    ! Not a port.
    recursive subroutine initialize_syntax_tables()
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
    end subroutine initialize_syntax_tables

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
    recursive subroutine initialize_keywords()
        character(len=*), parameter :: VA = "font_lock_variable_name_face", TY = "font_lock_type_face", &
                                       OP = "font_lock_operator_face", CO = "font_lock_constant_face"
        type(font_lock_keyword_type) :: kw(20)
        type(font_lock_highlight_type) :: h1(1), h2(2), a(1), h3(3)
        integer :: n

        h1(1) = h(1, "font_lock_function_name_face")
        kw(1) = font_lock_keyword(DEF_REGULAR_EXPRESSION, highlights=h1)
        h1(1) = h(1, TY)
        kw(2) = font_lock_keyword(CLASS_REGULAR_EXPRESSION, highlights=h1)
        python_font_lock_keywords_level_1 = kw(1:2)
        kw(3) = font_lock_keyword(KEYWORDS2_REGULAR_EXPRESSION)
        ! Builtins
        kw(4) = font_lock_keyword(BUILTINS_REGULAR_EXPRESSION, face="font_lock_builtin_face")
        python_font_lock_keywords_level_2 = kw(1:4)
        n = 0
        if (allocated(python_font_lock_keywords_maximum_decoration)) deallocate (python_font_lock_keywords_maximum_decoration)
        call append(font_lock_keyword(matcher=python_font_lock_f_strings))
        call append_level_2()
        ! Constants
        call append(font_lock_keyword(CONSTANTS_REGULAR_EXPRESSION, face=CO))
        ! Decorators.
        h1(1) = h(1, TY)
        call append(font_lock_keyword(DECORATOR_REGULAR_EXPRESSION, highlights=h1))
        ! Builtin Exceptions
        call append(font_lock_keyword(EXCEPTIONS_REGULAR_EXPRESSION, face=TY))
        ! single assignment with/without type hints, e.g.
        !   a: int = 5
        !   b: Tuple[Optional[int], Union[Sequence[string], string]] = (None, 'foo')
        !   c: Collection = {1, 2, 3}
        !   d: Mapping[int, string] = {1: 'bar', 2: 'baz'}
        h1(1) = h(0, TY)
        a(1) = font_lock_anchored(h1, regular_expression=SYMBOL_NAME_REGULAR_EXPRESSION, pre_match_form=assignment_1_pre_form)
        h3(1) = h(1, VA)
        h3(2) = h(3, OP)
        h3(3) = a(1)
        call append(font_lock_keyword(matcher=assignment_matcher_1, highlights=h3))
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
        a(1) = font_lock_anchored(h1, regular_expression=GROUPED_TARGET_REGULAR_EXPRESSION, pre_match_form=assignment_2_pre_form)
        h3(1) = h(1, VA)
        h3(2) = h(2, OP)
        h3(3) = a(1)
        call append(font_lock_keyword(matcher=assignment_matcher_2, highlights=h3))
        ! special cases
        !   (a) = 5
        !   [a] = 5,
        !   [*a] = 5, 6
        h2(1) = h(1, VA)
        h2(2) = h(2, OP)
        call append(font_lock_keyword(matcher=assignment_matcher_3, highlights=h2))
        ! Operators.
        call append(font_lock_keyword(OPERATOR_REGULAR_EXPRESSION, face=OP))
        ! escape sequences within bytes literals
        !   "\\" "\'" "\a" "\b" "\f" "\n" "\r" "\t" "\v"
        !   "\ooo" character with octal value ooo
        !   "\xhh" character with hex value hh
        h1(1) = h(1, CO, FONT_LOCK_OVERRIDE_REPLACE)
        call append(font_lock_keyword(matcher=bytes_escape_matcher, highlights=h1))
        ! escape sequences within string literals, the same as appear in bytes
        ! literals in addition to:
        !   "\uxxxx" Character with 16-bit hex value xxxx
        !   "\Uxxxxxxxx" Character with 32-bit hex value xxxxxxxx
        !   "\N{name}" Character named name in the Unicode database
        call append(font_lock_keyword(matcher=string_escape_matcher, highlights=h1))

    contains

        subroutine append(k)
            type(font_lock_keyword_type), intent(in) :: k

            n = n + 1

            if (.not. allocated(python_font_lock_keywords_maximum_decoration)) then
                allocate (python_font_lock_keywords_maximum_decoration(0))
            end if

            python_font_lock_keywords_maximum_decoration = [python_font_lock_keywords_maximum_decoration, k]
        end subroutine append

        subroutine append_level_2()
            integer :: k

            do k = 1, size(python_font_lock_keywords_level_2)
                call append(python_font_lock_keywords_level_2(k))
            end do
        end subroutine append_level_2
    end subroutine initialize_keywords

    ! The rules of python-syntax-propertize-function.
    ! Not a port.
    recursive subroutine initialize_rules()
        type(syntax_highlight_type) :: hl

        if (allocated(propertize_rules%rules)) return
        allocate (propertize_rules%rules(1))
        propertize_rules%rules(1)%regular_expression = TRIPLE_QUOTES_REGULAR_EXPRESSION
        hl = syntax_highlight_type(group=0)
        hl%after => stringify_after
        propertize_rules%rules(1)%highlights = [hl]
    end subroutine initialize_rules

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

        call goto_character(start)
        call propertize_rules%apply(start, end, status)
    end subroutine python_syntax_propertize_function

    ! kill-all-local-variables: forget the current buffer's navigation
    ! cache.
    ! Not a port.
    recursive subroutine clear_python_locals()
        integer :: k

        k = navigation_cache_index()
        if (k > 0) navigation_caches(k)%serial = 0
    end subroutine clear_python_locals

    ! ------------------------------------------------------------------
    ! Font-lock and syntax
    ! ------------------------------------------------------------------

    ! The parse state at point (syntax-ppss).
    ! Not a port.
    recursive function parser_state_here() result(parser_state)
        type(parse_state_type) :: parser_state

        call syntax_parser_state(parser_state)
    end function parser_state_here

    ! The parse state at POSITION, leaving point there ((syntax-ppss POSITION)).
    ! Not a port.
    recursive function parser_state_at(position) result(parser_state)
        integer(int64), intent(in) :: position
        type(parse_state_type) :: parser_state

        call goto_character(position)
        call syntax_parser_state(parser_state)
    end function parser_state_at

    !> Return the start position if point is on TYPE using SYNTAX_PARSER_STATE
    !> (0 if not).  TYPE can be PYTHON_CONTEXT_COMMENT,
    !> PYTHON_CONTEXT_STRING, PYTHON_CONTEXT_SINGLE_QUOTED_STRING,
    !> PYTHON_CONTEXT_TRIPLE_QUOTED_STRING or PYTHON_CONTEXT_PARENTHESIS.  It
    !> returns the start character address of the specified TYPE.  Port of
    !> python.el:python-syntax-context.
    recursive integer(int64) function python_syntax_context(type, syntax_parser_state_state) result(position)
        integer, intent(in) :: type
        type(parse_state_type), intent(in), optional :: syntax_parser_state_state
        type(parse_state_type) :: parser_state

        if (present(syntax_parser_state_state)) then
            parser_state = syntax_parser_state_state
        else
            parser_state = parser_state_here()
        end if

        position = 0

        select case (type)
        case (PYTHON_CONTEXT_COMMENT)
            if (parser_state%incomment /= 0) position = parser_state%comstr_start
        case (PYTHON_CONTEXT_STRING)
            if (parser_state%instring >= 0) position = parser_state%comstr_start
        case (PYTHON_CONTEXT_SINGLE_QUOTED_STRING)
            if (parser_state%instring >= 0 .and. parser_state%instring /= ST_STRING_STYLE) position = parser_state%comstr_start
        case (PYTHON_CONTEXT_TRIPLE_QUOTED_STRING)
            if (parser_state%instring == ST_STRING_STYLE) position = parser_state%comstr_start
        case (PYTHON_CONTEXT_PARENTHESIS)
            position = parser_state%innermost_start
        case default
            continue
        end select
    end function python_syntax_context

    !> Return the context type using SYNTAX_PARSER_STATE.
    !> The type returned can be PYTHON_CONTEXT_COMMENT,
    !> PYTHON_CONTEXT_STRING or PYTHON_CONTEXT_PARENTHESIS (0 for none).  Port of
    !> python.el:python-syntax-context-type.
    recursive integer function python_syntax_context_type(syntax_parser_state_state) result(type)
        type(parse_state_type), intent(in), optional :: syntax_parser_state_state
        type(parse_state_type) :: parser_state

        if (present(syntax_parser_state_state)) then
            parser_state = syntax_parser_state_state
        else
            parser_state = parser_state_here()
        end if

        type = 0

        if (parser_state%comstr_start > 0) then
            if (parser_state%incomment /= 0) then
                type = PYTHON_CONTEXT_COMMENT
            else
                type = PYTHON_CONTEXT_STRING
            end if
        else if (parser_state%innermost_start > 0) then
            type = PYTHON_CONTEXT_PARENTHESIS
        end if
    end function python_syntax_context_type

    !> Return the start of the comment or string PARSER_STATE is inside (0 if
    !> neither).  Port of python.el:python-syntax-comment-or-string-p.
    recursive integer(int64) function is_python_syntax_comment_or_string(syntax_parser_state_state) result(position)
        type(parse_state_type), intent(in), optional :: syntax_parser_state_state
        type(parse_state_type) :: parser_state

        if (present(syntax_parser_state_state)) then
            parser_state = syntax_parser_state_state
        else
            parser_state = parser_state_here()
        end if

        position = parser_state%comstr_start
    end function is_python_syntax_comment_or_string

    !> Return true if char after point is a closing paren.  Port of
    !> python.el:python-syntax-closing-paren-p.
    recursive logical function is_python_syntax_closing_parenthesis()
        type(syntax_descriptor_type) :: d, close
        type(status_type) :: st
        integer :: c

        is_python_syntax_closing_parenthesis = .false.
        c = after_character_at(point())
        if (c < 0) return
        d = syntax_entry_at(c, point())
        call string_to_syntax(")", close, st)
        is_python_syntax_closing_parenthesis = iand(d%code, 65535) == iand(close%code, 65535)
    end function is_python_syntax_closing_parenthesis

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

    ! Return true if the pos where PARSER_STATE was found is inside an f-string.
    ! Port of python.el:python--f-string-p.
    recursive logical function is_f_string(parser_state)
        type(parse_state_type), intent(in) :: parser_state
        character(len=:), allocatable :: before_quote
        integer(int64) :: spos
        type(status_type) :: st
        character :: syntax

        is_f_string = .false.
        if (parser_state%instring < 0) return
        spos = parser_state%comstr_start - 1
        call buffer_substring(max(spos - 4, point_minimum()), min(spos + 2, point_maximum()), before_quote, st)
        if (string_match(F_STRING_START_REGULAR_EXPRESSION, before_quote, st) <= 0) return

        if (point_minimum() < spos) then
            is_f_string = .true.
        else
            syntax = character_syntax(before_character_at(spos))
            is_f_string = syntax /= "w" .and. syntax /= "_"
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
        type(parse_state_type) :: parser_state
        integer(int64) :: send, beginning, end, position, f, original_point
        type(status_type) :: st
        logical :: saved_ignore

        status = status_type()
        found = .false.
        parser_state = parser_state_here()

        do
            do while (.not. is_f_string(parser_state))
                call regular_expression_search_forward(F_STRING_START_REGULAR_EXPRESSION, f, st, bound=limit, noerror=SEARCH_MOVE)
                if (.not. st%ok() .or. f <= 0) exit
                parser_state = parser_state_here()
            end do

            if (.not. point() < limit) exit
            ! (goto-char (nth 8 ppss)) and (forward-sexp 1) with the
            ! default function.
            original_point = point()
            call goto_character(parser_state%comstr_start)
            call forward_expression_default_function(1_int64, st)

            if (st%ok()) then
                send = min(limit, point() - 1)
            else
                send = limit
            end if

            call goto_character(original_point)

            do
                call regular_expression_search_forward("{", f, st, bound=send, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit

                if (after_character_at(point()) == iachar("{")) then
                    ! Just skip over {{
                    call goto_character(point() + 1)
                else
                    beginning = match_beginning(0, st)
                    saved_ignore = parse_expression_ignore_comments%value
                    parse_expression_ignore_comments%value = .false.
                    position = scan_groups(point(), 1_int64, 1_int64, st)
                    parse_expression_ignore_comments%value = saved_ignore

                    if (st%ok() .and. position > 0) then
                        end = min(send, position)
                    else
                        end = send
                    end if

                    call goto_character(end)
                    call put_text_property(beginning, end, value_type(name="face"), value_type(), st)
                end if
            end do

            call goto_character(min(limit, send + 1))
            parser_state = parser_state_here()
        end do
    end subroutine python_font_lock_f_strings

    ! The matcher of the assignments for REGULAR_EXPRESSION: search for the next
    ! occurrence if REGULAR_EXPRESSION matched within a paren context (to avoid, e.g.,
    ! default values for arguments or passing arguments by name being
    ! treated as assignments) or is followed by an '=' sign (to avoid '=='
    ! being treated as an assignment).  Set point to the position one
    ! character before the end of the occurrence found so that subsequent
    ! searches can detect the '=' sign in chained assignment.  Port of
    ! python.el:python-font-lock-assignment-matcher.
    recursive subroutine assignment_matcher(regular_expression, limit, found)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        integer(int64) :: f
        type(status_type) :: st
        integer(int64), allocatable :: md(:)

        found = .false.

        do
            call regular_expression_search_forward(regular_expression, f, st, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) return
            md = match_data()

            if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0 .and. after_character_at(point()) /= iachar("=")) then
                call set_match_data(md)
                call goto_character(point() - 1)
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
        call assignment_matcher(ASSIGN1_REGULAR_EXPRESSION, limit, found)
    end subroutine assignment_matcher_1

    ! Not a port.
    recursive subroutine assignment_matcher_2(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call assignment_matcher(ASSIGN2_REGULAR_EXPRESSION, limit, found)
    end subroutine assignment_matcher_2

    ! Not a port.
    recursive subroutine assignment_matcher_3(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call assignment_matcher(ASSIGN3_REGULAR_EXPRESSION, limit, found)
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
        if (st%ok() .and. type_start > 0) call goto_character(type_start)
        limit = match_end(0, st)
    end subroutine assignment_1_pre_form

    ! (progn (goto-char (match-end 1)) (match-beginning 2))
    ! Port of python.el:python-font-lock-keywords-maximum-decoration (in part).
    recursive subroutine assignment_2_pre_form(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st

        ! go back after the first symbol
        call goto_character(match_end(1, st))
        ! limit the search until the assignment
        limit = match_beginning(2, st)
    end subroutine assignment_2_pre_form

    ! Match REGULAR_EXPRESSION within a string or bytes literal whose start matches
    ! START_REGULAR_EXPRESSION.  Port of python.el:python--string-bytes-literal-matcher.
    recursive subroutine string_bytes_literal_matcher(regular_expression, start_regular_expression, limit, found)
        character(len=*), intent(in) :: regular_expression, start_regular_expression
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        character(len=:), allocatable :: before_quote
        integer(int64), allocatable :: md(:)
        integer(int64) :: f, position
        type(parse_state_type) :: parser_state
        type(status_type) :: st

        found = .false.

        do
            call regular_expression_search_forward(regular_expression, f, st, bound=limit, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) return
            md = match_data()
            parser_state = parser_state_here()
            position = parser_state%comstr_start

            if (position > 0) then
                call buffer_substring(max(position - 4, point_minimum()), min(position + 1, point_maximum()), before_quote, st)
                call goto_character(point() - 1)

                if (string_match(start_regular_expression, before_quote, st) > 0) then
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
        call string_bytes_literal_matcher(BYTES_ESCAPE_REGULAR_EXPRESSION, NOT_RAW_BYTES_START_REGULAR_EXPRESSION, limit, found)
    end subroutine bytes_escape_matcher

    ! Not a port.
    recursive subroutine string_escape_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call string_bytes_literal_matcher(STRING_ESCAPE_REGULAR_EXPRESSION, NOT_RAW_STRING_START_REGULAR_EXPRESSION, limit, found)
    end subroutine string_escape_matcher

    !> Count number of quotes around point (max is 3).
    !> QUOTE_CHARACTER is the quote char to count.  Optional argument POINT is
    !> the point where scan starts (defaults to current point), and LIMIT
    !> is used to limit the scan.  Port of python.el:python-syntax-count-quotes.
    recursive integer function python_syntax_count_quotes(quote_character, position, limit) result(i)
        integer, intent(in) :: quote_character
        integer(int64), intent(in), optional :: position, limit
        integer(int64) :: p

        p = point()
        if (present(position)) p = position
        i = 0

        do while (i < 3)
            if (present(limit)) then
                if (.not. p + i < limit) exit
            end if

            if (after_character_at(p + i) /= quote_character) exit
            i = i + 1
        end do
    end function python_syntax_count_quotes

    !> Put `syntax_table' property correctly on single/triple quotes.
    !> Port of python.el:python-syntax-stringify.
    recursive subroutine python_syntax_stringify()
        type(parse_state_type) :: parser_state
        integer(int64) :: string_start, quote_starting_position, quote_ending_position, original_point
        type(status_type) :: st
        logical :: string_literal_concat, other_quotes

        original_point = point()
        call goto_character(point() - 3)
        parser_state = parser_state_here()
        call goto_character(original_point)
        string_start = 0
        if (parser_state%instring == ST_STRING_STYLE) string_start = parser_state%comstr_start
        other_quotes = .false.

        if (string_start > 0) then
            other_quotes = after_character_at(string_start) /= after_character_at(point() - 3)
        end if

        string_literal_concat = parser_state%instring >= 0 .and. parser_state%instring /= ST_STRING_STYLE
        quote_starting_position = point() - 3
        quote_ending_position = point()

        if (parser_state%incomment /= 0) then
            ! Inside a comment: do nothing.
            continue
        else if (other_quotes) then
            ! Inside of a string quoted with different triple quotes.
            continue
        else if (parser_state%quoted) then
            ! The first quote is escaped, so it's not part of a triple quote!
            call goto_character(quote_starting_position + 1)
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
            call put_text_property(quote_ending_position - 1, quote_ending_position, value_type(name="syntax_table"), fence(), st)
        else
            ! This set of quotes delimit the end of a string.  Put
            ! string fence syntax on first quote. (bug#49518)
            call put_text_property(quote_starting_position, quote_starting_position + 1, value_type(name="syntax_table"), fence(), &
                                   st)
        end if
    end subroutine python_syntax_stringify

    ! (string-to-syntax "|") as a `syntax_table' property value.
    ! Not a port.
    recursive function fence() result(v)
        use syntax_mod, only: syntax_descriptor_value
        type(value_type) :: v
        type(syntax_descriptor_type) :: d
        type(status_type) :: st

        call string_to_syntax("|", d, st)
        v = syntax_descriptor_value(d)
    end function fence

    ! ------------------------------------------------------------------
    ! Navigation
    ! ------------------------------------------------------------------

    ! Internal implementation of `python_navigation_beginning_of_definition'.
    ! With positive ARGUMENT search backwards, else search forwards.  Port of
    ! python.el:python-nav--beginning-of-defun.
    recursive logical function navigation_beginning_of_subprogram_1(argument_in) result(found)
        integer(int64), intent(in) :: argument_in
        integer(int64) :: argument, line_beginning_value, line_content_start, position, minimum_indentation, body_indentation, f, &
                          original_point
        logical :: backward, more
        type(status_type) :: st

        argument = argument_in
        if (argument == 0) argument = 1
        backward = argument > 0
        line_beginning_value = line_beginning_position()
        line_content_start = line_beginning_value + current_indentation()
        position = point()

        if (is_python_info_current_line_empty()) then
            minimum_indentation = huge(1_int64)
        else
            minimum_indentation = current_indentation()
        end if

        body_indentation = 0

        if (argument > 0) then
            if (python_info_looking_at_beginning_of_definition(check_statement=.true.)) then
                original_point = point()
                f = python_navigation_beginning_of_statement()
                body_indentation = current_indentation() + python_indent_offset%value
                call goto_character(original_point)
            else
                original_point = point()

                do
                    if (python_navigation_backward_block() == 0) exit
                    more = .not. python_info_looking_at_beginning_of_definition()
                    if (.not. more) more = current_indentation() >= minimum_indentation
                    if (.not. more) exit
                    minimum_indentation = min(minimum_indentation, current_indentation())
                end do

                if (python_info_looking_at_beginning_of_definition()) then
                    body_indentation = current_indentation() + python_indent_offset%value
                else
                    body_indentation = 0
                end if

                call goto_character(original_point)
            end if
        end if

        if (python_info_looking_at_beginning_of_definition(check_statement=.true.)) then
            ! If looking at beginning of subprogram, and if
            ! pos is > line-content-start, ensure a
            ! backward re search match this subprogram by
            ! going to end of line before calling
            ! re-search-fn bug#40563
            if (argument < 0) then
                f = python_navigation_end_of_statement()
            else if (is_python_info_continuation_line() > 0 .or. position > line_content_start) then
                f = python_navigation_end_of_statement()
            end if
        end if

        do
            if (backward) then
                call regular_expression_search_backward(python_navigation_beginning_of_definition_regular_expression, f, st, &
                                                        noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_forward(python_navigation_beginning_of_definition_regular_expression, f, st, &
                                                       noerror=SEARCH_NOERROR)
            end if

            if (.not. st%ok() .or. f <= 0) exit
            if (python_syntax_context_type() /= 0) cycle
            ! Handle nested subprograms when moving backwards by checking
            ! indentation.
            if (argument > 0 .and. current_indentation() /= 0 .and. current_indentation() >= body_indentation) cycle
            exit
        end do

        found = python_info_looking_at_beginning_of_definition(check_statement=.true.)

        if (found) then
            found = line_number_at_position(position) /= line_number_at_position()
            if (.not. found) found = point() >= line_beginning_value &
                                     .and. point() <= line_content_start .and. position > line_content_start
        end if

        if (found) then
            if (argument < 0) f = python_navigation_beginning_of_statement()
            call beginning_of_line(1_int64)
        else
            call goto_character(position)
        end if
    end function navigation_beginning_of_subprogram_1

    !> Move point to `beginning_of_subprogram'.
    !> With positive ARGUMENT search backwards else search forward.
    !> ARGUMENT 0 defaults to 1.  When searching backwards,
    !> nested subprograms are handled with care depending on current
    !> point position.  Return true if point is moved to
    !> `beginning_of_subprogram'.  Port of python.el:python-nav-beginning-of-defun
    !> (the `beginning_of_subprogram_function' of Python mode).
    recursive subroutine python_navigation_beginning_of_definition(argument_in, found, status)
        integer(int64), intent(in) :: argument_in
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: argument

        status = status_type()
        argument = argument_in
        if (argument == 0) argument = 1
        found = .false.

        do while (argument /= 0)
            if (.not. navigation_beginning_of_subprogram_1(argument)) exit
            found = .true.

            if (argument > 0) then
                argument = argument - 1
            else
                argument = argument + 1
            end if
        end do
    end subroutine python_navigation_beginning_of_definition

    ! (python-nav-beginning-of-defun ARGUMENT) as a function.
    ! Not a port.
    recursive logical function navigation_beginning_of_subprogram(argument) result(found)
        integer(int64), intent(in) :: argument
        type(status_type) :: st

        call python_navigation_beginning_of_definition(argument, found, st)
    end function navigation_beginning_of_subprogram

    !> Move point to the end of def or class.
    !> Returns false if point is not in a def or class.  Port of
    !> python.el:python-nav-end-of-defun.
    recursive logical function python_navigation_end_of_definition() result(moved)
        integer(int64) :: beginning_subprogram_indent, beginning_position, f, shortage
        logical :: ok, dummy

        beginning_position = point()
        moved = .false.
        ok = python_info_looking_at_beginning_of_definition()
        if (.not. ok) ok = navigation_beginning_of_subprogram(1_int64)
        if (.not. ok) ok = navigation_beginning_of_subprogram(-1_int64)
        if (.not. ok) return
        beginning_subprogram_indent = current_indentation()

        do
            f = python_navigation_end_of_statement()
            dummy = python_utility_forward_comment(1)
            if (.not. (current_indentation() > beginning_subprogram_indent .and. .not. is_end_of_buffer())) exit
        end do

        dummy = python_utility_forward_comment(-1)
        shortage = forward_line(1_int64)
        ! Ensure point moves forward.
        if (beginning_position > point()) call goto_character(beginning_position)
        ! Return non-nil if we did something (because then we were in a
        ! def/class).
        moved = beginning_position /= point()
    end function python_navigation_end_of_definition

    ! `python_navigation_end_of_definition' as `end_of_subprogram_function'.
    ! Not a port.
    recursive subroutine python_navigation_end_of_definition_function(status)
        type(status_type), intent(out) :: status
        logical :: moved

        status = status_type()
        moved = python_navigation_end_of_definition()
    end subroutine python_navigation_end_of_definition_function

    ! Move point using FN avoiding places within comments, strings or
    ! parens: return the position (0 if none) where FN moved to out of
    ! them, further in direction DIRECTORY (1 forward, -1 backward) than the
    ! previous place found.  Port of python.el:python-nav--syntactically.
    recursive integer(int64) function navigation_syntactically(fn, directory) result(found)
        procedure(navigation_motion_procedure) :: fn
        integer, intent(in) :: directory
        integer(int64) :: start_position, previous_position, newpos
        integer :: context
        logical :: compare_ok

        start_position = point()
        previous_position = 0

        do
            newpos = 0
            if (fn()) newpos = point()
            context = python_syntax_context_type()
            compare_ok = .false.

            if (newpos > 0) then
                if (previous_position == 0) then
                    compare_ok = .true.
                else if (directory > 0) then
                    compare_ok = newpos > previous_position
                else
                    compare_ok = newpos < previous_position
                end if
            end if

            if (context == 0 .and. newpos > 0 .and. compare_ok) then
                found = point()
                return
            else if (newpos > 0 .and. context /= 0) then
                previous_position = point()
            else
                if (newpos == 0) call goto_character(start_position)
                found = 0
                return
            end if
        end do
    end function navigation_syntactically

    ! Port of python.el:python-nav--forward-defun (in part: its forward search).
    recursive logical function search_subprogram_forward()
        integer(int64) :: f
        type(status_type) :: st

        call regular_expression_search_forward(python_navigation_beginning_of_definition_regular_expression, f, st, &
                                               noerror=SEARCH_NOERROR)
        search_subprogram_forward = st%ok() .and. f > 0
    end function search_subprogram_forward

    ! Port of python.el:python-nav--forward-defun (in part: its backward search).
    recursive logical function search_subprogram_backward()
        integer(int64) :: f
        type(status_type) :: st

        call regular_expression_search_backward(python_navigation_beginning_of_definition_regular_expression, f, st, &
                                                noerror=SEARCH_NOERROR)
        search_subprogram_backward = st%ok() .and. f > 0
    end function search_subprogram_backward

    ! Internal implementation of python-nav-{backward,forward}-subprogram.
    ! Uses ARGUMENT to define which function to call, and how many times
    ! repeat it.  Port of python.el:python-nav--forward-defun.
    recursive integer(int64) function navigation_forward_subprogram(argument_in) result(found)
        integer(int64), intent(in) :: argument_in
        integer(int64) :: argument

        argument = argument_in
        found = 0

        do while (argument > 0)
            found = navigation_syntactically(search_subprogram_forward, 1)
            if (found == 0) exit
            argument = argument - 1
        end do

        do while (argument < 0)
            found = navigation_syntactically(search_subprogram_backward, -1)
            if (found == 0) exit
            argument = argument + 1
        end do
    end function navigation_forward_subprogram

    !> Navigate to closer subprogram backward ARGUMENT times.
    !> Unlikely `python_navigation_beginning_of_definition' this doesn't care about
    !> nested definitions.  Port of python.el:python-nav-backward-defun.
    recursive integer(int64) function python_navigation_backward_definition(argument) result(found)
        integer(int64), intent(in) :: argument

        found = navigation_forward_subprogram(-argument)
    end function python_navigation_backward_definition

    !> Navigate to closer subprogram forward ARGUMENT times.
    !> Unlikely `python_navigation_beginning_of_definition' this doesn't care about
    !> nested definitions.  Port of python.el:python-nav-forward-defun.
    recursive integer(int64) function python_navigation_forward_definition(argument) result(found)
        integer(int64), intent(in) :: argument

        found = navigation_forward_subprogram(argument)
    end function python_navigation_forward_definition

    !> Move to start of current statement.  Port of
    !> python.el:python-nav-beginning-of-statement.
    recursive integer(int64) function python_navigation_beginning_of_statement() result(position)
        type(parse_state_type) :: parser_state
        integer(int64) :: context_point, shortage, original_point
        logical :: continued

        shortage = forward_line(0_int64)
        parser_state = parser_state_here()
        context_point = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS, parser_state)
        if (context_point == 0) context_point = python_syntax_context(PYTHON_CONTEXT_STRING, parser_state)

        if (is_beginning_of_buffer()) then
            continue
        else if (context_point > 0) then
            call goto_character(context_point)
            position = python_navigation_beginning_of_statement()
        else
            original_point = point()
            shortage = forward_line(-1_int64)
            continued = is_python_info_line_ends_backslash() > 0
            call goto_character(original_point)

            if (continued) then
                shortage = forward_line(-1_int64)
                position = python_navigation_beginning_of_statement()
            end if
        end if

        call back_to_indentation()
        position = point()
    end function python_navigation_beginning_of_statement

    !> Move to end of current statement.
    !> Optional argument NOEND is internal and makes the logic to not
    !> jump to the end of line when moving forward searching for the end
    !> of the statement.  Port of python.el:python-nav-end-of-statement.
    recursive integer(int64) function python_navigation_end_of_statement(noend) result(position)
        logical, intent(in), optional :: noend
        integer(int64) :: string_start, bs_position, last_string_end, f, shortage, p
        character(len=:), allocatable :: message
        integer(int64) :: beginning, end
        type(parse_state_type) :: parser_state
        type(status_type) :: st
        logical :: no_end, found

        no_end = .false.
        if (present(noend)) no_end = noend
        last_string_end = 0

        do
            if (.not. no_end) call goto_character(line_end_position())
            if (is_end_of_buffer()) exit
            string_start = python_syntax_context(PYTHON_CONTEXT_STRING)

            if (string_start > 0) then
                ! The condition can be nil if syntax table
                ! text properties and the `syntax-ppss' cache
                ! are somehow out of whack.  This has been
                ! observed when using `syntax-ppss' during
                ! narrowing.
                if (.not. string_start >= last_string_end) exit
                call goto_character(string_start)

                if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) > 0) then
                    ! Ended up inside a paren, roll again.
                    p = python_navigation_end_of_statement(.true.)
                else
                    ! This is not inside a paren, move to the
                    ! end of this string.
                    call goto_character(point() + python_syntax_count_quotes(after_character_at(point()), point()))
                    parser_state = parser_state_here()
                    found = .false.

                    if (parser_state%instring == ST_STRING_STYLE) then
                        do
                            call regular_expression_search_forward(TRIPLE_QUOTES_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
                            if (.not. st%ok() .or. f <= 0) exit

                            if (python_syntax_context(PYTHON_CONTEXT_STRING) == 0) then
                                found = .true.
                                exit
                            end if
                        end do
                    else
                        call goto_character(string_start)
                        call navigation_lisp_forward_expression(1_int64, st)
                        found = st%ok()
                    end if

                    if (.not. found) call goto_character(point_maximum())
                    last_string_end = point()
                end if
            else if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) > 0) then
                ! The statement won't end before we've escaped
                ! at least one level of parenthesis.
                p = scan_groups(point(), 1_int64, -1_int64, st)

                if (st%ok()) then
                    call goto_character(p)
                else
                    call scan_error_data(st, message, beginning, end)
                    call goto_character(end)
                end if
            else
                bs_position = is_python_info_line_ends_backslash()
                if (bs_position == 0) exit
                call goto_character(bs_position)
                shortage = forward_line(1_int64)
            end if
        end do

        position = point()
    end function python_navigation_end_of_statement

    !> Move backward to previous statement.
    !> With ARGUMENT, repeat.  See `python_navigation_forward_statement'.  Port of
    !> python.el:python-nav-backward-statement.
    recursive subroutine python_navigation_backward_statement(argument)
        integer(int64), intent(in) :: argument

        call python_navigation_forward_statement(-argument)
    end subroutine python_navigation_backward_statement

    !> Move forward to next statement.
    !> With ARGUMENT, repeat.  With negative argument, move ARGUMENT times
    !> backward to previous statement.  Port of
    !> python.el:python-nav-forward-statement.
    recursive subroutine python_navigation_forward_statement(argument_in)
        integer(int64), intent(in) :: argument_in
        integer(int64) :: argument, p
        logical :: dummy

        argument = argument_in

        do while (argument > 0)
            p = python_navigation_end_of_statement()
            dummy = python_utility_forward_comment()
            p = python_navigation_beginning_of_statement()
            argument = argument - 1
        end do

        do while (argument < 0)
            p = python_navigation_beginning_of_statement()
            dummy = python_utility_forward_comment(-1)
            p = python_navigation_beginning_of_statement()
            argument = argument + 1
        end do
    end subroutine python_navigation_forward_statement

    ! The index of the current buffer's entry in `navigation_caches' (0 if none).
    ! Not a port.
    integer function navigation_cache_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(navigation_caches)) return
        k = findloc(navigation_caches%serial, buffers(current)%serial, dim=1)
    end function navigation_cache_index

    ! Find or make the current buffer's entry in `navigation_caches', reusing the
    ! entry of a closed buffer; K is its index (0 without a current
    ! buffer).  Not a port.
    recursive subroutine make_navigation_cache_entry(k)
        integer, intent(out) :: k
        type(navigation_cache_type) :: entry

        k = navigation_cache_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(navigation_caches)) allocate (navigation_caches(0))

        do k = 1, size(navigation_caches)
            if (.not. buffer_serial_live(navigation_caches(k)%serial)) exit
        end do

        if (k > size(navigation_caches)) navigation_caches = [navigation_caches, entry]
        navigation_caches(k) = navigation_cache_type(serial=buffers(current)%serial)
    end subroutine make_navigation_cache_entry

    ! The current buffer's `buffer_characters_modified_tick'.
    ! Not a port.
    recursive integer(int64) function characters_tick() result(tick)
        type(status_type) :: st

        call buffer_characters_modified_tick(current_buffer(), tick, st)
    end function characters_tick

    ! Get value from the navigation cache.
    ! If the current buffer is not modified, the navigation cache is searched
    ! using KIND and the current line number as a key.  Port of
    ! python.el:python-nav-cache-get (0 for nil).
    recursive integer(int64) function navigation_cache_get(kind) result(target)
        integer, intent(in) :: kind
        integer(int64) :: line
        integer :: k, i

        target = 0
        k = navigation_cache_index()
        if (k == 0) return
        if (characters_tick() /= navigation_caches(k)%tick) return
        if (.not. allocated(navigation_caches(k)%entries)) return
        line = line_number_at_position(absolute=.true.)

        do i = 1, size(navigation_caches(k)%entries)
            if (navigation_caches(k)%entries(i)%kind == kind .and. navigation_caches(k)%entries(i)%line == line) then
                target = navigation_caches(k)%entries(i)%target
                return
            end if
        end do
    end function navigation_cache_get

    ! Add a key-value pair to the navigation cache.
    ! Invalidate the navigation cache if the current buffer has been modified.
    ! Then add a key-value pair to the navigation cache.  The key consists of
    ! KIND and CURRENT line number, and the value is TARGET position.
    ! Port of python.el:python-nav-cache-set.
    recursive subroutine navigation_cache_set(kind, current, target)
        integer, intent(in) :: kind
        integer(int64), intent(in) :: current, target
        integer(int64) :: tick
        integer :: k

        call make_navigation_cache_entry(k)
        if (k == 0) return
        tick = characters_tick()

        if (tick /= navigation_caches(k)%tick .or. .not. allocated(navigation_caches(k)%entries)) then
            if (allocated(navigation_caches(k)%entries)) deallocate (navigation_caches(k)%entries)
            allocate (navigation_caches(k)%entries(0))
            navigation_caches(k)%tick = tick
        end if

        navigation_caches(k)%entries = [navigation_cache_entry_type(kind, current, target), navigation_caches(k)%entries]
    end subroutine navigation_cache_set

    !> Move to start of current block (its position, 0 if none).  Port of
    !> python.el:python-nav-beginning-of-block.
    recursive integer(int64) function python_navigation_beginning_of_block() result(position)
        integer(int64) :: target, current

        target = navigation_cache_get(NAVIGATION_BEGINNING_OF_BLOCK)

        if (target > 0) then
            call goto_character(target)
            position = point()
        else
            current = line_number_at_position(absolute=.true.)
            position = navigation_beginning_of_block_1()
            call navigation_cache_set(NAVIGATION_BEGINNING_OF_BLOCK, current, position)
        end if
    end function python_navigation_beginning_of_block

    ! Move to start of current block.
    ! This is an internal implementation of `python_navigation_beginning_of_block'
    ! without the navigation cache.  Port of
    ! python.el:python-nav--beginning-of-block.
    recursive integer(int64) function navigation_beginning_of_block_1() result(position)
        integer(int64) :: starting_position, p, shortage, block_matching_indent
        type(status_type) :: st

        starting_position = point()

        ! Go to first line beginning a statement
        do
            if (is_beginning_of_buffer()) exit
            p = python_navigation_beginning_of_statement()
            if (.not. (is_python_info_current_line_comment() .or. is_python_info_current_line_empty())) exit
            shortage = forward_line(-1_int64)
        end do

        p = python_navigation_beginning_of_statement()

        if (looking_at(BLOCK_START_REGULAR_EXPRESSION, st)) then
            position = point()
            return
        end if

        block_matching_indent = current_indentation() - python_indent_offset%value

        do
            if (python_navigation_backward_block() == 0) exit
            if (.not. current_indentation() > block_matching_indent) exit
        end do

        position = 0

        if (looking_at(BLOCK_START_REGULAR_EXPRESSION, st)) then
            if (current_indentation() == block_matching_indent) position = point()
        end if

        if (position == 0) call goto_character(starting_position)
    end function navigation_beginning_of_block_1

    !> Move to end of current block (its position, 0 if none).  Port of
    !> python.el:python-nav-end-of-block.
    recursive integer(int64) function python_navigation_end_of_block() result(position)
        integer(int64) :: target, current

        target = navigation_cache_get(NAVIGATION_END_OF_BLOCK)

        if (target > 0) then
            call goto_character(target)
            position = point()
        else
            current = line_number_at_position(absolute=.true.)
            position = navigation_end_of_block_1()
            call navigation_cache_set(NAVIGATION_END_OF_BLOCK, current, position)
        end if
    end function python_navigation_end_of_block

    ! Move to end of current block.
    ! This is an internal implementation of `python_navigation_end_of_block' without
    ! the navigation cache.  Port of python.el:python-nav--end-of-block.
    recursive integer(int64) function navigation_end_of_block_1() result(position)
        integer(int64) :: block_indentation, p, shortage
        logical :: dummy

        position = 0
        if (python_navigation_beginning_of_block() == 0) return
        block_indentation = current_indentation()
        p = python_navigation_end_of_statement()

        do
            shortage = forward_line(1_int64)
            if (is_end_of_buffer()) exit

            if (current_indentation() > block_indentation) then
                p = python_navigation_end_of_statement()
                cycle
            end if

            if (is_python_info_current_line_comment() .or. is_python_info_current_line_empty()) cycle
            exit
        end do

        dummy = python_utility_forward_comment(-1)
        position = point()
    end function navigation_end_of_block_1

    !> Move backward to previous block of code.
    !> With ARGUMENT, repeat.  See `python_navigation_forward_block'.  Port of
    !> python.el:python-nav-backward-block.
    recursive integer(int64) function python_navigation_backward_block(argument) result(position)
        integer(int64), intent(in), optional :: argument
        integer(int64) :: n

        n = 1
        if (present(argument)) n = argument
        position = python_navigation_forward_block(-n)
    end function python_navigation_backward_block

    !> Move forward to next block of code.
    !> With ARGUMENT, repeat.  With negative argument, move ARGUMENT times
    !> backward to previous block.  Port of python.el:python-nav-forward-block.
    recursive integer(int64) function python_navigation_forward_block(argument_in) result(position)
        integer(int64), intent(in), optional :: argument_in
        integer(int64) :: argument, original_argument, starting_position, p, f
        type(status_type) :: st

        argument = 1
        if (present(argument_in)) argument = argument_in
        starting_position = point()
        original_argument = argument

        do while (argument > 0)
            p = python_navigation_end_of_statement()

            do
                call regular_expression_search_forward(FORWARD_BLOCK_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit
                if (python_syntax_context_type() == 0) exit
            end do

            argument = argument - 1
        end do

        do while (argument < 0)
            p = python_navigation_beginning_of_statement()

            do
                call regular_expression_search_backward(FORWARD_BLOCK_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit
                if (python_syntax_context_type() == 0) exit
            end do

            argument = argument + 1
        end do

        p = python_navigation_beginning_of_statement()
        position = 0

        if ((original_argument > 0 .and. point() < starting_position) .or. .not. looking_at(BLOCK_START_REGULAR_EXPRESSION, &
                                                                                            st)) then
            call goto_character(starting_position)
        else if (point() /= starting_position) then
            position = point()
        end if
    end function python_navigation_forward_block

    ! Standard version `forward_expression'.
    ! It ignores completely the value of `forward_expression_function' by
    ! setting it to nil before calling `forward_expression'.  With positive
    ! ARGUMENT move forward only one expression, else move backwards.  Port of
    ! python.el:python-nav--lisp-forward-sexp.
    recursive subroutine navigation_lisp_forward_expression(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (argument > 0) then
            call forward_expression_default_function(1_int64, status)
        else
            call forward_expression_default_function(-1_int64, status)
        end if
    end subroutine navigation_lisp_forward_expression

    ! Safe version of standard `forward_expression'.
    ! When at end of expression (i.e. looking at an opening/closing paren)
    ! skips it instead of throwing an error.  With positive ARGUMENT move
    ! forward only one expression, else move backwards.  Port of
    ! python.el:python-nav--lisp-forward-sexp-safe.
    recursive subroutine navigation_lisp_forward_expression_safe(argument)
        integer(int64), intent(in) :: argument
        integer(int64) :: f
        type(status_type) :: st

        call navigation_lisp_forward_expression(argument, st)
        if (st%ok()) return

        do
            if (argument > 0) then
                call regular_expression_search_forward(CLOSE_PARENTHESIS_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
            else
                call regular_expression_search_backward(OPEN_PARENTHESIS_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
            end if

            if (.not. st%ok() .or. f <= 0) exit
            if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0) exit
        end do
    end subroutine navigation_lisp_forward_expression_safe

    ! The syntax class of the character at POSITION (syntax-class (syntax-after
    ! POSITION)), -1 if none.
    ! Not a port.
    recursive integer function class_after(position)
        integer(int64), intent(in) :: position
        type(syntax_descriptor_type) :: d
        integer :: c

        class_after = -1
        c = after_character_at(position)
        if (c < 0) return
        d = syntax_entry_at(c, position)
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

    ! The kinds of place `navigation_forward_expression_1' tells apart.
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
    ! With positive optional argument DIRECTORY direction move forward, else
    ! backwards.  When optional argument SAFE is true do not throw
    ! errors when at end of expression, skip it instead.  With optional
    ! argument SKIP_PARENTHESES_P force expression motion to ignore parenthesized
    ! expressions when looking at them in either direction.  Port of
    ! python.el:python-nav--forward-sexp.
    recursive subroutine navigation_forward_expression_1(directory_in, safe, skip_parentheses_p, status)
        integer(int64), intent(in) :: directory_in
        logical, intent(in) :: safe, skip_parentheses_p
        type(status_type), intent(out) :: status
        integer(int64), parameter :: BLOCK_START = 1, BLOCK_END = 2, STATEMENT_START = 3, STATEMENT_END = 4, &
                                     STARTS_BLOCK = 5, ENDS_BLOCK = 6
        integer(int64) :: directory, next_expression_position, original_point, p
        integer :: context_type, context, next_expression_context
        logical :: forward_p, at_parenthesis, dummy

        status = status_type()
        if (directory_in == 0) return

        if (directory_in > 0) then
            directory = 1
        else
            directory = -1
        end if

        forward_p = directory > 0
        context_type = python_syntax_context_type()

        if (context_type == PYTHON_CONTEXT_STRING .or. context_type == PYTHON_CONTEXT_COMMENT) then
            ! Inside of a string, get out of it.
            call forward_expression_default_function(directory, status)
            return
        end if

        at_parenthesis = context_type == PYTHON_CONTEXT_PARENTHESIS

        if (.not. at_parenthesis) then
            if (forward_p) then
                at_parenthesis = class_after(point()) == class_of("(")
            else
                at_parenthesis = class_after(point() - 1) == class_of(")")
            end if
        end if

        if (.not. skip_parentheses_p .and. at_parenthesis) then
            ! Inside a paren or looking at it, lisp knows what to do.
            if (safe) then
                call navigation_lisp_forward_expression_safe(directory)
            else
                call navigation_lisp_forward_expression(directory, status)
            end if

            return
        end if

        ! This part handles the lispy feel of
        ! `python-nav-forward-sexp'.  Knowing everything about the
        ! current context and the context of the next expression tries to
        ! follow the lisp expression motion commands in a symmetric manner.
        context = expression_context(.false.)
        original_point = point()

        if (safe) then
            call navigation_lisp_forward_expression_safe(directory)
        else
            call navigation_lisp_forward_expression(directory, status)
        end if

        next_expression_position = point()
        call goto_character(original_point)
        if (.not. status%ok()) return
        call goto_character(next_expression_position)
        next_expression_context = expression_context(.true.)
        call goto_character(original_point)

        if (forward_p) then
            if (.not. is_end_of_buffer() .and. is_python_info_current_line_empty()) then
                dummy = python_utility_forward_comment(int(directory))
                call navigation_forward_expression_1(directory, safe, skip_parentheses_p, status)
            else if (context == BLOCK_START) then
                p = python_navigation_end_of_block()
            else if (context == STATEMENT_START) then
                p = python_navigation_end_of_statement()
            else if ((context == STATEMENT_END .or. context == BLOCK_END) .and. next_expression_context == ENDS_BLOCK) then
                call goto_character(next_expression_position)
                p = python_navigation_end_of_block()
            else if ((context == STATEMENT_END .or. context == BLOCK_END) .and. next_expression_context == STARTS_BLOCK) then
                call goto_character(next_expression_position)
                p = python_navigation_end_of_block()
            else if (context == STATEMENT_END .or. context == BLOCK_END) then
                call goto_character(next_expression_position)
                p = python_navigation_end_of_statement()
            else
                call goto_character(next_expression_position)
            end if
        else
            if (.not. is_beginning_of_buffer() .and. is_python_info_current_line_empty()) then
                dummy = python_utility_forward_comment(int(directory))
                call navigation_forward_expression_1(directory, safe, skip_parentheses_p, status)
            else if (context == BLOCK_END) then
                p = python_navigation_beginning_of_block()
            else if (context == STATEMENT_END) then
                p = python_navigation_beginning_of_statement()
            else if ((context == STATEMENT_START .or. context == BLOCK_START) .and. next_expression_context == STARTS_BLOCK) then
                call goto_character(next_expression_position)
                p = python_navigation_beginning_of_block()
            else if ((context == STATEMENT_START .or. context == BLOCK_START) .and. next_expression_context == ENDS_BLOCK) then
                call goto_character(next_expression_position)
                p = python_navigation_beginning_of_block()
            else if (context == STATEMENT_START .or. context == BLOCK_START) then
                call goto_character(next_expression_position)
                p = python_navigation_beginning_of_statement()
            else
                call goto_character(next_expression_position)
            end if
        end if
    end subroutine navigation_forward_expression_1

    !> Move forward across expressions.
    !> With ARGUMENT, do it that many times.  Negative arg -N means move
    !> backward N times.  When optional argument SAFE is true, no error
    !> condition occurs at the end of an expression: it is skipped.  With optional
    !> argument SKIP_PARENTHESES_P force expression motion to ignore parenthesized
    !> expressions when looking at them in either direction (forced to true
    !> in interactive calls).  Port of python.el:python-nav-forward-sexp
    !> (the default `python_forward_expression_function').
    recursive subroutine python_navigation_forward_expression(argument_in, status, safe, skip_parentheses_p)
        integer(int64), intent(in) :: argument_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: safe, skip_parentheses_p
        integer(int64) :: argument
        logical :: sf, skip

        status = status_type()
        argument = argument_in
        sf = .false.
        if (present(safe)) sf = safe
        skip = .false.
        if (present(skip_parentheses_p)) skip = skip_parentheses_p

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
            skip = associated(real_this_command, commands%forward_expression) &
                   .or. associated(real_this_command, commands%backward_expression) &
                   .or. associated(real_this_command, commands%python_navigation_forward_expression) &
                   .or. associated(real_this_command, commands%python_navigation_backward_expression) &
                   .or. associated(real_this_command, commands%python_navigation_forward_expression_safe)
        end if

        do while (argument > 0)
            call navigation_forward_expression_1(1_int64, sf, skip, status)
            if (.not. status%ok()) return
            argument = argument - 1
        end do

        do while (argument < 0)
            call navigation_forward_expression_1(-1_int64, sf, skip, status)
            if (.not. status%ok()) return
            argument = argument + 1
        end do
    end subroutine python_navigation_forward_expression

    ! (python-nav-forward-sexp ARGUMENT) as the default
    ! `python_forward_expression_function'.
    ! Not a port.
    recursive subroutine navigation_forward_expression_default(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        call python_navigation_forward_expression(argument, status)
    end subroutine navigation_forward_expression_default

    !> Move backward across expressions.
    !> With ARGUMENT, do it that many times.  Negative arg -N means move
    !> forward N times.  When optional argument SAFE is true, no error
    !> condition occurs at the end of an expression: it is skipped.  With optional
    !> argument SKIP_PARENTHESES_P force expression motion to ignore parenthesized
    !> expressions when looking at them in either direction (forced to true
    !> in interactive calls).  Port of python.el:python-nav-backward-sexp.
    recursive subroutine python_navigation_backward_expression(argument, status, safe, skip_parentheses_p)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: safe, skip_parentheses_p

        call python_navigation_forward_expression(-argument, status, safe, skip_parentheses_p)
    end subroutine python_navigation_backward_expression

    !> Move forward safely across expressions.
    !> With ARGUMENT, do it that many times.  Negative arg -N means move
    !> backward N times.  With optional argument SKIP_PARENTHESES_P force
    !> expression motion to ignore parenthesized expressions when looking at
    !> them in either direction (forced to true in interactive calls).
    !> Port of python.el:python-nav-forward-sexp-safe.
    recursive subroutine python_navigation_forward_expression_safe(argument, status, skip_parentheses_p)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: skip_parentheses_p

        call python_navigation_forward_expression(argument, status, .true., skip_parentheses_p)
    end subroutine python_navigation_forward_expression_safe

    !> Move backward safely across expressions.
    !> With ARGUMENT, do it that many times.  Negative arg -N means move
    !> forward N times.  With optional argument SKIP_PARENTHESES_P force expression
    !> motion to ignore parenthesized expressions when looking at them in
    !> either direction (forced to true in interactive calls).  Port of
    !> python.el:python-nav-backward-sexp-safe.
    recursive subroutine python_navigation_backward_expression_safe(argument, status, skip_parentheses_p)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: skip_parentheses_p

        call python_navigation_backward_expression(argument, status, .true., skip_parentheses_p)
    end subroutine python_navigation_backward_expression_safe

    ! Internal implementation of `python_navigation_up_group'.
    ! DIRECTORY is always 1 or -1 and comes sanitized from
    ! `python_navigation_up_group' calls.  Port of python.el:python-nav--up-list.
    recursive subroutine navigation_up_group_1(directory, status)
        integer(int64), intent(in) :: directory
        type(status_type), intent(out) :: status
        integer(int64) :: parent_end_position, original_point, indentation, previous_block_position, p
        integer :: context
        logical :: forward_p

        status = status_type()
        context = python_syntax_context_type()
        forward_p = directory > 0

        if (context == PYTHON_CONTEXT_STRING .or. context == PYTHON_CONTEXT_COMMENT) then
            continue
        else if (context == PYTHON_CONTEXT_PARENTHESIS) then
            call up_group_default(directory, status)
        else if (forward_p .and. is_python_info_end_of_block()) then
            original_point = point()
            indentation = 0
            if (python_navigation_beginning_of_block() > 0) indentation = current_indentation()

            do while (indentation > 0)
                if (.not. current_indentation() >= indentation) exit
                if (python_navigation_backward_block() == 0) exit
            end do

            parent_end_position = python_navigation_end_of_block()
            call goto_character(original_point)
            if (parent_end_position > point()) call goto_character(parent_end_position)
        else if (forward_p) then
            p = python_navigation_end_of_block()
        else if (.not. forward_p .and. current_indentation() > 0 .and. is_python_info_beginning_of_block()) then
            original_point = point()
            indentation = current_indentation()

            do
                if (python_navigation_backward_block() == 0) exit
                if (.not. current_indentation() >= indentation) exit
            end do

            previous_block_position = point()
            call goto_character(original_point)
            if (point() > previous_block_position) call goto_character(previous_block_position)
        else if (.not. forward_p) then
            p = python_navigation_beginning_of_block()
        end if
    end subroutine navigation_up_group_1

    ! (let ((forward-sexp-function)) (up-list DIRECTORY)): up-list-default-function
    ! without a `forward_expression_function'.
    ! Not a port.
    recursive subroutine up_group_default(directory, status)
        integer(int64), intent(in) :: directory
        type(status_type), intent(out) :: status
        integer(int64) :: position

        position = scan_groups(point(), directory, 1_int64, status)
        if (.not. status%ok()) return

        if (position == 0) then
            if (directory > 0) then
                position = point_maximum()
            else
                position = point_minimum()
            end if
        end if

        call goto_character(position)
    end subroutine up_group_default

    !> Move forward out of one level of parentheses (or blocks).
    !> With ARGUMENT, do this that many times.
    !> A negative argument means move backward but still to a less deep spot.
    !> This command assumes point is not in a string or comment.  Port of
    !> python.el:python-nav-up-list.
    recursive subroutine python_navigation_up_group(argument_in, status)
        integer(int64), intent(in) :: argument_in
        type(status_type), intent(out) :: status
        integer(int64) :: argument

        status = status_type()
        argument = argument_in

        do while (argument > 0)
            call navigation_up_group_1(1_int64, status)
            if (.not. status%ok()) return
            argument = argument - 1
        end do

        do while (argument < 0)
            call navigation_up_group_1(-1_int64, status)
            if (.not. status%ok()) return
            argument = argument + 1
        end do
    end subroutine python_navigation_up_group

    !> Move backward out of one level of parentheses (or blocks).
    !> With ARGUMENT, do this that many times.
    !> A negative argument means move forward but still to a less deep spot.
    !> This command assumes point is not in a string or comment.  Port of
    !> python.el:python-nav-backward-up-list.
    recursive subroutine python_navigation_backward_up_group(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        call python_navigation_up_group(-argument, status)
    end subroutine python_navigation_backward_up_group

    !> Move point at the beginning the __main__ block.
    !> When "if __name__ == '__main__':" is found returns its
    !> position, else returns 0.  Port of python.el:python-nav-if-name-main.
    recursive integer(int64) function python_navigation_if_name_main() result(position)
        integer(int64) :: original_point, f
        type(status_type) :: st

        original_point = point()
        call goto_character(point_minimum())
        position = 0

        do
            call regular_expression_search_forward(IF_NAME_MAIN_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) exit

            if (python_syntax_context_type() == 0) then
                call beginning_of_line()
                position = point()
                return
            end if
        end do

        call goto_character(original_point)
    end function python_navigation_if_name_main

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
        type(string_type), allocatable :: names(:)
        character(len=:), allocatable :: type, matched, first, second
        integer(int64) :: original_point, beginning_of_accessible_portion, end_of_accessible_portion_from_end, &
                          starting_indentation, starting_position, last_indent, minimum_indent, &
                          ending, p
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: first_run, has_last, accept, incl, moved
        integer :: k

        incl = .false.
        if (present(include_type)) incl = include_type
        ! (save-restriction (widen) (save-excursion ...))
        original_point = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        call widen()
        call end_of_line(1_int64)
        allocate (names(0))
        starting_indentation = current_indentation()
        starting_position = point()
        first_run = .true.
        has_last = .false.
        last_indent = 0

        do
            if (.not. navigation_beginning_of_subprogram(1_int64)) exit
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
                    call goto_character(starting_position)
                    ending = python_navigation_beginning_of_statement()
                    call beginning_of_line(1_int64)
                    accept = looking_at(python_navigation_beginning_of_definition_regular_expression, st)
                    call goto_character(p)
                end if

                if (.not. accept) then
                    p = point()
                    minimum_indent = current_indentation() + python_indent_offset%value

                    if (starting_indentation < minimum_indent) then
                        ! If the starting indentation is not
                        ! within the min subprogram indent make the
                        ! check fail.
                        ending = starting_position
                    else
                        ! Else go to the end of subprogram and add
                        ! up the current indentation to the
                        ! ending position.
                        moved = python_navigation_end_of_definition()

                        if (current_indentation() >= minimum_indent) then
                            ending = point() + 1 + current_indentation()
                        else
                            ending = point()
                        end if
                    end if

                    call goto_character(p)
                    accept = starting_position < ending
                end if
            end if

            call set_match_data(md)

            if (accept) then
                last_indent = current_indentation()
                has_last = .true.

                if (.not. incl .or. allocated(type)) then
                    call prepend_string(names, match_string_no_properties(1))
                else
                    matched = match_string_no_properties(0)
                    call split2(matched, first, second)
                    type = first
                    call prepend_string(names, second)
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
                name = name//names(k)%text
            end do
        end if

        call goto_character(original_point)
        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)

    contains

        ! The first two words of S (split-string at whitespace).
        subroutine split2(s, w1, w2)
            character(len=*), intent(in) :: s
            character(len=:), allocatable, intent(out) :: w1, w2
            character(len=*), parameter :: blanks = " "//TAB//LF
            integer :: i, b, e

            w1 = ""
            w2 = ""
            b = verify(s, blanks)
            if (b == 0) return
            e = scan(s(b:), blanks)
            i = merge(len(s) + 1, b + e - 1, e == 0)
            w1 = s(b:i - 1)
            if (i > len(s)) return
            b = verify(s(i:), blanks)
            if (b == 0) return
            b = i + b - 1
            e = scan(s(b:), blanks)
            i = merge(len(s) + 1, b + e - 1, e == 0)
            w2 = s(b:i - 1)
        end subroutine split2
    end subroutine python_info_current_definition

    !> `python_info_current_definition' without the type of the subprogram:
    !> Python mode's `add_log_current_subprogram_function'.  Not a port:
    !> python.el gives that variable python-info-current-defun.
    recursive subroutine get_python_current_definition(name)
        character(len=:), allocatable, intent(out) :: name

        call python_info_current_definition(name)
    end subroutine get_python_current_definition

    !> Return true if current statement opens a block.  Port of
    !> python.el:python-info-statement-starts-block-p.
    recursive logical function is_python_info_statement_starts_block() result(starts)
        integer(int64) :: original_point, p
        type(status_type) :: st

        original_point = point()
        p = python_navigation_beginning_of_statement()
        starts = looking_at(BLOCK_START_REGULAR_EXPRESSION, st)
        call goto_character(original_point)
    end function is_python_info_statement_starts_block

    !> Return true if point is at end of block.  Port of
    !> python.el:python-info-statement-ends-block-p.
    recursive logical function is_python_info_statement_ends_block() result(ends)
        integer(int64) :: original_point, current_statement, current_ind, next_statement, next_ind, end_of_statement_position, &
                          end_of_block_position
        logical :: dummy

        original_point = point()
        current_statement = python_navigation_beginning_of_statement()
        current_ind = current_indentation()
        call goto_character(original_point)
        call python_navigation_forward_statement(1_int64)
        next_statement = point()
        next_ind = current_indentation()
        call goto_character(original_point)
        ends = .false.
        if (current_statement < next_statement .and. current_ind <= next_ind) return
        end_of_statement_position = python_navigation_end_of_statement()
        dummy = python_utility_forward_comment(-1)
        end_of_statement_position = point()
        call goto_character(original_point)
        end_of_block_position = python_navigation_end_of_block()
        call goto_character(original_point)
        ends = end_of_block_position /= 0 .and. end_of_block_position == end_of_statement_position
    end function is_python_info_statement_ends_block

    !> Return true if point is at beginning of statement.  Port of
    !> python.el:python-info-beginning-of-statement-p.
    recursive logical function is_python_info_beginning_of_statement() result(p)
        integer(int64) :: original_point, b

        original_point = point()
        b = python_navigation_beginning_of_statement()
        call goto_character(original_point)
        p = original_point == b
    end function is_python_info_beginning_of_statement

    !> Return true if point is at end of statement.  Port of
    !> python.el:python-info-end-of-statement-p.
    recursive logical function is_python_info_end_of_statement() result(p)
        integer(int64) :: original_point, e

        original_point = point()
        e = python_navigation_end_of_statement()
        call goto_character(original_point)
        p = original_point == e
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
        integer(int64) :: original_point, e
        logical :: dummy

        original_point = point()
        e = python_navigation_end_of_statement()
        dummy = python_utility_forward_comment(-1)
        e = point()
        call goto_character(original_point)
        p = original_point == e
        if (p) p = is_python_info_statement_ends_block()
    end function is_python_info_end_of_block

    !> Return the point of the closest block the current line closes.
    !> Returns 0 if point is not on a dedenter statement or no opening
    !> block can be detected.  The latter case meaning current file is
    !> likely an invalid python file.  Port of
    !> python.el:python-info-dedenter-opening-block-position.
    recursive integer(int64) function python_info_dedenter_opening_block_position() result(position)
        integer(int64), allocatable :: positions(:)
        integer(int64) :: indentation, original_point
        integer :: k

        positions = python_info_dedenter_opening_block_positions()
        indentation = current_indentation()
        position = 0
        original_point = point()

        do k = 1, size(positions)
            call goto_character(positions(k))

            if (current_indentation() <= indentation) then
                position = positions(k)
                exit
            end if
        end do

        call goto_character(original_point)
    end function python_info_dedenter_opening_block_position

    ! The motion of `python_info_dedenter_opening_block_positions': search
    ! back for a block start at the start of its line.
    ! Port of python.el:python-info-dedenter-opening-block-positions (in part).
    recursive logical function search_block_start_at_beginning_of_line() result(found)
        integer(int64) :: f
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: at_beginning_of_line

        found = .false.

        do
            call regular_expression_search_backward(BLOCK_START_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) return
            md = match_data()
            at_beginning_of_line = looking_back(LINE_START_WHITESPACE_REGULAR_EXPRESSION, st, line_beginning_position())
            call set_match_data(md)

            if (at_beginning_of_line) then
                found = .true.
                return
            end if
        end do
    end function search_block_start_at_beginning_of_line

    !> Return points of blocks the current line may close sorted by closer.
    !> Returns an empty array if point is not on a dedenter statement or no
    !> opening block can be detected.  The latter case meaning current file
    !> is likely an invalid python file.  Port of
    !> python.el:python-info-dedenter-opening-block-positions.
    recursive function python_info_dedenter_opening_block_positions() result(opening_blocks)
        integer(int64), allocatable :: opening_blocks(:)
        integer(int64), allocatable :: collected(:), md(:)
        character(len=:), allocatable :: dedenter, keyword
        integer(int64) :: original_point, dedenter_position, current_line, indentation, p, shortage
        logical :: no_back_indent, take

        allocate (opening_blocks(0), collected(0))
        original_point = point()
        dedenter_position = is_python_info_dedenter_statement()

        if (dedenter_position > 0) then
            call goto_character(dedenter_position)
            current_line = line_beginning_position()
            dedenter = match_string_no_properties(0)

            do
                if (navigation_syntactically(search_block_start_at_beginning_of_line, -1) == 0) exit
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
                    shortage = python_navigation_end_of_statement()
                    shortage = forward_line(1_int64)
                    no_back_indent = .true.

                    do while (point() < current_line)
                        no_back_indent = current_indentation() > indentation .or. is_python_info_current_line_empty() &
                                         .or. is_python_info_current_line_comment()
                        if (.not. no_back_indent) exit
                        shortage = forward_line(1_int64)
                    end do

                    call set_match_data(md)
                    call goto_character(p)
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

        call goto_character(original_point)

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
        integer(int64) :: position, original_point
        character(len=:), allocatable :: text
        type(status_type) :: st

        position = python_info_dedenter_opening_block_position()
        if (position == 0) return
        original_point = point()
        call goto_character(position)
        call buffer_substring(point(), line_end_position(), text, st)
        call goto_character(original_point)
        call message("Closes "//text)
    end subroutine python_info_dedenter_opening_block_message

    !> Return point if current statement is a dedenter (0 if not).
    !> Sets `match_data' to the keyword that starts the dedenter
    !> statement.  Port of python.el:python-info-dedenter-statement-p.
    recursive integer(int64) function is_python_info_dedenter_statement() result(position)
        integer(int64) :: original_point, p, q
        integer(int64), allocatable :: md(:)
        type(status_type) :: st
        logical :: first_case, dummy

        original_point = point()
        position = 0
        p = python_navigation_beginning_of_statement()

        if (python_syntax_context_type() == 0) then
            if (looking_at(DEDENTER_REGULAR_EXPRESSION, st)) then
                ! Exclude the first "case" in the block.
                first_case = .false.

                if (match_string_no_properties(0) == "case") then
                    md = match_data()
                    q = point()
                    call back_to_indentation()
                    dummy = python_utility_forward_comment(-1)
                    first_case = before_character_at(point()) == iachar(":")
                    call goto_character(q)
                    call set_match_data(md)
                end if

                if (.not. first_case) position = point()
            end if
        end if

        call goto_character(original_point)
    end function is_python_info_dedenter_statement

    !> Return the backslash position if current line ends with backslash
    !> (0 if not).  With optional argument LINE_NUMBER, check that line
    !> instead.  Port of python.el:python-info-line-ends-backslash-p.
    recursive integer(int64) function is_python_info_line_ends_backslash(line_number) result(position)
        integer(int64), intent(in), optional :: line_number
        integer(int64) :: original_point, shortage

        original_point = point()
        if (present(line_number)) call python_utility_goto_line(line_number)

        do
            if (is_end_of_buffer()) exit
            call goto_character(line_end_position())
            if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0) exit
            if (before_character_at(point()) == iachar("\")) exit
            shortage = forward_line(1_int64)
        end do

        position = 0
        if (before_character_at(point()) == iachar("\")) position = point()
        call goto_character(original_point)
    end function is_python_info_line_ends_backslash

    !> Return the point where the backslashed line starts (0 if not).
    !> Optional argument LINE_NUMBER forces the line number to check against.
    !> Port of python.el:python-info-beginning-of-backslash.
    recursive integer(int64) function python_info_beginning_of_backslash(line_number) result(position)
        integer(int64), intent(in), optional :: line_number
        integer(int64) :: original_point, shortage, q
        logical :: in_parenthesis

        original_point = point()
        position = 0
        if (present(line_number)) call python_utility_goto_line(line_number)

        if (is_python_info_line_ends_backslash() > 0) then
            do
                q = point()
                call goto_character(line_beginning_position())
                in_parenthesis = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) > 0
                call goto_character(q)
                if (.not. in_parenthesis) exit
                shortage = forward_line(-1_int64)
            end do

            call back_to_indentation()
            position = point()
        end if

        call goto_character(original_point)
    end function python_info_beginning_of_backslash

    !> Check if current line is continuation of another.
    !> When current line is continuation of another return the point
    !> where the continued line ends (0 if not).  Port of
    !> python.el:python-info-continuation-line-p.
    recursive integer(int64) function is_python_info_continuation_line() result(position)
        integer(int64) :: original_point, line_start, context_start
        integer :: context_type
        logical :: dummy

        original_point = point()
        position = 0
        call back_to_indentation()
        context_type = python_syntax_context_type()
        line_start = line_number_at_position()
        context_start = 0
        if (context_type /= 0) context_start = python_syntax_context(context_type)

        if (context_type == PYTHON_CONTEXT_PARENTHESIS) then
            ! Lines inside a paren are always a continuation line
            ! (except the first one).
            dummy = python_utility_forward_comment(-1)
            position = point()
        else if (context_type == PYTHON_CONTEXT_STRING .or. context_type == PYTHON_CONTEXT_COMMENT) then
            ! move forward an roll again
            call goto_character(context_start)
            dummy = python_utility_forward_comment()
            position = is_python_info_continuation_line()
        else
            ! Not within a paren, string or comment, the only way
            ! we are dealing with a continuation line is that
            ! previous line contains a backslash, and this can
            ! only be the previous line from current
            call back_to_indentation()
            dummy = python_utility_forward_comment(-1)

            if (line_start - 1 == line_number_at_position()) then
                if (is_python_info_line_ends_backslash() > 0) position = point()
            end if
        end if

        call goto_character(original_point)
    end function is_python_info_continuation_line

    !> Return the start of the block if current line is a continuation of
    !> a block (0 if not).  Port of python.el:python-info-block-continuation-line-p.
    recursive integer(int64) function is_python_info_block_continuation_line() result(position)
        integer(int64) :: original_point, shortage
        type(status_type) :: st

        original_point = point()
        position = 0

        if (is_python_info_continuation_line() > 0) then
            shortage = forward_line(-1_int64)
            call back_to_indentation()
            if (looking_at(BLOCK_START_REGULAR_EXPRESSION, st)) position = point()
        end if

        call goto_character(original_point)
    end function is_python_info_block_continuation_line

    !> Check if current line is an assignment (the point after the
    !> operator, 0 if not).
    !> With argument CURRENT_LINE_ONLY is true, don't follow any
    !> continuations, just check the if current line is an assignment.
    !> Port of python.el:python-info-assignment-statement-p.
    recursive integer(int64) function is_python_info_assignment_statement(current_line_only) result(position)
        logical, intent(in), optional :: current_line_only
        integer(int64) :: original_point, f, p, q
        type(status_type) :: st
        logical :: found, only

        original_point = point()
        only = .false.
        if (present(current_line_only)) only = current_line_only
        found = .false.
        position = 0

        if (only) then
            call back_to_indentation()
        else
            p = python_navigation_beginning_of_statement()
        end if

        do while (.not. found)
            call regular_expression_search_forward(ASSIGNMENT_STATEMENT_REGULAR_EXPRESSION, f, st, bound=line_end_position(), &
                                                   noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) exit
            ! The assignment operator should not be inside a string.
            q = point()
            call goto_character(point() - len(match_string_no_properties(1)))
            found = python_syntax_context_type() == 0
            call goto_character(q)
        end do

        if (found) then
            p = skip_syntax_forward(" ")
            position = point()
        end if

        call goto_character(original_point)
    end function is_python_info_assignment_statement

    !> Check if current line is the first continuation of an assignment.
    !> When current line is continuation of another with an assignment
    !> return the point of the first non-blank character after the
    !> operator (0 if not).  Port of
    !> python.el:python-info-assignment-continuation-line-p.
    recursive integer(int64) function is_python_info_assignment_continuation_line() result(position)
        integer(int64) :: original_point, shortage

        original_point = point()
        position = 0

        if (is_python_info_continuation_line() > 0) then
            shortage = forward_line(-1_int64)
            position = is_python_info_assignment_statement(.true.)
        end if

        call goto_character(original_point)
    end function is_python_info_assignment_continuation_line

    !> Check if point is at `beginning_of_subprogram' using SYNTAX_PARSER_STATE.
    !> When CHECK_STATEMENT is true, the current statement is checked
    !> instead of the current physical line.  Port of
    !> python.el:python-info-looking-at-beginning-of-defun.
    recursive logical function python_info_looking_at_beginning_of_definition(syntax_parser_state_state, check_statement) result(p)
        type(parse_state_type), intent(in), optional :: syntax_parser_state_state
        logical, intent(in), optional :: check_statement
        integer(int64) :: original_point, q
        type(status_type) :: st
        logical :: check

        original_point = point()
        check = .false.
        if (present(check_statement)) check = check_statement
        if (check) q = python_navigation_beginning_of_statement()
        call beginning_of_line(1_int64)

        if (present(syntax_parser_state_state)) then
            p = python_syntax_context_type(syntax_parser_state_state) == 0
        else
            p = python_syntax_context_type() == 0
        end if

        if (p) p = looking_at(python_navigation_beginning_of_definition_regular_expression, st)
        call goto_character(original_point)
    end function python_info_looking_at_beginning_of_definition

    !> Check if point is at the beginning of block.  Port of
    !> python.el:python-info-looking-at-beginning-of-block.
    recursive logical function python_info_looking_at_beginning_of_block() result(p)
        integer(int64) :: position, q
        type(status_type) :: st

        position = point()
        q = python_navigation_beginning_of_statement()
        call beginning_of_line()
        p = point() <= position .and. position <= point() + current_indentation()
        if (p) p = looking_at(python_navigation_beginning_of_block_regular_expression, st)
        call goto_character(position)
    end function python_info_looking_at_beginning_of_block

    !> Return true if current line is a comment line.  Port of
    !> python.el:python-info-current-line-comment-p.
    recursive logical function is_python_info_current_line_comment()
        is_python_info_current_line_comment = after_character_at(line_beginning_position() + current_indentation()) &
                                              == iachar("#")
    end function is_python_info_current_line_comment

    !> Return true if current line is empty, ignoring whitespace.  Port of
    !> python.el:python-info-current-line-empty-p.
    recursive logical function is_python_info_current_line_empty() result(empty)
        integer(int64) :: original_point
        type(status_type) :: st
        logical :: matched

        original_point = point()
        call beginning_of_line(1_int64)
        matched = looking_at(EMPTY_LINE_REGULAR_EXPRESSION, st)
        empty = len(match_string_no_properties(1)) == 0
        call goto_character(original_point)
    end function is_python_info_current_line_empty

    !> Return true if point is in a docstring.
    !> When optional argument SYNTAX_PARSER_STATE is given, use that instead of
    !> point's current `syntax_parser_state'.  Port of python.el:python-info-docstring-p.
    recursive logical function is_python_info_docstring(syntax_parser_state_state) result(docstring)
        type(parse_state_type), intent(in), optional :: syntax_parser_state_state
        character(len=*), parameter :: regular_expression = "[uU]?[rR]?[""']"
        integer(int64) :: original_point, counter, indentation, backward_expression_point, last_backward_expression_point, p
        type(status_type) :: st
        logical :: dummy, more, has_last

        original_point = point()
        docstring = .false.

        if (present(syntax_parser_state_state)) then
            if (python_syntax_context(PYTHON_CONTEXT_STRING, syntax_parser_state_state) > 0) then
                call goto_character(syntax_parser_state_state%comstr_start)
            end if
        end if

        p = python_navigation_beginning_of_statement()
        counter = 1
        indentation = current_indentation()

        if (is_python_info_assignment_statement() == 0 .and. looking_at(regular_expression, st)) then
            ! Allow up to two consecutive docstrings only.
            has_last = .false.
            last_backward_expression_point = 0

            do while (counter <= 2)
                p = point()
                call python_navigation_backward_expression(1_int64, st)
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
                    if (more) more = looking_at(regular_expression, st)
                end if

                call goto_character(p)
                if (.not. more) exit
                ! Previous expression was a string, restore point.
                call goto_character(backward_expression_point)
                counter = counter + 1
            end do

            if (counter <= 2) then
                dummy = python_utility_forward_comment(-1)
                p = python_navigation_beginning_of_statement()

                if (is_beginning_of_buffer()) then
                    p = point()
                    dummy = python_utility_forward_comment()
                    docstring = looking_at(regular_expression, st)
                    call goto_character(p)
                end if

                if (.not. docstring) docstring = is_python_info_assignment_statement() > 0
                if (.not. docstring) docstring = python_info_looking_at_beginning_of_definition()
            end if
        end if

        call goto_character(original_point)
    end function is_python_info_docstring

    !> Check if point is in a triple quoted string including quotes.
    !> It returns the position of the third quote character of the start
    !> of the string (0 if not).  Port of python.el:python-info-triple-quoted-string-p.
    recursive integer(int64) function is_python_info_triple_quoted_string() result(found)
        integer, parameter :: OFFSETS(6) = [0, 3, -2, 2, -1, 1]
        integer(int64) :: position, check_position
        type(parse_state_type) :: parser_state
        integer :: k

        position = point()
        found = 0

        do k = 1, size(OFFSETS)
            check_position = position + OFFSETS(k)

            if (check_position >= point_minimum() .and. check_position <= point_maximum()) then
                parser_state = parser_state_at(check_position)
                found = python_syntax_context(PYTHON_CONTEXT_TRIPLE_QUOTED_STRING, parser_state)
                if (found > 0) exit
            end if
        end do

        call goto_character(position)
    end function is_python_info_triple_quoted_string

    !> Detect current buffer's encoding from its coding cookie
    !> (ENCODING unallocated if there is none).  Port of
    !> python.el:python-info-encoding-from-cookie.
    recursive subroutine python_info_encoding_from_cookie(encoding)
        character(len=:), allocatable, intent(out) :: encoding
        character(len=:), allocatable :: first_two_lines
        integer(int64) :: original_point, beginning_of_accessible_portion, end_of_accessible_portion_from_end, shortage
        type(status_type) :: st

        original_point = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        call widen()
        call goto_character(point_minimum())
        shortage = forward_line(2_int64)
        call buffer_substring(point_minimum(), point(), first_two_lines, st)
        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
        call goto_character(original_point)
        if (string_match(CODING_COOKIE_REGULAR_EXPRESSION, first_two_lines, st) > 0) encoding = match_string_no_properties(1, &
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
    recursive subroutine python_utility_goto_line(line_number)
        integer(int64), intent(in) :: line_number
        integer(int64) :: shortage

        call goto_character(point_minimum())
        shortage = forward_line(line_number - 1)
    end subroutine python_utility_goto_line

    !> Python mode specific version of `forward_comment'.
    !> Optional argument DIRECTION defines the direction to move to.
    !> Port of python.el:python-util-forward-comment.
    recursive logical function python_utility_forward_comment(direction) result(moved)
        integer, intent(in), optional :: direction
        integer(int64) :: comment_start_position, factor

        comment_start_position = python_syntax_context(PYTHON_CONTEXT_COMMENT)
        factor = 99999

        if (present(direction)) then
            if (direction < 0) factor = -99999
        end if

        if (comment_start_position > 0) call goto_character(comment_start_position)
        moved = forward_comment(factor)
    end function python_utility_forward_comment

    ! The character after POSITION, -1 if none.
    ! Not a port.
    recursive integer function after_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        after_character_at = -1
        if (position < point_minimum() .or. position >= point_maximum()) return
        call character_after(c, found, position)
        if (found) after_character_at = c
    end function after_character_at

    ! The character before POSITION, -1 if none.
    ! Not a port.
    recursive integer function before_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        before_character_at = -1
        if (position <= point_minimum() .or. position > point_maximum()) return
        call character_before(c, found, position)
        if (found) before_character_at = c
    end function before_character_at

    ! ------------------------------------------------------------------
    ! Indentation
    ! ------------------------------------------------------------------

    !> Guess and set `python_indent_offset' for the current buffer.  Port of
    !> python.el:python-indent-guess-indent-offset.
    recursive subroutine python_indent_guess_indent_offset()
        integer(int64) :: original_point, beginning_of_accessible_portion, end_of_accessible_portion_from_end, f, block_end, &
                          indentation, shortage
        character(len=24) :: buffer
        type(status_type) :: st
        logical :: is_end, dummy

        original_point = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        call widen()
        call goto_character(point_minimum())
        block_end = 0

        do while (block_end == 0)
            call regular_expression_search_forward(LINE_START_BLOCK_START_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. f <= 0) exit
            if (python_syntax_context_type() /= 0) cycle
            call goto_character(line_end_position())
            dummy = python_utility_forward_comment(-1)

            if (before_character_at(point()) == iachar(":")) then
                is_end = .true.
            else
                is_end = .false.
                shortage = forward_line(1_int64)

                if (is_python_info_block_continuation_line() > 0) then
                    do while (is_python_info_continuation_line() > 0 .and. .not. is_end_of_buffer())
                        shortage = forward_line(1_int64)
                    end do

                    dummy = python_utility_forward_comment(-1)
                    is_end = before_character_at(point()) == iachar(":")
                end if
            end if

            if (is_end) block_end = point()
        end do

        indentation = 0

        if (block_end > 0) then
            call goto_character(block_end)
            dummy = python_utility_forward_comment()
            indentation = current_indentation()
        end if

        if (block_end > 0 .and. indentation /= 0) then
            call python_indent_offset%set_local(indentation)
        else if (python_indent_guess_indent_offset_verbose%value) then
            write (buffer, "(i0)") python_indent_offset%value
            call message("Can't guess python_indent_offset, using defaults: "//trim(buffer))
        end if

        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
        call goto_character(original_point)
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
    !> PYTHON_INDENT_INSIDE_PARENTHESIS
    !>  - Fallback case when point is inside paren.
    !>  - START is the first non space char position *after* the open paren.
    !> PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_NESTED_PARENTHESIS
    !>  - Point is on a line that contains a nested paren closer.
    !>  - START is the position of the open paren it closes.
    !> PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_PARENTHESIS
    !>  - Point is on a line that contains a paren closer.
    !>  - START is the position of the open paren.
    !> PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START
    !>  - Point is inside a paren with items starting in their own line.
    !>  - START is the position of the open paren.
    !> PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START_FROM_BLOCK
    !>  - Point is inside a paren with items starting in their own line
    !>    from a block start.
    !>  - START is the position of the open paren.
    !> PYTHON_INDENT_INSIDE_PARENTHESIS_FROM_BLOCK
    !>  - Point is inside a paren from a block start followed by some
    !>    items on the same line.
    !>  - START is the first non space char position *after* the open paren.
    !> PYTHON_INDENT_INSIDE_PARENTHESIS_CONTINUATION_LINE
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
        type(parse_state_type) :: parser_state
        integer(int64) :: original_point, s, continuation_start, previous_start, after_start, l1, p, block_continuation_start, &
                          assignment_continuation_start, shortage, f
        type(status_type) :: st
        logical :: starts_in_newline, cond, dummy

        original_point = point()
        call beginning_of_line()
        parser_state = parser_state_here()
        call goto_character(original_point)
        start = 0

        ! Beginning of buffer.
        if (line_number_at_position() == 1) then
            kind = PYTHON_INDENT_NO_INDENT
            return
        end if

        ! Inside a string.
        s = python_syntax_context(PYTHON_CONTEXT_STRING, parser_state)

        if (s > 0) then
            if (is_python_info_docstring()) then
                kind = PYTHON_INDENT_INSIDE_DOCSTRING
            else
                kind = PYTHON_INDENT_INSIDE_STRING
            end if

            start = s
            call goto_character(original_point)
            return
        end if

        ! Inside a paren.
        s = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS, parser_state)

        if (s > 0) then
            call goto_character(s)
            call goto_character(point() + 1)
            l1 = line_number_at_position()
            dummy = python_utility_forward_comment()
            starts_in_newline = l1 /= line_number_at_position()
            call goto_character(original_point)
            continuation_start = 0
            shortage = forward_line(-1_int64)
            call back_to_indentation()
            ! Skip inner parens.
            previous_start = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS)

            do while (previous_start > 0 .and. previous_start >= s)
                if (previous_start == s) then
                    continuation_start = point()
                    exit
                end if

                call goto_character(previous_start)
                call back_to_indentation()
                previous_start = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS)
            end do

            call goto_character(original_point)
            ! Current line only holds the closing paren.
            p = skip_syntax_forward(" ")
            cond = is_python_syntax_closing_parenthesis()

            if (cond) then
                call goto_character(point() + 1)
                cond = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0
            end if

            call goto_character(original_point)

            if (cond) then
                kind = PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_PARENTHESIS
                start = s
                return
            end if

            ! Current line only holds a closing paren for nested.
            call back_to_indentation()
            cond = is_python_syntax_closing_parenthesis()
            call goto_character(original_point)

            if (cond) then
                kind = PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_NESTED_PARENTHESIS
                start = s
                return
            end if

            ! This line is a continuation of the previous line.
            if (continuation_start > 0) then
                kind = PYTHON_INDENT_INSIDE_PARENTHESIS_CONTINUATION_LINE
                start = continuation_start
                return
            end if

            ! This line starts from an opening block in its own line.
            if (starts_in_newline) then
                call goto_character(s)
                call back_to_indentation()
                cond = looking_at(BLOCK_START_REGULAR_EXPRESSION, st)
                call goto_character(original_point)

                if (cond) then
                    kind = PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START_FROM_BLOCK
                else
                    kind = PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START
                end if

                start = s
                return
            end if

            ! General case.
            call goto_character(s + 1)
            p = skip_syntax_forward("(", 1_int64)
            p = skip_syntax_forward(" ")
            after_start = point()
            call goto_character(original_point)
            p = python_navigation_beginning_of_statement()
            cond = python_info_looking_at_beginning_of_block()
            call goto_character(original_point)

            if (cond) then
                kind = PYTHON_INDENT_INSIDE_PARENTHESIS_FROM_BLOCK
            else
                kind = PYTHON_INDENT_INSIDE_PARENTHESIS
            end if

            start = after_start
            return
        end if

        ! After backslash.
        s = 0
        if (is_python_syntax_comment_or_string(parser_state) == 0) s = is_python_info_line_ends_backslash(line_number_at_position( &
                                                                                                          ) - 1)

        if (s > 0) then
            ! Continuation of dotted expression.
            call back_to_indentation()

            if (after_character_at(point()) == iachar(".")) then
                ! Move point back until it's not inside a paren.
                do
                    shortage = forward_line(-1_int64)
                    if (is_beginning_of_buffer()) exit
                    if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0) exit
                end do

                call goto_character(line_end_position())

                do
                    call search_backward(".", f, st, bound=line_beginning_position(), noerror=SEARCH_NOERROR)
                    if (.not. st%ok() .or. f <= 0) exit
                    if (python_syntax_context_type() == 0) exit
                end do

                ! Ensure previous statement has dot to align with.
                if (after_character_at(point()) == iachar(".") .and. python_syntax_context_type() == 0) then
                    kind = PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION
                    start = point()
                    call goto_character(original_point)
                    return
                end if
            end if

            call goto_character(original_point)
            ! Continuation of block definition.
            block_continuation_start = is_python_info_block_continuation_line()

            if (block_continuation_start > 0) then
                call goto_character(block_continuation_start)
                call regular_expression_search_forward(BLOCK_START_SPACE_REGULAR_EXPRESSION, f, st, bound=line_end_position(), &
                                                       noerror=SEARCH_NOERROR)
                kind = PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION
                start = point()
                call goto_character(original_point)
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
            call goto_character(s)
            cond = line_number_at_position() == 1
            if (.not. cond) cond = python_info_beginning_of_backslash(line_number_at_position() - 1) == 0
            call goto_character(original_point)

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
        dummy = python_utility_forward_comment(-1)
        s = 0
        if (before_character_at(point()) == iachar(":")) s = python_navigation_beginning_of_block()
        call goto_character(original_point)

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
        p = skip_characters_backward(" "//TAB//LF, st)

        if (is_beginning_of_buffer()) then
            kind = PYTHON_INDENT_NO_INDENT
            start = 0
        else
            p = python_navigation_beginning_of_statement()

            if (is_python_info_current_line_comment()) then
                kind = PYTHON_INDENT_AFTER_COMMENT
            else
                p = point()
                call goto_character(line_end_position())
                dummy = python_utility_forward_comment(-1)
                f = python_navigation_beginning_of_statement()
                cond = looking_at(BLOCK_ENDER_REGULAR_EXPRESSION, st)
                call goto_character(p)

                if (cond) then
                    kind = PYTHON_INDENT_AFTER_BLOCK_END
                else
                    kind = PYTHON_INDENT_AFTER_LINE
                end if
            end if

            start = point()
        end if

        call goto_character(original_point)
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
        integer(int64) :: original_point, start, line_indentation, base_indent, column, p
        integer :: kind, k
        type(status_type) :: st

        original_point = point()
        is_list = .false.
        call python_indent_context(kind, start)
        allocate (values(1))
        values(1) = programming_first_column()

        select case (kind)
        case (PYTHON_INDENT_NO_INDENT)
            ! usually 0
            values(1) = programming_first_column()
        case (PYTHON_INDENT_AFTER_LINE, PYTHON_INDENT_AFTER_COMMENT, PYTHON_INDENT_INSIDE_STRING, &
              PYTHON_INDENT_AFTER_BACKSLASH, PYTHON_INDENT_INSIDE_PARENTHESIS_CONTINUATION_LINE)
            ! Copy previous indentation.
            call goto_character(start)
            values(1) = current_indentation()
        case (PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_PARENTHESIS, &
              PYTHON_INDENT_INSIDE_PARENTHESIS_AT_CLOSING_NESTED_PARENTHESIS)
            call goto_character(start + 1)

            if (looking_at("[ "//TAB//"]*\(?:#\|$\)", st)) then
                ! Copy previous indentation.
                values(1) = current_indentation()
            else
                ! Align with opening paren.
                values(1) = current_column()
            end if
        case (PYTHON_INDENT_INSIDE_DOCSTRING)
            line_indentation = current_indentation()
            call goto_character(start)
            base_indent = current_indentation()
            values(1) = max(line_indentation, base_indent)
        case (PYTHON_INDENT_AFTER_BLOCK_START, PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE, &
              PYTHON_INDENT_AFTER_BACKSLASH_ASSIGNMENT_CONTINUATION, PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START)
            ! Add one indentation level.
            call goto_character(start)
            values(1) = current_indentation() + python_indent_offset%value
        case (PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION)
            call goto_character(start)
            column = current_column()

            if (column == current_indentation() + python_indent_offset%value) then
                ! Add one level to avoid same indent as next logical line.
                values(1) = column + python_indent_offset%value
            else
                values(1) = column
            end if
        case (PYTHON_INDENT_INSIDE_PARENTHESIS, PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION)
            ! Use the column given by the context.
            call goto_character(start)
            values(1) = current_column()
        case (PYTHON_INDENT_AFTER_BLOCK_END)
            ! Subtract one indentation level.
            call goto_character(start)
            values(1) = max(0_int64, current_indentation() - python_indent_offset%value)
        case (PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
            ! List all possible indentation levels from opening blocks.
            positions = python_info_dedenter_opening_block_positions()

            if (size(positions) > 0) then
                is_list = .true.
                deallocate (values)
                allocate (values(size(positions)))

                do k = 1, size(positions)
                    call goto_character(positions(k))
                    values(k) = current_indentation()
                end do
            end if
        case (PYTHON_INDENT_INSIDE_PARENTHESIS_NEWLINE_START_FROM_BLOCK)
            call goto_character(start)
            values(1) = current_indentation() + python_indent_offset%value*python_indent_def_block_scale%value
        case (PYTHON_INDENT_INSIDE_PARENTHESIS_FROM_BLOCK)
            call goto_character(start)
            column = current_column()
            values(1) = column

            if (python_indent_block_parenthesis_deeper%value) then
                p = python_navigation_beginning_of_statement()

                if (column == current_indentation() + python_indent_offset%value) then
                    values(1) = column + python_indent_offset%value
                end if
            end if
        case default
            continue
        end select

        call goto_character(original_point)
    end subroutine indent_calculate_indentation_1

    ! Calculate levels list given INDENTATION (a list when IS_LIST).
    ! Levels are returned in ascending order, and in the
    ! case INDENTATION is a list, this order is enforced.  Port of
    ! python.el:python-indent--calculate-levels.
    recursive function indent_calculate_levels_1(indentation, is_list) result(levels)
        integer(int64), intent(in) :: indentation(:)
        logical, intent(in) :: is_list
        integer(int64), allocatable :: levels(:)
        integer(int64) :: temporary, v
        integer :: i, j, n

        if (is_list) then
            levels = indentation

            do i = 2, size(levels)
                temporary = levels(i)
                j = i - 1

                do while (j >= 1)
                    if (.not. levels(j) > temporary) exit
                    levels(j + 1) = levels(j)
                    j = j - 1
                end do

                levels(j + 1) = temporary
            end do

            return
        end if

        n = 0
        v = programming_first_column()

        if (python_indent_offset%value > 0) then
            do while (v <= indentation(1) - 1)
                n = n + 1
                v = v + python_indent_offset%value
            end do
        end if

        allocate (levels(n + 1))
        v = programming_first_column()

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
        logical :: is_list, previous_value

        previous_value = .false.
        if (present(previous)) previous_value = previous
        call indent_calculate_indentation_1(values, is_list)
        levels = indent_calculate_levels_1(values, is_list)

        if (previous_value) then
            indentation = indent_previous_level(levels, current_indentation())
        else if (size(levels) > 0) then
            indentation = maxval(levels)
        else
            indentation = programming_first_column()
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
        call goto_character(marked_position(m))
        call destroy_marker(m, st)
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

        noindent = .false.
        previous = .false.
        if (associated(this_command)) previous = any(python_indent_trigger_commands == trim(this_command%name))
        if (previous) previous = associated(last_command, this_command)
        call python_indent_line(status, previous)
    end subroutine python_indent_line_function

    !> De-indent current line (true if it did).  Port of
    !> python.el:python-indent-dedent-line.
    recursive logical function python_indent_dedent_line() result(done)
        type(status_type) :: st

        done = .false.

        if (.not. is_beginning_of_line() .and. is_python_syntax_comment_or_string() == 0 .and. current_indentation() &
            == current_column()) then
            call python_indent_line(st, .true.)
            done = .true.
        end if
    end function python_indent_dedent_line

    !> De-indent current line.
    !> Argument ARGUMENT is passed to `backward_delete_character_untabify' when point is
    !> not in between the indentation or when Transient Mark mode is enabled,
    !> the mark is active, and ARGUMENT is 1.  Port of
    !> python.el:python-indent-dedent-line-backspace.
    recursive subroutine python_indent_dedent_line_backspace(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical :: delete

        status = status_type()
        delete = is_use_region() .and. argument == 1
        if (.not. delete) delete = .not. python_indent_dedent_line()
        if (delete) call backward_delete_character_untabify(argument, status)
    end subroutine python_indent_dedent_line_backspace

    !> Indent a Python region automagically.
    !>
    !> Called from a program, START and END specify the region to indent.
    !> Port of python.el:python-indent-region.
    recursive subroutine python_indent_region(start, end, status)
        use buffer_mod, only: deactivate_mark_variable
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(handle_type) :: endm, m
        integer(int64) :: shortage, q
        type(status_type) :: st
        logical :: saved_deactivate, line_is_comment_p, ok, comment_or_empty

        status = status_type()
        saved_deactivate = deactivate_mark_variable
        call copy_marker_at(point(), m, .false., st)
        call goto_character(end)
        call copy_marker_at(point(), endm, .false., st)
        call goto_character(start)
        if (.not. is_beginning_of_line()) shortage = forward_line(1_int64)

        do while (point() < marked_position(endm))
            if (.not. (is_beginning_of_line() .and. is_end_of_line())) then
                ! Skip if previous line is empty or a comment.
                q = point()
                line_is_comment_p = is_python_info_current_line_comment()
                shortage = forward_line(-1_int64)
                comment_or_empty = is_python_info_current_line_comment() .and. .not. line_is_comment_p
                if (.not. comment_or_empty) comment_or_empty = is_python_info_current_line_empty()
                ok = .not. comment_or_empty
                call goto_character(q)

                ! Don't mess with strings, unless it's the
                ! enclosing set of quotes or a docstring.
                if (ok) then
                    if (python_syntax_context(PYTHON_CONTEXT_STRING) > 0) then
                        ok = class_after(point() - 1 + current_indentation() &
                                         + python_syntax_count_quotes(after_character_at(point()), point())) == class_of("|")
                        if (.not. ok) ok = is_python_info_docstring()
                    end if
                end if

                ! Skip if current line is a block start, a
                ! dedenter or block ender.
                if (ok) then
                    q = point()
                    call back_to_indentation()
                    ok = .not. looking_at(INDENT_REGION_SKIP_REGULAR_EXPRESSION, st)
                    call goto_character(q)
                end if

                if (ok) call python_indent_line(status)
            end if

            shortage = forward_line(1_int64)
        end do

        call destroy_marker(endm, st)
        call goto_character(marked_position(m))
        call destroy_marker(m, st)
        deactivate_mark_variable = saved_deactivate
    end subroutine python_indent_region

    !> Shift lines contained in region START END by COUNT columns to the left.
    !> COUNT defaults to `python_indent_offset'.  If region isn't
    !> active, the current line is shifted.  The shifted region includes
    !> the lines in which START and END lie.  An error is signaled if
    !> any lines in the region are indented less than COUNT columns.
    !> Port of python.el:python-indent-shift-left.
    recursive subroutine python_indent_shift_left(start, end, status, count)
        use buffer_mod, only: deactivate_mark_variable
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        integer(int64) :: n, original_point, shortage
        type(status_type) :: st
        logical :: saved

        status = status_type()
        n = python_indent_offset%value
        if (present(count)) n = count
        if (n <= 0) return
        saved = deactivate_mark_variable
        original_point = point()
        call goto_character(start)

        do while (point() < end)
            if (current_indentation() < n .and. .not. looking_at("[ "//TAB//"]*$", st)) then
                call goto_character(original_point)
                call set_error(status, USER_ERROR_CODE, "Can't shift all lines enough")
                return
            end if

            shortage = forward_line()
        end do

        call indent_rigidly(start, end, -n, status)
        call goto_character(original_point)
        deactivate_mark_variable = saved
    end subroutine python_indent_shift_left

    !> Shift lines contained in region START END by COUNT columns to the right.
    !> COUNT defaults to `python_indent_offset'.  If region isn't
    !> active, the current line is shifted.  The shifted region includes
    !> the lines in which START and END lie.  Port of
    !> python.el:python-indent-shift-right.
    recursive subroutine python_indent_shift_right(start, end, status, count)
        use buffer_mod, only: deactivate_mark_variable
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
        integer(int64) :: parenthesis_start, indentation, original_point, dedenter_position, ev
        type(handle_type) :: startm, endm
        type(status_type) :: st
        logical :: ok
        integer :: c

        status = status_type()
        if (.not. minor_modes%electric_indent_mode%enabled()) return
        ev = -1
        if (vtag(last_command_event) == V_INTEGER) ev = xfixnum(last_command_event)
        if (before_character_at(point()) /= ev) return
        if (python_syntax_context(PYTHON_CONTEXT_STRING) > 0) return
        original_point = point()
        call beginning_of_line()
        ok = python_syntax_context(PYTHON_CONTEXT_STRING, parser_state_here()) == 0
        call goto_character(original_point)
        if (.not. ok) return
        ! Electric indent inside parens
        ok = .not. is_beginning_of_line()

        if (ok) then
            parenthesis_start = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS)
            ! Check that point is inside parens.
            ok = parenthesis_start > 0
            ! Filter the case where input is happening in the same
            ! line where the open paren is.
            if (ok) ok = line_number_at_position() /= line_number_at_position(parenthesis_start)
        end if

        if (ok) then
            ! When content has been added before the closing paren or a
            ! comma has been inserted, it's ok to do the trick.
            c = after_character_at(point())
            ok = c == iachar(")") .or. c == iachar("]") .or. c == iachar("}") .or. before_character_at(point()) == iachar(",")
        end if

        if (ok) then
            call copy_marker_at(point(), startm, .false., st)
            call goto_character(line_beginning_position())
            indentation = python_indent_calculate_indentation()
            if (current_indentation() < indentation) call indent_line_to(indentation, status)
            call goto_character(marked_position(startm))
            call destroy_marker(startm, st)
            return
        end if

        ! Electric colon
        if (ev /= iachar(":")) return
        if (.not. any(electric_indent_characters%get_current() == iachar(":"))) return
        if (current_prefix_argument%kind /= PREFIX_NONE) return
        ! Trigger electric colon only at end of line
        if (.not. is_end_of_line()) return
        ! Avoid re-indenting on extra colon
        if (before_character_at(point() - 1) == iachar(":")) return
        if (is_python_syntax_comment_or_string() /= 0) return
        ! Just re-indent dedenters
        dedenter_position = is_python_info_dedenter_statement()
        if (dedenter_position == 0) return
        call copy_marker_at(dedenter_position, startm, .false., st)
        call copy_marker_at(point(), endm, .false., st)
        original_point = point()
        call goto_character(marked_position(startm))
        call python_indent_line(status)

        ! Reindent region if this is a multiline statement
        if (line_number_at_position(marked_position(startm)) /= line_number_at_position(marked_position(endm))) then
            call python_indent_region(marked_position(startm), marked_position(endm), status)
        end if

        call goto_character(marked_position(endm))
        call destroy_marker(startm, st)
        call destroy_marker(endm, st)
        if (original_point < 0) continue
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
        logical :: parenthesis

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
            parenthesis = python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) > 0
            if (.not. parenthesis) parenthesis = looking_at(OPEN_PARENTHESIS_REGULAR_EXPRESSION, st)

            if (.not. parenthesis) then
                q = point()
                moved = skip_syntax_forward("^(", line_end_position())
                parenthesis = looking_at(OPEN_PARENTHESIS_REGULAR_EXPRESSION, st)
                call goto_character(q)
            end if

            if (parenthesis) call python_fill_parenthesis_function(status, done, justify)
        end if

        call goto_character(marked_position(m))
        call destroy_marker(m, st)

    contains

        ! (equal (char-after (save-excursion (python-nav-beginning-of-statement))) ?@)
        logical function decorator_here()
            integer(int64) :: q, b

            q = point()
            b = python_navigation_beginning_of_statement()
            call goto_character(q)
            decorator_here = after_character_at(b) == iachar("@")
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
        type(handle_type) :: string_start, string_line_start, string_end
        character(len=:), allocatable :: text
        integer(int64) :: quote_count, f, beginning_of_accessible_portion, end_of_accessible_portion_from_end, start_newlines, &
                          end_newlines, q, moved, e
        type(status_type) :: st
        logical :: multi_line_p

        done = .true.
        call copy_marker_at(is_python_info_triple_quoted_string(), string_start, .false., status)
        if (.not. status%ok()) return
        ! JT@2021-09-21: Since bug#49518's fix this will always be 1
        quote_count = python_syntax_count_quotes(after_character_at(marked_position(string_start)), marked_position(string_start))
        q = point()
        call goto_character(marked_position(string_start))
        call beginning_of_line()
        call copy_marker_at(point(), string_line_start, .false., st)
        call goto_character(marked_position(string_start) + quote_count)
        call regular_expression_search_forward(STRING_DELIM_SYNTAX_REGULAR_EXPRESSION, f, st, noerror=SEARCH_NOERROR)
        if (.not. st%ok() .or. f <= 0) call goto_character(point_maximum())
        call copy_marker_at(point(), string_end, .false., st)
        call goto_character(q)
        ! Docstring styles may vary for one-liners and multi-liners.
        call buffer_substring(marked_position(string_start), marked_position(string_end), text, st)
        multi_line_p = index(text, LF) > 0
        ! delimiters-style is a cons cell with the form
        ! (START-NEWLINES .  END-NEWLINES). When any of the expressions
        ! is FALSE means to not add any newlines for start or end
        ! of docstring.  See `python-fill-docstring-style' for a
        ! graphic idea of each style.
        start_newlines = 0
        end_newlines = 0

        select case (python_fill_docstring_style%value)
        case (PYTHON_FILL_DOCSTRING_STYLE_DJANGO)
            start_newlines = 1
            end_newlines = 1
        case (PYTHON_FILL_DOCSTRING_STYLE_ONETWO)
            if (multi_line_p) then
                start_newlines = 1
                end_newlines = 2
            end if
        case (PYTHON_FILL_DOCSTRING_STYLE_PEP_257)
            if (multi_line_p) end_newlines = 2
        case (PYTHON_FILL_DOCSTRING_STYLE_PEP_257_NN)
            if (multi_line_p) end_newlines = 1
        case (PYTHON_FILL_DOCSTRING_STYLE_SYMMETRIC)
            if (multi_line_p) then
                start_newlines = 1
                end_newlines = 1
            end if
        case default
            continue
        end select

        ! (save-restriction (narrow-to-region ...) (fill-paragraph justify))
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion
        call narrow_to_region(marked_position(string_line_start), marked_position(string_end), status)
        if (status%ok()) call fill_paragraph(status, justify)
        call widen()
        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
        q = point()

        if (is_python_info_docstring() .and. python_fill_docstring_style%value /= PYTHON_FILL_DOCSTRING_STYLE_NONE) then
            ! Add the number of newlines indicated by the selected style
            ! at the start of the docstring.
            call goto_character(marked_position(string_start) + quote_count)
            e = point()
            moved = skip_syntax_forward("> ")
            call delete_region(e, point(), st)
            call goto_character(e)

            if (start_newlines > 0) then
                call newline(st, start_newlines)
                ! Indent only if a newline is added.
                call indent_according_to_mode(st)
            end if

            ! Add the number of newlines indicated by the selected style
            ! at the end of the docstring.
            if (marked_position(string_end) /= point_maximum()) then
                call goto_character(marked_position(string_end) - quote_count)
            else
                call goto_character(marked_position(string_end))
            end if

            e = point()
            moved = skip_syntax_backward("> ")
            call delete_region(point(), e, st)

            ! Add newlines only if string ends.
            if (end_newlines > 0 .and. marked_position(string_end) /= point_maximum()) then
                call newline(st, end_newlines)
                ! Again indent only if a newline is added.
                call indent_according_to_mode(st)
            end if
        end if

        call goto_character(q)
        call destroy_marker(string_start, st)
        call destroy_marker(string_line_start, st)
        call destroy_marker(string_end, st)
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
    recursive subroutine python_fill_parenthesis(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        character(len=:), allocatable :: saved_start, saved_separate
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion_from_end, b, e, moved, shortage
        type(status_type) :: st
        logical :: had_start, had_separate

        done = .true.
        status = status_type()
        ! (save-restriction (narrow-to-region ...))
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion_from_end = buffers(current)%text%z - buffers(current)%end_of_accessible_portion

        do while (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) > 0)
            call goto_character(point() - 1)
        end do

        b = line_beginning_position()

        if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0) then
            call end_of_line()
            if (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) == 0) moved = skip_syntax_backward("^)")
        end if

        do while (python_syntax_context(PYTHON_CONTEXT_PARENTHESIS) > 0 .and. .not. is_end_of_buffer())
            call goto_character(point() + 1)
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
        call goto_character(point_minimum())
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

        do while (.not. is_end_of_buffer())
            shortage = forward_line(1_int64)
            call python_indent_line(st)
            call goto_character(line_end_position())
        end do

        call widen()
        call narrow_to_region(beginning_of_accessible_portion, buffers(current)%text%z - end_of_accessible_portion_from_end, st)
    end subroutine python_fill_parenthesis

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
        integer(int64) :: original_point

        status = status_type()
        if (.not. minor_modes%electric_pair_mode%enabled()) return
        ev = -1
        if (vtag(last_command_event) == V_INTEGER) ev = xfixnum(last_command_event)
        if (ev /= iachar('"') .and. ev /= iachar("'")) return
        count = 0

        do while (before_character_at(point() - count) == ev)
            count = count + 1
        end do

        if (count /= 3) return
        if (after_character_at(point()) /= ev) return
        original_point = point()
        call insert(achar(ev)//achar(ev), status)
        call goto_character(original_point)
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

        if (.not. is_syntax_table_live(python_mode_syntax_table)) call initialize_python()
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
        chars = [iachar(":"), electric_indent_characters%get_current()]
        call electric_indent_characters%set_local(chars)
        ! Add """ ... """ pairing to electric-pair-mode.
        call post_self_insert_hook%append(python_electric_pair_string_delimiter, local=.true.)
        call paragraph_start%set_local("\s-*$")
        call fill_paragraph_function%set_local(python_fill_paragraph)
        call normal_auto_fill_function%set_local(python_do_auto_fill)
        call beginning_of_subprogram_function%set_local(python_navigation_beginning_of_definition)
        call end_of_subprogram_function%set_local(python_navigation_end_of_definition_function)
        call add_log_current_subprogram_function%set_local(get_python_current_definition)
        call post_self_insert_hook%append(python_indent_post_self_insert_function, local=.true.)
        if (.not. installed_grep_hook) call install_grep_settings()
        call major_modes%python_base_mode%end(saved, status)
    end subroutine python_base_mode

    ! (with-eval-after-load 'grep ...): the Python files alias and the
    ! directories `rgrep' ignores.
    ! Port of python.el:python-base-mode (in part: its grep settings).
    recursive subroutine install_grep_settings()
        character(len=*), parameter :: DIRECTORIES(6) = [character(len=14) :: ".mypy_cache", ".pytest_cache", &
                                                                                     ".ropeproject", ".ruff_cache", ".tox", ".venv"]
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
            grep_files_aliases = [grep_files_alias_type(name="py", files="*.py"), grep_files_aliases]
        end if

        if (.not. allocated(grep_find_ignored_directories)) allocate (grep_find_ignored_directories(0))

        do j = 1, size(DIRECTORIES)
            present_already = .false.

            do k = 1, size(grep_find_ignored_directories)
                if (trim(grep_find_ignored_directories(k)%name) == trim(DIRECTORIES(j)) &
                    .and. .not. associated(grep_find_ignored_directories(k)%applies)) present_already = .true.
            end do

            if (.not. present_already) then
                grep_find_ignored_directories = [grep_ignore_type(name=DIRECTORIES(j)), grep_find_ignored_directories]
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

        if (.not. is_syntax_table_live(python_mode_syntax_table)) call initialize_python()
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
        call index_menu_create_index_function%set_local(create_python_index_menu_index)
        call which_function_functions%prepend(python_which_function_adapter, local=.true.)
        if (python_indent_guess_indent_offset_variable%value) call python_indent_guess_indent_offset()
        call major_modes%python_mode%end(saved, status)
    end subroutine python_mode

    !> `python_info_current_definition' as a function of
    !> `which_function_functions'.  Not a port.
    recursive subroutine python_which_function_adapter(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        call python_info_current_definition(string)
    end subroutine python_which_function_adapter

    ! ------------------------------------------------------------------
    ! Imenu
    ! ------------------------------------------------------------------

    !> Return Imenu label for single node using TYPE and NAME.  Port of
    !> python.el:python-imenu-format-item-label.
    subroutine format_python_index_menu_item_label(type, name, label)
        character(len=*), intent(in) :: type, name
        character(len=:), allocatable, intent(out) :: label

        label = name//" ("//type//")"
    end subroutine format_python_index_menu_item_label

    !> Return Imenu label for parent node using TYPE and NAME.  Port of
    !> python.el:python-imenu-format-parent-item-label.
    subroutine format_python_index_menu_parent_item_label(type, name, label)
        character(len=*), intent(in) :: type, name
        character(len=:), allocatable, intent(out) :: label

        call format_python_index_menu_item_label(type, name, label)
        label = label//"..."
    end subroutine format_python_index_menu_parent_item_label

    !> Return Imenu label for parent node jump using TYPE and NAME.  Port of
    !> python.el:python-imenu-format-parent-item-jump-label.
    subroutine format_python_index_menu_parent_item_jump_label(type, name, label)
        character(len=*), intent(in) :: type, name
        character(len=:), allocatable, intent(out) :: label

        if (len(name) > 0) continue

        if (type == "class") then
            label = "*class definition*"
        else
            label = "*function definition*"
        end if
    end subroutine format_python_index_menu_parent_item_jump_label

    !> Return tree Imenu index for the current Python buffer.
    !> Change `python_index_menu_format_item_label_function',
    !> `python_index_menu_format_parent_item_label_function',
    !> `python_index_menu_format_parent_item_jump_label_function' to
    !> customize how labels are formatted.  Port of
    !> python.el:python-imenu-create-index.
    recursive subroutine create_python_index_menu_index(index, status)
        type(index_menu_index_type), intent(out) :: index
        type(status_type), intent(out) :: status
        type(index_menu_node_type), allocatable :: nodes(:)
        integer, allocatable :: top(:), none(:)
        integer :: tree, i

        status = status_type()
        allocate (index%entries(0), nodes(0), top(0), none(0))
        call goto_character(point_maximum())

        do
            tree = build_tree(0_int64, python_indent_offset%value, none)
            if (tree == 0) exit
            top = [tree, top]
        end do

        do i = 1, size(top)
            call append_node(top(i), 0)
        end do

    contains

        ! Make a node; K gets its index in NODES.
        subroutine make_node(k, label, position_value, parent, jump_label, children)
            integer, intent(out) :: k
            character(len=*), intent(in) :: label
            integer(int64), intent(in) :: position_value
            logical, intent(in) :: parent
            character(len=*), intent(in) :: jump_label
            integer, intent(in) :: children(:)
            type(index_menu_node_type) :: node

            node%label = label
            node%jump_label = jump_label
            node%position = position_value
            node%parent = parent
            allocate (node%children(size(children)))
            node%children(:) = children(:)
            nodes = [nodes, node]
            k = size(nodes)
        end subroutine make_node

        ! Recursively build the tree of nested definitions of a node: the
        ! node, 0 for none.  Arguments MINIMUM_INDENT, PREVIOUS_INDENT and TREE are
        ! internal.  Port of python.el:python-imenu--build-tree.
        recursive integer function build_tree(minimum_indent, previous_indent, tree) result(k)
            integer(int64), intent(in) :: minimum_indent, previous_indent
            integer, intent(in) :: tree(:)
            character(len=:), allocatable :: type, name, label, jump_label
            integer(int64) :: position_value, indent, children_indent_limit
            integer :: leaf, sub

            k = 0
            position_value = python_navigation_backward_definition(1_int64)
            ! Nothing found, probably near to is_beginning_of_buffer.
            if (position_value == 0) return
            call get_definition_type_name(type, name)
            call python_index_menu_format_item_label_function(type, name, label)
            indent = current_indentation()
            children_indent_limit = python_indent_offset%value + minimum_indent

            if (indent <= minimum_indent) then
                ! The current indentation points that this is a parent
                ! node, add it to the tree and stop recursing
                ! (python-imenu--put-parent).
                if (size(tree) == 0) then
                    call make_node(k, label, position_value, .false., "", tree)
                else
                    call python_index_menu_format_parent_item_jump_label_function(type, name, jump_label)
                    call make_node(k, label, position_value, .true., jump_label, tree)
                end if
            else if (indent <= children_indent_limit) then
                ! This lies within the children indent offset range, so
                ! it's a normal child of its parent (i.e., not a child of
                ! a child).
                call make_node(leaf, label, position_value, .false., "", tree(1:0))
                k = build_tree(minimum_indent, indent, [leaf, tree])
            else
                ! A child of a child: parse it recursively by swapping
                ! prev-indent and min-indent plus adding this newly found
                ! item to a fresh subtree.
                call make_node(leaf, label, position_value, .false., "", tree(1:0))
                sub = build_tree(previous_indent, indent, [leaf])

                if (sub == 0) then
                    k = build_tree(minimum_indent, indent, tree)
                else
                    k = build_tree(minimum_indent, indent, [sub, tree])
                end if
            end if
        end function build_tree

        ! TYPE and NAME of the subprogram at point.  Port of
        ! python.el:python-imenu--get-defun-type-name.
        subroutine get_definition_type_name(type, name)
            character(len=:), allocatable, intent(out) :: type, name
            character(len=*), parameter :: WHITESPACE = " "//TAB//LF//achar(13)//achar(12)//achar(11)
            character(len=:), allocatable :: text, first, second, last
            type(status_type) :: st
            integer :: b, e, k, n, rest

            type = ""
            name = ""
            if (.not. looking_at(python_navigation_beginning_of_definition_regular_expression, st)) return
            text = match_string_no_properties(0)
            ! split-string: the words between whitespace.
            n = 0
            rest = 1
            first = ""
            second = ""
            last = ""

            do
                k = verify(text(rest:), WHITESPACE)
                if (k == 0) exit
                b = rest + k - 1
                k = scan(text(b:), WHITESPACE)

                if (k == 0) then
                    e = len(text)
                else
                    e = b + k - 2
                end if

                n = n + 1
                if (n == 1) first = text(b:e)
                if (n == 2) second = text(b:e)
                last = text(b:e)
                rest = e + 1
            end do

            if (n == 2) then
                type = first
                name = second
            else if (n > 2) then
                type = first//" "//second
                name = last
            end if
        end subroutine get_definition_type_name

        ! Append node K to INDEX in submenu PARENT.
        recursive subroutine append_node(k, parent)
            integer, intent(in) :: k, parent
            integer :: entry, jump, i

            if (nodes(k)%parent) then
                call index%append(nodes(k)%label, entry, parent=parent, submenu=.true.)
                call index%append(nodes(k)%jump_label, jump, position=nodes(k)%position, parent=entry)

                do i = 1, size(nodes(k)%children)
                    call append_node(nodes(k)%children(i), entry)
                end do
            else
                call index%append(nodes(k)%label, entry, position=nodes(k)%position, parent=parent)
            end if
        end subroutine append_node
    end subroutine create_python_index_menu_index

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The position of marker M.
    ! Not a port.
    recursive integer(int64) function marked_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, marked_position, st)
    end function marked_position

    !> Guess and set `python_indent_offset' for the current buffer (the
    !> command).  Port of python.el:python-indent-guess-indent-offset.
    recursive subroutine execute_python_indent_guess_indent_offset(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_indent_guess_indent_offset()
    end subroutine execute_python_indent_guess_indent_offset

    !> De-indent current line (the command).  Port of
    !> python.el:python-indent-dedent-line.
    recursive subroutine execute_python_indent_dedent_line(status)
        use buffer_mod, only: barf_if_buffer_read_only
        type(status_type), intent(out) :: status
        logical :: done

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        done = python_indent_dedent_line()
    end subroutine execute_python_indent_dedent_line

    !> De-indent current line (the command).  Port of
    !> python.el:python-indent-dedent-line-backspace.
    recursive subroutine execute_python_indent_dedent_line_backspace(status)
        use buffer_mod, only: barf_if_buffer_read_only
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call python_indent_dedent_line_backspace(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_indent_dedent_line_backspace

    ! The region or the current line, and the count, of the shift commands.
    ! Port of python.el:python-indent-shift-left (in part: its interactive spec).
    recursive subroutine shift_arguments(start, end, status)
        integer(int64), intent(out) :: start, end
        type(status_type), intent(out) :: status

        status = status_type()

        if (buffers(current)%mark_active) then
            call region_beginning(start, status)
            if (status%ok()) call region_end(end, status)
        else
            start = line_beginning_position()
            end = line_end_position()
        end if
    end subroutine shift_arguments

    !> Shift lines contained in the region by COUNT columns to the left (the
    !> command).  Port of python.el:python-indent-shift-left.
    recursive subroutine execute_python_indent_shift_left(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end

        call shift_arguments(start, end, status)
        if (.not. status%ok()) return

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call python_indent_shift_left(start, end, status)
        else
            call python_indent_shift_left(start, end, status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_python_indent_shift_left

    !> Shift lines contained in the region by COUNT columns to the right (the
    !> command).  Port of python.el:python-indent-shift-right.
    recursive subroutine execute_python_indent_shift_right(status)
        type(status_type), intent(out) :: status
        integer(int64) :: start, end

        call shift_arguments(start, end, status)
        if (.not. status%ok()) return

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call python_indent_shift_right(start, end, status)
        else
            call python_indent_shift_right(start, end, status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_python_indent_shift_right

    !> Put mark at end of this subprogram, point at beginning (the command).
    !> Port of python.el:python-mark-defun.
    recursive subroutine execute_python_mark_definition(status)
        type(status_type), intent(out) :: status

        call python_mark_definition(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_mark_definition

    !> Move point to the end of def or class (the command).  Port of
    !> python.el:python-nav-end-of-defun.
    recursive subroutine execute_python_navigation_end_of_definition(status)
        type(status_type), intent(out) :: status
        logical :: moved

        status = status_type()
        moved = python_navigation_end_of_definition()
    end subroutine execute_python_navigation_end_of_definition

    !> Navigate to closer subprogram backward ARGUMENT times (the command).  Port of
    !> python.el:python-nav-backward-defun.
    recursive subroutine execute_python_navigation_backward_definition(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_backward_definition(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_python_navigation_backward_definition

    !> Navigate to closer subprogram forward ARGUMENT times (the command).  Port of
    !> python.el:python-nav-forward-defun.
    recursive subroutine execute_python_navigation_forward_definition(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_forward_definition(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_python_navigation_forward_definition

    !> Move to start of current statement (the command).  Port of
    !> python.el:python-nav-beginning-of-statement.
    recursive subroutine execute_python_navigation_beginning_of_statement(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_beginning_of_statement()
    end subroutine execute_python_navigation_beginning_of_statement

    !> Move to end of current statement (the command).  Port of
    !> python.el:python-nav-end-of-statement.
    recursive subroutine execute_python_navigation_end_of_statement(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_end_of_statement()
    end subroutine execute_python_navigation_end_of_statement

    !> Move backward to previous statement (the command).  Port of
    !> python.el:python-nav-backward-statement.
    recursive subroutine execute_python_navigation_backward_statement(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_navigation_backward_statement(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_python_navigation_backward_statement

    !> Move forward to next statement (the command).  Port of
    !> python.el:python-nav-forward-statement.
    recursive subroutine execute_python_navigation_forward_statement(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call python_navigation_forward_statement(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_python_navigation_forward_statement

    !> Move to start of current block (the command).  Port of
    !> python.el:python-nav-beginning-of-block.
    recursive subroutine execute_python_navigation_beginning_of_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_beginning_of_block()
    end subroutine execute_python_navigation_beginning_of_block

    !> Move to end of current block (the command).  Port of
    !> python.el:python-nav-end-of-block.
    recursive subroutine execute_python_navigation_end_of_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_end_of_block()
    end subroutine execute_python_navigation_end_of_block

    !> Move backward to previous block of code (the command).  Port of
    !> python.el:python-nav-backward-block.
    recursive subroutine execute_python_navigation_backward_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_backward_block(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_python_navigation_backward_block

    !> Move forward to next block of code (the command).  Port of
    !> python.el:python-nav-forward-block.
    recursive subroutine execute_python_navigation_forward_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_forward_block(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_python_navigation_forward_block

    !> Move forward across expressions (the command).  Port of
    !> python.el:python-nav-forward-sexp.
    recursive subroutine execute_python_navigation_forward_expression(status)
        type(status_type), intent(out) :: status

        call python_navigation_forward_expression(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_navigation_forward_expression

    !> Move backward across expressions (the command).  Port of
    !> python.el:python-nav-backward-sexp.
    recursive subroutine execute_python_navigation_backward_expression(status)
        type(status_type), intent(out) :: status

        call python_navigation_backward_expression(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_navigation_backward_expression

    !> Move forward safely across expressions (the command).  Port of
    !> python.el:python-nav-forward-sexp-safe.
    recursive subroutine execute_python_navigation_forward_expression_safe(status)
        type(status_type), intent(out) :: status

        call python_navigation_forward_expression_safe(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_navigation_forward_expression_safe

    !> Move backward safely across expressions (the command).  Port of
    !> python.el:python-nav-backward-sexp-safe.
    recursive subroutine execute_python_navigation_backward_expression_safe(status)
        type(status_type), intent(out) :: status

        call python_navigation_backward_expression_safe(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_navigation_backward_expression_safe

    !> Move forward out of one level of parentheses (or blocks) (the
    !> command).  Port of python.el:python-nav-up-list.
    recursive subroutine execute_python_navigation_up_group(status)
        type(status_type), intent(out) :: status

        call python_navigation_up_group(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_navigation_up_group

    !> Move backward out of one level of parentheses (or blocks) (the
    !> command).  Port of python.el:python-nav-backward-up-list.
    recursive subroutine execute_python_navigation_backward_up_group(status)
        type(status_type), intent(out) :: status

        call python_navigation_backward_up_group(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_python_navigation_backward_up_group

    !> Move point at the beginning the __main__ block (the command).
    !> Port of python.el:python-nav-if-name-main.
    recursive subroutine execute_python_navigation_if_name_main(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p

        status = status_type()
        p = python_navigation_if_name_main()
    end subroutine execute_python_navigation_if_name_main

    !> Fill a comment, string, decorator or paren (the command).  Port of
    !> python.el:python-fill-paragraph.
    recursive subroutine execute_python_fill_paragraph(status)
        use fill_mod, only: DEFAULT_JUSTIFICATION_FULL
        type(status_type), intent(out) :: status
        logical :: done

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call python_fill_paragraph(status, done)
        else
            call python_fill_paragraph(status, done, DEFAULT_JUSTIFICATION_FULL)
        end if
    end subroutine execute_python_fill_paragraph
end module python_mod
