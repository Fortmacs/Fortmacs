! simple_minded_indentation_engine_mod.f90 -- the Simple Minded Indentation Engine.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/smie.el.
!
! Copyright (C) 2010-2026 Free Software Foundation, Inc.
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

!> SIMPLE_MINDED_INDENTATION_ENGINE: navigation and indentation from an operator precedence grammar.
!>
!> There are 4 different representations of a "grammar":
!> - a BACKUS_NAUR_FORM table (`simple_minded_indentation_engine_backus_naur_form_type'), a list of rules (NONTERM RHS1 ... RHSn)
!>   where each RHS is a list of terminals (tokens) or nonterminals.  Any
!>   element which does not appear as the NONTERM of a rule is taken to be
!>   a terminal.
!> - a list of precedences (`simple_minded_indentation_engine_precs_type'), sorted from lowest to
!>   highest precedence, of precedence classes (ASSOCIATIVITY TERMINAL1 ..
!>   TERMINALn), where ASSOCIATIVITY is `SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC', `SIMPLE_MINDED_INDENTATION_ENGINE_LEFT',
!>   `SIMPLE_MINDED_INDENTATION_ENGINE_RIGHT' or `SIMPLE_MINDED_INDENTATION_ENGINE_NONASSOC'.
!> - a 2 dimensional precedence table (`simple_minded_indentation_engine_prec2_type') recording the
!>   precedence relation (`SIMPLE_MINDED_INDENTATION_ENGINE_LT', `SIMPLE_MINDED_INDENTATION_ENGINE_EQ',
!> `SIMPLE_MINDED_INDENTATION_ENGINE_GT' or none) between
!>   each pair of tokens.
!> - a precedence-level table (`simple_minded_indentation_engine_grammar_type'), giving for each token
!>   its left and right precedence level, which is what's used by the rest
!>   of SIMPLE_MINDED_INDENTATION_ENGINE.
!> The prec2 tables are kept in insertion order, like the hash tables
!> upstream iterates over, so the levels come out the same.
!>
!> A mode's lexer functions return the token in an allocatable string
!> (unallocated for none), its `simple_minded_indentation_engine_rules_function' its answer as a
!> `simple_minded_indentation_engine_rule_result_type'.  Positions are 0 for none.
!>
!> smie-config (`simple_minded_indentation_engine_configuration', `simple_minded_indentation_engine_configuration_local',
!> `simple_minded_indentation_engine_configuration_show_indent',
!> `simple_minded_indentation_engine_configuration_set_indent', `simple_minded_indentation_engine_configuration_guess',
!> `simple_minded_indentation_engine_configuration_save') rewrites
!> the answers of `simple_minded_indentation_engine_rules_function'.
!>
!> Deferred: marking `simple_minded_indentation_engine_configuration' as set for Customize,
!> `simple_minded_indentation_engine_edebug', the
!> conflict warnings (display-warning, from warnings.el, is not ported;
!> conflicting constraints are resolved as upstream but not reported) and
!> `debug_on_error' in `simple_minded_indentation_engine_rule_separator' (a message is shown).
module simple_minded_indentation_engine_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use character_mod, only: downcase_ascii
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, SCAN_ERROR_CODE, USER_ERROR_CODE
    use print_mod, only: fortran_string, quoted_text
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, buffer_serial_live, current_buffer, buffer_characters_modified_tick
    use buffer_local_setting_mod, only: buffer_local_setting_type
    use value_mod, only: command_procedure, xfixnum, vtag, V_INTEGER
    use data_mod, only: append_locals_clearer, function_setting_default
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_buffer, &
                                     is_end_of_buffer, is_beginning_of_line, is_end_of_line, line_beginning_position, &
                                     line_end_position, buffer_substring, character_after, character_before, character_to_string
    use editing_commands_mod, only: forward_line, forward_character, beginning_of_line, post_self_insert_hook
    use insert_delete_mod, only: insert
    use syntax_mod, only: forward_comment, skip_syntax_forward, skip_syntax_backward, skip_characters_forward, &
                          skip_characters_backward, syntax_entry_at, syntax_descriptor_type, parse_state_type, &
                          syntax_parser_state, scan_error_data
    use search_mod, only: looking_at, string_match, match_beginning, match_end, regular_expression_search_forward, &
                          regular_expression_quote, SEARCH_NOERROR
    use basic_subprograms_mod, only: looking_back, after_change_major_mode_hook, read_y_or_n, progress_reporter_type, &
                                     make_progress_reporter, progress_reporter_update, progress_reporter_done
    use derived_mod, only: major_mode_type, major_mode_named
    use minibuffer_reading_mod, only: read_string, completion_table_type
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use replace_mod, only: format_prompt
    use value_mod, only: value_type, make_list
    use utility_functions_mod, only: is_string_less
    use keyboard_mod, only: sit_for
    use execute_interactively_mod, only: PREFIX_NONE
    use expression_motion_mod, only: forward_expression, backward_expression, backward_up_group, down_group, &
                                     forward_expression_function, &
                                     forward_expression_procedure
    use indent_mod, only: current_column, current_indentation, indent_line_to, move_to_column, indent_according_to_mode, &
                          indent_line_function
    use fill_mod, only: current_fill_column
    use simple_mod, only: newline, newline_and_indent, reindent_then_newline_and_indent, blink_matching_open, &
                          blink_matching_check_mismatch, blink_matching_check_procedure, blink_matching_check_function, &
                          blink_matching_parenthesis, normal_auto_fill_function, do_auto_fill, &
                          blink_matching_delay
    use newcomment_mod, only: comment_normalize_variables, comment_start_skip, comment_end_skip, comment_continue, &
                              comment_string_strip
    use parenthesis_mod, only: show_parenthesis_data_type, show_parenthesis_data_procedure, show_parenthesis_data_function
    use programming_mode_mod, only: programming_first_column
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use keyboard_mod, only: last_command_event
    use display_engine_mod, only: message
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use string_type_mod, only: string_type, append_string, prepend_string
    implicit none(type, external)
    private

    public :: simple_minded_indentation_engine_backus_naur_form_to_prec2, simple_minded_indentation_engine_precs_to_prec2, &
              simple_minded_indentation_engine_merge_prec2s, simple_minded_indentation_engine_prec2_to_grammar, &
              simple_minded_indentation_engine_set_prec2tab, &
              simple_minded_indentation_engine_backus_naur_form_closer_table, &
              simple_minded_indentation_engine_backus_naur_form_classify
    public :: simple_minded_indentation_engine_default_forward_token, simple_minded_indentation_engine_default_backward_token, &
              simple_minded_indentation_engine_next_expression, simple_minded_indentation_engine_backward_expression, &
              simple_minded_indentation_engine_forward_expression, simple_minded_indentation_engine_backward_expression_command, &
              simple_minded_indentation_engine_forward_expression_command, simple_minded_indentation_engine_close_block, &
              simple_minded_indentation_engine_down_group, simple_minded_indentation_engine_blink_matching_check, &
              simple_minded_indentation_engine_blink_matching_open, simple_minded_indentation_engine_forward_token, &
              simple_minded_indentation_engine_backward_token, simple_minded_indentation_engine_grammar_token, &
              is_simple_minded_indentation_engine_closer, is_simple_minded_indentation_engine_opener
    public :: is_simple_minded_indentation_engine_rule_hanging, is_simple_minded_indentation_engine_rule_beginning_of_line, &
              simple_minded_indentation_engine_indent_is_beginning_of_line_1, &
              simple_minded_indentation_engine_indent_current_column, is_simple_minded_indentation_engine_rule_parent, &
              is_simple_minded_indentation_engine_rule_next, is_simple_minded_indentation_engine_rule_previous, &
              is_simple_minded_indentation_engine_rule_sibling, simple_minded_indentation_engine_rule_parent, &
              simple_minded_indentation_engine_rule_separator, &
              simple_minded_indentation_engine_indent_offset, simple_minded_indentation_engine_indent_forward_token, &
              simple_minded_indentation_engine_indent_backward_token, simple_minded_indentation_engine_indent_virtual, &
              simple_minded_indentation_engine_indent_calculate, simple_minded_indentation_engine_indent_line, &
              simple_minded_indentation_engine_indent_keyword, simple_minded_indentation_engine_indent_fixindent, &
              simple_minded_indentation_engine_indent_beginning_of_buffer, &
              simple_minded_indentation_engine_indent_close, simple_minded_indentation_engine_indent_comment, &
              simple_minded_indentation_engine_indent_comment_continue, &
              simple_minded_indentation_engine_indent_comment_close, &
              simple_minded_indentation_engine_indent_comment_inside, simple_minded_indentation_engine_indent_inside_string, &
              simple_minded_indentation_engine_indent_after_keyword, &
              simple_minded_indentation_engine_indent_empty_line, simple_minded_indentation_engine_indent_exps, &
              simple_minded_indentation_engine_auto_fill, simple_minded_indentation_engine_setup, &
              simple_minded_indentation_engine_next_indent_change, &
              simple_minded_indentation_engine_parent, simple_minded_indentation_engine_parent_level, &
              initialize_simple_minded_indentation_engine, &
              is_simple_minded_indentation_engine_default_hanging_end_of_line, &
              simple_minded_indentation_engine_indent_procedure, simple_minded_indentation_token_procedure, &
              simple_minded_indentation_engine_rules_procedure, simple_minded_indentation_engine_hanging_line_procedure, &
              simple_minded_indentation_engine_configuration_local, simple_minded_indentation_engine_configuration_show_indent, &
              simple_minded_indentation_engine_configuration_set_indent, &
              simple_minded_indentation_engine_configuration_guess, &
              simple_minded_indentation_engine_configuration_save

    !> Precedence classes (ASSOCIATIVITY of a precs table).
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_LEFT = 1, SIMPLE_MINDED_INDENTATION_ENGINE_RIGHT = 2, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC = 3, SIMPLE_MINDED_INDENTATION_ENGINE_NONASSOC = 4
    !> Precedence relations: <, = and >.
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_LT = 1, SIMPLE_MINDED_INDENTATION_ENGINE_EQ = 2, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_GT = 3
    !> Token classes of `simple_minded_indentation_engine_backus_naur_form_classify'.
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_NEITHER = 0, SIMPLE_MINDED_INDENTATION_ENGINE_OPENER = 1, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_CLOSER = 2
    integer, parameter :: SIMPLE_MINDED_INDENTATION_ENGINE_NONTERMINAL = 3
    !> METHODs of `simple_minded_indentation_engine_rules_function'.
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_AFTER = 1, SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE = 2, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT = 3, SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO = 4, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_CLOSE_ALL = 5
    !> Kinds of a precedence level: a number, a list (the token does not
    !> bind on that side), SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE and SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS.
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE = 0, &
                               SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER = 1, SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST = 2, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS = 3
    !> Kinds of a `simple_minded_indentation_engine_rules_function' answer: none (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE), an
    !> OFFSET, a column
    !> COLUMN, a token (for empty_line_token) and .true. (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES, for
    !> SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO
    !> and SIMPLE_MINDED_INDENTATION_ENGINE_CLOSE_ALL).
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE = 0, &
                               SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET = 1, SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN = 2, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN = 3, &
                                  SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES = 4
    !> Kinds of an indentation (an indentation function's value): none (SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE), a
    !> column (SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN), no indentation (SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT).
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE = 0, &
                            SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN = 1, SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT = 2

    !> One right-hand side of a BACKUS_NAUR_FORM rule.
    type, public :: simple_minded_indentation_engine_rhs_type
        type(string_type), allocatable :: items(:)
    end type simple_minded_indentation_engine_rhs_type

    !> A BACKUS_NAUR_FORM rule: NONTERM and its right-hand sides.
    type, public :: simple_minded_indentation_engine_nonterminal_type
        character(len=:), allocatable :: nt
        type(simple_minded_indentation_engine_rhs_type), allocatable :: rhs(:)
    end type simple_minded_indentation_engine_nonterminal_type

    !> A BACKUS_NAUR_FORM grammar.  `add' appends a right-hand side (an array of tokens
    !> and nonterminals, trailing blanks not part of them) to the rule of
    !> NT when that is the last rule, else starts a new rule.
    type, public :: simple_minded_indentation_engine_backus_naur_form_type
        type(simple_minded_indentation_engine_nonterminal_type), allocatable :: rules(:)
    contains
        procedure :: append => backus_naur_form_append
        !> Add the nonterminal NT with no right-hand sides, (NT).
        procedure :: append_nonterminal => backus_naur_form_append_nonterminal
    end type simple_minded_indentation_engine_backus_naur_form_type

    !> One precedence class (ASSOCIATIVITY OP ...).
    type, public :: simple_minded_indentation_engine_prec_type
        integer :: assoc = SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC
        type(string_type), allocatable :: ops(:)
    end type simple_minded_indentation_engine_prec_type

    !> A precs table: precedence classes sorted from lowest to highest
    !> precedence.  `add' appends a class.
    type, public :: simple_minded_indentation_engine_precs_type
        type(simple_minded_indentation_engine_prec_type), allocatable :: precs(:)
    contains
        procedure :: append => precs_append
    end type simple_minded_indentation_engine_precs_type

    !> A constraint (T1 RELATIVE T2) resolving a conflict.
    type, public :: simple_minded_indentation_engine_constraint_type
        character(len=:), allocatable :: t1, t2
        integer :: relative = SIMPLE_MINDED_INDENTATION_ENGINE_EQ
    end type simple_minded_indentation_engine_constraint_type

    !> A 2D precedence table: the relation `relative' between `x' and `y', and
    !> the :smie-open/close-alist and :smie-closer-alist of a table made
    !> from a BACKUS_NAUR_FORM.
    type, public :: simple_minded_indentation_engine_prec2_type
        integer :: n = 0
        type(string_type), allocatable :: x(:), y(:)
        integer, allocatable :: relative(:)
        logical :: has_open_close = .false.
        type(string_type), allocatable :: class_tokens(:)
        integer, allocatable :: classes(:)
        logical :: has_closer_table = .false.
        type(string_type), allocatable :: openers(:), closers(:)
    end type simple_minded_indentation_engine_prec2_type

    !> A precedence level.  Two list levels are the same (eq) only if
    !> they are the same level of the same token (`id').
    type, public :: simple_minded_indentation_engine_level_type
        integer :: kind = SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE
        integer :: value = 0
        integer :: id = 0
    end type simple_minded_indentation_engine_level_type

    !> A precedence-level table (`simple_minded_indentation_engine_grammar'): (TOKEN LEFT-LEVEL
    !> RIGHT-LEVEL) for each token, and the :smie-closer-alist (OPENER and
    !> CLOSER for each).  `unset' is the value 'unset.
    type, public :: simple_minded_indentation_engine_grammar_type
        logical :: unset = .true.
        type(string_type), allocatable :: tokens(:)
        type(simple_minded_indentation_engine_level_type), allocatable :: left(:), right(:)
        logical :: has_closer_table = .false.
        type(string_type), allocatable :: openers(:), closers(:)
    end type simple_minded_indentation_engine_grammar_type

    !> What `simple_minded_indentation_engine_next_expression' returns: nothing (`null'), or LEVEL, POSITION and TOKEN
    !> (`token' unallocated for none, `position' 0 too).
    type, public :: simple_minded_indentation_engine_expression_type
        logical :: null = .true.
        type(simple_minded_indentation_engine_level_type) :: level
        integer(int64) :: position = 0
        character(len=:), allocatable :: token
    end type simple_minded_indentation_engine_expression_type

    !> A token and its levels: TOKEN, LEFT and RIGHT, or nothing (`null').
    type, public :: simple_minded_indentation_engine_toklevels_type
        logical :: null = .true.
        character(len=:), allocatable :: token
        type(simple_minded_indentation_engine_level_type) :: left, right
    end type simple_minded_indentation_engine_toklevels_type

    !> What a `simple_minded_indentation_engine_rules_function' returns (`kind' one of SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE,
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET, SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN,
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN, SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES).
    type, public :: simple_minded_indentation_engine_rule_result_type
        integer :: kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE
        integer :: value = 0
        character(len=:), allocatable :: token
    end type simple_minded_indentation_engine_rule_result_type

    !> The longest token or mode name of a `simple_minded_indentation_engine_configuration_rule_type'.
    integer, parameter, public :: SIMPLE_MINDED_INDENTATION_ENGINE_CONFIGURATION_NAME_MAXIMUM = 64

    !> A rule of the user configuration of SIMPLE_MINDED_INDENTATION_ENGINE indentation (an element
    !> (NEW KIND TOKEN NORMAL) of upstream's RULES): when
    !> `simple_minded_indentation_engine_rules_function' returns NORMAL (of kind NORMAL_KIND) for METHOD
    !> (upstream's KIND) and TOKEN, use NEW (of kind NEW_KIND) instead.  In
    !> `simple_minded_indentation_engine_configuration', MODE names the major mode the rule is for.  Kinds are
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET (the default), SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN,
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE and
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES.
    type, public :: simple_minded_indentation_engine_configuration_rule_type
        character(len=SIMPLE_MINDED_INDENTATION_ENGINE_CONFIGURATION_NAME_MAXIMUM) :: mode = ""
        integer :: method = SIMPLE_MINDED_INDENTATION_ENGINE_AFTER
        character(len=SIMPLE_MINDED_INDENTATION_ENGINE_CONFIGURATION_NAME_MAXIMUM) :: token = ""
        integer :: normal = 0
        integer :: new = 0
        integer :: normal_kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
        integer :: new_kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
    end type simple_minded_indentation_engine_configuration_rule_type

    ! The signature of a call of `simple_minded_indentation_engine_rules_function': METHOD, TOKEN and the
    ! answer's KIND and VALUE.
    type :: simple_minded_indentation_engine_signature_type
        integer :: method = 0
        character(len=:), allocatable :: token
        integer :: kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE
        integer :: value = 0
    end type simple_minded_indentation_engine_signature_type

    ! One call of a trace: where, its signature, and the rule that
    ! rewrote its answer (if `rewritten').
    type :: trace_entry_type
        integer(int64) :: position = 0
        type(simple_minded_indentation_engine_signature_type) :: signature
        logical :: rewritten = .false.
        type(simple_minded_indentation_engine_configuration_rule_type) :: rule
    end type trace_entry_type

    ! An offset found with a trace, and how often (smie-config--guess's
    ! otraces).
    type :: otrace_type
        integer :: offset = 0
        integer :: count = 0
        type(simple_minded_indentation_engine_signature_type), allocatable :: signatures(:)
    end type otrace_type

    ! The data of an adjustable signature: the times it was found, its offsets
    ! and their counts (the newest first), the traces it occurs in (the
    ! newest first) and the trace whose signature is its key (OWNER).
    type :: signature_data_type
        type(simple_minded_indentation_engine_signature_type) :: signature
        integer :: total = 0
        integer :: owner = 0
        integer, allocatable :: offsets(:), counts(:), cotraces(:)
    end type signature_data_type

    !> User configuration of SIMPLE_MINDED_INDENTATION_ENGINE indentation.
    !> This is a list of rules (`simple_minded_indentation_engine_configuration_rule_type'), each for a MODE,
    !> describing when and how to change the indentation rules: KIND and TOKEN
    !> are the elements passed to `simple_minded_indentation_engine_rules_function',
    !> NORMAL is the value returned by `simple_minded_indentation_engine_rules_function' and NEW is the
    !> value with which to replace it.  Port of smie.el smie-config.
    type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: simple_minded_indentation_engine_configuration(:)
    public :: simple_minded_indentation_engine_configuration

    ! smie-config--trace: whether calls of `simple_minded_indentation_engine_rules_function' are
    ! recorded in `trace'.
    logical :: tracing = .false.
    type(trace_entry_type), allocatable :: trace(:)

    !> An indentation: none, a column or no indentation (SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT).
    type, public :: simple_minded_indentation_engine_indent_type
        integer :: kind = SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE
        integer :: column = 0
    end type simple_minded_indentation_engine_indent_type

    abstract interface
        !> A token function (`simple_minded_indentation_engine_forward_token_function',
        !> `simple_minded_indentation_engine_backward_token_function'): move over the next token and
        !> return it (unallocated for none).  Not a port.
        subroutine simple_minded_indentation_token_procedure(token, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: token
            type(status_type), intent(out) :: status
        end subroutine simple_minded_indentation_token_procedure

        !> A `simple_minded_indentation_engine_rules_function': METHOD and ARGUMENT as documented there.
        !> Not a port.
        subroutine simple_minded_indentation_engine_rules_procedure(method, argument, result, status)
            import :: simple_minded_indentation_engine_rule_result_type, status_type
            implicit none(type, external)
            integer, intent(in) :: method
            character(len=*), intent(in) :: argument
            type(simple_minded_indentation_engine_rule_result_type), intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine simple_minded_indentation_engine_rules_procedure

        !> smie--hanging-eolp-function: move over what may follow a
        !> hanging token and tell whether the line ends there.  Not a port.
        logical function simple_minded_indentation_engine_hanging_line_procedure()
            implicit none(type, external)
        end function simple_minded_indentation_engine_hanging_line_procedure

        !> A function of `simple_minded_indentation_engine_indent_functions': the column point should
        !> be indented to, or none, or no indentation (SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT).  It shouldn't move point.
        !> Not a port.
        subroutine simple_minded_indentation_engine_indent_procedure(indent, status)
            import :: simple_minded_indentation_engine_indent_type, status_type
            implicit none(type, external)
            type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
            type(status_type), intent(out) :: status
        end subroutine simple_minded_indentation_engine_indent_procedure
    end interface

    ! One function of `simple_minded_indentation_engine_indent_functions', or the t of a buffer's own
    ! list.
    type :: indent_entry_type
        procedure(simple_minded_indentation_engine_indent_procedure), pointer, nopass :: fn => null()
        logical :: is_t = .false.
    end type indent_entry_type

    ! A buffer's own `simple_minded_indentation_engine_indent_functions'.
    type :: indent_local_type
        integer(int64) :: serial = 0
        type(indent_entry_type), allocatable :: entries(:)
    end type indent_local_type

    !> The type of `simple_minded_indentation_engine_indent_functions': the default functions and
    !> buffers' own lists (where t stands for the default ones), changed
    !> with `prepend', `append' and `remove' as add-hook and remove-hook do.
    type, public :: simple_minded_indentation_engine_indent_functions_type
        type(indent_entry_type), allocatable :: entries(:)
        type(indent_local_type), allocatable :: locals(:)
    contains
        !> Add FN at the front, as add-hook does (to the current buffer's own
        !> list with LOCAL).
        procedure :: prepend => indent_functions_prepend
        !> Add FN at the end, as add-hook with APPEND does (to the current
        !> buffer's own list with LOCAL).
        procedure :: append => indent_functions_append
        procedure, private :: place => indent_functions_place
        !> Remove FN (from the current buffer's own list with LOCAL).
        procedure :: remove => indent_functions_remove
        procedure, private :: local_index => indent_functions_local_index
        procedure, private :: get_current => indent_functions_current
    end type simple_minded_indentation_engine_indent_functions_type

    ! A buffer's own SIMPLE_MINDED_INDENTATION_ENGINE variables.
    type :: simple_minded_indentation_engine_local_type
        integer(int64) :: serial = 0
        type(simple_minded_indentation_engine_grammar_type), pointer :: grammar => null()
        procedure(simple_minded_indentation_engine_rules_procedure), pointer, nopass :: rules => null()
        logical :: has_rules = .false.
        procedure(simple_minded_indentation_token_procedure), pointer, nopass :: forward_token => null()
        procedure(simple_minded_indentation_token_procedure), pointer, nopass :: backward_token => null()
        logical :: has_closer_table = .false.
        logical :: has_triggers = .false.
        character(len=:), allocatable :: triggers
        ! smie--matching-block-data-cache: ((POSITION . TICK) . DATA).
        logical :: has_cache = .false.
        integer(int64) :: cache_position = 0, cache_tick = 0
        logical :: has_cache_data = .false.
        type(show_parenthesis_data_type) :: cache_data
        ! The functions SIMPLE_MINDED_INDENTATION_ENGINE's :around advice wraps (null for the global
        ! value at the time of the call).
        logical :: has_auto_fill = .false., auto_fill_local = .false.
        procedure(command_procedure), pointer, nopass :: auto_fill => null()
        logical :: has_parenthesis_data = .false., parenthesis_data_local = .false.
        procedure(show_parenthesis_data_procedure), pointer, nopass :: parenthesis_data => null()
        ! smie-config--buffer-local and smie-config--mode-local.
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: buffer_rules(:), mode_rules(:)
    end type simple_minded_indentation_engine_local_type

    !> List of token parsing info.
    !> This list is normally built by `simple_minded_indentation_engine_prec2_to_grammar'.
    !> Each element holds a TOKEN, a LEFT-LEVEL and a RIGHT-LEVEL.
    !> Parsing is done using an operator precedence parser.
    !> LEFT-LEVEL and RIGHT-LEVEL can be either numbers or SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST, where
    !> SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST
    !> means that this operator does not bind on the corresponding side,
    !> e.g. a LEFT-LEVEL of SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE means this is a token that behaves somewhat like
    !> an open-paren, whereas a RIGHT-LEVEL of SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE would correspond to something
    !> like a close-paren.  Port of smie.el smie-grammar (the default value,
    !> 'unset; `simple_minded_indentation_engine_setup' gives a buffer its own).
    type(simple_minded_indentation_engine_grammar_type), target, public :: simple_minded_indentation_engine_grammar

    !> One buffer's own value of
    !> `simple_minded_indentation_engine_hanging_line_test_function'.
    type :: simple_minded_indentation_engine_hanging_line_test_entry_type
        procedure(simple_minded_indentation_engine_hanging_line_procedure), pointer, nopass :: procedure => null()
    end type simple_minded_indentation_engine_hanging_line_test_entry_type

    !> The type of
    !> `simple_minded_indentation_engine_hanging_line_test_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: simple_minded_indentation_engine_hanging_line_test_setting_type
        procedure(simple_minded_indentation_engine_hanging_line_procedure), pointer, nopass :: procedure => null()
        type(simple_minded_indentation_engine_hanging_line_test_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_simple_minded_indentation_engine_hanging_line_test_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_simple_minded_indentation_engine_hanging_line_test_current
        procedure :: append_local_entry => append_simple_minded_indentation_engine_hanging_line_test_entry
        procedure :: clear_local_entry => clear_simple_minded_indentation_engine_hanging_line_test_entry
    end type simple_minded_indentation_engine_hanging_line_test_setting_type

    !> smie--hanging-eolp-function: the test of whether the line ends
    !> after a hanging token.  A mode advises it in a buffer
    !> (add-function :around (local ...)) with `%set_local', its procedure
    !> calling `is_simple_minded_indentation_engine_default_hanging_end_of_line'
    !> for the original; null for the default.  Port of smie.el
    !> smie--hanging-eolp-function.
    type(simple_minded_indentation_engine_hanging_line_test_setting_type), public :: &
        simple_minded_indentation_engine_hanging_line_test_function

    !> Function to scan forward for the next token.
    !> Called with no argument should return a token and move to its end.
    !> If no token is found, return an unallocated or empty string.
    !> It can return an unallocated string when bumping into a parenthesis, which lets SIMPLE_MINDED_INDENTATION_ENGINE
    !> use syntax-tables to handle them in efficient C code.  Port of
    !> smie.el smie-forward-token-function (the default value).
    procedure(simple_minded_indentation_token_procedure), pointer :: simple_minded_indentation_engine_forward_token_function => &
        null()
    public :: simple_minded_indentation_engine_forward_token_function

    !> Function to scan backward the previous token.
    !> Same calling convention as `simple_minded_indentation_engine_forward_token_function' except
    !> it should move backward to the beginning of the previous token.
    !> Port of smie.el smie-backward-token-function (the default value).
    procedure(simple_minded_indentation_token_procedure), pointer :: simple_minded_indentation_engine_backward_token_function => &
        null()
    public :: simple_minded_indentation_engine_backward_token_function

    !> Chars which might trigger `blink_matching_open'.
    !> These can include the final chars of end-tokens, or chars that are
    !> typically inserted right after an end token.  Port of smie.el
    !> smie-blink-matching-triggers (the default value, a string of the
    !> characters).
    character(len=:), allocatable, public :: simple_minded_indentation_engine_blink_matching_triggers

    !> Whether SIMPLE_MINDED_INDENTATION_ENGINE should blink to matching opener for inner keywords.
    !> If true, it will blink not only for "begin..end" but also for "if...else".
    !> Port of smie.el smie-blink-matching-inners.
    logical, public :: simple_minded_indentation_engine_blink_matching_inners = .true.

    !> Basic amount of indentation.  Port of smie.el smie-indent-basic.
    integer, public :: simple_minded_indentation_engine_indent_basic = 4

    !> Function providing the indentation rules.
    !> It takes two arguments METHOD and ARGUMENT where the meaning of ARGUMENT
    !> and the expected return value depends on METHOD.
    !> METHOD can be:
    !> - `SIMPLE_MINDED_INDENTATION_ENGINE_AFTER', in which case ARGUMENT is a token and the function should return the
    !>   OFFSET to use for indentation after ARGUMENT.
    !> - `SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE', in which case ARGUMENT is a token and the function should return the
    !>   OFFSET to use to indent ARGUMENT itself.
    !> - `SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT', in which case the function should return either:
    !>   - the offset to use to indent function arguments (ARGUMENT = "args")
    !>   - the basic indentation step (ARGUMENT = "basic").
    !>   - the token to use (when ARGUMENT = "empty_line_token") when we don't know how
    !>     to indent an empty line.
    !> - `SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO', in which case ARGUMENT is a token and the function should return
    !>   true if TOKEN is followed by a list of expressions (not separated by any
    !>   token) rather than an expression.
    !> - `SIMPLE_MINDED_INDENTATION_ENGINE_CLOSE_ALL', in which case ARGUMENT is a close-paren token at indentation and
    !>   the function should return true if it should be aligned with the opener
    !>   of the last close-paren token on the same line, if there are multiple.
    !>   Otherwise, it will be aligned with its own opener.
    !>
    !> When ARGUMENT is a token, the function is called with point just before that token.
    !> A return value of SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE always means to fallback on the default behavior, so the
    !> function should return SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE for arguments it does not expect.
    !>
    !> OFFSET can be:
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE                     use the default indentation rule.
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN, COLUMN     indent to column COLUMN.
    !> SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET, NUMBER     offset by NUMBER, relative to a base token
    !>                              which is the current token for SIMPLE_MINDED_INDENTATION_ENGINE_AFTER and
    !>                              its parent for SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE.
    !>
    !> The functions whose name starts with "simple_minded_indentation_engine_rule_" are helper functions
    !> designed specifically for use in this function.  Port of smie.el
    !> smie-rules-function (the default value; null is `ignore').
    procedure(simple_minded_indentation_engine_rules_procedure), pointer :: simple_minded_indentation_engine_rules_function => &
        null()
    public :: simple_minded_indentation_engine_rules_function

    !> How far a separator is outdented when nothing follows it on its
    !> line.  Port of smie.el smie-rule-separator-outdent.
    integer, public :: simple_minded_indentation_engine_rule_separator_outdent = 2

    !> Functions to compute the indentation.
    !> Each function is called with no argument, shouldn't move point, and should
    !> return either SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE if it has no opinion, or an integer representing the column
    !> to which that point should be aligned, if we were to reindent it.
    !> Port of smie.el smie-indent-functions.
    type(simple_minded_indentation_engine_indent_functions_type), public :: simple_minded_indentation_engine_indent_functions

    ! (NT . OPS) lists of the BACKUS_NAUR_FORM conversion.
    type :: nt_ops_type
        character(len=:), allocatable :: nt
        type(string_type), allocatable :: ops(:)
    end type nt_ops_type

    ! A path of smie-debug--prec2-cycle: (START . REST), REST reversed.
    type :: path_type
        integer, allocatable :: nodes(:)
    end type path_type

    type(simple_minded_indentation_engine_local_type), allocatable :: locals(:)

    ! smie-warning-count.
    integer :: warning_count = 0

    ! The dynamically bound smie--parent, smie--after and smie--token.
    type(simple_minded_indentation_engine_expression_type) :: simple_minded_indentation_engine_parent_variable
    integer(int64) :: simple_minded_indentation_engine_after_variable = 0
    character(len=:), allocatable :: simple_minded_indentation_engine_token

    ! The counter giving list levels their identities.
    integer :: level_ids = 0

contains

    ! ------------------------------------------------------------------
    ! Building the grammar data
    ! ------------------------------------------------------------------

    ! Add the rule NT -> ITEMS to the BACKUS_NAUR_FORM grammar SELF.  Not a port.
    subroutine backus_naur_form_append(self, nt, items)
        class(simple_minded_indentation_engine_backus_naur_form_type), intent(inout) :: self
        character(len=*), intent(in) :: nt
        character(len=*), intent(in) :: items(:)
        type(simple_minded_indentation_engine_nonterminal_type) :: rule
        type(simple_minded_indentation_engine_rhs_type), allocatable :: rhs(:)
        integer :: n, i, m

        if (.not. allocated(self%rules)) allocate (self%rules(0))
        n = size(self%rules)

        if (n > 0) then
            if (self%rules(n)%nt == nt) then
                m = size(self%rules(n)%rhs)
                allocate (rhs(m + 1))

                do i = 1, m
                    call move_alloc(self%rules(n)%rhs(i)%items, rhs(i)%items)
                end do

                call make_items(items, rhs(m + 1))
                call move_alloc(rhs, self%rules(n)%rhs)
                return
            end if
        end if

        rule%nt = nt
        allocate (rule%rhs(1))
        call make_items(items, rule%rhs(1))
        self%rules = [self%rules, rule]
    end subroutine backus_naur_form_append

    ! Make NT a nonterminal of the BACKUS_NAUR_FORM grammar SELF.  Not a port.
    subroutine backus_naur_form_append_nonterminal(self, nt)
        class(simple_minded_indentation_engine_backus_naur_form_type), intent(inout) :: self
        character(len=*), intent(in) :: nt
        type(simple_minded_indentation_engine_nonterminal_type) :: rule

        if (.not. allocated(self%rules)) allocate (self%rules(0))
        rule%nt = nt
        allocate (rule%rhs(0))
        self%rules = [self%rules, rule]
    end subroutine backus_naur_form_append_nonterminal

    ! The right-hand side RHS made of ITEMS.  Not a port.
    subroutine make_items(items, rhs)
        character(len=*), intent(in) :: items(:)
        type(simple_minded_indentation_engine_rhs_type), intent(out) :: rhs
        integer :: i

        allocate (rhs%items(size(items)))

        do i = 1, size(items)
            rhs%items(i)%text = trim(items(i))
        end do
    end subroutine make_items

    ! Add the precedence level of OPS, with associativity ASSOC, to SELF.
    ! Not a port.
    subroutine precs_append(self, assoc, ops)
        class(simple_minded_indentation_engine_precs_type), intent(inout) :: self
        integer, intent(in) :: assoc
        character(len=*), intent(in) :: ops(:)
        type(simple_minded_indentation_engine_prec_type) :: prec
        integer :: i

        if (.not. allocated(self%precs)) allocate (self%precs(0))
        prec%assoc = assoc
        allocate (prec%ops(size(ops)))

        do i = 1, size(ops)
            prec%ops(i)%text = trim(ops(i))
        end do

        self%precs = [self%precs, prec]
    end subroutine precs_append

    ! The index of the pair (X . Y) in TABLE, 0 if none.  Not a port.
    integer function prec2_index(table, x, y) result(k)
        type(simple_minded_indentation_engine_prec2_type), intent(in) :: table
        character(len=*), intent(in) :: x, y

        do k = 1, table%n
            if (table%x(k)%text == x .and. len(table%x(k)%text) == len(x)) then
                if (table%y(k)%text == y .and. len(table%y(k)%text) == len(y)) return
            end if
        end do

        k = 0
    end function prec2_index

    ! (puthash (X . Y) RELATIVE TABLE).  Not a port.
    subroutine prec2_set(table, x, y, relative)
        type(simple_minded_indentation_engine_prec2_type), intent(inout) :: table
        character(len=*), intent(in) :: x, y
        integer, intent(in) :: relative
        type(string_type), allocatable :: gx(:), gy(:)
        integer, allocatable :: grel(:)
        integer :: k, cap

        k = prec2_index(table, x, y)

        if (k > 0) then
            table%relative(k) = relative
            return
        end if

        if (.not. allocated(table%x)) then
            allocate (table%x(16), table%y(16), table%relative(16))
        else if (table%n == size(table%x)) then
            cap = 2*size(table%x)
            allocate (gx(cap), gy(cap), grel(cap))

            do k = 1, table%n
                call move_alloc(table%x(k)%text, gx(k)%text)
                call move_alloc(table%y(k)%text, gy(k)%text)
                grel(k) = table%relative(k)
            end do

            call move_alloc(gx, table%x)
            call move_alloc(gy, table%y)
            call move_alloc(grel, table%relative)
        end if

        table%n = table%n + 1
        table%x(table%n)%text = x
        table%y(table%n)%text = y
        table%relative(table%n) = relative
    end subroutine prec2_set

    !> Set the relation of X and Y in TABLE to VALUE, unless it already has
    !> another one; then the relation in OVERRIDE, if it has one, wins and
    !> otherwise the old one stays (a conflict).  Port of
    !> smie.el:smie-set-prec2tab.
    subroutine simple_minded_indentation_engine_set_prec2tab(table, x, y, value, override)
        type(simple_minded_indentation_engine_prec2_type), intent(inout) :: table
        character(len=*), intent(in) :: x, y
        integer, intent(in) :: value
        type(simple_minded_indentation_engine_prec2_type), intent(in), optional :: override
        integer :: k, o

        k = prec2_index(table, x, y)

        if (k > 0) then
            if (table%relative(k) /= value) then
                o = 0
                if (present(override)) o = prec2_index(override, x, y)

                if (o > 0) then
                    table%relative(k) = override%relative(o)
                else
                    ! FIXME: The override is meant to resolve ambiguities,
                    ! but it also hides real conflicts.  (The warning
                    ! "Conflict: X OLD/VALUE Y" is deferred.)
                    warning_count = warning_count + 1
                end if

                return
            end if
        end if

        call prec2_set(table, x, y, value)
    end subroutine simple_minded_indentation_engine_set_prec2tab

    !> Compute a 2D precedence table from a list of precedences.
    !> PRECS should be a list, sorted by precedence (e.g. "+" will
    !> come before "*"), of elements of the form (left OP ...)
    !> or (right OP ...) or (nonassoc OP ...) or (assoc OP ...).  All operators in
    !> one of those elements share the same precedence level and associativity.
    !> Port of smie.el:smie-precs->prec2.
    subroutine simple_minded_indentation_engine_precs_to_prec2(precs, prec2)
        type(simple_minded_indentation_engine_precs_type), intent(in) :: precs
        type(simple_minded_indentation_engine_prec2_type), intent(out) :: prec2
        integer :: p, i, j, q, selfrule, op1, op2, np

        np = 0
        if (allocated(precs%precs)) np = size(precs%precs)

        do p = 1, np
            associate (prec => precs%precs(p))
                do i = 1, size(prec%ops)
                    select case (prec%assoc)
                    case (SIMPLE_MINDED_INDENTATION_ENGINE_LEFT)
                        selfrule = SIMPLE_MINDED_INDENTATION_ENGINE_GT
                    case (SIMPLE_MINDED_INDENTATION_ENGINE_RIGHT)
                        selfrule = SIMPLE_MINDED_INDENTATION_ENGINE_LT
                    case (SIMPLE_MINDED_INDENTATION_ENGINE_ASSOC)
                        selfrule = SIMPLE_MINDED_INDENTATION_ENGINE_EQ
                    case default
                        selfrule = 0
                    end select

                    if (selfrule /= 0) then
                        do j = 1, size(prec%ops)
                            call simple_minded_indentation_engine_set_prec2tab(prec2, prec%ops(i)%text, prec%ops(j)%text, selfrule)
                        end do
                    end if

                    op1 = SIMPLE_MINDED_INDENTATION_ENGINE_LT
                    op2 = SIMPLE_MINDED_INDENTATION_ENGINE_GT

                    do q = 1, np
                        if (q == p) then
                            op1 = SIMPLE_MINDED_INDENTATION_ENGINE_GT
                            op2 = SIMPLE_MINDED_INDENTATION_ENGINE_LT
                        else
                            do j = 1, size(precs%precs(q)%ops)
                                call simple_minded_indentation_engine_set_prec2tab(prec2, prec%ops(i)%text, &
                                                                                   precs%precs(q)%ops(j)%text, op2)
                                call simple_minded_indentation_engine_set_prec2tab(prec2, precs%precs(q)%ops(j)%text, &
                                                                                   prec%ops(i)%text, op1)
                            end do
                        end if
                    end do
                end do
            end associate
        end do
    end subroutine simple_minded_indentation_engine_precs_to_prec2

    !> Merge the 2D precedence TABLES into PREC2.  Port of
    !> smie.el:smie-merge-prec2s.
    subroutine simple_minded_indentation_engine_merge_prec2s(tables, prec2, status)
        type(simple_minded_indentation_engine_prec2_type), intent(in) :: tables(:)
        type(simple_minded_indentation_engine_prec2_type), intent(out) :: prec2
        type(status_type), intent(out) :: status
        integer :: t, k

        status = status_type()

        if (size(tables) == 1) then
            prec2 = tables(1)
            return
        end if

        do t = 1, size(tables)
            do k = 1, tables(t)%n
                call simple_minded_indentation_engine_set_prec2tab(prec2, tables(t)%x(k)%text, tables(t)%y(k)%text, &
                                                                   tables(t)%relative(k))
            end do

            if (tables(t)%has_open_close) then
                if (prec2%has_open_close) then
                    if (.not. same_classes(prec2, tables(t))) then
                        call set_error(status, GENERIC_ERROR_CODE, "Conflicting values for :smie-open/close-alist property")
                        return
                    end if
                else
                    prec2%has_open_close = .true.
                    prec2%class_tokens = tables(t)%class_tokens
                    prec2%classes = tables(t)%classes
                end if
            end if

            if (tables(t)%has_closer_table) then
                if (prec2%has_closer_table) then
                    if (.not. same_pairs(prec2%openers, prec2%closers, tables(t)%openers, tables(t)%closers)) then
                        call set_error(status, GENERIC_ERROR_CODE, "Conflicting values for :smie-closer-alist property")
                        return
                    end if
                else
                    prec2%has_closer_table = .true.
                    prec2%openers = tables(t)%openers
                    prec2%closers = tables(t)%closers
                end if
            end if
        end do
    end subroutine simple_minded_indentation_engine_merge_prec2s

    ! Whether the prec2 tables A and B have the same classes.  Not a port.
    logical function same_classes(a, b) result(same)
        type(simple_minded_indentation_engine_prec2_type), intent(in) :: a, b
        integer :: i

        same = size(a%classes) == size(b%classes)
        if (.not. same) return

        do i = 1, size(a%classes)
            same = a%classes(i) == b%classes(i) .and. string_eq(a%class_tokens(i)%text, b%class_tokens(i)%text)
            if (.not. same) return
        end do
    end function same_classes

    ! Whether the pairs (A1 . A2) and (B1 . B2) are equal.  Not a port.
    logical function same_pairs(a1, a2, b1, b2) result(same)
        type(string_type), intent(in) :: a1(:), a2(:), b1(:), b2(:)
        integer :: i

        same = size(a1) == size(b1)
        if (.not. same) return

        do i = 1, size(a1)
            same = string_eq(a1(i)%text, b1(i)%text) .and. string_eq(a2(i)%text, b2(i)%text)
            if (.not. same) return
        end do
    end function same_pairs

    ! Whether A and B are the same string, lengths included.  Not a port.
    pure logical function string_eq(a, b)
        character(len=*), intent(in) :: a, b

        string_eq = len(a) == len(b)
        if (string_eq) string_eq = a == b
    end function string_eq

    ! Whether S is one of LIST.  Not a port.
    pure logical function has_string(s, list)
        character(len=*), intent(in) :: s
        type(string_type), intent(in) :: list(:)
        integer :: i

        has_string = .false.

        do i = 1, size(list)
            if (string_eq(s, list(i)%text)) then
                has_string = .true.
                return
            end if
        end do
    end function has_string

    ! cl-pushnew S onto LIST (at the front, if it is not there).
    ! Not a port.
    subroutine pushnew(s, list)
        character(len=*), intent(in) :: s
        type(string_type), allocatable, intent(inout) :: list(:)

        if (has_string(s, list)) return
        call prepend_string(list, s)
    end subroutine pushnew

    !> Convert the BACKUS_NAUR_FORM grammar into a prec2 table.
    !> BACKUS_NAUR_FORM is a list of nonterminal definitions of the form:
    !>   (NONTERM RHS1 RHS2 ...)
    !> where each RHS is a (non-empty) list of terminals (aka tokens) or non-terminals.
    !> Not all grammars are accepted:
    !> - an RHS cannot be an empty list (this is not needed, since SIMPLE_MINDED_INDENTATION_ENGINE allows all
    !>   non-terminals to match the empty string anyway).
    !> - an RHS cannot have 2 consecutive non-terminals: between each non-terminal
    !>   needs to be a terminal (aka token).  This is a fundamental limitation of
    !>   the parsing technology used (operator precedence grammar).
    !> Additionally, conflicts can occur:
    !> - The returned prec2 table holds constraints between two
    !>   tokens, and for any given two only one constraint can be
    !>   present, either: T1 < T2, T1 = T2, or T1 > T2.
    !> - A token can either be an `opener' (something similar to an open-paren),
    !>   a `closer' (like a close-paren), or `neither' of the two (e.g. an infix
    !>   operator, or an inner token like "else").
    !> Conflicts can be resolved via RESOLVERS, which can be
    !> - precs tables (see `simple_minded_indentation_engine_precs_to_prec2') to resolve conflicting
    !>   constraints (PRECS),
    !> - constraints (T1 RELATIVE T2) where RELATIVE is one of SIMPLE_MINDED_INDENTATION_ENGINE_EQ,
    !> SIMPLE_MINDED_INDENTATION_ENGINE_LT or
    !>   SIMPLE_MINDED_INDENTATION_ENGINE_GT (CONSTRAINTS).
    !> Port of smie.el:smie-bnf->prec2.
    subroutine simple_minded_indentation_engine_backus_naur_form_to_prec2(backus_naur_form, prec2, status, precs, constraints)
        type(simple_minded_indentation_engine_backus_naur_form_type), intent(in) :: backus_naur_form
        type(simple_minded_indentation_engine_prec2_type), intent(out) :: prec2
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_precs_type), intent(in), optional :: precs(:)
        type(simple_minded_indentation_engine_constraint_type), intent(in), optional :: constraints(:)
        type(string_type), allocatable :: nonterminals(:), first_ops(:), last_ops(:), first_nonterminals(:), last_nonterminals(:)
        type(nt_ops_type), allocatable :: first_ops_table(:), last_ops_table(:), first_nonterminals_table(:), &
                                          last_nonterminals_table(:)
        type(simple_minded_indentation_engine_prec2_type) :: over, override
        type(simple_minded_indentation_engine_prec2_type), allocatable :: tables(:)
        type(string_type), allocatable :: ops(:)
        integer :: r, h, i, n, nrules, saved_count, k, j

        status = status_type()
        saved_count = warning_count
        warning_count = 0
        nrules = 0
        if (allocated(backus_naur_form%rules)) nrules = size(backus_naur_form%rules)
        ! Non-terminals.
        allocate (nonterminals(nrules))

        do r = 1, nrules
            nonterminals(r)%text = backus_naur_form%rules(r)%nt
        end do

        allocate (first_ops_table(0), last_ops_table(0), first_nonterminals_table(0), last_nonterminals_table(0))

        ! The override: constraints and precs tables.
        if (present(constraints)) then
            do i = 1, size(constraints)
                call simple_minded_indentation_engine_set_prec2tab(over, constraints(i)%t1, constraints(i)%t2, &
                                                                   constraints(i)%relative)
            end do
        end if

        n = 0
        if (present(precs)) n = size(precs)
        allocate (tables(n + 1))
        tables(1) = over

        ! (The precs are pushed, so they are merged last first.)
        do i = 1, n
            call simple_minded_indentation_engine_precs_to_prec2(precs(n + 1 - i), tables(i + 1))
        end do

        call simple_minded_indentation_engine_merge_prec2s(tables, override, status)

        if (.not. status%ok()) then
            warning_count = saved_count
            return
        end if

        do r = 1, nrules
            allocate (first_ops(0), last_ops(0), first_nonterminals(0), last_nonterminals(0))

            do h = 1, size(backus_naur_form%rules(r)%rhs)
                associate (rhs => backus_naur_form%rules(r)%rhs(h)%items)
                    n = size(rhs)

                    if (n == 0) then
                        call set_error(status, GENERIC_ERROR_CODE, "Wrong type argument: expected nonempty array")
                        warning_count = saved_count
                        return
                    end if

                    if (.not. has_string(rhs(1)%text, nonterminals)) then
                        call pushnew(rhs(1)%text, first_ops)
                    else
                        call pushnew(rhs(1)%text, first_nonterminals)

                        if (n > 1) then
                            ! If the first is not an OP we add the second (which
                            ! should be an OP if BACKUS_NAUR_FORM is an "operator grammar").
                            ! Strictly speaking, this should only be done if the
                            ! first is a non-terminal which can expand to a phrase
                            ! without any OP in it, but checking doesn't seem worth
                            ! the trouble, and it lets the writer of the BACKUS_NAUR_FORM
                            ! be a bit more sloppy by skipping uninteresting base
                            ! cases which are terminals but not OPs.
                            if (has_string(rhs(2)%text, nonterminals)) then
                                call set_error(status, GENERIC_ERROR_CODE, "Adjacent non-terminals: "//rhs(1)%text//" "// &
                                               rhs(2)%text)
                                warning_count = saved_count
                                return
                            end if

                            call pushnew(rhs(2)%text, first_ops)
                        end if
                    end if

                    if (.not. has_string(rhs(n)%text, nonterminals)) then
                        call pushnew(rhs(n)%text, last_ops)
                    else
                        call pushnew(rhs(n)%text, last_nonterminals)

                        if (n > 1) then
                            if (has_string(rhs(n - 1)%text, nonterminals)) then
                                call set_error(status, GENERIC_ERROR_CODE, "Adjacent non-terminals: "//rhs(n - 1)%text//" "// &
                                               rhs(n)%text)
                                warning_count = saved_count
                                return
                            end if

                            call pushnew(rhs(n - 1)%text, last_ops)
                        end if
                    end if
                end associate
            end do

            call append_table(first_ops_table, backus_naur_form%rules(r)%nt, first_ops)
            call append_table(last_ops_table, backus_naur_form%rules(r)%nt, last_ops)
            call append_table(first_nonterminals_table, backus_naur_form%rules(r)%nt, first_nonterminals)
            call append_table(last_nonterminals_table, backus_naur_form%rules(r)%nt, last_nonterminals)
        end do

        ! Compute all first-ops by propagating the initial ones we have
        ! now, according to first-nts.
        call propagate(first_nonterminals_table, first_ops_table)
        ! Same thing for last-ops.
        call propagate(last_nonterminals_table, last_ops_table)

        ! Now generate the 2D precedence table.
        do r = 1, nrules
            do h = 1, size(backus_naur_form%rules(r)%rhs)
                associate (rhs => backus_naur_form%rules(r)%rhs(h)%items)
                    do i = 1, size(rhs) - 1
                        if (has_string(rhs(i)%text, nonterminals)) then
                            k = table_index(last_ops_table, rhs(i)%text)

                            do j = 1, size(last_ops_table(k)%ops)
                                call simple_minded_indentation_engine_set_prec2tab(prec2, last_ops_table(k)%ops(j)%text, &
                                                                     rhs(i + 1)%text, SIMPLE_MINDED_INDENTATION_ENGINE_GT, override)
                            end do
                        else if (has_string(rhs(i + 1)%text, nonterminals)) then
                            k = table_index(first_ops_table, rhs(i + 1)%text)

                            do j = 1, size(first_ops_table(k)%ops)
                                call simple_minded_indentation_engine_set_prec2tab(prec2, rhs(i)%text, &
                                                      first_ops_table(k)%ops(j)%text, SIMPLE_MINDED_INDENTATION_ENGINE_LT, override)
                            end do

                            if (i + 2 <= size(rhs)) then
                                if (.not. has_string(rhs(i + 2)%text, nonterminals)) then
                                    call simple_minded_indentation_engine_set_prec2tab(prec2, rhs(i)%text, rhs(i + 2)%text, &
                                                                                      SIMPLE_MINDED_INDENTATION_ENGINE_EQ, override)
                                end if
                            end if
                        else
                            call simple_minded_indentation_engine_set_prec2tab(prec2, rhs(i)%text, rhs(i + 1)%text, &
                                                                               SIMPLE_MINDED_INDENTATION_ENGINE_EQ, override)
                        end if
                    end do
                end associate
            end do
        end do

        ! Keep track of which tokens are openers/closer, so they can get a nil
        ! precedence in smie-prec2->grammar.
        call simple_minded_indentation_engine_backus_naur_form_classify(backus_naur_form, ops, prec2%classes)
        call move_alloc(ops, prec2%class_tokens)
        prec2%has_open_close = .true.
        call simple_minded_indentation_engine_backus_naur_form_closer_table(backus_naur_form, prec2%openers, prec2%closers)
        prec2%has_closer_table = .true.
        ! (The total of warnings is not reported: display-warning is
        ! deferred.)
        warning_count = saved_count

    contains

        subroutine append_table(table, nt, list)
            type(nt_ops_type), allocatable, intent(inout) :: table(:)
            character(len=*), intent(in) :: nt
            type(string_type), allocatable, intent(inout) :: list(:)
            type(nt_ops_type) :: entry

            entry%nt = nt
            call move_alloc(list, entry%ops)
            table = [entry, table]
        end subroutine append_table
    end subroutine simple_minded_indentation_engine_backus_naur_form_to_prec2

    ! (assoc NT TABLE): the first entry for NT.  Not a port.
    integer function table_index(table, nt) result(k)
        type(nt_ops_type), intent(in) :: table(:)
        character(len=*), intent(in) :: nt

        do k = 1, size(table)
            if (string_eq(table(k)%nt, nt)) return
        end do

        k = 0
    end function table_index

    ! Propagate the ops of OPS_TABLE along the nonterminals of NONTERMINALS_TABLE
    ! until nothing changes.  Port of part of smie.el:smie-bnf->prec2.
    subroutine propagate(nonterminals_table, ops_table)
        type(nt_ops_type), intent(in) :: nonterminals_table(:)
        type(nt_ops_type), intent(inout) :: ops_table(:)
        type(string_type), allocatable :: snapshot(:), cell(:)
        logical :: again
        integer :: t, i, j, k, source

        again = .true.

        do while (again)
            again = .false.

            do t = 1, size(nonterminals_table)
                k = table_index(ops_table, nonterminals_table(t)%nt)

                do i = 1, size(nonterminals_table(t)%ops)
                    source = table_index(ops_table, nonterminals_table(t)%ops(i)%text)
                    if (source == 0) cycle
                    snapshot = ops_table(source)%ops

                    do j = 1, size(snapshot)
                        ! (member op first-ops), where first-ops is (NT . OPS).
                        allocate (cell(size(ops_table(k)%ops) + 1))
                        cell(1)%text = ops_table(k)%nt
                        cell(2:) = ops_table(k)%ops

                        if (.not. has_string(snapshot(j)%text, cell)) then
                            again = .true.
                            call prepend_string(ops_table(k)%ops, snapshot(j)%text)
                        end if

                        deallocate (cell)
                    end do
                end do
            end do
        end do
    end subroutine propagate

    !> Build a closer-alist from a BACKUS_NAUR_FORM table: OPENER and CLOSER of each entry, in
    !> OPENERS and CLOSERS.  The return value is in the same form as
    !> `simple_minded_indentation_engine_closers'.  NO_INNERS true means that inner keywords will
    !> be excluded from the table, e.g. the table will not include things
    !> like ("if" . "else").  Port of smie.el:smie-bnf--closer-alist.
    subroutine simple_minded_indentation_engine_backus_naur_form_closer_table(backus_naur_form, openers, closers, no_inners)
        type(simple_minded_indentation_engine_backus_naur_form_type), intent(in) :: backus_naur_form
        type(string_type), allocatable, intent(out) :: openers(:), closers(:)
        logical, intent(in), optional :: no_inners
        type(string_type), allocatable :: nonterminals(:)
        integer :: r, h, i, n
        logical :: inners

        inners = .true.
        if (present(no_inners)) inners = .not. no_inners
        allocate (openers(0), closers(0), nonterminals(size(backus_naur_form%rules)))

        do r = 1, size(backus_naur_form%rules)
            nonterminals(r)%text = backus_naur_form%rules(r)%nt
        end do

        do r = 1, size(backus_naur_form%rules)
            do h = 1, size(backus_naur_form%rules(r)%rhs)
                associate (rhs => backus_naur_form%rules(r)%rhs(h)%items)
                    n = size(rhs)
                    if (n < 2) cycle
                    if (has_string(rhs(1)%text, nonterminals)) cycle

                    if (.not. inners) then
                        if (.not. has_string(rhs(n)%text, nonterminals)) call append_pair(rhs(1)%text, rhs(n)%text)
                    else
                        ! Reverse so that the "real" closer gets there first,
                        ! which is important for smie-close-block.
                        do i = n, 2, -1
                            if (.not. has_string(rhs(i)%text, nonterminals)) call append_pair(rhs(1)%text, rhs(i)%text)
                        end do
                    end if
                end associate
            end do
        end do

    contains

        ! cl-pushnew, then nreverse: pairs in the order first added.
        subroutine append_pair(a, b)
            character(len=*), intent(in) :: a, b
            type(string_type), allocatable :: go(:), gc(:)
            integer :: m

            do m = 1, size(openers)
                if (string_eq(openers(m)%text, a) .and. string_eq(closers(m)%text, b)) return
            end do

            allocate (go(size(openers) + 1), gc(size(closers) + 1))

            do m = 1, size(openers)
                call move_alloc(openers(m)%text, go(m)%text)
                call move_alloc(closers(m)%text, gc(m)%text)
            end do

            go(size(go))%text = a
            gc(size(gc))%text = b
            call move_alloc(go, openers)
            call move_alloc(gc, closers)
        end subroutine append_pair
    end subroutine simple_minded_indentation_engine_backus_naur_form_closer_table

    !> Return a table classifying terminals: each terminal of TOKENS can
    !> either be an opener (SIMPLE_MINDED_INDENTATION_ENGINE_OPENER) or a closer (SIMPLE_MINDED_INDENTATION_ENGINE_CLOSER); the
    !> others are left out.  Port of smie.el:smie-bnf--classify.
    subroutine simple_minded_indentation_engine_backus_naur_form_classify(backus_naur_form, tokens, classes)
        type(simple_minded_indentation_engine_backus_naur_form_type), intent(in) :: backus_naur_form
        type(string_type), allocatable, intent(out) :: tokens(:)
        integer, allocatable, intent(out) :: classes(:)
        type(string_type), allocatable :: table(:)
        integer, allocatable :: values(:)
        integer :: r, h, i, n

        allocate (table(0), values(0))

        ! Mark non-terminals.
        do r = 1, size(backus_naur_form%rules)
            call set_class(backus_naur_form%rules(r)%nt, SIMPLE_MINDED_INDENTATION_ENGINE_NONTERMINAL, .true.)
        end do

        do r = 1, size(backus_naur_form%rules)
            do h = 1, size(backus_naur_form%rules(r)%rhs)
                associate (rhs => backus_naur_form%rules(r)%rhs(h)%items)
                    n = size(rhs)

                    if (n == 1) then
                        call set_class(rhs(1)%text, SIMPLE_MINDED_INDENTATION_ENGINE_NEITHER, .false.)
                    else
                        call set_class(rhs(1)%text, SIMPLE_MINDED_INDENTATION_ENGINE_OPENER, .false.)

                        ! Remove internals.
                        do i = 2, n - 1
                            call set_class(rhs(i)%text, SIMPLE_MINDED_INDENTATION_ENGINE_NEITHER, .false.)
                        end do

                        call set_class(rhs(n)%text, SIMPLE_MINDED_INDENTATION_ENGINE_CLOSER, .false.)
                    end if
                end associate
            end do
        end do

        allocate (tokens(0), classes(0))

        do i = 1, size(table)
            if (values(i) == SIMPLE_MINDED_INDENTATION_ENGINE_CLOSER .or. values(i) == SIMPLE_MINDED_INDENTATION_ENGINE_OPENER) then
                call prepend_string(tokens, table(i)%text)
                classes = [values(i), classes]
            end if
        end do

    contains

        ! smie-bnf--set-class (or the puthash marking a nonterminal).
        subroutine set_class(token, class, force)
            character(len=*), intent(in) :: token
            integer, intent(in) :: class
            logical, intent(in) :: force
            integer :: m, previous

            do m = 1, size(table)
                if (string_eq(table(m)%text, token)) then
                    previous = values(m)

                    if (force) then
                        values(m) = class
                    else if (previous == class) then
                        continue
                    else if (previous == SIMPLE_MINDED_INDENTATION_ENGINE_NONTERMINAL) then
                        ! Non-terminal.
                        continue
                    else
                        ! (The warning "token TOKEN is both CLASS and PREVIOUS" is
                        ! deferred.)
                        values(m) = SIMPLE_MINDED_INDENTATION_ENGINE_NEITHER
                    end if

                    return
                end if
            end do

            call append_string(table, token)
            values = [values, class]
        end subroutine set_class
    end subroutine simple_minded_indentation_engine_backus_naur_form_classify

    !> Take a 2D precedence table and turn it into a table of precedence levels.
    !> PREC2 is a table as returned by `simple_minded_indentation_engine_precs_to_prec2' or
    !> `simple_minded_indentation_engine_backus_naur_form_to_prec2'.  Port of smie.el:smie-prec2->grammar.
    subroutine simple_minded_indentation_engine_prec2_to_grammar(prec2, grammar, status)
        type(simple_minded_indentation_engine_prec2_type), intent(in) :: prec2
        type(simple_minded_indentation_engine_grammar_type), intent(out) :: grammar
        type(status_type), intent(out) :: status
        ! The table, in creation order (upstream pushes, so its order is
        ! the reverse): each token's left and right "variables" (cells).
        type(string_type), allocatable :: tokens(:)
        integer, allocatable :: lcell(:), rcell(:)
        ! The cells' values (`has' false is nil).
        integer, allocatable :: value_text(:)
        logical, allocatable :: has(:)
        ! Equality and < constraints, in push order (upstream's lists are
        ! the reverse).
        integer, allocatable :: eqx(:), eqy(:), cx(:), cy(:)
        logical, allocatable :: alive(:)
        integer :: ntok, ncell, k, a, b, x, y, i, e, j, m, c
        logical :: progress, in_rhs
        character(len=:), allocatable :: description

        status = status_type()
        allocate (tokens(0), lcell(0), rcell(0), value_text(0), has(0), eqx(0), eqy(0), cx(0), cy(0))
        ntok = 0
        ncell = 0

        ! From `prec2' we construct a list of constraints between
        ! variables (aka "precedence levels").  These can be either
        ! equality constraints (in `eqs') or `<' constraints (in `csts').
        do k = 1, prec2%n
            a = token_index(prec2%x(k)%text)
            if (a == 0) a = make_token(prec2%x(k)%text)
            x = rcell(a)
            b = token_index(prec2%y(k)%text)
            if (b == 0) b = make_token(prec2%y(k)%text)
            y = lcell(b)

            select case (prec2%relative(k))
            case (SIMPLE_MINDED_INDENTATION_ENGINE_EQ)
                eqx = [eqx, x]
                eqy = [eqy, y]
            case (SIMPLE_MINDED_INDENTATION_ENGINE_LT)
                cx = [cx, x]
                cy = [cy, y]
            case (SIMPLE_MINDED_INDENTATION_ENGINE_GT)
                cx = [cx, y]
                cy = [cy, x]
            case default
                call set_error(status, GENERIC_ERROR_CODE, "SMIE error: prec2 has an unknown relation")
                return
            end select
        end do

        ! Upstream's lists start with the last constraint pushed.
        eqx = eqx(size(eqx):1:-1)
        eqy = eqy(size(eqy):1:-1)
        cx = cx(size(cx):1:-1)
        cy = cy(size(cy):1:-1)

        ! First process the equality constraints.
        do e = 1, size(eqx)
            if (eqy(e) == eqx(e)) cycle

            do j = e + 1, size(eqx)
                if (eqy(j) == eqx(e)) eqy(j) = eqy(e)
                ! This can happen because of `assoc' settings in precs
                ! or because of a rhs like ("op" foo "op").
                if (eqx(j) == eqx(e)) eqx(j) = eqy(e)
            end do

            do j = 1, size(cx)
                if (cy(j) == eqx(e)) cy(j) = eqy(e)
                if (cx(j) == eqx(e)) cx(j) = eqy(e)
            end do
        end do

        ! Then eliminate trivial constraints iteratively.
        i = 0
        allocate (alive(size(cx)))
        alive = .true.

        do while (any(alive))
            progress = .false.

            ! The right-hand variables when this pass starts.
            block
                integer, allocatable :: rhvs(:)
                rhvs = pack(cy, alive)

                do c = 1, size(cx)
                    if (.not. alive(c)) cycle
                    in_rhs = any(rhvs == cx(c))

                    if (.not. in_rhs) then
                        progress = .true.

                        ! We could give each var in a given iteration the same value,
                        ! but we can also give them arbitrarily different values.
                        ! Basically, these are vars between which there is no
                        ! constraint (neither equality nor inequality), so
                        ! anything will do.
                        ! We give them arbitrary values, which means that we
                        ! replace the "no constraint" case with either > or <
                        ! but not =.  The reason we do that is so as to try and
                        ! distinguish associative operators (which will have
                        ! left = right).
                        if (.not. has(cx(c))) then
                            value_text(cx(c)) = i
                            has(cx(c)) = .true.
                            i = i + 1
                        end if

                        alive(c) = .false.
                    end if
                end do
            end block

            if (.not. progress) then
                description = describe_cycle()
                call set_error(status, GENERIC_ERROR_CODE, "Can't resolve the precedence cycle: "//description)
                return
            end if

            i = i + 10
        end do

        ! Propagate equality constraints back to their sources.
        do e = size(eqx), 1, -1
            if (.not. has(eqy(e))) then
                ! There's an equality constraint, but we still haven't given
                ! it a value: that means it binds tighter than anything else,
                ! and it can't be an opener/closer (those don't have equality
                ! constraints).
                ! So set it here rather than below since doing it below
                ! makes it more difficult to obey the equality constraints.
                value_text(eqy(e)) = i
                has(eqy(e)) = .true.
                i = i + 1
            end if

            value_text(eqx(e)) = value_text(eqy(e))
            has(eqx(e)) = .true.
        end do

        ! Finally, fill in the remaining vars (which did not appear on the
        ! left side of any < constraint).
        do m = ntok, 1, -1
            if (.not. has(lcell(m))) then
                value_text(lcell(m)) = i
                has(lcell(m)) = .true.
                i = i + 1
            end if

            if (.not. has(rcell(m))) then
                value_text(rcell(m)) = i
                has(rcell(m)) = .true.
                i = i + 1
            end if
        end do

        ! The grammar, in upstream's table order.
        grammar%unset = .false.
        allocate (grammar%tokens(ntok), grammar%left(ntok), grammar%right(ntok))

        do m = 1, ntok
            k = ntok + 1 - m
            grammar%tokens(m)%text = tokens(k)%text
            grammar%left(m) = simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, &
                                                                          value=value_text(lcell(k)))
            grammar%right(m) = simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, &
                                                                           value=value_text(rcell(k)))
        end do

        ! Mark closers and openers.
        if (prec2%has_open_close) then
            do k = 1, size(prec2%classes)
                m = simple_minded_indentation_engine_grammar_index(grammar, prec2%class_tokens(k)%text)
                ! `cons' can be nil for openers/closers which only contain
                ! "atomic" elements.
                if (m == 0) cycle

                select case (prec2%classes(k))
                case (SIMPLE_MINDED_INDENTATION_ENGINE_CLOSER)
                    call make_list_level(grammar%right(m))
                case (SIMPLE_MINDED_INDENTATION_ENGINE_OPENER)
                    call make_list_level(grammar%left(m))
                case default
                    continue
                end select
            end do
        end if

        if (prec2%has_closer_table) then
            if (size(prec2%openers) > 0) then
                grammar%has_closer_table = .true.
                grammar%openers = prec2%openers
                grammar%closers = prec2%closers
            end if
        end if

    contains

        integer function token_index(token) result(t)
            character(len=*), intent(in) :: token

            do t = 1, ntok
                if (string_eq(tokens(t)%text, token)) return
            end do

            t = 0
        end function token_index

        integer function make_token(token) result(t)
            character(len=*), intent(in) :: token
            type(string_type) :: string

            string%text = token
            tokens = [tokens(1:ntok), string]
            ntok = ntok + 1
            t = ntok
            lcell = [lcell, ncell + 1]
            rcell = [rcell, ncell + 2]
            ncell = ncell + 2
            value_text = [value_text, 0, 0]
            has = [has, .false., .false.]
        end function make_token

        ! smie-debug--describe-cycle of smie-debug--prec2-cycle: the
        ! tokens of each variable of the cycle (".TOKEN" for a left level,
        ! "TOKEN." for a right one) joined with "=", the cycle with " < ".
        function describe_cycle() result(text)
            character(len=:), allocatable :: text
            integer, allocatable :: found(:)
            type(string_type), allocatable :: names(:), res(:)
            integer :: q, s, r

            call find_cycle(found)
            allocate (names(size(found)))

            do q = 1, size(found)
                allocate (res(0))

                do s = ntok, 1, -1
                    if (lcell(s) == found(q)) call prepend_string(res, "."//tokens(s)%text)
                    if (rcell(s) == found(q)) call prepend_string(res, tokens(s)%text//".")
                end do

                names(q)%text = ""

                do r = 1, size(res)
                    if (r > 1) names(q)%text = names(q)%text//"="
                    names(q)%text = names(q)%text//res(r)%text
                end do

                deallocate (res)
            end do

            text = ""

            do q = 1, size(names) + 1
                if (q > 1) text = text//" < "
                text = text//names(mod(q - 1, size(names)) + 1)%text
            end do
        end function describe_cycle

        ! smie-debug--prec2-cycle: a cycle in the remaining constraints.
        subroutine find_cycle(cycle_nodes)
            integer, allocatable, intent(out) :: cycle_nodes(:)
            type(path_type), allocatable :: paths(:), next(:)
            integer :: p, q
            logical :: found

            allocate (paths(0))

            do q = 1, size(cx)
                if (alive(q)) call append_path(paths, [cx(q), cy(q)], .false.)
            end do

            found = .false.

            do while (.not. found)
                if (size(paths) == 0) then
                    ! (No cycle: cannot happen without progress.)
                    cycle_nodes = [cx(1)]
                    return
                end if

                allocate (next(0))

                do p = 1, size(paths)
                    do q = 1, size(cx)
                        if (.not. alive(q)) cycle
                        if (cx(q) /= paths(p)%nodes(2)) cycle

                        if (cy(q) == paths(p)%nodes(1)) then
                            ! (cons (car cycle) (nreverse (cdr cycle))); the
                            ! last one found wins.
                            cycle_nodes = [paths(p)%nodes(1), paths(p)%nodes(size(paths(p)%nodes):2:-1)]
                            found = .true.
                        else
                            call append_path(next, [paths(p)%nodes(1), cy(q), paths(p)%nodes(2:)], .true.)
                        end if
                    end do
                end do

                call move_alloc(next, paths)
            end do
        end subroutine find_cycle

        ! Add NODES to LIST, at the front with FRONT (push) or at the end.
        subroutine append_path(list, nodes, front)
            type(path_type), allocatable, intent(inout) :: list(:)
            integer, intent(in) :: nodes(:)
            logical, intent(in) :: front
            type(path_type), allocatable :: grown(:)
            integer :: q, o

            allocate (grown(size(list) + 1))
            o = 0
            if (front) o = 1

            do q = 1, size(list)
                call move_alloc(list(q)%nodes, grown(q + o)%nodes)
            end do

            if (front) then
                grown(1)%nodes = nodes
            else
                grown(size(grown))%nodes = nodes
            end if

            call move_alloc(grown, list)
        end subroutine append_path
    end subroutine simple_minded_indentation_engine_prec2_to_grammar

    ! (setf (car cons) (list (car cons))): LEVEL becomes a list level.
    ! Not a port.
    subroutine make_list_level(level)
        type(simple_minded_indentation_engine_level_type), intent(inout) :: level

        level_ids = level_ids + 1
        level%kind = SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST
        level%id = level_ids
    end subroutine make_list_level

    !> The index of TOKEN in GRAMMAR (assoc), 0 if none.  Not a port.
    integer function simple_minded_indentation_engine_grammar_index(grammar, token) result(k)
        type(simple_minded_indentation_engine_grammar_type), intent(in) :: grammar
        character(len=*), intent(in) :: token

        k = 0
        if (.not. allocated(grammar%tokens)) return

        do k = 1, size(grammar%tokens)
            if (string_eq(grammar%tokens(k)%text, token)) return
        end do

        k = 0
    end function simple_minded_indentation_engine_grammar_index

    ! ------------------------------------------------------------------
    ! Levels
    ! ------------------------------------------------------------------

    ! (eq A B) of two levels.  Not a port.
    pure logical function level_eq(a, b)
        type(simple_minded_indentation_engine_level_type), intent(in) :: a, b

        level_eq = a%kind == b%kind
        if (.not. level_eq) return

        select case (a%kind)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER)
            level_eq = a%value == b%value
        case (SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST)
            level_eq = a%id == b%id
        case default
            continue
        end select
    end function level_eq

    ! (or LEVEL t).  Not a port.
    pure function level_or_t(level) result(l)
        type(simple_minded_indentation_engine_level_type), intent(in) :: level
        type(simple_minded_indentation_engine_level_type) :: l

        l = level
        if (l%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE) l%kind = SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS
    end function level_or_t

    ! ------------------------------------------------------------------
    ! Buffer-local values
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
        type(simple_minded_indentation_engine_local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, entry]
        locals(k) = simple_minded_indentation_engine_local_type(serial=buffers(current)%serial)
    end subroutine make_local_entry

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine clear_simple_minded_indentation_engine_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = simple_minded_indentation_engine_local_type()
        k = simple_minded_indentation_engine_indent_functions%local_index()
        if (k > 0) simple_minded_indentation_engine_indent_functions%locals(k)%serial = 0
        call simple_minded_indentation_engine_hanging_line_test_function%clear_local()
    end subroutine clear_simple_minded_indentation_engine_locals

    ! The current buffer's `simple_minded_indentation_engine_grammar'.  Not a port.
    function grammar() result(g)
        type(simple_minded_indentation_engine_grammar_type), pointer :: g
        integer :: k

        g => simple_minded_indentation_engine_grammar
        k = local_index()

        if (k > 0) then
            if (associated(locals(k)%grammar)) g => locals(k)%grammar
        end if
    end function grammar

    ! Whether the current buffer has a `simple_minded_indentation_engine_closers'.  Not a port.
    logical function is_closer_table()
        integer :: k
        type(simple_minded_indentation_engine_grammar_type), pointer :: g

        is_closer_table = .false.
        k = local_index()
        if (k == 0) return
        if (.not. locals(k)%has_closer_table) return
        g => grammar()
        is_closer_table = g%has_closer_table
    end function is_closer_table

    !> Whether TOKEN is a closer of `simple_minded_indentation_engine_closers' (rassoc).
    !> Not a port.
    logical function is_simple_minded_indentation_engine_closer(token)
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_grammar_type), pointer :: g

        is_simple_minded_indentation_engine_closer = .false.
        if (.not. is_closer_table()) return
        g => grammar()
        is_simple_minded_indentation_engine_closer = has_string(token, g%closers)
    end function is_simple_minded_indentation_engine_closer

    !> Whether TOKEN is an opener of `simple_minded_indentation_engine_closers' (assoc).
    !> Not a port.
    logical function is_simple_minded_indentation_engine_opener(token)
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_grammar_type), pointer :: g

        is_simple_minded_indentation_engine_opener = .false.
        if (.not. is_closer_table()) return
        g => grammar()
        is_simple_minded_indentation_engine_opener = has_string(token, g%openers)
    end function is_simple_minded_indentation_engine_opener

    ! Whether (OPENER . CLOSER) is in `simple_minded_indentation_engine_closers'.  Not a port.
    logical function is_closer_pair(opener, closer)
        character(len=*), intent(in) :: opener, closer
        type(simple_minded_indentation_engine_grammar_type), pointer :: g
        integer :: i

        is_closer_pair = .false.
        if (.not. is_closer_table()) return
        g => grammar()

        do i = 1, size(g%openers)
            if (string_eq(g%openers(i)%text, opener) .and. string_eq(g%closers(i)%text, closer)) then
                is_closer_pair = .true.
                return
            end if
        end do
    end function is_closer_pair

    !> TOKEN's levels in the current buffer's grammar (assoc TOKEN
    !> smie-grammar); `null' if it has none.  Not a port.
    function simple_minded_indentation_engine_grammar_token(token) result(t)
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_toklevels_type) :: t
        type(simple_minded_indentation_engine_grammar_type), pointer :: g
        integer :: k

        g => grammar()
        k = simple_minded_indentation_engine_grammar_index(g, token)
        if (k == 0) return
        t%null = .false.
        t%token = token
        t%left = g%left(k)
        t%right = g%right(k)
    end function simple_minded_indentation_engine_grammar_token

    !> Move over the next token with the current buffer's
    !> `simple_minded_indentation_engine_forward_token_function'.  Not a port.
    recursive subroutine simple_minded_indentation_engine_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        procedure(simple_minded_indentation_token_procedure), pointer :: fn
        integer :: k

        fn => simple_minded_indentation_engine_forward_token_function
        k = local_index()

        if (k > 0) then
            if (associated(locals(k)%forward_token)) fn => locals(k)%forward_token
        end if

        if (associated(fn)) then
            call fn(token, status)
        else
            call simple_minded_indentation_engine_default_forward_token(token, status)
        end if
    end subroutine simple_minded_indentation_engine_forward_token

    !> Move over the previous token with the current buffer's
    !> `simple_minded_indentation_engine_backward_token_function'.  Not a port.
    recursive subroutine simple_minded_indentation_engine_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        procedure(simple_minded_indentation_token_procedure), pointer :: fn
        integer :: k

        fn => simple_minded_indentation_engine_backward_token_function
        k = local_index()

        if (k > 0) then
            if (associated(locals(k)%backward_token)) fn => locals(k)%backward_token
        end if

        if (associated(fn)) then
            call fn(token, status)
        else
            call simple_minded_indentation_engine_default_backward_token(token, status)
        end if
    end subroutine simple_minded_indentation_engine_backward_token

    ! (funcall smie-rules-function METHOD ARGUMENT).  Not a port.
    recursive subroutine execute_rules(method, argument, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: argument
        type(simple_minded_indentation_engine_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(simple_minded_indentation_engine_rules_procedure), pointer :: fn
        integer :: k

        status = status_type()
        fn => simple_minded_indentation_engine_rules_function
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_rules) fn => locals(k)%rules
        end if

        if (associated(fn)) call fn(method, argument, result, status)
        if (status%ok()) call configuration_advice(method, argument, result)
    end subroutine execute_rules

    ! The current buffer's `simple_minded_indentation_engine_blink_matching_triggers'.  Not a port.
    function triggers() result(t)
        character(len=:), allocatable :: t
        integer :: k

        t = simple_minded_indentation_engine_blink_matching_triggers
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_triggers) t = locals(k)%triggers
        end if
    end function triggers

    ! ------------------------------------------------------------------
    ! Parsing using a precedence level table
    ! ------------------------------------------------------------------

    ! (buffer-substring-no-properties A B) (either order).  Not a port.
    function substring(a, b) result(text)
        integer(int64), intent(in) :: a, b
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring(a, b, text, st)
        if (.not. allocated(text)) text = ""
    end function substring

    !> Default value of `simple_minded_indentation_engine_backward_token_function'.  Port of
    !> smie.el:smie-default-backward-token.
    recursive subroutine simple_minded_indentation_engine_default_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: ok

        status = status_type()
        ok = forward_comment(-point())
        p = point()

        if (skip_syntax_backward(".") == 0) then
            if (skip_syntax_backward("w_'") == 0) continue
        end if

        token = substring(p, point())
    end subroutine simple_minded_indentation_engine_default_backward_token

    !> Default value of `simple_minded_indentation_engine_forward_token_function'.  Port of
    !> smie.el:smie-default-forward-token.
    recursive subroutine simple_minded_indentation_engine_default_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: ok

        status = status_type()
        ok = forward_comment(point_maximum())
        p = point()

        if (skip_syntax_forward(".") == 0) then
            if (skip_syntax_forward("w_'") == 0) continue
        end if

        token = substring(p, point())
    end subroutine simple_minded_indentation_engine_default_forward_token

    ! smie--associative-p: in "a + b + c" we want to stop at each +, but in
    ! "if a then b elsif c then d else c" we don't want to stop at each keyword.
    ! To distinguish the two cases, we made smie-prec2->grammar choose
    ! different levels for each part of "if a then b else c", so that
    ! by checking if the left-level is equal to the right level, we can
    ! figure out that it's an associative operator.  Port of
    ! smie.el:smie--associative-p.
    pure logical function is_associative(left, right)
        type(simple_minded_indentation_engine_level_type), intent(in) :: left, right

        is_associative = level_eq(left, right)
    end function is_associative

    ! Signal `scan_error' with MESSAGE, A and B.  Not a port.
    subroutine signal_scan_error(status, message, a, b)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: message
        integer(int64), intent(in) :: a, b
        character(len=24) :: sa, sb

        write (sa, "(i0)") a
        write (sb, "(i0)") b
        call set_error(status, SCAN_ERROR_CODE, fortran_string(message)//", "//trim(sa)//", "//trim(sb))
    end subroutine signal_scan_error

    !> Skip over one expression, forward (FORWARD) or backward.
    !> The next token comes from the token function, the lower-level
    !> expression is skipped with `forward_expression' or `backward_expression', and the
    !> forward and backward levels are the right and left ones forward, the
    !> left and right ones backward.
    !> HALFEXPRESSION if true, means skip over a partial expression if needed.  I.e. if the
    !> first token we see is an operator, skip over its left-hand-side argument.
    !> HALFEXPRESSION can also be a token (TOKEN), in which case it means to parse as if
    !> we had just successfully passed this token.
    !> Possible results in RES:
    !>   LEVEL a number, POSITION and TOKEN: we couldn't skip TOKEN because its back-level
    !>     is too high.  FORW-LEVEL is the forw-level of TOKEN,
    !>     POSITION is its start position in the buffer.
    !>   LEVEL SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS, POSITION and TOKEN: same thing when we bump on the wrong side of
    !> a paren.
    !>     Instead of SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS, the LEVEL can also be
    !> SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST.
    !>   LEVEL SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE, POSITION and TOKEN: we skipped over a paren-like pair.
    !>   `null' .true.: we skipped over an identifier, matched parentheses, ...
    !> Port of smie.el:smie-next-sexp.
    recursive subroutine simple_minded_indentation_engine_next_expression(forward, res, status, halfexpression, token)
        logical, intent(in) :: forward
        type(simple_minded_indentation_engine_expression_type), intent(out) :: res
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: halfexpression
        character(len=*), intent(in), optional :: token
        type(simple_minded_indentation_engine_level_type), allocatable :: lfw(:), lbk(:), last_fw(:), last_bk(:)
        type(simple_minded_indentation_engine_toklevels_type) :: toklevels
        type(simple_minded_indentation_engine_level_type) :: forw, back
        character(len=:), allocatable :: token_text, message
        integer(int64) :: position, epos1, epos, beginning, end_position
        integer :: n
        logical :: half, index

        status = status_type()
        half = .false.
        if (present(halfexpression)) half = halfexpression
        allocate (lfw(0), lbk(0))

        if (present(token)) then
            toklevels = simple_minded_indentation_engine_grammar_token(token)

            if (.not. toklevels%null) then
                call append_levels(toklevels%left, toklevels%right)
            else
                index = string_match("\`\s(\|\s)\(\)\'", token, status, inhibit_modify=.false.) > 0
                if (.not. status%ok()) return

                if (index) then
                    if (match_end(1, status) > 0) then
                        call append_levels(simple_minded_indentation_engine_level_type( &
                                           kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, value=0), &
                                           simple_minded_indentation_engine_level_type())
                    else
                        call append_levels(simple_minded_indentation_engine_level_type(), &
                                   simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, &
                                                                                       value=0))
                    end if
                else
                    call set_error(status, GENERIC_ERROR_CODE, "Unknown token: "//quoted_text(token))
                    return
                end if
            end if

            half = .false.
        end if

        do
            position = point()

            if (forward) then
                call simple_minded_indentation_engine_forward_token(token_text, status)
            else
                call simple_minded_indentation_engine_backward_token(token_text, status)
            end if

            if (.not. status%ok()) return
            if (.not. allocated(token_text)) token_text = ""
            toklevels = simple_minded_indentation_engine_grammar_token(token_text)

            if (toklevels%null) then
                if (len(token_text) == 0) then
                    call next_expression(status)

                    if (status%code == SCAN_ERROR_CODE) then
                        call scan_error_data(status, message, beginning, end_position)
                        status = status_type()
                        epos1 = beginning

                        if (point() <= epos1) then
                            epos = end_position
                        else
                            epos = epos1
                        end if

                        call goto_character(position)
                        res%null = .false.
                        res%level%kind = SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS
                        res%position = epos

                        if (point() /= epos) then
                            if (point() < epos) then
                                res%token = substring(epos, epos - 1)
                            else
                                res%token = substring(epos, epos + 1)
                            end if
                        end if

                        return
                    end if

                    if (.not. status%ok()) return

                    if (position == point()) then
                        ! We did not move, so let's abort the loop.
                        res%null = .false.
                        res%level%kind = SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS
                        res%position = point()
                        return
                    end if
                end if
            else
                call forw_back(toklevels, forw, back)

                if (back%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                    ! A token like a paren-close.
                    call append_levels(toklevels%left, toklevels%right)
                else
                    do while (size(lfw) > 0)
                        if (.not. top_forw_number()) exit
                        if (.not. back%value < top_forw()) exit
                        call pop_levels()
                    end do

                    if (size(lfw) == 0) then
                        if (half .and. forw%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                            call append_levels(toklevels%left, toklevels%right)
                        else
                            call bump(forw, token_text, position)
                            return
                        end if
                    else
                        last_fw = lfw
                        last_bk = lbk

                        if (top_forw_number()) then
                            if (back%value == top_forw()) call pop_levels()
                        end if

                        ! We may have found a match for the previously pending
                        ! operator.  Is this the end?
                        if (size(lfw) > 0) then
                            ! Keep looking as long as we haven't matched the
                            ! topmost operator.
                            if (forw%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) call append_levels(toklevels%left, &
                                                                                                               toklevels%right)
                        else if (forw%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                            ! We matched the topmost operator.  If the new operator
                            ! is the last in the corresponding BACKUS_NAUR_FORM rule, we're done.
                            ! It is the last element, let's stop here.
                            res%null = .false.
                            res%level = simple_minded_indentation_engine_level_type()
                            res%position = point()
                            res%token = token_text
                            return
                        else if (.not. is_associative(toklevels%left, toklevels%right)) then
                            ! If the new operator is not the last in the BACKUS_NAUR_FORM rule,
                            ! and is not associative, it's one of the inner operators
                            ! (like the "in" in "let .. in .. end"), so keep looking.
                            call append_levels(toklevels%left, toklevels%right)
                        else if (associative_last()) then
                            ! The new operator is associative.  Two cases:
                            ! - it's really just an associative operator (like + or ;)
                            !   in which case we should have stopped right before.
                            call bump(forw, token_text, position)
                            return
                        else
                            ! - it's an associative operator within a larger construct
                            !   (e.g. an "elsif"), so we should just ignore it and keep
                            !   looking for the closing element.
                            lfw = last_fw
                            lbk = last_bk
                        end if
                    end if
                end if
            end if

            if (size(lfw) == 0) exit
            half = .false.
        end do

    contains

        ! The forward and backward levels of T.
        recursive subroutine forw_back(t, f, b)
            type(simple_minded_indentation_engine_toklevels_type), intent(in) :: t
            type(simple_minded_indentation_engine_level_type), intent(out) :: f, b

            if (forward) then
                f = t%right
                b = t%left
            else
                f = t%left
                b = t%right
            end if
        end subroutine forw_back

        ! (push (LEFT RIGHT) levels).
        recursive subroutine append_levels(left, right)
            type(simple_minded_indentation_engine_level_type), intent(in) :: left, right

            if (forward) then
                lfw = [lfw, right]
                lbk = [lbk, left]
            else
                lfw = [lfw, left]
                lbk = [lbk, right]
            end if
        end subroutine append_levels

        recursive subroutine pop_levels()
            n = size(lfw)
            lfw = lfw(1:n - 1)
            lbk = lbk(1:n - 1)
        end subroutine pop_levels

        logical function top_forw_number()
            top_forw_number = lfw(size(lfw))%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER
        end function top_forw_number

        integer function top_forw()
            top_forw = lfw(size(lfw))%value
        end function top_forw

        ! Whether the top of lastlevels is associative.
        logical function associative_last()
            integer :: m

            m = size(last_fw)
            associative_last = .false.
            if (m == 0) return

            if (forward) then
                associative_last = is_associative(last_bk(m), last_fw(m))
            else
                associative_last = is_associative(last_fw(m), last_bk(m))
            end if
        end function associative_last

        ! (prog1 (list (or FORW t) (point) TOKEN) (goto-char POSITION)).
        recursive subroutine bump(f, t, p)
            type(simple_minded_indentation_engine_level_type), intent(in) :: f
            character(len=*), intent(in) :: t
            integer(int64), intent(in) :: p

            res%null = .false.
            res%level = level_or_t(f)
            res%position = point()
            res%token = t
            call goto_character(p)
        end subroutine bump

        ! NEXT-SEXP: skip one expression.
        recursive subroutine next_expression(st)
            type(status_type), intent(out) :: st

            st = status_type()

            if (forward) then
                call forward_expression(1_int64, st)
            else if (is_beginning_of_buffer()) then
                ! Arguably backward-sexp should signal this error for us.
                call signal_scan_error(st, "Beginning of buffer", point(), point())
            else
                call backward_expression(1_int64, st)
            end if
        end subroutine next_expression
    end subroutine simple_minded_indentation_engine_next_expression

    !> Skip over one expression.
    !> HALFEXPRESSION if true, means skip over a partial expression if needed.  I.e. if the
    !> first token we see is an operator, skip over its left-hand-side argument.
    !> HALFEXPRESSION can also be a token (TOKEN), in which case we should skip the text
    !> assuming it is the left-hand-side argument of that token.
    !> Possible results in RES:
    !>   (LEFT-LEVEL POSITION TOKEN): we couldn't skip TOKEN because its right-level
    !>     is too high.  LEFT-LEVEL is the left-level of TOKEN,
    !>     POSITION is its start position in the buffer.
    !>   LEVEL SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS, POSITION and TOKEN: same thing but for an open-paren or the
    !> beginning of buffer.
    !>     Instead of SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS, the LEVEL can also be
    !> SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST.
    !>   LEVEL SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE, POSITION and TOKEN: we skipped over a paren-like pair.
    !>   `null' .true.: we skipped over an identifier, matched parentheses, ...
    !> Port of smie.el:smie-backward-sexp.
    recursive subroutine simple_minded_indentation_engine_backward_expression(res, status, halfexpression, token)
        type(simple_minded_indentation_engine_expression_type), intent(out) :: res
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: halfexpression
        character(len=*), intent(in), optional :: token

        call simple_minded_indentation_engine_next_expression(.false., res, status, halfexpression, token)
    end subroutine simple_minded_indentation_engine_backward_expression

    !> Skip over one expression.
    !> HALFEXPRESSION if true, means skip over a partial expression if needed.  I.e. if the
    !> first token we see is an operator, skip over its right-hand-side argument.
    !> HALFEXPRESSION can also be a token (TOKEN), in which case we should skip the text
    !> assuming it is the right-hand-side argument of that token.
    !> Possible results in RES:
    !>   (RIGHT-LEVEL POSITION TOKEN): we couldn't skip TOKEN because its left-level
    !>     is too high.  RIGHT-LEVEL is the right-level of TOKEN,
    !>     POSITION is its end position in the buffer.
    !>   LEVEL SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS, POSITION and TOKEN: same thing but for a close-paren or the end
    !> of buffer.
    !>     Instead of SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_PARENTHESIS, the LEVEL can also be
    !> SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST.
    !>   LEVEL SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE, POSITION and TOKEN: we skipped over a paren-like pair.
    !>   `null' .true.: we skipped over an identifier, matched parentheses, ...
    !> Port of smie.el:smie-forward-sexp.
    recursive subroutine simple_minded_indentation_engine_forward_expression(res, status, halfexpression, token)
        type(simple_minded_indentation_engine_expression_type), intent(out) :: res
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: halfexpression
        character(len=*), intent(in), optional :: token

        call simple_minded_indentation_engine_next_expression(.true., res, status, halfexpression, token)
    end subroutine simple_minded_indentation_engine_forward_expression

    ! ------------------------------------------------------------------
    ! Miscellaneous commands using the precedence parser
    ! ------------------------------------------------------------------

    !> Move backward through N logical elements.  Port of
    !> smie.el:smie-backward-sexp-command.
    recursive subroutine simple_minded_indentation_engine_backward_expression_command(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_forward_expression_command(-n, status)
    end subroutine simple_minded_indentation_engine_backward_expression_command

    !> Move forward through N logical elements (the `forward_expression_function'
    !> of SIMPLE_MINDED_INDENTATION_ENGINE modes).  Port of smie.el:smie-forward-sexp-command.
    recursive subroutine simple_minded_indentation_engine_forward_expression_command(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_expression_type) :: res
        integer(int64) :: left, position
        logical :: forw, edge, was_local
        procedure(forward_expression_procedure), pointer :: saved

        status = status_type()
        forw = n > 0
        left = n
        ! (let ((forward-sexp-function nil)) ...)
        was_local = forward_expression_function%is_local()
        saved => forward_expression_function%get_current()
        call forward_expression_function%set_local()

        do while (left /= 0)
            if (forw) then
                left = left - 1
            else
                left = left + 1
            end if

            position = point()

            if (forw) then
                call simple_minded_indentation_engine_forward_expression(res, status, halfexpression=.true.)
            else
                call simple_minded_indentation_engine_backward_expression(res, status, halfexpression=.true.)
            end if

            if (.not. status%ok()) exit

            if (forw) then
                edge = is_end_of_buffer()
            else
                edge = is_beginning_of_buffer()
            end if

            if (.not. res%null .and. res%level%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE .and. position == point() &
                .and. .not. edge) then
                call signal_scan_error(status, "Containing expression ends prematurely", res%position, res%position)
                exit
            end if
        end do

        if (was_local) then
            call forward_expression_function%set_local(saved)
        else
            call forward_expression_function%clear_local()
        end if
    end subroutine simple_minded_indentation_engine_forward_expression_command

    !> The character of SYNTAX entry D matches the matching character of `syntax_after' at POSITION,
    !> as a string; "" if it has none.  Not a port.
    function matching_character(position) result(s)
        integer(int64), intent(in) :: position
        character(len=:), allocatable :: s
        type(syntax_descriptor_type) :: d
        integer(int32) :: c
        logical :: found
        type(status_type) :: st

        s = ""
        call character_after(c, found, position)
        if (.not. found) return
        d = syntax_entry_at(int(c), position)
        if (d%match >= 0) call character_to_string(int(d%match, int32), s, st)
    end function matching_character

    ! The characters of the UTF-8 string S.  Not a port.
    function string_characters_of(s) result(cs)
        character(len=*), intent(in) :: s
        integer, allocatable :: cs(:)
        integer :: i, c, n

        allocate (cs(0))
        i = 1

        do while (i <= len(s))
            c = ichar(s(i:i))

            if (c < 128) then
                n = 1
            else if (c < 224) then
                n = 2
                c = iand(c, 31)
            else if (c < 240) then
                n = 3
                c = iand(c, 15)
            else
                n = 4
                c = iand(c, 7)
            end if

            block
                integer :: j

                do j = 1, n - 1
                    if (i + j <= len(s)) c = c*64 + iand(ichar(s(i + j:i + j)), 63)
                end do
            end block

            cs = [cs, c]
            i = i + n
        end do
    end function string_characters_of

    ! The character before point, or -1 at the beginning (char-before).
    ! Not a port.
    integer function before_character()
        integer(int32) :: c
        logical :: found

        call character_before(c, found)
        before_character = -1
        if (found) before_character = int(c)
    end function before_character

    ! The character after point, or -1 at the end (char-after).  Not a port.
    integer function after_character()
        integer(int32) :: c
        logical :: found

        call character_after(c, found)
        after_character = -1
        if (found) after_character = int(c)
    end function after_character

    ! The character of `last_command_event', -1 if it is not one.
    ! Not a port.
    integer function command_character()
        command_character = -1
        if (vtag(last_command_event) == V_INTEGER) command_character = int(xfixnum(last_command_event))
    end function command_character

    ! Whether C is one of the characters of the string S (memq).
    ! Not a port.
    logical function character_in(c, s)
        integer, intent(in) :: c
        character(len=*), intent(in) :: s

        character_in = any(string_characters_of(s) == c)
    end function character_in

    ! (save-excursion (skip-chars-backward " \t") (is_beginning_of_line)) and the like are
    ! written out where used.
    !> Close the closest surrounding block.  Port of smie.el:smie-close-block.
    recursive subroutine simple_minded_indentation_engine_close_block(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: closer, open
        type(simple_minded_indentation_engine_grammar_type), pointer :: g
        type(simple_minded_indentation_engine_toklevels_type) :: t
        integer, allocatable :: levels(:), seen(:)
        type(string_type), allocatable :: found(:)
        integer(int64) :: here, moved
        integer :: level, k, i
        logical :: matched
        type(status_type) :: st

        here = point()

        body: block
            call backward_up_group(1_int64, status)
            if (.not. status%ok()) exit body
            matched = looking_at("\s(", status)
            if (.not. status%ok()) exit body

            if (matched) then
                closer = matching_character(point())
                exit body
            end if

            call simple_minded_indentation_engine_forward_token(open, status)
            if (.not. status%ok()) exit body
            if (.not. allocated(open)) open = ""
            g => grammar()
            closer = first_closer(open)
            if (allocated(closer)) exit body
            t = simple_minded_indentation_engine_grammar_token(open)

            ! Even if we improve the auto-computation of closers,
            ! there are still cases where we need manual
            ! intervention, e.g. for Octave's use of `until'
            ! as a pseudo-closer of `do'.
            if (t%null) then
                call set_error(status, GENERIC_ERROR_CODE, "Doesn't look like a block")
                exit body
            end if

            if (t%left%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                call set_error(status, GENERIC_ERROR_CODE, "Doesn't look like a block")
                exit body
            end if

            ! Now that smie-setup automatically sets smie-closer-alist
            ! from the BACKUS_NAUR_FORM, this is not really needed any more.
            levels = [simple_minded_indentation_engine_grammar_index(g, open)]
            allocate (seen(0), found(0))

            do while (size(levels) > 0)
                level = levels(size(levels))
                levels = levels(1:size(levels) - 1)

                do k = 1, size(g%tokens)
                    if (.not. level_eq(g%right(level), g%left(k))) cycle
                    if (any(seen == k)) cycle
                    seen = [seen, k]

                    if (g%right(k)%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                        levels = [levels, k]
                    else
                        call prepend_string(found, g%tokens(k)%text)
                    end if
                end do
            end do

            if (size(found) == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "No known closer for opener "//open)
                exit body
            end if

            ! What should we do if there are various closers?
            closer = found(1)%text
        end block body

        call goto_character(here)
        if (.not. status%ok()) return
        moved = skip_characters_backward(" "//achar(9), st)
        i = 0
        if (.not. is_beginning_of_line()) i = 1
        call goto_character(here)

        if (i == 1) then
            call newline(status)
            if (.not. status%ok()) return
        end if

        call insert(closer, status)
        if (.not. status%ok()) return
        here = point()
        moved = skip_characters_forward(" "//achar(9), st)
        matched = is_end_of_line()
        call goto_character(here)

        if (matched) then
            call indent_according_to_mode(status)
        else
            call reindent_then_newline_and_indent(status)
        end if

    contains

        ! (cdr (assoc OPEN smie-closer-alist)).
        function first_closer(o) result(c)
            character(len=*), intent(in) :: o
            character(len=:), allocatable :: c
            integer :: m

            if (.not. is_closer_table()) return

            do m = 1, size(g%openers)
                if (string_eq(g%openers(m)%text, o)) then
                    c = g%closers(m)%text
                    return
                end if
            end do
        end function first_closer
    end subroutine simple_minded_indentation_engine_close_block

    ! `simple_minded_indentation_engine_close_block' as a command.
    recursive subroutine execute_simple_minded_indentation_engine_close_block(status)
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_close_block(status)
    end subroutine execute_simple_minded_indentation_engine_close_block

    !> Move forward down one level paren-like blocks.  Like `down_group'.
    !> With argument ARGUMENT, do this that many times.
    !> A negative argument means move backward but still go down a level.
    !> This command assumes point is not in a string or comment.  Port of
    !> smie.el:smie-down-list.
    recursive subroutine simple_minded_indentation_engine_down_group(argument_in, status)
        integer(int64), intent(in) :: argument_in
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token
        type(simple_minded_indentation_engine_toklevels_type) :: levels
        integer(int64) :: argument, start, inc, position, end_position
        integer :: offset
        logical :: parenthesis, more

        status = status_type()
        argument = argument_in
        start = point()
        inc = 1
        offset = 0

        if (argument < 0) then
            inc = -1
            offset = 1
        end if

        do while (argument /= 0)
            argument = argument - inc

            do
                position = point()

                if (inc < 0) then
                    call simple_minded_indentation_engine_backward_token(token, status)
                else
                    call simple_minded_indentation_engine_forward_token(token, status)
                end if

                if (.not. status%ok()) return
                if (.not. allocated(token)) token = ""
                levels = simple_minded_indentation_engine_grammar_token(token)

                if (len(token) == 0) then
                    if (inc < 0) then
                        parenthesis = looking_back("\s(\|\s)", status, point() - 1)
                    else
                        parenthesis = looking_at("\s(\|\s)", status)
                    end if

                    if (.not. status%ok()) return

                    if (parenthesis) then
                        ! Go back to `start' in case of an error.  This presumes
                        ! none of the token we've found until now include a ( or ).
                        call goto_character(start)
                        call down_group(inc, status)
                        if (.not. status%ok()) return
                        more = .false.
                    else
                        call forward_expression(inc, status)
                        if (.not. status%ok()) return
                        more = point() /= position
                    end if
                else if (.not. levels%null .and. merge(levels%left%kind, levels%right%kind, &
                                                       offset == 0) /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                    more = .false.
                else if (.not. levels%null .and. merge(levels%left%kind, levels%right%kind, &
                                                       offset == 1) /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                    end_position = point()
                    call goto_character(start)
                    call signal_scan_error(status, "Containing expression ends prematurely", position, end_position)
                    return
                else
                    more = .true.
                end if

                if (.not. more) exit
            end do
        end do
    end subroutine simple_minded_indentation_engine_down_group

    ! `simple_minded_indentation_engine_down_group' as a command ("p").
    recursive subroutine execute_simple_minded_indentation_engine_down_group(status)
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_down_group(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_simple_minded_indentation_engine_down_group

    ! `simple_minded_indentation_engine_forward_expression_command' as a command ("^p").
    recursive subroutine execute_simple_minded_indentation_engine_forward_expression(status)
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_forward_expression_command(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_simple_minded_indentation_engine_forward_expression

    ! `simple_minded_indentation_engine_backward_expression_command' as a command ("^p").
    recursive subroutine execute_simple_minded_indentation_engine_backward_expression(status)
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_backward_expression_command(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_simple_minded_indentation_engine_backward_expression

    !> The `blink_matching_check_function' of SIMPLE_MINDED_INDENTATION_ENGINE modes: whether the
    !> opener at START and the closer before END do not match.  Port of
    !> smie.el:smie-blink-matching-check.
    recursive logical function simple_minded_indentation_engine_blink_matching_check(start, end) result(mismatch)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable :: ender, starter
        type(status_type) :: st
        integer(int64) :: here

        here = point()
        call goto_character(end)
        call simple_minded_indentation_engine_backward_token(ender, st)
        mismatch = .false.

        if (.not. allocated(ender)) then
            mismatch = blink_matching_check_mismatch(start, end)
        else if (.not. is_simple_minded_indentation_engine_closer(ender)) then
            ! This is not one of the begin..end we know how to check.
            mismatch = blink_matching_check_mismatch(start, end)
        else if (start == 0) then
            mismatch = .true.
        else
            call goto_character(start)
            call simple_minded_indentation_engine_forward_token(starter, st)
            if (.not. allocated(starter)) starter = ""
            mismatch = .not. is_closer_pair(starter, ender)
        end if

        call goto_character(here)
    end function simple_minded_indentation_engine_blink_matching_check

    !> Blink the matching opener when applicable.
    !> This uses SIMPLE_MINDED_INDENTATION_ENGINE's tables and is expected to be placed on
    !> `post_self_insert_hook'.  Port of smie.el:smie-blink-matching-open.
    recursive subroutine simple_minded_indentation_engine_blink_matching_open(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token, trig
        type(parse_state_type) :: parser_state
        type(simple_minded_indentation_engine_toklevels_type) :: t
        integer(int64) :: position, here, moved, original_point
        integer :: lce
        logical :: ok, was_local
        procedure(blink_matching_check_procedure), pointer :: saved
        type(status_type) :: st

        status = status_type()
        ! Position after the close token.
        position = point()
        lce = command_character()
        if (blink_matching_parenthesis%is_off()) return
        ! Optimization.
        if (.not. is_closer_table()) return
        ! Sanity check.
        ok = before_character() == lce

        if (.not. ok) then
            here = point()
            moved = skip_characters_backward(" "//achar(9), st)
            position = point()
            ok = before_character() == lce

            if (.not. ok) then
                moved = skip_characters_backward(" "//achar(10)//achar(9), st)
                position = point()
                ok = before_character() == lce
            end if

            call goto_character(here)
        end if

        if (.not. ok) return
        trig = triggers()
        if (.not. character_in(lce, trig)) return
        here = point()
        call syntax_parser_state(parser_state)
        call goto_character(here)
        if (parser_state%comstr_start /= 0) return
        ! (save-excursion ...)
        original_point = point()

        body: block
            call simple_minded_indentation_engine_backward_token(token, status)
            if (.not. status%ok()) exit body
            if (.not. allocated(token)) token = ""

            if (point() == position - 1 .and. character_count_of(token) == 1 &
                .and. .not. is_simple_minded_indentation_engine_closer(token)) then
                ! The trigger char is itself a token but is not one of the
                ! closers (e.g. ?\; in Octave mode), so go back to the
                ! previous token.
                position = point()
                call simple_minded_indentation_engine_backward_token(token, status)
                if (.not. status%ok()) exit body
                if (.not. allocated(token)) token = ""
            end if

            if (.not. is_simple_minded_indentation_engine_closer(token)) exit body
            ! We're after a close token.  Let's still make sure we
            ! didn't skip a comment to find that token.
            call simple_minded_indentation_engine_forward_token(trig, status)
            if (.not. status%ok()) exit body
            here = point()
            ! Skip the trigger char, if applicable.
            if (after_character() == lce) call forward_character(st, 1_int64)
            ! Skip any auto-indentation, if applicable.
            if (lce == 10) moved = skip_characters_forward(" "//achar(9), st)
            ok = point() >= position
            call goto_character(here)
            if (.not. ok) exit body
            ! If token ends with a trigger char, don't blink for
            ! anything else than this trigger char, lest we'd blink
            ! both when inserting the trigger char and when
            ! inserting a subsequent trigger char like SPC.
            ok = before_character() == lce
            if (.not. ok) ok = .not. character_in(before_character(), triggers())
            if (.not. ok) exit body

            ! FIXME: For octave's "switch ... case ... case" we flash
            ! `switch' at the end of the first `case' and we burp
            ! "mismatch" at the end of the second `case'.
            if (.not. simple_minded_indentation_engine_blink_matching_inners) then
                t = simple_minded_indentation_engine_grammar_token(token)

                if (.not. t%null) then
                    if (t%right%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) exit body
                end if
            end if

            ! The major mode might set blink-matching-check-function
            ! buffer-locally so that interactive calls to
            ! blink-matching-open work right, but let's not presume
            ! that's the case.
            was_local = blink_matching_check_function%is_local()
            saved => blink_matching_check_function%get_current()
            call blink_matching_check_function%set_local(simple_minded_indentation_engine_blink_matching_check)
            call blink_matching_open(status)

            if (was_local) then
                call blink_matching_check_function%set_local(saved)
            else
                call blink_matching_check_function%clear_local()
            end if
        end block body

        call goto_character(original_point)
    end subroutine simple_minded_indentation_engine_blink_matching_open

    ! The number of characters of S.  Not a port.
    integer function character_count_of(s)
        character(len=*), intent(in) :: s

        character_count_of = size(string_characters_of(s))
    end function character_count_of

    ! smie--opener/closer-at-point: (OPENER TOKEN START END), or nil
    ! (FOUND false).  OPENER is true if TOKEN is an opener and false if
    ! it's a closer.  Port of smie.el:smie--opener/closer-at-point.
    recursive subroutine opener_closer_at_point(found, opener, token, start_position, end_position, status)
        logical, intent(out) :: found, opener
        character(len=:), allocatable, intent(out) :: token
        integer(int64), intent(out) :: start_position, end_position
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: ignored, btok, atok
        integer(int64) :: start, bend, aend

        found = .false.
        opener = .false.
        start_position = 0
        end_position = 0
        start = point()
        ! Move to a previous position outside of a token.
        call simple_minded_indentation_engine_backward_token(ignored, status)
        if (.not. status%ok()) return
        ! Move to the end of the token before point.
        call simple_minded_indentation_engine_forward_token(btok, status)
        if (.not. status%ok()) return
        if (.not. allocated(btok)) btok = ""
        bend = point()

        if (bend >= start .and. is_simple_minded_indentation_engine_closer(btok)) then
            ! Token before point is a closer?
            call simple_minded_indentation_engine_backward_token(ignored, status)
            if (.not. status%ok()) return

            if (point() < start) then
                found = .true.
                token = btok
                start_position = point()
                end_position = bend
                call goto_character(bend)
            end if
        else if (bend > start .and. is_simple_minded_indentation_engine_opener(btok)) then
            ! Token around point is an opener?
            call simple_minded_indentation_engine_backward_token(ignored, status)
            if (.not. status%ok()) return

            if (point() <= start) then
                found = .true.
                opener = .true.
                token = btok
                start_position = point()
                end_position = bend
            end if
        else if (bend <= start) then
            call simple_minded_indentation_engine_forward_token(atok, status)
            if (.not. status%ok()) return
            if (.not. allocated(atok)) atok = ""
            aend = point()
            ! Hopefully shouldn't happen.
            if (aend < start) return

            ! Token after point is a closer?
            if (is_simple_minded_indentation_engine_opener(atok)) then
                call simple_minded_indentation_engine_backward_token(ignored, status)
                if (.not. status%ok()) return

                if (point() <= start) then
                    found = .true.
                    opener = .true.
                    token = atok
                    start_position = point()
                    end_position = aend
                end if
            end if
        end if
    end subroutine opener_closer_at_point

    ! The `show_parenthesis_data_function' of SIMPLE_MINDED_INDENTATION_ENGINE modes: smie--matching-block-data
    ! around the function it advises.  Port of
    ! smie.el:smie--matching-block-data.
    recursive subroutine simple_minded_indentation_engine_matching_block_data(found, data)
        logical, intent(out) :: found
        type(show_parenthesis_data_type), intent(out) :: data
        type(status_type) :: st
        type(parse_state_type) :: parser_state
        type(simple_minded_indentation_engine_expression_type) :: sdata
        type(simple_minded_indentation_engine_toklevels_type) :: ht, tt
        character(len=:), allocatable :: htok, ignored
        integer(int64) :: tick, hbeg, hend, tend, tbeg, here, point_value
        integer :: k
        logical :: hfound, hopener, tfound, tmatch

        found = .false.
        call make_local_entry(k)
        call buffer_characters_modified_tick(current_buffer(), tick, st)

        if (is_closer_table()) then
            if (.not. (locals(k)%has_cache .and. locals(k)%cache_position == point() .and. locals(k)%cache_tick == tick)) then
                locals(k)%has_cache = .true.
                locals(k)%cache_position = point()
                locals(k)%cache_tick = tick
                locals(k)%has_cache_data = .false.
                point_value = point()
                call syntax_parser_state(parser_state)
                call goto_character(point_value)

                if (parser_state%comstr_start == 0) then
                    body: block
                        call opener_closer_at_point(hfound, hopener, htok, hbeg, hend, st)
                        if (.not. st%ok()) exit body
                        if (.not. hfound) exit body

                        if (.not. simple_minded_indentation_engine_blink_matching_inners) then
                            ht = simple_minded_indentation_engine_grammar_token(htok)

                            if (.not. ht%null) then
                                if (hopener) then
                                    if (ht%left%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) exit body
                                else
                                    if (ht%right%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) exit body
                                end if
                            end if
                        end if

                        tfound = .false.
                        tmatch = .false.
                        tbeg = 0
                        tend = 0

                        if (hopener) then
                            ! Opener.
                            call simple_minded_indentation_engine_forward_expression(sdata, st, halfexpression=.true.)
                            if (.not. st%ok()) exit body
                            tend = point()

                            if (sdata%null .or. sdata%level%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE) then
                                call simple_minded_indentation_engine_backward_token(ignored, st)
                                if (.not. st%ok()) exit body
                                tfound = .true.
                                if (allocated(sdata%token)) tmatch = is_closer_pair(htok, sdata%token)
                                tbeg = point()
                            end if
                        else
                            ! Closer.
                            call simple_minded_indentation_engine_backward_expression(sdata, st, halfexpression=.true.)
                            if (.not. st%ok()) exit body

                            if (.not. sdata%null .and. sdata%level%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE) then
                                ht = simple_minded_indentation_engine_grammar_token(htok)
                                tt%null = .true.
                                if (allocated(sdata%token)) tt = simple_minded_indentation_engine_grammar_token(sdata%token)

                                if (.not. ht%null .and. .not. tt%null) then
                                    ! Here is an inner.
                                    if (ht%right%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER .and. level_eq(ht%right, &
                                                                                                                      tt%left)) then
                                        call goto_character(sdata%position)
                                        tbeg = point()
                                        call simple_minded_indentation_engine_forward_token(ignored, st)
                                        if (.not. st%ok()) exit body
                                        tfound = .true.
                                        tmatch = .true.
                                        tend = point()
                                    end if
                                end if
                            else
                                tbeg = point()
                                call simple_minded_indentation_engine_forward_token(ignored, st)
                                if (.not. st%ok()) exit body
                                tfound = .true.
                                if (.not. sdata%null .and. allocated(sdata%token)) tmatch = is_closer_pair(sdata%token, htok)
                                tend = point()
                            end if
                        end if

                        ! Update the cache.
                        locals(k)%has_cache_data = .true.
                        locals(k)%cache_data%here_beginning = hbeg
                        locals(k)%cache_data%here_end = hend

                        if (tfound) then
                            locals(k)%cache_data%there_beginning = tbeg
                            locals(k)%cache_data%there_end = tend
                        else
                            locals(k)%cache_data%there_beginning = 0
                            locals(k)%cache_data%there_end = 0
                        end if

                        locals(k)%cache_data%mismatch = .not. (tfound .and. tmatch)
                    end block body
                end if

                here = locals(k)%cache_position
                call goto_character(here)
            end if

            if (locals(k)%has_cache_data) then
                found = .true.
                data = locals(k)%cache_data
                return
            end if
        end if

        call advised_parenthesis_data(k, found, data)
    end subroutine simple_minded_indentation_engine_matching_block_data

    ! The `show_parenthesis_data_function' SIMPLE_MINDED_INDENTATION_ENGINE's advice wraps.  Not a port.
    recursive subroutine advised_parenthesis_data(k, found, data)
        integer, intent(in) :: k
        logical, intent(out) :: found
        type(show_parenthesis_data_type), intent(out) :: data
        procedure(show_parenthesis_data_procedure), pointer :: fn

        found = .false.

        if (locals(k)%parenthesis_data_local) then
            fn => locals(k)%parenthesis_data
        else
            fn => show_parenthesis_data_function%procedure
        end if

        if (associated(fn)) call fn(found, data)
    end subroutine advised_parenthesis_data

    ! ------------------------------------------------------------------
    ! The indentation engine
    ! ------------------------------------------------------------------

    ! (funcall smie--hanging-eolp-function): the buffer's advised one, or
    ! the default.  Not a port.
    recursive logical function is_hanging_end_of_line()
        procedure(simple_minded_indentation_engine_hanging_line_procedure), pointer :: test

        test => simple_minded_indentation_engine_hanging_line_test_function%get_current()

        if (associated(test)) then
            is_hanging_end_of_line = test()
        else
            is_hanging_end_of_line = is_simple_minded_indentation_engine_default_hanging_end_of_line()
        end if
    end function is_hanging_end_of_line

    !> The default smie--hanging-eolp-function: skip blanks; true at the
    !> end of a line (or a comment up to it).  (A quick hack upstream,
    !> undocumented there.) Port of the default value of smie.el's
    !> smie--hanging-eolp-function.
    recursive logical function is_simple_minded_indentation_engine_default_hanging_end_of_line() result(end_of_line)
        type(status_type) :: st
        integer(int64) :: moved

        moved = skip_characters_forward(" "//achar(9), st)
        end_of_line = is_end_of_line()
        ! (looking-at comment-start-skip) ;(bug#16041).
        if (.not. end_of_line) end_of_line = forward_comment(point_maximum())
    end function is_simple_minded_indentation_engine_default_hanging_end_of_line

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `simple_minded_indentation_engine_hanging_line_test_function'.  Not a port.
    subroutine set_simple_minded_indentation_engine_hanging_line_test_local(self, local_procedure)
        class(simple_minded_indentation_engine_hanging_line_test_setting_type), intent(inout) :: self
        procedure(simple_minded_indentation_engine_hanging_line_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_simple_minded_indentation_engine_hanging_line_test_local

    !> The current buffer's value of
    !> `simple_minded_indentation_engine_hanging_line_test_function':
    !> its own, else the global one.  Not a port.
    function get_simple_minded_indentation_engine_hanging_line_test_current(self) result(current_procedure)
        class(simple_minded_indentation_engine_hanging_line_test_setting_type), intent(in) :: self
        procedure(simple_minded_indentation_engine_hanging_line_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_simple_minded_indentation_engine_hanging_line_test_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_simple_minded_indentation_engine_hanging_line_test_entry(self)
        class(simple_minded_indentation_engine_hanging_line_test_setting_type), intent(inout) :: self
        type(simple_minded_indentation_engine_hanging_line_test_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_simple_minded_indentation_engine_hanging_line_test_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_simple_minded_indentation_engine_hanging_line_test_entry(self, k)
        class(simple_minded_indentation_engine_hanging_line_test_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_simple_minded_indentation_engine_hanging_line_test_entry

    !> Return true if the current token is "hanging".
    !> A hanging keyword is one that's at the end of a line except it's not at
    !> the beginning of a line.  Port of smie.el:smie-indent--hanging-p
    !> (smie-rule-hanging-p).
    recursive logical function is_simple_minded_indentation_engine_rule_hanging(status) result(hanging)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token
        integer(int64) :: here, lep
        logical :: ok

        status = status_type()
        hanging = .false.
        if (is_simple_minded_indentation_engine_rule_beginning_of_line()) return
        here = point()
        lep = line_end_position()
        call simple_minded_indentation_engine_forward_token(token, status)

        if (status%ok()) then
            if (.not. allocated(token)) token = ""
            ! Could be an open-paren.
            if (len(token) == 0 .and. .not. is_end_of_buffer()) call forward_character(status, 1_int64)
            ok = is_hanging_end_of_line()
            hanging = lep <= point()
        end if

        call goto_character(here)
    end function is_simple_minded_indentation_engine_rule_hanging

    !> Return true if the current token is the first on the line.  Port of
    !> smie.el:smie-indent--bolp (smie-rule-bolp).
    recursive logical function is_simple_minded_indentation_engine_rule_beginning_of_line()
        integer(int64) :: here, moved
        type(status_type) :: st

        here = point()
        moved = skip_characters_backward(" "//achar(9), st)
        is_simple_minded_indentation_engine_rule_beginning_of_line = is_beginning_of_line()
        call goto_character(here)
    end function is_simple_minded_indentation_engine_rule_beginning_of_line

    !> Return true if the current token is the first on the line.
    !> Comments are treated as spaces.  Port of smie.el:smie-indent--bolp-1.
    recursive logical function simple_minded_indentation_engine_indent_is_beginning_of_line_1()
        integer(int64) :: beginning_of_line, here
        logical :: ok

        beginning_of_line = line_beginning_position()
        here = point()
        ok = forward_comment(-point())
        simple_minded_indentation_engine_indent_is_beginning_of_line_1 = point() <= beginning_of_line
        call goto_character(here)
    end function simple_minded_indentation_engine_indent_is_beginning_of_line_1

    !> Like `current_column', but if there's a comment before us, use that.
    !> Port of smie.el:smie-indent--current-column.
    recursive integer function simple_minded_indentation_engine_indent_current_column()
        integer(int64) :: position, lbp
        logical :: ok

        position = point()
        lbp = line_beginning_position()
        ok = forward_comment(-1_int64)
        if (ok) ok = point() >= lbp
        if (.not. ok) call goto_character(position)
        simple_minded_indentation_engine_indent_current_column = int(current_column())
        call goto_character(position)
    end function simple_minded_indentation_engine_indent_current_column

    !> The parent of the current token: smie--parent, computed if needed.
    !> Port of smie.el:smie-indent--parent.
    recursive subroutine simple_minded_indentation_engine_parent(parent, status)
        type(simple_minded_indentation_engine_expression_type), intent(out) :: parent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token
        type(simple_minded_indentation_engine_toklevels_type) :: t
        type(simple_minded_indentation_engine_expression_type) :: res
        integer(int64) :: position, here

        status = status_type()

        if (.not. simple_minded_indentation_engine_parent_variable%null) then
            parent = simple_minded_indentation_engine_parent_variable
            return
        end if

        here = point()

        body: block
            position = point()
            call simple_minded_indentation_engine_forward_token(token, status)
            if (.not. status%ok()) exit body
            t%null = .true.
            if (allocated(token)) t = simple_minded_indentation_engine_grammar_token(token)

            if (t%null) then
                call goto_character(position)
            else if (t%left%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                call goto_character(position)
            end if

            call simple_minded_indentation_engine_backward_expression(parent, status, halfexpression=.true.)
            if (.not. status%ok()) exit body

            if (parent%null) then
                do
                    call simple_minded_indentation_engine_backward_expression(res, status)
                    if (.not. status%ok()) exit body
                    if (.not. res%null) exit
                end do

                parent%null = .false.
                parent%level = simple_minded_indentation_engine_level_type()
                parent%position = point()
                if (allocated(res%token)) parent%token = res%token
            end if

            simple_minded_indentation_engine_parent_variable = parent
        end block body

        call goto_character(here)
    end subroutine simple_minded_indentation_engine_parent

    !> The level of the parent: the first element of `simple_minded_indentation_engine_indent__parent'.  Not a port.
    recursive function simple_minded_indentation_engine_parent_level(status) result(level)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_level_type) :: level
        type(simple_minded_indentation_engine_expression_type) :: parent

        call simple_minded_indentation_engine_parent(parent, status)
        level = parent%level
    end function simple_minded_indentation_engine_parent_level

    !> Return true if the current token's parent is among PARENTS (an
    !> array; trailing blanks are not part of the tokens).
    !> Only meaningful when called from within `simple_minded_indentation_engine_rules_function'.
    !> Port of smie.el:smie-rule-parent-p.
    recursive logical function is_simple_minded_indentation_engine_rule_parent(parents, status) result(p)
        character(len=*), intent(in) :: parents(:)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_expression_type) :: parent

        p = .false.
        call simple_minded_indentation_engine_parent(parent, status)
        if (.not. status%ok()) return
        if (allocated(parent%token)) p = in_list(parent%token, parents)
    end function is_simple_minded_indentation_engine_rule_parent

    ! Whether S is one of the trimmed strings of LIST.  Not a port.
    pure logical function in_list(s, list)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: list(:)
        integer :: i

        in_list = .false.

        do i = 1, size(list)
            if (string_eq(s, trim(list(i)))) in_list = .true.
        end do
    end function in_list

    !> Return true if the next token is among TOKENS.
    !> Only meaningful when called from within `simple_minded_indentation_engine_rules_function'.
    !> Port of smie.el:smie-rule-next-p.
    recursive logical function is_simple_minded_indentation_engine_rule_next(tokens, status) result(p)
        character(len=*), intent(in) :: tokens(:)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_toklevels_type) :: next
        integer(int64) :: here

        p = .false.
        here = point()

        body: block
            if (simple_minded_indentation_engine_after_variable == 0) then
                next = simple_minded_indentation_engine_indent_forward_token(status)
                if (.not. status%ok()) exit body
                simple_minded_indentation_engine_after_variable = point()
            end if

            call goto_character(simple_minded_indentation_engine_after_variable)
            next = simple_minded_indentation_engine_indent_forward_token(status)
            if (.not. status%ok()) exit body
            if (.not. next%null) p = in_list(next%token, tokens)
        end block body

        call goto_character(here)
    end function is_simple_minded_indentation_engine_rule_next

    !> Return true if the previous token is among TOKENS.  Port of
    !> smie.el:smie-rule-prev-p.
    recursive logical function is_simple_minded_indentation_engine_rule_previous(tokens, status) result(p)
        character(len=*), intent(in) :: tokens(:)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_toklevels_type) :: previous
        integer(int64) :: here

        p = .false.
        here = point()
        previous = simple_minded_indentation_engine_indent_backward_token(status)
        if (status%ok() .and. .not. previous%null) p = in_list(previous%token, tokens)
        call goto_character(here)
    end function is_simple_minded_indentation_engine_rule_previous

    !> Return true if the parent is actually a sibling.
    !> Only meaningful when called from within `simple_minded_indentation_engine_rules_function'.
    !> Port of smie.el:smie-rule-sibling-p.
    recursive logical function is_simple_minded_indentation_engine_rule_sibling(status) result(p)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_level_type) :: level, left
        type(simple_minded_indentation_engine_toklevels_type) :: t

        level = simple_minded_indentation_engine_parent_level(status)
        p = .false.
        if (.not. status%ok()) return
        left = simple_minded_indentation_engine_level_type()

        if (allocated(simple_minded_indentation_engine_token)) then
            t = simple_minded_indentation_engine_grammar_token(simple_minded_indentation_engine_token)
            if (.not. t%null) left = t%left
        end if

        p = level_eq(level, left)
    end function is_simple_minded_indentation_engine_rule_sibling

    !> Align with parent.
    !> If present, OFFSET should be an integer giving an additional offset to apply.
    !> Only meaningful when called from within `simple_minded_indentation_engine_rules_function'.
    !> Port of smie.el:smie-rule-parent.
    recursive function simple_minded_indentation_engine_rule_parent(status, offset) result(r)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: offset
        type(simple_minded_indentation_engine_rule_result_type) :: r
        type(simple_minded_indentation_engine_expression_type) :: parent
        type(simple_minded_indentation_engine_indent_type) :: v
        integer(int64) :: here

        here = point()
        call simple_minded_indentation_engine_parent(parent, status)
        if (.not. status%ok()) return
        call goto_character(parent%position)
        v = simple_minded_indentation_engine_indent_virtual(status)
        call goto_character(here)
        if (.not. status%ok()) return

        if (v%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
            call number_error(status, v)
            return
        end if

        r%kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN
        r%value = v%column
        if (present(offset)) r%value = r%value + offset
    end function simple_minded_indentation_engine_rule_parent

    ! The error of arithmetic on an indentation that is not a number.
    ! Not a port.
    subroutine number_error(status, v)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_indent_type), intent(in) :: v

        if (v%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT) then
            call set_error(status, GENERIC_ERROR_CODE, &
                           "Wrong type argument: expected integer, real or type(marker_type), got noindent")
        else
            call set_error(status, GENERIC_ERROR_CODE, &
                           "Wrong type argument: expected integer, real or type(marker_type), got unallocated")
        end if
    end subroutine number_error

    ! smie-indent--separator-outdent.  Port of
    ! smie.el:smie-indent--separator-outdent.
    recursive integer function separator_outdent(status) result(n)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token
        integer(int64) :: here, afterpos, moved

        here = point()
        n = simple_minded_indentation_engine_rule_separator_outdent
        call simple_minded_indentation_engine_forward_token(token, status)

        if (.not. status%ok()) then
            call goto_character(here)
            return
        end if

        if (.not. allocated(token)) then
            if (allocated(simple_minded_indentation_engine_token)) then
                call message("smie-rule-separator: Can't skip token "//simple_minded_indentation_engine_token)
            else
                call message('smie-rule-separator: Can''t skip token ""')
            end if
        end if

        moved = skip_characters_forward(" ", status)
        afterpos = 0
        if (.not. is_end_of_line()) afterpos = point()
        call goto_character(here)

        ! This should always be true, unless
        ! smie-forward-token-function skipped a \n.
        if (afterpos /= 0) then
            if (afterpos < line_end_position()) n = int(afterpos - point())
        end if
    end function separator_outdent

    !> Indent current token as a "separator".
    !> By "separator", we mean here a token whose sole purpose is to separate
    !> various elements within some enclosing syntactic construct, and which does
    !> not have any semantic significance in itself (i.e. it would typically no exist
    !> as a node in an abstract syntax tree).
    !> Such a token is expected to have an associative syntax and be closely tied
    !> to its syntactic parent.  Typical examples are "," in lists of arguments
    !> (enclosed inside parentheses), or ";" in sequences of instructions (enclosed
    !> in a {..} or begin..end block).
    !> METHOD should be the method name that was passed to `simple_minded_indentation_engine_rules_function'.
    !> Only meaningful when called from within `simple_minded_indentation_engine_rules_function'.
    !> Port of smie.el:smie-rule-separator.
    recursive function simple_minded_indentation_engine_rule_separator(method, status) result(r)
        integer, intent(in) :: method
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_rule_result_type) :: r
        type(simple_minded_indentation_engine_rule_result_type) :: pr
        type(simple_minded_indentation_engine_toklevels_type) :: t
        integer(int64) :: here
        integer :: parent_position_column, outdent
        logical :: ok, sibling

        status = status_type()

        if (method == SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE .and. is_simple_minded_indentation_engine_rule_beginning_of_line()) &
            then
            sibling = is_simple_minded_indentation_engine_rule_sibling(status)
            if (.not. status%ok()) return

            if (.not. sibling) then
                pr = simple_minded_indentation_engine_rule_parent(status)
                if (.not. status%ok()) return
                ! FIXME: we knew this when computing smie--parent.
                here = point()
                call goto_character(simple_minded_indentation_engine_parent_variable%position)
                t = simple_minded_indentation_engine_indent_forward_token(status)
                if (status%ok()) ok = forward_comment(point_maximum())
                parent_position_column = int(current_column())
                call goto_character(here)
                if (.not. status%ok()) return
                outdent = separator_outdent(status)
                if (.not. status%ok()) return
                r%kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN
                r%value = max(pr%value, min(parent_position_column, parent_position_column - outdent))
                return
            end if
        end if

        if (method == SIMPLE_MINDED_INDENTATION_ENGINE_AFTER .and. is_simple_minded_indentation_engine_rule_beginning_of_line()) &
            then
            r%kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
            r%value = separator_outdent(status)
        end if
    end function simple_minded_indentation_engine_rule_separator

    !> The offset of ELEMENT ("basic" or "args"): what `simple_minded_indentation_engine_rules_function'
    !> gives for (SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT ELEMENT), else for (SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT "basic"),
    !> else
    !> `simple_minded_indentation_engine_indent_basic'.  Port of smie.el:smie-indent--offset.
    recursive integer function simple_minded_indentation_engine_indent_offset(element, status) result(n)
        character(len=*), intent(in) :: element
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_rule_result_type) :: r

        n = simple_minded_indentation_engine_indent_basic
        call execute_rules(SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT, element, r, status)
        if (.not. status%ok()) return

        if (r%kind == SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE .and. element /= "basic") then
            call execute_rules(SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT, "basic", r, status)
            if (.not. status%ok()) return
        end if

        if (r%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE) n = r%value
    end function simple_minded_indentation_engine_indent_offset

    ! smie-indent--rule: compute indentation column according to
    ! `simple_minded_indentation_engine_rules_function'.  METHOD and TOKEN are passed to it.  AFTER is
    ! the position after TOKEN, if known (0 if not).  PARENT is the parent
    ! info returned by `simple_minded_indentation_engine_backward_expression', if known.  BASE_POSITION is the
    ! position relative to which offsets should be applied (0 for none).
    ! Port of smie.el:smie-indent--rule.
    recursive function indent_rule(method, token, status, after, parent, base_position) result(indent)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: after, base_position
        type(simple_minded_indentation_engine_expression_type), intent(in), optional :: parent
        type(simple_minded_indentation_engine_indent_type) :: indent
        type(simple_minded_indentation_engine_rule_result_type) :: offset
        type(simple_minded_indentation_engine_indent_type) :: v
        integer(int64) :: here
        logical :: virtual

        here = point()
        call indent_rule_1(method, token, offset, status, after, parent)

        body: block
            if (.not. status%ok()) exit body

            select case (offset%kind)
            case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE)
                continue
            case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN)
                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                      column=offset%value)
            case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET)
                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                      column=offset%value)
                if (.not. present(base_position)) exit body
                if (base_position == 0) exit body
                call goto_character(base_position)
                ! Use smie-indent-virtual when indenting relative to an opener:
                ! this will also by default use current-column unless
                ! that opener is hanging, but will additionally consult
                ! rules-function, so it gives it a chance to tweak indentation
                ! (e.g. by forcing indentation relative to its own parent, as in
                ! fn a => fn b => fn c =>).
                ! When parent==nil it doesn't matter because the only case
                ! where it's really used is when the base-pos is hanging anyway.
                virtual = .false.

                if (present(parent)) then
                    if (.not. parent%null) virtual = parent%level%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE
                end if

                if (.not. virtual) then
                    virtual = is_simple_minded_indentation_engine_rule_hanging(status)
                    if (.not. status%ok()) exit body
                end if

                if (virtual) then
                    v = simple_minded_indentation_engine_indent_virtual(status)
                    if (.not. status%ok()) exit body

                    if (v%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
                        call number_error(status, v)
                        exit body
                    end if

                    indent%column = indent%column + v%column
                else
                    indent%column = indent%column + int(current_column())
                end if
            case default
                call set_error(status, GENERIC_ERROR_CODE, "Unknown indentation offset")
            end select
        end block body

        call goto_character(here)
    end function indent_rule

    ! smie-indent--rule-1: call `simple_minded_indentation_engine_rules_function' with smie--parent,
    ! smie--token and smie--after bound.  Port of
    ! smie.el:smie-indent--rule-1.
    recursive subroutine indent_rule_1(method, token, result, status, after, parent)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: after
        type(simple_minded_indentation_engine_expression_type), intent(in), optional :: parent
        type(simple_minded_indentation_engine_expression_type) :: saved_parent
        character(len=:), allocatable :: saved_token
        integer(int64) :: saved_after
        logical :: had_token

        saved_parent = simple_minded_indentation_engine_parent_variable
        had_token = allocated(simple_minded_indentation_engine_token)
        if (had_token) saved_token = simple_minded_indentation_engine_token
        saved_after = simple_minded_indentation_engine_after_variable
        simple_minded_indentation_engine_parent_variable = simple_minded_indentation_engine_expression_type()
        if (present(parent)) simple_minded_indentation_engine_parent_variable = parent
        simple_minded_indentation_engine_token = token
        simple_minded_indentation_engine_after_variable = 0
        if (present(after)) simple_minded_indentation_engine_after_variable = after
        call execute_rules(method, token, result, status)
        simple_minded_indentation_engine_parent_variable = saved_parent

        if (had_token) then
            simple_minded_indentation_engine_token = saved_token
        else if (allocated(simple_minded_indentation_engine_token)) then
            deallocate (simple_minded_indentation_engine_token)
        end if

        simple_minded_indentation_engine_after_variable = saved_after
    end subroutine indent_rule_1

    !> Skip token forward and return it, along with its levels.
    !> Point should be between tokens when calling this function (i.e.,
    !> not in the middle of a string/comment).  Port of
    !> smie.el:smie-indent-forward-token.
    recursive function simple_minded_indentation_engine_indent_forward_token(status) result(t)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_toklevels_type) :: t
        character(len=:), allocatable :: token
        logical :: parenthesis

        call simple_minded_indentation_engine_forward_token(token, status)
        if (.not. status%ok()) return

        if (allocated(token)) then
            if (len(token) > 0) then
                t = simple_minded_indentation_engine_grammar_token(token)
                return
            end if
        end if

        parenthesis = looking_at("\s(\|\s)\(\)", status)
        if (.not. status%ok()) return

        if (parenthesis) then
            t%null = .false.

            if (match_end(1, status) > 0) then
                t%left = simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, value=0)
                t%right = simple_minded_indentation_engine_level_type()
            else
                t%left = simple_minded_indentation_engine_level_type()
                t%right = simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, value=0)
            end if

            call forward_character(status, 1_int64)
            t%token = substring(point() - 1, point())
            return
        end if

        parenthesis = looking_at("\s""\|\s|", status)
        if (.not. status%ok()) return

        if (parenthesis) then
            call forward_expression(1_int64, status)
            return
        end if

        if (is_end_of_buffer()) return

        if (allocated(token)) then
            call set_error(status, GENERIC_ERROR_CODE, "Bumped into unknown token: "//quoted_text(token))
        else
            call set_error(status, GENERIC_ERROR_CODE, 'Bumped into unknown token: ""')
        end if
    end function simple_minded_indentation_engine_indent_forward_token

    !> Skip token backward and return it, along with its levels.  Port of
    !> smie.el:smie-indent-backward-token.
    recursive function simple_minded_indentation_engine_indent_backward_token(status) result(t)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_toklevels_type) :: t
        character(len=:), allocatable :: token
        integer :: class

        call simple_minded_indentation_engine_backward_token(token, status)
        if (.not. status%ok()) return

        if (allocated(token)) then
            if (len(token) > 0) then
                t = simple_minded_indentation_engine_grammar_token(token)
                return
            end if
        end if

        class = syntax_class_after(point() - 1)

        ! 4 == open paren syntax, 5 == close.
        if (class == 4 .or. class == 5) then
            call forward_character(status, -1_int64)
            t%null = .false.
            t%token = substring(point(), point() + 1)

            if (class == 4) then
                t%left = simple_minded_indentation_engine_level_type()
                t%right = simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, value=0)
            else
                t%left = simple_minded_indentation_engine_level_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER, value=0)
                t%right = simple_minded_indentation_engine_level_type()
            end if
        else if (class == 7 .or. class == 15) then
            call backward_expression(1_int64, status)
        else if (is_beginning_of_buffer()) then
            continue
        else
            call set_error(status, GENERIC_ERROR_CODE, "Bumped into unknown token")
        end if
    end function simple_minded_indentation_engine_indent_backward_token

    ! (syntax-class (syntax-after POSITION)), -1 for nil.  Not a port.
    integer function syntax_class_after(position)
        integer(int64), intent(in) :: position
        type(syntax_descriptor_type) :: d
        integer(int32) :: c
        logical :: found

        syntax_class_after = -1
        call character_after(c, found, position)
        if (.not. found) return
        d = syntax_entry_at(int(c), position)
        if (d%code < 0) return
        syntax_class_after = iand(d%code, 65535)
    end function syntax_class_after

    ! (car (syntax-after POSITION)), -1 for nil.  Not a port.
    integer function syntax_code_after(position)
        integer(int64), intent(in) :: position
        type(syntax_descriptor_type) :: d
        integer(int32) :: c
        logical :: found

        syntax_code_after = -1
        call character_after(c, found, position)
        if (.not. found) return
        d = syntax_entry_at(int(c), position)
        syntax_code_after = d%code
    end function syntax_code_after

    !> Compute the virtual indentation to use for point.
    !> This is used when we're not trying to indent point but just
    !> need to compute the column at which point should be indented
    !> in order to figure out the indentation of some other (further down) point.
    !> Port of smie.el:smie-indent-virtual.
    recursive function simple_minded_indentation_engine_indent_virtual(status) result(indent)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_indent_type) :: indent

        status = status_type()

        ! Trust pre-existing indentation on other lines.
        if (is_simple_minded_indentation_engine_rule_beginning_of_line()) then
            indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                  column=int(current_column()))
        else
            indent = simple_minded_indentation_engine_indent_calculate(status)
        end if
    end function simple_minded_indentation_engine_indent_virtual

    ! A column as an indentation.  Not a port.
    pure function column_of(column_number) result(indent)
        integer(int64), intent(in) :: column_number
        type(simple_minded_indentation_engine_indent_type) :: indent

        indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                              column=int(column_number))
    end function column_of

    !> Obey the `fixindent' special comment.  Port of
    !> smie.el:smie-indent-fixindent.
    recursive subroutine simple_minded_indentation_engine_indent_fixindent(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: here, found
        character(len=:), allocatable :: start_skip, end_skip

        status = status_type()
        if (.not. is_simple_minded_indentation_engine_rule_beginning_of_line()) return
        here = point()
        call comment_normalize_variables(status)
        if (.not. status%ok()) return
        start_skip = ""
        if (allocated(comment_start_skip%value)) start_skip = comment_start_skip%value
        end_skip = ""
        if (allocated(comment_end_skip%value)) end_skip = comment_end_skip%value
        ! 1+ to account for the \n comment termination.
        call regular_expression_search_forward("\(?:"//start_skip//"\)fixindent\(?:"//end_skip//"\)", found, status, &
                                               bound=1 + line_end_position(), noerror=SEARCH_NOERROR)
        call goto_character(here)
        if (.not. status%ok()) return
        if (found /= 0) indent = column_of(current_column())
    end subroutine simple_minded_indentation_engine_indent_fixindent

    !> Start the file at column 0.  Port of smie.el:smie-indent-bob.
    recursive subroutine simple_minded_indentation_engine_indent_beginning_of_buffer(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: here
        logical :: ok

        status = status_type()
        here = point()
        ok = forward_comment(-point())
        if (is_beginning_of_buffer()) indent = simple_minded_indentation_engine_indent_type( &
                                             kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, column=programming_first_column())
        call goto_character(here)
    end subroutine simple_minded_indentation_engine_indent_beginning_of_buffer

    !> Align close paren with opening paren.  Port of
    !> smie.el:smie-indent-close.
    recursive subroutine simple_minded_indentation_engine_indent_close(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_rule_result_type) :: r
        integer(int64) :: here, moved
        logical :: at_close

        status = status_type()
        here = point()

        body: block
            ! (forward-comment (point-max))
            at_close = looking_at("\s)", status)
            if (.not. status%ok() .or. .not. at_close) exit body
            call indent_rule_1(SIMPLE_MINDED_INDENTATION_ENGINE_CLOSE_ALL, substring(point(), point() + 1), r, status, &
                               after=1 + point())
            if (.not. status%ok()) exit body

            if (r%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE) then
                do while (skip_syntax_forward(")") /= 0)
                    moved = skip_characters_forward(" "//achar(9), status)
                end do
            else
                call forward_character(status, 1_int64)
                if (.not. status%ok()) exit body
            end if

            call backward_expression(1_int64, status)

            if (status%code == SCAN_ERROR_CODE) then
                status = status_type()
                exit body
            end if

            if (.not. status%ok()) exit body
            ! :not-hanging
            indent = simple_minded_indentation_engine_indent_virtual(status)
        end block body

        call goto_character(here)
    end subroutine simple_minded_indentation_engine_indent_close

    ! Whether two token levels are `equal'.  Not a port.
    logical function toklevels_equal(a, b)
        type(simple_minded_indentation_engine_toklevels_type), intent(in) :: a, b

        toklevels_equal = a%null .eqv. b%null
        if (.not. toklevels_equal .or. a%null) return
        toklevels_equal = string_eq(a%token, b%token) .and. level_equal(a%left, b%left) .and. level_equal(a%right, b%right)
    end function toklevels_equal

    ! Whether two levels are `equal'.  Not a port.
    pure logical function level_equal(a, b)
        type(simple_minded_indentation_engine_level_type), intent(in) :: a, b

        level_equal = a%kind == b%kind
        if (level_equal .and. (a%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER .or. a%kind &
                               == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_LIST)) level_equal = a%value == b%value
    end function level_equal

    !> Indent point based on the token that follows it immediately.
    !> If TOKEN is present, assume that that is the token that follows point.
    !> Returns either a column number or SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE if it considers that indentation
    !> should not be computed on the basis of the following token.  Port of
    !> smie.el:smie-indent-keyword.
    recursive subroutine simple_minded_indentation_engine_indent_keyword(indent, status, token)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: token
        type(simple_minded_indentation_engine_toklevels_type) :: toklevels, res, again
        type(simple_minded_indentation_engine_expression_type) :: parent
        type(simple_minded_indentation_engine_level_type) :: plevel
        type(status_type) :: st
        integer(int64) :: position, endpos, shortage, here
        character(len=:), allocatable :: token_text
        logical :: skip, ok

        status = status_type()
        position = point()

        body: block
            if (present(token)) then
                toklevels = simple_minded_indentation_engine_grammar_token(token)
            else
                res = simple_minded_indentation_engine_indent_forward_token(status)
                if (.not. status%ok()) exit body
                ! Ignore tokens on subsequent lines.
                skip = .false.

                if (position < line_beginning_position()) then
                    ! Make sure `token' also *starts* on another line.
                    endpos = point()
                    call goto_character(position)
                    shortage = forward_line(1_int64)
                    ! As seen in bug#22960, pos may be inside
                    ! a string, and forward-token may then stumble.
                    again = simple_minded_indentation_engine_indent_forward_token(st)
                    if (st%ok()) skip = toklevels_equal(res, again) .and. point() == endpos
                    call goto_character(endpos)
                end if

                if (skip) then
                    toklevels = simple_minded_indentation_engine_toklevels_type()
                else
                    call goto_character(position)
                    toklevels = res
                end if
            end if

            ! Not a keyword.
            if (toklevels%null) exit body
            token_text = toklevels%token

            if (toklevels%left%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) then
                ! Different cases:
                ! - smie-indent--bolp: "indent according to others".
                ! - common hanging: "indent according to others".
                ! - SML-let hanging: "indent like parent".
                ! - if-after-else: "indent-like parent".
                ! - middle-of-line: "trust current position".
                indent = indent_rule(SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE, token_text, status)
                if (.not. status%ok() .or. indent%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE) exit body
                ! I.e. non-virtual indent.
                ! For an open-paren-like thingy at BEGINNING_OF_LINE, always indent only
                ! based on other rules (typically smie-indent-after-keyword).
                ! FIXME: we do the same if after a comment, since we may be trying
                ! to compute the indentation of this comment and we shouldn't indent
                ! based on the indentation of subsequent code.
                if (simple_minded_indentation_engine_indent_is_beginning_of_line_1()) exit body
                ! By default use point unless we're hanging.
                ok = is_simple_minded_indentation_engine_rule_hanging(status)
                if (.not. status%ok()) exit body
                if (.not. ok) indent = column_of(current_column())
                exit body
            end if

            ! FIXME: This still looks too much like black magic!!
            call simple_minded_indentation_engine_backward_expression(parent, status, token=token_text)
            if (.not. status%ok()) exit body
            ! Different behaviors:
            ! - align with parent.
            ! - parent + offset.
            ! - after parent's column + offset (actually, after or before
            !   depending on where backward-sexp stopped).
            ! ? let it drop to some other indentation function (almost never).
            ! ? parent + offset + parent's own offset.
            ! Different cases:
            ! - bump into a same-level operator.
            ! - bump into a specific known parent.
            ! - find a matching open-paren thingy.
            ! - bump into some random parent.
            ! ? borderline case (almost never).
            ! ? bump immediately into a parent.
            ok = point() < position
            if (.not. ok .and. .not. parent%null) ok = parent%position /= 0 .and. parent%position < position
            ! If we didn't move at all, that means we didn't really skip
            ! what we wanted.  Should almost never happen, other than
            ! maybe when an infix or close-paren is at the beginning
            ! of a buffer.
            if (.not. ok) exit body
            here = point()
            call goto_character(position)

            if (parent%null) then
                indent = indent_rule(SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE, token_text, status, parent=parent)
            else
                indent = indent_rule(SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE, token_text, status, parent=parent, &
                                     base_position=parent%position)
            end if

            call goto_character(here)
            if (.not. status%ok() .or. indent%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE) exit body
            plevel = merge_level(parent)

            if (level_eq(plevel, toklevels%left)) then
                ! We bumped into a same-level operator; align with it.
                ok = is_simple_minded_indentation_engine_rule_beginning_of_line() .and. point() /= position

                if (ok) then
                    here = point()
                    call goto_character(parent%position)
                    ok = .not. is_simple_minded_indentation_engine_rule_beginning_of_line()
                    call goto_character(here)
                end if

                if (ok) then
                    ! If the parent is at END_OF_LINE and its children are indented like
                    ! itself, then we can just obey the indentation chosen for the
                    ! child.
                    ! This is important for operators like ";" which
                    ! are usually at END_OF_LINE (and have an offset of 0): otherwise we'd
                    ! always go back over all the statements, which is
                    ! a performance problem and would also mean that fixindents
                    ! in the middle of such a sequence would be ignored.
                    !
                    ! This is a delicate point!
                    ! Even if the offset is not 0, we could follow the same logic
                    ! and subtract the offset from the child's indentation.
                    ! But that would more often be a bad idea: OT1H we generally
                    ! want to reuse the closest similar indentation point, so that
                    ! the user's choice (or the fixindents) are obeyed.  But OTOH
                    ! we don't want this to affect "unrelated" parts of the code.
                    ! E.g. a fixindent in the body of a "begin..end" should not
                    ! affect the indentation of the "end".
                    indent = column_of(current_column())
                else
                    call goto_character(parent%position)
                    ! Don't use (smie-indent-virtual :not-hanging) here, because we
                    ! want to jump back over a sequence of same-level ops such as
                    !    a -> b -> c
                    !    -> d
                    ! So as to align with the earliest appropriate place.
                    indent = simple_minded_indentation_engine_indent_virtual(status)
                end if
            else if (point() == position .and. is_simple_minded_indentation_engine_rule_beginning_of_line()) then
                ! Since we started at BEGINNING_OF_LINE, we're not computing a virtual
                ! indentation, and we're still at the starting point, so
                ! we can't use `current-column' which would cause
                ! indentation to depend on itself and we can't use
                ! smie-indent-virtual since that would be an inf-loop.
                continue
            else if (plevel%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE) then
                ! In indent-keyword, if we're indenting `then' wrt `if', we
                ! want to use indent-virtual rather than use just
                ! current-column, so that we can apply the (:before . "if")
                ! rule which does the "else if" dance in SML.  But in other
                ! cases, we do not want to use indent-virtual (e.g. indentation
                ! of "*" w.r.t "+", or ";" wrt "(").  We could just always use
                ! indent-virtual and then have indent-rules say explicitly to
                ! use `point' after things like "(" or "+" when they're not at
                ! END_OF_LINE, but you'd end up with lots of those rules.
                ! So we use a heuristic here, which is that we only use virtual
                ! if the parent is tightly linked to the child token (they're
                ! part of the same BACKUS_NAUR_FORM rule).
                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                    column=simple_minded_indentation_engine_indent_current_column())
            else
                indent = simple_minded_indentation_engine_indent_virtual(status)
            end if
        end block body

        call goto_character(position)

    contains

        ! (car parent): nil for a nil parent.
        function merge_level(p) result(l)
            type(simple_minded_indentation_engine_expression_type), intent(in) :: p
            type(simple_minded_indentation_engine_level_type) :: l

            l = simple_minded_indentation_engine_level_type()
            if (.not. p%null) l = p%level
        end function merge_level
    end subroutine simple_minded_indentation_engine_indent_keyword

    ! `simple_minded_indentation_engine_indent_keyword' as an indentation function.  Not a port.
    recursive subroutine indent_keyword_function(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_indent_keyword(indent, status)
    end subroutine indent_keyword_function

    !> Compute indentation of a comment.  Port of
    !> smie.el:smie-indent-comment.
    recursive subroutine simple_minded_indentation_engine_indent_comment(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: next
        integer(int64) :: position, found, at, moved
        logical :: ok, skip

        status = status_type()
        ! Don't do it for virtual indentations.  We should normally never be "in
        ! front of a comment" when doing virtual-indentation anyway.  And if we are
        ! (as can happen in octave-mode), moving forward can lead to inf-loops.
        if (.not. is_simple_minded_indentation_engine_rule_beginning_of_line()) return
        position = point()

        body: block
            call beginning_of_line()

            if (.not. allocated(comment_start_skip%value)) then
                call set_error(status, GENERIC_ERROR_CODE, "Wrong type argument: expected character(len=*), got unallocated")
                exit body
            end if

            call regular_expression_search_forward(comment_start_skip%value, found, status, bound=line_end_position(), &
                                                   noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit body
            at = match_end(1, status)
            if (at == 0) at = match_beginning(0, status)
            if (at /= position) exit body
            call goto_character(position)
            ok = forward_comment(point_maximum())
            moved = skip_characters_forward(" "//achar(9)//achar(13)//achar(10), status)
            ! Don't align with a closer, since the comment is "within" the
            ! closed element.  Don't align with END_OF_BUFFER either.
            at = point()
            call simple_minded_indentation_engine_forward_token(next, status)
            if (.not. status%ok()) exit body
            if (.not. allocated(next)) next = ""
            skip = .false.
            if (len(next) == 0) skip = is_end_of_buffer() .or. syntax_code_after(point()) == 5
            if (.not. skip) skip = is_simple_minded_indentation_engine_closer(next)
            call goto_character(at)
            if (skip) exit body
            ! FIXME: We assume here that smie-indent-calculate will compute the
            ! indentation of the next token based on text before the comment,
            ! but this is not guaranteed, so maybe we should let
            ! smie-indent-calculate return some info about which buffer
            ! position was used as the "indentation base" and check that this
            ! base is before `position'.
            indent = simple_minded_indentation_engine_indent_calculate(status)
        end block body

        call goto_character(position)
    end subroutine simple_minded_indentation_engine_indent_comment

    !> Indentation of comment-continue lines.  Port of
    !> smie.el:smie-indent-comment-continue.
    recursive subroutine simple_minded_indentation_engine_indent_comment_continue(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cont
        type(parse_state_type) :: parser_state
        integer(int64) :: position, start, shortage, moved
        logical :: ok

        status = status_type()
        if (.not. allocated(comment_continue%value)) return
        cont = comment_string_strip(comment_continue%value, .true., .true.)
        if (len(cont) == 0) return
        ok = looking_at(regular_expression_quote(cont), status)
        if (.not. status%ok() .or. .not. ok) return
        position = point()
        call syntax_parser_state(parser_state)
        call goto_character(position)
        if (parser_state%incomment == 0) return

        body: block
            shortage = forward_line(-1_int64)
            start = parser_state%comstr_start

            if (point() <= start) then
                call goto_character(start)
                ok = .false.

                if (allocated(comment_start_skip%value)) then
                    ok = looking_at(comment_start_skip%value, status)
                    if (.not. status%ok()) exit body
                end if

                if (.not. ok) then
                    call forward_character(status, 1_int64)
                else
                    call goto_character(match_end(0, status))
                    moved = skip_characters_backward(" "//achar(9), status)

                    ! Try to align the first char of the comment-continue
                    ! with the second char of the comment-start or the
                    ! first char if the comment-start is made of
                    ! a single char.  E.g.
                    !
                    !     /* foo
                    !      * bar */
                    !
                    ! but
                    !
                    !     { foo
                    !     | bar }
                    if (point() == 1 + start) then
                        call goto_character(start)
                    else
                        call goto_character(1 + start)
                    end if
                end if

                indent = column_of(current_column())
            else
                moved = skip_characters_forward(" "//achar(9), status)
                ok = looking_at(regular_expression_quote(cont), status)
                if (status%ok() .and. ok) indent = column_of(current_column())
            end if
        end block body

        call goto_character(position)
    end subroutine simple_minded_indentation_engine_indent_comment_continue

    !> Indentation of a comment closer.  Port of
    !> smie.el:smie-indent-comment-close.
    recursive subroutine simple_minded_indentation_engine_indent_comment_close(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: ender, start
        type(parse_state_type) :: parser_state
        integer(int64) :: position, index, mb0, mb2, me2
        logical :: ok

        status = status_type()
        if (.not. allocated(comment_end_skip%value)) return
        ! Not just a \n comment-closer.
        ok = looking_at(" "//achar(9)//"*$", status)
        if (.not. status%ok() .or. ok) return
        ok = looking_at(comment_end_skip%value, status)
        if (.not. status%ok() .or. .not. ok) return
        ender = substring(match_beginning(0, status), match_end(0, status))
        position = point()
        call syntax_parser_state(parser_state)
        call goto_character(position)
        if (parser_state%incomment == 0) return

        body: block
            call goto_character(parser_state%comstr_start)
            if (.not. allocated(comment_start_skip%value)) exit body
            ok = looking_at(comment_start_skip%value, status)
            if (.not. status%ok() .or. .not. ok) exit body
            start = substring(match_beginning(0, status), match_end(0, status))
            ! Align the common substring between starter
            ! and ender, if possible.
            index = string_match("\(.+\).*"//achar(10)//"\(.*?\)\1", start//achar(10)//ender, status)
            if (.not. status%ok()) exit body

            if (index > 0) then
                mb0 = match_beginning(0, status) - 1
                mb2 = match_beginning(2, status)
                me2 = match_end(2, status)
                indent = column_of(current_column() + mb0 + (mb2 - me2))
            else
                indent = column_of(current_column())
            end if
        end block body

        call goto_character(position)
    end subroutine simple_minded_indentation_engine_indent_comment_close

    !> Inside a comment: `noindent'.  Port of
    !> smie.el:smie-indent-comment-inside.
    recursive subroutine simple_minded_indentation_engine_indent_comment_inside(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(parse_state_type) :: parser_state
        integer(int64) :: position

        status = status_type()
        position = point()
        call syntax_parser_state(parser_state)
        call goto_character(position)
        if (parser_state%incomment /= 0) indent%kind = SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT
    end subroutine simple_minded_indentation_engine_indent_comment_inside

    !> Inside a string: `noindent'.  Port of
    !> smie.el:smie-indent-inside-string.
    recursive subroutine simple_minded_indentation_engine_indent_inside_string(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(parse_state_type) :: parser_state
        integer(int64) :: position

        status = status_type()
        position = point()
        call syntax_parser_state(parser_state)
        call goto_character(position)
        if (parser_state%instring /= -1) indent%kind = SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT
    end subroutine simple_minded_indentation_engine_indent_inside_string

    !> Indentation right after a special keyword.  Port of
    !> smie.el:smie-indent-after-keyword.
    recursive subroutine simple_minded_indentation_engine_indent_after_keyword(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_toklevels_type) :: toklevel
        type(simple_minded_indentation_engine_indent_type) :: v
        integer(int64) :: position
        integer :: basic

        status = status_type()
        position = point()

        body: block
            toklevel = simple_minded_indentation_engine_indent_backward_token(status)
            if (.not. status%ok() .or. toklevel%null) exit body
            indent = indent_rule(SIMPLE_MINDED_INDENTATION_ENGINE_AFTER, toklevel%token, status, after=position, &
                                 base_position=point())
            if (.not. status%ok() .or. indent%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE) exit body
            ! The default indentation after a keyword/operator is
            ! 0 for infix, t for prefix, and use another rule
            ! for postfix.
            ! A closer.
            if (toklevel%right%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER) exit body

            if (toklevel%left%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NUMBER &
                .or. is_simple_minded_indentation_engine_closer(toklevel%token)) then
                ! An opener, or an inner.
                v = simple_minded_indentation_engine_indent_virtual(status)
                if (.not. status%ok()) exit body
                basic = simple_minded_indentation_engine_indent_offset("basic", status)
                if (.not. status%ok()) exit body

                if (v%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
                    call number_error(status, v)
                    exit body
                end if

                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                      column=v%column + basic)
            else
                ! An infix.
                indent = simple_minded_indentation_engine_indent_virtual(status)
            end if
        end block body

        call goto_character(position)
    end subroutine simple_minded_indentation_engine_indent_after_keyword

    !> Indentation rule when there's nothing yet on the line.
    !> Without this rule, SIMPLE_MINDED_INDENTATION_ENGINE assumes that an empty line will be filled with an
    !> argument (since it falls back to smie-indent-sexps), which tends
    !> to indent far too deeply.  Port of smie.el:smie-indent-empty-line.
    recursive subroutine simple_minded_indentation_engine_indent_empty_line(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_rule_result_type) :: r
        type(simple_minded_indentation_engine_toklevels_type) :: t

        status = status_type()
        if (.not. is_end_of_line()) return
        ! FIXME: Should we default to ";"?
        call execute_rules(SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT, "empty_line_token", r, status)
        if (.not. status%ok()) return
        if (r%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN) return
        t = simple_minded_indentation_engine_grammar_token(r%token)
        if (.not. t%null) call simple_minded_indentation_engine_indent_keyword(indent, status, r%token)
    end subroutine simple_minded_indentation_engine_indent_empty_line

    !> Indentation of sequences of simple expressions without
    !> intervening keywords or operators.  E.g. "a b c" or "g (balbla) f".
    !> Can be a list of expressions or a function call.
    !> If it's a function call, the first element is special (it's the
    !> function).  We distinguish function calls from mere lists of
    !> expressions based on whether the preceding token is listed in
    !> the `list-intro' entry of smie-indent-rules.  Port of
    !> smie.el:smie-indent-exps.
    recursive subroutine simple_minded_indentation_engine_indent_exps(indent, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_expression_type) :: res
        type(simple_minded_indentation_engine_rule_result_type) :: r
        character(len=:), allocatable :: token
        integer(int64), allocatable :: positions(:)
        integer(int64) :: here, inner
        integer :: arguments
        logical :: argument

        status = status_type()
        here = point()
        allocate (positions(0))

        body: block
            do
                call simple_minded_indentation_engine_backward_expression(res, status)
                if (.not. status%ok()) exit body
                if (.not. (res%null .or. res%level%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE)) exit
                positions = [point(), positions]
                if (is_simple_minded_indentation_engine_rule_beginning_of_line()) exit
            end do

            ! Figure out if the atom we just skipped is an argument rather
            ! than a function.
            inner = point()
            call simple_minded_indentation_engine_backward_expression(res, status)
            if (.not. status%ok()) exit body
            argument = res%null .or. res%level%kind == SIMPLE_MINDED_INDENTATION_ENGINE_LEVEL_NONE

            if (.not. argument) then
                call simple_minded_indentation_engine_backward_token(token, status)
                if (.not. status%ok()) exit body
                if (.not. allocated(token)) token = ""
                call execute_rules(SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO, token, r, status)
                if (.not. status%ok()) exit body
                argument = r%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE
            end if

            call goto_character(inner)

            if (size(positions) == 0) then
                ! We're the first expression of the list.  In that case, the
                ! indentation should be (have been) determined by its context.
                continue
            else if (argument) then
                ! There's a previous element, and it's not special (it's not
                ! the function), so let's just align with that one.
                call goto_character(positions(1))
                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                    column=simple_minded_indentation_engine_indent_current_column())
            else if (size(positions) > 1) then
                ! We skipped some args plus the function and bumped into something.
                ! Align with the first arg.
                call goto_character(positions(2))
                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                                    column=simple_minded_indentation_engine_indent_current_column())
            else
                ! We're the first arg.
                call goto_character(positions(1))
                arguments = simple_minded_indentation_engine_indent_offset("args", status)
                if (.not. status%ok()) exit body
                ! We used to use (smie-indent-virtual), but that
                ! doesn't seem right since it might then indent args less than
                ! the function itself.
                indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, &
                                                        column=arguments + simple_minded_indentation_engine_indent_current_column())
            end if
        end block body

        call goto_character(here)
    end subroutine simple_minded_indentation_engine_indent_exps

    ! ------------------------------------------------------------------
    ! `simple_minded_indentation_engine_indent_functions'
    ! ------------------------------------------------------------------

    ! The index in SELF's local values of the current buffer's, 0 if none.
    ! Not a port.
    integer function indent_functions_local_index(self) result(k)
        class(simple_minded_indentation_engine_indent_functions_type), intent(in) :: self

        k = 0
        if (current == 0 .or. .not. allocated(self%locals)) return
        k = findloc(self%locals%serial, buffers(current)%serial, dim=1)
    end function indent_functions_local_index

    ! The functions run in the current buffer, in order.  Not a port.
    function indent_functions_current(self) result(list)
        class(simple_minded_indentation_engine_indent_functions_type), intent(in) :: self
        type(indent_entry_type), allocatable :: list(:)
        integer :: k, i, j

        allocate (list(0))
        k = self%local_index()

        if (k == 0) then
            if (allocated(self%entries)) list = self%entries
            return
        end if

        do i = 1, size(self%locals(k)%entries)
            if (self%locals(k)%entries(i)%is_t) then
                if (.not. allocated(self%entries)) cycle

                do j = 1, size(self%entries)
                    list = [list, self%entries(j)]
                end do
            else
                list = [list, self%locals(k)%entries(i)]
            end if
        end do
    end function indent_functions_current

    ! The %prepend of `simple_minded_indentation_engine_indent_functions' (add-hook).  Not a port.
    subroutine indent_functions_prepend(self, fn, local)
        class(simple_minded_indentation_engine_indent_functions_type), intent(inout) :: self
        procedure(simple_minded_indentation_engine_indent_procedure) :: fn
        logical, intent(in), optional :: local

        call self%place(fn, .false., local)
    end subroutine indent_functions_prepend

    ! The %append of `simple_minded_indentation_engine_indent_functions' (add-hook with APPEND).  Not a port.
    subroutine indent_functions_append(self, fn, local)
        class(simple_minded_indentation_engine_indent_functions_type), intent(inout) :: self
        procedure(simple_minded_indentation_engine_indent_procedure) :: fn
        logical, intent(in), optional :: local

        call self%place(fn, .true., local)
    end subroutine indent_functions_append

    ! Add FN, at the end if AT_END, else at the front.  Not a port.
    subroutine indent_functions_place(self, fn, at_end, local)
        class(simple_minded_indentation_engine_indent_functions_type), intent(inout) :: self
        procedure(simple_minded_indentation_engine_indent_procedure) :: fn
        logical, intent(in) :: at_end
        logical, intent(in), optional :: local
        type(indent_entry_type) :: e
        type(indent_local_type) :: entry
        integer :: k, i
        logical :: in_buffer

        in_buffer = .false.
        if (present(local)) in_buffer = local
        e%fn => fn

        if (.not. in_buffer) then
            if (.not. allocated(self%entries)) allocate (self%entries(0))

            do i = 1, size(self%entries)
                if (associated(self%entries(i)%fn, fn)) return
            end do

            if (at_end) then
                self%entries = [self%entries, e]
            else
                self%entries = [e, self%entries]
            end if

            return
        end if

        if (current == 0) return
        k = self%local_index()

        if (k == 0) then
            if (.not. allocated(self%locals)) allocate (self%locals(0))

            do k = 1, size(self%locals)
                if (.not. buffer_serial_live(self%locals(k)%serial)) exit
            end do

            if (k > size(self%locals)) self%locals = [self%locals, entry]
            self%locals(k)%serial = buffers(current)%serial
            ! A buffer's own value starts as (t).
            if (allocated(self%locals(k)%entries)) deallocate (self%locals(k)%entries)
            allocate (self%locals(k)%entries(1))
            self%locals(k)%entries(1)%is_t = .true.
        end if

        do i = 1, size(self%locals(k)%entries)
            if (self%locals(k)%entries(i)%is_t) cycle
            if (associated(self%locals(k)%entries(i)%fn, fn)) return
        end do

        if (at_end) then
            self%locals(k)%entries = [self%locals(k)%entries, e]
        else
            self%locals(k)%entries = [e, self%locals(k)%entries]
        end if
    end subroutine indent_functions_place

    ! The %remove of `simple_minded_indentation_engine_indent_functions' (remove-hook).  Not a port.
    subroutine indent_functions_remove(self, fn, local)
        class(simple_minded_indentation_engine_indent_functions_type), intent(inout) :: self
        procedure(simple_minded_indentation_engine_indent_procedure) :: fn
        logical, intent(in), optional :: local
        type(indent_entry_type), allocatable :: kept(:)
        integer :: k, i
        logical :: in_buffer

        in_buffer = .false.
        if (present(local)) in_buffer = local
        allocate (kept(0))

        if (.not. in_buffer) then
            if (.not. allocated(self%entries)) return

            do i = 1, size(self%entries)
                if (.not. associated(self%entries(i)%fn, fn)) kept = [kept, self%entries(i)]
            end do

            call move_alloc(kept, self%entries)
            return
        end if

        k = self%local_index()
        if (k == 0) return

        do i = 1, size(self%locals(k)%entries)
            if (self%locals(k)%entries(i)%is_t) then
                kept = [kept, self%locals(k)%entries(i)]
            else if (.not. associated(self%locals(k)%entries(i)%fn, fn)) then
                kept = [kept, self%locals(k)%entries(i)]
            end if
        end do

        call move_alloc(kept, self%locals(k)%entries)
    end subroutine indent_functions_remove

    !> Compute the indentation to use for point.  Port of
    !> smie.el:smie-indent-calculate.
    recursive function simple_minded_indentation_engine_indent_calculate(status) result(indent)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_indent_type) :: indent
        type(indent_entry_type), allocatable :: functions(:)
        integer :: i

        status = status_type()
        functions = simple_minded_indentation_engine_indent_functions%get_current()

        do i = 1, size(functions)
            if (.not. associated(functions(i)%fn)) cycle
            call functions(i)%fn(indent, status)
            if (.not. status%ok()) return
            if (indent%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE) return
        end do
    end function simple_minded_indentation_engine_indent_calculate

    !> Indent current line using the SIMPLE_MINDED_INDENTATION_ENGINE indentation engine.  NOINDENT is
    !> set when the engine said `noindent'.  Port of smie.el:smie-indent-line.
    recursive subroutine simple_minded_indentation_engine_indent_line(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        type(simple_minded_indentation_engine_indent_type) :: indent
        type(status_type) :: st
        integer(int64) :: savep, shortage, moved, here, column_number
        logical :: keep

        status = status_type()
        noindent = .false.
        savep = point()
        keep = .true.
        here = point()
        shortage = forward_line(0_int64)
        moved = skip_characters_forward(" "//achar(9), st)
        if (point() >= savep) keep = .false.
        indent = simple_minded_indentation_engine_indent_calculate(st)
        call goto_character(here)

        if (.not. st%ok()) then
            ! (with-demoted-errors "SIMPLE_MINDED_INDENTATION_ENGINE Error: %S" ...)
            call message("SMIE Error: "//format_error(st))
            indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, column=0)
        else if (indent%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NONE) then
            indent = simple_minded_indentation_engine_indent_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN, column=0)
        end if

        if (indent%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_NOINDENT) then
            ! If something funny is used (e.g. `noindent'), return it.
            noindent = .true.
            return
        end if

        ! Just in case.
        column_number = max(0, indent%column)

        if (keep) then
            call save_point_indent(column_number, status)
        else
            call indent_line_to(column_number, status)
        end if

    contains

        ! (save-excursion (indent-line-to COLUMN)).
        subroutine save_point_indent(c, s)
            integer(int64), intent(in) :: c
            type(status_type), intent(out) :: s
            type(handle_type) :: m
            integer(int64) :: p

            call copy_marker_at(point(), m, .false., s)
            call indent_line_to(c, s)
            call marker_position(m, p, st)
            call goto_character(p)
            call destroy_marker(m, st)
        end subroutine save_point_indent
    end subroutine simple_minded_indentation_engine_indent_line

    ! smie-auto-fill around the function it advises (the
    ! `normal_auto_fill_function' of SIMPLE_MINDED_INDENTATION_ENGINE modes).  Not a port.
    recursive subroutine simple_minded_indentation_engine_auto_fill_function(status)
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_auto_fill(status)
    end subroutine simple_minded_indentation_engine_auto_fill_function

    ! The auto-fill function SIMPLE_MINDED_INDENTATION_ENGINE's advice wraps.  Not a port.
    recursive subroutine do_advised_auto_fill(status)
        type(status_type), intent(out) :: status
        procedure(command_procedure), pointer :: fn
        integer :: k

        k = local_index()
        fn => null()

        if (k > 0) then
            if (locals(k)%auto_fill_local) then
                fn => locals(k)%auto_fill
            else
                call function_setting_default(normal_auto_fill_function, fn)
            end if
        else
            call function_setting_default(normal_auto_fill_function, fn)
        end if

        if (associated(fn)) then
            call fn(status)
        else
            call do_auto_fill(status)
        end if
    end subroutine do_advised_auto_fill

    !> Auto-fill for SIMPLE_MINDED_INDENTATION_ENGINE modes: break the line where SIMPLE_MINDED_INDENTATION_ENGINE gains the most
    !> indentation, else call the advised function (DO-AUTO-FILL).  Port of
    !> smie.el:smie-auto-fill.
    recursive subroutine simple_minded_indentation_engine_auto_fill(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: parser_state
        type(simple_minded_indentation_engine_toklevels_type) :: t
        type(simple_minded_indentation_engine_indent_type) :: newcol
        integer(int64) :: fc, here, end_position, bsf, gain, newgain, moved, reached
        logical :: done, skip
        type(handle_type) :: saved
        type(status_type) :: st

        status = status_type()
        fc = current_fill_column()

        if (current_column() > fc) then
            ! The loop below presumes BEGINNING_OF_LINE is outside of strings or comments.  Also,
            ! sometimes we prefer to fill the comment than the code around it.
            here = point()
            call syntax_parser_state(parser_state, line_beginning_position())
            call goto_character(here)
            skip = parser_state%comstr_start /= 0

            if (.not. skip) then
                call move_to_column(fc, reached, .false., status)
                call syntax_parser_state(parser_state)
                call goto_character(here)
                skip = parser_state%incomment /= 0
            end if

            if (.not. skip) then
                do
                    ! We silence the error completely since errors are "normal" in
                    ! some cases and an error message would be annoying (bug#19342).
                    done = .false.
                    call copy_marker_at(point(), saved, .false., st)

                    attempt: block
                        end_position = point()
                        ! Best-so-far.
                        bsf = 0
                        gain = 0
                        call beginning_of_line()

                        do
                            t = simple_minded_indentation_engine_indent_forward_token(status)
                            if (.not. status%ok()) exit attempt
                            if (.not. (point() <= end_position .and. current_column() <= fc)) exit
                            ! FIXME?  `smie-indent-calculate' can (and often
                            ! does) return a result that actually depends on the
                            ! presence/absence of a newline, so the gain computed
                            ! here may not be accurate, but in practice it seems
                            ! to work well enough.
                            moved = skip_characters_forward(" "//achar(9), status)
                            newcol = simple_minded_indentation_engine_indent_calculate(status)
                            if (.not. status%ok()) exit attempt

                            if (newcol%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
                                call number_error(status, newcol)
                                exit attempt
                            end if

                            newgain = current_column() - newcol%column

                            if (newgain > gain) then
                                gain = newgain
                                bsf = point()
                            end if
                        end do

                        if (gain > 0) then
                            call goto_character(bsf)
                            call newline_and_indent(status)
                            if (.not. status%ok()) exit attempt
                            done = .true.
                        end if
                    end block attempt

                    ! (save-excursion ...)
                    call marker_position(saved, here, st)
                    call goto_character(here)
                    call destroy_marker(saved, st)
                    if (status%code == SCAN_ERROR_CODE) status = status_type()
                    if (.not. status%ok()) return
                    if (.not. (done .and. current_column() > fc)) exit
                end do
            end if
        end if

        if (current_column() > fc) call do_advised_auto_fill(status)
    end subroutine simple_minded_indentation_engine_auto_fill

    ! smie--matching-block-data around the function it advises.  Not a port.
    recursive subroutine simple_minded_indentation_engine_parenthesis_data_function(found, data)
        logical, intent(out) :: found
        type(show_parenthesis_data_type), intent(out) :: data

        call simple_minded_indentation_engine_matching_block_data(found, data)
    end subroutine simple_minded_indentation_engine_parenthesis_data_function

    !> Setup SIMPLE_MINDED_INDENTATION_ENGINE navigation and indentation.
    !> GRAMMAR is a grammar table generated by `simple_minded_indentation_engine_prec2_to_grammar'.
    !> RULES_FUNCTION is a set of indentation rules for use on `simple_minded_indentation_engine_rules_function'.
    !> FORWARD_TOKEN and BACKWARD_TOKEN, if present, are the lexer
    !> functions (upstream's :forward-token and :backward-token keywords).
    !> Port of smie.el:smie-setup.
    subroutine simple_minded_indentation_engine_setup(grammar_in, rules_function, forward_token, backward_token)
        type(simple_minded_indentation_engine_grammar_type), intent(in), target :: grammar_in
        procedure(simple_minded_indentation_engine_rules_procedure) :: rules_function
        procedure(simple_minded_indentation_token_procedure), optional :: forward_token, backward_token
        type(string_type), allocatable :: closers(:)
        type(string_type) :: temporary
        character(len=:), allocatable :: closer, trig, text
        integer, allocatable :: trigs(:), cs(:)
        integer :: k, i, j
        logical :: dup
        type(status_type) :: st

        if (current == 0) return
        call make_local_entry(k)
        locals(k)%has_rules = .true.
        locals(k)%rules => rules_function
        locals(k)%grammar => grammar_in
        call indent_line_function%set_local(simple_minded_indentation_engine_indent_line)

        ! (add-function :around (local 'normal-auto-fill-function) #'smie-auto-fill),
        ! which does nothing when the advice is already there.
        if (.not. locals(k)%has_auto_fill) then
            locals(k)%has_auto_fill = .true.
            locals(k)%auto_fill_local = normal_auto_fill_function%is_local()
            locals(k)%auto_fill => normal_auto_fill_function%value
            call normal_auto_fill_function%set_local(simple_minded_indentation_engine_auto_fill_function)
        end if

        call forward_expression_function%set_local(simple_minded_indentation_engine_forward_expression_command)
        if (present(forward_token)) locals(k)%forward_token => forward_token
        if (present(backward_token)) locals(k)%backward_token => backward_token
        if (.not. grammar_in%has_closer_table) return
        locals(k)%has_closer_table = .true.
        ! Only needed for interactive calls to blink-matching-open.
        call blink_matching_check_function%set_local(simple_minded_indentation_engine_blink_matching_check)
        call post_self_insert_hook%append(simple_minded_indentation_engine_blink_matching_open, local=.true.)

        ! (add-function :around (local 'show-paren-data-function) #'smie--matching-block-data)
        if (.not. locals(k)%has_parenthesis_data) then
            locals(k)%has_parenthesis_data = .true.
            locals(k)%parenthesis_data_local = show_parenthesis_data_function%is_local()
            locals(k)%parenthesis_data => show_parenthesis_data_function%get_current()
            call show_parenthesis_data_function%set_local(simple_minded_indentation_engine_parenthesis_data_function)
        end if

        ! Setup smie-blink-matching-triggers.  Rather than wait for SPC to
        ! blink, try to blink as soon as we type the last char of a block ender.
        closers = grammar_in%closers

        do i = 2, size(closers)
            do j = i, 2, -1
                if (.not. is_string_less(closers(j)%text, closers(j - 1)%text)) exit
                temporary = closers(j)
                closers(j) = closers(j - 1)
                closers(j - 1) = temporary
            end do
        end do

        allocate (trigs(0))

        do i = 1, size(closers)
            closer = closers(i)%text

            ! FIXME: this eliminates prefixes of other closers, but we
            ! should probably eliminate prefixes of other keywords as well.
            if (i < size(closers)) then
                if (len(closer) <= len(closers(i + 1)%text)) then
                    if (closers(i + 1)%text(1:len(closer)) == closer) cycle
                end if
            end if

            cs = string_characters_of(closer)
            if (size(cs) > 0) trigs = [cs(size(cs)), trigs]
        end do

        trig = triggers()

        do i = 1, size(trigs)
            dup = .false.

            do j = 1, i - 1
                if (trigs(j) == trigs(i)) dup = .true.
            end do

            if (dup) cycle
            call character_to_string(int(trigs(i), int32), text, st)
            trig = trig//text
        end do

        call make_local_entry(k)
        locals(k)%has_triggers = .true.
        locals(k)%triggers = trig
    end subroutine simple_minded_indentation_engine_setup

    !> Go to the next line that needs to be reindented (and reindent it).
    !> Port of smie.el:smie--next-indent-change.
    recursive subroutine simple_minded_indentation_engine_next_indent_change(status)
        type(status_type), intent(out) :: status
        integer(int64) :: tick, after, shortage
        type(status_type) :: st

        do
            call buffer_characters_modified_tick(current_buffer(), tick, st)
            call indent_according_to_mode(status)
            if (.not. status%ok()) return
            call buffer_characters_modified_tick(current_buffer(), after, st)
            if (tick /= after) exit
            shortage = forward_line(1_int64)
        end do
    end subroutine simple_minded_indentation_engine_next_indent_change

    ! ------------------------------------------------------------------
    ! User configuration
    ! ------------------------------------------------------------------

    ! Whether the answer R of `simple_minded_indentation_engine_rules_function' for METHOD and TOKEN
    ! is the signature of RULE.  Not a port.
    logical function is_rule_for(rule, method, token, r) result(match)
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rule
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_rule_result_type), intent(in) :: r

        match = .false.
        if (rule%method /= method .or. trim(rule%token) /= token) return
        if (rule%normal_kind /= r%kind) return

        select case (r%kind)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET, SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN)
            match = rule%normal == r%value
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN)
            match = .false.
        case default
            match = .true.
        end select
    end function is_rule_for

    ! The rule of RULES whose signature is METHOD, TOKEN and R (0 if none).
    ! (rassoc SIGNATURE RULES)  Not a port.
    integer function find_rule(rules, method, token, r) result(k)
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rules(:)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_rule_result_type), intent(in) :: r

        do k = 1, size(rules)
            if (is_rule_for(rules(k), method, token, r)) return
        end do

        k = 0
    end function find_rule

    ! The answer RULE gives instead (its NEW).  Not a port.
    function rule_new(rule) result(r)
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rule
        type(simple_minded_indentation_engine_rule_result_type) :: r

        r%kind = rule%new_kind
        r%value = rule%new
    end function rule_new

    ! smie-config--advice: the answer R of `simple_minded_indentation_engine_rules_function' for METHOD
    ! and TOKEN replaced by the current buffer's rules and its mode's, and
    ! recorded for `simple_minded_indentation_engine_configuration_show_indent' while tracing.
    ! Port of smie.el:smie-config--advice.
    recursive subroutine configuration_advice(method, token, r)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_rule_result_type), intent(inout) :: r
        type(simple_minded_indentation_engine_configuration_rule_type) :: rule
        logical :: found
        integer :: k, j

        found = .false.
        k = local_index()

        if (k > 0) then
            if (allocated(locals(k)%buffer_rules)) then
                j = find_rule(locals(k)%buffer_rules, method, token, r)

                if (j > 0) then
                    rule = locals(k)%buffer_rules(j)
                    found = .true.
                end if
            end if

            if (.not. found .and. allocated(locals(k)%mode_rules)) then
                j = find_rule(locals(k)%mode_rules, method, token, r)

                if (j > 0) then
                    rule = locals(k)%mode_rules(j)
                    found = .true.
                end if
            end if
        end if

        if (found) r = rule_new(rule)
        if (tracing) call append_trace(method, token, r, found, rule)
    end subroutine configuration_advice

    ! Append a call of `simple_minded_indentation_engine_rules_function' to the trace.  Not a port.
    subroutine append_trace(method, token, r, rewritten, rule)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(simple_minded_indentation_engine_rule_result_type), intent(in) :: r
        logical, intent(in) :: rewritten
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rule
        type(trace_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (.not. allocated(trace)) allocate (trace(0))
        n = size(trace)
        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(trace(i)%signature%token, grown(i)%signature%token)
            grown(i)%signature%method = trace(i)%signature%method
            grown(i)%signature%kind = trace(i)%signature%kind
            grown(i)%signature%value = trace(i)%signature%value
            grown(i)%position = trace(i)%position
            grown(i)%rewritten = trace(i)%rewritten
            grown(i)%rule = trace(i)%rule
        end do

        grown(n + 1)%position = point()
        grown(n + 1)%signature%method = method
        grown(n + 1)%signature%token = token
        grown(n + 1)%signature%kind = r%kind
        grown(n + 1)%signature%value = r%value
        grown(n + 1)%rewritten = rewritten
        grown(n + 1)%rule = rule
        call move_alloc(grown, trace)
    end subroutine append_trace

    ! Install the rules of `simple_minded_indentation_engine_configuration' for the modes the current buffer's
    ! major mode derives from, as the mode hooks smie-config--setter adds
    ! do (the mode's own rules first).  Port of smie.el:smie-config--setter
    ! and smie.el:smie-config--mode-hook.
    recursive subroutine simple_minded_indentation_engine_configuration_mode_hook(status)
        type(status_type), intent(out) :: status
        type(major_mode_type), pointer :: mode
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: rules(:)
        integer :: i, k, depth

        status = status_type()
        if (.not. allocated(simple_minded_indentation_engine_configuration)) return
        if (size(simple_minded_indentation_engine_configuration) == 0) return
        if (.not. allocated(buffers(current)%major_mode)) return
        mode => major_mode_named(buffers(current)%major_mode)
        allocate (rules(0))
        depth = 0

        do while (associated(mode) .and. depth < 100)
            do i = 1, size(simple_minded_indentation_engine_configuration)
                if (trim(simple_minded_indentation_engine_configuration(i)%mode) == trim(mode%name)) rules = [rules, &
                                                                                  simple_minded_indentation_engine_configuration(i)]
            end do

            mode => mode%parent
            depth = depth + 1
        end do

        if (size(rules) == 0) return
        call make_local_entry(k)

        if (allocated(locals(k)%mode_rules)) then
            locals(k)%mode_rules = [rules, locals(k)%mode_rules]
        else
            call move_alloc(rules, locals(k)%mode_rules)
        end if
    end subroutine simple_minded_indentation_engine_configuration_mode_hook

    !> Add RULES as local indentation rules to use in this buffer.
    !> These replace any previous local rules, but supplement the rules
    !> specified in `simple_minded_indentation_engine_configuration'.  Port of smie.el:smie-config-local.
    subroutine simple_minded_indentation_engine_configuration_local(rules)
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rules(:)
        integer :: k

        call make_local_entry(k)
        locals(k)%buffer_rules = rules
    end subroutine simple_minded_indentation_engine_configuration_local

    ! The current buffer's local rules (empty for none).  Not a port.
    function buffer_rules() result(rules)
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: rules(:)
        integer :: k

        allocate (rules(0))
        k = local_index()

        if (k > 0) then
            if (allocated(locals(k)%buffer_rules)) rules = locals(k)%buffer_rules
        end if
    end function buffer_rules

    ! smie-config--get-trace: the indentation of the current line and the
    ! calls of `simple_minded_indentation_engine_rules_function' that computed it, the last first.
    ! Port of smie.el:smie-config--get-trace.
    recursive subroutine get_trace(indent, calls, status)
        type(simple_minded_indentation_engine_indent_type), intent(out) :: indent
        type(trace_entry_type), allocatable, intent(out) :: calls(:)
        type(status_type), intent(out) :: status
        type(trace_entry_type), allocatable :: saved(:)
        type(handle_type) :: m
        type(status_type) :: st
        logical :: saved_tracing
        integer(int64) :: original_point, back
        integer :: shortage

        call copy_marker_at(point(), m, .false., st)
        shortage = int(forward_line(0_int64))
        original_point = skip_characters_forward(" "//achar(9), st)
        if (allocated(trace)) call move_alloc(trace, saved)
        saved_tracing = tracing
        tracing = .true.
        allocate (trace(0))
        indent = simple_minded_indentation_engine_indent_calculate(status)
        tracing = saved_tracing
        call move_alloc(trace, calls)
        if (allocated(saved)) call move_alloc(saved, trace)
        calls = calls(size(calls):1:-1)
        call marker_position(m, back, st)
        call goto_character(back)
        call destroy_marker(m, st)
    end subroutine get_trace

    ! The name of a METHOD of `simple_minded_indentation_engine_rules_function' as a Fortran constant.
    ! Not a port.
    function method_text(method) result(s)
        integer, intent(in) :: method
        character(len=:), allocatable :: s

        select case (method)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_AFTER)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_AFTER"
        case (SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_BEFORE"
        case (SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_ELEMENT"
        case (SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_LIST_INTRO"
        case default
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_CLOSE_ALL"
        end select
    end function method_text

    ! An answer of `simple_minded_indentation_engine_rules_function' of KIND and VALUE (TOKEN for a
    ! token) as Fortran shows it.  Not a port.
    function result_text(kind, value, token) result(s)
        integer, intent(in) :: kind, value
        character(len=*), intent(in), optional :: token
        character(len=:), allocatable :: s
        character(len=64) :: text

        select case (kind)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET)
            write (text, "(i0)") value
            s = trim(text)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN)
            write (text, &
          '("simple_minded_indentation_engine_rule_result_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN, value=", i0, '// &
                   '")")') value
            s = trim(text)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN"
            if (present(token)) s = fortran_string(token)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES"
        case default
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE"
        end select
    end function result_text

    ! RULE as a Fortran structure constructor.  Not a port.
    function rule_text(rule) result(s)
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rule
        character(len=:), allocatable :: s
        character(len=40) :: text

        s = "simple_minded_indentation_engine_configuration_rule_type("
        if (len_trim(rule%mode) > 0) s = s//"mode="//fortran_string(trim(rule%mode))//", "
        write (text, '(", normal=", i0, ", new=", i0)') rule%normal, rule%new
        s = s//"method="//method_text(rule%method)//", token="//fortran_string(trim(rule%token))//trim(text)
        if (rule%normal_kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET) s = s//", normal_kind="//kind_text(rule%normal_kind)
        if (rule%new_kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET) s = s//", new_kind="//kind_text(rule%new_kind)
        s = s//")"
    end function rule_text

    ! A kind of answer as its constant's name.  Not a port.
    function kind_text(kind) result(s)
        integer, intent(in) :: kind
        character(len=:), allocatable :: s

        select case (kind)
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET"
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN"
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_TOKEN"
        case (SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES)
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES"
        case default
            s = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE"
        end select
    end function kind_text

    ! RULES as a Fortran array constructor.  Not a port.
    function rules_text(rules) result(s)
        type(simple_minded_indentation_engine_configuration_rule_type), intent(in) :: rules(:)
        character(len=:), allocatable :: s
        integer :: i

        s = "["

        do i = 1, size(rules)
            if (i > 1) s = s//", "
            s = s//rule_text(rules(i))
        end do

        s = s//"]"
    end function rules_text

    ! One call of a trace as `simple_minded_indentation_engine_configuration_show_indent' shows it.
    ! Not a port.
    function trace_text(entry) result(s)
        type(trace_entry_type), intent(in) :: entry
        character(len=:), allocatable :: s

        s = method_text(entry%signature%method)//" "//fortran_string(entry%signature%token)//" -> "// &
            result_text(entry%signature%kind, entry%signature%value)
        if (entry%rewritten) s = s//"(via "//result_text(entry%rule%normal_kind, entry%rule%normal)//")"
    end function trace_text

    !> Display the SIMPLE_MINDED_INDENTATION_ENGINE rules that are used to indent the current line.
    !> If ARGUMENT is true (a prefix argument), then move briefly point to the
    !> buffer position corresponding to each rule.  Port of
    !> smie.el:smie-config-show-indent.
    recursive subroutine simple_minded_indentation_engine_configuration_show_indent(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(trace_entry_type), allocatable :: calls(:)
        type(simple_minded_indentation_engine_indent_type) :: indent
        character(len=:), allocatable :: text
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: back
        integer :: i

        call get_trace(indent, calls, status)
        if (.not. status%ok()) return

        if (size(calls) == 0) then
            call message("No SMIE rules involved at this position")
        else if (.not. argument) then
            text = "Rules used: "

            do i = 1, size(calls)
                if (i > 1) text = text//", "
                text = text//trace_text(calls(i))
            end do

            call message(text)
        else
            call copy_marker_at(point(), m, .false., st)

            do i = 1, size(calls)
                call message(trace_text(calls(i)))
                call goto_character(calls(i)%position)
                call sit_for(int(1000*blink_matching_delay, int32))
            end do

            call marker_position(m, back, st)
            call goto_character(back)
            call destroy_marker(m, st)
        end if
    end subroutine simple_minded_indentation_engine_configuration_show_indent

    ! Not a port.
    recursive subroutine execute_show_simple_minded_indentation_engine_indent(status)
        type(status_type), intent(out) :: status

        call simple_minded_indentation_engine_configuration_show_indent(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_show_simple_minded_indentation_engine_indent

    ! smie-config--guess-value: the offset that gives the current line its
    ! current indentation through the rule for METHOD, TOKEN and the answer
    ! of kind KIND and VALUE (found false if none does).
    ! Port of smie.el:smie-config--guess-value.
    recursive subroutine guess_value(method, token, kind, value, offset, found, status)
        integer, intent(in) :: method, kind, value
        character(len=*), intent(in) :: token
        integer, intent(out) :: offset
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: saved(:)
        type(simple_minded_indentation_engine_configuration_rule_type) :: rule
        type(simple_minded_indentation_engine_indent_type) :: indent
        integer :: goal, k

        found = .false.
        offset = 0
        saved = buffer_rules()
        rule%method = method
        rule%token = token
        rule%normal_kind = kind
        rule%normal = value
        rule%new = 0
        call simple_minded_indentation_engine_configuration_local([rule, saved])
        goal = int(current_indentation())
        indent = simple_minded_indentation_engine_indent_calculate(status)

        if (status%ok() .and. indent%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
            offset = goal - indent%column
            call make_local_entry(k)
            locals(k)%buffer_rules(1)%new = offset
            indent = simple_minded_indentation_engine_indent_calculate(status)
            if (status%ok()) found = indent%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN .and. indent%column == goal
        end if

        call simple_minded_indentation_engine_configuration_local(saved)
    end subroutine guess_value

    !> Add a rule to adjust the indentation of current line.  Port of
    !> smie.el:smie-config-set-indent.
    recursive subroutine simple_minded_indentation_engine_configuration_set_indent(status)
        type(status_type), intent(out) :: status
        type(trace_entry_type), allocatable :: calls(:)
        type(simple_minded_indentation_engine_indent_type) :: indent
        type(completion_table_type) :: table
        type(value_type), allocatable :: choices(:)
        type(simple_minded_indentation_engine_configuration_rule_type) :: rule
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: rules(:)
        character(len=:), allocatable :: choice, default_text, answer
        character(len=11) :: digits
        integer :: i, k, method, kind, value, guess
        logical :: found, chosen

        call get_trace(indent, calls, status)
        if (.not. status%ok()) return

        if (size(calls) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No SMIE rules involved")
            return
        end if

        if (size(calls) == 1) then
            k = 1
        else
            allocate (choices(size(calls)))

            do i = 1, size(calls)
                choices(i) = value_type(text=method_word(calls(i)%signature%method)//" "//fortran_string(calls(i)%signature%token))
            end do

            table%list = make_list(choices)
            call completing_read("Adjust rule: ", table, choice, status, require_match=REQUIRE_MATCH_YES)
            if (.not. status%ok()) return
            k = 0

            do i = 1, size(calls)
                if (method_word(calls(i)%signature%method)//" "//fortran_string(calls(i)%signature%token) == choice) then
                    k = i
                    exit
                end if
            end do

            if (k == 0) return
        end if

        method = calls(k)%signature%method
        kind = calls(k)%signature%kind
        value = calls(k)%signature%value

        if (calls(k)%rewritten) then
            kind = calls(k)%rule%normal_kind
            value = calls(k)%rule%normal
        end if

        call guess_value(method, calls(k)%signature%token, kind, value, guess, found, status)
        if (.not. status%ok()) return

        if (found) then
            write (digits, "(i0)") guess
            default_text = trim(digits)
        else
            default_text = "SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE"
        end if

        call read_string(format_prompt("Adjust rule ("//method_text(method)//" "//fortran_string(calls(k)%signature%token)// &
                                       " -> "//result_text(kind, value)//") to", default_text)//": ", answer, status, &
                         default=default_text)
        if (.not. status%ok()) return
        rule%method = method
        rule%token = calls(k)%signature%token
        rule%normal_kind = kind
        rule%normal = value
        call parse_result(answer, rule%new_kind, rule%new, chosen)

        if (.not. chosen) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid read syntax: "//answer)
            return
        end if

        rules = buffer_rules()
        i = find_rule(rules, method, rule%token, simple_minded_indentation_engine_rule_result_type(kind=kind, value=value))
        if (i > 0) rules = [rules(1:i - 1), rules(i + 1:)]
        call simple_minded_indentation_engine_configuration_local([rule, rules])
        call message("Added rule "//method_text(method)//" "//fortran_string(trim(rule%token))//" -> "// &
                     result_text(rule%new_kind, rule%new)//" (via "//result_text(kind, value)//")")
    end subroutine simple_minded_indentation_engine_configuration_set_indent

    ! A METHOD as `simple_minded_indentation_engine_configuration_set_indent' offers it.  Not a port.
    function method_word(method) result(s)
        integer, intent(in) :: method
        character(len=:), allocatable :: s

        s = method_text(method)
        s = downcase_ascii(s(6:))
    end function method_word

    ! The answer TEXT names (as `result_text' writes it): its KIND and
    ! VALUE; OK false if TEXT is none of them.  Not a port.
    subroutine parse_result(text_in, kind, value, ok)
        character(len=*), intent(in) :: text_in
        integer, intent(out) :: kind, value
        logical, intent(out) :: ok
        character(len=:), allocatable :: text
        character(len=*), parameter :: COLUMN = &
                       "simple_minded_indentation_engine_rule_result_type(kind=SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN, value="
        integer :: ios

        text = trim(adjustl(text_in))
        kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
        value = 0
        ok = .true.

        select case (text)
        case ("SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE")
            kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_NONE
        case ("SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES")
            kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_YES
        case default
            if (index(text, COLUMN) == 1) then
                kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_COLUMN
                text = text(len(COLUMN) + 1:)

                if (index(text, ")") /= len(text)) then
                    ok = .false.
                    return
                end if

                text = text(1:len(text) - 1)
            end if

            if (verify(text, "+-0123456789") /= 0 .or. len(text) == 0) then
                ok = .false.
                return
            end if

            read (text, *, iostat=ios) value
            ok = ios == 0
        end select
    end subroutine parse_result

    ! The signature of a call: equal (as upstream's `equal').  Not a port.
    logical function is_same_signature(a, b)
        type(simple_minded_indentation_engine_signature_type), intent(in) :: a, b

        is_same_signature = .false.
        if (a%method /= b%method .or. a%kind /= b%kind .or. a%value /= b%value) return
        if (allocated(a%token) .neqv. allocated(b%token)) return

        if (allocated(a%token)) then
            if (a%token /= b%token .or. len(a%token) /= len(b%token)) return
        end if

        is_same_signature = .true.
    end function is_same_signature

    ! Whether the traces A and B are equal.  Not a port.
    logical function is_same_trace(a, b)
        type(simple_minded_indentation_engine_signature_type), intent(in) :: a(:), b(:)
        integer :: i

        is_same_trace = .false.
        if (size(a) /= size(b)) return

        do i = 1, size(a)
            if (.not. is_same_signature(a(i), b(i))) return
        end do

        is_same_trace = .true.
    end function is_same_trace

    ! smie-config--guess: the rules that would give the lines from
    ! BEGINNING to END their current indentation.
    ! Port of smie.el:smie-config--guess.
    recursive subroutine configuration_guess(beginning, end, rules, status)
        integer(int64), intent(in) :: beginning, end
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable, intent(out) :: rules(:)
        type(status_type), intent(out) :: status
        type(otrace_type), allocatable :: otraces(:)
        type(signature_data_type), allocatable :: signatures(:)
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: saved_buffer(:), saved_mode(:)
        type(trace_entry_type), allocatable :: calls(:)
        type(simple_minded_indentation_engine_signature_type), allocatable :: tr(:)
        type(simple_minded_indentation_engine_indent_type) :: indent
        type(progress_reporter_type) :: reporter
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: back, moved
        integer :: i, j, k, offset, shortage, have_mode, have_buffer
        logical :: double

        status = status_type()
        allocate (otraces(0), signatures(0))
        ! Bind the buffer's and the mode's rules to none.
        call make_local_entry(k)
        have_buffer = 0
        have_mode = 0

        if (allocated(locals(k)%buffer_rules)) then
            call move_alloc(locals(k)%buffer_rules, saved_buffer)
            have_buffer = 1
        end if

        if (allocated(locals(k)%mode_rules)) then
            call move_alloc(locals(k)%mode_rules, saved_mode)
            have_mode = 1
        end if

        call make_progress_reporter(reporter, "Analyzing the buffer", beginning, end)
        ! First, lets get the indentation traces and offsets for the region.
        call copy_marker_at(point(), m, .false., st)
        call goto_character(beginning)
        shortage = int(forward_line(0_int64))

        do while (point() < end)
            moved = skip_characters_forward(" "//achar(9), st)

            ! Skip empty lines.
            if (.not. is_end_of_line()) then
                call progress_reporter_update(reporter, point())
                call get_trace(indent, calls, status)
                if (.not. status%ok()) exit

                ! Skip `noindent' and friends.
                if (indent%kind == SIMPLE_MINDED_INDENTATION_ENGINE_INDENT_COLUMN) then
                    allocate (tr(size(calls)))

                    do i = 1, size(calls)
                        tr(i) = calls(i)%signature
                    end do

                    offset = int(current_indentation()) - indent%column
                    j = 0

                    do i = 1, size(otraces)
                        if (otraces(i)%offset == offset) then
                            if (is_same_trace(otraces(i)%signatures, tr)) then
                                j = i
                                exit
                            end if
                        end if
                    end do

                    if (j > 0) then
                        otraces(j)%count = otraces(j)%count + 1
                    else
                        call append_otrace(otraces, offset, tr)
                    end if

                    deallocate (tr)
                end if
            end if

            shortage = int(forward_line(1_int64))
            if (shortage /= 0) exit
        end do

        call marker_position(m, back, st)
        call goto_character(back)
        call destroy_marker(m, st)
        call progress_reporter_done(reporter)

        if (status%ok()) then
            ! Second, compile the data.  Our algorithm only knows how to adjust
            ! rules where the smie-rules-function returns an integer.  We call
            ! those "adjustable signatures".  We build a table mapping each adjustable
            ! signature to its data, describing the total number of times we
            ! encountered it, the offsets found, and the traces in which it was
            ! found.
            call message("Guessing...")

            do i = 1, size(otraces)
                double = .false.

                do j = 1, size(otraces(i)%signatures)
                    if (otraces(i)%signatures(j)%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET) cycle

                    do k = j + 1, size(otraces(i)%signatures)
                        if (is_same_signature(otraces(i)%signatures(j), otraces(i)%signatures(k))) double = .true.
                    end do
                end do

                ! Disregard those traces where an adjustable signature appears
                ! twice, because the rest of the code assumes that adding a
                ! rule to add an offset N will change the end result by N
                ! rather than 2*N or more.
                if (double) cycle

                do j = 1, size(otraces(i)%signatures)
                    ! Disregard those signatures that return nil or a column,
                    ! because our algorithm doesn't know how to adjust them
                    ! anyway.
                    if (otraces(i)%signatures(j)%kind /= SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET) cycle
                    k = 0

                    do offset = 1, size(signatures)
                        if (is_same_signature(signatures(offset)%signature, otraces(i)%signatures(j))) then
                            k = offset
                            exit
                        end if
                    end do

                    if (k == 0) then
                        call append_signature_data(signatures, otraces(i)%signatures(j), i)
                        k = size(signatures)
                    end if

                    signatures(k)%total = signatures(k)%total + otraces(i)%count
                    signatures(k)%cotraces = [i, signatures(k)%cotraces]
                    call add_offset(signatures(k), otraces(i)%offset, otraces(i)%count)
                end do
            end do

            ! Finally, guess the indentation rules.
            call configuration_guess_1(signatures, otraces, rules)
            call message("Guessing...done")
        end if

        call make_local_entry(k)
        if (allocated(locals(k)%buffer_rules)) deallocate (locals(k)%buffer_rules)
        if (allocated(locals(k)%mode_rules)) deallocate (locals(k)%mode_rules)
        if (have_buffer == 1) call move_alloc(saved_buffer, locals(k)%buffer_rules)
        if (have_mode == 1) call move_alloc(saved_mode, locals(k)%mode_rules)
    end subroutine configuration_guess

    ! Append the trace TR with OFFSET to OTRACES (count 1).  Not a port.
    subroutine append_otrace(otraces, offset, tr)
        type(otrace_type), allocatable, intent(inout) :: otraces(:)
        integer, intent(in) :: offset
        type(simple_minded_indentation_engine_signature_type), intent(in) :: tr(:)
        type(otrace_type), allocatable :: grown(:)
        integer :: i, n

        n = size(otraces)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%offset = otraces(i)%offset
            grown(i)%count = otraces(i)%count
            call move_alloc(otraces(i)%signatures, grown(i)%signatures)
        end do

        grown(n + 1)%offset = offset
        grown(n + 1)%count = 1
        grown(n + 1)%signatures = tr
        call move_alloc(grown, otraces)
    end subroutine append_otrace

    ! Append the data of SIGNATURE, first found in the trace OWNER.  Not a port.
    subroutine append_signature_data(signatures, signature, owner)
        type(signature_data_type), allocatable, intent(inout) :: signatures(:)
        type(simple_minded_indentation_engine_signature_type), intent(in) :: signature
        integer, intent(in) :: owner
        type(signature_data_type), allocatable :: grown(:)
        integer :: i, n

        n = size(signatures)
        allocate (grown(n + 1))

        do i = 1, n
            call move_signature_data(signatures(i), grown(i))
        end do

        grown(n + 1)%signature = signature
        grown(n + 1)%owner = owner
        allocate (grown(n + 1)%offsets(0), grown(n + 1)%counts(0), grown(n + 1)%cotraces(0))
        call move_alloc(grown, signatures)
    end subroutine append_signature_data

    ! Move the data FROM to TO.  Not a port.
    subroutine move_signature_data(from, to)
        type(signature_data_type), intent(inout) :: from
        type(signature_data_type), intent(out) :: to

        to%signature%method = from%signature%method
        to%signature%kind = from%signature%kind
        to%signature%value = from%signature%value
        call move_alloc(from%signature%token, to%signature%token)
        to%total = from%total
        to%owner = from%owner
        call move_alloc(from%offsets, to%offsets)
        call move_alloc(from%counts, to%counts)
        call move_alloc(from%cotraces, to%cotraces)
    end subroutine move_signature_data

    ! Add COUNT to the entry of OFFSET of DATA's offsets, made first if
    ! there is none.  Not a port.
    subroutine add_offset(data, offset, count)
        type(signature_data_type), intent(inout) :: data
        integer, intent(in) :: offset, count
        integer :: k

        k = findloc(data%offsets, offset, dim=1)

        if (k == 0) then
            data%offsets = [offset, data%offsets]
            data%counts = [0, data%counts]
            k = 1
        end if

        data%counts(k) = data%counts(k) + count
    end subroutine add_offset

    ! smie-config--guess-1: the rules from the data SIGNATURES of the traces
    ! OTRACES.  Port of smie.el:smie-config--guess-1.
    subroutine configuration_guess_1(signatures, otraces, rules)
        type(signature_data_type), intent(inout) :: signatures(:)
        type(otrace_type), intent(inout) :: otraces(:)
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable, intent(out) :: rules(:)
        integer, allocatable :: order(:), remaining(:), counts(:)
        type(simple_minded_indentation_engine_configuration_rule_type) :: rule
        integer :: i, j, k, n, s, offset, best, count, toffset, noffset, ko, kn, trace_index

        allocate (rules(0))
        n = size(signatures)
        ! Sort the signatures by frequency of occurrence (stably, from the
        ! reversed table order).
        order = [(n - i + 1, i=1, n)]

        do i = 2, n
            k = order(i)
            j = i - 1

            do while (j >= 1)
                if (signatures(order(j))%total >= signatures(k)%total) exit
                order(j + 1) = order(j)
                j = j - 1
            end do

            order(j + 1) = k
        end do

        remaining = order

        do while (size(remaining) > 0)
            s = remaining(1)
            remaining = remaining(2:)
            ! The offset found most often (the first of equals in the list).
            counts = signatures(s)%counts
            best = 1

            do i = 2, size(counts)
                if (counts(i) > counts(best)) best = i
            end do

            if (size(counts) == 0) cycle
            offset = signatures(s)%offsets(best)
            ! Nothing to do with this signature; indentation is correct already.
            if (offset == 0) cycle
            rule%method = signatures(s)%signature%method
            rule%token = signatures(s)%signature%token
            rule%normal_kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
            rule%normal = signatures(s)%signature%value
            rule%new_kind = SIMPLE_MINDED_INDENTATION_ENGINE_RULE_OFFSET
            rule%new = offset + signatures(s)%signature%value
            rules = [rule, rules]

            ! Adjust the rest of the data.
            do i = 1, size(signatures(s)%cotraces)
                trace_index = signatures(s)%cotraces(i)
                count = otraces(trace_index)%count
                toffset = otraces(trace_index)%offset
                otraces(trace_index)%offset = toffset - offset

                do j = 1, size(otraces(trace_index)%signatures)
                    ! (assq signature ssigs): the signature whose key is this very signature of
                    ! this trace, if not done yet.
                    k = 0

                    do kn = 1, size(remaining)
                        if (signatures(remaining(kn))%owner == trace_index) then
                            if (is_same_signature(signatures(remaining(kn))%signature, otraces(trace_index)%signatures(j))) then
                                k = remaining(kn)
                                exit
                            end if
                        end if
                    end do

                    if (k == 0) cycle
                    ko = findloc(signatures(k)%offsets, toffset, dim=1)
                    noffset = toffset - offset
                    call add_offset(signatures(k), noffset, 0)
                    ko = findloc(signatures(k)%offsets, toffset, dim=1)
                    kn = findloc(signatures(k)%offsets, noffset, dim=1)
                    if (ko > 0) signatures(k)%counts(ko) = signatures(k)%counts(ko) - count
                    signatures(k)%counts(kn) = signatures(k)%counts(kn) + count
                end do
            end do
        end do
    end subroutine configuration_guess_1

    !> Try and figure out this buffer's indentation settings.
    !> To save the result for future sessions, use `simple_minded_indentation_engine_configuration_save'.
    !> Port of smie.el:smie-config-guess.
    recursive subroutine simple_minded_indentation_engine_configuration_guess(status)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: configuration(:), existing(:)
        type(simple_minded_indentation_engine_grammar_type), pointer :: g
        logical :: yes

        g => grammar()

        if (g%unset) then
            call set_error(status, USER_ERROR_CODE, "This buffer does not seem to be using SMIE")
            return
        end if

        call configuration_guess(point_minimum(), point_maximum(), configuration, status)
        if (.not. status%ok()) return
        existing = buffer_rules()

        if (size(configuration) == 0) then
            call message("Nothing to change")
        else if (size(existing) == 0) then
            call simple_minded_indentation_engine_configuration_local(configuration)
            call message("Local rules set")
        else
            call read_y_or_n("Replace existing local config? ", yes, status)
            if (.not. status%ok()) return

            if (yes) then
                call message("Local rules replaced")
                call simple_minded_indentation_engine_configuration_local(configuration)
            else
                call read_y_or_n("Merge with existing local config? ", yes, status)
                if (.not. status%ok()) return

                if (yes) then
                    call message("Local rules adjusted")
                    call simple_minded_indentation_engine_configuration_local([configuration, existing])
                else
                    call message("Rules guessed: "//rules_text(configuration))
                end if
            end if
        end if
    end subroutine simple_minded_indentation_engine_configuration_guess

    !> Save local rules for use with this major mode.
    !> One way to generate local rules is the command `simple_minded_indentation_engine_configuration_guess'.
    !> Port of smie.el:smie-config-save (marking `simple_minded_indentation_engine_configuration' as set for
    !> Customize is Deferred).
    recursive subroutine simple_minded_indentation_engine_configuration_save(status)
        type(status_type), intent(out) :: status
        type(simple_minded_indentation_engine_configuration_rule_type), allocatable :: local(:), configuration(:), existing(:), &
                                                                                       others(:)
        character(len=:), allocatable :: mode
        logical :: yes, has_existing
        integer :: i, k, first

        status = status_type()
        local = buffer_rules()

        if (size(local) == 0) then
            call message("No local rules to save")
            return
        end if

        mode = ""
        if (allocated(buffers(current)%major_mode)) mode = buffers(current)%major_mode

        if (.not. allocated(simple_minded_indentation_engine_configuration)) then
            allocate (simple_minded_indentation_engine_configuration(0))
        end if

        allocate (existing(0), others(0))
        first = 0

        do i = 1, size(simple_minded_indentation_engine_configuration)
            if (trim(simple_minded_indentation_engine_configuration(i)%mode) == mode) then
                existing = [existing, simple_minded_indentation_engine_configuration(i)]
                if (first == 0) first = i
            end if
        end do

        has_existing = first > 0

        if (.not. has_existing) then
            call message("Local rules saved in `simple_minded_indentation_engine_configuration'")
            configuration = local
        else
            call read_y_or_n("Replace the existing mode's config? ", yes, status)
            if (.not. status%ok()) return

            if (yes) then
                call message("Mode rules replaced in `simple_minded_indentation_engine_configuration'")
                configuration = local
            else
                call read_y_or_n("Merge with existing mode's config? ", yes, status)
                if (.not. status%ok()) return

                if (.not. yes) then
                    call set_error(status, GENERIC_ERROR_CODE, "Abort")
                    return
                end if

                call message("Mode rules adjusted in `simple_minded_indentation_engine_configuration'")
                configuration = [local, existing]
            end if
        end if

        configuration(:)%mode = mode

        if (has_existing) then
            ! (setcdr existing config): the mode keeps its place.
            do i = 1, size(simple_minded_indentation_engine_configuration)
                if (i == first) others = [others, configuration]
                if (trim(simple_minded_indentation_engine_configuration(i)%mode) /= mode) others = [others, &
                                                                                  simple_minded_indentation_engine_configuration(i)]
            end do

            simple_minded_indentation_engine_configuration = others
        else
            simple_minded_indentation_engine_configuration = [configuration, simple_minded_indentation_engine_configuration]
        end if

        call make_local_entry(k)
        locals(k)%mode_rules = configuration
        if (allocated(locals(k)%buffer_rules)) deallocate (locals(k)%buffer_rules)
    end subroutine simple_minded_indentation_engine_configuration_save

    !> Give the variables their defaults and register the commands.
    !> Not a port.
    subroutine initialize_simple_minded_indentation_engine()
        simple_minded_indentation_engine_forward_token_function => simple_minded_indentation_engine_default_forward_token
        simple_minded_indentation_engine_backward_token_function => simple_minded_indentation_engine_default_backward_token
        simple_minded_indentation_engine_blink_matching_triggers = " "//achar(10)

        if (.not. allocated(simple_minded_indentation_engine_indent_functions%entries)) then
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_fixindent)
            call simple_minded_indentation_engine_indent_functions%append( &
                simple_minded_indentation_engine_indent_beginning_of_buffer)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_close)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_comment)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_comment_continue)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_comment_close)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_comment_inside)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_inside_string)
            call simple_minded_indentation_engine_indent_functions%append(indent_keyword_function)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_after_keyword)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_empty_line)
            call simple_minded_indentation_engine_indent_functions%append(simple_minded_indentation_engine_indent_exps)
        end if

        call append_locals_clearer(clear_simple_minded_indentation_engine_locals)
        call after_change_major_mode_hook%prepend(simple_minded_indentation_engine_configuration_mode_hook)
        commands%simple_minded_indentation_engine_configuration_show_indent%execute => &
            execute_show_simple_minded_indentation_engine_indent
        commands%simple_minded_indentation_engine_configuration_set_indent%execute => &
            simple_minded_indentation_engine_configuration_set_indent
        commands%simple_minded_indentation_engine_configuration_guess%execute => &
            simple_minded_indentation_engine_configuration_guess
        commands%simple_minded_indentation_engine_configuration_save%execute => simple_minded_indentation_engine_configuration_save
        commands%simple_minded_indentation_engine_close_block%execute => execute_simple_minded_indentation_engine_close_block
        commands%simple_minded_indentation_engine_down_group%execute => execute_simple_minded_indentation_engine_down_group
        commands%simple_minded_indentation_engine_forward_expression_command%execute => &
            execute_simple_minded_indentation_engine_forward_expression
        commands%simple_minded_indentation_engine_backward_expression_command%execute => &
            execute_simple_minded_indentation_engine_backward_expression
        commands%simple_minded_indentation_engine_next_indent_change%execute => simple_minded_indentation_engine_next_indent_change
    end subroutine initialize_simple_minded_indentation_engine
end module simple_minded_indentation_engine_mod
