! smie_fm.f90 -- the Simple Minded Indentation Engine.
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

!> SMIE: navigation and indentation from an operator precedence grammar.
!>
!> There are 4 different representations of a "grammar":
!> - a BNF table (`smie_bnf_type'), a list of rules (NONTERM RHS1 ... RHSn)
!>   where each RHS is a list of terminals (tokens) or nonterminals.  Any
!>   element which does not appear as the NONTERM of a rule is taken to be
!>   a terminal.
!> - a list of precedences (`smie_precs_type'), sorted from lowest to
!>   highest precedence, of precedence classes (ASSOCIATIVITY TERMINAL1 ..
!>   TERMINALn), where ASSOCIATIVITY is `SMIE_ASSOC', `SMIE_LEFT',
!>   `SMIE_RIGHT' or `SMIE_NONASSOC'.
!> - a 2 dimensional precedence table (`smie_prec2_type') recording the
!>   precedence relation (`SMIE_LT', `SMIE_EQ', `SMIE_GT' or none) between
!>   each pair of tokens.
!> - a precedence-level table (`smie_grammar_type'), giving for each token
!>   its left and right precedence level, which is what's used by the rest
!>   of SMIE.
!> The prec2 tables are kept in insertion order, like the hash tables
!> upstream iterates over, so the levels come out the same.
!>
!> A mode's lexer functions return the token in an allocatable string
!> (unallocated for none), its `smie_rules_function' its answer as a
!> `smie_rule_result_type'.  Positions are 0 for none.
!>
!> Deferred: smie-config (`smie_config', `smie_config_local',
!> `smie_config_show_indent', `smie_config_set_indent',
!> `smie_config_guess', `smie_config_save'), `smie_edebug', the
!> conflict warnings (display-warning, from warnings.el, is not ported;
!> conflicting constraints are resolved as upstream but not reported) and
!> `debug_on_error' in `smie_rule_separator' (a message is shown).
module smie_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_SCAN_ERROR, FM_USER_ERROR
    use print_fm, only: fortran_string, prin1_string
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, buffer_serial_live, current_buffer, buffer_chars_modified_tick
    use lisp_fm, only: command_proc, fixnump, xfixnum
    use data_fm, only: add_local_variables_killer, function_setting_default
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, bolp, eolp, line_beginning_position, &
                          line_end_position, buffer_substring, char_after, char_before
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, post_self_insert_hook
    use insdel_fm, only: insert
    use syntax_fm, only: forward_comment, skip_syntax_forward, skip_syntax_backward, skip_chars_forward, &
                         skip_chars_backward, syntax_entry_at, syntax_descriptor_type, parse_state_type, syntax_ppss, &
                         scan_error_data
    use search_fm, only: looking_at, string_match, match_beginning, match_end, re_search_forward, regexp_quote, &
                         SEARCH_NOERROR
    use subr_fm, only: looking_back
    use expression_motion_fm, only: forward_expression, backward_expression, backward_up_group, down_group, &
                                    forward_expression_function, &
                                    forward_expression_proc
    use indent_fm, only: current_column, current_indentation, indent_line_to, move_to_column, indent_according_to_mode, &
                         indent_line_function
    use fill_fm, only: current_fill_column
    use simple_fm, only: newline, newline_and_indent, reindent_then_newline_and_indent, blink_matching_open, &
                         blink_matching_check_mismatch, blink_matching_check_proc, &
                         set_local_blink_matching_check_function, current_blink_matching_check_function, &
                         blink_matching_check_function_local_p, kill_local_blink_matching_check_function, &
                         blink_matching_paren, BLINK_MATCHING_PAREN_NIL, normal_auto_fill_function, do_auto_fill, &
                         blink_matching_delay
    use newcomment_fm, only: comment_normalize_vars, comment_start_skip, comment_end_skip, comment_continue, &
                             comment_string_strip
    use paren_fm, only: show_paren_data_type, show_paren_data_proc, set_local_show_paren_data_function, &
                        current_show_paren_data_function, show_paren_data_function_local_p, show_paren_data_function
    use prog_mode_fm, only: prog_first_column
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use keyboard_fm, only: last_command_event
    use xdisp_fm, only: message
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    implicit none(type, external)
    private

    public :: smie_bnf_to_prec2, smie_precs_to_prec2, smie_merge_prec2s, smie_prec2_to_grammar, smie_set_prec2tab, &
              smie_bnf_closer_table, smie_bnf_classify
    public :: smie_default_forward_token, smie_default_backward_token, smie_next_expression, smie_backward_expression, &
              smie_forward_expression, smie_backward_expression_command, smie_forward_expression_command, smie_close_block, &
              smie_down_group, smie_blink_matching_check, smie_blink_matching_open, smie_forward_token, &
              smie_backward_token, smie_grammar_token, smie_closer_p, smie_opener_p
    public :: smie_rule_hanging_p, smie_rule_bolp, smie_indent_bolp_1, smie_indent_current_column, smie_rule_parent_p, &
              smie_rule_next_p, smie_rule_prev_p, smie_rule_sibling_p, smie_rule_parent, smie_rule_separator, &
              smie_indent_offset, smie_indent_forward_token, smie_indent_backward_token, smie_indent_virtual, &
              smie_indent_calculate, smie_indent_line, smie_indent_keyword, smie_indent_fixindent, smie_indent_bob, &
              smie_indent_close, smie_indent_comment, smie_indent_comment_continue, smie_indent_comment_close, &
              smie_indent_comment_inside, smie_indent_inside_string, smie_indent_after_keyword, &
              smie_indent_empty_line, smie_indent_exps, smie_auto_fill, smie_setup, smie_next_indent_change, &
              smie_parent, smie_parent_level, syms_of_smie, smie_default_hanging_eolp, smie_set_hanging_eolp_function, &
              smie_indent_proc, smie_token_proc, smie_rules_proc, smie_hanging_eolp_proc

    !> Precedence classes (ASSOCIATIVITY of a precs table).
    integer, parameter, public :: SMIE_LEFT = 1, SMIE_RIGHT = 2, SMIE_ASSOC = 3, SMIE_NONASSOC = 4
    !> Precedence relations: <, = and >.
    integer, parameter, public :: SMIE_LT = 1, SMIE_EQ = 2, SMIE_GT = 3
    !> Token classes of `smie_bnf_classify'.
    integer, parameter, public :: SMIE_NEITHER = 0, SMIE_OPENER = 1, SMIE_CLOSER = 2
    integer, parameter :: SMIE_NONTERMINAL = 3
    !> METHODs of `smie_rules_function'.
    integer, parameter, public :: SMIE_AFTER = 1, SMIE_BEFORE = 2, SMIE_ELEM = 3, SMIE_LIST_INTRO = 4, &
                                  SMIE_CLOSE_ALL = 5
    !> Kinds of a precedence level: a number, a list (the token does not
    !> bind on that side), SMIE_LEVEL_NIL and SMIE_LEVEL_T.
    integer, parameter, public :: SMIE_LEVEL_NIL = 0, SMIE_LEVEL_NUMBER = 1, SMIE_LEVEL_LIST = 2, SMIE_LEVEL_T = 3
    !> Kinds of a `smie_rules_function' answer: none (SMIE_NIL), an OFFSET, a column
    !> COLUMN, a token (for empty_line_token) and .true. (SMIE_RULE_T, for SMIE_LIST_INTRO
    !> and SMIE_CLOSE_ALL).
    integer, parameter, public :: SMIE_NIL = 0, SMIE_RULE_OFFSET = 1, SMIE_RULE_COLUMN = 2, SMIE_RULE_TOKEN = 3, &
                                  SMIE_RULE_T = 4
    !> Kinds of an indentation (an indentation function's value): none (SMIE_INDENT_NIL), a
    !> column (SMIE_INDENT_COLUMN), no indentation (SMIE_INDENT_NOINDENT).
    integer, parameter, public :: SMIE_INDENT_NIL = 0, SMIE_INDENT_COLUMN = 1, SMIE_INDENT_NOINDENT = 2

    !> A string of a list.
    type, public :: smie_string_type
        character(len=:), allocatable :: s
    end type smie_string_type

    !> One right-hand side of a BNF rule.
    type, public :: smie_rhs_type
        type(smie_string_type), allocatable :: items(:)
    end type smie_rhs_type

    !> A BNF rule: NONTERM and its right-hand sides.
    type, public :: smie_nonterminal_type
        character(len=:), allocatable :: nt
        type(smie_rhs_type), allocatable :: rhs(:)
    end type smie_nonterminal_type

    !> A BNF grammar.  `add' appends a right-hand side (an array of tokens
    !> and nonterminals, trailing blanks not part of them) to the rule of
    !> NT when that is the last rule, else starts a new rule.
    type, public :: smie_bnf_type
        type(smie_nonterminal_type), allocatable :: rules(:)
    contains
        procedure :: add => bnf_add
        !> Add the nonterminal NT with no right-hand sides, (NT).
        procedure :: add_nonterminal => bnf_add_nonterminal
    end type smie_bnf_type

    !> One precedence class (ASSOCIATIVITY OP ...).
    type, public :: smie_prec_type
        integer :: assoc = SMIE_ASSOC
        type(smie_string_type), allocatable :: ops(:)
    end type smie_prec_type

    !> A precs table: precedence classes sorted from lowest to highest
    !> precedence.  `add' appends a class.
    type, public :: smie_precs_type
        type(smie_prec_type), allocatable :: precs(:)
    contains
        procedure :: add => precs_add
    end type smie_precs_type

    !> A constraint (T1 REL T2) resolving a conflict.
    type, public :: smie_constraint_type
        character(len=:), allocatable :: t1, t2
        integer :: rel = SMIE_EQ
    end type smie_constraint_type

    !> A 2D precedence table: the relation `rel' between `x' and `y', and
    !> the :smie-open/close-alist and :smie-closer-alist of a table made
    !> from a BNF.
    type, public :: smie_prec2_type
        integer :: n = 0
        type(smie_string_type), allocatable :: x(:), y(:)
        integer, allocatable :: rel(:)
        logical :: has_open_close = .false.
        type(smie_string_type), allocatable :: class_tokens(:)
        integer, allocatable :: classes(:)
        logical :: has_closer_table = .false.
        type(smie_string_type), allocatable :: openers(:), closers(:)
    end type smie_prec2_type

    !> A precedence level.  Two list levels are the same (eq) only if
    !> they are the same level of the same token (`id').
    type, public :: smie_level_type
        integer :: kind = SMIE_LEVEL_NIL
        integer :: value = 0
        integer :: id = 0
    end type smie_level_type

    !> A precedence-level table (`smie_grammar'): (TOKEN LEFT-LEVEL
    !> RIGHT-LEVEL) for each token, and the :smie-closer-alist (OPENER and
    !> CLOSER for each).  `unset' is the value 'unset.
    type, public :: smie_grammar_type
        logical :: unset = .true.
        type(smie_string_type), allocatable :: tokens(:)
        type(smie_level_type), allocatable :: left(:), right(:)
        logical :: has_closer_table = .false.
        type(smie_string_type), allocatable :: openers(:), closers(:)
    end type smie_grammar_type

    !> What `smie_next_expression' returns: nothing (`null'), or LEVEL, POS and TOKEN
    !> (`token' unallocated for none, `pos' 0 too).
    type, public :: smie_expression_type
        logical :: null = .true.
        type(smie_level_type) :: level
        integer(int64) :: pos = 0
        character(len=:), allocatable :: token
    end type smie_expression_type

    !> A token and its levels: TOKEN, LEFT and RIGHT, or nothing (`null').
    type, public :: smie_toklevels_type
        logical :: null = .true.
        character(len=:), allocatable :: token
        type(smie_level_type) :: left, right
    end type smie_toklevels_type

    !> What a `smie_rules_function' returns (`kind' one of SMIE_NIL,
    !> SMIE_RULE_OFFSET, SMIE_RULE_COLUMN, SMIE_RULE_TOKEN, SMIE_RULE_T).
    type, public :: smie_rule_result_type
        integer :: kind = SMIE_NIL
        integer :: value = 0
        character(len=:), allocatable :: token
    end type smie_rule_result_type

    !> An indentation: none, a column or no indentation (SMIE_INDENT_NOINDENT).
    type, public :: smie_indent_type
        integer :: kind = SMIE_INDENT_NIL
        integer :: column = 0
    end type smie_indent_type

    abstract interface
        !> A token function (`smie_forward_token_function',
        !> `smie_backward_token_function'): move over the next token and
        !> return it (unallocated for none).  Not a port.
        subroutine smie_token_proc(token, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: token
            type(status_type), intent(out) :: status
        end subroutine smie_token_proc

        !> A `smie_rules_function': METHOD and ARG as documented there.
        !> Not a port.
        subroutine smie_rules_proc(method, arg, result, status)
            import :: smie_rule_result_type, status_type
            implicit none(type, external)
            integer, intent(in) :: method
            character(len=*), intent(in) :: arg
            type(smie_rule_result_type), intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine smie_rules_proc

        !> smie--hanging-eolp-function: move over what may follow a
        !> hanging token and tell whether the line ends there.  Not a port.
        logical function smie_hanging_eolp_proc()
            implicit none(type, external)
        end function smie_hanging_eolp_proc

        !> A function of `smie_indent_functions': the column point should
        !> be indented to, or none, or no indentation (SMIE_INDENT_NOINDENT).  It shouldn't move point.
        !> Not a port.
        subroutine smie_indent_proc(indent, status)
            import :: smie_indent_type, status_type
            implicit none(type, external)
            type(smie_indent_type), intent(out) :: indent
            type(status_type), intent(out) :: status
        end subroutine smie_indent_proc
    end interface

    ! One function of `smie_indent_functions', or the t of a buffer's own
    ! list.
    type :: indent_entry_type
        procedure(smie_indent_proc), pointer, nopass :: fn => null()
        logical :: is_t = .false.
    end type indent_entry_type

    ! A buffer's own `smie_indent_functions'.
    type :: indent_local_type
        integer(int64) :: serial = 0
        type(indent_entry_type), allocatable :: entries(:)
    end type indent_local_type

    !> The type of `smie_indent_functions': the default functions and
    !> buffers' own lists (where t stands for the default ones), changed
    !> with `add' and `remove' as add-hook and remove-hook do.
    type, public :: smie_indent_functions_type
        type(indent_entry_type), allocatable :: entries(:)
        type(indent_local_type), allocatable :: locals(:)
    contains
        !> Add FN (at the end with APPEND; to the current buffer's own list
        !> with LOCAL).
        procedure :: add => indent_functions_add
        !> Remove FN (from the current buffer's own list with LOCAL).
        procedure :: remove => indent_functions_remove
        procedure, private :: local_index => indent_functions_local_index
        procedure, private :: current => indent_functions_current
    end type smie_indent_functions_type

    ! A buffer's own SMIE variables.
    type :: smie_local_type
        integer(int64) :: serial = 0
        type(smie_grammar_type), pointer :: grammar => null()
        procedure(smie_rules_proc), pointer, nopass :: rules => null()
        logical :: has_rules = .false.
        procedure(smie_token_proc), pointer, nopass :: forward_token => null()
        procedure(smie_token_proc), pointer, nopass :: backward_token => null()
        logical :: has_closer_table = .false.
        logical :: has_triggers = .false.
        character(len=:), allocatable :: triggers
        ! smie--matching-block-data-cache: ((POS . TICK) . DATA).
        logical :: has_cache = .false.
        integer(int64) :: cache_pos = 0, cache_tick = 0
        logical :: has_cache_data = .false.
        type(show_paren_data_type) :: cache_data
        ! The functions SMIE's :around advice wraps (null for the global
        ! value at the time of the call).
        logical :: has_auto_fill = .false., auto_fill_local = .false.
        procedure(command_proc), pointer, nopass :: auto_fill => null()
        logical :: has_paren_data = .false., paren_data_local = .false.
        procedure(show_paren_data_proc), pointer, nopass :: paren_data => null()
        ! smie--hanging-eolp-function, when advised in the buffer.
        procedure(smie_hanging_eolp_proc), pointer, nopass :: hanging_eolp => null()
    end type smie_local_type

    !> List of token parsing info.
    !> This list is normally built by `smie_prec2_to_grammar'.
    !> Each element holds a TOKEN, a LEFT-LEVEL and a RIGHT-LEVEL.
    !> Parsing is done using an operator precedence parser.
    !> LEFT-LEVEL and RIGHT-LEVEL can be either numbers or SMIE_LEVEL_LIST, where SMIE_LEVEL_LIST
    !> means that this operator does not bind on the corresponding side,
    !> e.g. a LEFT-LEVEL of SMIE_LEVEL_NIL means this is a token that behaves somewhat like
    !> an open-paren, whereas a RIGHT-LEVEL of SMIE_LEVEL_NIL would correspond to something
    !> like a close-paren.  Port of smie.el smie-grammar (the default value,
    !> 'unset; `smie_setup' gives a buffer its own).
    type(smie_grammar_type), target, public :: smie_grammar

    !> Function to scan forward for the next token.
    !> Called with no argument should return a token and move to its end.
    !> If no token is found, return an unallocated or empty string.
    !> It can return an unallocated string when bumping into a parenthesis, which lets SMIE
    !> use syntax-tables to handle them in efficient C code.  Port of
    !> smie.el smie-forward-token-function (the default value).
    procedure(smie_token_proc), pointer, public :: smie_forward_token_function => null()

    !> Function to scan backward the previous token.
    !> Same calling convention as `smie_forward_token_function' except
    !> it should move backward to the beginning of the previous token.
    !> Port of smie.el smie-backward-token-function (the default value).
    procedure(smie_token_proc), pointer, public :: smie_backward_token_function => null()

    !> Chars which might trigger `blink_matching_open'.
    !> These can include the final chars of end-tokens, or chars that are
    !> typically inserted right after an end token.  Port of smie.el
    !> smie-blink-matching-triggers (the default value, a string of the
    !> characters).
    character(len=:), allocatable, public :: smie_blink_matching_triggers

    !> Whether SMIE should blink to matching opener for inner keywords.
    !> If true, it will blink not only for "begin..end" but also for "if...else".
    !> Port of smie.el smie-blink-matching-inners.
    logical, public :: smie_blink_matching_inners = .true.

    !> Basic amount of indentation.  Port of smie.el smie-indent-basic.
    integer, public :: smie_indent_basic = 4

    !> Function providing the indentation rules.
    !> It takes two arguments METHOD and ARG where the meaning of ARG
    !> and the expected return value depends on METHOD.
    !> METHOD can be:
    !> - `SMIE_AFTER', in which case ARG is a token and the function should return the
    !>   OFFSET to use for indentation after ARG.
    !> - `SMIE_BEFORE', in which case ARG is a token and the function should return the
    !>   OFFSET to use to indent ARG itself.
    !> - `SMIE_ELEM', in which case the function should return either:
    !>   - the offset to use to indent function arguments (ARG = "args")
    !>   - the basic indentation step (ARG = "basic").
    !>   - the token to use (when ARG = "empty_line_token") when we don't know how
    !>     to indent an empty line.
    !> - `SMIE_LIST_INTRO', in which case ARG is a token and the function should return
    !>   true if TOKEN is followed by a list of expressions (not separated by any
    !>   token) rather than an expression.
    !> - `SMIE_CLOSE_ALL', in which case ARG is a close-paren token at indentation and
    !>   the function should return true if it should be aligned with the opener
    !>   of the last close-paren token on the same line, if there are multiple.
    !>   Otherwise, it will be aligned with its own opener.
    !>
    !> When ARG is a token, the function is called with point just before that token.
    !> A return value of SMIE_NIL always means to fallback on the default behavior, so the
    !> function should return SMIE_NIL for arguments it does not expect.
    !>
    !> OFFSET can be:
    !> SMIE_NIL                     use the default indentation rule.
    !> SMIE_RULE_COLUMN, COLUMN     indent to column COLUMN.
    !> SMIE_RULE_OFFSET, NUMBER     offset by NUMBER, relative to a base token
    !>                              which is the current token for SMIE_AFTER and
    !>                              its parent for SMIE_BEFORE.
    !>
    !> The functions whose name starts with "smie_rule_" are helper functions
    !> designed specifically for use in this function.  Port of smie.el
    !> smie-rules-function (the default value; null is `ignore').
    procedure(smie_rules_proc), pointer, public :: smie_rules_function => null()

    !> How far a separator is outdented when nothing follows it on its
    !> line.  Port of smie.el smie-rule-separator-outdent.
    integer, public :: smie_rule_separator_outdent = 2

    !> Functions to compute the indentation.
    !> Each function is called with no argument, shouldn't move point, and should
    !> return either SMIE_INDENT_NIL if it has no opinion, or an integer representing the column
    !> to which that point should be aligned, if we were to reindent it.
    !> Port of smie.el smie-indent-functions.
    type(smie_indent_functions_type), public :: smie_indent_functions

    ! (NT . OPS) lists of the BNF conversion.
    type :: nt_ops_type
        character(len=:), allocatable :: nt
        type(smie_string_type), allocatable :: ops(:)
    end type nt_ops_type

    ! A path of smie-debug--prec2-cycle: (START . REST), REST reversed.
    type :: path_type
        integer, allocatable :: nodes(:)
    end type path_type

    type(smie_local_type), allocatable :: locals(:)

    ! smie-warning-count.
    integer :: warning_count = 0

    ! The dynamically bound smie--parent, smie--after and smie--token.
    type(smie_expression_type) :: smie__parent
    integer(int64) :: smie__after = 0
    character(len=:), allocatable :: smie__token

    ! The counter giving list levels their identities.
    integer :: level_ids = 0

contains

    ! ------------------------------------------------------------------
    ! Building the grammar data
    ! ------------------------------------------------------------------

    ! Add the rule NT -> ITEMS to the BNF grammar SELF.  Not a port.
    subroutine bnf_add(self, nt, items)
        class(smie_bnf_type), intent(inout) :: self
        character(len=*), intent(in) :: nt
        character(len=*), intent(in) :: items(:)
        type(smie_nonterminal_type), allocatable :: grown(:)
        type(smie_rhs_type), allocatable :: rhs(:)
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

        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(self%rules(i)%nt, grown(i)%nt)
            call move_alloc(self%rules(i)%rhs, grown(i)%rhs)
        end do

        grown(n + 1)%nt = nt
        allocate (grown(n + 1)%rhs(1))
        call make_items(items, grown(n + 1)%rhs(1))
        call move_alloc(grown, self%rules)
    end subroutine bnf_add

    ! Make NT a nonterminal of the BNF grammar SELF.  Not a port.
    subroutine bnf_add_nonterminal(self, nt)
        class(smie_bnf_type), intent(inout) :: self
        character(len=*), intent(in) :: nt
        type(smie_nonterminal_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(self%rules)) allocate (self%rules(0))
        n = size(self%rules)
        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(self%rules(i)%nt, grown(i)%nt)
            call move_alloc(self%rules(i)%rhs, grown(i)%rhs)
        end do

        grown(n + 1)%nt = nt
        allocate (grown(n + 1)%rhs(0))
        call move_alloc(grown, self%rules)
    end subroutine bnf_add_nonterminal

    ! The right-hand side RHS made of ITEMS.  Not a port.
    subroutine make_items(items, rhs)
        character(len=*), intent(in) :: items(:)
        type(smie_rhs_type), intent(out) :: rhs
        integer :: i

        allocate (rhs%items(size(items)))

        do i = 1, size(items)
            rhs%items(i)%s = trim(items(i))
        end do
    end subroutine make_items

    ! Add the precedence level of OPS, with associativity ASSOC, to SELF.
    ! Not a port.
    subroutine precs_add(self, assoc, ops)
        class(smie_precs_type), intent(inout) :: self
        integer, intent(in) :: assoc
        character(len=*), intent(in) :: ops(:)
        type(smie_prec_type), allocatable :: grown(:)
        integer :: n, i

        if (.not. allocated(self%precs)) allocate (self%precs(0))
        n = size(self%precs)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%assoc = self%precs(i)%assoc
            call move_alloc(self%precs(i)%ops, grown(i)%ops)
        end do

        grown(n + 1)%assoc = assoc
        allocate (grown(n + 1)%ops(size(ops)))

        do i = 1, size(ops)
            grown(n + 1)%ops(i)%s = trim(ops(i))
        end do

        call move_alloc(grown, self%precs)
    end subroutine precs_add

    ! The index of the pair (X . Y) in TABLE, 0 if none.  Not a port.
    integer function prec2_index(table, x, y) result(k)
        type(smie_prec2_type), intent(in) :: table
        character(len=*), intent(in) :: x, y

        do k = 1, table%n
            if (table%x(k)%s == x .and. len(table%x(k)%s) == len(x)) then
                if (table%y(k)%s == y .and. len(table%y(k)%s) == len(y)) return
            end if
        end do

        k = 0
    end function prec2_index

    ! (puthash (X . Y) REL TABLE).  Not a port.
    subroutine prec2_put(table, x, y, rel)
        type(smie_prec2_type), intent(inout) :: table
        character(len=*), intent(in) :: x, y
        integer, intent(in) :: rel
        type(smie_string_type), allocatable :: gx(:), gy(:)
        integer, allocatable :: grel(:)
        integer :: k, cap

        k = prec2_index(table, x, y)

        if (k > 0) then
            table%rel(k) = rel
            return
        end if

        if (.not. allocated(table%x)) then
            allocate (table%x(16), table%y(16), table%rel(16))
        else if (table%n == size(table%x)) then
            cap = 2*size(table%x)
            allocate (gx(cap), gy(cap), grel(cap))

            do k = 1, table%n
                call move_alloc(table%x(k)%s, gx(k)%s)
                call move_alloc(table%y(k)%s, gy(k)%s)
                grel(k) = table%rel(k)
            end do

            call move_alloc(gx, table%x)
            call move_alloc(gy, table%y)
            call move_alloc(grel, table%rel)
        end if

        table%n = table%n + 1
        table%x(table%n)%s = x
        table%y(table%n)%s = y
        table%rel(table%n) = rel
    end subroutine prec2_put

    !> Set the relation of X and Y in TABLE to VAL, unless it already has
    !> another one; then the relation in OVERRIDE, if it has one, wins and
    !> otherwise the old one stays (a conflict).  Port of
    !> smie.el:smie-set-prec2tab.
    subroutine smie_set_prec2tab(table, x, y, val, override)
        type(smie_prec2_type), intent(inout) :: table
        character(len=*), intent(in) :: x, y
        integer, intent(in) :: val
        type(smie_prec2_type), intent(in), optional :: override
        integer :: k, o

        k = prec2_index(table, x, y)

        if (k > 0) then
            if (table%rel(k) /= val) then
                o = 0
                if (present(override)) o = prec2_index(override, x, y)

                if (o > 0) then
                    table%rel(k) = override%rel(o)
                else
                    ! FIXME: The override is meant to resolve ambiguities,
                    ! but it also hides real conflicts.  (The warning
                    ! "Conflict: X OLD/VAL Y" is deferred.)
                    warning_count = warning_count + 1
                end if

                return
            end if
        end if

        call prec2_put(table, x, y, val)
    end subroutine smie_set_prec2tab

    !> Compute a 2D precedence table from a list of precedences.
    !> PRECS should be a list, sorted by precedence (e.g. "+" will
    !> come before "*"), of elements of the form (left OP ...)
    !> or (right OP ...) or (nonassoc OP ...) or (assoc OP ...).  All operators in
    !> one of those elements share the same precedence level and associativity.
    !> Port of smie.el:smie-precs->prec2.
    subroutine smie_precs_to_prec2(precs, prec2)
        type(smie_precs_type), intent(in) :: precs
        type(smie_prec2_type), intent(out) :: prec2
        integer :: p, i, j, q, selfrule, op1, op2, np

        np = 0
        if (allocated(precs%precs)) np = size(precs%precs)

        do p = 1, np
            associate (prec => precs%precs(p))
                do i = 1, size(prec%ops)
                    select case (prec%assoc)
                    case (SMIE_LEFT)
                        selfrule = SMIE_GT
                    case (SMIE_RIGHT)
                        selfrule = SMIE_LT
                    case (SMIE_ASSOC)
                        selfrule = SMIE_EQ
                    case default
                        selfrule = 0
                    end select

                    if (selfrule /= 0) then
                        do j = 1, size(prec%ops)
                            call smie_set_prec2tab(prec2, prec%ops(i)%s, prec%ops(j)%s, selfrule)
                        end do
                    end if

                    op1 = SMIE_LT
                    op2 = SMIE_GT

                    do q = 1, np
                        if (q == p) then
                            op1 = SMIE_GT
                            op2 = SMIE_LT
                        else
                            do j = 1, size(precs%precs(q)%ops)
                                call smie_set_prec2tab(prec2, prec%ops(i)%s, precs%precs(q)%ops(j)%s, op2)
                                call smie_set_prec2tab(prec2, precs%precs(q)%ops(j)%s, prec%ops(i)%s, op1)
                            end do
                        end if
                    end do
                end do
            end associate
        end do
    end subroutine smie_precs_to_prec2

    !> Merge the 2D precedence TABLES into PREC2.  Port of
    !> smie.el:smie-merge-prec2s.
    subroutine smie_merge_prec2s(tables, prec2, status)
        type(smie_prec2_type), intent(in) :: tables(:)
        type(smie_prec2_type), intent(out) :: prec2
        type(status_type), intent(out) :: status
        integer :: t, k

        status = status_type()

        if (size(tables) == 1) then
            prec2 = tables(1)
            return
        end if

        do t = 1, size(tables)
            do k = 1, tables(t)%n
                call smie_set_prec2tab(prec2, tables(t)%x(k)%s, tables(t)%y(k)%s, tables(t)%rel(k))
            end do

            if (tables(t)%has_open_close) then
                if (prec2%has_open_close) then
                    if (.not. same_classes(prec2, tables(t))) then
                        call set_error(status, FM_GENERIC_ERROR, "Conflicting values for :smie-open/close-alist property")
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
                        call set_error(status, FM_GENERIC_ERROR, "Conflicting values for :smie-closer-alist property")
                        return
                    end if
                else
                    prec2%has_closer_table = .true.
                    prec2%openers = tables(t)%openers
                    prec2%closers = tables(t)%closers
                end if
            end if
        end do
    end subroutine smie_merge_prec2s

    ! Whether the prec2 tables A and B have the same classes.  Not a port.
    logical function same_classes(a, b) result(same)
        type(smie_prec2_type), intent(in) :: a, b
        integer :: i

        same = size(a%classes) == size(b%classes)
        if (.not. same) return

        do i = 1, size(a%classes)
            same = a%classes(i) == b%classes(i) .and. str_eq(a%class_tokens(i)%s, b%class_tokens(i)%s)
            if (.not. same) return
        end do
    end function same_classes

    ! Whether the pairs (A1 . A2) and (B1 . B2) are equal.  Not a port.
    logical function same_pairs(a1, a2, b1, b2) result(same)
        type(smie_string_type), intent(in) :: a1(:), a2(:), b1(:), b2(:)
        integer :: i

        same = size(a1) == size(b1)
        if (.not. same) return

        do i = 1, size(a1)
            same = str_eq(a1(i)%s, b1(i)%s) .and. str_eq(a2(i)%s, b2(i)%s)
            if (.not. same) return
        end do
    end function same_pairs

    ! Whether A and B are the same string, lengths included.  Not a port.
    pure logical function str_eq(a, b)
        character(len=*), intent(in) :: a, b

        str_eq = len(a) == len(b)
        if (str_eq) str_eq = a == b
    end function str_eq

    ! Whether S is one of LIST.  Not a port.
    pure logical function member(s, list)
        character(len=*), intent(in) :: s
        type(smie_string_type), intent(in) :: list(:)
        integer :: i

        member = .false.

        do i = 1, size(list)
            if (str_eq(s, list(i)%s)) then
                member = .true.
                return
            end if
        end do
    end function member

    ! cl-pushnew S onto LIST (at the front, if it is not there).
    ! Not a port.
    subroutine pushnew(s, list)
        character(len=*), intent(in) :: s
        type(smie_string_type), allocatable, intent(inout) :: list(:)

        if (member(s, list)) return
        call push_front(s, list)
    end subroutine pushnew

    ! Push S onto the front of LIST.  Not a port.
    subroutine push_front(s, list)
        character(len=*), intent(in) :: s
        type(smie_string_type), allocatable, intent(inout) :: list(:)
        type(smie_string_type), allocatable :: grown(:)
        integer :: i

        allocate (grown(size(list) + 1))
        grown(1)%s = s

        do i = 1, size(list)
            call move_alloc(list(i)%s, grown(i + 1)%s)
        end do

        call move_alloc(grown, list)
    end subroutine push_front

    !> Convert the BNF grammar into a prec2 table.
    !> BNF is a list of nonterminal definitions of the form:
    !>   (NONTERM RHS1 RHS2 ...)
    !> where each RHS is a (non-empty) list of terminals (aka tokens) or non-terminals.
    !> Not all grammars are accepted:
    !> - an RHS cannot be an empty list (this is not needed, since SMIE allows all
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
    !> - precs tables (see `smie_precs_to_prec2') to resolve conflicting
    !>   constraints (PRECS),
    !> - constraints (T1 REL T2) where REL is one of SMIE_EQ, SMIE_LT or
    !>   SMIE_GT (CONSTRAINTS).
    !> Port of smie.el:smie-bnf->prec2.
    subroutine smie_bnf_to_prec2(bnf, prec2, status, precs, constraints)
        type(smie_bnf_type), intent(in) :: bnf
        type(smie_prec2_type), intent(out) :: prec2
        type(status_type), intent(out) :: status
        type(smie_precs_type), intent(in), optional :: precs(:)
        type(smie_constraint_type), intent(in), optional :: constraints(:)
        type(smie_string_type), allocatable :: nts(:), first_ops(:), last_ops(:), first_nts(:), last_nts(:)
        type(nt_ops_type), allocatable :: first_ops_table(:), last_ops_table(:), first_nts_table(:), last_nts_table(:)
        type(smie_prec2_type) :: over, override
        type(smie_prec2_type), allocatable :: tables(:)
        type(smie_string_type), allocatable :: ops(:)
        integer :: r, h, i, n, nrules, saved_count, k, j

        status = status_type()
        saved_count = warning_count
        warning_count = 0
        nrules = 0
        if (allocated(bnf%rules)) nrules = size(bnf%rules)
        ! Non-terminals.
        allocate (nts(nrules))

        do r = 1, nrules
            nts(r)%s = bnf%rules(r)%nt
        end do

        allocate (first_ops_table(0), last_ops_table(0), first_nts_table(0), last_nts_table(0))

        ! The override: constraints and precs tables.
        if (present(constraints)) then
            do i = 1, size(constraints)
                call smie_set_prec2tab(over, constraints(i)%t1, constraints(i)%t2, constraints(i)%rel)
            end do
        end if

        n = 0
        if (present(precs)) n = size(precs)
        allocate (tables(n + 1))
        tables(1) = over

        ! (The precs are pushed, so they are merged last first.)
        do i = 1, n
            call smie_precs_to_prec2(precs(n + 1 - i), tables(i + 1))
        end do

        call smie_merge_prec2s(tables, override, status)

        if (.not. status%ok()) then
            warning_count = saved_count
            return
        end if

        do r = 1, nrules
            allocate (first_ops(0), last_ops(0), first_nts(0), last_nts(0))

            do h = 1, size(bnf%rules(r)%rhs)
                associate (rhs => bnf%rules(r)%rhs(h)%items)
                    n = size(rhs)

                    if (n == 0) then
                        call set_error(status, FM_GENERIC_ERROR, "Wrong type argument: expected nonempty array")
                        warning_count = saved_count
                        return
                    end if

                    if (.not. member(rhs(1)%s, nts)) then
                        call pushnew(rhs(1)%s, first_ops)
                    else
                        call pushnew(rhs(1)%s, first_nts)

                        if (n > 1) then
                            ! If the first is not an OP we add the second (which
                            ! should be an OP if BNF is an "operator grammar").
                            ! Strictly speaking, this should only be done if the
                            ! first is a non-terminal which can expand to a phrase
                            ! without any OP in it, but checking doesn't seem worth
                            ! the trouble, and it lets the writer of the BNF
                            ! be a bit more sloppy by skipping uninteresting base
                            ! cases which are terminals but not OPs.
                            if (member(rhs(2)%s, nts)) then
                                call set_error(status, FM_GENERIC_ERROR, "Adjacent non-terminals: "//rhs(1)%s//" "// &
                                               rhs(2)%s)
                                warning_count = saved_count
                                return
                            end if

                            call pushnew(rhs(2)%s, first_ops)
                        end if
                    end if

                    if (.not. member(rhs(n)%s, nts)) then
                        call pushnew(rhs(n)%s, last_ops)
                    else
                        call pushnew(rhs(n)%s, last_nts)

                        if (n > 1) then
                            if (member(rhs(n - 1)%s, nts)) then
                                call set_error(status, FM_GENERIC_ERROR, "Adjacent non-terminals: "//rhs(n - 1)%s//" "// &
                                               rhs(n)%s)
                                warning_count = saved_count
                                return
                            end if

                            call pushnew(rhs(n - 1)%s, last_ops)
                        end if
                    end if
                end associate
            end do

            call push_table(first_ops_table, bnf%rules(r)%nt, first_ops)
            call push_table(last_ops_table, bnf%rules(r)%nt, last_ops)
            call push_table(first_nts_table, bnf%rules(r)%nt, first_nts)
            call push_table(last_nts_table, bnf%rules(r)%nt, last_nts)
        end do

        ! Compute all first-ops by propagating the initial ones we have
        ! now, according to first-nts.
        call propagate(first_nts_table, first_ops_table)
        ! Same thing for last-ops.
        call propagate(last_nts_table, last_ops_table)

        ! Now generate the 2D precedence table.
        do r = 1, nrules
            do h = 1, size(bnf%rules(r)%rhs)
                associate (rhs => bnf%rules(r)%rhs(h)%items)
                    do i = 1, size(rhs) - 1
                        if (member(rhs(i)%s, nts)) then
                            k = table_index(last_ops_table, rhs(i)%s)

                            do j = 1, size(last_ops_table(k)%ops)
                                call smie_set_prec2tab(prec2, last_ops_table(k)%ops(j)%s, rhs(i + 1)%s, SMIE_GT, override)
                            end do
                        else if (member(rhs(i + 1)%s, nts)) then
                            k = table_index(first_ops_table, rhs(i + 1)%s)

                            do j = 1, size(first_ops_table(k)%ops)
                                call smie_set_prec2tab(prec2, rhs(i)%s, first_ops_table(k)%ops(j)%s, SMIE_LT, override)
                            end do

                            if (i + 2 <= size(rhs)) then
                                if (.not. member(rhs(i + 2)%s, nts)) then
                                    call smie_set_prec2tab(prec2, rhs(i)%s, rhs(i + 2)%s, SMIE_EQ, override)
                                end if
                            end if
                        else
                            call smie_set_prec2tab(prec2, rhs(i)%s, rhs(i + 1)%s, SMIE_EQ, override)
                        end if
                    end do
                end associate
            end do
        end do

        ! Keep track of which tokens are openers/closer, so they can get a nil
        ! precedence in smie-prec2->grammar.
        call smie_bnf_classify(bnf, ops, prec2%classes)
        call move_alloc(ops, prec2%class_tokens)
        prec2%has_open_close = .true.
        call smie_bnf_closer_table(bnf, prec2%openers, prec2%closers)
        prec2%has_closer_table = .true.
        ! (The total of warnings is not reported: display-warning is
        ! deferred.)
        warning_count = saved_count

    contains

        subroutine push_table(table, nt, list)
            type(nt_ops_type), allocatable, intent(inout) :: table(:)
            character(len=*), intent(in) :: nt
            type(smie_string_type), allocatable, intent(inout) :: list(:)
            type(nt_ops_type), allocatable :: grown(:)
            integer :: m

            allocate (grown(size(table) + 1))
            grown(1)%nt = nt
            call move_alloc(list, grown(1)%ops)

            do m = 1, size(table)
                call move_alloc(table(m)%nt, grown(m + 1)%nt)
                call move_alloc(table(m)%ops, grown(m + 1)%ops)
            end do

            call move_alloc(grown, table)
        end subroutine push_table
    end subroutine smie_bnf_to_prec2

    ! (assoc NT TABLE): the first entry for NT.  Not a port.
    integer function table_index(table, nt) result(k)
        type(nt_ops_type), intent(in) :: table(:)
        character(len=*), intent(in) :: nt

        do k = 1, size(table)
            if (str_eq(table(k)%nt, nt)) return
        end do

        k = 0
    end function table_index

    ! Propagate the ops of OPS_TABLE along the nonterminals of NTS_TABLE
    ! until nothing changes.  Port of part of smie.el:smie-bnf->prec2.
    subroutine propagate(nts_table, ops_table)
        type(nt_ops_type), intent(in) :: nts_table(:)
        type(nt_ops_type), intent(inout) :: ops_table(:)
        type(smie_string_type), allocatable :: snapshot(:), cell(:)
        logical :: again
        integer :: t, i, j, k, src

        again = .true.

        do while (again)
            again = .false.

            do t = 1, size(nts_table)
                k = table_index(ops_table, nts_table(t)%nt)

                do i = 1, size(nts_table(t)%ops)
                    src = table_index(ops_table, nts_table(t)%ops(i)%s)
                    if (src == 0) cycle
                    snapshot = ops_table(src)%ops

                    do j = 1, size(snapshot)
                        ! (member op first-ops), where first-ops is (NT . OPS).
                        allocate (cell(size(ops_table(k)%ops) + 1))
                        cell(1)%s = ops_table(k)%nt
                        cell(2:) = ops_table(k)%ops

                        if (.not. member(snapshot(j)%s, cell)) then
                            again = .true.
                            call push_front(snapshot(j)%s, ops_table(k)%ops)
                        end if

                        deallocate (cell)
                    end do
                end do
            end do
        end do
    end subroutine propagate

    !> Build a closer-alist from a BNF table: OPENER and CLOSER of each entry, in
    !> OPENERS and CLOSERS.  The return value is in the same form as
    !> `smie_closer_table'.  NO_INNERS true means that inner keywords will
    !> be excluded from the table, e.g. the table will not include things
    !> like ("if" . "else").  Port of smie.el:smie-bnf--closer-alist.
    subroutine smie_bnf_closer_table(bnf, openers, closers, no_inners)
        type(smie_bnf_type), intent(in) :: bnf
        type(smie_string_type), allocatable, intent(out) :: openers(:), closers(:)
        logical, intent(in), optional :: no_inners
        type(smie_string_type), allocatable :: nts(:)
        integer :: r, h, i, n
        logical :: inners

        inners = .true.
        if (present(no_inners)) inners = .not. no_inners
        allocate (openers(0), closers(0), nts(size(bnf%rules)))

        do r = 1, size(bnf%rules)
            nts(r)%s = bnf%rules(r)%nt
        end do

        do r = 1, size(bnf%rules)
            do h = 1, size(bnf%rules(r)%rhs)
                associate (rhs => bnf%rules(r)%rhs(h)%items)
                    n = size(rhs)
                    if (n < 2) cycle
                    if (member(rhs(1)%s, nts)) cycle

                    if (.not. inners) then
                        if (.not. member(rhs(n)%s, nts)) call add_pair(rhs(1)%s, rhs(n)%s)
                    else
                        ! Reverse so that the "real" closer gets there first,
                        ! which is important for smie-close-block.
                        do i = n, 2, -1
                            if (.not. member(rhs(i)%s, nts)) call add_pair(rhs(1)%s, rhs(i)%s)
                        end do
                    end if
                end associate
            end do
        end do

    contains

        ! cl-pushnew, then nreverse: pairs in the order first added.
        subroutine add_pair(a, b)
            character(len=*), intent(in) :: a, b
            type(smie_string_type), allocatable :: go(:), gc(:)
            integer :: m

            do m = 1, size(openers)
                if (str_eq(openers(m)%s, a) .and. str_eq(closers(m)%s, b)) return
            end do

            allocate (go(size(openers) + 1), gc(size(closers) + 1))

            do m = 1, size(openers)
                call move_alloc(openers(m)%s, go(m)%s)
                call move_alloc(closers(m)%s, gc(m)%s)
            end do

            go(size(go))%s = a
            gc(size(gc))%s = b
            call move_alloc(go, openers)
            call move_alloc(gc, closers)
        end subroutine add_pair
    end subroutine smie_bnf_closer_table

    !> Return a table classifying terminals: each terminal of TOKENS can
    !> either be an opener (SMIE_OPENER) or a closer (SMIE_CLOSER); the
    !> others are left out.  Port of smie.el:smie-bnf--classify.
    subroutine smie_bnf_classify(bnf, tokens, classes)
        type(smie_bnf_type), intent(in) :: bnf
        type(smie_string_type), allocatable, intent(out) :: tokens(:)
        integer, allocatable, intent(out) :: classes(:)
        type(smie_string_type), allocatable :: table(:)
        integer, allocatable :: values(:)
        integer :: r, h, i, n

        allocate (table(0), values(0))

        ! Mark non-terminals.
        do r = 1, size(bnf%rules)
            call set_class(bnf%rules(r)%nt, SMIE_NONTERMINAL, .true.)
        end do

        do r = 1, size(bnf%rules)
            do h = 1, size(bnf%rules(r)%rhs)
                associate (rhs => bnf%rules(r)%rhs(h)%items)
                    n = size(rhs)

                    if (n == 1) then
                        call set_class(rhs(1)%s, SMIE_NEITHER, .false.)
                    else
                        call set_class(rhs(1)%s, SMIE_OPENER, .false.)

                        ! Remove internals.
                        do i = 2, n - 1
                            call set_class(rhs(i)%s, SMIE_NEITHER, .false.)
                        end do

                        call set_class(rhs(n)%s, SMIE_CLOSER, .false.)
                    end if
                end associate
            end do
        end do

        allocate (tokens(0), classes(0))

        do i = 1, size(table)
            if (values(i) == SMIE_CLOSER .or. values(i) == SMIE_OPENER) then
                call push_front(table(i)%s, tokens)
                classes = [values(i), classes]
            end if
        end do

    contains

        ! smie-bnf--set-class (or the puthash marking a nonterminal).
        subroutine set_class(token, class, force)
            character(len=*), intent(in) :: token
            integer, intent(in) :: class
            logical, intent(in) :: force
            type(smie_string_type), allocatable :: grown(:)
            integer :: m, prev

            do m = 1, size(table)
                if (str_eq(table(m)%s, token)) then
                    prev = values(m)

                    if (force) then
                        values(m) = class
                    else if (prev == class) then
                        continue
                    else if (prev == SMIE_NONTERMINAL) then
                        ! Non-terminal.
                        continue
                    else
                        ! (The warning "token TOKEN is both CLASS and PREV" is
                        ! deferred.)
                        values(m) = SMIE_NEITHER
                    end if

                    return
                end if
            end do

            allocate (grown(size(table) + 1))

            do m = 1, size(table)
                call move_alloc(table(m)%s, grown(m)%s)
            end do

            grown(size(grown))%s = token
            call move_alloc(grown, table)
            values = [values, class]
        end subroutine set_class
    end subroutine smie_bnf_classify

    !> Take a 2D precedence table and turn it into a table of precedence levels.
    !> PREC2 is a table as returned by `smie_precs_to_prec2' or
    !> `smie_bnf_to_prec2'.  Port of smie.el:smie-prec2->grammar.
    subroutine smie_prec2_to_grammar(prec2, grammar, status)
        type(smie_prec2_type), intent(in) :: prec2
        type(smie_grammar_type), intent(out) :: grammar
        type(status_type), intent(out) :: status
        ! The table, in creation order (upstream pushes, so its order is
        ! the reverse): each token's left and right "variables" (cells).
        type(smie_string_type), allocatable :: tokens(:)
        integer, allocatable :: lcell(:), rcell(:)
        ! The cells' values (`has' false is nil).
        integer, allocatable :: val(:)
        logical, allocatable :: has(:)
        ! Equality and < constraints, in push order (upstream's lists are
        ! the reverse).
        integer, allocatable :: eqx(:), eqy(:), cx(:), cy(:)
        logical, allocatable :: alive(:)
        integer :: ntok, ncell, k, a, b, x, y, i, e, j, m, c
        logical :: progress, in_rhs
        character(len=:), allocatable :: desc

        status = status_type()
        allocate (tokens(0), lcell(0), rcell(0), val(0), has(0), eqx(0), eqy(0), cx(0), cy(0))
        ntok = 0
        ncell = 0

        ! From `prec2' we construct a list of constraints between
        ! variables (aka "precedence levels").  These can be either
        ! equality constraints (in `eqs') or `<' constraints (in `csts').
        do k = 1, prec2%n
            a = token_index(prec2%x(k)%s)
            if (a == 0) a = new_token(prec2%x(k)%s)
            x = rcell(a)
            b = token_index(prec2%y(k)%s)
            if (b == 0) b = new_token(prec2%y(k)%s)
            y = lcell(b)

            select case (prec2%rel(k))
            case (SMIE_EQ)
                eqx = [eqx, x]
                eqy = [eqy, y]
            case (SMIE_LT)
                cx = [cx, x]
                cy = [cy, y]
            case (SMIE_GT)
                cx = [cx, y]
                cy = [cy, x]
            case default
                call set_error(status, FM_GENERIC_ERROR, "SMIE error: prec2 has an unknown relation")
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
                            val(cx(c)) = i
                            has(cx(c)) = .true.
                            i = i + 1
                        end if

                        alive(c) = .false.
                    end if
                end do
            end block

            if (.not. progress) then
                desc = describe_cycle()
                call set_error(status, FM_GENERIC_ERROR, "Can't resolve the precedence cycle: "//desc)
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
                val(eqy(e)) = i
                has(eqy(e)) = .true.
                i = i + 1
            end if

            val(eqx(e)) = val(eqy(e))
            has(eqx(e)) = .true.
        end do

        ! Finally, fill in the remaining vars (which did not appear on the
        ! left side of any < constraint).
        do m = ntok, 1, -1
            if (.not. has(lcell(m))) then
                val(lcell(m)) = i
                has(lcell(m)) = .true.
                i = i + 1
            end if

            if (.not. has(rcell(m))) then
                val(rcell(m)) = i
                has(rcell(m)) = .true.
                i = i + 1
            end if
        end do

        ! The grammar, in upstream's table order.
        grammar%unset = .false.
        allocate (grammar%tokens(ntok), grammar%left(ntok), grammar%right(ntok))

        do m = 1, ntok
            k = ntok + 1 - m
            grammar%tokens(m)%s = tokens(k)%s
            grammar%left(m) = smie_level_type(kind=SMIE_LEVEL_NUMBER, value=val(lcell(k)))
            grammar%right(m) = smie_level_type(kind=SMIE_LEVEL_NUMBER, value=val(rcell(k)))
        end do

        ! Mark closers and openers.
        if (prec2%has_open_close) then
            do k = 1, size(prec2%classes)
                m = smie_grammar_index(grammar, prec2%class_tokens(k)%s)
                ! `cons' can be nil for openers/closers which only contain
                ! "atomic" elements.
                if (m == 0) cycle

                select case (prec2%classes(k))
                case (SMIE_CLOSER)
                    call make_list_level(grammar%right(m))
                case (SMIE_OPENER)
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
                if (str_eq(tokens(t)%s, token)) return
            end do

            t = 0
        end function token_index

        integer function new_token(token) result(t)
            character(len=*), intent(in) :: token
            type(smie_string_type), allocatable :: grown(:)
            integer :: q

            allocate (grown(ntok + 1))

            do q = 1, ntok
                call move_alloc(tokens(q)%s, grown(q)%s)
            end do

            grown(ntok + 1)%s = token
            call move_alloc(grown, tokens)
            ntok = ntok + 1
            t = ntok
            lcell = [lcell, ncell + 1]
            rcell = [rcell, ncell + 2]
            ncell = ncell + 2
            val = [val, 0, 0]
            has = [has, .false., .false.]
        end function new_token

        ! smie-debug--describe-cycle of smie-debug--prec2-cycle: the
        ! tokens of each variable of the cycle (".TOKEN" for a left level,
        ! "TOKEN." for a right one) joined with "=", the cycle with " < ".
        function describe_cycle() result(text)
            character(len=:), allocatable :: text
            integer, allocatable :: found(:)
            type(smie_string_type), allocatable :: names(:), res(:)
            integer :: q, s, r

            call find_cycle(found)
            allocate (names(size(found)))

            do q = 1, size(found)
                allocate (res(0))

                do s = ntok, 1, -1
                    if (lcell(s) == found(q)) call push_front("."//tokens(s)%s, res)
                    if (rcell(s) == found(q)) call push_front(tokens(s)%s//".", res)
                end do

                names(q)%s = ""

                do r = 1, size(res)
                    if (r > 1) names(q)%s = names(q)%s//"="
                    names(q)%s = names(q)%s//res(r)%s
                end do

                deallocate (res)
            end do

            text = ""

            do q = 1, size(names) + 1
                if (q > 1) text = text//" < "
                text = text//names(mod(q - 1, size(names)) + 1)%s
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
                if (alive(q)) call add_path(paths, [cx(q), cy(q)], .false.)
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
                            call add_path(next, [paths(p)%nodes(1), cy(q), paths(p)%nodes(2:)], .true.)
                        end if
                    end do
                end do

                call move_alloc(next, paths)
            end do
        end subroutine find_cycle

        ! Add NODES to LIST, at the front with FRONT (push) or at the end.
        subroutine add_path(list, nodes, front)
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
        end subroutine add_path
    end subroutine smie_prec2_to_grammar

    ! (setf (car cons) (list (car cons))): LEVEL becomes a list level.
    ! Not a port.
    subroutine make_list_level(level)
        type(smie_level_type), intent(inout) :: level

        level_ids = level_ids + 1
        level%kind = SMIE_LEVEL_LIST
        level%id = level_ids
    end subroutine make_list_level

    !> The index of TOKEN in GRAMMAR (assoc), 0 if none.  Not a port.
    integer function smie_grammar_index(grammar, token) result(k)
        type(smie_grammar_type), intent(in) :: grammar
        character(len=*), intent(in) :: token

        k = 0
        if (.not. allocated(grammar%tokens)) return

        do k = 1, size(grammar%tokens)
            if (str_eq(grammar%tokens(k)%s, token)) return
        end do

        k = 0
    end function smie_grammar_index

    ! ------------------------------------------------------------------
    ! Levels
    ! ------------------------------------------------------------------

    ! Whether LEVEL is a number.  Not a port.
    pure logical function numberp(level)
        type(smie_level_type), intent(in) :: level

        numberp = level%kind == SMIE_LEVEL_NUMBER
    end function numberp

    ! (eq A B) of two levels.  Not a port.
    pure logical function level_eq(a, b)
        type(smie_level_type), intent(in) :: a, b

        level_eq = a%kind == b%kind
        if (.not. level_eq) return

        select case (a%kind)
        case (SMIE_LEVEL_NUMBER)
            level_eq = a%value == b%value
        case (SMIE_LEVEL_LIST)
            level_eq = a%id == b%id
        case default
            continue
        end select
    end function level_eq

    ! (or LEVEL t).  Not a port.
    pure function level_or_t(level) result(l)
        type(smie_level_type), intent(in) :: level
        type(smie_level_type) :: l

        l = level
        if (l%kind == SMIE_LEVEL_NIL) l%kind = SMIE_LEVEL_T
    end function level_or_t

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! The index in `locals' of the current buffer's (created if MAKE; 0 if
    ! none).  Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(smie_local_type), allocatable :: grown(:)
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
                locals(k) = smie_local_type(serial=buffers(cur)%serial)
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

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine kill_smie_locals()
        integer :: k

        k = local_index(.false.)
        if (k > 0) locals(k) = smie_local_type()
        k = smie_indent_functions%local_index()
        if (k > 0) smie_indent_functions%locals(k)%serial = 0
    end subroutine kill_smie_locals

    ! The current buffer's `smie_grammar'.  Not a port.
    function grammar() result(g)
        type(smie_grammar_type), pointer :: g
        integer :: k

        g => smie_grammar
        k = local_index(.false.)

        if (k > 0) then
            if (associated(locals(k)%grammar)) g => locals(k)%grammar
        end if
    end function grammar

    ! Whether the current buffer has a `smie_closer_table'.  Not a port.
    logical function closer_table_p()
        integer :: k
        type(smie_grammar_type), pointer :: g

        closer_table_p = .false.
        k = local_index(.false.)
        if (k == 0) return
        if (.not. locals(k)%has_closer_table) return
        g => grammar()
        closer_table_p = g%has_closer_table
    end function closer_table_p

    !> Whether TOKEN is a closer of `smie_closer_table' (rassoc).
    !> Not a port.
    logical function smie_closer_p(token)
        character(len=*), intent(in) :: token
        type(smie_grammar_type), pointer :: g

        smie_closer_p = .false.
        if (.not. closer_table_p()) return
        g => grammar()
        smie_closer_p = member(token, g%closers)
    end function smie_closer_p

    !> Whether TOKEN is an opener of `smie_closer_table' (assoc).
    !> Not a port.
    logical function smie_opener_p(token)
        character(len=*), intent(in) :: token
        type(smie_grammar_type), pointer :: g

        smie_opener_p = .false.
        if (.not. closer_table_p()) return
        g => grammar()
        smie_opener_p = member(token, g%openers)
    end function smie_opener_p

    ! Whether (OPENER . CLOSER) is in `smie_closer_table'.  Not a port.
    logical function closer_pair_p(opener, closer)
        character(len=*), intent(in) :: opener, closer
        type(smie_grammar_type), pointer :: g
        integer :: i

        closer_pair_p = .false.
        if (.not. closer_table_p()) return
        g => grammar()

        do i = 1, size(g%openers)
            if (str_eq(g%openers(i)%s, opener) .and. str_eq(g%closers(i)%s, closer)) then
                closer_pair_p = .true.
                return
            end if
        end do
    end function closer_pair_p

    !> TOKEN's levels in the current buffer's grammar (assoc TOKEN
    !> smie-grammar); `null' if it has none.  Not a port.
    function smie_grammar_token(token) result(t)
        character(len=*), intent(in) :: token
        type(smie_toklevels_type) :: t
        type(smie_grammar_type), pointer :: g
        integer :: k

        g => grammar()
        k = smie_grammar_index(g, token)
        if (k == 0) return
        t%null = .false.
        t%token = token
        t%left = g%left(k)
        t%right = g%right(k)
    end function smie_grammar_token

    !> Move over the next token with the current buffer's
    !> `smie_forward_token_function'.  Not a port.
    recursive subroutine smie_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        procedure(smie_token_proc), pointer :: fn
        integer :: k

        fn => smie_forward_token_function
        k = local_index(.false.)

        if (k > 0) then
            if (associated(locals(k)%forward_token)) fn => locals(k)%forward_token
        end if

        if (associated(fn)) then
            call fn(token, status)
        else
            call smie_default_forward_token(token, status)
        end if
    end subroutine smie_forward_token

    !> Move over the previous token with the current buffer's
    !> `smie_backward_token_function'.  Not a port.
    recursive subroutine smie_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        procedure(smie_token_proc), pointer :: fn
        integer :: k

        fn => smie_backward_token_function
        k = local_index(.false.)

        if (k > 0) then
            if (associated(locals(k)%backward_token)) fn => locals(k)%backward_token
        end if

        if (associated(fn)) then
            call fn(token, status)
        else
            call smie_default_backward_token(token, status)
        end if
    end subroutine smie_backward_token

    ! (funcall smie-rules-function METHOD ARG).  Not a port.
    recursive subroutine call_rules(method, arg, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: arg
        type(smie_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(smie_rules_proc), pointer :: fn
        integer :: k

        status = status_type()
        fn => smie_rules_function
        k = local_index(.false.)

        if (k > 0) then
            if (locals(k)%has_rules) fn => locals(k)%rules
        end if

        if (associated(fn)) call fn(method, arg, result, status)
    end subroutine call_rules

    ! The current buffer's `smie_blink_matching_triggers'.  Not a port.
    function triggers() result(t)
        character(len=:), allocatable :: t
        integer :: k

        t = smie_blink_matching_triggers
        k = local_index(.false.)

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

    !> Default value of `smie_backward_token_function'.  Port of
    !> smie.el:smie-default-backward-token.
    recursive subroutine smie_default_backward_token(token, status)
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
    end subroutine smie_default_backward_token

    !> Default value of `smie_forward_token_function'.  Port of
    !> smie.el:smie-default-forward-token.
    recursive subroutine smie_default_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: ok

        status = status_type()
        ok = forward_comment(point_max())
        p = point()

        if (skip_syntax_forward(".") == 0) then
            if (skip_syntax_forward("w_'") == 0) continue
        end if

        token = substring(p, point())
    end subroutine smie_default_forward_token

    ! smie--associative-p: in "a + b + c" we want to stop at each +, but in
    ! "if a then b elsif c then d else c" we don't want to stop at each keyword.
    ! To distinguish the two cases, we made smie-prec2->grammar choose
    ! different levels for each part of "if a then b else c", so that
    ! by checking if the left-level is equal to the right level, we can
    ! figure out that it's an associative operator.  Port of
    ! smie.el:smie--associative-p.
    pure logical function associative_p(left, right)
        type(smie_level_type), intent(in) :: left, right

        associative_p = level_eq(left, right)
    end function associative_p

    ! Signal `scan_error' with MESSAGE, A and B.  Not a port.
    subroutine signal_scan_error(status, message, a, b)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: message
        integer(int64), intent(in) :: a, b
        character(len=24) :: sa, sb

        write (sa, "(i0)") a
        write (sb, "(i0)") b
        call set_error(status, FM_SCAN_ERROR, fortran_string(message)//", "//trim(sa)//", "//trim(sb))
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
    !>   LEVEL a number, POS and TOKEN: we couldn't skip TOKEN because its back-level
    !>     is too high.  FORW-LEVEL is the forw-level of TOKEN,
    !>     POS is its start position in the buffer.
    !>   LEVEL SMIE_LEVEL_T, POS and TOKEN: same thing when we bump on the wrong side of a paren.
    !>     Instead of SMIE_LEVEL_T, the LEVEL can also be SMIE_LEVEL_LIST.
    !>   LEVEL SMIE_LEVEL_NIL, POS and TOKEN: we skipped over a paren-like pair.
    !>   `null' .true.: we skipped over an identifier, matched parentheses, ...
    !> Port of smie.el:smie-next-sexp.
    recursive subroutine smie_next_expression(forward, res, status, halfexpression, token)
        logical, intent(in) :: forward
        type(smie_expression_type), intent(out) :: res
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: halfexpression
        character(len=*), intent(in), optional :: token
        type(smie_level_type), allocatable :: lfw(:), lbk(:), last_fw(:), last_bk(:)
        type(smie_toklevels_type) :: toklevels
        type(smie_level_type) :: forw, back
        character(len=:), allocatable :: tok, message
        integer(int64) :: pos, epos1, epos, beg, end_pos
        integer :: n
        logical :: half, idx

        status = status_type()
        half = .false.
        if (present(halfexpression)) half = halfexpression
        allocate (lfw(0), lbk(0))

        if (present(token)) then
            toklevels = smie_grammar_token(token)

            if (.not. toklevels%null) then
                call push_levels(toklevels%left, toklevels%right)
            else
                idx = string_match("\`\s(\|\s)\(\)\'", token, status, inhibit_modify=.false.) > 0
                if (.not. status%ok()) return

                if (idx) then
                    if (match_end(1, status) > 0) then
                        call push_levels(smie_level_type(kind=SMIE_LEVEL_NUMBER, value=0), smie_level_type())
                    else
                        call push_levels(smie_level_type(), smie_level_type(kind=SMIE_LEVEL_NUMBER, value=0))
                    end if
                else
                    call set_error(status, FM_GENERIC_ERROR, "Unknown token: "//prin1_string(token))
                    return
                end if
            end if

            half = .false.
        end if

        do
            pos = point()

            if (forward) then
                call smie_forward_token(tok, status)
            else
                call smie_backward_token(tok, status)
            end if

            if (.not. status%ok()) return
            if (.not. allocated(tok)) tok = ""
            toklevels = smie_grammar_token(tok)

            if (toklevels%null) then
                if (len(tok) == 0) then
                    call next_expression(status)

                    if (status%code == FM_SCAN_ERROR) then
                        call scan_error_data(status, message, beg, end_pos)
                        status = status_type()
                        epos1 = beg

                        if (point() <= epos1) then
                            epos = end_pos
                        else
                            epos = epos1
                        end if

                        call goto_char(pos)
                        res%null = .false.
                        res%level%kind = SMIE_LEVEL_T
                        res%pos = epos

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

                    if (pos == point()) then
                        ! We did not move, so let's abort the loop.
                        res%null = .false.
                        res%level%kind = SMIE_LEVEL_T
                        res%pos = point()
                        return
                    end if
                end if
            else
                call forw_back(toklevels, forw, back)

                if (.not. numberp(back)) then
                    ! A token like a paren-close.
                    call push_levels(toklevels%left, toklevels%right)
                else
                    do while (size(lfw) > 0)
                        if (.not. top_forw_number()) exit
                        if (.not. back%value < top_forw()) exit
                        call pop_levels()
                    end do

                    if (size(lfw) == 0) then
                        if (half .and. numberp(forw)) then
                            call push_levels(toklevels%left, toklevels%right)
                        else
                            call bump(forw, tok, pos)
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
                            if (numberp(forw)) call push_levels(toklevels%left, toklevels%right)
                        else if (.not. numberp(forw)) then
                            ! We matched the topmost operator.  If the new operator
                            ! is the last in the corresponding BNF rule, we're done.
                            ! It is the last element, let's stop here.
                            res%null = .false.
                            res%level = smie_level_type()
                            res%pos = point()
                            res%token = tok
                            return
                        else if (.not. associative_p(toklevels%left, toklevels%right)) then
                            ! If the new operator is not the last in the BNF rule,
                            ! and is not associative, it's one of the inner operators
                            ! (like the "in" in "let .. in .. end"), so keep looking.
                            call push_levels(toklevels%left, toklevels%right)
                        else if (associative_last()) then
                            ! The new operator is associative.  Two cases:
                            ! - it's really just an associative operator (like + or ;)
                            !   in which case we should have stopped right before.
                            call bump(forw, tok, pos)
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
            type(smie_toklevels_type), intent(in) :: t
            type(smie_level_type), intent(out) :: f, b

            if (forward) then
                f = t%right
                b = t%left
            else
                f = t%left
                b = t%right
            end if
        end subroutine forw_back

        ! (push (LEFT RIGHT) levels).
        recursive subroutine push_levels(left, right)
            type(smie_level_type), intent(in) :: left, right

            if (forward) then
                lfw = [lfw, right]
                lbk = [lbk, left]
            else
                lfw = [lfw, left]
                lbk = [lbk, right]
            end if
        end subroutine push_levels

        recursive subroutine pop_levels()
            n = size(lfw)
            lfw = lfw(1:n - 1)
            lbk = lbk(1:n - 1)
        end subroutine pop_levels

        logical function top_forw_number()
            top_forw_number = numberp(lfw(size(lfw)))
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
                associative_last = associative_p(last_bk(m), last_fw(m))
            else
                associative_last = associative_p(last_fw(m), last_bk(m))
            end if
        end function associative_last

        ! (prog1 (list (or FORW t) (point) TOK) (goto-char POS)).
        recursive subroutine bump(f, t, p)
            type(smie_level_type), intent(in) :: f
            character(len=*), intent(in) :: t
            integer(int64), intent(in) :: p

            res%null = .false.
            res%level = level_or_t(f)
            res%pos = point()
            res%token = t
            call goto_char(p)
        end subroutine bump

        ! NEXT-SEXP: skip one expression.
        recursive subroutine next_expression(st)
            type(status_type), intent(out) :: st

            st = status_type()

            if (forward) then
                call forward_expression(1_int64, st)
            else if (bobp()) then
                ! Arguably backward-sexp should signal this error for us.
                call signal_scan_error(st, "Beginning of buffer", point(), point())
            else
                call backward_expression(1_int64, st)
            end if
        end subroutine next_expression
    end subroutine smie_next_expression

    !> Skip over one expression.
    !> HALFEXPRESSION if true, means skip over a partial expression if needed.  I.e. if the
    !> first token we see is an operator, skip over its left-hand-side argument.
    !> HALFEXPRESSION can also be a token (TOKEN), in which case we should skip the text
    !> assuming it is the left-hand-side argument of that token.
    !> Possible results in RES:
    !>   (LEFT-LEVEL POS TOKEN): we couldn't skip TOKEN because its right-level
    !>     is too high.  LEFT-LEVEL is the left-level of TOKEN,
    !>     POS is its start position in the buffer.
    !>   LEVEL SMIE_LEVEL_T, POS and TOKEN: same thing but for an open-paren or the beginning of buffer.
    !>     Instead of SMIE_LEVEL_T, the LEVEL can also be SMIE_LEVEL_LIST.
    !>   LEVEL SMIE_LEVEL_NIL, POS and TOKEN: we skipped over a paren-like pair.
    !>   `null' .true.: we skipped over an identifier, matched parentheses, ...
    !> Port of smie.el:smie-backward-sexp.
    recursive subroutine smie_backward_expression(res, status, halfexpression, token)
        type(smie_expression_type), intent(out) :: res
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: halfexpression
        character(len=*), intent(in), optional :: token

        call smie_next_expression(.false., res, status, halfexpression, token)
    end subroutine smie_backward_expression

    !> Skip over one expression.
    !> HALFEXPRESSION if true, means skip over a partial expression if needed.  I.e. if the
    !> first token we see is an operator, skip over its right-hand-side argument.
    !> HALFEXPRESSION can also be a token (TOKEN), in which case we should skip the text
    !> assuming it is the right-hand-side argument of that token.
    !> Possible results in RES:
    !>   (RIGHT-LEVEL POS TOKEN): we couldn't skip TOKEN because its left-level
    !>     is too high.  RIGHT-LEVEL is the right-level of TOKEN,
    !>     POS is its end position in the buffer.
    !>   LEVEL SMIE_LEVEL_T, POS and TOKEN: same thing but for a close-paren or the end of buffer.
    !>     Instead of SMIE_LEVEL_T, the LEVEL can also be SMIE_LEVEL_LIST.
    !>   LEVEL SMIE_LEVEL_NIL, POS and TOKEN: we skipped over a paren-like pair.
    !>   `null' .true.: we skipped over an identifier, matched parentheses, ...
    !> Port of smie.el:smie-forward-sexp.
    recursive subroutine smie_forward_expression(res, status, halfexpression, token)
        type(smie_expression_type), intent(out) :: res
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: halfexpression
        character(len=*), intent(in), optional :: token

        call smie_next_expression(.true., res, status, halfexpression, token)
    end subroutine smie_forward_expression

    ! ------------------------------------------------------------------
    ! Miscellaneous commands using the precedence parser
    ! ------------------------------------------------------------------

    !> Move backward through N logical elements.  Port of
    !> smie.el:smie-backward-sexp-command.
    recursive subroutine smie_backward_expression_command(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call smie_forward_expression_command(-n, status)
    end subroutine smie_backward_expression_command

    !> Move forward through N logical elements (the `forward_expression_function'
    !> of SMIE modes).  Port of smie.el:smie-forward-sexp-command.
    recursive subroutine smie_forward_expression_command(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        type(smie_expression_type) :: res
        integer(int64) :: left, pos
        logical :: forw, edge, was_local
        procedure(forward_expression_proc), pointer :: saved

        status = status_type()
        forw = n > 0
        left = n
        ! (let ((forward-sexp-function nil)) ...)
        was_local = forward_expression_function%local_p()
        saved => forward_expression_function%get()
        call forward_expression_function%set_local()

        do while (left /= 0)
            if (forw) then
                left = left - 1
            else
                left = left + 1
            end if

            pos = point()

            if (forw) then
                call smie_forward_expression(res, status, halfexpression=.true.)
            else
                call smie_backward_expression(res, status, halfexpression=.true.)
            end if

            if (.not. status%ok()) exit

            if (forw) then
                edge = eobp()
            else
                edge = bobp()
            end if

            if (.not. res%null .and. res%level%kind /= SMIE_LEVEL_NIL .and. pos == point() .and. .not. edge) then
                call signal_scan_error(status, "Containing expression ends prematurely", res%pos, res%pos)
                exit
            end if
        end do

        if (was_local) then
            call forward_expression_function%set_local(saved)
        else
            call forward_expression_function%kill_local()
        end if
    end subroutine smie_forward_expression_command

    !> The character of SYNTAX entry D matches the matching character of `syntax_after' at POS,
    !> as a string; "" if it has none.  Not a port.
    function matching_char(pos) result(s)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: s
        type(syntax_descriptor_type) :: d
        integer(int32) :: c
        logical :: found

        s = ""
        call char_after(c, found, pos)
        if (.not. found) return
        d = syntax_entry_at(int(c), pos)
        if (d%match >= 0) s = char_text(d%match)
    end function matching_char

    ! The UTF-8 text of character C.  Not a port.
    function char_text(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s

        if (c < 128) then
            s = achar(c)
        else if (c < 2048) then
            s = achar(192 + c/64)//achar(128 + iand(c, 63))
        else if (c < 65536) then
            s = achar(224 + c/4096)//achar(128 + iand(c/64, 63))//achar(128 + iand(c, 63))
        else
            s = achar(240 + c/262144)//achar(128 + iand(c/4096, 63))//achar(128 + iand(c/64, 63))// &
                achar(128 + iand(c, 63))
        end if
    end function char_text

    ! The characters of the UTF-8 string S.  Not a port.
    function string_chars_of(s) result(cs)
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
    end function string_chars_of

    ! The character before point, or -1 at the beginning (char-before).
    ! Not a port.
    integer function before_char()
        integer(int32) :: c
        logical :: found

        call char_before(c, found)
        before_char = -1
        if (found) before_char = int(c)
    end function before_char

    ! The character after point, or -1 at the end (char-after).  Not a port.
    integer function after_char()
        integer(int32) :: c
        logical :: found

        call char_after(c, found)
        after_char = -1
        if (found) after_char = int(c)
    end function after_char

    ! The character of `last_command_event', -1 if it is not one.
    ! Not a port.
    integer function command_char()
        command_char = -1
        if (fixnump(last_command_event)) command_char = int(xfixnum(last_command_event))
    end function command_char

    ! Whether C is one of the characters of the string S (memq).
    ! Not a port.
    logical function char_in(c, s)
        integer, intent(in) :: c
        character(len=*), intent(in) :: s

        char_in = any(string_chars_of(s) == c)
    end function char_in

    ! (save-excursion (skip-chars-backward " \t") (bolp)) and the like are
    ! written out where used.
    !> Close the closest surrounding block.  Port of smie.el:smie-close-block.
    recursive subroutine smie_close_block(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: closer, open
        type(smie_grammar_type), pointer :: g
        type(smie_toklevels_type) :: t
        integer, allocatable :: levels(:), seen(:)
        type(smie_string_type), allocatable :: found(:)
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
                closer = matching_char(point())
                exit body
            end if

            call smie_forward_token(open, status)
            if (.not. status%ok()) exit body
            if (.not. allocated(open)) open = ""
            g => grammar()
            closer = first_closer(open)
            if (allocated(closer)) exit body
            t = smie_grammar_token(open)

            ! Even if we improve the auto-computation of closers,
            ! there are still cases where we need manual
            ! intervention, e.g. for Octave's use of `until'
            ! as a pseudo-closer of `do'.
            if (t%null) then
                call set_error(status, FM_GENERIC_ERROR, "Doesn't look like a block")
                exit body
            end if

            if (numberp(t%left)) then
                call set_error(status, FM_GENERIC_ERROR, "Doesn't look like a block")
                exit body
            end if

            ! Now that smie-setup automatically sets smie-closer-alist
            ! from the BNF, this is not really needed any more.
            levels = [smie_grammar_index(g, open)]
            allocate (seen(0), found(0))

            do while (size(levels) > 0)
                level = levels(size(levels))
                levels = levels(1:size(levels) - 1)

                do k = 1, size(g%tokens)
                    if (.not. level_eq(g%right(level), g%left(k))) cycle
                    if (any(seen == k)) cycle
                    seen = [seen, k]

                    if (numberp(g%right(k))) then
                        levels = [levels, k]
                    else
                        call push_front(g%tokens(k)%s, found)
                    end if
                end do
            end do

            if (size(found) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No known closer for opener "//open)
                exit body
            end if

            ! What should we do if there are various closers?
            closer = found(1)%s
        end block body

        call goto_char(here)
        if (.not. status%ok()) return
        moved = skip_chars_backward(" "//achar(9), st)
        i = 0
        if (.not. bolp()) i = 1
        call goto_char(here)

        if (i == 1) then
            call newline(status)
            if (.not. status%ok()) return
        end if

        call insert(closer, status)
        if (.not. status%ok()) return
        here = point()
        moved = skip_chars_forward(" "//achar(9), st)
        matched = eolp()
        call goto_char(here)

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

            if (.not. closer_table_p()) return

            do m = 1, size(g%openers)
                if (str_eq(g%openers(m)%s, o)) then
                    c = g%closers(m)%s
                    return
                end if
            end do
        end function first_closer
    end subroutine smie_close_block

    ! `smie_close_block' as a command.
    recursive subroutine cmd_smie_close_block(status)
        type(status_type), intent(out) :: status

        call smie_close_block(status)
    end subroutine cmd_smie_close_block

    !> Move forward down one level paren-like blocks.  Like `down_group'.
    !> With argument ARG, do this that many times.
    !> A negative argument means move backward but still go down a level.
    !> This command assumes point is not in a string or comment.  Port of
    !> smie.el:smie-down-list.
    recursive subroutine smie_down_group(arg_in, status)
        integer(int64), intent(in) :: arg_in
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token
        type(smie_toklevels_type) :: levels
        integer(int64) :: arg, start, inc, pos, end_pos
        integer :: offset
        logical :: paren, more

        status = status_type()
        arg = arg_in
        start = point()
        inc = 1
        offset = 0

        if (arg < 0) then
            inc = -1
            offset = 1
        end if

        do while (arg /= 0)
            arg = arg - inc

            do
                pos = point()

                if (inc < 0) then
                    call smie_backward_token(token, status)
                else
                    call smie_forward_token(token, status)
                end if

                if (.not. status%ok()) return
                if (.not. allocated(token)) token = ""
                levels = smie_grammar_token(token)

                if (len(token) == 0) then
                    if (inc < 0) then
                        paren = looking_back("\s(\|\s)", status, point() - 1)
                    else
                        paren = looking_at("\s(\|\s)", status)
                    end if

                    if (.not. status%ok()) return

                    if (paren) then
                        ! Go back to `start' in case of an error.  This presumes
                        ! none of the token we've found until now include a ( or ).
                        call goto_char(start)
                        call down_group(inc, status)
                        if (.not. status%ok()) return
                        more = .false.
                    else
                        call forward_expression(inc, status)
                        if (.not. status%ok()) return
                        more = point() /= pos
                    end if
                else if (.not. levels%null .and. .not. numberp(side(levels, 1 + offset))) then
                    more = .false.
                else if (.not. levels%null .and. .not. numberp(side(levels, 2 - offset))) then
                    end_pos = point()
                    call goto_char(start)
                    call signal_scan_error(status, "Containing expression ends prematurely", pos, end_pos)
                    return
                else
                    more = .true.
                end if

                if (.not. more) exit
            end do
        end do
    end subroutine smie_down_group

    ! (nth N LEVELS) of (TOKEN LEFT RIGHT): 1 is LEFT, 2 is RIGHT.
    ! Not a port.
    function side(levels, n) result(l)
        type(smie_toklevels_type), intent(in) :: levels
        integer, intent(in) :: n
        type(smie_level_type) :: l

        if (n == 1) then
            l = levels%left
        else
            l = levels%right
        end if
    end function side

    ! `smie_down_group' as a command ("p").
    recursive subroutine cmd_smie_down_group(status)
        type(status_type), intent(out) :: status

        call smie_down_group(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_smie_down_group

    ! `smie_forward_expression_command' as a command ("^p").
    recursive subroutine cmd_smie_forward_expression_command(status)
        type(status_type), intent(out) :: status

        call smie_forward_expression_command(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_smie_forward_expression_command

    ! `smie_backward_expression_command' as a command ("^p").
    recursive subroutine cmd_smie_backward_expression_command(status)
        type(status_type), intent(out) :: status

        call smie_backward_expression_command(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_smie_backward_expression_command

    !> The `blink_matching_check_function' of SMIE modes: whether the
    !> opener at START and the closer before END do not match.  Port of
    !> smie.el:smie-blink-matching-check.
    recursive logical function smie_blink_matching_check(start, end) result(mismatch)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable :: ender, starter
        type(status_type) :: st
        integer(int64) :: here

        here = point()
        call goto_char(end)
        call smie_backward_token(ender, st)
        mismatch = .false.

        if (.not. allocated(ender)) then
            mismatch = blink_matching_check_mismatch(start, end)
        else if (.not. smie_closer_p(ender)) then
            ! This is not one of the begin..end we know how to check.
            mismatch = blink_matching_check_mismatch(start, end)
        else if (start == 0) then
            mismatch = .true.
        else
            call goto_char(start)
            call smie_forward_token(starter, st)
            if (.not. allocated(starter)) starter = ""
            mismatch = .not. closer_pair_p(starter, ender)
        end if

        call goto_char(here)
    end function smie_blink_matching_check

    !> Blink the matching opener when applicable.
    !> This uses SMIE's tables and is expected to be placed on
    !> `post_self_insert_hook'.  Port of smie.el:smie-blink-matching-open.
    recursive subroutine smie_blink_matching_open(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token, trig
        type(parse_state_type) :: ppss
        type(smie_toklevels_type) :: t
        integer(int64) :: pos, here, moved, opoint
        integer :: lce
        logical :: ok, was_local
        procedure(blink_matching_check_proc), pointer :: saved
        type(status_type) :: st

        status = status_type()
        ! Position after the close token.
        pos = point()
        lce = command_char()
        if (blink_matching_paren == BLINK_MATCHING_PAREN_NIL) return
        ! Optimization.
        if (.not. closer_table_p()) return
        ! Sanity check.
        ok = before_char() == lce

        if (.not. ok) then
            here = point()
            moved = skip_chars_backward(" "//achar(9), st)
            pos = point()
            ok = before_char() == lce

            if (.not. ok) then
                moved = skip_chars_backward(" "//achar(10)//achar(9), st)
                pos = point()
                ok = before_char() == lce
            end if

            call goto_char(here)
        end if

        if (.not. ok) return
        trig = triggers()
        if (.not. char_in(lce, trig)) return
        here = point()
        call syntax_ppss(ppss)
        call goto_char(here)
        if (ppss%comstr_start /= 0) return
        ! (save-excursion ...)
        opoint = point()

        body: block
            call smie_backward_token(token, status)
            if (.not. status%ok()) exit body
            if (.not. allocated(token)) token = ""

            if (point() == pos - 1 .and. char_count_of(token) == 1 .and. .not. smie_closer_p(token)) then
                ! The trigger char is itself a token but is not one of the
                ! closers (e.g. ?\; in Octave mode), so go back to the
                ! previous token.
                pos = point()
                call smie_backward_token(token, status)
                if (.not. status%ok()) exit body
                if (.not. allocated(token)) token = ""
            end if

            if (.not. smie_closer_p(token)) exit body
            ! We're after a close token.  Let's still make sure we
            ! didn't skip a comment to find that token.
            call smie_forward_token(trig, status)
            if (.not. status%ok()) exit body
            here = point()
            ! Skip the trigger char, if applicable.
            if (after_char() == lce) call forward_char(st, 1_int64)
            ! Skip any auto-indentation, if applicable.
            if (lce == 10) moved = skip_chars_forward(" "//achar(9), st)
            ok = point() >= pos
            call goto_char(here)
            if (.not. ok) exit body
            ! If token ends with a trigger char, don't blink for
            ! anything else than this trigger char, lest we'd blink
            ! both when inserting the trigger char and when
            ! inserting a subsequent trigger char like SPC.
            ok = before_char() == lce
            if (.not. ok) ok = .not. char_in(before_char(), triggers())
            if (.not. ok) exit body

            ! FIXME: For octave's "switch ... case ... case" we flash
            ! `switch' at the end of the first `case' and we burp
            ! "mismatch" at the end of the second `case'.
            if (.not. smie_blink_matching_inners) then
                t = smie_grammar_token(token)

                if (.not. t%null) then
                    if (numberp(t%right)) exit body
                end if
            end if

            ! The major mode might set blink-matching-check-function
            ! buffer-locally so that interactive calls to
            ! blink-matching-open work right, but let's not presume
            ! that's the case.
            was_local = blink_matching_check_function_local_p()
            saved => current_blink_matching_check_function()
            call set_local_blink_matching_check_function(smie_blink_matching_check)
            call blink_matching_open(status)

            if (was_local) then
                call set_local_blink_matching_check_function(saved)
            else
                call kill_local_blink_matching_check_function()
            end if
        end block body

        call goto_char(opoint)
    end subroutine smie_blink_matching_open

    ! The number of characters of S.  Not a port.
    integer function char_count_of(s)
        character(len=*), intent(in) :: s

        char_count_of = size(string_chars_of(s))
    end function char_count_of

    ! smie--opener/closer-at-point: (OPENER TOKEN START END), or nil
    ! (FOUND false).  OPENER is true if TOKEN is an opener and false if
    ! it's a closer.  Port of smie.el:smie--opener/closer-at-point.
    recursive subroutine opener_closer_at_point(found, opener, token, start_pos, end_pos, status)
        logical, intent(out) :: found, opener
        character(len=:), allocatable, intent(out) :: token
        integer(int64), intent(out) :: start_pos, end_pos
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: ignored, btok, atok
        integer(int64) :: start, bend, aend

        found = .false.
        opener = .false.
        start_pos = 0
        end_pos = 0
        start = point()
        ! Move to a previous position outside of a token.
        call smie_backward_token(ignored, status)
        if (.not. status%ok()) return
        ! Move to the end of the token before point.
        call smie_forward_token(btok, status)
        if (.not. status%ok()) return
        if (.not. allocated(btok)) btok = ""
        bend = point()

        if (bend >= start .and. smie_closer_p(btok)) then
            ! Token before point is a closer?
            call smie_backward_token(ignored, status)
            if (.not. status%ok()) return

            if (point() < start) then
                found = .true.
                token = btok
                start_pos = point()
                end_pos = bend
                call goto_char(bend)
            end if
        else if (bend > start .and. smie_opener_p(btok)) then
            ! Token around point is an opener?
            call smie_backward_token(ignored, status)
            if (.not. status%ok()) return

            if (point() <= start) then
                found = .true.
                opener = .true.
                token = btok
                start_pos = point()
                end_pos = bend
            end if
        else if (bend <= start) then
            call smie_forward_token(atok, status)
            if (.not. status%ok()) return
            if (.not. allocated(atok)) atok = ""
            aend = point()
            ! Hopefully shouldn't happen.
            if (aend < start) return

            ! Token after point is a closer?
            if (smie_opener_p(atok)) then
                call smie_backward_token(ignored, status)
                if (.not. status%ok()) return

                if (point() <= start) then
                    found = .true.
                    opener = .true.
                    token = atok
                    start_pos = point()
                    end_pos = aend
                end if
            end if
        end if
    end subroutine opener_closer_at_point

    ! The `show_paren_data_function' of SMIE modes: smie--matching-block-data
    ! around the function it advises.  Port of
    ! smie.el:smie--matching-block-data.
    recursive subroutine smie_matching_block_data(found, data)
        logical, intent(out) :: found
        type(show_paren_data_type), intent(out) :: data
        type(status_type) :: st
        type(parse_state_type) :: ppss
        type(smie_expression_type) :: sdata
        type(smie_toklevels_type) :: ht, tt
        character(len=:), allocatable :: htok, ignored
        integer(int64) :: tick, hbeg, hend, tend, tbeg, here, pt
        integer :: k
        logical :: hfound, hopener, tfound, tmatch

        found = .false.
        k = local_index(.true.)
        call buffer_chars_modified_tick(current_buffer(), tick, st)

        if (closer_table_p()) then
            if (.not. (locals(k)%has_cache .and. locals(k)%cache_pos == point() .and. locals(k)%cache_tick == tick)) then
                locals(k)%has_cache = .true.
                locals(k)%cache_pos = point()
                locals(k)%cache_tick = tick
                locals(k)%has_cache_data = .false.
                pt = point()
                call syntax_ppss(ppss)
                call goto_char(pt)

                if (ppss%comstr_start == 0) then
                    body: block
                        call opener_closer_at_point(hfound, hopener, htok, hbeg, hend, st)
                        if (.not. st%ok()) exit body
                        if (.not. hfound) exit body

                        if (.not. smie_blink_matching_inners) then
                            ht = smie_grammar_token(htok)

                            if (.not. ht%null) then
                                if (hopener) then
                                    if (numberp(ht%left)) exit body
                                else
                                    if (numberp(ht%right)) exit body
                                end if
                            end if
                        end if

                        tfound = .false.
                        tmatch = .false.
                        tbeg = 0
                        tend = 0

                        if (hopener) then
                            ! Opener.
                            call smie_forward_expression(sdata, st, halfexpression=.true.)
                            if (.not. st%ok()) exit body
                            tend = point()

                            if (sdata%null .or. sdata%level%kind == SMIE_LEVEL_NIL) then
                                call smie_backward_token(ignored, st)
                                if (.not. st%ok()) exit body
                                tfound = .true.
                                if (allocated(sdata%token)) tmatch = closer_pair_p(htok, sdata%token)
                                tbeg = point()
                            end if
                        else
                            ! Closer.
                            call smie_backward_expression(sdata, st, halfexpression=.true.)
                            if (.not. st%ok()) exit body

                            if (.not. sdata%null .and. sdata%level%kind /= SMIE_LEVEL_NIL) then
                                ht = smie_grammar_token(htok)
                                tt%null = .true.
                                if (allocated(sdata%token)) tt = smie_grammar_token(sdata%token)

                                if (.not. ht%null .and. .not. tt%null) then
                                    ! Here is an inner.
                                    if (numberp(ht%right) .and. level_eq(ht%right, tt%left)) then
                                        call goto_char(sdata%pos)
                                        tbeg = point()
                                        call smie_forward_token(ignored, st)
                                        if (.not. st%ok()) exit body
                                        tfound = .true.
                                        tmatch = .true.
                                        tend = point()
                                    end if
                                end if
                            else
                                tbeg = point()
                                call smie_forward_token(ignored, st)
                                if (.not. st%ok()) exit body
                                tfound = .true.
                                if (.not. sdata%null .and. allocated(sdata%token)) tmatch = closer_pair_p(sdata%token, htok)
                                tend = point()
                            end if
                        end if

                        ! Update the cache.
                        locals(k)%has_cache_data = .true.
                        locals(k)%cache_data%here_beg = hbeg
                        locals(k)%cache_data%here_end = hend

                        if (tfound) then
                            locals(k)%cache_data%there_beg = tbeg
                            locals(k)%cache_data%there_end = tend
                        else
                            locals(k)%cache_data%there_beg = 0
                            locals(k)%cache_data%there_end = 0
                        end if

                        locals(k)%cache_data%mismatch = .not. (tfound .and. tmatch)
                    end block body
                end if

                here = locals(k)%cache_pos
                call goto_char(here)
            end if

            if (locals(k)%has_cache_data) then
                found = .true.
                data = locals(k)%cache_data
                return
            end if
        end if

        call advised_paren_data(k, found, data)
    end subroutine smie_matching_block_data

    ! The `show_paren_data_function' SMIE's advice wraps.  Not a port.
    recursive subroutine advised_paren_data(k, found, data)
        integer, intent(in) :: k
        logical, intent(out) :: found
        type(show_paren_data_type), intent(out) :: data
        procedure(show_paren_data_proc), pointer :: fn

        found = .false.

        if (locals(k)%paren_data_local) then
            fn => locals(k)%paren_data
        else
            fn => show_paren_data_function
        end if

        if (associated(fn)) call fn(found, data)
    end subroutine advised_paren_data

    ! ------------------------------------------------------------------
    ! The indentation engine
    ! ------------------------------------------------------------------

    ! (funcall smie--hanging-eolp-function): the buffer's advised one, or
    ! the default.  Not a port.
    recursive logical function hanging_eolp()
        integer :: k

        k = local_index(.false.)

        if (k > 0) then
            if (associated(locals(k)%hanging_eolp)) then
                hanging_eolp = locals(k)%hanging_eolp()
                return
            end if
        end if

        hanging_eolp = smie_default_hanging_eolp()
    end function hanging_eolp

    !> The default smie--hanging-eolp-function: skip blanks; true at the
    !> end of a line (or a comment up to it).  (A quick hack upstream,
    !> undocumented there.) Port of the default value of smie.el's
    !> smie--hanging-eolp-function.
    recursive logical function smie_default_hanging_eolp() result(eol)
        type(status_type) :: st
        integer(int64) :: moved

        moved = skip_chars_forward(" "//achar(9), st)
        eol = eolp()
        ! (looking-at comment-start-skip) ;(bug#16041).
        if (.not. eol) eol = forward_comment(point_max())
    end function smie_default_hanging_eolp

    !> Advise smie--hanging-eolp-function in the current buffer with FN
    !> (add-function :around (local ...)): FN calls
    !> `smie_default_hanging_eolp' for the original.  Not a port.
    subroutine smie_set_hanging_eolp_function(fn)
        procedure(smie_hanging_eolp_proc) :: fn
        integer :: k

        k = local_index(.true.)
        if (k > 0) locals(k)%hanging_eolp => fn
    end subroutine smie_set_hanging_eolp_function

    !> Return true if the current token is "hanging".
    !> A hanging keyword is one that's at the end of a line except it's not at
    !> the beginning of a line.  Port of smie.el:smie-indent--hanging-p
    !> (smie-rule-hanging-p).
    recursive logical function smie_rule_hanging_p(status) result(hanging)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: token
        integer(int64) :: here, lep
        logical :: ok

        status = status_type()
        hanging = .false.
        if (smie_rule_bolp()) return
        here = point()
        lep = line_end_position()
        call smie_forward_token(token, status)

        if (status%ok()) then
            if (.not. allocated(token)) token = ""
            ! Could be an open-paren.
            if (len(token) == 0 .and. .not. eobp()) call forward_char(status, 1_int64)
            ok = hanging_eolp()
            hanging = lep <= point()
        end if

        call goto_char(here)
    end function smie_rule_hanging_p

    !> Return true if the current token is the first on the line.  Port of
    !> smie.el:smie-indent--bolp (smie-rule-bolp).
    recursive logical function smie_rule_bolp()
        integer(int64) :: here, moved
        type(status_type) :: st

        here = point()
        moved = skip_chars_backward(" "//achar(9), st)
        smie_rule_bolp = bolp()
        call goto_char(here)
    end function smie_rule_bolp

    !> Return true if the current token is the first on the line.
    !> Comments are treated as spaces.  Port of smie.el:smie-indent--bolp-1.
    recursive logical function smie_indent_bolp_1()
        integer(int64) :: bol, here
        logical :: ok

        bol = line_beginning_position()
        here = point()
        ok = forward_comment(-point())
        smie_indent_bolp_1 = point() <= bol
        call goto_char(here)
    end function smie_indent_bolp_1

    !> Like `current_column', but if there's a comment before us, use that.
    !> Port of smie.el:smie-indent--current-column.
    recursive integer function smie_indent_current_column()
        integer(int64) :: pos, lbp
        logical :: ok

        pos = point()
        lbp = line_beginning_position()
        ok = forward_comment(-1_int64)
        if (ok) ok = point() >= lbp
        if (.not. ok) call goto_char(pos)
        smie_indent_current_column = int(current_column())
        call goto_char(pos)
    end function smie_indent_current_column

    !> The parent of the current token: smie--parent, computed if needed.
    !> Port of smie.el:smie-indent--parent.
    recursive subroutine smie_parent(parent, status)
        type(smie_expression_type), intent(out) :: parent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tok
        type(smie_toklevels_type) :: t
        type(smie_expression_type) :: res
        integer(int64) :: pos, here

        status = status_type()

        if (.not. smie__parent%null) then
            parent = smie__parent
            return
        end if

        here = point()

        body: block
            pos = point()
            call smie_forward_token(tok, status)
            if (.not. status%ok()) exit body
            t%null = .true.
            if (allocated(tok)) t = smie_grammar_token(tok)

            if (t%null) then
                call goto_char(pos)
            else if (.not. numberp(t%left)) then
                call goto_char(pos)
            end if

            call smie_backward_expression(parent, status, halfexpression=.true.)
            if (.not. status%ok()) exit body

            if (parent%null) then
                do
                    call smie_backward_expression(res, status)
                    if (.not. status%ok()) exit body
                    if (.not. res%null) exit
                end do

                parent%null = .false.
                parent%level = smie_level_type()
                parent%pos = point()
                if (allocated(res%token)) parent%token = res%token
            end if

            smie__parent = parent
        end block body

        call goto_char(here)
    end subroutine smie_parent

    !> The level of the parent: the first element of `smie_indent__parent'.  Not a port.
    recursive function smie_parent_level(status) result(level)
        type(status_type), intent(out) :: status
        type(smie_level_type) :: level
        type(smie_expression_type) :: parent

        call smie_parent(parent, status)
        level = parent%level
    end function smie_parent_level

    !> Return true if the current token's parent is among PARENTS (an
    !> array; trailing blanks are not part of the tokens).
    !> Only meaningful when called from within `smie_rules_function'.
    !> Port of smie.el:smie-rule-parent-p.
    recursive logical function smie_rule_parent_p(parents, status) result(p)
        character(len=*), intent(in) :: parents(:)
        type(status_type), intent(out) :: status
        type(smie_expression_type) :: parent

        p = .false.
        call smie_parent(parent, status)
        if (.not. status%ok()) return
        if (allocated(parent%token)) p = in_list(parent%token, parents)
    end function smie_rule_parent_p

    ! Whether S is one of the trimmed strings of LIST.  Not a port.
    pure logical function in_list(s, list)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: list(:)
        integer :: i

        in_list = .false.

        do i = 1, size(list)
            if (str_eq(s, trim(list(i)))) in_list = .true.
        end do
    end function in_list

    !> Return true if the next token is among TOKENS.
    !> Only meaningful when called from within `smie_rules_function'.
    !> Port of smie.el:smie-rule-next-p.
    recursive logical function smie_rule_next_p(tokens, status) result(p)
        character(len=*), intent(in) :: tokens(:)
        type(status_type), intent(out) :: status
        type(smie_toklevels_type) :: next
        integer(int64) :: here

        p = .false.
        here = point()

        body: block
            if (smie__after == 0) then
                next = smie_indent_forward_token(status)
                if (.not. status%ok()) exit body
                smie__after = point()
            end if

            call goto_char(smie__after)
            next = smie_indent_forward_token(status)
            if (.not. status%ok()) exit body
            if (.not. next%null) p = in_list(next%token, tokens)
        end block body

        call goto_char(here)
    end function smie_rule_next_p

    !> Return true if the previous token is among TOKENS.  Port of
    !> smie.el:smie-rule-prev-p.
    recursive logical function smie_rule_prev_p(tokens, status) result(p)
        character(len=*), intent(in) :: tokens(:)
        type(status_type), intent(out) :: status
        type(smie_toklevels_type) :: prev
        integer(int64) :: here

        p = .false.
        here = point()
        prev = smie_indent_backward_token(status)
        if (status%ok() .and. .not. prev%null) p = in_list(prev%token, tokens)
        call goto_char(here)
    end function smie_rule_prev_p

    !> Return true if the parent is actually a sibling.
    !> Only meaningful when called from within `smie_rules_function'.
    !> Port of smie.el:smie-rule-sibling-p.
    recursive logical function smie_rule_sibling_p(status) result(p)
        type(status_type), intent(out) :: status
        type(smie_level_type) :: level, left
        type(smie_toklevels_type) :: t

        level = smie_parent_level(status)
        p = .false.
        if (.not. status%ok()) return
        left = smie_level_type()

        if (allocated(smie__token)) then
            t = smie_grammar_token(smie__token)
            if (.not. t%null) left = t%left
        end if

        p = level_eq(level, left)
    end function smie_rule_sibling_p

    !> Align with parent.
    !> If present, OFFSET should be an integer giving an additional offset to apply.
    !> Only meaningful when called from within `smie_rules_function'.
    !> Port of smie.el:smie-rule-parent.
    recursive function smie_rule_parent(status, offset) result(r)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: offset
        type(smie_rule_result_type) :: r
        type(smie_expression_type) :: parent
        type(smie_indent_type) :: v
        integer(int64) :: here

        here = point()
        call smie_parent(parent, status)
        if (.not. status%ok()) return
        call goto_char(parent%pos)
        v = smie_indent_virtual(status)
        call goto_char(here)
        if (.not. status%ok()) return

        if (v%kind /= SMIE_INDENT_COLUMN) then
            call number_error(status, v)
            return
        end if

        r%kind = SMIE_RULE_COLUMN
        r%value = v%column
        if (present(offset)) r%value = r%value + offset
    end function smie_rule_parent

    ! The error of arithmetic on an indentation that is not a number.
    ! Not a port.
    subroutine number_error(status, v)
        type(status_type), intent(out) :: status
        type(smie_indent_type), intent(in) :: v

        if (v%kind == SMIE_INDENT_NOINDENT) then
            call set_error(status, FM_GENERIC_ERROR, &
                           "Wrong type argument: expected integer, real or type(marker_type), got noindent")
        else
            call set_error(status, FM_GENERIC_ERROR, &
                           "Wrong type argument: expected integer, real or type(marker_type), got unallocated")
        end if
    end subroutine number_error

    ! smie-indent--separator-outdent.  Port of
    ! smie.el:smie-indent--separator-outdent.
    recursive integer function separator_outdent(status) result(n)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tok
        integer(int64) :: here, afterpos, moved

        here = point()
        n = smie_rule_separator_outdent
        call smie_forward_token(tok, status)

        if (.not. status%ok()) then
            call goto_char(here)
            return
        end if

        if (.not. allocated(tok)) then
            if (allocated(smie__token)) then
                call message("smie-rule-separator: Can't skip token "//smie__token)
            else
                call message('smie-rule-separator: Can''t skip token ""')
            end if
        end if

        moved = skip_chars_forward(" ", status)
        afterpos = 0
        if (.not. eolp()) afterpos = point()
        call goto_char(here)

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
    !> METHOD should be the method name that was passed to `smie_rules_function'.
    !> Only meaningful when called from within `smie_rules_function'.
    !> Port of smie.el:smie-rule-separator.
    recursive function smie_rule_separator(method, status) result(r)
        integer, intent(in) :: method
        type(status_type), intent(out) :: status
        type(smie_rule_result_type) :: r
        type(smie_rule_result_type) :: pr
        type(smie_toklevels_type) :: t
        integer(int64) :: here
        integer :: parent_pos_col, outdent
        logical :: ok, sibling

        status = status_type()

        if (method == SMIE_BEFORE .and. smie_rule_bolp()) then
            sibling = smie_rule_sibling_p(status)
            if (.not. status%ok()) return

            if (.not. sibling) then
                pr = smie_rule_parent(status)
                if (.not. status%ok()) return
                ! FIXME: we knew this when computing smie--parent.
                here = point()
                call goto_char(smie__parent%pos)
                t = smie_indent_forward_token(status)
                if (status%ok()) ok = forward_comment(point_max())
                parent_pos_col = int(current_column())
                call goto_char(here)
                if (.not. status%ok()) return
                outdent = separator_outdent(status)
                if (.not. status%ok()) return
                r%kind = SMIE_RULE_COLUMN
                r%value = max(pr%value, min(parent_pos_col, parent_pos_col - outdent))
                return
            end if
        end if

        if (method == SMIE_AFTER .and. smie_rule_bolp()) then
            r%kind = SMIE_RULE_OFFSET
            r%value = separator_outdent(status)
        end if
    end function smie_rule_separator

    !> The offset of ELEM ("basic" or "args"): what `smie_rules_function'
    !> gives for (SMIE_ELEM ELEM), else for (SMIE_ELEM "basic"), else
    !> `smie_indent_basic'.  Port of smie.el:smie-indent--offset.
    recursive integer function smie_indent_offset(elem, status) result(n)
        character(len=*), intent(in) :: elem
        type(status_type), intent(out) :: status
        type(smie_rule_result_type) :: r

        n = smie_indent_basic
        call call_rules(SMIE_ELEM, elem, r, status)
        if (.not. status%ok()) return

        if (r%kind == SMIE_NIL .and. elem /= "basic") then
            call call_rules(SMIE_ELEM, "basic", r, status)
            if (.not. status%ok()) return
        end if

        if (r%kind /= SMIE_NIL) n = r%value
    end function smie_indent_offset

    ! smie-indent--rule: compute indentation column according to
    ! `smie_rules_function'.  METHOD and TOKEN are passed to it.  AFTER is
    ! the position after TOKEN, if known (0 if not).  PARENT is the parent
    ! info returned by `smie_backward_expression', if known.  BASE_POS is the
    ! position relative to which offsets should be applied (0 for none).
    ! Port of smie.el:smie-indent--rule.
    recursive function indent_rule(method, token, status, after, parent, base_pos) result(indent)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: after, base_pos
        type(smie_expression_type), intent(in), optional :: parent
        type(smie_indent_type) :: indent
        type(smie_rule_result_type) :: offset
        type(smie_indent_type) :: v
        integer(int64) :: here
        logical :: virtual

        here = point()
        call indent_rule_1(method, token, offset, status, after, parent)

        body: block
            if (.not. status%ok()) exit body

            select case (offset%kind)
            case (SMIE_NIL)
                continue
            case (SMIE_RULE_COLUMN)
                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=offset%value)
            case (SMIE_RULE_OFFSET)
                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=offset%value)
                if (.not. present(base_pos)) exit body
                if (base_pos == 0) exit body
                call goto_char(base_pos)
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
                    if (.not. parent%null) virtual = parent%level%kind == SMIE_LEVEL_NIL
                end if

                if (.not. virtual) then
                    virtual = smie_rule_hanging_p(status)
                    if (.not. status%ok()) exit body
                end if

                if (virtual) then
                    v = smie_indent_virtual(status)
                    if (.not. status%ok()) exit body

                    if (v%kind /= SMIE_INDENT_COLUMN) then
                        call number_error(status, v)
                        exit body
                    end if

                    indent%column = indent%column + v%column
                else
                    indent%column = indent%column + int(current_column())
                end if
            case default
                call set_error(status, FM_GENERIC_ERROR, "Unknown indentation offset")
            end select
        end block body

        call goto_char(here)
    end function indent_rule

    ! smie-indent--rule-1: call `smie_rules_function' with smie--parent,
    ! smie--token and smie--after bound.  Port of
    ! smie.el:smie-indent--rule-1.
    recursive subroutine indent_rule_1(method, token, result, status, after, parent)
        integer, intent(in) :: method
        character(len=*), intent(in) :: token
        type(smie_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: after
        type(smie_expression_type), intent(in), optional :: parent
        type(smie_expression_type) :: saved_parent
        character(len=:), allocatable :: saved_token
        integer(int64) :: saved_after
        logical :: had_token

        saved_parent = smie__parent
        had_token = allocated(smie__token)
        if (had_token) saved_token = smie__token
        saved_after = smie__after
        smie__parent = smie_expression_type()
        if (present(parent)) smie__parent = parent
        smie__token = token
        smie__after = 0
        if (present(after)) smie__after = after
        call call_rules(method, token, result, status)
        smie__parent = saved_parent

        if (had_token) then
            smie__token = saved_token
        else if (allocated(smie__token)) then
            deallocate (smie__token)
        end if

        smie__after = saved_after
    end subroutine indent_rule_1

    !> Skip token forward and return it, along with its levels.
    !> Point should be between tokens when calling this function (i.e.,
    !> not in the middle of a string/comment).  Port of
    !> smie.el:smie-indent-forward-token.
    recursive function smie_indent_forward_token(status) result(t)
        type(status_type), intent(out) :: status
        type(smie_toklevels_type) :: t
        character(len=:), allocatable :: tok
        logical :: paren

        call smie_forward_token(tok, status)
        if (.not. status%ok()) return

        if (allocated(tok)) then
            if (len(tok) > 0) then
                t = smie_grammar_token(tok)
                return
            end if
        end if

        paren = looking_at("\s(\|\s)\(\)", status)
        if (.not. status%ok()) return

        if (paren) then
            t%null = .false.

            if (match_end(1, status) > 0) then
                t%left = smie_level_type(kind=SMIE_LEVEL_NUMBER, value=0)
                t%right = smie_level_type()
            else
                t%left = smie_level_type()
                t%right = smie_level_type(kind=SMIE_LEVEL_NUMBER, value=0)
            end if

            call forward_char(status, 1_int64)
            t%token = substring(point() - 1, point())
            return
        end if

        paren = looking_at("\s""\|\s|", status)
        if (.not. status%ok()) return

        if (paren) then
            call forward_expression(1_int64, status)
            return
        end if

        if (eobp()) return

        if (allocated(tok)) then
            call set_error(status, FM_GENERIC_ERROR, "Bumped into unknown token: "//prin1_string(tok))
        else
            call set_error(status, FM_GENERIC_ERROR, 'Bumped into unknown token: ""')
        end if
    end function smie_indent_forward_token

    !> Skip token backward and return it, along with its levels.  Port of
    !> smie.el:smie-indent-backward-token.
    recursive function smie_indent_backward_token(status) result(t)
        type(status_type), intent(out) :: status
        type(smie_toklevels_type) :: t
        character(len=:), allocatable :: tok
        integer :: class

        call smie_backward_token(tok, status)
        if (.not. status%ok()) return

        if (allocated(tok)) then
            if (len(tok) > 0) then
                t = smie_grammar_token(tok)
                return
            end if
        end if

        class = syntax_class_after(point() - 1)

        ! 4 == open paren syntax, 5 == close.
        if (class == 4 .or. class == 5) then
            call forward_char(status, -1_int64)
            t%null = .false.
            t%token = substring(point(), point() + 1)

            if (class == 4) then
                t%left = smie_level_type()
                t%right = smie_level_type(kind=SMIE_LEVEL_NUMBER, value=0)
            else
                t%left = smie_level_type(kind=SMIE_LEVEL_NUMBER, value=0)
                t%right = smie_level_type()
            end if
        else if (class == 7 .or. class == 15) then
            call backward_expression(1_int64, status)
        else if (bobp()) then
            continue
        else
            call set_error(status, FM_GENERIC_ERROR, "Bumped into unknown token")
        end if
    end function smie_indent_backward_token

    ! (syntax-class (syntax-after POS)), -1 for nil.  Not a port.
    integer function syntax_class_after(pos)
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d
        integer(int32) :: c
        logical :: found

        syntax_class_after = -1
        call char_after(c, found, pos)
        if (.not. found) return
        d = syntax_entry_at(int(c), pos)
        if (d%code < 0) return
        syntax_class_after = iand(d%code, 65535)
    end function syntax_class_after

    ! (car (syntax-after POS)), -1 for nil.  Not a port.
    integer function syntax_code_after(pos)
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d
        integer(int32) :: c
        logical :: found

        syntax_code_after = -1
        call char_after(c, found, pos)
        if (.not. found) return
        d = syntax_entry_at(int(c), pos)
        syntax_code_after = d%code
    end function syntax_code_after

    !> Compute the virtual indentation to use for point.
    !> This is used when we're not trying to indent point but just
    !> need to compute the column at which point should be indented
    !> in order to figure out the indentation of some other (further down) point.
    !> Port of smie.el:smie-indent-virtual.
    recursive function smie_indent_virtual(status) result(indent)
        type(status_type), intent(out) :: status
        type(smie_indent_type) :: indent

        status = status_type()

        ! Trust pre-existing indentation on other lines.
        if (smie_rule_bolp()) then
            indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=int(current_column()))
        else
            indent = smie_indent_calculate(status)
        end if
    end function smie_indent_virtual

    ! A column as an indentation.  Not a port.
    pure function column_of(col) result(indent)
        integer(int64), intent(in) :: col
        type(smie_indent_type) :: indent

        indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=int(col))
    end function column_of

    !> Obey the `fixindent' special comment.  Port of
    !> smie.el:smie-indent-fixindent.
    recursive subroutine smie_indent_fixindent(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: here, found
        character(len=:), allocatable :: start_skip, end_skip

        status = status_type()
        if (.not. smie_rule_bolp()) return
        here = point()
        call comment_normalize_vars(status)
        if (.not. status%ok()) return
        start_skip = ""
        if (allocated(comment_start_skip%value)) start_skip = comment_start_skip%value
        end_skip = ""
        if (allocated(comment_end_skip%value)) end_skip = comment_end_skip%value
        ! 1+ to account for the \n comment termination.
        call re_search_forward("\(?:"//start_skip//"\)fixindent\(?:"//end_skip//"\)", found, status, &
                               bound=1 + line_end_position(), noerror=SEARCH_NOERROR)
        call goto_char(here)
        if (.not. status%ok()) return
        if (found /= 0) indent = column_of(current_column())
    end subroutine smie_indent_fixindent

    !> Start the file at column 0.  Port of smie.el:smie-indent-bob.
    recursive subroutine smie_indent_bob(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        integer(int64) :: here
        logical :: ok

        status = status_type()
        here = point()
        ok = forward_comment(-point())
        if (bobp()) indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=prog_first_column())
        call goto_char(here)
    end subroutine smie_indent_bob

    !> Align close paren with opening paren.  Port of
    !> smie.el:smie-indent-close.
    recursive subroutine smie_indent_close(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(smie_rule_result_type) :: r
        integer(int64) :: here, moved
        logical :: at_close

        status = status_type()
        here = point()

        body: block
            ! (forward-comment (point-max))
            at_close = looking_at("\s)", status)
            if (.not. status%ok() .or. .not. at_close) exit body
            call indent_rule_1(SMIE_CLOSE_ALL, substring(point(), point() + 1), r, status, after=1 + point())
            if (.not. status%ok()) exit body

            if (r%kind /= SMIE_NIL) then
                do while (skip_syntax_forward(")") /= 0)
                    moved = skip_chars_forward(" "//achar(9), status)
                end do
            else
                call forward_char(status, 1_int64)
                if (.not. status%ok()) exit body
            end if

            call backward_expression(1_int64, status)

            if (status%code == FM_SCAN_ERROR) then
                status = status_type()
                exit body
            end if

            if (.not. status%ok()) exit body
            ! :not-hanging
            indent = smie_indent_virtual(status)
        end block body

        call goto_char(here)
    end subroutine smie_indent_close

    ! Whether two token levels are `equal'.  Not a port.
    logical function toklevels_equal(a, b)
        type(smie_toklevels_type), intent(in) :: a, b

        toklevels_equal = a%null .eqv. b%null
        if (.not. toklevels_equal .or. a%null) return
        toklevels_equal = str_eq(a%token, b%token) .and. level_equal(a%left, b%left) .and. level_equal(a%right, b%right)
    end function toklevels_equal

    ! Whether two levels are `equal'.  Not a port.
    pure logical function level_equal(a, b)
        type(smie_level_type), intent(in) :: a, b

        level_equal = a%kind == b%kind
        if (level_equal .and. (a%kind == SMIE_LEVEL_NUMBER .or. a%kind == SMIE_LEVEL_LIST)) level_equal = a%value == b%value
    end function level_equal

    !> Indent point based on the token that follows it immediately.
    !> If TOKEN is present, assume that that is the token that follows point.
    !> Returns either a column number or SMIE_INDENT_NIL if it considers that indentation
    !> should not be computed on the basis of the following token.  Port of
    !> smie.el:smie-indent-keyword.
    recursive subroutine smie_indent_keyword(indent, status, token)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: token
        type(smie_toklevels_type) :: toklevels, res, again
        type(smie_expression_type) :: parent
        type(smie_level_type) :: plevel
        type(status_type) :: st
        integer(int64) :: pos, endpos, shortage, here
        character(len=:), allocatable :: tok
        logical :: skip, ok

        status = status_type()
        pos = point()

        body: block
            if (present(token)) then
                toklevels = smie_grammar_token(token)
            else
                res = smie_indent_forward_token(status)
                if (.not. status%ok()) exit body
                ! Ignore tokens on subsequent lines.
                skip = .false.

                if (pos < line_beginning_position()) then
                    ! Make sure `token' also *starts* on another line.
                    endpos = point()
                    call goto_char(pos)
                    shortage = forward_line(1_int64)
                    ! As seen in bug#22960, pos may be inside
                    ! a string, and forward-token may then stumble.
                    again = smie_indent_forward_token(st)
                    if (st%ok()) skip = toklevels_equal(res, again) .and. point() == endpos
                    call goto_char(endpos)
                end if

                if (skip) then
                    toklevels = smie_toklevels_type()
                else
                    call goto_char(pos)
                    toklevels = res
                end if
            end if

            ! Not a keyword.
            if (toklevels%null) exit body
            tok = toklevels%token

            if (.not. numberp(toklevels%left)) then
                ! Different cases:
                ! - smie-indent--bolp: "indent according to others".
                ! - common hanging: "indent according to others".
                ! - SML-let hanging: "indent like parent".
                ! - if-after-else: "indent-like parent".
                ! - middle-of-line: "trust current position".
                indent = indent_rule(SMIE_BEFORE, tok, status)
                if (.not. status%ok() .or. indent%kind /= SMIE_INDENT_NIL) exit body
                ! I.e. non-virtual indent.
                ! For an open-paren-like thingy at BOL, always indent only
                ! based on other rules (typically smie-indent-after-keyword).
                ! FIXME: we do the same if after a comment, since we may be trying
                ! to compute the indentation of this comment and we shouldn't indent
                ! based on the indentation of subsequent code.
                if (smie_indent_bolp_1()) exit body
                ! By default use point unless we're hanging.
                ok = smie_rule_hanging_p(status)
                if (.not. status%ok()) exit body
                if (.not. ok) indent = column_of(current_column())
                exit body
            end if

            ! FIXME: This still looks too much like black magic!!
            call smie_backward_expression(parent, status, token=tok)
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
            ok = point() < pos
            if (.not. ok .and. .not. parent%null) ok = parent%pos /= 0 .and. parent%pos < pos
            ! If we didn't move at all, that means we didn't really skip
            ! what we wanted.  Should almost never happen, other than
            ! maybe when an infix or close-paren is at the beginning
            ! of a buffer.
            if (.not. ok) exit body
            here = point()
            call goto_char(pos)

            if (parent%null) then
                indent = indent_rule(SMIE_BEFORE, tok, status, parent=parent)
            else
                indent = indent_rule(SMIE_BEFORE, tok, status, parent=parent, base_pos=parent%pos)
            end if

            call goto_char(here)
            if (.not. status%ok() .or. indent%kind /= SMIE_INDENT_NIL) exit body
            plevel = merge_level(parent)

            if (level_eq(plevel, toklevels%left)) then
                ! We bumped into a same-level operator; align with it.
                ok = smie_rule_bolp() .and. point() /= pos

                if (ok) then
                    here = point()
                    call goto_char(parent%pos)
                    ok = .not. smie_rule_bolp()
                    call goto_char(here)
                end if

                if (ok) then
                    ! If the parent is at EOL and its children are indented like
                    ! itself, then we can just obey the indentation chosen for the
                    ! child.
                    ! This is important for operators like ";" which
                    ! are usually at EOL (and have an offset of 0): otherwise we'd
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
                    call goto_char(parent%pos)
                    ! Don't use (smie-indent-virtual :not-hanging) here, because we
                    ! want to jump back over a sequence of same-level ops such as
                    !    a -> b -> c
                    !    -> d
                    ! So as to align with the earliest appropriate place.
                    indent = smie_indent_virtual(status)
                end if
            else if (point() == pos .and. smie_rule_bolp()) then
                ! Since we started at BOL, we're not computing a virtual
                ! indentation, and we're still at the starting point, so
                ! we can't use `current-column' which would cause
                ! indentation to depend on itself and we can't use
                ! smie-indent-virtual since that would be an inf-loop.
                continue
            else if (plevel%kind /= SMIE_LEVEL_NIL) then
                ! In indent-keyword, if we're indenting `then' wrt `if', we
                ! want to use indent-virtual rather than use just
                ! current-column, so that we can apply the (:before . "if")
                ! rule which does the "else if" dance in SML.  But in other
                ! cases, we do not want to use indent-virtual (e.g. indentation
                ! of "*" w.r.t "+", or ";" wrt "(").  We could just always use
                ! indent-virtual and then have indent-rules say explicitly to
                ! use `point' after things like "(" or "+" when they're not at
                ! EOL, but you'd end up with lots of those rules.
                ! So we use a heuristic here, which is that we only use virtual
                ! if the parent is tightly linked to the child token (they're
                ! part of the same BNF rule).
                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=smie_indent_current_column())
            else
                indent = smie_indent_virtual(status)
            end if
        end block body

        call goto_char(pos)

    contains

        ! (car parent): nil for a nil parent.
        function merge_level(p) result(l)
            type(smie_expression_type), intent(in) :: p
            type(smie_level_type) :: l

            l = smie_level_type()
            if (.not. p%null) l = p%level
        end function merge_level
    end subroutine smie_indent_keyword

    ! `smie_indent_keyword' as an indentation function.  Not a port.
    recursive subroutine indent_keyword_function(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status

        call smie_indent_keyword(indent, status)
    end subroutine indent_keyword_function

    !> Compute indentation of a comment.  Port of
    !> smie.el:smie-indent-comment.
    recursive subroutine smie_indent_comment(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: next
        integer(int64) :: pos, found, at, moved
        logical :: ok, skip

        status = status_type()
        ! Don't do it for virtual indentations.  We should normally never be "in
        ! front of a comment" when doing virtual-indentation anyway.  And if we are
        ! (as can happen in octave-mode), moving forward can lead to inf-loops.
        if (.not. smie_rule_bolp()) return
        pos = point()

        body: block
            call beginning_of_line()

            if (.not. allocated(comment_start_skip%value)) then
                call set_error(status, FM_GENERIC_ERROR, "Wrong type argument: expected character(len=*), got unallocated")
                exit body
            end if

            call re_search_forward(comment_start_skip%value, found, status, bound=line_end_position(), &
                                   noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found == 0) exit body
            at = match_end(1, status)
            if (at == 0) at = match_beginning(0, status)
            if (at /= pos) exit body
            call goto_char(pos)
            ok = forward_comment(point_max())
            moved = skip_chars_forward(" "//achar(9)//achar(13)//achar(10), status)
            ! Don't align with a closer, since the comment is "within" the
            ! closed element.  Don't align with EOB either.
            at = point()
            call smie_forward_token(next, status)
            if (.not. status%ok()) exit body
            if (.not. allocated(next)) next = ""
            skip = .false.
            if (len(next) == 0) skip = eobp() .or. syntax_code_after(point()) == 5
            if (.not. skip) skip = smie_closer_p(next)
            call goto_char(at)
            if (skip) exit body
            ! FIXME: We assume here that smie-indent-calculate will compute the
            ! indentation of the next token based on text before the comment,
            ! but this is not guaranteed, so maybe we should let
            ! smie-indent-calculate return some info about which buffer
            ! position was used as the "indentation base" and check that this
            ! base is before `pos'.
            indent = smie_indent_calculate(status)
        end block body

        call goto_char(pos)
    end subroutine smie_indent_comment

    !> Indentation of comment-continue lines.  Port of
    !> smie.el:smie-indent-comment-continue.
    recursive subroutine smie_indent_comment_continue(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cont
        type(parse_state_type) :: ppss
        integer(int64) :: pos, start, shortage, moved
        logical :: ok

        status = status_type()
        if (.not. allocated(comment_continue%value)) return
        cont = comment_string_strip(comment_continue%value, .true., .true.)
        if (len(cont) == 0) return
        ok = looking_at(regexp_quote(cont), status)
        if (.not. status%ok() .or. .not. ok) return
        pos = point()
        call syntax_ppss(ppss)
        call goto_char(pos)
        if (ppss%incomment == 0) return

        body: block
            shortage = forward_line(-1_int64)
            start = ppss%comstr_start

            if (point() <= start) then
                call goto_char(start)
                ok = .false.

                if (allocated(comment_start_skip%value)) then
                    ok = looking_at(comment_start_skip%value, status)
                    if (.not. status%ok()) exit body
                end if

                if (.not. ok) then
                    call forward_char(status, 1_int64)
                else
                    call goto_char(match_end(0, status))
                    moved = skip_chars_backward(" "//achar(9), status)

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
                        call goto_char(start)
                    else
                        call goto_char(1 + start)
                    end if
                end if

                indent = column_of(current_column())
            else
                moved = skip_chars_forward(" "//achar(9), status)
                ok = looking_at(regexp_quote(cont), status)
                if (status%ok() .and. ok) indent = column_of(current_column())
            end if
        end block body

        call goto_char(pos)
    end subroutine smie_indent_comment_continue

    !> Indentation of a comment closer.  Port of
    !> smie.el:smie-indent-comment-close.
    recursive subroutine smie_indent_comment_close(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: ender, start
        type(parse_state_type) :: ppss
        integer(int64) :: pos, idx, mb0, mb2, me2
        logical :: ok

        status = status_type()
        if (.not. allocated(comment_end_skip%value)) return
        ! Not just a \n comment-closer.
        ok = looking_at(" "//achar(9)//"*$", status)
        if (.not. status%ok() .or. ok) return
        ok = looking_at(comment_end_skip%value, status)
        if (.not. status%ok() .or. .not. ok) return
        ender = substring(match_beginning(0, status), match_end(0, status))
        pos = point()
        call syntax_ppss(ppss)
        call goto_char(pos)
        if (ppss%incomment == 0) return

        body: block
            call goto_char(ppss%comstr_start)
            if (.not. allocated(comment_start_skip%value)) exit body
            ok = looking_at(comment_start_skip%value, status)
            if (.not. status%ok() .or. .not. ok) exit body
            start = substring(match_beginning(0, status), match_end(0, status))
            ! Align the common substring between starter
            ! and ender, if possible.
            idx = string_match("\(.+\).*"//achar(10)//"\(.*?\)\1", start//achar(10)//ender, status)
            if (.not. status%ok()) exit body

            if (idx > 0) then
                mb0 = match_beginning(0, status) - 1
                mb2 = match_beginning(2, status)
                me2 = match_end(2, status)
                indent = column_of(current_column() + mb0 + (mb2 - me2))
            else
                indent = column_of(current_column())
            end if
        end block body

        call goto_char(pos)
    end subroutine smie_indent_comment_close

    !> Inside a comment: `noindent'.  Port of
    !> smie.el:smie-indent-comment-inside.
    recursive subroutine smie_indent_comment_inside(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(parse_state_type) :: ppss
        integer(int64) :: pos

        status = status_type()
        pos = point()
        call syntax_ppss(ppss)
        call goto_char(pos)
        if (ppss%incomment /= 0) indent%kind = SMIE_INDENT_NOINDENT
    end subroutine smie_indent_comment_inside

    !> Inside a string: `noindent'.  Port of
    !> smie.el:smie-indent-inside-string.
    recursive subroutine smie_indent_inside_string(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(parse_state_type) :: ppss
        integer(int64) :: pos

        status = status_type()
        pos = point()
        call syntax_ppss(ppss)
        call goto_char(pos)
        if (ppss%instring /= -1) indent%kind = SMIE_INDENT_NOINDENT
    end subroutine smie_indent_inside_string

    !> Indentation right after a special keyword.  Port of
    !> smie.el:smie-indent-after-keyword.
    recursive subroutine smie_indent_after_keyword(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(smie_toklevels_type) :: toklevel
        type(smie_indent_type) :: v
        integer(int64) :: pos
        integer :: basic

        status = status_type()
        pos = point()

        body: block
            toklevel = smie_indent_backward_token(status)
            if (.not. status%ok() .or. toklevel%null) exit body
            indent = indent_rule(SMIE_AFTER, toklevel%token, status, after=pos, base_pos=point())
            if (.not. status%ok() .or. indent%kind /= SMIE_INDENT_NIL) exit body
            ! The default indentation after a keyword/operator is
            ! 0 for infix, t for prefix, and use another rule
            ! for postfix.
            ! A closer.
            if (.not. numberp(toklevel%right)) exit body

            if (.not. numberp(toklevel%left) .or. smie_closer_p(toklevel%token)) then
                ! An opener, or an inner.
                v = smie_indent_virtual(status)
                if (.not. status%ok()) exit body
                basic = smie_indent_offset("basic", status)
                if (.not. status%ok()) exit body

                if (v%kind /= SMIE_INDENT_COLUMN) then
                    call number_error(status, v)
                    exit body
                end if

                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=v%column + basic)
            else
                ! An infix.
                indent = smie_indent_virtual(status)
            end if
        end block body

        call goto_char(pos)
    end subroutine smie_indent_after_keyword

    !> Indentation rule when there's nothing yet on the line.
    !> Without this rule, SMIE assumes that an empty line will be filled with an
    !> argument (since it falls back to smie-indent-sexps), which tends
    !> to indent far too deeply.  Port of smie.el:smie-indent-empty-line.
    recursive subroutine smie_indent_empty_line(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(smie_rule_result_type) :: r
        type(smie_toklevels_type) :: t

        status = status_type()
        if (.not. eolp()) return
        ! FIXME: Should we default to ";"?
        call call_rules(SMIE_ELEM, "empty_line_token", r, status)
        if (.not. status%ok()) return
        if (r%kind /= SMIE_RULE_TOKEN) return
        t = smie_grammar_token(r%token)
        if (.not. t%null) call smie_indent_keyword(indent, status, r%token)
    end subroutine smie_indent_empty_line

    !> Indentation of sequences of simple expressions without
    !> intervening keywords or operators.  E.g. "a b c" or "g (balbla) f".
    !> Can be a list of expressions or a function call.
    !> If it's a function call, the first element is special (it's the
    !> function).  We distinguish function calls from mere lists of
    !> expressions based on whether the preceding token is listed in
    !> the `list-intro' entry of smie-indent-rules.  Port of
    !> smie.el:smie-indent-exps.
    recursive subroutine smie_indent_exps(indent, status)
        type(smie_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        type(smie_expression_type) :: res
        type(smie_rule_result_type) :: r
        character(len=:), allocatable :: tok
        integer(int64), allocatable :: positions(:)
        integer(int64) :: here, inner
        integer :: args
        logical :: arg

        status = status_type()
        here = point()
        allocate (positions(0))

        body: block
            do
                call smie_backward_expression(res, status)
                if (.not. status%ok()) exit body
                if (.not. (res%null .or. res%level%kind == SMIE_LEVEL_NIL)) exit
                positions = [point(), positions]
                if (smie_rule_bolp()) exit
            end do

            ! Figure out if the atom we just skipped is an argument rather
            ! than a function.
            inner = point()
            call smie_backward_expression(res, status)
            if (.not. status%ok()) exit body
            arg = res%null .or. res%level%kind == SMIE_LEVEL_NIL

            if (.not. arg) then
                call smie_backward_token(tok, status)
                if (.not. status%ok()) exit body
                if (.not. allocated(tok)) tok = ""
                call call_rules(SMIE_LIST_INTRO, tok, r, status)
                if (.not. status%ok()) exit body
                arg = r%kind /= SMIE_NIL
            end if

            call goto_char(inner)

            if (size(positions) == 0) then
                ! We're the first expression of the list.  In that case, the
                ! indentation should be (have been) determined by its context.
                continue
            else if (arg) then
                ! There's a previous element, and it's not special (it's not
                ! the function), so let's just align with that one.
                call goto_char(positions(1))
                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=smie_indent_current_column())
            else if (size(positions) > 1) then
                ! We skipped some args plus the function and bumped into something.
                ! Align with the first arg.
                call goto_char(positions(2))
                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=smie_indent_current_column())
            else
                ! We're the first arg.
                call goto_char(positions(1))
                args = smie_indent_offset("args", status)
                if (.not. status%ok()) exit body
                ! We used to use (smie-indent-virtual), but that
                ! doesn't seem right since it might then indent args less than
                ! the function itself.
                indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=args + smie_indent_current_column())
            end if
        end block body

        call goto_char(here)
    end subroutine smie_indent_exps

    ! ------------------------------------------------------------------
    ! `smie_indent_functions'
    ! ------------------------------------------------------------------

    ! The index in SELF's local values of the current buffer's, 0 if none.
    ! Not a port.
    integer function indent_functions_local_index(self) result(k)
        class(smie_indent_functions_type), intent(in) :: self

        k = 0
        if (cur == 0 .or. .not. allocated(self%locals)) return

        do k = 1, size(self%locals)
            if (self%locals(k)%serial == buffers(cur)%serial) return
        end do

        k = 0
    end function indent_functions_local_index

    ! The functions run in the current buffer, in order.  Not a port.
    function indent_functions_current(self) result(list)
        class(smie_indent_functions_type), intent(in) :: self
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

    ! The %add of `smie_indent_functions' (add-hook).  Not a port.
    subroutine indent_functions_add(self, fn, append, local)
        class(smie_indent_functions_type), intent(inout) :: self
        procedure(smie_indent_proc) :: fn
        logical, intent(in), optional :: append, local
        type(indent_entry_type) :: e
        type(indent_local_type), allocatable :: grown(:)
        integer :: k, i
        logical :: at_end, in_buffer

        at_end = .false.
        if (present(append)) at_end = append
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

        if (cur == 0) return
        k = self%local_index()

        if (k == 0) then
            if (.not. allocated(self%locals)) allocate (self%locals(0))

            do k = 1, size(self%locals)
                if (.not. buffer_serial_live(self%locals(k)%serial)) exit
            end do

            if (k > size(self%locals)) then
                allocate (grown(size(self%locals) + 1))

                do i = 1, size(self%locals)
                    grown(i) = self%locals(i)
                end do

                call move_alloc(grown, self%locals)
                k = size(self%locals)
            end if

            self%locals(k)%serial = buffers(cur)%serial
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
    end subroutine indent_functions_add

    ! The %remove of `smie_indent_functions' (remove-hook).  Not a port.
    subroutine indent_functions_remove(self, fn, local)
        class(smie_indent_functions_type), intent(inout) :: self
        procedure(smie_indent_proc) :: fn
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
    recursive function smie_indent_calculate(status) result(indent)
        type(status_type), intent(out) :: status
        type(smie_indent_type) :: indent
        type(indent_entry_type), allocatable :: fns(:)
        integer :: i

        status = status_type()
        fns = smie_indent_functions%current()

        do i = 1, size(fns)
            if (.not. associated(fns(i)%fn)) cycle
            call fns(i)%fn(indent, status)
            if (.not. status%ok()) return
            if (indent%kind /= SMIE_INDENT_NIL) return
        end do
    end function smie_indent_calculate

    !> Indent current line using the SMIE indentation engine.  NOINDENT is
    !> set when the engine said `noindent'.  Port of smie.el:smie-indent-line.
    recursive subroutine smie_indent_line(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        type(smie_indent_type) :: indent
        type(status_type) :: st
        integer(int64) :: savep, shortage, moved, here, col
        logical :: keep

        status = status_type()
        noindent = .false.
        savep = point()
        keep = .true.
        here = point()
        shortage = forward_line(0_int64)
        moved = skip_chars_forward(" "//achar(9), st)
        if (point() >= savep) keep = .false.
        indent = smie_indent_calculate(st)
        call goto_char(here)

        if (.not. st%ok()) then
            ! (with-demoted-errors "SMIE Error: %S" ...)
            call message("SMIE Error: "//format_error(st))
            indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=0)
        else if (indent%kind == SMIE_INDENT_NIL) then
            indent = smie_indent_type(kind=SMIE_INDENT_COLUMN, column=0)
        end if

        if (indent%kind == SMIE_INDENT_NOINDENT) then
            ! If something funny is used (e.g. `noindent'), return it.
            noindent = .true.
            return
        end if

        ! Just in case.
        col = max(0, indent%column)

        if (keep) then
            call save_point_indent(col, status)
        else
            call indent_line_to(col, status)
        end if

    contains

        ! (save-excursion (indent-line-to COL)).
        subroutine save_point_indent(c, s)
            integer(int64), intent(in) :: c
            type(status_type), intent(out) :: s
            type(handle_type) :: m
            integer(int64) :: p

            call copy_marker_at(point(), m, .false., s)
            call indent_line_to(c, s)
            call marker_position(m, p, st)
            call goto_char(p)
            call free_marker(m, st)
        end subroutine save_point_indent
    end subroutine smie_indent_line

    ! smie-auto-fill around the function it advises (the
    ! `normal_auto_fill_function' of SMIE modes).  Not a port.
    recursive subroutine smie_auto_fill_function(status)
        type(status_type), intent(out) :: status

        call smie_auto_fill(status)
    end subroutine smie_auto_fill_function

    ! The auto-fill function SMIE's advice wraps.  Not a port.
    recursive subroutine do_advised_auto_fill(status)
        type(status_type), intent(out) :: status
        procedure(command_proc), pointer :: fn
        integer :: k

        k = local_index(.false.)
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

    !> Auto-fill for SMIE modes: break the line where SMIE gains the most
    !> indentation, else call the advised function (DO-AUTO-FILL).  Port of
    !> smie.el:smie-auto-fill.
    recursive subroutine smie_auto_fill(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: ppss
        type(smie_toklevels_type) :: t
        type(smie_indent_type) :: newcol
        integer(int64) :: fc, here, end_pos, bsf, gain, newgain, moved, reached
        logical :: done, skip
        type(handle_type) :: saved
        type(status_type) :: st

        status = status_type()
        fc = current_fill_column()

        if (current_column() > fc) then
            ! The loop below presumes BOL is outside of strings or comments.  Also,
            ! sometimes we prefer to fill the comment than the code around it.
            here = point()
            call syntax_ppss(ppss, line_beginning_position())
            call goto_char(here)
            skip = ppss%comstr_start /= 0

            if (.not. skip) then
                call move_to_column(fc, reached, .false., status)
                call syntax_ppss(ppss)
                call goto_char(here)
                skip = ppss%incomment /= 0
            end if

            if (.not. skip) then
                do
                    ! We silence the error completely since errors are "normal" in
                    ! some cases and an error message would be annoying (bug#19342).
                    done = .false.
                    call copy_marker_at(point(), saved, .false., st)

                    attempt: block
                        end_pos = point()
                        ! Best-so-far.
                        bsf = 0
                        gain = 0
                        call beginning_of_line()

                        do
                            t = smie_indent_forward_token(status)
                            if (.not. status%ok()) exit attempt
                            if (.not. (point() <= end_pos .and. current_column() <= fc)) exit
                            ! FIXME?  `smie-indent-calculate' can (and often
                            ! does) return a result that actually depends on the
                            ! presence/absence of a newline, so the gain computed
                            ! here may not be accurate, but in practice it seems
                            ! to work well enough.
                            moved = skip_chars_forward(" "//achar(9), status)
                            newcol = smie_indent_calculate(status)
                            if (.not. status%ok()) exit attempt

                            if (newcol%kind /= SMIE_INDENT_COLUMN) then
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
                            call goto_char(bsf)
                            call newline_and_indent(status)
                            if (.not. status%ok()) exit attempt
                            done = .true.
                        end if
                    end block attempt

                    ! (save-excursion ...)
                    call marker_position(saved, here, st)
                    call goto_char(here)
                    call free_marker(saved, st)
                    if (status%code == FM_SCAN_ERROR) status = status_type()
                    if (.not. status%ok()) return
                    if (.not. (done .and. current_column() > fc)) exit
                end do
            end if
        end if

        if (current_column() > fc) call do_advised_auto_fill(status)
    end subroutine smie_auto_fill

    ! smie--matching-block-data around the function it advises.  Not a port.
    recursive subroutine smie_paren_data_function(found, data)
        logical, intent(out) :: found
        type(show_paren_data_type), intent(out) :: data

        call smie_matching_block_data(found, data)
    end subroutine smie_paren_data_function

    ! string-lessp.  Not a port.
    pure logical function string_lessp(a, b)
        character(len=*), intent(in) :: a, b
        integer :: i

        do i = 1, min(len(a), len(b))
            if (a(i:i) /= b(i:i)) then
                string_lessp = ichar(a(i:i)) < ichar(b(i:i))
                return
            end if
        end do

        string_lessp = len(a) < len(b)
    end function string_lessp

    !> Setup SMIE navigation and indentation.
    !> GRAMMAR is a grammar table generated by `smie_prec2_to_grammar'.
    !> RULES_FUNCTION is a set of indentation rules for use on `smie_rules_function'.
    !> FORWARD_TOKEN and BACKWARD_TOKEN, if present, are the lexer
    !> functions (upstream's :forward-token and :backward-token keywords).
    !> Port of smie.el:smie-setup.
    subroutine smie_setup(grammar_in, rules_function, forward_token, backward_token)
        type(smie_grammar_type), intent(in), target :: grammar_in
        procedure(smie_rules_proc) :: rules_function
        procedure(smie_token_proc), optional :: forward_token, backward_token
        type(smie_string_type), allocatable :: closers(:)
        type(smie_string_type) :: tmp
        character(len=:), allocatable :: closer, trig
        integer, allocatable :: trigs(:), cs(:)
        integer :: k, i, j
        logical :: dup

        if (cur == 0) return
        k = local_index(.true.)
        locals(k)%has_rules = .true.
        locals(k)%rules => rules_function
        locals(k)%grammar => grammar_in
        call indent_line_function%set_local(smie_indent_line)

        ! (add-function :around (local 'normal-auto-fill-function) #'smie-auto-fill),
        ! which does nothing when the advice is already there.
        if (.not. locals(k)%has_auto_fill) then
            locals(k)%has_auto_fill = .true.
            locals(k)%auto_fill_local = normal_auto_fill_function%local_p()
            locals(k)%auto_fill => normal_auto_fill_function%value
            call normal_auto_fill_function%set_local(smie_auto_fill_function)
        end if

        call forward_expression_function%set_local(smie_forward_expression_command)
        if (present(forward_token)) locals(k)%forward_token => forward_token
        if (present(backward_token)) locals(k)%backward_token => backward_token
        if (.not. grammar_in%has_closer_table) return
        locals(k)%has_closer_table = .true.
        ! Only needed for interactive calls to blink-matching-open.
        call set_local_blink_matching_check_function(smie_blink_matching_check)
        call post_self_insert_hook%add(smie_blink_matching_open, depth=90, local=.true.)

        ! (add-function :around (local 'show-paren-data-function) #'smie--matching-block-data)
        if (.not. locals(k)%has_paren_data) then
            locals(k)%has_paren_data = .true.
            locals(k)%paren_data_local = show_paren_data_function_local_p()
            locals(k)%paren_data => current_show_paren_data_function()
            call set_local_show_paren_data_function(smie_paren_data_function)
        end if

        ! Setup smie-blink-matching-triggers.  Rather than wait for SPC to
        ! blink, try to blink as soon as we type the last char of a block ender.
        closers = grammar_in%closers

        do i = 2, size(closers)
            do j = i, 2, -1
                if (.not. string_lessp(closers(j)%s, closers(j - 1)%s)) exit
                tmp = closers(j)
                closers(j) = closers(j - 1)
                closers(j - 1) = tmp
            end do
        end do

        allocate (trigs(0))

        do i = 1, size(closers)
            closer = closers(i)%s

            ! FIXME: this eliminates prefixes of other closers, but we
            ! should probably eliminate prefixes of other keywords as well.
            if (i < size(closers)) then
                if (len(closer) <= len(closers(i + 1)%s)) then
                    if (closers(i + 1)%s(1:len(closer)) == closer) cycle
                end if
            end if

            cs = string_chars_of(closer)
            if (size(cs) > 0) trigs = [cs(size(cs)), trigs]
        end do

        trig = triggers()

        do i = 1, size(trigs)
            dup = .false.

            do j = 1, i - 1
                if (trigs(j) == trigs(i)) dup = .true.
            end do

            if (.not. dup) trig = trig//char_text(trigs(i))
        end do

        k = local_index(.true.)
        locals(k)%has_triggers = .true.
        locals(k)%triggers = trig
    end subroutine smie_setup

    !> Go to the next line that needs to be reindented (and reindent it).
    !> Port of smie.el:smie--next-indent-change.
    recursive subroutine smie_next_indent_change(status)
        type(status_type), intent(out) :: status
        integer(int64) :: tick, after, shortage
        type(status_type) :: st

        do
            call buffer_chars_modified_tick(current_buffer(), tick, st)
            call indent_according_to_mode(status)
            if (.not. status%ok()) return
            call buffer_chars_modified_tick(current_buffer(), after, st)
            if (tick /= after) exit
            shortage = forward_line(1_int64)
        end do
    end subroutine smie_next_indent_change

    !> Give the variables their defaults and register the commands.
    !> Not a port.
    subroutine syms_of_smie()
        smie_forward_token_function => smie_default_forward_token
        smie_backward_token_function => smie_default_backward_token
        smie_blink_matching_triggers = " "//achar(10)

        if (.not. allocated(smie_indent_functions%entries)) then
            call smie_indent_functions%add(smie_indent_fixindent, append=.true.)
            call smie_indent_functions%add(smie_indent_bob, append=.true.)
            call smie_indent_functions%add(smie_indent_close, append=.true.)
            call smie_indent_functions%add(smie_indent_comment, append=.true.)
            call smie_indent_functions%add(smie_indent_comment_continue, append=.true.)
            call smie_indent_functions%add(smie_indent_comment_close, append=.true.)
            call smie_indent_functions%add(smie_indent_comment_inside, append=.true.)
            call smie_indent_functions%add(smie_indent_inside_string, append=.true.)
            call smie_indent_functions%add(indent_keyword_function, append=.true.)
            call smie_indent_functions%add(smie_indent_after_keyword, append=.true.)
            call smie_indent_functions%add(smie_indent_empty_line, append=.true.)
            call smie_indent_functions%add(smie_indent_exps, append=.true.)
        end if

        call add_local_variables_killer(kill_smie_locals)
        commands%smie_close_block%run => cmd_smie_close_block
        commands%smie_down_group%run => cmd_smie_down_group
        commands%smie_forward_expression_command%run => cmd_smie_forward_expression_command
        commands%smie_backward_expression_command%run => cmd_smie_backward_expression_command
        commands%smie__next_indent_change%run => smie_next_indent_change
    end subroutine syms_of_smie
end module smie_fm
