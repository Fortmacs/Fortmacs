! newcomment_mod.f90 -- (un)comment regions of buffers.
! Ported from GNU Emacs 31.1 lisp/newcomment.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> Commenting and uncommenting: `comment_do_what_i_mean' (M-;), `comment_region',
!> `uncomment_region', `comment_or_uncomment_region', `comment_box',
!> `comment_indent' (`indent_for_comment'), `comment_set_column' (C-x ;),
!> `comment_cut', `comment_line' (C-x C-;) and `comment_indent_new_line'
!> (`indent_new_comment_line'), which `default_indent_new_line' (M-j)
!> calls through `comment_line_break_function' when a comment syntax is
!> defined; and the variables major modes set to describe their comments
!> (`comment_start', `comment_end', `comment_start_skip', ...).
!>
!> Values: `comment_use_syntax' is .false., .true. or
!> COMMENT_USE_SYNTAX_UNDECIDED; `comment_style' one of the COMMENT_STYLE_*
!> constants (the styles of `comment_styles'); `comment_empty_lines' .false.,
!> .true. or COMMENT_EMPTY_LINES_END_OF_LINE; `comment_fill_column' negative for
!> none (use `fill_column'); `comment_padding' a string (upstream's integer
!> N is N spaces, and no padding the empty string).  A function that computes a
!> comment's indentation (`comment_indent_function') gives its answer as
!> a `comment_indent_type' (a column, a range (MIN and MAX), or none
!> (COMMENT_INDENT_NONE).  The region functions take the prefix argument as a number ARGUMENT
!> (absent for no prefix) and UNIVERSAL, .true. when it was C-u.
!>
!> Deferred: `combine_change_calls' (the region functions run the change
!> hooks for each change, as with `comment_combine_change_calls' false),
!> inheriting text properties (`insert_and_inherit' inserts plainly, as
!> everywhere in Fortmacs), buffer-local values of
!> `comment_region_function', `uncomment_region_function' and
!> `comment_quote_nested_function' (one global value each),
!> `use_hard_newlines', `is_comment_valid_prefix''s `string_width' of
!> non-ASCII prefixes (counted as one column per character), and the
!> obsolete aliases `indent_for_comment', `set_comment_column',
!> `cut_comment' and `indent_new_comment_line' as commands of their own
!> (they are the same commands under their upstream names).
module newcomment_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE, PREFIX_NUMBER, PREFIX_RAW, PREFIX_MINUS, command_type
    use error_mod, only: status_type, set_error, format_message, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use data_mod, only: value_eq, append_locals_clearer
    use data_mod, only: integer_setting_type, logical_setting_type, string_setting_type, function_setting_type, forward_setting
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use keyboard_mod, only: last_command, this_command
    use buffer_mod, only: buffers, current, current_buffer, get_or_make_buffer, set_buffer, close_buffer, &
                          generate_new_buffer_name, barf_if_buffer_read_only, fill_column
    use insert_delete_mod, only: insert, insert_before_markers
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use editing_functions_mod, only: interactive_region, point, point_minimum, point_maximum, goto_character, &
                                     is_beginning_of_line, is_end_of_line, is_beginning_of_buffer, is_end_of_buffer, &
                                     character_after, character_before, line_beginning_position, line_end_position, delete_region, &
                                     buffer_substring, narrow_to_region, widen, region_beginning, region_end, character_to_string
    use editing_commands_mod, only: forward_line, forward_character, beginning_of_line, end_of_line, delete_character
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, looking_at, string_match, &
                          match_beginning, match_end, replace_match, regular_expression_quote, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string
    use utility_functions_mod, only: substring, count_characters
    use syntax_mod, only: forward_comment, parse_partial_expression, syntax_parser_state, parse_state_type, skip_syntax_backward, &
                          skip_syntax_forward, skip_characters_forward, skip_characters_backward, string_to_syntax, &
                          syntax_descriptor_type, syntax_entry, syntax_table, set_syntax_table, COMMENTSTOP_COMMENT
    use text_properties_mod, only: get_text_property
    use indent_mod, only: current_column, current_indentation, move_to_column, indent_to, indent_according_to_mode, &
                          indent_to_left_margin, current_left_margin, move_to_left_margin, indent_tabs_mode
    use display_engine_mod, only: message, tab_width
    use minibuffer_reading_mod, only: read_string
    use fill_mod, only: adaptive_fill_mode
    use simple_mod, only: newline, delete_horizontal_space, cut_region, is_use_region, back_to_indentation, &
                          comment_line_break_function, fill_prefix, simple_count_lines => count_lines
    use data_mod, only: choice_setting_type
    use buffer_local_setting_mod, only: buffer_local_setting_type
    implicit none(type, external)
    private

    public :: initialize_newcomment, comment_indent_type, comment_indent_procedure, comment_region_procedure, &
              comment_quote_nested_procedure
    public :: comment_normalize_variables, comment_string_strip, comment_string_reverse, comment_search_forward, &
              comment_search_backward, comment_beginning, comment_forward, comment_enter_backward, &
              comment_indent_default, comment_choose_indent, comment_indent, comment_set_column, comment_cut, &
              comment_padright, comment_padleft, uncomment_region, uncomment_region_default, comment_region, &
              comment_region_default, comment_box, is_comment_only, comment_or_uncomment_region, comment_do_what_i_mean, &
              comment_add_count, comment_indent_new_line, comment_line, is_comment_valid_prefix, &
              comment_quote_nested_default, comment_make_beginning_of_line_whitespace

    !> Values of `comment_use_syntax' besides .true. and .false.
    integer, parameter, public :: COMMENT_USE_SYNTAX_UNDECIDED = 2

    !> `comment_style' values: the styles of `comment_styles'.
    integer, parameter, public :: COMMENT_STYLE_PLAIN = 1, COMMENT_STYLE_INDENT_OR_TRIPLE = 2, COMMENT_STYLE_INDENT = 3, &
                                  COMMENT_STYLE_ALIGNED = 4, COMMENT_STYLE_BOX = 5, COMMENT_STYLE_EXTRA_LINE = 6, &
                                  COMMENT_STYLE_MULTI_LINE = 7, COMMENT_STYLE_BOX_MULTI = 8

    !> Values of `comment_empty_lines' besides .true. and .false.
    integer, parameter, public :: COMMENT_EMPTY_LINES_END_OF_LINE = 2

    !> What a `comment_indent_function' answers: a column, a range of
    !> acceptable columns (MIN and MAX), or nothing.
    integer, parameter, public :: COMMENT_INDENT_NONE = 0, COMMENT_INDENT_COLUMN = 1, COMMENT_INDENT_RANGE = 2

    !> The answer of a `comment_indent_function'.
    type :: comment_indent_type
        integer :: kind = COMMENT_INDENT_NONE
        integer(int64) :: column = 0, min = 0, max = 0
    end type comment_indent_type

    abstract interface
        !> Compute the desired indentation of the comment starting at point.
        !> Not a port.
        subroutine comment_indent_procedure(indent, status)
            import :: comment_indent_type, status_type
            implicit none(type, external)
            type(comment_indent_type), intent(out) :: indent
            type(status_type), intent(out) :: status
        end subroutine comment_indent_procedure

        !> Comment or uncomment BEGINNING .. END (correctly ordered), with the
        !> prefix argument ARGUMENT (absent for no prefix; UNIVERSAL true when it was a
        !> C-u list, ARGUMENT then its numeric value).  Not a port.
        subroutine comment_region_procedure(beginning, end, status, argument, universal)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: argument
            logical, intent(in), optional :: universal
        end subroutine comment_region_procedure

        !> Quote (or with UNP unquote) the comment delimiters CS and CE in
        !> the buffer, narrowed to a single comment.  Not a port.
        subroutine comment_quote_nested_procedure(cs, ce, unp, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: cs, ce
            logical, intent(in) :: unp
            type(status_type), intent(out) :: status
        end subroutine comment_quote_nested_procedure
    end interface

    !> One buffer's own value of
    !> `comment_indent_function'.
    type :: comment_indent_entry_type
        procedure(comment_indent_procedure), pointer, nopass :: procedure => null()
    end type comment_indent_entry_type

    !> The type of
    !> `comment_indent_function':
    !> the global value `procedure' (set with `=>'; null for none) and
    !> the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: comment_indent_setting_type
        procedure(comment_indent_procedure), pointer, nopass :: procedure => null()
        type(comment_indent_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_comment_indent_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_comment_indent_current
        procedure :: append_local_entry => append_comment_indent_entry
        procedure :: clear_local_entry => clear_comment_indent_entry
    end type comment_indent_setting_type

    !> True if syntax-tables can be used instead of regexps.
    !> Can also be `undecided' which means that a somewhat expensive test will
    !> be used to try to determine whether syntax-tables should be trusted
    !> to understand comments or not in the given buffer.
    !> Major modes should set this variable.  Port of newcomment.el
    !> comment-use-syntax (.false., .true. or COMMENT_USE_SYNTAX_UNDECIDED).
    type(choice_setting_type), target, public :: comment_use_syntax = &
                                                 choice_setting_type(name="comment_use_syntax", choice=COMMENT_USE_SYNTAX_UNDECIDED)

    !> Column to use for `comment_indent'.  If negative, use
    !> `fill_column' instead.  Port of newcomment.el comment-fill-column.
    type(integer_setting_type), target, public :: comment_fill_column = integer_setting_type(name="comment_fill_column", value=-1)

    !> Column to indent right-margin comments to.
    !> Each mode may establish a different default value for this variable; you
    !> can set the value for a particular mode using that mode's hook.
    !> Comments might be indented to a different value in order not to go beyond
    !> `comment_fill_column' or in order to align them with surrounding comments.
    !> Port of newcomment.el comment-column (buffer-local when set).
    type(integer_setting_type), target, public :: comment_column = integer_setting_type(name="comment_column", value=32)

    !> String to insert to start a new comment, or unallocated if no comment
    !> syntax.  Port of newcomment.el comment-start.
    type(string_setting_type), target, public :: comment_start = string_setting_type(name="comment_start")

    !> Regexp to match the start of a comment plus everything up to its body.
    !> If there are any \(...\) groups and `comment_use_syntax' is .false.,
    !> the comment delimiter text is held to begin at the place matched
    !> by the close of the first group.  Port of newcomment.el
    !> comment-start-skip.
    type(string_setting_type), target, public :: comment_start_skip = string_setting_type(name="comment_start_skip")

    !> Regexp to match the end of a comment plus everything back to its
    !> body.  Port of newcomment.el comment-end-skip.
    type(string_setting_type), target, public :: comment_end_skip = string_setting_type(name="comment_end_skip")

    !> String to insert to end a new comment.
    !> Should be an empty string if comments are terminated by end-of-line.
    !> Port of newcomment.el comment-end.
    type(string_setting_type), target, public :: comment_end = string_setting_type(name="comment_end")

    !> Function to compute desired indentation for a comment.
    !> This function is called with no args with point at the beginning
    !> of the comment's starting delimiter and should return either the
    !> desired column indentation, a range of acceptable
    !> indentation (MIN and MAX), or none.
    !> If none is returned, indentation is delegated to
    !> `indent_according_to_mode'.  Port of newcomment.el
    !> comment-indent-function (`set_local' gives a buffer its own).
    type(comment_indent_setting_type), public :: comment_indent_function

    !> Function to insert a comment when a line doesn't contain one.
    !> The function has no args.
    !>
    !> Applicable at least in modes for languages like fixed-format Fortran where
    !> comments always start in column zero.  Port of newcomment.el
    !> comment-insert-comment-function.
    type(function_setting_type), target, public :: comment_insert_comment_function = &
                                                   function_setting_type(name="comment_insert_comment_function")

    !> If true (the default), use `combine_change_calls' around
    !> calls of `comment_region_function' and
    !> `uncomment_region_function'.  This Substitutes a single call to
    !> each of the hooks `before_change_functions' and
    !> `after_change_functions' in place of those hooks being called
    !> for each individual buffer change.  Port of newcomment.el
    !> comment-combine-change-calls (combine-change-calls is Deferred).
    type(logical_setting_type), target, public :: comment_combine_change_calls = &
                                                  logical_setting_type(name="comment_combine_change_calls", value=.true.)

    !> Function to comment a region.
    !> Its args are the same as those of `comment_region', but BEGINNING and END are
    !> guaranteed to be correctly ordered.  It is called within `save_excursion'.
    !>
    !> Applicable at least in modes for languages like fixed-format Fortran where
    !> comments always start in column zero.  Port of newcomment.el
    !> comment-region-function.
    procedure(comment_region_procedure), pointer, public :: comment_region_function => null()

    !> Function to uncomment a region.
    !> Its args are the same as those of `uncomment_region', but BEGINNING and END are
    !> guaranteed to be correctly ordered.  It is called within `save_excursion'.
    !>
    !> Applicable at least in modes for languages like fixed-format Fortran where
    !> comments always start in column zero.  Port of newcomment.el
    !> uncomment-region-function.
    procedure(comment_region_procedure), pointer, public :: uncomment_region_function => null()

    !> True if nested comments should be quoted.
    !> This should be locally set by each major mode if needed.  Port of
    !> newcomment.el comment-quote-nested.
    type(logical_setting_type), target, public :: comment_quote_nested = &
                                                  logical_setting_type(name="comment_quote_nested", value=.true.)

    !> Function to quote nested comments in a region.
    !> It takes the same arguments as `comment_quote_nested_default',
    !> and is called with the buffer narrowed to a single comment.  Port of
    !> newcomment.el comment-quote-nested-function.
    procedure(comment_quote_nested_procedure), pointer, public :: comment_quote_nested_function => null()

    !> Continuation string to insert for multiline comments.
    !> This string will be added at the beginning of each line except the very
    !> first one when commenting a region with a commenting style that allows
    !> comments to span several lines.
    !> It should generally have the same length as `comment_start' in order to
    !> preserve indentation.
    !> If it is unallocated a value will be automatically derived from
    !> `comment_start' by replacing its first character with a space.  Port
    !> of newcomment.el comment-continue.
    type(string_setting_type), target, public :: comment_continue = string_setting_type(name="comment_continue")

    !> How many more comment chars should be inserted by `comment_region'.
    !> This determines the default value of the numeric argument of `comment_region'.
    !> The `plain' comment style doubles this value.
    !>
    !> This should generally stay 0, except for a few modes like Lisp where
    !> it is 1 so that regions are commented with two or three semi-colons.
    !> Port of newcomment.el comment-add.
    type(integer_setting_type), target, public :: comment_add = integer_setting_type(name="comment_add", value=0)

    !> Comment region style definitions.
    !> Each style is defined with a form (STYLE . (MULTI ALIGN EXTRA INDENT DOC)).
    !> DOC should succinctly describe the style.
    !> STYLE should be a mnemonic name.
    !> MULTI specifies that comments are allowed to span multiple lines.
    !> ALIGN specifies that the `comment_end' markers should be aligned.
    !> EXTRA specifies that an extra line should be used before and after the
    !>   region to comment (to put the `comment_end' and `comment_start').
    !> INDENT specifies that the `comment_start' markers should not be put at the
    !>   left margin but at the current indentation of the region to comment.
    !> If INDENT is `multi_character', that means indent multi-character
    !>   comment starters, but not one-character comment starters.  Port of
    !> newcomment.el comment-styles (the COMMENT_STYLE_* constants index
    !> these arrays; INDENT 2 stands for `multi_character').
    logical, parameter :: style_multi(8) = [.false., .false., .false., .false., .false., .true., .true., .true.]
    logical, parameter :: style_align(8) = [.false., .false., .false., .true., .true., .false., .false., .true.]
    logical, parameter :: style_extra(8) = [.false., .false., .false., .false., .true., .true., .false., .true.]
    integer, parameter :: style_indent(8) = [0, 2, 1, 1, 1, 1, 1, 1]

    !> Style to be used for `comment_region'.
    !> See `comment_styles' for a list of available styles.  Port of
    !> newcomment.el comment-style (a COMMENT_STYLE_* constant).
    type(integer_setting_type), target, public :: comment_style = integer_setting_type(name="comment_style", &
                                                                                       value=COMMENT_STYLE_INDENT)

    !> Padding string that `comment_region' puts between comment chars and text.
    !> Can also be an integer which will be automatically turned into a string
    !> of the corresponding number of spaces.
    !>
    !> Extra spacing between the comment characters and the comment text
    !> makes the comment easier to read.  Default is 1.  An empty string
    !> means 0.  Port of newcomment.el comment-padding (a string).
    type(string_setting_type), target, public :: comment_padding = string_setting_type(name="comment_padding")

    !> Inline comments have to be preceded by at least this many spaces.
    !> This is useful when style-conventions require a certain minimal offset.
    !> Python's PEP8 for example recommends two spaces, so you could do:
    !>
    !>     call comment_inline_offset%set_local(2)
    !>
    !> in a procedure added to `python_mode_hook'.
    !>
    !> See `comment_padding' for whole-line comments.  Port of newcomment.el
    !> comment-inline-offset.
    type(integer_setting_type), target, public :: comment_inline_offset = integer_setting_type(name="comment_inline_offset", &
                                                                                               value=1)

    !> True means `comment_indent_new_line' continues comments.
    !> That is, it inserts no new terminator or starter.
    !> This affects `auto_fill_mode', which is the main reason to
    !> customize this variable.
    !>
    !> It also affects \[indent_new_comment_line].  However, if you want this
    !> behavior for explicit filling, you might as well use \[newline_and_indent].
    !> Port of newcomment.el comment-multi-line.
    type(logical_setting_type), target, public :: comment_multi_line = logical_setting_type(name="comment_multi_line", &
                                                                                            value=.false.)

    !> If .false., `comment_region' does not comment out
    !> empty lines.  If .true., it always comments out empty
    !> lines.  If COMMENT_EMPTY_LINES_END_OF_LINE, it only comments out empty lines
    !> if comments are terminated by the end of line (i.e., `comment_end'
    !> is empty).  Port of newcomment.el comment-empty-lines.
    type(choice_setting_type), target, public :: comment_empty_lines = &
                                                 choice_setting_type(name="comment_empty_lines")

    !> Function to set up variables needed by commenting functions (null
    !> to do nothing).  Port of newcomment.el comment-setup-function.
    procedure(no_argument_procedure), pointer, public :: comment_setup_function => null()

    !> True means that the global syntactic context is used.
    !> More specifically, it means that `syntax_parser_state' is used to find out whether
    !> point is within a string or not.  Major modes whose syntax is not faithfully
    !> described by the syntax-tables (or where `font_lock_syntax_table' is radically
    !> different from the main syntax table) can set this to false,
    !> then `syntax_parser_state' cache won't be used in comment-related routines.
    !> Port of newcomment.el comment-use-global-state (obsolete).
    logical, public :: comment_use_global_state = .true.

    !> True means to only auto-fill inside comments.
    !> This has no effect in modes that do not define a comment syntax.
    !> Port of newcomment.el comment-auto-fill-only-comments.
    type(logical_setting_type), target, public :: comment_auto_fill_only_comments = &
                                                  logical_setting_type(name="comment_auto_fill_only_comments", value=.false.)

    abstract interface
        !> A procedure of no arguments.  Not a port.
        subroutine no_argument_procedure()
            implicit none(type, external)
        end subroutine no_argument_procedure
    end interface

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    !> The restriction saved by `save_restriction': markers at BEGINNING_OF_ACCESSIBLE_PORTION and END_OF_ACCESSIBLE_PORTION
    !> when the buffer is narrowed.
    type :: restriction_type
        logical :: narrowed = .false.
        type(handle_type) :: beginning, end
    end type restriction_type

    !> Upstream's name comment-line-backward, which is not a command: what
    !> `this_command' becomes when `comment_line' goes backward, so that
    !> repeating it goes on backward (named in `initialize_newcomment').
    type(command_type), target :: comment_line_backward

contains

    !> Define the commenting commands and variables (bindings_mod binds
    !> M-;, C-x ; and C-x C-;, as bindings.el does).  Not a port.
    subroutine initialize_newcomment()
        comment_line_backward%name = "comment_line_backward"
        comment_line_backward%is_command = .false.
        call forward_setting(comment_use_syntax)
        call forward_setting(comment_column, local_if_set=.true.)
        call forward_setting(comment_start)
        call forward_setting(comment_start_skip)
        call forward_setting(comment_end_skip)
        call forward_setting(comment_end)
        call forward_setting(comment_insert_comment_function)
        call forward_setting(comment_combine_change_calls, local_if_set=.true.)
        call forward_setting(comment_quote_nested)
        call forward_setting(comment_continue)
        call forward_setting(comment_add)
        call forward_setting(comment_inline_offset)
        call forward_setting(comment_fill_column)
        call forward_setting(comment_style)
        call forward_setting(comment_padding)
        call forward_setting(comment_multi_line)
        call forward_setting(comment_empty_lines)
        call forward_setting(comment_auto_fill_only_comments)
        if (.not. allocated(comment_end%value)) comment_end = ""
        if (.not. allocated(comment_padding%value)) comment_padding = " "
        call append_locals_clearer(clear_newcomment_locals)
        if (.not. associated(comment_indent_function%procedure)) comment_indent_function%procedure => comment_indent_default
        if (.not. associated(comment_region_function)) comment_region_function => comment_region_default
        if (.not. associated(uncomment_region_function)) uncomment_region_function => uncomment_region_default
        if (.not. associated(comment_quote_nested_function)) comment_quote_nested_function => comment_quote_nested_default
        comment_line_break_function => comment_line_break
        commands%comment_indent%execute => execute_comment_indent
        commands%comment_set_column%execute => execute_comment_set_column
        commands%comment_cut%execute => execute_comment_cut
        commands%uncomment_region%execute => execute_uncomment_region
        commands%comment_region%execute => execute_comment_region
        commands%comment_box%execute => execute_comment_box
        commands%comment_or_uncomment_region%execute => execute_comment_or_uncomment_region
        commands%comment_do_what_i_mean%execute => execute_comment_do_what_i_mean
        commands%comment_indent_new_line%execute => execute_comment_indent_new_line
        commands%comment_line%execute => execute_comment_line
    end subroutine initialize_newcomment

    !> Give the current buffer its own value LOCAL_PROCEDURE (null when
    !> absent) of
    !> `comment_indent_function'.  Not a port.
    subroutine set_comment_indent_local(self, local_procedure)
        class(comment_indent_setting_type), intent(inout) :: self
        procedure(comment_indent_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_comment_indent_local

    !> The current buffer's value of
    !> `comment_indent_function':
    !> its own, else the global one.  Not a port.
    function get_comment_indent_current(self) result(current_procedure)
        class(comment_indent_setting_type), intent(in) :: self
        procedure(comment_indent_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_comment_indent_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_comment_indent_entry(self)
        class(comment_indent_setting_type), intent(inout) :: self
        type(comment_indent_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_comment_indent_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_comment_indent_entry(self, k)
        class(comment_indent_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_comment_indent_entry

    !> kill-all-local-variables: forget the current buffer's
    !> `comment_indent_function'.  Not a port.
    subroutine clear_newcomment_locals()
        call comment_indent_function%clear_local()
    end subroutine clear_newcomment_locals

    ! ------------------------------------------------------------------
    ! Small helpers.
    !> `string_match' of REGULAR_EXPRESSION on S: the 0-based index of the match, -1 for
    !> none.  Not a port.
    integer(int64) function smatch(regular_expression, s) result(index)
        character(len=*), intent(in) :: regular_expression, s
        type(status_type) :: st

        index = string_match(regular_expression, s, st) - 1
        if (.not. st%ok()) index = -1
    end function smatch

    !> The 0-based (string) start of group N of the last string match, -1
    !> if it did not match.  Not a port.
    integer(int64) function smb(n)
        integer, intent(in) :: n
        type(status_type) :: st

        smb = match_beginning(n, st) - 1
    end function smb

    !> The 0-based (string) end of group N of the last string match, -1 if
    !> it did not match.  Not a port.
    integer(int64) function sme(n)
        integer, intent(in) :: n
        type(status_type) :: st

        sme = match_end(n, st) - 1
    end function sme

    !> Buffer position of the start of group N of the last match (0 for
    !> none).  Not a port.
    integer(int64) function mb(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mb = match_beginning(n, st)
    end function mb

    !> Buffer position of the end of group N of the last match (0 for
    !> none).  Not a port.
    integer(int64) function me(n)
        integer, intent(in) :: n
        type(status_type) :: st

        me = match_end(n, st)
    end function me

    !> `looking_at' of REGULAR_EXPRESSION.  Not a port.
    logical function looking(regular_expression)
        character(len=*), intent(in) :: regular_expression
        type(status_type) :: st

        looking = looking_at(regular_expression, st)
        if (.not. st%ok()) looking = .false.
    end function looking

    !> `regular_expression_search_forward' of REGULAR_EXPRESSION with BOUND and NOERROR; .true. when found.
    !> Not a port.
    logical function regular_expression_forward(regular_expression, bound, noerror)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in), optional :: bound
        integer, intent(in) :: noerror
        integer(int64) :: found
        type(status_type) :: st

        call regular_expression_search_forward(regular_expression, found, st, bound=bound, noerror=noerror)
        regular_expression_forward = st%ok() .and. found > 0
    end function regular_expression_forward

    !> `regular_expression_search_backward' of REGULAR_EXPRESSION with BOUND and NOERROR; .true. when found.
    !> Not a port.
    logical function regular_expression_backward(regular_expression, bound, noerror)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in), optional :: bound
        integer, intent(in) :: noerror
        integer(int64) :: found
        type(status_type) :: st

        call regular_expression_search_backward(regular_expression, found, st, bound=bound, noerror=noerror)
        regular_expression_backward = st%ok() .and. found > 0
    end function regular_expression_backward

    !> The character at POSITION (default point), -1 if none.  Not a port.
    integer function after(position)
        integer(int64), intent(in), optional :: position
        integer(int32) :: c
        logical :: found

        call character_after(c, found, position)
        after = -1
        if (found) after = c
    end function after

    !> The character before POSITION (default point), -1 if none.  Not a port.
    integer function before(position)
        integer(int64), intent(in), optional :: position
        integer(int32) :: c
        logical :: found

        call character_before(c, found, position)
        before = -1
        if (found) before = c
    end function before

    !> The text from FROM to TO.  Not a port.
    function text(from, to) result(s)
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(min(from, to), max(from, to), s, st)
        if (.not. allocated(s)) s = ""
    end function text

    !> A string of N copies of character C (a byte string).  Not a port.
    function repeat_character(c, n) result(s)
        character(len=*), intent(in) :: c
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s

        s = repeat(c, int(max(n, 0_int64)))
    end function repeat_character

    !> The 0-based characters FROM to TO (exclusive; default the end) of S.
    !> Not a port.
    function sub(s, from, to) result(r)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: from
        integer(int64), intent(in), optional :: to
        character(len=:), allocatable :: r
        type(status_type) :: st

        r = substring(s, from, to, st)
        if (.not. st%ok()) r = ""
    end function sub

    !> The current buffer's comment-start, empty if it is unset.  Not a port.
    function cstart() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(comment_start%value)) s = comment_start%value
    end function cstart

    !> The current buffer's comment-end.  Not a port.
    function cend() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(comment_end%value)) s = comment_end%value
    end function cend

    !> The current buffer's comment-start-skip, empty if it is unset.
    !> Not a port.
    function cskip() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(comment_start_skip%value)) s = comment_start_skip%value
    end function cskip

    !> The padding, as a string.  Not a port.
    function cpad() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(comment_padding%value)) s = comment_padding%value
    end function cpad

    !> Whether comment-use-syntax is true (anything but .false.: COMMENT_USE_SYNTAX_UNDECIDED
    !> counts as true, as upstream).  Not a port.
    logical function use_syntax()
        use_syntax = .not. comment_use_syntax%is_off()
    end function use_syntax

    !> A marker at POSITION that advances when text is inserted at it if
    !> INSERTION_TYPE.  Not a port.
    function marker_at(position, insertion_type) result(m)
        integer(int64), intent(in) :: position
        logical, intent(in), optional :: insertion_type
        type(handle_type) :: m
        type(status_type) :: st
        logical :: it

        it = .false.
        if (present(insertion_type)) it = insertion_type
        call copy_marker_at(position, m, it, st)
    end function marker_at

    !> The position of marker M.  Not a port.
    integer(int64) function marked_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, marked_position, st)
    end function marked_position

    !> Release marker M.  Not a port.
    subroutine unmark(m)
        type(handle_type), intent(inout) :: m
        type(status_type) :: st

        call destroy_marker(m, st)
    end subroutine unmark

    !> Save the current restriction (save-restriction).  Not a port.
    function save_restriction() result(r)
        type(restriction_type) :: r

        r%narrowed = buffers(current)%beginning_of_accessible_portion /= 1 .or. buffers(current)%end_of_accessible_portion &
                     /= buffers(current)%text%z

        if (r%narrowed) then
            r%beginning = marker_at(buffers(current)%beginning_of_accessible_portion)
            r%end = marker_at(buffers(current)%end_of_accessible_portion, .true.)
        end if
    end function save_restriction

    !> Restore restriction R (the end of save-restriction).  Not a port.
    subroutine restore_restriction(r)
        type(restriction_type), intent(inout) :: r
        type(status_type) :: st

        call widen()

        if (r%narrowed) then
            call narrow_to_region(marked_position(r%beginning), marked_position(r%end), st)
            call unmark(r%beginning)
            call unmark(r%end)
        end if
    end subroutine restore_restriction

    ! ------------------------------------------------------------------
    ! Helpers.
    !> Strip STRING of any leading (if BEFOREP) and/or trailing (if AFTERP) space.
    !> Port of newcomment.el:comment-string-strip.
    function comment_string_strip(string, beforep, afterp) result(s)
        character(len=*), intent(in) :: string
        logical, intent(in) :: beforep, afterp
        character(len=:), allocatable :: s
        character(len=:), allocatable :: regular_expression
        integer(int64) :: k

        regular_expression = "\`"
        if (beforep) regular_expression = regular_expression//"\s-*"
        regular_expression = regular_expression//"\(.*?\)"
        if (afterp) regular_expression = regular_expression//"\s-*"//LF//"?"
        regular_expression = regular_expression//"\'"
        k = smatch(regular_expression, string)
        s = match_string(1, string)
    end function comment_string_strip

    !> Return the mirror image of string S, without any trailing space.
    !> Port of newcomment.el:comment-string-reverse.
    function comment_string_reverse(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        character(len=:), allocatable :: reversed
        integer :: i, j

        reversed = ""
        i = 1

        do while (i <= len(s))
            j = i + 1

            do while (j <= len(s))
                if (iand(iachar(s(j:j)), 192) /= 128) exit
                j = j + 1
            end do

            reversed = s(i:j - 1)//reversed
            i = j
        end do

        r = comment_string_strip(reversed, .false., .true.)
    end function comment_string_reverse

    !> Check and set up variables needed by other commenting functions.
    !> All the `comment_*' commands call this function to set up various
    !> variables, like `comment_start', to ensure that the commenting
    !> functions work correctly.  Fortran callers of any other `comment_*'
    !> function should first call this function explicitly.  Port of
    !> newcomment.el:comment-normalize-vars.
    recursive subroutine comment_normalize_variables(status, noerror)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        character(len=:), allocatable :: cs, ce, skip
        logical :: noerr, ok
        type(handle_type) :: old, temporary, table
        type(status_type) :: st

        status = status_type()
        noerr = .false.
        if (present(noerror)) noerr = noerror
        if (associated(comment_setup_function)) call comment_setup_function()
        if (.not. allocated(comment_start%value) .and. noerr) return

        if (.not. allocated(comment_start%value)) then
            call read_string("No comment syntax is defined.  Use: ", cs, status)
            if (.not. status%ok()) return

            if (len(cs) == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "No comment syntax defined")
                return
            end if

            call comment_start%set_local(cs)
            call comment_start_skip%set_local(cs)
        end if

        ! comment-use-syntax
        if (comment_use_syntax%choice == COMMENT_USE_SYNTAX_UNDECIDED) then
            ! Try to skip over a comment using forward-comment
            ! to see if the syntax tables properly recognize it.
            table = syntax_table()
            cs = cstart()
            ce = cend()
            if (ce == "") ce = LF
            old = current_buffer()
            call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st, inhibit_buffer_hooks=.true.)
            call set_buffer(temporary, st)
            call set_syntax_table(table, st)
            call insert(cs//" hello "//ce, st)
            call goto_character(point_minimum())
            ok = forward_comment(1_int64)
            if (ok) ok = is_end_of_buffer()
            call set_buffer(old, st)
            call close_buffer(temporary, st)

            if (ok) then
                call comment_use_syntax%set_local(.true.)
            else
                call comment_use_syntax%set_local(.false.)
            end if
        end if

        ! comment-padding is a string here (nil is "").
        if (.not. allocated(comment_padding%value)) comment_padding = ""
        ! comment-continue
        cs = cstart()
        ce = cend()

        if (.not. allocated(comment_continue%value) .and. ce /= "") then
            if (smatch("\S-\S-", cs) >= 0) then
                call comment_continue%set_local(" "//sub(cs, 1_int64))
            else
                call comment_continue%set_local("|"//sub(cs, 1_int64))
            end if
        end if

        ! comment-skip regexps
        ok = allocated(comment_start_skip%value)
        if (ok) ok = smatch(comment_start_skip%value, cs) >= 0

        if (.not. ok) then
            skip = ""

            if (.not. comment_use_syntax%value) then
                ! `syntax-ppss' will detect escaping.
                skip = "\(\(^\|[^\"//LF//"]\)\(\\\\\)*\)"
            end if

            ! Let's not allow any \s- but only [ \t] since \n
            ! might be both a comment-end marker and \s-.
            skip = skip//"\(?:\s<+\|"//regular_expression_quote(comment_string_strip(cs, .true., .true.))//"+\)[ "//TAB//"]*"
            call comment_start_skip%set_local(skip)
        end if

        if (ce == "") ce = LF
        ok = allocated(comment_end_skip%value)
        if (ok) ok = smatch(comment_end_skip%value, ce) >= 0

        if (.not. ok) then
            if (cend() /= "") ce = comment_string_strip(cend(), .true., .true.)
            ! We use [ \t] rather than \s- because we don't want to
            ! remove ^L in C mode when uncommenting.
            skip = "[ "//TAB//"]*\(\s>"
            if (.not. comment_quote_nested%value) skip = skip//"+"
            skip = skip//"\|"//regular_expression_quote(sub(ce, 0_int64, 1_int64))
            if (.not. (comment_quote_nested%value .and. count_characters(ce) <= 1)) skip = skip//"+"
            skip = skip//regular_expression_quote(sub(ce, 1_int64))//"\)"
            call comment_end_skip%set_local(skip)
        end if
    end subroutine comment_normalize_variables

    !> The regexp matching STRING with a backslash (several with UNP) after
    !> its first character.  Port of newcomment.el:comment-quote-re.
    function comment_quote_regular_expression(string, unp) result(regular_expression)
        character(len=*), intent(in) :: string
        logical, intent(in) :: unp
        character(len=:), allocatable :: regular_expression

        regular_expression = regular_expression_quote(sub(string, 0_int64, 1_int64))//"\\"

        if (unp) then
            regular_expression = regular_expression//"+"
        else
            regular_expression = regular_expression//"*"
        end if

        regular_expression = regular_expression//regular_expression_quote(sub(string, 1_int64))
    end function comment_quote_regular_expression

    !> Quote or unquote nested comments.
    !> If UNP is true, unquote nested comment markers.  Port of
    !> newcomment.el:comment-quote-nested.
    recursive subroutine quote_nested(cs_in, ce_in, unp, status)
        character(len=*), intent(in) :: cs_in, ce_in
        logical, intent(in) :: unp
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs, ce

        status = status_type()
        cs = comment_string_strip(cs_in, .true., .true.)
        ce = comment_string_strip(ce_in, .true., .true.)

        if (comment_quote_nested%value .and. len(ce) > 0) then
            call comment_quote_nested_function(cs, ce, unp, status)
        end if
    end subroutine quote_nested

    !> Quote comment delimiters in the buffer.
    !> It expects to be called with the buffer narrowed to a single comment.
    !> It is used as a default for `comment_quote_nested_function'.
    !>
    !> The arguments CS and CE are strings matching comment starting and
    !> ending delimiters respectively.
    !>
    !> If UNP is true, comments are unquoted instead.
    !>
    !> To quote the delimiters, a \ is inserted after the first
    !> character of CS or CE.  If CE is a single character it will
    !> change CE into !CS.  Port of newcomment.el:comment-quote-nested-default.
    subroutine comment_quote_nested_default(cs, ce, unp, status)
        character(len=*), intent(in) :: cs, ce
        logical, intent(in) :: unp
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer(int64) :: b, e
        type(status_type) :: st

        status = status_type()
        regular_expression = comment_quote_regular_expression(ce, unp)//"\|"//comment_quote_regular_expression(cs, unp)
        call goto_character(point_minimum())

        do while (regular_expression_forward(regular_expression, noerror=SEARCH_NOERROR))
            b = mb(0)
            e = me(0)
            call goto_character(b)
            call forward_character(st, 1_int64)

            if (unp) then
                call delete_character(1_int64, status)
            else
                call insert("\", status)
            end if

            if (.not. status%ok()) return

            if (count_characters(ce) == 1) then
                ! If the comment-end is a single char, adding a \ after that
                ! "first" char won't deactivate it, so we turn such a CE
                ! into !CS.  I.e. for pascal, we turn } into !{
                if (.not. unp) then
                    if (text(b, e) == ce) then
                        call replace_match("!"//cs, status, fixedcase=.true., literal=.true.)
                        if (.not. status%ok()) return
                    end if
                else
                    if (point_minimum() < b) then
                        if (text(b - 1, e - 1) == "!"//cs) then
                            call goto_character(point() - 2)
                            call delete_character(e - b, status)
                            if (.not. status%ok()) return
                            call insert(ce, status)
                            if (.not. status%ok()) return
                        end if
                    end if
                end if
            end if
        end do
    end subroutine comment_quote_nested_default

    ! ------------------------------------------------------------------
    ! Navigation.
    !> Find a comment start between point and LIMIT (0: no limit).
    !> Moves point to inside the comment and returns the position of the
    !> comment-starter.  If no comment is found, moves point to LIMIT
    !> and raises an error or returns 0 if NOERROR is true.
    !>
    !> Ensure that `comment_normalize_variables' has been called before you use
    !> this.  Port of newcomment.el:comment-search-forward.
    recursive integer(int64) function comment_search_forward(limit, noerror, status) result(start)
        integer(int64), intent(in) :: limit
        logical, intent(in) :: noerror
        type(status_type), intent(out) :: status
        type(parse_state_type) :: s, s2, old
        integer(int64) :: point_value, to, position, beginning_of_line, end
        type(status_type) :: st

        status = status_type()
        start = 0

        if (.not. use_syntax()) then
            if (limit > 0) then
                if (regular_expression_forward(cskip(), limit, SEARCH_NOERROR)) then
                    start = me(1)
                    if (start == 0) start = mb(0)
                    return
                end if

                call goto_character(limit)
            else
                if (regular_expression_forward(cskip(), noerror=SEARCH_NOERROR)) then
                    start = me(1)
                    if (start == 0) start = mb(0)
                    return
                end if
            end if

            if (.not. noerror) call set_error(status, GENERIC_ERROR_CODE, "No comment")
            return
        end if

        point_value = point()
        to = point_maximum()
        if (limit > 0) to = limit

        ! Assume (at first) that pt is outside of any string.
        if (comment_use_global_state) then
            call syntax_parser_state(old, point_value)
            call parse_partial_expression(point_value, to, s, st, oldstate=old, commentstop=COMMENTSTOP_COMMENT)
        else
            call parse_partial_expression(point_value, to, s, st, commentstop=COMMENTSTOP_COMMENT)
        end if

        if (s%comstr_start > 0 .and. s%instring /= -1 .and. .not. comment_use_global_state) then
            ! The search ended at eol inside a string.  Try to see if it
            ! works better when we assume that pt is inside a string.
            old = parse_state_type()
            old%instring = s%instring
            call goto_character(point_value)
            call parse_partial_expression(point_value, to, s2, st, oldstate=old, commentstop=COMMENTSTOP_COMMENT)
            s = s2
        end if

        if (.not. (s%comstr_start > 0 .and. s%instring == -1)) then
            if (.not. noerror) call set_error(status, GENERIC_ERROR_CODE, "No comment")
            return
        end if

        ! Make sure the comment starts after POINT.
        if (s%comstr_start < point_value) then
            if (.not. noerror) call set_error(status, GENERIC_ERROR_CODE, "No comment")
            return
        end if

        ! We found the comment.
        position = point()
        start = s%comstr_start
        beginning_of_line = line_beginning_position()
        end = 0

        do while (end == 0 .and. point() >= beginning_of_line)
            if (looking(cskip())) then
                end = min(to, me(0))
            else
                if (point() <= point_minimum()) exit
                call goto_character(point() - 1)
            end if
        end do

        if (end > 0) then
            call goto_character(end)
        else
            call goto_character(position)
        end if
    end function comment_search_forward

    !> Find a comment start between LIMIT (0: none) and point.
    !> Moves point to inside the comment and returns the position of the
    !> comment-starter.  If no comment is found, moves point to LIMIT
    !> and raises an error or returns 0 if NOERROR is true.
    !>
    !> Ensure that `comment_normalize_variables' has been called before you use
    !> this.  Port of newcomment.el:comment-search-backward.
    recursive integer(int64) function comment_search_backward(limit, noerror, status) result(cs)
        integer(int64), intent(in) :: limit
        logical, intent(in) :: noerror
        type(status_type), intent(out) :: status
        integer(int64) :: end, point_value
        logical :: found
        type(status_type) :: st

        status = status_type()
        cs = 0

        if (limit > 0) then
            found = regular_expression_backward(cskip(), limit, SEARCH_MOVE)
        else
            found = regular_expression_backward(cskip(), noerror=SEARCH_MOVE)
        end if

        if (.not. found) then
            if (.not. noerror) call set_error(status, GENERIC_ERROR_CODE, "No comment")
            return
        end if

        end = me(0)
        call beginning_of_line(1_int64)
        cs = comment_search_forward(end, .true., st)
        point_value = point()

        if (cs == 0) then
            call beginning_of_line(1_int64)
            cs = comment_search_backward(limit, noerror, status)
            return
        end if

        do
            call goto_character(cs)
            found = comment_forward(1_int64)
            if (.not. (point() < end)) exit
            cs = comment_search_forward(end, .true., st)
            if (cs == 0) exit
            point_value = point()
        end do

        call goto_character(point_value)
    end function comment_search_backward

    !> Find the beginning of the enclosing comment.
    !> Returns 0 if not inside a comment, else moves point and returns
    !> the same as `comment_search_backward'.  Port of
    !> newcomment.el:comment-beginning.
    recursive integer(int64) function comment_beginning() result(start)
        type(parse_state_type) :: state
        type(restriction_type) :: r
        type(value_type) :: face
        integer(int64) :: point_value, cs
        logical :: is_cs, ok, fwd
        type(status_type) :: st

        start = 0

        if (use_syntax() .and. comment_use_global_state) then
            point_value = point()
            call syntax_parser_state(state, point_value)

            if (state%incomment /= 0) then
                call goto_character(state%comstr_start)
                start = point()
                ! `comment-start-skip' sometimes checks that the
                ! comment char is not escaped.  (Bug#16971)
                r = save_restriction()
                call narrow_to_region(point(), point_maximum(), st)
                ok = looking(cskip())
                call restore_restriction(r)
                if (ok) call goto_character(me(0))
            end if

            return
        end if

        ! Can't rely on the syntax table, let's guess based on font-lock.
        call get_text_property(point(), value_type(name="face"), face, st)
        if (value_eq(face, value_type(name="font_lock_string_face"))) return
        point_value = point()
        cs = comment_search_backward(0_int64, .true., st)
        if (cs == 0) return
        call goto_character(cs)
        ! For modes where comment-start and comment-end are the same,
        ! the search above may have found a `ce' rather than a `cs'.
        is_cs = .false.

        if (allocated(comment_end_skip%value)) then
            is_cs = .not. looking(comment_end_skip%value)
        end if

        if (.not. is_cs) then
            ! Maybe font-lock knows that it's a `cs'?
            call get_text_property(me(0), value_type(name="face"), face, st)
            is_cs = value_eq(face, value_type(name="font_lock_comment_face"))
        end if

        if (.not. is_cs) then
            call get_text_property(point(), value_type(name="face"), face, st)

            if (.not. value_eq(face, value_type(name="font_lock_comment_face"))) then
                ! Let's assume it's a `cs' if we're on the same line.
                is_cs = line_end_position() >= point_value
            end if
        end if

        if (is_cs) then
            ! Make sure that POINT is not past the end of the comment.
            fwd = comment_forward(1_int64)

            if (fwd) then
                is_cs = point() > point_value
            else
                is_cs = is_end_of_buffer()
            end if
        end if

        call goto_character(cs)

        if (is_cs) then
            start = cs
        else
            call goto_character(point_value)
        end if
    end function comment_beginning

    !> Skip forward over N comments.
    !> Just like `forward_comment' but only for positive N
    !> and can use regexps instead of syntax.  Port of
    !> newcomment.el:comment-forward (N must not be negative).
    recursive logical function comment_forward(n_in) result(done)
        integer(int64), intent(in) :: n_in
        integer(int64) :: n
        logical :: ok

        n = n_in

        if (use_syntax()) then
            done = forward_comment(n)
            return
        end if

        do while (n > 0)
            ok = forward_comment(1_int64)

            if (.not. ok) then
                if (looking(cskip())) then
                    call goto_character(me(0))
                    ok = regular_expression_forward(comment_end_skip%value, noerror=SEARCH_MOVE)
                end if
            end if

            if (ok) then
                n = n - 1
            else
                n = -1
            end if
        end do

        done = n == 0
    end function comment_forward

    !> Move from the end of a comment to the end of its content.
    !> Point is assumed to be just at the end of a comment.  Port of
    !> newcomment.el:comment-enter-backward.
    subroutine comment_enter_backward()
        type(restriction_type) :: r
        type(syntax_descriptor_type) :: d3, d4
        integer(int64) :: moved, point_value
        logical :: found
        type(status_type) :: st
        character(len=:), allocatable :: text

        if (is_beginning_of_line()) then
            ! comment-end = ""
            call goto_character(point() - 1)
            moved = skip_syntax_backward(" ")
            return
        end if

        point_value = point()
        r = save_restriction()
        call narrow_to_region(line_beginning_position(), point_value, st)
        call goto_character(point_minimum())
        found = .false.
        if (allocated(comment_end_skip%value)) found = regular_expression_forward(comment_end_skip%value//"\'", &
                                                                                  noerror=SEARCH_NOERROR)
        call restore_restriction(r)

        if (found) then
            call goto_character(mb(0))
            return
        end if

        call goto_character(point_value)
        ! comment-end-skip not found probably because it was not set
        ! right.  Since \\s> should catch the single-char case, let's
        ! check that we're looking at a two-char comment ender.
        call string_to_syntax("  4", d4, st)
        call string_to_syntax("  3", d3, st)

        if (.not. (point_maximum() - line_beginning_position() <= 1 .or. &
                   iand(syntax_code_at(point() - 1), d4%code) == 0 .or. &
                   iand(syntax_code_at(point() - 2), d3%code) == 0)) then
            call goto_character(point() - 2)
            call character_to_string(int(after(), int32), text, st)
            moved = skip_characters_backward(text, st)
            moved = skip_syntax_backward(" ")
        else
            ! No clue what's going on: maybe we're really not right after the
            ! end of a comment.  Maybe we're at the "end" because of END_OF_BUFFER rather
            ! than because of a marker.
            moved = skip_syntax_backward(" ")
        end if
    end subroutine comment_enter_backward

    !> The raw syntax code (with flags) of the character at POSITION (the first element of
    !> syntax-after), 0 outside the buffer.  Not a port.
    integer(int32) function syntax_code_at(position)
        integer(int64), intent(in) :: position
        type(syntax_descriptor_type) :: d
        integer :: c

        syntax_code_at = 0
        c = after(position)
        if (c < 0) return
        d = syntax_entry(c)
        if (d%code > 0) syntax_code_at = d%code
    end function syntax_code_at

    ! ------------------------------------------------------------------
    ! Commands.
    !> Default for `comment_indent_function'.  Port of
    !> newcomment.el:comment-indent-default.
    subroutine comment_indent_default(indent, status)
        type(comment_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status
        logical :: zero

        status = status_type()
        zero = .false.

        if (looking("\s<\s<\(\s<\)?")) then
            zero = me(1) > 0
            if (.not. zero) zero = current_column() /= current_indentation()
        end if

        if (zero) then
            indent%kind = COMMENT_INDENT_COLUMN
            indent%column = 0
            return
        end if

        if (current_column() /= current_indentation()) then
            indent%kind = COMMENT_INDENT_COLUMN
            indent%column = comment_column%value
        else if (comment_add%value > 0) then
            if (looking("\s<\(\S<\|\'\)")) then
                indent%kind = COMMENT_INDENT_COLUMN
                indent%column = comment_column%value
            end if
        end if
    end subroutine comment_indent_default

    !> `fill_column', or `comment_fill_column' when it is set.  Not a port.
    integer(int64) function the_fill_column()
        the_fill_column = fill_column%value
        if (comment_fill_column%value >= 0) the_fill_column = comment_fill_column%value
    end function the_fill_column

    !> Choose the indentation to use for a right-hand-side comment.
    !> The criteria are (in this order):
    !> - try to keep the comment's text within `comment_fill_column'.
    !> - try to align with surrounding comments.
    !> - prefer INDENT (or `comment_column' if absent).
    !> Point is expected to be at the start of the comment.  Port of
    !> newcomment.el:comment-choose-indent.
    recursive integer(int64) function comment_choose_indent(indent_in) result(column)
        type(comment_indent_type), intent(in), optional :: indent_in
        integer(int64) :: indent, min, max, other, original_point, omax, omin, moved, end_of_line_column
        logical :: has_other, is_range
        type(status_type) :: st

        indent = comment_column%value
        original_point = point()
        is_range = .false.

        if (present(indent_in)) then
            if (indent_in%kind == COMMENT_INDENT_COLUMN) indent = indent_in%column
            is_range = indent_in%kind == COMMENT_INDENT_RANGE
        end if

        if (is_range) then
            min = indent_in%min
            max = indent_in%max
            indent = comment_column%value
        else
            ! Avoid moving comments past the fill-column.
            call end_of_line(1_int64)
            end_of_line_column = current_column()
            call goto_character(original_point)
            max = current_column() + (the_fill_column() - end_of_line_column)
            moved = skip_characters_backward(" "//TAB, st)

            ! Leave at least `comment-inline-offset' space after
            ! other nonwhite text on the line.
            if (is_beginning_of_line()) then
                min = 0
            else
                min = comment_inline_offset%value + current_column()
            end if

            call goto_character(original_point)
        end if

        ! Fix up the range.
        if (max < min) max = min
        ! Don't move past the fill column.
        if (max <= indent) indent = max
        ! We can choose anywhere between min..max.
        ! Let's try to align to a comment on the previous line.
        has_other = .false.
        other = 0

        if (forward_line(-1_int64) == 0) then
            other = comment_search_forward(line_end_position(), .true., st)

            if (other > 0) then
                call goto_character(other)
                other = current_column()
                has_other = .true.
            end if
        end if

        call goto_character(original_point)

        if (has_other) then
            if (other <= max .and. other >= min) then
                ! There is a comment and it's in the range: bingo!
                column = other
                return
            end if
        end if

        ! Can't align to a previous comment: let's try to align to comments
        ! on the following lines, then.  These have not been re-indented yet,
        ! so we can't directly align ourselves with them.  All we do is to try
        ! and choose an indentation point with which they will be able to
        ! align themselves.
        do
            if (forward_line(1_int64) /= 0) exit
            other = comment_search_forward(line_end_position(), .true., st)
            if (other == 0) exit
            call goto_character(other)
            call end_of_line(1_int64)
            end_of_line_column = current_column()
            call goto_character(other)
            omax = current_column() + (the_fill_column() - end_of_line_column)
            moved = skip_characters_backward(" "//TAB, st)
            omin = current_column() + 1
            call goto_character(other)

            if (omax >= min .and. omin <= max) then
                min = merge(omin, min, omin > min)
                max = merge(omax, max, omax < max)
            else
                ! Can't align with this anyway, so exit the loop.
                exit
            end if
        end do

        call goto_character(original_point)
        ! Return the closest point to indent within min..max.
        column = merge(indent, max, indent < max)
        if (column < min) column = min
    end function comment_choose_indent

    !> Indent this line's comment to `comment_column', or insert an empty comment.
    !> If CONTINUE is true, use the `comment_continue' markers if any.
    !> Port of newcomment.el:comment-indent.
    recursive subroutine comment_indent(status, continue)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: continue
        character(len=:), allocatable :: starter, ender
        integer(int64) :: begpos, original_point, moved, n, column
        type(handle_type) :: cpos
        type(comment_indent_type) :: indent
        type(parse_state_type) :: state
        procedure(comment_indent_procedure), pointer :: fn
        logical :: cont, in_multi, at_beginning_of_line
        type(status_type) :: st

        cont = .false.
        if (present(continue)) cont = continue
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_variables(status)
        if (.not. status%ok()) return
        call beginning_of_line(1_int64)

        if (cont .and. allocated(comment_continue%value)) then
            starter = comment_continue%value
            ender = ""
        else if (allocated(comment_start%value)) then
            starter = comment_start%value
            ender = cend()
        else
            call set_error(status, GENERIC_ERROR_CODE, "No comment syntax defined")
            return
        end if

        begpos = comment_search_forward(line_end_position(), .true., st)
        ! If we couldn't find a comment *starting* on this line, see if we
        ! are already within a multiline comment at BEGINNING_OF_LINE (bug#78003).
        in_multi = .false.

        if (begpos == 0 .and. .not. cont .and. use_syntax() .and. comment_use_global_state) then
            original_point = point()
            call syntax_parser_state(state, line_beginning_position())
            in_multi = state%incomment /= 0
            call goto_character(original_point)
        end if

        if (in_multi) then
            ! We don't know anything about the nature of the multiline
            ! construct, so immediately delegate to the mode.
            call indent_according_to_mode(status)
            return
        end if

        if (begpos == 0 .and. associated(comment_insert_comment_function%value)) then
            ! If no comment and c-i-c-f is set, let it do everything.
            call comment_insert_comment_function%value(status)
            return
        end if

        ! An existing comment?
        if (begpos > 0) then
            if (.not. looking("["//TAB//LF//" ]")) then
                if (looking(comment_end_skip%value)) then
                    ! The comment is empty and we have skipped all its space
                    ! and landed right before the comment-ender:
                    ! Go back to the middle of the space.
                    moved = skip_characters_backward(" "//TAB, st)
                    call goto_character(point() + (-moved)/2)
                end if
            end if

            cpos = marker_at(point())
        else
            ! If none, insert one.
            original_point = point()
            ! Some `comment-indent-function's insist on not moving
            ! comments that are in column 0, so we first go to the
            ! likely target column.
            column = indent_to(comment_column%value, status)
            if (.not. status%ok()) return
            ! Ensure there's a space before the comment for things
            ! like sh where it matters (as well as being neater).
            n = before()

            if (.not. (n == -1 .or. n == 10 .or. n == 9 .or. n == 32)) then
                call insert(" ", status)
                if (.not. status%ok()) return
            end if

            begpos = point()
            call insert(starter, status)
            if (.not. status%ok()) return
            cpos = marker_at(point())
            call insert(ender, status)
            if (.not. status%ok()) return
            call goto_character(original_point)
        end if

        call goto_character(begpos)
        ! Compute desired indent.
        original_point = point()
        fn => comment_indent_function%get_current()
        call fn(indent, st)
        call goto_character(original_point)
        ! If `indent' is nil and there's code before the comment, we can't
        ! use `indent-according-to-mode', so we default to comment-column.
        moved = skip_characters_backward(" "//TAB, st)
        at_beginning_of_line = is_beginning_of_line()
        call goto_character(original_point)

        if (indent%kind == COMMENT_INDENT_NONE .and. .not. at_beginning_of_line) then
            indent%kind = COMMENT_INDENT_COLUMN
            indent%column = comment_column%value
        end if

        if (indent%kind == COMMENT_INDENT_NONE) then
            ! comment-indent-function refuses: delegate to line-indent.
            call indent_according_to_mode(status)
        else
            ! If the comment is at the right of code, adjust the indentation.
            if (.not. at_beginning_of_line) then
                column = comment_choose_indent(indent)
            else if (indent%kind == COMMENT_INDENT_COLUMN) then
                column = indent%column
            else
                ! A range at the beginning of a line: (car indent) is a
                ! cons, which upstream's `=' rejects; use MIN.
                column = indent%min
            end if

            ! If that's different from comment's current position, change it.
            if (current_column() /= column) then
                original_point = point()
                moved = skip_characters_backward(" "//TAB, st)
                call delete_region(point(), original_point, status)
                if (.not. status%ok()) return
                column = indent_to(column, status)
            end if
        end if

        call goto_character(marked_position(cpos))
        call unmark(cpos)
    end subroutine comment_indent

    !> `comment_indent' as a command.
    recursive subroutine execute_comment_indent(status)
        type(status_type), intent(out) :: status

        call comment_indent(status)
    end subroutine execute_comment_indent

    !> Set the comment column based on point.
    !> With no ARGUMENT, set the comment column to the current column.
    !> With just minus as arg, kill any comment on this line.
    !> With any other arg, set comment column to indentation of the previous comment
    !>  and then align or create a comment on this line at that column.
    !> Port of newcomment.el:comment-set-column (MINUS for the argument
    !> `-', ARGUMENT present for another argument).
    recursive subroutine comment_set_column(status, argument, minus)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument, minus
        integer(int64) :: original_point, cs
        character(len=24) :: buffer
        type(status_type) :: st

        status = status_type()

        if (present(minus)) then
            if (minus) then
                call comment_cut(status)
                return
            end if
        end if

        if (present(argument)) then
            if (argument) then
                call comment_normalize_variables(status)
                if (.not. status%ok()) return
                original_point = point()
                call beginning_of_line(1_int64)
                cs = comment_search_backward(0_int64, .false., status)

                if (.not. status%ok()) then
                    call goto_character(original_point)
                    return
                end if

                call beginning_of_line(1_int64)
                cs = comment_search_forward(line_end_position(), .false., status)

                if (.not. status%ok()) then
                    call goto_character(original_point)
                    return
                end if

                call goto_character(cs)
                call comment_column%set_local(current_column())
                write (buffer, "(i0)") comment_column%value
                call message("Comment column set to "//trim(buffer))
                call goto_character(original_point)
                call comment_indent(status)
                return
            end if
        end if

        call comment_column%set_local(current_column())
        write (buffer, "(i0)") comment_column%value
        call message("Comment column set to "//trim(buffer))
        st = status_type()
    end subroutine comment_set_column

    !> Set the comment column based on point.
    !> With no ARGUMENT, set the comment column to the current column.
    !> With just minus as arg, kill any comment on this line.
    !> With any other arg, set comment column to indentation of the previous comment
    !>  and then align or create a comment on this line at that column.
    !> Port of newcomment.el:comment-set-column (the command).
    recursive subroutine execute_comment_set_column(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call comment_set_column(status)
        else if (current_prefix_argument%kind == PREFIX_MINUS) then
            call comment_set_column(status, minus=.true.)
        else
            call comment_set_column(status, argument=.true.)
        end if
    end subroutine execute_comment_set_column

    !> Cut the first comment on this line, if any.
    !> With prefix ARGUMENT, cut comments on that many lines starting with this
    !> one.  Port of newcomment.el:comment-kill (ARGUMENT absent for no prefix).
    recursive subroutine comment_cut(status, argument)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        integer(int64) :: i, n, cs, original_point, moved, e, shortage
        logical :: done
        type(status_type) :: st

        call comment_normalize_variables(status)
        if (.not. status%ok()) return
        n = 1
        if (present(argument)) n = argument

        do i = 1, n
            original_point = point()
            call beginning_of_line(1_int64)
            cs = comment_search_forward(line_end_position(), .true., st)

            if (cs > 0) then
                call goto_character(cs)
                moved = skip_syntax_backward(" ")
                cs = point()
                done = comment_forward(1_int64)
                e = point()
                if (is_beginning_of_line()) e = e - 1
                call cut_region(cs, e, status)
                if (.not. status%ok()) return
                call indent_according_to_mode(status)
                if (.not. status%ok()) return
            end if

            ! (save-excursion: point returns to where it was.)
            call goto_character(min(original_point, point_maximum()))
            if (present(argument)) shortage = forward_line(1_int64)
        end do
    end subroutine comment_cut

    !> Cut the first comment on this line, if any.
    !> With prefix ARGUMENT, cut comments on that many lines starting with this
    !> one.  Port of newcomment.el:comment-kill (the command).
    recursive subroutine execute_comment_cut(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call comment_cut(status)
        else
            call comment_cut(status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine execute_comment_cut

    !> Construct a string composed of STRING plus `comment_padding'.
    !> It also adds N copies of the last non-whitespace chars of STRING.
    !> If STRING already contains padding, the corresponding amount is
    !> ignored from `comment_padding'.
    !> N defaults to 0.
    !> If REGULAR_EXPRESSION is true, a regexp is returned instead, that would match
    !> the string for any N (upstream's N `regular_expression').  FOUND is .false.
    !> when STRING is unallocated or has no non-whitespace character.
    !>
    !> Ensure that `comment_normalize_variables' has been called before you
    !> use this.  Port of newcomment.el:comment-padright.
    function comment_padright(string, n, regular_expression, found) result(r)
        character(len=*), intent(in), optional :: string
        integer(int64), intent(in), optional :: n
        logical, intent(in), optional :: regular_expression
        logical, intent(out), optional :: found
        character(len=:), allocatable :: r
        character(len=:), allocatable :: s, lpad, rpad, pad
        integer(int64) :: k, m1b, m1e, m0e, count
        logical :: multi, want_regular_expression
        integer :: i

        r = ""
        if (present(found)) found = .false.
        if (.not. present(string)) return
        if (smatch("\S-", string) < 0) return
        if (present(found)) found = .true.
        count = 0
        if (present(n)) count = n
        want_regular_expression = .false.
        if (present(regular_expression)) want_regular_expression = regular_expression
        ! Separate the actual string from any leading/trailing padding
        k = smatch("\`\s-*\(.*?\)\s-*\'", string)
        m1b = smb(1)
        m1e = sme(1)
        m0e = sme(0)
        s = match_string(1, string)
        lpad = sub(string, 0_int64, m1b)
        pad = cpad()

        if (smatch("\`\s-", pad) < 0) then
            ! If the padding isn't spaces, then don't
            ! shorten the padding.
            rpad = sub(string, m1e)//pad
        else
            rpad = sub(string, m1e)//sub(pad, min(m0e - m1e, count_characters(pad)))
        end if

        ! We can only duplicate C if the comment-end has multiple chars
        ! or if comments can be nested, else the comment-end `}' would
        ! be turned into `}}}' where only the first ends the comment
        ! and the rest becomes bogus junk.
        multi = .not. (comment_quote_nested%value .and. smatch("\`\s-*\S-\s-*\'", cend()) >= 0)

        if (.not. want_regular_expression) then
            r = lpad//s

            if (multi) then
                do i = 1, int(count)
                    r = r//sub(string, m1e - 1, m1e)
                end do
            end if

            r = r//rpad
        else
            ! construct a regexp that would match anything from just S
            ! to any possible output of this function for any N.
            r = optional_characters(lpad)//regular_expression_quote(s)
            if (multi) r = r//"+"
            r = r//optional_characters(rpad)
        end if
    end function comment_padright

    !> The regexp matching each character of S optionally, in order
    !> (padding is not required).  Not a port.
    function optional_characters(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer(int64) :: k

        r = ""

        do k = 0, count_characters(s) - 1
            r = r//regular_expression_quote(sub(s, k, k + 1))//"?"
        end do
    end function optional_characters

    !> Construct a string composed of `comment_padding' plus STRING.
    !> It also adds N copies of the first non-whitespace chars of STRING.
    !> If STRING already contains padding, the corresponding amount is
    !> ignored from `comment_padding'.
    !> N defaults to 0.
    !> If REGULAR_EXPRESSION is true, a regexp is returned instead, that would match the
    !> string for any N.  FOUND is .false. when STRING is absent or empty.
    !>
    !> Ensure that `comment_normalize_variables' has been called before you
    !> use this.  Port of newcomment.el:comment-padleft.
    function comment_padleft(string, n, regular_expression, found) result(r)
        character(len=*), intent(in), optional :: string
        integer(int64), intent(in), optional :: n
        logical, intent(in), optional :: regular_expression
        logical, intent(out), optional :: found
        character(len=:), allocatable :: r
        character(len=:), allocatable :: s, pad, padding, c
        integer(int64) :: k, m0b, m0e, count
        logical :: multi, want_regular_expression
        integer :: i

        r = ""
        if (present(found)) found = .false.
        if (.not. present(string)) return
        if (len(string) == 0) return
        if (present(found)) found = .true.
        count = 0
        if (present(n)) count = n
        want_regular_expression = .false.
        if (present(regular_expression)) want_regular_expression = regular_expression
        ! Only separate the left pad because we assume there is no right pad.
        k = smatch("\`\s-*", string)
        m0b = smb(0)
        m0e = sme(0)
        s = sub(string, m0e)
        padding = cpad()

        if (smatch("\`\s-", padding) < 0) then
            ! If the padding isn't spaces, then don't
            ! shorten the padding.
            pad = padding
        else
            pad = sub(padding, min(m0e - m0b, count_characters(padding)))
        end if

        pad = pad//sub(string, m0b, m0e)
        c = sub(string, m0e, m0e + 1)
        ! We can only duplicate C if the comment-end has multiple chars
        ! or if comments can be nested, else the comment-end `}' would
        ! be turned into `}}}' where only the first ends the comment
        ! and the rest becomes bogus junk.
        multi = .not. (comment_quote_nested%value .and. smatch("\`\s-*\S-\s-*\'", cend()) >= 0)

        if (.not. want_regular_expression) then
            r = pad

            if (multi) then
                do i = 1, int(count)
                    r = r//c
                end do
            end if

            r = r//s
        else
            ! Construct a regexp that would match anything from just S
            ! to any possible output of this function for any N.
            ! We match any number of leading spaces because this regexp will
            ! be used for uncommenting where we might want to remove
            ! uncomment markers with arbitrary leading space (because
            ! they were aligned).
            r = "\s-*"
            if (multi) r = r//regular_expression_quote(c)//"*"
            r = r//regular_expression_quote(s)
        end if
    end function comment_padleft

    !> Uncomment each line in the BEGINNING .. END region.
    !> The numeric prefix ARGUMENT can specify a number of chars to remove from the
    !> comment delimiters.  Port of newcomment.el:uncomment-region.
    recursive subroutine uncomment_region(beginning_in, end_in, status, argument, universal)
        integer(int64), intent(in) :: beginning_in, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        logical, intent(in), optional :: universal
        integer(int64) :: beginning, end, original_point
        logical :: saved
        type(handle_type) :: om

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_variables(status)
        if (.not. status%ok()) return
        beginning = min(beginning_in, end_in)
        end = max(beginning_in, end_in)
        ! Bind `comment-use-global-state' to nil.  While uncommenting a region
        ! (which works a line at a time), a comment can appear to be
        ! included in a multi-line string, but it is actually not.
        saved = comment_use_global_state
        comment_use_global_state = .false.
        original_point = point()
        om = marker_at(original_point)
        call uncomment_region_function(beginning, end, status, argument, universal)
        comment_use_global_state = saved
        call goto_character(marked_position(om))
        call unmark(om)
    end subroutine uncomment_region

    !> Uncomment each line in the BEGINNING .. END region.
    !> The numeric prefix ARGUMENT can specify a number of chars to remove from the
    !> comment delimiters.  Port of newcomment.el:uncomment-region (the
    !> command, on the region).
    recursive subroutine execute_uncomment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call with_prefix(beginning, end, status, uncomment_region)
    end subroutine execute_uncomment_region

    !> Call REGION_FN on BEGINNING..END with the current prefix argument.
    !> Not a port.
    recursive subroutine with_prefix(beginning, end, status, region_fn)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        procedure(comment_region_procedure) :: region_fn

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call region_fn(beginning, end, status)
        else if (current_prefix_argument%kind == PREFIX_RAW) then
            call region_fn(beginning, end, status, prefix_numeric_value(current_prefix_argument), .true.)
        else
            call region_fn(beginning, end, status, prefix_numeric_value(current_prefix_argument))
        end if
    end subroutine with_prefix

    !> (region-beginning) and (region-end).  Not a port.
    subroutine region_bounds(beginning, end, status)
        integer(int64), intent(out) :: beginning, end
        type(status_type), intent(out) :: status

        call region_beginning(beginning, status)
        if (.not. status%ok()) return
        call region_end(end, status)
    end subroutine region_bounds

    !> Uncomment each line in the BEGINNING .. END region.
    !> The numeric prefix ARGUMENT can specify a number of chars to remove from the
    !> comment delimiters.
    !> This function is the default value of `uncomment_region_function'.
    !> Port of newcomment.el:uncomment-region-default-1.
    recursive subroutine uncomment_region_default_1(beginning, end_in, status, argument)
        integer(int64), intent(in) :: beginning, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        type(handle_type) :: endm, opointm
        type(restriction_type) :: r
        character(len=:), allocatable :: srei, csre, sre, cce, erei, ere
        integer(int64) :: numarg, spt, ipt, ept, moved, position, fcol, slim, e
        logical :: box, box_equal, has_srei, ok, has_ere
        integer :: grp
        type(status_type) :: st

        status = status_type()
        call goto_character(beginning)
        endm = marker_at(end_in)
        numarg = 1
        if (present(argument)) numarg = argument
        srei = comment_padright(comment_continue_or_absent(), regular_expression=.true., found=has_srei)

        if (.not. has_srei .and. allocated(comment_continue%value)) then
            srei = comment_continue%value
            has_srei = .true.
        end if

        csre = comment_padright(cstart(), regular_expression=.true.)
        sre = ""
        if (has_srei) sre = "^\s-*?\("//srei//"\)"

        do
            if (.not. (point() < marked_position(endm))) exit
            spt = comment_search_forward(marked_position(endm), .true., st)
            if (spt == 0) exit
            ipt = point()
            ! Find the end of the comment.
            call goto_character(spt)

            if (.not. comment_forward(1_int64)) then
                ! Allow non-terminated comments.
                if (.not. is_end_of_buffer()) then
                    call set_error(status, GENERIC_ERROR_CODE, "Can't find the comment end")
                    exit
                end if
            end if

            ept = point()
            box = .false.
            ! Whether we might be using `=' for boxes.
            box_equal = .false.
            r = save_restriction()
            call narrow_to_region(spt, ept, st)
            ! Remove the comment-start.
            call goto_character(ipt)
            moved = skip_syntax_backward(" ")
            ! A box-comment starts with a looong comment-start marker.
            ok = .false.

            if (point() - point_minimum() == 1) then
                box_equal = .true.

                if (looking("=\{7\}")) then
                    if (before(point_maximum()) /= 10) then
                        moved = skip_characters_forward("=", st)
                        ok = .true.
                    end if
                end if
            end if

            if (.not. ok) ok = point() - point_minimum() - count_characters(cstart()) > 7
            if (ok) ok = count_lines(point_minimum(), point_maximum()) > 2
            if (ok) box = .true.
            ! Skip the padding.  Padding can come from comment-padding and/or
            ! from comment-start, so we first check comment-start.
            position = point()
            call goto_character(point_minimum())
            ok = looking(csre)
            call goto_character(position)
            if (.not. ok) ok = looking(regular_expression_quote(cpad()))
            if (ok) call goto_character(me(0))

            if (has_srei) then
                if (looking("\s-*"//LF//"\s-*"//srei)) call goto_character(me(0))
            end if

            if (.not. present(argument)) then
                call delete_region(point_minimum(), point(), status)
            else
                opointm = marker_at(point())
                moved = skip_syntax_backward(" ")
                call delete_character(-numarg, status)
                ok = .false.

                if (.not. is_beginning_of_buffer()) then
                    position = point()
                    call goto_character(point_minimum())
                    ok = looking(cskip())
                    call goto_character(position)
                end if

                if (.not. ok) then
                    ! If there's something left but it doesn't look like
                    ! a comment-start any more, just remove it.
                    call delete_region(point_minimum(), marked_position(opointm), status)
                end if

                call unmark(opointm)
            end if

            if (.not. status%ok()) then
                call restore_restriction(r)
                exit
            end if

            ! Remove the end-comment (and leading padding and such).
            call goto_character(point_maximum())
            call comment_enter_backward()

            ! Check for special `=' used sometimes in comment-box.
            if (box_equal .and. before(point_maximum()) /= 10) then
                position = point()
                ! skip `=' but only if there are at least 7.
                if (skip_characters_backward("=", st) > -7) call goto_character(position)
            end if

            if (.not. looking("\("//LF//"\|\s-\)*\'")) then
                if (is_beginning_of_line() .and. .not. is_beginning_of_buffer()) call goto_character(point() - 1)

                if (.not. present(argument)) then
                    call delete_region(point(), point_maximum(), status)
                else
                    moved = skip_syntax_forward(" ")
                    call delete_character(numarg, status)
                    ok = is_end_of_buffer()
                    if (.not. ok) ok = looking(comment_end_skip%value)
                    ! If there's something left but it doesn't look like
                    ! a comment-end any more, just remove it.
                    if (.not. ok .and. status%ok()) call delete_region(point(), point_maximum(), status)
                end if
            end if

            if (.not. status%ok()) then
                call restore_restriction(r)
                exit
            end if

            ! Unquote any nested end-comment.
            call quote_nested(cstart(), cend(), .true., status)

            ! Eliminate continuation markers as well.
            if (has_srei) then
                if (allocated(comment_continue%value)) then
                    cce = comment_string_reverse(comment_continue%value)
                else
                    cce = comment_string_reverse(cstart())
                end if

                has_ere = .false.
                ere = ""

                if (box) then
                    erei = comment_padleft(cce, regular_expression=.true., found=has_ere)
                    if (has_ere) ere = "\("//erei//"\)\s-*$"
                end if

                call goto_character(point_minimum())

                do
                    if (has_ere) then
                        if (regular_expression_forward(ere, line_end_position(), SEARCH_NOERROR)) then
                            grp = 1
                            if (me(2) > 0) grp = 2
                            call replace_match("", st, fixedcase=.true., literal=.true., subexp=grp)
                        else
                            has_ere = .false.
                        end if
                    end if

                    e = forward_line(1_int64)
                    if (.not. regular_expression_forward(sre, line_end_position(), SEARCH_NOERROR)) exit
                    grp = 1
                    if (me(2) > 0) grp = 2
                    call replace_match("", st, fixedcase=.true., literal=.true., subexp=grp)
                end do
            end if

            ! Go to the end for the next comment.
            call goto_character(point_maximum())
            call restore_restriction(r)

            ! Remove any obtrusive spaces left preceding a tab at `spt'.
            if (after(spt) == 9 .and. before(spt) == 32 .and. tab_width%value > 0) then
                position = point()
                call goto_character(spt)
                fcol = current_column()
                slim = point() - mod(fcol, tab_width%value)
                moved = skip_characters_backward(" ", st, slim)
                call delete_character(-moved, status)
                call goto_character(position)
            end if
        end do

        call unmark(endm)
    end subroutine uncomment_region_default_1

    !> `comment_continue' as an optional argument: absent when unset.
    !> Not a port.
    function comment_continue_or_absent() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(comment_continue%value)) s = comment_continue%value
    end function comment_continue_or_absent

    !> The number of lines between FROM and TO.  Not a port.
    integer(int64) function count_lines(from, to)
        integer(int64), intent(in) :: from, to

        count_lines = simple_count_lines(from, to)
    end function count_lines

    !> Uncomment each line in the BEGINNING .. END region.
    !> The numeric prefix ARGUMENT can specify a number of chars to remove from the
    !> comment markers.  Port of newcomment.el:uncomment-region-default
    !> (combine-change-calls is Deferred).
    recursive subroutine uncomment_region_default(beginning, end, status, argument, universal)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        logical, intent(in), optional :: universal

        ! UNIVERSAL changes nothing here: C-u counts as its numeric value.
        if (present(universal)) continue
        call uncomment_region_default_1(beginning, end, status, argument)
    end subroutine uncomment_region_default

    !> Make a white-space string of width LEN for use at BEGINNING_OF_LINE.
    !> When `indent_tabs_mode' is true, tab characters will be used.
    !> Port of newcomment.el:comment-make-bol-ws.
    function comment_make_beginning_of_line_whitespace(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s

        if (indent_tabs_mode%value .and. tab_width%value > 0) then
            s = repeat_character(TAB, n/tab_width%value)//repeat_character(" ", mod(n, tab_width%value))
        else
            s = repeat_character(" ", n)
        end if
    end function comment_make_beginning_of_line_whitespace

    !> Make the leading and trailing extra lines.
    !> This is used for `extra-line' style (or `box' style if BLOCK is
    !> specified).  Port of newcomment.el:comment-make-extra-lines.
    subroutine comment_make_extra_lines(cs, ce, ccs, cce, minimum_indent, maximum_indent, block, out_cs, out_ce)
        character(len=*), intent(in) :: cs, ce, ccs, cce
        integer(int64), intent(in) :: minimum_indent, maximum_indent
        logical, intent(in) :: block
        character(len=:), allocatable, intent(out) :: out_cs, out_ce
        character(len=:), allocatable :: cs2, ce2, s, e, regular_expression, fill, c
        integer(int64) :: eindent, width, lcs, k
        type(status_type) :: st

        eindent = 0
        cs2 = cs
        ce2 = ce

        if (.not. block) then
            ! Try to match CS and CE's content so they align aesthetically.
            ce2 = comment_string_strip(ce2, .true., .true.)

            if (smatch("\(.+\).*"//LF//"\(.*?\)\1", ce2//LF//cs2) >= 0) then
                eindent = max(sme(2) - smb(2) - smb(0), 0_int64)
            end if
        else
            ! box comment
            width = maximum_indent - minimum_indent
            s = cs2//"a=m"//cce
            e = ccs//"a=m"//ce2

            if (smatch(".*\S-\S-", cs2) >= 0) then
                c = sub(cs2, sme(0) - 1, sme(0))
            else if (ce2 == "" .and. smatch(".*\S-", cs2) >= 0) then
                c = sub(cs2, sme(0) - 1, sme(0))
            else
                c = "="
            end if

            regular_expression = "\s-*a=m\s-*"
            k = smatch(regular_expression, s)
            lcs = count_characters(cs2)
            fill = ""

            do k = 1, width + (sme(0) - smb(0) - lcs - 3)
                fill = fill//c
            end do

            call replace_in_string(fill, s, cs2)

            if (smatch(cskip(), cs2) < 0 .and. smatch("a=m", s) >= 0) then
                ! The whitespace around CS cannot be ignored: put it back.
                regular_expression = "a=m"
                fill = ""

                do k = 1, width - lcs
                    fill = fill//c
                end do

                call replace_in_string(fill, s, cs2)
            end if

            k = smatch(regular_expression, e)
            call replace_in_string(fill, e, ce2)
        end if

        out_cs = cs2//LF//comment_make_beginning_of_line_whitespace(minimum_indent)//ccs
        out_ce = cce//LF//comment_make_beginning_of_line_whitespace(minimum_indent + eindent)//ce2
        st = status_type()
    end subroutine comment_make_extra_lines

    !> (replace-match NEW t t S): S with the last string match replaced
    !> by NEW, into R.  Not a port.
    subroutine replace_in_string(new, s, r)
        character(len=*), intent(in) :: new, s
        character(len=:), allocatable, intent(out) :: r

        r = sub(s, 0_int64, smb(0))//new//sub(s, sme(0))
    end subroutine replace_in_string

    !> Comment region BEGINNING .. END.
    !> CS and CE are the comment start string and comment end string,
    !> respectively.  CCS and CCE are the comment continuation strings
    !> for the start and end of lines, respectively (default to CS and CE).
    !> BLOCK indicates that end of lines should be marked with either CCE,
    !> CE or CS (if CE is empty) and that those markers should be aligned.
    !> LINES indicates that an extra lines will be used at the beginning
    !> and end of the region for CE and CS.
    !> INDENT indicates to put CS and CCS at the current indentation of
    !> the region rather than at left margin.  Absent strings mean none.
    !> Port of newcomment.el:comment-region-internal.
    recursive subroutine comment_region_internal(beginning, end, cs_in, status, ce_in, ccs_in, cce_in, block, lines, indent)
        integer(int64), intent(in) :: beginning, end
        character(len=*), intent(in) :: cs_in
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: ce_in, ccs_in, cce_in
        logical, intent(in), optional :: block, lines, indent
        character(len=:), allocatable :: cs, ce, ccs, cce, ccs2, cce2
        logical :: no_empty, has_ce, has_ccs, has_cce, ce_sanitized, blk, lns, ind, has_tab
        integer(int64) :: minimum_indent, maximum_indent, reached, bindent, original_point, found, column, shell, n
        type(handle_type) :: endm
        type(restriction_type) :: r
        type(status_type) :: st

        status = status_type()
        blk = .false.
        if (present(block)) blk = block
        lns = .false.
        if (present(lines)) lns = lines
        ind = .false.
        if (present(indent)) ind = indent
        cs = cs_in
        has_ce = present(ce_in)
        ce = ""
        if (has_ce) ce = ce_in
        has_ccs = present(ccs_in)
        ccs = ""
        if (has_ccs) ccs = ccs_in
        has_cce = present(cce_in)
        cce = ""
        if (has_cce) cce = cce_in
        no_empty = .not. (comment_empty_lines%value .or. &
                          (.not. comment_empty_lines%is_off() .and. len(ce) == 0))
        ! Sanitize CE and CCE.
        if (has_ce .and. ce == "") has_ce = .false.
        ce_sanitized = has_ce
        if (has_cce .and. cce == "") has_cce = .false.

        ! If CE is empty, multiline cannot be used.
        if (.not. has_ce) then
            has_ccs = .false.
            has_cce = .false.
        end if

        ! Should we mark empty lines as well ?
        if (has_ccs .or. blk .or. lns) no_empty = .false.

        ! Make sure we have end-markers for BLOCK mode.
        if (blk .and. .not. has_ce) then
            ce = comment_string_reverse(cs)
            has_ce = .true.
        end if

        ! If BLOCK is not requested, we don't need CCE.
        if (.not. blk) has_cce = .false.

        ! Continuation defaults to the same as CS and CE.
        if (.not. has_ccs) then
            ccs = cs
            cce = ce
            has_cce = has_ce
        end if

        if (.not. has_ce) ce = ""
        if (.not. has_cce) cce = ""
        original_point = point()
        call goto_character(end)

        ! If the end is not at the end of a line and the comment-end
        ! is implicit (i.e. a newline), explicitly insert a newline.
        if (.not. (ce_sanitized .or. is_end_of_line())) then
            call insert(LF, status)
            if (.not. status%ok()) return
            call indent_according_to_mode(status)
            if (.not. status%ok()) return
        end if

        endm = marker_at(end)
        ! comment-with-narrowing: the space added (and then removed) at
        ! the beginning keeps the text's indentation as it was.
        call goto_character(beginning)
        bindent = current_column()
        r = save_restriction()
        call narrow_to_region(beginning, marked_position(endm), st)
        call goto_character(point_minimum())
        call insert(repeat_character(" ", bindent), status)
        minimum_indent = point_maximum()
        maximum_indent = 0
        call goto_character(point_minimum())
        ! Quote any nested comment marker
        call quote_nested(cstart(), cend(), .false., status)
        ! Loop over all lines to find the needed indentations.
        call goto_character(point_minimum())

        do
            if (.not. looking("[ "//TAB//"]*$")) minimum_indent = min(minimum_indent, current_indentation())
            call end_of_line(1_int64)
            maximum_indent = max(maximum_indent, current_column())
            if (is_end_of_buffer()) exit
            shell = forward_line(1_int64)
        end do

        has_tab = .false.
        call goto_character(beginning)
        call search_tab(marked_position(endm), has_tab)
        maximum_indent = maximum_indent + max(count_characters(cs), count_characters(ccs))
        ! Inserting ccs can change max-indent by (1- tab-width)
        ! but only if there are TABs in the boxed text, of course.
        if (has_tab) maximum_indent = maximum_indent + tab_width%value - 1
        if (.not. ind) minimum_indent = 0

        ! make the leading and trailing lines if requested
        if (lns) then
            ! Trim trailing whitespace from cs if there's some.
            found = smatch("[ "//TAB//LF//char(13)//"]+\'", cs)
            if (found >= 0) cs = sub(cs, 0_int64, found)
            call comment_make_extra_lines(cs, ce, ccs, cce, minimum_indent, maximum_indent, blk, ccs2, cce2)
            cs = ccs2
            ce = cce2
            has_ce = .true.
        end if

        call goto_character(point_minimum())

        ! Loop over all lines from BEGINNING to END.
        do
            if (.not. (no_empty .and. looking("[ "//TAB//"]*$"))) then
                call move_to_column(minimum_indent, reached, force=.true.)
                call insert(cs, status)
                if (.not. status%ok()) exit
                ! switch to CCS after the first line
                cs = ccs
                call end_of_line(1_int64)

                if (is_end_of_buffer()) then
                    cce = ce
                    has_cce = has_ce .and. len(ce) > 0
                end if

                if (has_cce) then
                    if (blk) call move_to_column(maximum_indent, reached, force=.true.)
                    call insert(cce, status)
                    if (.not. status%ok()) exit
                end if
            end if

            call end_of_line(1_int64)
            if (is_end_of_buffer()) exit
            shell = forward_line(1_int64)
        end do

        ! remove the bindent
        call goto_character(point_minimum())

        if (looking(" *")) then
            n = min(me(0) - mb(0), bindent)
            call delete_character(n, st)
            bindent = bindent - n
        end if

        call end_of_line(1_int64)
        column = point()
        call beginning_of_line(1_int64)

        do while (bindent > 0)
            if (.not. regular_expression_forward("   *", column, SEARCH_NOERROR)) exit
            n = min(bindent, me(0) - mb(0) - 1)
            call goto_character(mb(0))
            call delete_character(n, st)
            bindent = bindent - n
        end do

        call restore_restriction(r)
        call unmark(endm)
        call goto_character(min(original_point, point_maximum()))
    end subroutine comment_region_internal

    !> Whether there is a TAB between point and LIMIT.  Not a port.
    subroutine search_tab(limit, found)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        integer(int64) :: position

        position = point()
        found = regular_expression_forward(TAB, limit, SEARCH_NOERROR)
        call goto_character(position)
    end subroutine search_tab

    !> Comment or uncomment each line in the region.
    !> With just \[universal_argument] prefix arg, uncomment each line in region BEGINNING .. END.
    !> Numeric prefix ARGUMENT means use ARGUMENT comment characters.
    !> If ARGUMENT is negative, delete that many comment characters instead.
    !>
    !> The strings used as comment starts are built from `comment_start'
    !> and `comment_padding'; the strings used as comment ends are built
    !> from `comment_end' and `comment_padding'.
    !>
    !> By default, the `comment_start' markers are inserted at the
    !> current indentation of the region, and comments are terminated on
    !> each line (even for syntaxes in which newline does not end the
    !> comment and blank lines do not get comments).  This can be
    !> changed with `comment_style'.  Port of newcomment.el:comment-region.
    recursive subroutine comment_region(beginning_in, end_in, status, argument, universal)
        integer(int64), intent(in) :: beginning_in, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        logical, intent(in), optional :: universal
        integer(int64) :: beginning, end
        type(handle_type) :: om

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_variables(status)
        if (.not. status%ok()) return
        beginning = min(beginning_in, end_in)
        end = max(beginning_in, end_in)
        om = marker_at(point())
        call comment_region_function(beginning, end, status, argument, universal)
        call goto_character(marked_position(om))
        call unmark(om)
    end subroutine comment_region

    !> Comment or uncomment each line in the region.
    !> With just \[universal_argument] prefix arg, uncomment each line in region BEGINNING .. END.
    !> Numeric prefix ARGUMENT means use ARGUMENT comment characters.
    !> If ARGUMENT is negative, delete that many comment characters instead.
    !>
    !> The strings used as comment starts are built from `comment_start'
    !> and `comment_padding'; the strings used as comment ends are built
    !> from `comment_end' and `comment_padding'.
    !>
    !> By default, the `comment_start' markers are inserted at the
    !> current indentation of the region, and comments are terminated on
    !> each line (even for syntaxes in which newline does not end the
    !> comment and blank lines do not get comments).  This can be
    !> changed with `comment_style'.  Port of newcomment.el:comment-region
    !> (the command).
    recursive subroutine execute_comment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call with_prefix(beginning, end, status, comment_region)
    end subroutine execute_comment_region

    !> Port of newcomment.el:comment-region-default-1.
    recursive subroutine comment_region_default_1(beginning_in, end_in, status, argument, universal)
        integer(int64), intent(in) :: beginning_in, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        logical, intent(in), optional :: universal
        integer(int64) :: beginning, end, numarg, moved, k
        integer :: style
        logical :: lines, block, multi, multi_character, triple, indent, ok, universal_argument, has
        character(len=:), allocatable :: cs, ce, ccs, cce, s
        type(status_type) :: st

        status = status_type()
        numarg = 1
        if (present(argument)) numarg = argument
        universal_argument = .false.
        if (present(universal)) universal_argument = universal
        style = int(comment_style%value)
        if (style < 1 .or. style > 8) style = COMMENT_STYLE_INDENT
        lines = style_extra(style)
        block = style_align(style)
        multi = style_multi(style)
        ! We use `chars' instead of `syntax' because `\n' might be
        ! of end-comment syntax rather than of whitespace syntax.
        ! sanitize BEGINNING and END
        call goto_character(beginning_in)
        moved = skip_characters_forward(" "//TAB//LF//char(13), st)
        call beginning_of_line(1_int64)
        beginning = max(beginning_in, point())
        call goto_character(end_in)
        moved = skip_characters_backward(" "//TAB//LF//char(13), st)
        call end_of_line(1_int64)
        end = min(end_in, point())

        if (beginning >= end) then
            call set_error(status, GENERIC_ERROR_CODE, "Nothing to comment")
            return
        end if

        ! sanitize LINES
        if (lines) then
            call goto_character(beginning)
            call beginning_of_line(1_int64)
            moved = skip_syntax_forward(" ")
            ok = point() >= beginning

            if (ok) then
                call goto_character(end)
                call end_of_line(1_int64)
                moved = skip_syntax_backward(" ")
                ok = point() <= end
            end if

            if (ok) ok = block .or. cend() /= ""

            if (ok .and. .not. block) then
                call goto_character(beginning)
                ok = regular_expression_forward("$", end, SEARCH_NOERROR)
            end if

            lines = ok
        end if

        ! don't add end-markers just because the user asked for `block'
        if (.not. (lines .or. cend() == "")) block = .false.

        if (universal_argument) then
            call uncomment_region(beginning, end, status)
            return
        else if (numarg < 0) then
            call uncomment_region(beginning, end, status, -numarg)
            return
        end if

        multi_character = smatch("[ "//TAB//"]*\'", cstart()) /= 1
        triple = .false.

        if (style_indent(style) == 2) then
            k = point()
            call goto_character(beginning)
            indent = multi_character
            ! Triple if we will put the comment starter at the margin
            ! and the first line of the region isn't indented
            ! at least two spaces.
            triple = .not. multi_character .and. looking(TAB//"\|  ")
            call goto_character(k)
        else
            indent = style_indent(style) == 1
        end if

        ! In Lisp and similar modes with one-character comment starters,
        ! double it by default if `comment-add' says so.
        ! If it isn't indented, triple it.
        if (.not. present(argument) .and. .not. multi_character) then
            numarg = comment_add%value
            if (triple) numarg = numarg*2
        else
            numarg = numarg - 1
        end if

        s = comment_padright(cstart(), numarg)

        if (smatch(cskip(), s) >= 0) then
            cs = s
        else
            cs = comment_padright(cstart())
        end if

        s = comment_padleft(cend(), numarg, found=has)

        if (has) then
            if (smatch(comment_end_skip%value, s) >= 0) then
                ce = s
            else
                ce = comment_padright(cend(), found=has)
            end if
        end if

        if (multi) then
            ccs = comment_padright(comment_continue_or_absent(), numarg, found=ok)

            if (.not. ok .and. allocated(comment_continue%value)) then
                ! `comment-padright' returns nil when
                ! `comment-continue' contains only whitespace
                ccs = comment_continue%value
                ok = .true.
            end if

            cce = comment_padleft(comment_string_reverse(comment_continue_or_absent()), numarg)

            if (has .and. ok) then
                call comment_region_internal(beginning, end, cs, status, ce, ccs, cce, block=block, lines=lines, indent=indent)
            else if (has) then
                call comment_region_internal(beginning, end, cs, status, ce, cce_in=cce, block=block, lines=lines, &
                                             indent=indent)
            else if (ok) then
                call comment_region_internal(beginning, end, cs, status, ccs_in=ccs, cce_in=cce, block=block, &
                                             lines=lines, indent=indent)
            else
                call comment_region_internal(beginning, end, cs, status, cce_in=cce, block=block, lines=lines, indent=indent)
            end if
        else if (has) then
            call comment_region_internal(beginning, end, cs, status, ce, block=block, lines=lines, indent=indent)
        else
            call comment_region_internal(beginning, end, cs, status, block=block, lines=lines, indent=indent)
        end if
    end subroutine comment_region_default_1

    !> Comment BEGINNING .. END with the prefix argument ARGUMENT, as
    !> `comment_region' does by default (the default
    !> `comment_region_function').  Port of
    !> newcomment.el:comment-region-default (combine-change-calls is
    !> Deferred).
    recursive subroutine comment_region_default(beginning, end, status, argument, universal)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        logical, intent(in), optional :: universal

        call comment_region_default_1(beginning, end, status, argument, universal)
    end subroutine comment_region_default

    !> Comment out the BEGINNING .. END region, putting it inside a box.
    !> The numeric prefix ARGUMENT specifies how many characters to add to begin- and
    !> end- comment markers additionally to what variable `comment_add' already
    !> specifies.  Port of newcomment.el:comment-box.
    recursive subroutine comment_box(beginning, end, argument, status)
        integer(int64), intent(in) :: beginning, end, argument
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_variables(status)
        if (.not. status%ok()) return
        saved = comment_style%value

        if (saved >= 1 .and. saved <= 8) then
            if (style_multi(saved)) then
                comment_style%value = COMMENT_STYLE_BOX_MULTI
            else
                comment_style%value = COMMENT_STYLE_BOX
            end if
        else
            comment_style%value = COMMENT_STYLE_BOX
        end if

        call comment_region(beginning, end, status, comment_add%value + argument)
        comment_style%value = saved
    end subroutine comment_box

    !> Comment out the BEGINNING .. END region, putting it inside a box.
    !> The numeric prefix ARGUMENT specifies how many characters to add to begin- and
    !> end- comment markers additionally to what variable `comment_add' already
    !> specifies.  Port of newcomment.el:comment-box (the command).
    recursive subroutine execute_comment_box(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call comment_box(beginning, end, prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_comment_box

    !> Return true if the text between BEGINNING and END is all comments.
    !> Port of newcomment.el:comment-only-p.
    recursive logical function is_comment_only(beginning, end) result(only)
        integer(int64), intent(in) :: beginning, end
        integer(int64) :: original_point
        logical :: done

        original_point = point()
        call goto_character(beginning)
        done = comment_forward(point_maximum())
        only = end <= point()
        call goto_character(original_point)
    end function is_comment_only

    !> Call `comment_region', unless the region only consists of comments,
    !> in which case call `uncomment_region'.  If a prefix arg is given, it
    !> is passed on to the respective function.  Port of
    !> newcomment.el:comment-or-uncomment-region.
    recursive subroutine comment_or_uncomment_region(beginning, end, status, argument, universal)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: argument
        logical, intent(in), optional :: universal

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_variables(status)
        if (.not. status%ok()) return

        if (is_comment_only(beginning, end)) then
            call uncomment_region(beginning, end, status, argument, universal)
        else
            call comment_region(beginning, end, status, argument, universal)
        end if
    end subroutine comment_or_uncomment_region

    !> Call `comment_region', unless the region only consists of comments,
    !> in which case call `uncomment_region'.  If a prefix arg is given, it
    !> is passed on to the respective function.  Port of
    !> newcomment.el:comment-or-uncomment-region (the command).
    recursive subroutine execute_comment_or_uncomment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call with_prefix(beginning, end, status, comment_or_uncomment_region)
    end subroutine execute_comment_or_uncomment_region

    !> Call the comment command you want (Do What I Mean).
    !> If the region is active and `transient_mark_mode' is on, call
    !> `comment_region' (unless it only consists of comments, in which
    !> case it calls `uncomment_region'); in this case, prefix numeric
    !> argument ARGUMENT specifies how many characters to remove from each
    !> comment delimiter (so don't specify a prefix argument whose value
    !> is greater than the total length of the comment delimiters).
    !> Else, if the current line is empty, call `comment_insert_comment_function'
    !> if it is defined, otherwise insert a comment and indent it.
    !> Else, if a prefix ARGUMENT is specified, call `comment_cut'; in this
    !> case, prefix numeric argument ARGUMENT specifies on how many lines to kill
    !> the comments.
    !> Else, call `comment_indent'.
    !> You can configure `comment_style' to change the way regions are
    !> commented.  Port of newcomment.el:comment-dwim.
    recursive subroutine comment_do_what_i_mean(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end, add, original_point
        logical :: blank, has
        character(len=:), allocatable :: s

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_variables(status)
        if (.not. status%ok()) return

        if (is_use_region()) then
            call region_bounds(beginning, end, status)
            if (.not. status%ok()) return
            call with_prefix(beginning, end, status, comment_or_uncomment_region)
            return
        end if

        original_point = point()
        call beginning_of_line(1_int64)
        blank = looking("\s-*$")
        call goto_character(original_point)

        if (.not. blank) then
            ! FIXME: If there's no comment to kill on this line and ARGUMENT is
            ! specified, calling comment-kill is not very clever.
            if (current_prefix_argument%kind /= PREFIX_NONE) then
                if (current_prefix_argument%kind == PREFIX_NUMBER) then
                    call comment_cut(status, current_prefix_argument%value)
                else
                    call comment_cut(status)
                end if
            else
                call comment_indent(status)
            end if

            return
        end if

        ! Inserting a comment on a blank line. comment-indent calls
        ! c-i-c-f if needed in the non-blank case.
        if (associated(comment_insert_comment_function%value)) then
            call comment_insert_comment_function%value(status)
            return
        end if

        add = comment_add_count()
        ! Some modes insist on keeping column 0 comment in column 0
        ! so we need to move away from it before inserting the comment.
        call indent_according_to_mode(status)
        if (.not. status%ok()) return
        call insert(comment_padright(cstart(), add), status)
        if (.not. status%ok()) return
        original_point = point()

        if (cend() /= "") then
            s = comment_padleft(cend(), add, found=has)
            call insert(s, status)
            if (.not. status%ok()) return
        end if

        call indent_according_to_mode(status)
        call goto_character(original_point)
    end subroutine comment_do_what_i_mean

    !> `comment_do_what_i_mean' as a command.
    recursive subroutine execute_comment_do_what_i_mean(status)
        type(status_type), intent(out) :: status

        call comment_do_what_i_mean(status)
    end subroutine execute_comment_do_what_i_mean

    !> Compute the number of extra comment starter characters.
    !> (Extra semicolons in Lisp mode, extra stars in C mode, etc.)
    !> If ARGUMENT is true, just follow ARGUMENT.
    !> If the comment starter is multi-char, just follow ARGUMENT.
    !> Otherwise obey `comment_add'.  Port of newcomment.el:comment-add
    !> (ARGUMENT is the current prefix argument).
    integer(int64) function comment_add_count() result(n)
        if (current_prefix_argument%kind == PREFIX_NONE .and. smatch("[ "//TAB//"]*\'", cstart()) == 1) then
            n = comment_add%value
        else
            n = prefix_numeric_value(current_prefix_argument) - 1
        end if
    end function comment_add_count

    !> Check that the adaptive fill prefix is consistent with the context.
    !> PREFIX is the prefix (presumably guessed by `adaptive_fill_mode').
    !> COMPOS is the position of the beginning of the comment we're in, or 0
    !> if we're not inside a comment.  Port of
    !> newcomment.el:comment-valid-prefix-p.
    recursive logical function is_comment_valid_prefix(prefix, compos) result(valid)
        character(len=*), intent(in) :: prefix
        integer(int64), intent(in) :: compos
        character(len=:), allocatable :: prefix_com, prefix_space, prefix_comstart
        integer(int64) :: original_point, prefix_indent, k, lm
        logical :: done
        type(status_type) :: st

        if (compos == 0) then
            ! We're not inside a comment: the prefix shouldn't match
            ! a comment-starter.
            valid = .true.

            if (allocated(comment_start%value) .and. allocated(comment_start_skip%value)) then
                valid = smatch(comment_start_skip%value, prefix) < 0
            end if

            return
        end if

        original_point = point()
        ! Accept any prefix if the current comment is not EOL-terminated.
        call goto_character(compos)
        done = comment_forward(1_int64)
        valid = .not. is_beginning_of_line()
        call goto_character(original_point)
        if (valid) return
        ! Accept any prefix that starts with the same comment-start marker
        ! as the current one.
        if (smatch("\`[ "//TAB//"]*\(?:"//cskip()//"\)", prefix) < 0) return
        prefix_com = comment_string_strip(match_string(0, prefix), .false., .true.)
        k = smatch("\`[ "//TAB//"]*", prefix_com)
        prefix_space = match_string(0, prefix_com)
        prefix_indent = count_characters(prefix_space)
        prefix_comstart = sub(prefix_com, sme(0))
        call goto_character(compos)

        ! The comstart marker is the same.
        if (looking(regular_expression_quote(prefix_comstart))) then
            ! The indentation as well.
            valid = prefix_indent == current_column() - current_left_margin()

            if (.not. valid) then
                ! Check the indentation in two different ways, just
                ! to try and avoid most of the potential funny cases.
                lm = point()
                call move_to_left_margin(st)
                valid = prefix_space == text(lm, point())
            end if
        end if

        call goto_character(original_point)
    end function is_comment_valid_prefix

    !> Break line at point and indent, continuing comment if within one.
    !> This indents the body of the continued comment
    !> under the previous comment line.
    !>
    !> This command is intended for styles where you write a comment per line,
    !> starting a new comment (and terminating it if necessary) on each line.
    !> If you want to continue one comment across several lines, use \[newline_and_indent].
    !>
    !> If a fill column is specified, it overrides the use of the comment column
    !> or comment indentation.
    !>
    !> The inserted newline is marked hard if variable `use_hard_newlines' is true,
    !> unless optional argument SOFT is true.  INTERACTIVE says it was
    !> called as a command.  Port of newcomment.el:comment-indent-new-line.
    recursive subroutine comment_indent_new_line(status, soft, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: soft, interactive
        integer(int64) :: compos, comin, original_point, point_value, saved_column, column, endp
        character(len=:), allocatable :: comstart, comend, saved_start, saved_end, saved_continue, rest
        logical :: soft_newline, inter, normalp, continuep, fwd, has_continue, noindent, own_line, had_local
        type(handle_type) :: cursor_motion
        procedure(comment_indent_procedure), pointer :: saved_fn
        type(status_type) :: st

        soft_newline = .false.
        if (present(soft)) soft_newline = soft
        inter = .false.
        if (present(interactive)) inter = interactive
        call comment_normalize_variables(status, noerror=.true.)
        if (.not. status%ok()) return
        compos = 0
        comin = 0

        ! If we are not inside a comment and we only auto-fill comments,
        ! don't do anything (unless no comment syntax is defined).
        if (allocated(comment_start%value) .and. comment_auto_fill_only_comments%value .and. .not. inter) then
            original_point = point()
            compos = comment_beginning()
            comin = point()
            call goto_character(original_point)
            if (compos == 0) return
        end if

        ! Now we know we should auto-fill.
        ! Insert the newline before removing empty space so that markers
        ! get preserved better.
        if (soft_newline) then
            call insert(LF, status)
        else
            call newline(status, 1_int64)
        end if

        if (.not. status%ok()) return
        original_point = point()
        call goto_character(point() - 1)
        call delete_horizontal_space(.false., status)
        if (.not. status%ok()) return
        call goto_character(original_point)
        call goto_character(point())
        call delete_horizontal_space(.false., status)
        if (.not. status%ok()) return

        if (allocated(fill_prefix%value) .and. .not. adaptive_fill_mode%value) then
            ! Blindly trust a non-adaptive fill-prefix.
            call indent_to_left_margin(status, noindent)
            if (status%ok()) call insert_before_markers(fill_prefix%value, status)
            return
        end if

        ! If necessary check whether we're inside a comment.
        if (compos == 0 .and. allocated(comment_start%value)) then
            original_point = point()
            call goto_character(point() - 1)
            compos = comment_beginning()
            comin = point()
            call goto_character(original_point)
        end if

        ! If there's an adaptive prefix, use it unless we're inside
        ! a comment and the prefix is not a comment starter.
        if (allocated(fill_prefix%value)) then
            if (is_comment_valid_prefix(fill_prefix%value, compos)) then
                call indent_to_left_margin(status, noindent)
                if (status%ok()) call insert(fill_prefix%value, status)
                return
            end if
        end if

        ! If we're not inside a comment, just try to indent.
        if (compos == 0) then
            call indent_according_to_mode(status)
            return
        end if

        comstart = text(compos, comin)
        normalp = smatch(regular_expression_quote(comment_string_strip(cstart(), .true., .true.)), comstart) >= 0

        if (normalp) then
            comend = cend()
        else
            ! The comment starter is not the normal comment-start
            ! so we can't just use comment-end.
            original_point = point()
            call goto_character(compos)
            fwd = comment_forward(1_int64)

            if (.not. fwd) then
                comend = cend()
            else
                endp = point()
                call comment_enter_backward()
                comend = comment_string_strip(text(point(), endp), .false., .true.)
            end if

            call goto_character(original_point)
        end if

        if (comment_multi_line%value .and. len(comend) > 0) then
            call indent_according_to_mode(status)
            return
        end if

        call insert(LF, status)
        if (.not. status%ok()) return
        call goto_character(point() - 1)
        ! The continuation indentation should be somewhere
        ! between the current line's indentation (plus 2 for
        ! good measure) and the current comment's indentation,
        ! with a preference for comment-column.
        original_point = point()
        call goto_character(compos)
        column = min(current_column(), max(comment_column%value, 2 + current_indentation()))
        ! If the previous comment is on its own line, then
        ! reuse its indentation unconditionally.
        ! Important for modes like Python/Haskell where
        ! auto-indentation is unreliable.
        point_value = skip_characters_backward(" "//TAB, st)
        own_line = is_beginning_of_line()
        call goto_character(original_point)
        saved_column = comment_column%value
        saved_fn => comment_indent_function%get_current()
        had_local = comment_indent_function%is_local()
        saved_start = cstart()
        saved_end = cend()
        has_continue = allocated(comment_continue%value)
        if (has_continue) saved_continue = comment_continue%value
        comment_column%value = column
        if (own_line) call comment_indent_function%set_local(own_line_indent)
        comment_start%value = comstart
        comment_end%value = comend
        ! Recreate comment-continue from comment-start.
        ! FIXME: wrong if comment-continue was set explicitly!
        ! FIXME: use prev line's continuation if available.
        if (allocated(comment_continue%value)) deallocate (comment_continue%value)
        continuep = comment_multi_line%value

        if (.not. continuep .and. comment_style%value >= 1 .and. comment_style%value <= 8) then
            continuep = style_multi(comment_style%value)
        end if

        call comment_indent(status, continuep)
        comment_column%value = saved_column

        if (own_line) then
            if (had_local) then
                call comment_indent_function%set_local(saved_fn)
            else
                call comment_indent_function%clear_local()
            end if
        end if

        comment_start%value = saved_start
        comment_end%value = saved_end

        if (has_continue) then
            comment_continue%value = saved_continue
        else if (allocated(comment_continue%value)) then
            deallocate (comment_continue%value)
        end if

        if (.not. status%ok()) return
        point_value = point()
        cursor_motion = marker_at(point_value)
        call end_of_line(1_int64)
        rest = text(point_value, point())
        ! The 1+ is to make sure we delete the \n inserted above.
        call delete_region(point_value, point() + 1, status)
        if (.not. status%ok()) return
        call end_of_line(0_int64)
        call insert(rest, status)
        call goto_character(marked_position(cursor_motion))
        call unmark(cursor_motion)
    end subroutine comment_indent_new_line

    !> The indentation of a comment continuing one on its own line:
    !> `comment_column' unconditionally.  Not a port.
    subroutine own_line_indent(indent, status)
        type(comment_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status

        status = status_type()
        indent%kind = COMMENT_INDENT_COLUMN
        indent%column = comment_column%value
    end subroutine own_line_indent

    !> Break line at point and indent, continuing comment if within one.
    !> (the command: `indent_new_comment_line').  Port of
    !> newcomment.el:comment-indent-new-line (interactive).
    recursive subroutine execute_comment_indent_new_line(status)
        type(status_type), intent(out) :: status

        call comment_indent_new_line(status, interactive=.true.)
    end subroutine execute_comment_indent_new_line

    !> `comment_line_break_function': `comment_indent_new_line' with SOFT.
    !> Not a port.
    recursive subroutine comment_line_break(status, soft)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: soft

        call comment_indent_new_line(status, soft)
    end subroutine comment_line_break

    !> Comment or uncomment current line and leave point after it.
    !> With positive prefix, apply to N lines including current one.
    !> With negative prefix, apply to -N lines above.  Also, further
    !> consecutive invocations of this command will inherit the negative
    !> argument.
    !>
    !> If region is active, comment lines in active region instead.
    !> Unlike `comment_do_what_i_mean', this always comments whole lines.  Port of
    !> newcomment.el:comment-line.
    recursive subroutine comment_line(n_in, status)
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: n, b, e, a, z, shell

        status = status_type()
        n = n_in

        if (is_use_region()) then
            call region_bounds(b, e, status)
            if (.not. status%ok()) return
            call goto_character(b)
            a = line_beginning_position()
            call goto_character(e)
            z = line_end_position()
            call comment_or_uncomment_region(a, z, status)
            return
        end if

        if (associated(last_command, comment_line_backward) .and. n >= 0) n = -n
        a = line_beginning_position()
        z = line_end_position(n)
        call goto_character(z)
        call comment_or_uncomment_region(min(a, z), max(a, z), status)
        if (.not. status%ok()) return
        shell = forward_line(1_int64)
        call back_to_indentation()
        if (n < 0) this_command => comment_line_backward
    end subroutine comment_line

    !> Comment or uncomment current line and leave point after it.
    !> With positive prefix, apply to N lines including current one.
    !> With negative prefix, apply to -N lines above.  Also, further
    !> consecutive invocations of this command will inherit the negative
    !> argument.
    !>
    !> If region is active, comment lines in active region instead.
    !> Unlike `comment_do_what_i_mean', this always comments whole lines.  Port of
    !> newcomment.el:comment-line (the command).
    recursive subroutine execute_comment_line(status)
        type(status_type), intent(out) :: status

        call comment_line(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_comment_line
end module newcomment_mod
