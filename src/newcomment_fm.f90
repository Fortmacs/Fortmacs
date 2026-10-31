! newcomment_fm.f90 -- (un)comment regions of buffers.
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

!> Commenting and uncommenting: `comment_dwim' (M-;), `comment_region',
!> `uncomment_region', `comment_or_uncomment_region', `comment_box',
!> `comment_indent' (`indent_for_comment'), `comment_set_column' (C-x ;),
!> `comment_kill', `comment_line' (C-x C-;) and `comment_indent_new_line'
!> (`indent_new_comment_line'), which `default_indent_new_line' (M-j)
!> calls through `comment_line_break_function' when a comment syntax is
!> defined; and the variables major modes set to describe their comments
!> (`comment_start', `comment_end', `comment_start_skip', ...).
!>
!> Values: `comment_use_syntax' is COMMENT_USE_SYNTAX_NIL, _T or
!> _UNDECIDED; `comment_style' one of the COMMENT_STYLE_* constants (the
!> styles of `comment_styles'); `comment_empty_lines' one of the
!> COMMENT_EMPTY_LINES_* constants; `comment_fill_column' negative for
!> none (use `fill_column'); `comment_padding' a string (upstream's integer
!> N is N spaces, and no padding the empty string).  A function that computes a
!> comment's indentation (`comment_indent_function') gives its answer as
!> a `comment_indent_type' (a column, a range (MIN and MAX), or none
!> (COMMENT_INDENT_NONE).  The region functions take the prefix argument as a number ARG
!> (absent for no prefix) and UNIVERSAL, .true. when it was C-u.
!>
!> Deferred: `combine_change_calls' (the region functions run the change
!> hooks for each change, as with `comment_combine_change_calls' false),
!> inheriting text properties (`insert_and_inherit' inserts plainly, as
!> everywhere in Fortmacs), buffer-local values of
!> `comment_region_function', `uncomment_region_function' and
!> `comment_quote_nested_function' (one global value each),
!> `use_hard_newlines', `comment_valid_prefix_p''s `string_width' of
!> non-ASCII prefixes (counted as one column per character), and the
!> obsolete aliases `indent_for_comment', `set_comment_column',
!> `kill_comment' and `indent_new_comment_line' as commands of their own
!> (they are the same commands under their upstream names).
module newcomment_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nilp, consp, fixnump, xfixnum
    use data_fm, only: value_eq, add_local_variables_killer
    use lread_fm, only: sym
    use data_fm, only: integer_setting, logical_setting, string_setting, function_setting, forward_setting
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: last_command, this_command
    use buffer_fm, only: buffers, cur, current_buffer, get_buffer_create, set_buffer, kill_buffer, &
                         generate_new_buffer_name, barf_if_buffer_read_only, fill_column, buffer_serial_live
    use insdel_fm, only: insert, insert_before_markers
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use editfns_fm, only: interactive_region, &
                          point, point_min, point_max, goto_char, bolp, eolp, bobp, eobp, char_after, char_before, &
                          line_beginning_position, line_end_position, delete_region, buffer_substring, &
                          narrow_to_region, widen, region_beginning, region_end
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line, delete_char
    use search_fm, only: re_search_forward, re_search_backward, looking_at, string_match, match_beginning, match_end, &
                         replace_match, regexp_quote, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string
    use fns_fm, only: substring, schars
    use syntax_fm, only: forward_comment, parse_partial_expression, syntax_ppss, parse_state_type, skip_syntax_backward, &
                         skip_syntax_forward, skip_chars_forward, skip_chars_backward, string_to_syntax, &
                         syntax_descriptor_type, syntax_entry, syntax_table, set_syntax_table, COMMENTSTOP_COMMENT
    use textprop_fm, only: get_text_property
    use indent_fm, only: current_column, current_indentation, move_to_column, indent_to, indent_according_to_mode, &
                         indent_to_left_margin, current_left_margin, move_to_left_margin, indent_tabs_mode
    use xdisp_fm, only: message, tab_width
    use minibuf_fm, only: read_string
    use fill_fm, only: adaptive_fill_mode
    use simple_fm, only: newline, delete_horizontal_space, kill_region, use_region_p, back_to_indentation, &
                         comment_line_break_function, fill_prefix, simple_count_lines => count_lines
    implicit none(type, external)
    private

    public :: syms_of_newcomment, comment_indent_type, comment_indent_proc, comment_region_proc, &
              comment_quote_nested_proc, comment_indent_function_type
    public :: comment_normalize_vars, comment_string_strip, comment_string_reverse, comment_search_forward, &
              comment_search_backward, comment_beginning, comment_forward, comment_enter_backward, &
              comment_indent_default, comment_choose_indent, comment_indent, comment_set_column, comment_kill, &
              comment_padright, comment_padleft, uncomment_region, uncomment_region_default, comment_region, &
              comment_region_default, comment_box, comment_only_p, comment_or_uncomment_region, comment_dwim, &
              comment_add_count, comment_indent_new_line, comment_line, comment_valid_prefix_p, &
              comment_quote_nested_default, comment_make_bol_ws

    !> `comment_use_syntax' values: COMMENT_USE_SYNTAX_NIL, COMMENT_USE_SYNTAX_T and COMMENT_USE_SYNTAX_UNDECIDED.
    integer, parameter, public :: COMMENT_USE_SYNTAX_NIL = 0, COMMENT_USE_SYNTAX_T = 1, COMMENT_USE_SYNTAX_UNDECIDED = 2

    !> `comment_style' values: the styles of `comment_styles'.
    integer, parameter, public :: COMMENT_STYLE_PLAIN = 1, COMMENT_STYLE_INDENT_OR_TRIPLE = 2, COMMENT_STYLE_INDENT = 3, &
                                  COMMENT_STYLE_ALIGNED = 4, COMMENT_STYLE_BOX = 5, COMMENT_STYLE_EXTRA_LINE = 6, &
                                  COMMENT_STYLE_MULTI_LINE = 7, COMMENT_STYLE_BOX_MULTI = 8

    !> `comment_empty_lines' values: COMMENT_EMPTY_LINES_NIL, COMMENT_EMPTY_LINES_T and COMMENT_EMPTY_LINES_EOL.
    integer, parameter, public :: COMMENT_EMPTY_LINES_NIL = 0, COMMENT_EMPTY_LINES_T = 1, COMMENT_EMPTY_LINES_EOL = 2

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
        subroutine comment_indent_proc(indent, status)
            import :: comment_indent_type, status_type
            implicit none(type, external)
            type(comment_indent_type), intent(out) :: indent
            type(status_type), intent(out) :: status
        end subroutine comment_indent_proc

        !> Comment or uncomment BEG .. END (correctly ordered), with the
        !> prefix argument ARG (absent for no prefix; UNIVERSAL true when it was a
        !> C-u list, ARG then its numeric value).  Not a port.
        subroutine comment_region_proc(beg, end, status, arg, universal)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: arg
            logical, intent(in), optional :: universal
        end subroutine comment_region_proc

        !> Quote (or with UNP unquote) the comment delimiters CS and CE in
        !> the buffer, narrowed to a single comment.  Not a port.
        subroutine comment_quote_nested_proc(cs, ce, unp, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: cs, ce
            logical, intent(in) :: unp
            type(status_type), intent(out) :: status
        end subroutine comment_quote_nested_proc
    end interface

    !> One buffer's `comment_indent_function'.
    type :: indent_entry_type
        integer(int64) :: serial = 0
        procedure(comment_indent_proc), pointer, nopass :: fn => null()
    end type indent_entry_type

    !> The type of `comment_indent_function': a default function and
    !> buffers' own (`set_local').
    type :: comment_indent_function_type
        procedure(comment_indent_proc), pointer, nopass :: value => null()
        type(indent_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's function (set_local).
        procedure :: set_local => indent_function_set_local
        !> The current buffer's function.
        procedure :: get => indent_function_get
        !> Whether the current buffer has its own function.
        procedure :: local_p => indent_function_local_p
        !> Make the current buffer use the default function again.
        procedure :: kill_local => indent_function_kill_local
    end type comment_indent_function_type

    !> True if syntax-tables can be used instead of regexps.
    !> Can also be `undecided' which means that a somewhat expensive test will
    !> be used to try to determine whether syntax-tables should be trusted
    !> to understand comments or not in the given buffer.
    !> Major modes should set this variable.  Port of newcomment.el
    !> comment-use-syntax (COMMENT_USE_SYNTAX_NIL, _T or _UNDECIDED).
    type(integer_setting), target, public :: comment_use_syntax = &
                                             integer_setting(name="comment_use_syntax", value=COMMENT_USE_SYNTAX_UNDECIDED)

    !> Column to use for `comment_indent'.  If negative, use
    !> `fill_column' instead.  Port of newcomment.el comment-fill-column.
    type(integer_setting), target, public :: comment_fill_column = integer_setting(name="comment_fill_column", value=-1)

    !> Column to indent right-margin comments to.
    !> Each mode may establish a different default value for this variable; you
    !> can set the value for a particular mode using that mode's hook.
    !> Comments might be indented to a different value in order not to go beyond
    !> `comment_fill_column' or in order to align them with surrounding comments.
    !> Port of newcomment.el comment-column (buffer-local when set).
    type(integer_setting), target, public :: comment_column = integer_setting(name="comment_column", value=32)

    !> String to insert to start a new comment, or unallocated if no comment
    !> syntax.  Port of newcomment.el comment-start.
    type(string_setting), target, public :: comment_start = string_setting(name="comment_start")

    !> Regexp to match the start of a comment plus everything up to its body.
    !> If there are any \(...\) groups and `comment_use_syntax' is COMMENT_USE_SYNTAX_NIL,
    !> the comment delimiter text is held to begin at the place matched
    !> by the close of the first group.  Port of newcomment.el
    !> comment-start-skip.
    type(string_setting), target, public :: comment_start_skip = string_setting(name="comment_start_skip")

    !> Regexp to match the end of a comment plus everything back to its
    !> body.  Port of newcomment.el comment-end-skip.
    type(string_setting), target, public :: comment_end_skip = string_setting(name="comment_end_skip")

    !> String to insert to end a new comment.
    !> Should be an empty string if comments are terminated by end-of-line.
    !> Port of newcomment.el comment-end.
    type(string_setting), target, public :: comment_end = string_setting(name="comment_end")

    !> Function to compute desired indentation for a comment.
    !> This function is called with no args with point at the beginning
    !> of the comment's starting delimiter and should return either the
    !> desired column indentation, a range of acceptable
    !> indentation (MIN and MAX), or none.
    !> If none is returned, indentation is delegated to
    !> `indent_according_to_mode'.  Port of newcomment.el
    !> comment-indent-function (`set_local' gives a buffer its own).
    type(comment_indent_function_type), public :: comment_indent_function

    !> Function to insert a comment when a line doesn't contain one.
    !> The function has no args.
    !>
    !> Applicable at least in modes for languages like fixed-format Fortran where
    !> comments always start in column zero.  Port of newcomment.el
    !> comment-insert-comment-function.
    type(function_setting), target, public :: comment_insert_comment_function = &
                                              function_setting(name="comment_insert_comment_function")

    !> If true (the default), use `combine_change_calls' around
    !> calls of `comment_region_function' and
    !> `uncomment_region_function'.  This Substitutes a single call to
    !> each of the hooks `before_change_functions' and
    !> `after_change_functions' in place of those hooks being called
    !> for each individual buffer change.  Port of newcomment.el
    !> comment-combine-change-calls (combine-change-calls is Deferred).
    type(logical_setting), target, public :: comment_combine_change_calls = &
                                             logical_setting(name="comment_combine_change_calls", value=.true.)

    !> Function to comment a region.
    !> Its args are the same as those of `comment_region', but BEG and END are
    !> guaranteed to be correctly ordered.  It is called within `save_excursion'.
    !>
    !> Applicable at least in modes for languages like fixed-format Fortran where
    !> comments always start in column zero.  Port of newcomment.el
    !> comment-region-function.
    procedure(comment_region_proc), pointer, public :: comment_region_function => null()

    !> Function to uncomment a region.
    !> Its args are the same as those of `uncomment_region', but BEG and END are
    !> guaranteed to be correctly ordered.  It is called within `save_excursion'.
    !>
    !> Applicable at least in modes for languages like fixed-format Fortran where
    !> comments always start in column zero.  Port of newcomment.el
    !> uncomment-region-function.
    procedure(comment_region_proc), pointer, public :: uncomment_region_function => null()

    !> True if nested comments should be quoted.
    !> This should be locally set by each major mode if needed.  Port of
    !> newcomment.el comment-quote-nested.
    type(logical_setting), target, public :: comment_quote_nested = &
                                             logical_setting(name="comment_quote_nested", value=.true.)

    !> Function to quote nested comments in a region.
    !> It takes the same arguments as `comment_quote_nested_default',
    !> and is called with the buffer narrowed to a single comment.  Port of
    !> newcomment.el comment-quote-nested-function.
    procedure(comment_quote_nested_proc), pointer, public :: comment_quote_nested_function => null()

    !> Continuation string to insert for multiline comments.
    !> This string will be added at the beginning of each line except the very
    !> first one when commenting a region with a commenting style that allows
    !> comments to span several lines.
    !> It should generally have the same length as `comment_start' in order to
    !> preserve indentation.
    !> If it is unallocated a value will be automatically derived from
    !> `comment_start' by replacing its first character with a space.  Port
    !> of newcomment.el comment-continue.
    type(string_setting), target, public :: comment_continue = string_setting(name="comment_continue")

    !> How many more comment chars should be inserted by `comment_region'.
    !> This determines the default value of the numeric argument of `comment_region'.
    !> The `plain' comment style doubles this value.
    !>
    !> This should generally stay 0, except for a few modes like Lisp where
    !> it is 1 so that regions are commented with two or three semi-colons.
    !> Port of newcomment.el comment-add.
    type(integer_setting), target, public :: comment_add = integer_setting(name="comment_add", value=0)

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
    !> If INDENT is `multi_char', that means indent multi-character
    !>   comment starters, but not one-character comment starters.  Port of
    !> newcomment.el comment-styles (the COMMENT_STYLE_* constants index
    !> these arrays; INDENT 2 stands for `multi_char').
    logical, parameter :: style_multi(8) = [.false., .false., .false., .false., .false., .true., .true., .true.]
    logical, parameter :: style_align(8) = [.false., .false., .false., .true., .true., .false., .false., .true.]
    logical, parameter :: style_extra(8) = [.false., .false., .false., .false., .true., .true., .false., .true.]
    integer, parameter :: style_indent(8) = [0, 2, 1, 1, 1, 1, 1, 1]

    !> Style to be used for `comment_region'.
    !> See `comment_styles' for a list of available styles.  Port of
    !> newcomment.el comment-style (a COMMENT_STYLE_* constant).
    type(integer_setting), target, public :: comment_style = integer_setting(name="comment_style", &
                                                                             value=COMMENT_STYLE_INDENT)

    !> Padding string that `comment_region' puts between comment chars and text.
    !> Can also be an integer which will be automatically turned into a string
    !> of the corresponding number of spaces.
    !>
    !> Extra spacing between the comment characters and the comment text
    !> makes the comment easier to read.  Default is 1.  An empty string
    !> means 0.  Port of newcomment.el comment-padding (a string).
    type(string_setting), target, public :: comment_padding = string_setting(name="comment_padding")

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
    type(integer_setting), target, public :: comment_inline_offset = integer_setting(name="comment_inline_offset", value=1)

    !> True means `comment_indent_new_line' continues comments.
    !> That is, it inserts no new terminator or starter.
    !> This affects `auto_fill_mode', which is the main reason to
    !> customize this variable.
    !>
    !> It also affects \[indent_new_comment_line].  However, if you want this
    !> behavior for explicit filling, you might as well use \[newline_and_indent].
    !> Port of newcomment.el comment-multi-line.
    type(logical_setting), target, public :: comment_multi_line = logical_setting(name="comment_multi_line", value=.false.)

    !> If COMMENT_EMPTY_LINES_NIL, `comment_region' does not comment out
    !> empty lines.  If COMMENT_EMPTY_LINES_T, it always comments out empty
    !> lines.  If COMMENT_EMPTY_LINES_EOL, it only comments out empty lines
    !> if comments are terminated by the end of line (i.e., `comment_end'
    !> is empty).  Port of newcomment.el comment-empty-lines.
    type(integer_setting), target, public :: comment_empty_lines = &
                                             integer_setting(name="comment_empty_lines", value=COMMENT_EMPTY_LINES_NIL)

    !> Function to set up variables needed by commenting functions (null
    !> to do nothing).  Port of newcomment.el comment-setup-function.
    procedure(no_arg_proc), pointer, public :: comment_setup_function => null()

    !> True means that the global syntactic context is used.
    !> More specifically, it means that `syntax_ppss' is used to find out whether
    !> point is within a string or not.  Major modes whose syntax is not faithfully
    !> described by the syntax-tables (or where `font_lock_syntax_table' is radically
    !> different from the main syntax table) can set this to false,
    !> then `syntax_ppss' cache won't be used in comment-related routines.
    !> Port of newcomment.el comment-use-global-state (obsolete).
    logical, public :: comment_use_global_state = .true.

    !> True means to only auto-fill inside comments.
    !> This has no effect in modes that do not define a comment syntax.
    !> Port of newcomment.el comment-auto-fill-only-comments.
    type(logical_setting), target, public :: comment_auto_fill_only_comments = &
                                             logical_setting(name="comment_auto_fill_only_comments", value=.false.)

    abstract interface
        !> A procedure of no arguments.  Not a port.
        subroutine no_arg_proc()
            implicit none(type, external)
        end subroutine no_arg_proc
    end interface

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    !> The restriction saved by `save_restriction': markers at BEGV and ZV
    !> when the buffer is narrowed.
    type :: restriction_type
        logical :: narrowed = .false.
        type(handle_type) :: beg, end
    end type restriction_type

contains

    !> Define the commenting commands and variables (bindings_fm binds
    !> M-;, C-x ; and C-x C-;, as bindings.el does).  Not a port.
    subroutine syms_of_newcomment()
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
        call add_local_variables_killer(kill_newcomment_locals)
        if (.not. associated(comment_indent_function%value)) comment_indent_function%value => comment_indent_default
        if (.not. associated(comment_region_function)) comment_region_function => comment_region_default
        if (.not. associated(uncomment_region_function)) uncomment_region_function => uncomment_region_default
        if (.not. associated(comment_quote_nested_function)) comment_quote_nested_function => comment_quote_nested_default
        comment_line_break_function => comment_line_break
        commands%comment_indent%run => cmd_comment_indent
        commands%comment_set_column%run => cmd_comment_set_column
        commands%comment_kill%run => cmd_comment_kill
        commands%uncomment_region%run => cmd_uncomment_region
        commands%comment_region%run => cmd_comment_region
        commands%comment_box%run => cmd_comment_box
        commands%comment_or_uncomment_region%run => cmd_comment_or_uncomment_region
        commands%comment_dwim%run => cmd_comment_dwim
        commands%comment_indent_new_line%run => cmd_comment_indent_new_line
        commands%comment_line%run => cmd_comment_line
    end subroutine syms_of_newcomment

    ! ------------------------------------------------------------------
    ! The buffer-local comment-indent-function.
    ! Make FN the current buffer's local function.  Not a port.
    subroutine indent_function_set_local(self, fn)
        class(comment_indent_function_type), intent(inout) :: self
        procedure(comment_indent_proc) :: fn
        type(indent_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%fn => fn
                return
            end if
        end do

        ! Reuse the entry of a dead buffer.
        do i = 1, size(self%entries)
            if (.not. buffer_serial_live(self%entries(i)%serial)) then
                self%entries(i)%serial = buffers(cur)%serial
                self%entries(i)%fn => fn
                return
            end if
        end do

        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%serial = self%entries(i)%serial
            grown(i)%fn => self%entries(i)%fn
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine indent_function_set_local

    ! The function in effect in the current buffer: its local one, else
    ! the default.  Not a port.
    function indent_function_get(self) result(fn)
        class(comment_indent_function_type), intent(in) :: self
        procedure(comment_indent_proc), pointer :: fn
        integer :: i

        fn => self%value
        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                if (associated(self%entries(i)%fn)) fn => self%entries(i)%fn
            end if
        end do
    end function indent_function_get

    ! Whether the current buffer has a local function.  Not a port.
    logical function indent_function_local_p(self) result(local)
        class(comment_indent_function_type), intent(in) :: self
        integer :: i

        local = .false.
        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                local = associated(self%entries(i)%fn)
                return
            end if
        end do
    end function indent_function_local_p

    ! Forget the current buffer's local function.  Not a port.
    subroutine indent_function_kill_local(self)
        class(comment_indent_function_type), intent(inout) :: self
        integer :: i

        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%serial = 0
                self%entries(i)%fn => null()
            end if
        end do
    end subroutine indent_function_kill_local

    !> kill-all-local-variables: forget the current buffer's
    !> `comment_indent_function'.  Not a port.
    subroutine kill_newcomment_locals()
        call comment_indent_function%kill_local()
    end subroutine kill_newcomment_locals

    ! ------------------------------------------------------------------
    ! Small helpers.
    !> `string_match' of RE on S: the 0-based index of the match, -1 for
    !> none.  Not a port.
    integer(int64) function smatch(re, s) result(idx)
        character(len=*), intent(in) :: re, s
        type(status_type) :: st

        idx = string_match(re, s, st) - 1
        if (.not. st%ok()) idx = -1
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

    !> `looking_at' of RE.  Not a port.
    logical function looking(re)
        character(len=*), intent(in) :: re
        type(status_type) :: st

        looking = looking_at(re, st)
        if (.not. st%ok()) looking = .false.
    end function looking

    !> `re_search_forward' of RE with BOUND and NOERROR; .true. when found.
    !> Not a port.
    logical function re_forward(re, bound, noerror)
        character(len=*), intent(in) :: re
        integer(int64), intent(in), optional :: bound
        integer, intent(in) :: noerror
        integer(int64) :: found
        type(status_type) :: st

        call re_search_forward(re, found, st, bound=bound, noerror=noerror)
        re_forward = st%ok() .and. found > 0
    end function re_forward

    !> `re_search_backward' of RE with BOUND and NOERROR; .true. when found.
    !> Not a port.
    logical function re_backward(re, bound, noerror)
        character(len=*), intent(in) :: re
        integer(int64), intent(in), optional :: bound
        integer, intent(in) :: noerror
        integer(int64) :: found
        type(status_type) :: st

        call re_search_backward(re, found, st, bound=bound, noerror=noerror)
        re_backward = st%ok() .and. found > 0
    end function re_backward

    !> The character at POS (default point), -1 if none.  Not a port.
    integer function after(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found

        call char_after(c, found, pos)
        after = -1
        if (found) after = c
    end function after

    !> The character before POS (default point), -1 if none.  Not a port.
    integer function before(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found

        call char_before(c, found, pos)
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
    function repeat_char(c, n) result(s)
        character(len=*), intent(in) :: c
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s

        s = repeat(c, int(max(n, 0_int64)))
    end function repeat_char

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

    !> Whether comment-use-syntax is true (anything but COMMENT_USE_SYNTAX_NIL: COMMENT_USE_SYNTAX_UNDECIDED
    !> counts as true, as upstream).  Not a port.
    logical function use_syntax()
        use_syntax = comment_use_syntax%value /= COMMENT_USE_SYNTAX_NIL
    end function use_syntax

    !> A marker at POS that advances when text is inserted at it if
    !> INSERTION_TYPE.  Not a port.
    function marker_at(pos, insertion_type) result(m)
        integer(int64), intent(in) :: pos
        logical, intent(in), optional :: insertion_type
        type(handle_type) :: m
        type(status_type) :: st
        logical :: it

        it = .false.
        if (present(insertion_type)) it = insertion_type
        call copy_marker_at(pos, m, it, st)
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

    !> Save the current restriction (save-restriction).  Not a port.
    function save_restriction() result(r)
        type(restriction_type) :: r

        r%narrowed = buffers(cur)%begv /= 1 .or. buffers(cur)%zv /= buffers(cur)%text%z

        if (r%narrowed) then
            r%beg = marker_at(buffers(cur)%begv)
            r%end = marker_at(buffers(cur)%zv, .true.)
        end if
    end function save_restriction

    !> Restore restriction R (the end of save-restriction).  Not a port.
    subroutine restore_restriction(r)
        type(restriction_type), intent(inout) :: r
        type(status_type) :: st

        call widen()

        if (r%narrowed) then
            call narrow_to_region(mpos(r%beg), mpos(r%end), st)
            call unmark(r%beg)
            call unmark(r%end)
        end if
    end subroutine restore_restriction

    ! ------------------------------------------------------------------
    ! Helpers.
    !> Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space.
    !> Port of newcomment.el:comment-string-strip.
    function comment_string_strip(str, beforep, afterp) result(s)
        character(len=*), intent(in) :: str
        logical, intent(in) :: beforep, afterp
        character(len=:), allocatable :: s
        character(len=:), allocatable :: re
        integer(int64) :: k

        re = "\`"
        if (beforep) re = re//"\s-*"
        re = re//"\(.*?\)"
        if (afterp) re = re//"\s-*"//LF//"?"
        re = re//"\'"
        k = smatch(re, str)
        s = match_string(1, str)
    end function comment_string_strip

    !> Return the mirror image of string S, without any trailing space.
    !> Port of newcomment.el:comment-string-reverse.
    function comment_string_reverse(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        character(len=:), allocatable :: rev
        integer :: i, j

        rev = ""
        i = 1

        do while (i <= len(s))
            j = i + 1

            do while (j <= len(s))
                if (iand(iachar(s(j:j)), 192) /= 128) exit
                j = j + 1
            end do

            rev = s(i:j - 1)//rev
            i = j
        end do

        r = comment_string_strip(rev, .false., .true.)
    end function comment_string_reverse

    !> Check and set up variables needed by other commenting functions.
    !> All the `comment_*' commands call this function to set up various
    !> variables, like `comment_start', to ensure that the commenting
    !> functions work correctly.  Fortran callers of any other `comment_*'
    !> function should first call this function explicitly.  Port of
    !> newcomment.el:comment-normalize-vars.
    recursive subroutine comment_normalize_vars(status, noerror)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        character(len=:), allocatable :: cs, ce, skip
        logical :: noerr, ok
        type(handle_type) :: old, tmp, table
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
                call set_error(status, FM_GENERIC_ERROR, "No comment syntax defined")
                return
            end if

            call comment_start%set_local(cs)
            call comment_start_skip%set_local(cs)
        end if

        ! comment-use-syntax
        if (comment_use_syntax%value == COMMENT_USE_SYNTAX_UNDECIDED) then
            ! Try to skip over a comment using forward-comment
            ! to see if the syntax tables properly recognize it.
            table = syntax_table()
            cs = cstart()
            ce = cend()
            if (ce == "") ce = LF
            old = current_buffer()
            call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st, inhibit_buffer_hooks=.true.)
            call set_buffer(tmp, st)
            call set_syntax_table(table, st)
            call insert(cs//" hello "//ce, st)
            call goto_char(point_min())
            ok = forward_comment(1_int64)
            if (ok) ok = eobp()
            call set_buffer(old, st)
            call kill_buffer(tmp, st)

            if (ok) then
                call comment_use_syntax%set_local(COMMENT_USE_SYNTAX_T)
            else
                call comment_use_syntax%set_local(COMMENT_USE_SYNTAX_NIL)
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

            if (comment_use_syntax%value /= COMMENT_USE_SYNTAX_T) then
                ! `syntax-ppss' will detect escaping.
                skip = "\(\(^\|[^\"//LF//"]\)\(\\\\\)*\)"
            end if

            ! Let's not allow any \s- but only [ \t] since \n
            ! might be both a comment-end marker and \s-.
            skip = skip//"\(?:\s<+\|"//regexp_quote(comment_string_strip(cs, .true., .true.))//"+\)[ "//TAB//"]*"
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
            skip = skip//"\|"//regexp_quote(sub(ce, 0_int64, 1_int64))
            if (.not. (comment_quote_nested%value .and. schars(ce) <= 1)) skip = skip//"+"
            skip = skip//regexp_quote(sub(ce, 1_int64))//"\)"
            call comment_end_skip%set_local(skip)
        end if
    end subroutine comment_normalize_vars

    !> The regexp matching STR with a backslash (several with UNP) after
    !> its first character.  Port of newcomment.el:comment-quote-re.
    function comment_quote_re(str, unp) result(re)
        character(len=*), intent(in) :: str
        logical, intent(in) :: unp
        character(len=:), allocatable :: re

        re = regexp_quote(sub(str, 0_int64, 1_int64))//"\\"

        if (unp) then
            re = re//"+"
        else
            re = re//"*"
        end if

        re = re//regexp_quote(sub(str, 1_int64))
    end function comment_quote_re

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
        character(len=:), allocatable :: re
        integer(int64) :: b, e
        type(status_type) :: st

        status = status_type()
        re = comment_quote_re(ce, unp)//"\|"//comment_quote_re(cs, unp)
        call goto_char(point_min())

        do while (re_forward(re, noerror=SEARCH_NOERROR))
            b = mb(0)
            e = me(0)
            call goto_char(b)
            call forward_char(st, 1_int64)

            if (unp) then
                call delete_char(1_int64, status)
            else
                call insert("\", status)
            end if

            if (.not. status%ok()) return

            if (schars(ce) == 1) then
                ! If the comment-end is a single char, adding a \ after that
                ! "first" char won't deactivate it, so we turn such a CE
                ! into !CS.  I.e. for pascal, we turn } into !{
                if (.not. unp) then
                    if (text(b, e) == ce) then
                        call replace_match("!"//cs, status, fixedcase=.true., literal=.true.)
                        if (.not. status%ok()) return
                    end if
                else
                    if (point_min() < b) then
                        if (text(b - 1, e - 1) == "!"//cs) then
                            call goto_char(point() - 2)
                            call delete_char(e - b, status)
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
    !> Ensure that `comment_normalize_vars' has been called before you use
    !> this.  Port of newcomment.el:comment-search-forward.
    recursive integer(int64) function comment_search_forward(limit, noerror, status) result(start)
        integer(int64), intent(in) :: limit
        logical, intent(in) :: noerror
        type(status_type), intent(out) :: status
        type(parse_state_type) :: s, s2, old
        integer(int64) :: pt, to, pos, bol, end
        type(status_type) :: st

        status = status_type()
        start = 0

        if (.not. use_syntax()) then
            if (limit > 0) then
                if (re_forward(cskip(), limit, SEARCH_NOERROR)) then
                    start = me(1)
                    if (start == 0) start = mb(0)
                    return
                end if

                call goto_char(limit)
            else
                if (re_forward(cskip(), noerror=SEARCH_NOERROR)) then
                    start = me(1)
                    if (start == 0) start = mb(0)
                    return
                end if
            end if

            if (.not. noerror) call set_error(status, FM_GENERIC_ERROR, "No comment")
            return
        end if

        pt = point()
        to = point_max()
        if (limit > 0) to = limit

        ! Assume (at first) that pt is outside of any string.
        if (comment_use_global_state) then
            call syntax_ppss(old, pt)
            call parse_partial_expression(pt, to, s, st, oldstate=old, commentstop=COMMENTSTOP_COMMENT)
        else
            call parse_partial_expression(pt, to, s, st, commentstop=COMMENTSTOP_COMMENT)
        end if

        if (s%comstr_start > 0 .and. s%instring /= -1 .and. .not. comment_use_global_state) then
            ! The search ended at eol inside a string.  Try to see if it
            ! works better when we assume that pt is inside a string.
            old = parse_state_type()
            old%instring = s%instring
            call goto_char(pt)
            call parse_partial_expression(pt, to, s2, st, oldstate=old, commentstop=COMMENTSTOP_COMMENT)
            s = s2
        end if

        if (.not. (s%comstr_start > 0 .and. s%instring == -1)) then
            if (.not. noerror) call set_error(status, FM_GENERIC_ERROR, "No comment")
            return
        end if

        ! Make sure the comment starts after PT.
        if (s%comstr_start < pt) then
            if (.not. noerror) call set_error(status, FM_GENERIC_ERROR, "No comment")
            return
        end if

        ! We found the comment.
        pos = point()
        start = s%comstr_start
        bol = line_beginning_position()
        end = 0

        do while (end == 0 .and. point() >= bol)
            if (looking(cskip())) then
                end = min(to, me(0))
            else
                if (point() <= point_min()) exit
                call goto_char(point() - 1)
            end if
        end do

        if (end > 0) then
            call goto_char(end)
        else
            call goto_char(pos)
        end if
    end function comment_search_forward

    !> Find a comment start between LIMIT (0: none) and point.
    !> Moves point to inside the comment and returns the position of the
    !> comment-starter.  If no comment is found, moves point to LIMIT
    !> and raises an error or returns 0 if NOERROR is true.
    !>
    !> Ensure that `comment_normalize_vars' has been called before you use
    !> this.  Port of newcomment.el:comment-search-backward.
    recursive integer(int64) function comment_search_backward(limit, noerror, status) result(cs)
        integer(int64), intent(in) :: limit
        logical, intent(in) :: noerror
        type(status_type), intent(out) :: status
        integer(int64) :: end, pt
        logical :: found
        type(status_type) :: st

        status = status_type()
        cs = 0

        if (limit > 0) then
            found = re_backward(cskip(), limit, SEARCH_MOVE)
        else
            found = re_backward(cskip(), noerror=SEARCH_MOVE)
        end if

        if (.not. found) then
            if (.not. noerror) call set_error(status, FM_GENERIC_ERROR, "No comment")
            return
        end if

        end = me(0)
        call beginning_of_line(1_int64)
        cs = comment_search_forward(end, .true., st)
        pt = point()

        if (cs == 0) then
            call beginning_of_line(1_int64)
            cs = comment_search_backward(limit, noerror, status)
            return
        end if

        do
            call goto_char(cs)
            found = comment_forward(1_int64)
            if (.not. (point() < end)) exit
            cs = comment_search_forward(end, .true., st)
            if (cs == 0) exit
            pt = point()
        end do

        call goto_char(pt)
    end function comment_search_backward

    !> Find the beginning of the enclosing comment.
    !> Returns 0 if not inside a comment, else moves point and returns
    !> the same as `comment_search_backward'.  Port of
    !> newcomment.el:comment-beginning.
    recursive integer(int64) function comment_beginning() result(start)
        type(parse_state_type) :: state
        type(restriction_type) :: r
        type(value_type) :: face
        integer(int64) :: pt, cs
        logical :: is_cs, ok, fwd
        type(status_type) :: st

        start = 0

        if (use_syntax() .and. comment_use_global_state) then
            pt = point()
            call syntax_ppss(state, pt)

            if (state%incomment /= 0) then
                call goto_char(state%comstr_start)
                start = point()
                ! `comment-start-skip' sometimes checks that the
                ! comment char is not escaped.  (Bug#16971)
                r = save_restriction()
                call narrow_to_region(point(), point_max(), st)
                ok = looking(cskip())
                call restore_restriction(r)
                if (ok) call goto_char(me(0))
            end if

            return
        end if

        ! Can't rely on the syntax table, let's guess based on font-lock.
        call get_text_property(point(), sym("face"), face, st)
        if (value_eq(face, sym("font_lock_string_face"))) return
        pt = point()
        cs = comment_search_backward(0_int64, .true., st)
        if (cs == 0) return
        call goto_char(cs)
        ! For modes where comment-start and comment-end are the same,
        ! the search above may have found a `ce' rather than a `cs'.
        is_cs = .false.

        if (allocated(comment_end_skip%value)) then
            is_cs = .not. looking(comment_end_skip%value)
        end if

        if (.not. is_cs) then
            ! Maybe font-lock knows that it's a `cs'?
            call get_text_property(me(0), sym("face"), face, st)
            is_cs = value_eq(face, sym("font_lock_comment_face"))
        end if

        if (.not. is_cs) then
            call get_text_property(point(), sym("face"), face, st)

            if (.not. value_eq(face, sym("font_lock_comment_face"))) then
                ! Let's assume it's a `cs' if we're on the same line.
                is_cs = line_end_position() >= pt
            end if
        end if

        if (is_cs) then
            ! Make sure that PT is not past the end of the comment.
            fwd = comment_forward(1_int64)

            if (fwd) then
                is_cs = point() > pt
            else
                is_cs = eobp()
            end if
        end if

        call goto_char(cs)

        if (is_cs) then
            start = cs
        else
            call goto_char(pt)
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
                    call goto_char(me(0))
                    ok = re_forward(comment_end_skip%value, noerror=SEARCH_MOVE)
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
        integer(int64) :: moved, pt
        logical :: found
        type(status_type) :: st

        if (bolp()) then
            ! comment-end = ""
            call goto_char(point() - 1)
            moved = skip_syntax_backward(" ")
            return
        end if

        pt = point()
        r = save_restriction()
        call narrow_to_region(line_beginning_position(), pt, st)
        call goto_char(point_min())
        found = .false.
        if (allocated(comment_end_skip%value)) found = re_forward(comment_end_skip%value//"\'", noerror=SEARCH_NOERROR)
        call restore_restriction(r)

        if (found) then
            call goto_char(mb(0))
            return
        end if

        call goto_char(pt)
        ! comment-end-skip not found probably because it was not set
        ! right.  Since \\s> should catch the single-char case, let's
        ! check that we're looking at a two-char comment ender.
        call string_to_syntax("  4", d4, st)
        call string_to_syntax("  3", d3, st)

        if (.not. (point_max() - line_beginning_position() <= 1 .or. &
                   iand(syntax_code_at(point() - 1), d4%code) == 0 .or. &
                   iand(syntax_code_at(point() - 2), d3%code) == 0)) then
            call goto_char(point() - 2)
            moved = skip_chars_backward(char_string(after()), st)
            moved = skip_syntax_backward(" ")
        else
            ! No clue what's going on: maybe we're really not right after the
            ! end of a comment.  Maybe we're at the "end" because of EOB rather
            ! than because of a marker.
            moved = skip_syntax_backward(" ")
        end if
    end subroutine comment_enter_backward

    !> The raw syntax code (with flags) of the character at POS (the first element of
    !> syntax-after), 0 outside the buffer.  Not a port.
    integer(int32) function syntax_code_at(pos)
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type) :: d
        integer :: c

        syntax_code_at = 0
        c = after(pos)
        if (c < 0) return
        d = syntax_entry(c)
        if (d%code > 0) syntax_code_at = d%code
    end function syntax_code_at

    !> The UTF-8 string of character C (empty for -1).  Not a port.
    function char_string(c) result(s)
        use character_fm, only: char_string_utf8 => char_string, MAX_MULTIBYTE_LENGTH
        integer, intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nbytes
        type(status_type) :: st

        s = ""
        if (c < 0) return
        call char_string_utf8(int(c, int32), buf, nbytes, st)
        if (st%ok()) s = buf(1:nbytes)
    end function char_string

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
    recursive integer(int64) function comment_choose_indent(indent_in) result(col)
        type(comment_indent_type), intent(in), optional :: indent_in
        integer(int64) :: indent, min, max, other, opoint, omax, omin, moved, eol_col
        logical :: have_other, is_range
        type(status_type) :: st

        indent = comment_column%value
        opoint = point()
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
            eol_col = current_column()
            call goto_char(opoint)
            max = current_column() + (the_fill_column() - eol_col)
            moved = skip_chars_backward(" "//TAB, st)

            ! Leave at least `comment-inline-offset' space after
            ! other nonwhite text on the line.
            if (bolp()) then
                min = 0
            else
                min = comment_inline_offset%value + current_column()
            end if

            call goto_char(opoint)
        end if

        ! Fix up the range.
        if (max < min) max = min
        ! Don't move past the fill column.
        if (max <= indent) indent = max
        ! We can choose anywhere between min..max.
        ! Let's try to align to a comment on the previous line.
        have_other = .false.
        other = 0

        if (forward_line(-1_int64) == 0) then
            other = comment_search_forward(line_end_position(), .true., st)

            if (other > 0) then
                call goto_char(other)
                other = current_column()
                have_other = .true.
            end if
        end if

        call goto_char(opoint)

        if (have_other) then
            if (other <= max .and. other >= min) then
                ! There is a comment and it's in the range: bingo!
                col = other
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
            call goto_char(other)
            call end_of_line(1_int64)
            eol_col = current_column()
            call goto_char(other)
            omax = current_column() + (the_fill_column() - eol_col)
            moved = skip_chars_backward(" "//TAB, st)
            omin = current_column() + 1
            call goto_char(other)

            if (omax >= min .and. omin <= max) then
                min = merge(omin, min, omin > min)
                max = merge(omax, max, omax < max)
            else
                ! Can't align with this anyway, so exit the loop.
                exit
            end if
        end do

        call goto_char(opoint)
        ! Return the closest point to indent within min..max.
        col = merge(indent, max, indent < max)
        if (col < min) col = min
    end function comment_choose_indent

    !> Indent this line's comment to `comment_column', or insert an empty comment.
    !> If CONTINUE is true, use the `comment_continue' markers if any.
    !> Port of newcomment.el:comment-indent.
    recursive subroutine comment_indent(status, continue)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: continue
        character(len=:), allocatable :: starter, ender
        integer(int64) :: begpos, opoint, moved, n, col
        type(handle_type) :: cpos
        type(comment_indent_type) :: indent
        type(parse_state_type) :: state
        procedure(comment_indent_proc), pointer :: fn
        logical :: cont, in_multi, at_bol
        type(status_type) :: st

        cont = .false.
        if (present(continue)) cont = continue
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_vars(status)
        if (.not. status%ok()) return
        call beginning_of_line(1_int64)

        if (cont .and. allocated(comment_continue%value)) then
            starter = comment_continue%value
            ender = ""
        else if (allocated(comment_start%value)) then
            starter = comment_start%value
            ender = cend()
        else
            call set_error(status, FM_GENERIC_ERROR, "No comment syntax defined")
            return
        end if

        begpos = comment_search_forward(line_end_position(), .true., st)
        ! If we couldn't find a comment *starting* on this line, see if we
        ! are already within a multiline comment at BOL (bug#78003).
        in_multi = .false.

        if (begpos == 0 .and. .not. cont .and. use_syntax() .and. comment_use_global_state) then
            opoint = point()
            call syntax_ppss(state, line_beginning_position())
            in_multi = state%incomment /= 0
            call goto_char(opoint)
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
                    moved = skip_chars_backward(" "//TAB, st)
                    call goto_char(point() + (-moved)/2)
                end if
            end if

            cpos = marker_at(point())
        else
            ! If none, insert one.
            opoint = point()
            ! Some `comment-indent-function's insist on not moving
            ! comments that are in column 0, so we first go to the
            ! likely target column.
            col = indent_to(comment_column%value, status)
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
            call goto_char(opoint)
        end if

        call goto_char(begpos)
        ! Compute desired indent.
        opoint = point()
        fn => comment_indent_function%get()
        call fn(indent, st)
        call goto_char(opoint)
        ! If `indent' is nil and there's code before the comment, we can't
        ! use `indent-according-to-mode', so we default to comment-column.
        moved = skip_chars_backward(" "//TAB, st)
        at_bol = bolp()
        call goto_char(opoint)

        if (indent%kind == COMMENT_INDENT_NONE .and. .not. at_bol) then
            indent%kind = COMMENT_INDENT_COLUMN
            indent%column = comment_column%value
        end if

        if (indent%kind == COMMENT_INDENT_NONE) then
            ! comment-indent-function refuses: delegate to line-indent.
            call indent_according_to_mode(status)
        else
            ! If the comment is at the right of code, adjust the indentation.
            if (.not. at_bol) then
                col = comment_choose_indent(indent)
            else if (indent%kind == COMMENT_INDENT_COLUMN) then
                col = indent%column
            else
                ! A range at the beginning of a line: (car indent) is a
                ! cons, which upstream's `=' rejects; use MIN.
                col = indent%min
            end if

            ! If that's different from comment's current position, change it.
            if (current_column() /= col) then
                opoint = point()
                moved = skip_chars_backward(" "//TAB, st)
                call delete_region(point(), opoint, status)
                if (.not. status%ok()) return
                col = indent_to(col, status)
            end if
        end if

        call goto_char(mpos(cpos))
        call unmark(cpos)
    end subroutine comment_indent

    !> `comment_indent' as a command.
    recursive subroutine cmd_comment_indent(status)
        type(status_type), intent(out) :: status

        call comment_indent(status)
    end subroutine cmd_comment_indent

    !> Set the comment column based on point.
    !> With no ARG, set the comment column to the current column.
    !> With just minus as arg, kill any comment on this line.
    !> With any other arg, set comment column to indentation of the previous comment
    !>  and then align or create a comment on this line at that column.
    !> Port of newcomment.el:comment-set-column (MINUS for the argument
    !> `-', ARG present for another argument).
    recursive subroutine comment_set_column(status, arg, minus)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg, minus
        integer(int64) :: opoint, cs
        character(len=24) :: buf
        type(status_type) :: st

        status = status_type()

        if (present(minus)) then
            if (minus) then
                call comment_kill(status)
                return
            end if
        end if

        if (present(arg)) then
            if (arg) then
                call comment_normalize_vars(status)
                if (.not. status%ok()) return
                opoint = point()
                call beginning_of_line(1_int64)
                cs = comment_search_backward(0_int64, .false., status)

                if (.not. status%ok()) then
                    call goto_char(opoint)
                    return
                end if

                call beginning_of_line(1_int64)
                cs = comment_search_forward(line_end_position(), .false., status)

                if (.not. status%ok()) then
                    call goto_char(opoint)
                    return
                end if

                call goto_char(cs)
                call comment_column%set_local(current_column())
                write (buf, "(i0)") comment_column%value
                call message("Comment column set to "//trim(buf))
                call goto_char(opoint)
                call comment_indent(status)
                return
            end if
        end if

        call comment_column%set_local(current_column())
        write (buf, "(i0)") comment_column%value
        call message("Comment column set to "//trim(buf))
        st = status_type()
    end subroutine comment_set_column

    !> Set the comment column based on point.
    !> With no ARG, set the comment column to the current column.
    !> With just minus as arg, kill any comment on this line.
    !> With any other arg, set comment column to indentation of the previous comment
    !>  and then align or create a comment on this line at that column.
    !> Port of newcomment.el:comment-set-column (the command).
    recursive subroutine cmd_comment_set_column(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call comment_set_column(status)
        else if (value_eq(current_prefix_arg, sym("-"))) then
            call comment_set_column(status, minus=.true.)
        else
            call comment_set_column(status, arg=.true.)
        end if
    end subroutine cmd_comment_set_column

    !> Kill the first comment on this line, if any.
    !> With prefix ARG, kill comments on that many lines starting with this
    !> one.  Port of newcomment.el:comment-kill (ARG absent for no prefix).
    recursive subroutine comment_kill(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: i, n, cs, opoint, moved, e, shortage
        logical :: done
        type(status_type) :: st

        call comment_normalize_vars(status)
        if (.not. status%ok()) return
        n = 1
        if (present(arg)) n = arg

        do i = 1, n
            opoint = point()
            call beginning_of_line(1_int64)
            cs = comment_search_forward(line_end_position(), .true., st)

            if (cs > 0) then
                call goto_char(cs)
                moved = skip_syntax_backward(" ")
                cs = point()
                done = comment_forward(1_int64)
                e = point()
                if (bolp()) e = e - 1
                call kill_region(cs, e, status)
                if (.not. status%ok()) return
                call indent_according_to_mode(status)
                if (.not. status%ok()) return
            end if

            ! (save-excursion: point returns to where it was.)
            call goto_char(min(opoint, point_max()))
            if (present(arg)) shortage = forward_line(1_int64)
        end do
    end subroutine comment_kill

    !> Kill the first comment on this line, if any.
    !> With prefix ARG, kill comments on that many lines starting with this
    !> one.  Port of newcomment.el:comment-kill (the command).
    recursive subroutine cmd_comment_kill(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call comment_kill(status)
        else
            call comment_kill(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_comment_kill

    !> Construct a string composed of STR plus `comment_padding'.
    !> It also adds N copies of the last non-whitespace chars of STR.
    !> If STR already contains padding, the corresponding amount is
    !> ignored from `comment_padding'.
    !> N defaults to 0.
    !> If RE is true, a regexp is returned instead, that would match
    !> the string for any N (upstream's N `re').  FOUND is .false.
    !> when STR is unallocated or has no non-whitespace character.
    !>
    !> Ensure that `comment_normalize_vars' has been called before you
    !> use this.  Port of newcomment.el:comment-padright.
    function comment_padright(str, n, re, found) result(r)
        character(len=*), intent(in), optional :: str
        integer(int64), intent(in), optional :: n
        logical, intent(in), optional :: re
        logical, intent(out), optional :: found
        character(len=:), allocatable :: r
        character(len=:), allocatable :: s, lpad, rpad, pad
        integer(int64) :: k, m1b, m1e, m0e, count
        logical :: multi, want_re
        integer :: i

        r = ""
        if (present(found)) found = .false.
        if (.not. present(str)) return
        if (smatch("\S-", str) < 0) return
        if (present(found)) found = .true.
        count = 0
        if (present(n)) count = n
        want_re = .false.
        if (present(re)) want_re = re
        ! Separate the actual string from any leading/trailing padding
        k = smatch("\`\s-*\(.*?\)\s-*\'", str)
        m1b = smb(1)
        m1e = sme(1)
        m0e = sme(0)
        s = match_string(1, str)
        lpad = sub(str, 0_int64, m1b)
        pad = cpad()

        if (smatch("\`\s-", pad) < 0) then
            ! If the padding isn't spaces, then don't
            ! shorten the padding.
            rpad = sub(str, m1e)//pad
        else
            rpad = sub(str, m1e)//sub(pad, min(m0e - m1e, schars(pad)))
        end if

        ! We can only duplicate C if the comment-end has multiple chars
        ! or if comments can be nested, else the comment-end `}' would
        ! be turned into `}}}' where only the first ends the comment
        ! and the rest becomes bogus junk.
        multi = .not. (comment_quote_nested%value .and. smatch("\`\s-*\S-\s-*\'", cend()) >= 0)

        if (.not. want_re) then
            r = lpad//s

            if (multi) then
                do i = 1, int(count)
                    r = r//sub(str, m1e - 1, m1e)
                end do
            end if

            r = r//rpad
        else
            ! construct a regexp that would match anything from just S
            ! to any possible output of this function for any N.
            r = optional_chars(lpad)//regexp_quote(s)
            if (multi) r = r//"+"
            r = r//optional_chars(rpad)
        end if
    end function comment_padright

    !> The regexp matching each character of S optionally, in order
    !> (padding is not required).  Not a port.
    function optional_chars(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer(int64) :: k

        r = ""

        do k = 0, schars(s) - 1
            r = r//regexp_quote(sub(s, k, k + 1))//"?"
        end do
    end function optional_chars

    !> Construct a string composed of `comment_padding' plus STR.
    !> It also adds N copies of the first non-whitespace chars of STR.
    !> If STR already contains padding, the corresponding amount is
    !> ignored from `comment_padding'.
    !> N defaults to 0.
    !> If RE is true, a regexp is returned instead, that would match the
    !> string for any N.  FOUND is .false. when STR is absent or empty.
    !>
    !> Ensure that `comment_normalize_vars' has been called before you
    !> use this.  Port of newcomment.el:comment-padleft.
    function comment_padleft(str, n, re, found) result(r)
        character(len=*), intent(in), optional :: str
        integer(int64), intent(in), optional :: n
        logical, intent(in), optional :: re
        logical, intent(out), optional :: found
        character(len=:), allocatable :: r
        character(len=:), allocatable :: s, pad, padding, c
        integer(int64) :: k, m0b, m0e, count
        logical :: multi, want_re
        integer :: i

        r = ""
        if (present(found)) found = .false.
        if (.not. present(str)) return
        if (len(str) == 0) return
        if (present(found)) found = .true.
        count = 0
        if (present(n)) count = n
        want_re = .false.
        if (present(re)) want_re = re
        ! Only separate the left pad because we assume there is no right pad.
        k = smatch("\`\s-*", str)
        m0b = smb(0)
        m0e = sme(0)
        s = sub(str, m0e)
        padding = cpad()

        if (smatch("\`\s-", padding) < 0) then
            ! If the padding isn't spaces, then don't
            ! shorten the padding.
            pad = padding
        else
            pad = sub(padding, min(m0e - m0b, schars(padding)))
        end if

        pad = pad//sub(str, m0b, m0e)
        c = sub(str, m0e, m0e + 1)
        ! We can only duplicate C if the comment-end has multiple chars
        ! or if comments can be nested, else the comment-end `}' would
        ! be turned into `}}}' where only the first ends the comment
        ! and the rest becomes bogus junk.
        multi = .not. (comment_quote_nested%value .and. smatch("\`\s-*\S-\s-*\'", cend()) >= 0)

        if (.not. want_re) then
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
            if (multi) r = r//regexp_quote(c)//"*"
            r = r//regexp_quote(s)
        end if
    end function comment_padleft

    !> Uncomment each line in the BEG .. END region.
    !> The numeric prefix ARG can specify a number of chars to remove from the
    !> comment delimiters.  Port of newcomment.el:uncomment-region.
    recursive subroutine uncomment_region(beg_in, end_in, status, arg, universal)
        integer(int64), intent(in) :: beg_in, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: universal
        integer(int64) :: beg, end, opoint
        logical :: saved
        type(handle_type) :: om

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_vars(status)
        if (.not. status%ok()) return
        beg = min(beg_in, end_in)
        end = max(beg_in, end_in)
        ! Bind `comment-use-global-state' to nil.  While uncommenting a region
        ! (which works a line at a time), a comment can appear to be
        ! included in a multi-line string, but it is actually not.
        saved = comment_use_global_state
        comment_use_global_state = .false.
        opoint = point()
        om = marker_at(opoint)
        call uncomment_region_function(beg, end, status, arg, universal)
        comment_use_global_state = saved
        call goto_char(mpos(om))
        call unmark(om)
    end subroutine uncomment_region

    !> Uncomment each line in the BEG .. END region.
    !> The numeric prefix ARG can specify a number of chars to remove from the
    !> comment delimiters.  Port of newcomment.el:uncomment-region (the
    !> command, on the region).
    recursive subroutine cmd_uncomment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call interactive_region(beg, end, status)
        if (.not. status%ok()) return
        call with_prefix(beg, end, status, uncomment_region)
    end subroutine cmd_uncomment_region

    !> Call REGION_FN on BEG..END with the current prefix argument.
    !> Not a port.
    recursive subroutine with_prefix(beg, end, status, region_fn)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        procedure(comment_region_proc) :: region_fn

        if (nilp(current_prefix_arg)) then
            call region_fn(beg, end, status)
        else if (consp(current_prefix_arg)) then
            call region_fn(beg, end, status, prefix_numeric_value(current_prefix_arg), .true.)
        else
            call region_fn(beg, end, status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine with_prefix

    !> (region-beginning) and (region-end).  Not a port.
    subroutine region_bounds(beg, end, status)
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status

        call region_beginning(beg, status)
        if (.not. status%ok()) return
        call region_end(end, status)
    end subroutine region_bounds

    !> Uncomment each line in the BEG .. END region.
    !> The numeric prefix ARG can specify a number of chars to remove from the
    !> comment delimiters.
    !> This function is the default value of `uncomment_region_function'.
    !> Port of newcomment.el:uncomment-region-default-1.
    recursive subroutine uncomment_region_default_1(beg, end_in, status, arg)
        integer(int64), intent(in) :: beg, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(handle_type) :: endm, opointm
        type(restriction_type) :: r
        character(len=:), allocatable :: srei, csre, sre, cce, erei, ere
        integer(int64) :: numarg, spt, ipt, ept, moved, pos, fcol, slim, e
        logical :: box, box_equal, has_srei, ok, has_ere
        integer :: grp
        type(status_type) :: st

        status = status_type()
        call goto_char(beg)
        endm = marker_at(end_in)
        numarg = 1
        if (present(arg)) numarg = arg
        srei = comment_padright(comment_continue_or_absent(), re=.true., found=has_srei)

        if (.not. has_srei .and. allocated(comment_continue%value)) then
            srei = comment_continue%value
            has_srei = .true.
        end if

        csre = comment_padright(cstart(), re=.true.)
        sre = ""
        if (has_srei) sre = "^\s-*?\("//srei//"\)"

        do
            if (.not. (point() < mpos(endm))) exit
            spt = comment_search_forward(mpos(endm), .true., st)
            if (spt == 0) exit
            ipt = point()
            ! Find the end of the comment.
            call goto_char(spt)

            if (.not. comment_forward(1_int64)) then
                ! Allow non-terminated comments.
                if (.not. eobp()) then
                    call set_error(status, FM_GENERIC_ERROR, "Can't find the comment end")
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
            call goto_char(ipt)
            moved = skip_syntax_backward(" ")
            ! A box-comment starts with a looong comment-start marker.
            ok = .false.

            if (point() - point_min() == 1) then
                box_equal = .true.

                if (looking("=\{7\}")) then
                    if (before(point_max()) /= 10) then
                        moved = skip_chars_forward("=", st)
                        ok = .true.
                    end if
                end if
            end if

            if (.not. ok) ok = point() - point_min() - schars(cstart()) > 7
            if (ok) ok = count_lines(point_min(), point_max()) > 2
            if (ok) box = .true.
            ! Skip the padding.  Padding can come from comment-padding and/or
            ! from comment-start, so we first check comment-start.
            pos = point()
            call goto_char(point_min())
            ok = looking(csre)
            call goto_char(pos)
            if (.not. ok) ok = looking(regexp_quote(cpad()))
            if (ok) call goto_char(me(0))

            if (has_srei) then
                if (looking("\s-*"//LF//"\s-*"//srei)) call goto_char(me(0))
            end if

            if (.not. present(arg)) then
                call delete_region(point_min(), point(), status)
            else
                opointm = marker_at(point())
                moved = skip_syntax_backward(" ")
                call delete_char(-numarg, status)
                ok = .false.

                if (.not. bobp()) then
                    pos = point()
                    call goto_char(point_min())
                    ok = looking(cskip())
                    call goto_char(pos)
                end if

                if (.not. ok) then
                    ! If there's something left but it doesn't look like
                    ! a comment-start any more, just remove it.
                    call delete_region(point_min(), mpos(opointm), status)
                end if

                call unmark(opointm)
            end if

            if (.not. status%ok()) then
                call restore_restriction(r)
                exit
            end if

            ! Remove the end-comment (and leading padding and such).
            call goto_char(point_max())
            call comment_enter_backward()

            ! Check for special `=' used sometimes in comment-box.
            if (box_equal .and. before(point_max()) /= 10) then
                pos = point()
                ! skip `=' but only if there are at least 7.
                if (skip_chars_backward("=", st) > -7) call goto_char(pos)
            end if

            if (.not. looking("\("//LF//"\|\s-\)*\'")) then
                if (bolp() .and. .not. bobp()) call goto_char(point() - 1)

                if (.not. present(arg)) then
                    call delete_region(point(), point_max(), status)
                else
                    moved = skip_syntax_forward(" ")
                    call delete_char(numarg, status)
                    ok = eobp()
                    if (.not. ok) ok = looking(comment_end_skip%value)
                    ! If there's something left but it doesn't look like
                    ! a comment-end any more, just remove it.
                    if (.not. ok .and. status%ok()) call delete_region(point(), point_max(), status)
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
                    erei = comment_padleft(cce, re=.true., found=has_ere)
                    if (has_ere) ere = "\("//erei//"\)\s-*$"
                end if

                call goto_char(point_min())

                do
                    if (has_ere) then
                        if (re_forward(ere, line_end_position(), SEARCH_NOERROR)) then
                            grp = 1
                            if (me(2) > 0) grp = 2
                            call replace_match("", st, fixedcase=.true., literal=.true., subexp=grp)
                        else
                            has_ere = .false.
                        end if
                    end if

                    e = forward_line(1_int64)
                    if (.not. re_forward(sre, line_end_position(), SEARCH_NOERROR)) exit
                    grp = 1
                    if (me(2) > 0) grp = 2
                    call replace_match("", st, fixedcase=.true., literal=.true., subexp=grp)
                end do
            end if

            ! Go to the end for the next comment.
            call goto_char(point_max())
            call restore_restriction(r)

            ! Remove any obtrusive spaces left preceding a tab at `spt'.
            if (after(spt) == 9 .and. before(spt) == 32 .and. tab_width%value > 0) then
                pos = point()
                call goto_char(spt)
                fcol = current_column()
                slim = point() - mod(fcol, tab_width%value)
                moved = skip_chars_backward(" ", st, slim)
                call delete_char(-moved, status)
                call goto_char(pos)
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

    !> Uncomment each line in the BEG .. END region.
    !> The numeric prefix ARG can specify a number of chars to remove from the
    !> comment markers.  Port of newcomment.el:uncomment-region-default
    !> (combine-change-calls is Deferred).
    recursive subroutine uncomment_region_default(beg, end, status, arg, universal)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: universal

        ! UNIVERSAL changes nothing here: C-u counts as its numeric value.
        if (present(universal)) continue
        call uncomment_region_default_1(beg, end, status, arg)
    end subroutine uncomment_region_default

    !> Make a white-space string of width LEN for use at BOL.
    !> When `indent_tabs_mode' is true, tab characters will be used.
    !> Port of newcomment.el:comment-make-bol-ws.
    function comment_make_bol_ws(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s

        if (indent_tabs_mode%value .and. tab_width%value > 0) then
            s = repeat_char(TAB, n/tab_width%value)//repeat_char(" ", mod(n, tab_width%value))
        else
            s = repeat_char(" ", n)
        end if
    end function comment_make_bol_ws

    !> Make the leading and trailing extra lines.
    !> This is used for `extra-line' style (or `box' style if BLOCK is
    !> specified).  Port of newcomment.el:comment-make-extra-lines.
    subroutine comment_make_extra_lines(cs, ce, ccs, cce, min_indent, max_indent, block, out_cs, out_ce)
        character(len=*), intent(in) :: cs, ce, ccs, cce
        integer(int64), intent(in) :: min_indent, max_indent
        logical, intent(in) :: block
        character(len=:), allocatable, intent(out) :: out_cs, out_ce
        character(len=:), allocatable :: cs2, ce2, s, e, re, fill, c
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
            width = max_indent - min_indent
            s = cs2//"a=m"//cce
            e = ccs//"a=m"//ce2

            if (smatch(".*\S-\S-", cs2) >= 0) then
                c = sub(cs2, sme(0) - 1, sme(0))
            else if (ce2 == "" .and. smatch(".*\S-", cs2) >= 0) then
                c = sub(cs2, sme(0) - 1, sme(0))
            else
                c = "="
            end if

            re = "\s-*a=m\s-*"
            k = smatch(re, s)
            lcs = schars(cs2)
            fill = ""

            do k = 1, width + (sme(0) - smb(0) - lcs - 3)
                fill = fill//c
            end do

            call replace_in_string(fill, s, cs2)

            if (smatch(cskip(), cs2) < 0 .and. smatch("a=m", s) >= 0) then
                ! The whitespace around CS cannot be ignored: put it back.
                re = "a=m"
                fill = ""

                do k = 1, width - lcs
                    fill = fill//c
                end do

                call replace_in_string(fill, s, cs2)
            end if

            k = smatch(re, e)
            call replace_in_string(fill, e, ce2)
        end if

        out_cs = cs2//LF//comment_make_bol_ws(min_indent)//ccs
        out_ce = cce//LF//comment_make_bol_ws(min_indent + eindent)//ce2
        st = status_type()
    end subroutine comment_make_extra_lines

    !> (replace-match NEW t t S): S with the last string match replaced
    !> by NEW, into R.  Not a port.
    subroutine replace_in_string(new, s, r)
        character(len=*), intent(in) :: new, s
        character(len=:), allocatable, intent(out) :: r

        r = sub(s, 0_int64, smb(0))//new//sub(s, sme(0))
    end subroutine replace_in_string

    !> Comment region BEG .. END.
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
    recursive subroutine comment_region_internal(beg, end, cs_in, status, ce_in, ccs_in, cce_in, block, lines, indent)
        integer(int64), intent(in) :: beg, end
        character(len=*), intent(in) :: cs_in
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: ce_in, ccs_in, cce_in
        logical, intent(in), optional :: block, lines, indent
        character(len=:), allocatable :: cs, ce, ccs, cce, ccs2, cce2
        logical :: no_empty, has_ce, has_ccs, has_cce, ce_sanitized, blk, lns, ind, has_tab
        integer(int64) :: min_indent, max_indent, reached, bindent, opoint, found, col, sh, n
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
        no_empty = .not. (comment_empty_lines%value == COMMENT_EMPTY_LINES_T .or. &
                          (comment_empty_lines%value /= COMMENT_EMPTY_LINES_NIL .and. len(ce) == 0))
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
        opoint = point()
        call goto_char(end)

        ! If the end is not at the end of a line and the comment-end
        ! is implicit (i.e. a newline), explicitly insert a newline.
        if (.not. (ce_sanitized .or. eolp())) then
            call insert(LF, status)
            if (.not. status%ok()) return
            call indent_according_to_mode(status)
            if (.not. status%ok()) return
        end if

        endm = marker_at(end)
        ! comment-with-narrowing: the space added (and then removed) at
        ! the beginning keeps the text's indentation as it was.
        call goto_char(beg)
        bindent = current_column()
        r = save_restriction()
        call narrow_to_region(beg, mpos(endm), st)
        call goto_char(point_min())
        call insert(repeat_char(" ", bindent), status)
        min_indent = point_max()
        max_indent = 0
        call goto_char(point_min())
        ! Quote any nested comment marker
        call quote_nested(cstart(), cend(), .false., status)
        ! Loop over all lines to find the needed indentations.
        call goto_char(point_min())

        do
            if (.not. looking("[ "//TAB//"]*$")) min_indent = min(min_indent, current_indentation())
            call end_of_line(1_int64)
            max_indent = max(max_indent, current_column())
            if (eobp()) exit
            sh = forward_line(1_int64)
        end do

        has_tab = .false.
        call goto_char(beg)
        call search_tab(mpos(endm), has_tab)
        max_indent = max_indent + max(schars(cs), schars(ccs))
        ! Inserting ccs can change max-indent by (1- tab-width)
        ! but only if there are TABs in the boxed text, of course.
        if (has_tab) max_indent = max_indent + tab_width%value - 1
        if (.not. ind) min_indent = 0

        ! make the leading and trailing lines if requested
        if (lns) then
            ! Trim trailing whitespace from cs if there's some.
            found = smatch("[ "//TAB//LF//char(13)//"]+\'", cs)
            if (found >= 0) cs = sub(cs, 0_int64, found)
            call comment_make_extra_lines(cs, ce, ccs, cce, min_indent, max_indent, blk, ccs2, cce2)
            cs = ccs2
            ce = cce2
            has_ce = .true.
        end if

        call goto_char(point_min())

        ! Loop over all lines from BEG to END.
        do
            if (.not. (no_empty .and. looking("[ "//TAB//"]*$"))) then
                call move_to_column(min_indent, reached, force=.true.)
                call insert(cs, status)
                if (.not. status%ok()) exit
                ! switch to CCS after the first line
                cs = ccs
                call end_of_line(1_int64)

                if (eobp()) then
                    cce = ce
                    has_cce = has_ce .and. len(ce) > 0
                end if

                if (has_cce) then
                    if (blk) call move_to_column(max_indent, reached, force=.true.)
                    call insert(cce, status)
                    if (.not. status%ok()) exit
                end if
            end if

            call end_of_line(1_int64)
            if (eobp()) exit
            sh = forward_line(1_int64)
        end do

        ! remove the bindent
        call goto_char(point_min())

        if (looking(" *")) then
            n = min(me(0) - mb(0), bindent)
            call delete_char(n, st)
            bindent = bindent - n
        end if

        call end_of_line(1_int64)
        col = point()
        call beginning_of_line(1_int64)

        do while (bindent > 0)
            if (.not. re_forward("   *", col, SEARCH_NOERROR)) exit
            n = min(bindent, me(0) - mb(0) - 1)
            call goto_char(mb(0))
            call delete_char(n, st)
            bindent = bindent - n
        end do

        call restore_restriction(r)
        call unmark(endm)
        call goto_char(min(opoint, point_max()))
    end subroutine comment_region_internal

    !> Whether there is a TAB between point and LIMIT.  Not a port.
    subroutine search_tab(limit, found)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        integer(int64) :: pos

        pos = point()
        found = re_forward(TAB, limit, SEARCH_NOERROR)
        call goto_char(pos)
    end subroutine search_tab

    !> Comment or uncomment each line in the region.
    !> With just \[universal_argument] prefix arg, uncomment each line in region BEG .. END.
    !> Numeric prefix ARG means use ARG comment characters.
    !> If ARG is negative, delete that many comment characters instead.
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
    recursive subroutine comment_region(beg_in, end_in, status, arg, universal)
        integer(int64), intent(in) :: beg_in, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: universal
        integer(int64) :: beg, end
        type(handle_type) :: om

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_vars(status)
        if (.not. status%ok()) return
        beg = min(beg_in, end_in)
        end = max(beg_in, end_in)
        om = marker_at(point())
        call comment_region_function(beg, end, status, arg, universal)
        call goto_char(mpos(om))
        call unmark(om)
    end subroutine comment_region

    !> Comment or uncomment each line in the region.
    !> With just \[universal_argument] prefix arg, uncomment each line in region BEG .. END.
    !> Numeric prefix ARG means use ARG comment characters.
    !> If ARG is negative, delete that many comment characters instead.
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
    recursive subroutine cmd_comment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call interactive_region(beg, end, status)
        if (.not. status%ok()) return
        call with_prefix(beg, end, status, comment_region)
    end subroutine cmd_comment_region

    !> Port of newcomment.el:comment-region-default-1.
    recursive subroutine comment_region_default_1(beg_in, end_in, status, arg, universal)
        integer(int64), intent(in) :: beg_in, end_in
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: universal
        integer(int64) :: beg, end, numarg, moved, k
        integer :: style
        logical :: lines, block, multi, multi_char, triple, indent, ok, cons_arg, has
        character(len=:), allocatable :: cs, ce, ccs, cce, s
        type(status_type) :: st

        status = status_type()
        numarg = 1
        if (present(arg)) numarg = arg
        cons_arg = .false.
        if (present(universal)) cons_arg = universal
        style = int(comment_style%value)
        if (style < 1 .or. style > 8) style = COMMENT_STYLE_INDENT
        lines = style_extra(style)
        block = style_align(style)
        multi = style_multi(style)
        ! We use `chars' instead of `syntax' because `\n' might be
        ! of end-comment syntax rather than of whitespace syntax.
        ! sanitize BEG and END
        call goto_char(beg_in)
        moved = skip_chars_forward(" "//TAB//LF//char(13), st)
        call beginning_of_line(1_int64)
        beg = max(beg_in, point())
        call goto_char(end_in)
        moved = skip_chars_backward(" "//TAB//LF//char(13), st)
        call end_of_line(1_int64)
        end = min(end_in, point())

        if (beg >= end) then
            call set_error(status, FM_GENERIC_ERROR, "Nothing to comment")
            return
        end if

        ! sanitize LINES
        if (lines) then
            call goto_char(beg)
            call beginning_of_line(1_int64)
            moved = skip_syntax_forward(" ")
            ok = point() >= beg

            if (ok) then
                call goto_char(end)
                call end_of_line(1_int64)
                moved = skip_syntax_backward(" ")
                ok = point() <= end
            end if

            if (ok) ok = block .or. cend() /= ""

            if (ok .and. .not. block) then
                call goto_char(beg)
                ok = re_forward("$", end, SEARCH_NOERROR)
            end if

            lines = ok
        end if

        ! don't add end-markers just because the user asked for `block'
        if (.not. (lines .or. cend() == "")) block = .false.

        if (cons_arg) then
            call uncomment_region(beg, end, status)
            return
        else if (numarg < 0) then
            call uncomment_region(beg, end, status, -numarg)
            return
        end if

        multi_char = smatch("[ "//TAB//"]*\'", cstart()) /= 1
        triple = .false.

        if (style_indent(style) == 2) then
            k = point()
            call goto_char(beg)
            indent = multi_char
            ! Triple if we will put the comment starter at the margin
            ! and the first line of the region isn't indented
            ! at least two spaces.
            triple = .not. multi_char .and. looking(TAB//"\|  ")
            call goto_char(k)
        else
            indent = style_indent(style) == 1
        end if

        ! In Lisp and similar modes with one-character comment starters,
        ! double it by default if `comment-add' says so.
        ! If it isn't indented, triple it.
        if (.not. present(arg) .and. .not. multi_char) then
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
                call comment_region_internal(beg, end, cs, status, ce, ccs, cce, block=block, lines=lines, indent=indent)
            else if (has) then
                call comment_region_internal(beg, end, cs, status, ce, cce_in=cce, block=block, lines=lines, &
                                             indent=indent)
            else if (ok) then
                call comment_region_internal(beg, end, cs, status, ccs_in=ccs, cce_in=cce, block=block, &
                                             lines=lines, indent=indent)
            else
                call comment_region_internal(beg, end, cs, status, cce_in=cce, block=block, lines=lines, indent=indent)
            end if
        else if (has) then
            call comment_region_internal(beg, end, cs, status, ce, block=block, lines=lines, indent=indent)
        else
            call comment_region_internal(beg, end, cs, status, block=block, lines=lines, indent=indent)
        end if
    end subroutine comment_region_default_1

    !> Comment BEG .. END with the prefix argument ARG, as
    !> `comment_region' does by default (the default
    !> `comment_region_function').  Port of
    !> newcomment.el:comment-region-default (combine-change-calls is
    !> Deferred).
    recursive subroutine comment_region_default(beg, end, status, arg, universal)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: universal

        call comment_region_default_1(beg, end, status, arg, universal)
    end subroutine comment_region_default

    !> Comment out the BEG .. END region, putting it inside a box.
    !> The numeric prefix ARG specifies how many characters to add to begin- and
    !> end- comment markers additionally to what variable `comment_add' already
    !> specifies.  Port of newcomment.el:comment-box.
    recursive subroutine comment_box(beg, end, arg, status)
        integer(int64), intent(in) :: beg, end, arg
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_vars(status)
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

        call comment_region(beg, end, status, comment_add%value + arg)
        comment_style%value = saved
    end subroutine comment_box

    !> Comment out the BEG .. END region, putting it inside a box.
    !> The numeric prefix ARG specifies how many characters to add to begin- and
    !> end- comment markers additionally to what variable `comment_add' already
    !> specifies.  Port of newcomment.el:comment-box (the command).
    recursive subroutine cmd_comment_box(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call interactive_region(beg, end, status)
        if (.not. status%ok()) return
        call comment_box(beg, end, prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_comment_box

    !> Return true if the text between BEG and END is all comments.
    !> Port of newcomment.el:comment-only-p.
    recursive logical function comment_only_p(beg, end) result(only)
        integer(int64), intent(in) :: beg, end
        integer(int64) :: opoint
        logical :: done

        opoint = point()
        call goto_char(beg)
        done = comment_forward(point_max())
        only = end <= point()
        call goto_char(opoint)
    end function comment_only_p

    !> Call `comment_region', unless the region only consists of comments,
    !> in which case call `uncomment_region'.  If a prefix arg is given, it
    !> is passed on to the respective function.  Port of
    !> newcomment.el:comment-or-uncomment-region.
    recursive subroutine comment_or_uncomment_region(beg, end, status, arg, universal)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: universal

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_vars(status)
        if (.not. status%ok()) return

        if (comment_only_p(beg, end)) then
            call uncomment_region(beg, end, status, arg, universal)
        else
            call comment_region(beg, end, status, arg, universal)
        end if
    end subroutine comment_or_uncomment_region

    !> Call `comment_region', unless the region only consists of comments,
    !> in which case call `uncomment_region'.  If a prefix arg is given, it
    !> is passed on to the respective function.  Port of
    !> newcomment.el:comment-or-uncomment-region (the command).
    recursive subroutine cmd_comment_or_uncomment_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call interactive_region(beg, end, status)
        if (.not. status%ok()) return
        call with_prefix(beg, end, status, comment_or_uncomment_region)
    end subroutine cmd_comment_or_uncomment_region

    !> Call the comment command you want (Do What I Mean).
    !> If the region is active and `transient_mark_mode' is on, call
    !> `comment_region' (unless it only consists of comments, in which
    !> case it calls `uncomment_region'); in this case, prefix numeric
    !> argument ARG specifies how many characters to remove from each
    !> comment delimiter (so don't specify a prefix argument whose value
    !> is greater than the total length of the comment delimiters).
    !> Else, if the current line is empty, call `comment_insert_comment_function'
    !> if it is defined, otherwise insert a comment and indent it.
    !> Else, if a prefix ARG is specified, call `comment_kill'; in this
    !> case, prefix numeric argument ARG specifies on how many lines to kill
    !> the comments.
    !> Else, call `comment_indent'.
    !> You can configure `comment_style' to change the way regions are
    !> commented.  Port of newcomment.el:comment-dwim.
    recursive subroutine comment_dwim(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end, add, opoint
        logical :: blank, has
        character(len=:), allocatable :: s

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call comment_normalize_vars(status)
        if (.not. status%ok()) return

        if (use_region_p()) then
            call region_bounds(beg, end, status)
            if (.not. status%ok()) return
            call with_prefix(beg, end, status, comment_or_uncomment_region)
            return
        end if

        opoint = point()
        call beginning_of_line(1_int64)
        blank = looking("\s-*$")
        call goto_char(opoint)

        if (.not. blank) then
            ! FIXME: If there's no comment to kill on this line and ARG is
            ! specified, calling comment-kill is not very clever.
            if (.not. nilp(current_prefix_arg)) then
                if (fixnump(current_prefix_arg)) then
                    call comment_kill(status, xfixnum(current_prefix_arg))
                else
                    call comment_kill(status)
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
        opoint = point()

        if (cend() /= "") then
            s = comment_padleft(cend(), add, found=has)
            call insert(s, status)
            if (.not. status%ok()) return
        end if

        call indent_according_to_mode(status)
        call goto_char(opoint)
    end subroutine comment_dwim

    !> `comment_dwim' as a command.
    recursive subroutine cmd_comment_dwim(status)
        type(status_type), intent(out) :: status

        call comment_dwim(status)
    end subroutine cmd_comment_dwim

    !> Compute the number of extra comment starter characters.
    !> (Extra semicolons in Lisp mode, extra stars in C mode, etc.)
    !> If ARG is true, just follow ARG.
    !> If the comment starter is multi-char, just follow ARG.
    !> Otherwise obey `comment_add'.  Port of newcomment.el:comment-add
    !> (ARG is the current prefix argument).
    integer(int64) function comment_add_count() result(n)
        if (nilp(current_prefix_arg) .and. smatch("[ "//TAB//"]*\'", cstart()) == 1) then
            n = comment_add%value
        else
            n = prefix_numeric_value(current_prefix_arg) - 1
        end if
    end function comment_add_count

    !> Check that the adaptive fill prefix is consistent with the context.
    !> PREFIX is the prefix (presumably guessed by `adaptive_fill_mode').
    !> COMPOS is the position of the beginning of the comment we're in, or 0
    !> if we're not inside a comment.  Port of
    !> newcomment.el:comment-valid-prefix-p.
    recursive logical function comment_valid_prefix_p(prefix, compos) result(valid)
        character(len=*), intent(in) :: prefix
        integer(int64), intent(in) :: compos
        character(len=:), allocatable :: prefix_com, prefix_space, prefix_comstart
        integer(int64) :: opoint, prefix_indent, k, lm
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

        opoint = point()
        ! Accept any prefix if the current comment is not EOL-terminated.
        call goto_char(compos)
        done = comment_forward(1_int64)
        valid = .not. bolp()
        call goto_char(opoint)
        if (valid) return
        ! Accept any prefix that starts with the same comment-start marker
        ! as the current one.
        if (smatch("\`[ "//TAB//"]*\(?:"//cskip()//"\)", prefix) < 0) return
        prefix_com = comment_string_strip(match_string(0, prefix), .false., .true.)
        k = smatch("\`[ "//TAB//"]*", prefix_com)
        prefix_space = match_string(0, prefix_com)
        prefix_indent = schars(prefix_space)
        prefix_comstart = sub(prefix_com, sme(0))
        call goto_char(compos)

        ! The comstart marker is the same.
        if (looking(regexp_quote(prefix_comstart))) then
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

        call goto_char(opoint)
    end function comment_valid_prefix_p

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
        integer(int64) :: compos, comin, opoint, pt, saved_column, col, endp
        character(len=:), allocatable :: comstart, comend, saved_start, saved_end, saved_continue, rest
        logical :: soft_nl, inter, normalp, continuep, fwd, has_continue, noindent, own_line, had_local
        type(handle_type) :: cm
        procedure(comment_indent_proc), pointer :: saved_fn
        type(status_type) :: st

        soft_nl = .false.
        if (present(soft)) soft_nl = soft
        inter = .false.
        if (present(interactive)) inter = interactive
        call comment_normalize_vars(status, noerror=.true.)
        if (.not. status%ok()) return
        compos = 0
        comin = 0

        ! If we are not inside a comment and we only auto-fill comments,
        ! don't do anything (unless no comment syntax is defined).
        if (allocated(comment_start%value) .and. comment_auto_fill_only_comments%value .and. .not. inter) then
            opoint = point()
            compos = comment_beginning()
            comin = point()
            call goto_char(opoint)
            if (compos == 0) return
        end if

        ! Now we know we should auto-fill.
        ! Insert the newline before removing empty space so that markers
        ! get preserved better.
        if (soft_nl) then
            call insert(LF, status)
        else
            call newline(status, 1_int64)
        end if

        if (.not. status%ok()) return
        opoint = point()
        call goto_char(point() - 1)
        call delete_horizontal_space(.false., status)
        if (.not. status%ok()) return
        call goto_char(opoint)
        call goto_char(point())
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
            opoint = point()
            call goto_char(point() - 1)
            compos = comment_beginning()
            comin = point()
            call goto_char(opoint)
        end if

        ! If there's an adaptive prefix, use it unless we're inside
        ! a comment and the prefix is not a comment starter.
        if (allocated(fill_prefix%value)) then
            if (comment_valid_prefix_p(fill_prefix%value, compos)) then
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
        normalp = smatch(regexp_quote(comment_string_strip(cstart(), .true., .true.)), comstart) >= 0

        if (normalp) then
            comend = cend()
        else
            ! The comment starter is not the normal comment-start
            ! so we can't just use comment-end.
            opoint = point()
            call goto_char(compos)
            fwd = comment_forward(1_int64)

            if (.not. fwd) then
                comend = cend()
            else
                endp = point()
                call comment_enter_backward()
                comend = comment_string_strip(text(point(), endp), .false., .true.)
            end if

            call goto_char(opoint)
        end if

        if (comment_multi_line%value .and. len(comend) > 0) then
            call indent_according_to_mode(status)
            return
        end if

        call insert(LF, status)
        if (.not. status%ok()) return
        call goto_char(point() - 1)
        ! The continuation indentation should be somewhere
        ! between the current line's indentation (plus 2 for
        ! good measure) and the current comment's indentation,
        ! with a preference for comment-column.
        opoint = point()
        call goto_char(compos)
        col = min(current_column(), max(comment_column%value, 2 + current_indentation()))
        ! If the previous comment is on its own line, then
        ! reuse its indentation unconditionally.
        ! Important for modes like Python/Haskell where
        ! auto-indentation is unreliable.
        pt = skip_chars_backward(" "//TAB, st)
        own_line = bolp()
        call goto_char(opoint)
        saved_column = comment_column%value
        saved_fn => comment_indent_function%get()
        had_local = comment_indent_function%local_p()
        saved_start = cstart()
        saved_end = cend()
        has_continue = allocated(comment_continue%value)
        if (has_continue) saved_continue = comment_continue%value
        comment_column%value = col
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
                call comment_indent_function%kill_local()
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
        pt = point()
        cm = marker_at(pt)
        call end_of_line(1_int64)
        rest = text(pt, point())
        ! The 1+ is to make sure we delete the \n inserted above.
        call delete_region(pt, point() + 1, status)
        if (.not. status%ok()) return
        call end_of_line(0_int64)
        call insert(rest, status)
        call goto_char(mpos(cm))
        call unmark(cm)
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
    recursive subroutine cmd_comment_indent_new_line(status)
        type(status_type), intent(out) :: status

        call comment_indent_new_line(status, interactive=.true.)
    end subroutine cmd_comment_indent_new_line

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
    !> Unlike `comment_dwim', this always comments whole lines.  Port of
    !> newcomment.el:comment-line.
    recursive subroutine comment_line(n_in, status)
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: n, b, e, a, z, sh

        status = status_type()
        n = n_in

        if (use_region_p()) then
            call region_bounds(b, e, status)
            if (.not. status%ok()) return
            call goto_char(b)
            a = line_beginning_position()
            call goto_char(e)
            z = line_end_position()
            call comment_or_uncomment_region(a, z, status)
            return
        end if

        if (value_eq(last_command, sym("comment_line_backward")) .and. n >= 0) n = -n
        a = line_beginning_position()
        z = line_end_position(n)
        call goto_char(z)
        call comment_or_uncomment_region(min(a, z), max(a, z), status)
        if (.not. status%ok()) return
        sh = forward_line(1_int64)
        call back_to_indentation()
        if (n < 0) this_command = sym("comment_line_backward")
    end subroutine comment_line

    !> Comment or uncomment current line and leave point after it.
    !> With positive prefix, apply to N lines including current one.
    !> With negative prefix, apply to -N lines above.  Also, further
    !> consecutive invocations of this command will inherit the negative
    !> argument.
    !>
    !> If region is active, comment lines in active region instead.
    !> Unlike `comment_dwim', this always comments whole lines.  Port of
    !> newcomment.el:comment-line (the command).
    recursive subroutine cmd_comment_line(status)
        type(status_type), intent(out) :: status

        call comment_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_comment_line
end module newcomment_fm
